{
	Copyright 1992-2024 Sudhir Kumar and Koichiro Tamura

	This file is part of the MEGA (Molecular Evolutionary Genetics Analyis) software.

	MEGA (Molecular Evolutionary Genetics Analysis) is free software:
	you can redistribute it and/or modify it under the terms of the
	GNU General Public License as published by the Free Software
	Foundation, either version 3 of the License, or (at your option)
	any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <https://www.gnu.org/licenses/>.

   Contributor(s):   The MEGA source code and software is made available in the hopes that it will be useful. 
   In keeping with the spirit and intent that the MEGA project is developed under, the authors of MEGA request that before
   distributing any significant changes to the MEGA source code (or derivatives thereof), you share
   those changes with the MEGA authors so that they may have the opportunity to test that
   the changes do not introduce errors into the code or otherwise negatively impact the correctness
   or performance of the MEGA software.
   
	Please email inqiries to s.kumar@temple.edu
}

unit alignmentconcatenator;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  MD_Sequences, Classes, SysUtils, MegaConsts, MDataFileInfo, MegaUtils_NV,
  malignmentfileloader, mmegaalignmentfileloader, mfastaalignmentfileloader,
  mruntimeprogressdlg, MDomainInfo;

const
  NUM_FILES_READ = 'No. of files read';
  NUM_TAXA_FOUND = 'No. of taxa found';
  NUM_SITES_READ = 'No. of sites read';

type

  { TAlignmentConcatenator }

  TAlignmentConcatenator = class(TObject)
    private
      FAllDomainInfo: TAllDomainInfo;
      FDoRecursiveSearch: Boolean;
      FFilenames: TStringList;
      FLog: TStringList;
      FSourceDirectory: String;
      FAlignmentLoader: TAlignmentFileLoader;
      FSequences: TFastSequenceList;
      FGapSym, FIdentSym, FMissSym: Char;
      FSpecialCharsSet: Boolean;
      function GetAllDomainInfo: TAllDomainInfo;
      function GetLog: String;
      function GetNumSeqs: Integer;
      function GetSequences: TFastSequenceList;
      procedure SetDoRecursiveSearch(AValue: Boolean);
      procedure SetSourceDirectory(AValue: String);
    protected
      FFileInfo: TDataFileInfo;
      FUseFileList: Boolean;
      function CheckSpecialChars(gap, ident, miss: Char): Boolean;
      procedure InitAlignmentLoader(aFile: String);
    public
      CheckCancel: TCheckCancelFunc;
      constructor Create(fileInfo: TDataFileInfo);
      destructor Destroy; override;
      function GetMegaAlignmentStrings: TStringList;
      function FindAlignmentFiles: Boolean;
      function ProcessFile(aFile: String): Boolean;
      procedure Finalize;
      function NumFiles: Integer;
      procedure UseFileList(ListOfFiles: TStringList);
      procedure LogMsg(aMsg: String);
      property SourceDirectory: String read FSourceDirectory write SetSourceDirectory;
      property Filenames: TStringList read FFilenames;
      property Log: String read GetLog;
      property DoRecursiveSearch: Boolean read FDoRecursiveSearch write SetDoRecursiveSearch;
      property Sequences: TFastSequenceList read GetSequences;
      property NumSeqs: Integer read GetNumSeqs;
      property AllDomainInfo: TAllDomainInfo read GetAllDomainInfo write FAllDomainInfo;
  end;

  { TAlignConcatThread }

  TAlignConcatThread = class(TMegaThread)
    private
      FCheckCancel: TCheckCancelFunc;
      FFileInfo: TDataFileInfo;
      FIsSuccess: Boolean;
      FSubtaskCheckCancel: TCheckCancelFunc;
      FUpdateRunStatusInfoProc: TUpdateRunStatusInfoEvent;
      function GetLogMessages: String;
      function GetNumFiles: Integer;
      procedure SetCheckCancel(AValue: TCheckCancelFunc);
      procedure SetSubtaskCheckCancel(AValue: TCheckCancelFunc);
      procedure SetUpdateRunStatusInfoProc(AValue: TUpdateRunStatusInfoEvent);

    protected
      FCancelled: Boolean;
      FConcatenator: TAlignmentConcatenator;
      FProgress: Integer;
      FStatus: String;
      FInfoType: String;
      FInfoValue: String;
      procedure DoCheckCancel;
      procedure DoConcatenation;
      procedure UpdateRunStatusInfo(aType: String; aInfo: String);
      procedure DoUpdateRunStatusInfo;
      function ClientCheckCancel(aProgress: Integer; aStatus: AnsiString): Boolean;
    public
      constructor Create(SourceDir: String; aFileInfo: TDataFileInfo);
      constructor CreateWithList(aFileList: TStringList; aFileInfo: TDataFileInfo);
      destructor Destroy; override;
      procedure Execute; override;
      function GetSequences: TSequenceList;
      function GetAllDomainInfo: TAllDomainInfo;
      property CheckCancel: TCheckCancelFunc read FCheckCancel write SetCheckCancel;
      property SubtaskCheckCancel: TCheckCancelFunc read FSubtaskCheckCancel write SetSubtaskCheckCancel;
      property UpdateRunStatusInfoProc: TUpdateRunStatusInfoEvent read FUpdateRunStatusInfoProc write SetUpdateRunStatusInfoProc;
      property IsSuccess: Boolean read FIsSuccess;
      property LogMessages: String read GetLogMessages;
      property FileInfo: TDataFileInfo read FFileInfo;
      property NumFiles: Integer read GetNumFiles;
  end;

implementation

uses
  LazFileUtils, FileUtil;

{ TAlignConcatThread }

procedure TAlignConcatThread.SetCheckCancel(AValue: TCheckCancelFunc);
begin
  FCheckCancel:=AValue;
end;

function TAlignConcatThread.GetLogMessages: String;
begin
  Result := FConcatenator.Log;
end;

function TAlignConcatThread.GetNumFiles: Integer;
begin
  if Assigned(FConcatenator) then
    Result := FConcatenator.NumFiles
  else
    Result := 0;
end;

procedure TAlignConcatThread.SetSubtaskCheckCancel(AValue: TCheckCancelFunc);
begin
  FSubtaskCheckCancel:=AValue;
end;

procedure TAlignConcatThread.SetUpdateRunStatusInfoProc(AValue: TUpdateRunStatusInfoEvent);
begin
  FUpdateRunStatusInfoProc := AValue;
end;

procedure TAlignConcatThread.DoCheckCancel;
begin
  if Assigned(FCheckCancel) then
  begin
    FCancelled := FCheckCancel(FProgress, FStatus);
    if FCancelled then
      Terminate;
  end;
end;

procedure TAlignConcatThread.DoConcatenation;
var
  i: Integer;
  aFile: String;
begin
  FIsSuccess := FConcatenator.FindAlignmentFiles;
  if (not FIsSuccess) or Terminated then
    Exit;
  for i := 0 to FConcatenator.NumFiles - 1 do
  begin
    aFile := FConcatenator.Filenames[i];
    FProgress := Round(i / FConcatenator.NumFiles * 100);
    FStatus := 'Processing file: ' + ExtractFilename(aFile);
    {$IFDEF VISUAL_BUILD}
    Synchronize(DoCheckCancel);
    {$ELSE}
    DoCheckCancel;
    {$ENDIF}
    if Terminated then
    begin
      FIsSuccess := False;
      Exit;
    end;
    FIsSuccess := (FIsSuccess and FConcatenator.ProcessFile(aFile));
    if (i mod 5) = 0 then
    begin
      UpdateRunStatusInfo(NUM_FILES_READ, Format('%.0n', [(i + 1)*1.0]));
      UpdateRunStatusInfo(NUM_TAXA_FOUND, Format('%.0n', [FConcatenator.NumSeqs*1.0]));
      UpdateRunStatusInfo(NUM_SITES_READ, Format('%.0n', [FConcatenator.Sequences.NumTotalSites*1.0]));
    end;
  end;
  if FIsSuccess then
  begin
    UpdateRunStatusInfo('Status', 'finalizing...');
    FConcatenator.Sequences.CheckCancel := ClientCheckCancel;
    FConcatenator.Finalize;
  end;
end;

procedure TAlignConcatThread.UpdateRunStatusInfo(aType: String; aInfo: String);
begin
  FInfoType := aType;
  FInfoValue := aInfo;
  Synchronize(DoUpdateRunStatusInfo)
end;

procedure TAlignConcatThread.DoUpdateRunStatusInfo;
begin
  if Assigned(FUpdateRunStatusInfoProc) then
    FUpdateRunStatusInfoProc(FInfoType, FInfoValue);
end;

function TAlignConcatThread.ClientCheckCancel(aProgress: Integer;
  aStatus: AnsiString): Boolean;
begin
  Result := False;
  if Assigned(FCheckCancel) then
  begin
    FProgress := aProgress;
    FStatus := aStatus;
    Synchronize(DoCheckCancel);
    Result := FCancelled;
  end;
end;

constructor TAlignConcatThread.Create(SourceDir: String; aFileInfo: TDataFileInfo);
begin
  inherited Create(True);
  FCancelled := False;
  FreeOnTerminate := True;
  FFileInfo := aFileInfo;
  FConcatenator := TAlignmentConcatenator.Create(FFileInfo);
  FConcatenator.SourceDirectory := SourceDir;
  FIsSuccess := False;
end;

constructor TAlignConcatThread.CreateWithList(aFileList: TStringList; aFileInfo: TDataFileInfo);
begin
  inherited Create(True);
  FCancelled := False;
  FreeOnTerminate := True;
  FFileInfo := aFileInfo;
  FConcatenator := TAlignmentConcatenator.Create(FFileInfo);
  FConcatenator.UseFileList(aFileList);
  FConcatenator.SourceDirectory := EmptyStr;
  FIsSuccess := False;
end;

destructor TAlignConcatThread.Destroy;
begin
  if Assigned(FConcatenator) then
    FConcatenator.Free;
  inherited Destroy;
end;

procedure TAlignConcatThread.Execute;
begin
  if not Terminated then
  begin
    try
      DoConcatenation;
    except
      on E: Exception do
      begin
        FIsSuccess := False;
        FConcatenator.LogMsg('Error when concatenating files: ' + E.Message);
      end;
    end;
  end;
end;

function TAlignConcatThread.GetSequences: TSequenceList;
begin
  Result := FConcatenator.Sequences;
end;

function TAlignConcatThread.GetAllDomainInfo: TAllDomainInfo;
begin
  Result := FConcatenator.AllDomainInfo;
  FConcatenator.AllDomainInfo := nil;
end;

{ TAlignmentConcatenator }

procedure TAlignmentConcatenator.SetSourceDirectory(AValue: String);
begin
  if FSourceDirectory=AValue then Exit;
  FSourceDirectory:=AValue;
end;

function TAlignmentConcatenator.CheckSpecialChars(gap, ident, miss: Char): Boolean;
begin
  if FSpecialCharsSet then
  begin
    Result := ((gap = FGapSym) and (ident = FIdentSym) and (miss = FMissSym));
  end
  else
  begin
    FSpecialCharsSet := True;
    Result := True;
    FGapSym := gap;
    FIdentSym := ident;
    FMissSym := miss
  end;
end;

procedure TAlignmentConcatenator.InitAlignmentLoader(aFile: String);
var
  extension: String;
begin
  extension := ExtractFileExt(aFile);
  if (extension = MegaExt1) or (extension = MegaExt3) then
  begin
    FAlignmentLoader := TMegaAlignmentFileLoader.Create(aFile, False);
    FAlignmentLoader.IsConcatenatingFiles := True;
    TMegaAlignmentFileLoader(FAlignmentLoader).UpdateExpectedFileInfo(FFileInfo);
  end
  else if (extension = FastaExt1) or (extension = FastaExt2) or (extension = FastaExt3) or (extension = FastaExt4) or (extension = FastaExt5) or (extension = FastaExt6) or (extension = FastaExt7) then
  begin
    FAlignmentLoader := TFastaAlignmentLoader.Create(aFile, FFileInfo, False);
    FAlignmentLoader.IsConcatenatingFiles := True
  end
  else
    raise Exception.Create('Only MEGA and FASTA alignment files are currently supported');
end;

function TAlignmentConcatenator.FindAlignmentFiles: Boolean;
begin
  Result := False;
  if (not FUseFileList) and (not DirectoryExists(FSourceDirectory)) then
  begin
    FLog.Add('Source directory not found(' + FSourceDirectory + ')');
    Exit;
  end;
  if not FUseFileList then
    FFilenames := FindAllFiles(FSourceDirectory, '*.meg;*.fas;*.fasta;*.fst;*.fsta', FDoRecursiveSearch, faAnyFile);
  if FFilenames.Count = 0 then
  begin
    if FUseFileList then
      FLog.Add('No alignment type files found in the source directory')
    else
      FLog.Add('No alignment type files found in the supplied files list');
    Exit;
  end;
  FFilenames.Sort;
  Result := True;
end;

function TAlignmentConcatenator.ProcessFile(aFile: String): Boolean;
var
  j: Integer = -1;
  g: Char = #0;
  i: Char = #0;
  m: Char = #0;
  seqs: TSequenceList = nil;
  aInfo: TDomainInfo = nil;
  domainStart: Int64 = -1;
begin
  Result := True;
  try
    if Assigned(FAlignmentLoader) then
      FreeAndNil(FAlignmentLoader);
    InitAlignmentLoader(aFile);
    Result := FAlignmentLoader.LoadFile;
    if Result then
    begin
      if FAlignmentLoader.TryGetSpecialChars(g, i, m) then
        if not CheckSpecialChars(g, i, m) then
          raise Exception.Create('All files must use the same special symbols for indels, identicals, and missing data');
      if FSequences.Count > 0 then
        domainStart := FSequences.SeqLengths[0]
      else
        domainStart := 0;
      seqs := FAlignmentLoader.SequenceList;
      if FSequences.Count = 0 then
      begin
        FSequences.IsDNA := seqs.IsDNA;
        FSequences.IsProteinCoding := seqs.IsProteinCoding;
      end;
      if not FSequences.AppendSequences(seqs, m, False) then
        raise Exception.Create('Could not resolve alignment to previously loaded alignments');
      if (FSequences.Title = EmptyStr) and (seqs.Title <> EmptyStr) then
        FSequences.Title := seqs.Title;
      aInfo := TDomainInfo.Create;
      aInfo.FromSite := domainStart;
      aInfo.ToSite := FSequences.SeqLengths[0] - 1;
      aInfo.GeneName := ExtractFileNameOnly(aFile);
      aInfo.Name := Format('Data-%d', [FAllDomainInfo.NoOfDomains + 1]);
      aInfo.IsDomain := True;
      aInfo.IsCoding := seqs.IsProteinCoding;
      FAllDomainInfo.Add(aInfo);
    end
    else
      raise Exception.Create('Unkown error');
  except
    on E:Exception do
    begin
      FLog.Add('Exception when processing a file: ' + ExtractFileName(aFile) + '. ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure TAlignmentConcatenator.Finalize;
begin
  FSequences.Finalize;
end;

procedure TAlignmentConcatenator.SetDoRecursiveSearch(AValue: Boolean);
begin
  if FDoRecursiveSearch=AValue then Exit;
  FDoRecursiveSearch:=AValue;
end;

function TAlignmentConcatenator.GetLog: String;
begin
  Result := FLog.Text;
end;

function TAlignmentConcatenator.GetAllDomainInfo: TAllDomainInfo;
begin
  Result := FAllDomainInfo;
end;

function TAlignmentConcatenator.GetNumSeqs: Integer;
begin
  if Assigned(FSequences) then
    Result := FSequences.Count
  else
    Result := 0;
end;

function TAlignmentConcatenator.GetSequences: TFastSequenceList;
begin
  Result := FSequences;
end;

constructor TAlignmentConcatenator.Create(fileInfo: TDataFileInfo);
begin
  FUseFileList := False;
  FFileInfo := fileInfo;
  FFilenames := nil;
  FDoRecursiveSearch := False;
  FLog := TStringList.Create;
  FSequences := TFastSequenceList.Create;
  FGapSym := '-';
  FIdentSym := '.';
  FMissSym := '?';
  FSpecialCharsSet := False;
  FAllDomainInfo := TAllDomainInfo.Create;
end;

destructor TAlignmentConcatenator.Destroy;
begin
  if Assigned(FFilenames) then
    FFilenames.Free;
  if Assigned(FLog) then
    FLog.Free;
  if Assigned(FSequences) then
    FSequences.Free;
  if Assigned(FAlignmentLoader) then
    FAlignmentLoader.Free;
  if Assigned(FAllDomainInfo) then
    FAllDomainInfo.Free;
  inherited Destroy;
end;

function TAlignmentConcatenator.GetMegaAlignmentStrings: TStringList;
begin
  Result := FSequences.GetMegaSequenceAlignmentFileStrings('Concatenated files', FGapSym, FIdentSym, FMissSym);
end;

function TAlignmentConcatenator.NumFiles: Integer;
begin
  Result := FFilenames.Count;
end;

procedure TAlignmentConcatenator.UseFileList(ListOfFiles: TStringList);
begin
  FSourceDirectory := EmptyStr;
  FUseFileList := True;
  if not Assigned(FFilenames) then
     FFilenames := TStringList.Create
  else
    FFilenames.Clear;
  FFilenames.AddStrings(ListOfFiles);
  FDoRecursiveSearch := False;
end;

procedure TAlignmentConcatenator.LogMsg(aMsg: String);
begin
  FLog.Add(aMsg);
end;


end.

