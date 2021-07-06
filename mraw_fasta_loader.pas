{
	Copyright 1992-2021 Sudhir Kumar and Koichiro Tamura

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

unit mraw_fasta_loader;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MegaConsts, MD_Sequences, mchar_scanner, MegaUtils_NV,
  mruntimeprogressdlg;

const
  MEM_INCREMENT = 4096;

type

  { TRawFastaLoader }

  TRawFastaLoader = class(TObject)
    private
      FIsLikelyDna: Boolean;
      FLog: TStringList;
      FIsSuccess: Boolean;
      FUserCancelled: Boolean;
      FLengthOfFirstSeq: Int64;
      function GetLogText: String;
    protected
      FSeqs: TSequenceList;
      FFilename: String;
      FScanner: TCharScanner;
      FTaxa: Array of PAnsiChar;
      FSequence: Array of PAnsiChar;
      FNoOfTaxa: LongInt;
      FGapSymbol: AnsiChar;
      FIdenticalSymbol: AnsiChar;
      FMissingSymbol: AnsiChar;
      FLines: Integer;
      FFoundGreaterThan: Boolean;
      procedure CheckIfLooksLikeDna;
      function ParseGreaterThan: Boolean;
      function ParseName: Boolean;
      function ParseSeqData: Boolean;
      function IsValidNucBase(c: AnsiChar): Boolean; deprecated;
      function IsValidAmino(c: AnsiChar): Boolean; deprecated;
      procedure FilterSequenceNames;
    public
      CheckAbort: TCheckAbortFunc;
      UpdateStatusProc: TRunStatusProc;
      constructor Create(filename: String; gapSym, identSym, missingSym: AnsiChar);
      destructor Destroy; override;
      function Parse: Boolean;
      procedure GetSequences(var seqs: TSequenceList);
      property LogText: String read GetLogText;
      property IsSuccess: Boolean read FIsSuccess;
      property GapSymbol: AnsiChar read FGapSymbol;
      property IdenticalSymbol: AnsiChar read FIdenticalSymbol;
      property MissingSymbol: AnsiChar read FMissingSymbol;
      property UserCancelled: Boolean read FUserCancelled;
      property IsLikelyDna: Boolean read FIsLikelyDna;
  end;

  { TRawFastaLoaderThread }

  TRawFastaLoaderThread = class(TMEGAThread)
    private
      FHeaderStr: AnsiString;
      FGapSymbol: AnsiChar;
      FIdenticalSymbol: AnsiChar;
      FIsLikelyDna: Boolean;
      FIsSuccess: Boolean;
      FMissingSymbol: AnsiChar;
      FOverwrite: Boolean;
      FUserCancelled: Boolean;
      FProgress: TRuntimeProgress;
      function GetIsLikelyDna: Boolean;
      function GetLogText: String;
      function LoadData: Boolean;
      function UpdateHeaderString: Boolean;
    protected
      FStatus: AnsiString;
      FInfo: AnsiString;
      FParser: TRawFastaLoader;
      FLog: TStringList;
      FFileName: String;
      procedure Execute; override;
      procedure DoCheckAbort;
      procedure DoUpdateStatus;
      function CheckAbort: Boolean;
      procedure UpdateStatus(aStatus: AnsiString; aInfo: AnsiString);
      procedure ClearSequences;
    public
      CheckAbortFunc: TCheckAbortFunc;
      UpdateStatusProc: TRunStatusProc;
      Sequences: TSequenceList;
      constructor Create(filename: String; progress: TRuntimeProgress; overwrite: Boolean; gap, ident, missing: AnsiChar);
      destructor Destroy; override;

      property IsSuccess: Boolean read FIsSuccess;
      property UserCancelled: Boolean read FUserCancelled;
      property LogText: String read GetLogText;
      property GapSymbol: AnsiChar read FGapSymbol;
      property IdenticalSymbol: AnsiChar read FIdenticalSymbol;
      property MissingSymbol: AnsiChar read FMissingSymbol;
      property Overwrite: Boolean read FOverwrite;
      property Progress: TRuntimeProgress read FProgress;
      property FileName: String read FFileName;
      property IsLikelyDna: Boolean read FIsLikelyDna;
      property HeaderStr: AnsiString read FHeaderStr;
  end;

implementation

uses
  dateutils, MegaUtils, msequence_name_filter, MAlignGrid;

{ TRawFastaLoaderThread }

function TRawFastaLoaderThread.GetLogText: String;
begin
  Result := FLog.Text;
end;

function TRawFastaLoaderThread.GetIsLikelyDna: Boolean;
begin
  if Assigned(FParser) then
    Result := FParser.IsLikelyDna
  else
    Result := False;
end;

function TRawFastaLoaderThread.LoadData: Boolean;
begin
  try
    Sequences := TSequenceList.Create;
    FParser := TRawFastaLoader.Create(FFilename, FGapSymbol, FIdenticalSymbol, FMissingSymbol);
    FParser.CheckAbort := CheckAbort;
    FParser.UpdateStatusProc := UpdateStatus;
    Result := FParser.Parse;
    if Result then
      FParser.FilterSequenceNames;
    UpdateStatus('Status', 'Initializing Alignment Explorer...');
    FLog.Add(FParser.LogText);
    if Result then
      FParser.GetSequences(Sequences);
    FIsLikelyDna := FParser.IsLikelyDna;
    Result := Result and UpdateHeaderString;
  finally
    if Assigned(FParser) then
      FreeAndNil(FParser);
  end;
end;

function TRawFastaLoaderThread.UpdateHeaderString: Boolean;
var
  t: TUpdateHeaderStringThread = nil;
  numSites: Integer;
begin
  Result := False;
  try
    numSites := Sequences.GetMaxNoOfSites;
    SetLength(FHeaderStr, numSites);
    t := TUpdateHeaderStringThread.Create(FHeaderStr, Sequences, frConserved, 1);
    t.UpdateStatusProc := UpdateStatus;
    t.CheckAbort := CheckAbort;
    t.Start;
    t.WaitFor;
    if t.IsSuccess and (not t.IsCancelled) then
    begin
      FHeaderStr := t.HeaderString;
      Result := Length(FHeaderStr) = numSites;
    end;
  finally
    if Assigned(t) then
      t.Free;
  end;
end;

procedure TRawFastaLoaderThread.Execute;
begin
  try
    while True do
    begin
      FIsSuccess := LoadData;
      Terminate;
      Exit;
    end;
  except
    on E:EAbort Do
    begin
      FUserCancelled := True;
      FLog.Add(E.Message);
    end;
    on E:Exception do
    begin
      FIsSuccess := False;
      FLog.Add(E.Message);
    end
  end;
end;

procedure TRawFastaLoaderThread.DoCheckAbort;
begin
  if Assigned(CheckAbortFunc) then
    FUserCancelled := CheckAbortFunc;
end;

procedure TRawFastaLoaderThread.DoUpdateStatus;
begin
  if Assigned(UpdateStatusProc) then
    UpdateStatusProc(FStatus, FInfo);
end;

function TRawFastaLoaderThread.CheckAbort: Boolean;
begin
  Synchronize(DoCheckAbort);
  Result := FUserCancelled;
end;

procedure TRawFastaLoaderThread.UpdateStatus(aStatus: AnsiString; aInfo: AnsiString);
begin
  FStatus := aStatus;
  FInfo := aInfo;
  Synchronize(DoUpdateStatus);
end;

procedure TRawFastaLoaderThread.ClearSequences;
var
  i: Integer;
begin
  if Assigned(Sequences) and (Sequences.Count > 0) then
    for i := 0 to Sequences.Count - 1 do
      Sequences.GetItems(i).Free;
end;

constructor TRawFastaLoaderThread.Create(filename: String; progress: TRuntimeProgress; overwrite: Boolean; gap, ident, missing: AnsiChar);
begin
  inherited Create(True);
  FIsLikelyDna := False;
  FFileName := filename;
  FreeOnTerminate := True;
  FLog := TStringList.Create;
  FProgress := progress;
  if Assigned(FProgress) then
  begin
    CheckAbortFunc := progress.GetUserCancelled;
    UpdateStatusProc := progress.UpdateRunStatusInfo;
  end;
  FGapSymbol := gap;
  FIdenticalSymbol := ident;
  FMissingSymbol := missing;
  Sequences := nil;
  FOverwrite := overwrite;
end;

destructor TRawFastaLoaderThread.Destroy;
begin
  FProgress := nil;
  if Assigned(FLog) then
    FLog.Free;
  if Assigned(FParser) then
    FParser.Free;
  if Assigned(Sequences) then
  begin
    if (not FIsSuccess) or FUserCancelled then { otherwise, TAlignGrid took ownership}
      ClearSequences;
    Sequences.Free;
  end;
  inherited Destroy;
end;

{ TRawFastaLoader }

function TRawFastaLoader.GetLogText: String;
begin
  if Assigned(FLog) then
    Result := FLog.Text
  else
    Result := EmptyStr;
end;

procedure TRawFastaLoader.CheckIfLooksLikeDna;
begin
  FIsLikelyDna := FSeqs.LooksLikeDna;
end;

function TRawFastaLoader.ParseGreaterThan: Boolean;
var
  letter: AnsiChar = #0;
begin
  Result := FScanner.Read(letter);
  if letter <> '>' then
    Raise Exception.Create('Fasta parse error.  Expected ''>'', but found ' + letter + '. Please make sure that your FASTA file is formatted correctly.');
end;

function TRawFastaLoader.ParseName: Boolean;
var
  charIn: AnsiChar = #0;
  Name: AnsiString = '';
  PName: PAnsiChar;
  Lines: Integer = 0;
begin
  Result := False;
  while FScanner.HasNext do
  begin
    FScanner.Read(charIn);
    if (charIn = #10) or (charIn = #13) then
      break;
    Name := Name + charIn;
  end;
  if charIn = #13 then // If we read in a CR then there's still a LF which has to be read off before we have the next line.
    FScanner.Read(charIn);
  inc(Lines);
  TrimTaxaName(Name);
  Result := (Name <> EmptyStr);
  if (Name = EmptyStr) then
    Raise Exception.Create('Fasta parse error.  Expected a name to follow ''>'', but none found');

  inc(FNoOfTaxa);
  if Length(FTaxa) = FNoOfTaxa then
  begin
    SetLength(FTaxa, FNoOfTaxa + MEM_INCREMENT);
    SetLength(FSequence, FNoOfTaxa + MEM_INCREMENT);
  end;
  GetMem(PName, (sizeof(AnsiChar)*length(Name))+1);
  StrCopy(PName, PAnsiChar(Name));
  FTaxa[FNoOfTaxa-1] := PName;
end;

function TRawFastaLoader.ParseSeqData: Boolean;
var
  charIn: AnsiChar = #0;
  siteNo: Integer = 0;
  seq: AnsiString = '';
  PSeq: PAnsiChar;
begin
  Result := False;
  FFoundGreaterThan := False;
  while (not FFoundGreaterThan) and FScanner.HasNext do
  begin
    if not FScanner.Read(charIn) then
      break;
    if (charIn = #13) or (charIn = #10) or (charIn = ' ') then
    begin
      if charIn = #10 then
        inc(FLines);
    end
    else
    begin
      if charIn = '>' then
        FFoundGreaterThan := True
      else
      begin
        inc(SiteNo);
        if charIn = FIdenticalSymbol then
        begin
          if FNoOfTaxa = 1 then
            Raise Exception.Create('Fasta parse error.  An identical symbol was found in the first sequence.');
          if SiteNo > FLengthOfFirstSeq then
            raise Exception.Create(Format('Identical base symbol found at site %d  for sequence %d but that sequence is longer than the first sequence', [siteNo, FNoOfTaxa]));
          charIn := FSequence[0][SiteNo];
        end;

        if Length(Seq) < SiteNo then
          SetLength(Seq, Length(Seq) + 10000);
        Seq[SiteNo] := charIn;
      end;
    end;
  end;
  SetLength(Seq, SiteNo);
  if (Trim(Seq) = EmptyStr) then
    Raise Exception.Create('Fasta data parsing error.  Expected a sequence, but none found.');
  if FNoOfTaxa = 1 then
    FLengthOfFirstSeq := Length(Seq);
  GetMem(PSeq, (sizeof(AnsiChar)*Length(Seq))+1);
  StrCopy(PSeq, PAnsiChar(Seq));
  FSequence[FNoOfTaxa-1] := PSeq;
  Result := True;
end;

function TRawFastaLoader.IsValidNucBase(c: AnsiChar): Boolean;
begin
  if (c = FMissingSymbol) or (c = FIdenticalSymbol) or (c = FGapSymbol) then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  case UpCase(c) of
  'A'..'D','G','H','K','M','R','S','T','U','V','W','Y','N': Result := True;
  end;
end;

function TRawFastaLoader.IsValidAmino(c: AnsiChar): Boolean;
begin
  if (c = FMissingSymbol) or (c = FIdenticalSymbol) or (c = FGapSymbol) then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  case UpCase(c) of
  'A'..'I','K'..'N', 'P'..'T', 'V'..'Z', '*': Result := True;
  end;
end;

procedure TRawFastaLoader.FilterSequenceNames;
var
  filter: TSequenceNameFilter = nil;
  i: Integer;
  s: TSequence = nil;
  updateTime: TDateTime;
begin
  try
    try
      updateTime := Now;
      filter := TSequenceNameFilter.Create(FSeqs, MMaxTaxaNameLen);
      if FSeqs.Count > 0 then
        for i := 0 to FSeqs.Count - 2 do
        begin
          s := FSeqs.GetItems(i);
          filter.FilterName(s, i);
          if Assigned(UpdateStatusProc) and (MillisecondsBetween(Now, updateTime) > 500) then
          begin
            UpdateStatusProc('Status', Format('checking sequence names %.0n', [FSeqs.Count*(i/FSeqs.Count)]));
            updateTime := Now;
            if Assigned(CheckAbort) then
              if CheckAbort then
                raise EAbort.Create('user cancelled');
          end;
        end;
    except
      on E:EAbort do
      begin
        FLog.Add(E.Message);
        FUserCancelled := True;
        FIsSuccess := False;
      end;
      on E:Exception do
      begin
        FIsSuccess := False;
        FLog.Add(E.Message);
      end;
    end;
  finally
    if Assigned(filter) then
      filter.Free;
  end;
end;

constructor TRawFastaLoader.Create(filename: String; gapSym, identSym, missingSym: AnsiChar);
begin
  FLengthOfFirstSeq := 0;
  FFoundGreaterThan := False;
  FFilename := filename;
  FScanner := nil;
  FSeqs := nil;
  FLog := TStringList.Create;
  FIsSuccess := False;
  CheckAbort := nil;
  UpdateStatusProc := nil;
  FGapSymbol := gapSym;
  FIdenticalSymbol := identSym;
  FMissingSymbol := missingSym;
  FLines := 0;
  FUserCancelled := False;
end;

destructor TRawFastaLoader.Destroy;
var
  i: Integer;
begin
  if Assigned(FScanner) then
    FScanner.Free;
  if Assigned(FSeqs) then
    FSeqs.Free;
  if Assigned(FLog) then
    FLog.Free;
  if Length(FTaxa) > 0 then
    for i := Low(FTaxa) to High(FTaxa) do
      FreeMem(FTaxa[i]);
  if Length(FSequence) > 0 then
    for i := Low(FSequence) to High(FSequence) do
      FreeMem(FSequence[i]);
  inherited Destroy;
end;

function TRawFastaLoader.Parse: Boolean;
var
  updateTime: TDateTime;
  i: Integer;
  aSeq: TSequence = nil;
begin
  try
    try
      if not FileExists(FFilename) then
        raise Exception.Create(Format('input fasta file not found', [FFilename]));
      updateTime := Now;
      FScanner := TCharScanner.Create(FFilename);
      SetLength(FTaxa, MEM_INCREMENT);
      SetLength(FSequence, MEM_INCREMENT);
      ParseGreaterThan;
      while FScanner.HasNext do
      begin
        if (MilliSecondsBetween(Now, updateTime) > 200) then
        begin
          if Assigned(CheckAbort) then
            if CheckAbort then
              raise EAbort.Create('user cancelled');
          if Assigned(UpdateStatusProc) then
            UpdateStatusProc('Status', Format('Found %.0n sequences', [FNoOfTaxa*1.0]));
          updateTime := Now;
        end;
        ParseName;
        ParseSeqData;
      end;
      SetLength(FTaxa, FNoOfTaxa);
      SetLength(FSequence, FNoOfTaxa);
      FSeqs := TSequenceList.Create;
      if FNoOfTaxa > 0 then
        for i := 0 to FNoOfTaxa - 1 do
        begin
          aSeq := TSequence.Create;
          aSeq.FFileName := FFilename;
          aSeq.SeqName := FTaxa[i];
          aSeq.SeqData := FSequence[i];
          FSeqs.Add(aSeq);
        end;
      CheckIfLooksLikeDna;
      Result := FSeqs.Count > 0;
    except
      on E:EAbort do
      begin
        FLog.Add(E.Message);
        FUserCancelled := True;
        FIsSuccess := False;
      end;
      on E:Exception do
      begin
        FLog.Add(E.Message);
        FIsSuccess := False;
      end;
    end;
  finally
    if Assigned(FScanner) then
      FreeAndNil(FScanner);
  end;
end;

procedure TRawFastaLoader.GetSequences(var seqs: TSequenceList);
var
  i: Integer;
begin
  if Assigned(FSeqs) and (FSeqs.Count > 0) then
    for i := 0 to FSeqs.Count - 1 do
      seqs.Add(FSeqs.GetItems(i));
  FSeqs.RelinquishSequences
end;

end.

