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

unit mesl_input_data;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MD_InputSeqData, MegaConsts;
type
  IPrepareEslInputData = interface
    ['{AB228954-C625-485D-ADCA-100FA6CBA4E6}']
    function PrepareEslData: Boolean;
    procedure SetNewLineProc(aProc: TNewLineProc);
    function ErrorMsg: String;
    function NumFiles: Integer;
    function NumSequences: Integer;
  end;

  { TAbstractPrepareEslInputData }

  TAbstractPrepareEslInputData = class abstract (TInterfacedObject, IPrepareEslInputData)
    protected
      FNewLineProc: TNewLineProc;
      FTargetDir: String;
      FNumFiles: Integer;
      FNumSequences: Integer;
      FErrorLog: TStringList;
      FSeqNames: TStringList;
      procedure UpdateSeqNames(fastaFile: String);
    public
      constructor Create;
      destructor Destroy; override;
      function PrepareEslData: Boolean; virtual abstract;
      procedure SetNewLineProc(aProc: TNewLineProc);
      function ErrorMsg: String;
      function NumFiles: Integer;
      function NumSequences: Integer;
  end;

  { TCopyFastaFiles }

  TCopyFastaFiles = class(TAbstractPrepareEslInputData)
    private
      FSourceDir: String;
      FAlignmentsListFile: String;
      FAlignmentsDir: String;
    public
      constructor Create(sourceDir: String; targetDir: String);
      destructor Destroy; override;
      function PrepareEslData: Boolean; override;
  end;

  { TDomainsToFastaFiles }

  TDomainsToFastaFiles = class(TAbstractPrepareEslInputData)
    private
      FSeqData: TD_InputSeqData;
    public
      constructor Create(seqData: TD_InputSeqData; targetDir: String);
      destructor Destroy; override;
      function PrepareEslData: Boolean; override;
  end;

  { TSegmentsToFastaFiles }

  TSegmentsToFastaFiles = class(TAbstractPrepareEslInputData)
    private
      FSeqData: TD_InputSeqData;
      FSegmentSize: Integer;
    public
      constructor Create(seqData: TD_InputSeqData; targetDir: String; segmentSize: Integer);
      destructor Destroy; override;
      function PrepareEslData: Boolean; override;
  end;

implementation

uses
  FileUtil, LazFileUtils, mseqdataexport, math;

{ TAbstractPrepareEslInputData }

procedure TAbstractPrepareEslInputData.UpdateSeqNames(fastaFile: String);
var
  tempStrings: TStringList = nil;
  tempName: String = '';
  i: Integer = 0;
begin
  try
    tempStrings := TStringList.Create;
    tempStrings.LoadFromFile(fastaFile);
    if tempStrings.Count > 0 then
      for i := 0 to tempStrings.Count - 1 do
      begin
        tempName := Trim(tempStrings[i]);
        if (tempName <> EmptyStr) and (tempName[1] = '>') then
        begin
          if Length(tempName) > 1 then
          begin
            tempName := Copy(tempName, 2, Length(tempName));
            FSeqNames.Add(tempName);
          end;
        end;
      end;
  finally
    if Assigned(tempStrings) then
      tempStrings.Free;
  end;
end;

constructor TAbstractPrepareEslInputData.Create;
begin
  FErrorLog := TStringList.Create;
  FSeqNames := TStringList.Create;
  FSeqNames.Sorted := True;
  FSeqNames.Duplicates := dupIgnore;
  FNumSequences := 0;
  FNumFiles := 0;
  FNewLineProc := nil;
  FTargetDir := EmptyStr;
end;

destructor TAbstractPrepareEslInputData.Destroy;
begin
  if Assigned(FErrorLog) then
    FErrorLog.Free;
  inherited Destroy;
end;

procedure TAbstractPrepareEslInputData.SetNewLineProc(aProc: TNewLineProc);
begin
  FNewLineProc := aProc;
end;

function TAbstractPrepareEslInputData.ErrorMsg: String;
begin
  Result := FErrorLog.Text;
end;

function TAbstractPrepareEslInputData.NumFiles: Integer;
begin
  Result := FNumFiles;
end;

function TAbstractPrepareEslInputData.NumSequences: Integer;
begin
  Result := FNumSequences;
end;

{ TSegmentsToFastaFiles }

constructor TSegmentsToFastaFiles.Create(seqData: TD_InputSeqData; targetDir: String; segmentSize: Integer);
begin
  inherited Create;
  FSeqData := seqData;
  FTargetDir := targetDir;
  FSegmentSize := segmentSize;
end;

destructor TSegmentsToFastaFiles.Destroy;
begin
  inherited Destroy;
end;

function TSegmentsToFastaFiles.PrepareEslData: Boolean;
var
  relativePaths: TStringList = nil;
  alignmentsListFile: String = '';
  aPath: String = '';
  alignmentsDirName: String = '';
  i: Integer = 0;
begin
  try
    Result := False;
    if not DirectoryExists(FTargetDir) then
      if not ForceDirectoriesUTF8(FTargetDir) then
        raise Exception.Create('failed to create target directory needed for DrPhylo: ' + FTargetDir);
    if not ExportDataInSegmentsToFastaFiles(D_InputSeqData, FTargetDir, FSegmentSize, FNewLineProc) then
      raise Exception.Create('failed to copy segments to target directory for DrPhylo: ' + FTargetDir);
    relativePaths := FindAllFiles(FTargetDir, EmptyStr, False);
    alignmentsDirName := ExtractFileName(FTargetDir);
    for i := 0 to relativePaths.Count - 1 do
    begin
      UpdateSeqNames(relativePaths[i]);
      aPath := alignmentsDirName + PathDelim + ExtractFileName(relativePaths[i]);
      relativePaths[i] := aPath;
    end;
    alignmentsListFile := ExtractFileDir(FTargetDir) + PathDelim + 'alignment_files.txt';
    relativePaths.SaveToFile(alignmentsListFile);
    Result := FileExists(alignmentsListFile);
    FNumFiles := relativePaths.Count;
    FNumSequences := FSeqNames.Count;
  finally
    if Assigned(relativePaths) then
      relativePaths.Free;
  end;
end;

{ TDomainsToFastaFiles }

constructor TDomainsToFastaFiles.Create(seqData: TD_InputSeqData; targetDir: String);
begin
  inherited Create;
  FSeqData := seqData;
  FTargetDir := targetDir;
end;

destructor TDomainsToFastaFiles.Destroy;
begin
  inherited Destroy;
end;

function TDomainsToFastaFiles.PrepareEslData: Boolean;
var
  relativePaths: TStringList = nil;
  alignmentsListFile: String = '';
  aPath: String = '';
  alignmentsDirName: String = '';
  i: Integer = 0;
begin
  try
    Result := False;
    if not DirectoryExists(FTargetDir) then
      if not ForceDirectoriesUTF8(FTargetDir) then
        raise Exception.Create('failed to create target directory needed for DrPhylo: ' + FTargetDir);
    if not ExportActiveDomainsToFastaFiles(D_InputSeqData, FTargetDir, FNewLineProc) then
      raise Exception.Create('failed to copy domains to target directory for DrPhylo: ' + FTargetDir);
    relativePaths := FindAllFiles(FTargetDir, EmptyStr, False);
    alignmentsDirName := ExtractFileName(FTargetDir);
    for i := 0 to relativePaths.Count - 1 do
    begin
      UpdateSeqNames(relativePaths[i]);
      aPath := alignmentsDirName + PathDelim + ExtractFileName(relativePaths[i]);
      relativePaths[i] := aPath;
    end;
    FNumFiles := relativePaths.Count;
    FNumSequences := FSeqNames.Count;
    alignmentsListFile := ExtractFileDir(FTargetDir) + PathDelim + 'alignment_files.txt';
    relativePaths.SaveToFile(alignmentsListFile);
    Result := FileExists(alignmentsListFile);
  finally
    if Assigned(relativePaths) then
      relativePaths.Free;
  end;
end;

{ TCopyFastaFiles }

constructor TCopyFastaFiles.Create(sourceDir: String; targetDir: String);
begin
  inherited Create;
  FSourceDir := sourceDir;
  FTargetDir := targetDir;
end;

destructor TCopyFastaFiles.Destroy;
begin
  inherited Destroy;
end;

function TCopyFastaFiles.PrepareEslData: Boolean;
var
  aList: TStringList = nil;
  relativePaths: TStringLIst = nil;
  i: Integer = -1;
  tempFile: String = '';
  aProgress: Integer = 0;
begin
  Result := False;
  try
    FAlignmentsDir := FTargetDir + PathDelim + 'alignment_files';
    if DirectoryExists(FAlignmentsDir) then { should never happen as a new directory is created for every run}
      raise Exception.Create('bad environment: FAlignmentsDir already exists - '  + FAlignmentsDir);

    if not ForceDirectoriesUTF8(FAlignmentsDir) then
      raise Exception.Create('bad environment: unable to create FAlignmentsDir - ' + FAlignmentsDir);

    relativePaths := TStringList.Create;
    aList := FindAllFiles(FSourceDir, EmptyStr, False);
    if aList.Count = 0 then
      raise Exception.Create('zero input alignment files found');
    for i := 0 to aList.Count - 1 do
    begin
      UpdateSeqNames(aList[i]);
      tempFile := ExtractFileName(aList[i]);
      tempFile := FAlignmentsDir + PathDelim + tempFile;
      aProgress := max(1, Round((i + 1)/aList.Count*100));
      if Assigned(FNewLineProc) then
        FNewLineProc(Format('%d%% preparing file %s', [aProgress, ExtractFileName(aList[i])]));
      if not CopyFile(aList[i], tempFile, False, False) then
        raise Exception.Create(Format('failed to copy input file %s to alignments directory %s', [aList[i], tempFile]));
      tempFile := ExtractFileName(FAlignmentsDir) + PathDelim + ExtractFileName(aList[i]);
      relativePaths.Add(tempFile);
    end;
    FNumSequences := FSeqNames.Count;
    FNumFiles := aList.Count;
    FAlignmentsListFile := FTargetDir + PathDelim + ExtractFileName(FAlignmentsDir) + '.txt';
    relativePaths.SaveToFile(FAlignmentsListFile);
    Result := FileExists(FAlignmentsListFile);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

end.

