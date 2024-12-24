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

unit mseqdataexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MD_InputSeqData, MegaConsts, mseqexportoptions, ExcelWrite,
  MegaUtils_NV, MDomainInfo;

type

  { TExportSeqDataThread }

  TExportSeqDataThread = class(TThread)
    private
      FExcelFile: TExcelWrite;
      FIsCancelled: Boolean;
      FIsSuccess: Boolean;
      FMsgLog: TStringList;
      FSeqData: TD_InputSeqData;
      procedure SetSeqData(AValue: TD_InputSeqData);
    protected
      FProgress: Integer;
      FMsg: String;
      FOptions: TSeqExportOptions;
      FSaveTo: String;
      FOutList: TStringList;
      function ExportProgress(p: Integer; msg: String): Boolean;
      procedure DoExportProgress;
      function ProcessData: Boolean;
      procedure Execute; override;
    public

      CheckCancelFunc: TProgressCheckCancelFunc;

      constructor Create(aSeqData: TD_InputSeqData; aOptions: TSeqExportOptions; aSaveTo: String; aResultOutput: TStringList);
      destructor Destroy; override;
      property SeqData: TD_InputSeqData read FSeqData write SetSeqData;
      property IsSuccess: Boolean read FIsSuccess;
      property MsgLog: TStringList read FMsgLog;
      property FileDestination: String read FSaveTo;
      property ExportOptions: TSeqExportOptions read FOptions write FOptions;
      property ExcelFile: TExcelWrite read FExcelFile;
  end;

  { TExportActiveDomainsToFastaThread }

  TExportActiveDomainsToFastaThread = class(TMegaThread)
    private
      FSelectedDomains: TArrayOfBoolean;
      FIndependentsSelected: Boolean;
      FOptions: TSeqExportOptions;
      FSeqData: TD_InputSeqData;
      FActiveDomainInfo: TAllDomainInfo;
      FLog: TStringList;
      FTargetDirectory: String;
      FTempList: TStringList;
      FProgress: Integer;
      FCurrentDomain: String;
      FCancelled: Boolean;

      procedure InitOptions;
      procedure LaunchExportThread(destination: String);
      function FilenameForDomainInfo(aInfo: TDomainInfo; fileNumber: Integer): String;
    protected
      procedure DoNewLine; virtual;
      procedure Execute; override;
      function ProcessData: Boolean; virtual;
      procedure LogMessage(aMsg: String);
      procedure RestoreDomainInfoState;
      procedure SaveDomainInfoState;
    public
      NewLineProc: TNewLineProc;
      constructor Create(aSeqData: TD_InputSeqData; targetDirectory: String);
      destructor Destroy; override;
      procedure WriteLogToFile(filename: String);
      property Log: TStringList read FLog;
  end;

  { TExportDataSeqmentsToFastaThread }

  TExportDataSeqmentsToFastaThread = class(TExportActiveDomainsToFastaThread)
    private
      FTempDomainInfo: TDomainInfo;
      FOrigDomainMarks: TList;
      FSegmentSize: Integer;
    protected
      procedure SaveDomainMarksState;
      procedure RestoreDomainMarksState;
      function ProcessData: Boolean; override;
      procedure DoNewLine; override;
    public
      constructor Create(aSeqData: TD_InputSeqData; targetDirectory: String; segmentSize: Integer);
      destructor Destroy; override;
      property SegmentSize: Integer read FSegmentSize;
  end;

  function ExportActiveDomainsToFastaFiles(aSeqData: TD_InputSeqData; targetDirectory: String; updateProc: TNewLineProc): Boolean;
  function ExportDataInSegmentsToFastaFiles(aSeqData: TD_InputSeqData; targetDirectory: String; segmentSize: Integer; updateProc: TNewLineProc): Boolean;

implementation

uses
  MVS_SeqDataExplorer, Dialogs, math;

function ExportActiveDomainsToFastaFiles(aSeqData: TD_InputSeqData; targetDirectory: String; updateProc: TNewLineProc): Boolean;
var
  t: TExportActiveDomainsToFastaThread = nil;
begin
  if aSeqData.AllDomainInfo.NoOfDomains > 0 then
    Result := True
  else
    Exit(False);

  try
    try
      t := TExportActiveDomainsToFastaThread.Create(aSeqData, targetDirectory);
      t.NewLineProc := updateProc;
      t.FreeOnTerminate := False;
      t.Start;
      t.WaitFor;
    except
      on E:Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        ShowMessage('Error exporting active domains: ' + E.Message);
        {$ELSE}
        t.WriteLogToFile(NextAvailableFilenameNV('.txt'));
        error_nv('Error exporting active domains', E);
        {$ENDIF}
        Result := False;
      end;
    end;
  finally
    if Assigned(t) then
      t.Free;
  end;
end;

function ExportDataInSegmentsToFastaFiles(aSeqData: TD_InputSeqData; targetDirectory: String; segmentSize: Integer; updateProc: TNewLineProc): Boolean;
var
  t: TExportDataSeqmentsToFastaThread = nil;
begin
  if aSeqData.NoOfSites > segmentSize then
    Result := True
  else
    Exit(False);

  try
    try
      t := TExportDataSeqmentsToFastaThread.Create(aSeqData, targetDirectory, segmentSize);
      t.NewLineProc := updateProc;
      t.FreeOnTerminate := False;
      t.Start;
      t.WaitFor;
    except
      on E:Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        ShowMessage('Error exporting data segments: ' + E.Message);
        {$ELSE}
        t.WriteLogToFile(NextAvailableFilenameNV('.txt'));
        error_nv('Error exporting data segments', E);
        {$ENDIF}
        Result := False;
      end;
    end;
  finally
    if Assigned(t) then
      t.Free;
  end;
end;

{ TExportDataSeqmentsToFastaThread }

procedure TExportDataSeqmentsToFastaThread.SaveDomainMarksState;
var
  i: Integer = 0;
  aInfo: TDomainInfo = nil;
begin
  for i := 1 to FSeqData.NoOfSites do
  begin
    aInfo := FSeqData.DomainMark[i - 1];
    FOrigDomainMarks.Add(aInfo);
  end;
end;

procedure TExportDataSeqmentsToFastaThread.RestoreDomainMarksState;
var
  i: Integer = 0;
  aInfo: TDomainInfo = nil;
  dMarks: TAllSiteDomainMark = nil;
begin
  Assert(FOrigDomainMarks.Count = FSeqData.NoOfSites);
  dMarks := FSeqData.DomainMarks;
  for i := 0 to FOrigDomainMarks.Count - 1 do
  begin
    aInfo := TDomainInfo(FOrigDomainMarks[i]);
    dMarks[i] := aInfo;
  end;
end;

function TExportDataSeqmentsToFastaThread.ProcessData: Boolean;
var
  aSite: Integer = 1;
  index: Integer = 0;
  filename: String = '';
  numSegments: Integer = -1;
  fromSite: Integer = 0;
  toSite: Integer = -1;
  dMarks: TAllSiteDomainMark = nil;
  sitesRemaining: Integer = 0;
begin
  Result := False;
  try
    SaveDomainInfoState;
    SaveDomainMarksState;
    FActiveDomainInfo := FSeqData.AllDomainInfo;
    LogMessage(Format('preparing to data into segments of %.0n sites', [FSegmentSize*1.0]));
    FIndependentsSelected := FSeqData.DomainMarks.IsUsedIndependent;
    FSeqData.DomainMarks.IsUsedIndependent := False;
    numSegments := (FSeqData.NoOfSites div FSegmentSize) + 1;
    FTempDomainInfo := TDomainInfo.Create;
    FTempDomainInfo.IsUsed := True;
    FActiveDomainInfo.UnselectAll;
    FActiveDomainInfo.Add(FTempDomainInfo);
    toSite := fromSite + FSegmentSize - 1;
    if toSite > FSeqData.NoOfSites then
      toSite := FSeqData.NoOfSites - 1;
    sitesRemaining := FSeqData.NoOfSites - fromSite; { use from site so the while loop executes at least once}
    while sitesRemaining > 0 do
    begin
      FTempDomainInfo.FromSite := fromSite;
      FTempDomainInfo.ToSite :=  toSite;
      FTempDomainInfo.Name := Format('sites_%d_to_%d', [fromSite + 1, toSite + 1]);
      dMarks := FSeqData.DomainMarks;
      for aSite := fromSite to toSite do
        dMarks[aSite] := FTempDomainInfo;
      FProgress := max(1, Round(index/numSegments*100));
      inc(index);
      Synchronize(@DoNewLine);
      Result := True;
      filename := FilenameForDomainInfo(FTempDomainInfo, index);
      LaunchExportThread(filename);

      for aSite := fromSite to toSite do
        dMarks[aSite] := nil;

      sitesRemaining := FSeqData.NoOfSites - toSite - 1;
      if sitesRemaining > 0 then
      begin
        fromSite += FSegmentSize;
        if fromSite > FSeqData.NoOfSites then
          fromSite := FSeqData.NoOfSites;
        toSite := fromSite + FSegmentSize - 1;
        if toSite > (FSeqData.NoOfSites - 1) then
          toSite := (FSeqData.NoOfSites - 1);
      end;
    end;
  finally
    if Assigned(FTempDomainInfo) then
    begin
      FActiveDomainInfo.Remove(FTempDomainInfo);
      FreeAndNil(FTempDomainInfo);
    end;
    RestoreDomainInfoState;
    RestoreDomainMarksState;
  end;
end;

procedure TExportDataSeqmentsToFastaThread.DoNewLine;
begin
  if Assigned(NewLineProc) then
    NewLineProc(Format('%d%% generating segment data %s', [FProgress, FTempDomainInfo.Name]));
end;

constructor TExportDataSeqmentsToFastaThread.Create(aSeqData: TD_InputSeqData; targetDirectory: String; segmentSize: Integer);
begin
  inherited Create(aSeqData, targetDirectory);
  FSegmentSize := segmentSize;
  FOrigDomainMarks := TList.Create;
end;

destructor TExportDataSeqmentsToFastaThread.Destroy;
begin
  if Assigned(FOrigDomainMarks) then
    FOrigDomainMarks.Free;
  inherited Destroy;
end;

{ TExportActiveDomainsToFastaThread }

procedure TExportActiveDomainsToFastaThread.RestoreDomainInfoState;
var
  i: Integer = 0;
begin
  if FActiveDomainInfo.NoOfDomains > 0 then
    for i := 0 to FActiveDomainInfo.NoOfDomains - 1 do
    begin
      if Assigned(FActiveDomainInfo[i]) then
        FActiveDomainInfo[i].IsUsed := FSelectedDomains[i];
    end;
  FSeqData.DomainMarks.IsUsedIndependent := FIndependentsSelected;
end;

procedure TExportActiveDomainsToFastaThread.InitOptions;
begin
  FOptions := TSeqExportOptions.Create;
  FOptions.ExportFormat := mefFasta;
  FOptions.IsInMainThread := False;
  FOptions.isNucData := FSeqData.IsNuc and (not VS_SeqDataExplorer.IsTranslated);
  FOptions.SitesPerLine := 80;
  FOptions.IsFastaFormat := True;
  FOptions.IsInterleaved := False;
end;

procedure TExportActiveDomainsToFastaThread.LaunchExportThread(destination: String);
var
  t: TExportSeqDataThread = nil;
begin
  try
    LogMessage(Format('Processing file - %s', [destination]));
    FTempList.Clear;
    t := TExportSeqDataThread.Create(FSeqData, FOptions, destination, FTempList);
    t.FreeOnTerminate := False;
    t.Start;
    t.WaitFor;
    t.ExportOptions := nil;
  finally
    if Assigned(t) then
      t.Free;
  end;
end;

function TExportActiveDomainsToFastaThread.FilenameForDomainInfo(aInfo: TDomainInfo; fileNumber: Integer): String;
var
  aName: String = '';
begin
  if Trim(aInfo.GeneName) <> EmptyStr then
    aName := Trim(aInfo.GeneName)
  else if Trim(aInfo.Name) <> EmptyStr then
    aName := Trim(aInfo.Name)
  else
    aName := 'nameless_domain';
  {$IFDEF VISUAL_BUILD}
  Result := Format('%s%s%s.fas', [FTargetDirectory, PathDelim, aName]);
  {$ELSE}
  Result := Format('%s%s%d_%s.fas', [FTargetDirectory, PathDelim, fileNumber, aName]);
  {$ENDIF}
end;

procedure TExportActiveDomainsToFastaThread.DoNewLine;
begin
  if Assigned(NewLineProc) then
    NewLineProc(Format('%d%% generating domain data %s', [FProgress, FCurrentDomain]));
end;

procedure TExportActiveDomainsToFastaThread.Execute;
begin
  ProcessData;
  Terminate;
end;

function TExportActiveDomainsToFastaThread.ProcessData: Boolean;
var
  i: Integer = -1;
  index: Integer = 1;
  filename: String = '';
begin
  Result := False;
  try
    SaveDomainInfoState;
    LogMessage(Format('preparing to process %d domain infos', [FActiveDomainInfo.NoOfDomains]));
    FSeqData.DomainMarks.IsUsedIndependent := False;
    for i := 0 to FActiveDomainInfo.NoOfDomains - 1 do
    begin
      FActiveDomainInfo.UnselectAll;
      if Assigned(FActiveDomainInfo[i]) and FSelectedDomains[i] then
      begin
        FActiveDomainInfo[i].IsUsed := True;
        FProgress := max(1, Round((i + 1)/FActiveDomainInfo.NoOfDomains*100));
        if FActiveDomainInfo[i].GeneName <> EmptyStr then
          FCurrentDomain := FActiveDomainInfo[i].GeneName
        else
          FCurrentDomain := FActiveDomainInfo[i].Name;
        Synchronize(@DoNewLine);
        Result := True;
        filename := FilenameForDomainInfo(FActiveDomainInfo[i], index);
        inc(index);
        LaunchExportThread(filename);
      end
      else
      begin
        if Assigned(FActiveDomainInfo[i]) then
          LogMessage(Format('skipping unused domain info %d %s %s', [i + 1, FActiveDomainInfo[i].GeneName, FActiveDomainInfo[i].Name]))
        else
          LogMessage(Format('skipping nil domain info %d', [i + 1]));
      end;
    end;

    if FIndependentsSelected and (FSeqData.DomainMarks.IndependentCount > 0) then
    begin
      FActiveDomainInfo.UnselectAll;
      FSeqData.DomainMarks.IsUsedIndependent := True;
      filename := Format('%s%s%s.fas', [FTargetDirectory, PathDelim, 'independent']);
      LaunchExportThread(filename);
    end;
  finally
    RestoreDomainInfoState;
  end;
end;

procedure TExportActiveDomainsToFastaThread.LogMessage(aMsg: String);
begin
  FLog.Add(Format('%s %s', [FormatDateTime('YYYY-MM-DD hh:mm:ss', Now), aMsg]));
end;

procedure TExportActiveDomainsToFastaThread.SaveDomainInfoState;
var
  i: Integer = -1;
begin
  SetLength(FSelectedDomains, FSeqData.AllDomainInfo.NoOfDomains);
  for i := Low(FSelectedDomains) to High(FSelectedDomains) do
    if FSeqData.AllDomainInfo[i].IsUsed then
      FSelectedDomains[i] := True
    else
      FSelectedDomains[i] := False;
  FActiveDomainInfo := FSeqData.AllDomainInfo;
  FIndependentsSelected := FSeqData.DomainMarks.IsUsedIndependent;
end;

constructor TExportActiveDomainsToFastaThread.Create(aSeqData: TD_InputSeqData; targetDirectory: String);
begin
  inherited Create(True);
  FLog := TStringList.Create;
  FSeqData := aSeqData;
  FTargetDirectory := targetDirectory;
  InitOptions;
  FTempList := TStringList.Create;
  FProgress := 0;
  FCancelled := False;
end;

destructor TExportActiveDomainsToFastaThread.Destroy;
begin
  if Assigned(FLog) then
    FLog.Free;
  if Assigned(FOptions) then
    FOptions.Free;
  if Assigned(FTempList) then
    FTempList.Free;
  inherited Destroy;
end;

procedure TExportActiveDomainsToFastaThread.WriteLogToFile(filename: String);
begin
  FLog.SaveToFile(filename);
end;

{ TExportSeqDataThread }

procedure TExportSeqDataThread.SetSeqData(AValue: TD_InputSeqData);
begin
  if FSeqData=AValue then Exit;
  FSeqData:=AValue;
  FSeqData.ProgressCheckCancelFunc := @ExportProgress;
end;

function TExportSeqDataThread.ExportProgress(p: Integer; msg: String): Boolean;
begin
  FProgress := p;
  FMsg := msg;
  Synchronize(@DoExportProgress);
  Result := FIsCancelled;
end;

procedure TExportSeqDataThread.DoExportProgress;
begin
  if Assigned(CheckCancelFunc) then
    FIsCancelled := CheckCancelFunc(FProgress);
end;

function TExportSeqDataThread.ProcessData: Boolean;
begin
  try
    FOptions.IsInMainThread := False; { so ExcelWriter won't show TPleaseWait outside the context of the main thread}
    FIsSuccess := FSeqData.WriteExportDataToFile(FOptions, FSaveTo, FOutList, FExcelFile);
    FSeqData.ProgressCheckCancelFunc := nil;
    Result := True;
  except
    on E:Exception do
    begin
      FMsgLog.Add(E.Message);
      FIsSuccess := False;
    end;
  end;
end;

procedure TExportSeqDataThread.Execute;
var
  i: Integer = 0;
begin
  while i = 0 do
  begin
    ProcessData;
    inc(i);
    Terminate;
    Exit;
  end;
end;

constructor TExportSeqDataThread.Create(aSeqData: TD_InputSeqData; aOptions: TSeqExportOptions; aSaveTo: String; aResultOutput: TStringList);
begin
  inherited Create(True);
  SeqData := aSeqData;
  FOptions := aOptions;
  FSaveTo := aSaveTo;
  FOutList := aResultOutput;
  FreeOnTerminate := True;
  FMsgLog := TStringList.Create;
  FIsSuccess := False;
  FIsCancelled := False;
  case FOptions.ExportFormat of
    mefExcel, mefExcelXml, mefOds, mefCsv: FExcelFile := TExcelWrite.Create(nil, 'Sequence Data');
  else
    FExcelFile := nil;
  end;
end;

destructor TExportSeqDataThread.Destroy;
begin
  if Assigned(FMsgLog) then
    FMsgLog.Free;
  if Assigned(FOptions) then
    FOptions.Free;
  inherited Destroy;
end;

end.

