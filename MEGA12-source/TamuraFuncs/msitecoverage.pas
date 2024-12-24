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

unit msitecoverage;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MTreeData, MTreeDataAdapter, MSimpleTreeNode, MAnalysisInfo,
  MRuntimeProgressDlg, dateutils, MegaConsts, MTreeList, MegaUtils_NV;

type

  { TSiteCoverageComputer }

  TSiteCoverageComputer = class(TObject)
    private
      FLastUpdateTime: TDateTime;
      FAnalysisInfo: TAnalysisInfo; { we don't own it, we are just using it}
      FTree: TSimpleTreeNodeArray;
      FTreeData: TTreeData;
      FAdapter: TSimpleTreeDataAdapter;
      FNumTaxa: Integer;
      FNumNodes: Integer;
      FNumSites: Integer;
      FNumCoveredSites: array of Integer;
      FSiteCoverage: array of Boolean;
      FNumInternalNodes: Integer;
      FUsingMappedData: Boolean;
      FUsingParsimonyData: Boolean;
      FCoverageComputed: Boolean;
      procedure InitTree;
      procedure InitUsableDataCounts;
      procedure SetTreeData(AData: TTreeData);
      procedure SetAnalysisInfo(AnalysisInfo: TAnalysisInfo);
      procedure SetNumSites(AValue: Integer);
      procedure SetNumTaxa(AValue: Integer);
      procedure SetSeqDataFromSeqStrings(SeqData: TStringList);
      procedure SetSeqDataFromMappedData(MappedData: TList);
      procedure SetSeqDataFromInfoSites(InfoSites: TList);
      function ComputeCoverageOfNode(NodeIndex: Integer): Boolean;
      procedure ComputeCoverageOfNodeAtSite(NodeIndex: Int64; SiteIndex: Int64);
      function CheckIfSiteCoveredFullTraversal(aNodeIndex: Integer; SiteIndex: Integer): Boolean;
      function IsUsableData(AChar: AnsiChar): Boolean;
      procedure UpdateProgress(siteIndex: Integer);
      procedure ComputeSiteCoverageFullTraversal;
    public
      CheckCancelFunc: TProgressCheckCancelFunc;
      constructor Create;
      destructor Destroy; override;

      procedure ComputeSiteCoverage(AnalysisInfo: TAnalysisInfo; TreeIndex: Integer); overload;
      procedure ComputeSiteCoverage(AnalysisInfo: TAnalysisInfo; AData: TTreeData); overload;
      function ExportDataCoverage(filename: String): Boolean;
      property NumTaxa: Integer read FNumTaxa write SetNumTaxa;
      property NumNodes: Integer read FNumNodes;
      property NumSites: Integer read FNumSites write SetNumSites;
      property CoverageComputed: Boolean read FCoverageComputed;
  end;

  { TSiteCoverageThread }

  TSiteCoverageThread = class(TMegaThread)
    private
      FIsSuccess: Boolean;
      FLog: TStringList;
    protected
      FAnalysisInfo: TAnalysisInfo;
      FTreeData: TTreeData;
      FCancelled: Boolean;
      FComputer: TSiteCoverageComputer;
      FProgress: Integer;
      FRuntimeProgress: TRuntimeProgress;
      function ComputeSiteCoverage: Boolean;
      procedure Execute; override;
      procedure DoRemoveBootstrapProgressString;

      procedure DoCheckCancel;
      function CheckCancel(aProgress: Integer): Boolean;
      procedure Initialize(aInfo: TAnalysisInfo; aTreeData: TTreeData);
    public
      constructor CreateFromTreeData(aInfo: TAnalysisInfo; aTreeData: TTreeData);
      destructor Destroy; override;
      function ExportDataCoverage(filename: String): Boolean;
      property Log: TStringList read FLog;
      property IsSuccess: Boolean read FIsSuccess;
      property IsCancelled: Boolean read FCancelled;
      property AnalysisInfo: TAnalysisInfo read FAnalysisInfo;
      property RuntimeProgress: TRuntimeProgress read FRuntimeProgress;
  end;

  function ComputeSiteCoverage(var aInfo: TAnalysisInfo): Boolean; overload;
  function ComputeSiteCoverage(var aInfo: TAnalysisInfo; var aTree: TTreeData): Boolean; overload;

implementation

uses
  Forms, math;

function ComputeSiteCoverage(var aInfo: TAnalysisInfo): Boolean;
var
  aTree: TTreeData = nil;
begin
  Result := False;
  if (aInfo.MyNoOfSites > 0) and Assigned(aInfo.MyOriTreeList) and (aInfo.MyOriTreeList.Count > 0) then
  begin
    aTree := aInfo.MyOriTreeList[0];
    if aTree.IsDataCoverage then
      Result := True
    else
      Result := ComputeSiteCoverage(aInfo, aTree);
  end;
end;

function ComputeSiteCoverage(var aInfo: TAnalysisInfo; var aTree: TTreeData): Boolean;
var
  siteCoverageThread: TSiteCoverageThread = nil;
begin
  Result := False;
  try
    try
      if aInfo.MyNoOfSites > 0 then
      begin
        siteCoverageThread := TSiteCoverageThread.CreateFromTreeData(aInfo, aTree);
        siteCoverageThread.Start;
        siteCoverageThread.WaitFor;
        Result := True;
      end;
    except
      Result := False;
    end;
  finally
    if Assigned(siteCoverageThread) then
      siteCoverageThread.Free;
  end;
end;

{ TSiteCoverageThread }

function TSiteCoverageThread.ComputeSiteCoverage: Boolean;
begin
  FComputer.ComputeSiteCoverage(FAnalysisInfo, FTreeData);
  Result := FComputer.CoverageComputed;
  {$IFNDEF VISUAL_BUILD}
  if IsDeveloper then
    FComputer.ExportDataCoverage(NextAvailableFilenameNV('_data_coverage.csv'));
  {$ENDIF}
end;

procedure TSiteCoverageThread.Execute;
begin
  try
    FIsSuccess := ComputeSiteCoverage;
    if not FIsSuccess then
      FLog.Add('failed to compute site coverage');
  except
    on E:Exception do
    begin
      FIsSuccess := False;
      FLog.Add(E.Message);
    end;
  end;
  Terminate;
  Exit;
end;

procedure TSiteCoverageThread.DoRemoveBootstrapProgressString;
begin
  if Assigned(FRuntimeProgress) then
  begin
    FRuntimeProgress.RemoveRunStatusInfo(BOOT_REP_PROG_STR);
    FRuntimeProgress.UpdateRunStatusInfo('Status', 'Calculating Site Coverages');
  end;
end;

procedure TSiteCoverageThread.DoCheckCancel;
begin
  if Assigned(FRuntimeProgress) then
    FCancelled := FRuntimeProgress.ProgressCheckCancel(FProgress);
end;

function TSiteCoverageThread.CheckCancel(aProgress: Integer): Boolean;
begin
  if Assigned(FRuntimeProgress) then
  begin
    FProgress := AProgress;
    {$IFDEF VISUAL_BUILD}
    Synchronize(DoCheckCancel);
    {$ELSE}
    FRuntimeProgress.Progress := FProgress;
    {$ENDIF}
    Result := FCancelled;
  end
  else
    Result := False;
end;

procedure TSiteCoverageThread.Initialize(aInfo: TAnalysisInfo; aTreeData: TTreeData);
begin
  FreeOnTerminate := False;
  FCancelled := False;
  FProgress := 0;
  Synchronize(DoCheckCancel);
  FRuntimeProgress := aInfo.ARP;
  FAnalysisInfo := aInfo;
  FTreeData := aTreeData;
  FComputer := TSiteCoverageComputer.Create;
  FComputer.CheckCancelFunc := CheckCancel;
  FLog := TStringList.Create;
  Synchronize(DoRemoveBootstrapProgressString);
end;

constructor TSiteCoverageThread.CreateFromTreeData(aInfo: TAnalysisInfo; aTreeData: TTreeData);
begin
  inherited Create(True);
  Initialize(aInfo, aTreeData);
end;

destructor TSiteCoverageThread.Destroy;
begin
  if Assigned(FComputer) then
    FComputer.Free;
  if Assigned(FLog) then
    FLog.Free;
  FAnalysisInfo := nil;
  FTreeData := nil;
  inherited Destroy;
end;

function TSiteCoverageThread.ExportDataCoverage(filename: String): Boolean;
begin
  if Assigned(FComputer) then
    Result := FComputer.ExportDataCoverage(filename)
  else
    Result := False;
end;

{ TSiteCoverageComputer }

procedure TSiteCoverageComputer.InitTree;
var
  i: Integer = -1;
begin
  Assert((Assigned(FAnalysisInfo.MySeqStrings) or (Assigned(FAnalysisInfo.MyMappedData))) or Assigned(FAnalysisInfo.MyInfoSites));

  If Assigned(FAnalysisInfo.MySeqStrings) then
  begin
    if FAnalysisInfo.MySeqStrings.Count > 0 then
      SetSeqDataFromSeqStrings(FAnalysisInfo.MySeqStrings)
    else if Assigned(FAnalysisInfo.MyOrigSeqStrings) and (FAnalysisInfo.MyOrigSeqStrings.Count > 0) then
      SetSeqDataFromSeqStrings(FAnalysisInfo.MyOrigSeqStrings)
    else
      raise Exception.Create('ML seq data is missing when calculating site coverages');
  end
  else if Assigned(FAnalysisInfo.MyMappedData) then
    SetSeqDataFromMappedData(FAnalysisInfo.MyMappedData)
  else if Assigned(FAnalysisInfo.MyInfoSites) then
    SetSeqDataFromInfoSites(FAnalysisInfo.MyInfoSites)
  else if Assigned(FAnalysisInfo.MySeqPartitions) then
  begin
    Assert(False, 'ComputeSiteCoverage not yet implemented for partitioned ML analysis');
  end;

  SetLength(FSiteCoverage, NumSites);
  SetLength(FNumCoveredSites, FNumNodes);
  for i := Low(FNumCoveredSites) to High(FNumCoveredSites) do
    FNumCoveredSites[i] := 0;
end;

procedure TSiteCoverageComputer.InitUsableDataCounts;
begin
  FAdapter.UpdateUsableDataArrays;
end;

procedure TSiteCoverageComputer.SetNumSites(AValue: Integer);
begin
  if FNumSites=AValue then
    Exit;
  FNumSites:=AValue;
end;

procedure TSiteCoverageComputer.SetNumTaxa(AValue: Integer);
begin
  if FNumTaxa=AValue then Exit;
  FNumTaxa := AValue;
  FNumNodes := 2 * NumTaxa - 1;
  FNumInternalNodes := (FNumNodes - FNumTaxa);
end;

procedure TSiteCoverageComputer.SetSeqDataFromSeqStrings(SeqData: TStringList);
var
  aNode: Integer = -1;
  aSite: Integer = -1;
begin
  FUsingMappedData := False;
  FUsingParsimonyData := False;
  Assert(SeqData.Count = NumTaxa);

  for aNode := 0 to NumTaxa - 1 do
  begin
    FTree[aNode].SequenceData := SeqData[aNode];
    SetLength(FTree[aNode].HasUsableDataAtSite, NumSites);
    for aSite := 0 to NumSites - 1 do
    begin
      if IsUsableData(FTree[aNode].SequenceData[aSite + 1]) then
      begin
        FTree[aNode].HasUsableDataAtSite[aSite].X := 1;
        FTree[aNode].HasUsableDataAtSite[aSite].Y := 1;
      end
      else
      begin
        FTree[aNode].HasUsableDataAtSite[aSite].X := 0;
        FTree[aNode].HasUsableDataAtSite[aSite].Y := 0;
      end;
    end;
  end;
end;

procedure TSiteCoverageComputer.SetTreeData(AData: TTreeData);
begin
  FTreeData := AData;
  FAdapter.SetTreeData(FTreeData, False, True);
  FTree := FAdapter.GetSimpleTreeReference;
end;

procedure TSiteCoverageComputer.UpdateProgress(siteIndex: Integer);
begin
  if MillisecondsBetween(Time, FLastUpdateTime) < 500 then
    Exit;
  FLastUpdateTime := Time;
  if Assigned(CheckCancelFunc) then
    if CheckCancelFunc(Trunc(siteIndex / Max(1, FNumSites) * 100)) then
      raise EAbort.Create('site coverage calculation cancelled by user');
end;

procedure TSiteCoverageComputer.SetSeqDataFromInfoSites(InfoSites: TList);
var
  aNode: Integer = -1;
  aSite: Integer = -1;
begin
  FUsingParsimonyData := True;
  FUsingMappedData := False;
  Assert(InfoSites.Count = NumTaxa);
  for aNode := 0 to NumTaxa - 1 do
  begin
    SetString(FTree[aNode].SequenceData, PAnsiChar(InfoSites[aNode]), NumSites);
    SetLength(FTree[aNode].HasUsableDataAtSite, NumSites);
    for aSite := 0 to NumSites - 1 do
    begin
      if IsUsableData(FTree[aNode].SequenceData[aSite + 1]) then
      begin
        FTree[aNode].HasUsableDataAtSite[aSite].X := 1;
        FTree[aNode].HasUsableDataAtSite[aSite].Y := 1;
      end
      else
      begin
        FTree[aNode].HasUsableDataAtSite[aSite].X := 0;
        FTree[aNode].HasUsableDataAtSite[aSite].Y := 0;
      end;
    end;
  end;
end;

procedure TSiteCoverageComputer.SetSeqDataFromMappedData(MappedData: TList);
var
  aNode: Integer = -1;
  aSite: Integer = -1;
begin
  FUsingMappedData := True;
  FUsingParsimonyData := False;
  Assert(MappedData.Count = NumTaxa);
  for aNode := 0 to NumTaxa - 1 do
  begin
    Assert(MappedData[aNode] <> nil);
    SetString(FTree[aNode].SequenceData, PAnsiChar(MappedData[aNode]), NumSites);
    SetLength(FTree[aNode].HasUsableDataAtSite, NumSites);
    for aSite := 0 to NumSites - 1 do
    begin
      if IsUsableData(FTree[aNode].SequenceData[aSite + 1]) then
      begin
        FTree[aNode].HasUsableDataAtSite[aSite].X := 1;
        FTree[aNode].HasUsableDataAtSite[aSite].Y := 1;
      end
      else
      begin
        FTree[aNode].HasUsableDataAtSite[aSite].X := 0;
        FTree[aNode].HasUsableDataAtSite[aSite].Y := 0;
      end;
    end;
  end;
end;

function TSiteCoverageComputer.ComputeCoverageOfNode(NodeIndex: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  Assert(not FTree[NodeIndex].IsOtu);
  FNumCoveredSites[NodeIndex] := 0;
  for i := 1 to NumSites do
    if CheckIfSiteCoveredFullTraversal(NodeIndex, i) then
      inc(FNumCoveredSites[NodeIndex]);
  if FNumSites > 0 then
    FTree[NodeIndex].DataCoverage := (FNumCoveredSites[NodeIndex] / FNumSites)
  else
    FTree[NodeIndex].DataCoverage := 0.0;
  UpdateProgress(NodeIndex);
end;

procedure TSiteCoverageComputer.ComputeCoverageOfNodeAtSite(NodeIndex: Int64; SiteIndex: Int64);
begin
  Assert(not FTree[NodeIndex].IsOtu);
  if CheckIfSiteCoveredFullTraversal(NodeIndex, SiteIndex) then
    inc(FNumCoveredSites[NodeIndex]);
end;

procedure TSiteCoverageComputer.ComputeSiteCoverage(AnalysisInfo: TAnalysisInfo; AData: TTreeData);
begin
  FLastUpdateTime := Time;
  SetAnalysisInfo(AnalysisInfo);
  SetTreeData(AData);
  ComputeSiteCoverageFullTraversal;
  FCoverageComputed := True;
end;

function TSiteCoverageComputer.ExportDataCoverage(filename: String): Boolean;
var
  aFile: TextFile;
  i: Integer;
  aNode: TSimpleTreeNode = nil;
begin
  Result := True;
  try
    try
      AssignFile(aFile, filename);
      Rewrite(aFile);
      WriteLn(aFile, 'node_id,data_coverage');
      if Length(FTree) > 0 then
        for i := FNumTaxa to FNumNodes - 1 do
        begin
          aNode := FTree[i];
          WriteLn(aFile, Format('%d,%.2f%%', [aNode.NodeIndex + 1, aNode.DataCoverage*100]));
        end;
    except
      on E:Exception do
        Result := False;
    end;
  finally
    CloseFile(aFile);
  end;
  Result := Result and FileExists(filename);
end;

function TSiteCoverageComputer.CheckIfSiteCoveredFullTraversal(aNodeIndex: Integer; SiteIndex: Integer): Boolean;
begin
  Result := (FTree[aNodeIndex].HasUsableDataAtSite[SiteIndex - 1].X = 1) and (FTree[aNodeIndex].HasUsableDataAtSite[SiteIndex - 1].Y = 1);
end;

function TSiteCoverageComputer.IsUsableData(AChar: AnsiChar): Boolean;
begin
  if FUsingMappedData then
  begin
    if FAnalysisInfo.isAminoAcid then
      Result := (AChar <> gdResiX)
    else
      Result := (AChar <> gdBaseN);
  end
  else if FUsingParsimonyData then
  begin
    if FAnalysisInfo.IsAminoAcid then
      Result := (LongInt(AChar) <> pmResiX)
    else
      Result := (AChar <> pmBaseN);
  end
  else
    Result := ((AChar <> '-') and (AChar <> '?'))
end;

procedure TSiteCoverageComputer.SetAnalysisInfo(AnalysisInfo: TAnalysisInfo);
begin
  FAnalysisInfo := AnalysisInfo;
  NumTaxa := AnalysisInfo.NoOfSeqs; { use NumTaxa and not FNumTaxa so other values get updated}
  NumSites := AnalysisInfo.NoOfSites;
  if Assigned(CheckCancelFunc) then
    CheckCancelFunc(1);
end;

constructor TSiteCoverageComputer.Create;
begin
  FCoverageComputed := False;
  FUsingMappedData := False;
  FUsingParsimonyData := False;
  FADapter := TSimpleTreeDataAdapter.Create;
end;

destructor TSiteCoverageComputer.Destroy;
begin
  if Assigned(FAdapter) then
    FAdapter.Free;
  FAnalysisInfo := nil; { we don't own it so don't free it}
  inherited Destroy;
end;

procedure TSiteCoverageComputer.ComputeSiteCoverageFullTraversal;
var
  site, node: Integer;
begin
  InitTree;
  InitUsableDataCounts;

  if FNumSites > 0 then
    for site := 1 to FNumSites do
    begin
      for node := NumTaxa to NumNodes - 1 do
        ComputeCoverageOfNodeAtSite(node, site);
      if (site mod 1000) = 0 then
        UpdateProgress(site);
    end;
  for node := NumTaxa to NumNodes - 1 do
    if FNumSites > 0 then
      FTree[node].DataCoverage := FNumCoveredSites[node] / FNumSites
    else
      FTree[node].DataCoverage := 0;
  FAdapter.GetTreeData(FTreeData);
  FTreeData.IsDataCoverage := True;
  if Assigned(CheckCancelFunc) then
    CheckCancelFunc(100);
end;

procedure TSiteCoverageComputer.ComputeSiteCoverage(AnalysisInfo: TAnalysisInfo; TreeIndex: Integer);
begin
  FLastUpdateTime := Time;
  SetAnalysisInfo(AnalysisInfo);
  SetTreeData(AnalysisInfo.MyOriTreeList[TreeIndex]);
  ComputeSiteCoverageFullTraversal;
  FCoverageComputed := True;
end;

end.

