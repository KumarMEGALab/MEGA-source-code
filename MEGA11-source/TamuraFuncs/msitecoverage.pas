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

unit msitecoverage;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MTreeData, MTreeDataAdapter, MSimpleTreeNode, MAnalysisInfo,
  MRuntimeProgressDlg, dateutils, MegaConsts, MTreeList, fgl;

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
      Des1Taxa: TSimpleTreeNodeList;
      Des2Taxa: TSimpleTreeNodeList;
      Des1TaxaList: TSimpleTreeNodeListArray;
      Des2TaxaList: TSimpleTreeNodeListArray;
      FSiteCoverage: array of Boolean;
      FNumInternalNodes: Integer;
      FUsingMappedData: Boolean;
      FUsingParsimonyData: Boolean;
      FCoverageComputed: Boolean;
      procedure InitTree;
      procedure InitNodeLists;
      procedure SetTreeData(AData: TTreeData);
      procedure SetAnalysisInfo(AnalysisInfo: TAnalysisInfo);
      procedure SetNumSites(AValue: Integer);
      procedure SetNumTaxa(AValue: Integer);
      procedure SetSeqDataFromSeqStrings(SeqData: TStringList);
      procedure SetSeqDataFromMappedData(MappedData: TList);
      procedure SetSeqDataFromInfoSites(InfoSites: TList);
      function ComputeCoverageOfNode(NodeIndex: Integer): Boolean;
      procedure ComputeCoverageOfNodeAtSite(NodeIndex: Int64; SiteIndex: Int64);
      function CheckIfSiteCovered(aNodeIndex: Integer; SiteIndex: Integer): Boolean;
      function IsUsableData(AChar: AnsiChar): Boolean;
      procedure SetDes1Taxa(aIndex: Int64);
      procedure SetDes2Taxa(aIndex: Int64);
      procedure UpdateProgress(siteIndex: Integer);
      procedure ComputeSiteCoverageSimultaneous;
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

  TSiteCoverageThread = class(TThread)
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
      constructor CreateFromTreeList(aInfo: TAnalysisInfo; aTreeList: TTreeList; aIndex: Integer);
      constructor CreateFromTreeData(aInfo: TAnalysisInfo; aTreeData: TTreeData);
      destructor Destroy; override;
      property Log: TStringList read FLog;
      property IsSuccess: Boolean read FIsSuccess;
      property IsCancelled: Boolean read FCancelled;
      property AnalysisInfo: TAnalysisInfo read FAnalysisInfo;
      property RuntimeProgress: TRuntimeProgress read FRuntimeProgress;
  end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ENDIF}
  Forms, math;

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
    FRuntimeProgress.UpdateRunStatusInfo('Status', 'Computing Data Coverage');
  end;
end;

procedure TSiteCoverageThread.DoCheckCancel;
begin
  if Assigned(FRuntimeProgress) then
    FCancelled := FRuntimeProgress.ProgressCheckCancel(FProgress);
end;

function TSiteCoverageThread.CheckCancel(aProgress: Integer): Boolean;
begin
  FProgress := AProgress;
  Synchronize(DoCheckCancel);
  Result := FCancelled;
end;

procedure TSiteCoverageThread.Initialize(aInfo: TAnalysisInfo; aTreeData: TTreeData);
begin
  FreeOnTerminate := False;
  FCancelled := False;
  FRuntimeProgress := aInfo.ARP;
  FAnalysisInfo := aInfo;
  FTreeData := aTreeData;
  FComputer := TSiteCoverageComputer.Create;
  FComputer.CheckCancelFunc := CheckCancel;
  FLog := TStringList.Create;
  Synchronize(DoRemoveBootstrapProgressString);
end;

constructor TSiteCoverageThread.CreateFromTreeList(aInfo: TAnalysisInfo; aTreeList: TTreeList; aIndex: Integer);
begin
  inherited Create(True);
  Initialize(aInfo, aTreeList[aIndex]);
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

{ TSiteCoverageComputer }

procedure TSiteCoverageComputer.InitTree;
var
  i: Integer;
begin
  Assert((Assigned(FAnalysisInfo.MySeqStrings) or (Assigned(FAnalysisInfo.MyMappedData))) or Assigned(FAnalysisInfo.MyInfoSites));
  If Assigned(FAnalysisInfo.MySeqStrings) then
  begin
    SetSeqDataFromSeqStrings(FAnalysisInfo.MySeqStrings);
  end
  else if Assigned(FAnalysisInfo.MySeqPartitions) then
  begin
    Assert(False, 'ComputeSiteCoverage not yet implemented for partitioned ML analysis');
  end
  else if Assigned(FAnalysisInfo.MyMappedData) then
  begin
    SetSeqDataFromMappedData(FAnalysisInfo.MyMappedData);
  end
  else if Assigned(FAnalysisInfo.MyInfoSites) then
    SetSeqDataFromInfoSites(FAnalysisInfo.MyInfoSites);
  SetLength(FSiteCoverage, NumSites);
  SetLength(FNumCoveredSites, FNumNodes);
  for i := Low(FNumCoveredSites) to High(FNumCoveredSites) do
    FNumCoveredSites[i] := 0;
end;

procedure TSiteCoverageComputer.InitNodeLists;
var
  i: Integer;
begin
  SetLength(Des1TaxaList, FNumNodes - FNumTaxa + 1);
  SetLength(Des2TaxaList, FNumNodes - FNumTaxa + 1);
  for i := 0 to Length(Des1TaxaList) - 1 do
  begin
    Des1TaxaList[i] := TSimpleTreeNodeList.Create;
    Des2TaxaList[i] := TSimpleTreeNodeList.Create;
  end;
  for i := FNumTaxa to FNumNodes - 1 do
  begin
    SetDes1Taxa(i);
    SetDes2Taxa(i);
  end;
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
  i: Integer;
begin
  FUsingMappedData := False;
  FUsingParsimonyData := False;
  Assert(SeqData.Count = NumTaxa);
  for i := 0 to NumTaxa - 1 do
    FTree[i].SequenceData := SeqData[i];
end;

procedure TSiteCoverageComputer.SetTreeData(AData: TTreeData);
begin
  FTreeData := AData;
  FAdapter.SetTreeData(FTreeData, False);
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
  i: Integer;
begin
  FUsingParsimonyData := True;
  FUsingMappedData := False;
  Assert(InfoSites.Count = NumTaxa);
  for i := 0 to NumTaxa - 1 do
    SetString(FTree[i].SequenceData, PAnsiChar(InfoSites[i]), NumSites)
end;

procedure TSiteCoverageComputer.SetSeqDataFromMappedData(MappedData: TList);
var
  i: Integer;
begin
  FUsingMappedData := True;
  FUsingParsimonyData := False;
  Assert(MappedData.Count = NumTaxa);
  for i := 0 to NumTaxa - 1 do
  begin
    Assert(MappedData[i] <> nil);
    SetString(FTree[i].SequenceData, PAnsiChar(MappedData[i]), NumSites);
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
    if CheckIfSiteCovered(NodeIndex, i) then
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
  if CheckIfSiteCovered(NodeIndex, SiteIndex) then
    inc(FNumCoveredSites[NodeIndex]);
end;

procedure TSiteCoverageComputer.ComputeSiteCoverage(AnalysisInfo: TAnalysisInfo; AData: TTreeData);
begin
  FLastUpdateTime := Time;
  SetAnalysisInfo(AnalysisInfo);
  SetTreeData(AData);
  ComputeSiteCoverageSimultaneous;
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

function TSiteCoverageComputer.CheckIfSiteCovered(aNodeIndex: Integer; SiteIndex: Integer): Boolean;
var
  i, j: Integer;
  Des1, Des2: TSimpleTreeNode;
begin
  Result := False;
  Des1Taxa := Des1TaxaList[aNodeIndex - FNumTaxa];
  Des2Taxa := Des2TaxaList[aNodeIndex - FNumTaxa];
  Assert((Des1Taxa.Count > 0) and (Des2Taxa.Count > 0));

  for i := 0 to Des1Taxa.Count - 1 do
  begin
    Des1 := Des1Taxa[i];
    Assert(Length(Des1.SequenceData) = NumSites);
    for j := 0 to Des2Taxa.Count - 1 do
    begin
      Des2 := Des2Taxa[j];
      Assert(Length(Des2.SequenceData) = NumSites);
      if IsUsableData(Des1.SequenceData[SiteIndex]) and IsUsableData(Des2.SequenceData[SiteIndex]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
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

procedure TSiteCoverageComputer.SetDes1Taxa(aIndex: Int64);
var
  Des1: TSimpleTreeNode;
  aNode: TSimpleTreeNode;

  procedure ProcessNode(Node: TSimpleTreeNode);
  begin
    if Node.IsOtu then
    begin
      Des1Taxa.Add(Node);
      Exit;
    end;
    ProcessNode(Node.Des1);
    ProcessNode(Node.Des2);
  end;

begin
  aNode := FTree[aIndex];
  Assert(not ANode.IsOtu);
  Des1Taxa := Des1TaxaList[aIndex - FNumTaxa];
  if Des1Taxa.Count > 0 then
    Des1Taxa.Clear;
  Des1 := ANode.Des1;
  if Des1.IsOtu then
  begin
    Des1Taxa.Add(Des1);
    Exit;
  end;
  ProcessNode(Des1);
end;

procedure TSiteCoverageComputer.SetDes2Taxa(aIndex: Int64);
var
  Des2: TSimpleTreeNode;
  aNode: TSimpleTreeNode;

  procedure ProcessNode(Node: TSimpleTreeNode);
  begin
    if Node.IsOtu then
    begin
      Des2Taxa.Add(Node);
      Exit;
    end;
    ProcessNode(Node.Des1);
    ProcessNode(Node.Des2);
  end;

begin
  aNode := FTree[aIndex];
  Assert(not ANode.IsOtu);
  Des2Taxa := Des2TaxaList[aIndex - FNumTaxa];
  if Des2Taxa.Count > 0 then
    Des2Taxa.Clear;
  Des2 := ANode.Des2;
  if Des2.IsOtu then
  begin
    Des2Taxa.Add(Des2);
    Exit;
  end;
  ProcessNode(Des2);
end;

constructor TSiteCoverageComputer.Create;
begin
  FCoverageComputed := False;
  FUsingMappedData := False;
  FUsingParsimonyData := False;
  FADapter := TSimpleTreeDataAdapter.Create;
  Des1Taxa := TSimpleTreeNodeList.Create;
  Des2Taxa := TSimpleTreeNodeList.Create;
end;

destructor TSiteCoverageComputer.Destroy;
var
  i: Integer;
begin
  if Assigned(FAdapter) then
    FAdapter.Free;
  if Length(Des1TaxaList) > 0 then
  begin
    for i := 0 to Length(Des1TaxaList) - 1 do
      if Assigned(Des1TaxaList[i]) then
        Des1TaxaList[i].Free;
    SetLength(Des1TaxaList, 0);
  end;
  if Length(Des2TaxaList) > 0 then
  begin
    for i := 0 to Length(Des2TaxaList) - 1 do
      if Assigned(Des2TaxaList[i]) then
        Des2TaxaList[i].Free;
    SetLength(Des2TaxaList, 0);
  end;
  FAnalysisInfo := nil; { we don't own it so don't free it}
  inherited Destroy;
end;

procedure TSiteCoverageComputer.ComputeSiteCoverageSimultaneous;
var
  site, node: Integer;
begin
  InitTree;
  InitNodeLists;
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
  if Assigned(CheckCancelFunc) then
    CheckCancelFunc(100);
end;

procedure TSiteCoverageComputer.ComputeSiteCoverage(AnalysisInfo: TAnalysisInfo; TreeIndex: Integer);
begin
  FLastUpdateTime := Time;
  SetAnalysisInfo(AnalysisInfo);
  SetTreeData(AnalysisInfo.MyOriTreeList[TreeIndex]);
  ComputeSiteCoverageSimultaneous;
  FCoverageComputed := True;
end;

end.

