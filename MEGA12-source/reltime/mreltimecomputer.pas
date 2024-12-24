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

unit mreltimecomputer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MTreeData, MCalibrationData, ExcelWrite,
  MegaConsts, dateutils, mcorrelationtest, mreltimeexport, mreltimetreenode;

type
  TProgressProc = procedure(Progress: Integer) of object;


  { TReltimeComputer }

  TReltimeComputer = class(TObject)
    private
      FNeedsRootByOutgroup: Boolean;
      FNodeLabels: TStringList;
      FIsBlensOnly: Boolean;
      FIsLittleBootstrapsTiming: Boolean;
      FLatestSampleDate: Extended;
      FPropagateConstraints: Boolean;
      FSourceData: TTreeData;
      FNoOfOtus: Integer;
      FNodes: TReltimeTreeNodeArray;
      FRoot: TReltimeTreeNode;
      FNumNodesProcessed: Integer;
      FCaptionString: AnsiString;
      FStrictClockRate: Double;
      FTimeFactor: extended;
      FMinTimeFactor: Extended;
      FMaxTimeFactor: Extended;
      FLatestTime: extended;
      FMinTimes, FMaxTimes: TDivTimesArray;
      FOrigMinTimes, FOrigMaxTimes, FOrigSamplingTimes: array of Extended;
      FVarianceEstimationMethod: TReltimeVarianceMethod;
      FIsSamplingTime: boolean;
      FMaxRateRatio: extended;
      FOriginalReltimes: ArrayOfExtended;
      FLog: TStringList;
      FExportBuilder: TReltimeExportBuilder;
      FCorrTestTree: TTreeData;
      FNamesList: TStringList;
      function GetIsStrictClockTree: Boolean;
      procedure InitCorrTestTree;
      function ComputeHeights: TTreeData;
      function GetSpreadsheetExport: TExcelWrite;
      procedure ResetTreeData(Tree: TTreeData);
      procedure InitNodeData(Tree: TTreeData);
      procedure InitNodeRelationships(Tree: TTreeData);
      procedure SetPropagateConstraints(AValue: Boolean);
      procedure SetSizeDepth;
      function GetSBL(ANode: TRelTimeTreeNode): Double;
      function GetCaptionString: AnsiString;
      function GetNode(Index: Integer): TReltimeTreeNode;
      function GetNodeLabel(Calibrations: TCalibrations; NodeIndex: Integer): String;
      function GetReltimes: ArrayOfExtended;
      procedure ResetNodes;
      procedure SetStrictClockRate(AValue: Double);
      procedure ChangeRootByOutgroup;
      procedure DebugDumpOutgroupIngroupInfo;
      procedure SetVarianceEstimationMethod(const Value: TReltimeVarianceMethod);
      procedure ComputeHeightVar;
      procedure CorrectShallowHeightVar;
      procedure GenerateSpreadsheetExport(const names, labels: TStringList; const minTimes, maxTimes: array of Extended); overload;
      procedure GenerateSpreadsheetExport(const names, labels: TStringList); overload;
      procedure GenerateSpreadsheetExport(const names, labels: TStringList; const sampleTimes: array of Extended); overload;
      function HasOutgroup: Boolean;
      procedure SetMinTimes(aMinTimes: array of Extended);
      procedure SetMaxTimes(aMaxTimes: array of Extended);
      procedure ClearNodes;
      {$IFDEF DEBUG}
      procedure ExportRates(Filename: String);
      {$ENDIF}
      function GetSibling(aNode: TRelTimeTreeNode): TRelTimeTreeNode;
      function GetSecondDescendents(aNode: TReltimeTreeNode): TRelTimeTreeNodeArray;
      function GetThirdDescendents(aNode: TReltimeTreeNode): TRelTimeTreeNodeArray;
      function IsSecondDescendent(Ancestor, Descendent: TReltimeTreeNode): Boolean;
      function IsThirdDescendent(Ancestor, Descendent: TReltimeTreeNode): Boolean;
      function GetSiblingRates: TCorrTestDataList;
      function GetDirectDescendentRates: TCorrTestDataList;
      function GetSecondDescendentRates: TCorrTestDataList;
      function GetThirdDescendentRates: TCorrTestDataList;

      procedure SetLatestTime(const Value: Extended);
      function GetInternalNodeLabel(TreeDataIndex: Integer; n: TRelTimeTreeNode): String;
      function ResultsFootnote: String;
      function DebugNodeInfoToStringList(n: TRelTimeTreeNode): TStringList;

      function DebugInfo(index: Integer): String;
      function DebugPropogatedConstraints(minTimes, maxTimes: array of extended): TStringList;

      {$IFDEF DEBUG}
      function DebugNodeInfoToFile(n: TRelTimeTreeNode): Boolean;
      function DebugAllNodeInfo: TStringList;
      function DebugAllNodeInfoToFile(filename: String): Boolean;
      {$ENDIF}

      function DumpMinMaxTimes(minTimes, maxTimes: array of Extended; filename: String): Boolean;
      function FindMrca(aNode: TRelTimeTreeNode; taxon1: Integer; taxon2: Integer): Integer;
      function Mrca(leafNodes: TList): TRelTimeTreeNode;
      function CheckCalibrationIds(var aCalibs: TCalibrations): Boolean;
      procedure PrepareIngroupCalibration(const aCalib: TCalibrationTime);
    public
      IsVarBlen: boolean;
      Zvalue: extended;
      ProgressProc: TProgressProc;
      constructor Create;
      destructor Destroy; override;
      {$IFDEF DEBUG}
      function BlensFunc(tree: TTreeData): Double;
      {$ENDIF}

      function RtdtFloatTimeToDateString(aFloatTime: Extended): String;
      function ElapsedDaysForRtdtTime(index: Integer): Int64;
      procedure WriteToFile(var aFile: File);
      procedure ReadFromFile(var aFile: File; const sessionVersion: Integer);
      procedure Assign(Source: TReltimeComputer);
      procedure SetNodeLabels(aNodeLabels: TStringList);
      procedure FlagOutgroupNodes;
      procedure FlagOutgroupCluster;
      procedure InitFromSessionFile(TreeData: TTreeData);
      procedure InitParentChildRelationships(TreeData: TTreeData);
      procedure InitSpreadsheetExportFromSessionFile(aNames: TStringList; aLabels: TStringList);
      function CorrelationTest(TreeData: TTreeData; MaxRateRatio: Extended; ComputeReltimes: Boolean): TCorrelationTest;
      function AutomaticCorrelationTest(MaxRateRatio: Extended): TCorrelationTest;
      function ComputeDensityDistributionTree(TreeData: TTreeData; MaxRateRatio: Extended; aMinTimes, aMaxTimes: TDivTimesArray): TTreeData;
      function ComputeRelTimeBLens(TreeData: TTreeData; MaxRateRatio: Extended): TTreeData; overload; // so we can do reltime blen analysis inside the TreeExplorer thread
      function ComputeReltimeBLens(TreeData: TTreeData; Calibrations: TCalibrations; MaxRateRatio: Extended): TTreeData; overload;
      function ComputeRelTimeBLens(TreeData: TTreeData;
                                   var SamplingTime: array of extended;
                                   MaxRateRatio: Extended;
                                   namesList: TStringList): TTreeData; overload;
      function ComputeRelTimeBLens(TreeData: TTreeData;
                                   var ExportList: TStringList;
                                   const NamesList: TStringList;
                                   const NodeLabels: TStringList;
                                   MaxRateRatio: Extended): TTreeData; overload;
      function ComputeRelTimeBLens(TreeData: TTreeData;
                                   var ExportList: TStringList;
                                   const NamesList: TStringList;
                                   var NodeLabels: TStringList;
                                   var CalibrationTimes: TCalibrations;
                                   MaxRateRatio: Extended): TTreeData; overload;
      function ComputeRelTimeBLens(TreeData: TTreeData;
                                   var ExportList: TStringList;
                                   const NamesList: TStringList;
                                   var CalibrationTimes: TCalibrations;
                                   MaxRateRatio: Extended): TTreeData; overload;
      function ComputeLbsDivTimes(treeData: TTreeData; var calibs: TCalibrations): TTreeData;
      procedure GenerateCalibratedClockTreeExport(var ExportList: TStringList; const NamesList, NodeLabels: TStringList; MinTimes: array of Extended; MaxTimes: array of Extended);
      procedure GenerateClockTreeExport(var ExportList: TStringList; const NamesList, NodeLabels: TStringList);
      procedure GenerateSampledTimesClockTreeExport(var ExportList: TStringList;
                                                  const NamesList: TStringList;
                                                  const NodeLabels: TStringList;
                                                  const SampleTimes: array of Extended);
      function GenerateNexusReltimeExport(NamesList: TStringList; HasCalibrations: Boolean; HasRates: Boolean; HasDataCoverage: Boolean=True): TStringList;
      function GenerateStrictClockSpreadsheetExport(const aNames: TStringList; aLabels: TStringList; minTimes: array of Extended; maxTimes: array of Extended; aCaption: TStringList = nil): TExcelWrite; overload;
      function GenerateStrictClockSpreadsheetExport(const aNames: TStringList; aLabels: TStringList; aCaption: TStringList = nil): TExcelWrite; overload;
      function GenerateElapsedTimesSpreadsheetExport(const aNames: TStringList; aLabels: TStringList; aCaption: TStringList = nil): TExcelWrite;
      { Given an id for an internal node, finds two extant taxa for whom the internal
        node is their most recent common ancestor }
      function FindTwoExtantTaxa(InternalNodeId: Integer;
                                 TaxaNamesList: TStringList;
                                 var TaxaAId: Integer;
                                 var TaxaAName: String;
                                 var TaxaBId: Integer;
                                 var TaxaBName: String): Boolean;

      function AnchorClockTree(var minTime, maxTime: array of extended): TTreeData;
      { Return nil if the minTime and maxTime arrays contain inconsistency, e.g.,
        the minTime of a node is older than the maxTime of its ancestor. }
      function TimeTreeToNewickString(OtuNames: TStringList; UseTimeFactor: Boolean): String;
      function Empty: boolean;
      function MapTreeDataIndexToNodeIndex(TreeDataIndex: Integer): Integer;
      function MapNodeIndexToTreeDataIndex(NodeIndex: Integer): Integer;
      function GetTimeFactorFunc: Extended;
      property Root: TReltimeTreeNode read FRoot write FRoot;
      property NoOfOtus: Integer read FNoOfOtus write FNoOfOtus;
      property CaptionString: AnsiString read GetCaptionString;
      property Node[Index: Integer] : TReltimeTreeNode read GetNode;
      property NodeArray: TRelTimeTreeNodeArray read FNodes;
      property TimeFactor: extended read FTimeFactor write FTimeFactor;
      property MinTimeFactor: Extended read FMinTimeFactor write FMinTimeFactor;
      property MaxTimeFactor: Extended read FMaxTimeFactor write FMaxTimeFactor;
      property LatestTime: Extended read FLatestTime write SetLatestTime;
      property LatestSampleDate: Extended read FLatestSampleDate;
      property IsBlensOnly: Boolean read FIsBlensOnly write FIsBLensOnly;
      property IsSamplingTime: Boolean read FIsSamplingTime;
      property VarianceEstimationMethod: TReltimeVarianceMethod read FVarianceEstimationMethod write SetVarianceEstimationMethod;
      property MaxRateRatio: extended read FMaxRateRatio write FMaxRateRatio;
      property Log: TStringList read FLog;
      function ValidateConstraints(tree: TTreeData; minTime, maxTime: array of extended): Boolean;
      function GetReltimesArray: ArrayOfExtended;
      function DivTime(index: integer): extended;
      function MaxDivTime(index: integer): extended;
      function MinDivTime(index: integer): extended;
      function MaxDivTimeRaw(index: Integer): extended;
      function MinDivTimeRaw(index: Integer): extended;
      function NodeHeight(index: integer): extended;
      function MaxNodeHeight(index: integer): extended;
      function MinNodeHeight(index: integer): extended;
      function DataCoverage(index: Integer): Double;
      function NumNodes: Integer;
      function GetNodeLabels: TStringList;
      function ReleaseControlOfSpreadsheetExport: TReltimeExportBuilder;
      procedure AcceptControlOfSpreadsheetExport(aExport: TReltimeExportBuilder);
      procedure SetNodeData(TreeData: TTreeData);
      property PropagateConstraints: Boolean read FPropagateConstraints write SetPropagateConstraints; { developer feature requested by Sudhir}
      property OriginalReltimes: ArrayOfExtended read FOriginalReltimes;
      property SpreadsheetExport: TExcelWrite read GetSpreadsheetExport;
      property ExportBuilder: TReltimeExportBuilder read FExportBuilder;
      property StrictClockRate: Double read FStrictClockRate write SetStrictClockRate;
      property IsLittleBootstrapsTiming: Boolean read FIsLittleBootstrapsTiming write FIsLittleBootstrapsTiming;
      property IsStrictClockTree: Boolean read GetIsStrictClockTree;
      property NeedsRootByOutgroup: Boolean read FNeedsRootByOutgroup write FNeedsRootByOutgroup;
  end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}MegaUtils_NV,{$ENDIF}
  MegaUtils, Math, StringUtils, mnexuswriter, MTreeList, MTreeDataAdapter;


{ TReltimeComputer }

function TReltimeComputer.MapNodeIndexToTreeDataIndex(NodeIndex: Integer): Integer;
begin
  if NodeIndex >= FNoOfOtus then
    Result := NodeIndex - FNoOfOtus
  else
    Result := NodeIndex + FNoOfOtus - 1;
end;

function TReltimeComputer.GetTimeFactorFunc: Extended;
begin
  Result := FTimeFactor;
end;

function TReltimeComputer.MapTreeDataIndexToNodeIndex(TreeDataIndex: Integer): Integer;
begin
  if TreeDataIndex < FNoOfOtus then
    Result := FNoOfOtus + TreeDataIndex
  else
    Result := TreeDataIndex - FNoOfOtus + 1;
end;

function TReltimeComputer.Empty: boolean;
begin
  result := Length(FNodes) = 0;
end;

{$IFDEF DEBUG}
procedure TReltimeComputer.ExportRates(Filename: String);
var
  i: Integer;
  output: array of Extended;
begin
  setLength(output, Length(FNodes));
  if Length(FNodes) > 0 then
    for i := 0 to Length(FNodes) - 1 do
      output[i] := FNodes[i].rate;
  arrayToFile(output, Filename);
end;
{$ENDIF}

function TReltimeComputer.ComputeReltimeBLens(TreeData: TTreeData; Calibrations: TCalibrations; MaxRateRatio: Extended): TTreeData;
var
  i: Integer;
begin
  try
    FLatestTime := 0;
    FMaxRateRatio := MaxRateRatio;
    FSourceData := TTreeData.Create(TreeData.NoOfOTUs, True, False, False);
    FSourceData.Assign(TreeData);
    FNodeLabels := TStringList.Create;
    FNodeLabels.Assign(TreeData.GetInternalNodeLabels);
    InitNodeData(FSourceData);
    for i := 0 to Calibrations.Count - 1 do
      Calibrations.GetCalibration(i).ReltimeTreeIndexBLens := Calibrations.GetCalibration(i).NodeID + TreeData.NoOfOtus;
    Result := ComputeHeights;
    FIsSamplingTime := false;
  Except
    on E:Exception do
    begin
      FLog.Add('Error when computing Reltime branch lengths: ' + E.Message);
      Result := nil;
    end;
  end;
end;

function TReltimeComputer.ComputeRelTimeBLens(TreeData: TTreeData; MaxRateRatio: Extended): TTreeData;
begin
  try
    FLatestTime := 0;
    FMaxRateRatio := MaxRateRatio;
    FSourceData := TTreeData.Create(TreeData.NoOfOTUs, True, False, False);
    FSourceData.Assign(TreeData);
    FNodeLabels := TStringList.Create;
    FNodeLabels.Assign(TreeData.GetInternalNodeLabels);
    Assert(FSourceData.isBlen, 'Reltime requires a tree with branch lengths');
    InitNodeData(FSourceData);
    Result := ComputeHeights;
    FIsSamplingTime := false;
  Except
    on E:Exception do
    begin
      FLog.Add('Error when computing Reltime branch lengths: ' + E.Message);
      Result := nil;
    end;
  end;
end;

function TReltimeComputer.ComputeRelTimeBLens(TreeData: TTreeData; var SamplingTime: array of extended; MaxRateRatio: Extended; namesList: TStringList): TTreeData;
var
  sIndex: Integer;

  procedure SetH0;
  var
    x,y: array of extended;
    mx,my, mx2,my2,mxy: extended;
    sn: integer;

    procedure SetH0Node(n: TReltimeTreeNode);
    begin
      if n.OTU then
      begin
        if abs(TimeFactor) > FP_CUTOFF then
          n.h0 := (n.Time-LatestTime)/TimeFactor;
        n.height := n.h0;
      end
      else
      begin
        SetH0Node(n.des1);
        SetH0Node(n.des2);

        n.h0 := max(n.des1.h0, n.des2.h0);
      end;
    end;

    procedure SetTipDistance(n: TRelTimeTreeNode);
    var
      oriMy: Extended;
    begin
      oriMy := my;
      my := my +n.blen;

      if n.OTU then
      begin
        x[sn]  := n.Time;
        y[sn]  := my;
        n.rtd1 := my;
        inc(sn);
      end
      else
      begin
        SetTipDistance(n.des1);
        SetTipDistance(n.des2);
      end;

      my := oriMy;
      //my := my -n.blen;
    end;

  var
    a0,b0,a1,b1,a,b: extended;
    i,j,n,nn,mini,maxi: integer;
    cx,cy,bx: array of extended;
    cn: array of integer;
    maxx,minx: extended;
  begin
    FTimeFactor := 0;
    FLatestTime := 0;
    for i := 0 to 2*NoOfOtus-2 do
      Node[i].h0 := 0;
    for i := 0 to NoOfOtus-1 do
      Node[i].Time := SamplingTime[i];

    setlength(x, NoOfOtus);
    setlength(y, NoOfOtus);
    setlength(bx, NoOfOtus);
    setlength(cx, NoOfOtus);
    setlength(cy, NoOfOtus);
    setlength(cn, NoOfOtus);

    sn := 0;
    my := 0;

    SetTipDistance(Root.des1);
//    SetTipDistance(Root.des2);    // Must be outgroups
    maxi := 0;
    mini := 0;
    j := 0;
    while (j < sn-1) and (x[j] < FP_CUTOFF) do
      inc(j);
    maxx := x[j];
    minx := x[j];
    maxi := j;
    mini := j;
    for i := j+1 to sn-1 do
    begin
      if maxx < x[i] then
      begin
        maxx := x[i];
        maxi := i;
      end;
      if minx > x[i] then
      begin
        minx := x[i];
        mini := i;
      end;
    end;

    nn := sn;

    for i := 0 to sn-1 do
    begin
      cx[i] := 0;
      cy[i] := 0;
      cn[i] := 0;
    end;

    for i := 0 to nn-1 do
    begin
      cx[i] := x[i];
      cy[i] := y[i];
      cn[i] := 1;
    end;

    mx := 0;
    my := 0;
    n  := 0;
    for i := 0 to nn-1 do
    begin
      if cn[i] = 0 then continue;
      mx := mx +cx[i];
      my := my +cy[i];
      inc(n);
    end;
    mx := mx/n;
    my := my/n;

    mx2 := 0;
    my2 := 0;
    mxy := 0;
    for i := 0 to nn-1 do
    begin
      if cn[i] = 0 then continue;
      mx2 := mx2 +(cx[i] -mx)*(cx[i] -mx);
      my2 := my2 +(cy[i] -my)*(cy[i] -my);
      mxy := mxy +(cx[i] -mx)*(cy[i] -my);
    end;

    if mx2 < FP_CUTOFF then
    begin
      for i := 0 to NoOfOtus-1 do
        Node[i].rtd0 := my;
      exit;
    end;

    a0 := mxy/mx2;
    b0 := -mxy/mx2*mx +my;

/////

    for i := 0 to sn-1 do
    begin
      cx[i] := 0;
      cy[i] := 0;
      cn[i] := 0;
    end;

    if NoOfOtus > 100 then
      nn := NoOfOtus div 20
    else if NoOfOtus > 50 then
      nn := NoOfOtus div 10
    else
      nn := 5;

    bx[0] := minx  +(maxx-minx)/nn;
    for i := 1 to nn-1 do
      bx[i] := bx[i-1] +(maxx-minx)/nn;

    for i := 0 to sn-1 do
    begin
      j := 0;
      while (j < nn-1) and (x[i] > bx[j]) do
        inc(j);

      cx[j] := cx[j] +x[i];
      cy[j] := cy[j] +ln(y[i]);
      inc(cn[j]);
    end;

    for i := 0 to nn-1 do
    begin
      if cn[i] = 0 then continue;
      cx[i] := cx[i]/cn[i];
      cy[i] := exp(cy[i]/cn[i]);
    end;

    mx := 0;
    my := 0;
    n  := 0;
    for i := 0 to nn-1 do
    begin
      if cn[i] = 0 then continue;
      mx := mx +cx[i];
      my := my +cy[i];
      inc(n);
    end;
    mx := mx/n;
    my := my/n;

    mx2 := 0;
    my2 := 0;
    mxy := 0;
    for i := 0 to nn-1 do
    begin
      if cn[i] = 0 then continue;
      mx2 := mx2 +(cx[i] -mx)*(cx[i] -mx);
      my2 := my2 +(cy[i] -my)*(cy[i] -my);
      mxy := mxy +(cx[i] -mx)*(cy[i] -my);
    end;

    a1 := mxy/mx2;
    b1 := -mxy/mx2*mx +my;

    a := a0;
    b := b0;
    if abs(a0) > FP_CUTOFF then
    begin
      if (abs(a1) > FP_CUTOFF) and (abs((a0-a1)/a0) > 0.1) then
      begin
        a := a1;
        b := b1;
      end;
    end
    else if abs(a1) > FP_CUTOFF then
    begin
      a := a1;
      b := b1;
    end;

    for i := 0 to NoOfOtus-1 do
      if not Node[i].IsOutgroupMember then
      begin
        Node[i].h0 := a*Node[i].Time +b;
        Node[i].rtd0 := Node[i].h0;
      end;
////
    my := 0;
    for i := 0 to NoOfOtus-1 do
      if not Node[i].IsOutgroupMember then
        if my < Node[i].h0 then
          my := Node[i].h0;
    mx := 0;
    for i := 0 to NoOfOtus-1 do
      if not Node[i].IsOutgroupMember then
      begin
        Node[i].h0 := my -Node[i].h0;
        if mx < Node[i].h0 then
          mx := Node[i].h0;
      end;
    for i := 0 to NoOfOtus-1 do
      if Node[i].IsOutgroupMember then
        Node[i].h0 := mx;
////

    mini := 0;
    while (mini < NoOfOtus-1) and (Node[mini].rtd1 < FP_CUTOFF) do
      inc(mini);
    my := Node[mini].h0;
    for i := mini+1 to NoOfOtus-1 do
    begin
      if Node[i].h0 < my then
      begin
        my   := Node[i].h0;
        mini := i;
      end;
    end;
    maxi := 0;
    while (maxi < NoOfOtus-1) and (Node[maxi].rtd1 < FP_CUTOFF) do
      inc(maxi);
    my   := Node[maxi].h0;
    for i := maxi+1 to NoOfOtus-1 do
      if Node[i].h0 > my then
      begin
        my   := Node[i].h0;
        maxi := i;
      end;

    if Node[maxi].h0-Node[mini].h0 < FP_CUTOFF then exit;

    FTimeFactor := (Node[maxi].Time-Node[mini].Time)/(Node[maxi].h0-Node[mini].h0);
    FMaxTimeFactor := FTimeFactor;
    FMinTimeFactor := FTimeFactor;
    FLatestTime := Node[mini].Time;

    SetH0Node(Root);

    setlength(x, 0);
    setlength(y, 0);
    setlength(bx, 0);
    setlength(cx, 0);
    setlength(cy, 0);
    setlength(cn, 0);
  end;

begin
  try
    FNamesList := namesList;
    SetLength(FOrigSamplingTimes, Length(SamplingTime));
    FLatestSampleDate := SamplingTime[0];
    for sIndex := 0 to Length(SamplingTime) - 1 do
    begin
      FOrigSamplingTimes[sIndex] := SamplingTime[sIndex];
      if CompareValue(SamplingTime[sIndex], FLatestSampleDate, FP_CUTOFF) > 0 then
        FLatestSampleDate := SamplingTime[sIndex];
    end;
    FLatestTime := 0.0;
    FMaxRateRatio := MaxRateRatio;
    FSourceData := TTreeData.Create(TreeData.NoOfOTUs, True, False, False);
    FSourceData.Assign(TreeData);
    FNodeLabels := TStringList.Create;
    FNodeLabels.Assign(TreeData.GetInternalNodeLabels);
    InitNodeData(FSourceData);
    SetH0;
    FIsSamplingTime := True;
    Result := ComputeHeights;

  Except
    on E:Exception do
    begin
      FLog.Add('Error when computing Reltime tree: ' + E.Message);
      Result := nil;
    end;
  end;
end;

function TReltimeComputer.ComputeRelTimeBLens(TreeData: TTreeData;
                                              var ExportList: TStringList;
                                              const NamesList: TStringList;
                                              const NodeLabels: TStringList;
                                              MaxRateRatio: Extended): TTreeData;
begin
  try
    FNodeLabels := TStringList.Create;
    FNodeLabels.Assign(TreeData.GetInternalNodeLabels);
    Result := ComputeRelTimeBLens(TreeData, MaxRateRatio);
    if not Assigned(ExportList) then
      ExportList := TStringList.Create
    else
      ExportList.Clear;
    GenerateClockTreeExport(ExportList, NamesList, NodeLabels);
  Except
    on E:Exception do
    begin
      Result := nil;
      raise Exception.Create(E.Message);
    end;
  end;
end;

function TReltimeComputer.ComputeRelTimeBLens(TreeData: TTreeData;
                                              var ExportList: TStringList;
                                              const NamesList: TStringList;
                                              var NodeLabels: TStringList;
                                              var CalibrationTimes: TCalibrations;
                                              MaxRateRatio: Extended): TTreeData;
var
  TempData: TTreeData;
begin

  try
    try
      TempData := ComputeRelTimeBLens(TreeData, MaxRateRatio);
      TempData.ResolveRelTimes(CalibrationTimes);
      CheckCalibrationIds(CalibrationTimes);
      CalibrationTimes.PrepareDivTimeArrays(FMinTimes, FMaxTimes, TreeData.NoOfOTUs, False, FNodeLabels, False);
      Result := AnchorClockTree(FMinTimes, FMaxTimes);
      if not Assigned(ExportList) then
        ExportList := TStringList.Create
      else
        ExportList.Clear;
      GenerateCalibratedClockTreeExport(ExportList, NamesList, NodeLabels, FOrigMinTimes, FOrigMaxTimes);
      {$IFNDEF VISUAL_BUILD}
      if CalibrationTimes.CalibrationsModified then
        CalibrationTimes.SaveToFile(NextAvailableFilenameNV('_calibrations_used.txt'));
      {$ENDIF}
    except
      on E:Exception do
      begin
        Result := nil;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if Assigned(TempData) then
      TempData.Free;
  end;
end;

procedure TReltimeComputer.Assign(Source: TReltimeComputer);
var
  i: Integer;
begin
  if Assigned(Source.FNodeLabels) then
    FNodeLabels := Source.FNodeLabels
  else
    FNodeLabels := nil;

  FPropagateConstraints := Source.FPropagateConstraints;
  SetLength(FOrigSamplingTimes, Length(Source.FOrigSamplingTimes));
  if Length(FOrigSamplingTimes) > 0 then
    for i := Low(FOrigSamplingTimes) to High(FOrigSamplingTimes) do
    FOrigSamplingTimes[i] := Source.FOrigSamplingTimes[i];
  FVarianceEstimationMethod := Source.FVarianceEstimationMethod;

  SetLength(FOriginalReltimes, Length(Source.FOriginalReltimes));
  if Length(FOriginalReltimes) > 0 then
    for i := Low(FOriginalReltimes) to High(FOriginalReltimes) do
    FOriginalReltimes[i] := Source.FOriginalReltimes[i];
  FVarianceEstimationMethod := Source.FVarianceEstimationMethod;

  FLog.Assign(Source.FLog);

  if Assigned(Source.FNamesList) then
    FNamesList := Source.FNamesList
  else
    FNamesList := nil;
  IsVarBlen := Source.IsVarBlen;
  Zvalue := Source.Zvalue;

  FIsBlensOnly := Source.FIsBlensOnly;
  FIsLittleBootstrapsTiming := Source.FIsLittleBootstrapsTiming;
  if Assigned(FSourceData) then
    FSourceData := Source.FSourceData.Clone;
  FNoOfOtus := Source.FNoOfOtus;
  ClearNodes;
  SetLength(FNodes, Length(Source.FNodes));
  if Length(FNodes) > 0 then
  begin
    for i := 0 to Length(FNodes) - 1 do
      FNodes[i] := TReltimeTreeNode.Create(i);
    for i := 0 to Length(FNodes) - 1 do
    begin
      FNodes[i].Assign(Source.FNodes[i]);
      if Assigned(Source.FNodes[i].Des1) then
        FNodes[i].Des1 := FNodes[Source.FNodes[i].Des1.Index]
      else
        FNodes[i].Des1 := nil;
      if Assigned(Source.FNodes[i].Des2) then
        FNodes[i].Des2 := FNodes[Source.FNodes[i].Des2.Index]
      else
        FNodes[i].Des2 := nil;
      if Assigned(Source.FNodes[i].Anc) then
        FNodes[i].Anc := FNodes[Source.FNodes[i].Anc.Index]
      else
        FNodes[i].Anc := nil;
    end;
  end;

  FRoot := FNodes[Source.FRoot.Index];
  FNumNodesProcessed := Source.FNumNodesProcessed;
  FCaptionString := Source.FCaptionString;
  FTimeFactor := Source.FTimeFactor;
  FMinTimeFactor := Source.FMinTimeFactor;
  FMaxTimeFactor := Source.FMaxTimeFactor;

  SetLength(FMinTimes, Length(Source.FMinTimes));
  SetLength(FMaxTimes, Length(Source.FMaxTimes));
  if Length(FMinTimes) > 0 then
    for i := Low(FMinTimes) to High(FMinTimes) do
    begin
      FMinTimes[i] := Source.FMinTimes[i];
      FMaxTimes[i] := Source.FMaxTimes[i];
    end;

  SetLength(FOrigMinTimes, Length(Source.FOrigMinTimes));
  SetLength(FOrigMaxTimes, Length(Source.FOrigMaxTimes));
  if Length(FOrigMinTimes) > 0 then
    for i := Low(FOrigMinTimes) to High(FOrigMinTimes) do
    begin
      FOrigMinTimes[i] := Source.FOrigMinTimes[i];
      FOrigMaxTimes[i] := Source.FOrigMaxTimes[i];
    end;

  FMaxRateRatio := Source.FMaxRateRatio;
  FLatestTime := Source.FLatestTime;
  FIsSamplingTime := Source.FIsSamplingTime;
  FLatestSampleDate := Source.FLatestSampleDate;
  FStrictClockRate := Source.FStrictClockRate;
  if Assigned(Source.FCorrTestTree) then
  begin
    if not Assigned(FCorrTestTree) then
      FCorrTestTree := TTreeData.Create(Source.FCorrTestTree.NoOfOTUs, True, False, False);
    FCorrTestTree.Assign(Source.FCorrTestTree);
  end;
end;

procedure TReltimeComputer.SetNodeLabels(aNodeLabels: TStringList);
begin
  FNodeLabels := aNodeLabels;
end;

procedure TReltimeComputer.ChangeRootByOutgroup;
var
  newroot: TReltimeTreeNode;
  maxsize: integer;

  procedure FindRoot0(node: TReltimeTreeNode);
  begin
    if node.OTU then
    begin
      if not node.flag then
        if maxsize = 0 then
        begin
          newroot := node;
          maxsize := 1;
        end;
    end
    else if node.flag then
    begin
      FindRoot0(node.des1);
      FindRoot0(node.des2);
    end
    else if maxsize < node.size then
    begin
      newroot := node;
      maxsize := node.size;
    end;
  end;

  procedure FindRoot1(node: TReltimeTreeNode);
  begin
    if node.OTU then
    begin
      if node.flag then
        newroot := node;
    end
    else
    begin
      if node.des1.flag then
        FindRoot1(node.des1);
      if node.des2.flag then
        FindRoot1(node.des2);
      if node.des1.flag and node.des2.flag then
        newroot := node;
    end;
  end;

var
  a, p, d : TReltimeTreeNode;
  len0, len1, len2: extended;
  fix0, fix1, fix2: boolean;
begin
  newroot := nil;
  FlagOutgroupNodes;
  maxsize := 0;
  if not FRoot.flag then
    newroot := FRoot
  else if FRoot.des1.flag and FRoot.des2.flag then
    FindRoot0(FRoot)
  else if FRoot.des1.flag then
    FindRoot1(FRoot.des1)
  else if root.des2.flag then
    FindRoot1(FRoot.des2);

  if (newroot = FRoot) or (newroot = FRoot.des2) then
    exit;

  if newroot = FRoot.des1 then
  begin
    FRoot.des1 := FRoot.des2;
    FRoot.des2 := newroot;

    FRoot.des1.blen := FRoot.des1.blen + FRoot.des2.blen;
    FRoot.des2.blen := 0;
    FRoot.des1.vb := FRoot.des1.vb + FRoot.des2.vb;
    FRoot.des2.vb := 0;
    exit;
  end;

  len0 := FRoot.des1.blen + FRoot.des2.blen;
  fix0 := FRoot.des1.fixed and FRoot.des2.fixed;
  d := newroot;
  p := d.anc;
  a := p.anc;
  len2 := d.blen;
  fix2 := d.fixed;
  while p <> FRoot do
  begin
    len1 := p.blen;
    p.blen := len2;
    len2 := len1;
    fix1 := p.fixed;
    p.fixed := fix2;
    fix2 := fix1;

    p.anc := d;
    if d = p.des1 then
      p.des1 := a
    else
      p.des2 := a;
    d := p;
    p := a;
    a := a.anc;
  end;
  if d = p.des1 then
  begin
    p.des2.anc := d;
    p.des2.blen := len0;
    p.des2.fixed := fix0;
    if p = d.des1 then
      d.des1 := p.des2
    else
      d.des2 := p.des2;
  end
  else
  begin
    p.des1.anc := d;
    p.des1.blen := len0;
    p.des1.fixed := fix0;
    if p = d.des1 then
      d.des1 := p.des1
    else
      d.des2 := p.des1;
  end;
  len0 := newroot.blen;
  fix0 := newroot.fixed;
  p := newroot.anc;
  p.anc := FRoot;
  p.blen := len0;
  p.fixed := fix0;
  newroot.anc := FRoot;
  newroot.blen := 0;
  newroot.fixed := p.fixed;
  if newroot.flag then
  begin
    FRoot.des1 := p;
    FRoot.des2 := newroot;
  end
  else
  begin
    FRoot.des1 := newroot;
    FRoot.des2 := p;
  end;

  FRoot.des2.blen := 0;
  FRoot.des1.blen := FRoot.des1.blen +FRoot.des2.blen;
  FRoot.des2.vb := 0;
  FRoot.des1.vb := FRoot.des1.vb +FRoot.des2.vb;

  SetSizeDepth;
end;

procedure TReltimeComputer.DebugDumpOutgroupIngroupInfo;
var
  aList: TStringList = nil;

  procedure PreorderTraversal(n: TRelTimeTreeNode);
  begin
    if Assigned(n.des1) then
      PreorderTraversal(n.des1);
    if Assigned(n.des2) then
      PreorderTraversal(n.des2);
    aList.Add(n.DebugStringOneLine);
  end;

begin
  {$IFDEF DEBUG}
  try
    aList := TStringList.Create;
    aList.Add(Root.DebugHeaderStringOneLine);
    PreorderTraversal(Root.des1);
    aList.SaveToFile('C:\Users\gstecher\mydata\Downloads\des1.txt');
    aList.Clear;
    aList.Add(Root.DebugHeaderStringOneLine);
    PreorderTraversal(Root.des2);
    aList.SaveToFile('C:\Users\gstecher\mydata\Downloads\des2.txt');
  finally
    if Assigned(aList) then
      aList.Free;
  end;
  {$ENDIF}
end;

procedure TReltimeComputer.ClearNodes;
var
  i: Integer;
begin
  if Length(FNodes) > 0 then
    for i := 0 to Length(FNodes) - 1 do
      FNodes[i].Free;
  SetLength(FNodes, 0);
end;

function TReltimeComputer.ComputeRelTimeBLens(TreeData: TTreeData;
                                              var ExportList: TStringList;
                                              const NamesList: TStringList;
                                              var CalibrationTimes: TCalibrations;
                                              MaxRateRatio: Extended): TTreeData;
var
  i:Integer = -1;
  TempData: TTreeData = nil;
  NodeLabels: TStringList = nil;
  aLabel: String = '';
  aCalib: TCalibrationTime = nil;
begin
  try
    try
      FNodeLabels := TStringList.Create;
      FNodeLabels.Assign(TreeData.GetInternalNodeLabels);
      TempData := ComputeRelTimeBLens(TreeData, CalibrationTimes, MaxRateRatio);
      CheckCalibrationIds(CalibrationTimes);
      CalibrationTimes.PrepareDivTimeArrays(FMinTimes, FMaxTimes, TreeData.NoOfOTUs, True, FNodeLabels, False);
      Result := AnchorClockTree(FMinTimes, FMaxTimes);
      if not Assigned(ExportList) then
        ExportList := TStringList.Create;
      ExportList.Clear;
      NodeLabels := TreeData.GetInternalNodeLabels;
      for i := 0 to CalibrationTimes.Count - 1 do
      begin
        aCalib := CalibrationTimes.GetCalibration(i);
        aLabel := aCalib.NodeLabel;
        if aLabel <> EmptyStr then
          NodeLabels[aCalib.ReltimeTreeIndexBLens - FNoOfOtus] := aLabel;
      end;

      if IsDeveloper then
        GenerateCalibratedClockTreeExport(ExportList, NamesList, NodeLabels, FMinTimes, FMaxTimes)
      else
        GenerateCalibratedClockTreeExport(ExportList, NamesList, NodeLabels, FOrigMinTimes, FOrigMaxTimes);
      {$IFNDEF VISUAL_BUILD}
      if CalibrationTimes.CalibrationsModified then
        CalibrationTimes.SaveToFile(NextAvailableFilenameNV('_calibrations_used.txt'));
      {$ENDIF}
    except
      on E:Exception do
      begin
        Result := nil;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if Assigned(TempData) then
      TempData.Free;
  end;
end;

function TReltimeComputer.ComputeLbsDivTimes(treeData: TTreeData; var calibs: TCalibrations): TTreeData;
var
  tempData: TTreeData = nil;
  i: Integer = -1;
  c: TCalibrationTime = nil;
begin
  try
    FLatestTime := 0;
    FMaxRateRatio := DEFAULT_MAX_RATE_RATIO;
    FSourceData := TTreeData.Create(TreeData.NoOfOTUs, True, False, False);
    FSourceData.Assign(TreeData);
    InitNodeData(FSourceData);
    CheckCalibrationIds(calibs);
    for i := 0 to calibs.Count - 1 do
    begin
      c := calibs.GetCalibration(i);
      if c.NodeID < 0 then
        raise Exception.Create(Format('bad node id(%d) for calibration', [c.NodeID]));
    end;
    tempData := ComputeHeights;
    FIsSamplingTime := False;
    calibs.PrepareDivTimeArrays(FMinTimes, FMaxTimes, TreeData.NoOfOTUs, True, FRoot.des1.index, False);
    Result := AnchorClockTree(FMinTimes, FMaxTimes);
    {$IFNDEF VISUAL_BUILD}
    if calibs.CalibrationsModified then
      calibs.SaveToFile(NextAvailableFilenameNV('_calibrations_used.txt'));
    {$ENDIF}
  finally
    if Assigned(tempData) then
      tempData.Free;
  end;
end;

procedure TReltimeComputer.GenerateSpreadsheetExport(const names, labels: TStringList; const minTimes, maxTimes: array of Extended);
begin
  FExportBuilder.Initialize(self, FNodes, names, labels, FIsBlensOnly or FIsLittleBootstrapsTiming, minTimes, maxTimes);
end;

procedure TReltimeComputer.GenerateSpreadsheetExport(const names, labels: TStringList);
begin
  FExportBuilder.Initialize(self, FNodes, names, labels, FIsBlensOnly or FIsLittleBootstrapsTiming);
end;

procedure TReltimeComputer.GenerateSpreadsheetExport(const names, labels: TStringList; const sampleTimes: array of Extended);
begin
  FExportBuilder.Initialize(self, FNodes, names, labels, FIsBlensOnly or FIsLittleBootstrapsTiming, sampleTimes);
end;

function TReltimeComputer.CorrelationTest(TreeData: TTreeData; MaxRateRatio: Extended; ComputeReltimes: Boolean): TCorrelationTest;
var
  aData: TTreeData = nil;
  siblingRates: TCorrTestDataList = nil;
  directDescendentRates: TCorrTestDataList = nil;
  secondDescendentRates: TCorrTestDataList = nil;
  thirdDescendentRates: TCorrTestDataList = nil;
begin
  try
    if ComputeReltimes then
      aData := ComputeRelTimeBLens(TreeData, MaxRateRatio);
    siblingRates := GetSiblingRates;
    directDescendentRates := GetDirectDescendentRates;
    secondDescendentRates := GetSecondDescendentRates;
    thirdDescendentRates := GetThirdDescendentRates;
    Result := TCorrelationTest.Create(siblingRates, directDescendentRates, secondDescendentRates, thirdDescendentRates);
    Result.Compute;

   // {$IFDEF DEBUG}
   // siblingRates.SaveToFile('c:\src\siblingRates.txt');
   // directDescendentRates.SaveToFile('c:\src\directRates.txt');
   // secondDescendentRates.SaveToFile('c:\src\secondRates.txt');
   // thirdDescendentRates.SaveToFile('c:\src\thirdRates.txt');
   // {$ENDIF}
  finally
    if Assigned(aData) then
      aData.Free;
    if Assigned(siblingRates) then
      siblingRates.Free;
    if Assigned(directDescendentRates) then
      directDescendentRates.Free;
    if Assigned(secondDescendentRates) then
      secondDescendentRates.Free;
    if Assigned(thirdDescendentRates) then
      thirdDescendentRates.Free;
  end;
end;

function TReltimeComputer.AutomaticCorrelationTest(MaxRateRatio: Extended): TCorrelationTest;
var
  c: TReltimeComputer = nil;
begin
  if not Assigned(FCorrTestTree) then
    raise Exception.Create('Application Error: CorrTest tree not initialized');
  try
    c := TReltimeComputer.Create;
    c.IsBlensOnly := True;
    Result := c.CorrelationTest(FCorrTestTree, MaxRateRatio, True);
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

function TReltimeComputer.ComputeDensityDistributionTree(TreeData: TTreeData; MaxRateRatio: Extended; aMinTimes, aMaxTimes: TDivTimesArray): TTreeData;
var
  i:Integer;
  TempData: TTreeData = nil;
begin
  try
    TempData := ComputeRelTimeBLens(TreeData, MaxRateRatio);
    SetLength(FMinTimes, Length(aMinTimes));
    SetLength(FMaxTimes, Length(FMinTimes));
    for i := 0 to Length(FMinTimes) - 1 do
    begin
      FMinTimes[i] := aMinTimes[i];
      FMaxTimes[i] := aMaxTimes[i];
    end;
    Result := AnchorClockTree(FMinTimes, FMaxTimes);
  finally
    if Assigned(TempData) then
      TempData.Free;
  end;
end;

constructor TReltimeComputer.Create;
begin
  FNeedsRootByOutgroup := True;
  FIsLittleBootstrapsTiming := False;
  FStrictClockRate := 1;
  FNumNodesProcessed := 0;
  SetLength(FNodes, 0);
  SetLength(FOriginalReltimes, 0);
  FSourceData := nil;
  SetLength(FMinTimes, 0);
  SetLength(FMaxTimes, 0);
  FIsBLensOnly := False;
  FVarianceEstimationMethod := rvmNone;
  FMaxRateRatio := DEFAULT_MAX_RATE_RATIO;
  Zvalue := 1.96;
  FLog := TStringList.Create;
  FPropagateConstraints := True;
  FExportBuilder := TReltimeExportBuilder.Create;
  FIsSamplingTime := False;
  FCorrTestTree := nil;
  FNamesList := nil;
end;

function TReltimeComputer.DataCoverage(index: Integer): Double;
begin
  Result := FNodes[index].DataCoverage;
end;

destructor TReltimeComputer.Destroy;
begin
  FNamesList := nil; { not owned}
  ProgressProc := nil;
  ClearNodes;
  if Assigned(FSourceData) then
    FSourceData.Free;
  if Assigned(FLog) then
    FLog.Free;
  ProgressProc := nil;
  SetLength(FMinTimes, 0);
  SetLength(FMaxTimes, 0);
  //if Assigned(FSpreadsheetExport) then
  //  FSpreadsheetExport.Free;
  if Assigned(FExportBuilder) then
    TReltimeExportBuilder(FExportBuilder).Free;
  if Assigned(FCorrTestTree) then
    FCorrTestTree.Free;
  inherited;
end;

{$IFDEF DEBUG}
function TReltimeComputer.BlensFunc(tree: TTreeData): Double;
var
  i: integer;
begin
  tree.isBLen := true;
  for i := FNoOfOtus to 2*FNoOfOtus - 2 do
  begin
    Tree.NodeArray[i-FNoOfOtus].des1 := FNodes[i].des1.index;
    Tree.NodeArray[i-FNoOfOtus].des2 := FNodes[i].des2.index;
    Tree.BLenArray[FNodes[i].des1.index] := abs(FNodes[i].height - FNodes[i].des1.height);
    Tree.BLenArray[FNodes[i].des2.index] := abs(FNodes[i].height - FNodes[i].des2.height);
  end;
  for i := 0 to FNoOfOtus - 1 do
    Tree.IsOutgroupMember[i] := FNodes[i].IsOutgroupMember;
  Tree.Value := Tree.SBL;
  Result := Tree.Value;
end;

{$ENDIF}

function TReltimeComputer.ElapsedDaysForRtdtTime(index: Integer): Int64;
begin
  Result := RtdtDaysElapsed(FLatestSampleDate, DivTime(index));
end;

function TReltimeComputer.FindTwoExtantTaxa(InternalNodeId: Integer;
  TaxaNamesList: TStringList; var TaxaAId: Integer; var TaxaAName: String;
  var TaxaBId: Integer; var TaxaBName: String): Boolean;

  function FindOneLeafNode(AncestorId: Integer): Boolean;
  var
    CurrIndex: Integer;
  begin
    Result := False;
    CurrIndex := AncestorId;
    while True do
    begin
      if not Assigned(FNodes[CurrIndex].des1.des1) then // found a leaf node
      begin
        CurrIndex := FNodes[CurrIndex].des1.Index;
        break;
      end;
      CurrIndex := FNodes[CurrIndex].des1.Index;
    end;
    if CurrIndex <> AncestorId then
    begin
      Result := True;
      TaxaAId := CurrIndex;
      TaxaAName := TaxaNamesList[TaxaAId];
    end;
  end;

  function FindAnotherLeafNode(AncestorId: Integer): Boolean;
  var
    CurrIndex: Integer;
  begin
    Result := False;
    CurrIndex := AncestorId;
    while True do
    begin
      if not Assigned(FNodes[CurrIndex].des2.des2) then // found a leaf node
      begin
        CurrIndex := FNodes[CurrIndex].des2.Index;
        break;
      end;
      CurrIndex := FNodes[CurrIndex].des2.Index;
    end;
    if CurrIndex <> AncestorId then
    begin
      Result := True;
      TaxaBId := CurrIndex;
      TaxaBName := TaxaNamesList[TaxaBId];
    end;
  end;
begin
  Result := FindOneLeafNode(InternalNodeId) and FindAnotherLeafNode(InternalNodeId);
end;

procedure TReltimeComputer.FlagOutgroupNodes;

  procedure ProcessNode(ANode: TRelTimeTreeNode);
  var
    n: TRelTimeTreeNode;
  begin
    n := ANode;
    if n.IsOutgroupMember then
    begin
      n.flag := True;
      while (n.anc <> nil) and (not n.anc.flag) do
      begin
        n.Anc.flag := True;
        //n.IsOutgroupMember := True;
        n := n.Anc;
      end;
    end;
  end;

var
  i: Integer;
begin
  for i := 0 to 2 * FNoOfOtus - 2 do
    Node[i].flag := False;
  for i := 0 to FNoOfOtus - 1 do
    ProcessNode(Node[i]);
end;

procedure TReltimeComputer.FlagOutgroupCluster;

  procedure InOrderTraversal(n: TRelTimeTreeNode);
  begin
    n.flag := True;
    if Assigned(n.des1) then
      InOrderTraversal(n.des1);
    if Assigned(n.des2) then
      InOrderTraversal(n.des2);
  end;

var
  i: Integer;
begin
  for i := 0 to 2 * FNoOfOtus - 2 do
    Node[i].flag := False;
  Root.flag := True;
  InOrderTraversal(Root.des2);
end;

procedure TReltimeComputer.GenerateCalibratedClockTreeExport(var ExportList: TStringList; const NamesList, NodeLabels: TStringList; MinTimes: array of Extended; MaxTimes: array of Extended);
const
  IdFormatString = '%6d';
  SIdFormatString = '%6s';
  FloatFormatString = '%16.6f';
  SFloatFormatString = '%16s';
  FloatFormatString2 = '%23.6f';
  SFloatFormatString2 = '%23s';
  PercentString = '%13.2f%%';
  SPercentString = '%13s';
var
  i: integer;
  TempString: AnsiString;
  OtuName: AnsiString;
  TaxonColumnWidth: Integer;
  NodeLabel: AnsiString;
  NoOfNames: Integer;
begin
  if not Assigned(ExportList) then
    ExportList := TStringList.Create
  else
    ExportList.Clear;
  FlagOutgroupNodes;
  NoOfNames := NamesList.Count;
  FNodeLabels := NodeLabels;

  // determine the width of the first column where taxa names will be written
  TaxonColumnWidth := 0;
  for i := 0 to NoOfNames - 1 do
  begin
    OtuName := NamesList[i];
    if Length(OtuName) > TaxonColumnWidth then
      TaxonColumnWidth := Length(OtuName);
  end;
  for i := 0 to NodeLabels.Count - 1 do
    if Length(NodeLabels[i]) > TaxonColumnWidth then
      TaxonColumnWidth := Length(NodeLabels[i]);

  if TaxonColumnWidth > 100 then
    TaxonColumnWidth := 100
  else if TaxonColumnWidth < 15 then
    TaxonColumnWidth := 15
  else
    TaxonColumnWidth := TaxonColumnWidth + 3 + Length(OUTGROUP_MARKER);

  // Write out a header line that gives names for each colum
  TempString := MakePrettyTaxonNameString('NodeLabel', TaxonColumnWidth) + #9;
  TempString := TempString + Format(SIdFormatString, ['NodeId']) + #9;
  TempString := TempString + Format(SIdFormatString, ['Des1']) + #9;
  TempString := TempString + Format(SIdFormatString, ['Des2']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['RelTime']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['StdErr(RelTime)']) + #9;
  TempString := Tempstring + Format(SFloatFormatString, ['Rate']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['DivTime']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['CI_Lower']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['CI_Upper']) + #9;
  TempString := TempString + Format(SFloatFormatString2, ['CI_Lower_No_Constraint']) + #9;
  TempString := TempString + Format(SFloatFormatString2, ['CI_Upper_No_Constraint']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['MinTime']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['MaxTime']) + #9;
  if not (FIsBlensOnly or FIsLittleBootstrapsTiming) then
    TempString := TempString + Format(SPercentString, ['Data Coverage'])
  else
    TempString := Trim(TempString);
  ExportList.Add(TempString);

  // Write out the information for the leaf nodes
  for i := 0 to NoOfNames-1 do
  begin
    OtuName := NamesList[i];
    if FNodes[i].IsOutgroupMember then
      OtuName := OtuName + OUTGROUP_MARKER;
    OtuName := MakePrettyTaxonNameString(OtuName, TaxonColumnWidth);
    TempString := OtuName + #9; // Taxon name
    TempString := TempString + Format(IdFormatString, [FNodes[i].Index + 1]) + #9; // NodeId
    TempString := TempString + Format(SIdFormatString, ['-']) + #9; // Desc1
    TempString := TempString + Format(SIdFormatString, ['-']) + #9; // Desc2
    TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // RelTime
    TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // StdErr (RelTime)
    TempString := TempString + Format(FloatFormatString, [FNodes[i].Rate]) + #9;  // Rate
    TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // DivTime
    TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // CI Lower
    TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // CI Upper
    TempString := TempString + Format(SFloatFormatString2, ['-']) + #9; // CI Lower No Constraint
    TempString := TempString + Format(SFloatFormatString2, ['-']) + #9; // CI Upper No Constraint
    TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // MinTime
    TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // MaxTime
    if not (FIsBlensOnly or FIsLittleBootstrapsTiming) then
      TempString := TempString + Format(SPercentString, ['-']); // DataCoverage
    ExportList.Add(TempString);
  end;

  // Write out the information for the internal nodes
  for i := NoOfNames to 2*NoOfNames-2 do
  begin
    if FIsBlensOnly then
      NodeLabel := GetInternalNodeLabel(FNodes[i].Index - FNoOfOtus, FNodes[i])
    else
      NodeLabel := GetInternalNodeLabel(MapNodeIndexToTreeDataIndex(FNodes[i].Index), FNodes[i]);
    TempString := MakePrettyTaxonNameString(NodeLabel, TaxonColumnWidth) + #9;
    TempString := TempString + Format(IdFormatString, [FNodes[i].Index + 1]) + #9;
    TempString := TempString + Format(IdFormatString, [FNodes[i].des1.Index+1]) + #9;
    TempString := TempString + Format(IdFormatString, [FNodes[i].des2.Index+1]) + #9;

    if FNodes[i].flag then
    begin
      TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // Height
      TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // StdErr(Reltime)
      TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // Rate
      TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // DivTime
      TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // CI Lower
      TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // CI Upper
      TempString := TempString + Format(SFloatFormatString2, ['-']) + #9; // CI Lower No Constraint
      TempString := TempString + Format(SFloatFormatString2, ['-']) + #9; // CI Upper No Constraint
      TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // MinTime
      TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // MaxTime
    end
    else
    begin
      TempString := TempString + Format(FloatFormatString, [FNodes[i].Height]) + #9;
      TempString := TempString + Format(FloatFormatString, [sqrt(FNodes[i].vh)]) + #9;
      TempString := TempString + Format(FloatFormatString, [FNodes[i].Rate]) + #9;
      TempString := TempString + Format(FloatFormatString, [DivTime(i)]) + #9;
      TempString := TempString + Format(FloatFormatString, [MinDivTime(i)]) + #9;
      TempString := TempString + Format(FloatFormatString, [MaxDivTime(i)]) + #9;

      if MinTimes[i] > FP_CUTOFF then
        TempString := TempString + Format(FloatFormatString2, [MinDivTimeRaw(i)]) + #9
      else
        TempString := TempString + Format(SFloatFormatString2, ['-']) + #9;
      if MaxTimes[i] > FP_CUTOFF then
        TempString := TempString + Format(FloatFormatString2, [MaxDivTimeRaw(i)]) + #9
      else
        TempString := TempString + Format(SFloatFormatString2, ['-']) + #9;

      if MinTimes[i] > FP_CUTOFF then
        TempString := TempString + Format(FloatFormatString, [MinTimes[i]]) + #9
      else
        TempString := TempString + Format(SFloatFormatString, ['-']) + #9;
      if MaxTimes[i] > FP_CUTOFF then
        TempString := TempString + Format(FloatFormatString, [MaxTimes[i]]) + #9
      else
        TempString := TempString + Format(SFloatFormatString, ['-']) + #9;
    end;

    if not (FIsBlensOnly or FIsLittleBootstrapsTiming) then
      TempString := TempString + Format(PercentString, [FNodes[i].DataCoverage * 100])
    else
      TempString := Trim(TempString);
    ExportList.Add(TempString);
  end;
  ExportList.Add(EmptyStr);
  ExportList.Add(ResultsFootnote);
  GenerateSpreadsheetExport(NamesList, NodeLabels, MinTimes, MaxTimes);
end;

procedure TReltimeComputer.GenerateClockTreeExport(var ExportList: TStringList;const NamesList, NodeLabels: TStringList);
const
  IdFormatString = '%6d';
  SIdFormatString = '%6s';
  FloatFormatString = '%16.6f';
  SFloatFormatString = '%16s';
  PercentString = '%13.2f%%';
  SPercentString = '%13s';

var
  i: integer;
  TempString: AnsiString;
  OtuName: AnsiString;
  TaxonColumnWidth: Integer;
  NodeLabel: AnsiString;
  NoOfNames: Integer;
begin

  if not Assigned(ExportList) then
    ExportList := TStringList.Create
  else
    ExportList.Clear;
  FlagOutgroupNodes;
  NoOfNames := NamesList.Count;

  // determine the width of the first column where taxa names will be written
  TaxonColumnWidth := 0;
  for i := 0 to NoOfNames - 1 do
  begin
    OtuName := NamesList[i];
    if Length(OtuName) > TaxonColumnWidth then
      TaxonColumnWidth := Length(OtuName);
  end;

  if Assigned(NodeLabels) then
    for i := 0 to NodeLabels.Count - 1 do
    begin
      if Length(NodeLabels[i]) > TaxonColumnWidth then
        TaxonColumnWidth := Length(NodeLabels[i]);
    end;

  if TaxonColumnWidth > 100 then
    TaxonColumnWidth := 100
  else if TaxonColumnWidth < 15 then
    TaxonColumnWidth := 15
  else
    TaxonColumnWidth := TaxonColumnWidth + 3 + Length(OUTGROUP_MARKER);

  // Write out a header line that gives names for each colum
  TempString := MakePrettyTaxonNameString('NodeLabel', TaxonColumnWidth) + #9;
  TempString := TempString + Format(SIdFormatString, ['NodeId']) + #9;
  TempString := TempString + Format(SIdFormatString, ['Des1']) + #9;
  TempString := TempString + Format(SIdFormatString, ['Des2']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['RelTime']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['CI_Lower']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['CI_Upper']) + #9;
  TempString := Tempstring + Format(SFloatFormatString, ['Rate']) + #9;

  if not (FIsBlensOnly or FIsLittleBootstrapsTiming) then
    TempString := TempString + Format(SPercentString, ['Data Coverage'])
  else
    TempString := Trim(TempString);
  ExportList.Add(TempString);

  // Write out the information for the leaf nodes
  for i := 0 to NoOfNames-1 do
  begin
    OtuName := NamesList[i];
    if FNodes[i].IsOutgroupMember then
      OtuName := OtuName + OUTGROUP_MARKER;
    OtuName := MakePrettyTaxonNameString(OtuName, TaxonColumnWidth);
    TempString := OtuName + #9; // Taxon name
    TempString := TempString + Format(IdFormatString, [FNodes[i].Index + 1]) + #9; // NodeId
    TempString := TempString + Format(SIdFormatString, ['-']) + #9; // Desc1
    TempString := TempString + Format(SIdFormatString, ['-']) + #9; // Desc2
    TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // RelTime
    TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // CI Lower
    TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // CI Upper
    TempString := TempString + Format(FloatFormatString, [FNodes[i].Rate]) + #9;  // Rate
    if not (FIsBlensOnly or FIsLittleBootstrapsTiming) then
      TempString := TempString + Format(SPercentString, ['-']); // DataCoverage
    ExportList.Add(TempString);
  end;

  // Write out the information for the internal nodes
  for i := NoOfNames to 2*NoOfNames-2 do
  begin
    NodeLabel := GetInternalNodeLabel(MapNodeIndexToTreeDataIndex(FNodes[i].Index), FNodes[i]);
    TempString := MakePrettyTaxonNameString(NodeLabel, TaxonColumnWidth) + #9;
    TempString := TempString + Format(IdFormatString, [FNodes[i].Index + 1]) + #9;
    TempString := TempString + Format(IdFormatString, [FNodes[i].des1.Index+1]) + #9;
    TempString := TempString + Format(IdFormatString, [FNodes[i].des2.Index+1]) + #9;
    if FNodes[i].flag then
    begin
      TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // RelTime
      TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // MinTime
      TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // MaxTime
      TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // Rate
    end
    else
    begin
      TempString := TempString + Format(FloatFormatString, [FNodes[i].Height]) + #9;
      TempString := TempString + Format(FloatFormatString, [MinDivTime(i)]) + #9;
      TempString := TempString + Format(FloatFormatString, [MaxDivTime(i)]) + #9;
      TempString := TempString + Format(FloatFormatString, [FNodes[i].Rate]) + #9;
    end;

    if not (FIsBlensOnly or FIsLittleBootstrapsTiming) then
      TempString := TempString + Format(PercentString, [FNodes[i].DataCoverage * 100])
    else
      TempString := Trim(TempString);
    ExportList.Add(TempString);
  end;
  ExportList.Add(EmptyStr);
  ExportList.Add(ResultsFootnote);
  GenerateSpreadsheetExport(NamesList, NodeLabels);
end;

function TReltimeComputer.GenerateNexusReltimeExport(NamesList: TStringList; HasCalibrations: Boolean; HasRates: Boolean; HasDataCoverage: Boolean=True): TStringList;
var
  Writer: TNexusWriter;
begin
  Writer := nil;

  try
    Writer := TNexusWriter.Create;
    if HasCalibrations then
      Writer.SetData(FRoot, FNodes, FTimeFactor, NamesList, NoOfOtus)
    else
      Writer.SetData(FRoot, FNodes, NamesList, NoOfOtus);
    Writer.MinDivTimeFunc := MaxDivTime;
    Writer.MaxDivTimeFunc := MinDivTime;
    Writer.DivTimeFunc := DivTime;
    Writer.HasReltimeStdErr := False;
    Writer.HasDataCoverage := HasDataCoverage;
    Writer.HasRates := HasRates;
    Writer.IsSamplingTime := FIsSamplingTime;
    Result := Writer.BuildNexusStrings;
  finally
    if Assigned(Writer) then
      Writer.Free;
  end;
end;

function TReltimeComputer.GenerateStrictClockSpreadsheetExport(const aNames: TStringList; aLabels: TStringList; minTimes: array of Extended; maxTimes: array of Extended; aCaption: TStringList = nil): TExcelWrite;
var
  eb: TReltimeExportBuilder = nil;
begin
  try
    FlagOutgroupNodes;
    eb := TReltimeExportBuilder.Create;
    eb.Initialize(Self, FNodes, aNames, aLabels, FIsBlensOnly or FIsLittleBootstrapsTiming, minTimes, maxTimes);
    if Assigned(aCaption) then
      eb.AddCaptionAsWorksheet(aCaption);
    Result := eb.GenerateExport;
  finally
    if Assigned(eb) then
      eb.Free;
  end;
end;

function TReltimeComputer.GenerateStrictClockSpreadsheetExport(const aNames: TStringList; aLabels: TStringList; aCaption: TStringList): TExcelWrite;
var
  eb: TReltimeExportBuilder = nil;
begin
  try
    FlagOutgroupNodes;
    eb := TReltimeExportBuilder.Create;
    eb.Initialize(Self, FNodes, aNames, aLabels, FIsBlensOnly or FIsLittleBootstrapsTiming);
    if Assigned(aCaption) then
      eb.AddCaptionAsWorksheet(aCaption);
    eb.ReltimeExportType := retStrictClock;
    Result := eb.GenerateExport;
  finally
    if Assigned(eb) then
      eb.Free;
  end;
end;

function TReltimeComputer.GenerateElapsedTimesSpreadsheetExport(
  const aNames: TStringList; aLabels: TStringList; aCaption: TStringList
  ): TExcelWrite;
var
  eb: TElapsedTimesExportBuilder = nil;
  sampleTimes: TArrayOfExt;
  i: Integer = -1;
begin
  try
    FlagOutgroupNodes;
    eb := TElapsedTimesExportBuilder.Create;
    if Length(FOrigMinTimes) > 0 then
      eb.Initialize(Self, FNodes, aNames, aLabels, FIsBlensOnly or FIsLittleBootstrapsTiming, FOrigMinTimes, FOrigMaxTimes)
    else if FIsSamplingTime then
    begin
      SetLength(sampleTimes, FNoOfOtus);
      for i := 0 to FNoOfOtus - 1 do
        sampleTimes[i] := FNodes[i].Time;
      eb.Initialize(Self, FNodes, aNames, aLabels, FIsBlensOnly or FIsLittleBootstrapsTiming, sampleTimes)
    end
    else
      eb.Initialize(Self, FNodes, aNames, aLabels, FIsBlensOnly or FIsLittleBootstrapsTiming);
    if Assigned(aCaption) then
      eb.AddCaptionAsWorksheet(aCaption);

    Result := eb.GenerateExport;
  finally
    if Assigned(eb) then
      eb.Free;
    SetLength(sampleTimes, 0);
  end;
end;

procedure TReltimeComputer.GenerateSampledTimesClockTreeExport(
  var ExportList: TStringList; const NamesList: TStringList;
  const NodeLabels: TStringList; const SampleTimes: array of Extended);
const
  IdFormatString = '%6d';
  SIdFormatString = '%6s';
  SDaysFormatString = '%14s';
  SFloatFormatString = '%16s';
  PercentString = '%13.2f%%';
  SPercentString = '%13s';
  Precision = 7;
  ValueDigits = 16;
var
  i: integer;
  TempString: AnsiString;
  TempFloatStr: String;
  OtuName: AnsiString;
  TaxonColumnWidth: Integer;
  NodeLabel: AnsiString;
begin
  if not Assigned(ExportList) then
    ExportList := TStringList.Create
  else
    ExportList.Clear;
  FNodeLabels := NodeLabels;
  FlagOutgroupNodes; { div times for all outgroup nodes should be set to n/a}
  // determine the width of the first column where taxa names will be written
  TaxonColumnWidth := 0;
  for i := 0 to NamesList.Count - 1 do
  begin
    OtuName := NamesList[i];
    if Length(OtuName) > TaxonColumnWidth then
      TaxonColumnWidth := Length(OtuName);
  end;

  if TaxonColumnWidth > 100 then
    TaxonColumnWidth := 100
  else if TaxonColumnWidth < 15 then
    TaxonColumnWidth := 15
  else
    TaxonColumnWidth := TaxonColumnWidth + 3 + Length(OUTGROUP_MARKER);
  for i := 0 to NodeLabels.Count - 1 do
    if Length(NodeLabels[i]) > TaxonColumnWidth then
      TaxonColumnWidth := Length(NodeLabels[i]);
  // Write out a header line that gives names for each colum
  TempString := MakePrettyTaxonNameString('NodeLabel', TaxonColumnWidth) + #9;
  TempString := TempString + Format(SIdFormatString, ['NodeId']) + #9;
  TempString := TempString + Format(SIdFormatString, ['Des1']) + #9;
  TempString := TempString + Format(SIdFormatString, ['Des2']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['RelTime']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['StdErr(RelTime)']) + #9;
  TempString := Tempstring + Format(SFloatFormatString, ['Rate']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['DivTime']) + #9;
  TempString := TempString + Format(SDaysFormatString, ['Elapsed (Days)']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['CI_Lower']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['CI_Upper']) + #9;
  TempString := TempString + Format(SFloatFormatString, ['Sample_Time']) + #9;
  if not (FIsBlensOnly or FIsLittleBootstrapsTiming) then
    TempString := TempString + Format(SPercentString, ['Data Coverage']);
  ExportList.Add(TempString);

  // Write out the information for the leaf nodes
  for i := 0 to NamesList.Count - 1 do
  begin
    OtuName := NamesList[i];
    if FNodes[i].IsOutgroupMember then
      OtuName := OtuName + OUTGROUP_MARKER;
    OtuName := MakePrettyTaxonNameString(OtuName, TaxonColumnWidth);
    TempString := OtuName + #9; // Taxon name
    TempString := TempString + Format(IdFormatString, [FNodes[i].index + 1]) + #9; // NodeId
    TempString := TempString + Format(SIdFormatString, ['-']) + #9; // Desc1
    TempString := TempString + Format(SIdFormatString, ['-']) + #9; // Desc2

    if IsSamplingTime then
    begin
      TempFloatStr := Trim(FormatDoubleSafe(FNodes[i].height, Precision, ValueDigits));
      TempString := TempString + Format(SFloatFormatString, [TempFloatStr]) + #9; // Reltime
    end
    else
      TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // Reltime
    TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // reltime stderr
    TempFloatStr := Trim(FormatDoubleSafe(FNodes[i].rate, Precision, ValueDigits));
    TempString := TempString + Format(SFloatFormatString, [TempFloatStr]) + #9;  // Rate
    TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // DivTime
    TempString := TempString + Format(SDaysFormatString, [Format('%.0n', [ElapsedDaysForRtdtTime(i)*1.0])]) + #9; // Elapsed (Days)
    TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // LowerBound
    TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // UpperBound
    if FNodes[i].Time > FP_CUTOFF then
    begin
      TempFloatStr := RtdtFloatTimeToDateString(FNodes[i].Time);
      TempString := TempString + Format(SFloatFormatString, [TempFloatStr]) + #9;
    end
    else
      TempString := TempString + Format(SFloatFormatString, ['-']) + #9;

    if not (FIsBlensOnly or FIsLittleBootstrapsTiming) then
      TempString := TempString + Format(SPercentString, ['-']);          // Data Coverage
    ExportList.Add(TempString);
  end;

  // Write out the information for the internal nodes
  for i := NoOfOtus to 2*NoOfOtus-2 do
  begin
    NodeLabel := GetInternalNodeLabel(MapNodeIndexToTreeDataIndex(FNodes[i].Index), FNodes[i]);
    TempString := MakePrettyTaxonNameString(NodeLabel, TaxonColumnWidth) + #9;
    TempString := TempString + Format(IdFormatString, [FNodes[i].index + 1]) + #9;
    TempString := TempString + Format(IdFormatString, [FNodes[i].des1.index+1]) + #9;
    TempString := TempString + Format(IdFormatString, [FNodes[i].des2.index+1]) + #9;

    if IsSamplingTime then
    begin
      if FNodes[i].flag then
      begin
        TempFloatStr := Trim(FormatDoubleSafe(FNodes[i].height, Precision, ValueDigits));
        TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // Reltime
        TempFloatStr := Trim(FormatDoubleSafe(sqrt(FNodes[i].vh), Precision, ValueDigits));
        TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // stderrs
        TempFloatStr := Trim(FormatDoubleSafe(FNodes[i].rate, Precision, ValueDigits));
        TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // Rate
        TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // DivTime
        TempString := TempString + Format(SDaysFormatString, ['-']) + #9; // Elapsed (Days)
        TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // LowerBound
        TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // UpperBound
        TempString := TempString + Format(SFloatFormatString, ['-']) + #9; // sample time
      end
      else
      begin
        TempFloatStr := Trim(FormatDoubleSafe(FNodes[i].height, Precision, ValueDigits));
        TempString := TempString + Format(SFloatFormatString, [TempFloatStr]) + #9; // Reltime
        TempFloatStr := Trim(FormatDoubleSafe(sqrt(FNodes[i].vh), Precision, ValueDigits));
        TempString := TempString + Format(SFloatFormatString, [TempFloatStr]) + #9; // stderrs
        TempFloatStr := Trim(FormatDoubleSafe(FNodes[i].rate, Precision, ValueDigits));
        TempString := TempString + Format(SFloatFormatString, [TempFloatStr]) + #9;
        TempFloatStr := RtdtFloatTimeToDateString(DivTime(i));
        TempString := TempString + Format(SFloatFormatString, [TempFloatStr]) + #9;
        TempFloatStr := Format(SDaysFormatString, [Format('%.0n', [ElapsedDaysForRtdtTime(i)*1.0])]);
        TempString := TempString + TempFloatStr + #9;
        TempFloatStr := RtdtFloatTimeToDateString(MinDivTime(i));
        TempString := TempString + Format(SFloatFormatString, [TempFloatStr]) + #9;
        TempFloatStr := RtdtFloatTimeToDateString(MaxDivTime(i));
        TempString := TempString + Format(SFloatFormatString, [TempFloatStr]) + #9;
        TempString := TempString + Format(SFloatFormatString, ['-']) + #9;
      end;
    end;
    if not (FIsBlensOnly or FIsLittleBootstrapsTiming) then
      TempString := TempString + Format(PercentString, [FNodes[i].DataCoverage*100]);
    ExportList.Add(TempString);
  end;
  ExportList.Add(EmptyStr);
  ExportList.Add(ResultsFootnote);
  GenerateSpreadsheetExport(NamesList, NodeLabels, SampleTimes);
end;

function TReltimeComputer.GetCaptionString: AnsiString;
begin
  Result := EmptyStr;
  Assert(False, 'not implemented');
end;

function TReltimeComputer.GetNode(Index: Integer): TReltimeTreeNode;
begin
  Result := nil;
  if (Index < 0) or (Index >= Length(FNodes)) then
    Exit;
  Result := FNodes[Index];
end;

function TReltimeComputer.GetNodeLabel(Calibrations: TCalibrations; NodeIndex: Integer): String;
var
  i: Integer;
begin
  Result := '-';
  for i := 0 to Calibrations.Count - 1 do
  begin
    if NodeIndex = Calibrations.GetCalibration(i).ReltimeTreeIndexBLens then
    begin
      Result := Calibrations.GetCalibration(i).NodeLabel;
      Exit;
    end;
  end;
end;

function TReltimeComputer.GetReltimes: ArrayOfExtended;
var
  i: Integer;
begin
  SetLength(Result, Length(FNodes));
  for i := 0 to Length(Result) - 1 do
    Result[i] := FNodes[i].height;
end;

function TReltimeComputer.GetSBL(ANode: TRelTimeTreeNode): Double;
begin
  Result := ANode.BLen;
  if ANode.Otu then
    Exit;

  Result := Result + GetSBL(ANode.Des1) + GetSBL(ANode.Des2);
end;

function TReltimeComputer.GetSibling(aNode: TRelTimeTreeNode): TRelTimeTreeNode;
begin
  Result := nil;
  if Assigned(aNode.anc) then
  begin
    if aNode = aNode.anc.des1 then
      Result := aNode.anc.des2
    else
      Result := aNode.anc.des1;
  end;
end;

function TReltimeComputer.GetSecondDescendents(aNode: TReltimeTreeNode): TRelTimeTreeNodeArray;
var
  desNode: TReltimeTreeNode;
  i: Integer;

  procedure AddNode(n: TReltimeTreeNode);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := n;
  end;

begin
  SetLength(Result, 0);
  if aNode.OTU then
   Exit;
  for i := 0 to NumNodes - 1 do
  begin
    desNode := FNodes[i];
    if IsSecondDescendent(aNode, desNode) then
      AddNode(desNode);
  end;
end;

function TReltimeComputer.GetThirdDescendents(aNode: TReltimeTreeNode): TRelTimeTreeNodeArray;
var
  desNode: TReltimeTreeNode;
  i: Integer;

  procedure AddNode(n: TReltimeTreeNode);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := n;
  end;

begin
  SetLength(Result, 0);
  if aNode.OTU then
   Exit;
  for i := 0 to NumNodes - 1 do
  begin
    desNode := FNodes[i];
    if IsThirdDescendent(aNode, desNode) then
      AddNode(desNode);
  end;
end;

function TReltimeComputer.IsSecondDescendent(Ancestor, Descendent: TReltimeTreeNode): Boolean;
begin
  Result := False;
  if Assigned(Descendent.anc) and Assigned(Descendent.anc.anc) then
    Result := (Descendent.anc.anc = Ancestor);
end;

function TReltimeComputer.IsThirdDescendent(Ancestor,
  Descendent: TReltimeTreeNode): Boolean;
begin
  Result := False;
  if Assigned(Descendent.anc) and Assigned(Descendent.anc.anc) and (Assigned(Descendent.anc.anc.anc)) then
    Result := (Descendent.anc.anc.anc = Ancestor);
end;

function TReltimeComputer.GetSiblingRates: TCorrTestDataList;
var
  aNode, aSibling: TReltimeTreeNode;
  i: Integer;
begin
  Result := TCorrTestDataList.Create;
  for i := 0 to NumNodes - 1 do
    FNodes[i].done := False;

  for i := 0 to NumNodes - 1 do
  begin
    if FNodes[i].done or FNodes[i].IsOutgroupMember or (FNodes[i].anc = nil) or (FNodes[i].anc.anc = nil) then
      continue;
    aNode := FNodes[i];
    aSibling := GetSibling(aNode);
    if Assigned(aSibling) and (not aSibling.IsOutgroupMember) then
    begin
      if aNode = aNode.anc.des1 then
        Result.Add(aNode.CorrTestRate, aSibling.CorrTestRate)
      else
        Result.Add(aSibling.CorrTestRate, aNode.CorrTestRate);
    end;
    aNode.done := True;
    aSibling.done := True;
  end;
end;

function TReltimeComputer.GetDirectDescendentRates: TCorrTestDataList;
var
  aNode, aChild: TReltimeTreeNode;
  i: Integer;
begin
  Result := TCorrTestDataList.Create;
  for i := 0 to NumNodes - 1 do
    FNodes[i].done := False;

  for i := 0 to NumNodes - 1 do
  begin
    if FNodes[i].done or FNodes[i].IsOutgroupMember or (FNodes[i].OTU) or (FNodes[i].anc = nil) or (FNodes[i].anc.anc = nil) then
      continue;
    if FNodes[i].BlenIsZero then
      continue;
    aNode := FNodes[i];
    aChild := aNode.des1;
    if Assigned(aChild) and (not aChild.IsOutgroupMember) and (not aChild.BlenIsZero) then
      Result.Add(aNode.CorrTestRate, aChild.CorrTestRate);
    //aChild.done := True;
    aChild := aNode.des2;
    if Assigned(aChild) and (not aChild.IsOutgroupMember) and (not aChild.BlenIsZero) then
      Result.Add(aNode.CorrTestRate, aChild.CorrTestRate);
    //aChild.done := True;
    //aNode.done := True;
  end;
end;

function TReltimeComputer.GetSecondDescendentRates: TCorrTestDataList;
var
  aNode, aDesc: TReltimeTreeNode;
  descendents: TReltimeTreeNodeArray;
  i, j: Integer;
begin
  Result := TCorrTestDataList.Create;
  for i := 0 to NumNodes - 1 do
    FNodes[i].done := False;

  for i := 0 to NumNodes - 1 do
  begin
    if FNodes[i].done or
       FNodes[i].IsOutgroupMember or
       (FNodes[i].OTU) or
       (FNodes[i].anc = nil) or
       (FNodes[i].anc.anc = nil) then
      continue;
    aNode := FNodes[i];
    if aNode.BLenIsZero then
      continue;
    descendents := GetSecondDescendents(aNode);

    if Length(descendents) > 0 then
      for j := 0 to Length(descendents) - 1 do
      begin
        aDesc := descendents[j];
        if (not aDesc.done) and (not aDesc.IsOutgroupMember) and (not aDesc.BlenIsZero) then
        begin
          Result.Add(aNode.CorrTestRate, aDesc.CorrTestRate);
          aDesc.Done := True;
        end;
      end;
    //aNode.done := True;
  end;
end;

function TReltimeComputer.GetThirdDescendentRates: TCorrTestDataList;
var
  aNode, aDesc: TReltimeTreeNode;
  descendents: TReltimeTreeNodeArray;
  i, j: Integer;
begin
  Result := TCorrTestDataList.Create;
  for i := 0 to NumNodes - 1 do
    FNodes[i].done := False;

  for i := 0 to NumNodes - 1 do
  begin
    if FNodes[i].done or
       FNodes[i].IsOutgroupMember or
       (FNodes[i].OTU) or
       (FNodes[i].anc = nil) or
       (FNodes[i].anc.anc = nil) then
      continue;
    aNode := FNodes[i];
    if aNode.BLenIsZero then
      continue;
    descendents := GetThirdDescendents(aNode);
    if Length(descendents) > 0 then
      for j := 0 to Length(descendents) - 1 do
      begin
        aDesc := descendents[j];
        if (not aDesc.done) and (not aDesc.IsOutgroupMember) and (not aDesc.BLenIsZero) then
        begin
          Result.Add(aNode.CorrTestRate, aDesc.CorrTestRate);
          aDesc.done := true;
        end;
      end;
    //aNode.done := True;
  end;
end;

function TReltimeComputer.HasOutgroup: Boolean;
var
  i: Integer;
begin
  Result := False;
  if Length(FNodes) > 0 then
  begin
    for i := 0 to FNoOfOtus - 1 do
    begin
      if FNodes[i].IsOutgroupMember then
      begin
        Result := True;
        break;
      end;
    end;
  end;
end;

function ComputeMinHeight(h,d: extended): extended;
begin
  result := h -((h+d) -sqrt((h+d)*(h+d) -2*h*d));
end;

function ComputeMaxHeight(h,d: extended): extended;
begin
  result := ComputeMinHeight(h,d) +2*d;
end;

procedure ConstrainMinMaxHeight(n: TRelTimeTreeNode);
begin
  if n.anc.anc <> nil then
    if n.anc.maxh+FP_CUTOFF > n.anc.height then
      if n.maxh > n.anc.maxh then
        n.maxh := n.anc.maxh;

  if n.OTU then exit;

  ConstrainMinMaxHeight(n.des1);
  ConstrainMinMaxHeight(n.des2);

  if n.minh < n.des1.minh then
    n.minh := n.des1.minh;
  if n.minh < n.des2.minh then
    n.minh := n.des2.minh;

end;

procedure TReltimeComputer.InitCorrTestTree;
var
  i: Integer;
  //{$IFDEF DEBUG}debug: Boolean = True;{$ENDIF}
  ancestorHeight: Extended;
  nodeHeight: Extended;
begin
  FCorrTestTree := TTreeData.Create(FSourceData.NoOfOTUs, True, False, False);
  FCorrTestTree.Assign(FSourceData);
  if FIsSamplingTime then
  begin
    for i := 0 to 2*NoOfOtus-2 do
    begin
      if Assigned(Node[i].anc) then
      begin
        ancestorHeight := Root.h0 - Node[i].anc.h0;
        nodeHeight := Root.h0 - Node[i].h0;
        FCorrTestTree.BLen[i] := nodeHeight - ancestorHeight;
      end
      else if Node[i] = Root then
        FCorrTestTree.Blen[i] := 0
      else
      begin
        Assert(False, 'missing ancestor for non-root node');
      end;
    end;
  end;
  //{$IFNDEF VISUAL_BUILD}
  //{$IFDEF DEBUG}
  //Debug := DebugOutputTreeToNewickFile(FCorrTestTree, FNamesList, NextAvailableFilenameNV('_dev_corrtest_input.nwk'), msg);
  //{$ENDIF}
  //{$ENDIF}
end;

function TReltimeComputer.GetIsStrictClockTree: Boolean;
begin
  Result := CompareValue(FStrictClockRate, 1.0, FP_CUTOFF) <> 0;
end;

function TReltimeComputer.ComputeHeights: TTreeData;

  procedure SetNewHeightDown(n: TRelTimeTreeNode; dh: extended);
  begin
    if n.anc.height -dh -n.h0 < FP_CUTOFF then exit;

    dh := (n.height -n.h0)/(n.anc.height -dh -n.h0)*dh;

    n.height := n.height +dh;

    if n.blen < FP_CUTOFF then
      n.rate := n.anc.rate
    else if n.anc.height-n.height < FP_CUTOFF then
      n.rate := 0
    else
      n.rate := n.blen/(n.anc.height-n.height);

    if n.OTU then exit;

    SetNewHeightDown(n.des1, dh);
    SetNewHeightDown(n.des2, dh);
  end;

  procedure ComputeHeightOfNode(n: TRelTimeTreeNode);
  var
    h,h1,h2,h0: extended;
  begin
    if not n.OTU then
    begin
      ComputeHeightOfNode(n.des1);
      ComputeHeightOfNode(n.des2);
    end;
    if n.OTU then
      n.height := n.h0
    else
    begin
      h1 := n.des1.height +n.des1.blen -n.des1.h0;
      h2 := n.des2.height +n.des2.blen -n.des2.h0;
      if (h1 > FP_CUTOFF) and (h2 > FP_CUTOFF) then
      begin
        if n.des1.h0 > n.des2.h0 then
        begin
          h0 := n.des1.h0 -n.des2.h0;
          h  := (-h0 +sqrt(h0*h0 +4*h1*h2))/2;
          h  := h +n.des1.h0;
        end
        else
        begin
          h0 := n.des2.h0 -n.des1.h0;
          h  := (-h0 +sqrt(h0*h0 +4*h1*h2))/2;
          h  := h +n.des2.h0;
        end;
        if (h1+n.des1.h0) > (h2+n.des2.h0) then
        begin
          if h1/(h-n.des1.h0) > sqrt(MaxRateRatio) then
            h := h1/sqrt(MaxRateRatio) +n.des1.h0;
        end
        else if (h2+n.des2.h0) > (h1+n.des1.h0) then
        begin
          if h2/(h-n.des2.h0) > sqrt(MaxRateRatio) then
            h := h2/sqrt(MaxRateRatio) +n.des2.h0;
        end;
      end
      else if h1 > FP_CUTOFF then
        h := max(h1/2+n.des1.h0, n.des2.h0)
      else if h2 > FP_CUTOFF then
        h := max(n.des1.h0, h2/2+n.des2.h0)
      else
        h := max(n.des1.h0, n.des2.h0);

      n.height := h;

      n.h1 := h1;
      n.h2 := h2;

      SetNewHeightDown(n.des1, h-(h1+n.des1.h0));
      SetNewHeightDown(n.des2, h-(h2+n.des2.h0));
    end;
  end;


  procedure ComputeRateOfNode(n: TRelTimeTreeNode);
  begin
    if (n.blen < FP_CUTOFF) or ((n.anc.height-n.height) < FP_CUTOFF) or n.flag then
      n.rate := n.anc.rate
    else
      n.rate := n.blen/(n.anc.height-n.height);

    if n.OTU then exit;

    ComputeRateOfNode(n.des1);
    ComputeRateOfNode(n.des2);
  end;

  procedure ComputeMinMaxHeight;
  var
    i: integer;
  begin
    for i := NoOfOtus to 2*NoOfOtus-2 do
    begin
      Node[i].maxh := ComputeMaxHeight((Node[i].height-Node[i].h0), Zvalue*sqrt(Node[i].vh)) +Node[i].h0;
      Node[i].minh := ComputeMinHeight((Node[i].height-Node[i].h0), Zvalue*sqrt(Node[i].vh)) +Node[i].h0;
    end;
    ConstrainMinMaxHeight(root.des1);
  end;

var
  b,h: extended;
  i: integer;
begin
  Result := nil;

  try
    RootTreeDataOnOutgroup(FSourceData);
    InitCorrTestTree;
    Assert(Assigned(Root.des1.des1), 'invalid ingroup after rooting');
    Result := TTreeData.Create(FSourceData.NoOfOTUs, True, False, False);
    Result.Assign(FSourceData);

    for i := 0 to 2 * NoOfOtus - 2 do
      FNodes[i].flag := false;

    FNumNodesProcessed := 0;
    for i := 0 to 2 * NoOfOtus - 2 do
      FNodes[i].Rate := 1;

    ComputeHeightOfNode(Root.des1);
    Root.des1.rate := 1;
    ComputeRateOfNode(Root.des1.des1);
    ComputeRateOfNode(Root.des1.des2);

    b := root.des1.blen+root.des2.blen;
    h := (root.des1.height +root.des2.height +b)/2;
    if abs(root.des1.height-root.des2.height) <= (root.des1.blen+root.des2.blen) then
    begin
      root.des1.blen := h -root.des1.height;
      root.des2.blen := h -root.des2.height ;
      root.height := h;
    end
    else
    begin
      if root.des1.height > root.des2.height then
      begin
        root.des1.blen := 0;
        root.des2.blen := b;
        root.height    := root.des1.height;
        SetNewHeightDown(root.des2, root.des1.height);
      end
      else
      begin
        root.des1.blen := b;
        root.des2.blen := 0;
        root.height    := root.des1.height+b;
        SetNewHeightDown(root.des2, root.des1.height+b);
      end;
    end;

    for i := 0 to 2*NoOfOtus-2 do
      Node[i].anchored := false;

    for i := 0 to NoOfOtus-1 do
      Node[i].flag := true;
    for i := NoOfOtus to 2*NoOfOtus-2 do
      Node[i].flag := false;

    ComputeHeightVar;
    CorrectShallowHeightVar;

    ComputeMinMaxHeight;

    Result.isSE := true;
    for i := 0 to 2*NoOfOtus-2 do
    begin
      if Node[i].anc = nil then
        continue;
      Result.BLen[i] := (FNodes[i].anc.height - FNodes[i].height);
      Result.SE[i]   := sqrt(FNodes[i].vh);
    end;
  except
    on E:Exception do
    begin
      FLog.Add('Error when computing node heights: ' + E.Message);
      if assigned(Result) then
      begin
        Result.Free;
        Result := nil;
      end;
    end;
  end;
end;

function TReltimeComputer.GetSpreadsheetExport: TExcelWrite;
begin
  Result := FExportBuilder.GenerateExport;
end;

function TReltimeComputer.AnchorClockTree(var minTime, maxTime: array of extended): TTreeData;

  procedure SetHa(n: TRelTimeTreeNode);
  var
    a1,a2: extended;
  begin
    if n.OTU then
      n.ha0 := n.h0
    else
    begin
      SetHa(n.des1);
      SetHa(n.des2);

      if n.des1.anchored then
        a1 := n.des1.minh
      else
        a1 := n.des1.ha0;
      if n.des2.anchored then
        a2 := n.des2.minh
      else
        a2 := n.des2.ha0;
      n.ha0 := max(a1, a2);
    end;
  end;

  procedure RecomputeNodeHeightDown(n: TRelTimeTreeNode; dh: extended);
  var
    h: extended;
  begin
    if abs(dh) < FP_CUTOFF then exit;
    if abs(n.anc.height -dh -n.ha0) < FP_CUTOFF then exit;
    if IsNan(dh) then
      raise Exception.Create('Invalid value (NaN) for dh');

    dh := (n.height -n.ha0)/(n.anc.height -dh -n.ha0)*dh;
    if IsNan(dh) then
      raise Exception.Create('Invalid value (NaN) for dh');

    if  (n.maxh > FP_CUTOFF) and (n.height +dh > n.maxh) then
      dh := n.maxh -n.height
    else if (n.height > FP_CUTOFF) and (n.height +dh < n.minh) then
      dh := n.minh -n.height;

    if abs(dh) < FP_CUTOFF then exit;

    h := n.height;
    n.height := n.height +dh;
    n.rate := n.rate*h/n.height;

    if not n.OTU then
    begin
      RecomputeNodeHeightDown(n.des1, dh);
      RecomputeNodeHeightDown(n.des2, dh);
    end;
  end;

  procedure ComputeHeightOfNode(n: TRelTimeTreeNode);
  var
    h,h0,h1,h2,h01,h02, dh: extended;
  begin
    if n.OTU then
    begin
      n.rate := 1;
      exit;
    end;

    ComputeHeightOfNode(n.des1);
    ComputeHeightOfNode(n.des2);

    if n.des1.anchored then
      h01 := n.des1.minh
    else
      h01 := n.des1.ha0;
    if n.des2.anchored then
      h02 := n.des2.minh
    else
      h02 := n.des2.ha0;

    if n.des1.rate < FP_CUTOFF then
      h1 := n.des1.height -h01
    else
      h1 := n.des1.height +n.des1.blen/n.des1.rate -h01;
    if n.des2.rate < FP_CUTOFF then
      h2 := n.des2.height -h02
    else
      h2 := n.des2.height +n.des2.blen/n.des2.rate -h02;

    if (h1 > FP_CUTOFF) and (h2 > FP_CUTOFF) and (max(h1,h2)/min(h1,h2) < MaxRateRatio) then
    begin
      if h01 > h02 then
      begin
        h := h01 -h02;
        h := (-h +sqrt(h*h +4*h1*h2))/2;
        h := h +h01;
      end
      else
      begin
        h := h02 -h01;
        h := (-h +sqrt(h*h +4*h1*h2))/2;
        h := h +h02;
      end;
    end
    else
    begin
      if h1 > h2 then
        h := max(h1/MaxRateRatio+h01, h02)
      else if h1 < h2 then
        h := max(h2/MaxRateRatio+h02, h01)
      else
        h := max(h1/MaxRateRatio+h01, h2/MaxRateRatio+h02);
      n.des1.flag := true;
    end;

    h0 := n.height;
    n.height := h;

    n.h1 := h1;
    n.h2 := h2;

    if abs(n.height-h0) > FP_CUTOFF then
    begin
      dh := n.height-(h1+h01);
      if IsNan(dh) then
        raise Exception.Create('Invalid value (NaN) for dh');
      RecomputeNodeHeightDown(n.des1, (n.height-(h1+h01)));
      dh := n.height-(h2+h02);
      if IsNan(dh) then
        raise Exception.Create('Invalid value (NaN) for dh');
      RecomputeNodeHeightDown(n.des2, (n.height-(h2+h02)));
    end;

    n.rate := sqrt(n.des1.rate*n.des2.rate);

    if (n.maxh > FP_CUTOFF) and (h > n.maxh) then
      n.height := n.maxh
    else if (n.minh > FP_CUTOFF) and (h < n.minh) then
      n.height := n.minh;

    if abs(n.height-h) > FP_CUTOFF then
    begin
      dh := n.height-h;
      if IsNan(dh) then
        raise Exception.Create('Invalid value (NaN) for dh');
      RecomputeNodeHeightDown(n.des1, (n.height-h));
      RecomputeNodeHeightDown(n.des2, (n.height-h));

      n.rate := n.rate*h/n.height;
    end;
  end;

  procedure ComputeRateOfNode(n: TRelTimeTreeNode);
  begin
    if (n.blen < FP_CUTOFF) or ((n.anc.height-n.height) < FP_CUTOFF) or n.flag then
      n.rate := n.anc.rate
    else
      n.rate := n.blen/(n.anc.height-n.height);

    if n.OTU then exit;

    ComputeRateOfNode(n.des1);
    ComputeRateOfNode(n.des2);
  end;

  procedure CalibrateRateOfNode(n: TRelTimeTreeNode; r: extended);
  begin
    n.rate   := n.rate/r;
    n.height := n.height*r;
    n.maxh   := n.maxh*r;
    n.minh   := n.minh*r;
    n.h0     := n.h0*r;
    n.ha0    := n.ha0*r;

    if not n.OTU then
    begin
      CalibrateRateOfNode(n.des1, r);
      CalibrateRateOfNode(n.des2, r);
    end;
  end;

  procedure CalibrateRate;
  begin
    if CompareValue(Root.des1.rate, 0.0, FP_CUTOFF) <> 0 then
    begin
      FMinTimeFactor := FMinTimeFactor/Root.des1.rate;
      FMaxTimeFactor := FMaxTimeFactor/Root.des1.rate;
      FTimeFactor    := FTimeFactor/Root.des1.rate;
      CalibrateRateOfNode(Root.des1, Root.des1.rate);
    end;
  end;

  procedure ComputeNodeHeight;
  begin
    SetHa(Root.des1);
    ComputeHeightOfNode(Root.des1);
    ComputeRateOfNode(Root.des1.des1);
    ComputeRateOfNode(Root.des1.des2);

    root.des1.rate := sqrt(Root.des1.des1.rate*Root.des1.des2.rate);
    CalibrateRate;

    if root.height  < root.des1.height then
      root.height := root.des1.height +root.des1.blen/root.des1.rate;
  end;

  function InitMinMaxTime: boolean;

    procedure InitMinMaxTimeOfNode(n: TRelTimeTreeNode);
    begin
      if n.OTU then exit;
      if n.anc <> nil then
        if (maxTime[n.anc.Index] > FP_CUTOFF) and (maxTime[n.Index] < FP_CUTOFF) then
        begin
          maxTime[n.Index] := maxTime[n.anc.Index];
        end;
//      if n.anc <> nil then
//        if (maxTime[n.anc.Index] > FP_CUTOFF) and (maxTime[n.Index] > maxTime[n.anc.Index]) then
//          maxTime[n.Index] := maxTime[n.anc.Index];

      InitMinMaxTimeOfNode(n.des1);
      InitMinMaxTimeOfNode(n.des2);
      if minTime[n.Index] < minTime[n.des1.Index] then
        minTime[n.Index] := minTime[n.des1.Index];
      if minTime[n.Index] < minTime[n.des2.Index] then
        minTime[n.Index] := minTime[n.des2.Index];

      if ((n.des1.blen < FP_CUTOFF) and (maxTime[n.des1.index] > FP_CUTOFF)) and
         ((n.des2.blen < FP_CUTOFF) and (maxTime[n.des2.index] > FP_CUTOFF)) then
        maxTime[n.index] := max(minTime[n.index], max(maxTime[n.des1.index], maxTime[n.des2.index]))
      else if (n.des1.blen < FP_CUTOFF) and (maxTime[n.des1.index] > FP_CUTOFF) then
        maxTime[n.index] := max(minTime[n.index], maxTime[n.des1.index])
      else if (n.des2.blen < FP_CUTOFF) and (maxTime[n.des2.index] > FP_CUTOFF) then
        maxTime[n.index] := max(minTime[n.index], maxTime[n.des2.index]);
    end;

  var
    i: integer;
  begin
    InitMinMaxTimeOfNode(Root);
    maxTime[Root.index] := 0.0; { to ensure it does not propagate above the ingroup root}
    result := true;
    for i := FNoOfOtus to 2*FNoOfOtus-2 do
      if (maxTime[i] > FP_CUTOFF) and (maxTime[i] < minTime[i]) then
      begin
        result := false;
        break;
      end;
  end;

  procedure ComputeMinMaxHeight;
  var
    i: integer;
  begin
    for i := 0 to NoOfOtus-1 do
    begin
      Node[i].maxh := Node[i].height;
      Node[i].minh := Node[i].height;
    end;
    for i := NoOfOtus to 2*NoOfOtus-2 do
      if Node[i].anchored then
      begin
        Node[i].maxh := min(Node[i].maxh, ComputeMaxHeight((Node[i].height-Node[i].h0), Zvalue*sqrt(Node[i].vh))+Node[i].h0);
        Node[i].minh := max(Node[i].minh, ComputeMinHeight((Node[i].height-Node[i].h0), Zvalue*sqrt(Node[i].vh))+Node[i].h0);
      end
      else
      begin
        Node[i].maxh := ComputeMaxHeight((Node[i].height-Node[i].h0), Zvalue*sqrt(Node[i].vh))+Node[i].h0;
        Node[i].minh := ComputeMinHeight((Node[i].height-Node[i].h0), Zvalue*sqrt(Node[i].vh))+Node[i].h0;
      end;

    ConstrainMinMaxHeight(root.des1);
  end;

var
  maxRate, minRate: array of extended;
  r0,r1: extended;
  i,n: integer;
  flag: boolean;
begin
  SetMinTimes(MinTime);
  SetMaxTimes(MaxTime);
  Result := nil;
  FIsSamplingTime := false;

  for i := 0 to NoOfOtus-1 do
    Node[i].anchored := false;
  for i := NoOfOtus to 2*NoOfOtus-2 do
    if (minTime[i] > FP_CUTOFF) or (maxTime[i] > FP_CUTOFF) then
      Node[i].anchored := true
    else
      Node[i].anchored := false;

  Assert(FPropagateConstraints, 'Propagation of constraints is now a required feature');
  if FPropagateConstraints then
  begin

    flag := InitMinMaxTime;
    if not flag then
    begin
      raise Exception.Create('Invalid divergence time constraints');
      exit;
    end;
  end;

  if IsDeveloper then
    FOriginalReltimes := GetReltimes;

  Result := TTreeData.Create(FSourceData.NoOfOTUs, True, False, False);
  Result.Assign(FSourceData);

  setlength(minRate, 2*FNoOfOtus-1);
  setlength(maxRate, 2*FNoOfOtus-1);
  for i := FNoOfOtus to 2*FNoOfOtus-2 do
  begin
    minRate[i] := 0;
    maxRate[i] := 0;
  end;
  for i := 0 to 2*FNoOfOtus-2 do
  begin
    Node[i].minh := 0;
    Node[i].maxh := 0;
  end;

  for i := FNoOfOtus to 2*FNoOfOtus-2 do
    if Node[i].height > FP_CUTOFF then
    begin
      minRate[i] := minTime[i]/Node[i].height;
      maxRate[i] := maxTime[i]/Node[i].height;
    end;

  n := FNoOfOtus;
  while (n < 2*FNoOfOtus-2) and (not Node[n].anchored) do
    inc(n);

  r0 := minRate[n];
  r1 := maxRate[n];
  for i := n+1 to 2*FNoOfOtus-2 do
    if (Node[i].anchored) and (Node[i].height > FP_CUTOFF) then
    begin
      if r0 < minRate[i] then
        r0 := minRate[i];
      if r1 < FP_CUTOFF then
        r1 := maxRate[i]
      else if (maxRate[i] > FP_CUTOFF) and (r1 > maxRate[i]) then
        r1 := maxRate[i];
    end;

  if (r1 < FP_CUTOFF) then
    raise Exception.Create('Invalid divergence time constraints');

  if r1 >= r0 then
  begin
    FTimeFactor := (r0+r1)/2;
    FMinTimeFactor := r0;
    FMaxTimeFactor := r1;
  end
  else
  begin
    FTimeFactor := r1;
    FMinTimeFactor := FTimeFactor;
    FMaxTimeFactor := FTimeFactor;
  end;

  for i := FNoOfOtus to 2*FNoOfOtus-2 do
  begin
    Node[i].minh := minTime[i]/FTimeFactor;
    Node[i].maxh := maxTime[i]/FTimeFactor;
  end;

  if r1 < r0 then
  begin
    ComputeNodeHeight;

    for i := FNoOfOtus to 2*FNoOfOtus-2 do
      if Node[i].height > FP_CUTOFF then
      begin
        minRate[i] := minTime[i]/Node[i].height;
        maxRate[i] := maxTime[i]/Node[i].height;
      end;
    r0 := minRate[n];
    r1 := maxRate[n];
    for i := n+1 to 2*FNoOfOtus-2 do
      if (Node[i].anchored) and (Node[i].height > FP_CUTOFF) then
      begin
        if r0 < minRate[i] then
          r0 := minRate[i];
        if r1 < FP_CUTOFF then
          r1 := maxRate[i]
        else if (maxRate[i] > FP_CUTOFF) and (r1 > maxRate[i]) then
          r1 := maxRate[i];
      end;
    FMinTimeFactor := r0;
    FMaxTimeFactor := r1;
    FTimeFactor := (r0+r1)/2;

    for i := FNoOfOtus to 2*FNoOfOtus-2 do
    begin
      Node[i].minh := minTime[i]/FTimeFactor;
      Node[i].maxh := maxTime[i]/FTimeFactor;
    end;
  end;

  ComputeHeightVar;
  CorrectShallowHeightVar;

  ComputeMinMaxHeight;


  Result.isSE := true;
  for i := 0 to 2*FNoOfOtus-2 do
  begin
    if Node[i].anc = nil then continue;
    Result.BLen[i] := (Node[i].anc.height -Node[i].height);
    Result.SE[i]   := sqrt(Node[i].vh);
  end;

  setlength(minRate, 0);
  setlength(maxRate, 0);
end;

procedure TReltimeComputer.ComputeHeightVar;
var
  n: integer;
  sn,vr0,vr1,sr1: extended;
  x,x2: array of extended;

  procedure SortX(L, R: integer);
  var
    i,j: integer;
    xx,tmp: extended;
  begin
    xx := x[(L+R) div 2];
    i := L-1;
    j := R+1;
    repeat
      repeat
        inc(i);
      until xx <= x[i];
      repeat
        dec(j);
      until x[j] <= xx;
      tmp  := x[i];
      x[i] := x[j];
      x[j] := tmp;
    until i >= j;
    x[j] := x[i];
    x[i] := tmp;
    if L < i-1 then SortX(L, i-1);
    if j+1 < R then SortX(j+1, R);
  end;

  procedure CountNoOfBranches(node: TRelTimeTreeNode);
  begin
    if not node.OTU then
    begin
      CountNoOfBranches(node.des1);
      CountNoOfBranches(node.des2);

      if (node.des1.blen > FP_CUTOFF) and (node.des2.blen > FP_CUTOFF) and
         (node.des1.rate < 10) and (node.des2.rate < 10)
      then
        inc(n, 2);
    end;
  end;

  procedure ComputeVarOfSB(node: TRelTimeTreeNode);
  begin
    if not node.OTU then
    begin
      ComputeVarOfSB(node.des1);
      ComputeVarOfSB(node.des2);

      if (node.des1.blen > FP_CUTOFF) and (node.des2.blen > FP_CUTOFF) and
         (node.des1.rate < 10) and (node.des2.rate < 10)
      then
      begin
        vr0 := vr0 +node.des1.vr;
        vr1 := vr1 +node.des1.rate*node.des1.rate;
        sr1 := sr1 +node.des1.rate;

        x[n] := node.des1.rate;

        sn  := sn +1;
        inc(n);

        vr0 := vr0 +node.des2.vr;
        vr1 := vr1 +node.des2.rate*node.des2.rate;
        sr1 := sr1 +node.des2.rate;

        x[n] := node.des2.rate;

        sn  := sn +1;
        inc(n);
      end;
    end;

  end;

  procedure ComputeVh(node: TRelTimeTreeNode);
  var
     vb1,vb2: extended;
  begin
    if (node.des1.rate < FP_CUTOFF) or (node.des1.blen < FP_CUTOFF) then
      vb1 := 0
    else
      vb1 := (node.des1.vb +node.des1.vr)/node.des1.rate/node.des1.rate;

    if (node.des2.rate < FP_CUTOFF) or (node.des2.blen < FP_CUTOFF) then
      vb2 := 0
    else
      vb2 := (node.des2.vb +node.des2.vr)/node.des2.rate/node.des2.rate;

    node.vh1 := node.des1.vh +vb1;
    node.vh2 := node.des2.vh +vb2;

    node.vh := (node.vh1 +node.vh2)/4;

  end;

  procedure ComputeHeightVarOfNodeUp(node: TRelTimeTreeNode);
  begin
    if node.OTU then exit;

    ComputeHeightVarOfNodeUp(node.des1);
    ComputeHeightVarOfNodeUp(node.des2);

    ComputeVh(node);
  end;

  procedure ComputeHeightVarOfNodeDown(node: TRelTimeTreeNode);

    procedure ComputeVh0(node: TRelTimeTreeNode);
    var
       vb,r,h,h0,r1,r2,b: extended;
    begin
      h0 := node.anc.height -node.ha0;
      h  := node.height -node.ha0;
      b  := h0 -h;
      vb := (node.vb +node.vr)/node.rate/node.rate;
      r  := h*h/h0/h0;
      if r > 1 then r := 1;


      if node = node.anc.des1 then
      begin
        r1 := node.rate;
        r2 := node.anc.des2.rate;
        node.vh0 := node.anc.vh0*r;
        node.vh0 := node.vh0 +(node.anc.vh2 +vb*r2/r1)*r/4;

        node.vh := node.vh*r2/r1*(h*h +4*h*b +4*b*b)/h0/h0/4;
      end
      else
      begin
        r1 := node.anc.des1.rate;
        r2 := node.rate;
        node.vh0 := node.anc.vh0*r;
        node.vh0 := node.vh0 +(node.anc.vh1 +vb*r1/r2)*r/4;

        node.vh := node.vh*r1/r2*(h*h +4*h*b +4*b*b)/h0/h0/4;
      end;
    end;

  begin
    if node.OTU or node.flag then exit;
    if (node.height-node.ha0) < FP_CUTOFF then exit;

    ComputeVh0(node);

    ComputeHeightVarOfNodeDown(node.des1);
    ComputeHeightVarOfNodeDown(node.des2);
  end;

  procedure ComputeHeightVarOfNodeAnchoredUp(node: TRelTimeTreeNode);

    procedure RecomputeVhOfNodeDown(node: TRelTimeTreeNode; h0: extended);
    var
      vb,r0,r1: extended;
    begin
      if node.OTU or node.flag then exit;
      if (node.anc.height-node.ha0) < FP_CUTOFF then exit;

      vb := (node.vb +node.vr)/node.rate/node.rate;

      r0 := (h0-node.height)/(h0-node.ha0);
      r1 := (node.height-node.ha0)/(node.anc.height-node.ha0);
      if r0 > 1 then r0 := 1;
      if r1 > 1 then r1 := 1;
      r0 := r0*r0;
      r1 := r1*r1;

      if node = node.anc.des1 then
      begin
        node.vh0 := node.anc.vh0*r1;
        node.vh0 := node.vh0 +node.anc.vh2*r1/4;
      end
      else
      begin
        node.vh0 := node.anc.vh0*r1;
        node.vh0 := node.vh0 +node.anc.vh1*r1/4;
      end;

      node.vh  := node.vh*r0;
      node.vh1 := node.vh1*r0;
      node.vh2 := node.vh2*r0;

      RecomputeVhOfNodeDown(node.des1, h0);
      RecomputeVhOfNodeDown(node.des2, h0);
    end;

  var
    v0,v1,v2: extended;
  begin
    if node.OTU then exit;

    ComputeHeightVarOfNodeAnchoredUp(node.des1);
    ComputeHeightVarOfNodeAnchoredUp(node.des2);

    v0 := node.vh;
    v1 := node.vh1;
    v2 := node.vh2;
    ComputeVh(node);

    if node.flag then
    begin
      node.vh  := v0;
      node.vh1 := v1;
      node.vh2 := v2;
      RecomputeVhOfNodeDown(node.des1, node.height);
      RecomputeVhOfNodeDown(node.des2, node.height);
    end;
  end;

  procedure ComputeVROfNode(node: TRelTimeTreeNode);
  var
    h0,h1,h2,r1,r2: extended;
  begin
    if node.OTU then exit;

    ComputeVROfNode(node.des1);
    ComputeVROfNode(node.des2);

    node.vr := 0;
    if (node.height-node.h0) > FP_CUTOFF then
    begin
      h0 := node.height;
      r1 := node.des1.rate;
      r2 := node.des2.rate;
      h1 := h0*r1;
      h2 := h0*r2;

      node.des1.vr := node.vh1/h0/h0 +(node.vh2+node.vh0)/h0/h0/4;
      node.des2.vr := node.vh2/h0/h0 +(node.vh1+node.vh0)/h0/h0/4;

    end;
  end;

  procedure RecomputeRateOfNode(n: TRelTimeTreeNode; r: extended);
  begin

    n.rate   := n.rate/r;
    n.height := n.height*r;

    if n.OTU then exit;

    RecomputeRateOfNode(n.des1, r);
    RecomputeRateOfNode(n.des2, r);
  end;

var
  i,n0,n1: integer;
  v0,v1,VRate: extended;
  aflag: boolean;
begin
  for i := NoOfOtus to 2*FNoOfOtus-2 do
    Node[i].flag := false;
  for i := 0 to 2*FNoOfOtus-2 do
    Node[i].vr     := 0;

  RecomputeRateOfNode(Root.des1, Root.des1.rate);

  VRate := 0;
  root.vh0 := 0;
  ComputeHeightVarOfNodeUp(root.des1);
  ComputeHeightVarOfNodeDown(root.des1);

  ComputeVROfNode(root.des1);

  n   := 0;
  CountNoOfBranches(root.des1);
  setlength(x, n);
  setlength(x2, n);

  n   := 0;
  sn  := 0;
  vr0 := 0;
  vr1 := 0;
  sr1 := 0;

  ComputeVarOfSB(root.des1);
  if n > 0 then
  begin
    vr0 := vr0/sn;

    sr1 := sr1/sn;
    vr1 := (vr1/sn -sr1*sr1)/sr1/sr1;

    if n >= 4 then
    begin
      SortX(0,n-1);
      n0 := ceil(n/4);
      n1 := n-ceil(n/4)-1;
      sr1 := 0;
      for i := n0 to n1 do
        sr1 := sr1 +x[i];
      vr1 := 0;
      for i := n0 to n1 do
        vr1 := vr1 +x[i]*x[i];

      if n div 4 = 0 then
      begin
//        sr1 := (x[n div 2] +x[(n div 2)-1])/2;    // median
        vr1 := ((x[n-(n div 4)] +x[n-(n div 4)-1])/2 - (x[n div 4] +x[(n div 4)-1])/2)/2/0.6755;
      end
      else if n div 4 = 1 then
      begin
//        sr1 := x[n div 2];                        // median
        sr1 := sr1 +(x[n0-1]+x[n1+1])*3/4;
        vr1 := ((x[n-(n div 4)] +3*x[n-(n div 4)-1])/4 - (x[n div 4] +3*x[(n div 4)-1])/4)/2/0.6755;
      end
      else if n div 4 = 2 then
      begin
//        sr1 := (x[n div 2] +x[(n div 2)-1])/2;    // median
        sr1 := sr1 +(x[n0-1]+x[n1+1])/2;
        vr1 := ((x[n-(n div 4)-1] - x[n div 4])/2)/2/0.6755;
      end
      else // if n div 4 = 3 then
      begin
//        sr1 := x[n div 2];                        // median
        sr1 := sr1 +(x[n0-1]+x[n1+1])/4;
        vr1 := ((3*x[n-(n div 4)-1] +x[n-(n div 4)-2])/4 - (3*x[n div 4] +x[(n div 4)+1])/4)/2/0.6755;
      end;

      sr1 := sr1/(n/2);
      vr1 := vr1*vr1/sr1/sr1;
    end;

      if vr1 > vr0 then
      begin
        VRate := (vr1 - vr0);
        for i := 0 to 2*FNoOfOtus-2 do
          Node[i].vr := VRate*Node[i].blen*Node[i].blen;
      end
      else
        for i := 0 to 2*FNoOfOtus-2 do
          Node[i].vr := 0;
  end;
  ComputeHeightVarOfNodeUp(root.des1);
  ComputeHeightVarOfNodeDown(root.des1);

  aflag := false;
  for i := FNoOfOtus to 2*FNoOfOtus-2 do
    if Node[i].anchored then
    begin
      v0 := Node[i].vh+Node[i].vh0;
      v1 := (Node[i].maxh-Node[i].minh)*(Node[i].maxh-Node[i].minh)/Zvalue/Zvalue/4;
      if v1 < v0 then
      begin
        Node[i].vh  := Node[i].vh*v1/v0;
        Node[i].vh0 := Node[i].vh0*v1/v0;
        Node[i].vh1 := Node[i].vh1*v1/v0;
        Node[i].vh2 := Node[i].vh2*v1/v0;
        Node[i].flag := true;
        aflag := true;
      end;
    end;

  if aflag then
  begin
    ComputeHeightVarOfNodeAnchoredUp(root.des1);
    ComputeHeightVarOfNodeDown(root.des1);
  end;

  for i := FNoOfOtus to 2*FNoOfOtus-2 do
    if Node[i] <> root.des1 then
      Node[i].vh := node[i].vh +Node[i].vh0;

  if not root.des1.flag then
  begin
    v0 := root.des1.des1.vh +root.des1.des2.vh;
    v1 := (sqrt(root.des1.des1.vb)+sqrt(root.des1.des2.vb))*(sqrt(root.des1.des1.vb)+sqrt(root.des1.des2.vb))
         +Vrate*(root.des1.des1.blen+root.des1.des2.blen)*(root.des1.des1.blen+root.des1.des2.blen);
    root.des1.vh := (v0 +v1)/4;
  end;

end;

procedure TReltimeComputer.CorrectShallowHeightVar;
var
  n:TRelTimeTreeNode = nil;
  xx,xy,mx,my,h,v,a,b,mh,hh,r: extended;
  i,nn: integer;
begin
  FlagOutgroupCluster; {DON'T USE FlagOutgroupNodes here! It does not work when some taxa in the outgroup have not been marked as outgroup.}
  nn := 0;
  mx := 0;
  my := 0;
  xx := 0;
  xy := 0;
  for i := FNoOfOtus to 2*FNoOfOtus-2 do
  begin
    if Node[i].flag then continue;
    if Node[i] = root.des1 then continue;

    h := Node[i].height-Node[i].h0;
    if h*h > FP_CUTOFF then
    begin
      mx := mx +h;
      my := my +(h+Zvalue*sqrt(Node[i].vh));
      xx := xx +h*h;
      xy := xy +h*(h+Zvalue*sqrt(Node[i].vh));
      inc(nn);
    end;
  end;
  if nn = 0 then
    nn := 1;
  if CompareValue(xx -mx*mx/nn, 0, FP_CUTOFF) <> 0 then
    a := (xy -mx*my/nn)/(xx -mx*mx/nn)
  else
    a := 1;
  b := my/nn -a*mx/nn;
  Assert(not IsNan(a), 'IsNan(a) in TReltimeComputer.CorrectShallowHeightVar');
  Assert(not IsNan(b), 'IsNan(b) in TReltimeComputer.CorrectShallowHeightVar');

  mh := root.des1.height-root.des1.h0;
  for i := FNoOfOtus to 2*FNoOfOtus-2 do
  begin
    h := Node[i].height-Node[i].h0;
    if h < mh then
      if h+Zvalue*sqrt(Node[i].vh) > a*h+b then
        mh := h;
  end;

  nn := 0;
  mx := 0;
  my := 0;
  xx := 0;
  xy := 0;
  for i := FNoOfOtus to 2*FNoOfOtus-2 do
  begin
    if Node[i].flag then continue;
    if Node[i] = root.des1 then continue;

    h := Node[i].height-Node[i].h0;
    mx := mx +h;
    my := my +(h+Zvalue*sqrt(Node[i].vh));
    xx := xx +h*h;
    xy := xy +h*(h+Zvalue*sqrt(Node[i].vh));
    inc(nn);
  end;
  if nn = 0 then
    nn := 1;
  if CompareValue(xx -mx*mx/nn, 0, FP_CUTOFF) <> 0 then
    a := (xy -mx*my/nn)/(xx -mx*mx/nn)
  else
    a := 1;
  b := my/nn -a*mx/nn;
  Assert(not IsNan(a), 'IsNan(a) in TReltimeComputer.CorrectShallowHeightVar');
  Assert(not IsNan(b), 'IsNan(b) in TReltimeComputer.CorrectShallowHeightVar');

  for i := FNoOfOtus to 2*FNoOfOtus-2 do
  begin
    if Node[i].flag then continue;
    if Node[i] = root.des1 then continue;

    h := Node[i].height-Node[i].h0;
    if h < mh then
    begin
      r  := 0;
      nn := 0;
      n := Node[i];
      repeat
        Assert(n <> root, 'invalid operation on root node');
        n := n.anc;
        Assert(Assigned(n), 'accessing nil node in TReltimeComputer');
        hh := n.height-n.h0;
        if not (hh < mh) then
        begin
          r := r +ComputeMaxHeight(hh, Zvalue*sqrt(n.vh))/(a*hh+b);
          inc (nn);
        end;
      until n.anchored or (n = root.des1 );
      if nn = 0 then
        r := 1.0
      else
        r := r/nn;
{
      n := Node[i];
      while (n.height-n.h0 < mh) and (not n.anchored) and (n <> root.des1 ) do
        n := n.anc;
      hh := n.height-n.h0;
      r := ComputeMaxHeight(hh, Zvalue*sqrt(n.vh))/(a*hh+b);
}
      //if (a*h + b) <> 0 then
      //begin
        v := ((r*(a*h+b)/2)-h*h/(2*r*(a*h+b)))*((r*(a*h+b)/2)-h*h/(2*r*(a*h+b)))/Zvalue/Zvalue;
        Assert((not IsNan((sqrt(v)+sqrt(Node[i].vh))*(sqrt(v)+sqrt(Node[i].vh))/4)) and (not IsInfinite((sqrt(v)+sqrt(Node[i].vh))*(sqrt(v)+sqrt(Node[i].vh))/4)), 'division by zero?');
        Node[i].vh := (sqrt(v)+sqrt(Node[i].vh))*(sqrt(v)+sqrt(Node[i].vh))/4;
      //end;
    end;
  end;
end;

procedure TReltimeComputer.InitFromSessionFile(TreeData: TTreeData);
begin
  FSourceData := TTreeData.Create(TreeData.NoOfOTUs, True, False, False);
  FSourceData.Assign(TreeData);
  InitNodeData(FSourceData);
  FIsSamplingTime := false;
end;

procedure TReltimeComputer.InitParentChildRelationships(TreeData: TTreeData);
begin
  FSourceData := TTreeData.Create(TreeData.NoOfOTUs, True, False, False);
  FSourceData.Assign(TreeData);
  InitNodeRelationships(FSourceData);
end;

procedure TReltimeComputer.InitSpreadsheetExportFromSessionFile( aNames: TStringList; aLabels: TStringList);
begin
  if IsSamplingTime then
    FExportBuilder.Initialize(self, FNodes, aNames, aLabels, FIsBlensOnly or FIsLittleBootstrapsTiming, FOrigSamplingTimes)
  else if Length(FMinTimes) > 0 then
    FExportBuilder.Initialize(self, FNodes, aNames, aLabels, FIsBlensOnly or FIsLittleBootstrapsTiming, FOrigMinTimes, FOrigMaxTimes)
  else
    FExportBuilder.Initialize(self, FNodes, aNames, aLabels, FIsBlensOnly or FIsLittleBootstrapsTiming);
end;

procedure TReltimeComputer.InitNodeData(Tree: TTreeData);
var
  i: Integer;
  n: Integer;
  EstProgress: Integer;
begin
  n := Tree.NoOfOTUs;

  SetLength(FNodes, 2 * n - 1);
  FNoOfOtus := n;

  for i := 0 to Length(FNodes) - 1 do
    FNodes[i] := TReltimeTreeNode.Create(i);

  ResetNodes;

  IsVarBlen := Tree.isSE;
  for i := 0 to n - 2 do
  begin
    FNodes[n+i].DataCoverage := Tree.DataCoverage[i];
    FNodes[n+i].Des1 := FNodes[Tree[i].Des1];
    FNodes[n+i].Des2 := FNodes[Tree[i].Des2];
    FNodes[tree[i].Des1].Anc := FNodes[n+i];
    FNodes[tree[i].Des2].Anc := FNodes[n+i];
    FNodes[n+i].Des1.Blen := Tree.BLen[Tree[i].Des1];
    if FNodes[n + i].Des1.Blen < 0 then
      FNodes[n+i].Des1.Blen := 0;
    FNodes[n+i].Des2.Blen := Tree.BLen[Tree[i].Des2];
    if FNodes[n+i].Des2.Blen < 0 then
      FNodes[n+i].Des2.Blen := 0;
    if IsVarBlen then
    begin
      FNodes[n+i].Des1.vb := Tree.SE[Tree[i].Des1]*Tree.SE[Tree[i].Des1];
      FNodes[n+i].Des2.vb := Tree.SE[Tree[i].Des2]*Tree.SE[Tree[i].Des2];
    end;

    if (i mod 50) = 0 then
    begin
      EstProgress := trunc(i / (n - 2) * 100);
      if Assigned(ProgressProc) then
        ProgressProc(EstProgress);
    end;
  end;

  for i := 0 to FNoOfOtus - 1 do
    FNodes[i].IsOutgroupMember := Tree.IsOutgroupMember[i];

  for i := 0 to 2 * FNoOfOtus - 2 do
    if FNodes[i].Anc = nil then
    begin
      FRoot := FNodes[i];
      break;
    end;

  FRoot.Des2.Blen := FRoot.Des1.Blen + FRoot.Des2.Blen;
  FRoot.Des1.Blen := 0;
  FRoot.Des1.vb := FRoot.Des1.vb + FRoot.Des2.vb;
  FRoot.Des2.vb := 0;
  if FNeedsRootByOutgroup then
  begin
    Assert(HasOutgroup, 'Needed outgroup information is missing in TReltimeComputer.InitNodeData');
    ChangeRootByOutgroup;
  end;
  SetSizeDepth;
end;

procedure TReltimeComputer.InitNodeRelationships(Tree: TTreeData);
var
  i: Integer;
  n: Integer;
  EstProgress: Integer;
begin
  n := Tree.NoOfOTUs;
  FNoOfOtus := n;
  IsVarBlen := Tree.isSE;
  if Length(FNodes) = 0 then
    InitNodeData(Tree);
  for i := 0 to n - 2 do
  begin
    FNodes[n+i].DataCoverage := Tree.DataCoverage[i];
    FNodes[n+i].Des1 := FNodes[Tree[i].Des1];
    FNodes[n+i].Des2 := FNodes[Tree[i].Des2];
    FNodes[tree[i].Des1].Anc := FNodes[n+i];
    FNodes[tree[i].Des2].Anc := FNodes[n+i];
    //FNodes[n+i].Des1.Blen := Tree.BLen[Tree[i].Des1];
    //if FNodes[n + i].Des1.Blen < 0 then
    //  FNodes[n+i].Des1.Blen := 0;
    //FNodes[n+i].Des2.Blen := Tree.BLen[Tree[i].Des2];
    //if FNodes[n+i].Des2.Blen < 0 then
    //  FNodes[n+i].Des2.Blen := 0;
    //if IsVarBlen then
    //begin
    //  FNodes[n+i].Des1.vb := Tree.SE[Tree[i].Des1]*Tree.SE[Tree[i].Des1];
    //  FNodes[n+i].Des2.vb := Tree.SE[Tree[i].Des2]*Tree.SE[Tree[i].Des2];
    //end;

    if (i mod 50) = 0 then
    begin
      EstProgress := trunc(i / (n - 2) * 100);
      if Assigned(ProgressProc) then
        ProgressProc(EstProgress);
    end;
  end;

  for i := 0 to FNoOfOtus - 1 do
    FNodes[i].IsOutgroupMember := Tree.IsOutgroupMember[i];

  for i := 0 to 2 * FNoOfOtus - 2 do
    if FNodes[i].Anc = nil then
    begin
      FRoot := FNodes[i];
      break;
    end;

  FRoot.Des2.Blen := FRoot.Des1.Blen + FRoot.Des2.Blen;
  FRoot.Des1.Blen := 0;
  FRoot.Des1.vb := FRoot.Des1.vb + FRoot.Des2.vb;
  FRoot.Des2.vb := 0;
  Assert(HasOutgroup, 'Outgroup information is missing in TReltimeComputer.InitNodeData');
  ChangeRootByOutgroup;
  SetSizeDepth;
end;

procedure TReltimeComputer.SetPropagateConstraints(AValue: Boolean);
begin
  if FPropagateConstraints=AValue then Exit;
    FPropagateConstraints:=AValue;
end;

procedure TReltimeComputer.SetSizeDepth;

  procedure SetSizeDepthOfNode(node: TRelTimeTreeNode);
  begin
    if node.OTU then
      exit;
    SetSizeDepthOfNode(node.des1);
    SetSizeDepthOfNode(node.des2);
    node.size  := node.des1.size + node.des2.size;
    node.depth := max(node.des1.depth, node.des2.depth) + 1;
  end;

begin
  SetSizeDepthOfNode(FRoot);
end;

procedure TReltimeComputer.ReadFromFile(var aFile: File; const sessionVersion: Integer);
var
  i: Integer = -1;
  n: Integer = -1;
  c: AnsiChar = #0;
  aRootIndex: Integer = -1;
  b: Boolean = False;
begin
  if sessionVersion >= 1203 then
  begin
    BlockRead(aFile, aRootIndex, SizeOf(Integer));
    BlockRead(aFile, FLatestSampleDate, SizeOf(Extended));
    BlockRead(aFile, FIsLittleBootstrapsTiming, SizeOf(Boolean));
    BlockRead(aFile, FPropagateConstraints, SizeOf(Boolean));
    BlockRead(aFile, FNumNodesProcessed, SizeOf(Integer));
    BlockRead(aFile, FStrictClockRate, SizeOf(Double));
    BlockRead(aFile, n, SizeOf(Integer));
    SetLength(FOrigSamplingTimes, n);
    if n > 0 then
      for i := Low(FOrigSamplingTimes) to High(FOrigSamplingTimes) do
        BlockRead(aFile, FOrigSamplingTimes[i], SizeOf(Extended));

    BlockRead(aFile, n, SizeOf(Integer));
    SetLength(FOriginalReltimes, n);
    if n > 0 then
      for i := Low(FOriginalReltimes) to High(FOriginalReltimes) do
        BlockRead(aFile, FOriginalReltimes[i], SizeOf(Extended));
  end;

  BlockRead(aFile, IsVarBlen, SizeOf(Boolean));
  BlockRead(aFile, Zvalue, SizeOf(Extended));
  BlockRead(aFile, FIsBLensOnly, SizeOf(Boolean));
  BlockRead(aFile, FNoOfOtus, SizeOf(Integer));
  BlockRead(aFile, FTimeFactor, SizeOf(Extended));
  BlockRead(aFile, FMinTimeFactor, SizeOf(Extended));
  BlockRead(aFile, FMaxTimeFactor, SizeOf(Extended));
  BlockRead(aFile, FLatestTime, SizeOf(Extended));
  BlockRead(aFile, FIsSamplingTime, SizeOf(Boolean));
  BlockRead(aFile, FMaxRateRatio, SizeOf(Extended));

  BlockRead(aFile, n, SizeOf(Integer));
  SetLength(FMinTimes, n);
  if n > 0 then
    for i := 0 to n - 1 do
      BlockRead(aFile, FMinTimes[i], SizeOf(Extended));
  BlockRead(aFile, n, SizeOf(Integer));
  SetLength(FMaxTimes, n);
  if n > 0 then
    for i := 0 to n - 1 do
      BlockRead(aFile, FMaxTimes[i], SizeOf(Extended));

  BlockRead(aFile, n, SizeOf(Integer));
  SetLength(FOrigMinTimes, n);
  if n > 0 then
    for i := 0 to n - 1 do
      BlockRead(aFile, FOrigMinTimes[i], SizeOf(Extended));
  BlockRead(aFile, n, SizeOf(Integer));
  SetLength(FOrigMaxTimes, n);
  if n > 0 then
    for i := 0 to n - 1 do
      BlockRead(aFile, FOrigMaxTimes[i], SizeOf(Extended));

  BlockRead(aFile, n, SizeOf(Integer));
  FVarianceEstimationMethod := TReltimeVarianceMethod(n);

  BlockRead(aFile, n, SizeOf(Integer));
  SetLength(FCaptionString, n);
  if n > 0 then
    for i := 1 to n do
    begin
      BlockRead(aFile, c, SizeOf(AnsiChar));
      FCaptionString[i] := c;
    end;

  BlockRead(aFile, n, SizeOf(Integer));
  SetLength(FNodes, n);
  if n > 0 then
  begin
    for i := 0 to n - 1 do
    begin
      FNodes[i] := TRelTimeTreeNode.Create(i);
      FNodes[i].ReadFromFile(aFile);
    end;
    if aRootIndex >= 0 then
      FRoot := FNodes[aRootIndex];
  end;

  if SessionVersion >= 1004 then
    BlockRead(aFile, FStrictClockRate, SizeOf(FStrictClockRate));

  if SessionVersion >= 1203 then
  begin
    BlockRead(aFile, b, SizeOf(Boolean));
    if b then
    begin
      FSourceData := TTreeData.Create(FNoOfOtus, True, True, False);
      FSourceData.ReadFromFile(aFile, sessionVersion);
    end;
    BlockRead(aFile, b, SizeOf(Boolean));
    if b then
    begin
      FCorrTestTree := TTreeData.Create(FNoOfOtus, True, True, False);
      FCorrTestTree.ReadFromFile(aFile, sessionVersion);
    end;
  end;
end;

procedure TReltimeComputer.ResetNodes;
var
  i: Integer;
begin
  for i := 0 to Length(FNodes) - 1 do
  begin
    FNodes[i].Index  := i;
    FNodes[i].Blen   := 0.0;
    FNodes[i].Rate   := 0.0;
    FNodes[i].Height := 0.0;
    FNodes[i].Size   := 1;
    FNodes[i].Depth  := 0;
    FNodes[i].Anc    := nil;
    FNodes[i].Des1   := nil;
    FNodes[i].Des2   := nil;
  end;
end;

procedure TReltimeComputer.SetStrictClockRate(AValue: Double);
begin
  if FStrictClockRate=AValue then Exit;
  FStrictClockRate:=AValue;
end;

procedure TReltimeComputer.ResetTreeData(Tree: TTreeData);
var
  i: integer;
begin

  Tree.isBLen := true;
  Tree.isSE   := false;
  for i := FNoOfOtus to 2 * FNoOfOtus - 2 do
  begin
    Tree.DataCoverage[i-FNoOfOtus] := FNodes[i].DataCoverage;
    Tree.NodeArray[i - FNoOfOtus].des1 := FNodes[i].des1.Index;
    Tree.NodeArray[i - FNoOfOtus].des2 := FNodes[i].des2.Index;
    Tree.BLenArray[FNodes[i].des1.Index] := FNodes[i].des1.blen;
    Tree.BLenArray[FNodes[i].des2.Index] := FNodes[i].des2.blen;
  end;
  Tree.Value := GetSBL(FRoot);
end;

procedure TReltimeComputer.SetLatestTime(const Value: Extended);
begin
  FLatestTime := Value;
end;

function TReltimeComputer.GetInternalNodeLabel(TreeDataIndex: Integer; n: TRelTimeTreeNode): String;
begin
  if Assigned(FNodeLabels) and (TreeDataIndex < FNodeLabels.Count) and (FNodeLabels[TreeDataIndex] <> EmptyStr) then
  begin
    if n.IsOutgroupMember or n.flag then
    begin
      if (Trim(FNodeLabels[TreeDataindex]) <> EmptyStr) and (Trim(FNodeLabels[TreeDataIndex]) <> '-') then
        Result := FNodeLabels[TreeDataIndex] + OUTGROUP_MARKER
      else
        Result := Trim(OUTGROUP_MARKER);
    end
    else
      Result := FNodeLabels[TreeDataIndex];
  end
  else
  begin
    if n.IsOutgroupMember or n.flag then
      Result := Trim(OUTGROUP_MARKER)
    else
      Result := '-';
  end;
end;

function TReltimeComputer.ResultsFootnote: String;
begin
  Result := Format('Note: %s indicates members of the outgroup. The Reltime method does not estimate times for outgroup nodes.', [Trim(OUTGROUP_MARKER)]);
end;

function TReltimeComputer.DebugNodeInfoToStringList(n: TRelTimeTreeNode): TStringList;
begin
  Result := TStringList.Create;
  Result.Add(n.DebugString);
  Result.Add(EmptyStr);
  Result.Add(DebugInfo(n.index));
end;

{$IFDEF DEBUG}
function TReltimeComputer.DebugNodeInfoToFile(n: TRelTimeTreeNode): Boolean;
{$IFNDEF VISUAL_BUILD}
var
  index: Integer;
  aList: TStringList = nil;
  filename: String;
{$ENDIF}
begin
  Result := True;
    {$IFNDEF VISUAL_BUILD}
    index := n.index;
    aList := DebugNodeInfoToStringList(Node[index]);
    aList.Add(Format('MinDivTime    = %.8f', [MinDivTime(index)]));
    aList.Add(Format('DivTime       = %.8f', [DivTime(index)]));
    aList.Add(Format('MaxDivTime    = %.8f', [MaxDivTime(index)]));
    aList.Add(Format('MinNodeHeight = %.8f', [MinNodeHeight(index)]));
    aList.Add(Format('NodeHeight    = %.8f', [NodeHeight(index)]));
    aList.Add(Format('MaxNodeHeight = %.8f', [MaxNodeHeight(index)]));
    filename := NextAvailableFilenameNV('_node_info.txt');
    aList.SaveToFile(filename);
    Result := FileExists(filename);
    aList.Free;
    {$ENDIF}
end;
{$ENDIF}

function TReltimeComputer.DebugInfo(index: Integer): String;
var
  aList: TStringList = nil;
  i: Integer;
begin
  try
    aList := TStringList.Create;
    //aList.Add(Format('MinTime               = %.8f', [MinDivTime(index)]));
    //aList.Add(Format('DivTime               = %.8f', [DivTime(index)]));
    //aList.Add(Format('MaxTime               = %.8f', [MaxDivTime(index)]));
    aList.Add(Format('FIsBlensOnly          = %s', [BoolToStr(FIsBlensOnly, True)]));
    aList.Add(Format('FIsLittleBootstraps   = %s', [BoolToStr(FIsLittleBootstrapsTiming, True)]));
    aList.Add(Format('FPropagateConstraints = %s', [BoolToStr(FPropagateConstraints, True)]));
    aList.Add(Format('FNoOfOtus             = %d', [FNoOfOtus]));
    aList.Add(Format('FRoot                 = %d', [FRoot.index]));
    aList.Add(Format('FCaptionString        = %s', [FCaptionString]));
    aList.Add(Format('FTimeFactor           = %.8f', [FTimeFactor]));

    aList.Add(Format('FMinTimeFactor        = %.8f', [FMinTimeFactor]));
    aList.Add(Format('FMaxTimeFactor        = %.8f', [FMaxTimeFactor]));
    aList.Add(Format('FLatestTime           = %.8f', [FLatestTime]));
    aList.Add(Format('FIsSamplingTime       = %s', [BoolToStr(FIsSamplingTime, True)]));
    aList.Add(Format('FMaxRateRatio         = %.8f', [FMaxRateRatio]));
    if Length(FMinTimes) > 0 then
    begin
      aList.Add(Format('%-6s %-8s %-8s', ['index', 'mintime', 'maxtime']));
      for i := 0 to Length(FMinTimeS) - 1 do
        aList.Add(Format('%-6d %-8.6f %-8.6f', [i, FMinTimes[i], FMaxTimes[i]]));
    end;
    Result := aList.Text
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TReltimeComputer.DebugPropogatedConstraints(minTimes, maxTimes: array of extended): TStringList;
var
  i: Integer;
  n: TRelTimeTreeNode;

  function DebugString(aNode: TRelTimeTreeNode): String;
  begin
    if assigned(aNode.anc) then
      Result := Format('%d: %.2f - %.2f', [aNode.anc.index+1, minTimes[aNode.anc.index], maxTimes[aNode.anc.index]])
    else
      Result := Format('root: %.2f - %.2f', [0.0, 0.0]);
    Result := Result + #9 + Format('%d: %.2f - %.2f', [aNode.index+1, minTimes[aNode.index], maxTimes[aNode.index]]);
    if assigned(n.des1) then
      Result := Result + #9 + Format('%d: %.2f - %.2f', [aNode.des1.index+1, minTimes[aNode.des1.index], maxTimes[aNode.des1.index]]);
    if assigned(n.des2) then
      Result := Result + #9 + Format('%d: %.2f - %.2f', [aNode.des2.index+1, minTimes[aNode.des2.index], maxTimes[aNode.des2.index]]);
    if n.anchored then
      Result := Result + #9 + 'anchored = true'
    else
      Result := Result + #9 + 'anchored = false';
  end;

begin
  Result := TStringList.Create;
  for i := FNoOfOtus to 2*FNoOfOtus - 2 do
  begin
    n := Node[i];
    Result.Add(DebugString(n));
  end;
end;
{$IFDEF DEBUG}
function TReltimeComputer.DebugAllNodeInfo: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  Result.Add(TRelTimeTreeNode.DebugHeaderStringOneLine);
  for i := Low(FNodes) to High(FNodes) do
    Result.Add(FNodes[i].DebugStringOneLine);
end;

function TReltimeComputer.DebugAllNodeInfoToFile(filename: String): Boolean;
var
  aList: TStringList = nil;
begin
  try
    aList := DebugAllNodeInfo;
    aList.SaveToFile(filename);
    Result := FileExists(filename);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;
 {$ENDIF}

function TReltimeComputer.DumpMinMaxTimes(minTimes,maxTimes: array of Extended; filename: String): Boolean;
var
  aList: TStringList = nil;
  i: Integer;
begin
  try
    try
      aList := TStringList.Create;
      if Length(minTimes) > 0 then
        for i := Low(minTimes) to High(minTimes) do
          aList.Add(Format('%d, %.4f, %.4f', [i + 1, minTimes[i], maxTimes[i]]));
      aList.SaveToFile(filename);
    except
      Result := False;
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TReltimeComputer.FindMrca(aNode: TRelTimeTreeNode; taxon1: Integer; taxon2: Integer): Integer;
var
  lcaLeft: Integer = -1;
  lcaRight: Integer = -1;
  leafNodes: TList = nil;
  aMrca: TRelTimeTreeNode = nil;
begin
  Result := -1;
  try
    leafNodes := TList.Create;
    leafNodes.Add(FNodes[taxon1]);
    leafNodes.Add(FNodes[taxon2]);
    aMrca := Mrca(leafNodes);
    Result := aMrca.index;
  finally
    if Assigned(leafNodes) then
      leafNodes.Free;
  end;
  exit;

  Result := -1;
  if not Assigned(aNode) then
    Exit;

  if (aNode.index = taxon1) or (aNode.index = taxon2) then
  begin
    Result := aNode.index;
    Exit;
  end;

  if not aNode.OTU then
  begin
    if ((aNode.des1.index = taxon1) or (aNode.des1.index = taxon2)) and ((aNode.des2.index = taxon2) or(aNode.des2.index = taxon1)) then
    begin
      Result := aNode.index;
      Exit;
    end;
    lcaLeft := FindMrca(aNode.des1, taxon1, taxon2);
    lcaRight := FindMrca(aNode.des2, taxon1, taxon2);
    if (lcaLeft >= 0) and (lcaRight >= 0) then
      Result := aNode.index
    else if lcaLeft >= 0 then
      Result := FindMrca(aNode.des1, taxon1, taxon2)
    else if lcaRight >= 0 then
      Result := FindMrca(aNode.des2, taxon1, taxon2);
  end;
end;

function TReltimeComputer.Mrca(leafNodes: TList): TRelTimeTreeNode;

  function GetKeysCount(aNode: TReltimeTreeNode; keys: TList; matchingNodes: Integer; var ancestors: TList): Integer;
  begin
    if not Assigned(aNode) then
      Exit(0);
    matchingNodes := matchingNodes + GetKeysCount(aNode.des1, keys, matchingNodes, ancestors) + GetKeysCount(aNode.des2, keys, matchingNodes, ancestors);

    if keys.IndexOf(aNode) >= 0 then
      inc(matchingNodes);
    if matchingNodes = keys.Count then
      ancestors.Add(aNode);
    Result := matchingNodes;
  end;

  function LcaOfNodes(aNode: TRelTimeTreeNode; keys: TList): TRelTimeTreeNode;
  var
    ancestors: TList = nil;
    matchingNodes: Integer = 0;
  begin
     try
       ancestors := TList.Create;
       GetKeysCount(aNode, keys, matchingNodes, ancestors);
       if ancestors.Count > 0 then
         Result := ancestors[0]
       else
         Result := nil;
     finally
       if Assigned(ancestors) then
         ancestors.Free;
     end;
  end;

begin
  Result := LcaOfNodes(FRoot, leafNodes);
end;

function TReltimeComputer.CheckCalibrationIds(var aCalibs: TCalibrations): Boolean;
var
  i: Integer = -1;
  c: TCalibrationTime = nil;
  mrca: Integer = -1;
begin
  if aCalibs.Count = 0 then
    Exit(False);
  Result := True;
  for i := 0 to aCalibs.Count - 1 do
  begin
    c := aCalibs.GetCalibration(i);
    if c.IsSelected then
    begin
      if c is TIngroupRootCalibration then
        mrca := Root.des1.index
      else
        mrca := FindMrca(Root.des1, c.NodeIDA, c.NodeIDB);
      Assert(mrca >= NoOfOtus, Format('invalid mrca found - there are %d taxa but mrca is %d', [NoOfOtus, mrca]));
      c.NodeID := mrca;
      c.ReltimeTreeIndexBLens := mrca;
    end;
  end;
end;

procedure TReltimeComputer.PrepareIngroupCalibration(const aCalib: TCalibrationTime);
var
  i: Integer = -1;
  ingroupRoot: Integer = -1;
begin
  SetLength(FMinTimes, 2*NoOfOtus - 1);
  SetLength(FMaxTimes, 2*NoOfOtus - 1);

  for i := Low(FMinTimes) to High(FMinTimes) do
  begin
    FMinTimes[i] := 0.0;
    FMaxTimes[i] := 0.0;
  end;
  Assert(Assigned(FRoot));
  ingroupRoot := FRoot.des1.index;
  FMinTimes[ingroupRoot] := aCalib.MinTime;
  FMaxTimes[ingroupRoot] := aCalib.MaxTime;
end;

function TReltimeComputer.RtdtFloatTimeToDateString(aFloatTime: Extended): String;
begin
  try
    Result := FloatTimeToDateString(aFloatTime, DEFAULT_DATE_FORMAT);
  except
    on E:Exception do
      Result := '?';
  end;
end;

procedure TReltimeComputer.SetMaxTimes(aMaxTimes: array of Extended);
var
  i: Integer;
begin
  SetLength(FOrigMaxTimes, Length(aMaxTimes));
  for i := 0 to Length(aMaxTimes) - 1 do
    FOrigMaxTimes[i] := aMaxTimes[i];
end;

function TReltimeComputer.DivTime(index: integer): extended;
begin
  if Abs(FTimeFactor) < FP_CUTOFF then
    Result := FNodes[index].height/FStrictClockRate
  else
    Result := FNodes[index].height*FTimeFactor/FStrictClockRate + FLatestTime;
end;

procedure TReltimeComputer.SetMinTimes(aMinTimes: array of Extended);
var
  i: Integer;
begin
  SetLength(FOrigMinTimes, Length(aMinTimes));
  for i := 0 to Length(aMinTimes) - 1 do
    FOrigMinTimes[i] := aMinTimes[i];
end;

procedure TReltimeComputer.SetNodeData(TreeData: TTreeData);
begin
  ClearNodes;
  InitNodeData(TreeData);
end;

function TReltimeComputer.ValidateConstraints(tree: TTreeData; minTime, maxTime: array of extended): Boolean;
var
  TempMin, TempMax: array of extended;
  i: Integer;

    function InitMinMaxTime: boolean;

    procedure InitMinMaxTimeOfNode(n: TRelTimeTreeNode);
    begin
      if n.OTU then exit;

      if n.anc <> nil then
        if (TempMax[n.anc.index] > 0.000000000001) and (TempMax[n.index] > TempMax[n.anc.index]) then
          TempMax[n.index] := TempMax[n.anc.index];

      InitMinMaxTimeOfNode(n.des1);
      InitMinMaxTimeOfNode(n.des2);

      if TempMin[n.index] < TempMin[n.des1.index] then
        TempMin[n.index] := TempMin[n.des1.index];
      if TempMin[n.index] < TempMin[n.des2.index] then
        TempMin[n.index] := TempMin[n.des2.index];
    end;

  var
    i: integer;
  begin
    InitMinMaxTimeOfNode(Root);

    result := true;
    for i := NoOfOtus to 2*FNoOfOtus-2 do
      if (TempMax[i] > 0.000000000001) and (TempMax[i] < TempMin[i]) then
      begin
        result := false;
        break;
      end;
  end;

begin
  SetLength(TempMin, Length(minTime));
  SetLength(TempMax, Length(maxtime));
  for i := Low(minTime) to High(minTime) do
  begin
    TempMin[i] := minTime[i];
    TempMax[i] := maxTime[i];
  end;

  InitNodeData(tree);
  result := InitMinMaxTime;
//  SetLength(TempMin, 0); not needed because dynamic arrays are garbage collected
//  SetLength(TempMax, 0);
end;

procedure TReltimeComputer.WriteToFile(var aFile: File);
var
  i, n: Integer;
  c: AnsiChar = #0;
  b: Boolean = False;
begin
  if Assigned(FRoot) then
    BlockWrite(aFile, FRoot.index, SizeOf(Integer))
  else
  begin
    i := -1;
    BlockWrite(aFile, i, SizeOf(Integer));
  end;
  BlockWrite(aFile, FLatestSampleDate, SizeOf(Extended));
  BlockWrite(aFile, FIsLittleBootstrapsTiming, SizeOf(Boolean));
  BlockWrite(aFile, FPropagateConstraints, SizeOf(Boolean));

  BlockWrite(aFile, FNumNodesProcessed, SizeOf(Integer));
  BlockWrite(aFile, FStrictClockRate, SizeOf(Double));
  n := Length(FOrigSamplingTimes);
  BlockWrite(aFile, n, SizeOf(Integer));
  if n > 0 then
    for i := Low(FOrigSamplingTimes) to High(FOrigSamplingTimes) do
      BlockWrite(aFile, FOrigSamplingTimes[i], SizeOf(Extended));

  n := Length(FOriginalReltimes);
  BlockWrite(aFile, n, SizeOf(Integer));
  if n > 0 then
    for i := Low(FOriginalReltimes) to High(FOriginalReltimes) do
      BlockWrite(aFile, FOriginalReltimes[i], SizeOf(Extended));


  BlockWrite(aFile, IsVarBlen, SizeOf(Boolean));
  BlockWrite(aFile, Zvalue, SizeOf(Extended));
  BlockWrite(aFile, FIsBLensOnly, SizeOf(Boolean));
  BlockWrite(aFile, FNoOfOtus, SizeOf(Integer));
  BlockWrite(aFile, FTimeFactor, SizeOf(Extended));
  BlockWrite(aFile, FMinTimeFactor, SizeOf(Extended));
  BlockWrite(aFile, FMaxTimeFactor, SizeOf(Extended));
  BlockWrite(aFile, FLatestTime, SizeOf(Extended));
  BlockWrite(aFile, FIsSamplingTime, SizeOf(Boolean));
  BlockWrite(aFile, FMaxRateRatio, SizeOf(Extended));

  n := Length(FMinTimes);
  BlockWrite(aFile, n, SizeOf(Integer));
  if n > 0 then
    for i := 0 to n - 1 do
      BlockWrite(aFile, FMinTimes[i], SizeOf(Extended));
  n := Length(FMaxTimes);
  BlockWrite(aFile, n, SizeOf(Integer));
  if n > 0 then
    for i := 0 to n - 1 do
      BlockWrite(aFile, FMaxTimes[i], SizeOf(Extended));

  n := Length(FOrigMinTimes);
  BlockWrite(aFile, n, SizeOf(Integer));
  if n > 0 then
    for i := 0 to n - 1 do
      BlockWrite(aFile, FOrigMinTimes[i], SizeOf(Extended));
  n := Length(FOrigMaxTimes);
  BlockWrite(aFile, n, SizeOf(Integer));
  if n > 0 then
    for i := 0 to n - 1 do
      BlockWrite(aFile, FOrigMaxTimes[i], SizeOf(Extended));
  n := Ord(FVarianceEstimationMethod);
  BlockWrite(aFile, n, SizeOf(Integer));
  n := Length(FCaptionString);
  BlockWrite(aFile, n, SizeOf(Integer));
  if n > 0 then
    for i := 1 to n do
    begin
      c := FCaptionString[i];
      BlockWrite(aFile, c, SizeOf(AnsiChar));
    end;
  n := Length(FNodes);
  BlockWrite(aFile, n, SizeOf(Integer));
  if n > 0 then
    for i := 0 to n - 1 do
      FNodes[i].WriteToFile(aFile);
  BlockWrite(aFile, FStrictClockRate, SizeOf(FStrictClockRate));

  b := Assigned(FSourceData);
  BlockWrite(aFile, b, SizeOf(Boolean));
  if b then
    FSourceData.WriteToFile(aFile);
  b := Assigned(FCorrTestTree);
  BlockWrite(aFile, b, SizeOf(Boolean));
  if b then
    FCorrTestTree.WriteToFile(aFile);
end;

function TReltimeComputer.GetReltimesArray: ArrayOfExtended;
var
  i: Integer;
begin
  SetLength(Result, Length(FNodes));
  if Length(Result) > 0 then
    for i := 0 to Length(Result) - 1 do
      Result[i] := Node[i].height;
end;

function TReltimeComputer.NodeHeight(index: integer): extended;
begin
  result := Node[index].height;
end;

function TReltimeComputer.NumNodes: Integer;
begin
  Result := Length(FNodes);
end;

function TReltimeComputer.GetNodeLabels: TStringList;
begin
  Result := TStringList.Create;
  if Assigned(FNodeLabels) then
    Result.Assign(FNodeLabels);
end;

function TReltimeComputer.ReleaseControlOfSpreadsheetExport: TReltimeExportBuilder;
begin
  if not FExportBuilder.IsInitialized then
    raise Exception.Create('Spreadsheet export has not yet been initialized');
  Result := FExportBuilder;
  FExportBuilder := nil;
end;

procedure TReltimeComputer.AcceptControlOfSpreadsheetExport(aExport: TReltimeExportBuilder);
begin
  if Assigned(FExportBuilder) then
    FExportBuilder.Free;
  FExportBuilder := aExport;
end;

function TReltimeComputer.MaxNodeHeight(index: integer): extended;
begin
  result := Node[index].maxh;
end;

function TReltimeComputer.MinNodeHeight(index: integer): extended;
begin
  result := Node[index].minh;
end;

function TReltimeComputer.MaxDivTime(index: integer): extended;
begin
  if abs(TimeFactor) < FP_CUTOFF then
    result := Node[index].maxh/FStrictClockRate
  else if TimeFactor < 0 then
    result := Node[index].minh*TimeFactor/FStrictClockRate + LatestTime
  else
    result := Node[index].maxh*TimeFactor/FStrictClockRate + LatestTime;
end;

function TReltimeComputer.MinDivTime(index: integer): extended;
begin
  if abs(TimeFactor) < FP_CUTOFF then
    result := Node[index].minh/FStrictClockRate
  else if TimeFactor < 0 then
    result := Node[index].maxh*TimeFactor/FStrictClockRate + LatestTime
  else
    result := Node[index].minh*TimeFactor/FStrictClockRate + LatestTime;
end;

function TReltimeComputer.MaxDivTimeRaw(index: Integer): extended;
begin
  if abs(TimeFactor) < FP_CUTOFF then
    result := Node[index].maxh/FStrictClockRate
  else if TimeFactor < 0 then
    result := (ComputeMinHeight((Node[index].height-Node[index].h0), Zvalue*sqrt(Node[index].vh)) +Node[index].h0)*MinTimeFactor/FStrictClockRate + LatestTime
  else
    result := (ComputeMaxHeight((Node[index].height-Node[index].h0), Zvalue*sqrt(Node[index].vh)) +Node[index].h0)*MaxTimeFactor/FStrictClockRate + LatestTime;
end;

function TReltimeComputer.MinDivTimeRaw(index: Integer): extended;
begin
  if abs(TimeFactor) < FP_CUTOFF then
    result := Node[index].minh/FStrictClockRate
  else if TimeFactor < 0 then
    result := (ComputeMaxHeight((Node[index].height-Node[index].h0), Zvalue*sqrt(Node[index].vh)) +Node[index].h0)*MaxTimeFactor/FStrictClockRate + LatestTime
  else
    result := (ComputeMinHeight((Node[index].height-Node[index].h0), Zvalue*sqrt(Node[index].vh)) +Node[index].h0)*MinTimeFactor/FStrictClockRate + LatestTime;
end;

procedure TReltimeComputer.SetVarianceEstimationMethod(const Value: TReltimeVarianceMethod);
begin
  FVarianceEstimationMethod := Value;
end;

function TReltimeComputer.TimeTreeToNewickString(OtuNames: TStringList;UseTimeFactor: Boolean): String;
const
  Precision = 7;
  ValueDigits = 16;

  function RtdtTimeBlen(aNode: TReltimeTreeNode): Double;
  begin
    Result := 0;
    if UseTimeFactor then
    begin
      if Assigned(aNode.anc) then
        Result := DivTime(aNode.index) - DivTime(aNode.anc.index);
    end
    else
    begin
      if Assigned(aNode.anc) then
        Result := NodeHeight(aNode.index) - NodeHeight(aNode.anc.index);
    end;
  end;

  function NewickStringRecursive(ANode: TRelTimeTreeNode): String;
  var
    aBlen, aBlen2: Extended;
    nLabel: String = '';
  begin
    // Base case - leaf nodes return their names
    if ANode.OTU then
    begin
      Result := TTreeList.CleanOtuNameForExport(OtuNames[ANode.Index]);
      exit;
    end;

    if ANode.des1.OTU and ANode.des2.Otu then
    begin
      Result := '(' + NewickStringRecursive(ANode.des1) + ':' + Trim(FormatDoubleSafe(ANode.height, Precision, ValueDigits)) + ',' + NewickStringRecursive(ANode.des2) + ':' + Trim(FormatDoubleSafe(ANode.height, Precision, ValueDigits)) + ')'
    end
    else if ANode.des1.OTU then
    begin
      if IsZero(ANode.height - ANode.des2.height, FP_CUTOFF) then
        ablen := 0
      else
        ablen := ANode.height - ANode.des2.height;
      Result := '(' + NewickStringRecursive(ANode.des1) + ':' + Trim(FormatDoubleSafe(ANode.height, Precision, ValueDigits)) + ',' + NewickStringRecursive(ANode.des2) + ':' + Trim(FormatDoubleSafe(ablen, Precision, ValueDigits)) + ')'
    end
    else if ANode.des2.OTU then
    begin
      if IsZero(ANode.height - ANode.des1.height, FP_CUTOFF) then
        ablen := 0
      else
        ablen := ANode.height - ANode.des1.height;
      Result := '(' + NewickStringRecursive(ANode.des1) + ':' + Trim(FormatDoubleSafe(ablen, Precision, ValueDigits)) + ',' + NewickStringRecursive(ANode.des2) + ':' + Trim(FormatDoubleSafe(ANode.height, Precision, ValueDigits)) + ')'
    end
    else
    begin
      if IsZero(ANode.height - ANode.des1.height, FP_CUTOFF) then
        ablen := 0
      else
        ablen := ANode.height - ANode.des1.height;
      if IsZero(ANode.height - ANode.des2.height, FP_CUTOFF) then
        ablen2 := 0
      else
        ablen2 := ANode.height - ANode.des2.height;
      Result := '(' + NewickStringRecursive(ANode.des1) + ':' + Trim(FormatDoubleSafe(ablen, Precision, ValueDigits)) + ',' + NewickStringRecursive(ANode.des2) + ':' + Trim(FormatDoubleSafe(ablen2, Precision, ValueDigits)) + ')';
    end;
    nLabel := GetInternalNodeLabel(MapNodeIndexToTreeDataIndex(ANode.Index), ANode);
    if (nLabel <> EmptyStr) and (nLabel <> '-') then
      Result := Format('%s%s%s%s', [Result, #39, nLabel, #39]);
  end;

  function NewickStringRecursiveWithTimeFactor(ANode: TRelTimeTreeNode): String;
  var
    TF: extended;
    aBlen, aBlen2: Extended;
    nLabel: String = '';
  begin
    TF := abs(TimeFactor);
    // Base case - leaf nodes return their names
    if ANode.OTU then
    begin
      Result := TTreeList.CleanOtuNameForExport(OtuNames[ANode.Index]);
      exit;
    end;

    if ANode.des1.OTU and ANode.des2.Otu then
    begin
      Result := '(' + NewickStringRecursiveWithTimeFactor(ANode.des1) + ':' + Trim(FormatDoubleSafe(ANode.height*TF, Precision, ValueDigits)) + ',' + NewickStringRecursiveWithTimeFactor(ANode.des2) + ':' + Trim(FormatDoubleSafe(ANode.height*TF, Precision, ValueDigits)) + ')'
    end
    else if ANode.des1.OTU then
    begin
      if IsZero(ANode.height - ANode.des2.height, FP_CUTOFF) then
        aBlen := 0
      else
        aBlen := ANode.height - ANode.des2.height;
      Result := '(' + NewickStringRecursiveWithTimeFactor(ANode.des1) + ':' + Trim(FormatDoubleSafe(ANode.height*TF, Precision, ValueDigits)) + ',' + NewickStringRecursiveWithTimeFactor(ANode.des2) + ':' + Trim(FormatDoubleSafe((aBlen)*TF, Precision, ValueDigits)) + ')'
    end
    else if ANode.des2.OTU then
    begin
      if IsZero(ANode.height - ANode.des1.height, FP_CUTOFF) then
        aBlen := 0
      else
        aBlen := ANode.height - ANode.des1.height;
      Result := '(' + NewickStringRecursiveWithTimeFactor(ANode.des1) + ':' + Trim(FormatDoubleSafe((aBLen)*TF, Precision, ValueDigits)) + ',' + NewickStringRecursiveWithTimeFactor(ANode.des2) + ':' + Trim(FormatDoubleSafe(ANode.height*TF, Precision, ValueDigits)) + ')'
    end
    else
    begin
      if IsZero(ANode.height - ANode.des1.height, FP_CUTOFF) then
        aBLen := 0
      else
        aBlen := ANode.height - ANode.des1.height;
      if IsZero(ANode.height - ANode.des2.height, FP_CUTOFF) then
        aBLen2 := 0
      else
        aBLen2 := ANode.height - ANode.des2.height;
      Result := '(' + NewickStringRecursiveWithTimeFactor(ANode.des1) + ':' + Trim(FormatDoubleSafe((aBLen)*TF, Precision, ValueDigits)) + ',' + NewickStringRecursiveWithTimeFactor(ANode.des2) + ':' + Trim(FormatDoubleSafe((aBLen2)*TF, Precision, ValueDigits)) + ')';
    end;
    nLabel := GetInternalNodeLabel(MapNodeIndexToTreeDataIndex(ANode.Index), ANode);
    if (nLabel <> EmptyStr) and (nLabel <> '-') then
      Result := Format('%s%s%s%s', [Result, #39, nLabel, #39]);
  end;

  function RtdtNewick(aNode: TReltimeTreeNode): String;
  var
    TF: extended = 1;
    aBlen, aBlen2: Extended;
    nLabel: String = '';
  begin
    if UseTimeFactor then
      TF := abs(TimeFactor);

    // Base case - leaf nodes return their names
    if ANode.OTU then
    begin
      Result := TTreeList.CleanOtuNameForExport(OtuNames[ANode.Index]);
      exit;
    end;

    if ANode.des1.OTU and ANode.des2.Otu then
    begin
      Result := '(' + RtdtNewick(ANode.des1) + ':' + Trim(FormatDoubleSafe(RtdtTimeBlen(aNode.des1), Precision, ValueDigits)) + ',' + RtdtNewick(ANode.des2) + ':' + Trim(FormatDoubleSafe(RtdtTimeBlen(ANode.des2), Precision, ValueDigits)) + ')'
    end
    else if ANode.des1.OTU then
    begin
      if IsZero(ANode.height - ANode.des2.height, FP_CUTOFF) then
        aBlen := 0
      else
        aBlen := ANode.height - ANode.des2.height;
      Result := '(' + RtdtNewick(ANode.des1) + ':' + Trim(FormatDoubleSafe(RtdtTimeBlen(ANode.des1), Precision, ValueDigits)) + ',' + RtdtNewick(ANode.des2) + ':' + Trim(FormatDoubleSafe((aBlen)*TF, Precision, ValueDigits)) + ')'
    end
    else if ANode.des2.OTU then
    begin
      if IsZero(ANode.height - ANode.des1.height, FP_CUTOFF) then
        aBlen := 0
      else
        aBlen := ANode.height - ANode.des1.height;
      Result := '(' + RtdtNewick(ANode.des1) + ':' + Trim(FormatDoubleSafe((aBLen)*TF, Precision, ValueDigits)) + ',' + RtdtNewick(ANode.des2) + ':' + Trim(FormatDoubleSafe(RtdtTimeBlen(ANode.des2), Precision, ValueDigits)) + ')'
    end
    else
    begin
      if IsZero(ANode.height - ANode.des1.height, FP_CUTOFF) then
        aBLen := 0
      else
        aBlen := ANode.height - ANode.des1.height;
      if IsZero(ANode.height - ANode.des2.height, FP_CUTOFF) then
        aBLen2 := 0
      else
        aBLen2 := ANode.height - ANode.des2.height;
      Result := '(' + RtdtNewick(ANode.des1) + ':' + Trim(FormatDoubleSafe((aBLen)*TF, Precision, ValueDigits)) + ',' + RtdtNewick(ANode.des2) + ':' + Trim(FormatDoubleSafe((aBLen2)*TF, Precision, ValueDigits)) + ')';
    end;
    nLabel := GetInternalNodeLabel(MapNodeIndexToTreeDataIndex(ANode.Index), ANode);
    if (nLabel <> EmptyStr) and (nLabel <> '-') then
      Result := Format('%s%s%s%s', [Result, #39, nLabel, #39]);
  end;

begin
  Assert(Assigned(FRoot), 'root node undefined');
  Assert(Assigned(FRoot.des1), 'descendant 1 of root node undefined');
  if FIsSamplingTime then
    Result := RtdtNewick(FRoot.des1) + ';'
  else if UseTimeFactor then
    Result := NewickStringRecursiveWithTimeFactor(FRoot.des1) + ';'
  else
    Result := NewickStringRecursive(FRoot.des1) + ';';
end;

end.
