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

unit mlittle_bootstrap_tree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MTreeDataAdapter, MTreeData, MTreeList, MSimpleTreeNode,
  mextendedlist, StrHashMap, MCalibrationData, mnode_time_data, mreltimecomputer,
  MNewickExportOptions;

const
  IdFormatString = '%6d';
  SIdFormatString = '%6s';
  FloatFormatString = '%16.6f';
  SFloatFormatString = '%16s';
  FloatFormatString2 = '%23.6f';
  SFloatFormatString2 = '%23s';
  PercentString = '%13.2f%%';
  SPercentString = '%13s';
  MAX_LABEL_LENGTH = 50;

type

  { TLittleBootstrapsTimetree }

  TLittleBootstrapsTimetree = class(TObject)
    private
      FTimeSmoothingApplied: Boolean;
      FTimeFactor: Extended;
      FData: TMultiSampleNodeTimeDataArray;
      function GetLogText: String;
      function GetNumOtus: Integer;
    protected
      FFailedReplicates: TTreeList;
      FCalibrations: TCalibrations;
      FIsSessionFileTree: Boolean;
      FDebugTimeStrings: TStringList;
      FDebugVarianceStrings: TStringList; { these debug TStringLists are only used for dumping raw data for testing. We can get rid of them once testing is done}
      FSubsampleTreesArray: TTreeListArray;
      FLog: TStringList;
      FPartitionStrings: TStringList;
      FReltimeTreesArray: TTreeListArray;
      FOtuNames: TStringList;
      FCandidateTree: TTreeData;
      FTimeTree: TSimpleTreeDataAdapter;
      FDivergenceTimeCalculators: TStringHashMap;
      FTempTree: TSimpleTreeDataAdapter;
      FTreeNodeMap: TStringHashMap;
      FNameFormatString: String;
      FBranchLengthsSetFromNodeHeights: Boolean;
      procedure Initialize;
      function GetComplementOfPartitionString(partitionString: String): String;

      function ClearCalculators(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;
      function GetNameFormatString: String;
      function GetHeaderFormatString: String;
      function GetLineFormatString(nodeIndex: Integer): String;
      procedure SetBranchLengthsFromNodeHeights;
      procedure GenerateReltimeTrees;
      function ResultsFootnote: String;
      function GetNodeTime(n: TSimpleTreeNode; useDivtime: Boolean): Extended;
      procedure ResetNodeTimeDataFromSmoothedTiming(computer: TReltimeComputer);
      function DivTimeTrees(aTreeList: TTreeList; options: TNewickExportOptions; dataToUse: String): TStringList;
      function DivTimetreeToFile(filename: String; appendToFile: Boolean; comment: String; dataToUse: String): Boolean;
    public
      {
          to add:
             First check if any ancestor heights are less than descendant heights. If so, the analysis must fail.
             Second, check if the tree is ultrametric. If not, time smoothing must be done and the user must be notified:
                     take the current time tree and use it to create a new reltime tree
                     compare this new time tree to the original one and where node heights have been changed, confidence intervals must be shifed accordingly by % change
      }
      constructor CreateFromAvgBLens(subsampleTrees: TTreeList; candidateTree: TTreeData; otuNames: TStringList); deprecated;
      constructor CreateFromSubsampleTrees(subsampleTrees: TTreeListArray; candidateTree: TTreeData; otuNames: TStringList);
      constructor CreateFromSessionFile(var aFile: File; sessionFileVersion: Integer; numOtus: Integer; otuNames: TStringList);

      destructor Destroy; override;
      function SaveToSessionFile(var aFile: File): Boolean;
      function IsUltrametric: Boolean;
      function IsValidTree(var aMsg: String): Boolean;
      function ApplyTimeSmoothing(nonUltraMetricTree: TTreeData): Boolean;
      function EstimateDivergenceTimes(aCalibrations: TCalibrations): Boolean;
      function GetTabularOutputHeader: String;
      function GetTabularOutputLine(nodeIndex: Integer): String;
      function TabularOutputToStringList: TStringList;
      function TabularOutputToFile(filename: String; sortByInputOrder: Boolean = False): Boolean;
      function ExportToNexusStrings: TStringList;
      function ExportToNexusFile(Filename: String): Boolean;
      function GetNewickNoOutgroup(useDivTimes: Boolean): String;
      function NewickOutputToFile(const filename: String; const appendToFile: Boolean; var newickStr: String): Boolean;
      function NewickOutputToFileNoOutgroup(const filename: String; const appendToFile: Boolean): Boolean;
      function ReltimeTreesToFile(filename: String; appendToFile: Boolean; comment: String = ''): Boolean;
      function DebugTimeStringsToFile(filename: String; appendToFile: Boolean): Boolean;
      function DebugVarianceStringsToFile(filename: String; appendToFile: Boolean): Boolean;
      function MaxTime(nodeIndex: Integer): Double;
      function MinTime(nodeIndex: Integer): Double;
      function DivTime(nodeIndex: Integer): Double;
      function GetIngroupRootDivTime: Double;
      function HasCalibrations: Boolean;
      function DoTimingForReplicateTreesOnly(aCalibrations: TCalibrations): Boolean;
      procedure SetCalibrations(aCalibrations: TCalibrations);
      procedure ErrorBarHeight(nodeIndex: Integer; var errHigh: Double; var errLow: Double);
      procedure GetTreeData(var aData: TTreeData);
      property LogText: String read GetLogText;
      property NumOtus: Integer read GetNumOtus;
      property TimeSmoothingApplied: Boolean read FTimeSmoothingApplied;
      property TimeFactor: Extended read FTimeFactor write FTimeFactor;
  end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ENDIF}
  StringUtils, math, MegaConsts,
  mtree_display_setup, MegaUtils, mreltimetreenode;

{ TLittleBootstrapsTimetree }

function TLittleBootstrapsTimetree.GetLogText: String;
begin
  if Assigned(FLog) then
    Result := FLog.Text
  else
    Result := EmptyStr;
end;

function TLittleBootstrapsTimetree.GetNumOtus: Integer;
begin
  if Assigned(FTimeTree) then
    Result := FTimeTree.NumTaxa
  else
    Result := 0;
end;

procedure TLittleBootstrapsTimetree.Initialize;
var
  nodeDataCalculators: TMultiSampleNodeTimeData = nil;
  i: Integer;
  n: TSimpleTreeNode = nil;
  aNumSamples: Integer = -1;
begin
  FPartitionStrings := TStringList.Create;
  GetInternalNodePartitionStrings(FPartitionStrings, FCandidateTree, FOtuNames);
  GenerateReltimeTrees;
  aNumSamples := Length(FReltimeTreesArray);
  //{$IFDEF DEBUG}
  //FPartitionStrings.SaveToFile(NextAvailableFilenameNV('_partition_strings.txt'));
  //FReltimeTrees.ExportToNewickFile(NextAvailableFilenameNV('_reltime_trees.nwk'), True, False, 0.0);
  //{$ENDIF}
  if not FIsSessionFileTree then
  begin
    FDivergenceTimeCalculators := TStringHashMap.Create;
    SetLength(FData, FPartitionStrings.Count);
    for i := 0 to FPartitionStrings.Count - 1 do
    begin
      nodeDataCalculators := TMultiSampleNodeTimeData.Create(aNumSamples, FPartitionStrings[i]);
      nodeDataCalculators.HasCalibrations := Assigned(FCalibrations);
      FDivergenceTimeCalculators.Add(FPartitionStrings[i], nodeDataCalculators);
      FData[i] := nodeDataCalculators;
    end;
  end;

  FTempTree := TSimpleTreeDataAdapter.Create;
  FTimeTree := TSimpleTreeDataAdapter.Create;
  FTimeTree.SetTreeData(FCandidateTree, False, FOtuNames, FPartitionStrings);
  FTreeNodeMap := TStringHashMap.Create;
  for i := FTimeTree.NumTaxa to FTimeTree.NumNodes - 1 do
  begin
    n := FTimeTree.Nodes[i];
    FTreeNodeMap.Add(n.Id, n);
  end;
end;

function TLittleBootstrapsTimetree.GetComplementOfPartitionString(partitionString: String): String;
var
  i: Integer;
begin
  SetLength(Result, Length(partitionString));
  if Length(Result) > 0 then
    for i := 1 to Length(Result) do
      if partitionString[i] = '0' then
        Result[i] := '1'
      else
        Result[i] := '0';
end;

function TLittleBootstrapsTimetree.ClearCalculators(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;
var
  aList: TExtendedList = nil;
begin
  if Assigned(APtr) then
  begin
    aList := TExtendedList(APtr);
    aList.Free;
  end;
  Result := True;
end;

function TLittleBootstrapsTimetree.GetNameFormatString: String;
var
  longestName: Integer;
begin
  longestName := LengthOfLongestString(FOtuNames) + Length(OUTGROUP_MARKER);
  if (FTimeTree.NumTaxa <= MAX_LABEL_LENGTH) and (longestName < FTimeTree.NumTaxa) then
    longestName := FTimeTree.NumTaxa;
  FNameFormatString := Format('%%-%ds', [longestName]);
  Result := FNameFormatString;
end;

function TLittleBootstrapsTimetree.GetHeaderFormatString: String;
begin
  if HasCalibrations then
    Result := FNameFormatString + SIdFormatString + SIdFormatString + SIdFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString
  else
    Result := FNameFormatString + SIdFormatString + SIdFormatString + SIdFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString;
end;

function TLittleBootstrapsTimetree.GetLineFormatString(nodeIndex: Integer): String;
begin
  if HasCalibrations then
  begin
    if nodeIndex < FTimeTree.NumTaxa then
      Result := FNameFormatString + IdFormatString + SIdFormatString + SIdFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString
    else if FTimetree.Nodes[nodeIndex].IsOutgroupMember or FTimetree.Nodes[nodeIndex].IsRoot then
      Result := FNameFormatString + IdFormatString + IdFormatString + IdFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString
    else
      Result := FNameFormatString + IdFormatString + IdFormatString + IdFormatString + FloatFormatString + FloatFormatString + FloatFormatString + FloatFormatString + FloatFormatString;
  end
  else
  begin
    if nodeIndex < FTimeTree.NumTaxa then
      Result := FNameFormatString + IdFormatString + SIdFormatString + SIdFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString
    else if FTimetree.Nodes[nodeIndex].IsOutgroupMember or FTimetree.Nodes[nodeIndex].IsRoot then
      Result := FNameFormatString + IdFormatString + IdFormatString + IdFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString + SFloatFormatString
    else
      Result := FNameFormatString + IdFormatString + IdFormatString + IdFormatString + FloatFormatString + FloatFormatString + FloatFormatString + FloatFormatString;
  end;
end;

procedure TLittleBootstrapsTimetree.SetBranchLengthsFromNodeHeights;

  procedure ProcessNode(n: TSimpleTreeNode);
  begin
    if not n.IsOtu then
    begin
      ProcessNode(n.Des1);
      ProcessNode(n.Des2);
    end;
    if n.IsOtu then
      n.BLen := n.Ancestor.Value
    else if n <> FTimeTree.Root then
      n.Blen := n.Ancestor.Value - n.Value;
    if CompareValue(n.Blen, 0, FP_CUTOFF) < 0 then
      n.Blen := 0;
  end;

begin
  ProcessNode(FTimeTree.Root);
  FBranchLengthsSetFromNodeHeights := True;
end;

procedure TLittleBootstrapsTimetree.GenerateReltimeTrees;
var
  aSample: Integer = -1;
  aReplicate: Integer = -1;
  aTree: TTreeData = nil;
  aFailedReplicate: TTreeData = nil;
  computer: TReltimeComputer = nil;
  aTimetree: TTimetreeData = nil;
  i: Integer = -1;
  aCalib: TCalibrationTime = nil;
  numSuccessful: Integer = 0;
  {$IFDEF DEBUG}
  debug: Boolean = False;
  {$ENDIF}
begin
  try
    FFailedReplicates := TTreeList.Create;
    FFailedReplicates.OTUNameList.AddStrings(FOtuNames);
    computer := TReltimeComputer.Create;
    SetLength(FReltimeTreesArray, Length(FSubsampleTreesArray));
    for i := Low(FSubsampleTreesArray) to High(FSubsampleTreesArray) do
    begin
      FReltimeTreesArray[i] := TTreeList.Create;
      FReltimeTreesArray[i].OTUNameList.AddStrings(FOtuNames);
      if FSubsampleTreesArray[i].OTUNameList.Count = 0 then
        FSubsampleTreesArray[i].OTUNameList.AddStrings(FOtuNames);
    end;

    if Assigned(FCalibrations) then
    begin
      for i := 0 to FCalibrations.Count - 1 do
      begin
        aCalib := FCalibrations.GetCalibration(i);
        Assert((aCalib.NodeName = EmptyStr) or (aCalib is TIngroupRootCalibration), 'node labels are not supported for LBS dating because topologies may differ between bootstrap replicates');
        aCalib.NodeName := EmptyStr;
        TrimTaxaName2(aCalib.NodeA);
        TrimTaxaName2(aCalib.NodeB);
        aCalib.NodeIDA := FSubsampleTreesArray[0].NodeNameToID(aCalib.NodeA);
        aCalib.NodeIDB := FSubsampleTreesArray[0].NodeNameToID(aCalib.NodeB);
      end;
    end;

    for aSample := Low(FSubsampleTreesArray) to High(FSubsampleTreesArray) do
    begin
      if FSubSampleTreesArray[aSample].Count > 0 then
      begin
        for aReplicate := 0 to FSubSampleTreesArray[aSample].Count - 1 do
        begin
          if Assigned(FCalibrations) then
          begin
            for i := 0 to FCalibrations.Count - 1 do
            begin
              aCalib := FCalibrations.GetCalibration(i);
              aCalib.NodeID := -1;
              if aCalib is TGroupCalibrationTime then
              begin
                FSubSampleTreesArray[aSample].ResolveGroupCalibrationTimeName(TGroupCalibrationTime(aCalib));
                TrimTaxaName2(aCalib.NodeA);
                TrimTaxaName2(aCalib.NodeB);
              end;
            end;

            FSubSampleTreesArray[aSample][aReplicate].ResolveRelTimes(FCalibrations);
            try
              aTree := computer.ComputeLbsDivTimes(FSubSampleTreesArray[aSample][aReplicate], FCalibrations);
            except
              aFailedReplicate := TTreeData.Create(FSubSampleTreesArray[aSample][aReplicate].NoOfOTUs, FSubSampleTreesArray[aSample][aReplicate].isBLen, FSubSampleTreesArray[aSample][aReplicate].isStats, FSubSampleTreesArray[aSample][aReplicate].isStats);
              aFailedReplicate.Assign(FSubSampleTreesArray[aSample][aReplicate]);
              FFailedReplicates.Add(aFailedReplicate);
              {$IFNDEF VISUAL_BUILD}
              warn_NV(Format('Reltime analysis failed for replicate %d in sample %d', [aReplicate + 1, aSample + 1] ));
              {$ENDIF}
              FLog.Add(Format('skipping div time estimation for replicate %d from subsample %d because Reltime failed', [aReplicate + 1, aSample + 1]));
              continue;
            end;
          end
          else
            aTree := computer.ComputeRelTimeBLens(FSubSampleTreesArray[aSample][aReplicate], DEFAULT_MAX_RATE_RATIO);
          if not Assigned(aTree) then
            raise Exception.Create(Format('failed to generate a Reltime tree for replicate %d in sample %d', [aReplicate + 1, aSample + 1]));
          inc(numSuccessful);
          aTimetree := TTimetreeData.Create(aTree.NoOfOTUs, aTree.isBLen, aTree.isSE, aTree.isStats, Assigned(FCalibrations), False);
          aTimetree.Assign(aTree);
          for i := aTree.NoOfOtus to 2*aTree.NoOfOtus - 2 do
          begin
            if computer.Node[i].anc <> nil then
            begin
              aTimetree.Reltimes[i] := computer.NodeHeight(i);
              aTimetree.MinReltimes[i] := computer.MinNodeHeight(i);
              aTimetree.MaxReltimes[i] := computer.MaxNodeHeight(i);
              aTimetree.DivTimes[i] := computer.DivTime(i);
              aTimetree.MinDivTimes[i] := computer.MinDivTime(i);
              aTimetree.MaxDivTimes[i] := computer.MaxDivTime(i);
              {$IFDEF DEBUG}
              if Assigned(FCalibrations) and (computer.MaxDivTime(i) > FCalibrations.GetCalibration(0).MaxTime*100) then
                debug := True;
              {$ENDIF}
            end;
          end;
          aTimetree.TimeFactor := computer.TimeFactor;
          aTimetree.MinTimeFactor := computer.MinTimeFactor;
          aTimetree.MaxTimeFactor := computer.MaxTimeFactor;
          FReltimeTreesArray[aSample].Add(aTimetree);
        end;
      end;
    end;
    if numSuccessful = 0 then
      raise Exception.Create('failed to generate Reltime trees for Little Bootstraps timing');
    {$IFNDEF VISUAL_BUILD}
    if FFailedReplicates.Count > 0 then
      FFailedReplicates.ExportToNewickFile(NextAvailableFilenameNV('_failed_replicates.nwk'), True, True, 0.0);
    {$ENDIF}
  finally
    if Assigned(computer) then
      computer.Free;
  end;
end;

function TLittleBootstrapsTimetree.GetNewickNoOutgroup(useDivTimes: Boolean): String;
const
  Precision = 7;
  ValueDigits = 16;

  function NewickStringRecursive(ANode: TSimpleTreeNode): String;
  var
    aBlen, aBlen2: Extended;
  begin
    // Base case - leaf nodes return their names
    if ANode.IsOtu then
    begin
      Result := TTreeList.CleanOtuNameForExport(FOtuNames[ANode.NodeIndex]);
      exit;
    end;

    if ANode.des1.IsOtu and ANode.des2.IsOtu then
    begin
      Result := '(' + NewickStringRecursive(ANode.des1) + ':' + Trim(FormatDoubleSafe(ANode.Value, Precision, ValueDigits)) + ',' + NewickStringRecursive(ANode.des2) + ':' + Trim(FormatDoubleSafe(ANode.Value, Precision, ValueDigits)) + ')'
    end
    else if ANode.des1.IsOtu then
    begin
      if IsZero(GetNodeTime(ANode, useDivTimes) - GetNodeTime(ANode.des2, useDivTimes), FP_CUTOFF) then
        ablen := 0
      else
        ablen := GetNodeTime(ANode, useDivTimes) - GetNodeTime(ANode.des2, useDivTimes);
      Result := '(' + NewickStringRecursive(ANode.des1) + ':' + Trim(FormatDoubleSafe(GetNodeTime(ANode, useDivTimes), Precision, ValueDigits)) + ',' + NewickStringRecursive(ANode.des2) + ':' + Trim(FormatDoubleSafe(ablen, Precision, ValueDigits)) + ')'
    end
    else if ANode.des2.IsOtu then
    begin
      if IsZero(GetNodeTime(ANode, useDivTimes) - GetNodeTime(ANode.des1, useDivTimes), FP_CUTOFF) then
        ablen := 0
      else
        ablen := GetNodeTime(ANode, useDivTimes) - GetNodeTime(ANode.des1, useDivTimes);
      Result := '(' + NewickStringRecursive(ANode.des1) + ':' + Trim(FormatDoubleSafe(ablen, Precision, ValueDigits)) + ',' + NewickStringRecursive(ANode.des2) + ':' + Trim(FormatDoubleSafe(GetNodeTime(ANode, useDivTimes), Precision, ValueDigits)) + ')'
    end
    else
    begin
      if IsZero(GetNodeTime(ANode, useDivTimes) - GetNodeTime(ANode.des1, useDivTimes), FP_CUTOFF) then
        ablen := 0
      else
        ablen := GetNodeTime(ANode, useDivTimes) - GetNodeTime(ANode.des1, useDivTimes);
      if IsZero(GetNodeTime(ANode, useDivTimes) - GetNodeTime(ANode.des2, useDivTimes), FP_CUTOFF) then
        ablen2 := 0
      else
        ablen2 := GetNodeTime(ANode, useDivTimes) - GetNodeTime(ANode.des2, useDivTimes);
      Result := '(' + NewickStringRecursive(ANode.des1) + ':' + Trim(FormatDoubleSafe(ablen, Precision, ValueDigits)) + ',' + NewickStringRecursive(ANode.des2) + ':' + Trim(FormatDoubleSafe(ablen2, Precision, ValueDigits)) + ')';
    end;
  end;

begin
  Result := NewickStringRecursive(FTimeTree.Root.Des1) + ';';
end;

function TLittleBootstrapsTimetree.ResultsFootnote: String;
begin
  Result := Format('Note: %s indicates members of the outgroup. The timing method does not estimate times for outgroup nodes.', [Trim(OUTGROUP_MARKER)]);
  if FTimeSmoothingApplied then
    Result := Result + LineEnding + 'Note: times were computed by applying the Reltime method to a non-ultrametric timetree. Confidence intervals were estimated by applying the Reltime (using branch lengths) method to each bootstrap replicate produced by iterative subsampling and then taking the mean CIs among replicate Reltime trees'
  else
    Result := Result + LineEnding + 'Note: times and confidence intervals were estimated by applying the Reltime (using branch lengths) method to each bootstrap replicate produced using bootstrap subsampling and then taking the mean times and CIs among replicate Reltime trees';
end;

function TLittleBootstrapsTimetree.HasCalibrations: Boolean;
begin
  if Assigned(FCalibrations) and (FCalibrations.Count > 0) then
    Result := True
  else
    Result := False;
end;

function TLittleBootstrapsTimetree.DoTimingForReplicateTreesOnly(aCalibrations: TCalibrations): Boolean;
var
  filename: String = '';
begin
  FCalibrations := aCalibrations;
  FPartitionStrings := TStringList.Create;
  GenerateReltimeTrees;
  {$IFNDEF VISUAL_BUILD}
  ReltimeTreesToFile(NextAvailableFilenameNV('_replicate_timetrees.nwk'), False);
  {$ENDIF}
end;

procedure TLittleBootstrapsTimetree.SetCalibrations(aCalibrations: TCalibrations);
begin
  FCalibrations := aCalibrations;
end;

function TLittleBootstrapsTimetree.GetNodeTime(n: TSimpleTreeNode; useDivtime: Boolean): Extended;
var
  d: TMultiSampleNodeTimeData = nil;
begin
  Assert(Assigned(n.MyObject));
  Assert(n.MyObject is TMultiSampleNodeTimeData);
  d := TMultiSampleNodeTimeData(n.MyObject);
  if useDivtime then
    Result := d.DivTime
  else
    Result := d.Reltime;
end;

procedure TLittleBootstrapsTimetree.ResetNodeTimeDataFromSmoothedTiming(computer: TReltimeComputer);
var
  i: Integer = -1;
  nodeTimeData: TMultiSampleNodeTimeData = nil;
  n: TSimpleTreeNode = nil;
  rTime: Extended = -1;
  rTimeMin: Extended = -1;
  rTimeMax: Extended = -1;
  dTime: Extended = -1;
  dTimeMin: Extended = -1;
  dTimeMax: Extended = -1;
begin
  for i := FTimeTree.NumTaxa to FTimeTree.NumNodes - 1 do
  begin
    n := FTimeTree.Nodes[i];
    nodeTimeData := TMultiSampleNodeTimeData(n.MyObject);
    Assert(nodeTimeData.NodeIndex > 0);
    if CompareValue(nodeTimeData.Reltime, 0, FP_CUTOFF) > 0 then
    begin
      rTime := computer.NodeHeight(nodeTimeData.NodeIndex);
      rTimeMin := max(0, rTime - (nodeTimeData.Reltime - nodeTimeData.MinReltime));
      rTimeMax := rTime + (nodeTimeData.MaxReltime - nodeTimeData.Reltime);
      if Assigned(FCalibrations) then
      begin
        dTime := computer.DivTime(nodeTimeData.NodeIndex);
        dTimeMin := max(0, dTime - (nodeTimeData.DivTime - nodeTimeData.MinDivTime));
        dTimeMax := dTime + (nodeTimeData.MaxDivTime - nodeTimeData.DivTime);
      end;
    end
    else
    begin
      {$IFNDEF VISUAL_BUILD}
      warn_nv('LBS Invalid State Error: time smoothing for a non-ultrametric tree failed. Possibly, at least one branch has zero BCL and therefore, no valid CI to map to an adjusted time');
      {$ENDIF}
      raise Exception.Create('LBS Invalid State Error: time smoothing for a non-ultrametric tree failed. Possible, at least one branch has zero BCL and therefore, no valid CI to map to an adjusted time');
      rTime := computer.NodeHeight(nodeTimeData.NodeIndex);
      rTimeMin := computer.MinNodeHeight(nodeTimeData.NodeIndex);
      rTimeMax := computer.MaxNodeHeight(nodeTimeData.NodeIndex);
      if Assigned(FCalibrations) then
      begin
        dTime := computer.DivTime(nodeTimeData.NodeIndex);
        dTimeMin := computer.MinDivTime(nodeTimeData.NodeIndex);
        dTimeMax := computer.MaxDivTime(nodeTimeData.NodeIndex);
      end;
    end;


    nodeTimeData.Reset(1);

    nodeTimeData[0].ReltimeCalculator.Add(rTime);
    nodeTimeData[0].MinReltimeCalculator.Add(rTimeMin);
    nodeTimeData[0].MaxReltimeCalculator.Add(rTimeMax);
    nodeTimeData[0].StdErrCalculator.Add(sqrt(computer.Node[nodeTimeData.NodeIndex].vh));
    if Assigned(FCalibrations) then
    begin
      nodeTimeData[0].DivtimeCalculator.Add(dTime);
      nodeTimeData[0].MinDivtimeCalculator.Add(dTimeMin);
      nodeTimeData[0].MaxDivtimeCalculator.Add(dTimeMax);
      n.Value := nodeTimeData.DivTime;
    end
    else
      n.Value := nodeTimeData.Reltime;
  end;
end;

function TLittleBootstrapsTimetree.DivTimeTrees(aTreeList: TTreeList; options: TNewickExportOptions; dataToUse: String): TStringList;
var
  tempList: TTreeList = nil;
  d: TTimetreeData = nil;
  aData: TTimetreeData = nil;
  i: Integer;
begin
  if aTreeList.Count = 0 then
  begin
    Result := TStringList.Create;
    Exit;
  end;

  try
    tempList := TTreeList.Create;
    tempList.OTUNameList.AddStrings(FOtuNames);
    for i := 0 to aTreeList.Count - 1 do
    begin
      Assert(aTreeList[i] is TTimetreeData);
      d := TTimeTreeData(aTreeList[i]);
      aData := TTimetreeData.Create(d.NoOfOTUs, d.isBLen, d.isSE, d.isStats, d.IsCalibrated);
      aData.Clone(d);
      SetBranchLengthsFromDivTimes(aData, aTreeList.isRooted, FOtuNames, dataToUse);
      tempList.Add(aData);
    end;
    Result := tempList.OutputAllNewickTrees(options, 0.0);
  finally
    if Assigned(tempList) then
      tempList.Free;
  end;
end;

function TLittleBootstrapsTimetree.DivTimetreeToFile(filename: String;appendToFile: Boolean; comment: String; dataToUse: String): Boolean;
var
  aFile: TextFile;
  aFilename: String;
  aList: TStringList = nil;
  options: TNewickExportOptions;
  i: Integer;
  aSample: Integer = -1;
begin
  Result := False;
  if Length(FReltimeTreesArray) = 0 then
    Exit;
  aFilename := ChangeFileExt(filename, '_' + dataToUse + '.nwk');
  try
    try
      AssignFile(aFile, aFilename);
      if appendToFile and FileExists(aFilename) then
        Append(aFile)
      else
        Rewrite(aFile);
      if Trim(comment) <> EmptyStr then
        WriteLn(aFile, Format('[%s]', [comment]));
      options := GetDefaultNewickExportOptions(FReltimeTreesArray[0]);
      for aSample := Low(FReltimeTreesArray) to High(FReltimeTreesArray) do
      begin
        try
          aList := DivTimeTrees(FReltimeTreesArray[aSample], options, dataToUse);
          WriteLn(aFile, Format('[sample - %d %s trees]', [aSample + 1, dataToUse]));
          for i := 0 to aList.Count - 1 do
            WriteLn(aFile, aList[i]);
          WriteLn(aFile);
        finally
          if Assigned(aList) then
            aList.Free;
        end;
      end;
      Result := FileExists(aFilename);
    except
      on E:Exception do
      begin
        Result := False;
        FLog.Add(E.Message);
      end;
    end;
  finally
    CloseFile(aFile);
  end;
end;

constructor TLittleBootstrapsTimetree.CreateFromAvgBLens(subsampleTrees: TTreeList; candidateTree: TTreeData; otuNames: TStringList);
begin
  raise Exception.Create('deprecated constructor - use CreateFromSubsampleTrees');
  FTimeSmoothingApplied := False;
  FBranchLengthsSetFromNodeHeights := False;
  FCalibrations := nil;
  FIsSessionFileTree := False;
  FCandidateTree := candidateTree;
  FOtuNames := TStringList.Create;
  if Assigned(otuNames) then
    FOtuNames.AddStrings(otuNames);
  FLog := TStringList.Create;
  FDebugTimeStrings := TStringList.Create;
  FDebugVarianceStrings := TStringList.Create;
end;

constructor TLittleBootstrapsTimetree.CreateFromSubsampleTrees(subsampleTrees: TTreeListArray; candidateTree: TTreeData; otuNames: TStringList);
begin
  FTimeSmoothingApplied := False;
  FBranchLengthsSetFromNodeHeights := False;
  FCalibrations := nil;
  FIsSessionFileTree := False;
  FSubSampleTreesArray := subsampleTrees;
  FCandidateTree := candidateTree;
  FOtuNames := TStringList.Create;
  if Assigned(otuNames) then
    FOtuNames.AddStrings(otuNames);
  FLog := TStringList.Create;
  FDebugTimeStrings := TStringList.Create;
  FDebugVarianceStrings := TStringList.Create;
end;

constructor TLittleBootstrapsTimetree.CreateFromSessionFile(var aFile: File; sessionFileVersion: Integer; numOtus: Integer; otuNames: TStringList);
var
  i: Integer = -1;
  aNumNodes: Integer = -1;
begin
  FTimeSmoothingApplied := False;
  FBranchLengthsSetFromNodeHeights := False;
  FTimeFactor := 1;
  FIsSessionFileTree := True;
  FCandidateTree := TTreeData.Create(numOtus, True, False, True);
  FCandidateTree.ReadFromFile(aFile, sessionFileVersion);
  FOtuNames := TStringList.Create;
  FOtuNames.AddStrings(otuNames);
  FTimeTree := TSimpleTreeDataAdapter.Create;
  FTimeTree.SetTreeData(FCandidateTree, True, FOtuNames);
  BlockRead(aFile, aNumNodes, SizeOf(aNumNodes));
  Assert(aNumNodes = FTimetree.NumNodes, 'mismatched number of nodes');
  if aNumNodes > 0 then
    for i := 0 to aNumNodes - 1 do
      FTimeTree.Nodes[i].ReadFromTimetreeFile(aFile, sessionFileVersion);
  BlockRead(aFile, aNumNodes, SizeOf(Integer));
  if aNumNodes > 0 then
  begin
    FDivergenceTimeCalculators := TStringHashMap.Create;
    SetLength(FData, aNumNodes);
    for i := Low(FData) to High(FData) do
    begin
      FData[i] := TMultiSampleNodeTimeData.Create(1, EmptyStr);
      FData[i].LoadFromSessionFile(aFile);
      FDivergenceTimeCalculators.Add(FData[i].NodeId, FData[i]);
      FTimeTree.Nodes[FData[i].NodeIndex].MyObject := FData[i];
    end;
  end;
  BlockRead(aFile, FTimeSmoothingApplied, SizeOf(Boolean));
  FLog := TStringList.Create;
  FDebugTimeStrings := nil;
  FDebugVarianceStrings := nil;
end;

destructor TLittleBootstrapsTimetree.Destroy;
var
  i: Integer;
begin
  if Length(FData) > 0 then
    for i := Low(FData) to High(FData) do
      if Assigned(FData[i]) then
        FData[i].Free;
  SetLength(FData, 0);

  if Length(FSubsampleTreesArray) > 0 then
    for i := Low(FSubsampleTreesArray) to High(FSubsampleTreesArray) do
    begin
      if Assigned(FSubsampleTreesArray[i]) then
        FSubsampleTreesArray[i].Free
    end;

  if FIsSessionFileTree and Assigned(FCandidateTree) then
    FreeAndNil(FCandidateTree);
  if Assigned(FLog) then
    FLog.Free;
  if Assigned(FOtuNames) then
    FOtuNames.Free;
  if Assigned(FDivergenceTimeCalculators) then
    FDivergenceTimeCalculators.Free;
  if Assigned(FPartitionStrings) then
    FPartitionStrings.Free;
  if Assigned(FTempTree) then
    FTempTree.Free;
  if Assigned(FTimeTree) then
    FTimeTree.Free;
  if Assigned(FDebugTimeStrings) then
    FDebugTimeStrings.Free;
  if Assigned(FDebugVarianceStrings) then
    FDebugVarianceStrings.Free;
  inherited Destroy;
end;

function TLittleBootstrapsTimetree.SaveToSessionFile(var aFile: File): Boolean;
var
  aNumNodes: Integer = -1;
  i: Integer = -1;
begin
  FCandidateTree.WriteToFile(aFile);
  aNumNodes := FTimeTree.NumNodes;
  BlockWrite(aFile, aNumNodes, SizeOf(Integer));
  if aNumNodes > 0 then
    for i := 0 to aNumNodes - 1 do
      FTimeTree.Nodes[i].WriteToTimetreeFile(aFile);
  aNumNodes := Length(FData);
  BlockWrite(aFile, aNumNodes, SizeOf(Integer));
  if aNumNodes > 0 then
    for i := Low(FData) to High(FData) do
      FData[i].SaveToSessionFile(aFile);
  BlockWrite(aFile, FTimeSmoothingApplied, SizeOf(Boolean));
  Result := True;
end;

function TLittleBootstrapsTimetree.IsUltrametric: Boolean;
begin
  Result := False;
  if Assigned(FTimeTree) then
    Result := FTimeTree.IsUltrametric;
end;

function TLittleBootstrapsTimetree.IsValidTree(var aMsg: String): Boolean;
begin
  if not FBranchLengthsSetFromNodeHeights then
    raise Exception.Create('Application Error: node heights must be set before validating a time tree');
  Result := not FTimeTree.HasNegativeBranchLengths;
  aMsg := 'The generated timetree is invalid. At least one ancestor has a divergence time that is less than a descendant divergence time';
end;

function TLittleBootstrapsTimetree.ApplyTimeSmoothing(nonUltraMetricTree: TTreeData): Boolean;
var
  aMsg: String = '';
  computer: TReltimeComputer = nil;
  aTimeTree: TTreeData = nil;
  tabularOutput: TStringList = nil;
begin
  Result := False;
  try
    try
      if not IsValidTree(aMsg) then
        raise Exception.Create('Application Error: cannot apply time smoothing to a tree with negative branch lengths');
      computer := TReltimeComputer.Create;
      tabularOutput := TStringList.Create;
      if Assigned(FCalibrations) then
        aTimeTree := computer.ComputeReltimeBLens(nonUltraMetricTree, tabularOutput, FOtuNames, FCalibrations, DEFAULT_MAX_RATE_RATIO)
      else
        aTimeTree := computer.ComputeReltimeBLens(nonUltraMetricTree, tabularOutput, FOtuNames, nil, DEFAULT_MAX_RATE_RATIO);
      Result := Assigned(aTimeTree);
      if Result then
      begin
        ResetNodeTimeDataFromSmoothedTiming(computer);
        SetBranchLengthsFromNodeHeights;
        FTimeSmoothingApplied := True;
        {$IFNDEF VISUAL_BUILD}{$IFDEF DEBUG}
        TabularOutputToFile(NextAvailableFilenameNV('_after_smoothing.txt'));
        {$ENDIF}{$ENDIF}
      end;
    except
      on E:Exception do
        FLog.Add('error when trying to apply time smoothing to a non-ultrametric timetree: ' + E.Message);
    end;
  finally
    if Assigned(computer) then
      computer.Free;
    if Assigned(aTimeTree) then
      aTimeTree.Free;
    if Assigned(tabularOutput) then
      tabularOutput.Free;
  end;
end;

function TLittleBootstrapsTimetree.EstimateDivergenceTimes(aCalibrations: TCalibrations): Boolean;
var
  aSample: Integer = -1;
  aReplicate: Integer = -1;
  i, j: Integer;
  n: TSimpleTreeNode = nil;
  nodeTimeData: TMultiSampleNodeTimeData = nil;
  map: TStringToTimeDataMapArray;
  aTree: TTreeData = nil;
  aTimetree: TTimetreeData = nil;
  pTime: Pointer = nil;
  pNodeMap: Pointer =  nil;
begin
  try
    try
      FTimeSmoothingApplied := False;
      if Assigned(aCalibrations) then
        FCalibrations := aCalibrations;
      Initialize;

      for aSample := Low(FReltimeTreesArray) to High(FReltimeTreesArray) do
      begin
        for aReplicate := 0 to FReltimeTreesArray[aSample].Count - 1 do
        begin
          aTree := FReltimeTreesArray[aSample][aReplicate];
          aTimetree := TTimetreeData(aTree);

          //if Assigned(FCalibrations) then
          //  FTimeFactor += aTimetree.TimeFactor;

          FTempTree.SetTimetreeData(aTimetree, False, FOtuNames);
          FTempTree.SetPartitions;
          map := FTempTree.GetTimeDataMap;
          for j := Low(map) to High(map) do
          begin
            if FDivergenceTimeCalculators.Find(map[j].Id, pTime) then
            begin
              nodeTimeData := TMultiSampleNodeTimeData(pTime);
              nodeTimeData[aSample].ReltimeCalculator.Add(map[j].Reltime);
              nodeTimeData[aSample].MinReltimeCalculator.Add(map[j].MinReltime);
              nodeTimeData[aSample].MaxReltimeCalculator.Add(map[j].MaxReltime);
              nodeTimeData[aSample].StdErrCalculator.Add(map[j].StdDev);

              nodeTimeData[aSample].DivtimeCalculator.Add(map[j].DivTime);
              nodeTimeData[aSample].MinDivtimeCalculator.Add(map[j].MinDivTime);
              nodeTimeData[aSample].MaxDivtimeCalculator.Add(map[j].MaxDivTime);
            end;
            map[j].Free;
            map[j] := nil;
          end;
          SetLength(map, 0);
        end;
      end;
      aTree := nil;

      //if Assigned(FCalibrations) then
      //  FTimeFactor := timeFactor/FReltimeTrees.Count;

      if IsDeveloper then
      begin
        FDebugTimeStrings.Clear;
        FDebugVarianceStrings.Clear;
      end;

      for i := 0 to FPartitionStrings.Count - 1 do
      begin
        if FDivergenceTimeCalculators.Find(FPartitionStrings[i], pTime) then
        begin
          nodeTimeData := TMultiSampleNodeTimeData(pTime);

          if FTreeNodeMap.Find(FPartitionStrings[i], pNodeMap) then
          begin
            n := TSimpleTreeNode(pNodeMap);
            n.MyObject := nodeTimeData;
            nodeTimeData.NodeIndex := n.NodeIndex;
            if IsDeveloper then
            begin
              FDebugTimeStrings.Add(Format('{ %s },', [FPartitionStrings[i]]) + nodeTimeData[0].DivTimeCalculator.ToCsvString('%.8f'));
              FDebugVarianceStrings.Add(Format('{ %s },', [FPartitionStrings[i]]) + nodeTimeData[0].MaxDivtimeCalculator.ToCsvString('%.8f'));
            end;
            if HasCalibrations then
              n.Value := nodeTimeData.DivTime
            else
              n.Value := nodeTimeData.Reltime;
          end
          else
            raise Exception.Create('missing partition string for node');
        end
        else
          raise Exception.Create('missing partition string');
      end;
      FTimeTree.Root.IsRoot := True;
      SetBranchLengthsFromNodeHeights;
      if FTimeTree.HasNegativeBranchLengths then
        Raise Exception.Create('Little Bootstraps time estimation failed. At least one computed node time is greater than that of a descendant node time')
      else
        Result := True;
      if not FTimeTree.IsUltrametric then
      begin
        //{$IFNDEF VISUAL_BUILD}{$IFDEF DEBUG}
        //TabularOutputToFile(NextAvailableFilenameNV('_before_smoothing.txt'));
        //{$ENDIF}{$ENDIF}
        aTree := TTreeData.Create(FCandidateTree.NoOfOTUs, FCandidateTree.isBLen, FCandidateTree.isSE, FCandidateTree.isStats);
        FTimeTree.GetTreeData(aTree);
        if not ApplyTimeSmoothing(aTree) then
          raise Exception.Create('failed to apply correction to a non-ultrametric timetree');
      end
      else
        aTree := nil;
    except
      on E:Exception do
      begin
        Result := False;
        FLog.Add(E.Message);
      end;
    end;
  finally
    if Assigned(aTree) then
      aTree.Free;
  end;
end;

function TLittleBootstrapsTimetree.GetTabularOutputHeader: String;
var
  aFormatString: String;
begin
  GetNameFormatString;
  aFormatString := GetHeaderFormatString;
  if HasCalibrations then
    Result := Format(aFormatString, ['NodeLabel', 'NodeId', 'Des1', 'Des2', 'Reltime', 'StdErr', 'DivTime', 'CI-Lower', 'CI-Upper'])
  else
    Result := Format(aFormatString, ['NodeLabel', 'NodeId', 'Des1', 'Des2', 'Reltime', 'StdErr', 'CI-Lower', 'CI-Upper']);
end;

function TLittleBootstrapsTimetree.GetTabularOutputLine(nodeIndex: Integer): String;
var
  aFormatString: String = '';
  n: TSimpleTreeNode = nil;
  d: TMultiSampleNodeTimeData = nil;
  labelStr: String = '-';
begin
  n := FTimeTree.Nodes[nodeIndex];
  if (not n.IsOutgroupMember) and (not n.IsRoot) then
    d := TMultiSampleNodeTimeData(n.MyObject);

  aFormatString := GetLineFormatString(nodeIndex);

  if HasCalibrations then
  begin
    if nodeIndex < FTimeTree.NumTaxa then
    begin
      if n.IsOutgroupMember then
        Result := Format(aFormatString, [FOtuNames[nodeIndex] + OUTGROUP_MARKER, n.NodeIndex + 1, '-', '-', '-', '-', '-', '-', '-'])
      else
        Result := Format(aFormatString, [FOtuNames[nodeIndex], n.NodeIndex + 1, '-', '-', '-', '-', '-', '-', '-']);
    end
    else
    begin
      if FTimeTree.NumTaxa <= (MAX_LABEL_LENGTH - 4) then
        labelStr := Format('{ %s }', [n.Id]);
      if n.IsOutgroupMember or n.IsRoot then
        labelStr := labelStr + OUTGROUP_MARKER;
      if n.IsOutgroupMember or n.IsRoot then
        Result := Format(aFormatString, [labelStr, n.NodeIndex + 1, n.Des1.NodeIndex + 1, n.Des2.NodeIndex + 1, '-', '-', '-', '-', '-'])
      else
      begin
        Assert(Assigned(d), 'TNodeTimeData not initialized');
        Result := Format(aFormatString, [labelStr, n.NodeIndex + 1, n.Des1.NodeIndex + 1, n.Des2.NodeIndex + 1, d.Reltime, d.StdErr, d.DivTime, d.MinDivTime, d.MaxDivTime]);
      end;
    end;
  end
  else
  begin
    if nodeIndex < FTimeTree.NumTaxa then
    begin
      if n.IsOutgroupMember then
        Result := Format(aFormatString, [FOtuNames[nodeIndex] + OUTGROUP_MARKER, n.NodeIndex + 1, '-', '-', '-', '-', '-', '-'])
      else
        Result := Format(aFormatString, [FOtuNames[nodeIndex], n.NodeIndex + 1, '-', '-', '-', '-', '-', '-']);
    end
    else
    begin
      if FTimeTree.NumTaxa <= (MAX_LABEL_LENGTH - 4) then
        labelStr := Format('{ %s }', [n.Id]);
      if n.IsOutgroupMember or n.IsRoot then
        labelStr := labelStr + OUTGROUP_MARKER;
      if n.IsOutgroupMember or n.IsRoot then
        Result := Format(aFormatString, [labelStr, n.NodeIndex + 1, n.Des1.NodeIndex + 1, n.Des2.NodeIndex + 1, '-', '-', '-', '-'])
      else
      begin
        Assert(Assigned(d), 'TNodeTimeData not initialized');
        Result := Format(aFormatString, [labelStr, n.NodeIndex + 1, n.Des1.NodeIndex + 1, n.Des2.NodeIndex + 1, d.Reltime, d.StdErr, d.MinReltime, d.MaxReltime]);
      end;
    end;
  end;
end;

function TLittleBootstrapsTimetree.TabularOutputToStringList: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  Result.Add(GetTabularOutputHeader);
  for i := 0 to FTimeTree.NumNodes - 1 do
    Result.Add(GetTabularOutputLine(i));
  Result.Add(EmptyStr);
  Result.Add(ResultsFootnote);
end;

function TLittleBootstrapsTimetree.TabularOutputToFile(filename: String; sortByInputOrder: Boolean = False): Boolean;
var
  aFile: TextFile;
  i: Integer;
begin
  try
    try
      if sortByInputOrder then
        FTimeTree.SortByInputOrder;
      AssignFile(aFile, filename);
      Rewrite(aFile);
      WriteLn(aFile, GetTabularOutputHeader);
      for i := 0 to FTimeTree.NumNodes - 1 do
        WriteLn(aFile, GetTabularOutputLine(i));
      WriteLn(aFile);
      WriteLn(aFile, ResultsFootnote);
    except
      on E:Exception do
      begin
        FLog.Add('Error when writing tabular output to file');
        FLog.Add(E.Message);
      end;
    end;
  finally
    CloseFile(aFile);
  end;
  Result := FileExists(filename);
end;

function TLittleBootstrapsTimetree.ExportToNexusStrings: TStringList;
begin
  Result := ExportTimetreeToNexusStrings(FTimeTree, FOtuNames, HasCalibrations);
end;

function TLittleBootstrapsTimetree.ExportToNexusFile(Filename: String): Boolean;
var
  aList: TStringList = nil;
begin
  try
    aList := ExportToNexusStrings;
    aList.SaveToFile(filename);
    Result := FileExists(filename);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TLittleBootstrapsTimetree.NewickOutputToFile(const filename: String; const appendToFile: Boolean; var newickStr: String): Boolean;
var
  aList: TTreeList = nil;
  aData: TTreeData = nil;
  newick: String = '';
  aFile: TextFile;
begin
  try
    try
      AssignFile(aFile, filename);
      if appendToFile and FileExists(filename) then
        Append(aFile)
      else
        Rewrite(aFile);
      aList := TTreeList.Create;
      aList.OTUNameList.AddStrings(FOtuNames);
      aData := TTreeData.Create(FTimeTree.NumTaxa, True, True, False);
      FTimeTree.GetTreeData(aData);
      aList.Add(aData);
      newick := aList.OutputNewickTree(0, True, False, 0.0);
      WriteLn(aFile, newick);
      Result := FileExists(filename);
      newickStr := newick;
      NewickOutputToFileNoOutgroup(ChangeFileExt(filename, '_no_outgroup.nwk'), False);
    except
      on E:Exception do
      begin
        Result := False;
        FLog.Add(E.Message);
      end;
    end;
  finally
    CloseFile(aFile);
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TLittleBootstrapsTimetree.NewickOutputToFileNoOutgroup(const filename: String; const appendToFile: Boolean): Boolean;
var
  newick: String = '';
  aFile: TextFile;
begin
  try
    try
      AssignFile(aFile, filename);
      if appendToFile and FileExists(filename) then
        Append(aFile)
      else
        Rewrite(aFile);
      newick := GetNewickNoOutgroup(HasCalibrations);
      WriteLn(aFile, newick);
      Result := FileExists(filename);
    except
      on E:Exception do
      begin
        Result := False;
        FLog.Add(E.Message);
      end;
    end;
  finally
    CloseFile(aFile);
  end;
end;

function TLittleBootstrapsTimetree.ReltimeTreesToFile(filename: String; appendToFile: Boolean; comment: String = ''): Boolean;
begin
  Result := DivTimetreeToFile(filename, appendToFile, comment, 'divtimes');
  Result := Result and DivTimetreeToFile(filename, appendToFile, comment, 'ci-upper');
  Result := Result and DivTimetreeToFile(filename, appendToFile, comment, 'ci-lower');
end;

function TLittleBootstrapsTimetree.DebugTimeStringsToFile(filename: String; appendToFile: Boolean): Boolean;
var
  i: Integer;
  aFile: TextFile;
begin
  try
    try
      AssignFile(aFile, filename);
      if appendToFile then
        Append(aFile)
      else
        Rewrite(aFile);
      if FDebugTimeStrings.Count > 1 then
        for i := 0 to FDebugTimeStrings.Count - 2 do
          WriteLn(aFile, FDebugTimeStrings[i]);
    except
      on E:Exception do
        FLog.Add('failed to export time strings: ' + E.Message);
    end;
  finally
    CloseFile(aFile);
  end;
  Result := FileExists(filename);
end;

function TLittleBootstrapsTimetree.DebugVarianceStringsToFile(filename: String; appendToFile: Boolean): Boolean;
var
  i: Integer;
  aFile: TextFile;
begin
  try
    try
      AssignFile(aFile, filename);
      if appendToFile then
        Append(aFile)
      else
        Rewrite(aFile);
      if FDebugVarianceStrings.Count > 1 then
        for i := 0 to FDebugVarianceStrings.Count - 2 do
          WriteLn(aFile, FDebugVarianceStrings[i]);
    except
      on E:Exception do
        FLog.Add('failed to export time strings: ' + E.Message);
    end;
  finally
    CloseFile(aFile);
  end;
  Result := FileExists(filename);
end;

function TLittleBootstrapsTimetree.MaxTime(nodeIndex: Integer): Double;
var
  n: TSimpleTreeNode = nil;
  d: TMultiSampleNodeTimeData = nil;
begin
  n := FTimeTree.Nodes[nodeIndex];
  d := TMultiSampleNodeTimeData(n.MyObject);
  if Assigned(FCalibrations) then
    Result := d.MaxDivTime
  else
    Result := d.MaxReltime;
end;

function TLittleBootstrapsTimetree.MinTime(nodeIndex: Integer): Double;
var
  n: TSimpleTreeNode = nil;
  d: TMultiSampleNodeTimeData = nil;
begin
  n := FTimeTree.Nodes[nodeIndex];
  d := TMultiSampleNodeTimeData(n.MyObject);
  if Assigned(FCalibrations) then
    Result := d.MinDivTime
  else
    Result := d.MinReltime;
end;

function TLittleBootstrapsTimetree.DivTime(nodeIndex: Integer): Double;
var
  n: TSimpleTreeNode = nil;
  d: TMultiSampleNodeTimeData = nil;
begin
  n := FTimeTree.Nodes[nodeIndex];
  d := TMultiSampleNodeTimeData(n.MyObject);
  if Assigned(FCalibrations) then
    Result := d.DivTime
  else
    Result := d.Reltime;
end;

function TLittleBootstrapsTimetree.GetIngroupRootDivTime: Double;
var
  n: TSimpleTreeNode = nil;
  d: TMultiSampleNodeTimeData = nil;
begin
  n := FTimetree.Root;
  if n.Des2.IsOutgroupMember then
    n := n.Des1
  else
    n := n.Des2;
  d := TMultiSampleNodeTimeData(n.MyObject);
  if Assigned(FCalibrations) then
    Result := d.DivTime
  else
    Result := d.Reltime;
end;

procedure TLittleBootstrapsTimetree.ErrorBarHeight(nodeIndex: Integer; var errHigh: Double; var errLow: Double);
var
  n: TSimpleTreeNode = nil;
  d: TMultiSampleNodeTimeData = nil;
begin
  n := FTimeTree.Nodes[nodeIndex];
  d := TMultiSampleNodeTimeData(n.MyObject);
  errHigh := d.MaxReltime - d.Reltime;
  errLow := d.Reltime - d.MinReltime;
end;

procedure TLittleBootstrapsTimetree.GetTreeData(var aData: TTreeData);
begin
  if Assigned(FTimeTree) then
    FTimeTree.GetTreeData(aData)
  else
    raise Exception.Create('TLittleBootstrapsTimetree not initialized');
end;

end.

