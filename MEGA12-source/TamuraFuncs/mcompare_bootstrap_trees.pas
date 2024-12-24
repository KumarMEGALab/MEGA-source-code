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

unit mcompare_bootstrap_trees;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MTreeList, MTreeDataAdapter, MPartitionList, MTreeData;

type

  { TCompareTrees }

  TCompareTrees = class(TObject)
    private
      FOutgroupTaxa: TStringList;
      FSeqDataFile: String;
      FFirstTree: String;
      FSecondTree: String;
      FTreeList1: TTreeList;
      FTreeList2: TTreeList;
      FLog: TStringList;
      function GetLogText: String;
      function DoCompareBootstrapSupport: Boolean;

      function LengthOfLongestName: Integer;
      function GetPartitionStrings(aData: TTreeData; isRooted: Boolean): TStringList;
    public
      constructor Create(firstTree, secondTree: String);
      constructor CreateFromTreeFiles(firstTreesFile: String; secondTreesFile: String);

      destructor Destroy; override;
      function CompareBootstrapSupport: Boolean;
      function CompareNodeHeights(seqData: String): Boolean;
      function CompareTopologies(outFile: String; var areSame: Boolean): Boolean;
      procedure AddOutgroupTaxon(aTaxon: String);
      property LogText: String read GetLogText;
  end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ENDIF}
  MSimpleTreeNode, MD_InputSeqData, ProcessInputData, MTreeProc;

{ TCompareTrees }

function TCompareTrees.GetLogText: String;
begin
  if Assigned(FLog) then
    Result := FLog.Text
  else
    Result := EmptyStr;
end;

function TCompareTrees.DoCompareBootstrapSupport: Boolean;
var
  adptr1: TSimpleTreeDataAdapter = nil;
  adptr2: TSimpleTreeDataAdapter = nil;
  aFile: TextFile;
  i: Integer;
  formatStr: String = '';
  n1, n2: TSimpleTreeNodeArray;
  node1: TSimpleTreeNode = nil;
  node2: TSimpleTreeNode = nil;
  filename: String = '';
  idStr: String = '';
begin
  Result := False;
  {$IFNDEF VISUAL_BUILD}
  formatStr := '%d, %d, %d, %s, %d, %d, %.4f, %.4f, %s, %s';
  try
    filename := NextAvailableFilenameNV('.csv');
    AssignFile(aFile, filename);
    Rewrite(aFile);
    adptr1 := TSimpleTreeDataAdapter.Create;
    adptr2 := TSimpleTreeDataAdapter.Create;
    adptr1.SetTreeData(FTreeList1[0], FTreeList1.isRooted, FTreeList1.OTUNameList);
    adptr1.SortByInputOrder;
    adptr1.SetPartitions;
    adptr2.SetTreeData(FTreeList2[0], FTreeList1.isRooted, FTreeList2.OTUNameList);
    adptr2.SortByInputOrder;
    adptr2.SetPartitions;
    n1 := adptr1.Nodes;
    n2 := adptr2.Nodes;
    WriteLn(aFile, Format('%s, %s, %s, %s, %s, %s, %s, %s, %s, %s', ['Id', 'Des1', 'Des2', 'Name', 'BCL1', 'BCL2', 'BLen1', 'Blen2', 'SBL1', 'SBL2']));
    for i := 0 to adptr1.NumNodes - 2 do
    begin
      node1 := n1[i];
      node2 := adptr2.GetNodeByPartitionId(node1.Id);
      if not Assigned(node2) then
        node2 := adptr2.GetNodeByPartitionId(node1.ComplementId);
      if Assigned(node2) then
        node2.Marked := True;
      if i < adptr1.NumTaxa then
      begin
        if i = 0 then
        begin
          if Assigned(node2) then
            WriteLn(aFile, Format(formatStr, [node1.NodeIndex, -1, -1, node1.SequenceName, Round(node1.Value), Round(node2.Value), node1.BLen, node2.BLen, Format('%.4f', [adptr1.SumOfBlens]), Format('%.4f', [adptr2.SumOfBlens])]))
          else
            WriteLn(aFile, Format(formatStr, [node1.NodeIndex, -1, -1, node1.SequenceName, Round(node1.Value), -1, node1.BLen, -1, Format('%.4f', [adptr1.SumOfBlens]), Format('%.4f', [adptr2.SumOfBlens])]));
        end
        else
        begin
          if Assigned(node2) then
            WriteLn(aFile, Format(formatStr, [node1.NodeIndex, -1, -1, node1.SequenceName, Round(node1.Value), Round(node2.Value), node1.BLen, node2.BLen, EmptyStr, EmptyStr]))
          else
            WriteLn(aFile, Format(formatStr, [node1.NodeIndex, -1, -1, node1.SequenceName, Round(node1.Value), -1, node1.BLen, -1, EmptyStr, EmptyStr]));
        end;
      end;
      if i >= adptr1.NumTaxa then
      begin
        if node1.Ancestor = adptr1.Root then
          idStr := Format('{ %s - root child }', [node1.Id])
        else
          idStr := Format('{ %s }', [node1.Id]);
        if Assigned(node2) then
          WriteLn(aFile, Format(formatStr, [node1.NodeIndex, node1.Des1.NodeIndex, node1.Des2.NodeIndex, idStr, Round(node1.Value*100), Round(node2.Value*100), node1.BLen, node2.BLen, EmptyStr, EmptyStr]))
        else
          WriteLn(aFile, Format(formatStr, [node1.NodeIndex, node1.Des1.NodeIndex, node1.Des2.NodeIndex, idStr, Round(node1.Value*100), -1, node1.BLen, -1.0, EmptyStr, EmptyStr]))
      end;
    end;
    if adptr2.HasUnmarkedNodes then
    begin
      WriteLn(aFile, 'nodes in tree2 not found in tree1:');
      for i := 0 to adptr2.NumNodes - 2 do
      begin
        node2 := n2[i];
        if node2.Ancestor = adptr2.Root then
          idStr := Format('{ %s - root child }', [node2.Id])
        else
          idStr := Format('{ %s }', [node2.Id]);
        if not node2.Marked then
          WriteLn(aFile, Format(formatStr, [node2.NodeIndex, node2.Des2.NodeIndex, node2.Des2.NodeIndex, idStr, -1, Round(node2.Value*100), -1.0, node2.BLen, EmptyStr, EmptyStr]));
      end;
    end;
  finally
    CloseFile(aFile);
    if Assigned(adptr1) then
      adptr1.Free;
    if Assigned(adptr2) then
      adptr2.Free;
  end;
  Result := FileExists(filename);
  {$ENDIF}
end;

function TCompareTrees.LengthOfLongestName: Integer;
var
  i: Integer;
begin
  Result := 0;
  if Assigned(FTreeList1) and (FTreeList1.OTUNameList.Count > 0) then
    for i := 0 to FTreeList1.OTUNameList.Count - 1 do
      if Length(FTreeList1.OTUName[i]) > Result then
        Result := Length(FTreeList1.OTUName[i]);
end;

function TCompareTrees.GetPartitionStrings(aData: TTreeData; isRooted: Boolean): TStringList;
var
  p: TPartitionList = nil;
begin
  try
    p := TPartitionList.Create(aData.NoOfOTUs, 0, isRooted, False);
    p.Add(aData.NodeArray, 0);
    Result := p.GetPartitionStringsForTreeData(aData);
    Result.Sort;
  finally
    if Assigned(p) then
      p.Free;
  end;
end;

constructor TCompareTrees.Create(firstTree, secondTree: String);
begin
  FSeqDataFile := EmptyStr;
  FTreeList1 := TTreeList.Create;
  FTreeList2 := TTreeList.Create;
  FLog := TStringList.Create;
  FFirstTree := firstTree;
  FSecondTree := secondTree;
  FOutgroupTaxa := TStringList.Create;
end;

constructor TCompareTrees.CreateFromTreeFiles(firstTreesFile: String; secondTreesFile: String);
begin
  FFirstTree := FirstTreesFile;
  FSecondTree := SecondTreesFile;
  FTreeList1 := TTreeList.Create;
  FTreeList2 := TTreeList.Create;
  FLog := TStringList.Create;
  FSeqDataFile := EmptyStr;
  FOutgroupTaxa := TStringList.Create;
end;

destructor TCompareTrees.Destroy;
begin
  if Assigned(FTreeList1) then
    FTreeList1.Free;
  if Assigned(FTreeList2) then
    FTreeList2.Free;
  if Assigned(FLog) then
    FLog.Free;
  inherited Destroy;
end;

function TCompareTrees.CompareBootstrapSupport: Boolean;
var
  namesList: TStringList = nil;
begin
  Result := False;
  try
    try
      if not FileExists(FFirstTree) then
        raise Exception.Create('missing first tree file');
      if not FileExists(FSecondTree) then
        raise Exception.Create('missing second tree file');
      if not FTreeList1.ImportFromNewickFile(FFirstTree, nil) then
        raise Exception.Create('failed to import first tree file: ' + FFirstTree);
      namesList := TStringList.Create;
      namesList.AddStrings(FTreeList1.OTUNameList);
      FTreeList1.Clear;
      if not FTreeList1.ImportFromNewickFile(FFirstTree, namesList) then
        raise Exception.Create('failed to import first tree file (2): ' + FFirstTree);
      if not FTreeList2.ImportFromNewickFile(FSecondTree, namesList) then
        raise Exception.Create('failed to import second tree file: ' + FSecondTree);
      if not DoCompareBootstrapSupport then
        raise Exception.Create('comparison of bootstrap trees failed');
      Result := True;
    except
      on E:Exception do
      begin
        FLog.Add(E.Message);
        {$IFDEF VISUAL_BUILD}
        raise E;
        {$ELSE}
        error_nv('failed to compare bootstrap trees', E);
        {$ENDIF}
      end;
    end;
  finally

  end;
end;

function TCompareTrees.CompareNodeHeights(seqData: String): Boolean;
var
  adptr1: TSimpleTreeDataAdapter = nil;
  adptr2: TSimpleTreeDataAdapter = nil;
  aFile: TextFile;
  i: Integer;
  formatStr: String;
  longest: Integer;
  n1, n2: TSimpleTreeNodeArray;
  filename: String;
  otuNames: TStringList = nil;
begin
  FSeqDataFile := seqData;
  Result := False;

  {$IFNDEF VISUAL_BUILD}
  try
    DoOpenDataFile(FSeqDataFile, False);
    otuNames := D_InputSeqData.GetTaxaNamesList;
    if not FileExists(FFirstTree) then
      raise Exception.Create('missing first tree file');
    if not FileExists(FSecondTree) then
      raise Exception.Create('missing second tree file');
    if not FTreeList1.ImportFromNewickFile(FFirstTree, otuNames) then
      raise Exception.Create('failed to import first tree file: ' + FFirstTree);
    if not FTreeList2.ImportFromNewickFile(FSecondTree, otuNames) then
      raise Exception.Create('failed to import second tree file: ' + FSecondTree);
    if FOutgroupTaxa.Count > 0 then
    begin
      FTreeList1.SetOutgroupMembers(FOutgroupTaxa);
      FTreeList2.SetOutgroupMembers(FOutgroupTaxa);
    end;
    longest := LengthOfLongestName;
    formatStr := '%-5d %-5d %-5d %-' + IntToStr(longest) + 's %.4f %.4f';
    filename := NextAvailableFilenameNV('_timing.txt');
    AssignFile(aFile, filename);
    Rewrite(aFile);
    adptr1 := TSimpleTreeDataAdapter.Create;
    adptr2 := TSimpleTreeDataAdapter.Create;
    adptr1.SetTreeData(FTreeList1[0], FTreeList1.isRooted, FTreeList1.OTUNameList);
    adptr2.SetTreeData(FTreeList2[0], FTreeList1.isRooted, FTreeList2.OTUNameList);
    adptr1.FindDivergenceTimes(False);
    adptr2.FindDivergenceTimes(False);
    n1 := adptr1.Nodes;
    n2 := adptr2.Nodes;
    WriteLn(aFile, Format('%-5s %-5s %-5s %-' + IntToStr(longest) + 's %-5s %-5s', ['Id', 'Des1', 'Des2', 'Name', 'Time', 'Time']));
    Assert(adptr1.NumNodes = adptr2.NumNodes, 'Application Error: comparing node heights of trees with differing number of nodes');
    for i := 0 to adptr1.NumNodes - 1 do
    begin
      if i < adptr1.NumTaxa then
      begin
        if (n1[i].NodeIndex <> n2[i].NodeIndex) or (n1[i].SequenceName <> n2[i].SequenceName) then
          raise Exception.Create('to compare trees, they must have the same topology');
        WriteLn(aFile, Format(formatStr, [n1[i].NodeIndex + 1, -1, -1, n1[i].SequenceName, n1[i].Value, n2[i].Value]));
      end;
      if i >= adptr1.NumTaxa then
      begin
        Assert(Assigned(n1[i]), 'n1 is nil');
        Assert(Assigned(n1[i].Des1), 'n1.Des1 is nil');
        Assert(Assigned(n1[i].Des2), 'n1.Des2 is nil');
        Assert(Assigned(n2[i]), 'n2 is nil');
        Assert(Assigned(n2[i].Des1), 'n2.Des1 is nil');
        Assert(Assigned(n2[i].Des2), 'n2.Des2 is nil');

        if (n1[i].NodeIndex <> n2[i].NodeIndex) or (n1[i].Des1.NodeIndex <> n2[i].Des1.NodeIndex) or (n1[i].Des2.NodeIndex <> n2[i].Des2.NodeIndex) then
          raise Exception.Create('to compare trees, they must have the same topology');
        WriteLn(aFile, Format(formatStr, [n1[i].NodeIndex + 1, n1[i].Des1.NodeIndex + 1, n1[i].Des2.NodeIndex + 1, n1[i].SequenceName, n1[i].Value, n2[i].Value]));
      end;
    end;
  finally
    CloseFile(aFile);
    if Assigned(adptr1) then
      adptr1.Free;
    if Assigned(adptr2) then
      adptr2.Free;
    if Assigned(otuNames) then
      otuNames.Free;
  end;
  Result := FileExists(filename);
  {$ENDIF}
end;

function TCompareTrees.CompareTopologies(outFile: String; var areSame: Boolean): Boolean;
var
  partitions: TPartitionList = nil;
  aFile: TextFile;
  names1: TStringList = nil;
  names2: TStringList = nil;
  i: Integer = -1;
  data1: TTreeData = nil;
  data2: TTreeData = nil;
  namesFinder: TTreeList = nil;
  otuNames: TStringList = nil;
  pStrings1: TStringList = nil;
  pStrings2: TStringList = nil;
begin
  try
    try
      areSame := True;
      namesFinder := TTreeList.Create; { first get a list of all names in the first tree so that we can force data structures to keep the taxa names in the same order}
      if not namesFinder.ImportFromNewickFile(FFirstTree, nil, False) then
        raise Exception.Create('failed to import first tree when retrieving otu names');
      otuNames := namesFinder.OTUNameList;
      if not FTreeList1.ImportFromNewickFile(FFirstTree, otuNames, False) then
        raise Exception.Create('failed to import first tree file: ' + FFirstTree);
      if not FTreeList2.ImportFromNewickFile(FSecondTree, otuNames, False) then
        raise Exception.Create('failed to import second tree file: ' + FSecondTree);

      if FTreeList1.NoOfOTUs <> FTreeList2.NoOfOTUs then
        areSame := False;
      if FTreeList1.Count <> FTreeList2.Count then
        areSame := False;
      if areSame then
      begin
        names1 := FTreeList1.OTUNameList;
        names2 := FTreeList2.OTUNameList;
        for i := 0 to names1.Count - 1 do
          if names2.IndexOf(names1[i]) < 0 then
          begin
            areSame := False;
            break;
          end;
      end;

      if areSame then
      begin
        for i := 0 to FTreeList1.Count - 1 do
        begin
          data1 := FTreeList1[i];
          data2 := FTreeList2[i];
          if Assigned(partitions) then
            FreeAndNil(partitions);
          if FTreeList1.isRooted or FTreeList2.isRooted then
          begin
            data1.isBLen := False;
            data2.isBLen := False;
            areSame := data1.Identical(data2);
          end
          else
          begin
            partitions := TPartitionList.Create(data1.NoOfOTUs, 0, FTreeList1.isRooted, False);
            partitions.Add(data1.NodeArray, 0);
            areSame := (not partitions.AddIfNewTree(data2.NodeArray, 0, 1));
            if not areSame then
            begin
              pStrings1 := GetPartitionStrings(data1, FTreeList1.isRooted);
              pStrings2 := GetPartitionStrings(data2, FTreeList2.isRooted);
              areSame := (pStrings1.Text = pStrings2.Text);
            end;
          end;

          if not areSame then
            break;
        end;
      end;

      if outFile <> EmptyStr then
      begin
        AssignFile(aFile, outFile);
        Rewrite(aFile);
        WriteLn(aFile, Format('%s', [BoolToStr(areSame, True)]));
        Result := FileExists(outFile);
      end
      else
        Result := True;
    except
      on E:Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        raise Exception.Create('failed to compare tree topologies: ' + E.Message);
        {$ELSE}
        Result := False;
        areSame := False;
        AssignFile(aFile, outFile);
        Rewrite(aFile);
        WriteLn(aFile, Format('%s', [BoolToStr(areSame, True)]));
        warn_NV('could not compare topologies: ' + E.Message);
        {$ENDIF}
      end;
    end;
  finally
    if Assigned(partitions) then
      partitions.Free;
    if Assigned(namesFinder) then
      namesFinder.Free;
    if Assigned(pStrings1) then
      pStrings1.Free;
    if Assigned(pStrings2) then
      pStrings2.Free;
    if outFile <> EmptyStr then
      CloseFile(aFile);
  end;
end;

procedure TCompareTrees.AddOutgroupTaxon(aTaxon: String);
begin
  FOutgroupTaxa.Add(aTaxon);
end;

end.

