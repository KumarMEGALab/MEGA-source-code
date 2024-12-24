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

unit mtabular_tree_export;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MTreeList, MTreeData, MSimpleTreeNode, MTreeDataAdapter;

type

  { TTabularTreeExport }

  TTabularTreeExport = class(TObject)
    private
      FTree: TSimpleTreeDataAdapter;
      FNodes: TSimpleTreeNodeArray;
      FTreeList: TTreeList;
      procedure Initialize(aData: TTreeData; aNames: TStringList; isRooted: Boolean);
    public
      constructor Create(aList: TTreeList);
      destructor Destroy; override;

      function GenerateTabularTreeExport(treeIndex: Integer; isBlen: Boolean): TStringList;
      function ExportTreeTabular(treeIndex: Integer; filename: String; isBlen: Boolean): Boolean;
  end;

implementation

uses
  MegaUtils;

{ TTabularTreeExport }

procedure TTabularTreeExport.Initialize(aData: TTreeData; aNames: TStringList; isRooted: Boolean);
begin
  if Assigned(FTree) then
    FreeAndNil(FTree);
  FTree := TSimpleTreeDataAdapter.Create;
  FTree.SetTreeData(aData, isRooted, aNames);
  FNodes := FTree.GetSimpleTreeReference;
end;

constructor TTabularTreeExport.Create(aList: TTreeList);
begin
  FTreeList :=  TTreeList.Create;
  FTreeList.Assign(aList);
end;

destructor TTabularTreeExport.Destroy;
begin
  if Assigned(FTree) then
    FTree.Free;
  if Assigned(FTreeList) then
    FTreeList.Free;
  inherited Destroy;
end;

function TTabularTreeExport.GenerateTabularTreeExport(treeIndex: Integer; isBLen: Boolean): TStringList;
const
  IdFormatString = '%6d';
  SIdFormatString = '%6s';
  FloatFormatString = '%16.6f';
  SFloatFormatString = '%16s';
var
  i: integer;
  TempString: AnsiString;
  OtuName: AnsiString;
  TaxonColumnWidth: Integer;
  NodeLabel: AnsiString;
  NoOfNames: Integer;
  NodeLabels: TStringList = nil;
  NamesList: TStringList = nil;
  aTree: TTreeData = nil;

  function GetInternalNodeLabel(TreeDataIndex: Integer): AnsiString;
  begin
    if (NodeLabels <> nil) and (TreeDataIndex < NodeLabels.Count) and (NodeLabels.Strings[TreeDataIndex] <> EmptyStr) then
      Result := NodeLabels.Strings[TreeDataIndex]
    else
      Result := '-';
  end;

begin
  aTree := FTreeList[treeIndex];
  Initialize(aTree, FTreeList.OTUNameList, FTreeList.isRooted);
  Result := TStringList.Create;
  if Assigned(FTreeList.InternalNodeLbls[treeIndex]) then
    NodeLabels := FTreeList.InternalNodeLbls[treeIndex];
  NamesList := FTreeList.OTUNameList;
  NoOfNames := NamesList.Count;

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
    TaxonColumnWidth := TaxonColumnWidth + 3;

  // Write out a header line that gives names for each colum
  TempString := MakePrettyTaxonNameString('NodeLabel', TaxonColumnWidth) + #9;
  TempString += Format(SIdFormatString, ['NodeId']) + #9;
  TempString += Format(SIdFormatString, ['Des1']) + #9;
  TempString += Format(SIdFormatString, ['Des2']) + #9;
  if isBLen then
    TempString += Format(SFloatFormatString, ['BLen']) + #9;
  if FtreeList[treeIndex].isStats and (FTreeList.MaxStats > 0) then
  begin
    TempString += Format(SFloatFormatString, ['Med BCL']) + #9;
    if FTreeList[treeIndex].isSE then
      TempString += Format(SFloatFormatString, ['SE(Med BCL)']) + #9;
    if aTree.HasMeanBcl then
      TempString += Format(SFloatFormatString, ['Avg BCL']) + #9;
  end;
  Result.Add(TempString);

  // Write out the information for the leaf nodes
  for i := 0 to NoOfNames-1 do
  begin
    OtuName := NamesList[i];
    OtuName := MakePrettyTaxonNameString(OtuName, TaxonColumnWidth);
    TempString := OtuName + #9; // Taxon name
    TempString += Format(IdFormatString, [FNodes[i].NodeIndex + 1]) + #9; // NodeId
    TempString += Format(SIdFormatString, ['-']) + #9; // Desc1
    TempString += Format(SIdFormatString, ['-']) + #9; // Desc2
    if isBlen then
      TempString += Format(FloatFormatString, [FNodes[i].BLen]) + #9;  // BLen
    if aTree.isStats and (FTreeList.MaxStats > 0) then
    begin
      TempString += Format(SFloatFormatString, ['-']) + #9;
      if FTreeList[treeIndex].isSE then
        TempString += Format(SFloatFormatString, ['-']) + #9;
      if aTree.HasMeanBcl then
        TempString += Format(SFloatFormatString, ['-']) + #9;
    end;
    Result.Add(TempString);
  end;

  // Write out the information for the internal nodes
  for i := NoOfNames to 2*NoOfNames-2 do
  begin
    NodeLabel := GetInternalNodeLabel(FNodes[i].NodeIndex);
    TempString := MakePrettyTaxonNameString(NodeLabel, TaxonColumnWidth) + #9;
    TempString += Format(IdFormatString, [FNodes[i].NodeIndex + 1]) + #9;
    TempString += Format(IdFormatString, [FNodes[i].des1.NodeIndex+1]) + #9;
    TempString += Format(IdFormatString, [FNodes[i].des2.NodeIndex+1]) + #9;
    if isBlen then
      TempString := TempString + Format(FloatFormatString, [FNodes[i].BLen]) + #9;
    if aTree.isStats and (FTreeList.MaxStats > 0) then
    begin
      TempString += Format(FloatFormatString, [FNodes[i].Value/FTreeList.MaxStats*100]) + #9;
      if FTreeList[treeIndex].isSE then
        TempString += Format(FloatFormatString, [FNodes[i].StdError]) + #9;
      if aTree.HasMeanBcl then
        TempString += Format(FloatFormatString, [FNodes[i].Value2]) + #9;
    end;
    Result.Add(TempString);
  end;
end;

function TTabularTreeExport.ExportTreeTabular(treeIndex: Integer;filename: String; isBLen: Boolean): Boolean;
var
  aList: TStringList = nil;
begin
  try
    aList := GenerateTabularTreeExport(treeIndex, isBlen);
    aList.SaveToFile(filename);
    Result := FileExists(filename);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

end.

