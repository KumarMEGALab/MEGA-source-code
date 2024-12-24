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

unit mcompare_bootstrap_trees;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MTreeList, MTreeData, MTreeDataAdapter;

type

  { TCompareBootstrapTrees }

  TCompareBootstrapTrees = class(TObject)
    private
      FFirstTree: String;
      FSecondTree: String;
      FTreeList1: TTreeList;
      FTreeList2: TTreeList;
      FLog: TStringList;
      function GetLogText: String;
      function DoComparison: Boolean;
      function LengthOfLongestName: Integer;
    public
      constructor Create(firstTree, secondTree: String);
      destructor Destroy; override;
      function Execute: Boolean;
      property LogText: String read GetLogText;
  end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ENDIF}
  MSimpleTreeNode;

{ TCompareBootstrapTrees }

function TCompareBootstrapTrees.GetLogText: String;
begin
  if Assigned(FLog) then
    Result := FLog.Text
  else
    Result := EmptyStr;
end;

function TCompareBootstrapTrees.DoComparison: Boolean;
var
  adptr1: TSimpleTreeDataAdapter = nil;
  adptr2: TSimpleTreeDataAdapter = nil;
  aFile: TextFile;
  i: Integer;
  formatStr: String;
  longest: Integer;
  n1, n2: TSimpleTreeNodeArray;
  filename: String;
begin
  Result := False;
  longest := LengthOfLongestName;
  formatStr := '%-5d %-5d %-5d %-' + IntToStr(longest) + 's %d %d';
  try
    filename := NextAvailableFilenameNV('.txt');
    AssignFile(aFile, filename);
    Rewrite(aFile);
    adptr1 := TSimpleTreeDataAdapter.Create;
    adptr2 := TSimpleTreeDataAdapter.Create;
    adptr1.SetTreeData(FTreeList1[0], FTreeList1.isRooted, FTreeList1.OTUNameList);
    adptr2.SetTreeData(FTreeList2[0], FTreeList1.isRooted, FTreeList2.OTUNameList);
    n1 := adptr1.Nodes;
    n2 := adptr2.Nodes;
    WriteLn(aFile, Format('%-5s %-5s %-5s %-' + IntToStr(longest) + 's %-5s %-5s', ['Id', 'Des1', 'Des2', 'Name', 'BCL1', 'BCL2']));
    for i := 0 to adptr1.NumNodes - 2 do
    begin
      if i < adptr1.NumTaxa then
      begin
        if (n1[i].NodeIndex <> n2[i].NodeIndex) or (n1[i].SequenceName <> n2[i].SequenceName) then
          raise Exception.Create('to compare bootstrap trees, they must have the same topology');
        WriteLn(aFile, Format(formatStr, [n1[i].NodeIndex, -1, -1, n1[i].SequenceName, Round(n1[i].Value), Round(n2[i].Value)]));
      end;
      if i >= adptr1.NumTaxa then
      begin
        if (n1[i].NodeIndex <> n2[i].NodeIndex) or (n1[i].Des1.NodeIndex <> n2[i].Des1.NodeIndex) or (n1[i].Des2.NodeIndex <> n2[i].Des2.NodeIndex) then
          raise Exception.Create('to compare bootstrap trees, they must have the same topology');
        WriteLn(aFile, Format(formatStr, [n1[i].NodeIndex, n1[i].Des1.NodeIndex, n1[i].Des2.NodeIndex, n1[i].SequenceName, Round(n1[i].Value*100), Round(n2[i].Value*100)]));
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
end;

function TCompareBootstrapTrees.LengthOfLongestName: Integer;
var
  i: Integer;
begin
  Result := 0;
  if Assigned(FTreeList1) and (FTreeList1.OTUNameList.Count > 0) then
    for i := 0 to FTreeList1.OTUNameList.Count - 1 do
      if Length(FTreeList1.OTUName[i]) > Result then
        Result := Length(FTreeList1.OTUName[i]);
end;

constructor TCompareBootstrapTrees.Create(firstTree, secondTree: String);
begin
  FTreeList1 := TTreeList.Create;
  FTreeList2 := TTreeList.Create;
  FLog := TStringList.Create;
  FFirstTree := firstTree;
  FSecondTree := secondTree;
end;

destructor TCompareBootstrapTrees.Destroy;
begin
  if Assigned(FTreeList1) then
    FTreeList1.Free;
  if Assigned(FTreeList2) then
    FTreeList2.Free;
  if Assigned(FLog) then
    FLog.Free;
  inherited Destroy;
end;

function TCompareBootstrapTrees.Execute: Boolean;
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
      if not FTreeList2.ImportFromNewickFile(FSecondTree, nil) then
        raise Exception.Create('failed to import second tree file: ' + FSecondTree);
      if not DoComparison then
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

end.

