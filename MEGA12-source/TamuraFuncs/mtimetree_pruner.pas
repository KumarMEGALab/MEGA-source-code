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

unit mtimetree_pruner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MTreeData, MTreeList, MTreeDataAdapter, MD_InputSeqData;

type

  { TMegaTreePruner }

  TMegaTreePruner = class(TObject)
    protected
      FLog: TStringList;
      FTreeList: TTreeList;
      FAlignmentNames: TStringList;
      FOtuNamesToPrune: TStringList;
      function LoadOutgroupsFromFile(groupsFile: String): Boolean;
      function LoadOutgroupsFromList(groupsList: TStringList): Boolean;
      procedure MakeListOfNamesToPrune;
    public
      constructor CreateFromInputFiles(alignmentFile: String; treeFile: String; groupsFile: String);
      constructor CreateFromTreeList(treeList: TTreeList; alignmentOtuNames: TStringList; outgroupList: TStringList);
      destructor Destroy; override;
      function PruneTree(needsOutgroup: Boolean): Boolean;
      function TreeNeedsPruned: Boolean;
      function GetPrunedTree: TTreeData;
      function GetPrunedTreeList: TTreeList;
      function GetLogText: String;
  end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ENDIF}
  ProcessInputData, MegaUtils;

{ TMegaTreePruner }

function TMegaTreePruner.LoadOutgroupsFromFile(groupsFile: String): Boolean;
var
  aGroupsList: TStringList = nil;
begin
  Result := False;

  try
    try
      if not FileExists(groupsFile) then
        raise Exception.Create('The specified groups file (' + groupsFile + ') was not found');

      aGroupsList := TStringList.Create;
      aGroupsList.LoadFromFile(groupsFile);
      Result := LoadOutgroupsFromList(aGroupsList);
    except
      on E:Exception do
      begin
        FLog.Add('Error when loading the groups list from file: ' + E.Message);
        Result := False
      end;
    end;
  finally
    if Assigned(aGroupsList) then
      aGroupsList.Free;
  end;
end;

function TMegaTreePruner.LoadOutgroupsFromList(groupsList: TStringList): Boolean;
var
  i: Integer = -1;
  aTaxon: String = '';
  aGroup: String = '';
  outgroupMembersInTree: TStringList = nil;
begin
  Result := False;

  try
    try
      outgroupMembersInTree := TStringList.Create;
      if groupsList.Count > 0 then
        for i := 0 to groupsList.Count - 1 do
        begin
          if Trim(groupsList[i]) = EmptyStr then
            continue;
          aTaxon := groupsList.Names[i];
          TrimTaxaName2(aTaxon);
          aGroup := groupsList.ValueFromIndex[i];
          TrimTaxaName2(aGroup);
          if (aTaxon = EmptyStr) or (aGroup = EmptyStr) then
            raise Exception.Create('Invalid aGroup specification: ' + groupsList.Names[i] + '=' + groupsList.ValueFromIndex[i]);

          if SameText(aGroup, 'outgroup') then
          begin
            if FTreeList.HasOtu(aTaxon) then
            begin
              Result := True;
              outgroupMembersInTree.Add(aTaxon);
            end;
          end;
        end;

      if not Result then
        raise Exception.Create('no outgroup members found in groups file');
      FTreeList.SetOutgroupMembers(outgroupMembersInTree);
      if FTreeList.NumOutgroupMembers(0) = 0 then
        raise Exception.Create('failed to define an outgroup for the input tree');
    except
      on E:Exception do
      begin
        FLog.Add('Error when loading the groups list: ' + E.Message);
        Result := False
      end;
    end;
  finally
    if Assigned(outgroupMembersInTree) then
      outgroupMembersInTree.Free;
  end;
end;

procedure TMegaTreePruner.MakeListOfNamesToPrune;
var
  i: Integer = -1;
  index: Integer = -1;
  aName: String = '';
  tempNames: TStringList = nil;
begin
  try
    tempNames := TStringList.Create;
    tempNames.Assign(FTreeList.OTUNameList);
    if Assigned(FOtuNamesToPrune) then
      FOtuNamesToPrune.Clear
    else
      FOtuNamesToPrune := TStringList.Create;
    for i := 0 to tempNames.Count - 1 do
    begin
      aName := tempNames[i];
      index := FAlignmentNames.IndexOf(aName);
      if index = -1 then
        FOtuNamesToPrune.Add(aName);
    end;
    if FOtuNamesToPrune.Count = 0 then
      FLog.Add('Zero names were found to be missing in the input tree');
  finally
    if Assigned(tempNames) then
      tempNames.Free;
  end;
end;

constructor TMegaTreePruner.CreateFromInputFiles(alignmentFile: String; treeFile: String; groupsFile: String);
begin
  FLog := TStringList.Create;
  if not DoOpenDataFile(alignmentFile, False, nil) then
    raise Exception.Create('failed to load the input alignment file');
  FAlignmentNames := D_InputSeqData.GetTaxaNamesList;
  FTreeList := TTreeList.Create;
  if not FTreeList.ImportFromNewickFile(treeFile, nil) then
    raise Exception.Create('failed to load the input tree file');
  if not LoadOutgroupsFromFile(groupsFile) then
  begin
    if FLog.Text <> EmptyStr then
      raise Exception.Create(FLog.Text)
    else
      raise Exception.Create('Failed to load the groups file');
  end;
end;

constructor TMegaTreePruner.CreateFromTreeList(treeList: TTreeList; alignmentOtuNames: TStringList; outgroupList: TStringList);
begin
  FLog := TStringList.Create;
  FTreeList := TTreeList.Create;
  FTreeList.Assign(treeList);
  FAlignmentNames := TStringList.Create;
  FAlignmentNames.Assign(alignmentOtuNames);
  if not LoadOutgroupsFromList(outgroupList) then
  begin
    if FLog.Text <> EmptyStr then
      raise Exception.Create(FLog.Text)
    else
      raise Exception.Create('Failed to load the groups file');
  end;
end;

destructor TMegaTreePruner.Destroy;
begin
  if Assigned(FLog) then
    FLog.Free;
  if Assigned(FTreeList) then
    FTreeList.Free;
  if Assigned(FAlignmentNames) then
    FAlignmentNames.Free;
  if Assigned(FOtuNamesToPrune) then
    FOtuNamesToPrune.Free;
  inherited Destroy;
end;

function TMegaTreePruner.PruneTree(needsOutgroup: Boolean): Boolean;
var
  aData: TTreeData = nil;
  rootedTree: TTreeData = nil;
  i: Integer = -1;
  aName: String = '';
  aMsg: String = '';
begin
  try
    Result := False;
    aData := FTreeList[0];
    RootTreeDataOnOutgroup(aData);
    rootedTree := TTreeData.Create(aData.NoOfOTUs, aData.isBLen, aData.isSE, aData.isStats);
    rootedTree.Assign(aData);
    FTreeList.DeleteAll;
    FTreeList.isRooted := True;
    FTreeList.Add(rootedTree);

    if needsOutgroup and (FTreeList.NumOutgroupMembers(0) = 0) then
      raise Exception.Create('Prune timetree analysis requires that outgroup taxa in the tree be specified');

    MakeListOfNamesToPrune;
    if FOtuNamesToPrune.Count > 0 then
    begin
      for i := 0 to FOtuNamesToPrune.Count - 1 do
      begin
        aName := FOtuNamesToPrune[i];
        if not FTreeList.RemoveOtu(aName, True, aMsg) then
          raise Exception.Create(aMsg);
        {$IFNDEF VISUAL_BUILD}
        warn_nv(Format('"%s" was removed from the tree', [aName]));
        {$ENDIF}
        if needsOutgroup and (FTreeList.NumOutgroupMembers(0) = 0) then
          raise Exception.Create('pruning unused taxa caused the outgroup to be removed');
      end;
    end
    else
      FLog.Add('no taxa need to be pruned, nothing to do');
    Result := True;
  except
    on E:Exception do
    begin
      FLog.Add(E.Message);
      Result := False;
    end;
  end;
end;

function TMegaTreePruner.TreeNeedsPruned: Boolean;
var
  i: Integer = -1;
begin
  Result := True;
  if FTreeList.NoOfOTUs > FAlignmentNames.Count then
  begin
    {$IFNDEF VISUAL_BUILD}
    warn_NV(Format('Input tree needs pruned as it has %d tips but there are only %d sequences in the input alignment', [FTreeList.NoOfOTUs, FAlignmentNames.Count]));
    {$ENDIF}
    Exit;
  end;
  for i := 0 to FTreeList.NoOfOTUs - 1 do
  begin
    if FAlignmentNames.IndexOf(FTreeList.OTUName[i]) = -1 then
    begin
      {$IFNDEF VISUAL_BUILD}
      warn_NV(Format('Input tree needs pruned - "%s" not found in the input alignment', [FTreeList.OTUName[i]]));
      {$ENDIF}
      Exit;
    end;
  end;
  Result := False;
end;

function TMegaTreePruner.GetPrunedTree: TTreeData;
begin
  if Assigned(FTreeList) and (FTreeList.Count = 1) then
  begin
    Result := TTreeData.Create(FTreeList.NoOfOTUs, FTreeList.isBLen, FTreeList.isSE, FTreeList.isStats);
    Result.Assign(FTreeList[0]);
  end
  else
    Result := nil;
end;

function TMegaTreePruner.GetPrunedTreeList: TTreeList;
begin
  if Assigned(FTreeList) then
  begin
    Result := TTreeList.Create;
    Result.Assign(FTreeList);
  end
  else
    Result := nil;
end;

function TMegaTreePruner.GetLogText: String;
begin
  if Assigned(FLog) then
    Result := FLog.Text
  else
    Result := EmptyStr;
end;

end.

