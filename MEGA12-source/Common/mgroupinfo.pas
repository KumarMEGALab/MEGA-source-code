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

unit mgroupinfo;

interface

uses
  Classes, SysUtils, MTreeList,
  MTreeData, MegaConsts;

type

  { TGroupInfo }

  TGroupInfo = class(TObject)
    private
      function GetGroupNames: TStringList;
      procedure SetGroupNames(const Value: TStringList);
      procedure SetTreeList(const Value: TTreeList);
      function GetIsOutgroupMember(Index: Integer): Boolean;
      procedure SetIsOutgroupMember(Index: Integer; const Value: Boolean);
      procedure ClearOutgroup;
      procedure RefreshGroupNamesList;
    procedure SetUsedOtuInfos(const Value: TList);
    protected
      FGroupNames: TStringList;
      FTreeList: TTreeList;
      FUsedOtuInfos: TList;
      function IsOutgroup(aOtuName: String): Boolean; overload;
      function IsOutgroup(Index: Integer): Boolean; overload;
    public
      constructor Create(aTreeList: TTreeList);
      destructor Destroy; override;
      procedure Assign(Source: TGroupInfo);
      function NumTaxa: Integer;
      function GetOutgroupMembers: TBoolArray;
      procedure UpdateOutgroupTaxa(OutgroupNames: TStringList);
      procedure SetTreeOutgroupFromOtuInfos;
      procedure SetTreeOutgroupFromGroupsFile(aFilename: String);
      function UpdateGroupNames(GroupNames: TStringList): Boolean;

      property UsedOtuInfos: TList read FUsedOtuInfos write SetUsedOtuInfos;
      property GroupNames: TStringList read GetGroupNames write SetGroupNames;
      property TreeList: TTreeList write SetTreeList;
      property IsOutgroupMember[Index: Integer]: Boolean read GetIsOutgroupMember write SetIsOutgroupMember;
  end;

implementation

uses
  MegaUtils, MOtuInfo;

{ TGroupInfo }

procedure TGroupInfo.Assign(Source: TGroupInfo);
begin
  if Assigned(Source.FTreeList) then
  begin
    if not Assigned(FTreeList) then
      FTreeList := TTreeList.Create;
    FTreeList.Assign(Source.FTreeList);
  end
  else
    FTreeList := nil;
  FGroupNames.Assign(Source.FGroupNames);
  FUsedOtuInfos := Source.FUsedOtuInfos;
end;

procedure TGroupInfo.ClearOutgroup;
var
  i: Integer;
  aInfo: TOtuInfo;
begin
  if Assigned(FTreeList) then
    FTreeList.ClearOutgroup;
  if Assigned(FUsedOtuInfos) then
    for i := 0 to FUsedOtuInfos.Count - 1 do
    begin
      aInfo := TOtuInfo(FUsedOtuInfos[i]);
      aInfo.OutgroupMember := False;
      if SameText(aInfo.GpName, 'outgroup') then
        aInfo.GpName := EmptyStr;
    end;
end;

constructor TGroupInfo.Create(aTreeList: TTreeList);
begin
  FTreeList := aTreeList;
  FGroupNames := TStringList.Create;
  FUsedOtuInfos := nil;
end;

destructor TGroupInfo.Destroy;
begin
  if Assigned(FGroupNames) then
    FGroupNames.Free;
  FTreeList := nil; { we do not own it}
  FUsedOtuInfos := nil;
  inherited;
end;

function TGroupInfo.GetGroupNames: TStringList;
begin
  RefreshGroupNamesList;
  Result := FGroupNames;
end;

function TGroupInfo.GetIsOutgroupMember(Index: Integer): Boolean;
var
  aInfo: TOtuInfo;
begin
  Result := False;
  if Assigned(FTreeList) and (FTreeList.NoOfOtus > Index) then
  begin
    Result := FTreeList[0].IsOutgroupMember[Index];
    if (not Result) and Assigned(FUsedOtuInfos) and (FUsedOtuInfos.Count > Index) then
    begin
      aInfo := TOtuInfo(FUsedOtuInfos[Index]);
      Result := aInfo.OutgroupMember;
    end;
  end
  else if Assigned(FUsedOtuInfos) and (FUsedOtuInfos.Count > Index) then
  begin
    aInfo := TOtuInfo(FUsedOtuInfos[Index]);
    Result := aInfo.OutgroupMember;
  end;
end;

function TGroupInfo.IsOutgroup(aOtuName: String): Boolean;
var
  i: Integer;
  aInfo: TOtuInfo;
begin
  Result := False;

  if Assigned(FTreeList) and (FTreeList.NoOfOTUs > 0) then
  begin
    Result := FTreeList.IsOutgroupMember(aOtuName);
    Exit;
  end;

  if Assigned(FUsedOtuInfos) and (FUsedOtuInfos.Count > 0) then
    for i := 0 to FUsedOtuInfos.Count - 1 do
    begin
      aInfo := TOtuInfo(FUsedOtuInfos[i]);
      if SameText(aOtuName, aInfo.Name) then
      begin
        Result := aInfo.OutgroupMember;
        Exit;
      end;
    end;
end;

function TGroupInfo.NumTaxa: Integer;
begin
  Result := 0;
  if Assigned(FTreeList) then
    Result := FTreeList.NoOfOTUs
  else if Assigned(FUsedOtuInfos) then
    Result := FUsedOtuInfos.Count;
end;

procedure TGroupInfo.RefreshGroupNamesList;
var
  i: Integer;
  index: Integer;
  aInfo: TOtuInfo;
  aName: String;
begin
  if Assigned(FTreeList) and (FTreeList.NoOfOtus > 0) then
  begin
    for i := 0 to FTreeList.NoOfOtus - 1 do
    begin
      aName := FTreeList.OTUName[i];
      index := FGroupNames.IndexOfName(aName);
      if FTreeList[0].IsOutgroupMember[i] then
      begin
        if index >= 0 then
          FGroupNames[index] := aName + '=outgroup'
        else
          FGroupNames.Add(aName + '=outgroup');
      end;
    end;
  end
  else if Assigned(FUsedOtuInfos) and (FUsedOtuInfos.Count > 0) then
  begin
    for i := 0 to FUsedOtuInfos.Count - 1 do
    begin
      aInfo := TOtuInfo(FUsedOtuInfos[i]);
      aName := aInfo.Name;
      index := FGroupNames.IndexOfName(aName);
      if aInfo.OutgroupMember then
      begin
        if index >= 0 then
          FGroupNames[index] := aName + '=outgroup'
        else
          FGroupNames.Add(aName + '=outgroup');
      end;
    end;
  end;
  for i := FGroupNames.Count - 1 downto 0 do
    if Trim(FGroupNames.ValueFromIndex[i]) = EmptyStr then
      FGroupNames.Delete(i);
end;

function TGroupInfo.GetOutgroupMembers: TBoolArray;
var
  i: Integer;
begin
  SetLength(Result, NumTaxa);
  if Assigned(FTreeList) and (FTreeList.NoOfOTUs > 0) then
  begin
    for i := 0 to FTreeList.NoOfOtus - 1 do
      Result[i] := FTreeList[0].IsOutgroupMember[i];
    Exit;
  end;
  if Assigned(FUsedOtuInfos) and (FUsedOtuInfos.Count > 0) then
    for i := 0 to FUsedOtuInfos.Count - 1 do
      Result[i] := TOtuInfo(FUsedOtuInfos[i]).OutgroupMember;
end;

function TGroupInfo.IsOutgroup(Index: Integer): Boolean;
begin
  Result := False;
  if Assigned(FTreeList) and (FTreeList.NoOfOTUs > Index) then
  begin
    Result := FTreeList[0].IsOutgroupMember[Index];
    Exit;
  end;

  if Assigned(FUsedOtuInfos) and (FUsedOtuInfos.Count > Index) then
  begin
    Result := TOtuInfo(FUsedOtuInfos[Index]).OutgroupMember;
    Exit;
  end;
  raise Exception.Create('missing outgroup information');
end;

procedure TGroupInfo.SetGroupNames(const Value: TStringList);
var
  i, j: Integer;
  aInfo: TOtuInfo;
  aName: String;
  ogMembers: TStringList;
begin
  ogMembers := nil;
  if Assigned(Value) and (not (Value.Count > 0)) then
    raise Exception.Create('Cannot set group names from empty list');
  FGroupNames.Clear;
  FGroupNames.AddStrings(Value);

  if Assigned(FUsedOtuInfos) and (FUsedOtuInfos.Count > 0) then
  begin
    for i := 0 to Value.Count - 1 do
      for j := 0 to FUsedOtuInfos.Count - 1 do
      begin
        aInfo := TOtuInfo(FUsedOtuInfos[j]);
        aName := Value.Names[i];
        TrimTaxaName2(aName);
        if SameText(aName, aInfo.Name) then
        begin
          aName := Value.ValueFromIndex[i];
          TrimTaxaName2(aName);
          aInfo.GpName := aName;
          if SameText(aInfo.GpName, 'outgroup') then
            aInfo.OutgroupMember := True
          else
            aInfo.OutgroupMember := False;
          break;
        end;
      end;
  end;

  if Assigned(FTreeList) and (FTreeList.NoOfTrees > 0) and (FTreeList.NoOfOTUs > 0) then
  begin
    try
      ogMembers := TStringList.Create;
      for i := 0 to Value.Count - 1 do
        if SameText('outgroup', Value.ValueFromIndex[i]) then
          ogMembers.Add(Value.Names[i]);
      if ogMembers.Count > 0 then
        FTreeList.SetOutgroupMembers(ogMembers);
    finally
      if Assigned(ogMembers) then
        ogMembers.Free;
    end;
  end;
end;

procedure TGroupInfo.SetIsOutgroupMember(Index: Integer; const Value: Boolean);
var
  aInfo: TOtuInfo;
begin
  if Assigned(FTreeList) and (FTreeList.NoOfOTUs > Index) then
    FTreeList.SetOutgroupMember(Index, Value);

  if Assigned(FUsedOtuInfos) and (FUsedOtuInfos.Count > Index) then
  begin
    aInfo := TOtuInfo(FUsedOtuInfos[Index]);
    aInfo.OutgroupMember := Value;
    if aInfo.OutgroupMember then
      aInfo.GpName := 'outgroup'
    else if SameText(aInfo.GpName, 'outgroup') then
      aInfo.GpName := '';
  end;
end;

procedure TGroupInfo.SetTreeList(const Value: TTreeList);
begin
  FTreeList := Value;
end;

procedure TGroupInfo.SetTreeOutgroupFromOtuInfos;
var
  i: Integer;
  aInfo: TOtuInfo;
begin
  if Assigned(FTreeList) then
  begin
    FTreeList.ClearOutgroup;
    FGroupNames.Clear;
    if Assigned(FUsedOtuInfos) and (FUsedOtuInfos.Count > 0) then
      for i := 0 to FUsedOtuInfos.Count - 1 do
      begin
        aInfo := TOtuInfo(FUsedOtuInfos[i]);
        FTreeList.SetOutgroupMember(i, aInfo.OutgroupMember);
        if aInfo.OutgroupMember then
          FGroupNames.Add(aInfo.Name + '=outgroup');
      end;
  end;
end;

procedure TGroupInfo.SetTreeOutgroupFromGroupsFile(aFilename: String);
var
  i, index: Integer;
  aName: String;
begin
  FGroupNames.LoadFromFile(aFilename);
  FTreeList.ClearOutgroup;
  if FGroupNames.Count > 0 then
    for i := 0 to FGroupNames.Count - 1 do
    begin
      aName := FGroupNames.Names[i];
      TrimTaxaName2(aName);
      index := FTreeList.OTUNameList.IndexOf(aName);
      if index < 0 then
        raise Exception.Create('invalid taxon name in groups file');
      if SameText(FGroupNames.Values[aName], 'outgroup') then
        FTreeList.SetOutgroupMember(index, True);
    end;
end;

procedure TGroupInfo.SetUsedOtuInfos(const Value: TList);
begin
  FUsedOtuInfos := Value;
end;

function TGroupInfo.UpdateGroupNames(GroupNames: TStringList): Boolean;
begin
  Result := True;
  SetGroupNames(GroupNames);
end;

procedure TGroupInfo.UpdateOutgroupTaxa(OutgroupNames: TStringList);
var
  i, j: Integer;
  aInfo: TOtuInfo;
  aName: String;
begin
  ClearOutgroup;
  if OutgroupNames.Count = 0 then
    Exit;
  if Assigned(FTreeList) and (FTreeList.NoOfOtus > 0) then
    FTreeList.SetOutgroupMembers(OutgroupNames);
  if Assigned(FUsedOtuInfos) and (FUsedOtuInfos.Count > 0) then
  begin
    for i := 0 to OutgroupNames.Count - 1 do
    begin
      aName := OutgroupNames[i];
      TrimTaxaName2(aName);
      for j := 0 to FUsedOtuInfos.Count - 1 do
      begin
        aInfo := TOtuInfo(FUsedOtuInfos[j]);
        if SameText(aName, aInfo.Name) then
        begin
          aInfo.OutgroupMember := True;
          aInfo.GpName := 'outgroup';
          break;
        end;
      end;
    end;
  end;
end;

end.
