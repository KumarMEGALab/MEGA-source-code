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

unit mtaxa_name_search;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { TNodeDisplayInfo }

  TNodeDisplayInfo = class(TObject)
    private
      FIsMatch: Boolean;
      FNodeIndex: Integer;
      FOrderOfAppearance: Integer;
      FNodeName: String;
    public
      constructor Create(aName: String; aNodeIndex: Integer; aOrderOfAppearance: Integer);
      function CsvString: String;
      procedure Assign(source: TNodeDisplayInfo);
      function Clone: TNodeDisplayInfo;
      class function CsvHeaderString: String;
      property NodeName: String read FNodeName write FNodeName;
      property NodeIndex: Integer read FNodeIndex write FNodeIndex;
      property OrderOfAppearance: Integer read FOrderOfAppearance write FOrderOfAppearance;
      property IsMatch: Boolean read FIsMatch write FIsMatch;

  end;

  TNodeDisplayInfoList = specialize TFPGList<TNodeDisplayInfo>;

  { TTaxaNamesSearch }

  TTaxaNamesSearch = class(TObject)
    private
      FPointer: Integer;
      FNumMatches: Integer;
      FNodeDisplayInfo: TNodeDisplayInfoList;
      FCurrentMatchNodeId: Integer;
      FCurrentMatchNumber: Integer;
      FQueryStr: String;
      procedure ClearDisplayInfoList;
      function GetItems(index: Integer): TNodeDisplayInfo;
      function GetNumMatches: Integer;
      function GetNumNodes: Integer;
    public
      constructor Create;
      destructor Destroy; override;
      function NewSearch(query: String; nodeDisplayInfo: TNodeDisplayInfoList; mustStartWith: Boolean): Integer;
      function HasNext: Boolean;
      function AdvanceToNext: Integer;
      function IsMatch(index: Integer): Boolean;
      property CurrentMatchNodeId: Integer read FCurrentMatchNodeId;
      property CurrentMatchNumber: Integer read FCurrentMatchNumber;
      property QueryStr: String read FQueryStr;
      property NumMatches: Integer read GetNumMatches;
      property Items[index: Integer]: TNodeDisplayInfo read GetItems; default;
      property NumNodes: Integer read GetNumNodes;
  end;

  function NodeDisplayInfoListToStringList(infoList: TNodeDisplayInfoList): TStringList;
  function NodeDisplayInfoListHasNodeDisplayInfo(infoList: TNodeDisplayInfoList; aNodeIndex: Integer): Boolean;

implementation

uses
  StrUtils, StringUtils;

function NodeDisplayInfoListToStringList(infoList: TNodeDisplayInfoList): TStringList;
var
  i: Integer = -1;
begin
  Result := TStringList.Create;
  Result.Add(TNodeDisplayInfo.CsvHeaderString);
  if Assigned(infoList) and (infoList.Count > 0) then
    for i := 0 to infoList.Count - 1 do
      Result.Add(infoList[i].CsvString);
end;

function NodeDisplayInfoListHasNodeDisplayInfo(infoList: TNodeDisplayInfoList; aNodeIndex: Integer): Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if infoList.Count > 0 then
    for i := 0 to infoList.Count - 1 do
      if infoList[i].NodeIndex = aNodeIndex then
        Exit(True);
end;

{ TTaxaNamesSearch }

procedure TTaxaNamesSearch.ClearDisplayInfoList;
var
  i: Integer = -1;
begin
  if FNodeDisplayInfo.Count > 0 then
  begin
    for i := 0 to FNodeDisplayInfo.Count - 1 do
      FNodeDisplayInfo[i].Free;
    FNodeDisplayInfo.Clear;
  end;
end;

function TTaxaNamesSearch.GetItems(index: Integer): TNodeDisplayInfo;
begin
  if index < FNodeDisplayInfo.Count then
    Result := FNodeDisplayInfo[index]
  else
    Result := nil;
end;

function TTaxaNamesSearch.GetNumMatches: Integer;
begin
  Result := FNumMatches;
end;

function TTaxaNamesSearch.GetNumNodes: Integer;
begin
  Result := FNodeDisplayInfo.Count;
end;

constructor TTaxaNamesSearch.Create;
begin
  FNodeDisplayInfo := TNodeDisplayInfoList.Create;
  FQueryStr := EmptyStr;
  FCurrentMatchNodeId := -1;
  FCurrentMatchNumber := -1;
  FNumMatches := 0;
  FPointer := 0;
end;

destructor TTaxaNamesSearch.Destroy;
begin
  ClearDisplayInfoList;
  FNodeDisplayInfo.Free;
  inherited Destroy;
end;

function TTaxaNamesSearch.NewSearch(query: String; nodeDisplayInfo: TNodeDisplayInfoList; mustStartWith: Boolean): Integer;
var
  aIsMatch: Boolean = False;
  i: Integer;
  aInfo: TNodeDisplayInfo = nil;
begin
  FPointer := -1;
  FNumMatches := 0;
  FCurrentMatchNodeId := -1;
  FQueryStr := query;
  ClearDisplayInfoList;
  for i := 0 to nodeDisplayInfo.Count - 1 do
  begin
    aInfo := nodeDisplayInfo[i].Clone;
    aInfo.IsMatch := False;
    FNodeDisplayInfo.Add(aInfo);
  end;

  for i := 0 to FNodeDisplayInfo.Count - 1 do
  begin
    aIsMatch := False;
    aInfo := FNodeDisplayInfo[i];
    if mustStartWith then
    begin
      if Pos(Uppercase(FQueryStr), Uppercase(aInfo.NodeName)) = 1 then
        aIsMatch := True;
    end
    else if ContainsText(aInfo.NodeName, FQueryStr) then
      aIsMatch := True;

    if aIsMatch then
    begin
      if FPointer = -1 then
        FPointer := i;
      aInfo.IsMatch := True;
      if FCurrentMatchNodeId = -1 then
        FCurrentMatchNodeId := aInfo.NodeIndex;
      inc(FNumMatches);
    end;
  end;
  Result := FNumMatches;
  if FNumMatches > 0 then
    FCurrentMatchNumber := 1;
end;

function TTaxaNamesSearch.HasNext: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if FPointer < (FNodeDisplayInfo.Count - 1) then
    for i := FPointer + 1 to FNodeDisplayInfo.Count - 1 do
      if FNodeDisplayInfo[i].IsMatch then
        Exit(True);
end;

function TTaxaNamesSearch.AdvanceToNext: Integer;
var
  i: Integer = -1;
begin
  Result := -1;
  if FPointer < (FNodeDisplayInfo.Count - 1) then
    for i := FPointer + 1 to FNodeDisplayInfo.Count - 1 do
      if FNodeDisplayInfo[i].IsMatch then
      begin
        FCurrentMatchNodeId := FNodeDisplayInfo[i].NodeIndex;
        FPointer := i;
        inc(FCurrentMatchNumber);
        Exit(FCurrentMatchNodeId);
      end;
end;

function TTaxaNamesSearch.IsMatch(index: Integer): Boolean;
begin
  Result := False;
  if (index >= 0) and (index < FNodeDisplayInfo.Count) then
    Result := FNodeDisplayInfo[index].IsMatch;
end;

{ TNodeDisplayInfo }

constructor TNodeDisplayInfo.Create(aName: String; aNodeIndex: Integer; aOrderOfAppearance: Integer);
begin
  FIsMatch := False;
  FNodeName := aName;
  FNodeIndex := aNodeIndex;
  FOrderOfAppearance := aOrderOfAppearance;
end;

function TNodeDisplayInfo.CsvString: String;
begin
  Result := Format('%d,%d,%s,%s', [FOrderOfAppearance, FNodeIndex, FNodeName, BoolToStr(FIsMatch, True)]);
end;

procedure TNodeDisplayInfo.Assign(source: TNodeDisplayInfo);
begin
  FNodeIndex := source.FNodeIndex;
  FOrderOfAppearance := source.FOrderOfAppearance;
  FNodeName := source.FNodeName;
  FIsMatch := source.FIsMatch;
end;

function TNodeDisplayInfo.Clone: TNodeDisplayInfo;
begin
  Result := TNodeDisplayInfo.Create(EmptyStr, -1, -1);
  Result.Assign(Self);
end;

class function TNodeDisplayInfo.CsvHeaderString: String;
begin
  Result := 'OrderOfAppearance,NodeIndex,TaxonName,IsMatch';
end;

end.

