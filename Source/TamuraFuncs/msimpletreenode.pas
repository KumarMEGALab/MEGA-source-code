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

unit MSimpleTreeNode;

{$IFDEF FPC}
  {$MODE objfpc}
{$ENDIF}

interface


uses
  Classes, SysUtils, MegaConsts, fgl;

type


  { TSimpleTreeNode }

  TSimpleTreeNode = class(TObject)
    private
      FMyObject: TObject;
      FSpeciesName: AnsiString;
      procedure SetMyObject(AValue: TObject);
      procedure SetSpeciesName(const Value: AnsiString);
    public
      Size: Integer;
      Depth: Integer;
      Ancestor: TSimpleTreeNode;
      Des1: TSimpleTreeNode;
      Des2: TSimpleTreeNode;
      NodeIndex: Integer;
      TempIndex: Integer; // needed for gene duplication inference, setup using a pre-order traversal
      IsDuplicationEvent: Boolean;
      IsSpeciationEvent: Boolean;
      IsOtu: Boolean;
      IsOutgroupMember: Boolean;
      IsRoot: Boolean;
      SequenceName: AnsiString;
      SequenceData: AnsiString;
      ExtantSpecies: TStringList; // the names (sorted) of all species at the leaves of this sub-tree
      AncSpeciesIndex: Integer;

      Id: AnsiString;
      Accession: AnsiString;
      ScientificName: AnsiString;
      CommonName: AnsiString;
      Code: AnsiString;
      Annotation: AnsiString;
      Symbol: AnsiString;
      DataCoverage: Double;
      StdError: Double;
      BLen: Double;
      Value: Double; {can be used for bootstrap value, divergence time, etc...}
      Value2: Double;
      constructor Create;
      destructor Destroy; override;
      function StringDescription: AnsiString;
      function GetSpeciesName: AnsiString;
      function GetLeafNodeIds: TIntArray;
      function BothDescendentsAreOtus: Boolean;
      function Descendent1IsOtu: Boolean;
      function Descendent2IsOtu: Boolean;
      property SpeciesName: AnsiString read FSpeciesName write SetSpeciesName; // SetSpeciesName forces lower case so we can search case-sensitive
      property MyObject: TObject read FMyObject write SetMyObject;
  end;

  TSimpleTreeNodeArray = array of TSimpleTreeNode;
  TSimpleTreeNodeArrayArray = array of TSimpleTreeNodeArray;
  TSimpleTreeNodeList = specialize TFPGList<TSimpleTreeNode>;
  TSimpleTreeNodeListArray = array of TSimpleTreeNodeList;

  function CompareSimpleTreeNodes(Item1: Pointer; Item2: Pointer): Integer;
  function TreeAsTable(RootNode: TSimpleTreeNode): TStringList; overload;
  function TreeAsTable(ATree: TSimpleTreeNodeArray): TStringList; overload;
  procedure CloneTreeNodeArray(Source: TSimpleTreeNodeArray; var Destination: TSimpleTreeNodeArray);


implementation


uses
  Math;

{ TSimpleTreeNode }



constructor TSimpleTreeNode.Create;
begin
  MyObject := nil;
  Value := 0.0;
  BLen := 0.0;
  StdError := 0.0;
  DataCoverage := 0.0;
  IsOutgroupMember := False;
  Depth := -1;
  Size := -1;
  Ancestor := nil;
  Des1 := nil;
  Des2 := nil;
  NodeIndex := -1;
  IsDuplicationEvent := False;
  IsSpeciationEvent := False;
  IsOtu := False;
  FSpeciesName := EmptyStr;
  AncSpeciesIndex := -1;
  ExtantSpecies := TStringList.Create;
  ExtantSpecies.Sorted := True;
  ExtantSpecies.CaseSensitive := True;
  IsRoot := False;
  TempIndex := -1;
  SequenceName := EmptyStr;
  Id := EmptyStr;
  Accession := EmptyStr;
  ScientificName := EmptyStr;
  CommonName := EmptyStr;
  Code := EmptyStr;
  Annotation := EmptyStr;
  Symbol := EmptyStr;
  SequenceData := EmptyStr;
end;

destructor TSimpleTreeNode.Destroy;
begin
  if Assigned(ExtantSpecies) then
    ExtantSpecies.Free;
  inherited;
end;

function TSimpleTreeNode.GetSpeciesName: AnsiString;
begin
  Result := EmptyStr;
  if ScientificName <> EmptyStr then
    Result := ScientificName
  else if SpeciesName <> EmptyStr then
    Result := SpeciesName
  else if CommonName <> EmptyStr then
    Result := CommonName;
end;

function TSimpleTreeNode.GetLeafNodeIds: TIntArray;

  procedure FindLeafNodeIds(aNode: TSimpleTreeNode);
  begin
    if aNode.IsOtu then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := aNode.NodeIndex;
    end
    else
    begin
      if Assigned(aNode.Des1) then
        FindLeafNodeIds(aNode.Des1);
      if Assigned(aNode.Des2) then
        FindLeafNodeIds(aNode.Des2);
    end;
  end;

begin
  SetLength(Result, 0);
  if IsOtu then
    Exit;
  if Assigned(Des1) then
    FindLeafNodeIds(Des1);
  if Assigned(Des2) then
    FindLeafNodeIds(Des2);
end;

function TSimpleTreeNode.BothDescendentsAreOtus: Boolean;
begin
  Result := ((not IsOtu) and Assigned(Des1) and (Des1.IsOtu) and Assigned(Des2) and (Des2.IsOtu));
end;

function TSimpleTreeNode.Descendent1IsOtu: Boolean;
begin
  Result := (not IsOtu) and Assigned(Des1) and Des1.IsOtu;
end;

function TSimpleTreeNode.Descendent2IsOtu: Boolean;
begin
  Result := (not IsOtu) and Assigned(Des2) and Des2.IsOtu;
end;

procedure TSimpleTreeNode.SetSpeciesName(const Value: AnsiString);
begin
  FSpeciesName := LowerCase(Value);
end;


procedure TSimpleTreeNode.SetMyObject(AValue: TObject);
begin
  if FMyObject=AValue then Exit;
  FMyObject:=AValue;
end;
function TSimpleTreeNode.StringDescription: AnsiString;
begin
  if IsOtu then
    Result := Format('%8d %7d %6d %6d %6s %6s %6s %6s %6s %-30s %-20s', [TempIndex, AncSpeciesIndex, Depth, NodeIndex + 1, BoolToStr(IsOutgroupMember, True), '-', '-', BoolToStr(IsOtu, True), BoolToStr(IsDuplicationEvent, True), SequenceName, SpeciesName])
  else
    Result := Format('%8d %7d %6d %6d %6s %6s %6s %6s %6s', [TempIndex, AncSpeciesIndex, Depth, NodeIndex + 1, BoolToStr(IsOutgroupMember, True), IntToStr(Des1.NodeIndex + 1), IntToStr(Des2.NodeIndex + 1), BoolToStr(IsOtu, True), BoolToStr(IsDuplicationEvent, True)]);
end;

function CompareSimpleTreeNodes(Item1: Pointer; Item2: Pointer): Integer;
var
  Node1, Node2: TSimpleTreeNode;
begin
  Node1 := TSimpleTreeNode(Item1);
  Node2 := TSimpleTreeNode(Item2);

  if Node1.IsOtu and (not Node2.IsOtu) then
    Result := -1
  else if (not Node1.IsOtu) and Node2.IsOtu then
    Result := 1
  else if Node1.IsOtu and Node2.IsOtu then
    Result := CompareValue(Node1.NodeIndex, Node2.NodeIndex)
  else
    Result := CompareValue(Node2.NodeIndex, Node1.NodeIndex);
end;

function TreeAsTable(RootNode: TSimpleTreeNode): TStringList;

  procedure ProcessNode(ANode: TSimpleTreeNode);
  begin
    Result.Add(ANode.StringDescription);
    if Assigned(ANode.Des1) then
      ProcessNode(ANode.Des1);
    if Assigned(ANode.Des2) then
      Processnode(ANode.Des2);
  end;

begin
  Result := TStringList.Create;
  Result.Add(Format('%8s %7s %6s %6s %6s %6s %6s %6s %6s %20s %20s', ['TempInd', 'AncInd', 'Depth', 'NodeId', 'outgroup', 'Des1', 'Des2', 'IsOtu', 'IsDup', 'OtuName', 'SpName']));
  ProcessNode(RootNode);
end;

function TreeAsTable(ATree: TSimpleTreeNodeArray): TStringList;
var
  RootNode: TSimpleTreeNode;

  function FindRoot: TSimpleTreeNode;
  var
    j: Integer;
  begin
    for j := 0 to Length(ATree) - 1 do
      if not Assigned(ATree[j].Ancestor) then
      begin
        Result := ATree[j];
        break;
      end;
  end;

begin
  RootNode := FindRoot;
  Result := TreeAsTable(RootNode);
end;

procedure CloneTreeNodeArray(Source: TSimpleTreeNodeArray; var Destination: TSimpleTreeNodeArray);
var
  i: Integer;
begin
  Assert(Length(Source) = Length(Destination)); { it is assumed that memory is alread allocaton for the destination}
  for i := 0 to Length(Destination) - 1 do
  begin
    Destination[i].SpeciesName := Source[i].SpeciesName;
    Destination[i].Depth := Source[i].Depth;
    if Assigned(Source[i].Ancestor) then
      Destination[i].Ancestor := Destination[Source[i].Ancestor.NodeIndex];
    if not Source[i].IsOtu then
      Destination[i].Des1 := Destination[Source[i].Des1.NodeIndex];
    if not Source[i].IsOtu then
      Destination[i].Des2 := Destination[Source[i].Des2.NodeIndex];
    Destination[i].NodeIndex := Source[i].NodeIndex;
    Destination[i].TempIndex := Source[i].TempIndex; // needed for gene duplication inference, setup using a pre-order traversal
    Destination[i].IsDuplicationEvent := Source[i].IsDuplicationEvent;
    Destination[i].IsSpeciationEvent := Source[i].IsSpeciationEvent;
    Destination[i].IsOtu := Source[i].IsOtu;
    Destination[i].IsRoot := Source[i].IsRoot;
    Destination[i].SequenceName := Source[i].SequenceName;
    Destination[i].ExtantSpecies.Assign(Source[i].ExtantSpecies); // the names (sorted) of all species at the leaves of this sub-tree
    Destination[i].AncSpeciesIndex := Source[i].AncSpeciesIndex;
    Destination[i].Id := Source[i].Id;
    Destination[i].Accession := Source[i].Accession;
    Destination[i].ScientificName := Source[i].ScientificName;
    Destination[i].CommonName := Source[i].CommonName;
    Destination[i].Code := Source[i].Code;
    Destination[i].Annotation := Source[i].Annotation;
    Destination[i].Symbol := Source[i].Symbol;
    Destination[i].Value := Source[i].Value;
  end;
end;

end.
