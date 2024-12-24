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

unit mancestralstatesexportheader;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MTreeList;

type
  TAncStatesExportHeaderElement = class(TObject)
  private
    FNodeId: LongInt;
    FNodeLabel: String;
    FChild2: String;
    FChild1: String;
    procedure SetChild1(const Value: String);
    procedure SetChild2(const Value: String);
    procedure SetNodeId(const Value: LongInt);
    procedure SetNodeLabel(const Value: String);
  public

    constructor Create(Id: LongInt; Child1: String; Child2: String);
    property NodeId: LongInt read FNodeId write SetNodeId;
    property Child1: String read FChild1 write SetChild1;
    property Child2: String read FChild2 write SetChild2;
    property NodeLabel: String read FNodeLabel write SetNodeLabel;
  end;


  TAncStatesExportHeaderElementsArray = array of TAncStatesExportHeaderElement;

  TAncStatesExportHeader = class(TObject)
    private
      FHeaderElements: TAncStatesExportHeaderElementsArray;
    function GetItem(Index: Integer): TAncStatesExportHeaderElement;
    procedure SetItem(Index: Integer;
      const Value: TAncStatesExportHeaderElement);
    public
      constructor Create(NumNodes: Integer);
      destructor Destroy; override;
      procedure ReplaceSpacesInNodeLabels;
      function HeaderElementString(Index: Integer): String;
      function Count: Integer;
      property Items[Index: Integer]: TAncStatesExportHeaderElement read GetItem write SetItem; default;

  end;

  {$IFNDEF VISUAL_BUILD}
  function GenerateAncStatesExportHeader(TreeList: TTreeList; Index: Integer): TAncStatesExportHeader;
  {$ENDIF}

implementation

uses
  MTreeData, mtreedataadapter, MSimpleTreeNode;

{$IFNDEF VISUAL_BUILD} { can't use this for visual mega because we are incrementing the node indices to not be zero-based}
function GenerateAncStatesExportHeader(TreeList: TTreeList; Index: Integer): TAncStatesExportHeader;
var
  Adapter: TSimpleTreeDataAdapter;
  i, NoOfNodes: Integer;
  Nodes: TSimpleTreeNodeArray;
begin
  Assert(TreeList.NoOfTrees > Index);
  Adapter := nil;

  try
    Adapter := TSimpleTreeDataAdapter.Create;
    Adapter.SetTreeData(TreeList[Index], Treelist.isRooted);
    Nodes := Adapter.GetSimpleTreeReference;
    NoOfNodes := Adapter.NumNodes;
    Result := TAncStatesExportHeader.Create(NoOfNodes);
    if NoOfNodes > 0 then
    begin
      for i := 0 to NoOfNodes - 1 do
      begin
        Result[i].NodeId := Nodes[i].NodeIndex + 1;
        if i >= TreeList.NoOfOtus then
        begin
          Result[i].Child1 := IntToStr(Nodes[i].Des1.NodeIndex + 1);
          Result[i].Child2 := IntToStr(Nodes[i].Des2.NodeIndex + 1);
          Result[i].NodeLabel := '-';
        end
        else
        begin
          Result[i].Child1 := '-';
          Result[i].Child2 := '-';
          Result[i].NodeLabel := TreeList.OTUName[i];
        end;
      end;
    end;
  finally
    if Assigned(Adapter) then
      Adapter.Free;
  end;
end;
{$ENDIF}

{ TAncStatesExportHeader }

function TAncStatesExportHeader.Count: Integer;
begin
  Result := Length(FHeaderElements);
end;

constructor TAncStatesExportHeader.Create(NumNodes: Integer);
var
  i: Integer;
begin
  SetLength(FHeaderElements, NumNodes);
  if NumNodes > 0 then
    for i := 0 to NumNodes - 1 do
      FHeaderElements[i] := TAncStatesExportHeaderElement.Create(i, EmptyStr, EmptyStr);
end;

destructor TAncStatesExportHeader.Destroy;
var
  i: Integer;
begin
  if Length(FHeaderElements) > 0 then
    for i := 0 to Length(FHeaderElements) - 1 do
      FHeaderElements[i].Free;
  SetLength(FHeaderElements, 0);
  inherited;
end;

function TAncStatesExportHeader.GetItem(Index: Integer): TAncStatesExportHeaderElement;
begin
  Assert((Index >= 0) and (Index < Length(FHeaderElements)));
  Result := FHeaderElements[Index];
end;

function TAncStatesExportHeader.HeaderElementString(Index: Integer): String;
begin
  Assert((Index >= 0) and (Index < Length(FHeaderElements)));
  Result := IntToStr(GetItem(Index).NodeId) + '. ' + '(' + GetItem(Index).Child1 + ' . ' + GetItem(Index).Child2 + ')';
end;

procedure TAncStatesExportHeader.ReplaceSpacesInNodeLabels;
var
  i: Integer;
begin
  if Length(FHeaderElements) > 0 then
    for i  := 0 to Length(FHeaderElements) - 1 do
      FHeaderElements[i].NodeLabel := StringReplace(FHeaderElements[i].NodeLabel, ' ', '_', [rfReplaceAll]);
end;

procedure TAncStatesExportHeader.SetItem(Index: Integer; const Value: TAncStatesExportHeaderElement);
begin
  Assert((Index >= 0) and (Index < Length(FHeaderElements)));
  FHeaderElements[Index] := Value;
end;

{ TAncStatesExportHeaderElement }

constructor TAncStatesExportHeaderElement.Create(Id: Integer; Child1, Child2: String);
begin
  FNodeId := Id;
  FChild1 := Child1;
  FChild2 := Child2;
end;

procedure TAncStatesExportHeaderElement.SetChild1(const Value: String);
begin
  FChild1 := Value;
end;

procedure TAncStatesExportHeaderElement.SetChild2(const Value: String);
begin
  FChild2 := Value;
end;

procedure TAncStatesExportHeaderElement.SetNodeId(const Value: LongInt);
begin
  FNodeId := Value;
end;

procedure TAncStatesExportHeaderElement.SetNodeLabel(const Value: String);
begin
  FNodeLabel := Value;
end;

end.
