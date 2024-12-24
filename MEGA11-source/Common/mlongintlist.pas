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

unit MLongintList;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes;

type

  { TMegaLongInt }

  TMegaLongInt = class(TObject)
    protected
      FValue: LongInt;
    public
      constructor Create(aValue: LongInt);
      procedure Assign(Source: TMegaLongInt);
      property Value: LongInt read FValue;
  end;

  { TLongintList }

  TLongintList = Class(TList)
  private
    FIsSorted: Boolean;
  protected
    function Get(Index: Longint): Longint;
    procedure Put(Index: Longint; Item: Longint);
    function BinarySearch(const aKey: LongInt; const aMin: Integer; const aMax: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TLongIntList);
    function  Add(Item: Longint): Longint;
    procedure Delete(Index: Longint);
    function  First: Longint;
    function  Last: Longint;
    function Find(aValue: LongInt): Integer;
    procedure SortAscending;
    procedure Shuffle;
    property Capacity;
    property Count;
    property Items[Index: Longint]: Longint read Get write Put ; default;
    property List;
    property IsSorted: Boolean read FIsSorted;
  end;

  function CompareAsc(Item1: Pointer; Item2: Pointer): Integer;


implementation

function CompareAsc(Item1: Pointer; Item2: Pointer): Integer;
var
  first: TMegaLongInt = nil;
  second: TMegaLongInt = nil;
begin
  first := TMegaLongInt(Item1);
  second := TMegaLongInt(Item2);
  if first.Value < second.Value then
    Result := -1
  else if first.Value > second.Value then
    Result := 1
  else
    Result := 0;
end;

{ TMegaLongInt }

constructor TMegaLongInt.Create(aValue: LongInt);
begin
  FValue := aValue;
end;

procedure TMegaLongInt.Assign(Source: TMegaLongInt);
begin
  FValue := Source.FValue;
end;


function TLongintList.Get(Index: Longint): Longint;
var
  x: Pointer;
begin
  x := inherited Items[Index];
  Result := TMegaLongInt(x).Value;
end;

procedure TLongintList.Put(Index: Longint; Item: Longint);
var
  x: Pointer = nil;
  aItem: TMegaLongInt = nil;
begin
  if (Index >= 0) and (Index < Count) then
  begin
    x := inherited Items[Index];
    if Assigned(x) then
    begin
      aItem := TMegaLongInt(x);
      aItem.Free;
    end;
  end;
  inherited Items[Index] := TMegaLongInt.Create(Item);
end;

function TLongintList.Add(Item: Longint): Longint;
begin
  Result := inherited Add(TMegaLongInt.Create(Item));
end;

procedure TLongintList.Delete(Index: Longint);
var
  aItem: TMegaLongInt = nil;
  x: Pointer = nil;
begin
  if (Index < Count) and (Index <= 0) then
  begin
    x := inherited Items[Index];
    if Assigned(x) then
    begin
      aItem := TMegaLongInt(x);
      aItem.Free;
    end;
  end;
  inherited Delete(Index);
end;

function TLongintList.First: Longint;
var
  x: Pointer;
begin
  x := inherited First;
  Result := TMegaLongInt(x).Value;
end;

function TLongintList.Last: Longint;
var
  x: Pointer;
begin
  x := inherited Last;
  Result := TMegaLongInt(x).Value;
end;

function TLongintList.Find(aValue: LongInt): Integer;
begin
  if not FIsSorted then
    SortAscending;
  Result := BinarySearch(aValue, 0, Count - 1);
end;

function TLongintList.BinarySearch(const aKey: LongInt; const aMin: Integer; const aMax: Integer): Integer;
var
  aMid: Integer;
  aValue: LongInt;
  x: Pointer = nil;
begin
  if aMax < aMin then
    Result := -1
  else
  begin
    aMid := aMin + ((aMax - aMin) div 2);
    x := inherited Items[aMid];
    aValue := TMegaLongInt(x).Value;
    if aValue > aKey then
      Result := BinarySearch(aKey, aMin, aMid - 1)
    else if aValue < aKey then
      Result := BinarySearch(aKey, aMid + 1, aMax)
    else
      Result := aMid;
  end;
end;

constructor TLongintList.Create;
begin
  inherited Create;
  FIsSorted := False;
end;

destructor TLongintList.Destroy;
var
  i: Integer;
  x: Pointer;
begin
  if Count > 0 then
    for i := Count - 1 downto 0 do
    begin
      x := inherited Items[i];
      if Assigned(x) then
        TMegaLongInt(x).Free;
    end;
  inherited Destroy;
end;

procedure TLongintList.Assign(Source: TLongIntList);
var
  i: Integer;
begin
  Clear;
  if Source.Count > 0 then
    for i := 0 to Source.Count - 1 do
      Add(Source.Get(i));
end;

procedure TLongintList.SortAscending;
begin
  inherited Sort(@CompareAsc);
  FIsSorted := True;
end;

procedure TLongintList.Shuffle;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    Exchange(i, Random(i+1));
end;

end.


