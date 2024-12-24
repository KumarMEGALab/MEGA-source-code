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

unit mextendedlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MegaConsts;

type
  { TExtended }
  { A wrapper around an Extended value - needed because TList will not support casting Pointers to Extended on all platforms}
  TExtended = class(TObject)
    private
      FValue: Extended;
    public
      constructor Create(aVal: Extended);
      property Value: Extended read FValue write FValue;
  end;

  { TExtendedList }
  { Just a TList for easy management of Extended floating point values}
  TExtendedList = class(TList)
    protected
      function GetItem(Index: Int64): Extended;
      procedure SetItem(Index: Int64; AValue: Extended);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(aValue: Extended);
      procedure ClearItems;
      function CalcMeanAndVariance(var aMean: Extended; var aVariance: Extended): Boolean;
      function GetMedianValue(vals: TArrayOfExt): Extended;
      function GetMedianValueWithCutoff(vals: TArrayOfExt; minVal: Extended): Extended; overload;
      function GetMedianValueWithCutoff(minVal: Extended): Extended; overload;
      property Items[Index: Int64]: Extended read GetItem write SetItem; default;
  end;

  function CompareExtendedVals(Item1: Pointer; Item2: Pointer): Integer;

implementation

uses
  math;

function CompareExtendedVals(Item1: Pointer; Item2: Pointer): Integer;
var
  e1, e2: TExtended;
begin
  e1 := TExtended(Item1);
  e2 := TExtended(Item2);
  Result := CompareValue(e1.Value, e2.Value, FP_CUTOFF);
end;

{ TExtended }

constructor TExtended.Create(aVal: Extended);
begin
  FValue := aVal;
end;

{ TExtendedList }

function TExtendedList.GetItem(Index: Int64): Extended;
var
  val: Pointer;
begin
  val := inherited Items[Index];
  if Assigned(val) then
    Result := TExtended(val).Value;
end;

procedure TExtendedList.SetItem(Index: Int64; AValue: Extended);
var
  p: Pointer;
  e: TExtended;
begin
  p := inherited Items[Index];
  e := TExtended(p);
  e.Value := AValue;
end;

procedure TExtendedList.ClearItems;
var
  i: Integer;
  p: Pointer = nil;
begin
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      p := inherited Items[i];
      if Assigned(p) then
        TExtended(p).Free;
    end;
  inherited Clear;
end;

constructor TExtendedList.Create;
begin
  inherited Create;
end;

destructor TExtendedList.Destroy;
begin
  ClearItems;
  inherited Destroy;
end;

procedure TExtendedList.Add(aValue: Extended);
begin
  inherited Add(TExtended.Create(aValue));
end;

function TExtendedList.CalcMeanAndVariance(var aMean: Extended; var aVariance: Extended): Boolean;
var
  i: Integer;
  vals: array of Extended;
  sd: Extended = 0.0;
begin
  Result := False;
  if Count = 0 then
  begin
    aMean := 0.0;
    aVariance := 0.0;
    Exit;
  end
  else if Count = 1 then
  begin
    aMean := GetItem(0);
    aVariance := 0.0;
    Exit;
  end;

  SetLength(vals, Count);
  for i := 0 to Count - 1 do
    vals[i] := GetItem(i);
  try
    meanandstddev(vals, aMean, sd);
    aVariance := sd*sd;
  except
    Result := False;
  end;
  Result := True;
end;

function TExtendedList.GetMedianValue(vals: TArrayOfExt): Extended;
var
  i: Integer;
begin
  Result := 0.0;
  if Length(vals) = 0 then
    Exit;
  if Length(vals) = 1 then
  begin
    Result := vals[Low(vals)];
    Exit;
  end;

  ClearItems;
  if Length(vals) > 0 then
    for i := Low(vals) to High(vals) do
      Add(vals[i]);
  Sort(@CompareExtendedVals);
  if odd(Count) then
    Result := GetItem(Count div 2)
  else
    Result := (GetItem(Count div 2) + GetItem((Count div 2) - 1))/2;
end;

function TExtendedList.GetMedianValueWithCutoff(vals: TArrayOfExt; minVal: Extended): Extended;
var
  i: Integer;
begin
  Result := 0.0;
  if Length(vals) = 0 then
    Exit;
  if Length(vals) = 1 then
  begin
    Result := vals[Low(vals)];
    Exit;
  end;

  ClearItems;
  if Length(vals) > 0 then
    for i := Low(vals) to High(vals) do
      if CompareValue(vals[i], minVal, FP_CUTOFF) >= 0 then
        Add(vals[i]);
  if Count = 0 then
    Exit;
  if Count > 1 then
    Sort(@CompareExtendedVals);
  if Count = 1 then
    Exit(GetItem(0));
  if odd(Count) then
    Result := GetItem(Count div 2)
  else
    Result := (GetItem(Count div 2) + GetItem((Count div 2) - 1))/2;
end;

function TExtendedList.GetMedianValueWithCutoff(minVal: Extended): Extended;
var
  i: Integer;
begin
  Result := 0.0;
  if Count = 0 then
    Exit;
  if Count = 1 then
  begin
    Result := GetItem(0);
    Exit;
  end;

  for i := Count - 1 downto 0 do
    if CompareValue(GetItem(i), minVal, FP_CUTOFF) < 0 then
      Delete(i);
  if Count = 0 then
    Exit;
  if Count > 1 then
    Sort(@CompareExtendedVals);
  if Count = 1 then
    Exit(GetItem(0));
  if odd(Count) then
    Result := GetItem(Count div 2)
  else
    Result := (GetItem(Count div 2) + GetItem((Count div 2) - 1))/2;
end;

end.

