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

unit mextendedlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MegaConsts, fgl;

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
      function HasDistinctValues: Boolean;
      function GetItem(Index: Int64): Extended;
      procedure SetItem(Index: Int64; AValue: Extended);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(aValue: Extended);
      procedure AddValues(aList: TExtendedList);
      procedure GetValues(var aList: TExtendedList); overload;
      procedure GetValues(var Vals: TArrayOfExt); overload;
      procedure ClearItems;
      function CalcMeanAndVariance(var aMean: Extended; var aVariance: Extended): Boolean;
      function CalcMeanAndMeanSqErr(var aMean: Extended; var aMeanSqErr: Extended): Boolean;
      function CalcMeanSeqErr(var aMeanSqErr: Extended): Boolean;
      function GetMean: Extended; overload;
      function GetMean(vals: TArrayOfExt): Extended; overload;
      function GetStdDev: Extended;
      function GetVariance: Extended;
      function GetProportionGTE(threshold: Extended): Extended;
      function GetMedianValue(vals: TArrayOfExt): Extended;
      function GetMedian: Extended;
      function GetMedianValueWithCutoff(vals: TArrayOfExt; minVal: Extended): Extended; overload;
      function GetMedianValueWithCutoff(minVal: Extended): Extended; overload;
      function GetMeanWithCutoff(vals: TArrayOfExt; minVal: Extended): Extended; overload; deprecated;
      function GetMeanWithCutoff(minVal: Extended): Extended; overload; deprecated;
      function Quantile(q: Double): Extended;
      function SaveToFile(filename: String): Boolean;
      function ToCsvString(aFormat: String): String;
      property Items[Index: Int64]: Extended read GetItem write SetItem; default;
  end;

  TListOfExtendedList = specialize TFPGList<TExtendedList>;

  function CompareExtendedVals(Item1: Pointer; Item2: Pointer): Integer;
  function GetMeanAndMedian(const vals: TArrayOfExt; var aMean: Double; var aMedian: Double): Boolean;

implementation

uses
  math;

function CompareExtendedVals(Item1: Pointer; Item2: Pointer): Integer;
var
  e1: TExtended = nil;
  e2: TExtended = nil;
begin
  if Assigned(Item1) then
    e1 := TExtended(Item1);
  if Assigned(Item2) then
    e2 := TExtended(Item2);
  if Assigned(e1) and Assigned(e2) then
    Result := CompareValue(e1.Value, e2.Value, FP_CUTOFF)
  else if Assigned(e1) then
    Result := 1
  else if Assigned(e2) then
    Result := -1
  else
    Result := 0;
end;

function GetMeanAndMedian(const vals: TArrayOfExt; var aMean: Double;var aMedian: Double): Boolean;
var
  aList: TExtendedList = nil;
  i: Integer = -1;
begin
  Result := False;
  try
    aList := TExtendedList.Create;
    if Length(vals) > 0 then
    begin
      aMean := aList.GetMean(vals);
      aMedian := aList.GetMedianValue(vals);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

{ TExtended }

constructor TExtended.Create(aVal: Extended);
begin
  FValue := aVal;
end;

{ TExtendedList }

function TExtendedList.HasDistinctValues: Boolean;
var
  i: Integer = -1;
begin
  if Count <= 1 then
    Exit(False);
  for i := 1 to Count - 1 do
    if CompareValue(GetItem(i), GetItem(i - 1), FP_CUTOFF) <> 0 then
      Exit(True);
  Result := False;
end;

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

procedure TExtendedList.AddValues(aList: TExtendedList);
var
  i: Integer;
begin
  if aList.Count > 0 then
    for i := 0 to aList.Count - 1 do
      Add(aList.GetItem(i));
end;

procedure TExtendedList.GetValues(var aList: TExtendedList);
var
  i: Integer = -1;
begin
  if Count > 0 then
    for i := 0 to Count - 1 do
      aList.Add(GetItem(i));
end;

procedure TExtendedList.GetValues(var Vals: TArrayOfExt);
var
  i: Integer = -1;
begin
  if Count > 0 then
  begin
    SetLength(vals, Count);
    for i := 0 to Count - 1 do
      vals[i] := GetItem(i);
  end;
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
    Exit(True);
  end
  else if Count = 1 then
  begin
    aMean := GetItem(0);
    aVariance := 0.0;
    Exit(True);
  end;

  SetLength(vals, Count);
  for i := 0 to Count - 1 do
    vals[i] := GetItem(i);
  try
    if HasDistinctValues then
    begin
      meanandstddev(vals, aMean, sd);
      aVariance := sd*sd;
    end
    else
    begin
      aMean := GetMean;
      aVariance := 0;
    end;
  except
    Result := False;
  end;
  Result := True;
end;

function TExtendedList.CalcMeanAndMeanSqErr(var aMean: Extended; var aMeanSqErr: Extended): Boolean;
var
  i: Integer;
  diffs: array of Extended;
  aSumOfSqs: Extended = 0.0;
begin
  Result := False;
  if Count = 0 then
  begin
    aMean := 0.0;
    aMeanSqErr := 0.0;
    Exit(True);
  end
  else if Count = 1 then
  begin
    aMean := GetItem(0);
    aMeanSqErr := 0.0;
    Exit(True);
  end;

  aMean := GetMean;
  SetLength(diffs, Count);
  for i := 0 to Count - 1 do
    diffs[i] := GetItem(i) - aMean;

  try
    if HasDistinctValues then
    begin
      aSumOfSqs := SumOfSquares(diffs);
      aMeanSqErr := aSumOfSqs/Count;
    end
    else
      aMeanSqErr := 0;
  except
    Result := False;
  end;
  Result := True;
end;

function TExtendedList.CalcMeanSeqErr(var aMeanSqErr: Extended): Boolean;
var
  aMean: Extended = 0;
begin
  Result := CalcMeanAndMeanSqErr(aMean, aMeanSqErr);
end;

function TExtendedList.GetMean: Extended;
var
  i: Integer;
begin
  if Count = 0 then
    Exit(0);
  if Count = 1 then
    Exit(GetItem(0));
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + GetItem(i);
  Result := Result/Count;
end;

function TExtendedList.GetMean(vals: TArrayOfExt): Extended;
var
  i: Integer = -1;
begin
  Result := 0.0;
  if Length(vals) = 0 then
    Exit;
  if Length(vals) = 1 then
  begin
    Result := vals[0];
    Exit;
  end;

  for i := Low(vals) to High(vals) do
    Result := Result + vals[i];
  Result := Result/(Length(vals));
end;

function TExtendedList.GetStdDev: Extended;
var
  aMean: Extended = -1;
  aVariance: Extended = -1;
begin
  CalcMeanAndVariance(aMean, aVariance);
  if CompareValue(aVariance, 0.0, FP_CUTOFF) < 0 then
    raise Exception.Create(Format('Application error - invalid variance cannot be negative but got %.3e', [aVariance]));
  if CompareValue(aVariance, 0.0, FP_CUTOFF) = 0 then
    Result := 0.0
  else
    Result := sqrt(aVariance);
end;

function TExtendedList.GetVariance: Extended;
var
  aMean: Extended = -1;
  aVariance: Extended = -1;
begin
  CalcMeanAndVariance(aMean, aVariance);
  Result := aVariance;
end;

function TExtendedList.GetProportionGTE(threshold: Extended): Extended;
var
  i: Integer;
  numGTE: Integer = 0;
  val: Extended;
begin
  if Count = 0 then
    Exit(0);
  for i := 0 to Count - 1 do
  begin
    val := GetItem(i);
    if CompareValue(val, threshold, FP_CUTOFF) >= 0 then
      inc(numGTE);
  end;
  Result := numGTE/Count*100;
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

function TExtendedList.GetMedian: Extended;
begin
  if Count = 0 then
    Exit(0.0)
  else if Count = 1 then
    Exit(GetItem(0))
  else if Count = 2 then
    Exit((GetItem(0) + GetItem(1))/2);

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

  for i := Count - 1 downto 0 do
    if CompareValue(GetItem(i), minVal, FP_CUTOFF) < 0 then
      Delete(i);
  inherited Pack;
  if Count = 0 then
    Exit;
  if Count = 1 then
    Exit(GetItem(0));
  if Count > 1 then
    Sort(@CompareExtendedVals);
  if odd(Count) then
    Result := GetItem(Count div 2)
  else
    Result := (GetItem(Count div 2) + GetItem((Count div 2) - 1))/2;
end;

function TExtendedList.GetMeanWithCutoff(vals: TArrayOfExt; minVal: Extended): Extended;
var
  i: Integer;
begin
  raise Exception.Create('call to deprecated GetMeanWithCutoff function');
  Result := 0.0;
  ClearItems;
  if Length(vals) > 0 then
    for i := Low(vals) to High(vals) do
      if CompareValue(vals[i], minVal, FP_CUTOFF) >= 0 then
        Add(vals[i]);

  if Count = 0 then
    Exit;
  if Count = 1 then
  begin
    Result := GetItem(0);
    Exit;
  end;
  Result := GetMean;
end;

function TExtendedList.GetMeanWithCutoff(minVal: Extended): Extended;
var
  i: Integer;
begin
  raise Exception.Create('call to deprecated GetMeanWithCutoff function');
  Result := 0.0;
  if Count > 0 then
    for i := Count - 1 downto 0 do
      if CompareValue(GetItem(i), minVal, FP_CUTOFF) < 0 then
        Delete(i);
  inherited Pack;
  if Count = 0 then
    Exit;
  if Count = 1 then
  begin
    Result := GetItem(0);
    Exit;
  end;
  Result := GetMean;
end;

function TExtendedList.Quantile(q: Double): Extended;
var
  observations: array of Extended;
  i: Integer;
  temp: Extended = -1;
  target: Integer = -1;
begin
  if (CompareValue(q, 1.0, FP_CUTOFF) > 0) or (CompareValue(1, 0.0, FP_CUTOFF) < 0) then
    raise Exception.Create(Format('quantile parameter must be > 0 and < 1 but got %.3e ', [q]));
  if Count = 0 then
    Exit(0);
  if Count > 1 then
    Sort(@CompareExtendedVals);
  SetLength(observations, Count + 1);
  observations[0] := 0;
  for i := 1 to High(observations) do
    observations[i] := GetItem(i - 1);
  temp := q*(Count + 1);
  target := max(0, trunc(temp) - 1);
  if CompareValue(frac(temp), 0.0, FP_CUTOFF) = 0 then
    Result := observations[target]
  else
    Result := observations[target] + (observations[target + 1] - observations[target])*frac(temp);
end;

function TExtendedList.SaveToFile(filename: String): Boolean;
var
  aFile: TextFile;
  i: Integer;
begin
  try
    AssignFile(aFile, filename);
    Rewrite(aFile);
    if Count > 0 then
      for i := 0 to Count - 1 do
        WriteLn(aFile, Format('%.8f', [GetItem(i)]));
  finally
    CloseFile(aFile);
  end;
  Result := FileExists(filename);
end;

function TExtendedList.ToCsvString(aFormat: String): String;
var
  i: Integer;
  fmt: String;
begin
  fmt := aFormat + ',';
  Result := EmptyStr;
  if Count > 0 then
    for i := 0 to Count - 1 do
      Result := Result + Format(fmt, [GetItem(i)]);
end;

end.

