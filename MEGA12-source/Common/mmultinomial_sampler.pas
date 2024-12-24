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

unit mmultinomial_sampler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MegaConsts;

type

  { TMultinomialSampler }

  TMultinomialSampler = class(TObject)
    private
      function BinarySearch(const cdf: TArrayOfExt; const target: Extended): Integer;
    public
      function RandomSample(const probabilities: TArrayOfExt; const numSamples: Integer): TIntArray; overload;
      function RandomSample(const cdf: TArrayOfExt): Integer;
  end;

implementation

uses
  math;

{ TMultinomialSampler }

function TMultinomialSampler.BinarySearch(const cdf: TArrayOfExt; const target: Extended): Integer;
var
  left: Integer = -1;
  right: Integer = -1;
  middle: Integer = -1;
begin
  Result := -1;
  left := 0;
  right := High(cdf);

  while left <= right do
  begin
    middle := floor((left + right)/2);
    if (CompareValue(cdf[middle], target, FP_CUTOFF) <= 0) and (CompareValue(cdf[middle + 1], target, FP_CUTOFF) > 0) then
    begin
      Result := middle;
      break;
    end
    else if CompareValue(cdf[middle], target, FP_CUTOFF) < 0 then
      left := middle + 1
    else if CompareValue(cdf[middle], target, FP_CUTOFF) > 0 then
      right := middle - 1;
  end;
end;

function TMultinomialSampler.RandomSample(const probabilities: TArrayOfExt; const numSamples: Integer): TIntArray;
var
  i: Integer = -1;
  j: Integer = -1;
  cdf: TArrayOfExt;
  aRand: Real = 0;
begin
  SetLength(Result, Length(probabilities));
  for i := Low(Result) to High(Result) do
    Result[i] := 0;
  SetLength(cdf, Length(probabilities) + 1);
  cdf[0] := 0;
  for i := 1 to High(cdf) do
    cdf[i] := cdf[i - 1] + probabilities[i - 1];
  if CompareValue(cdf[High(cdf)], 1, FP_CUTOFF) <> 0 then
    raise Exception.Create(Format('invalid probabilities given. They must sum to 1 but there sum is %.3e', [cdf[High(cdf)]]));
  try
    for i := 1 to numSamples do
    begin
      aRand := Random;
      j := BinarySearch(cdf, aRand);
      Result[j] := Result[j] + 1;
    end;
  finally
    SetLength(cdf, 0);
  end;
end;

function TMultinomialSampler.RandomSample(const cdf: TArrayOfExt): Integer;
var
  aRand: Real;
begin
  aRand := Random;
  Result := BinarySearch(cdf, aRand);
end;

end.

