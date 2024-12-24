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

unit mpartitions_blens_summary;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, mpartitions_blens_list, MegaConsts;

const
  MIN_FREQUENCY_CUTOFF = 0.1;

type

  { TPartitionsBLensSummary }

  TPartitionsBLensSummary = class(TObject)
    private
      FNumRepsPerSample: Integer;
      FPartitionString: String;
      FRawBLensCount: Integer;
      FSubsampleCount: Integer;
      FRawBLens: PArrayOfExtended;
      FAvgBLens: PArrayOfExtended;
      FVariance: PArrayOfExtended;
      FFrequencies: PArrayOfInt;
      function MeanVariance: Extended;
      function GetNumSubsamples: Integer;
      function MeanAndStdDevOfAvgBLens(var aMean: Extended; var aStdDev: Extended): Boolean;
      function MeanAndStdDevOfRawBLens(var aMean: Extended; var aStdDev: Extended): Boolean;
    public
      constructor Create(aPartitionString: String; numSamples: Integer; numRepsPerSample: Integer);
      destructor Destroy; override;
      procedure AddBranchLengthInfo(avgBlensList: TAvgBLensList; subSampleIndex: Integer);
      function GetCsvString(isBootstrap: Boolean; numSamples: Integer): String;
      class function GetCsvHeaderString(numSubsamples: Integer; isBootstrap: Boolean): String; static;
      property PartitionString: String read FPartitionString;
      property NumSubsamples: Integer read GetNumSubsamples;
  end;

implementation

uses
  math;

{ TPartitionsBLensSummary }

function TPartitionsBLensSummary.MeanAndStdDevOfAvgBLens(var aMean: Extended; var aStdDev: Extended): Boolean;
var
  i: Integer;
  numVals: Integer = 0;
  tempVals: array of Extended;
begin
  Result := False;
  if FSubsampleCount = 0 then
    Exit;
  for i := 0 to FSubsampleCount - 1 do
    if CompareValue(FAvgBLens[i], -1, FP_CUTOFF) <> 0 then
    begin
      SetLength(tempVals, numVals + 1);
      tempVals[numVals] := FAvgBLens[i];
      inc(numVals);
    end;
  meanandstddev(tempVals, aMean, aStdDev);
  Result := True;
end;

function TPartitionsBLensSummary.MeanVariance: Extended;
var
  i: Integer;
  numVals: Integer = 0;
begin
  Result := 0;
  if FSubsampleCount = 0 then
    Exit;
  for i := 0 to FSubsampleCount - 1 do
    if CompareValue(FVariance[i], -1, FP_CUTOFF) <> 0 then
    begin
      Result := Result + FVariance[i];
      inc(numVals);
    end;
  if numVals > 0 then
    Result := Result/numVals
  else
    Result := 0;
end;

function TPartitionsBLensSummary.GetNumSubsamples: Integer;
begin
  Result := FSubsampleCount;
end;

function TPartitionsBLensSummary.MeanAndStdDevOfRawBLens(var aMean: Extended; var aStdDev: Extended): Boolean;
begin
  Result := False;
  if FRawBLensCount = 0 then
    Exit;
  meanandstddev(slice(FRawBLens^, FRawBLensCount), aMean, aStdDev);
  Result := True;
end;

constructor TPartitionsBLensSummary.Create(aPartitionString: String; numSamples: Integer; numRepsPerSample: Integer);
var
  i: Integer;
begin
  FNumRepsPerSample := numRepsPerSample;
  FSubsampleCount := numSamples;
  FRawBLensCount := 0;
  FPartitionString := aPartitionString;
  GetMem(FRawBLens, SizeOf(Extended)*(numSamples*numRepsPerSample + 1));
  for i := 0 to numSamples*numRepsPerSample do
    FRawBLens[i] := 0;
  GetMem(FAvgBLens, SizeOf(Extended)*(numSamples + 1));
  GetMem(FVariance, SizeOf(Extended)*(numSamples + 1));
  GetMem(FFrequencies, SizeOf(Integer)*(numSamples + 1));
  for i := 0 to numSamples do
  begin
    FAvgBLens[i] := -1;
    FVariance[i] := -1;
    FFrequencies[i] := -1;
  end;
end;

destructor TPartitionsBLensSummary.Destroy;
begin
  FreeMemAndNil(FRawBLens);
  FreeMemAndNil(FAvgBLens);
  FreeMemAndNil(FVariance);
  FreeMemAndNil(FFrequencies);
  inherited Destroy;
end;

procedure TPartitionsBLensSummary.AddBranchLengthInfo(avgBlensList: TAvgBLensList; subSampleIndex: Integer);
var
  aMean: Extended = 0;
  aStdDev: Extended = 0;
  s: String = '';
  i: Integer = 0;
begin
  if not (FPartitionString = avgBlensList.PartitionString) then
    raise Exception.Create(Format('invalid comparison of not same partitions. Expected - %s but got %s', [FPartitionString, avgBlensLIst.PartitionString]));
  if CompareValue(avgBlensList.Frequency/FNumRepsPerSample, MIN_FREQUENCY_CUTOFF, FP_CUTOFF) <= 0 then
    Exit;
  avgBlensList.GetPartitionInfo(aMean, aStdDev, s, i);
  FAvgBLens[subSampleIndex] := aMean;
  FVariance[subSampleIndex] := aStdDev*aStdDev;
  FFrequencies[subSampleIndex] := avgBlensList.Count;
  if avgBLensList.Count > 0 then { keep the raw branch length data for development and testing purposes}
    for i := 0 to avgBLensList.Count - 1 do
    begin
      FRawBLens[FRawBLensCount] := avgBLensList[i];
      inc(FRawBLensCount);
    end;
end;

function TPartitionsBLensSummary.GetCsvString(isBootstrap: Boolean; numSamples: Integer): String;
var
  i: Integer;
begin
  Assert(numSamples <= FSubsampleCount, 'invalid numSamples requested for blens summary csv string');
  if FSubsampleCount > 1 then
  begin
    Result := Format('{ %s },%.3e', [FPartitionString, MeanVariance]);
    for i := 0 to numSamples - 1 do
      Result := Result + Format(',%.3e,%.3e,%d', [FAvgBLens[i], FVariance[i], FFrequencies[i]]);
  end
  else
  begin
    if isBootstrap then
      Result := Format('{ %s },%.3e,%.3e,%d', [FPartitionString, FVariance[0], FAvgBLens[0], FFrequencies[0]])
    else
      Result := Format('{ %s },%.3e', [FPartitionString, FAvgBLens[0]]);
  end;
end;

class function TPartitionsBLensSummary.GetCsvHeaderString(numSubsamples: Integer; isBootStrap: Boolean): String;
var
  i: Integer;
begin
  if numSubsamples > 1 then
  begin
    Result := 'partition,mean_blens_variance';
    if numSubsamples > 0 then
      for i := 1 to numSubsamples do
        Result := Result + Format(',mean_blen_%d,blens_variance_%d,frequency_%d', [i, i, i]);
  end
  else
  begin
    if isBootstrap then
      Result := 'partition,blen_variance,mean_blen,frequency'
    else
      Result := 'partition,blen';
  end;
end;

end.

