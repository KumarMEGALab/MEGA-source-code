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

unit mcalibrationdensity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MegaConsts;

const
  MIN_CUMULATIVE_PROB = 0.025;
  MAX_CUMULATIVE_PROB = 0.975;

type

  { TFloatPoint }

  TFloatPoint = class(TObject)
    public
      X: Double;
      Y: Double;
      constructor Create(ax, ay: Double);
  end;

  TFloatPointArray = array of TFloatPoint;

  { TCalibrationDensity }

  TCalibrationDensity = class(TObject)
    private
      FMinQuantile: Double;
      FMaxQuantile: Double;
      FCalibrationDensityDistribution: TCalibrationDensityDistribution;
      FFinalizedMaxTime: Double;
      FFinalizedMinTime: Double;
      FMean: Double;
      FLambda: Double;
      FStdDev: Double;
      FTime: Double;
      FMinTime: Double;
      FMaxTime: Double;
      procedure SetCalibrationDensityDistribution(AValue: TCalibrationDensityDistribution);
      procedure SetFinalizedMaxTime(AValue: Double);
      procedure SetFinalizedMinTime(AValue: Double);
      procedure Initialize;
    protected
      function NormalMin: Double;
      function NormalMax: Double;
      function LogNormalMin: Double;
      function LogNormalMax: Double;
      function ExponentialMin: Double;
      function ExponentialMax: Double;
      function ExponentialProbForQuantile(q: Double): Double;
      function UniformMin: Double;
      function UniformMax: Double;
      function XYValsForNormal: TFloatPointArray;
      function XYValsForLogNormal: TFloatPointArray;
      function XYValsForExponential: TFloatPointArray;
      function XYValsForUniform: TFloatPointArray;
      procedure SampleTimesFromNormalDistribution(var sampleMin: Extended; var SampleMax: Extended; const repNum: Integer);
      procedure SampleTimesFromLogNormalDistribution(var sampleMin: Extended; var SampleMax: Extended; const repNum: Integer);
      procedure SampleTimesFromExponentialDistribution(var sampleMin: Extended; var SampleMax: Extended; const repNum: Integer);
      procedure SampleTimesFromUniformDistribution(var sampleMin: Extended; var SampleMax: Extended);
      function MinRandomQuantile: Double;
      function MaxRandomQuantile: Double;
      procedure InitQuantiles;
    public
      constructor CreateNormalDist(mean: Double; stdev: Double);
      constructor CreateLogNormalDist(aTime: Double; aMean: Double; stdev: Double);
      constructor CreateExponentialDistUsingLambda(time: Double; aLambda: Double);
      constructor CreateExponentialDistUsingMean(time: Double; aMean: Double);
      constructor CreateUniformDist(minTime: Double; maxTime: Double);
      destructor Destroy; override;
      function AsCalibrationString: String;
      function WriteToFile(var SessionFile: File): Boolean;
      function LoadFromFile(var SessionFile: File): Boolean;
      function XYValsForAreaChart: TFloatPointArray;
      function MinTime: Double;
      function MaxTime: Double;
      function NormalPdf(val: Double): Double;
      function LogNormalPdf(val: Double): Double;
      function ExponentialPdf(val: Double): Double;
      function UniformPdf(val: Double): Double;
      function ParamsCaptionString: String;
      function DistributionNameString: String;
      procedure GetSampleTimesFromDistribution(var sampleMin: Extended; var SampleMax: Extended; const repNum: Integer);
      procedure Assign(Source: TCalibrationDensity);
      property CalibrationDensityDistribution: TCalibrationDensityDistribution read FCalibrationDensityDistribution write SetCalibrationDensityDistribution;
      property Mean: Double read FMean;
      property StdDev: Double read FStdDev;
      property Lambda: Double read FLambda;
      property Time: Double read FTime;
      property FinalizedMinTime: Double read FFinalizedMinTime write SetFinalizedMinTime;
      property FinalizedMaxTime: Double read FFinalizedMaxTime write SetFinalizedMaxTime;
  end;
 {$IFDEF DEBUG}
  procedure FloatPointArrayToFile(a: TFloatPointArray; filename: String);
 {$ENDIF}
implementation

uses
  typ, spe, typinfo, StringUtils, math, MegaUtils;

{$IFDEF DEBUG}
procedure FloatPointArrayToFile(a: TFloatPointArray; filename: String);
var
  i: Integer;
  p: TFloatPoint;
  aList: TStringList = nil;
begin
  try
    aList := TStringList.Create;
    if Length(a) > 0 then
      for i := 0 to Length(a) - 1 do
      begin
        p := a[i];
        aList.Add(Format('x: %.4f, y: %.4f', [p.X, p.Y]));
      end;
    aList.SaveToFile(filename);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;
{$ENDIF}

{ TFloatPoint }

constructor TFloatPoint.Create(ax, ay: Double);
begin
  X := ax;
  Y := ay;
end;

{ TCalibrationDensity }

function TCalibrationDensity.NormalPdf(val: Double): Double;
var
  numerator, denominator: Extended;
begin
  numerator := 1;
  denominator := sqrt(2*pi*FStdDev*FStdDev);
  Result := numerator/denominator;
  denominator := exp(((val - FMean)*(val - FMean))/(2*FStdDev*FStdDev));
  Result := Result/denominator;
end;

function TCalibrationDensity.LogNormalPdf(val: Double): Double;
var
  numerator, denominator, factor1, factor2: Extended;
begin
  factor1 := 1/(sqrt(2*pi)*val*FStdDev);
  numerator := power(ln(val) - FMean, 2);
  denominator := 2*FStdDev*FStdDev;
  factor2 := 1/exp(numerator/denominator);
  Result := factor1*factor2;
end;

function TCalibrationDensity.ExponentialPdf(val: Double): Double;
begin
  if val < 0 then
    Result := 0
  else if CompareValue(exp(FLambda*Val), 0.0, FP_CUTOFF) <> 0 then
    Result := FLambda/exp(FLambda*Val)
  else
    Result := 0;
end;

function TCalibrationDensity.UniformPdf(val: Double): Double;
begin
  Result := 0.0;
  if (val < FMinTime) or (val > FMaxTime) or (CompareValue(FMaxTime, 0.0, FP_CUTOFF) = 0) then
    Exit;
  if CompareValue(FMaxTime - FMinTime, 0.0, FP_CUTOFF) = 0 then
    Result := 1
  else
    Result := 1/(FMaxTime - FMinTime);
end;

procedure TCalibrationDensity.SetCalibrationDensityDistribution(
  AValue: TCalibrationDensityDistribution);
begin
  if FCalibrationDensityDistribution=AValue then Exit;
  FCalibrationDensityDistribution:=AValue;
end;

procedure TCalibrationDensity.SetFinalizedMaxTime(AValue: Double);
begin
  if FFinalizedMaxTime=AValue then Exit;
  FFinalizedMaxTime:=AValue;
end;

procedure TCalibrationDensity.SetFinalizedMinTime(AValue: Double);
begin
  if FFinalizedMinTime=AValue then Exit;
  FFinalizedMinTime:=AValue;
end;

procedure TCalibrationDensity.Initialize;
begin
  FFinalizedMinTime := -1;
  FFinalizedMaxTime := -1;
end;

function TCalibrationDensity.NormalMin: Double;
begin
  Result := Max(0, invnormaldist(MIN_CUMULATIVE_PROB)*FStdDev + FMean);
end;

function TCalibrationDensity.NormalMax: Double;
begin
  Result := Max(0, invnormaldist(MAX_CUMULATIVE_PROB)*FStdDev + FMean);
end;

function TCalibrationDensity.LogNormalMin: Double;
//var
//  m, s: Extended;
begin
  //m := ln(FMean*FMean/sqrt(FMean*FMean + FStdDev*FStdDev));
  //s := sqrt(ln((FMean*FMean + FStdDev*FStdDev)/FMean*FMean));
  Result := Max(0, FTime + exp(invnormaldist(MIN_CUMULATIVE_PROB)*FStdDev + FMean));
end;

function TCalibrationDensity.LogNormalMax: Double;
//var
//  m, s: Extended;
begin
  //m := ln(FMean*FMean/sqrt(FMean*FMean + FStdDev*FStdDev));
  //s := sqrt(ln((FMean*FMean + FStdDev*FStdDev)/FMean*FMean));
  Result := Max(0, FTime + exp(invnormaldist(MAX_CUMULATIVE_PROB)*FStdDev + FMean));
end;

function TCalibrationDensity.ExponentialMin: Double;
begin
  Result := Max(0, FTime + ln(1 - MIN_CUMULATIVE_PROB)/FLambda*(-1));
end;

function TCalibrationDensity.ExponentialMax: Double;
begin
  Result := Max(0, FTime + ln(1 - MAX_CUMULATIVE_PROB)/FLambda*(-1));
end;

function TCalibrationDensity.ExponentialProbForQuantile(q: Double): Double;
begin
  Result := Max(0, FTime + ln(1 - q)/FLambda*(-1));
end;

function TCalibrationDensity.UniformMin: Double;
begin
  Result := FMinTime;
end;

function TCalibrationDensity.UniformMax: Double;
begin
  Result := FMaxTime;
end;

function TCalibrationDensity.XYValsForNormal: TFloatPointArray;
var
  increment: Double;
  range: Double;
  amin, amax: Double;
  i: Integer;
  x, y: Double;
begin
  SetLength(Result, 0);
  amin := NormalMin;
  amax := NormalMax;
  if (amin > amax) then
    Exit;
  range := Max(1, amax - amin);
  increment := range/100;
  amin := Max(0, amin - increment*10);
  amax := amax + increment*10;

  SetLength(Result, ceil((amax - amin)/increment));
  x := amin;
  for i := Low(Result) to High(Result) do
  begin
    y := NormalPdf(x);
    Result[i] := TFloatPoint.Create(x, y);
    x := x + increment;
  end;
end;

function TCalibrationDensity.XYValsForLogNormal: TFloatPointArray;
var
  increment: Double;
  range: Double;
  amin, amax: Double;
  i: Integer;
  x, y: Double;
begin
  SetLength(Result, 0);
  amin := LogNormalMin;
  amax := LogNormalMax;
  if (amin > amax) then
    Exit;
  range := Max(1, amax - amin);
  increment := range/100;
  amin := Max(0, amin - increment*10);
  amax := amax + increment*10;

  SetLength(Result, ceil((amax - amin)/increment));
  x := amin;
  for i := Low(Result) to High(Result) do
  begin
    y := LogNormalPdf(x - FTime);
    Result[i] := TFloatPoint.Create(x, y);
    x := x + increment;
  end;
end;

function TCalibrationDensity.XYValsForExponential: TFloatPointArray;
var
  increment: Double;
  range: Double;
  amin, amax: Double;
  i: Integer;
  x, y: Double;
begin
  SetLength(Result, 0);
  amin := ExponentialMin;
  amax := ExponentialMax;
  if (amin > amax) then
    Exit;
  range := Max(1, amax - amin);
  increment := range/100;
  amin := amin - increment*10;
  amax := amax + increment*10;
  SetLength(Result, ceil((amax - amin)/increment));
  x := amin;
  for i := Low(Result) to High(Result) do
  begin
    y := ExponentialPdf(x);
    Result[i] := TFloatPoint.Create(x, y);
    x := x + increment;
  end;
end;

function TCalibrationDensity.XYValsForUniform: TFloatPointArray;
var
  increment: Double;
  range: Double;
  amin, amax: Double;
  i: Integer;
  x, y: Double;
begin
  SetLength(Result, 0);
  amin := UniformMin;
  amax := UniformMax;
  if (amin > amax) then
    Exit;
  range := Max(1, amax - amin);
  increment := range/10;
  amin := amin - increment*10;
  amax := amax + increment*10;

  SetLength(Result, ceil((amax - amin)/increment));
  x := amin;
  for i := Low(Result) to High(Result) do
  begin
    y := UniformPdf(x);
    Result[i] := TFloatPoint.Create(x, y);
    x := x + increment;
  end;
end;

procedure TCalibrationDensity.SampleTimesFromNormalDistribution(var sampleMin: Extended; var SampleMax: Extended; const repNum: Integer);
var
  first, second: Extended;
begin
  first := randg(FMean, FStdDev);
  second := randg(FMean, FStdDev);
  if IsDeveloper then
  begin
    sampleMin := floor(min(first, second));
    sampleMax := ceil(max(first, second));
  end
  else
  begin
    sampleMin := min(first, second);
    sampleMax := max(first, second);
  end;
end;

procedure TCalibrationDensity.SampleTimesFromLogNormalDistribution(var sampleMin: Extended; var SampleMax: Extended; const repNum: Integer);
var
  first, second: Extended;
begin
  first := RandomFromLogNormalDist(FTime, FMean, FStdDev);
  second := RandomFromLogNormalDist(FTime, FMean, FStdDev);
  if IsDeveloper then
  begin
    SampleMin := floor(min(first, second));
    SampleMax := ceil(max(first, second));
  end
  else
  begin
    SampleMin := min(first, second);
    SampleMax := max(first, second);
  end;
end;

procedure TCalibrationDensity.SampleTimesFromExponentialDistribution(var sampleMin: Extended; var SampleMax: Extended; const repNum: Integer);
var
  first, second: Extended;
begin
  first := RandomFromExponentialDist(FTime, FLambda);
  second := RandomFromExponentialDist(FTime, FLambda);
  if IsDeveloper then
  begin
    SampleMin := floor(min(first, second));
    SampleMax := ceil(max(first, second));
  end
  else
  begin
    SampleMin := min(first, second);
    SampleMax := max(first, second);
  end;
end;

procedure TCalibrationDensity.SampleTimesFromUniformDistribution(var sampleMin: Extended; var SampleMax: Extended);
var
  first, second: Extended;
  range: Extended;
begin
  range := abs(FMaxTime - FMinTime);
  first := FMinTime + Random*range;
  second := FMinTime + Random*range;
  if IsDeveloper then
  begin
    SampleMin := floor(min(first, second));
    SampleMax := ceil(max(first, second));
  end
  else
  begin
    SampleMin := min(first, second);
    SampleMax := max(first, second);
  end;
end;

function TCalibrationDensity.MinRandomQuantile: Double;
const
  q: array[0..5] of Double = (0.0, 0.1, 0.2, 0.3, 0.4, 0.5);
var
  index: Integer;
begin
  index := Round(Random(5));
  Result := q[index];
end;

function TCalibrationDensity.MaxRandomQuantile: Double;
const
  q: array[0..5] of Double = (0.51, 0.6, 0.7, 0.8, 0.9, 1.0);
var
  index: Integer;
begin
  index := Round(Random(5));
  Result := q[index];
end;

procedure TCalibrationDensity.InitQuantiles;
begin
  FMinQuantile := MinRandomQuantile;
  FMaxQuantile := MaxRandomQuantile;
end;

constructor TCalibrationDensity.CreateNormalDist(mean: Double; stdev: Double);
begin
  FCalibrationDensityDistribution := cddNormal;
  FMean := mean;
  FStdDev := stdev;
  FLambda := -1.0;
  FTime := -1.0;
  FMinTime := -1.0;
  FMaxTime := -1.0;
  FMinQuantile := -1;
  FMaxQuantile := -1;
  Initialize;
end;

constructor TCalibrationDensity.CreateLogNormalDist(aTime: Double; aMean: Double; stdev: Double);
begin
  FCalibrationDensityDistribution := cddLognormal;
  FMean := aMean;
  FStdDev := stdev;
  FLambda := 1/aMean;
  FTime := aTime;
  FMinTime := -1.0;
  FMaxTime := -1.0;
  FMinQuantile := MinRandomQuantile;
  FMaxQuantile := MaxRandomQuantile;
  Initialize;
end;

constructor TCalibrationDensity.CreateExponentialDistUsingLambda(time: Double; aLambda: Double);
begin
  FCalibrationDensityDistribution := cddExponential;
  FMean := 1/aLambda;
  FStdDev := -1.0;
  FLambda := aLambda;
  FTime := time;
  FMinTime := -1.0;
  FMaxTime := -1.0;
  FMinQuantile := MinRandomQuantile;
  FMaxQuantile := MaxRandomQuantile;
  Initialize;
end;

constructor TCalibrationDensity.CreateExponentialDistUsingMean(time: Double;aMean: Double);
begin
  FCalibrationDensityDistribution := cddExponential;
  FMean := aMean;
  FStdDev := -1.0;
  FLambda := 1/aMean;
  FTime := time;
  FMinTime := -1.0;
  FMaxTime := -1.0;
  FMinQuantile := MinRandomQuantile;
  FMaxQuantile := MaxRandomQuantile;
  Initialize;
end;

constructor TCalibrationDensity.CreateUniformDist(minTime: Double; maxTime: Double);
begin
  FCalibrationDensityDistribution := cddUniform;
  FMean := -1.0;
  FStdDev := -1.0;
  FLambda := -1.0;
  FTime := -1.0;
  FMinQuantile := -1;
  FMaxQuantile := -1;
  FMinTime := minTime;
  FMaxTime := maxTime;
  Initialize;
end;

destructor TCalibrationDensity.Destroy;
begin
  inherited Destroy;
end;

function TCalibrationDensity.AsCalibrationString: String;
begin
  Result := EmptyStr;
  case FCalibrationDensityDistribution of
    cddNormal: Result := Format('Distribution=normal mean=%.8n stddev=%.8n', [FMean, FStdDev]);
    cddLogNormal: Result := Format('Distribution=lognormal offset=%.8n mean=%.8n stddev=%.8n', [FTime, FMean, FStdDev]);
    cddExponential: Result := Format('Distribution=exponential offset=%.8n lambda=%.8n', [FTime, FLambda]);
    cddUniform: Result := Format('Distribution=uniform mintime=%.8n maxtime=%.8n', [FMinTime, FMaxTime]);
    cddNone: Result := EmptyStr;
  end;
end;

function TCalibrationDensity.WriteToFile(var SessionFile: File): Boolean;
begin
  BlockWrite(SessionFile, FMean, SizeOf(FMean));
  BlockWrite(SessionFile, FLambda, SizeOf(FLambda));
  BlockWrite(SessionFile, FStdDev, SizeOf(FStdDev));
  BlockWrite(SessionFile, FTime, SizeOf(FTime));
  BlockWrite(SessionFile, FCalibrationDensityDistribution, SizeOf(TCalibrationDensityDistribution));
  Result := True;
end;

function TCalibrationDensity.LoadFromFile(var SessionFile: File): Boolean;
begin
  BlockRead(SessionFile, FMean, SizeOf(FMean));
  BlockRead(SessionFile, FLambda, SizeOf(FLambda));
  BlockRead(SessionFile, FStdDev, SizeOf(FStdDev));
  BlockRead(SessionFile, FTime, SizeOf(FTime));
  BlockRead(SessionFile, FCalibrationDensityDistribution, SizeOf(TCalibrationDensityDistribution));
  Result := True;
end;

function TCalibrationDensity.XYValsForAreaChart: TFloatPointArray;
begin
  Result := nil;
  case FCalibrationDensityDistribution of
    cddNormal: Result := XYValsForNormal;
    cddLogNormal: Result := XYValsForLogNormal;
    cddExponential: Result := XYValsForExponential;
    cddUniform: Result := XYValsForUniform;
    cddNone: Result := nil;
  end;
end;

function TCalibrationDensity.MinTime: Double;
begin
  Result := - 1.0;
  if CompareValue(FFinalizedMinTime, -1, FP_CUTOFF) <> 0 then
    Result := FFinalizedMinTime
  else
  begin
    case FCalibrationDensityDistribution of
      cddNormal: Result := NormalMin;
      cddLogNormal: Result := LogNormalMin;
      cddExponential: Result := ExponentialMin;
      cddUniform: Result := UniformMin;
      cddNone: Result := -1.0;
    end;
  end;
end;

function TCalibrationDensity.MaxTime: Double;
begin
  Result := -1.0;
  if CompareValue(FFinalizedMaxTime, -1, FP_CUTOFF) <> 0 then
    Result := FFinalizedMaxTime
  else
  begin
    case FCalibrationDensityDistribution of
      cddNormal: Result := NormalMax;
      cddLogNormal: Result := LogNormalMax;
      cddExponential: Result := ExponentialMax;
      cddUniform: Result := UniformMax;
      cddNone: Result := -1.0;
    end;
  end;
end;

function TCalibrationDensity.ParamsCaptionString: String;
begin
  Result := EmptyStr;
  case FCalibrationDensityDistribution of
    cddNormal:
      begin
        Result := '   µ: ' + fds(FMean, 2, 8) + LineEnding;
        Result := Result + '   σ: ' + fds(FStdDev, 2, 8);
      end;
    cddLogNormal:
      begin
        Result := '   µ: ' + fds(FMean, 2, 8) + LineEnding;
        Result := Result + '   σ: ' + fds(FStdDev, 2, 8) + LineEnding;
        Result := Result + '   offset: ' + fds(FTime, 4, 8);
      end;
    cddExponential:
      begin
        Result := '   λ:    ' + fds(FLambda, 4, 8) + LineEnding;
        Result := Result + '   offset: ' + fds(FTime, 4, 8);
        {$IFNDEF VISUAL_BUILD}
        Result := Result + LineEnding + '   µ:      ' + fds(1/FLambda, 4, 8) + LineEnding;
        Result := Result + '   median: ' + fds(1/FLambda*ln(2), 4, 8);
        {$ENDIF}
      end;
    cddUniform:
      begin
        Result := '   min: ' + fds(FMinTime, 2, 8) + LineEnding;
        Result := Result + '   max: ' + fds(FMaxTime, 2, 8);
      end;
    cddNone: Result := EmptyStr;
  end;
end;

function TCalibrationDensity.DistributionNameString: String;
begin
  Result := EmptyStr;
  case FCalibrationDensityDistribution of
    cddNormal: Result := 'Normal Distribution';
    cddLogNormal: Result := 'Log-normal Distribution';
    cddExponential: Result := 'Exponential Distribution';
    cddUniform: Result := 'Uniform Distribution';
    cddNone: Result := EmptyStr;
  end;
end;

procedure TCalibrationDensity.GetSampleTimesFromDistribution(var sampleMin: Extended; var SampleMax: Extended; const repNum: Integer);
begin
  case FCalibrationDensityDistribution of
    cddNormal: SampleTimesFromNormalDistribution(sampleMin, sampleMax, repNum);
    cddLogNormal: SampleTimesFromLogNormalDistribution(sampleMin, sampleMax, repNum);
    cddExponential: SampleTimesFromExponentialDistribution(sampleMin, sampleMax, repNum);
    cddUniform: SampleTimesFromUniformDistribution(sampleMin, sampleMax);
    cddNone: raise Exception.Create('invalid calibration density distribution');
  end;
end;

procedure TCalibrationDensity.Assign(Source: TCalibrationDensity);
begin
  FMean := Source.FMean;
  FStdDev := Source.FStdDev;
  FLambda := Source.FLambda;
  FTime := Source.FTime;
  FMinTime := Source.FMinTime;
  FMaxTime := Source.FMaxTime;
  CalibrationDensityDistribution := Source.CalibrationDensityDistribution;
  FFinalizedMinTime := Source.FFinalizedMinTime;
  FFinalizedMaxTime := Source.FFinalizedMaxTime;
end;

end.

