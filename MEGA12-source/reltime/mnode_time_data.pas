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

unit mnode_time_data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mextendedlist;

type

  { TNodeTimeData }

  TNodeTimeData = class(TObject)
    private
      FTempCalculator: TExtendedList;
    public
      NodeIndex: Integer;
      NodeId: String;
      ReltimeCalculator: TExtendedList;
      MinReltimeCalculator: TExtendedList;
      MaxReltimeCalculator: TExtendedList;
      StdErrCalculator: TExtendedList;
      DivtimeCalculator: TExtendedList;
      MinDivtimeCalculator: TExtendedList;
      MaxDivtimeCalculator: TExtendedList;

      function Reltime: Extended;
      function StdErr: Extended;
      function MinReltime: Extended;
      function MaxReltime: Extended;
      function DivTime: Extended;
      function MinDivTime: Extended;
      function MaxDivTime: Extended;

      function MeanStdDevReltimeDeltaUpperBound(useMean: Boolean; var aMean: Extended; var aStdDev: Extended): Boolean;
      function MeanStdDevReltimeDeltaLowerBound(useMean: Boolean; var aMean: Extended; var aStdDev: Extended): Boolean;
      function MeanStdDevDivtimeDeltaUpperBound(useMean: Boolean; var aMean: Extended; var aStdDev: Extended): Boolean;
      function MeanStdDevDivtimeDeltaLowerBound(useMean: Boolean; var aMean: Extended; var aStdDev: Extended): Boolean;

      function ToCsvStringList(aFormatStr: String): TStringList;
      function ToCsvString(aFormatStr: String): String;
      function CsvStringToFile(filename: String; aFormatStr: String): Boolean;
      constructor Create(aNodeId: String);
      destructor Destroy; override;
      procedure SaveToSessionFile(var aFile: File);
      procedure LoadFromSessionFile(var aFile: File);
  end;

  TNodeTimeDataArray = array of TNodeTimeData;
  TNodeTimeDataArrayArray = array of TNodeTimeDataArray;

  { TMultiSampleNodeTimeData }

  TMultiSampleNodeTimeData = class(TObject)
    private
      FHasCalibrations: Boolean;
      FTempCalculator: TExtendedList;
      FReltime: Extended;
      FDivTime: Extended;
      FReltimeUpperBound: Extended;
      FReltimeLowerBound: Extended;
      FDivtimeUpperBound: Extended;
      FDivtimeLowerBound: Extended;
      FStdDev: Extended;
      FNodeId: String;
      FFinalized: Boolean;
      FNodeIndex: Integer;
      FNumSamples: Integer;
      FData: TNodeTimeDataArray;
      function GetNodeTimeData(Index: Integer): TNodeTimeData;
      procedure SetNodeTimeData(Index: Integer; AValue: TNodeTimeData);
      function FinalizeUsingMean: Boolean;
      function FinalizeUsingMedian: Boolean; deprecated 'use FinalizeUsingMean as using the median introduced problems with trees having high uncertainty';
    public
      constructor Create(numSamples: Integer; aNodeId: String);
      destructor Destroy; override;
      {$IFDEF DEBUG}
      procedure SetFinalized(aValue: Boolean);
      {$ENDIF}
      function Finalize: Boolean;
      function Reltime: Extended;
      function StdErr: Extended;
      function MinReltime: Extended;
      function MaxReltime: Extended;
      function DivTime: Extended;
      function MinDivTime: Extended;
      function MaxDivTime: Extended;
      procedure Reset(numSamples: Integer);
      procedure SaveToSessionFile(var aFile: File);
      procedure LoadFromSessionFile(var aFile: File);
      property NodeId: String read FNodeId write FNodeId;
      property NodeIndex: Integer read FNodeIndex write FNodeIndex;
      property Items[Index: Integer]: TNodeTimeData read GetNodeTimeData write SetNodeTimeData; default;
      property HasCalibrations: Boolean read FHasCalibrations write FHasCalibrations;
  end;

  TMultiSampleNodeTimeDataArray = array of TMultiSampleNodeTimeData;

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ENDIF}
  math, MegaConsts;

{ TMultiSampleNodeTimeData }

function TMultiSampleNodeTimeData.GetNodeTimeData(Index: Integer): TNodeTimeData;
begin
  if Index < Length(FData) then
    Result := FData[Index]
  else
    Result := nil;
end;

procedure TMultiSampleNodeTimeData.SetNodeTimeData(Index: Integer; AValue: TNodeTimeData);
begin
  if Index < Length(FData) then
  begin
    if FData[Index] <> AValue then
      FData[Index] := AValue
  end
  else
    raise Exception.Create('list index out of bounds');
end;

function TMultiSampleNodeTimeData.FinalizeUsingMean: Boolean;
var
  aSample: Integer = -1;
  aMean: Extended = -1;
  aStdDev: Extended = -1;
  sampleMean: Extended = 0;
  sampleStdDev: Extended = 0;
  q: Extended = -1;
begin
  Result := True;
  FTempCalculator.ClearItems;
  for aSample := 0 to FNumSamples - 1 do
    FData[aSample].ReltimeCalculator.GetValues(FTempCalculator);
  FReltime := FTempCalculator.GetMean;

  FTempCalculator.ClearItems;
  for aSample := 0 to FNumSamples - 1 do
    FData[aSample].StdErrCalculator.GetValues(FTempCalculator);
  FStdDev := FTempCalculator.GetMean;

  if HasCalibrations then
  begin
    FTempCalculator.ClearItems;
    for aSample := 0 to FNumSamples - 1 do
      FData[aSample].DivtimeCalculator.GetValues(FTempCalculator);
    FDivtime := FTempCalculator.GetMean;
  end;

  FTempCalculator.ClearItems;
  aMean := 0;
  aStdDev := 0;
  for aSample := 0 to FNumSamples - 1 do
  begin
    if FData[aSample].MeanStdDevReltimeDeltaUpperBound(True, sampleMean, sampleStdDev) then
    begin
      aMean := aMean + sampleMean;
      aStdDev := aStdDev + sampleStdDev;
      FTempCalculator.Add(sampleMean);
    end;
  end;
  if FTempCalculator.Count > 0 then
  begin
    aMean := aMean/FTempCalculator.Count;
    aStdDev := aStdDev/FTempCalculator.Count;
    q := FTempCalculator.Quantile(0.975);
    if CompareValue(aMean, 0, FP_CUTOFF) > 0 then
      FReltimeUpperBound := FReltime + aMean + q*aStdDev/aMean
    else
      FReltimeUpperBound := FReltime;
  end
  else
    FReltimeUpperBound := 0;
  if CompareValue(FReltime, FReltimeUpperBound, FP_CUTOFF) > 0 then
    raise Exception.Create(Format('invalid CI - reltime is %.8f but upper bound is %.8f', [FReltime, FReltimeUpperBound]));

  FTempCalculator.ClearItems;
  aMean := 0;
  aStdDev := 0;
  for aSample := 0 to FNumSamples - 1 do
  begin
    if FData[aSample].MeanStdDevReltimeDeltaLowerBound(True, sampleMean, sampleStdDev) then
    begin
      aMean := aMean + sampleMean;
      aStdDev += sampleStdDev;
      FTempCalculator.Add(sampleMean);
    end;
  end;
  if FTempCalculator.Count > 0 then
  begin
    aMean := aMean/FTempCalculator.Count;
    aStdDev := aStdDev/FTempCalculator.Count;
    q := FTempCalculator.Quantile(0.975);
    if CompareValue(aMean, 0, FP_CUTOFF) > 0 then
      FReltimeLowerBound := max(0, FReltime - aMean - q*aStdDev/aMean)
    else
      FReltimeLowerBound := FReltime;
  end
  else
    FReltimeLowerBound := 0;
  if CompareValue(FReltime, FReltimeLowerBound, FP_CUTOFF) < 0 then
    raise Exception.Create(Format('invalid CI - reltime is %.8f but lower bound is %.8f', [FReltime, FReltimeLowerBound]));

  if HasCalibrations then
  begin
    FTempCalculator.ClearItems;
    aMean := 0;
    aStdDev := 0;
    for aSample := 0 to FNumSamples - 1 do
    begin
      if FData[aSample].MeanStdDevDivtimeDeltaUpperBound(True, sampleMean, sampleStdDev) then
      begin
        aMean := aMean + sampleMean;
        aStdDev += sampleStdDev;
        FTempCalculator.Add(sampleMean);
      end;
    end;
    if FTempCalculator.Count > 0 then
    begin
      aMean := aMean/FTempCalculator.Count;
      aStdDev := aStdDev/FTempCalculator.Count;
      q := FTempCalculator.Quantile(0.975);
      if CompareValue(aMean, 0, FP_CUTOFF) > 0 then
        FDivtimeUpperBound := FDivTime + aMean + q*aStdDev/aMean
      else
        FDivtimeUpperBound := FDivTime;
    end
    else
      FDivTimeUpperBound := 0;
    if CompareValue(FDivTime, FDivtimeUpperBound, FP_CUTOFF) > 0 then
    begin
      //{$IFNDEF VISUAL_BUILD}{$IFDEF DEBUG}
      //debug := NextAvailableFilenameNV('_INVALID_CI.txt');
      //for aSample := 0 to FNumSamples - 1 do
      //  FData[aSample].CsvStringToFile(debug, '%.2f');
      //{$ENDIF}{$ENDIF}
      raise Exception.Create(Format('invalid CI - divtime is %.8f but upper bound is %.8f.', [FDivTime, FDivtimeUpperBound]));
    end;
  end;

  if HasCalibrations then
  begin
    FTempCalculator.ClearItems;
    aMean := 0;
    aStdDev := 0;
    for aSample := 0 to FNumSamples - 1 do
    begin
      if FData[aSample].MeanStdDevDivtimeDeltaLowerBound(True, sampleMean, sampleStdDev) then
      begin
        aMean := aMean + sampleMean;
        aStdDev += sampleStdDev;
        FTempCalculator.Add(sampleMean);
      end;
    end;
    if FTempCalculator.Count > 0 then
    begin
      aMean := aMean/FTempCalculator.Count;
      aStdDev := aStdDev/FTempCalculator.Count;
      q := FTempCalculator.Quantile(0.975);
      if CompareValue(aMean, 0, FP_CUTOFF) > 0 then
        FDivtimeLowerBound := max(0, FDivTime - aMean - q*aStdDev/aMean)
      else
        FDivTimeLowerBound := FDivTime;
    end
    else
      FDivTimeLowerBound := 0;
    if CompareValue(FDivTime, FDivTimeLowerBound, FP_CUTOFF) < 0 then
      raise Exception.Create(Format('invalid CI - divtime is %.8f but lower bound is %.8f.', [FDivTime, FDivTimeLowerBound]));
  end;

  FFinalized := True;
end;

function TMultiSampleNodeTimeData.FinalizeUsingMedian: Boolean;
var
  aSample: Integer = -1;
begin
  Result := True;
  FTempCalculator.ClearItems;
  for aSample := 0 to FNumSamples - 1 do
    FTempCalculator.Add(FData[aSample].ReltimeCalculator.GetMedian);
  FReltime := FTempCalculator.GetMedian;

  FTempCalculator.ClearItems;
  for aSample := 0 to FNumSamples - 1 do
    FTempCalculator.Add(FData[aSample].StdErrCalculator.GetMedian);
  FStdDev := FTempCalculator.GetMedian;

  if HasCalibrations then
  begin
    FTempCalculator.ClearItems;
    for aSample := 0 to FNumSamples - 1 do
      FTempCalculator.Add(FData[aSample].DivtimeCalculator.GetMedian);
    FDivtime := FTempCalculator.GetMedian;
  end;

  FTempCalculator.ClearItems;
  for aSample := 0 to FNumSamples - 1 do
    FTempCalculator.Add(FData[aSample].MaxReltimeCalculator.GetMedian);
  FReltimeUpperBound := FTempCalculator.GetMedian;

  if CompareValue(FReltime, FReltimeUpperBound, FP_CUTOFF) > 0 then
    raise Exception.Create(Format('invalid CI - reltime is %.8f but upper bound is %.8f', [FReltime, FReltimeUpperBound]));

  FTempCalculator.ClearItems;
  for aSample := 0 to FNumSamples - 1 do
    FTempCalculator.Add(FData[aSample].MinReltimeCalculator.GetMedian);
  FReltimeLowerBound := FTempCalculator.GetMedian;

  if CompareValue(FReltime, FReltimeLowerBound, FP_CUTOFF) < 0 then
    raise Exception.Create(Format('invalid CI - reltime is %.8f but lower bound is %.8f', [FReltime, FReltimeLowerBound]));

  if HasCalibrations then
  begin
    FTempCalculator.ClearItems;
    for aSample := 0 to FNumSamples - 1 do
      FTempCalculator.Add(FData[aSample].MaxDivtimeCalculator.GetMedian);
    FDivtimeUpperBound := FTempCalculator.GetMedian;

    if CompareValue(FDivTime, FDivtimeUpperBound, FP_CUTOFF) > 0 then
      raise Exception.Create(Format('invalid CI - divtime is %.8f but upper bound is %.8f.', [FDivTime, FDivtimeUpperBound]));
  end;

  if HasCalibrations then
  begin
    FTempCalculator.ClearItems;
    for aSample := 0 to FNumSamples - 1 do
      FTempCalculator.Add(FData[aSample].MinDivtimeCalculator.GetMedian);
    FDivTimeLowerBound := FTempCalculator.GetMedian;

    if CompareValue(FDivTime, FDivTimeLowerBound, FP_CUTOFF) < 0 then
      raise Exception.Create(Format('invalid CI - divtime is %.8f but lower bound is %.8f.', [FDivTime, FDivTimeLowerBound]));
  end;

  FFinalized := True;
end;

constructor TMultiSampleNodeTimeData.Create(numSamples: Integer; aNodeId: String);
var
  aSample: Integer;
begin
  FHasCalibrations := False;
  FFinalized := False;
  FNumSamples := numSamples;
  SetLength(FData, FNumSamples);
  FNodeId := aNodeId;
  for aSample := Low(FData) to High(FData) do
    FData[aSample] := TNodeTimeData.Create(aNodeId);
  FTempCalculator := TExtendedList.Create;
end;

destructor TMultiSampleNodeTimeData.Destroy;
var
  aSample: Integer = -1;
begin
  if Length(FData) > 0 then
    for aSample := Low(FData) to High(FData) do
      if Assigned(FData[aSample]) then
        FData[aSample].Free;
  SetLength(FData, 0);
  if Assigned(FTempCalculator) then
    FTempCalculator.Free;
  inherited Destroy;
end;

{$IFDEF DEBUG}
procedure TMultiSampleNodeTimeData.SetFinalized(aValue: Boolean);
begin
  FFinalized := aValue;
end;
{$ENDIF}

function TMultiSampleNodeTimeData.Finalize: Boolean;
begin
  Result := FinalizeUsingMean;
end;

function TMultiSampleNodeTimeData.Reltime: Extended;
begin
  if not FFinalized then
    Finalize;
  Result := FReltime;
end;

function TMultiSampleNodeTimeData.StdErr: Extended;
begin
  if not FFinalized then
    Finalize;
  Result := FStdDev;
end;

function TMultiSampleNodeTimeData.MinReltime: Extended;
begin
  if not FFinalized then
    Finalize;
  Result := FReltimeLowerBound;
end;

function TMultiSampleNodeTimeData.MaxReltime: Extended;
begin
  if not FFinalized then
    Finalize;
  Result := FReltimeUpperBound;
end;

function TMultiSampleNodeTimeData.DivTime: Extended;
begin
  if not FFinalized then
    Finalize;
  Result := FDivTime;
end;

function TMultiSampleNodeTimeData.MinDivTime: Extended;
begin
  if not FFinalized then
    Finalize;
  Result := FDivtimeLowerBound;
end;

function TMultiSampleNodeTimeData.MaxDivTime: Extended;
begin
  if not FFinalized then
    Finalize;
  Result := FDivtimeUpperBound;
end;

procedure TMultiSampleNodeTimeData.Reset(numSamples: Integer);
var
  i: Integer = -1;
begin
  if Length(FData) > 0 then
    for i := Low(FData) to High(FData) do
      if Assigned(FData[i]) then
        FData[i].Free;
  SetLength(FData, numSamples);
  for i := Low(FData) to High(FData) do
  begin
    FData[i] := TNodeTimeData.Create(NodeId);
    FData[i].NodeIndex := NodeIndex;
  end;
  FFinalized := False;
end;

procedure TMultiSampleNodeTimeData.SaveToSessionFile(var aFile: File);
var
  NumChars: Integer = -1;
  Buffer: Array[0..32767] of AnsiChar;
  i: Integer = -1;
begin
  if not FFinalized then
    raise Exception.Create('trying to save session for TMultiTreeNodeTimeData which has not been finalized');
  for i := Low(Buffer) to High(Buffer) do
    Buffer[i] := #0;
  BlockWrite(aFile, FHasCalibrations, SizeOf(FHasCalibrations));
  BlockWrite(aFile, FReltime, SizeOf(FReltime));
  BlockWrite(aFile, FDivTime, SizeOf(FDivTime));
  BlockWrite(aFile, FReltimeUpperBound, SizeOf(FReltimeUpperBound));
  BlockWrite(aFile, FReltimeLowerBound, SizeOf(FReltimeLowerBound));
  BlockWrite(aFile, FDivtimeUpperBound, SizeOf(FDivtimeUpperBound));
  BlockWrite(aFile, FDivtimeLowerBound, SizeOf(FDivtimeLowerBound));
  BlockWrite(aFile, FStdDev, SizeOf(FStdDev));
  BlockWrite(aFile, FFinalized, SizeOf(FFinalized));
  BlockWrite(aFile, FNodeIndex, SizeOf(FNodeIndex));
  BlockWrite(aFile, FNumSamples, SizeOf(FNumSamples));
  NumChars := Length(FNodeId);
  BlockWrite(aFile, NumChars, SizeOf(Integer));
  if NumChars > 0 then
  begin
    StrPCopy(Buffer, FNodeId);
    BlockWrite(aFile, Buffer, NumChars);
  end;
end;

procedure TMultiSampleNodeTimeData.LoadFromSessionFile(var aFile: File);
var
  NumChars: Integer = -1;
  Buffer: Array[0..32767] of AnsiChar;
  i: Integer = -1;
begin
  for i := Low(Buffer) to High(Buffer) do
    Buffer[i] := #0;
  BlockRead(aFile, FHasCalibrations, SizeOf(FHasCalibrations));
  BlockRead(aFile, FReltime, SizeOf(FReltime));
  BlockRead(aFile, FDivTime, SizeOf(FDivTime));
  BlockRead(aFile, FReltimeUpperBound, SizeOf(FReltimeUpperBound));
  BlockRead(aFile, FReltimeLowerBound, SizeOf(FReltimeLowerBound));
  BlockRead(aFile, FDivtimeUpperBound, SizeOf(FDivtimeUpperBound));
  BlockRead(aFile, FDivtimeLowerBound, SizeOf(FDivtimeLowerBound));
  BlockRead(aFile, FStdDev, SizeOf(FStdDev));
  BlockRead(aFile, FFinalized, SizeOf(FFinalized));
  BlockRead(aFile, FNodeIndex, SizeOf(FNodeIndex));
  BlockRead(aFile, FNumSamples, SizeOf(FNumSamples));
  BlockRead(aFile, NumChars, SizeOf(Integer));
  if NumChars > 0 then
  begin
    BlockRead(aFile, Buffer, NumChars);
    Buffer[NumChars] := #0;
    FNodeId := StrPas(Buffer);
  end;
end;

{ TNodeTimeData }

function TNodeTimeData.Reltime: Extended;
begin
  Result := ReltimeCalculator.GetMean;
end;

function TNodeTimeData.StdErr: Extended;
begin
  Result := StdErrCalculator.GetMean;
end;

function TNodeTimeData.MinReltime: Extended;
begin
  Result := MinReltimeCalculator.GetMean;
end;

function TNodeTimeData.MaxReltime: Extended;
begin
  Result := MaxReltimeCalculator.GetMean;
end;

function TNodeTimeData.DivTime: Extended;
begin
  Result := DivtimeCalculator.GetMean;
end;

function TNodeTimeData.MinDivTime: Extended;
begin
  Result := MinDivtimeCalculator.GetMean;
end;

function TNodeTimeData.MaxDivTime: Extended;
begin
  Result := MaxDivtimeCalculator.GetMean;
end;

function TNodeTimeData.MeanStdDevReltimeDeltaUpperBound(useMean: Boolean; var aMean: Extended; var aStdDev: Extended): Boolean;
var
  i: Integer = -1;
begin
  FTempCalculator.ClearItems;
  Assert(ReltimeCalculator.Count = MaxReltimeCalculator.Count, 'unequal number of items in TExtendedLists');
  if ReltimeCalculator.Count = 0 then
  begin
    aMean := 0;
    aStdDev := 0;
    Exit (False);
  end;
  Result := True;
  for i := 0 to ReltimeCalculator.Count - 1 do
  begin
    Assert(CompareValue(MaxReltimeCalculator[i], ReltimeCalculator[i], FP_CUTOFF) >= 0, Format('bad CI - upper is %.8f time is %.8f', [MaxReltimeCalculator[i], RelTimeCalculator[i]]));
    FTempCalculator.Add(MaxReltimeCalculator[i] - ReltimeCalculator[i]);
  end;
  if useMean then
    FTempCalculator.CalcMeanAndVariance(aMean, aStdDev)
  else
  begin
    aMean := FTempCalculator.GetMedian;
    aStdDev := FTempCalculator.GetVariance;
  end;
  if CompareValue(aStdDev, 0.0, FP_CUTOFF) > 0 then
    aStdDev := sqrt(aStdDev);
end;

function TNodeTimeData.MeanStdDevReltimeDeltaLowerBound(useMean: Boolean; var aMean: Extended; var aStdDev: Extended): Boolean;
var
  i: Integer = -1;
begin
  FTempCalculator.ClearItems;
  Assert(ReltimeCalculator.Count = MinReltimeCalculator.Count, 'unequal number of items in TExtendedLists');
  if ReltimeCalculator.Count = 0 then
  begin
    aMean := 0;
    aStdDev := 0;
    Exit (False);
  end;
  Result := True;
  for i := 0 to ReltimeCalculator.Count - 1 do
  begin
    Assert(CompareValue(ReltimeCalculator[i], MinRelTimeCalculator[i], FP_CUTOFF) >= 0, Format('bad CI - time is %.8f but lower is %.8f', [ReltimeCalculator[i], MinRelTimeCalculator[i]]));
    FTempCalculator.Add(max(0, ReltimeCalculator[i] - MinReltimeCalculator[i]));
  end;
  if useMean then
    FTempCalculator.CalcMeanAndVariance(aMean, aStdDev)
  else
  begin
    aMean := FTempCalculator.GetMedian;
    aStdDev := FTempCalculator.GetVariance;
  end;
  if CompareValue(aStdDev, 0.0, FP_CUTOFF) > 0 then
    aStdDev := sqrt(aStdDev);
end;

function TNodeTimeData.MeanStdDevDivtimeDeltaUpperBound(useMean: Boolean; var aMean: Extended; var aStdDev: Extended): Boolean;
var
  i: Integer = -1;
begin
  FTempCalculator.ClearItems;
  Assert(DivTimeCalculator.Count = MaxDivtimeCalculator.Count, 'unequal number of items in TExtendedLists');
  if DivTimeCalculator.Count = 0 then
  begin
    aMean := 0;
    aStdDev := 0;
    Exit(False);
  end;
  Result := True;
  for i := 0 to DivTimeCalculator.Count - 1 do
  begin
    Assert(CompareValue(MaxDivtimeCalculator[i], DivTimeCalculator[i], FP_CUTOFF) >= 0, Format('bad CI - upper is %.8f time is %.8f', [MaxDivtimeCalculator[i], DivTimeCalculator[i]]));
    FTempCalculator.Add(MaxDivtimeCalculator[i] - DivTimeCalculator[i]);
  end;
  if useMean then
    FTempCalculator.CalcMeanAndVariance(aMean, aStdDev)
  else
  begin
    aMean := FTempCalculator.GetMedian;
    aStdDev := FTempCalculator.GetVariance;
  end;
  if CompareValue(aStdDev, 0.0, FP_CUTOFF) > 0 then
    aStdDev := sqrt(aStdDev);
end;

function TNodeTimeData.MeanStdDevDivtimeDeltaLowerBound(useMean: Boolean; var aMean: Extended; var aStdDev: Extended): Boolean;
var
  i: Integer = -1;
begin
  FTempCalculator.ClearItems;
  Assert(DivTimeCalculator.Count = MinDivtimeCalculator.Count, 'unequal number of items in TExtendedLists');
  if DivTimeCalculator.Count = 0 then
  begin
    aMean := 0;
    aStdDev := 0;
    Exit(False);
  end;
  Result := True;
  for i := 0 to DivTimeCalculator.Count - 1 do
  begin
    Assert(CompareValue(DivtimeCalculator[i], MinDivTimeCalculator[i], FP_CUTOFF) >= 0, Format('bad CI - time is %.8f but lower is %.8f', [DivtimeCalculator[i], MinDivTimeCalculator[i]]));
    FTempCalculator.Add(DivtimeCalculator[i] - MinDivTimeCalculator[i]);
  end;
  if useMean then
    FTempCalculator.CalcMeanAndVariance(aMean, aStdDev)
  else
  begin
    aMean := FTempCalculator.GetMedian;
    aStdDev := FTempCalculator.GetVariance;
  end;
  if CompareValue(aStdDev, 0.0, FP_CUTOFF) > 0 then
    aStdDev := sqrt(aStdDev);
end;

function TNodeTimeData.ToCsvStringList(aFormatStr: String): TStringList;
begin
  Result := TStringList.Create;
  Result.Add(Format('%.20s: %s', ['MinReltime', MinReltimeCalculator.ToCsvString(aFormatStr)]));
  Result.Add(Format('%.20s: %s', ['Reltime', ReltimeCalculator.ToCsvString(aFormatStr)]));
  Result.Add(Format('%.20s: %s', ['MaxReltime', MaxReltimeCalculator.ToCsvString(aFormatStr)]));
  Result.Add(Format('%.20s: %s', ['MinDivtime', MinDivtimeCalculator.ToCsvString(aFormatStr)]));
  Result.Add(Format('%.20s: %s', ['Divtime', DivtimeCalculator.ToCsvString(aFormatStr)]));
  Result.Add(Format('%.20s: %s', ['MaxDivtime', MaxDivtimeCalculator.ToCsvString(aFormatStr)]));
  Result.Add(Format('%.20s: %s', ['StdErr', StdErrCalculator.ToCsvString(aFormatStr)]));
end;

function TNodeTimeData.ToCsvString(aFormatStr: String): String;
var
  aList: TStringList = nil;
begin
  try
    aList := ToCsvStringLIst(aFormatStr);
    Result := aList.Text;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TNodeTimeData.CsvStringToFile(filename: String; aFormatStr: String): Boolean;
var
  aFile: TextFile;
begin
  try
    AssignFile(aFile, filename);
    if FileExists(filename) then
      Append(aFile)
    else
      Rewrite(aFile);
    WriteLn(aFile, ToCsvString(aFormatStr));
  finally
    CloseFile(aFile);
  end;
  Result := FileExists(filename);
end;

constructor TNodeTimeData.Create(aNodeId: String);
begin
  FTempCalculator := TExtendedList.Create;
  NodeIndex := -1;
  NodeId := aNodeId;
  ReltimeCalculator := TExtendedList.Create;
  MinReltimeCalculator := TExtendedList.Create;
  MaxReltimeCalculator := TExtendedList.Create;
  StdErrCalculator := TExtendedList.Create;
  DivtimeCalculator := TExtendedList.Create;
  MinDivtimeCalculator := TExtendedList.Create;
  MaxDivtimeCalculator := TExtendedList.Create;
end;

destructor TNodeTimeData.Destroy;
begin
  if Assigned(FTempCalculator) then
    FTempCalculator.Free;
  if Assigned(ReltimeCalculator) then
    ReltimeCalculator.Free;
  if Assigned(MinReltimeCalculator) then
    MinReltimeCalculator.Free;
  if Assigned(MaxReltimeCalculator) then
    MaxReltimeCalculator.Free;
  if Assigned(StdErrCalculator) then
    StdErrCalculator.Free;
  if Assigned(DivtimeCalculator) then
    DivtimeCalculator.Free;
  if Assigned(MinDivtimeCalculator) then
    MinDivtimeCalculator.Free;
  if Assigned(MaxDivTimeCalculator) then
    MaxDivtimeCalculator.Free;
  inherited Destroy;
end;

procedure TNodeTimeData.SaveToSessionFile(var aFile: File);
var
  numChars: Integer = -1;
  buffer: array[0..65535] of AnsiChar;
  temp: Extended = -1;
begin
  BlockWrite(aFile, NodeIndex, SizeOf(Integer));
  temp := Reltime;
  BlockWrite(aFile, temp, SizeOf(Extended));
  temp := MinReltime;
  BlockWrite(aFile, temp, SizeOf(Extended));
  temp := MaxReltime;
  BlockWrite(aFile, temp, SizeOf(Extended));
  temp := DivTime;
  BlockWrite(aFile, temp, SizeOf(Extended));
  temp := MinDivTime;
  BlockWrite(aFile, temp, SizeOf(Extended));
  temp := MaxDivTime;
  BlockWrite(aFile, temp, SizeOf(Extended));
  temp := StdErr;
  BlockWrite(aFile, temp, SizeOf(Extended));

  numChars := Length(NodeId);
  BlockWrite(aFile, numChars, SizeOf(Integer));
  if numChars > 0 then
  begin
    StrPCopy(buffer, NodeId);
    BlockWrite(aFile, buffer, numChars);
  end;
end;

procedure TNodeTimeData.LoadFromSessionFile(var aFile: File);
var
  i: Integer = -1;
  buffer: array[0..65535] of AnsiChar;
  numChars: Integer = -1;
  aNodeIndex: Integer = -1;
  aReltime: Extended = -1;
  aStdErr: Extended = -1;
  aMinReltime: Extended = -1;
  aMaxReltime: Extended = -1;
  aDivTime: Extended = -1;
  aMinDivTime: Extended = -1;
  aMaxDivTime: Extended = -1;
begin
  for i := Low(buffer) to High(buffer) do
    buffer[i] := #0;
  BlockRead(aFile, aNodeIndex, SizeOf(Integer));
  NodeIndex := aNodeIndex;
  BlockRead(aFile, aReltime, SizeOf(Extended));
  ReltimeCalculator.Add(aReltime);
  BlockRead(aFile, aMinReltime, SizeOf(Extended));
  MinReltimeCalculator.Add(aMinRelTime);
  BlockRead(aFile, aMaxReltime, SizeOf(Extended));
  MaxReltimeCalculator.Add(aMaxReltime);
  BlockRead(aFile, aDivTime, SizeOf(Extended));
  DivtimeCalculator.Add(aDivTime);
  BlockRead(aFile, aMinDivTime, SizeOf(Extended));
  MinDivtimeCalculator.Add(aMinDivTime);
  BlockRead(aFile, aMaxDivTime, SizeOf(Extended));
  MaxDivtimeCalculator.Add(aMaxDivTime);
  BlockRead(aFile, aStdErr, SizeOf(Extended));
  StdErrCalculator.Add(aStdErr);

  BlockRead(aFile, numChars, SizeOf(Integer));
  if numChars > Length(buffer) then
    raise Exception.Create(Format('Application Error: invalid number of characters(got %.0n but limit is 65,536) when loading node time data', [numChars*1.0]));
  if numChars > 0 then
  begin
    BlockRead(aFile, buffer, numChars);
    buffer[numChars] := #0;
    NodeId := StrPas(buffer);
  end;
end;

end.

