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

unit mhistogram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MegaConsts;

type

  { TBin }

  TBin = class(TObject)
    private
      FBin: Int64;
      FFrequency: Int64;
      procedure SetFrequency(AValue: Int64);
    protected

    public
      constructor Create(aBin: Int64);

      property Bin: Int64 read FBin;
      property Frequency: Int64 read FFrequency write SetFrequency;
  end;

  TBinArray = array of TBin;


  { TIntegerHistogram }

  TIntegerHistogram = class(TObject)
    protected

    public
      constructor Create;
      destructor Destroy; override;
      function GenerateHistogram(const sampleData: TDivTimesArray): TBinArray; virtual;
  end;

  THistogramWriter = class abstract(TObject)
    public
      function GetStrings(binArray: TBinArray): TStringList; virtual; abstract;
      function WriteToFile(binArray: TBinArray; filename: String): Boolean; virtual; abstract;
  end;

  { TCsvHistogramWriter }

  TCsvHistogramWriter = class(THistogramWriter)
    function GetStrings(binArray: TBinArray): TStringList; override;
    function WriteToFile(binArray: TBinArray; filename: String): Boolean; override;
  end;

implementation

uses
  math, MegaUtils;

{ TCsvHistogramWriter }

function TCsvHistogramWriter.GetStrings(binArray: TBinArray): TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  Result.Add('bin,frequency');
  if Length(binArray) > 0 then
    for i := Low(binArray) to High(binArray) do
      Result.Add(Format('%d,%d', [binArray[i].Bin, binArray[i].Frequency]));
end;

function TCsvHistogramWriter.WriteToFile(binArray: TBinArray; filename: String): Boolean;
var
  aFile: TextFile;
  i: Integer;
begin
  try
    AssignFile(aFile, filename);
    Rewrite(aFile);
    WriteLn(aFile, 'bin,frequency');
    if Length(binArray) > 0 then
      for i := Low(binArray) to High(binArray) do
        WriteLn(aFile, Format('%d,%d',[binArray[i].Bin, binArray[i].Frequency]));
  finally
    CloseFile(aFile);
  end;
  Result := FileExists(filename);
end;

{ TBin }

procedure TBin.SetFrequency(AValue: Int64);
begin
  if FFrequency=AValue then Exit;
  FFrequency:=AValue;
end;

constructor TBin.Create(aBin: Int64);
begin
  FBin := aBin;
  FFrequency := 0;
end;

{ TIntegerHistogram }

constructor TIntegerHistogram.Create;
begin

end;

destructor TIntegerHistogram.Destroy;
begin
  inherited Destroy;
end;

function TIntegerHistogram.GenerateHistogram(const sampleData: TDivTimesArray): TBinArray;
var
  numBins: Integer;
  i: Integer;
  index: Integer;
  aMin: Extended = -1;
  aMax: Extended = -1;
  minBin, maxBin: Int64;
begin
  FindMinAndMaxTimes(sampleData, aMin, aMax);
  minBin := floor(aMin);
  maxBin := ceil(aMax);
  numBins := maxBin - minBin + 1;
  SetLength(Result, numBins);
  for i := Low(Result) to High(Result) do
    Result[i] := TBin.Create(minBin + i);
  for i := Low(sampleData) to High(sampleData) do
  begin
    index := floor(sampleData[i] - minBin);
    Assert((index >= Low(Result)) and (index <= High(Result)), Format('invalid index at bin-%d: %d', [i + 1, index]));
    Result[index].Frequency := Result[index].Frequency + 1;
  end;
end;

end.

