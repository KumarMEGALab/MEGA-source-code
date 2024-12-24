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

unit mcorrelationtest;

interface

uses
  Classes, SysUtils;

const
  B0 = -0.07500227;
  B1 = 6.02875922;
  B2 = -0.29265746;
  B3 = 2.30731322;
  B4 = -3.2276037;

type

  TPvalueCategory = (pv_lt_001, pv_lt_01, pv_lt_05, pv_gt_05, pv_unknown);

  { TCorrTestData }

  TCorrTestData = class(TObject)
    private
      FDescendentRate: Extended;
      FAncestorRate: Extended;
      FAncestorRateRank: Extended;
      FDescendentRateRank: Extended;
      procedure SetAncestorRate(const Value: Extended);
      procedure SetDescendentRate(const Value: Extended);
      procedure SetAncestorRateRank(const Value: Extended);
      procedure SetDesendentRateRank(const Value: Extended);
      function GetRankDeltaSquared: Extended;
      function GetRankDelta: Extended;
    public
      constructor Create(aRate: Extended; dRate: Extended);
      function GetAsString: String;
      property AncestorRate: Extended read FAncestorRate write SetAncestorRate;
      property DescendentRate: Extended read FDescendentRate write SetDescendentRate;
      property AncestorRateRank: Extended read FAncestorRateRank write SetAncestorRateRank;
      property DescendentRateRank: Extended read FDescendentRateRank write SetDesendentRateRank;
      property RankDelta: Extended read GetRankDelta;
      property RankDeltaSquared: Extended read GetRankDeltaSquared;
  end;

  { TCorrTestDataList }

  TCorrTestDataList = class(TObject)
    private
      FData: TList;
      FAvgAncestorRank: Extended;
      FAvgDescendentRank: Extended;
      //function GetData(Index: Integer): TCorrTestData;
      procedure Clear;
      procedure SetRanks;
      procedure HandleTiedRanks;
      procedure ComputeAvgRanks;
      function ComputeSanc: Extended;
      function ComputeSdesc: Extended;
      function ComputeSxy: Extended;
      function ComputeSxSy: Extended;
      function ComputeSumDSquared: Extended;
    protected

    public
      constructor Create;
      destructor Destroy; override;
//      procedure Assign(Source: TCorrTestDataList);
      function Add(AncestorRate: Extended; DescendentRate: Extended): Integer;
      function ComputeRho: Extended;
      function SaveToFile(filename: String): Boolean;
      //property Items[Index: Longint]: TCorrTestData read GetData; default;
  end;

  { TCorrelationTest }

  TCorrelationTest = class(TObject)
     FSiblings: TCorrTestDataList;
     FDirectDescendents: TCorrTestDataList;
     FSecondDescendents: TCorrTestDataList;
     FThirdDescendents: TCorrTestDataList;
     FRhoSiblings, FRhoDirectDes, FRhoSecondDes, FRhoThirdDes: Extended;
     FRhoSiblingsNorm, FRhoDirectDesNorm: Extended;
     FDecaySecondDes, FDecayThirdDes, FDecaySecondDesNorm, FDecayThirdDesNorm: Extended;
    private
      FPvalue: TPvalueCategory;
      FScore: Extended;
      function ComputeScore: Extended;
      function ComputePValue: TPvalueCategory;
      function GetPValueString: String;
    public
      constructor Create(siblingData, des1Data, des2Data, des3Data: TCorrTestDataList);
      function Compute: Boolean;
      function Summary: TStringList;
      function SaveSummaryToFile(filename: String): Boolean;
      property Score: Extended read FScore;
      property Pvalue: TPvalueCategory read FPvalue;
      property PvalueString: String read GetPValueString;
      function GetCaptionString: String;
  end;

  function CompareAncestorsAsc(Item1: Pointer; Item2: Pointer): Integer;
  function CompareDescendentsAsc(Item1: Pointer; Item2: Pointer): Integer;
  function PValueCategoryString(aValue: TPvalueCategory; IsHTML: Boolean = False): String;

implementation

uses
  {$IFNDEF DEBUG}
  StringUtils,
  {$ENDIF}
  math;

function CompareAncestorsAsc(Item1: Pointer; Item2: Pointer): Integer;
begin
  Result := CompareValue(TCorrTestData(Item1).AncestorRate, TCorrTestData(Item2).AncestorRate);
  if Result = 0 then
    Result := CompareValue(TCorrTestData(Item1).DescendentRate, TCorrTestData(Item2).DescendentRate);
end;

function CompareDescendentsAsc(Item1: Pointer; Item2: Pointer): Integer;
begin
  Result := CompareValue(TCorrTestData(Item1).DescendentRate, TCorrTestData(Item2).DescendentRate);
  if Result = 0 then
    CompareValue(TCorrTestData(Item1).AncestorRate, TCorrTestData(Item2).AncestorRate);
end;

function PValueCategoryString(aValue: TPvalueCategory; IsHTML: Boolean = False): String;
begin
  if IsHTML then
  begin
    case aValue of
      pv_lt_001: Result := 'p-value &lt; 0.001';
      pv_lt_01: Result := 'p-value &lt; 0.01';
      pv_lt_05: Result := 'p-value &lt; 0.05';
      pv_gt_05: Result := 'p-value &gt; 0.05';
      pv_unknown: Result := 'unknown';
    end;
  end
  else
  begin
    case aValue of
      pv_lt_001: Result := 'p-value < 0.001';
      pv_lt_01: Result := 'p-value < 0.01';
      pv_lt_05: Result := 'p-value < 0.05';
      pv_gt_05: Result := 'p-value > 0.05';
      pv_unknown: Result := 'unknown';
    end;
  end;
end;

{ TCorrelationTest }

function TCorrelationTest.ComputeScore: Extended;
var
  x: Extended;
begin
  x := -(B0 + B1*FRhoSiblingsNorm + B2*FRhoDirectDesNorm + B3*FDecaySecondDesNorm + B4*FDecayThirdDesNorm);
  Result := 1/(1 + exp(x));
  FScore := Result;
end;

function TCorrelationTest.ComputePValue: TPvalueCategory;
begin
  if FScore >= 0.92 then
    Result := pv_lt_001
  else if FScore >= 0.83 then
    Result := pv_lt_01
  else if FScore >= 0.5 then
    Result := pv_lt_05
  else if FScore < 0.5 then
    Result := pv_gt_05
  else
    Result := pv_unknown;
  FPvalue:= Result;
end;

function TCorrelationTest.GetCaptionString: String;
begin
  Result := 'The null hypothesis of rate independence of evolutionary rates among lineages in the user-supplied phylogeny ';
  case FPvalue of
    pv_lt_001, pv_lt_01, pv_lt_05: Result := Result + 'was rejected (' + PValueCategoryString(FpValue, True) + ').';
    pv_gt_05: Result := Result + ' was not rejected (' + PValueCategoryString(FPvalue, True) + ').';
    pv_unknown: Result := 'The rate correlation test did not return a valid result.';
  end;
end;

function TCorrelationTest.GetPValueString: String;
begin
  Result := PValueCategoryString(FPvalue);
end;

constructor TCorrelationTest.Create(siblingData, des1Data, des2Data, des3Data: TCorrTestDataList);
begin
  FSiblings := siblingData;
  FDirectDescendents := des1Data;
  FSecondDescendents := des2Data;
  FThirdDescendents := des3Data;
end;

function TCorrelationTest.Compute: Boolean;
begin
  Result := True;
  FRhoSiblings := FSiblings.ComputeRho;
  FRhoDirectDes := FDirectDescendents.ComputeRho;
  FRhoSecondDes := FSecondDescendents.ComputeRho;
  FRhoThirdDes := FThirdDescendents.ComputeRho;
  if CompareValue(FRhoDirectDes, 0, 0.00001) <> 0 then
  begin
    FDecaySecondDes := (FRhoSecondDes - FRhoDirectDes)/FRhoDirectDes;
    FDecayThirdDes := (FRhoThirdDes - FRhoDirectDes)/FRhoDirectDes;
  end
  else
    Result := False;
  FRhoSiblingsNorm := (FRhoSiblings - 0.436462708)/0.268015804;
  FRhoDirectDesNorm := (FRhoDirectDes - 0.828259994)/0.087506404;
  FDecaySecondDesNorm := (FDecaySecondDes - ( - 0.169515205))/0.103192689;
  FDecayThirdDesNorm := (FDecayThirdDes - ( - 0.292940377))/0.163616884;
  ComputeScore;
  ComputePValue;
end;

function TCorrelationTest.Summary: TStringList;
const
  PRECISION = 8;
  DIGITS = 10;
begin
  {$IFDEF DEBUG}
  Result := TStringList.Create;
  Result.Add(Format('%.4f %.4f %.4f %.4f %.4f %.4f %.4f', [FRhoSiblings, FRhoDirectDes, FDecaySecondDes, FDecayThirdDes, FScore, FRhoSecondDes, FRhoThirdDes]));
  {$ELSE}
  Result := TStringList.Create;
  Result.Add('score   :  ' + FormatDoubleSafe(FScore, PRECISION, DIGITS, True));
  Result.Add('p-value :  ' + PValueCategoryString(FPvalue));
  Result.Add(EmptyStr);
  Result.Add(';; Detailed information about the CorrTest method, interpretation of results, and source code can be found at https://github.com/cathyqqtao/CorrTest');
  {$ENDIF}
end;

function TCorrelationTest.SaveSummaryToFile(filename: String): Boolean;
var
  aList: TStringList;
begin
  aList := nil;
  Result := False;

  try
    aList := Summary;
    aList.SaveToFile(filename);
    Result := FileExists(filename);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

{ TCorrTestData }

constructor TCorrTestData.Create(aRate: Extended; dRate: Extended);
begin
  inherited Create;
  FAncestorRate := aRate;
  FDescendentRate := dRate;
end;

function TCorrTestData.GetAsString: String;
begin
  Result := Format('%.6f %.2f %.6f %.2f %.3f', [FAncestorRate, FAncestorRateRank, FDescendentRate, FDescendentRateRank, GetRankDeltaSquared]);
end;

function TCorrTestData.GetRankDelta: Extended;
begin
  Result := (FAncestorRateRank - FDescendentRateRank);
end;

function TCorrTestData.GetRankDeltaSquared: Extended;
var
  aDelta: Extended;
begin
  aDelta := GetRankDelta;
  Result := aDelta * aDelta;
end;

procedure TCorrTestData.SetAncestorRate(const Value: Extended);
begin
  FAncestorRate := Value;
end;

procedure TCorrTestData.SetAncestorRateRank(const Value: Extended);
begin
  FAncestorRateRank := Value;
end;

procedure TCorrTestData.SetDescendentRate(const Value: Extended);
begin
  FDescendentRate := Value;
end;

procedure TCorrTestData.SetDesendentRateRank(const Value: Extended);
begin
  FDescendentRateRank := Value;
end;


{ TCorrTestDataList }

function TCorrTestDataList.Add(AncestorRate: Extended; DescendentRate: Extended
  ): Integer;
var
  aData: TCorrTestData;
begin
  if CompareValue(AncestorRate, 0.0, 0.00000000001) = 0 then
    Exit;
  if CompareValue(DescendentRate, 0.0, 0.00000000001) = 0 then
    Exit;
  aData := TCorrTestData.Create(AncestorRate, DescendentRate);
  Result := FData.Add(aData);
end;

procedure TCorrTestDataList.Clear;
var
  i: Integer;
begin
  if FData.Count > 0 then
    for i := 0 to FData.Count - 1 do
      TCorrTestData(FData[i]).Free;
end;

procedure TCorrTestDataList.ComputeAvgRanks;
var
  i: Integer;
begin
  FAvgAncestorRank := 0.0;
  FAvgDescendentRank := 0.0;
  if FData.Count > 0 then
  begin
    for i := 0 to FData.Count - 1 do
    begin
      FAvgAncestorRank := FAvgAncestorRank + TCorrTestData(FData[i]).AncestorRateRank;
      FAvgDescendentRank := FAvgDescendentRank + TCorrTestData(FData[i]).DescendentRateRank;
    end;
    FAvgAncestorRank := FAvgAncestorRank/FData.Count;
    FAvgDescendentRank := FAvgDescendentRank/FData.Count;
  end;
end;

function TCorrTestDataList.ComputeRho: Extended;
var
  SxSy: Extended;
begin
  SetRanks;
  ComputeAvgRanks;
  SxSy := ComputeSxSy;
  if CompareValue(SxSy, 0.0, 0.00000001)<> 0 then
    Result := ComputeSxy / SxSy
  else
    Result := 0;
end;

function TCorrTestDataList.SaveToFile(filename: String): Boolean;
var
  aList: TStringList;
  i: Integer;
begin
  Result := False;
  try
    aList := TStringList.Create;
    if FData.Count > 0 then
    begin
      for i := 0 to FData.Count - 1 do
        aList.Add(TCorrTestData(FData[i]).GetAsString);
      aList.SaveToFile(filename);
      Result := FileExists(filename);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TCorrTestDataList.ComputeSanc: Extended;
var
  i: Integer;
  aData: TCorrTestData;
  Sanc: Extended;
begin
  Result := 0.0;
  if FData.Count > 0 then
    for i := 0 to FData.Count - 1 do
    begin
      aData := TCorrTestData(FData[i]);
      Sanc := (aData.AncestorRateRank - FAvgAncestorRank);
      Sanc := Sanc*Sanc;
      Result := Result + Sanc;
    end;
end;

function TCorrTestDataList.ComputeSxSy: Extended;
var
  Sanc, Sdesc: Extended;
begin
  Sanc := ComputeSanc;
  Sdesc := ComputeSdesc;
  Result := Sanc*Sdesc;
  if Result > 0 then
    Result := Sqrt(Result)
  else
    Result := 0;
end;

function TCorrTestDataList.ComputeSumDSquared: Extended;
var
  i: Integer;
  temp: Extended;
  aData: TCorrTestData;
begin
  Result := 0;
  if FData.Count > 0 then
    for i := 0 to FData.Count - 1 do
    begin
      aData := TCorrTestData(FData[i]);
      temp := (aData.AncestorRateRank - aData.DescendentRateRank);
      temp := temp*temp;
      Result := Result + temp;
    end;
end;

function TCorrTestDataList.ComputeSxy: Extended;
var
  i: Integer;
  aData: TCorrTestData;
begin
  Result := 0.0;
  if FData.Count > 0 then
    for i := 0 to FData.Count - 1 do
    begin
      aData := TCorrTestData(FData[i]);
      Result := Result + (aData.AncestorRateRank - FAvgAncestorRank)*(aData.DescendentRateRank - FAvgDescendentRank);
    end;
end;

function TCorrTestDataList.ComputeSdesc: Extended;
var
  i: Integer;
  aData: TCorrTestData;
  Sdesc: Extended;
begin
  Result := 0.0;
  if FData.Count > 0 then
    for i := 0 to FData.Count - 1 do
    begin
      aData := TCorrTestData(FData[i]);
      Sdesc := (aData.DescendentRateRank - FAvgDescendentRank);
      Sdesc := Sdesc*Sdesc;
      Result := Result + Sdesc;
    end;
end;

constructor TCorrTestDataList.Create;
begin
  FData := TList.Create;
end;

destructor TCorrTestDataList.Destroy;
begin
  Clear;
  if Assigned(FData) then
    FData.Free;
  inherited;
end;

procedure TCorrTestDataList.SetRanks;
var
  i, j: Integer;
  data1, data2: TCorrTestData;

  procedure AverageRanks(sIndex, eIndex: Integer);
  var
    k: Integer;
    s: Extended;
  begin
    s := 0;
    for k := sIndex to eIndex do
      s := s + TCorrTestData(FData[k]).AncestorRateRank;
    s := s/(eIndex - sIndex + 1);
    for k := sIndex to eIndex do
      TCorrTestData(FData[k]).AncestorRateRank := s;
  end;

begin
  if FData.Count = 0 then
    Exit;
  FData.Sort(@CompareAncestorsAsc);
  for i := 0 to FData.Count - 1 do
    TCorrTestData(FData[i]).AncestorRateRank := i + 1;

  i := 0;
  j := 1;
  if FData.Count > 1 then
    while i < FData.Count - 1 do
    begin
      if j > (FData.Count - 1) then
      begin
        if (j - i) > 1 then
          AverageRanks(i, j - 1);
        break;
      end;
      data1 := TCorrTestData(FData[i]);
      data2 := TCorrTestData(FData[j]);
      if CompareValue(data1.AncestorRate, data2.AncestorRate, 0.0000001) = 0 then
      begin
        inc(j)
      end
      else
      begin
        if (j - i) > 1 then
        begin
          AverageRanks(i, j - 1);
        end;
        inc(i);
        j := i + 1;
      end;
    end;

  FData.Sort(@CompareDescendentsAsc);
  for i := 0 to FData.Count - 1 do
    TCorrTestData(FData[i]).DescendentRateRank := i + 1;

  i := 0;
  j := 1;
  if FData.Count > 1 then
    while i < FData.Count - 1 do
    begin
      if j > (FData.Count - 1) then
      begin
        if (j - i) > 1 then
          AverageRanks(i, j - 1);
        break;
      end;
      data1 := TCorrTestData(FData[i]);
      data2 := TCorrTestData(FData[j]);
      if CompareValue(data1.DescendentRate, data2.DescendentRate, 0.0000001) = 0 then
      begin
        inc(j)
      end
      else
      begin
        if (j - i) > 1 then
        begin
          AverageRanks(i, j - 1);
        end;
        inc(i);
        j := i + 1;
      end;
    end;
end;

procedure TCorrTestDataList.HandleTiedRanks;
begin

end;

end.
