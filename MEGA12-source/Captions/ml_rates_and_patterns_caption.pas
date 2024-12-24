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

unit ml_rates_and_patterns_caption;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MAnalysisInfo, MLTreeAnalyzer;

type

  { TMLRatesAndPatternsCaption }

  TMLRatesAndPatternsCaption = class(TObject)
      FAnalysisInfo: TAnalysisInfo;
      FMLTreeAnalyzer: TMLTreeAnalyzer;
    public
      constructor Create(aInfo: TAnalysisInfo; aAnalyzer: TMLTreeAnalyzer);
      function GenerateCaption: String;
  end;

  function GenerateMLRatesAndPatternsCaption(aInfo: TAnalysisInfo; aAnalyzer: TMLTreeAnalyzer): String;

implementation

uses
  mstringbuilder, mdistpack, MegaConsts, MLTree;

function GenerateMLRatesAndPatternsCaption(aInfo: TAnalysisInfo; aAnalyzer: TMLTreeAnalyzer): String;
var
  c: TMLRatesAndPatternsCaption = nil;
begin
  Result := EmptyStr;

  try
    c := TMLRatesAndPatternsCaption.Create(aInfo, aAnalyzer);
    Result := c.GenerateCaption;
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

{ TMLRatesAndPatternsCaption }

constructor TMLRatesAndPatternsCaption.Create(aInfo: TAnalysisInfo; aAnalyzer: TMLTreeAnalyzer);
begin
  FAnalysisInfo := aInfo;
  FMLTreeAnalyzer := aAnalyzer;
end;

function TMLRatesAndPatternsCaption.GenerateCaption: String;
var
  b: TMegaStringBuilder = nil;
  aModelInfo: TModelInfo = nil;
begin
  Result := 'N/A';
  if not Assigned(FMLTreeAnalyzer) then
    Exit;

  try
    b := TMegaStringBuilder.Create;
    aModelInfo := TModelInfo.Create;
    if Assigned(FMLTreeAnalyzer) then
      FMLTreeAnalyzer.GetModelInfo(aModelInfo)
    else if Assigned(FAnalysisInfo.LbsModelInfo) then
      aModelInfo.Assign(FAnalysisInfo.LbsModelInfo)
    else
      raise Exception.Create('cannot generate caption as model info is missing');
    if FAnalysisInfo.MyDistPack.ComputeGammaAndInvarParameters then
    begin
     b.add(Format('The evolutionary rate differences among sites were modeled using a discrete Gamma distribution across %d categories (<i>+G</i>, parameter = %.4f),', [aModelInfo.NoOfRates, aModelInfo.Gamma]));
     b.add(Format(' with %.2f%% of sites deemed evolutionarily invariant (<i>+I</i>).', [aModelInfo.Invar*100]));
    end
    else if FAnalysisInfo.MyDistPack.ComputeGammaParameter then
    begin
     b.add(Format('The evolutionary rate differences among sites were modeled using a discrete Gamma distribution across %d categories (<i>+G</i>, parameter = %.4f).', [aModelInfo.NoOfRates, aModelInfo.Gamma]));
    end
    else if FAnalysisInfo.MyDistPack.ComputeInvarParameter then
    begin
      b.add(Format('The rate model allowed for %.2f%% of sites to be evolutionarily invariable (<i>I</i>).', [aModelInfo.Invar*100]));
    end
    else
      b.Add('N/A');
    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
    if Assigned(aModelInfo) then
      aModelInfo.Free;
  end;
end;

end.

