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

unit ml_gamma_param_caption;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MLTree, MAnalysisInfo;

type

  { TGammaParameterCaption }

  TGammaParameterCaption = class(TObject)
    protected
      FAnalysisInfo: TAnalysisInfo;
      FModelInfo: TModelInfo;
      function GetRatesAndPatternsCaption: String;
      function CatRates: String;
      function Frequencies: String;
    public
      constructor Create(aMai: TAnalysisInfo; aModelInfo: TModelInfo);
      function GenerateCaption: String;
  end;

  function GenerateGammaParameterCaption(aMai: TAnalysisInfo; aModelInfo: TModelInfo): String;
  function FrequenciesCaption(isAminoAcid: Boolean; aModelInfo: TModelInfo): String;

implementation

uses
  mstringbuilder, MegaConsts, mdistpack, ml_rates_and_patterns_caption, MLTreeAnalyzer;

function GenerateGammaParameterCaption(aMai: TAnalysisInfo; aModelInfo: TModelInfo): String;
var
  c: TGammaParameterCaption = nil;
begin
  Result := EmptyStr;

  try
    c := TGammaParameterCaption.Create(aMai, aModelInfo);
    Result := c.GenerateCaption;
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

function FrequenciesCaption(isAminoAcid: Boolean; aModelInfo: TModelInfo): String;
var
  b: TMegaStringBuilder = nil;
begin
  Result := EmptyStr;
  try
     b := TMegaStringBuilder.Create;
    if isAminoAcid then
    begin
      b.Add(' The amino acid frequencies are');
      b.Add(Format(' %s%% (A),', [FloatToStrF(aModelInfo.Freq[0]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (R),', [FloatToStrF(aModelInfo.Freq[1]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (N),', [FloatToStrF(aModelInfo.Freq[2]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (D),', [FloatToStrF(aModelInfo.Freq[3]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (C),', [FloatToStrF(aModelInfo.Freq[4]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (Q),', [FloatToStrF(aModelInfo.Freq[5]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (E),', [FloatToStrF(aModelInfo.Freq[6]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (G),', [FloatToStrF(aModelInfo.Freq[7]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (H),', [FloatToStrF(aModelInfo.Freq[8]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (I),', [FloatToStrF(aModelInfo.Freq[9]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (L),', [FloatToStrF(aModelInfo.Freq[10]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (K),', [FloatToStrF(aModelInfo.Freq[11]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (M),', [FloatToStrF(aModelInfo.Freq[12]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (F),', [FloatToStrF(aModelInfo.Freq[13]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (P),', [FloatToStrF(aModelInfo.Freq[14]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (S),', [FloatToStrF(aModelInfo.Freq[15]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (T),', [FloatToStrF(aModelInfo.Freq[16]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (W),', [FloatToStrF(aModelInfo.Freq[17]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (Y),', [FloatToStrF(aModelInfo.Freq[18]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' and %s (V).', [FloatToStrF(aModelInfo.Freq[19]*100, ffFixed, 12, paraDigits)]));
    end
    else
    begin
      b.Add(' The nucleotide frequencies are');
      b.Add(Format(' %s%% (A),', [FloatToStrF(aModelInfo.Freq[0]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (T/U),', [FloatToStrF(aModelInfo.Freq[1]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (C),', [FloatToStrF(aModelInfo.Freq[2]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' and %s%% (G).', [FloatToStrF(aModelInfo.Freq[3]*100, ffFixed, 12, paraDigits)]));
    end;

     Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

{ TGammaParameterCaption }

function TGammaParameterCaption.GetRatesAndPatternsCaption: String;
var
  c: TMLRatesAndPatternsCaption = nil;
begin
  try
    c := TMLRatesAndPatternsCaption.Create(FAnalysisInfo, FAnalysisInfo.MyMLAnalysisPack);
    Result := c.GenerateCaption;
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

function TGammaParameterCaption.CatRates: String;
begin
  Result := FModelInfo.CatRatesCaption;
end;

function TGammaParameterCaption.Frequencies: String;
begin
  Result := FrequenciesCaption(FAnalysisInfo.isAminoAcid, FModelInfo);
end;


constructor TGammaParameterCaption.Create(aMai: TAnalysisInfo; aModelInfo: TModelInfo);
begin
  FAnalysisInfo := aMai;
  FModelInfo := aModelInfo;
end;

function TGammaParameterCaption.GenerateCaption: String;
var
  b: TMegaStringBuilder = nil;
begin
  Result := EmptyStr;

  try
    b := TMegaStringBuilder.Create;
    b.Add(Format('The estimated value of the shape parameter for the Gamma Distribution is %.2f.', [FModelInfo.Gamma]));
    if FAnalysisInfo.MyDistPack.DoesContain(gdInvar) then
       b.Add(Format(' The proportion of sites estimated to be invariant is %.2f%%.', [FModelInfo.Invar*100]));
    b.Add(Format(' Substitution rates were estimated under the %s [{%s:%s}].', [FModelInfo.FullBaseModelName, POST_PROCESS_CITATION, FAnalysisInfo.MyDistPack.MethodCitation]));

    b.Add(Format(' A discrete Gamma distribution was used to model evolutionary rate differences among sites (%d categories, [+<i>G</i>]) ', [FModelInfo.NoOfRates]));
    b.Add(CatRates);
    b.Add(Frequencies);
    if FAnalysisInfo.MyMLAnalysisPack.IsUserSuppliedTree then
      b.Add(' For estimating ML values, a user-specified topology was used.')
    else
      b.Add(Format(' For estimating ML values, a tree topology was automatically computed [{%s:Mega5Citation}].', [POST_PROCESS_CITATION]));

    if not (FAnalysisInfo.MyUsrOperation in [dtdoLbsGamma, dtdoLbsTsTvBias]) then
      b.Add(Format(' The maximum Log likelihood for this computation was %.3n.', [FModelInfo.LogL]));

    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

end.

