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

unit ml_substitution_pattern_caption;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MegaConsts, MAnalysisInfo, MLTree;

type

  { TMLSubstitutionPatternCaption }

  TMLSubstitutionPatternCaption = class(TObject)
  protected
    FAnalysisInfo: TAnalysisInfo;
    FModelInfo: TModelInfo;
  public
    constructor Create(aMai: TAnalysisInfo; aModelInfo: TModelInfo);
    function GenerateCaption: String;
  end;

  function GenerateMLSubstitutionPatternCaption(aMai: TAnalysisInfo; aModelInfo: TModelInfo): String;

implementation

uses
  mstringbuilder, ml_gamma_param_caption;

function GenerateMLSubstitutionPatternCaption(aMai: TAnalysisInfo; aModelInfo: TModelInfo): String;
var
  c: TMLSubstitutionPatternCaption = nil;
begin
  try
    c := TMLSubstitutionPatternCaption.Create(aMai, aModelInfo);
    Result := c.GenerateCaption;
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

{ TMLSubstitutionPatternCaption }

constructor TMLSubstitutionPatternCaption.Create(aMai: TAnalysisInfo; aModelInfo: TModelInfo);
begin
  FAnalysisInfo := aMai;
  FModelInfo := aModelInfo;
end;

function TMLSubstitutionPatternCaption.GenerateCaption: String;
var
  b: TMegaStringBuilder = nil;
begin
  try
    b := TMegaStringBuilder.Create;
    b.Add(Format(' Substitution pattern and rates were estimated under the %s [{%s:%s}].', [FModelInfo.FullBaseModelName, POST_PROCESS_CITATION, FAnalysisInfo.MyDistPack.MethodCitation]));
    if FModelInfo.NoOfRates > 1 then
      b.Add(Format(' A discrete Gamma distribution was used to model evolutionary rate differences among sites (%d categories, [+G], parameter = %.4f).', [FModelInfo.NoOfRates, FModelInfo.Gamma]));
    if FModelInfo.UseInvar then
      b.Add(Format(' The rate variation model allowed for some sites to be evolutionarily invariable ([+I], %d%% sites).', [Round(FModelInfo.Invar*100)]));
    if not FAnalysisInfo.isAminoAcid then
      b.Add(' Rates of different transitional substitutions are shown in <b>bold</b> and those of transversionsal substitutions are shown in <i>italics</i>.');
    b.Add(' Relative values of instantaneous <i>r</i> should be considered when evaluating them.');
    b.Add(' For simplicity, sum of <i>r</i> values is made equal to 100.');
    b.Add(FrequenciesCaption(FAnalysisInfo.isAminoAcid, FModelInfo));
    b.Add(' For estimating ML values, a tree topology was automatically computed.');
    b.Add(Format(' The maximum Log likelihood for this computation was %.3f', [FModelInfo.LogL]));
    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

end.

