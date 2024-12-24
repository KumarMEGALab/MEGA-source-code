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

unit ml_tstv_caption;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MAnalysisInfo, MLTree;

type

  { TMLTsTvCaption }

  TMLTsTvCaption = class(TObject)
    private
      FAnalysisInfo: TAnalysisInfo;
      FModelInfo: TModelInfo;
    public
      constructor Create(aMAI: TAnalysisInfo; aModelInfo: TModelInfo);

      function GenerateCaption: String;
  end;

  function GenerateMLTsTvCaption(aMAI: TAnalysisInfo; aModelInfo: TModelInfo): String;

implementation

uses
  MegaConsts, MLTreeAnalyzer, mstringbuilder, mdistpack, mtreepack,
  math;

function GenerateMLTsTvCaption(aMAI: TAnalysisInfo; aModelInfo: TModelInfo): String;
var
  c: TMLTsTvCaption = nil;
begin
  Result := EmptyStr;
  try
    c := TMLTsTvCaption.Create(aMAI, aModelInfo);
    Result := c.GenerateCaption;
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

{ TMLTsTvCaption }

constructor TMLTsTvCaption.Create(aMAI: TAnalysisInfo; aModelInfo: TModelInfo);
begin
  FAnalysisInfo := aMAI;
  FModelInfo := aModelInfo;
end;

function TMLTsTvCaption.GenerateCaption: String;
var
  b: TMegaStringBuilder = nil;
begin
  Result := EmptyStr;

  try
    b := TMegaStringBuilder.Create;
    if CompareValue(FModelInfo.SVR, 0.5, FP_CUTOFF) = 0 then
      b.Add(Format('The assumed Transition/Transversion bias (<i>R</i>) is %.2f.', [FModelInfo.SVR]))
    else
      b.Add(Format('The estimated Transition/Transversion bias (<i>R</i>) is %.2f.', [FModelInfo.SVR]));

    if FAnalysisInfo.MyDistPack.DoesContain(gdGamma) then
      b.Add(Format(' The estimated value for the Gamma Distribution shape parameter is %.2f.', [FModelInfo.Gamma]));
    if FAnalysisInfo.MyDistPack.DoesContain(gdInvar) then
      b.Add(Format(' The proportion of sites estimated to be invariant is %.2f%%.', [FModelInfo.Invar*100]));
    b.Add(Format(' Substitution rates were estimated under the %s [{%s:%s}].', [FModelInfo.FullBaseModelName, POST_PROCESS_CITATION, FAnalysisInfo.MyDistPack.MethodCitation]));

    if FAnalysisInfo.MyDistPack.DoesContain(gdGamma) then
    begin
      b.Add(Format(' A discrete Gamma distribution was used to model evolutionary rate differences among sites (%d categories, [+<i>G</i>]) ', [FModelInfo.NoOfRates]));
      b.Add(FModelInfo.CatRatesCaption);
    end;
    b.Add(FModelInfo.FrequenciesCaption);
    if FAnalysisInfo.MyMLAnalysisPack.IsUserSuppliedTree then
      b.Add(' For estimating ML values, a user-specified topology was used.')
    else
      b.Add(Format(' For estimating ML values, a tree topology was automatically computed [{%s:Mega5Citation}].', [POST_PROCESS_CITATION]));
      if not (FAnalysisInfo.MyUsrOperation = dtdoLbsTsTvBias) then
        b.Add(Format(' The maximum Log likelihood for this computation was %.3n.', [FModelInfo.LogL]));

    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

end.

