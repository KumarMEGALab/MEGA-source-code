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

unit mreltime_caption;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MAnalysisInfo,{$IFDEF VISUAL_BUILD} MTreeBox,{$ENDIF} MLTreeAnalyzer;

type

  { TReltimeCaption }

  TReltimeCaption = class(TObject)
    private
      FMLAnalyzer: TMLTreeAnalyzer;
      FAnalysisInfo: TAnalysisInfo;
      {$IFDEF VISUAL_BUILD}
      FTree: TTreeBox;
      {$ENDIF}
      function AdaptiveParamsString: String;
      function DisplayDescription: String;
      function AncestralStates: String;
    public
      constructor Create(aInfo: TAnalysisInfo; {$IFDEF VISUAL_BUILD} aTree: TTreeBox;  {$ENDIF}aAnalyzer: TMLTreeAnalyzer);
      function GenerateCaption: String;
  end;

  function GenerateReltimeCaption(aInfo: TAnalysisInfo;{$IFDEF VISUAL_BUILD}  aTree: TTreeBox;{$ENDIF} aAnalyzer:  TMLTreeAnalyzer): String;

implementation

uses
  MegaConsts, mdistpack, mstringbuilder, ml_rates_and_patterns_caption, MegaAnalysisPrefStrings,
  mdata_subset_caption, MegaVerConsts, mega_citation, mtreepack;

function GenerateReltimeCaption(aInfo: TAnalysisInfo; {$IFDEF VISUAL_BUILD} aTree: TTreeBox;  {$ENDIF}aAnalyzer: TMLTreeAnalyzer): String;
var
  c: TReltimeCaption = nil;
begin
  Result := EmptyStr;

  try
    c := TReltimeCaption.Create(aInfo,{$IFDEF VISUAL_BUILD}  aTree,{$ENDIF} aAnalyzer);
    Result := c.GenerateCaption;
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

{ TReltimeCaption }

function TReltimeCaption.AdaptiveParamsString: String;
var
  b: TMegaStringBuilder = nil;
  numAdaptiveParams: Integer = 0;
  elements: TArrayOfString = nil;
  index: Integer = 0;
begin
  Result := EmptyStr;

  try
    b := TMegaStringBuilder.Create;
    if FAnalysisInfo.UseAdaptivePercentageOfSites then
      inc(numAdaptiveParams);
    if FAnalysisInfo.UseAdaptiveNumSamples then
      inc(numAdaptiveParams);
    if FAnalysisInfo.UseAdaptiveRepsPerSample then
      inc(numAdaptiveParams);

    if numAdaptiveParams > 0 then
    begin
      SetLength(elements, numAdaptiveParams);
      if FAnalysisInfo.UseAdaptivePercentageOfSites then
      begin
        elements[index] := Format(' distinct site configurations in each subsample (%.0n)', [FAnalysisInfo.NumSitesPerSubSample*1.0]);
        inc(index);
      end;

      if FAnalysisInfo.UseAdaptiveNumSamples then
      begin
        elements[index] := Format(' count of subsamples (%d)', [FAnalysisInfo.NumSubSamples]);
        inc(index);
      end;

      if FAnalysisInfo.UseAdaptiveRepsPerSample then
      begin
        elements[index] := Format(' number of replicates per subsample (%d)', [FAnalysisInfo.NumRepsPerSubSample]);
        inc(index);
      end;

      b.Add('The number of');
      for index := Low(elements) to High(elements) do
      begin
        if index = High(elements) then
          b.Add(' and ')
        else if index <> Low(elements) then
          b.Add(', ');
        b.Add(elements[index]);
      end;

      b.Add(Format(' were determined adaptively [{%s:LittleBootstrapsCitation}][{%s:MegaCitation}].', [POST_PROCESS_CITATION, POST_PROCESS_CITATION]));
      Result := b.GenerateString;
    end
    else
      Result := EmptyStr;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

function TReltimeCaption.DisplayDescription: String;
var
  b: TMegaStringBuilder = nil;
begin
  Result := EmptyStr;
  try
    b := TMegaStringBuilder.Create;

    if {$IFDEF VISUAL_BUILD}FTree.ShowHeightErrBar and FTree.ShowDivergenceTimes{$ELSE} True{$ENDIF} then
    begin
      if Assigned(FAnalysisInfo.CalibrationTimes) and (FAnalysisInfo.CalibrationTimes.NumSelectedCalibrations > 0) then
        b.Add(' Divergence time estimates are')
      else
        b.Add(' Relative divergence time estimates are');
      b.Add(' shown next to nodes in the tree and bars around each node represent 95% confidence intervals');
      if Assigned(FAnalysisInfo.CalibrationTimes) and FAnalysisInfo.CalibrationTimes.GetUsesCalibrationDensities then
        b.Add(Format(' [{%s:DensitiesCitation}]', [POST_PROCESS_CITATION]));
      if FAnalysisInfo.MyUsrOperation = dtdoRelTimeBLens then
        b.Add(', which considered only the rate variation among lineages because a phylogeny with only branch lengths was provided');
      b.Add('.');
    end
    else if {$IFDEF VISUAL_BUILD}FTree.ShowHeightErrBar{$ELSE}True{$ENDIF} then
    begin
      b.Add(' Bars around each node represent 95% confidence intervals');
      if Assigned(FAnalysisInfo.CalibrationTimes) and FAnalysisInfo.CalibrationTimes.GetUsesCalibrationDensities then
        b.Add(Format(' [{%s:DensitiesCitation}]', [POST_PROCESS_CITATION]));
      if FAnalysisInfo.MyUsrOperation = dtdoRelTimeBLens then
        b.Add(', which considered only the rate variation among lineages because a phylogeny with only branch lengths was provided');
      b.Add('.');
    end
    else if {$IFDEF VISUAL_BUILD}FTree.ShowDivergenceTimes{$ELSE}True{$ENDIF} then
    begin
      if Assigned(FAnalysisInfo.CalibrationTimes) and (FAnalysisInfo.CalibrationTimes.NumSelectedCalibrations > 0) then
        b.Add(' Divergence time estimates are shown next to nodes in the tree.')
      else
        b.Add(' Relative divergence time estimates are shown next to nodes in the tree.');
    end;

    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

function TReltimeCaption.AncestralStates: String;
var
  b: TMegaStringBuilder = nil;
begin
  Result := EmptyStr;
  try
    b := TMegaStringBuilder.Create;
    b.Add(' The tree shows a set of');
    if FAnalysisInfo.isAminoAcid then
      b.Add(' amino acid')
    else
      b.Add(' nucleotides');
    b.Add(' states at each ancestral node');
    if FAnalysisInfo.MyTreePack.NameAcronym = 'ML' then
      b.Add(' based on their inferred likelihood');

    {$IFDEF VISUAL_BUILD}
    b.Add(Format(' at site %s', [FTree.SiteIndexForCaption]));
    {$ELSE}
    b.Add(' at site 1');
    {$ENDIF}

    b.Add('.');
    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

constructor TReltimeCaption.Create(aInfo: TAnalysisInfo; {$IFDEF VISUAL_BUILD} aTree: TTreeBox; {$ENDIF} aAnalyzer: TMLTreeAnalyzer);
begin
  FAnalysisInfo := aInfo;
  FMLAnalyzer := aAnalyzer;

  {$IFDEF VISUAL_BUILD}
  FTree := aTree;
  {$ENDIF}
end;

function TReltimeCaption.GenerateCaption: String;
var
  b: TMegaStringBuilder = nil;
  tempStr: String = '';
  majVersion: Integer = -1;
  citation: String = '';
begin
  Result := EmptyStr;
  try
    b := TMegaStringBuilder.Create;
    b.Add('The timetree was inferred by the RelTime ');
    if FAnalysisInfo.IsRtdt then
      b.Add('with Dated Tips (RTDT) ');
    b.Add(Format('method [{%s:ReltimeCitation}]', [POST_PROCESS_CITATION]));
    b.Add(Format('[{%s:ReltimeCitation2}]', [POST_PROCESS_CITATION]));
    if FAnalysisInfo.IsRtdt then
      b.Add(Format('[{%s:RtdtCitation}]', [POST_PROCESS_CITATION]));
    b.Add('.');

    if not (FAnalysisInfo.MyUsrOperation in [dtdoRelTimeBLens, dtdoRtdtBlens]) then
    begin
      if FAnalysisInfo.isAminoAcid then
        tempStr := 'amino acid'
      else
        tempStr := 'nucleotide';
      if FAnalysisInfo.MyDistPack.DistModel = gdNoOfDiff then
        b.Add(Format(' For this analysis, a user-provided phylogeny and the number of %s differences were used [{%s:%s}].', [tempStr, POST_PROCESS_CITATION, FAnalysisInfo.MyDistPack.MethodCitation]))
      else if FAnalysisInfo.MyDistPack.DistModel = gdPropDist then
        b.Add(Format(' For this analysis, a user-provided phylogeny and the p-distance method were used [{%s:%s}].', [POST_PROCESS_CITATION, FAnalysisInfo.MyDistPack.MethodCitation]))
      else if FAnalysisInfo.MyTreePack.DoesContain(ttUserTree) then
        b.Add(Format(' For this analysis, a user-provided phylogeny and %s model [{%s:%s}] of %s substitutions were used.', [FAnalysisInfo.DistModelName, POST_PROCESS_CITATION, FAnalysisInfo.MyDistPack.MethodCitation, tempStr]))
      else
        b.Add(Format(' For this analysis, the %s model [{%s:%s}] of %s substitutions was used.', [FAnalysisInfo.DistModelName, POST_PROCESS_CITATION, FAnalysisInfo.MyDistPack.MethodCitation, tempStr]));
    end;

    if FAnalysisInfo.MyUsrOperation in [dtdoRelTimeML, dtdoRtdtML, dtdoLbsTiming, dtdoLbsInference] then
    begin
      b.Add(' Branch lengths');
      if FAnalysisInfo.IsSubsampling then
        b.Add(' and their variances');
      b.Add(Format(' were estimated using the Maximum Likelihood method [{%s:MLCitation}]', [POST_PROCESS_CITATION]));
        b.Add(Format('. The estimated log likelihood of the tree is %.2n.', [FMLAnalyzer.LogLikelihood]));
    end
    else if FAnalysisInfo.MyUsrOperation in [dtdoMLTree, dtdoLbsAnalyzeTree, dtdoMLComputeUserTreeBLens, dtdoMLInferAncSeq, dtdoMLInferAncSeqMyPeg] then
      b.Add(Format(' Branch lengths were computed using the Maximum Likelihood method [{%s:MLCitation}].', [POST_PROCESS_CITATION]))
    else if FAnalysisInfo.MyUsrOperation = dtdoNJTree then
      b.Add(Format(' Branch lengths were computed using the Neighbor-joining method [{%s:NJCitation}].', [POST_PROCESS_CITATION]))
    else if FAnalysisInfo.MyUsrOperation = dtdoMETree then
      b.Add(Format(' Branch lengths were computed using the Minimum Evolution method [{%s:RzhNeiCPTestCitation}].', [POST_PROCESS_CITATION]))
    else if FAnalysisInfo.MyUsrOperation = dtdoUPGMATree then
      b.Add(Format(' Branch lengths were computed using the UPGMA method [{%s:%s}].', [POST_PROCESS_CITATION, FAnalysisInfo.MyTreePack.TreeMethodCitation]))
    else if FAnalysisInfo.MyUsrOperation in [dtdoMPTree, dtdoMPComputeUserTreeBLens, dtdoMPInferAncSeq, dtdoMPInferAncSeqMyPeg] then
      b.Add(' Branch lengths were computed using the Maximum Parsimony method.')
    else if FAnalysisInfo.MyUsrOperation in [dtdoRelTimeLS, dtdoRtdtLS, dtdoOLSComputeUserTreeBLens] then
      b.Add(' Branch lengths were computed using the Ordinary Least Squares method.')
    else if (FAnalysisInfo.MyUsrOperation = dtdoRelTimeBLens) or (FAnalysisInfo.MyUsrOperation = dtdoRtdtBlens) then
      b.Add(' Branch lengths were obtained from the user-provided tree.')
    else
      raise Exception.Create('Application error, missing handler for Reltime caption');

    if Assigned(FAnalysisInfo.MyMLAnalysisPack) then
    begin
      tempStr := GenerateMLRatesAndPatternsCaption(FAnalysisInfo, FAnalysisInfo.MyMLAnalysisPack);
      if not SameText(tempStr, 'N/A') then
        b.Add(' ' + tempStr);
    end;

    if Assigned(FAnalysisInfo.CalibrationTimes) and (FAnalysisInfo.CalibrationTimes.NumSelectedCalibrations > 0) then
    begin
      if FAnalysisInfo.CalibrationTimes.NumSelectedCalibrations = 1 then
      begin
        if FAnalysisInfo.CalibrationTimes.GetUsesCalibrationDensities then
          b.Add(Format(' The RelTime analysis incorporated a single calibration that was used to derive minimum and maximum bounds for the calibrated node [{%s:DensitiesCitation}].', [POST_PROCESS_CITATION]))
        else
          b.Add(' The RelTime analysis incorporated a single calibration that defined minimum and maximium bounds for the calibrated node.');
      end
      else
      begin
        b.Add(Format(' The RelTime analysis incorporated %d calibration constraints that were used to derive minimum and/or maximum bounds', [FAnalysisInfo.CalibrationTimes.NumSelectedCalibrations]));
        if FAnalysisInfo.CalibrationTimes.GetUsesCalibrationDensities then
          b.Add(Format(' at nodes with calibration densities [{%s:DensitiesCitation}].', [POST_PROCESS_CITATION]))
        else
          b.Add(' at nodes with constraints.');
      end;
    end;

    {$IFDEF VISUAL_BUILD}
    if FTree.ShowHeightErrBar or FTree.ShowDivergenceTimes then
      b.Add(DisplayDescription);
    {$ELSE}
    b.Add(DisplayDescription);
    {$ENDIF}

    b.Add(Format(' Notably, times are not estimated for nodes in the outgroup because the RelTime method does not assume that evolutionary rates are not the same for ingroup and outgroup lineages [{%s:ReltimeCitation2}].', [POST_PROCESS_CITATION]));

    if {$IFDEF VISUAL_BUILD}FTree.ShowCharState{$ELSE}FAnalysisInfo.MyUsrOperation in [dtdoRelTimeML, dtdoRtdtML]{$ENDIF} then
      b.Add(AncestralStates);
    if {$IFDEF VISUAL_BUILD}FTree.ShowDataCoverage{$ELSE}not (FAnalysisInfo.MyUsrOperation in [dtdoRelTimeBLens, dtdoRtdtBlens]){$ENDIF} then
      b.Add(' The percentage of sites where at least 1 unambiguous base is present in at least 1 sequence for each descendant clade is shown next to each internal node in the tree.');

    if FAnalysisInfo.MyNoOfSeqs > 0 then
    begin
      tempStr := GenerateDataSubsetCaption(FAnalysisInfo);
      if tempStr <> EmptyStr then
        b.Add(' ' + tempStr);
    end
    else
    begin
      if FAnalysisInfo.TreeSessionVersion > 0 then
      begin
        majVersion := MapSessionVersionToMegaVersion(sftMts, FAnalysisInfo.TreeSessionVersion);
        citation := MapMajorVersionToCitation(majVersion);
        if majVersion = 10 then
          b.Add(Format(' Evolutionary analyses were conducted in MEGA X [{%s:%s}]', [POST_PROCESS_CITATION, citation]))
        else
          b.Add(Format(' Evolutionary analyses were conducted in MEGA%d [{%s:%s}]', [majVersion, POST_PROCESS_CITATION, citation]));
      end;
    end;

    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

end.

