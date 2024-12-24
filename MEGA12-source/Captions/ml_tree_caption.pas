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

unit ml_tree_caption;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MAnalysisInfo, MLTreeAnalyzer, MTreeBox, MegaConsts;

type

  { TMLTreeCaption }

  TMLTreeCaption = class(TObject)
    private
      FAnalysisInfo: TAnalysisInfo;
      {$IFDEF VISUAL_BUILD}
      FTree: TTreeBox;
      {$ENDIF}

      FAnalyzer: TMLTreeAnalyzer;
      function StatsPosCaption: String;
      function BranchLengthsCaption: String;
      function StartingTreeCaption: String;
      function HasBootstrapSupport: Boolean;
      function IsStats: Boolean;
      function ShowStats: Boolean;
      function ShowStatsRange: Boolean;
      function IsConsensus: Boolean;
      function ShowBlens: Boolean;
      function ShowTopologyOnly: Boolean;
      function IsLinearized: Boolean;
      function IsCondensed: Boolean;
      function IsTimeScale: Boolean;
      function ShowDataCoverage: Boolean;
      function CondenseValue: Integer;
      function ConsensusValue: Integer;
      function IsBranchLength: Boolean;
      function TreeStyle: TTreeStyle;
      function TimeFactor: Double;
      function IsGeneDups: Boolean;

    public
      constructor Create(aInfo: TAnalysisInfo; aAnalyzer: TMLTreeAnalyzer{$IFDEF VISUAL_BUILD}; aTree: TTreeBox {$ENDIF});
      function GenerateCaption: String;
  end;

  function GenerateMLTreeCaption(aInfo: TAnalysisInfo; aAnalyzer: TMLTreeAnalyzer{$IFDEF VISUAL_BUILD}; aTree: TTreeBox {$ENDIF}): String;
  function MLStartingTreeCaption(aInfo: TAnalysisInfo): String;

implementation

uses
  mstringbuilder, ml_rates_and_patterns_caption, mdistpack, mtreepack,
  mdata_subset_caption, mltree;

function GenerateMLTreeCaption(aInfo: TAnalysisInfo; aAnalyzer: TMLTreeAnalyzer{$IFDEF VISUAL_BUILD}; aTree: TTreeBox {$ENDIF}): String;
var
  c: TMLTreeCaption = nil;
begin
  try
    c := TMLTreeCaption.Create(aInfo, aAnalyzer{$IFDEF VISUAL_BUILD}, aTree{$ENDIF});
    Result := c.GenerateCaption;
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

function MLStartingTreeCaption(aInfo: TAnalysisInfo): String;
var
  b: TMegaStringBuilder = nil;
  initModel: String = '';
  initModelCitation: String = '';
  a: TMLTreeAnalyzer = nil;
  m: TModelInfo =  nil;
begin
  try
    a := aInfo.MyMLAnalysisPack;
    if a.InitialNJTreeModelUsed = gdNone then
    begin
      m := TModelInfo.Create;
      if Assigned(aInfo.MyMLAnalysisPack) then
        aInfo.MyMLAnalysisPack.GetModelInfo(m)
      else
        m.Assign(aInfo.LbsModelInfo);
      initModel := m.FullBaseModelName;
      initModelCitation := aInfo.MyDistPack.MethodCitation
    end
    else
    begin
      initModel := DistCorrectionName(a.InitialNJTreeModelUsed);
      initModelCitation := DistModelCitation(a.InitialNJTreeModelUsed);
    end;

    b := TMegaStringBuilder.Create;

    if aInfo.MyTreePack.MLInitialTreeMethodStr = 'default' then
    begin
        b.add(Format('Initial tree(s) for the heuristic search were obtained automatically by applying Neighbor-Joining [{%s:NJCitation}] and BioNJ [{%s:BIONJCitation}] algorithms to a matrix of pairwise distances estimated using the Maximum Composite Likelihood (MCL) [{%s:MCLCitation}] approach, and then selecting the topology with superior log likelihood value.', [POST_PROCESS_CITATION, POST_PROCESS_CITATION, POST_PROCESS_CITATION]))
    end
    else if aInfo.MyTreePack.MLInitialTreeMethodStr = 'nj' then
      b.add(Format('Initial tree(s) for the heuristic search were obtained by applying the Neighbor-Joining [{%s:NJCitation}] method to a matrix of pairwise distances estimated using %s [{%s:%s}].', [POST_PROCESS_CITATION, InitModel, POST_PROCESS_CITATION, initModelCitation]))
    else if aInfo.MyTreePack.MLInitialTreeMethodStr = 'bionj' then
      b.Add(Format('Initial tree(s) for the heuristic search were obtained by applying the BioNJ [{%s:BIONJCitation}] method to a matrix of pairwise distances estimated using %s [{%s:%s}].', [POST_PROCESS_CITATION, initModel, POST_PROCESS_CITATION, initModelCitation]))
    else if aInfo.MyTreePack.MLInitialTreeMethodStr = 'mp' then
      b.Add('Initial tree(s) for the heuristic search were obtained automatically by applying the Maximum Parsimony method.')
    else if aInfo.MyTreePack.MLInitialTreeMethodStr = 'userTree' then
      b.Add('The initial tree for the heuristic search was provided by the user.');

    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
    if Assigned(m) then
      m.Free;
  end;
end;

{ TMLTreeCaption }

function TMLTreeCaption.StatsPosCaption: String;
begin
  Result := EmptyStr;

  {$IFDEF VISUAL_BUILD}
  if FTree.TreeStyle <> tsTraditional then
    Exit;

  if not (FTree.isStats and (FTree.ShowStats or FTree.ShowStatsRange)) then
    Exit;

  case FTree.StatsPosition of
    bipAboveBranch: Result := 'above the branches';
    bipBelowBranch: Result := 'below the branches';
  else
    Result := 'next to the branches'
  end;
  {$ELSE}
  Result := 'next to the branches';
  {$ENDIF}
end;

function TMLTreeCaption.BranchLengthsCaption: String;
begin
  Result := EmptyStr;
  {$IFDEF VISUAL_BUILD}
  if FTree.TreeStyle <> tsTraditional then
    Exit;

  if not (FTree.isBranchLength and FTree.ShowBLen) then
    Exit;

  case FTree.BLenPosition of
    bipAboveBranch: Result := '(shown above the branches)';
    bipBelowBranch: Result := '(shown below the branches)';
  else
    Result := '(shown next to the branches)';
  end;
  {$ELSE}
  Result := '(shown next to the branches)';
  {$ENDIF}
end;

function TMLTreeCaption.StartingTreeCaption: String;
begin
  Result := MLStartingTreeCaption(FAnalysisInfo);
end;

function TMLTreeCaption.HasBootstrapSupport: Boolean;
begin
  Result := True;
  if not FAnalysisInfo.IsSubsampling then
    Result := False;
  if FAnalysisInfo.MyUsrOperation = dtdoLbsAnalyzeTree then
    Result := False;
end;

function TMLTreeCaption.IsStats: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.isStats;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function TMLTreeCaption.ShowStats: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.ShowStats;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TMLTreeCaption.ShowStatsRange: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.ShowStatsRange;
  {$ELSE}
  Result := FAnalysisInfo.MyBootReps > 0;
  {$ENDIF}
end;

function TMLTreeCaption.IsConsensus: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.isConsensus;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TMLTreeCaption.ShowBlens: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.ShowBLen;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function TMLTreeCaption.ShowTopologyOnly: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.ShowTopologyOnly;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TMLTreeCaption.IsLinearized: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.IsLinearized;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TMLTreeCaption.IsCondensed: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.IsCondensed;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TMLTreeCaption.IsTimeScale: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.IsTimeScale;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TMLTreeCaption.ShowDataCoverage: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.ShowDataCoverage;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function TMLTreeCaption.CondenseValue: Integer;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.CondenseValue;
  {$ELSE}
  Result := 50;
  {$ENDIF}
end;

function TMLTreeCaption.ConsensusValue: Integer;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.ConsensusValue;
  {$ELSE}
  Result := 50;
  {$ENDIF}
end;

function TMLTreeCaption.IsBranchLength: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.isBranchLength;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function TMLTreeCaption.TreeStyle: TTreeStyle;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.TreeStyle;
  {$ELSE}
  Result := tsTraditional;
  {$ENDIF}
end;

function TMLTreeCaption.TimeFactor: Double;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.TimeFactor;
  {$ELSE}
  Result := 1.0;
  {$ENDIF}
end;

function TMLTreeCaption.IsGeneDups: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.IsGeneDups;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

constructor TMLTreeCaption.Create(aInfo: TAnalysisInfo; aAnalyzer: TMLTreeAnalyzer{$IFDEF VISUAL_BUILD} ; aTree: TTreeBox {$ENDIF});
begin
  FAnalysisInfo := aInfo;
  {$IFDEF VISUAL_BUILD}
  FTree := aTree;
  {$ENDIF}
  FAnalyzer := aAnalyzer;
end;

function TMLTreeCaption.GenerateCaption: String;
var
  b: TMegaStringBuilder = nil;
  tempStr: String = '';
  isUserTreeAnalysis: Boolean = False;
  m: TModelInfo = nil;
begin
  try
    b := TMegaStringBuilder.Create;
    isUserTreeAnalysis := (FAnalysisInfo.MyUsrOperation = dtdoMLComputeUserTreeBLens) or (FAnalysisInfo.MyUsrOperation = dtdoLbsAnalyzeTree);

    Assert(Assigned(FAnalysisInfo.MyMLAnalysisPack) or Assigned(FAnalysisInfo.LbsModelInfo));
    m := TModelInfo.Create;
    if Assigned(FAnalysisInfo.MyMLAnalysisPack) then
      FAnalysisInfo.MyMLAnalysisPack.GetModelInfo(m)
    else
      m.Assign(FAnalysisInfo.LbsModelInfo);

    if not IsConsensus then
    begin
      if isUserTreeAnalysis or (FAnalysisInfo.MyUsrOperation = dtdoLbsTiming) then
        b.Add('The user-specified tree topology was analyzed')
      else
        b.Add('The phylogeny was inferred');

      if FAnalysisInfo.isAminoAcid then
        tempStr := 'amino acid'
      else
        tempStr := 'nucleotide';
      b.Add(Format(' using the %s method and %s [{%s:%s}] of %s substitutions', [FAnalysisInfo.MyTreePack.TreeMethodName, m.FullBaseModelName, POST_PROCESS_CITATION, FAnalysisInfo.MyDistPack.MethodCitation, tempStr]));
      if FAnalysisInfo.MyUsrOperation = dtdoMLComputeUserTreeBLens then
        b.Add(Format(' and the log likelihood of the tree is %.2n.', [FAnalyzer.LogLikelihood]))
      else if not (FAnalysisInfo.IsSubsampling or isUserTreeAnalysis) then
        b.Add(Format(' and the tree with the highest log likelihood (%.2n) is shown.', [FAnalyzer.LogLikelihood]))
      else
        b.Add('.');

      if FAnalysisInfo.IsSubsampling then
      begin
        if FAnalysisInfo.MyUsrOperation = dtdoLbsInference then
        begin
          if ShowStats or ShowStatsRange then
            tempStr := Format(' (shown %s)', [StatsPosCaption])
          else
            tempStr := EmptyStr;
          b.Add(Format(' The Little Bootstraps approach [{%s:LittleBootstrapsCitation}] was used to generate the bootstrap confidence limits%s and consensus phylogeny.', [POST_PROCESS_CITATION, tempStr]));
        end;
      end;

      if ShowBlens then
      begin
        if not ShowTopologyOnly then
          tempStr := 'to scale '
        else
          tempStr := EmptyStr;
        if FAnalysisInfo.IsSubsampling then
        begin
          if FAnalysisInfo.MyUsrOperation in [dtdoLbsInference, dtdoLbsTiming] then
            b.Add(Format(' The tree is drawn %swith branch lengths %s derived from bootstrap replicate trees [{%s:MegaCitation}] and measured in the number of substitutions per site.', [tempStr, BranchLengthsCaption, POST_PROCESS_CITATION]))
          else
            b.Add(Format(' The tree is drawn %swith branch lengths %s derived from subsample data [{%s:MegaCitation}] and measured in the number of substitutions per site.', [tempStr, BranchLengthsCaption, POST_PROCESS_CITATION]));
        end
        else
          b.Add(Format(' The tree is drawn %swith branch lengths %s computed using the Maximum Likelihood method [{%s:MLCitation}] and measured in the number of substitutions per site.', [tempStr, BranchLengthsCaption, POST_PROCESS_CITATION]));
      end;
    end;

    if IsStats then
    begin
      if IsConsensus then
      begin
        b.Add(Format(' The bootstrap consensus tree inferred from %.0n replicates [{%s:BootstrapTestCitation}] is taken to represent the evolutionary history of the taxa analyzed', [FAnalysisInfo.MyBootReps*1.0, POST_PROCESS_CITATION]));
        b.Add(Format(' where branches corresponding to partitions reproduced in less than %d%% of replicate trees are collapsed%s.', [{$IFDEF VISUAL_BUILD}FTree.ConsensusValue{$ELSE}50{$ENDIF}, tempStr]))
      end
      else if IsCondensed then
      begin
        b.Add(Format(' Branches corresponding to partitions reproduced in less than %d%% of replicate trees are collapsed.', [{$IFDEF VISUAL_BUILD}FTree.CondenseValue{$ELSE}50{$ENDIF}]));
      end;

      if ShowStats or ShowStatsRange then
      begin
        if not FAnalysisInfo.IsSubsampling then
        begin
          b.Add(' The percentage of replicate trees in which the associated taxa clustered together');
          if FAnalysisInfo.UseAdaptiveRepsPerSample and (FAnalysisInfo.MyUsrOperation = dtdoMLTree) then
            b.Add(Format(', where the number of replicates (%.0n) was determined adaptively [{%s:Mega12Citation}], is shown %s.', [FAnalysisInfo.MyBootReps*1.0, POST_PROCESS_CITATION, StatsPosCaption]))
          else
            b.Add(Format(' (%.0n replicates) is shown %s [{%s:BootstrapTestCitation}].', [FAnalysisInfo.MyBootReps*1.0, StatsPosCaption, POST_PROCESS_CITATION]));
        end;
      end;
    end;


    if not (isUserTreeAnalysis or (FAnalysisInfo.MyUsrOperation = dtdoLbsTiming)) then
      b.Add(Format(' %s', [StartingTreeCaption]));

    if not (FAnalysisInfo.MyUsrOperation in [dtdoLbsAnalyzeTree, dtdoLbsTiming]) then
    begin
      tempStr := GenerateMLRatesAndPatternsCaption(FAnalysisInfo, FAnalyzer);
      if (tempStr <> EmptyStr) and (not SameText(tempStr, 'n/a')) then
        b.Add(' ' + tempStr);
    end;

    if not ShowTopologyOnly then
    begin
      if IsLinearized then
      begin
        b.Add(' The phylogenetic tree was linearized by conducting ML analysis under the assumption of equal evolutionary rates in all lineages (molecular clock).');
        if IsTimeScale then
          b.Add(Format(' The clock calibration used to convert node heights to time was %.4f (time/node height).', [TimeFactor]));
      end;
    end;

    if ShowDataCoverage then
      b.Add(' The proportion of sites where at least 1 unambiguous base is present in at least 1 sequence for each descendent clade is shown next to each internal node in the tree.');

    if IsGeneDups then
    begin
      if FAnalysisInfo.InitialUsrOperation = dtdoGeneDupInference then
        b.Add(' The evolutionary history shown is from a user-provided tree.');
      if FAnalysisInfo.UsedSpeciesTree = True then
    	 b.Add(Format(' Gene duplications were inferred using the method described in [{%s:GeneDupsCitation}].', [POST_PROCESS_CITATION]))
      else
        b.Add(' Gene duplications were identified by searching for all branching points in the topology with at least one species that is present in both subtrees of the branching point.');

      if not FAnalysisInfo.GeneTreeRooted then
        b.Add(' An unrooted gene tree was used for the analysis such that the search for duplication events was performed by finding the placement of the root on a branch or branches that produced the minimum number of duplication events.');
      {$IFDEF VISUAL_BUILD}
      b.Add(' ' + FTree.NumDupsBreakdown);
      {$ENDIF}
    end;

    tempStr := GenerateDataSubsetCaption(FAnalysisInfo);
    if tempStr <> EmptyStr then
    begin
      b.Add(' ');
      b.Add(tempStr);
    end;

    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

end.

