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

unit mdefault_tree_caption;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF VISUAL_BUILD}
  MTreeViewForm, MTreeBox,
  {$ENDIF}
  Classes, SysUtils, MAnalysisInfo,  mltree;

type

  { TDefaultTreeCaption }

  TDefaultTreeCaption = class(TObject)
    protected
      {$IFDEF VISUAL_BUILD}
       FTreeViewForm: TTreeViewForm;
       FTreeBox: TTreeBox;
      {$ENDIF}
      FAnalysisInfo: TAnalysisInfo;
      function ModelName: String;
      function BLenCaption: String;
      function StatPosCaption: String;
      function ActiveTabName: String;
      function StrictClockTreeSnippet: String;
      function TimetreeTypeStr: String;
      function NoOfTrees: Integer;
      function TreeIndex: Integer;
      function SBL(index: Integer): Double;
      function ConsensusValue: Integer;
      function IsGeneDups: Boolean;
      function ShowTopologyOnly: Boolean;
      function IsTimescale: Boolean;
      function IsLinearized: Boolean;
      function TimeFactor: Double;
    public
      constructor Create(aInfo: TAnalysisInfo{$IFDEF VISUAL_BUILD}; treeViewer: TTreeViewForm; treeBox: TTreeBox{$ENDIF});
      function GenerateCaption: String;
  end;

  function GenerateDefaultTreeCaption(aInfo: TAnalysisInfo{$IFDEF VISUAL_BUILD}; treeViewer: TTreeViewForm; treeBox: TTreeBox{$ENDIF}): String;

implementation

uses
  mtreepack, mdistpack, mstringbuilder, MegaConsts, mdata_subset_caption;

function GenerateDefaultTreeCaption(aInfo: TAnalysisInfo{$IFDEF VISUAL_BUILD}; treeViewer: TTreeViewForm; treeBox: TTreeBox{$ENDIF}): String;
var
  c: TDefaultTreeCaption = nil;
begin
  try
    c := TDefaultTreeCaption.Create(aInfo{$IFDEF VISUAL_BUILD}, treeViewer, treeBox{$ENDIF});
    Result := c.GenerateCaption;
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

{ TDefaultTreeCaption }

function TDefaultTreeCaption.ModelName: String;
var
  aModelInfo: TModelInfo = nil;
begin
  Result := EmptyStr;

  try
     if Assigned(FAnalysisInfo.MyMLAnalysisPack) then
     begin
       aModelInfo := TModelInfo.Create;
       FAnalysisInfo.MyMLAnalysisPack.GetModelInfo(aModelInfo);
       Result := aModelInfo.ModelName;
     end
     else if Assigned(FAnalysisInfo.MyDistPack) then
     begin
       if FAnalysisInfo.MyDistPack.DistModel = gdNoOfDiff  then
         Result := 'number of differences'
       else if FAnalysisInfo.MyDistPack.DistModel = gdPropDist  then
         Result := 'p-distance';
     end;
  finally
    if Assigned(aModelInfo) then
      aModelInfo.Free
  end;
end;

function TDefaultTreeCaption.BLenCaption: String;
begin
  Result := EmptyStr;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeViewForm) then
    Result := FTreeViewForm.BLenCaption;
  {$ELSE}
  Result := 'next to the branches';
  {$ENDIF}
end;

function TDefaultTreeCaption.StatPosCaption: String;
begin
  Result := EmptyStr;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeViewForm) then
    Result := FTreeViewForm.StatPosCaption;
  {$ELSE}
  Result := 'next to the branches'
  {$ENDIF}
end;

function TDefaultTreeCaption.ActiveTabName: String;
begin
  Result := EmptyStr;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeViewForm) then
    Result := FTreeViewForm.ActiveTabName;
  {$ELSE}
  if Assigned(FAnalysisInfo) and (FAnalysisInfo.MyTreePack.BootReps > 0) then
    Result := 'Original Tree';
  {$ENDIF}
end;

function TDefaultTreeCaption.StrictClockTreeSnippet: String;
var
  b: TMegaStringBuilder = nil;
begin
  Result := EmptyStr;
  b := TMegaStringBuilder.Create;
  {$IFDEF VISUAL_BUILD}
  try
     b.Add('A timetree inferred using the strict-clock method');
     if FTreeViewForm.IsMLStr = 'true' then
       b.Add(Format(' and the %s model [{%s:%s}].', [FAnalysisInfo.DistModelName, POST_PROCESS_CITATION, FAnalysisInfo.MyDistPack.MethodCitation]))
     else
       b.Add(Format(' and estimates of branch lengths inferred using the %s method [{%s:%s}].', [FAnalysisInfo.MyTreePack.TreeMethodName, POST_PROCESS_CITATION, FAnalysisInfo.MyTreePack.TreeMethodCitation]));

     if FTreeViewForm.CalibrateClockMode = ccmEvoRate then
       b.Add(Format(' A user-provide global clock rate (%.2f) was used to compute divergence times throughout the tree.', [FTreeViewForm.MolClockClockRate]))
     else
       b.Add(Format(' A user-provided divergence time (%.2f for node %d) was used to derive a global clock rate for computing divergence times throughout the tree.', [FTreeViewForm.MolClockDivTime, FTreeViewForm.MolClockCalibratedNode]));
  finally
    if Assigned(b) then
      b.Free;
  end;
  {$ENDIF}
end;

function TDefaultTreeCaption.TimetreeTypeStr: String;
begin
  Result := EmptyStr;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeViewForm) then
    Result := FTreeViewForm.TimeTreeTypeStr;
  {$ENDIF}
end;

function TDefaultTreeCaption.NoOfTrees: Integer;
begin
  Result := 1;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeBox) then
    Result := FTreeBox.NoOfTrees;
  {$ENDIF}
end;

function TDefaultTreeCaption.TreeIndex: Integer;
begin
  Result := 1;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeBox) then
    Result := FTreeBox.TreeIndex;
  {$ENDIF}
end;

function TDefaultTreeCaption.SBL(index: Integer): Double;
begin
  Result := 0;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeBox) then
    Result := FTreeBox.SBL[index];
  {$ELSE}
  if Assigned(FAnalysisInfo) and Assigned(FAnalysisInfo.MyOriTreeList) then
    Result := FAnalysisInfo.MyOriTreeList[0].SBL;
  {$ENDIF}
end;

function TDefaultTreeCaption.ConsensusValue: Integer;
begin
  Result := 50;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeBox) then
    Result := FTreeBox.ConsensusValue;
  {$ENDIF}
end;

function TDefaultTreeCaption.IsGeneDups: Boolean;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeBox) then
    Result := FTreeBox.IsGeneDups;
  {$ENDIF}
end;

function TDefaultTreeCaption.ShowTopologyOnly: Boolean;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeBox) then
    Result := FTreeBox.ShowTopologyOnly;
  {$ENDIF}
end;

function TDefaultTreeCaption.IsTimescale: Boolean;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeBox) then
    Result := FTreeBox.IsTimescale;
  {$ENDIF}
end;

function TDefaultTreeCaption.IsLinearized: Boolean;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeBox) then
    Result := FTreeBox.IsLinearized;
  {$ENDIF}
end;

function TDefaultTreeCaption.TimeFactor: Double;
begin
  Result := 1;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeBox) then
    Result := FTreeBox.TimeFactor;
  {$ENDIF}
end;

constructor TDefaultTreeCaption.Create(aInfo: TAnalysisInfo{$IFDEF VISUAL_BUILD}; treeViewer: TTreeViewForm; treeBox: TTreeBox{$ENDIF});
begin
  FAnalysisInfo := aInfo;
  {$IFDEF VISUAL_BUILD}
  FTreeViewForm := treeViewer;
  FTreeBox := treeBox;
  {$ENDIF}
end;

function TDefaultTreeCaption.GenerateCaption: String;
var
  b: TMegaStringBuilder = nil;
  info: TAnalysisInfo = nil;
  tempStr: String = '';
begin
  try
    info := FAnalysisInfo;
    b := TMegaStringBuilder.Create;
    if ActiveTabName = 'Timetree' then
    begin
      if TimeTreeTypeStr = 'tttStrictClocksLocal' then
        b.Add(StrictClockTreeSnippet)
      else if TimeTreeTypeStr = 'tttStrictClocksMain' then
        b.Add(StrictClockTreeSnippet)
      else
        b.Add('IncludeExternalFile=Reltime_snippet.htm}');
    end
    else
    begin
      if FAnalysisInfo.MyTreePack.DoesContain(ttUserTree) and (not FAnalysisInfo.MyTreePack.DoesContain(ttInferTree)) then
        b.Add('The user-specified tree topology was analyzed')
      else
        b.Add('The evolutionary history was inferred');
      b.Add(Format(' using the %s method [{%s:%s}].', [FAnalysisInfo.MyTreePack.TreeMethodName, POST_PROCESS_CITATION, FAnalysisInfo.MyTreePack.TreeMethodCitation]));

      if ActiveTabName = 'Bootstrap Consensus Tree' then
      begin
        b.Add(Format(' The bootstrap consensus tree inferred from %.0n', [info.MyTreePack.BootReps*1.0]));
        b.Add(Format(' replicates [{%s:BootstrapTestCitation}] is', [POST_PROCESS_CITATION]));
        b.Add(' taken to represent the evolutionary history of the taxa');
        b.Add(Format(' analyzed [{%s:BootstrapTestCitation}].', [POST_PROCESS_CITATION]));
        b.Add(' Branches corresponding to partitions reproduced in less');
        b.Add(Format(' than %d%% bootstrap replicates are collapsed.', [ConsensusValue]));

        if StatPosCaption <> EmptyStr then
          b.Add(Format(' The percentage of replicate trees in which the associated taxa clustered together in the bootstrap test (%.0n replicates) are shown %s [{%s:BootstrapTestCitation}].', [info.MyTreePack.BootReps*1.0, StatPosCaption, POST_PROCESS_CITATION]));
       end
       else if ActiveTabName = 'Consensus tree' then
       begin
         b.Add(Format(' The consensus tree inferred from %d optimal trees', [NoOfTrees]));
         if SBL(TreeIndex) > 0 then
           b.Add(Format(' (sum of branch lengths = %.3f)', [SBL(TreeIndex)]));
         b.Add(' is shown. Branches corresponding to partitions reproduced in less');
         b.Add(Format(' than %d%% trees are collapsed.', [ConsensusValue]));
         if StatPosCaption <> EmptyStr then
           b.Add(Format(' The percentage of optimal trees in which the associated taxa clustered together are shown %s.', [StatPosCaption]));
       end
       else if TreeIndex = 0 then
       begin
         b.Add(Format(' The consensus tree inferred from %.0n optimal trees', [NoOfTrees*1.0]));
         if SBL(0) > 0 then
           b.Add(Format(' (sum of branch length = %.3f)', [SBL(0)]));
         b.Add(' is shown. Branches corresponding to partitions reproduced in less');
         b.Add(Format(' than %d%% trees are collapsed.', [ConsensusValue]));
         if StatPosCaption <> EmptyStr then
           b.Add(Format(' The percentage of parsimonious trees in which the associated taxa clustered together are shown %s.', [StatPosCaption]));
       end
       else if NoOfTrees > 1 then
       begin
         if IsGeneDups then
           b.Add(Format(' Tree #%d out of %d trees', [TreeIndex, NoOfTrees]))
         else
        b.Add(Format(' Tree #%d out of %d minimum evolution trees', [TreeIndex, NoOfTrees]));

        if SBL(TreeIndex) > 0 then
          b.Add(Format(' &#32(sum of branch length = %.3f)', [SBL(TreeIndex)]));
        b.Add('&#32is shown.');
        if info.MyTreePack.BootReps > 0 then
          b.Add(Format(' &#32 The percentage of replicate trees in which the associated taxa clustered together in the bootstrap test (%.0n replicates) are shown %s [{%s:BootstrapTestCitation}].', [info.MyTreePack.BootReps*1.0, StatPosCaption, POST_PROCESS_CITATION]));
       end
       else
       begin
         if FAnalysisInfo.MyTreePack.DoesContain(ttUserTree) and (not FAnalysisInfo.MyTreePack.DoesContain(ttInferTree)) then
           b.Add(Format(' The sum of branch lengths = %.3f for the displayed tree.', [SBL(TreeIndex)]))
         else
         begin
             b.Add(' The optimal tree');
           if SBL(TreeIndex) > 0 then
             b.Add(Format(' &#32with the sum of branch length = %.3f', [SBL(TreeIndex)]));
           b.Add(' is shown.');
         end;
         if info.MyTreePack.BootReps > 0 then
         begin
           if info.MyTreePack.TestType = ttCPTest then
             b.Add(Format(' &#32 The confidence probability (multiplied by 100) that the interior branch length is greater than 0, as estimated using the bootstrap test (%.0n replicates is shown %s [{%s:DopazoCPTestCitation}, {%s:RzhNeiCPTestCitation}].', [info.MyTreePack.BootReps*1.0, StatPosCaption, POST_PROCESS_CITATION, POST_PROCESS_CITATION]))
           else
             b.Add(Format(' &#32 The percentage of replicate trees in which the associated taxa clustered together in the bootstrap test (%.0n replicates) are shown %s [{%s:BootstrapTestCitation}].', [info.MyTreePack.BootReps*1.0, StatPosCaption, POST_PROCESS_CITATION]));
         end;
       end;
     end;

     if not ShowTopologyOnly then
     begin
       if IsLinearized  then
       begin
         b.Add(Format(' &#32 The phylogenetic tree was linearized assuming equal evolutionary rates in all lineages [{%s:LinearizedCitation}].', [POST_PROCESS_CITATION]));
         if IsTimescale then
           b.Add(Format(' &#32 The clock calibration to convert distance to time was %.4f (time/node height).', [TimeFactor]));
       end;
       b.Add(' &#32 The tree is drawn to scale, with branch lengths');
       if BLenCaption <> EmptyStr then
         b.Add(Format(' &#32(%s)', [BLenCaption]));
       b.Add(' &#32in the same units as those of the evolutionary distances used to infer the phylogenetic tree.');
     end
     else if BLenCaption <> EmptyStr then
       b.Add(Format(' &#32(%s).', [BLenCaption]));

     if not info.HasDistPack then
       b.Add(' The evolutionary distances were provided by the user.')
     else if info.DistModelName = EmptyStr then
       b.Add(Format(' The evolutionary distances were computed using the %s method [{%s:%s}] and are in the units of the %s.', [ModelName, POST_PROCESS_CITATION, info.MyDistPack.MethodCitation, info.DistUnitCaption]))
     else
       b.Add(Format(' &#32 The evolutionary distances were computed using the %s method [{%s:%s}] and are in the units of the %s.', [info.DistModelName, POST_PROCESS_CITATION, info.MyDistPack.MethodCitation, info.DistUnitCaption]));
     if info.HasDistPack and (info.DistGammaParaCaption <> EmptyStr) then
       b.Add(Format(' &#32 %s (shape parameter =  %.2f).', [info.DistGammaParaCaption, info.MyDistPack.GammaParameter]));

     if info.DistHeteroPatternCaption <> EmptyStr then
       b.Add(Format(' &#32 %s [{%sHeteroPatternDistCitation}].', [info.DistHeteroPatternCaption, POST_PROCESS_CITATION]));

     if info.MyTreePack.NameAcronym = 'ME' then
       b.Add(Format(' &#32 The ME tree was searched using the Close-Neighbor-Interchange (CNI) algorithm [{%s:MegaBook}] at a search level of %.0n.  The Neighbor-joining algorithm [{%s:NJCitation}] was used to generate the initial tree.', [POST_PROCESS_CITATION, info.MyTreePack.SearchFactor, POST_PROCESS_CITATION]));

     {$IFDEF VISUAL_BUILD}
     if Assigned(FTreeBox) and FTreeBox.ShowDataCoverage then
       b.Add(' The proportion of sites where at least 1 unambiguous base is present in at least 1 sequence for each descendent clade is shown next to each internal node in the tree.');

     if Assigned(FTreeBox) and  FTreeBox.IsGeneDups then
     begin
       if info.InitialUsrOperation = dtdoGeneDupInference then
         b.Add(' The evolutionary history shown is from a user-provided tree.');
       if info.UsedSpeciesTree = True then
     	 b.Add(Format(' Gene duplications were inferred using the method described in [{%s:GeneDupsCitation}].', [POST_PROCESS_CITATION]))
       else
         b.Add(' Gene duplications were identified by searching for all branching points in the topology with at least one species that is present in both subtrees of the branching point.');

       if not info.GeneTreeRooted then
         b.Add(' An unrooted gene tree was used for the analysis such that the search for duplication events was performed by finding the placement of the root on a branch or branches that produced the minimum number of duplication events.');
       b.Add(' ' + FTreeBox.NumDupsBreakdown);
     end;
     {$ENDIF}
     tempStr := GenerateDataSubsetCaption(FAnalysisInfo);
     if tempStr <> EmptyStr then
       b.Add(tempStr);
     b.Add(' </div>');
     Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

end.

