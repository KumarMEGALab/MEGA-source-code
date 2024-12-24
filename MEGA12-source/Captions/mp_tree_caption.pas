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

unit mp_tree_caption;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF VISUAL_BUILD}
  MTreeViewForm,
  {$ENDIF}
  Classes, SysUtils, MAnalysisInfo;

type

  { TMpTreeCaption }

  TMpTreeCaption = class(TObject)
    private
      FAnalysisInfo: TAnalysisInfo;
      {$IFDEF VISUAL_BUILD}
      FTreeViewForm: TTreeViewForm;
      {$ENDIF}
      function ActiveTabName: String;
      function HasCalibrations: Boolean;
      function DisplayingDivTimes: Boolean;
      function DisplayingErrorBars: Boolean;
      function ConsensusValue: Integer;
      function NoOfTrees: Integer;
      function StatPosCaption: String;
      function BlenCaption: String;
      function TreeIndex: Integer;
      function TreeLength: Integer;
      function TreeCI: String;
      function TreeRI: String;
      function TreeRCI: String;
      function TreeCI_infosites: String;
      function TreeRI_infosites: String;
      function TreeRCI_infosites: String;
      function ShowTopologyOnly: Boolean;
    public
      RI: Double;
      CI: Double;
      RCI: Double;
      iRI: Double;
      iCI: Double;
      iRCI: Double;
      constructor Create(aInfo: TAnalysisInfo{$IFDEF VISUAL_BUILD}; treeViewer: TTreeViewForm{$ENDIF});
      constructor CreateForNonVisual(aInfo: TAnalysisInfo; aRI, aCI, aRCI, aIRI, aICI, aIRCI: Double);
      function GenerateCaption: String;
  end;

  function GenerateMpTreeCaption(aInfo: TAnalysisInfo{$IFDEF VISUAL_BUILD}; treeViewer: TTreeViewForm{$ENDIF}): String;
  function GenerateMpTreeCaptionNonVisual(aInfo: TAnalysisInfo; aRI, aCI, aRCI, aIRI, aICI, aIRCI: Double): String;

implementation

uses
  MegaConsts, mtreepack, mstringbuilder, mdistpack, mdata_subset_caption;

function GenerateMpTreeCaption(aInfo: TAnalysisInfo{$IFDEF VISUAL_BUILD}; treeViewer: TTreeViewForm{$ENDIF}): String;
var
  c: TMpTreeCaption = nil;
begin
  try
    c := TMpTreeCaption.Create(aInfo{$IFDEF VISUAL_BUILD}, treeViewer{$ENDIF});
    Result := c.GenerateCaption;
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

function GenerateMpTreeCaptionNonVisual(aInfo: TAnalysisInfo; aRI, aCI, aRCI, aIRI, aICI, aIRCI: Double): String;
var
  c: TMpTreeCaption = nil;
begin
  try
    c := TMpTreeCaption.CreateForNonVisual(aInfo, aRI, aCI, aRCI, aIRI, aICI, aIRCI);
    Result := c.GenerateCaption;
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

{ TMpTreeCaption }

function TMpTreeCaption.ActiveTabName: String;
begin
  Result := EmptyStr;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeViewForm) then
    Result := FTreeViewForm.ActiveTabName;
  {$ELSE}
  Result := ORI_TREE_TAB;
  {$ENDIF}
end;

function TMpTreeCaption.HasCalibrations: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.HasCalibrations;
  {$ELSE}
  Result := Assigned(FAnalysisInfo.CalibrationTimes) and (FAnalysisInfo.CalibrationTimes.Count > 0);
  {$ENDIF}
end;

function TMpTreeCaption.DisplayingDivTimes: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.Tree.ShowDivergenceTimes;
  {$ELSE}
  Result := HasCalibrations;
  {$ENDIF}
end;

function TMpTreeCaption.DisplayingErrorBars: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.Tree.ShowHeightErrBar;
  {$ELSE}
  Result := HasCalibrations;
  {$ENDIF}
end;

function TMpTreeCaption.ConsensusValue: Integer;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.Tree.ConsensusValue;
  {$ELSE}
  Result := 50;
  {$ENDIF}
end;

function TMpTreeCaption.NoOfTrees: Integer;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.Tree.NoOfTrees;
  {$ELSE}
  Result := 1;
  {$ENDIF}
end;

function TMpTreeCaption.StatPosCaption: String;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.StatPosCaption;
  {$ELSE}
  if FAnalysisInfo.MyBootReps > 0 then
    Result := 'above the branches'
  else
    Result := EmptyStr;
  {$ENDIF}
end;

function TMpTreeCaption.BlenCaption: String;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.BLenCaption;
  {$ELSE}
  Result := 'above the branches';
  {$ENDIF}
end;

function TMpTreeCaption.TreeIndex: Integer;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.Tree.TreeIndex;
  {$ELSE}
  Result := 1;
  {$ENDIF}
end;

function TMpTreeCaption.TreeLength: Integer;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.InfoDisplay.TreeLength;
  {$ELSE}
  Result := Round(FAnalysisInfo.MyOriTreeList[0].Value);
  {$ENDIF}
end;

function TMpTreeCaption.TreeCI: String;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.Tree.TreeCI;
  {$ELSE}
  Result := Format('%.6f', [CI]);
  {$ENDIF}
end;

function TMpTreeCaption.TreeRI: String;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.Tree.TreeRI;
  {$ELSE}
  Result := Format('%.6f', [RI]);
  {$ENDIF}
end;

function TMpTreeCaption.TreeRCI: String;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.Tree.TreeRCI;
  {$ELSE}
  Result := Format('%.6f', [RCI]);
  {$ENDIF}
end;

function TMpTreeCaption.TreeCI_infosites: String;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.Tree.TreeCI_infosites;
  {$ELSE}
  Result := Format('%.6f', [iCI]);
  {$ENDIF}
end;

function TMpTreeCaption.TreeRI_infosites: String;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.Tree.TreeRI_infosites;
  {$ELSE}
  Result := Format('%.6f', [iRI]);
  {$ENDIF}
end;

function TMpTreeCaption.TreeRCI_infosites: String;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.Tree.TreeRCI_infosites;
  {$ELSE}
  Result := Format('%.6f', [iRCI]);
  {$ENDIF}
end;

function TMpTreeCaption.ShowTopologyOnly: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTreeViewForm.Tree.ShowTopologyOnly;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

constructor TMpTreeCaption.Create(aInfo: TAnalysisInfo{$IFDEF VISUAL_BUILD}; treeViewer: TTreeViewForm{$ENDIF});
begin
  RI := -1;
  CI := -1;
  RCI := -1;
  iRI := -1;
  iCI := -1;
  iRCI := -1;
  {$IFDEF VISUAL_BUILD}
  FTreeViewForm := treeViewer;
  {$ENDIF}
  FAnalysisInfo := aInfo;
end;

constructor TMpTreeCaption.CreateForNonVisual(aInfo: TAnalysisInfo; aRI, aCI, aRCI, aIRI, aICI, aIRCI: Double);
begin
  FAnalysisInfo := aInfo;
  RI := aRI;
  CI := aCI;
  RCI := aRCI;
  iRI := aIRI;
  iCI := aICI;
  iRCI := aIRCI;
end;

function TMpTreeCaption.GenerateCaption: String;
var
  b: TMegaStringBuilder = nil;
  tempStr: String = '';
begin
  try
    b := TMegaStringBuilder.Create;

    if FAnalysisInfo.MyUsrOperation in [dtdoMPComputeUserTreeBLens] then
  	 b.Add('The user-specified tree topology was analyzed')
    else
  	 b.Add('The evolutionary history was inferred');
    b.Add(Format(' using the %s method.', [FAnalysisInfo.MyTreePack.TreeMethodName]));

    if ActiveTabName = TIME_TREE_TAB then
    begin
  	b.Add('Divergence times for all branching points in the topology were calculated with the');
  	b.Add(Format(' RelTime method [{%s:ReltimeCitation}] using the branch lengths contained in the inferred tree.', [POST_PROCESS_CITATION]));
  	b.Add(Format(' Those branch lengths were calculated using the average pathway method [see pg. 132 in ref. {%s:MegaBook}] and', [POST_PROCESS_CITATION]));
  	b.Add(' are in the units of the number of changes over the whole sequence.');
  	if HasCalibrations then
        begin
  	  if DisplayingDivTimes then
          begin
  	    b.Add(' Relative node times were optimized and converted to absolute divergence times (shown next to branching points) based');
  	    b.Add(' on the user-supplied calibration constraints.');
  	  end;
  	end;
      if DisplayingErrorBars then
	b.Add(Format(' Bars around each node represent 95% confidence intervals which were computed using the method described in Tamura et al. (2013)[{%s:ReltimeCitation}].', [POST_PROCESS_CITATION]));
    end
    else
    begin
      if ActiveTabName = BOOT_CONS_TREE_TAB then
      begin
  	b.Add(Format('The bootstrap consensus tree inferred from %.0n', [FAnalysisInfo.MyBootReps*1.0]));
  	b.Add(' replicates is taken to represent the evolutionary history of the taxa');
  	b.Add(Format(' analyzed [{%s:BootstrapTestCitation}].', [POST_PROCESS_CITATION]));
  	b.Add(Format(' Branches corresponding to partitions reproduced in less than %d%% bootstrap replicates are collapsed.', [ConsensusValue]));
  	if StatPosCaption <> EmptyStr then
  	  b.Add(Format('The percentage of replicate trees in which the associated taxa clustered together in the bootstrap test (%d replicates) are shown %s [{%s:BootstrapTestCitation}].', [FAnalysisInfo.MyBootReps, StatPosCaption, POST_PROCESS_CITATION]));
      end
      else if (ActiveTabName = 'Consensus tree') or (TreeIndex = 0) then
      begin
  	b.Add(Format('The consensus tree inferred from %.0n most parsimonious trees', [NoOfTrees*1.0]));
  	if TreeLength > 0 then
  	  b.Add(Format('(length = %.0n)', [TreeLength*1.0]));
  	b.Add(Format(' is shown. Branches corresponding to partitions reproduced in less than %d%% trees are collapsed.', [ConsensusValue]));
  	b.Add(Format(' The consistency index is %s (%s),', [TreeCI, TreeCI_infosites]));
  	b.Add(Format(' the retention index is %s (%s), and', [TreeRI, TreeRI_infosites]));
  	b.Add(Format(' the composite index is %s (%s) for', [TreeRCI, TreeRCI_infosites]));
  	b.Add(' all sites and parsimony-informative sites (in parentheses).');
  	if StatPosCaption <> EmptyStr then
  	  b.Add(Format(' The percentage of parsimonious trees in which the associated taxa clustered together are shown %s.', [StatPosCaption]));
      end
      else if NoOfTrees > 1 then
      begin
  	b.Add(Format('Tree #%d out of %.0n most parsimonious trees (length = %.0n) is shown', [TreeIndex, NoOfTrees*1.0, TreeLength*1.0]));
  	b.Add(Format('The consistency index is %s (%s),', [TreeCI, TreeCI_infosites]));
  	b.Add(Format(' the retention index is %s (%s), and', [TreeRI, TreeRI_infosites]));
  	b.Add(Format(' the composite index is %s (%s) for', [TreeRCI, TreeRCI_infosites]));
  	b.Add(' all sites and parsimony-informative sites (in parentheses).');
  	if StatPosCaption <> EmptyStr then
  	  b.Add(Format(' The percentage of parsimonious trees in which the associated taxa clustered together are shown %s.', [StatPosCaption]));
      end
      else
      begin
  	if FAnalysisInfo.MyUsrOperation = dtdoMPComputeUserTreeBLens then
  	  b.Add(Format(' The tree length is %.0n.', [TreeLength*1.0]))
  	else
  	  b.Add(Format(' The most parsimonious tree with length = %.0n is shown.', [TreeLength*1.0]));

    	b.Add(Format(' The consistency index is %s (%s),', [TreeCI, TreeCI_infosites]));
    	b.Add(Format(' the retention index is %s (%s), and', [TreeRI, TreeRI_infosites]));
    	b.Add(Format(' the composite index is %s (%s) for', [TreeRCI, TreeRCI_infosites]));
    	b.Add(' all sites and parsimony-informative sites (in parentheses).');
    	if StatPosCaption <> EmptyStr then
    	  b.Add(Format(' The percentage of replicate trees in which the associated taxa clustered together are shown %s.', [StatPosCaption]));
      end;

      if FAnalysisInfo.MyTreePack.MPSearchName = 'Branch-&-Bound' then
      begin
  	b.Add(' The MP tree was obtained using the Max-mini branch-and-bound algorithm');
  	b.Add(Format(' (pg. 122 in ref. [{%s:MegaBook}]).', [POST_PROCESS_CITATION]));
  	b.Add(Format(' This search guarantees to find all optimal topologies and resulted in %.0n parsimony tree(s).', [NoOfTrees*1.0]));
      end
      else if FAnalysisInfo.MyTreePack.MPSearchName = 'Min-Mini' then
  	b.Add(Format(' The MP tree was obtained using the Min-mini heuristic algorithm (pg. 128 in ref. [{%s:MegaBook}]) with a search factor of %.0n.', [POST_PROCESS_CITATION, FAnalysisInfo.MyTreePack.SearchFactor]))
      else if FAnalysisInfo.MyTreePack.MPSearchName = 'SPR' then
  	b.Add(Format(' The MP tree was obtained using the Subtree-Pruning-Regrafting (SPR) algorithm (pg. 126 in ref. [{%s:MegaBook}]) with search level %.d in which the initial trees were obtained by the random addition of sequences (%.0n replicates).', [POST_PROCESS_CITATION, FAnalysisInfo.MyTreePack.SearchLevel, FAnalysisInfo.MyTreePack.RandomAddReps*1.0]))
      else if FAnalysisInfo.MyTreePack.MPSearchName = 'TBR' then
  	b.Add(Format(' The MP tree was obtained using the Tree-Bisection-Regrafting (TBR) algorithm (pg. 126 in ref. [{%s:MegaBook}]) with search level %d in which the initial trees were obtained by the random addition of sequences (%.0n replicates).', [POST_PROCESS_CITATION, FAnalysisInfo.MyTreePack.SearchLevel, FAnalysisInfo.MyTreePack.RandomAddReps*1.0]))
      else if FAnalysisInfo.MyTreePack.MPSearchName = 'CNI' then
      begin
        b.Add(Format(' The MP tree was obtained using the Close-Neighbor-Interchange algorithm (pg. 128 in ref. [{%s:MegaBook}]) with search level %d in which the initial trees were obtained with', [POST_PROCESS_CITATION, FAnalysisInfo.MyTreePack.SearchLevel]));
  	if FAnalysisInfo.MyTreePack.MPInitialTreesMethod = 'MinMini' then
  	  b.Add(Format(' the Min-mini heuristic algorithm (search factor =  %.2f).', [FAnalysisInfo.MyTreePack.SearchFactor]))
  	else
  	  b.Add(Format(' the random addition of sequences (%.0n replicates).', [FAnalysisInfo.MyTreePack.RandomAddReps*1.0]));
      end;

      if BlenCaption <> EmptyStr then
  	    b.Add(Format(' Branch lengths were calculated using the average pathway method [see pg. 132 in ref. {%s:MegaBook}] and are in the units of the number of changes over the whole sequence. They are shown %s.', [POST_PROCESS_CITATION, BLenCaption]))
      else if not ShowTopologyOnly then
      begin
  	b.Add(' The tree is drawn to scale');
  	if BLenCaption <> EmptyStr then
  	  b.Add(Format(', with branch lengths calculated using the average pathway method [see pg. 132 in ref. {%s:MegaBook}] and are in the units of the number of changes over the whole sequence', [POST_PROCESS_CITATION]));
        b.Add('.');
      end;
    end;
    // add gene duplications?
    b.Add(' ');
    b.Add(GenerateDataSubsetCaption(FAnalysisInfo));
    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

end.

