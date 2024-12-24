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

unit mancestral_states_caption;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF VISUAL_BUILD}
  MTreeBox,
  {$ENDIF}
  Classes, SysUtils, MAnalysisInfo, MegaConsts;

type

  { TAncestralSeqsCaption }

  TAncestralSeqsCaption = class(TObject)
    private
      {$IFDEF VISUAL_BUILD}
      FTree: TTreeBox;
      {$ENDIF}
      FAnalysisInfo: TAnalysisInfo;
      FCurDisplay: String;
      FShowingExtendedCodes: Boolean;
      function CurrentDisplayCaption(isML: Boolean): String;
      function TreeStyle: TTreeStyle;
      function ShowCharState: Boolean;
      function SiteIndexForCaption: String;
    public
      constructor Create(aInfo: TAnalysisInfo; curDisplay: String; showingExtendedCodes: Boolean{$IFDEF VISUAL_BUILD}; aTree: TTreeBox {$ENDIF});
      function GenerateCaption: String;
  end;

  function GenerateAncestralStatesCaption(aInfo: TAnalysisInfo; curDisplay: String; showingExtendedCodes: Boolean{$IFDEF VISUAL_BUILD}; aTree: TTreeBox {$ENDIF}): String;

implementation

uses
  mltree, mdistpack, mtreepack, mstringbuilder, MLTreeAnalyzer, ml_tree_caption,
  ml_rates_and_patterns_caption, mdata_subset_caption;

function GenerateAncestralStatesCaption(aInfo: TAnalysisInfo; curDisplay: String; showingExtendedCodes: Boolean{$IFDEF VISUAL_BUILD}; aTree: TTreeBox{$ENDIF}): String;
var
  c: TAncestralSeqsCaption = nil;
begin
  try
    c := TAncestralSeqsCaption.Create(aInfo, curDisplay, showingExtendedCodes{$IFDEF VISUAL_BUILD}, aTree{$ENDIF});
    Result := c.GenerateCaption;
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

{ TAncestralSeqsCaption }

function TAncestralSeqsCaption.CurrentDisplayCaption(isML: Boolean): String;
var
  b: TMegaStringBuilder = nil;
begin
  Result := EmptyStr;
  if TreeStyle <> tsTraditional then
    Exit;
  if not ShowCharState then
    Exit;

  try
    b := TMegaStringBuilder.Create;
    if isML then
      begin
        if FAnalysisInfo.MyUsrOperation = dtdoBEAM then
          b.Add(' For each node only the most probable state is shown.')
        else if FCurDisplay = SHOW_ALL then
          b.Add(' The set of states at each node is ordered from most likely to least likely, excluding states with probabilities below 5%.')
        else if FCurDisplay = SHOW_MOST_PROBABLE then
          b.Add(' For each node only the most probable state is shown.')
        else if FCurDisplay = HIDE_AMBIGUOUS then
          b.Add(' Ambiguous states are not shown.');
      end
      else
      begin
        if FAnalysisInfo.MyUsrOperation = dtdoBEAM then
          b.Add(' For each node only the most parsimonious state is shown.')
        else if FCurDisplay = SHOW_ALL then
        begin
          if FShowingExtendedCodes then
            b.Add(' Ambiguous states are shown using extended IUPAC codes.')
        end
        else if FCurDisplay = HIDE_AMBIGUOUS then
          b.Add(' Ambiguous states are not shown.');
      end;
    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

function TAncestralSeqsCaption.TreeStyle: TTreeStyle;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.TreeStyle;
  {$ELSE}
  Result := tsTraditional;
  {$ENDIF}
end;

function TAncestralSeqsCaption.ShowCharState: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.ShowCharState;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function TAncestralSeqsCaption.SiteIndexForCaption: String;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FTree.SiteIndexForCaption;
  {$ELSE}
  Result := '1';
  {$ENDIF}
end;

constructor TAncestralSeqsCaption.Create(aInfo: TAnalysisInfo; curDisplay: String; showingExtendedCodes: Boolean{$IFDEF VISUAL_BUILD}; aTree: TTreeBox {$ENDIF});
begin
  {$IFDEF VISUAL_BUILD}
  FTree := aTree;
  {$ENDIF}
  FAnalysisInfo := aInfo;
  FShowingExtendedCodes := showingExtendedCodes;
  FCurDisplay := curDisplay;
end;

function TAncestralSeqsCaption.GenerateCaption: String;
var
  b: TMegaStringBuilder = nil;
  a: TMLTreeAnalyzer = nil;
  m: TModelInfo = nil;
  tempStr: String = '';
begin
  try
    b := TMegaStringBuilder.Create;
    if Assigned(FAnalysisInfo.MyMLAnalysisPack) then
    begin
      a := FAnalysisInfo.MyMLAnalysisPack;
      m := TModelInfo.Create;
      a.GetModelInfo(m);
    end;
   // {if TTreeViewForm.ActiveTabName = 'Timetree'}
  	//{if TTreeViewForm.TimeTreeTypeStr = 'tttStrictClocksLocal'}
  	//	{IncludeExternalFile=StrictClockTree_snippet.htm}
  	//{elseif TTreeViewForm.TimeTreeTypeStr = 'tttStrictClocksMain'}
  	//	{IncludeExternalFile=StrictClockTree_snippet.htm}
  	//{else}
  	//	{IncludeExternalFile=Reltime_snippet.htm}
  	//{endif}
   // {else}

  b.Add(Format('Ancestral states were inferred using the %s method [{%s:%s}]', [FAnalysisInfo.MyTreePack.TreeMethodName, POST_PROCESS_CITATION, FAnalysisInfo.MyTreePack.TreeMethodCitation]));
  if Assigned(m) then
    b.Add(Format(' and %s model [{%s:%s}].', [m.BaseModelName, POST_PROCESS_CITATION, FAnalysisInfo.MyDistPack.MethodCitation]))
  else
    b.Add('.');

  if FAnalysisInfo.isAminoAcid then
    tempStr := 'amino acids'
  else
    tempStr := 'nucleotides';

    b.Add(Format(' The tree shows a set of possible %s (states) at each ancestral node', [tempStr]));
    if Assigned(m) then
      b.Add(' based on their inferred likelihood');
    b.Add(Format(' at site %s.', [SiteIndexForCaption]));
    tempStr := CurrentDisplayCaption(Assigned(a));
    if tempStr <> EmptyStr then
      b.Add(tempStr);

    if FAnalysisInfo.IsMyPegAnalysis then
      b.Add(Format(' The tree shown was obtained from the UCSC resource [{%s:%s}].', [POST_PROCESS_CITATION, FAnalysisInfo.MyTreePack.MyPegTreeMethodCitation]))
    else if FAnalysisInfo.UserTreeFile <> EmptyStr then
      b.Add(Format(' The initial tree was inferred using a pre-computed tree file (%s).', [FAnalysisInfo.UserTreeFileNameOnly]))
    else if Assigned(m) then
    begin
      b.Add(' ' + MLStartingTreeCaption(FAnalysisInfo));
    end
    else
    begin
      if FAnalysisInfo.MyTreePack.MPSearchName = 'SPR' then
        b.Add(Format(' The initial trees were obtained by the random addition of sequences (%.0n replicates).', [FAnalysisInfo.MyTreePack.RandomAddReps*1.0]))
      else if FAnalysisInfo.MyTreePack.MPSearchName = 'TBR' then
        b.Add(Format(' The initial trees were obtained by the random addition of sequences (%.0n replicates).', [FAnalysisInfo.MyTreePack.RandomAddReps*1.0]))
      else if FAnalysisInfo.MyTreePack.MPSearchName = 'CNI' then
      begin
        b.Add(' The initial trees were obtained with');
        if FAnalysisInfo.MyTreePack.MPInitialTreesMethod = 'Min-Mini' then
          b.Add(Format(' the Min-mini heuristic algorithm (search factor = %d).', [FAnalysisInfo.MyTreePack.SearchFactor]))
        else
          b.Add(Format(' the random addition of sequences (%.0n replicates).', [FAnalysisInfo.MyTreePack.RandomAddReps]));
      end;
    end;

    if Assigned(m) then
    begin
      tempStr := GenerateMLRatesAndPatternsCaption(FAnalysisInfo, a);
      if not SameText(tempStr, 'N/A') then
        b.Add(' ' + GenerateMLRatesAndPatternsCaption(FAnalysisInfo, a));
    end;
    b.Add(' ' + GenerateDataSubsetCaption(FAnalysisInfo));

    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

end.

