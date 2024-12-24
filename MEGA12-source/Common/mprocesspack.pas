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

unit MProcessPack;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MegaVerConsts, MDistPack, MegaConsts;

{$M+}
type
  TProcessType = (ppNone,
                 ppInfer, ppNJ, ppME, ppUPGMA, ppMP,
                 ppML, ppZtest, ppFisherExact, ppTajimaNeutrality,
                 ppEstimateSelectionCodons, ppTajimaRelativeRate,
                 ppMLClock, ppMLClockLocal, ppMLIQTree, ppRelTimeML, ppRelTimeBLens,
                 ppRelTimeLS, ppAncSeq, ppModelSelML, ppSubPatternEstML,
                 ppTsTvML, ppEstRateVarAmongSitesML, ppSubPatternEstMCL,
                 ppTsTvMCL, ppEstRateBySite, ppDistTestPatternHomo,
                 ppEstCompDist, ppEstDispIndex, ppDistEst, ppPairTaxa,
                 ppWithinGpAvg, ppBetweenGpAvg, ppNetBetweenGpAvg, ppOverallMean,
                 ppAvgInSubPops, ppAvgOverallPops, ppPhyloQ, ppInterPopDiversity,
                 ppPorportionInterDiversity, ppAnalyzeUserTree,
                 ppLeastSquares, ppOLSInteriorBranchTest, ppAlign, ppProfileAlignment,
                 ppClustalW, ppMuscle, ppPredictLivingSeq, ppMyPegAncestInfer, ppInferGeneDups,
                 ppCalcBlens, ppCalTest, ppFindClones, ppBuildTumorTree, ppCorrTestBlens, ppCorrTestML, ppSkipMinMax,
                 ppEpML, ppRtdtMl, ppRtdtLs, ppRtdtBlens, ppBEAM, ppMegaAlignment, ppFastaAlignment,
                 ppAlignmentExplorer, ppDistanceMatrixExplorer, ppTreeExplorer, ppExcelSpreadsheet, ppCaptionExpert,
                 ppNewickOut, ppNewickIn, ppCalibrationConstraints, ppTaxaGroups, ppOutputName, ppSiteCoverage,
                 ppModelSelIQTree, ppSequenceFileIterator, ppTreeFileIterator, ppDistMatrixFileIterator,
                 ppModelTamerML, ppLbsInference, ppLbsTiming, ppLbsAnalyzeTree, ppDrPhylo);

  TProcessTypes = array[0..20] of TProcessType;
  TProcessTypesSet = Set of TProcessType;
  dataTypeEnum = (dtNucleotide, dtCodingNucleotide, dtProtein, dtDistance);

  { TProcessPack }

  TProcessPack = class
  private
    AvailableIndex: integer;
    FileToSaveTo : AnsiString;
    FIsWorkflowPack: Boolean;
    FWorkflowAnalysisInfo: TObject;
    WritingToFile : Boolean;
    function HasCodonData: Boolean;
    function HasMultiFileData: Boolean;
    function HasNucleotideData: Boolean;
    procedure SetIsWorkflowPack(AValue: Boolean);
    function UseSequenceData: Boolean;
    function UseDistanceData: Boolean;
    function DataTypeString: String;
    procedure Needs(Needs: AnsiString);
    procedure HandleTimeLimit;
    procedure CheckBLensCalc;
    procedure DevActionInfoToFile;
  public
    OnFreeNotify: TNotifyEvent;
    TextualSettingsList : TStringList;
    TreeFile: String;
    CalibrationFile: String;
    GroupsFile: String;
    ProcessTypes: TProcessTypes;
    ProcessImageIndex: Integer;
    ProcessParentImageIndex: Integer;
    CommonName: String;
    dataType: dataTypeEnum;
    containsCodingNuc: Boolean;
    MyPegPeptideSite: Integer;
    CalibrationWizard: TObject;
    function ProposeFilename(dtypeStr: String =''): String;
    function ppTodtdo: TDistTreeDlgOption; // Moved so that Webtop could access this function
    function HasAminoAcidData: Boolean;
    procedure SaveToFile(FileName: String);
    function toStringList(): TStringList;
    procedure AddProcessType(ProcessType : TProcessType);
    function TryAddProcessTypeFromString(processTypeString: String): Boolean;
    function Count: integer;
    procedure AddKeyValuePairSetting(key: AnsiString; value: AnsiString); Overload;
    procedure AddKeyValuePairSetting(KeyValue : AnsiString); Overload;
    procedure PerformAnalysis;
    function ClockLevel: Integer;
    function GetClockLevel: TClockLevel;
    function GetClockType: TClockType;
    function GetRateMergeOption: Boolean;
    function GetBootVarReps: Integer;
    function GetCalibrationFile: String;
    function MaxRateRatio: Extended;
    function MLSearchFilter: Extended;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TProcessPack);
    function StateToStringList(Comment: String=''): TStringList;
    function ContainsProcessType(MyProcessType: TProcessType): Boolean;
    function needsTree: Boolean;
    function NeedsTreeFileOnly: Boolean;
    function HasProcessType(AType: TProcessType): Boolean;
    function IsGettingWorkflowSettings: Boolean;
    {$IFNDEF VISUAL_BUILD}
    function HasSetting(settingName: String): Boolean;
    function SettingValueIs(settingName: String; settingValue: String): Boolean;
    function SettingIsApplicable(settingName: String): Boolean;
    function SettingString(settingName: String): String;
    {$ENDIF}
    property IsWorkflowPack: Boolean read FIsWorkflowPack write SetIsWorkflowPack;
    property WorkflowAnalysisInfo: TObject read FWorkflowAnalysisInfo write FWorkflowAnalysisInfo;
end;

type
  Long = record
    LoWord: Word;
    HiWord: Word;
  end;

const
  CRCPOLY = $EDB88320;

 var
  CRCTable: array[0..512] Of Longint;
  CurrentActionCaption: String = '';

  function ProcessTypeString(AType: TProcessType): String;
  function DataTypeEnumString(AType: dataTypeEnum): String;
  function ProcessTypesThatRequireSequenceAlignment: TProcessTypesSet;
  function ProcessTypesThatRequireATree: TProcessTypesSet;
  function ProcessTypesThatRequireGroups: TProcessTypesSet;
  function ProcessTypesThatUseDistanceMatrix: TProcessTypesSet;
  function ProcessTypesThatUseCalibrations: TProcessTypesSet;

  function ProcessTypesThatProduceSequenceAlignment: TProcessTypesSet;
  function ProcessTypesThatProduceATree: TProcessTypesSet;
  function ProcessTypesThatProduceADistanceMatrix: TProcessTypesSet;
  function ProcessTypesThatProduceCalibrations: TProcessTypesSet;
  function ProcessTypesThatProduceGroups: TProcessTypesSet;
  function ProcessTypesThatProduceASpreadsheet: TProcessTypesSet;


implementation

uses
  {$IFDEF VISUAL_BUILD}
  MEditorForm, Mega_Main, MV_DistDataExplorer,
  {$ELSE}
  mtimelimit, MD_MegaMain, MegaUtils_NV,
  {$ENDIF}
  MaskEdit, LCLIntf, LCLType, KeywordConsts, MD_InputSeqData, MD_InputDistData,
  Graphics, Controls, Forms, Dialogs, Menus, ExtCtrls, ComCtrls, Buttons,
  ActnList, ImgList, MLegendGenerator, DateUtils, StrUtils, MegaUtils, ProcessMLTreeCmds,
  ProcessTreeCmds, ProcessParsimTreeCmds, ProcessDistCmds, ProcessTestCmds,
  ProcessCodonOmegaCmds, TypInfo, MD_Align,
  MegaAnalysisPrefStrings;

function ProcessTypeString(AType: TProcessType): String;
begin
  Result := EmptyStr;
  case AType of
  ppNone: Result := 'none';
  ppInfer: Result := 'infer';
  ppNJ: Result := 'NJ';
  ppME: Result := 'ME';
  ppUPGMA: Result := 'UPGMA';
  ppMP: Result := 'MP';
  ppML: Result := 'ML';
  ppZtest: Result := 'zTest';
  ppFisherExact: Result := 'fisher_exact_test';
  ppTajimaNeutrality: Result := 'tajima_neutrality_test';
  ppEstimateSelectionCodons: Result := 'estimate_selection_codons';
  ppTajimaRelativeRate: Result := 'tajima_rel_rate_test';
  ppMLClock: Result := 'ML_clock_test';
  ppMLClockLocal: Result := 'local_clocks';
  ppRelTimeML: Result := 'reltime_ml';
  ppRelTimeBLens: Result := 'reltime_blens';
  ppRelTimeLS: Result := 'reltime';
  ppRtdtML: Result := 'rtdt_ml';
  ppRtdtLS: Result := 'rtdt_ls';
  ppRtdtBlens: Result := 'rtdt_blens';
  ppAncSeq: Result := 'ancestral_seqs';
  ppModelSelML: Result := 'model_sel_ml';
  ppModelTamerML: Result := 'model_tamer_ml';
  ppSubPatternEstML: Result := 'substitution_pattern_ml';
  ppTsTvML: Result := 'R_ratio_ml';
  ppEstRateVarAmongSitesML: Result := 'rate_var_among_sites_ml';
  ppSubPatternEstMCL: Result := 'est_substitute_pattern_mcl';
  ppTsTvMCL: Result := 'R_ratio_MCL';
  ppEstRateBySite: Result := 'site_by_site_rates';
  ppDistTestPatternHomo: Result := 'pattern_homogeneity_test';
  ppEstCompDist: Result := 'composition_distance';
  ppEstDispIndex: Result := 'est_disparity_index';
  ppDistEst: Result := 'distance_estimation';
  ppPairTaxa: Result := 'pairwise';
  ppWithinGpAvg: Result := 'within_grp_avg';
  ppBetweenGpAvg: Result := 'between_grp_avg';
  ppNetBetweenGpAvg: Result := 'net_between_grp_avg';
  ppOverallMean: Result := 'overall_mean';
  ppAvgInSubPops: Result := 'avg_in_sub_pops';
  ppAvgOverallPops: Result := 'avg_overall_pops';
  ppPhyloQ: Result := 'phyloq';
  ppInterPopDiversity: Result := 'inter_pop_diversity';
  ppPorportionInterDiversity: Result := 'proportion_inter_pop_diversity';
  ppAnalyzeUserTree: Result := 'analyze_user_tree';
  ppLeastSquares: Result := 'least_squares';
  ppOLSInteriorBranchTest: Result := 'ols_interior_branch_test';
  ppAlign: Result := 'align';
  ppProfileAlignment: Result := 'profile_alignment';
  ppClustalW: Result := 'clustal';
  ppMuscle: Result := 'muscle';
  ppPredictLivingSeq: Result := 'living_sequence_prediction';
  ppEpML: Result := 'evolutionary_probabilities';
  ppMyPegAncestInfer: Result := 'ancestral_seqs';
  ppInferGeneDups: Result := 'gene_dup_inference';
  ppCorrTestML: Result := 'rate_correlation_test_ml';
  ppCorrTestBlens: Result := 'rate_correlation_test_blens';
  ppBEAM: Result := 'impute_missing_data';
  ppFindClones: Result := 'find_clones';
  ppSiteCoverage: Result := 'site_coverage';
  ppLbsAnalyzeTree: Result := 'lbs_analyze_tree';
  ppLbsInference: Result := 'lbs_infer';
  ppLbsTiming: Result := 'lbs_timing';
  ppCalcBlens: Result := EmptyStr;
  else
    begin
      Assert(False, 'missing TProcessType handler');
    end;
  end;
end;

function DataTypeEnumString(AType: dataTypeEnum): String;
begin
  Result := EmptyStr;
  case AType of
  dtNucleotide: Result := 'nucleotide';
  dtCodingNucleotide: Result := 'coding';
  dtProtein: Result := 'protein';
  dtDistance: Result := 'distances';
  end;
end;

function ProcessTypesThatRequireSequenceAlignment: TProcessTypesSet;
begin
  Result := [ppInfer, ppML, ppMP, ppZtest, ppFisherExact, ppTajimaNeutrality, ppTajimaRelativeRate, ppMLClock,
             ppMLIQTree, ppRelTimeML, ppRelTimeLS, ppAncSeq, ppModelSelML, ppSubPatternEstMCL, ppSubPatternEstML,
             ppTsTvMCL, ppTsTvML, ppEstRateBySite, ppEstRateVarAmongSitesML, ppDistTestPatternHomo, ppDistEst,
             ppEstCompDist, ppEstDispIndex, ppWithinGpAvg, ppBetweenGpAvg, ppNetBetweenGpAvg, ppOverallMean,
             ppAvgInSubPops, ppAvgOverallPops, ppInterPopDiversity, ppPorportionInterDiversity, ppAnalyzeUserTree,
             ppLeastSquares, ppOLSInteriorBranchTest, ppPredictLivingSeq, ppMyPegAncestInfer, ppInferGeneDups,
             ppCalcBlens, ppCalTest, ppFindClones, ppBuildTumorTree, ppCorrTestML, ppEpML, ppRtdtMl, ppRtdtLs,
             ppBEAM, ppAlignmentExplorer, ppClustalW, ppMuscle, ppLbsTiming, ppLbsAnalyzeTree, ppLbsInference];
end;

function ProcessTypesThatRequireATree: TProcessTypesSet;
begin
  Result := [ppMLClock, ppRelTimeBLens, ppRelTimeLS, ppRelTimeML, ppAncSeq,
             ppAnalyzeUserTree, ppPredictLivingSeq, ppMyPegAncestInfer, ppInferGeneDups,
             ppCalcBlens, ppCalTest, ppCorrTestML, ppCorrTestBlens, ppEpML, ppRtdtBlens,
             ppRtdtLs, ppRtdtMl, ppTreeExplorer, ppLbsAnalyzeTree];
end;

function ProcessTypesThatRequireGroups: TProcessTypesSet;
begin
  Result := [ppMLClock, ppRelTimeBLens, ppRelTimeLS, ppRelTimeML, ppWithinGpAvg,
             ppBetweenGpAvg, ppNetBetweenGpAvg, ppAvgInSubPops, ppAvgOverallPops,
             ppInterPopDiversity, ppPorportionInterDiversity, ppCalTest, ppCorrTestBlens,
             ppCorrTestML, ppEpML, ppRtdtBlens, ppRtdtLs, ppRtdtMl];
end;

function ProcessTypesThatUseDistanceMatrix: TProcessTypesSet;
begin
  Result := [ppNJ, ppME, ppUPGMA, ppDistanceMatrixExplorer];
end;

function ProcessTypesThatUseCalibrations: TProcessTypesSet;
begin
  Result := [ppRelTimeML, ppRelTimeBLens, ppRelTimeLS, ppRtdtMl, ppRtdtLs, ppRtdtBlens, ppLbsTiming];
end;

function ProcessTypesThatProduceSequenceAlignment: TProcessTypesSet;
begin
  Result := [ppAlign, ppProfileAlignment, ppClustalW, ppMuscle, ppMegaAlignment, ppFastaAlignment, ppSequenceFileIterator];
end;

function ProcessTypesThatProduceATree: TProcessTypesSet;
begin
  Result := [ppInfer, ppNJ, ppME, ppUPGMA, ppMP, ppMLIQTree, ppRelTimeML, ppRelTimeBLens,
             ppRelTimeLS, ppAnalyzeUserTree, ppCalcBlens, ppCalTest, ppBuildTumorTree,
             ppRtdtMl, ppRtdtLs, ppRtdtBlens, ppNewickIn, ppTreeFileIterator, ppLbsInference];
end;

function ProcessTypesThatProduceADistanceMatrix: TProcessTypesSet;
begin
  Result := [ppDistTestPatternHomo, ppEstCompDist, ppEstDispIndex, ppDistEst,
             ppWithinGpAvg, ppBetweenGpAvg, ppNetBetweenGpAvg, ppDistanceMatrixExplorer,
             ppDistMatrixFileIterator];
end;

function ProcessTypesThatProduceCalibrations: TProcessTypesSet;
begin
  Result := [ppCalibrationConstraints]
end;

function ProcessTypesThatProduceGroups: TProcessTypesSet;
begin
  Result := [ppTaxaGroups];
end;

function ProcessTypesThatProduceASpreadsheet: TProcessTypesSet;
begin
  Result := [ppZtest, ppFisherExact, ppTajimaNeutrality, ppTajimaRelativeRate,
             ppMLClock, ppMLClockLocal, ppRelTimeML, ppRelTimeBLens, ppRelTimeLS, ppAncSeq,
             ppModelSelML, ppSubPatternEstML, ppTsTvML, ppEstRateBySite, ppDistTestPatternHomo,
             ppEstCompDist, ppEstDispIndex, ppDistEst, ppWithinGpAvg, ppBetweenGpAvg,
             ppNetBetweenGpAvg, ppPredictLivingSeq, ppMyPegAncestInfer, ppEpML, ppRtdtMl,
             ppRtdtLs, ppRtdtBlens, ppDistanceMatrixExplorer]
end;

constructor TProcessPack.Create;
begin
  OnFreeNotify := nil;
  AvailableIndex := 0;
  TextualSettingsList := TStringList.Create;
  WritingToFile := False;
  ProcessImageIndex := -1;
  ProcessParentImageIndex := -1;
  CalibrationWizard := nil;
  GroupsFile := EmptyStr;
  FIsWorkflowPack := False;
end;

procedure TProcessPack.AddProcessType(ProcessType: TProcessType);
begin
  ProcessTypes[AvailableIndex] := ProcessType;
  inc(AvailableIndex);
end;

function TProcessPack.TryAddProcessTypeFromString(processTypeString: String): Boolean;
var
  aType: TProcessType;
begin
  try
    aType := TProcessType(GetEnumValue(TypeInfo(TProcessType), processTypeString));
    AddProcessType(aType);
    Result := HasProcessType(aType);
  except
    Result := False;
  end;
end;

procedure TProcessPack.Assign(Source: TProcessPack);
var
  i: Integer;
begin
  FIsWorkflowPack := Source.IsWorkflowPack;
  AvailableIndex := 0;
  FileToSaveTo := Source.FileToSaveTo;
  WritingToFile := Source.WritingToFile;
  if Assigned(Source.TextualSettingsList) then
  begin
    if not Assigned(TextualSettingsList) then
      TextualSettingsList := TStringList.Create;
    TextualSettingsList.Assign(Source.TextualSettingsList);
  end;
  TreeFile := Source.TreeFile;
  CalibrationFile := Source.CalibrationFile;
  for i := Low(Source.ProcessTypes) to High(Source.ProcessTypes) do
    AddProcessType(Source.ProcessTypes[i]);
  ProcessImageIndex := Source.ProcessImageIndex;
  ProcessParentImageIndex := Source.ProcessParentImageIndex;
  CommonName := Source.CommonName;
  dataType := Source.dataType;
  containsCodingNuc := Source.ContainsCodingNuc;
  MyPegPeptideSite := Source.MyPegPeptideSite;
end;

procedure TProcessPack.Needs(Needs : AnsiString);
begin
  Raise Exception.Create('MEGA needs ' + Needs + ' data to perform this analysis');
end;

procedure TProcessPack.HandleTimeLimit;
{$IFNDEF VISUAL_BUILD}
var
  HasConstraint: Boolean;
  Hours: Double;
{$ENDIF}
begin
  {$IFNDEF VISUAL_BUILD}
  try
    if TextualSettingsList.IndexOfName(opsHasTimeLimit) >= 0 then
    begin
      if TryStrToBool(TextualSettingsList.Values[opsHasTimeLimit], HasConstraint) then
      begin
        if HasConstraint then
        begin
          if TextualSettingsList.IndexOfName(opsMaxExecutionTime2) > 0 then
          begin
            if TryStrToFloat(TextualSettingsList.Values[opsMaxExecutionTime2], Hours) then
            begin
              if Hours > 0.0 then
                LaunchTimeLimitThread(Hours);
            end
            else
              warn_nv('invalid time limit constraint was encountered and so the constraint was ignored');
          end
          else
          warn_nv('Time limit constraint was not found');
        end;
      end
      else
        warn_nv('invalid time limit constraint was encountered and so the constraint was ignored');
    end;
  except
    on E:Exception do
      warn_nv('Warning: an error occurred when setting the max time limit constraint and so the constraint was ignored: ' + E.Message);
  end;
  {$ELSE}
  Assert(False, 'Time limit not implemented for MEGA-GUI');
  {$ENDIF}
end;

procedure TProcessPack.CheckBLensCalc;
begin
  {$IFNDEF VISUAL_BUILD}
  if TextualSettingsList.IndexOfName(opsCalcBLens2) >= 0 then
  begin
    if SameText(TextualSettingsList.Values[opsCalcBLens2], 'Yes') then
      AddProcessType(ppCalcBLens);
  end;
  {$ENDIF}
end;

procedure TProcessPack.DevActionInfoToFile;
{$IFDEF DEBUG}
var
  aFile: TextFile;
  filename: String;
  i: Integer;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  if not IsDeveloper then
    Exit;
  filename := 'C:\Users\gstecher\mydata\Documents\MEGA X\Examples\MegaActionData.txt';
  AssignFile(aFile, filename);
  try
    if not FileExists(filename) then
      Rewrite(aFile)
    else
      Append(aFile);
    Write(aFile, CurrentActionCaption, '=');
    for i := 0 to AvailableIndex - 1 do
      if i < (AvailableIndex - 1) then
        Write(aFile, ProcessTypes[i], ',')
      else
        WriteLn(aFile, ProcessTypes[i]);
  finally
    CloseFile(aFile);
  end;
  {$ENDIF}
end;

function TProcessPack.ProposeFilename(dtypeStr: String = ''): String;
var
  i: Integer;
begin
  Result := EmptyStr;
  if HasProcessType(ppBEAM) then
  begin
    Result := ProcessTypeString(ppBEAM) + '_SNV';
    Exit;
  end;
  if HasProcessType(ppFindClones) then
  begin
    Result := ProcessTypeString(ppFindClones) + '_SNV';
    Exit;
  end;
  if HasProcessType(ppEpML) then
  begin
    Result := ProcessTypeString(ppEpML) + '_' + dtypeStr;
    Exit;
  end;
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      if i = 0 then
        Result := ProcessTypeString(ProcessTypes[i])
      else
        Result := Result + '_' + ProcessTypeString(ProcessTypes[i]);
    end;
    if Trim(dtypeStr) <> EmptyStr then
      Result := Result + '_' + dtypeStr
    {$IFDEF VISUAL_BUILD}
    else if MegaForm.DataTypeString <> EmptyStr then
      Result := Result + '_' + MegaForm.DataTypeString;
    {$ELSE}
    ;
    {$ENDIF}
  end
  else
    Result := 'MEGA_result';
  Result := StringReplace(Result, ' ', '_', [rfReplaceAll]);
end;

function TProcessPack.NeedsTreeFileOnly: Boolean;
begin
  Result := False;
  if ContainsProcessType(ppRelTimeBLens) or ContainsProcessType(ppCorrTestBlens) or ContainsProcessType(ppRtdtBlens) then
    Result := True;
end;

function TProcessPack.needsTree: Boolean;
var
  UsrOperation : TDistTreeDlgOption;
  analysesWhichMustHave, hasTreeLine: Boolean;
begin
  UsrOperation := ppToDtdo;
  analysesWhichMustHave := (UsrOperation = dtdoMLInferAncSeq) or
                           (UsrOperation = dtdoMLComputeUserTreeBLens) or
                           (UsrOperation = dtdoMLClockTest) or
                           (UsrOperation = dtdoMPComputeUserTreeBLens) or
                           (UsrOperation = dtdoMPInferAncSeq) or
                           (UsrOperation = dtdoOLSComputeUserTreeBLens) or
                           (UsrOperation = dtdoOLSInteriorBranchTest) or
                           (UsrOperation = dtdoEpML) or
                           (UsrOperation = dtdoMLPredictLivingSeq) or
                           (UsrOperation = dtdoRelTimeBLens) or
                           (UsrOperation = dtdoRelTimeLS) or
                           (UsrOperation = dtdoRelTimeML) or
                           (UsrOperation = dtdoRtdtBlens) or
                           (UsrOperation = dtdoRtdtLS) or
                           (UsrOperation = dtdoRtdtML) or
                           (UsrOperation = dtdoCorrTestBlens) or
                           (UsrOperation = dtdoCorrTestML) or
                           (UsrOperation = dtdoPhyloQ);
  hasTreeLine := (TextualSettingsList.Values[opsTreeToUse2] = UserSpecifyFromFile) or
                 (TextualSettingsList.Values[opsMLInitialTrees2] = UserSpecifyFromFile);
  result := analysesWhichMustHave or hasTreeLine;
end;

function TProcessPack.ppTodtdo: TDistTreeDlgOption;
begin
  Case ProcessTypes[0] of
    //Normal Analysis Preferences
    ppInfer:
    Case ProcessTypes[1] of
      ppNJ:
      begin
        result := dtdoNJTree;
      end;
      ppME:
      begin
        result := dtdoMETree;
      end;
      ppUPGMA:
      begin
        result := dtdoUPGMATree;
      end;
      ppMP:
        result := dtdoMPTree;
      ppML:
        result := dtdoMLTree;
      else
        Raise Exception.Create('Unable to determine what Statistical Method to use to perform a ' + GetEnumName(TypeInfo(TProcessType),
                  integer(ProcessTypes[1])));
    end;
    ppZtest: result := dtdoSelectionZTest;
    ppFisherExact:  result := dtdoSelectionExactTest;
    ppTajimaNeutrality:  result := dtdoTajimaNeutralityTest;
    ppEstimateSelectionCodons: result := dtdoMLCodonOmega;
    ppTajimaRelativeRate:      result := dtdoTajimaClockTest;
    ppMLClock:  result := dtdoMLClockTest;
    ppMLClockLocal:  result := dtdoMLClockTestLocal;
    ppRelTimeML: result := dtdoRelTimeML;
    ppRelTimeBLens: result := dtdoRelTimeBLens;
    ppRelTimeLS: result := dtdoRelTimeLS;
    ppRtdtML: result := dtdoRtdtML;
    ppRtdtLS: result := dtdoRtdtLS;
    ppRtdtBlens: result := dtdoRtdtBlens;
    ppAncSeq:
    case ProcessTypes[1] of
      ppML: if ProcessTypes[2] = ppPredictLivingSeq then
              result := dtdoMLPredictLivingSeq
            else if ProcessTypes[2] = ppEpML then
              result := dtdoEpML
            else
              result := dtdoMLInferAncSeq;
      ppMP: if ProcessTypes[2] = ppPredictLivingSeq then
                result := dtdoMPPredictLivingSeq
              else
                result := dtdoMPInferAncSeq;
      else Raise Exception.Create('Unable to determine what Statistical Method to use to perform an ' + GetEnumName(TypeInfo(TProcessType),
                  integer(ProcessTypes[1])));
    end;
    ppModelSelML:            result := dtdoMLModelTest;
    ppSubPatternEstML:       result := dtdoMLComputePattern;
    ppTsTvML:  result := dtdoMLTsTvBias;
    ppEstRateVarAmongSitesML:   result := dtdoMLGammRates;
    ppSubPatternEstMCL:  result := dtdoMCLComputePattern;
    ppTsTvMCL:    result := dtdoMCLTsTvBias;
    ppEstRateBySite: result := dtdoMLInferSiteBySiteRates;
    ppDistTestPatternHomo: result := dtdoDisparityIndexTest;
    ppEstCompDist:  result := dtdoCompositionDistance;
    ppEstDispIndex: result := dtdoDisparityIndex;
    ppDistEst:
      Case ProcessTypes[1] of
        ppPairTaxa:  result := dtdoPairwiseDist;
        ppWithinGpAvg:
        begin
          result := dtdoWithinGroupMean;
        end;
        ppBetweenGpAvg:
        begin
          result := dtdoBetweenGroupMean;
        end;
        ppNetBetweenGpAvg:
        begin
          result := dtdoNetGroupMean;
        end;
        ppOverallMean:
        begin
            result := dtdoOverallMean;
        end;
        ppAvgInSubPops: result := dtdoAvgDiversityWithinSubPops;
        ppAvgOverallPops: result := dtdoAvgDiversityForEntirePop;
        ppInterPopDiversity:  result := dtdoInterPopDiversity;
        ppPorportionInterDiversity: result := dtdoPropOfInterPopDiversity;
        else  Raise Exception.Create('Unable to determine what Scope to use to perform a ' + GetEnumName(TypeInfo(TProcessType),
                  integer(ProcessTypes[1])));
      end;
      ppAnalyzeUserTree:
        Case ProcessTypes[1]  of
          ppLeastSquares, ppMuscle:
          begin
            if ProcessTypes[2] = ppPhyloQ then
            begin
              result := dtdoPhyloQ;
            end
            else
            begin
              result := dtdoOLSComputeUserTreeBLens;
            end;
          end;
          ppML, ppMLIQTree:    result := dtdoMLComputeUserTreeBLens;
          ppMP:   result := dtdoMPComputeUserTreeBLens;
          ppLbsAnalyzeTree: Result := dtdoLbsAnalyzeTree;
          else Raise Exception.Create('Unable to determine which Statistical Method to use to ' + GetEnumName(TypeInfo(TProcessType),
                  integer(ProcessTypes[1])));

        end;
     ppOLSInteriorBranchTest: result := dtdoOLSInteriorBranchTest;

     ppAlign: Result := dtdoAlign;
     ppMuscle: Result := dtdoAlign;
     ppCorrTestBlens: Result := dtdoCorrTestBlens;
     ppCorrTestML: Result := dtdoCorrTestML;
     ppBEAM: Result := dtdoBEAM;
     ppLbsAnalyzeTree: Result := dtdoLbsAnalyzeTree;
     ppLbsInference: Result := dtdoLbsInference;
     ppLbsTiming: Result := dtdoLbsTiming;
     ppSiteCoverage: Result := dtdoSiteCoverage;
  else
    Raise Exception.Create('Unable to recognize type of Analysis.  Please check that your Analysis Preferences has not been corrupted');
  end;
end;


procedure TProcessPack.PerformAnalysis;
var
  PrecisionOfResult: Integer;
begin
  {$IFNDEF VISUAL_BUILD}
  HandleTimeLimit; { it is a MEGA-CC thing}
  CheckBLensCalc; { it is a parsimony thing}
  {$ENDIF}
  //{$IFDEF DEBUG}
  //DevActionInfoToFile;
  //{$ENDIF}
  Case ProcessTypes[0] of
    //Normal Analysis Preferences
    ppInfer:
    Case ProcessTypes[1] of
      ppNJ:
      begin
        if UseSequenceData then ProcessSeqDistPhylogenyCommand(dtdoNJTree, Self)
        else if UseDistanceData then ProcessDistDistPhylogenyCommand(dtdoNJTree, Self);
      end;
      ppME:
      begin
        if UseSequenceData then ProcessSeqDistPhylogenyCommand(dtdoMETree, Self)
        else if UseDistanceData then ProcessDistDistPhylogenyCommand(dtdoMETree, Self);
      end;
      ppUPGMA:
      begin
        if UseSequenceData then ProcessSeqDistPhylogenyCommand(dtdoUPGMATree, Self)
        else if UseDistanceData then ProcessDistDistPhylogenyCommand(dtdoUPGMATree, Self);
      end;
      ppMP:
        begin
            if UseSequenceData then
              ProcessSeqParsimPhylogenyCommand(dtdoMPTree, Self);
        end;
      ppML: ProcessSeqMLPhylogenyCommand(dtdoMLTree, Self);
      //save preferences to ini or json file
      ppMLIQTree:
      begin
      end
      else
        Raise Exception.Create('Unable to determine what Statistical Method to use to perform a ' + GetEnumName(TypeInfo(TProcessType),
                  integer(ProcessTypes[1])));
    end;
    ppSiteCoverage: if UseSequenceData then ComputeSiteCoverage(Self) else Needs('Sequence');
    ppZtest: if HasCodonData then ProcessDistanceCommand(dtdoSelectionZTest, Self)  else Needs('Coding');
    ppFisherExact:  if HasCodonData then ProcessDistanceCommand(dtdoSelectionExactTest, Self) else Needs('Coding');
    ppTajimaNeutrality:  if UseSequenceData then ProcessTajimaNeutralityTestCommand(Self) else Needs('Sequence');
    ppEstimateSelectionCodons: if HasCodonData then ProcessCodonOmegaCommand(dtdoMLCodonOmega, 'None', Self) else Needs('Coding');
    ppTajimaRelativeRate:      if UseSequenceData then ProcessTajimaClockCommand(Self) else Needs('Sequence');
    ppMLClock:  if UseSequenceData then   ProcessSeqMLPhylogenyCommand(dtdoMLClockTest, Self) else Needs('Sequence');
    ppMLClockLocal:  if UseSequenceData then   ProcessSeqMLPhylogenyCommand(dtdoMLClockTestLocal, Self) else Needs('Sequence');
    ppRelTimeML: if UseSequenceData then  ProcessSeqMLRelTimeCCCommand(dtdoRelTimeML, Self) else Needs('Sequence');
    ppRtdtML: if UseSequenceData then ProcessSeqMLReltimeCCCommand(dtdoRtdtML, Self) else Needs('Sequence');
    ppCorrTestBlens: ProcessPhylogenyBLensCommand(dtdoCorrTestBlens, Self);
    ppCorrTestML: if UseSequenceData then ProcessSeqMLRelTimeCCCommand(dtdoCorrTestML, Self) else Needs('Sequence');
    ppRelTimeBLens: ProcessPhylogenyBLensCommand(dtdoRelTimeBLens, Self);
    ppRtdtBLens: ProcessPhylogenyBLensCommand(dtdoRtdtBLens, Self);
    ppRelTimeLS: if UseSequenceData then
                   ProcessSeqRelTimeCCCommand(dtdoRelTimeLS, Self)
                 else if UseDistanceData then
                   ProcessDistDistPhylogenyCommand(dtdoRelTimeLS, Self)
                 else Needs('Sequence');
    ppRtdtLS: if UseSequenceData then
                   ProcessSeqRelTimeCCCommand(dtdoRtdtLS, Self)
                 else if UseDistanceData then
                   ProcessDistDistPhylogenyCommand(dtdoRtdtLS, Self)
                 else Needs('Sequence');
    ppAncSeq:
    case ProcessTypes[1] of
      ppML: if UseSequenceData then
            begin
              if ProcessTypes[2] = ppPredictLivingSeq then
                ProcessSeqMLPhylogenyCommand(dtdoMLPredictLivingSeq, Self)
              else if ProcessTypes[2] = ppEpML then
                ProcessSeqMLPhylogenyCommand(dtdoEpML, Self)
              else
              begin
                if HasProcessType(ppMyPegAncestInfer) then
                  ProcessSeqMLPhylogenyCommand(dtdoMLInferAncSeqMyPeg, Self)
                else
                  ProcessSeqMLPhylogenyCommand(dtdoMLInferAncSeq, Self);
              end;
            end
            else Needs('Sequence');
      ppMP: if UseSequenceData then
            begin
              if ProcessTypes[2] = ppPredictLivingSeq then
                ProcessSeqParsimPhylogenyCommand(dtdoMPPredictLivingSeq, Self)
              else
              begin
                if HasProcessType(ppMyPegAncestInfer) then
                  ProcessSeqParsimPhylogenyCommand(dtdoMPInferAncSeqMyPeg, Self)
                else
                  ProcessSeqParsimPhylogenyCommand(dtdoMPInferAncSeq, Self);
              end;
            end
            else Needs('Sequence');
      else Raise Exception.Create('Unable to determine what Statistical Method to use to perform an ' + GetEnumName(TypeInfo(TProcessType),
                  integer(ProcessTypes[1])));
    end;
    ppModelSelIQTree:        if UseSequenceData then   ProcessSeqMLPhylogenyCommand(dtdoMLModelTestIQTree, Self) else Needs('Sequence');
    ppModelSelML:            if UseSequenceData then   ProcessSeqMLPhylogenyCommand(dtdoMLModelTest, Self) else Needs('Sequence');
    ppModelTamerML:            if UseSequenceData then   ProcessSeqMLPhylogenyCommand(dtdoMLModelTamer, Self) else Needs('Sequence');
    ppSubPatternEstML:       if UseSequenceData then   ProcessSeqMLPhylogenyCommand(dtdoMLComputePattern, Self) else Needs('Sequence');
    ppTsTvML:  if HasNucleotideData then ProcessSeqMLPhylogenyCommand(dtdoMLTsTvBias, Self) else Needs('Nucleotide');
    ppEstRateVarAmongSitesML:   if HasNucleotideData OR HasAminoAcidData then ProcessSeqMLPhylogenyCommand(dtdoMLGammRates, Self);
    ppSubPatternEstMCL:  if HasNucleotideData then ProcessMCLCommand(dtdoMCLComputePattern, Self) else Needs('Nucleotide');
    ppTsTvMCL:    if HasNucleotideData then ProcessMCLCommand(dtdoMCLTsTvBias, Self) else Needs('Nucleotide');
    ppEstRateBySite: if UseSequenceData then ProcessSeqMLPhylogenyCommand(dtdoMLInferSiteBySiteRates, Self) else Needs('Sequence');
    ppDistTestPatternHomo: if UseSequenceData then ProcessDisparityIndexTestCommand(dtdoDisparityIndexTest, Self) else Needs('Sequence');
    ppEstCompDist:  if UseSequenceData then ProcessDisparityIndexTestCommand(dtdoCompositionDistance, Self) else Needs('Sequence');
    ppEstDispIndex: if UseSequenceData then ProcessDisparityIndexTestCommand(dtdoDisparityIndex, Self) else Needs('Sequence');
    ppDistEst:
      Case ProcessTypes[1] of
        ppPairTaxa:  if UseSequenceData then ProcessDistanceCommand(dtdoPairwiseDist, Self) else Needs('Sequence');
        ppWithinGpAvg:
        begin
          if UseSequenceData then  ProcessDistanceCommand(dtdoWithinGroupMean, Self)
          else if UseDistanceData then
          {$IFDEF VISUAL_BUILD}
          AvgGpsCalculation(gdWithinGroupMean)
          {$ELSE}
          D_InputDistData.GetAvgGps(gdWithinGroupMean)
          {$ENDIF}
        end;
        ppBetweenGpAvg:
        begin
          if UseSequenceData then  ProcessDistanceCommand(dtdoBetweenGroupMean, Self)
          else if UseDistanceData then
          {$IFDEF VISUAL_BUILD}
          AvgGpsCalculation(gdBetweenGroupMean)
          {$ELSE}
          D_InputDistData.GetAvgGps(gdBetweenGroupMean)
          {$ENDIF}
        end;
        ppNetBetweenGpAvg:
        begin
          if UseSequenceData then   ProcessDistanceCommand(dtdoNetGroupMean, Self)
          else if UseDistanceData then
          {$IFDEF VISUAL_BUILD}
            AvgGpsCalculation(gdNetGroupMean)
            {$ELSE}
            D_InputDistData.GetAvgGps(gdNetGroupMean)
           {$ENDIF}
        end;
        ppOverallMean:
        begin
            if UseSequenceData then
              ProcessDistanceCommand(dtdoOverallMean, self)
            else if UseDistanceData then
            begin
              PrecisionOfResult := 10;
              MessageDlg('The overall average is '+ FloatToStrF(D_InputDistData.GetAvgAll,ffFixed,PrecisionOfResult+4,PrecisionOfResult),mtInformation, [mbOK], 0);
            end;
        end;
        ppAvgInSubPops: if UseSequenceData then ProcessDistanceCommand(dtdoAvgDiversityWithinSubPops, Self) else Needs('Sequence');
        ppAvgOverallPops: if UseSequenceData then ProcessDistanceCommand(dtdoAvgDiversityForEntirePop, Self) else Needs('Sequence');
        ppInterPopDiversity:  if UseSequenceData then    ProcessDistanceCommand(dtdoInterPopDiversity, Self) else Needs('Sequence');
        ppPorportionInterDiversity: if UseSequenceData then  ProcessDistanceCommand(dtdoPropOfInterPopDiversity, Self) else Needs('Sequence');
        else  Raise Exception.Create('Unable to determine what Scope to use to perform a ' + GetEnumName(TypeInfo(TProcessType),
                  integer(ProcessTypes[1])));
      end;
      ppAnalyzeUserTree:
        Case ProcessTypes[1]  of
          ppLeastSquares:
          begin
            if      UseSequenceData then    ProcessSeqDistPhylogenyCommand(dtdoOLSComputeUserTreeBLens, Self)
            else if UseDistanceData then    ProcessDistDistPhylogenyCommand(dtdoOLSComputeUserTreeBLens, Self);
          end;
          ppLbsAnalyzeTree:
            if UseSequenceData then
              ProcessSeqMLPhylogenyCommand(dtdoLbsAnalyzeTree, Self)
            else
              Needs('Sequence');
          ppML:
            if UseSequenceData then
              ProcessSeqMLPhylogenyCommand(dtdoMLComputeUserTreeBLens, Self)
            else Needs('Sequence');
          ppMLIQTree :
            if UseSequenceData then
            begin
            end
            else Needs('Sequence');
          ppMP:
            begin
              AddProcessType(ppCalcBlens);
              ProcessSeqParsimPhylogenyCommand(dtdoMPComputeUserTreeBLens, Self);
            end;
          else
          begin
            if ProcessTypes[2] = ppPhyloQ then
            begin
              IsPhyloQAnalysis := True;
              if UseSequenceData then
              begin
                ProcessPhyloQSeqCommand(Self);
              end
              else
                Raise Exception.Create('PhyloQ analysis requires sequence data.');
              end
            else
            Raise Exception.Create('Unable to determine which Statistical Method to use to ' + GetEnumName(TypeInfo(TProcessType), integer(ProcessTypes[1])));
          end;
        end;
     ppOLSInteriorBranchTest: if UseSequenceData then   ProcessSeqDistPhylogenyCommand(dtdoOLSInteriorBranchTest, Self) else Needs('Sequence');
     ppAlign:
       begin
         AlignSequences(self);
       end;
  else
    Raise Exception.Create('Unable to recognize type of Analysis. Please report this bug at https://www.megasoftware.net');
  end;
end;

function TProcessPack.Count: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(ProcessTypes) do
    if ProcessTypes[i] = ppNone then
    begin
      result := i;
      exit;
    end;
end;

function TProcessPack.UseSequenceData: Boolean;
begin
  Result := D_InputSeqData <> nil;
end;

function TProcessPack.UseDistanceData: Boolean;
begin
  Result := D_InputDistData <> nil;
end;

function TProcessPack.DataTypeString: String;
begin
  Result := EmptyStr;
  if HasAminoAcidData then
    Result := 'protein'
  else if HasCodonData then
    Result := 'coding'
  else if HasNucleotideData then
    Result := 'nucleotide'
  else if UseDistanceData then
    Result := 'distances';
end;

function TProcessPack.HasMultiFileData: Boolean;
begin
  Result := False;
end;

function TProcessPack.HasNucleotideData: Boolean;
begin
  Result := D_InputSeqData <> nil;
  if Result then
    Result := D_InputSeqData.IsNuc;
end;

procedure TProcessPack.SetIsWorkflowPack(AValue: Boolean);
begin
  if FIsWorkflowPack = AValue then Exit;
  FIsWorkflowPack := AValue;
end;

function TProcessPack.HasProcessType(AType: TProcessType): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to AvailableIndex - 1 do
  begin
    if ProcessTypes[i] = AType then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TProcessPack.IsGettingWorkflowSettings: Boolean;
begin
  Result := IsWorkflowPack and IsPrototyper;
end;

{$IFNDEF VISUAL_BUILD}
function TProcessPack.HasSetting(settingName: String): Boolean;
begin
  Result := TextualSettingsList.IndexOfName(settingName) >= 0;
end;

function TProcessPack.SettingValueIs(settingName: String; settingValue: String): Boolean;
var
  aValue: String = '';
begin
  aValue := LowerCase(settingValue);
  Result := HasSetting(settingName) and (LowerCase(TextualSettingsList.Values[settingName]) = aValue);
end;

function TProcessPack.SettingIsApplicable(settingName: String): Boolean;
begin
  Result := False;
  if HasSetting(settingName) and (not SettingValueIs(settingName, NotApplicableStr)) then
    Result := True;
end;

function TProcessPack.SettingString(settingName: String): String;
begin
  if HasSetting(settingName) then
    Result := TextualSettingsList.Values[settingName]
  else
    Result := EmptyStr;
end;

{$ENDIF}

function TProcessPack.HasCodonData: Boolean;
begin
  Result := D_InputSeqData <> nil;
  if Result then
     Result := D_InputSeqData.IsNuc and D_InputSeqData.IsCoding;
end;

function TProcessPack.HasAminoAcidData: Boolean;
begin
  Result := D_InputSeqData <> nil;
  if Result then
    Result := D_InputSeqData.IsAmino or HasCodonData;
end;

//////////////////////////////////////////////////////////////
//////Exporting to Saved MEGA Analysis from here on in////////
//////////////////////////////////////////////////////////////

// Saved in an INI style format
procedure TProcessPack.SaveToFile(FileName : String);
var
  ToSave : TStringList;
begin
  ToSave := toStringList;
  ToSave.SaveToFile(FileName);
  ToSave.Free;

end;

function TProcessPack.toStringList(): TStringList;
var
  AmtAnalysisSettings, AmtProcessTypes, i: integer;
  ToSave : TStringList;
  MyDataType: TSnTokenCode;
begin
  ToSave := TStringList.Create;

  //HEADER
  ToSave.Add('[ MEGAinfo ]');
  ToSave.Add('ver=' + IntToStr(VER_SAVED_MEGA_ANALYIS));

  ToSave.Add('[ DataSettings ]');
  {$IFDEF VISUAL_BUILD}
  MyDataType := MegaForm.DataType;
  {$ELSE}
  MyDataType := D_MegaMain.FDataType;
  {$ENDIF}
  ToSave.Add(DataTypeStr + '=' + GetEnumName(TypeInfo(TSnTokenCode), Integer(MyDataType)));
  if MyDataType = snNucleotide then
    ToSave.Add(ContainsCodingNucStr + '=' + BoolToStr(containsCodingNuc, true));
  ToSave.Add(MissingBaseSymbolStr + '=' + DefaultMissingSymbol);
  ToSave.Add(IdenticalBaseSymbolStr + '=' + DefaultIdenticalSymbol);
  ToSave.Add(GapSymbolStr + '=' + DefaultGapSymbol);
  //PROCESS TYPES
  AmtProcessTypes := Count;
  ToSave.Add('[ ProcessTypes ]');
  for i := 0 to AmtProcessTypes-1 do
    ToSave.Add(GetEnumName(TypeInfo(TProcessType), integer(ProcessTypes[i])) + '=true'); //GS - added this so that we create a properly formed INI file. This allows us to use TIniFile for parsing the analysis preferences file

  //ANALYSIS SETTINGS
  ToSave.Add('[ AnalysisSettings ]');
  AmtAnalysisSettings := TextualSettingsList.Count;
  for i := 0 to AmtAnalysisSettings-1 do
  begin
    if (Trim(TextualSettingsList.Names[i]) <> EmptyStr) and (Trim(TextualSettingsList.Names[i]) <> 'Alignment Info') then
      ToSave.Add(TextualSettingsList.Names[i] + '=' + TextualSettingsList.Values[TextualSettingsList.Names[i]]);
  end;
  (* CRC DISABLED
  ToSave.Insert(2, 'CRC=' + Copy(GetCRC32(ToSave), 2, 9999)); //Get the CRC, remove leading '$' and put into third line
  *)
  result := ToSave;
end;

destructor TProcessPack.Destroy;
begin
  if Assigned(TextualSettingsList) then
    FreeAndNil(TextualSettingsList);
  if Assigned(OnFreeNotify) then
    OnFreeNotify(Self);
  inherited;
end;

//The Key/Value pair may either be sent as
//One string, with Key and Value i.e. "AnalysisType=Phylogenetic Inference"
procedure TProcessPack.AddKeyValuePairSetting(KeyValue: AnsiString);
begin
  TextualSettingsList.Values[Copy(KeyValue, 0, Pos('=', KeyValue)-1)] := Copy(KeyValue, pos('=', KeyValue)+1, 999999);
end;

//The Key/Value pair may either be sent as
//Two strings, Key and Value i.e. "AnalysisType"  "Phylogenetic Inference"
procedure TProcessPack.AddKeyValuePairSetting(key: AnsiString; value: AnsiString);
begin
  TextualSettingsList.Values[key] := value;
end;

///////////////////////////////////////
//////Calculate CRC///////////////////
/////////////////////////////////////

// CRC means Cyclic Redudency Check
procedure BuildCRCTable;
var
  i, j: Word;
  r: Longint;
begin
  FillChar(CRCTable, SizeOf(CRCTable), 0);
  for i := 0 to 255 do
  begin
    r := i shl 1;
    for j := 8 downto 0 do
      if (r and 1) <> 0 then
        r := (r Shr 1) xor CRCPOLY
      else
        r := r shr 1;
    CRCTable[i] := r;
   end;
end;



function RecountCRC(b: byte; CrcOld: Longint): Longint;
begin
  RecountCRC := CRCTable[byte(CrcOld xor Longint(b))] xor ((CrcOld shr 8) and $00FFFFFF)
end;

function HextW(w: Word): AnsiString;
const
  h: array[0..15] Of AnsiChar = '0123456789ABCDEF';
begin
  HextW := '';
  HextW := h[Hi(w) shr 4] + h[Hi(w) and $F] + h[Lo(w) shr 4]+h[Lo(w) and $F];
end;

function HextL(l: Longint): AnsiString;
begin
  with Long(l) do
    HextL := HextW(HiWord) + HextW(LoWord);
end;

function TProcessPack.GetCalibrationFile: String;
begin
  Result := EmptyStr;
  if TextualSettingsList.IndexOfName(opsCalibrationFile2) <> -1 then
    Result := TextualSettingsList.Values[opsCalibrationFile2];
end;

function TProcessPack.GetClockLevel: TClockLevel;
var
  RowName: AnsiString;
begin
  Result := clUnknown;
  RowName := EmptyStr;

  if TextualSettingsList.IndexOfName(opsClockLevel2) <> -1 then
    RowName := opsClockLevel2
  else if TextualSettingsList.IndexOfName(opsGlobalClockLevel2) <> -1 then
    RowName := opsGlobalClockLevel2;

  if RowName <> EmptyStr then
  begin
    if TextualSettingsList.Values[rowName] = AllClocksStr then
      Result := clNoStdErr
    else if TextualSettingsList.Values[rowName] = ManyClocksStr then   // high stringency clock
      Result := clOneStdErr
    else if TextualSettingsList.Values[rowName] = FewClocksStr then   // low stringency clock
      Result := clTwoStdErr
    else if TextualSettingsList.Values[rowName] = FewestClocksStr then
      Result := clThreeStdErr
    else
      Result := clUnknown;
  end;
end;

function TProcessPack.GetClockType: TClockType;
begin
  Result := ctUnknown;

  if TextualSettingsList.IndexOfName(opsClockType2) <> -1 then
  begin
    if TextualSettingsList.Values[opsClockType2] = GlobalClockStr then
      Result := ctGlobal
    else if TextualSettingsList.Values[opsClockType2] = '' then
      Result := ctLocal
    else
      Result := ctUnknown;
  end;
end;

function TProcessPack.GetBootVarReps: Integer;
var
  TempStr: AnsiString;
begin
  TempStr := TextualSettingsList.Values[opsBootReps2];
  if TempStr = NotApplicableStr then
    Result := 0
  else
    Result := StrToInt(TempStr);
end;

function TProcessPack.GetRateMergeOption: Boolean;
var
  ChosenOption: String;
begin
  Result := False;
  Exit; { for now we are disabling the rate merging option as it is not fruitful. Sudhir wants to keep the possibility of adding it back in later though}
  if TextualSettingsList.IndexOfName(opsClockLevel2) <> -1 then
  begin
    ChosenOption := TextualSettingsList.Values[opsClockLevel2];
    if ChosenOption = AllClocksStr then
      Result := False
    else
      Result := True;
  end
  else
    Assert(False);
end;

function TProcessPack.MaxRateRatio: Extended;
var
  tempStr: String;
  value: Extended;
begin
  if TextualSettingsList.IndexOfName(opsMaxRateRatio2) >= 0 then
  begin
    tempStr := TextualSettingsList.Values[opsMaxRateRatio2];
    if TryStrToFloat(tempStr, value) then
      Result := value
    else
      raise Exception.Create('Invalid value for ' + opsMaxRateRatio2 + '. A numeric value is required');
  end
  else
    Result := DEFAULT_MAX_RATE_RATIO; { for backwards compatibility}
end;

function TProcessPack.MLSearchFilter: Extended;
var
  filterSetting: AnsiString;
begin
  Result := -1;
  if TextualSettingsList.IndexOfName(opsSearchFilter2) <> -1 then
  begin
    filterSetting := TextualSettingsList.Values[opsSearchFilter2];
    if filterSetting <> EmptyStr then
    begin
      if filterSetting = NoSearchFilterStr then
        result := 1.0 + FP_CUTOFF
//      else if filterSetting = VeryStrongStr then
//        result := 0.5
      else if filterSetting = StrongStr then
        result := 0.5
      else if filterSetting = ModerateStr then
        result := 0.7
      else if filterSetting = WeakStr then
        result := 0.9
//      else if filterSetting = VeryWeakStr then
//        result := 0.9
      else
        result := -1;
    end;
  end;
end;

function TProcessPack.ClockLevel: Integer;
var
  rowName: AnsiString;
begin
  rowName := EmptyStr;

  // find which row is included (if any)
  if TextualSettingsList.IndexOfName(opsClockType2) <> -1 then   // Only 1 of these row types will ever be there at any one time.
    rowName := opsClockType2
  else if TextualSettingsList.IndexOfName(opsClockLevel2) <> -1 then
    rowName := opsClockLevel2
  else if TextualSettingsList.IndexOfName(opsGlobalClockLevel2) <> -1 then
    rowName := opsGlobalClockLevel2;

  if rowName <> EmptyStr then
  begin
    if (TextualSettingsList.Values[rowName] = GlobalClockStr) or
       (TextualSettingsList.Values[rowName] = AllClocksStr) then   // Global clock
      result := 0
    else if TextualSettingsList.Values[rowName] = ManyClocksStr then   // high stringency clock
      result := 1
    else if TextualSettingsList.Values[rowName] = FewClocksStr then   // low stringency clock
      result := 2
    else if TextualSettingsList.Values[rowName] = FewestClocksStr then
      result := 3
    else
      result := -1;
  end
  else
    result := -1;
end;

/// <summary>Test if the given TProcessType is contained in this TProcessPack</summary>
/// <param name="MyProcessType" type="TProcessType"></param>
/// <returns>true if the given TProcessType is contained in this TProcessPack, false otherwise</returns>
function TProcessPack.ContainsProcessType(MyProcessType: TProcessType): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(ProcessTypes) - 1 do
  begin
    if MyProcessType = ProcessTypes[i] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

/// <summary>Return a TStringList that represents the current state of this TProcessPack.</summary>
/// <param name="Comment">Optional comment that will be added as a string to the returned TStringList</param>
/// <returns>A TStringList where every string is a key/value pair of field=value, for all fields comprising
/// this TProcessPack</returns>
function TProcessPack.StateToStringList(Comment: String=''): TStringList;
var
  MyStringList: TStringList;
  i: Integer;
  TypeString: String;
begin
  MyStringList := TStringList.Create;
  MyStringList.Add('TProcessPack=MyName');
  if Comment <> EmptyStr then
    MyStringList.Add('Comment=' + Comment);

  for i := 0 to AvailableIndex - 1 do
  begin
    TypeString := GetEnumName(TypeInfo(TProcessType), integer(ProcessTypes[i]));
    MyStringList.Add('ProcessType[' + IntToStr(i) + ']=' + TypeString);
  end;

  MyStringList.Add('ProcessImageIndex=' + IntToStr(ProcessImageIndex));
  MyStringList.Add('ProcessParentImageIndex=' + IntToStr(ProcessParentImageIndex));
  MyStringList.Add('CommonName=' + CommonName);

  Result := MyStringList;
end;


end.
