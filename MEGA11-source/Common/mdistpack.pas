{
	Copyright 1992-2021 Sudhir Kumar and Koichiro Tamura

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

unit mdistpack;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

Uses
  MegaConsts, Classes, TypInfo;

{$M+}
type
  TDistType = (gdNone,
    // Basic analysis types
    gdOneNuc,  // nuc-by-nuc
    gdThreeNuc, // three nucs at a time
    gdAmino,    // amino acid sequences or translated analysis
    gdSynNonSyn,  // level for estimating syn/nonsyn distances

    // Dist Models
    gdNeiGojobori, gdIna, gdLWL, gdPBL, gdComeronKumar,  // Basic models of Ds-Dn
    gdGamma, gdInvar, gdHetero,     // additional model or substitution parameters
    // Dist Correction; used intermixed for above models
    gdNoOfDiff, gdPropDist,  // for nuc and amino

    gdJukesCantor, gdTajimaNei, gdFelsenstein81, gdKimura2para, gdTamura,
    gdHKY, gdREV,
    gdTamuraNei, gdLogDet, gdMCL, gdGlobalPara, // for nuc
    gdEmbryoWhole, gdEmbryoExpression, gdEmbryoWtd, // expression distance

    // Aminoacid
    gdModel,
    gdPoisson, // for amino
    gdEqualInput,                       // amino
    gdJones, gdJonesPi, gdDayhoff, gdDayhoffPi,  // amino
    gdMtRev24, gdMtRev24Pi,
    gdCpRev, gdCpRevPi,
    gdRtRev, gdRtRevPi,
    gdWAG, gdWAGPi, 
    gdLG, gdLGPi, // Mt/Cp/rt/WAG/LG
    gdModelAAFreq, gdDataAAFreq,

    // Tree assisted calculation
    gdNJ, gdUserSpecified,

    // Dist Components
    gdAll,
    gdNucTsOnly, gdNucTvOnly, gdNucRatioTsTv,
    gdNucSeparateTsTv,
    gdCommonSites, gdTestRandom,
    gdSynOnly, gdNonsynOnly,
    gdRatioSynNonsyn, gdRatioNonsynSyn,
    gdDiffSynNonsyn,  gdDiffNonsynSyn,
    gdNoOfSynSites,   gdNoOfNonsynSites,
    gdNonsyn0Fold,    gdSyn4Fold,
    gdNoOf0FoldSites, gdNoOf4FoldSites,
    // for allele frequency data
    gdAlleleFreqDist,
    gdBhattacharyaDist,
    //gdCzekanowskiDist,
    gdManhattanDist, gdProvostiDist, //nice because of triangle inequality
    gdRogersDist,  // this is basically the square root of DC where frequencies are used
    gdNei1983Dist, gdKumarGadagkarDist, gdKumarGadagkarDisparityDist,
    gdChordDist, gdFstDist, gdNei1972Dist,
    gdDeltaMuSqrDist,
    // Computation Type
    gdPairwise, gdOverallMean, gdUpdate,
    gdWithinGroupMean, gdBetweenGroupMean, gdNetGroupMean,
    gdAvgDiversityWithinSubPops,
    gdAvgDiversityForEntirePop,
    gdInterPopDiversity,
    gdPropOfInterPopDiversity,

    // variances
    gdAnalyticalVar, gdBootstrapVar, gdJacknifeVar, gdMonteCarloTest,

    // tests for selection and other purposes
    gdExactTest,        // Exact test for positive selection
    gdZSelectionTest,
    gdMcdonaldKreitmanTest,
    gdNeutralEvolTest,  // s <> n
    gdPosSelTest,       // n > s
    gdPurifySelTest,    // n < s
    gdChiSqrTajimaClockTest, // this is needed for some stuff
    gdBinomialSelectionTest, gdChiSqrSelectionTest,
    gdTajimaNeutralityTest,
    gdTajimaClockTest,

    gdDisparityIndexTest,
    gdCompositionDistance,
    gdDisparityIndex,

    // MCL stuff
    gdMCLComputePattern, // For 4x4 calculation
    gdMCLTsTvBias       // For R calculation
  );

  // for computing gamma para
  TOptGammaPara  = set of (ogpMoments,     ogpYangKumar,
                           ogpOutputHisto, ogpOutputPerSite,
                           ogpOutputMean,  ogpOutputVar);

  TOptSubstParas = set of (ospFt, ospPt, ospQt, ospRates, ospR);

  { WARNING: when adding types here, add them to the end, otherwise session file
    reading gets broken for session files made with older versions because the
    ordering changes. Also TAnalysisInfo.ReadFromFile has to be updated where we
    read the MyUsrOperation variable because the size of it may change when
    adding/removing types here.}
  TDistTreeDlgOption = (
                        dtdoPairwiseDist,  // allows boot/analytical var
                        dtdoOverallMean,
                        dtdoWithinGroupMean,  // allows only boot var
                        dtdoBetweenGroupMean,
                        dtdoNetGroupMean,
                        dtdoMetagenomicDist, // Metagenomics

                        dtdoAvgDiversityWithinSubPops,
                        dtdoAvgDiversityForEntirePop,
                        dtdoInterPopDiversity,
                        dtdoPropOfInterPopDiversity,

                        dtdoSelectionExactTest,
                        dtdoSelectionZGpTest,
                        dtdoSelectionZTest,
                        dtdoTajimaNeutralityTest,

                        dtdoTajimaClockTest,
                        dtdoMLClockTest,
                        dtdoMLClockTestLocal,

                        dtdoMLModelTest,

                        dtdoCompositionDistance,
                        dtdoDisparityIndex,
                        dtdoDisparityIndexTest, // for disparity index

                        dtdoMCLComputePattern, // For 4x4 calculation
                        dtdoMCLTsTvBias,       // For R calculation

                        dtdoNJTree,
                        dtdoMETree,
                        dtdoMPTree,
                        dtdoUPGMATree,
                        dtdoMLTree,
                        dtdoOLSInteriorBranchTest,
                        

                        dtdoMPInferAncSeq,
                        
                        dtdoMLInferAncSeq,
                        

                        dtdoMPPredictLivingSeq,
                        dtdoMLPredictLivingSeq,

                        dtdoMLComputePattern, // For 4x4, R, gamma, inv, calculations
                        dtdoMLTsTvBias,
                        dtdoMLGammRates,
                        dtdoMLInferSiteBySiteRates,
                        dtdoMLCodonOmega,

                        dtdoMPComputeUserTreeBLens,
                        dtdoOLSComputeUserTreeBLens,
                        dtdoMLComputeUserTreeBLens, //for branch lengths, patterns, and everything
                        dtdoRelTimeML,

                        dtdoRelTimeBLens,
                        dtdoRelTimeLS,
			dtdoPhyloQ,
                        dtdoOLSComputeUserTreeBLensMetagenomics,
                        dtdoAlign,
			dtdoMPInferAncSeqMyPeg,
			dtdoMLInferAncSeqMyPeg,
                        dtdoUnknown,
                        dtdoGeneDupInference,
                        dtdoCalTest, dtdoFindClones, dtdoBuildTumorTree,
                        dtdoCorrTestBlens,
                        dtdoCorrTestML, dtdoEpML,dtdoMLIQTree,
                        dtdoRtdtML, dtdoRtdtLS, dtdoRtdtBlens,
                        dtdoBEAM, dtdoSiteCoverage, dtdoMLModelTestIQTree
                        );

  TDistPack = class
  private
    DistType: array[0..200] of TDistType;
    FGammaCats: Integer;
    FGammaPara    : Double;
    FInaRValue    : Double;
    FGeneticCode  : AnsiString;

    FBootReps: Integer;
    FRandSeed: Integer;

    Where: Integer;  // current index in get routines

    function GetAcronym: AnsiString;
    function GetName: AnsiString;
    function GetNameWithoutDistComponent: AnsiString;
    function GetSimpleName: AnsiString;
    function GetMethodCitation : AnsiString;
    function GetPatternAssumption : AnsiString;
    function GetFullDescription: AnsiString;
    function GetIsValid: Boolean;

    function GetGeneticCode: AnsiString;
    function GetSelectionTestType: TDistType;
    function GetComputationType: TDistType;
    function GetDistModel: TDistType;
    function GetDistCorrection: TDistType;
    function GetDistComponent: TDistType;
    function GetVarType: TDistType;
    function GetBasicAnalysisType : TDistType;
  public
    constructor Create;

    procedure Clear;  // clears up the whole structure
    procedure Unwind; // go back up to index 0;
    function  AddType(Value: TDistType): Boolean;  // adds a new structure info
    function  AddTypeByStr(TheValue: AnsiString; IsCoding: Boolean): Boolean;

    function  GetNext: TDistType;  // returns next type of info
    function  DoesContain(Value: TDistType): Boolean; // if contained

    function  Replace(FromValue, toValue : TDistType): Boolean; // if contained the replaced, else added

    function  HasDifference: Boolean;

    // all of these add items
    procedure AddOperationTypeAndScope(FOperation: TDistTreeDlgOption; const AStringList: TStrings; IsCoding: Boolean);  // converts dtdo to gd appropriately
    procedure AddPrimaryModelType(const AStringList: TStrings; IsCoding: Boolean);  // adds model type
    procedure AddExtendedModelInfo(const AStringList: TStrings);
    procedure AddDistAnalysisPrefs(const AStringList: TStrings);
    procedure ConstructPack(FOperation: TDistTreeDlgOption; const AStringList: TStrings;IsCoding: Boolean);

    //==
    function CanCorrectForPatternHeterogeneity: Boolean;
    function HasOnlyPatternHeterogeneity      : Boolean;
    function CanCorrectForGammaRates          : Boolean;
    function CanCorrectForGammaRatesPlusInv(AOperation: TDistTreeDlgOption): Boolean;
    function CanCorrectForOnlyGammaRates      : Boolean;
    function CanComputeAnalyticalVar          : Boolean;
    function CanComputeVar                    : Boolean;
    function CanSeparateTsTv                  : Boolean;
    function IsNeiGojoboriType                : Boolean;
    function NeedsFixedRValue                 : Boolean;

    function GetIsComputeGamma : Boolean;  // gdGamma with ttML
    function GetIsComputeInvar : Boolean;  // gdGammaInv or gdInvar with ttML
    function GetNoOfGammaCats  : Integer;  // returns the number of gamma categories

    function ReadFromFile(var data: File):boolean;
    procedure WriteToFile(var data: File);
    procedure Assign(Source: TDistPack);
    function StateToStringList(Comment: String=''): TStringList;
  published
    property IsValid: Boolean read GetIsValid;

    property Acronym: AnsiString read GetAcronym;
    property Name: AnsiString read GetName;
    property NameWithoutDistComponent: AnsiString read GetNameWithoutDistComponent;
    property SimpleName : AnsiString read GetSimpleName;
    property FullDescription: AnsiString read GetFullDescription;
    property MethodCitation : AnsiString read GetMethodCitation;
    property PatternAssumption : AnsiString read GetPatternAssumption;

    property BootReps: Integer  read FBootReps write FBootReps;
    property RandSeed: Integer  read FRandSeed write FRandSeed;

    property ComputeGammaParameter: boolean read GetIsComputeGamma;                  // for ML
    property ComputeInvarParameter: boolean read GetIsComputeInvar;                  // for ML
    property NoOfGammaCategories: integer   read GetNoOfGammaCats;                  // for ML

    property GammaParameter: Double read FGammaPara write FGammaPara;

    property InaRValue:      Double read FInaRValue write FInaRValue;

    property GeneticCode:     AnsiString    read GetGeneticCode;
    property ComputationType: TDistType read GetComputationType;
    property DistModel:       TDistType read GetDistModel;
    property DistCorrection:  TDistType read GetDistCorrection;
    property DistComponent:   TDistType read GetDistComponent;
    property VarType:         TDistType read GetVarType;
    property SelectionTestType: TDistType read GetSelectionTestType;
    property BasicAnalysisType: TDistType read GetBasicAnalysisType;
  end;

  function DoesOperationRequireGroups(AOperation: TDistTreeDlgOption): Boolean;
  function MinNoOfTaxaNeededForAnalysis(AOperation: TDistTreeDlgOption): Integer;

implementation

uses
  Sysutils, MegaUtils,
  MegaAnalysisPrefStrings, {$IFDEF VISUAL_BUILD}Mega_Main,{$ELSE} MD_MegaMain, MegaUtils_NV,{$ENDIF} Dialogs;

//===
function DoesOperationRequireGroups(AOperation: TDistTreeDlgOption): Boolean;
begin
  Result := False;
  case AOperation of
    dtdoWithinGroupMean,
    dtdoBetweenGroupMean,
    dtdoNetGroupMean,
    dtdoAvgDiversityWithinSubPops,
    dtdoAvgDiversityForEntirePop,
    dtdoInterPopDiversity,
    dtdoPropOfInterPopDiversity,
    dtdoSelectionZGpTest:
       Result := True;
  end;
end;

function MinNoOfTaxaNeededForAnalysis(AOperation: TDistTreeDlgOption): Integer;
begin
  Result := 2;   // for all pairwise distances and related measures
  case AOperation of
    dtdoTajimaNeutralityTest,
    dtdoMLCodonOmega,
    dtdoTajimaClockTest,
    dtdoMLClockTest,
    dtdoMLClockTestLocal,
    dtdoRelTimeML, dtdoRtdtML,
    dtdoCorrTestML,
    dtdoEpML,
    dtdoMLModelTest, dtdoMLModelTestIQTree,
    dtdoMCLComputePattern,
    dtdoMCLTsTvBias:           Result := 3;

    dtdoMPTree,
    dtdoMPComputeUserTreeBLens: Result := 4;

    dtdoNJTree,
    dtdoMETree,
    dtdoUPGMATree,
    dtdoMLTree,
    dtdoMLIQTree,
    dtdoOLSInteriorBranchTest:  Result := 3;

    dtdoMPInferAncSeq,
    dtdoMPInferAncSeqMyPeg,
    dtdoMLInferAncSeqMyPeg,
    dtdoMLInferAncSeq:          Result := 3;

    dtdoMPPredictLivingSeq,
    dtdoMLPredictLivingSeq:     Result := 3;

    dtdoMLComputePattern,
    dtdoOLSComputeUserTreeBLens,
    dtdoPhyloQ,
    dtdoMLComputeUserTreeBLens,
    dtdoMLInferSiteBySiteRates: Result := 3;
  end;

end;

//--- Distance information ----
constructor TDistPack.Create;
var
  i: Integer;
begin
  where := 0;
  for i := 0 to High(DistType) do
    DistType[i] := gdNone;
  FGammaPara := 0.0;
  BootReps := 1000;
  RandSeed := 1;
  FInaRValue := -1;
end;

function TDistPack.GetIsValid: Boolean;
begin
  Result := (DistType[0] <> gdNone);
end;

procedure TDistPack.clear;
var
 i: Integer;
begin
  where := 0;
  for i:= 0 to High(DistType) do
    DistType[i] := gdNone;
  FGammaPara := 0.0;
  BootReps := 1000;
end;

procedure TDistPack.Unwind;
begin
  Where := 0;
end;

function TDistPack.GetComputationType: TDistType;
begin
  Result := gdNone;
  if      DoesContain(gdPairwise)         then Result := gdPairwise
  else if DoesContain(gdOverallMean)      then Result := gdOverallMean
  else if DoesContain(gdWithinGroupMean)  then Result := gdWithinGroupMean
  else if DoesContain(gdBetweenGroupMean) then Result := gdBetweenGroupMean
  else if DoesContain(gdNetGroupMean)     then Result := gdNetGroupMean
  else if DoesContain(gdAvgDiversityWithinSubPops) then Result := gdAvgDiversityWithinSubPops
  else if DoesContain(gdAvgDiversityForEntirePop)  then Result := gdAvgDiversityForEntirePop
  else if DoesContain(gdInterPopDiversity)         then Result := gdInterPopDiversity
  else if DoesContain(gdPropOfInterPopDiversity)   then Result := gdPropOfInterPopDiversity;
end;

function TDistPack.GetBasicAnalysisType: TDistType;
begin
  Result := gdNone;   // note that it is a delicate balance, and gdOneNuc must come before gdThreeNuc test.
  if      DoesContain(gdOneNuc)     then Result := gdOneNuc
  else if DoesContain(gdThreeNuc)   then Result := gdThreeNuc
  else if DoesContain(gdAmino)      then Result := gdAmino
  else if DoesContain(gdSynNonSyn)  then Result := gdSynNonSyn
end;

// Distance model
function TDistPack.GetDistModel: TDistType;
begin
  Result := gdNone;
  if DoesContain(gdOneNuc) then
  begin
    if      DoesContain(gdNoOfDiff)    then Result := gdNoOfDiff
    else if DoesContain(gdPropDist)    then Result := gdPropDist
    else if DoesContain(gdTestRandom)  then Result := gdTestRandom
    else if DoesContain(gdJukesCantor) then Result := gdJukesCantor
    else if DoesContain(gdTajimaNei)   then Result := gdTajimaNei
    else if DoesContain(gdKimura2para) then Result := gdKimura2para
    else if DoesContain(gdTamura)      then Result := gdTamura
    else if DoesContain(gdTamuraNei)   then Result := gdTamuraNei
    else if DoesContain(gdMCL)         then Result := gdMCL
    else if DoesContain(gdLogDet)      then Result := gdLogDet
    else if DoesContain(gdGlobalPara)  then Result := gdGlobalPara
    else if DoesContain(gdREV)         then Result := gdREV
    else if DoesContain(gdHKY)         then Result := gdHKY                // for ML
    else if DoesContain(gdEmbryoWhole) then Result := gdEmbryoWhole
    else if DoesContain(gdEmbryoExpression) then Result := gdEmbryoExpression
    else if DoesContain(gdEmbryoWtd)        then Result := gdEmbryoWtd
    else if DoesContain(gdAlleleFreqDist)   then Result := gdAlleleFreqDist
  end
  else if DoesContain(gdSynNonsyn) then
  begin
    if      DoesContain(gdNeiGojobori) then Result := gdNeiGojobori
    else if DoesContain(gdIna)         then Result := gdIna
    else if DoesContain(gdLWL)         then Result := gdLWL
    else if DoesContain(gdPBL)         then Result := gdPBL
    else if DoesContain(gdComeronKumar)then Result := gdComeronKumar
    else if DoesContain(gdTamuraNei)   then Result := gdTamuraNei
    else if DoesContain(gdFelsenstein81) then Result := gdFelsenstein81
    else if DoesContain(gdREV)         then Result := gdREV
    else if DoesContain(gdHKY)         then Result := gdHKY;
  end
  else if DoesContain(gdAmino) then
  begin
    if      DoesContain(gdNoOfDiff)    then Result := gdNoOfDiff
    else if DoesContain(gdPropDist)    then Result := gdPropDist
    else if DoesContain(gdPoisson)     then Result := gdPoisson
    else if DoesContain(gdEqualInput)  then Result := gdEqualInput
    else if DoesContain(gdTestRandom)  then Result := gdTestRandom
    else if DoesContain(gdJones)       then Result := gdJones
    else if DoesContain(gdJonesPi)     then Result := gdJonesPi
    else if DoesContain(gdDayhoff)     then Result := gdDayhoff
    else if DoesContain(gdDayhoffPi)   then Result := gdDayhoffPi
    else if DoesContain(gdWAG)         then Result := gdWAG       // for ML
    else if DoesContain(gdWAGPi)       then Result := gdWAGPi       // for ML
    else if DoesContain(gdLG)          then Result := gdLG       // for ML
    else if DoesContain(gdLGPi)        then Result := gdLGPi       // for ML
    else if DoesContain(gdMtRev24)     then Result := gdMtRev24       // for ML
    else if DoesContain(gdMtRev24Pi)   then Result := gdMtRev24Pi       // for ML
    else if DoesContain(gdCpRev)       then Result := gdCpRev         // for ML
    else if DoesContain(gdCpRevPi)     then Result := gdCpRevPi        // for ML
    else if DoesContain(gdRtRev)       then Result := gdRtRev         // for ML
    else if DoesContain(gdRtRevPi)     then Result := gdRtRevPi         // for ML
    else if DoesContain(gdAlleleFreqDist)   then Result := gdAlleleFreqDist;
  end;
end;

// Distance Correction (same as DistModel for nuc and amino)
function TDistPack.GetDistCorrection: TDistType;
begin
  Result := gdNone;
  if DoesContain(gdOneNuc) or DoesContain(gdAmino) then
    Result := DistModel
  else if DoesContain(gdSynNonsyn) then
  begin
    if      DoesContain(gdNoOfDiff)    then Result := gdNoOfDiff
    else if DoesContain(gdPropDist)    then Result := gdPropDist
    else if DoesContain(gdJukesCantor) then Result := gdJukesCantor
    else if DoesContain(gdKimura2para) then Result := gdKimura2para;
  end
end;

// Dist component
function TDistPack.GetDistComponent: TDistType;
begin
  Result := gdNone;
  if DoesContain(gdAlleleFreqDist) then // it applies to all types of cases if implemented (even for codons)
  begin
    if      DoesContain(gdBhattacharyaDist)then Result := gdBhattacharyaDist
    else if DoesContain(gdManhattanDist)   then Result := gdManhattanDist
    else if DoesContain(gdProvostiDist)    then Result := gdProvostiDist
    else if DoesContain(gdRogersDist)      then Result := gdRogersDist
    else if DoesContain(gdNei1983Dist)     then Result := gdNei1983Dist
    else if DoesContain(gdKumarGadagkarDist)then Result := gdKumarGadagkarDist
    else if DoesContain(gdKumarGadagkarDisparityDist)then Result := gdKumarGadagkarDisparityDist;
  end
  else if DoesContain(gdOneNuc) then
  begin
    if      DoesContain(gdAll)          then  Result := gdAll
    else if DoesContain(gdNucTsOnly)    then  Result := gdNucTsOnly
    else if DoesContain(gdNucTvOnly)    then  Result := gdNucTvOnly
    else if DoesContain(gdNucRatioTsTv) then  Result := gdNucRatioTsTv
    else if DoesContain(gdNucSeparateTsTv) then  Result := gdNucSeparateTsTv
    else if DoesContain(gdCommonSites)  then  Result := gdCommonSites;
  end
  else if DoesContain(gdSynNonsyn) then
  begin
    if      DoesContain(gdSynOnly)         then  Result := gdSynOnly
    else if DoesContain(gdNonsynOnly)      then  Result := gdNonsynOnly
    else if DoesContain(gdRatioSynNonsyn)  then  Result := gdRatioSynNonsyn
    else if DoesContain(gdRatioNonsynSyn)  then  Result := gdRatioNonsynSyn
    else if DoesContain(gdDiffSynNonsyn)   then  Result := gdDiffSynNonsyn
    else if DoesContain(gdDiffNonsynSyn)   then  Result := gdDiffNonsynSyn
    else if DoesContain(gdNonsyn0Fold)     then  Result := gdNonsyn0Fold
    else if DoesContain(gdSyn4Fold)        then  Result := gdSyn4Fold
    else if DoesContain(gdNoOfSynSites)    then  Result := gdNoOfSynSites
    else if DoesContain(gdNoOfNonsynSites) then  Result := gdNoOfNonsynSites
    else if DoesContain(gdNoOf0FoldSites)  then  Result := gdNoOf0FoldSites
    else if DoesContain(gdNoOf4FoldSites)  then  Result := gdNoOf4FoldSites
    else if DoesContain(gdNeutralEvolTest) then  Result := gdNeutralEvolTest
    else if DoesContain(gdPosSelTest)      then  Result := gdPosSelTest
    else if DoesContain(gdPurifySelTest)   then  Result := gdPurifySelTest;
  end
  else if DoesContain(gdAmino) then
  begin
    if      DoesContain(gdCommonSites)  then  Result := gdCommonSites
    else if DoesContain(gdAll)          then  Result := gdAll;
  end;
end;

function TDistPack.GetSelectionTestType: TDistType;
begin
  Result := gdNone;
  if DoesContain(gdSynNonsyn) then
  begin
    if      DoesContain(gdExactTest)            then  Result := gdExactTest
    else if DoesContain(gdZSelectionTest)       then  Result := gdZSelectionTest
    else if DoesContain(gdMcdonaldKreitmanTest) then  Result := gdMcdonaldKreitmanTest;
  end;
end;

function TDistPack.GetVarType: TDistType;
begin
  Result := gdNone;
  if      DoesContain(gdBootstrapVar)     then Result := gdBootstrapVar
  else if DoesContain(gdAnalyticalVar)    then Result := gdAnalyticalVar
  else if DoesContain(gdJacknifeVar)      then Result := gdJacknifeVar
  else if DoesContain(gdMonteCarloTest)   then Result := gdMonteCarloTest;
end;

function TDistPack.HasDifference: Boolean;
begin
  Result := False;
  if DoesContain(gdDiffSynNonsyn)  or
     DoesContain(gdDiffNonsynSyn)  or
     DoesContain(gdZSelectionTest) then
  Result := True;
end;

function TDistPack.GetIsComputeGamma : Boolean;
begin
  Result := DoesContain(gdGamma); //
end;

function TDistPack.GetIsComputeInvar : Boolean;  // gdGammaInv or gdInvar with ttML
begin
  Result := DoesContain(gdInvar); //
end;

function TDistPack.GetNoOfGammaCats : Integer;  // returns the number of gamma categories
begin
  Result := FGammaCats;
end;

function TDistPack.CanCorrectForPatternHeterogeneity: Boolean;
begin
  Result := False;
  if DoesContain(gdOneNuc) then
  begin
    Result := DoesContain(gdTajimaNei) or
              DoesContain(gdTamura)    or
              DoesContain(gdTamuraNei) or
              DoesContain(gdMCL)       or
              DoesContain(gdLogDet);
    Result := Result and not DoesContain(gdNucRatioTsTv);
  end
  else if DoesContain(gdAmino) then
    Result := DoesContain(gdEqualInput);

  Result := Result and (not DoesContain(gdCommonSites));
end;

function TDistPack.CanSeparateTsTv: Boolean;
begin
  Result := not ( DoesContain(gdTajimaNei) or
                  DoesContain(gdJukesCantor) or
                  DoesContain(gdLogDet) 
                );
end;

function TDistPack.IsNeiGojoboriType: Boolean;
begin
  Result := not ( DoesContain(gdLWL) or
                  DoesContain(gdPBL) or
                  DoesContain(gdComeronKumar)
                );
end;

function TDistPack.NeedsFixedRValue: Boolean;
begin
  Result := DoesContain(gdIna);
end;

// some help
function TDistPack.HasOnlyPatternHeterogeneity: Boolean;
begin
  Result := False;
  if DoesContain(gdOneNuc) then
    Result := DoesContain(gdLogDet);
end;

function TDistPack.CanCorrectForGammaRates: Boolean;
begin
  Result := False;
  if DoesContain(gdOneNuc) then
  begin
    Result := DoesContain(gdJukesCantor) or DoesContain(gdKimura2para) or
              DoesContain(gdTamura) or DoesContain(gdHKY) or
              DoesContain(gdTajimaNei) or DoesContain(gdTamuraNei) or
              DoesContain(gdMCL) or DoesContain(gdREV);
    Exit;
  end;

  if DoesContain(gdAmino) then
  begin
    Result := DoesContain(gdPoisson) or DoesContain(gdEqualInput) or
              DoesContain(gdDayhoff) or DoesContain(gdDayhoffPi) or
              DoesContain(gdJones)   or DoesContain(gdJonesPi) or
              DoesContain(gdMtRev24) or DoesContain(gdMtRev24Pi) or
              DoesContain(gdCpRev)   or DoesContain(gdCpRevPi) or
              DoesContain(gdWAG)     or DoesContain(gdWAGPi) or
              DoesContain(gdLG)      or DoesContain(gdLGPi) or
              DoesContain(gdRtRev)   or DoesContain(gdRtRevPi);
  end;
end;

function TDistPack.CanCorrectForOnlyGammaRates: Boolean;
begin
  Result := False;
  if DoesContain(gdOneNuc) then
  begin
    Result := DoesContain(gdJukesCantor) or DoesContain(gdKimura2para) or
              DoesContain(gdTamura) or
              DoesContain(gdTajimaNei) or DoesContain(gdTamuraNei) or
              DoesContain(gdMCL);
    Exit;
  end;

  if DoesContain(gdAmino) then
  begin
    Result := DoesContain(gdPoisson) or DoesContain(gdDayhoff) or
              DoesContain(gdJones) or DoesContain(gdEqualInput);
  end;
end;

function TDistPack.CanCorrectForGammaRatesPlusInv(AOperation: TDistTreeDlgOption): Boolean;
begin
  case AOperation of
    dtdoMLClockTest,
    dtdoMLClockTestLocal,
    dtdoRelTimeML, dtdoRtdtML,
    dtdoCorrTestML,
    dtdoMLModelTest,
    dtdoMLTree,
    dtdoMLIQTree,
    dtdoMLInferAncSeq,
    dtdoMLInferAncSeqMyPeg,
    dtdoMLPredictLivingSeq, dtdoBEAM,
    dtdoEpML,
    dtdoMLComputePattern,
    dtdoMLTsTvBias,
    dtdoMLGammRates,
    dtdoMLInferSiteBySiteRates,
    dtdoMLComputeUserTreeBLens:
      Result := True
  else
    Result := False;
  end;
  Result := Result and CanCorrectForGammaRates;
end;

function TDistPack.CanComputeVar: Boolean;
begin
  Result := not ( DoesContain(gdCommonSites) or
                  DoesContain(gdNoOfSynSites) or
                  DoesContain(gdNoOfNonsynSites) or
                  DoesContain(gdNonsyn0Fold) or
                  DoesContain(gdSyn4Fold) or
                  DoesContain(gdNoOf0FoldSites) or
                  DoesContain(gdNoOf4FoldSites)
                );
end;

function TDistPack.CanComputeAnalyticalVar: Boolean;
begin
  Result := ComputationType = gdPairwise;
  if not Result then Exit;
  Result := False;
  if DoesContain(gdOneNuc) then
  begin
    Result := DoesContain(gdNoOfDiff) or
              DoesContain(gdPropDist) or
              DoesContain(gdJukesCantor) or
              DoesContain(gdTajimaNei) or
              DoesContain(gdKimura2para) or
              DoesContain(gdTamura) or
              DoesContain(gdTamuraNei);
              {TODO 5 -oKumar: gdGlobalPara?}
              // note that logDet is excluded
  end
  else if DoesContain(gdSynNonsyn) then
  begin
    Result := DoesContain(gdNeiGojobori) or
              DoesContain(gdIna) or
              DoesContain(gdLWL) or
              DoesContain(gdPBL) or
              DoesContain(gdComeronKumar);
    if Result then
      Result := not
              (DoesContain(gdRatioSynNonsyn) or
               DoesContain(gdRatioNonsynSyn) or
               DoesContain(gdNoOfSynSites)   or
               DoesContain(gdNoOfNonsynSites)or
               DoesContain(gdNoOf0FoldSites) or
               DoesContain(gdNoOf4FoldSites))
  end
  else if DoesContain(gdAmino) then
  begin
    Result := DoesContain(gdNoOfDiff) or
              DoesContain(gdPropDist) or
              DoesContain(gdPoisson) or
              DoesContain(gdEqualInput);
  end;

  if Result then
      Result := not (DoesContain(gdCommonSites) or  DoesContain(gdHetero))
end;

// based on three level structure
// 0: Nuc/Amino/SynNonsyn
// 2: DistModel
// 2: DistCorrection  (redundant)
// 3: DistComponent

function TDistPack.AddTypeByStr(TheValue: AnsiString; IsCoding: Boolean): Boolean;
begin
  Result := True;

  if TheValue = SynStr then                        AddType(gdThreeNuc)
  else if IsCoding and (TheValue = AminoStr) then  AddType(gdThreeNuc)
  else if IsCoding and (TheValue = CodonStr) then  AddType(gdThreeNuc);

  if      TheValue = NucStr   then AddType(gdOneNuc)
  else if TheValue = SnvStr   then AddType(gdOneNuc)
  else if TheValue = SynStr   then AddType(gdSynNonSyn)
  else if TheValue = AminoStr then AddType(gdAmino);

  if      TheValue = NeutralityPickStr   then AddType(gdNeutralEvolTest)
  else if TheValue = PositiveSelPickStr  then AddType(gdPosSelTest)
  else if TheValue = PurifyingSelPicStr  then AddType(gdPurifySelTest);

  // Averaging
  if      TheValue = SelInSeqPairsPickStr  then AddType(gdPairwise)
  else if TheValue = SelGpAvgPickStr       then AddType(gdWithinGroupMean)
  else if TheValue = SelOverallAvgStr      then AddType(gdOverallMean);

      // for all models
  if      TheValue = Model_NoOfDiffStr        then AddType(gdNoOfDiff)
  else if TheValue = Model_p_distanceStr      then AddType(gdPropDist)
    // nucleotide models
  else if TheValue = Model_Jukes_CantorStr    then AddType(gdJukesCantor)
  else if TheValue = Model_Kimura_2_ParaStr   then AddType(gdKimura2para)
  else if TheValue = Model_Tajima_NeiStr      then AddType(gdTajimaNei)
  else if TheValue = Model_FelsensteinStr     then AddType(gdFelsenstein81)
  else if TheValue = Model_Tamura_3_ParaStr   then AddType(gdTamura)
  else if TheValue = Model_Tamura_Nei_ParaStr then AddType(gdTamuraNei)
  else if TheValue = Model_HKYStr             then AddType(gdHKY)
  else if TheValue = Model_MCLStr             then AddType(gdMCL)
  else if TheValue = Model_GTRStr             then AddType(gdREV)
  else if TheValue = Model_LogDet_TKStr       then AddType(gdLogDet)
    // syn-nonsyn models
  else if TheValue = Model_NeiGojoboriNoOfDiffStr       then begin AddType(gdNeiGojobori); AddType(gdNoOfDiff)    end
  else if TheValue = Model_NeiGojoboriPDistStr          then begin AddType(gdNeiGojobori); AddType(gdPropDist)    end
  else if TheValue = Model_NeiGojoboriJCDistStr         then begin AddType(gdNeiGojobori); AddType(gdJukesCantor) end
  else if TheValue = Model_ModifiedNeiGojoboriPDistStr  then begin AddType(gdIna);         AddType(gdPropDist)    end
  else if TheValue = Model_ModifiedNeiGojoboriJCDistStr then begin AddType(gdIna);         AddType(gdJukesCantor) end
  else if TheValue = Model_Li_Wu_LuoStr                 then AddType(gdLWL)
  else if TheValue = Model_Pamilo_Bianchi_LiStr         then AddType(gdPBL)
  else if TheValue = Model_KumarStr                     then AddType(gdComeronKumar)
    // amino models
  else if TheValue = Model_PoissonStr     then AddType(gdPoisson)
  else if TheValue = Model_EqualInputStr  then AddType(gdEqualInput)
  else if TheValue = Model_DayhoffStr     then AddType(gdDayhoff)
  else if TheValue = Model_DayhoffPistr   then AddType(gdDayhoffPi)
  else if TheValue = Model_JTTStr         then AddType(gdJones)
  else if TheValue = Model_JTTPiStr       then AddType(gdJonesPi)
  else if TheValue = Model_WAGStr         then AddType(gdWAG)
  else if TheValue = Model_WAGPiStr       then AddType(gdWAGPi)
  else if TheValue = Model_LGStr         then AddType(gdLG)
  else if TheValue = Model_LGPiStr       then AddType(gdLGPi)
  else if TheValue = Model_mtREV24Str     then AddType(gdMtREV24)
  else if TheValue = Model_mtREV24PiStr   then AddType(gdMtREV24Pi)
  else if TheValue = Model_cpREVStr       then AddType(gdCpREV)
  else if TheValue = Model_cpREVPiStr     then AddType(gdCpREVPi)
  else if TheValue = Model_rtREVStr       then AddType(gdRtREV)
  else if TheValue = Model_rtREVPiStr     then AddType(gdRtREVPi)
  else if TheValue = Model_Allele_Freq_Dist then AddType(gdAlleleFreqDist)
    // components
  else if TheValue = ValidCommonSitesPickStr       then  AddType(gdCommonSites)
  else if TheValue = AllStr                        then  AddType(gdAll)
  else if TheValue = NucTsOnlyPickStr              then  AddType(gdNucTsOnly)
  else if TheValue = NucTvOnlyPickStr              then  AddType(gdNucTvOnly)
  else if TheValue = NucTsTvSeparatePickStr        then  AddType(gdNucSeparateTsTv)
  else if TheValue = NucRatioTsTvPickStr           then  AddType(gdNucRatioTsTv)
  else if TheValue = SynOnlyPickStr                then  AddType(gdSynOnly)
  else if TheValue = NonsynOnlyPickStr             then  AddType(gdNonsynOnly)
  else if TheValue = NoOfSynSitesPickStr           then  AddType(gdNoOfSynSites)
  else if TheValue = NoOfNonsynSitesPickStr        then  AddType(gdNoOfNonsynSites)
  else if TheValue = DiffSynNonsynPickStr          then  AddType(gdDiffSynNonsyn)
  else if TheValue = DiffNonsynSynPickStr          then  AddType(gdDiffNonsynSyn)
  else if TheValue = Syn4FoldPickStr               then  AddType(gdSyn4Fold)
  else if TheValue = Nonsyn0FoldPickStr            then  AddType(gdNonsyn0Fold)
  else if TheValue = NoOf4FoldSitesPickStr         then  AddType(gdNoOf4FoldSites)
  else if TheValue = NoOf0FoldSitesPickStr         then  AddType(gdNoOf0FoldSites)
    // allele freq dist components
  else if TheValue = BhattacharyaPickStr           then  AddType(gdBhattacharyaDist)
  else if TheValue = PrevostiPickStr               then  AddType(gdProvostiDist)
  else if TheValue = RogersPickStr                 then  AddType(gdRogersDist)
  else if TheValue = Nei1983PickStr                then  AddType(gdNei1983Dist)
  else if TheValue = KumarGadagkarPickStr          then  AddType(gdKumarGadagkarDist)
  else if TheValue = KumarGadagkarDisparityPickStr then AddType(gdKumarGadagkarDisparityDist)
{  else if StrPos(TheValue, 'Whole Embryo (DW)') <> nil then
    AddType(gdEmbryoWhole)
  else if StrPos(TheValue, 'Only Expression (DE)') <> nil then
    AddType(gdEmbryoExpression)
  else if StrPos(TheValue, 'Weighted (DWtd)') <> nil then
    AddType(gdEmbryoWtd) // expression distance
}  else
    Result := False;
end;

function TDistPack.AddType(Value: TDistType): Boolean;  // adds a new structure info
begin
  if DoesContain(Value) then // Checks to see if there's already an instance of the DistType in the array, if so then it doesn't add it again. This prevents range check errors when MEGA gets into a loop of adding something over and over (Although you may still get a stack overflow).
  begin
    Result := True;
    Exit;
  end;
  DistType[where] := Value;
  Inc(where);
  Result := True;
end;

procedure TDistPack.ConstructPack(FOperation: TDistTreeDlgOption; const AStringList: TStrings; IsCoding: Boolean);
begin
  Clear;
  AddOperationTypeAndScope(FOperation, AStringList, IsCoding);
  AddPrimaryModelType(AStringList, IsCoding);
  AddExtendedModelInfo(AStringList);
  AddDistAnalysisPrefs(AStringList);
end;

procedure TDistPack.AddOperationTypeAndScope(FOperation: TDistTreeDlgOption; const AStringList: TStrings; IsCoding: Boolean);  // converts dtdo to gd appropriately
begin
   case FOperation of
     dtdoPairwiseDist             : AddType(gdPairwise);
     dtdoOverallMean              : AddType(gdOverallMean);
     dtdoWithinGroupMean          : AddType(gdWithinGroupMean);
     dtdoBetweenGroupMean         : AddType(gdBetweenGroupMean);
     dtdoNetGroupMean             : AddType(gdNetGroupMean);
     dtdoAvgDiversityWithinSubPops: AddType(gdAvgDiversityWithinSubPops);
     dtdoAvgDiversityForEntirePop : AddType(gdAvgDiversityForEntirePop);
     dtdoInterPopDiversity        : AddType(gdInterPopDiversity);
     dtdoPropOfInterPopDiversity  : AddType(gdPropOfInterPopDiversity);
     dtdoCompositionDistance      : AddType(gdCompositionDistance);
     dtdoDisparityIndex           : AddType(gdDisparityIndex);
     dtdoDisparityIndexTest       : AddType(gdDisparityIndexTest);
     dtdoSelectionZTest           : AddType(gdZSelectionTest);
     dtdoSelectionExactTest       : AddType(gdExactTest);
     dtdoTajimaNeutralityTest     : AddType(gdTajimaNeutralityTest);
     dtdoTajimaClockTest          : AddType(gdTajimaClockTest);
     dtdoMCLComputePattern        : AddType(gdMCLComputePattern);
     dtdoMCLTsTvBias              : AddType(gdMCLTsTvBias);
   else
     AddType(gdPairwise);
   end;

   with AStringList do
   begin
     if IndexOfName(opsScope2) >= 0 then
       AddTypeByStr(Values[opsScope2], IsCoding);

     if IndexOfName(opsSelHypoToTest2) >= 0 then
       AddTypeByStr(Values[opsSelHypoToTest2], IsCoding);
   end;

  if DoesContain(gdCompositionDistance) or DoesContain(gdDisparityIndex) or DoesContain(gdDisparityIndexTest) then
    AddType(gdNoOfDiff);  // added by Dan; these three tests weren't computing anything at all and just spitting out 0 for all results.
end;

procedure TDistPack.AddPrimaryModelType(const AStringList: TStrings; IsCoding: Boolean);  // adds model type
begin
  with AStringList do
  begin
    // class of model (nuc, syn/nonsyn, amino)
    if IndexOfName(opsNucSynAminoType2) >= 0 then
      AddTypeByStr(Values[opsNucSynAminoType2], IsCoding);

    // type of model (JC, F81, etc)
    if IndexOfName(opsSubsModel2) >= 0 then
      AddTypeByStr(Values[opsSubsModel2], IsCoding);

    // sub components to include, included allele freq model
    if IndexOfName(opsSubsToInclude2) >= 0 then
      AddTypeByStr(Values[opsSubsToInclude2], IsCoding);
  end;
end;

procedure TDistPack.AddExtendedModelInfo(const AStringList: TStrings);
begin
  with AStringList do
  begin
    if DoesContain(gdIna) then
      InaRValue := StrToFloatWithInvalid(Values[opsFixedRValue2]);

    if IndexOfName(opsRatesAmongSites2) >= 0 then
    begin
      if (Trim(LowerCase(Values[opsRatesAmongSites2])) = Trim(LowerCase(GammaRatePickStr))) or
         (Trim(LowerCase(Values[opsRatesAmongSites2])) = Trim(LowerCase(GammaInvRatePickStr))) then
        AddType(gdGamma);

      if (Trim(LowerCase(Values[opsRatesAmongSites2])) = Trim(LowerCase(InvariantPickStr))) or
         (Trim(LowerCase(Values[opsRatesAmongSites2])) = Trim(LowerCase(GammaInvRatePickStr))) then
        AddType(gdInvar);

      if (IndexOfName(opsGammaPara2) >= 0) then
          FGammaPara :=  StrToFloatWithInvalid(Values[opsGammaPara2]);

      if (IndexOfName(opsGammaCats2) >= 0) then
         FGammaCats := StrToIntWithInvalid(Values[opsGammaCats2]);
    end;

    if IndexOfName(opsPatternAmongLineages2) >= 0 then
      if Trim(LowerCase(Values[opsPatternAmongLineages2])) = Trim(LowerCase(HeteroPatternPickStr)) then
        AddType(gdHetero);
  end;
end;

procedure TDistPack.AddDistAnalysisPrefs(const AStringList: TStrings);
begin
  with AStringList do
  begin
    if IndexOfName(opsEstimateVar2) >= 0 then
    begin
      {if IndexOfName(opsEstimateVar2) >= 0 then // duplicate code, this will always be true when this code is reachable.
      begin}
        if Values[opsEstimateVar2] = BootTestStr then
        begin
          AddType(gdBootstrapVar);
          BootReps := StrToIntWithInvalid(Values[opsBootReps2]);
          RandSeed := Random(100);
        end
        else if Values[opsEstimateVar2] = AnalTestStr then
          AddType(gdAnalyticalVar);
      {end;}
    end
    else if IndexOfName(opsMonteCarloReps2) >= 0 then
    begin
      AddType(gdMonteCarloTest);
      BootReps := StrToIntWithInvalid(Values[opsMonteCarloReps2]);
      RandSeed := Random(1000);
    end;
    if (IndexOfName(opsGeneticCodeTableSLAC) >= 0) and (Values[opsGeneticCodeTableSLAC] <> NotApplicableStr) then
      FGeneticCode := Values[opsGeneticCodeTableSLAC];
    if (IndexOfName(opsGeneticCodeTable2) >= 0) and (Values[opsGeneticCodeTable2] <> NotApplicableStr) then
      FGeneticCode := Values[opsGeneticCodeTable2]
    else
      {$IFDEF VISUAL_BUILD}
      FGeneticCode := MegaForm.GetCodeTableName;
      {$ELSE}
      FGeneticCode := D_MegaMain.CodeTableName;
      {$ENDIF}
  end;
end;

function TDistPack.GetNext: TDistType;
begin
  Result := DistType[where];
  if Result <> gdNone then
    Inc(where);
end;

function TDistPack.DoesContain(Value: TDistType): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i:=0 to High(DistType) do
   if DistType[i]= Value then
     Exit;
  Result := False;
end;

function TDistPack.Replace(FromValue, ToValue : TDistType): Boolean; // if contained the replaced, else added
var
  i: Integer;
begin
  Result := True;
  if FromValue <> gdNone then
    for i:=0 to High(DistType) do
      if DistType[i]= FromValue then
      begin
        DistType[i] := ToValue;
        Exit;
      end;
    // if it falls here then
  AddType(ToValue);
  Result := False;
end;


function TDistPack.GetAcronym: AnsiString;
begin
  Result := EmptyStr;
  // if number of sites then
  // if difference then
  if DoesContain(gdTestRandom) then
    Result := 'Test Random'
  else if DoesContain(gdCompositionDistance) then
    Result := 'Dc'
  else if DoesContain(gdDisparityIndex) then
    Result := 'ID'
  else if DoesContain(gdDisparityIndexTest) then
    Result := 'ID'
  else if DoesContain(gdOneNuc) then
  begin
    Result := 'd';
    if      DoesContain(gdNucRatioTsTv) then Result := 'R'
    else if DoesContain(gdCommonSites)  then Result := 'L';
  end
  else if DoesContain(gdSynNonsyn) then
  begin
    if      DoesContain(gdSynOnly)        then Result := 'dS'
    else if DoesContain(gdNonsynOnly)     then Result := 'dN'
    else if DoesContain(gdRatioSynNonsyn) then Result := 'dS/dN'
    else if DoesContain(gdRatioNonsynSyn) then Result := 'dN/dS'
    else if DoesContain(gdDiffSynNonsyn)  then Result := 'dS-dN'
    else if DoesContain(gdDiffNonsynSyn)  then Result := 'dN-dS'
    else if DoesContain(gdNoOfSynSites)   then Result := 'Lsyn'
    else if DoesContain(gdNoOfNonsynSites)then Result := 'Lnonsyn'
    else if DoesContain(gdNonsyn0Fold)    then Result := 'd0f'
    else if DoesContain(gdSyn4Fold)       then Result := 'd4f'
    else if DoesContain(gdNoOf0FoldSites) then Result := 'L0'
    else if DoesContain(gdNoOf4FoldSites) then Result := 'L4'
    else
      Result := 'd';
  end
  else if DoesContain(gdAmino) then
  begin
    Result := 'd';
    if DoesContain(gdCommonSites)  then Result := 'L';
  end;
end;

function TDistPack.GetNameWithoutDistComponent: AnsiString;
  function DistModelName(Value: TDistType): AnsiString;
  begin
    Result := EmptyStr;
    case Value of
      gdNeiGojobori:  Result := 'Nei-Gojobori';
      gdIna        :  Result := 'Modified Nei-Gojobori';
      gdLWL        :  Result := 'Li-Wu-Luo';
      gdPBL        :  Result := 'Pamilo-Bianchi-Li';
      gdComeronKumar: Result := 'Kumar';
      gdGamma      :  Result := 'Gamma'; //Format('Gamma %5.3f',[FGammaPara]);
      gdHetero     :  Result := 'Heterogeneous';
    end;
  end;
  function DistCorrectionName(Value: TDistType): AnsiString;
  var
    ValName : AnsiString;
  begin
    Result := EmptyStr;
    case Value of
      gdNone:              Result := EmptyStr;
      gdNoOfDiff:          Result := 'Number of differences';
      gdPropDist:          Result := 'p-distance';
      gdPoisson:           Result := 'Poisson correction';
      gdEqualInput:        Result := 'Equal Input ';
      gdJones:             Result := 'JTT matrix-based';
      gdJonesPi:           Result := 'JTT+F freq.';
      gdWAG:               Result := 'Whelan And Goldman'; // added 10-5-10, due to missing citation bug, Glen_S.
      gdWAGPi:             Result := 'Whelan And Goldman + Freq.'; // added 10-5-10, due to missing citation bug, Glen_S.
      gdLG:                Result := 'Le And Gascuel';
      gdLGPi:              Result := 'Le And Gascuel + Freq.';
      gdMtRev24:           Result := 'General Reversible Mitochondrial'; // added 10-5-10, due to missing citation bug, Glen_S.
      gdMtRev24Pi:         Result := 'General Reversible Mitochondrial + Freq.'; // added 10-5-10, due to missing citation bug, Glen_S.
      gdDayhoff:           Result := 'Dayhoff Matrix';
      gdDayhoffPi:         Result := 'Dayhoff+F.';
      gdJukesCantor:       Result := 'Jukes-Cantor';
      gdKimura2para:       Result := 'Kimura 2-parameter';
      gdTamura:            Result := 'Tamura 3-parameter';
      gdTamuraNei:         Result := 'Tamura-Nei';
      gdLogDet:            Result := 'LogDet (Tamura-Kumar)';
      gdMCL:               Result := 'Maximum Composite Likelihood';
      gdREV:               Result := 'Data specific';
      gdHKY:               Result := 'Hasegawa-Kishino-Yano';
      gdTajimaNei:         Result := 'Tajima-Nei';
      gdLWL:               Result := 'Li-Wu-Luo';
      gdPBL:               Result := 'Pamilo-Bianchi-Li';
      gdComeronKumar:      Result := 'Kumar';
      gdTestRandom:        Result := 'Test Randomness';
      gdGamma:             Result := 'Gamma'; //Format('Gamma %5.3f',[FGammaPara]);
      gdHetero:            Result := 'Heterogeneous';
      gdGlobalPara:        Result := 'Global Parameters';
      gdAlleleFreqDist:    Result := 'Allele Frequencies';
      else
      begin
        ValName := GetEnumName(TypeInfo(TDistType),integer(Value)) ;
        if IsDeveloper then
	  Raise Exception.Create('TDistPack.GetNameWithoutDistComponent.DistCorrectionName; The value "' + ValName + '" : TDistType was not found (missing case?)')
        else
	{$IFDEF VISUAL_BUILD}
          MessageDlg('MEGA has encountered an error in retrieving a citation for your caption.'+ LineEnding +'Please report this to the authors by the bug report form on our webpage.'+LineEnding+'Technical Info: ' + 'TDistPack.GetNameWithoutDistComponent.DistCorrectionName; The value "' + ValName + '" : TDistType was not found (missing case?)', mtInformation, [mbOK], 0);
        {$ELSE}
	  Warn_NV('MEGA has encountered an error in retrieving a citation for your caption.'+ LineEnding +'Please report this to the authors by the bug report form on our webpage.'+LineEnding+'Technical Info: ' + 'TDistPack.GetNameWithoutDistComponent.DistCorrectionName; The value "' + ValName + '" : TDistType was not found (missing case?)');
	{$ENDIF}
      end;
    end;
  end;
begin
  Result := EmptyStr;
  if  DoesContain(gdCompositionDistance) then
    Result := 'Composition Distance per site'
  else if DoesContain(gdDisparityIndex)       then
    Result := 'Disparity Index per site'
  else if DoesContain(gdDisparityIndexTest)   then
    Result := 'Disparity Index Test'
  else if DoesContain(gdOneNuc) then
  begin
    Result := 'Nucleotide: ';
    Result := Result + DistCorrectionName(DistCorrection);
    if DoesContain(gdGamma) AND (Length(DistModelName(gdGamma)) > 0) then
    begin
      if DoesContain(gdHetero) AND (Length(DistModelName(gdHetero)) > 0) then
        Result := Result + ' ('+ DistModelName(gdGamma)+','+DistModelName(gdHetero)+ ')'
      else
        Result := Result + ' ('+ DistModelName(gdGamma)+ ')';
    end;

    if DoesContain(gdHetero)  AND (Length(DistModelName(gdHetero)) > 0) then
      Result := Result + ' ('+ DistModelName(gdHetero)+ ')';
  end
  else if DoesContain(gdSynNonsyn) then
  begin
    Result := 'Codon: '+    DistModelName(DistModel) ;
    if Length(DistCorrectionName(DistCorrection)) > 0 then
      Result := Result + ' ('+ DistCorrectionName(DistCorrection)+')';
  end
  else if DoesContain(gdAmino) then
  begin
    Result := 'Amino: ';
    Result := Result + DistCorrectionName(DistCorrection);
    if DoesContain(gdGamma)  AND (Length(DistModelName(gdGamma)) > 0)then
    begin
      if DoesContain(gdHetero) AND (Length(DistModelName(gdHetero)) > 0) then
        Result := Result + ' ('+ DistModelName(gdGamma)+','+DistModelName(gdHetero)+ ')'
      else
        Result := Result + ' ('+ DistModelName(gdGamma)+ ')';
    end;
    if DoesContain(gdHetero) AND (Length(DistModelName(gdHetero)) > 0)  AND (Length(DistModelName(gdGamma)) > 0) then
      Result := Result + ' ('+ DistModelName(gdGamma)+','+DistModelName(gdHetero)+ ')'
  end;
end;

function TDistPack.GetName: AnsiString;
  function DistComponentName(Value: TDistType): AnsiString;
  begin
    Result := EmptyStr;
    case Value of
      gdNucTsOnly:     Result := '(transitional substitutions only)';
      gdNucTvOnly:     Result := '(transversional substitutions only)';
      gdNucRatioTsTv:  Result := '(transition/transversion)';
      gdCommonSites:   Result := '(Common valid pairwise sites)';
      gdSynOnly:       Result := '[synonymous substitutions]';
      gdNonsynOnly:    Result := '[nonsynonymous substitutions]';
      gdRatioSynNonsyn:Result := '[syn/nonsyn ratio]';
      gdRatioNonsynSyn:Result := '[nonsyn/syn ratio]';
      gdDiffSynNonsyn: Result := '[syn-nonsyn difference]';
      gdDiffNonsynSyn: Result := '[nonsyn-syn difference]';
      gdNonsyn0Fold:   Result := '(nonsynonymous subst. at 0-fold degenerate sites)';
      gdSyn4Fold:      Result := '(synonymous subst. at 4-fold degenerate sites)';
      gdNoOf0FoldSites:Result := '(No. of 0-fold redundant sites)';
      gdNoOf4FoldSites:Result := '(No. of 4-fold redundant sites)';
      gdExactTest:     Result := '(Exact test for positive selection)';
    end;
  end;
begin
  Result := GetNameWithoutDistComponent;
  if DoesContain(gdOneNuc) then
    Result := Result + ' '+ DistComponentName(DistComponent)
  else if DoesContain(gdSynNonsyn) then
    Result := Result + ' '+ DistComponentName(DistComponent);
end;

function TDistPack.GetFullDescription: AnsiString;
begin
  Result := Name;

  if      DoesContain(gdCompositionDistance) then  Result := Result + ' [Pairwise Composition Distance per site]'
  else if DoesContain(gdDisparityIndex)       then  Result := Result + ' [Pairwise Disparity Index per site]'
  else if DoesContain(gdDisparityIndexTest)   then  Result := Result + ' [Pairwise Disparity Index Test]'
  else if DoesContain(gdPairwise)         then Result := Result + ' [Pairwise distances]'
  else if DoesContain(gdOverallMean)      then Result := Result + ' [Overall average]'
  else if DoesContain(gdWithinGroupMean)  then Result := Result + ' [Within group average]'
  else if DoesContain(gdBetweenGroupMean) then Result := Result + ' [Between group average]'
  else if DoesContain(gdNetGroupMean)     then Result := Result + ' [Net between group average]';

  if VarType = gdBootstrapVar then
  begin
    Result := Result +
              ' Standard Error estimated by bootstrap method'+
              ' (Replications = '+IntToStr(BootReps)+ ' and'+
              ' random number seed = '+IntToStr(RandSeed)+')';
  end
  else if VarType = gdMonteCarloTest then
  begin
    Result := Result +
              ' Monte Carlo Test '+
              ' (Replications = '+IntToStr(BootReps)+ ' and'+
              ' random number seed = '+IntToStr(RandSeed)+')';
  end;
end;

{TODO -oKumar : This function really needs to be cleaned up }
function TDistPack.GetMethodCitation: AnsiString;
  function DistModelName(Value: TDistType): AnsiString;
  var
    ValName : AnsiString;
  begin
    Result := EmptyStr;
    case Value of
      gdNoOfDiff   :  Result := 'Nei_and_Kumar_2000';
      gdPropDist   :  Result := 'Nei_and_Kumar_2000';
      gdNeiGojobori:  Result := 'Nei_and_Gojobori_1986';
      gdIna        :  Result := 'Zhang_et_al_1998';
      gdLWL        :  Result := 'Li_et_al_1985';
      gdPBL        :  Result := 'Pamilo_and_Bianchi_1993';
      gdComeronKumar: Result := 'Nei_and_Kumar_2000';
      gdGamma      :  Result := '';
      gdHetero     :  Result := '';
      gdPoisson    :  Result := 'Zuckerkandl_and_Pauling_1965';
      gdEqualInput :  Result := 'Tajima_and_Nei_1984';
      gdJones      :  Result := 'RH_Jones_et_al_1992';
      gdJonesPi    :  Result := 'RH_Jones_et_al_1992';
      gdDayhoff    :  Result := 'RH_Dayhoff_1979';
      gdDayhoffPi  :  Result := 'RH_Dayhoff_1979';
      gdMtRev24    :  Result := 'Adachi_Hasegawa_1996';
      gdMtRev24Pi  :  Result := 'Adachi_Hasegawa_1996';
      gdCpRev      :  Result := 'Adachi_et_al_2000';
      gdCpRevPi    :  Result := 'Adachi_et_al_2000';
      gdRtRev      :  Result := 'Dimmic_et_al_2002';
      gdRtRevPi    :  Result := 'Dimmic_et_al_2002';
      gdWag        :  Result := 'Whelan_Goldman_2001';
      gdWagPi      :  Result := 'Whelan_Goldman_2001';
      gdLg         :  Result := 'Le_Gascuel_2008';
      gdLgPi       :  Result := 'Le_Gascuel_2008';
      gdREV        :  Result := 'Nei_and_Kumar_2000';
      gdHKY        :  Result := 'Hasegawa_Kishino_Yano_1985';
      gdTajimaNei  :  Result := 'Tajima_and_Nei_1984';
      gdTamuraNei:    Result := 'Tamura_and_Nei_1993';
      gdFelsenstein81 : Result := 'Felsenstein_1981';
      gdNone: Result := EmptyStr;
      else
      begin
        ValName := GetEnumName(TypeInfo(TDistType),integer(Value)) ;
        if IsDeveloper then
	  Raise Exception.Create('TDistPack.GetMethodCitation.DistModelName; The value "' + ValName + '" : TDistType was not found (missing case?)')
    	else
        {$IFDEF VISUAL_BUILD}
	  MessageDlg('MEGA has encountered an error in retrieving a citation for your caption.'+LineEnding+'Please report this to the authors by the bug report form on our webpage.'+LineEnding+'Technical Info: ' + 'TDistPack.GetMethodCitation.DistModelName; The value "' + ValName + '" : TDistType was not found (missing case?)', mtInformation, [mbOK], 0);
        {$ELSE}
	  Warn_NV('MEGA has encountered an error in retrieving a citation for your caption.'+LineEnding+'Please report this to the authors by the bug report form on our webpage.'+LineEnding+'Technical Info: ' + 'TDistPack.GetMethodCitation.DistModelName; The value "' + ValName + '" : TDistType was not found (missing case?)');
	{$ENDIF}
      end;
    end;
  end;
  function DistCorrectionName(Value: TDistType): AnsiString;
  var
    ValName: AnsiString;
  begin
    Result := EmptyStr;
    case Value of
      gdNone:              Result := EmptyStr;
      gdNoOfDiff:          Result := 'Nei_and_Kumar_2000';
      gdPropDist:          Result := 'Nei_and_Kumar_2000';
      gdPoisson:           Result := 'Zuckerkandl_and_Pauling_1965';
      gdEqualInput:        Result := 'Tajima_and_Nei_1984';
      gdJones:             Result := 'RH_Jones_et_al_1992';
      gdJonesPi:           Result := 'RH_Jones_et_al_1992';
      gdWag :              Result := 'Whelan_Goldman_2001';
      gdWagPi :            Result := 'Whelan_Goldman_2001';
      gdLg :               Result := 'Le_Gascuel_2008';
      gdLgPi:              Result := 'Le_Gascuel_2008';
      gdMtRev24:           Result := 'Adachi_Hasegawa_1996';
      gdMtRev24Pi:         Result := 'Adachi_Hasegawa_1996';
      gdCpRev:             Result := 'Adachi_et_al_2000';
      gdCpRevPi:           Result := 'Adachi_et_al_2000';
      gdRtRev:             Result := 'Dimmic_et_al_2002';
      gdRtRevPi:           Result := 'Dimmic_et_al_2002';
      gdDayhoff:           Result := 'RH_Dayhoff_1979';
      gdDayhoffPi:         Result := 'RH_Dayhoff_1979';
      gdJukesCantor:       Result := 'Jukes_and_Cantor_1969';
      gdKimura2para:       Result := 'Kimura_1980';
      gdTamura:            Result := 'Tamura_1992';
      gdTamuraNei:         Result := 'Tamura_and_Nei_1993';
      gdMCL:               Result := 'Tamura_et_al_2004';
      gdLogDet:            Result := 'Tamura_and_Kumar_2002';
      gdREV:               Result := 'Nei_and_Kumar_2000';
      gdHKY:               Result := 'Hasegawa_Kishino_Yano_1985';
      gdTajimaNei:         Result := 'Tajima_and_Nei_1984';
      gdLWL:               Result := 'Li_et_al_1985';
      gdPBL:               Result := 'Pamilo_and_Bianchi_1993';
      gdComeronKumar:      Result := 'Nei_and_Kumar_2000';
      gdTestRandom:        Result := '';
      gdGamma:             Result := '';
      gdHetero:            Result := '';
      gdGlobalPara:        Result := '';
      gdAlleleFreqDist:    Result := ''; //Allele Frequencies
      gdFelsenstein81:  Result := 'Felsenstein_1981';
      else
      begin
        ValName := GetEnumName(TypeInfo(TDistType),integer(Value)) ;
        if IsDeveloper then
	  Raise Exception.Create('TDistPack.GetMethodCitation.DistCorrectionName; The value "' + ValName + '" : TDistType was not found (missing case?)')
	else
	{$IFDEF VISUAL_BUILD} // Todo -o Glen, refactor so that we don't need the ifdef
	  MessageDlg('MEGA has encountered an error in retrieving a citation for your caption.' + LineEnding + 'Please report this to the authors by the bug report form on our webpage.'+LineEnding+'Technical Info: ' + 'TDistPack.GetMethodCitation.DistCorrectionName; The value "' + ValName + '" : TDistType was not found (missing case?)', mtInformation, [mbOK], 0);
        {$ELSE}
	  Warn_NV('MEGA has encountered an error in retrieving a citation for your caption.' + LineEnding + 'Please report this to the authors by the bug report form on our webpage.'+LineEnding+'Technical Info: ' + 'TDistPack.GetMethodCitation.DistCorrectionName; The value "' + ValName + '" : TDistType was not found (missing case?)');
	{$ENDIF}
      end;
    end;
  end;
begin
  Result := EmptyStr;
  if  DoesContain(gdCompositionDistance) then
    Result := ''
  else if DoesContain(gdDisparityIndex)       then
    Result := ''
  else if DoesContain(gdDisparityIndexTest)   then
    Result := ''
  else if DoesContain(gdOneNuc) then
  begin
    Result := Result + DistCorrectionName(DistCorrection);
  end
  else if DoesContain(gdThreeNuc) then
  begin
    Result := DistModelName(DistModel);
  end
  else if DoesContain(gdSynNonsyn) then
  begin
    Result := DistModelName(DistModel) ;
    if Length(DistCorrectionName(DistCorrection)) > 0 then
      Result := Result + ' ('+ DistCorrectionName(DistCorrection)+')';
  end
  else if DoesContain(gdAmino) then
  begin
    Result := Result + DistCorrectionName(DistCorrection);
    if DoesContain(gdGamma)  AND (Length(DistModelName(gdGamma)) > 0) then
    begin
      if DoesContain(gdHetero)  AND (Length(DistModelName(gdHetero)) > 0) then
        Result := Result + ' ('+ DistModelName(gdGamma)+','+DistModelName(gdHetero)+ ')'
      else
        Result := Result + ' ('+ DistModelName(gdGamma)+ ')';
    end;
    if DoesContain(gdHetero) AND (Length(DistModelName(gdHetero)) > 0)  AND (Length(DistModelName(gdGamma)) > 0) then
      Result := Result + ' ('+ DistModelName(gdGamma)+','+DistModelName(gdHetero)+ ')'
  end;
  if Result = EmptyStr then
    result := 'None';
end;

function TDistPack.GetPatternAssumption: AnsiString;
begin
  if DoesContain(gdHetero) then
    Result := 'heterogeneous'
  else
    Result := 'homogeneous';
end;

function TDistPack.GetSimpleName: AnsiString;
  function DistModelName(Value: TDistType): AnsiString;
  begin
    Result := EmptyStr;
    case Value of
      gdNeiGojobori:  Result := 'Nei-Gojobori';
      gdIna        :  Result := 'modified Nei-Gojobori';
      gdLWL        :  Result := 'Li-Wu-Luo';
      gdPBL        :  Result := 'Pamilo-Bianchi-Li';
      gdComeronKumar: Result := 'Kumar';
      gdGamma      :  Result := 'Gamma'; //Format('Gamma %5.3f',[FGammaPara]);
      gdHetero     :  Result := 'Heterogeneous';
      gdREV        :  Result := 'General Time Reversible';
      gdHKY        :  Result := 'Hasegawa-Kishino-Yano';
      gdFelsenstein81: Result := 'Felsenstein 1981';
      gdTamuraNei  :  Result := 'Tamura-Nei';
    end;
  end;
  function DistCorrectionName(Value: TDistType): AnsiString;
  begin
    Result := EmptyStr;
    case Value of
      gdNoOfDiff:          Result := 'Number of differences';
      gdPropDist:          Result := 'p-distance';
      gdPoisson:           Result := 'Poisson correction';
      gdEqualInput:        Result := 'Equal Input';
      gdJones:             Result := 'JTT matrix-based';
      gdJonesPi:           Result := 'Jones et al. w/freq.';
      gdDayhoff:           Result := 'Dayhoff matrix based';
      gdDayhoffPi:         Result := 'Dayhoff w/freq.';
      gdJukesCantor:       Result := 'Jukes-Cantor';
      gdKimura2para:       Result := 'Kimura 2-parameter';
      gdTamura:            Result := 'Tamura 3-parameter';
      gdTamuraNei:         Result := 'Tamura-Nei';
      gdLogDet:            Result := 'LogDet (Tamura-Kumar)';
      gdMCL:               Result := 'Maximum Composite Likelihood';
      gdREV:               Result := 'Data specific';
      gdHKY:               Result := 'Hasegawa-Kishino-Yano';
      gdTajimaNei:         Result := 'Tajima-Nei';
      gdLWL:               Result := 'Li-Wu-Luo';
      gdPBL:               Result := 'Pamilo-Bianchi-Li';
      gdComeronKumar:      Result := 'Kumar';
      gdTestRandom:        Result := 'Test Randomness';
      gdGamma:             Result := 'Gamma'; //Format('Gamma %5.3f',[FGammaPara]);
      gdHetero:            Result := 'Heterogeneous';
      gdGlobalPara:        Result := 'Global Parameters';
      gdWAG:               Result := 'Whelan And Goldman';
      gdWAGPi:             Result := 'Whelan And Goldman + Freq.';
      gdLg :               Result := 'Le_Gascuel_2008';
      gdLgPi:              Result := 'Le_Gascuel_2008';
      gdMtRev24:           Result := 'General Reversible Mitochondrial';
      gdMtRev24Pi:         Result := 'General Reversible Mitochondrial + Freq.';
      gdCpRev:             Result := 'General Reversible Chloroplast';
      gdCpRevPi:           Result := 'General Reversible Chloroplast + Freq.';
      gdRtRev:             Result := 'General Reverse Transcriptase';
      gdRtRevPi:           Result := 'General Reverse Transcriptase + Freq.';
      gdAlleleFreqDist:    Result := 'Allele Frequency Distance';
    end;
  end;
begin
  Result := EmptyStr;
  if  DoesContain(gdCompositionDistance) then
    Result := 'Composition Distance per site'
  else if DoesContain(gdDisparityIndex)       then
    Result := 'Disparity Index per site'
  else if DoesContain(gdDisparityIndexTest)   then
    Result := 'Disparity Index Test'
  else if DoesContain(gdRev) then  // GS - otherwise it goes to DistCorrectionName and caption expert displays 'Data Specific model', 8-23-2011
    Result := DistModelName(DistModel)
  else if DoesContain(gdOneNuc) then
    Result := Result + DistCorrectionName(DistCorrection)
  else if DoesContain(gdSynNonsyn) then
  begin
    Result := DistModelName(DistModel) ;
    //if Length(DistCorrectionName(DistCorrection)) > 0 then
      //Result := Result + ' ('+ DistCorrectionName(DistCorrection)+')';
  end
  else if DoesContain(gdAmino) then
    Result := Result + DistCorrectionName(DistCorrection);
end;

function TDistPack.ReadFromFile(var data: File):boolean;
var
  i: Integer;
  version: integer = -1;
begin
  result := true;
  try
    BlockRead(data, version, 4);

    for i := 0 to High(DistType) do
      BlockRead(data, DistType[i], SizeOf(TDistType));

    BlockRead(data, FGammaPara, SizeOf(FGammaPara));
    BlockRead(data, FInaRValue, SizeOf(FInaRValue));
    BlockRead(data, FBootReps, SizeOf(FBootReps));
    BlockRead(data, FRandSeed, SizeOf(FRandSeed));
    BlockRead(data, Where, SizeOf(Where));
  except
    on E:Exception do
    begin
      result := false;
    end;
  end;
end;

procedure TDistPack.WriteToFile(var data: File);
var
  i: integer;
begin
  i := 1;  //version
  BlockWrite(data, i, 4);

  for i := 0 to High(DistType) do
    BlockWrite(data, DistType[i], SizeOf(TDistType));

  BlockWrite(data, FGammaPara, SizeOf(FGammaPara));
  BlockWrite(data, FInaRValue, SizeOf(FInaRValue));
  BlockWrite(data, FBootReps, SizeOf(FBootReps));
  BlockWrite(data, FRandSeed, SizeOf(FRandSeed));
  BlockWrite(data, Where, SizeOf(Where));
end;

procedure TDistPack.Assign(Source: TDistPack);
var
  i: integer;
begin
  for i := 0 to High(DistType) do
    DistType[i] := Source.DistType[i];
  FGammaPara := Source.FGammaPara;
  FGammaCats := Source.FGammaCats;
  FInaRValue := Source.FInaRValue;
  FBootReps  := Source.FBootReps;
  FRandSeed  := Source.FRandSeed;
  Where      := Source.Where;
  FGeneticCode := Source.FGeneticCode;
end;

function TDistPack.GetGeneticCode: AnsiString;
begin
  if FGeneticCode <> EmptyStr then
    Result := FGeneticCode
  else
    Result := 'N/A';
end;

function TDistPack.StateToStringList(Comment: String=''): TStringList;
var
  MyStringList: TStringList;
  Count: Integer;
  i: Integer;
  DistTypeString: String;
begin
  MyStringList := TStringList.Create;
  if Comment <> EmptyStr then
    MyStringList.Add(Comment);
  MyStringList.Add('TDistPack=MyName');
  if BootReps > 0 then
    MyStringlist.Add('BootReps=' + IntToStr(BootReps));
  if RandSeed > 0 then
    MyStringlist.Add('RandSeed=' + IntToStr(RandSeed));
  if NoOfGammaCategories > 0 then
    MyStringlist.Add('NoOfGammaCategories=' + IntToStr(NoOfGammaCategories));
  if Where > 0 then
    MyStringlist.Add('Where=' + IntToStr(Where));

  if Acronym <> '' then
    MyStringlist.Add('Acronym=' + Acronym);
  if Name <> '' then
    MyStringlist.Add('Name=' + Name);
  if NameWithoutDistComponent <> '' then
    MyStringlist.Add('NameWithoutDistComponent=' + NameWithoutDistComponent);
  if SimpleName <> '' then
    MyStringlist.Add('SimpleName=' + SimpleName);
  if FullDescription <> '' then
    MyStringlist.Add('FullDescription=' + FullDescription);
  if MethodCitation <> '' then
    MyStringlist.Add('MethodCitation=' + MethodCitation);
  if PatternAssumption <> '' then
    MyStringlist.Add('PatternAssumption=' + PatternAssumption);
  if GeneticCode <> '' then
    MyStringlist.Add('[GeneticCode=' + GeneticCode);


  MyStringlist.Add('GammaParameter=' + FloatToStrF(GammaParameter, ffFixed, 2, 4));
  MyStringlist.Add('InaRValue=' + FloatToStrF(InaRValue, ffFixed, 2, 4));

  MyStringlist.Add('ComputeGammaParameter=' + BoolToStr(ComputeGammaParameter));
  MyStringlist.Add('ComputeInvarParameter=' + BoolToStr(ComputeInvarParameter));
  MyStringlist.Add('IsValid=' + BoolToStr(IsValid));

  MyStringlist.Add('ComputationType=' + GetEnumName(TypeInfo(TDistType), integer(ComputationType)));
  MyStringlist.Add('DistModel=' + GetEnumName(TypeInfo(TDistType), integer(DistModel)));
  MyStringlist.Add('DistCorrection=' + GetEnumName(TypeInfo(TDistType), integer(DistCorrection)));
  MyStringlist.Add('DistComponent=' + GetEnumName(TypeInfo(TDistType), integer(DistComponent)));
  MyStringlist.Add('VarType=' + GetEnumName(TypeInfo(TDistType), integer(VarType)));
  MyStringlist.Add('SelectionTestType=' + GetEnumName(TypeInfo(TDistType), integer(SelectionTestType)));
  MyStringlist.Add('BasicAnalysisType=' + GetEnumName(TypeInfo(TDistType), integer(BasicAnalysisType)));

  if Length(DistType) > 0 then
  begin
    Count := Length(DistType);
    if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      DistTypeString := GetEnumName(TypeInfo(TDistType), integer(DistType[i]));
      if DistTypeString <> 'gdNone' then
        MyStringlist.Add('DistType[' + IntToStr(i) + ']=' + DistTypeString);
    end;

  end;
  Result := MyStringList;
end;

end.

