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

unit MegaAnalysisPrefStrings;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  MegaConsts;

const
  NUM_TREES_SEARCHED = 'No. of Trees Searched';

  // suffix 1 means level 1 and 2 means level 2
  opsOperationType1     = 'Analysis'; // distance computation or NJ tree making; etc.
    opsScope2           = 'Scope';    // Scope in terms of over taxa or groups of taxa.
    opsSelHypoToTest2   = 'Test Hypothesis (HA: alternative)';
    opsTreeToUse2       = 'Tree to Use';    // NJ, user specified etc.
    opsPickUserTree2    = 'User Tree File'; // user selected tree file
    opsPhylo2           = 'Statistical Method'; // ML, OLS, MP, etc.
    opsMLBLenOptimize2  = 'Double Optimize Branch Lengths';
    opsAssumeMolClock2  = 'Assume Molecular Clock';
    opsEstRateVarAmongSitesML = 'Estimate Rate Variation among Sites (ML)';
    opsEstTsTvML        = 'Estimate Transition/Transversion Bias (ML)';
  opsApproach1          = 'Scope';
  opsApproachType2      = 'Approach';
    opsStandard2        = 'Standard';
    opsStandardSlow2    = 'Standard (slow)';
    opsSubsampleUpsample2 = 'Little Bootstraps (fast)';
    opsSubsampling2     = 'Subsampling (fast)';
    opsModelTamerFast2    = 'ModelTamer (fast)';
    opsModelSelFiltered2  = 'Filtered (medium)';
    opsModelSelFilteredFast2  = 'Filtered (fast)';
    opsModelSelFull2      = 'Full (slow)';
    opsSubsampleUpsamplePickList = '"' + opsStandardSlow2 + '",' +
                                   '"' + opsSubsampling2 + '"';
    opsModelSelectionApproachPickList = '"' + opsModelTamerFast2 + '",' +
                                        '"' + opsModelSelFiltered2 + '",' +
                                        '"' + opsModelSelFull2 + '"';
    opsModelSelectionApproachPickListMega12 = '"' + opsModelSelFull2 + '","' + opsModelSelFilteredFast2 + '"';
    opsAdvancedSettings = 'Advanced Settings';
    opsDeltaBicCutoff = 'BIC Threshold';
    opsDeltaAicCutoff = 'AICc Threshold';
    opsBootstrapPrecisionThreshold = 'Threshold';
  opsChooseThreeTaxaForClocks1 = 'Choose Taxa Triplet';
    opsIngroupTaxonA2   = 'Taxon A';  // Tajima's clock
    opsIngroupTaxonB2   = 'Taxon B';  // Tajima's clock
    opsOutgroupTaxonC2  = 'Outgroup'; // Tajima's clock
  opsDistanceEstimation1 = 'Distance Estimation';
  opsDiversityEstimation1 = 'Diversity Estimation';
  opsEstimateVar1       = 'Estimate Variance'; // variance
    opsEstimateVar2     = 'Variance Estimation Method';
  opsTestPhylo1         = 'Phylogeny Test';
    opsTestPhylo2       = 'Test of Phylogeny';
    opsMaxSampleSites   = 'Max No. of Bootstrap Sample Sites';
    opsSubSampleData    = 'Limit the No. of Sample Sites';
    opsBootstrapSubSamples   = 'Subsamples';  {if any of these bootstrap strings change, update the test harness as it uses the same strings}
    opsBootstrapRepsPerSample = 'Replicates/subsample';
    opsPercentSitesPerSample = 'Subsample size (%)';
    opsBootstrapMaxSitesPerSample = 'Max No. of Sites Per Sample';
    opsBoostrapSitesPerSample = 'No. of Sites per Sample';
    opsNumSitePatterns = 'No. of Site Patterns';
    opsNumSitePatternsSampled = 'No. of Site Patterns Sampled';
    opsNumEvaluationsPerformed = 'No. of Models Evaluated';
    opsBootstrapSampleSizeFactor = 'Sample Size Factor';
    opsEstimateDivergenceTimes = 'Estimate Divergence Times';
    opsBootReps2        = 'Bootstrap Replicates';
    opsMonteCarloReps2  = 'No. of Monte Carlo Replications';
    opsFocalSpecies2    = 'Focal Sequence';

  opsModelInfo1       = 'Substitution Model';
    opsPlus             = '====================';
    opsNucSynAminoType2 = 'Substitutions Type';  // Nuc/syn/amino
    opsGeneticCodeTable2     = 'Genetic Code Table';
    opsGeneticCode2     = 'Genetic Code';
    opsGeneticCodeTableSLAC     = 'Genetic Code Table';
    opsSubsModel2       = 'Model/Method';
    opsSubsToInclude2   = 'Substitutions to Include';

    opsFixedRValue2          = 'Fixed Transition/Transversion Ratio';

  opsRatesPatterns1          = 'Rates and Patterns';
    opsRatesAmongSites2      = 'Rates among Sites';
    opsGammaPara2            = 'Gamma Parameter';
    opsGammaCats2            = 'No of Discrete Gamma Categories';
    opsPatternAmongLineages2 = 'Pattern among Lineages';
    opsAminoAcidFreq2        = 'Amino Acid Frequencies';

  opsDataSubset1             = 'Data Subset to Use';
    opsGapMissingHandle2     = 'Gaps/Missing Data';
    opsSiteCoverage2         = 'Site Coverage Cutoff (%)';
    opsCodonPosPanel2        = 'Select Codon Positions';
    opsLabelledSites2        = 'Labelled Sites';
    opsSelectLabelsPanel2    = 'Labels to Include'; // Checkboxes
    opsRecodeBases2          = 'Recode Bases';      // Mapping (e.g., Atomic tree of life)

  opsTreeMethod1             = 'Tree Inference Options';
    opsMESearchMethod2       = 'ME Heuristic Method';
    opsMESearchLevelPanel2   = 'ME Search Level';
    opsMEInitialTrees2       = 'Initial Tree for ME';

    opsMPSearchMethod2       = 'MP Search Method';
    opsMinMiniSearchFactor2  = 'Search Factor (Min-mini)';
    opsMPSearchLevelPanel2   = 'MP Search level';
    opsMPRandomAddTreesPanel2= 'No. of Initial Trees (random addition)';
    opsMaxTrees2             = 'Max No. of Trees to Retain';
    opsCalcBlens2            = 'Calculate Branch Lengths';

    opsMLSearchMethod2       = 'ML Heuristic Method';
    opsMLInitialTrees2       = 'Initial Tree for ML';
    opsPickStartTree2        = 'Initial Tree File';
    opsMLSearchLevelPanel2   = 'ML Search Level';
    opsSystemResourceUsage   = 'System Resource Usage';
    opsMLNumThreads          = 'Number of Threads';
    opsMaxExecutionTime2     = 'Maximum Execution Time';
    opsHasTimeLimit          = 'Has Time Limit';
    opsDataOrientation       = 'Data Orientation';
    opsTransposeData2        = 'Transpose Data';
    opsKeepUserTreeBLens2    = 'Keep user tree branch lengths';
    opsKeepUserTreeTimes2    = 'Use times from tree file';

  // clock stuff
    opsClockSettings1        = 'Reltime Settings';
      opsClockLevel2         = 'Clock Stringency';  // Only 1SE, 2SE, 3SE
      opsClockType2          = 'Clock Type';
      opsGlobalClockLevel2   = 'Clock Stringency Type';
      opsCalibrationFile2    = 'Calibrations File (optional)';
      opsMaxRateRatio2        = 'Max Relative Rate Ratio';

  // BEAM
    opsBeamSettings         = 'Max Sequence Similarity Cutoff';
    opsImputeCutoff2        = '% SNVs for Identical sequences';

  // Branch length filter (used during searching of ML trees)
    opsSearchFilter2          = 'Branch Swap Filter';
    opsRelTimeDoOptimize2    = 'Optimize Branch Lengths';

    opsGapOpen               = 'Gap Open';
    opsGapExtend             = 'Gap Extend';
    opsMaxMem                = 'Max Memory in MB';
    opsMaxIters              = 'Max Iterations';
    opsCluster1              = 'Clustering Method (Iteration 1,2)';
    opsCluster2              = 'Clustering Method (Other Iterations)';
    opsLambda                = 'Min Diag Length (lambda)';
    opsHydrophobicity        = 'Hydrophobicity Multiplier';

const
  MultithreadNotAvailableStr = 'Multithreading restricted to single ML analysis';
  NAStr = 'n/a';
  NotApplicableStr  = 'Not Applicable';
  NotSelectedStr    = 'Not Selected';
  NJTreeStr         = 'Automatic (Neighbor-joining tree)' ;
  UserSpecifyFromFile = 'Use tree from file';
  UserSpecifyManager = 'Use Topology Editor';
  NoSpeciesTree = 'Do not use species tree';

  UserTreeList      = '"'+UserSpecifyFromFile+'",'+
                      '"'+UserSpecifyManager+'"';
  UserTreeListForPrototyper = '"'+UserSpecifyFromFile+'"';
  TreeToUsePickList = '"'+NJTreeStr+'",'+UserTreeList;
  TreeToUsePickListForPrototyper = '"'+NJTreeStr+'",'+UserTreeListForPrototyper;

  YesStr = 'Yes';
  NoStr = 'No';
  KeepUserTreeBLensPickList = '"Yes","No"';
  YesNoPickList         = '"No", "Yes"';

  ClickToPickTreeStr = 'Click here to pick a tree file';

  LbsTreeConstruction = 'LBS tree construction';
  LbsTimingEstimation = 'SU timing estimation';
  LbsBranchLengthEstimation = 'SU blens estimation';
  opsLbsTargetPrecisionStdDev = 'Target Precision';
  FastStr = 'Fast';
  opsSubsamplingVerifyResult = 'Verify Result';

  opsLbsSkipTreeSwaps = 'Skip Optimal Replicate Check';

  TargetPrecStdDevPickList = '"1",' +
                             '"2.5"';

  AutomaticStr = 'Automatic';
  AdaptiveStr = 'Adaptive';
  UserDefinedStr = 'User-defined';
  LbsParamOptimizationPickList = '"' + AutomaticStr + '",' +
                                 '"' + UserDefinedStr + '"';
  LbsParamSettingPickList = '"' + AdaptiveStr + '",' +
                            '"' + UserDefinedStr + '"';
  NoneStr           = 'None';
  IntBranchTestStr  = 'Interior-branch test';
  BootTestStr       = 'Bootstrap method';
  BootTestSlowStr   = 'Standard Bootstrap (slow)';
  BootTestAdaptiveStr = 'Adaptive Bootstrap (medium)';
  BootTestAdaptiveStrFast = 'Adaptive Bootstrap (fast)';
  AnalTestStr       = 'Analytical method';

  BootstrapVarStr = 'Bootstrap';
  RateVarStr = 'Rates';
  SampleVarStr = 'Sampling';
  RateAndSampleVarStr = 'Rates and Sampling';

  ReltimeVarPickList = '"' + NoneStr + '",' +
                       '"' + BootstrapVarStr + '",'+
                       '"' + RateVarStr + '",' +
                       '"' + SampleVarStr + '",' +
                       '"' + RateAndSampleVarStr + '"';

  TestBootPickList          = '"'+ NoneStr     +'",' +
                              '"'+ BootTestStr +'"';

  StdAdaptiveBootPickList       = '"' + NoneStr + '",' +
                                  '"' + BootTestSlowStr + '",' +
                                  '"' + BootTestAdaptiveStrFast + '"';

  LittleBootPickList       = '"' + NoneStr + '",' +
                             '"' + BootTestSlowStr + '",' +
                             '"' + BootTestAdaptiveStr + '",' +
                             '"' + opsSubsampleUpsample2 + '"';

  LittleBootUserTreePickList = '"' + NoneStr + '",' +
                             '"' + opsSubsampleUpsample2 + '"';

  TestBootIntBranchPickList = '"'+ NoneStr     +'",' +
                              '"'+ BootTestStr +'",'+
                              '"'+ IntBranchTestStr+'"';

  StdErrBootPickList        = '"'+ NoneStr     +'",'+
                              '"'+ BootTestStr +'"';

  StdErrBootAnalPickList    = '"'+ NoneStr      +'",'+
                              '"'+ BootTestStr  +'",'+
                              '"'+ AnalTestStr  +'"';

  StdErrBootAnalReqPickList = '"'+ BootTestStr  +'",'+
                              '"'+ AnalTestStr  +'"';

  BootAndIntBranchPickList  = '"'+ BootTestStr      +'",'+
                              '"'+ IntBranchTestStr +'"';

  StdErrLbsAnalPickList     = '"' + AnalTestStr + '",' +
                              '"' + opsSubsampling2 + '"';
  NucStr = 'Nucleotide';
  SynStr = 'Syn-Nonsynonymous';
  AminoStr = 'Amino acid';
  CodonStr = 'Codon';
  SnvStr = 'SNV';
  NucSynAminoPickList = '"'+NucStr    +'",' +
                        '"'+SynStr    +'",' +
                        '"'+AminoStr  +'"';

  NucAminoPickList    = '"'+NucStr    +'",' +
                        '"'+AminoStr  +'"';

  NucAminoCodonPickList =  '"'+NucStr    +'",' +
                        '"'+AminoStr  +'",' +
                        '"'+CodonStr + '"';

 GeneticCodeTablePickList =   '"'+'Standard'    +'",' +
                              '"'+'Vertebrate Mitochondrial'    +'",' +
                              '"'+'Invertebrate Mitochondrial'    +'",' +
                              '"'+'Yeast Mitochondrial'              +'",' +
                              '"'+'Mold Mitochondrial'  +'",' +
                              '"'+'Protozoan Mitochondrial'     +'",' +
                              '"'+'Coelenterate Mitochondrial'     +'",' +
                              '"'+'Mycoplasma'    +'",' +
                              '"'+'Spiroplasma'    +'",' +
                              '"'+'Ciliate Nuclear'             +'",' +
                              '"'+'Dasycladacean Nuclear'  +'",' +
                              '"'+'Hexamita Nuclear'             +'",' +
                              '"'+'Echinoderm Mitochondrial'  +'",' +
                              '"'+'Euplotid Nuclear'    +'",' +
                              '"'+'Bacterial Plastid'   +'",' +
                              '"'+'Plant Plastid'   +'",' +
                              '"'+'Alternative Yeast Nuclear'            +'",' +
                              '"'+'Ascidian Mitochondrial' +'",' +
                              '"'+'Flatworm Mitochondrial'  +'",' +
                              '"'+'Blepharisma Macronuclear'              +'",' +
                              '"'+'Chlorophycean Mitochondrial'  +'",' +
                              '"'+'Trematode Mitochondrial'    +'",' +
                              '"'+'Scenedesmus obliquus Mitochondrial'    +'",' +
                              '"'+'Thraustochytrium Mitochondrial'             +'"';
  GeneticCodeTableCodonOmegaList =   '"'+'Universal code'    +'",' +
                                    '"'+'Vertebrate mitochondrial DNA code'    +'",' +
                                    '"'+'Invertebrate mitochondrial DNA code'    +'",' +
                                    '"'+'Yeast mitochondrial DNA code'              +'",' +
                                    '"'+'Mold, Protozoan and Coelenterate mt; Mycloplasma/Spiroplasma'  +'",' +
                                    '"'+'Ciliate, Dasycladacean and Hexamita Nuclear code'             +'",' +
                                    '"'+'Echinoderm mitochondrial DNA code'  +'",' +
                                    '"'+'Euplotid Nuclear code'    +'",' +
                                    '"'+'Alternative Yeast Nuclear code'            +'",' +
                                    '"'+'Ascidian mitochondrial DNA code' +'",' +
                                    '"'+'Flatworm mitochondrial DNA code'  +'",' +
                                    '"'+'Blepharisma Nuclear code'              +'"';

 RateAmongSitesPickList1 =     '"'+'Uniform Rates'  +'",' +
                              '"'+'Gamma Distributed (G)'    +'",' +
                              '"'+ 'Has Invariant Sites (I)'    +'",' +
                              '"'+'Gamma Distributed With Invariant Sites (G+I)'             +'"';




  // Distance methods
  Model_NoOfDiffStr         = 'No. of differences';
  Model_p_distanceStr       = 'p-distance';
  Model_Jukes_CantorStr     = 'Jukes-Cantor model';
  Model_Kimura_2_ParaStr    = 'Kimura 2-parameter model';
  Model_Tajima_NeiStr       = 'Tajima-Nei model';
  Model_FelsensteinStr      = 'Felsenstein 1981 model';
  Model_Tamura_3_ParaStr    = 'Tamura 3-parameter model';
  Model_HKYStr              = 'Hasegawa-Kishino-Yano model';
  Model_Tamura_Nei_ParaStr  = 'Tamura-Nei model';
  Model_MCLStr              = 'Maximum Composite Likelihood';
  Model_GTRStr              = 'General Time Reversible model';
  Model_LogDet_TKStr        = 'LogDet (Tamura-Kumar)';

  Model_NeiGojoboriNoOfDiffStr       = 'Nei-Gojobori method (No. of Differences)';
  Model_NeiGojoboriPDistStr          = 'Nei-Gojobori method (Proportion)';
  Model_NeiGojoboriJCDistStr         = 'Nei-Gojobori method (Jukes-Cantor)';
  Model_ModifiedNeiGojoboriPDistStr  = 'Modified Nei-Gojobori method (Proportion)';
  Model_ModifiedNeiGojoboriJCDistStr = 'Modified Nei-Gojobori method (Jukes-Cantor)';
  Model_Li_Wu_LuoStr                 = 'Li-Wu-Luo method (Kimura 2-para)';
  Model_Pamilo_Bianchi_LiStr         = 'Pamilo-Bianchi-Li method (Kimura 2-para)';
  Model_KumarStr                     = 'Kumar method (Kimura 2-para)';

  Model_PoissonStr             = 'Poisson model';
  Model_EqualInputStr          = 'Equal input model';
  Model_DayhoffStr             = 'Dayhoff model';
  Model_DayhoffPiStr           = 'Dayhoff model with Freqs. (+F)';
  Model_JTTStr                 = 'Jones-Taylor-Thornton (JTT) model';
  Model_JTTPiStr               = 'JTT with Freqs. (+F) model';
  Model_WAGStr                 = 'WAG model';
  Model_WAGPiStr               = 'WAG with Freqs. (+F) model';
  Model_LGStr                  = 'LG model';
  Model_LGPiStr                = 'LG with Freqs. (+F) model';
  Model_mtREV24Str             = 'General Reversible Mitochondrial (mtREV)';
  Model_mtREV24PiStr           = 'mtREV with Freqs. (+F) model';
  Model_cpREVStr               = 'General Reversible Chloroplast (cpREV)';
  Model_cpREVPiStr             = 'cpREV with Freqs. (+F) model';
  Model_rtREVStr               = 'General Reverse Transcriptase model (rtREV)';
  Model_rtREVPiStr             = 'rtREV with Freqs. (+F) model';

  Model_Allele_Freq_Dist       = 'Allele Frequency Distance';

  TempPickList    =       '"tempItem1",'+
                          '"tempItem2",';
  // Dist Nuc pick list
  DistNucPickList =       '"'+Model_NoOfDiffStr         +'",' +
                          '"'+Model_p_distanceStr       +'",' +
                          '"'+Model_Jukes_CantorStr     +'",' +
                          '"'+Model_Kimura_2_ParaStr    +'",' +
                          '"'+Model_Tajima_NeiStr       +'",' +
                          '"'+Model_Tamura_3_ParaStr    +'",' +
                          '"'+Model_Tamura_Nei_ParaStr  +'",' +
                          '"'+Model_MCLStr              +'",' +
                          '"'+Model_LogDet_TKStr        +'"';

  MLNucTsTvPickList =     '"'+Model_Kimura_2_ParaStr    +'",' +
                          '"'+Model_Tamura_3_ParaStr    +'",' +
                          '"'+Model_HKYStr              +'",' +
                          '"'+Model_Tamura_Nei_ParaStr  +'",' +
                          '"'+Model_GTRStr              +'"';

  MLNucPickList   =       '"'+Model_Jukes_CantorStr     +'",' +
                          '"'+Model_Kimura_2_ParaStr    +'",' +
                          '"'+Model_Tamura_3_ParaStr    +'",' +
                          '"'+Model_HKYStr              +'",' +
                          '"'+Model_Tamura_Nei_ParaStr  +'",' +
                          '"'+Model_GTRStr              +'"';

  MLHyPhyPickList   =     '"'+Model_FelsensteinStr      +'",' +
                          '"'+Model_HKYStr              +'",' +
                          '"'+Model_Tamura_Nei_ParaStr  +'",' +
                          '"'+Model_GTRStr              +'"';

  DistSynNonsynPickList = '"'+Model_NeiGojoboriNoOfDiffStr        +'",' +
                          '"'+Model_NeiGojoboriPDistStr           +'",' +
                          '"'+Model_NeiGojoboriJCDistStr          +'",' +
                          '"'+Model_ModifiedNeiGojoboriPDistStr   +'",' +
                          '"'+Model_ModifiedNeiGojoboriJCDistStr  +'",' +
                          '"'+Model_Li_Wu_LuoStr                  +'",' +
                          '"'+Model_Pamilo_Bianchi_LiStr          +'",' +
                          '"'+Model_KumarStr                      +'"';


  DistFisherSelTestModelPickList = '"'+Model_NeiGojoboriPDistStr        +'",' +
                                   '"'+Model_ModifiedNeiGojoboriPDistStr +'"';

  DistSelTestModelPickList =      '"'+Model_NeiGojoboriPDistStr           +'",' +
                                  '"'+Model_NeiGojoboriJCDistStr          +'",' +
                                  '"'+Model_ModifiedNeiGojoboriPDistStr   +'",' +
                                  '"'+Model_ModifiedNeiGojoboriJCDistStr  +'",' +
                                  '"'+Model_Li_Wu_LuoStr                  +'",' +
                                  '"'+Model_Pamilo_Bianchi_LiStr          +'",' +
                                  '"'+Model_KumarStr                      +'"';

  DistAminoPickList=      '"'+Model_NoOfDiffStr    +'",' +
                          '"'+Model_p_distanceStr  +'",' +
                          '"'+Model_PoissonStr     +'",' +
                          '"'+Model_EqualInputStr  +'",' +
                          '"'+Model_DayhoffStr     +'",' +
                          '"'+Model_JTTStr         +'"'   // bugfix, by removing ',' used to have an additional empty row.
//                          +
//                          '"'+Model_Allele_Freq_Dist +'"'
                          ;

  MLAminoPickList  =      '"'+Model_PoissonStr    +'",' +
                          '"'+Model_EqualInputStr +'",' +
                          '"'+Model_DayhoffStr    +'",' +
                          '"'+Model_DayhoffPiStr  +'",' +
                          '"'+Model_JTTStr        +'",' +
                          '"'+Model_JTTPiStr      +'",' +
                          '"'+Model_WAGStr        +'",' +
                          '"'+Model_WAGPiStr      +'",' +
                          '"'+Model_LGStr         +'",' +
                          '"'+Model_LGPiStr       +'",' +
                          '"'+Model_mtREV24Str    +'",' +
                          '"'+Model_mtREV24PiStr  +'",' +
                          '"'+Model_cpREVStr      +'",' +
                          '"'+Model_cpREVPiStr    +'",' +
                          '"'+Model_rtREVStr      +'",' +
                          '"'+Model_rtREVPiStr    +'"';

   // PickList strings for substitution types
  AllStr                  = 'All';
  NucTsOnlyPickStr        = 's: Transitions only';
  NucTvOnlyPickStr        = 'v: Transversions only';
  NucRatioTsTvPickStr     = 'R = s/v';
  SynOnlyPickStr          = 's: Synonymous only';
  NonsynOnlyPickStr       = 'n: Nonsynonymous only';
  DiffSynNonsynPickStr    = 'Difference (s - n)';
  DiffNonsynSynPickStr    = 'Difference (n - s)';
  Syn4FoldPickStr         = 'At 4-fold degenerate sites only';
  Nonsyn0FoldPickStr      = 'At 0-fold degenerate sites only';

  NucTsTvSeparatePickStr  = 'Use s and v separately';

  NoOfSynSitesPickStr     = 'S: No. of synonymous sites';
  NoOfNonsynSitesPickStr  = 'N: No. of nonsynonymous sites';
  ValidCommonSitesPickStr = 'L: No. of Valid Common Sites';
  NoOf4FoldSitesPickStr   = 'No. of 4-fold degenerate sites';
  NoOf0FoldSitesPickStr   = 'No. of 0-fold degenerate sites';

  // Allele frequency substitution types
  BhattacharyaPickStr      = 'Bhattacharya Angular (1946) Distance';
  PrevostiPickStr          = 'Provosti (1975) Distance';
  RogersPickStr            = 'Rogers (1972) Distance';
  Nei1983PickStr           = 'Nei (1983)';
  KumarGadagkarPickStr     = 'Kumar-Gadagkar (2001) Distance';
  KumarGadagkarDisparityPickStr= 'Kumar-Gadagkar (2001) Disparity Index';

  //AllelFrequencyDistance
  AlleleFreqDistPickList   = '"'+KumarGadagkarPickStr+'",'+
                             '"'+RogersPickStr+'",'+
                             '"'+BhattacharyaPickStr+'",'+
                             '"'+Nei1983PickStr+'",'+
                             '"'+KumarGadagkarDisparityPickStr+'",'+
                             '"'+PrevostiPickStr+'"';

  ActualFromDataPickStr    = 'Actual from Data (+F)';
  ExpectedFromModelPickStr = 'Expected from Model';
  AminoAcidFreqPickList    = '"'+ActualFromDataPickStr+'",'+
                             '"'+ExpectedFromModelPickStr+'"';

  // Recoding strings
  AtomicNoRecodingPickStr  =    'None';
  AtomicNvsNonNPickStr     =    'Side-chain N-content: Binary (WQNKHR|ACDEFGILMPSTVY)';
  AtomicSvsNonSPickStr     =    'Side-chain S-content: Binary (MC|ADEFGHIKLNPQRSTVWY)';
  AtomicOvsNonOPickStr     =    'Side-chain O-content: Binary (STYQNDE|ACFGHIKLMPRWY)';
  AtomicNmultiallelePickStr=    'Side-chain N-content: Multiallelic (WQN|KH|R|ACDEFGILMPSTVY)';
  AtomicOmultiallelePicStr =    'Side-chain O-content: Multiallelic (STYQN|DE|ACFGHIKLMPRWY)';
  AtomicONSBinaryPickStr   =    'Side-chain ONS-content: Binary (WQNKHRMCSTYDE|AFGILPV)';
  AtomicONSmultiallelePickStr=  'Side-chain ONS-content: Multiallelic(W|KH|R|STY|QN|DE|MC|AFGILPV)';
  RecodeBasesPickList      = '"'+AtomicNoRecodingPickStr+'",'+
                             '"'+AtomicNvsNonNPickStr+'",'+
                             '"'+AtomicSvsNonSPickStr+'",'+
                             '"'+AtomicOvsNonOPickStr+'",'+
                             '"'+AtomicNmultiallelePickStr+'",'+
                             '"'+AtomicOmultiallelePicStr+'",'+
                             '"'+AtomicONSBinaryPickStr+'",'+
                             '"'+AtomicONSmultiallelePickStr+'",';

  // PickList strings for other options
  HomoPatternPickStr      = 'Same (Homogeneous)';
  HeteroPatternPickStr    = 'Different (Heterogeneous)';
  UniformRatePickStr      = 'Uniform Rates';
  GammaRatePickStr        = 'Gamma Distributed (G)';
  GammaInvRatePickStr     = 'Gamma Distributed With Invariant Sites (G+I)';
  InvariantPickStr        = 'Has Invariant Sites (I)';

  DistOnlyPickStr         = 'Distances only';
  DistAndSEPickStr        = 'Distances & Std. Err.';

  CompleteDelStr = 'Complete deletion';
  PairwiseDelStr = 'Pairwise deletion';
  PartialDelStr  = 'Partial deletion';
  UseAllStr      = 'Use all sites';

  CompPairwisePartialDelPickList = '"' + CompleteDelStr + '",' +
                                   '"' + PairwiseDelStr + '",' +
                                   '"' + PartialDelStr  + '"';
  CompAllPartialDelPickList      = '"' + CompleteDelStr + '",' +
                                   '"' + UseAllStr      + '",' +
                                   '"' + PartialDelStr  + '"';

  CompleteDeletionPickList       = '"' + CompleteDelStr + '"';
  UseAllSitesPickList            = '"' + UseAllStr + '"';

  // Select Test pick str
  NeutralityPickStr       = 'Neutrality (HA: dN =/= dS)';
  PositiveSelPickStr      = 'Positive selection (HA: dN  > dS)';
  PurifyingSelPicStr      = 'Purifying selection (HA: dN < dS)';

  // Averaging
  SelInSeqPairsPickStr    = 'In Sequence Pairs';
  SelGpAvgPickStr         = 'Average within Groups';
  SelOverallAvgStr        = 'Overall Average';

  //Standard Computation
  NonePickStr             = 'None';
  BootstrapPickStr        = 'Bootstrap';
  AnalyticalPickStr       = 'Analytical';
  InteriorPickStr         = 'Interior Branch Test';

  // Disp Index
  CompositionDistancePickStr = 'Dc: Composition Distance';
  DisparityIndexPickStr   = 'ID: Disparity Index';
  TestPatternHomology     = 'Conduct ID-Test';
  DisparityIndexPickList = '"'+CompositionDistancePickStr+'",'+
                           '"'+DisparityIndexPickStr+'",'+
                           '"'+TestPatternHomology+'",';

  // Tree making for  Selected Groups or sequences
  AllSelectedTaxaStr       = 'All Selected Taxa';
  AllSelectedGroupsStr     = 'All Groups';
  DistTreeTaxaPickList     = '"' + AllSelectedTaxaStr +'",' +
                             '"' + AllSelectedGroupsStr +'"';
  // ML, ME & MP
  MaxMiniStr = 'Max-mini Branch-&-bound';
  MinMiniStr = 'Min-Mini Heuristic';
  SPRStr = 'Subtree-Pruning-Regrafting (SPR)';
  TBRStr = 'Tree-Bisection-Reconnection (TBR)';
  SPRFastStr = 'Subtree-Pruning-Regrafting - Fast (SPR level 3)'; // GS - added in MEGA_5.1 for SPR heuristic, 7-14-2011
  SPRExtensiveStr = 'Subtree-Pruning-Regrafting - Extensive (SPR level 5)';
  CNIStr = 'Close-Neighbor-Interchange (CNI)';
  NNIStr = 'Nearest-Neighbor-Interchange (NNI)';
  ObtainInitialByMEStr ='Obtain initial tree by ME';
  ObtainInitialByNJStr ='Obtain initial tree by Neighbor-Joining';


  // Values for the drop down list for selecting an auto tree making method for ML inference
  InitialTreeByNJStr = 'Make initial tree automatically (Neighbor Joining)';
  InitialTreeByBioNJStr = 'Make initial tree automatically (BioNJ)';
  InitialTreeByDefaultStr = 'Make initial tree automatically (Default - NJ/MP)';
  InitialTreeByParsimonyStr = 'Make initial tree automatically (Maximum Parsimony)';
  InitialParsimonyTreesStr = 'Make multiple initial trees automatically (Maximum Parsimony)';
  MakeInitialAutoStr =
              InitialTreeByDefaultStr + '",' +
        '"' + InitialTreeByParsimonyStr + '",' +
        '"' + InitialParsimonyTreesStr + '",' +
        '"' + InitialTreeByNJStr; //+ '",' +
//        '"' + InitialTreeByBioNJStr;

  MakeInitialAutoStrNoMP =
              InitialTreeByNJStr;
{
              InitialTreeByDefaultStr + '",' +
        '"' + InitialTreeByNJStr + '",' +
        '"' + InitialTreeByBioNJStr;
}
  RandomAdditionTreesStr = 'Close-Neighbor-Interchange (CNI) on Random Trees';
  ObtainNJStr = 'Obtain by Neighbor-Joining Method';
  StartWithUserTreeStr = 'Start with a user tree';
  opsNumInitialTrees2 = 'No. of Initial Trees';

  //Clocks
  AllClocksStr = 'All clocks (do not merge clock rates)';
  ManyClocksStr = 'Many clocks (merge clock rates on 1 StdError)';
  FewClocksStr = 'Few clocks (merge clock rates on 2 StdErrors)';
  FewestClocksStr = 'Fewer clocks (merge clock rates on 3 StdErrors)';
  GlobalClockStr = 'Global clocks';
  RelativeRatesStr = 'Relative Rates';
  RRFrameworkStr = 'Relative Rate Framework';

  NoSearchFilterStr = 'None';
  VeryStrongStr = 'Very Strong';
  StrongStr     = 'Strong';
  ModerateStr   = 'Moderate';
  WeakStr       = 'Weak';
  VeryWeakStr   = 'Very Weak';

  YesSlowStr = 'Yes (more accurate)';
  NoFastStr  = 'No (faster)';

  YesFastStr = 'Yes (faster)';
  NoSlowStr = 'No (slower)';
  YesNoFastSlowPickList = '"' + NoFastStr + '",' +
                          '"' + YesSlowStr + '"';

  YesFastNoSlowPickList = '"' + YesFastStr + '",' +
                          '"' + NoSlowStr + '"';

  MolClockPickList = '"' + NoStr +'",' +
                     '"' + YesStr +'"';

  // MLBlenPickList  =  '"' + YesSlowStr + '",' +
  //                   '"' + NoFastStr +'"';

  MLSearchPickList = '"' + NNIStr + '",' +
                     //'"' + CNIStr + '",' + // GS - no longer supported for ML inference as of MEGA_5.1
                     '"' + SPRFastStr + '",' +        // GS - added for MEGA_5.1, 7-14-2011
                     '"' + SPRExtensiveStr + '"';

  MLInitialTreesPickList = '"' + MakeInitialAutoStr +'",' + UserTreeList;
  MLInitialTreesPickListNoMP = '"' + MakeInitialAutoStrNoMP +'",' + UserTreeList;

  MLInitialTreesPickListPrototyper = '"' + MakeInitialAutoStr + '","' + UserSpecifyFromFile + '"';

  MPSearchPickList = {'"' + RandomAdditionTreesStr +'",' +}    // CNI should be the default.
                     '"' + SPRStr + '",' +
                     '"' + TBRStr + '",' +
                     '"' + MinMiniStr +'",' +
                     '"' + MaxminiStr +'"';

  SearchFilterPickList = '"' + NoSearchFilterStr + '",' +
  //                       '"' + VeryWeakStr   + '",' +
                         '"' + WeakStr       + '",' +
                         '"' + ModerateStr   + '",' +
                         '"' + StrongStr     + '"';//'",' +
 //                        '"' + VeryStrongStr + '"';

  TrueFalsePickList =  '"' + 'True' + '",' +
                              '"' + 'False' + '"';

  ClockLvlPickList = '"' + AllClocksStr + '",' +
                     '"' + ManyClocksStr + '",' +
                     '"' + FewClocksStr + '",' +
                     '"' + FewestClocksStr + '"';

  GlobalClockLvlPickList = '"' + GlobalClockStr + '",' +
                           ClockLvlPickList;

  AllSitesStr            = 'All Sites';
  OnlyLabeledSitesStr    = 'Sites with Selected Labels';
  OnlyUnlabelledSitesStr = 'Unlabelled Sites';
  LabelledSitesPickList  = '"'+AllSitesStr+'",'+
                          '"'+OnlyLabeledSitesStr+'",'+
                          '"'+OnlyUnlabelledSitesStr+'"';

  StdErrComputationPickList        = '"'+ BootTestStr +'",'+
                                     '"'+ AnalTestStr+'"';
  StdErrorComputationTreePickList  = '"'+ NonePickStr      +'",'+
                                     '"'+ BootTestStr +'",'+
                                     '"'+ InteriorPickStr  +'"';
  //AllTreePickList = AllStr;   // These should not be picklists since they only have 1 option. They WILL be colored incorrectly.
  //AllPickList     = AllStr;
  TsTvTreePickList= '"d: Transitions + Transversions",'+
                    '"'+NucTsOnlyPickStr+'",'+
                    '"'+NucTvOnlyPickStr+'"';
  TsTvPickList    =     TsTvTreePickList +','+
                    '"'+NucRatioTsTvPickStr+'"';
  NGTreePickList      = '"'+SynOnlyPickStr+'",'+
                        '"'+NonsynOnlyPickStr+'"';
  NG_NoOfDiffPickList =     NGTreePickList;
  NGPickList          =     NGTreePickList+','+
                        '"'+DiffSynNonsynPickStr+'",'+
                        '"'+DiffNonsynSynPickStr+'"';
  LwlTreePickList =     NGTreePIckList+','+
                    '"'+Syn4FoldPickStr+'",'+
                    '"'+Nonsyn0FoldPickStr+'"';
  LwlPickList     =     LWLTreePickList+ ','+
                    '"'+DiffSynNonsynPickStr+'",'+
                    '"'+DiffNonsynSynPickStr+'"';

  TajimaClockTsTvPickList = '"'+AllStr+'",'+
                            '"'+NucTsOnlyPickStr+'",'+
                            '"'+NucTvOnlyPickStr+'",'+
                            '"'+NucTsTvSeparatePickStr+'"';

  // Other picklists
  PatternAmongLineagePickList = '"'+HomoPatternPickStr+'",'+
                                '"'+HeteroPatternPickStr+'"';
  RateAmonSitesUniformOnlyPickList = '"' + UniformRatePickStr + '"';
  RateAmongSitesPickList      = '"'+UniformRatePickStr+'",'+
                                '"'+GammaRatePickStr+'"';
  RateAmongSitesGammaPickList = '"'+UniformRatePickStr  +'",'+
                                '"'+GammaRatePickStr    +'",'+
                                '"'+InvariantPickStr    +'",'+
                                '"'+GammaInvRatePickStr + '"';
  RateAmongSitesNoUniformPickList = '"'+GammaRatePickStr    +'",'+
                                    '"'+InvariantPickStr +'",'+
                                    '"'+GammaInvRatePickStr +'"';

  ComputeDistOptPickList      = '"'+DistOnlyPickStr+'",'+
                                '"'+DistAndSEPickStr+'"';

  SelTestHypoPickList         = '"'+NeutralityPickStr+'",'+
                                '"'+PositiveSelPickStr+'",'+
                                '"'+PurifyingSelPicStr+'"';
  SelWithNoGpPickList         = '"'+SelInSeqPairsPickStr+'",'+
                                '"'+SelOverallAvgStr+'"';
  SelWithGpPickList           = '"'+SelInSeqPairsPickStr+'",'+
                                '"'+SelGpAvgPickStr+'",'+
                                '"'+SelOverallAvgStr+'"';

  // strings for the combo boxes used in the Data Settings page in AnalysisPrefDlg
  // when isPrototyper = true
  NucleotidePickStr = 'Nucleotide (Non-coding)';
  NucleotideCodingPickStr = 'Nucleotide(Coding)';
  AminoAcidPickStr = 'Amino Acid';
  DistanceMatrixPickStr = 'Distance Matrix (Mega format)';
  QuestionMarkPickStr = '?';
  HyphenPickStr = '-';
  PeriodPickStr = '.';
  NucleotidePickStrList: array[0..3] of String = (NucleotidePickStr, NucleotideCodingPickStr, AminoAcidPickStr, DistanceMatrixPickStr);
  SpecialSymbolPickStrList: array[0..2] of String = (QuestionMarkPickStr, PeriodPickStr, HyphenPickStr);
implementation

end.
