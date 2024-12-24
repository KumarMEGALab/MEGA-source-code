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

Unit MegaConsts;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF VISUAL_BUILD}
  Graphics, Forms, fgl, menus,
  {$ENDIF}
  LCLIntf, LCLType, Classes, SysUtils, syncobjs;

const
  DEVELOPER_STR = 'Developer';
  UNKNOWN_USER_TYPE = 'Unknown';

var
  CURRENT_PROCESS_ID: Integer = -1;
  DEV_LOG_MARKER: Integer = 0;
  DEV_LOG_INDENT: String = '';

  {$IFDEF DEBUG}
  USER_TYPE: String = DEVELOPER_STR;
  {$ELSE}
  USER_TYPE: String = UNKNOWN_USER_TYPE;
  {$ENDIF}

  SEQDATA: String = 'seqdata';
  DISTDATA: String = 'distdata';


const

  STUDENT_STR = 'Student';
  INSTRUCTOR_STR = 'Instructor';
  RESEARCHER_STR = 'Researcher';
  PROFESSOR_STR = 'Professor';
  OTHER_STR = 'Other';
  BUILD_STR = 'Build';
  VERSION_STR = 'Version';
  INTERFACE_STR = 'User Interface';
  OS_STR = 'Operating System';
  OS_PLATFORM_STR = 'Platform';
  DATA_TYPE_STR = 'Data Type';
  NUM_TAXA_STR = 'No. of Taxa';
  NUM_SITES_STR = 'No. of Sites';
  NUM_GROUPS_STR = 'No. of Groups';
  NUM_DOMAINS_STR = 'No. of Domains';
  CODE_TABLE_STR = 'Code Table';
  APPLICATION_INFO = '[Application Info]';
  TECHNICAL_INFO = '[Technical Info]';
  GENERAL_INFO = '[General Info]';
  ANALYSIS_SETTINGS = '[Analysis Settings]';
  ANALYSIS_STATS = '[Analysis Statistics]';


  CURSOR_COMPRESS = 1;
  CURSOR_FLIP = 2;
  CURSOR_FLIPALL = 3;
  CURSOR_ROOT = 4;
  CURSOR_ZOOM = 5;
  CURSOR_DRAG_BRANCH = 6;
  CURSOR_DROP_BRANCH = 7;

  ENTERRESIZEMODE_IMAGE = 151;
  QUITRESIZEMODE_IMAGE = 152;

  KEEP_BOTH_MODELS = 0;
  KEEP_BASE_MODEL = 1;
  KEEP_FREQUENCY_MODEL = 2;
  DEFAULT_BIC_THRESHOLD = 5;
  DEFAULT_AICC_THRESHOLD = 5;

  FASTA_FILES = 'FASTA_FILES';
  ALIGNMENT_DOMAINS = 'ALIGNMENT_DOMAINS';
  ALIGNMENT_SEGMENTS = 'ALIGNMENT_SEGMENTS';
  UNDEFINED = 'UNDEFINED';
  DR_PHYLO_MGRID = 'Model Grid (Gene Contribution Weights - Graphical)';

  CONSENSUS_VALUE = 'Consensus value';
  NUM_BCL_PRECISION_SAMPLES = 100;
  DR_PHYLO_TARGET_NODE = 'DRPHYLOTARGETNODE';
  SAVE_RESULTS_FILES = 'Save Results to File System...';
  CLOSE_RESULTS_FILES = 'Clear Results';
  ESL_PINUP_IMAGE_INDEX = 10;
  CONFIRM_CLOSE_MEGA_STRING = 'Are you sure you want to close MEGA?                ';
  SHOW_ALL = 'Show All';
  SHOW_MOST_PROBABLE = 'Show Most Probable';
  HIDE_AMBIGUOUS = 'Hide Ambiguous';
  POST_PROCESS_CITATION = 'POST_PROCESS_CITATION';
  WM_CANCELMODE = 31;
  STATS_DISPLAY_FREQUENCY = 'Frequency';
  STATS_DISPLAY_RANGE = 'Range';
  STATS_DISPLAY_SITE_COVERAGE = 'Site Coverage';
  STATS_DISPLAY_NODE_IDS = 'Node IDs';
  STATS_DISPLAY_NOTHING = 'nothing';
  MASTER_PROCESS_ID = 0;
  RSV_TAG_INDEX = 0;
  SPECIES_TAG_INDEX = 1;
  POPULATION_TAG_INDEX = 2;
  CONTINENT_TAG_INDEX = 3;
  COUNTRY_TAG_INDEX = 4;
  CITY_TAG_INDEX = 5;
  YEAR_TAG_INDEX = 6;
  MONTH_TAG_INDEX = 7;
  DAY_TAG_INDEX = 8;
  TIME_TAG_INDEX = 9;
  NONE_TAG_INDEX = 10;
type
  TGroupTagIndex = (gtiRsv = RSV_TAG_INDEX, gtiSpecies = SPECIES_TAG_INDEX, gtiPopulation = POPULATION_TAG_INDEX, gtiContinent = CONTINENT_TAG_INDEX, gtiCountry = COUNTRY_TAG_INDEX, gtiCity = CITY_TAG_INDEX, gtiYear = YEAR_TAG_INDEX, gtiMonth = MONTH_TAG_INDEX , gtiDay = DAY_TAG_INDEX, gtiTime = TIME_TAG_INDEX, gtiNone = NONE_TAG_INDEX);

const
  BEGIN_MARKER = 0;
  END_MARKER = 1;
  OUTGROUP_MARKER = ' [*]';
  DEFAULT_MODEL_TEST_BCI_DELTA = 20;
  MIN_LITTLE_BOOTSTRAP_SUPPORT = 10;
  MIN_LITTLE_BOOTSTRAP_SAMPLES = 5;
  MIN_LBS_USER_TREE_SAMPLES = 3;
  MIN_LITTLE_BOOTSTRAP_REPS = 10;
  MIN_TOTAL_LITTLE_BOOTSTRAP_REPS = 25;
  MIN_LBS_USER_TREE_REPS = 5;
  NUM_LBS_PARAM_REPS = 1;
  MAX_LITTLE_BOOTSTRAP_ADDITIONAL_REPS = 5;
  MST_MAX_TREES = 100;
  SDE_MAX_TOTAL_SITES = 1000000*2000;
  DEFAULT_DATE_FORMAT = 'yyyy-mm-dd';
  SUM_OF_BLENS = 'Sum of branch lengths';
  {$IFDEF VISUAL_BUILD}
  TE_DEFAULT_TREE_WIDTH = 400;
  TE_FRAME_HEADER_DEFAULT_COLOR = $00CCCCCC;
  TE_FRAME_HEADER_HOVER_COLOR = $00DDDDDD;
  TE_FRAME_BODY_COLOR = clDefault; //$00F8F8F7;
  TE_FRAME_HEADER_DEFAULT_FONT_COLOR = clBlack;
  TE_FRAME_HEADER_HOVER_FONT_COLOR = clBlack;
  HOVER_PANEL_COLOR = $003641c7;
  HOVER_PANEL_FONT = clWhite;
  MULTITHREADING_MSG = 'A multi-threaded Maxmimum Likelihood analysis is ' + LineEnding +
                       'already running in MEGA but only one multithreaded' + LineEnding +
                       'calculation may be run at a time. To run multiple'+ LineEnding +
                       'ML analyses simultaneously, run each analysis using ' + LineEnding +
                       'a single thread.';
  {$ENDIF}
  MIN_BOOTSTRAP_REPS_NEEDED = 2;
  OPTIMIZE_LBS_MSG = 'Optimizing replicates';
  SKIP_STATUS_UPDATE = 'skip-status-update';
  INGROUP_ROOT = 'ingroup-root';
  OUTGROUP_SEQUENCE = 'Outgroup Sequence';
  TREE_FILE = 'Tree File';
  ALIGNMENT_FILE = 'Alignment File';
  STARTING_TREE = 'Starting Tree';
  NUMBER_OF_QUARTETS = 'Number of Quartets';
  EQUALS_SIGN_PLACEHOLDER = ':::EQUALS:::';
  MEGA_COMMAND_DELIMITER = '=';
  PROTEIN_SEQUENCES = 'Protein Sequences';
  DNA_SEQUENCES = 'DNA Sequences';
  SKIP_SAVE_FILE = 'skip_save_file';
  NOT_FOUND_STR = 'not found';
  NJ_CITATION = 'Saitou_and_Nei_1987';
  BIONJ_CITATION = 'Gasceul_1997';
  MAX_CUSTOM_CONTROL_DIMENSION = 65535; { if set the width or height of a control to a value greater than this, an error is raised}
  FP_CUTOFF = 0.000000000001; { Floating Point Cutoff - if a value is less than this, we are worried about overflow and div by zero errors}
  {$IFDEF VISUAL_BUILD}
  DEFAULT_BG_COLOR = $00fcfaf9;
  DEFAULT_FONT_COLOR = $00333333;
  DEFAULT_ACTIVE_BG_COLOR = $009c9d1f;
  DEFAULT_ACTIVE_BG_COLOR2 = $00e7e7e7;
  DEFAULT_ACTIVE_FONT_COLOR = clWhite;
  {$ENDIF}
  GET_VERSION_INFO = 'get_version_info';

  { stuff for automated sanity checks of the session file system}
  SESSION_TEST = 'session_test';
  DROSOPHILA_FILE = 'Drosophila_Adh.meg';
  MTCDNA_FILE = 'mtCDNA.meg';
  DISTANCE_DATA_FILE = 'Distance_Data.meg';
  CHLOROPLAST_FILE = 'Chloroplast_Martin.meg';
  HSP20_FILE = 'hsp20.fas';
  GENE_TREE_FILE = 'gene_tree.nwk';
  SPECIES_TREE_FILE = 'species_tree.nwk';
  lnLDigits = 3;
  paraDigits = 2;
  pDigits = 4;
  freqDigits = 3;

type
  {$IFDEF VISUAL_BUILD}
  TCustomFormList = specialize TFPGList<TCustomForm>;
  TMenuItemsList = specialize TFPGList<TMenuItem>;
  {$ENDIF}
  {$PACKENUM 1}
  TTreeDataValue = (tdvValue, tdvValue2, tdvValue3, tdvTreeLength, tdvInitialLogLikelihood, tdvLogLikelihood);
  TTreeDataValueSet = set of TTreeDataValue;
  TCalendarMonth = (NoMonth = 0, January = 1, February = 2, March = 3, April = 4, May = 5, June = 6, July = 7, August = 8, September = 9, October = 10, November = 11, December = 12);
  //TContinent = (Africa, AfricaEurasia, America, Antarctica, Asia, Australia, Europe, Eurasia, NorthAmerica, SouthAmerica);
  TMegaReleaseType = (mrtAlpha, mrtBeta, mrtStable, mrtDeveloper);
  TTreeboxType = (tttOriTree, tttBootTree, tttReltimeTree, tttEditBox, tttCustomTimetreeBox);
  TCalibrationDensityDistribution = (cddNormal, cddExponential, cddUniform, cddLognormal, cddNone);
  TCalibrationCategory = (ccDensityNormal, ccDensityExponential, ccDensityUniform, ccMinTimeOnly, ccMaxTimeOnly, ccMinMaxTime, ccFixedTime, ccFixedRate, ccDensityLogNormal, ccNone);
  TCalibrateClockMode = (ccmEvoRate, ccmDivTime, ccmNone);
  PNotifyEvent = ^TNotifyEvent;
  TAnalysisWizardMode = (awmSeqData);
  TAnalysisWizardAnalysis = (ttaHyPhy, ttaIQTree);
  TTimeTreeWizardMode = (ttwmSeqData, ttwmBlens);
  TImageFileFormat = (iffPng, iffBitmap, iffTiff, iffJpeg, iffPdf, iffEmf, iffSvg);
  TTimeTreeAnalysis = (ttaReltime, ttaCorrtest, ttaEP, ttaLbsInference, ttaLbsTreeTiming);
  TMegaCheckBoxState = (cbsChecked, cbsUnchecked, cbsPartiallyChecked);
  TMegaExportFormat = (mefPaup4, mefPaup3, mefMega, mefFasta, mefPhylip, mefExcel, mefExcelXml, mefOds, mefCsv, mefTxt, mefNexus);
  TPointArray = array of TPoint;
  PPointArray = ^TPointArray;
  TReltimeVarianceMethod = (rvmNone, rvmBootstrap, rvmRates, rvmSampling, rvmRatesAndSampling);
  TSpeciesNameTokenFilter = (sntfNumber, sntfNonAlphaNumeric, sntfText, sntfRegex);
  TSpeciesNameTokenFilterArray = array of TSpeciesNameTokenFilter;
  TTreeRebuildPhase = (rpIdle, rpSetPosition, rpSetSize, rpCreateMetafile, rpDrawBranches, rpDrawMarkers, rpDrawNodeIds, rpDrawDivTimes, rpDrawStats, rpDrawDataCoverage, rpDrawBranchLengths, rpDrawCharState, rpDrawScale, rpPaint);
  TBeginRefreshCallback = procedure of object;
  TEndRefreshCallback = procedure of object;
  TProgressCallback = procedure(AProgress: Integer) of object;
  TRebuildStatusCallback = procedure(AStatus: TTreeRebuildPhase) of object;
  TTreeStyle = (tsTraditional, tsRadiation, tsCircle);
  TBranchStyle = (bsRectangular, bsCurved, bsStraight);
  TBranchOption = (boFullBranch, boHalfBranch, boNoBranch, boBranchOnly);
  TBracketStyle = (brsSquare, brsLine, brsNone);
  TGraphicAlign = (gaLeft, gaRight, gaTop, gaBottom);
  UndoAction = (uNone, uSetOTUName, uSetOTUPrivateName, uMoveNode, uRemoveOTU, uInsertOTU, uFlipCluster, uFlipAllCluster, uMakeRootOnMidPoint, uMakeRootByOutgroup, uMakeRootOnBranch, uEditBranchLength);  // KT Added uNone when there is no undo history
  TBranchInfoPosition = (bipAutomatic, bipAboveBranch, bipBelowBranch);

  TNodeType = (ntNone, ntNoFocus, ntOTU, ntInterior, ntCompressed, ntHidden, ntRoot);
  TBranchType = (btNone, btNoFocus, btExterior, btRootedExterior, btInterior, btRootedInterior);

  TNodeMarkerShape = (msNone, msOpenCircle, msFilledCircle, msOpenSquare, msFilledSquare,
                 msOpenUpTriangle, msFilledUpTriangle, msOpenDownTriangle,
                 msFilledDownTriangle, msOpenDiamond, msFilledDiamond);
  TConstraintConsistencyStatus = (ccsOk, ccsOverestimated, ccsUnderestimated);
  TRunStatusProc = procedure (aStatus: AnsiString; aInfo: AnsiString) of object;
  TProgressProc = procedure (aProgress: Integer; aStatus: String) of object;
  TSendMessageProc = procedure(aMsg: String) of object;
  TProgressAndStatusCheckCancel = function (Progress: Integer; AType: String; AInfo: String): Boolean of object;
  TProgressCheckCancelFunc = function (Progress: Integer): Boolean of object;
  TUpdateProgressProc = procedure(aProgress: Integer) of object;
  TNewLineProc = procedure (NewLine: String) of object;
  TCheckAbortFunc = function: Boolean of object;
  TCheckCancelFunc = function (Progress: integer; Status: AnsiString): boolean of object;
  TChildCheckCancelFunc = function(ThreadIndex: Integer; Progress: Integer; Status: AnsiString): boolean of object;
  TCheckCancelFuncArray = array of TCheckCancelFunc;
  PTriVertex = ^TTriVertex;
  TTriVertex = record
    X, Y: DWORD;
    Red, Green, Blue, Alpha: WORD;
  end;
  TSearchDirection = (sdForward, sdBackward);
  TIntArray = Array of Integer;
  TArrayOfInteger = array of Integer;
  T2DArrayOfInteger = array of TIntArray;
  T2DExtArray = array of array of Extended;
  T2DDoubleArray = array of array of Double;
  T3DDoubleArray = array of T2DDoubleArray;
  TBoolArray = array of Boolean;
  TBoolArrayArray = array of TBoolArray;
  TTtProcessStep = (ttpsLoadTree, ttpsLoadSeqData, ttpsDoOutgroup, ttpsDoCalibrations, ttpsDoIQSettings, ttpsSelectIQTreeOption, ttpsDoSettings, ttpsLaunchAnalysis, ttpsDone, ttpsCancelAnalysis, ttpsDisableOutgroup);

  TValidationMessage = (vsError, vsWarning, vsOk);
  TValidationMessages = set of TValidationMessage;
const
  XLSX_FORMAT_STR = 'XLSX: MS Excel workbook (2007+)';
  XLS_FORMAT_STR = 'XLS: MS Excel Workbook (all versions)';
  ODS_FORMAT_STR = 'ODS: Open/Libre Office Workbook';
  CSV_FORMAT_STR = 'CSV: Comma-Separated-Values';
  TEXT_FORMAT_STR = 'TXT: Text File';
  NO_FORMAT_STR = 'Unformatted text';
  MEGA_FORMAT_STR = 'MEGA format';
  TEMPORARY_MEGA = 'MEGA-result';
  FASTA_FORMAT_STR = 'FASTA format';
  SITES_PER_SEGMENT = 10;
  SITES_PER_LINE = 80;
  ACTION_LIST_REFRESH_RATE = 500;
  CHECK_CANCEL_NO_PROG_VAL   = -999;
  DEFAULT_MAX_RATE_RATIO = 20.0;
  {$IFDEF VISUAL_BUILD}
  DEFAULT_MAX_PERCENT_RAM = 50;
  {$ELSE}
  DEFAULT_MAX_PERCENT_RAM = 75;
  {$ENDIF}
  MIN_NUM_BOOT_REPS = 50;
  DBLQ = #34; { double quote character}
  USER_ABORTED = 999;
  MAX_INTERVAL_NO_BSPROG_UPDATE = 20000; { max number of milliseconds an ML tree search takes before we start updating progress for each tree search}
  PROG_UPDATE_INTERVAL = 50; { number of milliseconds between progress updates}
  SUBTASK_PROG_UPDATE_INTERVAL = 50;
  TimeTreeBaseUrl = 'http://www.timetree.org/index.php'; // for the TimeTree.org button
  Mega_CC_tutorial = 'http://megasoftware.net/tutorial_CC.php';
  DatamonkeyUrl = 'http://www.datamonkey.org/slac';
  MEGA_CITATIONS_URL = 'https://www.megasoftware.net/citations';
  MEGA_RESOURCES_URL = 'https://www.megasoftware.net/pdfs/Caspermeyer_2018.pdf';
  MEGA_WEBSITE_URL = 'https://www.megasoftware.net';
  NCBI_URL = 'https://www.ncbi.nlm.nih.gov';
  // CalTester stuff
  { these indices are for TTreeList after running the main CalTester analysis}
  RELTIME_TREE_INDEX = 0;  { index for the TTreeData which has the calculated relative times}
  ML_BLENS_TREE_INDEX = 1; { index for the TTreeData which has the branch lengths that were inferred using the Analyze User Tree (ML) method}
  DIV_TIME_TREE_INDEX = 2; { index for the TTreeData of the user-provided tree from which divergence times were inferred}

const
  OPERATION_RUN_TIME = 'Operation Run Time';
  MEGA_INTERNAL_ERROR = 'MEGA_INTERNAL_ERROR';
  NO_GEOGRAPHICAL_INFO = 'NO_GEOGRAPHICAL_INFO';
  // key values for analysis settings
  MissingBaseSymbolStr = 'missingBaseSymbol';
  MissingBaseSymbolStr2 = 'Missing Base Symbol'; //  for backwards compatibility
  IdenticalBaseSymbolStr = 'identicalBaseSymbol';
  IdenticalBaseSymbolStr2 = 'Identical Base Symbol';
  GapSymbolStr = 'gapSymbol';
  GapSymbolStr2 = 'Gap Symbol';
  DataTypeStr = 'datatype';
  DefaultMissingSymbol = '?';
  DefaultIdenticalSymbol = '.';
  DefaultGapSymbol = '-';
  ContainsCodingNucStr = 'containsCodingNuc';

  DISP_DIV_TIMES_STR = 'DisplayingDivTimes';
  DISP_ERR_BARS_STR = 'DisplayingErrorBars';
  BOOT_REP_PROG_STR = 'Bootstrap Rep #';
  CURRENT_BEST_MODEL = 'Current best model';

const
  // methods for generating initial trees when performing ML phylogeny inference. See MLTreeAnalyzer.MakeTree.
  NJInitTreeMethod      = 0;
  BioNJInitTreeMethod   = 1;
  MEInitTreeMethod      = 2;
  MPInitTreeMethod      = 3;
  DefaultInitTreeMethod = 4;
  UserProvidedInitTree  = 5;
  MultipleMPTreesMethod = 6;

const
  DNAStr               : String = 'DNA';
  cDNAStr              : String = 'cDNA';
  ProteinStr           : String = 'Protein';
  DNABases : Set of AnsiChar = ['A','T','C','G','U','R','Y','M','K','S','W','B','V','D','H'];
  AABases  : Set of AnsiChar = ['A','C','D','E','F','G','H','I','K','L','M','N','P','Q','R','S','T','V','W','Y'];

const
  stCurSite      = 0;
  stMarkedSites  = 1;
  stCounts       = 2;
  stDomainName   = 3;
  SeqOffset = '   ';

  NewLine = #10#13;

  { these are the current versions for saving/loading session files}
  MSDX_SESSION_VERSION = 1200; //The version of Mega Data Session file that this build uses
  MAS_SESSION_VERSION = 1200; // the current version of alignment session files
  LAST_BAD_MAS_SESSION_VERSION = 111; // this version and earlier are broken for later versions of MEGA
  MTS_SESSION_VERSION = 1209; // the current version of tree session files
  LAST_BAD_MTS_SESSION_VERSION = 603; // this version and earlier are broken for later versions of MEGA (>6.0)
  DIST_PACK_SESSION_VERSION = 2;

  TIME_TREE_TAB = 'Timetree';
  ORI_TREE_TAB = 'Original Tree';
  CONSENSUS_TREE_TAB = 'Consensus Tree';
  BOOT_TREE_TAB = 'Bootstrap Tree';
  BOOT_CONS_TREE_TAB = 'Bootstrap Consensus Tree';
  CUSTOM_TIMETREE_TAB = 'Divergence Times';
  CONDENSED_TOPOLOGY_TAB = 'Condensed Topology';

  WIN32_TARGET = 1;
  WIN64_TARGET = 2;
  MAC32_TARGET = 3;
  MAC64_TARGET = 4;
  LINUX32_TARGET = 5;
  LINUX64_TARGET = 6;
  UNKNOWN_TARGET = 7;
  { 8 and 10 are the respective sizes of Extended depending on platform }
  EXTENDED_UNSUPPORTED_TARGET = 8;
  EXTENDED_SUPPORTED_TARGET = 10;

  { don't add new platforms, just indicate support for Extended }
  {$IFDEF CPUAARCH64}
  TARGET_PLATFORM = EXTENDED_UNSUPPORTED_TARGET;
  {$ELSE}
    {$IFDEF UNIX}
      {$IFDEF DARWIN}
        {$IFDEF CPU32}
        TARGET_PLATFORM = MAC32_TARGET;
        {$ENDIF}
        {$IFDEF CPU64}
        TARGET_PLATFORM = MAC64_TARGET;
        {$ENDIF}
      {$ELSE}
        {$IFDEF CPU32}
        TARGET_PLATFORM = LINUX32_TARGET;
        {$ENDIF}
        {$IFDEF CPU64}
        TARGET_PLATFORM = LINUX64_TARGET;
        {$ENDIF}
      {$ENDIF}
    {$ELSE}
      {$IFDEF MSWINDOWS}
        {$IFDEF CPU32}
        TARGET_PLATFORM = WIN32_TARGET;
        {$ENDIF}
        {$IFDEF CPU64}
        TARGET_PLATFORM = WIN64_TARGET;
        {$ENDIF}
      {$ELSE}
        {$IFDEF FPC_HAS_TYPE_EXTENDED}
           TARGET_PLATFORM = EXTENDED_SUPPORTED_TARGET;
        {$ELSE}
           TARGET_PLATFORM = EXTENDED_UNSUPPORTED_TARGET;
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  type
    TOtuDataType = (DataDist, DataString);
    TXmlAttribute = record
      Name: String;
      Value: String;
    end;

const
  {$IFDEF DARWIN}
  ESL_EXE = 'MyESL';
  {$ELSE}
  ESL_EXE = 'MyESL.exe';
  {$ENDIF}
  ESL_NEWICK = 'mega_newick.nwk';
  {$IFDEF MSWINDOWS}
    {$IFDEF FPC}
      MUSCLE_EXE : String = 'muscleWin64.exe';
    {$ELSE}
      {$IFDEF WIN64}
      MUSCLE_EXE : String = 'muscleWin64.exe';
      {$ELSE}
      MUSCLE_EXE : String = 'muscle.exe';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF UNIX}
    {$IFDEF DARWIN}
    MUSCLE_EXE : String = 'muscledarwin64';
    {$ELSE}
      {$IFDEF CPU64}
      MUSCLE_EXE : String = 'muscleUnix64.exe';
      {$ELSE}
      MUSCLE_EXE : String = 'muscleUnix32.exe';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  MuscleInput : String = 'SelectedDataFromMEGA';
  MafftInput  : String = 'SelectedDataFromMEGAforMAFFT.txt';
  MuscleOutput : String = 'MuscleResultForSelectedData';
  MafftOutput : String = 'MafftResultsForSelectedData.txt';
  MuscleRunLog : String = 'MuscleRunLog';


const
    // these are some exceptions that didn't make the port to D6 from D4
  SParseError = 'Error while parsing data or file';
  SListIndexError = 'List Index Error';
  SInvalidProperty = 'Invalid Property';
  SSymbolExpected = 'Symbol Expected';
  SNumberExpected = 'Number Expected';
  SIdentifierExpected = 'Identifier Expected';
  SStringExpected = 'String Expected';
  SCharExpected = 'Char Expected';
  SLineTooLong = 'Line Too Long';
  SInvalidString = 'Invalid String';

const  // Strings in the Menu preferences
  UserPref_AlwaysUseCurActiveDataStr  = 'Remember to reuse currently active data.';
  UserPref_MegaMainAlwaysUseCurActiveDataStr = 'Main Form: Remember to reuse currently active data.';
  UserPref_TreeExplorerAlwaysComputeMpBlensStr = 'Tree Explorer: Do not prompt to compute MP branch lengths.';
  UserPref_CloseProtoyperPopupStr = 'Protoyper: Do not display a Protoyper mode notification again.';
  UserPref_AlwaysCloseTextEditorStr   = 'Always close Text Editor without warning.';
  UserPref_TextEditorAlwaysCloseTextEditorStr = 'Text Editor: Do not display a warning when closing the text editor with open files.';
  UserPref_UseSelectedGeneticCodeForSessionStr = 'Use the selected genetic code for rest of the Alignment Session.';
  UserPref_AlignmentEditorUseSelectedGeneticCodeForSessionStr = 'Alignment Explorer: Do not prompt to use currently selected genetic code.';
  UserPref_DoNotWarnOnMEGACloseStr = 'Do not ask me when closing MEGA again.';
  UserPref_AllowCollectUsageData = 'Global: Allow MEGA to collect anonymous usage data.';
  UserPref_MegaMainDoNotWarnOnMEGACloseStr = 'Main Form: Don''t ask for confirmation that I want to exit.';

  DATA_COLLECT_PREFS_FILE = 'MegaDataCollectPrefs.txt';
  USER_TYPE_FILE = 'userType.txt';
  MIN_MMSE_TO_CHECK_PERCENT_DIFF = 0.000001; {Mean Mean Squared Error}
  MEGA_ERROR_EXIT_CODE = 500; { this value should not be changed as the test harness depends on it}
  DESIGN_TIME_PIXELS_PER_INCH = 96;
  DEFAULT_MAX_SAMPLE_SITES = 1000;
  DEFAULT_MAX_SUBSAMPLES = 20;
  DEFAULT_MAX_REPS_PER_SUBSAMPLE = 50;
  DEFAULT_MAX_ADAPTIVE_REPS = 500;
  DEFAULT_ADAPTIVE_BOOTSTRAP_TARGET_PRECISION = 5;
  MIN_ADAPTIVE_BOOTSTRAP_REPS = 25;
  DefaultMaxResults = 10000;
  MMaxTaxaNameLen                = 1024;
  MMaxTitleLen                   = 256;

  MMaxCodeTables                 = 20;
  MMaxCodeTableNameLen           = 40;
  InvalidDistValue               = -214748364;

  TrackBarTickValue              = 20;  // # pixels per tick mark
  {$IFDEF DARWIN}
  MegaReleaseType: TMegaReleaseType = mrtAlpha;
  {$ELSE}
  MegaReleaseType: TMegaReleaseType = mrtBeta;
  {$ENDIF}
  IsReleaseRecoding              = False;
type
  TAncestralStatesExportType = (asExportMostProbable, asExportCurrentSite, asExportAllSites, asExportChangesList, asExportDetails);
const
  MOST_PROBABLE = 'Most Probable';
  CURRENT_SITE_ANCESTORS = 'Current Site Ancestors';
  ALL_SITE_ANCESTORS = 'All Site Ancestors';
  CHANGES_LIST = 'Changes List';
  DETAILED_TEXT_EXPORT = 'Detailed Text Export';

var
  {$IFNDEF VISUAL_BUILD}
  ForceCaptionsExport: Boolean = False; { used for the test harness so captions can be tested programatically}
  ExportUsageData: Boolean = False; { used for the test harness to output the analysis summary as json and test the usage data reporting}
  {$ENDIF}
  UseLbsMeanForCutoffs: Boolean = True;
  ExportReplicateTrees: Boolean = False;
  IsSessionTest: Boolean = False; // This should NEVER be set to true here, it is a command-line option for automated testing of session files
  IsLegacySessionTest: Boolean = False;
  IsDeveloper: Boolean = False; // Make sure to change before release!
  EnableSubsamplingAnalyses: Boolean = True;
  KeepUserTreeBLens: Boolean = False; // needs to always be false as it is a developer feature
  IsPhyloQAnalysis: Boolean = False; // Make sure to set as 'False' before release! This enables SearchLastOTUPositionInUserTree for Alan's metagenomics analysis
  IsPrototyper: Boolean = False;
  DoBootstrapReplicateTiming: Boolean = False;
  IsLittleBootstrap: Boolean = False;
  DrPhyloEnabled: Boolean = True;

  ShowLogFiles: Boolean = {$IFDEF DEBUG}True{$ELSE}False{$ENDIF};
  ExportIncludedSitesSummary: Boolean = False; { false by default but command-line users can override it}
  CompressMLdata: Boolean = False;
  {$IFDEF DEBUG}{$IFNDEF VISUAL_BUILD}
  DevLogFileCS: TCriticalSection;
  DeveloperLog: TStringList;
  {$ENDIF}{$ENDIF}
  {$IFNDEF VISUAL_BUILD} {$IFNDEF MSWINDOWS}
  PeakMemoryUsedByMega: QWord;
  PeakMemoryCS: TCriticalSection;
 {$ENDIF}{$ENDIF}
 {$IFDEF VISUAL_BUILD}
 TreeBoxSessionPropsCI: TCriticalSection;
 FSessionTestCriticalSection: TCriticalSection;
 {$ENDIF}
 AlignmentSessionPropsCI: TCriticalSection;
const
  ValidOtuNameStartSet    = ['A'..'Z', 'a'..'z', '0'..'9','-','+','.'];
  ValidOtuNameContinueSet = ['A'..'Z', 'a'..'z', '0'..'9','-','+','.',':','_','(',')','*','|','/','\','='];
  ValidSiteLabelSet       = ['A'..'Z', 'a'..'z', '0'..'9','-','+','.',':','_','(',')','*',
                             '<','>','|','\','@','$','%','?','^','&'];
  UnambiguousBaseSet      = ['A','a','T','t','C','c','U','u','G','g'];
  UnambiguousAminoSet     = ['A'..'Z', 'a'..'z'] - ['B','b','X','x','Z','z','J','j','O','o','U','u'];
  ValidQuoteSet           = ['''', '"', '`'];

type
  TDataFileFormat = (dfUnknown, dfFasta, dfMeg, dfNexus, dfMas, dfMts, dfNewick, dfMdsx, dfPhylip,
                     dfPir, dfNbrf, dfMsf, dfIg, dfClustal, dfText, dfCsv, dfAbi, dfXml, dfStaden,
                     dfGcg, dfBlast, dfMao, dfHtml);

const
  AStar      = '*';
  SemiCol    = ';';

  // Fasta Extensions
  FastaExt1  = '.fas';
  FastaExt2  = '.fst';
  FastaExt3  = '.fta';
  FastaExt4  = '.fsa';
  FastaExt5  = '.fasta';
  FastaExt6  = '.fsta';
  FastaExt7  = '.faa';
  FastaExts    = AStar+FastaExt1  +SemiCol+
                 AStar+FastaExt2  +SemiCol+
                 AStar+FastaExt3  +SemiCol+
                 AStar+FastaExt4  +SemiCol+
                 AStar+FastaExt5  +SemiCol+
                 AStar+FastaExt6  +SemiCol+
                 AStar+FastaExt7;

  // Mega data extensions for text files
  MegaExt1   = '.meg';
  MegaExt2   = '.mdsx';
  MegaExt3   = '.mega';
  // We can not include this in the MEGA extensions.  We determine how to open a file by the extension, by including MAS in MegaExts it always opens for analysis which can cause problems.
  MegaExts     = AStar+MegaExt1   +SemiCol+
                 AStar+MegaExt2   +SemiCol+
                 AStar+MegaExt3;

  MasExt1    = '.mas';
  MasExt2    = '.masx';
  MASExts    = AStar+MasExt1 + SemiCol + AStar+ MasExt2;

  // Mega Tree extensions
  MtsExt1    = '.mts';
  MtsExt2    = '.mtsx';
  NewickExt1 = '.nwk';
  NewickExt2 = '.tre';
  NewickExt3 = '.dnd';
  NewickExt4 = '.ph';
  NewickExt5 = '.phb';
  NewickExt6 = '.newick';
  NewickExt7 = '.phy';

  NewickExts   = AStar+NewickExt1 +SemiCol+
                 AStar+NewickExt2 +SemiCol+
                 AStar+NewickExt3 +SemiCol+
                 AStar+NewickExt4 +SemiCol+
                 AStar+NewickExt7 + SemiCol +
                 AStar+NewickExt5 +SemiCol+
                 AStar+NewickExt6;

  TreeSessionExts = AStar+MtsExt1 + SemiCol +
                    AStar+MtsExt2;

  // other text formats
  PhylipExt1 = '.phylip';
  PhylipExt2 = '.phylip2';
  PhylipExts = AStar+PhylipExt1 + SemiCol +
               AStar+PhylipExt2;

  PAUPExt1   = '.nexus';
  PAUPExt2   = '.nex';
  PAUPExt3   = '.nexorg';
  PaupExts     = AStar+PaupExt1   +SemiCol+
                 AStar+PaupExt2   +Semicol+
                 AStar+PAUPExt3;

  PIRExt1    = '.pir';
  NbrfExt1   = '.nbrf';
  NbrfExt2   = '.nbr';
  MSFExt1    = '.msf';
  IGExt1     = '.ig';

  ClustalExt1= '.aln';
  ClustalExts  = AStar+ClustalExt1;

  TextExt1   = '.txt';
  TextExt2   = '.seq';

  ABIExt1    = '.abi';
  ABIExt2    = '.ab1';
  StadenExt1 = '.scf';
  XMLExt1    = '.xml';
  GCGExt1    = '.gcg';
  BlastExt1  = '.blast';

  AnalysisOptExt = '.mao';
  AnalysisOptType = 'MEGA Analysis Options';
  AnalysisOpt = 'M7CC';

  MtsExts      = AStar+MtsExt1+SemiCol+AStar+MtsExt2;

  GCGExts      = AStar+GCGExt1;
  PIRExts      = AStar+PIRExt1;
  NbrfExts     = AStar+NbrfExt1   +SemiCol+
                 AStar+NbrfExt2;
  MsfExts      = AStar+MsfExt1;
  IgExts       = AStar+IgExt1;
  ABIExts      = AStar+ABIExt1    +SemiCol+
                 AStar+ABIExt2;
  StadenExts   = AStar+StadenExt1 +SemiCol;
  XMLExts      = AStar+XMLExt1;
  TextExts     = AStar+TextExt1   +SemiCol+
                 AStar+TextExt2;
  HTMLExts     = AStar+BlastExt1;

const
  gdBaseA: AnsiChar = #0;  // used in distance mapping: Amino
  gdBaseT: AnsiChar = #1;
  gdBaseU: AnsiChar = #1;
  gdBaseC: AnsiChar = #2;
  gdBaseG: AnsiChar = #3;
  gdBaseN: AnsiChar = #4;  // Basically Char(NStates)

  gdResiA: AnsiChar =#0;  // used in distance mapping: Amino
  gdResiC: AnsiChar =#1;
  gdResiD: AnsiChar =#2;
  gdResiE: AnsiChar =#3;
  gdResiF: AnsiChar =#4;
  gdResiG: AnsiChar =#5;
  gdResiH: AnsiChar =#6;
  gdResiI: AnsiChar =#7;
  gdResiK: AnsiChar =#8;
  gdResiL: AnsiChar =#9;
  gdResiM: AnsiChar =#10;
  gdResiN: AnsiChar =#11;
  gdResiP: AnsiChar =#12;
  gdResiQ: AnsiChar =#13;
  gdResiR: AnsiChar =#14;
  gdResiS: AnsiChar =#15;
  gdResiT: AnsiChar =#16;
  gdResiV: AnsiChar =#17;
  gdResiW: AnsiChar =#18;
  gdResiY: AnsiChar =#19;
  gdResiX: AnsiChar =#20; // Basically Char(NStates)

const
    // parsimony: Nucleotide maps; must correspond in principle with gdBaseA
  pmBaseA: AnsiChar = #1;
  pmBaseT: AnsiChar = #2;
  pmBaseC: AnsiChar = #4;
  pmBaseG: AnsiChar = #8;
  pmBaseN: AnsiChar = #255; // this is also ? etc, and must be so
  pmBaseR: AnsiChar = #9;  // A or G: #1 + #8
  pmBaseY: AnsiChar = #6;  // C or T: #2 + #6
  pmBaseM: AnsiChar = #5;  // A or C: #1 + #4
  pmBaseK: AnsiChar = #10; // G or T: #8 + #2
  pmBaseS: AnsiChar = #12; // C or G: #4 + #8
  pmBaseW: AnsiChar = #3;  // A or T: #1 + #2
  pmBaseH: AnsiChar = #7;  // A, T, or C: #1 + #2 + #3
  pmBaseB: AnsiChar = #14; // T, C, or G: #2 + #4 + #8
  pmBaseV: AnsiChar = #13; // A, C, or G: #1 + #4 + #8
  pmBaseD: AnsiChar = #11; // A, T, or G: #1 + #2 + #8

const
    // parsimony: Amino acid maps
  pmResiA: Longint = $00000001; //1 = 1
  pmResiC: Longint = $00000002; //1 shl 1
  pmResiD: Longint = $00000004; //1 shl 2
  pmResiE: Longint = $00000008; //1 shl 3
  pmResiF: Longint = $00000010; //1 shl 4
  pmResiG: Longint = $00000020; //1 shl 5
  pmResiH: Longint = $00000040; //1 shl 6
  pmResiI: Longint = $00000080; //1 shl 7
  pmResiK: Longint = $00000100; //1 shl 8
  pmResiL: Longint = $00000200; //1 shl 9
  pmResiM: Longint = $00000400; //1 shl 10;
  pmResiN: Longint = $00000800; //1 shl 11;
  pmResiP: Longint = $00001000; //1 shl 12;
  pmResiQ: Longint = $00002000; //1 shl 13;
  pmResiR: Longint = $00004000; //1 shl 14;
  pmResiS: Longint = $00008000; //1 shl 15;
  pmResiT: Longint = $00010000; //1 shl 16;
  pmResiV: Longint = $00020000; //1 shl 17;
  pmResiW: Longint = $00040000; //1 shl 18;
  pmResiY: Longint = $00080000; //1 shl 19;
  pmResiB: Longint = $00008004; //pmResiD or pmResiN;
  pmResiZ: Longint = $00002008; //pmResiE or pmResiQ;  	{TODo -o DanResolve: 'Z' in AA is not allowed to be fully ambiguous}
  pmResiX: Longint = $0FFFFFFF; // one 0 because compiler complains otherwise

{const
  //binary recoding; - is ignore, 0 means none; try 1 to be low state
  //                   ABCDEFGHIJKLMNOPQRSTUVWXYZ
  rcBinaryN      =  'Wxxxx; xxxx; xxxx;
  rcMultiAlleleN =  '0-000002
  rcBinaryO      =  '0-0100
  rcMultiAlleleO =  '0-0220
  rcBinaryS      =  '0-100000000010000000000000'
  rcBinaryCys    =  '0-100000000000000000000000'
  rcPolar        =
}
const
  rtfHeaderStr   = ''; //  '{\rtf1\ansi\deff0\deftab720{\fonttbl{\f0\fnil MS Sans Serif;}{\f1\fnil\fcharset2 Symbol;}{\f2\fswiss\fprq2 System;}{\f3\fnil Times New Roman;}{\f4\fnil Courier New;}}'+
                       // '{\colortbl\red0\green0\blue0;}';
  rtfStartStr    = ''; //'\deflang1033\pard';
  rtfParaStr     = ''; //'\par ';
  rtfEndStr      = ''; //'\par }';
  rtfBoldStr     = ''; //'\plain\f4\fs16\b ';
  rtfItalStr     = ''; //'\plain\f4\fs16\i ';
  rtfPlainStr    = ''; //'\plain\f4\fs16 ';

  ImplementInInheritedForm = 'Implement in the inherited Form.';
  NeedsToBeImplemented     = 'Needs to be implemented.';

const // Tree Input
  UserAddedStr = '<user added>';
  UndefinedStr = '<unmapped>';

type
  TClockType = (ctGlobal, ctLocal, ctUnknown);
  TClockLevel = (clNoStdErr, clOneStdErr, clTwoStdErr, clThreeStdErr, clUnknown);

type
  TExportType = (EXInvalid, EXnone, EXtext, EXtextSave, EXcsvDisp, EXcsvSave, EXexcelDisp, EXexcelSave, EXexcelXmlDisp, EXexcelXmlSave, EXfasta, EXFastaSave, EXodsDisp, EXodsSave, EXmega);
  TExportTypeSet = set of TExportType;
  TOutputFileType = (ExportExelXML, ExportExcel, ExportODS, ExportCSV, ExportText, ExportMega, ExportFasta, ExportNone, ExportInvalid);

type
  icsSetType = (meg1stSite, meg2ndSite, meg3rdSite, megNoncoding);
  TIncludeCodonPos = set of icsSetType;
  TIncludeDegeneracy = (megAllFold, megZeroFold, megTwoFold, megFourFold);
  TSiteAttrType =
    (megNone,      // always keep this as the first type
     megUse,
       // keep the following five together with megVar as first
     meg0thBase,                          // noncoding nuc
     meg1stBase, meg2ndBase, meg3rdBase,  // Coding sequence parts
     megGap, megMissing,                  // status are marked by the viewer
       // last six entries for coding data; keep them in this structure
     megConst,
     megVar, megParsimInfo, megSingleton, megLabelled, // property of all sites
     meg0Fold, meg2Fold, meg4Fold,megCoverage,megCpG,        // property of coding sites;
      // these Amino/Nuc/Cod attr are used for storage
     megAminoConst,
     megAminoVar, megAminoParsimInfo, megAminoSingleton, megAminoLabelled,
     megAminoGap, megAminoMissing,
     megNucConst,
     megNucGap, megNucMissing,
     megNucVar, megNucParsimInfo, megNucSingleton,  megNucLabelled,
     megCod0Fold, megCod2Fold, megCod4Fold,
     megLast // always keep it as the last type
    );
  TSiteAttr = set of TSiteAttrType;

  dsoDataSubsetOption = (dsoUseNuc, dsoUseAmino, dsoUseCodon,
                         dsoCompleteDeletion,  // insert for comp-del
                         dsoPairwiseDeletion,
                         dsoPartialDeletion, 
                         dsoUseOnlyLabelledSites, dsoUseOnlyUnlabelledSites,
                         dsoUseNonCod, dsoUse1stPos, dsoUse2ndPos, dsoUse3rdPos,
                         dsoParsimMap, dsoNucToTvParsim, dsoDistMap, dsoNoMap,
                         dsoRemoveInvar, dsoRemoveUninfoVar,
                         dsoRecodeBases);
  TDataSubsetOptions = set of dsoDataSubsetOption;

type
  TOLSStatus = set of (msGetPartition, msGetOLS, msReversal);
  TArrayOfTOLSStatus = array [Word] of TOLSStatus;
  PArrayOfTOLSStatus = ^TArrayOfTOLSStatus;

const
  MaxTPointElts   = High(Longint) div sizeOf(TPoint);
  MaxLongintElts  = High(Longint) div sizeOf(Longint);
  MaxSmallIntElts = High(Longint) div sizeOf(SmallInt);
  MaxIntElts      = High(Longint) div sizeOf(Integer);
  MaxSingleElts   = High(Longint) div sizeOf(Single);
  MaxDoubleElts   = High(Longint) div sizeOf(Double);
  MaxBoolElts     = High(Longint) div sizeOf(Boolean);
  MaxPointerElts  = High(Longint) div sizeOf(Pointer);
  MaxTSiteAttrElts= High(Longint) div sizeOf(TSiteAttr);

  MaxExtendedElts   = High(Longint) div sizeOf(Extended);   // for ML

type  // the following types replace the later types
  ArrayOfInteger = array of Integer;
  TDivTimesArray = array of Extended;

  ArrayOfDouble  = array of Double;
  ArrayOfExtended = array of Extended;
  T2dArrayOfExtended = array of array of Extended;
  T2dArrayOfLongInt = array of array of LongInt;
  TArrayOfInt64 = array of Int64;

type
  TArrayOfTPoint = array [0..MaxTPointElts-1] of TPoint;
  PArrayOfTPoint = ^TArrayOfTPoint;

  TArrayOfPChar  = array [0..MaxPointerElts-1] of PAnsiChar;
  PArrayOfPChar  = ^TArrayOfPChar;

  PString        = ^String;
  TArrayOfString = array of String;
  T2DStringArray = array of TArrayOfString;
//  PArrayOfString = ^TArrayOfString;

  PInt           = ^Integer;
  TArrayOfInt    = array [0..MaxIntElts-1] of Integer;
  PArrayOfInt    = ^TArrayOfInt;
  TArrayOfInt2D  = array [0..MaxPointerElts-1] of PArrayOfInt;
  PArrayOfInt2D  = ^TArrayOfInt2D;

  PLongint           = ^Longint;
  TArrayOfLongint    = array [0..MaxLongintElts-1] of Longint;
  PArrayOfLongint    = ^TArrayOfLongint;
  TArrayOfLongint2D  = array [0..MaxPointerElts-1] of PArrayOfLongint;
  PArrayOfLongint2D  = ^TArrayOfLongint2D;

  PSmallInt       = ^Smallint;
  TArrayOfSmallint = array[0..MaxSmallIntElts-1] of Smallint;
  PArrayOfSmallint = ^TArrayOfSmallint;

  PSingle        = ^Single;
  TArrayOfSingle = array[0..MaxSingleElts-1] of single;
  PArrayOfSingle = ^TArrayOfSingle;

  PDouble        = ^Double;
  TArrayOfDouble = array [0..MaxDoubleElts-1] of Double;
  PArrayOfDouble = ^TArrayOfDouble;

  TDistanceMatrix= array [0..MaxPointerElts-1] of PArrayOfDouble;
  PDistanceMatrix= ^TDistanceMatrix;

  TArrayOfBool   = array [0..MaxBoolElts-1] of Boolean;
  PArrayOfBool   = ^TArrayOfBool;
  TArrayOfBoolean = array of Boolean;
  TArrayArrayOfBoolean = array of TArrayOfBoolean;
  TSiteAttrArray = array [0..MaxTSiteAttrElts-1] of TSiteAttr;
  PSiteAttrArray = ^TSiteAttrArray;

  TNodeData = record
    des1, des2 : Integer;
  end;
  TArrayOfNodeData = array[0..(MaxInt div SizeOf(TNodeData) -1)] of TNodeData;
  PArrayOfNodeData = ^TArrayOfNodeData;

  TArrayAtoZofChar = array['A'..'Z'] of AnsiChar;


  TMatrixOfInt = array [0..MaxPointerElts-1] of PArrayOfInt;
  PMatrixOfInt = ^TMatrixOfInt;

  TArrayOfExtended = array [0..MaxExtendedElts-1] of Extended;
  PArrayOfExtended = ^TArrayOfExtended;
  TMatrixOfExtended = array [0..MaxPointerElts-1] of PArrayOfExtended;
  PMatrixOfExtended = ^TMatrixOfExtended;
  TArrayOfPointer = array[0..(MaxInt div SizeOf(Pointer)-1)] of pointer;
  TpArrayOfPointer = ^TArrayOfPointer;

  TArrayOfExt = array of Extended;
  PArrayOfExt = ^TArrayOfExt;
  TArrayOfArrayOfExt = array of TArrayOfExt;

// The constants here are for the CRC-32 generator
// polynomial, as defined in the Microsoft
// Systems Journal, March 1995, pp. 107-108
CONST
  table: ARRAY[0..255] OF DWORD =
 ($00000000, $77073096, $EE0E612C, $990951BA,
  $076DC419, $706AF48F, $E963A535, $9E6495A3,
  $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
  $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
  $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
  $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
  $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
  $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
  $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
  $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
  $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
  $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
  $26D930AC, $51DE003A, $C8D75180, $BFD06116,
  $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
  $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
  $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

  $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
  $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
  $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
  $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
  $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
  $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
  $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
  $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
  $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
  $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
  $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
  $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
  $5005713C, $270241AA, $BE0B1010, $C90C2086,
  $5768B525, $206F85B3, $B966D409, $CE61E49F,
  $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
  $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

  $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
  $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
  $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
  $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
  $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
  $F762575D, $806567CB, $196C3671, $6E6B06E7,
  $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
  $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
  $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
  $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
  $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
  $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
  $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
  $CC0C7795, $BB0B4703, $220216B9, $5505262F,
  $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
  $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

  $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
  $9C0906A9, $EB0E363F, $72076785, $05005713,
  $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
  $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
  $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
  $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
  $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
  $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
  $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
  $A7672661, $D06016F7, $4969474D, $3E6E77DB,
  $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
  $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
  $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
  $BAD03605, $CDD70693, $54DE5729, $23D967BF,
  $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
  $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

const
  NoCommonSitesStr = ' If you selected the complete deletion option then you' +
    ' might achieve better results using the pairwise deletion option, as complete' +
    ' deletion removes all sites containing a gap in any part of the alignment.' +
    ' If you selected the pairwise deletion option then MEGA was unable to calculate' +
    ' the distance between one and several of the sequence pairs in the alignment.' +
    ' To identify such pairs compute a pairwise distance matrix using the p-distance'+
    ' method and look for the word "n/c" in place of the pairwise distance value.';


type
  TSearchResult = class
  public
    Left: Integer;
    Top: Integer;
    Right: Integer;
    UserInput: AnsiString; // what the user actually typed in to search for.
    SearchStr: AnsiString; // what you searched for which returned this match. May be a regex, or be expanded somehow.  A?GG?C or AC[T|U]C
    MatchStr: AnsiString; // the string which matched the SearchStr using whatever method of comparison.
    constructor Create(const FLeft: Integer; const FTop: Integer; const FRight: Integer; const FUserInput: AnsiString; const FSearchStr: AnsiString; const FMatchStr: AnsiString);  overload;
    constructor Create(); overload;
    procedure Assign(Source: TSearchResult);
  end;

type
  TSearchResults = Array of TSearchResult;


  function ConstraintConsistencyStatusString(AStatus: TConstraintConsistencyStatus): String;

implementation

{ TSearchResult }
constructor TSearchResult.Create(const FLeft, FTop, FRight: Integer; const FUserInput, FSearchStr, FMatchStr: AnsiString);
begin
  Left := FLeft;
  Top := FTop;
  Right := FRight;
  UserInput := FUserInput;
  SearchStr := FSearchStr;
  MatchStr := FMatchStr;
end;

procedure TSearchResult.Assign(Source: TSearchResult);
begin
  Left := Source.Left;
  Top := Source.Top;
  Right := Source.Right;
  UserInput := Source.UserInput;
  SearchStr := Source.SearchStr;
  MatchStr := Source.MatchStr;
end;

constructor TSearchResult.Create;
begin
  // just create the object
end;


function ConstraintConsistencyStatusString(AStatus: TConstraintConsistencyStatus): String;
begin
  Result := EmptyStr;
  case AStatus of
    ccsOk: Result := 'Ok';
    ccsOverestimated: Result := 'Over Estimated';
    ccsUnderestimated: Result := 'Under Estimated';
  else
    begin
      Assert(False, 'missing constraint constistency status handler');
    end
  end;
end;

end.


