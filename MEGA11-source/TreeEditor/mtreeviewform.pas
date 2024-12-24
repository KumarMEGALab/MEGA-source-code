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


unit MTreeViewForm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF VISUAL_BUILD}
  PrintersDlgs, mworkflow_element,
  {$ENDIF}
  msubtreedlg, MJumpToGeneDupDlg, LCLType, LCLIntf, Classes, SysUtils, FileUtil,
  htmlview, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, ActnList, StdCtrls, Spin, Buttons, IniPropStorage, MegaConsts,
  MCalibrationDlg, MComputeParsimInfo, MTreeBox, MLegendGenerator,
  MSimpleTreeNode, MTreeList, MTreeData, MLTreeAnalyzer, MCalibrationData,
  MAnalysisInfo, MPartitionList, MRelTimeComputer, MVirtualScrollbox,
  mtreeinfoform, MWebBrowser, geneduplicationwizardform, MMultiStageProgress,
  MTimeTreeWizard, MTimeTreeWizardForm, MNewickExportOptions, MFindDlg,
  MTreeRebuildProgress, mimageexportdlg, cef3types, cef3intf, cef3lcl,
  mbrowserutils, MTreeDataAdapter, mimageform, miniformstream,
  taxa_names_frame, branch_lengths_frame, node_stats_frame, frame_utils,
  layout_frame, subtree_frame, compute_frame, ancestors_frame, distance_scale_frame,
  time_scale_frame, div_times_frame, multi_trees_frame, caption_frame, gene_duplications_frame,
  collapse_nodes_frame, tree_info_frame, htmloptionsdlg, AppLinker;

const
  MIN_TAXA_FOR_DRAW_PROGRESS = 50000;
  TIME_TREE_TAB = 'Timetree';
  ORI_TREE_TAB = 'Original Tree';
  CONSENSUS_TREE_TAB = 'Consensus Tree';
  BOOT_TREE_TAB = 'Bootstrap tree';
  BOOT_CONS_TREE_TAB = 'Bootstrap consensus Tree';
  TOO_MANY_TAXA_CUTOFF = 10000; { don't do things like redraw the tree on resize if we have too many taxa}
  CURSOR_COMPRESS = 1;
  CURSOR_FLIP = 2;
  CURSOR_FLIPALL = 3;
  CURSOR_ROOT = 4;
  CURSOR_ZOOM = 5;
  CHECKMARK_INDEX = 203;
  UNCHECK_INDEX = 204;
  SHOWTIMESCALE_IMAGE = 165;
  SHOWSCALEBAR_IMAGE = 127;
  SHOWNODEIDS_IMAGE = 117;
  SHOWDATACOVERAGE_IMAGE = 164;
  SHOWDIVERGENCETIMES_IMAGE = 141;
  SHOWGENEDUPMARKERS_IMAGE = 134;
  SHOWSPECIATIONMARKERS_IMAGE = 135;
  SHOWSPECIESNAME_IMAGE = 162;
  SHOWSTATS_IMAGE = 129;
  SHOWBLEN_IMAGE = 12;
  SHOWOTUNAME_IMAGE = 163;
  RADIALTREESTYLE_IMAGE = 146;
  RECTTREESTYLE_IMAGE = 147;
  CURVEDTREESTYLE_IMAGE = 140;
  CIRCULARTREESTYLE_IMAGE = 153;
  SLANTEDTREESTYLE_IMAGE = 139;
  RIGHTARROW_IMAGE = 2;
  LEFTARROW_IMAGE = 1;
  ENTERRESIZEMODE_IMAGE = 151;
  QUITRESIZEMODE_IMAGE = 152;
  SHOWTOPOLOGY_IMAGE = 118;

  {$R Cursors.res}

type
  TTimetreeType = (tttReltimeMain, tttReltimeLocal, tttStrictClocksMain, tttStrictClocksLocal, tttNone);
  TTimeTreeTypes = set of TTimetreeType;

  TRootTreeMode = (rtTimetree, rtGeneDups);
  TCharStateType = (csNone, csDNA, csAminoAcid);

  { For the case when the user has multiple parsimony trees and they apply the
    RelTime analysis, we need to keep track of some values as they navigate
    through the trees}
  TRelTimeTreeState = record
    ShowDivergenceTimes: Boolean;
    ShowBlens: Boolean;
    ShowNodeIds: Boolean;
    ShowTimeScale: Boolean;
    ShowCalibratedNodeMarker: Boolean;
    NodeFocused: Boolean;
    FocusedIndex: Integer;
    CalibratedNodes: ArrayOfInteger;
    IsClockTree: Boolean;
    BLensActionChecked: Boolean;
    TimesActionChecked: Boolean;
    TimeScaleActionChecked: Boolean;
    NodeIdsActionChecked: Boolean;
    StateIsSaved: Boolean;
  end;

  TRelTimeTreeStateArray = array of TRelTimeTreeState;

  { TTreeViewForm }

  TTreeViewForm = class(TForm, IObservedForm)
    CorrtestAction: TAction;
    ActionImportNames: TAction;
    MenuItem118: TMenuItem;
    MenuItem119: TMenuItem;
    MenuItem120: TMenuItem;
    IQTreeMenu: TMenuItem;
    N2: TMenuItem;
    MenuItem61: TMenuItem;
    MacOSTabsPanel: TPanel;
    OriTreeBtn: TRadioButton;
    BootTreeBtn: TRadioButton;
    ReltimeTreeBtn: TRadioButton;
    SortTreeByGroup: TAction;
    ActionSortByContinent: TAction;
    ActionSortTreeByYear: TAction;
    ExportPairwiseDistancesAction: TAction;
    ActionSaveImageToEmf: TAction;
    ActionHideOverlappingDivTimes: TAction;
    ActionMolecularClock: TAction;
    ActionSaveImageToBmp: TAction;
    ActionSaveImageToSvg: TAction;
    ActionSaveImageToPng: TAction;
    DebugAction: TAction;
    CaptionSaveToTextAction: TAction;
    CaptionZoomOutAction: TAction;
    CaptionZoomInAction: TAction;
    CaptionDockInTEAction: TAction;
    CaptionShowInNewWindowAction: TAction;
    CaptionPrintAction: TAction;
    ActionAncStateExportTextExport: TAction;
    ActionAncStateExportPredictLivingSequence: TAction;
    ActionActStateExportChangesList: TAction;
    ActionActStateExportMostProbableSequenceCurrentNode: TAction;
    ActionActStateExportCurrentSite: TAction;
    ActionActStateExportMostProbableSequence: TAction;
    CaptionCopyToClipboardAction: TAction;
    ActionQuit: TAction;
    ActionPrinterSetup: TAction;
    ActionPrintSmall: TAction;
    ActionExportTreeTabular: TAction;
    ActionAncStateExportAllSites: TAction;
    ActionMarkerDisplay: TAction;
    ActionScaleBarDisplay: TAction;
    ActionLoadImages: TAction;
    ActionImportGpNames: TAction;
    ActionExportGpNames: TAction;
    ActionSaveImageToTIFF: TAction;
    ActionSaveImageToPdf: TAction;
    ActionExportPartitionList: TAction;
    ActionExportAnalysisSummary: TAction;
    ActionExportCurrentTreeClock: TAction;
    ActionExportTimetreeNexus: TAction;
    ActionSearchGeneDups: TAction;
    ActionBLenFont: TAction;
    ActionTimesFont: TAction;
    ActionDisplayGeneDupMarkers: TAction;
    ActionDisplaySpeciationMarkers: TAction;
    ActionDisplayErrorBars: TAction;
    ActionAncStateFont: TAction;
    ActionScaleBarFont: TAction;
    ActionBranchInfoFont: TAction;
    ActionTaxonFont: TAction;
    ActionMLLinearized: TAction;
    ActionRecalibrateTimetree: TAction;
    DisplayTaxaNamesAction: TAction;
    ActionDisplaySpeciesNames: TAction;
    ActionDataCoverageDisplay: TAction;
    ActionDisplaySeparately: TAction;
    ActionReltime: TAction;
    ActionConsensus: TAction;
    ActionCondensed: TAction;
    FontDialog: TFontDialog;
    CondensedTreeItem: TMenuItem;
    ConsensusTreeItem: TMenuItem;
    Buttons: TImageList;
    ArrowBtns: TImageList;
    CollapsePanel: TPanel;
    CollapseButton: TSpeedButton;
    ArrowBtnsSmall: TImageList;
    IniPropStorage1: TIniPropStorage;
    MenuItem100: TMenuItem;
    MenuItem101: TMenuItem;
    MenuItem102: TMenuItem;
    MenuItem103: TMenuItem;
    MenuItem104: TMenuItem;
    MenuItem105: TMenuItem;
    MenuItem106: TMenuItem;
    MenuItem107: TMenuItem;
    MenuItem108: TMenuItem;
    MenuItem109: TMenuItem;
    MenuItem110: TMenuItem;
    FormatCuttoffItem: TMenuItem;
    MenuItem111: TMenuItem;
    MenuItem112: TMenuItem;
    GeneDupsSpacer: TMenuItem;
    MenuItem113: TMenuItem;
    MenuItem114: TMenuItem;
    MenuItem115: TMenuItem;
    MenuItem116: TMenuItem;
    MenuItem117: TMenuItem;
    N1: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem99: TMenuItem;
    ButtonsPanel: TPanel;
    Panel13: TPanel;
    Panel2: TPanel;
    SidePanel: TScrollBox;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Splitter1: TSplitter;
    CaptionSplitter: TSplitter;
    TabSheet5: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton101: TToolButton;
    ToolButton102: TToolButton;
    ToolButton103: TToolButton;
    ToolButton104: TToolButton;
    ToolButton105: TToolButton;
    ToolButton106: TToolButton;
    ToolButton107: TToolButton;
    ToolButton108: TToolButton;
    ToolButton109: TToolButton;
    ToolButton11: TToolButton;
    ToolButton110: TToolButton;
    ToolButton111: TToolButton;
    ToolButton112: TToolButton;
    ToolButton113: TToolButton;
    ToolButton114: TToolButton;
    RelTimeBtn: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    FormattingToolsBtn: TToolButton;
    ToolButton7: TToolButton;
    TreeStyleBtn2: TToolButton;
    ToolButton9: TToolButton;
    TreeStyleOptionsItem: TMenuItem;
    FormatBranchesItem: TMenuItem;
    FormatLabelsItem: TMenuItem;
    FormatScaleItem: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    InferGeneDupsItem: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    Options1: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem81: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItem83: TMenuItem;
    MenuItem84: TMenuItem;
    MenuItem85: TMenuItem;
    MenuItem86: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    FontMenuItem: TMenuItem;
    MenuItem89: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem91: TMenuItem;
    MenuItem92: TMenuItem;
    MenuItem93: TMenuItem;
    GoToNodeBranch1: TMenuItem;
    GotoSeparator: TMenuItem;
    MenuItem94: TMenuItem;
    MenuItem95: TMenuItem;
    MenuItem96: TMenuItem;
    MenuItem97: TMenuItem;
    MenuItem98: TMenuItem;
    Exportmostprobablesequenceforcurrentlyselectednode1: TMenuItem;
    TreeOptionsPopup: TPopupMenu;
    ScrollAssist: TPanel;
    TreeStyleMenuItem: TMenuItem;
    ShowHideMenuItem: TMenuItem;
    OverlappingTaxaItem: TMenuItem;
    ChangeRootMenuItem: TMenuItem;
    UseSubtreeAttribItem: TMenuItem;
    UseGroupAttribItem: TMenuItem;
    PrintDialog: TPrintDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
    RectangularPMenuItem: TMenuItem;
    RadiationPMenuItem: TMenuItem;
    CirclePMenuItem: TMenuItem;
    TraditionalPMenuItem: TMenuItem;
    TreeStylePopupMenu: TPopupMenu;
    ReltimeMenuItem: TMenuItem;
    ToggleTimescaleAction: TAction;
    ActionDisplayDivergenceTimes: TAction;
    ActionLaunchGeneDupsSearch: TAction;
    ActionFindTaxaName: TAction;
    ActionAddCalibration: TAction;
    ActionNodeIdsDisplay: TAction;
    ActionExportAllTrees: TAction;
    ActionExportCurrentTree: TAction;
    ActionExportCurrentTreeCalibrations: TAction;
    ActionRootOnMidpoint: TAction;
    ActionAncStateExtended: TAction;
    ActionHideOverlappingTaxa: TAction;
    ActionAncStateHideAmbiguous: TAction;
    ActionAncStateShowNone: TAction;
    ActionAncStateShowMost: TAction;
    ActionRootByOutgroup: TAction;
    MenuItem17: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem32: TMenuItem;
    ExtCharAncPopupItem: TMenuItem;
    DrawOptionsItem: TMenuItem;
    MenuItem27: TMenuItem;
    ExtCharAncItem: TMenuItem;
    FileOpenDialog: TOpenDialog;
    FileSaveDialog: TSaveDialog;
    ShowCharStateItem: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    CharStatePopupMenu: TPopupMenu;
    ShowAllAncPopupItem: TMenuItem;
    ShowMostProbAncPopupItem: TMenuItem;
    HideAmbigAncPopupItem: TMenuItem;
    NoneAncPopupItem: TMenuItem;
    RootTreeFinishedAction: TAction;
    CaptionHideAction: TAction;
    ActionLinearized: TAction;
    ActionAncStateShowAll: TAction;
    ActionBranchInfoDisplay: TAction;
    ActionInputOrderTree: TAction;
    ActionBalancedTree: TAction;
    ActionSubtreeOption: TAction;
    ActionFormattingTools: TAction;
    ActionPoint: TAction;
    ActionInfo: TAction;
    ActionCopy: TAction;
    ActionPrint: TAction;
    ActionSaveSession: TAction;
    ActionCompress: TAction;
    ActionAutoSize: TAction;
    ActionSwap: TAction;
    ActionRootOnBranch: TAction;
    ActionResize: TAction;
    ActionFlip: TAction;
    ActionTopology: TAction;
    ActionBranchLengthDisplay: TAction;
    ActionRadial: TAction;
    ActionCircular: TAction;
    ActionCurved: TAction;
    ActionRectangular: TAction;
    ActionSlanted: TAction;
    TreeActionList: TActionList;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    FileMenu: TMenuItem;
    SearchMenu: TMenuItem;
    ImageMenu: TMenuItem;
    SubtreeMenu: TMenuItem;
    ViewMenu: TMenuItem;
    ComputeMenu: TMenuItem;
    HelpMenu: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    ArrangeTaxaMenuItem: TMenuItem;
    SortClusterShapeItem: TMenuItem;
    SortClusterOrderItem: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miCaptionExpert: TMenuItem;
    BottomPanel: TPanel;
    CaptionPanel: TPanel;
    BranchStyleMenu: TPopupMenu;
    TreePopupMenu: TPopupMenu;
    ProgressBar1: TProgressBar;
    TreeStatusBar: TStatusBar;
    TreeTabControl: TTabControl;
    procedure ActionActStateExportChangesListExecute(Sender: TObject);
    procedure ActionActStateExportCurrentSiteExecute(Sender: TObject);
    procedure ActionActStateExportMostProbableSequenceCurrentNodeExecute(
      Sender: TObject);
    procedure ActionActStateExportMostProbableSequenceExecute(Sender: TObject);
    procedure ActionAddCalibrationExecute(Sender: TObject);
    procedure ActionAncStateExportAllSitesExecute(Sender: TObject);
    procedure ActionAncStateExportPredictLivingSequenceExecute(Sender: TObject);
    procedure ActionAncStateExportTextExportExecute(Sender: TObject);
    procedure ActionAncStateExtendedExecute(Sender: TObject);
    procedure ActionAncStateFontExecute(Sender: TObject);
    procedure ActionAncStateHideAmbiguousExecute(Sender: TObject);
    procedure ActionAncStateShowAllExecute(Sender: TObject);
    procedure ActionAncStateShowMostExecute(Sender: TObject);
    procedure ActionAncStateShowNoneExecute(Sender: TObject);
    procedure ActionAutoSizeExecute(Sender: TObject);
    procedure ActionBalancedTreeExecute(Sender: TObject);
    procedure ActionBLenFontExecute(Sender: TObject);
    procedure ActionBranchInfoDisplayExecute(Sender: TObject);
    procedure ActionBranchInfoFontExecute(Sender: TObject);
    procedure ActionBranchLengthDisplayExecute(Sender: TObject);
    procedure ActionCircularExecute(Sender: TObject);
    procedure ActionCompressExecute(Sender: TObject);
    procedure ActionCondensedExecute(Sender: TObject);
    procedure ActionConsensusExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionCurvedExecute(Sender: TObject);
    procedure ActionDataCoverageDisplayExecute(Sender: TObject);
    procedure ActionDisplayDivergenceTimesExecute(Sender: TObject);
    procedure ActionDisplayErrorBarsExecute(Sender: TObject);
    procedure ActionDisplayGeneDupMarkersExecute(Sender: TObject);
    procedure ActionDisplaySeparatelyExecute(Sender: TObject);
    procedure ActionDisplaySpeciationMarkersExecute(Sender: TObject);
    procedure ActionDisplaySpeciesNamesExecute(Sender: TObject);
    procedure ActionExportAllTreesExecute(Sender: TObject);
    procedure ActionExportAnalysisSummaryExecute(Sender: TObject);
    procedure ActionExportCurrentTreeCalibrationsExecute(Sender: TObject);
    procedure ActionExportCurrentTreeClockExecute(Sender: TObject);
    procedure ActionExportCurrentTreeExecute(Sender: TObject);
    procedure ActionExportGpNamesExecute(Sender: TObject);
    procedure ActionExportPartitionListExecute(Sender: TObject);
    procedure ActionExportTimetreeNexusExecute(Sender: TObject);
    procedure ActionExportTreeTabularExecute(Sender: TObject);
    procedure ActionFindTaxaNameExecute(Sender: TObject);
    procedure ActionFlipExecute(Sender: TObject);
    procedure ActionFormattingToolsExecute(Sender: TObject);
    procedure ActionHideOverlappingDivTimesExecute(Sender: TObject);
    procedure ActionHideOverlappingTaxaExecute(Sender: TObject);
    procedure ActionImportGpNamesExecute(Sender: TObject);
    procedure ActionImportNamesExecute(Sender: TObject);
    procedure ActionInfoExecute(Sender: TObject);
    procedure ActionInputOrderTreeExecute(Sender: TObject);
    procedure ActionLaunchGeneDupsSearchExecute(Sender: TObject);
    procedure ActionLinearizedExecute(Sender: TObject);
    procedure ActionLoadImagesExecute(Sender: TObject);
    procedure ActionMarkerDisplayExecute(Sender: TObject);
    procedure ActionMolecularClockExecute(Sender: TObject);
    procedure ActionPrinterSetupExecute(Sender: TObject);
    procedure ActionPrintSmallExecute(Sender: TObject);
    procedure ActionQuitExecute(Sender: TObject);
    procedure ActionSaveImageToBmpExecute(Sender: TObject);
    procedure ActionSaveImageToEmfExecute(Sender: TObject);
    procedure ActionSaveImageToPdfExecute(Sender: TObject);
    procedure ActionSaveImageToPngExecute(Sender: TObject);
    procedure ActionSaveImageToSvgExecute(Sender: TObject);
    procedure ActionSaveImageToTIFFExecute(Sender: TObject);
    procedure ActionScaleBarDisplayExecute(Sender: TObject);
    procedure ActionSearchGeneDupsExecute(Sender: TObject);
    procedure ActionSortByContinentExecute(Sender: TObject);
    procedure ActionSortTreeByYearExecute(Sender: TObject);
    procedure CaptionCopyToClipboardActionExecute(Sender: TObject);
    procedure CaptionDockInTEActionExecute(Sender: TObject);
    procedure CaptionPrintActionExecute(Sender: TObject);
    procedure CaptionSaveToTextActionExecute(Sender: TObject);
    procedure CaptionShowInNewWindowActionExecute(Sender: TObject);
    procedure CaptionSplitterMoved(Sender: TObject);
    procedure CaptionZoomInActionExecute(Sender: TObject);
    procedure CaptionZoomOutActionExecute(Sender: TObject);
    procedure ClearAllSubtreeOptionSubmenuItemClick(Sender: TObject);
    procedure ClearSubtreeOptionSubmenuItemClick(Sender: TObject);
    procedure CollapsePanelMouseEnter(Sender: TObject);
    procedure CollapsePanelMouseLeave(Sender: TObject);
    procedure CorrtestActionExecute(Sender: TObject);
    procedure DebugActionExecute(Sender: TObject);
    procedure DrawOptionsItemClick(Sender: TObject);
    procedure ExportPairwiseDistancesActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormatBranchesItemClick(Sender: TObject);
    procedure FormatCuttoffItemClick(Sender: TObject);
    procedure FormatLabelsItemClick(Sender: TObject);
    procedure FormatScaleItemClick(Sender: TObject);
    procedure FormattingToolsBtnClick(Sender: TObject);
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
    procedure GoToNodeBranch1Click(Sender: TObject);
    procedure GroupOptionSubmenuItemClick(Sender: TObject);
    procedure HelpMenuClick(Sender: TObject);
    procedure IniPropStorage1RestoreProperties(Sender: TObject);
    procedure IniPropStorage1SaveProperties(Sender: TObject);
    procedure MenuItem99Click(Sender: TObject);
    procedure CollapsePanelClick(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure Panel2MouseEnter(Sender: TObject);
    procedure Panel2MouseLeave(Sender: TObject);
    procedure SortTreeByGroupExecute(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure SubtreeOptionSubmenuItemClick(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure TreeActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionMLLinearizedExecute(Sender: TObject);
    procedure ActionNodeIdsDisplayExecute(Sender: TObject);
    procedure ActionPointExecute(Sender: TObject);
    procedure ActionPrintExecute(Sender: TObject);
    procedure ActionRadialExecute(Sender: TObject);
    procedure ActionRecalibrateTimetreeExecute(Sender: TObject);
    procedure ActionRectangularExecute(Sender: TObject);
    procedure ActionReltimeExecute(Sender: TObject);
    procedure ActionResizeExecute(Sender: TObject);
    procedure ActionRootByOutgroupExecute(Sender: TObject);
    procedure ActionRootOnBranchExecute(Sender: TObject);
    procedure ActionRootOnMidpointExecute(Sender: TObject);
    procedure ActionSaveSessionExecute(Sender: TObject);
    procedure ActionScaleBarFontExecute(Sender: TObject);
    procedure ActionSlantedExecute(Sender: TObject);
    procedure ActionSubtreeOptionExecute(Sender: TObject);
    procedure SetupSubtreeOptionsDlg(NodeAttrib: TNodeAttrib; onFinished: TProcessMessageReceivedProc; nodeID: Integer; isGroupOption: Boolean = False);
    procedure ActionSwapExecute(Sender: TObject);
    procedure ActionTaxonFontExecute(Sender: TObject);
    procedure ActionTimesFontExecute(Sender: TObject);
    procedure ActionTopologyExecute(Sender: TObject);
    procedure CaptionHideActionExecute(Sender: TObject);
    procedure DisplayTaxaNamesActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RootTreeFinishedActionExecute(Sender: TObject);
    procedure SiteNumSEditChange(Sender: TObject);
    procedure ToggleTimescaleActionExecute(Sender: TObject);
    procedure TreeNumSEditChange(Sender: TObject);
    procedure TreeStyleButtonClick(Sender: TObject);
    procedure TreeStyleOptionsItemClick(Sender: TObject);
    procedure TreeTabControlChange(Sender: TObject);
    procedure TreeTabControlChangeBounds(Sender: TObject);
    procedure UseGroupAttribItemClick(Sender: TObject);
    procedure UseSubtreeAttribItemClick(Sender: TObject);
    procedure MacTabButtonClick(Sender: TObject);
  private
    FMacTabButtonsAreUpdating: Boolean;
    FInternalOutgroupTaxa: TStringList;
    FSidePanelInitialized: Boolean;
    FFormObservers: TFormObserverList;
    FOnCloseNotify: TNotifyEvent;
    FCaptionHtml: String;
    FOkButton: TImage;
    FCancelButton: TImage;
    FImageExportOptionsDlg: TImageExportForm;
    FRebuildingTree: Boolean;
    FRebuildProgress: TRebuildTreeProgress;
    FLastUpdateTime: TDateTime;
    FIsLoadingSession: Boolean;
    FJumpToGeneDupDlg: TJumpToGeneDupForm;
    FTimeTreeTypes: TTimeTreeTypes;
    SubtreeDlg: TSubtreeDlg;
    FGeneDupWizardForm: TGDWizardForm;
    FIgnoreProgressUpdates: Boolean;
    FIsTreeSession: Boolean;
    FOutgroupRoot: Integer; { index of the node whose subtree comprises the outgroup. Needed for rooting timetrees}
    CurTool      : TAction;  // ptr to current subtree format tool
    MAI: TAnalysisInfo;
    MLAnalyzer: TMLTreeAnalyzer;
    InfoDisplay: TTreeInfoForm;
    //FormatDlg: TFormatDlg;
    MyFindDlg: TFindDlg;
    codonsUsedinAnalysis: String;
    D: PDistanceMatrix;
    DataOpened: boolean;
    DataSaved: boolean;
    DoMolClockTest: Double;
    FCalibrationDlg: TCalibrationDlg;
    FRootingToolsEnabled: Boolean;
    FCalibrationsActionEnabled: Boolean;
    FIsRelTimeTreeSession: Boolean;
    FComputeParsimInfo: TComputeParsimInfo;
    FHideAmbigAncStateProc: TAncStateProc;
    FJustRootingATree: Boolean;
    FRelTimeActionDisabled: Boolean;
    FRelTimeActionVisible: Boolean;
    FShowAllAncStateProc: TAncStateProc;
    FShowExtCharAncStateProc: TAncStateProc;
    FShowMostProbAncStateProc: TAncStateProc;
    FTree: TTreeBox;
    FHasSpeciesNames: Boolean;
    FUseMLForReltime: Boolean; // if true, the ML framework will be used for time tree analysis launched from here
    FTimeTreeWizard: TTimeTreeWizard;
    FTimeTreeWizardForm: TTimeTreeWizardForm;
    FFormHeight: Integer; { used to reset form height after doing reltime calculation}
    FCalibrations: TCalibrations;
    FRelTimeTreeStates: TRelTimeTreeStateArray; { for reltime with multiple parsimony trees}
    FTreeNumSEditPrevVal: Integer;
    FRelTimeAnalyzer: TRelTimeComputer;

    SessionFileName: String;
    GroupFileName: String;
    MousePos: TPoint;
    ScrollPos: TPoint;
    FDragStartPoint: TPoint;
    FDragLastPoint : TPoint;
    FStartPixelsPerOTU: Integer;
    FStartTreeWidth: Integer;
    FStartTreeHeight: Integer;
    FStartTreeRadius: Integer;
    FIsComputingMPBLens: Boolean;
    CurAncState: TAction;

    MethodInfoType: array[0..10] of String;
    MethodInfo:     array[0..10] of String;

    TreeInfoType: array[0..10] of String;  // keep types of info
    TreeInfo:     array[0..10] of String;  // keep the actual info
    MaxTreeInfoIndex: Integer;

    FBInfoTypeStrings: array[0..10] of String;
    FBInfoStrings:     array[0..10] of String;
    FMaxBInfoIndex: Integer;

    AncStateInfoType: array[0..30] of String;
    AncStateInfo:     array[0..30] of String;
    AncStateList:     array[0..20] of Char;
    NoOfAncStates: integer;
    MaxAncStateIndex: Integer;

    AncRateInfoType: array[0..30] of String;
    AncRateInfo:     array[0..30] of String;
    MaxAncRateIndex: Integer;
    FNewickFileName: String;
    CaptionViewer: TChromium;
    FCapViewer: TChromiumBrowser; { one of the three below, corresponding the tree being displayed}
    FOriTreeCapViewer: TChromiumBrowser;
    FBootTreeCapViewer: TChromiumBrowser;
    FReltimeTreeCapViewer: TChromiumBrowser;
    FCaptionPanelHeight: Integer;
    FCaptionFontSize: Integer;
    FCaptionWasVisible: Boolean;
    FTreeNameHighlighted: String;
    FInstructionsLabel: TLabel;
    FTreeAdapter: TSimpleTreeDataAdapter;
    FFormIsClosing: Boolean;  // prevents an error which tries to redraw the tree when the form is begin freed.
    FCheckStateChanging: Boolean;
    FTreeInfoFrame: TTreeInfoFrame;
    FCollapseNodesFrame: TCollapseNodesFrame;
    FGeneDuplicationsFrame: TGeneDuplicationsFrame;
    FCaptionFrame: TCaptionFrame;
    FMultiTreesFrame: TMultiTreesFrame;
    FDivTimesFrame: TDivergenceTimesFrame;
    FTaxaNamesFrame: TTaxaNamesFrame;
    FBLensFrame: TBranchLengthsFrame;
    FDistScaleFrame: TDistanceScaleFrame;
    FTimeScaleFrame: TTimeScaleFrame;
    FNodeStatsFrame: TNodeStatsFrame;
    FLayoutFrame: TLayoutFrame;
    FSubtreeFrame: TSubtreeFrame;
    FComputeFrame: TComputeFrame;
    FAncestorsFrame: TAncestorsFrame;
    FOtuInfos: TList;
    procedure ClearOtuInfos;
    procedure SetOtuInfos(aInfos: TList);
    function NextAncestralStateChange: Int64;
    function PreviousAncestralStateChange: Int64;
    procedure NextAncestralStateChangeNotify(b: TObject);
    procedure PrevAncestralStateChangeNotify(b: TObject);
    procedure PropagateTreeStyleChange;
    function CurAncStateCaption: String;
    procedure ShowHideMacTabButtons;
    procedure InitFormPropertyStorage;
    procedure GetStrictClockMinMaxTimes(var minTimes: ArrayOfExtended; var maxTimes: ArrayOfExtended);
    procedure DoImageExport(aFormat: TImageFileFormat);
    function GetIsMLStr: String;
    function GetTimeTreeTypeStr: String;
    procedure SetFormIsClosing(AValue: Boolean);
    function UpdateCaptionForAncestralStates: Boolean;
    procedure CutoffDlgCancelledCallback(Sender: TObject);
    procedure ShowOptionsDialog;
    procedure HideOptionsDialog;
    procedure InitInstructionsLabel;
    procedure OkButtonMouseEnter(Sender: TObject);
    procedure OkButtonMouseLeave(Sender: TObject);
    procedure CancelButtonMouseEnter(Sender: TObject);
    procedure CancelButtonMouseLeave(Sender: TObject);
    procedure ReEnableCallback(Sender: TObject);
    procedure ValidateCheckbox(c: TObject);
    procedure ValidateRadioButton(r: TObject);
    procedure ValidateIntSpinEdit(s: TObject);
    procedure ValidateFloatSpinEdit(s: TObject);
    procedure ValidateComboBox(c: TObject);
    procedure ValidateButton(b: TObject);
    procedure ValidateSpeedButton(b: TObject);
    procedure ValidateTrackBar(t: TObject);
    procedure ValidateEdit(e: TObject);
    procedure ToggleTaxaNames;
    procedure ToggleTaxaNamesNotify(aCheckBx: TObject);

    procedure ToggleBLensNotify(c: TObject);
    procedure HideBLensShorterThanNotify(s: TObject);
    procedure HideBlensShortThanCheckBoxNotify(c: TObject);
    procedure BLensPrecisionNotify(s: TObject);
    procedure BLensFontSizeNotify(s: TObject);
    procedure BLensPlacementNotify(c: TObject);
    procedure HideOverlappingTaxaNotify(aCheckBx: TObject);
    procedure UpdateMarkerDisplay;
    procedure MarkerDisplayChangedNotify(c: TObject);
    procedure EditMarkersNotify(b: TObject);
    procedure ArrangeTaxaNotify(r: TObject);

    procedure ToggleStatsNotify(c: TObject);
    procedure StatsFontSizeNotify(s: TObject);
    procedure StatsPlacementNotify(c: TObject);
    procedure StatsHideShorterThanNotify(c: TObject);
    procedure StatsHideShorterThanSpinEditNotify(s: TObject);
    procedure StatsHorizDistNotify(s: TObject);
    procedure StatsVertDistNotify(s: TObject);
    procedure DisplayTypeChange(r: TObject);
    procedure UpdateStatsDisplay;

    procedure LayoutTreeStyleNotify(b: TObject);
    procedure LayoutTreeWidthNotify(t: TObject);
    procedure LayoutTreeHeightNotify(t: TObject);

    procedure DistScaleDisplayNotify(c: TObject);
    procedure DistScaleFontSizeNotify(s: TObject);
    procedure DistScaleCaptionEditNotify(e: TObject);
    procedure DistScaleTickIntervalNotify(s: TObject);
    procedure DistScaleLengthNotify(s: TObject);
    procedure DistScaleLineWidthNotify(c: TObject);

    procedure TimescaleDisplayNotify(c: TObject);
    procedure TimescaleLineWidthNotify(c: TObject);
    procedure TimescaleFontSizeNotify(s: TObject);
    procedure TimescaleCaptionEditNotify(e: TObject);
    procedure TimescaleMinorTickNotify(s: TObject);
    procedure TimescaleMajorTickNotify(s: TObject);
    procedure TimescaleHeightErrBarsNotify(c: TObject);

    procedure DivTimesDisplayNotify(c: TObject);
    procedure DivTimesFontNotify(b: TObject);
    procedure DivTimesFontSizeNotify(s: TObject);
    procedure DivTimesPrecisionNotify(s: TObject);

    procedure SubtreeUseSubtreeAttribsNotify(c: TObject);
    procedure SubtreeUseGroupAttribsNotify(c: TObject);

    procedure ComputeConsensusNotify(b: TObject);
    procedure ComputeCondensedNotify(b: TObject);
    procedure ConsensusCutoffChangedNotify(s: TObject);
    procedure CondensedCutoffChangedNotify(s: TObject);

    procedure AncRadioGroupNotify(r: TObject);
    procedure AncExtCharNotfity(c: TObject);
    procedure AncFontNotify(b: TObject);
    procedure AncFontSizeNotify(s: TObject);
    procedure AncExportBtnNotify(s: TObject);
    procedure UpdateAncestorRadioBtns;

    procedure CollapsePanelsChbxNotify(Sender: TObject);
    procedure CollapsePanelsTargetNotify(Sender: TObject);
    procedure InitMainMenu;
    procedure InitPopupMenus;
    procedure InitTreeOptionsPopupMenu;
    procedure TaxaFontSizeChange(aSpinEdit: TObject);

    procedure CaptionToggleDisplayNotify(c: TObject);
    procedure GeneDupsDisplaySourceNotify(r: TObject);
    procedure InitTreeInfoFrame;
    procedure InitCollapseNodesFrame;
    procedure InitGeneDuplicationsFrame;
    procedure InitCaptionFrame;
    procedure InitMultiTreesFrame;
    procedure InitDivTimesFrame;
    procedure InitTaxaNamesFrame;
    procedure InitBLensFrame;
    procedure InitDistScaleFrame;
    procedure InitDistScaleSpinEdits(var scaleEdit: TFloatSpinEdit; var tickEdit: TSpinEdit);
    procedure InitTimeScaleFrame;
    procedure InitNodeStatsFrame;
    procedure InitLayoutFrame;
    procedure InitSubtreeFrame;
    procedure InitComputeFrame;
    procedure InitAncestorsFrame;
    function GetNodeAttribValsFromProcessMessage(NodeAttrib: TNodeAttrib; const message: ICefProcessMessage; var nodeID: Integer): Boolean;
    procedure ProcessSubtreeOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure ProcessGroupOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure ProcessTreeOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure ProcessBranchOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure ProcessScaleOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure ProcessLabelsOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure ProcessCutoffOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure ProcessNewickOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure ProcessCopyCaptionMessage(Sender: TObject; const Browser: ICefBrowser; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
    procedure ProcessSaveCaptionMessage(Sender: TObject; const Browser: ICefBrowser; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
    procedure ApplyConsensusCondensedTreeOptions(aConsensusCutoff: Integer; aCondensedCutoff: Integer);
    procedure AssignHelpContext;
    function GetNewickExportOptions: TNewickExportOptions;
    procedure GenerateCapViewerForCurrentTree;

    procedure OriTreeCapViewNotify(Sender: TObject); { notify when closed so it can be set to nil}
    procedure BootTreeCapViewNotify(Sender: TObject); { notify when closed so it can be set to nil}
    procedure ReltimeTreeCapViewNotify(Sender: TObject); { notify when closed so it can be set to nil}
    function ReltimeShouldBeEnabled: Boolean;
    procedure DecideRootOnMidpoint;
    procedure DisableActions;
    procedure EnableActions;
    procedure InitCalibrationDlg;
    procedure LaunchTimetreeAnalysis;
    procedure CancelTimetreeAnalysis;
    procedure CancelCalibrations;
    procedure CancelRecalibrateTimetree;
    procedure RecalibrateTimetree(Calibrations: TCalibrations);
    procedure ResetStatusBar;
    procedure SetupTreeBox;
    procedure ChangeTree(aTree: TTreeBox);
    procedure LoadCustomCursors;
    function GetActiveTabName: String;
    function GetAncStateCaption: String;
    function GetBLenCaption: String;
    function GetHasCalibrations: Boolean;
    function GetStatPosCaption: String;
    procedure SetHideAmbigAncStateProc(AValue: TAncStateProc);
    procedure SetShowAllAncStateProc(AValue: TAncStateProc);
    procedure SetShowExtCharAncStateProc(AValue: TAncStateProc);
    procedure SetShowMostProbAncStateProc(AValue: TAncStateProc);
    function SetOLSBranchLength(tree: TTreeData):double;
    function ShowingStrictClockTree: Boolean;
    procedure SetMLAnalyzer;
    function MLLinearizeFunc(treedata: TTreeData): double;
    function RelTimeLinearizeFunc(TreeData: TTreeData): Double;
    function AnchoredRelTimeLinearizeFunc(TreeData: TTreeData): Double;
    function RelTimeMLLinearizeFunc(TreeData: TTreeData): Double;
    function AnchoredMLRelTimeLinearizeFunc(TreeData: TTreeData): Double;
    function MLBLenFunc(treedata: TTreeData): double;
    procedure RelTimeHeightErrBarProc(NodeIndex: integer; var err0,err1: double);
    function RelTimeMinHeightfunc(NodeIndex: Integer): Double;
    function RelTimeMaxHeightfunc(NodeIndex: Integer): Double;
    function RelTimeHeightFunc(NodeIndex: Integer): Double;
    procedure DoRelTimeNoCalibrations;
    procedure SetUpRelTimeCalibrations;
    procedure RefreshCalibrationDlgTreeList;
    procedure DoRelTimeWithCalibrations;
    procedure AssignCalibrations(Calibrations: TCalibrations);
    procedure CancelCalibrationChanges;

    procedure PrepareForReltimeDisplay;
    procedure PrepareOriTreeForReltimeDisplay;
    procedure PrepareReltimeTreeForReltimeDisplay;

    procedure SetTree(AValue: TTreeBox);
    procedure SaveRelTimeTreeViewState(TreeIndex: Integer);
    procedure InitRelTimeTreeViewStates; { It is important to initialize them before the user starts interacting with the trees}
    function LoadTemplate(MAI: TAnalysisInfo; var FigureGenerator: TLegendGenerator): Boolean;
    procedure InitTrees;
    function PreserveOriginalTreeRoot: Boolean;
    procedure OnTreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnTreeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnTreeClick(x,y : integer);
    procedure TreeChange(Sender: TObject);
    procedure TreeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TreeEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure TreeSearch(Sender: TObject);
    function IsShowingBootTree: Boolean;
    function IsShowingRelTimeTree: Boolean;
    function ClearPrevHighlightedTaxa: Boolean;
    procedure TreeJumpToTreeName(TreeName: String; ColorResult: Boolean);
    procedure TreeJumpToTreeNode(Index: Integer; ColorResult: Boolean);
    procedure ScrollToShowNode(NodeIndex: Integer);
    function InResizeMode: Boolean;
    procedure QuitResizeMode;
    procedure EnterResizeMode;

    procedure InitializeMethodInfo;
    procedure RenewBranchInfo;
    procedure RenewTreeInfo;
    procedure RenewAncState;
    procedure RenewAncRate;
    function TaxaListString(otuNodes: TList): String;
    procedure SaveGroupOption(a: TNodeAttrib);
    procedure LoadGroupOption(a: TNodeAttrib);
    procedure SetComputeParsimInfo;
    function SetAverageMPBranchLength(tree: TTreeData):Double;
    procedure GetAncStateProbWYSIWYG(node, site: integer; tree: TMLTreeAnalyzer; var stateOut: AnsiString; var ProbabilityOUT: double; showoption: TAction);
    procedure InitCaptionViewer;
    procedure UpdateAncestralStateActions;
    { private declarations }
  public
    OriTree, BootTree, ReltimeTree: TTreeBox;
    CurOriTreeIndex: Integer;
    CurBootTreeIndex: Integer;
    CurReltimeTreeIndex: Integer;
    FProgress: TMultiStageProgressForm;
    KeepHidden: Boolean;
    IsSetRelTimeCalibsMode: Boolean;
    IsBootTree: boolean;

    IsFree: boolean;
    FigureGenerator : TLegendGenerator;
    RootedIndex: Integer; { for gene dups, the node/branch that the user roots on - this gets lost by ttreebox so we need to save it right away}
    IsGeneDupsMode: Boolean; { true when the user is rooting a gene tree or species tree for gene dups analysis}
    GeneDupTree: TSimpleTreeNodeArray; // owned by GeneDuplicationWizard
    GeneDupTreeList: TTreeList;  // owned by GeneDuplicationWizard
//    function GetRootedTrees(Progress: TRuntimeProgressDlg): TSimpleTreeNodeArrayArray; { gets an array of trees where each tree is rooted on a different branch; needed for finding the rootings which give the fewest number of gene duplications}
    procedure SetAncestralSiteNum(aSite: Int64);
    procedure InitSidePanel;
    procedure UpdateSidePanel;
    procedure DisableComputeFrame;
    function IsReltimeTree: Boolean;
    procedure DisableCaption;
    procedure RefreshCaption;
    procedure BeginRefresh;
    procedure EndRefresh;
    procedure UpdateRefreshProgress(AProgress: Integer);
    procedure UpdateRefreshStatus(AStatus: TTreeRebuildPhase);

    procedure SetUsingSpeciesTree(AValue: Boolean);
    procedure SetGeneTreeRooted(AValue: Boolean); { these are so that MAI gets updated but the gene dups wizard may not have a reference to it}
    procedure SetSpeciesTreeRooted(AValue: Boolean);
    procedure SetNumGeneDups(AValue: Integer);
    procedure SetNumSpeciations(AValue: Integer);
    procedure RootOnNode(Index: Integer);
    procedure GeneDupsCancelled;
    procedure GetOutgroupTaxa(var AList: TStringList);
    procedure SetOutgroupCluster(AColor: TColor; AIndex: Integer); { call this after the user has placed the root on a branch, then we mark all taxa nodes in the outgroup as being outgroup members}
    procedure ColorOutgroupClusterGray; { call this when displaying a timetree so we can gray out the outgroup cluster}
    procedure ColorOutgroupClusterBlack;
    procedure CompressOutgroup(MarkGrey: Boolean);
    function GetNewickString: String;
    procedure GetTreeData(var AData: TTreeData);
    procedure GetTreeAdapterData(aTreeData: TTreeData);
    function HasInvalidCalibrations(var Calibrations: TCalibrations): Boolean;
    procedure UpdateInfoBox;
    procedure UpdateViewWithGeneDupes(TreeList: TTreeList);
    procedure SetSpeciesNames(SpNamesList: TStringList);
    procedure PrepareToWorkWithTimeTreeWizard(AnalysisInfo: TAnalysisInfo; MakeVisible: Boolean=True);
    procedure MoveTreeToCalibrationDlg;
    procedure RetrieveTreeFromCalibrationDlg;
    procedure PrepareToRootTreeOnly(RootTreeMode: TRootTreeMode);
    property JustRootingGeneTree: Boolean read FJustRootingATree write FJustRootingATree;
    procedure LoadImagesDirect(ImagesDir: String);
    procedure ActionExportSiteRateExecute(Sender: TObject);
    function GetAnalysisType: String;
    function TryGetOriTreeBMP(var aBitmap: TBitmap; aWidth: Integer; aHeight: Integer): Boolean;

    property OnCloseNotify: TNotifyEvent read FOnCloseNotify write FOnCloseNotify;
    property CalibrationDlg: TCalibrationDlg read FCalibrationDlg;
    property ComputeParsimInfo: TComputeParsimInfo read FComputeParsimInfo write FComputeParsimInfo;
    property RelTimeActionDisabled: Boolean read FRelTimeActionDisabled write FRelTimeActionDisabled;
    property RelTimeActionVisible: Boolean read FRelTimeActionVisible write FRelTimeActionVisible;
    property CalibrationsActionEnabled: Boolean read FCalibrationsActionEnabled write FCalibrationsActionEnabled;
    property HasCalibrations: Boolean read GetHasCalibrations;
    property isTreeOpened: boolean read DataOpened;
    property isTreeSaved: boolean read DataSaved;
    property Tree: TTreeBox read FTree write SetTree;
    property DistanceMatrix: PDistanceMatrix read D write D;
    property ShowAllAncStateProc: TAncStateProc read FShowAllAncStateProc write SetShowAllAncStateProc;
    property ShowMostProbAncStateProc: TAncStateProc read FShowMostProbAncStateProc write SetShowMostProbAncStateProc;
    property HideAmbigAncStateProc: TAncStateProc read FHideAmbigAncStateProc write SetHideAmbigAncStateProc;
    property ShowExtCharAncStateProc: TAncStateProc read FShowExtCharAncStateProc write SetShowExtCharAncStateProc;
    property RelTimeAnalyzer: TRelTimeComputer read FRelTimeAnalyzer write FRelTimeAnalyzer;
    function IsML: Boolean;
    function isAncState:boolean;
    function isAncProb:boolean;

    procedure RenewForm;

    procedure ShowAllParsimonyAncState(AncState: PArrayOfString; SiteIndex : integer; ATree: TTreeData);
    procedure ShowAllParsimonyAncStateEx(AncState: PArrayOfString; SiteIndex : integer; ATree: TTreeData);
    procedure HideAmbigParsimonyAncState(AncState: PArrayOfString; SiteIndex : integer; ATree: TTreeData);

    procedure ShowAllMLAncState(AncState: PArrayOfString; SiteIndex : integer; ATree: TTreeData);
    procedure ShowMLAncState(AncState: PArrayOfString; SiteIndex : integer; ATree: TTreeData);
    procedure HideAmbigMLAncState(AncState: PArrayOfString; SiteIndex : integer; ATree: TTreeData);

    procedure SetOriTrees(newtree: TTreeList);
    procedure SetOriPartitions(newtree: TPartitionList);
    procedure SetBootTrees(newtree: TTreeList);
    procedure SetBootPartitions(newtree: TPartitionList);
    procedure SetRelTimeTrees(newtree: TTreeList);
    procedure SetGeneDuplications(AGeneTree: TSimpleTreeNodeArray); overload;
    procedure SetGroupInfo(GpName: TStringList);
    procedure SetGroupInfoAndRefresh(GpName: TStringList);
    procedure SetData(data: TAnalysisInfo);
    procedure ShowOriTree;
    procedure ShowBootTree;
    procedure ShowReltimeTree;
    function AskForFileName: Boolean;
    function SaveSession(const filename : String; var aMsg: String):boolean;
    function RetrieveSession(filename : String):boolean;
    function ImportNewickStandard(filename : String; Unroot: Boolean=False):boolean;
    function ImportFromNewickString(NewickString: String): Boolean; overload;
    function ImportFromNewickString(NewickString: AnsiString; NamesList: TStringList): Boolean; overload;
    function ImportTreeList(AList: TTreeList; doVisualUpdates: Boolean = False; Filename: String = ''): Boolean;
    procedure ShowSubtreeWindow;
    procedure UpdateClientSize;
    procedure ScrollToBottom;
    procedure ResizeTreeToFitWindow(doRefresh : Boolean = True); overload;
    procedure ResizeTreeToFitWindow(aTree: TTreeBox; doRefresh: Boolean = True); overload;
    procedure AdjustTreeForBestFitToWindow(doRefresh: Boolean=True);
    procedure InitForm;
    procedure ResetForm;
    procedure  AddFormObserver(observer: IFormObserver);
    property FormIsClosing: Boolean read FFormIsClosing write SetFormIsClosing;
  published
    property IsMLStr: String read GetIsMLStr;
    //property TimeTreeType: TTimetreeType read FTimeTreeType write FTimeTreeType;
    property TimeTreeTypeStr: String read GetTimeTreeTypeStr;
    property ActiveTabName : String read GetActiveTabName;
    property AnalysisType : String read GetAnalysisType;
    property BLenCaption: String read GetBLenCaption;
    property StatPosCaption: String read GetStatPosCaption;
    property AncStateCaption: String read GetAncStateCaption;
    property MolecularClockTest: Double read DoMolClockTest;
    property codonsUsed: String read codonsUsedinAnalysis;
  end;

var
  TreeViewForm: TTreeViewForm;

implementation

uses
  mega_main, MegaUtils, mdistpack, mtreepack, MTreeEstFunc, MPleaseWait,
  math, LResources, MegaVerConsts, mglobalsettings, mcrossprojectutils,
  MegaPrivateFiles, StringUtils, MLTree, dateutils, Clipbrd, MMatrixExport,
  mmypegmessagedlg, MMegaWindowInfo, Printers, MEditorForm, mancestralstatesexporter,
  mancestralstatesexportheader, MWriteOutputDlg, mnewickexportdlg, ExcelWrite,
  LazFileUtils, mhelpfiles, mhelpkeywords, TreeExplorer_HC, maboutbox,
  mreltimeexport, mcalibrateclockdlg, mshortcutshelper, MDisplayMatrixDlg, MOtuInfo,
  typinfo, mancestral_states_navigator, mtree_display_setup, mcorrelationtest;

{$R *.lfm}

{ TTreeViewForm }

procedure TTreeViewForm.FormCreate(Sender: TObject);
begin
  FMacTabButtonsAreUpdating := False;
  FInternalOutgroupTaxa := TStringList.Create;
  FOtuInfos := nil;
  FSidePanelInitialized := False;
  FCheckStateChanging := False;
  FFormObservers := TFormObserverList.Create;
  IniPropStorage1.IniFileName := GetPrivateFile(MEGASessionFile);
  UpdateShortcutsForMacOs(TreeActionList);
  {$IFNDEF MSWINDOWS}
  ActionSaveImageToEmf.Visible := False;
  ActionSaveImageToEmf.Enabled := False;
  {$ENDIF}
  FOnCloseNotify := nil;
  FTimeTreeTypes := [];
  TreeTabControl.TabHeight := 0;
  FInstructionsLabel := nil;
  FOkButton := nil;
  FCancelButton := nil;
  FImageExportOptionsDlg := TImageExportForm.Create(Self);
  FRebuildingTree := False;
  FRebuildProgress := TRebuildTreeProgress.Create;
  FFormIsClosing := False;
  FCaptionFontSize := 10;
  FOriTreeCapViewer := nil;
  FBootTreeCapViewer := nil;
  FReltimeTreeCapViewer := nil;
  CaptionViewer := nil;
  FLastUpdateTime := Now;
  FOutgroupRoot := -1;
  FIsComputingMPBLens := False;
  IsGeneDupsMode := False;
  FRootingToolsEnabled := True;
  FIgnoreProgressUpdates := False;
  FIsLoadingSession := False;
  FTimeTreeWizard := nil;
  FTimeTreeWizardForm := nil;
  FJumpToGeneDupDlg := nil;
  SubtreeDlg := TSubtreeDlg.Create(Self);
  SetLength(FRelTimeTreeStates, 0);
  FCalibrations := nil;
  FTreeNumSEditPrevVal := 1;
  IsSetRelTimeCalibsMode := False;
  FIsRelTimeTreeSession := False;
  FUseMLForReltime := False;
  FJustRootingATree := False;
  FHasSpeciesNames := False;
  IsFree := true;
  IsBootTree := false;
  DataOpened := false;
  DataSaved := false;
  SessionFileName := '';
  MAI := nil;
  ComputeParsimInfo := nil;
  Tree := nil;
  OriTree := nil;
  BootTree := nil;
  ReltimeTree := nil;
  InitTrees;
  CurTool := ActionPoint;
  ActionPoint.checked := True;
  CurAncState := nil;
  KeepHidden := False;
  FRebuildingTree := False;
  SetupTreeBox;
  LoadCustomCursors;
  FigureGenerator := TLegendGenerator.Create;
  FIsTreeSession := False;
  FIsRelTimeTreeSession := False;
  FProgress := TMultiStageProgressForm.Create(Self);
  InfoDisplay := TTreeInfoForm.Create(Self);
  InitTreeInfoFrame;
  //FormatDlg := TFormatDlg.Create(Self);
  MyFindDlg := nil;
  FCalibrationDlg := TCalibrationDlg.Create(Self);
  FRelTimeAnalyzer := TRelTimeComputer.Create;
  ActionLaunchGeneDupsSearch.Visible := False;
  GeneDupsSpacer.Visible := False;
  ActionDisplaySpeciesNames.Visible := False;
  FCaptionPanelHeight := CaptionPanel.Height;
  InitMainMenu;
  InitPopupMenus;
  {$IFDEF DARWIN}
  MacOSTabsPanel.Visible := True;
  {$ENDIF}
  AssignHelpContext;
  FShowAllAncStateProc := nil;
  FShowExtCharAncStateProc := nil;
  FShowMostProbAncStateProc := nil;
  FTreeAdapter := nil;
  ImageForm.UpdateImgList(Self);
end;

procedure TTreeViewForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FInternalOutgroupTaxa) then
    FInternalOutgroupTaxa.Free;
  if Assigned(FFormObservers) then
    FFormObservers.Free;
  if Assigned(FOriTreeCapViewer) then
  begin
    FOriTreeCapViewer.OnCloseNotify := nil;
    FOriTreeCapViewer.Close;
  end;
  if Assigned(FBootTreeCapViewer) then
  begin
    FBootTreeCapViewer.OnCloseNotify := nil;
    FBootTreeCapViewer.Close;
  end;
  if Assigned(FReltimeTreeCapViewer) then
  begin
    FReltimeTreeCapViewer.OnCloseNotify := nil;
    FReltimeTreeCapViewer.Close;
  end;
  if Assigned(CaptionViewer) then
    CaptionViewer.Free;
  if Assigned(FRebuildProgress) then
    FRebuildProgress.Free;
  if Assigned(FProgress) then
    FProgress.Free;
  OriTree.DistanceMatrix := nil;
  BootTree.DistanceMatrix := nil;
  ReltimeTree.DistanceMatrix := nil;
  if Assigned(FJumpToGeneDupDlg) then
    FJumpToGeneDupDlg.Free;
  if Assigned(SubtreeDlg) then
    SubtreeDlg.Free;
  if Assigned(FTreeInfoFrame) then
    FTreeInfoFrame.Clear; { frees up memory - necessary because TFrame has no destructor}
  //if Assigned(InfoDisplay) then
  //  InfoDisplay.Free;
  if Assigned(MAI) then
  begin
    if (Assigned(MAI.MyMLAnalysisPack)) and (MAI.MyMLAnalysisPack = MLAnalyzer) then
      MAI.MyMLAnalysisPack := nil;
    if (not FJustRootingATree) then // MAI is still needed by the GeneDuplicationWizard
    begin
      MAI.MyOriTreeList := nil;
      if Assigned(MAI.ARP) then
        MAI.ARP.Free;
      MAI.MyProcessPack := nil;
      MAI.Free;
    end;
  end;
  if Assigned(FCalibrationDlg) and FCalibrationDlg.Visible then
    FCalibrationDlg.Close; // call close because we have some cleanup code there. The destructor will automatically be called.
  if Assigned(FGeneDupWizardForm) then
    FGeneDupWizardForm.Release;
  if ComputeParsimInfo <> nil then
    ComputeParsimInfo.Free;
  if Assigned(FJumpToGeneDupDlg) then
    FJumpToGeneDupDlg.Free;
  try
    if Assigned(MLAnalyzer) then
      FreeAndNil(MLAnalyzer);
  except
    {$IFDEF DEBUG}
    on E:Exception do
      ShowMessage('Error freeing MLAnalyzer: ' + E.Message);
    {$ENDIF}
  end;
  if Assigned(MyFindDlg) then
    FreeAndNil(MyFindDlg);
  if Assigned(FigureGenerator) then
    FreeAndNil(FigureGenerator);
  if Assigned(FRelTimeAnalyzer) then
    FreeAndNil(FRelTimeAnalyzer);
  if Assigned(FTimeTreeWizard) then
    FreeAndNil(FTimeTreeWizard);
  if Assigned(FTimeTreeWizardForm) then
    FreeAndNil(FTimeTreeWizardForm);
  if Assigned(D) then
    FreeDistMatrix(D, OriTree.NoOfOTUs);
  D := nil;
  if Assigned(FCalibrations) then
    FreeAndNil(FCalibrations);
  if Assigned(FImageExportOptionsDlg) then
    FImageExportOptionsDlg.Free;
  if Assigned(FTreeAdapter) then
    FTreeAdapter.Free;
  RemoveWindowFromTray(Self);
  ClearOtuInfos;
end;

procedure TTreeViewForm.FormHide(Sender: TObject);
begin
  ActionInfo.Checked := False;
end;

procedure TTreeViewForm.FormResize(Sender: TObject);
begin
  if not Visible then
    Exit;
  if not FFormIsClosing then
  begin
    Tree.Invalidate;
    FCaptionPanelHeight := CaptionPanel.Height;
    if Assigned(CaptionViewer) and CaptionViewer.Visible then
      CaptionViewer.Invalidate;
    TreeStatusBar.Panels[0].Width := TreeStatusBar.Width - TreeStatusBar.Panels[1].Width - 2;
  end;
end;

procedure TTreeViewForm.FormShow(Sender: TObject);
begin
  if DataOpened then
  begin
    CurOriTreeIndex := OriTree.TreeIndex;
    CurBootTreeIndex := 1;
    CurReltimeTreeIndex := 1;
    if Assigned(MAI) and IsReltimeAnalysis(MAI.MyUsrOperation) then
      Tree := ReltimeTree
    else
      Tree := OriTree;
    InitForm;
    Tree.Refresh;
  end;
  if Top < 0 then
    Top := 0;
  AddWindowToTray(Self);
  ShowCharStateItem.Visible := isAncState;
  if OriTree <> nil then
    OriTree.HideOverlappingTaxa := ActionHideOverlappingTaxa.Checked;
  if BootTree <> nil then
    BootTree.HideOverlappingTaxa := ActionHideOverlappingTaxa.Checked;
  if ReltimeTree <> nil then
    ReltimeTree.HideOverlappingTaxa := ActionHideOverlappingTaxa.Checked;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Tree Explorer: (' + ExtractFilename(GetActiveDataFileName) + ')';
  if Assigned(FIQTreeLink) then
    Caption := Caption + ' - IQTree Search ' + FormatDateTime('hh:mm:ss', FIQTreeLink.StartTime);
  Tree.Invalidate;
  if FJustRootingATree then
  begin
    {$IFDEF DARWIN}
    TreeTabControlChangeBounds(Self);
    {$ENDIF}
  end;
end;

procedure TTreeViewForm.RootTreeFinishedActionExecute(Sender: TObject);
var
  Response: Integer;
begin
  if Tree.Height > ClientHeight then
    ScrollToBottom;
  Tree.Cursor := crDefault;
  {$IFNDEF DARWIN}
  Response := MessageDlg('Please confirm: the tree is correctly rooted with the outgroup displayed (drawn in red) at the lower part of the Tree Explorer window', mtConfirmation, mbYesNoCancel, 0, mbYes);
  {$ELSE}
  Response := mrYes;
  {$ENDIF}
  if Response = mrYes then
    ModalResult := mrOk
  else if Response = mrCancel then
    ModalResult := mrCancel;
end;

procedure TTreeViewForm.SiteNumSEditChange(Sender: TObject);
var
  se: TSpinEdit = nil;
begin
  ValidateIntSpinEdit(Sender);
  se := TSpinEdit(Sender);
  if (se.Value <= 0) or ((se.Value - 1) > Tree.MaxSiteIndex) then
    Exit;
  if Tree.SiteIndex <> (se.Value - 1) then
  begin
    OriTree.SiteIndex := se.Value - 1;
    BootTree.SiteIndex := OriTree.SiteIndex;
    ReltimeTree.SiteIndex := OriTree.SiteIndex;
  end;
  Tree.Invalidate;
  RenewAncState;
  RefreshCaption;
end;

procedure TTreeViewForm.ToggleTimescaleActionExecute(Sender: TObject);
begin
  ToggleTimeScaleAction.Checked := not ToggleTimeScaleAction.Checked;
  Tree.ShowTimeScale := ToggleTimeScaleAction.Checked;
  Tree.ShowCalibratedNodeMarker := Tree.ShowTimeScale;
  Tree.Invalidate;
end;

procedure TTreeViewForm.TreeNumSEditChange(Sender: TObject);
var
  NewTreeIndex: Integer; // zero-based index (the spin edit starts at 1)
  se: TSpinEdit = nil;

  procedure UpdateCurrentState;
  begin
    if Length(FRelTimeTreeStates) = 0 then
      Exit;
    NewTreeIndex := FMultiTreesFrame.TreeNumSEdit.Value - 1;

    if not FRelTimeTreeStates[NewTreeIndex].StateIsSaved then
      Exit;
    { TODO 1 -oglen -creltime : need to update reltime sessions stuff for the case of multiple parsimony trees }
    Tree.ShowDivergenceTimes                 := FRelTimeTreeStates[NewTreeIndex].ShowDivergenceTimes;
    Tree.ShowBLen                            := FRelTimeTreeStates[NewTreeIndex].ShowBlens;
    Tree.ShowNodeIds                         := FRelTimeTreeStates[NewTreeIndex].ShowNodeIds;
    Tree.ShowCalibratedNodeMarker            := FRelTimeTreeStates[NewTreeIndex].ShowCalibratedNodeMarker;
    Tree.ShowTimeScale                       := FRelTimeTreeStates[NewTreeIndex].ShowTimeScale;
    Tree.CalibratedNodes                     := FRelTimeTreeStates[NewTreeIndex].CalibratedNodes;
    ActionBranchLengthDisplay.Checked        := FRelTimeTreeStates[NewTreeIndex].BLensActionChecked;
    ActionDisplayDivergenceTimes.Checked     := FRelTimeTreeStates[NewTreeIndex].TimesActionChecked;
    ToggleTimeScaleAction.Checked            := FRelTimeTreeStates[NewTreeIndex].TimeScaleActionChecked;
    ActionNodeIdsDisplay.Checked             := FRelTimeTreeStates[NewTreeIndex].NodeIdsActionChecked;
    if FRelTimeTreeStates[NewTreeIndex].NodeFocused then
      Tree.FocusOnNode(FRelTimeTreeStates[NewTreeIndex].FocusedIndex);
  end;

begin
  if IsRelTimeTree and (Length(FReltimeTreeStates) = 0) then
    Exit;
  ValidateIntSpinEdit(Sender);
  se := TSpinEdit(Sender);
  SaveRelTimeTreeViewState(FTreeNumSEditPrevVal - 1);

  try
    FMultiTreesFrame.TreeNumSEdit.Enabled := False;
    Tree.TreeIndex := se.Value;
    if Tree = OriTree then
        CurOriTreeIndex := Tree.TreeIndex
    else if Tree = BootTree then
        CurBootTreeIndex := Tree.TreeIndex;

    UpdateCurrentState;

    if ActionRootOnMidpoint.Checked then
      Tree.MakeRootOnMidpoint
    else if (FInternalOutgroupTaxa.Count > 0) and (not Tree.IsOutgroup) then
      Tree.MakeRootByInternalOutgroup(FInternalOutgroupTaxa, True);

    if SortClusterShapeItem.Checked then
      Tree.SortClusterForShape
    else if SortClusterOrderItem.Checked and (FInternalOutgroupTaxa.Count = 0) then
      Tree.SortClusterInOrder;
    UpdateSidePanel;
    Tree.Refresh;
    ResetStatusBar;
    if InfoDisplay.Visible then
    begin
      RenewTreeInfo;
      RenewBranchInfo;
    end;
  finally
    FMultiTreesFrame.TreeNumSEdit.Enabled := True;
  end;
end;

procedure TTreeViewForm.TreeStyleButtonClick(Sender: TObject);
var
  target: TPoint;
begin
  target.x := FLayoutFrame.TreeStyleBtn.ClientOrigin.x;
  target.y := FLayoutFrame.TreeStyleBtn.ClientOrigin.y + FLayoutFrame.TreeStyleBtn.Height;
  TreeStylePopupMenu.Popup(target.X, target.Y);
end;

procedure TTreeViewForm.TreeStyleOptionsItemClick(Sender: TObject);
var
  HorzFactor{, HorzRatio}: Double;
  js: String;
  blen, separation, aWidth: Integer;
  //maxBlen: Integer; why is this not used?
  radius, angle: Integer;
begin
  //HorzRatio := Tree.TreeWidth/Tree.PixelsPerUnit;
  HorzFactor := Power(10, 2-Floor(Log10(Tree.PixelsPerUnit)));
  //maxBlen := Floor(8190*HorzFactor/HorzRatio);
  blen := Round(Tree.PixelsPerUnit*HorzFactor);
  aWidth := Tree.TreeWidth;
  separation := Tree.PixelsPerOTU;
  radius := Tree.Radius;
  angle := Tree.StartAngle;

  js := EmptyStr;

  try
    ShowOptionsDialog;
    HtmlOptionsDialog.ProcessMessageReceivedProc := ProcessTreeOptionsMessage;
    HtmlOptionsDialog.DomProcedureName := VISITDOMPROC_TREE_OPTIONS_TREE;
    js := Format('$("#%s").spinner("value", "%d");', [TREE_OPTIONS_RECT_TAXON_SEPARATION, separation]) + LineEnding;
    if Tree.ShowTopologyOnly or (not Tree.isBranchLength) then
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_RECT_BRANCH_LENGTH])
    else
      js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_RECT_BRANCH_LENGTH, blen]) + LineEnding;
    js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_RECT_WIDTH, aWidth]) + LineEnding;
    js := js + Format('$("#%s").attr("ischecked", "%s");', [TREE_OPTIONS_TREE_TAXON_NAME_CIRCLE, BoolToStr(Tree.HorzTaxonName, True)]) + LineEnding;
    js := js + Format('$("#%s").prop("checked", %s);', [TREE_OPTIONS_TREE_TAXON_NAME_CIRCLE, BoolToStrLC(Tree.HorzTaxonName, True)]) + LineEnding;
    js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_CIRCLE_START_ANGLE, angle]) + LineEnding;
    js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_CIRCLE_RADIUS, radius]) + LineEnding;
    js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_CIRCLE_CENTER_HOLE, Tree.CenterMargin]) + LineEnding;
    js := js + Format('$("#%s").attr("ischecked", "%s");', [TREE_OPTIONS_TREE_TAXON_NAME_RAD, BoolToStr(Tree.HorzTaxonName, True)]) + LineEnding;
    js := js + Format('$("#%s").prop("checked", %s);', [TREE_OPTIONS_TREE_TAXON_NAME_RAD, BoolToStrLC(Tree.HorzTaxonName, True)]) + LineEnding;
    js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_RAD_BRANCH_LENGTH, blen]) + LineEnding;
    js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_RAD_START_ANGLE, angle]);
    case Tree.TreeStyle of
      tsRadiation: js := js + '$("#accordion").accordion("option", "active", 2);';
      tsCircle: js := js + '$("#accordion").accordion("option", "active", 1);';
    end;
    HtmlOptionsDialog.LoadOptionsFile(wofTreeOptionsTreeStyleFile, js, 'Tree Drawing Options', 350, 580, HC_Tree_tab_in_Format_dialog_box);
  except
   On E: Exception do
   begin
     ShowMessage('Oh no! An error has occurred: ' + E.Message);
     HideOptionsDialog;
   end;
  end;
end;

procedure TTreeViewForm.ActionPointExecute(Sender: TObject);
begin
  CurTool.Checked := false;
  CurTool := ActionPoint;
  CurTool.Checked := true;
  Tree.Cursor := crDefault;
  FSubtreeFrame.SubtreePointerBtn.Down := True;
end;

procedure TTreeViewForm.ActionPrintExecute(Sender: TObject);
begin
  try
   {$IFDEF DARWIN}
   ShowMessage('Printing not yet implemented for macOS. Please save this page as a PDF instead.');
   Exit;
   {$ENDIF}
    if PrintDialog.Execute then
      Tree.Print;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when printing the tree image: ' + E.Message);
  end;
end;

procedure TTreeViewForm.ActionRadialExecute(Sender: TObject);
begin
  if not Tree.isBranchLength then Exit;

  Tree.TreeStyle := tsRadiation;
  PropagateTreeStyleChange;
  Tree.Refresh;
  FLayoutFrame.TreeStyleBtn.ImageIndex := RADIALTREESTYLE_IMAGE;
  TreeStyleBtn2.ImageIndex := RADIALTREESTYLE_IMAGE;
  DataSaved := False;
end;

procedure TTreeViewForm.ActionRecalibrateTimetreeExecute(Sender: TObject);
var
  calibrationType: TCalibrationTarget = ctInternalNode;
begin
  if OriTree.NamesAreChanged then
  begin
    ShowMessage('The timetree analysis cannot be redone once taxa names are edited');
    Exit;
  end;
  if not PromptForCalibrationsType(calibrationType) then
    Exit;

  try
    if InfoDisplay.Visible then
      InfoDisplay.Hide;
    FFormHeight := Self.Height;
    Self.Hide;
    Tree := OriTree;
    FUseMLForReltime := Assigned(MLAnalyzer);
    UpdateClientSize;
    MoveTreeToCalibrationDlg;
    if CalibrationDlg.NumCalibrations > 0 then
    begin
      if not Assigned(FCalibrations) then
        FCalibrations := TCalibrations.Create;
      CalibrationDlg.AssignCalibrations(FCalibrations);
    end;
    CalibrationDlg.CalibrationsType := calibrationType;
    CalibrationDlg.CalibrationWizard := nil;
    CalibrationDlg.CWCancelCallBack := CancelRecalibrateTimetree;
    CalibrationDlg.CWSaveChangesCallBack := RecalibrateTimetree;
    CalibrationDlg.FinalizeCalibrationFunc := Tree.FinalizeCalibration;
    OriTree.AssignTreeListInternalNodeLabelFunc(FCalibrationDlg.GetInternalNodeLabelFunc);
    CalibrationDlg.ResetForm;
    CalibrationDlg.ClearCalibrations;
    RefreshCalibrationDlgTreeList;
    if not Assigned(CalibrationDlg.TreeViewForm) then
      CalibrationDlg.TreeViewForm := Self;
    CalibrationDlg.Show;
    if CalibrationType = ctLeafNode then
      CalibrationDlg.PromptIfTipDatesInNames
  except
    on E:Exception do
    begin
      ShowMessage('Oh no! An error occurred when recalibrating the timetree: ' + E.Message);
    end;
  end;
end;

procedure TTreeViewForm.ActionRectangularExecute(Sender: TObject);
begin
  Tree.TreeStyle := tsTraditional;
  Tree.BranchStyle := bsRectangular;
  if IsBootTree then
    if Tree = OriTree then
    begin
      BootTree.TreeStyle := OriTree.TreeStyle;
      BootTree.BranchStyle := OriTree.BranchStyle;
    end
    else
    begin
      OriTree.TreeStyle := BootTree.TreeStyle;
      OriTree.BranchStyle := BootTree.BranchStyle;
    end;
  Tree.Refresh;
  FLayoutFrame.TreeStyleBtn.ImageIndex := RECTTREESTYLE_IMAGE;
  TreeStyleBtn2.ImageIndex := RECTTREESTYLE_IMAGE;
  TreeTabControl.Enabled := true;
  DataSaved := false;
end;

procedure TTreeViewForm.ActionReltimeExecute(Sender: TObject);
var
  TempNames: TStringList = nil;
begin
  FFormHeight := Self.Height;

  if (not Tree.IsOutgroup) and (not MegaForm.HasSequenceData) then
  begin
    ShowMessage('When an outgroup has not yet been defined, the timetree analysis requires sequence data');
    ReltimeActionDisabled := True; { if they reopen the data file, taxa mappings may not be done correctly}
    Exit;
  end;
  if OriTree.NamesAreChanged then
  begin
    ShowMessage('The timetree analysis is not supported once taxa names have been edited');
    Exit;
  end;
  if MAI.UsingAlignmentAndTreeThatDontResolve then
  begin
    ShowMessage('The timetree analysis is not supported when the sequence alignment and user tree are not perfectly resolved');
    RelTimeActionDisabled := True;
    Exit;
  end;

  try
    if InfoDisplay.Visible then
      InfoDisplay.Hide;
    ReltimeActionDisabled := True;
    FTimeTreeWizard := TTimeTreeWizard.Create;
    FTimeTreeWizardForm := TTimeTreeWizardForm.Create(Self);
    FTimeTreeWizardForm.Wizard := FTimeTreeWizard;
    FTimeTreeWizard.AnalysisInfo := MAI;
    FTimeTreeWizard.TempTreeList := TTreeList.Create;
    TempNames := Tree.GetOTUNamesList;
    FTimeTreeWizard.TempTreeList.OTUNameList.Assign(TempNames);
    Tree.AssignTreeList(FTimeTreeWizard.TempTreeList);
    FTimeTreeWizard.LaunchedFrom := ttsTreeExplorer;
    FTimeTreeWizard.WizardFormUpdate := FTimeTreeWizardForm.UpdateView;
    FTimeTreeWizard.SetupCalibrationsCallback := SetUpRelTimeCalibrations;
    FTimeTreeWizard.OutgroupsAssignedCallback := SetGroupInfoAndRefresh;
    FCalibrationDlg.SetTreeList(FTimeTreeWizard.TempTreeList);
    FCalibrationDlg.TreeViewForm := Self;
    FUseMLForReltime := Assigned(MLAnalyzer);
    FTimeTreeWizardForm.LaunchAnalysisCallback := LaunchTimetreeAnalysis;
    FTimeTreeWizardForm.CancelAnalysisCallback := CancelTimetreeAnalysis;
    FTimeTreeWizardForm.CancelCalibrationsCallback := CancelCalibrations;
    MegaForm.AddWindowToTray(FTimeTreeWizardForm);
    FTimeTreeWizardForm.Show;
    if not Tree.IsOutgroup then
      FTimeTreeWizardForm.UpdateView(ttpsDoOutgroup)
    else
      FTimeTreeWizardForm.UpdateView(ttpsDoCalibrations);
    ActionRootByOutgroupExecute(Self);
    if Tree.IsOutgroup then
    begin
      Tree.FocusOnRoot;
      Tree.SetOutgroupCluster(clSilver, Tree.NodeInfo.Des2Index);
      Tree.Refresh;
    end;
    Self.Hide;
  finally
    if Assigned(TempNames) then
      TempNames.Free;
  end;
end;

procedure TTreeViewForm.ActionResizeExecute(Sender: TObject);
begin
  if not InResizeMode then
  begin
    EnterResizeMode;
    CurTool := ActionResize;
    {$IFDEF LINUX}
    if Assigned(CaptionViewer) then
      FreeAndNil(CaptionViewer);
    InitCaptionViewer;
    RefreshCaption;
    CaptionViewer.Invalidate;
    {$ENDIF}
  end
  else
  begin
    QuitResizeMode;
    CurTool := ActionPoint;
    ActionPointExecute(Sender);
  end;
end;

procedure TTreeViewForm.ActionRootByOutgroupExecute(Sender: TObject);
begin
  Tree.MakeRootByOutgroup;

  if ActionBalancedTree.Checked then
    Tree.SortClusterForShape
  else if ActionInputOrderTree.Checked then
    Tree.SortClusterInOrder;

  Tree.Refresh;
  DataSaved := False;

  if ActionInfo.Checked then
  begin
    RenewTreeInfo;
    RenewBranchInfo;
  end;
end;

procedure TTreeViewForm.ActionFlipExecute(Sender: TObject);
begin
  if Tree.BranchFocused then { need to focus on a node, not a branch}
    Tree.FocusOnNode(Tree.FocusedIndex);
  if Tree.NodeFocused then
  begin
    Tree.FlipAllCluster;
    ActionBalancedTree.Checked := False;
    ActionInputOrderTree.Checked := False;
    DataSaved := False;
    if CurTool = ActionPoint then
      ActionPointExecute(Sender);
    if FJustRootingATree and (not IsGeneDupsMode) and (Tree.IsFocusedOnRoot) then
    begin
      SetOutgroupCluster(clRed , Tree.NodeInfo.Des2Index);
      Tree.Refresh;
    end;
  end
  else if Tree.Cursor <> CURSOR_FLIPALL then
  begin
    CurTool.Checked := false;
    CurTool := ActionFlip;
    CurTool.Checked := true;
    Tree.Cursor := CURSOR_FLIPALL;
  end
  else
    ActionPointExecute(nil);
end;

procedure TTreeViewForm.ActionFormattingToolsExecute(Sender: TObject);
var
  clientCoords: TPoint;
begin
  FormatScaleItem.Enabled := Tree.IsScale;
  FormatCuttoffItem.Enabled := (ActionCondensed.Enabled or ActionConsensus.Enabled);
  InitTreeOptionsPopupMenu;
  if Sender = FormattingToolsBtn then
  begin
    clientCoords.x := FormattingToolsBtn.Left;
    clientCoords.y := FormattingToolsBtn.Top + FormattingToolsBtn.Height;
  end
  else
  begin
    clientCoords.x := FSubtreeFrame.FormatSubtreeBtn.Left;
    clientCoords.y := FSubtreeFrame.FormatSubtreeBtn.Top + FSubtreeFrame.FormatSubtreeBtn.Height*2 + FSubtreeFrame.Top + 10;
  end;
  clientCoords := ClientToScreen(clientCoords);
  TreeOptionsPopup.PopUp(clientCoords.X, clientCoords.Y);
end;

procedure TTreeViewForm.ActionHideOverlappingDivTimesExecute(Sender: TObject);
begin
  if FCheckStateChanging then
    Exit;
  try
    FCheckStateChanging := True;
    ActionHideOverlappingDivTimes.Checked := not ActionHideOverlappingDivTimes.Checked;
    ReltimeTree.HideOverlappingDivTimes := ActionHideOverlappingDivTimes.Checked;
    ReltimeTree.Invalidate;
    FDivTimesFrame.HideOverlappingDivTimesCheckBx.Checked := ActionHideOverlappingDivTimes.Checked;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.ActionHideOverlappingTaxaExecute(Sender: TObject);
begin
  ActionHideOverlappingTaxa.Checked := not ActionHideOverlappingTaxa.Checked;
  FTaxaNamesFrame.ShowTaxaNamesCheckBx.Checked := ActionHideOverlappingTaxa.Checked;
  Tree.HideOverlappingTaxa := ActionHideOverlappingTaxa.Checked;

  if OriTree <> nil then
    OriTree.HideOverlappingTaxa := ActionHideOverlappingTaxa.Checked;

  if BootTree <> nil then
    BootTree.HideOverlappingTaxa := ActionHideOverlappingTaxa.Checked;

  if ReltimeTree <> nil then
    ReltimeTree.HideOverlappingTaxa := ActionHideOverlappingTaxa.Checked;

  Tree.Invalidate;
end;

procedure TTreeViewForm.ActionImportGpNamesExecute(Sender: TObject);
var
  sl: TStringList;
  i: integer;
  aName, aValue: String;
begin
  sl := nil;
  try
    FileOpenDialog.DefaultExt := 'TXT';
    FileOpenDialog.Filter := 'Text File (*.txt)|*.txt';
    FileOpenDialog.FileName := '';
    FileOpenDialog.Title := 'Select a Group Names File';
    FileOpenDialog.InitialDir := ChangeInitialDirectorySaveDialogForMac(FileOpenDialog.InitialDir);
    if FileOpenDialog.Execute then
    begin
      sl := TStringList.Create;
      sl.LoadFromFile(FileOpenDialog.FileName)
    end
    else
      Exit;
    if sl.Count = 0 then
      Exit;
    for i := 0 to sl.Count-1 do
    begin
      aName := sl.Names[i];
      aValue := sl.ValueFromIndex[i];
      TrimTaxaName(aValue);
      TrimTaxaName(aName);
      sl[i] := aName + '=' + aValue;
    end;

    Tree.SetGroupInfo(sl);
    Tree.Refresh;
    GroupFileName := FileOpenDialog.FileName;
  finally
    if sl <> nil then sl.Free;
  end;
end;

procedure TTreeViewForm.ActionImportNamesExecute(Sender: TObject);
var
  aList: TStringList = nil;
  numReplaced: Integer = 0;
begin
  FileOpenDialog.InitialDir := GetCurrentDirUTF8;
  FileOpenDialog.Filter := 'Text File(*.txt)|*.txt|All Files|*.*';
  if FileOpenDialog.Execute then
  begin
    try
      aList := TStringList.Create;
      aList.LoadFromFile(FileOpenDialog.FileName);
      numReplaced := Tree.ReplaceTaxaNames(aList);
      Tree.Invalidate;
      if numReplaced > 0 then
        ShowMessage(Format('%d taxa name(s) were replaced', [numReplaced]))
      else
        ShowMessage('No names were replaced. Please make sure that the name translations file has key=value pairs in the format:' + LineEnding + #9 + 'old_name=new_name');
    finally
      if Assigned(aList) then
        aList.Free;
    end;
  end;
end;

procedure TTreeViewForm.ActionInfoExecute(Sender: TObject);
begin
  ActionInfo.Checked := not ActionInfo.Checked;

  with FTreeInfoFrame do
    if ActionInfo.Checked then
    begin
      InitializeMethodInfo;
      RenewTreeInfo;

      if isAncState then
      begin
        RenewAncState;
        //DisplayAncStateTypes(AncStateInfoType, MaxAncStateIndex);
        AncRateTab.TabVisible := ((Assigned(MLAnalyzer)) and (MLAnalyzer.Model.NoOfRates > 1));
      end
      else
      begin
        AncStateTab.TabVisible := false;
        AncRateTab.TabVisible := false;
      end;

      if (Tree.BranchFocused or Tree.NodeFocused) then
      begin
          RenewBranchInfo;
          if Tree.ShowCharState then
            PageControl.Pages[3].Show
          else
            PageControl.Pages[2].Show;
      end
      else
        PageControl.Pages[1].Show;
      InfoDisplay.Show;
    end
    else
      InfoDisplay.Hide;
end;

procedure TTreeViewForm.ActionInputOrderTreeExecute(Sender: TObject);
begin
  if ActionInputOrderTree.Checked then
  begin
    ActionInputOrderTree.Checked := False;
    SortClusterOrderItem.Checked := False;
  end
  else
  begin
    ActionInputOrderTree.Checked := True;
    ActionBalancedTree.Checked   := False;
    SortClusterOrderItem.Checked := True;
    SortClusterShapeItem.Checked := False;
    Tree.SortClusterInOrder;
    Tree.Refresh;
    DataSaved := False;
  end;
  try
    FCheckStateChanging := True;
    FTaxaNamesFrame.BalancedShapeRadioBtn.Checked := ActionBalancedTree.Checked;
    FTaxaNamesFrame.InputOrderRadioBtn.Checked := ActionInputOrderTree.Checked;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.ActionLaunchGeneDupsSearchExecute(Sender: TObject);
var
  OtuNames: TStringList;
  SpeciesNames: TStringList;
  AInfo: TAnalysisInfo;
begin
  OtuNames := nil;
  SpeciesNames := nil;
  if FJustRootingATree then
  begin
    try
      try
        Tree.GetSimpleNodeData(GeneDupTree); // gets a copy of the gene tree nodes
        Tree.AssignTreeList(GeneDupTreeList);
        OtuNames := Tree.GetOTUNamesList;
        SpeciesNames := Tree.GetSpeciesNamesList;
        GeneDupTreeList.OTUNameList.Assign(OtuNames);
        GeneDupTreeList.AssignSpeciesNames(SpeciesNames);
        FGeneDupWizardForm := nil; // otherwise it will get freed when we close
        ModalResult := mrOk;
      except
        on E:Exception do
        begin
          ShowMessage('Oh no! An error has occurred: ' + E.Message);
          ModalResult := mrAbort;
        end;
      end;
    finally
      GeneDupTree := nil; // owned by GeneDuplicationWizard
      GeneDupTreeList := nil; // owned by GeneDuplicationWizard
      if Assigned(OtuNames) then
        OtuNames.Free;
      if Assigned(SpeciesNames) then
        SpeciesNames.Free;
    end;
  end
  else
  begin
    try
      try
        FJustRootingATree := True;
        FGeneDupWizardForm := TGDWizardForm.Create(MegaForm);
        if not MAI.AllOtusHaveSpName then
          FGeneDupWizardForm.ProcessStep := gdpsMapSpecies
        else
          FGeneDupWizardForm.ProcessStep := gdpsLoadSpeciesTree;
        AInfo := TAnalysisInfo.Create;
        AInfo.Assign(MAI);
        AInfo.MyOriTreeList := TTreeList.Create;
        Tree.AssignTreeList(AInfo.MyOriTreeList);
        FGeneDupWizardForm.InitFromTreeExplorer(AInfo, Self);
        FGeneDupWizardForm.CancelledCallback := GeneDupsCancelled;
        FGeneDupWizardForm.Show;
      except
        on E:Exception do
        begin
          ShowMessage('Oh no! An error has occured: ' + E.Message);
        end;
      end;
    finally
      if Assigned(OtuNames) then
        OtuNames.Free;
      if Assigned(SpeciesNames) then
        SpeciesNames.Free;
    end;
  end;
end;

procedure TTreeViewForm.ActionLinearizedExecute(Sender: TObject);
var
  ATreeList: TTreeList;
  ATreeData: TTreeData;
  i: Integer;
  NumTaxa: Integer;
begin
  if IsReltimeBLensAnalysis(MAI.MyUsrOperation) then
    NumTaxa := MAI.MyOtuNames.Count
  else
    NumTaxa := MAI.MyNoOfSeqs;
  FCalibrationDlg.Hide;
  RemoveWindowFromTray(FCalibrationDlg);

  ATreeData := TTreeData.Create(NumTaxa, True, False, False);
  OriTree.GetTreeData(ATreeData);
  Tree := ReltimeTree;
  if Assigned(FCalibrations) and (FCalibrations.Count > 0) then
    AnchoredReltimeLinearizeFunc(ATreeData)
  else
    RelTimeLinearizeFunc(ATreeData);
  ATreeList := TTreeList.Create;
  for i := 0 to NumTaxa - 1 do
    ATreeList.OTUName[i] := MAI.MyOtuNames[i];
  ATreeList.Add(ATreeData);
  ReltimeTree.SetTreeList(ATreeList, True);
  if Assigned(FOtuInfos) then
    ReltimeTree.SetOtuInfo(FOtuInfos);
  ReltimeTree.TimeFactor := RelTimeAnalyzer.TimeFactor;
  ReltimeTree.LatestTime := RelTimeAnalyzer.LatestTime;
  ReltimeTree.LinearizeFunc := nil;
  ReltimeTree.ShowCalibratedNodeMarker := (FCalibrations.CalibrationsType <> ctLeafNode);
  if TreeTabControl.Tabs.IndexOf(ORI_TREE_TAB) < 0 then
    TreeTabControl.Tabs.Add(ORI_TREE_TAB);
  if TreeTabControl.Tabs.IndexOf(TIME_TREE_TAB) < 0 then
    TreeTabControl.Tabs.Add(TIME_TREE_TAB);
  FDivTimesFrame.Enabled := True;
  TreeTabControl.Enabled := true;
  Tree.Enabled := True;
  if Length(Tree.CalibratedNodes) = 0 then
    Tree.TimeFactor := 1.0;
  ReltimeTree.IsRooted := True;
  OriTree.SessionIsReltime := True;
  if ActionRootOnMidpoint.Checked then
    Tree.MakeRootOnMidpoint;
  if ActionBalancedTree.Checked then
    Tree.SortClusterForShape
  else if (ActionInputOrderTree.Checked and (Tree <> ReltimeTree)) then
    Tree.SortClusterInOrder;
  if (MAI.MyUsrOperation = dtdoRelTimeML) or (MAI.MyUsrOperation = dtdoRtdtML) then
    Tree.Value2[Tree.TreeIndex] := MLAnalyzer.MLTree.LogLikelihood
  else if IsReltimeNonMLAnalysis(MAI.MyUsrOperation) then
    Tree.Value2[Tree.TreeIndex] := Tree.SBL[Tree.TreeIndex];
  ResetStatusBar;
  if ActionInfo.Checked then
  begin
    RenewTreeInfo;
    RenewBranchInfo;
  end;
  if Length(FRelTimeTreeStates) > 0 then
    SaveRelTimeTreeViewState(Tree.TreeIndex - 1);
end;

procedure TTreeViewForm.ActionLoadImagesExecute(Sender: TObject);
var
  directory, filename: string;
  a : TNodeAttrib;
  i: integer;
  flag: boolean;
begin
  if SelectDirectory('','',directory) then
  begin
     a := TNodeAttrib.Create;
     for i := 1 to Tree.NoOfNodes do
       if Tree.OTUName[i] <> '' then
       begin
         flag := false;
         filename := Tree.OTUName[i];
         if FileExists(directory+'\'+filename+'.bmp') then
           flag := true
         else if Pos(' ', filename) > 0 then
         begin
           repeat
             filename[Pos(' ', filename)] := '_';
           until Pos(' ', filename) = 0;
           if FileExists(directory+'\'+filename+'.bmp') then
             flag := true;
         end;
         if flag then
         begin
           if not Tree.GetSubtreeAttrib(a, i) then
             a.ShowBracket := false;
           a.LoadBitmapFromFile(directory+'\'+filename+'.bmp');
           if a.IsImage then
             Tree.SetSubtreeAttrib(a, i);
           DataSaved := false;
           continue;
         end;
         if FileExists(directory+'\'+filename+'.jpg') then
           flag := true
         else if Pos(' ', filename) > 0 then
         begin
           repeat
             filename[Pos(' ', filename)] := '_';
           until Pos(' ', filename) = 0;
           if FileExists(directory+'\'+filename+'.jpg') then
             flag := true;
         end;
         if flag then
         begin
           Tree.GetSubtreeAttrib(a, i);
           a.LoadJPEGFromFile(directory+'\'+filename+'.jpg');
           Tree.SetSubtreeAttrib(a, i);
           DataSaved := false;
         end;
       end;
     a.Free;
     Tree.Refresh;
  end;
end;

procedure TTreeViewForm.ActionMarkerDisplayExecute(Sender: TObject);
begin
  OriTree.ShowOTUMarker := not OriTree.ShowOTUMarker;
  UpdateMarkerDisplay;
end;

procedure TTreeViewForm.ActionMolecularClockExecute(Sender: TObject);
var
  CalibrateClockDlg: TCalibrateClockDlg = nil;
  clockRate: Double;
  calibration: ArrayOfInteger;
begin
  Assert(Tree = ReltimeTree, 'molecular clock tree action should only be used for ReltimeTree');
  ActionMolecularClock.Checked := (not ActionMolecularClock.Checked);

  if not ActionMolecularClock.Checked then
  begin
    FRelTimeAnalyzer.StrictClockRate := 1;
    if CompareValue(abs(FReltimeAnalyzer.TimeFactor), 0.0, FP_CUTOFF) <> 0 then
      ReltimeTree.TimeFactor := FRelTimeAnalyzer.TimeFactor
    else
      ReltimeTree.TimeFactor := 1;
    ReltimeTree.ClearCalibratedNodeMarkers;
    if FCalibrationDlg.NumCalibrations > 0 then
    begin
      ReltimeTree.CalibratedNodes := FCalibrationDlg.GetCalibrations.CalibrationIndicesForTE;
      ReltimeTree.ShowCalibratedNodeMarker := True;
      ReltimeTree.TimeUnit := 'Divergence Times';
    end
    else
      ReltimeTree.TimeUnit := 'Relative Times';
    Exclude(FTimeTreeTypes, tttStrictClocksLocal);
    ResetStatusBar;
    if ActionInfo.Checked then
    begin
      RenewTreeInfo;
      RenewBranchInfo;
    end;
    ReltimeTree.Invalidate;
    Exit;
  end;

  //if Length(Tree.CalibratedNodes) > 0 then
  //begin
  //  response := MessageDlg('Delete calibrations?', 'Calibrating the tree using a strict clock will result in the deletion of previously used calibrations. Do you want to continue?', mtConfirmation, mbYesNo, 0);
  //  if response = mrNo then
  //    Exit;
  //end;
  try
    CalibrateClockDlg := TCalibrateClockDlg.Create(Self);
    if Trim(ReltimeTree.TimeUnit) <> EmptyStr then
      CalibrateClockDlg.TimeUnitEdit.Text := ReltimeTree.TimeUnit;
    if Tree.NodeFocused then
    begin
      CalibrateClockDlg.CalibrateClockMode := ccmDivTime;
      CalibrateClockDlg.FocusedNode := Tree.FocusedIndex;
    end
    else
    begin
      CalibrateClockDlg.CalibrateClockMode := ccmEvoRate;
      CalibrateClockDlg.FocusedNode := -1;
    end;

    if CalibrateClockDlg.ShowModal = mrOk then
    begin
      if Length(ReltimeTree.CalibratedNodes) > 0 then
        ReltimeTree.ClearCalibratedNodeMarkers;
      Include(FTimeTreeTypes, tttStrictClocksLocal);
      case CalibrateClockDlg.CalibrateClockMode of
        ccmEvoRate:
          begin
            if abs(FReltimeAnalyzer.TimeFactor) < FP_CUTOFF then
              clockRate := CalibrateClockDlg.EvoRate
            else
              clockRate := CalibrateClockDlg.EvoRate*FReltimeAnalyzer.TimeFactor;
            FigureGenerator.AssignData('clockRate', FormatDoubleSafe(clockRate, 3, 6));
            FReltimeAnalyzer.StrictClockRate := clockRate;
            ReltimeTree.TimeFactor := 1/clockRate;
            ReltimeTree.ShowCalibratedNodeMarker := False;
            ReltimeTree.TimeUnit := '';
            ReltimeTree.Invalidate;
          end;
        ccmDivTime:
          begin
            if abs(FReltimeAnalyzer.TimeFactor) < FP_CUTOFF then
              FRelTimeAnalyzer.StrictClockRate := 1/(CalibrateClockDlg.DivTime/Tree.NodeInfo.height)
            else
              FReltimeAnalyzer.StrictClockRate := (1/(CalibrateClockDlg.DivTime/Tree.NodeInfo.height))*FReltimeAnalyzer.TimeFactor;
            ReltimeTree.TimeUnit := CalibrateClockDlg.TimeUnitEdit.Text;
            FigureGenerator.AssignData('calibratedNode', IntToStr(ReltimeTree.FocusedIndex));
            FigureGenerator.AssignData('divTime', Trim(FormatDoubleSafe(CalibrateClockDlg.DivTime, 2, 6)));
            SetLength(calibration, 1);
            calibration[0] := ReltimeTree.FocusedIndex;
            ReltimeTree.TimeFactor := CalibrateClockDlg.DivTime/Tree.NodeInfo.height;
            ReltimeTree.CalibratedNodes := calibration;
            ReltimeTree.ShowCalibratedNodeMarker := True;
            ReltimeTree.Invalidate;
          end;
      end;
      ReltimeTree.LatestTime := 0.0;
      ReltimeTree.LinearizeFunc := nil;
      ReltimeTree.ShowHeightErrBar := False;
      Tree.Value2[Tree.TreeIndex] := Tree.SBL[Tree.TreeIndex];
      ResetStatusBar;
      if ActionInfo.Checked then
      begin
        RenewTreeInfo;
        RenewBranchInfo;
      end;
    end;
  finally
    if Assigned(CalibrateClockDlg) then
      CalibrateClockDlg.Free;
  end;
end;

procedure TTreeViewForm.ActionPrinterSetupExecute(Sender: TObject);
begin
   try
     PrinterSetupDialog.Execute
   except
     on E:Exception do
       ShowMessage('Oh no! An error occurred during printer setup: ' + E.Message);
   end;
end;

procedure TTreeViewForm.ActionPrintSmallExecute(Sender: TObject);
begin
  try
    if PrintDialog.Execute then
     Tree.PrintSmall;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when printing the tree image: ' + E.Message);
  end;
end;

procedure TTreeViewForm.ActionQuitExecute(Sender: TObject);
begin
  if (fsModal in FormState) then
    ModalResult := mrCancel;
  Close;
end;

procedure TTreeViewForm.ActionSaveImageToBmpExecute(Sender: TObject);
begin
  DoImageExport(iffBitmap);
end;

procedure TTreeViewForm.ActionSaveImageToEmfExecute(Sender: TObject);
begin
  DoImageExport(iffEmf);
end;

procedure TTreeViewForm.ActionSaveImageToPdfExecute(Sender: TObject);
begin
  DoImageExport(iffPdf);
end;

procedure TTreeViewForm.ActionSaveImageToPngExecute(Sender: TObject);
begin
  DoImageExport(iffPng);
end;

procedure TTreeViewForm.ActionSaveImageToSvgExecute(Sender: TObject);
begin
  DoImageExport(iffSvg);
end;

procedure TTreeViewForm.ActionSaveImageToTIFFExecute(Sender: TObject);
begin
  DoImageExport(iffTiff);
end;

procedure TTreeViewForm.ActionScaleBarDisplayExecute(Sender: TObject);
begin
  ActionScaleBarDisplay.Checked := not ActionScaleBarDisplay.Checked;
  Tree.ShowScale := ActionScaleBarDisplay.Checked;
  Tree.Invalidate;
end;

procedure TTreeViewForm.ActionSearchGeneDupsExecute(Sender: TObject);
begin
  if not Assigned(FJumpToGeneDupDlg) then
    FJumpToGeneDupDlg := TJumpToGeneDupForm.Create(Self);
  FJumpToGeneDupDlg.Tree := Tree;
  FJumpToGeneDupDlg.JumpCallback := ScrollToShowNode;
  FJumpToGeneDupDlg.Show;
end;

procedure TTreeViewForm.ActionSortByContinentExecute(Sender: TObject);
begin
  Tree.SortTreeByContinent;
  Tree.Refresh;
  DataSaved := False;
end;

procedure TTreeViewForm.ActionSortTreeByYearExecute(Sender: TObject);
begin
  if ActionSortTreeByYear.Checked then
  begin
    ActionSortTreeByYear.Checked := False;
  end
  else
  begin
    ActionSortTreeByYear.Checked := True;
    ActionInputOrderTree.Checked := False;
    ActionBalancedTree.Checked   := False;
    SortClusterOrderItem.Checked := False;
    SortClusterShapeItem.Checked := False;
    Tree.SortTreeByYear;
    Tree.Refresh;
    DataSaved := False;
  end;
  try
    FCheckStateChanging := True;
    FTaxaNamesFrame.BalancedShapeRadioBtn.Checked := ActionBalancedTree.Checked;
    FTaxaNamesFrame.InputOrderRadioBtn.Checked := ActionInputOrderTree.Checked;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.CaptionCopyToClipboardActionExecute(Sender: TObject);
begin
  CaptionViewer.Browser.MainFrame.SelectAll;
  CaptionViewer.Browser.MainFrame.Copy;
  if Trim(FCaptionHtml) <> EmptyStr then
    CaptionViewer.Browser.MainFrame.LoadString(FCaptionHtml, 'http://localhost/');
end;

procedure TTreeViewForm.CaptionDockInTEActionExecute(Sender: TObject);
begin
  if Assigned(FCapViewer) and FCapViewer.Visible then
    FCapViewer.Visible := False;
  Tree.Align := alClient;
  CaptionSplitter.Visible := True;
  CaptionPanel.Visible := True;
  CaptionViewer.Visible := True;
  CaptionShowInNewWindowAction.Checked := False;
  CaptionDockInTEAction.Checked := False;
  CaptionShowInNewWindowAction.Checked := False;
  RefreshCaption;
  {$IFDEF DARWIN}
  BottomPanel.Height := 24;
  BottomPanel.BringToFront;
  {$ENDIF}
  try
    FCheckStateChanging := True;
    FCaptionFrame.DisplayCaptionCheckBox.Checked := True;
  finally
    FCheckStateChanging := False;
  end;
  Tree.Invalidate;
end;

procedure TTreeViewForm.CaptionPrintActionExecute(Sender: TObject);
begin
  try
      CaptionViewer.Browser.Host.Print;
    except
      on E:Exception do
        ShowMessage('Oh no! An error occurred when printing the caption: ' + E.Message);
    end;
end;

procedure TTreeViewForm.CaptionSaveToTextActionExecute(Sender: TObject);
var
  str: String;
  aList: TStringList = nil;
  aSaveDlg: TSaveDialog = nil;
begin
  try
    try
      CaptionViewer.Browser.MainFrame.SelectAll;
      CaptionViewer.Browser.MainFrame.Copy;
      if Trim(FCaptionHtml) <> EmptyStr then
        CaptionViewer.Browser.MainFrame.LoadString(FCaptionHtml, 'http://localhost/');

      aSaveDlg := TSaveDialog.Create(nil);
      aSaveDlg.Options := [ofOverwritePrompt, ofViewDetail, ofEnableSizing];
      aSaveDlg.Filter := 'Text Files|*.txt|All Files|*.*';
      aSaveDlg.DefaultExt := 'txt';
      aSaveDlg.InitialDir := GetCurrentDir;
      aSaveDlg.Filename := 'Caption.txt';
      if aSaveDlg.Execute then
      begin
        aList := TStringList.Create;
        str := Clipboard.AsText;
        aList.Text := str;
        aList.SaveToFile(aSaveDlg.Filename);
        OpenFileAndFocus(aSaveDlg.Filename, 0, 0);
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
    if Assigned(aSaveDlg) then
      aSaveDlg.Free;
  end;
end;

procedure TTreeViewForm.CaptionShowInNewWindowActionExecute(Sender: TObject);
begin
  if CaptionPanel.Visible then
    FCaptionPanelHeight := CaptionPanel.Height;
  GenerateCapViewerForCurrentTree;
  FCapViewer.Show;
  CaptionViewer.Visible := False;
  CaptionPanel.Visible := False;
  CaptionSplitter.Visible := False;
  CaptionDockInTEAction.Checked := False;
  if not (Tree.Align = alClient) then
    Tree.Align := alClient;
  RefreshCaption;
  Tree.Invalidate;
  try
    FCheckStateChanging := True;
    FCaptionFrame.DisplayCaptionCheckBox.Checked := False;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.CaptionSplitterMoved(Sender: TObject);
begin
  //FCaptionPanelHeight := CaptionPanel.Height;
  //{$IFDEF DARWIN}
  //BottomPanel.Height := 24;
  //BottomPanel.BringToFront;
  //CaptionToolbar.Invalidate;
  //{$ENDIF}
  //Tree.Invalidate;
end;

procedure TTreeViewForm.CaptionZoomInActionExecute(Sender: TObject);
begin
  FigureGenerator.StyleSheet.IncrementFontSizes;
  RefreshCaption;
end;

procedure TTreeViewForm.CaptionZoomOutActionExecute(Sender: TObject);
begin
  FigureGenerator.StyleSheet.DecrementFontSize;
  RefreshCaption;
end;

procedure TTreeViewForm.ClearAllSubtreeOptionSubmenuItemClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to clear all subtree drawing options?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
  begin
    Tree.ClearAllSubtreeAttrib;
    Tree.Refresh;
    Options1.Caption := 'Selected Subtree';
  end;
end;

procedure TTreeViewForm.ClearSubtreeOptionSubmenuItemClick(Sender: TObject);
begin
  if not (Tree.NodeFocused or Tree.BranchFocused) then exit;
  Tree.ClearSubtreeAttrib(Tree.FocusedIndex, False);
  Tree.Refresh;
end;

procedure TTreeViewForm.CollapsePanelMouseEnter(Sender: TObject);
begin
  CollapsePanel.Color := TE_FRAME_HEADER_HOVER_COLOR;
end;

procedure TTreeViewForm.CollapsePanelMouseLeave(Sender: TObject);
begin
  CollapsePanel.Color := clDefault;
end;

procedure TTreeViewForm.CorrtestActionExecute(Sender: TObject);
var
  test: TCorrelationTest = nil;
  setup: TTreeDisplaySetup = nil;
begin
  try
    try
      test := FRelTimeAnalyzer.AutomaticCorrelationTest(MAI.MaxRateRatio);
      setup := TTreeDisplaySetup.Create;
      setup.ShowCorrTestTreeExplorerResult(MAI, test);
    except
      on E:Exception do
        ShowMessage('Application error when running CorrTest: ' + E.Message);
    end;
  finally
    if Assigned(setup) then
      setup.Free;
    if Assigned(test) then
      test.Free;
  end;
end;

procedure TTreeViewForm.DebugActionExecute(Sender: TObject);
var
  attribList: TStringList = nil;
  treeInfoList: TStringList = nil;
  aList: TStringList = nil;
begin
  {$IFDEF DEBUG}
  try
     //attribList := Tree.CompressedNodeAttributes.DebugStrings;
     //OpenStringList(attribList, 'Node Attributes');
     //treeInfoList := Tree.DebugStrings;
     //OpenStringList(treeInfoList, 'Tree Info');
    aList := Tree.GetCurrentTree.StateToStringList('debug');
    OpenStringList(aList, 'debug');
  finally
    //if Assigned(attribList) then
    //  attribList.Free;
    //if Assigned(treeInfoList) then
    //  treeInfoList.Free;
    if Assigned(aList) then
      aList.Free;
  end;
  {$ENDIF}
end;

procedure TTreeViewForm.DrawOptionsItemClick(Sender: TObject);
var
  i: integer;
  newitem: TMenuItem = nil;
  a: TNodeAttrib = nil;
begin
  try
    a := TNodeAttrib.Create;
    if DrawOptionsItem.Count > 1 then
      for i := DrawOptionsItem.Count-1 downto 1 do
         DrawOptionsItem.Delete(i);

    if Tree.GroupAttrib.Count > 0 then
    begin
      newitem := TMenuItem.Create(MainMenu);
      newitem.Caption := '-';
      DrawOptionsItem.Add(newitem);
      for i := 0 to Tree.GroupAttrib.Count-1 do
      begin
        newitem := TMenuItem.Create(MainMenu);
        newitem.Hint        := Tree.GroupAttrib[i].Name;
        newitem.Caption     := Tree.GroupAttrib[i].Name;
        newitem.ImageIndex  := 36;
        newitem.OnClick     := GroupOptionSubmenuItemClick;
        DrawOptionsItem.Add(newitem);
      end;
    end;
    if Tree.AttribList.Count > 1 then
    begin
      newitem := TMenuItem.Create(Self);
      newitem.Caption := '-';
      DrawOptionsItem.Add(newitem);
      for i := 1 to Tree.AttribList.Count-1 do
      begin
        newitem := TMenuItem.Create(MainMenu);
        newitem.Hint := IntToStr(Tree.AttribList[i].NodeIndex);
        if Tree.AttribList[i].Caption = '' then
          newitem.Caption := '(Node '+newitem.Hint+')'
        else
          newitem.Caption := Tree.AttribList[i].Caption;
        newitem.ImageIndex := 175;
        newitem.OnClick := SubtreeOptionSubmenuItemClick;
        DrawOptionsItem.Add(newitem);
      end;
        newitem := TMenuItem.Create(MainMenu);
        newitem.Caption := '-';
        DrawOptionsItem.Add(newitem);

        newitem := TMenuItem.Create(MainMenu);
        newitem.Enabled := ActionSubtreeOption.Enabled and Tree.GetSubtreeAttrib(a, Tree.FocusedIndex);
        if newitem.Enabled then
          if a.Caption = '' then
          begin
            newitem.Caption  := 'Clear "(Node '+IntToStr(a.nodeindex)+')"';
            Options1.Caption := '(Node '+IntToStr(a.nodeindex)+')';
          end
          else
          begin
            newitem.Caption := 'Clear "'+a.Caption+'"';
            Options1.Caption := a.Caption;
          end
        else
        begin
          newitem.Caption := 'Clear Selected Subtree Drawing Options';
          Options1.Caption := 'Selected Subtree';
        end;
        newitem.ImageIndex := 24;
        newitem.OnClick := ClearSubtreeOptionSubmenuItemClick;
        DrawOptionsItem.Add(newitem);

        newitem := TMenuItem.Create(MainMenu);
        newitem.Caption := 'Clear All Subtree Drawing Options';
        newitem.ImageIndex := -1;
        newitem.OnClick := ClearAllSubtreeOptionSubmenuItemClick;
        DrawOptionsItem.Add(newitem);
    end;
  finally
    if Assigned(a) then
      a.Free;
  end;
end;

procedure TTreeViewForm.ExportPairwiseDistancesActionExecute(Sender: TObject);
var
  adapter: TSimpleTreeDataAdapter = nil;
  aData: TTreeData = nil;
  namesList: TStringList = nil;
  d: PDistanceMatrix = nil;
  DispDlg: TD_DisplayMatrix = nil;
  DispDlgVS: TVS_DisplayMatrix = nil;
  DispDlgV: TDisplayMatrixDlg = nil;
  i: Integer;
begin
  try
    try
      aData := TTreeData.Create(Tree.NoOfOTUs, True, False, False);
      namesList := TStringList.Create;
      if Tree = ReltimeTree then
        Tree.GetTreeDataNoOutgroup(aData, namesList, not Tree.IsSamplingTimes)
      else
      begin
        Tree.GetTreeData(aData, False);
        Tree.GetOTUNames(namesList);
      end;
      adapter := TSimpleTreeDataAdapter.Create;
      d := adapter.GetPairwiseDistances(aData, Tree.IsRooted, Tree = ReltimeTree);
      DispDlg := TD_DisplayMatrix.Create;
      DispDlgVS := TVS_DisplayMatrix.Create;
      DispDlgV := TDisplayMatrixDlg.Create(Self);
      DispDlgV.LinkDataClass(DispDlg);
      DispDlgV.LinkVisualStatusClass(DispDlgVS);
      DispDlg.LinkVisualStatusClass(DispDlgVS);
      DispDlg.DAcronym  := 'd';
      DispDlg.OnlyDistance := True;
      if Assigned(MAI) then
      begin
        DispDlg.FigureGenerator.LoadTemplateFromFile('Pairwise_Times_Matrix.htm');
        DispDlg.FigureGenerator.AssignData('Operation', GetEnumName(TypeInfo(TDistTreeDlgOption), Integer(MAI.MyUsrOperation)));
        if Tree = ReltimeTree then
          DispDlg.FigureGenerator.AssignData('IsLinearized', 'True')
        else
          DispDlg.FigureGenerator.AssignData('IsLinearized', 'False');
      end;
      DispDlg.Title := MegaForm.DataTitle;
      if IsReltimeTree then
      begin
        DispDlg.Fullname := 'Pairwise divergence times';
        DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Pairwise Divergence Times';
      end
      else
      begin
        DispDlg.Fullname := 'Patristic distances';
        DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Patristic Distances';
      end;
      DispDlg.ComputationType := gdPairwise;
      DispDlg.AllowNeg      := False;
      DispDlg.HasTestResult := False;
      DispDlg.NoOfTaxa := aData.NoOfOTUs;
      for i:=0 to namesList.Count - 1 do
        DispDlg.TaxonName[i] := namesList[i];
      if Assigned(MAI) and (MAI.MyNoOfGps > 0) then
      begin
        DispDlg.NoOfGps := MAI.MyNoOfGps;
        for i:=0 to MAI.MyNoOfSeqs-1 do
        begin
          if (MAI.MyUsedOtuInfos.Count <= i) or ((Tree = ReltimeTree) and TOtuInfo(MAI.MyUsedOtuInfos[i]).OutgroupMember) then
            continue;
          DispDlg.GpName[i] := TOtuInfo(MAI.MyUsedOtuInfos[i]).GpName;
        end;
      end;
      DispDlg.DistMat := d;
      d := nil;
      DispDlg.Initialize;
      if Assigned(MAI) then
      begin
        with DispDlg do
        begin
          FigureGenerator.BindData(MAI.MyDistPack, True);
          FigureGenerator.BindData(DispDlg, True);
          FigureGenerator.BindData(DispDlgV, True);
          FigureGenerator.BindData(MAI, True);
        end;
      end;
      DispDlgV.Show;
      if Assigned(MAI) then
        DispDlgV.Caption := DispDlgV.Caption +' (' + ExtractFilename(MAI.DataFileName) + ')'
      else
        DispDlgV.Caption := DispDlgV.Caption + ' (' + FNewickFileName + ')';
    except
      on E:Exception do
        ShowMessage('Application Error: ' + E.Message);
    end;
  finally
    if Assigned(adapter) then
      adapter.Free;
    if Assigned(aData) then
      aData.Free;
    if Assigned(namesList) then
      namesList.Free;
  end;
end;

procedure TTreeViewForm.FormActivate(Sender: TObject);
begin
  if Assigned(Tree) and Tree.Visible then
  begin
    if Assigned(Tree.EditBox) and Tree.EditBox.Visible then
      Tree.EditBox.Hide;
    Tree.Invalidate;
  end;
end;

procedure TTreeViewForm.FormatBranchesItemClick(Sender: TObject);
var
  js: String;
  min, max, step, val: Integer;
  //min2, max2: Double;
  temp: String;
begin
  js := EmptyStr;

  try
    ShowOptionsDialog;
    HtmlOptionsDialog.ProcessMessageReceivedProc := ProcessBranchOptionsMessage;
    HtmlOptionsDialog.DomProcedureName := VISITDOMPROC_TREE_OPTIONS_BRANCH;

    js := Format('$("#%s").val("%d").trigger("change");', [TREE_OPTIONS_BRANCH_LINES, Tree.BranchPen.Width]) + LineEnding;
    if Tree.isTimes then
    begin
      js := js + Format('$("#%s").attr("%s", "%s");', [TREE_OPTIONS_BRANCH_DISPLAY_DIVERGENCE, DMEO_IS_CHECKED, BoolToStr(Tree.ShowDivergenceTimes, True)]) + LineEnding;
      js := js + Format('$("#%s").prop("checked", %s);', [TREE_OPTIONS_BRANCH_DISPLAY_DIVERGENCE, BoolToStrLC(Tree.ShowDivergenceTimes, True)]) + LineEnding;
      js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_BRANCH_TIME_PLACEMENT, Integer(Tree.TimesPosition)]) + LineEnding;
      js := js + Format('$("#%s").val("%s");', [TREE_OPTIONS_BRANCH_DIVERGENCE_FONT, CssStringForFont(Tree.TimesFont)]) + LineEnding;
      js := js + JavaScriptFontPickerOptions(TREE_OPTIONS_BRANCH_DIVERGENCE_FONT) + LineEnding;

      js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_BRANCH_DIVERGENCE_PRECISION, Tree.DivTimeDecimals]) + LineEnding;
      js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_BRANCH_DISTANCE_HORIZONTAL, Tree.TimesMargin.X]) + LineEnding;
      js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_BRANCH_DISTANCE_VERTICAL, Tree.TimesMargin.Y]) + LineEnding;
    end
    else
    begin
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_DISPLAY_DIVERGENCE]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_TIME_PLACEMENT]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_DIVERGENCE_FONT]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_DIVERGENCE_PRECISION]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_DISTANCE_HORIZONTAL]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_DISTANCE_VERTICAL]) + LineEnding;
    end;

    if Tree.isStats then
    begin
      js := js + Format('$("#%s").attr("%s", "%s");', [TREE_OPTIONS_BRANCH_DISPLAY_STATS, DMEO_IS_CHECKED, BoolToStr(Tree.ShowStats, True)]) + LineEnding;
      js := js + Format('$("#%s").prop("checked", %s);', [TREE_OPTIONS_BRANCH_DISPLAY_STATS, BoolToStrLC(Tree.ShowStats, True)]) + LineEnding;
      js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_BRANCH_STAT_PLACEMENT, Integer(Tree.StatsPosition)]) + LineEnding;
      js := js + Format('$("#%s").val("%s");', [TREE_OPTIONS_BRANCH_STATISTICS_FONT, CssStringForFont(Tree.StatsFont)]) + LineEnding;
      js := js + JavaScriptFontPickerOptions(TREE_OPTIONS_BRANCH_STATISTICS_FONT) + LineEnding;

      min := -Tree.PixelsPerOTU;
      max := Tree.PixelsPerOTU;
      step := 1;
      val := Tree.StatsMargin.X;
      js := js + Format('$("#%s").spinner({min: %d, max: %d, step: %d});', [TREE_OPTIONS_BRANCH_HORIZONTAL, min, max, step]) + LineEnding;
      js := js + Format('$("#%s").spinner("value", %d);', [TREE_OPTIONS_BRANCH_HORIZONTAL, val]) + LineEnding;
      min := 1;
      max := Tree.PixelsPerOTU div 2;
      val := Tree.StatsMargin.Y;
      js := js + Format('$("#%s").spinner({min: %d, max: %d, step: %d});', [TREE_OPTIONS_BRANCH_VERTICAL, min, max, step, val]) + LineEnding;
      js := js + Format('$("#%s").spinner("value", %d);', [TREE_OPTIONS_BRANCH_VERTICAL, val]) + LineEnding;
      js := js + Format('$("#%s").attr("%s", "%s");', [TREE_OPTIONS_BRANCH_HIDE_LOWER, DMEO_IS_CHECKED, BoolToStr(Tree.StatsCutoff <> 0, True)]) + LineEnding;
      js := js + Format('$("#%s").prop("checked", %s);', [TREE_OPTIONS_BRANCH_HIDE_LOWER, BoolToStrLC(Tree.StatsCutoff <> 0, True)]) + LineEnding;
      js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_BRANCH_HIDE_VALUES, Tree.StatsCutoff]) + LineEnding;
    end
    else
    begin
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_DISPLAY_STATS]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_STAT_PLACEMENT]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_STATISTICS_FONT]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_HORIZONTAL]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_VERTICAL]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_HIDE_LOWER]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_HIDE_VALUES]) + LineEnding;
    end;

    if (Tree.isBranchLength and (Tree.TreeStyle = tsTraditional)) then
    begin
      js := js + Format('$("#%s").attr("%s", "%s");', [TREE_OPTIONS_BRANCH_DISPLAY_BRANCH, DMEO_IS_CHECKED, BoolToStr(Tree.ShowBLen, True)]) + LineEnding;
      js := js + Format('$("#%s").prop("checked", %s);', [TREE_OPTIONS_BRANCH_DISPLAY_BRANCH, BoolToStrLC(Tree.ShowBLen, True)]) + LineEnding;
      js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_BRANCH_BRANCH_PLACEMENT, Integer(Tree.BLenPosition)]) + LineEnding;
      js := js + Format('$("#%s").val("%s");', [TREE_OPTIONS_BRANCH_LENGTH_FONT, CssStringForFont(Tree.BlensFont)]) + LineEnding;
      js := js + JavaScriptFontPickerOptions(TREE_OPTIONS_BRANCH_LENGTH_FONT) + LineEnding;

      js := js + Format('$("#%s").val("%d");', [TREE_OPTIONS_BRANCH_PRECISION, Tree.BLenDecimals]) + LineEnding;
      js := js + Format('$("#%s").attr("%s", "%s");', [TREE_OPTIONS_BRANCH_HIDE_SHORTER, DMEO_IS_CHECKED, BoolToStr(Tree.BLenCutoff > Tree.MinBranchLength, True)]) + LineEnding;
      js := js + Format('$("#%s").prop("checked", %s);', [TREE_OPTIONS_BRANCH_HIDE_SHORTER, BoolToStrLC(Tree.BLenCutoff > Tree.MinBranchLength, True)]) + LineEnding;
        //if Tree.MinBranchLength < 0.0 then
        //  min2 := Tree.MinBranchLength
        //else
        //  min2 := 0.0;
        //max2 := Tree.MaxBranchLength;
      temp := Format('val("%%0.%df");', [Tree.BLenDecimals]);
      js := js + Format('$("#%s").' + temp, [TREE_OPTIONS_HIDE_SHORTER, Tree.BLenCutoff]) + LineEnding;
    end
    else
    begin
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_DISPLAY_BRANCH]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_BRANCH_PLACEMENT]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_LENGTH_FONT]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_PRECISION]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_BRANCH_HIDE_SHORTER]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_HIDE_SHORTER]) + LineEnding;
    end;

    HtmlOptionsDialog.LoadOptionsFile(wofTreeOptionsBranchFile, js, 'Branch Drawing Options', 395, 580, HC_Branch_tab_in_Format_dialog_box);
  except
   On E: Exception do
   begin
     ShowMessage('Oh no! An error has occurred: ' + E.Message);
     HideOptionsDialog;
   end;
  end;
end;

procedure TTreeViewForm.FormatCuttoffItemClick(Sender: TObject);
var
  js: String;
  isEnabled: Boolean;
begin
  js := EmptyStr;

  try
    ShowOptionsDialog;
    HtmlOptionsDialog.ProcessMessageReceivedProc := ProcessCutoffOptionsMessage;
    HtmlOptionsDialog.CancelCallback := CutoffDlgCancelledCallback;
    HtmlOptionsDialog.DomProcedureName := VISITDOMPROC_TREE_OPTIONS_CUTOFF;
    js := Format('$("#%s").spinner("value", %d);', [TREE_OPTIONS_CUTOFF_CONDENSED, Tree.CondenseValue]) + LineEnding;
    js := js + Format('$("#%s").spinner("value", %d);', [TREE_OPTIONS_CUTOFF_CONSENSUS, Tree.ConsensusValue]) + LineEnding;
    isEnabled := Tree.isStats and (Tree.TreeStyle <> tsRadiation) and (not ActionConsensus.Checked);
    if not isEnabled then
      js := js + Format('$("#%s").spinner("disable");', [TREE_OPTIONS_CUTOFF_CONDENSED]) + LineEnding;
    isEnabled := Tree.isConsensus and (Tree.TreeStyle <> tsRadiation) and ActionConsensus.Checked;
    if not isEnabled then
      js := js + Format('$("#%s").spinner("disable");', [TREE_OPTIONS_CUTOFF_CONSENSUS]);
    HtmlOptionsDialog.LoadOptionsFile(wofTreeOptionsCutoffFile, js, 'Tree Cutoff Options', 390, 180, HC_Subtree_Drawing_Options);
  except
   On E: Exception do
   begin
     ShowMessage('Oh no! An error has occurred: ' + E.Message);
     HideOptionsDialog;
   end;
  end;
end;

procedure TTreeViewForm.FormatLabelsItemClick(Sender: TObject);
var
  js: String;
  i: Integer;
  aName, aMarker, aColor, anchor: String;
begin
  js := EmptyStr;

  try
    ShowOptionsDialog;
    HtmlOptionsDialog.ProcessMessageReceivedProc := ProcessLabelsOptionsMessage;
    HtmlOptionsDialog.DomProcedureName := VISITDOMPROC_TREE_OPTIONS_LABELS;

    js := Format('$("#%s").attr("%s", "%s");', [TREE_OPTIONS_LABELS_DISPLAY_TAXON_NAMES, DMEO_IS_CHECKED, BoolToStr(Tree.ShowOTUName, True)]) + LineEnding;
    js := Format('$("#%s").prop("checked", %s);', [TREE_OPTIONS_LABELS_DISPLAY_TAXON_NAMES, BoolToStrLC(Tree.ShowOTUName, True)]) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [TREE_OPTIONS_BRANCH_LABELS_FONT, CssStringForFont(Tree.OTU_Font)]) + LineEnding;
    js := js + JavaScriptFontPickerOptions(TREE_OPTIONS_BRANCH_LABELS_FONT) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [TREE_OPTIONS_BRANCH_LABELS_COLOR, SColorToHtmlColor(Tree.OTU_Font.Color)]) + LineEnding;

    js := js + Format('$("#%s").attr("%s", "%s");', [TREE_OPTIONS_LABELS_DISPLAY_TAXON_MARKERS, DMEO_IS_CHECKED, BoolToStr(Tree.ShowOTUMarker, True)]) + LineEnding;
    js := js + Format('$("#%s").prop("checked", %s);', [TREE_OPTIONS_LABELS_DISPLAY_TAXON_MARKERS, BoolToStrLC(Tree.ShowOTUMarker, True)]) + LineEnding;

    if Tree.NoOfOTUs > 0 then
      for i := 1 to Tree.NoOfOTUs do
      begin
        aName := Tree.OTUName[i];
        aMarker := NodeMarkerShapeToHtmlString(Tree.Marker[i].Shape);
        aColor := SColorToHtmlColor(Tree.Marker[i].Color);
        anchor := MarkerToAnchorString(Tree.Marker[i].Shape, Tree.Marker[i].Color);
        js := js + Format('$("#%s").append(%s<tr marker_shape="%s" color="%s" value="%s"><td>%s</td><td>%s</td></tr>%s);', [TAXA_NAMES_LIST, #39, aMarker, aColor, aName, anchor, aName, #39]) +LineEnding;
      end;
    js := Trim(js);
    HtmlOptionsDialog.LoadOptionsFile(wofTreeOptionsLabelsFile, js, 'Labels Drawing Options', 390, 580, HC_Taxon_Name_tab_in_Format_dialog_box);
  except
   On E: Exception do
   begin
     ShowMessage('Oh no! An error has occurred: ' + E.Message);
     HideOptionsDialog;
   end;
  end;
end;


procedure TTreeViewForm.FormatScaleItemClick(Sender: TObject);
var
  js: String;
  aMin, aMax, aVal, aStep: Double;
begin
  js := EmptyStr;

  try
    ShowOptionsDialog;
    HtmlOptionsDialog.ProcessMessageReceivedProc := ProcessScaleOptionsMessage;
    HtmlOptionsDialog.DomProcedureName := VISITDOMPROC_TREE_OPTIONS_SCALE;

    if Tree.isScale then
    begin
      js := Format('$("#%s").attr("%s", "%s");', [TREE_OPTIONS_SCALE_DISTANCE_SCALE, DMEO_IS_CHECKED, BoolToStr(Tree.ShowScale, True)]) + LineEnding;
      js := js + Format('$("#%s").prop("checked", %s);', [TREE_OPTIONS_SCALE_DISTANCE_SCALE, BoolToStrLC(Tree.ShowScale, True)]) + LineEnding;
      js := js + Format('$("#%s").val("%s").trigger("change");', [TREE_OPTIONS_SCALE_LINES, IntToStr(Tree.ScalePen.Width)]) + LineEnding;
      js := js + Format('$("#%s").val("%s");', [TREE_OPTIONS_SCALE_NAME_CAPTION_DISTANCE_SCALE, Tree.ScaleUnit]) + LineEnding;
      js := js + Format('$("#%s").val("%s");', [TREE_OPTIONS_SCALE_FONT, CssStringForFont(Tree.ScaleFont)]) + LineEnding;
      js := js + JavaScriptFontPickerOptions(TREE_OPTIONS_SCALE_FONT) + LineEnding;
      if Tree.TreeStyle = tsCircle then
        aMax := Tree.LongestPath*1.1
      else
        aMax := Tree.LongestPath;
      aMin := power(10, floor(log10(aMax/100)));
      aVal := StrToFloat(Tree.ScaleText);
      if Pos(FormatSettings.DecimalSeparator, Tree.ScaleText) = 0 then
        aStep := 1
      else
        aStep := Max(0.01, Power(10, Pos(FormatSettings.DecimalSeparator, Tree.ScaleText)-Length(Tree.ScaleText)));
      js := js + Format('$("#%s").spinner({min: %.2f, max: %.2f, step: %.2f});', [TREE_OPTIONS_DISTANCE_SCALE_LENGTH, aMin, aMax, aStep]) + LineEnding;
      js := js + Format('$("#%s").spinner("value", %.2f);', [TREE_OPTIONS_DISTANCE_SCALE_LENGTH, aVal]) + LineEnding;

      if Tree.IsLinearized or ToggleTimeScaleAction.Enabled then
      begin
        js := js + Format('$("#%s").prop("disabled", %s);', [TREE_OPTIONS_DISTANCE_TICK_INTERVAL, 'false']) + LineEnding;
        js := js + Format('$("#%s").val("%s");', [TREE_OPTIONS_SCALE_NAME_CAPTION_TIME_SCALE, Tree.TimeUnit]) + LineEnding;

        aMax := aVal;
        aVal := Tree.ScaleTick;
        js := js + Format('$("#%s").spinner({min: %.2f, max: %.2f, step: %.2f});', [TREE_OPTIONS_DISTANCE_TICK_INTERVAL, aMin, aMax, aStep]) + LineEnding;
        js := js + Format('$("#%s").spinner("value", %.2f);', [TREE_OPTIONS_DISTANCE_TICK_INTERVAL, aVal]) + LineEnding;
        js := js + Format('$("#%s").prop("disabled", %s);', [TREE_OPTIONS_SCALE_NODE_HEIGHT_ERR, BoolToStrLC(not (ActionDisplayErrorBars.Enabled and ActionDisplayErrorBars.Visible), True)]) + LineEnding;
        js := js + Format('$("#%s").attr("%s", "%s");', [TREE_OPTIONS_SCALE_NODE_HEIGHT_ERR, DMEO_IS_CHECKED, BoolToStr(Tree.ShowHeightErrBar, True)]) + LineEnding;
        js := js + Format('$("#%s").prop("checked", %s);', [TREE_OPTIONS_SCALE_NODE_HEIGHT_ERR, BoolToStrLC(Tree.ShowHeightErrBar, True)]) + LineEnding;
      end
      else
      begin
        js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_DISTANCE_TICK_INTERVAL]) + LineEnding;
        js := js + Format('$("#%s").attr("%s", "%s");', [TREE_OPTIONS_SCALE_NODE_HEIGHT_ERR, DMEO_IS_CHECKED, BoolToStr(False, True)]) + LineEnding;
        js := js + Format('$("#%s").prop("checked", %s);', [TREE_OPTIONS_SCALE_NODE_HEIGHT_ERR, BoolToStrLC(Tree.ShowTimeScale, True)]) + LineEnding;
        js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_SCALE_NODE_HEIGHT_ERR]) + LineEnding;
      end;

      if Tree.IsTimeScale and (Tree.IsLinearized or ToggleTimeScaleAction.Enabled) then
      begin
        js := js + Format('$("#%s").prop("checked", %s)', [TREE_OPTIONS_SCALE_TIME_SCALE, BoolToStrLC(Tree.ShowTimeScale, True)]) + LineEnding;
        js := js + Format('$("#%s").attr("%s", "%s");', [TREE_OPTIONS_SCALE_TIME_SCALE, DMEO_IS_CHECKED, BoolToStr(Tree.ShowTimeScale, True)]) + LineEnding;
        js := js + Format('$("#%s").val("%s");', [TREE_OPTIONS_SCALE_NAME_CAPTION_TIME_SCALE, Tree.TimeUnit]) + LineEnding;

        if ReltimeTree.IsSamplingTimes then
        begin
          aMax := MaxDouble;
          aMin := 0;
          aStep := abs(ReltimeTree.TimeScale/10);
          if CompareValue(abs(ReltimeTree.TimeScale), 10, FP_CUTOFF) > 0 then
            aStep := Floor(aStep);
        end
        else
        begin
          aMax := 10*abs(StrToFloat(Tree.TimeText));
          aMin := power(10, floor(log10(aMax/100)));
        end;

        aVal := abs(ReltimeTree.TimeScale);
        js := js + Format('$("#%s").spinner({min: %.2f, max: %.2f, step: %.2f});', [TREE_OPTIONS_SCALE_MAJOR_TICK, aMin, aMax, aStep]) + LineEnding;
        js := js + Format('$("#%s").spinner("value", %.2f);', [TREE_OPTIONS_SCALE_MAJOR_TICK, aVal]) + LineEnding;
        aMax := aVal;
        aVal := Tree.TimeTick;
        js := js + Format('$("#%s").spinner({min: %.2f, max: %.2f, step: %.2f});', [TREE_OPTIONS_SCALE_MINOR_TICK, aMin, aMax, aStep]) + LineEnding;
        js := js + Format('$("#%s").spinner("value", %.2f);', [TREE_OPTIONS_SCALE_MINOR_TICK, aVal]) + LineEnding;
      end
      else
      begin
        js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_SCALE_TIME_SCALE]) + LineEnding;
        js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_SCALE_NAME_CAPTION_TIME_SCALE]) + LineEnding;
        js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_SCALE_MAJOR_TICK]) + LineEnding;
        js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_SCALE_MINOR_TICK]) + LineEnding;
        js := js + Format('$("#%s").attr("%s", "%s");', [TREE_OPTIONS_SCALE_TIME_SCALE, DMEO_IS_CHECKED, BoolToStr(Tree.ShowTimeScale, True)]) + LineEnding;
        js := js + Format('$("#%s").prop("checked", %s);', [TREE_OPTIONS_SCALE_TIME_SCALE, BoolToStrLC(Tree.ShowTimeScale, True)]) + LineEnding;
      end;
    end
    else
    begin
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_SCALE_DISTANCE_SCALE]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_SCALE_DISTANCE_SCALE]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_SCALE_NAME_CAPTION_DISTANCE_SCALE]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_DISTANCE_SCALE_LENGTH]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_DISTANCE_TICK_INTERVAL]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_SCALE_TIME_SCALE]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_SCALE_NAME_CAPTION_TIME_SCALE]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_SCALE_MAJOR_TICK]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", true);', [TREE_OPTIONS_SCALE_MINOR_TICK]) + LineEnding;
    end;
    HtmlOptionsDialog.LoadOptionsFile(wofTreeOptionsScaleFile, js, 'Scale Drawing Options', 390, 580, HC_Scale_Bar_tab_in_Format_dialog_box);
  except
   On E: Exception do
   begin
     ShowMessage('Oh no! An error has occurred: ' + E.Message);
     HideOptionsDialog;
   end;
  end;
end;

procedure TTreeViewForm.FormattingToolsBtnClick(Sender: TObject);
begin
  ActionFormattingToolsExecute(Sender);
end;

procedure TTreeViewForm.FormDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Source is TPanel then
  begin
    TPanel(Source).Left := x;
    TPanel(Source).Top := y;
  end;
end;

procedure TTreeViewForm.FormDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Source is TPanel then
  begin
    Accept := True;
  end;
end;

function TTreeViewForm.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
var
  helpTopic: String;
begin
  try
    Result := True;
    helpTopic := MapHelpContextToKeyword(HelpContext);
    CallHelp := False;
    if Trim(helpTopic) <> EmptyStr then
      ShowContextSensitiveHelp(helpTopic)
    else
      ShowContextSensitiveHelp(HelpKeyword);
  except
    on E:Exception do
      ShowMessage('Oh no! Failed to initialize the help browser: ' + E.Message);
  end;
end;

procedure TTreeViewForm.GoToNodeBranch1Click(Sender: TObject);
var
  NBNum: String = '';
  NB: Integer;
begin
  NB := -1;
  repeat
    InputQuery('Goto a Node or Branch', 'Node or Branch number [1-' + IntToStr(Tree.NoOfNodes) + ']', NBNum);
  until TryStrToInt(NBNum, NB) or (NBNum = EmptyStr);
  if NB <> -1 then
  begin
    if (NB <= 0) then
    begin
      ShowMessage('Node or branch must be a value >= 1');
      Exit;
    end;
    if (NB > Tree.NoOfNodes) then
    begin
      ShowMessage('Node or branch must be a value <= ' + IntToStr(Tree.NoOfNodes));
      Exit;
    end;
    Tree.FocusOnNode(NB);
    TreeJumpToTreeNode(NB, true);
  end;
end;

procedure TTreeViewForm.GroupOptionSubmenuItemClick(Sender: TObject);
var
  NodeAttrib: TNodeAttrib = nil;
begin
  try
    NodeAttrib := TNodeAttrib.Create;
    NodeAttrib.Name := TMenuItem(Sender).Hint;
    Tree.GetGroupAttrib(NodeAttrib, NodeAttrib.Name);
    SetupSubtreeOptionsDlg(NodeAttrib, ProcessGroupOptionsMessage, -1, True);
  finally
    if Assigned(NodeAttrib) then
      NodeAttrib.Free;
  end;
end;

procedure TTreeViewForm.HelpMenuClick(Sender: TObject);
begin
  ShowContextSensitiveHelp(HelpKeyword);
end;

procedure TTreeViewForm.IniPropStorage1RestoreProperties(Sender: TObject);
begin
  if FileExists(GetPrivateFile(sfTreeViewForm, False)) then
     TIniFormStream.IniPropStorageRestoreProperties(Self, GetPrivateFile(sfTreeViewForm));

end;

procedure TTreeViewForm.IniPropStorage1SaveProperties(Sender: TObject);
begin
  TIniFormStream.IniPropStorageSaveProperties(Self, GetPrivateFile(sfTreeViewForm, False));
end;

procedure TTreeViewForm.MenuItem99Click(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure TTreeViewForm.CollapsePanelClick(Sender: TObject);
begin
  if SidePanel.Align = alLeft then
  begin
    if SidePanel.Width > CollapsePanel.Width then
    begin
      if Constraints.MinWidth > SidePanel.Width then
        Constraints.MinWidth := Constraints.MinWidth - SidePanel.Width;
      SidePanel.AutoSize := False;
      SidePanel.Width := 1;
      CollapseButton.ImageIndex := RIGHTARROW_IMAGE;
    end
    else
    begin
      SidePanel.AutoSize := False;
      SidePanel.Width := 290;
      CollapseButton.ImageIndex := LEFTARROW_IMAGE;
    end;
  end
  else
  begin
    if SidePanel.Width > CollapsePanel.Width then
    begin
      if Constraints.MinWidth > SidePanel.Width then
        Constraints.MinWidth := Constraints.MinWidth - SidePanel.Width;
      SidePanel.AutoSize := False;
      SidePanel.Width := 1;
      CollapseButton.ImageIndex := LEFTARROW_IMAGE;
    end
    else
    begin
      SidePanel.AutoSize := False;
      SidePanel.Width := 290;
      CollapseButton.ImageIndex := RIGHTARROW_IMAGE;
    end;
  end;
end;

procedure TTreeViewForm.Panel2Click(Sender: TObject);
begin
  if SidePanel.Align = alLeft then
    SpeedButton2.Click
  else
    SpeedButton3.Click;
  Invalidate;
end;

procedure TTreeViewForm.Panel2MouseEnter(Sender: TObject);
begin
  Panel2.Color := TE_FRAME_HEADER_HOVER_COLOR;
end;

procedure TTreeViewForm.Panel2MouseLeave(Sender: TObject);
begin
  Panel2.Color := clDefault;
end;

procedure TTreeViewForm.SortTreeByGroupExecute(Sender: TObject);
begin
  Tree.SortTreeByGroup;
  Tree.Refresh;
  DataSaved := False;
end;

procedure TTreeViewForm.SpeedButton2Click(Sender: TObject);
begin
  SidePanel.Align := alRight;
  SidePanel.Left := TreeTabControl.Left + TreeTabControl.Width;
  Splitter1.Align := alRight;
  CollapsePanel.Align := alRight;
  CollapseButton.ImageIndex := RIGHTARROW_IMAGE;
  Panel13.Align := alClient;
  Invalidate;
end;

procedure TTreeViewForm.SpeedButton3Click(Sender: TObject);
begin
  SidePanel.Align := alLeft;
  SidePanel.Left := TreeTabControl.Left - TreeTabControl.Width;
  Splitter1.Align := alLeft;
  CollapsePanel.Align := alLeft;
  CollapseButton.ImageIndex := LEFTARROW_IMAGE;
  Panel13.Align := alClient;
  Invalidate;
end;

procedure TTreeViewForm.Splitter1Moved(Sender: TObject);
begin
  Invalidate;
end;

procedure TTreeViewForm.SubtreeOptionSubmenuItemClick(Sender: TObject);
var
  NodeAttrib: TNodeAttrib = nil;
  nodeindex: integer;
begin
  nodeindex := StrToInt(TMenuItem(Sender).Hint);
  if nodeindex = 0 then exit;
  try
    NodeAttrib := TNodeAttrib.Create;
    Tree.GetSubtreeAttrib(NodeAttrib, nodeindex);
    SetupSubtreeOptionsDlg(NodeAttrib, ProcessSubtreeOptionsMessage, nodeIndex, False);
  finally
    if Assigned(NodeAttrib) then
      NodeAttrib.Free;
  end;
end;

procedure TTreeViewForm.ToolButton11Click(Sender: TObject);
begin
  ActionInfoExecute(Sender);
end;

procedure TTreeViewForm.TreeActionListUpdate(AAction: TBasicAction;var Handled: Boolean);
var
  TimeToolsVisible: Boolean;
  TimeToolsEnabled: Boolean;
begin
  Handled := True;
  if FIsLoadingSession then
    Exit;
  if MillisecondsBetween(Now, FLastUpdateTime) < ACTION_LIST_REFRESH_RATE then
    Exit;
  try
    if (Tree = nil) or FRebuildingTree then
    begin
      DisableActions;
      Exit;
    end
    else
    begin
      FLastUpdateTime := Now;
      EnableActions;
      FGeneDuplicationsFrame.Visible := ActionDisplayGeneDupMarkers.Enabled;
      FCaptionFrame.Enabled := Assigned(CaptionViewer);
      if Assigned(FAncestorsFrame) then
      begin
        FAncestorsFrame.NextChangeBtn.Enabled := ((Tree.NodeFocused or Tree.BranchFocused) and
                                                  (not Tree.IsFocusedOnRoot) and
                                                  (Assigned(MLAnalyzer) or Assigned(FComputeParsimInfo)) and
                                                  (CurAncState <> ActionAncStateShowNone));
        FAncestorsFrame.PrevChangeBtn.Enabled := FAncestorsFrame.NextChangeBtn.Enabled;
        if FAncestorsFrame.Visible then
        begin
          FAncestorsFrame.SetRadioButtonsEnabled(True, Assigned(MLAnalyzer));
          FAncestorsFrame.AncSiteNumSpinEdit.Enabled := (not ActionAncStateShowNone.Checked) and (Tree.MaxSiteIndex > 0);
        end;
      end;
      if Assigned(FCollapseNodesFrame) then
        FCollapseNodesFrame.Enabled := not FJustRootingATree;
      if Assigned(FComputeFrame) then
        FComputeFrame.Enabled := not FJustRootingATree;
      if Tree.IsLinearized or ToggleTimeScaleAction.Enabled then
      begin
        FDistScaleFrame.DistScaleTickIntervalSpinEdit.Enabled := True;
        FDistScaleFrame.DistScaleTickIntervalSpinEdit.MaxValue := FDistScaleFrame.DistScaleLengthSpinEdit.Value;
        FDistScaleFrame.DistScaleTickIntervalSpinEdit.MinValue := FDistScaleFrame.DistScaleLengthSpinEdit.MinValue;
        if Tree.ScaleTick < 0.0000000000001 then
          FDistScaleFrame.DistScaleTickIntervalSpinEdit.Value := FDistScaleFrame.DistScaleLengthSpinEdit.Value
        else
          FDistScaleFrame.DistScaleTickIntervalSpinEdit.Value := Tree.ScaleTick;
        FDistScaleFrame.DistScaleTickIntervalSpinEdit.Increment := FDistScaleFrame.DistScaleLengthSpinEdit.Increment;
      end
      else
      begin
        FDistScaleFrame.DistScaleTickIntervalSpinEdit.Enabled := false;
      end;

      CaptionCopyToClipboardAction.Enabled := CaptionPanel.Visible;
      if (Tree = OriTree) and Assigned(FOriTreeCapViewer) then
        CaptionShowInNewWindowAction.Checked := FOriTreeCapViewer.Visible
      else if (Tree = BootTree) and Assigned(FBootTreeCapViewer) then
        CaptionShowInNewWindowAction.Checked := FBootTreeCapViewer.Visible
      else if (Tree = ReltimeTree) and Assigned(FReltimeTreeCapViewer) then
        CaptionShowInNewWindowAction.Checked := FReltimeTreeCapViewer.Visible;
      CaptionDockInTEAction.Checked := CaptionPanel.Visible;
      CaptionHideAction.Enabled := CaptionPanel.Visible;
      CaptionZoomInAction.Enabled := CaptionPanel.Visible;
      CaptionZoomOutAction.Enabled := CaptionPanel.Visible;
      CaptionSaveToTextAction.Enabled := CaptionPanel.Visible;

      FLayoutFrame.TreeStyleBtn.Enabled := true;
      ActionSaveSession.Visible := (not FJustRootingATree);
      ActionSaveSession.Enabled := ((not FJustRootingATree) and FRootingToolsEnabled);
      if Tree.IsGeneDups then
      begin
        ActionDisplaySpeciesNames.Visible := True;
        ActionDisplaySpeciesNames.Enabled := True;
        ActionLaunchGeneDupsSearch.Enabled := False; // this one is for when the wizard prompts the user to root the tree
        ActionDisplayGeneDupMarkers.Visible := True;
        ActionDisplayGeneDupMarkers.Enabled := Tree.HasGeneDups and (Tree.TreeStyle <> tsCircle) and (Tree.TreeStyle <> tsRadiation);
        ActionDisplayGeneDupMarkers.Checked := Tree.ShowGeneDupMarkers;
        ActionDisplaySpeciationMarkers.Visible := True;
        ActionDisplaySpeciationMarkers.Enabled := Tree.HasSpeciations and (Tree.TreeStyle <> tsCircle) and (Tree.TreeStyle <> tsRadiation);
        ActionDisplaySpeciationMarkers.Checked := Tree.ShowSpeciationMarkers;
        ActionSearchGeneDups.Visible := True;
        ActionSearchGeneDups.Enabled := True;
        N2.Visible := True;
      end
      else
      begin
        ActionLaunchGeneDupsSearch.Enabled := ((Tree = OriTree) and Assigned(MAI) and (FJustRootingATree or (not MAI.IsGeneDupsTree)));
        ActionDisplaySpeciesNames.Visible := True;
        ActionDisplaySpeciesNames.Enabled := FHasSpeciesNames;
        ActionDisplayGeneDupMarkers.Visible := False;
        ActionDisplayGeneDupMarkers.Enabled := False;
        ActionDisplaySpeciationMarkers.Visible := False;
        ActionDisplaySpeciationMarkers.Enabled := False;
        ActionSearchGeneDups.Visible := False;
        ActionSearchGeneDups.Enabled := False;
        N2.Visible := False;
      end;

      { disabling these image exports until we have a chance to detect when images are too large}
      //ActionSaveImageToBmp.Visible := False;
      //ActionSaveImageToBmp.Enabled := False;
      //ActionSaveImageToJpeg.Enabled := False;
      //ActionSaveImageToJpeg.Visible := False;
      //ActionSaveAsGif.Visible := False;
      //ActionSaveASGif.Enabled := False;

      ActionExportAllTrees.Enabled := (Tree.NoOfTrees > 1) and (Tree.TreeIndex > 0);
      ActionExportCurrentTree.Enabled := true; // (Tree.TreeIndex > 0);
      ActionExportCurrentTreeCalibrations.Visible := (Assigned(FCalibrationDlg) and(FCalibrationDlg.NumCalibrations > 0));
      ActionExportCurrentTreeCalibrations.Enabled := ActionExportCurrentTreeCalibrations.Visible and (not ShowingStrictClockTree);
      ActionExportCurrentTreeClock.Enabled := Tree.IsLinearized or IsRelTimeTree;
      ActionExportTimetreeNexus.Enabled := ActionExportCurrentTreeClock.Enabled;
      ActionExportAnalysisSummary.Enabled := (Assigned(MAI) and (MAI.NoOfSeqs > 0) and (not FIsTreeSession));
      ActionExportPartitionList.Enabled := Tree.HasPartitions;
      ExportPairwiseDistancesAction.Enabled := not Tree.ShowTopologyOnly;
      ActionPrint.Enabled := true;
      ActionCopy.Enabled := true;
      //ActionSaveImageToEMF.Enabled := true;
      ActionSaveImageToPDF.Enabled := true;
      ActionSaveImageToTIFF.Enabled := true;
      ActionExportGpNames.Enabled := false;
      ActionImportGpNames.Enabled := false;
      ActionLoadImages.Visible := (not FJustRootingATree);
      ActionFormattingTools.Visible := (not FJustRootingATree);
      ActionFormattingTools.Enabled := (not FJustRootingATree);
      ActionInfo.Enabled := (not FJustRootingATree);

      ActionPoint.Visible := (not FJustRootingATree);
      ActionSwap.Visible := (not FJustRootingATree);
      ActionCompress.Visible := (not FJustRootingATree);
      ActionPoint.Enabled := (not FJustRootingATree);
      ActionSwap.Enabled := (not FJustRootingATree);
      ActionFlip.Enabled := True;
      ActionCompress.Enabled := (not FJustRootingATree);

      ActionSubtreeOption.Visible := (not FJustRootingATree);
      ActionSubtreeOption.Enabled := (not FJustRootingATree) and (FRootingToolsEnabled) and (Tree.NodeFocused or Tree.BranchFocused) and (Tree.NodeInfo.NodeType <> ntRoot);
      ActionRootOnBranch.Visible := FRootingToolsEnabled;
      ActionRootOnBranch.Enabled := ((not Tree.isRooted) or FJustRootingATree) and (not Tree.IsLinearized);
      ActionRootByOutgroup.Visible := FRootingToolsEnabled;
      ActionRootByOutGroup.Enabled := (Tree.IsOutgroup and (not Tree.IsRooted) and (not (Tree = ReltimeTree) and (not FJustRootingATree)));
      FLayoutFrame.RootOnOutgroupBtn.Enabled := ActionRootByOutGroup.Enabled;
      ActionHideOverlappingTaxa.Enabled := true;
      ActionRootOnMidpoint.Visible := FRootingToolsEnabled;
      ActionRootOnMidpoint.Enabled := (Tree.isBranchLength and ActionRootOnBranch.Enabled and (not FJustRootingATree));
      if not ActionRootOnMidpoint.Enabled then
        ActionRootOnMidpoint.Checked := False;
      ActionBalancedTree.Enabled := ((not FJustRootingATree) and FRootingToolsEnabled);
      ActionInputOrderTree.Enabled := ((not FJustRootingATree) and FRootingToolsEnabled);
      ActionDisplaySeparately.Visible := (not FJustRootingATree);
      ActionDisplaySeparately.Enabled := ((not FJustRootingATree) and FRootingToolsEnabled);
      ActionFindTaxaName.Enabled := True;
      ActionAutoSize.Enabled := Tree.TreeStyle = tsTraditional; // autofit is only available for traditional style trees.
      ActionResize.Enabled := True;
      ActionTopology.Visible := (not FJustRootingATree);
      ActionTopology.Enabled := (Tree.isBranchLength or Assigned(Tree.BLenFunc) or (Tree.ForceLinearized and assigned(Tree.DistanceMatrix))) and
                                (Tree.TreeStyle <> tsRadiation) and
                                (Tree <> BootTree) and
                                (not Tree.isCondensed)and
                                (not FJustRootingATree) and
                                ((TreeTabControl.TabIndex < 0) or (TreeTabControl.TabIndex <> TreeTabControl.Tabs.IndexOf(BOOT_CONS_TREE_TAB)));

      ActionRadial.Visible := (not FJustRootingATree);
      ActionRadial.Enabled := Tree.isBranchLength and
                                 (not Tree.IsLinearized) and
                                 (not FJustRootingATree) and
                                 FRootingToolsEnabled and
                                 (not Tree.IsCondensed) and
                                 (not Tree.ShowTopologyOnly);
      ActionCircular.Visible := (not FJustRootingATree);
      ActionCircular.Enabled := ((not FJustRootingATree) and FRootingToolsEnabled);
      ActionRectangular.Enabled := true;
      ActionSlanted.Enabled := true;
      ActionCurved.Enabled := true;

      ActionScaleBarDisplay.Enabled := Tree.isBranchLength;

      DisplayTaxaNamesAction.Enabled := (not FJustRootingATree);
      DisplayTaxaNamesAction.Checked := Tree.ShowOTUName;
      ActionMarkerDisplay.Enabled := true;
      ActionBranchInfoDisplay.Enabled := Tree.isStats and (Tree.TreeStyle = tsTraditional) and (not (Tree = ReltimeTree));
      FNodeStatsFrame.BootstrapFrequencyBtn.Enabled := ActionBranchInfoDisplay.Enabled;
      FNodeStatsFrame.DataCoverageBtn.Enabled := ActionDataCoverageDisplay.Enabled;
      ActionBranchLengthDisplay.Enabled := Tree.isBranchLength and
                                           (not FJustRootingATree) and
                                           (not IsSetRelTimeCalibsMode) and
                                           (Tree.TreeStyle = tsTraditional) and
                                           ((TreeTabControl.Tabs.Count = 0) or (TreeTabControl.TabIndex <> TreeTabControl.Tabs.IndexOf(BOOT_CONS_TREE_TAB)));
      if Assigned(ComputeParsimInfo) and (not ComputeParsimInfo.BranchLengthsAreComputed) then
        ActionBranchLengthDisplay.Enabled := False;
      FBLensFrame.Enabled := ActionBranchLengthDisplay.Enabled;
      ActionNodeIdsDisplay.Enabled := ((Tree.TreeStyle <> tsRadiation) and (Tree.TreeStyle <> tsCircle));
      ActionNodeIdsDisplay.Checked := Tree.ShowNodeIds;
      ActionCondensed.Visible := (not FJustRootingATree);
      ActionCondensed.Enabled := Tree.isStats and (Tree.TreeStyle <> tsRadiation);
      FComputeFrame.CondensedTreeBtn.Enabled := ActionCondensed.Enabled;
      FComputeFrame.CondensedTreeCutoffSpinEdit.Enabled := ActionCondensed.Enabled;

      ActionRecalibrateTimetree.Enabled := IsShowingReltimeTree;
      ActionLinearized.Enabled := Tree.isBranchLength and
                                  (Tree.TreeStyle <> tsRadiation) and
                                  (not Tree.isCondensed) and
                                  (not Tree.ForceLinearized);

      TimeToolsVisible := (ReltimeTree.NoOfTrees > 0);
      TimeToolsEnabled := TimeToolsVisible and
                          (Tree.TreeStyle <> tsRadiation) and
                          (Tree.TreeStyle <> tsCircle) and
                          (Tree = ReltimeTree) and
                          ((ReltimeTree.TimeFactor > 0.0) or (ReltimeTree.LatestTime > 0.0));
      ActionMolecularClock.Enabled := (TimeToolsEnabled and (not Tree.IsSamplingTimes) and (not Tree.FocusedOnOtuNode) and (not Tree.FocusedOnOutgroup) and (not (FCalibrationDlg.NumCalibrations > 0)));
      ActionHideOverlappingDivTimes.Enabled := TimeToolsEnabled;
      ActionDataCoverageDisplay.Enabled := ((Tree.TreeStyle <> tsCircle) and (Tree.TreeStyle <> tsRadiation) and (not (Tree = BootTree)) and (Assigned(MAI) and (not FRelTimeAnalyzer.IsBlensOnly) and MegaForm.HasSequenceData));
      ActionDataCoverageDisplay.Checked := Tree.ShowDataCoverage;
      ActionDisplayDivergenceTimes.Visible := TimeToolsVisible;
      ActionDisplayDivergenceTimes.Enabled := (TimeToolsEnabled and ((ReltimeTree.TimeFactor > 0.0) or (ReltimeTree.LatestTime > 0.0)) and (Tree = ReltimeTree)) or
                                              ((tttReltimeLocal in FTimetreeTypes) and (Tree = ReltimeTree));
      ActionDisplayDivergenceTimes.Checked := Tree.ShowDivergenceTimes;
      ActionDisplayErrorBars.Visible := TimeToolsVisible and Assigned(RelTimeAnalyzer) and not RelTimeAnalyzer.Empty;
        ActionDisplayErrorBars.Enabled := TimeToolsEnabled and (not (Tree.ShowTopologyOnly)) and (not (Tree.BranchStyle = bsCurved)) and (not (Tree.BranchStyle = bsStraight));
      ActionDisplayErrorBars.Checked := Tree.ShowHeightErrBar;

      ActionReltime.Enabled := ReltimeShouldBeEnabled;
      ActionAddCalibration.Enabled := FCalibrationsActionEnabled and
                                      ((Tree.TreeStyle <> tsRadiation) and
                                      (not Tree.IsCondensed) and
                                      (Assigned(MAI)) and
                                      (not IsShowingBootTree)) or
                                      (IsSetRelTimeCalibsMode);

      ToggleTimeScaleAction.Visible := (Assigned(MAI) and IsRelTimeAnalysis(MAI.InitialUsrOperation)) or
                                        (tttRelTimeLocal in FTimetreeTypes) or
                                        FIsRelTimeTreeSession;

      ToggleTimeScaleAction.Enabled := Tree.isBranchLength and
                               (Tree.TreeStyle <> tsRadiation) and
                               (not Tree.IsCondensed) and
                               (ToggleTimeScaleAction.Visible) and
                               (IsShowingReltimeTree) and
                               (not IsShowingBootTree) and
                               (CompareValue(Tree.TimeFactor, 0.0, FP_CUTOFF) <> 0);
      ToggleTimeScaleAction.Checked := Tree.ShowTimeScale;
      ActionNodeIdsDisplay.Checked := Tree.ShowNodeIds;

      ActionConsensus.Visible := (not FJustRootingATree);
      ActionConsensus.Enabled := Tree.isConsensus and
                                 (Tree.TreeStyle <> tsRadiation)and
                                 (Tree.NoOfTrees > 0);

      DisplayTaxaNamesAction.Checked := Tree.ShowOTUName;
      ActionMarkerDisplay.Checked := Tree.ShowOTUMarker;
      ActionBranchInfoDisplay.Checked := ActionBranchInfoDisplay.Enabled and Tree.ShowStats;
      ActionBranchLengthDisplay.Checked := ActionBranchLengthDisplay.Enabled and Tree.ShowBlen;
      ActionScaleBarDisplay.Checked := Tree.ShowScale;

      ActionRadial.Checked := Tree.TreeStyle = tsRadiation;
      ActionCircular.Checked := Tree.TreeStyle = tsCircle;
      ActionRectangular.Checked := (Tree.TreeStyle = tsTraditional) and (Tree.BranchStyle = bsRectangular);
      ActionSlanted.Checked := (Tree.TreeStyle = tsTraditional) and (Tree.BranchStyle = bsStraight);
      ActionCurved.Checked := (Tree.TreeStyle = tsTraditional) and (Tree.BranchStyle = bsCurved);

      ActionTopology.Checked := Tree.ShowTopologyOnly;
      ActionLinearized.Checked := Tree.IsLinearized;
      ActionRelTime.Checked := Tree.IsLinearized;
      ActionCondensed.Checked := Tree.IsCondensed;
      ActionConsensus.Checked := Tree.TreeIndex = 0;

      ActionTaxonFont.Enabled := true;
      ActionBranchInfoFont.Enabled := Tree.isBranchLength or Tree.isStats;
      ActionScaleBarFont.Enabled := Tree.isBranchLength;

      UseSubtreeAttribItem.Enabled := Tree.AttribList.Count > 1;
      UseGroupAttribItem.Enabled   := Tree.GroupAttrib.Count > 0;
      UseSubtreeAttribItem.Checked := Tree.UseSubtreeAttrib;
      UseGroupAttribItem.Checked   := Tree.UseGroupAttrib;

      ActionExportGpNames.Enabled := (FRootingToolsEnabled and (not FJustRootingATree));
      ActionImportGpNames.Enabled := (FRootingToolsEnabled and (not FJustRootingATree));

      miCaptionExpert.Enabled := ((MAI <> nil) and (not FJustRootingATree) and (FRootingToolsEnabled));
      FormatCuttoffItem.Enabled := (ActionCondensed.Enabled or ActionConsensus.Enabled);
    end;
  except
    on E:Exception do
      ShowMessage('Error when update tree viewer action list: ' + E.Message);
  end;
end;

procedure TTreeViewForm.ActionMLLinearizedExecute(Sender: TObject);
var
  ATreeList: TTreeList;
  ATreeData: TTreeData;
  i: Integer;
  CalibrationIndices: ArrayOfInteger;
begin
  FCalibrationDlg.Hide;
  RemoveWindowFromTray(FCalibrationDlg);

  ATreeData := TTreeData.Create(MAI.MyNoOfSeqs, True, False, False);
  OriTree.GetTreeData(ATreeData);
  if FCalibrations.Count > 0 then
    AnchoredMLRelTimeLinearizeFunc(ATreeData)
  else
    RelTimeMLLinearizeFunc(ATreeData);
  ATreeList := TTreeList.Create;
  for i := 0 to MAI.MyNoOfSeqs - 1 do
    ATreeList.OTUName[i] := MAI.MyOtuNames[i];
  ATreeList.Add(ATreeData);
  //ReltimeTree.LinearizeFunc := nil;
  ReltimeTree.SetTreeList(ATreeList, false);
  if Assigned(FOtuInfos) then
    ReltimeTree.SetOtuInfo(FOtuInfos);
  ReltimeTree.TimeFactor := RelTimeAnalyzer.TimeFactor;
  ReltimeTree.LatestTime := RelTimeAnalyzer.LatestTime;
  ReltimeTree.ShowHeightErrBar := False;
  ReltimeTree.ShowTopologyOnly := False;
  ReltimeTree.ShowCalibratedNodeMarker := (FCalibrations.CalibrationsType <> ctLeafNode);
  if TreeTabControl.Tabs.IndexOf(ORI_TREE_TAB) < 0 then
    TreeTabControl.Tabs.Add(ORI_TREE_TAB);
  if TreeTabControl.Tabs.IndexOf(TIME_TREE_TAB) < 0 then
    TreeTabControl.Tabs.Add(TIME_TREE_TAB);
  FDivTimesFrame.Enabled := True;
  TreeTabControl.TabIndex := TreeTabControl.Tabs.IndexOf(TIME_TREE_TAB);
  TreeTabControl.Enabled := true;
  Tree.Enabled := true;
  if FCalibrations.Count > 0 then
  begin
    SetLength(CalibrationIndices, FCalibrations.Count);
    for i := 0 to FCalibrations.Count - 1 do
      CalibrationIndices[i] := FCalibrations.GetCalibration(i).TreeViewFormIndex;
    ReltimeTree.CalibratedNodes := CalibrationIndices;
  end;

  if Length(ReltimeTree.CalibratedNodes) = 0 then
    ReltimeTree.TimeFactor := 1.0; // needed for timetrees with only relative times

  OriTree.SessionIsReltime := True;
  ReltimeTree.Value2[ReltimeTree.TreeIndex] := MLAnalyzer.MLTree.LogLikelihood;
  ResetStatusBar;
  if ActionInfo.Checked then
  begin
    RenewTreeInfo;
    RenewBranchInfo;
  end;
  if Length(FRelTimeTreeStates) > 0 then
    SaveRelTimeTreeViewState(ReltimeTree.TreeIndex - 1);
end;

procedure TTreeViewForm.ActionNodeIdsDisplayExecute(Sender: TObject);
begin
  Tree.ShowNodeIds := not Tree.ShowNodeIds;
  if Tree.ShowNodeIds then
  begin
    Tree.ShowStats := False;
    Tree.ShowDataCoverage := False;
  end;
  try
    FCheckStateChanging := True;
    UpdateStatsDisplay;
    Tree.Invalidate;
    DataSaved := false;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.ActionCompressExecute(Sender: TObject);
var
  NodeAttrib: TNodeAttrib;
begin
  if Tree.BranchFocused then
    Tree.FocusOnNode(Tree.FocusedIndex);
  if Tree.NodeFocused then
  begin
    if Tree.NodeInfo.NodeType <> ntRoot then
    begin
      if Tree.NodeInfo.NodeType = ntOTU then
      begin
        if CurTool = ActionPoint then
          ActionPointExecute(Sender);
        Exit;
      end;

      NodeAttrib := TNodeAttrib.Create;
      if not Tree.GetSubtreeAttrib(NodeAttrib, Tree.FocusedIndex) then
        NodeAttrib.Caption := '';
      NodeAttrib.ManualCompressed := not NodeAttrib.ManualCompressed;
      if NodeAttrib.ManualCompressed then
        NodeAttrib.AutoCompressed := False;
      Tree.SetSubtreeAttrib(NodeAttrib, Tree.FocusedIndex);
      Tree.Refresh;
      ActionSubtreeOptionExecute(Self);

      if ActionInfo.Checked then
        RenewBranchInfo;

      NodeAttrib.Free;
      DataSaved := False;
    end;

    if CurTool = ActionPoint then
      ActionPointExecute(Sender);
  end
  else if Tree.Cursor <> CURSOR_COMPRESS then
  begin
    CurTool.Checked := False;
    CurTool := ActionCompress;
    CurTool.Checked := True;
    Tree.Cursor := CURSOR_COMPRESS;
  end
  else
    ActionPointExecute(nil);
  Invalidate;
end;

procedure TTreeViewForm.ActionCondensedExecute(Sender: TObject);
begin

  if not Tree.IsCondensed then
  begin
    ActionCondensed.Checked := True;
    if Sender = FComputeFrame.CondensedTreeBtn then
      ApplyConsensusCondensedTreeOptions(FComputeFrame.ConsensusTreeCutoffSpinEdit.Value, FComputeFrame.CondensedTreeCutoffSpinEdit.Value)
    else
      FormatCuttoffItemClick(Sender);
  end
  else
  begin
    Tree.IsCondensed := False;
    Tree.Refresh;
  end;
end;

procedure TTreeViewForm.ActionConsensusExecute(Sender: TObject);
begin
  if ActionConsensus.Checked then
  begin
    ActionConsensus.Checked := False;
    if IsBootTree then
      with TreeTabControl do
        if Tree = OriTree then
          TreeTabControl.Tabs[Tabs.IndexOf(CONSENSUS_TREE_TAB)] := ORI_TREE_TAB
        else if Tree = BootTree then
          TreeTabControl.Tabs[Tabs.IndexOf(BOOT_CONS_TREE_TAB)] := BOOT_TREE_TAB;
    if Tree = OriTree then
      if CurOriTreeIndex = 0 then
        Tree.TreeIndex := 1
      else
        Tree.TreeIndex := CurOriTreeIndex
    else if Tree = BootTree then
      if CurBootTreeIndex = 0 then
        Tree.TreeIndex := 1
      else
        Tree.TreeIndex := CurBootTreeIndex;
    if Tree.NoOfTrees > 1 then
    begin
      FMultiTreesFrame.TreeNumSEdit.MaxValue := Tree.NoOfTrees;
      FMultiTreesFrame.TreeNumSEdit.Value := Tree.TreeIndex;
      FMultiTreesFrame.NumTreesLabel.Caption := 'of ' + Format('%-6d', [Tree.NoOfTrees]);
      FMultiTreesFrame.Show;
    end
    else
      FMultiTreesFrame.Hide;
    if InfoDisplay.Visible then
    begin
      RenewTreeInfo;
      RenewBranchInfo;
    end;
    ResetStatusBar;
    Tree.Refresh;
  end
  else
  begin
    ActionConsensus.Checked := True;
    FormatCuttoffItemClick(Sender);
    //Exit;
  end;

  //if ActionConsensus.Checked then
  //begin
  //  if IsBootTree then
  //    with TreeTabControl do
  //      if Tree = OriTree then
  //        TreeTabControl.Tabs[Tabs.IndexOf('Consensus tree')] := ORI_TREE_TAB
  //      else if Tree = BootTree then
  //        TreeTabControl.Tabs[Tabs.IndexOf('Bootstrap consensus tree')] := 'Bootstrap tree';
  //  if Tree = OriTree then
  //    if CurOriTreeIndex = 0 then
  //      Tree.TreeIndex := 1
  //    else
  //      Tree.TreeIndex := CurOriTreeIndex
  //  else if Tree = BootTree then
  //    if CurBootTreeIndex = 0 then
  //      Tree.TreeIndex := 1
  //    else
  //      Tree.TreeIndex := CurBootTreeIndex;
  //  if Tree.NoOfTrees > 1 then
  //  begin
  //    TreeNumSEdit.MaxValue := Tree.NoOfTrees;
  //    TreeNumSEdit.Value := Tree.TreeIndex;
  //    NumTreesLabel.Caption := 'of ' + Format('%-6d', [Tree.NoOfTrees]);
  //    MultiTreeToolBx.Show;
  //  end
  //  else
  //    MultiTreeToolBx.Hide;
  //end
  //else
  //begin
  //  TreeStatusBar.Panels[0].Text := 'Now, computing consensus tree.....';
  //  if IsBootTree then
  //    with TreeTabControl do
  //      if Tree = OriTree then
  //        TreeTabControl.Tabs[Tabs.IndexOf(ORI_TREE_TAB)] := 'Consensus tree'
  //      else if Tree = BootTree then
  //        TreeTabControl.Tabs[Tabs.IndexOf('Bootstrap tree')] := 'Bootstrap consensus tree';
  //  if Tree = OriTree Then
  //    CurOriTreeIndex := Tree.TreeIndex
  //  else if Tree = BootTree then
  //    CurBootTreeIndex := Tree.TreeIndex;
  //  Tree.TreeIndex := 0;
  //  MultiTreeToolBx.Hide;
  //end;
  //if InfoDisplay.Visible then
  //begin
  //  RenewTreeInfo;
  //  RenewBranchInfo;
  //end;
  //ResetStatusBar;
  //Tree.Refresh;
end;

procedure TTreeViewForm.ActionCopyExecute(Sender: TObject);
begin
  if CaptionPanel.Visible then
  begin
    if CaptionViewer.Focused then
      CaptionCopyToClipboardActionExecute(Sender)
    else
      Tree.CopyImageToClipBoard;
  end
  else
    Tree.CopyImageToClipBoard;
end;

procedure TTreeViewForm.ActionCurvedExecute(Sender: TObject);
begin
    Tree.TreeStyle := tsTraditional;
    Tree.BranchStyle := bsCurved;
    PropagateTreeStyleChange;
    Tree.Refresh;
    FLayoutFrame.TreeStyleBtn.ImageIndex := CURVEDTREESTYLE_IMAGE;
    TreeStyleBtn2.ImageIndex := CURVEDTREESTYLE_IMAGE;
    TreeTabControl.Enabled := true;
    DataSaved := false;
end;

procedure TTreeViewForm.ActionDataCoverageDisplayExecute(Sender: TObject);
begin
  Tree.ShowDataCoverage := not Tree.ShowDataCoverage;
  if Tree.ShowDataCoverage then
  begin
    Tree.ShowStats := False;
    Tree.ShowNodeIds := False;
  end;
  try
    FCheckStateChanging := True;
    UpdateStatsDisplay;
    Tree.Invalidate;
    DataSaved := false;
  finally
    FCheckStateChanging := False
  end;
end;

procedure TTreeViewForm.ActionDisplayDivergenceTimesExecute(Sender: TObject);
begin
    if not (Tree = ReltimeTree) then
      Exit;
    Tree.ShowDivergenceTimes := not Tree.ShowDivergenceTimes;
    Tree.ShowCalibratedNodeMarker := Tree.ShowDivergenceTimes;

    if Tree.ShowDivergenceTimes then
    begin
      FigureGenerator.AssignData(DISP_DIV_TIMES_STR, 'True');
      if ReltimeTree.ShowCharState then
        ActionAncStateShowNoneExecute(Sender);
    end
    else
      FigureGenerator.AssignData(DISP_DIV_TIMES_STR, 'False');
    try
      FCheckStateChanging := True;
      if FDivTimesFrame.DivTimesDisplayCheckBx.Checked <> Tree.ShowDivergenceTimes then
        FDivTimesFrame.DivTimesDisplayCheckBx.Checked := Tree.ShowDivergenceTimes;
    finally
      FCheckStateChanging := False;
    end;
    Tree.Invalidate;
    DataSaved := False;
    ResetStatusBar;
end;

procedure TTreeViewForm.ActionDisplayErrorBarsExecute(Sender: TObject);
begin
    ReltimeTree.ShowHeightErrBar := not ReltimeTree.ShowHeightErrBar;
    if ReltimeTree.ShowHeightErrBar then
    begin
      FigureGenerator.AssignData(DISP_ERR_BARS_STR, 'True');
      if ReltimeTree.ShowCharState then
        ActionAncStateShowNoneExecute(Sender);
    end
    else
      FigureGenerator.AssignData(DISP_ERR_BARS_STR, 'False');
    ReltimeTree.Refresh;
    RefreshCaption;
    DataSaved := False;
    try
      FCheckStateChanging := True;
    finally
      FCheckStateChanging := False;
      FDivTimesFrame.ShowNodeHeightErrBarsCheckBx.Checked := ReltimeTree.ShowHeightErrBar;
    end;
end;

procedure TTreeViewForm.ActionDisplayGeneDupMarkersExecute(Sender: TObject);
begin
  Assert(Tree.IsGeneDups, 'display gene dups action enabled for a non-genedup tree');
  Tree.ShowGeneDupMarkers := not Tree.ShowGeneDupMarkers;
  if Tree.ShowGeneDupMarkers = True then
  begin
    OriTree.ShowGeneDupMarkers := True;
    BootTree.ShowGeneDupMarkers := True;
    ReltimeTree.ShowGeneDupMarkers := True;
  end
  else
  begin
    OriTree.ShowGeneDupMarkers := False;
    BootTree.ShowGeneDupMarkers := False;
    ReltimeTree.ShowGeneDupMarkers := False;
  end;
  ActionDisplayGeneDupMarkers.Checked := Tree.ShowGeneDupMarkers;
  Tree.Refresh;
  DataSaved := false;
end;

procedure TTreeViewForm.ActionDisplaySeparatelyExecute(Sender: TObject);
begin
    if Tree.BranchFocused or Tree.NodeFocused then
    begin
      //ZoomTool.Down := false;
      ShowSubtreeWindow;
      if CurTool = ActionPoint then
        ActionPointExecute(Sender);
    end
    else if Tree.Cursor <> CURSOR_ZOOM then
    begin
      CurTool.Checked := false;
      CurTool := ActionDisplaySeparately;
      CurTool.Checked := true;
      Tree.Cursor := CURSOR_ZOOM;
    end
    else
      ActionPointExecute(nil);
end;

procedure TTreeViewForm.ActionDisplaySpeciationMarkersExecute(Sender: TObject);
begin
  Assert(Tree.IsGeneDups, 'display speciation markers enabled on a non-gene dup tree');
  Tree.ShowSpeciationMarkers := not Tree.ShowSpeciationMarkers;
  if Tree.ShowSpeciationMarkers = True then
  begin
    OriTree.ShowSpeciationMarkers := True;
    BootTree.ShowSpeciationMarkers := True;
    ReltimeTree.ShowSpeciationMarkers := True;
  end
  else
  begin
    OriTree.ShowSpeciationMarkers := False;
    BootTree.ShowSpeciationMarkers := False;
    ReltimeTree.ShowSpeciationMarkers := False;
  end;
  ActionDisplaySpeciationMarkers.Checked := Tree.ShowSpeciationMarkers;
  Tree.Refresh;
  DataSaved := false;
end;

procedure TTreeViewForm.ActionDisplaySpeciesNamesExecute(Sender: TObject);
begin
  OriTree.ShowSpeciesName := not OriTree.ShowSpeciesName;
  if OriTree.ShowSpeciesName then
  begin
    OriTree.ShowOTUName := False;
    BootTree.ShowOtuName := False;
    ReltimeTree.ShowOtuName := False;
  end
  else
  begin
    OriTree.ShowOtuName := True;
    BootTree.ShowOtuName := True;
    ReltimeTree.ShowOtuName := True;
  end;

  if IsBootTree then
    BootTree.ShowSpeciesName := OriTree.ShowSpeciesName;
  if IsReltimeTree then
    ReltimeTree.ShowSpeciesName := OriTree.ShowSpeciesName;
  Tree.Refresh;
  DataSaved := false;
  try
    FCheckStateChanging := True;
    if Assigned(FGeneDuplicationsFrame) then
      if Tree.ShowSpeciesName then
        FGeneDuplicationsFrame.DisplaySpeciesNamesBtn.Checked := True
      else if Tree.ShowOTUName then
        FGeneDuplicationsFrame.DisplaySequenceNamesBtn.Checked := True;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.ActionExportAllTreesExecute(Sender: TObject);
var
  AList: TStringList;
  Options: TNewickExportOptions;
  ExportOptionsForm: TNewickExportOptionsForm;
  Response: Integer;
begin
  AList := nil;
  ExportOptionsForm := nil;

  try
    ExportOptionsForm := TNewickExportOptionsForm.Create(Self);
    Options := GetNewickExportOptions;
    ExportOptionsForm.SelectEnabledCheckboxes(Options);
    Response := ExportOptionsForm.ShowModal;
    if Response <> mrYes then
      Exit;
    Options := ExportOptionsForm.ExportOptions;
    AList := Tree.GetAllNewickTrees(Options);
    OpenStringList(AList, 'Newick Export.nwk');
  finally
    if Assigned(ExportOptionsForm) then
      FreeAndNil(ExportOptionsForm);
    if Assigned(AList) then
      AList.Free;
  end;
end;

procedure TTreeViewForm.ActionExportAnalysisSummaryExecute(Sender: TObject);
var
  AList: TStringList;
begin
  AList := nil;

  try
    try
      AList := MAI.AnalysisSummary.ToStringList;
      if Assigned(AList) then
        OpenStringList(AList, 'AnalysisSummary');
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(AList) then
      AList.Free;
  end;
end;

procedure TTreeViewForm.ActionExportCurrentTreeCalibrationsExecute(
  Sender: TObject);
begin
  FCalibrationDlg.ExportSelectedCalibrationsToEditor;
end;

procedure TTreeViewForm.ActionExportCurrentTreeClockExecute(Sender: TObject);
var
  Filename: String;
  SaveLocation: String;
  PleaseWait : TPleaseWait = nil;
  ExportType : TExportType;
  aCaption: TStringList = nil;
  eb: TReltimeExportBuilder = nil;
  spreadsheet: TExcelWrite = nil;
  aExport: TStringList = nil;
  aLabels: TStringList = nil;
  aNames: TStringList = nil;
  minTimes, maxTimes: array of Extended;
begin
  SetLength(minTimes, 0);
  SetLength(maxTimes, 0);
  Filename := ExtractFilename(MAI.MyUserTreeFile);
  if (Trim(Filename) = EmptyStr) and (Trim(SessionFileName) <> EmptyStr) then
    Filename := ExtractFilename(SessionFileName);
  SaveLocation := ChangeFileExt(MAI.MyUserTreeFile, '-timetree');
  ExportType := PromptUserWriteOutput(SaveLocation, True, EXexcelXmlDisp);
  if (ExportType = EXnone) or (Trim(SaveLocation) = EmptyStr) then
    Exit;

  try
    try
      PleaseWait := TPleaseWait.Create(Self);
      PleaseWait.Action := 'Generating Clock Tree Export';
      PleaseWait.Show;

      if not ExportIsTextEditorDisplay(ExportType) then
      begin
        aCaption := TStringList.Create;
        FigureGenerator.GenerateLegendAsText(aCaption);
        if ShowingStrictClockTree then
        begin
          aLabels := TStringList.Create;
          ReltimeTree.GetInternalNodeLabels(aLabels);
          aNames := ReltimeTree.GetOTUNamesList;
          if Length(ReltimeTree.CalibratedNodes) > 0 then
          begin
            GetStrictClockMinMaxTimes(minTimes, maxTimes);
            spreadsheet := FRelTimeAnalyzer.GenerateStrictClockSpreadsheetExport(aNames, aLabels, minTimes, maxTimes, aCaption);
          end
          else
            spreadsheet := FRelTimeAnalyzer.GenerateStrictClockSpreadsheetExport(aNames, aLabels, aCaption);
        end
        else
        begin
          eb := FRelTimeAnalyzer.ExportBuilder;
          eb.AddCaptionAsWorksheet(aCaption);
          spreadsheet := eb.GenerateExport;
        end;
        spreadsheet.SaveFile(SaveLocation, ExcelExportToFiletype(ExportType));
        If ExportIsWorkbookDisplay(ExportType) then
          RunAProgram(SaveLocation)
      end
      else
      begin
        if ShowingStrictClockTree then
        begin
          GetStrictClockMinMaxTimes(minTimes, maxTimes);
          aLabels := TStringList.Create;
          ReltimeTree.GetInternalNodeLabels(aLabels);
          aExport := TStringList.Create;
          aNames := ReltimeTree.GetOTUNamesList;
          FRelTimeAnalyzer.GenerateCalibratedClockTreeExport(aExport, aNames, aLabels, minTimes, maxTimes);
        end
        else
          aExport := MAI.ClockTreeExport;

        if ExportType = EXtext then
          OpenStringList(aExport, ChangeFileExt(Filename, '_timetree.txt'))
        else
          aExport.SaveToFile(ChangeFileExt(Filename, '_timetree.txt'));

        if not ShowingStrictClockTree then
          aExport := nil;
      end;
    except
     on E: Exception do
       ShowMessage('Oh no! An error has occurred when exporting the timetree results: ' + E.Message);
    end
  finally
    if Assigned(aCaption) then
      aCaption.Free;
    if Assigned(PleaseWait) then
      PleaseWait.Free;
    if Assigned(spreadsheet) then
      spreadsheet.Free;
    if Assigned(aExport) then
      aExport.Free;
    if Assigned(aLabels) then
      aLabels.Free;
    if Assigned(aNames) then
      aNames.Free;
  end;
end;

procedure TTreeViewForm.ActionExportCurrentTreeExecute(Sender: TObject);
var
  Options: TNewickExportOptions;
  js: String;
begin
  js := EmptyStr;
  try
    ShowOptionsDialog;
    HtmlOptionsDialog.ProcessMessageReceivedProc := ProcessNewickOptionsMessage;
    HtmlOptionsDialog.DomProcedureName := VISITDOMPROC_NEWICK_EXPORT_OPTIONS;
    Options := GetNewickExportOptions;
    if not Options.BranchLengths then
      js := js + Format('$("#%s").attr("disabled", "disabled");', [NEWICK_BRANCH_LENGTHS]) + LineEnding;
    if not Options.BootstrapVals then
      js := js + Format('$("#%s").attr("disabled", true);', [NEWICK_BOOTSTRAP_VALUES]) + LineEnding;
    if not Options.GeneDuplications then
      js := js + Format('$("#%s").attr("disabled", "disabled");', [NEWICK_GENE_DUPLICATIONS]) + LineEnding;
    if not Options.SpeciationEvents then
      js := js + Format('$("#%s").attr("disabled", "disabled");', [NEWICK_SPECIATIONS]) + LineEnding;
    if not Options.DivergenceTimes then
      js := js + Format('$("#%s").attr("disabled", "disabled");', [NEWICK_DIVERGENCE_TIMES]) + LineEnding;
    if not Options.Reltimes then
      js := js + Format('$("#%s").attr("disabled", "disabled");', [NEWICK_RELATIVE_TIMES]) + LineEnding;
    if not Options.NodeLabels then
      js := js + Format('$("#%s").attr("disabled", "disabled");', [NEWICK_NODE_LABELS]) + LineEnding;
    if Tree = ReltimeTree then
    begin
      js := js + Format('$("#accordion").accordion({ active: 2 });', []) + LineEnding;
      js := js + Format('$("#%s").attr("ischecked", "%s");', [NEWICK_RELATIVE_TIMES, BoolToStr(not Options.DivergenceTimes, True)]) + LineEnding;
      js := js + Format('$("#%s").prop("checked", %s);', [NEWICK_RELATIVE_TIMES, BoolToStrLC(not Options.DivergenceTimes, True)]) + LineEnding;
      js := js + Format('$("#%s").attr("ischecked", "%s");', [NEWICK_DIVERGENCE_TIMES, BoolToStr(Options.DivergenceTimes, True)]) + LineEnding;
      js := js + Format('$("#%s").prop("checked", %s);', [NEWICK_DIVERGENCE_TIMES, BoolToStrLC(Options.DivergenceTimes, True)]) + LineEnding;
    end;
    HtmlOptionsDialog.LoadOptionsFile(wofNewickExportOptions, js, 'Newick Export Options', 380, 510, HC_Tree_Explorer);
    {$IFNDEF DARWIN}
    AlphaBlend := True;
    AlphaBlendValue := 150;
    Enabled := False;
    {$ELSE}
    FormIsClosing := True;
    {$ENDIF}
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TTreeViewForm.ActionExportGpNamesExecute(Sender: TObject);
var
  sl: TStringList;
  str: AnsiString;
  i: integer;
begin
  sl := nil;
  try
    sl := TStringList.Create;
    if Tree.UseSubtreeAttrib then
      for i := 1 to Tree.NoOfOTUs do
        if Tree.ClusterName[i] <> '' then
        begin
          str := Tree.OTUName[i]+'='+Tree.ClusterName[i];
          while pos(' ', str) > 0 do
            str[pos(' ', str)] := '_';
          sl.Add(str);
        end;
    if Tree.UseGroupAttrib then
      for i := 1 to Tree.NoOfOTUs do
        if Tree.GroupName[i] <> '' then
        begin
          str := Tree.OTUName[i]+'='+Tree.GroupName[i];
          while pos(' ', str) > 0 do
            str[pos(' ', str)] := '_';
          if sl.IndexOf(str) < 0 then
            sl.Add(str);
        end;

    if sl.Count > 0 then
      OpenStringList(sl, 'group_names.txt', true)
    else
      ShowMessage('No groups have been defined. Nothing to export');
  finally
    if sl <> nil then
      sl.Free;
  end;
end;

procedure TTreeViewForm.ActionExportPartitionListExecute(Sender: TObject);
var
  aList: TStringList;
  PartitionsList: TPartitionList;
  Cutoff: Double;
  NumReps: Integer;
begin
  aList := nil;
  Assert(Tree.HasPartitions);
  try
    try
      if Tree.ConsensusValue > 0 then
        Cutoff := Tree.ConsensusValue / 100
      else
        Cutoff := 0.0;
      PartitionsList := Tree.GetPartitionList;
      NumReps := Max(MAI.MyBootReps, MAI.MyNoOfReps);
      PartitionsList.BuildPartitionsSummary(NumReps, Cutoff);
      aList := Tree.GetPartitionList.PartitionsSummary;
      OpenStringList(aList, 'Partions List', True);
    except
      on E:Exception do
        ShowMessage('Oh no! An error occured when exporting the partition list: ' + E.Message);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TTreeViewForm.ActionExportTimetreeNexusExecute(Sender: TObject);
var
  Filename: String;
begin
  Filename := ExtractFileName(MegaForm.DataFileName);
  try
    OpenStringList(MAI.ReltimeNexusExport, ChangeFileExt(Filename, '_timetree.nexus'));
  Except
    ShowMessage('Oh no! An error occured when trying to export the clock tree.');
  end;
end;

procedure TTreeViewForm.ActionExportTreeTabularExecute(Sender: TObject);
var
  Output: TStringList;
  CurStr: AnsiString;

  procedure WriteInfo(AIndex: Integer);
  var
    TempDes2Index: Integer;
  begin
    Tree.FocusOnNode(AIndex);
    if Tree.NodeInfo.Des1Index > Tree.NoOfOTUs then
      WriteInfo(Tree.NodeInfo.Des1Index);

    Tree.FocusOnNode(AIndex);
    if Tree.NodeInfo.Des2Index > Tree.NoOfOTUs then
      WriteInfo(Tree.NodeInfo.Des2Index);

    Tree.FocusOnNode(AIndex);
    CurStr := IntToStr(AIndex)  + ', ';
    if Tree.NodeInfo.Des1Index <= Tree.NoOfOTUs then
      CurStr := CurStr + Tree.OtuName[Tree.NodeInfo.Des1Index]
    else
      CurStr := CurStr + IntToStrWidth(Tree.NodeInfo.Des1Index,5);
    CurStr := CurStr + ', ';

    Tree.FocusOnNode(AIndex);
    if Tree.NodeInfo.Des2Index <= Tree.NoOfOTUs then
      CurStr := CurStr + Tree.OtuName[Tree.NodeInfo.Des2Index]
    else
      CurStr := CurStr + IntToStrWidth(Tree.NodeInfo.Des2Index,5);
    CurStr := CurStr + ', ';

    TempDes2Index := Tree.NodeInfo.Des2Index; // We need to save this now, because once we focus on Des1Index we have shifted to a new node, and it's Des2Index is not the one we are interested in.
    Tree.FocusOnBranch(Tree.NodeInfo.Des1Index);
    CurStr := CurStr + FloatToStrF(Tree.BranchInfo.Length, ffFixed,6,10) +', ';
    Tree.FocusOnBranch(TempDes2Index);
    CurStr := CurStr + FloatToStrF(Tree.BranchInfo.Length, ffFixed,6,10);
    Output.Add(CurStr);
  end;
begin
  Output := TStringList.Create;
  Output.Add('AncId, Desc1, Desc2, Branch Length 1, Branch Length 2');
  WriteInfo(Tree.NoOfNodes); // root node
  OpenStringList(Output, 'Export Table');
  Output.Free;
end;

procedure TTreeViewForm.ActionFindTaxaNameExecute(Sender: TObject);
begin
  if MyFindDlg = nil then
    MyFindDlg := TFindDlg.Create(Self);

  MyFindDlg.Tree := Tree;
  MyFindDlg.Show;
end;

procedure TTreeViewForm.ActionBalancedTreeExecute(Sender: TObject);
begin
  if ActionBalancedTree.Checked then
  begin
    ActionBalancedTree.Checked := False;
    SortClusterShapeItem.Checked := False;
  end
  else
  begin
    ActionBalancedTree.Checked   := True;
    ActionInputOrderTree.Checked := False;
    SortClusterShapeItem.Checked := True;
    SortClusterOrderItem.Checked := False;
    Tree.SortClusterForShape;
    Tree.Refresh;
    DataSaved := False;
  end;
  try
    FCheckStateChanging := True;
    if Assigned(FTaxaNamesFrame) then
    begin
      FTaxaNamesFrame.BalancedShapeRadioBtn.Checked := ActionBalancedTree.Checked;
      FTaxaNamesFrame.InputOrderRadioBtn.Checked := ActionInputOrderTree.Checked;
    end;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.ActionBLenFontExecute(Sender: TObject);
begin
  FontDialog.Font.Assign(Tree.BLensFont);
  if FontDialog.Execute then
  begin
    OriTree.BLensFont := FontDialog.Font;
    if IsReltimeTree then
      ReltimeTree.BLensFont := FontDialog.Font;
    Tree.Invalidate;
    DataSaved := False;
    FBLensFrame.BlensFontSizeSpinEdit.Value := Tree.BLensFont.Size;
  end;
end;

procedure TTreeViewForm.ActionAutoSizeExecute(Sender: TObject);
begin
  ResizeTreeToFitWindow;
end;

procedure TTreeViewForm.ActionAncStateShowAllExecute(Sender: TObject);
begin
  CurAncState := ActionAncStateShowAll;
  if ActionAncStateExtended.Checked then
    OriTree.AncStateProc := ShowExtCharAncStateProc
  else
    OriTree.AncStateProc := ShowAllAncStateProc;
  OriTree.ShowCharState := true;
  if IsBootTree and (MLAnalyzer = nil) then
  begin
    BootTree.AncStateProc := OriTree.AncStateProc;
    BootTree.ShowCharState := True;
  end;

  if IsReltimeTree then
  begin
    ReltimeTree.AncStateProc := OriTree.AncStateProc;
    ReltimeTree.ShowCharState := True;
    ReltimeTree.ShowDivergenceTimes := False;
    ReltimeTree.ShowHeightErrBar := False;
  end;
  Tree.Refresh;
  DataSaved := False;
  RenewAncState;
  try
    FCheckStateChanging := True;
    if IsReltimeTree then
      FDivTimesFrame.DivTimesDisplayCheckBx.Checked := False;
    UpdateAncestralStateActions;
    UpdateAncestorRadioBtns;
    UpdateCaptionForAncestralStates;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.ActionAncStateHideAmbiguousExecute(Sender: TObject);
begin
  if CurAncState = ActionAncStateHideAmbiguous then Exit;
  CurAncState := ActionAncStateHideAmbiguous;

  OriTree.AncStateProc := HideAmbigAncStateProc;
  OriTree.ShowCharState := true;
  if IsBootTree and (MLAnalyzer = nil) then begin
    BootTree.AncStateProc := HideAmbigAncStateProc;
    BootTree.ShowCharState := true;
  end;
  if IsReltimeTree then
  begin
    ReltimeTree.AncStateProc := HideAmbigAncStateProc;
    ReltimeTree.ShowCharState := true;
    ReltimeTree.ShowDivergenceTimes := False;
    ReltimeTree.ShowHeightErrBar := False;
  end;
  Tree.Refresh;
  DataSaved := false;
  RenewAncState;
  try
    FCheckStateChanging := True;
    if IsReltimeTree then
      FDivTimesFrame.DivTimesDisplayCheckBx.Checked := False;
    UpdateAncestralStateActions;
    UpdateAncestorRadioBtns;
    UpdateCaptionForAncestralStates;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.ActionAncStateExtendedExecute(Sender: TObject);
begin
  ActionAncStateExtended.Checked := not ActionAncStateExtended.Checked;
  if ActionAncStateExtended.Checked then
    ActionAncStateShowAllExecute(ExtCharAncItem)
  else
  begin
    UpdateCaptionForAncestralStates;
    RenewAncState;
  end;
  Tree.Invalidate;
end;

procedure TTreeViewForm.ActionAncStateFontExecute(Sender: TObject);
begin
  FontDialog.Font.Assign(Tree.CharStateFont);
  if FontDialog.Execute then begin
      OriTree.CharStateFont := FontDialog.Font;
      if IsBootTree then
          BootTree.CharStateFont := OriTree.CharStateFont;
      if IsReltimeTree then
        ReltimeTree.CharStateFont := OriTree.CharStateFont;
    Tree.Refresh;
    DataSaved := false;
    FAncestorsFrame.AncStateFontSizeSpinEdit.Value := Tree.CharStateFont.Size;
  end;
end;

procedure TTreeViewForm.ActionAddCalibrationExecute(Sender: TObject);
var
  isFixedRate: Boolean = False;
begin
  if Sender is TMenuItem then
    isFixedRate := (TMenuItem(Sender).Name = 'FixedRateItem');
  if FCalibrationDlg.CalibrationsType = ctInternalNode then
    if ((not Tree.NodeFocused) or (Tree.FocusedIndex <= Tree.NoOfOtus)) and (not isFixedRate) then
    begin
      ShowMessage('Please select an internal node in the tree for adding calibration time constraints');
      Exit;
    end;

  if FCalibrationDlg.CalibrationsType = ctLeafNode then
    if (not Tree.NodeFocused) or (Tree.FocusedIndex > Tree.NoOfOtus) then
    begin
      if Tree.BranchFocused and (Tree.FocusedIndex <= Tree.NoOfOtus) then
        Tree.FocusOnNode(Tree.FocusedIndex)
      else
      begin
        ShowMessage('Please select a leaf node in the tree for adding sample times');
        Exit;
      end;
    end;

  if Tree.NodeInfo.IsOrHasChildInOutgroup and (not isFixedRate) and (not (FCalibrationDlg.CalibrationsType = ctInternalNode)) then
  begin
    ShowMessage('Calibrations cannot be applied to outroup nodes');
    Exit;
  end;

  FCalibrationDlg.Show;
  if FCalibrationDlg.CalibrationsType = ctInternalNode then
    FCalibrationDlg.NewCalibrationExternal(Sender, Tree.FocusedIndex)
  else
    FCalibrationDlg.NewCalibrationExternal(Sender, Tree.FocusedIndex, Tree.GetTaxonName(Tree.FocusedIndex));
  Tree.ShowCalibratedNodeMarker := True;
end;

procedure TTreeViewForm.ActionActStateExportMostProbableSequenceExecute( Sender: TObject);
var
  NodeName: AnsiString = '';
  MaxNameLength: integer = -1;
  sequence: AnsiString = '';
  name: AnsiString = '';
  InterleavedNumber : integer = -1;
  node, site, i, DataOffsetX, DataOffsetY:integer; //Node is the current node we are on, site is the current site we are on, line is used as a temporary variable to loop throught a TStringList
  MaxProbability : double = 0;    //Hold the probability of the most probable state at a node and site, holds the probability of a node and site we want to see if is larger
  MostProbableState : AnsiString = '';  //Holds the 1 letter representation of the most probable state so far for a node and site, Holds the location of where to save exported file to users machine
  SaveLocation: String = '';
  StringListResults: TStringList = nil;  //Transports probability results, Holds the data to export to excel, csv, text, etc
  MostProbableGrid : Array of Array of AnsiString;
  PleaseWait : TPleaseWait = nil;
  ExportType : TExportType =  EXNone;   //How the user wants their file
  ExcelComponent : TExcelWrite = nil;
  ProbColor: TColor = clYellow;
  aRect : TRect;
  ExportOnlyCurNode: Boolean = False;
  InitialSite: Integer = -1;
  csvStrings: TStringList = nil;
begin
  DataOffsetX := 0;
  DataOffsetY := 0;
  node := 0;
  site := 0;
  if (Tree.MaxSiteIndex * Tree.NoOfNodes > 1073740000 { max # cells}) and (not ExportOnlyCurNode) and (not IsExcel2010OrGreater) then
  begin
    WriteOutputDlg.Disallow(EXexcelDisp);
    WriteOutputDlg.Disallow(EXexcelSave);
  end;
  SaveLocation := MegaForm.DataFileNameOnly + '-Most-probable-seqs';
  ExportType := PromptUserWriteOutput(SaveLocation);
  if (ExportType = EXnone) or (Trim(SaveLocation) = EmptyStr) then
    Exit;

  try
    try
      if Sender = Exportmostprobablesequenceforcurrentlyselectednode1 then
        ExportOnlyCurNode := True
      else
        ExportOnlyCurNode := False;

      PleaseWait := TPleaseWait.Create(Self);
      PleaseWait.Action := 'Analyzing most probable Seqs...';
      PleaseWait.show;

      StringListResults := TStringList.Create;
      ExcelComponent := TExcelWrite.Create(Self, 'Most Probable Seqs');
      ExcelComponent.IsXLS := True;
      //Write the legend for the first tab (yellow < 90% likely, red < 50% likely, white > 90% likely)
      ExcelComponent.Add('Sites with Maximum Probability < 0.9 for any state are shown in yellow', RGB(255,255,0));
      ExcelComponent.Add('Site', RGB(160,230,226));
      ExcelComponent.WriteLine();
      aRect := ExcelComponent.LastCellWriteXY();
      aRect.Left := aRect.Left +1;
      aRect.Right := aRect.Right + 81;
      ExcelComponent.MergeCells(aRect, aCenter);
      ExcelComponent.Add('Sites with Maximum Probability < 0.5 for any state are shown in red', RGB(255,0,0));
      DataOffsetX := (ExcelComponent.LastCellWriteXY().Left - node);
      DataOffsetY := (ExcelComponent.LastCellWriteXY().Top - site);
      SetLength(MostProbableGrid, Tree.MaxSiteIndex+2);
      if ExportOnlyCurNode then
      begin
        for i:=0 to Tree.MaxSiteIndex+1 do
          SetLength(MostProbableGrid[i], 1);
      end
      else
      begin
        for i:=0 to Tree.MaxSiteIndex+1 do
          SetLength(MostProbableGrid[i], (Tree.NoOfNodes-1));
      end;

      for interleavednumber := 0 to Math.Ceil((Tree.MaxSiteIndex+1)/80)-1 do  //each loop is a group of 80 sites in interleaved format
      begin
        for i := 1+(81*interleavednumber) to min(81+(81*interleavednumber), Tree.MaxSiteIndex+1) do // adding site numbers for the current block
          ExcelComponent.Add(i, RGB(160,230,226), 16777215, 90);
        ExcelComponent.WriteLine();
        for node:=0 to Tree.NoOfNodes-2 do
        begin
          if Tree.OTUName[node+1] = EmptyStr then  // if its an internal node
            NodeName := IntToStr(node+1)+'. '+Tree.CoordsName[node+1]
          else
            NodeName := IntToStr(node+1)+'. '+Tree.OTUName[node+1];
          ExcelComponent.Add(NodeName); //Draw Node Name

          PleaseWait.PercentDone := Math.Ceil(((interleavednumber+1)*node) / (Tree.MaxSiteIndex-1/80*Tree.NoOfNodes-1));
          Application.ProcessMessages;
          InitialSite := ((interleavednumber*81)+1);
          For site := InitialSite to min(Tree.MaxSiteIndex+1, (((interleavednumber+1)*81))) do
          begin
            GetAncStateProbWYSIWYG(node, site, MLAnalyzer, MostProbableState, MaxProbability, ActionAncStateShowMost);
            if not (MaxProbability > 0) then
              MostProbableState := '-'
            else
            begin
              if MaxProbability < 0.50 then
                  ProbColor := RGB(255, 0, 0)
               else if MaxProbability < 0.90 then
                  ProbColor := RGB(255, 255, 0)
               else
                  ProbColor := RGB(255, 255, 255);
            end;
            MostProbableGrid[site-1][node] := MostProbableState; //Saves state for later comparison
            if Length(MostProbableState) > 0 then  // just a extra assurance that we don't crash if this happens.
              ExcelComponent.Add(MostProbableState[1], ProbColor)
            else
              ExcelComponent.Add('?',ProbColor);
          end;
            ExcelComponent.WriteLine();
          end;
          ExcelComponent.Add(' ');
          ExcelComponent.WriteLine();
          ExcelComponent.AddBlankCell;
        end;

        if (ExportType <> EXtext) and (ExportType <> EXtextSave) then
        begin
          //Draw the black line for the first tab to seperate site # and data
          aRect.Top := DataOffsetY+1;
          aRect.Bottom := DataOffsetY + ((Math.Ceil((Tree.MaxSiteIndex+1)/80)+1)*(Tree.NoOfNodes+1));
          aRect.Left := 0;
          aRect.Right := aRect.Left;
          ExcelComponent.ColorCells(aRect, RGB(0,0,0), xlBorderRight);

          //Draw black line for first tab to sepearate node # and data
          aRect.Top := DataOffsetY;
          aRect.Bottom := aRect.Top;
          aRect.Left := DataOffsetX;
          aRect.Right := 80 + DataOffsetX;
          ExcelComponent.ColorCells(aRect, RGB(0,0,0), xlBorderBottom);
        end;

      if not ExportIsTextEditorDisplay(ExportType) then
        ExcelComponent.SaveFile(SaveLocation, ExcelExportToFiletype(ExportType));

      If ExportIsWorkbookDisplay(ExportType) then
        RunAProgram(SaveLocation)
      else if ExportIsTextEditorDisplay(ExportType) then
      begin
        for i := 0 to Tree.NoOfNodes-1  do
          MaxNameLength := Max(MaxNameLength, length(Tree.OTUName[i]));
        csvStrings := ExcelComponent.GetCsvList;
        for i := 0 to csvStrings.Count-1 do
        begin
          name := PadStrToWidth(Copy(csvStrings[i], 0, pos(',', csvStrings[i])-1) + ':', MaxNameLength+3);
          sequence := StringReplace(Copy(csvStrings[i], pos(',', csvStrings[i]), 9999), ',', '', [rfReplaceAll]);
          if Sequence <> EmptyStr then
            csvStrings[i] := name + sequence
          else
            csvStrings[i]  := EmptyStr;
        end;
        if ExportType = EXtext then
          OpenStringList(csvStrings, 'Most Probable Sequences', true)
        else
          csvStrings.SaveToFile(SaveLocation);
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! An error occurred: ' + E.Message);
    end;
  finally
    if Assigned(StringListResults) then
      FreeAndNil(StringListResults);
    if Assigned(PleaseWait) then
      FreeAndNil(PleaseWait);
    if Assigned(ExcelComponent) then
      FreeAndNil(ExcelComponent);
    if Assigned(csvStrings) then
      csvStrings.Free;
  end;
end;

procedure TTreeViewForm.ActionActStateExportCurrentSiteExecute(Sender: TObject);
var
  RowColor: TColor;
  TempStr: AnsiString;
  des2: integer = -1;
  des1: integer = -1;
  NodeName: AnsiString;
  i: integer;
  node:integer;
  SaveLocation : String;
  DataStringGrid : TStringList = nil;
  TextOut: TStringList = nil;
  MatrixExport : TMatrixExport = nil;
  PleaseWait : TPleaseWait = nil;
  ExportType : TExportType;
  ExcelComponent : TExcelWrite = nil;
  Probability : double;
  aRect : TRect;
  AncStateProbabilities : Array of TAncStateRec;
  IsML: Boolean;
  CurStr: AnsiString;
  MaxLengthNodeNo: Integer = -1;
  LongestTreeOtu: Integer = -1;
begin
  IsML := (MLAnalyzer <> nil);  // Lets us know whether we have a ML or MP tree here.
  SaveLocation := MegaForm.DataFileNameOnly + '-site(' + IntToStr(Tree.SiteIndex+1) + ')-ancestral-states';
  ExportType := PromptUserWriteOutput(SaveLocation); //Popup user dlg to ask them how they want their data formated and where to save it
  if ExportType = EXnone then
    Exit;

  try
    try
      PleaseWait := TPleaseWait.Create(Self);
      if IsML then
        PleaseWait.Action := 'Formatting state likelihoods for site ' + IntToStr(Tree.SiteIndex+1)
      else
        PleaseWait.Action := 'Formatting states at site ' + IntToStr(Tree.SiteIndex+1);
      PleaseWait.show;

      if IsML then
      begin
        if MAI.isAminoAcid then
          SetLength(AncStateProbabilities, 20)
        else
          SetLength(AncStateProbabilities, 4);
      end;

      if not ExportIsTextEditorDisplay(ExportType) then
      begin
        ExcelComponent := TExcelWrite.Create(self);
        ExcelComponent.IsXLS := True;
        ExcelComponent.Add('Inferred Ancestral Sequences at Site ' + IntToStr(Tree.SiteIndex+1));
        ExcelComponent.WriteLine();
        aRect := ExcelComponent.LastCellWriteXY();
        aRect.Right := aRect.Right + 2 + Length(AncStateProbabilities);
        ExcelComponent.MergeCells(aRect, aCenter);
        ExcelComponent.BoldCells(aRect);
        ExcelComponent.Add('Parent Node');
        ExcelComponent.Add('Descendent 1');
        ExcelComponent.Add('Descendent 2');
        if isML then
          ExcelComponent.Add('Probability')
        else
          ExcelComponent.Add('State');
        ExcelComponent.WriteLine();
        aRect := ExcelComponent.LastCellWriteXY();
        ExcelComponent.AddBlankCell;
        ExcelComponent.AddBlankCell;
        ExcelComponent.AddBlankCell;

        if IsML then
        begin
          MLAnalyzer.GetAncStateProbAlphabetical(0, 1, AncStateProbabilities);
          for i :=0 to Length(AncStateProbabilities)-1 do
          begin
            TempStr := AncStateProbabilities[i].Name;
            if (not MAI.IsAminoAcid) AND (TempStr = 'T') then
              TempStr := 'T/U';
            ExcelComponent.Add(TempStr);
            aRect := ExcelComponent.LastCellWriteXY();
            aRect.Left := aRect.Left -1;
            aRect.Right := aRect.Right -1;
            ExcelComponent.AlignCells(aRect, aRight);
          end;
        end;

        ExcelComponent.WriteLine();
        aRect.Left := 0;
        aRect.Right := aRect.Left;
        aRect.Top := aRect.Top -1;
        ExcelComponent.AlignCells(aRect, aCenter, aTop);
        aRect.Left := 1;
        aRect.Right := aRect.Left;
        ExcelComponent.AlignCells(aRect, aCenter, aTop);
        aRect.Left := 2;
        aRect.Right := aRect.Left;
        ExcelComponent.AlignCells(aRect, aCenter, aTop);
        aRect.Left := 3;

        aRect.Right := aRect.Left+ Length(AncStateProbabilities)-1;
        aRect.Bottom := aRect.Top;
        ExcelComponent.MergeCells(aRect, aCenter, aTop);
        aRect.Bottom := aRect.Bottom + 1;
        aRect.Top := aRect.Bottom;
        aRect.Left := 0;
        aRect.Right := 6;
        ExcelComponent.ColorCells(aRect, RGB(0,0,0), xlBorderBottom);
      end
      else
      begin
        TextOut := TStringList.Create;
        TextOut.Add('Inferred Ancestral Sequences at Site ' + IntToStr(Tree.SiteIndex+1));
        MaxLengthNodeNo := max(Length(IntToStr(Tree.NoOfNodes))+1, Length('Node No.')+1);
        for i:=0 to Tree.NoOfNodes-1 do
          LongestTreeOtu := max(max(LongestTreeOtu, Length(Tree.OTUName[i])), Length(Tree.CoordsName[i]));
        inc(LongestTreeOtu); // Extra space between the columns
        CurStr := PadStrToWidth('Node No.', MaxLengthNodeNo) + PadStrToWidth('OTU', LongestTreeOtu);
        if IsML then
        begin
          MLAnalyzer.GetAncStateProbAlphabetical(0, 1, AncStateProbabilities);
          for i :=0 to Length(AncStateProbabilities)-1 do
            CurStr := CurStr + PadStrToWidth(' ' + AncStateProbabilities[i].Name, 7);
        end
        else
          CurStr := CurStr + ' State';
        TextOut.Add(CurStr);
      end;

      for node:=Tree.NoOfOTUs to Tree.NoOfNodes-2 do     //check to make sure that nodes aren't different node number on different sites
      begin
        PleaseWait.PercentDone := Math.Ceil(100* ((node * 1.0) / (Tree.NoOfNodes-1)));
        Application.ProcessMessages;
        if not ExportIsTextEditorDisplay(ExportType) then
        begin
          if (Node mod 2) = 0 then
            RowColor := TColor($C3E5EB)
          else
            RowColor := RGB(255,255,255);
          ExcelComponent.Add(node+1, RowColor);
          aRect := ExcelComponent.LastCellWriteXY();
          aRect.Left := 0;
          aRect.Right := 0;
          ExcelComponent.MergeCells(aRect, aCenter);
          Tree.GetIntCoords(node+1, des1, des2);
          TempStr := Tree.OTUName[des1];
          if TempStr = EmptyStr then
            ExcelComponent.Add(des1, RowColor)
          else
            ExcelComponent.Add(TempStr, RowColor);
          aRect := ExcelComponent.LastCellWriteXY();
          aRect.Left := aRect.Left -1;
          aRect.Right := aRect.Left;
          ExcelComponent.AlignCells(aRect, aLeft);
          TempStr := Tree.OTUName[des2];
          if TempStr = EmptyStr then
            ExcelComponent.Add(des2, RowColor)
          else
            ExcelComponent.Add(TempStr, RowColor);
          aRect := ExcelComponent.LastCellWriteXY();
          aRect.Left := aRect.Left -1;
          aRect.Right := aRect.Left;
          ExcelComponent.AlignCells(aRect, aLeft);
          if Not IsML then
            ExcelComponent.add(OriTree.GetAncState(OriTree.AncStateProc, Node+1), RowColor)
          else
          begin
            MLAnalyzer.GetAncStateProbAlphabetical(node, (Tree.SiteIndex+1), AncStateProbabilities);
            for i :=0 to Length(AncStateProbabilities)-1 do
            begin
              Probability := RoundTo(AncStateProbabilities[i].Prob, -4);
              if Probability = 0 then //Don't show 0's
                ExcelComponent.AddBlankCell(RowColor)
              else
                ExcelComponent.Add(Probability, RowColor);
            end;
          end;
          ExcelComponent.WriteLine();
        end
        else  // Is Text
        begin
          CurStr := PadStrToWidth(IntToStr(Node+1), MaxLengthNodeNo);
          NodeName := Tree.OTUName[node+1];
          if NodeName = EmptyStr then
            NodeName := Tree.CoordsName[node+1];
          CurStr := CurStr + PadStrToWidth(NodeName, LongestTreeOtu);
           if Tree.OTUName[node+1] <> EmptyStr then
          begin
            TextOut.Add(CurStr);
            Continue;
          end;
          if Not IsML then
            CurStr := CurStr + OriTree.GetAncState(OriTree.AncStateProc, Node+1)
          else
          begin
              MLAnalyzer.GetAncStateProbAlphabetical(node, (Tree.SiteIndex+1), AncStateProbabilities);
            for i :=0 to Length(AncStateProbabilities)-1 do
            begin
              Probability := RoundTo(AncStateProbabilities[i].Prob, -2);
              if Probability < 0.01 then
                CurStr := CurStr + PadStrToWidth(EmptyStr, 7)
              else if Probability < 0.50 then
                CurStr := CurStr + PadStrToWidth(' (' + FloatToStr(Probability) + ')', 7)
              else if Probability < 0.90 then
                CurStr := CurStr + PadStrToWidth(' [' + FloatToStr(Probability) + ']', 7)
              else
                CurStr := CurStr + PadStrToWidth(' ' + FloatToStr(Probability), 7)
            end;
          end;
          TextOut.Add(CurStr);
        end;
      end;

      if not ExportIsTextEditorDisplay(ExportType) then
        ExcelComponent.SaveFile(SaveLocation, ExcelExportToFileType(ExportType));
      If ExportIsWorkbookDisplay(ExportType) then
        RunAProgram(SaveLocation)
      else if ExportIsTextEditorDisplay(ExportType) then
      begin
        if ExportType = EXtext then
          OpenStringList(TextOut, 'site(' + IntToStr(Tree.SiteIndex + 1) + ') ancestral states', true)
        else
          TextOut.SaveToFile(SaveLocation);
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(DataStringGrid) then
      FreeAndNil(DataStringGrid);
    if Assigned(MatrixExport) then
      FreeAndNil(MatrixExport);
    if Assigned(PleaseWait) then
      FreeAndNil(PleaseWait);
    if Assigned(ExcelComponent) then
      FreeAndNil(ExcelComponent);
    if Assigned(TextOut) then
      FreeAndNil(TextOut);
  end;
end;

procedure TTreeViewForm.ActionActStateExportChangesListExecute(Sender: TObject);
var
  ExportHeader: TAncStatesExportHeader = nil;
  Exporter: TAncestralStatesExporter = nil;
  ExportType : TExportType;
  SaveLocation: String;
  aTree: TTreeData = nil;
begin
  SaveLocation := MegaForm.DataFileNameOnly + '-changes-list';
  ExportType := PromptUserWriteOutput(SaveLocation); //Popup user dlg to ask them how they want their data formated and where to save it
  if ExportType = EXnone then  //If the user canceled then we stop here
    Exit;

  try
    try
      if Assigned(MLAnalyzer) then
        Exporter := TMLAncestralStatesExporter.Create(MLAnalyzer, OriTree, CurAncStateCaption)
      else
      begin
        ExportHeader := OriTree.GetAncStatesExportHeader;
        aTree := TTreeData.Create(OriTree.NoOfOTUs, OriTree.isBranchLength, OriTree.isSE, OriTree.isStats);
        GetTreeAdapterData(aTree);
        OriTree.GetTreeData(aTree);
        ComputeParsimInfo.SetTreeData(aTree);
        Exporter := TParsimonyAncestralStatesExporter.Create(ExportHeader, ComputeParsimInfo, MAI.MyOriTreeList, aTree, OriTree.NoOfOTUs, OriTree.NoOfNodes, MLAnalyzer, ExtCharAncItem.Checked);
      end;

       if not Exporter.ExportChangesList(ExportType, SaveLocation) then
         ShowMessage('Applicaton Error! The ancestral changes export may not have completed correctly');
    except
      on E:Exception do
        ShowMessage('Application Error: ' + E.Message);
    end;
  finally
    if Assigned(Exporter) then
      Exporter.Free;
  end;
end;

procedure TTreeViewForm.ActionActStateExportMostProbableSequenceCurrentNodeExecute(Sender: TObject);
var
  NoOfSites: integer;
  TempStr: AnsiString;
  j: integer;
  node: Integer = 1; //Node is the current node we are on,
  site: Integer = 1; //site is the current site we are on,
  line, i, DataOffsetX, DataOffsetY:integer;  // line is used as a temporary variable to loop throught a TStringList
  MaxProbability, TestingProbability : double;    //Hold the probability of the most probable state at a node and site, holds the probability of a node and site we want to see if is larger
  MostProbableState : AnsiString;  //Holds the 1 letter representation of the most probable state so far for a node and site, Holds the location of where to save exported file to users machine
  SaveLocation: String;
  StringListResults: TStringList = nil;
  HorizontalMostProbableSeqs: TStringList = nil;  //Transports probability results, Holds the data to export to excel, csv, text, etc
  MatrixExport : TMatrixExport = nil;  //Does the formating, exporting and opening of the file
  MostProbableGrid : Array of Array of AnsiString;
  PleaseWait : TPleaseWait = nil;
  ExportType : TExportType;   //How the user wants their file
  ExcelComponent : TExcelWrite = nil;
  ProbColor, ChangedColor : TColor;
  aRect : TRect;
  ExportOnlyCurNode: Boolean;
  CurNode, LoopFrom, LoopTo: Integer;
begin
  CurNode := CurOriTreeIndex; // Get the current node's index so we know which to export.
  ExportOnlyCurNode := True;
  ChangedColor := RGB(33, 255, 33);  //Color of borders for cells different than infered parents

  if (Tree.MaxSiteIndex * Tree.NoOfNodes > 1073740000 { max # cells}) and (not ExportOnlyCurNode) and (not IsExcel2010OrGreater) then
  begin
    WriteOutputDlg.Disallow(EXexcelDisp);
    WriteOutputDlg.Disallow(EXexcelSave);
  end;
  SaveLocation := MegaForm.DataFileNameOnly + 'node(' + IntToStr(Tree.FocusedIndex) + ')-Most-probable-seqs';
  ExportType := PromptUserWriteOutput(SaveLocation); //Popup user dlg to ask them how they want their data formated and where to save it
  if ExportType = EXnone then  //If the user canceled then we stop here
    Exit;

  try
    try
      PleaseWait := TPleaseWait.Create(Self);
      PleaseWait.Action := 'Analyzing most probable Seqs...';
      PleaseWait.show;
      HorizontalMostProbableSeqs := TStringList.Create;
      StringListResults := TStringList.Create;
      ExcelComponent := TExcelWrite.Create(Self, 'Vertical');
      ExcelComponent.IsXLS := True;
      ExcelComponent.AddWorksheet('Horizontal');

      //Write the legend for the first tab (yellow < 90% likely, red < 50% likely, white > 90% likely)
      ExcelComponent.Add('P < 90%', RGB(255,255,0));
      ExcelComponent.WriteLine();
      ExcelComponent.Add('P < 50%', RGB(255,0,0));
      ExcelComponent.WriteLine();
      ExcelComponent.Add('Different than Infered Parent');
      ExcelComponent.WriteLine();
      aRect := ExcelComponent.LastCellWriteXY();
      aRect.Right := aRect.Right +8;
      ExcelComponent.MergeCells(aRect);
      ExcelComponent.ColorCells(aRect, ChangedColor);
      DataOffsetX := (ExcelComponent.LastCellWriteXY().Left - node+1);
      DataOffsetY := (ExcelComponent.LastCellWriteXY().Top - site+1);

      if ExportOnlyCurNode then
      begin
        LoopFrom := CurNode;
        LoopTo := CurNode;
      end
      else
      begin
        LoopFrom := 0;
        LoopTo := Tree.NoOfNodes-2;
      end;

      for node:=LoopFrom to LoopTo do
      begin
        ExcelComponent.Add(node+1);
        HorizontalMostProbableSeqs.Add('')
      end;
      ExcelComponent.WriteLine();
      SetLength(MostProbableGrid, Tree.MaxSiteIndex);
      if ExportOnlyCurNode then
      begin
        for i:=0 to Tree.MaxSiteIndex-1 do
          SetLength(MostProbableGrid[i], 1);
      end
      else
      begin
        for i:=0 to Tree.MaxSiteIndex-1 do
          SetLength(MostProbableGrid[i], (Tree.NoOfNodes-1));
      end;

      for site:=1 to Tree.MaxSiteIndex-1 do
      begin
        if (site mod 1000) = 0 then
        begin
          PleaseWait.PercentDone := Math.Ceil(100* ((site * 1.0) / (Tree.MaxSiteIndex-1)));  //the % we have done
          Application.ProcessMessages;
        end;
        ExcelComponent.Add(site);
        if ExportOnlyCurNode then
        begin
          LoopFrom := CurNode;
          LoopTo := CurNode;
        end
        else
        begin
          LoopFrom := 0;
          LoopTo := Tree.NoOfNodes-2;
        end;
        For node:=LoopFrom to LoopTo do
        begin
          MaxProbability := 0;  //since we're at a new site, reset probability
          StringListResults.Clear;  //Also reset the object that obtains probability results
          MLAnalyzer.GetAncStateProb(node, site, StringListResults);

          for line :=0 to StringListResults.Count-1 do  //For each probability at this node/site
          begin
            TestingProbability := StrToFloat(Copy(StringListResults.Strings[line], 3, 20)); //Extract the probability as float
            if (TestingProbability >= MaxProbability) then
            begin
              MaxProbability := TestingProbability; //Found a new highest probability
              MostProbableState := Copy(StringListResults.Strings[line], 0, 1);  //Extract state (i.e. A, T, C, G)
            end;
          end;
          MostProbableGrid[site-1][node] := MostProbableState; //Saves state for later comparison
          if MaxProbability < 0.50 then
            ProbColor := RGB(255, 0, 0)
          else if MaxProbability < 0.90 then
            ProbColor := RGB(255, 255, 0)
          else
            ProbColor := RGB(255, 255, 255);

          if ((MLAnalyzer.MLTree.Node[node].des1 <> nil) and (MostProbableGrid[site-1][MLAnalyzer.MLTree.Node[node].des1.index] <> EmptyStr)) then
          begin
            if MostProbableGrid[site-1][MLAnalyzer.MLTree.Node[node].des1.index] <> MostProbableState then
            begin
              aRect.Top := ExcelComponent.LastCellWriteXY().Top ;
              aRect.Bottom := aRect.Top;
              aRect.Left := MLAnalyzer.MLTree.Node[node].des1.index + DataOffsetX;
              aRect.Right := aRect.Left;
              ExcelComponent.ColorCells(aRect, ChangedColor);
            end;
          end;
          if ((MLAnalyzer.MLTree.Node[node].des2 <> nil) and (MostProbableGrid[site-1][MLAnalyzer.MLTree.Node[node].des2.index] <> EmptyStr)) then
          begin
            if MostProbableGrid[site-1][MLAnalyzer.MLTree.Node[node].des2.index] <> MostProbableState then
            begin
              aRect.Top := ExcelComponent.LastCellWriteXY().Top ;
              aRect.Bottom := aRect.Top;
              aRect.Left := MLAnalyzer.MLTree.Node[node].des2.index + DataOffsetX;
              aRect.Right := aRect.Left;
              ExcelComponent.ColorCells(aRect, ChangedColor);
            end;
          end;
          ExcelComponent.Add(MostProbableState,ProbColor);
        end;
        HorizontalMostProbableSeqs.Strings[node] := HorizontalMostProbableSeqs.Strings[node] + MostProbableState;
      end;
      ExcelComponent.WriteLine();
      ExcelComponent.Add('Node No.');
      ExcelComponent.Add('Most Probable Sequence');
      ExcelComponent.WriteLine(1); //Write it to worksheet1 (the second tab)
      //Write out the most probable seqs horizontally
      if ExportOnlyCurNode then
      begin
        LoopFrom := CurNode;
        LoopTo := CurNode;
      end
      else
      begin
        LoopFrom := 0;
        LoopTo := Tree.NoOfNodes-2;
      end;
      for node :=LoopFrom to LoopTo do
      begin
        ExcelComponent.Add(node+1);  //Node number
        ExcelComponent.Add(HorizontalMostProbableSeqs.Strings[node]); //Most probable sequence
        ExcelComponent.WriteLine(1); //Write it to worksheet 1 (the second tab)
      end;

      //Draw the black line for the first tab to seperate site # and data
      aRect.Top := DataOffsetY+1;
      aRect.Bottom := DataOffsetY + Tree.MaxSiteIndex-1;
      aRect.Left := 0;
      aRect.Right := aRect.Left;
      ExcelComponent.ColorCells(aRect, RGB(0,0,0), xlBorderRight);

      //Draw black line for first tab to sepearate node # and data
      aRect.Top := DataOffsetY;
      aRect.Bottom := aRect.Top;
      aRect.Left := DataOffsetX;
      aRect.Right := Tree.NoOfNodes-2 + DataOffsetX;
      ExcelComponent.ColorCells(aRect, RGB(0,0,0), xlBorderBottom);


      if not ExportIsTextEditorDisplay(ExportType) then
        ExcelComponent.SaveFile(SaveLocation, ExcelExportToFiletype(ExportType));

      If ExportIsWorkbookDisplay(ExportType) then
        RunAProgram(SaveLocation)
      else if (ExportType = EXcsvSave) then
      begin
        raise Exception.Create('not implemented. CSV export requires a separate file for each worksheet');
        NoOfSites := Length(HorizontalMostProbableSeqs.Strings[0]);
        if ExportOnlyCurNode then
        begin
          LoopFrom := CurNode;
          LoopTo := CurNode;
        end
        else
        begin
          LoopFrom := 0;
          LoopTo := Tree.NoOfNodes-2;
        end;

        for i := LoopFrom to LoopTo do
          begin
            TempStr := EmptyStr;
            for j := 1 to NoOfSites do
            begin
              TempStr := TempStr + ',' + HorizontalMostProbableSeqs.Strings[i][j] ;
            end;
            HorizontalMostProbableSeqs.Strings[i] := TempStr;
          end;
        for i := LoopFrom to LoopTo do
            HorizontalMostProbableSeqs.Strings[i] := StrToStrWidth('Node ' + IntToStr(i) + ':', Length(IntToStr(Tree.NoOfNodes))+7) + HorizontalMostProbableSeqs.Strings[i];
          HorizontalMostProbableSeqs.SaveToFile(SaveLocation);
      end
      else if ExportIsTextEditorDisplay(ExportType) then
      begin
        raise Exception.Create('not implemented. Text export requires a separate file for each worksheet');
        for i := LoopFrom to LoopTo do
          HorizontalMostProbableSeqs.Strings[i] := StrToStrWidth('Node ' + IntToStr(i) + ':', Length(IntToStr(Tree.NoOfNodes))+7) + HorizontalMostProbableSeqs.Strings[i];
        if ExportType = EXtext then
          OpenStringList(HorizontalMostProbableSeqs, 'Most Probable Sequences', true)
        else
          HorizontalMostProbableSeqs.SaveToFile(SaveLocation);
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(StringListResults) then
      StringListResults.Free;
    if Assigned(MatrixExport) then
      MatrixExport.Free;
    if Assigned(PleaseWait) then
      PleaseWait.Free;
    if Assigned(ExcelComponent) then
      ExcelComponent.Free;
  end;
end;

procedure TTreeViewForm.ActionAncStateExportAllSitesExecute(Sender: TObject);
var
  ExportHeader: TAncStatesExportHeader = nil;
  Exporter: TParsimonyAncestralStatesExporter = nil;
  ExportType: TExportType;
  SaveLocation: String;
  aTree: TTreeData = nil;
begin
  SaveLocation := MegaForm.DataFileNameOnly + '_ancestral_states';
  ExportType := PromptUserWriteOutput(SaveLocation);
  if ExportType = EXnone then
    Exit;

  try
    try
      ExportHeader := OriTree.GetAncStatesExportHeader;
      aTree := TTreeData.Create(OriTree.NoOfOTUs, OriTree.isBranchLength, OriTree.isSE, OriTree.isStats);
      OriTree.GetTreeData(aTree);
      ComputeParsimInfo.SetTreeData(aTree);
      Exporter := TParsimonyAncestralStatesExporter.Create(ComputeParsimInfo, ExportHeader, OriTree.NoOfOTUs, OriTree.NoOfNodes, ExtCharAncItem.Checked);
       if not Exporter.ExportStates(ExportType, SaveLocation) then
         ShowMessage('Oh no! The ancestral states export may not have completed correctly');
    except
      on E:Exception do
        ShowMessage('Oh no! An error occurred when exporting ancestral states: ' + E.Message);
    end;
  finally
    if Assigned(ExportHeader) then
      ExportHeader.Free;
    if Assigned(Exporter) then
      Exporter.Free;
  end;
end;

procedure TTreeViewForm.ActionAncStateExportPredictLivingSequenceExecute( Sender: TObject);
var
  RowColor: TColor;
  TempStr: AnsiString;
  i: integer;
  site:integer;
  SaveLocation : String;
  DataStringGrid : TStringList = nil;
  TextOut: TStringList = nil;
  MatrixExport : TMatrixExport = nil;
  PleaseWait : TPleaseWait = nil;
  ExportType : TExportType;
  ExcelComponent : TExcelWrite = nil;
  Probability : double;
  aRect : TRect;
  AncStateProbabilities : Array of TAncStateRec;
  ExpectedStateProb: TStringList = nil;
  IsML, IsText: Boolean;
  selNodeIndex: Integer;
  selNodeName: AnsiString;
begin
  SaveLocation := MegaForm.DataFileNameOnly + '-living-seqs';
  ExportType := PromptUserWriteOutput(SaveLocation); //Popup user dlg to ask them how they want their data formated and where to save it
  if ExportType = EXnone then
    Exit;

  try
    try
      SelNodeIndex := Tree.FocusedIndex;
      if (SelNodeIndex < 1) or (SelNodeIndex > Tree.NoOfOTUs) then
        SelNodeIndex := 1;
      SelNodeName := Tree.OTUName[SelNodeIndex];
      IsML := MLAnalyzer <> nil;  // Lets us know whether we have a ML or MP tree here.

      PleaseWait := TPleaseWait.Create(Self);
      if IsML then
        PleaseWait.Action := 'Formating state likelihoods for each site ' + IntToStr(Tree.SiteIndex+1)
      else
        PleaseWait.Action := 'Formating states at each site ' + IntToStr(Tree.SiteIndex+1);
      PleaseWait.show;

      if IsML then
      begin
        if MAI.isAminoAcid then
          SetLength(AncStateProbabilities, 20)
        else
          SetLength(AncStateProbabilities, 4);
      end;

      IsText := (ExportType = ExText) or (ExportType = EXtextSave);

      if IsText then
      begin
        ShowMessage('Text format not supported, try Excel or CSV');
        Exit;
      end;

      if not IsText then
      begin
        ExcelComponent := TExcelWrite.Create(self);
        ExcelComponent.IsXLS := True;
        ExcelComponent.Add('Predicted Living sequence for ' + SelNodeName);
        ExcelComponent.WriteLine();
        aRect := ExcelComponent.LastCellWriteXY();
        aRect.Right := aRect.Right + Length(AncStateProbabilities);
        ExcelComponent.MergeCells(aRect, aCenter);
        ExcelComponent.BoldCells(aRect);
        ExcelComponent.Add('Site #');
        //ExcelComponent.Add('Name');
        if isML then
          ExcelComponent.Add('Probability')
        else
          ExcelComponent.Add('State');
        ExcelComponent.WriteLine();
        aRect := ExcelComponent.LastCellWriteXY();
        ExcelComponent.AddBlankCell;

        if IsML then
        begin
          MLAnalyzer.GetAncStateProbAlphabetical(0, 1, AncStateProbabilities);
          for i :=0 to Length(AncStateProbabilities)-1 do
          begin
            TempStr := AncStateProbabilities[i].Name;
            if (not MAI.IsAminoAcid) AND (TempStr = 'T') then
              TempStr := 'T/U';
            ExcelComponent.Add(TempStr);
            aRect := ExcelComponent.LastCellWriteXY();
            aRect.Left := aRect.Left -1;
            aRect.Right := aRect.Right -1;
            ExcelComponent.AlignCells(aRect, aRight);
          end;
        end;

        ExcelComponent.WriteLine();
        aRect.Left := 0;
        aRect.Right := aRect.Left;
        aRect.Top := aRect.Top -1;
        ExcelComponent.AlignCells(aRect, aCenter, aTop);
        aRect.Left := 1;
        aRect.Right := aRect.Left;
        ExcelComponent.AlignCells(aRect, aCenter, aTop);
        aRect.Left := 2;
        aRect.Right := aRect.Left;
        ExcelComponent.AlignCells(aRect, aCenter, aTop);
        aRect.Left := 2;

        aRect.Right := aRect.Left+ Length(AncStateProbabilities)-1;
        aRect.Bottom := aRect.Top;
        ExcelComponent.MergeCells(aRect, aCenter, aTop);
        aRect.Bottom := aRect.Bottom + 1;
        aRect.Top := aRect.Bottom;
        aRect.Left := 0;
        aRect.Right := 6;
        ExcelComponent.ColorCells(aRect, RGB(0,0,0), xlBorderBottom);
      end;
      for site:=1 to Tree.MaxSiteIndex+1 do     //check to make sure that nodes aren't different node number on different sites
      begin
        if not IsText then
        begin
          if (site mod 2) = 0 then
            RowColor := TColor($C3E5EB)
          else
            RowColor := RGB(255,255,255);
          ExcelComponent.Add(site, RowColor);
          aRect := ExcelComponent.LastCellWriteXY();
          aRect.Left := 0;
          aRect.Right := 0;
          ExcelComponent.MergeCells(aRect, aCenter);
          //ExcelComponent.Add(SelNodeName, RowColor);
          aRect := ExcelComponent.LastCellWriteXY();
          aRect.Left := aRect.Left -1;
          aRect.Right := aRect.Left;
          ExcelComponent.AlignCells(aRect, aLeft);
          aRect := ExcelComponent.LastCellWriteXY();
          aRect.Left := aRect.Left -1;
          aRect.Right := aRect.Left;
          ExcelComponent.AlignCells(aRect, aLeft);
          if Not IsML then
            ExcelComponent.add(OriTree.GetAncState(OriTree.AncStateProc, Site), RowColor)
          else
          begin
            try
              ExpectedStateProb := TStringList.Create;
              MLAnalyzer.GetExpectedStateProbAlphabetical(SelNodeIndex-1, site, ExpectedStateProb);
              for i :=0 to Length(AncStateProbabilities)-1 do
              begin
                Probability := RoundTo(StrToFloat(ExpectedStateProb.Values[ExpectedStateProb.Names[i]]), -4);
                if Probability = 0 then //Don't show 0's
                  ExcelComponent.Add('', RowColor)
                else
                  ExcelComponent.Add(Probability, RowColor);
              end;
            finally
              FreeAndNil(ExpectedStateProb);
            end;
          end;
          ExcelComponent.WriteLine();
        end;
      end;
      if not IsText then
        ExcelComponent.SaveFile(SaveLocation, ExcelExportToFileType(ExportType));
      If ExportIsWorkbookDisplay(ExportType) then
        RunAProgram(SaveLocation)
      else if (ExportType = EXtext) then
        OpenStringList(TextOut, 'Most Probable Sequences', true)
      else if (ExportType = EXtextSave) then
        TextOut.SaveToFile(SaveLocation);
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(DataStringGrid) then
      DataStringGrid.Free;
    if Assigned(MatrixExport) then
      MatrixExport.Free;
    if Assigned(PleaseWait) then
      PleaseWait.Free;
    if Assigned(ExcelComponent) then
      ExcelComponent.Free;
    if Assigned(TextOut) then
      TextOut.Free;
  end;
end;

procedure TTreeViewForm.ActionAncStateExportTextExportExecute(Sender: TObject);
const
  VIEW_RESULT = 1001;
  SAVE_FILE = 1002;
var
  i: integer;
  node:integer;
  DataStringGrid : TStringList = nil;
  TextOut: TStringList = nil;
  PleaseWait : TPleaseWait = nil;
  Probability : double;
  AncStateProbabilities : Array of TAncStateRec;
  IsML: Boolean;
  CurStr: AnsiString;
  LongestTreeOtu: Integer;
  curSite : Integer;
  response: Integer;
begin
  IsML := MLAnalyzer <> nil;  // Lets us know whether we have a ML or MP tree here.
  try
    try
      PleaseWait := TPleaseWait.Create(Self);
      PleaseWait.Action := 'Formatting state likelihoods';
      PleaseWait.show;

      if IsML then
      begin
        if MAI.isAminoAcid then
          SetLength(AncStateProbabilities, 20)
        else
          SetLength(AncStateProbabilities, 4);
      end;

      TextOut := TStringList.Create;
      LongestTreeOtu := Length(Tree.OTUName[0]);
      for i := 0 to Tree.NoOfNodes - 1 do
        LongestTreeOtu := max(max(LongestTreeOtu, Length(Tree.OTUName[i])), Length(Tree.CoordsName[i]));
      inc(LongestTreeOtu); // Extra space between the columns

      for node:=Tree.NoOfOTUs to Tree.NoOfNodes-2 do
      begin
        PleaseWait.PercentDone := Math.Ceil(100* ((node * 1.0) / (Tree.NoOfNodes-1)));
        Application.ProcessMessages;
        CurStr := '>Node ' + IntToStr(Node+1) + ' ';

        if IsML then
        begin
          MLAnalyzer.GetAncStateProbAlphabetical(0, 1, AncStateProbabilities);
          for i :=0 to Length(AncStateProbabilities)-1 do
            CurStr := CurStr + AncStateProbabilities[i].Name;
        end;

        TextOut.Add(CurStr);
        CurStr := EmptyStr;

        for curSite := 0 to Tree.MaxSiteIndex do
        begin
          CurStr := IntToStr(curSite+1);
          MLAnalyzer.GetAncStateProbAlphabetical(node, (curSite+1), AncStateProbabilities);
          for i :=0 to Length(AncStateProbabilities)-1 do
          begin
            Probability := RoundTo(AncStateProbabilities[i].Prob, -2);
            CurStr := CurStr + #9 + Format('%.2f', [Probability]);
          end;
          TextOut.Add(CurStr);
          CurStr := EmptyStr;
        end;
      end;
      if Assigned(PleaseWait) then
        PleaseWait.Hide;
      response := QuestionDlg('Save or view file?', 'Would you like to view the results or save to a file?', mtCustom, [VIEW_RESULT, 'View Result', SAVE_FILE, 'Save to File'], 0);
      if response = VIEW_RESULT then
        OpenStringList(TextOut, 'Ancestral-states-details.txt', True)
      else
      begin
        FileSaveDialog.Filter := 'Text files (.txt)|*.txt|All files|*.*';
        FileSaveDialog.Filename := 'Ancestral-states-details.txt';
        if FileSaveDialog.Execute then
          TextOut.SaveToFile(FileSaveDialog.FileName)
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(DataStringGrid) then
      DataStringGrid.Free;
    if Assigned(PleaseWait) then
      PleaseWait.Free;
    if Assigned(TextOut) then
      TextOut.Free;
  end;
end;

procedure TTreeViewForm.ActionAncStateShowMostExecute(Sender: TObject);
begin
  if CurAncState = ActionAncStateShowMost then Exit;
  CurAncState := ActionAncStateShowMost;

  OriTree.AncStateProc := ShowMostProbAncStateProc;
  OriTree.ShowCharState := true;
  if IsBootTree and (MLAnalyzer = nil) then begin
    BootTree.AncStateProc := ShowMostProbAncStateProc;
    BootTree.ShowCharState := true;
  end;
  if IsReltimeTree then
  begin
    ReltimeTree.AncStateProc := ShowMostProbAncStateProc;
    ReltimeTree.ShowCharState := True;
    ReltimeTree.ShowDivergenceTimes := False;
    ReltimeTree.ShowHeightErrBar := False;
  end;
  Tree.Refresh;
  DataSaved := false;
  RenewAncState;
  try
    FCheckStateChanging := True;
    if IsReltimeTree then
      FDivTimesFrame.DivTimesDisplayCheckBx.Checked := False;
    UpdateAncestralStateActions;
    UpdateAncestorRadioBtns;
    UpdateCaptionForAncestralStates;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.ActionAncStateShowNoneExecute(Sender: TObject);
begin
  if CurAncState = ActionAncStateShowNone then Exit;
  CurAncState := ActionAncStateShowNone;
  OriTree.ShowCharState := false;
  if IsBootTree then
    BootTree.ShowCharState := false;
  if IsReltimeTree then
    ReltimeTree.ShowCharState := false;
  Tree.Refresh;
  DataSaved := false;
  RenewAncState;
  try
    FCheckStateChanging := True;
    UpdateAncestralStateActions;
    UpdateAncestorRadioBtns;
    UpdateCaptionForAncestralStates;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.ActionBranchInfoDisplayExecute(Sender: TObject);
begin
  Tree.ShowStats := not Tree.ShowStats;
  if Tree.ShowStats then
  begin
    Tree.ShowDataCoverage := False;
    Tree.ShowNodeIds := False;
  end;
  try
    FCheckStateChanging := True;
    UpdateStatsDisplay
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.ActionBranchInfoFontExecute(Sender: TObject);
begin
  FontDialog.Font.Assign(Tree.StatsFont);
  if FontDialog.Execute then
  begin
    OriTree.StatsFont := FontDialog.Font;
    if IsBootTree then
      BootTree.StatsFont := OriTree.StatsFont;
    if IsReltimeTree then
      ReltimeTree.StatsFont := OriTree.StatsFont;
    Tree.Invalidate;
    DataSaved := False;
    try
      FCheckStateChanging := True;
      FNodeStatsFrame.StatsFontSizeSpinEdit.Value := Tree.StatsFont.Size;
    finally
      FCheckStateChanging := False;
    end;
  end;
end;

procedure TTreeViewForm.ActionBranchLengthDisplayExecute(Sender: TObject);
begin
  Tree.ShowBLen := not Tree.ShowBLen;
  Tree.Invalidate;
  DataSaved := False;
  ActionBranchLengthDisplay.Checked := Tree.ShowBLen;
  FBLensFrame.BLensCheckBx.Checked := Tree.ShowBLen;
end;

procedure TTreeViewForm.ActionCircularExecute(Sender: TObject);
begin
    Tree.TreeStyle := tsCircle;
    PropagateTreeStyleChange;
    Tree.Refresh;
    FLayoutFrame.TreeStyleBtn.ImageIndex := CIRCULARTREESTYLE_IMAGE;
    TreeStyleBtn2.ImageIndex := CIRCULARTREESTYLE_IMAGE;
    TreeTabControl.Enabled := true;
    DataSaved := False;
end;

procedure TTreeViewForm.ActionRootOnBranchExecute(Sender: TObject);
begin
  if Tree.BranchFocused or Tree.NodeFocused then
  begin
    if FJustRootingATree then
      RootedIndex := Tree.FocusedIndex; { save it here because it gets reset in TTreeBox.MakeRootOnBranch}

    Tree.MakeRootOnBranch;
    Tree.GetInternalOutgroupNames(FInternalOutgroupTaxa);

    if ActionBalancedTree.Checked then
      Tree.SortClusterForShape
    else if ActionInputOrderTree.Checked then
      Tree.SortClusterInOrder;

    ActionRootOnMidpoint.Checked := False;
    if FJustRootingATree and (not IsGeneDupsMode) then
    begin
      Tree.FocusOnRoot;
      FOutgroupRoot := Tree.NodeInfo.Des2Index;
      SetOutgroupCluster(clRed ,FOutgroupRoot);
    end;
    Tree.Refresh;

    DataSaved := False;

    if CurTool = ActionPoint then
      ActionPointExecute(Sender);
    if ActionInfo.Checked then
      RenewBranchInfo;
  end
  else if Tree.Cursor <> CURSOR_ROOT then
  begin
    CurTool.Checked := false;
    CurTool := ActionRootOnBranch;
    CurTool.Checked := true;
    Tree.Cursor := CURSOR_ROOT;
  end
  else
    ActionPointExecute(nil);
  ResetStatusBar;
  if FJustRootingATree then
    if Tree.Height > ClientHeight then
      ScrollToBottom;
  if ActionInfo.Checked then
  begin
    RenewTreeInfo;
    RenewBranchInfo;
  end;
  Invalidate;
  Tree.Invalidate;
end;

procedure TTreeViewForm.ActionRootOnMidpointExecute(Sender: TObject);
begin
  if ActionRootOnMidpoint.Checked then
  begin
    ActionRootOnMidpoint.Checked := False;
    ChangeRootMenuItem.Checked := False;
  end
  else
  begin
    ActionRootOnMidpoint.Checked := True;
    ChangeRootMenuItem.Checked := True;
    Tree.MakeRootOnMidpoint;

    if ActionBalancedTree.Checked then
      Tree.SortClusterForShape
    else if ActionInputOrderTree.Checked then
      Tree.SortClusterInOrder;

    Tree.Refresh;
    DataSaved := False;
    if ActionInfo.Checked then
      RenewBranchInfo;
  end;
  ResetStatusBar;
  if ActionInfo.Checked then
  begin
    RenewTreeInfo;
    RenewBranchInfo;
  end;
end;

procedure TTreeViewForm.ActionSaveSessionExecute(Sender: TObject);
var
  aMsg: String = '';
  response: Integer = -1;
begin
  if Assigned(MLAnalyzer) and MLAnalyzer.IsLittleBootstrap then
  begin
    response := MessageDlg('Unable to Save Tree Session', 'Tree Session files are not supported for the Little Bootstraps analysis. Do you want to export the current tree to a Newick file instead?', mtInformation, mbYesNo, 0);
    if response = mrYes then
      ActionExportCurrentTreeExecute(Sender);
    Exit;
  end;

  FileSaveDialog.DefaultExt := 'mtsx';
  FileSaveDialog.Filter := TREE_SESSION_FILE_FILTER;
  if SessionFileName <> '' then
    FileSaveDialog.FileName := ChangeFileExt(SessionFileName, '.mtsx')
  else
    FileSaveDialog.FileName := ChangeFileExt(ExtractFileName(MegaForm.DataFileName), '.mtsx');
  if not AskForFileName then
    Exit;

  if not SaveSession(FileSaveDialog.FileName, aMsg) then
    ShowMessage('Failed to save tree session: ' + aMsg);
  SessionFileName := FileSaveDialog.FileName;
end;

procedure TTreeViewForm.ActionScaleBarFontExecute(Sender: TObject);
begin
  FontDialog.Font.Assign(Tree.ScaleFont);
  if FontDialog.Execute then
  begin
      OriTree.ScaleFont.Assign(FontDialog.Font);
      if IsBootTree then
        BootTree.ScaleFont.Assign(OriTree.ScaleFont);
      if IsRelTimeTree then
        ReltimeTree.ScaleFont.Assign(OriTree.ScaleFont);
    if Tree.TreeStyle = tsCircle then
      Tree.Refresh
    else
    begin
      Tree.SetTreeSize;
      Tree.Invalidate;
    end;
    DataSaved := False;
    try
      FCheckStateChanging := True;
      if Tree = ReltimeTree then
        FTimeScaleFrame.TimescaleFontSizeSpinEdit.Value := Tree.ScaleFont.Size
      else
        FDistScaleFrame.DistScaleFontSizeSpinEdit.Value := Tree.ScaleFont.Size;
    finally
      FCheckStateChanging := False;
    end;
  end;
end;

procedure TTreeViewForm.ActionSlantedExecute(Sender: TObject);
begin
    Tree.TreeStyle := tsTraditional;
    Tree.BranchStyle := bsStraight;
    PropagateTreeStyleChange;
    Tree.Refresh;
    FLayoutFrame.TreeStyleBtn.ImageIndex := SLANTEDTREESTYLE_IMAGE;
    TreeStyleBtn2.ImageIndex := SLANTEDTREESTYLE_IMAGE;
    TreeTabControl.Enabled := true;
    DataSaved := false;
end;

procedure TTreeViewForm.ActionSubtreeOptionExecute(Sender: TObject);
var
  NodeAttrib: TNodeAttrib = nil;
begin
  try
    NodeAttrib := TNodeAttrib.Create;
    if not Tree.GetSubtreeAttrib(NodeAttrib, Tree.FocusedIndex) then
      NodeAttrib.ClearImage;
    if NodeAttrib.NodeIndex <> Tree.FocusedIndex then
      NodeAttrib.Caption := '';
    SetupSubtreeOptionsDlg(NodeAttrib, ProcessSubtreeOptionsMessage, Tree.FocusedIndex);
  finally
    if Assigned(NodeAttrib) then
      NodeAttrib.Free;
  end;
end;

procedure TTreeViewForm.SetupSubtreeOptionsDlg(NodeAttrib: TNodeAttrib; onFinished: TProcessMessageReceivedProc; nodeID: Integer; isGroupOption: Boolean = False);
var
  js: String = '';
  Filename: String = '';
  TempDir: String = '';
begin
  TempDir := getTemp;

  try
    ShowOptionsDialog;
    HtmlOptionsDialog.ProcessMessageReceivedProc := onFinished;
    HtmlOptionsDialog.DomProcedureName := VISITDOMPROC_SUBTREE_DRAWING_OPTIONS;
    if isGroupOption then
      js := js + Format('$("#%s").removeAttr("disabled");', [SUBTREE_OPTIONS_DEFAULTS]) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [SUBTREE_OPTIONS_GROUP_NAME, NodeAttrib.Name]) + LineEnding;
    js := js + Format('$("#%s").val("%d");', [SUBTREE_OPTIONS_NODE_ID, nodeID]) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [SUBTREE_OPTIONS_CAPTION_FONT, CssStringForFont(NodeAttrib.CaptionFont)]) + LineEnding;
    js := js + JavaScriptFontPickerOptions(SUBTREE_OPTIONS_CAPTION_FONT) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [SUBTREE_OPTIONS_DISPLAY_FONT, CssStringForFont(NodeAttrib.Font)]) + LineEnding;
    js := js + JavaScriptFontPickerOptions(SUBTREE_OPTIONS_DISPLAY_FONT) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [SUBTREE_OPTIONS_NAME_CAPTION, NodeAttrib.Caption]) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [SUBTREE_OPTIONS_NODE_SHAPE, NodeMarkerShapeToHtmlString(NodeAttrib.Marker.Shape)]) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [SUBTREE_COLOR1, SColorToHtmlColor(NodeAttrib.Marker.Color)]) + LineEnding;
    js := js + Format('$("#%s").attr("ischecked", "%s");', [SUBTREE_OPTIONS_APPLY_TO_TAXON_MARKERS, BoolToStr(NodeAttrib.OverwriteMarker, True)]) + LineEnding;
    js := js + Format('$("#%s").prop("checked", %s);', [SUBTREE_OPTIONS_APPLY_TO_TAXON_MARKERS, BoolToStrLC(NodeAttrib.OverwriteMarker, True)]) + LineEnding;
    js := js + Format('$("#%s").val("%s").trigger("change");', [SUBTREE_OPTIONS_BRANCH_LINES, BranchOptionToStr(NodeAttrib.BranchOption)]) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [SUBTREE_COLOR2, SColorToHtmlColor(NodeAttrib.LineColor)]) + LineEnding;
    js := js + Format('$("#%s").val("%d").trigger("change");', [SUBTREE_OPTIONS_BRANCH_WIDTH, NodeAttrib.LineWidth]) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [SUBTREE_OPTIONS_BRANCH_STYLE, PenStyleToHtmlLineTypeStr(NodeAttrib.LineStyle)]) + LineEnding;
    js := js + Format('$("#%s").attr("ischecked", "%s");', [SUBTREE_OPTIONS_DISPLAY_CAPTION, BoolToStr(NodeAttrib.ShowCaption, True)]) + LineEnding;
    js := js + Format('$("#%s").prop("checked", %s);', [SUBTREE_OPTIONS_DISPLAY_CAPTION, BoolToStrLC(NodeAttrib.ShowCaption, True)]) + LineEnding;
    js := js + Format('$("#%s").attr("ischecked", "%s");', [SUBTREE_OPTIONS_ALIGN_VERT, BoolToStr(Tree.AlignCaption, True)]) + LineEnding;
    js := js + Format('$("#%s").prop("checked", %s);', [SUBTREE_OPTIONS_ALIGN_VERT, BoolToStrLC(Tree.AlignCaption, True)]) + LineEnding;
    js := js + Format('$("#%s").attr("ischecked", "%s");', [SUBTREE_OPTIONS_DISPLAY_BRACKET, BoolToStr(NodeAttrib.ShowBracket, True)]) + LineEnding;
    js := js + Format('$("#%s").prop("checked", %s);', [SUBTREE_OPTIONS_DISPLAY_BRACKET, BoolToStrLC(NodeAttrib.ShowBracket)]) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [SUBTREE_OPTIONS_BRACKET_STYLE, BracketStyleToString(NodeAttrib.BracketStyle)]) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [SUBTREE_COLOR3, SColorToHtmlColor(NodeAttrib.BracketColor)]) + LineEnding;
    js := js + Format('$("#%s").val("%d");', [SUBTREE_DRAWING_OPTIONS_LINE_WIDTH, NodeAttrib.BracketLineWidth]) + LineEnding;
    js := js + Format('$("#%s").attr("ischecked", "%s");', [SUBTREE_OPTIONS_DISPLAY_TAXON_NAME, BoolToStr(NodeAttrib.ShowTaxonName, True)]) + LineEnding;
    js := js + Format('$("#%s").prop("checked", %s);', [SUBTREE_OPTIONS_DISPLAY_TAXON_NAME, BoolToStrLC(NodeAttrib.ShowTaxonName, True)]) + LineEnding;
    js := js + Format('$("#%s").attr("ischecked", "%s");', [SUBTREE_OPTIONS_DISPLAY_NODE, BoolToStr(NodeAttrib.ShowNodeMarker, True)]) + LineEnding;
    js := js + Format('$("#%s").prop("checked", %s);', [SUBTREE_OPTIONS_DISPLAY_NODE, BoolToStrLC(NodeAttrib.ShowNodeMarker, True)]) + LineEnding;
    js := js + Format('$("#%s").attr("ischecked", "%s");', [SUBTREE_OPTIONS_DISPLAY_TAXON_MARKER, BoolToStr(NodeAttrib.ShowTaxonMarker, True)]) + LineEnding;
    js := js + Format('$("#%s").prop("checked", %s);', [SUBTREE_OPTIONS_DISPLAY_TAXON_MARKER, BoolToStrLC(NodeAttrib.ShowTaxonMarker, True)]) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [SUBTREE_COLOR4, SColorToHtmlColor(NodeAttrib.Font.Color)]) + LineEnding;
    if NodeAttrib.ManualCompressed then
    begin
      js := js + Format('$("#%s").attr("ischecked", "%s");', [SUBTREE_OPTIONS_COMPRESS, BoolToStr(NodeAttrib.ManualCompressed, True)]) + LineEnding;
      js := js + Format('$("#%s").prop("checked", %s);', [SUBTREE_OPTIONS_COMPRESS, BoolToStrLC(NodeAttrib.ManualCompressed, True)]) + LineEnding;
      js := js + Format('$("#%s").prop("disabled", false);', [SUBTREE_OPTIONS_FILL_PATTERN]) + LineEnding;
      js := js + Format('$("#%s").spinner("enable");', [SUBTREE_DRAWING_OPTIONS_VERTICAL_UNIT]) + LineEnding;
    end;
    js := js + Format('$("#%s").spinner("value", %d);', [SUBTREE_DRAWING_OPTIONS_VERTICAL_UNIT, Tree.PixelsPerGroupMember]) + LineEnding;
    js := js + Format('$("#%s").val("%s").trigger("change");', [SUBTREE_OPTIONS_FILL_PATTERN, BrushStyleToString(NodeAttrib.FillStyle)]) + LineEnding;
    js := js + Format('$("#%s").attr("ischecked", "%s");', [SUBTREE_OPTIONS_DISPLAY_IMAGE, BoolToStr(NodeAttrib.ShowImage, True)]) + LineEnding;
    js := js + Format('$("#%s").prop("checked", %s);', [SUBTREE_OPTIONS_DISPLAY_IMAGE, BoolToStrLC(NodeAttrib.ShowImage and Assigned(NodeAttrib.Image), True)]) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [SUBTREE_OPTIONS_STYLE_OPTIONS, GraphicAlignToString(NodeAttrib.GraphicAlign)]) + LineEnding;
    js := js + Format('$("#%s").attr("ischecked", "%s");', [SUBTREE_OPTIONS_OVERWRITE, BoolToStr(NodeAttrib.OverwriteDownstream, True)]) + LineEnding;
    js := js + Format('$("#%s").prop("checked", %s);', [SUBTREE_OPTIONS_OVERWRITE, BoolToStrLC(NodeAttrib.OverwriteDownstream, True)]) + LineEnding;
    if FileExists(TempDir + 'temp_image.bmp') and (not NodeAttrib.Image.Empty) then
       js := js + Format('$("#%s").attr("src", "%s");', [SUBTREE_OPTIONS_IMAGE, (TempDir.Replace('\', '/') + 'temp_image.bmp')]) + LineEnding;
    js := js + Format('$("#%s").val("%s");', [SUBTREE_OPTIONS_IMAGE_PATH, (Filename)]) + LineEnding;
    js := js + Format('$("#%s").attr("ischecked", "False");', [SUBTREE_OPTIONS_CLEAR_IMAGE]) + LineEnding;
    js := js + Format('$("#%s").prop("checked", false);', [SUBTREE_OPTIONS_CLEAR_IMAGE]) + LineEnding;
    js := js + Format('$("#%s").attr("ischecked", "False");', [SUBTREE_OPTIONS_EXPORT_IMAGE]) + LineEnding;
    js := js + Format('$("#%s").prop("checked", false);', [SUBTREE_OPTIONS_EXPORT_IMAGE]) + LineEnding;
    HtmlOptionsDialog.LoadOptionsFile(wofSubtreeDrawingOptionsFile, js, 'Subtree Drawing Options', 400, 560, HC_Subtree_Drawing_Options);
  except
   On E: Exception do
     ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TTreeViewForm.ActionSwapExecute(Sender: TObject);
begin
  if Tree.BranchFocused then
    Tree.FocusOnNode(Tree.FocusedIndex);
  if Tree.NodeFocused then
  begin
    Tree.FlipCluster;
    ActionBalancedTree.Checked := False;
    ActionInputOrderTree.Checked := False;
    DataSaved := False;
    if CurTool = ActionPoint then
      ActionPointExecute(Sender);
  end
  else if Tree.Cursor <> CURSOR_FLIP then
  begin
    CurTool.Checked := false;
    CurTool := ActionSwap;
    CurTool.Checked := true;
    Tree.Cursor := CURSOR_FLIP;
  end
  else
    ActionPointExecute(nil);
end;

procedure TTreeViewForm.ActionTaxonFontExecute(Sender: TObject);
var
  i: integer;
  a: TNodeAttrib;
begin
  FontDialog.Font.Assign(Tree.OTU_Font);
  if FontDialog.Execute then
  begin
    a := TNodeAttrib.Create;
    if OriTree.GroupAttrib.Count > 0 then
      for i := 0 to OriTree.GroupAttrib.Count-1 do
        if OriTree.GroupAttrib.IsSameFont(i, OriTree.OTU_Font) then
          OriTree.GroupAttrib[i].Font.Assign(FontDialog.Font);
    if OriTree.AttribList.Count > 1 then
      for i := 1 to OriTree.AttribList.Count-1 do
        if OriTree.AttribList.IsSameFont(i, OriTree.OTU_Font) then
        begin
          a.Assign(OriTree.AttribList[i]);
          a.Font.Assign(FontDialog.Font);
          OriTree.SetSubtreeAttrib(a, a.NodeIndex);
        end;
    OriTree.AttribList[0].Font.Assign(FontDialog.Font);
    if IsBootTree then
    begin
      if BootTree.GroupAttrib.Count > 0 then
        for i := 0 to BootTree.GroupAttrib.Count-1 do
          if BootTree.GroupAttrib.IsSameFont(i, BootTree.OTU_Font) then
            BootTree.GroupAttrib[i].Font.Assign(FontDialog.Font);
      if BootTree.AttribList.Count > 1 then
        for i := 1 to OriTree.AttribList.Count-1 do
          if BootTree.AttribList.IsSameFont(i, BootTree.OTU_Font) then
          begin
            a.Assign(BootTree.AttribList[i]);
            a.Font.Assign(FontDialog.Font);
            BootTree.SetSubtreeAttrib(a, a.NodeIndex);
          end;
      BootTree.AttribList[0].Font.Assign(FontDialog.Font);
    end;

    if IsReltimeTree then
    begin
      if ReltimeTree.GroupAttrib.Count > 0 then
        for i := 0 to ReltimeTree.GroupAttrib.Count-1 do
          if ReltimeTree.GroupAttrib.IsSameFont(i, ReltimeTree.OTU_Font) then
            ReltimeTree.GroupAttrib[i].Font.Assign(FontDialog.Font);
      if ReltimeTree.AttribList.Count > 1 then
        for i := 1 to OriTree.AttribList.Count-1 do
          if ReltimeTree.AttribList.IsSameFont(i, ReltimeTree.OTU_Font) then
          begin
            a.Assign(ReltimeTree.AttribList[i]);
            a.Font.Assign(FontDialog.Font);
            ReltimeTree.SetSubtreeAttrib(a, a.NodeIndex);
          end;
      ReltimeTree.AttribList[0].Font.Assign(FontDialog.Font);
    end;
    a.Free;
    OriTree.OTU_Font := FontDialog.Font;
    if IsBootTree then
      BootTree.OTU_Font := OriTree.OTU_Font;
    if IsReltimeTree then
      ReltimeTree.OTU_Font := OriTree.OTU_Font;
    Tree.Refresh;
    DataSaved := false;
    try
      FCheckStateChanging := True;
      FTaxaNamesFrame.TaxaFontSizeSpinEdit.Value := Tree.OTU_Font.Size;
    finally
      FCheckStateChanging := False;
    end;
  end;
end;

procedure TTreeViewForm.ActionTimesFontExecute(Sender: TObject);
begin
  FontDialog.Font.Assign(Tree.TimesFont);
  if FontDialog.Execute then
  begin
    OriTree.TimesFont := FontDialog.Font;
    if IsReltimeTree then
      ReltimeTree.TimesFont := FontDialog.Font;
    Tree.Refresh;
    DataSaved := false;
    try
      FCheckStateChanging := True;
      FDivTimesFrame.DivTimesFontSizeSpinEdit.Value := ReltimeTree.TimesFont.Size;
    finally
      FCheckStateChanging := False;
    end;
  end;
end;

procedure TTreeViewForm.ActionTopologyExecute(Sender: TObject);
begin
  Tree.ShowTopologyOnly := not Tree.ShowTopologyOnly;
  if ActionBalancedTree.Checked then
    Tree.SortClusterForShape
  else if ActionInputOrderTree.Checked then
    Tree.SortClusterInOrder;
  Tree.Refresh;
end;

//procedure TTreeViewForm.AncStatesSBtnClick(Sender: TObject);
//var
//  p: TPoint;
//begin
//  p.X := AncStatesToolBx.ClientOrigin.x;
//  p.Y := AncStatesSBtn.ClientOrigin.y + AncStatesSBtn.Height;
//  CharStatePopupMenu.Popup(p.X, p.Y);
//end;

procedure TTreeViewForm.CaptionHideActionExecute(Sender: TObject);
begin
  if not CaptionPanel.Visible then
    Exit;
  FCaptionPanelHeight := CaptionPanel.Height;
  if Assigned(CaptionViewer) then
    CaptionViewer.Visible := False;
  CaptionSplitter.Visible := False;
  CaptionPanel.Visible := False;
  Tree.Align := alClient;
  Tree.Invalidate;
  if Assigned(FCapViewer) and FCapViewer.Visible then
    FCapViewer.Visible := False;
  try
    FCheckStateChanging := True;
    if Assigned(FCaptionFrame) then
      FCaptionFrame.DisplayCaptionCheckBox.Checked := False;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.DisplayTaxaNamesActionExecute(Sender: TObject);
begin
  Tree.ShowOTUName := not Tree.ShowOTUName;
  ToggleTaxaNames;
end;

procedure TTreeViewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  TempFileName: String;
  i: Integer;
begin
  FormIsClosing := True;
  FCheckStateChanging := True;
  OriTree.IsClosing := True;
  BootTree.IsClosing := True;
  ReltimeTree.IsClosing := True;
  OriTree.Cursor := crDefault;
  BootTree.Cursor := crDefault;
  ReltimeTree.Cursor := crDefault;
  DisableActions;
  IniPropStorage1SaveProperties(Sender);
  CloseAction := caFree;
  if Assigned(FCalibrationDlg) then
    FCalibrationDlg.CancelledByTreeViewer := True;
  TempFileName := getTemp + 'temp_image.bmp';
  if FileExists(TempFileName) then
  begin
    try
      DeleteFile(TempFileName);
    except

    end;
  end;
  if Assigned(FFormObservers) and (FFormObservers.Count > 0) then
    for i := 0 to FFormObservers.Count - 1 do
      if Assigned(FFormObservers[i]) then
        FFormObservers[i].RemoveResultsForm(Self);
  if Assigned(FOnCloseNotify) then
    FOnCloseNotify(Self);
end;

procedure TTreeViewForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
{$IFNDEF DEBUG}
var
  aResponse: Integer;
{$ENDIF}
begin
  if IsSessionTest then
  begin
    CanClose := True;
    Exit;
  end;
  if FIsComputingMPBLens then
  begin
    ShowMessage('Please terminate the current running calculation (Parsimony branch lengths) before closing Tree Explorer');
    CanClose := False;
  end
  else if not FJustRootingATree then
  begin
    CanClose := True;
    {$IFNDEF DEBUG}
    else
    begin
      aResponse := QuestionDlg('Save Session?', 'Exiting Tree Explorer will discard the results. Save this session to store information for the future?', mtCustom, [mrYes, 'Save Session...', mrNo, 'Discard Results', mrCancel, 'Cancel'], 0);
      case aResponse of
        mrYes: ActionSaveSessionExecute(Sender);
        mrNo:;
        mrCancel:
        begin
          CanClose := False;
        end;
      end;
    end;
    {$ENDIF}
    {$IFDEF DARWIN}Hide;{$ENDIF}
  end;
end;

procedure TTreeViewForm.TreeTabControlChange(Sender: TObject);
begin
  {$IFDEF CALTEST}
  Exit;
  {$ENDIF}
  if not TreeTabControl.Enabled then
    Exit;
  OriTree.Visible := False;
  BootTree.Visible := False;
  RelTimeTree.Visible := False;

  with TreeTabControl do
  begin
    if (TabIndex = Tabs.IndexOf(ORI_TREE_TAB))
    or (TabIndex = Tabs.IndexOf(CONSENSUS_TREE_TAB)) then
    begin
      Tree := OriTree;
      //FormatDlg.EnableBlenOptions := OriTree.isBranchLength;
    end
    else if (TabIndex = Tabs.IndexOf(BOOT_TREE_TAB)) or (TabIndex = Tabs.IndexOf(BOOT_CONS_TREE_TAB)) then
    begin
      Tree := BootTree;
      Tree.ShowTopologyOnly := true;
      //FormatDlg.EnableBlenOptions := False;
    end
    else if (TabIndex = Tabs.IndexOf(TIME_TREE_TAB)) then
    begin
      ReltimeTree.LinearizeFunc := nil;
      Tree := ReltimeTree;
    end;
  end;
  ChangeTree(Tree);
  Tree.Refresh;
  Application.ProcessMessages;
  ActiveControl := Tree;

  if FSidePanelInitialized then
  begin
    if (Tree.NoOfTrees > 1) and (Tree.TreeIndex > 0) and (Tree <> ReltimeTree) then
    begin
      FMultiTreesFrame.TreeNumSEdit.MaxValue := Tree.NoOfTrees;
      FMultiTreesFrame.TreeNumSEdit.Value := Tree.TreeIndex;
      FMultiTreesFrame.NumTreesLabel.Caption := 'of ' + Format('%-6d', [Tree.NoOfTrees]);
      FMultiTreesFrame.Visible := True;
    end
    else
      FMultiTreesFrame.Visible := False;
  end;

  if RadiationPMenuItem.Checked and (Tree.TreeStyle <> tsRadiation) then
    if Tree.TreeStyle = tsCircle then
      ActionCircularExecute(Self)
    else if Tree.BranchStyle = bsRectangular then
      ActionRectangularExecute(Self)
    else if Tree.BranchStyle = bsStraight then
      ActionSlantedExecute(Self)
    else
      ActionCurvedExecute(Self)
  else
  begin
    if ActionRootOnMidpoint.Checked then
      Tree.MakeRootOnMidpoint;
    if ActionBalancedTree.Checked then
      Tree.SortClusterForShape
    else if ActionInputOrderTree.Checked and (Tree <> ReltimeTree) then
      Tree.SortClusterInOrder;
  end;
  CorrTestAction.Enabled := (Tree = ReltimeTree);
  Tree.Refresh;
  ResetStatusBar;
  RefreshCaption;
  UpdateSidePanel;
  if ActionInfo.Checked then
  begin
    RenewTreeInfo;
    RenewBranchInfo;
  end;
  Invalidate;
end;

procedure TTreeViewForm.TreeTabControlChangeBounds(Sender: TObject);
{$IFDEF DARWIN}
const
  TC_MARGIN = 8;
var
  aTree: TTreeBox;
  offset: Integer = 0;
{$ENDIF}
begin
  {$IFDEF DARWIN}
  if not Visible then
    Exit;
  offset := Screen.PixelsPerInch;
  if Screen.PixelsPerInch > 96 then
    offset := 32
  else
    offset := 0;
  aTree := Tree;
  if aTree.Align <> alNone then
    aTree.Align := alNone;
  if Tree.Top <> (TreeTabControl.Height - TreeTabControl.ClientHeight) then
    Tree.Top := (TreeTabControl.Height - TreeTabControl.ClientHeight);
  if Tree.Left <> TC_MARGIN then
    Tree.Left := TC_MARGIN;
  if Tree.Width <> TreeTabControl.ClientWidth then
    Tree.Width := TreeTabControl.ClientWidth;
  if Tree.Height <> (TreeTabControl.ClientHeight - offset) then
    Tree.Height := TreeTabControl.ClientHeight - offset;
  {$ENDIF}
end;

procedure TTreeViewForm.UseGroupAttribItemClick(Sender: TObject);
begin
  if FCheckStateChanging then Exit;
  Tree.UseGroupAttrib := not Tree.UseGroupAttrib;
  Tree.Refresh;
end;

procedure TTreeViewForm.UseSubtreeAttribItemClick(Sender: TObject);
begin
  Tree.UseSubtreeAttrib := not Tree.UseSubtreeAttrib;
  Tree.Refresh;
end;

procedure TTreeViewForm.MacTabButtonClick(Sender: TObject);
begin
  if FMacTabButtonsAreUpdating then
    Exit;
  try
    FMacTabButtonsAreUpdating := True;
    OriTreeBtn.Checked := False;
    BootTreeBtn.Checked := False;
    ReltimeTreeBtn.Checked := False;
    if Sender = OriTreeBtn then
    begin
      OriTreeBtn.Checked := True;
      TreeTabControl.TabIndex := TreeTabControl.Tabs.IndexOf(ORI_TREE_TAB);
    end
    else if Sender = BootTreeBtn then
    begin
      BootTreeBtn.Checked := True;
      if TreeTabControl.Tabs.IndexOf(CONSENSUS_TREE_TAB) >= 0 then
        TreeTabControl.TabIndex := TreeTabControl.Tabs.IndexOf(CONSENSUS_TREE_TAB)
      else if TreeTabControl.Tabs.IndexOf(BOOT_TREE_TAB) >= 0 then
        TreeTabControl.TabIndex := TreeTabControl.Tabs.IndexOf(BOOT_TREE_TAB)
      else if TreeTabControl.Tabs.IndexOf(BOOT_CONS_TREE_TAB) >= 0 then
        TreeTabControl.TabIndex := TreeTabControl.Tabs.IndexOf(BOOT_CONS_TREE_TAB);
    end
    else if Sender = ReltimeTreeBtn then
    begin
    ReltimeTreeBtn.Checked := True;
      TreeTabControl.TabIndex := TreeTabControl.Tabs.IndexOf(TIME_TREE_TAB);
    end;
    TreeTabControlChange(Sender);
  finally
    FMacTabButtonsAreUpdating := False;
  end;
end;

procedure TTreeViewForm.ClearOtuInfos;
var
  i: Integer;
  aInfo: TOtuInfo = nil;
begin
  if Assigned(FOtuInfos) then
  begin
    if FOtuInfos.Count > 0 then
      for i := 0 to FOtuInfos.Count - 1 do
      begin
        aInfo := TOtuInfo(FOtuInfos[i]);
        aInfo.Free;
        FOtuInfos[i] := nil;
      end;
    FOtuInfos.Clear;
    FreeAndNil(FOtuInfos);
  end;
end;

procedure TTreeViewForm.SetOtuInfos(aInfos: TList);
var
  i: Integer;
  aInfo: TOtuInfo = nil;
begin
  if not Assigned(aInfos) then
    Exit;
  FOtuInfos := TList.Create;
  if aInfos.Count > 0 then
    for i := 0 to aInfos.Count - 1 do
    begin
      aInfo := TOtuInfo.Create;
      aInfo.Assign(TOtuInfo(aInfos[i]));
      FOtuInfos.Add(aInfo);
    end;
end;

function TTreeViewForm.NextAncestralStateChange: Int64;
var
  site: Int64;
  n: TAbstractAncestralStatesNavigation = nil;
  anc, des: Int64;
begin
  Result := -1;
  if Tree.IsFocusedOnRoot or ((not Tree.NodeFocused) and (not Tree.BranchFocused)) then
  begin
    ShowMessage('Please select an internal branch for searching ancestral state changes');
    Exit;
  end;

  site := FAncestorsFrame.AncSiteNumSpinEdit.Value;
  try
    try
      if Assigned(MLAnalyzer) then
        n := TMLAncestralStatesNavigation.Create(MLAnalyzer)
      else if Assigned(FComputeParsimInfo) then
        n := TMPAncestralStatesNavigation.Create(FComputeParsimInfo, ExtCharAncItem.Checked)
      else
        raise Exception.Create('missing ancestral states navigation resourceds');
      des := Tree.FocusedIndex;
      anc := Tree.AncestorNodeNumber[des];
      Result := n.FindNextChange(des - 1, anc - 1, site);
    except
      on E:Exception do
        ShowMessage('Application Error: ' + E.Message);
    end;
  finally
    if Assigned(n) then
      n.Free;
  end;
end;

function TTreeViewForm.PreviousAncestralStateChange: Int64;
var
  site: Int64;
  n: TAbstractAncestralStatesNavigation = nil;
  anc, des: Int64;
begin
  Result := -1;
  if Tree.IsFocusedOnRoot or ((not Tree.NodeFocused) and (not Tree.BranchFocused)) then
  begin
    ShowMessage('Please select an internal branch for searching ancestral state changes');
    Exit;
  end;

  site := FAncestorsFrame.AncSiteNumSpinEdit.Value;
  try
    try
      if Assigned(MLAnalyzer) then
        n := TMLAncestralStatesNavigation.Create(MLAnalyzer)
      else if Assigned(FComputeParsimInfo) then
        n := TMPAncestralStatesNavigation.Create(FComputeParsimInfo, ExtCharAncItem.Checked)
      else
        raise Exception.Create('missing ancestral states navigation resources');
      des := Tree.FocusedIndex;
      anc := Tree.AncestorNodeNumber[des];
      Result := n.FindPreviousChange(des - 1, anc - 1, site);
    except
      on E: Exception do
        ShowMessage('Application Error: ' + E.Message);
    end;
  finally
    if Assigned(n) then
      n.Free;
  end;
end;

procedure TTreeViewForm.NextAncestralStateChangeNotify(b: TObject);
var
  site: Int64;
begin
  ValidateSpeedButton(b);
  site := NextAncestralStateChange;
  if site >= 1 then
    FAncestorsFrame.AncSiteNumSpinEdit.Value := site
  else
    ShowMessage('No more ancestral state changes for the selected branch were found');
end;

procedure TTreeViewForm.PrevAncestralStateChangeNotify(b: TObject);
var
  site: Int64;
begin
  ValidateSpeedButton(b);
  site := PreviousAncestralStateChange;
  if site >= 1 then
    FAncestorsFrame.AncSiteNumSpinEdit.Value := site
  else
    ShowMessage('No more ancestral state changes for the selected');
end;

procedure TTreeViewForm.PropagateTreeStyleChange;
begin
  if OriTree.NoOfTrees > 0 then
  begin
    OriTree.TreeStyle := Tree.TreeStyle;
    if Tree.TreeStyle = tsTraditional then
      OriTree.BranchStyle := Tree.BranchStyle;
  end;
  if IsBootTree and (Tree.TreeStyle <> tsRadiation) then
  begin
    BootTree.TreeStyle := Tree.TreeStyle;
    if Tree.TreeStyle = tsTraditional then
      BootTree.BranchStyle := Tree.BranchStyle;
  end;
  if IsReltimeTree then
  begin
    ReltimeTree.TreeStyle := Tree.TreeStyle;
    if Tree.TreeStyle = tsTraditional then
      ReltimeTree.BranchStyle := Tree.BranchStyle;
  end;
end;

function TTreeViewForm.CurAncStateCaption: String;
begin
  if CurAncState = ActionAncStateShowAll then
    Result := SHOW_ALL
  else if CurAncState = ActionAncStateShowMost then
    Result := SHOW_MOST_PROBABLE
  else if CurAncState = ActionAncStateHideAmbiguous then
    Result := HIDE_AMBIGUOUS
  else
    Result := EmptyStr;
end;

procedure TTreeViewForm.ShowHideMacTabButtons;
var
  aLeft: Integer = 0;
begin
  { this is a temporary workaround for tabs disappearing on macOS when the windos is resized. As
    soon as we upgrade the macOS dev environment to 2.0.10 we can remove MacOSTabsPanel and all
    related code including this procedure
  }
  {$IFDEF DARWIN}
  if FMacTabButtonsAreUpdating then
    Exit;
  try
    FMacTabButtonsAreUpdating := True;
    aLeft := OriTreeBtn.Left;
    OriTreeBtn.Checked := False;
    BootTreeBtn.Checked := False;
    ReltimeTreeBtn.Checked := False;
    OriTreeBtn.Visible := TreeTabControl.Tabs.IndexOf(ORI_TREE_TAB) >= 0;
    if OriTreeBtn.Visible then
      aLeft := aLeft + OriTreeBtn.Width;
    BootTreeBtn.Visible := (TreeTabControl.Tabs.IndexOf(CONSENSUS_TREE_TAB) >= 0) or (TreeTabControl.Tabs.IndexOf(BOOT_TREE_TAB) >= 0) or (TreeTabControl.Tabs.IndexOf(BOOT_CONS_TREE_TAB) >= 0);
    if BootTreeBtn.Visible then
    begin
      BootTreeBtn.Left := aLeft;
      aLeft := aLeft + BootTreeBtn.Width;
    end;
    ReltimeTreeBtn.Visible := TreeTabControl.Tabs.IndexOf(TIME_TREE_TAB) >= 0;
    if ReltimeTreeBtn.Visible then
      ReltimeTreeBtn.Left := aLeft;
    if TreeTabControl.TabIndex = TreeTabControl.Tabs.IndexOf(ORI_TREE_TAB) then
      OriTreeBtn.Checked := True;
    if (TreeTabControl.TabIndex = TreeTabControl.Tabs.IndexOf(CONSENSUS_TREE_TAB)) or (TreeTabControl.TabIndex = TreeTabControl.Tabs.IndexOf(BOOT_TREE_TAB)) or (TreeTabControl.TabIndex = TreeTabControl.Tabs.IndexOf(BOOT_CONS_TREE_TAB)) then
      BootTreeBtn.Checked := True;
    if TreeTabControl.TabIndex = TreeTabControl.Tabs.IndexOf(TIME_TREE_TAB) then
      ReltimeTreeBtn.Checked := True;
    MacOSTabsPanel.Visible := (OriTreeBtn.Visible or BootTreeBtn.Visible or ReltimeTreeBtn.Visible);
    TreeTabControl.TabPosition := tpBottom;
  finally
    FMacTabButtonsAreUpdating := False;
  end;
  {$ENDIF}
end;

procedure TTreeViewForm.InitFormPropertyStorage;
begin
  //IniPropStorage1.Active := True;
  //IniPropStorage1.Restore;
end;

procedure TTreeViewForm.GetStrictClockMinMaxTimes(var minTimes: ArrayOfExtended; var maxTimes: ArrayOfExtended);
var
  i: integer;
begin
  Assert(Length(ReltimeTree.CalibratedNodes) <= 1, 'strict clock trees can only have one calibration point');
  SetLength(minTimes, ReltimeTree.NoOfNodes);
  SetLength(maxTimes, ReltimeTree.NoOfNodes);
  for i := 0 to Length(minTimes) - 1 do
  begin
    if (Length(ReltimeTree.CalibratedNodes) = 1) and ((i + 1) = ReltimeTree.CalibratedNodes[0]) then
    begin
      minTimes[i] := ReltimeTree.GetDivergenceTime(i + 1);
      maxTimes[i] := ReltimeTree.GetDivergenceTime(i + 1);
    end
    else
    begin
      minTimes[i] := 0;
      maxTimes[i] := 0;
    end;
  end;
end;

procedure TTreeViewForm.DoImageExport(aFormat: TImageFileFormat);
var
  format: TImageFileFormat;
  pw: TPleaseWait = nil;
begin
  FImageExportOptionsDlg.SetExportFormat(aFormat);
  if FImageExportOptionsDlg.ShowModal = mrOk then
  begin
    try
      try
        pw := TPleaseWait.Create(Self);
        pw.Action := 'Generating Image Export...';
        pw.SetToMarqueeMode;
        pw.Show;
        format := FImageExportOptionsDlg.GetExportFormat;
        case format of
          iffPng: Tree.SaveToPng(FImageExportOptionsDlg.Filename);
          iffBitmap: Tree.SaveToBitmap(FImageExportOptionsDlg.Filename);
          iffTiff: Tree.SaveToTiff(FImageExportOptionsDlg.Filename);
          iffPdf: Tree.SaveToPdf(FImageExportOptionsDlg.Filename);
          iffSvg: Tree.SaveToSvg(FImageExportOptionsDlg.Filename);
          iffEmf: Tree.SaveToEmf(FImageExportOptionsDlg.Filename);
          else
            raise Exception.Create('Unsupported image file export format');
        end;
      except
        on E:Exception do
          ShowMessage('Oh no! An error has occurred: ' + E.Message);
      end;
    finally
      if Assigned(pw) then
      begin
        pw.hide;
        pw.Free;
      end;
    end;
  end;
end;

function TTreeViewForm.GetIsMLStr: String;
begin
  Result := 'false';
  case MAI.InitialUsrOperation of
    dtdoMLClockTest, dtdoMLClockTestLocal, dtdoMLModelTest, dtdoMLTree, dtdoMLInferAncSeq,
    dtdoMLPredictLivingSeq, dtdoMLComputePattern, dtdoMLTsTvBias, dtdoMLGammRates, dtdoMLInferSiteBySiteRates,
    dtdoMLCodonOmega, dtdoMLComputeUserTreeBLens, dtdoRelTimeML, dtdoRtdtML, dtdoMLInferAncSeqMyPeg, dtdoCorrTestML,
    dtdoEpML, dtdoMLIQTree: Result := 'true';
  end;
end;

function TTreeViewForm.GetTimeTreeTypeStr: String;
begin
  Result := EmptyStr;
  if tttStrictClocksMain in FTimetreeTypes then Result := 'tttStrictClocksMain'
  else if tttStrictClocksLocal in FTimetreeTypes then Result := 'tttStrictClocksLocal'
  else if tttReltimeLocal in FTimetreeTypes then Result := 'tttReltimeLocal'
  else if tttReltimeMain in FTimetreeTypes then Result := 'tttReltimeMain'
  else Result := 'tttNone';
end;

procedure TTreeViewForm.SetFormIsClosing(AValue: Boolean);
begin
  FFormIsClosing:=AValue;
  if Assigned(Tree) then
    Tree.ParentFormIsClosing := AValue;
end;

function TTreeViewForm.UpdateCaptionForAncestralStates: Boolean;
begin
  if Assigned(FigureGenerator) then
  begin
    FreeAndNil(FigureGenerator);
    FigureGenerator := TLegendGenerator.Create;
  end;
  if MAI.MyUsrOperation = dtdoBeam then
    Result := LoadTemplate(MAI, FigureGenerator)
  else if (CurAncState = ActionAncStateExtended) or
     (CurAncState = ActionAncStateShowAll) or
     (CurAncState = ActionAncStateShowMost) or
     (CurAncState = ActionAncStateHideAmbiguous) then
    Result := FigureGenerator.LoadTemplateFromFile('Infer_ancesteral_sequences.htm')
  else
    Result := LoadTemplate(MAI, FigureGenerator);
  RefreshCaption;
end;

procedure TTreeViewForm.CutoffDlgCancelledCallback(Sender: TObject);
begin
  HideOptionsDialog;
  ActionConsensus.Checked := False;
end;

procedure TTreeViewForm.ShowOptionsDialog;
begin
  HtmlOptionsDialog.Show;
  HtmlOptionsDialog.CancelCallback := ReEnableCallback;
  {$IFNDEF DARWIN}
  AlphaBlend := True;
  AlphaBlendValue := 190;
  Enabled := False;
  {$ELSE}
  FormIsClosing := True;
  {$ENDIF}
end;

procedure TTreeViewForm.HideOptionsDialog;
begin
  try
    if HtmlOptionsDialog.Visible then
      HtmlOptionsDialog.Hide;
  finally
    {$IFNDEF DARWIN}
    AlphaBlend := False;
    AlphaBlendValue := 255;
    Enabled := True;
    {$ELSE}
    FormIsClosing := False;
    {$ENDIF}
    Self.BringToFront;
  end;
end;

procedure TTreeViewForm.InitInstructionsLabel;
var
  margins: Integer = 10;
begin
  FBLensFrame.Visible := False;
  FNodeStatsFrame.Visible := False;
  FDistScaleFrame.Visible := False;
  FDivTimesFrame.Visible := False;
  FCollapseNodesFrame.Visible := False;
  FComputeFrame.Visible := False;
  FCaptionFrame.Visible := False;
  FTimeScaleFrame.Visible := False;

  ButtonsPanel.Visible := True;
  ButtonsPanel.Enabled := True;
  ButtonsPanel.Color := $00f7e892;
  FInstructionsLabel := TLabel.Create(Self);
  FInstructionsLabel.Alignment := taCenter;
  FInstructionsLabel.Caption := '  Click on a branch or node in the tree to set the root at that position.  ';
  FInstructionsLabel.WordWrap := True;

  FInstructionsLabel.Parent := ButtonsPanel;
  FInstructionsLabel.Align := alTop;
  FInstructionsLabel.WordWrap := True;

  if not Assigned(FOkButton) then
    FOkButton := TImage.Create(Self);
  FOkButton.Height := Buttons.Height;
  FOkButton.Width := Buttons.Width;
  Buttons.GetBitmap(0, FOkButton.Picture.Bitmap);
  FOkButton.Parent := ButtonsPanel;
  FOkButton.Anchors := [akBottom, akRight];
  FOkButton.BringToFront;
  FOkButton.OnMouseEnter := OkButtonMouseEnter;
  FOkButton.OnMouseLeave := OkButtonMouseLeave;

  if not Assigned(FCancelButton) then
    FCancelButton := TImage.Create(Self);
  FCancelButton.Height := Buttons.Height;
  FCancelButton.Width := Buttons.Width;
  FCancelButton.Anchors := [akBottom, akRight];
  FOkButton.Left := ButtonsPanel.Width - FOkButton.Width - margins;
  Buttons.GetBitmap(2, FCancelButton.Picture.Bitmap);
  FCancelButton.Parent := ButtonsPanel;
  FCancelButton.BringToFront;
  FCancelButton.Left := FOkButton.Left - FOkButton.Width - margins;
  FCancelButton.OnMouseEnter := CancelButtonMouseEnter;
  FCancelButton.OnMouseLeave := CancelButtonMouseLeave;

  ButtonsPanel.Height := FInstructionsLabel.Height + FOkButton.Height + 20;
  FOkButton.Top := ButtonsPanel.Height - FOkButton.Height - 4;
  FCancelButton.Top := FOkButton.Top;
  ButtonsPanel.Parent := Panel13;
  ButtonsPanel.Align := alBottom;
  {$IFDEF DARWIN}
  FOkButton.Proportional := True;
  FCancelButton.Proportional := True;
  {$ENDIF}
end;

procedure TTreeViewForm.OkButtonMouseEnter(Sender: TObject);
begin
  Buttons.GetBitmap(1, FOkButton.Picture.Bitmap);
end;

procedure TTreeViewForm.OkButtonMouseLeave(Sender: TObject);
begin
  Buttons.GetBitmap(0, FOkButton.Picture.Bitmap);
end;

procedure TTreeViewForm.CancelButtonMouseEnter(Sender: TObject);
begin
  Buttons.GetBitmap(3, FCancelButton.Picture.Bitmap);
end;

procedure TTreeViewForm.CancelButtonMouseLeave(Sender: TObject);
begin
  Buttons.GetBitmap(2, FCancelButton.Picture.Bitmap);
end;

procedure TTreeViewForm.ReEnableCallback(Sender: TObject);
begin
  HideOptionsDialog;
end;

procedure TTreeViewForm.ValidateCheckbox(c: TObject);
begin
  if not (c is TCheckBox) then
    raise EInvalidCast.Create('expected TCheckBox but got ' + c.ClassName);
end;

procedure TTreeViewForm.ValidateRadioButton(r: TObject);
begin
  if not (r is TRadioButton) then
    raise EInvalidCast.Create('expected TRadioButton but got ' + r.ClassName);
end;

procedure TTreeViewForm.ValidateIntSpinEdit(s: TObject);
begin
  if not (s is TSpinEdit) then
    raise EInvalidCast.Create('expected TSpinEdit but got ' + s.ClassName);
end;

procedure TTreeViewForm.ValidateFloatSpinEdit(s: TObject);
begin
  if not (s is TFloatSpinEdit) then
    raise EInvalidCast.Create('expected TFloatSpinEdit but got ' + s.ClassName);
end;

procedure TTreeViewForm.ValidateComboBox(c: TObject);
begin
  if not (c is TComboBox) then
    raise EInvalidCast.Create('expected TComboBox but got ' + c.ClassName);
end;

procedure TTreeViewForm.ValidateButton(b: TObject);
begin
  if not (b is TButton) then
    raise EInvalidCast.Create('expected TButton but got ' + b.ClassName);
end;

procedure TTreeViewForm.ValidateSpeedButton(b: TObject);
begin
  if not (b is TSpeedButton) then
    raise EInvalidCast.Create('expected TSpeedButton but got ' + b.ClassName);
end;

procedure TTreeViewForm.ValidateTrackBar(t: TObject);
begin
  if not (t is TTrackBar) then
    raise EInvalidCast.Create('expected TTrackBar but got ' + t.ClassName);
end;

procedure TTreeViewForm.ValidateEdit(e: TObject);
begin
  if not (e is TEdit) then
    raise EInvalidCast.Create('expectd TEdit but got ' + e.ClassName);
end;

procedure TTreeViewForm.ToggleTaxaNames;
begin
  if Tree.ShowOTUName then
  begin
    OriTree.ShowSpeciesName := False;
    OriTree.ShowOTUName := True;
    BootTree.ShowSpeciesName := False;
    BootTree.ShowOTUName := True;
    ReltimeTree.ShowSpeciesName := False;
    ReltimeTree.ShowOTUName := True;
  end
  else
  begin
    OriTree.ShowOtuName := False;
    BootTree.ShowOtuName := False;
    ReltimeTree.ShowOtuName := False;
  end;
  Tree.Invalidate;
  DataSaved := False;
  try
    FCheckStateChanging := True;
    if Assigned(FGeneDuplicationsFrame) then
      if Tree.ShowOTUName then
        FGeneDuplicationsFrame.DisplaySequenceNamesBtn.Checked := True
      else if Tree.ShowSpeciesName then
        FGeneDuplicationsFrame.DisplaySpeciesNamesBtn.Checked := True;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.ToggleTaxaNamesNotify(aCheckBx: TObject);
var
  cb: TCheckBox = nil;
begin
  if FCheckStateChanging then
    Exit;
  ValidateCheckbox(aCheckBx);
  cb := TCheckBox(aCheckBx);
  Tree.ShowOTUName := cb.Checked;
  ToggleTaxaNames;
end;

procedure TTreeViewForm.ToggleBLensNotify(c: TObject);
var
  cb: TCheckBox = nil;
begin
  ValidateCheckbox(c);
  cb := TCheckBox(c);
  Tree.ShowBLen := cb.Checked;
  Tree.Invalidate;
  DataSaved := False;
  ActionBranchLengthDisplay.Checked := cb.Checked;
end;

procedure TTreeViewForm.HideBLensShorterThanNotify(s: TObject);
var
  se: TFloatSpinEdit = nil;
begin
  ValidateFloatSpinEdit(s);
  se := TFloatSpinEdit(s);
  if FBLensFrame.BLensHideIfShorterCheckBx.Checked then
    Tree.BLenCutoff := se.Value;
  Tree.Invalidate;
end;

procedure TTreeViewForm.HideBlensShortThanCheckBoxNotify(c: TObject);
var
  cb: TCheckBox = nil;
begin
  ValidateCheckBox(c);
  cb := TCheckBox(c);
  if cb.Checked then
    Tree.BLenCutoff := FBLensFrame.BLensHideIfShorterSpinEdit.Value
  else
    Tree.BLenCutoff := Tree.MinBranchLength;
  Tree.Invalidate;
end;

procedure TTreeViewForm.BLensPrecisionNotify(s: TObject);
var
  se: TSpinEdit = nil;
begin
  ValidateIntSpinEdit(s);
  se := TSpinEdit(s);
  Tree.BLenDecimals := se.Value;
  Tree.Invalidate;
end;

procedure TTreeViewForm.BLensFontSizeNotify(s: TObject);
var
  se: TSpinEdit = nil;
begin
  ValidateIntSpinEdit(s);
  se := TSpinEdit(s);
  Tree.BLensFont.Size := se.Value;
  Tree.Invalidate;
end;

procedure TTreeViewForm.BLensPlacementNotify(c: TObject);
var
  cb: TComboBox = nil;
begin
  ValidateComboBox(c);
  cb := TComboBox(c);
  Tree.BLenPosition := TBranchInfoPosition(cb.ItemIndex);
  Tree.Invalidate;
end;

procedure TTreeViewForm.HideOverlappingTaxaNotify(aCheckBx: TObject);
var
  c: TCheckBox = nil;
begin
  ValidateCheckBox(aCheckBx);
  c := TCheckBox(aCheckBx);
  ActionHideOverlappingTaxa.Checked := c.Checked;
  Tree.HideOverlappingTaxa := c.Checked;
  Tree.Invalidate;
end;

procedure TTreeViewForm.UpdateMarkerDisplay;
begin
  if IsBootTree then
    BootTree.ShowOTUMarker := OriTree.ShowOTUMarker;
  if IsReltimeTree then
    ReltimeTree.ShowOTUMarker := OriTree.ShowOTUMarker;
  Tree.Invalidate;
  DataSaved := False;
  ActionMarkerDisplay.Checked := Tree.ShowOTUMarker;
end;

procedure TTreeViewForm.MarkerDisplayChangedNotify(c: TObject);
var
  cb: TCheckBox = nil;
begin
  ValidateCheckBox(c);
  cb := TCheckBox(c);
  Tree.ShowOTUMarker := cb.Checked;
  UpdateMarkerDisplay;
end;

procedure TTreeViewForm.EditMarkersNotify(b: TObject);
begin
  FormatLabelsItemClick(b);
end;

procedure TTreeViewForm.ArrangeTaxaNotify(r: TObject);
var
  rb: TRadioButton = nil;
begin
  if FCheckStateChanging then
    Exit;
  ValidateRadioButton(r);
  try
    FCheckStateChanging := True;
    rb := TRadioButton(r);
    if (rb = FTaxaNamesFrame.BalancedShapeRadioBtn) or (rb = FTaxaNamesFrame.InputOrderRadioBtn) then
    begin
      if rb = FTaxaNamesFrame.BalancedShapeRadioBtn then
        ActionBalancedTreeExecute(nil)
      else
        ActionInputOrderTreeExecute(nil);
    end
    else
      raise Exception.Create('invalid message sender in ArrangeTaxaNotify');
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.ToggleStatsNotify(c: TObject);
var
  cb: TCheckBox = nil;
begin
  if FCheckStateChanging then
    Exit;
  ValidateCheckbox(c);
  cb := TCheckBox(c);
  try
    FCheckStateChanging := True;
    Tree.ShowStats := (cb.Checked and FNodeStatsFrame.BootstrapFrequencyBtn.Checked);
    Tree.ShowDataCoverage := (cb.Checked and FNodeStatsFrame.DataCoverageBtn.Checked);
    Tree.ShowNodeIds := (cb.Checked and FNodeStatsFrame.NodeIdsBtn.Checked);
    UpdateStatsDisplay;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.StatsFontSizeNotify(s: TObject);
var
  se: TSpinEdit = nil;
begin
  ValidateIntSpinEdit(s);
  se := TSpinEdit(s);
  Tree.StatsFont.Size := se.Value;
  Tree.Invalidate;
end;

procedure TTreeViewForm.StatsPlacementNotify(c: TObject);
var
  cb: TComboBox = nil;
begin
  ValidateComboBox(c);
  cb := TComboBox(c);
  Tree.StatsPosition := TBranchInfoPosition(cb.ItemIndex);
  Tree.Invalidate;
end;

procedure TTreeViewForm.StatsHideShorterThanNotify(c: TObject);
var
  cb: TCheckBox = nil;
begin
  ValidateCheckBox(c);
  cb := TCheckBox(c);
  if cb.Checked then
    Tree.StatsCutoff := FNodeStatsFrame.StatsHideIfLowerThanSpinEdit.Value
  else
    Tree.StatsCutoff := 0;
  Tree.Invalidate;
end;

procedure TTreeViewForm.StatsHideShorterThanSpinEditNotify(s: TObject);
var
  se: TSpinEdit = nil;
begin
  ValidateIntSpinEdit(s);
  se := TSpinEdit(s);
  if FNodeStatsFrame.StatsHideIfLowerThanCheckBx.Checked then
    Tree.StatsCutoff := se.Value;
  Tree.Invalidate;
end;

procedure TTreeViewForm.StatsHorizDistNotify(s: TObject);
var
  se: TSpinEdit = nil;
  p: TPoint;
begin
  ValidateIntSpinEdit(s);
  se := TSpinEdit(s);
  p := Tree.StatsMargin;
  p.X := se.Value;
  Tree.StatsMargin := p;
  Tree.Invalidate;
end;

procedure TTreeViewForm.StatsVertDistNotify(s: TObject);
var
  se: TSpinEdit = nil;
  p: TPoint;
begin
  ValidateIntSpinEdit(s);
  se := TSpinEdit(s);
  p := Tree.StatsMargin;
  p.Y := se.Value;
  Tree.StatsMargin := p;
  Tree.Invalidate;
end;

procedure TTreeViewForm.DisplayTypeChange(r: TObject);
var
  btn: TRadioButton = nil;
begin
  if FCheckStateChanging then
    Exit;
  ValidateRadioButton(r);
  btn := TRadioButton(r);
  if not btn.Checked then { event fires for checked and unchecked button leading to unneeded updates if not exited}
    Exit;
  try
    FCheckStateChanging := True;

    FNodeStatsFrame.RadioBtnChange(r);
    if FNodeStatsFrame.ShowStatsCheckBx.Checked then
    begin
      if FNodeStatsFrame.BootstrapFrequencyBtn.Checked then
        ActionBranchInfoDisplayExecute(r)
      else if FNodeStatsFrame.NodeIdsBtn.Checked then
        ActionNodeIdsDisplayExecute(r)
      else if FNodeStatsFrame.DataCoverageBtn.Checked then
        ActionDataCoverageDisplayExecute(r)
      else
        ShowMessage('Application Error: missing radio button handler');
    end;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.UpdateStatsDisplay;
begin
  if Tree.ShowStats then
  begin
    Tree.ShowNodeIds := False;
    Tree.ShowDataCoverage := False;
    if not FNodeStatsFrame.BootstrapFrequencyBtn.Checked then
      FNodeStatsFrame.BootstrapFrequencyBtn.Checked := True;
  end
  else if Tree.ShowNodeIds then
  begin
    Tree.ShowStats := False;
    Tree.ShowDataCoverage := False;
    if not FNodeStatsFrame.NodeIdsBtn.Checked then
      FNodeStatsFrame.NodeIdsBtn.Checked := True;
  end
  else if Tree.ShowDataCoverage then
  begin
    Tree.ShowStats := False;
    Tree.ShowNodeIds := False;
    if not FNodeStatsFrame.DataCoverageBtn.Checked then
      FNodeStatsFrame.DataCoverageBtn.Checked := True;
  end;
  ActionBranchInfoDisplay.Checked := Tree.ShowStats;
  ActionNodeIdsDisplay.Checked := Tree.ShowNodeIds;
  ActionDataCoverageDisplay.Checked := Tree.ShowDataCoverage;
  FNodeStatsFrame.ShowStatsCheckBx.Checked := (Tree.ShowStats or Tree.ShowNodeIds or Tree.ShowDataCoverage);
  Tree.Invalidate;
  DataSaved := False;
  RefreshCaption;
end;

procedure TTreeViewForm.LayoutTreeStyleNotify(b: TObject);
var
  target: TPoint;
  btn: TSpeedButton = nil;
begin
  ValidateSpeedButton(b);
  btn := TSpeedButton(b);
  target.x := btn.Left;
  target.y := btn.Top + btn.Height;
  TreeStylePopupMenu.Popup(target.X, target.Y);
end;

procedure TTreeViewForm.LayoutTreeWidthNotify(t: TObject);
const
  DEFAULT_WIDTH = 400;
var
  tb: TTrackBar = nil;
begin
  ValidateTrackBar(t);
  tb := TTrackBar(t);
  Tree.TreeWidth := DEFAULT_WIDTH + 50*tb.Position;
  Tree.Refresh;
end;

procedure TTreeViewForm.LayoutTreeHeightNotify(t: TObject);
var
  tb: TTrackBar = nil;
begin
  ValidateTrackBar(t);
  tb := TTrackBar(t);
  if tb.Position > 0 then
    Tree.PixelsPerOTU := DEFAULT_PPOTU*tb.Position
  else
    Tree.PixelsPerOTU := DEFAULT_PPOTU;
  Tree.Refresh;
end;

procedure TTreeViewForm.DistScaleDisplayNotify(c: TObject);
var
  cb: TCheckBox = nil;
begin
  ValidateCheckbox(c);
  cb := TCheckBox(c);
  Tree.ShowScale := cb.Checked;
  Tree.Invalidate;
end;

procedure TTreeViewForm.DistScaleFontSizeNotify(s: TObject);
var
  se: TSpinEdit = nil;
begin
  if FCheckStateChanging then
    Exit;
  ValidateIntSpinEdit(s);
  se := TSpinEdit(s);
  Tree.ScaleFont.Size := se.Value;
  if Tree.TreeStyle <> tsCircle then
  begin
    Tree.SetTreeSize;
    Tree.Invalidate;
  end
  else
    Tree.Refresh;
end;

procedure TTreeViewForm.DistScaleCaptionEditNotify(e: TObject);
var
  edit: TEdit = nil;
begin
  ValidateEdit(e);
  edit := TEdit(e);
  Tree.ScaleUnit := edit.Text;
  Tree.Invalidate;
end;

procedure TTreeViewForm.DistScaleTickIntervalNotify(s: TObject);
var
  se: TSpinEdit = nil;
begin
  ValidateIntSpinEdit(s);
  se := TSpinEdit(s);
  Tree.ScaleTick := se.Value;
  Tree.Invalidate;
end;

procedure TTreeViewForm.DistScaleLengthNotify(s: TObject);
var
  se: TFloatSpinEdit = nil;
begin
  ValidateFloatSpinEdit(s);
  se := TFloatSpinEdit(s);
  Tree.ScaleText := Format('%.2f', [se.Value]);
  Tree.Invalidate;
end;

procedure TTreeViewForm.DistScaleLineWidthNotify(c: TObject);
var
  cb: TComboBox = nil;
begin
  if FCheckStateChanging then
    Exit;
  ValidateComboBox(c);
  cb := TComboBox(c);
  Tree.ScalePen.Width := cb.ItemIndex + 1;
  Tree.Invalidate;
end;

procedure TTreeViewForm.TimescaleDisplayNotify(c: TObject);
var
  cb: TCheckBox = nil;
begin
  ValidateCheckbox(c);
  cb := TCheckBox(c);
  ReltimeTree.ShowTimeScale := cb.Checked;
  if Assigned(MAI) and MAI.IsRtdt then
    ReltimeTree.ShowSamplingTimeScale := cb.Checked;
  ReltimeTree.Invalidate;
end;

procedure TTreeViewForm.TimescaleLineWidthNotify(c: TObject);
var
  cb: TComboBox = nil;
begin
  if FCheckStateChanging then
    Exit;
  ValidateComboBox(c);
  cb :=  TComboBox(c);
  ReltimeTree.ScalePen.Width := cb.ItemIndex + 1;
  ReltimeTree.Invalidate;
end;

procedure TTreeViewForm.TimescaleFontSizeNotify(s: TObject);
var
  se: TSpinEdit = nil;
begin
  if FCheckStateChanging then
    Exit;
  ValidateIntSpinEdit(s);
  se := TSpinEdit(s);
  ReltimeTree.ScaleFont.Size := se.Value;
  if ReltimeTree.TreeStyle <> tsCircle then
  begin
    ReltimeTree.SetTreeSize;
    ReltimeTree.Invalidate;
  end
  else
    ReltimeTree.Refresh;
end;

procedure TTreeViewForm.TimescaleCaptionEditNotify(e: TObject);
var
  edit: TEdit = nil;
begin
  ValidateEdit(e);
  edit := TEdit(e);
  ReltimeTree.TimeUnit := edit.Text;
  ReltimeTree.Invalidate;
end;

procedure TTreeViewForm.TimescaleMinorTickNotify(s: TObject);
var
  se: TFloatSpinEdit = nil;
begin
  ValidateFloatSpinEdit(s);
  se := TFloatSpinEdit(s);
  ReltimeTree.TimeTick := se.Value;
  ReltimeTree.Invalidate;
end;

procedure TTreeViewForm.TimescaleMajorTickNotify(s: TObject);
var
  se: TFloatSpinEdit = nil;
  val: Double = 0.0;
begin
  if FCheckStateChanging then
    Exit;
  ValidateFloatSpinEdit(s);
  se := TFloatSpinEdit(s);
  if ReltimeTree.IsSamplingTimes then
    val := -1*se.Value
  else
    val := se.Value;
  ReltimeTree.TimeText := Format('%.2f', [val]);
  ReltimeTree.Invalidate;
end;

procedure TTreeViewForm.TimescaleHeightErrBarsNotify(c: TObject);
var
  cb: TCheckBox = nil;
begin
  if FCheckStateChanging then
    Exit;
  ValidateCheckbox(c);
  cb := TCheckBox(c);
  ReltimeTree.ShowHeightErrBar := cb.Checked;
  ReltimeTree.Invalidate;
end;

procedure TTreeViewForm.DivTimesDisplayNotify(c: TObject);
var
  cb: TCheckbox = nil;
begin
  if FCheckStateChanging then
    Exit;
  ValidateCheckbox(c);
  try
    FCheckStateChanging := True;
    ActionDisplayDivergenceTimesExecute(c);
    cb := TCheckBox(c);
    cb.Checked := ReltimeTree.ShowDivergenceTimes;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.DivTimesFontNotify(b: TObject);
begin
  ActionTimesFontExecute(b);
end;

procedure TTreeViewForm.DivTimesFontSizeNotify(s: TObject);
var
  se: TSpinEdit = nil;
begin
  ValidateIntSpinEdit(s);
  se := TSpinEdit(s);
  ReltimeTree.TimesFont.Size := se.Value;
  ReltimeTree.Invalidate;
end;

procedure TTreeViewForm.DivTimesPrecisionNotify(s: TObject);
var
  se: TSpinEdit = nil;
begin
  ValidateIntSpinEdit(s);
  se := TSpinEdit(s);
  ReltimeTree.DivTimeDecimals := se.Value;
  ReltimeTree.Invalidate;
end;

procedure TTreeViewForm.SubtreeUseSubtreeAttribsNotify(c: TObject);
var
  cb: TCheckBox = nil;
begin
  ValidateCheckbox(c);
  cb := TCheckBox(c);
  Tree.UseSubtreeAttrib := cb.Checked;
  Tree.Invalidate;
end;

procedure TTreeViewForm.SubtreeUseGroupAttribsNotify(c: TObject);
var
  cb: TCheckBox = nil;
begin
  ValidateCheckbox(c);
  cb := TCheckBox(c);
  Tree.UseGroupAttrib := cb.Checked;
end;

procedure TTreeViewForm.ComputeConsensusNotify(b: TObject);
var
  btn: TSpeedButton = nil;
begin
  ValidateSpeedButton(b);
  btn := TSpeedButton(b);
  ActionConsensus.Checked := (not ActionConsensus.Checked);
  ActionConsensusExecute(btn);
end;

procedure TTreeViewForm.ComputeCondensedNotify(b: TObject);
var
  btn: TSpeedButton = nil;
begin
  ValidateSpeedButton(b);
  btn := TSpeedButton(b);
  ActionCondensed.Checked := (not ActionCondensed.Checked);
  ActionCondensedExecute(btn);
end;

procedure TTreeViewForm.ConsensusCutoffChangedNotify(s: TObject);
begin
  ValidateIntSpinEdit(s);
  ApplyConsensusCondensedTreeOptions(FComputeFrame.ConsensusTreeCutoffSpinEdit.Value, FComputeFrame.CondensedTreeCutoffSpinEdit.Value);
end;

procedure TTreeViewForm.CondensedCutoffChangedNotify(s: TObject);
begin
  if FCheckStateChanging then
    Exit;
  ValidateIntSpinEdit(s);
  ValidateIntSpinEdit(s);
  ApplyConsensusCondensedTreeOptions(FComputeFrame.ConsensusTreeCutoffSpinEdit.Value, FComputeFrame.CondensedTreeCutoffSpinEdit.Value);
end;

procedure TTreeViewForm.AncRadioGroupNotify(r: TObject);
var
  index: Integer = -1;
begin
  if FCheckStateChanging then
    Exit;
  try
    FCheckStateChanging := True;
    index := FAncestorsFrame.AncStatesDisplaySelection;
    case index of
      0:
        begin
          ActionAncStateShowAllExecute(r);
          FAncestorsFrame.AncExportComboBx.ItemIndex := ALL_SITE_INDEX;
        end;
      1:
        begin
          ActionAncStateShowMostExecute(r);
          FAncestorsFrame.AncExportComboBx.ItemIndex := MOST_PROBABLE_INDEX;
        end;
      2:
        begin
          ActionAncStateHideAmbiguousExecute(r);
          FAncestorsFrame.AncExportComboBx.ItemIndex := ALL_SITE_INDEX;
        end;
      3:
        begin
          ActionAncStateShowNoneExecute(r);
        end
      else
        ShowMessage('Application Error: missing ancestral states display handler');
    end;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.AncExtCharNotfity(c: TObject);
var
  cb: TCheckBox = nil;
begin
  if FCheckStateChanging then
    Exit;
  ValidateCheckbox(c);
  cb := TCheckBox(c);
  ActionAncStateExtendedExecute(c);
  try
    FCheckStateChanging := True;
    cb.Checked := ActionAncStateExtended.Checked;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.AncFontNotify(b: TObject);
begin
  ValidateButton(b);
  ActionAncStateFontExecute(b);
end;

procedure TTreeViewForm.AncFontSizeNotify(s: TObject);
var
  se: TSpinEdit = nil;
begin
  ValidateIntSpinEdit(s);
  se := TSpinEdit(s);
  OriTree.CharStateFont.Size := se.Value;
  BootTree.CharStateFont.Size := se.Value;
  ReltimeTree.CharStateFont.Size := se.Value;
  Tree.Invalidate;
end;

procedure TTreeViewForm.AncExportBtnNotify(s: TObject);
var
  exportType: TAncestralStatesExportType;
begin
  ValidateSpeedButton(s);
  exportType := FAncestorsFrame.GetSelectedExportOption;
  case exportType of
    asExportMostProbable:
      begin
        if Assigned(MLAnalyzer) then
          ActionActStateExportMostProbableSequenceExecute(s)
        else
          ShowMessage('Export of most probable sequences is only available when using the Maximum Likelihood method');
      end;
    asExportCurrentSite: ActionActStateExportCurrentSiteExecute(s);
    asExportAllSites: ActionAncStateExportAllSitesExecute(s);
    asExportChangesList: ActionActStateExportChangesListExecute(s);
    asExportDetails: ActionAncStateExportTextExportExecute(s);
    else
      ShowMessage('Application Error: invalid ancestral states export type');
  end;
end;

procedure TTreeViewForm.UpdateAncestorRadioBtns;
begin
  if CurAncState = ActionAncStateShowAll then
  begin
    if (not FAncestorsFrame.AncShowAllRadioBtn.Checked) then
      FAncestorsFrame.AncShowAllRadioBtn.Checked := True;
  end
  else if CurAncState = ActionAncStateShowMost then
  begin
    if (not FAncestorsFrame.AncMostProbableRadioBtn.Checked) then
      FAncestorsFrame.AncMostProbableRadioBtn.Checked := True;
  end
  else if CurAncState = ActionAncStateHideAmbiguous then
  begin
    if (not FAncestorsFrame.AncHideAmbiguousRadioBtn.Checked) then
      FAncestorsFrame.AncHideAmbiguousRadioBtn.Checked := True;
  end
  else
    FAncestorsFrame.AncShowNoneRadioBtn.Checked := True;
end;

procedure TTreeViewForm.CollapsePanelsChbxNotify(Sender: TObject);
begin
  if FCheckStateChanging then
    Exit;

  if FCollapseNodesFrame.CollapseNodesChbx.Checked then
  begin
    Tree.ShowCompressedClusterSize := FCollapseNodesFrame.ShowClusterSizeCheckbox.Checked;
    if FCollapseNodesFrame.CollapseGroupsBtn.Checked then
      Tree.CompressGroups
    else if FCollapseNodesFrame.CollapseByDiffBtn.Checked then
    begin
      Tree.CompressSimilarSequences(FCollapseNodesFrame.CollapseBranchesCutoffSpinEdit.Value);
    end
    else if FCollapseNodesFrame.CollapseSizeBtn.Checked then
      Tree.CompressClustersBySize(FCollapseNodesFrame.ClusterSizeSpinEdit.Value);
  end
  else
    Tree.ExpandAutoCompressedClusters(True);
end;

procedure TTreeViewForm.CollapsePanelsTargetNotify(Sender: TObject);
begin
  if Sender = FCollapseNodesFrame.ClusterSizeSpinEdit then
    if not FCollapseNodesFrame.CollapseSizeBtn.Checked then
      Exit;
  if Sender = FCollapseNodesFrame.CollapseBranchesCutoffSpinEdit then
    if not FCollapseNodesFrame.CollapseByDiffBtn.Checked then
      Exit;
  if FCollapseNodesFrame.CollapseNodesChbx.Checked then
    CollapsePanelsChbxNotify(Sender);
end;

procedure TTreeViewForm.InitMainMenu;
{$IFDEF DARWIN}
const
  UNCHECK_IMAGE_INDEX = 204;
var
  i: Integer;
{$ENDIF}
begin
  {$IFNDEF DARWIN}
  MainMenu.OwnerDraw := True;
  MainMenu.OnMeasureItem := MegaForm.MeasureMenuItem;
  MainMenu.OnDrawItem := MegaForm.DrawMenuItem
  {$ELSE}
  if TreeActionList.ActionCount > 0 then
    for i := 0 to TreeActionList.ActionCount - 1 do
      if TreeActionList.Actions[i] is TAction then
        with TreeActionList.Actions[i] as TAction do
          if ImageIndex = UNCHECK_IMAGE_INDEX then
            ImageIndex := -1;
  {$ENDIF}
end;

procedure TTreeViewForm.InitPopupMenus;
begin
  MegaForm.AddSettingsToPopupMenu(TreeStylePopupMenu);
  MegaForm.AddSettingsToPopupMenu(TreePopupMenu);
  MegaForm.AddSettingsToPopupMenu(BranchStyleMenu);
  MegaForm.AddSettingsToPopupMenu(CharStatePopupMenu);
end;

procedure TTreeViewForm.InitTreeOptionsPopupMenu;
begin
  MegaForm.AddSettingsToPopupMenu(TreeOptionsPopup);
end;

procedure TTreeViewForm.TaxaFontSizeChange(aSpinEdit: TObject);
var
  i: Integer;
  a: TNodeAttrib = nil;
  e: TSpinEdit = nil;
begin
  if not (aSpinEdit is TSpinEdit) then
    raise EInvalidCast.Create('expected TSpinEdit but got ' + aSpinEdit.ClassName);
  e := TSpinEdit(aSpinEdit);
  a := TNodeAttrib.Create;
  if OriTree.GroupAttrib.Count > 0 then
    for i := 0 to OriTree.GroupAttrib.Count-1 do
      if OriTree.GroupAttrib.IsSameFont(i, OriTree.OTU_Font) then
        OriTree.GroupAttrib[i].Font.Size := e.Value;
  if OriTree.AttribList.Count > 1 then
    for i := 1 to OriTree.AttribList.Count-1 do
      if OriTree.AttribList.IsSameFont(i, OriTree.OTU_Font) then
      begin
        a.Assign(OriTree.AttribList[i]);
        a.Font.Assign(OriTree.OTU_Font);
        a.Font.Size := e.Value;
        OriTree.SetSubtreeAttrib(a, a.NodeIndex);
      end;
  OriTree.AttribList[0].Font.Size := e.Value;
  if IsBootTree then
  begin
    if BootTree.GroupAttrib.Count > 0 then
      for i := 0 to BootTree.GroupAttrib.Count-1 do
        if BootTree.GroupAttrib.IsSameFont(i, BootTree.OTU_Font) then
          BootTree.GroupAttrib[i].Font.Size := e.Value;
    if BootTree.AttribList.Count > 1 then
      for i := 1 to OriTree.AttribList.Count-1 do
        if BootTree.AttribList.IsSameFont(i, BootTree.OTU_Font) then
        begin
          a.Assign(BootTree.AttribList[i]);
          a.Font.Assign(BootTree.OTU_Font);
          a.Font.Size := e.Value;
          BootTree.SetSubtreeAttrib(a, a.NodeIndex);
        end;
    BootTree.AttribList[0].Font.Size := e.Value;
  end;

  if IsReltimeTree then
  begin
    if ReltimeTree.GroupAttrib.Count > 0 then
      for i := 0 to ReltimeTree.GroupAttrib.Count-1 do
        if ReltimeTree.GroupAttrib.IsSameFont(i, ReltimeTree.OTU_Font) then
          ReltimeTree.GroupAttrib[i].Font.Size := e.Value;
    if ReltimeTree.AttribList.Count > 1 then
      for i := 1 to OriTree.AttribList.Count-1 do
        if ReltimeTree.AttribList.IsSameFont(i, ReltimeTree.OTU_Font) then
        begin
          a.Assign(ReltimeTree.AttribList[i]);
          a.Font.Assign(ReltimeTree.OTU_Font);
          a.Font.Size := e.Value;
          ReltimeTree.SetSubtreeAttrib(a, a.NodeIndex);
        end;
    ReltimeTree.AttribList[0].Font.Size := e.Value;
  end;
  a.Free;
  OriTree.OTU_Font.Size := e.Value;
  if IsBootTree then
    BootTree.OTU_Font.Size := e.Value;
  if IsReltimeTree then
    ReltimeTree.OTU_Font.Size := e.Value;
  Tree.Invalidate;
  DataSaved := False;
end;

procedure TTreeViewForm.CaptionToggleDisplayNotify(c: TObject);
var
  cb: TCheckBox = nil;
begin
  if FCheckStateChanging then
    Exit;
  ValidateCheckbox(c);
  cb := TCheckBox(c);
  if cb.Checked  and (not CaptionPanel.Visible) then
    CaptionDockInTEActionExecute(c)
  else
    CaptionHideActionExecute(c);
end;

procedure TTreeViewForm.GeneDupsDisplaySourceNotify(r: TObject);
begin
  if FCheckStateChanging then
    Exit;
  if (FGeneDuplicationsFrame.DisplaySequenceNamesBtn = r) or (FGeneDuplicationsFrame.DisplaySpeciesNamesBtn = r) then
  begin
    try
      FCheckStateChanging := True;
      if FGeneDuplicationsFrame.DisplayNameIsSequenceName then
      begin
        Tree.ShowOTUName := True;
        Tree.ShowSpeciesName := False;
      end
      else if FGeneDuplicationsFrame.DisplayNameIsSpeciesName then
      begin
        Tree.ShowOTUName := False;
        Tree.ShowSpeciesName := True;
      end
      else
        ShowMessage('Application Error: missing taxa name display source');
      if Tree.ShowOTUName or Tree.ShowSpeciesName then
        FTaxaNamesFrame.ShowTaxaNamesCheckBx.Checked := True;
    finally
      FCheckStateChanging := False;
    end;
    Tree.Invalidate;
    Exit;
  end;

  try
    FCheckStateChanging := True;
    ValidateRadioButton(r);
    if FGeneDuplicationsFrame.DisplayNameIsSequenceName then
      DisplayTaxaNamesActionExecute(r)
    else
      ActionDisplaySpeciesNamesExecute(r);
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.InitTreeInfoFrame;
begin
  FTreeInfoFrame := TTreeInfoFrame.Create(Self);
  FrameUtils.InitFrame(FTreeInfoFrame as ITreeToolbarFrame, InfoDisplay, True);
  FTreeInfoFrame.Initialize;
  FTreeInfoFrame.GetHeaderPanel.Visible := False;
  FTreeInfoFrame.Align := alClient;
  InfoDisplay.Panel1.Top := -1;
  InfoDisplay.InfoFrame := FTreeInfoFrame;
end;

procedure TTreeViewForm.InitCollapseNodesFrame;
var
  expanded: Boolean = False;
begin
  expanded := (Tree = OriTree) and (Tree.NumGroups > 1);
  FCollapseNodesFrame := TCollapseNodesFrame.Create(Self);
  FrameUtils.InitFrame(FCollapseNodesFrame as ITreeToolbarFrame, SidePanel, expanded);
  FCollapseNodesFrame.CollapseBranchesCutoffSpinEdit.Increment := CalculateIncrement(Tree.SmallestPositiveBranchLength);
  FCollapseNodesFrame.CollapseBranchesCutoffSpinEdit.Value := Tree.SmallestPositiveBranchLength;
  FCollapseNodesFrame.ClusterSizeSpinEdit.Value := CalculateCollapseClusterSizeInitialValue(Tree.NoOfOtus);
  FCollapseNodesFrame.ClusterSizeSpinEdit.Increment := CalculateCollapseClusterSizeIncrement(Tree.NoOfOtus);
  FCollapseNodesFrame.ClusterSizeSpinEdit.MaxValue := MaxInt;
  FCollapseNodesFrame.CollapseNodesChbx.OnChange := CollapsePanelsChbxNotify;
  FCollapseNodesFrame.CollapseGroupsBtn.OnChange := CollapsePanelsTargetNotify;
  FCollapseNodesFrame.CollapseSizeBtn.OnChange := CollapsePanelsTargetNotify;
  FCollapseNodesFrame.CollapseByDiffBtn.OnChange := CollapsePanelsTargetNotify;
  FCollapseNodesFrame.CollapseBranchesCutoffSpinEdit.OnEditingDone := CollapsePanelsTargetNotify;
  FCollapseNodesFrame.ClusterSizeSpinEdit.OnChange := CollapsePanelsTargetNotify;
  FCollapseNodesFrame.ShowClusterSizeCheckbox.OnChange := CollapsePanelsChbxNotify;
end;

procedure TTreeViewForm.InitGeneDuplicationsFrame;
begin
  FGeneDuplicationsFrame := TGeneDuplicationsFrame.Create(Self);
  FrameUtils.InitFrame(FGeneDuplicationsFrame as ITreeToolbarFrame, SidePanel, Tree.IsGeneDups);
  FGeneDuplicationsFrame.ShowGeneDupsChbx.OnChange := ActionDisplayGeneDupMarkersExecute;
  FGeneDuplicationsFrame.ShowSpeciationsChbx.OnChange := ActionDisplaySpeciationMarkersExecute;
  FGeneDuplicationsFrame.DisplaySequenceNamesBtn.OnChange := GeneDupsDisplaySourceNotify;
  FGeneDuplicationsFrame.DisplaySpeciesNamesBtn.OnChange := GeneDupsDisplaySourceNotify;
  FGeneDuplicationsFrame.SearchBtn.OnClick := ActionSearchGeneDupsExecute;
end;

procedure TTreeViewForm.InitCaptionFrame;
begin
  FCaptionFrame := TCaptionFrame.Create(Self);
  FrameUtils.InitFrame(FCaptionFrame as ITreeToolbarFrame, SidePanel, False);
  FCaptionFrame.DisplayCaptionCheckBox.OnChange := CaptionToggleDisplayNotify;
  FCaptionFrame.CaptionZoomInBtn.Action := CaptionZoomInAction;
  FCaptionFrame.CaptionZoomOutBtn.Action := CaptionZoomOutAction;
  FCaptionFrame.CaptionPrintBtn.Action := CaptionPrintAction;
  FCaptionFrame.CaptionClipBoardBtn.Action := CaptionCopyToClipboardAction;
  FCaptionFrame.CaptionTextBtn.Action := CaptionSaveToTextAction;
  FCaptionFrame.CaptionNewWindowBtn.Action := CaptionShowInNewWindowAction;
end;

procedure TTreeViewForm.InitMultiTreesFrame;
begin
  FMultiTreesFrame := TMultiTreesFrame.Create(Self);
  FrameUtils.InitFrame(FMultiTreesFrame as ITreeToolbarFrame, SidePanel, False);
  FMultiTreesFrame.TreeNumSEdit.MaxValue := Tree.NoOfTrees;
  FMultiTreesFrame.TreeNumSEdit.Value := 1;
  FMultiTreesFrame.NumTreesLabel.Caption := Format('out of %d trees', [Tree.NoOfTrees]);
  FMultiTreesFrame.TreeNumSEdit.OnChange := TreeNumSEditChange;
  FMultiTreesFrame.Visible := (Tree.NoOfTrees > 1);
end;

procedure TTreeViewForm.InitDivTimesFrame;
begin
  FDivTimesFrame := TDivergenceTimesFrame.Create(Self);
  FrameUtils.InitFrame(FDivTimesFrame as ITreeToolbarFrame, SidePanel, Assigned(ReltimeTree) and (ReltimeTree.NoOfTrees > 0));

  FDivTimesFrame.DivTimesDisplayCheckBx.OnChange := DivTimesDisplayNotify;
  FDivTimesFrame.FontBtn.OnClick := ActionTimesFontExecute;
  FDivTimesFrame.DivTimesFontSizeSpinEdit.OnChange := DivTimesFontSizeNotify;
  FDivTimesFrame.DivTimesPrecisionSpinEdit.OnChange := DivTimesPrecisionNotify;
  FDivTimesFrame.HideOverlappingDivTimesCheckBx.OnChange := ActionHideOverlappingDivTimesExecute;
  FDivTimesFrame.ShowNodeHeightErrBarsCheckBx.OnChange := TimescaleHeightErrBarsNotify;
  FDivtimesFrame.Enabled := (TreeTabControl.Tabs.IndexOf(TIME_TREE_TAB) >= 0);
end;

procedure TTreeViewForm.InitTaxaNamesFrame;
begin
  FTaxaNamesFrame := TTaxaNamesFrame.Create(Self);
  FrameUtils.InitFrame(FTaxaNamesFrame as ITreeToolbarFrame, SidePanel, False);
  FTaxaNamesFrame.TaxaNameFontBtn.OnClick := ActionTaxonFontExecute;
  FTaxaNamesFrame.TaxaFontSizeSpinEdit.OnChange := TaxaFontSizeChange;
  FTaxaNamesFrame.ShowTaxaNamesCheckBx.OnChange := ToggleTaxaNamesNotify;
  FTaxaNamesFrame.HideOverlappingTaxaCheckBx.OnChange := HideOverlappingTaxaNotify;
  FTaxaNamesFrame.ShowNodeMarkersCheckBx.OnChange := MarkerDisplayChangedNotify;
  FTaxaNamesFrame.EditMarkersBtn.OnClick := EditMarkersNotify;
  FTaxaNamesFrame.BalancedShapeRadioBtn.OnChange := ArrangeTaxaNotify;
  FTaxaNamesFrame.InputOrderRadioBtn.OnChange := ArrangeTaxaNotify;
end;

procedure TTreeViewForm.InitBLensFrame;
begin
  FBLensFrame := TBranchLengthsFrame.Create(Self);
  FrameUtils.InitFrame(FBLensFrame as ITreeToolbarFrame, SidePanel, False);
  FBLensFrame.Enabled := ActionBranchLengthDisplay.Enabled;
  FBLensFrame.BLensFontBtn.OnClick := ActionBLenFontExecute;
  FBLensFrame.BLensCheckBx.OnChange := ToggleBLensNotify;
  FBLensFrame.BLensHideIfShorterSpinEdit.OnChange := HideBLensShorterThanNotify;
  FBLensFrame.BlensFontSizeSpinEdit.OnChange := BLensFontSizeNotify;
  FBLensFrame.BLensHideIfShorterCheckBx.OnChange := HideBlensShortThanCheckBoxNotify;
  FBLensFrame.BLensPlacementComboBx.OnChange := BLensPlacementNotify;
  FBLensFrame.BLensPrecisionSpinEdit.OnChange := BLensPrecisionNotify;
end;

procedure TTreeViewForm.InitDistScaleFrame;
begin
  FDistScaleFrame := TDistanceScaleFrame.Create(Self);
  FrameUtils.InitFrame(FDistScaleFrame as ITreeToolbarFrame, SidePanel, (Tree = OriTree) and (Tree.HasNonZeroBranchLength));
  FDistScaleFrame.DistanceScaleCheckBx.OnChange := DistScaleDisplayNotify;
  FDistScaleFrame.LineWidthComboBx.OnChange := DistScaleLineWidthNotify;
  FDistScaleFrame.DistScaleFontBtn.OnClick := ActionScaleBarFontExecute;
  FDistScaleFrame.DistScaleFontSizeSpinEdit.OnChange := DistScaleFontSizeNotify;
  FDistScaleFrame.CaptionEdit.OnEditingDone := DistScaleCaptionEditNotify;
  FDistScaleFrame.DistScaleLengthSpinEdit.OnChange := DistScaleLengthNotify;
  FDistScaleFrame.DistScaleTickIntervalSpinEdit.OnChange := DistScaleTickIntervalNotify;
end;

procedure TTreeViewForm.InitDistScaleSpinEdits(var scaleEdit: TFloatSpinEdit; var tickEdit: TSpinEdit);
var
  aMin, aMax, aVal, aStep: Double;
begin
  if Tree.isScale then
  begin
    if Tree.TreeStyle = tsCircle then
      aMax := Tree.LongestPath*1.1
    else
      aMax := Tree.LongestPath;
    aMin := power(10, floor(log10(aMax/100)));
    aVal := StrToFloat(Tree.ScaleText);
    if Pos(FormatSettings.DecimalSeparator, Tree.ScaleText) = 0 then
      aStep := 1
    else
      aStep := Max(0.01, Power(10, Pos(FormatSettings.DecimalSeparator, Tree.ScaleText)-Length(Tree.ScaleText)));
    scaleEdit.MinValue := aMin;
    scaleEdit.MaxValue := aMax;
    scaleEdit.Increment := aStep;
    scaleEdit.Value := aVal;
    if Tree.IsLinearized or ToggleTimeScaleAction.Enabled then
    begin
      aMax := aVal;
      aVal := Tree.ScaleTick;
      tickEdit.MinValue := aMin;
      tickEdit.MaxValue := aMax;
      tickEdit.Increment := aStep;
      tickEdit.Value := aVal;
    end;

  end;
end;

procedure TTreeViewForm.InitTimeScaleFrame;
begin
  FTimeScaleFrame := TTimeScaleFrame.Create(Self);
  FrameUtils.InitFrame(FTimeScaleFrame as ITreeToolbarFrame, SidePanel, Assigned(ReltimeTree) and (ReltimeTree.NoOfTrees > 0));
  FTimeScaleFrame.TimescaleFontBtn.OnClick := ActionScaleBarFontExecute;
  FTimeScaleFrame.TimescaleCheckBx.OnChange := TimescaleDisplayNotify;
  FTimeScaleFrame.TimeScaleLineWidthComboBox.OnChange := TimescaleLineWidthNotify;
  FTimeScaleFrame.TimescaleFontSizeSpinEdit.OnChange := TimescaleFontSizeNotify;
  FTimeScaleFrame.TimescaleCaptionEdit.OnEditingDone := TimescaleCaptionEditNotify;
  FTimeScaleFrame.TimescaleMinorTickSpinEdit.OnChange := TimescaleMinorTickNotify;
  FTimeScaleFrame.TimeMajorTickSpinEdit.MaxValue := MaxDouble;
  FTimeScaleFrame.TimeMajorTickSpinEdit.OnChange := TimescaleMajorTickNotify;
end;

procedure TTreeViewForm.InitNodeStatsFrame;
begin
  FNodeStatsFrame := TNodeStatsFrame.Create(Self);
  FrameUtils.InitFrame(FNodeStatsFrame as ITreeToolbarFrame, SidePanel, Assigned(BootTree) and ((BootTree.NoOfTrees > 0) or BootTree.isConsensus));
  if IsBootTree then
    FNodeStatsFrame.BootstrapFrequencyBtn.Checked := Tree.ShowStats;
  FNodeStatsFrame.ShowStatsCheckBx.OnChange := ToggleStatsNotify;
  FNodeStatsFrame.StatsPlacementComboBox.OnChange := StatsPlacementNotify;
  FNodeStatsFrame.StatsFontBtn.OnClick := ActionBranchInfoFontExecute;
  FNodeStatsFrame.StatsFontSizeSpinEdit.OnChange := StatsFontSizeNotify;
  FNodeStatsFrame.StatsHideIfLowerThanCheckBx.OnChange := StatsHideShorterThanNotify;
  FNodeStatsFrame.StatsHideIfLowerThanSpinEdit.OnChange := StatsHideShorterThanSpinEditNotify;
  FNodeStatsFrame.StatsHorizDistanceSpinEdit.OnChange := StatsHorizDistNotify;
  FNodeStatsFrame.StatsVertDistanceSpinEdit.OnChange := StatsVertDistNotify;
  FNodeStatsFrame.DataCoverageBtn.OnChange := DisplayTypeChange;
  FNodeStatsFrame.BootstrapFrequencyBtn.OnChange := DisplayTypeChange;
  FNodeStatsFrame.NodeIdsBtn.OnChange := DisplayTypeChange;
end;

procedure TTreeViewForm.InitLayoutFrame;
begin
  FLayoutFrame := TLayoutFrame.Create(Self);
  FrameUtils.InitFrame(FLayoutFrame as ITreeToolbarFrame, SidePanel, False);
  FLayoutFrame.TreeStyleBtn.OnClick := TreeStyleButtonClick;
  FLayoutFrame.TopologyOnlyBtn.Action := ActionTopology;
  FLayoutFrame.AutoSizeBtn.Action := ActionAutoSize;
  FLayoutFrame.DragResizeBtn.Action := ActionResize;
  FLayoutFrame.RootOnMidpointBtn.Action := ActionRootOnMidpoint;
  FLayoutFrame.RootOnOutgroupBtn.Action := ActionRootByOutgroup;
  FLayoutFrame.TreeHeightTrackBar.OnChange := LayoutTreeHeightNotify;
  FLayoutFrame.TreeWidthTrackBar.OnChange := LayoutTreeWidthNotify;
end;

procedure TTreeViewForm.InitSubtreeFrame;
begin
  FSubtreeFrame := TSubtreeFrame.Create(Self);
  FrameUtils.InitFrame(FSubtreeFrame as ITreeToolbarFrame, SidePanel, False);
  FSubtreeFrame.SubtreePointerBtn.Action := ActionPoint;
  FSubtreeFrame.DisplaySubtreeBtn.Action := ActionDisplaySeparately;
  FSubtreeFrame.FormatSubtreeBtn.Action := ActionFormattingTools;
  FSubtreeFrame.SwapSubtreeBtn.Action := ActionSwap;
  FSubtreeFrame.FlipSubtreeBtn.Action := ActionFlip;
  FSubtreeFrame.RootTreeBtn.Action := ActionRootOnBranch;
  FSubtreeFrame.CompressSubtreeBtn.Action := ActionCompress;
  FSubtreeFrame.UseSubtreeOptionsCheckBx.OnChange := SubtreeUseSubtreeAttribsNotify;
  FSubtreeFrame.UseGroupOptionsCheckBx.OnChange := SubtreeUseGroupAttribsNotify;
end;

procedure TTreeViewForm.InitComputeFrame;
begin
  FComputeFrame := TComputeFrame.Create(Self);
  FrameUtils.InitFrame(FComputeFrame as ITreeToolbarFrame, SidePanel, False);
  FComputeFrame.ConsensusTreeBtn.OnClick := ComputeConsensusNotify;
  FComputeFrame.CondensedTreeBtn.OnClick := ComputeCondensedNotify;
  if Tree = ReltimeTree then
  begin
    FComputeFrame.ComputeTimetreeBtn.Action := ActionRecalibrateTimeTree;
    FComputeFrame.OperationLabel.Caption := 'Recalibrate Tree...';
  end
  else
    FComputeFrame.ComputeTimetreeBtn.Action := ActionReltime;
  FComputeFrame.CalibrateClockBtn.Action := ActionMolecularClock;
  FComputeFrame.ConsensusTreeCutoffSpinEdit.OnChange := ConsensusCutoffChangedNotify;
  FComputeFrame.CondensedTreeCutoffSpinEdit.OnChange := CondensedCutoffChangedNotify;
  FComputeFrame.CorrTestButton.Action := CorrtestAction;
end;

procedure TTreeViewForm.InitAncestorsFrame;
begin
  FAncestorsFrame := TAncestorsFrame.Create(Self);
  FrameUtils.InitFrame(FAncestorsFrame as ITreeToolbarFrame, SidePanel, (Tree = OriTree) and (Assigned(MLAnalyzer) or Assigned(ComputeParsimInfo)));
  FAncestorsFrame.PrevChangeBtn.Enabled := Assigned(MLAnalyzer);
  FAncestorsFrame.NextChangeBtn.Enabled := Assigned(MLAnalyzer);
  FAncestorsFrame.AncSiteNumSpinEdit.MaxValue := Tree.MaxSiteIndex;
  FAncestorsFrame.AncSiteNumSpinEdit.OnChange := SiteNumSEditChange;
  FAncestorsFrame.AncShowAllRadioBtn.OnChange := AncRadioGroupNotify;
  if Assigned(MLAnalyzer) then
    FAncestorsFrame.AncMostProbableRadioBtn.OnChange := AncRadioGroupNotify
  else
    FAncestorsFrame.AncMostProbableRadioBtn.Enabled := False;
  FAncestorsFrame.AncHideAmbiguousRadioBtn.OnChange := AncRadioGroupNotify;
  FAncestorsFrame.AncShowNoneRadioBtn.OnChange := AncRadioGroupNotify;
  FAncestorsFrame.AncExtendedCharsCheckBx.OnChange := AncExtCharNotfity;
  FAncestorsFrame.AncFontBtn.OnClick := AncFontNotify;
  FAncestorsFrame.AncStateFontSizeSpinEdit.OnChange := AncFontSizeNotify;
  FAncestorsFrame.AncExportSpeedBtn.OnClick := AncExportBtnNotify;
  if not isAncState then
    FAncestorsFrame.Visible := False;
  if Assigned(MLAnalyzer) or Assigned(FComputeParsimInfo) then
  begin
    FAncestorsFrame.PrevChangeBtn.OnClick := PrevAncestralStateChangeNotify;
    FAncestorsFrame.NextChangeBtn.OnClick := NextAncestralStateChangeNotify;
  end;
  try
    FCheckStateChanging := True;
    UpdateAncestralStateActions;
  finally
    FCheckStateChanging := False;
  end;
end;

function TTreeViewForm.GetNodeAttribValsFromProcessMessage(NodeAttrib: TNodeAttrib; const message: ICefProcessMessage; var nodeID: Integer): Boolean;
var
  image: TImage = nil;
  stream: TMemoryStream = nil;
  len: Integer = -1;
  temp: String;
  subtreeCaptionFont: TFont = nil;
  subtreeDisplayFont: TFont = nil;
  tempFileName: String = '';
begin
  Result := False;
  try
    try
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_CAPTION_FONT_INDEX);
      subtreeCaptionFont := TFont.Create;
      if Trim(temp) <> EmptyStr then
        if UpdateFontFromCssString(subtreeCaptionFont, temp) then
          NodeAttrib.CaptionFont.Assign(subtreeCaptionFont)
        else
          raise Exception.Create('Application Error: failed to update caption font');

      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_DISPLAY_FONT_INDEX);
      subtreeDisplayFont := TFont.Create;
      if Trim(temp) <> EmptyStr then
        if UpdateFontFromCssString(subtreeDisplayFont, temp) then
          NodeAttrib.Font.Assign(subtreeDisplayFont)
        else
          raise Exception.Create('Application Error: failed to update subtree font');
      NodeAttrib.Name := message.ArgumentList.GetString(SUBTREE_OPTIONS_GROUP_NAME_INDEX);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_NODE_ID_INDEX);
      nodeID := StrToInt(temp);
      NodeAttrib.Caption := message.ArgumentList.GetString(SUBTREE_OPTIONS_NAME_CAPTION_INDEX);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_NODE_SHAPE_INDEX);
      NodeAttrib.Marker.Shape := HtmlStringToTNodeMarkerShape(temp);
      temp := message.ArgumentList.GetString(SUBTREE_COLOR1_INDEX);
      NodeAttrib.Marker.Color := SHtmlColorToColor(temp, len, clBlack);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_APPLY_TO_TAXON_MARKERS_INDEX);
      NodeAttrib.OverwriteMarker := StrToBool(temp);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_BRANCH_LINES_INDEX);
      NodeAttrib.BranchOption := StringToBranchOption(temp);
      temp := message.ArgumentList.GetString(SUBTREE_COLOR2_INDEX);
      NodeAttrib.LineColor := SHtmlColorToColor(temp, len, clBlack);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_BRANCH_WIDTH_INDEX);
      NodeAttrib.LineWidth := StrToInt(temp);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_BRANCH_STYLE_INDEX);
      NodeAttrib.LineStyle := HtmlLineTypeStrToPenStyle(temp);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_DISPLAY_CAPTION_INDEX);
      NodeAttrib.ShowCaption := StrToBool(temp);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_ALIGN_VERT_INDEX);
      Tree.AlignCaption := StrToBool(temp);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_DISPLAY_BRACKET_INDEX);
      NodeAttrib.ShowBracket := StrToBool(temp);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_BRACKET_STYLE_INDEX);
      NodeAttrib.BracketStyle := StringToBracketStyle(temp);
      temp := message.ArgumentList.GetString(SUBTREE_COLOR3_INDEX);
      NodeAttrib.BracketColor := SHtmlColorToColor(temp, len, clBlack);
      temp := message.ArgumentList.GetString(SUBTREE_DRAWING_OPTIONS_LINE_WIDTH_INDEX);
      NodeAttrib.BracketLineWidth := StrToInt(temp);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_DISPLAY_TAXON_NAME_INDEX);
      NodeAttrib.ShowTaxonName := StrToBool(temp);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_DISPLAY_NODE_INDEX);
      NodeAttrib.ShowNodeMarker := StrToBool(temp);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_DISPLAY_TAXON_MARKER_INDEX);
      NodeAttrib.ShowTaxonMarker := StrToBool(temp);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_COMPRESS_INDEX);
      NodeAttrib.ManualCompressed := StrToBool(temp);
      temp := message.ArgumentList.GetString(SUBTREE_DRAWING_OPTIONS_VERTICAL_UNIT_INDEX);
      Tree.PixelsPerGroupMember := StrToInt(temp);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_FILL_PATTERN_INDEX);
      NodeAttrib.FillStyle := StringToBrushStyle(temp);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_DISPLAY_IMAGE_INDEX);
      NodeAttrib.ShowImage := StrToBool(temp);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_STYLE_OPTIONS_INDEX);
      NodeAttrib.GraphicAlign := StringToGraphicAlign(temp);
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_OVERWRITE_INDEX);
      NodeAttrib.OverwriteDownstream := StrToBool(temp);
      temp := message.ArgumentList.GetString(SUBTREE_COLOR4_INDEX);
      NodeAttrib.Font.Color := SHtmlColorToColor(temp, len, clBlack);

      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_IMAGE_PATH_INDEX);
      if Trim(temp) <> EmptyStr then
      begin
       Image  := TImage.Create(Self);
       Stream := TMemoryStream.Create;
       Base64ToStream(temp, Stream);
       Image.Picture.LoadFromStream(Stream);
       TempFileName := getTemp + 'temp_image.bmp';
       Image.Picture.Bitmap.SaveToFile(TempFileName);
       If not Image.Picture.Bitmap.Empty then
       begin
           NodeAttrib.ShowImage:=true;
           NodeAttrib.Image.Assign(Image.Picture.Bitmap);
       end;
      end;
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_EXPORT_IMAGE_INDEX);
      if temp = 'true' then
      begin
        FileSaveDialog.Filter := 'Bitmap Files|*.bmp';
        if FileSaveDialog.Execute then
          Image.Picture.Bitmap.SaveToFile(ChangeFileExt(FileSaveDialog.FileName, '.bmp'))
      end;
      temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_CLEAR_IMAGE_INDEX);
      if temp = 'true' then
      begin
        NodeAttrib.ClearImage;
      end;
      Result := True;
    except
      on E:Exception do
        ShowMessage('Application error when retrieving node attribute options: ' + E.Message);
    end
  finally
    if Assigned(subtreeCaptionFont) then
      subtreeCaptionFont.Free;
    if Assigned(subtreeDisplayFont) then
      subtreeDisplayFont.Free;
  end;
end;

procedure TTreeViewForm.InitSidePanel;
begin
  if not Assigned(FrameUtils) then
    FrameUtils := TFrameUtils.Create;
  ButtonsPanel.Visible := False;
  Panel2.Align := alTop;
  InitGeneDuplicationsFrame;
  InitCaptionFrame;
  InitComputeFrame;
  InitCollapseNodesFrame;
  InitAncestorsFrame;
  InitTimeScaleFrame;
  InitDivTimesFrame;
  InitDistScaleFrame;
  InitNodeStatsFrame;
  InitBLensFrame;
  InitSubtreeFrame;
  InitLayoutFrame;
  InitTaxaNamesFrame;
  InitMultiTreesFrame;
  ButtonsPanel.Top := -1;
  Panel2.Top := -2; { a hack to force it to the top}
  FSidePanelInitialized := True;
end;

procedure TTreeViewForm.UpdateSidePanel;
begin
  if not FSidePanelInitialized then
    Exit;
  FMultiTreesFrame.Enabled := Tree.NoOfTrees > 1;
  FDivTimesFrame.Enabled := (Tree = ReltimeTree);
  try
    FCheckStateChanging := True;
    FTaxaNamesFrame.TaxaFontSizeSpinEdit.Value := abs(Tree.OTU_Font.Size);
    FTaxaNamesFrame.BalancedShapeRadioBtn.Checked := ActionBalancedTree.Checked;
    FTaxaNamesFrame.InputOrderRadioBtn.Checked := ActionInputOrderTree.Checked;
    FBLensFrame.BlensFontSizeSpinEdit.Value := Tree.BLensFont.Size;
    FBLensFrame.BLensPrecisionSpinEdit.Value := Tree.BLenDecimals;
    FBLensFrame.BLensHideIfShorterCheckBx.Checked := (Tree.BLenCutoff > Tree.MinBranchLength);
    FBLensFrame.BLensHideIfShorterSpinEdit.Value := Tree.BLenCutoff;
    FBLensFrame.BLensHideIfShorterSpinEdit.Increment := CalculateIncrement(Tree.MinBranchLength);
    FBLensFrame.BLensPlacementComboBx.ItemIndex := Ord(Tree.BLenPosition);
    FBLensFrame.BLensCheckBx.Checked := Tree.ShowBLen;

    FDistScaleFrame.DistanceScaleCheckBx.Checked := Tree.ShowScale;
    FDistScaleFrame.DistScaleFontSizeSpinEdit.Value := Tree.ScaleFont.Size;
    FDistScaleFrame.LineWidthComboBx.ItemIndex := Tree.ScalePen.Width - 1;
    FDistScaleFrame.CaptionEdit.Text := Tree.ScaleUnit;
    FDistScaleFrame.Enabled := (Tree <> ReltimeTree) and (not FJustRootingATree);
    if FDistScaleFrame.Enabled then
      InitDistScaleSpinEdits(FDistScaleFrame.DistScaleLengthSpinEdit, FDistScaleFrame.DistScaleTickIntervalSpinEdit);

    FTimeScaleFrame.TimescaleCheckBx.Checked := Tree.ShowTimeScale;
    FTimeScaleFrame.TimeScaleLineWidthComboBox.ItemIndex := ReltimeTree.ScalePen.Width - 1;
    FTimeScaleFrame.TimescaleCaptionEdit.Text := ReltimeTree.TimeUnit;
    FTimeScaleFrame.TimescaleMinorTickSpinEdit.Value := ReltimeTree.TimeTick;
    FTimeScaleFrame.TimeMajorTickSpinEdit.Value := Abs(ReltimeTree.TimeScale);
    FTimeScaleFrame.Enabled := (Tree = ReltimeTree);

    FNodeStatsFrame.ShowStatsCheckBx.Checked := (Tree.ShowStats or Tree.ShowNodeIds or Tree.ShowDataCoverage);
    FNodeStatsFrame.StatsFontSizeSpinEdit.Value := Tree.StatsFont.Size;
    FNodeStatsFrame.StatsHorizDistanceSpinEdit.Value := Tree.StatsMargin.X;
    FNodeStatsFrame.StatsVertDistanceSpinEdit.Value := Tree.StatsMargin.Y;
    FNodeStatsFrame.StatsPlacementComboBox.ItemIndex := Ord(Tree.StatsPosition);
    FNodeStatsFrame.StatsHideIfLowerThanCheckBx.Checked := (Tree.StatsCutoff > 0);
    FNodeStatsFrame.StatsHideIfLowerThanSpinEdit.Value := Tree.StatsCutoff;
    FNodeStatsFrame.BootstrapFrequencyBtn.Checked := Tree.ShowStats;
    FNodeStatsFrame.NodeIdsBtn.Checked := Tree.ShowNodeIds;
    FNodeStatsFrame.DataCoverageBtn.Checked := Tree.ShowDataCoverage;
    if (not FNodeStatsFrame.BootstrapFrequencyBtn.Enabled) and (not FNodeStatsFrame.DataCoverageBtn.Enabled) and (not FNodeStatsFrame.ShowStatsCheckBx.Checked) then
      FNodeStatsFrame.NodeIdsBtn.Checked;

    FSubtreeFrame.UseSubtreeOptionsCheckBx.Checked := Tree.UseSubtreeAttrib;
    FSubtreeFrame.UseGroupOptionsCheckBx.Checked := Tree.UseGroupAttrib;

    FComputeFrame.ConsensusTreeCutoffSpinEdit.Value := Tree.ConsensusValue;
    FComputeFrame.CondensedTreeCutoffSpinEdit.Value := Tree.CondenseValue;

    FAncestorsFrame.AncSiteNumSpinEdit.Value := Tree.SiteIndex;
    UpdateAncestorRadioBtns;
    if ActionAncStateExtended.Checked then
      FAncestorsFrame.AncExtendedCharsCheckBx.Checked;
    FAncestorsFrame.AncStateFontSizeSpinEdit.Value := Tree.CharStateFont.Size;
    FMultiTreesFrame.Enabled := (Tree.NoOfTrees > 1);
    if FGeneDuplicationsFrame.Visible then
    begin
      FGeneDuplicationsFrame.ShowSpeciationsChbx.Enabled := Tree.HasSpeciations;
      FGeneDuplicationsFrame.ShowGeneDupsChbx.Enabled := Tree.HasGeneDups;
      FGeneDuplicationsFrame.DisplaySpeciesNamesBtn.Enabled := Tree.HasSpeciesNames;
    end;
    FCollapseNodesFrame.CollapseNodesChbx.Checked := Tree.IsAutoCompressedClusters;
    FCollapseNodesFrame.CollapseGroupsBtn.Checked := Tree.GroupsAreAutoCompressed;
    FCollapseNodesFrame.CollapseSizeBtn.Checked := Tree.ClustersAreAutoCompressed;
    FCollapseNodesFrame.CollapseByDiffBtn.Checked := Tree.SimilarSeqsAreAutoCompressed;
    if Tree = ReltimeTree then
    begin
      FDivTimesFrame.ShowNodeHeightErrBarsCheckBx.Checked := ReltimeTree.ShowHeightErrBar;
      FDivTimesFrame.DivTimesDisplayCheckBx.Checked := ReltimeTree.ShowDivergenceTimes;
      FDivTimesFrame.DivTimesFontSizeSpinEdit.Value := ReltimeTree.TimesFont.Size;
      FDivTimesFrame.DivTimesPrecisionSpinEdit.Value := ReltimeTree.DivTimeDecimals;
      FDivTimesFrame.HideOverlappingDivTimesCheckBx.Checked := ActionHideOverlappingDivTimes.Checked;
    end;
  finally
    FCheckStateChanging := False;
  end;
end;

procedure TTreeViewForm.DisableComputeFrame;
begin
  FComputeFrame.Enabled := False;
end;

procedure TTreeViewForm.ProcessSubtreeOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
var
  temp: ustring;
  NodeAttrib: TNodeAttrib = nil;
  nodeID: Integer = -1;
begin
  try
    try
      HideOptionsDialog;
      temp := message.ArgumentList.GetString(0);
      if Pos(RENDER_PROCESS_ERROR, temp) > 0 then
        raise Exception.Create(temp);
      NodeAttrib := TNodeAttrib.Create;
      Tree.GetSubtreeAttrib(NodeAttrib, Tree.FocusedIndex);
      Result := GetNodeAttribValsFromProcessMessage(NodeAttrib, message, nodeID);
      NodeAttrib.NodeIndex := Tree.FocusedIndex;
      Tree.ClusterName[Tree.FocusedIndex] := NodeAttrib.Caption;
      Tree.SetSubtreeAttrib(NodeAttrib, Tree.FocusedIndex);
      if NodeAttrib.OverwriteDownstream then
        Tree.OverwriteAttribDownstream(Tree.FocusedIndex);
      if NodeAttrib.OverwriteMarker then
        Tree.SetTaxonMarkerOfSubtree(Tree.FocusedIndex, NodeAttrib.Marker);
      Tree.Refresh;
      DataSaved := false;
      UpdateSidePanel;
    except
      on E:Exception do
        ShowMessage('An application error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(NodeAttrib) then
      NodeAttrib.Free;
  end;
end;

procedure TTreeViewForm.ProcessGroupOptionsMessage(const Browser: ICefBrowser;const message: ICefProcessMessage; out Result: Boolean);
var
  NodeAttrib: TNodeAttrib = nil;
  nodeID: Integer = -1;
  temp: String;
begin
  try
    HideOptionsDialog;
    NodeAttrib := TNodeAttrib.Create;
    Result := GetNodeAttribValsFromProcessMessage(NodeAttrib, message, nodeID);
    temp := message.ArgumentList.GetString(SUBTREE_DRAWING_OPTIONS_VERTICAL_UNIT_INDEX);
    Tree.PixelsPerGroupMember := StrToInt(temp);
    temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_ALIGN_VERT_INDEX);
    Tree.AlignCaption := StrToBool(temp);
    Tree.SetGroupAttrib(NodeAttrib, NodeAttrib.Name);
    if NodeAttrib.OverwriteMarker then
      Tree.SetTaxonMarkerOfGroup(NodeAttrib.Name, NodeAttrib.Marker);
    Tree.Refresh;
    DataSaved := False;
    temp := message.ArgumentList.GetString(SUBTREE_OPTIONS_DEFAULTS_INDEX);
    if StrToBool(temp) then
      SaveGroupOption(NodeAttrib);
    UpdateSidePanel;
  finally
    if Assigned(NodeAttrib) then
      NodeAttrib.Free;
  end;
end;

procedure TTreeViewForm.ProcessTreeOptionsMessage(const Browser: ICefBrowser;const message: ICefProcessMessage; out Result: Boolean);
var
  temp: String;
  i, vertPixels, horizPixels, aTreeWidth: Integer;
  HorzFactor: Double;
begin
  DataSaved := False;

  try
    HideOptionsDialog;
    HorzFactor := Power(10, 2-Floor(Log10(Tree.PixelsPerUnit)));
    case Tree.TreeStyle of
      tsTraditional:
        begin
          temp := message.ArgumentList.GetString(TREE_OPTIONS_RECT_TAXON_SEPARATION_INDEX);
          vertPixels := StrToInt(temp);
          temp := message.ArgumentList.GetString(TREE_OPTIONS_RECT_BRANCH_LENGTH_INDEX);
          horizPixels := StrToInt(temp);
          temp := message.ArgumentList.GetString(TREE_OPTIONS_RECT_WIDTH_INDEX);
          aTreeWidth := StrToInt(temp);
          if Tree.ShowTopologyOnly or (not Tree.isBranchLength) then
            Tree.TreeWidth := aTreeWidth
          else
            Tree.PixelsPerUnit := horizPixels/HorzFactor;
          Tree.PixelsPerOTU := vertPixels;
        end;
      tsRadiation:
        begin
          temp := message.ArgumentList.GetString(TREE_OPTIONS_TREE_TAXON_NAME_RAD_INDEX);
          Tree.HorzTaxonName := StrToBool(temp);
          temp := message.ArgumentList.GetString(TREE_OPTIONS_RAD_BRANCH_LENGTH_INDEX);
          horizPixels := StrToInt(temp);
          temp := message.ArgumentList.GetString(TREE_OPTIONS_RAD_START_ANGLE_INDEX);
          Tree.StartAngle := StrToInt(temp);
          Tree.PixelsPerUnit := horizPixels/HorzFactor;
        end;
      tsCircle:
        begin
          temp := message.ArgumentList.GetString(TREE_OPTIONS_TREE_TAXON_NAME_CIRCLE_INDEX);
          Tree.HorzTaxonName := StrToBool(temp);
          temp := message.ArgumentList.GetString(TREE_OPTIONS_CIRCLE_START_ANGLE_INDEX);
          Tree.StartAngle := StrToInt(temp);
          temp := message.ArgumentList.GetString(TREE_OPTIONS_CIRCLE_RADIUS_INDEX);
          Tree.Radius := StrToInt(temp);
          temp := message.ArgumentList.GetString(TREE_OPTIONS_CIRCLE_CENTER_HOLE_INDEX);
          Tree.CenterMargin := StrToInt(temp);
        end;
    end;

    if IsBootTree then
      if Tree = OriTree then
      begin
        BootTree.AssignTreeAttrib(OriTree);
        for i := 1 to Tree.NoOfOTUs do
          BootTree.Marker[i] := OriTree.Marker[i];
      end
      else if Tree = BootTree then
      begin
        OriTree.AssignTreeAttrib(BootTree);
        for i := 1 to Tree.NoOfOTUs do
          OriTree.Marker[i] := BootTree.Marker[i];
      end;
    UpdateInfoBox;
    Tree.Refresh;
    UpdateSidePanel;
  except
  on E:Exception do
    ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TTreeViewForm.ProcessBranchOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
var
  i, tempInt: Integer;
  tempBool: Boolean;
  tempFloat: Double;
  tempMargin: TPoint;
  temp: String;
  a: TNodeAttrib = nil;
  statsFont: TFont = nil;
  blensFont: TFont = nil;
  timesFont: TFont = nil;
begin
  try
    try
      HideOptionsDialog;
      temp := message.ArgumentList.GetString(0);
      if Pos(RENDER_PROCESS_ERROR, temp) > 0 then
        raise Exception.Create(temp);
      temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_LINES_INDEX);
      tempInt := StrToInt(temp);
      a := TNodeAttrib.Create;
      if Tree.GroupAttrib.Count > 0 then
        for i := 0 to Tree.GroupAttrib.Count-1 do
          if Tree.GroupAttrib[i].LineWidth = Tree.BranchPen.Width then
          begin
            a.Assign(Tree.GroupAttrib[i]);
            a.LineWidth := tempInt;
            Tree.SetGroupAttrib(a, a.Caption);
          end;
      if Tree.AttribList.Count > 1 then
        for i := 1 to Tree.AttribList.Count-1 do
          if Tree.AttribList[i].LineWidth = Tree.BranchPen.Width then
          begin
            a.Assign(Tree.AttribList[i]);
            a.LineWidth := tempInt;
            Tree.SetSubtreeAttrib(a, a.NodeIndex);
          end;

      Tree.BranchPen.Width := tempInt;
      Tree.AttribList[0].LineWidth := Tree.BranchPen.Width;

      if Tree.isStats then
      begin
        temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_DISPLAY_STATS_INDEX);
        tempBool := StrToBool(temp);
        if tempBool then
          Tree.ShowStats := true
        else
          Tree.ShowStats := false;

        temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_HIDE_LOWER_INDEX);
        tempBool := StrToBool(temp);
        temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_HIDE_VALUES_INDEX);
        tempInt := StrToInt(temp);
        if tempBool then
          Tree.StatsCutoff := tempInt
        else
          Tree.StatsCutoff := 0;

        temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_STAT_PLACEMENT_INDEX);
        tempInt := StrToInt(temp);
        Tree.StatsPosition := TBranchInfoPosition(tempInt);
        temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_HORIZONTAL_INDEX);
        tempInt := StrToInt(temp);
        TempMargin.X := tempInt;
        temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_VERTICAL_INDEX);
        tempInt := StrToInt(temp);
        TempMargin.Y := tempInt;
        Tree.StatsMargin := TempMargin;
        temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_STATISTICS_FONT_INDEX);
        statsFont := TFont.Create;
        statsFont.Assign(Tree.StatsFont);
        if Trim(temp) <> EmptyStr then
          if UpdateFontFromCssString(statsFont, temp) then
            Tree.StatsFont.Assign(statsFont)
          else
            ShowMessage('Oops! Failed to retrieve the selected font settings. Please report this bug to the developers of MEGA');
      end;

      if Tree.IsTimes then
      begin
        temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_DISPLAY_DIVERGENCE_INDEX);
        tempBool := StrToBool(temp);
        if tempBool then
        begin
          Tree.ShowDivergenceTimes := True;
          ActionDisplayDivergenceTimes.Checked := True;
          FigureGenerator.AssignData(DISP_DIV_TIMES_STR, 'True');
        end
        else
        begin
          Tree.ShowDivergenceTimes := False;
          ActionDisplayDivergenceTimes.Checked := False;
          FigureGenerator.AssignData(DISP_DIV_TIMES_STR, 'False')
        end;
        temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_TIME_PLACEMENT_INDEX);
        tempInt := StrToInt(temp);
        Tree.TimesPosition := TBranchInfoPosition(tempInt);
        temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_DIVERGENCE_PRECISION_INDEX);
        tempInt := StrToInt(temp);
        Tree.DivTimeDecimals := tempInt;
        temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_DISTANCE_HORIZONTAL_INDEX);
        tempInt := StrToInt(temp);
        TempMargin.X := tempInt;
        temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_DISTANCE_VERTICAL_INDEX);
        tempInt := StrToInt(temp);
        TempMargin.Y := tempInt;
        Tree.TimesMargin := TempMargin;
        temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_DIVERGENCE_FONT_INDEX);
        timesFont := TFont.Create;
        timesFont.Assign(Tree.TimesFont);
        if Trim(temp) <> EmptyStr then
          if UpdateFontFromCssString(timesFont, temp) then
            Tree.TimesFont.Assign(timesFont)
          else
            ShowMessage('Oops! Failed to retrieve the selected font settings. Please report this bug to the developers of MEGA');
      end;

      if Tree <> BootTree then
      begin
        if Tree.isBranchLength then
        begin
          temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_DISPLAY_BRANCH_INDEX);
          tempBool := StrToBool(temp);
          if tempBool then
            Tree.ShowBLen := true
          else
            Tree.ShowBLen := false;
          temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_HIDE_SHORTER_INDEX);
          tempBool := StrToBool(temp);
          temp := message.ArgumentList.GetString(TREE_OPTIONS_HIDE_SHORTER_INDEX);
          tempFloat := StrToFloat(temp);
          if tempBool then
            Tree.BLenCutoff := tempFloat
          else
            Tree.BLenCutoff := Tree.MinBranchLength;
          temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_BRANCH_PLACEMENT_INDEX);
          tempInt := StrToInt(temp);
          Tree.BLenPosition := TBranchInfoPosition(tempInt);
          temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_LENGTH_FONT_INDEX);
          blensFont := TFont.Create;
          blensFont.Assign(Tree.BLensFont);
          if Trim(temp) <> EmptyStr then
            if UpdateFontFromCssString(blensFont, temp) then
              Tree.BLensFont.Assign(blensFont)
            else
              ShowMessage('Oops! Failed to retrieve the selected font settings. Please report this bug to the developers of MEGA');
        end;
        temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_PRECISION_INDEX);
        tempInt := StrToInt(temp);
        if Tree.isBranchLength then
          Tree.BLenDecimals := tempInt;
        if IsBootTree and BootTree.isBranchLength then
          BootTree.BLenDecimals := tempInt;
      end;
      UpdateInfoBox;
      Tree.Refresh;
      UpdateSidePanel;
    except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(a) then
      a.Free;
    if Assigned(statsFont) then
      statsFont.Free;
    if Assigned(timesFont) then
      timesFont.Free;
    if Assigned(blensFont) then
      blensFont.Free;
  end;
end;

procedure TTreeViewForm.ProcessScaleOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
var
  temp, scaleText, scaleUnit: String;
  timeText, timeUnit: String;
  showScale, showTimeScale, showErrBars: Boolean;
  lineWidth: Integer;
  scaleTick, timeTick: Double;
  aFont: TFont = nil;
begin
  try
    try
      HideOptionsDialog;
      temp := message.ArgumentList.GetString(0);
        if Pos(RENDER_PROCESS_ERROR, temp) > 0 then
          raise Exception.Create(temp);
      temp := message.ArgumentList.GetString(TREE_OPTIONS_SCALE_LINES_INDEX);
      lineWidth := StrToInt(temp);
      temp := message.ArgumentList.GetString(TREE_OPTIONS_SCALE_DISTANCE_SCALE_INDEX);
      showScale := StrToBool(temp);
      scaleUnit := message.ArgumentList.GetString(TREE_OPTIONS_SCALE_NAME_CAPTION_DISTANCE_SCALE_INDEX);
      scaleText := message.ArgumentList.GetString(TREE_OPTIONS_DISTANCE_SCALE_LENGTH_INDEX);
      temp := message.ArgumentList.GetString(TREE_OPTIONS_DISTANCE_TICK_INTERVAL_INDEX);
      scaleTick := StrToFloat(temp);
      temp := message.ArgumentList.GetString(TREE_OPTIONS_SCALE_TIME_SCALE_INDEX);
      showTimeScale := StrToBool(temp);
      timeUnit := message.ArgumentList.GetString(TREE_OPTIONS_SCALE_NAME_CAPTION_TIME_SCALE_INDEX);
      timeText := message.ArgumentList.GetString(TREE_OPTIONS_SCALE_MAJOR_TICK_INDEX);
      temp := message.ArgumentList.GetString(TREE_OPTIONS_SCALE_MINOR_TICK_INDEX);
      timeTick := StrToFloat(temp);
      temp := message.ArgumentList.GetString(TREE_OPTIONS_SCALE_NODE_HEIGHT_ERR_INDEX);
      showErrBars := StrToBool(temp);
      temp := message.ArgumentList.GetString(TREE_OPTIONS_SCALE_FONT_INDEX);
      aFont := TFont.Create;
      aFont.Assign(Tree.ScaleFont);
      if Trim(temp) <> EmptyStr then
        if UpdateFontFromCssString(aFont, temp) then
          Tree.ScaleFont.Assign(aFont)
        else
          ShowMessage('Oops! Failed to retrieve the selected font settings. Please report this bug to the developers of MEGA');
      if Tree.IsScale then
      begin
        if OriTree.isScale then begin
          OriTree.ShowScale := showScale;
          OriTree.ScalePen.Width := lineWidth;
          OriTree.ScaleUnit := scaleUnit;
          OriTree.ScaleText := scaleText;
          OriTree.ScaleTick := scaleTick;
        end;
        if IsBootTree and BootTree.isScale then begin
          BootTree.ShowScale := showScale;
          BootTree.ScalePen.Width := lineWidth;
          BootTree.ScaleUnit := scaleUnit;
          BootTree.ScaleText := scaleText;
          BootTree.ScaleTick := scaleTick;
        end;
        if IsReltimeTree and ReltimeTree.isScale then begin
          ReltimeTree.ShowScale := showScale;
          ReltimeTree.ScalePen.Width := lineWidth;
          ReltimeTree.ScaleUnit := scaleUnit;
          ReltimeTree.ScaleText := scaleText;
          ReltimeTree.ScaleTick := scaleTick;
          if CompareValue(ReltimeTree.TimeFactor, 0.0, FP_CUTOFF) <> 0 then begin
            ReltimeTree.ShowTimeScale := showTimeScale;
            ReltimeTree.TimeUnit := timeUnit;
            if ReltimeTree.IsSamplingTimes then
              ReltimeTree.TimeText := '-' + timeText
            else
              ReltimeTree.TimeText := timeText;
            ReltimeTree.TimeTick := timeTick;
          end;
        end;
      end;
      Tree.ShowHeightErrBar := showErrBars;
      ReltimeTree.ShowHeightErrBar := showErrBars;
      ActionDisplayErrorBars.Checked := Tree.ShowHeightErrBar;
      if ReltimeTree.ShowHeightErrBar then
        FigureGenerator.AssignData(DISP_ERR_BARS_STR, 'True')
      else
        FigureGenerator.AssignData(DISP_ERR_BARS_STR, 'False');
      UpdateInfoBox;
      Tree.Refresh;
      UpdateSidePanel;
    except
     on E:Exception do
       ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(aFont) then
      aFont.Free;
  end;
end;

procedure TTreeViewForm.ProcessLabelsOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
var
  temp: String;
  showNames, showMarkers: Boolean;
  aFont: TFont = nil;
  a: TNodeAttrib = nil;
  i: Integer;
  len: Integer;
  aList: TStringList = nil;
  tokenizer: TStringList = nil;
  aMarker: TNodeMarker;

  procedure UpdateMarker(const aStr: String; var aMarker: TNodeMarker);
  var
    myStr: String;
    shapeStr: String;
    colorStr: String;
    j: Integer;
  begin
    myStr := StringReplace(aStr, ',', LineEnding, [rfReplaceAll]);
    tokenizer.Text := myStr;
    Assert(tokenizer.Count = 3);
    for j := 0 to tokenizer.Count - 1 do
      tokenizer[j] := Trim(tokenizer[j]);
    shapeStr := tokenizer.Values['shape'];
    colorStr := tokenizer.Values['color'];
    aMarker.Shape := HtmlStringToTNodeMarkerShape(shapeStr);
    aMarker.Color := SHtmlColorToColor(colorStr, len, clBlack);
  end;

begin
  try
    try
      HideOptionsDialog;
      temp := message.ArgumentList.GetString(0);
      if Pos(RENDER_PROCESS_ERROR, temp) > 0 then
        raise Exception.Create(temp);
      temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_LABELS_FONT_INDEX);
      aFont := TFont.Create;
      aFont.Assign(Tree.OTU_Font);
      if Trim(temp) <> EmptyStr then
        if UpdateFontFromCssString(aFont, temp) then
        begin
          //Tree.OTU_Font.Assign(aFont)
          a := TNodeAttrib.Create;
          temp := message.ArgumentList.GetString(TREE_OPTIONS_BRANCH_LABELS_COLOR_INDEX);
          aFont.Color := SHtmlColorToColor(temp, len, clBlack);
          if OriTree.GroupAttrib.Count > 0 then
            for i := 0 to OriTree.GroupAttrib.Count-1 do
              if (OriTree.GroupAttrib[i].Font.Name = OriTree.OTU_Font.Name) and
                 (OriTree.GroupAttrib[i].Font.Size = OriTree.OTU_Font.Size) and
                 (OriTree.GroupAttrib[i].Font.Color = OriTree.OTU_Font.Color) and
                 (OriTree.GroupAttrib[i].Font.Style = OriTree.OTU_Font.Style) then
                OriTree.GroupAttrib[i].Font.Assign(aFont);
          if OriTree.AttribList.Count > 1 then
            for i := 1 to OriTree.AttribList.Count-1 do
              if (OriTree.AttribList[i].Font.Name = OriTree.OTU_Font.Name) and
                 (OriTree.AttribList[i].Font.Size = OriTree.OTU_Font.Size) and
                 (OriTree.AttribList[i].Font.Color = OriTree.OTU_Font.Color) and
                 (OriTree.AttribList[i].Font.Style = OriTree.OTU_Font.Style) then
              begin
                a.Assign(OriTree.AttribList[i]);
                a.Font.Assign(aFont);
                OriTree.SetSubtreeAttrib(a, a.NodeIndex);
              end;
          OriTree.AttribList[0].Font.Assign(aFont);
          if IsBootTree then
          begin
            if BootTree.GroupAttrib.Count > 0 then
              for i := 0 to BootTree.GroupAttrib.Count-1 do
                if (BootTree.GroupAttrib[i].Font.Name = BootTree.OTU_Font.Name) and
                   (BootTree.GroupAttrib[i].Font.Size = BootTree.OTU_Font.Size) and
                   (BootTree.GroupAttrib[i].Font.Color = BootTree.OTU_Font.Color) and
                   (BootTree.GroupAttrib[i].Font.Style = BootTree.OTU_Font.Style) then
                  BootTree.GroupAttrib[i].Font.Assign(aFont);
            if BootTree.AttribList.Count > 1 then
              for i := 1 to OriTree.AttribList.Count-1 do
                if (BootTree.AttribList[i].Font.Name = BootTree.OTU_Font.Name) and
                   (BootTree.AttribList[i].Font.Size = BootTree.OTU_Font.Size) and
                   (BootTree.AttribList[i].Font.Color = BootTree.OTU_Font.Color) and
                   (BootTree.AttribList[i].Font.Style = BootTree.OTU_Font.Style) then
                begin
                  a.Assign(BootTree.AttribList[i]);
                  a.Font.Assign(aFont);
                  BootTree.SetSubtreeAttrib(a, a.NodeIndex);
                end;
            BootTree.AttribList[0].Font.Assign(aFont);
          end;

          if IsReltimeTree then
          begin
            if ReltimeTree.GroupAttrib.Count > 0 then
              for i := 0 to ReltimeTree.GroupAttrib.Count-1 do
                if (ReltimeTree.GroupAttrib[i].Font.Name = ReltimeTree.OTU_Font.Name) and
                   (ReltimeTree.GroupAttrib[i].Font.Size = ReltimeTree.OTU_Font.Size) and
                   (ReltimeTree.GroupAttrib[i].Font.Color = ReltimeTree.OTU_Font.Color) and
                   (ReltimeTree.GroupAttrib[i].Font.Style = ReltimeTree.OTU_Font.Style) then
                  ReltimeTree.GroupAttrib[i].Font.Assign(aFont);
            if ReltimeTree.AttribList.Count > 1 then
              for i := 1 to OriTree.AttribList.Count-1 do
                if (ReltimeTree.AttribList[i].Font.Name = ReltimeTree.OTU_Font.Name) and
                   (ReltimeTree.AttribList[i].Font.Size = ReltimeTree.OTU_Font.Size) and
                   (ReltimeTree.AttribList[i].Font.Color = ReltimeTree.OTU_Font.Color) and
                   (ReltimeTree.AttribList[i].Font.Style = ReltimeTree.OTU_Font.Style) then
                begin
                  a.Assign(ReltimeTree.AttribList[i]);
                  a.Font.Assign(aFont);
                  ReltimeTree.SetSubtreeAttrib(a, a.NodeIndex);
                end;
            ReltimeTree.AttribList[0].Font.Assign(aFont);
          end;
          Tree.OTU_Font := aFont;
        end
        else
          ShowMessage('Oops! Failed to retrieve the selected font settings. Please report this bug to the developers of MEGA');
      temp := message.ArgumentList.GetString(TREE_OPTIONS_LABELS_DISPLAY_TAXON_NAMES_INDEX);
      showNames := StrToBool(temp);
      temp := message.ArgumentList.GetString(TREE_OPTIONS_LABELS_DISPLAY_TAXON_MARKERS_INDEX);
      showMarkers := StrToBool(temp);

      if showNames then
        Tree.ShowOTUName := True
      else
        Tree.ShowOTUName := False;
      if showMarkers then
        Tree.ShowOTUMarker := True
      else
        Tree.ShowOTUMarker := False;

      temp := message.ArgumentList.GetString(MARKER_INFO_INDEX);
      tokenizer := TStringList.Create;
      aList := TStringList.Create;
      aList.Text := Trim(temp);
      for i := 0 to aList.Count - 1 do
      begin
        aMarker := Tree.Marker[i + 1];
        UpdateMarker(aList[i], aMarker);
        Tree.Marker[i + 1] := aMarker;
      end;

      //for i := 1 to Tree.NoOfOTUs do
      //    Tree.Marker[i] := Marker[i - 1];
      Tree.Refresh;
      DataSaved := false;
      UpdateSidePanel;
    except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(aFont) then
      aFont.Free;
    if Assigned(a) then
      a.Free;
    if Assigned(aList) then
      aList.Free;
    if Assigned(tokenizer) then
      tokenizer.Free;
  end;
end;

procedure TTreeViewForm.ProcessCutoffOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
var
  temp: String;
  aConsensusCutoff: Integer = 0;
  aCondensedCutoff: Integer = 0;
begin
  try
    HideOptionsDialog;
    temp := message.ArgumentList.GetString(0);
    if Pos(RENDER_PROCESS_ERROR, temp) > 0 then
      raise Exception.Create(temp);
    temp := message.ArgumentList.GetString(TREE_OPTIONS_CUTOFF_CONDENSED_INDEX);
    aCondensedCutoff := StrToInt(temp);
    temp := message.ArgumentList.GetString(TREE_OPTIONS_CUTOFF_CONSENSUS_INDEX);
    aConsensusCutoff := StrToInt(temp);
    ApplyConsensusCondensedTreeOptions(aConsensusCutoff, aCondensedCutoff);
  except
  on E:Exception do
    ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TTreeViewForm.ProcessNewickOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
var
  aList: TStringList = nil;
  Options: TNewickExportOptions;
  temp, Newick: String;
begin
  try
    try
      HideOptionsDialog;
      temp := message.ArgumentList.GetString(NEWICK_BRANCH_LENGTHS_INDEX);
      Options.BranchLengths := StrToBool(temp);
      temp := message.ArgumentList.GetString(NEWICK_BOOTSTRAP_VALUES_INDEX);
      Options.BootstrapVals := StrToBool(temp);
      temp := message.ArgumentList.GetString(NEWICK_GENE_DUPLICATIONS_INDEX);
      Options.GeneDuplications := StrToBool(temp);
      temp := message.ArgumentList.GetString(NEWICK_SPECIATIONS_INDEX);
      Options.SpeciationEvents := StrToBool(temp);
      temp := message.ArgumentList.GetString(NEWICK_DIVERGENCE_TIMES_INDEX);
      Options.DivergenceTimes := StrToBool(temp);
      temp := message.ArgumentList.GetString(NEWICK_RELATIVE_TIMES_INDEX);
      Options.Reltimes := StrToBool(temp);
      temp := message.ArgumentList.GetString(NEWICK_NODE_LABELS_INDEX);
      Options.NodeLabels := StrToBool(temp);

      if Tree = ReltimeTree then
      begin
        if (not (tttReltimeLocal in FTimetreeTypes)) and Assigned(MLAnalyzer) then
          Newick := MLAnalyzer.TimeTreeToNewickString(MAI.MyOtuNames, Options.DivergenceTimes)
        else
          Newick := RelTimeAnalyzer.TimeTreeToNewickString(MAI.MyOtuNames, Options.DivergenceTimes);
      end
      else
        Newick := Tree.GetNewickTree(Options);

      AList := TStringList.Create;
      AList.Add(Newick);
      OpenStringList(AList, 'Newick Export.nwk');
    except
      on E:Exception do
        ShowMessage('Oh no! An error occurred when exporting to a newick file: ' + E.Message);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TTreeViewForm.ProcessCopyCaptionMessage(Sender: TObject; const Browser: ICefBrowser; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
var
  temp: ustring;
  OutString: WideString;
begin
  try
    temp := Trim(message.ArgumentList.GetString(0));
    OutString := CleanupMegaCaptionText(temp);
    ClipBoard.AsText := OutString;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when copying to the clipboard: ' + E.Message);
  end;
end;

procedure TTreeViewForm.ProcessSaveCaptionMessage(Sender: TObject; const Browser: ICefBrowser; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
var
  ExportList: TStringList = nil;
  temp: ustring;
  outstring: WideString;
begin
  try
    try
      ExportList := TStringList.Create;
      temp := Trim(message.ArgumentList.GetString(0));
      outstring := CleanupMegaCaptionText(temp);
      ExportList.Text := outstring;
      OpenStringList(ExportList, 'Caption Export');
    except
      on E:Exception do
        ShowMessage('Oh no! An error occurred when saving the caption: ' + E.Message);
    end;
  finally
    if Assigned(ExportList) then
      ExportList.Free;
  end;
end;

procedure TTreeViewForm.ApplyConsensusCondensedTreeOptions(aConsensusCutoff: Integer; aCondensedCutoff: Integer);
begin
  Tree.CondenseValue := aCondensedCutoff;
  Tree.ConsensusValue := aConsensusCutoff;
  if (Tree = BootTree) and (ActionCondensed.Checked) then
    Tree.CondenseValue := aCondensedCutoff;
  if ActionConsensus.Checked then
  begin
    if IsBootTree then
      with TreeTabControl do
        if Tree = OriTree then
        begin
          if Tabs.IndexOf(ORI_TREE_TAB) >= 0 then
            TreeTabControl.Tabs[Tabs.IndexOf(ORI_TREE_TAB)] := CONSENSUS_TREE_TAB
        end
        else if Tree = BootTree then
        begin
          if Tabs.IndexOf(BOOT_TREE_TAB) >= 0 then
            TreeTabControl.Tabs[Tabs.IndexOf(BOOT_TREE_TAB)] := BOOT_CONS_TREE_TAB;
        end;
    if Tree = OriTree Then
      CurOriTreeIndex := Tree.TreeIndex
    else if (Tree = BootTree) and (not ActionCondensed.Checked) then
      CurBootTreeIndex := Tree.TreeIndex;
    Tree.TreeIndex := 0;
    FMultiTreesFrame.Visible := False;
  end;

  if ActionCondensed.Checked then
  begin
    Tree.IsCondensed := True;
    if IsBootTree then
      with TreeTabControl do
        if Tree = OriTree then
        begin
          if Tabs.IndexOf(CONSENSUS_TREE_TAB) >= 0 then
            TreeTabControl.Tabs[Tabs.IndexOf(CONSENSUS_TREE_TAB)] := ORI_TREE_TAB
        end
        else if (Tree = BootTree) and (not Tree.isConsensus) then
        begin
          if Tabs.IndexOf(BOOT_CONS_TREE_TAB) >= 0 then
            TreeTabControl.Tabs[Tabs.IndexOf(BOOT_CONS_TREE_TAB)] := BOOT_TREE_TAB;
        end;
    if Tree = OriTree then
      if CurOriTreeIndex = 0 then
        Tree.TreeIndex := 1
      else
        Tree.TreeIndex := CurOriTreeIndex
    else if Tree = BootTree then
      if CurBootTreeIndex = 0 then
        Tree.TreeIndex := 1
      else
        Tree.TreeIndex := CurBootTreeIndex;
    if Tree.NoOfTrees > 1 then
    begin
      FMultiTreesFrame.TreeNumSEdit.MaxValue := Tree.NoOfTrees;
      FMultiTreesFrame.TreeNumSEdit.Value := Tree.TreeIndex;
      FMultiTreesFrame.NumTreesLabel.Caption := 'of ' + Format('%-6d', [Tree.NoOfTrees]);
      FMultiTreesFrame.Visible := True;
    end
    else
      FMultiTreesFrame.Visible := False;
  end;
  if InfoDisplay.Visible then
  begin
    RenewTreeInfo;
    RenewBranchInfo;
  end;
  ResetStatusBar;
  Tree.Refresh;
  UpdateSidePanel;
end;

procedure TTreeViewForm.AssignHelpContext;
begin
  HelpContext := HC_Tree_Explorer;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
  FileMenu.HelpContext    :=  HC_File_Menu_in_Tree_Explorer;
  ImageMenu.HelpContext   :=  HC_Image_Menu_in_Tree_Explorer;
  SubtreeMenu.HelpContext :=  HC_Subtree_in_Tree_Explorer;
  ViewMenu.HelpContext    :=  HC_View_Menu_in_Tree_Explorer;
  ComputeMenu.HelpContext :=  HC_Compute_in_Tree_Explorer;
end;

function TTreeViewForm.GetNewickExportOptions: TNewickExportOptions;
begin
  Result.BranchLengths := (Tree.HasNonZeroBranchLength and (Tree <> ReltimeTree) and (Tree <> BootTree));
  Result.BootstrapVals := Tree.isStats;
  Result.NodeLabels := Tree.HasInternalNodeLabels;
  Result.GeneDuplications := Tree.HasGeneDups;
  Result.SpeciationEvents := Tree.HasSpeciations;
  Result.Reltimes := (Tree = ReltimeTree);
  Result.DivergenceTimes := ((Tree = ReltimeTree) and HasCalibrations);
end;

procedure TTreeViewForm.GenerateCapViewerForCurrentTree;
begin
    if Tree = OriTree then
    begin
      if not Assigned(FOriTreeCapViewer) then
      begin
        FOriTreeCapViewer := CreateNewChromiumBrowserWindow(bmCaption, Self, True);
        FOriTreeCapViewer.OnCloseNotify := OriTreeCapViewNotify;
      end;
      FCapViewer := FOriTreeCapViewer;
    end
    else if Tree = BootTree then
    begin
      if not Assigned(FBootTreeCapViewer) then
      begin
        FBootTreeCapViewer := CreateNewChromiumBrowserWindow(bmCaption, Self, True);
        FBootTreeCapViewer.OnCloseNotify := BootTreeCapViewNotify;
      end;
      FCapViewer := FBootTreeCapViewer;
    end
    else if Tree = ReltimeTree then
    begin
      if not Assigned(FReltimeTreeCapViewer) then
      begin
        FReltimeTreeCapViewer := CreateNewChromiumBrowserWindow(bmCaption, Self, True);
        FReltimeTreeCapViewer.OnCloseNotify := ReltimeTreeCapViewNotify;
      end;
      FCapViewer := FReltimeTreeCapViewer;
    end
    else
      ShowMessage('Current tree not assigned. This is a bug!');
end;

procedure TTreeViewForm.RefreshCaption;
var
  ModelInfo : TModelInfo;
begin
  if FFormIsClosing then
    Exit;
  if FJustRootingATree or (not Assigned(FigureGenerator)) or (not Assigned(MAI)) then
    Exit;
  ModelInfo := nil;
  try
    try
    RenewTreeInfo;
    FigureGenerator.FlushTemplate(Tree);
    FigureGenerator.FlushTemplate(MAI);
    FigureGenerator.LoadStyleSheet;
    FigureGenerator.AssignData('GeneDupsCitation', 'Zmasek_and_Eddy_2001');
    if (MLAnalyzer <> nil) and (not MAI.MyDistPack.DoesContain(gdInvar)) then
      FigureGenerator.AssignData('PropOfInvariant', 'N/A');
    if (MLAnalyzer <> nil) and (not MAI.MyDistPack.DoesContain(gdGamma)) then
      FigureGenerator.AssignData('GammaPara', 'N/A');
    if MLAnalyzer <> nil then
    begin
      FigureGenerator.AssignData('LogLikelihoodCE', Format('%.2f', [MLAnalyzer.LogLikelihood]));
      ModelInfo := TModelInfo.Create;    //Used for models of protein names
      MLAnalyzer.GetModelInfo(ModelInfo); //for whatever reason, this doesn't work even though they are accessed with properties declared published
      FigureGenerator.AssignData('FullModelName', ModelInfo.FullName);   //Binding ModelInfo with published properties doesn't seem to work

      if MAI.MyDistPack.DoesContain(gdInvar) then
        FigureGenerator.AssignData('PropOfInvariant', Format('%.2f', [ModelInfo.Invar*100]));

      if MAI.MyDistPack.DoesContain(gdGamma) then
      begin
        FigureGenerator.AssignData('GammaPara', Format('%.4f', [ModelInfo.Gamma]));
        FigureGenerator.AssignData('NoOfGCats', IntToStr(ModelInfo.NoOfRates));
      end;

      if MAI.MyTreePack.DoesContain(ttClock) or MAI.MyTreePack.DoesContain(ttClockTest) then
        FigureGenerator.AssignData('MolecularClockTest', Format('%.4f', [MolecularClockTest]));

      if MAI.ClockType = ctLocal then
      begin
        if (MAI.MyUsrOperation = dtdoRelTimeML) or (MAI.MyUsrOperation = dtdoRtdtML) then
          FigureGenerator.AssignData('Operation', 'dtdoRelTimeML')
        else if (MAI.MyUsrOperation = dtdoRelTimeBLens) or (MAI.MyUsrOperation = dtdoRtdtBlens) then
          FigureGenerator.AssignData('Operation', 'dtdoRelTimeBLens')
        else if (MAI.MyUsrOperation = dtdoRelTimeLS) or (MAI.MyUsrOperation = dtdoRtdtLS) then
          FigureGenerator.AssignData('Operation', 'dtdoRelTimeLS')
        else if MAI.MyUsrOperation = dtdoGeneDupInference then
          FigureGenerator.AssignData('Operation', 'dtdoGeneDupInference');

        if MAI.ClockLevel = clOneStdErr then
          FigureGenerator.AssignData('ClockLevel', IntToStr(1))
        else if MAI.ClockLevel = clTwoStdErr then
          FigureGenerator.AssignData('ClockLevel', IntToStr(2))
        else if MAI.ClockLevel = clThreeStdErr then
          FigureGenerator.AssignData('ClockLevel', IntToStr(3))
        else
          FigureGenerator.AssignData('ClockLevel', 'Unknown');
        if MAI.MergeRates then
          FigureGenerator.AssignData('MergeRates', 'True')
        else
          FigureGenerator.AssignData('MergeRates', 'False');

        if Tree.ShowDivergenceTimes then
          FigureGenerator.AssignData(DISP_DIV_TIMES_STR, 'True')
        else
          FigureGenerator.AssignData(DISP_DIV_TIMES_STR, 'False');
        if Tree.ShowHeightErrBar then
          FigureGenerator.AssignData(DISP_ERR_BARS_STR, 'True')
        else
          FigureGenerator.AssignData(DISP_ERR_BARS_STR, 'False');
      end;
    end
    else
    begin
      if IsReltimeBLensAnalysis(MAI.MyUsrOperation) then
        FigureGenerator.AssignData('Operation', 'dtdoRelTimeBLens')
      else if (MAI.MyUsrOperation = dtdoRelTimeLS) or (MAI.MyUsrOperation = dtdoRtdtLS) then
      begin
        FigureGenerator.AssignData('GammaPara', 'N/A');
        FigureGenerator.AssignData('Operation', 'dtdoRelTimeLS');
      end
      else if MAI.MyUsrOperation = dtdoOLSComputeUserTreeBLens then
        FigureGenerator.AssignData('Operation', 'dtdoOLSComputeUserTreeBLens')
      else if MAI.MyUsrOperation = dtdoMPComputeUserTreeBLens then
        FigureGenerator.AssignData('Operation', 'dtdoMPComputeUserTreeBLens');

      if (MAI.MyDistPack <> nil) then
      begin
        if MAI.MyDistPack.DistModel = gdNoOfDiff  then
          FigureGenerator.AssignData('FullModelName', 'number of differences')
        else if MAI.MyDistPack.DistModel = gdPropDist  then
          FigureGenerator.AssignData('FullModelName', 'p-distance');
      end;
    end;

    if (MAI.MyTreePack <> nil) and (MAI.MyTreePack.DoesContain(ttML)) then
    begin
      if MAI.MyTreePack.DoesContain(ttInferTree) then
        FigureGenerator.AssignData('InitialTree', MAI.MyTreePack.GetMLInitialTreesMethodCitation(MAI.isAminoAcid, MAI.MyDistPack.DistModel))
      else
        FigureGenerator.AssignData('AnalyzeUserTree', 'True');
    end;
    if MAI.MyTreePack.DoesContain(ttUserTree) and (not MAI.MyTreePack.DoesContain(ttInferTree)) then
      FigureGenerator.AssignData('AnalyzeUserTree', 'True');
    FigureGenerator.BindData(InfoDisplay);
    FigureGenerator.BindData(Tree);
    FigureGenerator.BindData(MAI);
    if Assigned(MAI.MyTreePack) and (not IsReltimeBLensAnalysis(MAI.MyUsrOperation)) then
      FigureGenerator.BindData(MAI.MyTreePack);
    FigureGenerator.FlushTemplate(Self);
    FigureGenerator.BindData(Self);
    if Assigned(MAI.MyDistPack) then
      FigureGenerator.BindData(MAI.MyDistPack);
    FCaptionHtml := FigureGenerator.GenerateLegend;
    if (CaptionPanel.Visible) and (CaptionViewer.Browser <> nil) then
      CaptionViewer.Browser.MainFrame.LoadString(FCaptionHtml, 'http://localhost/')
    else if Assigned(FCapViewer) and (FCapViewer.Visible) then
      FCapViewer.LoadHtmlFromString(FCaptionHtml);
    except
      on E:Exception do
        ShowMessage('Oh no! An error occurred when refreshing the tree caption: ' + E.Message);
    end;
  finally
    if Assigned(ModelInfo) then
      ModelInfo.Free;
  end;
end;

procedure TTreeViewForm.OriTreeCapViewNotify(Sender: TObject);
begin
  if FCapViewer = FOriTreeCapViewer then
    FCapViewer := nil;
  FOriTreeCapViewer := nil;
end;

procedure TTreeViewForm.BootTreeCapViewNotify(Sender: TObject);
begin
  if FCapViewer = FBootTreeCapViewer then
    FCapViewer := nil;
  FBootTreeCapViewer := nil;
end;

procedure TTreeViewForm.ReltimeTreeCapViewNotify(Sender: TObject);
begin
  if FCapViewer = FReltimeTreeCapViewer then
    FCapViewer := nil;
  FReltimeTreeCapViewer := nil;
end;

function TTreeViewForm.ReltimeShouldBeEnabled: Boolean;
begin
  Result := ((not RelTimeActionDisabled) and
                            (not FJustRootingATree) and
                            (not FIsTreeSession) and
                            (not Tree.IsGeneDups) and
                            (Tree <> ReltimeTree) and
                            (Tree <> BootTree)and
                            (Tree.HasNonZeroBranchLength));
end;

procedure TTreeViewForm.DecideRootOnMidpoint;
var
  ShouldRoot: Boolean;
begin
  if not Assigned(MAI) then
    Exit;

  ShouldRoot := True;

  case MAI.MyUsrOperation of
    dtdoMLClockTest,
    dtdoMLClockTestLocal: ShouldRoot := False;

    dtdoNJTree,
    dtdoMETree,
    dtdoMPTree,
    dtdoUPGMATree,
    dtdoMLTree: ShouldRoot := True;

    dtdoMPComputeUserTreeBLens,
    dtdoOLSComputeUserTreeBLens,
    dtdoMLComputeUserTreeBLens,
    dtdoRtdtML, dtdoRtdtLS, dtdoRtdtBlens,
    dtdoRelTimeML,
    dtdoRelTimeBLens,
    dtdoRelTimeLS: ShouldRoot := False;

    dtdoMLInferAncSeq, dtdoMPInferAncSeq,
    dtdoMPInferAncSeqMyPeg,
    dtdoMLInferAncSeqMyPeg: ShouldRoot := False;
    dtdoGeneDupInference: ShouldRoot := False;
    else
      ShouldRoot := True;
  end;
  ActionRootOnMidpoint.Checked := ShouldRoot;
end;

procedure TTreeViewForm.DisableActions;
begin
  TreeActionList.State := asSuspended;
end;

procedure TTreeViewForm.EnableActions;
begin
  TreeActionList.State := asNormal;
end;

procedure TTreeViewForm.InitCalibrationDlg;
begin
  //FCalibrationDlg.Show;
  FCalibrationDlg.GetInternalNodeLabelFunc := MAI.MyOriTreeList.GetInternalNodeLbl;
  FCalibrationDlg.TreeViewForm := Self;
  FCalibrationDlg.SetAnalysisInfo(MAI);
  FCalibrationDlg.SetFileName(MegaForm.DataFileName);
  //FCalibrationDlg.Hide;
  try
    MegaForm.RemoveWindowFromTray(FCalibrationDlg);
  except
    on E:Exception do
      ShowMessage('Error removing CalibrationDlg from window tray: ' + E.Message);
  end;
end;

procedure TTreeViewForm.LaunchTimetreeAnalysis;
var
  Info: TStringList;
begin
  try
    if Assigned(FReltimeAnalyzer) then
      FRelTimeAnalyzer.StrictClockRate := 1;
    ReltimeTree.LatestTime := 0.0;
    RetrieveTreeFromCalibrationDlg;
    Self.Show;
    RemoveWindowFromTray(FTimeTreeWizardForm);
    Application.ProcessMessages;
    if Assigned(FTimeTreeWizard) then
      FreeAndNil(FTimeTreeWizard);
    if Assigned(FTimeTreeWizardForm) then
      FTimeTreeWizardForm.Hide;
    if Assigned(MAI.ReltimeNexusExport) then
      FreeAndNil(MAI.ReltimeNexusExport);
    if FCalibrations.Count > 0 then
      DoRelTimeWithCalibrations
    else
      DoRelTimeNoCalibrations;
    TreeTabControl.TabHeight := 0;
    Info := OriTree.Information;
    if Info.Count > 1 then
      Info.Insert(2, '  Post Processing ----------------- Estimate Divergence Times (Reltime)')
    else
      Info.Add('  Post Processing ----------------- Estimate Divergence Times (Reltime)');
    ReltimeTree.Information.Assign(Info);
    ReltimeTree.FocusOnRoot;
    ReltimeTree.SetOutgroupCluster(clSilver, ReltimeTree.NodeInfo.Des2Index);
    if (OriTree.BranchStyle <> ReltimeTree.BranchStyle) and (OriTree.TreeStyle = tsTraditional) then
      ReltimeTree.BranchStyle := OriTree.BranchStyle;
    ChangeTree(ReltimeTree);
    TreeTabControl.TabIndex := TreeTabControl.Tabs.IndexOf(TIME_TREE_TAB);
    ShowHideMacTabButtons;
    CompressOutgroup(FCalibrations.Count > 0);
    if Tree.Height*4 < (ClientHeight - CaptionPanel.Height) then
      ActionAutoSizeExecute(Self);
    if Assigned(FCapViewer) and (not FCapViewer.Visible) then
      CaptionPanel.Visible := True;

    Self.Height := FFormHeight;
    AdjustTreeForBestFitToWindow(True);
    if CaptionPanel.Visible then
      CaptionViewer.Visible := True;
    RefreshCaption;
    RelTimeBtn.Action := ActionRecalibrateTimetree;
    RelTimeMenuItem.Action := ActionRecalibrateTimetree;
    FComputeFrame.ComputeTimetreeBtn.Action := ActionRecalibrateTimetree;
    FComputeFrame.OperationLabel.Caption := 'Recalibrate Tree...';
    Invalidate;
  except
    on E:Exception do
    begin
      Tree := OriTree;
      ShowMessage('Oh no! The timetree analysis failed: ' + E.Message);
    end;
  end;
end;

procedure TTreeViewForm.CancelTimetreeAnalysis;
begin
  MegaForm.RemoveWindowFromTray(FTimeTreeWizardForm);
  FreeAndNil(FTimeTreeWizard);
  //FreeAndNil(FTimeTreeWizardForm);
  if Assigned(FCalibrations) and (FCalibrations.Count > 0) then
    FreeAndNil(FCalibrations);
  RelTimeActionDisabled := False;
  MegaForm.RemoveWindowFromTray(FCalibrationDlg);
  RetrieveTreeFromCalibrationDlg;
  FCalibrationsActionEnabled := False;
  IsSetRelTimeCalibsMode := False;
  Self.Show;
end;

procedure TTreeViewForm.CancelCalibrations;
begin
  CancelRecalibrateTimetree;
  FCalibrationsActionEnabled := False;
  FCalibrationDlg.CWCancelCallBack := nil;
end;

procedure TTreeViewForm.CancelRecalibrateTimetree;
begin
  RetrieveTreeFromCalibrationDlg;
  if Assigned(FCalibrations) and (FCalibrations.Count > 0) then
    CalibrationDlg.SetCalibrations(FCalibrations);
  OriTree.ShowCalibratedNodeMarker := False;
  CalibrationDlg.CWCancelCallBack := nil;
  try
    MegaForm.RemoveWindowFromTray(CalibrationDlg);
  except
    {$IFDEF DEBUG}
    on E:Exception do
      ShowMessage('Exception raised when removing form from window tray: ' + E.Message);
    {$ENDIF}
  end;
  Self.Show;
end;

procedure TTreeViewForm.RecalibrateTimetree(Calibrations: TCalibrations);
var
  aList: TTreeList;
begin
  if not Assigned(FCalibrations) then
    FCalibrations := TCalibrations.Create;
  FCalibrations.Clear;
  FCalibrations.Assign(Calibrations);
  FCalibrations.SetInfo(MAI);
  if Assigned(MAI.CalibrationTimes) then
    MAI.CalibrationTimes.Assign(FCalibrations);
  aList := TTreeList.Create;
  OriTree.AssignTreeList(aList);
  OriTree.ShowCalibratedNodeMarker := False;
  FCalibrations.AssignTreeList(aList);
  LaunchTimetreeAnalysis;
end;

procedure TTreeViewForm.ResetStatusBar;
begin
  if FJustRootingATree then
  begin
    TreeStatusBar.Panels[0].Text := 'Displaying Tree to Root';
    Exit;
  end;

  if Tree.IsLinearized then
  begin
    TreeStatusBar.Panels[0].Text := 'Timetree';
    if Tree.ShowDivergenceTimes then
    begin
      if ShowingStrictClockTree then
        TreeStatusBar.Panels[0].Text := TreeStatusBar.Panels[0].Text + ' (Strict Clock) '
      else if (Length(ReltimeTree.CalibratedNodes) = 0) then
        TreeStatusBar.Panels[0].Text := TreeStatusBar.Panels[0].Text + ' (Relative Times) '
      else
        TreeStatusBar.Panels[0].Text := TreeStatusBar.Panels[0].Text + ' (Exact Times) ';
    end;
    if tttRelTimeLocal in FTimetreeTypes then
    begin
      if Tree.isValue then
        TreeStatusBar.Panels[0].Text := TreeStatusBar.Panels[0].Text + '; ' + Tree.ValueName+' = '
                     +FloatToStrF(Tree.Value[Tree.TreeIndex],ffFixed,18,Tree.ValueDecimals);
      if Tree.isValue2 then
      begin
        TreeStatusBar.Panels[0].Text := TreeStatusBar.Panels[0].Text + '; ' + Tree.Value2Name+' = '
                     +FloatToStrF(Tree.Value2[Tree.TreeIndex],ffFixed,18,Tree.Value2Decimals);
      end;
    end
    else if Assigned(MLAnalyzer) then
    begin
      TreeStatusBar.Panels[0].Text := TreeStatusBar.Panels[0].Text + '; ' + Tree.ValueName + ' = '
                   +FloatToStrF(Tree.Value2[Tree.TreeIndex],ffFixed,18,Tree.ValueDecimals);
    end;
    Exit;
  end;

  if (((TreeTabControl.TabIndex = TreeTabControl.Tabs.IndexOf(BOOT_TREE_TAB)) or
     (TreeTabControl.TabIndex = TreeTabControl.Tabs.IndexOf(BOOT_CONS_TREE_TAB))) and
     (TreeTabControl.TabIndex <> -1)) then
  begin
    TreeStatusBar.Panels[0].Text := '';
    Exit;
  end;

  with TreeStatusBar do
  begin
    Panels[0].Text := '';
    if MLAnalyzer <> nil then
    begin
      if IsReltimeTree then
        Panels[0].Text := 'Original Tree (LogL = ' + FloatToStrF(MLAnalyzer.LogLikelihood, ffFixed, 18, Tree.ValueDecimals) + ')'
      else
        Panels[0].Text := Panels[0].Text +Tree.ValueName+' = '
                   +FloatToStrF(Tree.Value[Tree.TreeIndex],ffFixed,18,Tree.ValueDecimals);
    end
    else
    begin
      if Tree.isValue then
        Panels[0].Text := Panels[0].Text +Tree.ValueName+' = '
                     +FloatToStrF(Tree.Value[Tree.TreeIndex],ffFixed,18,Tree.ValueDecimals);
      if Tree.isValue2 then
      begin
        if Panels[0].Text <> '' then Panels[0].Text := Panels[0].Text + '; ';
        Panels[0].Text := Panels[0].Text +Tree.Value2Name+' = '
                     +FloatToStrF(Tree.Value2[Tree.TreeIndex],ffFixed,18,Tree.Value2Decimals);
      end;
    end;
    if Tree.isFreq then begin
      if Panels[0].Text <> '' then Panels[0].Text := Panels[0].Text + '; ';
      Panels[0].Text := Panels[0].Text +'Freq = '
                   +FloatToStrF(Tree.Frequency[Tree.TreeIndex],ffFixed,18,Tree.FreqDecimals);
      if Tree.FreqDecimals = 0 then
         Panels[0].Text := Panels[0].Text +'/ '+FloatToStrF(Tree.TotalFrequency,ffFixed,18,0);
    end;
    if Tree.isBranchLength and (ComputeParsimInfo = nil) and (MLAnalyzer = nil) then
    begin
      if Panels[0].Text <> '' then Panels[0].Text := Panels[0].Text + '; ';
      Panels[0].Text := Panels[0].Text +'SBL = '+FloatToStrF(Tree.SBL[Tree.TreeIndex],ffFixed,18,8);
    end;
  end;
end;

procedure TTreeViewForm.SetupTreeBox;
begin
  Tree := OriTree;
  Tree.Align := alClient;
  Tree.Visible := True;
  Tree.Enabled := True;
  Tree.Cursor := crDefault;
  Tree.Color := clAqua;
  OriTree.Parent := TreeTabControl;
  BootTree.Parent := TreeTabControl;
  ReltimeTree.Parent := TreeTabControl;
  ScrollAssist.Parent := Tree;
  OriTree.Visible := True;
  BootTree.Visible := False;
  ReltimeTree.Visible := False;
  ScrollAssist.Visible := False;
  Tree.BringToFront;
end;

procedure TTreeViewForm.ChangeTree(aTree: TTreeBox);
begin
  OriTree.Visible := False;
  BootTree.Visible := False;
  ReltimeTree.Visible := False;
  Tree := aTree;
  Tree.Visible := True;
  Tree.Enabled := True;
  Tree.Cursor := crDefault;
  Tree.Color := clAqua;
  ScrollAssist.Parent := Tree;
  Tree.BringToFront;
  UpdateSidePanel;
end;

procedure TTreeViewForm.EnterResizeMode;
begin
  FIgnoreProgressUpdates := True;
  Tree.AutoSize := False;
  Tree.IsResizing := True;
  {$IFDEF DARWIN}
  TreeTabControlChangeBounds(Self);
  {$ELSE}
  if Tree.Width < (Tree.Width - GetSystemMetrics(SM_CXVSCROLL)) then
    Tree.Width := Tree.Width - GetSystemMetrics(SM_CXVSCROLL);
  if Tree.Height < Tree.Height - GetSystemMetrics(SM_CXVSCROLL) then
    Tree.Height := Tree.Height - GetSystemMetrics(SM_CXVSCROLL);
  {$ENDIF}
  ActionResize.ImageIndex := ENTERRESIZEMODE_IMAGE;
  Tree.Cursor := crSizeNWSE;
  Tree.DragCursor := crSizeNWSE; // We have to set the drag cursor here since the scrollbox's drag type is Automatic.  Otherwise even when the resize button is not down and we did a drag on the scrollbox the crSizeNWSE cursor would appear.
  Tree.ShowSelection := false;
end;

procedure TTreeViewForm.InitializeMethodInfo;
var
  i: integer;
  aList: TStringList = nil;
begin
  i := -1;
  if Tree.Gap <> '' then begin
    Inc(i);
    MethodInfoType[i] := 'Gap treatment';
    MethodInfo[i] := Trim(Tree.Gap);
  end;
  if Tree.Distance <> '' then begin
    Inc(i);
    MethodInfoType[i] := 'Distance method';
    MethodInfo[i] := Trim(Tree.Distance);
  end;
  if Tree.Method <> '' then begin
    Inc(i);
    MethodInfoType[i] := 'Tree making method';
    MethodInfo[i] := Trim(Tree.Method);
  end;
  if Tree.TestMethod <> '' then begin
    Inc(i);
    MethodInfoType[i] := 'Branch test method';
    MethodInfo[i] := Trim(Tree.TestMethod);
  end;
  FTreeInfoFrame.DisplayGeneralInfo(Tree.Information);
  try
    if Assigned(MAI) and Assigned(MAI.AnalysisSummary) then
    begin
      aList := MAI.AnalysisSummary.StringsForInfoBox;
      FTreeInfoFrame.DisplayAnalysisInfo(aList);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TTreeViewForm.LoadCustomCursors;
begin
  Screen.Cursors[CURSOR_COMPRESS] := LoadCursor(hInstance, 'GROUP');
  Screen.Cursors[CURSOR_FLIP] := LoadCursor(hInstance, 'FLIP');
  Screen.Cursors[CURSOR_FLIPALL] := LoadCursor(hInstance, 'FLIPALL');
  Screen.Cursors[CURSOR_ROOT] := LoadCursor(hInstance, 'ROOT');
  Screen.Cursors[CURSOR_ZOOM] := LoadCursor(hInstance, 'ZOOM');
end;

function TTreeViewForm.GetActiveTabName: String;
begin
  Result := '';
  if (TreeTabControl.Tabs.Count > 0) and (TreeTabControl.TabIndex >= 0) then
    Result := TreeTabControl.Tabs.Strings[TreeTabControl.TabIndex];
end;

function TTreeViewForm.GetAncStateCaption: String;
begin
  Result := '';
  if Tree.TreeStyle <> tsTraditional then
    Exit;
  if (not Tree.ShowCharState) then
    Exit;
  if Assigned(MLAnalyzer) then
  begin
    if MAI.MyUsrOperation = dtdoBEAM then
      Result := 'For each node only the most probable state is shown.'
    else if CurAncState = ActionAncStateShowAll then
      Result := 'The set of states at each node is ordered from most likely to least likely, excluding states with probabilities below 5%.'
    else if CurAncState = ActionAncStateShowMost then
      Result := 'For each node only the most probable state is shown.'
    else if CurAncState = ActionAncStateHideAmbiguous then
      Result := 'Ambiguous states are not shown.';
    if CurAncState = ActionAncStateExtended then
      Result := Result + '  Ambiguous states are shown using extended IUPAC codes.'
  end
  else
  begin
    if MAI.MyUsrOperation = dtdoBEAM then
      Result := 'For each node only the most parsimonious state is shown.'
    else if CurAncState = ActionAncStateShowAll then
    begin
      if ActionAncStateExtended.Checked then
        Result := ' Ambiguous states are shown using extended IUPAC codes.'
    end
    else if CurAncState = ActionAncStateHideAmbiguous then
      Result := 'Ambiguous states are not shown.';
  end;
end;

function TTreeViewForm.GetBLenCaption: String;
begin
  Result := '';
  if Tree.TreeStyle <> tsTraditional then
    Exit;

  if not (Tree.isBranchLength and Tree.ShowBLen) then
    Exit;

  case Tree.BLenPosition of
    bipAboveBranch: Result := Result + 'above the branches';
    bipBelowBranch: Result := Result + 'below the branches';
  else
    Result := Result + 'next to the branches'
  end;
end;

function TTreeViewForm.GetHasCalibrations: Boolean;
begin
  if Length(Tree.CalibratedNodes) > 0 then // handles the case for a subtree in a new window (which does not have MAI)
  begin
    Result := True;
    Exit;
  end;
  Result := Assigned(MAI) and ((ReltimeTree.NumCalibrations > 0) or (Assigned(FCalibrations) and (FCalibrations.Count > 0)) or (MAI.HasCalibrations));
end;

function TTreeViewForm.GetStatPosCaption: String;
begin
  Result := '';

  if Tree.TreeStyle <> tsTraditional then
    Exit;

  if not (Tree.isStats and Tree.ShowStats) then
    Exit;

  case Tree.StatsPosition of
    bipAboveBranch: Result := 'above the branches';
    bipBelowBranch: Result := 'below the branches';
  else
    Result := 'next to the branches'
  end;
end;

procedure TTreeViewForm.SetHideAmbigAncStateProc(AValue: TAncStateProc);
begin
  FHideAmbigAncStateProc:=AValue;
end;

procedure TTreeViewForm.SetShowAllAncStateProc(AValue: TAncStateProc);
begin
  FShowAllAncStateProc:=AValue;
end;

procedure TTreeViewForm.SetShowExtCharAncStateProc(AValue: TAncStateProc);
begin
  FShowExtCharAncStateProc:=AValue;
end;

procedure TTreeViewForm.SetShowMostProbAncStateProc(AValue: TAncStateProc);
begin
  FShowMostProbAncStateProc:=AValue;
end;

procedure TTreeViewForm.SetTree(AValue: TTreeBox);
begin
  if FTree=AValue then Exit;
  FTree:=AValue;
end;

procedure TTreeViewForm.SaveRelTimeTreeViewState(TreeIndex: Integer);
begin
  if TreeIndex > (Length(FRelTimeTreeStates) - 1) then
    Exit;
  FRelTimeTreeStates[TreeIndex].ShowDivergenceTimes      := Tree.ShowDivergenceTimes;
  FRelTimeTreeStates[TreeIndex].ShowBlens                := Tree.ShowBLen;
  FRelTimeTreeStates[TreeIndex].ShowNodeIds              := Tree.ShowNodeIds;
  FRelTimeTreeStates[TreeIndex].ShowTimeScale            := Tree.ShowTimeScale;
  FRelTimeTreeStates[TreeIndex].ShowCalibratedNodeMarker := Tree.ShowCalibratedNodeMarker;
  FRelTimeTreeStates[TreeIndex].CalibratedNodes          := Tree.CalibratedNodes;
  FRelTimeTreeStates[TreeIndex].BLensActionChecked       := ActionBranchLengthDisplay.Checked;
  FRelTimeTreeStates[TreeIndex].TimesActionChecked       := ActionDisplayDivergenceTimes.Checked;
  FRelTimeTreeStates[TreeIndex].TimeScaleActionChecked   := ToggleTimeScaleAction.Checked;
  FRelTimeTreeStates[TreeIndex].NodeIdsActionChecked     := ActionNodeIdsDisplay.Checked;
  FRelTimeTreeStates[TreeIndex].IsClockTree              := Tree.IsLinearized;

  if Tree.NodeFocused then
  begin
    FRelTimeTreeStates[TreeIndex].NodeFocused            := True;
    FRelTimeTreeStates[TreeIndex].FocusedIndex           := Tree.FocusedIndex;
  end;

  if FSidePanelInitialized then
    FTreeNumSEditPrevVal := FMultiTreesFrame.TreeNumSEdit.Value;
  FRelTimeTreeStates[TreeIndex].StateIsSaved             := True;
end;

procedure TTreeViewForm.InitRelTimeTreeViewStates;
var
  NumTrees: Integer;
  i: Integer;
begin
  NumTrees := Tree.NoOfTrees;
  if NumTrees = 1 then
    Exit; { then there is no need to save state as the user will not be switching trees}
  SetLength(FRelTimeTreeStates, NumTrees);
  for i := 0 to NumTrees - 1 do
  begin
    SaveRelTimeTreeViewState(i);
    FRelTimeTreeStates[i].IsClockTree := False;
  end;
end;

function TTreeViewForm.LoadTemplate(MAI: TAnalysisInfo; var FigureGenerator: TLegendGenerator): Boolean;
begin
  if MAI.ClockType = ctLocal then
    Result := FigureGenerator.LoadTemplateFromFile('RelTime.htm')
  else if MAI.MyTreePack.IsAncesteral then
    Result := FigureGenerator.LoadTemplateFromFile('Infer_ancesteral_sequences.htm')
  else if (MAI.MyTreePack.DoesContain(ttUserTree)) and (MAI.MyTreePack.DoesContain(ttClock)) then
    Result := FigureGenerator.LoadTemplateFromFile('ml_tree.htm')
  else if MAI.MyTreePack.DoesContain(ttClock) or MAI.MyTreePack.DoesContain(ttClockTest) then
    Result := FigureGenerator.LoadTemplateFromFile('molecular_clock.htm')
  else if MAI.MyTreePack.NameAcronym = 'ML' then
    Result := FigureGenerator.LoadTemplateFromFile('ml_tree.htm')
  else if MAI.MyTreePack.NameAcronym = 'MP' then
    Result := FigureGenerator.LoadTemplateFromFile('mp_tree.htm')
  else if MAI.MyUsrOperation = dtdoGeneDupInference then
    Result := FigureGenerator.LoadTemplateFromFile('Gene_duplication.htm')
  else
    Result := FigureGenerator.LoadTemplateFromFile('Default_tree.htm');
end;

procedure TTreeViewForm.InitTrees;
begin
  OriTree := TTreeBox.Create(Self, tttOriTree);
  OriTree.Parent := TreeTabControl;
  OriTree.Align := alClient;
  OriTree.Visible := True;
  OriTree.Enabled := True;
  OriTree.Top := 0;
  OriTree.Left := 0;
  OriTree.Cursor := crDefault;
  OriTree.OnChange := TreeChange;
  OriTree.DragMode := dmManual;
  OriTree.OnMouseDown    := TreeMouseDown;
  OriTree.OnMouseUp      := TreeMouseUp;
  OriTree.OnDragOver     := TreeDragOver;
  OriTree.OnEndDrag      := TreeEndDrag;
  OriTree.OnSearch       := TreeSearch;
  OriTree.DoubleBuffered := True;
  OriTree.Canvas.Clipping := True;
  Tree := OriTree;

  BootTree := TTreeBox.Create(Self, tttBootTree);
  BootTree.Parent := TreeTabControl;
  BootTree.Align := alClient;
  BootTree.Visible := False;
  BootTree.Enabled := False;
  BootTree.Top := 0;
  BootTree.Left := 0;
  BootTree.Cursor := crDefault;
  BootTree.OnChange := TreeChange;
  BootTree.DragMode := dmManual;
  BootTree.OnMouseDown    := TreeMouseDown;
  BootTree.OnMouseUp      := TreeMouseUp;
  BootTree.OnDragOver     := TreeDragOver;
  BootTree.OnEndDrag      := TreeEndDrag;
  BootTree.OnSearch       := TreeSearch;
  BootTree.DoubleBuffered := True;

  ReltimeTree := TTreeBox.Create(Self, tttReltimeTree);
  ReltimeTree.Parent := TreeTabControl;
  ReltimeTree.Align := alClient;
  ReltimeTree.Visible := False;
  ReltimeTree.Enabled := False;
  ReltimeTree.Top := 0;
  ReltimeTree.Left := 0;
  ReltimeTree.Cursor := crDefault;
  ReltimeTree.OnChange := TreeChange;
  ReltimeTree.DragMode := dmManual;
  ReltimeTree.OnMouseDown    := TreeMouseDown;
  ReltimeTree.OnMouseUp      := TreeMouseUp;
  ReltimeTree.OnDragOver     := TreeDragOver;
  ReltimeTree.OnEndDrag      := TreeEndDrag;
  ReltimeTree.OnSearch       := TreeSearch;
  ReltimeTree.DoubleBuffered := True;
  ReltimeTree.IsLinearized := True;

  OriTree.ProgressCallback := UpdateRefreshProgress;
  OriTree.StatusCallback := UpdateRefreshStatus;
  OriTree.BeginRefreshCallback := BeginRefresh;
  OriTree.EndRefreshCallback := EndRefresh;

  BootTree.ProgressCallback := UpdateRefreshProgress;
  BootTree.StatusCallback := UpdateRefreshStatus;
  BootTree.BeginRefreshCallback := BeginRefresh;
  BootTree.EndRefreshCallback := EndRefresh;

  ReltimeTree.ProgressCallback := UpdateRefreshProgress;
  ReltimeTree.StatusCallback := UpdateRefreshStatus;
  ReltimeTree.BeginRefreshCallback := BeginRefresh;
  ReltimeTree.EndRefreshCallback := EndRefresh;

  InitFormPropertyStorage;
end;

function TTreeViewForm.PreserveOriginalTreeRoot: Boolean;
begin
  Result := False;
  if Assigned(MAI) then
  begin
    if MAI.MyTreePack.DoesContain(ttUserTree) and (not MAI.MyTreePack.DoesContain(ttInferTree)) then
      Result := True
  end;
end;

procedure TTreeViewForm.OnTreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FFormIsClosing then Exit;
  MousePos.X := X +ClientOrigin.X +TreeTabControl.Left +TreeTabControl.DisplayRect.Left
                  -Tree.ScrollLeft;
  MousePos.Y := Y +ClientOrigin.Y +TreeTabControl.Top +TreeTabControl.DisplayRect.Top
                  -Tree.ScrollTop;

  ScrollPos.X := Tree.ScrollLeft;
  ScrollPos.Y := Tree.ScrollTop;
end;

procedure TTreeViewForm.TreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FFormIsClosing then Exit;
  try
     with Tree do
     begin
       GetCursorPos(FDragStartPoint);
       FDragStartPoint := ScreenToClient(FDragStartPoint);
       FStartPixelsPerOTU := PixelsPerOTU;
       FStartTreeWidth := TreeWidth;
       FStartTreeHeight := TreeHeight;
       FStartTreeRadius := Radius;
       if Button = mbRight then
       begin
         FocusOnPoint(FDragStartPoint);
       end;
       if InResizeMode then
       begin
         if (Button = mbLeft) and (not Dragging) then
           BeginDrag(true);
         {$IFDEF LINUX}
	 if Assigned(CaptionViewer) then
	   FreeAndNil(CaptionViewer);
         InitCaptionViewer;
         RefreshCaption;
         CaptionViewer.Invalidate;
         {$ENDIF}
       end
       else
         OnTreeMouseDown(Sender, Button, Shift, X, Y);
     end;
   except
     on E:Exception do
       ShowMessage('Oh no! An error occurred in the mouse down procedure: ' + E.Message);
   end;
end;

procedure TTreeViewForm.OnTreeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  if FFormIsClosing then Exit;
  p.X := X + Tree.Left + 2;
  p.Y := Y + TreeTabControl.Top;
  p := ClientToScreen(p);
  if Button = mbRight then
    TreePopupMenu.Popup(p.x, p.y)
  else
    OnTreeClick(X, Y);
end;

procedure TTreeViewForm.TreeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FFormIsClosing then Exit;
  try
    if InResizeMode then
      Tree.EndDrag(true)
    else
      OnTreeMouseUp(Sender, Button, Shift, X, Y);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred in the mouse up procedure: ' + E.Message);
  end;
end;

procedure TTreeViewForm.OnTreeClick(x, y: integer);
begin
  if (not Assigned(Tree)) or FFormIsClosing then
    Exit;
  if not (Tree.NodeFocused or Tree.BranchFocused) then
    Exit;

  if CurTool = ActionPoint then
  begin
    with FTreeInfoFrame do
      if InfoDisplay.Visible and ((PageControl.Activepage = GeneralInfoTab) or (PageControl.Activepage = TreeInfoTab)) then
        if Tree.ShowCharState then
          PageControl.Pages[3].Show
        else
          PageControl.Pages[2].Show;
  end
  else
  begin
    CurTool.Execute;
    FIgnoreProgressUpdates := True;
    //Tree.ClearFocus;
    FIgnoreProgressUpdates := False;
  end;

  if ActionInfo.Visible then
    RenewBranchInfo;
end;

procedure TTreeViewForm.TreeChange(Sender: TObject);
begin
  if FFormIsClosing then Exit;
  if ActionInfo.Checked then
    RenewBranchInfo;
end;

procedure TTreeViewForm.TreeDragOver(Sender, Source: TObject; X, Y: Integer;State: TDragState; var Accept: Boolean);
var
  PPOTU, NewRadius: Integer;
  CursorPos: TPoint;
  SmallMoveRect: TRect;
  ButtonDown: SHORT;

  function IsInsideRect(aRect: TRect; aPoint: TPoint): Boolean;
  begin
    Result := False;
    if (aPoint.X < aRect.Left) or (aPoint.x > aRect.Right) then
      Exit;
    if (aPoint.Y < aRect.Top) or (aPoint.y > aRect.Bottom) then
      Exit;
    Result := True;
  end;

begin
  CursorPos := TPoint.Create(0, 0);
  try
    FIgnoreProgressUpdates := True;
    GetCursorPos(CursorPos);
    CursorPos := Tree.ScreenToClient(CursorPos);
    SmallMoveRect := rect(FDragLastPoint.X-2, FDragLastPoint.Y-2, FDragLastPoint.X+2, FDragLastPoint.Y+2);
    if IsInsideRect(SmallMoveRect, CursorPos) or (not InResizeMode) then
      Exit;
    with Tree do
    begin
      case Tree.TreeStyle of
        tsTraditional:
        begin
          if GetSystemMetrics(SM_SWAPBUTTON) <> 0 then { if true, the left and right mouse buttons have been swapped}
            ButtonDown := GetKeyState(VK_RBUTTON)
          else
            ButtonDown := GetKeyState(VK_LBUTTON);
          if ButtonDown < 0 then { then the button is still down}
          begin
            PPOTU := FStartPixelsPerOTU - ((FDragStartPoint.Y - CursorPos.Y) div 4);
            if PPOTU < 1 then
              PPOTU := 1;
            {$IFDEF DARWIN}
            PPOTU := Max(8, PPOTU);
            {$ENDIF}
            FDragLastPoint := CursorPos;
            if PPOTU < PixelsPerOTU then
              PixelsPerOTU := Max(PixelsPerOTU - 4, PPOTU)
            else
              PixelsPerOTU := Min(PixelsPerOTU + 4, PPOTU);
            TreeWidth := FStartTreeWidth - ((FDragStartPoint.X - CursorPos.X) div 1);
            OnMouseUp := nil;
            {$IFNDEF DARWIN}
            if Tree.NoOfOTUs < TOO_MANY_TAXA_CUTOFF then
              tree.BuildNoDraw;
            Width := Min(MAX_CUSTOM_CONTROL_DIMENSION, MinWidth);
            Height := Min(MAX_CUSTOM_CONTROL_DIMENSION, MinHeight);
            if Width < (Tree.Width - GetSystemMetrics(SM_CXVSCROLL)) then
              Width := Tree.Width - GetSystemMetrics(SM_CXVSCROLL);
            if Height < Tree.Height - GetSystemMetrics(SM_CXVSCROLL) then
              Height := Tree.Height - GetSystemMetrics(SM_CXVSCROLL);
            if Tree.NoOfOTUs < TOO_MANY_TAXA_CUTOFF then
              Refresh;
            {$ENDIF}
          end
          else
            Tree.Refresh;
          OnMouseUp := TreeMouseUp;
        end;
        tsCircle:
        begin
          FDragLastPoint := CursorPos;
          NewRadius := FStartTreeRadius - (FDragStartPoint.X - CursorPos.X) - (FDragStartPoint.Y - CursorPos.Y);
          if NewRadius > 0 then
            Radius := NewRadius;  // With only one element to change we can just use the absolute distance.
          OnMouseUp := nil;
          if Tree.NoOfOTUs < TOO_MANY_TAXA_CUTOFF then
            tree.BuildTree;
          Width := MinWidth;
          Height := MinHeight;
          if Width < (Tree.Width - GetSystemMetrics(SM_CXVSCROLL)) then
            Width := Tree.Width - GetSystemMetrics(SM_CXVSCROLL);
          if Height < Tree.Height - GetSystemMetrics(SM_CXVSCROLL) then
            Height := Tree.Height - GetSystemMetrics(SM_CXVSCROLL);
          if Tree.NoOfOTUs < TOO_MANY_TAXA_CUTOFF then
            Refresh;
          OnMouseUp := TreeMouseUp;
        end;
        tsRadiation:
        begin
          FDragLastPoint := CursorPos;
          TreeWidth := FStartTreeWidth - (FDragStartPoint.X - CursorPos.X)- (FDragStartPoint.Y - CursorPos.Y);  // With only one element to change we can just use the absolute distance.
          OnMouseUp := nil;
          // When we build with autosize it calculates the height and width necessary to hold the tree.
          // We can't just turn autosize on because sometimes it will make the tree component's width & height samller than the container it's in which would prevent the onDrag event from working properly.
          // This could be refactored to look better (probably a function which just calculated the MinWidth & MinHeight and returned it.
          if Tree.NoOfOTUs < TOO_MANY_TAXA_CUTOFF then
            tree.BuildTree;
          Width := MinWidth;
          Height := MinHeight;
          if Width < (Tree.Width - GetSystemMetrics(SM_CXVSCROLL)) then
            Width := Tree.Width - GetSystemMetrics(SM_CXVSCROLL);
          if Height < Tree.Height - GetSystemMetrics(SM_CXVSCROLL) then
            Height := Tree.Height - GetSystemMetrics(SM_CXVSCROLL);
          if Tree.NoOfOTUs < TOO_MANY_TAXA_CUTOFF then
            Refresh;
          OnMouseUp := TreeMouseUp;
        end;
      end;
      Self.Invalidate;
    end;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred during the drag operation' + E.Message);
  end;
end;

procedure TTreeViewForm.TreeEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  try
    FIgnoreProgressUpdates := False;
    if Tree.NoOfOTUs >= TOO_MANY_TAXA_CUTOFF then
      Tree.Refresh;
    if InResizeMode then
    begin
      QuitResizeMode;
    end;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when dragging over the tree: ' + E.Message);
  end;
end;

procedure TTreeViewForm.TreeSearch(Sender: TObject);
begin
  with Tree do
  begin
    if (FocusedIndex > 0) and (FocusedIndex <= NoOfOTUs) then
      TreeJumpToTreeName(OTUName[FocusedIndex], False)
  end;
end;

function TTreeViewForm.IsShowingBootTree: Boolean;
begin
  Result := False;
  if TreeTabControl.TabIndex < 0 then
    Exit;
  if (TreeTabControl.TabIndex = TreeTabControl.Tabs.IndexOf(BOOT_TREE_TAB)) or (TreeTabControl.TabIndex = TreeTabControl.Tabs.IndexOf(BOOT_CONS_TREE_TAB)) then
    Result := True;
end;

function TTreeViewForm.IsShowingRelTimeTree: Boolean;
begin
  Result := False;
  if TreeTabControl.TabIndex < 0 then
    Exit;
  if (TreeTabControl.TabIndex = TreeTabControl.Tabs.IndexOf(TIME_TREE_TAB))  then
    Result := True;
end;

function TTreeViewForm.ClearPrevHighlightedTaxa: Boolean;
begin
  result := false;
  if FTreeNameHighlighted <> EmptyStr then
  begin
    Tree.SetCustomHilightColorOTUName(Tree.IndexOfName[FTreeNameHighlighted], True, clWhite);
    result := true;
  end;
end;

procedure TTreeViewForm.TreeJumpToTreeName(TreeName: String; ColorResult: Boolean);
var
  TreeNameIndex: Integer;
  TreeNameCoords: TRect;
begin
  with Tree do
  begin
    TreeNameIndex := IndexOfName[TreeName];  // If name exists in the tree
    if TreeNameIndex = -1 then
      exit;
    // Set the ScrollAssist over the name and scroll so that it is in view.
    TreeNameCoords := NodeCoords(TreeNameIndex);
    ScrollAssist.Left := TreeNameCoords.Left;

    ScrollAssist.Top := TreeNameCoords.Top - Tree.Top;
    Tree.ScrollTo(ScrollAssist.Left, ScrollAssist.Top);

    if ColorResult then
    begin
      // Now highlight the selected TreeName
      if FTreeNameHighlighted <> TreeName then
        ClearPrevHighlightedTaxa;
      SetCustomHilightColorOTUName(IndexOfName[TreeName], true, clBlue);
      FTreeNameHighlighted := TreeName;
      FocusOnName(IndexOfName[TreeName]);
    end;
    Refresh;
  end;
end;

procedure TTreeViewForm.TreeJumpToTreeNode(Index: Integer; ColorResult: Boolean);
var
  TreeNameCoords: TRect;
begin
  with Tree do
  begin

    if Index = -1 then
      exit;
    // Set the ScrollAssist over the name and scroll so that it is in view.
    TreeNameCoords := NodeCoords(Index);
    ScrollAssist.Left := TreeNameCoords.Left;

    ScrollAssist.Top := TreeNameCoords.Top - Tree.ScrollTop;
    Tree.ScrollTo(ScrollAssist.Left, ScrollAssist.Top);

    if ColorResult then
    begin
      // Now highlight the selected TreeName
      ClearPrevHighlightedTaxa;
      SetCustomHilightColorOTUName(Index, true, clBlue);
      FocusOnNode(Index);
    end;
    Refresh;
  end;
end;

procedure TTreeViewForm.ScrollToShowNode(NodeIndex: Integer);
var
  NodeCoordinates: TRect;
begin
  if (Tree.Height < ClientHeight) and (Tree.Width < ClientWidth) then { it is already in view}
  begin
    Tree.FocusOnNode(NodeIndex);
    Tree.Invalidate;
    Exit;
  end;
  with Tree do
  begin
    if (NodeIndex < 1) or (NodeIndex > NoOfNodes) then
      exit;
    // Set the ScrollAssist over the node and scroll so that it is in view.
    NodeCoordinates := NodeCoords(NodeIndex);
    ScrollAssist.Left := NodeCoordinates.Left;
    ScrollAssist.Top := NodeCoordinates.Top - Tree.Top;
    Tree.ScrollTo(ScrollAssist.Left div 2, ScrollAssist.Top div 2);
    Invalidate;
  end;
end;

function TTreeViewForm.InResizeMode: Boolean;
begin
  Result := (ActionResize.ImageIndex = ENTERRESIZEMODE_IMAGE);
end;

procedure TTreeViewForm.QuitResizeMode;
begin
  try
    Tree.IsResizing := False;
    FIgnoreProgressUpdates := False;
    ActionResize.ImageIndex := QUITRESIZEMODE_IMAGE;
    CurTool := ActionPoint;
    Tree.Cursor := crDefault;
    Tree.DragCursor := crDefault;
    Tree.ShowSelection := True;
    Tree.SetAttrIndex;
    Tree.Refresh;
    Tree.AutoSize := True;  // This makes sure that the Tree resizes properly after resizing.
    Tree.ResetScrollBounds;
    Tree.Invalidate;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred after resizing: ' + E.Message);
  end;
end;

procedure TTreeViewForm.RenewBranchInfo;
const
  STD_ERRS = 1.96;
  STD_ERR_STR = '(95%)';
var
  i: integer;
  LowerBound: Extended;
  UpperBound: Extended;
  earliestDate: String = '';
  latestDate: String = '';
  nearestTipNodes: TList = nil;
  furthestTipNodes: TList = nil;
  tipNames: TStringList = nil;
begin
  if ActionInfo.Checked = False then Exit;

  try
    for i := 0 to 10 do
    begin
      FBInfoTypeStrings[i] := '';
      FBInfoStrings[i] := '';
    end;

    if Tree.HasNonZeroBranchLength and
       (Tree.NodeFocused or Tree.BranchFocused) and
       (not (Tree = ReltimeTree)) and
       (Assigned(MAI) and (not (MAI.MyUsrOperation = dtdoUPGMATree))) then
    begin
      nearestTipNodes := Tree.NearestTipNodes(Tree.FocusedIndex);
      furthestTipNodes := Tree.FurthestTipNodes(Tree.FocusedIndex);
    end;

    FMaxBInfoIndex := 0;
    if Tree.NodeFocused then
    begin
      FBInfoTypeStrings[FMaxBInfoIndex] := 'Node';
      FBInfoStrings[FMaxBInfoIndex] := IntToStr(Tree.NodeInfo.index);
      if Tree.FindEarliestAndLatestDates(earliestDate, latestDate) then
      begin
        inc(FMaxBInfoIndex);
        FBInfoTypeStrings[FMaxBInfoIndex] := 'Earliest Date';
        FBInfoStrings[FMaxBInfoIndex] := earliestDate;
        inc(FMaxBInfoIndex);
        FBInfoTypeStrings[FMaxBInfoIndex] := 'Latest Date';
        FBInfoStrings[FMaxBInfoIndex] := latestDate;
      end;

      if Assigned(nearestTipNodes) and (nearestTipNodes.Count > 0) then
      begin
        inc(FMaxBInfoIndex);
        FBInfoTypeStrings[FMaxBInfoIndex] := 'Nearest Leaf Node(s)';
        FBInfoStrings[FMaxBInfoIndex] := TaxaListString(nearestTipNodes);
      end;
      if Assigned(furthestTipNodes) and (furthestTipNodes.Count > 0) then
      begin
        inc(FMaxBInfoIndex);
        FBInfoTypeStrings[FMaxBInfoIndex] := 'Furthest Leaf Node(s)';
        FBInfoStrings[FMaxBInfoIndex] := TaxaListString(furthestTipNodes);
      end;

      inc(FMaxBInfoIndex);
      FBInfoTypeStrings[FMaxBInfoIndex] := 'No. of Leaf Nodes';
      FBInfoStrings[FMaxBInfoIndex] := IntToStr(Tree.NodeInfo.Size);

      if (Tree.IsLinearized or Tree.IsSamplingTimes) and (not Tree.FocusedOnOutgroup) then
      begin
        Inc(FMaxBInfoIndex);
        if (Tree.NumCalibrations > 0) or ShowingStrictClockTree then
        begin
          FBInfoTypeStrings[FMaxBInfoIndex] := 'Divergence Time';
          if Assigned(ReltimeAnalyzer) then
          begin
            if Tree.FocusedIndex <= Tree.NoOfOTUs then
              FBInfoStrings[FMaxBInfoIndex] := Tree.GetFormattedDivergenceTimeString(Tree.IsSamplingTimes)
            else
            begin
              LowerBound := Tree.GetMinDivergenceTime;
              UpperBound := Tree.GetMaxDivergenceTime;
              FBInfoStrings[FMaxBInfoIndex] := Tree.GetFormattedDivergenceTimeString(Tree.IsSamplingTimes);
              inc(FMaxBInfoIndex);
              FBInfoTypeStrings[FMaxBInfoIndex] := 'CI Lower Bound';
              FBInfoStrings[FMaxBInfoIndex] := Tree.GetFormattedDivergenceTimeString(LowerBound, Tree.IsSamplingTimes);
              inc(FMaxBInfoIndex);
              FBInfoTypeStrings[FMaxBInfoIndex] := 'CI Upper Bound';
              FBInfoStrings[FMaxBInfoIndex] := Tree.GetFormattedDivergenceTimeString(UpperBound, Tree.IsSamplingTimes);
            end
          end
          else
            FBInfoStrings[FMaxBInfoIndex] := Tree.GetFormattedDivergenceTimeString(Tree.IsSamplingTimes);
          if Tree.IsSamplingTimes then
          begin
            inc(FMaxBInfoIndex);
            FBInfoTypeStrings[FMaxBInfoIndex] := 'Latest Sample Time';
            FBInfoStrings[FMaxBInfoIndex] := Tree.GetFormattedDivergenceTimeString(Tree.LatestSampleTime, Tree.IsSamplingTimes);
            inc(FMaxBInfoIndex);
            FBInfoTypeStrings[FMaxBInfoIndex] := 'Days Elapsed (from latest sample time)';
            try
              FBInfoStrings[FMaxBInfoIndex] := Format('%.0n', [Tree.GetDaysElapsed*1.0]);
            except
              on E:Exception do
                FBInfoStrings[FMaxBInfoIndex] := '?';
            end;
          end;
        end
        else
        begin
          if Tree.FocusedIndex > Tree.NoOfOTUs then
          begin
            FBInfoTypeStrings[FMaxBInfoIndex] := 'Relative Time';
            if Assigned(ReltimeAnalyzer)  then
            begin
              FBInfoStrings[FMaxBInfoIndex] := Trim(FormatDoubleSafe(Tree.NodeInfo.height, Tree.DivTimeDecimals, 15));
              inc(FMaxBInfoIndex);
              LowerBound := Tree.GetMinDivergenceTime;
              UpperBound := Tree.GetMaxDivergenceTime;
              FBInfoTypeStrings[FMaxBInfoIndex] := 'CI Lower Bound';
              FBInfoStrings[FMaxBInfoIndex] := Trim(FormatDoubleSafe(LowerBound, Tree.DivTimeDecimals, 15));
              inc(FMaxBInfoIndex);
              FBInfoTypeStrings[FMaxBInfoIndex] := 'CI Upper Bound';
              FBInfoStrings[FMaxBInfoIndex] := Trim(FormatDoubleSafe(UpperBound, Tree.DivTimeDecimals, 15));
            end
            else
              FBInfoStrings[FMaxBInfoIndex] := FloatToStrF(Tree.NodeInfo.height, ffFixed, 15, Tree.DivTimeDecimals);
          end;
        end;
        if Tree.FocusedIndex > Tree.NoOfOTUs then
        begin
          if Assigned(FRelTimeAnalyzer) and (FReltimeAnalyzer.IsBlensOnly = False) then
          begin
            inc(FMaxBInfoIndex);
            FBInfoTypeStrings[FMaxBInfoIndex] := 'Data Coverage';
            FBInfoStrings[FMaxBInfoIndex] := Format('%d%%', [Round(Tree.NodeInfo.DataCoverage * 100)]);
          end;
        end;
      end;
      if Tree.IsGeneDups then
      begin
        inc(FMaxBInfoIndex);
        if not Tree.NodeInfo.IsOtu then
        begin
          FBInfoTypeStrings[FMaxBInfoIndex] := 'Is Gene Duplication Event';
          FBInfoStrings[FMaxBInfoIndex] := BoolToStr(Tree.NodeInfo.IsGeneDup, True);
          inc(FMaxBInfoIndex);
          if Tree.HasSpeciations then
          begin
            FBInfoTypeStrings[FMaxBInfoIndex] := 'Is Speciation Event';
            FBInfoStrings[FMaxBInfoIndex] := BoolToStr(Tree.NodeInfo.IsSpeciation, True);
          end;
        end;
      end;
    end
    else if Tree.BranchFocused then
    begin
      FBInfoTypeStrings[FMaxBInfoIndex] := 'Branch';
      FBInfoStrings[FMaxBInfoIndex] := 'Node ' + IntToStr(Tree.BranchInfo.ancnodeindex) + ' to ' + IntToStr(Tree.BranchInfo.nodeindex);
      if Tree.FindEarliestAndLatestDates(earliestDate, latestDate) then
      begin
        inc(FMaxBInfoIndex);
        FBInfoTypeStrings[FMaxBInfoIndex] := 'Earliest Date';
        FBInfoStrings[FMaxBInfoIndex] := earliestDate;
        inc(FMaxBInfoIndex);
        FBInfoTypeStrings[FMaxBInfoIndex] := 'Latest Date';
        FBInfoStrings[FMaxBInfoIndex] := latestDate;
      end;

      if Assigned(nearestTipNodes) and (nearestTipNodes.Count > 0) then
      begin
        inc(FMaxBInfoIndex);
        FBInfoTypeStrings[FMaxBInfoIndex] := 'Nearest Leaf Node(s)';
        FBInfoStrings[FMaxBInfoIndex] := TaxaListString(nearestTipNodes);
      end;
      if Assigned(furthestTipNodes) and (furthestTipNodes.Count > 0) then
      begin
        inc(FMaxBInfoIndex);
        FBInfoTypeStrings[FMaxBInfoIndex] := 'Furthest Leaf Node(s)';
        FBInfoStrings[FMaxBInfoIndex] := TaxaListString(furthestTipNodes);
      end;

      if (MLAnalyzer <> nil) and Tree.IsLinearized then
      begin
        Inc(FMaxBInfoIndex);
        if ShowingStrictClockTree and (Tree.TimeFactor > 0) then
        begin
          FBInfoTypeStrings[FMaxBInfoIndex] := 'Strict Clock Rate';
          FBInfoStrings[FMaxBInfoIndex] := Trim(FormatDoubleSafe(1/ReltimeTree.TimeFactor, Tree.BLenDecimals, 15));
        end
        else
        begin
          FBInfoTypeStrings[FMaxBInfoIndex] := 'Local Clock Rate';
          FBInfoStrings[FMaxBInfoIndex] := Trim(FormatDoubleSafe(FReltimeAnalyzer.Node[Tree.FocusedIndex-1].rate, Tree.BLenDecimals, 15));
        end;
        Inc(FMaxBInfoIndex);
        FBInfoTypeStrings[FMaxBInfoIndex] := 'Branch length';

        FBInfoStrings[FMaxBInfoIndex] := FloatToStrF(Tree.BranchInfo.length, ffFixed, 15, Tree.BLenDecimals);
        if (Tree.BranchInfo.BranchType = btRootedInterior) or
           (Tree.BranchInfo.BranchType = btRootedExterior) then
          FBInfoStrings[FMaxBInfoIndex] := FBInfoStrings[FMaxBInfoIndex]+'('+FloatToStrF(Tree.BranchInfo.totallength*FReltimeAnalyzer.Node[Tree.FocusedIndex-1].rate, ffFixed, 15, Tree.BLenDecimals)+')';
        if Tree.isSE and (not (Tree.TreeBoxType = tttReltimeTree)) then
        begin
          LowerBound := Tree.BranchInfo.totallength*FReltimeAnalyzer.Node[Tree.FocusedIndex-1].rate  - STD_ERRS * Tree.BranchInfo.SE;
          UpperBound := Tree.BranchInfo.totallength*FReltimeAnalyzer.Node[Tree.FocusedIndex-1].rate  + STD_ERRS * Tree.BranchInfo.SE;
          FBInfoStrings[FMaxBInfoIndex] := FBInfoStrings[FMaxBInfoIndex];
          inc(FMaxBInfoIndex);
          FBInfoTypeStrings[FMaxBInfoIndex] := 'CI Lower Bound';
          FBInfoStrings[FMaxBInfoIndex] := Trim(FormatDoubleSafe(LowerBound, Tree.DivTimeDecimals, 15));
          inc(FMaxBInfoIndex);
          FBInfoTypeStrings[FMaxBInfoIndex] := 'CI Upper Bound';
          FBInfoStrings[FMaxBInfoIndex] := Trim(FormatDoubleSafe(UpperBound, Tree.DivTimeDecimals, 15));
        end;
      end
      else if Assigned(MAI) and IsReltimeNonMLAnalysis(MAI.InitialUsrOperation) then
      begin
        Inc(FMaxBInfoIndex);
        if ShowingStrictClockTree and (Tree.TimeFactor > 0) then
        begin
          FBInfoTypeStrings[FMaxBInfoIndex] := 'Strict Clock Rate';
          FBInfoStrings[FMaxBInfoIndex] := Trim(FormatDoubleSafe(1/Tree.TimeFactor, Tree.BLenDecimals, 15));
        end
        else
        begin
          FBInfoTypeStrings[FMaxBInfoIndex] := 'Local Clock Rate';
          FBInfoStrings[FMaxBInfoIndex] := Trim(FormatDoubleSafe(ReltimeAnalyzer.Node[Tree.FocusedIndex - 1].Rate, Tree.BLenDecimals, 15));
        end;

        Inc(FMaxBInfoIndex);
        FBInfoTypeStrings[FMaxBInfoIndex] := 'Branch length';
        FBInfoStrings[FMaxBInfoIndex] := Trim(FormatDoubleSafe(Tree.BranchInfo.length, Tree.BLenDecimals, 15));
        if (Tree.BranchInfo.BranchType = btRootedInterior) or
           (Tree.BranchInfo.BranchType = btRootedExterior) then
          FBInfoStrings[FMaxBInfoIndex] := FBInfoStrings[FMaxBInfoIndex]+'('+FloatToStrF(Tree.BranchInfo.totallength * ReltimeAnalyzer.Node[Tree.FocusedIndex - 1].Rate, ffFixed, 15, Tree.BLenDecimals)+')';
        if Tree.isSE then
          FBInfoStrings[FMaxBInfoIndex] := FBInfoStrings[FMaxBInfoIndex]+' +- '+ Trim(FormatDoubleSafe(STD_ERRS*Tree.BranchInfo.SE, Tree.BLenDecimals, 15)) + ' ' + STD_ERR_STR;
      end
      else if Tree.isBranchLength and Tree.DataInitialized[Tree.TreeIndex] then
      begin
        Inc(FMaxBInfoIndex);
        FBInfoTypeStrings[FMaxBInfoIndex] := 'Branch length';
        FBInfoStrings[FMaxBInfoIndex] := FloatToStrF(Tree.BranchInfo.length, ffFixed, 15, Tree.BLenDecimals);
        if (Tree.BranchInfo.BranchType = btRootedInterior) or
           (Tree.BranchInfo.BranchType = btRootedExterior) then
          FBInfoStrings[FMaxBInfoIndex] := FBInfoStrings[FMaxBInfoIndex]+'('+FloatToStrF(Tree.BranchInfo.totallength, ffFixed, 15, Tree.BLenDecimals)+')';
        if Tree.isSE then
          FBInfoStrings[FMaxBInfoIndex] := FBInfoStrings[FMaxBInfoIndex]+' +- '+FloatToStrF(Tree.BranchInfo.SE, ffFixed, 15, Tree.BLenDecimals);
      end;
      if Tree.isStats then
      begin
        Inc(FMaxBInfoIndex);
        FBInfoTypeStrings[FMaxBInfoIndex] := Tree.StatsName;
        if (Tree.BranchInfo.BranchType = btExterior) or (Tree.BranchInfo.BranchType = btRootedExterior) then
          FBInfoStrings[FMaxBInfoIndex] := 'n/c'
        else if (Tree.MaxStats > 1.0) and (Frac(Tree.BranchInfo.stats) = 0.0) then
          FBInfoStrings[FMaxBInfoIndex] := IntToStr(Trunc(Tree.BranchInfo.stats+0.000000000001)) +' / '
                                           +FloatToStrF(Tree.MaxStats, ffFixed, 15, 0)
        else if Tree.MaxStats = 100 then
          FBInfoStrings[FMaxBInfoIndex] := IntToStr(Trunc(Tree.BranchInfo.stats+0.000000000001)) +' %'
        else
          FBInfoStrings[FMaxBInfoIndex] := IntToStr(Trunc(Tree.BranchInfo.stats+0.000000000001));
      end;
    end;

    tipNames := TStringList.Create;
    if Tree.NodeFocused or Tree.BranchFocused then
      Tree.GetOtuNamesInCluster(Tree.FocusedIndex, tipNames);
    FTreeInfoFrame.DisplayBranchInfo(FBInfoTypeStrings, FBInfoStrings, FMaxBInfoIndex, tipNames);
  finally
    if Assigned(nearestTipNodes) then
      nearestTipNodes.Free;
    if Assigned(furthestTipNodes) then
      furthestTipNodes.Free;
    if Assigned(tipNames) then
      tipNames.Free;
  end;

  if isAncState then
    RenewAncState;
end;

procedure TTreeViewForm.RenewTreeInfo;
var
  ModelInfo: TModelInfo = nil;
  str: AnsiString = '';
  i: integer;
begin
  for i := 0 to 10 do
  begin
    TreeInfoType[i] := '';
    TreeInfo[i] := '';
  end;
  MaxTreeInfoIndex := 0;
  TreeInfoType[MaxTreeInfoIndex] := 'Type';
  if Tree.IsGeneDups then
    TreeInfo[MaxTreeInfoIndex] := 'Rooted Gene Tree'
  else if Tree.isRooted then
    TreeInfo[MaxTreeInfoIndex]   := 'Rooted'
  else
    TreeInfo[MaxTreeInfoIndex]   := 'Unrooted';

  if Tree.NoOfTrees > 1 then
  begin
    Inc(MaxTreeInfoIndex);
    TreeInfoType[MaxTreeInfoIndex] := 'Total # of trees';
    TreeInfo[MaxTreeInfoIndex]     := IntToStr(Tree.NoOfTrees);

    Inc(MaxTreeInfoIndex);
    TreeInfoType[MaxTreeInfoIndex] := 'Tree #';
    if Tree.TreeIndex = 0 then
      TreeInfo[MaxTreeInfoIndex] := 'Consensus'
    else
      TreeInfo[MaxTreeInfoIndex] := IntToStr(Tree.TreeIndex);
  end;

  inc(MaxTreeInfoIndex);
  TreeInfoType[MaxTreeInfoIndex] := '# of Taxa';
  TreeInfo[MaxTreeInfoIndex] := IntToStr(Tree.NoOfOTUs);

  if Tree.isBranchLength and (not Assigned(FComputeParsimInfo)) then
  begin
    Inc(MaxTreeInfoIndex);
    TreeInfoType[MaxTreeInfoIndex] := 'SBL';
    TreeInfo[MaxTreeInfoIndex] := FloatToStrF(Tree.SBL[Tree.TreeIndex],ffFixed,18,8);
  end;


  if Tree.isStats and (Tree.IsCondensed) then
  begin
    Inc(MaxTreeInfoIndex);
    if Tree.TreeIndex = 0 then
    begin
      TreeInfoType[MaxTreeInfoIndex] := 'Consensus value';
      TreeInfo[MaxTreeInfoIndex]     := IntToStr(Tree.ConsensusValue);
    end
    else
    begin
      TreeInfoType[MaxTreeInfoIndex] := 'Condensed value';
      TreeInfo[MaxTreeInfoIndex]     := IntToStr(Tree.CondenseValue);
    end;
  end;

  if MLAnalyzer <> nil then
  begin
    ModelInfo := TModelInfo.Create;
    MLAnalyzer.GetModelInfo(ModelInfo);

    Inc(MaxTreeInfoIndex);
    TreeInfoType[MaxTreeInfoIndex] := 'Log Likelihood';
    TreeInfo[MaxTreeInfoIndex] := FloatToStrF(ModelInfo.LogL,ffFixed,18,Tree.ValueDecimals);

    Inc(MaxTreeInfoIndex);
    TreeInfoType[MaxTreeInfoIndex] := 'Model';
    TreeInfo[MaxTreeInfoIndex]     := ModelInfo.ModelName;
    if ModelInfo.DataType = 'DNA' then
    begin
      Inc(MaxTreeInfoIndex);
      TreeInfoType[MaxTreeInfoIndex] := 'Ts/Tv';
      TreeInfo[MaxTreeInfoIndex]     := FloatToStrF(ModelInfo.SVR, ffFixed,12, 4);
    end;
    if Pos('+G', ModelInfo.ModelName) > 0 then
    begin
      Inc(MaxTreeInfoIndex);
      TreeInfoType[MaxTreeInfoIndex] := 'Gamma';
      TreeInfo[MaxTreeInfoIndex]     := FloatToStrF(ModelInfo.Gamma, ffFixed,12, 4);
    end;
    if ModelInfo.NoOfRates > 1 then
    begin
      str := str +FloatToStrF(ModelInfo.Rate[0], ffFixed,12, 4);
      for i := 1 to ModelInfo.NoOfRates-1 do
        str := str +', '+FloatToStrF(ModelInfo.Rate[i], ffFixed,12, 4);

      Inc(MaxTreeInfoIndex);
      TreeInfoType[MaxTreeInfoIndex] := 'Discrete Rates';
      TreeInfo[MaxTreeInfoIndex]     := str;
    end;
    if Pos('+I', ModelInfo.ModelName) > 0 then
    begin
      Inc(MaxTreeInfoIndex);
      TreeInfoType[MaxTreeInfoIndex] := 'Invariant';
      TreeInfo[MaxTreeInfoIndex]     := FloatToStrF(ModelInfo.Invar, ffFixed,12, 4);
    end;
    ModelInfo.Free;
  end
  else
  begin
    if Tree.isValue then
    begin
      Inc(MaxTreeInfoIndex);
      TreeInfoType[MaxTreeInfoIndex] := Tree.ValueName;
      TreeInfo[MaxTreeInfoIndex] := FloatToStrF(Tree.Value[Tree.TreeIndex],ffFixed,18,Tree.ValueDecimals);
    end;
    if Tree.isValue2 then
    begin
      Inc(MaxTreeInfoIndex);
      TreeInfoType[MaxTreeInfoIndex] := Tree.Value2Name;
      TreeInfo[MaxTreeInfoIndex] := FloatToStrF(Tree.Value2[Tree.TreeIndex],ffFixed,18,Tree.Value2Decimals);
    end;

    if Tree.isFreq then
    begin
      Inc(MaxTreeInfoIndex);
      TreeInfoType[MaxTreeInfoIndex] := Tree.FreqName;
      TreeInfo[MaxTreeInfoIndex] := FloatToStrF(Tree.Frequency[Tree.TreeIndex],ffFixed,18,Tree.FreqDecimals);
      if (Tree.FreqDecimals = 0) then
        TreeInfo[MaxTreeInfoIndex] := TreeInfo[MaxTreeInfoIndex] +'/ ' +FloatToStrF(Tree.TotalFrequency,ffFixed,18,Tree.FreqDecimals);
    end;
  end;
  if Tree.IsGeneDups then
  begin
    inc(MaxTreeInfoIndex);
    TreeInfoType[MaxTreeInfoIndex] := 'Has Gene Duplication Events';
    TreeInfo[MaxTreeInfoIndex] := BoolToStr(Tree.HasGeneDups, True);
    inc(MaxTreeInfoIndex);
    if MAI.UsedSpeciesTree then
    begin
      TreeInfoType[MaxTreeInfoIndex] := 'Has Speciation Events';
      TreeInfo[MaxTreeInfoIndex] := BoolToStr(Tree.HasSpeciations, True);
    end
    else
    begin
      TreeInfoType[MaxTreeInfoIndex] := 'Has Speciation Events';
      TreeInfo[MaxTreeInfoIndex] := 'Unknown';
    end;
  end;
  FTreeInfoFrame.DisplayTreeInfo(TreeInfoType, TreeInfo, MaxTreeInfoIndex);
end;

procedure TTreeViewForm.RenewAncState;
var
  stl: TStringList;
  i : integer;
  flag: boolean;
begin
  if ActionInfo.Checked = False then Exit;

  if not isAncState then Exit;
  for i := 0 to 30 do begin
    AncStateInfoType[i] := '';
    AncStateInfo[i] := '';
  end;

  MaxAncStateIndex := 0;
  AncStateInfoType[MaxAncStateIndex] := 'Character Type ';

  if MLAnalyzer <> nil then
  begin
    if MLAnalyzer.Model.NoOfStates = 4 then
      AncStateInfo[MaxAncStateIndex] := 'Nucleotide'
    else
      AncStateInfo[MaxAncStateIndex] := 'Amino Acid';

    Inc(MaxAncStateIndex);
    AncStateInfoType[MaxAncStateIndex] := 'Site No. ';
    AncStateInfo[MaxAncStateIndex] := IntToStr(Tree.SiteIndex+1);

    if Tree.NodeFocused then
    begin
      Inc(MaxAncStateIndex);
      AncStateInfoType[MaxAncStateIndex] := 'Node No. ';
      AncStateInfo[MaxAncStateIndex] := IntToStr(Tree.NodeInfo.index);

      Inc(MaxAncStateIndex);
      AncStateInfoType[MaxAncStateIndex] := 'Character States';
      stl := TStringList.Create;

      if IsDeveloper = true then
        flag := MLAnalyzer.GetExpectedStateProb(Tree.FocusedIndex-1, Tree.SiteIndex+1, stl)
      else
        flag := MLAnalyzer.GetAncStateProb(Tree.FocusedIndex-1, Tree.SiteIndex+1, stl);
      if not flag then
      begin
        AncStateInfo[MaxAncStateIndex]     := '-';
        Inc(MaxAncStateIndex);
      end
      else
        for i := 0 to stl.Count-1 do
        begin
          if StrToFloat(stl.Values[stl.Names[i]]) < 0.00005 then
            break;
          AncStateInfoType[MaxAncStateIndex] := stl.Names[i];
          AncStateInfo[MaxAncStateIndex]     := FloatToStrF(StrToFloat(stl.Values[stl.Names[i]])*100, ffFixed, 12, 2)+' %';
          Inc(MaxAncStateIndex);
        end;

      stl.Free;
    end;
  end

  else if @ComputeParsimInfo <> nil then
  begin
    if ComputeParsimInfo.IsNucData then
      AncStateInfo[MaxAncStateIndex] := 'Nucleotide'
    else
      AncStateInfo[MaxAncStateIndex] := 'Amino Acid';

    Inc(MaxAncStateIndex);
    AncStateInfoType[MaxAncStateIndex] := 'Site No. ';
    AncStateInfo[MaxAncStateIndex] := IntToStr(Tree.SiteIndex+1);

    if Tree.NodeFocused then
    begin
      Inc(MaxAncStateIndex);
      AncStateInfoType[MaxAncStateIndex] := 'Node No. ';
      AncStateInfo[MaxAncStateIndex] := IntToStr(Tree.NodeInfo.index);

      Inc(MaxAncStateIndex);
      AncStateInfoType[MaxAncStateIndex] := 'Character States';
      if ActionAncStateShowAll.Enabled then
        if ExtCharAncItem.Checked then
          AncStateInfo[MaxAncStateIndex] := Tree.GetAncState(ShowExtCharAncStateProc, Tree.NodeInfo.index)
        else
          AncStateInfo[MaxAncStateIndex] := Tree.GetAncState(ShowAllAncStateProc, Tree.NodeInfo.index)
      else if ActionAncStateShowMost.Enabled then
        AncStateInfo[MaxAncStateIndex] := Tree.GetAncState(ShowMostProbAncStateProc, Tree.NodeInfo.index)
      else if ActionAncStateHideAmbiguous.Enabled then
        AncStateInfo[MaxAncStateIndex] := Tree.GetAncState(HideAmbigAncStateProc, Tree.NodeInfo.index);
    end
    else
    begin
      AncStateInfo[MaxAncStateIndex] := 'Not specified';
      if ActionAncStateShowAll.Enabled then
        if ExtCharAncItem.Checked and Assigned(ShowExtCharAncStateProc) then
          Tree.GetAncState(ShowExtCharAncStateProc, 1)
        else if Assigned(ShowAllAncStateProc) then
          Tree.GetAncState(ShowAllAncStateProc, 1)
    end;
  end;

  FTreeInfoFrame.DisplayAncState(AncStateInfoType, AncStateInfo, MaxAncStateIndex);
  RenewAncRate;
end;

procedure TTreeViewForm.RenewAncRate;
var
  p: array [0..20] of extended;
  i : integer;
begin
  if ActionInfo.Checked = False then Exit;
  for i := Low(p) to High(p) do
    p[i] := 0;
  if not isAncState then Exit;
  for i := 0 to 30 do begin
    AncRateInfoType[i] := '';
    AncRateInfo[i] := '';
  end;

  MaxAncRateIndex := 0;

  if MLAnalyzer <> nil then
  begin
    if Tree.NodeFocused then
    begin
      if (MLAnalyzer.Model.UseInvar) or (MLAnalyzer.Model.NoOfRates > 1) then
      begin
        MLAnalyzer.GetProbOfRateCategoryAtSite(Tree.SiteIndex+1, p);
        if MLAnalyzer.Model.UseInvar then
        begin
          AncRateInfoType[MaxAncRateIndex] := 'Prob(invariable)';
          AncRateInfo[MaxAncRateIndex] := FloatToStrF(p[MLAnalyzer.Model.NoOfRates], ffFixed, 15, 4);
        end;
        if MLAnalyzer.Model.NoOfRates = 1 then
        begin
          AncRateInfoType[MaxAncRateIndex] := 'Prob(variable)';
          AncRateInfo[MaxAncRateIndex] := FloatToStrF(p[0],ffFixed, 15, 4);
        end
        else
        begin
          for i := 0 to MLAnalyzer.Model.NoOfRates-1 do
          begin
            AncRateInfoType[MaxAncRateIndex] := 'Prob(rate' + EQUALS_SIGN_PLACEHOLDER + FloatToStrF(MLAnalyzer.Model.Rate[i],ffFixed, 15, 4)+')';
            AncRateInfo[MaxAncRateIndex] := FloatToStrF(p[i],ffFixed, 15, 4);
            inc(MaxAncRateIndex);
          end;
        end;
      end;
    end;
  end;

  FTreeInfoFrame.DisplayAncRate(AncRateInfoType, AncRateInfo, MaxAncRateIndex);
end;

function TTreeViewForm.TaxaListString(otuNodes: TList): String;
var
  i: Integer;
  n: TpNode = nil;
begin
  Result := EmptyStr;
  if otuNodes.Count = 1 then
    Result := TpNode(otuNodes[0]).name
  else if otuNodes.Count > 0 then
  begin
    for i := 0 to otuNodes.Count - 1 do
    begin
      n := TpNode(otuNodes[i]);
      Result := Format('%s%s,', [Result, n.name]);
    end;
    Result := copy(Result, 1, Length(Result) - 1);
  end;
end;

procedure TTreeViewForm.SaveGroupOption(a: TNodeAttrib);
var
  f: File;
begin
  if not DirectoryExists(GetPrivateDir + 'Group') then
    if not CreateDir(GetPrivateDir + 'Group') then
      exit;
  try
    AssignFile(f, GetPrivateDir + 'Group' + PathDelim + a.Name + '.dat');
    ReWrite(f, 1);
    a.WriteToFile(f);
  finally
    CloseFile(f);
  end;
end;

procedure TTreeViewForm.LoadGroupOption(a: TNodeAttrib);
var
  f: File;
begin
  if not FileExists(GetPrivateDir+'Group\'+a.Name+'.dat') then
    exit;
  AssignFile(f, GetPrivateDir+'Group\'+a.Name+'.dat');
  Reset(f, 1);
  a.ReadFromFile(f,10, MTS_SESSION_VERSION);
  CloseFile(f);
end;

procedure TTreeViewForm.SetComputeParsimInfo;
begin
  if FComputeParsimInfo = nil then Exit;

  OriTree.BLenFunc := SetAverageMPBranchLength;
  if IsBootTree then
    BootTree.BLenFunc := SetAverageMPBranchLength;

  ShowAllAncStateProc := ShowAllParsimonyAncState;
  ShowExtCharAncStateProc := ShowAllParsimonyAncStateEx;
  HideAmbigAncStateProc := HideAmbigParsimonyAncState;
end;

function TTreeViewForm.SetAverageMPBranchLength(tree: TTreeData): Double;
begin
  Result := -1;
  ComputeParsimInfo.SetTreeData(tree);
  try
    FIsComputingMPBLens := True;
    ComputeParsimInfo.ComputeAvgMPBLens(tree.BLenArray);
  finally
    FIsComputingMPBLens := False;
  end;
end;

procedure TTreeViewForm.GetAncStateProbWYSIWYG(node, site: integer; tree: TMLTreeAnalyzer; var stateOut: AnsiString; var ProbabilityOUT: double; showoption: TAction);
var
  Probability: double;
  State: AnsiString;
  i: integer;
  TempList : TStringList;
begin
  stateOut := EmptyStr;
  TempList := nil;
  ProbabilityOUT := -1;
  try
  TempList :=  TStringList.Create;
  tree.GetAncStateProb(node, site, TempList);
  if showoption = ActionAncStateShowAll then //If the user has selected to show all the possible ancesteral states
  begin
    for i := 0 to TempList.Count-1 do  //Go through each ancesteral state
    begin
      State := TempList.Names[i];   //Extract the state
      Probability := StrToFloat(TempList.values[TempList.Names[i]]);  //Extract the probability of the state

      if Probability >= 0.05 then  //If the state is at least 5% possible, add the state to the list of possible states
        stateOUT := stateOUT + State;
    end;

    ProbabilityOUT := StrToFloat(TempList.Values[TempList.Names[0]]);  //The probability is that of the most probable state /////CHECK THIS WITH DR. KUMAR/////
  end
  else if showoption = ActionAncStateShowMost then
  begin
    stateOut := TempList.Names[0];
    ProbabilityOUT := StrToFloat(TempList.Values[TempList.Names[0]]);
  end
  else if showoption = ActionAncStateHideAmbiguous then
  begin
    if StrToFloat(TempList.Values[TempList.Names[0]]) >= 0.95 then  //If its 95% likely to be just one of them then show it otherwise not
    begin
      stateOut := TempList.Names[0];
      ProbabilityOUT := StrToFloat(TempList.Values[TempList.Names[0]]);
    end;
  end;
  finally
    FreeAndNil(TempList);
  end;
end;

procedure TTreeViewForm.InitCaptionViewer;
begin
  CaptionViewer := TChromium.Create(Self);
  CaptionViewer.Parent := CaptionPanel;
  {$IFDEF LINUX}
  CaptionViewer.Align := alCustom;
  CaptionViewer.Anchors := [akTop, akLeft, akRight, akBottom];
  CaptionViewer.Top := 22;
  CaptionViewer.Left := { CaptionToolbar.Width +} 1;
  CaptionViewer.Width := CaptionPanel.Width { - CaptionToolBar.Width} - 2;
  CaptionViewer.Height := CaptionPanel.Height - 1;
  {$ELSE}
  CaptionViewer.Align := alClient;
  {$ENDIF}
end;

procedure TTreeViewForm.UpdateAncestralStateActions;
begin
  if isAncState then
  begin
    ActionAncStateFont.Enabled := true;
    ActionAncStateShowAll.Enabled := (@ShowAllAncStateProc <> nil);
    ActionAncStateShowMost.Enabled := (@ShowMostProbAncStateProc <> nil);
    ActionAncStateHideAmbiguous.Enabled := (@HideAmbigAncStateProc <> nil);

    ActionAncStateExtended.Enabled := (@ShowExtCharAncStateProc <> nil);
    if Assigned(FAncestorsFrame) then
      FAncestorsFrame.AncExtendedCharsCheckBx.Enabled := ActionAncStateExtended.Enabled;
    ActionAncStateShowNone.Enabled := true;
    ActionAncStateExportAllSites.Enabled := (Tree.ShowCharState and ((MAI.InitialUsrOperation = dtdoMPTree) or (MAI.InitialUsrOperation = dtdoMPInferAncSeq) or (MAI.InitialUsrOperation = dtdoMPComputeUserTreeBLens)));
    if FAncestorsFrame.Visible then
      FAncestorsFrame.UpdateExportOptions(ActionAncStateExportAllSites.Enabled);
    if ActionAncStateShowAll.Enabled then
      ActionAncStateShowAll.Checked := (@Tree.AncStateProc = @ShowAllAncStateProc) and Tree.ShowCharState;
    if ActionAncStateShowMost.Enabled then
      ActionAncStateShowMost.Checked := (@Tree.AncStateProc = @ShowMostProbAncStateProc) and Tree.ShowCharState;
    if ActionAncStateHideAmbiguous.Enabled then
      ActionAncStateHideAmbiguous.Checked := (@Tree.AncStateProc = @HideAmbigAncStateProc) and Tree.ShowCharState;
    if ActionAncStateExtended.Enabled then
      if (@Tree.AncStateProc = @ShowExtCharAncStateProc) and Tree.ShowCharState then
      begin
        ActionAncStateShowAll.Checked := true;
        ActionAncStateExtended.Checked := true;
      end
      else
        ActionAncStateExtended.Checked := false;
    ActionAncStateShowNone.Checked := (@Tree.AncStateProc = nil) or (not Tree.ShowCharState);

    if ActionAncStateShowAll.Checked then
      CurAncState := ActionAncStateShowAll
    else if ActionAncStateShowMost.Checked then
      CurAncState := ActionAncStateShowMost
    else if ActionAncStateHideAmbiguous.Checked then
      CurAncState := ActionAncStateHideAmbiguous
    else if ActionAncStateShowNone.Checked then
      CurAncState := ActionAncStateShowNone;
    FAncestorsFrame.AncSiteNumSpinEdit.Enabled := (not ActionAncStateShowNone.Checked) and (Tree.MaxSiteIndex > 0); // in V2.1
    if  ShowCharStateItem.Enabled <> isAncState then // otherwise it fires too many events
      ShowCharStateItem.Enabled := isAncState; //not ActionAncStateShowNone.Checked;
    FAncestorsFrame.AncSiteNumSpinEdit.MaxValue := Tree.MaxSiteIndex +1;
    ActionActStateExportMostProbableSequence.Enabled := (CurAncState <> ActionAncStateShowNone) and (MLAnalyzer <> nil);
    ActionActStateExportCurrentSite.Enabled := (CurAncState <> ActionAncStateShowNone);
    ActionActStateExportChangesList.Enabled := (CurAncState <> ActionAncStateShowNone);
    ActionAncStateExportPredictLivingSequence.Visible := isDeveloper and (CurAncState <> ActionAncStateShowNone);
    ActionAncStateExportTextExport.Enabled := (CurAncState <> ActionAncStateShowNone) and (MLAnalyzer <> nil);
  end
  else
  begin
    ActionActStateExportMostProbableSequence.Enabled := False;
    ActionActStateExportCurrentSite.Enabled := False;
    ActionActStateExportChangesList.Enabled := False;
    ActionAncStateExportPredictLivingSequence.Visible := False;
    ActionAncStateExportTextExport.Enabled := False;
  end;
end;


procedure TTreeViewForm.SetAncestralSiteNum(aSite: Int64);
begin
  if Assigned(FAncestorsFrame) then
  begin
    if FAncestorsFrame.AncSiteNumSpinEdit.MaxValue < aSite then
      FAncestorsFrame.AncSiteNumSpinEdit.MaxValue := aSite;
    FAncestorsFrame.AncSiteNumSpinEdit.Value := aSite;
  end;
end;

function TTreeViewForm.IsReltimeTree: Boolean;
begin
  Result := ((tttReltimeMain in FTimetreeTypes) or (tttReltimeLocal in FTimetreeTypes));
end;

procedure TTreeViewForm.DisableCaption;
begin
  CaptionHideActionExecute(Self);
  CaptionDockInTEAction.Enabled := False;
  CaptionShowInNewWindowAction.Enabled := False;
  CaptionPrintAction.Enabled := False;
end;

procedure TTreeViewForm.BeginRefresh;
begin
  FRebuildingTree := True;
  if Tree.NoOfOTUs > 100 then
    DisableActions;
end;

procedure TTreeViewForm.EndRefresh;
var
  i: Integer;
begin
  FRebuildingTree := False;
  {$IFDEF DARWIN}
  if Tree.Parent = TreeTabControl then
    TreeTabControlChangeBounds(Self);
  {$ENDIF}
  for i := 0 to MainMenu.Items.Count - 1 do
    MainMenu.Items[i].Enabled := True;
  ProgressBar1.Position := 0;
  ProgressBar1.Invalidate;
end;

procedure TTreeViewForm.UpdateRefreshProgress(AProgress: Integer);
var
  OverallProgress: Integer;
begin
  if Tree.NoOfOTUs < MIN_TAXA_FOR_DRAW_PROGRESS then
    Exit;
  if FIgnoreProgressUpdates or (not Tree.Focused) then
    Exit;
  FRebuildProgress.PhaseProgress := Min(100, AProgress);
  OverallProgress := FRebuildProgress.OverallProgress;
  ProgressBar1.Position := OverallProgress;
  ProgressBar1.Invalidate;
end;

procedure TTreeViewForm.UpdateRefreshStatus(AStatus: TTreeRebuildPhase);
begin
  if Tree.NoOfOTUs < MIN_TAXA_FOR_DRAW_PROGRESS then
    Exit;
  if FIgnoreProgressUpdates or (not Tree.Focused) then
    Exit;
  FRebuildProgress.Phase := AStatus;
  TreeStatusBar.Panels[1].Text := FRebuildProgress.PhaseToString(AStatus);
  TreeStatusBar.Invalidate;
  if AStatus = rpIdle then
    UpdateRefreshProgress(0);
end;

procedure TTreeViewForm.SetUsingSpeciesTree(AValue: Boolean);
begin
  if Assigned(MAI) then
    MAI.UsedSpeciesTree := AValue;
end;

procedure TTreeViewForm.SetGeneTreeRooted(AValue: Boolean);
begin
  if Assigned(MAI) then
    MAI.GeneTreeRooted := AValue;
end;

procedure TTreeViewForm.SetSpeciesTreeRooted(AValue: Boolean);
begin
  if Assigned(MAI) then
    MAI.SpeciesTreeRooted := AValue;
end;

procedure TTreeViewForm.SetNumGeneDups(AValue: Integer);
begin
  if Assigned(MAI) then
    MAI.NumGeneDups := AValue;
end;

procedure TTreeViewForm.SetNumSpeciations(AValue: Integer);
begin
  if Assigned(MAI) then
    MAI.NumSpeciations := AValue;
end;

procedure TTreeViewForm.RootOnNode(Index: Integer);
begin
  Tree.SetIsRooted(False); { other wise it won't work}
  Tree.FocusOnNode(Index);
  ActionRootOnBranchExecute(Self);
  Tree.SetIsRooted(True);
  Tree.SetTreeListRooted(True);
end;

procedure TTreeViewForm.GeneDupsCancelled;
begin
  FJustRootingATree := False;
  if Assigned(FGeneDupWizardForm) then
  begin
    FreeAndNil(FGeneDupWizardForm);
  end;
end;


procedure TTreeViewForm.GetOutgroupTaxa(var AList: TStringList);
begin
  Tree.GetOutgroupTaxa(AList);
end;

procedure TTreeViewForm.SetOutgroupCluster(AColor: TColor; AIndex: Integer);
begin
  if AIndex < 1 then
  begin
    Tree.FocusOnRoot;
    Tree.SetOutgroupCluster(AColor, Tree.NodeInfo.Des2Index);
  end
  else
    Tree.SetOutgroupCluster(AColor, AIndex);
end;

procedure TTreeViewForm.ColorOutgroupClusterGray;
begin
  Tree.FocusOnRoot;
  if Tree.OutgroupIsDes1 then
    SetOutgroupCluster(clSilver, Tree.NodeInfo.Des1Index)
  else
    SetOutgroupCluster(clSilver, Tree.NodeInfo.Des2Index);
  Tree.Refresh;
end;

procedure TTreeViewForm.ColorOutgroupClusterBlack;
begin
  Tree.FocusOnRoot;
  if Tree.OutgroupIsDes1 then
    SetOutgroupCluster(clBlack, Tree.NodeInfo.Des1Index)
  else
    SetOutgroupCluster(clBlack, Tree.NodeInfo.Des2Index);
  Tree.Refresh;
end;

procedure TTreeViewForm.CompressOutgroup(MarkGrey: Boolean);
var
  NodeAttrib: TNodeAttrib;
  AFont: TFont;
begin
  Tree.FocusOnNode(Tree.GetOutgroupAncestorIndex);
  NodeAttrib := TNodeAttrib.Create;
  Tree.GetSubtreeAttrib(NodeAttrib, Tree.FocusedIndex);
  NodeAttrib.ManualCompressed := True;
  if MarkGrey then
  begin
    NodeAttrib.LineColor := clSilver;
    NodeAttrib.CaptionFont.Color := clSilver;
    NodeAttrib.BracketColor := clSilver;
    NodeAttrib.Font.Color := clSilver;
    AFont := TFont.Create;
    AFont.Assign(Tree.OTU_Font);
    AFont.Color := clSilver;
    NodeAttrib.Font.Assign(AFont);
    AFont.Free;
  end;
  NodeAttrib.NodeIndex := Tree.FocusedIndex;
  Tree.SetSubtreeAttrib(NodeAttrib, Tree.FocusedIndex);
  if NodeAttrib.OverwriteDownstream then
    Tree.OverwriteAttribDownstream(Tree.FocusedIndex);

  if NodeAttrib.OverwriteMarker then
    Tree.SetTaxonMarkerOfSubtree(Tree.FocusedIndex, NodeAttrib.Marker);
  Tree.Refresh;
  DataSaved := False;
  NodeAttrib.Free;
end;

function TTreeViewForm.GetNewickString: String;
var
  Options: TNewickExportOptions;
begin
  Options.BranchLengths := Tree.isBranchLength;
  Options.BootstrapVals := Tree.isStats;
  Options.NodeLabels := True;
  Result := Tree.GetNewickTree(Options);
end;

procedure TTreeViewForm.GetTreeData(var AData: TTreeData);
begin
  Tree.GetTreeData(AData);
end;

procedure TTreeViewForm.GetTreeAdapterData(aTreeData: TTreeData);
begin
  if not Assigned(FTreeAdapter) then
    FTreeAdapter := TSimpleTreeDataAdapter.Create;
  FTreeAdapter.GetTreeData(aTreeData);
end;

function TTreeViewForm.HasInvalidCalibrations(var Calibrations: TCalibrations): Boolean;
var
  AMsg: String;
  AData: TTreeData;
begin
  MyPegMessageDlg := nil;
  Result := False;
  AMsg := EmptyStr;
  Assert(Calibrations.Count > 0);
  try
    AData := TTreeData.Create(Tree.NoOfOTUs, Tree.isBranchLength, Tree.isSE, Tree.isStats);
    Tree.GetTreeData(AData);
    if not Calibrations.ValidateAndFilterOutgroupConstraint(AMsg, AData) then
    begin
      try
        Result := (Calibrations.NumValidCalibrations > 0);
        MyPegMessageDlg := TMyPegMessageDlg.Create(Self);
        MyPegMessageDlg.Memo1.Color := clWhite;
        MyPegMessageDlg.Caption := 'Invalid Calibration(s)';
        MyPegMessageDlg.Memo1.Lines.Add(AMsg);
        MyPegMessageDlg.ShowModal;
        FCalibrationDlg.ClearFields;
      finally
        MyPegMessageDlg.Free;
        MyPegMessageDlg := nil;
      end;
    end
    else
    if not Calibrations.ValidateMinMaxTimes(AMsg, AData) then
    begin
      ShowMessage(AMsg);
      FCalibrationDlg.ClearFields;
    end
    else if not Calibrations.ValidateCalibrations(AMsg) then
    begin
      ShowMessage(AMsg);
      FCalibrationDlg.ClearFields;
    end
    else
      Result := True;
  finally
    if Assigned(AData) then
      AData.Free;
  end;
end;

procedure TTreeViewForm.UpdateInfoBox;
begin
  if ActionInfo.Checked then
  begin
    RenewTreeInfo;
    RenewBranchInfo;
  end;
end;

procedure TTreeViewForm.UpdateViewWithGeneDupes(TreeList: TTreeList);
begin

end;

procedure TTreeViewForm.SetSpeciesNames(SpNamesList: TStringList);
begin
  Tree.SetSpeciesNames(SpNamesList);
end;

procedure TTreeViewForm.PrepareToWorkWithTimeTreeWizard( AnalysisInfo: TAnalysisInfo; MakeVisible: Boolean);
begin
  try
    FRelTimeActionVisible := False;
    FCalibrationsActionEnabled := True;
    FMultiTreesFrame.Visible := False;
    if MakeVisible then
      MegaForm.AddWindowToTray(FCalibrationDlg, False);
    OriTree.AssignTreeListInternalNodeLabelFunc(FCalibrationDlg.GetInternalNodeLabelFunc);
    OriTree.SetGroupInfo(AnalysisInfo.MyGpNames);
    OriTree.UseGroupAttrib := True;
    OriTree.ShowBLen := False;
    OriTree.MakeRootByOutgroup(True);
    OriTree.FocusOnRoot;
    OriTree.SetOutgroupCluster(clSilver, OriTree.NodeInfo.Des2Index);
    if MakeVisible then
    begin
      FCalibrationDlg.TreeViewForm := Self;
      FCalibrationDlg.SetAnalysisInfo(AnalysisInfo);
      FCalibrationDlg.SetFileName(AnalysisInfo.DataFilename);
      FCalibrationDlg.DisplayMode := cdmBeforeConstructTree;
      if not KeepHidden then
        Show;
      UpdateClientSize;
      DataSaved := True;
      FRootingToolsEnabled := False;
      MoveTreeToCalibrationDlg;
    end;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when preparing the Calibration Wizard: ' + E.Message);
  end;
end;

procedure TTreeViewForm.MoveTreeToCalibrationDlg;
begin
  FCaptionWasVisible := CaptionPanel.Visible;
  CaptionHideActionExecute(self);
  FCalibrationDlg.UpdateFormSizeForTree(ClientWidth - SidePanel.Width, OriTree.Height);
  OriTree.EditNameEnabled := False;
  OriTree.Parent := FCalibrationDlg.TreePanel;
  {$IFDEF DARWIN}
  OriTree.Align := alClient;
  {$ENDIF}
  FCalibrationDlg.Tree := OriTree;
  FCalibrationDlg.Tree.ShowCalibratedNodeMarker := False;
  OriTree.IsDoingCalibrations := True;
  OriTree.Visible := True;
  {$IFNDEF DARWIN}
  FCalibrationDlg.SetBottomBars(TreeStatusBar, ProgressBar1);
  {$ENDIF}
  OriTree.OnMouseUp := FCalibrationDlg.DoMouseUp;
  if (OriTree.TreeStyle = tsCircle) or (OriTree.TreeStyle = tsRadiation) then
  begin
    OriTree.TreeStyle := tsTraditional;
    TreeStyleBtn2.ImageIndex := RECTTREESTYLE_IMAGE;
    FLayoutFrame.TreeStyleBtn.ImageIndex := RECTTREESTYLE_IMAGE;
  end;
  FCalibrationDlg.AutoSizeActionExecute(Self);
  OriTree.Refresh;
end;

procedure TTreeViewForm.RetrieveTreeFromCalibrationDlg;
begin
  OriTree.IsDoingCalibrations := False;
  OriTree.Parent := TreeTabControl;
  OriTree.OnMouseUp := TreeMouseUp;
  OriTree.EditNameEnabled := True;
  {$IFNDEF DARWIN}
  TreeStatusBar.Parent := BottomPanel;
  ProgressBar1.Parent := BottomPanel;
  ProgressBar1.BorderWidth := 0;
  {$ENDIF}
  FormResize(Self);
  Tree.Invalidate;
  FCalibrationDlg.StatusBar := nil;
  FCalibrationDlg.ProgressBar := nil;
  FCalibrationDlg.Hide;
  if FCaptionWasVisible then
    CaptionDockInTEActionExecute(Self);
end;

procedure TTreeViewForm.PrepareToRootTreeOnly(RootTreeMode: TRootTreeMode);
begin
  {$IFDEF DARWIN}
  MacOSTabsPanel.Visible := False;
  {$ENDIF}
  InitInstructionsLabel;
  case RootTreeMode of
    rtTimetree: FOkButton.OnClick := RootTreeFinishedActionExecute;
    rtGeneDups: FOkButton.OnClick := ActionLaunchGeneDupsSearchExecute;
  end;

  FCancelButton.OnClick := ActionQuitExecute;
  ShowCharStateItem.Enabled := False;
  ComputeMenu.Enabled := False;
  DrawOptionsItem.Enabled := False;
  ActionBalancedTreeExecute(nil);
  FJustRootingATree := True; { this must be set after the call to ActionBalancedTreeExecute, otherwise it will be disabled}
  Tree.IsRooted := False;
  Tree.ShowBLen := False;
  Tree.ShowTopologyOnly := True;
  Tree.PixelsPerOTU := DEFAULT_PPOTU;
  Tree.Refresh;
  ActionRootOnBranchExecute(nil);
  Tree.ScrollToBottom;
  Tree.Invalidate;
end;

procedure TTreeViewForm.LoadImagesDirect(ImagesDir: String);
var
  Filename: string;
  a : TNodeAttrib;
  i: Integer;
  FoundAnImage: boolean;
begin

  try
    try
     if DirectoryExists(ImagesDir) then
    begin
      a := TNodeAttrib.Create;
      for i := 1 to Tree.NoOfNodes do
       if Tree.OTUName[i] <> '' then
       begin
         FoundAnImage := False;
         Filename := Tree.OTUName[i];
         if FileExists(ImagesDir + '\' + Filename + '.bmp') then
           FoundAnImage := true
         else if Pos(' ', Filename) > 0 then
         begin
           repeat
             Filename[Pos(' ', Filename)] := '_';
           until Pos(' ', Filename) = 0;
           if FileExists(ImagesDir + '\' + Filename + '.bmp') then
             FoundAnImage := true;
         end;
         if FoundAnImage then
         begin
           if not Tree.GetSubtreeAttrib(a, i) then
             a.ShowBracket := false;
           a.LoadBitmapFromFile(ImagesDir + '\' + Filename + '.bmp');
           if a.IsImage then
             Tree.SetSubtreeAttrib(a, i);
           DataSaved := False;
           continue;
         end;
         if FileExists(ImagesDir + '\' + Filename + '.jpg') then
           FoundAnImage := true
         else if Pos(' ', Filename) > 0 then
         begin
           repeat
             Filename[Pos(' ', Filename)] := '_';
           until Pos(' ', Filename) = 0;
           if FileExists(ImagesDir + '\' + Filename + '.jpg') then
             FoundAnImage := true;
         end;
         if FoundAnImage then
         begin
           Tree.GetSubtreeAttrib(a, i);
           a.LoadJPEGFromFile(ImagesDir + '\' + Filename + '.jpg');
           Tree.SetSubtreeAttrib(a, i);
           DataSaved := false;
         end;
       end;
      Tree.Refresh;
    end;
    except
      // don't sweat it, the user did not request this action
    end;
  finally
    if Assigned(a) then
      a.Free;
  end;
end;

procedure TTreeViewForm.ActionExportSiteRateExecute(Sender: TObject);
begin
  Assert(False, 'not implemented');
end;

function TTreeViewForm.GetAnalysisType: String;
begin
  case MAI.InitialUsrOperation of
   dtdoNJTree: Result := 'Neighbor Joining';
   dtdoMETree: Result := 'Minimum Evolution';
   dtdoMPTree: Result := 'Maximum Parsimony';
   dtdoUPGMATree: Result := 'UPGMA';
   dtdoMLTree: Result := 'Maximum Likelihood';
   dtdoMPInferAncSeq, dtdoMPInferAncSeqMyPeg: Result := 'Infer Ancesteral Sequences with (MP)';
   dtdoOLSInteriorBranchTest: Result := 'Interior Branch Test';
   dtdoMLInferAncSeq, dtdoMLInferAncSeqMyPeg: Result := 'Infer Ancesteral Sequences (ML)';
   dtdoMLPredictLivingSeq: Result := 'Predict Living Sequences by Inferring Ancestral Sequences (ML)';
   dtdoEpML: Result := 'Evolutionary Probabilities (ML)';
   dtdoMPComputeUserTreeBLens: Result := 'Analyze User Tree by Maximum Parsimony';
   dtdoOLSComputeUserTreeBLens: Result := 'Analyze User Tree by Least Squares';
   dtdoMLComputeUserTreeBLens: Result := 'Analyze User Tree by Maximum Likelihood';
  else
    Result := EmptyStr;
  end;
end;

function TTreeViewForm.TryGetOriTreeBMP(var aBitmap: TBitmap; aWidth: Integer;
  aHeight: Integer): Boolean;
begin
  Result := False;
  Assert(False, 'not implemented');
end;

function TTreeViewForm.IsML: Boolean;
begin
  Result := Assigned(MLAnalyzer);
end;

function TTreeViewForm.isAncState: boolean;
begin
  Result := false;
  if @FShowAllAncStateProc <> nil then Result := true;
  if @FShowMostProbAncStateProc <> nil then Result := true;
  if @FHideAmbigAncStateProc <> nil then Result := true;
  if @FShowExtCharAncStateProc <> nil then Result := true;
  if not Result then Exit;

  if Tree.TreeStyle <> tsTraditional then Result := false;
  if Tree.BranchStyle <> bsRectangular then Result := false;
  if Tree.isCondensed then Result := false;
  if Tree.TreeIndex = 0 then Result := false;
end;

function TTreeViewForm.isAncProb: boolean;
begin
  if @Tree.AncProbFunc = nil then
      Result := false
  else
      Result := true;
end;

procedure TTreeViewForm.RenewForm;
begin
  If Tree = nil then
    Exit;
  InitForm;
end;

procedure TTreeViewForm.ShowAllParsimonyAncState(AncState: PArrayOfString;SiteIndex: integer; ATree: TTreeData);
var
  AStates: PArrayOfLongint;
  i: integer;
begin
  GetMem(AStates, SizeOf(Longint)*FTree.NoOfNodes);
  ComputeParsimInfo.SetTreeData(ATree);
  ComputeParsimInfo.ComputeSiteMPAncStates(SiteIndex, AStates^);
  if ComputeParsimInfo.IsNucData then
    for i := 0 to Tree.NoOfNodes-1 do
      AncState[i]^ := ParsimMapToNucStr(AStates[i])
  else
    for i := 0 to Tree.NoOfNodes-1 do
      AncState[i]^ := ParsimMapToAminoStr(AStates[i]);
  FreeMemAndNil(AStates);
end;

procedure TTreeViewForm.ShowAllParsimonyAncStateEx(AncState: PArrayOfString; SiteIndex: integer; ATree: TTreeData);
var
  AStates: PArrayOfLongint;
  i: integer;
begin
  GetMem(AStates, SizeOf(Longint)*FTree.NoOfNodes);
  ComputeParsimInfo.SetTreeData(ATree);
  ComputeParsimInfo.ComputeSiteMPAncStates(SiteIndex, AStates^);
  if ComputeParsimInfo.IsNucData then
    for i := 0 to Tree.NoOfNodes-1 do
      AncState[i]^ := ParsimMapToNuc(AnsiChar(Chr(AStates[i])))
  else
    for i := 0 to Tree.NoOfNodes-1 do
      AncState[i]^ := ParsimMapToAmino(AStates[i]);
  FreeMemAndNil(AStates);
end;

procedure TTreeViewForm.HideAmbigParsimonyAncState(AncState: PArrayOfString; SiteIndex: integer; ATree: TTreeData);
var
  AStates: PArrayOfLongint;
  i: integer;
begin
  GetMem(AStates, SizeOf(Longint)*FTree.NoOfNodes);
  ComputeParsimInfo.SetTreeData(ATree);
  ComputeParsimInfo.ComputeSiteMPAncStates(SiteIndex, AStates^);
  if ComputeParsimInfo.IsNucData then
    for i := 0 to Tree.NoOfNodes-1 do begin
      AncState[i]^ := ParsimMapToNuc(AnsiChar(Chr(AStates[i])));
      if not IsUnambiguousNucleotide(AncState[i]^[1]) then
        AncState[i]^ := '';
    end
  else
    for i := 0 to Tree.NoOfNodes-1 do begin
      AncState[i]^ := ParsimMapToAmino(AStates[i]);
      if not IsUnambiguousAminoAcid(AncState[i]^[1]) then
        AncState[i]^ := '';
    end;
  FreeMemAndNil(AStates);
end;

procedure TTreeViewForm.ShowAllMLAncState(AncState: PArrayOfString; SiteIndex: integer; ATree: TTreeData);
var
  i,j: integer;
  gap: boolean;
  states : TAncStateRecArray;
begin
  if MAI.isAminoAcid then
    SetLength(states, 20)
  else
    SetLength(states, 4);
  for i := 0 to Tree.NoOfNodes-1 do
  begin
    AncState[i]^ := '';
    gap := not MLAnalyzer.GetAncStateProb(i, SiteIndex+1, states);
    if gap then
      AncState[i]^ := '-'
    else
    begin
      for j := Low(states) to High(states) do
      begin
        if CompareValue(states[j].Prob, 0.05, 0.001) >= 0 then
          AncState[i]^ := AncState[i]^ + states[j].Name;
      end;
    end;
  end;
end;

procedure TTreeViewForm.ShowMLAncState(AncState: PArrayOfString; SiteIndex: integer; ATree: TTreeData);
var
  AStrList: TStringList;
  AStates: PArrayOfLongint;
  i: integer;
  gap: boolean;
begin
  GetMem(AStates, SizeOf(Longint)*FTree.NoOfNodes);

  AStrList := TStringList.Create;
  for i := 0 to Tree.NoOfNodes-1 do
  begin
    gap := not MLAnalyzer.GetAncStateProb(i, SiteIndex+1, AStrList);
    if gap then
      AncState[i]^ := '-'
    else
      AncState[i]^ := AStrList.Names[0];
  end;

  AStrList.Free;
end;

procedure TTreeViewForm.HideAmbigMLAncState(AncState: PArrayOfString; SiteIndex: integer; ATree: TTreeData);
var
  AStrList: TStringList;
  AStates: PArrayOfLongint;
  i: integer;
  gap: boolean;
begin
  GetMem(AStates, SizeOf(Longint)*FTree.NoOfNodes);

  AStrList := TStringList.Create;
  for i := 0 to Tree.NoOfNodes-1 do
  begin
    gap := not MLAnalyzer.GetAncStateProb(i, SiteIndex+1, AStrList);
    if gap then
      AncState[i]^ := '-'
    else if StrToFloat(AStrList.Values[AStrList.Names[0]]) >= 0.95 then
      AncState[i]^ := AStrList.Names[0]
    else
      AncState[i]^ := '';
  end;

  AStrList.Free;
end;

procedure TTreeViewForm.SetOriTrees(newtree: TTreeList);
begin
  SetComputeParsimInfo;
  D := newtree.DistanceMatrix;
  OriTree.SetTreeList(newtree, not PreserveOriginalTreeRoot);
  if Assigned(FOtuInfos) and (not MAI.TransposeData) then
    OriTree.SetOtuInfo(FOtuInfos);
  Tree := OriTree;
  if Tree.Title <> EmptyStr then
  begin
      if Pos(VER_MEGA_WIN_CAPTION_PREFIX+ ':', Tree.Title) = 0 then
         Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': '+Tree.Title
      else
        Caption := Tree.Title;
  end
  else
      Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Tree Explorer';
  DataOpened := true;
  DataSaved := false;
end;

procedure TTreeViewForm.SetOriPartitions(newtree: TPartitionList);
begin
  SetComputeParsimInfo;
  D := newtree.DistanceMatrix;
  OriTree.SetPartitionList(newtree);
  Tree := OriTree;
  if Tree.Title <> '' then
  begin
    if Pos(VER_MEGA_WIN_CAPTION_PREFIX+ ':', Tree.Title) = 0 then
       Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': ' + Tree.Title
    else
      Caption := Tree.Title;
  end
  else
    Caption := 'Tree Explorer';
  DataOpened := true;
  DataSaved := false;
end;

procedure TTreeViewForm.SetBootTrees(newtree: TTreeList);
begin
  SetComputeParsimInfo;
  BootTree.SetTreeList(newtree, true);
  if Assigned(FOtuInfos) then
    BootTree.SetOtuInfo(FOtuInfos);
  if Tree = nil then
    Tree := BootTree;
  Tree.Refresh;
  IsBootTree := true;
  DataOpened := true;
  DataSaved := false;
end;

procedure TTreeViewForm.SetBootPartitions(newtree: TPartitionList);
begin
  SetComputeParsimInfo;
  BootTree.SetPartitionList(newtree);
  if Tree = nil then
    Tree := BootTree;
  Tree.Refresh;
  IsBootTree := true;
  DataOpened := true;
  DataSaved := false;
end;

procedure TTreeViewForm.SetRelTimeTrees(newtree: TTreeList);
begin
  D := newtree.DistanceMatrix;
  RelTimeTree.SetTreeList(newtree, true);
  if Assigned(FOtuInfos) then
    RelTimeTree.SetOtuInfo(FOtuInfos);
  ReltimeTree.ShowRoot := False;
  ReltimeTree.IsRooted := True;
  ReltimeTree.ForceLinearized := True;
  RelTimeTree.ShowHeightErrBar := True;
  Tree := RelTimeTree;
  if Tree.Title <> EmptyStr then
  begin
    if Pos(VER_MEGA_WIN_CAPTION_PREFIX+ ':', Tree.Title) = 0 then
       Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': '+Tree.Title
    else
      Caption := Tree.Title;
  end
  else
    Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Tree Explorer';
  DataOpened := true;
  DataSaved := false;
end;

procedure TTreeViewForm.SetGeneDuplications(AGeneTree: TSimpleTreeNodeArray);
var
  i: Integer;
  NDups: Integer;
  NSpeciations: Integer;
begin
  NDups := 0;
  NSpeciations := 0;
  for i := Low(AGeneTree) to High(AGeneTree) do
  begin
    if AGeneTree[i].IsDuplicationEvent then
      inc(NDups)
    else if AGeneTree[i].IsSpeciationEvent then
      inc(NSpeciations);
  end;
  FigureGenerator.AssignData('GeneDupsCitation', 'Zmasek_and_Eddy_2001');
  SetNumGeneDups(NDups);
  SetNumSpeciations(NSpeciations);
  FHasSpeciesNames := True;
  FJustRootingATree := False;
  miCaptionExpert.Enabled := True;
  OriTree.IsGeneDups := True;
  OriTree.SetGeneDuplications(AGeneTree);
  OriTree.ShowGeneDupMarkers := True;
  if NDups > 0 then
    OriTree.HasGeneDups := True;
  if NSpeciations > 0 then
  begin
    OriTree.HasSpeciations := True;
    OriTree.ShowSpeciationMarkers := True;
  end;
  Self.Show;
  TreeTabControl.TabIndex := TreeTabControl.Tabs.IndexOf(ORI_TREE_TAB);
  Tree := OriTree;
  OriTree.ShowRoot := False;
  Tree.Refresh;
end;

procedure TTreeViewForm.SetGroupInfo(GpName: TStringList);
var
  i: integer;
begin
  OriTree.SetGroupInfo(GpName);
  if IsRelTimeTree then
    RelTimeTree.SetGroupInfo(GpName);
  if IsBootTree then
    BootTree.SetGroupInfo(GpName);

  if OriTree.GroupAttrib.Count > 0 then
    for i := 0 to OriTree.GroupAttrib.Count-1 do
    begin
      LoadGroupOption(OriTree.GroupAttrib[i]);
      if OriTree.GroupAttrib[i].OverwriteMarker then
        OriTree.SetTaxonMarkerOfGroup(OriTree.GroupAttrib[i].Name, OriTree.GroupAttrib[i].Marker);

      if IsBootTree then
      begin
        BootTree.GroupAttrib[i].Assign(OriTree.GroupAttrib[i]);
        if BootTree.GroupAttrib[i].OverwriteMarker then
          BootTree.SetTaxonMarkerOfGroup(BootTree.GroupAttrib[i].Name, BootTree.GroupAttrib[i].Marker);
      end;

      if IsRelTimeTree then
      begin
        ReltimeTree.GroupAttrib[i].Assign(OriTree.GroupAttrib[i]);
        if ReltimeTree.GroupAttrib[i].OverwriteMarker then
          ReltimeTree.SetTaxonMarkerOfGroup(ReltimeTree.GroupAttrib[i].Name, ReltimeTree.GroupAttrib[i].Marker);
      end;
    end;
end;

procedure TTreeViewForm.SetGroupInfoAndRefresh(GpName: TStringList);
begin
  SetGroupInfo(GpName);
  Tree.Refresh;
  if Tree.IsOutgroup then
    ActionRootByOutgroupExecute(Self);
end;

procedure TTreeViewForm.SetData(data: TAnalysisInfo);
var
  i: integer;
  aList: TTreeList = nil;
begin
  OriTree.IsGeneDups := False;
  ReltimeTree.IsGeneDups := False;
  BootTree.IsGeneDups := False;
  if Assigned(data) and Assigned(data.MyUsedOtuInfos) then
    SetOtuInfos(data.MyUsedOtuInfos);
  Tree.Visible := True;
  ReltimeActionDisabled := (not MegaForm.HasSequenceData);
  MAI := data;
  DecideRootOnMidpoint;
  InitCaptionViewer;
  if MAI.IsGeneDupsTree then
    FHasSpeciesNames := True
  else
    FHasSpeciesNames := MAI.MyOriTreeList.HasSpeciesNames;
  AddWindowToTray(Self);
  if not IsRelTimeAnalysis(MAI.MyUsrOperation) then
  begin
    OriTree.ShowRoot := MAI.IsAncestralSeqsOperation;
    FCalibrations := TCalibrations.Create;
    FCalibrations.SetInfo(MAI);
    if (MAI.MyUsrOperation = dtdoGeneDupInference) or MAI.IsGeneDupsTree then
    begin
      if (not FJustRootingATree) and MAI.IsGeneDupsTree then
      begin
        ActionDisplaySpeciesNames.Visible := True;
        DisplayTaxaNamesAction.Visible := True;
        MegaForm.SetTheWizardFree;
        Tree.IsGeneDups := True;
        Tree.ShowGeneDupMarkers := True;
      end
      else
        Tree.IsGeneDups := False;
    end;
    if MAI.MyTreePack.DoesContain(ttUpgma) then
      OriTree.IsUPGMA := True;
    SetOriTrees(MAI.MyOriTreeList);
    SetGroupInfo(MAI.MyGpNames);
  end
  else
  begin
    Include(FTimeTreeTypes, tttReltimeMain);
    FRelTimeActionDisabled := True;
    RelTimeBtn.Action := ActionRecalibrateTimetree;
    RelTimeMenuItem.Action := ActionRecalibrateTimetree;
    MAI.MyOriTreeList.IsRooted := True;
    ReltimeTree.ForceLinearized := True;
    ReltimeTree.ShowRoot := False;
    OriTree.ShowRoot := False;
  end;

  if MAI.MyTreePack.DoesContain(ttUpgma) then
  begin
    OriTree.ForceLinearized := true;
    OriTree.ShowRoot := not MAI.MyTreePack.DoesContain(ttBootstrap);
    RelTimeActionVisible := False;
  end
  else if MAI.MyTreePack.DoesContain(ttMP) then
  begin
    OriTree.ShowTopologyOnly := true;
    OriTree.ValueDecimals := 0;
    OriTree.MaxSiteIndex := MAI.MyNoOfSites-1;
    InfoDisplay.TreeLength := Round(MAI.MyOriTreeList[0].Value);
    InitRelTimeTreeViewStates; { in case there are multiple parsimony trees}
  end;

  if MAI.MyTreePack.DoesContain(ttBootstrap) then
  begin
    if MAI.MyTreePack.DoesContain(ttUPGMA) then
      BootTree.BLenFunc := nil
    else if MAI.MyTreePack.DoesContain(ttMP) then
      BootTree.BLenFunc := OriTree.BLenFunc
    else if MAI.MyTreePack.DoesContain(ttML) then
      BootTree.BLenFunc := OriTree.BLenFunc
    else
      BootTree.BLenFunc := SetOLSBranchLength;

    SetBootPartitions(MAI.MyBootPartitionList);
    MAI.MyBootPartitionList := nil;
    if MAI.MyTreePack.DoesContain(ttUpgma) then
    begin
      for i := 1 to BootTree.NoOfOTUs do
        BootTree.Outgroup[i] := OriTree.CurrentOutgroup[i];
      BootTree.MakeRootByOutgroup;
      for i := 1 to BootTree.NoOfOTUs do
        BootTree.Outgroup[i] := false;
      BootTree.ForceLinearized := true;
    end;

    OriTree.ShowStats := true;
    BootTree.ShowStats := true;
    if MAI.MyTreePack.DoesContain(ttMP) then
      BootTree.ShowTopologyOnly := true;
    if MAI.MyTreePack.DoesContain(ttML) then
      BootTree.ValueDecimals := 2;
    IsBootTree := true;
  end;

  if MAI.MyTreePack.DoesContain(ttCPTest) then
    OriTree.ShowStats := true;

  if MAI.MyTreePack.DoesContain(ttML) then
  begin
    MLAnalyzer := MAI.MyMLAnalysisPack;
    SetMLAnalyzer;
  end;
  //else if not IsReltimeAnalysis(MAI.MyUsrOperation) then
  //  OriTree.LinearizeFunc := RelTimeLinearizeFunc;
  if IsReltimeAnalysis(MAI.MyUsrOperation)then
  begin
    if Assigned(MLAnalyzer) then
    begin
      FReltimeAnalyzer.Assign(MLAnalyzer.MLTree.ReltimeComputer);
      FRelTimeAnalyzer.AcceptControlOfSpreadsheetExport(MLAnalyzer.MLTree.ReltimeComputer.ReleaseControlOfSpreadsheetExport);
    end
    else
    begin
      FReltimeAnalyzer.Assign(MAI.MyRelTimeComputer);
      FRelTimeAnalyzer.AcceptControlOfSpreadsheetExport(MAI.MyReltimeComputer.ReleaseControlOfSpreadsheetExport);
    end;
    PrepareForReltimeDisplay; // sets up oritree with the users original tree and reltimetree with the clock tree
    Include(FTimeTreeTypes, tttReltimeMain);
  end;
  if (MAI.MyUsrOperation <> dtdoGeneDupInference) and (not IsReltimeBLensAnalysis(MAI.MyUsrOperation)) then
  begin
    { Because TTreeList is about to become invisible, we get references to a couple
      of methods that we need from it. This is safer than getting a reference to
      TTreeList itself since we won't be able to modify anything}
    InitCalibrationDlg;
  end;

  if (MAI.ClockType = ctLocal) or IsReltimeAnalysis(MAI.InitialUsrOperation) then // then our tree is already a RelTime clock tree
  begin
    OriTree.SessionIsReltime := True;
    if Assigned(MAI.CalibrationTimes) then
      FCalibrationDlg.SetCalibrations(MAI.CalibrationTimes);
    if Assigned(MAI.CalibrationTimes) and (MAI.CalibrationTimes.Count > 0) then
    begin
      Tree.UpdateCalibratedNodes(MAI.CalibrationTimes);
      Tree.TimeFactor := FReltimeAnalyzer.TimeFactor;
      if MAI.CalibrationTimes.IsSampleTimes then
        ReltimeTree.LatestSampleTime := MAI.CalibrationTimes.GetLatestSamplingTime;
    end;
    FRelTimeActionVisible := False;
    Tree.IsTimes := True;
  end
  else
  begin
    if not MAI.MyTreePack.DoesContain(ttUPGMA) then
    begin
      FRelTimeActionVisible := True;
      Tree.IsTimes := False;
    end;
    FRelTimeAnalyzer := TRelTimeComputer.Create;
  end;
  aList := TTreeList.Create;
  aList.Assign(MAI.MyOriTreeList);
  MAI.MyOriTreeList := aList;
  //MAI.MyOriTreeList := nil; // relinquish control
  if not KeepHidden then
  begin
    Caption := ConvertToMegaWinCaption('TreeExplorer (' + MAI.DataFileName+')');
    //{$IFDEF DARWIN}BottomPanel.Top := TreeTabControl.Height + CaptionPanel.Height;{$ENDIF}
    Show;
    UpdateClientSize;
    if IsReltimeAnalysis(MAI.MyUsrOperation) then
    begin
      if OriTree.MinHeight < Tree.Height then
        ResizeTreeToFitWindow(OriTree, False);
    end;
  end;
  LoadTemplate(MAI, FigureGenerator);
  RefreshCaption;
  CaptionHideActionExecute(nil);
  AdjustTreeForBestFitToWindow(True);
  if (not KeepHidden) and IsBootTree then
  begin
    BootTree.PixelsPerOTU := Tree.PixelsPerOTU;
    BootTree.TreeWidth := Tree.TreeWidth;
  end;
  Invalidate;
  Tree.Repaint;
end;


procedure TTreeViewForm.ShowOriTree;
var
  index: Integer;
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  index := TreeTabControl.Tabs.IndexOf(ORI_TREE_TAB);
  if index >= 0 then
  begin
    TreeTabControl.TabIndex := index;
    TreeTabControlChange(nil);
  end;
  Show;
end;

procedure TTreeViewForm.ShowBootTree;
var
  index: Integer;
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  index := TreeTabControl.Tabs.IndexOf(BOOT_CONS_TREE_TAB);
  if index < 0 then
    index := TreeTabControl.Tabs.IndexOf(BOOT_TREE_TAB);
  if index >= 0 then
  begin
    TreeTabControl.TabIndex := index;
    TreeTabControlChange(nil);
  end;
  Show;
end;

procedure TTreeViewForm.ShowReltimeTree;
var
  index: Integer;
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  index := TreeTabControl.Tabs.IndexOf(TIME_TREE_TAB);
  if index >= 0 then
  begin
    TreeTabControl.TabIndex := index;
    TreeTabControlChange(nil);
  end;
  Show;
end;

function TTreeViewForm.AskForFileName: Boolean;
//var
//  mr: word;
begin
  Result := true;
  FileSaveDialog.InitialDir := ChangeInitialDirectorySaveDialogForMac(FileSaveDialog.InitialDir);
  if not FileSaveDialog.Execute then
  begin
    Result := false;
    exit;
  end;

  //while FileExists(FileSaveDialog.FileName) do
  //begin
  //  mr := MessageDlg('The file exists. Overwrite?', mtWarning, [mbYes, mbNo, mbCancel], 0);
  //  if  mr = mrYes then
  //    Exit
  //  else if mr = mrNo then
  //  begin
  //    FileSaveDialog.InitialDir := ChangeInitialDirectorySaveDialogForMac(FileSaveDialog.InitialDir);
  //    if not FileSaveDialog.Execute then
  //    begin
  //      Result := false;
  //      Exit;
  //    end
  //  end
  //  else
  //  begin
  //    Result := false;
  //    Exit;
  //  end
  //end;
end;

function TTreeViewForm.SaveSession(const filename: String; var aMsg: String): boolean;
var
  data : File;
  i,f,n : longint;
  m: TNodeMarker;
begin
  AssignFile(data, filename);
  ReWrite(data, 1);
  try
    try
      FProgress.SetToMarqueeMode;
      FProgress.StartProgress('Saving tree session...', EmptyStr);
      FProgress.CancelButton.Visible := False;
      FProgress.FormStyle := fsStayOnTop;
      FProgress.Show;
      i := $53544D23;
      BlockWrite(data, i, 4);
      i := MTS_SESSION_VERSION;
      BlockWrite(data, i, 4);  // Write version #
      i := TARGET_PLATFORM;
      BlockWrite(data, i, SizeOf(i));
      f := 0;
      if (BootTree.NoOfTrees > 0) or (BootTree.isConsensus) then
        f := f or 1;
      if D <> nil then
        f := f or 2;
      if OriTree.isOutgroup then
        f := f or 4;
      BlockWrite(data, f, 4);

      OriTree.WriteToFile(data);
      if (f and 1 = 1) then
        BootTree.WriteToFile(data);

      if (f and 2 = 2) then
        for i := 1 to OriTree.NoOfOTUs-1 do
          BlockWrite(data, D[i]^, i*8);

      if (f and 4 = 4) then
      begin
        n := 0;
        for i := 1 to OriTree.NoOfOTUs do
          if OriTree.Outgroup[i] then
            Inc(n);
        BlockWrite(data, n, 4);
        for i := 1 to OriTree.NoOfOTUs do
          if OriTree.Outgroup[i] then
            BlockWrite(data, i, 4);
      end;

      for i := 1 to OriTree.NoOfOTUs do
      begin
        m := OriTree.Marker[i];
        BlockWrite(data, m.shape, SizeOf(TNodeMarkerShape));
        BlockWrite(data, m.color, SizeOf(TColor));
      end;

      if Tree = OriTree then
        i := 0
      else if Tree = BootTree then
        i := 1
      else if Tree = ReltimeTree then
        i := 2;

      BlockWrite(data, i, 4);

      if MTS_SESSION_VERSION >= 602 then
      begin
        if IsReltimeTree or (tttReltimeLocal in FTimeTreeTypes) then
          i := 1
        else
          i := 0;
        BlockWrite(data, i, SizeOf(Integer));
        if i = 1 then
          ReltimeTree.WriteToFile(data);
      end;

      BlockWrite(data, CurOriTreeIndex, 4);
      BlockWrite(data, CurBootTreeIndex, 4);

      i := 0;
      if ActionRootOnMidpoint.Checked then
        i := i or 1;
      if ActionBalancedTree.Checked then
        i := i or 2
      else if ActionInputOrderTree.Checked then
        i := i or 4;
      BlockWrite(data, i, 4);

  {}  BlockWrite(data, i, 4);    // for future use
  {}  BlockWrite(data, i, 4);    // for future use
  {}  BlockWrite(data, i, 4);    // for future use

  // for parsimony tree
      if ComputeParsimInfo <> nil then begin
        i := 1;
        BlockWrite(data, i, 4);
        i := ComputeParsimInfo.NoOfSites;
        BlockWrite(data, i, 4);
        if ComputeParsimInfo.IsNucData then
          i := 4
        else
          i := 20;
        BlockWrite(data, i, 4);
        for i := 0 to Tree.NoOfOTUs-1 do
          if ComputeParsimInfo.IsNucData then begin
            BlockWrite(data, ComputeParsimInfo.Seqs[i]^, ComputeParsimInfo.NoOfSites);
          end
          else begin
            BlockWrite(data, ComputeParsimInfo.Seqs[i]^, ComputeParsimInfo.NoOfSites*4);
          end;
      end
      else begin
        if @BootTree.BLenFunc <> nil then
          i := 2
        else
          i := 0;
        BlockWrite(data, i, 4);
      end;

      if MAI <> nil then
      begin
        i := 1;
        BlockWrite(data, i, 4);
        MAI.WriteToFile(data, MTS_SESSION_VERSION);
        if MAI.MyTreePack <> nil then
        begin
          i := 1;
          BlockWrite(data, i, 4);
          MAI.MyTreePack.WriteToFile(data);
        end
        else
        begin
          i := 0;
          BlockWrite(data, i, 4);
        end;
        if MAI.MyDistPack <> nil then
        begin
          i := 1;
          BlockWrite(data, i, 4);
          MAI.MyDistPack.WriteToFile(data);
        end
        else
        begin
          i := 0;
          BlockWrite(data, i, 4);
        end;
      end
      else
      begin
        i := 0;
        BlockWrite(data, i, 4);
      end;

      if MTS_SESSION_VERSION >= 501 then
      begin
        if MAI <> nil then // necessary or we get an access violation, the same is done on reading it in.
        begin
          BlockWrite(data, MAI.MyTreePack.TreeFile, SizeOf(MAI.MyTreePack.TreeFile));
          if MAI.MyDistPack <> nil then    // second part of earlier fix to take into account the mydistpack is nil and you shouldn't wrigte out somethign which is nil.
            BlockWrite(data, MAI.MyDistPack.GammaParameter, SizeOf(MAI.MyDistPack.GammaParameter));
        end;
      end;

      if MTS_SESSION_VERSION >= 600 then
      begin
        if assigned(MLAnalyzer) then
          i := 1
        else
          i := 0;
        BlockWrite(data, i, 4);

        if i = 1 then
          MLAnalyzer.WriteToFile(data);
      end;

      FReltimeAnalyzer.WriteToFile(data);
      Result := true;
      DataSaved := true;
      SessionFileName := filename;
    except
      on E:Exception do
      begin
        aMsg := E.Message;
        Result := False;
      end;
    end;
  finally
    CloseFile(data);
    if FProgress.Visible then
      FProgress.Hide;
  end;
end;

function TTreeViewForm.RetrieveSession(filename: String): boolean;
var
  data: File;
  i: Integer = -1;
  j: Integer = -1;
  f: Integer = -1;
  n: Integer = -1;
  version: longint = -1;
  m: TNodeMarker;
  seqs: TList = nil;
  buffer: pointer = nil;
  BufferString: AnsiString = '';
  BufferDouble: Double = 0;
  VersionString: String = '';
  msg: String = '';
  aNames: TStringList = nil;
  aLabels: TStringList = nil;
  aData: TTreeData = nil;
begin
  if not FileExists(filename) then
  begin
    ShowMessage('The specified tree session file does not exist: ' + filename);
    Exit;
  end;
  Result := false;
  InitCaptionViewer;
  FIsTreeSession := True;
  try
    AssignFile(data, filename);
  except
    on E:Exception do
    begin
      if IsSessionTest then
        raise Exception.Create(E.Message)
      else
      begin
        ShowMessage('Error when opening the session file: ' + E.Message);
        exit;
      end;
    end;
  end;

  if DataOpened then
  begin
    DataOpened := false;
    IsBootTree := false;
    FreeDistMatrix(D, OriTree.NoOfOTUs);
    D := nil;
    OriTree.Clear;
    BootTree.Clear;
  end;

  try
    try
      FIsLoadingSession := True;
      FProgress.SetToMarqueeMode;
      FProgress.StartProgress('Retrieving tree session...', EmptyStr);
      FProgress.Show;

      Reset(data, 1);
      BlockRead(data, i, 4);
      if i <> $53544D23 then  // checking for #MTS at start of file at the start of the file to make sure it is an MTS
        Exit;

      BlockRead(data, version, 4);  // Read version #
      if version <= LAST_BAD_MTS_SESSION_VERSION then
      begin
        FProgress.Hide;
        VersionString := MapSessionVersionToMegaVersion(sftMts, version);
        ShowMessage('This session file was created with ' + VersionString + ' and cannot be opened by this version of MEGA. ' +
                    'Please use ' + VersionString + ' to open this session file. You can then export the tree to a .nwk file which can be opened in this version of MEGA. ' + VersionString +
                    ' can be downloaded from ' + WEBSITE_URL);
        Abort;
      end;
      if version >= 1001 then
      begin
        BlockRead(data, i, SizeOf(i));
        if not SessionIsSameTargetPlatform(i, msg) then
          raise Exception.Create(msg);
      end;

      BlockRead(data, f, 4);

      OriTree.ReadFromFile(data, version);

      if (f and 1 = 1) then
      begin
        BootTree.ReadFromFile(data, version);
        IsBootTree := true;
      end
      else
        IsBootTree := false;

      Application.ProcessMessages;

      OriTree.AssignTreeListInternalNodeLabelFunc(FCalibrationDlg.GetInternalNodeLabelFunc);
      FCalibrationDlg.TreeViewForm := Self;
      FCalibrationDlg.SetFileName(Filename);
      FCalibrationDlg.FinalizeCalibrationFunc := Tree.FinalizeCalibration;

      if OriTree.SessionIsReltime then
      begin
        RelTimeActionVisible := False; // because it is already a clock tree
        FIsRelTimeTreeSession := True;
        Include(FTimeTreeTypes, tttReltimeLocal);
      end
      else
      begin
        FIsRelTimeTreeSession := False;
        RelTimeActionVisible := True;
      end;

      if version = 100 then begin                      // To convert from version 100
        if OriTree.PixelsPerUnit = 0.0 then
          OriTree.TreeWidth := OriTree.TreeWidth div 4;
        if IsBootTree then begin
          if BootTree.PixelsPerUnit = 0.0 then
            BootTree.TreeWidth := BootTree.TreeWidth div 4;
        end;
      end;

      if (f and 2 = 2) then begin
        D := NewDistMatrix(OriTree.NoOfOTUs, true);
        for i := 1 to OriTree.NoOfOTUs-1 do begin
          BlockRead(data, D[i]^, i*8);
          for j := 0 to i-1 do
            D[j][i] := D[i][j];
        end;
        OriTree.DistanceMatrix := D;
        if IsBootTree then
          BootTree.DistanceMatrix := D;
      end;

      Application.ProcessMessages;

      if (f and 4 = 4) then
      begin
        BlockRead(data, n, 4);
        for i := 1 to n do begin
          BlockRead(data, j, 4);
          OriTree.Outgroup[j] := true;
        end;
        if IsBootTree then
          for i := 1 to OriTree.NoOfOTUs do
            BootTree.Outgroup[i] := OriTree.Outgroup[i];
      end;

      for i := 1 to OriTree.NoOfOTUs do
      begin
        m := OriTree.Marker[i];
        BlockRead(data, m.shape, SizeOf(TNodeMarkerShape));
        if version >= 300 then
          BlockRead(data, m.color, SizeOf(TColor))
        else
          m.Color := clBlack;
        OriTree.Marker[i] := m;
        if IsBootTree then
          BootTree.Marker[i] := m;
      end;

      BlockRead(data, i, 4);
      if i = 0 then
        Tree := OriTree
      else if i = 1 then
        Tree := BootTree
      else if i = 2 then
        Tree := ReltimeTree;

      if version >= 602 then
      begin
        BlockRead(data, i, SizeOf(Integer));
        if i = 1 then
        begin
          ReltimeTree.ReadFromFile(data, version);
          if OriTree.IsOutgroup then
            for i := 1 to OriTree.NoOfOTUs do
              ReltimeTree.Outgroup[i] := OriTree.Outgroup[i];
          for i := 1 to OriTree.NoOfOtus do
            ReltimeTree.Marker[i] := OriTree.Marker[i];
        end;
      end;

      BlockRead(data, CurOriTreeIndex, 4);
      BlockRead(data, CurBootTreeIndex, 4);

      BlockRead(data, i, 4);
      ActionRootOnMidpoint.Checked := i and 1 = 1;
      ActionBalancedTree.Checked   := i and 2 = 2;
      ActionInputOrderTree.Checked := i and 4 = 4;

      BlockRead(data, i, 4);    // for future use
      BlockRead(data, i, 4);    // for future use
      BlockRead(data, i, 4);    // for future use

      // for parsimony trees
      BlockRead(data, i, 4);
      if i = 1 then
      begin
        InitRelTimeTreeViewStates; { in case there are multiple trees}
        ComputeParsimInfo := TComputeParsimInfo.Create;
        ComputeParsimInfo.BranchLengthsAreComputed := OriTree.isBranchLength;
        seqs := TList.Create;
        BlockRead(data, i, 4);
        ComputeParsimInfo.NoOfSites := i;
        OriTree.MaxSiteIndex := i-1;

        BlockRead(data, i, 4);
        if i = 4 then begin
          ComputeParsimInfo.SetNucData(true);
          for i := 0 to Tree.NoOfOTUs-1 do begin
            GetMem(buffer, ComputeParsimInfo.NoOfSites);
            BlockRead(data, buffer^, ComputeParsimInfo.NoOfSites);
            seqs.Add(buffer);
          end;
          ComputeParsimInfo.SetSeqs(seqs);
        end
        else begin
          ComputeParsimInfo.SetNucData(false);
          for i := 0 to Tree.NoOfOTUs-1 do begin
            GetMem(buffer, ComputeParsimInfo.NoOfSites*4);
            BlockRead(data, buffer^, ComputeParsimInfo.NoOfSites*4);
            seqs.Add(buffer);
          end;
          ComputeParsimInfo.SetSeqs(seqs);
        end;
        SetComputeParsimInfo;
      end
      else if i = 2 then
        BootTree.BLenFunc := SetOLSBranchLength;

      Application.ProcessMessages;

      if version >= 500 then
      begin
        BlockRead(data, i, 4);
        if (i = 0) and (MAI <> nil) then
        begin
          if MAI.ARP <> nil then
            MAI.ARP.Free;
          if MAI.MyTreePack <> nil then
            MAI.MyTreePack.Free;
          if MAI.MyDistPack <> nil then
            MAI.MyDistPack.Free;
          MAI.MyTreePack := nil;
          MAI.MyDistPack := nil;
          MAI.MyProcessPack := nil;
          MAI.Free;
        end;

        Application.ProcessMessages;

        if i > 0 then
        begin
          MAI := TAnalysisInfo.Create;
          MAI.DataFilename := Filename;
          MAI.ReadFromFile(data, version);
          FHasSpeciesNames := Tree.HasSpeciesNames;

          if FIsRelTimeTreeSession then
            RelTimeActionVisible := False;

          Application.ProcessMessages;

          BlockRead(data, i, 4);
          if i > 0 then
          begin
            MAI.MyTreePack := TTreePack.Create;
            MAI.MyTreePack.ReadFromFile(data);
          end;

          Application.ProcessMessages;

          BlockRead(data, i, 4);
          if i > 0 then
          begin
            MAI.MyDistPack := TDistPack.Create;
            MAI.MyDistPack.ReadFromFile(data);
          end;

          Application.ProcessMessages;
        end;
        if Assigned(MAI) then
          LoadTemplate(MAI, FigureGenerator);
        CaptionHideActionExecute(Self);
      end;

      if version >= 501 then				 // fixed by KT on 09/10/05
      begin
        if MAI <> nil then // This is absolutely necessary since it is possible to reach this point where MAI = nil.  Try opening a newick file, saving a session, then re-opening.
        begin
          SetLength(BufferString, SizeOf(Mai.MyTreePack.TreeFile));
          BlockRead(data, BufferString[1], SizeOf(Mai.MyTreePack.TreeFile));
          MAI.MyTreePack.TreeFile := BufferString;
          if MAI.MyDistPack <> nil then  // Fix: not for not being able to open MTS tree sessions in M5 which were created with distance data only.
          begin
            BlockRead(data, BufferDouble, SizeOf(MAI.MyDistPack.GammaParameter));
            MAI.MyDistPack.GammaParameter := BufferDouble;
          end;
        end;
      end;

      if version >= 600 then				 // fixed by KT on 09/10/05
      begin
        BlockRead(data, i, 4);
        if i = 1 then
        begin
          if not assigned(MLAnalyzer) then
            MLAnalyzer := TMLTreeAnalyzer.Create(nil,nil,nil);
          MLAnalyzer.ReadFromFile(data, version, MAI.MyOutgroupMembersArray);
          OriTree.Value[OriTree.TreeIndex] := MLAnalyzer.LogLikelihood;
          SetMLAnalyzer;
          if FIsRelTimeTreeSession then
          begin
            aNames := ReltimeTree.GetOTUNamesList;
            aLabels := TStringList.Create;
            ReltimeTree.GetInternalNodeLabels(aLabels);
            MLAnalyzer.MLTree.ReltimeComputer.InitSpreadsheetExportFromSessionFile(aNames, aLabels);
            OriTree.LinearizeFunc := nil; // otherwise the ML linearize func will be used
          end;
        end;
      end;
      if version >= 702 then
        FReltimeAnalyzer.ReadFromFile(data, version);
      if FIsRelTimeTreeSession then
      begin
        aData := TTreeData.Create(ReltimeTree.NoOfOTUs, True, True, False);
        ReltimeTree.GetTreeData(aData);
        FRelTimeAnalyzer.InitParentChildRelationships(aData);
        FreeAndNil(aData);
        if not Assigned(aNames) then
          aNames := ReltimeTree.GetOTUNamesList;
        if not Assigned(aLabels) then
        begin
          aLabels := TStringList.Create;
          ReltimeTree.GetInternalNodeLabels(aLabels);
        end;
        FRelTimeAnalyzer.InitSpreadsheetExportFromSessionFile(aNames, aLabels);
        FreeAndNil(aNames);
        FreeAndNil(aLabels);
        ReltimeTree.HeightErrBarProc := RelTimeHeightErrBarProc;
        ReltimeTree.MaxTimeFunc := RelTimeMaxHeightFunc;
        ReltimeTree.MinTimeFunc := RelTimeMinHeightFunc;
        ReltimeTree.DivTimeFunc := RelTimeHeightFunc;
        ReltimeTree.IsSamplingTimes := FReltimeAnalyzer.IsSamplingTime;
      end;
      Application.ProcessMessages;

      InitForm;
      Application.ProcessMessages;
      Tree.Visible := True;
      Tree.Refresh;
      Application.ProcessMessages;
      DataOpened := true;
      DataSaved := true;

      if Assigned(MAI) and Assigned(MAI.CalibrationTimes) and (MAI.CalibrationTimes.Count > 0) then
      begin
        FCalibrationDlg.SetCalibrations(MAI.CalibrationTimes);
        ReltimeTree.FocusOnRoot;
        Reltimetree.SetOutgroupCluster(clSilver, ReltimeTree.NodeInfo.Des2Index);
        if Assigned(FRelTimeAnalyzer) and FReltimeAnalyzer.IsSamplingTime then
        begin
          if version <= 1005 then { bug fix needed to preserve backwards compatibility}
          begin
            MAI.CalibrationTimes.CalibrationsType := ctLeafNode;
            for i := 0 to MAI.CalibrationTimes.Count - 1 do
                MAI.CalibrationTimes.GetCalibration(i).IsSamplingTime := True;
          end;
          ReltimeTree.LatestSampleTime := MAI.CalibrationTimes.GetLatestSamplingTime;
        end;
      end;

      SessionFileName := filename;
      Caption := filename;
      Result := true;
      UpdateClientSize;
      if Result then
        MegaForm.AppendToRecentlyUsedFiles(Filename);
      if Assigned(ComputeParsimInfo) and Tree.isValue then
        InfoDisplay.TreeLength := Round(Tree.Value[Tree.TreeIndex]);
    except
      on E: EAbort do
      begin
        { do nothing as we aborted purposefully}
      end;
     on E: Exception do
     begin
       FProgress.Hide;
       if IsSessionTest then
         raise Exception.Create(E.Message)
       else
         ShowMessage('Oh no! An error occurred when reading the session file: ' + E.Message);
     end;
    end;
  finally
    FIsLoadingSession := False;
    CloseFile(data);
    FProgress.StopProgress;
    FProgress.Hide;
    if Assigned(aNames) then
      aNames.Free;
    if Assigned(aLabels) then
      aLabels.Free;
    if Assigned(aData) then
      aData.Free;
    if IsSessionTest then
      MegaForm.SessionTestFinished(Self);
  end;
end;

function TTreeViewForm.ImportNewickStandard(filename: String; Unroot: Boolean): boolean;
begin
  try
    try
      FRebuildingTree := True;
      if DataOpened then
      begin
        DataOpened := false;
        IsBootTree := false;
        FreeDistMatrix(D, OriTree.NoOfOTUs);
        D := nil;
        OriTree.Clear;
        BootTree.Clear;
        ReltimeTree.Clear;
        IsBootTree := false;
        Caption := ConvertToMegaWinCaption('TreeExplorer');
      end;
      result := false;
      Application.ProcessMessages;
      if OriTree.ImportFromNewickStandard(filename, Unroot) then
      begin
        Application.ProcessMessages;
        D := OriTree.DistanceMatrix;
        Tree := OriTree;
        Caption := ConvertToMegaWinCaption('TreeExplorer ('+filename+')');
        ActionRootOnMidpoint.Checked := false;
        ActionBalancedTree.Checked   := false;
        ActionInputOrderTree.Checked := false;
        InitForm;
        Application.processMessages;
        DataOpened := true;
        DataSaved := false;
        Application.ProcessMessages;
        Result := True;
        FRelTimeActionDisabled := True;
        CaptionHideActionExecute(Self);
      end;
      UpdateClientSize;
      Tree.Refresh;
      Application.ProcessMessages;
      Tree.Invalidate;
    except
      on E:Exception do
        ShowMessage('Oh no! Failed to import the newick tree: ' + E.Message);
    end;
  finally
    FRebuildingTree := False;
  end;
end;

function TTreeViewForm.ImportFromNewickString(NewickString: String): Boolean;
var
  PleaseWait : TPleaseWait;
begin
  if DataOpened then
  begin
    DataOpened := false;
    IsBootTree := false;
    if Assigned(D) then
      FreeDistMatrix(D, OriTree.NoOfOTUs);
    D := nil;
    OriTree.Clear;
    BootTree.Clear;
    ReltimeTree.Clear;
    IsBootTree := false;
    Caption := ConvertToMegaWinCaption('TreeExplorer');
  end;
  result := false;

  PleaseWait := TPleaseWait.Create(self);
  PleaseWait.PercentDone := 0;
  PleaseWait.Show;

  if OriTree.ImportFromNewickString(NewickString) then
  begin
    D := OriTree.DistanceMatrix;
    Tree := OriTree;
    ActionRootOnMidpoint.Checked := false;
    ActionBalancedTree.Checked   := false;
    ActionInputOrderTree.Checked := false;
    InitForm;
    DataOpened := true;
    DataSaved := false;
    Tree.Refresh;
    Result := True;
    FRelTimeActionDisabled := True;
    CaptionHideActionExecute(Self);
  end;
  FreeAndNil(PleaseWait);
  UpdateClientSize;
end;

function TTreeViewForm.ImportFromNewickString(NewickString: AnsiString; NamesList: TStringList): Boolean;
var
  PleaseWait : TPleaseWait;
begin
  if DataOpened then
  begin
    DataOpened := false;
    IsBootTree := false;
    if Assigned(D) then
      FreeDistMatrix(D, OriTree.NoOfOTUs);
    D := nil;
    OriTree.Clear;
    BootTree.Clear;
    ReltimeTree.Clear;
    IsBootTree := false;
    Caption := ConvertToMegaWinCaption('TreeExplorer');
  end;
  result := false;

  PleaseWait := TPleaseWait.Create(self);
  PleaseWait.PercentDone := 0;
  PleaseWait.Show;

  if OriTree.ImportFromNewickString(NewickString, NamesList) then
  begin
    D := OriTree.DistanceMatrix;
    Tree := OriTree;
    ActionRootOnMidpoint.Checked := false;
    ActionBalancedTree.Checked   := false;
    ActionInputOrderTree.Checked := false;
    InitForm;
    DataOpened := true;
    DataSaved := false;
    Tree.Refresh;
    Result := True;
    FRelTimeActionDisabled := True;
    CaptionHideActionExecute(Self);
  end;
  FreeAndNil(PleaseWait);
  UpdateClientSize;
end;

function TTreeViewForm.ImportTreeList(AList: TTreeList; doVisualUpdates: Boolean = False; Filename: String = ''): Boolean;
begin
  Result := False;

  try
    try
      FRebuildingTree := True;
      if DataOpened then
      begin
        DataOpened := false;
        IsBootTree := false;
        FTimeTreeTypes := [];
        FreeDistMatrix(D, OriTree.NoOfOTUs);
        D := nil;
        OriTree.Clear;
        BootTree.Clear;
        ReltimeTree.Clear;
        IsBootTree := false;
        Caption := ConvertToMegaWinCaption('TreeExplorer');
      end;
      if not AList.isStats then
        OriTree.ShowStats := False;
      OriTree.SetTreeList(AList, False);
      if ALIst.isBLen and (OriTree.NoOfOTUs <= 1000) then
        OriTree.ShowBLen := True;
      D := OriTree.DistanceMatrix;
      Tree := OriTree;
      Tree.Visible := True;
      FNewickFileName := ExtractFileName(Filename);
      Caption := ConvertToMegaWinCaption('TreeExplorer (' + ExtractFileName(Filename) + ')');
      ActionRootOnMidpoint.Checked := false;
      ActionBalancedTree.Checked   := false;
      ActionInputOrderTree.Checked := True;
      InitForm;
      DataOpened := true;
      DataSaved := false;
      Result := True;
      FRelTimeActionDisabled := True;
      if doVisualUpdates then
      begin
        DisableCaption;
        UpdateClientSize;
        Tree.PixelsPerOTU := DEFAULT_PPOTU;
        Tree.SortClusterForShape;
        {$IFDEF DARWIN}
        ResizeTreeToFitWindow(True);
        {$ENDIF}
        Tree.Refresh;
        Invalidate;
        Application.ProcessMessages;
      end;
      Result := True;
    except
      on E:Exception do
        ShowMessage('Oh no! Failed to import the newick tree: ' + E.Message);
    end;
  finally
    FRebuildingTree := False;
  end;
end;

procedure TTreeViewForm.ShowSubtreeWindow;
var
  SubtreeWindow: TTreeViewForm;
begin
  if not (Tree.NodeFocused or Tree.BranchFocused) then
    Exit;
  if Tree.NodeInfo.NodeType = ntRoot then
    Exit;
  if Tree.FocusedIndex <= Tree.NoOfOTUs then
    Exit;

  SubtreeWindow := TTreeViewForm.Create(Self);
  Tree.GetSubTree(SubtreeWindow.OriTree);
  SubtreeWindow.RelTimeActionDisabled := True;
  SubtreeWindow.OriTree.TreeWidth := Tree.TreeWidth;
  if SubtreeWindow.OriTree.NoOfOTUs > 40 then
     SubtreeWindow.OriTree.PixelsPerOTU := Abs(Tree.OTU_Font.Height)
  else if SubtreeWindow.OriTree.NoOfOTUs > 20 then
     SubtreeWindow.OriTree.PixelsPerOTU  := Abs(Tree.OTU_Font.Height)*3 div 2
  else
     SubtreeWindow.OriTree.PixelsPerOTU  := Abs(Tree.OTU_Font.Height)*2;
  if SubtreeWindow.OriTree.PixelsPerOTU < Tree.PixelsPerOTU then
    SubtreeWindow.OriTree.PixelsPerOTU := Tree.PixelsPerOTU;
  SubtreeWindow.D := SubtreeWindow.OriTree.DistanceMatrix;
  SubtreeWindow.Tree := SubtreeWindow.OriTree;
  SubtreeWindow.Tree.Refresh;
  SubtreeWindow.ActionSaveSession.Caption := 'Save this subtree';
  SubtreeWindow.ActionExportCurrentTree.Caption := 'Export this subtree';
  SubtreeWindow.IsFree := true;
  SubtreeWindow.DataSaved := true;
  if assigned(MLAnalyzer) then  // THIS IS A TEMPORARY DISABLING of linearization which fails for subtrees generated by an ML method.
    SubtreeWindow.OriTree.LinearizeFunc := nil;
  SubtreeWindow.InitSidePanel;
  SubtreeWindow.UpdateSidePanel;
  SubtreeWindow.Show;
  SubtreeWindow.Caption := Copy(Caption, 1, Length(Caption) - 1) + '-subtree)';
  SubtreeWindow.CaptionHideActionExecute(SubtreeWindow);
  SubtreeWindow.DisableComputeFrame;
end;

function TTreeViewForm.SetOLSBranchLength(tree: TTreeData): double;
begin
  tree.isBLen := true;
  Result := FastOLSBranchLength(tree, DistanceMatrix);
end;

function TTreeViewForm.ShowingStrictClockTree: Boolean;
begin
  Result := ((Tree = ReltimeTree) and ((tttStrictClocksLocal in FTimetreeTypes) or (tttStrictClocksMain in FTimeTreeTypes)));
end;

procedure TTreeViewForm.SetMLAnalyzer;
begin
  if MLAnalyzer = nil then exit;

  OriTree.LinearizeFunc := MLLinearizeFunc;
  OriTree.BLenFunc      := MLBLenFunc;
  OriTree.ValueDecimals := 2;
  OriTree.MaxSiteIndex := MAI.MyNoOfSites-1;
  if IsBootTree {MAI.MyBootPartitionList <> nil} then
  begin
    BootTree.BLenFunc      := MLBLenFunc;
    BootTree.LinearizeFunc := MLLinearizeFunc;
    BootTree.ValueDecimals := 2;
  end;
  if IsReltimeTree then
  begin
    RelTimeTree.BLenFunc := MLBLenFunc;
    RelTimeTree.LinearizeFunc := nil;
    RelTimeTree.ValueDecimals := 2;
    RelTimeTree.HeightErrBarProc  := RelTimeHeightErrBarProc;
    ReltimeTree.MaxTimeFunc := RelTimeMaxHeightFunc;
    ReltimeTree.MinTimeFunc := RelTimeMinHeightFunc;
    ReltimeTree.DivTimeFunc := RelTimeHeightFunc;
    RelTimeTree.MaxSiteIndex := MAI.MyNoOfSites - 1;
  end;
  ShowAllAncStateProc := ShowAllMLAncState;
  ShowMostProbAncStateProc := ShowMLAncState;
  HideAmbigAncStateProc    := HideAmbigMLAncState;
end;

function TTreeViewForm.MLLinearizeFunc(treedata: TTreeData): double;
var
  PleaseWait : TPleaseWait;
begin
  if (MAI.ClockType = ctLocal) or (MAI.InitialUsrOperation = dtdoMLClockTest) then
  begin
    MLAnalyzer.GetTreeData(treedata);
  end
  else
  begin
    PleaseWait := TPleaseWait.Create(self);
    PleaseWait.PercentDone := 0;
    PleaseWait.Caption := 'Likelihood Analysis';
    PleaseWait.Action := 'Computing node heights...';
    PleaseWait.Show;
    if IsDeveloper then
      MLAnalyzer.MakeClockTree(treedata, MAI.MaxRateRatio, MAI.MyOtuNames)
    else
      MLAnalyzer.MakeClockTree(treeData, DEFAULT_MAX_RATE_RATIO, MAI.MyOtuNames);
    FreeAndNil(PleaseWait);
  end;

  result := MLAnalyzer.LogLikelihood;
  Tree.Value2[Tree.TreeIndex] := result;
end;

function TTreeViewForm.AnchoredMLRelTimeLinearizeFunc(TreeData: TTreeData): Double;
var
  origTreeData: TTreeData = nil;
  ResultTreeData: TTreeData = nil;
  MinTimes: TDivTimesArray;
  MaxTimes: TDivTimesArray;
  SampleTimes: TDivTimesArray;
  PleaseWait: TPleaseWait = nil;
  ErrorMsg: String = '';
  NodeLabels: TStringList = nil;
  aRateRatio: Double;
begin
  SetLength(MinTimes, 0);
  SetLength(MaxTimes, 0);
  SetLength(SampleTimes, 0);
  try
    PleaseWait := TPleaseWait.Create(Self);
    PleaseWait.SetToMarqueeMode;
    PleaseWait.Caption := 'RelTime Analysis';
    PleaseWait.Action := 'Computing divergence times...';
    PleaseWait.Show;
    NodeLabels := TStringList.Create;
    OriTree.GetInternalNodeLabels(NodeLabels);
    if Assigned(MAI.ClockTreeExport) then
      MAI.ClockTreeExport.Clear
    else
      MAI.ClockTreeExport := TStringList.Create;

    origTreeData := TTreeData.Create(MAI.MyNoOfSeqs, True, True, False, False);
    origTreeData.Assign(TreeData);
    if (not FCalibrations.ValidationDone) and (not FCalibrations.DoAllValidations(ErrorMsg, False)) then
    begin
      Assert((vsError in MAI.CalibrationTimes.ValidationMessages) or (vsWarning in MAI.CalibrationTimes.ValidationMessages));
      if vsError in MAI.CalibrationTimes.ValidationMessages then
      begin
        {$IFDEF VISUAL_BUILD}
        raise Exception.Create('Validation of time constraints failed: ' + ErrorMsg);
        {$ELSE}
        error_nv('Validation of time Constraints failed: ' + ErrorMsg);
        {$ENDIF};
      end
      else if vsWarning in MAI.CalibrationTimes.ValidationMessages then
      begin
        {$IFDEF VISUAL_BUILD}
        ShowMessage('TimeConstraints validation warning: ' + ErrorMsg);
        {$ELSE}
        warn_nv('Time Constraints validation warning: ' + ErrorMsg);
        {$ENDIF};
      end;
    end;
    if IsDeveloper then
      aRateRatio := MAI.MaxRateRatio
    else
      aRateRatio := DEFAULT_MAX_RATE_RATIO;

    if FCalibrations.CalibrationsType = ctInternalNode then
    begin
      FCalibrations.PrepareDivTimeArrays(MinTimes, MaxTimes, MAI.MyNoOfSeqs, False);
      ResultTreeData := FReltimeAnalyzer.ComputeRelTimeBLens(origTreeData, MAI.ClockTreeExport, MAI.MyOtuNames, FCalibrations, aRateRatio);
      FReltimeAnalyzer.GenerateCalibratedClockTreeExport(MAI.ClockTreeExport, MAI.MyOtuNames, NodeLabels, MinTimes, MaxTimes);
    end
    else
    begin
      FCalibrations.PrepareSamplingTimeArray(SampleTimes, MAI.MyOtuNames);
      ResultTreeData := FReltimeAnalyzer.ComputeRelTimeBLens(origTreeData, SampleTimes, aRateRatio, MAI.MyOtuNames);
      FReltimeAnalyzer.GenerateSampledTimesClockTreeExport(MAI.ClockTreeExport, MAI.MyOtuNames, NodeLabels, SampleTimes);
      ReltimeTree.ShowCalibratedNodeMarker := False;
    end;

    TreeData.Assign(ResultTreeData);
    OriTree.UpdateDataCoverage(origTreeData);
    OriTree.ShowDataCoverage := False;
    ReltimeTree.ShowDataCoverage := False;
    Result := ResultTreeData.SBL;
    Tree.Value2[Tree.TreeIndex] := Result;
  finally
    if Assigned(origTreeData) then
      origTreeData.Free;
    if Assigned(PleaseWait) then
      PleaseWait.Free;
    if Assigned(NodeLabels) then
      NodeLabels.Free;
    if Assigned(ResultTreeData) then
      ResultTreeData.Free;
  end;
end;

function TTreeViewForm.MLBLenFunc(treedata: TTreeData): double;
var
  PleaseWait : TPleaseWait;
  oritreedata,tmptreedata: TTreeData;
begin
  {$IFNDEF DARWIN}
  PleaseWait := TPleaseWait.Create(self);
  PleaseWait.PercentDone := 0;
  PleaseWait.Caption := 'Likelihood Analysis';
  PleaseWait.Action := 'Computing branch lengths...';
  PleaseWait.Show;
  {$ENDIF}
  oritreedata := TTreeData.Create(treedata.NoOfOTUs, true, true, true);
  tmptreedata := TTreeData.Create(treedata.NoOfOTUs, true, true, true);
  tmptreedata.Assign(treedata);

  MLAnalyzer.GetTreeData(oritreedata);
  MLAnalyzer.SetTreeData(tmptreedata);

  MLAnalyzer.OptimizeBLens;
  MLAnalyzer.GetTreeData(treedata);
  result := MLAnalyzer.LogLikelihood;

  MLAnalyzer.SetTreeData(oritreedata);
  MLAnalyzer.Initialize;
  MLAnalyzer.InitAncStateEst;
  {$IFNDEF DARWIN}
  FreeAndNil(PleaseWait);
  {$ENDIF}
  treedata.Value := result;
end;

procedure TTreeViewForm.RelTimeHeightErrBarProc(NodeIndex: integer; var err0,err1: double);
begin
  try
    if (RelTimeAnalyzer = nil) or RelTimeAnalyzer.Empty then
      Exit;
    err0 := Max(0, ReltimeAnalyzer.MaxNodeHeight(NodeIndex - 1) - ReltimeAnalyzer.NodeHeight(NodeIndex - 1));
    err1 := Max(0, ReltimeAnalyzer.NodeHeight(NodeIndex - 1) - ReltimeAnalyzer.MinNodeHeight(NodeIndex - 1));
  except
    Exit;
  end;
end;

function TTreeViewForm.RelTimeMinHeightfunc(NodeIndex: Integer): Double;
begin
  Result := 0.0;
  if (RelTimeAnalyzer = nil) or RelTimeAnalyzer.Empty then
    Exit;
  Result := RelTimeAnalyzer.MinDivTime(NodeIndex - 1);
end;

function TTreeViewForm.RelTimeMaxHeightfunc(NodeIndex: Integer): Double;
begin
  Result := 0.0;
  if (RelTimeAnalyzer = nil) or RelTimeAnalyzer.Empty then
    Exit;
  Result := RelTimeAnalyzer.MaxDivTime(NodeIndex - 1);
end;

function TTreeViewForm.RelTimeHeightFunc(NodeIndex: Integer): Double;
begin
  Result := 0.0;
  if (RelTimeAnalyzer = nil) or RelTimeAnalyzer.Empty then
    Exit;
  Result := RelTimeAnalyzer.DivTime(NodeIndex - 1);
end;

procedure TTreeViewForm.DoRelTimeNoCalibrations;
begin
  MegaForm.RemoveWindowFromTray(FCalibrationDlg);
  FCalibrationDlg.Hide;
  if FUseMLForReltime then
    ReltimeTree.LinearizeFunc := RelTimeMLLinearizeFunc
  else
    ReltimeTree.LinearizeFunc := ReltimeLinearizeFunc;
  ReltimeTree.HeightErrBarProc := RelTimeHeightErrBarProc;
  ReltimeTree.MaxTimeFunc := RelTimeMaxHeightFunc;
  ReltimeTree.MinTimeFunc := RelTimeMinHeightFunc;
  ReltimeTree.DivTimeFunc := RelTimeHeightFunc;
  ReltimeTree.IsTimes := True;
  ReltimeTree.ShowDivergenceTimes := True;
  ReltimeTree.TimeUnit := 'Relative Times';
  ReltimeTree.ShowScale := False;
  FigureGenerator.AssignData(DISP_DIV_TIMES_STR, 'True');
  ReltimeTree.CharStateFont := OriTree.CharStateFont;
  ReltimeTree.MaxSiteIndex := MAI.MyNoOfSites - 1;
  Include(FTimeTreeTypes, tttReltimeLocal);
  if FUseMLForReltime then
  begin
    Self.ActionMLLinearizedExecute(nil);
    if not Assigned(MAI.ReltimeNexusExport) then
      MAI.ReltimeNexusExport := FReltimeAnalyzer.GenerateNexusReltimeExport(MAI.MyOtuNames, False);
  end
  else
  begin
    Self.ActionLinearizedExecute(nil);
    MAI.ReltimeNexusExport := RelTimeAnalyzer.GenerateNexusReltimeExport(MAI.MyOtuNames, False, True);
  end;

  FReltimeActionDisabled := True;
  RelTimeActionVisible := False;
  FCalibrationsActionEnabled := False;
end;

procedure TTreeViewForm.SetUpRelTimeCalibrations;
var
  aCalibType: TCalibrationTarget = ctInternalNode;
begin
  if not PromptForCalibrationsType(aCalibType) then
    Exit;
  IsSetRelTimeCalibsMode := True;
  FCalibrationsActionEnabled := True;
  RelTimeActionVisible := False;
  FCalibrationDlg.DisplayMode := cdmAfterConstructTree;
  Tree.AssignTreeListInternalNodeLabelFunc(FCalibrationDlg.GetInternalNodeLabelFunc);
  ColorOutgroupClusterGray;
  FCalibrationDlg.CalibrationsType := aCalibType;
  FCalibrationDlg.FinalizeCalibrationFunc := Tree.FinalizeCalibration;
  FCalibrationDlg.CWSaveChangesCallBack := AssignCalibrations;
  FCalibrationDlg.CWCancelCallBack := CancelCalibrationChanges;
  FCalibrationDlg.HasBadCalibsFunc := HasInvalidCalibrations;
  FCalibrationDlg.ResetForm;
  RefreshCalibrationDlgTreeList;
  MoveTreeToCalibrationDlg;
  MegaForm.AddWindowToTray(FCalibrationDlg);
  FCalibrationDlg.Show;
  if aCalibType = ctLeafNode then
    FCalibrationDlg.PromptIfTipDatesInNames
end;

procedure TTreeViewForm.RefreshCalibrationDlgTreeList;
var
  TempNames: TStringList = nil;
  TempTreeList: TTreeList = nil;
  ATree: TTreeData = nil;
begin
  try
    TempTreeList := TTreeList.Create;
    TempNames := Tree.GetOTUNamesList;
    TempTreeList.OTUNameList.Assign(TempNames);
    ATree := TTreeData.Create(Tree.NoOfOTUs, True, False, False);
    Tree.GetTreeData(ATree);
    TempTreeList.Add(ATree);
    FCalibrationDlg.SetTreeList(TempTreeList);
  finally
    if Assigned(TempNames) then
      TempNames.Free;
    if Assigned(TempTreeList) then
      TempTreeList.Free;
  end;
end;

procedure TTreeViewForm.DoRelTimeWithCalibrations;
begin
  FCalibrationsActionEnabled := False;
  RelTimeActionVisible := False;
  ActionRelTime.Checked := True;
  ReltimeActionDisabled := True;
  Include(FTimeTreeTypes, tttReltimeLocal);
  Exclude(FTimeTreeTypes, tttStrictClocksLocal);
  Exclude(FTimeTreeTypes, tttStrictClocksMain);
  ReltimeTree.isTimes := True;
  ReltimeTree.ShowDivergenceTimes := True;
  ReltimeTree.TimeUnit := 'Divergence Time';
  ReltimeTree.ShowTimeScale := True;
  ReltimeTree.ShowScale := True;
  ReltimeTree.ShowHeightErrBar := False;
  FigureGenerator.AssignData(DISP_DIV_TIMES_STR, 'True');
  ReltimeTree.ShowScale := False;
  ReltimeTree.CharStateFont := OriTree.CharStateFont;
  ReltimeTree.MaxSiteIndex := MAI.MyNoOfSites - 1;
  ReltimeTree.LinearizeFunc := nil;
  if FUseMLForReltime then
    ReltimeTree.LinearizeFunc := AnchoredMLRelTimeLinearizeFunc
  else
    ReltimeTree.LinearizeFunc := AnchoredRelTimeLinearizeFunc;
  ReltimeTree.HeightErrBarProc := ReltimeHeightErrBarProc;
  ReltimeTree.MaxTimeFunc := RelTimeMaxHeightFunc;
  ReltimeTree.MinTimeFunc := RelTimeMinHeightFunc;
  ReltimeTree.DivTimeFunc := RelTimeHeightFunc;

  if FUseMLForReltime then
  begin
    Self.ActionMLLinearizedExecute(nil);
    FigureGenerator.AssignData(DISP_ERR_BARS_STR, 'False');
    MAI.ReltimeNexusExport := FReltimeAnalyzer.GenerateNexusReltimeExport(MAI.MyOtuNames, True, True);
  end
  else
  begin
    Self.ActionLinearizedExecute(nil);
    MAI.ReltimeNexusExport := RelTimeAnalyzer.GenerateNexusReltimeExport(MAI.MyOtuNames, True, True);
  end;
end;

procedure TTreeViewForm.AssignCalibrations(Calibrations: TCalibrations);
begin
  Assert(Calibrations.Count > 0);
  Assert(Assigned(FTimeTreeWizardForm));
  if not Assigned(FCalibrations) then
    FCalibrations := TCalibrations.Create;
  IsSetRelTimeCalibsMode := False;
  FCalibrations.Assign(Calibrations);
  FCalibrationDlg.Hide;
  MegaForm.RemoveWindowFromTray(FCalibrationDlg);
  if IsDeveloper then
    FTimeTreeWizardForm.UpdateView(ttpsDoSettings)
  else
    FTimeTreeWizardForm.UpdateView(ttpsLaunchAnalysis);
  OriTree.ShowCalibratedNodeMarker := False;
  OriTree.UpdateCalibratedNodes(nil); { were set by calibrationdlg for drawing the markers}
  ColorOutgroupClusterBlack;
end;

procedure TTreeViewForm.CancelCalibrationChanges;
begin
  if FFormIsClosing then
    Exit;
  RetrieveTreeFromCalibrationDlg;
  FRelTimeActionDisabled := True;
  ReltimeActionVisible := True;
  FTimeTreeTypes := [];
  FCalibrationsActionEnabled := False;
  IsSetReltimeCalibsMode := False;
  FTimeTreeWizardForm.UpdateView(ttpsDoCalibrations);
  if Tree = OriTree then
    ColorOutgroupClusterBlack;
  Self.Show;
  FCalibrationDlg.ClearCalibrations;
  FCalibrationDlg.ClearForm;
  FCalibrationDlg.Hide;
end;

procedure TTreeViewForm.PrepareForReltimeDisplay;
begin
  PrepareOriTreeForReltimeDisplay;
  PrepareReltimeTreeForReltimeDisplay;
  ResetStatusBar;
  if ActionInfo.Checked then
  begin
    RenewTreeInfo;
    RenewBranchInfo;
  end;
  MegaForm.SetTimeTreeWizardFree;
end;

procedure TTreeViewForm.PrepareOriTreeForReltimeDisplay;
var
  ATreeList: TTreeList = nil;
  ATreeData: TTreeData = nil;
  i: Integer;
  NumTaxa: Integer;
begin
  if (MAI.MyUsrOperation = dtdoRelTimeBLens) or (MAI.MyUsrOperation = dtdoRtdtBLens) then
    NumTaxa := MAI.MyOtuNames.Count
  else
    NumTaxa := MAI.MyNoOfSeqs;
  Tree := OriTree;
  ATreeData := TTreeData.Create(NumTaxa, True, False, False);
  ATreeData.Assign(MAI.MyOriTreeList[1]);
  ATreeList := TTreeList.Create;
  ATreeList.Information.Assign(MAI.MyOriTreeList.Information);
  ATreeList.IsRooted := True; { per Sudhir, we don't want to draw the root}
  for i := 0 to NumTaxa - 1 do
    ATreeList.OTUName[i] := MAI.MyOtuNames[i];
  ATreeList.Add(ATreeData);
  if (MAI.MyUsrOperation = dtdoRelTimeML) or (MAI.MyUsrOperation = dtdoRtdtML) then
  begin
    ATreeList.ValueName := 'LogL';
    Tree.Value[0] := MLAnalyzer.LogLikelihood;
    Tree.Value2[Tree.TreeIndex] := MLAnalyzer.MLTree.LogLikelihood;
  end
  else if IsReltimeNonMLAnalysis(MAI.MyUsrOperation) then
    Tree.Value2[Tree.TreeIndex] := Tree.SBL[Tree.TreeIndex];
  OriTree.SetTreeList(ATreeList, True);
  if (MAI.MyUsrOperation <> dtdoRelTimeBLens) and (MAI.MyUsrOperation <> dtdoRtdtBLens) then
    OriTree.SetOtuInfo(FOtuInfos);
  OriTree.ValueDecimals := 2; // default
  OriTree.ShowRoot := False;
  OriTree.Enabled := True;
  OriTree.IsRooted := False;
  OriTree.MakeRootByOutgroup(True);
  OriTree.IsRooted := True;
end;

procedure TTreeViewForm.PrepareReltimeTreeForReltimeDisplay;
begin
  Tree := ReltimeTree;
  MAI.MyOriTreeList.Delete(1); // so the ReltimeTree will have only the clock tree
  Tree.isTimes := True;
  Tree.DivTimeFunc := RelTimeHeightFunc;
  if not MAI.MyOriTreeList.IsStats then
    ReltimeTree.ShowStats := False;
  SetReltimeTrees(MAI.MyOriTreeList);
  if Assigned(FOtuInfos) then
    ReltimeTree.SetOtuInfo(FOtuInfos);
  Tree.ShowHeightErrBar := False; // we are hiding them by default because they are so large
  SetGroupInfo(MAI.MyGpNames);
  Tree.ValueDecimals := 2;
  Tree.MaxSiteIndex := MAI.MyNoOfSites - 1;
  Tree.CharStateFont := OriTree.CharStateFont;
  Tree.ShowTopologyOnly := False;
  Tree.HeightErrBarProc := RelTimeHeightErrBarProc;
  Tree.MaxTimeFunc := RelTimeMaxHeightFunc;
  Tree.MinTimeFunc := RelTimeMinHeightFunc;

  if (MAI.MyUsrOperation = dtdoReltimeML) or (MAI.MyUsrOperation = dtdoRtdtML) then
  begin
    if (not FReltimeAnalyzer.IsSamplingTime) then
    begin
      Tree.IsLinearized := True; // will force a call to TTreeBox.SetIsLinearized which will execute a tree linearization function
      Tree.ShowScale := False;
    end
    else
    begin
      Tree.IsSamplingTimes := True;
      Tree.ShowSamplingTimeScale := True;
      Tree.ScaleUnit := 'Branch Length';
    end;
    if Tree.IsSamplingTimes then
      Tree.ShowCalibratedNodeMarker := False;

    Tree.Value2[Tree.TreeIndex] := MLAnalyzer.MLTree.LogLikelihood;
    if MAI.CalibrationTimes.Count > 0 then
      Tree.TimeFactor := FReltimeAnalyzer.TimeFactor;
    Tree.LatestTime := FReltimeAnalyzer.LatestTime;
  end
  else if IsReltimeNonMLAnalysis(MAI.MyUsrOperation) then
  begin
    Tree.IsSamplingTimes := MAI.CalibrationTimes.IsSampleTimes;
    if Tree.IsSamplingTimes then
    begin
      Tree.LatestTime := FReltimeAnalyzer.LatestTime;
      Tree.ShowCalibratedNodeMarker := False;
    end;
    Tree.IsLinearized := (not Tree.IsSamplingTimes);
    Tree.ShowSamplingTimeScale := Tree.IsSamplingTimes;
    Tree.Value2[Tree.TreeIndex] := Tree.SBL[Tree.TreeIndex];
    if MAI.CalibrationTimes.Count > 0 then
      ReltimeTree.TimeFactor := FReltimeAnalyzer.TimeFactor;
  end;

  if MAI.CalibrationTimes.Count > 0 then
  begin
    ReltimeTree.TimeUnit := 'Divergence Time';
    ReltimeTree.ShowTimeScale := True;
  end
  else
  begin
    ReltimeTree.TimeUnit := 'Relative Time';
    ReltimeTree.TimeFactor := 1.0;
    ReltimeTree.ShowScale := False;
  end;
  Tree.ShowScale := False; // (MAI.CalibrationTimes.Count > 0);
  Tree.ShowDivergenceTimes := True;
  Tree.ShowTimeScale := True;
  Tree.IsRooted := False;
  Tree.MakeRootByOutgroup(True);
  Tree.MakeBranchFromRoot;           // KT: to show ingroup root.
  Tree.IsRooted := True;
  ColorOutgroupClusterGray;
  CompressOutgroup(MAI.HasCalibrations);
  {$IFDEF DARWIN}
  TreeTabControlChangeBounds(Self);
  {$ENDIF}
end;

function TTreeViewForm.AnchoredRelTimeLinearizeFunc(TreeData: TTreeData): Double;
var
  PleaseWait: TPleaseWait = nil;
  ReltimeTreeData: TTreeData = nil;
  NodeLabels: TStringList = nil;
  CalibrationIndices: ArrayOfInteger = nil;
  i: Integer;
  aMaxRateRatio: Double;
  MinTimes: TDivTimesArray = nil;
  MaxTimes: TDivTimesArray = nil;
  SampleTimes: TDivTimesArray = nil;
begin
  PleaseWait := nil;
  ReltimeTreeData := nil;
  NodeLabels := TStringList.Create;
  OriTree.GetInternalNodeLabels(NodeLabels);
  try
    PleaseWait := TPleaseWait.Create(Self);
    PleaseWait.SetToMarqueeMode;
    PleaseWait.Caption := 'RelTime Analysis';
    PleaseWait.Action := 'Computing divergence times...';
    PleaseWait.Show;

    if Assigned(MAI.ClockTreeExport) then
    begin
      MAI.ClockTreeExport.Clear;
    end
    else
      MAI.ClockTreeExport := TStringList.Create;
    if IsDeveloper then
      aMaxRateRatio := MAI.MaxRateRatio
    else
      aMaxRateRatio := DEFAULT_MAX_RATE_RATIO;

    if FCalibrations.CalibrationsType = ctInternalNode then
    begin
      FCalibrations.PrepareDivTimeArrays(MinTimes, MaxTimes, TreeData.NoOfOTUs, False);
      ReltimeTreeData := FReltimeAnalyzer.ComputeRelTimeBLens(TreeData, MAI.ClockTreeExport, MAI.MyOtuNames, FCalibrations, aMaxRateRatio);
    end
    else
    begin
      FCalibrations.PrepareSamplingTimeArray(SampleTimes, MAI.MyOtuNames);
      ReltimeTreeData := FReltimeAnalyzer.ComputeRelTimeBLens(TreeData, SampleTimes, aMaxRateRatio, MAI.MyOtuNames);
      FReltimeAnalyzer.GenerateSampledTimesClockTreeExport(MAI.ClockTreeExport, MAI.MyOtuNames, NodeLabels, SampleTimes);
      ReltimeTree.ShowCalibratedNodeMarker := False;
    end;

    TreeData.Assign(RelTimeTreeData);
    OriTree.UpdateDataCoverage(ReltimeTreeData);
    OriTree.ShowDataCoverage := False;
    ReltimeTree.ShowDataCoverage := False;
    Result := RelTimeTreeData.SBL;
    Tree.Value2[Tree.TreeIndex] := Result;
    Include(FTimeTreeTypes, tttReltimeLocal);
    CalibrationsActionEnabled := False;
    SetLength(CalibrationIndices, FCalibrations.Count);
    for i := 0 to FCalibrations.Count - 1 do
      CalibrationIndices[i] := FCalibrations.GetCalibration(i).TreeViewFormIndex;
    Tree.CalibratedNodes := CalibrationIndices;
    RelTimeActionVisible := True;
    MAI.ReltimeNexusExport := FRelTimeAnalyzer.GenerateNexusReltimeExport(MAI.MyOtuNames, True, True)
  finally
    if Assigned(PleaseWait) then
      PleaseWait.Free;
    if Assigned(RelTimeTreeData) then
      RelTimeTreeData.Free;
    if Assigned(NodeLabels) then
      NodeLabels.Free;
  end;
end;

function TTreeViewForm.RelTimeLinearizeFunc(TreeData: TTreeData): Double;
var
  PleaseWait: TPleaseWait;
  RelTimeTreeData: TTreeData;
begin
  PleaseWait := nil;
  RelTimeTreeData := nil;

  try
    try
      PleaseWait := TPleaseWait.Create(Self);
      PleaseWait.SetToMarqueeMode;
      PleaseWait.Caption := 'RelTime Analysis';
      PleaseWait.Action := 'Computing relative divergence times...';
      PleaseWait.Show;

      if Assigned(MAI.ClockTreeExport) then
      begin
        MAI.ClockTreeExport.Clear;
      end
      else
        MAI.ClockTreeExport := TStringList.Create;

      if IsDeveloper then
        RelTimeTreeData := FRelTimeAnalyzer.ComputeRelTimeBLens(TreeData, MAI.ClockTreeExport, MAI.MyOtuNames, nil, MAI.MaxRateRatio)
      else
        RelTimeTreeData := FRelTimeAnalyzer.ComputeRelTimeBLens(TreeData, MAI.ClockTreeExport, MAI.MyOtuNames, nil, DEFAULT_MAX_RATE_RATIO);
      TreeData.Assign(RelTimeTreeData);
      Result := RelTimeTreeData.SBL;
      Tree.Value2[Tree.TreeIndex] := Result;
      OriTree.ShowDataCoverage := False;
      ReltimeTree.ShowDataCoverage := False;
      MAI.ReltimeNexusExport := FRelTimeAnalyzer.GenerateNexusReltimeExport(MAI.MyOtuNames, False, True);
    Except
      on E:Exception do
        ShowMessage('Oh no! An error occurred when doing the reltime analysis: ' + E.Message);
    end;
  finally
    PleaseWait.Free;
    RelTimeTreeData.Free;
  end;
end;

function TTreeViewForm.RelTimeMLLinearizeFunc(TreeData: TTreeData): Double;
var
  origTreeData: TTreeData;
  newTreeData: TTreeData;
  PleaseWait: TPleaseWait;
  NodeLabels: TStringList;
begin
  origTreeData := nil;
  newTreeData := nil;
  PleaseWait := nil;
  NodeLabels := nil;

  try
    PleaseWait := TPleaseWait.Create(Self);
    PleaseWait.SetToMarqueeMode;
    PleaseWait.Caption := 'RelTime Analysis';
    PleaseWait.Action := 'Computing divergence times...';
    PleaseWait.Show;
    if Assigned(MAI.ClockTreeExport) then
      MAI.ClockTreeExport.Clear
    else
      MAI.ClockTreeExport := TStringList.Create;
    NodeLabels := TStringList.Create;
    OriTree.GetInternalNodeLabels(NodeLabels);
    origTreeData := TTreeData.Create(MAI.MyNoOfSeqs, True, True, False, False);
    origTreeData.Assign(TreeData);
    if IsDeveloper then
      newTreeData := FReltimeAnalyzer.ComputeRelTimeBLens(origTreeData, MAI.ClockTreeExport, MAI.MyOtuNames, NodeLabels, MAI.MaxRateRatio)
    else
      newTreeData := FReltimeAnalyzer.ComputeRelTimeBLens(origTreeData, MAI.ClockTreeExport, MAI.MyOtuNames, NodeLabels, DEFAULT_MAX_RATE_RATIO);
    TreeData.Assign(newTreeData);
    OriTree.UpdateDataCoverage(origTreeData);
    OriTree.ShowDataCoverage := False;
    ReltimeTree.ShowDataCoverage := False;
    Result := newTreeData.SBL;
    ReltimeTree.Value2[ReltimeTree.TreeIndex] := Result;
    MAI.ReltimeNexusExport := FReltimeAnalyzer.GenerateNexusReltimeExport(MAI.MyOtuNames, False);
  finally
    if Assigned(origTreeData) then
      origTreeData.Free;
    if Assigned(newTreeData) then
      newTreeData.Free;
    if Assigned(PleaseWait) then
      PleaseWait.Free;
  end;
end;

procedure TTreeViewForm.UpdateClientSize;
var
  NewHeight: Integer;
  NewWidth: Integer;
  CaptionHeight: Integer;
  MaxHeight: Integer;
  WindowAdds: Integer; { how much height is added by the title bar, scrollbars, resize border}
begin
  MaxHeight := Screen.WorkAreaHeight - 30; { Desktop screen height - some pixels for the task bar}

  if CaptionPanel.Visible then
    CaptionHeight := CaptionPanel.Height
  else
    CaptionHeight := 0;
  WindowAdds := (Height - ClientHeight);
  NewHeight :=  Tree.Height + CaptionHeight + WindowAdds + 20;
  NewWidth := (ClientWidth - Tree.Width) + Tree.MinWidth + 100;
  if (NewHeight + Top) >= MaxHeight then // if the tree is close to going over the size of the screen then shrink it to fit the screen
    NewHeight := MaxHeight;
  if (newWidth + Left) >= (screen.Width - 60) then // if the tree is close to going over the size of the screen then shrink it to fit the screen
    NewWidth := screen.Width - Left - 60;
  Height := Max(700, NewHeight); { adjust if necessary so that the left toolbar does not need to wrap}
  Width := Max(600, NewWidth);
  Tree.Height := (Height - (CaptionHeight) - WindowAdds - 20);
end;

procedure TTreeViewForm.ScrollToBottom;
begin
  Tree.ScrollToBottom;
end;

procedure TTreeViewForm.ResizeTreeToFitWindow(doRefresh: Boolean);
begin
  ResizeTreeToFitWindow(Tree, doRefresh);
end;

procedure TTreeViewForm.ResizeTreeToFitWindow(aTree: TTreeBox;doRefresh: Boolean);
const
  TOP_BOTTOM_MARGIN = 10;
  SIDE_MARGIN = 20;
var
  PPOTU: Integer;
begin
  if aTree.NoOfOTUs <= 0 then
    Exit;

  Case aTree.TreeStyle of
    tsTraditional:
    begin
      PPOTU := Max(1, (aTree.ClientHeight - TOP_BOTTOM_MARGIN) div aTree.NoOfOTUs);
      aTree.PixelsPerOTU := PPOTU;
      aTree.TreeWidth := TreeTabControl.ClientWidth - GetSystemMetrics(SM_CXVSCROLL) - SIDE_MARGIN*2 - aTree.LargestWidthOfOTUNames;
      aTree.SetAttrIndex;
      if doRefresh then
        aTree.Refresh;
      aTree.Invalidate;
    end;
    tsCircle:  // There is no simple way to calculate the BranchLength or Radius Values to fit inside the current window.  This function isn't terribly important so it's probably OK if we disable it for non-traditional trees.
    begin
      if not doRefresh then
        ShowMessage('Auto fit is not available for Circle trees.');
    end;
    tsRadiation:
    begin
      if not doRefresh then
        ShowMessage('Auto fit is not available for Radiation trees.');
    end;
  end;
end;

procedure TTreeViewForm.AdjustTreeForBestFitToWindow(doRefresh: Boolean=True);
var
  PPOTU: Integer = 0;
  newHeight: Integer = 0;
  aScaleBarHeight: Integer = 0;
begin
  PPOTU := Tree.PixelsPerOTU;
  with Tree do
  begin
    if NoOfOTUs <= 0 then
      Exit;
    if (Tree.MinHeight*1 > TreeTabControl.ClientHeight) and (Tree.MinWidth*1 > TreeTabControl.ClientWidth) then
    begin
      if NoOfOtus >= 1000 then
        PixelsPerOtu := 24;
      Exit;
    end;
    if Tree.ShowScale or Tree.ShowTimeScale then
      aScaleBarHeight := Tree.ScaleBarHeight
    else
      aScaleBarHeight := 0;
    Assert(Tree.TreeStyle = tsTraditional, 'invalid call to AdjustTreeForBestFitToWindow');
    if NoOfOtus >= 1000 then
      PPOTU := 24
    else
      PPOTU := max(1, ((TreeTabControl.ClientHeight - TopMargin - aScaleBarHeight) div NoOfOtus + 1));
    newHeight := (PPOTU+1) * NoOfOTUs;
    if (TreeHeight*1 - TopMargin) < newHeight then
      PixelsPerOtu := PPOTU;
    TreeWidth := TreeTabControl.Width - GetSystemMetrics(SM_CXVSCROLL) - LeftMargin - 15  - LargestWidthOfOTUNames;  // 15 for the tree's padding on each side.
    if PixelsPerOtu > 100 then
      PixelsPerOtu := PPOTU;
    if PixelsPerOtu < DEFAULT_PPOTU then
      PixelsPerOtu := DEFAULT_PPOTU;
    SetAttrIndex;
    if doRefresh then
      Refresh;
  end;
end;

procedure TTreeViewForm.InitForm;
begin
  try
    BeginFormUpdate;
    ResetForm;
    TreeTabControl.Enabled := True;
    TreeTabControl.Visible := True;
    if not FSidePanelInitialized then
    begin
      InitSidePanel;
      UpdateSidePanel;
    end;
    if (BootTree.NoOfTrees > 0) or BootTree.isConsensus then
    begin
      if OriTree.NoOfTrees = 0 then
        TreeTabControl.Tabs.Add(CONSENSUS_TREE_TAB)
      else
        TreeTabControl.Tabs.Add(ORI_TREE_TAB);
      if BootTree.NoOfTrees = 0 then
        TreeTabControl.Tabs.Add(BOOT_CONS_TREE_TAB)
      else
        TreeTabControl.Tabs.Add(BOOT_TREE_TAB);
    end;

    if RelTimeTree.NoOfTrees > 0 then
    begin
      if not ((Boottree.NoOfTrees > 0) or BootTree.isConsensus) then
        TreeTabControl.Tabs.Add(ORI_TREE_TAB);
      if TreeTabControl.Tabs.IndexOf(TIME_TREE_TAB) < 0 then
        TreeTabControl.Tabs.Add(TIME_TREE_TAB);
      Tree := ReltimeTree;
      TreeTabControl.TabIndex := TreeTabControl.Tabs.IndexOf(TIME_TREE_TAB);
      TreeTabControlChange(nil);
      FDivTimesFrame.Enabled := True;
    end;
    if TreeTabControl.Tabs.Count = 0 then
      TreeTabControl.TabHeight := 3
    else
      TreeTabControl.TabHeight := 0;
    TreeStyleMenuItem.Enabled := true;
    ChangeRootMenuItem.Enabled := true;
    ArrangeTaxaMenuItem.Enabled := true;
    ShowHideMenuItem.Enabled := true;
    FontMenuItem.Enabled := true;

    if Tree.isBranchLength then
    begin
      ActionBranchInfoFont.Enabled := true;
      ActionScaleBarFont.Enabled := true;
      if Tree.BLenDecimals = 0 then
      begin
        if CompareValue(Tree.SBL[Tree.TreeIndex]/Tree.NoOfOTUs, 0.0, FP_CUTOFF) > 0 then
        begin
          Tree.BLenDecimals := 4 -trunc(log10(Tree.SBL[Tree.TreeIndex]/Tree.NoOfOTUs));
          BootTree.BLenDecimals := Tree.BLenDecimals;
          RelTimeTree.BLenDecimals := Tree.BLenDecimals;
        end;
      end;
    end;

    if Tree.isStats then
      Tree.ShowStats := true;

    if (Tree.NoOfTrees > 1) and (Tree.TreeIndex > 0) and ((not Assigned(MAI) or (not IsReltimeAnalysis(MAI.MyUsrOperation)))) then
    begin
      FMultiTreesFrame.TreeNumSEdit.MinValue := 1;
      FMultiTreesFrame.TreeNumSEdit.MaxValue := Tree.NoOfTrees;
      FMultiTreesFrame.TreeNumSEdit.Value := Tree.TreeIndex;
      FMultiTreesFrame.NumTreesLabel.Caption := 'of ' + Format('%-6d', [Tree.NoOfTrees]);
      FMultiTreesFrame.Visible := True;
      ActionRootOnMidpoint.Checked := False;
      try
        FCheckStateChanging := True;
        FTaxaNamesFrame.BalancedShapeRadioBtn.Checked := False;
        FTaxaNamesFrame.InputOrderRadioBtn.Checked := True;
      finally
        FCheckStateChanging := False;
      end;
      ActionBalancedTree.Checked := False;
      ActionInputOrderTree.Checked := True;
    end
    else
      FMultiTreesFrame.Visible := False;

    if Tree.isAncState then
    begin
      FAncestorsFrame.AncSiteNumSpinEdit.MaxValue := Tree.MaxSiteIndex +1;
      FAncestorsFrame.AncSiteNumSpinEdit.MinValue := 1;
      FAncestorsFrame.AncSiteNumSpinEdit.Value := Tree.SiteIndex +1;
    end;

    if ActionRootOnMidpoint.Checked then
      Tree.MakeRootOnMidpoint;
    if ActionBalancedTree.Checked then
      Tree.SortClusterForShape
    else if ActionInputOrderTree.Checked then
      Tree.SortClusterInOrder;
    ResetStatusBar;
    if Visible and Tree.Visible then
      Tree.SetFocus;
    ActionExportTreeTabular.Enabled := True;
    GotoNodeBranch1.Visible := True;
    GotoSeparator.Visible := True;
    CorrTestAction.Enabled := (Tree = ReltimeTree);
    ResetStatusBar;
    ShowHideMacTabButtons;
  finally
    EndFormUpdate;
  end;
end;

procedure TTreeViewForm.ResetForm;
var
  i: integer;
begin
  TreeTabControl.Enabled := False;
  with TreeTabControl.Tabs do
      for i := Count-1 downto 0 do
          Delete(i);
  //MultiTreeToolBx.Hide;
  //AncStatesToolBx.Hide;
  TreeStyleMenuItem.Enabled := false;
  //TreeStyleButton.Enabled := false;
  ChangeRootMenuItem.Enabled := false;
  ArrangeTaxaMenuItem.Enabled := false;
  ShowHideMenuItem.Enabled := false;
  FontMenuItem.Enabled := false;
  InfoDisplay.Hide;
end;

procedure TTreeViewForm.AddFormObserver(observer: IFormObserver);
begin
  FFormObservers.Add(observer);
end;

end.

