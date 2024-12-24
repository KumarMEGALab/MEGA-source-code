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

unit MV_SeqDataExplorer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, Menus,
  ComCtrls, ActnList, Buttons, ExtCtrls, StdCtrls, MV_Columns, MDomainInfo,
  MegaConsts, genedomainpropertiesdlg, htmloptionsdlg,
  LCLType, IniPropStorage, Spin, mruntimeprogressdlg,
  mseqexportoptions, mhelpkeywords, mimageform, medit_site_labels_frame,
  contnrs, Types;

const
  TAXA_COLUMN = 0;
  GROUP_COLUMN = 1;
  CHECKMARK_IMAGE_INDEX = 1;
  UNCHECKMARK_IMAGE_INDEX = 0;
  MENUITEM_CHECKMARK_IMAGE_INDEX = 2;
  MIN_BASES_FOR_SHOWING_PLEASE_WAIT = 30000;

type
  TMutationQueryHandler = procedure(FileList: TStringList; HasBadLines: Boolean = False) of object;

  { TV_SeqDataExplorer }

  TV_SeqDataExplorer = class(TForm)
    DomainInfoSummary: TAction;
    DeveloperAction: TAction;
    ActionExportNames: TAction;
    ActionSortByTaxaGpsDlg: TAction;
    ActionSortByNameDesc: TAction;
    ActionSortByNameAsc: TAction;
    ActionSortBySimilarityToFirstSequence: TAction;
    ActionClearSiteLabels: TAction;
    ActionAutoLabelSites: TAction;
    ActionToggleUnlabeledSites: TAction;
    ActionShowDomainBoundaries: TAction;
    ActionDisplayGroupCounts: TAction;
    ActionSelectEarliestDatedSeqInEachGroup: TAction;
    ActionSelectLargestGroups: TAction;
    ActionUnselectSmallGroups: TAction;
    ActionAddSiteLabelList: TAction;
    ActionHighlightLabelledSites: TAction;
    ActionDisableIndependentSites: TAction;
    ActionAddDomainsForGroupIdentityComparisonSites: TAction;
    ActionUnselectUngrouped: TAction;
    ActionLabelSitesUsedForGroupIdentityComparison: TAction;
    ActionSelectOneFromEachGroup: TAction;
    ActionGroupSeqsByIdentity: TAction;
    ActionPreviousHighlight: TAction;
    ActionNextHighlight: TAction;
    ActionAddSpecies: TAction;
    ActionAddPopulation: TAction;
    CheckBoxes32: TImageList;
    ImageList2: TImageList;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    MenuItem100: TMenuItem;
    MenuItem103: TMenuItem;
    MenuItem104: TMenuItem;
    MenuItem105: TMenuItem;
    MenuItem106: TMenuItem;
    MenuItem107: TMenuItem;
    MenuItem108: TMenuItem;
    MenuItem109: TMenuItem;
    MenuItem110: TMenuItem;
    MenuItem111: TMenuItem;
    MenuItem112: TMenuItem;
    MenuItem113: TMenuItem;
    MenuItem114: TMenuItem;
    MenuItem115: TMenuItem;
    HorizScrollbar: TScrollBar;
    DeveloperMenu: TMenuItem;
    MenuItem116: TMenuItem;
    MenuItem117: TMenuItem;
    MenuItem125: TMenuItem;
    MoveToTopItem: TMenuItem;
    MoveToBottomItem: TMenuItem;
    MenuItem118: TMenuItem;
    MenuItem119: TMenuItem;
    MenuItem120: TMenuItem;
    MenuItem121: TMenuItem;
    MenuItem122: TMenuItem;
    MenuItem123: TMenuItem;
    MenuItem124: TMenuItem;
    Separator1: TMenuItem;
    NamesColumnPopupMenu: TPopupMenu;
    WindowsMenuItem: TMenuItem;
    VertScrollbar: TScrollBar;
    SetGroupTagItem: TMenuItem;
    GroupItem: TMenuItem;
    GroupItem2: TMenuItem;
    SpeciesItem2: TMenuItem;
    PopulationItem2: TMenuItem;
    ContinentItem2: TMenuItem;
    CountryItem2: TMenuItem;
    CityItem2: TMenuItem;
    YearItem2: TMenuItem;
    MonthItem2: TMenuItem;
    DayItem2: TMenuItem;
    TimeItem2: TMenuItem;
    SetGroupTagPopup: TPopupMenu;
    SpeciesItem: TMenuItem;
    PopulationItem: TMenuItem;
    ContinentItem: TMenuItem;
    CountryItem: TMenuItem;
    CityItem: TMenuItem;
    SetGroupTagButton: TToolButton;
    YearItem: TMenuItem;
    MonthItem: TMenuItem;
    DayItem: TMenuItem;
    TimeItem: TMenuItem;
    N4: TMenuItem;
    MenuItem39: TMenuItem;
    N3: TMenuItem;
    N2: TMenuItem;
    MenuItem22: TMenuItem;
    MotifCountLabel: TLabel;
    N1: TMenuItem;
    MenuItem101: TMenuItem;
    MenuItem102: TMenuItem;
    MenuItem73: TMenuItem;
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
    MenuItem89: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem91: TMenuItem;
    MenuItem92: TMenuItem;
    MenuItem93: TMenuItem;
    MenuItem94: TMenuItem;
    MenuItem95: TMenuItem;
    MenuItem96: TMenuItem;
    MenuItem97: TMenuItem;
    MenuItem98: TMenuItem;
    MenuItem99: TMenuItem;
    BottomPanel: TPanel;
    EditLabelsPanel: TPanel;
    ShowHelpAction: TAction;
    ActionEditDomain: TAction;
    ActionDeleteDomain: TAction;
    ActionDeleteGene: TAction;
    ActionAddDomain: TAction;
    ActionAddGene: TAction;
    ActionStatDispInOds: TAction;
    ActionStatDispInXls: TAction;
    ActionFindPrevSingleMotif: TAction;
    ActionFindNextSingleMotif: TAction;
    ActionFindNextGrp: TAction;
    ActionFindPrevGrp: TAction;
    ActionFindSeq: TAction;
    ActionHideFind: TAction;
    ActionShowFind: TAction;
    ActionFindNext: TAction;
    ActionFindPrevious: TAction;
    ActionStatDispInText: TAction;
    ActionHomology: TAction;
    ActionStatDispInXl: TAction;
    ActionStatDispInCsv: TAction;
    ActionNucComposition: TAction;
    ActionDirectionalPairs: TAction;
    ActionUndirectionalPairs: TAction;
    ActionCodonUsage: TAction;
    ActionAminoAcidComposition: TAction;
    ActionMismatchDistribution: TAction;
    ActionAllSelectedSites: TAction;
    ActionHighlightedSites: TAction;
    ActionHighlightConserved: TAction;
    ActionHighlightVariableSites: TAction;
    ActionHighlightParsimInfoSites: TAction;
    ActionHighlightSingletonSites: TAction;
    ActionHighlight0FoldDegenSites: TAction;
    ActionHighlight2FoldDegenSites: TAction;
    ActionHighlight4FoldDegenSites: TAction;
    ActionCoverage: TAction;
    ActionCpG: TAction;
    ActionAddGroup: TAction;
    ActionFindSeqName: TAction;
    ActionFindNextSeq: TAction;
    ActionFindMotif: TAction;
    ActionFindNextMotif: TAction;
    ActionFindPrevMotif: TAction;
    ActionHideMotifSearchResults: TAction;
    ActionFindPrevSeq: TAction;
    ActionChangeFont: TAction;
    ActionDispBirdsEyeView: TAction;
    ActionDispGroupNames: TAction;
    ActionDispTaxaNames: TAction;
    ActionRestoreInputOrder: TAction;
    ActionSortSequences: TAction;
    ActionSelectHighlightColor: TAction;
    ActionColorCells: TAction;
    ActionUseIdenticalSymbol: TAction;
    ActionShowOnlySelected: TAction;
    ActionQuit: TAction;
    ActionEditGroups: TAction;
    ActionEditGenesDomains: TAction;
    ActionSelectCodeTable: TAction;
    ActionTranslate: TAction;
    ActionExportSearchResults: TAction;
    ActionExportData: TAction;
    ActionSaveSession: TAction;
    ActionList1: TActionList;
    ColorDialog1: TColorDialog;
    ColumnActions: TActionList;
    FontDialog1: TFontDialog;
    ImageList1: TImageList;
    MenuItem24: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    searchTypeCbx: TComboBox;
    SearchBox: TComboBox;
    DataGrid: TDrawGrid;
    EditGroup: TPopupMenu;
    EditSpecies: TPopupMenu;
    EditPopulation: TPopupMenu;
    Column0PopupMenu: TPopupMenu;
    DataMenu: TMenuItem;
    DisplayMenu: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem23: TMenuItem;
    DispSortBySeqNameItem: TMenuItem;
    DispSortByGpItem: TMenuItem;
    ByGpAndSeqNamesItem: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    AsperTaxaGroupOrganizerItem: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem4: TMenuItem;
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
    MenuItem5: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MainSearchPanel: TPanel;
    CloseSearchPanel: TSpeedButton;
    FindPrevBtn: TSpeedButton;
    FindNextBtn: TSpeedButton;
    SearchBtn: TSpeedButton;
    DiagnoseButton: TToolButton;
    ConservedBtn: TToolButton;
    ParsimInfoBtn: TToolButton;
    SingletonBtn: TToolButton;
    FourFoldBtn: TToolButton;
    SpecialButton: TToolButton;
    SiteNumSpinEdit: TSpinEdit;
    LabelledSitesBtn: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    TranslateBtn: TToolButton;
    TwoFoldBtn: TToolButton;
    ZeroFoldBtn: TToolButton;
    VariableBtn: TToolButton;
    StatNucPairFreqSubmenu: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem9: TMenuItem;
    SearchMenu: TMenuItem;
    GroupsMenu: TMenuItem;
    HighlightSitesMenu: TMenuItem;
    StatisticsMenu: TMenuItem;
    HelpMenu: TMenuItem;
    MenuItem8: TMenuItem;
    bird: TStringGrid;
    HintTimer: TTimer;
    HintRelease: TTimer;
    SpecialMenu: TPopupMenu;
    AddEditGeneDomains: TPopupMenu;
    DiagnoseMenu: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SeqDataViewerMenu: TMainMenu;
    MainToolbar: TToolBar;
    StatusBar: TStatusBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton2: TToolButton;
    ExportMegaTBtn: TToolButton;
    ExportXLTBtn: TToolButton;
    ExportCSVTBtn: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure ActionAddDomainExecute(Sender: TObject);
    procedure ActionAddDomainsForGroupIdentityComparisonSitesExecute(
      Sender: TObject);
    procedure ActionAddGeneExecute(Sender: TObject);
    procedure ActionAddGroupExecute(Sender: TObject);
    procedure ActionAddPopulationExecute(Sender: TObject);
    procedure ActionAddSiteLabelListExecute(Sender: TObject);
    procedure ActionAddSpeciesExecute(Sender: TObject);
    procedure ActionAllSelectedSitesExecute(Sender: TObject);
    procedure ActionAminoAcidCompositionExecute(Sender: TObject);
    procedure ActionAutoLabelSitesExecute(Sender: TObject);
    procedure ActionChangeFontExecute(Sender: TObject);
    procedure ActionClearSiteLabelsExecute(Sender: TObject);
    procedure ActionCodonUsageExecute(Sender: TObject);
    procedure ActionColorCellsExecute(Sender: TObject);
    procedure ActionCoverageExecute(Sender: TObject);
    procedure ActionCpGExecute(Sender: TObject);
    procedure ActionDeleteDomainExecute(Sender: TObject);
    procedure ActionDeleteGeneExecute(Sender: TObject);
    procedure ActionDirectionalPairsExecute(Sender: TObject);
    procedure ActionDisableIndependentSitesExecute(Sender: TObject);
    procedure ActionDispBirdsEyeViewExecute(Sender: TObject);
    procedure ActionDispGroupNamesExecute(Sender: TObject);
    procedure ActionDisplayGroupCountsExecute(Sender: TObject);
    procedure ActionDispTaxaNamesExecute(Sender: TObject);
    procedure ActionEditDomainExecute(Sender: TObject);
    procedure ActionEditGenesDomainsExecute(Sender: TObject);
    procedure ActionEditGroupsExecute(Sender: TObject);
    procedure ActionExportDataExecute(Sender: TObject);
    procedure ActionExportNamesExecute(Sender: TObject);
    procedure ActionExportSearchResultsExecute(Sender: TObject);
    procedure ActionExportSearchResultsUpdate(Sender: TObject);
    procedure ActionFindMotifExecute(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure ActionFindNextGrpExecute(Sender: TObject);
    procedure ActionFindNextMotifExecute(Sender: TObject);
    procedure ActionFindNextSeqExecute(Sender: TObject);
    procedure ActionFindNextSingleMotifExecute(Sender: TObject);
    procedure ActionFindPrevGrpExecute(Sender: TObject);
    procedure ActionFindPreviousExecute(Sender: TObject);
    procedure ActionFindPrevMotifExecute(Sender: TObject);
    procedure ActionFindPrevSeqExecute(Sender: TObject);
    procedure ActionFindPrevSingleMotifExecute(Sender: TObject);
    procedure ActionFindSeqExecute(Sender: TObject);
    procedure ActionFindSeqNameExecute(Sender: TObject);
    procedure ActionHideFindExecute(Sender: TObject);
    procedure ActionHideMotifSearchResultsExecute(Sender: TObject);
    procedure ActionHighlight0FoldDegenSitesExecute(Sender: TObject);
    procedure ActionHighlight2FoldDegenSitesExecute(Sender: TObject);
    procedure ActionHighlight4FoldDegenSitesExecute(Sender: TObject);
    procedure ActionHighlightConservedExecute(Sender: TObject);
    procedure ActionHighlightedSitesExecute(Sender: TObject);
    procedure ActionHighlightLabelledSitesExecute(Sender: TObject);
    procedure ActionHighlightParsimInfoSitesExecute(Sender: TObject);
    procedure ActionHighlightSingletonSitesExecute(Sender: TObject);
    procedure ActionHighlightVariableSitesExecute(Sender: TObject);
    procedure ActionHomologyExecute(Sender: TObject);
    procedure ActionLabelSitesUsedForGroupIdentityComparisonExecute(
      Sender: TObject);
    procedure ActionMismatchDistributionExecute(Sender: TObject);
    procedure ActionNextHighlightExecute(Sender: TObject);
    procedure ActionNucCompositionExecute(Sender: TObject);
    procedure ActionPreviousHighlightExecute(Sender: TObject);
    procedure ActionQuitExecute(Sender: TObject);
    procedure ActionRestoreInputOrderExecute(Sender: TObject);
    procedure ActionSaveSessionExecute(Sender: TObject);
    procedure ActionSelectCodeTableExecute(Sender: TObject);
    procedure ActionSelectEarliestDatedSeqInEachGroupExecute(Sender: TObject);
    procedure ActionSelectHighlightColorExecute(Sender: TObject);
    procedure ActionSelectLargestGroupsExecute(Sender: TObject);
    procedure ActionSelectOneFromEachGroupExecute(Sender: TObject);
    procedure ActionGroupSeqsByIdentityExecute(Sender: TObject);
    procedure ActionShowDomainBoundariesExecute(Sender: TObject);
    procedure ActionShowFindExecute(Sender: TObject);
    procedure ActionShowOnlySelectedExecute(Sender: TObject);
    procedure ActionSortByNameAscExecute(Sender: TObject);
    procedure ActionSortByNameDescExecute(Sender: TObject);
    procedure ActionSortBySimilarityToFirstSequenceExecute(Sender: TObject);
    procedure ActionSortByTaxaGpsDlgExecute(Sender: TObject);
    procedure ActionSortSequencesExecute(Sender: TObject);
    procedure ActionStatDispInCsvExecute(Sender: TObject);
    procedure ActionStatDispInOdsExecute(Sender: TObject);
    procedure ActionStatDispInTextExecute(Sender: TObject);
    procedure ActionStatDispInXlExecute(Sender: TObject);
    procedure ActionStatDispInXlsExecute(Sender: TObject);
    procedure ActionToggleUnlabeledSitesExecute(Sender: TObject);
    procedure ActionTranslateExecute(Sender: TObject);
    procedure ActionUndirectionalPairsExecute(Sender: TObject);
    procedure ActionUnselectSmallGroupsExecute(Sender: TObject);
    procedure ActionUnselectUngroupedExecute(Sender: TObject);
    procedure ActionUseIdenticalSymbolExecute(Sender: TObject);
    procedure AddEditGeneDomainsPopup(Sender: TObject);
    procedure AsperTaxaGroupOrganizerItemClick(Sender: TObject);
    procedure birdClick(Sender: TObject);
    procedure birdDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure birdMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure birdMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure birdMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ByGpAndSeqNamesItemClick(Sender: TObject);
    procedure CloseSearchPanelClick(Sender: TObject);
    procedure DataGridClick(Sender: TObject);
    procedure DataGridColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure DataGridContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure DataGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DataGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure DataGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure DataGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DataGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DataGridMouseEnter(Sender: TObject);
    procedure DataGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DataGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DataGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure DataGridTopleftChanged(Sender: TObject);
    procedure DeveloperActionExecute(Sender: TObject);
    procedure DiagnoseButtonClick(Sender: TObject);
    procedure DiagnoseMenuPopup(Sender: TObject);
    procedure DispSortByGpItemClick(Sender: TObject);
    procedure DispSortBySeqNameItemClick(Sender: TObject);
    procedure DomainInfoSummaryExecute(Sender: TObject);
    procedure EditGroupDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure EditGroupPopup(Sender: TObject);
    procedure ActiveGroupClick(Sender: TObject);  // from Groups main menu selecting which groups are active
    procedure EditGroupClick(Sender: TObject); // from Groups main menu editing a group name
    procedure DeleteGroupClick(Sender: TObject); // from Groups main menu deleting a group name
    procedure ChangeGroupClick(Sender: TObject);  // For right click popup menu
    procedure ChangeSpeciesClick(Sender: TObject);  // For right click popup menu
    procedure ChangePopulationClick(Sender: TObject);
    procedure EditPopulationDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure EditPopulationPopup(Sender: TObject);
    procedure EditSpeciesDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure EditSpeciesPopup(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    //procedure ActionEditGeneExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormMouseEnter(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HighlightSitesClick(Sender: TObject);
    procedure HintReleaseTimer(Sender: TObject);
    procedure HintTimerTimer(Sender: TObject);
    procedure HorizScrollbarChange(Sender: TObject);
    procedure HorizScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure MoveToTopItemClick(Sender: TObject);
    procedure MoveToBottomItemClick(Sender: TObject);
    procedure SearchBoxChange(Sender: TObject);
    procedure SearchBoxEnter(Sender: TObject);
    procedure SearchBtnClick(Sender: TObject);
    function SearchNextSingleMotif(SearchingMotif: String; StartAtRow: Integer; StartAtCol: Integer): Boolean;
    procedure searchTypeCbxChange(Sender: TObject);
    procedure ShowHelpActionExecute(Sender: TObject);
    procedure SiteNumSpinEditChange(Sender: TObject);
    procedure SiteNumSpinEditEnter(Sender: TObject);
    procedure SiteNumSpinEditExit(Sender: TObject);
    procedure SpecialButtonClick(Sender: TObject);
    procedure SetGroupItemClick(Sender: TObject);
    procedure VertScrollbarChange(Sender: TObject);
    procedure VertScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
  private
    FDisallowRowMoving: Boolean;
    FMouseGridCoords: TPoint;
    FHScrollPos: Integer;
    FVScrollPos: Integer;
    FSelfScrolling: Boolean;
    FScrollStarted: Boolean;
    FGroupCounts: TFPHashList;
    FEditLabelsFrame: TEditSiteLabelsFrame;
    FDnaSelectedSite: Int64;
    FUpdatingGroupTagMenus: Boolean;
    FUpdatingSiteIndex: Boolean;
    FUpdatingHighlightState: Boolean;
    FCurrentHighlightedSite: Int64;
    FForcingHighlightUpdate: Boolean;
    FRuntimeProg: TRuntimeProgress;
    FGeneDomainsPropsEditor: TGenesDomainsPropertiesEditor;
    FIsClosing: Boolean; { FormResize is being called after FormDestroy, in which case we want to avoid accessing freed objects}
    FEmptyInfos: TList; { keep references to empty domain info objects so they can be freed on form destroy}
    rightMouseDownDataGrid: Boolean;
    editDomainAt, tempDomainStart: Integer;
    FUpdatingFixedColWidths: Boolean;
    FLastRow: Integer;
    CurDesc: String;
    CurHint: THintWindow;
    SeqNameResult: String; // Search result to be bolded when it is searched for.
    DNASearchHistory: String;
    ProteinSearchHistory: String;

    // This 'down in box' is for drag/drop of the gene/domain over view called 'bird'
    downInBox: Boolean;
    offsetFromCenter: Integer;
    procedure EnableDisableGroupTagItems;
    function ValidSelectableCol(aCol: Integer): Integer;
    function ValidSelectableRow(aRow: Integer): Integer;
    function ColumnVisible(aCol: Integer): Boolean;
    function RowVisible(aRow: Integer): Boolean;
    function GetNumSeqs: Integer;
    function GetNumSites: Integer;
    procedure InitScrollBars(setPosition: Boolean);
    procedure ParseTipDatesThreadDone(aThread: TObject);
    procedure SelectEarliestDatedSequenceInEachGroup;
    procedure ClearGroupCountsList;
    procedure ResetGroupCounts;
    procedure DisableActionConflictingWithNumberKeys;
    procedure EnableActionsConflictingWithNumberKeys;
    procedure DisableActionsConflictingWithLetterKeys;
    procedure EnabledActionsConflictingWithLetterKeys;
    procedure UpdateCurStatisticsStrings(aCol, aRow: Integer);
    procedure CloseEditLabelsPanel(Sender: TObject);
    procedure ApplySiteLabelsEdit(Sender: TObject);
    procedure ToggleHighlightLabelledSites(Sender: TObject);
    procedure LabelledSitesEditOnEnter(Sender: TObject);
    procedure LabelledSitesEditOnExit(Sender: TObject);
    procedure AlleleFreqSearchDone(Thread: TObject);
    procedure UpdateMotifCountLabel(aMotif: String; aCount: Integer);
    procedure CountMotifsDone(Thread: TObject);
    procedure LaunchCountMotifsThread(aMotif: String);
    procedure DrawCurrentColumnBorder(aRect: TRect; topBorder: Boolean; bottomBorder: Boolean);
    procedure AutoHighlightSites(Sender: TObject);
    function NumberOfFixedColsVisible: Integer;
    function IsCodonByCodonWriting(remove1st, remove2nd, remove3rd, exportAllSites: Boolean): Boolean;
    procedure LaunchDataExportThread(ExportOptions: TSeqExportOptions; aDestination: String; Results: TStringList);
    procedure DataExportThreadDone(aThread: TObject);
    procedure SetSelectedStatsExportType(AValue: TExportType);
    procedure UpdateMarksForDomain(aInfo: TDomainInfo);
    function UpdateDomainCoordinates(aInfo: TDomainInfo; fromSite: Integer; toSite: Integer): Boolean;
    function UpdateGeneCoordinates(aInfo: TDomainInfo; fromSite: Integer; toSite: Integer): Boolean;
    function GetMaxCharLen: LongInt;
    function rowInHighlightRange(ARow: Integer): Boolean;
    function GetSelectedStatsExportType: TExportType;
    procedure AssignHelpContext;
    procedure drawBracket(site: Integer; aCanvas: TCanvas);
    function SiteInSearchBox(seqindex, siteindex: integer):boolean;
    function MatchSearchBox(box: String):boolean;
    function GetFileExportDestination(const aExportType: TMegaExportFormat; var aDestination: String): Boolean;
    function GetFileExtensionForExportType(const aExportType: TMegaExportFormat): String;
    function IsShowingAminoAcidData: Boolean;
    function GetSearchResult(Row, Col: Integer): TSearchResult;
    procedure SetBrushColor(x: AnsiChar);
    procedure updateBirdsGeneDomain;
    procedure focusBirdWindowOn(point: Integer);
    procedure UncheckSpecial;
    procedure UncheckSpecific;
    procedure ResetSearchBtns;
    procedure ShowTaxaSearch;
    procedure ShowMotifSearch;
    procedure SearchGroup;
    procedure SearchMotif;
    procedure UpdateHighlightButtonStates;
    procedure ClearHighlightButtonStates;
    procedure InitMainMenu;
    procedure InitPopupMenus;
    function NumSelectedTaxaString: String;
    function NumSelectedGroupsString: String;

    { private declarations }
  public
    // NMID used for diagnosis (calling EvoD).
    EvodNMID: AnsiString;
    EvoDPeptideId: AnsiString;
    MutationQueryHandler: TMutationQueryHandler;
    CheckMark      : TBitmap;
    MenuItemCheckmark: TBitmap;
    UnCheckMark    : TBitmap;
    GeneIcon       : TBitmap;
    DomainIcon     : TBitmap;
    DispChar: array [0..1] of AnsiChar; // used to drawing display strings

    FHasDataSubsetChanged : Boolean;
    FHasDataViewChanged   : Boolean;
    Cols: TColList;
    function GroupTagCheckedIndex: Integer;
    procedure UncheckGroupTagMenus;
    procedure UpdateGroupTagMenuCheckedItem(aIndex: Integer);
    function SelectedStatsMegaExportFormat: TMegaExportFormat;
    procedure UpdateStatusBar(Sender: TObject);
    procedure SetDiagnoseSite(Site: Integer);
    procedure UpdateDiagnoseBtnCaption(Site: Integer);
    procedure CustomColsChanged;
    procedure UpdateFixedColWidth;
    procedure UpdateRowColumn(Row, Col: Integer);
    procedure UpdateGridSizes;
    procedure UpdateMaxGpNamePixels;
    procedure UpdateMaxTaxaNamePixels;
    procedure UpdateMaxSpNamePixels;
    procedure UpdateMaxPopNamePixels;
    procedure NotifySearchCompletion(NumOfRes: Integer);
    procedure JumpTo(aTop, aLeft: Integer);
    procedure AdjustEnabledStatus;
  published
    //procedure AssignHelpContext;
    //procedure DataWriteToFileItemClick(Sender: TObject);
    //
    //procedure DataGridRowMoved(Sender: TObject; FromIndex, ToIndex: Longint);
    //procedure DataGridMouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    //procedure DataGridDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
    //
    //procedure DataSelectEditGenesDomainsItemClick(Sender: TObject);
    //procedure DataSelectEditTaxaGroupsItemClick(Sender: TObject);
    //
    //procedure HighlightSitesClick(Sender: TObject);
    //procedure IdenticalSBtnClick(Sender: TObject);
    //procedure DataTranslateSBtnClick(Sender: TObject);
    //procedure DataSelectCodeTableItemClick(Sender: TObject);
    //procedure DataQuitViewerItemClick(Sender: TObject);
    //
    //procedure DispSelTaxaItemClick(Sender: TObject); // handles both things
    //procedure DispAsInInputFileItemClick(Sender: TObject);
    //procedure DispChangeFontItemClick(Sender: TObject);
    //
    //
    //procedure aClick(Sender: TObject);
    //procedure bClick(Sender: TObject);
    //procedure StatAllSelSitesItemClick(Sender: TObject);
    //procedure StatUseOnlyMarkedSitesItemClick(Sender: TObject);
    //procedure PopupGoToSiteItemClick(Sender: TObject);
    //procedure PopupGoToDomainItemClick(Sender: TObject);
    //procedure PopupGoToGeneItemClick(Sender: TObject);
    //procedure PopupGoToNexthighlightedSiteItemClick(Sender: TObject);
    //procedure DispColorItemClick(Sender: TObject);
    //procedure DispSortByInputOrderItemClick(Sender: TObject);
    //procedure DispSortBySeqNameItemClick(Sender: TObject);
    //procedure DispSortByGpNameItemClick(Sender: TObject);
    //procedure ByGpAndSeqNamesItemClick(Sender: TObject);
    //procedure AsperTaxaGroupOrganizerItemClick(Sender: TObject);
    //procedure HelpMenuClick(Sender: TObject);
    //procedure StatPercentHomologyItemClick(Sender: TObject);
    //
    //procedure FormActivate(Sender: TObject);
    //procedure FormClose(Sender: TObject; var Action: TCloseAction);
    //procedure FormCreate(Sender: TObject);
    //procedure FormDestroy(Sender: TObject);
    //
    //procedure miDotPlotClick(Sender: TObject);
    //
    //procedure ActionAddGroupExecute(Sender: TObject);
    //procedure ActionDelGroupExecute(Sender: TObject);
    //procedure ActionListGpMembersExecute(Sender: TObject);
    //procedure ActionIncludeGroupExecute(Sender: TObject);
    //procedure ActionExcludeGroupExecute(Sender: TObject);
    //procedure ActionRenameGpExecute(Sender: TObject);
    //procedure ActionSortByGroupNameExecute(Sender: TObject);
    //procedure ActionExportGroupInformationExecute(Sender: TObject);
    //procedure ActionImportGroupInformationExecute(Sender: TObject);
    //procedure ListofGroupNamesPopupPopup(Sender: TObject);
    //procedure ActionFindSeqExecute(Sender: TObject);
    //procedure DataGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    //procedure DispSelectColorItemClick(Sender: TObject);
    //procedure DataGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    //procedure StatDispInXLItemClick(Sender: TObject);
    //procedure StatDispInCSVItemClick(Sender: TObject);
    //procedure StatDispInTextItemClick(Sender: TObject);

  protected // a lot of sundary items
    procedure WriteGeneDomainOnStatusBar(ADomainInfo: TDomainInfo);
    procedure UpdateDomainInfos;
  public // properties based on above variables; they are all set first by outside
    procedure InitGeneDomainsPropertyEditor;
    procedure Initialize;
    procedure ApplyUpdate;
    property HasDataSubsetChanged: Boolean read FHasDataSubsetChanged;
    property HasDataViewChanged:   Boolean read FHasDataViewChanged;
    property SelectedStatsExportType: TExportType read GetSelectedStatsExportType write SetSelectedStatsExportType;
    property NumSeqs: Integer read GetNumSeqs;
    property NumSites: Integer read GetNumSites;
  end;

var
  V_SeqDataExplorer: TV_SeqDataExplorer;

implementation

  {$R *.lfm}

uses
  mseqdataexportoptionsdlg, LCLIntf, MD_InputSeqData, MVS_SeqDataExplorer, math, DataExplorerHelp_HC,
  mcolorcodes, MegaVerConsts, MMegaWindowInfo, MPleaseWait, MegaUtils, mega_main,
  ExcelWrite, MEditorForm, LazFileUtils , MSelectGeneticCodeDlg,
  MTaxaGpsDlg, MLongintList, MGeneDomainDlg, mhelpfiles, mcrossprojectutils,
  MegaPrivateFiles, MutationExplorer,
  mdrawmenuitems, MOtuInfo, mseqdataexport, mshortcutshelper, MSeqDataSearchThreads,
  mallele_frequency_search, msequence_identity_dlg, mtipdatesfinder, MWriteOutputDlg;



{ TV_SeqDataExplorer }

procedure TV_SeqDataExplorer.ActionTranslateExecute(Sender: TObject);
var
  NewColPos: LongInt;
  gridOptions: TGridOptions;

  function FindAACol(CodonSite: LongInt): LongInt;  // Find amino acid pos of a col
  var
    i: Integer;
  begin
    Result := 0;
    for i:=0 to D_InputSeqData.AACodonStarts.Count-1 do
      if (D_InputSeqData.AACodonStarts[i] = CodonSite) or (D_InputSeqData.AACodonPos2[i] = CodonSite) or (D_InputSeqData.AACodonPos3[i] = CodonSite) then
      begin
         Result := i;
         Exit;
      end
      else if D_InputSeqData.AACodonPos3[i] >= CodonSite then
      begin
         Result := i-1;
         if Result < 0 then Result := 0;
         Exit;
      end;
  end;
begin
  gridOptions := DataGrid.Options;
  D_InputSeqData.ClearComputedStatsCache;
  ActionTranslate.Checked := (not ActionTranslate.Checked);
  if ActionTranslate.Checked  or (not VS_SeqDataExplorer.IsTranslated) then
  begin
    FDnaSelectedSite := DataGrid.Col - DataGrid.FixedCols;
    D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateAASeq];
    if VS_SeqDataExplorer.CurAttrDisp = megLabelled then
      VS_SeqDataExplorer.CurAttrDisp := megNone;
    if not D_InputSeqData.DoTranslation then
    begin
      ActionTranslate.Checked := False;
      VS_SeqDataExplorer.IsTranslated := False;
      Exit;
    end;
    Include(gridOptions, goCellHints);
    ActionTranslate.ImageIndex := 6;
    ActionTranslate.Hint := 'Display nucleotides';
    NewColPos := FindAACol(DataGrid.Col - DataGrid.FixedCols);
    DataGrid.ColCount := D_InputSeqData.FNoOfSites + Cols.VisibleCount;  // <- changes column pos
    DataGrid.Col := NewColPos + DataGrid.FixedCols;
    D_InputSeqData.CurDomainInfo := D_InputSeqData.FDomainMarks[NewColPos]; // added in Mega2.1
    DNASearchHistory := SearchBox.Items.Text;
    SearchBox.Items.Text := ProteinSearchHistory;
    if SearchBox.Items.Count > 0 then
    begin
      SearchBox.Text := SearchBox.Items.Strings[0];
      if SearchBox.Text <> EmptyStr then
        VS_SeqDataExplorer.CurMotifStr := UpperCase(SearchBox.Text);
      SearchBoxChange(nil);
    end
    else
      SearchBox.Text := 'Search for a Motif';
  end
  else
  begin
    Exclude(gridOptions, goCellHints);
    ActionTranslate.ImageIndex := 7;
    ActionTranslate.Hint := 'Translate Sequences';
    D_InputSeqData.DoUnTranslation;
    D_InputSeqData.FNoOfSites := D_InputSeqData.NoOfNucSites;
    NewColPos  := D_InputSeqData.AACodonStarts[Max(0, DataGrid.Col-Cols.VisibleCount)]+Cols.VisibleCount;  // what codon was at the given site
    DataGrid.ColCount := D_InputSeqData.FNoOfSites + Cols.VisibleCount;
    DataGrid.Col := FDnaSelectedSite + DataGrid.FixedCols;
    D_InputSeqData.CurDomainInfo := D_InputSeqData.FDomainMarks[NewColPos-1]; // added in Mega2.1
    ProteinSearchHistory := SearchBox.Items.Text;
    SearchBox.Items.Text := DNASearchHistory;
    if SearchBox.Items.Count > 0 then
    begin
      SearchBox.Text := SearchBox.Items.Strings[0];
      if SearchBox.Text <> EmptyStr then
        VS_SeqDataExplorer.CurMotifStr := UpperCase(SearchBox.Text);
      SearchBoxChange(nil);
    end
    else
      SearchBox.Text := 'Search for a Motif';
  end;
  VS_SeqDataExplorer.CurAttrDisp := megNone;
  SiteNumSpinEdit.Value := DataGrid.Col - NumberOfFixedColsVisible + 1;
  VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stCurSite] := '/' + IntToStr(D_InputSeqData.FNoOfSites);
  VS_SeqDataExplorer.CurAttrDisp := megNone;
  VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites] := 'Highlighted: None';
  VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stDomainName] := EmptyStr;
  AdjustEnabledStatus;
  if (DataGrid.Col > 0) and (DataGrid.Row > 0) then
    UpdateCurStatisticsStrings(DataGrid.Col, DataGrid.Row);
  DataGrid.Invalidate;
  if Active then
    DataGrid.SetFocus;
end;

procedure TV_SeqDataExplorer.ActionUndirectionalPairsExecute(Sender: TObject);
var
  SaveLocation: AnsiString = '';
  exportAs: TExportType;
begin
  try
    SaveLocation := MegaForm.DataFileNameOnly + '-Undirectional-Pair-Freqs.txt';
    exportAs := SelectedStatsExportType;
    exportAs := PromptUserWriteOutput(SaveLocation, True, exportAs);
    MegaForm.LaunchStatPairFreqsThread(SaveLocation, exportAs, False);
  except
    on E: Exception do
      ShowMessage('Application Error: Unable to perform statistical analysis - ' + E.Message);
  end;
end;

procedure TV_SeqDataExplorer.ActionUnselectSmallGroupsExecute(Sender: TObject);
var
  response: Boolean = False;
  groupSizeStr: String = '';
  groupSize: Integer;
  gc: TGroupCount = nil;
  i: Integer;
  index: Integer;
  otu: TOtuInfo = nil;
begin
  Repeat
    response := InputQuery('Group Size', 'Minimum Number of Taxa', groupSizeStr);
    if response = False then
      Exit;
  until ( not response) or (TryStrToInt(groupSizeStr, groupSize) and (groupSize >= 1));
  if response then
  begin
    try
      for i := 0 to D_InputSeqData.NoOfTaxa - 1 do
      begin
        otu := D_InputSeqData.OtuInfos[i];
        index := FGroupCounts.FindIndexOf(otu.GpName);
        if index >= 0 then
        begin
          gc := FGroupCounts[index];
          if gc.Count < groupSize then
            otu.IsUsed := False;
        end;
      end;
    finally
      D_InputSeqData.UpdateDispTaxaList;
      UpdateStatusBar(Sender);
      DataGrid.Invalidate;
    end;
  end;
end;

procedure TV_SeqDataExplorer.ActionUnselectUngroupedExecute(Sender: TObject);
var
  i: Integer;
  otuInfo: TOtuInfo = nil;
begin
  if D_InputSeqData.NoOfTaxa > 0 then
  begin
    for i := 0 to D_InputSeqData.NoOfTaxa - 1 do
    begin
      otuInfo := D_InputSeqData.OtuInfos[i];
      if Trim(otuInfo.GpName) = EmptyStr then
        otuInfo.IsUsed := False;
    end;
    if D_InputSeqData.OtuInfos.NoOfSelOtus = 0 then
      D_InputSeqData.OtuInfos[0].IsUsed := True;
  end;
  DataGrid.Invalidate;
  D_InputSeqData.UpdateDispTaxaList;
  UpdateStatusBar(Sender);
end;

procedure TV_SeqDataExplorer.ActionUseIdenticalSymbolExecute(Sender: TObject);
begin
  VS_SeqDataExplorer.DispUseIdenSymbolItem := (not VS_SeqDataExplorer.DispUseIdenSymbolItem);
  ActionUseIdenticalSymbol.Checked := VS_SeqDataExplorer.DispUseIdenSymbolItem;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.AddEditGeneDomainsPopup(Sender: TObject);
var
  mark: TDomainInfo;
  cursor: TPoint;
  ACol: Integer = -1;
  ARow: Integer = -1;
begin
  cursor := TPoint.Create(0, 0);
  GetCursorPos(cursor);
  cursor := Datagrid.ScreenToClient(cursor);
  Datagrid.MouseToCell(cursor.X, cursor.Y, ACol, ARow);
  if (ACol-Cols.VisibleCount) <= 0 then
    Exit;
  mark := D_InputSeqData.GetDomainMark(ACol-Cols.VisibleCount);
  editDomainAt := ACol-Cols.VisibleCount;
  if mark <> nil then
  begin
    if mark.IsGene then
    begin
      ActionDeleteGene.Enabled := True;
      ActionAddGene.Enabled := False;
      ActionEditDomain.Enabled := False;
      ActionAddDomain.Enabled := True;
      ActionDeleteDomain.Enabled := False;
    end
    else if mark.IsDomain then
    begin
      ActionDeleteGene.Enabled := (mark.GeneName <> EmptyStr);
      ActionAddGene.Enabled := False;
      ActionEditDomain.Enabled := True;
      ActionAddDomain.Enabled := False;
      ActionDeleteDomain.Enabled := True;
    end
    else
    begin
      Assert(False, 'invalid domain mark');
    end;
  end
  else
  begin
    ActionDeleteGene.Enabled := False;
    ActionAddGene.Enabled := True;
    ActionEditDomain.Enabled := False;
    ActionAddDomain.Enabled := True;
    ActionDeleteDomain.Enabled := False;
  end;
end;

procedure TV_SeqDataExplorer.AsperTaxaGroupOrganizerItemClick(Sender: TObject);
begin
  ActionSortSequencesExecute(Sender);
end;

procedure TV_SeqDataExplorer.birdClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt := TPoint.Create(0, 0); { to suppress compiler warning about not being initialized}
  getCursorPos(pt);
  pt := bird.ScreenToClient(pt);
  focusBirdWindowOn(pt.X);
  bird.Invalidate;
  if not (SiteNumSpinEdit.Value = (DataGrid.Col - NumberOfFixedColsVisible + 1)) then
  begin
    try
      FUpdatingSiteIndex := True;
      SiteNumSpinEdit.Value := DataGrid.Col - NumberOfFixedColsVisible + 1;
    finally
      FUpdatingSiteIndex := False;
    end;
  end;
end;

procedure TV_SeqDataExplorer.birdDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  boxRect: TRect;
  cellText: AnsiString;
  dinfo: TDomainInfo = nil;
  colsFromScreenLeft, selColPos: Integer;
  XEWidth: Integer;
  vertMerge: Boolean;
begin
  if not Visible then Exit;
  with bird, canvas do
  begin
    if Assigned(Objects[ACol, 0]) and (Objects[ACol, 0] is TDomainInfo) then
      dinfo := TDomainInfo(Objects[ACol, 0]);
    if not Assigned(dinfo) then
    begin
      Brush.Color := clWhite;
      Brush.Style := bsSolid;
      FillRect(aRect);
      Exit;
    end;
    vertMerge := (dinfo <> nil) and (Trim(dinfo.GeneName) = EmptyStr);

    if (ARow = 0) and (not vertMerge) then
    begin
      Brush.Color := clSilver;
      Font.Color := clBlack;
    end
    else
    begin
      Brush.Color := clDkGray;
      Font.Color := clWhite;
    end;

    if (dinfo <> nil) and (not dinfo.IsUsed) then
      Brush.Color := clWhite;
    Brush.Style := bsSolid;
    FillRect(aRect);
    Brush.Style := bsClear;
    cellText := bird.Cells[ACol, ARow];
    if TextWidth(cellText) > bird.ColWidths[ACol] then
      cellText := '*';
    TextRect(aRect, aRect.Left + ((aRect.Right - aRect.Left) div 2) - (TextWidth(cellText) div 2), aRect.Top, cellText);

    {// Draw icon
    if ARow = 0 then
      Draw(Rect.Left + 1, Rect.Top + (Rect.Bottom - Rect.Top - GeneIcon.Height) div 2, GeneIcon)
    else
      Draw(Rect.Left + 1, Rect.Top + (Rect.Bottom - Rect.Top - DomainIcon.Height) div 2, DomainIcon);}

    //prevPColor := pen.Color;

    // Draw seperator line
    pen.color := clWhite;
    pen.width := 2;
    moveTo(aRect.right - 1, aRect.Top);
    lineTo(aRect.right - 1, aRect.Bottom);

    pen.Color := clBlue;
    pen.Width := 1;

    boxRect.Left := Floor(bird.Width * (DataGrid.LeftCol - NumberOfFixedColsVisible) / DataGrid.ColCount);
    // If the box will fall off the LEFT side, make sure it stops at 0.
    if boxRect.Left < 0 then
      boxRect.Left := 0;
    boxRect.Top := aRect.Top;
    if ARow = 1 then
      boxRect.Top := boxRect.Top - 1;
    boxRect.Bottom := aRect.Bottom;
    if ARow = 0 then
      boxRect.Bottom := boxRect.Bottom + 3
    else
      boxRect.Bottom := boxRect.Bottom - 2;
    boxRect.Right := boxRect.Left + Max(10, Floor(bird.Width * (DataGrid.VisibleColCount / DataGrid.ColCount)));
    // If the box will fall off the right side, make sure it doesn't.
    if boxRect.Right > bird.Width then
      boxRect.Left := bird.Width - boxRect.Width;
    XEWidth := Floor(bird.Width * (DataGrid.VisibleColCount / DataGrid.ColCount));
    if boxRect.Right > bird.Width then
      boxRect.Left := bird.Width - XEWidth;

    // Draw the current view box (blue)
    moveTo(boxRect.left, boxRect.top);
    // top
    if ARow = 0 then
      lineTo(boxRect.Right, boxRect.Top)
    else
      moveTo(boxRect.Right, boxRect.Top);
    //right
    lineTo(boxRect.Right, boxRect.Bottom);
    //bottom
    if ARow = 1 then
      lineTo(boxRect.Left, boxRect.Bottom)
    else
      moveTo(boxRect.Left, boxRect.Bottom);
    //left
    lineTo(boxRect.Left, boxRect.Top);
    colsFromScreenLeft := (DataGrid.Col - DataGrid.LeftCol);
    selColPos := Floor(bird.Width * colsFromScreenLeft / DataGrid.ColCount) + boxRect.Left;

    //Draw the marker denoting the currently selected column
    if ARow = 0 then
    begin
      // Top marker
      moveTo(selColPos, boxRect.Top);
      lineTo(selColPos, boxRect.Top + 5);
    end
    else
    begin
      // Bottom marker
      moveTo(selColPos, boxRect.bottom);
      lineTo(selColPos, boxRect.Bottom - 5);
    end;

    //pen.Color := prevPColor;
  end;
end;

procedure TV_SeqDataExplorer.birdMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  curCenter: Integer;
  GR : TRect;

  function getBoxRectContains(const aPoint: TPoint; var boxRect: TRect): Boolean;
  begin
    Result := False;
    if boxRect.Left < 0 then
      boxRect.Left := 0;
    if boxRect.Right > bird.Width then
      boxRect.Left := bird.Width - boxRect.Right;
    Result := ((aPoint.X >= boxRect.Left) and (aPoint.X <= boxRect.Right) and (aPoint.Y >= boxRect.Top) and (aPoint.Y <= boxRect.Bottom));
  end;

begin
  GR := Rect(Floor(bird.Width*DataGrid.LeftCol/DataGrid.ColCount), 0, Floor(bird.Width*DataGrid.VisibleColCount/DataGrid.ColCount), bird.Height);
  downInBox := getBoxRectContains(point(x, y), GR);
  curCenter := Floor(bird.Width * (DataGrid.Col - DataGrid.LeftCol) / DataGrid.ColCount) + GR.Left;
  offsetFromCenter := x - curCenter;
end;

procedure TV_SeqDataExplorer.birdMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  ACol: Integer = -1;
  ARow: Integer = -1;
  newHint: String;
begin
  try
    bird.MouseToCell(x, y, acol, arow);
    if (acol >= 0) and (acol <= bird.ColCount-1) and (arow >= 0) and (arow <= bird.RowCount-1) then
    begin
      if arow = 0 then
      begin
        if bird.Cells[ACol, ARow] <> EmptyStr then
          newHint := 'Gene: ' + bird.Cells[ACol, ARow];
        if bird.Hint <> newHint then
          Application.CancelHint;
        bird.Hint := newHint;
      end
      else
      begin
        newHint := 'Domain: ' + bird.Cells[ACol, ARow];
        if bird.Hint <> newHint then
          Application.CancelHint;
        bird.Hint := newHint;
      end;
    end;
  Except
    on E: Exception do
    begin
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  end;
  Application.ProcessMessages;
  if downInBox and (ssLeft in Shift) then
    focusBirdWindowOn(X - offsetFromCenter);
end;

procedure TV_SeqDataExplorer.birdMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  downInBox := false;
  bird.Invalidate;
end;

procedure TV_SeqDataExplorer.ByGpAndSeqNamesItemClick(Sender: TObject);
begin
  ActionSortSequencesExecute(ByGpAndSeqNamesItem);
end;

procedure TV_SeqDataExplorer.CloseSearchPanelClick(Sender: TObject);
begin
  MainSearchPanel.Visible := false;
  ActionHideFind.execute;

  // Remove the bolding and highlighting of any searched sequence
  if SeqNameResult <> EmptyStr then
  begin
    SeqNameResult := EmptyStr;
    VS_SeqDataExplorer.CurColorRowRange := Point(-1, -1);
    DataGrid.Invalidate;
  end;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.DataGridClick(Sender: TObject);
begin
  if not Assigned(bird) then
    Exit;
  if bird.Visible then
    bird.Invalidate;
end;

procedure TV_SeqDataExplorer.DataGridColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if not IsColumn  then
  begin
    if sIndex = tIndex then
      Exit;
    D_InputSeqData.UpdateSearchResults(sIndex, tIndex);
    D_InputSeqData.FOtuInfos.Move(D_InputSeqData.DispTaxaList[sIndex-1], D_InputSeqData.DispTaxaList[tIndex-1]);
    D_InputSeqData.UpdateDispTaxaList;
    D_InputSeqData.ReferenceSeq := D_InputSeqData.Sequence[0];
    DataGrid.Invalidate;
    FHasDataViewChanged   := True;
  end;
end;

procedure TV_SeqDataExplorer.DataGridContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  coords: TPoint;
begin
  coords := ClientToScreen(MousePos);
  FMouseGridCoords := DataGrid.MouseToCell(MousePos);
  if FMouseGridCoords.X = 0 then
    NamesColumnPopupMenu.PopUp(coords.X, coords.Y)
  else
  begin
    if DiagnoseButton.Visible and DiagnoseButton.Enabled then
      DiagnoseMenu.PopUp(coords.x, coords.y)
    else
      AddEditGeneDomains.Popup(coords.x, coords.y);
  end;

  Handled := True
end;

procedure TV_SeqDataExplorer.DataGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
const
  PADDING = 4;
var
  x, y: LongInt;
  ResizeLineRect: TRect;
  IsUsed: Boolean;
  Arrow : Array[0..2] of TPoint;
  relSite: Integer;
  codonBoxMargins: TRect;
  ts: TTextStyle;
  gc: TGroupCount = nil;
begin
  if not Visible then Exit;
  codonBoxMargins.Left := 2;
  codonBoxMargins.Right := 2;
  codonBoxMargins.Top := 1;
  codonBoxMargins.Bottom := 2;

  try
    with DataGrid, Canvas do
    begin
      if (ACol >= D_InputSeqData.FNoOfSites+Cols.VisibleCount) or
         (ACol < 0) or (ARow > D_InputSeqData.FNoOfTaxa) or
         (ARow > DataGrid.RowCount) then
        Exit;

      relSite := ACol-Cols.VisibleCount;
      if (ARow = 0) and (Cols.VisibleCount > 0) and (ACol < Cols.VisibleCount) then  // On Row 0 draw the column headers
      begin
        Canvas.Brush.Color := DataGrid.FixedColor;
        Canvas.FillRect(aRect);
        if ACol = 0 then // draw check all box on name col
        begin
          Canvas.TextRect(aRect, aRect.Left+1+UnCheckMark.Width +2, aRect.Top+1, Cols.VisibleCol[ACol].ColName);
          if D_InputSeqData.OtuInfos.NoOfSelOtus = D_InputSeqData.OtuInfos.NoOfOtus then
            Draw(aRect.Left + 1, aRect.Top + (aRect.Bottom - aRect.Top - CheckMark.Height) div 2, CheckMark)
          else
            Draw(aRect.Left + 1, aRect.Top + (aRect.Bottom - aRect.Top - CheckMark.Height) div 2, UnCheckMark);
        end
        else
          Canvas.TextRect(aRect, aRect.Left+PADDING, aRect.Top+1, Cols.VisibleCol[ACol].ColName);
        Canvas.Pen.Color := clBlack;
        Case Cols.VisibleCol[ACol].SortOrder of
          soAscending:
            begin
              Arrow[0] := Point(aRect.Right-14, aRect.Bottom -6);
              Arrow[1] := Point(aRect.Right-6, aRect.Bottom -6);
              Arrow[2] := Point(aRect.Right-10, aRect.Top +6);
              Canvas.Polygon(Arrow);
            end;
          soDescending:
            begin
              Arrow[0] := Point(aRect.Right-14, aRect.Top +6);
              Arrow[1] := Point(aRect.Right-6, aRect.Top +6);
              Arrow[2] := Point(aRect.Right-10, aRect.Bottom -6);
              Canvas.Polygon(Arrow);
            end;
        end;
        if Cols.VisibleCol[ACol].isDown then
          Canvas.Pen.Color := clSilver
        else
          Canvas.Pen.Color := clWhite;
        Canvas.Pen.Width := 2;
        Canvas.MoveTo(aRect.Left+1, aRect.Top);
        Canvas.LineTo(aRect.Right-1, aRect.Top); // Top Line
        Canvas.Pen.Width := 1;
        Canvas.MoveTo(aRect.Left, aRect.Top);
        Canvas.LineTo(aRect.Left, aRect.Bottom); // Left Line

        if not Cols.VisibleCol[ACol].isDown then
        begin
          Canvas.Pen.Color := clSilver;
          Canvas.Pen.Width := 2;
          Canvas.MoveTo(aRect.Left, aRect.Bottom-1);  // Bottom line
          Canvas.LineTo(aRect.Right-1, aRect.Bottom-1);
          Canvas.MoveTo(aRect.Right-1, aRect.Top);    // Right line
          Canvas.LineTo(aRect.Right-1, aRect.Bottom-1);
          Canvas.Pen.Width := 1;
        end;
      end
      else if (ACol = Cols.getVisibleIndex(Cols.Col[GROUP_COLUMN])) and (VS_SeqDataExplorer.DispGpNamesItem) and (Cols.Col[GROUP_COLUMN].Visible) then
      begin
        if rowInHighlightRange(ARow) then
          Brush.Color := clAqua
        else
        Brush.Color := DataGrid.FixedColor;
        FillRect(aRect);
        X := aRect.Left + 1;
        Y := aRect.Top + (aRect.Bottom - aRect.Top - TextHeight(' ')) div 2;
        if Assigned(FGroupCounts) and ActionDisplayGroupCounts.Checked then
        begin
          gc := TGroupCount(FGroupCounts.Find(D_InputSeqData.GpName[ARow - 1]));
          if Assigned(gc) then
          begin
            if gc.Count = 1 then
              Canvas.TextRect(aRect, X, Y, Format('%s (1 taxon)', [D_InputSeqData.GpName[ARow - 1]]))
            else
              Canvas.TextRect(aRect, X, Y, Format('%s (%d taxa)', [D_InputSeqData.GpName[ARow - 1], gc.Count]))
          end
          else
            Canvas.TextRect(aRect, X, Y, D_InputSeqData.GpName[ARow-1]);
        end
        else
          Canvas.TextRect(aRect, X, Y, D_InputSeqData.GpName[ARow-1]);
      end
      else if (ACol = TAXA_COLUMN) and (ARow > 0) then // fixed column 0
      begin
        if rowInHighlightRange(ARow) then
          Brush.Color := clAqua
        else
        Brush.Color := DataGrid.FixedColor;
        FillRect(aRect);

        X := aRect.Left + 1;
        Y := aRect.Top + (aRect.Bottom - aRect.Top - CheckMark.Height) div 2;
        if D_InputSeqData.SeqUsed[ARow-1] then
          Draw(X, Y, CheckMark)
        else
          Draw(X, Y, UnCheckMark);
        X := X + CheckMark.Width + 2;
        Y := aRect.Top + (aRect.Bottom - aRect.Top - TextHeight(' ')) div 2;
        aRect.Left := X;
        DispChar[0] :=  ' ';
        if (SeqNameResult <> EmptyStr) and (SeqNameResult = D_InputSeqData.TaxaName[ARow-1]) then
          Canvas.Font.Style := Canvas.Font.Style + [fsBold];
        if ActionDispTaxaNames.Checked then
          TextRect(aRect, X, Y, IntToStr(ARow)+'. '+D_InputSeqData.TaxaName[ARow-1])
        else
          TextRect(aRect, X, Y, IntToStr(ARow)+'. '+DispChar);
        Canvas.Font.Style := Canvas.Font.Style - [fsBold];
      end
      else if (ARow = 0) then // fixed row
      begin
        if ActionUseIdenticalSymbol.Checked then
          DispChar[0] := D_InputSeqData.ReferenceSeq[relSite]
        else                    // how to show site specfic marks; create another fixed row?
          DispChar[0] :=  ' ';

        Brush.Color := DataGrid.FixedColor;
        FillRect(aRect);
        with aRect do
        begin
          Canvas.Brush.Style := bsClear;
          Canvas.Font.Color := clBlack; // IDentical
          ts := Canvas.TextStyle;
          ts.Alignment := taCenter;
          TextRect(aRect, 0, 0, DispChar[0], ts);
          Canvas.Font.Color := clBlack;
          Canvas.Brush.Style := bsSolid;

          if D_InputSeqData.FIsCoding and (not VS_SeqDataExplorer.IsTranslated) then
          begin
            if D_InputSeqData.HasAttribute(ACol-Cols.VisibleCount, meg1stBase) then
            begin
              Pen.Color   := clSilver;
              PolyLine([Point(Right, Bottom-codonBoxMargins.Bottom),
                        Point(Left+codonBoxMargins.Left,  Bottom-codonBoxMargins.Bottom),
                        Point(Left+codonBoxMargins.Left,  Top+codonBoxMargins.Top),
                        Point(Right, Top+codonBoxMargins.Top)]);
              Pen.Color   := clBlack;
            end
            else if D_InputSeqData.HasAttribute(ACol-Cols.VisibleCount, meg2ndBase) then
            begin
              Pen.Color   := clGray;
              MoveTo(Left, Bottom-codonBoxMargins.Bottom);
              LineTo(Right, Bottom-codonBoxMargins.Bottom);
              MoveTo(Left, Top+codonBoxMargins.Top);
              LineTo(Right, Top+codonBoxMargins.Top);
            end
            else if D_InputSeqData.HasAttribute(ACol-Cols.VisibleCount, meg3rdBase) then
            begin
              Pen.Color   := clSilver;
              PolyLine([Point(Left, Bottom-codonBoxMargins.Bottom),
                        Point(Right-codonBoxMargins.Right,
                        Bottom-codonBoxMargins.Bottom),
                        Point(Right-codonBoxMargins.Right,
                        Top+codonBoxMargins.Top),
                        Point(Left, Top+codonBoxMargins.Top)]);
            end;
          end;
        end;
      end
      else if (ARow > 0) and (ACol > Cols.VisibleCount-1) then // if a cell with sequence
      begin
        if D_InputSeqData.Sequence[ARow-1] <> nil then
          DispChar[0] := D_InputSeqData.Sequence[ARow-1][(ACol-Cols.VisibleCount)]
        else
          DispChar[0] := ' ';
        if ActionUseIdenticalSymbol.Checked and
           (Upcase(DispChar[0]) = Upcase(D_InputSeqData.ReferenceSeq[ACol-Cols.VisibleCount])) and (ARow > 0) then
        begin
          if (Upcase(DispChar[0]) <> Upcase(D_InputSeqData.FGapSym) ) and
             (Upcase(DispChar[0]) <> Upcase(D_InputSeqData.FMissSym)) then
            DispChar[0] := D_InputSeqData.FIdenSym;
        end;

        Font.Color := clGray;
        Brush.Color := clWhite;

        IsUsed := False;
        if D_InputSeqData.SeqUsed[ARow-1] and D_InputSeqData.SiteUsed[ACol-Cols.VisibleCount] then
        begin
          Font.Color := clBlack;
          if VS_SeqDataExplorer.CurAttrDisp <> megNone then
          begin
            if VS_SeqDataExplorer.IsHighlighted(ACol-Cols.VisibleCount) then
              Brush.Color := VS_SeqDataExplorer.HighlightColor
          end
          else if ActionColorCells.Checked then
          begin
            if DispChar[0] = D_InputSeqData.FIdenSym then
              SetBrushColor(D_InputSeqData.ReferenceSeq[ACol-Cols.VisibleCount])
            else
              SetBrushColor(DispChar[0]);
          end
          else if rowInHighlightRange(ARow) or ((ACol-Cols.VisibleCount+1) = VS_SeqDataExplorer.CurColorSite) then
            Brush.Color := clAqua;
          IsUsed := True;
        end;

        if gdFocused in aState then
        begin
          Brush.Color := clNavy;
          Font.Color  := clWhite;
        end
        else if IsUsed then
        begin
          if (VS_SeqDataExplorer.CurMotifStr <> EmptyStr) and
                  (CloseSearchPanel.Visible)
          then
          begin
            if SiteInSearchBox(ARow-1, ACol-Cols.VisibleCount+1) then
            begin
              if Brush.Color = clRed then
               Brush.Color := clFuchsia
              else
              begin
                if MainSearchPanel.Visible then
                  Brush.Color := Brush.Color and clLime;
              end;
               Font.Color := clBlack;
            end;
          end;
        end;

        FillRect(aRect);

        ts := Canvas.TextStyle;
        ts.Alignment := taCenter;
        TextRect(aRect, (aRect.Left + aRect.Right) div 2, aRect.Top + 2, DispChar[0], ts);
        Font.Color := clBlack;
        if (gdFocused in aState) or (gdSelected in aState) then
          DrawFocusRect(aRect);
      end;
      if (ACol-Cols.VisibleCount) >= 0 then
        drawBracket(ACol-Cols.VisibleCount, Canvas);


      if (ACol >= 0) and (ACol < Cols.VisibleCount) then
      begin
        if (VS_SeqDataExplorer.FIsResizing) and (ARow > 0) then Exit; // Cuts down on flicker/blur by only drawing the line once (on cell 0, 0) when resizing.
        Canvas.Pen.Color := clLtGray;
        ResizeLineRect := DataGrid.CellRect(ACol, 0);    // Rect for cell 1, 1
        Canvas.MoveTo(ResizeLineRect.Right-1, 0); // Move to the right side of the first col.
        Canvas.LineTo(ResizeLineRect.Right-1, (DataGrid.RowCount-1) * (ResizeLineRect.Bottom - ResizeLineRect.Top +1)+10); // Draw a line down the height of the first col.
        Canvas.MoveTo(ResizeLineRect.Left, aRect.Bottom-1);
        Canvas.LineTo(aRect.Right-1, aRect.Bottom-1);
      end;
      if (ACol = Col) and (aRow > 0) and (ACol >= NumberOfFixedColsVisible) then
        DrawCurrentColumnBorder(ARect, aRow = 1, aRow = D_InputSeqData.NoOfTaxa);
    end;
  except
    on E:Exception do
    begin
      {$IFDEF DEBUG}
      ShowMessage('Error in SDE draw cell: ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

procedure TV_SeqDataExplorer.DataGridGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
var
  aaCode: AnsiString;
begin
  if D_InputSeqData.IsCoding and (not VS_SeqDataExplorer.IsTranslated) and ((ACol - NumberOfFixedColsVisible) >= 0) and ((ARow > 0) or ActionUseIdenticalSymbol.Checked) then
  begin
    aaCode := D_InputSeqData.GetCodonStringAtNucSite(ACol - NumberOfFixedColsVisible, max(ARow - 1, 0));
    HintText := aaCode;
  end;
end;

procedure TV_SeqDataExplorer.DataGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  delta: Integer = -1;
  cellCoord: Integer = -1;
begin
  try
    FSelfScrolling := True;
    if (ssShift in Shift) or (ssCtrl in Shift) then
    begin
      if Key = VK_LEFT then
      begin
        delta := DataGrid.Col - DataGrid.LeftCol;
        cellCoord := DataGrid.LeftCol - HorizScrollbar.LargeChange;
        DataGrid.LeftCol := ValidSelectableCol(cellCoord);
        DataGrid.Col := ValidSelectableCol(DataGrid.Col - HorizScrollbar.LargeChange + 1);
      end
      else if Key = VK_RIGHT then
      begin
        delta := DataGrid.Col - DataGrid.LeftCol;
        cellCoord := DataGrid.LeftCol + HorizScrollbar.LargeChange;
        DataGrid.LeftCol := ValidSelectableCol(cellCoord);
        if delta > 0 then
        begin
          cellCoord := DataGrid.Col + HorizScrollbar.LargeChange - 1;
          DataGrid.Col := ValidSelectableCol(cellCoord);
        end
        else
          DataGrid.Col := ValidSelectableCol(DataGrid.LeftCol);
      end
      else if Key = VK_UP then
      begin
        delta := DataGrid.Row - DataGrid.TopRow;
        cellCoord := DataGrid.TopRow - VertScrollbar.LargeChange;
        DataGrid.TopRow := ValidSelectableRow(cellCoord);
        cellCoord := DataGrid.Row - VertScrollbar.LargeChange + 1;
        if delta > 0 then
          DataGrid.Row := ValidSelectableRow(cellCoord)
        else
          DataGrid.Row := DataGrid.TopRow;
      end
      else if Key = VK_DOWN then
      begin
        delta := DataGrid.Row - DataGrid.TopRow;
        cellCoord := DataGrid.TopRow + VertScrollbar.LargeChange;
        DataGrid.TopRow := ValidSelectableRow(cellCoord);
        if delta > 0 then
        begin
          cellCoord := DataGrid.Row + VertScrollbar.LargeChange - 1;
          DataGrid.Row := ValidSelectableRow(cellCoord);
        end
        else
        begin
          cellCoord := DataGrid.TopRow;
          DataGrid.Row := ValidSelectableRow(cellCoord);
        end;
      end;
    end
    else
      inherited;
  finally
    FSelfScrolling := False;
  end;
end;

procedure TV_SeqDataExplorer.DataGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  try
    FUpdatingSiteIndex := True;
    if SiteNumSpinEdit.Value <> DataGrid.Col - NumberOfFixedColsVisible + 1 then
      SiteNumSpinEdit.Value := DataGrid.Col - NumberOfFixedColsVisible + 1;
  finally
    FUpdatingSiteIndex := False;
  end;
end;

procedure TV_SeqDataExplorer.DataGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol: Integer = -1;
  ARow: Integer = -1;
  mouseRect: TRect;
begin
  if Button = mbRight then
    rightMouseDownDataGrid := true;
  DataGrid.MouseToCell(X, Y, ACol, ARow);
  if DataGrid.Cursor = crHSplit then  // Only start resizing, once the cursor is over the correct place and you get a click, this way you can't "slide" into it accidently.
  begin
    VS_SeqDataExplorer.FIsResizing := true;
    mouseRect := DataGrid.CellRect(ACol, ARow);
    if ((X - mouseRect.Left) < CheckMark.Width) and (ACol > 0) then
      ACol := ACol - 1;
    VS_SeqDataExplorer.targetedResizingCol := ACol;
  end;
  if (ARow = 0) and (Button = mbLeft) then
    if ACol < Cols.VisibleCount then
      begin
        if (ACol = 0) and (X < CheckMark.Width) then
        begin
          VS_SeqDataExplorer.allTaxaCheckDown := true;
        end
        else
          Cols.VisibleCol[ACol].isDown := true;
        DataGrid.Invalidate;
      end;
  if (ACol = 0) and (X <= (CheckMark.Width + 5)) then
  begin
    FDisallowRowMoving := True;
    DataGrid.Options := DataGrid.Options - [goRowMoving];
  end;
  bird.Invalidate;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TV_SeqDataExplorer.DataGridMouseEnter(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

procedure TV_SeqDataExplorer.DataGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const
  DRAG_INCREMENT = 5;
var
  Col: Integer = -1;
  Row: Integer = -1;
  SearchResult: TSearchResult = nil;
  i, colLeft: Integer;
  showSplit: Boolean = False;
  hideMsg: AnsiString = '';
begin
  FScrollStarted := False;
  showSplit := false;
  inherited MouseMove(Shift, X, Y);
  try
    if VS_SeqDataExplorer.FIsResizing then
    begin
      // Calculate actual width (if col # > 0 then calc width).
      for I := VS_SeqDataExplorer.targetedResizingCol-1 downto 0 do
        X := X - DataGrid.ColWidths[i];
      if X > CheckMark.Width then // Can't have a col smaller than our checkbox
      begin
        if VS_SeqDataExplorer.targetedResizingCol >= 0 then
          DataGrid.ColWidths[VS_SeqDataExplorer.targetedResizingCol] := X;
        FHasDataViewChanged   := True;
      end
      else if VS_SeqDataExplorer.targetedResizingCol <= 1 then
      begin
        hideMsg := 'Hide column ';
        case VS_SeqDataExplorer.targetedResizingCol of
          0: hideMsg := hideMsg + 'Sequence Names';
          1: hideMsg := hideMsg + 'Group Names';
        end;
        hideMsg := hideMsg + '?';
        DataGridMouseUp(Sender, mbLeft, [], X, Y);
        if (MessageDlg(hideMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
        begin
          case VS_SeqDataExplorer.targetedResizingCol of
            0: ActionDispTaxaNamesExecute(Sender);
            1: ActionDispGroupNamesExecute(Sender);
          end;
        end;
      end;
    end;

    colLeft := 0;
    for I := 0 to Cols.count-1 do
    begin
     if Cols.Col[i].Visible then
       colLeft := colLeft + DataGrid.ColWidths[i];

     if (abs(X-colLeft) < 5) or (VS_SeqDataExplorer.FIsResizing) then
      begin
        DataGrid.Cursor := crHSplit;
        DataGrid.Options := DataGrid.Options - [goRowMoving]; //If we show them the split arrow they can split but not move rows otherwise rows are accidentally moved
        ShowSplit := true;
      end;
    end;

    DataGrid.MouseToCell(X, Y, Col, Row);
    if (not showSplit) and (not ((Col = 0) and (X <= CheckMark.Width))) then
    begin
      if (not (goRowMoving in DataGrid.Options)) and (not FDisallowRowMoving) then
        DataGrid.Options := DataGrid.Options + [goRowMoving]; //They are not at a location to resize the first column so allow them to move rows
      if DataGrid.Cursor = crHSplit then
        DataGrid.Cursor := crArrow;
    end;

    CurDesc := EmptyStr;

    if (Col < 0) or (Row < 0) then Exit;

    if (Col <> 0) or (Row <> 0) or  (X > CheckMark.Width) then
      VS_SeqDataExplorer.allTaxaCheckDown := false;

    try
      if (D_InputSeqData.NoOfSearchResults > 0) and (Row > 0) then
        SearchResult := GetSearchResult(Row - 1, Col - 1);
    except
      on E:Exception do
     begin
       SearchResult := nil;
     end;
    end;

    if Row <> FLastRow then
    begin
      HintTimer.Enabled := False;
      HintTimer.Enabled := True;
      if ssLeft in Shift then
      begin
        if (Row < FLastRow) and (VertScrollbar.Position > 0) then { dragging upward}
        begin
          if abs(Row - VertScrollbar.Position) <= DRAG_INCREMENT then
          begin
            VertScrollbar.Position := max(0, VertScrollbar.Position - 2);
          end;
        end
        else if (Row > FLastRow) and (VertScrollBar.Position < VertScrollBar.Max) then { dragging downward}
        begin
          if abs(Row - VertScrollbar.Position - VertScrollBar.PageSize) <= DRAG_INCREMENT then
          begin
            VertScrollBar.Position := min(VertScrollBar.Max, VertScrollBar.Position + 2);
          end;
        end;
      end;
    end;
    FLastRow := Row;
    if SearchResult = nil then Exit;
    CurDesc := SearchResult.UserInput;
  except
     on E: Exception do
     begin
       CurDesc := EmptyStr;
     end;
  end;
end;

procedure TV_SeqDataExplorer.DataGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol: Integer = -1;
  ARow: Integer = -1;
  Rect: TRect;
  IsClickHandled, newCheckState: Boolean;
  xyFromDataGrid: TPoint;
  i, iCol: Integer;
begin
  FDisallowRowMoving := False;
  inherited MouseUp(Button, Shift, X, Y);
  try
    if VS_SeqDataExplorer.FIsResizing then
    begin
      VS_SeqDataExplorer.FIsResizing := False;
      DataGrid.Invalidate;
      Exit; // If resizing, can't do anything else.
    end;

    xyFromDataGrid.X := X + DataGrid.Left + (DataGrid.DefaultColWidth div 3);
    xyFromDataGrid.Y := Y;
    xyFromDataGrid := ClientToScreen(xyFromDataGrid);
    with DataGrid do
    begin
      MouseToCell(X, Y, ACol, ARow);
      IsClickHandled := False;

      // IF we click on a header, we want to sort the columns based on which header was clicked.
      if (ARow = 0) and (Button = mbLeft) and (ACol < Cols.VisibleCount) then
      begin
        // Check all button on name column
        Rect := CellRect(ACol, ARow);
        X := X - Rect.Left-1;
        Y := Y - (Rect.Top + (Rect.Bottom - Rect.Top - CheckMark.Height) div 2);

        if (X >= 0) and (X <= CheckMark.Width) and
           (Y >= 0) and (Y <= CheckMark.Height) and
           VS_SeqDataExplorer.allTaxaCheckDown then
        begin
          if VS_SeqDataExplorer.DispSelTaxaItem then
          begin
            MessageDlg('The "Select All" option is not available when "Show Only Selected Sequences" is turned on.', mtWarning, [mbOK], 0);
            Exit;
          end;
          VS_SeqDataExplorer.allTaxaCheckDown := false;
          // check/uncheck all taxa
          newCheckState := not (D_InputSeqData.OtuInfos.NoOfSelOtus = D_InputSeqData.OtuInfos.NoOfOtus);
          D_InputSeqData.FOtuInfos[D_InputSeqData.DispTaxaList[0]].IsUsed := true;

          for i := 1 to D_InputSeqData.DispTaxaList.Count-1 do
          begin
            with D_InputSeqData.FOtuInfos[D_InputSeqData.DispTaxaList[i]] do
              begin
                IsUsed := newCheckState;
              end;
          end;

          D_InputSeqData.FOtuInfos.IsDirty := True;
          Self.ApplyUpdate;
          D_InputSeqData.FOtuInfos.IsDirty := False;
        end;

        if Cols.VisibleCol[ACol].isDown then
        begin
          Cols.VisibleCol[ACol].SwapSort;

          if (ACol = Cols.getVisibleIndex(Cols.Col[TAXA_COLUMN])) and (VS_SeqDataExplorer.DispTaxaNamesItem) and (Cols.Col[TAXA_COLUMN].Visible) then
          begin
            if (Cols.VisibleCol[ACol].SortOrder = soNone) OR (Cols.VisibleCol[ACol].SortOrder = soDescending)  then
              D_InputSeqData.OtuInfos.SortByTaxaNameDes
            else
              D_InputSeqData.OtuInfos.SortByTaxaNameAsc;
            if Cols.VisibleCount > 1 then
            begin
              for i := 1 to Cols.VisibleCount - 1 do
                Cols.VisibleCol[i].SortOrder := soNone;
            end;
          end;

          if (ACol = Cols.getVisibleIndex(Cols.Col[GROUP_COLUMN])) and (VS_SeqDataExplorer.DispGpNamesItem) and (Cols.Col[GROUP_COLUMN].Visible) then
          begin
            if (Cols.VisibleCol[ACol].SortOrder = soNone) OR (Cols.VisibleCol[ACol].SortOrder = soDescending)  then
              D_InputSeqData.OtuInfos.SortByGroupNameDes
            else
              D_InputSeqData.OtuInfos.SortByGroupNameAsc;
            if Cols.VisibleCount > 1 then
              for i := 0 to Cols.VisibleCount - 1 do
                if i <> Cols.getVisibleIndex(Cols.Col[GROUP_COLUMN]) then
                  Cols.VisibleCol[i].SortOrder := soNone;
          end;
          D_InputSeqData.UpdateDispTaxaList;
        end;
        Invalidate;
      end;
      // Prevents any columns from being left in the 'down' position after mouse up.
      for iCol := 0 to Cols.VisibleCount-1 do
      begin
        if Cols.VisibleCol[iCol].isDown then
        begin
          Cols.VisibleCol[iCol].isDown := false;
          Invalidate;
        end;
      end;
      if (ACol = 0) and (ARow > 0) and (Button = mbLeft) then // toggle the status
      begin
        Rect := CellRect(ACol, ARow);
        X := X - Rect.Left-1;
        Y := Y - (Rect.Top + (Rect.Bottom - Rect.Top - CheckMark.Height) div 2);

        if (X >= 0) and (X <= CheckMark.Width) and
           (Y >= 0) and (Y <= CheckMark.Height) then
        begin
          if (VS_SeqDataExplorer.NoOfSelTaxa = 1) and D_InputSeqData.SeqUsed[ARow-1] then
          begin
            ShowMessage('Sorry, you cannot unselect all sequences');
            Exit;
          end;
          with D_InputSeqData.FOtuInfos[D_InputSeqData.DispTaxaList[ARow-1]] do
          begin
            IsUsed := not IsUsed;
            D_InputSeqData.FOtuInfos.IsDirty := True;
          end;
          HighlightSitesClick(nil);
          Self.ApplyUpdate;
          D_InputSeqData.FOtuInfos.IsDirty := False;
          IsClickHandled := True;
        end;
        Invalidate;
      end;
      if (not IsClickHandled) and (Button = mbLeft)  then
      begin
        if (ACol < Cols.VisibleCount) and (ARow > 0) then
        begin
           if VS_SeqDataExplorer.CurColorRowRange.X = ARow then
           begin
             VS_SeqDataExplorer.CurColorRowRange := Point(-1, -1);
           end
           else
           begin
             if ssShift in Shift then
             begin
               if VS_SeqDataExplorer.CurColorRowRange.X < 1 then
                 VS_SeqDataExplorer.CurColorRowRange.X := 1;
               if ARow < VS_SeqDataExplorer.CurColorRowRange.X then
                 // If the new row selected is smaller, then we need to make that the start, and the cur stat the end.
                 VS_SeqDataExplorer.CurColorRowRange := Point(ARow, VS_SeqDataExplorer.CurColorRowRange.X)
               else
                 VS_SeqDataExplorer.CurColorRowRange.Y := ARow;
             end
             else
             begin
               VS_SeqDataExplorer.CurColorRowRange := Point(ARow, ARow);
             end;
           end;
           Invalidate;
        end
        else if (ACol > 0) and (ARow = 0) then
        begin
           Invalidate;
        end;
      end;

      if (ARow > 0) and rightMouseDownDataGrid then
      begin
        // If right click on a group, NOT in the selected, range select it!
        if (not rowInHighlightRange(ARow))then
        begin
          VS_SeqDataExplorer.CurColorRowRange := Point(ARow, ARow);
          DataGrid.Invalidate;
        end;

        if ACol = Cols.getVisibleIndex(Cols.Col[1]) then
          EditGroup.PopUp(xyFromDataGrid.X, xyFromDataGrid.Y);
      end;

    end;
    rightMouseDownDataGrid := false;
    bird.Invalidate;
    try
      FUpdatingSiteIndex := True;
      SiteNumSpinEdit.Value := ACol - NumberOfFixedColsVisible + 1;
    finally
      FUpdatingSiteIndex := False;
    end;
    Invalidate;
  except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TV_SeqDataExplorer.DataGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  CanSelect := ((aCol >= 0) and (aRow >= 0));
  if not CanSelect then
    Exit;
  VS_SeqDataExplorer.CurCol := ACol;
  VS_SeqDataExplorer.CurRow := ARow;
  if DiagnoseButton.Visible then
    UpdateDiagnoseBtnCaption(ACol-Cols.VisibleCount+1);
  FHasDataViewChanged := True;
  bird.Invalidate;
  DataGrid.Invalidate;
  UpdateCurStatisticsStrings(aCol, aRow);
  if not FUpdatingSiteIndex then
  begin
    try
      FUpdatingSiteIndex := True;
      FSelfScrolling := True;
      SiteNumSpinEdit.Value := aCol - NumberOfFixedColsVisible + 1;

      if (not ColumnVisible(HorizScrollBar.Position)) or (not ColumnVisible(aCol)) then
      begin
        if aCol < HorizScrollbar.Position then
          HorizScrollBar.Position := ValidSelectableCol(aCol)
        else if aCol > HorizScrollbar.Position then
          HorizScrollBar.Position := ValidSelectableCol(max(1, aCol - DataGrid.VisibleColCount));
      end;

      if (not RowVisible(VertScrollbar.Position)) or (not RowVisible(aRow)) then
      begin
        if (aRow < VertScrollbar.Position) then
          VertScrollbar.Position := ValidSelectableRow(aRow)
        else if (aRow > VertScrollbar.Position) then
          VertScrollbar.Position := ValidSelectableRow(max(1, aRow - DataGrid.VisibleRowCount));
      end;
      if DiagnoseButton.Visible then
        VS_SeqDataExplorer.CurColorSite := aCol - Cols.VisibleCount + 1;
    finally
      FUpdatingSiteIndex := False;
      FSelfScrolling := False;
    end;
  end;
end;

procedure TV_SeqDataExplorer.DataGridTopleftChanged(Sender: TObject);
begin
  bird.Invalidate;
end;

procedure TV_SeqDataExplorer.DeveloperActionExecute(Sender: TObject);
begin
  ExportActiveDomainsToFastaFiles(D_InputSeqData, 'C:\Users\gstecher\Documents\TRASH\FastaFiles', nil);
end;

procedure TV_SeqDataExplorer.DiagnoseButtonClick(Sender: TObject);
var
  Mutation: String = '';
  TempFileName: String = '';
  i: Integer = 0;
  QueryList: TStringList = nil;
  SelectedSite: Integer = -1;
begin
  if not Sender.ClassNameIs('TMenuItem') then
    Exit;
  try
    try
      if not Assigned(MutationExplorerForm) then
        MutationExplorerForm := TMutationExplorerForm.Create(MegaForm);
      QueryList := TStringList.Create;
      SelectedSite := Max(VS_SeqDataExplorer.CurColorSite, 1);
      Mutation := (Sender as TMenuItem).Caption;
      if pos('&', Mutation) <> 0 then
        Delete(Mutation, Pos('&', Mutation), 1);
      if pos('(', Mutation) > 0 then
        Mutation := Mutation[1];
      TempFileName := getTemp + '\' + EvoDNmID + '_' + IntToStr(SelectedSite) + '_' + IntToStr(i) + '_result.xml';
      while FileExists(TempFileName) do
      begin
        inc(i);
        TempFileName := getTemp + '\' + EvoDNmID + '_' + IntToStr(SelectedSite) + '_' + IntToStr(i) + '_result.xml';
      end;
      if Mutation = 'All' then
      begin
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' A');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' C');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' D');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' E');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' F');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' G');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' H');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' I');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' K');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' L');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' M');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' N');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' P');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' Q');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' R');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' S');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' T');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' V');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' W');
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' Y');
      end
      else
      begin
        QueryList.Add(EvoDPeptideId + ' ' + IntToStr(SelectedSite) + ' ' + Mutation);
      end;
      QueryList.SaveToFile(TempFileName);
      MutationExplorerForm.ImportDataQueryFileDirect(TempFileName);
    except
      on E: Exception do
        ShowMessage('An error occurred when querying mutation data: ' + E.Message);
    end;
  finally
    if Assigned(QueryList) then
      QueryList.Free;
  end;
end;

procedure TV_SeqDataExplorer.DiagnoseMenuPopup(Sender: TObject);
begin
  DiagnoseMenu.PopUp;
end;

procedure TV_SeqDataExplorer.DispSortByGpItemClick(Sender: TObject);
begin
  ActionSortSequencesExecute(DispSortByGpItem);
end;

procedure TV_SeqDataExplorer.DispSortBySeqNameItemClick(Sender: TObject);
begin
  ActionSortSequencesExecute(DispSortBySeqNameItem);
end;

procedure TV_SeqDataExplorer.DomainInfoSummaryExecute(Sender: TObject);
var
  aList: TStringList = nil;
begin
  try
    aList := D_InputSeqData.AllDomainInfo.FullInfosStringList;
    OpenStringList(aList, 'Domain Info Summary', True);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TV_SeqDataExplorer.EditGroupDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
begin
  if not Visible then Exit;
  MenuRenderer.PopupMenuDrawItem(Sender, ACanvas, ARect, AState, MenuItemCheckmark);
end;

procedure TV_SeqDataExplorer.EditGroupPopup(Sender: TObject);
var
  ACol: Integer = -1;
  ARow: Integer = -1;
  i, insertIndex: Integer;
  newMenu: TMenuItem = nil;
  tempPoint: TPoint;
  sortedList: TStringList = nil;
begin
  sortedList := TStringList.Create;
  sortedList.Sorted := true;
  try
    EditGroup.Items.Clear;
    tempPoint := DataGrid.ScreenToClient(EditGroup.PopupPoint);
    DataGrid.MouseToCell(tempPoint.X, tempPoint.Y, ACol, ARow);
    for i := 0 to D_InputSeqData.OtuInfos.NoOfOtus-1 do
    begin
      if Trim(D_InputSeqData.OtuInfos.Otu[i].GpName) = EmptyStr then
        continue;
      if EditGroup.Items.Find(D_InputSeqData.OtuInfos.Otu[i].GpName) = nil then
      begin
        // Sorted index ensures that the list is in a constant order (otherwise the gpNames would appear in the order they are used, which can change as you change groups)
        sortedList.Add(D_InputSeqData.OtuInfos.Otu[i].GpName);
        insertIndex := sortedList.IndexOf(D_InputSeqData.OtuInfos.Otu[i].GpName);
        newMenu := TMenuItem.Create(EditGroup);
        newMenu.AutoCheck := true;
        newMenu.Caption := D_InputSeqData.OtuInfos.Otu[i].GpName;
        newMenu.OnClick := ChangeGroupClick;
        EditGroup.Items.Insert(insertIndex, newMenu);
      end;
      if i = ARow-1 then
        (EditGroup.Items.Find(D_InputSeqData.OtuInfos.Otu[i].GpName)).Checked := true;
    end;
    if EditGroup.Items.Count > 0 then
    begin
      newMenu := TMenuItem.Create(EditGroup);
      newMenu.Caption := '-';
      EditGroup.Items.Add(newMenu);
    end;
    newMenu := TMenuItem.Create(EditGroup);
    newMenu.Action := ActionAddGroup;
    newMenu.Caption := ActionAddGroup.Caption;
    EditGroup.Items.Add(newMenu);
  finally
    if Assigned(sortedList) then
      sortedList.Free;
  end;
end;

procedure TV_SeqDataExplorer.ActiveGroupClick(Sender: TObject);
var
  i: Integer;
  groupName: AnsiString;
begin
  groupName := TMenuItem(Sender).Caption;
  ShowMessage('De-selecting all taxa with group: ' + groupName);
  for I := 0 to D_InputSeqData.OtuInfos.NoOfOtus-1 do
  begin
    while pos('&', groupName) > 0 do
      Delete(groupname, Pos('&', groupName), 1);
    if D_InputSeqData.OtuInfos.Otu[i].GpName = groupName then
      D_InputSeqData.OtuInfos.Otu[i].IsUsed := TMenuItem(Sender).Checked;
  end;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.EditGroupClick(Sender: TObject);
var
  i: Integer;
  groupName, newGpName: String;
begin
  groupName := TMenuItem(Sender).Caption;
  while pos('&', groupName) > 0 do
    Delete(groupname, Pos('&', groupName), 1);
  newGpName := groupName; // set the default value as the current name
  if InputQuery('Rename group', 'Rename the group', newGpName) then
  begin
    for I := 0 to D_InputSeqData.OtuInfos.NoOfOtus-1 do
    begin
      if D_InputSeqData.OtuInfos.Otu[i].GpName = groupName then
      begin
        D_InputSeqData.OtuInfos.Otu[i].GpName := newGpName;
        D_InputSeqData.OtuInfos.IsDirty := true;
      end;
    end;
    DataGrid.Invalidate;
  end;
end;

procedure TV_SeqDataExplorer.DeleteGroupClick(Sender: TObject);
var
  i: Integer;
  groupName: String;
  AInfo: TOtuInfo;
begin
  groupName := TMenuItem(Sender).Caption;
  while pos('&', groupName) > 0 do
    Delete(groupname, Pos('&', groupName), 1);
  for I := 0 to D_InputSeqData.OtuInfos.NoOfOtus-1 do
  begin
    AInfo := D_InputSeqData.OtuInfos.Otu[i];
    if AInfo.GpName = groupName then
      begin
        AInfo.GpName := EmptyStr;
        if SameText(Groupname, 'outgroup') then
          AInfo.OutgroupMember := False;
        D_InputSeqData.OtuInfos.IsDirty := true;
      end;
  end;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ChangeGroupClick(Sender: TObject);
var
  i: Integer;
  groupName: AnsiString;
  tempPoint: TPoint;
  ACol: Integer = -1;
  ARow: Integer = -1;
  AInfo: TOtuInfo;
begin
  groupName := TMenuItem(Sender).Caption;
  while pos('&', groupName) > 0 do
    Delete(groupname, Pos('&', groupName), 1);
  tempPoint := DataGrid.ScreenToClient(EditGroup.PopupPoint);
  DataGrid.MouseToCell(tempPoint.X, tempPoint.Y, ACol, ARow);
  for I := VS_SeqDataExplorer.CurColorRowRange.X to VS_SeqDataExplorer.CurColorRowRange.Y do
  begin
    AInfo := D_InputSeqData.OtuInfos.Otu[i-1];
    AInfo.GpName := groupName;
    if SameText(groupName, 'outgroup') then
      AInfo.OutgroupMember := True
    else
      AInfo.OutgroupMember := False;
  end;
  D_InputSeqData.OtuInfos.IsDirty := true;
  TaxaGpsDlg.fillGroupTreeViewFromOtuInfo;
  ResetGroupCounts;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ChangeSpeciesClick(Sender: TObject);
var
  speciesName: AnsiString;
  tempPoint: TPoint;
  ACol: Integer = -1;
  ARow: Integer = -1;
begin
  speciesName := TMenuItem(Sender).Caption;
  while pos('&', speciesName) > 0 do
    Delete(speciesname, Pos('&', speciesName), 1);
  tempPoint := DataGrid.ScreenToClient(EditSpecies.PopupPoint);
  DataGrid.MouseToCell(tempPoint.X, tempPoint.Y, ACol, ARow);
  D_InputSeqData.OtuInfos.Otu[ARow-1].SpName := speciesName;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ChangePopulationClick(Sender: TObject);
var
  populationName: AnsiString;
  tempPoint: TPoint;
  ACol: Integer = -1;
  ARow: Integer = -1;
begin
  populationName := TMenuItem(Sender).Caption;
  while pos('&', populationName) > 0 do
    Delete(populationname, Pos('&', populationName), 1);
  tempPoint := DataGrid.ScreenToClient(Editpopulation.PopupPoint);
  DataGrid.MouseToCell(tempPoint.X, tempPoint.Y, ACol, ARow);
  D_InputSeqData.OtuInfos.Otu[ARow-1].PopName := populationName;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.EditPopulationDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
begin
  if not Visible then Exit;
  MenuRenderer.PopupMenuDrawItem(Sender, ACanvas, ARect, AState, MenuItemCheckmark);
end;

procedure TV_SeqDataExplorer.EditPopulationPopup(Sender: TObject);
var
  i: Integer;
  ACol: Integer = -1;
  ARow: Integer = -1;
  newMenu: TMenuItem;
  tempPoint: TPoint;
begin
  EditPopulation.Items.Clear;
  tempPoint := DataGrid.ScreenToClient(EditPopulation.PopupPoint);
  DataGrid.MouseToCell(tempPoint.X, tempPoint.Y, ACol, ARow);
  for i := 0 to D_InputSeqData.OtuInfos.NoOfOtus-1 do
  begin
    if Trim(D_InputSeqData.OtuInfos.Otu[i].PopName) = EmptyStr then
      continue;
    if EditPopulation.Items.Find(D_InputSeqData.OtuInfos.Otu[i].PopName) = nil then
    begin
      newMenu := TMenuItem.Create(EditPopulation);
      newMenu.AutoCheck := true;
      newMenu.Caption := D_InputSeqData.OtuInfos.Otu[i].PopName;
      newMenu.OnClick := ChangePopulationClick;
      EditPopulation.Items.Add(newMenu);
    end;
    if i = ARow-1 then
      (EditPopulation.Items.Find(D_InputSeqData.OtuInfos.Otu[i].PopName)).Checked := true;
  end;
  if EditPopulation.Items.Count > 0 then
  begin
    newMenu := TMenuItem.Create(EditPopulation);
    newMenu.Caption := '-';
    EditPopulation.Items.Add(newMenu);
  end;
  newMenu := TMenuItem.Create(EditPopulation);
  newMenu.Action := ActionAddPopulation;
  newMenu.Caption := ActionAddPopulation.Caption;
  EditPopulation.Items.Add(newMenu);
end;

procedure TV_SeqDataExplorer.EditSpeciesDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
begin
  if not Visible then Exit;
  MenuRenderer.PopupMenuDrawItem(Sender, ACanvas, ARect, AState, MenuItemCheckmark);
end;

procedure TV_SeqDataExplorer.EditSpeciesPopup(Sender: TObject);
var
  i: Integer;
  ACol: Integer = -1;
  ARow: Integer = -1;
  newMenu: TMenuItem;
  tempPoint: TPoint;
begin
  EditSpecies.Items.Clear;
  tempPoint := DataGrid.ScreenToClient(EditSpecies.PopupPoint);
  DataGrid.MouseToCell(tempPoint.X, tempPoint.Y, ACol, ARow);
  for i := 0 to D_InputSeqData.OtuInfos.NoOfOtus-1 do
  begin
    if Trim(D_InputSeqData.OtuInfos.Otu[i].SpName) = EmptyStr then
      continue;
    if EditSpecies.Items.Find(D_InputSeqData.OtuInfos.Otu[i].SpName) = nil then
    begin
      newMenu := TMenuItem.Create(EditSpecies);
      newMenu.AutoCheck := true;
      newMenu.Caption := D_InputSeqData.OtuInfos.Otu[i].SpName;
      newMenu.OnClick := ChangeSpeciesClick;
      EditSpecies.Items.Add(newMenu);
    end;
    if i = ARow-1 then
      (EditSpecies.Items.Find(D_InputSeqData.OtuInfos.Otu[i].SpName)).Checked := true;
  end;
  if EditSpecies.Items.Count > 0 then
  begin
    newMenu := TMenuItem.Create(EditSpecies);
    newMenu.Caption := '-';
    EditSpecies.Items.Add(newMenu);
  end;
  newMenu := TMenuItem.Create(EditSpecies);
  newMenu.Action := ActionAddSpecies;
  newMenu.Caption := ActionAddSpecies.Caption;
  EditSpecies.Items.Add(newMenu);
end;

procedure TV_SeqDataExplorer.FormActivate(Sender: TObject);
var
  aCol, aRow: Integer;
  scalingFactor: Double = 1;
begin
  if PixelsPerInch > DesignTimePPI then
    scalingFactor := PixelsPerInch/DesignTimePPI;
  if CompareValue(scalingFactor, 1.5, FP_CUTOFF) >= 0 then
  begin
    UnCheckMark.SetSize(32, 32);
    CheckMark.SetSize(32, 32);
    MenuItemCheckMark.SetSize(32, 32);
    CheckBoxes32.GetBitmap(CHECKMARK_IMAGE_INDEX, CheckMark);
    CheckBoxes32.GetBitmap(UNCHECKMARK_IMAGE_INDEX, UnCheckMark);
    CheckBoxes32.GetBitmap(MENUITEM_CHECKMARK_IMAGE_INDEX, MenuItemCheckmark);
  end
  else
  begin
    UnCheckMark.SetSize(16, 16);
    CheckMark.SetSize(16, 16);
    MenuItemCheckMark.SetSize(16, 16);
    ImageList1.GetBitmap(CHECKMARK_IMAGE_INDEX, CheckMark);
    ImageList1.GetBitmap(UNCHECKMARK_IMAGE_INDEX, UnCheckMark);
    ImageList1.GetBitmap(MENUITEM_CHECKMARK_IMAGE_INDEX, MenuItemCheckmark);
  end;
  InitScrollBars(False);
  if Visible then
    SetFocus;
  {$IFDEF DARWIN}
  MainToolbar.Invalidate;
  {$ENDIF}
  aCol := DataGrid.Col;
  aRow := DataGrid.Row;
  if (aCol > 0) and (aRow > 0) then
    UpdateCurStatisticsStrings(aCol, aRow);
  SetGroupTagButton.Enabled := D_InputSeqData.HasAnyGeographicalInfo;
  MegaForm.UpdateMenuForPinupItems(WindowsMenuItem, Self);
  {$IFNDEF DEBUG}
  DeveloperMenu.Visible := IsDeveloper;
  {$ELSE}
  DeveloperMenu.Visible := True;
  {$ENDIF}
end;

procedure TV_SeqDataExplorer.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TV_SeqDataExplorer.FormCreate(Sender: TObject);
var
  scalingFactor: Double = 1;
begin
  {$IFDEF DEBUG}
  DeveloperAction.Visible := True;
  {$ENDIF}
  FDisallowRowMoving := False;
  if PixelsPerInch > DesignTimePPI then
    scalingFactor := PixelsPerInch/DesignTimePPI;
  FScrollStarted := False;
  FSelfScrolling := False;
  FUpdatingGroupTagMenus := False;
  FUpdatingHighlightState := False;
  FEditLabelsFrame := TEditSiteLabelsFrame.Create(Self);
  FEditLabelsFrame.Parent := EditLabelsPanel;
  FEditLabelsFrame.Align := alClient;
  FEditLabelsFrame.CloseBtn.OnClick := CloseEditLabelsPanel;
  FEditLabelsFrame.ApplyBtn.OnClick := ApplySiteLabelsEdit;
  FEditLabelsFrame.HighlighLabelledSitesCBox.OnChange := ToggleHighlightLabelledSites;
  FEditLabelsFrame.SitesMemo.OnEnter := LabelledSitesEditOnEnter;
  FEditLabelsFrame.SitesMemo.OnExit := LabelledSitesEditOnExit;
  FEditLabelsFrame.LabelEdit.OnEnter := LabelledSitesEditOnEnter;
  FEditLabelsFrame.LabelEdit.OnExit := LabelledSitesEditOnExit;
  FUpdatingSiteIndex := False;
  FForcingHighlightUpdate := False;
  UpdateShortcutsForMacOs(ActionList1);
  FRuntimeProg := nil;
  FIsClosing := False;
  FEmptyInfos := TList.Create;
  bird.Selection := tGridRect(Rect(-1,-1,-1,-1));
  tempDomainStart := -1;
  CheckMark   := TBitMap.Create;
  UnCheckMark := TBitMap.Create;
  MenuItemCheckmark := TBitmap.Create;
  if CompareValue(scalingFactor, 1.5, FP_CUTOFF) >= 0 then
  begin
    UnCheckMark.SetSize(CheckBoxes32.Width, CheckBoxes32.Height);
    CheckMark.SetSize(CheckBoxes32.Width, CheckBoxes32.Height);
    MenuItemCheckMark.SetSize(CheckBoxes32.Width, CheckBoxes32.Height);
    CheckBoxes32.GetBitmap(CHECKMARK_IMAGE_INDEX, CheckMark);
    CheckBoxes32.GetBitmap(UNCHECKMARK_IMAGE_INDEX, UnCheckMark);
    CheckBoxes32.GetBitmap(MENUITEM_CHECKMARK_IMAGE_INDEX, MenuItemCheckmark);
  end
  else
  begin
    UnCheckMark.SetSize(ImageList2.Width, ImageList2.Height);
    CheckMark.SetSize(ImageList2.Width, ImageList2.Height);
    MenuItemCheckMark.SetSize(ImageList2.Width, ImageList2.Height);
    ImageList2.GetBitmap(CHECKMARK_IMAGE_INDEX, CheckMark);
    ImageList2.GetBitmap(UNCHECKMARK_IMAGE_INDEX, UnCheckMark);
    ImageList2.GetBitmap(MENUITEM_CHECKMARK_IMAGE_INDEX, MenuItemCheckmark);
  end;


  ColorDialog1.Color := VS_SeqDataExplorer.HighlightColor;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Sequence Data Explorer (' + ExtractFileName(GetActiveDataFileName) + ')';
  Cols := TColList.Create(Self);
  Cols.Add('Name', true);
  Cols.Add('Group', true);
  ActionExportSearchResults.Visible := IsDeveloper;
  ActionHomology.Visible := IsDeveloper;
  ActionMismatchDistribution.Visible := IsDeveloper;
  MainSearchPanel.Visible := False;
  if (not IsPrototyper) and (not MegaForm.ExplorersDisabledForLargeData) then
    AddWindowToTray(Self);
  FGeneDomainsPropsEditor := nil;
  ActionDispTaxaNames.Checked := True;
  VS_SeqDataExplorer.DispTaxaNamesItem := True;
  DataGrid.ColCount := 1;
  Datagrid.FixedCols := 1;
  {$IFNDEF DARWIN}
  InitMainMenu;
  InitPopupMenus;
  {$ENDIF}
  ImageForm.UpdateImgList(Self);
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
  DataGrid.AllowOutboundEvents := False;
  //ActionToggleUnlabeledSites.Visible := False;
  //ActionAutoLabelSites.Visible := False;
  //ActionClearSiteLabels.Visible := False;
  //ActionSelectEarliestDatedSeqInEachGroup.Visible := False;
  //ActionSelectLargestGroups.Visible := False;
  //ActionSelectOneFromEachGroup.Visible := False;
  //ActionUnselectSmallGroups.Visible := False;
  //ActionUnselectUngrouped.Visible := False;
  ActionAddDomainsForGroupIdentityComparisonSites.Visible := False;
  ActionDisableIndependentSites.Visible := False;
  ActionLabelSitesUsedForGroupIdentityComparison.Visible := False;
  ActionGroupSeqsByIdentity.Visible := False;
  //N2.Visible := False;
  N3.Visible := False;
end;

procedure TV_SeqDataExplorer.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(FRuntimeProg) then
    FRuntimeProg.Free;
  FIsClosing := True;
  RemoveWindowFromTray(Self);
  if Assigned(Checkmark) then
    FreeAndNil(Checkmark);
  if Assigned(UnCheckmark) then
    FreeAndNil(UnCheckmark);
  if Assigned(MenuItemCheckmark) then
    MenuItemCheckmark.Free;
  if Assigned(Cols) then
    Cols.Free;
  if Assigned(FEmptyInfos) then
  begin
    if FEmptyInfos.Count > 0 then
      for i := 0 to FEmptyInfos.Count - 1 do
        TDomainInfo(FEmptyInfos[i]).Free;
    FEmptyInfos.Clear;
    FEmptyInfos.Free;
  end;
  if Assigned(GeneIcon) then
    GeneIcon.Free;
  if Assigned(DomainIcon) then
    DomainIcon.Free;
  if Assigned(FGeneDomainsPropsEditor) then
    FGeneDomainsPropsEditor.Free;
  if Assigned(FGroupCounts) then
    ClearGroupCountsList;
end;

function TV_SeqDataExplorer.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
var
  helpTopic: String;
begin
  try
    Result := True;
    helpTopic := MapHelpContextToKeyword(Data);
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

procedure TV_SeqDataExplorer.FormMouseEnter(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

procedure TV_SeqDataExplorer.FormResize(Sender: TObject);
begin
  if FIsClosing then  { if the form is being destroyed, we don't want to access freed objects}
    Exit;
  InitScrollBars(False);
  updateBirdsGeneDomain;
  dataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.FormShow(Sender: TObject);
var
  i, totalHeight, maxHeight: Integer;
begin
  if not D_InputSeqData.HasGroups then
    if ActionDispGroupNames.Checked then
      ActionDispGroupNamesExecute(Self);

  // calculate the height of the DataGrid (to remove white space)
  totalHeight := 0;
  for i := 0 to DataGrid.RowCount-1 do
    totalHeight := totalHeight + DataGrid.RowHeights[i];
  maxHeight := Floor(Screen.Height * (0.3));
  if totalHeight > maxHeight then
    totalHeight := maxHeight;
  Self.Height := Self.Height - DataGrid.Height + totalHeight + 50; // 50 px padding.

  if evodNmid <> EmptyStr then
    DiagnoseButton.Visible := true;

  if D_InputSeqData.DomainMarks.NoOfDomains > 1 then
  begin
    ActionDispBirdsEyeView.Checked := True;
    VS_SeqDataExplorer.DispBirdsEyeViewItem := True;
    bird.Visible := True;
  end
  else
  begin
    ActionDispBirdsEyeView.Checked := False;
    VS_SeqDataExplorer.DispBirdsEyeViewItem := False;
    bird.Visible := False;
  end;
  UpdateGridSizes;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.HighlightSitesClick(Sender: TObject);
var
  IsSpecial: Boolean = False;
  CoverageStr: String = '100';
  CoveragePerc: Integer = 100;
  QueryBool: Boolean;
begin
  if FIsClosing or (not Assigned(D_InputSeqData)) then
    Exit;
  UncheckSpecial;
  UncheckSpecific;
  VS_SeqDataexplorer.HighlightItem := Sender;
  VS_SeqDataExplorer.CurAttrDisp := megNone;
  if (Sender <> nil) then
  begin
    FHasDataViewChanged := True;

    if (ActionHighlightConserved = Sender) then
    begin
      VS_SeqDataExplorer.ConstantItem := (not VS_SeqDataExplorer.ConstantItem);
      if VS_SeqDataExplorer.ConstantItem then
        VS_SeqDataExplorer.CurAttrDisp := megConst;
    end
    else if (ActionHighlightVariableSites = Sender) then
    begin
      VS_SeqDataExplorer.VariableItem := (not VS_SeqDataExplorer.VariableItem);
      if VS_SeqDataExplorer.VariableItem then
        VS_SeqDataExplorer.CurAttrDisp := megVar;
    end
    else if (ActionHighlightParsimInfoSites = Sender) then
    begin
      VS_SeqDataExplorer.ParsimInfoItem := (not VS_SeqDataExplorer.ParsimInfoItem);
      if VS_SeqDataExplorer.ParsimInfoItem then
        VS_SeqDataExplorer.CurAttrDisp := megParsimInfo;
    end
    else if (ActionHighlightLabelledSites = Sender) then
    begin
      VS_SeqDataExplorer.LabelledItem := (not VS_SeqDataExplorer.LabelledItem);
      if VS_SeqDataExplorer.LabelledItem then
        VS_SeqDataExplorer.CurAttrDisp := megLabelled;
    end
    else if (ActionHighlightSingletonSites = Sender) then
    begin
      VS_SeqDataExplorer.SingletonItem := (not VS_SeqDataExplorer.SingletonItem);
      if VS_SeqDataExplorer.SingletonItem then
        VS_SeqDataExplorer.CurAttrDisp := megSingleton;
    end
    else if (ActionHighlight0FoldDegenSites = Sender) then
    begin
      VS_SeqDataExplorer.ZeroFoldItem := (not VS_SeqDataExplorer.ZeroFoldItem);
      if VS_SeqDataExplorer.ZeroFoldItem then
        VS_SeqDataExplorer.CurAttrDisp := meg0Fold;
    end
    else if (ActionHighlight2FoldDegenSites = Sender) then
    begin
      VS_SeqDataExplorer.TwoFoldItem := (not VS_SeqDataExplorer.TwoFoldItem);
      if VS_SeqDataExplorer.TwoFoldItem then
        VS_SeqDataExplorer.CurAttrDisp := meg2Fold;
    end
    else if (ActionHighlight4FoldDegenSites = Sender) then
    begin
      VS_SeqDataExplorer.FourFoldItem := (not VS_SeqDataExplorer.FourFoldItem);
      if VS_SeqDataExplorer.FourFoldItem then
        VS_SeqDataExplorer.CurAttrDisp := meg4Fold;
    end
    else if (ActionCoverage = Sender) then
    begin
      VS_SeqDataExplorer.SpecialItem := True;
      VS_SeqDataExplorer.CurAttrDisp := megCoverage;
      IsSpecial := True;
    end
    else if (ActionCpG = Sender) then
    begin
      VS_SeqDataExplorer.SpecialItem := True;
      VS_SeqDataExplorer.CurAttrDisp := megCpG;
      IsSpecial := True;
    end;
  end
  else
    ClearHighlightButtonStates;

  if (IsSpecial) then
  begin
    SpecialButton.Down := True;
    D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateSpecial];
    Repeat
      QueryBool := InputQuery('Coverage Percentage', 'Percentage 1-100', CoverageStr);
      if QueryBool = False then
       begin
         if Sender = ActionCoverage then
           CoveragePerc := 0;
         if Sender = ActionCpG then
           CoveragePerc := 100;
         Exit;
       end;
    until ( not QueryBool) or (TryStrToInt(CoverageStr, CoveragePerc) and (CoveragePerc >= 1) and (CoveragePerc <= 100));
    ActionCoverage.Caption := 'Coverage (' + IntToStr(CoveragePerc) + '%)';
    VS_SeqDataExplorer.CoveragePerc := CoveragePerc;
  end;

  // Now I am ready to highlight
  try
    FForcingHighlightUpdate := True;
    ApplyUpdate;
  finally
    FForcingHighlightUpdate := False;
  end;
  With StatusBar do
  begin
    if VS_SeqDataExplorer.CurAttrDisp <> megNone then
    begin
      D_InputSeqData.UpdateSiteAttrArray;
      ActionHighlightedSites.Enabled := True;
    end
    else
    begin
      ActionAllSelectedSites.checked := True;
      ActionHighlightedSites.Enabled := False;
      ActionHighlightedSites.Checked := False;
    end;

    VS_SeqDataExplorer.StatUseOnlyMarkedSitesItem := ActionHighlightedSites.Checked;

    case VS_SeqDataExplorer.CurAttrDisp of
      megConst:      VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites] := 'Conserved: '   + IntToStr(D_InputSeqData.FNoOfAttrSites[VS_SeqDataExplorer.CurAttrDisp])+'/'+IntToStr(D_InputSeqData.NoOfSitesUsed);
      megVar:        VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites] := 'Variable: '    + IntToStr(D_InputSeqData.FNoOfAttrSites[VS_SeqDataExplorer.CurAttrDisp])+'/'+IntToStr(D_InputSeqData.NoOfSitesUsed);
      megParsimInfo: VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites] := 'Parsim-info: ' + IntToStr(D_InputSeqData.FNoOfAttrSites[VS_SeqDataExplorer.CurAttrDisp])+'/'+IntToStr(D_InputSeqData.NoOfSitesUsed);
      megSingleton:  VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites] := 'Singleton: '   + IntToStr(D_InputSeqData.FNoOfAttrSites[VS_SeqDataExplorer.CurAttrDisp])+'/'+IntToStr(D_InputSeqData.NoOfSitesUsed);
      meg0Fold:      VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites] := 'Zero-fold: '   + IntToStr(D_InputSeqData.FNoOfAttrSites[VS_SeqDataExplorer.CurAttrDisp])+'/'+IntToStr(D_InputSeqData.NoOfCodingSitesUsed);
      meg2Fold:      VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites] := 'Two-fold: '    + IntToStr(D_InputSeqData.FNoOfAttrSites[VS_SeqDataExplorer.CurAttrDisp])+'/'+IntToStr(D_InputSeqData.NoOfCodingSitesUsed);
      meg4Fold:      VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites] := 'Four-fold: '   + IntToStr(D_InputSeqData.FNoOfAttrSites[VS_SeqDataExplorer.CurAttrDisp])+'/'+IntToStr(D_InputSeqData.NoOfCodingSitesUsed);
      megCoverage:   VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites] := 'Coverage (' + IntToStr(VS_SeqDataExplorer.CoveragePerc) + '%): ' + IntToStr(D_InputSeqData.FNoOfAttrSites[VS_SeqDataExplorer.CurAttrDisp])+'/'+IntToStr(D_InputSeqData.NoOfSitesUsed);
      megCpG:        VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites] := 'CpG: '   + IntToStr(D_InputSeqData.FNoOfAttrSites[VS_SeqDataExplorer.CurAttrDisp])+'/'+IntToStr(D_InputSeqData.NoOfCodingSitesUsed);
      megLabelled:   VS_SeqDataExplorer.CurStatusBarStatistics[stMarkedSites] := Format('Labelled: %d/%d', [D_InputSeqData.FNoOfAttrSites[VS_SeqDataExplorer.CurAttrDisp], D_InputSeqData.NoOfsitesUsed]);
    else
      VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stMarkedSites] := 'Highlighted: None';
    end;
    DataGrid.Invalidate;
  end;
  if (VS_SeqDataExplorer.CurAttrDisp <> megNone) and (D_InputSeqData.FNoOfAttrSites[VS_SeqDataExplorer.CurAttrDisp] < 1) then
  begin
    VS_SeqDataExplorer.CurAttrDisp := megNone;
    HighlightSitesClick(nil);
  end;
  UpdateHighlightButtonStates;
  ActionNextHighlight.Enabled := VS_SeqDataExplorer.CurAttrDisp <> megNone;
  ActionPreviousHighlight.Enabled := VS_SeqDataExplorer.CurAttrDisp <> megNone;
  FCurrentHighlightedSite := -1;
  D_InputSeqData.HideProgressDlg;
end;

procedure TV_SeqDataExplorer.HintReleaseTimer(Sender: TObject);
var
  CurPos, CurClientPos: TPoint;
  Col: Integer = -1;
  Row: Integer = -1;
  HintRect: TRect;
begin
  CurPos := TPoint.Create(0, 0);
  if CurDesc = '' then Exit;
  GetCursorPos(CurPos);
  CurClientPos := DataGrid.ScreenToClient(CurPos);
  DataGrid.MouseToCell(CurClientPos.X, CurClientPos.Y, Col, Row);
  if Row = FLastRow then
  begin
    if CurHint = nil then
      CurHint := THintWindow.Create(Self);
    HintRect := Rect(CurPos.X +10, CurPos.Y+10, CurPos.X + CurHint.Canvas.TextWidth(CurDesc) + 20, CurPos.Y + 15 + 10);  // Left Top Right Bottom

    CurHint.ActivateHint(HintRect, CurDesc);
  end;
  HintTimer.Enabled := false;
  HintRelease.Enabled := true;
end;

procedure TV_SeqDataExplorer.HintTimerTimer(Sender: TObject);
var
  CurPos, CurClientPos: TPoint;
  Col: Integer = -1;
  Row: Integer = -1;
  HintRect: TRect;
begin
  if CurDesc = '' then Exit;
  CurPos := TPoint.Create(-1, -1);
  GetCursorPos(CurPos);
  CurClientPos := DataGrid.ScreenToClient(CurPos);
  DataGrid.MouseToCell(CurClientPos.X, CurClientPos.Y, Col, Row);
  if Row = FLastRow then
  begin
    if CurHint = nil then
      CurHint := THintWindow.Create(Self);
    HintRect := Rect(CurPos.X +10, CurPos.Y+10, CurPos.X + CurHint.Canvas.TextWidth(CurDesc) + 20, CurPos.Y + 15 + 10);  // Left Top Right Bottom

    CurHint.ActivateHint(HintRect, CurDesc);
  end;
  HintTimer.Enabled := false;
  HintRelease.Enabled := true;
end;

procedure TV_SeqDataExplorer.HorizScrollbarChange(Sender: TObject);
var
  mouseLeftButtonDown: Boolean = False;
begin
  if FSelfScrolling then
    Exit;
  try
    { begin workaround to stop the scrollbar from chasing the cursor when the mouseup event is lost}
    mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
    if (not mouseLeftButtonDown) and FScrollStarted then
    begin
      SendMessage(HorizScrollbar.Handle, WM_CANCELMODE, WM_CANCELMODE, 0);
      FScrollStarted := False;
      Exit;
    end;
    { end workaround}

    FHScrollPos := HorizScrollbar.Position;
    if (FHScrollPos >= DataGrid.FixedCols) and (FHScrollPos <= DataGrid.ColCount) then
    begin
      DataGrid.LeftCol := min(FHScrollPos, DataGrid.ColCount - DataGrid.VisibleColCount);
      FScrollStarted := True;
    end;
  except
    FScrollStarted := False;
  end;
end;

procedure TV_SeqDataExplorer.HorizScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  mouseLeftButtonDown: Boolean = False;
begin
  mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
  if (not mouseLeftButtonDown) and FScrollStarted then
    ScrollPos := FHScrollPos;
  if ScrollCode = scEndScroll then
    FScrollStarted := False;
  inherited;
end;

procedure TV_SeqDataExplorer.MoveToTopItemClick(Sender: TObject);
var
  aRow: Integer = -1;
begin
  aRow := FMouseGridCoords.Y;
  if (aRow >= 1) and (aRow <= D_InputSeqData.FOtuInfos.NoOfOtus) then
  begin
    D_InputSeqData.FOtuInfos.Move(aRow - 1, 0);
    DataGrid.Invalidate;
  end;
end;

procedure TV_SeqDataExplorer.MoveToBottomItemClick(Sender: TObject);
var
  aCell: TPoint;
  aCol: Integer = -1;
  aRow: Integer = -1;

begin
  aRow := FMouseGridCoords.Y;
  if (aRow >= 1) and (aRow <= D_InputSeqData.FOtuInfos.NoOfOtus) then
  begin
    D_InputSeqData.FOtuInfos.Move(aRow - 1, D_InputSeqData.FOtuInfos.NoOfOtus - 1);
    DataGrid.Invalidate;
  end;
end;

procedure TV_SeqDataExplorer.SearchBoxChange(Sender: TObject);
begin
  resetSearchBtns;
end;

procedure TV_SeqDataExplorer.SearchBoxEnter(Sender: TObject);
begin
  if (SearchBox.Text = 'Search') or (SearchBox.Text = 'Search for a Motif') then
    SearchBox.Text := EmptyStr;
end;

procedure TV_SeqDataExplorer.SearchBtnClick(Sender: TObject);
begin
  try
    case searchTypeCbx.ItemIndex of
      // Name
      0:
      begin
        VS_SeqDataExplorer.CurSearchStr := SearchBox.Text;
        ActionFindSeqExecute(Sender);
      end;
      // Group
      1:
      begin
        VS_SeqDataExplorer.CurGroupStr := SearchBox.Text;
        SearchGroup;
      end;
      // Motif
      2:
      begin
         VS_SeqDataExplorer.CurMotifStr := UpperCase(SearchBox.Text);
        SearchMotif;
      end;
    end;
  except
    on E:Exception do
      ShowMessage('Application error: ' + E.Message);
  end;
end;

function TV_SeqDataExplorer.SearchNextSingleMotif(SearchingMotif: String; StartAtRow: Integer; StartAtCol: Integer): Boolean;
var
  i, j: Integer;
  ResultFound: Boolean;
  APW: TPleaseWait = nil;
begin
  Result := false;
  ResultFound := False;
  if Length(SearchingMotif) > D_InputSeqData.NoOfSites then
    raise Exception.Create('search motif cannot be longer than the sequence alignment to search');
  if StartAtCol > (D_InputSeqData.NoOfSites-1-Length(SearchingMotif)) then
    StartAtCol := D_InputSeqData.NoOfSites-Length(SearchingMotif);

  if D_InputSeqData.TotalNumBases >= MIN_BASES_FOR_SHOWING_PLEASE_WAIT then
  begin
    APW := TPleaseWait.Create(nil);
    APW.Action := 'Searching for next motif';
    APW.show;
  end;

  try
    ActionFindNextMotif.Enabled := false;
    SearchBtn.Enabled := False;
    SearchBox.Enabled := False;
    for i:=StartAtRow to D_InputSeqData.NoOfTaxa-1 do
    begin
      if Assigned(APW) then
        APW.PercentDone := ((i-StartAtRow)*100) div (D_InputSeqData.NoOfTaxa - StartAtRow);
      if not D_InputSeqData.OtuInfos.Otu[i].IsUsed then
        Continue;
      for j:=StartAtCol to Min((D_InputSeqData.NoOfSites-Length(SearchingMotif)+1), D_InputSeqData.NoOfsites - 1) do
      begin
        if not D_InputSeqData.SiteUsed[j] then
        begin
          Continue;
        end;
        if D_InputSeqData.Sequence[i][j] = '-' then
          continue;
        if D_InputSeqData.CheckKMer(SearchingMotif, i, j, 0, False) then
        begin
          ResultFound := True;
        end;
        if ResultFound then Break;
      end;
      StartAtCol := 0;
      if ResultFound then Break;
    end;

    if ResultFound then
    begin
      DataGrid.Row := i+1;
      DataGrid.Col := j+Cols.VisibleCount;
      DataGrid.Selection := Rect(DataGrid.Col, DataGrid.Row, DataGrid.Col, DataGrid.Row);
      // make sure that it isn't off the edge of the page, and that ALL of the text is visible.
      if (j+Cols.VisibleCount+length(SearchingMotif)) > (DataGrid.LeftCol + DataGrid.VisibleColCount - Cols.VisibleCount) then
        DataGrid.LeftCol := j+cols.VisibleCount - DataGrid.VisibleColCount + length(SearchingMotif); //(J+cols.VisibleCount-1);
      Result := True;
    end
    else
    begin
      if Assigned(APW) then
        APW.Hide;
      MessageDlg('There are no more matches to the motif.', mtWarning, [mbOK], 0);
    end;
  finally
    ActionFindNextMotif.Enabled := True;
    SearchBtn.Enabled := True;
    SearchBox.Enabled := True;
    if Assigned(APW) then
      APW.Free;
    DataGrid.Invalidate;
  end;
end;

procedure TV_SeqDataExplorer.searchTypeCbxChange(Sender: TObject);
begin
  resetSearchBtns;

  if searchTypeCBx.ItemIndex <> 0 then
  begin
    VS_SeqDataExplorer.CurSearchStr := EmptyStr;
    SeqNameResult := EmptyStr;
    VS_SeqDataExplorer.CurColorRowRange := Point(-1, -1);
  end;

  if searchTypeCBx.ItemIndex <> 1 then
  begin
    VS_SeqDataExplorer.CurGroupStr := EmptyStr;
    SeqNameResult := EmptyStr;
    VS_SeqDataExplorer.CurColorRowRange := Point(-1, -1);
  end;

  if searchTypeCBx.ItemIndex <> 2 then
    VS_SeqDataExplorer.CurMotifStr := EmptyStr;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ShowHelpActionExecute(Sender: TObject);
begin
  ShowContextSensitiveHelp(HelpKeyword);
end;

procedure TV_SeqDataExplorer.SiteNumSpinEditChange(Sender: TObject);
var
  site: Integer = -1;
begin
  if not FUpdatingSiteIndex then
  begin
    try
      FUpdatingSiteIndex := True;
      if SiteNumSpinEdit.Value + NumberOfFixedColsVisible - 1 < DataGrid.ColCount then
        DataGrid.Col := SiteNumSpinEdit.Value + NumberOfFixedColsVisible - 1;

      if DiagnoseButton.Visible then
      begin
        site := DataGrid.Col - Cols.VisibleCount + 1;
        if VS_SeqDataExplorer.CurColorSite <> site then
          VS_SeqDataExplorer.CurColorSite := site;
      end;
    finally
      FUpdatingSiteIndex := False;
    end;
  end;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.SiteNumSpinEditEnter(Sender: TObject);
begin
  DisableActionConflictingWithNumberKeys;
end;

procedure TV_SeqDataExplorer.SiteNumSpinEditExit(Sender: TObject);
begin
  EnableActionsConflictingWithNumberKeys;
  try
    FUpdatingSiteIndex := True;
    if DataGrid.Col > NumberOfFixedColsVisible then
      SiteNumSpinEdit.Value := DataGrid.Col - NumberOfFixedColsVisible + 1;
  finally
    FUpdatingSiteIndex := False;
  end;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.SpecialButtonClick(Sender: TObject);
var
  p: TPoint;
begin
  p.x := SpecialButton.Left + 2;
  p.y :=  MainToolbar.Height + 2;
  p := ClientToScreen(p);
  SpecialMenu.PopUp(p.x, p.y);
end;

procedure TV_SeqDataExplorer.SetGroupItemClick(Sender: TObject);
var
  otuInfos: TAllOtuInfo = nil;
begin
  if FUpdatingGroupTagMenus then
    Exit;
  UncheckGroupTagMenus;
  otuInfos := D_InputSeqData.OtuInfos;
  if (Sender = GroupItem) or (Sender = GroupItem2) then
  begin
    otuInfos.SetGroupSource(gtiRsv);
    GroupItem.Checked := True;
    GroupItem2.Checked := True;
  end
  else if (Sender = SpeciesItem) or (Sender = SpeciesItem2) then
  begin
    otuInfos.SetGroupSource(gtiSpecies);
    SpeciesItem.Checked := True;
    SpeciesItem2.Checked := True;
  end
  else if (Sender = PopulationItem) or (Sender = PopulationItem2) then
  begin
    otuInfos.SetGroupSource(gtiPopulation);
    PopulationItem.Checked := True;
    PopulationItem2.Checked := True;
  end
  else if (Sender = ContinentItem) or (Sender = ContinentItem2) then
  begin
    otuInfos.SetGroupSource(gtiContinent);
    ContinentItem.Checked := True;
    ContinentItem2.Checked := True;
  end
  else if (Sender = CountryItem) or (Sender = CountryItem2) then
  begin
    otuInfos.SetGroupSource(gtiCountry);
    CountryItem.Checked := True;
    CountryItem2.Checked := True;
  end
  else if (Sender = CityItem) or (Sender = CityItem2) then
  begin
    otuInfos.SetGroupSource(gtiCity);
    CityItem.Checked := True;
    CityItem2.Checked := True;
  end
  else if (Sender = YearItem) or (Sender = YearItem2) then
  begin
    otuInfos.SetGroupSource(gtiYear);
    YearItem.Checked := True;
    YearItem2.Checked := True;
  end
  else if (Sender = MonthItem) or (Sender = MonthItem2) then
  begin
    otuInfos.SetGroupSource(gtiMonth);
    MonthItem.Checked := True;
    MonthItem2.Checked := True;
  end
  else if (Sender = DayItem) or (Sender = DayItem2) then
  begin
    otuInfos.SetGroupSource(gtiDay);
    DayItem.Checked := True;
    DayItem2.Checked := True;
  end
  else if (Sender = TimeItem) or (Sender = TimeItem2) then
  begin
    otuInfos.SetGroupSource(gtiTime);
    TimeItem.Checked := True;
    TimeItem2.Checked := True;
  end;
  ResetGroupCounts;
end;

procedure TV_SeqDataExplorer.VertScrollbarChange(Sender: TObject);
var
  mouseLeftButtonDown: Boolean = False;
begin
  if FSelfScrolling then Exit;

  try
    { begin workaround to stop the scrollbar from chasing the cursor when the mouseup event is lost}
    mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
    if (not mouseLeftButtonDown) and FScrollStarted then
    begin
      SendMessage(VertScrollbar.Handle, WM_CANCELMODE, WM_CANCELMODE, 0);
      FScrollStarted := False;
      Exit;
    end;
    { end workaround}
    FVScrollPos := VertScrollbar.Position;
    if (FVScrollPos >= DataGrid.FixedRows) and (FVScrollPos <= DataGrid.RowCount) then
    begin
      DataGrid.TopRow := min(FVScrollPos, DataGrid.RowCount - DataGrid.VisibleRowCount);
      FScrollStarted := True;
    end;
  except
    FScrollStarted := False;
  end;
end;

procedure TV_SeqDataExplorer.VertScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  mouseLeftButtonDown: Boolean = False;
begin
  mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
  if (not mouseLeftButtonDown) and FScrollStarted then
  begin
    ScrollPos := FVScrollPos;
  end;

  if ScrollCode = scEndScroll then
    FScrollStarted := False;
  inherited;
end;

procedure TV_SeqDataExplorer.EnableDisableGroupTagItems;
var
  hasGp: Boolean = False;
  hasSp: Boolean = False;
  hasPop: Boolean = False;
  hasCont: Boolean = False;
  hasCoun: Boolean = False;
  hasCity: Boolean = False;
  hasYear: Boolean = False;
  hasMonth: Boolean = False;
  hasDay: Boolean = False;
  hasTime: Boolean = False;
begin
  D_InputSeqData.OtuInfos.GetHasGeoInfoAllAtOnce(hasGp, hasSp, hasPop, hasCont, hasCoun, hasCity, hasTime, hasMonth, hasDay, hasYear);
  GroupItem2.Enabled := hasGp;
  SpeciesItem2.Enabled := hasSp;
  PopulationItem2.Enabled := hasPop;
  ContinentItem2.Enabled := hasCont;
  CountryItem2.Enabled := hasCoun;
  CityItem2.Enabled := hasCity;
  YearItem2.Enabled := hasYear;
  MonthItem2.Enabled := hasMonth;
  DayItem2.Enabled := hasDay;
  TimeItem2.Enabled := hasTime;

  GroupItem.Enabled := hasGp;
  SpeciesItem.Enabled := hasSp;
  PopulationItem.Enabled := hasPop;
  ContinentItem.Enabled := hasCont;
  CountryItem.Enabled := hasCoun;
  CityItem.Enabled := hasCity;
  YearItem.Enabled := hasYear;
  MonthItem.Enabled := hasMonth;
  DayItem.Enabled := hasDay;
  TimeItem.Enabled := hasTime;
end;

function TV_SeqDataExplorer.ValidSelectableCol(aCol: Integer): Integer;
begin
  Result := aCol;
  if aCol < DataGrid.FixedCols then
    Result := DataGrid.FixedCols
  else if aCol >= DataGrid.ColCount then
    Result := DataGrid.ColCount - 1;
end;

function TV_SeqDataExplorer.ValidSelectableRow(aRow: Integer): Integer;
begin
  Result := aRow;
  if aRow < DataGrid.FixedRows then
    Result := DataGrid.FixedRows
  else if aRow >= DataGrid.RowCount then
    Result := DataGrid.RowCount - 1;
end;

function TV_SeqDataExplorer.ColumnVisible(aCol: Integer): Boolean;
begin
  Result := False;
  if aCol < DataGrid.LeftCol then
    Exit;
  if aCol > (DataGrid.LeftCol + DataGrid.VisibleColCount) then
    Exit;
  Result := True;
end;

function TV_SeqDataExplorer.RowVisible(aRow: Integer): Boolean;
begin
  Result := False;
  if aRow < DataGrid.TopRow then
    Exit;
  if aRow > (DataGrid.TopRow + DataGrid.VisibleRowCount) then
    Exit;
  Result := True;
end;

procedure TV_SeqDataExplorer.InitScrollBars(setPosition: Boolean);
var
  p: Integer;
  aPosition: Integer = -1;
begin
  if setPosition then
    aPosition := 1
  else
    aPosition := HorizScrollbar.Position;
  if DataGrid.ColCount > (DataGrid.VisibleColCount + DataGrid.FixedCols) then
  begin
    HorizScrollbar.Visible := True;
    HorizScrollbar.SmallChange := 1;
    HorizScrollbar.LargeChange := max(DataGrid.VisibleColCount, HorizScrollbar.SmallChange);
    if DataGrid.ColCount > DataGrid.VisibleColCount then
      p := max(1, DataGrid.VisibleColCount - 1)
    else
      p := 1;
    HorizScrollbar.SetParams(aPosition, 1, DataGrid.ColCount - DataGrid.FixedCols, max(1, p));
  end
  else
    HorizScrollbar.Visible := False;

  if setPosition then
    aPosition := 1
  else
    aPosition := VertScrollbar.Position;
  if DataGrid.RowCount > (DataGrid.VisibleRowCount + DataGrid.FixedRows) then
  begin
    VertScrollbar.Visible := True;
    VertScrollbar.SmallChange := 1;
    VertScrollbar.LargeChange := max(DataGrid.VisibleRowCount, VertScrollbar.SmallChange);
    if DataGrid.RowCount > DataGrid.VisibleRowCount then
      p := max(1, DataGrid.VisibleRowCount - 1)
    else
      p := 1;
    VertScrollbar.SetParams(aPosition, 1, DataGrid.RowCount - DataGrid.FixedRows, max(1, p));
  end
  else
    VertScrollbar.Visible := False;
end;

function TV_SeqDataExplorer.GetNumSeqs: Integer;
begin
  if Assigned(D_InputSeqData) then
    Result := D_InputSeqData.NoOfTaxa
  else
    Result := 0;
end;

function TV_SeqDataExplorer.GetNumSites: Integer;
begin
  if Assigned(D_InputSeqData) then
    Result := D_InputSeqData.NoOfSites
  else
    Result := 0;
end;

function TV_SeqDataExplorer.GroupTagCheckedIndex: Integer;
var
  i: Integer;
begin
  Result := 0;
  if SetGroupTagItem.Count > 0 then
    for i := 0 to SetGroupTagItem.Count - 1 do
      if SetGroupTagItem.Items[i].Checked then
      begin
        Result := i;
        Exit;
      end;
end;

procedure TV_SeqDataExplorer.ParseTipDatesThreadDone(aThread: TObject);
var
  t: TParseTipDatesThread = nil;
begin
  try
    try
      if not (aThread is TParseTipDatesThread) then
        raise Exception.Create(Format('Expected TParseTipDatesThread but got %s', [aThread.ClassName]));
      t := TParseTipDatesThread(aThread);
      if Assigned(t.RuntimeProgress) then
        t.RuntimeProgress.Hide;
      if t.IsCancelled then
      begin
        ShowMessage('Timestamp search in sequence names aborted');
        Exit;
      end;

      if not t.IsSuccess then
        ShowMessage('Searching for timestamps in sequence names returned the following messsage: ' + t.LogText);
      SelectEarliestDatedSequenceInEachGroup;
    except
      on E:Exception do
        ShowMessage('Application Error: ' + E.Message);
    end;
  finally
    if Assigned(t.RuntimeProgress) then
      t.RuntimeProgress.Free;
    DataGrid.Invalidate;
  end;
end;

procedure TV_SeqDataExplorer.SelectEarliestDatedSequenceInEachGroup;
var
  sortedGroups: TFPHashList = nil;
  i: Integer;
  aList: TList;
  index: Integer = 0;
  foundValidOtu: Boolean = False;
  otu: TOtuInfo = nil;

  function HasSelectedOtu(group: TList): Boolean;
  var
    j: Integer;
  begin
    Result := False;
    if group.Count = 0 then
      Exit;
    for j := 0 to group.Count - 1 do
      if TOtuInfo(group[j]).IsUsed then
      begin
        Result := True;
        Exit;
      end;
  end;

begin
  try
    try
      sortedGroups := D_InputSeqData.GetGroupsSortedByTime;
      if sortedGroups.Count > 0 then
        for i := 0 to sortedGroups.Count - 1 do
        begin
          aList := TList(sortedGroups.Items[i]);
          if HasSelectedOtu(aList) then
          begin
            index := 0;
            foundValidOtu := False;
            while (index < aList.Count) do
            begin
              otu := TOtuInfo(aList[index]);
              if otu.HasDateInfo and (not foundValidOtu) then
              begin
                otu.IsUsed := True;
                foundValidOtu := True;
              end
              else
                otu.IsUsed := False;
              inc(index);
            end;
          end;
        end;
      UpdateStatusBar(nil);
    except
      on E:Exception do
        ShowMessage('Application error when selecting earliest dated grouped sequences: ' + E.Message);
    end;
  finally
    DataGrid.Invalidate;
    if Assigned(sortedGroups) then
    begin
      if sortedGroups.Count > 0 then
        for i := 0 to sortedGroups.Count - 1 do
        begin
          aList := TList(sortedGroups.Items[i]);
          aList.Clear;
          aList.Free;
        end;
      sortedGroups.Free;
    end;
  end;
end;

procedure TV_SeqDataExplorer.ClearGroupCountsList;
var
  gc: TGroupCount = nil;
  i: Integer;
begin
  if FGroupCounts.Count > 0 then
    for i := FGroupCounts.Count - 1 downto 0 do
    begin
      gc := TGroupCount(FGroupCounts[i]);
      gc.Free;
      FGroupCounts.Delete(i);
    end;
  FGroupCounts.Clear;
  FreeAndNil(FGroupCounts);
end;

procedure TV_SeqDataExplorer.ResetGroupCounts;
begin
  if Assigned(FGroupCounts) then
    ClearGroupCountsList;
  FGroupCounts := D_InputSeqData.GetGroupCounts;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.DisableActionConflictingWithNumberKeys;
begin
  ActionHighlight0FoldDegenSites.Enabled := False;
  ActionHighlight2FoldDegenSites.Enabled := False;
  ActionHighlight4FoldDegenSites.Enabled := False;
end;

procedure TV_SeqDataExplorer.EnableActionsConflictingWithNumberKeys;
begin
  ActionHighlight0FoldDegenSites.Enabled := D_InputSeqData.FIsNuc and D_InputSeqData.FIsCoding and (not VS_SeqDataExplorer.IsTranslated);
  ActionHighlight2FoldDegenSites.Enabled := D_InputSeqData.FIsNuc and D_InputSeqData.FIsCoding and (not VS_SeqDataExplorer.IsTranslated);
  ActionHighlight4FoldDegenSites.Enabled := D_InputSeqData.FIsNuc and D_InputSeqData.FIsCoding and (not VS_SeqDataExplorer.IsTranslated);
end;

procedure TV_SeqDataExplorer.DisableActionsConflictingWithLetterKeys;
begin
  ActionHighlightConserved.Enabled := False;
  ActionHighlightVariableSites.Enabled := False;
  ActionHighlightParsimInfoSites.Enabled := False;
  ActionHighlightSingletonSites.Enabled := False;
  ActionHighlightLabelledSites.Enabled := False;
end;

procedure TV_SeqDataExplorer.EnabledActionsConflictingWithLetterKeys;
begin
  ActionHighlightConserved.Enabled := True;
  ActionHighlightVariableSites.Enabled := True;
  ActionHighlightParsimInfoSites.Enabled := True;
  ActionHighlightSingletonSites.Enabled := True;
  ActionHighlightLabelledSites.Enabled := D_InputSeqData.HasSiteLabels and (not VS_SeqDataExplorer.IsTranslated);
end;

procedure TV_SeqDataExplorer.UpdateCurStatisticsStrings(aCol, aRow: Integer);
var
  aStr: String = '';
  index: Integer = -1;
begin
  if (not Assigned(D_InputSeqData.FDomainMarks)) or ((aCol-Cols.VisibleCount) < 0) then
    Exit;
  with DataGrid do
  begin
    if (D_InputSeqData.FIsNuc and D_InputSeqData.FisCoding and VS_SeqDataExplorer.IsTranslated) then
    begin
      index := (ACol - Cols.VisibleCount);
      if index < D_InputSeqData.AACodonStarts.Count then { necessary check because aCol is sometimes not updated yet for translated data}
      begin
        AStr :=   IntToStr(D_InputSeqData.AACodonStarts[ACol-Cols.VisibleCount]+1)+','+
                  IntToStr(D_InputSeqData.AACodonPos2  [ACol-Cols.VisibleCount]+1)+','+
                  IntToStr(D_InputSeqData.AACodonPos3  [ACol-Cols.VisibleCount]+1)+' ['+IntToStr(ACol-Cols.VisibleCount+1)+'/'+IntToStr(D_InputSeqData.FNoOfSites)+']';
        if D_InputSeqData.FDomainMarks.NoOfLabelledSites > 0 then
          AStr := AStr+'('+D_InputSeqData.FDomainMarks.SiteLabel[D_InputSeqData.AACodonStarts[ACol-Cols.VisibleCount]]
                          +D_InputSeqData.FDomainMarks.SiteLabel[D_InputSeqData.AACodonPos2  [ACol-Cols.VisibleCount]]
                          +D_InputSeqData.FDomainMarks.SiteLabel[D_InputSeqData.AACodonPos3  [ACol-Cols.VisibleCount]]+')';
      end
      else
        AStr := '/' +IntToStr(D_InputSeqData.FNoOfSites);
    end
    else
    begin
      AStr := '/' +IntToStr(D_InputSeqData.FNoOfSites);
      if D_InputSeqData.FDomainMarks.NoOfLabelledSites > 0 then
        AStr := AStr+'('+D_InputSeqData.FDomainMarks.SiteLabel[ACol-Cols.VisibleCount]+')';
    end;
    if VS_SeqDataExplorer.CurStatusBarStatistics.Count > stCurSite then
      VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stCurSite] :=  AStr;

    if D_InputSeqData.DomainMark[aCol-Cols.VisibleCount] = nil then
    begin
      VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stDomainName] :=  'Independent';
      D_InputSeqData.CurDomainInfo := nil;
    end
    else if (D_InputSeqData.CurDomainInfo <> D_InputSeqData.DomainMark[ACol-Cols.VisibleCount]) or (ACol-Cols.VisibleCount+1 = Cols.getVisibleIndex(Cols.Col[1])) then // so avoid not writing for the first column
      WriteGeneDomainOnStatusBar(D_InputSeqData.DomainMark[ACol-Cols.VisibleCount]);
  end;
end;

procedure TV_SeqDataExplorer.CloseEditLabelsPanel(Sender: TObject);
begin
  EditLabelsPanel.Visible := False;
end;

procedure TV_SeqDataExplorer.ApplySiteLabelsEdit(Sender: TObject);
var
  msg: String = '';
  aLabel: AnsiChar = #0;
  response: Integer;
begin
  if VS_SeqDataExplorer.IsTranslated then
  begin
    response := MessageDlg('Not supported for translated data', 'Site labels are not supported for translated data. Untranslate the data and continue?', mtConfirmation, mbYesNo, 0);
    if response = mrNo then
      Exit;
    ActionTranslateExecute(Sender);
  end;
  if Trim(FEditLabelsFrame.LabelEdit.Text) = EmptyStr then
  begin
    ShowMessage('Site label must be one of the following: A-Z a-z 0-9 * + - _');
    Exit;
  end;
  if Trim(FEditLabelsFrame.SitesMemo.Lines.Text) = EmptyStr then
  begin
    ShowMessage('No sites specified for labelling');
    Exit;
  end;
  aLabel := Trim(FEditLabelsFrame.LabelEdit.Text)[1];
  if not D_InputSeqData.FDomainMarks.AddSiteLabelOverrides(aLabel, Trim(FEditLabelsFrame.SitesMemo.Lines.Text), msg) then
    ShowMessage(Format('updating site labels failed (%s). Please check the label carefully', [msg]))
  else if FEditLabelsFrame.HighlighLabelledSitesCBox.Checked then
  begin
    VS_SeqDataExplorer.CurAttrDisp := megLabelled;
    D_InputSeqData.UpdateSiteAttrArray;
    ApplyUpdate;
    ActionHighlightLabelledSites.Checked := True;
  end
  else
    ActionHighlightLabelledSites.Checked := False;
  ActionHighlightLabelledSites.Enabled := D_InputSeqData.HasSiteLabels and (not VS_SeqDataExplorer.IsTranslated);
  ActionPreviousHighlight.Enabled := VS_SeqDataExplorer.CurAttrDisp <> megNone;
  ActionNextHighlight.Enabled := VS_SeqDataExplorer.CurAttrDisp <> megNone;
  VS_SeqDataExplorer.LabelledItem := VS_SeqDataExplorer.CurAttrDisp <> megNone;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ToggleHighlightLabelledSites(Sender: TObject);
begin
  if not (Sender = FEditLabelsFrame.HighlighLabelledSitesCBox) then
    Exit;
  if FUpdatingHighlightState then
    Exit;
  if ActionHighlightLabelledSites.Enabled then
  begin
    ActionHighlightLabelledSitesExecute(ActionHighlightLabelledSites);
    DataGrid.Invalidate;
  end;
end;

procedure TV_SeqDataExplorer.LabelledSitesEditOnEnter(Sender: TObject);
begin
  DisableActionConflictingWithNumberKeys;
  DisableActionsConflictingWithLetterKeys;
end;

procedure TV_SeqDataExplorer.LabelledSitesEditOnExit(Sender: TObject);
begin
  EnableActionsConflictingWithNumberKeys;
  EnabledActionsConflictingWithLetterKeys;
end;

procedure TV_SeqDataExplorer.AlleleFreqSearchDone(Thread: TObject);
var
  t: TAlleleFreqsThread = nil;
  response: Integer = mrCancel;
begin
  try
    D_InputSeqData.ProgressCheckCancelFunc := nil;
    if not (Thread is TAlleleFreqsThread) then
      raise Exception.Create('Application Error: expected TAlleleFreqsThread but got ' + Thread.ClassName);
    t := TAlleleFreqsThread(Thread);
    t.ProgressDlg.Free;
    UpdateStatusBar(nil);
    if t.IsSuccess then
    begin
      ResetGroupCounts;
      if not ActionDispGroupNames.Checked then
        ActionDispGroupNamesExecute(nil);
      DataGrid.Invalidate;
      if t.NumGroupsCreated > 0 then
        response := MessageDlg('View Report?', 'Open a text file report of sites with minor alleles used to group identical sequences?', mtConfirmation, mbYesNo, 0)
      else
        response := MessageDlg('View Report?', 'No sequences were found to be identical. Open a text file report of sites with major and minor allele frequencies?', mtConfirmation, mbYesNo, 0)
    end
    else
      response := mrCancel;
    if response = mrYes then
      OpenStringList(t.Report, 'Allele Frequencies', True)
    else if response = mrCancel then
      ShowMessage('Operation failed: ' + t.LogText);
    ActionLabelSitesUsedForGroupIdentityComparison.Enabled := (D_InputSeqData.NumAlleleComparisonSitesUsed > 0);
    ActionAddDomainsForGroupIdentityComparisonSites.Enabled := (D_InputSeqData.NumAlleleComparisonSitesUsed > 0);
    AdjustEnabledStatus;
  except
    on E:Exception do
      ShowMessage('Operation failed: ' + E.Message);
  end;
end;

procedure TV_SeqDataExplorer.UpdateMotifCountLabel(aMotif: String; aCount: Integer);
begin
  if FUpdatingHighlightState then
    Exit;
  if Trim(aMotif) = EmptyStr then
  begin
    MotifCountLabel.Caption := EmptyStr;
    StatusBar.Panels[stMarkedSites].Text := EmptyStr;
    Exit;
  end
  else if aCount <= 0 then
  begin
    MotifCountLabel.Caption := 'Motif not found';
    StatusBar.Panels[stMarkedSites].Text := EmptyStr;
    Exit;
  end;

  try
    FUpdatingHighlightState := True;
    if Length(aMotif) <= 10 then
    begin
      if aCount = 1 then
        MotifCountLabel.Caption :=  Format('Motif %s%s%s found 1 time', [#39, aMotif, #39])
      else
        MotifCountLabel.Caption := Format('Motif %s%s%s found %d times', [#39, aMotif, #39, aCount]);
      StatusBar.Panels[stMarkedSites].Text := MotifCountLabel.Caption;
    end
    else
    begin
      if aCount = 1 then
        MotifCountLabel.Caption := 'Motif found 1 time'
      else
        MotifCountLabel.Caption := Format('Motif found %.0n times', [aCount*1.0]);
      StatusBar.Panels[stMarkedSites].Text := MotifCountLabel.Caption;
    end;
    Invalidate;
  finally
    FUpdatingHighlightState := False;
  end;
end;

procedure TV_SeqDataExplorer.CountMotifsDone(Thread: TObject);
var
  t: TCountMotifsThread = nil;
begin
  t := TCountMotifsThread(Thread);
  if not t.IsSuccess then
    ShowMessage('Motif count failed: ' + t.LogText)
  else
    UpdateMotifCountLabel(t.Motif, t.NumMatchesFound);
end;

procedure TV_SeqDataExplorer.LaunchCountMotifsThread(aMotif: String);
var
  t: TCountMotifsThread = nil;
begin
  try
    MotifCountLabel.Caption := 'Counting motifs...';
    t := TCountMotifsThread.Create(aMotif);
    t.OnTerminate := CountMotifsDone;
    t.Start;
  except
    on E:Exception do
    begin
      ShowMessage('Failed to launch motifs count thread: ' + E.Message);
      MotifCountLabel.Caption := EmptyStr;
      StatusBar.Panels[stMarkedSites].Text := EmptyStr;
    end;
  end;
end;

procedure TV_SeqDataExplorer.DrawCurrentColumnBorder(aRect: TRect; topBorder: Boolean; bottomBorder: Boolean);
begin
  with DataGrid.Canvas do
  begin
    Brush.Style := bsClear;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Pen.Color := clBlack;
    Line(aRect.Left, aRect.Top, aRect.Left, aRect.Bottom);
    Line(aRect.Right - 1, aRect.Top, aRect.Right - 1, aRect.Bottom);
    if topBorder then
      Line(aRect.Left, aRect.Top, aRect.Right - 1, aRect.Top);
    if bottomBorder then
      Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
  end;
end;

procedure TV_SeqDataExplorer.AutoHighlightSites(Sender: TObject);
var
  a: TSiteAttrType;
  index: Integer;
begin
  if not Assigned(GeneDomainDlg) then
    Exit;

  try
    FForcingHighlightUpdate := True;
    D_InputSeqData.DomainMarks.IsDirty := True;
    ApplyUpdate;
    index := GeneDomainDlg.AutoHighlightComboBox.ItemIndex;
    case index of
      0: a := megConst;
      1: a := megVar;
      2: a := megParsimInfo;
      3: a := megSingleton;
      4: a := meg0Fold;
      5: a := meg2Fold;
      6: a := meg4Fold;
    end;
    D_InputSeqData.UpdateSiteAttrArray(True);
    D_InputSeqData.AutoLabelSites(a);
    GeneDomainDlg.SiteLabelGrid.Invalidate;
    D_InputSeqData.HideProgressDlg;
    AdjustEnabledStatus;
  except
    on E:Exception do
      ShowMessage(E.Message);
  end;
end;

function TV_SeqDataExplorer.NumberOfFixedColsVisible: Integer;
begin
  Result := 0;
  if VS_SeqDataExplorer.DispTaxaNamesItem then
    inc(Result);
  if VS_SeqDataExplorer.DispGpNamesItem then
    inc(Result);
end;

function TV_SeqDataExplorer.IsCodonByCodonWriting(remove1st, remove2nd, remove3rd, exportAllSites: Boolean): Boolean;
begin
  Result := D_InputSeqData.IsNuc and D_InputSeqData.FIsCoding and (not VS_SeqDataExplorer.IsTranslated);
  if Result then
    Result := Result and (not remove1st) and
                         (not remove2nd) and
                         (not remove3rd);
  if Result and (VS_SeqDataExplorer.CurAttrDisp <> megNone) then
    Result := Result and exportAllSites;
end;

procedure TV_SeqDataExplorer.LaunchDataExportThread(ExportOptions: TSeqExportOptions; aDestination: String; Results: TStringList);
var
  t: TExportSeqDataThread = nil;
begin
  t := TExportSeqDataThread.Create(D_InputSeqData, ExportOptions, aDestination, Results);
  t.OnTerminate := DataExportThreadDone;
  if Assigned(FRuntimeProg) then
    FreeAndNil(FRuntimeProg);
  FRuntimeProg := TRuntimeProgress.Create(Self);
  t.CheckCancelFunc := FRuntimeProg.ProgressCheckCancel;
  FRuntimeProg.Show;
  FRuntimeProg.UpdateRunStatusInfo('Status', 'Exporting Sequence Data...');
  t.Start;
end;

procedure TV_SeqDataExplorer.DataExportThreadDone(aThread: TObject);
var
  t: TExportSeqDataThread = nil;
  Response: Integer;
begin
  Assert(aThread is TExportSeqDataThread);
  try
    D_InputSeqData.ProgressCheckCancelFunc := nil;
    D_InputSeqData.HideProgressDlg;
    if Assigned(FRuntimeProg) then
    begin
      FRuntimeProg.Hide;
      FreeAndNil(FRuntimeProg);
    end;
    t := TExportSeqDataThread(aThread);
    if (not t.IsSuccess) then
      raise Exception.Create(t.MsgLog.Text);

    if t.ExportOptions.IsSpreadsheetFormat then
    begin
      case t.ExportOptions.ExportFormat of
        mefExcel: t.ExcelFile.SaveFile(t.FileDestination, ExportExcel, False);
        mefExcelXml: t.ExcelFile.SaveFile(t.FileDestination, ExportExelXML, False);
        mefOds: t.ExcelFile.SaveFile(t.FileDestination, ExportODS, False);
        mefCsv: t.ExcelFile.SaveFile(t.FileDestination, ExportCSV, False);
      end;
    end;
    if not FileExists(t.FileDestination) then
      raise Exception.Create('Failed to generate sequence data export file');
    Response := MessageDlg('Data Exported Successfully', 'Do you want to view the result?', mtInformation, mbYesNo, 0);
    if Response = mrYes then
    begin
      case t.ExportOptions.ExportFormat of
        mefPaup4, mefPaup3, mefMega, mefFasta, mefPhylip, mefCsv, mefTxt, mefNexus:
        begin
          OpenFileAndFocus(t.FileDestination, 1, 1);
        end;
        mefExcel, mefExcelXml, mefOds: OpenDocument(t.FileDestination);
      end;
    end;
  except
    on E:Exception do
      ShowMessage('An application error occurred when exporting sequence data: ' + E.Message);
  end;
end;

procedure TV_SeqDataExplorer.SetSelectedStatsExportType(AValue: TExportType);
begin
  case AValue of
    EXtext, EXtextSave: ActionStatDispInTextExecute(nil);
    EXcsvDisp, EXcsvSave: ActionStatDispInCsvExecute(nil);
    EXexcelDisp, EXexcelSave: ActionStatDispInXlExecute(nil);
    EXexcelXmlDisp, EXexcelXmlSave: ActionStatDispInXlsExecute(nil);
    EXodsDisp, EXodsSave: ActionStatDispInOdsExecute(nil);
    else
      raise Exception.Create('Application Error: Invalid export type');
  end;
end;

procedure TV_SeqDataExplorer.UpdateMarksForDomain(aInfo: TDomainInfo);
var
  i: Integer;
begin
  for i := aInfo.FromSite to aInfo.ToSite do
    D_InputSeqData.FDomainMarks[i] := nil;
  FGeneDomainsPropsEditor.GetDomainInfoProperties(aInfo);
  for i := aInfo.FromSite to aInfo.ToSite do
    D_InputSeqData.FDomainMarks[i] := aInfo;
end;

function TV_SeqDataExplorer.UpdateDomainCoordinates(aInfo: TDomainInfo; fromSite: Integer; toSite: Integer): Boolean;
begin
  Result := True;
  if (aInfo.FromSite >= fromSite) and (aInfo.ToSite <= toSite) then
    Exit; { nothing needs to be done}
  if aInfo.FromSite < fromSite then
    aInfo.FromSite := fromSite;
  if aInfo.ToSite > toSite then
    aInfo.ToSite := toSite;
  if aInfo.FromSite > aInfo.ToSite then
    Result := False;
end;

function TV_SeqDataExplorer.UpdateGeneCoordinates(aInfo: TDomainInfo; fromSite: Integer; toSite: Integer): Boolean;
var
  i: Integer;
  childInfo: TDomainInfo;
begin
  Result := True;
  aInfo.FromSite := fromSite;
  aInfo.ToSite := toSite;
  if aInfo.ChildDomains.Count > 0 then
    for i := 0 to aInfo.ChildDomains.Count - 1 do
    begin
      childInfo := TDomainInfo(aInfo.ChildDomains[i]);
      aInfo.FromSite := Min(aInfo.FromSite, childInfo.FromSite);
      aInfo.ToSite := Max(aInfo.ToSite, childInfo.ToSite);
    end;
  Result := (aInfo.FromSite <= aInfo.ToSite);
end;

function TV_SeqDataExplorer.GetMaxCharLen: LongInt;
begin
  Result := 1;
  with DataGrid, Canvas do
    if D_InputSeqData.FIsCoding or (not D_InputSeqData.FIsNuc) then
      Result := TextWidth('W')
    else
      Result := Floor(MaxValue([TextWidth('A')*1.0, TextWidth('T')*1.0,
                                TextWidth('C')*1.0, TextWidth('U')*1.0,
                                TextWidth('G')*1.0]));
end;

function TV_SeqDataExplorer.rowInHighlightRange(ARow: Integer): Boolean;
begin
  Result := False;
    if (ARow >= VS_SeqDataExplorer.CurColorRowRange.X) and (ARow <= VS_SeqDataExplorer.CurColorRowRange.Y) then
      Result := True;

end;

procedure TV_SeqDataExplorer.AssignHelpContext;
begin
  HelpContext := HC_Sequence_Data_Explorer;
  HelpKeyword := 'Sequence_Data_Explorer.htm';
  DataMenu.HelpContext := HC_Data_Menu_In_Sequence_Data_Explorer;
  ActionExportData.HelpContext  := HC_Export_Data_in_Sequence_Data_Explorer;
  ActionTranslate.HelpContext := HC_Translate_Untranslate_in_Sequence_Data_Explorer;
  ActionSelectCodeTable.HelpContext := HC_Select_Genetic_Code_Table_in_Sequence_Data_Explorer;
  ActionEditGenesDomains.HelpContext := HC_Setup_Select_Genes_Domains_in_Sequence_Data_Explorer_;
  ActionEditGroups.HelpContext :=  HC_Setup_Select_Taxa_Groups_in_Sequence_Data_Explorer;
  ActionQuit.HelpContext := HC_Quit_Data_Viewer;
  ActionColorCells.HelpContext := HC_Color_Cells;

  DisplayMenu.HelpContext := HC_Display_Menu_In_Sequence_Data_Explorer;
  ActionShowOnlySelected.HelpContext :=  HC_Show_Only_Selected_Sequences;
  ActionUseIdenticalSymbol.HelpContext := HC_Use_Identical_Symbol;
  ActionSortSequences.HelpContext := HC_Sort_Sequences;
  DispSortBySeqNameItem.HelpContext := HC_Sort_Sequences_By_Sequence_Name;
  DispSortByGpItem.HelpContext := HC_Sort_Sequences_by_Group_Name;
  ByGpAndSeqNamesItem.HelpContext := HC_Sort_Sequences_by_Group_and_Sequence_Names;
  AsperTaxaGroupOrganizerItem.HelpContext := HC_Sort_Sequences_As_per_Taxa_Group_Organizer;
  ActionRestoreInputOrder.HelpContext := HC_Restore_Input_Order;
  ActionDispTaxaNames.HelpContext := HC_Show_Sequence_Names;
  ActionDispGroupNames.HelpContext := HC_Show_Group_Names;
  ActionChangeFont.HelpContext := HC_Change_Font;

  HighlightSitesMenu.HelpContext := HC_Highlight_Menu_In_Sequence_Data_Explorer;
  ActionHighlightConserved.HelpContext   := HC_Highlight_Conserved_Sites;
  ActionHighlightVariableSites.HelpContext   := HC_Highlight_Variable_Sites;
  ActionHighlightParsimInfoSites.HelpContext := HC_Highlight_Parsimony_Informative_Sites;
  ActionHighlightSingletonSites.HelpContext  := HC_Highlight_Singleton_Sites;
  ActionHighlight0FoldDegenSites.HelpContext   := HC_Highlight_0_fold_Degenerate_Sites;
  ActionHighlight2FoldDegenSites.HelpContext    := HC_Highlight_2_fold_Degenerate_Sites;
  ActionHighlight4FoldDegenSites.HelpContext   := HC_Highlight_4_fold_Degenerate_Sites;

  StatisticsMenu.HelpContext   := HC_Stats_menu_in_Sequence_Data_Explorer;
  ActionNucComposition.HelpContext := HC_Nucleotide_Composition;
  StatNucPairFreqSubmenu.HelpContext := HC_Nucleotide_Pair_Frequencies;
  ActionDirectionalPairs.HelpContext := HC_Nucleotide_Pair_Frequencies;
  ActionUndirectionalPairs.HelpContext := HC_Nucleotide_Pair_Frequencies;
  ActionCodonUsage.HelpContext := HC_Codon_Usage;
  ActionAminoAcidComposition.HelpContext := HC_Amino_Acid_Composition;
  ActionMismatchDistribution.HelpContext := -1;
  ActionAllSelectedSites.HelpContext := HC_Use_All_Selected_Sites;
  ActionHighlightedSites.HelpContext := HC_Use_only_Highlighted_Sites;

  SetGroupTagButton.HelpContext := HC_COMMAND_STATEMENTS;
  SetGroupTagItem.HelpContext := HC_COMMAND_STATEMENTS;
end;

//procedure TV_SeqDataExplorer.SetFontSmoothing(AFont: TFont);
//var
//  tagLogFont: TLogFont;
//begin
//  GetObject(
//    AFont.Handle,
//    SizeOf(TLogFont),
//    @tagLOGFONT);
//  tagLOGFONT.lfQuality := ANTIALIASED_QUALITY;
//  AFont.Handle := CreateFontIndirect(tagLOGFONT);
//end;

//procedure TV_SeqDataExplorer.drawDomainArrow(site: Integer; aCanvas: TCanvas);
//const
//  arrowSize = 6;
//var
//  aLeft, aTop, aBottom, aRight: Integer;
//  point : array[0..3] of TPoint;
//  dmark: TDomainInfo;
//  offset: Integer;
//begin
//  offset := 7;
//  dmark := D_InputSeqData.GetDomainMark(site);
//  if (ACol >= Cols.VisibleCount) and (dmark <> nil)  then
//    begin
//        PrevPenColor2 := aCanvas.Pen.Color;
//        aCanvas.Pen.Color := clMaroon;
//        PrevBrushColor2 := aCanvas.Brush.Color;
//        aCanvas.Brush.Color := clMaroon;
//        aCanvas.Pen.Width := 1;
//        ResizeLineRect := DataGrid.CellRect(ACol, 0);    // Rect for cell 1, 1
//
//        left := ResizeLineRect.Left;
//        top := ResizeLineRect.Top;
//        bottom := ResizeLineRect.Bottom;
//        right := ResizeLineRect.Right;
//      if dmark.FromSite = Site then
//      begin
//        // Draw left arrow
//        point[0].X := left+offset;
//        point[0].Y := (DataGrid.RowHeights[ARow] div 2) ;
//        point[1].X := left + ArrowSize + offset;
//        point[1].Y := (DataGrid.RowHeights[ARow] div 2)-ArrowSize;
//        point[2].X := left + ArrowSize + offset;
//        point[2].Y := (DataGrid.RowHeights[ARow] div 2)+ArrowSize;
//        point[3].X := point[0].X;
//        point[3].Y := point[0].Y;
//        aCanvas.Polygon(point);
//      end
//      else if dmark.ToSite = Site then
//      begin
//         // Draw right arrow
//        point[0].X := right-offset;
//        point[0].Y := (DataGrid.RowHeights[ARow] div 2) ;
//        point[1].X := right - ArrowSize - offset;
//        point[1].Y := (DataGrid.RowHeights[ARow] div 2)-ArrowSize;
//        point[2].X := right - ArrowSize - offset;
//        point[2].Y := (DataGrid.RowHeights[ARow] div 2)+ArrowSize;
//        point[3].X := point[0].X;
//        point[3].Y := point[0].Y;
//        aCanvas.Polygon(point);
//      end;
//        aCanvas.Pen.Width := 1;
//        aCanvas.Pen.Color := PrevPenColor2;
//        aCanvas.Brush.Color := PrevBrushColor2;
//    end;
//end;

procedure TV_SeqDataExplorer.drawBracket(site: Integer; aCanvas: TCanvas);
const
  offset = 2;
var
  aLeft, aTop, aBottom: Integer;
  dmark: TDomainInfo;
  ResizeLineRect: TRect;
  aCol: Integer;
begin
  if (not Visible) or (not ActionShowDomainBoundaries.Checked) then Exit;
  aCol := site + Cols.VisibleCount;
  dmark := D_InputSeqData.GetDomainMark(site);
  if (site >= 0) and (dmark <> nil)  then
  begin
    ResizeLineRect := DataGrid.CellRect(aCol, 0);
    aLeft := ResizeLineRect.Left;
    aTop := ResizeLineRect.Top;
    aBottom := (DataGrid.RowCount-1) * (ResizeLineRect.Bottom - ResizeLineRect.Top +1)+2;

    aCanvas.Pen.Color := clBlue;
    aCanvas.Pen.Width := 2;
    if dmark.FromSite = Site then
    begin
      aCanvas.MoveTo(aLeft+offset, aTop+1);
      aCanvas.LineTo(aLeft+offset, aBottom);
    end;
    aCanvas.Pen.Width := 1;
  end;
end;

function TV_SeqDataExplorer.SiteInSearchBox(seqindex, siteindex: integer): boolean;
var
  i,j: integer;
  str: AnsiString;
  SequenceStr: AnsiString;
begin
  if siteindex < 1 then
    MessageDlg('Warning: site index is < 1.  This will cause an error likely.', mtWarning, [mbOK], 0);
  SequenceStr := D_InputSeqData.Sequence[seqindex];

  if Pos('-', VS_SeqDataExplorer.CurMotifStr) > 0 then
  begin
    result := false;
    if siteindex-length(VS_SeqDataExplorer.CurMotifStr) < 0 then
    begin
      j := length(VS_SeqDataExplorer.CurMotifStr) +siteindex -1;
      str := system.copy(SequenceStr, 1, j);
    end
    else
      str := system.copy(SequenceStr, siteindex-length(VS_SeqDataExplorer.CurMotifStr)+1, length(VS_SeqDataExplorer.CurMotifStr)*2-1);
    for i := 1 to length(str)-length(VS_SeqDataExplorer.CurMotifStr)+1 do
      if MatchSearchBox(system.Copy(str, i, length(VS_SeqDataExplorer.CurMotifStr))) then
      begin
        result := true;
        break;
      end;
    exit;
  end;

  str := '';
  i := siteindex;
  j := 0;
  while (i > 0) and (j < length(VS_SeqDataExplorer.CurMotifStr)) do
  begin
    if SequenceStr[i] <> '-' then
    begin
      str := SequenceStr[i] +str;
      inc(j);
    end;
    dec(i);
  end;
  if (SequenceStr[siteindex] = '-') and (length(str) = length(VS_SeqDataExplorer.CurMotifStr)) then
  begin
    result := false;
    exit;
  end;
  i := siteindex+1;
  j := 0;
  while (i <= Length(SequenceStr)) and (j < length(VS_SeqDataExplorer.CurMotifStr)-1) do
  begin
    if SequenceStr[i] <> '-' then
    begin
      str := str + SequenceStr[i];
      inc(j);
    end;
    inc(i);
  end;
  result := false;
  if length(str) >= length(VS_SeqDataExplorer.CurMotifStr) then
    for i := 1 to length(str)-length(VS_SeqDataExplorer.CurMotifStr)+1 do
      if MatchSearchBox(system.Copy(str, i, length(VS_SeqDataExplorer.CurMotifStr))) then
      begin
        result := true;
        break;
      end;
end;

function TV_SeqDataExplorer.MatchSearchBox(box: String): boolean;
var
  i: integer;
begin
  result := false;
  box := uppercase(box);
  if length(box) >= length(VS_SeqDataExplorer.CurMotifStr) then
    if D_InputSeqData.IsNuc and (not VS_SeqDataExplorer.IsTranslated) then
      for i := 1 to length(VS_SeqDataExplorer.CurMotifStr) do
      begin
        case VS_SeqDataExplorer.CurMotifStr[i] of
          'A': result := (box[i] = 'A');
          'T': result := (box[i] = 'T') or (box[i] = 'U');
          'U': result := (box[i] = 'U') or (box[i] = 'T');
          'C': result := (box[i] = 'C');
          'G': result := (box[i] = 'G');
          'R': result := (box[i] = 'R') or (box[i] = 'A') or (box[i] = 'G');
          'Y': result := (box[i] = 'Y') or (box[i] = 'T') or (box[i] = 'C') or (box[i] = 'U');
          'M': result := (box[i] = 'M') or (box[i] = 'A') or (box[i] = 'C');
          'K': result := (box[i] = 'K') or (box[i] = 'T') or (box[i] = 'G') or (box[i] = 'U');
          'S': result := (box[i] = 'S') or (box[i] = 'C') or (box[i] = 'G');
          'W': result := (box[i] = 'W') or (box[i] = 'A') or (box[i] = 'T') or (box[i] = 'U');
          'B': result := (box[i] = 'B') or (box[i] = 'T') or (box[i] = 'U') or (box[i] = 'C') or (box[i] = 'G') or (box[i] = 'Y') or (box[i] = 'K') or (box[i] = 'S');
          'V': result := (box[i] = 'V') or (box[i] = 'A') or (box[i] = 'C') or (box[i] = 'G')                   or (box[i] = 'R') or (box[i] = 'M') or (box[i] = 'S');
          'D': result := (box[i] = 'D') or (box[i] = 'A') or (box[i] = 'T') or (box[i] = 'U') or (box[i] = 'G') or (box[i] = 'R') or (box[i] = 'K') or (box[i] = 'W');
          'H': result := (box[i] = 'H') or (box[i] = 'A') or (box[i] = 'T') or (box[i] = 'U') or (box[i] = 'C') or (box[i] = 'Y') or (box[i] = 'M') or (box[i] = 'W');
          'N': result := true;
          '-': result := box[i] = '-'
        else
          result := box[i] = VS_SeqDataExplorer.CurMotifStr[i];
        end;
        if not result then break;
      end
    else
      for i := 1 to length(VS_SeqDataExplorer.CurMotifStr) do
      begin
        if VS_SeqDataExplorer.CurMotifStr[i] = 'X' then
          result := true
        else
          result := box[i] = VS_SeqDataExplorer.CurMotifStr[i];
        if not result then break;
      end
  else
    result := false;
end;

function TV_SeqDataExplorer.SelectedStatsMegaExportFormat: TMegaExportFormat;
begin
  if ActionStatDispInXls.Checked then
    Result := mefExcelXml
  else if ActionStatDispInXl.Checked then
    Result := mefExcel
  else if ActionStatDispInOds.Checked then
    Result := mefOds
  else if ActionStatDispInCsv.Checked then
    Result := mefCsv
  else if ActionStatDispInText.Checked then
    Result := mefTxt
  else
    raise Exception.Create('Invalid export type');
end;

function TV_SeqDataExplorer.GetSelectedStatsExportType: TExportType;
begin
  if ActionStatDispInXls.Checked then
    Result := EXexcelXmlDisp
  else if ActionStatDispInXl.Checked then
    Result := EXexcelDisp
  else if ActionStatDispInOds.Checked then
    Result := EXodsDisp
  else if ActionStatDispInCsv.Checked then
    Result := EXcsvDisp
  else if ActionStatDispInText.Checked then
    Result := EXtext
  else
    raise Exception.Create('Invalid export type');
end;

function TV_SeqDataExplorer.GetFileExportDestination(const aExportType: TMegaExportFormat; var aDestination: String): Boolean;
begin
  Result := False;
  SaveDialog1.InitialDir := LazFileUtils.GetCurrentDirUTF8;
  SaveDialog1.FileName := ChangeFileExt(ExtractFileName(aDestination), GetFileExtensionForExportType(aExportType));
  case aExportType of
    mefPaup4, mefPaup3, mefNexus: SaveDialog1.Filter := 'Paup-Nexus (*.nex)|*.nex|All files (.*)|*.*';
    mefMega: SaveDialog1.Filter := 'MEGA file (*.meg)|*.meg|All files (*.*)|*.*';
    mefFasta: SaveDialog1.Filter := 'Fasta file (*.fas, *.fasta)|*.fasta; *.fas|All files (*.*)|*.*';
    mefPhylip: SaveDialog1.Filter := 'Phylip file (*.phy)|*.Phy|All files (*.*)|*.*';
    mefExcel, mefExcelXml: SaveDialog1.Filter := 'Excel file (*.xlsx, *.xls)|*.xlsx;*.xls|All files (*.*)|*.*';
    mefOds: SaveDialog1.Filter := 'Open/Libre Office file(*.ods)|*.ods|All files (*.*)}*.*';
    mefCsv: SaveDialog1.Filter := 'CSV file (*.csv)|*.csv|All files (*.*)|*.*';
    mefTxt: SaveDialog1.Filter := 'Text file (*.txt)|*.txt|All files (*.*)|*.*';
  end;
  if not SaveDialog1.Execute then
    Exit;
  aDestination := SaveDialog1.Filename;
  aDestination := ChangeFileExt(aDestination, GetFileExtensionForExportType(aExportType));
  Result := True;
end;

function TV_SeqDataExplorer.GetFileExtensionForExportType(const aExportType: TMegaExportFormat): String;
begin
  case aExportType of
    mefPaup4, mefPaup3, mefNexus: Result := '.nex';
    mefMega: Result := '.meg';
    mefFasta: Result := '.fas';
    mefPhylip: Result := '.phy';
    mefExcel: Result := '.xls';
    mefExcelXml: Result := '.xlsx';
    mefCsv: Result := '.csv';
    mefTxt: Result := '.txt';
    mefOds: Result := '.ods';
  end;
end;

function TV_SeqDataExplorer.IsShowingAminoAcidData: Boolean;
begin
  Result := False;
  if VS_SeqDataExplorer.IsTranslated then
    Result := True
  else if D_InputSeqData.IsAmino then
    Result := True;
end;

function TV_SeqDataExplorer.GetSearchResult(Row, Col: Integer): TSearchResult;
var
  i: Integer;
  DispTaxaRow: Integer;
begin
  result := nil;
  if Row >= (D_InputSeqData.DispTaxaList.Count) then
    Exit;
  DispTaxaRow := D_InputSeqData.DispTaxaList[Row];

  for i:=0 to D_InputSeqData.NoOfSearchResults-1 do
  begin
    if (D_InputSeqData.SearchResults[i].Top = DispTaxaRow) then
      if (Col >= D_InputSeqData.SearchResults[i].Left) then
        if (Col < D_InputSeqData.SearchResults[i].Right) then
        begin
          result := D_InputSeqData.SearchResults[i];
          Exit;
        end;
  end;
end;

procedure TV_SeqDataExplorer.SetBrushColor(x: AnsiChar);
begin
  with DataGrid, DataGrid.Canvas do
  begin
    if IsShowingAminoAcidData then
      Brush.Color := GetBGColorForAminoAcid(x)
    else
      Brush.Color := GetBGColorForNucleotide(x);
  end;
end;

procedure TV_SeqDataExplorer.updateBirdsGeneDomain;
var
  i, site, numEmptySites, startEmpty: Integer;
  emptyInfo, dinfo: TDomainInfo;
begin
  if (not bird.Visible) or (not Assigned(D_InputSeqData)) then
    Exit;

  bird.ColCount := D_InputSeqData.FDomainMarks.NoOfDomains;
  i := 0;
  site := 0;
  numEmptySites := 0;
  // This information (ranges) should be CACHED, and updated when domains are changed.  NOT calculated DURING resize.
  while site < D_InputSeqData.NoOfSites do
  begin
    if (site < D_InputSeqData.NoOfSites-1) then
    begin
      if D_InputSeqData.FDomainMarks[site] = nil then
      begin
        inc(site);
        if numEmptySites = 0 then
          startEmpty := site;
        inc(numEmptySites);
        continue;
      end;
    end;
    if numEmptySites > 0 then
    begin
      emptyInfo := TDomainInfo.Create;
      emptyInfo.FromSite := startEmpty;
      emptyInfo.ToSite := startEmpty + numEmptySites-1;
      emptyInfo.Name := EmptyStr;
      emptyInfo.GeneName := EmptyStr;
      emptyInfo.IsUsed := false;

      bird.ColCount := bird.ColCount + 1;
      bird.Objects[i, 0] := emptyInfo;
      bird.Cells[i, 0] := emptyInfo.GeneName;
      bird.Cells[i, 1] := emptyInfo.name;
      bird.ColWidths[i] := Round((bird.Width / D_InputSeqData.NoOfSites) * numEmptySites);
      if Assigned(FEmptyInfos) then
        FEmptyInfos.Add(emptyInfo);
      numEmptySites := 0;
      inc(i);
    end;
    if D_InputSeqData.FDomainMarks[site] <> nil then
    begin
      dinfo := D_InputSeqData.FDomainMarks[site];
      bird.Objects[i, 0] := dinfo;
      bird.Cells[i, 0] := dinfo.GeneName;
      if dinfo.IsDomain then
        bird.Cells[i, 1] := dinfo.name
      else
        bird.Cells[i, 1] := '';
      bird.ColWidths[i] := Round((bird.Width / D_InputSeqData.NoOfSites) * dinfo.NoOfSites);

      site := dinfo.ToSite+1;
      inc(i);
    end
    else
      inc(site);
  end;
end;

procedure TV_SeqDataExplorer.focusBirdWindowOn(point: Integer);
var
  boxWidth, focusPoint, newLeftFocusedCol, newFocusedCol, prevCol: Integer;
begin
  prevCol := DataGrid.Col;
  try
    // Calculate which left most column we need to select, in order to have the screen show where we clicked.
    boxWidth := Floor(bird.Width * (DataGrid.VisibleColCount / DataGrid.ColCount));
    focusPoint := (point - (boxWidth div 2));
    newLeftFocusedCol := Floor(DataGrid.ColCount * (focusPoint / bird.Width));
    if newLeftFocusedCol < Cols.VisibleCount then
      newLeftFocusedCol := Cols.VisibleCount;
    if (newLeftFocusedCol + DataGrid.VisibleColCount) > DataGrid.ColCount-1 then
      newLeftFocusedCol := DataGrid.ColCount-1-DataGrid.VisibleColCount;

    DataGrid.LeftCol := newLeftFocusedCol;

    // Calculate the actual column to point at in the middle of the box, which we can focus on.
    newFocusedCol := Floor(DataGrid.ColCount * (point / bird.Width));
    if newFocusedCol > DataGrid.ColCount-1 then
      newFocusedCol := DataGrid.ColCount-1;
    if newFocusedCol < Cols.VisibleCount then
      newFocusedCol := Cols.VisibleCount;
    DataGrid.Col := newFocusedCol;
    bird.Invalidate;
    try
      FSelfScrolling := True;
      HorizScrollbar.Position := DataGrid.Col;
    finally
      FSelfScrolling := False;
    end;
  Except
    on E: Exception do
    begin
      DataGrid.Col := prevCol;
    end;
  end;
end;

procedure TV_SeqDataExplorer.UncheckSpecial;
begin
  ActionCoverage.Checked := False;
  ActionCpG.Checked := False;
  SpecialButton.Down := False;
end;

procedure TV_SeqDataExplorer.UncheckSpecific;
begin
  ActionHighlightConserved.Checked := False;
  ActionHighlightVariableSites.Checked := False;
  ActionHighlightParsimInfoSites.Checked := False;
  ActionHighlightLabelledSites.Checked := False;
  ActionHighlightSingletonSites.Checked := False;
  ActionHighlight0FoldDegenSites.Checked := False;
  ActionHighlight2FoldDegenSites.Checked := False;
  ActionHighlight4FoldDegenSites.Checked := False;
  ActionHighlightLabelledSites.Checked := False;
end;

procedure TV_SeqDataExplorer.ResetSearchBtns;
begin
  SearchBtn.Visible := True;
  FindPrevBtn.Visible := False;
  FindNextBtn.Visible := False;
  UpdateMotifCountLabel(EmptyStr, -1);
end;

procedure TV_SeqDataExplorer.ShowTaxaSearch;
begin
  if searchTypeCbx.ItemIndex <> 0 then
  begin
    searchTypeCbx.ItemIndex := 0;
    resetSearchBtns;
  end;
end;

procedure TV_SeqDataExplorer.ShowMotifSearch;
begin
  if searchTypeCbx.ItemIndex <> 2 then
  begin
    searchTypeCbx.ItemIndex := 2;
    resetSearchBtns;
  end;
end;

procedure TV_SeqDataExplorer.SearchGroup;
var
   GroupAt: Integer;
   ARP: TRuntimeProgress=nil;
begin
  ARP := TRuntimeProgress.Create(nil);
  ARP.UpdateRunStatusInfo('Status', 'Searching for sequence');
  try
    begin
      ARP.Show;
      with DataGrid do
      begin
        GroupAt := D_InputSeqData.FindGroup(VS_SeqDataExplorer.CurGroupStr, Datagrid.Row, ARP); //Returns -1 if couldn't find it otherwise the row#
          if (GroupAt = -1) AND (Row <> 1) then  //Not Found and we didn't start at the top so prompt them if they want to continue from the top
          begin
            ARP.Hide;
            if MessageDlg('Query name not found. Do you want to go to the top and search again?', mtConfirmation, [mbYes, mbNo],0) = mrYes
            then
            begin
              ARP.Show;
              GroupAt := D_InputSeqData.FindGroup(VS_SeqDataExplorer.CurGroupStr, 1, ARP);  //start at 1 since we are starting at the top and row 0 is fixed
            end;
          end;

        if (GroupAt = -1) then
        begin
          FreeAndNil(ARP);
          ShowMessage('Query name not found.');
          Exit;
        end
        else
        begin
          Row := GroupAt;
          VS_SeqDataExplorer.CurColorRowRange := Point(GroupAt, GroupAt);
          SeqNameResult := D_InputSeqData.TaxaName[GroupAt-1];
          Invalidate;
          SearchBtn.Visible := False;
          FindPrevBtn.Visible := True;
          FindNextBtn.Visible := True;
        end;
      end;
    end;
  finally
    if Assigned(ARP) then
      ARP.Free;
  end;
end;

procedure TV_SeqDataExplorer.SearchMotif;
begin
  if VS_SeqDataExplorer.CurMotifStr = EmptyStr then
  begin
    ShowMessage('Please specify a motif to search for.');
    Exit;
  end;

  if Length(VS_SeqDataExplorer.CurMotifStr) > D_InputSeqData.NoOfSites then
  begin
    ShowMessage('Length of search motif cannot be greater than the number of positions in the data.');
    Exit;
  end;

  if SearchNextSingleMotif(VS_SeqDataExplorer.CurMotifStr, 0, 1) then
  begin
    SearchBtn.Visible := False;
    FindPrevBtn.Visible := True;
    FindNextBtn.Visible := True;

    if SearchBox.Items.IndexOf(VS_SeqDataExplorer.CurMotifStr) <> -1 then
    begin
      if SearchBox.Items.Strings[0] <> VS_SeqDataExplorer.CurMotifStr then
        SearchBox.Items.Move(SearchBox.Items.IndexOf(VS_SeqDataExplorer.CurMotifStr), 0);  // move to the top
    end
    else
      SearchBox.Items.Insert(0, VS_SeqDataExplorer.CurMotifStr);
    DataGrid.Invalidate;
    LaunchCountMotifsThread(VS_SeqDataExplorer.CurMotifStr);
  end;
end;

procedure TV_SeqDataExplorer.UpdateHighlightButtonStates;
begin
  ConservedBtn.Down := VS_SeqDataExplorer.ConstantItem;
  VariableBtn.Down := VS_SeqDataExplorer.VariableItem;
  ParsimInfoBtn.Down := VS_SeqDataExplorer.ParsimInfoItem;
  LabelledSitesBtn.Down := VS_SeqDataExplorer.LabelledItem;
  SingletonBtn.Down := VS_SeqDataExplorer.SingletonItem;
  ZeroFoldBtn.Down := VS_SeqDataExplorer.ZeroFoldItem;
  TwoFoldBtn.Down := VS_SeqDataExplorer.TwoFoldItem;
  FourFoldBtn.Down := VS_SeqDataExplorer.FourFoldItem;
end;

procedure TV_SeqDataExplorer.ClearHighlightButtonStates;
begin
  ConservedBtn.Down := False;
  VS_SeqDataExplorer.ConstantItem :=  False;
  VariableBtn.Down := False;
  VS_SeqDataExplorer.VariableItem := False;
  ParsimInfoBtn.Down := False;
  VS_SeqDataExplorer.ParsimInfoItem := False;
  LabelledSitesBtn.Down := False;
  VS_SeqDataExplorer.LabelledItem := False;
  SingletonBtn.Down := False;
  VS_SeqDataExplorer.SingletonItem := False;
  ZeroFoldBtn.Down := False;
  VS_SeqDataExplorer.ZeroFoldItem := False;
  TwoFoldBtn.Down := False;
  VS_SeqDataExplorer.TwoFoldItem := False;
  FourFoldBtn.Down := False;
  VS_SeqDataExplorer.FourFoldItem := False;
end;

procedure TV_SeqDataExplorer.InitMainMenu;
begin
  {$IFNDEF DARWIN}
  SeqDataViewerMenu.OwnerDraw := True;
  SeqDataViewerMenu.OnMeasureItem := MegaForm.MeasureMenuItem;
  SeqDataViewerMenu.OnDrawItem := MegaForm.DrawMenuItem
  {$ENDIF}
end;

procedure TV_SeqDataExplorer.InitPopupMenus;
begin
  MegaForm.AddSettingsToPopupMenu(Column0PopupMenu);
  MegaForm.AddSettingsToPopupMenu(SpecialMenu);
  MegaForm.AddSettingsToPopupMenu(EditGroup);
  MegaForm.AddSettingsToPopupMenu(EditSpecies);
  MegaForm.AddSettingsToPopupMenu(EditPopulation);
  MegaForm.AddSettingsToPopupMenu(AddEditGeneDomains);
  MegaForm.AddSettingsToPopupMenu(DiagnoseMenu);
end;

function TV_SeqDataExplorer.NumSelectedTaxaString: String;
var
  numTaxa: Integer;
begin
  numTaxa := D_InputSeqData.OtuInfos.NoOfSelOtus;
  if numTaxa = 1 then
    Result := '1 taxon'
  else
    Result := Format('%d taxa', [numTaxa]);
end;

function TV_SeqDataExplorer.NumSelectedGroupsString: String;
var
  numGroups: Integer;
begin
  numGroups := D_InputSeqData.OtuInfos.NoOfSelGroups;
  if numGroups > 0 then
  begin
    if numGroups = 1 then
      Result := ', 1 group'
    else
      Result := Format(', %d groups', [numGroups]);
  end
  else
  Result := EmptyStr;
end;

procedure TV_SeqDataExplorer.UncheckGroupTagMenus;
begin
  GroupItem.Checked := False;
  GroupItem2.Checked := False;
  SpeciesItem.Checked := False;
  SpeciesItem2.Checked := False;
  PopulationItem.Checked := False;
  PopulationItem2.Checked := False;
  ContinentItem.Checked := False;
  ContinentItem2.Checked := False;
  CountryItem.Checked := False;
  CountryItem2.Checked := False;
  CityItem.Checked := False;
  CityItem2.Checked := False;
  YearItem.Checked := False;
  YearItem2.Checked := False;
  MonthItem.Checked := False;
  MonthItem2.Checked := False;
  DayItem.Checked := False;
  DayItem2.Checked := False;
  TimeItem.Checked := False;
  TimeItem2.Checked := False;
end;

procedure TV_SeqDataExplorer.UpdateGroupTagMenuCheckedItem(aIndex: Integer);
begin
  UncheckGroupTagMenus;
  try
    FUpdatingGroupTagMenus := True;
    if aIndex < SetGroupTagPopup.Items.Count then
      SetGroupTagPopup.Items[aIndex].Checked := True;
    if aIndex < SetGroupTagItem.Count then
      SetGroupTagItem.Items[aIndex].Checked := True;
  finally
    FUpdatingGroupTagMenus := False;
  end;
end;

procedure TV_SeqDataExplorer.ActionSaveSessionExecute(Sender: TObject);
var
  PleaseWait : TPleaseWait = nil;
begin
  try
    PleaseWait := TPleaseWait.Create(nil);
    SaveDialog1.Filter := MegaSessionFilesFilter;
    if MegaForm.DataFileName = EmptyStr then
      SaveDialog1.FileName := '*.mdsx'
    else
      SaveDialog1.FileName := ChangeFileExt(ExtractFileName(MegaForm.DataFileName), '.mdsx'); // if they already have a file open then we should prompt to save with a variation of the current filename.
    SaveDialog1.Title := 'Please Choose a name for your Mega Data Session file';
    SaveDialog1.Options := [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing];
    SaveDialog1.InitialDir := ChangeInitialDirectorySaveDialogForMac(SaveDialog1.InitialDir);
    if not SaveDialog1.Execute then
      Exit;
    if ExtractFileExt(SaveDialog1.FileName) <> '.mdsx' then
      SaveDialog1.Filename := SaveDialog1.Filename + '.mdsx';
    SaveDialog1.InitialDir := ChangeInitialDirectorySaveDialogForMac(ExtractFilePath(SaveDialog1.FileName));
    PleaseWait.Caption := 'Saving Sequence Data Session File';
    PleaseWait.Action := 'Please Wait .... ';
    PleaseWait.show;
    D_InputSeqData.SaveSession(SaveDialog1.FileName);
    PleaseWait.hide;
  finally
    FreeAndNil(PleaseWait);
  end;
end;

procedure TV_SeqDataExplorer.ActionSelectCodeTableExecute(Sender: TObject);
var
  CodeTableDlg1: TSelectGeneticCodeDlg = nil;
begin
  try
    CodeTableDlg1 := TSelectGeneticCodeDlg.Create(Self);
    CodeTableDlg1.CodeTableName := D_InputSeqData.FCodeName;
    if CodeTableDlg1.ShowModal = mrOK then
    begin
      D_InputSeqData.CodeName := CodeTableDlg1.CodeTableName;
      D_InputSeqData.CodeTable := CodeTableDlg1.CodeTable;
      MegaForm.CodeTableName := D_InputSeqData.FCodeName;
      MegaForm.CodeTable := D_InputSeqData.CodeTable;
      VS_SeqDataExplorer.UpdateCodeTable;
    end;
  finally
    if Assigned(CodeTableDlg1) then
      CodeTableDlg1.Free;
  end;
end;

procedure TV_SeqDataExplorer.ActionSelectEarliestDatedSeqInEachGroupExecute(Sender: TObject);
var
  response: Boolean = False;
  tempDateFormat: String = 'yyyymmdd';
  t: TParseTipDatesThread = nil;
  arp: TRuntimeProgress = nil;
begin
  if not D_InputSeqData.OtuInfos.AllOtusHaveDateInfo then
  begin
    response := InputQuery('MEGA can try to find date information in sequence names', 'Please provide the date format for timestamps', tempDateFormat);
    if response then
    begin
      t := TParseTipDatesThread.Create(tempDateFormat);
      t.OnTerminate := ParseTipDatesThreadDone;
      arp := TRuntimeProgress.Create(Self);
      arp.UpdateRunStatusInfo('Status', 'Searching for timestamps in sequence names');
      t.CheckCancel := arp.ProgressAndStatusInfoCheckCancel;
      arp.Show;
      t.RuntimeProgress := arp;
      t.Start
    end
    else
      Exit;
  end
  else
    SelectEarliestDatedSequenceInEachGroup;
end;

procedure TV_SeqDataExplorer.ActionSelectHighlightColorExecute(Sender: TObject);
begin
  with ColorDialog1 do
  begin
    Color := VS_SeqDataExplorer.HighlightColor;
    if not Execute then
      Exit;
    VS_SeqDataExplorer.HighlightColor := Color;
    if VS_SeqDataExplorer.CurAttrDisp <> megNone then
      DataGrid.Invalidate;
    DataGrid.SetFocus;
  end;
end;

procedure TV_SeqDataExplorer.ActionSelectLargestGroupsExecute(Sender: TObject);
var
  numGroupsStr: String = '';
  numGroups: Integer;
  i: Integer;
  otu: TOtuInfo = nil;
  response: Boolean = False;
  groupCounts: TFPHashList = nil;
begin
  Repeat
    response := InputQuery('Number of Groups', 'Maximum Number of Groups', numGroupsStr);
    if response = False then
      Exit;
  until (not response) or (TryStrToInt(NumGroupsStr, numGroups) and (numGroups >= 1));
  if response then
  begin
    try
      try
        groupCounts := D_InputSeqData.GetLargestGroups(numGroups);
        for i := 0 to D_InputSeqData.NoOfTaxa - 1 do
        begin
          otu := D_InputSeqData.OtuInfos[i];
          if groupCounts.FindIndexOf(otu.GpName) < 0 then
            otu.IsUsed := False
          else
            otu.IsUsed := True
        end;
      except
        on E:Exception do
          ShowMessage('Application Error when auto-selecting groups: ' + E.Message);
      end;
    finally
      if Assigned(groupCounts) then
      begin
        if groupCounts.Count > 0 then
          for i := 0 to groupCounts.Count - 1 do
            if Assigned(groupCounts.Items[i]) then
              TGroupCount(groupCounts.Items[i]).Free;
        groupCounts.Free;
      end;
      D_InputSeqData.UpdateDispTaxaList;
      UpdateStatusBar(Sender);
      DataGrid.Invalidate;
    end;
  end;
end;

procedure TV_SeqDataExplorer.ActionSelectOneFromEachGroupExecute(Sender: TObject);
var
  aList: TStringList = nil;
  otuInfo: TOtuInfo = nil;
  i: Integer;
begin
  try
    aList := TStringList.Create;
    aList.Sorted := True;
    aList.Duplicates := dupAccept;
    for i := 0 to D_InputSeqData.NoOfTaxa - 1 do
    begin
      otuInfo := D_InputSeqData.OtuInfos[i];
      if otuInfo.IsUsed and (Trim(otuInfo.GpName) <> EmptyStr) then
      begin
        if aList.IndexOf(otuInfo.GpName) < 0 then
        begin
          aList.Add(otuInfo.GpName);
          otuInfo.IsUsed := True
        end
        else
          otuInfo.IsUsed := False
      end;
    end;
  finally
    D_InputSeqData.UpdateDispTaxaList;
    UpdateStatusBar(Sender);
    DataGrid.Invalidate;
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TV_SeqDataExplorer.ActionGroupSeqsByIdentityExecute(Sender: TObject);
var
  minFreq: Double = -1;
  maxFreq: Double = -1;
  t: TAlleleFreqsThread = nil;
  Arp: TRuntimeProgress = nil;
  options: TSequenceIdentityOptionsDlg = nil;
  response: Integer;
begin
  if not (D_InputSeqData.OtuInfos.NoOfSelOtus > 1) then
  begin
    ShowMessage('grouping by sequence identity requires that at least 2 sequences are selected');
    Exit;
  end;

  try
    options := TSequenceIdentityOptionsDlg.Create(Self);
    repeat
      response := options.ShowModal;
      minFreq := options.MinCutoffValue;
      maxFreq := options.MaxCutoffValue;
    until (response = mrCancel) or (response = mrOK);
    if response = mrOk then
    begin
      try
        if options.FirstSequenceIsReference and (not D_InputSeqData.OtuInfos[0].IsUsed) then
          raise Exception.Create('when using the first sequence to reference major alleles, it must be selected');
        Arp := TRuntimeProgress.Create;
        Arp.SetKeepOnTop(True);
        Arp.HideAnalysisOptions;
        Arp.Show;
        t := TAlleleFreqsThread.Create(Arp, minFreq/100, maxFreq/100, options.FirstSequenceIsReference, options.UseRefSeqInFrequencyCalculation);
        t.OnTerminate := AlleleFreqSearchDone;
        t.Start;
      except
        on E:Exception do
          ShowMessage('Error: ' + E.Message);
      end;
    end;
  finally
    if Assigned(options) then
      options.Free;
  end;
end;

procedure TV_SeqDataExplorer.ActionShowDomainBoundariesExecute(Sender: TObject);
begin
  ActionShowDomainBoundaries.Checked := not ActionShowDomainBoundaries.Checked;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionShowFindExecute(Sender: TObject);
begin
  if Sender <> nil then // if the action is invoked by the ctrl+f then it will have a sender.  In this case we want to show both types of search.
  begin
    ShowTaxaSearch;
    ShowMotifSearch;
  end;
  MainSearchPanel.Visible := True;
  ActionTranslate.ShortCut := 0;
  ActionHighlightConserved.ShortCut := 0;
  ActionHighlightVariableSites.ShortCut := 0;
  ActionHighlightParsimInfoSites.Shortcut := 0;
  ActionHighlightSingletonSites.ShortCut := 0;
  ActionHighlight0FoldDegenSites.ShortCut := 0;
  ActionHighlight2FoldDegenSites.ShortCut := 0;
  ActionHighlight4FoldDegenSites.ShortCut := 0;
end;

procedure TV_SeqDataExplorer.ActionShowOnlySelectedExecute(Sender: TObject);
begin
  VS_SeqDataExplorer.DispSelTaxaItem := (not VS_SeqDataExplorer.DispSelTaxaItem);
  ActionShowOnlySelected.Checked := VS_SeqDataExplorer.DispSelTaxaItem;
  D_InputSeqData.UpdateDispTaxaList;
  D_InputSeqData.ReferenceSeq := D_InputSeqData.Sequence[0];
  DataGrid.RowCount := D_InputSeqData.DispTaxaList.Count+1;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionSortByNameAscExecute(Sender: TObject);
begin
  D_InputSeqData.FOtuInfos.SortByTaxaNameAsc;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionSortByNameDescExecute(Sender: TObject);
begin
  D_InputSeqData.FOtuInfos.SortByTaxaNameDes;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionSortBySimilarityToFirstSequenceExecute(
  Sender: TObject);
begin
  D_InputSeqData.FOtuInfos.CountDifferencesFromFirstVisibleSequence;
  D_InputSeqData.FOtuInfos.SortBySimilarityToFirstOtu;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionSortByTaxaGpsDlgExecute(Sender: TObject);
begin
  D_InputSeqData.FOtuInfos.SortByTaxaGpsDlg;
  D_InputSeqData.UpdateDispTaxaList;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionSortSequencesExecute(Sender: TObject);
begin
  DispSortBySeqNameItem.Checked := False;
  DispSortByGpItem.Checked := False;
  ByGpAndSeqNamesItem.Checked := False;
  AsperTaxaGroupOrganizerItem.Checked := False;

  if Sender = DispSortBySeqNameItem then
  begin
    D_InputSeqData.FOtuInfos.SortByTaxaNameAsc;
    DispSortBySeqNameItem.Checked := True;
  end
  else if Sender = DispSortByGpItem then
  begin
    D_InputSeqData.FOtuInfos.SortByGroupNameAsc;
    DispSortByGpItem.Checked := True;
  end
  else if Sender = ByGpAndSeqNamesItem then
  begin
    D_InputSeqData.FOtuInfos.SortByGroupAndTaxaName;
    ByGpAndSeqNamesItem.Checked := True;
  end
  else if Sender = AsperTaxaGroupOrganizerItem then
  begin
    D_InputSeqData.FOtuInfos.SortByTaxaGpsDlg;
    AsperTaxaGroupOrganizerItem.Checked := True;
  end;
  D_InputSeqData.UpdateDispTaxaList;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionStatDispInCsvExecute(Sender: TObject);
begin
  ActionStatDispInXL.Checked := False;
  ActionStatDispInXls.Checked := False;
  ActionStatDispInOds.Checked := False;
  ActionStatDispInCSV.Checked := True;
  ActionStatDispInText.Checked := False;
  VS_SeqDataExplorer.StatDispInXLItem := False;
  VS_SeqDataExplorer.StatDispInXlsItem := False;
  VS_SeqDataExplorer.StatDispInOdsItem := False;
  VS_SeqDataExplorer.StatDispInCSVItem := True;
  VS_SeqDataExplorer.StatDispInTextItem := False;
end;

procedure TV_SeqDataExplorer.ActionStatDispInOdsExecute(Sender: TObject);
begin
  ActionStatDispInXL.Checked := False;
  ActionStatDispInXls.Checked := False;
  ActionStatDispInOds.Checked := True;
  ActionStatDispInCSV.Checked := False;
  ActionStatDispInText.Checked := False;
  VS_SeqDataExplorer.StatDispInXLItem := False;
  VS_SeqDataExplorer.StatDispInXlsItem := False;
  VS_SeqDataExplorer.StatDispInOdsItem := True;
  VS_SeqDataExplorer.StatDispInCSVItem := False;
  VS_SeqDataExplorer.StatDispInTextItem := False;
end;

procedure TV_SeqDataExplorer.ActionStatDispInTextExecute(Sender: TObject);
begin
  ActionStatDispInXL.Checked := False;
  ActionStatDispInXls.Checked := False;
  ActionStatDispInOds.Checked := False;
  ActionStatDispInCSV.Checked := False;
  ActionStatDispInText.Checked := True;
  VS_SeqDataExplorer.StatDispInXLItem := False;
  VS_SeqDataExplorer.StatDispInXlsItem := False;
  VS_SeqDataExplorer.StatDispInOdsItem := False;
  VS_SeqDataExplorer.StatDispInCSVItem := False;
  VS_SeqDataExplorer.StatDispInTextItem := True;
end;

procedure TV_SeqDataExplorer.ActionStatDispInXlExecute(Sender: TObject);
begin
  ActionStatDispInXL.Checked := True;
  ActionStatDispInXls.Checked := False;
  ActionStatDispInOds.Checked := False;
  ActionStatDispInCSV.Checked := False;
  ActionStatDispInText.Checked := False;
  VS_SeqDataExplorer.StatDispInXLItem := True;
  VS_SeqDataExplorer.StatDispInXlsItem := False;
  VS_SeqDataExplorer.StatDispInOdsItem := False;
  VS_SeqDataExplorer.StatDispInCSVItem := False;
  VS_SeqDataExplorer.StatDispInTextItem := False;
end;

procedure TV_SeqDataExplorer.ActionStatDispInXlsExecute(Sender: TObject);
begin
  ActionStatDispInXL.Checked := False;
  ActionStatDispInXls.Checked := True;
  ActionStatDispInOds.Checked := False;
  ActionStatDispInCSV.Checked := False;
  ActionStatDispInText.Checked := False;
  VS_SeqDataExplorer.StatDispInXLItem := False;
  VS_SeqDataExplorer.StatDispInXlsItem := True;
  VS_SeqDataExplorer.StatDispInOdsItem := False;
  VS_SeqDataExplorer.StatDispInCSVItem := False;
  VS_SeqDataExplorer.StatDispInTextItem := False;
end;

procedure TV_SeqDataExplorer.ActionToggleUnlabeledSitesExecute(Sender: TObject);
begin
  ActionToggleUnlabeledSites.Checked := not ActionToggleUnlabeledSites.Checked;
  D_InputSeqData.UseUnlabeledSites := ActionToggleUnlabeledSites.Checked;
  DataGrid.Invalidate;
  UpdateStatusBar(Sender);
end;

procedure TV_SeqDataExplorer.ActionExportDataExecute(Sender: TObject);
var
  ExportDataDlg: TSeqDataExportOptionsDlg = nil;
  Results : TStringList = nil;
  ExportOptions : TSeqExportOptions = nil;
  aDestination: String = '';
begin
  try
    try
      ExportDataDlg := TSeqDataExportOptionsDlg.Create(Self);
      ExportDataDlg.IsNucData := D_InputSeqData.IsNuc;
      ExportDataDlg.IsChooseBases := (D_InputSeqData.IsNuc and D_InputSeqData.IsCoding and (not VS_SeqDataExplorer.IsTranslated));

      ExportDataDlg.FileTitle       := MegaForm.DataTitle;
      ExportDataDlg.FileDescription := MegaForm.DataDescription;
      ExportDataDlg.HasHighlightedSites := VS_SeqDataExplorer.CurAttrDisp <> megNone;

      if Sender= ExportCSVTBtn then
        ExportDataDlg.ForceCSVFormat
      else if Sender = ExportXLTBtn then
        ExportDataDlg.ForceExcelFormat
      else if Sender = ExportMEGATBtn then
        ExportDataDlg.ForceMEGAFormat;

      if ExportDataDlg.ShowModal = mrOK then
      begin
        if ExportDataDlg.IsExcelFormat then
        begin
           if ExportDataDlg.SitesPerLineSE.Value > 250 then
             ExportDataDlg.SitesPerLineSE.Value := 250;
        end;

        Results := TStringList.Create;
        ExportOptions := ExportDataDlg.ExportOptions;

        aDestination := MegaForm.DataFileName;
        if Trim(aDestination) = EmptyStr then
          aDestination := 'sequence_data';
        if GetFileExportDestination(ExportOptions.ExportFormat, aDestination) then
        begin
          D_InputSeqData.UpdateSiteAttrArray;
          if ExportOptions.IsEXCELFormat or ExportOptions.IsExcelXmlFormat then
            ExportOptions.IsInterleaved := True;
          LaunchDataExportThread(ExportOptions, aDestination, Results);
        end;
      end;
    Except
      on E: Exception do
      begin
        ShowMessage('Error: ' + E.Message);
      end;
    end;
  finally;
    if Assigned(ExportDataDlg) then
      ExportDataDlg.Free;
  end;
end;

procedure TV_SeqDataExplorer.ActionExportNamesExecute(Sender: TObject);
var
  aList: TStringList = nil;
begin
  try
    aList := D_InputSeqData.GetTaxaNamesList;
    OpenStringList(aList, 'Sequence Names', True);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TV_SeqDataExplorer.ActionEditGenesDomainsExecute(Sender: TObject);
var
  PleaseWait: TPleaseWait = nil;
begin
  if GeneDomainDlg = nil then
    Exit;
  GeneDomainDlg.AutoHighlightSitesBtn.OnClick := AutoHighlightSites;
  GeneDomainDlg.ShowModal;
  updateBirdsGeneDomain;
  if D_InputSeqData.FDomainMarks.IsDirty then
  begin
    try
      try
        PleaseWait := TPleaseWait.Create(self);
        PleaseWait.Action := 'Genes/Domains';
        PleaseWait.Caption := VER_MEGA_WIN_CAPTION_PREFIX+': Updating...';
        PleaseWait.Show;
        ApplyUpdate;
        D_InputSeqData.FDomainMarks.IsDirty := False;
        DataGrid.Invalidate;
        ActionHighlightLabelledSites.Enabled := D_InputSeqData.HasSiteLabels and (not VS_SeqDataExplorer.IsTranslated);
      except
        on E: Exception do
          ShowMessage('Oh no! An error has occurred: ' + E.Message);
      end
    finally
      D_InputSeqData.HideProgressDlg;
      if FForcingHighlightUpdate then
        FForcingHighlightUpdate := False;
      FreeAndNil(PleaseWait);
    end;
  end;
end;

procedure TV_SeqDataExplorer.ActionColorCellsExecute(Sender: TObject);
begin
  VS_SeqDataExplorer.DispColorItem := (not VS_SeqDataExplorer.DispColorItem);
  ActionColorCells.Checked := VS_SeqDataExplorer.DispColorItem;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionCoverageExecute(Sender: TObject);
begin
  HighlightSitesClick(Sender);
end;

procedure TV_SeqDataExplorer.ActionCpGExecute(Sender: TObject);
begin
  HighlightSitesClick(Sender);
end;

procedure TV_SeqDataExplorer.ActionDeleteDomainExecute(Sender: TObject);
var
  aInfo: TDomainInfo;
  i, mResponse: Integer;
begin
  aInfo := D_InputSeqData.GetDomainMark(editDomainAt);
  Assert(Assigned(aInfo) and aInfo.IsDomain);
  if not Assigned(aInfo) then
    Exit;
  mResponse := MessageDlg('Confirm Deletion', 'Are you sure you want to delete the domain named ' + aInfo.Name, mtConfirmation, mbYesNo, 0);
  if mResponse <> mrYes then
    Exit;
  for i := aInfo.FromSite to aInfo.ToSite do
    D_InputSeqData.FDomainMarks[i] := nil;
  D_InputSeqData.AllDomainInfo.Remove(aInfo);
  GeneDomainDlg.RemoveInfo(aInfo);
  FreeAndNil(aInfo);
  updateBirdsGeneDomain;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionDeleteGeneExecute(Sender: TObject);
var
  aInfo: TDomainInfo;
  i: Integer;
  mResponse: Integer;
  childInfos: TList = nil;
  childInfo: TDomainInfo = nil;
begin
  aInfo := D_InputSeqData.GetDomainMark(editDomainAt);
  if aInfo.IsDomain then
    aInfo := aInfo.ParentDomain;
  Assert(Assigned(aInfo) and aInfo.IsGene);
  if not Assigned(aInfo) then
  begin
    ShowMessage('Oh no! No gene info was found for those coordinates');
    Exit;
  end;
  mResponse := MessageDlg('Confirm Deletion', 'Are you sure you want to delete the gene named ' + aInfo.GeneName, mtConfirmation, mbYesNo, 0);
  if mResponse <> mrYes then
    Exit;

  try
    BeginFormUpdate;
    for i := aInfo.FromSite to aInfo.ToSite do
      D_InputSeqData.FDomainMarks[i] := nil;
    childInfos := aInfo.ChildDomains;
    if childInfos.Count > 0 then
    begin
      for i := 0 to childInfos.Count - 1 do
      begin
        childInfo := TDomainInfo(childInfos[i]);
        D_InputSeqData.AllDomainInfo.Remove(childInfo);
        GeneDomainDlg.RemoveInfo(childInfo);
        FreeAndNil(childInfo);
      end;
    end;
    D_InputSeqData.AllDomainInfo.Remove(aInfo);
    GeneDomainDlg.RemoveInfo(aInfo);
    updateBirdsGeneDomain;
    DataGrid.Invalidate;
  finally
    EndFormUpdate;
    if Assigned(aInfo) then
      aInfo.Free;
  end;
end;

procedure TV_SeqDataExplorer.ActionDirectionalPairsExecute(Sender: TObject);
var
  SaveLocation: String;
  exportAs: TExportType;
begin
  try
    SaveLocation := MegaForm.DataFileNameOnly + '-Directional-Pair-Freqs.txt';
    exportAs := SelectedStatsExportType;
    exportAs := PromptUserWriteOutput(SaveLocation, True, exportAs);
    MegaForm.LaunchStatPairFreqsThread(SaveLocation, exportAs, True);
  except
    on E: Exception do
      ShowMessage('Application Error: Unable to perform statistical analysis - ' + E.Message);
  end;
end;

procedure TV_SeqDataExplorer.ActionDisableIndependentSitesExecute(Sender: TObject);
begin
  D_InputSeqData.FDomainMarks.IsUsedIndependent := (not D_InputSeqData.FDomainMarks.IsUsedIndependent);
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionChangeFontExecute(Sender: TObject);
begin
  FontDialog1.Font := DataGrid.Canvas.Font;
  if FontDialog1.Execute then
  begin
    VS_SeqDataExplorer.CurFont.Assign(FontDialog1.Font);
    DataGrid.Font := FontDialog1.Font;
    with DataGrid, Canvas do
    begin
      Font := FontDialog1.Font;
      UpdateGridSizes;
      DataGrid.Invalidate;
    end;
  end;
end;

procedure TV_SeqDataExplorer.ActionClearSiteLabelsExecute(Sender: TObject);
begin
  if ActionHighlightLabelledSites.Checked then
    ActionHighlightLabelledSitesExecute(Sender);
  if Assigned(D_InputSeqData) and Assigned(D_InputSeqData.FDomainMarks) then
    D_InputSeqData.FDomainMarks.ClearSiteMarks;
  UpdateStatusBar(Sender);
end;

procedure TV_SeqDataExplorer.ActionAminoAcidCompositionExecute(Sender: TObject);
begin
  MegaForm.AminoAcidCompActionExecute(Self);
end;

procedure TV_SeqDataExplorer.ActionAutoLabelSitesExecute(Sender: TObject);
begin
  GeneDomainDlg.PageControl1.ActivePage := GeneDomainDlg.SiteLabelsTabSheet;
  GeneDomainDlg.ShowHintLabel;
  ActionEditGenesDomainsExecute(Sender);
  GeneDomainDlg.HideHintLabel;
  AdjustEnabledStatus;
end;

procedure TV_SeqDataExplorer.ActionAllSelectedSitesExecute(Sender: TObject);
begin
  ActionAllSelectedSites.Checked := True;
  ActionHighlightedSites.Checked := False;
  VS_SeqDataExplorer.StatAllSelSitesItem := True;
  VS_SeqDataExplorer.StatUseOnlyMarkedSitesItem := False;
end;

procedure TV_SeqDataExplorer.ActionAddGroupExecute(Sender: TObject);
var
  newGpStr: String = '';
  i, startRow, endRow: Integer;
  promptNewGroupNameStr, tempIntStr: String;
  newRangeStr: String = '';
begin
  // If user doesn't have rows selected, then give them the option of specifying a range.
  if VS_SeqDataExplorer.CurColorRowRange.X = -1 then
  begin
    promptNewGroupNameStr := 'Group Name';
    if InputQuery('Taxa in new group', 'What taxa (or range) should be in the new group ex. 1-10', newRangeStr) then
    begin
      try
        endRow := -1;
        newRangeStr := trim(newRangeStr);
        if pos('-', newRangeStr) > 0 then  // if there is a '-' range, then select the first int.
          tempIntStr := copy(newRangeStr, 0, pos('-', newRangeStr)-1)
        else
          tempIntStr := newRangeStr;
        if not TryStrToInt(tempIntStr, startRow) then
          raise Exception.Create('Error Message');

        if pos('-', newRangeStr) > 0 then // if there is a '-' range, then select the second int.
        begin
          tempIntStr := trim(copy(newRangeStr, pos('-', newRangeStr)+1, length(newRangeStr)));
          if not TryStrToInt(tempIntStr, endRow) then
            raise Exception.Create('Error Message');
        end
        else
          endRow := startRow;
        if (startRow <= 0) or (startRow > D_InputSeqData.NoOfTaxa) or (endRow > D_InputSeqData.NoOfTaxa) or (endRow <= 0) or (startRow > endRow) then
          raise Exception.Create('Error Message');
        VS_SeqDataExplorer.CurColorRowRange := Point(startRow, endRow);
        ResetGroupCounts;
        DataGrid.Invalidate;
      Except on E: Exception do
        begin
          MessageDlg('Invalid row or range.  '+LineEnding+''+LineEnding+'For a single row, type the row number (listed next to the taxa name).'+#13+#10+'For a range, type the first row, a dash (-), then the end row. ex. 1-10', mtWarning, [mbOK], 0);
          Exit;
        end;
      end;
    end
    else
      Exit;
  end
  else
    promptNewGroupNameStr := 'Group (for selected taxa) Name';
  if InputQuery('New Group', promptNewGroupNameStr, newGpStr) then
  begin
    for I := VS_SeqDataExplorer.CurColorRowRange.X to VS_SeqDataExplorer.CurColorRowRange.Y do
    begin
      D_InputSeqData.OtuInfos.Otu[i-1].GpName := newGpStr;
      if SameText(newGpStr, 'outgroup') then
        D_InputSeqData.OtuInfos.Otu[i-1].OutgroupMember := True
      else
        D_InputSeqData.OtuInfos.Otu[i-1].OutgroupMember := False;
    end;

    D_InputSeqData.OtuInfos.IsDirty := true;
    DataGrid.Invalidate;
    updateFixedColWidth;
    ResetGroupCounts;
  end
  else
    MessageDlg('Group has not been added.', mtWarning, [mbOK], 0);
end;

procedure TV_SeqDataExplorer.ActionAddPopulationExecute(Sender: TObject);
var
  newPopStr: String = '';
  i: Integer;
begin
if InputQuery('New Population', 'New Population Name', newPopStr) then
  begin
    for i:=VS_SeqDataExplorer.CurColorRowRange.X to VS_SeqDataExplorer.CurColorRowRange.Y do
      D_InputSeqData.OtuInfos.Otu[i-1].PopName := newPopStr;
    D_InputSeqData.OtuInfos.IsDirty := true;
    DataGrid.Invalidate;
    updateFixedColWidth;
  end
  else
    MessageDlg('Population has not been added.', mtWarning, [mbOK], 0);
end;

procedure TV_SeqDataExplorer.ActionAddSiteLabelListExecute(Sender: TObject);
var
  response: Integer;
begin
  if VS_SeqDataExplorer.IsTranslated then
  begin
    response := MessageDlg('Not supported for translated data', 'Site labels are not supported for translated data. Untranslate the data and continue?', mtConfirmation, mbYesNo, 0);
    if response = mrNo then
      Exit;
    ActionTranslateExecute(Sender);
  end;
  EditLabelsPanel.Visible := True;
  FEditLabelsFrame.Clear;
  try
    FUpdatingHighlightState := True;
    FEditLabelsFrame.HighlighLabelledSitesCBox.Checked := ActionHighlightLabelledSites.Checked;
    AdjustEnabledStatus;
  finally
    FUpdatingHighlightState := False;
  end;
end;

procedure TV_SeqDataExplorer.ActionAddSpeciesExecute(Sender: TObject);
var
  newSpStr: String = '';
  i: Integer;
begin
  if InputQuery('New Species', 'New Species Name', newSpStr) then
  begin
    for i:=VS_SeqDataExplorer.CurColorRowRange.X to VS_SeqDataExplorer.CurColorRowRange.Y do
      D_InputSeqData.OtuInfos.Otu[i-1].SpName := newSpStr;
    D_InputSeqData.OtuInfos.IsDirty := true;
    DataGrid.Invalidate;
    updateFixedColWidth;
  end
  else
    MessageDlg('Species has not been added.', mtWarning, [mbOK], 0);
end;

procedure TV_SeqDataExplorer.ActionAddGeneExecute(Sender: TObject);
var
  mResponse: Integer;
  aInfo: TDomainInfo = nil;
  parentInfo: TDomainInfo = nil;
  i: Integer;
begin
  try
    if not Assigned(FGeneDomainsPropsEditor) then
      InitGeneDomainsPropertyEditor;
    FGeneDomainsPropsEditor.IsEditingGene := True;
    parentInfo := D_InputSeqData.FDomainMarks[editDomainAt];
    if Assigned(parentInfo) and parentInfo.IsGene then
    begin
      ShowMessage('cannot add a gene to a gene');
      Exit;
    end;
    aInfo := TDomainInfo.Create;
    aInfo.GeneName := GeneDomainDlg.NextAvailableGeneName;
    aInfo.FromSite := editDomainAt;
    aInfo.ToSite := editDomainAt;
    aInfo.IsGene := True;
    aInfo.IsUsed := True;
    FGeneDomainsPropsEditor.InitFromDomainInfo(aInfo);
    mResponse := FGeneDomainsPropsEditor.ShowModal;
    if mResponse = mrOk then
    begin
      FGeneDomainsPropsEditor.GetDomainInfoProperties(aInfo);
      for i := aInfo.FromSite to aInfo.ToSite do
        D_InputSeqData.FDomainMarks[i] := aInfo;
      GeneDomainDlg.AddInfo(aInfo);
      updateBirdsGeneDomain;
    end
    else
      FreeAndNil(aInfo);
    DataGrid.Invalidate;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred' + E.Message);
  end;
end;

procedure TV_SeqDataExplorer.ActionAddDomainExecute(Sender: TObject);
var
  mResponse: Integer;
  aInfo: TDomainInfo = nil;
  parentInfo: TDomainInfo = nil;
  i: Integer;
begin
  try
    try
      BeginFormUpdate;
      if not Assigned(FGeneDomainsPropsEditor) then
        InitGeneDomainsPropertyEditor;
      FGeneDomainsPropsEditor.IsEditingGene := False;
      parentInfo := D_InputSeqData.FDomainMarks[editDomainAt];
      if Assigned(parentInfo) and (not parentInfo.IsGene) then
      begin
        ShowMessage('cannot add a domain to another domain');
        Exit;
      end;
      aInfo := TDomainInfo.Create;
      aInfo.Name := GeneDomainDlg.NextAvailableDomainName;
      if Assigned(parentInfo) and parentInfo.IsGene then
      begin
        aInfo.GeneName := parentInfo.GeneName;
        aInfo.ParentDomain := parentInfo;
        ParentInfo.ChildDomains.Add(aInfo);
      end;
      aInfo.FromSite := editDomainAt;
      aInfo.ToSite := editDomainAt;
      aInfo.IsGene := False;
      aInfo.IsUsed := True;
      FGeneDomainsPropsEditor.InitFromDomainInfo(aInfo);
      mResponse := FGeneDomainsPropsEditor.ShowModal;
      if mResponse = mrOk then
      begin
        FGeneDomainsPropsEditor.GetDomainInfoProperties(aInfo);
        for i := aInfo.FromSite to aInfo.ToSite do
          D_InputSeqData.FDomainMarks[i] := aInfo;
        GeneDomainDlg.AddInfo(aInfo);
        updateBirdsGeneDomain;
      end
      else
        FreeAndNil(aInfo);
      DataGrid.Invalidate;
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred' + E.Message);
    end;
  finally
    EndFormUpdate;
  end;
end;

procedure TV_SeqDataExplorer.ActionAddDomainsForGroupIdentityComparisonSitesExecute(Sender: TObject);
var
  response: Integer;
  i: Integer;
  aInfo: TDomainInfo = nil;
begin
  if D_InputSeqData.DomainMarks.IndependentCount = 0 then
  begin
    response := MessageDlg('Overwrite Existing Domain?', 'Independent sites are needed to add new domains. Do you want to overwrite existing domain information?', mtConfirmation, mbYesNo, 0);
    if response = mrNo then
      Exit;
    if D_InputSeqData.AllDomainInfo.NoOfDomains > 0 then
      for i := 0 to D_InputSeqData.AllDomainInfo.NoOfDomains - 1 do
      begin
        aInfo := D_InputSeqData.AllDomainInfo[i];
        D_InputSeqData.AllDomainInfo.Remove(aInfo);
        GeneDomainDlg.RemoveInfo(aInfo);
        D_InputSeqData.DomainMarks.ClearAll;
      end;
  end;
  D_InputSeqData.AddDomainsToSitesUsedFromMinorAlleleIdentityComparisons;
  ApplyUpdate;
end;

procedure TV_SeqDataExplorer.ActionCodonUsageExecute(Sender: TObject);
begin
  MegaForm.CodonUsageActionExecute(Self);
end;

procedure TV_SeqDataExplorer.ActionDispBirdsEyeViewExecute(Sender: TObject);
begin
  VS_SeqDataExplorer.DispBirdsEyeViewItem := not VS_SeqDataExplorer.DispBirdsEyeViewItem;
  ActionDispBirdsEyeView.Checked := VS_SeqDataExplorer.DispBirdsEyeViewItem;
  bird.Visible := VS_SeqDataExplorer.DispBirdsEyeViewItem;
  if bird.Visible then
    updateBirdsGeneDomain;
end;

procedure TV_SeqDataExplorer.ActionDispGroupNamesExecute(Sender: TObject);
begin
  VS_SeqDataExplorer.DispGpNamesItem := (not VS_SeqDataExplorer.DispGpNamesItem);
  ActionDispGroupNames.Checked := VS_SeqDataExplorer.DispGpNamesItem;
  if ActionDispGroupNames.Checked and (not Assigned(FGroupCounts)) then
    ResetGroupCounts;
  ActionDisplayGroupCounts.Enabled := ActionDispGroupNames.Checked;
  DataGrid.ColCount := NumberOfFixedColsVisible + D_InputSeqData.FNoOfSites;
  DataGrid.FixedCols := NumberOfFixedColsVisible;
  UpdateFixedColWidth;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionDisplayGroupCountsExecute(Sender: TObject);
begin
  ActionDisplayGroupCounts.Checked := not ActionDisplayGroupCounts.Checked;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionDispTaxaNamesExecute(Sender: TObject);
begin
  VS_SeqDataExplorer.DispTaxaNamesItem := (not VS_SeqDataExplorer.DispTaxaNamesItem);
  ActionDispTaxaNames.Checked := VS_SeqDataExplorer.DispTaxaNamesItem;
  DataGrid.ColCount := NumberOfFixedColsVisible + D_InputSeqData.FNoOfSites;
  DataGrid.FixedCols := NumberOfFixedColsVisible;
  UpdateFixedColWidth;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionEditDomainExecute(Sender: TObject);
var
  mResponse: Integer;
  aInfo: TDomainInfo = nil;
  parentInfo: TDomainInfo = nil;
begin
  try
    if not Assigned(FGeneDomainsPropsEditor) then
      InitGeneDomainsPropertyEditor;
    FGeneDomainsPropsEditor.IsEditingGene := False;
    aInfo := D_InputSeqData.DomainMark[editDomainAt];
    if not Assigned(aInfo) then
    begin
      ShowMessage('Oh no! Domain info was not found at those coordinates');
      Exit;
    end;
    FGeneDomainsPropsEditor.InitFromDomainInfo(aInfo);
    mResponse := FGeneDomainsPropsEditor.ShowModal;
    if mResponse = mrOk then
    begin
      UpdateMarksForDomain(aInfo);
      parentInfo := aInfo.ParentDomain;
      if Assigned(parentInfo) then
        UpdateGeneCoordinates(parentInfo, aInfo.FromSite, aInfo.ToSite);
      updateBirdsGeneDomain;
      DataGrid.Invalidate;
    end;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred' + E.Message);
  end;
end;

procedure TV_SeqDataExplorer.ActionEditGroupsExecute(Sender: TObject);
var
  PleaseWait: TPleaseWait;
begin
  if TaxaGpsDlg = nil then
    Exit;
  TaxaGpsDlg.fillGroupTreeViewFromOtuInfo;
  TaxaGpsDlg.GroupTagSourceComboBox.ItemIndex := GroupTagCheckedIndex;
  TaxaGpsDlg.GroupsChanged := False;
  TaxaGpsDlg.ShowModal;
  // now if info is changed
  if D_InputSeqData.FOtuInfos.IsDirty or TaxaGpsDlg.GroupsChanged then
  begin
    try try
      UpdateGroupTagMenuCheckedItem(TaxaGpsDlg.GroupTagSourceComboBox.ItemIndex);
      PleaseWait := TPleaseWait.Create(self);
      PleaseWait.Action := 'Taxa/Groups';
      PleaseWait.Caption := VER_MEGA_WIN_CAPTION_PREFIX+': Updating...';
      PleaseWait.Show;
      ResetGroupCounts;
      if D_InputSeqData.FOtuInfos.IsDirty then
      begin
        ApplyUpdate;
        D_InputSeqData.FOtuInfos.IsDirty := False;
      end;
      SetGroupTagButton.Enabled := D_InputSeqData.HasAnyGeographicalInfo;
    except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end
    finally
      FreeAndNil(PleaseWait);
    end;
  end;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionExportSearchResultsExecute(Sender: TObject);
var
  OutList: TStringList=nil;
  SaveLoc: String;
  APleaseWait: TPleaseWait=nil;
begin
  try
    try
      SaveLoc := MegaForm.DataFileNameOnly + '-Search-Results.txt';
      if GetFileExportDestination(mefTxt, SaveLoc) then
      begin
        OutList := TStringList.Create;
        APleaseWait := TPleaseWait.Create(nil);
        APleaseWait.Action := 'Exporting Search Results...';
        APleaseWait.Show;
        APleaseWait.PercentDone := 0;
        D_InputSeqData.WriteSearchResultsOnALine(OutList, SaveLoc, EXtext, APleaseWait);
        APleaseWait.PercentDone := 100;
        OutList.SaveToFile(SaveLoc);
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! An error occurred when exporting search results: ' + E.Message);
    end;
  finally
    if Assigned(OutList) then
      OutList.Free;
    if Assigned(APleaseWait) then
      APleaseWait.Free;
  end;
end;

procedure TV_SeqDataExplorer.ActionExportSearchResultsUpdate(Sender: TObject);
begin
  ActionExportSearchResults.Enabled := (D_InputSeqData.NoOfSearchResults > 0);
end;

procedure TV_SeqDataExplorer.ActionFindMotifExecute(Sender: TObject);
begin
  UpdateMotifCountLabel(EmptyStr, -1);
  ActionShowFindExecute(nil);
  ShowMotifSearch;
end;

procedure TV_SeqDataExplorer.ActionFindNextExecute(Sender: TObject);
begin
  case searchTypeCbx.ItemIndex of
    // Name
    0:
    begin
      ActionFindNextSeqExecute(Sender);
    end;
    // Group
    1:
    begin
      ActionFindNextGrpExecute(Sender);
    end;
    // Motif
    2:
    begin
      ActionFindNextSingleMotifExecute(Sender);
    end;
  end;
end;

procedure TV_SeqDataExplorer.ActionFindNextGrpExecute(Sender: TObject);
var
   GroupAt: Integer;
   ARP: TRuntimeProgress = nil;
begin
  ARP := TRuntimeProgress.Create(nil);
  ARP.UpdateRunStatusInfo('Status', 'Searching for sequence');
  try
      with DataGrid do
      begin
        GroupAt := D_InputSeqData.FindGroup(VS_SeqDataExplorer.CurGroupStr, Datagrid.Row+1, ARP); //Returns -1 if couldn't find it otherwise the row#
        if (GroupAt = -1) then
        begin
          ShowMessage('No more instances of the group name were found.');
          Exit;
        end
        else
        begin
          Row :=GroupAt;
          VS_SeqDataExplorer.CurColorRowRange := Point(GroupAt, GroupAt); //Let the visual status know what row we're on
          SeqNameResult := D_InputSeqData.TaxaName[GroupAt-1];
          Invalidate;
        end;
      end;
  finally
    if Assigned(ARP) then
      ARP.Free;
  end;
end;

procedure TV_SeqDataExplorer.ActionFindNextMotifExecute(Sender: TObject);
begin
  SearchNextSingleMotif(VS_SeqDataExplorer.CurMotifStr, DataGrid.Row-1, DataGrid.Col);
end;

procedure TV_SeqDataExplorer.ActionFindNextSeqExecute(Sender: TObject);
var
   NameAt: Integer;
   ARP: TRuntimeProgress;
begin
  ARP := TRuntimeProgress.Create(nil);
  ARP.UpdateRunStatusInfo('Status', 'Searching for sequence');
  try
    with DataGrid do
    begin
      NameAt := D_InputSeqData.FindSequence(VS_SeqDataExplorer.CurSearchStr, Datagrid.Row+1, ARP); //Returns -1 if couldn't find it otherwise the row#
      if (NameAt = -1) then
      begin
        ShowMessage('No more instances of the query name were found.');
        Exit;
      end
      else
      begin
        Row :=NameAt;  //we've found it so set the grid to that row
        VS_SeqDataExplorer.CurColorRowRange := Point(NameAt, NameAt); //Let the visual status know what row we're on
        SeqNameResult := D_InputSeqData.TaxaName[NameAt-1];
        Invalidate;   //When something changes the grid will totally re-draw
      end;
    end;
  finally
    FreeAndNil(ARP);
  end;
end;

procedure TV_SeqDataExplorer.ActionFindNextSingleMotifExecute(Sender: TObject);
begin
  SearchNextSingleMotif(VS_SeqDataExplorer.CurMotifStr, DataGrid.Row-1, DataGrid.Col);
end;

procedure TV_SeqDataExplorer.ActionFindPrevGrpExecute(Sender: TObject);
var
   GroupAt: Integer;
   ARP: TRuntimeProgress=nil;
begin
  ARP := TRuntimeProgress.Create(nil);
  ARP.UpdateRunStatusInfo('Status', 'Searching for sequence');
  try
    with DataGrid do
    begin
      GroupAt := D_InputSeqData.FindPrevGroup(VS_SeqDataExplorer.CurGroupStr, Datagrid.Row-1, ARP); //Returns -1 if couldn't find it otherwise the row#

      if (GroupAt = -1) then
      begin
        ShowMessage('No more instances of the group name were found.');
        Exit;
      end
      else
      begin
        Row :=GroupAt;
        VS_SeqDataExplorer.CurColorRowRange := Point(GroupAt, GroupAt); //Let the visual status know what row we're on
        SeqNameResult := D_InputSeqData.TaxaName[GroupAt-1];
        Invalidate;
      end;
    end;
  finally
    If Assigned(ARP) then
      ARP.Free;
  end;
end;

procedure TV_SeqDataExplorer.ActionFindPreviousExecute(Sender: TObject);
begin
  case searchTypeCbx.ItemIndex of
    // Name
    0:
    begin
      ActionFindPrevSeqExecute(Sender);
    end;
    // Group
    1:
    begin
      ActionFindPrevGrpExecute(Sender);
    end;
    // Motif
    2:
    begin
      ActionFindPrevSingleMotifExecute(Sender);
    end;
  end;
end;

procedure TV_SeqDataExplorer.ActionFindPrevMotifExecute(Sender: TObject);
var
  i, j: Integer;
  StartAtRow, StartAtCol: Integer;
  SearchingMotif: AnsiString;
  ResultFound: Boolean;
  APW: TPleaseWait;
begin
  ResultFound := False;
  SearchingMotif := VS_SeqDataExplorer.CurMotifStr;
  StartAtRow := DataGrid.Row-1;

  StartAtCol := DataGrid.Col-3;
    if StartAtCol > (D_InputSeqData.NoOfSites-1-Length(SearchingMotif)) then
      StartAtCol := D_InputSeqData.NoOfSites-1-Length(SearchingMotif);

  APW := TPleaseWait.Create(nil);
  APW.Action := 'Searching for previous motif';
  APW.show;
  try
    ActionFindPrevMotif.Enabled := False;
    SearchBtn.Enabled := False;
    for i:=StartAtRow downto 0 do
    begin
      APW.PercentDone := ((StartAtRow+1-i)*100) div (StartAtRow+1); // fixes small div by 0 error in percent done.
      if not D_InputSeqData.OtuInfos.Otu[i].IsUsed then
        Continue;
      for j:=StartAtCol downto 0 do
      begin
        if D_InputSeqData.Sequence[i][j] = '-' then
          continue;
        if D_InputSeqData.CheckKMer(SearchingMotif, i, j, 0, False) then
        begin
          ResultFound := True;
        end;
        if ResultFound then Break;
      end;
      StartAtCol := D_InputSeqData.NoOfSites-1-Length(SearchingMotif);
      if ResultFound then Break;
    end;

    if ResultFound then
    begin
      DataGrid.Row := i+1;
      DataGrid.Col := j+Cols.VisibleCount;
      if (j+Cols.VisibleCount+length(SearchingMotif)) > (DataGrid.LeftCol + DataGrid.VisibleColCount - Cols.VisibleCount) then
        DataGrid.LeftCol := (J+cols.VisibleCount)-length(searchingMotif);
    end
    else
      MessageDlg('There are no more matches to the motif before this location.', mtWarning, [mbOK], 0);
  finally
    ActionFindPrevMotif.Enabled := True;
    SearchBtn.Enabled := True;
    FreeAndNil(APW);
  end;
end;

procedure TV_SeqDataExplorer.ActionFindPrevSeqExecute(Sender: TObject);
var
   NameAt: Integer;
   ARP: TRuntimeProgress;
begin
  ARP := TRuntimeProgress.Create(nil);
  ARP.UpdateRunStatusInfo('Status', 'Searching for sequence');
  try
      with DataGrid do
      begin
        NameAt := D_InputSeqData.FindPrevSequence(VS_SeqDataExplorer.CurSearchStr, Datagrid.Row-1, ARP); //Returns -1 if couldn't find it otherwise the row#
        if (NameAt = -1) then
        begin
          ShowMessage('No more instances of the query name were found.');
          Exit;
        end
        else
        begin
          Row := NameAt;  //we've found it so set the grid to that row
          VS_SeqDataExplorer.CurColorRowRange := Point(NameAt, NameAt); //Let the visual status know what row we're on
          SeqNameResult := D_InputSeqData.TaxaName[NameAt-1];
          Invalidate;   //When something changes the grid will totally re-draw
        end;
      end;
  finally
    FreeAndNil(ARP);
  end;
end;

procedure TV_SeqDataExplorer.ActionFindPrevSingleMotifExecute(Sender: TObject);
var
  i, j: Integer;
  StartAtRow, StartAtCol: Integer;
  SearchingMotif: String;
  ResultFound: Boolean;
  APW: TPleaseWait = nil;
begin
  ResultFound := False;
  SearchingMotif := VS_SeqDataExplorer.CurMotifStr;
  StartAtRow := DataGrid.Row-1;
  if DataGrid.Col >= Length(SearchingMotif) then
    StartAtCol := DataGrid.Col - Max(2, Length(SearchingMotif))
  else
    StartAtCol := DataGrid.Col - (1 + DataGrid.FixedCols);
  if StartAtCol > (D_InputSeqData.NoOfSites-1-Length(SearchingMotif)) then
    StartAtCol := D_InputSeqData.NoOfSites-1-Length(SearchingMotif);
  if D_InputSeqData.TotalNumBases >= MIN_BASES_FOR_SHOWING_PLEASE_WAIT then
  begin
    APW := TPleaseWait.Create(nil);
    APW.Action := 'Searching for previous motif';
    APW.show;
  end;

  try
    ActionFindPrevSingleMotif.Enabled := false;
    SearchBtn.Enabled := False;
    for i:=StartAtRow downto 0 do
    begin
      if Assigned(APW) then
        APW.PercentDone := ((StartAtRow+1-i)*100) div (StartAtRow+1); // fixes small div by 0 error in percent done.
      if not D_InputSeqData.OtuInfos.Otu[i].IsUsed then
        Continue;

      for j:=StartAtCol downto 0 do
      begin
        if D_InputSeqData.Sequence[i][j] = '-' then
          continue;
        if D_InputSeqData.CheckKMer(SearchingMotif, i, j, 0, False) then
        begin
          ResultFound := True;
        end;
        if ResultFound then Break;
      end;
      StartAtCol := D_InputSeqData.NoOfSites-1-Length(SearchingMotif);
      if ResultFound then Break;
    end;

    if ResultFound then
    begin
      DataGrid.Row := i+1;
      DataGrid.Col := j+Cols.VisibleCount;
      if (j+Cols.VisibleCount+length(SearchingMotif)) > (DataGrid.LeftCol + DataGrid.VisibleColCount - Cols.VisibleCount) then
        DataGrid.LeftCol := (J+cols.VisibleCount)-length(searchingMotif);
    end
    else
    begin
      if Assigned(APW) then
        APW.Hide;
      MessageDlg('There are no more matches to the motif before this location.', mtWarning, [mbOK], 0);
    end;
  finally
    if Assigned(APW) then
      APW.Free;
    ActionFindPrevSingleMotif.Enabled := True;
    SearchBtn.Enabled := True;
    DataGrid.Invalidate;
  end;
end;

procedure TV_SeqDataExplorer.ActionFindSeqExecute(Sender: TObject);
var
   NameAt: Integer;
   ARP: TRuntimeProgress=nil;
begin
  ARP := TRuntimeProgress.Create(nil);
  ARP.UpdateRunStatusInfo('Status', 'Searching for sequence');
  try
    begin
      ARP.Show;
      with DataGrid do
      begin
        NameAt := D_InputSeqData.FindSequence(VS_SeqDataExplorer.CurSearchStr, Datagrid.Row, ARP); //Returns -1 if couldn't find it otherwise the row#
          if (NameAt = -1) AND (Row <> 1) then  //Not Found and we didn't start at the top so prompt them if they want to continue from the top
          begin
            ARP.Hide;
            if MessageDlg('Query name not found. Do you want to go to the top and search again?', mtConfirmation, [mbYes, mbNo],0) = mrYes
            then
            begin
              ARP.Show;
              NameAt := D_InputSeqData.FindSequence(VS_SeqDataExplorer.CurSearchStr, 1, ARP);  //start at 1 since we are starting at the top and row 0 is fixed
            end;
          end;

        if (NameAt = -1) then
        begin
          FreeAndNil(ARP);
          ShowMessage('Query name not found.');
          Exit;
        end
        else
        begin
          Row :=NameAt;  //we've found it so set the grid to that row
          VS_SeqDataExplorer.CurColorRowRange := Point(NameAt, NameAt); //Let the visual status know what row we're on
          SeqNameResult := D_InputSeqData.TaxaName[NameAt-1];
          Invalidate;   //When something changes the grid will totally re-draw
          SearchBtn.Visible := False;
          FindPrevBtn.Visible := True;
          FindNextBtn.Visible := True;
        end;
      end;
    end;
  finally
    if Assigned(ARP) then
      ARP.Free;
  end;
end;

procedure TV_SeqDataExplorer.ActionFindSeqNameExecute(Sender: TObject);
begin
  ActionShowFindExecute(nil);
  ShowTaxaSearch;
end;

procedure TV_SeqDataExplorer.ActionHideFindExecute(Sender: TObject);
begin
  MainSearchPanel.Visible := False;
  ActionTranslate.ShortCut := 84;
  ActionHighlightConserved.ShortCut := 67;
  ActionHighlightVariableSites.ShortCut := 86;
  ActionHighlightParsimInfoSites.Shortcut := 80;
  ActionHighlightSingletonSites.ShortCut := 83;
  ActionHighlight0FoldDegenSites.ShortCut := 48;
  ActionHighlight2FoldDegenSites.ShortCut := 50;
  ActionHighlight4FoldDegenSites.ShortCut := 52;
end;

procedure TV_SeqDataExplorer.ActionHideMotifSearchResultsExecute(Sender: TObject);
begin
  //VS_SeqDataExplorer.HideSeqItem := HideSeqBtn.Down;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionHighlight0FoldDegenSitesExecute(Sender: TObject);
begin
  HighlightSitesClick(Sender);
  ActionHighlight0FoldDegenSites.Checked := VS_SeqDataExplorer.ZeroFoldItem;
end;

procedure TV_SeqDataExplorer.ActionHighlight2FoldDegenSitesExecute(Sender: TObject);
begin
  HighlightSitesClick(Sender);
  ActionHighlight2FoldDegenSites.Checked := VS_SeqDataExplorer.TwoFoldItem;
end;

procedure TV_SeqDataExplorer.ActionHighlight4FoldDegenSitesExecute(Sender: TObject);
begin
  HighlightSitesClick(Sender);
  ActionHighlight4FoldDegenSites.Checked := VS_SeqDataExplorer.TwoFoldItem;
end;

procedure TV_SeqDataExplorer.ActionHighlightConservedExecute(Sender: TObject);
begin
  HighlightSitesClick(Sender);
  ActionHighlightConserved.Checked := VS_SeqDataExplorer.ConstantItem;
end;

procedure TV_SeqDataExplorer.ActionHighlightedSitesExecute(Sender: TObject);
begin
  ActionAllSelectedSites.Checked := False;
  ActionHighlightedSites.Checked := True;
  VS_SeqDataExplorer.StatAllSelSitesItem := False;
  VS_SeqDataExplorer.StatUseOnlyMarkedSitesItem := True;
end;

procedure TV_SeqDataExplorer.ActionHighlightLabelledSitesExecute(Sender: TObject);
begin
  if VS_SeqDataExplorer.IsTranslated then
    Exit;
  HighlightSitesClick(Sender);
  ActionHighlightLabelledSites.Checked := VS_SeqDataExplorer.LabelledItem;
  try
    FUpdatingHighlightState := True;
    FEditLabelsFrame.HighlighLabelledSitesCBox.Checked := ActionHighlightLabelledSites.Checked;
  finally
    FUpdatingHighlightState := False;
  end;
end;

procedure TV_SeqDataExplorer.ActionHighlightParsimInfoSitesExecute(Sender: TObject);
begin
  HighlightSitesClick(Sender);
  ActionHighlightParsimInfoSites.Checked := VS_SeqDataExplorer.ParsimInfoItem;
end;

procedure TV_SeqDataExplorer.ActionHighlightSingletonSitesExecute(Sender: TObject);
begin
  HighlightSitesClick(Sender);
  ActionHighlightSingletonSites.Checked := VS_SeqDataExplorer.SingletonItem;
end;

procedure TV_SeqDataExplorer.ActionHighlightVariableSitesExecute(Sender: TObject);
begin
  HighlightSitesClick(Sender);
  ActionHighlightVariableSites.Checked := VS_SeqDataExplorer.VariableItem;
end;

procedure TV_SeqDataExplorer.ActionHomologyExecute(Sender: TObject);
begin
  ShowMessage('not yet implemented');
end;

procedure TV_SeqDataExplorer.ActionLabelSitesUsedForGroupIdentityComparisonExecute(Sender: TObject);
var
  siteLabel: String = '';
  response: Boolean;
begin
  Repeat
    response := InputQuery('Please provide a character to use for labelling sites', 'Valid characters are: A-Z a-z 0-9 * + - _', siteLabel);
    if response = False then
     Exit;
  until (not response) or ((Length(siteLabel) = 1) and IsValidSiteLabel(AnsiChar(siteLabel[1])));
  D_InputSeqData.LabelSitesUsedForMinorAlleleIdentityComparisons(siteLabel[1]);

  if VS_SeqDataExplorer.CurAttrDisp = megLabelled then
  begin
    D_InputSeqData.UpdateSiteAttrArray;
    ApplyUpdate;
  end;
  AdjustEnabledStatus;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.ActionMismatchDistributionExecute(Sender: TObject);
begin
  ShowMessage('not yet implemented');
end;

procedure TV_SeqDataExplorer.ActionNextHighlightExecute(Sender: TObject);
var
   i: Integer;
   fCols: Integer;
   response: Integer;
begin
  if VS_SeqDataExplorer.CurAttrDisp = megNone then
    Exit;
  fCols := NumberOfFixedColsVisible;
  if (DataGrid.Col < D_InputSeqData.FNoOfSites + fCols) and (FCurrentHighlightedSite >= 0) then
    FCurrentHighlightedSite := DataGrid.Col - fCols;
  for i := FCurrentHighlightedSite + 1 to D_InputSeqData.FNoOfSites - 1 do
  begin
    if VS_SeqDataExplorer.IsHighlighted(i) then
    begin
      FCurrentHighlightedSite := i;
      DataGrid.Col := FCurrentHighlightedSite + fCols;
      DataGrid.LeftCol := Max(fCols, DataGrid.Col - (DataGrid.VisibleColCount div 2) + fCols);
      DataGrid.Invalidate;
      try
        FUpdatingSiteIndex := True;
        SiteNumSpinEdit.Value := FCurrentHighlightedSite + 1;
      finally
        FUpdatingSiteIndex := False;
      end;
      Exit;
    end;
  end;
  response := MessageDlg('Continue from beginning?', 'No more highlighted sites found in this direction. Resume search from the beginning?', mtConfirmation, mbYesNo, 0);
  if response = mrYes then
  begin
    DataGrid.Col := fCols;
    FCurrentHighlightedSite := -1;
    ActionNextHighlightExecute(Sender);
  end;
end;

procedure TV_SeqDataExplorer.ActionNucCompositionExecute(Sender: TObject);
begin
  MegaForm.NucleotideCompActionExecute(Self);
end;

procedure TV_SeqDataExplorer.ActionPreviousHighlightExecute(Sender: TObject);
var
  i: Integer;
  fCols: Integer;
  response: Integer;
begin
  if VS_SeqDataExplorer.CurAttrDisp = megNone then
    Exit;
  fCols := NumberOfFixedColsVisible;
  if (DataGrid.Col > fCols) and (FCurrentHighlightedSite <> D_InputSeqData.FNoOfSites) then
    FCurrentHighlightedSite := DataGrid.Col - fCols
  else if FCurrentHighlightedSite < 0 then
    FCurrentHighlightedSite := D_InputSeqData.FNoOfSites;
  for i := FCurrentHighlightedSite - 1 downto 0 do
    if VS_SeqDataExplorer.IsHighlighted(i) then
    begin
      FCurrentHighlightedSite := i;
      DataGrid.Col := FCurrentHighlightedSite + fCols;
      DataGrid.LeftCol := Max(fCols, DataGrid.Col - (DataGrid.VisibleColCount div 2) + fCols);
      DataGrid.Invalidate;
      try
        FUpdatingSiteIndex := True;
        SiteNumSpinEdit.Value := FCurrentHighlightedSite + 1;
      finally
        FUpdatingSiteIndex := False;
      end;
      Exit
    end;
  response := MessageDlg('Continue search?', 'No more highlighted sites found in this direction. Resume search from the end?', mtConfirmation, mbYesNo, 0);
  if response = mrYes then
  begin
    DataGrid.Col := D_InputSeqData.FNoOfSites + NumberOfFixedColsVisible - 1;
    FCurrentHighlightedSite := D_InputSeqData.FNoOfSites;
    ActionPreviousHighlightExecute(Sender);
  end;
end;

procedure TV_SeqDataExplorer.ActionQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TV_SeqDataExplorer.ActionRestoreInputOrderExecute(Sender: TObject);
begin
  D_InputSeqData.FOtuInfos.SortByInputOrder;
  D_InputSeqData.UpdateDispTaxaList;
  DataGrid.Invalidate;
end;

procedure TV_SeqDataExplorer.UpdateStatusBar(Sender: TObject);
begin
  if (not Assigned(VS_SeqDataExplorer)) or (stDomainName >= VS_SeqDataExplorer.CurStatusBarStatistics.Count) then
    Exit;
  StatusBar.Panels[stMarkedSites].Text := VS_SeqDataExplorer.CurStatusBarStatistics[stMarkedSites];
  StatusBar.Panels[stCurSite].Text := VS_SeqDataExplorer.CurStatusBarStatistics[stCurSite];
  StatusBar.Panels[stDomainName].Text := VS_SeqDataExplorer.CurStatusBarStatistics[stDomainName];
  StatusBar.Panels[stCounts].Text := Trim(NumSelectedTaxaString + NumSelectedGroupsString) + ' selected';
end;

procedure TV_SeqDataExplorer.SetDiagnoseSite(Site: Integer);
begin
  { TODO 1 -oglen -cmypeg : need to refactor for when more then one fixed column is visible, currently mypeg_only restricts sde to just the taxa names column }
  DataGrid.Col := Site + Cols.VisibleCount - 1;
  VS_SeqDataExplorer.CurColorSite := Site;
end;

procedure TV_SeqDataExplorer.UpdateDiagnoseBtnCaption(Site: Integer);
var
  Capt: String;
  BtnWidth: Integer;
begin
  Capt := ' Diagnose Variant (Site#' + IntToStr(Site) + ') ';
  BtnWidth := MainToolBar.Canvas.TextWidth(Capt); // can't get the canvas of the button but it uses parent font so we use the canvas of its parent
  DiagnoseButton.Width := BtnWidth + 45;
  DiagnoseButton.Caption := Capt;
end;

procedure TV_SeqDataExplorer.CustomColsChanged;
begin

end;

procedure TV_SeqDataExplorer.UpdateFixedColWidth;
const
  PADDING = 20;
  HEADER_PADDING = 7;
  MAX_FIXED_COL_WIDTH_PERC = 0.75;
var
  TempInt: Longint;
  i: Integer;
  VisibleIndex: Integer = -1;
begin
  if FUpdatingFixedColWidths then
    Exit; // Prevents calling this function again if it's already executing.
  FUpdatingFixedColWidths := True;
  try
    if DataGrid.ColCount >= Cols.Count then
      for i := 0 to Cols.Count - 1 do
        DataGrid.ColWidths[i] := DataGrid.DefaultColWidth;

    UpdateMaxTaxaNamePixels;

    TempInt := CheckMark.Width + 4 + DataGrid.Canvas.TextWidth(IntToStr(DataGrid.RowCount));
    if ActionDispTaxaNames.Checked then
    begin
      TempInt := TempInt + VS_SeqDataExplorer.MaxTaxaNamePixels + 15; // +15 added because this was often getting cut off
      if TempInt > Trunc(Width * (MAX_FIXED_COL_WIDTH_PERC)) then
        TempInt := Trunc(Width * (MAX_FIXED_COL_WIDTH_PERC));
      DataGrid.ColWidths[TAXA_COLUMN] := TempInt;
    end;

    if (Cols.Count < 2) or (DataGrid.ColCount < 2) then
      Exit; // This function can end up being called before all setup is done, we need to exit before we access unallocated vars.

    if (Cols.Col[GROUP_COLUMN] <> nil) then
    begin
      UpdateMaxGpNamePixels;
      if ActionDispGroupNames.Checked OR VS_SeqDataExplorer.DispGpNamesItem then
      begin
        Cols.Col[GROUP_COLUMN].Visible := True;
        DataGrid.ColWidths[GROUP_COLUMN] := PADDING + VS_SeqDataExplorer.MaxGpNamePixels;
        if (DataGrid.ColWidths[GROUP_COLUMN]  < (DataGrid.Canvas.TextWidth(Cols.Col[GROUP_COLUMN].ColName)+HEADER_PADDING)) then
          DataGrid.ColWidths[GROUP_COLUMN]  := DataGrid.Canvas.TextWidth(Cols.Col[GROUP_COLUMN].ColName)+HEADER_PADDING;
      end
      else
      begin
        Cols.Col[GROUP_COLUMN].Width := DataGrid.DefaultColWidth;
        if VisibleIndex >= 0 then
          DataGrid.ColWidths[VisibleIndex] := Cols.Col[GROUP_COLUMN].Width;
        Cols.Col[GROUP_COLUMN].Visible := False;
      end;
    end;
  finally
    FUpdatingFixedColWidths := False;
  end;
end;

procedure TV_SeqDataExplorer.UpdateRowColumn(Row, Col: Integer);
begin
  DataGrid.Row := Row;
  DataGrid.Col := Col;
end;

procedure TV_SeqDataExplorer.UpdateGridSizes;
begin
  if not Assigned(D_InputSeqData) then
    Exit;

  With DataGrid do
   begin
     DefaultColWidth :=  GetMaxCharLen+4 + 2 + Length(IntToStr(D_InputSeqData.FNoofTaxa));
     DefaultRowHeight := Canvas.TextHeight('W')+4;
   end;
  UpdateFixedColWidth;
end;

procedure TV_SeqDataExplorer.UpdateMaxGpNamePixels;
var
  i: LongInt;
  tempStr: String = '';

begin
  with DataGrid, Canvas do
  begin
    VS_SeqDataExplorer.MaxGpNamePixels := TextWidth('Group');
    D_InputSeqData.MaxGpNameLen := Length('Group');
    tempStr := 'Group';
    for i:=0 to D_InputSeqData.FNoOfTaxa -1 do
    begin
      if D_InputSeqData.FOtuInfos[i].IsUsed then
      begin
        if Length(D_InputSeqData.OtuInfos[i].GpName) > Length(tempStr) then
          tempStr := D_InputSeqData.OtuInfos[i].GpName;
      end;
    end;
    if ActionDisplayGroupCounts.Checked then
      tempStr := TempStr + '(' + IntToStr(D_InputSeqData.FNoOfTaxa div 4) + ' taxa)';
    VS_SeqDataExplorer.MaxGpNamePixels := TextWidth(tempStr);
    D_InputSeqData.MaxGpNameLen := Length(tempStr);
  end;
end;

procedure TV_SeqDataExplorer.UpdateMaxTaxaNamePixels;
var
  i: LongInt;
begin
  VS_SeqDataExplorer.MaxTaxaNamePixels := 0;
  D_InputSeqData.MaxTaxaNameLen:=0;
  with DataGrid, Canvas do
    for i:=0 to D_InputSeqData.FNoOfTaxa -1 do
    begin
      if not D_InputSeqData.FOtuInfos[i].IsUsed then
        Continue;
      if TextWidth(D_InputSeqData.FOtuInfos[i].Name) > VS_SeqDataExplorer.MaxTaxaNamePixels then
        VS_SeqDataExplorer.MaxTaxaNamePixels := TextWidth(D_InputSeqData.FOtuInfos[i].Name);
      if Length(D_InputSeqData.FOtuInfos[i].name) > D_InputSeqData.MaxTaxaNameLen then
        D_InputSeqData.MaxTaxaNameLen := Length(D_InputSeqData.FOtuInfos[i].name);
    end;
end;

procedure TV_SeqDataExplorer.UpdateMaxSpNamePixels;
var
  i: LongInt;
begin
  with DataGrid, Canvas do
  begin
    VS_SeqDataExplorer.MaxSpNamePixels := TextWidth('Species');
    D_InputSeqData.MaxSpNameLen := Length('Species');
    for i:=0 to D_InputSeqData.FNoOfTaxa -1 do
    begin
      if not D_InputSeqData.FOtuInfos[i].IsUsed then
        Continue;
      if TextWidth(D_InputSeqData.FOtuInfos[i].SpName) > VS_SeqDataExplorer.MaxSpNamePixels then
        VS_SeqDataExplorer.MaxSpNamePixels := TextWidth(D_InputSeqData.FOtuInfos[i].SpName);
      if Length(D_InputSeqData.FOtuInfos[i].Spname) > D_InputSeqData.MaxSpNameLen then
        D_InputSeqData.MaxSpNameLen := Length(D_InputSeqData.FOtuInfos[i].SpName);
    end;
  end;
end;

procedure TV_SeqDataExplorer.UpdateMaxPopNamePixels;
var
  i: LongInt;
begin
  with DataGrid, Canvas do
  begin
    VS_SeqDataExplorer.MaxPopNamePixels := TextWidth('Population');
    D_InputSeqData.MaxPopNameLen := Length('Population');
    for i:=0 to D_InputSeqData.FNoOfTaxa -1 do
    begin
      if not D_InputSeqData.FOtuInfos[i].IsUsed then
        Continue;
      if TextWidth(D_InputSeqData.FOtuInfos[i].PopName) > VS_SeqDataExplorer.MaxPopNamePixels then
        VS_SeqDataExplorer.MaxPopNamePixels := TextWidth(D_InputSeqData.FOtuInfos[i].PopName);
      if Length(D_InputSeqData.FOtuInfos[i].Popname) > D_InputSeqData.MaxPopNameLen then
        D_InputSeqData.MaxPopNameLen := Length(D_InputSeqData.FOtuInfos[i].PopName);
    end;
  end;
end;

procedure TV_SeqDataExplorer.NotifySearchCompletion(NumOfRes: Integer);
begin
  ShowMessage('There were ' + IntTOStr(NumOfRes) + ' search results');
end;

procedure TV_SeqDataExplorer.JumpTo(aTop, aLeft: Integer);
begin
  DataGrid.Row := aTop + 1;
  DataGrid.Col := aLeft + 1;
end;

procedure TV_SeqDataExplorer.AdjustEnabledStatus;
var
  aNumSelectedGroups: Integer = 0;
  aNumAlleleComparisonSites: Integer = 0;
begin
  if FIsClosing or (not Assigned(D_InputSeqData)) then
    Exit;
  aNumSelectedGroups := D_InputSeqData.OtuInfos.NoOfSelGroups;
  aNumAlleleComparisonSites := D_InputSeqData.NumAlleleComparisonSitesUsed;
  ActionDisplayGroupCounts.Enabled := ActionDispGroupNames.Checked;
  ActionSelectEarliestDatedSeqInEachGroup.Enabled := aNumSelectedGroups > 0;
  ActionSelectLargestGroups.Enabled := aNumSelectedGroups > 0;
  ActionUnselectSmallGroups.Enabled := aNumSelectedGroups > 0;
  ActionSelectOneFromEachGroup.Enabled := aNumSelectedGroups > 0;
  ActionDisableIndependentSites.Enabled := D_InputSeqData.DomainMarks.IndependentCount > 0;
  ActionAddDomainsForGroupIdentityComparisonSites.Enabled := aNumAlleleComparisonSites > 0;
  ActionToggleUnlabeledSites.Enabled := D_InputSeqData.DomainMarks.HasLabelledSites;

  ActionTranslate.Enabled := D_InputSeqData.FIsNuc and D_InputSeqData.FIsCoding;
  ActionSelectCodeTable.Enabled := D_InputSeqData.FIsNuc and D_InputSeqData.FIsCoding;
  ActionHighlight0FoldDegenSites.Enabled := D_InputSeqData.FIsNuc and D_InputSeqData.FIsCoding and (not VS_SeqDataExplorer.IsTranslated);
  ActionHighlight2FoldDegenSites.Enabled := D_InputSeqData.FIsNuc and D_InputSeqData.FIsCoding and (not VS_SeqDataExplorer.IsTranslated);
  ActionHighlight4FoldDegenSites.Enabled := D_InputSeqData.FIsNuc and D_InputSeqData.FIsCoding and (not VS_SeqDataExplorer.IsTranslated);
  ActionHighlightLabelledSites.Enabled := D_InputSeqData.HasSiteLabels and (not VS_SeqDataExplorer.IsTranslated);
  ActionCodonUsage.Enabled := D_InputSeqData.FIsNuc and D_InputSeqData.FIsCoding and (not VS_SeqDataExplorer.IsTranslated);
  ActionNucComposition.Enabled    := not (VS_SeqDataExplorer.IsTranslated or (not D_InputSeqData.FIsNuc));
  StatNucPairFreqSubMenu.Enabled    := not (VS_SeqDataExplorer.IsTranslated or (not D_InputSeqData.FIsNuc));
  ActionHomology.Visible := False;
  ActionAminoAcidComposition.Enabled    := (VS_SeqDataExplorer.IsTranslated or (not D_InputSeqData.FIsNuc));
  HighlightSitesClick(nil); // this sets all sites type buttons up

  { Everything that starts with 'SW' is commented out because it is from the ommitted
    Sliding-Window menu from the Delphi code. That menu is hidden in all releases so
    I did not move it here due to time constraints. If we ever need it, it is in the Delphi code}
  //SWSynAndNonSynItem.Enabled := D_InputSeqData.FIsNuc and D_InputSeqData.FIsCoding and (not VS_SeqDataExplorer.IsTranslated);
  //SWNucCompositionItem.Enabled  := not (VS_SeqDataExplorer.IsTranslated or (not D_InputSeqData.FIsNuc));
  //SWNucSeqDiversityItem.Enabled := not (VS_SeqDataExplorer.IsTranslated or (not D_InputSeqData.FIsNuc));
  //SWAminoCompositionItem.Enabled      := (VS_SeqDataExplorer.IsTranslated or (not D_InputSeqData.FIsNuc));
  //SWAminoAcidSeqDiversityItem.Enabled := (VS_SeqDataExplorer.IsTranslated or (not D_InputSeqData.FIsNuc));
  //SWHydropathyProfileItem.Enabled     := (VS_SeqDataExplorer.IsTranslated or (not D_InputSeqData.FIsNuc));
  //SWCustomProfileItem.Enabled         := (VS_SeqDataExplorer.IsTranslated or (not D_InputSeqData.FIsNuc));
end;


procedure TV_SeqDataExplorer.WriteGeneDomainOnStatusBar(ADomainInfo: TDomainInfo);
begin
  if ADomainInfo = nil then
    Exit;
  D_InputSeqData.CurDomainInfo := ADomainInfo;
  with D_InputSeqData.CurDomainInfo do
    if Length(GeneName) > 0 then
      VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stDomainName] := Name + ' ('+GeneName+')'
    else
      VS_SeqDataExplorer.CurStatusBarStatistics.Strings[stDomainName] := Name;
end;

procedure TV_SeqDataExplorer.UpdateDomainInfos;
var
  i: Integer;
  aInfo: TDomainInfo;
  parent: TDomainInfo;
  children: TList;
begin
  if D_InputSeqData.AllDomainInfo.NoOfDomains = 0 then
    Exit;
  for i := 0 to D_InputSeqData.AllDomainInfo.NoOfDomains - 1 do
  begin
    aInfo := D_InputSeqData.AllDomainInfo[i];
    if aInfo.IsDomain and (aInfo.GeneName <> EmptyStr) then
    begin
      parent := D_InputSeqData.AllDomainInfo.GetGeneByGeneName(aInfo.GeneName);
      if Assigned(parent) then
      begin
        aInfo.ParentDomain := parent;
        parent.ChildDomains.Add(aInfo);
      end
      else
      begin
        parent := TDomainInfo.Create;
        parent.Assign(aInfo);
        parent.Name := aInfo.GeneName;
        parent.IsGene := True;
        parent.ChildDomains.Add(aInfo);
        parent.IsTemp := True;
        D_InputSeqData.AllDomainInfo.Add(parent);
        UpdateGeneCoordinates(parent, aInfo.FromSite, aInfo.ToSite);
      end;
    end
    else if aInfo.IsGene then
    begin
      children := D_InputSeqData.AllDomainInfo.GetGeneChildDomains(aInfo.GeneName);
      aInfo.ChildDomains.Assign(children);
      children.Free;
    end;
  end;
end;

procedure TV_SeqDataExplorer.InitGeneDomainsPropertyEditor;
begin
  if not Assigned(FGeneDomainsPropsEditor) then
    FGeneDomainsPropsEditor := TGenesDomainsPropertiesEditor.Create(Self);
  FGeneDomainsPropsEditor.MaxSiteIndex := D_InputSeqData.FNoOfSites;
  GeneDomainDlg.FSitePicker := FGeneDomainsPropsEditor.SitePicker;
  GeneDomainDlg.GenesDomainsPropsEditor := FGeneDomainsPropsEditor;
end;

procedure TV_SeqDataExplorer.Initialize;
var
  gridOptions: TGridOptions;
  pw: TPleaseWait = nil;
  showPleaseWait: Boolean = False;
begin
  try
    try
      BeginFormUpdate;
      MegaForm.ExplorersDisabledForLargeData := False;
      ResetGroupCounts;
      FCurrentHighlightedSite := -1;
      ActionShowOnlySelected.Checked := False;
      ActionDispTaxaNames.Checked := True;
      ActionDispGroupNames.Checked := False;
      ActionUseIdenticalSymbol.Checked := True;
      ActionToggleUnlabeledSites.Checked := True;
      ActionLabelSitesUsedForGroupIdentityComparison.Enabled := False;
      ActionAddDomainsForGroupIdentityComparisonSites.Enabled := False;
      with DataGrid do
      begin
        gridOptions := Options;
        showPleaseWait := (D_InputSeqData.NoOfSites*D_InputSeqData.NoOfTaxa > 1000000);
        if showPleaseWait then
        begin
          pw := TPleaseWait.Create(self);
          pw.SetPosition(poMainFormCenter);
          pw.SetShowCancel(False);
          pw.Action := 'Initializing Data Grid (This may take a long time)';
          pw.Caption := 'Initializing...';
          pw.Show;
          Application.ProcessMessages;
        end;
        ColCount := D_InputSeqData.FNoOfSites + NumberOfFixedColsVisible;
        DataGrid.FixedCols := NumberOfFixedColsVisible;
        if showPleaseWait then
          Application.ProcessMessages;
        RowCount := D_InputSeqData.FNoOfTaxa + 1;
        if showPleaseWait then
        begin
          Application.ProcessMessages;
          pw.Hide;
        end;
        if D_InputSeqData.IsCoding then
          Include(gridOptions, goCellHints)
        else
          Exclude(gridOptions, goCellHints);
      end;
      AssignHelpContext;
      DataGrid.Col := NumberOfFixedColsVisible;
      SiteNumSpinEdit.Value := NumberOfFixedColsVisible;
      SiteNumSpinEdit.MaxValue := D_InputSeqData.FNoOfSites;
      SiteNumSpinEdit.Width := Canvas.TextWidth(Format('---%s---', [IntToStr(D_InputSeqData.FNoOfSites)]));
      InitScrollBars(True);
      Application.ProcessMessages;
    except
      on E:Exception do
      begin
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
        Close;
      end;
    end;
  finally
    EndFormUpdate;
    if Assigned(pw) then
      pw.Free;
  end;
end;

procedure TV_SeqDataExplorer.ApplyUpdate;
var
  i: Integer;
  CodeTableDlg1: TSelectGeneticCodeDlg = nil;
begin
  if FIsClosing or (not Assigned(D_InputSeqData)) then
    Exit;
  updateBirdsGeneDomain;
  try
    if D_InputSeqData.FOtuInfos.IsDirty or D_InputSeqData.DomainMarks.IsDirty then
      D_InputSeqData.ClearComputedStatsCache;
    if D_InputSeqData.FOtuInfos.IsDirty or FForcingHighlightUpdate then
    begin
      if not FForcingHighlightUpdate then
      begin
        FHasDataSubsetChanged := True;
        D_InputSeqData.UpdateDispTaxaList;
        DataGrid.RowCount := D_InputSeqData.DispTaxaList.Count+1;
        VS_SeqDataExplorer.NoOfSelTaxa := D_InputSeqData.FOtuInfos.NoOfSelOtus;
        UpdateFixedColWidth;
      end;
      D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateBasicCounts];
      if D_InputSeqData.FIsNuc then
      begin
        D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateNucVar];
        if D_InputSeqData.FIsCoding then
          D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateDegeneracy, svUpdateAminoVar];
      end
      else
        D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateAminoVar];
    end;

    if D_InputSeqData.FDomainMarks.IsDirty then
    begin
      FHasDataSubsetChanged := True;
      for i:=0 to D_InputSeqData.FNoOfTaxa-1 do // just make the markings correctly
      begin
         if D_InputSeqData.FOtuInfos[i].RsvData <> nil then
           FreeMem(D_InputSeqData.FOtuInfos[i].RsvData);
         D_InputSeqData.FOtuInfos[i].RsvData := nil;
      end;
      D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateBasicCounts];

      if D_InputSeqData.FIsNuc then
      begin
        if (D_InputSeqData.FIsCoding = False) and D_InputSeqData.FDomainMarks.IsCoding then
        begin
          // Joel: Should update selection test menus here since we changed to coding
          D_InputSeqData.IsCoding  := True;
          CodeTableDlg1 := TSelectGeneticCodeDlg.Create(Self);
          D_InputSeqData.FCodeName := CodeTableDlg1.CodeTableName;
          D_InputSeqData.SetCodeTable(CodeTableDlg1.CodeTable);
          MegaForm.CodeTableName := D_InputSeqData.FCodeName;
          if D_InputSeqData.AACodonStarts = nil then
          begin
            D_InputSeqData.AACodonStarts := TLongintList.Create;
            D_InputSeqData.AACodonPos2   := TLongintList.Create;
            D_InputSeqData.AACodonPos3   := TLongintList.Create;
          end;
          VS_SeqDataExplorer.IsTranslated := False;
          D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateCodonStarts,
                                        svUpdateAASeq, svUpdateNucVar,svUpdateAminoVar,
                                        svUpdateDegeneracy];
          D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateCodonSiteType]; // always needed
          D_InputSeqData.UpdateCodonSitesType;
          AdjustEnabledStatus;
        end
        else if D_InputSeqData.FIsCoding and (not D_InputSeqData.FDomainMarks.IsCoding) then
        begin
          if VS_SeqDataExplorer.IsTranslated then
          begin
            ActionTranslate.Checked := False;
            ActionTranslateExecute(nil);
          end;
          D_InputSeqData.IsCoding  := False;
          VS_SeqDataExplorer.IsTranslated := False;   // essentially untranslated
          D_InputSeqData.FNoOfSites := D_InputSeqData.NoOfNucSites;
          D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateNucVar];
          D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateCodonSiteType]; // always needed
          D_InputSeqData.UpdateCodonSitesType;
          AdjustEnabledStatus;
        end
        else if D_InputSeqData.FIsCoding then
        begin
          D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateCodonStarts,
                                        svUpdateAASeq, svUpdateNucVar,svUpdateAminoVar,
                                        svUpdateDegeneracy];
          D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateCodonSiteType]; // always needed
          D_InputSeqData.UpdateCodonSitesType;
        end
        else
        begin
          D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateNucVar];
          D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateCodonSiteType]; // always needed
          D_InputSeqData.UpdateCodonSitesType;
        end
      end
      else
      begin
        D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateAminoVar];
        D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateCodonSiteType]; // always needed
        D_InputSeqData.UpdateCodonSitesType;
      end;

      if D_InputSeqData.FIsNuc and VS_SeqDataExplorer.IsTranslated then // that means we have coding
      begin
        D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateCodonStarts, svUpdateAASeq];
        if not FForcingHighlightUpdate then
          ActionTranslateExecute(nil)
        else if svUpdateCodonStarts in D_InputSeqData.JobsPending then
          D_InputSeqData.DoTranslation;
      end;
    end;

    if D_InputSeqData.FOtuInfos.IsDirty or D_InputSeqData.FDomainMarks.IsDirty or FForcingHighlightUpdate then
    begin
      if not FForcingHighlightUpdate then
        HighlightSitesClick(VS_SeqDataExplorer.HighlightItem);
      DataGrid.Invalidate;
    end;
    EnableDisableGroupTagItems;
    ResetSearchBtns;
  finally
    if CodeTableDlg1 <> nil then
      CodeTableDlg1.Free;
    D_InputSeqData.HideProgressDlg;
  end;
end;

end.

