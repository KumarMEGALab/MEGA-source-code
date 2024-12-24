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

unit MAlignEditMainForm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, ActnList, ExtCtrls, StdCtrls, Spin, Buttons, IniPropStorage,
  MAlignGrid, MPleaseWait, MegaConsts, AppLinker, applinkeroptions,
  mruntimeprogressdlg, MClustalW, Types, KeywordConsts,
  mmegaalignmentfileloader, recentlyusedfiles, MD_Sequences, ExcelWrite,
  MEditorForm, MCodons, mimageform, LResources,
  miniformstream, mraw_fasta_loader, uCEFInterfaces,uCEFTypes, mcodon_translation_selection_map;

const
  SOURCE_DATA_PAGE = 0;
  PROTEIN_DATA_PAGE = 1;
  SPECIES_COLUMN = 0;
  GROUP_COLUMN = 1;
  ACTION_LIST_UPDATE_FREQUENCY = 300; { the minimum number of milliseconds between action list updates}

type

  { TAlignEditMainForm }

  TAlignEditMainForm = class(TForm)
    DeveloperAction: TAction;
    ActionReferToGenbank: TAction;
    ActionShowHelp: TAction;
    ActionClustalAlignCodons: TAction;
    ActionMuscleAlignCodons: TAction;
    ActionExportPaup: TAction;
    ActionToUpper: TAction;
    ActionComp: TAction;
    ActionRev: TAction;
    ActionGroupNameVisible: TAction;
    ActionSpeciesVisible: TAction;
    ActionSetupMuscle: TAction;
    ActionAnalyze: TAction;
    ActionCaptionExpert: TAction;
    ActionQuery: TAction;
    ActionClose: TAction;
    ActionSaveSession: TAction;
    ActionShowInfo: TAction;
    ActionTraceEdit: TAction;
    ActionDataType: TAction;
    ActionSelectSeqOnClick: TAction;
    ActionSelectSiteOnClick: TAction;
    ActionEditSeqName: TAction;
    ActionEditEnabled: TAction;
    ActionFontSetup: TAction;
    ActionDelSeq: TAction;
    ActionBrowse: TAction;
    ActionBlast: TAction;
    ActionAddFromFile: TAction;
    ActionExportMEGA: TAction;
    ActionShowBgColor: TAction;
    ActionUseColor: TAction;
    ActionSelectSite: TAction;
    ActionSelectSeq: TAction;
    ActionMoveRight: TAction;
    ActionMoveLeft: TAction;
    ActionFillEnd: TAction;
    ActionDelGapOnlySites: TAction;
    ActionDelGap: TAction;
    ActionRevComp: TAction;
    ActionGeneticCode: TAction;
    ActionTranslate: TAction;
    ActionSetupClustal: TAction;
    ActionClustalAlign: TAction;
    ActionUnmarkSite: TAction;
    ActionAlignMarkedSites: TAction;
    ActionFindMark: TAction;
    ActionMarkSite: TAction;
    ActionHighlightMotif: TAction;
    ActionFindPrevious: TAction;
    ActionFindNext: TAction;
    ActionFind: TAction;
    ActionSelectAll: TAction;
    ActionAddClipbrd: TAction;
    ActionAddSeq: TAction;
    ActionDelete: TAction;
    ActionEditGrpName: TAction;
    ActionPaste: TAction;
    ActionEditSequenceName: TAction;
    ActionCut: TAction;
    ActionCopy: TAction;
    ActionMuscleAlign: TAction;
    ActionUndo: TAction;
    ActionExit: TAction;
    ActionExportFASTA: TAction;
    ActionOpenFile: TAction;
    ActionNewFile: TAction;
    ActionList1: TActionList;
    FontDlg: TFontDialog;
    EditToolbarItem: TMenuItem;
    ClustalDNABtn: TMenuItem;
    ClustalcDNABtn: TMenuItem;
    ClustalProteinBtn: TMenuItem;
    IniPropStorage1: TIniPropStorage;
    GeneticCodeLabel: TLabel;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem81: TMenuItem;
    WindowsMenuItem: TMenuItem;
    MuscleProteinBtn: TMenuItem;
    MusclecDNABtn: TMenuItem;
    MuscleDNABtn: TMenuItem;
    MenuItem13: TMenuItem;
    Panel1: TPanel;
    ClustalOptionsPopup: TPopupMenu;
    MuscleOptionsPopup: TPopupMenu;
    HorizScrollbar: TScrollBar;
    HorizScrollbar2: TScrollBar;
    VertScrollbar2: TScrollBar;
    VertScrollbar: TScrollBar;
    StatusBarLabel: TLabel;
    MenuItem1: TMenuItem;
    OpenFileItem: TMenuItem;
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
    DataRecentlyUsedFilesSessionsItem: TMenuItem;
    MenuItem110: TMenuItem;
    MenuItem111: TMenuItem;
    MenuItem112: TMenuItem;
    MenuItem113: TMenuItem;
    MenuItem114: TMenuItem;
    MenuItem115: TMenuItem;
    MenuItem116: TMenuItem;
    MenuItem117: TMenuItem;
    MenuItem118: TMenuItem;
    MenuItem119: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem120: TMenuItem;
    MenuItem121: TMenuItem;
    MenuItem122: TMenuItem;
    MenuItem123: TMenuItem;
    MenuItem124: TMenuItem;
    MenuItem125: TMenuItem;
    MenuItem126: TMenuItem;
    MenuItem127: TMenuItem;
    MenuItem128: TMenuItem;
    MenuItem129: TMenuItem;
    OpenAlignmentSessionItem: TMenuItem;
    MenuItem130: TMenuItem;
    MenuItem131: TMenuItem;
    MenuItem132: TMenuItem;
    ImportFromFileItem: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    SaveFileItem: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    DNAMenuItem: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
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
    MenuItem39: TMenuItem;
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
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    MarkConserved1: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem80: TMenuItem;
    FileToolbarItem: TMenuItem;
    ShowSpeciesNamesItem: TMenuItem;
    ShowGroupNamesItem: TMenuItem;
    Mark50Level1: TMenuItem;
    Mark60Level1: TMenuItem;
    Mark70Level1: TMenuItem;
    Mark80Level1: TMenuItem;
    Mark90Level1: TMenuItem;
    Mark100Level1: TMenuItem;
    MenuItem83: TMenuItem;
    MenuItem84: TMenuItem;
    MenuItem85: TMenuItem;
    MenuItem86: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    MenuItem89: TMenuItem;
    Mark_none1: TMenuItem;
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
    FileOpenDlg: TOpenDialog;
    InsertFileDlg: TOpenDialog;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    PopupMenu3: TPopupMenu;
    PopupMenu4: TPopupMenu;
    PopupMenu5: TPopupMenu;
    PopupMenu6: TPopupMenu;
    FileSaveDlg: TSaveDialog;
    ReopenMenuMRU: TPopupMenu;
    SearchToolbarItem: TMenuItem;
    ProteinMenuItem: TMenuItem;
    MenuItem9: TMenuItem;
    FileToolBar: TToolBar;
    EditToolbar: TToolBar;
    SearchToolbar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    WGapRBtn: TRadioButton;
    WOGapRBtn: TRadioButton;
    SiteIndexLabel: TLabel;
    SiteIndexNSEdit: TSpinEdit;
    MainMenu1: TMainMenu;
    DataMenu: TMenuItem;
    EditMenu: TMenuItem;
    SearchMenu: TMenuItem;
    AlignmentMenu: TMenuItem;
    WebMenu: TMenuItem;
    SequencerMenu: TMenuItem;
    DisplayMenu: TMenuItem;
    HelpMenu: TMenuItem;
    PageControl1: TPageControl;
    StatusBar: TPanel;
    DataTabSheet: TTabSheet;
    ProteinTabSheet: TTabSheet;
    rgAlignEditInitOptions: TRadioGroup;
    procedure ActionAddClipbrdExecute(Sender: TObject);
    procedure ActionAddFromFileExecute(Sender: TObject);
    procedure ActionAddSeqExecute(Sender: TObject);
    procedure ActionAlignMarkedSitesExecute(Sender: TObject);
    procedure ActionAnalyzeExecute(Sender: TObject);
    procedure ActionBlastExecute(Sender: TObject);
    procedure ActionBrowseExecute(Sender: TObject);
    procedure ActionCaptionExpertExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionClustalAlignCodonsExecute(Sender: TObject);
    procedure ActionClustalAlignExecute(Sender: TObject);
    procedure ActionReferToGenbankExecute(Sender: TObject);
    procedure DeveloperActionExecute(Sender: TObject);
    //procedure DoClustalExecute;
    procedure ActionCompExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionCutExecute(Sender: TObject);
    procedure ActionDataTypeExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionDelGapExecute(Sender: TObject);
    procedure ActionDelGapOnlySitesExecute(Sender: TObject);
    procedure ActionDelSeqExecute(Sender: TObject);
    procedure ActionEditEnabledExecute(Sender: TObject);
    procedure ActionEditGrpNameExecute(Sender: TObject);
    procedure ActionEditSeqNameExecute(Sender: TObject);
    procedure ActionEditSequenceNameExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionExportFASTAExecute(Sender: TObject);
    procedure ActionExportMEGAExecute(Sender: TObject);
    procedure ActionExportPaupExecute(Sender: TObject);
    procedure ActionFillEndExecute(Sender: TObject);
    procedure ActionFindExecute(Sender: TObject);
    procedure ActionFindMarkExecute(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure ActionFindPreviousExecute(Sender: TObject);
    procedure ActionFontSetupExecute(Sender: TObject);
    procedure ActionGeneticCodeExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ActionGroupNameVisibleExecute(Sender: TObject);
    procedure ActionHighlightMotifExecute(Sender: TObject);
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionMarkSiteExecute(Sender: TObject);
    procedure ActionMoveLeftExecute(Sender: TObject);
    procedure ActionMoveRightExecute(Sender: TObject);
    procedure ActionMuscleAlignCodonsExecute(Sender: TObject);
    procedure ActionMuscleAlignExecute(Sender: TObject);
    procedure ActionNewFileExecute(Sender: TObject);
    procedure ActionOpenFileExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionQueryExecute(Sender: TObject);
    procedure ActionRevCompExecute(Sender: TObject);
    procedure ActionRevExecute(Sender: TObject);
    procedure ActionSaveSessionExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure ActionSelectSeqExecute(Sender: TObject);
    procedure ActionSelectSeqOnClickExecute(Sender: TObject);
    procedure ActionSelectSiteExecute(Sender: TObject);
    procedure ActionSelectSiteOnClickExecute(Sender: TObject);
    procedure ActionSetupClustalExecute(Sender: TObject);
    procedure HorizScrollbar2Change(Sender: TObject);
    procedure HorizScrollbar2Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure HorizScrollbarChange(Sender: TObject);
    procedure HorizScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure IniPropStorageRestoreProperties(Sender:TObject);
    procedure IniPropStorageSavingProperties(Sender:TObject);
    procedure ActionShowBgColorExecute(Sender: TObject);
    procedure ActionShowHelpExecute(Sender: TObject);
    procedure ActionShowInfoExecute(Sender: TObject);
    procedure ActionSpeciesVisibleExecute(Sender: TObject);
    procedure ActionToUpperExecute(Sender: TObject);
    procedure ActionTraceEditExecute(Sender: TObject);
    procedure ActionTranslateExecute(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ActionUnmarkSiteExecute(Sender: TObject);
    procedure ActionUseColorExecute(Sender: TObject);
    procedure ClustalcDNABtnClick(Sender: TObject);
    procedure ClustalDNABtnClick(Sender: TObject);
    procedure ClustalOptionsPopupPopup(Sender: TObject);
    procedure ClustalProteinBtnClick(Sender: TObject);
    procedure DataTabSheetShow(Sender: TObject);
    procedure DNAMenuItemClick(Sender: TObject);
    procedure EditToolbarItemClick(Sender: TObject);
    procedure FileToolbarItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure ImportFromFileItemClick(Sender: TObject);
    procedure Mark100Level1Click(Sender: TObject);
    procedure Mark50Level1Click(Sender: TObject);
    procedure Mark60Level1Click(Sender: TObject);
    procedure Mark70Level1Click(Sender: TObject);
    procedure Mark80Level1Click(Sender: TObject);
    procedure Mark90Level1Click(Sender: TObject);
    procedure Mark_none1Click(Sender: TObject);
    procedure MenuItem121Click(Sender: TObject);
    procedure MusclecDNABtnClick(Sender: TObject);
    procedure MuscleDNABtnClick(Sender: TObject);
    procedure MuscleOptionsPopupPopup(Sender: TObject);
    procedure MuscleProteinBtnClick(Sender: TObject);
    procedure OpenAlignmentSessionItemClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure EditAlignWelcomeMessageCancelled(Sender :TObject);
    procedure ProcessEditAlignWelcomeMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure ProteinMenuItemClick(Sender: TObject);
    procedure RecentlyUsedFilesItemClick(Sender: TObject);
    procedure SearchToolbarItemClick(Sender: TObject);
    procedure SiteIndexNSEditChange(Sender: TObject);
    procedure SiteIndexNSEditExit(Sender: TObject);
    procedure SiteIndexNSEditKeyPress(Sender: TObject; var Key: char);
    procedure ToolButton13Click(Sender: TObject);
    procedure VertScrollbar2Change(Sender: TObject);
    procedure VertScrollbar2Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure VertScrollbarChange(Sender: TObject);
    procedure VertScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure WGapRBtnClick(Sender: TObject);
    procedure WOGapRBtnClick(Sender: TObject);
  private
    FLastRow: Integer;
    FCodonTranslationSelectionMap: TCodonTranslationSelectionMap;
    FTranslateSelectionOnly: Boolean;
    FHScrollPos: Integer;
    FVScrollPos: Integer;
    FScrollStarted: Boolean;
    FCurrentDirectory: String;
    FLastActionListUpdateTime: TDateTime;
    FHasData: Boolean;
    FIsSourceGrid: Boolean;
    FIsUpdatingSite: Boolean;
    ClustalW: TClustalW;
    FTransferringAlignmentForAnalysis: Boolean;
    FAlignmentIsInProgress: Boolean;
    FIsLoadingFiles: Boolean;
    FIsResizing: Boolean;
    FResizingCol: Integer;
    MousePos: TPoint;
    TempPos, CurPos: TPoint;
    TempGap: integer;
    ClustalSetupResult: boolean;
    CreatedFromFileClickEvent : boolean;
    FAppLinkOptionsManager: TApplinkOptionsManager;
    FMuscleThread: TMuscleLinkThread;
    FLoader: TMegaAlignmentFileLoader;
    FOverwrite: Boolean;
    FRecentlyUsedFiles: TRecentlyUsedFilesList;
    procedure SetSiteSpinEditValue(aValue: Integer);
    function ColumnVisible(aCol: Integer): Boolean;
    function RowVisible(aRow: Integer): Boolean;
    function ValidSelectableCol(aCol: Integer): Integer;
    function ValidSelectableRow(aRow: Integer): Integer;
    procedure DoOnSelection(Sender: TObject; aCol: Integer; aRow: Integer);
    procedure DoAfterSelection(Sender: TObject; aCol: Integer; aRow: Integer);
    procedure UpdateAllShortcutsForMacOs;
    procedure ClustalSetupCancelled(Sender: TObject);
    procedure InitMainMenu;
    procedure InitPopupMenus;
    procedure InitHelpContexts;
    procedure InitAlignGrid1;
    procedure InitScrollBars1(resetPosition: Boolean);
    procedure InitAlignGrid2;
    procedure InitScrollbars2(resetPosition: Boolean);
    procedure InitClustalW;
    procedure PositionToolbars;
    function GetCurGeneticCode: String;
    function GetCurGeneticCodeName: String;
    function GetMuscleOptions(var Options: TStringList; const DataType: TSnTokenCode): Boolean;
    function FinalizeMuscleOptions(Options: TStringList; const DataType: TSnTokenCode): Boolean;
    procedure BLASTSearch(query: String; isDNA: boolean);
    function Translate(OnlySelected: boolean; IsAlignmentPrep: Boolean = False): Boolean;

    procedure AlignGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure AlignGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AlignGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AlignGrid2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AlignGrid2RowMoved(Sender: TObject; IsColumn: Boolean; FromIndex, ToIndex: Integer);
    procedure ShrinkNameColToFit(percent: Integer = 65); // Encapsulation of current code which forces the Taxa name column to be <= percent of the dlg width.
    procedure InitRecentlyUsedFiles;
    procedure ClustalWTerminate(Sender: TObject);
    procedure MuscleThreadFinished(Thread: TObject);
    procedure LoaderDone(Sender: TObject);
    procedure SetAlignmentInProgress(aValue: Boolean);
    procedure LaunchFastaLoadingThread(filename: String; overwrite: Boolean);
    procedure FastaLoadingThreadDone(Thread: TObject);
    procedure UpdateUsageStatistics(usageData: String);
    function GetUsageDataAsJsonString(method: String; options: TStringList = nil; dataType: TSnTokenCode = snNoToken): String;
    function GetUsageDataAsJsonStringClustal: String;
    function GetUsageDataAsJsonStringMuscle(muscleOptions: TStringList; dataType: TSnTokenCode): String;
    { private declarations }
  public
    SourceAlignGrid: TAlignGrid;
    TranslatedAlignGrid: TAlignGrid;
    CurAlignGrid: TAlignGrid;

    SessionFileName: String;
    MEGAFileName: String;
    cmdargs, ForceArgs: String;
    procedure UpdateGeneticCodeLabel;
    procedure ShowWelcomeMessage;
    function SetGeneticCodeTableByName(aName: String): Boolean;
    procedure ExportGeneticCodeTable(ExportTypeStr: String; SaveLocation: String);
    procedure ExportGeneticCodeStatistics(ExportTypeStr: String; SaveLocation: String);

    function TryTranslate(OnlySelected: Boolean; IsAlignmentPrep: Boolean = False): Boolean;
    function CheckStopCodons: boolean;
    function TryRemoveGaps: Boolean;  // Added so we could remove gaps prior to codon translation. This is different from the regular Remove Gaps feature.
    procedure UnTranslate;
    procedure InitForm;
    procedure ReplaceSelectedFromFasta(FileName: String; StandardNames: Boolean; XSites: String; seq: TSequenceList; Progress: TRuntimeProgress = nil);

    function OpenAMEGA2File(AFileName: String; overwrite: boolean): Boolean;
    function OpenAMDSXFile(AFileName: String; overwrite: boolean): Boolean;
//    function ReadMEGA2SequenceFile(FileName: String; Data: TSequenceList; HasPara: Boolean; ADataType, ADataFormat: TSnTokenCode; AMissSym, AIdenSym, AGapSym: AnsiChar): Boolean;
    function LoadThirdPartyFileFormat(InFile : String; overwrite: boolean): Boolean;

    function SaveSession(filename : String):boolean;
    function GetSessionVersion(FileName: String): Integer;
    function RetrieveSession(filename : String; PleaseWait: TPleaseWait = nil):boolean;
    function RetrieveBrokenSession(var AFile: File; PleaseWait: TPleaseWait = nil):Boolean;
    function InsertSession(filename : String):boolean;
    function SelectedToFasta(const FileName: String; StandardizeNames: Boolean; ReplaceUnkChars: Boolean; var XSites: String; var seqList: TSequenceList): Boolean;
    property CurGeneticCodeName: String read GetCurGeneticCodeName;
    property CurGeneticCode: String read GetCurGeneticCode;
    property AlignmentIsInProgress: Boolean read FAlignmentIsInProgress write FAlignmentIsInProgress;
    procedure AppendToRecentlyUsedFiles(aFilename: String);
    procedure UpdateRecentFilesList(AFile: String);
    function NumSequences: Integer;
  end;

  function FindAlignmentEditorWindow(ShowNow: boolean) : TAlignEditMainForm;
  function ShowAlignmentFromFile(filename : String):TForm;
  procedure LoadAlignmentFileFinished(filename: String; progress: TRuntimeProgress);
  function ParseBlastHTMLFile(Filename: String): Boolean;
  function IsAlignmentSessionFileExt(aFile: String): Boolean;

var
  AlignEditMainForm: TAlignEditMainForm;

implementation

{$R *.lfm}

uses
  {$IFDEF DEBUG}
  mdeveloper_console,
  {$ENDIF}
  maligneditwelcomedlg, MegaVerConsts, AlnBuilder_HC, MSelectGeneticCodeDlg, LCLIntf,
  LCLType, Grids, MV_Columns, MegaUtils, uMegaBrowser, MegaMainPreferences, MD_Align,
  MFormatConvertToMega, mshortcutshelper, ClustalOptionForm, fpjson, MUsageStatistics,
  MAlnThread, typinfo, RegExpr, MAnalysisInfo, MProcessPack,
  dateutils, MegaAnalysisPrefStrings, mdatatypedlg, Mega_Main, MMegaWindowInfo,
  MTraceEditForm, mhelpfiles, mhelpkeywords, ProcessInputData, MLegendGenerator,
  htmloptionsdlg, mbrowserutils, LCLProc, MegaPrivateFiles, syncobjs, math;

function FindAlignmentEditorWindow(ShowNow: boolean): TAlignEditMainForm;
begin
 if (AlignEditMainForm = nil) then
   AlignEditMainForm := TAlignEditMainForm.Create(Application);

 if ShowNow then
   AlignEditMainForm.Show;

 Result := AlignEditMainForm;
 if Result.WindowState = wsMinimized then
   Result.WindowState := wsNormal;
end;

function ShowAlignmentFromFile(filename: String): TForm;
begin
  Result := nil;
  FindAlignmentEditorWindow(False);

  with AlignEditMainForm do
  begin
    if AlignEditMainForm.AlignmentIsInProgress then
    begin
      ShowMessage('A sequence alignment is currently in progress. You must cancel that alignment or wait for it to complete before opening another alignment file');
      Exit;
    end;
    CreatedFromFileClickEvent := True;
    ActionCloseExecute(nil);
    if not SourceAlignGrid.Empty then
    begin
      Result := AlignEditMainForm;
      exit;
    end;

    if IsAlignmentSessionFileExt(filename) then
    begin
      if (not RetrieveSession(filename, nil)) and IsSessionTest then
        raise Exception.Create('session test failed for alignment session file: ' + filename);
      SessionFileName := filename;
      if IsSessionTest then
        MegaForm.SessionTestFinished(Result);
    end
    else if CompareText(ExtractFileExt(filename), '.meg') = 0 then
    begin
      if OpenAMEGA2File(filename, true) then
        SessionFileName := ChangeFileExt(filename, '.mas')
      else
        exit;
    end
    else if CompareText(ExtractFileExt(filename), '.mdsx') = 0 then
    begin
      if OpenAMDSXFile(filename, true) then
        SessionFileName := ChangeFileExt(filename, '.mas')
      else
        exit;
    end  // We are creating a toolbar for popular web browsers which let you import seqs to MEGA directly from your native browser
    else if CompareText(ExtractFileExt(filename), '.blast') = 0 then
    begin
      if not ParseBlastHTMLFile(filename) then
      exit;
    end
    else
    begin
      if LoadThirdPartyFileFormat(filename, true) then
        SessionFileName := ChangeFileExt(filename, '.mas')
      else if not HasFastaFileExtension(filename) then
        ShowMessage('Failed to load sequence data file. Please check that the file format is correct');
    end;
    if not HasFastaFileExtension(filename) then
      LoadAlignmentFileFinished(filename, nil);
    InitScrollBars1(True);
    InitScrollBars2(True);
  end;

  Result := AlignEditMainForm;
end;

procedure LoadAlignmentFileFinished(filename: String; progress: TRuntimeProgress);
begin

  with AlignEditMainForm do
  begin
    SessionFileName := ChangeFileExt(filename, '.mas');
    FileSaveDlg.FileName :=  SessionFileName;
    FileOpenDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(ExtractFileDir(filename));
    UpdateRecentFilesList(filename);
    if Assigned(progress) then
    begin
      progress.UpdateRunStatusInfo('Status', 'initializing data grid...This may take some time...');
      //Application.ProcessMessages;
    end;
    InitForm;
    PageControl1.ActivePageIndex := SOURCE_DATA_PAGE;
    PageControl1.Show;
    ShrinkNameColToFit;
    SourceAlignGrid.Invalidate;
    SourceAlignGrid.SetFocus;
    if SourceAlignGrid.MaxNoOfSites > 0 then
      Caption := ConvertToMegaWinCaption('Alignment Explorer ('+ExtractFileName(filename)+')');
    UpdateGeneticCodeLabel;
  end;
end;

function ParseBlastHTMLFile(Filename: String): Boolean;
var
  Document: TStringList = nil;
  aChBrowser: TMegaBrowserFrm = nil;
begin
  Result := False;
  Document := TStringList.Create;
  Document.LoadFromFile(filename);
  Result := Document.Count > 0;
  aChBrowser := CreateNewChromiumBrowserWindow(bmBrowser);
  { TODO -oHasnat -ccef4delphi : uncomment below line }
  //aChBrowser.LoadSequencesFromHTML(Document);
  if Assigned(aChBrowser) then
    FreeAndNil(aChBrowser);
  if Assigned(Document) then
    FreeAndNil(Document);
end;

function IsAlignmentSessionFileExt(aFile: String): Boolean;
begin
  Result := ((CompareText(ExtractFileExt(aFile), '.mas') = 0) or (CompareText(ExtractFileExt(aFile), '.masx') = 0))
end;

{ TAlignEditMainForm }

procedure TAlignEditMainForm.FormCreate(Sender: TObject);
begin
  FLastRow := 0;
  FCodonTranslationSelectionMap := TCodonTranslationSelectionMap.Create(1, 1);
  FTranslateSelectionOnly := False;
  FScrollStarted := False;
  ActionList1.State := asSuspended;
  HeaderCS := TCriticalSection.Create;
  FIsUpdatingSite := False;
  StatusBarLabel.Caption := EmptyStr;
  {$IFDEF DARWIN}
  ActionFindNext.ShortCut := TextToShortCut('CTRL+G');
  ActionFindMark.ShortCut := TextToShortCut('CTRL+M');
  {$ENDIF}
  UpdateAllShortcutsForMacOs;
  FOverwrite := False;
  FLoader := nil;
  FTransferringAlignmentForAnalysis := False;
  FAlignmentIsInProgress := False;
  FIsResizing := False;
  FIsLoadingFiles := False;
  InitAlignGrid1;
  InitAlignGrid2;
  InitClustalW;
  CurAlignGrid := SourceAlignGrid;
  InitHelpContexts;
  InitRecentlyUsedFiles;
  IniPropStorage1.IniFileName := GetPrivateFile(MEGASessionFile);
  IniPropStorageRestoreProperties(Sender);
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Alignment Explorer';
  FAppLinkOptionsManager := nil;
  StatusBar.Height := SiteIndexNSEdit.Height + 10;
  FLastActionListUpdateTime := Now;
  {$IFNDEF DARWIN}
  InitMainMenu;
  {$ENDIF}
  InitPopupMenus;
  ImageForm.UpdateImgList(Self);
  {$IFNDEF DEBUG}
  DeveloperAction.Visible := False;
  {$ENDIF}
end;

procedure TAlignEditMainForm.FormDestroy(Sender: TObject);
begin
 RemoveWindowFromTray(Self);
 if Assigned(HeaderCS) then
 begin
   HeaderCS.Free;
   HeaderCS := nil;
 end;
 if ClustalW <> nil then
   ClustalW.Free;
 if Assigned(FAppLinkOptionsManager) then
   FAppLinkOptionsManager.Free;
 if Assigned(FRecentlyUsedFiles) then
    FRecentlyUsedFiles.Free;
 if Assigned(FCodonTranslationSelectionMap) then
   FCodonTranslationSelectionMap.Free;
 AlignEditMainForm := nil;
end;

procedure TAlignEditMainForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i: Integer;
  CurrentFile: String;
begin
  try
    if Length(Filenames) > 0 then
      for i := 0 to Length(Filenames) - 1 do
      begin
        CurrentFile := Filenames[i];
        if not FileExists(CurrentFile) then
          raise Exception.Create('file not found');
        if ShowAlignmentFromFile(CurrentFile) = nil then
          raise Exception.Create('The file could not be retrieved in Alignment Explorer.');
      end;
  except
    on E:Exception do
      ShowMessage('An error occurred when loading the alignment file -' + ExtractFileName(CurrentFile) + '- : ' + E.Message );
  end;
end;

function TAlignEditMainForm.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
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
      ShowMessage('Oh no! An error occurred when initializing the help browser: ' + E.Message);
  end;
end;

procedure TAlignEditMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
 if SourceAlignGrid.Empty then
 begin
   ActionAddSeqExecute(nil);
   SourceAlignGrid.Sequence[0].SeqData := AnsiString(Key);
 end;
end;

procedure TAlignEditMainForm.EditAlignWelcomeMessageCancelled(Sender :TObject);
begin
  RemoveWindowFromTray(Self);
end;

procedure TAlignEditMainForm.FormShow(Sender: TObject);
begin
  if (AlignEditMainForm.CreatedFromFileClickEvent = False) and (AlignEditMainForm.SourceAlignGrid.NoOfSeqs < 1) then
    PageControl1.Hide;
  AddWindowToTray(Self);
  SourceAlignGrid.Invalidate;
  TranslatedAlignGrid.Invalidate;
  UpdateGeneticCodeLabel;
end;
procedure TAlignEditMainForm.ProcessEditAlignWelcomeMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
var
  temp: ustring;
begin
  try
    HtmlOptionsDialog.Hide;
    HtmlOptionsDialog.CancelCallback := nil;
    temp := message.ArgumentList.GetString(0);
    if Pos(RENDER_PROCESS_ERROR, temp) > 0 then
      raise Exception.Create(temp);
    temp := message.ArgumentList.GetString(CREATE_NEW_ALIGNMENT_INDEX);
    Self.Show;
    if SameText(temp, 'true') then
      ActionNewFileExecute(nil)
    else
    begin
      temp := message.ArgumentList.GetString(OPEN_SAVED_ALIGNMENT_INDEX);
      if SameText(temp, 'true') then
        ActionOpenFileExecute(OpenAlignmentSessionItem)
      else
      begin
        temp := message.ArgumentList.GetString(RETRIEVE_SEQUENCE_FROM_FILE_INDEX);
        if SameText(temp, 'true') then
          ActionOpenFileExecute(ImportFromFileItem)
        else
          raise Exception.Create('BUG! invalid alignment command');
      end;
    end;
  except
  on E:Exception do
    ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TAlignEditMainForm.ImportFromFileItemClick(Sender: TObject);
begin
  ActionOpenFileExecute(ImportFromFileItem);
  UpdateGeneticCodeLabel;
end;

procedure TAlignEditMainForm.UpdateRecentFilesList(AFile: String);
begin
    AppendToRecentlyUsedFiles(AFile);
end;

function TAlignEditMainForm.NumSequences: Integer;
begin
  if Assigned(SourceAlignGrid) then
    Result := SourceAlignGrid.NoOfSeqs
  else if Assigned(TranslatedAlignGrid) then
    Result := TranslatedAlignGrid.NoOfSeqs
  else
    Result := 0;
end;

procedure TAlignEditMainForm.InitRecentlyUsedFiles;
var
  storage: String;
  i: Integer;
  aMenuItem: TMenuItem;
begin
  storage := GetPrivateFile(mfAlignmentExplorerFiles, False);
  FRecentlyUsedFiles := TRecentlyUsedFilesList.Create(Self, RecentlyUsedFilesItemClick, storage);
  if FRecentlyUsedFiles.Count > 0 then
    for i := 0 to FRecentlyUsedFiles.Count - 1 do
    begin
      aMenuItem := TRecentlyUsedItem(FRecentlyUsedFiles[i]).MenuItem;
      DataRecentlyUsedFilesSessionsItem.Add(aMenuItem);
    end;
end;

procedure TAlignEditMainForm.AppendToRecentlyUsedFiles(aFilename: String);
var
  aMenu: TMenuItem;
begin
  if not FRecentlyUsedFiles.HasFile(aFileName) then
  begin
    aMenu := FRecentlyUsedFiles.Add(aFilename).MenuItem;
    DataRecentlyUsedFilesSessionsItem.Add(aMenu);
  end;
end;

procedure TAlignEditMainForm.RecentlyUsedFilesItemClick(Sender: TObject);
var
  filename: String;
  aMenuItem: TMenuItem;
  aAction: TAction;
  i: Integer;
begin
  if Sender is TAction then
  begin
    aAction := TAction(Sender);
    filename := aAction.Hint;
  end
  else if Sender is TMenuItem then
  begin
    aMenuItem := TMenuItem(Sender);
    filename := aMenuItem.Hint;
  end
  else
  begin
    ShowMessage('Application Error: Invalid event sender for recently used file list');
    Exit;
  end;
  if FileExists(filename) then
  begin
    ShowAlignmentFromFile(filename);
  end
  else
  begin
    ShowMessage('Uh oh! Cannot find the file specified: ' + filename);
    if DataRecentlyUsedFilesSessionsItem.Count > 0 then
      for i := 0 to DataRecentlyUsedFilesSessionsItem.Count - 1 do
      begin
        aMenuItem := DataRecentlyUsedFilesSessionsItem.Items[i];
        if aMenuItem.Hint = aAction.Hint then
        begin
          DataRecentlyUsedFilesSessionsItem.Remove(aMenuItem);
          FRecentlyUsedFiles.Remove(aMenuItem);
          break;
        end;
      end;
  end;
end;

procedure TAlignEditMainForm.Mark100Level1Click(Sender: TObject);
begin
 SourceAlignGrid.ConsensusValue := 100;
 TranslatedAlignGrid.ConsensusValue := 100;
 SourceAlignGrid.MarkConsensus := true;
 TranslatedAlignGrid.MarkConsensus := true;
 CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.Mark50Level1Click(Sender: TObject);
begin
  SourceAlignGrid.ConsensusValue := 50;
  TranslatedAlignGrid.ConsensusValue := 50;
  SourceAlignGrid.MarkConsensus := true;
  TranslatedAlignGrid.MarkConsensus := true;
  CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.Mark60Level1Click(Sender: TObject);
begin
  SourceAlignGrid.ConsensusValue := 60;
  TranslatedAlignGrid.ConsensusValue := 60;
  SourceAlignGrid.MarkConsensus := true;
  TranslatedAlignGrid.MarkConsensus := true;
  CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.Mark70Level1Click(Sender: TObject);
begin
  SourceAlignGrid.ConsensusValue := 70;
  TranslatedAlignGrid.ConsensusValue := 70;
  SourceAlignGrid.MarkConsensus := true;
  TranslatedAlignGrid.MarkConsensus := true;
  CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.Mark80Level1Click(Sender: TObject);
begin
  SourceAlignGrid.ConsensusValue := 80;
  TranslatedAlignGrid.ConsensusValue := 80;
  SourceAlignGrid.MarkConsensus := true;
  TranslatedAlignGrid.MarkConsensus := true;
  CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.Mark90Level1Click(Sender: TObject);
begin
  SourceAlignGrid.ConsensusValue := 90;
  TranslatedAlignGrid.ConsensusValue := 90;
  SourceAlignGrid.MarkConsensus := true;
  TranslatedAlignGrid.MarkConsensus := true;
  CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.Mark_none1Click(Sender: TObject);
begin
  SourceAlignGrid.MarkConsensus := false;
  TranslatedAlignGrid.MarkConsensus := False;
  CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.MenuItem121Click(Sender: TObject);
begin
  BLASTSearch(CurAlignGrid.Sequence[MousePos.Y-1].SeqData, CurAlignGrid.IsDNA)
end;

procedure TAlignEditMainForm.MusclecDNABtnClick(Sender: TObject);
begin
  ActionMuscleAlignExecute(Sender);
end;

procedure TAlignEditMainForm.MuscleDNABtnClick(Sender: TObject);
begin
  ActionMuscleAlignExecute(Sender);
end;

procedure TAlignEditMainForm.MuscleOptionsPopupPopup(Sender: TObject);
begin
  MuscleDNABtn.Visible     := CurAlignGrid.IsDNA;
  MusclecDNABtn.Visible    := CurAlignGrid.IsDNA and (CurAlignGrid.IsProteinCoding);
  MuscleProteinBtn.Visible := not CurAlignGrid.IsDNA;
end;

procedure TAlignEditMainForm.MuscleProteinBtnClick(Sender: TObject);
begin
  ActionMuscleAlignExecute(Sender);
end;

procedure TAlignEditMainForm.OpenAlignmentSessionItemClick(Sender: TObject);
begin
  ActionOpenFileExecute(OpenAlignmentSessionItem);
  UpdateGeneticCodeLabel;
end;

procedure TAlignEditMainForm.PageControl1Change(Sender: TObject);
var
  i: Integer;
begin
  if PageControl1.ActivePageIndex = SOURCE_DATA_PAGE then  // moving to DNATab
  begin
    SourceAlignGrid.MarkConsensus  := TranslatedAlignGrid.MarkConsensus;
    SourceAlignGrid.ConsensusValue := TranslatedAlignGrid.ConsensusValue;
    CurAlignGrid := SourceAlignGrid;
    SourceAlignGrid.SetSelectionFromCachedData(FCodonTranslationSelectionMap.CurrentDnaSelection);
    //{$IFDEF DEBUG}WriteToDevConsole(Format('after change CAG.LeftCol=%d CAG.Col=%d CAG.Selection.Left=%d CAG.Selection.Right=%d', [CurAlignGrid.LeftCol, CurAlignGrid.Col, CurAlignGrid.Selection.Left, CurAlignGrid.Selection.Right]));{$ENDIF}
    InitScrollBars1(False);
    //{$IFDEF DEBUG}WriteToDevConsole(Format('after init   CAG.LeftCol=%d CAG.Col=%d CAG.Selection.Left=%d CAG.Selection.Right=%d', [CurAlignGrid.LeftCol, CurAlignGrid.Col, CurAlignGrid.Selection.Left, CurAlignGrid.Selection.Right]));{$ENDIF}
  end
  else if PageControl1.ActivePageIndex = PROTEIN_DATA_PAGE then // moving to ProteinTab
  begin
    TranslatedAlignGrid.Cols.Assign(SourceAlignGrid.Cols);
    for i:=0 to TranslatedAlignGrid.Cols.Count-1 do
     TranslatedAlignGrid.ColWidths[i] := SourceAlignGrid.ColWidths[i];
    TranslatedAlignGrid.MarkConsensus  := SourceAlignGrid.MarkConsensus;
    TranslatedAlignGrid.ConsensusValue := SourceAlignGrid.ConsensusValue;
    CurAlignGrid := TranslatedAlignGrid;
    InitScrollbars2(False);
  end;
  CurAlignGrid.Enabled := True;
  CurAlignGrid.SetFocus;
end;

procedure TAlignEditMainForm.PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
var
  i: Integer;
  checked: Boolean = False;
  questionRes: String = '';
  aRect: TRect;
begin
  if not Visible then
    Exit;
  //{$IFDEF DEBUG}WriteToDevConsole(Format('before changing CAG.LeftCol=%d CAG.Col=%d CAG.Selection.Left=%d CAG.Selection.Right=%d', [CurAlignGrid.LeftCol, CurAlignGrid.Col, CurAlignGrid.Selection.Left, CurAlignGrid.Selection.Right]));{$ENDIF}
  if PageControl1.ActivePageIndex = SOURCE_DATA_PAGE then  { leaving the Nucleotide page for the translated data page}
  begin
    SourceAlignGrid.CacheSelectionData(aRect);
    FCodonTranslationSelectionMap.SetCodingDnaSelection(aRect);
    if (not SourceAlignGrid.AllSelected) and (SourceAlignGrid.SequenceSelected or SourceAlignGrid.BlockSelected) then
      FTranslateSelectionOnly := MessageDlg('Translate only the selected segment?', mtConfirmation, [mbYes, mbNo], 0) = mrYes
    else
      FTranslateSelectionOnly := False;

    if FTranslateSelectionOnly and (SourceAlignGrid.BlockSelected) and (SourceAlignGrid.GetNoOfSelectedSites < 3) then
    begin
      ShowMessage('At least 3 sites must be selected in order to translate DNA to protein');
      AllowChange := False;
      Exit;
    end;

    if not TryToGetPreferencesMainMenu(UserPref_AlignmentEditorUseSelectedGeneticCodeForSessionStr, checked, QuestionRes) then
      checked := False;
    if checked then // they want to use the same genetic code
    begin
      UpdatePreferencesMainMenu(UserPref_AlignmentEditorUseSelectedGeneticCodeForSessionStr, checked, BoolToStr(True));
    end
    else
    begin
      if MessageDlgCheck(Self, 'The current Genetic Code is: ' + CurGeneticCodeName + '. Is this correct?',
        UserPref_UseSelectedGeneticCodeForSessionStr, UserPref_UseSelectedGeneticCodeForSessionStr, checked, mtConfirmation, [mbYES, mbNO], 0) = mrYes then
      begin
        UpdatePreferencesMainMenu(UserPref_AlignmentEditorUseSelectedGeneticCodeForSessionStr, checked, BoolToStr(True));
      end
      else
      begin
        UpdatePreferencesMainMenu(UserPref_AlignmentEditorUseSelectedGeneticCodeForSessionStr, checked, BoolToStr(False));
        ActionGeneticCodeExecute(nil);
      end;
    end;

    Translate(FTranslateSelectionOnly);
  end;

 if PageControl1.ActivePageIndex = PROTEIN_DATA_PAGE then
 begin
   //{$IFDEF DEBUG}WriteToDevConsole(Format('after changing CAG.LeftCol=%d CAG.Col=%d CAG.Selection.Left=%d CAG.Selection.Right=%d', [CurAlignGrid.LeftCol, CurAlignGrid.Col, CurAlignGrid.Selection.Left, CurAlignGrid.Selection.Right]));{$ENDIF}
   TranslatedAlignGrid.CacheSelectionData(aRect);
   FCodonTranslationSelectionMap.SetAminoAcidSelection(aRect);
   SourceAlignGrid.Cols.Assign(TranslatedAlignGrid.Cols);
   for i:=0 to SourceAlignGrid.Cols.Count-1 do
     SourceAlignGrid.ColWidths[i] := TranslatedAlignGrid.ColWidths[i];
 end
end;

procedure TAlignEditMainForm.ProteinMenuItemClick(Sender: TObject);
begin
  SourceAlignGrid.IsDNA := false;
  DataTabSheet.Caption := PROTEIN_SEQUENCES;
  ProteinTabSheet.TabVisible := false;
  SourceAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.SearchToolbarItemClick(Sender: TObject);
begin
  SearchToolbar.Visible := (not SearchToolbar.Visible);
  SearchToolbarItem.Checked := SearchToolbar.Visible;
  PositionToolbars;
end;

procedure TAlignEditMainForm.SiteIndexNSEditChange(Sender: TObject);
begin
 if CurAlignGrid.empty then
   Exit;
 if SiteIndexNSEdit.Focused then
 begin
   try
     FIsUpdatingSite := True;
     CurAlignGrid.JumpToSite(SiteIndexNSEdit.Value, WGapRBtn.Checked);
   finally
     FIsUpdatingSite := False;
   end;
 end;
end;

procedure TAlignEditMainForm.SiteIndexNSEditExit(Sender: TObject);
begin
  SetSiteSpinEditValue(CurAlignGrid.SiteIndex(WGapRBtn.Checked));
end;

procedure TAlignEditMainForm.SiteIndexNSEditKeyPress(Sender: TObject;
  var Key: char);
begin
 if Key = Char(VK_RETURN) then
   CurAlignGrid.SetFocus;
end;

procedure TAlignEditMainForm.ToolButton13Click(Sender: TObject);
var
  target: TPoint;
begin
  target.x := ToolButton13.Left;
  target.y := ToolButton13.Top;
  target := ScreenToClient(target);
  MuscleOptionsPopup.Popup(target.X, target.Y);
end;

procedure TAlignEditMainForm.VertScrollbar2Change(Sender: TObject);
var
  mouseLeftButtonDown: Boolean = False;
begin
  try
    mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
    if (not mouseLeftButtonDown) and FScrollStarted then
    begin
      SendMessage(VertScrollbar2.Handle, WM_CANCELMODE, WM_CANCELMODE, 0);
      FScrollStarted := False;
      Exit;
    end;

    FVScrollPos := VertScrollbar2.Position;
    if (FVScrollPos >= TranslatedAlignGrid.FixedRows) and (FVScrollPos <= TranslatedAlignGrid.RowCount) then
    begin
      TranslatedAlignGrid.TopRow := min(FVScrollPos, TranslatedAlignGrid.RowCount - TranslatedAlignGrid.VisibleRowCount);
      FScrollStarted := True;
    end;
  except
    FScrollStarted := False;
  end;
end;

procedure TAlignEditMainForm.VertScrollbar2Scroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  mouseLeftButtonDown: Boolean = False;
begin
  mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
  if (not mouseLeftButtonDown) and FScrollStarted then
    ScrollPos := FVScrollPos;
  if ScrollCode = scEndScroll then
    FScrollStarted := False;
  inherited;
end;

procedure TAlignEditMainForm.VertScrollbarChange(Sender: TObject);
var
  mouseLeftButtonDown: Boolean = False;
begin
  try
    mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
    if (not mouseLeftButtonDown) and FScrollStarted then
    begin
      SendMessage(VertScrollbar.Handle, WM_CANCELMODE, WM_CANCELMODE, 0);
      Exit;
    end;

    FVScrollPos := VertScrollbar.Position;
    if (FVScrollPos >= SourceAlignGrid.FixedRows) and (FVScrollPos <= SourceAlignGrid.RowCount) then
    begin
      SourceAlignGrid.TopRow := min(FVScrollPos, SourceAlignGrid.RowCount - SourceAlignGrid.VisibleRowCount);
      FScrollStarted := True;
    end;
  except
    FScrollStarted := False;
  end;
end;

procedure TAlignEditMainForm.VertScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  mouseLeftButtonDown: Boolean = False;
begin
  mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
  if (not mouseLeftButtonDown) and FScrollStarted then
    ScrollPos := FVScrollPos;
  if ScrollCode = scEndScroll then
    FScrollStarted := False;
  inherited;
end;

procedure TAlignEditMainForm.WGapRBtnClick(Sender: TObject);
begin
  if (not Visible) or (not CurAlignGrid.Enabled) then
    Exit;
  CurAlignGrid.SetFocus;
end;

procedure TAlignEditMainForm.WOGapRBtnClick(Sender: TObject);
begin
  if (not Visible) or (not CurAlignGrid.Enabled) then
    Exit;
  CurAlignGrid.SetFocus;
end;

procedure TAlignEditMainForm.SetSiteSpinEditValue(aValue: Integer);
begin
  try
    FIsUpdatingSite := True;
    if SiteIndexNSEdit.Value <> aValue then
      SiteIndexNSEdit.Value := aValue;
  finally
    FIsUpdatingSite := False;
  end;
end;

function TAlignEditMainForm.ColumnVisible(aCol: Integer): Boolean;
begin
  Result := False;
  if aCol < CurAlignGrid.LeftCol then
    Exit;
  if aCol > (CurAlignGrid.LeftCol + CurAlignGrid.VisibleColCount) then
    Exit;
  Result := True;
end;

function TAlignEditMainForm.RowVisible(aRow: Integer): Boolean;
begin
  Result := False;
  if aRow < CurAlignGrid.TopRow then
    Exit;
  if aRow > (CurAlignGrid.TopRow + CurAlignGrid.VisibleRowCount) then
    Exit;
  Result := True;
end;

function TAlignEditMainForm.ValidSelectableCol(aCol: Integer): Integer;
begin
  Result := aCol;
  if aCol < CurAlignGrid.FixedCols then
    Result := CurAlignGrid.FixedCols
  else if aCol >= CurAlignGrid.ColCount then
    Result := CurAlignGrid.ColCount - 1;
end;

function TAlignEditMainForm.ValidSelectableRow(aRow: Integer): Integer;
begin
  Result := aRow;
  if aRow < CurAlignGrid.FixedRows then
    Result := CurAlignGrid.FixedRows
  else if aRow >= CurAlignGrid.RowCount then
    Result := CurAlignGrid.RowCount - 1;
end;

procedure TAlignEditMainForm.DoOnSelection(Sender: TObject; aCol: Integer; aRow: Integer);
begin
  if not FIsUpdatingSite then
    SetSiteSpinEditValue(CurAlignGrid.Col);
  if (not ColumnVisible(HorizScrollBar.Position)) or (not ColumnVisible(aCol)) then
  begin
    if aCol < HorizScrollbar.Position then
      HorizScrollBar.Position := ValidSelectableCol(aCol)
    else if aCol > HorizScrollbar.Position then
      HorizScrollBar.Position := ValidSelectableCol(max(1, aCol - CurAlignGrid.VisibleColCount));
  end;

  if (not RowVisible(VertScrollbar.Position)) or (not RowVisible(aRow)) then
  begin
    if (aRow < VertScrollbar.Position) then
      VertScrollbar.Position := ValidSelectableRow(aRow)
    else if (aRow > VertScrollbar.Position) then
      VertScrollbar.Position := ValidSelectableRow(max(1, aRow - CurAlignGrid.VisibleRowCount));
  end;
end;

procedure TAlignEditMainForm.DoAfterSelection(Sender: TObject; aCol: Integer; aRow: Integer);
begin
  with CurAlignGrid do
    ActionDelGap.Enabled := FHasData and DelGapEnabled;
end;

procedure TAlignEditMainForm.UpdateGeneticCodeLabel;
begin
  if (DataTabSheet.Caption = PROTEIN_SEQUENCES)then
  begin
    GeneticCodeLabel.Caption := EmptyStr;
    Exit;
  end;
  if CurAlignGrid.CodeTableName <> EmptyStr then
    GeneticCodeLabel.Caption := 'Selected genetic code: ' + CurAlignGrid.CodeTableName
  else if CurAlignGrid = TranslatedAlignGrid then
    GeneticCodeLabel.Caption := ' '
  else
    GeneticCodeLabel.Caption := 'No genetic code selected';
end;

procedure TAlignEditMainForm.UpdateAllShortcutsForMacOs;
begin
  UpdateShortcutsForMacOs(ActionList1);
end;

procedure TAlignEditMainForm.ShowWelcomeMessage;

  procedure DoWelcomeFormAction;
  begin
    if WelcomeForm.NewAlignmentBtn.Checked then
      ActionNewFileExecute(nil)
    else if WelcomeForm.OpenSessionBtn.Checked then
      ActionOpenFileExecute(OpenAlignmentSessionItem)
    else if WelcomeForm.OpenFileBtn.Checked then
      ActionOpenFileExecute(ImportFromFileItem)
    else
      ShowMessage('Application Error: missing welcome form handler');
  end;

begin
  case WelcomeForm.ShowModal of
    mrOK: DoWelcomeFormAction;
    mrCancel: ActionExitExecute(nil);
  end;
end;

procedure TAlignEditMainForm.ClustalSetupCancelled(Sender :TObject);
begin
  SetAlignmentInProgress(False);
end;

procedure TAlignEditMainForm.InitMainMenu;
begin
  {$IFNDEF DARWIN}
  MainMenu1.OwnerDraw := True;
  MainMenu1.OnMeasureItem := MegaForm.MeasureMenuItem;
  MainMenu1.OnDrawItem := MegaForm.DrawMenuItem
  {$ENDIF}
end;

procedure TAlignEditMainForm.InitPopupMenus;
begin
  MegaForm.AddSettingsToPopupMenu(PopupMenu1);
  MegaForm.AddSettingsToPopupMenu(PopupMenu2);
  MegaForm.AddSettingsToPopupMenu(PopupMenu3);
  MegaForm.AddSettingsToPopupMenu(PopupMenu4);
  MegaForm.AddSettingsToPopupMenu(PopupMenu5);
  MegaForm.AddSettingsToPopupMenu(PopupMenu6);
  MegaForm.AddSettingsToPopupMenu(ClustalOptionsPopup);
  MegaForm.AddSettingsToPopupMenu(MuscleOptionsPopup);
end;

procedure TAlignEditMainForm.FormActivate(Sender: TObject);
var
  CodeTableDlg1: TSelectGeneticCodeDlg = nil;
begin
  if length(SourceAlignGrid.CodeTableName) = 0 then
  begin
    CodeTableDlg1 := TSelectGeneticCodeDlg.Create(Application);
    SourceAlignGrid.CodeTableName := CodeTableDlg1.CodeTableName;
    SourceAlignGrid.CodeTable     := CodeTableDlg1.CodeTable;
    CodeTableDlg1.Free;
  end;
  TranslatedAlignGrid.CodeTableName := SourceAlignGrid.CodeTableName;
  TranslatedAlignGrid.CodeTable     := SourceAlignGrid.CodeTable;
  SourceAlignGrid.Invalidate;
  TranslatedAlignGrid.Invalidate;
  UpdateGeneticCodeLabel;
  MegaForm.UpdateMenuForPinupItems(WindowsMenuItem, Self);
  {$IFDEF DARWIN}
  Invalidate;
  Application.ProcessMessages;
  {$ENDIF}
end;

procedure TAlignEditMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Response: Integer;
begin
  FIsLoadingFiles := True;
  CloseAction := caFree;
  if SourceAlignGrid.Empty then
    Exit;

  if SourceAlignGrid.Modified then
  begin
    Response := QuestionDlg('Save Changes?', 'Closing Alignment Explorer. Would you like to save the current alignment session to file?', mtCustom, [mrYes, 'Save Session...', mrNo, 'Discard Changes', mrCancel, 'Cancel'], 0);

    Case Response of
      mrYes:
      begin
        ActionSaveSessionExecute(Sender);
        Hide;
      end;
      mrNo:
      begin
        Hide;
      end;
      mrCancel:
      begin
        FIsLoadingFiles := False;
        CloseAction := caNone;
      end;
    end;
  end;
  if CloseAction <> caNone then
  begin
    if Assigned(SourceAlignGrid) then
    begin
      SourceAlignGrid.ColCount := 0;
      SourceAlignGrid.RowCount := 0;
    end;

    if Assigned(TranslatedAlignGrid) then
    begin
      TranslatedAlignGrid.ColCount := 0;
      TranslatedAlignGrid.RowCount := 0;
    end;

    IniPropStorageSavingProperties(Sender);
    UpdatePreferencesMainMenu(UserPref_AlignmentEditorUseSelectedGeneticCodeForSessionStr, false, BoolToStr(False)); // Clear user choice of Genetic code asking.
    AlignEditMainForm := nil;
  end;
end;

procedure TAlignEditMainForm.ActionCloseExecute(Sender: TObject);
var
  Response: Integer;
begin
  if SourceAlignGrid.Modified then
  begin
    Response := MessageDlg('Save the current alignment session to file?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if Response = mrCancel then
      Exit
    else if Response = mrYes then
    begin
      ActionSaveSessionExecute(Sender);
      if SourceAlignGrid.Modified then
        Exit;
    end;
  end;

  TranslatedAlignGrid.Clear;
  SourceAlignGrid.Clear;
  PageControl1.ActivePageIndex := 0;
  PageControl1.Hide;
  Caption := ConvertToMegaWinCaption('Alignment Explorer');
  UpdatePreferencesMainMenu(UserPref_AlignmentEditorUseSelectedGeneticCodeForSessionStr, false, BoolToStr(False)); // turn off the check for using the selected genetic code if they close the session.
end;

procedure TAlignEditMainForm.ActionClustalAlignCodonsExecute(Sender: TObject);
begin
  ActionClustalAlignExecute(Sender);
end;

procedure TAlignEditMainForm.ActionClustalAlignExecute(Sender: TObject);
var
  seq: TSequenceList = nil;
  aList: TStringList = nil;
  name: TStringList  = nil;
  i: integer;
  usageData: String = '';
begin
  if (not CurAlignGrid.BlockSelected) and (not CurAlignGrid.SequenceSelected) then
    if MessageDlg('Nothing selected for alignment. Select all?', mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
      CurAlignGrid.SelectAll
    else
      exit;

  if CurAlignGrid.GetNoOfSelectedSeqs < 2 then
  begin
    ShowMessage('You need to have more than one sequence selected for alignment.');
    Exit;
  end;

  if not CurAlignGrid.CanAlignSelectedData then
  begin
    ShowMessage('Alignable data not found. Cannot align only indels');
    Exit;
  end;

  SetAlignmentInProgress(True);
  seq := TSequenceList.Create;
  aList := TStringList.Create;
  name := TStringList.Create;
  ClustalW.RequestUntranslate := (Sender=ClustalcDNABtn) or (Sender=ActionClustalAlignCodons);
  ClustalSetupResult := True;
  ActionSetupClustalExecute(self);

  if not ClustalSetupResult then
    begin
      SetAlignmentInProgress(False);
      Exit;
    end;

    try
      // If we are doing cDNA then
      if ClustalW.RequestUntranslate then
      begin
        if ClustalW.ResetGaps then
          if not TryRemoveGaps then
            Raise Exception.Create('Error, was not able to remove gaps prior to codon alignment.');

        if not TryTranslate(true) then
          Raise Exception.Create('Error, was not able to translate DNA to Protein.');
        if not CheckStopCodons then
        begin
          Untranslate;
          //Enabled := true;  // this change prevents a false error from showing to the user when they press abort.
          Exit;
        end;
      end;

      try
        if (not CurAlignGrid.BlockSelected) and (not CurAlignGrid.SequenceSelected) then // if we are selecting all for clustal
        begin
          TempPos.X := 0;
          TempGap := 0;
          for i := 1 to CurAlignGrid.Col do
          begin
            if i > CurAlignGrid.Sequence[CurAlignGrid.Row-1].NoOfSites then break;
            if CurAlignGrid.Sequence[CurAlignGrid.Row-1].Base[i] = '-' then
              inc(TempGap)
            else
              inc(TempPos.X);
          end;
          TempPos.Y := CurAlignGrid.Row;
          CurPos.X := CurAlignGrid.LeftCol;
          CurPos.Y := CurAlignGrid.TopRow;
          CurAlignGrid.SelectAll;
        end
        else
        begin
          TempPos.X := 0;
          TempPos.Y := 0;
        end;

        CurAlignGrid.GetSelectedData(seq);
        if seq.Count <= 1 then
        begin
          Enabled := true;
          exit;
        end;
        for i := 0 to seq.Count-1 do
        begin
          aList.Add(seq[i].SeqData);
          name.Add(seq[i].SeqName);
        end;

        ClustalW.Sequences := aList;
        ClustalW.SeqNames  := name;
        ClustalW.IsDNA := CurAlignGrid.IsDNA;
        usageData := GetUsageDataAsJsonString('clustal');
        UpdateUsageStatistics(usageData);
        SetAlignmentInProgress(True);
        ClustalW.Align;
      finally
        seq.Free;
        aList.Free;
        name.Free;
      end;
    except
      on E:Exception do
      begin
        SetAlignmentInProgress(False);
        ShowMessage('Application Error: ' + E.Message);
      end;
    end;
end;

//procedure TAlignEditMainForm.DoClustalExecute;
//var
//  i: Integer;
//  seq: TSequenceList;
//  sl,name: TStringList;
//  usageData: String = '';
//begin
//  if not ClustalSetupResult then
//  begin
//    ActionClustalAlign.Enabled := True;
//    SetAlignmentInProgress(False);
//    Exit;
//  end;
//
//  try
//    if ClustalW.RequestUntranslate then { doing alignment by codons}
//    begin
//      if ClustalW.ResetGaps then
//        if not TryRemoveGaps then
//          Raise Exception.Create('Error, was not able to remove gaps prior to Codon alignment.');
//
//      if not TryTranslate(True, True) then
//        Raise Exception.Create('Error, was not able to translate DNA to Protein.');
//      CurAlignGrid := TranslatedAlignGrid;
//      if not CheckStopCodons then
//      begin
//        Untranslate;
//        SetAlignmentInProgress(False);
//        Exit;
//      end;
//    end;
//    try
//      if (not CurAlignGrid.BlockSelected) and (not CurAlignGrid.SequenceSelected) then // if we are selecting all for clustal
//      begin
//        TempPos.X := 0;
//        TempGap := 0;
//        for i := 1 to CurAlignGrid.Col do
//        begin
//          if i > CurAlignGrid.Sequence[CurAlignGrid.Row-1].NoOfSites then break;
//          if CurAlignGrid.Sequence[CurAlignGrid.Row-1].Base[i] = '-' then
//            inc(TempGap)
//          else
//            inc(TempPos.X);
//        end;
//        TempPos.Y := CurAlignGrid.Row;
//        CurPos.X := CurAlignGrid.LeftCol;
//        CurPos.Y := CurAlignGrid.TopRow;
//        CurAlignGrid.SelectAll;
//      end
//      else
//      begin
//        TempPos.X := 0;
//        TempPos.Y := 0;
//      end;
//      seq := TSequenceList.Create;
//      sl := TStringList.Create;
//      name := TStringList.Create;
//      CurAlignGrid.GetSelectedData(seq);
//      if seq.Count <= 1 then
//      begin
//        SetAlignmentInProgress(False);
//        exit;
//      end;
//      for i := 0 to seq.Count-1 do
//      begin
//        sl.Add(seq[i].SeqData);
//        name.Add(seq[i].SeqName);
//      end;
//
//      ClustalW.Sequences := sl;
//      ClustalW.SeqNames  := name;
//      ClustalW.IsDNA := CurAlignGrid.IsDNA;
//
//      UpdateUsageStatistics;
//      SetAlignmentInProgress(True);
//      ClustalW.Align;
//    finally
//      seq.Free;
//      sl.Free;
//      name.Free;
//    end;
//  except
//    on E:Exception do
//    begin
//      SetAlignmentInProgress(False);
//      ShowMessage('Oh no! An error occurred: ' + E.Message);
//    end;
//  end;
//end;

procedure TAlignEditMainForm.ActionCompExecute(Sender: TObject);
begin
  SourceAlignGrid.ComplimentSelection;
end;

procedure TAlignEditMainForm.ActionAddSeqExecute(Sender: TObject);
begin
  SourceAlignGrid.InsertBlankSequence;
end;

procedure TAlignEditMainForm.ActionAlignMarkedSitesExecute(Sender: TObject);
begin
  CurAlignGrid.AlignMakedSites;
end;

procedure TAlignEditMainForm.ActionAnalyzeExecute(Sender: TObject);
var
  fa: Boolean = False;
  tempCurDir: AnsiString = '';
  response: Integer = -1;
begin
  if CurAlignGrid.Empty or (CurAlignGrid.MaxNoOfSites <= 0) then
  begin
    ShowMessage('Cannot perform phylogenetic analysis on an empty data set');
    Exit;
  end;

  try
    tempCurDir := GetCurrentDir;
    FTransferringAlignmentForAnalysis := True;
    ActionAnalyze.Enabled := False;
    fa := SourceAlignGrid.ForceAlignment;
    SourceAlignGrid.ForceAlignment := true;
    CurAlignGrid.Title := 'Phylogenetic Analysis';
    if CurAlignGrid.IsDNA then
    begin
      if (PageControl1.PageCount = 2) and (PageControl1.ActivePageIndex = 2) then // If they are in the protein view then we can assume that it is PROTEIN coding.
        CurAlignGrid.IsProteinCoding := True
      else
      begin
        response := MessageDlg('Protein-coding nucleotide sequence data?', mtConfirmation, [mbYes, mbNo], 0);
        CurAlignGrid.IsProteinCoding := (response = mrYes);
      end;
    end;
    MEGAFileName := NextAvailableFilename(GetTempDir + 'PhyloAnalysis.meg');
    CurAlignGrid.SaveToMEGAFile(MEGAFileName);
    SourceAlignGrid.ForceAlignment := fa;

    if MEGAFileName = '' then exit; // They canceled saving.
    MegaForm.PromptCloseCurrentlyActive;
    if MegaForm.HasActiveData then
    begin
      // They might have clicked NO, in which case we just don't want to procede.
      Exit;
    end;

    DoOpenDataFile(MEGAFileName);
    MegaForm.AddFilenameToStatusBar(MegaFileName);
    SetCurrentDir(tempCurDir);
    if FileExists(MEGAFileName) then
      try
        DeleteFile(MEGAFileName);
      except
        // hope for the best
      end;
    MegaForm.BringToFront;
  finally
    ActionAnalyze.Enabled := True;
    FTransferringAlignmentForAnalysis := False;
  end;
end;

procedure TAlignEditMainForm.ActionBlastExecute(Sender: TObject);
var
  Seqs: TSequenceList;
begin
  Seqs := TSequenceList.Create;

  if not CurAlignGrid.Empty then
    if CurAlignGrid.BlockSelected or CurAlignGrid.SequenceSelected then
      CurAlignGrid.GetSelectedData(Seqs);

  if Seqs.Count > 0 then
    BLASTSearch(Seqs[0].SeqData, CurAlignGrid.IsDNA)
  else
    BLASTSearch('', CurAlignGrid.IsDNA);

  Seqs.Free;
end;

procedure TAlignEditMainForm.ActionBrowseExecute(Sender: TObject);
var
  aBrowser: TMegaBrowserFrm = nil;
begin
  try
    aBrowser := CreateNewChromiumBrowserWindow(bmBrowser, Self, True);
    if Assigned(aBrowser) then
    begin
      aBrowser.Show;
      aBrowser.LoadPlaceholder(NCBI_URL);
    end;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when launching the internal web browser: ' + E.Message);
  end;
end;

procedure TAlignEditMainForm.ActionReferToGenbankExecute(Sender: TObject);
var
  aChBrowser : TMegaBrowserFrm = nil;
  Seqs: TSequenceList = nil;
begin
  try
    Seqs := TSequenceList.Create;

    if not CurAlignGrid.Empty then
      if CurAlignGrid.SequenceSelected then
        CurAlignGrid.GetSelectedData(Seqs);

    if Seqs.Count > 0 then
    begin
      aChBrowser := CreateNewChromiumBrowserWindow(bmBrowser, Application, False);
      if Seqs.IsDNA then
        aChBrowser.GoToUrl(Format('%s/nuccore/%s', [NCBI_URL, Seqs[0].AccessionNum]))
      else
        aChBrowser.GoToUrl(Format('%s/protein/%s', [NCBI_URL, Seqs[0].AccessionNum]));
    end;
  finally
    if Assigned(Seqs) then
      Seqs.Free;
  end;
end;

procedure TAlignEditMainForm.DeveloperActionExecute(Sender: TObject);
var
  aList: TStringList = nil;
begin
  {$IFDEF DEBUG}
   if FileOpenDlg.Execute then
   begin
     try
       aList := TStringList.Create;
       aList.LoadFromFile(FileOpenDlg.Filename);
       CurAlignGrid.ReplaceTaxaNames(aList);
     finally
       if Assigned(aList) then
         aList.Free;
     end;
   end;
  {$ENDIF}
end;

procedure TAlignEditMainForm.ActionCaptionExpertExecute(Sender: TObject);
var
  aChBrowser: TMegaBrowserFrm;
  CaptionExpert : TLegendGenerator;
begin
  CaptionExpert := TLegendGenerator.Create;
  CaptionExpert.LoadTemplateFromFile('default_alignment.template');
  CaptionExpert.BindData(ClustalW);
  CaptionExpert.AssignData('WindowCaption',ConvertToMegaWinCaption('Alignment Caption'));
  aChBrowser := CreateNewChromiumBrowserWindow(bmCaption);
  aChBrowser.LoadHtmlFromString(CaptionExpert.GenerateLegend);
  CaptionExpert.Free;
end;

procedure TAlignEditMainForm.ActionAddClipbrdExecute(Sender: TObject);
begin
  SourceAlignGrid.InsertFASTASequenceFromClipBoard;
end;

procedure TAlignEditMainForm.ActionAddFromFileExecute(Sender: TObject);
var
  s: TGridRect;
  i,n: integer;
  ext: AnsiString;
begin
  n := SourceAlignGrid.NoOfSeqs;
  InsertFileDlg.FileName := '';
  InsertFileDlg.Options := [ofHideReadOnly,ofAllowMultiSelect,ofEnableSizing, ofPathMustExist, ofFileMustExist];
  InsertFileDlg.Filter := InsertFromFileFilters;
  InsertFileDlg.FilterIndex := 16;
  InsertFileDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(InsertFileDlg.InitialDir);
  if InsertFileDlg.Execute then
  begin
    try
      FIsLoadingFiles := True;

      for i := 0 to InsertFileDlg.Files.Count-1 do
      begin
        ext := LowerCase(ExtractFileExt(InsertFileDlg.Files[i]));
        if Pos(ext, MegaExts) > 0 then
          OpenAMEGA2File(InsertFileDlg.Files[i], false)
        else if (Pos(ext, ABIExts) > 0) or
                (Pos(ext, StadenExts) > 0) or
                (Pos(ext, TextExts) > 0) then
        begin
          SourceAlignGrid.InsertFile(InsertFileDlg.Files[i])
        end
        else if AnsiCompareText(ext, '.mas') = 0 then
          InsertSession(InsertFileDlg.Files[i])
        else
          LoadThirdPartyFileFormat(InsertFileDlg.Files[i], false);
        Application.ProcessMessages;
        if n < SourceAlignGrid.NoOfSeqs then
        begin
          s.Top := SourceAlignGrid.Row-(SourceAlignGrid.NoOfSeqs-n)+1;
          s.Bottom := SourceAlignGrid.Row;
          s.Left := 1;
          s.Right := 1;
          SourceAlignGrid.Selection := s;
          SourceAlignGrid.SelectSequence;
        end;
      end;
      SourceAlignGrid.Invalidate;
      InsertFileDlg.InitialDir := ExtractFileDir(InsertFileDlg.FileName);
    finally
      FIsLoadingFiles := False;
    end;
  end;
end;

procedure TAlignEditMainForm.ActionCopyExecute(Sender: TObject);
begin
  CurAlignGrid.Copy;
end;

procedure TAlignEditMainForm.ActionCutExecute(Sender: TObject);
begin
  CurAlignGrid.Cut;
end;

procedure TAlignEditMainForm.ActionDataTypeExecute(Sender: TObject);
begin
 SourceAlignGrid.IsDNA := not SourceAlignGrid.IsDNA;
 if SourceAlignGrid.IsDNA then
   DataTabSheet.Caption := DNA_SEQUENCES
 else
   DataTabSheet.Caption := PROTEIN_SEQUENCES;
 ProteinTabSheet.TabVisible := SourceAlignGrid.IsDNA and SourceAlignGrid.IsProteinCoding;
 SourceAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.ActionDeleteExecute(Sender: TObject);
begin
  CurAlignGrid.Delete;
end;

procedure TAlignEditMainForm.ActionDelGapExecute(Sender: TObject);
begin
  CurAlignGrid.DeleteGaps;
end;

procedure TAlignEditMainForm.ActionDelGapOnlySitesExecute(Sender: TObject);
begin
 try
   CurAlignGrid.Pack;
   CurAlignGrid.Invalidate;
 except
   on E:Exception do
     ShowMessage('Application Error: ' + E.Message);
 end;
end;

procedure TAlignEditMainForm.ActionDelSeqExecute(Sender: TObject);
begin
  CurAlignGrid.DeleteSequence;
end;

procedure TAlignEditMainForm.ActionEditEnabledExecute(Sender: TObject);
begin
  if CurAlignGrid = TranslatedAlignGrid then exit;
  SourceAlignGrid.EditEnabled := not ActionEditEnabled.Checked;

  if not SourceAlignGrid.EditEnabled then
    StatusBarLabel.Caption := 'Editing disabled.'
  else
    StatusBarLabel.Caption := 'Editing enabled';
end;

procedure TAlignEditMainForm.ActionEditGrpNameExecute(Sender: TObject);
begin
  CurAlignGrid.EditGrpName(MousePos.Y-1);
end;

procedure TAlignEditMainForm.ActionEditSeqNameExecute(Sender: TObject);
begin
  CurAlignGrid.EditSeqName(MousePos.Y-1);
end;

procedure TAlignEditMainForm.ActionEditSequenceNameExecute(Sender: TObject);
begin
  CurAlignGrid.EditSeqName(MousePos.Y-1);
end;

procedure TAlignEditMainForm.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TAlignEditMainForm.ActionExportFASTAExecute(Sender: TObject);
var
  filename: String;
begin
  filename := FileSaveDlg.FileName;
  if CurAlignGrid = SourceAlignGrid then
  begin
    if FileOpenDlg.FileName <> '' then
      FileSaveDlg.FileName := ChangeFileExt(FileOpenDlg.FileName, '.fas');
  end
  else
    FileSaveDlg.FileName := '';

  FileSaveDlg.Filter  := 'FASTA Files|*.fas;*.fst;*.fta|All Files|*.*';
  FileSaveDlg.FilterIndex := 1;
  FileSaveDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(FileSaveDlg.InitialDir);
  if FileSaveDlg.Execute then
  begin
    CurAlignGrid.SaveToFASTAFile(ChangeFileExt(FileSaveDlg.FileName, '.fas'));
    if CurAlignGrid = SourceAlignGrid then
      Caption := ConvertToMegaWinCaption('Alignment Explorer ('+ExtractFileName(FileSaveDlg.FileName)+')')
    else
      FileSaveDlg.FileName := filename;
    FileSaveDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(ExtractFileDir(FileSaveDlg.FileName));
  end;
end;

procedure TAlignEditMainForm.ActionExportMEGAExecute(Sender: TObject);
var
  filename: String;
  fa: boolean;
  Str: String;
begin
  filename := FileSaveDlg.FileName;

  fa := SourceAlignGrid.ForceAlignment;
  try
    SourceAlignGrid.ForceAlignment := true;

    FileSaveDlg.Filter  := 'MEGA Files|*.meg|All Files|*.*';
    if CurAlignGrid = SourceAlignGrid then
    begin
      if MEGAFileName <> '' then
        FileSaveDlg.FileName := MEGAFileName
      else if SessionFileName <> '' then
        FileSaveDlg.FileName := ChangeFileExt(SessionFileName, '.meg')
      else if FileOpenDlg.FileName <> '' then
        FileSaveDlg.FileName := ChangeFileExt(FileOpenDlg.FileName, '.meg')
      else
        FileSaveDlg.FileName := '';
    end
    else
      FileSaveDlg.FileName := '';

    FileSaveDlg.FilterIndex := 1;
    FileSaveDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(FileSaveDlg.InitialDir);
    if FileSaveDlg.Execute then
    begin
      str := CurAlignGrid.Title;
      {$IFNDEF DARWIN}  { for Cocoa widgetset, InputQuery does not come to front}
      if InputQuery('Title', 'Input title of the data', str) then
        CurAlignGrid.Title := str;
      {$ENDIF}
      if CurAlignGrid.IsDNA and (not IsSessionTest) then
        CurAlignGrid.IsProteinCoding := MessageDlg('Protein-coding nucleotide sequence data?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;

      filename := ChangeFileExt(FileSaveDlg.FileName, '.meg');
      CurAlignGrid.SaveToMEGAFile(filename);
      if CurAlignGrid = SourceAlignGrid then
      begin
        MEGAFileName    := filename;
        SessionFileName := ChangeFileExt(filename, '.mas');
        Caption := ConvertToMegaWinCaption('Alignment Explorer ('+ExtractFileName(filename)+')');
      end
      else
        FileSaveDlg.FileName := filename;
      FileSaveDlg.InitialDir :=  ChangeInitialDirectorySaveDialogForMac(ExtractFileDir(FileSaveDlg.FileName));
    end;
  finally
    SourceAlignGrid.ForceAlignment := fa;
  end;
end;

procedure TAlignEditMainForm.ActionExportPaupExecute(Sender: TObject);
var
  filename: String;
begin
  filename := FileSaveDlg.FileName;
  if CurAlignGrid = SourceAlignGrid then
  begin
    if FileOpenDlg.FileName <> '' then
      FileSaveDlg.FileName := ChangeFileExt(FileOpenDlg.FileName, '.nexus'); // not sure what the extension should be
  end
  else
    FileSaveDlg.FileName := '';

  FileSaveDlg.Filter  := 'Nexus Files|*.nex;*.nexus|All Files|*.*';
  FileSaveDlg.FilterIndex := 1;
  FileSaveDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(FileSaveDlg.InitialDir);
  if FileSaveDlg.Execute then
  begin
    CurAlignGrid.SaveToPAUPFile(ChangeFileExt(FileSaveDlg.FileName, '.nexus')); // again not sure about the extnesion
    if CurAlignGrid = SourceAlignGrid then
      Caption := ConvertToMegaWinCaption('Alignment Explorer ('+FileSaveDlg.FileName+')')
    else
      FileSaveDlg.FileName := filename;
    FileSaveDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(ExtractFileDir(FileSaveDlg.FileName));
  end;
end;

procedure TAlignEditMainForm.ActionFillEndExecute(Sender: TObject);
begin
 CurAlignGrid.ForceAlignment := not CurAlignGrid.ForceAlignment;
 CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.ActionFindExecute(Sender: TObject);
var
  seqs: TSequenceList;
  str: String;
  x0,y0: integer;
  flag: boolean;
begin
  try
    seqs := TSequenceList.Create;
    if CurAlignGrid.BlockSelected then
    begin
      CurAlignGrid.GetSelectedData(Seqs);
      str := Seqs[0].SeqData;
    end
    else
  str := CurAlignGrid.SearchBox;
  if InputQuery('Find Motif', 'Enter the sequence to search. '+
                        #13#10+
                        'Extended IUPAC codes such as R (for A or G) and Y (for T or C) can be used.', str) then
  begin
    CurAlignGrid.SearchBox := UpperCase(str);
    if CurAlignGrid.SearchBox = '' then
    begin
      CurAlignGrid.Invalidate;
      exit;
    end;

      x0 := CurAlignGrid.Col;
      y0 := CurAlignGrid.Row;
    if not CurAlignGrid.Find then
      begin
        flag := false;
        if CurAlignGrid.Row < CurAlignGrid.RowCount-1 then
          repeat
            CurAlignGrid.Col := 1;
            CurAlignGrid.Row := CurAlignGrid.Row+1;
            flag := CurAlignGrid.Find;
          until flag or (CurAlignGrid.Row = CurAlignGrid.RowCount-1);
        if not flag then
        begin
          CurAlignGrid.Col := 1;
          CurAlignGrid.Row := 1;
          flag := CurAlignGrid.Find;
          if (y0 > 1) and (not flag) then
          begin
            repeat
              CurAlignGrid.Col := 1;
              CurAlignGrid.Row := CurAlignGrid.Row+1;
              flag := CurAlignGrid.Find;
            until flag or (CurAlignGrid.Row = y0);
          end;
          if not flag then
          begin
            CurAlignGrid.Row := y0;
            CurAlignGrid.Col := x0;
            ShowMessage('The query sequence is not found in the data.');
          end;
        end;
      end;
    end;
  finally
    seqs.Free;
  end;
end;

procedure TAlignEditMainForm.ActionFindMarkExecute(Sender: TObject);
begin
  if not CurAlignGrid.FindMark then
    ShowMessage('No site has been marked.');
end;

procedure TAlignEditMainForm.ActionFindNextExecute(Sender: TObject);
var
  str: String;
  x0,y0: integer;
  flag: boolean;
begin
  str := CurAlignGrid.SearchBox;
  if str = '' then
  begin
    if InputQuery('Find Motif', 'Enter the sequence to search. '+
                        #13#10+
                        'Extended IUPAC codes such as R (for A or G) and Y (for T or C) can be used.', str) then
    begin
      CurAlignGrid.SearchBox := UpperCase(str);
      if CurAlignGrid.SearchBox = '' then
      begin
        CurAlignGrid.Invalidate;
        exit;
      end;
    end
    else
    begin
      exit; // If the user cancels the InputQuery, we shouldn't go on to search for an empty string in the alignment.
    end;
  end;
  x0 := CurAlignGrid.Col;
  y0 := CurAlignGrid.Row;
  if not CurAlignGrid.FindNext then
  begin
    flag := false;
    if CurAlignGrid.Row < CurAlignGrid.RowCount-1 then
      repeat
        CurAlignGrid.Col := 1;
        CurAlignGrid.Row := CurAlignGrid.Row+1;
        flag := CurAlignGrid.FindNext;
      until flag or (CurAlignGrid.Row = CurAlignGrid.RowCount-1);
    if not flag then
    begin
      CurAlignGrid.Row := y0;
      CurAlignGrid.Col := x0;
      ShowMessage('The query sequence is not found in the rest of the data.');
    end;
  end;
end;

procedure TAlignEditMainForm.ActionFindPreviousExecute(Sender: TObject);
var
  str: String;
  x0,y0: integer;
  flag: boolean;
begin
  str := CurAlignGrid.SearchBox;
  if str = '' then
  begin
    if InputQuery('Find Motif', 'Enter the sequence to search. '+
                        #13#10+
                        'Extended IUPAC codes such as R (for A or G) and Y (for T or C) can be used.', str) then
    begin
      CurAlignGrid.SearchBox := UpperCase(str);
      if CurAlignGrid.SearchBox = '' then
      begin
        CurAlignGrid.Invalidate;
        exit;
      end;
    end;
  end;
  x0 := CurAlignGrid.Col;
  y0 := CurAlignGrid.Row;
  if not CurAlignGrid.FindPrev then
  begin
    flag := false;
    if CurAlignGrid.Row > 1 then
      repeat
        CurAlignGrid.Col := 1;
        CurAlignGrid.Row := CurAlignGrid.Row-1;
        flag := CurAlignGrid.FindNext;
      until flag or (CurAlignGrid.Row = 1);
    if not flag then
    begin
      CurAlignGrid.Row := y0;
      CurAlignGrid.Col := x0;
      ShowMessage('The query sequence is not found in the rest of the data.');
    end;
  end;
end;

procedure TAlignEditMainForm.ActionFontSetupExecute(Sender: TObject);
begin
 FontDlg.Font := CurAlignGrid.Font;

 if not FontDlg.Execute then exit;

 SourceAlignGrid.Font := FontDlg.Font;
 TranslatedAlignGrid.Font := FontDlg.Font;

 SourceAlignGrid.ResetSize;
 TranslatedAlignGrid.ResetSize;

 CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.ActionGeneticCodeExecute(Sender: TObject);
var
  MyDlg: TSelectGeneticCodeDlg = nil;
begin
  try
    MyDlg := TSelectGeneticCodeDlg.Create(Self);
    if SourceAlignGrid.CodeTableName = EmptyStr then
      SourceAlignGrid.CodeTableName := 'Standard';
    MyDlg.CodeTableName := SourceAlignGrid.CodeTableName;
    if MyDlg.ShowModal <> mrOK then
      Exit;

    SourceAlignGrid.CodeTableName := MyDlg.CodeTableName;
    SourceAlignGrid.CodeTable     := MyDlg.CodeTable;
    TranslatedAlignGrid.CodeTableName := SourceAlignGrid.CodeTableName;
    TranslatedAlignGrid.CodeTable     := SourceAlignGrid.CodeTable;
    UpdateGeneticCodeLabel;
  finally
    if Assigned(MyDlg) then
      MyDlg.Free;
  end;
end;

procedure TAlignEditMainForm.FormResize(Sender: TObject);
begin
  if Assigned(SourceAlignGrid) and SourceAlignGrid.Visible then
  begin
    SourceAlignGrid.Invalidate;
    InitScrollBars1(False);
  end;
  if Assigned(TranslatedAlignGrid) and TranslatedAlignGrid.Visible then
  begin
    TranslatedAlignGrid.Invalidate;
    InitScrollBars2(False);
  end;
end;

procedure TAlignEditMainForm.ActionGroupNameVisibleExecute(Sender: TObject);
begin
 CurAlignGrid.Cols.Col[GROUP_COLUMN].Visible := not CurAlignGrid.Cols.Col[GROUP_COLUMN].Visible;
 CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.ActionHighlightMotifExecute(Sender: TObject);
begin
 CurAlignGrid.HighlightSearchBox := not CurAlignGrid.HighlightSearchBox;
 CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
var
  si: integer;
begin
  Handled := True;
  if FIsLoadingFiles or (not Visible) or FAlignmentIsInProgress then
    Exit;
  if MilliSecondsBetween(Now, FLastActionListUpdateTime) < ACTION_LIST_UPDATE_FREQUENCY then
    Exit;
  if not Visible then
    Exit;
  if PageControl1.ActivePageIndex = 1 then
  begin
    if CurAlignGrid <> TranslatedAlignGrid then
    begin
      CurAlignGrid := TranslatedAlignGrid;
      ActionEditGrpName.Enabled := False;
      ActionEditSeqName.Enabled := False;
    end;
  end
  else
  begin
    if CurAlignGrid <> SourceAlignGrid then
    begin
      CurAlignGrid := SourceAlignGrid;
      ActionEditGrpName.Enabled := True;
      ActionEditSeqName.Enabled := True;
    end;
  end;

  with CurAlignGrid do
  begin
    FHasData := (not CurAlignGrid.Empty);
    FIsSourceGrid := (CurAlignGrid = SourceAlignGrid);

    if FHasData then
    begin
      ActionNewFile.Enabled := FIsSourceGrid;
      ActionOpenFile.Enabled := ActionNewFile.Enabled;
      OpenFileItem.Enabled   := ActionNewFile.Enabled;
      OpenAlignmentSessionItem.Enabled   := ActionNewFile.Enabled;
      ImportFromFileItem.Enabled   := ActionNewFile.Enabled;
      SaveFileItem.Enabled      := True;
      ActionExportFASTA.Enabled := True;
      ActionExportMEGA.Enabled  := True;
      ActionExportPAUP.Enabled  := True;
      ActionSaveSession.Enabled := True;
      ActionClose.Enabled       := True;
      ActionSelectSite.Enabled := True;
      ActionSelectSeq.Enabled := True;
      ActionSelectAll.Enabled := True;
      ActionReferToGenbank.Enabled := SequenceSelected and (Sequence[CurAlignGrid.Row-1].AccessionNum <> '');
    end
    else { then the align grid has no data}
    begin
      ActionNewFile.Enabled := True;
      ActionOpenFile.Enabled := True;
      OpenFileItem.Enabled   := True;
      OpenAlignmentSessionItem.Enabled := True;
      ImportFromFileItem.Enabled := True;
      SaveFileItem.Enabled      := False;
      ActionExportFASTA.Enabled := False;
      ActionExportMEGA.Enabled  := False;
      ActionExportPAUP.Enabled  := False;
      ActionSaveSession.Enabled := False;
      ActionClose.Enabled       := False;
      ActionSelectSite.Enabled := False;
      ActionSelectSeq.Enabled := False;
      ActionSelectAll.Enabled := False;
      ActionReferToGenbank.Enabled := False;
    end;

    ActionDataType.Enabled := FIsSourceGrid;
    DNAMenuItem.Enabled        := ActionDataType.Enabled;
    DNAMenuItem.Checked        := SourceAlignGrid.IsDNA;
    ProteinMenuItem.Enabled    := ActionDataType.Enabled;
    ProteinMenuItem.Checked    := not SourceAlignGrid.IsDNA;

    ActionGeneticCode.Enabled := ActionDataType.Enabled and SourceAlignGrid.IsProteinCoding;
    ActionTranslate.Enabled := FHasData and SourceAlignGrid.IsProteinCoding;

    ActionUndo.Enabled := UndoEnabled;
    ActionCopy.Enabled := FHasData and CopyEnabled;
    ActionCut.Enabled := FHasData and DeleteEnabled and FIsSourceGrid;
    ActionPaste.Enabled := PasteEnabled;
    ActionDelete.Enabled := FHasData and DeleteEnabled and FIsSourceGrid;
    ActionAddSeq.Enabled := EditEnabled;
    ActionAddClipbrd.Enabled := EditEnabled;
    ActionAddFromFile.Enabled := EditEnabled;



    ActionEditSeqName.Enabled := FHasData and EditEnabled;

    ActionToUpper.Enabled := FHasData;
    ActionDelSeq.Enabled := FHasData and (SequenceSelected);
    ActionFillEnd.Enabled := FHasData and FIsSourceGrid;
    ActionFillEnd.Checked := CurAlignGrid.ForceAlignment;

    ActionFind.Enabled := FHasData;
    ActionFindNext.Enabled := FHasData;
    ActionFindPrevious.Enabled := FHasData;
    ActionMarkSite.Enabled := FHasData;
    ActionFindMark.Enabled := FHasData;
    ActionHighlightMotif.Enabled := FHasData and (SearchBox <> '');
    ActionHighlightMotif.Checked := HighlightSearchBox;


    ActionUnmarkSite.Enabled := FHasData and Marked;
    ActionAlignMarkedSites.Enabled := FHasData and Marked;
    ActionClustalAlign.Enabled := FHasData and
                                  (not ((GetNoOfSelectedSites > 1) and (GetNoOfSelectedSeqs = 1)));
    ActionClustalAlignCodons.Enabled := (CurAlignGrid.IsDNA and (CurAlignGrid.IsProteinCoding));
    ActionMuscleAlignCodons.Enabled := ActionClustalAlignCodons.Enabled;
    ActionMuscleAlign.Enabled := ActionClustalAlign.Enabled;
    ActionMoveLeft.Enabled := FHasData;
    ActionMoveRight.Enabled := FHasData;
    ActionDelGapOnlySites.Enabled := FHasData;

    ActionEditEnabled.Enabled := FHasData and FIsSourceGrid;
    ActionEditEnabled.Checked := CurAlignGrid.EditEnabled;
    ActionUseColor.Enabled := FHasData;
    ActionUseColor.Checked := FHasData and ShowColor;
    ActionShowBGColor.Enabled := FHasData;
    ActionShowBGColor.Checked := FHasData and ShowBGColor;
    ActionRevComp.Enabled := FHasData and IsDNA and (BlockSelected or SequenceSelected);
    ActionRev.Enabled :=ActionRevComp.Enabled;
    ActionComp.Enabled := ActionRevComp.Enabled;
    ActionFontSetup.Enabled := FHasData;

    ActionSelectSiteOnClick.Enabled := FHasData;
    ActionSelectSeqOnClick.Enabled := FHasData;

    SiteIndexLabel.Enabled := FHasData;
    SiteIndexNSEdit.Enabled := FHasData;
    SiteIndexNSEdit.MaxValue := CurAlignGrid.MaxNoOfSites;
    if FHasData and SearchToolbar.Visible and (not SiteIndexNSEdit.Focused) then
    begin
      si := CurAlignGrid.SiteIndex(WGapRBtn.Checked);
      if SiteIndexNSEdit.Value <> si then
        SetSiteSpinEditValue(si);
    end;

    WGapRBtn.Enabled := FHasData;
    WOGapRBtn.Enabled := FHasData;

    ActionGroupNameVisible.Checked := CurAlignGrid.Cols.Col[GROUP_COLUMN].Visible;
    ActionSpeciesVisible.Checked := CurAlignGrid.Cols.Col[SPECIES_COLUMN].Visible;
    ActionAnalyze.Enabled := FHasData and (not FTransferringAlignmentForAnalysis);

    MarkConserved1.Enabled := ActionUseColor.Enabled;
    MarkConserved1.Checked := CurAlignGrid.MarkConsensus;
    Mark_none1.Checked     := not CurAlignGrid.MarkConsensus;
    if MarkConserved1.Checked then
    begin
      Mark50level1.Checked  := (CurAlignGrid.ConsensusValue = 50);
      Mark60level1.Checked  := (CurAlignGrid.ConsensusValue = 60);
      Mark70level1.Checked  := (CurAlignGrid.ConsensusValue = 70);
      Mark80level1.Checked  := (CurAlignGrid.ConsensusValue = 80);
      Mark90level1.Checked  := (CurAlignGrid.ConsensusValue = 90);
      Mark100level1.Checked := (CurAlignGrid.ConsensusValue = 100);
    end
    else
    begin
      Mark50level1.Checked  := False;
      Mark60level1.Checked  := False;
      Mark70level1.Checked  := False;
      Mark80level1.Checked  := False;
      Mark90level1.Checked  := False;
      Mark100level1.Checked := False;
    end;
  end;
  FLastActionListUpdateTime := Now;
end;

procedure TAlignEditMainForm.ActionMarkSiteExecute(Sender: TObject);
begin
  CurAlignGrid.MarkSite;
end;

procedure TAlignEditMainForm.ActionMoveLeftExecute(Sender: TObject);
begin
  CurAlignGrid.MoveLeft;
end;

procedure TAlignEditMainForm.ActionMoveRightExecute(Sender: TObject);
begin
  CurAlignGrid.MoveRight;
end;

procedure TAlignEditMainForm.ActionMuscleAlignCodonsExecute(Sender: TObject);
begin
  ActionMuscleAlignExecute(Sender);
end;

procedure TAlignEditMainForm.ActionMuscleAlignExecute(Sender: TObject);
var
  MuscleExeFile: String;
  MuscleLink: TMuscleLink = nil;
  MAI: TAnalysisInfo;
  ARP: TRuntimeProgress;
  DataType: TSnTokenCode;
  xSites: AnsiString;
  isSuccess: Boolean = False;
  aMsg: String = '';
  usageData: String = '';
begin
  FCurrentDirectory := GetCurrentDir;
  MuscleExeFile := GetMuscleExe;
  if not FileExists(MuscleExeFile) then
  begin
    ShowMessage('MEGA installation appears to be corrupted; critical MUSCLE files are missing. Please reinstall MEGA.');
    Exit;
  end;

  if not isPrototyper then
  begin
    if (not CurAlignGrid.BlockSelected) and (not CurAlignGrid.SequenceSelected) then
    begin
      if MessageDlg('Nothing selected for alignment. Select all?', mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
        CurAlignGrid.SelectAll
      else
        Exit;
    end;

    if CurAlignGrid.GetNoOfSelectedSeqs < 2 then
    begin
      ShowMessage('At least two sequences need to be selected for alignment.');
      Exit;
    end;

    if not CurAlignGrid.CanAlignSelectedData then
    begin
      ShowMessage('Alignable data not found. Cannot align only indels');
      Exit;
    end;
  end;

  try
    SetAlignmentInProgress(True);
    ARP := TRuntimeProgress.Create(Application);
    MAI := TAnalysisInfo.Create;
    MAI.MyProcessPack := TProcessPack.Create;
    MAI.MyProcessPack.AddProcessType(ppAlign);
    MAI.MyProcessPack.AddProcessType(ppMuscle);
    MAI.ARP := ARP;
    MAI.ARP.HasCmdLineOutput := True;
    ARP := nil;

    MuscleLink := TMuscleLink.Create(nil);
    MuscleLink.OnTerm := nil; { the MuscleLinkThread has it's own OnTerminate procedure}
    if ((Sender = MusclecDNABtn) or (Sender = ActionMuscleAlignCodons)) and (CurAlignGrid.IsDNA) then
    begin
      MuscleLink.IsDNA := True;
      MuscleLink.IscDNA := True;
      DataType := snCodingDna;
    end
    else
    if (CurAlignGrid.IsDNA) then
    begin
      MuscleLink.IsDNA := True;
      MuscleLInk.IscDNA := False;
      DataType := snNucleotide;
    end
    else
    begin
      MuscleLink.IsDNA := False;
      MuscleLink.IscDNA := False;
      DataType := snProtein;
    end;

    if not GetMuscleOptions(MuscleLink.MuscleOptions, DataType) then
    begin
      SetAlignmentInProgress(False);
      CurAlignGrid.Invalidate;
      Exit;
    end;
    if not FinalizeMuscleOptions(MuscleLink.MuscleOptions, DataType) then
    begin
      SetAlignmentInProgress(False);
      CurAlignGrid.Invalidate;
      Exit;
    end;
    xSites := EmptyStr;
    if not SelectedToFasta(MuscleLink.InputFileName, True, True, xSites, MuscleLink.SeqList) then
      raise Exception.Create('Failed to convert sequence data into the expected input format for MUSCLE');
    MuscleLink.XSites := xSites;
    SetAlignmentInProgress(True);
    usageData := GetUsageDataAsJsonString('muscle', MuscleLink.MuscleOptions, DataType);
    UpdateUsageStatistics(usageData);
    FMuscleThread := LaunchMuscleLinkThread(MuscleLink, MAI, MuscleThreadFinished, isSuccess, aMsg);
    if not isSuccess then
      raise Exception.Create(aMsg);
  except
    on E:Exception do
    begin
      ShowMessage('An error occurred when doing MUSCLE sequence alignment: ' + E.Message);
      SetAlignmentInProgress(False);
      if Assigned(FMuscleThread) then
        FreeAndNil(FMuscleThread);
    end;
  end;
end;

procedure TAlignEditMainForm.ActionNewFileExecute(Sender: TObject);
var
  check: Integer;
begin
  if FAlignmentIsInProgress then
  begin
    ShowMessage('A sequence alignment is currently in progress. You must cancel that alignment before creating a new file');
    Exit;
  end;

  if not SourceAlignGrid.Empty then
    if MessageDlg('Are you creating a new data set?', mtConfirmation, [mbYes, mbCancel], 0) = mrCancel then
      exit;

  if SourceAlignGrid.Modified then
  begin
    check := MessageDlg('Save the current alignment session to file (recommended)?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if check = mrYes then
      ActionSaveSessionExecute(Sender)
    else if check = mrCancel then
      exit;
  end;

  check := AlignmentDataTypeDlg.ShowModal;
  if check = mrCancel then // cancel
  begin
    Close;
    exit;
  end;

  SourceAlignGrid.IsDNA := (check = mrYes); // DNA
  SourceAlignGrid.IsProteinCoding := True;
  CreatedFromFileClickEvent := True;
  InitForm;
  SourceAlignGrid.Clear;
  ActionAddSeqExecute(nil);
  SourceAlignGrid.Invalidate;
  Caption := ConvertToMegaWinCaption('Alignment Explorer');
  SessionFileName := '';
  MEGAFileName    := '';
  FileSaveDlg.FileName := '';
end;

procedure TAlignEditMainForm.ActionOpenFileExecute(Sender: TObject);
var
  check: Word;
begin
  if FAlignmentIsInProgress then
  begin
    ShowMessage('A sequence alignment is currently in progress. To open another file, you must cancel that alignment or wait for it to finish');
    Exit;
  end;

  if (not SourceAlignGrid.Empty) and SourceAlignGrid.Modified then
  begin
    check := MessageDlg('Save the current alignment session to file?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if check = mrYes then
    begin
      ActionSaveSessionExecute(Sender);
      if SourceAlignGrid.Modified then
        exit;
    end
    else if check = mrCancel then
    begin
      RemoveWindowFromTray(Self);
      exit;
    end;
  end;

  FileOpenDlg.Options := [ofHideReadOnly,ofEnableSizing];
  if Sender = OpenAlignmentSessionItem then
  begin
    FileOpenDlg.Filter  := AlignmentSessionFilter;
    FileOpenDlg.Filename := '';
  end
  else if Sender = ImportFromFileItem then
  begin
    FileOpenDlg.Filter  := ImportFromFileFilters;
    {$IFDEF DEBUG}
    FileOpenDlg.FilterIndex := 11;
    {$ENDIF}
    {$IFDEF DARWIN} { on macOS it is not obvious that you can filter by file ext}
    FileOpenDlg.FilterIndex := 11;
    {$ENDIF}
    FileOpenDlg.Filename := '';
  end
  else
  begin
    FileOpenDlg.Filter  := ImportFromFileFilters +'|'+ AlignmentSessionFilter;  // Shows "Supported files" filter first.
    FileOpenDlg.Filename := '';
    {$IFDEF DEBUG}
    FileOpenDlg.FilterIndex := 11;
    {$ENDIF}
    {$IFDEF DARWIN} { on macOS it is not obvious that you can filter by file ext}
    FileOpenDlg.FilterIndex := 11;
    {$ENDIF}
  end;
  FileOpenDlg.InitialDir := GetCurrentDir;
  FileOpenDlg.Title       := 'Open a File';
  if FileOpenDlg.Execute then
  begin
    SessionFileName := ChangeFileExt(FileOpenDlg.FileName, '.mas');
    UpdateRecentFilesList(FileOpenDlg.FileName);
  end
  else
    exit;
  SourceAlignGrid.Clear;
  TranslatedAlignGrid.Clear;
  ShowAlignmentFromFile(FileOpenDlg.FileName);
end;

procedure TAlignEditMainForm.ActionPasteExecute(Sender: TObject);
begin
  CurAlignGrid.Paste;
  InitScrollBars1(False);
  InitScrollBars2(False);
end;

procedure TAlignEditMainForm.ActionQueryExecute(Sender: TObject);
var
  aChBrowser : TMegaBrowserFrm;
  Seqs: TSequenceList;
  gi: String;
begin
  Seqs := TSequenceList.Create;

  if (not CurAlignGrid.Empty) and (CurAlignGrid.IsDNA) then
    if CurAlignGrid.BlockSelected or CurAlignGrid.SequenceSelected then
      CurAlignGrid.GetSelectedData(Seqs);

  aChBrowser := CreateNewChromiumBrowserWindow(bmBrowser);
  if Seqs.Count = 0 then
    gi := ''
  else
    gi := Seqs[0].UIDS;

  if gi = '' then
    aChBrowser.QueryGene(CurAlignGrid.IsDNA)
  else
    aChBrowser.ShowGene(gi, CurAlignGrid.IsDNA);

  Seqs.Free;
end;

procedure TAlignEditMainForm.ActionRevCompExecute(Sender: TObject);
begin
  SourceAlignGrid.ReverseCompliment;
end;

procedure TAlignEditMainForm.ActionRevExecute(Sender: TObject);
begin
  SourceAlignGrid.ReverseSelection;
end;

procedure TAlignEditMainForm.ActionSaveSessionExecute(Sender: TObject);
var
  Filename: String;
begin
  Filename := ExtractFileName(FileSaveDlg.FileName);

  FileSaveDlg.Filter  := AlignmentSessionFilter;
  if SessionFileName <> '' then
    FileSaveDlg.FileName := ExtractFileName(SessionFileName)
  else if MEGAFileName <> '' then
    FileSaveDlg.FileName := ChangeFileExt(ExtractFileName(MEGAFileName), '.mas')
  else if FileOpenDlg.FileName <> '' then
    FileSaveDlg.FileName := ChangeFileExt(FileName, '.mas');

  FileSaveDlg.FilterIndex := 1;
  FileSaveDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(FileSaveDlg.InitialDir);
  if FileSaveDlg.Execute then
  begin
    if PageControl1.ActivePageIndex = 1 then
      PageControl1.ActivePageIndex := 0;
    if SaveSession(ChangeFileExt(FileSaveDlg.FileName, '.mas')) then
    begin
      SessionFileName := FileSaveDlg.FileName;
      Caption :=  ConvertToMegaWinCaption('Alignment Explorer ('+ExtractFileName(FileSaveDlg.FileName)+')')
    end
    else
      FileSaveDlg.FileName := Filename;
    FileSaveDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(ExtractFileDir(FileSaveDlg.FileName));
    UpdateRecentFilesList(FileSaveDlg.FileName);
  end;
end;

procedure TAlignEditMainForm.ActionSelectAllExecute(Sender: TObject);
begin
  CurAlignGrid.SelectAll;
  if Assigned(CurAlignGrid.OnAfterSelection) then
  begin
    CurAlignGrid.OnAfterSelection(Sender, 0, 0);
  end;
  {$IFDEF DARWIN}
  Invalidate;
  Application.ProcessMessages;
  {$ENDIF}
end;

procedure TAlignEditMainForm.ActionSelectSeqExecute(Sender: TObject);
begin
  CurAlignGrid.SelectSequence;
end;

procedure TAlignEditMainForm.ActionSelectSeqOnClickExecute(Sender: TObject);
begin
 CurAlignGrid.Row := MousePos.Y;
 CurAlignGrid.SelectSequence;
end;

procedure TAlignEditMainForm.ActionSelectSiteExecute(Sender: TObject);
begin
  CurAlignGrid.SelectSite;
end;

procedure TAlignEditMainForm.ActionSelectSiteOnClickExecute(Sender: TObject);
begin
 CurAlignGrid.Col := MousePos.X;
 CurAlignGrid.SelectSite;
end;

procedure TAlignEditMainForm.ActionSetupClustalExecute(Sender: TObject);
var
  ClustalSetupDlg: TClustalSetupDlg = nil;
  setupReturnedOK: Boolean = False;
begin
  ClustalSetupResult := False;
  ClustalSetupDlg := TClustalSetupDlg.Create(Self);
  ClustalSetupDlg.ClustalW := ClustalW;

  with ClustalW do
  begin
    ClustalSetupDlg.IsUsingcDNA := ClustalW.RequestUntranslate;
    ClustalSetupDlg.DNAMatrixCBx.ItemIndex     := integer(DNAMatrix);
    ClustalSetupDlg.ProteinMatrixCBx.ItemIndex := integer(ProteinMatrix);

    ClustalSetupDlg.DNAPWGapOpenNEdit.Value   := DNAPWGapOpenPenalty;
    ClustalSetupDlg.DNAPWGapExtendNEdit.Value := DNAPWGapExtendPenalty;
    ClustalSetupDlg.DNAGapOpenNEdit.Value     := DNAGapOpenPenalty;
    ClustalSetupDlg.DNAGapExtendNEdit.Value   := DNAGapExtendPenalty;
    ClustalSetupDlg.ProteinPWGapOpenNEdit.Value   := ProteinPWGapOpenPenalty;
    ClustalSetupDlg.ProteinPWGapExtendNEdit.Value := ProteinPWGapExtendPenalty;
    ClustalSetupDlg.ProteinGapOpenNEdit.Value     := ProteinGapOpenPenalty;
    ClustalSetupDlg.ProteinGapExtendNEdit.Value   := ProteinGapExtendPenalty;

    ClustalSetupDlg.TransitionWeightNEdit.Value := TransitionWeight;

    ClustalSetupDlg.DivergentCutoffNEdit.Value := DivergentCutoff;
    if UseNegativeMatrix then
      ClustalSetupDlg.UseNegativeCBx.ItemIndex   := 0
    else
      ClustalSetupDlg.UseNegativeCBx.ItemIndex   := 1;

    if ResidueSpecificPenalty then
      ClustalSetupDlg.ResidueSpecificCBx.ItemIndex := 0
    else
      ClustalSetupDlg.ResidueSpecificCBx.ItemIndex := 1;

    if HydrophilicPenalty then
      ClustalSetupDlg.HidrophilicCBx.ItemIndex := 0
    else
      ClustalSetupDlg.HidrophilicCBx.ItemIndex := 1;

    if EndGapSeparation then
      ClustalSetupDlg.EndGapSeparationCBx.ItemIndex := 0
    else
      ClustalSetupDlg.EndGapSeparationCBx.ItemIndex := 1;

    ClustalSetupDlg.GapSeparationDistNEdit.Value := GapSeparationDistance;

    ClustalSetupDlg.GeneticCodeLabel.Visible := CurAlignGrid.IsProteinCoding;
    ClustalSetupDlg.GeneticCodeCombo.Visible := CurAlignGrid.IsProteinCoding;
    ClustalSetupDlg.KeepGapsCBx.Checked := not ResetGaps;

    if CurAlignGrid.IsDNA and (not ClustalW.RequestUntranslate)then
      ClustalSetupDlg.SetIsDna(True)
    else
      ClustalSetupDlg.SetIsDna(False);

    setupReturnedOK := (ClustalSetupDlg.ShowModal = mrOK);

    if setupReturnedOK then
    begin
      DNAMatrix                  := TDNAMatrix(ClustalSetupDlg.DNAMatrixCBx.ItemIndex);
      ProteinMatrix              := TProteinMatrix(ClustalSetupDlg.ProteinMatrixCBx.ItemIndex);

      DNAPWGapOpenPenalty        := ClustalSetupDlg.DNAPWGapOpenNEdit.Value;
      DNAPWGapExtendPenalty      := ClustalSetupDlg.DNAPWGapExtendNEdit.Value;
      DNAGapOpenPenalty          := ClustalSetupDlg.DNAGapOpenNEdit.Value;
      DNAGapExtendPenalty        := ClustalSetupDlg.DNAGapExtendNEdit.Value;
      ProteinPWGapOpenPenalty    := ClustalSetupDlg.ProteinPWGapOpenNEdit.Value;
      ProteinPWGapExtendPenalty  := ClustalSetupDlg.ProteinPWGapExtendNEdit.Value;
      ProteinGapOpenPenalty      := ClustalSetupDlg.ProteinGapOpenNEdit.Value;
      ProteinGapExtendPenalty    := ClustalSetupDlg.ProteinGapExtendNEdit.Value;

      DivergentCutoff            := ClustalSetupDlg.DivergentCutoffNEdit.Value;
      TransitionWeight           := ClustalSetupDlg.TransitionWeightNEdit.Value;
      UseNegativeMatrix          := ClustalSetupDlg.UseNegativeCBx.ItemIndex = 0;

      ResidueSpecificPenalty     := ClustalSetupDlg.ResidueSpecificCBx.ItemIndex = 0;
      HydrophilicPenalty         := ClustalSetupDlg.HidrophilicCBx.ItemIndex = 0;
      EndGapSeparation           := ClustalSetupDlg.EndGapSeparationCBx.ItemIndex = 0;
      GapSeparationDistance      := ClustalSetupDlg.GapSeparationDistNEdit.Value;

      ResetGaps                  := not ClustalSetupDlg.KeepGapsCBx.Checked;
      GuideTreeFile           := ClustalSetupDlg.SelectedGuideTreeEdit.Text;
      ClustalSetupResult := true;
    end;
  end;
  ClustalSetupDlg.Free;
end;

procedure TAlignEditMainForm.HorizScrollbar2Change(Sender: TObject);
var
  mouseLeftButtonDown: Boolean = False;
begin
  try
    mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
    if (not mouseLeftButtonDown) and FScrollStarted then
    begin
      SendMessage(HorizScrollbar2.Handle, WM_CANCELMODE, WM_CANCELMODE, 0);
      FScrollStarted := False;
      Exit;
    end;
    FHScrollPos := HorizScrollbar2.Position;
    if (FHScrollPos >= TranslatedAlignGrid.FixedCols) and (FHScrollPos <= TranslatedAlignGrid.ColCount) then
    begin
      TranslatedAlignGrid.LeftCol := min(FHScrollPos, TranslatedAlignGrid.MaxNoOfSites - TranslatedAlignGrid.VisibleColCount + 1);
      FScrollStarted := True;
    end;
  except
    FScrollStarted := False;
  end;
end;

procedure TAlignEditMainForm.HorizScrollbar2Scroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
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

procedure TAlignEditMainForm.HorizScrollbarChange(Sender: TObject);
var
  mouseLeftButtonDown: Boolean = False;
begin
  try
    mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
    if (not mouseLeftButtonDown) and FScrollStarted then
    begin
      SendMessage(HorizScrollbar.Handle, WM_CANCELMODE, WM_CANCELMODE, 0);
      FScrollStarted := False;
      Exit;
    end;
    FHScrollPos := HorizScrollbar.Position;
    if (FHScrollPos >= SourceAlignGrid.FixedCols) and (FHScrollPos <= SourceAlignGrid.ColCount) then
    begin
      SourceAlignGrid.LeftCol := min(FHScrollPos, SourceAlignGrid.MaxNoOfSites - SourceAlignGrid.VisibleColCount + 1);
      FScrollStarted := True;
    end;
  except
    FScrollStarted := False;
  end;
end;

procedure TAlignEditMainForm.HorizScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
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

procedure TAlignEditMainForm.IniPropStorageRestoreProperties(Sender:TObject);
var
  filename: String;
  //aComponent: TComponent = nil;
begin
  try
    //aComponent := TComponent(SourceAlignGrid);
    filename := GetPrivateFile(sfAlignEditMainForm, False);
    {$IFDEF DEBUG}
    { for debugging purposes you can use the LoadCustomComponentSessionProperties below which
      will read/write the sfAlignEditMain in text format like lfm files. However, you will
      first need to find the existing sfAlignEditMainForm binary file and delete it. Then when
      you are done debugging, you need to delete the sfAlignEditMainForm text file. You also
      need to update the IniPropStorageSaveProperties procedure}
    if FileExists(filename) then
      //LoadCustomComponentSessionProperties(aComponent, filename);
      TIniFormStream.IniPropStorageRestoreProperties(Self, filename);
    {$ELSE}
    if FileExists(filename) then
      TIniFormStream.IniPropStorageRestoreProperties(Self, filename);
    {$ENDIF}
  except
    on E:Exception do { if it somehow goes wonky, delete the existing file because it may become corrupted}
    begin
      if FileExists(filename) then
        DeleteFile(filename);
    end;
  end;
end;

procedure TAlignEditMainForm.IniPropStorageSavingProperties(Sender:TObject);
var
  filename: String;
  //aComponent: TComponent = nil;
begin
  //aComponent := TComponent(SourceAlignGrid);
  filename := GetPrivateOutputFile(sfAlignEditMainForm);
  {$IFDEF DEBUG}
  { for debugging purposes you can use the SaveCustomComponentSessionProperties below which
    will read/write the sfAlignEditMain in text format like lfm files. However, you will
    first need to find the existing sfAlignEditMainForm binary file and delete it. Then when
    you are done debugging, you need to delete the sfAlignEditMainForm text file. You also
    need to update the IniPropStorageRestoreProperties procedure}
  //SaveCustomComponentSessionProperties(aComponent, filename);
  TIniFormStream.IniPropStorageSaveProperties(Self, filename);
  {$ELSE}
  TIniFormStream.IniPropStorageSaveProperties(Self, filename);
  {$ENDIF}
end;

procedure TAlignEditMainForm.ActionShowBgColorExecute(Sender: TObject);
begin
 SourceAlignGrid.ShowBGColor := not ActionShowBGColor.Checked;
 TranslatedAlignGrid.ShowBGColor := not ActionShowBGColor.Checked;
 CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.ActionShowHelpExecute(Sender: TObject);
begin
  ShowContextSensitiveHelp(HelpKeyword);
end;

procedure TAlignEditMainForm.ActionShowInfoExecute(Sender: TObject);
var
  aInfo: String = '';
  aSeq: TSequence = nil;
begin
  aSeq := CurAlignGrid.Sequence[MousePos.Y - 1];
  if Assigned(aSeq) then
  begin
    aInfo := InputBox(aSeq.SeqName, '', aSeq.SeqInfo);
    aSeq.SeqInfo := aInfo;
  end
  else
    ShowMessage('Application error: no sequence is selected');
end;

procedure TAlignEditMainForm.ActionSpeciesVisibleExecute(Sender: TObject);
begin
 CurAlignGrid.Cols.Col[0].Visible := not CurAlignGrid.Cols.Col[0].Visible;
 CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.ActionToUpperExecute(Sender: TObject);
begin
  CurAlignGrid.ToUpper;
end;

procedure TAlignEditMainForm.ActionTraceEditExecute(Sender: TObject);
var
  TraceEditForm: TTraceEditForm;
  fname: String = '';
begin
  if CurAlignGrid.SequenceSelected and (CurAlignGrid.GetNoOfSelectedSeqs = 1) then
    if CurAlignGrid.Sequence[CurAlignGrid.Selection.Top-1].FileName <> '' then
      fname := CurAlignGrid.Sequence[CurAlignGrid.Selection.Top-1].FileName;

  TraceEditForm := OpenFileInNewTraceEditorWindow(fname);
  if Assigned(TraceEditForm) then
  begin
    if fname <> '' then
      CurAlignGrid.Sequence[CurAlignGrid.Selection.Top-1].FileName := TraceEditForm.TraceEdit.Data.FileName;
    TraceEditForm.Show;
  end;
end;

procedure TAlignEditMainForm.ActionTranslateExecute(Sender: TObject);
var
  flag: boolean = False;
begin
  if PageControl1.ActivePageIndex = 0 then
  begin
    PageControl1Changing(nil, flag);
    PageControl1.ActivePageIndex := 1;
  end
  else if PageControl1.ActivePageIndex = 1 then
  begin
    PageControl1.ActivePageIndex := 0;
  end;
end;

procedure TAlignEditMainForm.ActionUndoExecute(Sender: TObject);
begin
  CurAlignGrid.Undo;
end;

procedure TAlignEditMainForm.ActionUnmarkSiteExecute(Sender: TObject);
begin
  CurAlignGrid.UnMarkSite;
end;

procedure TAlignEditMainForm.ActionUseColorExecute(Sender: TObject);
begin
 SourceAlignGrid.ShowColor := not ActionUseColor.Checked;
 TranslatedAlignGrid.ShowColor := not ActionUseColor.Checked;
 CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.ClustalcDNABtnClick(Sender: TObject);
begin
  ActionClustalAlignExecute(Sender);
end;

procedure TAlignEditMainForm.ClustalDNABtnClick(Sender: TObject);
begin
  ActionClustalAlignExecute(Sender);
end;

procedure TAlignEditMainForm.ClustalOptionsPopupPopup(Sender: TObject);
begin
  ClustalDNABtn.Visible     := CurAlignGrid.IsDNA;
  ClustalcDNABtn.Visible    := CurAlignGrid.IsDNA and (CurAlignGrid.IsProteinCoding);
  ClustalProteinBtn.Visible := not CurAlignGrid.IsDNA;
end;

procedure TAlignEditMainForm.ClustalProteinBtnClick(Sender: TObject);
begin
  ActionClustalAlignExecute(Sender);
end;

procedure TAlignEditMainForm.DataTabSheetShow(Sender: TObject);
var
  seqlist: TSequenceList = nil;
  sl: TStringList = nil;
  i: integer;
  dataSelected: boolean;
  x10: Integer = -1;
  x11: Integer = -1;
  y10: Integer = -1;
  y11: integer = -1;
  x20: Integer = -1;
  x21: Integer = -1;
  y20: Integer = -1;
  y21: integer = -1;
begin
  if (not Assigned(TranslatedAlignGrid)) or TranslatedAlignGrid.Empty then
    exit;

  try
    try
      seqlist := TSequenceList.Create;
      sl := TStringList.Create;

      SourceAlignGrid.SelectedBlock(x10,y10,x11,y11);
      TranslatedAlignGrid.SelectedBlock(x20,y20,x21,y21);

      TranslatedAlignGrid.UnTranslate;
      TranslatedAlignGrid.GetSeqData(seqlist);
      TranslatedAlignGrid.Clear;

      for i := 0 to seqlist.Count-1 do
        sl.Add(seqlist[i].SeqData);
      //{$IFDEF DEBUG}WriteToDevConsole(sl.DelimitedText);{$ENDIF}
      dataSelected := SourceAlignGrid.BlockSelected or SourceAlignGrid.SequenceSelected;
      if not FTranslateSelectionOnly then
        SourceAlignGrid.SelectAll;

      SourceAlignGrid.Replace(sl);

      if not dataSelected then
      begin
        x10 := (x20-1)*3 +1;
        y10 := y20;
        y11 := y21;
        if (x21 = x20) and (y21 = y20) then
          x11 := x10
        else
          x11 := x21*3;

        SourceAlignGrid.SelectBlock(x10,y10,x11,y11);
      end;

      SourceAlignGrid.Invalidate;

      if not SourceAlignGrid.EditEnabled then
        StatusBarLabel.Caption := 'Editing disabled.'
      else
        StatusBarLabel.Caption := 'Editing enabled';
    except
      on E:Exception do
        ShowMessage('Oh no! An error occurred: ' + E.Message);
    end;
  finally
    if Assigned(seqlist) then
      seqlist.Free;
    if Assigned(sl) then
      sl.Free;
  end;
end;

procedure TAlignEditMainForm.DNAMenuItemClick(Sender: TObject);
begin
  SourceAlignGrid.IsDNA := true;
  DataTabSheet.Caption := 'DNA Sequences';
  ProteinTabSheet.TabVisible := true;
  SourceAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.EditToolbarItemClick(Sender: TObject);
begin
  EditToolbar.Visible := (not EditToolbar.Visible);
  EditToolbarItem.Checked := EditToolbar.Visible;
  PositionToolbars;
end;

procedure TAlignEditMainForm.FileToolbarItemClick(Sender: TObject);
begin
  FileToolbar.Visible := (not FileToolbar.Visible);
  FileToolbarItem.Checked := FileToolbar.Visible;
  PositionToolbars;
end;

procedure TAlignEditMainForm.InitHelpContexts;
begin
  HelpKeyword := 'Alignment_Explorer.htm';
  HelpContext := HC_Alignment_Builder;
  DataMenu.HelpContext := HC_Data_Menu_in_Alignment_Explorer;
  EditMenu.HelpContext := HC_Edit_Menu_in_Alignment_Explorer;
  SearchMenu.HelpContext := HC_Search_Menu_in_Alignment_Explorer;
  AlignmentMenu.HelpContext := HC_Alignment_Menu_in_Alignment_Explorer;
  WebMenu.HelpContext := HC_Web_Menu_in_Alignment_Explorer;
  SequencerMenu.HelpContext := HC_Sequencer_Menu_in_Alignment_Explorer;
  DisplayMenu.HelpContext := HC_Display_Menu_in_Alignment_Explorer;
end;

procedure TAlignEditMainForm.InitAlignGrid1;
begin
  SourceAlignGrid := TAlignGrid.Create(Self);
  SourceAlignGrid.InstanceName := 'AlignGrid1';
  CurAlignGrid := SourceAlignGrid;
  SourceAlignGrid.ScrollBars := ssNone;
  SourceAlignGrid.Parent := DataTabSheet;
  SourceAlignGrid.Align := alClient;
  SourceAlignGrid.DefaultDrawing := False;
  SourceAlignGrid.Cols.Add('Species/Abbrv', True);
  SourceAlignGrid.Cols.Add('Group Name', True);
  SourceAlignGrid.DefaultRowHeight := SourceAlignGrid.Canvas.TextHeight('W') + 4;
  SourceAlignGrid.AlignEnabled := True;
  SourceAlignGrid.EditEnabled := True;
  SourceAlignGrid.ForceAlignment := True;
  SourceAlignGrid.HighlightSearchBox := True;
  SourceAlignGrid.ShowBGColor := True;
  SourceAlignGrid.ShowColor := True;

  SourceAlignGrid.OnMouseMove   := AlignGridMouseMove;
  SourceAlignGrid.OnMouseDown   := AlignGridMouseDown;
  SourceAlignGrid.OnSelection := DoOnSelection;
  SourceAlignGrid.OnAfterSelection := DoAfterSelection;
  SourceAlignGrid.Cols.Col[GROUP_COLUMN].Visible := False;
  InitScrollBars1(True);
end;

procedure TAlignEditMainForm.InitScrollBars1(resetPosition: Boolean);
var
  p: Integer;
  aVisibleColCount: Integer = -1;
  aVisibleRowCount: Integer = -1;
  aPosition: Integer = -1;
begin
  aVisibleColCount := SourceAlignGrid.VisibleColCount;
  aVisibleRowCount := SourceAlignGrid.VisibleRowCount;
  //{$IFDEF DEBUG}
  //if Assigned(CurAlignGrid) then
  //  WriteToDevConsole(Format('InitSbar1 CAG.LeftCol=%d CAG.Col=%d CAG.Selection.Left=%d CAG.Selection.Right=%d Reset=%s', [CurAlignGrid.LeftCol, CurAlignGrid.Col, CurAlignGrid.Selection.Left, CurAlignGrid.Selection.Right, BoolToStr(resetPosition, True)]));
  //{$ENDIF}
  try
    if resetPosition then
      aPosition := 1
    else
      aPosition := HorizScrollbar.Position;
    SourceAlignGrid.Align := alNone;
    if SourceAlignGrid.ColCount > (aVisibleColCount + SourceAlignGrid.FixedCols) then
    begin
      HorizScrollbar.Visible := True;
      HorizScrollbar.SmallChange := 1;
      HorizScrollbar.LargeChange := max(aVisibleColCount, HorizScrollbar.SmallChange);
      if SourceAlignGrid.ColCount > aVisibleColCount then
        p := max(1, aVisibleColCount - 1)
      else
        p := 1;
      HorizScrollbar.SetParams(aPosition, 1, SourceAlignGrid.ColCount - SourceAlignGrid.FixedCols, max(1, p));
    end
    else
      HorizScrollbar.Visible := False;

    //{$IFDEF DEBUG}
    //WriteToDevConsole(Format('InitSbar1 hsbar.Pos=%d hsbar.Min=%d hsbar.Max=%d hsbar.PageSize=%d', [HorizScrollBar.Position, HorizScrollBar.Min, HorizScrollBar.Max, HorizScrollBar.PageSize]));
    //{$ENDIF}

    if resetPosition then
      aPosition := 1
    else
      aPosition := VertScrollbar.Position;
    if SourceAlignGrid.RowCount > (aVisibleRowCount + SourceAlignGrid.FixedRows) then
    begin
      VertScrollbar.Visible := True;
      VertScrollbar.SmallChange := 1;
      VertScrollbar.LargeChange := max(aVisibleRowCount, VertScrollbar.SmallChange);
      if SourceAlignGrid.RowCount > aVisibleRowCount then
        p := max(1, aVisibleRowCount - 1)
      else
        p := 1;
      VertScrollbar.SetParams(aPosition, 1, SourceAlignGrid.RowCount - SourceAlignGrid.FixedRows, max(1, p));
    end
    else
      VertScrollbar.Visible := False;
  finally
    SourceAlignGrid.Align := alClient;
  end;
end;

procedure TAlignEditMainForm.InitAlignGrid2;
begin
  TranslatedAlignGrid := TAlignGrid.Create(Self);
  TranslatedAlignGrid.InstanceName := 'AlignGrid2';
  TranslatedAlignGrid.ScrollBars := ssNone;
  TranslatedAlignGrid.Parent := ProteinTabSheet;
  TranslatedAlignGrid.Align := alClient;
  TranslatedAlignGrid.DefaultDrawing := False;
  TranslatedAlignGrid.ShowBGColor := True;
  TranslatedAlignGrid.ShowColor := True;
  TranslatedAlignGrid.FixSequenceOrder := True;
  TranslatedAlignGrid.Cols.Add('Species/Abbrv', True);
  TranslatedAlignGrid.Cols.Add('Group Name', True);
  TranslatedAlignGrid.DefaultRowHeight := TranslatedAlignGrid.Canvas.TextHeight('W') + 4;
  TranslatedAlignGrid.OnMouseMove   := AlignGridMouseMove;
  TranslatedAlignGrid.OnMouseDown   := AlignGridMouseDown;
  TranslatedAlignGrid.OnKeyDown := AlignGrid2KeyDown;
  TranslatedAlignGrid.OnColRowMoved := AlignGrid2RowMoved;
  TranslatedAlignGrid.Cols.Col[GROUP_COLUMN].Visible := False;
  TranslatedAlignGrid.SearchHighlightBgColor := clFuchsia;
  TranslatedAlignGrid.SearchHighlightFontColor := clWhite;
  TranslatedAlignGrid.HighlightSearchBox := True;
  InitScrollBars2(True);
end;

procedure TAlignEditMainForm.InitScrollbars2(resetPosition: Boolean);
var
  p: Integer;
  aVisibleColCount: Integer = -1;
  aVisibleRowCount: Integer = -1;
  aPosition: Integer = -1;
begin
  try
    aVisibleColCount := TranslatedAlignGrid.VisibleColCount;
    aVisibleRowCount := TranslatedAlignGrid.VisibleRowCount;
    TranslatedAlignGrid.Align := alNone;
    if resetPosition then
      aPosition := 1
    else
      aPosition := HorizScrollbar2.Position;
    if TranslatedAlignGrid.ColCount > (aVisibleColCount + TranslatedAlignGrid.FixedCols) then
    begin
      HorizScrollbar2.Visible := True;
      HorizScrollbar2.SmallChange := 1;
      HorizScrollbar2.LargeChange := max(aVisibleColCount, HorizScrollbar2.SmallChange);
      if TranslatedAlignGrid.ColCount > aVisibleColCount then
        p := max(1, TranslatedAlignGrid.VisibleColCount - 1)
      else
        p := 1;
      HorizScrollbar2.SetParams(aPosition, 1, TranslatedAlignGrid.ColCount - TranslatedAlignGrid.FixedCols, max(1, p));
    end
    else
      HorizScrollbar2.Visible := False;

    if resetPosition then
      aPosition := 1
    else
      aPosition := VertScrollbar2.Position;
    if TranslatedAlignGrid.RowCount > (aVisibleRowCount + TranslatedAlignGrid.FixedRows) then
    begin
      VertScrollbar2.Visible := True;
      VertScrollbar2.SmallChange := 1;
      VertScrollbar2.LargeChange := max(aVisibleRowCount, VertScrollbar2.SmallChange);
      if TranslatedAlignGrid.RowCount > aVisibleRowCount then
        p := max(1, TranslatedAlignGrid.VisibleRowCount - 1)
      else
        p := 1;
      VertScrollbar2.SetParams(aPosition, 1, TranslatedAlignGrid.RowCount - TranslatedAlignGrid.FixedRows, max(1, p));
    end
    else
      VertScrollbar2.Visible := False;
  finally
    TranslatedAlignGrid.Align := alClient;
  end;
end;

procedure TAlignEditMainForm.InitClustalW;
var
  filename: String = '';
begin
  ClustalW := TClustalW.Create(Self);
  {$IFDEF VISUAL_BUILD}
  filename := GetPrivateOutputFile(sfClustalOptions);
  if FileExists(filename) then
    if not ClustalW.LoadSessionProperties(filename) then
      ClustalW.SetDefaults;
  {$ENDIF}
  ClustalW.OnTerminate := ClustalWTerminate;
end;

procedure TAlignEditMainForm.PositionToolbars;
var
  CurLeftPos: Integer = 0;
begin
  if FileToolBar.Visible then
  begin
    FileToolbar.Left := CurLeftPos;
    inc(CurLeftPos, FileToolBar.Width + 1);
  end;
  if EditToolbar.Visible then
  begin
    EditToolbar.Left := CurLeftPos;
    inc(CurLeftPos, EditToolbar.Width + 1);
  end;
  if SearchToolbar.Visible then
    SearchToolbar.Left := CurLeftPos;
  Panel1.Invalidate;
end;

function TAlignEditMainForm.GetCurGeneticCode: String;
begin
  result := CurAlignGrid.CodeTable;
end;

function TAlignEditMainForm.GetCurGeneticCodeName: String;
begin
  result := CurAlignGrid.CodeTableName;
end;

function TAlignEditMainForm.GetMuscleOptions(var Options: TStringList; const DataType: TSnTokenCode): Boolean;
begin
  try
    if Assigned(FAppLinkOptionsManager) then
      FreeAndNil(FAppLinkOptionsManager); { important to free between every use or we sometimes have a problem where spin edit OnExit events fire and set values to zero}
    FAppLinkOptionsManager := TApplinkOptionsManager.Create;
    Result := FAppLinkOptionsManager.GetMuscleSettings(Options, DataType);
  except
    on E:Exception do
      ShowMessage('Application error when initializing MUSCLE options: ' + E.Message);
  end;
end;

function TAlignEditMainForm.FinalizeMuscleOptions(Options: TStringList; const DataType: TSnTokenCode): Boolean;
var
  temp: TSequenceList = nil;
  Col: Integer = -1;
  Row: Integer = -1;
  CodeTableName: String;
begin
  Result := False;
  if DataType = snCodingDna then
  begin
    if (MessageDlg('Remove Gaps', 'Would you like to remove gaps before alignment?', mtConfirmation, mbYesNo, 0) = mrYes) then
      if not TryRemoveGaps then
        Raise Exception.Create('MEGA was not able to remove gaps prior to cDNA alignment.');

    if Options.Values[opsGeneticCodeTable2] <> EmptyStr then
    begin
      CodeTableName := Options.Values[opsGeneticCodeTable2];
      SetGeneticCodeTableByName(CodeTableName);
      Options.Delete(Options.IndexOfName(opsGeneticCodeTable2));
    end
    else
      raise Exception.Create('Missing genetic code table');
    if not Translate(True, True) then
      Raise Exception.Create('Unable to translate DNA to Protein.');

    try
      Temp := TSequenceList.create;
      SourceAlignGrid.GetSeqData(temp);
      Temp.CodeTable := SourceAlignGrid.CodeTable;
      temp.Translate;
      if (MegaUtils.CheckStopCodons(Temp, Row, Col)) and (not CheckStopCodons) then
        Exit;
    finally
      if Assigned(Temp) then
        Temp.Free;
    end;
    CurAlignGrid := TranslatedAlignGrid;
  end
  else if DataType = snProtein then
  begin
    temp := TSequenceList.create;
    try
      CurAlignGrid.GetSeqData(temp);
      if (MegaUtils.CheckStopCodons(Temp, Row, Col)) and (not CheckStopCodons) then
        Exit;
    finally
      FreeAndNil(Temp);
    end;
  end;
  Result := True;
end;

procedure TAlignEditMainForm.BLASTSearch(query: String; isDNA: boolean);
var
  aChBrowser : TMegaBrowserFrm;
begin
  aChBrowser := CreateNewChromiumBrowserWindow(bmBrowser);
  aChBrowser.BLAST(query, isDNA);
end;

function TAlignEditMainForm.Translate(OnlySelected: boolean; IsAlignmentPrep: Boolean = False): Boolean;
var
  seqlist: TSequenceList = nil;
  x10: Integer = -1;
  x11: Integer = -1;
  y10: Integer = -1;
  y11: integer = -1;
  x20: Integer = -1;
  x21: Integer = -1;
  y20: Integer = -1;
  y21: integer = -1;
begin
  Result := False;
  if SourceAlignGrid.Empty then exit;

  if SourceAlignGrid.CodeTableName = '' then
    ActionGeneticCodeExecute(nil);
  try
    SourceAlignGrid.SelectedBlock(x10,y10,x11,y11);

    seqlist := TSequenceList.Create;
    if OnlySelected then
      SourceAlignGrid.GetSelectedData(seqlist)
    else
      SourceAlignGrid.GetAllData(seqlist);

    TranslatedAlignGrid.SetSeqData(seqlist);
    TranslatedAlignGrid.Translate;
    if OnlySelected and (not IsAlignmentPrep) then
      TranslatedAlignGrid.SelectBlock(1,0,1,0)
    else
    begin
      x20 := (x10+2) div 3;
      x21 := x11 div 3;
      y20 := y10;
      y21 := y11;
      TranslatedAlignGrid.SelectBlock(x20,y20,x21,y21);
    end;
    TranslatedAlignGrid.Font.Assign(SourceAlignGrid.Font);
    Result := True;
    TranslatedAlignGrid.EditEnabled := False;
    StatusBarLabel.Caption := 'Edit disabled for translated protein data.';
  finally
    FreeAndNil(seqlist);
  end;
end;

procedure TAlignEditMainForm.AlignGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const
  DRAG_INCREMENT = 5;
var
  ARow: Integer = -1;
  CurCol: Integer = 0;
  Plus5Col: Integer = 0;
  NewWidth, MaxResizeWidth: Integer;
begin

  if FIsResizing then
  begin
    NewWidth := X - CurAlignGrid.CellRect(FResizingCol, 0).Left;
    if NewWidth > 15 then // Can't have col 0 smaller than 15 pixels
    begin
      MaxResizeWidth := SourceAlignGrid.width-SourceAlignGrid.Canvas.TextWidth('W')-4-(GetSystemMetrics(SM_CXVSCROLL));
      if NewWidth > MaxResizeWidth then
        NewWidth := MaxResizeWidth;  // Makes or Max Resize Width to be smaller than the edge of the screen.
      CurAlignGrid.ColWidths[FResizingCol] := NewWidth;
    end;
  end;

  try
    CurAlignGrid.MouseToCell(X, Y, CurCol, ARow);
    if CurCol > CurAlignGrid.Cols.VisibleCount - 1 then
      Exit;

    CurAlignGrid.MouseToCell(X + 5, Y, Plus5Col, ARow);
  finally
    if Plus5Col > CurCol then
    begin
     CurAlignGrid.Cursor := crHSplit;
     CurAlignGrid.Options := CurAlignGrid.Options - [goRowMoving];
    end
    else
    begin
     CurAlignGrid.Cursor := crArrow;
     CurAlignGrid.Options := CurAlignGrid.Options + [goRowMoving];
    end;
  end;

  CurAlignGrid.MouseToCell(X, Y, curCol, aRow);
  if (aRow <> FLastRow) and (goRowMoving in CurAlignGrid.Options) then
  begin
    if ssLeft in Shift then
    begin
      if (aRow < FLastRow) and (VertScrollbar.Position > 0) then { dragging upward}
      begin
        if abs(aRow - VertScrollbar.Position) <= DRAG_INCREMENT then
          VertScrollbar.Position := max(0, VertScrollbar.Position - 2);
      end
      else if (aRow > FLastRow) and (VertScrollBar.Position < VertScrollBar.Max) then { dragging downward}
      begin
        if abs(aRow - VertScrollbar.Position - VertScrollBar.PageSize) <= DRAG_INCREMENT then
          VertScrollBar.Position := min(VertScrollBar.Max, VertScrollBar.Position + 2);
      end;
    end;
  end;
  FLastRow := aRow;
end;

procedure TAlignEditMainForm.AlignGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  x0,y0: Integer;
  ACol: Integer = -1;
  ARow:integer = -1;
  clientCoords: TPoint;
begin
  try
    CurAlignGrid.OnMouseUp := AlignGridMouseUp;  // No idea why this is here rather than assigned with the other events earlier.

    if CurAlignGrid.Cursor = crHSplit then  // Only start resizing, once the cursor is over the correct place and you get a click, this way you can't "slide" into it accidently.
      begin
        FIsResizing := true;
        CurAlignGrid.MouseToCell(X-5, Y, FResizingCol, ARow);
        CurAlignGrid.ClearSelection;
        Exit;
      end;

    CurAlignGrid.MouseToCell(X, Y, ACol, ARow);
    if (ARow = 0) and (Button = mbRight) then
      Exit;
    if (ARow = 0) and (Button = mbLeft) then
      if ACol < CurAlignGrid.Cols.Count then
        begin
          CurAlignGrid.Cols.Col[ACol].isDown := true;
          CurAlignGrid.Invalidate;
        end;

    with CurAlignGrid do
    begin
      if Empty then exit;
      MousePos.X := MouseCoord(X, Y).X;
      MousePos.Y := MouseCoord(X, Y).Y;
      x0 := MousePos.X;
      y0 := MousePos.Y;
      if (x0 = 0) and (y0 = 0) then exit;
      if y0 > NoOfSeqs then exit;
      if Button = mbRight then
      begin
        clientCoords.x := X + CurAlignGrid.Left + CurAlignGrid.DefaultColWidth div 3;
        clientCoords.y := Y {+ PageControl1.Top + (PageControl1.Height - PageControl1.ActivePage.Height)};
        clientCoords := ClientToScreen(clientCoords);
        if not Selected(x0, y0) then
          ClearSelection;
        if x0 < CurAlignGrid.Cols.Count then
          if not (Selected(x0,y0) and (GetNoOfSelectedSeqs > 1)) then
          begin
            Col := LeftCol;
            Row := y0;
            SelectSequence;
            PopupMenu5.Popup(clientCoords.X, clientCoords.Y);
          end
          else
            PopupMenu4.Popup(clientCoords.X, clientCoords.Y)
        else if y0 = 0 then
          PopupMenu6.Popup(clientCoords.X, clientCoords.Y)
        else if SequenceSelected then
          PopupMenu4.Popup(clientCoords.X, clientCoords.Y)
        else if SiteSelected then
          PopupMenu3.Popup(clientCoords.X, clientCoords.Y)
        else if BlockSelected then
          PopupMenu2.Popup(clientCoords.X, clientCoords.Y)
        else if x0 <= Sequence[y0-1].NoOfSites then
        begin
          Col := x0;
          Row := y0;
          PopupMenu1.Popup(clientCoords.X, clientCoords.Y);
        end;
      end;
    end;
  except
    on E:Exception do
      Assert(False, 'Error in AlignEditMainForm.AlignGridMouseDown' + E.Message);
  end;end;

procedure TAlignEditMainForm.AlignGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol: Integer = -1;
  ARow: Integer = -1;
begin
 if FIsResizing then
    FIsResizing := false;

  with SourceAlignGrid do
  begin
    MouseToCell(X, Y, ACol, ARow);
    if ARow = 0 then
      if IsFixedColumn(ACol) then
        begin
          if Cols.Col[ACol].isDown then
          begin
            if Cols.Col[ACol].SortOrder = soAscending then
              SortByName(soDescending)
            else
              SortByName(soAscending);
          end;
          Cols.Col[ACol].isDown := false;
          Invalidate;
        end;
    if CurAlignGrid.IsVisibleCell(ACol, ARow) and (ACol >= CurAlignGrid.FixedCols) then
      SetSiteSpinEditValue(ACol - CurAlignGrid.FixedCols + 1);
  end;
  inherited MouseUp(Button, Shift, X, Y);
  CurAlignGrid.Invalidate;
end;

procedure TAlignEditMainForm.AlignGrid2KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if TranslatedAlignGrid.AlignEnabled and ((Key = Ord('-')) or (Key = VK_SUBTRACT)) then exit;
end;

procedure TAlignEditMainForm.AlignGrid2RowMoved(Sender: TObject; IsColumn: Boolean; FromIndex, ToIndex: Integer);
begin
  TranslatedAlignGrid.Undo;
end;

procedure TAlignEditMainForm.ShrinkNameColToFit(percent: Integer);
begin
 if SourceAlignGrid.ColWidths[0] > (Self.Width * (percent / 100)) then    // If the length of the name is over ##% of the window width then make it's col smaller so that it doesn't take up the whole window.
   SourceAlignGrid.ColWidths[0] := Trunc(Self.Width * (percent / 100));
end;

procedure TAlignEditMainForm.ClustalWTerminate(Sender: TObject);
var
  i,j: integer;
  CurEditEnabled: boolean;
begin
  SetAlignmentInProgress(False);
  try
    if ClustalW.Done then
    begin
      CurEditEnabled := CurAlignGrid.EditEnabled;
      CurAlignGrid.EditEnabled := true;
      CurAlignGrid.Replace(ClustalW.Sequences);
      CurAlignGrid.EditEnabled := CurEditEnabled;

      if (TempPos.X > 0) and (TempPos.Y > 0) then
      begin
        i := 1;
        j := 1;
        while i <= TempPos.X do
        begin
          if CurAlignGrid.Sequence[TempPos.Y-1].Base[j] <> '-' then
            inc(i);
          inc(j);
        end;
        dec(j);
        CurAlignGrid.Col := j;
        CurAlignGrid.Row := TempPos.Y;
        CurAlignGrid.LeftCol := j-TempPos.X-TempGap+CurPos.X;
        CurAlignGrid.TopRow := CurPos.Y;
      end;
    end;
    Application.ProcessMessages;
    if ClustalW.RequestUntranslate then
      Untranslate;
    ShrinkNameColToFit;
    Visible := True;
    SetFocus;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred: ' + E.Message);
  end;
end;

procedure TAlignEditMainForm.MuscleThreadFinished(Thread: TObject);
var
  Msg: String = '';
  aLink: TMuscleLink = nil;
begin
  try
    try
      SetAlignmentInProgress(False);
      if not (Thread = FMuscleThread) then
        raise Exception.Create('Expected TMuscleThread but got ' + Thread.ClassName);
      FMuscleThread.UpdateProgressProc := nil;
      FMuscleThread.UpdateStatusProc := nil;
      FMuscleThread.NewLineProc := nil;
      FMuscleThread.NewLineCheckCancelFunc := nil;
      FMuscleThread.ShowAnalysisOptionsProc := nil;
      aLink := FMuscleThread.MuscleLink;
      aLink.MAI.ARP.UseTimer := False;
      aLink.MAI.ARP.Hide;
      if FMuscleThread.UserAborted then
        raise EAbort.Create('user cancelled');
      if not FMuscleThread.IsSuccess then
      begin
        Msg := FMuscleThread.MessagesLog.Text;
        ShowMessage('MUSCLE error. The alignment procedure exited abnormally: ' + Msg);
      end
      else
        case aLink.ErrorCode of
          noOutput: ShowMessage('Error-Alignment Failed: MUSCLE application did not produce any output!');
          notAbleToSave, UnknownErr, UnknownOption: ShowMessage('Error-Alignment Failed: An error has occurred during the Alignment Process.');
          notFinished: ShowMessage('Error-Alignment Failed: MUSCLE Log file did not end properly, suggesting an unhandled exception.');
          notEnoughMemory: ShowMessage('Error-Alignment Failed: Computer reported lack of enough memory (RAM). Try freeing up memory by closing other programs and starting over.');
          notEnoughMemorySpecified: ShowMessage('Alignment failed. MUSCLE needs more memory than you specified. You can try to re-run the analysis and specify a higher value for max memory (MB)');
          noLogFile: ShowMessage('Error-Alignment Failed: No MUSCLE alignment log file was found.');
          FatalException: ShowMessage('Error-Alignment Failed: MUSCLE has encountered a Fatal Exception.');
          noError:
            begin
              if not aLink.MAI.MyProcessPack.ContainsProcessType(ppPhyloQ) then
              begin
                aLink.MAI.ARP.UpdateRunStatusInfo('Status', 'Reading in MUSCLE Results...');
                aLink.MAI.ARP.SetProgress(10); // if set to zero, nothing will be reported
              end;
              ReplaceSelectedFromFasta(aLink.OutputFilename, True, aLink.GetXSites, aLink.SeqList);
              if aLink.IscDNA then
                UnTranslate;
            end;
        end;
      BringToFront;
    except
      on E:EAbort do
        ShowMessage('MUSCLE alignment cancelled');
      on E:Exception do
        ShowMessage('Application error finalizing MUSCLE alignment: ' + E.Message);
    end;
  finally
    if DirectoryExists(FCurrentDirectory) then
      SetCurrentDir(FCurrentDirectory);
    SetAlignmentInProgress(False);
    FMuscleThread := nil;
    if FileExists(aLink.OutputFilename) then
      DeleteFile(aLink.OutputFilename);
    if FileExists(aLink.InputFilename) then
      DeleteFile(aLink.InputFilename);
    if FileExists(aLink.LogFilename) then
      DeleteFile(aLink.LogFilename);
  end;
end;

procedure TAlignEditMainForm.LoaderDone(Sender: TObject);
var
  ASeqList: TSequenceList;
  msg: String;
begin
  ASeqList := nil;
  try try
    ASeqList := FLoader.CloneSequenceList;
    ASeqList.IsProteinCoding := true;       // for default setting
    if ASeqList.IsDNA and (not IsSessionTest) then
      ASeqList.IsProteinCoding := MessageDlg('Protein-coding nucleotide sequence data?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
    if FOverwrite then
      SourceAlignGrid.SetSeqData(ASeqList)
    else if ASeqList.IsDNA = SourceAlignGrid.IsDNA then
      SourceAlignGrid.InsertSequenceList(ASeqList)
    else
    begin
      msg := Format('Incompatible data type. Grid expects %s data but file has %s data', [SourceAlignGrid.DataTypeString, ASeqList.DataTypeString]);
      if MessageDlg(msg, mtWarning, [mbCancel, mbIgnore], 0) = mrIgnore then
        SourceAlignGrid.InsertSequenceList(ASeqList);
    end;
    CurAlignGrid.Cols.Col[GROUP_COLUMN].Visible := (ASeqList.HasGroupNames);
    InitForm;
    PageControl1.ActivePageIndex := SOURCE_DATA_PAGE;
    PageControl1.Show;
    ShrinkNameColToFit;
    SourceAlignGrid.Invalidate;
    SourceAlignGrid.SetFocus;
    BringToFront;
    //ReopenMenuMRU.Add(filename);
  except
    On E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
  finally
    if Assigned(ASeqList) then
      ASeqList.Free;
    if Assigned(FLoader) then
      FreeAndNil(FLoader);
  end;
end;

procedure TAlignEditMainForm.SetAlignmentInProgress(aValue: Boolean);
var
  tempCurAlignGrid: TAlignGrid = nil;
begin
  tempCurAlignGrid := CurAlignGrid;
  SourceAlignGrid.Enabled := (not aValue);
  TranslatedAlignGrid.Enabled := (not aValue);
  FileToolBar.Enabled := (not aValue);
  EditToolbar.Enabled := (not aValue);
  SearchToolbar.Enabled := (not aValue);
  if aValue then
  begin
    FileToolBar.Images := ImageForm.DisabledIcons;
    EditToolbar.Images := ImageForm.DisabledIcons;
    SearchToolbar.Images := ImageForm.DisabledIcons;
    MainMenu1.Images := ImageForm.DisabledIcons;
  end
  else
  begin
    FileToolBar.Images := ImageForm.DefaultIcons;
    EditToolbar.Images := ImageForm.DefaultIcons;
    SearchToolbar.Images := ImageForm.DefaultIcons;
    MainMenu1.Images := ImageForm.DefaultIcons;
  end;
  FileToolBar.Invalidate;
  FAlignmentIsInProgress := aValue;
  if aValue then
    ActionList1.State := asSuspended
  else
    ActionList1.State := asNormal;
  if tempCurAlignGrid <> CurAlignGrid then
    CurAlignGrid := tempCurAlignGrid; { somehow this gets changed some times}
end;

procedure TAlignEditMainForm.LaunchFastaLoadingThread(filename: String;overwrite: Boolean);
var
  t: TRawFastaLoaderThread = nil;
  progress: TRuntimeProgress = nil;
begin
  try
    progress := TRuntimeProgress.Create;
    t := TRawFastaLoaderThread.Create(filename, progress, overwrite, '-', '.', '?');
    progress.Show;
    progress.SetKeepOnTop(True);
    progress.IsMarqueeMode := True;
    t.OnTerminate := FastaLoadingThreadDone;
    t.Start;
  except
    on E:Exception do
    begin
      ShowMessage('Application Error: ' + E.Message);
      if Assigned(progress) then
        progress.Free;
    end;
  end;
end;

procedure TAlignEditMainForm.FastaLoadingThreadDone(Thread: TObject);
var
  t: TRawFastaLoaderThread = nil;
  SeqList: TSequenceList = nil;
  mr: Integer;
begin
    if not (Thread is TRawFastaLoaderThread) then
      raise Exception.Create(Format('Expected TRawFastaLoaderThread but got "%s"', [Thread.ClassName]));
  t := TRawFastaLoaderThread(Thread);
  try
    try
      if t.UserCancelled then
        ShowMessage('Loading of FASTA file was cancelled')
      else
      begin
        if not t.IsSuccess then
          raise Exception.Create('failed to load fasta file: ' + t.LogText);
        CurAlignGrid.HeaderString := t.HeaderStr;
        SeqList := t.Sequences;
        if SeqList.Count = 0 then
          raise Exception.Create('no sequences found in FASTA file');

        SourceAlignGrid.Enabled := false;

        if t.Overwrite then
        begin
          SourceAlignGrid.TransferSeqData(SeqList);
          SourceAlignGrid.IsDNA := t.IsLikelyDna;
        end
        else if SourceAlignGrid.IsDNA <> t.IsLikelyDna then
        begin
          if SourceAlignGrid.IsDNA then
            mr := MessageDlg('You may be inserting protein sequences in DNA data. Are you sure you wish to continue?', mtWarning, [mbCancel, mbIgnore], 0)
          else
            mr := MessageDlg('You may be inserting DNA sequences in protein data. Are you sure you wish to continue?', mtWarning, [mbCancel, mbIgnore], 0);

          if mr = mrCancel then
            exit;

          SourceAlignGrid.InsertSequenceList(SeqList);
        end
        else
          SourceAlignGrid.InsertSequenceList(SeqList);

        SourceAlignGrid.Enabled := true;
        CurAlignGrid.Cols.Col[GROUP_COLUMN].Visible := False; { group names are only supported by .meg and .mas files}
        LoadAlignmentFileFinished(t.FileName, t.Progress);
      end;
    except
      on E:Exception do
      begin
        ShowMessage('Application Error: ' + E.Message);
      end;
    end;
  finally
    if Assigned(t.progress) then
       t.progress.Free;
  end;
end;

procedure TAlignEditMainForm.UpdateUsageStatistics(usageData: String);
var
  usageStats: TStatisticsReporter = nil;
begin
  try
    try
      {$IFDEF DEBUG}
      WriteToDevConsole(usageData);
      {$ENDIF}
      usageStats := TStatisticsReporter.Create(True);
      usageStats.UsageStatistics := usageData;
      usageStats.Start;
      usageStats := nil;
    Except
      on E:Exception do
      begin
        Assert(False, 'Error saving usage statistics: ' + E.Message);
        // do nothing
      end;
    end;
  finally
    if UsageStats <> nil then
      UsageStats.Free;
  end;
end;

function TAlignEditMainForm.GetUsageDataAsJsonString(method: String; options: TStringList = nil; dataType: TSnTokenCode = snNoToken): String;
begin
  if method = 'clustal' then
    Result := GetUsageDataAsJsonStringClustal
  else if method = 'muscle' then
  begin
    if not Assigned(options) then
      raise Exception.Create('missing options for MUSCLE sequence alignment');
    Result := GetUsageDataAsJsonStringMuscle(options, dataType);
  end
  else
    raise Exception.Create('missing handler for alignment method: ' + method);
end;

function TAlignEditMainForm.GetUsageDataAsJsonStringClustal: String;
var
  json: TJSONObject = nil;
  technicalInfo: TJSONObject = nil;
  settingsInfo: TJSONObject = nil;
  generalInfo: TJSONObject = nil;
  dStr: String = '';
begin
  try
    json := TJSONObject.Create;
    technicalInfo := GetTechnicalInfoJson;
    json.Add(TECHNICAL_INFO, technicalInfo);

    generalInfo := TJSONObject.Create;
    if ClustalW.RequestUntranslate then
      dStr := 'translated codons'
    else if CurAlignGrid.IsDNA then
      dStr := 'dna'
    else
      dStr := 'amino acid';
    generalInfo.Add(DATA_TYPE_STR, dStr);
    generalInfo.Add(NUM_TAXA_STR, CurAlignGrid.NoOfSeqs);
    generalInfo.Add(NUM_SITES_STR, CurAlignGrid.MaxNoOfSites);
    json.Add(GENERAL_INFO, generalInfo);

    settingsInfo := ClustalW.GetSettingsAsJson;
    json.Add(ANALYSIS_SETTINGS, settingsInfo);

    {$IFDEF DEBUG}
    Result := json.FormatJSON([foUseTabchar, foSkipWhiteSpaceOnlyLeading]);
    {$ELSE}
    Result := json.AsJSON;
    {$ENDIF}
  finally
    if Assigned(json) then
      json.Free;
  end;
end;

function TAlignEditMainForm.GetUsageDataAsJsonStringMuscle(muscleOptions: TStringList; dataType: TSnTokenCode): String;
var
  json: TJSONObject = nil;
  technicalInfo: TJSONObject = nil;
  settingsInfo: TJSONObject = nil;
  generalInfo: TJSONObject = nil;
  dStr: String = '';
begin
  try
    json := TJSONObject.Create;
    technicalInfo := GetTechnicalInfoJson;
    json.Add(TECHNICAL_INFO, technicalInfo);

    generalInfo := TJSONObject.Create;
    if (dataType = snCoding) or (dataType = snCodingDna) then
      dStr := 'translated codons'
    else if dataType = snNucleotide then
      dStr := 'dna'
    else if dataType = snProtein then
      dStr := 'amino acid'
    else
      raise Exception.Create('invalid data type specified for MUSCLE alignment');

    generalInfo.Add(DATA_TYPE_STR, dStr);
    generalInfo.Add(NUM_TAXA_STR, CurAlignGrid.NoOfSeqs);
    generalInfo.Add(NUM_SITES_STR, CurAlignGrid.MaxNoOfSites);
    json.Add(GENERAL_INFO, generalInfo);

    settingsInfo := MuscleOptionsToJson(muscleOptions);
    json.Add(ANALYSIS_SETTINGS, settingsInfo);

    {$IFDEF DEBUG}
    Result := json.FormatJSON([foUseTabchar, foSkipWhiteSpaceOnlyLeading]);
    {$ELSE}
    Result := json.AsJSON;
    {$ENDIF}
  finally
    if Assigned(json) then
      json.Free;
  end;
end;

function TAlignEditMainForm.SetGeneticCodeTableByName(aName: String): Boolean;
var
  MyDlg : TSelectGeneticCodeDlg;
begin
 Result := False;
  MyDlg := nil;
  try
    MyDlg := TSelectGeneticCodeDlg.Create(Self);
    MyDlg.CodeTableName := aName;
    // this is important as the user may have changed the codetable itself
    SourceAlignGrid.CodeTableName := MyDlg.CodeTableName;
    SourceAlignGrid.CodeTable     := MyDlg.CodeTable;
    TranslatedAlignGrid.CodeTableName := SourceAlignGrid.CodeTableName;
    TranslatedAlignGrid.CodeTable     := SourceAlignGrid.CodeTable;
    Result := True;
  finally
    FreeAndNil(MyDlg);
  end;
end;

procedure TAlignEditMainForm.ExportGeneticCodeTable(ExportTypeStr: String; SaveLocation: String);
var
  Ch : AnsiChar;
  i, j, k, Index: Integer;
  AStr: AnsiString;
  TheLines: TStringList;
  CodonName: array[0..4] of AnsiChar;
  ViewXls: TExcelWrite;
  BaseName: array[0..3] of Char;
  ExportType: TExportType;
begin
  TheLines := nil;
  ViewXls := nil;
  BaseName[0] := 'U';
  BaseName[1] := 'C';
  BaseName[2] := 'A';
  BaseName[3] := 'G';
  try
    TheLines := TStringList.Create;
    ViewXls := TExcelWrite.Create(Self, 'Code Table '+ SourceAlignGrid.CodeTableName);
    if (ExportTypeStr = 'EXexcelDisp') then
       ExportType := EXexcelDisp
    else if (ExportTypeStr = 'EXexcelSave') then
       ExportType := EXexcelSave
    else if (ExportTypeStr = 'EXcsvSave') then
       ExportType := EXcsvSave
    else if (ExportTypeStr = 'EXtext') then
       ExportType := EXtext
    else
       ExportType := EXnone;
    ViewXls.IsXLS := (ExportType = EXexcelDisp) or (ExportType = EXexcelSave) or (ExportType = EXcsvSave);
    AStr := 'Code Table: '+ SourceAlignGrid.CodeTableName;
    ViewXls.WriteLine(AStr);
    TheLines.Add(AStr);
    CodonName[3] := #0;
    TheLines.Add(' +----------+----------+----------+----------+');
    for i := 0 to 3  do
    begin
      CodonName[0] := BaseName[i];
      for j := 0 to 3 do
      begin
        AStr := ' | ';
        CodonName[2] := BaseName[j];
        for k := 0 to 3 do
        begin
          CodonName[1] := BaseName[k];
          Index := i*16+ k*4 + j;
          Ch := SourceAlignGrid.CodeTable[Index+1];
          AStr := AStr + CodonName+'  ' + GetThreeLetterCode(Ch)+' | ';
          ViewXls.Add(CodonName+'  ' + GetThreeLetterCode(Ch));
        end;
        TheLines.Add(AStr);
        ViewXls.WriteLine();
      end;
      TheLines.Add(' +----------+----------+----------+----------+');
    end;
  except
    On E:Exception do
    begin
      ShowMessage(E.Message);
      if TheLines <> nil then
        TheLines.Free;
      TheLines := nil;
      Exit;
    end;
  end;
  try try
    SaveLocation := ExcelWrite.GetSaveLocation;
    if ExportType = EXexcelDisp then
    begin
      ViewXls.SaveFile(SaveLocation, ExportExcel, False);
      RunAProgram(SaveLocation);
    end
    else if ExportType = EXexcelSave then
      ViewXls.SaveFile(SaveLocation, ExportExcel, False)
    else if ExportType = EXcsvSave then
      ViewXls.SaveFile(SaveLocation, ExportCSV, False)
    else
     OpenStringList(TheLines, 'Code Table: '+ SourceAlignGrid.CodeTableName);
  except
    On E: Exception do
      ShowMessage(E.message);
  end;
  finally
    TheLines.Free;
    ViewXls.Free;
  end;
end;

procedure TAlignEditMainForm.ExportGeneticCodeStatistics(ExportTypeStr: String; SaveLocation: String);
var
  MyStrList : TStringList;
  MyCodonInfo: TCodonInfo;
  StatXls: TExcelWrite;
  ExportType: TExportType;
begin
  StatXls := nil;
  MyCodonInfo := nil;
  MyStrList   := nil;
  if (ExportTypeStr = 'EXexcelDisp') then
     ExportType := EXexcelDisp
  else if (ExportTypeStr = 'EXexcelSave') then
     ExportType := EXexcelSave
  else if (ExportTypeStr = 'EXcsvSave') then
     ExportType := EXcsvSave
  else if (ExportTypeStr = 'EXtext') then
     ExportType := EXtext
  else
     ExportType := EXnone;
  try try
    StatXls := TExcelWrite.Create(self, 'Syn Nonsyn sites');
    StatXls.IsXLS := (ExportType = EXexcelDisp) or (ExportType = EXexcelSave) or (ExportType = EXcsvSave);
    MyCodonInfo := TCodonInfo.Create;
    MyCodonInfo.TsTvRatio := 1;
    MyCodonInfo.CodeTable := SourceAlignGrid.CodeTable;
    MyCodonInfo.GenerateSynSiteTable;
    MyStrList := TStringList.Create;
    begin
      MyStrList. Add('Code Table: '+ SourceAlignGrid.CodeTableName);
      StatXls.Add('Code Table: ' + SourceAlignGrid.CodeTableName);
      StatXls.WriteLine(0, 'A', '', True);
      MyStrList.Add('Method:     '+'Nei-Gojobori (1986) methodolgy');

      StatXls.Add('Method:  Nei-Gojobori (1986) methodology');
      StatXls.WriteLine(0, 'A', '', True);
    end;
    MyCodonInfo.WriteIntrinsicTables(MyStrList, True, False);
    MyCodonInfo.WriteIntrinsicTablesExcel(StatXls, True, False);
    if Assigned(MyCodonInfo) then
      MyCodonInfo.Free;
    MyCodonInfo := nil;
    StatXls.MergeCells(Rect(1, 5, 2, 5), aCenter, aCenter);
    StatXls.MergeCells(Rect(3, 5, 5, 5), aCenter, aCenter);
    SaveLocation := ExcelWrite.GetSaveLocation;
    if ExportType = EXexcelDisp then
    begin
      StatXls.SaveFile(SaveLocation, ExportExcel, False);
      RunAProgram(SaveLocation);
    end
    else if ExportType = EXexcelSave then
      StatXls.SaveFile(SaveLocation, ExportExcel, False)
    else if ExportType = EXcsvSave then
      StatXls.SaveFile(SaveLocation, ExportCSV, False)
    else
      OpenStringList(MyStrList, 'Syn/Nonsyn sites');
  except
    On E: Exception do ShowMessage(E.Message);
  end
  finally
    if StatXls <> nil     then StatXls.Free;
    if MyStrList <> nil   then MyStrList.Free;
    if MyCodonInfo <> nil then MyCodonInfo.Free;
  end;
end;

function TAlignEditMainForm.TryTranslate(OnlySelected: Boolean; IsAlignmentPrep: Boolean = False): Boolean;
begin
 Result := False;
 if CurAlignGrid.IsProteinCoding and CurAlignGrid.IsDNA and (PageControl1.PageCount > 1) then
 begin
   Translate(True, IsAlignmentPrep);
   CurAlignGrid := TranslatedAlignGrid;
 end
 else
   Exit;
 Result := True;
end;

function TAlignEditMainForm.CheckStopCodons: boolean;
var
  i, j: Integer;
begin
  Result := False;
  if (not TranslatedAlignGrid.IsDNA) then
  begin
    for i := 0 to TranslatedAlignGrid.NoOfSeqs-1 do
    begin
      for j := 1 to Length(TranslatedAlignGrid.Sequence[i].SeqData)-2 do // I'm checking the length -2 because we don't need to check the very last AA.  This is because it IS ALLOWED to be a stop codon '*'.  We just don't want any AA after the * in the current sequence.
        if TranslatedAlignGrid.Sequence[i].SeqData[j]  = '*' then
        begin
          if (MessageDlg('Stop Codon(s) are found in the translated sequences. Please select a correct Genetic Code or coding frame.', mtWarning, [mbAbort, mbIgnore], 0) = mrAbort) then
          begin
            TranslatedAlignGrid.Row := i+1;
            TranslatedAlignGrid.Col := j;
            Exit;
          end
          else
          begin
            Result := true;
            Exit;
          end;
        end;
    end;
  end;
  Result := True;
end;

function TAlignEditMainForm.TryRemoveGaps: Boolean;
var
  data: TSequenceList = nil;
  extraData: TSequenceList;
  replaceList: TStringList;
  curSeq: AnsiString;
  i, LenCurSeq, x0, y0, y1, MaxNoOfSites: Integer;
begin
  Result := False;
  if CurAlignGrid.IsProteinCoding and (PageControl1.PageCount > 1) then
  begin
    try
      // 1. Move the currently selected block into memory for manipulation
      data := TSequenceList.Create;
      extraData := TSequenceList.Create;
      SourceAlignGrid.GetSelectedData(data);
      SourceAlignGrid.GetSelectedData(extraData);
      for i := 0 to data.count-1 do
        extraData.Items[i].SeqData := '';
      x0 := SourceAlignGrid.Selection.Left;
      y0 := SourceAlignGrid.Selection.Top-1; // 1 fixed row
      y1 := SourceAlignGrid.Selection.Bottom; // 1 fixed row

      // 2. Walk through the block in memory and remove all gaps.
      for i := 0 to data.count-1 do
      begin
        curSeq := data.Items[i].SeqData;
        while Pos('-', curSeq) <> 0 do
          Delete(curSeq, Pos('-', curSeq), 1);

        data.Items[i].SeqData := curSeq;
        if Length(curSeq) = 0 then
          Raise Exception.Create('Could not remove gaps.  Sequence #' + IntToStr(i+1) + ' contains only gaps.');
      end;

      // 3. Remove 0,1 or 2 sites from the end of some sequences in the block to make the block all triplets
      for i:=0 to data.Count-1 do
      begin
        LenCurSeq := Length(data.Items[i].SeqData);
        MaxNoOfSites := 0;
        if LenCurSeq > MaxNoOfSites then
          MaxNoOfSites := LenCurSeq;
        if LenCurSeq mod 3 <> 0 then
        begin
          extraData.Items[i].SeqData := Copy(data.Items[i].SeqData, LenCurSeq-(LenCurSeq mod 3)+1, LenCurSeq mod 3);
          data.Items[i].SeqData := Copy(data.Items[i].SeqData, 1, LenCurSeq-(LenCurSeq mod 3));
        end;
      end;

      // 4. Pad empty space with gaps
      for i := 0 to data.count-1 do
      begin
        while Length(data.Items[i].SeqData) < MaxNoOfSites do // pad seqs to be all the same length
          data.Items[i].SeqData := data.Items[i].SeqData + '-';
        while Length(extraData.Items[i].SeqData) < extraData.MaxNoOfSites do // pad seqs to be all the same length
          extraData.Items[i].SeqData := extraData.Items[i].SeqData + '-';
      end;

      // 5. Replace the selected section in the DNA part of the Alignment Explorer with the block from memory.
      replaceList := TStringList.Create;
      try
        for i := 0 to data.count-1 do
          replaceList.Add(data.Items[i].SeqData + extraData.Items[i].SeqData);
        SourceAlignGrid.Replace(replaceList);
      finally
        replaceList.Free;
      end;

      // 6. Adjust the selection to only encompass the triplets with no gaps.
      SourceAlignGrid.Select(x0, y0, x0+data.MaxNoOfSites-extraData.MaxNoOfSites, y1);
      Result := True;
    finally
      if Assigned(data) then
        data.Free;
      if Assigned(extraData) then
        extraData.Free;
    end;
  end;
end;

procedure TAlignEditMainForm.UnTranslate;
begin
  if not Enabled then
    Enabled := True;
  if not Visible then
    Show;
  PageControl1.ActivePageIndex := 0;
  CurAlignGrid := SourceAlignGrid;
  if CurAlignGrid.EditEnabled then
    StatusBarLabel.Caption := 'Editing enabled'
  else
    StatusBarLabel.Caption := 'Editing disabled';
  DataTabSheetShow(Self);
end;

procedure TAlignEditMainForm.InitForm;
begin
 if SourceAlignGrid.IsDNA then
   DataTabSheet.Caption := DNA_SEQUENCES
 else
   DataTabSheet.Caption := PROTEIN_SEQUENCES;
 Show;
 ProteinTabSheet.TabVisible := SourceAlignGrid.IsDNA and SourceAlignGrid.IsProteinCoding;
 SourceAlignGrid.ResetSize;
 InitScrollBars1(False);
 TranslatedAlignGrid.ResetSize;
 InitScrollBars2(False);
 PageControl1.ActivePageIndex := SOURCE_DATA_PAGE;
 PageControl1.Show;
 ActionList1.State := asNormal;
end;

procedure TAlignEditMainForm.ReplaceSelectedFromFasta(FileName: String;StandardNames: Boolean; XSites: String; seq: TSequenceList; Progress: TRuntimeProgress = nil);
var
  TempFastaList, ResultList, NameOrder: TStringList;
  ResultStr, ToAdd, InsertName, str: AnsiString;
  i, j, SeqNum: Integer;
  CurEditEnabled: boolean;
  LocOfSeqI: Integer;
  aMsg: String;
begin
    TempFastaList := TStringList.Create;
    TempFastaList.LoadFromFile(FileName);
    ResultList := TStringList.Create;
    NameOrder := TStringList.Create;
    NameOrder.CaseSensitive := True; // Needed since case sensitive duplicates ARE allowed in Alignment explorer (but nowhere else in MEGA)
    ResultStr := TempFastaList.Text;

    Delete(ResultStr, 1, pos('>', ResultStr)-1); // Delete any junk before the first >
    While ResultStr <> '' do
    begin
      InsertName := Copy(ResultStr, pos('>', ResultStr)+1, pos(LineEnding, ResultStr)-2);  // Grabs from '>' to EOL
      SeqNum := StrToInt(Copy(InsertName, 3, Length(InsertName)));  // Turns TX1 into 1, etc.
      if (SeqNum < seq.Count) and (SeqNum >= 0)  then
        InsertName := seq[SeqNum].SeqName; // Get the real taxa name, number corelates to the index in seq.
      NameOrder.Add(InsertName);  // Add the real taxa name to NameOrder in the order it's listed in the TempFastaList file.
      Delete(ResultStr, pos('>', ResultStr), pos(#10, ResultStr)); // Delete the name we just processed
      if pos('>', ResultStr) = 0  then // if we've reached the last taxa, as there are no more '>' to denote the starting of a new name left
      begin
        ToAdd := Copy(ResultStr, 1, Length(ResultStr));
        Delete(ResultStr, 1, Length(ResultStr));
      end
      else
      begin
        ToAdd := Copy(ResultStr, 1, pos('>', ResultStr)-1); // Copy off the sequence (everything until the next '>')
        Delete(ResultStr, 1, pos('>', ResultStr)-1); // Delete the sequence (everything we just copied to ToAdd)
      end;
      ToAdd := StringReplace(ToAdd, LineEnding, '', [rfReplaceAll]);
      ResultList.Add(ToAdd);
      //ResultList.Add(ReplaceRegExpr(#$D#$A, ToAdd, '', False)); // Add the sequence to the List
    end;
   // now we need to check if there are any taxa missing from the result, for example any taxa which were only gaps will not get output.
    if NameOrder.Count <> Seq.Count then
    begin
      aMsg := 'Warning: The number of taxa which MUSCLE returned differs from the number of selected taxa in MEGA.' + #10#13 + 'This can be happen if for example one sequence was completely empty, or MUSCLE eliminated it for some reason.';
      // Compare out Names to the selected taxa and see WHERE we are missing taxa.

      // The previous strategy assumes that the sequences will be returned in order.  This was fine until we were forced to remove the -stable flag due to a known error in muscle when it is used.
      for i := 0 to seq.Count-1 do
      begin
        if NameOrder.IndexOf(seq[i].SeqName) = -1 then
        begin
          // In this case we have a name in the original unaligned sequence list which isn't in the aligned version.
          aMsg := 'The selection for sequence named ''' + Seq.Items[i].Seqname + ''' was removed from the final MUSCLE alignment, because it was empty or contained only gaps.  MEGA added the sequence back in for you, but you may want to remove it if it''s not necessary.';
          ResultList.Insert(i, StringOfChar('-', length(Seq.Items[i].SeqData) ));
          NameOrder.Insert(i, Seq.Items[i].SeqName);
        end;
      end;
      if Assigned(Progress) then
        Progress.Hide;
      ShowMessage(aMsg);
    end;

    // Since the -stable flag was depreciated we have to shuffle the sequences back into their origional order.  This is necessary for the XSites as well as not confusing the user.
    for i := 0 to ResultList.Count-1 do
    begin
      LocOfSeqI := NameOrder.IndexOf(Seq[i].SeqName);
      NameOrder.Move(LocOfSeqI, i);
      ResultList.Move(LocOfSeqI, i);
    end;

    // Before we can reaplace the aligned sequences we need to replace 'X' with their specific char we saved before alignment
    if XSites <> EmptyStr then
      for i := 0 to ResultList.Count-1 do
      begin
        str := ResultList.Strings[i];
        for j := 1 to Length(str) do
          if (CurAlignGrid.IsDNA and (str[j] = 'N')) or ((not CurAlignGrid.IsDNA) and (str[j] = 'X')) then
          begin
            str[j] := XSites[1];
            Delete(XSites, 1, 1);
          end;
        ResultList.Strings[i] := str;
      end;

    //AppLink.MAI.ARP.Hide;
    CurEditEnabled := CurAlignGrid.EditEnabled;
    CurAlignGrid.EditEnabled := true;
    CurAlignGrid.Replace(ResultList);
    CurAlignGrid.EditEnabled := CurEditEnabled;
    CurAlignGrid.Enabled := True;
    CurAlignGrid.Invalidate;
end;

function TAlignEditMainForm.OpenAMEGA2File(AFileName: String; overwrite: boolean): Boolean;
var
  ASeqList: TSequenceList = nil;
  msg: String;
begin
  FOverwrite := Overwrite;
  ASeqList := nil;
  try
    try
      if Assigned(FLoader) then
        FreeAndNil(FLoader);
      FLoader := TMegaAlignmentFileLoader.Create(AFilename);
      Result := FLoader.LoadFile;
      if Result then
      begin
        ASeqList := FLoader.CloneSequenceList;
        ASeqList.IsProteinCoding := true;       // for default setting
        if ASeqList.IsDNA and (not IsSessionTest) then
          ASeqList.IsProteinCoding := MessageDlg('Protein-coding nucleotide sequence data?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
        if overwrite then
          SourceAlignGrid.SetSeqData(ASeqList)
        else if ASeqList.IsDNA = SourceAlignGrid.IsDNA then
          SourceAlignGrid.InsertSequenceList(ASeqList)
        else
        begin
          msg := Format('Incompatible data type. Grid expects %s data but file has %s data', [SourceAlignGrid.DataTypeString, ASeqList.DataTypeString]);
          if MessageDlg(msg, mtWarning, [mbCancel, mbIgnore], 0) = mrIgnore then
            SourceAlignGrid.InsertSequenceList(ASeqList);
        end;
        SourceAlignGrid.Cols.Col[GROUP_COLUMN].Visible := (ASeqList.HasGroupNames);
        SourceAlignGrid.Invalidate;
      end
      else
      begin
        FLoader.OnLoadFileDone := LoaderDone;
        SessionFileName := ChangeFileExt(aFilename, '.mas');
        Caption := ConvertToMegaWinCaption('Alignment Explorer ('+ExtractFileName(aFilename)+')');
      end;
    except
      On E:Exception do
      begin
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
        Result := False;
      end;
    end;
  finally
    if Assigned(ASeqList) then
      ASeqList.Free;
    if Result and Assigned(FLoader) then
      FreeAndNil(FLoader);
  end;
end;

function TAlignEditMainForm.OpenAMDSXFile(AFileName: String; overwrite: boolean): Boolean;
begin
  Result := False;
  ShowMessage('Opening .mdsx files for the Alignment Explorer is not supported. Please open it from the main form (Data->Open a File/Session)');
end;

function TAlignEditMainForm.LoadThirdPartyFileFormat(InFile: String; overwrite: boolean): Boolean;
var
  Converter : TFormatConvertToMega = nil;
  Seq : TSequence = nil;
  TempSeq: AnsiString = '';
  SeqList : TSequenceList = nil;
  isDNA: boolean;
  mr: word;
  i: Integer;
begin
  Result := False;
  try
    try
    Converter := TFormatConvertToMega.Create(InFile);
    Converter.UpdateFileType(False);
    if Converter.InFileType = gm_FASTA then
      LaunchFastaLoadingThread(InFile, overwrite)
    else
    begin
      Converter.DoLoadInputData;
      Converter.Convert(Converter.InFileType);
      SeqList := TSequenceList.Create;
      if Converter.SeqDataHash.Count <> 0 then
      begin
        with Converter do
          for i := 0 to SeqDataHash.Count - 1 do
          begin
            Seq := TSequence.Create;
            Seq.SeqName := SeqDataHash.Names[i];
            TempSeq := Copy(SeqDataHash.Strings[i], Pos('=', SeqDataHash.Strings[i])+1, Length(SeqDataHash.Strings[i]));  // Fix for bug # 0002672 2/10/11 by Dan
            // Removes any whitespace which was left over after the conversion, fixes a bug we encountered.
            while pos(' ', TempSeq) <> 0 do
            begin
              delete(TempSeq, pos(' ', TempSeq), 1);
            end;
            Seq.SeqData := TempSeq;
            SeqList.Add(Seq);
            Seq := nil;
          end;

        if SeqList.Count = 0 then
          exit;

        isDNA := SeqList.LooksLikeDna;

        SourceAlignGrid.Enabled := false;

        if overwrite then
        begin
          SourceAlignGrid.SetSeqData(SeqList);
          SourceAlignGrid.IsDNA := isDNA;
        end
        else if SourceAlignGrid.IsDNA <> isDNA then
        begin
          if SourceAlignGrid.IsDNA then
            mr := MessageDlg('You may insert protein sequences in DNA data. Continue?', mtWarning, [mbCancel, mbIgnore], 0)
          else
            mr := MessageDlg('You may insert DNA sequences in protein data. Continue?', mtWarning, [mbCancel, mbIgnore], 0);

          if mr = mrCancel then
            exit;

          SourceAlignGrid.InsertSequenceList(SeqList);
        end
        else
          SourceAlignGrid.InsertSequenceList(SeqList);

        SourceAlignGrid.Enabled := true;
        CurAlignGrid.Cols.Col[GROUP_COLUMN].Visible := False; { group names are only supported by .meg and .mas files}
        result := true;
      end;
    end;
  except
    On E:Exception do
    begin
      ShowMessage('Oh no! An error occurred when loading the sequence data file: ' + E.Message);
    end;
  end;
  finally
    if Assigned(Converter) then
      Converter.Free;
    if Assigned(SeqList) then
      SeqList.Free;
    if Assigned(Seq) then
      Seq.Free;
  end;
end;

function TAlignEditMainForm.SaveSession(filename: String): boolean;
var
  f: File;
  i: integer;
  b: boolean;
  r: double;
  s: AnsiString;
  FileLength: Integer;
  FullFileBuffer: Array of Byte = nil;
  TheCRC: Cardinal;
  aProgress: TRuntimeProgress;
  UserAborted: Boolean;
begin
  aProgress := nil;
  result := false;
  UserAborted := False;

  try
    AssignFile(f, filename);
    ReWrite(f, 1);
  except
    exit;
  end;
  result := true;
  try
    try
      aProgress := TRuntimeProgress.Create(Self);
      aProgress.AddRunStatusInfo('Status', 'Saving Alignment Session');
      aProgress.UpdatePercentProgress(0);
      aProgress.Show;
      i := $53414D23;
      BlockWrite(f, i, 4);  // write '#MAS'
      i := MAS_SESSION_VERSION;
      BlockWrite(f, i, 4);  // write version
      { VerifiedTargetPlatform used to specify whether the file supports extended precision, see megautils comments }
      i := VerifiedTargetPlatform;
      BlockWrite(f, i, SizeOf(i));

      SourceAlignGrid.WriteToFile(f, UserAborted, aProgress);
      if UserAborted then
      begin
        Result := False;
      end
      else
      begin
        s := GetEnumName(TypeInfo(TDNAMatrix), integer(ClustalW.DNAMatrix));
        i := length(s)+1;
        BlockWrite(f, i, 4);
        BlockWrite(f, s[1], i);

        s := GetEnumName(TypeInfo(TProteinMatrix), integer(ClustalW.ProteinMatrix));
        i := length(s)+1;
        BlockWrite(f, i, 4);
        BlockWrite(f, s[1], i);

        r := ClustalW.DNAPWGapOpenPenalty;
        BlockWrite(f, r, 8);
        r := ClustalW.DNAPWGapExtendPenalty;
        BlockWrite(f, r, 8);
        r := ClustalW.DNAGapOpenPenalty;
        BlockWrite(f, r, 8);
        r := ClustalW.DNAGapExtendPenalty;
        BlockWrite(f, r, 8);
        r := ClustalW.ProteinPWGapOpenPenalty;
        BlockWrite(f, r, 8);
        r := ClustalW.ProteinPWGapExtendPenalty;
        BlockWrite(f, r, 8);
        r := ClustalW.ProteinGapOpenPenalty;
        BlockWrite(f, r, 8);
        r := ClustalW.ProteinGapExtendPenalty;
        BlockWrite(f, r, 8);

        i := ClustalW.DivergentCutoff;
        BlockWrite(f, i, 4);
        r := ClustalW.TransitionWeight;
        BlockWrite(f, r, 8);
        b := ClustalW.UseNegativeMatrix;
        BlockWrite(f, b, 1);

        b := ClustalW.ResidueSpecificPenalty;
        BlockWrite(f, b, 1);
        b := ClustalW.HydrophilicPenalty;
        BlockWrite(f, b, 1);
        b := ClustalW.EndGapSeparation;
        BlockWrite(f, b, 1);
        i := ClustalW.GapSeparationDistance;
        BlockWrite(f, i, 4);

        b := ClustalW.ResetGaps;
        BlockWrite(f, b, 1);
      end;
    except
      result := false;
    end;
  finally
    if Assigned(aProgress) then
      aProgress.Free;
    CloseFile(f);
    if UserAborted then
      DeleteFile(filename);
  end;

  if not UserAborted then
  begin
    // We construct a CRC of the file we just wrote out, and append it to the end of the file.
    try
      AssignFile(f, filename);
      FileMode := fmOpenReadWrite;                                             // Should be default, but it could be changed as it is a global var.
      Reset(f, 1);                                                             // Reset file handle with record length of 1
    except on E: Exception do
    begin
       // The file was not able to be opened with these permissions
       ShowMessage('MEGA has encountered an error.' +#10#13 + 'While opening the MAS file to append a CRC we could not open it.' + #10#13 + 'Technical Info: ' + E.Message);
       result := false; // An error has occured, save was unsuccessfull.
       exit;
    end;
    end;
    try try
      FileLength := FileSize(f);                                               // Get the file length of the session file we just wrote
      SetLength(FullFileBuffer, FileLength);                                   // create a buffer to hold the whole file we just created
      BlockRead(f, FullFileBuffer[0], FileLength);                             // read in the whole file
      if FilePos(f) <> FileLength then                                         // Check that we have successfully read the full file in.
        ShowMessage('MEGA has encountered an error.' + #10#13 + 'Location mismatch: FilePos:' + IntToStr(FilePos(f)) + '; FileLength:' + IntToStr(FileLength));
      theCRC := $FFFFFFFF;                                                     // Initalizing theCRC so that it conforms to the CRC standard
      CalcCRC32(FullFileBuffer, FileLength, theCRC);                           // Generate a CRC for the file in the buffer.
      theCRC := not theCRC;                                                    // Perform 1's complement to conform to the CRC standard
      BlockWrite(f, Integer(theCRC), SizeOf(Integer));                         // Append the CRC to the end of the file.
    except on E: Exception do
    begin
      ShowMessage('MEGA Encountered an error while attempting to write the CRC for this session file.  The file saved is incomplete.' + #10#13 + 'Technical Info: ' + E.Message);
      result := false;
    end;
    end;
    finally
      CloseFile(f);
    end;
    // End of CRC addition.
  end;
end;

function TAlignEditMainForm.GetSessionVersion(FileName: String): Integer;
var
  AFile: File;
  i: Integer = -1;
  AVersion: Integer = -1;
begin
  Result := -1;
  try
    AssignFile(AFile, FileName);
    Reset(AFile, 1);
    BlockRead(AFile, i, 4);
    if i <> $53414D23 then
      exit;
    BlockRead(AFile, AVersion, 4);
    Result := AVersion;
  finally
    CloseFile(AFile);
  end;
end;

function TAlignEditMainForm.RetrieveSession(filename: String; PleaseWait: TPleaseWait): boolean;
var
  f: File;
  i: Integer = -1;
  version: integer = -1;
  b: boolean = False;
  r: double = 0;
  s: AnsiString = '';
  calcCRC: Cardinal = $00000000;
  fileCRC: Cardinal = $00000000;
  FullFileBuffer: Array of Byte = nil;
  numRead: Integer = -1;
  FileLength: Integer = -1;
  BookmarkPos: Integer = -1;
  msg: String = '';
begin
  result := false;
  try
    AssignFile(f, filename);
    Reset(f, 1);

    BlockRead(f, i, 4);
    if i <> $53414D23 then exit;

    BlockRead(f, version, 4);
    if version <= LAST_BAD_MAS_SESSION_VERSION then
    begin
      Result := RetrieveBrokenSession(f, PleaseWait);
    end
    else
    begin
      if (version >= 1001) then
      begin
        BlockRead(f, i, SizeOf(i));
        if not SessionIsSameTargetPlatform(i, msg) then
          raise Exception.Create(msg);
      end;
      if version > 110 then                                       // Prior to ver 110 CRC was not included
      begin
        BookmarkPos := FilePos(f);                                // We will need to later return to this point
        try
          Seek(f, 0);                                               // seek to the start as the entire file needs to be included in CRC calculation
          FileLength := FileSize(f) - 4;                            // FileLength-4 since the last 4 bytes are the CRC!
          SetLength(FullFileBuffer, FileLength);
          numRead := 0;
          BlockRead(f, FullFileBuffer[0], FileLength, numRead);     // Read the entire file minus the 4 byte CRC at the end.
          if numRead <> FileLength then                             // Confirm that we have read the whole file (no IO Issue).
            ShowMessage('MEGA has encountered an error.' + #10#13 + 'We were not able to read the entire session file to be restored.  Read: ' + IntToStr(numRead) + '; Should have read: ' + IntToStr(FileLength));
          calcCRC := $FFFFFFFF;                                     // initaialize CRC value
          CalcCRC32(FullFileBuffer, FileLength, calcCRC);
          calcCRC := not calcCRC;                                   // perform 1's compelement
          BlockRead(f, fileCRC, 4);                                 // the CRC
          if fileCRC <> calcCRC then
            if (MessageDlg('Cyclic Redundancy Check (CRC) has failed for this alignment session file.'+LineEnding+'This means the file is likely corrupt!'+LineEnding+''+LineEnding+'Would you like to try to restore the file anyway?', mtError, [mbYes, mbNo], 0) <> mrYes) then
              Exit;
          Seek(f, BookmarkPos); // Seek back to the beginning.
        except on E: Exception do
          if (MessageDlg('MEGA Encountered an error while checking the integrity of this file.  This could mean that it is corrupt or MEGA has a bug.'+ LineEnding +'Please send us a bug report with the file and instructions on what happened before the error.'+ LineEnding +''+ LineEnding +'Would you like to attempt to open the file regardless?', mtError, [mbYes, mbNo], 0) = mrYes) then
            Seek(f, BookmarkPos)
          else
            Exit;
        end;
      end;

      if not SourceAlignGrid.ReadFromFile(f) then exit;

      BlockRead(f, i, 4);
      setLength(s, i-1);
      BlockRead(f, s[1], i);
      ClustalW.DNAMatrix := TDNAMatrix(GetEnumValue(TypeInfo(TDNAMatrix), s));

      BlockRead(f, i, 4);
      setLength(s, i-1);
      BlockRead(f, s[1], i);
      ClustalW.ProteinMatrix := TProteinMatrix(GetEnumValue(TypeInfo(TProteinMatrix), s));

      BlockRead(f, r, 8);
      ClustalW.DNAPWGapOpenPenalty := r;
      BlockRead(f, r, 8);
      ClustalW.DNAPWGapExtendPenalty := r;
      BlockRead(f, r, 8);
      ClustalW.DNAGapOpenPenalty := r;
      BlockRead(f, r, 8);
      ClustalW.DNAGapExtendPenalty := r;
      BlockRead(f, r, 8);
      ClustalW.ProteinPWGapOpenPenalty := r;
      BlockRead(f, r, 8);
      ClustalW.ProteinPWGapExtendPenalty := r;
      BlockRead(f, r, 8);
      ClustalW.ProteinGapOpenPenalty := r;
      BlockRead(f, r, 8);
      ClustalW.ProteinGapExtendPenalty := r;

      BlockRead(f, i, 4);
      ClustalW.DivergentCutoff := i;
      BlockRead(f, r, 8);
      ClustalW.TransitionWeight := r;
      BlockRead(f, b, 1);
      ClustalW.UseNegativeMatrix := b;

      BlockRead(f, b, 1);
      ClustalW.ResidueSpecificPenalty := b;
      BlockRead(f, b, 1);
      ClustalW.HydrophilicPenalty := b;
      BlockRead(f, b, 1);
      ClustalW.EndGapSeparation := b;
      BlockRead(f, i, 4);
      ClustalW.GapSeparationDistance := i;

      BlockRead(f, b, 1);
      ClustalW.ResetGaps := b;
      CurAlignGrid.Cols.Col[GROUP_COLUMN].Visible := SourceAlignGrid.HasGroupNames;
      Result := True;
    end;
  finally
    CloseFile(f);
  end;
end;

function TAlignEditMainForm.RetrieveBrokenSession(var AFile: File; PleaseWait: TPleaseWait): Boolean;
begin
 ShowMessage('The active alignment session file was made with an older version of MEGA and cannot be opened in this version of MEGA. However, you can open the session file in the version of MEGA that was used to create it and export it to either a FASTA or .meg file which can both be opened in this version of MEGA');
 Result := False;
 Exit;
end;

function TAlignEditMainForm.InsertSession(filename: String): boolean;
begin
 ShowMessage('Inserting sequences from session files is not yet implemented. You can open session files by clicking Data->Open->Saved Alignment Session');
 Result := False;
 Exit;
end;

function TAlignEditMainForm.SelectedToFasta(const FileName: String; StandardizeNames: Boolean; ReplaceUnkChars: Boolean; var XSites: String; var seqList: TSequenceList): Boolean;
var
  i, j: Integer;
  seq: TSequenceList;
  TempFastaList: TStringList;
  str: TSequence;
begin
  Result := False;
  seq := TSequenceList.Create;
  if (not CurAlignGrid.BlockSelected) and (not CurAlignGrid.SequenceSelected) then
  begin
    TempPos.X := 0;
    TempGap := 0;
    for i := 1 to CurAlignGrid.Col do
    begin
      if i > CurAlignGrid.Sequence[CurAlignGrid.Row-1].NoOfSites then
        break;
      if CurAlignGrid.Sequence[CurAlignGrid.Row-1].Base[i] = '-' then
        inc(TempGap)
      else
        inc(TempPos.X);
    end;
    TempPos.Y := CurAlignGrid.Row;
    CurPos.X := CurAlignGrid.LeftCol;
    CurPos.Y := CurAlignGrid.TopRow;
    CurAlignGrid.SelectAll;
  end
  else
  begin
    TempPos.X := 0;
    TempPos.Y := 0;
  end;
  CurAlignGrid.GetSelectedData(seq);
  SeqList.Assign(seq);
  if seq.Count <= 1 then
  begin
    Enabled := true;
    exit;
  end;
  try
    if ReplaceUnkChars then
    begin
      XSites := EmptyStr; // Make sure it is empty.
      for i := 0 to Seq.Count-1 do
      begin
        str := Seq[i];
        for j := 1 to str.NoOfSites do
          if CurAlignGrid.IsDNA then
          begin
            if (not (upcase(str[j]) in DNABases)) and (str[j] <> '-') then
            begin
              XSites := XSites +str[j];
              str[j] := 'N';
            end
          end
          else if (not (upcase(str[j]) in AABases)) and (str[j] <> '-') then // Substitute any non AA bases with X.
          begin
            XSites := XSites +str[j];
            str[j] := 'X';
          end;
        Seq[i] := str;
      end;
    end;
    TempFastaList := TStringList.Create;
    for i := 0 to seq.Count-1 do
    begin
      if StandardizeNames then
        TempFastaList.Add('>TX' + IntToStr(i))
      else
        TempFastaList.Add('>' + seq[i].SeqName);
      TempFastaList.Add(seq[i].SeqData);
    end;

    try
      { When we use this function for muscle the place we are writing the file to may
      be locked by muscle if there was an unhandled error and it didn't release the
      file, so we kill the muscle process so we can save again.}
      if Assigned(FMuscleThread) and Assigned(FMuscleThread.MuscleLink) and Assigned(FMuscleThread.MuscleLink.Process) then
        if FMuscleThread.MuscleLink.Process.Active then
          FMuscleThread.MuscleLink.Process.Terminate(1);
    except
      { cross our fingers and hope for the best!}
    end;
    sleep(100); // After kill task runs it needs a little time for the selected and results files to unlock.
    try
      TempFastaList.SaveToFile(FileName);
      Result := FileExists(FileName);
    except
      on E : Exception do
      begin
        ShowMessage('Error while attempting to save the selected data to a Fasta File: ' + FileName + '.  Please ensure another program is not currently using this file.');
      end;
    end;
  finally
    FreeAndNil(TempFastaList);
    FreeAndNil(seq);
  end;
end;


end.


