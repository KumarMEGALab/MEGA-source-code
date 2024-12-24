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

 unit mega_main;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFNDEF DEBUG}
   UniqueInstance,
  {$ENDIF}
  MTimeTreeWizard, mtimetreewizardform, Classes, SysUtils, FileUtil,
  PrintersDlgs, Forms, Controls, Graphics, Dialogs, Menus, ActnList, ComCtrls,
  ExtCtrls, KeywordConsts, StdCtrls, Buttons, MegaVerConsts, applinkeroptions,
  fpjson, jsonparser, MWebBrowser, PopupNotifier, AppLinker,
  recentlyusedfiles, GeneDuplicationWizardForm, mruntimeprogressdlg, MAnalysisWizardForm,
  MAnalysisWizard, LCLType, IniPropStorage, mhelpkeywords, mimageform, syncobjs,
  MegaConsts;

{$IFDEF DEBUG}
var
  DEBUG_File: String;
{$ENDIF}
const
  Beta_Expires_Year = 2021;
  Beta_Expires_Month = 8;

  MEN_IMG_ALIGN = 0;
  MEN_IMG_DATA = 2;
  MEN_IMG_MODELS = 5;
  MEN_IMG_DISTANCE = 3;
  MEN_IMG_DIVERSITY = 4;
  MEN_IMG_PHYLOGENY = 6;
  MEN_IMG_USERTREE = 9;
  MEN_IMG_ANCESTORS = 0;
  MEN_IMG_SELECTION = 8;
  MEN_IMG_RATES = 7;
  MEN_IMG_CLOCKS = 1;
  MEN_IMG_DIAGNOSE = 1;

  BT_IMG_HELP = 3;
  BT_IMG_EXAMPLES = 2;
  BT_IMG_CITATION = 1;
  BT_IMG_BUG_REPORT = 0;
  BT_IMG_UPDATES = 7;
  BT_IMG_MEGA_LINKS = 4;
  BT_IMG_TOOLBARS = 6;
  BT_IMG_PREFERENCES = 5;
  MENU_ITEM_MARGIN = 6;

type
  TOpenFrom = (OActivate, OAlign, OOther);  // Lets us know from which button(s) the user is trying to open a file.
  TMegaRunMode = (mrmGui, mrmPrototyper, mrmWorkflow);

  { TMegaForm }

  TMegaForm = class(TForm)
    AboutWindowAction: TAction;
    MenuItem191: TMenuItem;
    MenuItem192: TMenuItem;
    SiteCoverageAction: TAction;
    MenuItem140: TMenuItem;
    MenuItem188: TMenuItem;
    MenuItem189: TMenuItem;
    MenuItem190: TMenuItem;
    ReconstructMigrationsAction: TAction;
    EstimateTumorDiversityAction: TAction;
    EstimateMutationalSignaturesAction: TAction;
    InferTumorPhylogenyAction: TAction;
    InferClonePhylogenyAction: TAction;
    CloneFinderAction: TAction;
    LoadSingleCellSequenceAction: TAction;
    LoadBulkSequenceDataAction: TAction;
    BeamAction: TAction;
    DatamonkeyJobResults: TMenuItem;
    IniPropStorage1: TIniPropStorage;
    MenuItem152: TMenuItem;
    MenuItem153: TMenuItem;
    MenuItem172: TMenuItem;
    MenuItem179: TMenuItem;
    MenuItem180: TMenuItem;
    MenuItem181: TMenuItem;
    MenuItem182: TMenuItem;
    MenuItem183: TMenuItem;
    MenuItem184: TMenuItem;
    MenuItem185: TMenuItem;
    MenuItem186: TMenuItem;
    MenuItem187: TMenuItem;
    N1: TMenuItem;
    MenuItem154: TMenuItem;
    MenuItem169: TMenuItem;
    RtdtMlMenu2: TMenuItem;
    RtdtOLSMenu2: TMenuItem;
    RtdtBlensMenu2: TMenuItem;
    MenuItem73: TMenuItem;
    NewIcons: TImageList;
    MenuItem139: TMenuItem;
    RtdtBlensMenuItem: TMenuItem;
    RtdtMLMenuItem: TMenuItem;
    RtdtLsMenuItem: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItemImages: TImageList;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    TopToolbarEndCap: TImage;
    Image5: TImage;
    IQTREETreeInfAction: TAction;
    MEGAXPubStoryAction: TAction;
    MEGAXPubAction: TAction;
    JobBadges: TImageList;
    IQTreeBtn: TImage;
    IQTreePopup: TPopupMenu;
    IQTreeMenuItem2: TMenuItem;
    IQTreeMenuItem1: TMenuItem;
    MEGAXPubMenuItem: TMenuItem;
    MEGAXPubStoryMenuItem: TMenuItem;
    PublicationsLink: TLabel;
    PublicationsPopupMenu: TPopupMenu;
    TimeTreeBtn: TImage;
    TimetreeAction: TAction;
    TimelineAction: TAction;
    NodeTimeAction: TAction;
    NodeTimeMenuItem: TMenuItem;
    TimetreeMenuItem: TMenuItem;
    TimeTreeDBPopup: TPopupMenu;
    DatamonkeyBtn: TImage;
    DatamonkeyPopup: TPopupMenu;
    ScrollBox1: TScrollBox;
    Shape1: TShape;
    SLACMenuItem3: TMenuItem;
    ThirdPartyIcons: TImageList;
    IQTREEModelSelAction: TAction;
    TimelineMenuItem: TMenuItem;
    ThirdPartyToolsAction: TAction;
    WebToolsBtn: TImage;
    SLACAction: TAction;
    SLACMenuItem: TMenuItem;
    EvolutionaryProbsAction: TAction;
    FeedbackLink: TLabel;
    BtGray: TImageList;
    EpSpacer2: TMenuItem;
    MenuItem168: TMenuItem;
    MenuItem170: TMenuItem;
    EpSpacer1: TMenuItem;
    CorrtestMenuItem: TMenuItem;
    MenuItem171: TMenuItem;
    MenuItem178: TMenuItem;
    MenuItem60: TMenuItem;
    ProtoHelpImage: TImage;
    MegaLogoImg: TImage;
    MenuItem59: TMenuItem;
    PrototypeBtn: TImage;
    AnalyzeBtn: TImage;
    RunModeImages: TImageList;
    MenuItem150: TMenuItem;
    MenuItem155: TMenuItem;
    PreferencesMenuItem: TMenuItem;
    MenuItem175: TMenuItem;
    MenuItem176: TMenuItem;
    MenuItem177: TMenuItem;
    CheckBetaExpiredTimer: TTimer;
    MenuItem57: TMenuItem;
    MegaMenuItem: TMenuItem;
    AboutMegaMenuItem: TMenuItem;
    TutorialAction: TAction;
    FirstTimeUserAction: TAction;
    DeveloperAction: TAction;
    AlignMenuImg: TImage;
    AncestorsMenuImg: TImage;
    CenterPanel: TPanel;
    ClocksMenuImg: TImage;
    CloseDataBtn: TToolButton;
    CurExitBtn: TImage;
    DataMenuImg: TImage;
    DeveloperLabel: TLabel;
    DiagnoseMenuImg: TImage;
    DistanceMenuImg: TImage;
    DiversityMenuImg: TImage;
    HelpBtn: TImage;
    BtRed: TImageList;
    BtBlue: TImageList;
    ExamplesBtn: TImage;
    CitationBtn: TImage;
    BugReportBtn: TImage;
    HotIcons: TImageList;
    DisabledIcons: TImageList;
    ReltimeMlMenuItem: TMenuItem;
    ReltimeOlsMenuItem: TMenuItem;
    ReltimeBlensMenuItem: TMenuItem;
    ReltimeOLSMenu2: TMenuItem;
    ReltimeBLensMenu2: TMenuItem;
    MenuItem173: TMenuItem;
    MenuItem174: TMenuItem;
    ReltimeMLMenu2: TMenuItem;
    PinupImagesHot: TImageList;
    PinupImages: TImageList;
    PinupsToolbar: TToolBar;
    UpdatesBtn: TImage;
    LinksBtn: TImage;
    ToolbarBtn: TImage;
    PreferencesBtn: TImage;
    MagicBtn: TBitBtn;
    MessagesMemo: TMemo;
    ModelsMenuImg: TImage;
    NotificationsPanel: TPanel;
    OpenFileDirSTxt: TStaticText;
    BottomToolbar: TPanel;
    PhylogenyMenuImg: TImage;
    PreviewImg: TImage;
    PreviewPanel: TPanel;
    RatesMenuImg: TImage;
    SelectionMenuImg: TImage;
    SetIsDeveloperBtn: TButton;
    TopToolbar: TPanel;
    TrayImages: TImageList;
    Default78x90 :TImageList;
    Default72x90 :TImageList;
    Hover72x90 :TImageList;
    Gray78x90 :TImageList;
    Gray72x90 :TImageList;
    Hover78x90 :TImageList;
    TrayToolbar2: TToolBar;
    UrlButton: TButton;
    UserTreeMenuImg: TImage;
    FileMenu: TPopupMenu;
    MenuItem156: TMenuItem;
    MenuItem157: TMenuItem;
    MenuItem158: TMenuItem;
    MenuItem159: TMenuItem;
    MenuItem160: TMenuItem;
    MenuItem161: TMenuItem;
    MenuItem162: TMenuItem;
    MenuItem163: TMenuItem;
    MenuItem164: TMenuItem;
    HelpMenu: TPopupMenu;
    MenuItem165: TMenuItem;
    MenuItem166: TMenuItem;
    AnalysisMenu: TPopupMenu;
    MenuItem167: TMenuItem;
    ReltimeBlensAction: TAction;
    CorrTestBlensAction: TAction;
    CorrTestMLAction: TAction;
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
    MenuItem111: TMenuItem;
    MenuItem112: TMenuItem;
    MenuItem113: TMenuItem;
    MenuItem114: TMenuItem;
    MenuItem115: TMenuItem;
    MenuItem116: TMenuItem;
    MenuItem117: TMenuItem;
    MenuItem118: TMenuItem;
    MenuItem119: TMenuItem;
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
    MenuItem130: TMenuItem;
    MenuItem131: TMenuItem;
    MenuItem132: TMenuItem;
    MenuItem133: TMenuItem;
    MenuItem134: TMenuItem;
    MenuItem135: TMenuItem;
    MenuItem136: TMenuItem;
    MenuItem137: TMenuItem;
    MenuItem138: TMenuItem;
    MenuItem141: TMenuItem;
    MenuItem142: TMenuItem;
    MenuItem143: TMenuItem;
    MenuItem144: TMenuItem;
    MenuItem145: TMenuItem;
    MenuItem146: TMenuItem;
    MenuItem147: TMenuItem;
    MenuItem148: TMenuItem;
    MenuItem149: TMenuItem;
    MenuItem151: TMenuItem;
    MenuItem81: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem92: TMenuItem;
    MenuItem93: TMenuItem;
    MenuItem94: TMenuItem;
    MenuItem95: TMenuItem;
    MenuItem99: TMenuItem;
    SaveDialog: TSaveDialog;
    ConcatenateFilesAction: TAction;
    GeneDupsAction: TAction;
    DataSelectGeneticCodeAction: TAction;
    DataExitAction: TAction;
    DataPrinterSetupAction: TAction;
    DataOpenAFileAction: TAction;
    ConvertToMegaFormatAction: TAction;
    ATreeOpenDlg: TOpenDialog;
    MenuItem84: TMenuItem;
    MenuItem89: TMenuItem;
    TrayIconHot: TImageList;
    OpenTextEditorAction: TAction;
    ExampleFilesAction: TAction;
    aDataOpenDlg: TOpenDialog;
    MenuItem83: TMenuItem;
    DataRecentlyUsedFilesSessionsItem: TMenuItem;
    MenuItem85: TMenuItem;
    MenuItem86: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    MenuItem96: TMenuItem;
    MenuItem97: TMenuItem;
    MenuItem98: TMenuItem;
    APrinterSetupDlg: TPrinterSetupDialog;
    DisplayHelpContentsAction: TAction;
    MenuItem79: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem82: TMenuItem;
    PredictLivingSequenceAction: TAction;
    ComputeTimetreeLSAction: TAction;
    CheckUpdatesTimer: TTimer;
    MegaCCOnlineManualAction: TAction;
    CloseDataAction: TAction;
    AlignProteinClustalAction: TAction;
    AlignCodonsClustalAction: TAction;
    AlignDnaClustalAction: TAction;
    AlignProteinMuscleAction: TAction;
    AlignCodonsMuscleAction: TAction;
    AlignDnaMuscleAction: TAction;
    AlignProtoPopupMenu: TPopupMenu;
    ClustalNucAlignmentMenuItem: TMenuItem;
    MenuItem58: TMenuItem;
    ClustalCodonAlignmentMenuItem: TMenuItem;
    ClustalProteinAlignmentMenuItem: TMenuItem;
    MenuItem78: TMenuItem;
    MuscleProteinAlignmentMenuItem: TMenuItem;
    MuscleCodonAlignmentMenuItem: TMenuItem;
    MuscleNucAlignmentMenuItem: TMenuItem;
    HelpDocsPopupMenu: TPopupMenu;
    UpdateToolBarAction: TAction;
    AlignToolMenuItem: TMenuItem;
    DataToolMenuItem: TMenuItem;
    ModelsToolMenuItem: TMenuItem;
    DistanceToolMenuItem: TMenuItem;
    DiversityToolMenuItem: TMenuItem;
    PhylogenyToolMenuItem: TMenuItem;
    UserTreeToolMenuItem: TMenuItem;
    AncestorsToolMenuItem: TMenuItem;
    SelectionToolMenuItem: TMenuItem;
    RatesToolMenuItem: TMenuItem;
    ClocksToolMenuItem: TMenuItem;
    DiagnoseToolMenuItem: TMenuItem;
    MenuItem91: TMenuItem;
    OnlineManualAction: TAction;
    MEGABrowserAction: TAction;
    MenuItem77: TMenuItem;
    ShowMegaBrowserMenuItem: TMenuItem;
    PreferencesPopupMenu: TPopupMenu;
    ToolbarPopupMenu: TPopupMenu;
    TimetreeWebsiteAction: TAction;
    MEGAWebsiteAction: TAction;
    DiagnoseMutationAction: TAction;
    ComputeTimetreeMLAction: TAction;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    MolecularClockTestAction: TAction;
    MenuItem72: TMenuItem;
    DiagnosePopupMenu: TPopupMenu;
    LinksPopupMenu: TPopupMenu;
    TajimaRateTestAction: TAction;
    MenuItem71: TMenuItem;
    ClocksPopupMenu: TPopupMenu;
    PositionByPositionRatesMLAction: TAction;
    GammaParameterMLAction: TAction;
    MenuItem69: TMenuItem;
    MenuItem70: TMenuItem;
    RatesPopupMenu: TPopupMenu;
    TajimaNeutralityTestAction: TAction;
    CodonFisherTestAction: TAction;
    CodonZTestAction: TAction;
    AncestralSeqsMPAction: TAction;
    AncestralSeqsMLAction: TAction;
    DisplayTreeMenuItem: TMenuItem;
    OpenTreeSessionMenuItemSpacer: TMenuItem;
    TreeSessionMenuItem: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    DisplayNewickTreeAction: TAction;
    EditTreeAction: TAction;
    AnalyzeTreeMPAction: TAction;
    AnalyzeTreeLSAction: TAction;
    AnalyzeTreeMLAction: TAction;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    EditTreeMenuItemSpacer: TMenuItem;
    DrawTreeMenuItem: TMenuItem;
    AncestorsPopupMenu: TPopupMenu;
    SelectionPopupMenu: TPopupMenu;
    UserTreePopupMenu: TPopupMenu;
    OpenTreeSessionAction: TAction;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    ConstructMPTreeAction: TAction;
    MenuItem50: TMenuItem;
    ConstructUPGMATreeAction: TAction;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    ConstructMETreeAction: TAction;
    MenuItem47: TMenuItem;
    ConstructNJTreeAction: TAction;
    ConstructMLTreeAction: TAction;
    CoefficientOfDifferentiationAction: TAction;
    MeanDiversityInterPopAction: TAction;
    MeanDiversityEntireAction: TAction;
    MeanDiversityWithinAction: TAction;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    NetMeanAction: TAction;
    BetweenMeanAction: TAction;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    DiversityPopupMenu: TPopupMenu;
    PhylogenyPopupMenu: TPopupMenu;
    VersionLabel: TLabel;
    WithinMeanDistanceAction: TAction;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    OverallMeanDistanceAction: TAction;
    PairwiseDistanceAction: TAction;
    CodonUsageAction: TAction;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    NucleotideCompAction: TAction;
    AminoAcidCompAction: TAction;
    CompositionDistanceAction: TAction;
    MCLBiasAction: TAction;
    MCLMatrixAction: TAction;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    PatternDisparityAction: TAction;
    DistancePopupMenu: TPopupMenu;
    TransitionBiasAction: TAction;
    MenuItem24: TMenuItem;
    SubstitutionMatrixAction: TAction;
    DisparityIndexAction: TAction;
    FindBestDNAAction: TAction;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    ModelsPopupMenu: TPopupMenu;
    SelectTaxaGroupAction: TAction;
    MenuItem19: TMenuItem;
    SelectGeneDomainAction: TAction;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    SelectGeneticCodeAction: TAction;
    MenuItem13: TMenuItem;
    SaveDataAction: TAction;
    ExportDataAction: TAction;
    ExploreDataAction: TAction;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    OpenFileAction: TAction;
    BLASTAction: TAction;
    MenuItem10: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    DataPopupMenu: TPopupMenu;
    QueryDatabankAction: TAction;
    MenuItem7: TMenuItem;
    ShowBrowserAction: TAction;
    MenuItem6: TMenuItem;
    OpenAlignmentAction: TAction;
    EditSequencerAction: TAction;
    BuildAlignmentAction: TAction;
    MegaActionList: TActionList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    AlignPopupMenu: TPopupMenu;
    MainStatusBar: TStatusBar;

    procedure AboutWindowActionExecute(Sender: TObject);
    procedure BeamActionExecute(Sender: TObject);
    procedure CloneFinderActionExecute(Sender: TObject);
    procedure SiteCoverageActionExecute(Sender: TObject);
    procedure EstimateMutationalSignaturesActionExecute(Sender: TObject);
    procedure EstimateTumorDiversityActionExecute(Sender: TObject);
    procedure InferClonePhylogenyActionExecute(Sender: TObject);
    procedure InferTumorPhylogenyActionExecute(Sender: TObject);
    procedure IQTreeBtnClick(Sender: TObject);
    procedure IQTreeBtnMouseEnter(Sender: TObject);
    procedure IQTREETreeInfActionExecute(Sender: TObject);
    procedure LoadBulkSequenceDataActionExecute(Sender: TObject);
    procedure LoadSingleCellSequenceActionExecute(Sender: TObject);
    procedure MegaActionListExecute(AAction: TBasicAction; var Handled: Boolean
      );
    procedure PublicationsLinkClick(Sender: TObject);
    procedure PublicationsLinkMouseEnter(Sender: TObject);
    procedure PublicationsLinkMouseLeave(Sender: TObject);
    procedure ReconstructMigrationsActionExecute(Sender: TObject);
    procedure TimeTreeBtnClick(Sender: TObject);
    procedure TimeTreeBtnMouseEnter(Sender: TObject);
    procedure IQTreeBtnMouseLeave(Sender: TObject);
    procedure AlignCodonsClustalActionExecute(Sender: TObject);
    procedure AlignCodonsMuscleActionExecute(Sender: TObject);
    procedure AlignDnaClustalActionExecute(Sender: TObject);
    procedure AlignDnaMuscleActionExecute(Sender: TObject);
    procedure AlignMenuImgMouseEnter(Sender :TObject);
    procedure AlignMenuImgMouseLeave(Sender :TObject);
    procedure AlignProteinClustalActionExecute(Sender: TObject);
    procedure AlignProteinMuscleActionExecute(Sender: TObject);
    procedure AlignToolMenuItemClick(Sender: TObject);
    procedure AminoAcidCompActionExecute(Sender: TObject);
    procedure AnalyzeBtnClick(Sender: TObject);
    procedure AnalyzeBtnMouseEnter(Sender: TObject);
    procedure AnalyzeBtnMouseLeave(Sender: TObject);
    procedure AnalyzeTreeLSActionExecute(Sender: TObject);
    procedure AnalyzeTreeMLActionExecute(Sender: TObject);
    procedure AnalyzeTreeMPActionExecute(Sender: TObject);
    procedure AncestorsMenuImgMouseEnter(Sender :TObject);
    procedure AncestorsMenuImgMouseLeave(Sender :TObject);
    procedure AncestorsToolMenuItemClick(Sender: TObject);
    procedure AncestralSeqsMLActionExecute(Sender: TObject);
    procedure AncestralSeqsMPActionExecute(Sender: TObject);
    procedure BetweenMeanActionExecute(Sender: TObject);
    procedure BLASTActionExecute(Sender: TObject);
    procedure BugReportBtnClick(Sender: TObject);
    procedure BugReportBtnMouseEnter(Sender: TObject);
    procedure BugReportBtnMouseLeave(Sender: TObject);
    procedure BuildAlignmentActionExecute(Sender: TObject);
    procedure CheckBetaExpiredTimerTimer(Sender: TObject);
    procedure CheckUpdatesTimerTimer(Sender: TObject);
    procedure CitationBtnClick(Sender: TObject);
    procedure CitationBtnMouseEnter(Sender: TObject);
    procedure CitationBtnMouseLeave(Sender: TObject);
    procedure ClocksMenuImgMouseEnter(Sender :TObject);
    procedure ClocksMenuImgMouseLeave(Sender :TObject);
    procedure ClocksToolMenuItemClick(Sender: TObject);
    procedure CloseDataActionExecute(Sender: TObject);
    procedure CloseDataBtnClick(Sender: TObject);
    procedure CodonFisherTestActionExecute(Sender: TObject);
    procedure CodonUsageActionExecute(Sender: TObject);
    procedure CodonZTestActionExecute(Sender: TObject);
    procedure CompositionDistanceActionExecute(Sender: TObject);
    procedure ComputeTimetreeLSActionExecute(Sender: TObject);
    procedure ComputeTimetreeMLActionExecute(Sender: TObject);
    procedure ConcatenateFilesActionExecute(Sender: TObject);
    procedure ConvertToMegaFormatActionExecute(Sender: TObject);
    procedure CorrTestBlensActionExecute(Sender: TObject);
    procedure CorrTestMLActionExecute(Sender: TObject);
    procedure CurExitBtnClick(Sender: TObject);
    procedure CurExitBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CurExitBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DataExitActionExecute(Sender: TObject);
    procedure DataMenuImgMouseEnter(Sender :TObject);
    procedure DataMenuImgMouseLeave(Sender :TObject);
    procedure DatamonkeyBtnMouseEnter(Sender: TObject);
    procedure DatamonkeyBtnMouseLeave(Sender: TObject);
    procedure DataOpenAFileActionExecute(Sender: TObject);
    procedure DataPrinterSetupActionExecute(Sender: TObject);
    procedure DataSelectGeneticCodeActionExecute(Sender: TObject);
    procedure DataToolMenuItemClick(Sender: TObject);
    procedure DeveloperActionExecute(Sender: TObject);
    procedure DiagnoseMenuImgMouseEnter(Sender :TObject);
    procedure DiagnoseMenuImgMouseLeave(Sender :TObject);
    procedure DiagnoseToolMenuItemClick(Sender: TObject);
    procedure DisplayHelpContentsActionExecute(Sender: TObject);
    procedure DistanceMenuImgMouseEnter(Sender :TObject);
    procedure DistanceMenuImgMouseLeave(Sender :TObject);
    procedure DistanceToolMenuItemClick(Sender: TObject);
    procedure DiversityMenuImgMouseEnter(Sender :TObject);
    procedure DiversityMenuImgMouseLeave(Sender :TObject);
    procedure DiversityToolMenuItemClick(Sender: TObject);
    procedure EvolutionaryProbsActionExecute(Sender: TObject);
    procedure ExampleFilesActionExecute(Sender: TObject);
    procedure ExamplesBtnClick(Sender: TObject);
    procedure ExamplesBtnMouseEnter(Sender: TObject);
    procedure ExamplesBtnMouseLeave(Sender: TObject);
    procedure FeedbackLinkClick(Sender: TObject);
    procedure FeedbackLinkMouseEnter(Sender: TObject);
    procedure FeedbackLinkMouseLeave(Sender: TObject);
    procedure FirstTimeUserActionExecute(Sender: TObject);
    procedure FirstTimeUserBtnClick(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure HelpBtnMouseEnter(Sender: TObject);
    procedure HelpBtnMouseLeave(Sender: TObject);
    procedure DatamonkeyBtnClick(Sender: TObject);
    procedure IQTREEModelSelActionExecute(Sender: TObject);
    procedure MEGAXPubActionExecute(Sender: TObject);
    procedure MEGAXPubStoryActionExecute(Sender: TObject);
    procedure NodeTimeActionExecute(Sender: TObject);
    procedure ThirdPartyToolsActionExecute(Sender: TObject);
    procedure TimelineActionExecute(Sender: TObject);
    procedure TimetreeActionExecute(Sender: TObject);
    procedure TimetreeBtnMouseLeave(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TimeTreeDBBtnClick(Sender: TObject);
    procedure MenuItem91Click(Sender: TObject);
    procedure ProtoHelpImageClick(Sender: TObject);
    procedure ProtoHelpImageMouseEnter(Sender: TObject);
    procedure ProtoHelpImageMouseLeave(Sender: TObject);
    procedure LikelihoodRelTimeExecute(Sender: TObject);
    procedure DiagnoseMutationActionExecute(Sender: TObject);
    procedure CoefficientOfDifferentiationActionExecute(Sender: TObject);
    procedure DisparityIndexActionExecute(Sender: TObject);
    procedure EditSequencerActionExecute(Sender: TObject);
    procedure EditTreeActionExecute(Sender: TObject);
    procedure ExploreDataActionExecute(Sender: TObject);
    procedure ExportDataActionExecute(Sender: TObject);
    procedure FindBestDNAActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
    procedure FormResize(Sender: TObject);
    procedure GammaParameterMLActionExecute(Sender: TObject);
    procedure ConstructMLTreeActionExecute(Sender: TObject);
    procedure GeneDupsActionExecute(Sender: TObject);
    procedure LinksBtnMouseEnter(Sender: TObject);
    procedure LinksBtnMouseLeave(Sender: TObject);
    procedure MagicBtnClick(Sender: TObject);
    procedure MCLBiasActionExecute(Sender: TObject);
    procedure MCLMatrixActionExecute(Sender: TObject);
    procedure MeanDiversityEntireActionExecute(Sender: TObject);
    procedure MeanDiversityInterPopActionExecute(Sender: TObject);
    procedure MeanDiversityWithinActionExecute(Sender: TObject);
    procedure MEGABrowserActionExecute(Sender: TObject);
    procedure MegaCCOnlineManualActionExecute(Sender: TObject);
    procedure MEGAWebsiteActionExecute(Sender: TObject);
    procedure ConstructMETreeActionExecute(Sender: TObject);
    procedure ModelsMenuImgMouseEnter(Sender :TObject);
    procedure ModelsMenuImgMouseLeave(Sender :TObject);
    procedure ModelsToolMenuItemClick(Sender: TObject);
    procedure MolecularClockTestActionExecute(Sender: TObject);
    procedure ConstructNJTreeActionExecute(Sender: TObject);
    procedure NetMeanActionExecute(Sender: TObject);
    procedure DisplayNewickTreeActionExecute(Sender: TObject);
    procedure NucleotideCompActionExecute(Sender: TObject);
    procedure OnlineManualActionExecute(Sender: TObject);
    procedure OpenAlignmentActionExecute(Sender: TObject);
    procedure OpenFileActionExecute(Sender: TObject);
    procedure OpenTextEditorActionExecute(Sender: TObject);
    procedure OverallMeanDistanceActionExecute(Sender: TObject);
    procedure PairwiseDistanceActionExecute(Sender: TObject);
    procedure ConstructMPTreeActionExecute(Sender: TObject);
    procedure PatternDisparityActionExecute(Sender: TObject);
    procedure PhylogenyMenuImgMouseEnter(Sender :TObject);
    procedure PhylogenyMenuImgMouseLeave(Sender :TObject);
    procedure PhylogenyToolMenuItemClick(Sender: TObject);
    procedure PositionByPositionRatesMLActionExecute(Sender: TObject);
    procedure PredictLivingSequenceActionExecute(Sender: TObject);
    procedure PreferencesBtnMouseEnter(Sender: TObject);
    procedure PreferencesBtnMouseLeave(Sender: TObject);
    procedure PrototypeBtnClick(Sender: TObject);
    procedure PrototypeBtnMouseEnter(Sender: TObject);
    procedure PrototypeBtnMouseLeave(Sender: TObject);
    procedure QueryDatabankActionExecute(Sender: TObject);
    procedure RatesMenuImgMouseEnter(Sender :TObject);
    procedure RatesMenuImgMouseLeave(Sender :TObject);
    procedure RatesToolMenuItemClick(Sender: TObject);
    procedure ReltimeBlensActionExecute(Sender: TObject);
    procedure SaveDataActionExecute(Sender: TObject);
    procedure SelectGeneDomainActionExecute(Sender: TObject);
    procedure SelectGeneticCodeActionExecute(Sender: TObject);
    procedure SelectionMenuImgMouseEnter(Sender :TObject);
    procedure SelectionMenuImgMouseLeave(Sender :TObject);
    procedure SelectionToolMenuItemClick(Sender: TObject);
    procedure SelectTaxaGroupActionExecute(Sender: TObject);
    procedure SetIsDeveloperBtnClick(Sender: TObject);
    procedure ShowBrowserActionExecute(Sender: TObject);
    procedure SLACActionExecute(Sender: TObject);
    procedure SubstitutionMatrixActionExecute(Sender: TObject);
    procedure TajimaRateTestActionExecute(Sender: TObject);
    procedure TajimaNeutralityTestActionExecute(Sender: TObject);
    procedure TimetreeWebsiteActionExecute(Sender: TObject);
    procedure RatesMenuBtnClick(Sender: TObject);
    procedure ClocksMenuBtnClick(Sender: TObject);
    procedure DiagnoseMenuBtnClick(Sender: TObject);
    procedure LinksBtnClick(Sender: TObject);
    procedure AlignMenuBtnClick(Sender: TObject);
    procedure ToolBarBtnClick(Sender: TObject);
    procedure PreferencesBtnClick(Sender: TObject);
    procedure DataMenuBtnClick(Sender: TObject);
    procedure ModelsMenuBtnClick(Sender: TObject);
    procedure DistanceMenuBtnClick(Sender: TObject);
    procedure DiversityMenuBtnClick(Sender: TObject);
    procedure PhylogenyMenuBtnClick(Sender: TObject);
    procedure ToolbarBtnMouseEnter(Sender: TObject);
    procedure ToolbarBtnMouseLeave(Sender: TObject);
    procedure TutorialActionExecute(Sender: TObject);
    procedure UpdatesBtnClick(Sender: TObject);
    procedure UpdatesBtnMouseEnter(Sender: TObject);
    procedure UpdatesBtnMouseLeave(Sender: TObject);
    procedure UrlButtonClick(Sender: TObject);
    procedure UserTreeMenuBtnClick(Sender: TObject);
    procedure AncestorsMenuBtnClick(Sender: TObject);
    procedure SelectionMenuBtnClick(Sender: TObject);
    procedure TransitionBiasActionExecute(Sender: TObject);
    procedure OpenTreeSessionActionExecute(Sender: TObject);
    procedure UpdateToolBarActionExecute(Sender: TObject);
    procedure ConstructUPGMATreeActionExecute(Sender: TObject);
    procedure UserTreeMenuImgMouseEnter(Sender :TObject);
    procedure UserTreeMenuImgMouseLeave(Sender :TObject);
    procedure UserTreeToolMenuItemClick(Sender: TObject);
    procedure WithinMeanDistanceActionExecute(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure PopupNotifierClosed(Sender: TObject; var CloseAction: TCloseAction);
    procedure JobQueueMenuItemClick(Sender: TObject);
    procedure AppendToJobQueue(aJob: String);
    procedure UpdateJobQueueList(aJob: String);
    procedure InitJobQueue;
    procedure OpenSLACWindow;
    procedure SLACWindowClosed(Sender: TObject; var CloseAction: TCloseAction);
  private
     FExplorersDisabledForLargeData: Boolean;
     FIsClosing: Boolean;
     FClosePopup: Boolean;
     {$IFNDEF DEBUG}
     FUniqueInstance: TUniqueInstance;
     {$ENDIF}
     FMainMenuHeight: Integer;
     b1, b2: TBevel;
     FDragging: Boolean;
     FLastPos: TPoint;
     FPopupMenus: TList;
     FMainMenuBgColor: TColor;
     FMainMenuFontColor: TColor;

     FMenuItemBgColor: TColor;
     FMenuItemActiveBgColor: TColor;
     FMenuItemFontColor: TColor;
     FMenuItemActiveFontColor: TColor;
     FCodeTableName: String;
     FCodeTable: String;
     FRuntimeProgress: TRuntimeProgress;
     FRecentlyUsedFiles: TRecentlyUsedFilesList;
     FJobQueueList: TJobQueueList;
     IsPreviewingButton: TToolButton;
     {$IFNDEF NO_BROWSER}
     TimeTreeWizard: TTimeTreeWizard;
     TimeTreeWizardForm: TTimeTreeWizardForm;
     AnalysisWizard: TAnalysisWizard;
     AnalysisWizardForm: TAnalysisWizardForm;
     {$ENDIF}
     FDownloadUrl: String;
     FAppLinkOptionsManager: TApplinkOptionsManager;
     FDataFileName: String;
     FPreviousPrototyperIndex: Integer;
     FDataType :TSnTokenCode;
     FDataTitle: String;
     FDataFileDescription: String;
     FMultithreadedCalcIsRunning: Boolean;
     FNumSingleThreadedMLAnalyses: Integer;
     FGDWizard: TGDWizardForm;

     FTopToolbarImages: array[0..11] of TImage;
     FUserRequestedUpdateCheck: Boolean;
     FCommandLineParamsProcessed: Boolean;
     FPopupNotifier: TPopupNotifier;
     FTimer: TTimer;
     FMenuBitmap: TBitmap;
     FCheckmark: TBitmap;
     FNumSessionTests: Integer;
     FSessionTestCriticalSection: TCriticalSection;
     procedure RunSessionFileTests;
     procedure RunMasxFileTest;
     procedure RunMdsxFileTest;
     procedure RunMtsxFileTest;
     procedure AppendSessionLog(aMsg: String);
     procedure SetDataType(AValue: TSnTokenCode);
     procedure SetExplorersDisabledForLargeData(AValue: Boolean);
     procedure SetPrototyperModeNoPrompt;
     procedure ProcessCommandLineParams;
     function ProcessFile(filename: String): Boolean;
     procedure OnOtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
     function IsTrayForm(aForm: TForm): Boolean;
     procedure AlignTopToolbarButtons;
     procedure InitVersionLabel;
     function TopToolbarMenuYCoord: Integer;
     procedure InitMenus;
     procedure InitBevels;
     function MenuHeight(aMenu: TMenu): Integer;
     procedure InitMenuImages;
     procedure CheckHelpFilesDone(aThread: TObject);
     procedure CheckHelpFiles;
     procedure CheckWebDialogFilesDone(aThread: TObject);
     procedure CheckWebDialogFiles;
     procedure CleanUpTempFiles;
     function CheckCancel(Progress: Integer; Status: String): Boolean;
     procedure GeneDupsCancelled;
     procedure InitRecentlyUsedFiles;
     procedure RecentlyUsedFilesItemClick(Sender: TObject);
     procedure MegaGuiFormCreate;
     procedure MegaMacGuiFormCreate;
     procedure InitBetaMessage;
     procedure InitDeveloperMessage;
     procedure MyIdleFunction(Sender: TObject; var Done: Boolean);
     procedure OnNewickThreadDone(Thread: TObject);
     procedure LaunchUpdateThread(IsUserRequest: Boolean);
     procedure SetDataTitle(AValue: String);
     procedure SetMainMenuBgColor(AValue: TColor);
     procedure SetMainMenuFontColor(AValue: TColor);
     procedure UpdateThreadDone(Thread: TObject);
     procedure ResizeStatusBar;

     procedure ExportMuscleDnaJson;
     procedure ExportMuscleCodingJson;
     procedure ExportMuscleAminoJson;
     procedure ExportClustalDnaJson;
     procedure ExportClustalCodingJson;
     procedure ExportClustalAminoJson;
     procedure MegaProtoOnFormCreate;
     //procedure RotateText(ACanvas: TCanvas; R: TRect; aText: String; RotateType: TRotateType);
     //procedure DrawLabel;
     procedure SetDataFile(AValue: String);

     procedure EnableMenusForActiveDataType;
     procedure EnableMenusForRunMode;
     procedure SetProtoDataType(DataType: Integer);
     procedure UpdateRecentFilesList(AFile: String);
     procedure ClearData;
     procedure AssignContextHelpToMenus;
   //  procedure CheckForUpdates;
   public

     SLACWindow: TChromiumBrowser;
     containsCodingNuc : Boolean;
     BetaLabel: TLabel;
     SLACUrl: String;
     SLACJobID: String;
     SLACWindowIsShowing: Boolean;
     procedure SLACWindowNotify(Sender: TObject); { notify when closed so it can be set to nil}
     procedure DrawMenuItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
     procedure DrawPopupMenuItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
     procedure MeasureMenuItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
     procedure LaunchNucCompositionStatsThread(saveLocation: AnsiString; exportType: TExportType);
     procedure LaunchAminoAcidCompositionStatsThread(saveLocation: AnsiString; exportType: TExportType);
     procedure LaunchCodonUsageThread(saveLocation: AnsiString; exportType: TExportType);
     procedure LaunchStatPairFreqsThread(saveLocation: AnsiString; exportType: TExportType; isDirectional: Boolean);
     procedure CompositionStatsThreadDone(aThread: TObject);
     procedure LaunchNewickThread(Filename: String);
     procedure AddFilenameToStatusBar(aFilename: String);
     procedure ConcatenateFilesDone(aThread: TObject);
     procedure MCLCommandThreadDone(aThread: TObject);
     procedure DistCommandThreadDone(aThread: TObject);
     procedure DistTreeSearchThreadDone(aThread: TObject);
     procedure SiteCoverageThreadDone(aThread: TObject);
     function OpenFile(FFileName: String; FFileExt: String; OpenFrom: TOpenFrom): Boolean;
     procedure SetTheWizardFree;
     procedure SetTimeTreeWizardFree;
     procedure applyPrototyperVisualModifications;
     function GetDownloadLocation(LocalName: String): String;
     procedure NotifyNoUpdateAvailable;
     procedure ConstructActiveNames(addToMe: TStringList);
     procedure AppendToRecentlyUsedFiles(aFilename: String);
     procedure SetRunMode(aMode: TMegaRunMode);

     procedure IQTreeDropdownItemClick(Sender: TObject);
       // for tray
     procedure HidePreviewWindow;
     procedure TrayButtonClick(Sender: TObject);
     procedure TrayToolbarButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
     procedure MagnifyTrayIcon(Btn: TToolButton);
     procedure AddWindowToTray(Form: TForm; ShowDataBtn: Boolean=True);
     procedure AddDropdownToTray(dropDown: TPopupMenu; imageIndex: Integer; aHint: String);
     procedure RemoveDropdownFromTray(dropdown: TPopupMenu);
     function GetFormIcon(form: TForm; var up: TBitmap; var hot: TBitmap): Boolean;
     function GetFormHintAndIconIndex(const form: TForm; var Index: Integer; var aHint: String): Boolean;
     procedure RemoveWindowFromTray(Form: TForm);
     function ResizeImage(Img: TBitmap; ImgWidth: Integer; ImgHeight: Integer): TBitmap;
     function WindowButtonInTray(Form: TForm): TToolButton;
     procedure UpdateWindowInTray(Form: TForm);

     procedure UpdateMainWindow(FileName : String = ''; Title : String = ''; Description: String = ''; FDType : TSnTokenCode  = snNucleotide);
      function  GetCodeTable: String;
      procedure SetCodeTable(AValue: String);
      function  GetCodeTableName: String;
      procedure SetCodeTableName(AName: String);
      function  GetDataFileName: String;
      function  GetDataTitle: String;
      function  GetDataDescr: String;
      procedure PromptCloseCurrentlyActive;
     procedure ToggleToolbarButtonVisiblity(Sender: TObject);
     procedure GetFileAndOpenData(DefExt: String; aNotify: TNotifyEvent = nil);  // def extension needs to be without a period
     function  AskUserToActivateDataFile(aNotify: TNotifyEvent = nil): Boolean;
     function AskUserToKeepDataActive: Boolean; { for operations that only require a tree(s) but info in the data may be useful (i.e. gene dup inferrence where the data has species names)}
     function  AskUserToSpecifyCodons: Boolean;

     procedure SelectDataSubsetPreferences;
     procedure SaveSession(PromptUserForLocation : boolean);
     function SaveSessionWithPermission: Boolean;  // checks for saving sessio, returns FALSE if CANCEL
     function HasActiveData: Boolean;
     function HasSequenceData: Boolean;
     function HasDistanceData: Boolean;
     function HasMultiFileData: Boolean;
     function HasNucleotideData: Boolean;
     function HasAminoAcidData: Boolean;
     function HasCodonData: Boolean;

     function HasDataSubsetChanged: Boolean;
     function hasDataViewChanged: Boolean;
     function GetCitationHtmlString: String;
     function DataTypeString: String;
     function DataFileNameOnly: String;
     procedure TajimaNeutralityTestDone(aThread: TObject);
     procedure TajimaClockTestDone(aThread: TObject);
     procedure AddSettingsToPopupMenu(aMenu: TPopupMenu);
     procedure UpdateCollectDataPreference(Sender: TObject);
     procedure SessionTestFinished(Sender: TObject);
     property MenuBackgroundColor: TColor read FMenuItemBgColor;
     property MenuItemActiveColor: TColor read FMenuItemActiveBgColor;
     property MenuItemDefaultFontColor: TColor read FMenuItemFontColor;
     property MenuItemActiveFontColor: TColor read FMenuItemActiveFontColor;
     property MainMenuBgColor: TColor read FMainMenuBgColor write SetMainMenuBgColor;
     property MainMenuFontColor: TColor read FMainMenuFontColor write SetMainMenuFontColor;
     property ExplorersDisabledForLargeData: Boolean read FExplorersDisabledForLargeData write SetExplorersDisabledForLargeData;
   published
     property  CodeTable:       String  read GetCodeTable     write SetCodeTable;
     property  CodeTableName:   String  read GetCodeTableName write SetCodeTableName;
     property  DataFileName:    String  read FDataFileName write SetDataFile;

     property  DataTitle:       String  read GetDataTitle write SetDataTitle;
     property  DataDescription: String  read GetDataDescr;
     property  MultithreadedCalcIsRunning: Boolean read FMultithreadedCalcIsRunning write FMultithreadedCalcIsRunning;
     property  NumSingleThreadedMLAnalyses: Integer read FNumSingleThreadedMLAnalyses write FNumSingleThreadedMLAnalyses;
     property DataType: TSnTokenCode read FDataType write SetDataType;
  end;
  {$IFDEF DEBUG}
  procedure WriteToMegaLog(aMsg: String);
  {$ENDIF}

var
  MegaForm: TMegaForm;
  MegaRunMode: TMegaRunMode;

implementation

uses
  {$IFNDEF DEBUG}
  cef3types,
  {$ENDIF}
  MTreeInputForm, MTreeViewForm, mtajimaneutralitytestthread, MAnalysisSummary,
  LCLIntf, LazFileUtils, MAnalysisInfo, MLegendGenerator,
  types, MD_InputSeqData, MProcessPack, MegaUtils, strutils,
  StringUtils, ProcessInputData, ShowTrees, MEditorForm, mtajimaclocktestthread,
  MGeneDomainDlg, MTaxaGpsDlg, MOtuInfo, MSelectGeneticCodeDlg,
  MVS_SeqDataExplorer, MV_SeqDataExplorer, MV_DistDataExplorer,
  MVS_DistDataExplorer, MD_InputDistData, MegaMainPreferences,
  MutationDetailView, manalysissettings, MegaAnalysisPrefStrings, MLoadTreeThread,
  LResources, MegaPrivateFiles, mupdates, mupdatesdlg,
  MTreeList, MAlignEditMainForm, MegaErrUtils,  ContextHelp_HC,
  MExampleFiles, Printers, mdistpack, math, ExcelWrite,
  MMegaWindowInfo, alignmentconcatenator, maboutbox, MWriteOutputDlg, MTraceEditForm,
  mhelpfiles, TextEditorHelp_HC, DataExplorerHelp_HC, htmloptionsdlg,
  mwebdialogfiles, MMdWelcomeForm, MCalibrationDlg, MCalibrationData, Walk_Through_MEGA,
  cef3lib, cef3lcl, mshortcutshelper, MUsageStatistics,
  protodatatypeform, mversioninfo, clone_finder_main_form, mdrawmenuitems,
  mappdepot_dlg, mappflows_form, mdist_command_finalize, mdist_command_threads,
  mcl_command_threads, mcomposition_stats_thread, MPleaseWait, msitecoverage,
  mtree_display_setup, mega_citation, mega_info_form;

{$R *.lfm}

{$IFDEF DEBUG}
procedure WriteToMegaLog(aMsg: String);
var
  aFile: TextFile;
begin
  AssignFile(aFile, DEBUG_FILE);
  if FileExists(DEBUG_FILE) then
    Append(aFile)
  else
    Rewrite(aFile);
  try
    WriteLn(aFile, aMsg);
  finally
    CloseFile(aFile);
  end;
end;
{$ENDIF}

{ TMegaForm }

procedure TMegaForm.OpenAlignmentActionExecute(Sender: TObject);
begin
  try
    FindAlignmentEditorWindow(False);
    AlignEditMainForm.OpenAlignmentSessionItemClick(nil);
    if not AlignEditMainForm.Visible then
    begin
      AlignEditMainForm.Free;
      AlignEditMainForm := nil;
    end;
  Except
    on E: Exception do
    begin
      if Assigned(AlignEditMainForm) then
      begin
        RemoveWindowFromTray(AlignEditMainForm);
        FreeAndNil(AlignEditMainForm);
      end;
      ShowMessage('Oh no! An error occurred when activating the Alignment Explorer: ' + E.Message);
    end;
  end;
end;

procedure TMegaForm.OpenFileActionExecute(Sender: TObject);
var
  AFileName, AFileExt: String;
  MyOpenDlg: TOpenDialog;
begin
  if IsPrototyper then
    Exit;
  try
    MyOpenDlg := TOpenDialog.Create(Self);
    MyOpenDlg.Title       := 'Open a File';
    MyOpenDlg.Filter := 'All Files|*.*|MEGA Files|*.meg';
    MyOpenDlg.Options     := [ofShowHelp,ofPathMustExist,ofFileMustExist];
    MyOpenDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(MyOpenDlg.InitialDir);
    if not MyOpenDlg.Execute then
      Exit;
    AFileName := MyOpenDlg.FileName;
    AFileExt := lowercase(ExtractFileExt(AFileName));
    OpenFile(AFileName, AFileExt, OOther);
  Except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when opening the data file: ' + E.Message);
  end;
end;

procedure TMegaForm.OpenTextEditorActionExecute(Sender: TObject);
begin
  try
    FindTextEditorWindow(True);
  Except
    on E: Exception do
      ShowMessage('Oh no! An error occurred: ' + E.Message);
  end;
end;

procedure TMegaForm.OverallMeanDistanceActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(OverallMeanDistanceActionExecute) then
    Exit;
  if not(HasSequenceData OR HasDistanceData) then
  begin
    ShowMessage('Distance computation requires sequence or pairwise distance data.');
    Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppDistEst);
      ProcessPack.AddProcessType(ppOverallMean);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.PairwiseDistanceActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(PairwiseDistanceActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
    ShowMessage('Distance calculations require sequence data.');
    Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppDistEst);
      ProcessPack.AddProcessType(ppPairTaxa);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.ConstructMPTreeActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(ConstructMPTreeActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
     ShowMessage('Maximum Parsimony Analysis requires sequence data.');
     Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppInfer);
      ProcessPack.AddProcessType(ppMP);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.PatternDisparityActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(PatternDisparityActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
    ShowMessage('Disparity index calculations require sequence data.');
    Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppEstDispIndex);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.PhylogenyMenuImgMouseEnter(Sender :TObject);
begin
  Hover72x90.GetBitmap(MEN_IMG_PHYLOGENY, PhylogenyMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.PhylogenyMenuImgMouseLeave(Sender :TObject);
begin
  Default72x90.GetBitmap(MEN_IMG_PHYLOGENY, PhylogenyMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.PhylogenyToolMenuItemClick(Sender: TObject);
begin
  PhylogenyToolMenuItem.Checked := not PhylogenyToolMenuItem.Checked;
  PhylogenyMenuImg.Visible := PhylogenyToolMenuItem.Checked;
  AlignTopToolbarButtons;
end;

procedure TMegaForm.PositionByPositionRatesMLActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(PositionByPositionRatesMLActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
     ShowMessage('Maximum Likelihood tree construction requires sequence data.');
     Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppEstRateBySite);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.PredictLivingSequenceActionExecute(Sender: TObject);
var
  ProcessPack : TProcessPack;
begin
  if not AskUserToActivateDataFile(PredictLivingSequenceActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
     ShowMessage('Predict Living Sequence uses Maximum Likelihood Analysis which requires sequence data.');
     Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppAncSeq);
      ProcessPack.AddProcessType(ppML);
      ProcessPack.AddProcessType(ppPredictLivingSeq);
      ProcessPack.PerformAnalysis;
    Except
    on E: Exception do
      ShowMessage('Oh no! Living sequence prediction failed: ' + E.Message);
    end;
  finally
    ProcessPack.Free;
  end;
end;

procedure TMegaForm.PreferencesBtnMouseEnter(Sender: TObject);
begin
  BtRed.GetBitmap(BT_IMG_PREFERENCES, PreferencesBtn.Picture.Bitmap);
end;

procedure TMegaForm.PreferencesBtnMouseLeave(Sender: TObject);
begin
  BtBlue.GetBitmap(BT_IMG_PREFERENCES, PreferencesBtn.Picture.Bitmap);
end;

procedure TMegaForm.PrototypeBtnClick(Sender: TObject);
begin
  SetRunMode(mrmPrototyper);
end;

procedure TMegaForm.PrototypeBtnMouseEnter(Sender: TObject);
begin
  RunModeImages.GetBitmap(4, PrototypeBtn.Picture.Bitmap);
end;

procedure TMegaForm.PrototypeBtnMouseLeave(Sender: TObject);
begin
  if MegaRunMode = mrmPrototyper then
    RunModeImages.GetBitmap(5, PrototypeBtn.Picture.Bitmap)
  else
    RunModeImages.GetBitmap(3, PrototypeBtn.Picture.Bitmap);
end;

procedure TMegaForm.QueryDatabankActionExecute(Sender: TObject);
var
    aBrowser : TChromiumBrowser;
begin
  try
    aBrowser := CreateNewChromiumBrowserWindow(bmBrowser, Self);
    aBrowser.QueryGene(true);
  Except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when displaying the web browser: ' + E.Message);
  end;
end;

procedure TMegaForm.RatesMenuImgMouseEnter(Sender :TObject);
begin
  Hover72x90.GetBitmap(MEN_IMG_RATES, RatesMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.RatesMenuImgMouseLeave(Sender :TObject);
begin
  Default72x90.GetBitmap(MEN_IMG_RATES, RatesMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.RatesToolMenuItemClick(Sender: TObject);
begin
  RatesToolMenuItem.Checked := not RatesToolMenuItem.Checked;
  RatesMenuImg.Visible := RatesToolMenuItem.Checked;
  AlignTopToolbarButtons;
end;

procedure TMegaForm.ReltimeBlensActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack = nil;
begin
  try
    try
      if IsPrototyper then
      begin
        ProcessPack := TProcessPack.Create;
        if (Sender = ReltimeBlensMenuItem) or (Sender = ReltimeBLensMenu2) then
          ProcessPack.AddProcessType(ppRelTimeBlens)
        else if (Sender = RtdtBlensMenuItem) or (Sender = RtdtBlensMenu2) then
          ProcessPack.AddProcessType(ppRtdtBlens)
        else
          raise Exception.Create('invalid caller for Reltime branch lengths action');
        ProcessPack.PerformAnalysis;
      end
      else
      begin
        if Assigned(TimeTreeWizard) then
          FreeAndNil(TimeTreeWizard);
        TimeTreeWizard := TTimeTreeWizard.Create;
        if Assigned(TimeTreeWizardForm) then
          FreeAndNil(TimeTreeWizardForm);
        TimeTreeWizardForm := TTimeTreeWizardForm.Create(Self);
        TimeTreeWizardForm.Wizard := TimeTreeWizard;
        TimeTreeWizard.WizardFormUpdate := TimeTreeWizardForm.UpdateView;
        AddWindowToTray(TimeTreeWizardForm, False);
        ProcessPack := TProcessPack.Create;
        if (Sender = ReltimeBlensMenuItem) or (Sender = ReltimeBLensMenu2) then
        begin
          ProcessPack.AddProcessType(ppRelTimeBlens);
          TimeTreeWizard.UsrOperation := dtdoRelTimeBLens;
        end
        else if (Sender = RtdtBlensMenuItem) or (Sender = RtdtBlensMenu2) then
        begin
          ProcessPack.AddProcessType(ppRtdtBlens);
          TimeTreeWizard.UsrOperation := dtdoRtdtBLens;
        end
        else
          raise Exception.Create('invalid caller for Reltime branch lengths action');
        TimeTreeWizard.ProcessPack := ProcessPack;
        TimeTreeWizard.WizardMode := ttwmBlens;
        ProcessPack := nil;
        if IsGlenDevMode then
          TimeTreeWizardForm.Show
        else
        begin
          {$IFDEF WINDOWS}
          TimeTreeWizardForm.ShowModal;
          {$ELSE}
          TimeTreeWizardForm.Show;
          {$ENDIF}
        end;
        TimeTreeWizardForm := nil; { freed automatically when setting CloseAction := caFree in OnClose}
        {$IFDEF DEBUG}
        if IsGlenDevMode then
        begin
          TimeTreeWizard.LoadTree;
          //TimeTreeWizard.GetCalibrations;
        end;
        {$ENDIF}
      end;
    Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.SaveDataActionExecute(Sender: TObject);
begin
  try
    SaveSession(True); //Save the session and prompt the user where to save
  Except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when saving data: ' + E.Message);
  end;
end;

procedure TMegaForm.SelectGeneDomainActionExecute(Sender: TObject);
begin
  if (not HasActiveData) or (GeneDomainDlg = nil) then
    Exit;

  with GeneDomainDlg do
  begin
    ShowModal;
    if D_InputSeqData.DomainMarks.IsDirty then
    begin
      try
        if V_SeqDataExplorer <> nil then
          V_SeqDataExplorer.ApplyUpdate;
        D_InputSeqData.DomainMarks.IsDirty := False;
      except
	on E: Exception do
	  ShowMessage('Oh no! An error has occurred: ' + E.Message);
      end
    end;
  end;
end;

procedure TMegaForm.SelectGeneticCodeActionExecute(Sender: TObject);
var
  MyDlg: TSelectGeneticCodeDlg = nil;
  ACodeTableName: String;
begin
  if not HasActiveData then
  begin
    ShowMessage('Please load a sequence alignment in order to select a genetic code table');
    Exit;
  end;

  try
    try
      MyDlg := TSelectGeneticCodeDlg.Create(Self);
      if CodeTableName <> EmptyStr then
        ACodeTableName :=  CodeTableName
      else
        ACodeTableName := 'Standard';
      MyDlg.CodeTableName := ACodeTableName;
      if MyDlg.ShowModal <> mrOK then
        MyDlg.CodeTableName := ACodeTableName; // I must force it

      // this is important as the user may have changed the codetable itself
      CodeTableName := MyDlg.CodeTableName;
      CodeTable     := MyDlg.CodeTable;
      if V_SeqDataExplorer <> nil then
      begin
        D_InputSeqData.CodeName  := MyDlg.CodeTableName;
        D_InputSeqData.CodeTable := MyDlg.CodeTable;
        VS_SeqDataExplorer.UpdateCodeTable;
      end;
    Except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when selecting a genetic code: ' + E.Message);
    end;
  finally
    if Assigned(MyDlg) then
      MyDlg.Free;
  end;
end;

procedure TMegaForm.SelectionMenuImgMouseEnter(Sender :TObject);
begin
  Hover72x90.GetBitmap(MEN_IMG_SELECTION, SelectionMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.SelectionMenuImgMouseLeave(Sender :TObject);
begin
  Default72x90.GetBitmap(MEN_IMG_SELECTION, SelectionMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.SelectionToolMenuItemClick(Sender: TObject);
begin
  SelectionToolMenuItem.Checked := not SelectionToolMenuItem.Checked;
  SelectionMenuImg.Visible := SelectionToolMenuItem.Checked;
  AlignTopToolbarButtons;
end;

procedure TMegaForm.SelectTaxaGroupActionExecute(Sender: TObject);
begin
  if not HasActiveData then
    Exit;
  try
    if TaxaGpsDlg = nil then
      Exit;
    if Assigned(V_SeqDataExplorer) then
      TaxaGpsDlg.GroupTagSourceComboBox.ItemIndex := V_SeqDataExplorer.GroupTagCheckedIndex;
    with TaxaGpsDlg do
    begin
      ShowModal;
      if OtuInfo.IsDirty then
      begin
          if V_SeqDataExplorer <> nil  then
            V_SeqDataExplorer.ApplyUpdate
          else if V_DistDataExplorer <> nil then
            V_DistDataExplorer.ApplyUpdate;
          OtuInfo.IsDirty := False;
      end;
    end;
  Except
  on E: Exception do
    ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaForm.SetIsDeveloperBtnClick(Sender: TObject);
begin
  try
    IsDeveloper  := not IsDeveloper; // Make sure to change before release!
    if IsDeveloper then
    begin
      DeveloperLabel.Caption := 'NOTICE: This developer version is for in-house use only! Dev mode ON';
      //ReltimeLsAction.Enabled := True;
    end
    else
    begin
      DeveloperLabel.Caption := 'NOTICE: This developer version is for in-house use only! Dev mode OFF';
      //ReltimeLsAction.Enabled := False;
    end;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaForm.ShowBrowserActionExecute(Sender: TObject);
var
  aBrowser: TChromiumBrowser;
begin
  try
    aBrowser := CreateNewChromiumBrowserWindow(bmBrowser, Self, True);
    if Assigned(aBrowser) then
    begin
      aBrowser.Show;
      aBrowser.LoadPlaceholder('https://www.ncbi.nlm.nih.gov');
    end;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when launching the internal web browser: ' + E.Message);
  end;
end;

procedure TMegaForm.SLACActionExecute(Sender: TObject);
 var
  ProcessPack : TProcessPack;
begin
  if not AskUserToKeepDataActive then
    Exit;
  if (HasSequenceData and (not HasCodonData)) or (HasDistanceData) then
  begin
    ShowMessage('Codon selection analysis requires protein-coding sequence data. Please close the current data set to run this analysis.');
    Exit;
  end;
  try
    try
      if Assigned(AnalysisWizard) then
        FreeAndNil(AnalysisWizard);
      AnalysisWizard := TAnalysisWizard.Create;
      if Assigned(AnalysisWizardForm) then
        FreeAndNil(AnalysisWizardForm);
      AnalysisWizardForm := TAnalysisWizardForm.Create(Self);
      AnalysisWizardForm.Wizard := AnalysisWizard;
      AnalysisWizardForm.LoadTreePanel.Visible:=False;
      AnalysisWizardForm.LoadTreeAction.Enabled := False;
      AnalysisWizard.WizardFormUpdate := AnalysisWizardForm.UpdateView;
      AddWindowToTray(AnalysisWizardForm, HasSequenceData);
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppEstimateSelectionCodons);
      AnalysisWizard.ProcessPack := ProcessPack;
      AnalysisWizard.UsrOperation := dtdoMLCodonOmega;
      AnalysisWizard.AnalysisType := ttaHyPhy;
      if HasSequenceData then
      begin
        AnalysisWizard.InitAnalysisInfo;
        AnalysisWizardForm.UpdateView(ttpsDoSettings);
      end;
      AnalysisWizardForm.ShowModal;
      AnalysisWizardForm := nil; { freed automatically when setting CloseAction := caFree in OnClose}
    Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    ProcessPack.Free;
  end;
end;

procedure TMegaForm.UpdateJobQueueList(aJob: String);
begin
  SLACJobID := aJob;
  AppendToJobQueue(aJob);
end;

procedure TMegaForm.InitJobQueue;
var
  storage: String;
  i: Integer;
  aMenuItem: TMenuItem;
begin
  storage := GetPrivateFile(mfJobQueueIDs, True);
  FJobQueueList := TJobQueueList.Create(Self, JobQueueMenuItemClick, storage);
  if FJobQueueList.Count > 0 then
  begin
    DataMonkeyJobResults.Visible := True;
    for i := 0 to FJobQueueList.Count - 1 do
    begin
      aMenuItem := TRecentlyUsedItem(FJobQueueList[i]).MenuItem;
      DatamonkeyJobResults.Add(aMenuItem);
    end;
  end;
end;

procedure TMegaForm.AppendToJobQueue(aJob: String);
var
  aMenuItem: TMenuItem;
begin
  if not FJobQueueList.HasFile(aJob) then
  begin
    DataMonkeyJobResults.Visible := True;
    aMenuItem := FJobQueueList.Add(aJob).MenuItem;
    DatamonkeyJobResults.Add(aMenuItem);
  end;
end;

procedure TMegaForm.OpenSLACWindow;
begin
  SLACWindow := TChromiumBrowser.Create(Self);
  SLACWindow.RunInDefaultWebBrowserMode;
    SLACWindow.Show;
  SLACWindow.Chromium.Load(SLACUrl);
  SLACWindowIsShowing := True;
  SLACWindow.OnCloseNotify := SLACWindowNotify;
end;

procedure TMegaForm.SLACWindowClosed(Sender: TObject; var CloseAction: TCloseAction);
begin
  SLACWindowIsShowing := False;
  SLACWindow.OnCloseNotify := SLACWindowNotify;
  SLACWindow := nil;
end;

procedure TMegaForm.SLACWindowNotify(Sender: TObject);
begin
  SLACWindow := nil;
  FreeAndNil(SLACWindow);
  SLACWindowIsShowing := False;
end;

procedure TMegaForm.JobQueueMenuItemClick(Sender: TObject);
begin
  Assert(Sender.ClassNameIs('TMenuItem'));
  if ((not Assigned(SLACWindow))OR(not SLACWindowIsShowing)) then
  begin
    with Sender as TMenuItem do
      SLACUrl := DatamonkeyUrl + '/' + Caption;
    SLACWindow := TChromiumBrowser.Create(Self);
    SLACWindow.RunInDefaultWebBrowserMode;
    SLACWindow.Show;
    SLACWindow.Chromium.Load(SLACUrl);
    SLACWindowIsShowing := True;
    SLACWindow.OnCloseNotify := SLACWindowNotify;
  end
  else
    SLACWindow.BringToFront;
end;

procedure TMegaForm.SubstitutionMatrixActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(SubstitutionMatrixActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
     ShowMessage('Maximum Likelihood tree construction requires sequence data.');
     Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppSubPatternEstML);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.TajimaRateTestActionExecute(Sender: TObject);
var
  ProcessPack : TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(TajimaRateTestActionExecute) then
    Exit;
  if not HasSequenceData then
   begin
    ShowMessage('Tajima''s relative rate test requires sequence data.');
    Exit;
  end;
  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppTajimaRelativeRate);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.TajimaNeutralityTestActionExecute(Sender: TObject);
var
  ProcessPack : TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(TajimaNeutralityTestActionExecute) then
    Exit;
 if not HasSequenceData then
  begin
    ShowMessage('Tajima''s test of neutrality require sequence data.');
    Exit;
  end;
  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppTajimaNeutrality);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.TimetreeWebsiteActionExecute(Sender: TObject);
begin
  try
    if not OpenUrl('http://timetree.org') then
      raise exception.Create('failed to open url: http://timetree.org');
  Except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TMegaForm.RatesMenuBtnClick(Sender: TObject);
var
  BtnPos: TPoint;
begin
  BtnPos.X := RatesMenuImg.Left;
  BtnPos.Y := TopToolbarMenuYCoord + 2;
  BtnPos := ClientToScreen(BtnPos);
  RatesPopupMenu.Popup(BtnPos.X, BtnPos.Y);
end;

procedure TMegaForm.ClocksMenuBtnClick(Sender: TObject);
var
  BtnPos: TPoint;
begin
  BtnPos.X := ClocksMenuImg.Left;
  BtnPos.Y := TopToolbarMenuYCoord + 2;
  BtnPos := ClientToScreen(BtnPos);
  ClocksPopupMenu.Popup(BtnPos.X, BtnPos.Y);
end;

procedure TMegaForm.DiagnoseMenuBtnClick(Sender: TObject);
var
  BtnPos: TPoint;
begin
  BtnPos.X := DiagnoseMenuImg.Left;
  BtnPos.Y := TopToolbarMenuYCoord + 2;
  BtnPos := ClientToScreen(BtnPos);
  DiagnosePopupMenu.Popup(BtnPos.X, BtnPos.Y);
end;

procedure TMegaForm.LinksBtnClick(Sender: TObject);
var
  BtnPos: TPoint;
begin
  BtnPos.X := LinksBtn.Left;
  BtnPos.Y := BottomToolbar.Top + TopToolbar.Height - FMainMenuHeight*LinksPopupMenu.Items.Count + FMainMenuHeight;
  BtnPos := ClientToScreen(BtnPos);
  LinksPopupMenu.Popup(BtnPos.X, BtnPos.Y);
end;

procedure TMegaForm.AlignMenuBtnClick(Sender: TObject);
var
  BtnPos: TPoint;
begin
  BtnPos.X := AlignMenuImg.Left;
  BtnPos.Y := TopToolbarMenuYCoord + 2;
  BtnPos := ClientToScreen(BtnPos);
  if IsPrototyper then
    AlignProtoPopupMenu.PopUp(BtnPos.X, BtnPos.Y)
  else
    AlignPopupMenu.Popup(BtnPos.X, BtnPos.Y);
end;

procedure TMegaForm.ToolBarBtnClick(Sender: TObject);
var
  MenuPos: TPoint;
begin
  MenuPos.X := ToolbarBtn.Left;
  MenuPos.Y := BottomToolbar.Top + TopToolbar.Height - FMainMenuHeight*ToolbarPopupMenu.Items.Count + FMainMenuHeight;
  MenuPos := ClientToScreen(MenuPos);
  ToolbarPopupMenu.PopUp(MenuPos.X, MenuPos.Y);
end;

procedure TMegaForm.PreferencesBtnClick(Sender: TObject);
var
  MenuPos: TPoint;
begin
  MenuPos.X := PreferencesBtn.Left;
  MenuPos.Y := BottomToolbar.Top + TopToolbar.Height - FMainMenuHeight*PreferencesPopupMenu.Items.Count + FMainMenuHeight;
  MenuPos := ClientToScreen(MenuPos);
  PreferencesPopupMenu.Popup(MenuPos.X, MenuPos.Y);
end;

procedure TMegaForm.DataMenuBtnClick(Sender: TObject);
var
  BtnPos: TPoint;
begin
  if IsPrototyper then
  begin
    ShowMessage('Data menu not available in PROTOTYPE mode. To unlock this feature, click the ANALYZE button');
    Exit;
  end;
  BtnPos.X := DataMenuImg.Left;
  BtnPos.Y := TopToolbarMenuYCoord + 2;
  BtnPos := ClientToScreen(BtnPos);
  DataPopupMenu.PopUp(BtnPos.X, BtnPos.Y);
end;

procedure TMegaForm.ModelsMenuBtnClick(Sender: TObject);
var
  BtnPos: TPoint;
begin
  BtnPos.X := ModelsMenuImg.Left;
  BtnPos.Y := TopToolbarMenuYCoord + 2;
  BtnPos := ClientToScreen(BtnPos);
  ModelsPopupMenu.PopUp(BtnPos.X, BtnPos.Y);
end;

procedure TMegaForm.DistanceMenuBtnClick(Sender: TObject);
var
  BtnPos: TPoint;
begin
  BtnPos.X := DistanceMenuImg.Left;
  BtnPos.Y := TopToolbarMenuYCoord + 2;
  BtnPos := ClientToScreen(BtnPos);
  DistancePopupMenu.Popup(BtnPos.X, BtnPos.Y);
end;

procedure TMegaForm.DiversityMenuBtnClick(Sender: TObject);
var
  BtnPos: TPoint;
begin
  BtnPos.X := DiversityMenuImg.Left;
  BtnPos.Y := TopToolbarMenuYCoord + 2;
  BtnPos := ClientToScreen(BtnPos);
  DiversityPopupMenu.Popup(BtnPos.X, BtnPos.Y);
end;

procedure TMegaForm.PhylogenyMenuBtnClick(Sender: TObject);
var
  BtnPos: TPoint;
begin
  BtnPos.X := PhylogenyMenuImg.Left;
  BtnPos.Y := TopToolbarMenuYCoord + 2;
  BtnPos := ClientToScreen(BtnPos);
  PhylogenyPopupMenu.Popup(BtnPos.X, BtnPos.Y);
end;

procedure TMegaForm.ToolbarBtnMouseEnter(Sender: TObject);
begin
  BtRed.GetBitmap(BT_IMG_TOOLBARS, ToolbarBtn.Picture.Bitmap);
end;

procedure TMegaForm.ToolbarBtnMouseLeave(Sender: TObject);
begin
  BtBlue.GetBitmap(BT_IMG_TOOLBARS, ToolbarBtn.Picture.Bitmap);
end;

procedure TMegaForm.TutorialActionExecute(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HC_Introduction_to_Walk_Through_MEGA));
end;

procedure TMegaForm.UpdatesBtnClick(Sender: TObject);
begin
  LaunchUpdateThread(True);
end;

procedure TMegaForm.UpdatesBtnMouseEnter(Sender: TObject);
begin
  BtRed.GetBitmap(BT_IMG_UPDATES, UpdatesBtn.Picture.Bitmap);
end;

procedure TMegaForm.UpdatesBtnMouseLeave(Sender: TObject);
begin
  BtBlue.GetBitmap(BT_IMG_UPDATES, UpdatesBtn.Picture.Bitmap);
end;

procedure TMegaForm.UrlButtonClick(Sender: TObject);
begin
  OpenURL(FDownloadUrl);
end;

procedure TMegaForm.UserTreeMenuBtnClick(Sender: TObject);
var
  BtnPos: TPoint;
begin
  BtnPos.X := UserTreeMenuImg.Left;
  BtnPos.Y := TopToolbarMenuYCoord + 2;
  BtnPos := ClientToScreen(BtnPos);
  UserTreePopupMenu.Popup(BtnPos.X, BtnPos.Y);
end;

procedure TMegaForm.AncestorsMenuBtnClick(Sender: TObject);
var
  BtnPos: TPoint;
begin
  BtnPos.X := AncestorsMenuImg.Left;
  BtnPos.Y := TopToolbarMenuYCoord + 2;
  BtnPos := ClientToScreen(BtnPos);
  AncestorsPopupMenu.Popup(BtnPos.X, BtnPos.Y);
end;

procedure TMegaForm.SelectionMenuBtnClick(Sender: TObject);
var
  BtnPos: TPoint;
begin
  BtnPos.X := SelectionMenuImg.Left;
  BtnPos.Y := TopToolbarMenuYCoord + 2;
  BtnPos := ClientToScreen(BtnPos);
  SelectionPopupMenu.Popup(BtnPos.X, BtnPos.Y);
end;

procedure TMegaForm.TransitionBiasActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(TransitionBiasActionExecute) then
    Exit;
  if not HasNucleotideData then
  begin
     ShowMessage('This Maximum Likelihood analysis requires DNA/RNA sequence data.');
     Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppTsTvML);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.OpenTreeSessionActionExecute(Sender: TObject);
begin
  try
    with ATreeOpenDlg do
    begin
      Title := 'Select MEGA Tree Session File';
      Filter :=  TREE_SESSION_FILE_FILTER;
      Options := Options + [ofFileMustExist];
      InitialDir := GetCurrentDir;
      if Execute then
      begin
        ShowTreeFromFile(FileName);
        SetCurrentDir(ExtractFileDir(FileName));
      end;
    end
  Except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when loading a tree session: ' + E.Message);
  end;
end;

procedure TMegaForm.UpdateToolBarActionExecute(Sender: TObject);
begin
  if not IsPrototyper then
  begin
    with Sender as TMenuItem do
      Checked := (not Checked);
    AlignMenuImg.Visible := AlignToolMenuItem.Checked;
    ModelsMenuImg.Visible := ModelsToolMenuItem.Checked;
    DistanceMenuImg.Visible := DistanceToolMenuItem.Checked;
    DiversityMenuImg.Visible := DiversityToolMenuItem.Checked;
    PhylogenyMenuImg.Visible := PhylogenyToolMenuItem.Checked;
    UserTreeMenuImg.Visible := UserTreeToolMenuItem.Checked;
    AncestorsMenuImg.Visible := AncestorsToolMenuItem.Checked;
    SelectionMenuImg.Visible := SelectionToolMenuItem.Checked;
    RatesMenuImg.Visible := RatesToolMenuItem.Checked;
    ClocksMenuImg.Visible := ClocksToolMenuItem.Checked;
    DiagnoseMenuImg.Visible := DiagnoseToolMenuItem.Checked;
  end;
end;

procedure TMegaForm.ConstructUPGMATreeActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(ConstructUPGMATreeActionExecute) then
    Exit;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppInfer);
      ProcessPack.AddProcessType(ppUPGMA);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.UserTreeMenuImgMouseEnter(Sender :TObject);
begin
  Hover72x90.GetBitmap(MEN_IMG_USERTREE, UserTreeMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.UserTreeMenuImgMouseLeave(Sender :TObject);
begin
  Default72x90.GetBitmap(MEN_IMG_USERTREE, UserTreeMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.UserTreeToolMenuItemClick(Sender: TObject);
begin
  UserTreeToolMenuItem.Checked := not UserTreeToolMenuItem.Checked;
  UserTreeMenuImg.Visible := UserTreeToolMenuItem.Checked;
  AlignTopToolbarButtons;
end;

procedure TMegaForm.WithinMeanDistanceActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(WithinMeanDistanceActionExecute) then
    Exit;
  if not (HasSequenceData OR HasDistanceData) then
  begin
    ShowMessage('Within Group average requires sequence or pairwise distance data.');
    Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppDistEst);
      ProcessPack.AddProcessType(ppWithinGpAvg);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.SetPrototyperModeNoPrompt;
var
  str1, str2, str3: String;
  aPos: TPoint;
begin
  MegaRunMode := mrmPrototyper;
  DataRecentlyUsedFilesSessionsItem.Visible := False;
  IsPrototyper := True;
  RunModeImages.GetBitmap(0, AnalyzeBtn.Picture.Bitmap);
  RunModeImages.GetBitmap(5, PrototypeBtn.Picture.Bitmap);
  SetProtoDataType(0);
  EnableMenusForRunMode;

  if not ClosePrototyperPopup then
  begin
    FPopupNotifier := TPopupNotifier.Create(Self);
    str1 := 'Running in Prototype mode';
    str2 := 'Click the "Analyze" button to unlock';
    str3 := 'all MEGA GUI features';
    FPopupNotifier.Text := str1 + LineEnding + str2 + LineEnding + str3;
    FPopupNotifier.vNotifierForm.Height := FPopupNotifier.vNotifierForm.Canvas.TextHeight(str1)*3 + 20 + GetSystemMetrics(SM_CYCAPTION);
    FPopupNotifier.vNotifierForm.Width := FPopupNotifier.vNotifierForm.Canvas.TextWidth(str2) + 40;
    FPopupNotifier.Title:=EmptyStr;
    FPopupNotifier.Color:=clWhite;
    FPopupNotifier.vNotifierForm.BorderWidth := 0;
    FPopupNotifier.vNotifierForm.OnClose:=PopupNotifierClosed;
    aPos.y := Top + Height - FPopupNotifier.vNotifierForm.Height - 40;
    aPos.x := Left + Width - FPopupNotifier.vNotifierForm.Width;
    FPopupNotifier.ShowAtPos(aPos.x, aPos.y);
    FTimer := TTimer.Create(Self);
    FTimer.OnTimer := OnTimer;
    FTimer.Interval := 15000;
    FTimer.Enabled := True;
  end;
end;

procedure TMegaForm.PopupNotifierClosed(Sender: TObject; var CloseAction: TCloseAction);
begin
  FTimer.Enabled := False;
  FClosePopup := False;
  if (MessageDlg('Would you like to see the Protoyper mode notification again?', mtConfirmation, [mbYes, mbNo], 0) in [mrYes]) then
  begin
    FClosePopup := False;
    UpdatePreferencesMainMenu(UserPref_CloseProtoyperPopupStr, FClosePopup, BoolToStr(False, True));
  end
  else
  begin
    FClosePopup := True;
    UpdatePreferencesMainMenu(UserPref_CloseProtoyperPopupStr, FClosePopup, BoolToStr(True, True));
  end;
  SaveMainMenuPreferencesOnDestroy;
end;

procedure TMegaForm.ProcessCommandLineParams;
var
  aFile: String;
  mvi: TMegaVersionInfo = nil;
begin
  if FCommandLineParamsProcessed then
    Exit;
  try
    FCommandLineParamsProcessed := True;
    if ParamCount > 0 then
    begin
      if ParamStr(1) = GET_VERSION_INFO then
      begin
        try
          aFile := ParamStr(2);
          mvi := TMegaVersionInfo.Create;
          if not mvi.VersionInfoToFile(aFile) then
            ShowMessage('Failed to write version info to file');
        finally
          if Assigned(mvi) then
            mvi.Free;
        end;
        Halt(0);
      end
      else if ParamStr(1) = SESSION_TEST then
      begin
        if IsPrototyper then
          SetRunMode(mrmGui);
        RunSessionFileTests;
      end
      else
      begin
        aFile := ParamStr(1);
        if (ParamCount > 1) and (ParamStr(2) = SESSION_TEST) then
        begin
          {$IFDEF DEBUG}
          raise Exception.Create('the session test can only be run using a RELEASE build');
          {$ENDIF}
          IsSessionTest := True;
          IsLegacySessionTest := True;
        end;
        if (not ProcessFile(aFile)) and IsSessionTest then
          AppendSessionLog(Format('%s : FAILED test for old session file: %s', [FormatDateTime('YYYY/MM/DD - hh:nn:ss', Now), aFile]))
        else if IsSessionTest then
          AppendSessionLog(Format('%s : Successful test for old session file: %s', [FormatDateTime('YYYY/MM/DD - hh:nn:ss', Now), aFile]));
        if IsSessionTest then
          Halt(0);
      end;
    end;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when processing command line parameters: ' + E.Message);
  end;
end;

function TMegaForm.ProcessFile(filename: String): Boolean;
var
  aExt: String;
begin
  Result := False;
  if FileExists(filename) then
  begin
    aExt := LowerCase(ExtractFileExt(filename));
    if Pos(aExt, NewickExts) > 0 then
      LaunchNewickThread(filename)
    else if Pos(aExt, FastaExts) > 0 then
      Result := OpenFile(filename, aExt, OOther)
    else
    begin
      PromptCloseCurrentlyActive;
      if HasActiveData then // check if the use has bailed
        Exit;
      Result := True;
      if Pos(aExt, MegaExts) > 0 then
        Result := DoOpenDataFile(filename)
      else if Pos(aExt, TreeSessionExts) > 0 then
        Result := (ShowTreeFromFile(fileName) <> nil)
      else if Pos(aExt, MASExts) > 0 then
        Result := (ShowAlignmentFromFile(filename) <> nil);
    end;
  end;
end;

procedure TMegaForm.OnOtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
var
  temp: String;
begin
  if ParamCount > 0 then
  begin
   temp := Parameters[0];
   if FileExists(temp) then
     ProcessFile(temp)
   else
   begin
     {$IFDEF DEBUG}
     ShowMessage('Invalid call to OnOtherInstance: ' + Parameters[0]);
     {$ENDIF}
   end;
  end;
end;

function TMegaForm.IsTrayForm(aForm: TForm): Boolean;
begin
  if (aForm.ClassType = TV_SeqDataExplorer) or
     (aForm.ClassType = TTreeInputForm) or
     (aForm.ClassType = TCalibrationDlg) or
     (aForm.ClassType = TAnalysisWizardForm) or
     (aForm.ClassType = TTimeTreeWizardForm) or
     (aForm.ClassType = TMegaInfoForm) or
     (aForm.ClassType = TV_DistDataExplorer) then
    Result := True
  else
    Result := False;
end;

procedure TMegaForm.AlignTopToolbarButtons;
var
  leftCoord: Integer;
  i: Integer;
  aImage: TImage;
begin
  leftCoord := 0;
  for i := 0 to Length(FTopToolbarImages) - 1 do
  begin
    aImage := FTopToolbarImages[i];
    if aImage.Visible then
    begin
      aImage.Left := leftCoord;
      leftCoord := leftCoord + aImage.Width - 1;
    end;
  end;
  TopToolbarEndCap.Left := leftCoord;
  TopToolbarEndCap.Width := ClientWidth - LeftCoord - 2;
end;

procedure TMegaForm .InitVersionLabel ;
begin
  VersionLabel.Caption := VERSION_LABEL_CAPTION;
  VersionLabel.Font.Color := clSilver;
  VersionLabel.Invalidate;
end;

function TMegaForm.TopToolbarMenuYCoord: Integer;
begin
  Result := TopToolbar.Top + TopToolbar.Height - 20;
end;

procedure TMegaForm.InitMenus;
begin
  FMainMenuHeight := TopToolbar.Canvas.TextHeight('File') + MENU_ITEM_MARGIN;
  AlignProtoPopupMenu.OwnerDraw := True;
  IQTreePopup.OwnerDraw := True;
  TimeTreeDBPopup.OwnerDraw := True;
  DatamonkeyPopup.OwnerDraw := True;
  AlignPopupMenu.OwnerDraw := True;
  DataPopupMenu.OwnerDraw := True;
  ModelsPopupMenu.OwnerDraw := True;
  DistancePopupMenu.OwnerDraw := True;
  DiversityPopupMenu.OwnerDraw := True;
  PhylogenyPopupMenu.OwnerDraw := True;
  UserTreePopupMenu.OwnerDraw := True;
  AncestorsPopupMenu.OwnerDraw := True;
  RatesPopupMenu.OwnerDraw := True;
  SelectionPopupMenu.OwnerDraw := True;
  ClocksPopupMenu.OwnerDraw := True;
  DiagnosePopupMenu.OwnerDraw := True;
  HelpMenu.OwnerDraw := True;
  LinksPopupMenu.OwnerDraw := True;
  FileMenu.OwnerDraw := True;
  HelpMenu.OwnerDraw := True;
  AnalysisMenu.OwnerDraw := True;
  ToolbarPopupMenu.OwnerDraw := True;
  PublicationsPopupMenu.OwnerDraw := True;
  HelpDocsPopupMenu.OwnerDraw := True;;
  {$IFNDEF DARWIN}MainMenu1.OwnerDraw := True;{$ENDIF}
  PreferencesPopupMenu.OwnerDraw := True;

  AlignProtoPopupMenu.OnDrawItem := DrawMenuItem;
  IQTreePopup.OnDrawItem := DrawMenuItem;
  TimeTreeDBPopup.OnDrawItem := DrawMenuItem;
  DatamonkeyPopup.OnDrawItem := DrawMenuItem;
  AlignPopupMenu.OnDrawItem := DrawMenuItem;
  DataPopupMenu.OnDrawItem := DrawMenuItem;
  ModelsPopupMenu.OnDrawItem := DrawMenuItem;
  DistancePopupMenu.OnDrawItem := DrawMenuItem;
  DiversityPopupMenu.OnDrawItem := DrawMenuItem;
  PhylogenyPopupMenu.OnDrawItem := DrawMenuItem;
  UserTreePopupMenu.OnDrawItem := DrawMenuItem;
  AncestorsPopupMenu.OnDrawItem := DrawMenuItem;
  RatesPopupMenu.OnDrawItem := DrawMenuItem;
  SelectionPopupMenu.OnDrawItem := DrawMenuItem;
  ClocksPopupMenu.OnDrawItem := DrawMenuItem;
  DiagnosePopupMenu.OnDrawItem := DrawMenuItem;
  HelpMenu.OnDrawItem := DrawMenuItem;
  LinksPopupMenu.OnDrawItem := DrawMenuItem;
  FileMenu.OnDrawItem := DrawMenuItem;
  HelpMenu.OnDrawItem := DrawMenuItem;
  AnalysisMenu.OnDrawItem := DrawMenuItem;
  ToolbarPopupMenu.OnDrawItem := DrawMenuItem;
  PublicationsPopupMenu.OnDrawItem := DrawMenuItem;
  HelpDocsPopupMenu.OnDrawItem := DrawMenuItem;
  {$IFNDEF DARWIN}MainMenu1.OnDrawItem := DrawMenuItem;{$ENDIF}
  PreferencesPopupMenu.OnDrawItem := DrawMenuItem;

  AlignProtoPopupMenu.OnMeasureItem := MeasureMenuItem;
  IQTreePopup.OnMeasureItem := MeasureMenuItem;
  TimeTreeDBPopup.OnMeasureItem := MeasureMenuItem;
  DatamonkeyPopup.OnMeasureItem := MeasureMenuItem;
  AlignPopupMenu.OnMeasureItem := MeasureMenuItem;
  DataPopupMenu.OnMeasureItem := MeasureMenuItem;
  ModelsPopupMenu.OnMeasureItem := MeasureMenuItem;
  DistancePopupMenu.OnMeasureItem := MeasureMenuItem;
  DiversityPopupMenu.OnMeasureItem := MeasureMenuItem;
  PhylogenyPopupMenu.OnMeasureItem := MeasureMenuItem;
  UserTreePopupMenu.OnMeasureItem := MeasureMenuItem;
  AncestorsPopupMenu.OnMeasureItem := MeasureMenuItem;
  RatesPopupMenu.OnMeasureItem := MeasureMenuItem;
  SelectionPopupMenu.OnMeasureItem := MeasureMenuItem;
  ClocksPopupMenu.OnMeasureItem := MeasureMenuItem;
  DiagnosePopupMenu.OnMeasureItem := MeasureMenuItem;
  HelpMenu.OnMeasureItem := MeasureMenuItem;
  LinksPopupMenu.OnMeasureItem := MeasureMenuItem;
  FileMenu.OnMeasureItem := MeasureMenuItem;
  HelpMenu.OnMeasureItem := MeasureMenuItem;
  AnalysisMenu.OnMeasureItem := MeasureMenuItem;
  ToolbarPopupMenu.OnMeasureItem := MeasureMenuItem;
  PublicationsPopupMenu.OnMeasureItem := MeasureMenuItem;
  HelpDocsPopupMenu.OnMeasureItem := MeasureMenuItem;
  {$IFNDEF DARWIN}MainMenu1.OnMeasureItem := MeasureMenuItem;{$ENDIF}
  PreferencesPopupMenu.OnMeasureItem := MeasureMenuItem;
end;

procedure TMegaForm.InitBevels;
begin
  b1 := TBevel.Create(Self);
  b1.Parent := Self;
  b1.Align := alNone;
  b1.Shape := bsLeftLine;
  b1.Width := 2;
  b1.Height := (Height - ClientHeight);
  b1.Top := 0;
  b1.Left := 0;
  b1.BringToFront;
  b2 := TBevel.Create(Self);
  b2.Parent := Self;
  b2.Align := alNone;
  b2.Shape := bsRightLine;
  b2.Width := 2;
  b2.Height := MenuHeight(MainMenu1);
  b2.top := 0;
  b2.Left := Width - 2;
  b2.BringToFront;
end;

function TMegaForm.MenuHeight(aMenu: TMenu): Integer;
begin
  Result := aMenu.Items.Count*FMainMenuHeight;
end;

procedure TMegaForm.InitMenuImages;
begin
  Default78x90.GetBitmap(MEN_IMG_ALIGN, AlignMenuImg.Picture.Bitmap);
  //AlignMenuImg.Invalidate;
  Default78x90.GetBitmap(MEN_IMG_DIAGNOSE, DiagnoseMenuImg.Picture.Bitmap);
  //DiagnoseMenuImg.Invalidate;
  Default72x90.GetBitmap(MEN_IMG_DATA, DataMenuImg.Picture.Bitmap);
  Default72x90.GetBitmap(MEN_IMG_MODELS, ModelsMenuImg.Picture.Bitmap);
  Default72x90.GetBitmap(MEN_IMG_DISTANCE, DistanceMenuImg.Picture.Bitmap);
  Default72x90.GetBitmap(MEN_IMG_DIVERSITY, DiversityMenuImg.Picture.Bitmap);
  Default72x90.GetBitmap(MEN_IMG_PHYLOGENY, PhylogenyMenuImg.Picture.Bitmap);
  Default72x90.GetBitmap(MEN_IMG_USERTREE, UserTreeMenuImg.Picture.Bitmap);
  Default72x90.GetBitmap(MEN_IMG_ANCESTORS, AncestorsMenuImg.Picture.Bitmap);
  Default72x90.GetBitmap(MEN_IMG_SELECTION, SelectionMenuImg.Picture.Bitmap);
  Default72x90.GetBitmap(MEN_IMG_RATES, RatesMenuImg.Picture.Bitmap);
  Default72x90.GetBitmap(MEN_IMG_CLOCKS, ClocksMenuImg.Picture.Bitmap);

  FTopToolbarImages[0] := AlignMenuImg;
  FTopToolbarImages[1] := DataMenuImg;
  FTopToolbarImages[2] := ModelsMenuImg;
  FTopToolbarImages[3] := DistanceMenuImg;
  FTopToolbarImages[4] := DiversityMenuImg;
  FTopToolbarImages[5] := PhylogenyMenuImg;
  FTopToolbarImages[6] := UserTreeMenuImg;
  FTopToolbarImages[7] := AncestorsMenuImg;
  FTopToolbarImages[8] := SelectionMenuImg;
  FTopToolbarImages[9] := RatesMenuImg;
  FTopToolbarImages[10] := ClocksMenuImg;
  FTopToolbarImages[11] := DiagnoseMenuImg;

  BtBlue.GetBitmap(BT_IMG_HELP, HelpBtn.Picture.Bitmap);
  BtBlue.GetBitmap(BT_IMG_EXAMPLES, ExamplesBtn.Picture.Bitmap);
  BtBlue.GetBitmap(BT_IMG_CITATION, CitationBtn.Picture.Bitmap);
  BtBlue.GetBitmap(BT_IMG_BUG_REPORT, BugReportBtn.Picture.Bitmap);
  BtBlue.GetBitmap(BT_IMG_UPDATES, UpdatesBtn.Picture.Bitmap);
  BtBlue.GetBitmap(BT_IMG_MEGA_LINKS, LinksBtn.Picture.Bitmap);
  BtBlue.GetBitmap(BT_IMG_TOOLBARS, ToolbarBtn.Picture.Bitmap);
  BtBlue.GetBitmap(BT_IMG_PREFERENCES, PreferencesBtn.Picture.Bitmap);
end;

procedure TMegaForm.CheckHelpFilesDone(aThread: TObject);
var
  helpThread: TCheckHelpFilesThread;
begin
  helpThread := TCheckHelpFilesThread(aThread);
  if not helpThread.IsSuccess then
    ShowMessage('Oh no! Failed to initialize the help system: ' + helpThread.Log);
end;

procedure TMegaForm.CheckHelpFiles;
var
  aThread: TCheckHelpFilesThread = nil;
begin
  try
    aThread := TCheckHelpFilesThread.Create(True);
    aThread.OnTerminate := CheckHelpFilesDone;
    aThread.Start;
  except
    on E:Exception do
    begin
      ShowMessage('Failed to initialize the help system: ' + E.Message);
      if Assigned(aThread) then
        aThread.Free;
    end;
  end;
end;

procedure TMegaForm.CheckWebDialogFilesDone(aThread: TObject);
var
  webDialogFilesThread: TWebDialogFilesThread;
begin
  webDialogFilesThread := TWebDialogFilesThread(aThread);
  if not webDialogFilesThread.IsSuccess then
    ShowMessage('Oh no! Failed to initialize files for web options dialogs: ' + webDialogFilesThread.Log);
end;

procedure TMegaForm.CheckWebDialogFiles;
var
  aThread: TWebDialogFilesThread = nil;
begin
  try
    aThread := TWebDialogFilesThread.Create(True);
    aThread.OnTerminate := CheckWebDialogFilesDone;
    aThread.Start;
  except
    on E:Exception do
    begin
      ShowMessage('Failed to initialize the web options dialogs system: ' + E.Message);
      if Assigned(aThread) then
        aThread.Free;
    end;
  end;
end;

procedure TMegaForm.CleanUpTempFiles;
var
  i: Integer;
begin
  i := 0;
  while FileExists(GetEnvironmentVariable('Temp') + PathDelim + TEMPORARY_MEGA + IntToStr(i) + '.xls') do
  begin
    try
      DeleteFile(GetEnvironmentVariable('Temp') + PathDelim + TEMPORARY_MEGA + IntToStr(i) + '.xls');
    except
      // do nothing, it will get cleaned up some day
    end;
    inc(i);
  end;
  i := 0;
  while FileExists(GetEnvironmentVariable('Temp') + PathDelim + TEMPORARY_MEGA + IntToStr(i) + '.xlsx') do
  begin
    try
      DeleteFile(GetEnvironmentVariable('Temp') + PathDelim + TEMPORARY_MEGA + IntToStr(i) + '.xlsx');
    except
      // do nothing, it will get cleaned up some day
    end;
    inc(i);
  end;
end;

function TMegaForm.CheckCancel(Progress: Integer; Status: String): Boolean;
begin
  Result := False;
  if not Assigned(FRuntimeProgress) then
    Exit;
  FRuntimeProgress.UpdatePercentProgress(Progress);
  FRuntimeProgress.UpdateRunStatusInfo('Status', Status);
  Result := FRuntimeProgress.StopBtn.Down;
  FRuntimeProgress.Refresh;
end;

procedure TMegaForm.GeneDupsCancelled;
begin
  if Assigned(FGDWizard) then
    FGDWizard.Close;
end;

procedure TMegaForm.ConcatenateFilesDone(aThread: TObject);
var
  concatThread: TAlignConcatThread;
  aList: TStringList;
begin
  try
    if Assigned(FRuntimeProgress) then
      FreeAndNil(FRuntimeProgress);
    Application.ProcessMessages;
    concatThread := TAlignConcatThread(aThread);
    if not concatThread.IsSuccess then
    begin
      ShowMessage('Concatenation of alignment files failed: ' + Copy(concatThread.LogMessages, 1, 200));
      Exit;
    end;
    aList := concatThread.GetMegaAlignmentStrings;
    ProcessInputMegaFile(aList, True, 'concatenatedSeqs.meg');
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaForm.MCLCommandThreadDone(aThread: TObject);
var
  ChromiumBrowser: TChromiumBrowser = nil;
  t: TMclCommandThread = nil;
  legend: TStringList = nil;
begin
  try
    try
      if not (aThread is TMclCommandThread) then
        raise Exception.Create('Expected TMclCommandThread but got ' + aThread.ClassName);
      t := TMclCommandThread(aThread);
      if not t.IsSuccess then
        raise Exception.Create('MCL calculation failed - ' + t.Log.Text);
      if t.IsCancelled then
        ShowMessage('MCL computation was cancelled by user')
      else
      begin
        ChromiumBrowser := CreateNewChromiumBrowserWindow(bmCaption);
        legend := t.FigureLegend;
        ChromiumBrowser.LoadHtmlFromString(legend.Text);
      end;
    except
      on E:Exception do
        ShowMessage('Application Error: ' + E.Message)
    end;
  finally
    if Assigned(legend) then
      legend.Free;
  end;
end;

procedure TMegaForm.DistCommandThreadDone(aThread: TObject);
var
  f: TDistCommandFinalize = nil;
  t: TDistCommandThread = nil;
begin
  try
    try
      if not (aThread is TDistCommandThread) then
        raise Exception.Create('APPLICATION ERROR: expected TDistCommandThread but got ' + aThread.ClassName);
      t := TDistCommandThread(aThread);
      if t.IsCancelled then
        ShowMessage('Distance calculation was aborted')
      else
      begin
        if not t.IsSuccess then
        begin
          if t.InternalErrorId >= 0 then
            raise Exception.Create(Format('Distance calculation failed with error ID = %d (%s)', [t.InternalErrorId, t.Log.Text]))
          else
            raise Exception.Create('APPLICATION ERROR: TDistCommandThread reported failure. ' + t.Log.Text);
        end;
        f := TDistCommandFinalize.CreateFromThread(t);
        if not f.Execute then
          raise Exception.Create('APPLICATION ERROR: failed to finalize distance calculation display. ' + f.Log.Text);
      end;
    except
      on E:Exception do
        ShowMessage(E.Message);
    end;
  finally
     if Assigned(f) then
       f.Free;
  end;
end;

procedure TMegaForm.DistTreeSearchThreadDone(aThread: TObject);
var
  t: TDistTreeSearchThread = nil;
begin
  if not (aThread is TDistTreeSearchThread) then
    ShowErrorMessage(Exception.Create('APPLICATION ERROR - invalid dist tree search thread class'));
  t := TDistTreeSearchThread(aThread);
  if not t.IsSuccess then
  begin
    ShowMessage('Distance calculation failed - ' + t.Log.Text);
  end;
end;

procedure TMegaForm.SiteCoverageThreadDone(aThread: TObject);
var
  t: TSiteCoverageThread = nil;
  setup: TTreeDisplaySetup = nil;
begin
  try
    try
      if not (aThread is TSiteCoverageThread) then
        raise Exception.Create(Format('expected TSiteCoverageThread but got %s', [aThread.ClassName]));
      t := TSiteCoverageThread(aThread);
      if t.IsCancelled then
        ShowMessage('Site coverage computation was cancelled')
      else if t.IsSuccess then
      begin
        setup := TTreeDisplaySetup.Create;
        setup.ShowSiteCoverage(t.AnalysisInfo);
      end
      else
        ShowMessage('site coverage compuation failed: ' + t.Log.Text);
    except
      on E:Exception do
        ShowMessage(Format('Application error: %s', [E.Message]));
    end;
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;

procedure TMegaForm.InitRecentlyUsedFiles;
var
  storage: String;
  i: Integer;
  aMenuItem: TMenuItem;
begin
  storage := GetPrivateFile(mfMegaMainMruFiles, False);
  FRecentlyUsedFiles := TRecentlyUsedFilesList.Create(Self, RecentlyUsedFilesItemClick, storage);
  if FRecentlyUsedFiles.Count > 0 then;
    for i := 0 to FRecentlyUsedFiles.Count - 1 do
    begin
      aMenuItem := TRecentlyUsedItem(FRecentlyUsedFiles[i]).MenuItem;
      DataRecentlyUsedFilesSessionsItem.Add(aMenuItem);
    end;
end;

procedure TMegaForm.AppendToRecentlyUsedFiles(aFilename: String);
var
  aMenu: TMenuItem;
begin
  if not FRecentlyUsedFiles.HasFile(aFileName) then
  begin
    aMenu := FRecentlyUsedFiles.Add(aFilename).MenuItem;
    DataRecentlyUsedFilesSessionsItem.Add(aMenu);
  end;
end;

procedure TMegaForm.RecentlyUsedFilesItemClick(Sender: TObject);
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
    if ExtractFileExt(filename) <> '.mtsx' then
    begin
      PromptCloseCurrentlyActive;
      if HasActiveData then
        Exit; // They might have clicked NO, in which case we just don't want to proceed.
    end;
    DoOpenDataFile(filename);
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

procedure TMegaForm.MegaGuiFormCreate;
var
  ExampleFiles: TExampleFiles;
begin
  UpdateShortcutsForMacOs(MegaActionList);
  InitVersionLabel;
  InitBetaMessage;
  FGDWizard := nil;
  MainStatusBar.Panels.Delete(1);
  InitDeveloperMessage;
  Caption := VER_MEGA_CAPTION;
  MainStatusBar.Panels[0].Text := 'Build #'+VER_MEGA_BUILD + ' ' + MY_PLATFORM;
  MainStatusBar.Panels[1].Text := EmptyStr;
  Application.OnIdle := MyIdleFunction;
  MegaMacGuiFormCreate;
  ExampleFiles := nil;
  try
    try
      ExampleFiles := TExampleFiles.Create;
      ExampleFiles.CheckExampleFiles;
    except
      // do nothing
    end;
  finally
    if Assigned(ExampleFiles) then
      ExampleFiles.Free;
    Invalidate;
  end;
end;

procedure TMegaForm.MegaMacGuiFormCreate;
begin
  {$IFDEF DARWIN}
  ShowInTaskBar := stAlways;
  MegaMenuItem.Caption := #$EF#$A3#$BF;
  Application.MainFormOnTaskBar := True;
  {$ENDIF}
end;

procedure TMegaForm.InitBetaMessage;
var
  Year, Month, Day: Word;
begin
  if (MegaReleaseType <> mrtBeta) and (MegaReleaseType <> mrtAlpha) then
    Exit;
  BetaLabel := TLabel.Create(Self);
  BetaLabel.Parent := CenterPanel;
  DecodeDate(Now, Year, Month, Day);
  BetaLabel.Caption := 'This is a DEVELOPMENT release which is intended solely for testing.' + LineEnding;
  BetaLabel.Caption := BetaLabel.Caption + ' Please DO NOT use results generated in publications.' + LineEnding;
  BetaLabel.caption := BetaLabel.Caption + 'This release expires on: ' + FormatSettings.LongMonthNames[Beta_Expires_Month+1] + ' 1, ' + IntToStr(Beta_Expires_Year) + '.' + LineEnding;
  BetaLabel.Caption := BetaLabel.Caption + Format('While testing, please click the %sSubmit Feedback%s link below to report bugs', [#39, #39]) + LineEnding;
  BetaLabel.Caption := BetaLabel.Caption + 'and provide feedback regarding this version of MEGA';
  BetaLabel.Font.Color := clMaroon;
  BetaLabel.Visible := True;
  BetaLabel.WordWrap := True;
  BetaLabel.Alignment := taCenter;
  BetaLabel.Align := alNone;
  BetaLabel.Left := Round((CenterPanel.Width - BetaLabel.Canvas.TextWidth(' While testing, please click the Submit Feedback link below to report bugs '))/2);
  FeedbackLink.Caption := 'Submit Test Feedback';
  FeedbackLink.Font.Style := [fsBold, fsUnderline];
  FeedbackLink.Font.Color := clNavy;
  FeedbackLink.Font.Height := 20;
  FeedbackLink.Visible := True;
  FeedbackLink.Alignment := taCenter;
  FeedbackLink.Parent := CenterPanel;
  FeedbackLink.Left := Round((CenterPanel.Width - FeedbackLink.Width)/2);
  TopToolbar.Top := 1;
  PinupsToolbar.Top := 63;
  BetaLabel.Top := PinupsToolbar.Top + PinupsToolbar.ButtonHeight + 10;
  FeedbackLink.Top := BottomToolbar.Top - FeedbackLink.Height - 70;
  FeedbackLink.BringToFront;
  Invalidate;
end;

procedure TMegaForm.InitDeveloperMessage;
begin
  DeveloperLabel.Visible := IsDeveloper;
  if not IsDeveloper then
    Exit;
  DeveloperLabel.Alignment := taCenter;
  DeveloperLabel.Align := alTop;
  DeveloperLabel.Font.Color := clMaroon;
  DeveloperLabel.Top := 5;
  DeveloperLabel.WordWrap := True;
end;

procedure TMegaForm.MyIdleFunction(Sender: TObject; var Done: Boolean);
begin
  if Done then  // idle just needs it
    Exit;
  try
    if Sender.ClassType <> TMegaTrayItem then
      ShowMessage('Error: Unexpected item passed to Idle Function.');
    if Sender = nil then
      ShowMessage('Error: sender was nil');
    With TMegaTrayItem(Sender) do
    begin
      if IsCreating then
        AddWindowToTray(Form)
      else
        RemoveWindowFromTray(Form);
    end;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred in the idle function: ' + E.Message);
  end;
end;

procedure TMegaForm.LaunchNewickThread(Filename: String);
var
  AThread: TLoadNewickTreeThread;
  AList: TTreeList;
begin
  AThread := nil;
  AList := nil;
  if not FileExists(Filename) then
  begin
    ShowMessage('Oh no! The specified newick file does not exist');
    Exit;
  end;
  try
    AList := TTreeList.Create;
    AThread := TLoadNewickTreeThread.Create;
    AThread.TreeList := AList;
    AThread.Filename := Filename;
    AThread.OnTerminate := OnNewickThreadDone;
    AThread.Start;
  except
    on E:Exception do
    begin
      ShowMessage('Oh no! An error occurred while loading the newick tree: ' + E.Message);
    end;
  end;
end;

procedure TMegaForm.OnNewickThreadDone(Thread: TObject);
var
  AThread: TLoadNewickTreeThread;
  i: Integer;
  Msg: String;
begin
  AThread := TLoadNewickTreeThread(Thread);
  if AThread.IsSuccess then
  begin
    ShowTreeFromTreeList(AThread.TreeList, aThread.Filename);
  end
  else
  begin
    Msg := 'Oh no! Failed to load the newick tree.' + LineEnding;
    if AThread.Messages.Count > 0 then
      for i := 0 to AThread.Messages.Count - 1 do
    Msg := Msg + AThread.Messages[i] + LineEnding;
    ShowMessage(Trim(Msg));
  end;
end;

procedure TMegaForm.LaunchUpdateThread(IsUserRequest: Boolean);
var
  aThread: TUpdateThread = nil;
begin
  try
    FUserRequestedUpdateCheck := IsUserRequest;
    aThread := TUpdateThread.Create(True);
    aThread.FreeOnTerminate := True;
    aThread.OnTerminate := UpdateThreadDone;
    aThread.Start;
  except
    on E:Exception do
      ShowMessage('Oh no! MEGA encountered an error when checking for updates: ' + E.Message);
  end;
  try
    //CollectUsageStatisticsIfItsTime;
  except
    { just eat the exception}
  end;
end;

procedure TMegaForm.SetDataTitle(AValue: String);
begin
  if FDataTitle <> AValue then
    FDataTitle := AValue
end;

procedure TMegaForm.SetMainMenuBgColor(AValue: TColor);
begin
  if FMainMenuBgColor=AValue then Exit;
  FMainMenuBgColor:=AValue;
end;

procedure TMegaForm.SetMainMenuFontColor(AValue: TColor);
begin
  if FMainMenuFontColor=AValue then Exit;
  FMainMenuFontColor:=AValue;
end;

procedure TMegaForm.UpdateThreadDone(Thread: TObject);
var
  aThread: TUpdateThread;
  Margin: String;
  MResult: Integer;
begin
  Margin := '   ';
  try
    try
      aThread := TUpdateThread(Thread);
      if aThread.IgnoreUpdate then
        Exit;

      if aThread.IsSuccess then
      begin
        if aThread.HasUpdate or aThread.HasNewVersion then
        begin
          UpdatesDlg.MessagesMemo.Lines.Clear;

          if aThread.HasNewVersion then
            UpdatesDlg.HeaderLbl.Caption := 'A new version (' + aThread.VersionString + ' ' + aThread.ReleaseType + ') of MEGA is now available'
          else
            UpdatesDlg.HeaderLbl.Caption := 'Update ' + aThread.VersionString + ' (' + aThread.ReleaseType + ') is now available';
          UpdatesDlg.MessagesMemo.Lines.Add(Margin + 'It can be downloaded at: ');
          UpdatesDlg.MessagesMemo.Lines.Add(Margin + #9 + aThread.DownloadUrl);
          UpdatesDlg.MessagesMemo.Lines.Add(Margin + 'using your web browser or by clicking the button below.');
          UpdatesDlg.MessagesMemo.Lines.Add(' ');
          UpdatesDlg.MessagesMemo.Lines.Add(Margin + 'Notes:');
          UpdatesDlg.MessagesMemo.Lines.Add(Margin + aThread.Message);
          FDownloadUrl := aThread.DownloadUrl;
          UpdatesDlg.Top := Top + (Top div 2) - (UpdatesDlg.Height div 2);
          UpdatesDlg.Left := Left + (Left div 2 ) - (UpdatesDlg.Width div 2);
          MResult := UpdatesDlg.ShowModal;
          if MResult = mrOk then
            OpenURL(FDownloadUrl)
          else
          begin
            if UpdatesDlg.IgnoreUpdateCheckBox.Checked then
              aThread.SetIgnoreThisUpdate;
          end;
        end
        else if FUserRequestedUpdateCheck then
          ShowMessage('No updates are available at this time');
      end
      else if FUserRequestedUpdateCheck then
      begin
        ShowMessage('No updates are available at this time');
      end;
    except
      on E:Exception do
        ShowMessage('Notice: MEGA tried checking for updates but encountered an error: ' + E.Message);
    end;
  finally
    FUserRequestedUpdateCheck := False;
  end;
end;

procedure TMegaForm.ResizeStatusBar;
begin
  MainStatusBar.Panels[0].Width := 275;
  MainStatusBar.Panels[1].Width := (Width - 275);
end;


procedure TMegaForm.ExportMuscleDnaJson;
var
  i: Integer;
  AList: TStringList;
  StringSetting: TStringAnalysisSetting;
  IntSetting: TIntegerAnalysisSetting;
  FloatSetting: TFloatAnalysisSetting;
  PickListSetting: TPickListAnalysisSetting;
  AJson: TJSONObject;
  //GeneticCodeList: String;
begin

  AList := nil;
  AJson := nil;
  try
    i := 0;
    AList := TStringList.Create;
    AJson := TJSONObject.Create;
    AJson.Add('MUSCLE Settings', 'DNA');
    AJson.Add('Count', 10);

    StringSetting := TStringAnalysisSetting.Create(0);
    StringSetting.Name := 'Gap Penalties';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    FloatSetting := TFloatAnalysisSetting.Create(1);
    FloatSetting.Name := 'Gap Open';
    FloatSetting.Description := 'A negative number. The penalty for opening a gap (usually higher than the penalty to extend).';
    FloatSetting.LongCmdFlag := '-gapopen';
    FloatSetting.Precision := 2;
    FloatSetting.Min := -2147483648;
    FloatSetting.Value := -1.0;
    FloatSetting.Max := 0.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'Gap Extend';
    FloatSetting.Description := 'A negative number. The penalty for extending an already open gap (usually less than for opening a gap)';
    FloatSetting.DisplayName := 'Gap Extend';
    FloatSetting.LongCmdFlag := '-gapextend';
    FloatSetting.Value := 0.0;
    FloatSetting.Min := -2147483648;
    FloatSetting.Max := 0.0;
    FloatSetting.Increment := 0.1;
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    StringSetting.Name := 'Memory/Iterations';
    StringSetting.DisplayName := 'Memory/Iterations';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    IntSetting := TIntegerAnalysisSetting.Create(1);
    IntSetting.Name := 'Max Memory in MB';
    IntSetting.Description := 'A positive integer to limit system memory usage by MUSCLE. By default it is set to the max available memory at the moment. If you get an error, try increasing this option by a lot.';
    IntSetting.LongCmdFlag := '-maxmb';
    IntSetting.Min := 256;
    IntSetting.Max := MaxInt;
    IntSetting.Increment := 256;
    IntSetting.Value := 2048;
    IntSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), IntSetting.GetJson);
    inc(i);

    IntSetting.Name := 'Max Iterations';
    IntSetting.DisplayName := 'Max Iterations';
    IntSetting.Description := 'The maximum number of iterations which MUSCLE will run before finishing.';
    IntSetting.LongCmdFlag := '-maxiters';
    IntSetting.Min := 1;
    IntSetting.Max := MaxInt;
    IntSetting.Value := 16;
    IntSetting.Increment := 1;
    AJson.Add('FSettings-' + IntToStr(i), IntSetting.GetJson);
    inc(i);

    StringSetting.Name := 'Advanced Options';
    StringSetting.DisplayName := 'Advanced Options';
    StringSetting.Value := opsPlus;
    StringSetting.SetState(True, True);
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    //PickListSetting := TPickListAnalysisSetting.Create(1, EmptyStr);
    //PickListSetting.Name := 'Genetic Code';
    //PickListSetting.SetState(False, True);
    //GeneticCodeList := GeneticCodeTablePickList;
    //GeneticCodeList := StringReplace(GeneticCodeList, '"', '', [rfReplaceAll]);
    //PickListSetting.PickList := GeneticCodeList;
    //AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    //inc(i);

    PickListSetting := TPickListAnalysisSetting.Create(1, EmptyStr);
    PickListSetting.Name := 'Cluster Method (Iterations 1,2)';
    PickListSetting.DisplayName := 'Cluster Method (Iterations 1,2)';
    PickListSetting.Description := 'This clustering method is used for the first 2 iterations.';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'UPGMA, UPGMB, Neighbor Joining';
    PickListSetting.LongCmdPickList := '-cluster1 upgma, -cluster1 upgmb, -cluster1 neighborjoining';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'Cluster Method (Other Iterations)';
    PickListSetting.DisplayName := 'Cluster Method (Other Iterations)';
    PickListSetting.Description := 'This clustering method is used for all iterations after the first 2.';
    PickListSetting.LongCmdPicklist := '-cluster2 upgma, -cluster2 upgmb, -cluster2 neighborjoining';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    IntSetting.Name := 'Min Diag Length (Lambda)';
    IntSetting.DisplayName := 'Min Diag Length (Lambda)';
    IntSetting.Description := 'The minimum diagional length (lambda)';
    IntSetting.ShortCmdFlag := '-diaglength';
    IntSetting.Min := 0;
    IntSetting.Max := MaxInt;
    IntSetting.Value := 24;
    IntSetting.Increment := 1;
    AJson.Add('FSettings-' + IntToStr(i), IntSetting.GetJson);
    inc(i);

    //StringSetting.IndentLevel := 1;
    //StringSetting.Name := 'Description';
    //StringSetting.Value := 'MUSCLE stands for multiple sequence comparison by log-expectation. It is a public domain multiple alignment software for protein and nucleotide sequences (Edgar 2004).';
    //StringSetting.SetState(True, True);
    //AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    //inc(i);
    //
    //StringSetting.Name := 'Citation';
    //StringSetting.Value := 'Edgar, Robert C. (2004), MUSCLE: multiple sequence alignment with high accuracy and high throughput, Nucleic Acids Research 32(5), 1792-1797.';
    //StringSetting.SetState(True, True);
    //AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);

    AList.Add(AJson.AsJson);
    AList.SaveToFile('/home/glen/Documents/muscle_dna.json');
  finally
    StringSetting.Free;
    IntSetting.Free;
    FloatSetting.Free;
    PickListSetting.Free;
    AList.Free;
    AJson.Free;
  end;
end;

procedure TMegaForm.ExportMuscleCodingJson;
var
  i: Integer;
  AList: TStringList;
  StringSetting: TStringAnalysisSetting;
  IntSetting: TIntegerAnalysisSetting;
  FloatSetting: TFloatAnalysisSetting;
  PickListSetting: TPickListAnalysisSetting;
  AJson: TJSONObject;
  GeneticCodeList: String;
begin
  AList := nil;
  AJson := nil;
  try
    i := 0;
    AList := TStringList.Create;
    AJson := TJSONObject.Create;
    AJson.Add('MUSCLE Settings', 'cDNA');
    AJson.Add('Count', 12);

    StringSetting := TStringAnalysisSetting.Create(0);
    StringSetting.Name := 'Gap Penalties';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    FloatSetting := TFloatAnalysisSetting.Create(1);
    FloatSetting.Name := 'Gap Open';
    FloatSetting.Description := 'A negative number. The penalty for opening a gap (usually higher than the penalty to extend).';
    FloatSetting.LongCmdFlag := '-gapopen';
    FloatSetting.Precision := 2;
    FloatSetting.Min := -2147483648;
    FloatSetting.Value := -1.0;
    FloatSetting.Max := 0.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'Gap Extend';
    FloatSetting.DisplayName := 'Gap Extend';
    FloatSetting.Description := 'A negative number. The penalty for extending an already open gap (usually less than for opening a gap)';
    FloatSetting.LongCmdFlag := '-gapextend';
    FloatSetting.Min := -2147483648;
    FloatSetting.Max := 0.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 0.0;
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'Hydrophobicity Multiplier';
    FloatSetting.DisplayName := 'Hydrophobicity Multiplier';
    FloatSetting.Description := 'Multiplier for gap open/close penalties in hydrophobic regions.';
    FloatSetting.Min := 0.0;
    FloatSetting.Max := MaxInt;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 1.2;
    FloatSetting.LongCmdFlag := '-hydrofactor';
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    StringSetting.Name := 'Memory/Iterations';
    StringSetting.DisplayName := 'Memory/Iterations';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    IntSetting := TIntegerAnalysisSetting.Create(1);
    IntSetting.Name := 'Max Memory in MB';
    IntSetting.DisplayName := 'Max Memory in MB';
    IntSetting.Description := 'A positive integer to limit system memory usage by MUSCLE. By default it is set to the max available memory at the moment. If you get an error, try increasing this option by a lot.';
    IntSetting.LongCmdFlag := '-maxmb';
    IntSetting.Min := 256;
    IntSetting.Max := MaxInt;
    IntSetting.Increment := 256;
    IntSetting.Value := 2048;
    IntSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), IntSetting.GetJson);
    inc(i);

    IntSetting.Name := 'Max Iterations';
    IntSetting.DisplayName := 'Max Iterations';
    IntSetting.Description := 'The maximum number of iterations which MUSCLE will run before finishing.';
    IntSetting.LongCmdFlag := '-maxiters';
    IntSetting.Min := 1;
    IntSetting.Max := MaxInt;
    IntSetting.Value := 16;
    IntSetting.Increment := 1;
    AJson.Add('FSettings-' + IntToStr(i), IntSetting.GetJson);
    inc(i);

    StringSetting.Name := 'Advanced Options';
    StringSetting.DisplayName := 'Advanced Options';
    StringSetting.Value := opsPlus;
    StringSetting.SetState(True, True);
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    PickListSetting := TPickListAnalysisSetting.Create(1, EmptyStr);
    PickListSetting.Name := 'Genetic Code';
    PickListSetting.DisplayName := 'Genetic Code';
    PickListSetting.Description := 'Genetic code for translating cDNA to amino acids';
    PickListSetting.SetState(False, True);
    GeneticCodeList := GeneticCodeTablePickList;
    GeneticCodeList := StringReplace(GeneticCodeList, '"', '', [rfReplaceAll]);
    PickListSetting.PickList := GeneticCodeList;
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'Cluster Method (Iterations 1,2)';
    PickListSetting.DisplayName := 'Cluster Method (Iterations 1,2)';
    PickListSetting.Description := 'This clustering method is used for the first 2 iterations.';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'UPGMA, UPGMB, Neighbor Joining';
    PickListSetting.LongCmdPickList := '-cluster1 upgma, -cluster1 upgmb, -cluster1 neighborjoining';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'Cluster Method (Other Iterations)';
    PickListSetting.DisplayName := 'Cluster Method (Other Iterations)';
    PickListSetting.Description := 'This clustering method is used for all iterations after the first 2.';
    PickListSetting.LongCmdPicklist := '-cluster2 upgma, -cluster2 upgmb, -cluster2 neighborjoining';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    IntSetting.Name := 'Min Diag Length (Lambda)';
    IntSetting.DisplayName := 'Min Diag Length (Lambda)';
    IntSetting.Description := 'The minimum diagional length (lambda)';
    IntSetting.ShortCmdFlag := '-diaglength';
    IntSetting.Min := 0;
    IntSetting.Max := MaxInt;
    IntSetting.Value := 24;
    IntSetting.Increment := 1;
    AJson.Add('FSettings-' + IntToStr(i), IntSetting.GetJson);
    inc(i);

    //StringSetting.IndentLevel := 1;
    //StringSetting.Name := 'Description';
    //StringSetting.Value := 'MUSCLE stands for multiple sequence comparison by log-expectation. It is a public domain multiple alignment software for protein and nucleotide sequences (Edgar 2004).';
    //StringSetting.SetState(True, True);
    //AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    //inc(i);
    //
    //StringSetting.Name := 'Citation';
    //StringSetting.Value := 'Edgar, Robert C. (2004), MUSCLE: multiple sequence alignment with high accuracy and high throughput, Nucleic Acids Research 32(5), 1792-1797.';
    //StringSetting.SetState(True, True);
    //AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);

    AList.Add(AJson.AsJson);
    AList.SaveToFile('/home/glen/Documents/muscle_codons.json');
  finally
    StringSetting.Free;
    IntSetting.Free;
    FloatSetting.Free;
    PickListSetting.Free;
    AList.Free;
    AJson.Free;
  end;
end;

procedure TMegaForm.ExportMuscleAminoJson;
var
  i: Integer;
  AList: TStringList;
  StringSetting: TStringAnalysisSetting;
  IntSetting: TIntegerAnalysisSetting;
  FloatSetting: TFloatAnalysisSetting;
  PickListSetting: TPickListAnalysisSetting;
  AJson: TJSONObject;
begin
  AList := nil;
  AJson := nil;
  try
    i := 0;
    AList := TStringList.Create;
    AJson := TJSONObject.Create;
    AJson.Add('MUSCLE Settings', 'Amino Acids');
    AJson.Add('Count', 11);

    StringSetting := TStringAnalysisSetting.Create(0);
    StringSetting.Name := 'Gap Penalties';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    FloatSetting := TFloatAnalysisSetting.Create(1);
    FloatSetting.Name := 'Gap Open';
    FloatSetting.Description := 'A negative number. The penalty for opening a gap (usually higher than the penalty to extend).';
    FloatSetting.LongCmdFlag := '-gapopen';
    FloatSetting.Precision := 2;
    FloatSetting.Min := -2147483648;
    FloatSetting.Value := -1.0;
    FloatSetting.Max := 0.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'Gap Extend';
    FloatSetting.DisplayName := 'Gap Extend';
    FloatSetting.Description := 'A negative number. The penalty for extending an already open gap (usually less than for opening a gap)';
    FloatSetting.LongCmdFlag := '-gapextend';
    FloatSetting.Min := -2147483648;
    FloatSetting.Max := 0.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 0.0;
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'Hydrophobicity Multiplier';
    FloatSetting.DisplayName := 'Hydrophobicity Multiplier';
    FloatSetting.Description := 'Multiplier for gap open/close penalties in hydrophobic regions.';
    FloatSetting.Min := 0.0;
    FloatSetting.Max := MaxInt;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 1.2;
    FloatSetting.LongCmdFlag := '-hydrofactor';
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    StringSetting.Name := 'Memory/Iterations';
    StringSetting.DisplayName := 'Memory/Iterations';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    IntSetting := TIntegerAnalysisSetting.Create(1);
    IntSetting.Name := 'Max Memory in MB';
    IntSetting.DisplayName := 'Max Memory in MB';
    IntSetting.Description := 'A positive integer to limit system memory usage by MUSCLE. By default it is set to the max available memory at the moment. If you get an error, try increasing this option by a lot.';
    IntSetting.LongCmdFlag := '-maxmb';
    IntSetting.Min := 256;
    IntSetting.Max := MaxInt;
    IntSetting.Increment := 256;
    IntSetting.Value := 2048;
    IntSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), IntSetting.GetJson);
    inc(i);

    IntSetting.Name := 'Max Iterations';
    IntSetting.DisplayName := 'Max Iterations';
    IntSetting.Description := 'The maximum number of iterations which MUSCLE will run before finishing.';
    IntSetting.LongCmdFlag := '-maxiters';
    IntSetting.Min := 1;
    IntSetting.Max := MaxInt;
    IntSetting.Value := 16;
    IntSetting.Increment := 1;
    AJson.Add('FSettings-' + IntToStr(i), IntSetting.GetJson);
    inc(i);

    StringSetting.Name := 'Advanced Options';
    StringSetting.DisplayName := 'Advanced Options';
    StringSetting.Value := opsPlus;
    StringSetting.SetState(True, True);
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    //PickListSetting := TPickListAnalysisSetting.Create(1, EmptyStr);
    //PickListSetting.Name := 'Genetic Code';
    //PickListSetting.Description := 'Genetic code for translating cDNA to amino acids';
    //PickListSetting.SetState(False, True);
    //GeneticCodeList := GeneticCodeTablePickList;
    //GeneticCodeList := StringReplace(GeneticCodeList, '"', '', [rfReplaceAll]);
    //PickListSetting.PickList := GeneticCodeList;
    //AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    //inc(i);

    PickListSetting := TPickListAnalysisSetting.Create(1, EmptyStr);
    PickListSetting.Name := 'Cluster Method (Iterations 1,2)';
    PickListSetting.DisplayName := 'Cluster Method (Iterations 1,2)';
    PickListSetting.Description := 'This clustering method is used for the first 2 iterations.';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'UPGMA, UPGMB, Neighbor Joining';
    PickListSetting.LongCmdPickList := '-cluster1 upgma, -cluster1 upgmb, -cluster1 neighborjoining';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'Cluster Method (Other Iterations)';
    PickListSetting.DisplayName := 'Cluster Method (Other Iterations)';
    PickListSetting.Description := 'This clustering method is used for all iterations after the first 2.';
    PickListSetting.LongCmdPicklist := '-cluster2 upgma, -cluster2 upgmb, -cluster2 neighborjoining';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    IntSetting.Name := 'Min Diag Length (Lambda)';
    IntSetting.DisplayName := 'Min Diag Length (Lambda)';
    IntSetting.Description := 'The minimum diagional length (lambda)';
    IntSetting.ShortCmdFlag := '-diaglength';
    IntSetting.Min := 0;
    IntSetting.Max := MaxInt;
    IntSetting.Value := 24;
    IntSetting.Increment := 1;
    AJson.Add('FSettings-' + IntToStr(i), IntSetting.GetJson);
    inc(i);

    //StringSetting.IndentLevel := 1;
    //StringSetting.Name := 'Description';
    //StringSetting.Value := 'MUSCLE stands for multiple sequence comparison by log-expectation. It is a public domain multiple alignment software for protein and nucleotide sequences (Edgar 2004).';
    //StringSetting.SetState(True, True);
    //AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    //inc(i);
    //
    //StringSetting.Name := 'Citation';
    //StringSetting.Value := 'Edgar, Robert C. (2004), MUSCLE: multiple sequence alignment with high accuracy and high throughput, Nucleic Acids Research 32(5), 1792-1797.';
    //StringSetting.SetState(True, True);
    //AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);

    AList.Add(AJson.AsJson);
    AList.SaveToFile('/home/glen/Documents/muscle_amino.json');
  finally
    StringSetting.Free;
    IntSetting.Free;
    FloatSetting.Free;
    PickListSetting.Free;
    AList.Free;
    AJson.Free;
  end;
end;

procedure TMegaForm.ExportClustalDnaJson;
var
  i: Integer;
  AList: TStringList;
  StringSetting: TStringAnalysisSetting;
  IntSetting: TIntegerAnalysisSetting;
  FloatSetting: TFloatAnalysisSetting;
  PickListSetting: TPickListAnalysisSetting;
  AJson: TJSONObject;
begin
  AList := nil;
  AJson := nil;
  try
    i := 0;
    AList := TStringList.Create;
    AJson := TJSONObject.Create;
    AJson.Add('MUSCLE Settings', 'DNA');
    AJson.Add('Count', 12);

    StringSetting := TStringAnalysisSetting.Create(0);
    StringSetting.Name := 'Pairwise Alignment';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    FloatSetting := TFloatAnalysisSetting.Create(1);
    FloatSetting.Name := 'DNAPWGapOpeningPenalty';
    FloatSetting.DisplayName := 'Gap Openening Penalty';
    FloatSetting.Description := 'A negative number. The penalty for opening a gap (usually higher than the penalty to extend).';
    FloatSetting.Precision := 2;
    FloatSetting.Min := 0.0;
    FloatSetting.Max := 100.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 15.0;
    FloatSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'DNAPWGapExtensionPenalty';
    FloatSetting.DisplayName := 'Gap Extension Penalty';
    FloatSetting.Description := 'A negative number. The penalty for extending an already open gap (usually less than for opening a gap)';
    FloatSetting.Min := 0.0;
    FloatSetting.Max := 100.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 6.66;
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    StringSetting.Name := 'Multiple Alignment';
    StringSetting.DisplayName := 'Multiple Alignment';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'DNAMAGapOpeningPenalty';
    FloatSetting.DisplayName := 'Gap Openening Penalty';
    FloatSetting.Description := 'A negative number. The penalty for opening a gap (usually higher than the penalty to extend).';
    FloatSetting.Precision := 2;
    FloatSetting.Min := 0.0;
    FloatSetting.Max := 100.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 15.0;
    FloatSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'DNAMAGapExtensionPenalty';
    FloatSetting.DisplayName := 'Gap Extension Penalty';
    FloatSetting.Description := 'A negative number. The penalty for extending an already open gap (usually less than for opening a gap)';
    FloatSetting.Min := 0.0;
    FloatSetting.Max := 100.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 6.66;
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    StringSetting.Name := 'Global Options';
    StringSetting.DisplayName := 'Global Options';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    PickListSetting := TPickListAnalysisSetting.Create(1, EmptyStr);
    PickListSetting.Name := 'DNA Weight Matrix';
    PickListSetting.DisplayName := 'DNAWeightMatrix';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'IUB, ClustalW (1.6)';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'TransitionWeightNEdit';
    FloatSetting.DisplayName := 'Transition Weight';
    FloatSetting.Description := '';
    FloatSetting.Precision := 2;
    FloatSetting.Min := 0.0;
    FloatSetting.Max := 1.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 0.5;
    FloatSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'UseNegativeMatrix';
    PickListSetting.DisplayName := 'Use Negative Matrix';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'ON, OFF';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    IntSetting := TIntegerAnalysisSetting.Create(1);
    IntSetting.Name := 'DelayDivergentCutoff';
    IntSetting.DisplayName := 'Delay Divergence Cutoff(%)';
    IntSetting.Description := '';
    IntSetting.Min := 0;
    IntSetting.Max := 100;
    IntSetting.Increment := 1;
    IntSetting.Value := 30;
    IntSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), IntSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'KeepPredefinedGaps';
    PickListSetting.DisplayName := 'Keep Predefined Gaps';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'True, False';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    AList.Add(AJson.AsJson);
    AList.SaveToFile('/home/glen/Documents/clustal_dna.json');
  finally
    StringSetting.Free;
    IntSetting.Free;
    FloatSetting.Free;
    PickListSetting.Free;
    AList.Free;
    AJson.Free;
  end;
end;

procedure TMegaForm.ExportClustalCodingJson;
var
  i: Integer;
  AList: TStringList;
  StringSetting: TStringAnalysisSetting;
  IntSetting: TIntegerAnalysisSetting;
  FloatSetting: TFloatAnalysisSetting;
  PickListSetting: TPickListAnalysisSetting;
  AJson: TJSONObject;
  GeneticCodeList: String;
begin
  AList := nil;
  AJson := nil;
  try
    i := 0;
    AList := TStringList.Create;
    AJson := TJSONObject.Create;
    AJson.Add('MUSCLE Settings', 'DNA');
    AJson.Add('Count', 16);

    StringSetting := TStringAnalysisSetting.Create(0);
    StringSetting.Name := 'Pairwise Alignment';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    FloatSetting := TFloatAnalysisSetting.Create(1);
    FloatSetting.Name := 'ProteinPWGapOpeningPenalty';
    FloatSetting.DisplayName := 'Gap Openening Penalty';
    FloatSetting.Description := 'A negative number. The penalty for opening a gap (usually higher than the penalty to extend).';
    FloatSetting.Precision := 2;
    FloatSetting.Min := 0.0;
    FloatSetting.Max := 100.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 10.0;
    FloatSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'ProteinPWGapExtensionPenalty';
    FloatSetting.DisplayName := 'Gap Extension Penalty';
    FloatSetting.Description := 'A negative number. The penalty for extending an already open gap (usually less than for opening a gap)';
    FloatSetting.Min := 0.0;
    FloatSetting.Max := 100.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 0.2;
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    StringSetting.Name := 'Multiple Alignment';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'ProteinMAGapOpeningPenalty';
    FloatSetting.DisplayName := 'Gap Openening Penalty';
    FloatSetting.Description := 'A negative number. The penalty for opening a gap (usually higher than the penalty to extend).';
    FloatSetting.Precision := 2;
    FloatSetting.Min := 0.0;
    FloatSetting.Max := 100.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 10.0;
    FloatSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'ProteinMAGapExtensionPenalty';
    FloatSetting.DisplayName := 'Gap Extension Penalty';
    FloatSetting.Description := 'A negative number. The penalty for extending an already open gap (usually less than for opening a gap)';
    FloatSetting.Min := 0.0;
    FloatSetting.Max := 100.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 0.2;
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    StringSetting.Name := 'Global Options';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    PickListSetting := TPickListAnalysisSetting.Create(1, EmptyStr);
    PickListSetting.Name := 'ProteinWeightMatrix';
    PickListSetting.DisplayName := 'Protein Weight Matrix';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'BLOSUM, PAM, Gonnet, Identity';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'Residue-specificPenalties';
    PickListSetting.DisplayName := 'Residue-specific Penalties';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'ON, OFF';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'HydrophillicPenalties';
    PickListSetting.DisplayName := 'Hydrophilic Penalties';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'ON, OFF';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    IntSetting := TIntegerAnalysisSetting.Create(1);
    IntSetting.Name := 'GapSeperationDistance';
    IntSetting.DisplayName := 'Gap Separation Distance';
    IntSetting.Description := '';
    IntSetting.Min := 0;
    IntSetting.Max := 100;
    IntSetting.Increment := 1;
    IntSetting.Value := 4;
    IntSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), IntSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'EndGapSeperation';
    PickListSetting.DisplayName := 'End Gap Separation';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'OFF, ON';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'Genetic Code';
    PickListSetting.DisplayName := 'Genetic Code Table';
    PickListSetting.Description := 'Genetic code for translating cDNA to amino acids';
    PickListSetting.SetState(False, True);
    GeneticCodeList := GeneticCodeTablePickList;
    GeneticCodeList := StringReplace(GeneticCodeList, '"', '', [rfReplaceAll]);
    PickListSetting.PickList := GeneticCodeList;
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'UseNegativeMatrix';
    PickListSetting.DisplayName := 'Use Negative Matrix';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'OFF, ON';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    //IntSetting := TIntegerAnalysisSetting.Create(1);
    IntSetting.Name := 'DelayDivergentCutoff';
    IntSetting.DisplayName := 'Delay Divergence Cutoff(%)';
    IntSetting.Description := '';
    IntSetting.Min := 0;
    IntSetting.Max := 100;
    IntSetting.Increment := 1;
    IntSetting.Value := 30;
    IntSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), IntSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'KeepPredefinedGaps';
    PickListSetting.DisplayName := 'Keep Predefined Gaps';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'True, False';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    AList.Add(AJson.AsJson);
    AList.SaveToFile('/home/glen/Documents/clustal_codons.json');
  finally
    StringSetting.Free;
    IntSetting.Free;
    FloatSetting.Free;
    PickListSetting.Free;
    AList.Free;
    AJson.Free;
  end;
end;

procedure TMegaForm.ExportClustalAminoJson;
var
  i: Integer;
  AList: TStringList;
  StringSetting: TStringAnalysisSetting;
  IntSetting: TIntegerAnalysisSetting;
  FloatSetting: TFloatAnalysisSetting;
  PickListSetting: TPickListAnalysisSetting;
  AJson: TJSONObject;
begin
  AList := nil;
  AJson := nil;
  try
    i := 0;
    AList := TStringList.Create;
    AJson := TJSONObject.Create;
    AJson.Add('MUSCLE Settings', 'DNA');
    AJson.Add('Count', 15);

    StringSetting := TStringAnalysisSetting.Create(0);
    StringSetting.Name := 'Pairwise Alignment';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    FloatSetting := TFloatAnalysisSetting.Create(1);
    FloatSetting.Name := 'ProteinPWGapOpeningPenalty';
    FloatSetting.DisplayName := 'Gap Openening Penalty';
    FloatSetting.Description := 'A negative number. The penalty for opening a gap (usually higher than the penalty to extend).';
    FloatSetting.Precision := 2;
    FloatSetting.Min := 0.0;
    FloatSetting.Max := 100.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 10.0;
    FloatSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'ProteinPWGapExtensionPenalty';
    FloatSetting.DisplayName := 'Gap Extension Penalty';
    FloatSetting.Description := 'A negative number. The penalty for extending an already open gap (usually less than for opening a gap)';
    FloatSetting.Min := 0.0;
    FloatSetting.Max := 100.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 0.2;
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    StringSetting.Name := 'Multiple Alignment';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'ProteinMAGapOpeningPenalty';
    FloatSetting.DisplayName := 'Gap Openening Penalty';
    FloatSetting.Description := 'A negative number. The penalty for opening a gap (usually higher than the penalty to extend).';
    FloatSetting.Precision := 2;
    FloatSetting.Min := 0.0;
    FloatSetting.Max := 100.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 10.0;
    FloatSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    FloatSetting.Name := 'ProteinMAGapExtensionPenalty';
    FloatSetting.DisplayName := 'Gap Extension Penalty';
    FloatSetting.Description := 'A negative number. The penalty for extending an already open gap (usually less than for opening a gap)';
    FloatSetting.Min := 0.0;
    FloatSetting.Max := 100.0;
    FloatSetting.Increment := 0.1;
    FloatSetting.Value := 0.2;
    AJson.Add('FSettings-' + IntToStr(i), FloatSetting.GetJson);
    inc(i);

    StringSetting.Name := 'Global Options';
    StringSetting.SetState(True, True);
    StringSetting.Value := opsPlus;
    AJson.Add('FSettings-' + IntToStr(i), StringSetting.GetJson);
    inc(i);

    PickListSetting := TPickListAnalysisSetting.Create(1, EmptyStr);
    PickListSetting.Name := 'ProteinWeightMatrix';
    PickListSetting.DisplayName := 'Protein Weight Matrix';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'BLOSUM, PAM, Gonnet, Identity';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'Residue-specificPenalties';
    PickListSetting.DisplayName := 'Residue-specific Penalties';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'ON, OFF';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'HydrophillicPenalties';
    PickListSetting.DisplayName := 'Hydrophilic Penalties';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'ON, OFF';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    IntSetting := TIntegerAnalysisSetting.Create(1);
    IntSetting.Name := 'GapSeperationDistance';
    IntSetting.DisplayName := 'Gap Separation Distance';
    IntSetting.Description := '';
    IntSetting.Min := 0;
    IntSetting.Max := 100;
    IntSetting.Increment := 1;
    IntSetting.Value := 4;
    IntSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), IntSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'EndGapSeperation';
    PickListSetting.DisplayName := 'End Gap Separation';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'OFF, ON';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'UseNegativeMatrix';
    PickListSetting.DisplayName := 'Use Negative Matrix';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'OFF, ON';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    //IntSetting := TIntegerAnalysisSetting.Create(1);
    IntSetting.Name := 'DelayDivergentCutoff';
    IntSetting.DisplayName := 'Delay Divergence Cutoff(%)';
    IntSetting.Description := '';
    IntSetting.Min := 0;
    IntSetting.Max := 100;
    IntSetting.Increment := 1;
    IntSetting.Value := 30;
    IntSetting.SetState(False, True);
    AJson.Add('FSettings-' + IntToStr(i), IntSetting.GetJson);
    inc(i);

    PickListSetting.Name := 'KeepPredefinedGaps';
    PickListSetting.DisplayName := 'Keep Predefined Gaps';
    PickListSetting.Description := '';
    PickListSetting.SetState(False, True);
    PickListSetting.PickList := 'True, False';
    AJson.Add('FSettings-' + IntToStr(i), PickListSetting.GetJson);
    inc(i);

    AList.Add(AJson.AsJson);
    AList.SaveToFile('/home/glen/Documents/clustal_amino.json');
  finally
    StringSetting.Free;
    IntSetting.Free;
    FloatSetting.Free;
    PickListSetting.Free;
    AList.Free;
    AJson.Free;
  end;
end;

procedure TMegaForm.MegaProtoOnFormCreate;
begin
  MainStatusBar.Panels[0].Text := 'Build #'+VER_MEGA_BUILD + ' ' + MY_PLATFORM;
  MainStatusBar.Panels[1].Text := 'No data is loaded yet';
  MainStatusBar.Panels[2].Text := EmptyStr;
  UpdateToolBarActionExecute(nil);
  FPreviousPrototyperIndex := 0;
  ApplyPrototyperVisualModifications;
  Caption := VER_MEGA_MAJOR + VER_MEGA_MINOR + '-Proto';
  //CheckUpdatesTimer.Enabled := True;
  CheckUpdatesTimer.Enabled := False;
  OpenFileAction.Enabled := False;
end;

procedure TMegaForm.SetDataFile(AValue: String);
begin
  FDataFileName := AValue;
  if not IsPrototyper then
    SetCurrentDir(ExtractFilePath(AValue));
  if MainStatusBar.Panels.Count > 0 then
    MainStatusBar.Panels[MainStatusBar.Panels.Count - 1].Text := ExtractFileName(AValue) + '     ';
end;

procedure TMegaForm.AddFilenameToStatusBar(aFilename: String);
begin
  if MainStatusBar.Panels.Count > 0 then
    MainStatusBar.Panels[MainStatusBar.Panels.Count - 1].Text := ExtractFileName(aFilename) + '     ';
end;

procedure TMegaForm.EnableMenusForActiveDataType;
begin
  //Misc
  CloseDataAction.Enabled := (not IsPrototyper);
  DataOpenAFileAction.Enabled := (not IsPrototyper);

  // Align
  //AlignMenuImg.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  AlignDnaMuscleAction.Enabled := (FDataType = snNucleotide);
  AlignCodonsMuscleAction.Enabled := containsCodingNuc and (FDataType <> snProtein) and (FDataType <> snDistance) and (FDataType <> snTree);
  AlignProteinMuscleAction.Enabled := (FDataType = snProtein);
  AlignDnaClustalAction.Enabled := (FDataType = snNucleotide);
  AlignCodonsClustalAction.Enabled := containsCodingNuc and (FDataType <> snProtein) and (FDataType <> snDistance) and (FDataType <> snTree);
  AlignProteinClustalAction.Enabled := (FDataType = snProtein);

  { prototyper alignment menu items}
  MuscleNucAlignmentMenuItem.Enabled := (FDataType = snNucleotide);
  MuscleCodonAlignmentMenuItem.Enabled := (containsCodingNuc and (FDataType <> snProtein) and (FDataType <> snDistance) and (FDataType <> snTree));
  MuscleProteinAlignmentMenuItem.Enabled := (FDataType = snProtein);

  ClustalNucAlignmentMenuItem.Enabled := (FDataType = snNucleotide);
  ClustalCodonAlignmentMenuItem.Enabled := (containsCodingNuc and (FDataType <> snProtein) and (FDataType <> snDistance) and (FDataType <> snTree));
  ClustalProteinAlignmentMenuItem.Enabled := (FDataType = snProtein);

  // Models
  //ModelsMenuImg.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  FindBestDNAAction.Enabled := (FDataType <> snTree) and (FDataType <> snDistance);
  SubstitutionMatrixAction.Enabled := (FDataType <> snTree) and (FDataType <> snDistance);
  TransitionBiasAction.Enabled := (FDataType <> snProtein) and (FDataType <> snTree) and (FDataType <> snDistance);
  MCLMatrixAction.Enabled := (FDataType <> snProtein) and (FDataType <> snTree) and (FDataType <> snDistance);
  MCLBiasAction.Enabled := (FDataType <> snProtein) and (FDataType <> snTree) and (FDataType <> snDistance);
  AminoAcidCompAction.Enabled := (not isPrototyper);
  NucleotideCompAction.Enabled := (not isPrototyper);
  CodonUsageAction.Enabled := (not isPrototyper);
  DisparityIndexAction.Enabled := (FDataType <> snTree) and (FDataType <> snDistance);
  CompositionDistanceAction.Enabled := (FDataType <> snTree) and (FDataType <> snDistance);
  PatternDisparityAction.Enabled := (FDataType <> snTree) and (FDataType <> snDistance);

  // Distance
  //DistanceMenuImg.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  PairwiseDistanceAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  OverallMeanDistanceAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  WithinMeanDistanceAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  BetweenMeanAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  NetMeanAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));

  // Diversity
  //DiversityMenuImg.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  MeanDiversityWithinAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  MeanDiversityEntireAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  MeanDiversityInterpopAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  CoefficientofDifferentiationAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));

  // Phylogeny
  //PhylogenyMenuImg.Enabled := (not (FDataType = snTree));
  ConstructNJTreeAction.Enabled := (not (FDataType = snTree));
  ConstructMETreeAction.Enabled := (not (FDataType = snTree));
  ConstructUPGMATreeAction.Enabled := (not (FDataType = snTree));
  ConstructMLTreeAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  ConstructMPTreeAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  OpenTreeSessionAction.Enabled := (not IsPrototyper);

  // User Tree
  //UserTreeMenuImg.Enabled := not (FDataType = snTree);
  AnalyzeTreeLSAction.Enabled := (FDataType <> snTree);
  AnalyzeTreeMLAction.Enabled := (FDataType <> snDistance) and (FDataType <> snTree);
  AnalyzeTreeMPAction.Enabled := (FDataType <> snDistance) and (FDataType <> snTree);
  EditTreeAction.Enabled := (not IsPrototyper);
  DisplayNewickTreeAction.Enabled := (not IsPrototyper);
  GeneDupsAction.Enabled := (not IsPrototyper);

  // Ancestors
  //AncestorsMenuImg.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  AncestralSeqsMLAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  AncestralSeqsMPAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  PredictLivingSequenceAction.Visible := IsDeveloper;
  PredictLivingSequenceAction.Enabled := IsDeveloper and (not (FDataType = snDistance)) and (not (FDataType = snTree));

  // Selection
  //SelectionMenuImg.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  CodonZTestAction.Enabled := ((not IsPrototyper) or containsCodingNuc) and (FDataType <> snProtein) and (FDataType <> snDistance) and (FDataType <> snTree);
  CodonFisherTestAction.Enabled := ((not IsPrototyper) or containsCodingNuc) and (FDataType <> snProtein) and (FDataType <> snDistance) and (FDataType <> snTree);
  TajimaNeutralityTestAction.Enabled := ((not IsPrototyper) or (not (FDataType = snDistance))) and (FDataType <> snTree);

  // Rates
  //RatesMenuImg.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  GammaParameterMLAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));
  PositionbyPositionRatesMLAction.Enabled := (not (FDataType = snDistance)) and (not (FDataType = snTree));

  // Clocks
  //ClocksMenuImg.Enabled := (not (FDataType = snDistance));
  MolecularClockTestAction.Enabled := (FDataType <> snTree) and (FDataType <> snDistance);
  ComputeTimeTreeMLAction.Enabled := (FDataType <> snTree) and (FDataType <> snDistance);
  TajimaRateTestAction.Enabled := (FDataType <> snTree) and (FDataType <> snDistance);
  CorrTestMLAction.Enabled := (FDataType <> snTree) and (FDataType <> snDistance);
  CorrTestBlensAction.Enabled := (FDataType = snTree);
  ReltimeBlensAction.Enabled := (FDataType = snTree) or (not IsPrototyper);
  ComputeTimetreeLSAction.Enabled := (FDataType <> snTree);

  //Diagnose
  DiagnoseMutationAction.Enabled := (not IsPrototyper);

  //Third Party Tools
  ThirdPartyToolsAction.Enabled := IsDeveloper;
end;

procedure TMegaForm.EnableMenusForRunMode;
begin
  if IsPrototyper then
  begin
    Gray72x90.GetBitmap(MEN_IMG_DATA, DataMenuImg.Picture.Bitmap);
    BtGray.GetBitmap(BT_IMG_EXAMPLES, ExamplesBtn.Picture.Bitmap);
  end
  else
  begin
    Default72x90.GetBitmap(MEN_IMG_DATA, DataMenuImg.Picture.Bitmap);
    BtBlue.GetBitmap(BT_IMG_EXAMPLES, ExamplesBtn.Picture.Bitmap);
  end;
end;

function TMegaForm.OpenFile(FFileName: String; FFileExt: String; OpenFrom: TOpenFrom): Boolean;
var
  UpdateCurrentFilePane: Boolean;
  openInSDE: Boolean;
  aEditor: TTraceEditForm = nil;
begin
  openInSDE := (not IsPrototyper);
  Result := False;
  { TODO 2 -oglen -ccaltest : update TMegaForm.OpenFile to return true on success and false on failure everywhere }
  if (ContainsText(MegaExts,  FFileExt)) or
     (ContainsText(MasExts,   FFileExt)) or
     (ContainsText(FastaExts, FFileExt)) then
  begin
    PromptCloseCurrentlyActive;
    if HasActiveData then
      Exit; { the user opted out}
  end;

  try
    UpdateCurrentFilePane := false;
    if ContainsText(MegaExts, FFileExt) or ContainsText(PaupExts, FFileExt) then
    begin
      PromptCloseCurrentlyActive;
      if HasActiveData then // check if the use has bailed
        Exit;
       Result := DoOpenDataFile(FFileName, openInSDE);
      UpdateCurrentFilePane := true;
    end
    else if (Pos(FFileExt, MtsExts) > 0) or (Pos(FFileExt, NewickExts) > 0) then
      ShowTreeFromFile(FFileName)
    else if (Pos(FFileExt, MasExts) > 0) then
    begin
      MegaForm.SetFocus;
      if QuestionDlg('How would you like to open this MAS file?', 'Analyze or Align File?', mtInformation, [21, 'Align', 22, 'Analyze'], 0) = 21 then
        ShowAlignmentFromFile(FFileName)
      else
      begin
        PromptCloseCurrentlyActive;
        if HasActiveData then
          Exit; { user aborted}
        DoOpenDataFile(FFileName);
        UpdateCurrentFilePane := true;
      end;
    end
    else if AnsiContainsText(FastaExts, FFileExt) then
    begin
      if OpenFrom = OAlign then
        ShowAlignmentFromFile(FFileName)
      else
      if OpenFrom = OActivate then
      begin
        PromptCloseCurrentlyActive;
        if HasActiveData then
        begin
          // They might have clicked NO, in which case we just don't want to procede.
          Exit;
        end;
        Result := DoOpenDataFile(FFilename);
        UpdateCurrentFilePane := true;
      end
      else
      begin
        MegaForm.SetFocus;
        if QuestionDlg('How would you like to open this fasta file?', 'Analyze or Align File?', mtInformation, [21, 'Align', 22, 'Analyze'], 0) = 21 then
          ShowAlignmentFromFile(FFileName)
        else
        begin
          PromptCloseCurrentlyActive;
          if HasActiveData then
          begin
            // They might have clicked NO, in which case we just don't want to procede.
            Exit;
          end;
          Result := DoOpenDataFile(FFilename);
          UpdateCurrentFilePane := true;
        end;
      end;
    end
    else if AnsiContainsText(PaupExts+ClustalExts+PhylipExts+GCGExts+PIRExts+NbrfExts+MsfExts+IgExts+TextExts, FFileExt) then
      OpenFileAndFocus(FFilename, 0, 0)
    else if AnsiContainsText(ABIExts, FFileExt) then
    begin
      aEditor := OpenFileInNewTraceEditorWindow(FFilename);
      if Assigned(aEditor) then
        aEditor.Show;
    end
    else
    begin
      if MessageDlg('Would you like to view the file in Text editor?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        OpenFileAndFocus(FFilename, 0, 0);
    end;

    if not IsPrototyper then
      UpdateRecentFilesList(FFileName);
    if (UpdateCurrentFilePane = true) and (not IsPrototyper) then
      MainStatusBar.Panels[MainStatusBar.Panels.Count - 1].Text := ExtractFileName(FFileName) + '     ';
  Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaForm.SetTheWizardFree;
begin
  if Assigned(FGDWizard) then
    FreeAndNil(FGDWizard);
end;

procedure TMegaForm.SetTimeTreeWizardFree;
begin
  if Assigned(TimeTreeWizard) then
    FreeAndNil(TimeTreeWizard);
end;

procedure TMegaForm.applyPrototyperVisualModifications;
begin
  if isPrototyper then
  begin
    GeneDupsAction.Visible := False;
    GeneDupsAction.Enabled := False;
    EditTreeMenuItemSpacer.Visible := False;
    OpenTreeSessionMenuItemSpacer.Visible := False;
    ShowMegaBrowserMenuItem.Visible := False;
    UpdatesBtn.Visible := false;
    ExamplesBtn.Visible := false;
    ToolbarBtn.Visible := false;
    PreferencesBtn.Visible := False;
    MegaForm.Menu := nil; // Get rid of the main menu
    DataMenuImg.Visible := false;
    DiagnoseMenuImg.Visible := False;
    EditTreeAction.Visible := False;
    DisplayNewickTreeAction.Visible := False;
    OpenTreeSessionAction.Visible := False;
    FPreviousPrototyperIndex := -1;
    ComputeTimetreeLSAction.Visible := IsDeveloper;
    InitDeveloperMessage;
  end
  else
  begin
    ComputeTimetreeLSAction.Visible := False;
  end;
  {$IFNDEF DEBUG}
  MagicBtn.Visible := False;
  {$ENDIF}
  EnableMenusForActiveDataType;
end;

procedure TMegaForm.SetProtoDataType(DataType: Integer);
begin
  case dataType of
    0:
     begin
       OpenFile(GetPrivateFile('Private' + PathDelim + 'prototyper' + PathDelim + 'nuc.meg'), '.meg', OOther);
       FDataType := snNucleotide;
       containsCodingNuc := False;
       MainStatusBar.Panels[1].Text := 'Nucleotide (Non-coding) data specified';
     end;
    1:
     begin
       OpenFile(GetPrivateFile('Private' + PathDelim + 'prototyper' + PathDelim + 'cnuc.meg'), '.meg', OOther);
       FDataType := snNucleotide;
       containsCodingNuc := True;
       MainStatusBar.Panels[1].Text := 'Nucleotide (coding) data specified';
     end;
    2:
     begin
       OpenFile(GetPrivateFile('Private' + PathDelim + 'prototyper' + PathDelim + 'prot.meg'), '.meg', OOther);
       FDataType := snProtein;
       MainStatusBar.Panels[1].Text := 'Protein (amino acid) data specified';
     end;
    3:
     begin
       OpenFile(GetPrivateFile('Private' + PathDelim + 'prototyper' + PathDelim + 'dist.meg'), '.meg', OOther);
       FDataType := snDistance;
       MainStatusBar.Panels[1].Text := 'Distance matrix (MEGA format) specified';
     end;
    4:
     begin
       FDataType := snTree;
       MainStatusBar.Panels[1].Text := 'Newick Tree (with branch lengths) specified';
     end;
  end;
  EnableMenusForActiveDataType;
end;

procedure TMegaForm.UpdateRecentFilesList(AFile: String);
begin
  AppendToRecentlyUsedFiles(AFile);
end;

procedure TMegaForm.ClearData;
var
  pw: TPleaseWait = nil;
begin
  FDataTitle := EmptyStr;
  FDataFileDescription := EmptyStr;
  if FDataType = snNoToken then
    Exit;
  try
    pw := TPleaseWait.Create(self);
    pw.SetShowCancel(False);
    pw.Caption := 'Releasing Resources';
    pw.Action := 'Freeing used memory. Please wait...';
    pw.SetToMarqueeMode;
    pw.Show;
    FDataType := snNoToken;
    if Assigned(GeneDomainDlg) then
      FreeAndNil(GeneDomainDlg);
    Application.ProcessMessages;
    if Assigned(TaxaGpsDlg) then
      FreeAndNil(TaxaGpsDlg);
    Application.ProcessMessages;
    if Assigned(V_SeqDataExplorer) then
      FreeAndNil(V_SeqDataExplorer);
    Application.ProcessMessages;
    if Assigned(VS_SeqDataExplorer) then
      FreeAndNil(VS_SeqDataExplorer);
    Application.ProcessMessages;
    if Assigned(D_InputSeqData) then
      FreeAndNil(D_InputSeqData);
    Application.ProcessMessages;
    if Assigned(V_DistDataExplorer) then
      FreeAndNil(V_DistDataExplorer);
    Application.ProcessMessages;
    if Assigned(VS_DistDataExplorer) then
      FreeAndNil(VS_DistDataExplorer);
    Application.ProcessMessages;
    if Assigned(D_InputDistData) then
      FreeAndNil(D_InputDistData);
    if Assigned(MegaInfoForm) then
      FreeAndNil(MegaInfoForm);
    Application.ProcessMessages;
    MainStatusBar.Panels[MainStatusBar.Panels.Count - 1].Text := EmptyStr;
    DataFileName := EmptyStr;
  finally
    if Assigned(pw) then
      pw.Free;
  end;
end;

procedure TMegaForm.AssignContextHelpToMenus;
begin
  HelpContext := HC_Main_MEGA_Window;
  DataPopupMenu.HelpContext                           := HC_Data_Menu;
  OpenFileAction.HelpContext                          := HC_Open_A_File;
  DataOpenAFileAction.HelpContext                     := HC_Open_A_File;
  OpenTextEditorAction.HelpContext                    := HC_Text_Editor;
  ConvertToMegaFormatAction.HelpContext   := HC_Convert_To_MEGA_Format;
  SaveDataAction.HelpContext         := HC_Save_Data_Session_To_File;
  ExportDataAction.HelpContext                    := HC_Export_Data;
  CloseDataAction.HelpContext                     := HC_Close_data;
  ExploreDataAction.HelpContext             := HC_Data_Explorer; // Had to pick either Seq or Dist explorer, can't pick both.
  SelectGeneDomainAction.HelpContext            := HC_Setup_Genes_Domains;
  SelectTaxaGroupAction.HelpContext                 := HC_Setup_Taxa_Groups;
  SelectGeneticCodeAction.HelpContext             := HC_Select_Genetic_Code_Table;
  OpenAlignmentAction.HelpContext     := HC_Open_Saved_Alignment_Session;
  BuildAlignmentAction.HelpContext         := HC_Alignment_Explorer_CLUSTAL;
  EditSequencerAction.HelpContext        := HC_Edit_Sequencer_Files;
  DataPrinterSetupAction.HelpContext                    := HC_Printer_Setup;
  DataExitAction.HelpContext                        := HC_Exit;

  //Phylogeny
  ConstructNJTreeAction.HelpContext                          := HC_Neighbor_Joining;
  ConstructMETreeAction.HelpContext                          := HC_Minimum_Evolution;
  ConstructUPGMATreeAction.HelpContext                       := HC_UPGMA;
  ConstructMLTreeAction.HelpContext              := HC_Maximum_likelihood_ML_;
  ConstructMPTreeAction.HelpContext := HC_Maximum_Parsimony;
  //Selection
  CodonZTestAction.HelpContext                     := HC_Z_Test_of_Selection;
  CodonFisherTestAction.HelpContext               := HC_Fisher_Exact_Test_Of_Selection;
  TajimaNeutralityTestAction.HelpContext             := HC_Tajima_Test_of_Neutrality;
  //Molecular clocks
  TajimaRateTestAction.HelpContext             := HC_Tajima_Test_Relative_Rate;
  MolecularClockTestAction.HelpContext                 := HC_Molecular_Clock_Test_ML_;
  //Ancestral Sequences
  AncestralSeqsMLAction.HelpContext                  := HC_Inferring_Ancestral_Sequences_ML_;
  AncestralSeqsMPAction.HelpContext       := HC_Inferring_Ancestral_sequences_MP_;
  FindBestDNAAction.HelpContext              := HC_Find_Best_DNA_Protein_Models_ML_;
  // Model Parameters (ML/MCL)
  SubstitutionMatrixAction.HelpContext              := HC_Estimate_Substitution_Matrix_ML_;
  TransitionBiasAction.HelpContext                := HC_Estimate_Transition_Transversion_Bias_ML_;
  GammaParameterMLAction.HelpContext             := HC_Estimate_Rate_Variation_among_Sites_ML_;
  MCLMatrixAction.HelpContext       := HC_Compute_MCL_Substitution_Matrix;
  MCLBiasAction.HelpContext                 := HC_Compute_MCL_Transversion_Transition_bias;
  PositionByPositionRatesMLAction.HelpContext       := HC_Estimate_Position_by_Position_Rates_ML_;
  // Substitution Pattern Disparity
  PatternDisparityAction.HelpContext := HC_Pattern_Menu;
  CompositionDistanceAction.HelpContext              := HC_Pattern_Menu;
  DisparityIndexAction.HelpContext             := HC_Pattern_Menu;
  // Sequence Compositions
  NucleotideCompAction.HelpContext        := HC_Nucleotide_Composition;
  AminoAcidCompAction.HelpContext   := HC_Amino_Acid_Composition;
  CodonUsageAction.HelpContext                   := HC_Codon_Usage;
  // Distances
  PairwiseDistanceAction.HelpContext                     := HC_Pairwise_Distances;
  OverallMeanDistanceAction.HelpContext                  := HC_Overall_Mean;
  WithinMeanDistanceAction.HelpContext              := HC_Within_Group_Mean;
  BetweenMeanAction.HelpContext             := HC_Between_Groups_Means;
  NetMeanAction.HelpContext            := HC_Net_Between_Groups_Means;
  // Sequence Diversity
  MeanDiversityWithinAction.HelpContext    := HC_Sequence_Diversity;
  MeanDiversityEntireAction.HelpContext       := HC_Sequence_Diversity;
  MeanDiversityInterPopAction.HelpContext        := HC_Sequence_Diversity;
  CoefficientOfDifferentiationAction.HelpContext      := HC_Sequence_Diversity;
  // User Tree Computation
  AnalyzeTreeLSAction.HelpContext               := HC_Analyze_User_Tree_by_Least_Squares;
  AnalyzeTreeMLAction.HelpContext                   := HC_Analyze_User_Tree_by_Maximum_Likelihood;
  AnalyzeTreeMPAction.HelpContext         := HC_Analyze_User_Tree_by_Parsimony;
  //DistConductInteriorBranchTestbyLeastSquaresAction.HelpContext := HC_Conduct_Interior_Branch_Test_by_Least_Squares;


  OpenTreeSessionAction.HelpContext                := HC_Display_Saved_Tree_session;
  MegaBrowserAction.HelpContext                    := HC_Show_Web_Browser;
  ShowBrowserAction.HelpContext                    := HC_Show_Web_Browser;
  QueryDatabankAction.HelpContext                  := HC_Browse_Databanks;
  BLASTAction.HelpContext                          := HC_BLAST_Search;
end;

procedure TMegaForm.SetRunMode(aMode: TMegaRunMode);
var
  response: Integer;
begin
  if (MegaRunMode = mrmGui) and (aMode = mrmPrototyper) then
  begin
    if MegaForm.HasActiveData then
    begin
      if (MessageDlg('Switching to prototyper mode will require that the active data file be closed. Close the current active data file?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes) then
        MegaForm.CloseDataActionExecute(nil)  // Asks users if they would like to save their session.
      else
        Exit;
    end;
  end;

  MegaRunMode := aMode;
  case aMode of
    mrmGui:
     begin
       if IsPrototyper and HasActiveData then
         CloseDataActionExecute(nil);
       IsPrototyper := False;
       RunModeImages.GetBitmap(2, AnalyzeBtn.Picture.Bitmap);
       RunModeImages.GetBitmap(3, PrototypeBtn.Picture.Bitmap);
       DataRecentlyUsedFilesSessionsItem.Visible := True;
     end;
    mrmPrototyper, mrmWorkflow:
     begin
       response := ProtoDataTypeDlg.ShowModal;
       if response = mrOk then
       begin
         if FDataType = snTree then
           FDataType := snNoToken;
         DataRecentlyUsedFilesSessionsItem.Visible := False;
         IsPrototyper := True;
         RunModeImages.GetBitmap(0, AnalyzeBtn.Picture.Bitmap);
         RunModeImages.GetBitmap(5, PrototypeBtn.Picture.Bitmap);
         SetProtoDataType(ProtoDataTypeDlg.DataTypeComboBox.ItemIndex);
       end
       else
       begin
         if aMode = mrmWorkflow then
           ShowMessage('Please click the "Prototype" button and select an input data type to work with. Otherwise, the AppFlows system will not work optimally');
         IsPrototyper := False;
         RunModeImages.GetBitmap(2, AnalyzeBtn.Picture.Bitmap);
         RunModeImages.GetBitmap(3, PrototypeBtn.Picture.Bitmap);
         DataRecentlyUsedFilesSessionsItem.Visible := True;
       end;
     end;
  end;
  EnableMenusForActiveDataType;
  EnableMenusForRunMode;
end;

procedure TMegaForm.IQTreeDropdownItemClick(Sender: TObject);
var
  aMenu: TMenuItem = nil;
  command: String;
  aLink: TIQTreeLink = nil;
begin
end;

procedure TMegaForm.DrawMenuItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
begin
  if FIsClosing or (not Assigned(MenuRenderer)) then
    Exit;
  if Visible then
    MenuRenderer.MenuDrawItem(Sender, ACanvas, ARect, AState, FCheckmark, FMenuBitmap);
end;

procedure TMegaForm.DrawPopupMenuItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
begin
  if FIsClosing or (not Assigned(MenuRenderer)) then
    Exit;
  if Visible then
    MenuRenderer.PopupMenuDrawItem(Sender, ACanvas, ARect, AState, FCheckmark);
end;

procedure TMegaForm.MeasureMenuItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
begin
  if not (Sender is TMenuItem) then
    exit;
  if FIsClosing or (not Assigned(MenuRenderer)) then
    Exit;
  MenuRenderer.MeasureMenuItem(Sender, ACanvas, AWidth, AHeight);
end;

procedure TMegaForm.LaunchNucCompositionStatsThread(saveLocation: AnsiString; exportType: TExportType);
var
  t: TNucStatsCompositionThread = nil;
  arp: TRuntimeProgress = nil;
begin
  try
    arp := TRuntimeProgress.Create;
    arp.AddRunStatusInfo('Status', 'Computing nucleotide composition');
    arp.HideAnalysisOptions;
    t := TNucStatsCompositionThread.Create(arp, saveLocation, exportType);
    t.OnTerminate := CompositionStatsThreadDone;
    arp.Show;
    t.Start
  except
    on E:Exception do
      ShowMessage('Failed to launch composition stats thread: ' + E.Message);
  end;
end;

procedure TMegaForm.LaunchAminoAcidCompositionStatsThread(saveLocation: AnsiString; exportType: TExportType);
var
  t: TAminoAcidStatsCompositionThread = nil;
  arp: TRuntimeProgress = nil;
begin
  try
    arp := TRuntimeProgress.Create;
    arp.AddRunStatusInfo('Status', 'Computing amino acid composition');
    arp.HideAnalysisOptions;
    t := TAminoAcidStatsCompositionThread.Create(arp, saveLocation, exportType);
    t.OnTerminate := CompositionStatsThreadDone;
    arp.Show;
    t.Start
  except
    on E:Exception do
      ShowMessage('Failed to launch composition stats thread: ' + E.Message);
  end;
end;

procedure TMegaForm.LaunchCodonUsageThread(saveLocation: AnsiString; exportType: TExportType);
var
  t: TCodonUsageStatsThread = nil;
  arp: TRuntimeProgress = nil;
begin
  try
    arp := TRuntimeProgress.Create;
    arp.AddRunStatusInfo('Status', 'Computing codon usage');
    arp.HideAnalysisOptions;
    t := TCodonUsageStatsThread.Create(arp, saveLocation, exportType);
    t.OnTerminate := CompositionStatsThreadDone;
    arp.Show;
    t.Start
  except
    on E:Exception do
      ShowMessage('Failed to launch composition stats thread: ' + E.Message);
  end;
end;

procedure TMegaForm.LaunchStatPairFreqsThread(saveLocation: AnsiString; exportType: TExportType; isDirectional: Boolean);
var
  t: TStatPairFreqsThread = nil;
  arp: TRuntimeProgress = nil;
begin
  try
    arp := TRuntimeProgress.Create;
    arp.AddRunStatusInfo('Status', 'Computing nucleotide pair frequencies');
    arp.HideAnalysisOptions;
    t := TStatPairFreqsThread.Create(arp, saveLocation, exportType, isDirectional);
    t.OnTerminate := CompositionStatsThreadDone;
    arp.Show;
    t.Start
  except
    on E:Exception do
      ShowMessage('Failed to launch nucleotide pair frequencies thread: ' + E.Message);
  end;
end;

procedure TMegaForm.CompositionStatsThreadDone(aThread: TObject);
var
  t: TStatsCompositionThread = nil;
  ResultList: TStringList = nil;
begin
  try
    try
      t := TStatsCompositionThread(aThread);
      if t.IsCancelled then
        raise EAbort.Create(EmptyStr);
      if not t.IsSuccess then
        raise Exception.Create('Application Error: ' + t.LogText);
      ResultList := t.ResultList;
      case t.ExportType of
        EXtext, EXtextSave:
         begin
           if t.ExportType = EXtextSave then
             ResultList.SaveToFile(t.SaveLocation)
           else
             OpenStringList(ResultList, t.ResultsTitle);
         end;
        EXcsvDisp, EXcsvSave, EXexcelDisp, EXexcelSave, EXexcelXmlDisp,
        EXexcelXmlSave, EXodsDisp, EXodsSave:
         begin
           t.Spreadsheet.SaveFile(t.SaveLocation, ExcelExportToFileType(t.ExportType));
           if ExportIsWorkbookDisplay(t.ExportType) then
             RunAProgram(t.SaveLocation);
         end;
      end;
    except
      on E:EAbort do
        ShowMessage('Composition stats cancelled');
      on E:Exception do
        ShowMessage('Application Error: ' + E.Message);
    end;
  finally
    if Assigned(ResultList) then
      ResultList.Free;
    if Assigned(t.RuntimeProgressRef) then
      t.RuntimeProgressRef.Free;
    D_InputSeqData.HideProgressDlg;
  end;
end;

function TMegaForm.GetDownloadLocation(LocalName: String): String;
var
  Done: Boolean;
  TestFile: TStringList;
  FileLoc: String;
begin
  try
    Result := EmptyStr;
    SaveDialog.FileName := LocalName;
    MessageDlg('You will now be asked where you would like to save the new MEGA version.'+ LineEnding +'Please save it somewhere you will remember and have access to.', mtWarning, [mbOK], 0);

    Done := false;
    while not Done do
    begin
      if SaveDialog.Execute then
      begin
        FileLoc := SaveDialog.FileName;
        TestFile := TStringList.Create;
        try try
        TestFile.Text := 'Testing ability to write to this directory';
        TestFile.SaveToFile(FileLoc);
        Done := True;
        result := FileLoc;
        Except on E: Exception do
          ShowMessage('You can not save to this location, please pick a location you can save to.');
        end;
        finally
          TestFile.Free;
        end;
      end
      else
        Done := True;
    end;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaForm.NotifyNoUpdateAvailable;
begin

end;

procedure TMegaForm.ConstructActiveNames(addToMe: TStringList);
var
  MyUsedOtuInfos: TList;
  i: Integer;
begin
  if not HasActiveData then // check if data file is active
  begin
    Exit;
  end;
  if addToMe = nil then
    Raise Exception.Create('MEGA Has encountered an error.  Could not add Used OTU names to String List because the input stringlist was empty.');

  MyUsedOtuInfos := TList.Create;
  try
    CurrentAllOtuInfos.UsedOtuInfos(MyUsedOtuInfos, False);
    for i := 0 to MyUsedOtuInfos.Count-1 do
    begin
      if TOtuInfo(MyUsedOtuInfos.Items[i]).Name <> EmptyStr then
        addToMe.Add(TOtuInfo(MyUsedOtuInfos.Items[i]).Name);
    end;
  finally
    MyUsedOtuInfos.Free;
  end;
end;

procedure TMegaForm.HidePreviewWindow;
begin
  if PreviewPanel.Visible = False then
    Exit;
  PreviewPanel.Visible := False;
  IsPreviewingButton := nil;
end;

procedure TMegaForm.TrayButtonClick(Sender: TObject);
var
  Btn: TToolButton;
  Form: TForm;
begin
  try
    if (Sender.ClassType <> TToolButton) and (Sender.ClassType <> TImage) then
      raise Exception.Create('Expecting a TToolButton to call this restore procedure!  Sender was a ' + Sender.ClassName);

    if Sender.ClassType = TToolButton then
    begin
      Btn := TToolButton(Sender);
      if Btn.Owner.ClassType = nil then
        raise Exception.Create('Expecting the owner of this button to NOT be nil.');
      HidePreviewWindow;
      Form := TForm(Btn.Owner);
    end
    else
    begin
      if Sender.ClassType = TImage then   // If the user clicks on the image rather than the button we handle it a little differently.
      begin
        if IsPreviewingButton.Owner.ClassType = nil then
          raise Exception.Create('Expecting the owner of this image to NOT be nil.');
        Form := TForm(IsPreviewingButton.Owner);
        HidePreviewWindow;
      end;
    end;

    if Form = MegaInfoForm then
      Form.ShowModal
    else
    begin
      if Form.WindowState = wsMinimized then Form.WindowState := wsNormal;
      Form.Show;
      Form.BringToFront;
      if Form.Enabled then
        Form.SetFocus;
    end;
  except
    on E: Exception do
      ShowMessage('MEGA has encountered an error in retrieving the window you requested.');
  end;
end;

procedure TMegaForm.TrayToolbarButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  try
    magnifyTrayIcon(TToolButton(Sender));
  except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when calling magnifyTrayIcon: ' + E.Message);
  end;
end;

procedure TMegaForm.MagnifyTrayIcon(Btn: TToolButton);
var
  PercentLeft: Double;
  curForm: TForm = nil;
  curFormImage: TBitmap;
begin
  if IsPreviewingButton = Btn then
    Exit;
  PercentLeft := (((Btn.Left + Btn.Width) * 100) / TrayToolbar2.Width) / 100;
  PreviewPanel.Left := max(0, Floor(Btn.Left + (PercentLeft * Btn.Width) - (PercentLeft * PreviewPanel.Width)));
  PreviewPanel.Top := TrayToolbar2.Top + CenterPanel.Top + TrayToolbar2.Height; // position the preview panel below the buttons
  curForm := TForm(Btn.Owner);

  if curForm.ClassType = TTreeViewForm then
  begin
    curFormImage := curForm.GetFormImage;
  end
  else if (curForm.ClassType = TChromiumBrowser) then
  begin
    try
        CurFormImage := CurForm.GetFormImage;
    Except on E: Exception do
      curFormImage := nil;
    end;
  end
  else
    curFormImage := CurForm.GetFormImage;
  {$IFNDEF WIN64} { TODO 1 -oglen -c64-bit : refactor for 64 bit }
  //if curFormImage <> nil then
  //begin
  //  try
  //    curFormImage := ResizeImage(curFormImage, PreviewImg.Width, PreviewImg.Height);
  //    pict := TPicture.Create;
  //    pict.Bitmap.Assign(curFormImage);
  //    PreviewImg.Picture.Assign(pict);
  //    PreviewPanel.Visible := True;
  //    CurExitBtn.Visible := True;
  //  finally
  //    if Assigned(CurFormImage) then
  //      FreeAndNil(CurFormImage);
  //    if Assigned(pict) then
  //      FreeAndNil(pict);
  //  end;
  //end;
  {$ELSE}
  curFormImage := curForm.GetFormImage;
  {$ENDIF}
  IsPreviewingButton := Btn;
end;

procedure TMegaForm.AddWindowToTray(Form: TForm; ShowDataBtn: Boolean=True);
var
  NewButton: TToolButton;
  iconIndex: Integer = -1;
  aHint: String = '';
begin
  try
    NewButton := WindowButtonInTray(Form);
    if Assigned(NewButton) then
      Exit;
    NewButton := TToolButton.Create(Form);
    NewButton.OnClick := TrayButtonClick;
    NewButton.OnMouseMove := TrayToolbarButtonMouseMove;
    if GetFormHintAndIconIndex(form, iconIndex, aHint) then
    begin
      NewButton.ImageIndex := iconIndex;
      if aHint <> EmptyStr then
        NewButton.Hint := aHint;
    end
    else
    begin
      FreeAndNil(NewButton);
      Exit;
    end;
    if not IsTrayForm(Form) then
    begin
      NewButton.Left := 9999; // causes the new button to be inserted to the RIGHT of previous buttons, while data viewers always stay on the left.
    end
    else
    begin
      if ShowDataBtn then
        CloseDataBtn.Visible := True;
    end;

    if IsTrayForm(Form) then
      NewButton.Parent := TrayToolbar2
    else
      NewButton.Parent := PinupsToolbar;
  except on E: Exception do
    ShowMessage(E.Message);
  end;
end;

procedure TMegaForm.AddDropdownToTray(dropDown: TPopupMenu; imageIndex: Integer; aHint: String);
var
  NewButton: TToolButton = nil;
begin
  try
    NewButton := TToolButton.Create(Self);
    NewButton.DropdownMenu := dropDown;
    NewButton.ImageIndex := imageIndex;
    NewButton.Parent := PinupsToolbar;
    NewButton.Left := 9999;
    NewButton.Hint := aHint;
    dropDown.OnDrawItem := DrawPopupMenuItem;
    dropDown.OnMeasureItem := MeasureMenuItem;
  except on E: Exception do
    ShowMessage(E.Message);
  end;
end;

procedure TMegaForm.RemoveDropdownFromTray(dropdown: TPopupMenu);
var
  i: Integer;
  btn: TToolButton;
begin
  if PinupsToolbar.ButtonCount > 0 then
    for i := PinupsToolbar.ButtonCount - 1 downto 0 do
    begin
      btn := PinupsToolbar.Buttons[i];
      if Assigned(btn.DropdownMenu) and (btn.DropdownMenu = dropdown) then
      begin
        PinupsToolbar.Buttons[i].Free;
        Exit;
      end;
    end;
end;

function TMegaForm.GetFormIcon(form: TForm; var up: TBitmap; var hot: TBitmap): Boolean;
var
  iconFound: Boolean;
begin
  iconFound := false;
  try
    if SameText(form.ClassName, 'TTreeViewForm') then
    begin
      PinupImages.GetBitmap(11, up);
      PinupImagesHot.GetBitmap(11, hot);
      iconFound := true;
    end
    else
    if SameText(form.ClassName, 'TV_SeqDataExplorer') then
    begin
      if TrayImages.Count < 2 then
        Exception.Create('invalid number of icons');
      if ExplorersDisabledForLargeData then
      begin
        TrayImages.GetBitmap(14, up);
        TrayIconHot.GetBitmap(14, hot);
      end
      else
      begin
        TrayImages.GetBitmap(1, up);
        TrayIconHot.GetBitmap(1, hot);
      end;

      iconFound := true;
    end
    else
    if SameText(form.ClassName, 'TAlignEditMainForm') then
    begin
      PinupImages.GetBitmap(7, up);
      PinupImagesHot.GetBitmap(7, hot);
      iconFound := true;
    end
    else
    if SameText(form.ClassName, 'TEditorForm') then
    begin
      PinupImages.GetBitmap(0, up);
      PinupImagesHot.GetBitmap(0, hot);
      iconFound := true;
    end
    else
    if SameText(form.ClassName, 'TDisplayMatrixDlg') then
    begin
      PinupImages.GetBitmap(5, up);
      PinupImages.GetBitmap(5, hot);
      iconFound := true;
    end
    else
    if SameText(form.ClassName, 'TTraceEditForm') then
    begin
      PinupImages.GetBitmap(5, up);
      PinupImagesHot.GetBitmap(5, hot);
      iconFound := true;
    end
    else if SameText(form.ClassName, 'TChromiumBrowser') then
    begin
      // check if this is the result from an analysis shown as a caption
     if (form as TChromiumBrowser).BrowserMode = bmResults then
     begin
       PinupImages.GetBitmap(1, up);
       PinupImagesHot.GetBitmap(1, hot);
       iconFound := true;
     end
     else if (form as TChromiumBrowser).BrowserMode = bmBrowser then
     begin
       TrayImages.GetBitmap(6, up);
       TrayIconHot.GetBitmap(6, hot);
       iconFound := true;
     end
     else if (form as TChromiumBrowser).BrowserMode = bmCaption then
     begin
       PinupImages.GetBitmap(2, up);
       PinupImagesHot.GetBitmap(2, hot);
       iconFound := true;
     end
     else if (form as TChromiumBrowser).BrowserMode = bmDefaultBrowser then
     begin
       TrayImages.GetBitmap(6, up);
       TrayIconHot.GetBitmap(6, hot);
       iconFound := true;
     end
     else
     begin
       result := false;
       Exit;
     end;
    end
    else if SameText(form.ClassName, 'TV_DistDataExplorer') then
    begin
      TrayImages.GetBitmap(8, up);
      TrayIconHot.GetBitmap(8, hot);
      iconFound := true;
    end
    else if SameText(form.ClassName, 'TTreeInputForm') then
    begin
      TrayImages.GetBitmap(9, up);
      TrayIconHot.GetBitmap(9, hot);
      iconFound := true;
    end
    else if SameText(Form.ClassName, 'TCalibrationDlg') then
    begin
      TrayImages.GetBitmap(12, up);
      TrayIconHot.GetBitmap(12, hot);
      IconFound := True;
    end
    else if SameText(Form.ClassName, 'TTimeTreeWizardForm') then
    begin
      TrayImages.GetBitmap(13, up);
      TrayIconHot.GetBitmap(13, hot);
      IconFound := True;
    end
    else if SameText(Form.ClassName, 'TAnalysisWizardForm') then
    begin
      TrayImages.GetBitmap(13, up);
      TrayIconHot.GetBitmap(13, hot);
      IconFound := True;
    end;

    if not IconFound then
      Showmessage('MEGA has encountered an error, we could not locate the proper icon for the form: ' + form.classname);
  Except on E: exception do
    ShowMessage(E.message);
  end;
  Result := IconFound;
end;

function TMegaForm.GetFormHintAndIconIndex(const form: TForm; var Index: Integer; var aHint: String): Boolean;
begin
  Result := False;
  try
    if SameText(form.ClassName, 'TTreeViewForm') then
    begin
      Index := 11;
      aHint := 'Tree Explorer';
      Result := true;
    end
    else
    if SameText(form.ClassName, 'TV_SeqDataExplorer') then
    begin
      Index := 1;
      if ExplorersDisabledForLargeData then
        aHint := 'Sequence Data Explorer is disabled because the active data is too large to display'
      else
        aHint := 'Sequence Data Explorer';
      Result := true;
    end
    else
    if SameText(form.ClassName, 'TAlignEditMainForm') then
    begin
      Index := 7;
      aHint := 'Alignment Editor';
      Result := true;
    end
    else
    if SameText(form.ClassName, 'TEditorForm') then
    begin
      Index := 0;
      aHint := 'Text Editor';
      Result := true;
    end
    else
    if SameText(form.ClassName, 'TDisplayMatrixDlg') then
    begin
      Index := 5;
      aHint := 'Matrix Display';
      Result := true;
    end
    else
    if SameText(form.ClassName, 'TTraceEditForm') then
    begin
      Index := 8;
      aHint := 'Trace Editor';
      Result := true;
    end
    else if SameText(form.ClassName, 'TChromiumBrowser') then
    begin
      // check if this is the result from an analysis shown as a caption
     if (form as TChromiumBrowser).BrowserMode = bmResults then
     begin
       Index := 1;
       aHint := 'Results Browser';
       Result := true;
     end
     else if (form as TChromiumBrowser).BrowserMode = bmBrowser then
     begin
       Index := 6;
       aHint := 'Web Browser';
       Result := true;
     end
     else if (form as TChromiumBrowser).BrowserMode = bmCaption then
     begin
       Index := 2;
       aHint := 'Caption Expert';
       Result := true;
     end
     else if (form as TChromiumBrowser).BrowserMode = bmDefaultBrowser then
     begin
       Index := 6;
       aHint := 'Web Browser';
       Result := true;
     end
     else
     begin
       Result := False;
       Exit;
     end;
    end
    else if SameText(form.ClassName, 'TV_DistDataExplorer') then
    begin
      Index := 8;
      aHint := 'Distance Data Explorer';
      Result := true;
    end
    else if SameText(form.ClassName, 'TTreeInputForm') then
    begin
      Index := 9;
      aHint := 'Tree Input Form';
      Result := true;
    end
    else if SameText(Form.ClassName, 'TCalibrationDlg') then
    begin
      Index := 12;
      aHint := 'Calibration Times Editor';
      Result := True;
    end
    else if SameText(Form.ClassName, 'TTimeTreeWizardForm') then
    begin
      Index := 13;
      aHint := 'Time Tree Wizard';
      Result := True;
    end
    else if SameText(Form.ClassName, 'TAnalysisWizardForm') then
    begin
      Index := 13;
      aHint := 'Analysis Wizard';
      Result := True;
    end
    else if SameText(Form.ClassName, 'TMegaInfoForm') then
    begin
     Index := 14;
     aHint := 'Sequence Data Explorer is disabled because the active data is too large to display';
     Result := True;
    end;

    if not Result then
      Showmessage('MEGA has encountered an error, we could not locate the proper icon for the form: ' + form.classname);
  Except
    on E: exception do
      ShowMessage(E.message);
  end;
end;

procedure TMegaForm.RemoveWindowFromTray(Form: TForm);
var
  ButtonToRemove: TToolButton;
begin
  if not Assigned(Form) then
    Exit;
  try
    if (Assigned(TrayToolBar2) and (TrayToolbar2.ControlCount = 0)) and (Assigned(PinupsToolbar) and (PinupsToolbar.ControlCount = 0)) then
       Exit;
    ButtonToRemove := WindowButtonInTray(Form);
    if Assigned(ButtonToRemove) then
    begin
      if IsTrayForm(Form) then
      begin
       if (Form.ClassType = TV_SeqDataExplorer) or (Form.ClassType = TV_DistDataExplorer) or (Form.ClassType = TMegaInfoForm) then
          CloseDataBtn.Visible := False;
        TrayToolBar2.ButtonList.Remove(ButtonToRemove);
        TrayToolbar2.RemoveControl(ButtonToRemove);
      end
      else
      begin
        PinupsToolbar.ButtonList.Remove(ButtonToRemove);
        PinupsToolbar.RemoveControl(ButtonToRemove);
      end;
      if Assigned(ButtonToRemove.Owner) then
        ButtonToRemove.Owner.RemoveComponent(ButtonToRemove);
      FreeAndNil(ButtonToRemove);
    end;
    if Assigned(PreviewPanel) then
      PreviewPanel.Visible := False;
    IsPreviewingButton := nil;
  except
    on E:Exception do
       ShowMessage('Error in RemoveWindowFromTray: ' + E.Message);
  end;
end;

function TMegaForm.ResizeImage(Img: TBitmap; ImgWidth: Integer; ImgHeight: Integer): TBitmap;
//var
//  bit: TBitmap;
//  DrawRect: TRect;
begin
  Result := nil;
  //try
  //  bit := TBitmap.Create;
  //  bit.Width := ImgWidth;
  //  bit.Height := ImgHeight;
  //  SmoothResize(Img, bit);
  //  result := bit;
  //  Img.Free;
  //except
  //  result := nil;
  //end;
end;

function TMegaForm.WindowButtonInTray(Form: TForm): TToolButton;
var
  i: integer;
begin
  Result := nil;

  try
    if IsTrayForm(Form) and Assigned(TrayToolbar2) and (TrayToolbar2.ControlCount > 0) then
      for i := 0 to TrayToolbar2.ControlCount - 1 do
        if TrayToolbar2.Controls[i].ClassType = TToolButton then
        begin
          if TToolButton(TrayToolbar2.Controls[i]).Owner = Form then
          begin
            Result := TrayToolbar2.Controls[i] as TToolButton;
            Exit;
          end;
        end;

    if Assigned(PinupsToolbar) and (PinupsToolbar.ControlCount > 0) then
      for i := 0 to PinupsToolbar.ControlCount - 1 do
      begin
        if PinupsToolbar.Controls[i].ClassType = TToolButton then
        begin
          if TToolButton(PinupsToolbar.Controls[i]).Owner = Form then
          begin
             Result := PinupsToolbar.Controls[i] as TToolButton;
             Exit;
          end;
        end;
      end;
  except
     on E:Exception do
       ShowMessage('Error in WindowButtonInTray: ' + E.Message);
  end;
end;

procedure TMegaForm.UpdateWindowInTray(Form: TForm);
begin
  AddWindowToTray(Form);
end;

procedure TMegaForm.UpdateMainWindow(FileName : String = ''; Title : String = ''; Description: String = ''; FDType : TSnTokenCode  = snNucleotide);
begin
  if (D_InputSeqData = nil) and (D_InputDistData = nil) then
    Exit;

  FDataType := FDType;
  SetDataFile(FileName);
  DataTitle := Title;
  FDataFileDescription := Description;
  if  V_SeqDataExplorer <> nil then
  begin
    MegaForm.ExploreDataAction.ImageIndex := 17;
    if (FDataType = snNucleotide) and (D_InputSeqData.IsCoding) then
    begin
      CodeTableName := D_InputSeqData.CodeName;
      CodeTable     := D_InputSeqData.CodeTable;
    end;
  end
  else if  V_DistDataExplorer <> nil then
  begin
    MegaForm.ExploreDataAction.ImageIndex := 5;
  end;
end;

function TMegaForm.GetCodeTable: String;
begin
  if FCodeTableName = EmptyStr then
  begin
    SetCodeTableName('Standard');
    SetCodeTable(GetStandardGeneticCode);
  end;
  Result := FCodeTable;
end;

procedure TMegaForm.SetCodeTable(AValue: String);
begin
  FCodeTable := AValue;
end;

function TMegaForm.GetCodeTableName: String;
begin
  if FCodeTable = EmptyStr then
  begin
    SetCodeTableName('Standard');
    SetCodeTable(GetStandardGeneticCode);
  end;
  Result := FCodeTableName;
end;

procedure TMegaForm.SetCodeTableName(AName: String);
begin
  FCodeTableName := AName;
end;

function TMegaForm.GetDataFileName: String;
begin
  Result := FDataFileName;
end;

function TMegaForm.GetDataTitle: String;
begin
  Result := FDataTitle;
end;

function TMegaForm.GetDataDescr: String;
begin
  Result := FDataFileDescription;
end;

procedure TMegaForm.PromptCloseCurrentlyActive;
begin
  if isPrototyper then
  begin
    MegaForm.CloseDataActionExecute(nil); // Prototyper switches files on list box change.
    Exit;
  end;
  try
    if MegaForm.HasActiveData then
    begin
      if (MessageDlg('A file is already activated for analysis. Would you like to use another?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        MegaForm.CloseDataActionExecute(nil);  // Asks users if they would like to save their session.
      end
      else
        Exit;
    end;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaForm.ToggleToolbarButtonVisiblity(Sender: TObject);
begin

end;

procedure TMegaForm.GetFileAndOpenData(DefExt: String; aNotify: TNotifyEvent = nil);
var
  curext: string;
begin
  try
    {$IFDEF DEBUG}
    if IsGlenDevMode then
    begin
      DoOpenDataFile(GlenSeqdataFile, True);
      exit;
    end;
    {$ENDIF}
    with ADataOpenDlg do
    begin
      Title       := 'Choose a Data File to Analyze';
      DefaultExt  := DefExt;

      if Length(OpenFileDirSTxt.Caption) > 0 then
      begin
        FileName    := ExtractFileName(OpenFileDirSTxt.Caption);
        InitialDir  := ExtractFileDir(OpenFileDirSTxt.Caption);
      end
      else
      begin
        FileName    := EmptyStr; // '*.' + DefExt;
        InitialDir  := GetHomeDirectory + 'MEGA' + IntToStr(MAJOR_VERSION) + PathDelim + 'Examples';
      end;

      if DefExt = 'meg' then
        Filter      := MegaFilesFilter+'|'+MegaSessionFilesFilter+'|'+FastaFilesFilter+'|'+AllFilesFilter
      else if DefaultExt = 'mdsx' then
        Filter      := MegaSessionFilesFilter+'|'+MegaFilesFilter+'|'+FastaFilesFilter+'|'+AllFilesFilter;
      FilterIndex := 0;
      HelpContext := HC_Open_Data;
      Options     := [ofReadOnly,ofHideReadOnly,ofShowHelp, ofPathMustExist,ofFileMustExist];
      if FileName <> '' then
      begin
        curext := ExtractFileExt(FileName);
        if curext = '.meg' then
          FilterIndex := 1
        else if curext = '.mdsx' then
          FilterIndex := 2
        else if (curext = '.fas') or (curext = '.fasta') then
          FilterIndex := 3
        else
          FilterIndex := 4;
      end;
      if not Execute then
        Exit;
    end;

  OpenFileDirSTxt.Caption := ADataOpenDlg.FileName;
  PromptCloseCurrentlyActive;
  if HasActiveData then  // They might have clicked NO, in which case we just don't want to procede.
    Exit;
  DoOpenDataFile(ADataOpenDlg.FileName, True, aNotify);
  Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

function TMegaForm.AskUserToActivateDataFile(aNotify: TNotifyEvent = nil): Boolean;
var
  checked: Boolean = False;
  QuestionRes: String = '';
begin
  if isPrototyper then
  begin
    Result := True;
    if not HasActiveData then
      SetProtoDataType(0);
    Exit;
  end;

  Result := False;
  if HasActiveData then
  begin
    if not TryToGetPreferencesMainMenu(UserPref_MegaMainAlwaysUseCurActiveDataStr, checked, QuestionRes) then
      checked := false;  // if they haven't set any preference then assume they want to see this every time.
    if IsSessionTest then
      checked := True;
    if not checked then
    begin
      if MessageDlgCheck(Self, 'Would you like to use the currently active data ('+ ExtractFileName(FDataFileName) + ')?',
                         'Use the active file?', UserPref_AlwaysUseCurActiveDataStr, checked, mtConfirmation, [mbYES, mbNO], 0) = mrYes then
      begin
        Result := True;
        UpdatePreferencesMainMenu(UserPref_MegaMainAlwaysUseCurActiveDataStr, checked, BoolToStr(True, True));
        Exit;
      end
      else
        UpdatePreferencesMainMenu(UserPref_MegaMainAlwaysUseCurActiveDataStr, checked, BoolToStr(False, True));
    end
    else
      if QuestionRes = BoolToStr(true, True) then
      begin
        Result := True;
        Exit;
      end;
    if not SaveSessionWithPermission then
      Exit;
    ClearData;
    UpdateMainWindow;
  end;

  GetFileAndOpenData('meg', aNotify);
  Result := HasActiveData;
end;

function TMegaForm.AskUserToKeepDataActive: Boolean;
var
  PreferencesSet: Boolean = False;
  UserPreference: String = '';
  Response: Integer;
begin
  if isPrototyper then
  begin
    Result := HasActiveData;
    Exit;
  end;

  Result := False;
  if HasActiveData then
  begin
    if not TryToGetPreferencesMainMenu(UserPref_MegaMainAlwaysUseCurActiveDataStr, PreferencesSet, UserPreference) then
      PreferencesSet := False;  // if they haven't set any preference then assume they want to see this every time.

    if PreferencesSet then
    begin
      if UserPreference = BoolToStr(True, True) then { the user want's to use the active data}
        Result := True
      else
      begin   { the user does not want to use the active data}
        SaveSessionWithPermission;
        ClearData;
        UpdateMainWindow;
        Result := True;
        UpdatePreferencesMainMenu(UserPref_MegaMainAlwaysUseCurActiveDataStr, PreferencesSet, BoolToStr(False, True));
      end;
    end
    else
    begin
      Response :=  MessageDlgCheck(Self, 'Would you like to use the currently active data ('+ ExtractFileName(DataFileName) + ')?', 'Use the active file?', UserPref_AlwaysUseCurActiveDataStr, PreferencesSet, mtConfirmation, [mbYES, mbNO, mbCancel], 0);
      if Response = mrYes then {use the active data}
      begin
        Result := True;
        UpdatePreferencesMainMenu(UserPref_MegaMainAlwaysUseCurActiveDataStr, PreferencesSet, BoolToStr(True, True));
      end
      else if Response = mrNo then {close the active data}
      begin
        SaveSessionWithPermission;
        ClearData;
        UpdateMainWindow;
        Result := True;
        UpdatePreferencesMainMenu(UserPref_MegaMainAlwaysUseCurActiveDataStr, PreferencesSet, BoolToStr(False, True));
      end
      else { Response = mrCancel so don't close anything and don't continue}
      begin
        Result := False;
      end;
    end;
  end
  else
    Result := True;
end;

function TMegaForm.AskUserToSpecifyCodons: Boolean;
begin
  try
    Result := False;
    if (not HasActiveData) or HasCodonData then
      Exit;

    if (D_InputSeqData = nil) or (D_InputSeqData.IsAmino) then
      Exit;

    if not D_InputSeqData.IsCoding then
      Exit;

    if (MessageDlg('You need to specify codons for this analysis.'+ LineEnding +'Would you like to translate to Amino and select a genetic code table?', mtInformation, [mbYes, mbNo], 0) = mrYes) then
    begin
      if DataSelectGeneticCodeAction.Execute then
        D_InputSeqData.DoTranslation;
    end;
    Result := HasCodonData;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaForm.SelectDataSubsetPreferences;
begin

end;

procedure TMegaForm.SaveSession(PromptUserForLocation: boolean);
var
  ExceptionText : AnsiString;
begin
  try
    ExceptionText := 'Error: Neither Sequence data nor Distance data is present, can not save a file without data.';
    if PromptUserForLocation then  //Use the GUI and ask the user where to save the data
    begin
      if (V_SeqDataExplorer <> nil) then
        V_SeqDataExplorer.ActionSaveSessionExecute(Self)
      else if V_DistDataExplorer <> nil then
        V_DistDataExplorer.SaveSession.Click
      else
        ShowErrorMessage(Exception.Create(ExceptionText));
    end
    else   //Only used when the user is already using a session, saves current data to the session file they opened.
    begin
      if D_InputSeqData <> nil then
        D_InputSeqData.SaveSession(DataFileName)
      else if D_InputDistData <> nil then
        D_InputDistData.SaveSession(DataFileName)
      else
        ShowErrorMessage(Exception.Create(ExceptionText));
    end;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

function TMegaForm.SaveSessionWithPermission: Boolean;
{$IFNDEF DEBUG}
var
  AResult : WORD;
{$ENDIF}
begin
  try
    Result := True;
    if HasDataSubsetChanged or HasDataViewChanged then
    begin
      if ExtractFileExt(GetDataFileName) = '.mdsx' then
      begin
        if (MessageDlg('Would you like to save your changes back to the open MDSX session file?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
          SaveSession(False);
      end
      else if HasDataSubsetChanged then
      begin
        {$IFNDEF DEBUG} { don't prompt developers. It is just annoying. }
        AResult := MessageDlg('Your data subset selection can be preserved by creating a session file. ' + NewLine +
                               'Would you like to create one?',
                    mtConfirmation, [mbYes, mbNo, mbCancel],0);
        case AResult of
          mrYes    : SaveSession(True);
          mrCancel : Result := False;
        end;
        {$ENDIF}
      end
      else if HasDataViewChanged then
      begin
        {$IFNDEF DEBUG}
        AResult := MessageDlg('You can preserve your data view by creating a session file.'+NewLine+
                      'Would you like to create one?',
                       mtConfirmation, [mbYes, mbNo, mbCancel],0);
         case AResult of
          mrYes    : SaveSession(True);
          mrCancel : Result := False;
        end;
        {$ENDIF}
      end;
    end;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

function TMegaForm.HasActiveData: Boolean;
begin
  Result := FDataType <> snNoToken;
end;

function TMegaForm.HasSequenceData: Boolean;
begin
  Result := Assigned(D_InputSeqData);
end;

function TMegaForm.HasDistanceData: Boolean;
begin
  Result := (Assigned(D_InputDistData) and (DataType = snDistance)) ;
end;

function TMegaForm.HasMultiFileData: Boolean;
begin
  Result := False;
end;

function TMegaForm.HasNucleotideData: Boolean;
begin
  Result := Assigned(D_InputSeqData);
  if Result then
    Result := D_InputSeqData.IsNuc;
end;

function TMegaForm.HasAminoAcidData: Boolean;
begin
  Result := Assigned(D_InputSeqData);
  if Result then
    Result := D_InputSeqData.IsAmino or (D_InputSeqData.IsNuc and D_InputSeqData.IsCoding);
end;

function TMegaForm.HasCodonData: Boolean;
begin
  Result := Assigned(D_InputSeqData);
  if Result then
    Result := (D_InputSeqData.IsNuc and D_InputSeqData.IsCoding);
end;

function TMegaForm.HasDataSubsetChanged: Boolean;
begin
  Result := False;
  if V_SeqDataExplorer <> nil then
    Result := V_SeqDataExplorer.HasDataSubsetChanged
  else if V_DistDataExplorer <> nil then
    Result := V_DistDataExplorer.HasDataSubsetChanged;
end;

function TMegaForm.hasDataViewChanged: Boolean;
begin
  Result := False;
   if V_SeqDataExplorer <> nil then
     Result := V_SeqDataExplorer.HasDataViewChanged
   else if V_DistDataExplorer <> nil then
     Result := V_DistDataExplorer.HasDataViewChanged;
end;

function TMegaForm.GetCitationHtmlString: String;
var
  CSS: String;
  fontName: String;
begin
  {$IFDEF MSWINDOWS}
  fontName := 'Open Sans';
  {$ELSE}
  fontName := 'OpenSymbol';
  {$ENDIF}
  fontName := 'Arial, Helvetica, sans-serif';
  CSS := '<style type="text/css">' +
         'body { font-family: ' + fontName + '; ' +
         'color:#333333;' +
         'background-color: #FFFFFF;' +
         'font-size: 14px; ' +
         'margin: 2px 0px 2px 0px; }' +
         '</style>';

  Result := '<html><head>' + CSS + '</head><body>';
  Result := Result + '<h4>Citing MEGA in Publications</h4>';

  Result := Result + '<p>If you wish to cite MEGA in your publications, we suggest the following:' +
    '<ul>' +
    '<li>When referring to MEGA in the main text of your publication, you may choose a format such as:</br>' +
    Format('<blockquote>Phylogenetic and molecular evolutionary analyses were conducted using MEGA version %d (%s %s).</blockquote></li>', [MAJOR_VERSION, GUI_CITATION_AUTHORS, GUI_CITATION_YEAR]) +
    '<li>When including a MEGA citation in the Literature Cited/Bibliography section, you may use the following:' +
    MEGA_GUI_Citation('html') +
    '</ul>';
  Result := Result + '<p>Publication PDF available at: <b><a href="https://www.megasoftware.net/citations">' + WEBSITE_URL + '</a></b></blockquote></p>';
  Result := Result + '</body></html>';
end;

function TMegaForm.DataTypeString: String;
begin
  if HasDistanceData then
    Result := 'distances'
  else if HasCodonData then
    Result := 'coding'
  else if HasNucleotideData then
    Result := 'nucleotide'
  else if HasAminoAcidData then
    Result := 'protein'
  else
    Result := EmptyStr;
end;

function TMegaForm.DataFileNameOnly: String;
begin
  Result := ExtractFileNameOnly(DataFileName);
end;

procedure TMegaForm.TajimaNeutralityTestDone(aThread: TObject);
var
  ARP : TRuntimeProgress = nil;
  MAI: TAnalysisInfo = nil;
  t: TTajimaNeutralityTestThread = nil;
  summary: TAnalysisSummary;
begin
  t := TTajimaNeutralityTestThread(aThread);
  MAI := t.AnalysisInfo;
  ARP := MAI.ARP;
  ARP.UpdateRunStatusInfo('Status', 'Preparing result display');
  if t.Cancelled then
    Exit;
  if t.IsSuccess then
  begin
    summary := MAI.AnalysisSummary;
    ChromiumBrowser := CreateNewChromiumBrowserWindow(bmResults);
    ChromiumBrowser.LoadHtmlFromString(t.GetLegend);
    ChromiumBrowser.SetMatrixExport(t.GetSpreadsheetExport);
    t.UpdateSummary(summary);
    ARP.Hide;
  end
  else
  begin
    ARP.Hide;
    ShowMessage('Oh no! Tajima neutrality test failed: ' + t.Log);
  end;
end;

procedure TMegaForm.TajimaClockTestDone(aThread: TObject);
var
  Summary: TAnalysisSummary = nil;
  MAI: TAnalysisInfo = nil;
  t: TTajimaClockTestThread = nil;
begin
  t := TTajimaClockTestThread(aThread);
  if t.Cancelled then
    Exit;
  try
    if t.IsSuccess then
    begin
      MAI := t.AnalysisInfo;
      ChromiumBrowser := CreateNewChromiumBrowserWindow(bmResults);
      ChromiumBrowser.LoadHTMLFromString(t.GetLegendAsText);
      ChromiumBrowser.SetMatrixExport(t.GetSpreadsheetExport);
      Summary := MAI.AnalysisSummary;
      t.UpdateSummary(Summary);
    end
    else
      ShowMessage('Oh no! Tajima test failed: ' + t.Log);
  finally
    if Assigned(MAI) and Assigned(MAI.ARP) then
      FreeAndNil(MAI.ARP);
  end;
end;

procedure TMegaForm.AddSettingsToPopupMenu(aMenu: TPopupMenu);
begin
  aMenu.OwnerDraw := True;
  aMenu.OnDrawItem := DrawMenuItem;
  aMenu.OnMeasureItem := MeasureMenuItem;
end;

procedure TMegaForm.UpdateCollectDataPreference(Sender: TObject);
begin

end;

procedure TMegaForm.SessionTestFinished(Sender: TObject);
begin
  if IsLegacySessionTest then { only used for new session tests - needed because threads are launched and when they finish, they make this callback}
    Exit;
  try
    FSessionTestCriticalSection.Acquire;
    dec(FNumSessionTests);
  finally
    FSessionTestCriticalSection.Release;
  end;

  if FNumSessionTests = 0 then
  begin
    AppendSessionLog('all session tests completed');
    FSessionTestCriticalSection.Free;
    Halt(0);
  end;
end;

procedure TMegaForm.BuildAlignmentActionExecute(Sender: TObject);
var
  editor: TAlignEditMainForm;
begin
  try
    editor := FindAlignmentEditorWindow(False);
    if Assigned(editor) then
      editor.ShowWelcomeMessage
    else
      raise Exception.Create('window does not exist');
  Except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when activating the Alignment Explorer: ' + E.Message);
  end;
end;

procedure TMegaForm.CheckBetaExpiredTimerTimer(Sender: TObject);
var
  Year, Month, Day: Word;
begin
  CheckBetaExpiredTimer.Enabled := False;
  if (MegaReleaseType <> mrtBeta) and (MegaReleaseType <> mrtAlpha) then
    Exit;
  DecodeDate(Now, Year, Month, Day);
  if (Year > Beta_Expires_Year) or ((Year = Beta_Expires_Year) and (Month > Beta_Expires_Month))  then
  begin
    if MegaReleaseType = mrtBeta then
      ShowMessage('The current BETA test version of MEGA has expired.  We recommend that you obtain an updated version from http://www.megasoftware.net')
    else
      ShowMessage('The current ALPHA test version of MEGA has expired.  We recommend that you obtain an updated version from http://www.megasoftware.net')
  end;
end;

procedure TMegaForm.CheckUpdatesTimerTimer(Sender: TObject);
begin
  CheckUpdatesTimer.Enabled := False;
  LaunchUpdateThread(False);
end;

procedure TMegaForm.CitationBtnClick(Sender: TObject);
var
  aBrowser: TChromiumBrowser;
  callStack: TStringList;
begin
  try
    aBrowser := CreateNewChromiumBrowserWindow(bmCaption, Self);
    if Assigned(aBrowser) then
      aBrowser.LoadHtmlFromString(GetCitationHtmlString)
    else
      raise Exception.Create('Failed to create chromium browser');
  Except
    on E: Exception do
    begin
      callStack := DumpExceptionCallStack(E);
      ShowMessage(E.Message + LineEnding + callStack.Text);
      callStack.Free;
    end;
  end;
end;

procedure TMegaForm.CitationBtnMouseEnter(Sender: TObject);
begin
  BtRed.GetBitmap(BT_IMG_CITATION, CitationBtn.Picture.Bitmap);
end;

procedure TMegaForm.CitationBtnMouseLeave(Sender: TObject);
begin
  BtBlue.GetBitmap(BT_IMG_CITATION, CitationBtn.Picture.Bitmap);
end;

procedure TMegaForm.ClocksMenuImgMouseEnter(Sender :TObject);
begin
  Hover72x90.GetBitmap(MEN_IMG_CLOCKS, ClocksMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.ClocksMenuImgMouseLeave(Sender :TObject);
begin
  Default72x90.GetBitmap(MEN_IMG_CLOCKS, ClocksMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.ClocksToolMenuItemClick(Sender: TObject);
begin
  ClocksToolMenuItem.Checked := not ClocksToolMenuItem.Checked;
  ClocksMenuImg.Visible := ClocksToolMenuItem.Checked;
  AlignTopToolbarButtons;
end;

procedure TMegaForm.CloseDataActionExecute(Sender: TObject);
begin
  if not HasActiveData then
    Exit;
  try
    if not IsPrototyper then
    begin
      if SaveSessionWithPermission then
      begin
        ClearData;
        MainStatusBar.Panels[MainStatusBar.Panels.Count - 1].Text := EmptyStr; // Wasn't previously clearing when closing a file.
        UpdateMainWindow;
      end;
    end
    else
      ClearData;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaForm.CloseDataBtnClick(Sender: TObject);
begin
  if HtmlOptionsDialog.Visible then
    HtmlOptionsDialog.Hide;
  try
    CloseDataActionExecute(Sender);
  Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaForm.CodonFisherTestActionExecute(Sender: TObject);
var
  ProcessPack : TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(CodonFisherTestActionExecute) then
    Exit;
  if not HasCodonData then
    AskUserToSpecifyCodons;

  if not HasCodonData then
  begin
    ShowMessage('Codon-based tests of selection require protein-coding DNA sequences.');
    Exit;
  end;
  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppFisherExact);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.CodonUsageActionExecute(Sender: TObject);
var
  ExportAs: TExportType;
  SaveLocation : String;
begin
  if (Sender <> V_SeqDataExplorer) and (not AskUserToActivateDataFile(CodonUsageActionExecute)) then
    Exit;

  if not HasCodonData then
  begin
    ShowMessage('You need to have protein coding DNA sequences for this function.');
    Exit;
  end;

  if VS_SeqdataExplorer.IsTranslated then
  begin
    ShowMessage('The data explorer currently contains translated amino acid sequences.  Please Untranslate.');
    Exit;
  end;

  SaveLocation := DataFileNameOnly + '_codon-usage';
  if Assigned(V_SeqDataExplorer) then
    ExportAs := V_SeqDataExplorer.SelectedStatsExportType
  else
    ExportAs := EXexcelXMLDisp;
  ExportAs := PromptUserWriteOutput(SaveLocation, VS_SeqDataExplorer.CurAttrDisp = megNone, ExportAs);
  if ExportAs = EXnone then
    exit;
  try
    if Assigned(V_SeqDataExplorer) then
      V_SeqDataExplorer.SelectedStatsExportType := ExportAs;
    VS_SeqdataExplorer.StatUseOnlyMarkedSitesItem := WriteOutputDlg.IsUseHighlightedSites;
    VS_SeqdataExplorer.StatUseOnlyUnMarkedSitesItem := WriteOutputDlg.IsUseUnHighlightedSites;
    VS_SeqdataExplorer.StatAllSelSitesItem := WriteOutputDlg.IsUseAllSites;
    LaunchCodonUsageThread(SaveLocation, ExportAs);
  Except
    on E: Exception do
      ShowMessage('Unable to calculate Codon Usage Bias Error: ' + E.Message);
  end;
 end;

procedure TMegaForm.CodonZTestActionExecute(Sender: TObject);
var
  ProcessPack : TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(CodonZTestActionExecute) then
    Exit;
  if not HasCodonData then
    AskUserToSpecifyCodons;

  if not HasCodonData then
  begin
    ShowMessage('Codon-based tests of selection require protein-coding DNA sequences.') ;
    Exit;
  end;
  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppZtest);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
      if Assigned(ProcessPack) then
    ProcessPack.Free;
  end;
end;

procedure TMegaForm.CompositionDistanceActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(CompositionDistanceActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
    ShowMessage('Composition distance calculations require sequence data.');
    Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppEstCompDist);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.ComputeTimetreeLSActionExecute(Sender: TObject);
var
  ProcessPack : TProcessPack = nil;
begin
  if not AskUserToKeepDataActive then
    Exit;

  try
    try
      if IsPrototyper then
      begin
        ProcessPack := TProcessPack.Create();
        if (Sender = ReltimeOlsMenuItem) or (Sender = ReltimeOLSMenu2) then
          ProcessPack.AddProcessType(ppRelTimeLS)
        else if (Sender = RtdtLsMenuItem) or (Sender = RtdtOLSMenu2) then
          ProcessPack.AddProcessType(ppRtdtLs)
        else
          raise Exception.Create('Invalid caller for Reltime OLS action');
        ProcessPack.PerformAnalysis;
      end
      else
      begin
        if Assigned(TimeTreeWizard) then
          FreeAndNil(TimeTreeWizard);
        TimeTreeWizard := TTimeTreeWizard.Create;
        if Assigned(TimeTreeWizardForm) then
          FreeAndNil(TimeTreeWizardForm);
        TimeTreeWizardForm := TTimeTreeWizardForm.Create(Self);
        TimeTreeWizardForm.Wizard := TimeTreeWizard;
        TimeTreeWizard.WizardFormUpdate := TimeTreeWizardForm.UpdateView;
        AddWindowToTray(TimeTreeWizardForm);
        ProcessPack := TProcessPack.Create();
        if (Sender = ReltimeOlsMenuItem) or (Sender = ReltimeOLSMenu2) then
        begin
          ProcessPack.AddProcessType(ppRelTimeLS);
          TimeTreeWizard.UsrOperation := dtdoRelTimeLS;
        end
        else if (Sender = RtdtLsMenuItem) or (Sender = RtdtOLSMenu2) then
        begin
          ProcessPack.AddProcessType(ppRtdtLs);
          TimeTreeWizard.UsrOperation := dtdoRtdtLS;
        end
        else
          raise Exception.Create('Invalid caller for Reltime OLS action');
        TimeTreeWizard.ProcessPack := ProcessPack;
        ProcessPack := nil;
        if HasSequenceData then
        begin
          TimeTreeWizard.InitAnalysisInfo;
          TimeTreeWizardForm.UpdateView(ttpsLoadTree);
        end;
        if IsGlenDevMode then
          TimeTreeWizardForm.Show
        else
        begin
          {$IFDEF WINDOWS}
          TimeTreeWizardForm.ShowModal;
          {$ELSE}
          TimeTreeWizardForm.Show;
          {$ENDIF}
        end;
        TimeTreeWizardForm := nil;
        {$IFDEF DEBUG}
        if IsGlenDevMode then
        begin
          TimeTreeWizard.LoadSeqData;
          TimeTreeWizard.LoadTree;
          //TimeTreeWizard.GetCalibrations;
        end;
        {$ENDIF}
      end;
    Except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when setting up the Reltime analysis: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.ComputeTimetreeMLActionExecute(Sender: TObject);
var
  ProcessPack : TProcessPack;
begin
  if IsPrototyper then
  begin
    LikelihoodRelTimeExecute(Sender); { the old style, using the analysis preferences dlg}
    Exit;
  end;
  if MultithreadedCalcIsRunning then // prevent the user from running multiple multhreaded analyses concurrently or in conjunction with other ML analyses
  begin
    MessageDlg('A multhithreaded Maxmimum Likelihood analysis is currently running in MEGA.'+ LineEnding+'In order to avoid thread collisions, only one multithreaded calculation may be run at a time.To run '+#13+#10+'multiple ML analyses simultaneously, run each analysis using a single thread.', mtInformation, [mbOK], 0);
    Exit;
  end;

  if not AskUserToKeepDataActive then
    Exit;

  try
    try
      if Assigned(TimeTreeWizard) then
        FreeAndNil(TimeTreeWizard);
      TimeTreeWizard := TTimeTreeWizard.Create;
      if Assigned(TimeTreeWizardForm) then
        FreeAndNil(TimeTreeWizardForm);
      TimeTreeWizardForm := TTimeTreeWizardForm.Create(Self);
      TimeTreeWizardForm.Wizard := TimeTreeWizard;
      TimeTreeWizard.WizardFormUpdate := TimeTreeWizardForm.UpdateView;
      AddWindowToTray(TimeTreeWizardForm);
      ProcessPack := TProcessPack.Create();
      if (Sender = ReltimeMlMenuItem) or (Sender = ReltimeMLMenu2) then
      begin
        ProcessPack.AddProcessType(ppRelTimeML);
        TimeTreeWizard.UsrOperation := dtdoRelTimeML;
      end
      else if (Sender = RtdtMLMenuItem) or (Sender = RtdtMlMenu2) then
      begin
        ProcessPack.AddProcessType(ppRtdtMl);
        TimeTreeWizard.UsrOperation := dtdoRtdtML;
      end
      else
        raise Exception.Create('invalid caller for Reltime ML action');
      TimeTreeWizard.ProcessPack := ProcessPack;
      ProcessPack := nil;
      if HasSequenceData then
      begin
        TimeTreeWizard.InitAnalysisInfo;
        TimeTreeWizardForm.UpdateView(ttpsLoadTree);
      end;
      if IsGlenDevMode then
        TimeTreeWizardForm.Show
      else
      begin
        {$IFDEF WINDOWS}
        TimeTreeWizardForm.ShowModal;
        {$ELSE}
        TimeTreeWizardForm.Show;
        {$ENDIF}
      end;
      TimeTreeWizardForm := nil; { freed automatically when setting CloseAction := caFree in OnClose}
      {$IFDEF DEBUG}
      if IsGlenDevMode then
      begin
        TimeTreeWizard.LoadSeqData;
        TimeTreeWizard.LoadTree;
        //TimeTreeWizard.GetCalibrations;
      end;
      {$ENDIF}
    Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.ConcatenateFilesActionExecute(Sender: TObject);
var
  selDirDlg: TSelectDirectoryDialog = nil;
  Response: Integer;
begin
  if HasActiveData then
  begin
    Response := MessageDlg('Close Active Data?', 'The active data file must be closed first. Continue?', mtConfirmation, mbYesNo, 0);
    if Response <> mrYes then
      Exit;
    CloseDataActionExecute(Sender);
  end;

  try
    try
      selDirDlg := TSelectDirectoryDialog.Create(Self);
      selDirDlg.InitialDir := GetCurrentDirUTF8;
      SelDirDlg.Title := 'Select a Directory Containing Files To Concatenate';
      if selDirDlg.Execute then
      begin
        FRuntimeProgress := TRuntimeProgress.Create(Self);
        LaunchConcatenateAlignmentsThread(selDirDlg.FileName, CheckCancel, FRuntimeProgress);
      end;
    except
      on E:EAbort do
        ShowMessage('Alignment concatenation cancelled');
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(selDirDlg) then
      selDirDlg.Free;
  end;
end;

procedure TMegaForm.ConvertToMegaFormatActionExecute(Sender: TObject);
begin
  try
    ConvertFileToMegaFormat;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when converting to the MEGA format: ' + E.Message);
  end;
end;

procedure TMegaForm.CorrTestBlensActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack = nil;
begin
  if IsPrototyper then
  begin
    try
      try
        ProcessPack := TProcessPack.Create();
        ProcessPack.AddProcessType(ppCorrTestBlens);
        ProcessPack.PerformAnalysis;
      Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
      end;
    finally
      if Assigned(ProcessPack) then
        ProcessPack.Free;
    end;
    Exit;
  end;

  try
    try
      if Assigned(TimeTreeWizard) then
        FreeAndNil(TimeTreeWizard);
      TimeTreeWizard := TTimeTreeWizard.Create;
      if Assigned(TimeTreeWizardForm) then
        FreeAndNil(TimeTreeWizardForm);
      TimeTreeWizardForm := TTimeTreeWizardForm.Create(Self);
      TimeTreeWizardForm.Wizard := TimeTreeWizard;
      TimeTreeWizard.WizardFormUpdate := TimeTreeWizardForm.UpdateView;
      AddWindowToTray(TimeTreeWizardForm, False);
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppCorrtestBlens);
      TimeTreeWizard.ProcessPack := ProcessPack;
      ProcessPack := nil;
      TimeTreeWizard.WizardMode := ttwmBlens;
      TimeTreeWizard.AnalysisType := ttaCorrtest;
      TimeTreeWizard.UsrOperation := dtdoCorrtestBLens;
      TimeTreeWizardForm.ShowModal;
      TimeTreeWizardForm := nil; { free automatically}
    Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.CorrTestMLActionExecute(Sender: TObject);
var
  ProcessPack : TProcessPack;
begin
  if IsPrototyper then
  begin
    try
      try
        ProcessPack := TProcessPack.Create();
        ProcessPack.AddProcessType(ppCorrTestML);
        ProcessPack.PerformAnalysis;
      Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
      end;
    finally
      if Assigned(ProcessPack) then
        ProcessPack.Free;
    end;
    Exit;
  end;

  if MultithreadedCalcIsRunning then // prevent the user from running multiple multhreaded analyses concurrently or in conjunction with other ML analyses
  begin
    MessageDlg('A multi-threaded calculation is running in MEGA but only one multi-threaded calculation may be run at a time', mtInformation, [mbOK], 0);
    Exit;
  end;

  if not AskUserToKeepDataActive then
    Exit;

  try
    try
      if Assigned(TimeTreeWizard) then
        FreeAndNil(TimeTreeWizard);
      TimeTreeWizard := TTimeTreeWizard.Create;
      if Assigned(TimeTreeWizardForm) then
        FreeAndNil(TimeTreeWizardForm);
      TimeTreeWizardForm := TTimeTreeWizardForm.Create(Self);
      TimeTreeWizardForm.Wizard := TimeTreeWizard;
      TimeTreeWizard.WizardFormUpdate := TimeTreeWizardForm.UpdateView;
      AddWindowToTray(TimeTreeWizardForm);
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppCorrTestML);
      TimeTreeWizard.ProcessPack := ProcessPack;
      TimeTreeWizard.UsrOperation := dtdoCorrTestML;
      TimeTreeWizard.AnalysisType := ttaCorrtest;
      if HasSequenceData then
      begin
        TimeTreeWizard.InitAnalysisInfo;
        TimeTreeWizardForm.UpdateView(ttpsLoadTree);
      end;
      TimeTreeWizardForm.ShowModal;
      TimeTreeWizardForm := nil;  { freed automatically}
    Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    ProcessPack.Free;
  end;
end;

procedure TMegaForm.CurExitBtnClick(Sender: TObject);
begin
  if IsPreviewingButton <> nil then
  begin
    if (IsPreviewingButton.Owner.ClassType = TV_SeqDataExplorer) or (IsPreviewingButton.Owner.ClassType = TV_DistDataExplorer) then
      CloseDataAction.Execute
    else
      TForm(IsPreviewingButton.Owner).Close;
  end;
end;

procedure TMegaForm.CurExitBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CurExitBtn.Left := 202;
  CurExitBtn.Top := 10;
end;

procedure TMegaForm.CurExitBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CurExitBtn.Left := 200;
  CurExitBtn.Top := 8;
end;

procedure TMegaForm.DataExitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TMegaForm.DataMenuImgMouseEnter(Sender :TObject);
begin
  if not IsPrototyper then
    Hover72x90.GetBitmap(MEN_IMG_DATA, DataMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.DataMenuImgMouseLeave(Sender :TObject);
begin
  if IsPrototyper then
    Gray72x90.GetBitmap(MEN_IMG_DATA, DataMenuImg.Picture.Bitmap)
  else
    Default72x90.GetBitmap(MEN_IMG_DATA, DataMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.DatamonkeyBtnMouseEnter(Sender: TObject);
begin
   ThirdPartyIcons.GetBitmap(1, DatamonkeyBtn.Picture.Bitmap);
end;

procedure TMegaForm.DatamonkeyBtnMouseLeave(Sender: TObject);
begin
   ThirdPartyIcons.GetBitmap(0, DatamonkeyBtn.Picture.Bitmap);
end;

procedure TMegaForm.DataOpenAFileActionExecute(Sender: TObject);
var
  AFileName, AFileExt: String;
  MyOpenDlg: TOpenDialog;
begin
  try
    MyOpenDlg := TOpenDialog.Create(Self);
    MyOpenDlg.Title       := 'Open a File';
    MyOpenDlg.Filter := 'All Files|*.*|MEGA Files|*.meg';
    MyOpenDlg.Options     := [ofShowHelp,ofPathMustExist,ofFileMustExist];
    MyOpenDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(MyOpenDlg.InitialDir);
    if not MyOpenDlg.Execute then
      Exit;
    AFileName := MyOpenDlg.FileName;
    ADataOpenDlg.Filename := AFileName; { TODO -oglen -cbugs : this is quick bug fix, it would be better to just use ADataOpenDlg here }
    AFileExt := lowercase(ExtractFileExt(AFileName));
    OpenFile(AFileName, AFileExt, OOther);
  Except
    on E: Exception do
      ShowMessage('On no! An error occurred when opening a file: ' + E.Message);
  end;
end;

procedure TMegaForm.DataPrinterSetupActionExecute(Sender: TObject);
begin
  try
    APrinterSetupDlg.Execute
  except
    on E: Exception do
     ShowMessage('Oh no! An error occurred when setting up a printer: ' + E.Message);
  end;
end;

procedure TMegaForm.DataSelectGeneticCodeActionExecute(Sender: TObject);
var
  MyDlg: TSelectGeneticCodeDlg;
  ACodeTableName: String;
begin
  if not HasActiveData then
    Exit;
  try
    try
      MyDlg := nil;
      MyDlg := TSelectGeneticCodeDlg.Create(Self);
      if CodeTableName <> EmptyStr then
        ACodeTableName :=  CodeTableName
      else
        ACodeTableName := 'Standard';
      MyDlg.CodeTableName := ACodeTableName;
      if MyDlg.ShowModal <> mrOK then
        MyDlg.CodeTableName := ACodeTableName;

      // this is important as the user may have changed the codetable itself
      CodeTableName := MyDlg.CodeTableName;
      CodeTable     := MyDlg.CodeTable;
      if V_SeqDataExplorer <> nil then
      begin
        D_InputSeqData.CodeName  := MyDlg.CodeTableName;
        D_InputSeqData.CodeTable := MyDlg.CodeTable;
        VS_SeqDataExplorer.UpdateCodeTable;
      end;
    Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    MyDlg.Free;
  end;
end;

procedure TMegaForm.DataToolMenuItemClick(Sender: TObject);
begin
  DataToolMenuItem.Checked := not DataToolMenuItem.Checked;
  DataMenuImg.Visible := DataToolMenuItem.Checked;
  AlignTopToolbarButtons;
end;

procedure TMegaForm.DeveloperActionExecute(Sender: TObject);
{$IFDEF DEBUG}
var
  processTypes: TStringList = nil;
  wf: TWorkflowForm = nil;
{$ENDIF}
begin
end;

procedure TMegaForm.DiagnoseMenuImgMouseEnter(Sender :TObject);
begin
  Hover78x90.GetBitmap(MEN_IMG_DIAGNOSE, DiagnoseMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.DiagnoseMenuImgMouseLeave(Sender :TObject);
begin
  Default78x90.GetBitmap(MEN_IMG_DIAGNOSE, DiagnoseMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.DiagnoseToolMenuItemClick(Sender: TObject);
begin
  DiagnoseToolMenuItem.Checked := not DiagnoseToolMenuItem.Checked;
  DiagnoseMenuImg.Visible := DiagnoseToolMenuItem.Checked;
  AlignTopToolbarButtons;
end;

procedure TMegaForm.DisplayHelpContentsActionExecute(Sender: TObject);
var
  indexFile: String;
begin
  try
    indexFile := GetPrivateFile(mfWebHelpIndex, False);
    if FileExists(indexFile) then
      GetHelpBrowser(indexFile)
    else
      ShowMessage('Oh no! The help files are missing on your system. To view the MEGA help, please go to http://www.megasoftware.net');
  except
    on E:Exception do
      ShowMessage('An error occurred when opening the help files: ' + E.Message + '. Please go to http://www.megasoftware.net to view the help documentation');
  end;
end;

procedure TMegaForm.DistanceMenuImgMouseEnter(Sender :TObject);
begin
  Hover72x90.GetBitmap(MEN_IMG_DISTANCE, DistanceMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.DistanceMenuImgMouseLeave(Sender :TObject);
begin
  Default72x90.GetBitmap(MEN_IMG_DISTANCE, DistanceMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.DistanceToolMenuItemClick(Sender: TObject);
begin
  DistanceToolMenuItem.Checked := not DistanceToolMenuItem.Checked;
  DistanceMenuImg.Visible := DistanceToolMenuItem.Checked;
  AlignTopToolbarButtons;
end;

procedure TMegaForm.DiversityMenuImgMouseEnter(Sender :TObject);
begin
  Hover72x90.GetBitmap(MEN_IMG_DIVERSITY, DiversityMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.DiversityMenuImgMouseLeave(Sender :TObject);
begin
  Default72x90.GetBitmap(MEN_IMG_DIVERSITY, DiversityMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.DiversityToolMenuItemClick(Sender: TObject);
begin
  DiversityToolMenuItem.Checked := not DiversityToolMenuItem.Checked;
  DiversityMenuImg.Visible := DiversityToolMenuItem.Checked;
  AlignTopToolbarButtons;
end;

procedure TMegaForm.EvolutionaryProbsActionExecute(Sender: TObject);
var
  ProcessPack : TProcessPack = nil;
begin
  if IsPrototyper then
  begin
    try
      try
        ProcessPack := TProcessPack.Create();
        ProcessPack.AddProcessType(ppAncSeq);
        ProcessPack.AddProcessType(ppML);
        ProcessPack.AddProcessType(ppEpML);
        ProcessPack.PerformAnalysis;
      Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
      end;
    finally
      if Assigned(ProcessPack) then
        ProcessPack.Free;
    end;
    Exit;
  end;

  if MultithreadedCalcIsRunning then // prevent the user from running multiple multhreaded analyses concurrently or in conjunction with other ML analyses
  begin
    MessageDlg('A multi-threaded calculation is running in MEGA but only one multi-threaded calculation may be run at a time', mtInformation, [mbOK], 0);
    Exit;
  end;
  if not AskUserToActivateDataFile(EvolutionaryProbsActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
     ShowMessage('The EP calculation uses Maximum Likelihood analysis which requires sequence data.');
     Exit;
  end;

  try
    try
      if Assigned(TimeTreeWizard) then
        FreeAndNil(TimeTreeWizard);
      TimeTreeWizard := TTimeTreeWizard.Create;
      if Assigned(TimeTreeWizardForm) then
        FreeAndNil(TimeTreeWizardForm);
      TimeTreeWizardForm := TTimeTreeWizardForm.Create(Self);
      TimeTreeWizardForm.Wizard := TimeTreeWizard;
      TimeTreeWizard.WizardFormUpdate := TimeTreeWizardForm.UpdateView;
      AddWindowToTray(TimeTreeWizardForm);
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppEpML);
      TimeTreeWizard.ProcessPack := ProcessPack;
      ProcessPack := nil;
      TimeTreeWizard.UsrOperation := dtdoEpML;
      TimeTreeWizard.AnalysisType := ttaEP;
      if HasSequenceData then
      begin
        TimeTreeWizard.InitAnalysisInfo;
        TimeTreeWizardForm.UpdateView(ttpsLoadTree);
      end;
      TimeTreeWizardForm.ShowModal;
      TimeTreeWizardForm := nil;  { freed automatically}
    Except
    on E: Exception do
      ShowMessage('Oh no! EP calculation failed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.ExampleFilesActionExecute(Sender: TObject);
var
  AFileName: String;
  AFileExt: String;
  ExampleFiles: TExampleFiles = nil;
begin
  if IsPrototyper then
  begin
    ShowMessage('Cannot open example files in PROTOTYPE mode. Click the ANALYZE button to unlock this feature');
    Exit;
  end;

  try
    try
      ExampleFiles := TExampleFiles.Create;
      if not ExampleFiles.CheckExampleFiles then
        ShowMessage('Some or all example files may be missing. Please make sure that you have permission to copy files to the Documents directory');

      ADataOpenDlg.Title       := 'Open a File';
      ADataOpenDlg.Filter := 'All Files|*.*|MEGA Files|*.meg';
      ADataOpenDlg.InitialDir  := ExampleFiles.ExampleFilesFolderName;
      ADataOpenDlg.Options     := [ofShowHelp,ofPathMustExist,ofFileMustExist];
      if DirectoryExists(ADataOpenDlg.InitialDir) then
      begin
        if not ADataOpenDlg.Execute then
          Exit;
        AFileName := ADataOpenDlg.FileName;
        AFileExt := lowercase(ExtractFileExt(AFileName));
        OpenFile(AFileName, AFileExt, OOther);
      end
      else
        ShowMessage('Oh no! The example files directory (' + ADataOpenDlg.InitialDir + ') does not exist. Please ensure that you have permission to write to this directory');
    except
      on E:Exception do
        ShowMessage('On no! An error has occurred: ' + E.Message)
    end;
  finally
    if Assigned(ExampleFiles) then
      ExampleFiles.Free;
  end;
end;

procedure TMegaForm.ExamplesBtnClick(Sender: TObject);
var
  AFileName: String;
  AFileExt: String;
  ExampleFiles: TExampleFiles;
begin
  ExampleFiles := nil;
  try
    try
      ExampleFiles := TExampleFiles.Create;
      if not ExampleFiles.CheckExampleFiles then
        ShowMessage('Some or all example files may be missing. Please make sure that you have permission to copy files to the Documents directory');

      ADataOpenDlg.Title       := 'Open a File';
      ADataOpenDlg.Filter := 'All Files|*.*|MEGA Files|*.meg';
      ADataOpenDlg.InitialDir  := ExampleFiles.ExampleFilesFolderName;
      ADataOpenDlg.Options     := [ofShowHelp,ofPathMustExist,ofFileMustExist];
      if DirectoryExists(ADataOpenDlg.InitialDir) then
      begin
        if not ADataOpenDlg.Execute then
          Exit;
        AFileName := ADataOpenDlg.FileName;
        AFileExt := lowercase(ExtractFileExt(AFileName));
        OpenFile(AFileName, AFileExt, OOther);
      end
      else
        ShowMessage('Oh no! The example files directory (' + ADataOpenDlg.InitialDir + ') does not exist. Please ensure that you have permission to write to this directory');
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(ExampleFiles) then
      ExampleFiles.Free;
  end;
end;

procedure TMegaForm.ExamplesBtnMouseEnter(Sender: TObject);
begin
  if not IsPrototyper then
    BtRed.GetBitmap(BT_IMG_EXAMPLES, ExamplesBtn.Picture.Bitmap);
end;

procedure TMegaForm.ExamplesBtnMouseLeave(Sender: TObject);
begin
  if IsPrototyper then
    BtGray.GetBitmap(BT_IMG_EXAMPLES, ExamplesBtn.Picture.Bitmap)
  else
    BtBlue.GetBitmap(BT_IMG_EXAMPLES, ExamplesBtn.Picture.Bitmap);
end;

procedure TMegaForm.FeedbackLinkClick(Sender: TObject);
begin
  BugReportBtnClick(Sender);
end;

procedure TMegaForm.FeedbackLinkMouseEnter(Sender: TObject);
begin
  FeedbackLink.Cursor := crHandPoint;
  FeedbackLink.Color := clLtGray;
  FeedbackLink.Font.Color := clBlue;
end;

procedure TMegaForm.FeedbackLinkMouseLeave(Sender: TObject);
begin
  FeedbackLink.Cursor := crDefault;
  FeedbackLink.Color := clNone;
  FeedbackLink.Font.Color := clNavy;
end;

procedure TMegaForm.FirstTimeUserActionExecute(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HC_First_Time_User));
end;

procedure TMegaForm.FirstTimeUserBtnClick(Sender: TObject);
var
  BtnPos: TPoint;
begin
  BtnPos.X := HelpBtn.Left + 4;
  BtnPos.Y := BottomToolbar.Top + TopToolbar.Height - FMainMenuHeight*HelpDocsPopupMenu.Items.Count + FMainMenuHeight;
  BtnPos := ClientToScreen(BtnPos);
  HelpDocsPopupMenu.Popup(BtnPos.X, BtnPos.Y);
end;

procedure TMegaForm.FormChangeBounds(Sender: TObject);
begin
  {$IFDEF PROTOTYPER_MODE}
  if Assigned(FPopupNotifier) and FPopupNotifier.Visible then
    FPopupNotifier.Hide;
  {$ENDIF}
end;

procedure TMegaForm.FormDestroy(Sender: TObject);
var
  i: Integer;
  {$IFNDEF DEBUG}
  logFile: ustring;
{$ENDIF}
begin
  FIsClosing := True;
  {$IFNDEF DEBUG}
  if Assigned(FUniqueInstance) then
    FUniqueInstance.Free;
  {$ENDIF}
  if Assigned(MenuRenderer) then
    MenuRenderer.Free;
  if Assigned(FPopupMenus) then
    FPopupMenus.Free;
  if Assigned(FAppLinkOptionsManager) then
    FAppLinkOptionsManager.Free;
  if Assigned(FRecentlyUsedFiles) then
    FRecentlyUsedFiles.Free;
  if Assigned(FJobQueueList) then
    FJobQueueList.Free;
  if Assigned(SLACWindow) then
    SLACWindow.Free;
  if Assigned(FMenuBitmap) then
    FMenuBitmap.Free;
  if Assigned(FCheckmark) then
    FCheckmark.Free;
  {$IFNDEF DEBUG}
  logFile := CefLogFile;
  try
    if FileExists(logFile) then
      DeleteFile(logFile);
  except

  end;
  {$ENDIF}
end;

procedure TMegaForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  aFile: String;
  aExt: String;
begin
  try
    if Length(Filenames) > 0 then
    begin
      aFile := FileNames[0];
      if FileExists(aFile) then
      begin
        aExt := lowercase(ExtractFileExt(aFile));
        if Pos(aExt, FastaExts) > 0 then
        begin
          if QuestionDlg('How would you like to open this fasta file?', 'Analyze or Align File?', mtInformation, [21, 'Align', 22, 'Analyze'], 0) = 21 then
            ShowAlignmentFromFile(aFile)
          else
          begin
            PromptCloseCurrentlyActive;
            if HasActiveData then
            begin
              // They might have clicked NO, in which case we just don't want to procede.
              Exit;
            end;
            DoOpenDataFile(aFile);
          end;
        end
        else
          OpenFile(aFile, aExt, OOther);
      end;
    end;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  Screen.Cursor := crDefault;
end;

procedure TMegaForm.FormShow(Sender: TObject);
begin
  if not FCommandLineParamsProcessed then
    ProcessCommandLineParams;
  ImageForm.UpdateImgList(Self);
end;

procedure TMegaForm.HelpBtnMouseEnter(Sender: TObject);
begin
  BtRed.GetBitmap(BT_IMG_HELP, HelpBtn.Picture.Bitmap);
end;

procedure TMegaForm.HelpBtnMouseLeave(Sender: TObject);
begin
  BtBlue.GetBitmap(BT_IMG_HELP, HelpBtn.Picture.Bitmap);
end;

procedure TMegaForm.DatamonkeyBtnClick(Sender: TObject);
var
  MenuPos: TPoint;
begin
  MenuPos.X := MegaForm.Width - TopToolbar.Canvas.TextWidth('Codon by Codon Selection (SLAC)') - 20;
  MenuPos.Y := TopToolbar.Height + FMainMenuHeight*DataMonkeyPopup.Items.Count + TimeTreeBtn.Height + 10;
  MenuPos := ClientToScreen(MenuPos);
  DatamonkeyPopup.Popup(MenuPos.X, MenuPos.Y);
end;

procedure TMegaForm.IQTREEModelSelActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack = nil;
begin
end;

procedure TMegaForm.MEGAXPubActionExecute(Sender: TObject);
var
  aBrowser: TChromiumBrowser;
begin
  aBrowser := CreateNewChromiumBrowserWindow(bmDefaultBrowser, Self, False);
  aBrowser.Chromium.Load(MEGA_CITATIONS_URL);
  aBrowser.Show;
end;

procedure TMegaForm.MEGAXPubStoryActionExecute(Sender: TObject);
var
  aBrowser: TChromiumBrowser;
begin
  aBrowser := CreateNewChromiumBrowserWindow(bmDefaultBrowser, Self, False);
  aBrowser.Chromium.Load(MEGA_RESOURCES_URL);
  aBrowser.Show;
end;

procedure TMegaForm.NodeTimeActionExecute(Sender: TObject);
var
  NodeTimeWindow: TChromiumBrowser;
  url: String;
begin
  url := ('http://www.timetree.org/search/goto_pairwise');
  NodeTimeWindow := CreateNewChromiumBrowserWindow(bmDefaultBrowser, Self, False);
  NodeTimeWindow.Chromium.Load(url);
  NodeTimeWindow.Show;
end;

procedure TMegaForm.ThirdPartyToolsActionExecute(Sender: TObject);
begin
end;

procedure TMegaForm.TimelineActionExecute(Sender: TObject);
var
  TimelineWindow: TChromiumBrowser;
  url: String;
begin
  url := ('http://www.timetree.org/search/goto_timeline');
  TimelineWindow := CreateNewChromiumBrowserWindow(bmDefaultBrowser, Self, False);
  TimelineWindow.Chromium.Load(url);
  TimelineWindow.Show;
end;

procedure TMegaForm.TimetreeActionExecute(Sender: TObject);
var
  TimetreeWindow: TChromiumBrowser;
  url: String;
begin
  url := ('http://www.timetree.org/search/goto_timetree');
  TimetreeWindow := CreateNewChromiumBrowserWindow(bmDefaultBrowser, Self, False);
  TimetreeWindow.RunInDefaultWebBrowserMode;
  TimetreeWindow.Chromium.Load(url);
  TimetreeWindow.Show;
end;

procedure TMegaForm.TimetreeBtnMouseLeave(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  ThirdPartyIcons.GetBitmap(2, TimeTreeBtn.Picture.Bitmap);
end;

procedure TMegaForm.TimeTreeDBBtnClick(Sender: TObject);
  var
  MenuPos: TPoint;
begin
  MenuPos.X := PreferencesBtn.Left+20;
  MenuPos.Y := TopToolbar.Height + FMainMenuHeight + DatamonkeyBtn.Height-20;
  MenuPos := ClientToScreen(MenuPos);
  TimeTreeDBPopup.Popup(MenuPos.X, MenuPos.Y);
end;

procedure TMegaForm.MenuItem91Click(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to PrefMenu.Items.Count-1 do
    PrefMenu.Items.Items[i].Enabled:=True;
end;

procedure TMegaForm.ProtoHelpImageClick(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HC_Analyze_and_Prototyper_Modes));
end;

procedure TMegaForm.ProtoHelpImageMouseEnter(Sender: TObject);
begin
  NewIcons.GetBitmap(68, ProtoHelpImage.Picture.Bitmap);
end;

procedure TMegaForm.ProtoHelpImageMouseLeave(Sender: TObject);
begin
  HotIcons.GetBitmap(68, ProtoHelpImage.Picture.Bitmap);
end;

procedure TMegaForm.LikelihoodRelTimeExecute(Sender: TObject);
var
  ProcessPack : TProcessPack = nil;
begin
  if not HasSequenceData then
  begin
     ShowMessage('Maximum Likelihood Analysis requires sequence data.');
     Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      if (Sender = ReltimeMlMenuItem) or (Sender = ReltimeMLMenu2) then
        ProcessPack.AddProcessType(ppRelTimeML)
      else if (Sender = RtdtMLMenuItem) or (Sender = RtdtMlMenu2) then
        ProcessPack.AddProcessType(ppRtdtMl)
      else
        raise Exception.Create('invalid caller for Reltime ML action');
      ProcessPack.PerformAnalysis;
    Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.DiagnoseMutationActionExecute(Sender: TObject);
begin
  MdWelcomeForm := TMdWelcomeForm.Create(Self);
  MdWelcomeForm.ShowModal;
end;

procedure TMegaForm.CoefficientOfDifferentiationActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(CoefficientOfDifferentiationActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
    ShowMessage('Diversity estimation requires sequence data.');
    Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppDistEst);
      ProcessPack.AddProcessType(ppPorportionInterDiversity);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.DisparityIndexActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(DisparityIndexActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
    ShowMessage('Disparity index calculations requires nucleotide sequence data.');
    Exit;
  end;
  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppDistTestPatternHomo);
      //ProcessPack.CommonName := TAction(Sender).Caption;
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.BLASTActionExecute(Sender: TObject);
var
  aBrowser: TChromiumBrowser;
begin
  try
     aBrowser := CreateNewChromiumBrowserWindow(bmBrowser, Self);
     aBrowser.BLAST('', true);
   Except
     on E: Exception do
       ShowMessage('Oh no! An error occurred when displaying the web browser: ' + E.Message);
   end;
end;

procedure TMegaForm.BugReportBtnClick(Sender: TObject);
var
  //OS: String;
  //Version: String;
  URL: String;

begin
  try
   // Version := VER_MEGA_MAJOR_CHAR + VER_MEGA_MINOR_STR;
   //{$IFDEF MSWINDOWS}
   // OS := 'Windows';
   //{$ELSE}
   //  {$IFDEF DARWIN}
   //  OS := 'Mac';
   //  Version := 'MEGA_' + Version + '_Mac';
   //  {$ELSE}
   //  OS := 'Linux';
   //  Version := 'MEGA ' + Version + ' Linux';
   //  {$ENDIF}
   //{$ENDIF}

    URL := WEBSITE_URL + '/bugs';
    OpenUrl(URL);
  Except
    on E: Exception do
      ShowMessage('Oh no! An error occurred so that the requested operation could not be completed');
  end;
end;

procedure TMegaForm.BugReportBtnMouseEnter(Sender: TObject);
begin
  BtRed.GetBitmap(BT_IMG_BUG_REPORT, BugReportBtn.Picture.Bitmap);
end;

procedure TMegaForm.BugReportBtnMouseLeave(Sender: TObject);
begin
  BtBlue.GetBitmap(BT_IMG_BUG_REPORT, BugReportBtn.Picture.Bitmap);
end;

procedure TMegaForm.AminoAcidCompActionExecute(Sender: TObject);
var
  ExportAs: TExportType;
  SaveLocation : String;
begin
  if (Sender <> V_SeqDataExplorer) and (not AskUserToActivateDataFile(AminoAcidCompActionExecute)) then
    Exit;

  if D_InputSeqData = nil then  // refactored for clarification to the user.
  begin
    ShowMessage('You need to use sequence data for Amino Acid Composition.');
    Exit;
  end
  else if not (D_InputSeqData.IsAmino or (VS_SeqDataExplorer.isTranslated and HasCodonData)) then
  begin
    if HasCodonData then
      ShowMessage('You need to translate your sequences first. '+
                  'Do this by opening the data explorer and clicking the translate button.')
    else
      ShowMessage('You need amino acid or TRANSLATED coding DNA sequences to compute amino acid composition.');
    exit;
  end;
  SaveLocation := DataFileNameOnly + '_amino-acid-composition';
  if Assigned(V_SeqDataExplorer) then
    ExportAs := V_SeqDataExplorer.SelectedStatsExportType
  else
    ExportAs := EXexcelXMLDisp;
  ExportAs := PromptUserWriteOutput(SaveLocation, VS_SeqDataExplorer.CurAttrDisp = megNone, ExportAs); // hides sites panel if needed
  if ExportAs = EXnone then
    exit;
  try
    if Assigned(V_SeqDataExplorer) then
      V_SeqDataExplorer.SelectedStatsExportType := ExportAs;
    VS_SeqdataExplorer.StatUseOnlyMarkedSitesItem := WriteOutputDlg.IsUseHighlightedSites;
    VS_SeqdataExplorer.StatUseOnlyUnMarkedSitesItem := WriteOutputDlg.IsUseUnHighlightedSites;
    VS_SeqdataExplorer.StatAllSelSitesItem := WriteOutputDlg.IsUseAllSites;
    LaunchAminoAcidCompositionStatsThread(SaveLocation, ExportAs);
  Except
    on E: Exception do
      ShowMessage('Unable to calculate Amino Acid Composition Error: ' + E.Message);
  end;
end;

procedure TMegaForm.AnalyzeBtnClick(Sender: TObject);
begin
  if IsPrototyper then
  begin
    if Assigned(FTimer) and FTimer.Enabled then
      FTimer.Enabled := False;
    ClearData;
    if Assigned(FPopupNotifier) and FPopupNotifier.Visible then
      FPopupNotifier.vNotifierForm.Hide;
  end;
  SetRunMode(mrmGui);
end;

procedure TMegaForm.AnalyzeBtnMouseEnter(Sender: TObject);
begin
  RunModeImages.GetBitmap(1, AnalyzeBtn.Picture.Bitmap);
end;

procedure TMegaForm.AnalyzeBtnMouseLeave(Sender: TObject);
begin
  if MegaRunMode = mrmGui then
    RunModeImages.GetBitmap(2, AnalyzeBtn.Picture.Bitmap)
  else
    RunModeImages.GetBitmap(0, AnalyzeBtn.Picture.Bitmap);
end;

procedure TMegaForm.AlignToolMenuItemClick(Sender: TObject);
begin
  AlignToolMenuItem.Checked := not AlignToolMenuItem.Checked;
  AlignMenuImg.Visible := AlignToolMenuItem.Checked;
  AlignTopToolbarButtons;
end;

procedure TMegaForm.AlignDnaMuscleActionExecute(Sender: TObject);
begin
  try
    if not Assigned(FAppLinkOptionsManager) then
      FAppLinkOptionsManager := TApplinkOptionsManager.Create;
    FAppLinkOptionsManager.GetMuscleMaoSettings(FDataType);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred and the requested operation could not be completed: ' + E.Message);
  end;
end;

procedure TMegaForm.AlignMenuImgMouseEnter(Sender :TObject);
begin
  Hover78x90.GetBitmap(MEN_IMG_ALIGN, AlignMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.AlignMenuImgMouseLeave(Sender :TObject);
begin
  Default78x90.GetBitmap(MEN_IMG_ALIGN, AlignMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.AlignCodonsClustalActionExecute(Sender: TObject);
begin
  try
    if not Assigned(FAppLinkOptionsManager) then
      FAppLinkOptionsManager := TApplinkOptionsManager.Create;
    FAppLinkOptionsManager.GetClustalMaoSettings(snCoding);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred and the requested operation could not be completed: ' + E.Message);
  end;
end;

procedure TMegaForm.AboutWindowActionExecute(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure TMegaForm.BeamActionExecute(Sender: TObject);
var
  processPack: TProcessPack = nil;
begin
end;


procedure TMegaForm.SiteCoverageActionExecute(Sender: TObject);
var
  processPack: TProcessPack = nil;
begin
  if not AskUserToActivateDataFile(SiteCoverageActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
     ShowMessage('Computing site coverage analysis requires sequence data.');
     Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create;
      ProcessPack.AddProcessType(ppSiteCoverage);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('MEGA application error: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.EstimateMutationalSignaturesActionExecute(Sender: TObject);
begin
  ShowMessage('not implemented');
end;

procedure TMegaForm.EstimateTumorDiversityActionExecute(Sender: TObject);
begin
  ShowMessage('not implemented');
end;

procedure TMegaForm.InferClonePhylogenyActionExecute(Sender: TObject);
begin
  ShowMessage('not implemented');
end;

procedure TMegaForm.InferTumorPhylogenyActionExecute(Sender: TObject);
begin
  ShowMessage('not implemented');
end;

procedure TMegaForm.IQTreeBtnClick(Sender: TObject);
var
  MenuPos: TPoint;
begin
end;

procedure TMegaForm.IQTreeBtnMouseEnter(Sender: TObject);
begin
end;

procedure TMegaForm.IQTREETreeInfActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
end;

procedure TMegaForm.LoadBulkSequenceDataActionExecute(Sender: TObject);
begin
  ShowMessage('not implemented');
end;

procedure TMegaForm.LoadSingleCellSequenceActionExecute(Sender: TObject);
begin
  ShowMessage('not implemented');
end;

procedure TMegaForm.MegaActionListExecute(AAction: TBasicAction;
  var Handled: Boolean);
begin
  if IsDeveloper then
    CurrentActionCaption := TCustomAction(AAction).Caption;
  Handled := False;
end;

procedure TMegaForm.PublicationsLinkClick(Sender: TObject);
var
  MenuPos: TPoint;
begin
  MenuPos.X := PublicationsLink.Left;
  MenuPos.Y := MegaForm.Height - HelpBtn.Height - FMainMenuHeight*PublicationsPopupMenu.Items.Count - 10;
  MenuPos := ClientToScreen(MenuPos);
  PublicationsPopupMenu.Popup(MenuPos.X, MenuPos.Y);
end;

procedure TMegaForm.PublicationsLinkMouseEnter(Sender: TObject);
begin
  PublicationsLink.Font.Color := $9c9d1f;
end;

procedure TMegaForm.PublicationsLinkMouseLeave(Sender: TObject);
begin
  PublicationsLink.Font.Color := $717171;
end;

procedure TMegaForm.ReconstructMigrationsActionExecute(Sender: TObject);
begin
  ShowMessage('not implemented');
end;

procedure TMegaForm.TimeTreeBtnClick(Sender: TObject);
var
  MenuPos: TPoint;
begin
  MenuPos.X := MegaForm.Width - TopToolbar.Canvas.TextWidth('Time Tree (Pair)') - 10;
  MenuPos.Y := TopToolbar.Height + FMainMenuHeight + 70;
  MenuPos := ClientToScreen(MenuPos);
  TimeTreeDBPopup.Popup(MenuPos.X, MenuPos.Y);
end;

procedure TMegaForm.TimeTreeBtnMouseEnter(Sender: TObject);
begin
  ThirdPartyIcons.GetBitmap(3, TimeTreeBtn.Picture.Bitmap);
end;

procedure TMegaForm.IQTreeBtnMouseLeave(Sender: TObject);
begin
end;

procedure TMegaForm.AlignCodonsMuscleActionExecute(Sender: TObject);
begin
  try
    if not Assigned(FAppLinkOptionsManager) then
      FAppLinkOptionsManager := TApplinkOptionsManager.Create;
    FAppLinkOptionsManager.GetMuscleMaoSettings(snCoding);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred and the requested operation could not be completed: ' + E.Message);
  end;
end;

procedure TMegaForm.AlignDnaClustalActionExecute(Sender: TObject);
begin
  try
    if not Assigned(FAppLinkOptionsManager) then
      FAppLinkOptionsManager := TApplinkOptionsManager.Create;
    FAppLinkOptionsManager.GetClustalMaoSettings(FDataType);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred and the requested operation could not be completed: ' + E.Message);
  end;
end;

procedure TMegaForm.AlignProteinClustalActionExecute(Sender: TObject);
begin
  try
    if not Assigned(FAppLinkOptionsManager) then
      FAppLinkOptionsManager := TApplinkOptionsManager.Create;
    FAppLinkOptionsManager.GetClustalMaoSettings(FDataType);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred and the requested operation could not be completed: ' + E.Message);
  end;
end;

procedure TMegaForm.AlignProteinMuscleActionExecute(Sender: TObject);
begin
  try
    if not Assigned(FAppLinkOptionsManager) then
      FAppLinkOptionsManager := TApplinkOptionsManager.Create;
    FAppLinkOptionsManager.GetMuscleMaoSettings(FDataType);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred and the requested operation could not be completed: ' + E.Message);
  end;
end;

procedure TMegaForm.AnalyzeTreeLSActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;

  if not AskUserToActivateDataFile(AnalyzeTreeLSActionExecute) then
    Exit;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppAnalyzeUserTree);
      ProcessPack.AddProcessType(ppLeastSquares);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.AnalyzeTreeMLActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if MultithreadedCalcIsRunning then // prevent the user from running multiple multhreaded analyses concurrently or in conjunction with other ML analyses
  begin
    MessageDlg('A multi-threaded Maxmimum Likelihood analysis is already running in MEGA.'+ LineEnding +'In order to avoid thread collisions, only one multithreaded calculation may be run at a time.To run '+ LineEnding+'multiple ML analyses simultaneously, run each analysis using a single thread.', mtInformation, [mbOK], 0);
    Exit;
  end;
  if not AskUserToActivateDataFile(AnalyzeTreeMLActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
     ShowMessage('Maximum Likelihood tree construction requires sequence data.');
     Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppAnalyzeUserTree);
      ProcessPack.AddProcessType(ppML);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.AnalyzeTreeMPActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(AnalyzeTreeMPActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
     ShowMessage('Maximum Parsimony Analysis requires sequence data.');
     Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppAnalyzeUserTree);
      ProcessPack.AddProcessType(ppMP);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.AncestorsMenuImgMouseEnter(Sender :TObject);
begin
  Hover72x90.GetBitmap(MEN_IMG_ANCESTORS, AncestorsMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.AncestorsMenuImgMouseLeave(Sender :TObject);
begin
  Default72x90.GetBitmap(MEN_IMG_ANCESTORS, AncestorsMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.AncestorsToolMenuItemClick(Sender: TObject);
begin
  AncestorsToolMenuItem.Checked := not AncestorsToolMenuItem.Checked;
  AncestorsMenuImg.Visible := AncestorsToolMenuItem.Checked;
  AlignTopToolbarButtons;
end;

procedure TMegaForm.AncestralSeqsMLActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  {$IFNDEF MYPEG_ONLY}
  if not AskUserToActivateDataFile(AncestralSeqsMLActionExecute) then
    Exit;
  {$ENDIF}
  if not HasSequenceData then
  begin
     ShowMessage('Maximum Likelihood ancestral sequence inference requires sequence data.');
     Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppAncSeq);
      ProcessPack.AddProcessType(ppML);
      if Assigned(Sender) then
        ProcessPack.CommonName := TAction(Sender).Caption;
      if ProcessPack.CommonName = 'Infer Ancestral Seqs (ML)' then
      begin
        ProcessPack.AddProcessType(ppMyPegAncestInfer);
        ProcessPack.MyPegPeptideSite := MutationDetailViewForm.FocusedAASite;
      end;
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.AncestralSeqsMPActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  {$IFNDEF MYPEG_ONLY}
  if not AskUserToActivateDataFile(AncestralSeqsMPActionExecute) then
    Exit;
  {$ENDIF}
  if not HasSequenceData then
  begin
     ShowMessage('Maximum Parsimony Analysis requires sequence data.');
     Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppAncSeq);
      ProcessPack.AddProcessType(ppMP);
      if Assigned(Sender) then
        ProcessPack.CommonName := TAction(Sender).Caption;
      if (ProcessPack.CommonName = 'Infer Ancestral Seqs (MP)') then
      begin
        ProcessPack.AddProcessType(ppMyPegAncestInfer);
        ProcessPack.MyPegPeptideSite := MutationDetailViewForm.FocusedAASite;
      end;
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.BetweenMeanActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(BetweenMeanActionExecute) then
    Exit;
  if not(HasSequenceData OR HasDistanceData) then
  begin
    ShowMessage('Between Group average requires sequence or pairwise distance data.');
    Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppDistEst);
      ProcessPack.AddProcessType(ppBetweenGpAvg);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.EditSequencerActionExecute(Sender: TObject);
var
  TrEdt: TTraceEditForm;
begin
  try
    TrEdt := CreateNewTraceEditorWindow;
    if Assigned(TrEdt) then
      TrEdt.Show;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaForm.EditTreeActionExecute(Sender: TObject);
var
  TreeTopologyDlg: TTreeInputForm;
  ActiveNames: TStringList;
begin
  try
    ActiveNames := TStringList.Create;
    ConstructActiveNames(ActiveNames);
    TreeTopologyDlg := TTreeInputForm.Create(Self);
    TreeTopologyDlg.ShowAsTopologyEditor := True;
    TreeTopologyDlg.SetActiveDataNames(ActiveNames);
    TreeTopologyDlg.Show;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaForm.ExploreDataActionExecute(Sender: TObject);
begin
  if IsPrototyper then
    Exit;
  try
    If HasSequenceData then
    begin
      if V_SeqDataExplorer  <> nil then
        begin
          if V_SeqDataExplorer.WindowState = wsMinimized then
            V_SeqDataExplorer.WindowState := wsNormal
          else
            V_SeqDataExplorer.Show;
        end;
    end
    else if HasDistanceData then
    begin
      if V_DistDataExplorer <> nil then
      begin
        if V_DistDataExplorer.WindowState = wsMinimized then
          V_DistDataExplorer.WindowState := wsNormal
        else
          V_DistDataExplorer.Show;
      end;
    end;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when displaying the active data set: ' + E.Message);
  end;
end;

procedure TMegaForm.ExportDataActionExecute(Sender: TObject);
begin
  try
    if not HasActiveData then
    begin
      ShowMessage('Nothing to export, as no data sets are active.');
      Exit;
    end;

    if V_SeqDataExplorer <> nil then
    begin
      V_SeqDataExplorer.Show;
      V_SeqDataExplorer.ActionExportDataExecute(V_SeqDataExplorer);
    end
    else if V_DistDataExplorer <> nil then
    begin
      V_DistDataExplorer.Show;
      V_DistDataExplorer.FileSaveToFileItemClick(V_DistDataExplorer);
    end;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error occurred when exporting data: ' + E.Message);
  end;
end;

procedure TMegaForm.FindBestDNAActionExecute(Sender: TObject);
var
  ProcessPack : TProcessPack;
begin
  if not AskUserToActivateDataFile(FindBestDNAActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
     ShowMessage('Maximum Likelihood Analysis requires sequence data.');
     Exit;
  end;
  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppModelSelML);
      //ProcessPack.CommonName := TAction(Sender).Caption;
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error occurred and MEGA could not complete the analysis: ' + E.Message);
    end;
  finally
    ProcessPack.Free;
  end;
end;

procedure TMegaForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: integer;
  checked: Boolean = False;
  questionRes: String = '';
begin
  if IsPrototyper then
  begin
    if Assigned(FTimer) and FTimer.Enabled then
      FTimer.Enabled := False;
    try
      SaveMainMenuPreferencesOnDestroy;
    except
      on E:Exception do
        ShowMessage('Application Error when saving preferences: ' + E.Message);
    end;
    CanClose := True;
    Exit;
  end;

  // ask the user if they are sure they want to close MEGA.
  try
    if HasActiveData then
    begin
      if not TryToGetPreferencesMainMenu(UserPref_MegaMainDoNotWarnOnMEGACloseStr, checked, QuestionRes) then
        checked := false;
      if checked then // they want to use the same genetic code
      begin
        UpdatePreferencesMainMenu(UserPref_MegaMainDoNotWarnOnMEGACloseStr, checked, BoolToStr(True, True));
      end
      else
      begin
        {$IFDEF DEBUG}
        UpdatePreferencesMainMenu(UserPref_MegaMainDoNotWarnOnMEGACloseStr, checked, BoolToStr(True, True));
        {$ELSE}
        if MessageDlgCheck(Self, 'Are you sure you want to close MEGA?                ',
                             UserPref_DoNotWarnOnMEGACloseStr, UserPref_DoNotWarnOnMEGACloseStr, checked, mtConfirmation, [mbYES, mbNO], 0) = mrYes then
        begin
          UpdatePreferencesMainMenu(UserPref_MegaMainDoNotWarnOnMEGACloseStr, checked, BoolToStr(True, True));
        end
        else
        begin
          UpdatePreferencesMainMenu(UserPref_MegaMainDoNotWarnOnMEGACloseStr, checked, BoolToStr(False, True));
          CanClose := False;
          Exit;
        end;
        {$ENDIF}
      end;
    end;
  Except on E: Exception do
    // We're closing the form, if anything happens ignore it.
  end;

  // close all the edit windows that are open
  try
    CanClose  := True;
    for i := ComponentCount-1 downto 0 do
       if Components[i] is TEditorForm then
       begin
         TEditorForm(Components[i]).Close;
         if ComponentCount = (i-1) then
         begin
           CanClose := False;
           Exit;
         end;
       end;

    for i := 0 to Screen.FormCount - 1 do
    begin
      if Screen.Forms[i] is TTreeViewForm then
        TTreeViewForm(Screen.Forms[i]).FormIsClosing := True;
    end;
  except
    // don't sweat it
  end;

  try
    SaveMainMenuPreferencesOnDestroy;
  except
    on E:Exception do
      ShowMessage('Application Error when saving preferences: ' + E.Message);
  end;


  CanClose := True;
  if FDataType = snNoToken then
    Exit;
  CanClose := SaveSessionWithPermission;

  if not CanClose then
    Exit;

  try
    ClearData;
    UpdateMainWindow;
  finally
    //  Do nothing, even if this has an error we are closing the window anyway.  As soon as the process ends all memory is freed.
  end;

  CleanUpTempFiles;
end;

procedure TMegaForm.FormCreate(Sender: TObject);
var
  TempDir: String;
  SettingsFile: String;
begin
  FExplorersDisabledForLargeData := False;
  MenuRenderer := TMenuRenderer.Create;
  FNumSessionTests := 0;
  DataMonkeyJobResults.Visible := False;
  FMenuBitmap := TBitmap.Create;
  FMenuBitmap.SetSize(NewIcons.Width, NewIcons.Height);
  FCheckmark := TBitmap.Create;
  FCheckMark.SetSize(MenuItemImages.Width, MenuItemImages.Height);
  MenuItemImages.GetBitmap(0, FCheckMark);
  PublicationsLink.Font.Color := $717171;
  //NexusLabel.Font.Color := $717171;
  //NexusLabel.Font.Size := 7;
  FPopupNotifier := nil;
  FTimer := nil;
  FIsClosing := False;

  PredictLivingSequenceAction.Visible := IsDeveloper;
  PredictLivingSequenceAction.Enabled := IsDeveloper;
  MenuItem79.Visible := IsDeveloper;
  MenuItem79.Enabled := IsDeveloper;

  MegaRunMode := mrmGui;
  FCommandLineParamsProcessed := False;
  {$IFNDEF DEBUG}
    {$IFNDEF LINUX} { too risky for Linux as crash bugs pass through the final try/except block}
    FUniqueInstance := TUniqueInstance.Create(Self);
    FUniqueInstance.UpdateInterval := 800;
    FUniqueInstance.Identifier := 'A0000B39-6F7D-4A5A-95D4-47B44B658854';
    FUniqueInstance.OnOtherInstance := OnOtherInstance;
    FUniqueInstance.Enabled := True;
    {$ENDIF}
  {$ELSE}
  DEBUG_FILE := GetAppConfigDir(False) + PathDelim + 'megax.log';
  {$ENDIF}
  FUserRequestedUpdateCheck := False;
  FMainMenuHeight := 0;
  FDragging := False;
  FLastPos.X := -1;
  FLastPos.Y := -1;
  InitRecentlyUsedFiles;
  InitJobQueue;
  {$IFNDEF DARWIN}
  MegaMenuItem.Visible := False;
  MainMenu1.Items[0].Clear;
  {$ENDIF}
  InitMenuImages;
  HelpBrowser := nil;
  FAppLinkOptionsManager := nil;
  MultithreadedCalcIsRunning := False; // so we can prevent the user from running multiple multithreaded analyses (would lead to access violation)
  NumSingleThreadedMLAnalyses := 0; // so we can let the user run multiple analyses using a single thread for each
  FDataType := snNoToken;
  TempDir := GetHomeDirectory;
  if DirectoryExists(TempDir) then
    SetCurrentDir(TempDir); { so that the first call to GetCurrentDir won't return the same directory as the executable}
  OpenFileDirSTxt.Font.Color := CenterPanel.Color;
  PreferencesPopupMenu.Items.Clear;
  PreferencesPopupMenu.Items.Enabled:=True;
  MegaMainPreferences.PrefMenu := PreferencesPopupMenu;
  SettingsFile := GetPrivateFile('Private' + PathDelim + 'Ini' + PathDelim + 'Settings.ini');
  LoadMainMenuPreferencesOnCreate(SettingsFile);
  UpdatePreferencesMainMenu(UserPref_CloseProtoyperPopupStr, ClosePrototyperPopup, BoolToStr(ClosePrototyperPopup, ClosePrototyperPopup));
  MegaGuiFormCreate;
  {$IFNDEF DEBUG}
  MagicBtn.Visible := False;
  {$ENDIF}
  CheckHelpFiles;
  CheckWebDialogFiles;
  HelpKeyword := '';
  AssignContextHelpToMenus;
  {$IFDEF VISUAL_BUILD}
  AnchoredTimesCS := nil;
  VersionLabel.Visible := False;
  {$ENDIF}
  {$IFDEF PROTOTYPER_MODE}
  SetPrototyperModeNoPrompt;
  DatamonkeyBtn.Enabled:=False;
  DatamonkeyBtn.Visible:=False;
  {$ENDIF}
  // this ensures that MEGA writes and reads Decimal separator as '.' always in all countries
  //GetFormatSettings;
  FormatSettings.DecimalSeparator := '.';  // StrToFloat and FloatToStr uses this global variable. I initaially thought we shouldn't force period as it can break internationalization (in german , is a decimal) but in that case they wouldn't be able to read in a preiod decimal file.  For now I have to re-enable this as it will help more than hurt.
  InitMenus;
  InitBevels;
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
  //CheckUpdatesTimer.Enabled := True;
end;

procedure TMegaForm.OnTimer(Sender: TObject);
begin
  {$IFDEF PROTOTYPER_MODE}
  FTimer.Enabled := False;
  if Assigned(FPopupNotifier) and FPopupNotifier.vNotifierForm.Visible then
    FPopupNotifier.vNotifierForm.Hide;
  {$ENDIF}
end;

procedure TMegaForm.RunSessionFileTests;
var
  timeStr: String;
begin
  try
    timeStr := FormatDateTime('YYYY/MM/DD - hh:nn:ss', Now);
    IsSessionTest := True;
    IsLegacySessionTest := False;
    FSessionTestCriticalSection := TCriticalSection.Create;
    FNumSessionTests := 3;
    RunMasxFileTest;
    RunMdsxFileTest;
    RunMtsxFileTest;
    AppendSessionLog(Format('%s Successful test for new session files', [timeStr]));
  except
    on E:Exception do
    begin
      AppendSessionLog(Format('%s FAILED test for new session files: %s', [timeStr, E.Message]));
      Halt(1);
    end;
  end;
end;

procedure TMegaForm.RunMasxFileTest;
var
  filename: String;
begin
  try
    filename := GetExampleFilesFolderName + PathDelim + DROSOPHILA_FILE;
    if not FileExists(filename) then
      raise Exception.Create('session test failed - input data file not found');
    FindAlignmentEditorWindow(False);
    AlignEditMainForm.OpenAMEGA2File(filename, True);
    filename := NextAvailableFilename(GetTempFileName);
    filename := ChangeFileExt(filename, '.masx');
    AlignEditMainForm.SaveSession(filename);
    ShowAlignmentFromFile(filename);
  finally
    if FileExists(filename) then
      DeleteFile(filename);
  end;
end;

procedure TMegaForm.RunMdsxFileTest;
var
  filename: String;
begin
  try
    filename := GetExampleFilesFolderName + PathDelim + DROSOPHILA_FILE;
    if not FileExists(filename) then
      raise Exception.Create('session test failed - input data file not found');
    ProcessFile(filename);
    filename := NextAvailableFilename(GetTempFileName);
    filename := ChangeFileExt(filename, '.mdsx');
    D_InputSeqData.SaveSession(filename);
    if not DoOpenDataFile(filename, True) then
      raise Exception.Create('session test failed for mdsx file: ' + filename);
    ClearData;
  finally
    if FileExists(filename) then
      DeleteFile(filename);
  end;

  {
    add computeDistances, save session, and open session
  }
end;

procedure TMegaForm.RunMtsxFileTest;
var
  filename: String;
begin
  {
    DROSOPHILA_FILE = 'Drosophila_Adh.meg';
    MTCDNA_FILE = 'mtCDNA.meg';
    DISTANCE_DATA_FILE = 'Distance_Data.meg';
    CHLOROPLAST_FILE = 'Chloroplast_Martin.meg';
    HSP20_FILE = 'hsp20.fas';
    GENE_TREE_FILE = 'gene_tree.nwk';
    SPECIES_TREE_FILE = 'species_tree.nwk';
  }
  filename := GetExampleFilesFolderName + PathDelim + DROSOPHILA_FILE;
  if not FileExists(filename) then
    raise Exception.Create('session test failed - input data file not found');
  DoOpenDataFile(filename, True);
  ConstructMLTreeActionExecute(ConstructMLTreeAction); { the real test happens downstream - a session file will be saved and then read in again}
  {
    phylogeny construction
      ml tree
      nj tree
      me tree
      upgma tree
      parsimony tree
      timetree

    user trees
         ml tree
         ols tree
         parsimony tree
         display newick
  }
end;

procedure TMegaForm.AppendSessionLog(aMsg: String);
var
  aFile: TextFile;
  logFile: String;
begin
  try
    logFile := ExtractFilePath(Application.ExeName) + 'session_test.log';
    AssignFile(aFile, logFile);
    if not FileExists(logFile) then
      Rewrite(aFile)
    else
      Append(aFile);
    WriteLn(aFile, aMsg);
  finally
    CloseFile(aFile);
  end;
end;

procedure TMegaForm.SetDataType(AValue: TSnTokenCode);
begin
  if FDataType = AValue then Exit;
  FDataType := AValue;
end;

procedure TMegaForm.SetExplorersDisabledForLargeData(AValue: Boolean);
begin
  FExplorersDisabledForLargeData := AValue;
  { composition actions won't work when the SDE is disabled because site attributes don't get updated - refactoring would be needed to enable these}
  if FExplorersDisabledForLargeData then
  begin
    NucleotideCompAction.Enabled := False;
    AminoAcidCompAction.Enabled := False;
    CodonUsageAction.Enabled := False;
  end
  else
  begin
    NucleotideCompAction.Enabled := True;
    AminoAcidCompAction.Enabled := True;
    CodonUsageAction.Enabled := True;
  end;
end;

function TMegaForm.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
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

procedure TMegaForm.FormResize(Sender: TObject);
begin
  ResizeStatusBar;
  if Visible and Assigned(b2) then
    b2.Left := Width - 2;
  if FeedbackLink.Visible then
    FeedbackLink.Left := Round((CenterPanel.Width - FeedbackLink.Width) / 2);
end;

procedure TMegaForm.GammaParameterMLActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if MultithreadedCalcIsRunning then // prevent the user from running multiple multhreaded analyses concurrently or in conjunction with other ML analyses
  begin
    MessageDlg('A multi-threaded Maxmimum Likelihood analysis is already running in MEGA.'+ LineEnding +'In order to avoid thread collisions, only one multithreaded calculation may be run at a time.To run '+ LineEnding+'multiple ML analyses simultaneously, run each analysis using a single thread.', mtInformation, [mbOK], 0);
    Exit;
  end;
  if not AskUserToActivateDataFile(GammaParameterMLActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
     ShowMessage('Maximum Likelihood tree construction requires sequence data.');
     Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppEstRateVarAmongSitesML);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.ConstructMLTreeActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if MultithreadedCalcIsRunning then // prevent the user from running multiple multhreaded analyses concurrently or in conjunction with other ML analyses
  begin
    MessageDlg('A multi-threaded Maxmimum Likelihood analysis is already running in MEGA.'+ LineEnding +'In order to avoid thread collisions, only one multithreaded calculation may be run at a time.To run '+ LineEnding+'multiple ML analyses simultaneously, run each analysis using a single thread.', mtInformation, [mbOK], 0);
    Exit;
  end;
  if not AskUserToActivateDataFile(ConstructMLTreeActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
     ShowMessage('Maximum Likelihood tree construction requires sequence data.');
     Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppInfer);
      ProcessPack.AddProcessType(ppML);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.GeneDupsActionExecute(Sender: TObject);
begin
  if not AskUserToKeepDataActive then
    Exit;
  try
    if Assigned(FGDWizard) then
      FreeAndNil(FGDWizard);
    FGDWizard := TGDWizardForm.Create(Self);
    FGDWizard.CancelledCallback := GeneDupsCancelled;
    FGDWizard.Show;
  Except
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TMegaForm.LinksBtnMouseEnter(Sender: TObject);
begin
  BtRed.GetBitmap(BT_IMG_MEGA_LINKS, LinksBtn.Picture.Bitmap);
end;

procedure TMegaForm.LinksBtnMouseLeave(Sender: TObject);
begin
  BtBlue.GetBitmap(BT_IMG_MEGA_LINKS, LinksBtn.Picture.Bitmap);
end;

procedure TMegaForm.MagicBtnClick(Sender: TObject);
//var
//  i: Integer;
//  response: Integer;
//  js: String = '';
//  aList: TStringList;
begin
  //aList := TStringList.Create;
  //js := JavaScriptFontPickerOptions('my id');
  //aList.Text := js;
  //aLIst.SaveToFile('/home/gstecher/Downloads/deleteme.txt');

  //WriteOutputDlg.ShowModal;
  //HtmlOptionsDialog.Show;
  //HtmlOptionsDialog.FormStyle := fsStayOnTop;
  //HtmlOptionsDialog.LoadOptionsFile(wofTreeOptionsLabelsFile, js, 'Taxa Labels', 500, 400, HelpContext);
//);
end;

procedure TMegaForm.MCLBiasActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(MCLBiasActionExecute) then
    Exit;
  if not HasNucleotideData then
  begin
    ShowMessage('Transition/transversion bias calculations require nucleotide sequence data.');
    Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppTsTvMCL);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.MCLMatrixActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(MCLMatrixActionExecute) then
    Exit;
  if not HasNucleotideData then
  begin
    ShowMessage('MCL substitution matrix calculations require nucleotide sequence data.');
    Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppSubPatternEstMCL);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.MeanDiversityEntireActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(MeanDiversityEntireActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
    ShowMessage('Diversity estimation requires sequence data.');
    Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppDistEst);
      ProcessPack.AddProcessType(ppAvgOverallPops);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.MeanDiversityInterPopActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(MeanDiversityInterPopActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
    ShowMessage('Diversity estimation requires sequence data.');
    Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppDistEst);
      ProcessPack.AddProcessType(ppInterPopDiversity);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.MeanDiversityWithinActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(MeanDiversityWithinActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
    ShowMessage('Mean Diversity estimation requires sequence data.');
    Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppDistEst);
      ProcessPack.AddProcessType(ppAvgInSubPops);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.MEGABrowserActionExecute(Sender: TObject);
begin
  Showmessage('Hello MEGABrowser.');
end;

procedure TMegaForm.MegaCCOnlineManualActionExecute(Sender: TObject);
var
  url: String;
begin
  try
    url := 'http://megasoftware.net/web_help_10/index.htm#t=MEGA-CC_Overview.htm';
    if not OpenUrl(url) then
      raise Exception.Create('failed to open ' + url);
  Except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TMegaForm.MEGAWebsiteActionExecute(Sender: TObject);
begin
  try
    if not OpenUrl('http://www.megasoftware.net') then
      raise Exception.Create('failed to open url: http://www.megasoftware.net');
  Except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TMegaForm.ConstructMETreeActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(ConstructMETreeActionExecute) then
    Exit;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppInfer);
      ProcessPack.AddProcessType(ppME);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.ModelsMenuImgMouseEnter(Sender :TObject);
begin
  Hover72x90.GetBitmap(MEN_IMG_MODELS, ModelsMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.ModelsMenuImgMouseLeave(Sender :TObject);
begin
  Default72x90.GetBitmap(MEN_IMG_MODELS, ModelsMenuImg.Picture.Bitmap);
end;

procedure TMegaForm.ModelsToolMenuItemClick(Sender: TObject);
begin
  ModelsToolMenuItem.Checked := not ModelsToolMenuItem.Checked;
  ModelsMenuImg.Visible := ModelsToolMenuItem.Checked;
  AlignTopToolbarButtons;
end;

procedure TMegaForm.MolecularClockTestActionExecute(Sender: TObject);
var
  ProcessPack : TProcessPack;
begin
  if not AskUserToActivateDataFile(MolecularClockTestActionExecute) then
    Exit;
  if not HasSequenceData then
  begin
     ShowMessage('Maximum Likelihood Analysis requires sequence data.');
     Exit;
  end;
  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppMLClock);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error occurred and MEGA could not complete the command: ' + E.Message);
    end;
  finally
    ProcessPack.Free;
  end;
end;

procedure TMegaForm.ConstructNJTreeActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(ConstructNJTreeActionExecute) then
    Exit;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppInfer);
      ProcessPack.AddProcessType(ppNJ);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.NetMeanActionExecute(Sender: TObject);
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;
  if not AskUserToActivateDataFile(NetMeanActionExecute) then
    Exit;
  if not(HasSequenceData OR HasDistanceData) then
  begin
    ShowMessage('Net Between Group average requires sequence or pairwise distance data.');
    Exit;
  end;

  try
    try
      ProcessPack := TProcessPack.Create();
      ProcessPack.AddProcessType(ppDistEst);
      ProcessPack.AddProcessType(ppNetBetweenGpAvg);
      ProcessPack.PerformAnalysis;
    Except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred so that the analysis could not be completed: ' + E.Message);
    end;
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

procedure TMegaForm.DisplayNewickTreeActionExecute(Sender: TObject);
var
  ATreeOpenDlg: TOpenDialog;
begin
  try
    try
      ATreeOpenDlg := TOpenDialog.Create(Self);
      with ATreeOpenDlg do
      begin
        Title := 'Select Newick Standard File';
        DefaultExt := '*.*';
        Filter :=  NewickFilesFilter;
        Filename := '';
        Options := Options + [ofFileMustExist];
        InitialDir := ChangeInitialDirectorySaveDialogForMac(InitialDir);
        if Execute then
          LaunchNewickThread(Filename);
      end
    Except
      on E: Exception do
      begin
        ShowMessage('Oh no! An error occurred when displaying a newick tree: ' + E.Message);
      end;
    end;
  finally
    if Assigned(ATreeOpenDlg) then
      ATreeOpenDlg.Free;
  end;
end;

procedure TMegaForm.NucleotideCompActionExecute(Sender: TObject);
var
  ExportAs: TExportType;
  SaveLocation: String;
begin
  if (Sender <> V_SeqDataExplorer) and (not AskUserToActivateDataFile(NucleotideCompActionExecute)) then
    Exit;

  if not HasNucleotideData then
  begin
    ShowMessage('You need to have nucleotide sequences for this function');
    exit;
  end;
  if VS_SeqDataExplorer.isTranslated then
  begin
    ShowMessage('The data is currently translated. Please Go to Data Explorer and Untranslate it first.');
    Exit;
  end;

  try
    SaveLocation := DataFileNameOnly + '_nucleotide-composition';
    if Assigned(V_SeqDataExplorer) then
      ExportAs := V_SeqDataExplorer.SelectedStatsExportType
    else
      ExportAs := EXexcelXMLDisp;
    ExportAs := PromptUserWriteOutput(SaveLocation, VS_SeqDataExplorer.CurAttrDisp = megNone, ExportAs);
    if ExportAs = EXnone then
      Exit;
    if Assigned(V_SeqDataExplorer) then
      V_SeqDataExplorer.SelectedStatsExportType := ExportAs;
    VS_SeqdataExplorer.StatUseOnlyMarkedSitesItem := WriteOutputDlg.IsUseHighlightedSites;
    VS_SeqdataExplorer.StatUseOnlyUnMarkedSitesItem := WriteOutputDlg.IsUseUnHighlightedSites;
    VS_SeqdataExplorer.StatAllSelSitesItem := WriteOutputDlg.IsUseAllSites;
    LaunchNucCompositionStatsThread(SaveLocation, ExportAs);
  Except
    on E: Exception do
      ShowMessage('Unable to calculate Nucleotide Composition Error: ' + E.Message);
  end;
end;

procedure TMegaForm.OnlineManualActionExecute(Sender: TObject);
var
  url: String;
begin
  try
    url := WEBSITE_URL + '/web_help_10/index.htm';
    if not OpenUrl(url) then
      raise Exception.Create('failed to open ' + url)
  Except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

initialization

  {$IFDEF UNIX}
    {$IFDEF DARWIN}
    {$I resources_mega_mac.lrs}
    {$ELSE}
    {$I resources_mega_unix.lrs}
    {$ENDIF DARWIN}
  {$ELSE}
    {$I resources_mega_windows.lrs}
  {$ENDIF UNIX}


end.

