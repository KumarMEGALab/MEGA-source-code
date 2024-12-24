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

unit manalysisprefdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, ValEdit, StdCtrls, Spin, CheckLst, mdistpack, mtreepack,
  MegaConsts, MProcessPack, MegaAnalysisPrefStrings, Grids, EditBtn, ActnList,
  Menus, manalysissettings, mimageform, app_options_frame, syncobjs, Contnrs;

const
  MyEditableCellColor        : TColor = clYellow;  // for background
  MyNotApplicableColor : TColor = clSilver;  // for font
  MyDefaultBgColor     : TColor = $00f7f8f8;  // for fill

  IndentPrefix         : String = '   '; // some space


type



  { TAnalysisPrefDlg }

  TAnalysisPrefDlg = class(TForm)
    ArrowImg32: TImageList;
    DummyMenuItem: TMenuItem;
    NecessaryDummyMenu: TMainMenu;
    SaveSettingsAction: TAction;
    OkAction: TAction;
    HelpAction: TAction;
    CancelAction: TAction;
    ActionList1: TActionList;
    AppOptionsSheet: TTabSheet;
    PercentSitesCBox: TComboBox;
    PercentSitesSE: TFloatSpinEdit;
    NumReplicatesCBox: TComboBox;
    NumSamplesPanel: TPanel;
    NumSamplesCBox: TComboBox;
    NumSamplesSE: TSpinEdit;
    NumReplicatesPanel: TPanel;
    NumReplicatesSE: TSpinEdit;
    PercentSitesPanel: TPanel;
    ToolBar1: TToolBar;
    HelpButton: TToolButton;
    CancelButton: TToolButton;
    OkButton: TToolButton;
    TransposeDataPanel: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    ProtoLabelsToUseEdit: TEdit;
    ProtoLabelledSitesCombo: TComboBox;
    ButtonsPanel: TPanel;
    ImageList2: TImageList;
    ArrowImg16: TImageList;
    OptionsChosenGrid: TDrawGrid;
    TransposeDataCheckbx: TCheckBox;
    TreeFileNameEdit: TFileNameEdit;
    FPickListComboBx: TComboBox;
    Label2: TLabel;
    DataTypeLabel: TLabel;
    Label3: TLabel;
    seFloatSpinEdit: TFloatSpinEdit;
    NoncodingChkBx: TCheckBox;
    Pos3ChkBx: TCheckBox;
    Pos1ChkBx: TCheckBox;
    Pos2ChkBx: TCheckBox;
    ChkLBx: TCheckListBox;
    Label1: TLabel;
    MaxExecutionTimeEdit: TFloatSpinEdit;
    MaxRuntimeCheckbox: TCheckBox;
    MaxExecutionTimePanel: TPanel;
    DataSettingsDefaultsBtn: TButton;
    MissingSymbolEdit: TLabeledEdit;
    IdenticalSymbolEdit: TLabeledEdit;
    GapSymbolEdit: TLabeledEdit;
    SaveAnalysisDlg: TSaveDialog;
    OptionsPageControl: TPageControl;
    Panel1: TPanel;
    SelectLabelsPanel: TPanel;
    SelectCodonPosPanel: TPanel;
    seIntegerSpinEdit: TSpinEdit;
    OptionsSummaryTab: TTabSheet;
    DataSettingsPage: TTabSheet;
    PrevOptSheet: TTabSheet;
    PrevOptionVListEditor: TValueListEditor;
    procedure CancelBtnClick(Sender: TObject);
    procedure ChkLBxClickCheck(Sender: TObject);
    procedure DataSettingsDefaultsBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormResize(Sender: TObject);
    procedure FPickListComboBxChange(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure MaxRuntimeCheckboxClick(Sender: TObject);
    procedure NumReplicatesCBoxChange(Sender: TObject);
    procedure NumSamplesCBoxChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure OptionsChosenGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure OptionsChosenGridGetCellHint(Sender: TObject; ACol,
      ARow: Integer; var HintText: String);
    procedure OptionsChosenGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure OptionsChosenGridSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure OptionsChosenGridTopleftChanged(Sender: TObject);
    procedure PercentSitesCBoxChange(Sender: TObject);
    procedure ProtoLabelledSitesComboChange(Sender: TObject);
    procedure SaveSettingsBtnClick(Sender: TObject);
    procedure seFloatSpinEditChange(Sender: TObject);
    procedure seFloatSpinEditEnter(Sender: TObject);
    procedure seFloatSpinEditExit(Sender: TObject);
    procedure seIntegerSpinEditChange(Sender: TObject);
    procedure seIntegerSpinEditEnter(Sender: TObject);
    procedure seIntegerSpinEditExit(Sender: TObject);
    procedure CodPosChkBxClick(Sender: TObject);
    procedure TreeFileNameEditAcceptFileName(Sender: TObject; var Value: String
      );
    procedure TreeFileNameEditExit(Sender: TObject);
  private
    FEditorRow: Integer;
    FIsSettingUpOptions: Boolean;
    FIsWorkflowSettings: Boolean;
    FNeedsAppOptions: Boolean;
    FNumSitesBeforeSubsetting: Integer;
    FUpdatingSpinEdit: Boolean;
    FArrow: TBitmap;
    FArrow2: TBitmap;
    FNumTaxa: Integer;
    FUsingTreeFromTopologyEditor: Boolean;
    Gradient1Color: TColor;
    Gradient2Color: TColor;
    GridLineColor: TColor;
    FCurrentOptions: TList;
    FEnabledOptions: TList;
    FPreviousOptions: TList;
    FOperation: TDistTreeDlgOption;  // defines how the dialog will be displayed
    FIsCoding: Boolean;
    FIsNuc   : Boolean;
    FIsAmino : Boolean;
    FIsDist:   Boolean;  // if distance was the input data
    FHasGps:   Boolean;
    FTaxaList: TStringList;
    FTaxaPickList: String;
    FHasSiteLabelTypes: Boolean;
    FDistPack:     TDistPack;       // gets from outside to manipulate
    CurSubstType:  TDistType;
    FTreePack:     TTreePack;
    CustomPanels: TList; // Custom panels are added here
    IsMolSeq: Boolean;
    IsPreventChangeEventCount: Integer;  // this works like a reference counter
    IsUpdatingAnalysisScopeRows: Integer;
    CurSpinEditRow : Integer;
    FOptionsStringList: TStringList;
    FOptionsGrid: TOptionsGridFrame;
    procedure UpdateCurrentEnabledOptions;
    procedure InitSettings;
    function ScrollbarVisible: Boolean;
    procedure UpdateMaxSitesPerSample;
    function GetIsBootstrapSubSampling: Boolean;
    function GetMaxPercentSnvDiffForIdentSeqs: Double;
    procedure SetNeedsAppOptions(AValue: Boolean);
    function SubstitutionTypeStr: String;
    procedure PositionButtonsPanel;
    function GetMaxRateRatio: Extended;
    function ReltimeLsShouldDisableBootstrapReps: Boolean;
    function GetSetting(AName: String): TAnalysisSetting;
    procedure RefreshSettings;

    procedure UpdateSubsampleUpsampleOptions;
    procedure UpdateCurrentOptionsStringList;
    function SelectedGeneticCode: String;
    function GetCellColor(aRow: Integer): TColor;
    function GetIndentLevelForName(AName: String): Integer;
    procedure DrawRectEdges(ARect: TRect; DoLeft, DoTop, DoRight, DoBottom: Boolean);
    function GetGridText(const aCol: Integer; const aRow: Integer): String;
    procedure FreeOptions;
    procedure FreeCurrentOptions;
    procedure FreePreviousOptions;
    function GetMaoFormatLen: Integer; { length of longest name string + 5, for formatting the mao file}
    function ValidSpecialIs(TheChar: Char): Boolean;
    function ExamineThreeSymbols(gap,miss,iden: Char): String;
    function ValidateLabelledSitesOption: Boolean;
    procedure CMDialogKey(var msg: TCMDialogKey);
    procedure AddDataSettingsOptions;

    procedure AddStringSetting(AName: String; AValue: String = ''; IsReadOnly: Boolean = False; IsApplicable: Boolean = True; aHint: String = '');
    function AddIntegerSetting(AName: String; IsReadOnly: Boolean = False; IsApplicable: Boolean = True; aHint: String = ''): TIntegerAnalysisSetting;
    function AddFloatSetting(AName: String; IsReadOnly: Boolean = False; IsApplicable: Boolean = True; aHint: String = ''): TFloatAnalysisSetting;
    procedure AddPickListSetting(AName: String; AValue: String; AList: String; aHint: String = '');
    procedure AddRow(ASetting: TAnalysisSetting);
    procedure UpdateRow(const AName: String; const AValue: String; PickListName: String='');
    procedure UpdatePicklist(const AName: String; const AValue: String; const PickListName: String);
    function IsCurrentSetting(AName: String; AValue: String): Boolean;
    function HasSetting(AName: String; var ARow: Integer): Boolean; overload;
    function HasSetting(AName: String): Boolean; overload;
    function TaxaNamesPicklist: String;
    procedure SetStateForLittleBootstrapParams;
    procedure SetStateForModelSelectionParams;
    function OperationSupportsSubsamplingApproach(aOperation: TDistTreeDlgOption): Boolean;
    function OperationSupportsSubsamplingDoubleCheck(aOperation: TDistTreeDlgOption): Boolean;

    procedure AddOperationTypeRows;

    procedure AddAnalysisScopeRows;
    procedure UpdateAnalysisScopeRows;

    procedure AddPhyloAnalysisMethodRows;
    procedure UpdatePhyloAnalysisMethodRows;

    procedure AddDistAnalysisOptionsRows;
    procedure UpdateDistAnalysisOptionsRows;

    procedure AddSubsModelRows;
    procedure UpdateModelRows(ACategory: String);

    procedure UpdateVarEstimationRow;
    procedure UpdateModelDependentRows;
    function  SetupSubsModelPickList: Boolean;
    function  AddSubsModelPicklist: Boolean;
    function  SetupSubsToIncludePickList: Boolean;

    procedure AddDataSubsetRows;
    procedure UpdateDataSubsetRows;

    procedure AddGeneticCodeRow;
    procedure UpdateGeneticCodeRow;

    procedure AddTreeSearchRows;
    procedure UpdateTreeSearchRows;
    procedure UpdateMLInitialTreeRow;

    procedure AddClockRows;
    procedure UpdateClockRows;

    procedure AddSystemResourceUsageRows;
    procedure UpdateSystemResourceUsageRows;

    procedure AddBeamOptions;
    procedure UpdateBeamOptions;

    procedure StoreOption(AName, AValue: String; Suffix: String = ''); overload;
    procedure StoreOption(aSetting: TAnalysisSetting); overload;
    procedure StoreAllOptions;
    function CurrentOption(AName: String): TAnalysisSetting;
    function CurrentOptionStr(AName: String): String;
    function PreviousOption(AName: String; aType: TSettingType = stNone): TAnalysisSetting;
    function PreviousOptionStr(AName: String): String;
    function NumOccurrencesInPrevOps(AName: String): Integer;
    function IsBlacklistedFromUsingPrevOp(settingName: String): Boolean; { some things like opsPercentSitesPerSample should be calculated based on current data}
    procedure RemovePreviousOption(aName: String);
    function CheckPreviousPickListOptionValid(aName: String; aPickList: String): Boolean;
    procedure ShowActiveEllipsisPanels; // makes all elipses panels visible
    procedure PositionEllipsisPanel(APanel: TPanel);
    function GetCustomPanelPosition(APanel: TPanel): Integer;
    function GetCustomPanelRowName(APanel: TPanel): String;
    function IsTreeFileRow(ARow: Integer): Boolean;
    procedure UpdateTreeFileEdit(ARow: Integer);
    function  isSpinRow(sRKey: String): Boolean;
    function spinRowIsInteger(sRKey: String): Boolean;
    function spinRowMin(sRKey: String): double;
    function spinRowMax(sRKey: String): double;
    function spinRowIncrement(AName: String): Double;
    procedure SubstTypeChange(Value: String);
    procedure AssignHelpContexts;
    procedure SetIsCoding(AOption: Boolean);
    procedure SetIsNuc(AOption: Boolean);
    procedure SetIsAmino(AOption: Boolean);
    procedure SetIsDist(AOption: Boolean);
    procedure SetHasGps(AOption: Boolean);

    function  GetIsSiteLabelTypeUsed(Index: Char): Boolean;
    procedure SetIsSiteLabelTypeUsed(Index: Char; Value: Boolean);
    procedure SetHasSiteLabelTypes(Value: Boolean);

    function  GetIsIncOnlyLabelledSites: Boolean;
    procedure SetIsIncOnlyLabelledSites(Value: Boolean);
    function  GetIsIncOnlyUNLabelledSites: Boolean;
    procedure SetIsIncOnlyUNLabelledSites(Value: Boolean);

    function GetIsComputeVar: Boolean;
    function GetIsAnalyticalVar: Boolean;
    function GetReltimeVarianceMethod: TReltimeVarianceMethod;
    function GetBootVarReps: Integer;
    function GetBootVarRandSeed: Integer;

    function GetIsCompleteDeletion: Boolean;
    function GetIsPartialDeletion: Boolean;
    function GetIsPairwiseDeletion: Boolean;
    function GetSiteCoverage: Integer;
    function GetIncludeCodonPos: TIncludeCodonPos;
    function GetClockType: Integer;
    function GetTreeReliability: TTreeType;
    function GetReliabilityTestReps: Integer;
    function GetReliabilityTestRandSeed: Integer;
    function GetRateMergeOption: Boolean;
    function GetMLSearchFilter: Extended;
    function GetIsRecodeBases: Boolean;
    function GetRecodeScheme: String;  // contains the name and an embedded allele structure

    function SelectionWidth:Integer;
    procedure ResizeColumns;
    procedure AdjustColWidths;
    procedure SizeFormToFitOptions;
    function GetPickList(RowName: String): String;
    function GetLongestNameLength: Integer;
    function SettingIsEditable(aRow: Integer): Boolean;
    procedure SaveSettingsToFile;
    procedure LoadSettingsFromFile;
    procedure UpdatePickListValues;
    procedure UpdatePickListValue(aSetting: TPickListAnalysisSetting);
  public
    ProcessPackTemp : TProcessPack;
    UserTreeFile: String;
    SkipTreeActionActivated: Boolean;
    procedure TransferSettings(var destination: TFPHashList); { a new way to populate TAnalysisInfo. Instead of coding knowledge of every analysis into TAnalysisInfo, we can just give it the list of settings which can be queried by name downstream as needed}
    function CurrentOptionIsInArray(optionName: String; aArray: TArrayOfString): Boolean;
    function GetDefaultOptions(AName: String; PickListName: String=''): String;
    function HasDefaultOption(AName: String; PickListName: String=''): Boolean;
    function GetPickListSettingValue(picklistName: String): String;
    function GetIntegerSettingValue(settingName: String): Integer;
    function GetFloatSettingValue(settingName: String): Double;
    function GetStringSettingValue(settingName: String): String;
    function generateMAOFile(): TStringList; overload;
    function generateMAOFile(var aList: TStringList): Boolean; overload;
    function GetMaoFileSaveLocation(var saveLoc: String): Boolean;
    procedure AddAppOptions(optionsFrame: TOptionsGridFrame; tabCaption: String);
    procedure ClearAppOptions;
    function GetChosenOptionStrings: TStringList;
    procedure SendTextualSettingsToWebtop;
    procedure AddSiteLabelType(Value: Char); // HasSiteLabelTypes must be true
    procedure SetTaxaList(AList: TStringList);
    procedure GetThreeTaxa(var A, B, C: Integer); // for Tajima clock test

    function GetNumThreadsToUse: Integer;
    function GetInitialTreeMethod: Integer;
    function GetClockTestType: TClockType;
    function GetClockLevel: TClockLevel;
    function SubstitutionTypeString: String;
    property Operation: TDistTreeDlgOption read FOperation write FOperation; // IF YOU ARE GETTING AN ERROR ON THIS LINE: You probably used auto complete ctrl+shift+c which tacks on ' read FOperation', just remove it and MEGA will compile.

    property IsCoding: Boolean   read FIsCoding write SetIsCoding;
    property IsNuc:    Boolean   read FIsNuc write SetIsNuc;   // if raw data is nuc data
    property IsAmino:  Boolean   read FIsAmino write SetIsAmino; // raw data is amino acids
    property IsDist:   Boolean   read  FIsDist write SetIsDist;  // raw data is distance
    property HasGps:   Boolean   read FHasGps write SetHasGps;

    property HasSiteLabelTypes: Boolean read FHasSiteLabelTypes write SetHasSiteLabelTypes;
    // User selections
      // Distance
    property DistPack: TDistPack     read FDistPack;
      // Pairwise dist and variance
    property IsComputeVar: Boolean     read GetIsComputeVar;
    property IsAnalyticalVar: Boolean  read GetIsAnalyticalVar;
    property ReltimeVarianceMethod: TReltimeVarianceMethod read GetReltimeVarianceMethod;
      // Strategy for bootstrap variance
    property BootVarReps:     Integer read GetBootVarReps;
    property BootVarRandSeed: Integer read GetBootVarRandSeed;
    property IsBootstrapSubSampling: Boolean read GetIsBootstrapSubSampling;
    property NumSitesBeforeSubsetting: Integer read FNumSitesBeforeSubsetting write FNumSitesBeforeSubsetting;
      // Include sites options
    property IsCompleteDeletion: Boolean  read GetIsCompleteDeletion;
    property IsPartialDeletion: Boolean   read GetIsPartialDeletion;
    property IsPairwiseDeletion: Boolean   read GetIsPairwiseDeletion;
    property SiteCoverage: Integer        read GetSiteCoverage;

    property IncludeCodonPos: TIncludeCodonPos read GetIncludeCodonPos;
    property IsIncludeOnlyLabelledSites: Boolean   read GetIsIncOnlyLabelledSites   write SetIsIncOnlyLabelledSites;
    property IsIncludeOnlyUNLabelledSites: Boolean read GetIsIncOnlyUNLabelledSites write SetIsIncOnlyUNLabelledSites;
    property IsSiteLabelTypeUsed[Index: Char]: Boolean read GetIsSiteLabelTypeUsed write SetIsSiteLabelTypeUsed;
       // Tree making options
    property TreePack: TTreePack      read FTreePack;
    property UsingTreeFromTopologyEditor: Boolean read FUsingTreeFromTopologyEditor write FUsingTreeFromTopologyEditor;
       // Reliablity Test
    property TreeReliability:         TTreeType read GetTreeReliability;
    property ReliabilityTestReps:     Integer   read GetReliabilityTestReps;
    property ReliabilityTestRandSeed: Integer   read GetReliabilityTestRandSeed;
      // recode scheme
    property IsRecodeBases: Boolean            read GetIsRecodeBases;
    property RecodeScheme: String              read GetRecodeScheme;
      // ML Clock Test
    property ClockType: Integer               read GetClockType;
    property ClockLevel: TClockLevel read GetClockLevel;
    property ClockTestType: TClockType read GetClockTestType;
    property IsRateMerge: Boolean read GetRateMergeOption;
    property SearchFilter: Extended  read GetMLSearchFilter;
    property NumTaxa: Integer read FNumTaxa write FNumTaxa;
    property MaxRateRatio: Extended read GetMaxRateRatio;
    property IsWorkflowSettings: Boolean read FIsWorkflowSettings write FIsWorkflowSettings;

    // BEAM analysis
    property MaxPercentSnvDiffForIdentSeqs: Double read GetMaxPercentSnvDiffForIdentSeqs;
    property NeedsAppOptions: Boolean read FNeedsAppOptions write SetNeedsAppOptions;
  end;

  function IsJustDistCompute(AOperation: TDistTreeDlgOption): Boolean;
  function IsDistWithTreeCompute(AOperation: TDistTreeDlgOption): Boolean;
  function IsDistSelectionCompute(AOperation: TDistTreeDlgOption): Boolean;
  function IsMLCompute(AOperation: TDistTreeDlgOption): Boolean;
  function IsDisparityCompute(AOperation: TDistTreeDlgOption): Boolean;
  function IsMPCompute(AOperation: TDistTreeDlgOption): Boolean;
  function IsEllipsisRow(AName: String): Boolean;

var
  AnalysisPrefDlg: TAnalysisPrefDlg;
  AnalysisPrefsCriticalSection: TCriticalSection;

implementation

{$R *.lfm}

uses
  {$IFDEF DEBUG}lazlogger, {$ENDIF}
  MegaVerConsts, MegaUtils, MSelectGeneticCodeDlg, KeywordConsts, Mega_Main,
  typinfo, MutationDetailView, Math, mfileutils, CentralDialogBox_HC,
  ContextHelp_HC, Walk_Through_MEGA, mhelpfiles, mhelpkeywords, fpjson, jsonparser,
  MegaPrivateFiles, uMegaBrowser, StringUtils, MD_InputSeqData;

function IsJustDistCompute(AOperation: TDistTreeDlgOption): Boolean;
begin
  case AOperation of
    dtdoPairwiseDist,
    dtdoOverallMean,
    dtdoWithinGroupMean,
    dtdoBetweenGroupMean,
    dtdoNetGroupMean,
    dtdoAvgDiversityWithinSubPops,
    dtdoAvgDiversityForEntirePop,
    dtdoInterPopDiversity,
    dtdoPropOfInterPopDiversity:  Result := True;
    else
      Result := False;
  end;
end;

function IsDistWithTreeCompute(AOperation: TDistTreeDlgOption): Boolean;
begin
  case AOperation of
    dtdoNJTree,
    dtdoUPGMATree,
    dtdoMETree,
    dtdoOLSComputeUserTreeBLens,
    dtdoRelTimeLS, dtdoRtdtLS,
    dtdoOLSInteriorBranchTest  : Result := True;
  else
    Result := False;
  end;
end;

function IsDistSelectionCompute(AOperation: TDistTreeDlgOption): Boolean;
begin
  case AOperation of
      dtdoSelectionExactTest,
      dtdoSelectionZTest       : Result := True;
  else
    Result := False;
  end;
end;

function IsMLCompute(AOperation: TDistTreeDlgOption): Boolean;
begin
  case AOperation of
      dtdoMLClockTest,
      dtdoMLClockTestLocal,
      dtdoRelTimeML, dtdoRtdtML,
      dtdoCorrTestML,
      dtdoMLModelTest, dtdoMLModelTamer, dtdoMLModelTestIQTree,
      dtdoLbsAnalyzeTree, dtdoLbsInference, dtdoLbsTiming,
      dtdoMLTree,
      dtdoMLIQTree,
      dtdoMLInferAncSeq,
      dtdoMLInferAncSeqMyPeg,
      dtdoMLPredictLivingSeq, dtdoEpML,
      dtdoMLComputePattern,
      dtdoMLTsTvBias,
      dtdoMLGammRates,
      dtdoMLInferSiteBySiteRates,
      dtdoMLComputeUserTreeBLens, dtdoBEAM : Result := True;
  else
      Result := False;
  end;
end;

function IsDisparityCompute(AOperation: TDistTreeDlgOption): Boolean;
begin
  case AOperation of
      dtdoCompositionDistance,
      dtdoDisparityIndex,
      dtdoDisparityIndexTest    : Result := True;
  else
      Result := False;
  end;
end;

function IsMPCompute(AOperation: TDistTreeDlgOption): Boolean;
begin
  case AOperation of
      dtdoMPTree,
      dtdoMPComputeUserTreeBLens,
      dtdoMPInferAncSeqMyPeg,
      dtdoMPInferAncSeq          : Result := True;
  else
      Result := False;
  end;
end;

function IsEllipsisRow(AName: String): Boolean;
begin
  if (AName = opsCodonPosPanel2) or
     (AName = opsSelectLabelsPanel2)  or
     (AName = opsPickUserTree2) or
     (AName = opsPickStartTree2) or
     (AName = opsCalibrationFile2) or
     (AName = opsMPRandomAddTreesPanel2) or
     (AName = opsBootReps2        ) or
     (AName = opsMonteCarloReps2  ) or
     (AName = opsGammaPara2       ) or
     (AName = opsGammaCats2       ) or
     (AName = opsSiteCoverage2    ) or
     (AName = opsMaxTrees2        ) or
     (AName = opsFixedRValue2     ) or
     (AName = opsMinMiniSearchFactor2) or
     (AName = opsMESearchLevelPanel2) or
     (AName = opsMPSearchLevelPanel2) or
     (AName = opsMaxRateRatio2) or
     (AName = opsGeneticCodeTable2) then
       Result := True
  else
    Result := False or (AName = opsMLNumThreads);
end;

{ TAnalysisPrefDlg }

procedure TAnalysisPrefDlg.FormCreate(Sender: TObject);
begin
  FIsWorkflowSettings := False;
  FEditorRow := -1;
  FIsSettingUpOptions := False;
  FUpdatingSpinEdit := False;
  TreeFileNameEdit.Filter := NewickFilesFilter;
  Constraints.MinWidth := ButtonsPanel.Width;
  FArrow := TBitmap.Create;
  FArrow.SetSize(16, 16);
  ArrowImg16.GetBitmap(0, FArrow);
  FArrow2 := TBitmap.Create;
  FArrow2.SetSize(32, 32);
  ArrowImg32.GetBitmap(0, FArrow2);
  FNumTaxa := 0;
  Gradient1Color := RGBToColor($de, $df, $e1);
  Gradient2Color := RGBToColor($ec, $ec, $ee);
  GridLineColor := $00c5c5c5;
  FOptionsStringList := TStringList.Create;
  FCurrentOptions := TList.Create;
  FEnabledOptions := TList.Create;
  FpreviousOptions := TList.Create;
  {$IFDEF MYPEG_ONLY}
   HelpBtn.Enabled := False; // we don't have any context sensitive help for MYPEG_ONLY
   HelpBtn.Visible := False;
  {$ENDIF}
  UserTreeFile := EmptyStr;
  IsPreventChangeEventCount := 1;
  IsPreventChangeEventCount := 0;
  FIsNuc     := False;
  FIsAmino   := False;
  FIsCoding  := False;
  FIsDist    := False;
  IsMolSeq   := False;
  FHasSiteLabelTypes := False;
  HasGps    := False;
  FOperation := dtdoPairwiseDist;
  FDistPack  := TDistPack.Create;
  FTreePack  := TTreePack.Create;
  FTaxaList := TStringList.Create;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Analysis Preferences';

  CustomPanels := TList.Create;
  CustomPanels.Add(@SelectCodonPosPanel);
  CustomPanels.Add(@SelectLabelsPanel);
  CustomPanels.Add(@MaxExecutionTimePanel);
  CustomPanels.Add(@TransposeDataPanel);
  CustomPanels.Add(@NumSamplesPanel);
  CustomPanels.Add(@NumReplicatesPanel);
  CustomPanels.Add(@PercentSitesPanel);

  IsPreventChangeEventCount := 0;
  DataSettingsPage.TabVisible := isPrototyper;
  ProcessPackTemp := nil;
  OptionsPageControl.ActivePage := OptionsSummaryTab;
  if IsPrototyper then
    OkButton.Action := SaveSettingsAction
  else
    OkButton.Action := OkAction;
  LoadSettingsFromFile;
  OptionsChosenGrid.Color := MyDefaultBgColor;
  ImageForm.UpdateImgList(Self);
  {$IFDEF LINUX}
  OptionsChosenGrid.DefaultRowHeight := seIntegerSpinEdit.Height;
  {$ENDIF}
end;

procedure TAnalysisPrefDlg.FormDestroy(Sender: TObject);
begin
  ProcessPackTemp := nil;
  if Assigned(FArrow) then FArrow.Free;
  if FDistPack <> nil then FDistPack.Free;
  if FTreePack <> nil then FTreePack.Free;
  if CustomPanels <> nil then CustomPanels.Free;
  FreeAndNil(FTaxaList);
  FreeCurrentOptions;
  FreePreviousOptions;
  SkipTreeActionActivated := False;
  if Assigned(FOptionsStringList) then
    FOptionsStringList.Free;
  FCurrentOptions.Free;
  FPreviousOptions.Free;
  FEnabledOptions.Free;
end;

function TAnalysisPrefDlg.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
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

procedure TAnalysisPrefDlg.FormActivate(Sender: TObject);
begin
  {$IFDEF LINUX}
  OptionsChosenGrid.DefaultRowHeight := seIntegerSpinEdit.Height;
  {$ENDIF}
  Toolbar1.Images := ImageForm.GetDialogButtonImageList;
  Toolbar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  Toolbar1.ImagesWidth := Toolbar1.ButtonWidth;
  Constraints.MinWidth := ToolBar1.Width + 20;
  InitSettings;
  SizeFormToFitOptions;
  Invalidate;
end;

procedure TAnalysisPrefDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveSettingsToFile;
  SkipTreeActionActivated := False;
end;

procedure TAnalysisPrefDlg.ChkLBxClickCheck(Sender: TObject);
var
  i, MyCount: Integer;
begin
  MyCount := 0;
  With ChkLBx do
  begin
    for i:=0 to Count-1 do
      if Checked[i] then Inc(MyCount);
    if MyCount = 0 then
    begin
      ShowMessage('Unchecking all Labels will result in an empty dataset. Please select at least one label');
      Checked[ItemIndex] := True;
      Exit;
    end;
    StoreOption(opsSelectLabelsPanel2, BoolToStr(Checked[ItemIndex], True), Items[ItemIndex]);
  end;
end;

procedure TAnalysisPrefDlg.DataSettingsDefaultsBtnClick(Sender: TObject);
begin
  GapSymbolEdit.Text := '-';
  IdenticalSymbolEdit.Text := '.';
  MissingSymbolEdit.Text := '?';
end;

procedure TAnalysisPrefDlg.CancelBtnClick(Sender: TObject);
begin
  with OptionsChosenGrid do
    DefaultColWidth := ColWidths[0];
  if (FOperation = dtdoMLInferAncSeqMyPeg) or (FOperation = dtdoMPInferAncSeqMyPeg) then
  begin
    MutationDetailViewForm.AncMLAction.Enabled := True;
    MutationDetailViewForm.AncParsimonyAction.Enabled := True;
  end;
  FPreviousOptions.Clear;
  LoadSettingsFromFile;
  ModalResult := mrCancel;
end;

procedure TAnalysisPrefDlg.FormResize(Sender: TObject);
var
  i: Integer;
begin
  try
    BeginFormUpdate;
    AdjustColWidths;
    if seIntegerSpinEdit.Visible then
      seIntegerSpinEdit.Width := SelectionWidth;
    if seFloatSpinEdit.Visible then
      seFloatSpinEdit.Width := SelectionWidth;
    if FPickListComboBx.Visible then
      FPickListComboBx.Width := SelectionWidth;
    for i:=0 to CustomPanels.Count-1 do
    begin
      if TPanel(CustomPanels.Items[i]^).Visible then
      begin
        TPanel(CustomPanels.Items[i]^).Width := SelectionWidth;
        PositionEllipsisPanel(TPanel(CustomPanels.Items[i]^));
      end;
    end;
    PositionButtonsPanel;
    OptionsChosenGrid.Invalidate;
    if Assigned(FOptionsGrid) then
      FOptionsGrid.DoResize(Sender);
  finally
    EndFormUpdate;
  end;
end;

procedure TAnalysisPrefDlg.FPickListComboBxChange(Sender: TObject);
var
  ASetting: TPickListAnalysisSetting;
  prevOption: TAnalysisSetting;
begin
  if FPicklistComboBx.ItemIndex < 0 then
    Exit;
  ASetting := TPickListAnalysisSetting(FEnabledOptions[OptionsChosenGrid.Row - 1]);
  ASetting.ItemIndex := FPickListComboBx.ItemIndex;
  prevOption := PreviousOption(ASetting.Name);
  if Assigned(prevOption) and (prevOption.SettingType = stPicklist) then
    TPickListAnalysisSetting(prevOption).ItemIndex := ASetting.ItemIndex;
  RefreshSettings;
  FPickListComboBx.Visible := False;
  {$IFDEF UNIX}
  OptionsChosenGrid.Col := 0; { for some reason, on Linux, FPickListComboBox won't surrender focus and the user cannot select another editable row}
  {$ENDIF}

  OptionsChosenGrid.Invalidate;
  ShowActiveEllipsisPanels;
  FPickListComboBx.Visible := False;
  Invalidate;
end;

procedure TAnalysisPrefDlg.HelpBtnClick(Sender: TObject);
var
  browser: TMegaBrowserFrm = nil;
begin
  if (OptionsPageControl.ActivePage = AppOptionsSheet) and Assigned(FOptionsGrid) and (FOptionsGrid.HelpUrl <> EmptyStr) then
  begin
    browser := CreateNewChromiumBrowserWindow(bmDefaultBrowser, Self, False);
    browser.GoToUrl(FOptionsGrid.HelpUrl);
    if FOptionsGrid.HelpName <> EmptyStr then
      browser.activeTab.Caption := FOptionsGrid.HelpName;
    browser.Show;
  end
  else
    ShowContextSensitiveHelp(HelpKeyword);
end;

procedure TAnalysisPrefDlg.MaxRuntimeCheckboxClick(Sender: TObject);
begin
  MaxExecutionTimeEdit.Enabled := MaxRuntimeCheckbox.Checked;
end;

procedure TAnalysisPrefDlg.NumReplicatesCBoxChange(Sender: TObject);
begin
  NumReplicatesSE.Enabled := NumReplicatesCBox.ItemIndex = 1;
  SetStateForLittleBootstrapParams;
end;

procedure TAnalysisPrefDlg.NumSamplesCBoxChange(Sender: TObject);
begin
  NumSamplesSE.Enabled := NumSamplesCBox.ItemIndex = 1;
  SetStateForLittleBootstrapParams;
end;

procedure TAnalysisPrefDlg.OkBtnClick(Sender: TObject);
var
  msg: String = '';
  s1: TAnalysisSetting = nil;
  s2: TAnalysisSetting = nil;
begin
  if Assigned(FOptionsGrid) then
  begin
    if not FOptionsGrid.ValidateSettings(msg) then
    begin
      ShowMessage(msg);
      Exit;
    end;
  end;
  if HasSetting(opsPercentSitesPerSample) then
    UpdateSubsampleUpsampleOptions;
  UpdateCurrentOptionsStringList;
  with OptionsChosenGrid do
  begin
    FDistPack.ConstructPack(FOperation, FOptionsStringlist, FIsCoding);
    FTreePack.ConstructPack(FOperation, FOptionsStringlist, FIsCoding);
    if FTreePack.DoesContain(ttUserTree) and ((CurrentOptionStr(opsTreeToUse2) = UserSpecifyFromFile) or (CurrentOptionStr(opsMLInitialTrees2) = UserSpecifyFromFile)) then
    begin
      if (Length(Trim(TreePack.TreeFile)) = 0) then
      begin
        ShowMessage('You must specify a tree file in order to continue.');
        ModalResult := mrNone;
        Exit;
      end
      else if not FileExists(TreePack.TreeFile) then
      begin
        if FOperation = dtdoMLIQTree then
        begin
        
           TreeFileNameEdit.Enabled := False;
        end
        else
          begin
            ModalResult := mrNone;
            ShowMessage('The tree file you specified does not exist.');
            Exit;
          end;
      end;
    end;
    if HasSetting(opsMLInitialTrees2) and HasSetting(opsTestPhylo2) then
    begin
      s1 := GetSetting(opsMLInitialTrees2);
      s2 := GetSetting(opsTestPhylo2);
      if s1.StringValue = InitialParsimonyTreesStr then
      begin
        if (s2.StringValue = opsSubsampleUpsample2) or (s2.StringValue = BootTestSlowStr) or (s2.StringValue = BootTestAdaptiveStr) or (s2.StringValue = BootTestAdaptiveStrFast) then
        begin
          ShowMessage('Using multiple inital trees in tandem with bootstrapping is not yet supported. Please modify the selected settings to continue the anlaysis');
          Exit;
        end;
      end;

    end;
    DefaultColWidth := ColWidths[0];
  end;
  StoreAllOptions;
  ModalResult := mrOk;
  TreeFileNameEdit.Text := EmptyStr;
end;

procedure TAnalysisPrefDlg.OptionsChosenGridDrawCell(Sender: TObject; aCol,aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  RowValue, RowName: String;
  AText: String;
  x, y: Integer;
  ASetting: TAnalysisSetting;
  aTextStyle: TTextStyle;
  temp: Integer = -1;
  scalingFactor: Double = 1;
begin
  if (not Visible) and (not FIsWorkflowSettings) then Exit;
  if not OptionsChosenGrid.Visible then Exit;
  if FIsSettingUpOptions then
    Exit;
  if not ((aRow - 1) < FEnabledOptions.Count) then
    Exit;
  if PixelsPerInch > DesignTimePPI then
    scalingFactor := PixelsPerInch/DesignTimePPI;
  aTextStyle := OptionsChosenGrid.Canvas.TextStyle;
  aTextStyle.Layout := tlCenter;
  aTextStyle.Alignment := taLeftJustify;

  with OptionsChosenGrid.Canvas do
  begin
    Font.Style := [];
    Font.Color := DEFAULT_FONT_COLOR;
    Pen.Color := GridLineColor;
    if ARow = 0 then { handle the header row}
    begin
      Brush.Color := $00aaaaaa;
      Font.Color := clBlack;
      if (aCol = 0) or (aCol = 1) then
        FillRect(aRect)
      else
      begin
        Brush.Color := OptionsChosenGrid.Color;
        FillRect(aRect);
      end;

      if aCol = 0 then
        RowValue := 'Option'
      else if aCol = 1 then
        RowValue := 'Setting'
      else
        RowValue := EmptyStr;
      y := aRect.Top + 2;
      x := aRect.Left + 2;
      aTextStyle.Alignment := taCenter;
      Brush.Style := bsClear;
      Font.Style := [];
      TextRect(aRect, x, y, RowValue, aTextStyle);
      Pen.Color := GridLineColor;
    end
    else
    begin
      ASetting := TAnalysisSetting(FEnabledOptions[aRow - 1]);
      RowName := ASetting.Name;
      RowValue :=  ASetting.StringValue;
      if (ASetting.Name = opsPickUserTree2) and FileExists(RowValue) then
        RowValue := ExtractFileName(RowValue);
      if ACol = 0 then
      begin
        Brush.Color := MyDefaultBgColor;
        if (RowName = opsOperationType1) or (RowValue = opsPlus) then
        begin
          Font.Style := [fsBold];
          temp := Font.Size;
          if temp > 0 then
            Font.Size := Font.Size - 1;
          Font.Color := DEFAULT_ACTIVE_BG_COLOR;
          Brush.Color := DEFAULT_ACTIVE_BG_COLOR2;
        end
        else if RowValue = NotApplicableStr then
          Font.Color := MyNotApplicableColor;
        FillRect(aRect);
      end
      else
      begin
        if aCol = 1 then
          Brush.Color := GetCellColor(aRow)
        else
          Brush.Color := DEFAULT_BG_COLOR;
        FillRect(aRect);
        if (RowValue = NotApplicableStr) or (RowValue = MultithreadNotAvailableStr) then
        begin
          Font.Color := MyNotApplicableColor;
        end
        else if RowValue = opsPlus then
        begin
          RowValue := EmptyStr;
        end
        else
        begin
          Font.Color := DEFAULT_FONT_COLOR;
          Font.Style := [fsItalic];
        end;
      end;

      if (not ASetting.IsReadOnly) and ASetting.IsApplicable and (aCol = 1) then
      begin
        Pen.Color := GridLineColor;
        DrawRectEdges(aRect, True, not SettingIsEditable(aRow - 1), True, True);
      end;

      if ACol = 0 then
      begin
        AText := PutBackDisallowedChars(ASetting.DisplayName);
        x := aRect.Left + 2;
        if (RowName = opsOperationType1) or (RowValue = opsPlus) then
          AText := UpperCase(AText);
      end
      else
      begin
        AText := RowValue;
        x := aRect.Left + 2;
      end;
      y := aRect.Top + 2;
      Brush.Style := bsClear;
      if aCol <> 2 then
      begin
        if (aCol = 1) then
        begin
          aTextStyle.Alignment := taLeftJustify;
          AText := '  ' + aText;
        end
        else
        begin
          aTextStyle.Alignment := taRightJustify;
          aText := aText + '        ';
        end;

        TextRect(aRect, x, y, AText, aTextStyle);
        if (aCol = 0) and (ASetting.IndentLevel <> 0) then
        begin
          if CompareValue(scalingFactor, 1.5, FP_CUTOFF) >= 0 then
          begin
            x := aRect.Right - ArrowImg32.Width - 8;
            y := aRect.Top + Round((OptionsChosenGrid.DefaultRowHeight - FArrow2.Height)/2);
            Draw(x, y, FArrow2);
          end
          else
          begin
            x := aRect.Right - ArrowImg16.Width - 4;
            y := aRect.Top + Round((OptionsChosenGrid.DefaultRowHeight - FArrow.Height)/2);
            Draw(x, y, FArrow);
          end;
        end;
      end;
    end;
    if temp <> -1 then
      Font.Size := temp;
  end;
end;

procedure TAnalysisPrefDlg.OptionsChosenGridGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
var
  aSetting: TAnalysisSetting = nil;
begin
  if aRow < 1 then
    Exit;
  aSetting := TAnalysisSetting(FEnabledOptions[aRow - 1]);
  if Assigned(aSetting) and (aSetting.Hint <> EmptyStr) then
    HintText := aSetting.Hint;
end;

procedure TAnalysisPrefDlg.OptionsChosenGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  ShowActiveEllipsisPanels;
  OptionsChosenGrid.Invalidate;
end;

procedure TAnalysisPrefDlg.OptionsChosenGridSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
var
  ASetting: TAnalysisSetting;
  PListStrings: TStringList;
begin
  if (aCol = 0) or (aCol = 2) or (aRow = 0) then { disallow editing of read-only cells}
  begin
    Editor := nil;
    Exit;
  end;

  ASetting := TAnalysisSetting(FEnabledOptions[aRow - 1]);
  if ASetting.IsReadOnly then
  begin
    Editor := nil;
    Exit;
  end;

  case ASetting.SettingType of
  stInteger:
    begin
      with (ASetting as TIntegerAnalysisSetting) do
      begin
        try
          FUpdatingSpinEdit := True;
          seIntegerSpinEdit.MinValue := Min;
          seIntegerSpinEdit.MaxValue := Max;
          seIntegerSpinEdit.Value := Value;
          seIntegerSpinEdit.Increment := Increment;
        finally
          FUpdatingSpinEdit := False;
        end;
        seIntegerSpinEdit.BoundsRect := OptionsChosenGrid.CellRect(aCol, aRow);
        Editor := seIntegerSpinEdit;
      end;
    end;
  stFloat:
    begin
      with (ASetting as TFloatAnalysisSetting) do
      begin
        try
          FUpdatingSpinEdit := True;
          seFloatSpinEdit.MinValue := Min;
          seFloatSpinEdit.MaxValue := Max;
          seFloatSpinEdit.Value := Value;
          seFloatSpinEdit.Increment := Increment;
        finally
          FUpdatingSpinEdit := False;
        end;
        seFloatSpinEdit.BoundsRect := OptionsChosenGrid.CellRect(aCol, aRow);
        Editor := seFloatSpinEdit;
      end;
    end;
  stString:
    begin
      with (ASetting as TStringAnalysisSetting) do
      begin
        if (Value <> EmptyStr) and (FileExists(Value)) then
          TreeFileNameEdit.FileName := Value;
        TreeFileNameEdit.BoundsRect := OptionsChosenGrid.CellRect(aCol, aRow);
        Editor := TreeFileNameEdit;
      end;
    end;
  stPicklist:
    begin
      with (ASetting as TPickListAnalysisSetting) do
      begin
        FPickListComboBx.Items.Clear;
        PListStrings := PickListStrings;
        FPickListComboBx.Items.AddStrings(PListStrings);
        PListStrings.Free;
        FPickListComboBx.BoundsRect := OptionsChosenGrid.CellRect(aCol,aRow);
        FPickListComboBx.ItemIndex := ItemIndex;
        Editor := FPickListComboBx;
      end;
    end;
  end;
  FEditorRow := aRow;
end;

procedure TAnalysisPrefDlg.OptionsChosenGridTopleftChanged(Sender: TObject);
begin
  ShowActiveEllipsisPanels;
  if FPickListComboBx.Visible then
    FPickListComboBx.Visible := False;
  if seIntegerSpinEdit.Visible then
    seIntegerSpinEdit.Visible := False;
  if seFloatSpinEdit.Visible then
    seFloatSpinEdit.Visible := False;
end;

procedure TAnalysisPrefDlg.PercentSitesCBoxChange(Sender: TObject);
begin
  PercentSitesSE.Enabled := PercentSitesCBox.ItemIndex = 1;
  SetStateForLittleBootstrapParams;
  if FOperation = dtdoMLModelTest then
    SetStateForModelSelectionParams;
end;

procedure TAnalysisPrefDlg.ProtoLabelledSitesComboChange(Sender: TObject);
begin
  if ProtoLabelledSitesCombo.ItemIndex = 1 then
    ProtoLabelsToUseEdit.Enabled := True
  else
    ProtoLabelsToUseEdit.Enabled := False;
end;

procedure TAnalysisPrefDlg.SaveSettingsBtnClick(Sender: TObject);
var
  MaoStrings: TStringList = nil;
  MySettingsFileName: String = '';
  AMsg: String = '';
begin
  if HasSetting(opsPercentSitesPerSample) then
    UpdateSubsampleUpsampleOptions;
  AMsg := ExamineThreeSymbols(GapSymbolEdit.Text[1], MissingSymbolEdit.Text[1], IdenticalSymbolEdit.Text[1]);
  if Trim(AMsg) <> EmptyStr then
  begin
    OptionsPageControl.ActivePage := DataSettingsPage;
    ShowMessage(AMsg);
    ModalResult := mrRetry;
    Exit;
  end
  else if not ValidateLabelledSitesOption then
  begin
    ModalResult := mrRetry;
    Exit;
  end
  else
  begin
    try
      try
        if ProcessPackTemp.IsWorkflowPack then
        begin
          MaoStrings := generateMAOFile;
          ProcessPackTemp.TextualSettingsList.Assign(MaoStrings);
          CancelBtnClick(Sender);
        end
        else if NeedsAppOptions then
        begin
          StoreAllOptions;
          ModalResult := mrIgnore;
        end
        else if AppOptionsSheet.TabVisible then
        begin
          ModalResult := mrOk;
        end
        else
        begin
          if GetMaoFileSaveLocation(MySettingsFileName) then  //Get where to save the Analysis Preferences File
          begin
            MaoStrings := generateMAOFile;
            MaoStrings.SaveToFile(MySettingsFileName);
            CancelBtnClick(Sender); // We want to close the APD after saving settings.
            StoreAllOptions;
          end
          else
            CancelBtnClick(Sender);
        end;
      except
        on E : Exception do
          Exception.Create('Unable to save Analysis Preferences file Error: ' + E.Message);
      end;
    finally
      if Assigned(MaoStrings) then
        MaoStrings.Free;
    end;
  end;
end;

procedure TAnalysisPrefDlg.seFloatSpinEditChange(Sender: TObject);
begin
  if FUpdatingSpinEdit then
    Exit;
  seFloatSpinEditExit(Sender);
end;

procedure TAnalysisPrefDlg.seFloatSpinEditEnter(Sender: TObject);
var
  ASetting: TFloatAnalysisSetting;
begin
  if (OptionsChosenGrid.Row - 1) >= FEnabledOptions.Count then
    Exit;
  try
    FUpdatingSpinEdit := True;
    ASetting := TFloatAnalysisSetting(FEnabledOptions[OptionsChosenGrid.Row - 1]);
    if Assigned(ASetting) then
      seFloatSpinEdit.Value := ASetting.Value;
  finally
    FUpdatingSpinEdit := False;
  end;
end;

procedure TAnalysisPrefDlg.seFloatSpinEditExit(Sender: TObject);
var
  ASetting: TFloatAnalysisSetting = nil;
  prevOption: TAnalysisSetting = nil;
  temp: Integer;
  floatSetting: TAnalysisSetting = nil;
  intSetting2: TIntegerAnalysisSetting = nil;
begin
  if OptionsChosenGrid.Row <> FEditorRow then { bug fix for macOS where this procedure fires out of order and at weird times}
    Exit;
  if (OptionsChosenGrid.Row - 1) >= FEnabledOptions.Count then
    Exit;
  ASetting := TFloatAnalysisSetting(FEnabledOptions[OptionsChosenGrid.Row - 1]);
  ASetting.Value := seFloatSpinEdit.Value;
  prevOption := PreviousOption(ASetting.Name);
  if Assigned(prevOption) and (prevOption.SettingType = stFloat) then
    TFloatAnalysisSetting(prevOption).Value := ASetting.Value;
  if (ASetting.Name = opsBootstrapSampleSizeFactor) and (not IsPrototyper) then
  begin
    floatSetting := GetSetting(opsPercentSitesPerSample);
    if Assigned(floatSetting) then
    begin
      temp := ceil(Power(FNumSitesBeforeSubsetting, seFloatSpinEdit.Value));
      TFloatAnalysisSetting(floatSetting).Value := temp;
      intSetting2 := TIntegerAnalysisSetting(GetSetting(opsBootstrapMaxSitesPerSample));
      if intSetting2.Value < temp then
        intSetting2.Value := temp;
      OptionsChosenGrid.Invalidate;
    end;
  end;
end;

procedure TAnalysisPrefDlg.seIntegerSpinEditChange(Sender: TObject);
begin
  if FUpdatingSpinEdit then
    Exit;
  seIntegerSpinEditExit(Sender);
end;

procedure TAnalysisPrefDlg.seIntegerSpinEditEnter(Sender: TObject);
var
  ASetting: TIntegerAnalysisSetting;
begin
  if (OptionsChosenGrid.Row - 1) >= FEnabledOptions.Count then
    Exit;
  try
    FUpdatingSpinEdit := True;
    ASetting := TIntegerAnalysisSetting(FEnabledOptions[OptionsChosenGrid.Row - 1]);
    if Assigned(ASetting) then
      seIntegerSpinEdit.Value := ASetting.Value;
  finally
    FUpdatingSpinEdit := False;
  end;
end;

procedure TAnalysisPrefDlg.seIntegerSpinEditExit(Sender: TObject);
var
  ASetting: TIntegerAnalysisSetting = nil;
  prevOption: TAnalysisSetting;
begin
  if OptionsChosenGrid.Row <> FEditorRow then { bug fix for macOS where this procedure fires out of order and at weird times}
    Exit;
  if (OptionsChosenGrid.Row - 1) >= FEnabledOptions.Count then
    Exit;
  ASetting := TIntegerAnalysisSetting(FEnabledOptions[OptionsChosenGrid.Row - 1]);
  ASetting.Value := seIntegerSpinEdit.Value;
  prevOption := PreviousOption(ASetting.Name);
  if Assigned(prevOption) and (prevOption.SettingType = stInteger) then
    TIntegerAnalysisSetting(prevOption).Value := ASetting.Value;
end;

procedure TAnalysisPrefDlg.CodPosChkBxClick(Sender: TObject);
begin
  if not Active then exit;
  if not FIsCoding then exit;

  if IncludeCodonPos = [] then
  begin
    ModalResult := mrNone;
    MessageDlg('Please select at least one type of position.', mtError, [mbOK], 0);
    with Sender as TCheckBox do
      Checked := True;
  end;

  with Sender as TCheckBox do
    StoreOption(opsCodonPosPanel2, BoolToStr(Checked, True), Caption)
end;

procedure TAnalysisPrefDlg.TreeFileNameEditAcceptFileName(Sender: TObject; var Value: String);
var
  aSetting: TStringAnalysisSetting;
begin
  if not FileExists(Value) then
  begin
    ShowMessage('The specified tree file was not found');
    Exit;
  end;
  ASetting := TStringAnalysisSetting(FEnabledOptions[OptionsChosenGrid.Row - 1]);
  ASetting.Value := Value;
  StoreOption(aSetting.Name, Value);
  TreePack.TreeFile := Value;
end;

procedure TAnalysisPrefDlg.TreeFileNameEditExit(Sender: TObject);
var
  aSetting: TStringAnalysisSetting;
begin
  if OptionsChosenGrid.Row <> FEditorRow then { bug fix for macOS where this procedure fires out of order and at weird times}
    Exit;
  if FileExists(TreeFileNameEdit.Text) then
  begin
    ASetting := TStringAnalysisSetting(FEnabledOptions[OptionsChosenGrid.Row - 1]);
    ASetting.Value := TreeFileNameEdit.Text;
    StoreOption(ASetting.Name, TreeFileNameEdit.Text);
    TreePack.TreeFile := TreeFileNameEdit.Text;
  end;
end;

procedure TAnalysisPrefDlg.UpdateCurrentEnabledOptions;
var
  i: Integer = -1;
  aSetting: TAnalysisSetting = nil;
begin
  FEnabledOptions.Clear;
  if FCurrentOptions.Count > 0 then
    for i := 0 to FCurrentOptions.Count - 1 do
    begin
      aSetting := TAnalysisSetting(FCurrentOptions[i]);
      if aSetting.IsVisible and aSetting.IsApplicable then
        FEnabledOptions.Add(aSetting);
    end;
  OptionsChosenGrid.RowCount := FEnabledOptions.Count + 1;
  ResizeColumns;
  AdjustColWidths;
  OptionsChosenGrid.Invalidate;
end;

procedure TAnalysisPrefDlg.InitSettings;
{$IFDEF DEBUG}
var
  aSetting: TAnalysisSetting = nil;
{$ENDIF}
begin
  try
    try
      FIsSettingUpOptions := True;
      FreeCurrentOptions;
      TreeFileNameEdit.Visible := False;
      FPickListComboBx.Visible := False;
      seIntegerSpinEdit.Visible := False;
      seFloatSpinEdit.Visible := False;
      PrevOptSheet.TabVisible := False; //IsDeveloper;
      FDistPack.Clear;
      IsMolSeq := (FIsNuc or FIsAmino);
      IsPreventChangeEventCount := 1;
      AddOperationTypeRows;
      AddAnalysisScopeRows; // [pairs, groups, etc.]
      AddPhyloAnalysisMethodRows; // [ML/MP/NJ/and bootstrapping]
      AddDistAnalysisOptionsRows; // variance estimation system

      //Remember that the picklists will change based on the model selected.
      //It should be done at the end of this system
      AddSubsModelRows;
      if IsMolSeq then
        AddDataSubsetRows;
      AddTreeSearchRows;
      AddBeamOptions;
      AddSystemResourceUsageRows;

      // Apply Initial Settings
      UpdateAnalysisScopeRows; //Their initial value will affect many following options (gp vs. nongroup)
      UpdatePhyloAnalysisMethodRows; //sets up the method [ML/MP/NJ/bootstrapping]
      UpdateDistAnalysisOptionsRows;
      UpdateDataSubSetRows;
      UpdateModelRows(opsNucSynAminoType2); // this sets up rows and their initial values
      UpdateTreeSearchRows;
      UpdateClockRows;
      UpdateBeamOptions;
      UpdateSystemResourceUsageRows;
      ShowActiveEllipsisPanels;

      AssignHelpContexts;
      //Now focus on the calculation
      //setup operation type in summary datatype
      IsPreventChangeEventCount := 0;
      CurSpinEditRow := -1;

      if isPrototyper then
        AddDataSettingsOptions;
      DataSettingsPage.TabVisible := (isPrototyper and (FOperation <> dtdoRtdtBLens) and (FOperation <> dtdoRelTimeBLens) and (FOperation <> dtdoCorrTestBlens));
      //SelectLabelsPanel.Visible := (ChkLBx.Count > 0);
      if not SelectLabelsPanel.Visible then
        SelectLabelsPanel.Left := Self.left - 30;
      if FOperation = dtdoSelectionZTest then
      begin
        UpdateCurrentOptionsStringList;
        FDistPack.ConstructPack(FOperation, FOptionsStringList, FIsCoding);
        UpdateVarEstimationRow;
      end;
      OptionsChosenGrid.ClearSelections;
      OptionsChosenGrid.Selection := Rect(0, 1, 0, 1);
      Width := Width - 1; // this is a hack because TDrawGrid does funny autoscrolling which can hide the first column
      Width := Width + 1;
      UpdateCurrentEnabledOptions;
    except
      on E: Exception do
        ShowMessage('Oh no! An error  occurred when setting up the options dialog: ' + E.Message);
    end;
  finally
    FIsSettingUpOptions := False;
    OptionsChosenGrid.Invalidate;
    Application.ProcessMessages;
  end;
end;

function TAnalysisPrefDlg.ScrollbarVisible: Boolean;
begin
  if OptionsPageControl.ActivePage = OptionsSummaryTab then
  begin
    if (OptionsChosenGrid.Width - OptionsChosenGrid.ClientWidth) < 5 then
      Result := False
    else
      Result := True;
  end
  else
    Result := False; { not checking for other tab sheets because it is not currently needed}
end;

procedure TAnalysisPrefDlg.UpdateMaxSitesPerSample;
var
  floatSetting2: TFloatAnalysisSetting = nil;
  intSetting2: TIntegerAnalysisSetting = nil;
  floatSetting: TFloatAnalysisSetting = nil;
  sampleSizeFactor: Double;
  temp: Integer;
begin
  if (not IsPrototyper) and HasSetting(opsBootstrapSampleSizeFactor) then
  begin
    floatSetting := TFloatAnalysisSetting(GetSetting(opsBootstrapSampleSizeFactor));
    if Assigned(floatSetting) then
    begin
      sampleSizeFactor := floatSetting.Value;
      floatSetting2 := TFloatAnalysisSetting(GetSetting(opsPercentSitesPerSample));
      if Assigned(floatSetting) then
      begin
        temp := ceil(Power(FNumSitesBeforeSubsetting, sampleSizeFactor));
        floatSetting2.Value := temp/FNumSitesBeforeSubsetting*100;
        intSetting2 := TIntegerAnalysisSetting(GetSetting(opsBootstrapMaxSitesPerSample));
        if Assigned(intSetting2) then
        begin
          if intSetting2.Value < floatSetting.Value then
            intSetting2.Value := round(floatSetting.Value);
        end;
        OptionsChosenGrid.Invalidate;
      end;
    end;
  end;
end;

function TAnalysisPrefDlg.SubstitutionTypeStr: String;
begin
  Result := EmptyStr;
  if HasSetting(opsNucSynAminoType2) then
    Result := LowerCase(GetSetting(opsNucSynAminoType2).StringValue);
end;

function TAnalysisPrefDlg.GetMaxPercentSnvDiffForIdentSeqs: Double;
var
  temp: String;
  value: Double;
begin
  temp := GetSetting(opsImputeCutoff2).StringValue;
  if TryStrToFloat(temp, value) then
    Result := value
  else
    raise Exception.Create('failed to retrieve MaxPercentSnvDiffForIdentSeqs value');
end;

procedure TAnalysisPrefDlg.SetNeedsAppOptions(AValue: Boolean);
begin
  if FNeedsAppOptions = AValue then Exit;
  FNeedsAppOptions := AValue;
  SaveSettingsAction.ImageIndex := 3;
end;

function TAnalysisPrefDlg.GetIsBootstrapSubSampling: Boolean;
begin
  Result := False;
  if IsCurrentSetting(opsTestPhylo2, opsSubsampleUpsample2) then
    Result := True;
end;

procedure TAnalysisPrefDlg.PositionButtonsPanel;
const
  SPACING = 7;
var
  space: Integer;
begin
  if (MegaRunMode = mrmGui) and (not FNeedsAppOptions) then
    OkButton.Action := OkAction
  else
    OkButton.Action := SaveSettingsAction;

  if ButtonsPanel.Width > ToolBar1.Width then
    ToolBar1.Left := Round((ButtonsPanel.Width - ToolBar1.Width)/2);
end;

function TAnalysisPrefDlg.ReltimeLsShouldDisableBootstrapReps: Boolean;
begin
  Result := (IsDeveloper and
             ((FOperation = dtdoReltimeLs) or(FOperation = dtdoRtdtLS)) and
             ((IsCurrentSetting(opsEstimateVar2, NoneStr)) or
              (IsCurrentSetting(opsEstimateVar2, RateVarStr)) or
              (IsCurrentSetting(opsEstimateVar2, SampleVarStr)) or
              (IsCurrentSetting(opsEstimateVar2, RateAndSampleVarStr))))
end;

function TAnalysisPrefDlg.GetMaxRateRatio: Extended;
var
  temp: String;
  value: Extended;
begin
  temp := GetSetting(opsMaxRateRatio2).StringValue;
  if TryStrToFloat(temp, value) then
    result := value
  else
    raise Exception.Create('Invalid value for ' + opsMaxRateRatio2 + '. Must be a numeric value');
end;

function TAnalysisPrefDlg.GetSetting(AName: String): TAnalysisSetting;
var
  i: Integer;
begin
  Result := nil;
  if FCurrentOptions.Count = 0 then
    Exit;
  for i := 0 to FCurrentOptions.Count - 1 do
  begin
    if AName = TAnalysisSetting(FCurrentOptions[i]).Name then
    begin
      Result := TAnalysisSetting(FCurrentOptions[i]);
      Exit;
    end;
  end;
end;

procedure TAnalysisPrefDlg.RefreshSettings;
var
  ACurRow: Integer = -1;
  AProcRow: Integer = -1;
  sRKey, sRValue: String;
  ASetting: TAnalysisSetting = nil;
begin
  ACurRow := OptionsChosenGrid.Row;
  if ACurRow < 1 then { nothing selected yet}
    Exit;

  sRKey := GetGridText(0, ACurRow);
  sRValue := GetGridText(1,ACurRow);

  if (Length(sRkey) = 0) or (not HasSetting(sRKey, AProcRow)) then
    Exit;
  try
    try
      BeginFormUpdate;
      if sRKey = opsGapMissingHandle2 then  //INDEL handling
      begin
        if HasSetting(opsSiteCoverage2, AProcRow) then
        begin
          ASetting := GetSetting(opsSiteCoverage2);
          if sRValue <> PartialDelStr then
          begin
            ASetting.IsReadOnly := True;
            ASetting.IsApplicable := False;
          end
          else
          begin
            ASetting.IsReadOnly := False;
            ASetting.IsApplicable := True;
          end;
        end;
      end
      else if sRKey = opsRatesAmongSites2 then // Rate variation
      begin
        if HasSetting(opsGammaPara2, AProcRow) then
        begin
          ASetting := GetSetting(opsGammaPara2);
          if (sRValue = UniformRatePickStr) or (sRValue = InvariantPickStr) then
          begin
            ASetting.IsReadOnly := True;
            ASetting.IsApplicable := False;
          end
          else
          begin
            ASetting.IsReadOnly := False;
            ASetting.IsApplicable := True;
          end;
          with (ASetting as TFloatAnalysisSetting) do
          begin
            if (Value < Min) or (Value > Max) then
            begin
              if HasDefaultOption(Name) then
                ASetting.SetValueFromStr(GetDefaultOptions(Name))
              else
                Value := Min;
            end;
          end;
        end;

        if HasSetting(opsGammaCats2, AProcRow) then
        begin
          ASetting := GetSetting(opsGammaCats2);
          if (sRValue = UniformRatePickStr) or (sRValue = InvariantPickStr) then
          begin
            ASetting.IsReadOnly := True;
            ASetting.IsApplicable := False;
          end
          else
          begin
            ASetting.IsReadOnly := False;
            ASetting.IsApplicable := True;
          end;
        end;
      end
      //else if sRKey = opsPatternAmongLineages2 then // Lineage patterns
      //begin
      //  //if PickListName <> EmptyStr then
      //  //  StoreOption(PickListName, sRValue);
      //end
      //else if sRKey = opsRecodeBases2 then // Recoding of bases
      //begin
      //  if PickListName <> EmptyStr then
      //    StoreOption(opsRecodeBases2, sRValue);
      //end
      else if (sRKey = opsTestPhylo2) or (sRKey = opsEstimateVar2) or (sRKey = opsApproachType2) then // TEST phylogeny
      begin
        if HasSetting(opsBootReps2, AProcRow) or HasSetting(opsBootstrapSubSamples) then
        begin

          ASetting := GetSetting(opsBootReps2);
          if (IsCurrentSetting(opsTestPhylo2, NoneStr)) or
             (IsCurrentSetting(opsTestPhylo2, opsSubsampleUpsample2)) or
             (IsCurrentSetting(opsEstimateVar2, NoneStr)) or
             CurrentOptionIsInArray(opsTestPhylo2, [BootTestAdaptiveStr, BootTestAdaptiveStrFast, AnalTestStr]) or
             ReltimeLsShouldDisableBootstrapReps then
          begin
            if Assigned(ASetting) then
            begin
              ASetting.IsReadOnly := True;
              ASetting.IsApplicable := False;
            end;
          end
          else
          begin
            if Assigned(ASetting) then
            begin
              ASetting.IsReadOnly := False;
              ASetting.IsApplicable := True;
            end;
          end;

          if HasSetting(opsMLNumThreads) then
          begin
            aSetting := GetSetting(opsMLNumThreads);
            if IsCurrentSetting(opsTestPhylo2, NoneStr) and (not IsMLCompute(FOperation)) then
            begin
              ASetting.IsReadOnly := True;
              ASetting.IsApplicable := False;
            end
            else
            begin
              ASetting.IsReadOnly := False;
              ASetting.IsApplicable := True;
            end;
            if IsCurrentSetting(opsTestPhylo2, opsSubsampleUpsample2) or IsCurrentSetting(opsEstimateVar2, opsSubsampleUpsample2) then
              SetStateForLittleBootstrapParams
            else
            begin
              if HasSetting(opsBootstrapSubSamples) or HasSetting(opsBootstrapPrecisionThreshold) then
                SetStateForLittleBootstrapParams;
            end;
            if HasSetting(opsSystemResourceUsage) then
              GetSetting(opsSystemResourceUsage).IsApplicable := ASetting.IsApplicable;
          end
          else if FOperation in [dtdoMLGammRates, dtdoMLComputePattern, dtdoMLTsTvBias] then
            SetStateForLittleBootstrapParams;
        end
        else if (sRKey = opsApproachType2) and (FOperation in [dtdoMLGammRates, dtdoMLTsTvBias]) then
          SetStateForLittleBootstrapParams;
        if HasSetting(opsEstimateVar2) and (IsCurrentSetting(opsEstimateVar2, NoneStr) or IsCurrentSetting(opsEstimateVar2, AnalTestStr)) then
        begin
          if HasSetting(opsBootReps2) then
            GetSetting(opsBootReps2).SetState(True, False);
        end;
      end
      else if sRKey = opsNucSynAminoType2 then  // MODEL Parameters and further
      begin
        UpdateDataSubsetRows;
        UpdateGeneticCodeRow;
        UpdateModelRows(opsNucSynAminoType2);
        UpdateCurrentOptionsStringList;
        FDistPack.ConstructPack(FOperation, FOptionsStringList, FIsCoding);
        UpdateModelDependentRows;
        ShowActiveEllipsisPanels;
      end
      else if sRKey = opsSubsModel2 then
      begin
        UpdateModelRows(opsSubsModel2);
        UpdateCurrentOptionsStringList;
        FDistPack.ConstructPack(FOperation, FOptionsStringList, FIsCoding);
        UpdateModelDependentRows;
        ShowActiveEllipsisPanels;
      end
      else if sRKey = opsSubsToInclude2 then
      begin
        UpdateModelRows(opsSubsToInclude2);
        UpdateCurrentOptionsStringList;
        FDistPack.ConstructPack(FOperation, FOptionsStringList, FIsCoding);
        UpdateModelDependentRows;
        ShowActiveEllipsisPanels;
      end
      else if (sRKey = opsMESearchMethod2) or
              (sRKey = opsMPSearchMethod2) or
              (sRKey = opsMLSearchMethod2) then
      begin
        UpdateTreeSearchRows;
      end
      else if (sRKey = opsMLInitialTrees2) then
      begin
        if sRValue = UserSpecifyFromFile then
        begin
          if HasSetting(opsPickStartTree2) then
          begin
            GetSetting(opsPickStartTree2).IsApplicable := True;
            GetSetting(opsPickStartTree2).IsReadOnly := False;
          end;
        end;
        UpdateMLInitialTreeRow;
      end
      else if sRKey = opsTreeToUse2 then
      begin
        UpdateAnalysisScopeRows;
      end
      else if sRKey = opsLabelledSites2 then // Labelled sites
      begin
        if HasSetting(opsSelectLabelsPanel2, AProcRow) then
        begin
          ASetting := GetSetting(opsSelectLabelsPanel2);
          if sRValue <> OnlyLabeledSitesStr then
          begin
            ASetting.IsReadOnly := True;
            SelectLabelsPanel.Hide;
            UpdateRow(opsSelectLabelsPanel2, NotApplicableStr);
            ChkLBx.Enabled := False;
          end
          else
          begin
            ASetting.IsReadOnly := False;
            UpdateRow(opsSelectLabelsPanel2, EmptyStr);
            PositionEllipsisPanel(SelectLabelsPanel);
            SelectLabelsPanel.Visible := True;
            ChkLBx.Enabled := True;
          end;
        end;
      end
      else if sRKey = opsScope2 then // The var est row can be dependent upon scope in the case of the Z-Test so we must update it when scope changes.
      begin
        UpdateCurrentOptionsStringList;
        FDistPack.ConstructPack(FOperation, FOptionsStringList, FIsCoding);
        UpdateVarEstimationRow;
      end
      else if sRKey = opsAssumeMolClock2 then
      begin
        UpdateClockRows;
      end;
      if FOperation = dtdoMLModelTest then
        SetStateForModelSelectionParams;
      UpdateCurrentEnabledOptions;
    except
      on E:Exception do
        ShowMessage('Oh no! An error occurred when updating settings: ' + E.Message);
    end;
  finally
    EndFormUpdate;
    Invalidate;
  end;
end;

procedure TAnalysisPrefDlg.UpdateSubsampleUpsampleOptions;
var
  intSetting: TIntegerAnalysisSetting = nil;
  floatSetting: TFloatAnalysisSetting = nil;
  picklistSetting: TPickListAnalysisSetting = nil;
begin
  if HasSetting(opsBootstrapSubSamples) then
  begin
    intSetting := TIntegerAnalysisSetting(GetSetting(opsBootstrapSubSamples));
    intSetting.Value := NumSamplesSE.Value;
  end;

  if HasSetting(opsBootstrapRepsPerSample) then
  begin
    intSetting := TIntegerAnalysisSetting(GetSetting(opsBootstrapRepsPerSample));
    intSetting.Value := NumReplicatesSE.Value;
  end;

  if HasSetting(opsPercentSitesPerSample) then
  begin
    floatSetting := TFloatAnalysisSetting(GetSetting(opsPercentSitesPerSample));
    floatSetting.Value := PercentSitesSE.Value;
  end;
end;

procedure TAnalysisPrefDlg.UpdateCurrentOptionsStringList;
var
  i: Integer;
begin
  FOptionsStringList.Clear;
  if FCurrentOptions.Count = 0 then
    Exit;
  for i := 0 to FCurrentOptions.Count - 1 do
    FOptionsStringList.Add(TAnalysisSetting(FCurrentOptions[i]).NameValueString);
end;

function TAnalysisPrefDlg.SelectedGeneticCode: String;
var
  TableName: String;
  ARow: Integer = -1;
  GenCodeDlg: TSelectGeneticCodeDlg;
begin
  GenCodeDlg := nil;
  Result := EmptyStr;

  if HasSetting(opsGeneticCodeTable2, ARow) and GetSetting(opsGeneticCodeTable2).IsApplicable then
  begin
    try
      TableName := GetSetting(opsGeneticCodeTable2).StringValue;
      GenCodeDlg := TSelectGeneticCodeDlg.Create(nil);
      GenCodeDlg.CodeTableName := TableName;
      Result := GenCodeDlg.CodeTable;
    finally
      if Assigned(GenCodeDlg) then
        GenCodeDlg.Free;
    end;
  end
  else if HasSetting(opsGeneticCodeTable2, ARow) then
    Result := NotApplicableStr;
end;

function TAnalysisPrefDlg.GetCellColor(aRow: Integer): TColor;
var
  ASetting: TAnalysisSetting;
begin
  Assert(aRow <> 0); { should not call this for the header as it gets a gradient fill}


  ASetting := TAnalysisSetting(FEnabledOptions[aRow - 1]);
  if ASetting.StringValue = opsPlus then
    Result := DEFAULT_ACTIVE_BG_COLOR2
  else if (not ASetting.IsReadOnly) then
    Result := clWhite
  else
    Result := MyDefaultBgColor;
end;

function TAnalysisPrefDlg.GetIndentLevelForName(AName: String): Integer;
begin
  Result := 1; { most settings will be indented 1 level}

  { these are the section headers}
  if (AName = opsClockSettings1) or
     (AName = opsChooseThreeTaxaForClocks1) or
     (AName = opsDataSubset1) or
     (AName = opsDistanceEstimation1) or
     (AName = opsEstimateVar1) or
     (AName = opsModelInfo1) or
     (AName = opsRatesPatterns1) or
     (AName = opsTestPhylo1) or
     (AName = opsTreeMethod1) or
     (AName = opsOperationType1) or
     (AName = opsBeamSettings) or
     (AName = opsApproach1) or
     (AName = opsAdvancedSettings) or
     (AName = opsSystemResourceUsage) then
     begin
       Result := 0;
       Exit;
     end;

  { these are the sub-options, e.g if Rates Among Sites is set the Gamma Distributed, then No. of Discrete Gamma Categories would be a sub-option }
  if (AName = opsGeneticCodeTable2) or
     (AName = opsBootReps2) or
     (AName = opsGammaCats2) or
     (AName = opsSiteCoverage2) or
     (AName = opsPickStartTree2) or
     (AName = opsGammaPara2) or
     (AName = opsSelectLabelsPanel2) or
     (AName = opsMaxRateRatio2) or
     (AName = opsPickUserTree2) then
  begin
    Result := 2;
    Exit;
  end;
end;

procedure TAnalysisPrefDlg.DrawRectEdges(ARect: TRect; DoLeft, DoTop, DoRight, DoBottom: Boolean);
begin
  if (not Visible) and (not FIsWorkflowSettings) then Exit;
  with OptionsChosenGrid.Canvas do
  begin
    if DoLeft then
      Line(aRect.Left, aRect.Top, aRect.Left, aRect.Bottom);
    if DoTop then
      Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
    if DoRight then
      Line(aRect.Right - 1, aRect.Top, aRect.Right - 1, aRect.Bottom);
    if DoBottom then
      Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
  end;
end;

function TAnalysisPrefDlg.GetGridText(const aCol: Integer; const aRow: Integer): String;
var
  ASetting: TAnalysisSetting;
begin
  Assert((aRow < OptionsChosenGrid.RowCount) and ((aRow - 1) < FEnabledOptions.Count));
  ASetting := TAnalysisSetting(FEnabledOptions[aRow - 1]);
  case aCol of
    0: Result := ASetting.Name;
    1: Result := ASetting.StringValue;
  end;
end;

procedure TAnalysisPrefDlg.FreeOptions;
begin
  FEnabledOptions.Clear;
  FreeCurrentOptions;
  FreePreviousOptions;
end;

procedure TAnalysisPrefDlg.FreeCurrentOptions;
var
  i: Integer;
begin
  FEnabledOptions.Clear;
  if FCurrentOptions.Count > 0 then
  begin
    for i := 0 to FCurrentOptions.Count - 1 do
    begin
      case TAnalysisSetting(FCurrentOptions[i]).SettingType of
      stInteger: TIntegerAnalysisSetting(FCurrentOptions[i]).Free;
      stFloat: TFloatAnalysisSetting(FCurrentOptions[i]).Free;
      stString: TStringAnalysisSetting(FCurrentOptions[i]).Free;
      stPicklist: TPickListAnalysisSetting(FCurrentOptions[i]).Free;
      stPickListValue: TPickListValueSetting(FCurrentOptions[i]).Free;
      end;
    end;
    FCurrentOptions.Clear;
  end;
end;

procedure TAnalysisPrefDlg.FreePreviousOptions;
var
  i: Integer;
begin
  if FPreviousOptions.Count > 0 then
  begin
    for i := 0 to FPreviousOptions.Count - 1 do
    begin
      case TAnalysisSetting(FPreviousOptions[i]).SettingType of
      stInteger: TIntegerAnalysisSetting(FPreviousOptions[i]).Free;
      stFloat: TFloatAnalysisSetting(FPreviousOptions[i]).Free;
      stString: TStringAnalysisSetting(FPreviousOptions[i]).Free;
      stPicklist: TPickListAnalysisSetting(FPreviousOptions[i]).Free;
      stPickListValue: TPickListValueSetting(FPreviousOptions[i]).Free;
      end;
    end;
    FPreviousOptions.Clear;
  end;
end;

function TAnalysisPrefDlg.generateMAOFile(): TStringList;
begin
  Result := TStringList.Create;
  GenerateMAOFile(Result);
end;

function TAnalysisPrefDlg.generateMAOFile(var aList: TStringList): Boolean;
var
  i: integer;
  ARow: Integer = -1;
  MyDataType: TSnTokenCode;
  ASetting: TAnalysisSetting;
  FormatLen: Integer;
  FLStr: String; { FormatLen as a String}
  CodonStr: String;
  tempInt: Integer;
begin
  Result := True;
  FormatLen := GetMaoFormatLen;
  FLStr := '%-' + IntToStr(FormatLen) + 's = %-' + IntToStr(FormatLen) + 's';
  if not Assigned(ProcessPackTemp) then
    ProcessPackTemp := TProcessPack.Create; // in some cases the process pack is NOT created.

  aList.Clear;
  AddMaoFileHeader(aList, FLStr);
  aList.Add('[ DataSettings ]');
  MyDataType := MegaForm.DataType;
  aList.Add(Format(FLStr, [DataTypeStr, GetEnumName(TypeInfo(TSnTokenCode), Integer(MyDataType))]));
  if MyDataType = snNucleotide then
    aList.Add(Format(FLStr, [ContainsCodingNucStr, BoolToStr(MegaForm.containsCodingNuc, true)]));
  if MyDataType <> snTree then
  begin
    aList.Add(Format(FLStr, ['MissingBaseSymbol', MissingSymbolEdit.Text]));
    aList.Add(Format(FLStr, ['IdenticalBaseSymbol', IdenticalSymbolEdit.Text]));
    aList.Add(Format(FLStr, ['GapSymbol', GapSymbolEdit.Text]));
    aList.Add(Format(FLStr, [opsLabelledSites2, ProtoLabelledSitesCombo.Items[ProtoLabelledSitesCombo.ItemIndex]]));
    aList.Add(Format(FLStr, [opsSelectLabelsPanel2, Trim(ProtoLabelsToUseEdit.Text)]));
  end;

  aList.Add('[ ProcessTypes ]');

  for i := 0 to ProcessPackTemp.Count - 1 do
    aList.Add(Format(FLStr, [GetEnumName(TypeInfo(TProcessType), integer(ProcessPackTemp.ProcessTypes[i])), 'true']));

  aList.Add('[ AnalysisSettings ]');
  aList.Add(Format(FLStr, ['Analysis', OperationTypeString(FOperation)]));
  for i := 0 to FEnabledOptions.Count - 1 do
  begin
    ASetting := TAnalysisSetting(FEnabledOptions[i]);
    if not ASetting.IsVisible then
      continue;
    if ASetting.Name = opsCodonPosPanel2 then { a special case}
    begin
      CodonStr := EmptyStr;
      if (meg1stSite   in IncludeCodonPos) then  CodonStr := CodonStr + '1st, ';
      if (meg2ndSite   in IncludeCodonPos) then  CodonStr := CodonStr + '2nd, ';
      if (meg3rdSite   in IncludeCodonPos) then  CodonStr := CodonStr + '3rd, ';
      if (megNoncoding in IncludeCodonPos) then  CodonStr := CodonStr + 'Non-Coding';
      aList.add(Format(FLStr, [opsCodonPosPanel2,  CodonStr]));
    end
    else if ASetting.Name = opsBootstrapSubSamples then
    begin
      if NumSamplesCBox.ItemIndex = NumSamplesCBox.Items.IndexOf(AdaptiveStr) then
        aList.Add(Format(FLStr, [opsBootstrapSubSamples, AdaptiveStr]))
      else
        aList.Add(ASetting.MaoString(FLStr));
    end
    else if ASetting.Name = opsBootstrapRepsPerSample then
    begin
      if NumReplicatesCBox.ItemIndex = NumReplicatesCBox.Items.IndexOf(AdaptiveStr) then
        aList.Add(Format(FLStr, [opsBootstrapRepsPerSample, AdaptiveStr]))
      else
        aList.Add(ASetting.MaoString(FLStr));
    end
    else if ASetting.Name = opsPercentSitesPerSample then
    begin
      if PercentSitesCBox.ItemIndex = PercentSitesCBox.Items.IndexOf(AdaptiveStr) then
        aList.Add(Format(FLStr, [opsPercentSitesPerSample, AdaptiveStr]))
      else
        aList.Add(ASetting.MaoString(FLStr));
    end
    else if ASetting.Name = opsMLNumThreads then
    begin
      if not TryStrToInt(ASetting.StringValue, tempInt) then
        aList.Add(Format(FLStr, [ASetting.Name, '1']))
      else
        aList.Add(ASetting.MaoString(FLStr));
    end
    else
    begin
      if (ASetting.Name <> opsMaxExecutionTime2) and (ASetting.Name <> opsGeneticCodeTable2) and (ASetting.Name <> 'Analysis') then
        aList.Add(ASetting.MaoString(FLStr));
    end;
  end;

  if HasSetting(opsGeneticCodeTable2, ARow) then
  begin
    aList.Add(Format(FLStr, [opsGeneticCodeTable2, GetSetting(opsGeneticCodeTable2).StringValue]));
    aList.Add(Format(FLStr, [opsGeneticCode2, SelectedGeneticCode]));
  end;

  aList.Add(Format(FLStr, [opsHasTimeLimit, BoolToStr(MaxRuntimeCheckbox.Checked, True)]));
  if MaxRuntimeCheckbox.Checked then
    aList.Add(Format(FLStr, [opsMaxExecutionTime2, Trim(MaxExecutionTimeEdit.Text)]))
  else
    aList.Add(Format(FLStr, [opsMaxExecutionTime2, '-1']));
end;

function TAnalysisPrefDlg.GetMaoFileSaveLocation(var saveLoc: String): Boolean;
var
  CurrentDir: String;
begin
  Result := False;
  CurrentDir := GetCurrentDir;
  if DirectoryExists(CurrentDir) then
    SaveAnalysisDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(CurrentDir);
  SaveAnalysisDlg.FileName := ProcessPackTemp.ProposeFilename(SubstitutionTypeStr) + AnalysisOptExt;
  SaveAnalysisDlg.Filter := AnalysisOptType + ' (' + AnalysisOptExt + ') | *' + AnalysisOptExt;
  SaveAnalysisDlg.FilterIndex := 1;
  if SaveAnalysisDlg.Execute then
  begin
    saveLoc := SaveAnalysisDlg.FileName;
    SwitchDirectory(saveLoc);
    if FileExists(saveLoc) then
    begin
      if (MessageDlg('A file by this name already exists.'+ LineEnding +'Would you like to overwrite it?', mtConfirmation, [mbYes, mbNo], 0) in [mrYes]) then
        Result := True;
    end
    else
      Result := True;
    if not endsWith(saveLoc, AnalysisOptExt) then
      saveLoc := saveLoc + AnalysisOptExt;
  end;
end;

function TAnalysisPrefDlg.GetMaoFormatLen: Integer;
begin
  Result := Max(10, LengthOfLongestNameString(FCurrentOptions) + 5);
end;

function TAnalysisPrefDlg.ValidSpecialIs(TheChar: Char): Boolean;
begin
  case MegaForm.DataType of
    snNucleotide,
    snProtein:
      begin
         case UpCase(TheChar) of
           'A'..'Z':
             case TheChar of
               'A'..'D','G','H','K','M','R','S', 'T', 'U', 'V','W','Y':
                  Result := False;
             else
                  Result := True;
             end;
           '?','-','.':
             Result := True;
         else
           Result := False;
         end
      end
      else
      begin
         case UpCase(TheChar) of
           'A'..'Z':
             case TheChar of
               'J','O','U','X':
                  Result := False;
             else
               Result := True;
             end;
           '?','-','.':
           Result := True;
         else
           Result := False;
         end;
      end
    end;
end;

function TAnalysisPrefDlg.ExamineThreeSymbols(gap, miss, iden: Char): String;
begin
  Result := '';

  if not ValidSpecialIs(miss) then
  begin
    Result := Result + 'Invalid Missing data symbol: '+ miss + #13#10;
  end;

  if not ValidSpecialIs(iden) then
  begin
    Result := Result + 'Invalid identical symbol: '+ iden + #13#10;
  end;

  if not ValidSpecialIs(gap) then
  begin
    Result := Result + 'Invalid alignment gap symbol: '+ gap + #13#10;
  end;

  if (gap=miss) or (gap=iden) or (miss=iden) then
  begin
    Result := Result + 'Three symbols must be different. Found: Missing(' + miss + '),Identical(' + iden + '),Gap('+gap+')' + #13#10;
  end;
end;

function TAnalysisPrefDlg.ValidateLabelledSitesOption: Boolean;
var
  labels: String;
  i: Integer;
begin
  Result := True;
  if (ProtoLabelledSitesCombo.ItemIndex = 0) or (ProtoLabelledSitesCombo.ItemIndex = 2) then
    Exit;
  Result := False;
  labels := Trim(ProtoLabelsToUseEdit.Text);
  if Length(labels) = 0 then
  begin
    OptionsPageControl.ActivePage := DataSettingsPage;
    ShowMessage('Please provide valid site labels to use or select "Use All Sites"');
    Exit;
  end;
  for i := 1 to Length(labels) do
  begin
    if not IsValidSiteLabel(Labels[i]) then
    begin
      OptionsPageControl.ActivePage := DataSettingsPage;
      ShowMessage('Only letters, numbers, and the "*", "+", "-", and "_" characters can be used as site labels');
      Exit;
    end;
  end;
  Result := True;
end;

procedure TAnalysisPrefDlg.CMDialogKey(var msg: TCMDialogKey);
begin

end;

procedure TAnalysisPrefDlg.AddDataSettingsOptions;
var
  SelectedDataType: String;
  EnableSymbolEdits: Boolean;
begin
  SelectedDataType := MapTokenCodeToDataTypeString(MegaForm.DataType);
  DataTypeLabel.Caption := SelectedDataType;

  // if the user has selected distance data, disable the edit boxes for setting
  // special symbols, otherwise enable them
  EnableSymbolEdits := not (MegaForm.DataType = snDistance);

  MissingSymbolEdit.Enabled := EnableSymbolEdits;
  IdenticalSymbolEdit.Enabled := EnableSymbolEdits;
  GapSymbolEdit.Enabled := EnableSymbolEdits;
  DataSettingsDefaultsBtn.Enabled := EnableSymbolEdits;
end;

procedure TAnalysisPrefDlg.AddStringSetting(AName: String; AValue: String; IsReadOnly: Boolean = False; IsApplicable: Boolean = True; aHint: String = '');
var
  ASetting: TStringAnalysisSetting = nil;
  prevOption: TAnalysisSetting = nil;
begin
  ASetting := TStringAnalysisSetting.Create(GetIndentLevelForName(AName));
  ASetting.Update(AName, AValue);
  if IsReadOnly then
    ASetting.IsReadOnly := IsReadOnly;
  ASetting.IsApplicable := IsApplicable;
  prevOption := PreviousOption(AName);
  if Assigned(prevOption) and (not ASetting.IsReadOnly) and (aName <> opsPhylo2) and (prevOption.SettingType = ASetting.SettingType) then
  begin
    ASetting.AssignValues(TStringAnalysisSetting(prevOption));
    if ((SkipTreeActionActivated) and (AName = 'User Tree File')) then
      ASetting.Update(AName, NotApplicableStr);
    if (not(SkipTreeActionActivated) and (AName = 'User Tree File')) then
      ASetting.Update(AName, TreeFileNameEdit.Text);
  end;
  ASetting.Hint := aHint;
  AddRow(ASetting);
end;

function TAnalysisPrefDlg.AddIntegerSetting(AName: String; IsReadOnly: Boolean; IsApplicable: Boolean = True; aHint: String = ''): TIntegerAnalysisSetting;
var
  prevOption: TAnalysisSetting;
begin
  Result := TIntegerAnalysisSetting.Create(GetIndentLevelForName(AName));
  Result.Name := AName;
  Result.IsReadOnly := IsReadOnly;
  Result.IsApplicable := IsApplicable;
  Result.Min := Trunc(spinRowMin(AName));
  Result.Max := Trunc(spinRowMax(AName));
  Result.Increment := Trunc(spinRowIncrement(AName));
  prevOption := PreviousOption(AName);
  if not IsBlacklistedFromUsingPrevOp(AName) then
  begin
    if Assigned(prevOption) and (prevOption.SettingType = Result.SettingType) then
      Result.AssignValues(TIntegerAnalysisSetting(prevOption))
    else if HasDefaultOption(AName) then
      Result.SetValueFromStr(GetDefaultOptions(AName));
  end
  else if HasDefaultOption(AName) then
    Result.SetValueFromStr(GetDefaultOptions(AName));
  Result.Hint := aHint;
  AddRow(Result);
  if AName = opsBootstrapSubSamples then
    NumSamplesSE.Value := TIntegerAnalysisSetting(Result).Value
  else if AName = opsBootstrapRepsPerSample then
    NumReplicatesSE.Value := TIntegerAnalysisSetting(Result).Value;
end;

function TAnalysisPrefDlg.AddFloatSetting(AName: String; IsReadOnly: Boolean; IsApplicable: Boolean = True; aHint: String = ''): TFloatAnalysisSetting;
var
  prevOption: TAnalysisSetting = nil;
begin
  Result := TFloatAnalysisSetting.Create(GetIndentLevelForName(AName));
  Result.Name := AName;
  Result.IsReadOnly := IsReadOnly;
  Result.IsApplicable := IsApplicable;
  if isSpinRow(AName) then
  begin
    Result.Min := spinRowMin(AName);
    Result.Max := spinRowMax(AName);
    Result.Increment := spinRowIncrement(AName);
    if HasDefaultOption(Result.Name) then
      Result.SetValueFromStr(GetDefaultOptions(Result.Name))
    else
      Result.Value := Result.Min;
  end;

  prevOption := PreviousOption(AName);
  if Assigned(prevOption) and (prevOption.SettingType = Result.SettingType) then
    Result.AssignValues(TFloatAnalysisSetting(prevOption));
  Result.Hint := aHint;
  AddRow(Result);

  if AName = opsPercentSitesPerSample then
    PercentSitesSE.Value :=  TFloatAnalysisSetting(Result).Value;
end;

procedure TAnalysisPrefDlg.AddPickListSetting(AName: String; AValue: String; AList: String; aHint: String = '');
var
  ASetting: TPickListAnalysisSetting = nil;
  valueSetting: TPickListValueSetting = nil;
  prevOption: TAnalysisSetting = nil;
  defaultVal: String;
begin
  if CurrentOption(AName) <> nil then
  begin
    prevOption := CurrentOption(AName);
    FCurrentOptions.Remove(prevOption);
    FreeAndNil(prevOption);
  end;
  ASetting := TPickListAnalysisSetting.Create(GetIndentLevelForName(AName), aList);
  ASetting.Update(AName, AValue);
  if (aList <> EmptyStr) and (not IsBlacklistedFromUsingPrevOp(AName)) then
  begin
    prevOption := PreviousOption(aList);
    if (not Assigned(prevOption)) or (prevOption.SettingType <> stPickListValue) then
    begin
      valueSetting := TPickListValueSetting.Create(ASetting.IndentLevel);
      valueSetting.Update(AList, ASetting.DefaultValue);
      StoreOption(valueSetting);
    end
    else
      valueSetting := TPickListValueSetting(prevOption);
    ASetting.ValueSetting := valueSetting;
  end;
  prevOption := PreviousOption(AName);
  if Assigned(prevOption) then
    RemovePreviousOption(AName);

  if Assigned(valueSetting) and ASetting.HasValue(valueSetting.Value) then
    ASetting.Value := valueSetting.Value
  else
  begin
    if HasDefaultOption(AName, AList) then
      begin
        defaultVal := GetDefaultOptions(AName, AList);
        if ASetting.PickListStrings.IndexOf(defaultVal) >= 0 then
          ASetting.Value := defaultVal;
      end;
  end;
  ASetting.Hint := aHint;
  AddRow(ASetting);
end;

procedure TAnalysisPrefDlg.AddRow(ASetting: TAnalysisSetting);
begin
  StoreOption(aSetting);
  FCurrentOptions.Add(ASetting);
end;

procedure TAnalysisPrefDlg.UpdateRow(const AName: String; const AValue: String; PickListName: String);
var
  ASetting: TAnalysisSetting;
  AStatus: Boolean;
begin
  Assert(FCurrentOptions.Count > 0);
  ASetting := CurrentOption(AName);
  Assert(Assigned(ASetting));
  if not Assigned(ASetting) then
    Exit;
  StoreOption(aSetting);
  if Trim(PickListName) <> EmptyStr then
  begin
    if not (ASetting.SettingType = stPicklist) then
    begin
      Assert(False, 'trying to change setting type');
    end;
    with (ASetting as TPickListAnalysisSetting) do
      PickList := AValue;
  end
  else
  begin
    AStatus := ASetting.SetValueFromStr(AValue);
    Assert(AStatus);
  end;
  OptionsChosenGrid.Invalidate;
end;

procedure TAnalysisPrefDlg.UpdatePicklist(const AName: String; const AValue: String; const PickListName: String);
var
  curSetting: TAnalysisSetting = nil;
  prevSetting: TAnalysisSetting = nil;
  PlistSetting: TPickListAnalysisSetting = nil;
  AClone: TPickListAnalysisSetting = nil;
  vSetting: TPickListValueSetting = nil;
  Index: Integer;
begin
  Assert(NumOccurrencesInPrevOps(AName) <= 1, 'unexpected number of previous options: ' + AName);
  curSetting := CurrentOption(AName);
  Assert(Assigned(curSetting), 'invalid call to UpdatePickList - it does not exist');
  if curSetting.SettingType = stPicklist then
  begin
    TPickListAnalysisSetting(curSetting).PickList := AValue;
    TPickListAnalysisSetting(curSetting).PickListName := PickListName;
    prevSetting := PreviousOption(AName); { ensure that they have the same picklist}
    if prevSetting.SettingType = stString then
    begin { previous option was a string setting so replace it with a picklist setting}
      PlistSetting := TPickListAnalysisSetting(curSetting);
      PlistSetting := PlistSetting.GetClone;
      RemovePreviousOption(AName);
      StoreOption(PListSetting);
    end
    else
    begin
      PlistSetting := TPickListAnalysisSetting(curSetting);
      PlistSetting := PlistSetting.GetClone;
      if PreviousOption(AName) <> nil then
        RemovePreviousOption(AName);
      StoreOption(PlistSetting);
      if PreviousOption(PickListName) <> nil then
        RemovePreviousOption(PickListName);
      vSetting := TPickListValueSetting.Create(curSetting.IndentLevel);
      vSetting.Update(PickListName, TPickListAnalysisSetting(curSetting).Value);
      TPickListAnalysisSetting(curSetting).ValueSetting := vSetting;
      PlistSetting.ValueSetting := vSetting;
      StoreOption(vSetting);
      UpdatePickListValue(PlistSetting);
    end;
  end
  else { changing from a string to a picklist}
  begin
    PlistSetting := TPickListAnalysisSetting.Create(GetIndentLevelForName(AName), PickListName);
    PlistSetting.Update(AName, AValue);
    Index := FCurrentOptions.Remove(curSetting);
    FCurrentOptions.Insert(Index, PlistSetting);
    AClone := PlistSetting.GetClone;
    FreeAndNil(curSetting);
    RemovePreviousOption(AName);
    StoreOption(AClone);
    if PreviousOption(PListSetting.PickListName) <> nil then
    begin
      vSetting := TPickListValueSetting(PreviousOption(PListSetting.PickListName));
      vSetting.Value := PListSetting.Value;
    end
    else
    begin
      vSetting := TPickListValueSetting.Create(PListSetting.IndentLevel);
      vSetting.Update(PlistSetting.PickListName, PListSetting.Value);
    end;

    PListSetting.ValueSetting := vSetting;
    AClone.ValueSetting := vSetting;
    StoreOption(vSetting);
    UpdatePickListValue(PlistSetting);
  end;
end;

function TAnalysisPrefDlg.IsCurrentSetting(AName: String; AValue: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if FCurrentOptions.Count = 0 then
    Exit;
  for i := 0 to FCurrentOptions.Count - 1 do
  begin
    if TAnalysisSetting(FCurrentOptions[i]).Name = AName then
    begin
      Result := (TAnalysisSetting(FCurrentOptions[i]).StringValue = AValue);
      Exit;
    end;
  end;
end;

function TAnalysisPrefDlg.HasSetting(AName: String; var ARow: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if FCurrentOptions.Count = 0 then
    Exit;
  for i := 0 to FCurrentOptions.Count - 1 do
  begin
    if AName = TAnalysisSetting(FCurrentOptions[i]).Name then
    begin
      ARow := i;
      Result := True;
      Exit;
    end;
  end;
end;

function TAnalysisPrefDlg.HasSetting(AName: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if FCurrentOptions.Count = 0 then
    Exit;
  for i := 0 to FCurrentOptions.Count - 1 do
  begin
    if AName = TAnalysisSetting(FCurrentOptions[i]).Name then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TAnalysisPrefDlg.TaxaNamesPicklist: String;
var
  i: Integer;
begin
  Result := EmptyStr;
  if FTaxaList.Count > 0 then
    for i := 0 to FTaxaList.Count - 1 do
    begin
      if i = (FTaxaList.Count - 1) then
        Result := Result + #34 + FTaxaList[i] + #34
      else
        Result := Result + #34 + FTaxaList[i] + #34 + ','
    end;
end;

procedure TAnalysisPrefDlg.SetStateForLittleBootstrapParams;
var
  aSetting: TAnalysisSetting = nil;
  subsamplingIsSelected: Boolean = False;

  function CheckIsSubsampling: Boolean;
  begin
    if HasSetting(opsTestPhylo2) then
      Result := (CurrentOptionStr(opsTestPhylo2) = opsSubsampleUpsample2)
    else if HasSetting(opsEstimateVar2) then
      Result := (CurrentOptionStr(opsEstimateVar2) = opsSubsampleUpsample2)
    else if HasSetting(opsApproachType2) then
      Result := CurrentOptionStr(opsApproachType2) = opsSubsampleUpsample2
    else
      Result := False;
  end;

begin
  aSetting := GetSetting(opsBootstrapPrecisionThreshold);
  if Assigned(aSetting) then
  begin
    if (GetPickListSettingValue(opsTestPhylo2) = BootTestAdaptiveStr) or (GetPickListSettingValue(opsTestPhylo2) = BootTestAdaptiveStrFast) then
      aSetting.SetState(False, True)
    else
      aSetting.SetState(True, False);
  end;

  if not EnableSubsamplingAnalyses then
    Exit;
  if (not OperationSupportsSubsamplingApproach(FOperation)) or (FOperation = dtdoMLModelTest) then
    Exit;

  subsamplingIsSelected := CheckIsSubsampling;
  if HasSetting(opsBootstrapSubSamples) then
  begin
    aSetting := GetSetting(opsBootstrapSubSamples);
    aSetting.SetState(not subsamplingIsSelected, subsamplingIsSelected);
    NumSamplesPanel.Visible := aSetting.IsApplicable;
    NumSamplesSE.Visible := NumSamplesCBox.ItemIndex <> NumSamplesCBox.Items.IndexOf(AdaptiveStr);
    aSetting.IsAdaptive := (not NumSamplesSE.Visible);
    if NumSamplesPanel.Visible then
      PositionEllipsisPanel(NumSamplesPanel);
  end;

  if not (FOperation in [dtdoMLGammRates, dtdoMLTsTvBias]) then
  begin
    aSetting := GetSetting(opsBootstrapRepsPerSample);
    aSetting.SetState(not subsamplingIsSelected, subsamplingIsSelected);
    NumReplicatesPanel.Visible := aSetting.IsApplicable;
    NumReplicatesSE.Visible := NumReplicatesCBox.ItemIndex <> NumReplicatesCBox.Items.IndexOf(AdaptiveStr);
    aSetting.IsAdaptive := (not NumReplicatesSE.Visible);
    if NumReplicatesPanel.Visible then
      PositionEllipsisPanel(NumReplicatesPanel);
  end;

  aSetting := GetSetting(opsPercentSitesPerSample);
  aSetting.SetState(not subsamplingIsSelected, subsamplingIsSelected);
  PercentSitesPanel.Visible := aSetting.IsApplicable;
  PercentSitesSE.Visible := PercentSitesCBox.ItemIndex <> PercentSitesCBox.Items.IndexOf(AdaptiveStr);
  aSetting.IsAdaptive := (not PercentSitesSE.Visible);
  if PercentSitesPanel.Visible then
    PositionEllipsisPanel(PercentSitesPanel);

  if IsDeveloper then
  begin
    aSetting := GetSetting(opsBootstrapMaxSitesPerSample);
    if IsPrototyper then
      aSetting.SetState(True, False)
    else
      aSetting.SetState(not subsamplingIsSelected, subsamplingIsSelected);
  end;

  if IsDeveloper then
  begin
    aSetting := GetSetting(opsBootstrapSampleSizeFactor);
    aSetting.SetState(not subsamplingIsSelected, subsamplingIsSelected);
    aSetting := GetSetting(opsLbsTargetPrecisionStdDev);
    aSetting.SetState(not subsamplingIsSelected, subsamplingIsSelected);
    aSetting := GetSetting(opsLbsSkipTreeSwaps);
    aSetting.SetState(not subsamplingIsSelected, subsamplingIsSelected);
  end;

  if OperationSupportsSubsamplingDoubleCheck(FOperation) then
  begin
    aSetting := GetSetting(opsSubsamplingVerifyResult);
    aSetting.SetState(not subsamplingIsSelected, subsamplingIsSelected);
  end;
end;

procedure TAnalysisPrefDlg.SetStateForModelSelectionParams;
var
  pListSetting: TPickListAnalysisSetting = nil;
  percentSitesSetting: TFloatAnalysisSetting = nil;
  verifyResultSetting: TPickListAnalysisSetting = nil;
  s: TAnalysisSetting = nil;
begin
  if not HasSetting(opsApproach1) then
    Exit;

  pListSetting := GetSetting(opsApproachType2) as TPickListAnalysisSetting;
  if (pListSetting.Value = opsModelSelFiltered2) or (pListSetting.Value = opsModelSelFilteredFast2) or (pListSetting.Value = opsModelTamerFast2) then
  begin
    s := GetSetting(opsDeltaBicCutoff);
    if Assigned(s) then
      s.SetState(False, True);
    s := GetSetting(opsDeltaAicCutoff);
    if Assigned(s) then
      s.SetState(False, True);
  end
  else
  begin
    s := GetSetting(opsDeltaBicCutoff);
    if Assigned(s) then
      s.SetState(True, False);
    s := GetSetting(opsDeltaAicCutoff);
    if Assigned(s) then
      s.SetState(True, False);
  end;

  if not EnableSubsamplingAnalyses then
    Exit;

  verifyResultSetting := GetSetting(opsSubsamplingVerifyResult) as TPickListAnalysisSetting;
  percentSitesSetting := GetSetting(opsPercentSitesPerSample) as TFloatAnalysisSetting;
  if (pListSetting.Value = opsModelSelFull2) or (pListSetting.Value = opsModelSelFiltered2) or (pListSetting.Value = opsModelSelFilteredFast2) then
  begin
    percentSitesSetting.SetState(True, False);
    verifyResultSetting.SetState(True, False);
  end
  else if pListSetting.Value = opsModelTamerFast2 then
  begin
    percentSitesSetting.SetState(False, True);
    verifyResultSetting.SetState(False, True);
  end
  else
    raise Exception.Create(format('Application error: unexpected value for %s setting. Got %s but expected %s or %s', [opsApproachType2, pListSetting.Value, opsStandardSlow2, opsModelTamerFast2]));

  PercentSitesPanel.Visible := percentSitesSetting.IsApplicable;
  PercentSitesSE.Visible := PercentSitesCBox.ItemIndex <> PercentSitesCBox.Items.IndexOf(AdaptiveStr);
  percentSitesSetting.IsAdaptive := (not PercentSitesSE.Visible);
  if PercentSitesPanel.Visible then
    PositionEllipsisPanel(PercentSitesPanel);
  Invalidate;
end;

function TAnalysisPrefDlg.OperationSupportsSubsamplingApproach(aOperation: TDistTreeDlgOption): Boolean;
begin
  Result := aOperation in [dtdoMLGammRates, dtdoMLComputeUserTreeBLens, dtdoMLTsTvBias, dtdoMLModelTest, dtdoMLTree, dtdoRelTimeML];
end;

function TAnalysisPrefDlg.OperationSupportsSubsamplingDoubleCheck(aOperation: TDistTreeDlgOption): Boolean;
begin
  Result := aOperation in [dtdoMLTree, dtdoMLModelTest];
end;

procedure TAnalysisPrefDlg.AddOperationTypeRows;
var
  ASetting: TStringAnalysisSetting;
begin
  OptionsSummaryTab.Caption := OperationTypeString(FOperation);
  ASetting := TStringAnalysisSetting.Create(GetIndentLevelForName(opsOperationType1));
  ASetting.Update(opsOperationType1, opsPlus);
  ASetting.IsReadOnly := True;
  AddRow(ASetting);
  if FOperation in [dtdoMLClockTest, dtdoMLComputeUserTreeBLens, dtdoMLGammRates, dtdoMLInferAncSeq,
                    dtdoMLInferAncSeqMyPeg, dtdoMLInferSiteBySiteRates, dtdoMLModelTest, dtdoCorrTestML,
                    dtdoEpML, dtdoRelTimeML, dtdoRtdtML] then
    AddStringSetting(opsPhylo2, 'Maximum Likelihood', True);
end;

procedure TAnalysisPrefDlg.AddAnalysisScopeRows;
var
  A, B, C: String;
  aSetting: TPickListAnalysisSetting;

  function FindAnother(x,y: Integer): Integer;
  begin
     Result := 0;
     while (Result = x) or (Result = y) do
       Inc(Result);
  end;

begin
  case FOperation of
    dtdoPairwiseDist             : AddStringSetting(opsScope2, 'Pairs of taxa', True, True);
    dtdoOverallMean              : AddStringSetting(opsScope2, 'Overall Mean', True, True);
    dtdoWithinGroupMean          : AddStringSetting(opsScope2, 'Within group average', True, True);
    dtdoBetweenGroupMean         : AddStringSetting(opsScope2, 'Between group average', True, True);
    dtdoNetGroupMean             : AddStringSetting(opsScope2, 'Net between group average', True, True);
    dtdoAvgDiversityWithinSubPops: AddStringSetting(opsScope2, 'Average within subpopulation', True, True);
    dtdoAvgDiversityForEntirePop : AddStringSetting(opsScope2, 'Average for overall population', True, True);
    dtdoInterPopDiversity        : AddStringSetting(opsScope2, 'Interpopulation diversity', True, True);
    dtdoPropOfInterPopDiversity  : AddStringSetting(opsScope2, 'Proportion of Interpopulation diversity', True, True);

    dtdoSelectionExactTest       : AddStringSetting(opsScope2, 'In Sequence Pairs', True, True); //used to be 'Pairs of sequences'
    dtdoSelectionZTest           :
      begin
        if FHasGps then
          AddPickListSetting(opsScope2, SelWithGpPickList,  'SelWithGpPickList')
        else
          AddPickListSetting(opsScope2, SelWithNoGpPickList,'SelWithNoGpPickList');
        AddPickListSetting(opsSelHypoToTest2, SelTestHypoPickList, 'SelTestHypoPickList');
      end;
    dtdoTajimaNeutralityTest     : AddStringSetting(opsScope2, AllSelectedTaxaStr, True, True);

    dtdoTajimaClockTest          :
        begin
          AddStringSetting(opsScope2, 'For 3 chosen sequences', True, True);
          AddStringSetting(opsChooseThreeTaxaForClocks1, opsPlus, True, True);

          if IsPrototyper then
          begin
            A := 'Ingroup - first sequence in data file';
            B := 'Ingroup - second sequence in data file';
            C := 'Outgroup - third sequence in data file';
            AddStringSetting(opsIngroupTaxonA2, A, True, True);
            AddStringSetting(opsIngroupTaxonB2, B, True, True);
            AddStringSetting(opsOutgroupTaxonC2, C, True, True);
          end
          else
          begin
            FTaxaPickList := TaxaNamesPicklist;
            A := FTaxaList[0];
            B := FTaxaList[1];
            C := FTaxaList[2];
            CheckPreviousPickListOptionValid(opsIngroupTaxonA2, FTaxaPickList);
            CheckPreviousPickListOptionValid(opsIngroupTaxonB2, FTaxaPickList);
            CheckPreviousPickListOptionValid(opsOutgroupTaxonC2, FTaxaPickList);
            AddPickListSetting(opsIngroupTaxonA2, FTaxaPickList, 'TaxaNamesPickListA');
            AddPicklistSetting(opsIngroupTaxonB2, FTaxaPickList, 'TaxaNamesPickListB');
            AddPicklistSetting(opsOutgroupTaxonC2, FTaxaPickList, 'TaxaNamesPickListC');

            aSetting := TPickListAnalysisSetting(GetSetting(opsIngroupTaxonA2));
            aSetting.SetValueFromStr(A);
            aSetting := TPickListAnalysisSetting(GetSetting(opsIngroupTaxonB2));
            aSetting.SetValueFromStr(B);
            aSetting := TPickListAnalysisSetting(GetSetting(opsOutgroupTaxonC2));
            aSetting.SetValueFromStr(C);
          end;
        end;

    dtdoNJTree,
    dtdoUPGMATree,
    dtdoMETree                   :
        if FIsDist then
        begin
          AddStringSetting(opsScope2, AllSelectedTaxaStr, True, True);
        end
        else
        begin
          if FHasGps then
            AddPickListSetting(opsScope2, DistTreeTaxaPickList, 'DistTreeTaxaPickList')
          else
            AddStringSetting(opsScope2, AllSelectedTaxaStr, True, True);
        end;
    dtdoSiteCoverage:
      begin
        AddStringSetting(opsScope2, AllSelectedTaxaStr, True, True);
        AddStringSetting(opsTreeToUse2, UserSpecifyFromFile, True);
        AddStringSetting(opsPickUserTree2, UserTreeFile, False, True, UserTreeFile);
      end;
    dtdoCompositionDistance,
    dtdoDisparityIndex,
    dtdoDisparityIndexTest       : AddStringSetting(opsScope2, 'In Sequence Pairs', True, True); //Used to be 'Pairs of sequences'
    dtdoMCLComputePattern,
    dtdoMCLTsTvBias              : AddStringSetting(opsScope2, AllSelectedTaxaStr, True, True);

    dtdoMLComputePattern,
    dtdoMLTsTvBias,
    dtdoMLGammRates,
    dtdoMLInferSiteBySiteRates,
    dtdoMLModelTest, dtdoMLModelTamer:
      begin
        if isPrototyper then
        begin
          AddPickListSetting(opsTreeToUse2, TreeToUsePickListForPrototyper, 'TreeToUsePickList');
          AddStringSetting(opsPickUserTree2, NotApplicableStr, True, False);
        end
        else
          AddPickListSetting(opsTreeToUse2, TreeToUsePickList, 'TreeToUsePickList');

        if not isPrototyper then
        begin
          if (FOperation = dtdoMLCodonOmega) and FileExists(UserTreeFile) then
          begin
            UpdateRow(opsTreeToUse2, UserSpecifyFromFile, TreeToUsePickList);
            AddStringSetting(opsPickUserTree2, UserTreeFile, False, True, UserTreeFile);
          end
          else
            AddStringSetting(opsPickUserTree2, NotApplicableStr, True, False);
        end;
      end;
    dtdoRelTimeML, dtdoRtdtML,
    dtdoRelTimeLS, dtdoRtdtLS,
    dtdoRelTimeBlens,dtdoRtdtBlens,
    dtdoCorrTestML,
    dtdoCorrTestBlens,
    dtdoMLInferAncSeq,
    dtdoMPInferAncSeq,
    dtdoMLPredictLivingSeq, dtdoEpML,
    dtdoMPPredictLivingSeq,
    dtdoMLClockTest,
    dtdoMLClockTestLocal,
    dtdoOLSComputeUserTreeBLens,
    dtdoMPComputeUserTreeBLens,
    dtdoMLComputeUserTreeBLens, dtdoLbsAnalyzeTree, dtdoLbsTiming,
    dtdoOLSInteriorBranchTest:
      begin
        if isPrototyper then
          AddStringSetting(opsTreeToUse2, UserSpecifyFromFile, True, True)
        else if ((FOperation = dtdoRtdtML) or
                 (FOperation = dtdoRtdtLS) or
                 (FOperation = dtdoRelTimeML) or
                 (FOperation = dtdoRelTimeLS) or
                 (FOperation = dtdoCorrTestML) or
                 (FOperation = dtdoEpML)) then
        begin
          if FUsingTreeFromTopologyEditor then
            AddStringSetting(opsTreeToUse2, UserSpecifyManager, True)
          else
            AddStringSetting(opsTreeToUse2, UserSpecifyFromFile, True)
        end
        else
          AddPickListSetting(opsTreeToUse2, UserTreeList, UserSpecifyFromFile);  // formerly UserTreeStr
        if not isPrototyper then
          AddStringSetting(opsPickUserTree2, NotApplicableStr, FUsingTreeFromTopologyEditor, not FUsingTreeFromTopologyEditor);

        if isDeveloper and (FOperation <> dtdoEpML) then
          AddPickListSetting(opsKeepUserTreeBLens2, KeepUserTreeBLensPickList, 'KeepUserTreeBLensPickList');
        if FOperation = dtdoEpML then
        begin
          if (not FUsingTreeFromTopologyEditor) then
            AddPickListSetting(opsKeepUserTreeTimes2, YesNoPickList, 'YesNoPickList', 'If a rooted ultra-metric tree with times is provided, those times can be used for weighting mean posterior probabilities');
          FTaxaPickList := TaxaNamesPickList;
          AddPickListSetting(opsFocalSpecies2, FTaxaPickList, 'FTaxaPickList', 'The focal species or sequence for which evolutionary probabilities are to be computed');
        end;
        AddClockRows;
      end;
  end;
end;

procedure TAnalysisPrefDlg.UpdateAnalysisScopeRows;
var
  A, B, C: String;
  iA, iB, iC: Integer;

  function FindAnother(x,y: Integer): Integer;
  begin
     Result := 0;
     while (Result = x) or (Result = y) do
       Inc(Result);
  end;

begin
  Inc(IsPreventChangeEventCount);
  if IsUpdatingAnalysisScopeRows > 0 then
    Exit;
  Inc(IsUpdatingAnalysisScopeRows);
    case FOperation of
      dtdoSelectionZTest:
        begin
          UpdateRow(opsScope2, PreviousOptionStr(opsScope2));
          UpdateRow(opsSelHypoToTest2, PreviousOptionStr(opsSelHypoToTest2));
        end;
      dtdoTajimaClockTest:
        begin
          A := PreviousOptionStr(opsIngroupTaxonA2);
          B := PreviousOptionStr(opsIngroupTaxonB2);
          C := PreviousOptionStr(opsOutgroupTaxonC2);
          iA := -1; iB := -1; iC := -1;

          if Length(A) > 0 then iA := FTaxaList.IndexOf(A);
          if Length(B) > 0 then iB := FTaxaList.IndexOf(B);
          if Length(C) > 0 then iC := FTaxaList.IndexOf(C);

          if (iA < 0) and (iB <  0) and (iC <  0) then
          begin
            iA := 0; iB := 1; iC := 2;
          end;

          if      (iA = iB) then  begin iA := -1; iB := -1 end
          else if (iB = iC) then  begin iB := -1; iC := -1 end
          else if (iA = iC) then  begin iA := -1; iC := -1; end;

          if iA < 0 then   iA := FindAnother(iB, iC);
          if iB < 0 then   iB := FindAnother(iA, iC);
          if iC < 0 then   iC := FindAnother(iA, iB);
          if IsPrototyper then
          begin
            A := 'Ingroup - first sequence in data file';
            B := 'Ingroup - second sequence in data file';
            C := 'Outgroup - third sequence in data file';
          end
          else
          begin
            A := FTaxaList[iA];
            B := FTaxaList[iB];
            C := FTaxaList[iC];
          end;
          { TODO 1 -oglen -cmegaxplatform : make sure that picklists are created for selecting taxa here }
          UpdateRow(opsIngroupTaxonA2,  A);
          UpdateRow(opsIngroupTaxonB2,  B);
          UpdateRow(opsOutgroupTaxonC2, C);
          //UpdateRow(opsIngroupTaxonA2,  A, 'TajimaTestA');
          //UpdateRow(opsIngroupTaxonB2,  B, 'TajimaTestB');
          //UpdateRow(opsOutgroupTaxonC2, C, 'TajimaTestC');
        end;
      dtdoNJTree,
      dtdoUPGMATree,
      dtdoMETree:
        UpdateRow(opsScope2, PreviousOptionStr(opsScope2));

      dtdoMLComputePattern,
      dtdoMLTsTvBias,
      dtdoMLGammRates,
      dtdoMLInferSiteBySiteRates,
      dtdoMLModelTest, dtdoMLModelTamer:
        begin
          if (not IsCurrentSetting(opsTreeToUse2, UserSpecifyFromFile)) { and (not FileExists(UserTreeFile))} and (not IsPrototyper) then
          begin
            UpdateRow(opsPickUserTree2, NotApplicableStr);
            CurrentOption(opsPickUserTree2).IsReadOnly := True;
            CurrentOption(opsPickUserTree2).IsApplicable := False;
          end
          else
          begin
            if FileExists(UserTreeFile) and (not IsPrototyper) then
              UpdateRow(opsPickUserTree2, UserTreeFile)
            else if (not IsPrototyper) then
              UpdateRow(opsPickUserTree2, EmptyStr); // fixes an error where the row would NOT update.
            if (not IsPrototyper) then
            begin
              CurrentOption(opsPickUserTree2).IsReadOnly := False;
              CurrentOption(opsPickUserTree2).IsApplicable := True;
            end
            else
            begin
              CurrentOption(opsPickUserTree2).IsReadOnly := True;
              CurrentOption(opsPickUserTree2).IsApplicable := False;
            end;
          end;
        end;
      dtdoSiteCoverage,
      dtdoMLInferAncSeq,
      dtdoMPInferAncSeq,
      dtdoMLPredictLivingSeq,dtdoEpML,
      dtdoMPPredictLivingSeq,
      dtdoMLClockTest,
      dtdoMLClockTestLocal,
      dtdoRelTimeML, dtdoRtdtML,
      dtdoRelTimeBlens, dtdoRtdtBlens,
      dtdoCorrTestML,
      dtdoCorrTestBLens,
      dtdoRelTimeLS, dtdoRtdtLS,
      dtdoOLSComputeUserTreeBLens,
      dtdoMPComputeUserTreeBLens,
      dtdoMLComputeUserTreeBLens, dtdoLbsTiming, dtdoLbsAnalyzeTree,
      dtdoOLSInteriorBranchTest    :
      begin
        if (IsReltimeAnalysis(FOperation) or
           (FOperation = dtdoEpML) or
           (FOperation = dtdoCorrTestML) or
           (FOperation = dtdoCorrTestBLens) or
           (FOperation = dtdoSiteCoverage) or
           (FOperation = dtdoLbsTiming) or
           (FOperation = dtdoLbsAnalyzeTree)) and (not IsPrototyper) and (not FUsingTreeFromTopologyEditor)  then
        begin
          UpdateRow(opsPickUserTree2, UserTreeFile);
        end
        else if not IsCurrentSetting(opsTreeToUse2, UserSpecifyFromFile) then
        begin
          UpdateRow(opsPickUserTree2, NotApplicableStr);
          if IsDeveloper and (FOperation <> dtdoEpML) then
            UpdateRow(opsKeepUserTreeBLens2, NotApplicableStr);
          if (FOperation = dtdoEpML) and HasSetting(opsKeepUserTreeBLens2) then
            UpdateRow(opsKeepUserTreeTimes2, NotApplicableStr);
        end
        else if not IsPrototyper then
        begin
          if FileExists(PreviousOptionStr(opsPickUserTree2)) then
            UpdateRow(opsPickUserTree2, PreviousOptionStr(opsPickUserTree2))
          else
            UpdateRow(opsPickUserTree2, EmptyStr);
          if IsDeveloper and (FOperation <> dtdoEpML) then
            UpdateRow(opsKeepUserTreeBLens2, PreviousOptionStr(opsKeepUserTreeBLens2));
          if (FOperation = dtdoEpML) and (not FUsingTreeFromTopologyEditor) then
            UpdateRow(opsKeepUserTreeTimes2, PreviousOptionStr(opsKeepUserTreeTimes2));
        end;
      end;
    end;

  Dec(IsPreventChangeEventCount);
  Dec(IsUpdatingAnalysisScopeRows);
end;

procedure TAnalysisPrefDlg.AddPhyloAnalysisMethodRows;
var
  debug: String = '';
begin
  case FOperation of
    dtdoNJTree                   :
      begin
        AddStringSetting(opsPhylo2, 'Neighbor-joining', True);
        if IsMolSeq then
        begin
          AddStringSetting(opsTestPhylo1, opsPlus, True, True);
          AddPickListSetting(opsTestPhylo2, TestBootIntBranchPickList,'TestBootIntBranchPickList');
          AddIntegerSetting(opsBootReps2, True, False);
        end;
      end;

    dtdoUPGMATree                :
      begin
        AddStringSetting(opsPhylo2, 'UPGMA', True);
        if IsMolSeq then
        begin
          AddStringSetting(opsTestPhylo1, opsPlus, True);
          AddPickListSetting(opsTestPhylo2,TestBootPickList, 'TestBootPickList');
          AddIntegerSetting(opsBootReps2, True, False);
        end;
      end;

    dtdoMETree                   :
      begin
        AddStringSetting(opsPhylo2, 'Minimum Evolution method', True);
        if IsMolSeq then
        begin
          AddStringSetting(opsTestPhylo1, opsPlus, True);
          AddPickListSetting(opsTestPhylo2, TestBootIntBranchPickList, 'TestBootIntBranchPickList');
          AddIntegerSetting(opsBootReps2, True, False);
        end;
      end;

    dtdoMPComputeUserTreeBLens,
    dtdoMPInferAncSeqMyPeg,
    dtdoMPInferAncSeq            :
        AddStringSetting(opsPhylo2, 'Maximum Parsimony', True);

    dtdoMPTree                   :
      begin
        AddStringSetting(opsPhylo2, 'Maximum Parsimony', True);
        AddStringSetting(opsTestPhylo1, opsPlus, True);
        AddPickListSetting(opsTestPhylo2, TestBootPickList, 'TestBootPickList');
        AddIntegerSetting(opsBootReps2, True, False);
      end;
    dtdoMLIQTree:
      begin
        AddStringSetting(opsPhylo2, 'Maximum Likelihood', True);
        AddStringSetting(opsTestPhylo1, opsPlus, True);
      end;
    dtdoMLTree, dtdoLbsInference:
      begin
        AddStringSetting(opsPhylo2, 'Maximum Likelihood', True);
        AddStringSetting(opsTestPhylo1, opsPlus, True);

        if EnableSubsamplingAnalyses then
        begin
          AddPickListSetting(opsTestPhylo2, LittleBootPickList, 'LittleBootPickList');
          AddIntegerSetting(opsBootReps2, True, False);
          AddIntegerSetting(opsBootstrapSubSamples, False, True, 'Defines the maximum number of sub-samples that will be used (default is 10). The starting number of sub-samples is 3');
          AddIntegerSetting(opsBootstrapRepsPerSample, False, True, 'Defines the maximum number of replicates to use for each sub-sample (default is 10). The starting number of replicates is 3');
          AddFloatSetting(opsPercentSitesPerSample, IsPrototyper, not IsPrototyper, 'Defines the target percentage of site patterns to include in each sub-sample');

          if IsDeveloper then
          begin
            AddIntegerSetting(opsBootstrapMaxSitesPerSample, IsPrototyper, not IsPrototyper, 'Defines the maximum number of sites to sample (without replacement) for each sub-sample');
            AddFloatSetting(opsBootstrapSampleSizeFactor, True, False, 'Specifies sub-sample size as a fractional power of the total number of sites. Must be a value between 0.5 and 1.0 (inclusive). Using this option, sub-sample size is calculated as ceiling(alignment_length^' + opsBootstrapSampleSizeFactor + ').');
            AddPickListSetting(opsLbsTargetPrecisionStdDev, TargetPrecStdDevPickList, 'TargetPrecStdDevPickList', 'Little Bootstraps will check if the standard deviation of computed precision is greater than 2 times the value specified and increase the number of replicates per sub-sample if so');
            AddPickListSetting(opsLbsSkipTreeSwaps, YesNoPickList, 'YesNoPickList', 'Speed up the calculation by skipping the step to compare replicate trees within a sub-sample for best fit to data');
          end;
          AddPickListSetting(opsSubsamplingVerifyResult, YesNoFastSlowPickList, 'YesNoFastSlowPickList', 'After the calculation is complete, double the number of sites used and recalculate results to check the effect of increasing site coverage');
          if IsDeveloper then
            UpdateMaxSitesPerSample;
          AddFloatSetting(opsBootstrapPrecisionThreshold, False, True, 'Target precision for all bootstrap values when using adaptive bootstrap');
        end
        else
        begin
          AddPickListSetting(opsTestPhylo2, StdAdaptiveBootPickList, 'StdAdaptiveBootPickList');
          AddIntegerSetting(opsBootReps2, False, True);
          AddFloatSetting(opsBootstrapPrecisionThreshold, False, True, 'Target precision for all bootstrap values when using adaptive bootstrap');
        end;
      end;
    dtdoEpML:
      begin
        AddStringSetting(opsPhylo2, 'Bayesian/Reltime', True);
      end;
    dtdoMLCodonOmega,
    dtdoMLInferAncSeq,
    dtdoMLInferAncSeqMyPeg,
    dtdoMLPredictLivingSeq,
    dtdoMLComputePattern,
    dtdoMLTsTvBias,
    dtdoMLGammRates,
    dtdoMLInferSiteBySiteRates,
    dtdoMLClockTest,
    dtdoMLClockTestLocal,
    dtdoMLModelTestIQTree,
    dtdoMLComputeUserTreeBLens, dtdoLbsAnalyzeTree, dtdoLbsTiming,
    dtdoRelTimeML, dtdoRtdtML, dtdoBEAM :
      begin
        if EnableSubsamplingAnalyses and OperationSupportsSubsamplingApproach(FOperation) and (not (FOperation in [dtdoRelTimeML])) then
        begin
          AddStringSetting(opsApproach1, opsPlus, True);
          AddPickListSetting(opsApproachType2, opsSubsampleUpsamplePickList, 'LittleBootUserTreePickList');

          if not (FOperation in [dtdoMLGammRates, dtdoMLTsTvBias]) then
          begin
            AddIntegerSetting(opsBootstrapSubSamples, False, True, Format('Defines the maximum number of sub-samples that will be used. The starting number of sub-samples is %d', [MIN_LBS_USER_TREE_SAMPLES]));
            AddIntegerSetting(opsBootstrapRepsPerSample, False, True, Format('Defines the maximum number of replicates to use for each sub-sample. The starting number of replicates is %d', [MIN_LBS_USER_TREE_REPS]));
          end;
          AddFloatSetting(opsPercentSitesPerSample, IsPrototyper, not IsPrototyper, 'Defines the percentage of sites to sample (without replacement) for each sub-sample.');
          if IsDeveloper then
          begin
            AddIntegerSetting(opsBootstrapMaxSitesPerSample, IsPrototyper, not IsPrototyper, 'Defines the maximum number of sites to sample (without replacement) for each sub-sample as the number of sites is increased dynamically');
            AddFloatSetting(opsBootstrapSampleSizeFactor, False, True, 'Specifies sub-sample size as a fractional power of the total number of sites. Must be a value between 0.5 and 1.0 (inclusive). Using this option, sub-sample size is calculated as ceiling(alignment_length^' + opsBootstrapSampleSizeFactor + ').');
            AddPickListSetting(opsLbsTargetPrecisionStdDev, TargetPrecStdDevPickList, 'TargetPrecStdDevPickList', 'Little Bootstraps will check if the standard deviation of computed precision is greater than 2 times the value specified and increase the number of replicates per sub-sample if so');
            AddPickListSetting(opsLbsSkipTreeSwaps, YesNoPickList, 'YesNoPickList', 'Speed up the calculation by skipping the step to compare replicate trees within a sub-sample for best fit to data');
          end;
          if OperationSupportsSubsamplingDoubleCheck(FOperation) then
            AddPickListSetting(opsSubsamplingVerifyResult, YesNoFastSlowPickList, 'YesNoFastSlowPickList', 'After the calculation is complete, double the number of sites used and recalculate results');
          if IsDeveloper then
            UpdateMaxSitesPerSample;
        end;
      end;
    dtdoMLModelTest:
      begin
        if EnableSubsamplingAnalyses then
        begin
          AddStringSetting(opsApproach1, opsPlus, True);
          AddPickListSetting(opsApproachType2, opsModelSelectionApproachPickList, 'opsModelSelectionApproachPickList', 'Use the full sequence alignment data or subsamples of the data (for alignments with large numbers of sites');
          AddFloatSetting(opsPercentSitesPerSample, IsPrototyper, not IsPrototyper, 'Defines the target percentage of site patterns to sample for each sub-sample');
          AddPickListSetting(opsSubsamplingVerifyResult, YesNoFastSlowPickList, 'YesNoFastSlowPickList', 'Iteratively increase the number of site patterns used and redo model selection until results have coalesced');
        end
        else
        begin
          AddStringSetting(opsApproach1, opsPlus, True);
          AddPickListSetting(opsApproachType2, opsModelSelectionApproachPickListMega12, 'opsModelSelectionApproachPickListMega12', 'Analyze all available models or adaptively filter sub-optimal derivative models.');
        end;
        AddIntegerSetting(opsDeltaBicCutoff, False, True, 'Models whose difference in BIC value from the best model is greater than this value are discarded');
        AddIntegerSetting(opsDeltaAicCutoff, False, True, 'Models whose difference in AICc value from the best model is greater than this value are discarded');
      end;
    dtdoCorrTestML:
      begin
        AddStringSetting(opsPhylo2, 'Corrtest (ML)', True);
      end;
    dtdoMCLComputePattern,
    dtdoMCLTsTvBias         :
      AddStringSetting(opsPhylo2, 'Maximum Composite Likelihood', True);

    dtdoOLSComputeUserTreeBLens,
    dtdoRelTimeLS, dtdoRtdtLS  :
      begin
        AddStringSetting(opsPhylo2, 'Ordinary Least Squares', True);
      end;
    dtdoOLSInteriorBranchTest:
      begin
        AddStringSetting(opsPhylo2, 'Ordinary Least Squares', True);
        AddStringSetting(opsTestPhylo1, opsPlus, True);
        AddStringSetting(opsTestPhylo2, IntBranchTestStr);
        AddIntegerSetting(opsBootReps2, True);
      end;
  end;
end;

procedure TAnalysisPrefDlg.UpdatePhyloAnalysisMethodRows;
var
  ARow: Integer = -1;
  aSetting: TAnalysisSetting = nil;
begin
  Inc(IsPreventChangeEventCount);

  if (FOperation in [dtdoMLGammRates, dtdoMLComputePattern, dtdoMLTsTvBias]) then
  begin
    SetStateForLittleBootstrapParams;
    Exit;
  end;

  if HasSetting(opsTestPhylo2, ARow) then
  begin
    if CurrentOptionIsInArray(opsTestPhylo2, [NoneStr, BootTestAdaptiveStr, BootTestAdaptiveStrFast]) then
    begin
      if HasSetting(opsBootReps2) then
        GetSetting(opsBootReps2).IsApplicable := False;
      SetStateForLittleBootstrapParams;
    end
    else if (CurrentOptionStr(opsTestPhylo2) = BootTestStr) or (CurrentOptionStr(opsTestPhylo2) = IntBranchTestStr) or (CurrentOptionStr(opsTestPhylo2) = BootTestSlowStr) then
    begin
      aSetting := GetSetting(opsBootReps2);
      aSetting.SetState(False, True);
      SetStateForLittleBootstrapParams;
    end
    else if (CurrentOptionStr(opsTestPhylo2) = opsSubsampleUpsample2) then
      SetStateForLittleBootstrapParams;
  end
  else if HasSetting(opsApproachType2, ARow) then
    SetStateForLittleBootstrapParams;

  if HasSetting(opsMLBLenOptimize2, ARow) then
    UpdateRow(opsMLBLenOptimize2, PreviousOptionStr(opsMLBLenOptimize2));
  if FOperation = dtdoMLModelTest then
    SetStateForModelSelectionParams;
  Dec(IsPreventChangeEventCount);
end;

procedure TAnalysisPrefDlg.AddDistAnalysisOptionsRows;
begin
  if IsJustDistCompute(FOperation) then
  begin
    AddStringSetting(opsEstimateVar1, opsPlus, True);
    AddPickListSetting(opsEstimateVar2, StdErrBootPickList, 'StdErrBootPickList');
    AddIntegerSetting(opsBootReps2, True);
  end
  else if FOperation = dtdoSelectionZTest then
  begin
    AddStringSetting(opsEstimateVar1, opsPlus, True);
    AddStringSetting(opsEstimateVar2, AnalTestStr, True, True);
    AddIntegerSetting(opsBootReps2, True, False);
  end
  else if (FOperation = dtdoDisparityIndexTest) then
    AddIntegerSetting(opsMonteCarloReps2);
end;

procedure TAnalysisPrefDlg.UpdateDistAnalysisOptionsRows;
var
  debug: STring = '';
begin
  Inc(IsPreventChangeEventCount);
  if IsJustDistCompute(FOperation) or (FOperation = dtdoSelectionZTest) then
  begin
    debug := PreviousOptionStr(opsEstimateVar2);
    UpdateRow(opsEstimateVar2, PreviousOptionStr(opsEstimateVar2));
    UpdateRow(opsBootReps2, PreviousOptionStr(opsBootReps2));
  end
  else if (FOperation = dtdoDisparityIndexTest) then
    UpdateRow(opsMonteCarloReps2, PreviousOptionStr(opsMonteCarloReps2));
  Dec(IsPreventChangeEventCount);
end;

procedure TAnalysisPrefDlg.AddSubsModelRows;
var
  aValue: String;

  procedure AddOpsNucAminoType2ProcNonDist;
  begin
    if IsNuc and (not IsCoding) and (PreviousOptionStr(opsNucSynAminoType2) = AminoStr) then
      RemovePreviousOption(opsNucSynAminoType2);
    if IsAmino and (PreviousOptionStr(opsNucSynAminoType2) <> AminoStr) then
      RemovePreviousOption(opsNucSynAminoType2);
    if IsNuc and IsCoding then
    begin
      if (FOperation = dtdoMLIQTree) or (FOperation = dtdoMLModelTestIQTree) then
        AddPickListSetting(opsNucSynAminoType2, NucAminoCodonPickList, 'NucAminoCodonPickList')
      else
        AddPickListSetting(opsNucSynAminoType2, NucAminoPickList, 'NucAminoPickList');
    end
    else if IsNuc then
      AddStringSetting(opsNucSynAminoType2, NucStr, True, True)
    else if IsAmino then
      AddStringSetting(opsNucSynAminoType2, AminoStr, True, True);
  end;

begin
  if FIsDist then
    Exit;

  if IsJustDistCompute(FOperation) or IsDistWithTreeCompute(FOperation) then
  begin
    AddStringSetting(opsModelInfo1,opsPlus, True);
    if IsNuc and IsCoding then
    begin
      AddPickListSetting(opsNucSynAminoType2, NucSynAminoPickList, 'NucSynAminoPickList');
      AddGeneticCodeRow;
      SetupSubsModelPickList;
      AddFloatSetting(opsFixedRValue2, True);
      AddStringSetting(opsSubsToInclude2, EmptyStr);
     end
    else if IsNuc then
    begin
      AddStringSetting(opsNucSynAminoType2, NucStr, True, True);
      AddGeneticCodeRow;
      SetupSubsModelPickList;
      AddStringSetting(opsSubsToInclude2);
    end
    else if IsAmino then
    begin
      AddStringSetting(opsNucSynAminoType2, AminoStr, True, True);
      AddGeneticCodeRow;
      SetupSubsModelPickList;
    end;
    AddStringSetting(opsRatesPatterns1, opsPlus, True);
    AddPickListSetting(opsRatesAmongSites2, RateAmongSitesPickList, 'RateAmongSitesPickList');
    AddFloatSetting(opsGammaPara2, True, False);
    AddPickListSetting(opsPatternAmongLineages2, PatternAmongLineagePickList, 'PatternAmongLineagePickList');
  end
  else if FOperation = dtdoMLCodonOmega then
  begin
    AddStringSetting(opsModelInfo1,opsPlus, True);
    AddStringSetting(opsNucSynAminoType2, SynStr, True, True);
    AddGeneticCodeRow;  // I thought that the genetic code wasn't applicable to Syn-NonSyn analyses.
  end
  else if FOperation = dtdoMLTsTvBias then
  begin
    AddStringSetting(opsModelInfo1,opsPlus, True);
    AddStringSetting(opsNucSynAminoType2, NucStr, True, True);
    AddGeneticCodeRow;
    SetupSubsModelPickList;
    AddStringSetting(opsRatesPatterns1, opsPlus, True);
    AddPickListSetting(opsRatesAmongSites2, RateAmongSitesGammaPickList, 'RateAmongSitesGammaPickList');
    AddIntegerSetting(opsGammaCats2, True, False);
  end
  else if FOperation = dtdoMLIQTree then
  begin
    AddOpsNucAminoType2ProcNonDist;
    AddGeneticCodeRow;
  end
  else if FOperation = dtdoBEAM then
  begin
    AddStringSetting(opsModelInfo1,opsPlus, True);
    AddStringSetting(opsNucSynAminoType2, SnvStr, True, True);
    AddStringSetting(opsRatesPatterns1, opsPlus, True);
    AddStringSetting(opsRatesAmongSites2, GammaRatePickStr, True, True);
    AddIntegerSetting(opsGammaCats2, False, True);
  end
  else if IsMLCompute(FOperation) then
  begin
    AddStringSetting(opsModelInfo1,opsPlus, True);
    AddOpsNucAminoType2ProcNonDist;
    AddGeneticCodeRow;
    if (FOperation <> dtdoMLModelTest) and (FOperation <> dtdoMLModelTamer) and (FOperation <> dtdoMLModelTestIQTree) then
    begin
      SetupSubsModelPickList;
      AddStringSetting(opsRatesPatterns1, opsPlus, True);
      if (FOperation = dtdoMLGammRates) or (FOperation = dtdoMLInferSiteBySiteRates) then
        AddPickListSetting(opsRatesAmongSites2, RateAmongSitesNoUniformPickList, 'RateAmongSitesNoUniformPickList')
      else
        AddPickListSetting(opsRatesAmongSites2, RateAmongSitesGammaPickList, 'RateAmongSitesGammaPickList');
      AddIntegerSetting(opsGammaCats2, True, False);
      if FOperation = dtdoMLComputeUserTreeBLens then
      begin
        AddPickListSetting(opsAssumeMolClock2, MolClockPickList, 'MolClockPickList');
        AddStringSetting(opsGlobalClockLevel2, GlobalClockStr, True, True);
      end;
    end;
  end
  else if IsMPCompute(FOperation) then
  begin
    AddStringSetting(opsModelInfo1,opsPlus, True);
    AddOpsNucAminoType2ProcNonDist;
    AddGeneticCodeRow;
  end
  else
  begin
    case FOperation of
       dtdoSelectionExactTest,
       dtdoSelectionZTest           :
         begin
           AddStringSetting(opsModelInfo1,opsPlus, True);
           AddStringSetting(opsNucSynAminoType2,SynStr, True, True);
           SetupSubsModelPickList;
           //AddGeneticCodeRow;
           AddFloatSetting(opsFixedRValue2, True);
         end;
       dtdoTajimaNeutralityTest:
         begin
           AddStringSetting(opsModelInfo1, opsPlus, True);
           AddOpsNucAminoType2ProcNonDist;
         end;
       dtdoTajimaClockTest          :
         begin
           AddStringSetting(opsModelInfo1, opsPlus, True);
           AddOpsNucAminoType2ProcNonDist;
           AddGeneticCodeRow;
           aValue := GetSetting(opsNucSynAminoType2).StringValue;
           if aValue = NucStr then
             AddPickListSetting(opsSubsToInclude2, TajimaClockTsTvPickList, 'TajimaClockTsTvPickList')
           else
             AddPickListSetting(opsSubsToInclude2, AllStr, 'AllStr');
         end;
       dtdoSiteCoverage,
       dtdoCompositionDistance,
       dtdoDisparityIndex,
       dtdoDisparityIndexTest       :
         begin
           AddStringSetting(opsModelInfo1,opsPlus, True);
           AddOpsNucAminoType2ProcNonDist;
           AddGeneticCodeRow;
          end;
      dtdoMCLComputePattern,
      dtdoMCLTsTvBias:
        begin
          AddStringSetting(opsModelInfo1,opsPlus, True);
          AddStringSetting(opsNucSynAminoType2, NucStr, True, True);
          AddGeneticCodeRow;
          SetupSubsModelPickList;
        end;
    end;
  end;
end;

procedure TAnalysisPrefDlg.UpdateModelRows(ACategory: String);
var
  ARow: Integer = -1;
  ProcRow: Integer = -1;
  AValue: String;
begin
  if not ( (ACategory = opsNucSynAminoType2) or
           (ACategory = opsSubsModel2) or
           (ACategory = opsSubsToInclude2) ) then
    Exit;
  UpdateCurrentOptionsStringList;
  FDistPack.Clear;
  FDistPack.AddOperationTypeAndScope(FOperation, FOptionsStringList, FIsCoding);
  FDistPack.AddPrimaryModelType(FOptionsStringList, FIsCoding);


  if not HasSetting(ACategory, ARow) then
    Exit;
  AValue := GetSetting(ACategory).StringValue;
  if Length(AValue) = 0 then
    Exit;

  Inc(IsPreventChangeEventCount);

  if ACategory = opsNucSynAminoType2 then
  begin
    UpdateGeneticCodeRow;
    if SetupSubsModelPickList then  // we do this only if we are not in model selection
    begin
      ACategory := opsSubsModel2;
    end
    else
      ACategory := EmptyStr;
  end;

  if (ACategory = opsSubsModel2) and HasSetting(opsFixedRValue2, ProcRow) then
  begin
    if (AValue = Model_ModifiedNeiGojoboriPDistStr) or (AValue = Model_ModifiedNeiGojoboriJCDistStr) then
      GetSetting(opsFixedRValue2).SetState(False, True)
    else
      GetSetting(opsFixedRValue2).SetState(True, False);
  end;

  FDistPack.ConstructPack(FOperation, FOptionsStringList, FIsCoding);
  UpdateModelDependentRows;

  if HasSetting(opsAssumeMolClock2, ProcRow) then
  begin
    UpdateRow(opsAssumeMolClock2, PreviousOptionStr(opsAssumeMolClock2));
    UpdateClockRows;
  end;

  Dec(IsPreventChangeEventCount);
end;

procedure TAnalysisPrefDlg.UpdateVarEstimationRow;
var
  ARow: Integer = -1;
begin
  if HasSetting(opsEstimateVar2, ARow) then
  begin
    if (FOperation = dtdoRelTimeML) or (FOperation = dtdoCorrTestML) or (FOperation = dtdoRtdtML) then
      Exit;  // because we update it in UpdateClockRows

    if not FDistPack.CanComputeVar then
    begin
      UpdateRow(opsEstimateVar2, NotApplicableStr);
      if HasSetting(opsBootReps2, ARow) then
        UpdateRow(opsBootReps2, NotApplicableStr);
    end
    else
    begin
      if (FOperation = dtdoSelectionZTest) then
      begin
        if FDistPack.DoesContain(gdPairwise) and FDistPack.CanComputeAnalyticalVar then
        begin
          UpdateRow(opsEstimateVar2, AnalTestStr);
          GetSetting(opsBootReps2).IsReadOnly := True;
          GetSetting(opsBootReps2).IsApplicable := False;
        end
        else
        begin
          UpdateRow(opsEstimateVar2, BootTestStr);
          GetSetting(opsBootReps2).IsReadOnly := False;
          GetSetting(opsBootReps2).IsApplicable := True;
        end;
      end
      else
      begin
        if (FOperation = dtdoPairwiseDist) and FDistPack.CanComputeAnalyticalVar then
         UpdatePicklist(opsEstimateVar2, StdErrBootAnalPickList, 'StdErrBootAnalPickList')
       else
         UpdatePicklist(opsEstimateVar2, StdErrBootPickList, 'StdErrBootPickList');
       UpdateRow(opsEstimateVar2, PreviousOptionStr(opsEstimateVar2));
      end;

      if HasSetting(opsEstimateVar2) and (IsCurrentSetting(opsEstimateVar2, NoneStr) or IsCurrentSetting(opsEstimateVar2, AnalTestStr)) then
      begin
        if HasSetting(opsBootReps2) then
        begin
          GetSetting(opsBootReps2).SetState(True, False);
        end;
      end
      else if HasSetting(opsEstimateVar2) and HasSetting(opsBootReps2) then
        GetSetting(opsBootReps2).SetState(False, True);
      //if HasSetting(opsBootReps2, ARow) then
      //begin
      //  if IsCurrentSetting(opsEstimateVar2, BootTestStr) then
      //  begin
      //    //UpdateRow(opsBootReps2, EmptyStr);
      //    UpdateRow(opsBootReps2, PreviousOptionStr(opsBootReps2))
      //  end
      //  else
      //    UpdateRow(opsBootReps2, NotApplicableStr);
      //end;
    end;
  end;
end;

procedure TAnalysisPrefDlg.UpdateModelDependentRows;
var
  prevStr: String;
  aSetting: TAnalysisSetting = nil;
  ARow: Integer = -1;
begin
  //this depends on the calculation type
   { handle substitutions to include}
   if SetupSubsToIncludePickList then
   begin
     prevStr := PreviousOptionStr(opsSubsToInclude2);
     UpdateRow(opsSubsToInclude2, prevStr);
   end;

  { handle rates among sites}
  if HasSetting(opsRatesAmongSites2, ARow) and (FOperation <> dtdoBEAM) then
  begin
    if FDistPack.CanCorrectForGammaRatesPlusInv(FOperation) then
    begin
      if (FOperation = dtdoMLGammRates) or (FOperation = dtdoMLInferSiteBySiteRates) then
        UpdatePicklist(opsRatesAmongSites2, RateAmongSitesNoUniformPickList, 'RateAmongSitesNoUniformPickList')
      else
        UpdatePicklist(opsRatesAmongSites2, RateAmongSitesGammaPickList, 'RateAmongSitesGammaPickList');
      UpdateRow(opsRatesAmongSites2, PreviousOptionStr(opsRatesAmongSites2));
    end
    else if FDistPack.CanCorrectForOnlyGammaRates then
    begin
      UpdatePicklist(opsRatesAmongSites2, RateAmongSitesPickList, 'RateAmongSitesPickList');
      UpdateRow(opsRatesAmongSites2, PreviousOptionStr(opsRatesAmongSites2));
    end
    else
      UpdatePicklist(opsRatesAmongSites2, RateAmongSitesPickList1, 'RateAmongSitesPickList1');

    //turn on/off the dependent row
    if HasSetting(opsGammaPara2, ARow) then
    begin
      if IsCurrentSetting(opsRatesAmongSites2, UniformRatePickStr) or
         IsCurrentSetting(opsRatesAmongSites2, InvariantPickStr) or
         IsCurrentSetting(opsNucSynAminoType2, SynStr) or
         IsCurrentSetting(opsSubsModel2, Model_LogDet_TKStr) or
         IsCurrentSetting(opsSubsModel2, Model_NoOfDiffStr) or
         IsCurrentSetting(opsSubsModel2, Model_p_distanceStr) then
         begin
           UpdateRow(opsGammaPara2,NotApplicableStr);
           if IsCurrentSetting(opsSubsModel2, Model_LogDet_TKStr) or IsCurrentSetting(opsSubsModel2, Model_NoOfDiffStr) or IsCurrentSetting(opsSubsModel2, Model_p_distanceStr) or IsCurrentSetting(opsNucSynAminoType2, SynStr) then
             UpdatePickList(opsRatesAmongSites2, RateAmonSitesUniformOnlyPickList, 'RateAmonSitesUniformOnlyPickList');
         end
         else
         begin
           GetSetting(opsGammaPara2).IsReadOnly := False;
           UpdateRow(opsGammaPara2, PreviousOptionStr(opsGammaPara2));
         end;
    end;

    //turn on/off the dependent row
    if HasSetting(opsGammaCats2, ARow) then
    begin
      if (IsCurrentSetting(opsRatesAmongSites2, UniformRatePickStr)) or
         (IsCurrentSetting(opsRatesAmongSites2, InvariantPickStr)) then
         begin
           UpdateRow(opsGammaCats2,NotApplicableStr);
         end
         else
         begin
           aSetting := GetSetting(opsGammaCats2);
           aSetting.IsReadOnly := False;
           aSetting.IsApplicable := True;
           prevStr := PreviousOptionStr(opsGammaCats2);
           if prevStr <> NotApplicableStr then
             UpdateRow(opsGammaCats2, prevStr)
           else
             UpdateRow(opsGammaCats2, GetDefaultOptions(opsGammaCats2));
         end;
    end;
  end;

  { handle patterns among lineages}
  if HasSetting(opsPatternAmongLineages2, ARow) then
  begin
    if FDistPack.CanCorrectForPatternHeterogeneity then
    begin
      UpdatePickList(opsPatternAmongLineages2, PatternAmongLineagePickList, 'PatternAmongLineagePickList');
      UpdateRow(opsPatternAmongLineages2, PreviousOptionStr(opsPatternAmongLineages2));
    end
    else
      UpdatePickList(opsPatternAmongLineages2, HomoPatternPickStr, 'HomoPatternPickStr');
  end;

  { handle the transition/transversion ration (RValue)}
  if HasSetting(opsFixedRValue2, ARow) then
  begin
    if FDistPack.NeedsFixedRValue then
    begin
      GetSetting(opsFixedRValue2).SetState(False, True);
    end
    else
    begin
      GetSetting(opsFixedRValue2).SetState(True, False);
    end;
  end;

  { handle codon positions}
  if HasSetting(opsCodonPosPanel2, ARow) then
  begin
    if not IsCurrentSetting(opsNucSynAminoType2, NucStr) then
    begin
      GetSetting(opsCodonPosPanel2).SetState(True, False);
      UpdateRow(opsCodonPosPanel2, NotApplicableStr)
    end
    else
    begin
      GetSetting(opsCodonPosPanel2).SetState(False, True);
      UpdateRow(opsCodonPosPanel2, '1st,2nd,3rd,Non-Coding');
    end;
  end;
  UpdateVarEstimationRow;
end;

function TAnalysisPrefDlg.SetupSubsModelPickList: Boolean;
var
  AValue: String;
  ARow: Integer = -1;
begin
  Result := False;

  if not HasSetting(opsNucSynAminoType2, ARow) then
    Exit;
  AValue := GetSetting(opsNucSynAminoType2).StringValue;

  Inc(IsPreventChangeEventCount);

  if IsJustDistCompute(FOperation) or IsDistWithTreeCompute(FOperation) then
  begin
    if  AValue = NucStr then
    begin
      if HasSetting(opsSubsModel2, ARow) then
        UpdatePicklist(opsSubsModel2, DistNucPickList, 'DistNucPickList')
      else
        AddPickListSetting(opsSubsModel2, DistNucPickList, 'DistNucPickList');
    end
    else if AValue = SynStr then
    begin
      if HasSetting(opsSubsModel2, ARow) then
        UpdatePicklist(opsSubsModel2, DistSynNonsynPickList, 'DistSynNonsynPickList')
      else
        AddPickListSetting(opsSubsModel2, DistSynNonsynPickList, 'DistSynNonsynPickList');
    end
    else if AValue = AminoStr then
    begin
      if HasSetting(opsSubsModel2, ARow) then
        UpdatePicklist(opsSubsModel2, DistAminoPickList, 'DistAminoPickList')
      else
        AddPickListSetting(opsSubsModel2, DistAminoPickList, 'DistAminoPickList');
    end;
  end
  else if IsDistSelectionCompute(FOperation) then
  begin
    if FOperation = dtdoSelectionExactTest then
    begin
      if HasSetting(opsSubsModel2, ARow) then
        UpdatePicklist(opsSubsModel2, DistFisherSelTestModelPickList, 'DistFisherSelTestModelPickList')
      else
        AddPickListSetting(opsSubsModel2, DistFisherSelTestModelPickList, 'DistFisherSelTestModelPickList');
    end
    else if FOperation = dtdoSelectionZTest then
    begin
      if HasSetting(opsSubsModel2, ARow) then
        UpdatePicklist(opsSubsModel2, DistSelTestModelPickList, 'DistSelTestModelPickList')
      else
        AddPickListSetting(opsSubsModel2, DistSelTestModelPickList, 'DistSelTestModelPickList');
    end;
  end
  else if (FOperation = dtdoMCLComputePattern) or (FOperation = dtdoMCLTsTvBias) then
  begin
    if HasSetting(opsSubsModel2, ARow) then
      UpdateRow(opsSubsModel2, Model_Tamura_Nei_ParaStr)
    else
      AddStringSetting(opsSubsModel2, Model_Tamura_Nei_ParaStr, True);
  end
  else if (FOperation = dtdoBEAM) then
  begin
    AddStringSetting(opsSubsModel2, Model_Jukes_CantorStr, True, True);
  end
  else if IsMLCompute(FOperation) and (FOperation <> dtdoMLModelTest) and(FOperation <> dtdoMLModelTamer) and (FOperation <> dtdoMLIQTree) and (FOperation <> dtdoMLModelTestIQTree) then
  begin
    if AValue  = NucStr then
    begin
      if FOperation = dtdoMLTsTvBias then
      begin
        if HasSetting(opsSubsModel2, ARow) then
          UpdatePicklist(opsSubsModel2, MLNucTsTvPickList, 'MLNucTsTvPickList')
        else
          AddPickListSetting(opsSubsModel2, MLNucTsTvPickList, 'MLNucTsTvPickList');
      end
      else
      begin
        if HasSetting(opsSubsModel2, ARow) then
          UpdatePicklist(opsSubsModel2, MLNucPickList, 'MLNucPickList')
        else
          AddPickListSetting(opsSubsModel2, MLNucPickList, 'MLNucPickList');
      end;
    end
    else if AValue  = AminoStr then
    begin
      if HasSetting(opsSubsModel2, ARow) then
        UpdatePicklist(opsSubsModel2, MLAminoPickList, 'MLAminoPickList')
      else
        AddPickListSetting(opsSubsModel2, MLAminoPickList, 'MLAminoPickList');
    end;
  end;
  Dec(IsPreventChangeEventCount);
  Result := True;
end;

function TAnalysisPrefDlg.AddSubsModelPicklist: Boolean;
var
  AValue: String;
  ARow: Integer = -1;
begin
  Result := False;
  if not HasSetting(opsNucSynAminoType2, ARow) then
    Exit;

  AValue := GetSetting(opsNucSynAminoType2).StringValue;
  if Length(AValue) = 0 then
    Exit;

  Inc(IsPreventChangeEventCount);

  if IsJustDistCompute(FOperation) or IsDistWithTreeCompute(FOperation) then
  begin
    if  AValue = NucStr then
    begin
      AddPickListSetting(opsSubsModel2, DistNucPickList, 'DistNucPickList');
    end
    else if AValue = SynStr then
    begin
      UpdateRow(opsSubsModel2, DistSynNonsynPickList,'DistSynNonsynPickList');
    end
    else if AValue = AminoStr then
    begin
      UpdateRow(opsSubsModel2, DistAminoPickList, 'DistAminoPickList');
    end;
  end
  else if IsDistSelectionCompute(FOperation) then
  begin
    if FOperation = dtdoSelectionExactTest then
      UpdateRow(opsSubsModel2, DistFisherSelTestModelPickList, 'DistFisherSelTestModelPickList')
    else if FOperation = dtdoSelectionZTest then
      UpdateRow(opsSubsModel2, DistSelTestModelPickList, 'DistSelTestModelPickList');
  end
  else if (FOperation = dtdoMCLComputePattern) or (FOperation = dtdoMCLTsTvBias) then
  begin
    UpdatePicklist(opsSubsModel2, Model_Tamura_Nei_ParaStr, 'Model_Tamura_Nei_ParaStr'); // Moved Model_Tamura_Nei_ParaStr -> ''
  end
  else if FOperation = dtdoMLCodonOmega then
  begin
    UpdateRow(opsSubsModel2, MLHyPhyPickList, 'MLHyPhyPickList');
  end
  else if IsMLCompute(FOperation) then
  begin
    if AValue  = NucStr then
    begin
      if FOperation = dtdoMLTsTvBias then
        UpdateRow(opsSubsModel2, MLNucTsTvPickList,  'MLNucTsTvPickList')
      else
        UpdateRow(opsSubsModel2, MLNucPickList,  'MLNucPickList');
    end
    else if AValue  = AminoStr then
    begin
      UpdateRow(opsSubsModel2, MLAminoPickList,'MLAminoPickList');
    end;
  end;

  Dec(IsPreventChangeEventCount);
  Result := True;
end;

function TAnalysisPrefDlg.SetupSubsToIncludePickList: Boolean;
var
  ASetting: TAnalysisSetting;
  AValue: String;
  ARow: Integer = -1;
begin
  Result := False;
  if not HasSetting(opsSubsToInclude2, ARow) then
    Exit;

  ASetting := GetSetting(opsNucSynAminoType2);
  AValue := ASetting.StringValue;
  if Length(AValue) = 0 then
    Exit;

  Inc(IsPreventChangeEventCount);

  if FOperation = dtdoTajimaClockTest then
  begin
    if AValue = NucStr then
      UpdatePickList(opsSubsToInclude2, TajimaClockTsTvPickList, 'TajimaClockTsTvPickList')
    else if AValue = AminoStr then
      UpdateRow(opsSubsToInclude2, NotApplicableStr);
  end
  else if IsJustDistCompute(FOperation) then
  begin
    if AValue = NucStr then
    begin
      if FDistPack.DoesContain(gdAlleleFreqDist) then
        UpdatePickList(opsSubsToInclude2, AlleleFreqDistPickList, 'AlleleFreqDistPickList')
      else if FDistPack.CanSeparateTsTv then
        UpdatePickList(opsSubsToInclude2, TsTvPickList, 'TsTvPickList')
      else
        UpdatePickList(opsSubsToInclude2, AllStr, 'AllStr');
    end
    else if AValue = SynStr then
    begin
      if FDistPack.IsNeiGojoboriType then
      begin
        if DistPack.DoesContain(gdNoOfDiff) then
          UpdatePickList(opsSubsToInclude2, NG_NoOfDiffPickList, 'NG_NoOfDiffPickList')
        else
          UpdatePickList(opsSubsToInclude2, NGPickList, 'NGPickList');
      end
      else
        UpdatePicklist(opsSubsToInclude2, LwlPickList, 'LwlPickList');
    end
    else if AValue = AminoStr then
    begin
       if FDistPack.DoesContain(gdAlleleFreqDist)  then
         UpdatePicklist(opsSubsToInclude2, AlleleFreqDistPickList, 'AlleleFreqDistPickList')
       else
         UpdatePicklist(opsSubsToInclude2, AllStr, 'AllStr');
    end;
  end
  else if IsDistWithTreeCompute(FOperation) then
  begin
    if AValue = NucStr then
    begin
      if FDistPack.DoesContain(gdAlleleFreqDist) then
        UpdatePicklist(opsSubsToInclude2, AlleleFreqDistPickList, 'AlleleFreqDistPickList')
      else if FDistPack.CanSeparateTsTv then
        UpdatePicklist(opsSubsToInclude2, TsTvTreePickList, 'TsTvTreePickList')
      else
        UpdatePickList(opsSubsToInclude2, AllStr, 'AllStr');
    end
    else if AValue = SynStr then
    begin
      if FDistPack.IsNeiGojoboriType then
        UpdatePicklist(opsSubsToInclude2, NGTreePickList, 'NGTreePickList')
      else
        UpdatePicklist(opsSubsToInclude2, LwlTreePickList, 'LwlTreePickList');
    end
    else if AValue = AminoStr then
    begin
      if FDistPack.DoesContain(gdAlleleFreqDist) then
        UpdatePicklist(opsSubsToInclude2, AlleleFreqDistPickList, 'AlleleFreqDistPickList')
      else
        UpdatePicklist(opsSubsToInclude2, AllStr, 'AllStr');
    end;
  end;

  Dec(IsPreventChangeEventCount);
  Result := True;
end;

procedure TAnalysisPrefDlg.AddDataSubsetRows;
begin
  if FIsDist or (FOperation = dtdoRelTimeBLens) or (FOperation = dtdoCorrTestBlens) or (FOperation = dtdoRtdtBLens) then
    Exit;
  AddStringSetting(opsDataSubset1, opsPlus, True);

  if FOperation = dtdoSiteCoverage then
  begin
    AddStringSetting(opsGapMissingHandle2, UseAllStr, True, True);
    if FIsNuc and FIsCoding then
    begin
      AddStringSetting(opsCodonPosPanel2, '1st,2nd,3rd,Non-Coding', True, True);
      SelectCodonPosPanel.Enabled := False;
    end;
  end
  else if IsJustDistCompute(FOperation) or
     IsDistWithTreeCompute(FOperation) or
    (FOperation = dtdoMCLComputePattern) or
    (FOperation = dtdoMCLTsTvBias) then
  begin
    AddPickListSetting(opsGapMissingHandle2, CompPairwisePartialDelPickList, 'CompPairwisePartialDelPickList');
    AddIntegerSetting(opsSiteCoverage2, True, False);
    if (IsNuc and IsCoding) then
      AddStringSetting(opsCodonPosPanel2, EmptyStr);
  end
  else if IsDistSelectionCompute(FOperation) then
  begin
    AddPickListSetting(opsGapMissingHandle2, CompPairwisePartialDelPickList, 'CompPairwisePartialDelPickList');
    AddIntegerSetting(opsSiteCoverage2, True, False);
  end
  else if (FOperation = dtdoTajimaNeutralityTest) then
  begin
    AddPickListSetting(opsGapMissingHandle2, CompPairwisePartialDelPickList, 'CompPairwisePartialDelPickList');
    AddIntegerSetting(opsSiteCoverage2, True, False);
    if (IsNuc and IsCoding) then
      AddStringSetting(opsCodonPosPanel2, NotApplicableStr, True, False);
  end
  else if (FOperation = dtdoTajimaClockTest) then
  begin
    AddPickListSetting(opsGapMissingHandle2, CompleteDeletionPickList, 'CompleteDeletionPickList');
    if (IsNuc and IsCoding) then
      AddStringSetting(opsCodonPosPanel2, NotApplicableStr, True, False);
  end
  else if (FOperation = dtdoMLCodonOmega) then
  begin
    AddPickListSetting(opsGapMissingHandle2, CompAllPartialDelPickList, 'CompAllPartialDelPickList');
    AddIntegerSetting(opsSiteCoverage2, True);
  end
  else if FOperation = dtdoBEAM then
  begin
    AddStringSetting(opsGapMissingHandle2, UseAllStr, True, True);
  end
  else if IsMLCompute(FOperation) or IsMPCompute(FOperation) or IsDisparityCompute(FOperation) then
  begin
    if (FOperation = dtdoMLInferAncSeqMyPeg) or (FOperation = dtdoMPInferAncSeqMyPeg) then
      AddPickListSetting(opsGapMissingHandle2, UseAllSitesPickList, 'UseAllSitesPickList')
    else
      AddPickListSetting(opsGapMissingHandle2, CompAllPartialDelPickList, 'CompAllPartialDelPickList');
    AddIntegerSetting(opsSiteCoverage2, True);
    if (IsNuc and IsCoding) then
      AddStringSetting(opsCodonPosPanel2, NotApplicableStr, True, False);
    if IsMLCompute(FOperation) and IsDeveloper and (not GetIsBootstrapSubSampling) then
      AddStringSetting(opsDataOrientation, 'false', False, True);
  end;

  if FHasSiteLabelTypes and (ChkLBx.Items.Count>0) then
  begin
    AddPickListSetting(opsLabelledSites2, LabelledSitesPickList, 'LabelledSitesPickList');
    AddStringSetting(opsSelectLabelsPanel2, NotApplicableStr, True, False);
  end;

  if IsReleaseRecoding and IsAmino and IsDeveloper then
    AddPickListSetting(opsRecodeBases2, RecodeBasesPickList, 'RecodeBasesPickList'); //Mapping (e.g., Atomic tree of life)
end;

procedure TAnalysisPrefDlg.UpdateDataSubsetRows;
var
  ARow: Integer = -1;
  ASetting: TAnalysisSetting = nil;
begin
  if HasSetting(opsGapMissingHandle2, ARow) then
  begin
    if HasSetting(opsSiteCoverage2, ARow) then
    begin
      ASetting := GetSetting(opsSiteCoverage2);
      if IsCurrentSetting(opsGapMissingHandle2, PartialDelStr) then
      begin
        ASetting.IsReadOnly := False;
        ASetting.IsApplicable := True;
      end
      else
      begin
        ASetting.IsReadOnly := True;
        ASetting.IsApplicable := False;
      end;
    end;
  end;

  { codon positions}
  if HasSetting(opsCodonPosPanel2, ARow) then
  begin
    SelectCodonPosPanel.Visible := True;
    PositionEllipsisPanel(SelectCodonPosPanel);
  end;

  { labelled sites}
  if HasSetting(opsLabelledSites2, ARow) then
  begin
  //  UpdateRow(opsLabelledSites2, PreviousOptionStr(opsLabelledSites2));
  //  for i:=0 to ChkLBx.Count-1 do
  //  begin
  //     AInfo := GetPrevOption(opsSelectLabelsPanel2, ChkLBx.Items[i]);
  //     if (AInfo = NotApplicableStr) or (Length(AInfo) = 0) then
  //        AInfo := 'True';
  //     ChkLBx.Checked[i] := StrToBool(AInfo);
  //  end;

    aSetting := GetSetting(opsSelectLabelsPanel2);
    if Assigned(aSetting) then
    begin
      aSetting.SetState(GetPickListSettingValue(opsLabelledSites2) = OnlyLabeledSitesStr, True);
      aSetting.IsVisible := True;
      ChkLBx.Enabled := (GetPickListSettingValue(opsLabelledSites2) = OnlyLabeledSitesStr);
    end;
    PositionEllipsisPanel(SelectLabelsPanel);
    if not IsCurrentSetting(opsLabelledSites2, OnlyLabeledSitesStr) then
    begin
      SelectLabelsPanel.Hide;
      UpdateRow(opsSelectLabelsPanel2, NotApplicableStr);
    end
    else
    begin
      UpdateRow(opsSelectLabelsPanel2, EmptyStr);
      SelectLabelsPanel.Visible := True;
      GetSetting(opsSelectLabelsPanel2).IsApplicable := True;
      GetSetting(opsSelectLabelsPanel2).IsReadOnly := False;
    end;
  end;

  if HasSetting(opsRecodeBases2, ARow) then
    UpdateRow(opsRecodeBases2, PreviousOptionStr(opsRecodeBases2));
end;

procedure TAnalysisPrefDlg.AddGeneticCodeRow;
var
  PickListSetting: TPickListAnalysisSetting = nil;
  GeneticCodeList: String;
  GeneticCodeListValue: String;
begin
  if IsNuc and IsCoding then
  begin
	  if (FOperation = dtdoMLCodonOmega) then
	  begin
	    PickListSetting := TPickListAnalysisSetting.Create(1, 'Genetic_Code');
	    PickListSetting.Name := 'Genetic Code';
	    PickListSetting.DisplayName := 'Genetic Code';
	    PickListSetting.Description := 'Genetic code for translating cDNA to amino acids';
	    PickListSetting.SetState(False, True);
	    GeneticCodeList := GeneticCodeTableCodonOmegaList;
	    GeneticCodeList := StringReplace(GeneticCodeList, '"', '', [rfReplaceAll]);
	    PickListSetting.PickList := GeneticCodeList;
	    GeneticCodeListValue := PickListSetting.Value;
	    AddPickListSetting(opsGeneticCodeTable2, GeneticCodeTableCodonOmegaList, 'GeneticCodeTableCodonOmegaList');
	    if IsCurrentSetting(opsNucSynAminoType2, NucStr) then
	    begin
	      GetSetting(opsGeneticCodeTable2).IsReadOnly := True;
	      GetSetting(opsGeneticCodeTable2).IsApplicable := False;
	    end;
	  end
	  else
	  begin
	    AddPickListSetting(opsGeneticCodeTable2, GeneticCodeTablePickList, 'GeneticCodeTablePickList');
	    if IsCurrentSetting(opsNucSynAminoType2, NucStr) then
	    begin
	      GetSetting(opsGeneticCodeTable2).IsReadOnly := True;
	      GetSetting(opsGeneticCodeTable2).IsApplicable := False;
	    end;
	  end;
    if Assigned(PickListSetting) then
      PickListSetting.Free;
  end;
end;

procedure TAnalysisPrefDlg.UpdateGeneticCodeRow;
var
  ARow: Integer = -1;
  ASetting: TAnalysisSetting;
begin
  if HasSetting(opsGeneticCodeTable2, ARow) then
  begin
    if HasSetting(opsNucSynAminoType2, ARow) then
    begin
      ASetting := GetSetting(opsGeneticCodeTable2);
      if (IsCurrentSetting(opsNucSynAminoType2, AminoStr)) or (IsCurrentSetting(opsNucSynAminoType2, SynStr)) or (FOperation = dtdoMLCodonOmega) then // enabled genetic code row for Syn-nonSyn row.
      begin
        ASetting.IsReadOnly := False;
        ASetting.IsApplicable := True;
      end
      else
      begin
        ASetting.IsReadOnly := True;
        ASetting.IsApplicable := False;
      end;
    end;
    if (FOperation = dtdoMLCodonOmega) then
    begin
      ASetting := GetSetting(opsGeneticCodeTable2);
      if (FOperation = dtdoMLCodonOmega) then // enabled genetic code row for Syn-nonSyn row.
      begin
        ASetting.IsReadOnly := False;
        ASetting.IsApplicable := True;
      end
      else
      begin
        ASetting.IsReadOnly := True;
        ASetting.IsApplicable := False;
      end;
    end;
  end;
end;

procedure TAnalysisPrefDlg.AddTreeSearchRows;
begin
  case FOperation of
    dtdoMETree:
      begin
        AddStringSetting(opsTreeMethod1, opsPlus, True);
        AddStringSetting(opsMESearchMethod2,CNIStr, True, True);
        AddStringSetting(opsMEInitialTrees2,ObtainInitialByNJStr, True, True);
        AddIntegerSetting(opsMESearchLevelPanel2, False);
      end;
    dtdoMPTree:
      begin
        AddStringSetting(opsTreeMethod1, opsPlus, True);
        AddPickListSetting(opsMPSearchMethod2,MPSearchPickList, 'MPSearchPickList'); // bb or mini or CNI
        AddIntegerSetting(opsMPRandomAddTreesPanel2);
        AddIntegerSetting(opsMPSearchLevelPanel2, False);
        AddIntegerSetting(opsMaxTrees2, False);
        //if IsPrototyper then { turned off per Sudhir's request}
        //  AddPickListSetting(opsCalcBLens2, YesNoPickList, 'CalcBLensPickList');

      end;
    dtdoMLTree, dtdoLbsInference:
      begin
        AddStringSetting(opsTreeMethod1, opsPlus, True);
        AddPickListSetting(opsMLSearchMethod2,MLSearchPickList, 'MLSearchPickList');
        if isPrototyper then
          AddPickListSetting(opsMLInitialTrees2,MLInitialTreesPickListPrototyper,'MLInitialTreesPickList')
        else
          AddPickListSetting(opsMLInitialTrees2,MLInitialTreesPickList,'MLInitialTreesPickList');
        if FOperation = dtdoMLTree then
          AddIntegerSetting(opsNumInitialTrees2, True, True, 'Number of random addition parsimony trees to search');
        if not isPrototyper then
          AddStringSetting(opsPickStartTree2, NotApplicableStr, True, False);
      end;
  end;

  // Case statement covers Branch Length Filters
  case FOperation of
    dtdoMLModelTest, dtdoMLModelTamer,
    dtdoMLTree, dtdoLbsInference,
    dtdoMLComputePattern, // For 4x4, R, gamma, inv, calculations
    dtdoMLTsTvBias,
    dtdoMLGammRates,
    dtdoMLInferSiteBySiteRates:
      begin
        AddPickListSetting(opsSearchFilter2, SearchFilterPickList, 'SearchFilterPickList');
      end;
  end;
end;

procedure TAnalysisPrefDlg.UpdateTreeSearchRows;
var
  ARow: Integer = -1;
  aSetting: TAnalysisSetting;
begin
  if HasSetting(opsMESearchMethod2, ARow) then        // ME
  begin
    UpdateRow(opsMESearchMethod2, PreviousOptionStr(opsMESearchMethod2));
    UpdateRow(opsMESearchLevelPanel2, PreviousOptionStr(opsMESearchLevelPanel2));
  end
  else if HasSetting(opsMPSearchMethod2, ARow) then   // MP
  begin
    if IsCurrentSetting(opsMPSearchMethod2, MaxMiniStr) then // branch and bound
    begin
      UpdateRow(opsMPSearchLevelPanel2, NotApplicableStr);
      UpdateRow(opsMPRandomAddTreesPanel2, NotApplicableStr);
    end
    else if IsCurrentSetting(opsMPSearchMethod2, MinMiniStr) then // min-min heuristic
    begin
      UpdateRow(opsMPSearchLevelPanel2, PreviousOptionStr(opsMPSearchLevelPanel2));
      UpdateRow(opsMPRandomAddTreesPanel2, NotApplicableStr);
    end
    else if IsCurrentSetting(opsMPSearchMethod2, RandomAdditionTreesStr) then
    begin
      UpdateRow(opsMPSearchLevelPanel2, PreviousOptionStr(opsMPSearchLevelPanel2));
      UpdateRow(opsMPRandomAddTreesPanel2, PreviousOptionStr(opsMPRandomAddTreesPanel2));
    end
    else if (IsCurrentSetting(opsMPSearchMethod2, SPRStr)) or (IsCurrentSetting(opsMPSearchMethod2, TBRStr)) then
    begin
      UpdateRow(opsMPSearchLevelPanel2, PreviousOptionStr(opsMPSearchLevelPanel2));
      UpdateRow(opsMPRandomAddTreesPanel2, PreviousOptionStr(opsMPRandomAddTreesPanel2));
      aSetting := GetSetting(opsMPSearchLevelPanel2);
      aSetting.IsReadOnly := False;
      aSetting.IsApplicable := True;
      aSetting := GetSetting(opsMPRandomAddTreesPanel2);
      aSetting.IsReadOnly := False;
      aSetting.IsApplicable := True;
    end;
    UpdateRow(opsMaxTrees2, PreviousOptionStr(opsMaxTrees2));
  end
  else if HasSetting(opsMLSearchMethod2, ARow) then   // ML
  begin
    UpdateRow(opsMLInitialTrees2, PreviousOptionStr(opsMLInitialTrees2));
    UpdateMLInitialTreeRow;
  end;

  if HasSetting(opsSearchFilter2, ARow) then // Branch length filter (ML)
    UpdateRow(opsSearchFilter2, PreviousOptionStr(opsSearchFilter2));
end;

procedure TAnalysisPrefDlg.UpdateMLInitialTreeRow;
begin
  if IsCurrentSetting(opsMLInitialTrees2, UserSpecifyFromFile) then
  begin
    if not IsPrototyper then
    begin
      if FileExists(PreviousOptionStr(opsPickStartTree2)) then
        UpdateRow(opsPickStartTree2, PreviousOptionStr(opsPickStartTree2))
      else
        UpdateRow(opsPickStartTree2, EmptyStr);
      GetSetting(opsPickStartTree2).SetState(False, True);
    end;
  end
  else if IsCurrentSetting(opsMLInitialTrees2, InitialParsimonyTreesStr) then
    GetSetting(opsNumInitialTrees2).SetState(False, True)
  else if not IsPrototyper then
    UpdateRow(opsPickStartTree2, NotApplicableStr);

  if not IsCurrentSetting(opsMLInitialTrees2, InitialParsimonyTreesStr) then
    GetSetting(opsNumInitialTrees2).SetState(True, False)
end;

procedure TAnalysisPrefDlg.AddClockRows;
begin
  case FOperation of
    dtdoMLClockTest, dtdoMLClockTestLocal, dtdoRelTimeML, dtdoRelTimeBLens,
    dtdoRelTimeLS, dtdoCorrTestBlens, dtdoCorrTestML, dtdoEpML, dtdoRtdtML,
    dtdoRtdtLS, dtdoRtdtBlens: AddStringSetting(opsClockSettings1, opsPlus, True);
  end;

  case FOperation of
    dtdoMLClockTest:
    begin
      AddStringSetting(opsClockType2, GlobalClockStr, True, True);
    end;
    dtdoRelTimeML:
    begin
      AddStringSetting(opsClockType2, RelativeRatesStr, True, True);
      if EnableSubsamplingAnalyses then
      begin
        AddPickListSetting(opsEstimateVar2, StdErrLbsAnalPickList, 'StdErrLbsAnalPickList', 'Node height variance can be estimated using either the analytical method or using the Little Bootstraps sub-sampling method');
        AddIntegerSetting(opsBootstrapSubSamples, False, True, 'Defines the maximum number of sub-samples that will be used. The starting number of sub-samples is 5');
        AddIntegerSetting(opsBootstrapRepsPerSample, False, True, 'Defines the maximum number of replicates to use for each sub-sample. The starting number of replicates is 10');
        AddFloatSetting(opsPercentSitesPerSample, IsPrototyper, not IsPrototyper, 'Defines the percentage of sites to sample (without replacement) for each sub-sample. Alternatively, you can use "' + opsBootstrapSampleSizeFactor + '" instead');
        if IsDeveloper then
        begin
          AddIntegerSetting(opsBootstrapMaxSitesPerSample, IsPrototyper, not IsPrototyper, 'Defines the maximum number of sites to sample (without replacement) for each sub-sample as the number of sites is increased dynamically');
          AddFloatSetting(opsBootstrapSampleSizeFactor, False, True, 'Specifies sub-sample size as a fractional power of the total number of sites. Must be a value between 0.5 and 1.0 (inclusive). Using this option, sub-sample size is calculated as ceiling(alignment_length^' + opsBootstrapSampleSizeFactor + ').');
          AddPickListSetting(opsLbsTargetPrecisionStdDev, TargetPrecStdDevPickList, 'TargetPrecStdDevPickList', 'Little Bootstraps will check if the standard deviation of computed precision is greater than 2 times the value specified and increase the number of replicates per sub-sample if so');
          AddPickListSetting(opsLbsSkipTreeSwaps, YesNoPickList, 'YesNoPickList', 'Speed up the calculation by skipping the step to compare replicate trees within a sub-sample for best fit to data');
        end;
        AddPickListSetting(opsSubsamplingVerifyResult, YesNoFastSlowPickList, 'YesNoFastSlowPickList', 'After the calculation is complete, double the number of sites used and recalculate results');
        if IsDeveloper then
          UpdateMaxSitesPerSample;
      end
      else
      begin
        AddStringSetting(opsEstimateVar2, AnalTestStr, True, True);
        AddStringSetting(opsPhylo2, 'Maximum Likelihood', True, True);
      end;

      if OperationSupportsSubsamplingDoubleCheck(FOperation) then
        AddPickListSetting(opsSubsamplingVerifyResult, YesNoFastSlowPickList, 'YesNoFastSlowPickList', 'After the calculation is complete, double the number of sites used and recalculate results');
    end;
    dtdoRtdtML:
    begin
      AddStringSetting(opsClockType2, RelativeRatesStr, True, True);
      AddStringSetting(opsEstimateVar2, AnalTestStr, True, True);
    end;
    dtdoCorrTestML:
    begin
      AddStringSetting(opsClockType2, RRFrameworkStr, True, True);
    end;
    dtdoRelTimeLS, dtdoRtdtLS:
    begin
      AddStringSetting(opsClockType2, RelativeRatesStr, True, True);
    end;
  end;
  if IsDeveloper or IsPrototyper then
    AddIntegerSetting(opsMaxRateRatio2, False, True);
end;

procedure TAnalysisPrefDlg.UpdateClockRows;
var
  ARow: Integer = -1;
begin
    if HasSetting(opsClockLevel2, ARow) then
      UpdateRow(opsClockLevel2, PreviousOptionStr(opsClockLevel2));

    if HasSetting(opsGlobalClockLevel2, ARow) then
    begin
      if IsCurrentSetting(opsAssumeMolClock2, NoStr) then
        GetSetting(opsGlobalClockLevel2).SetState(True, False)
      else
      begin
        if not (FOperation = dtdoMLComputeUserTreeBLens) then
          UpdateRow(opsGlobalClockLevel2, PreviousOptionStr(opsGlobalClockLevel2))
        else
          GetSetting(opsGlobalClockLevel2).SetState(True, True);
      end;
    end;

    if HasSetting(opsEstimateVar2, ARow) then // opsEstimateVar2 = 'Variance Estimation Method'
    begin
      if (FOperation = dtdoRelTimeML) or (FOperation = dtdoCorrTestML) or (FOperation = dtdoRtdtML) then
      begin
        UpdateRow(opsEstimateVar2, PreviousOptionStr(opsEstimateVar2));
        if IsCurrentSetting(opsEstimateVar2, BootTestStr) then // BootTestStr = 'Bootstrap method'
        begin
          if HasSetting(opsBootReps2, ARow) then // opsBootReps2 = 'No. of Bootstrap Replications'
          begin
            UpdateRow(opsEstimateVar2, AnalTestStr);
            UpdateRow(opsBootReps2, PreviousOptionStr(opsBootReps2));
          end;
        end;
        if FOperation = dtdoRelTimeML then
          SetStateForLittleBootstrapParams;
      end;
    end;
    if IsDeveloper or IsPrototyper then
      if HasSetting(opsMaxRateRatio2, ARow) then
        UpdateRow(opsMaxRateRatio2, PreviousOptionStr(opsMaxRateRatio2));
end;

procedure TAnalysisPrefDlg.AddSystemResourceUsageRows;
begin
  case FOperation of
    dtdoMLTree, dtdoBEAM,
    dtdoMLComputeUserTreeBLens,
    dtdoMLClockTestLocal, dtdoMLInferAncSeq, dtdoMLInferAncSeqMyPeg,
    dtdoRelTimeML, dtdoRtdtML, dtdoMLComputePattern,
    dtdoCorrTestML, dtdoMLGammRates, dtdoMLTsTvBias, dtdoMLInferSiteBySiteRates,
    dtdoMLModelTest, dtdoMLModelTamer, dtdoEpML,
    dtdoMLClockTest, dtdoLbsTiming, dtdoLbsInference, dtdoLbsAnalyzeTree:
      begin
        AddStringSetting(opsSystemResourceUsage, opsPlus, True);
        if (GetNoOfProcessors < 2) or (MegaForm.NumSingleThreadedMLAnalyses > 0) then
          AddIntegerSetting(opsMLNumThreads, True) // if the user has only one processor or we only want them using one, don't give a spin edit for setting the number of threads
        else
          AddIntegerSetting(opsMLNumThreads, False);
      end;
  end;
  case FOperation of
    dtdoNJTree, dtdoMETree, dtdoMPTree, dtdoUPGMATree:
      begin
        AddStringSetting(opsSystemResourceUsage, opsPlus, True);
        if HasSetting(opsBootReps2) and GetSetting(opsBootReps2).IsApplicable then
        begin
          if (GetNoOfProcessors < 2) then
            AddIntegerSetting(opsMLNumThreads, True, False)
          else
            AddIntegerSetting(opsMLNumThreads, False);
        end
        else
          AddIntegerSetting(opsMLNumThreads, True, False);
      end;
  end;
  if IsPrototyper then
    if FOperation = dtdoMPTree then
      AddIntegerSetting(opsMaxExecutionTime2, False);
end;

procedure TAnalysisPrefDlg.UpdateSystemResourceUsageRows;
var
  ARow: integer = 0;
  aSetting: TAnalysisSetting = nil;
begin
    if HasSetting(opsMLNumThreads, ARow) then
    begin
      aSetting := GetSetting(opsMLNumThreads);
      if (GetNoOfProcessors = 1) and (not isPrototyper) then
      begin
        aSetting.IsReadOnly := True;
        UpdateRow(opsMLNumThreads, NotApplicableStr);
      end
      else if (IsMLCompute(FOperation) and (MegaForm.NumSingleThreadedMLAnalyses > 0)) or MegaForm.MultithreadedCalcIsRunning then
      begin
        aSetting.IsReadOnly := True;
        UpdateRow(opsMLNumThreads, MultithreadNotAvailableStr);
      end
      else
      begin
        if HasSetting(opsBootReps2) then
        begin
          if GetSetting(opsBootReps2).IsApplicable or IsMLCompute(FOperation) and (FOperation <> dtdoMLIQTree) then
          begin
            aSetting.IsReadOnly := False;
            aSetting.IsApplicable := True;
          end
          else
            aSetting.IsReadOnly := True;
        end
        else
        begin
          //GetSetting(opsMLNumThreads).IsReadOnly := False;
          //GetSetting(opsMLNumThreads).IsApplicable := False;
        end;
        if aSetting.IsApplicable and (not aSetting.IsReadOnly) then
          UpdateRow(opsMLNumThreads, PreviousOptionStr(opsMLNumThreads));
      end;
      if HasSetting(opsSystemResourceUsage) then
        GetSetting(opsSystemResourceUsage).IsApplicable := aSetting.IsApplicable;
    end;
    if IsPrototyper then
    begin
      if HasSetting(opsMaxExecutionTime2, ARow) then
        PositionEllipsisPanel(MaxExecutionTimePanel);
      MaxExecutionTimeEdit.Enabled := MaxRuntimeCheckbox.Checked;
    end;


end;

procedure TAnalysisPrefDlg.AddBeamOptions;
var
  strSetting: TStringAnalysisSetting = nil;
begin
  if FOperation <> dtdoBEAM then
    Exit;
  strSetting := TStringAnalysisSetting.Create(GetIndentLevelForName(opsBeamSettings));
  strSetting.Update(opsBeamSettings, opsPlus);
  strSetting.IsReadOnly := True;
  AddRow(strSetting);
  AddFloatSetting(opsImputeCutoff2, False, True);
end;

procedure TAnalysisPrefDlg.UpdateBeamOptions;
begin
  if Foperation <> dtdoBEAM then
    Exit;
  if HasSetting(opsImputeCutoff2) then
  begin
    if PreviousOption(opsImputeCutoff2, stFloat) <> nil then
      UpdateRow(opsImputeCutoff2, PreviousOption(opsImputeCutoff2, stFloat).StringValue);
  end;
end;

procedure TAnalysisPrefDlg.StoreOption(AName, AValue: String; Suffix: String = '');
var
  ASetting: TAnalysisSetting;
  test: Boolean;
begin
  Assert(FPreviousOptions.Count > 0);
  ASetting := PreviousOption(AName);
  test := (ASetting.IsReadOnly or ASetting.SetValueFromStr(AValue));
  Assert(test);
end;

procedure TAnalysisPrefDlg.StoreOption(aSetting: TAnalysisSetting);
var
  prevOpt: TAnalysisSetting = nil;
  debug: String = '';
begin
  debug := aSetting.Name;
  prevOpt := PreviousOption(aSetting.Name);
  if Assigned(prevOpt) and (prevOpt.SettingType = aSetting.SettingType) then
  begin
    case prevOpt.SettingType of
      stInteger: TIntegerAnalysisSetting(prevOpt).Assign(TIntegerAnalysisSetting(aSetting));
      stFloat: TFloatAnalysisSetting(prevOpt).Assign(TFloatAnalysisSetting(aSetting));
      stString: TStringAnalysisSetting(prevOpt).Assign(TStringAnalysisSetting(aSetting));
      stPicklist:
        begin
          TPickListAnalysisSetting(prevOpt).Assign(TPickListAnalysisSetting(aSetting));
          UpdatePickListValue(TPickListAnalysisSetting(aSetting));
        end;
      stPickListValue: TPickListValueSetting(prevOpt).Assign(TPickListValueSetting(aSetting));
    end;
  end
  else
  begin
    if Assigned(prevOpt) then
      RemovePreviousOption(aSetting.Name);
    prevOpt := ASetting.GetClone;
    FPreviousOptions.Add(prevOpt);
  end;
end;

procedure TAnalysisPrefDlg.StoreAllOptions;
var
  i: Integer;
  aSetting: TAnalysisSetting =  nil;
begin
  if FCurrentOptions.Count > 0 then
    for i := 0 to FCurrentOptions.Count - 1 do
    begin
      aSetting := TAnalysisSetting(FCurrentOptions[i]);
      StoreOption(aSetting);
    end;
end;

function TAnalysisPrefDlg.GetDefaultOptions(AName: String; PickListName: String=''): String;
var
  aNumSites: Integer = -1;
begin
  Result := EmptyStr;
  if      AName = opsCodonPosPanel2       then Result := 'True' // include all positions by default
  else if AName = opsSelectLabelsPanel2   then Result := 'True' // select all lables by default
  else if AName = opsSiteCoverage2        then Result := '95'
  else if AName = opsGammaPara2           then Result := FloatToStr(1.0)
  else if AName = opsGammaCats2           then Result := '5'
  else if AName = opsBootReps2            then Result := '500'
  else if AName = opsMonteCarloReps2      then Result := '500'
  else if AName = opsFixedRValue2         then Result := FloatToStr(2.0)
  else if AName = opsMinMiniSearchFactor2 then Result := '0'
  else if AName = opsMESearchLevelPanel2  then Result := '1'
  else if AName = opsMPSearchLevelPanel2  then Result := '1'
  else if AName = opsMPRandomAddTreesPanel2 then Result := '10'
  else if AName = opsMaxTrees2              then Result := '100'
  else if AName = opsTreeToUse2             then Result := ClickToPickTreeStr
  else if AName = opsPickStartTree2         then Result := ClickToPickTreeStr
  else if AName = opsMaxExecutionTime2      then Result := '-1'
  else if AName = opsMaxRateRatio2          then Result := Format('%.0f', [DEFAULT_MAX_RATE_RATIO])
  else if AName = opsMLNumThreads           then Result := IntToStr(GetDefaultNoOfProcessors(FOperation))
  else if AName = opsBootstrapSampleSizeFactor then Result := '0.9'
  else if AName = opsBootstrapSubSamples then Result := '10'
  else if AName = opsBootstrapRepsPerSample then Result := '20'
  else if AName = opsBootstrapMaxSitesPerSample then Result := IntToStr(NumSitesBeforeSubsetting)
  else if AName = opsLbsTargetPrecisionStdDev then Result := '2.5'
  else if AName = opsSubsamplingVerifyResult then Result := NoFastStr
  else if AName = opsLbsSkipTreeSwaps then Result := 'No'
  else if AName = opsApproachType2 then
  begin
    if PickListName = opsModelSelectionApproachPickList then
      Result := opsModelTamerFast2
    else if PickListName = opsSubsampleUpsamplePickList then
      Result := opsSubsampling2
    else
      Result := opsStandard2

  end
  else if AName = opsPercentSitesPerSample then
    begin
      aNumSites := D_InputSeqData.NoOfSites;
      if aNumSites > 1000000 then
        Result := '0.1'
      else if aNumSites > 100000 then
        Result := '1.0'
      else if aNumSites > 10000 then
        Result := '10.0'
      else if aNumSites > 5000 then
        Result := '20.0'
      else
        Result := '50.0';
    end
  else if (AName = opsSubsModel2) then
  begin
    if       PickListName = 'DistNucPickList'       then Result := Model_MCLStr
    else if  PickListName = 'DistAminoPickList'     then Result := Model_PoissonStr
    else if  PickListName = 'MLNucPickList'         then Result := Model_Tamura_Nei_ParaStr
    else if  PickListName = 'DistSynNonsynPickList' then Result := Model_NeiGojoboriJCDistStr
    else if  PickListName = 'DistFisherSelTestModelPickList' then Result := Model_NeiGojoboriPDistStr
    else if  PickListName = 'DistSynNonsynPickList' then Result := Model_NeiGojoboriJCDistStr
    else if  PickListName = 'MLAminoPickList'       then Result := Model_JTTStr;
  end
  else if AName = opsGapMissingHandle2 then
  begin
    if PickListName = 'CompPairwisePartialDelPickList' then Result := PairwiseDelStr
    else if PickListName = 'CompAllPartialDelPickList' then Result := UseAllStr;
  end
  else if AName = opsImputeCutoff2 then
    Result := '2'
  else if AName = opsNumInitialTrees2 then
    Result := '10'
  else if AName = opsDeltaAicCutoff then
    Result := Format('%d', [DEFAULT_AICC_THRESHOLD])
  else if AName = opsDeltaBicCutoff then
    Result := Format('%d', [DEFAULT_BIC_THRESHOLD])
  else if AName = opsBootstrapPrecisionThreshold then
    Result := IntToStr(DEFAULT_ADAPTIVE_BOOTSTRAP_TARGET_PRECISION);
end;

function TAnalysisPrefDlg.HasDefaultOption(AName: String; PickListName: String=''): Boolean;
begin
  Result := (GetDefaultOptions(AName, PickListName) <> EmptyStr);
end;

function TAnalysisPrefDlg.CurrentOption(AName: String): TAnalysisSetting;
var
  i: Integer;
begin
  Result := nil;
  if FCurrentOptions.Count = 0 then
    Exit;
  for i := 0 to FCurrentOptions.Count - 1 do
  begin
    if TAnalysisSetting(FCurrentOptions[i]).Name = AName then
    begin
      Result := TAnalysisSetting(FCurrentOptions[i]);
      Exit;
    end;
  end;
end;

function TAnalysisPrefDlg.CurrentOptionIsInArray(optionName: String; aArray: TArrayOfString): Boolean;
var
  aOptionValue: String = '';
  i: Integer = -1;
begin
  Result := False;
  aOptionValue := CurrentOptionStr(optionName);
  if Length(aArray) > 0 then
  begin
    for i := Low(aArray) to High(aArray) do
      if aOptionValue = aArray[i] then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

function TAnalysisPrefDlg.CurrentOptionStr(AName: String): String;
var
  i: Integer;
begin
  Result := EmptyStr;
  if FCurrentOptions.Count = 0 then
    Exit;
  for i := 0 to FCurrentOptions.Count - 1 do
  begin
    if TAnalysisSetting(FCurrentOptions[i]).Name = AName then
    begin
      Result := TAnalysisSetting(FCurrentOptions[i]).StringValue;
      Exit;
    end;
  end;
end;

function TAnalysisPrefDlg.PreviousOption(AName: String; aType: TSettingType = stNone): TAnalysisSetting;
var
  i: Integer;
  prevOption: TAnalysisSetting = nil;
begin
  Result := nil;
  if FPreviousOptions.Count = 0 then
    Exit;
  if IsBlacklistedFromUsingPrevOp(AName) then
    Exit;

  for i := 0 to FPreviousOptions.Count - 1 do
  begin
    if AName = TAnalysisSetting(FPreviousOptions[i]).Name then
    begin
      if (aType <> stNone) and (aType <> TAnalysisSetting(FPreviousOptions[i]).SettingType) then
        Continue;
      Result := TAnalysisSetting(FPreviousOptions[i]);
      if Result.SettingType = stPicklist then
      begin
        prevOption := PreviousOption(TPickListAnalysisSetting(Result).PickListName);
        if Assigned(prevOption) and (prevOption.SettingType = stPickListValue) and (TPickListAnalysisSetting(Result).PickListName = TPickListValueSetting(prevOption).Value) then
          TPickListAnalysisSetting(Result).Value := TPickListValueSetting(prevOption).Value
        else
          continue;
      end;
      Exit;
    end;
  end;
end;

function TAnalysisPrefDlg.PreviousOptionStr(AName: String): String;
var
  i: Integer;
  aSetting: TAnalysisSetting = nil;
  prevOption: TAnalysisSetting = nil;
begin
  Result := EmptyStr;
  if FPreviousOptions.Count = 0 then
    Exit;
  Assert(NumOccurrencesInPrevOps(AName) <= 1);
  for i := 0 to FPreviousOptions.Count - 1 do
  begin
    aSetting := TAnalysisSetting(FPreviousOptions[i]);
    if AName = aSetting.Name then
    begin
      if aSetting.SettingType = stPicklist then
      begin
        prevOption := PreviousOption(TPickListAnalysisSetting(aSetting).PickListName);
        if Assigned(prevOption) and (prevOption.SettingType = stPickListValue) and (TPickListAnalysisSetting(aSetting).PickListName = TPickListValueSetting(prevOption).Name) and (TPickListAnalysisSetting(aSetting).HasValue(TPickListValueSetting(prevOption).Value)) then
          TPickListAnalysisSetting(aSetting).Value := TPickListValueSetting(prevOption).Value
        else
          continue;
      end;
      Result := TAnalysisSetting(FPreviousOptions[i]).StoredValueString;
      Exit;
    end;
  end;
end;

function TAnalysisPrefDlg.NumOccurrencesInPrevOps(AName: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  if FPreviousOptions.Count = 0 then
    Exit;
  for i := 0 to FPreviousOptions.Count - 1 do
    if AName = TAnalysisSetting(FPreviousOptions[i]).Name then
      inc(Result);
end;

function TAnalysisPrefDlg.IsBlacklistedFromUsingPrevOp(settingName: String): Boolean;
begin
  if (settingName = opsBootstrapMaxSitesPerSample) or
     (settingName = opsFocalSpecies2) then
    Result := True
  else
    Result := False;
end;

procedure TAnalysisPrefDlg.RemovePreviousOption(aName: String);
var
  i: Integer;
  aOption: TAnalysisSetting;
begin
  if FPreviousOptions.Count = 0 then
    Exit;
  for i := 0 to FPreviousOptions.Count - 1 do
  begin
    if AName = TAnalysisSetting(FPreviousOptions[i]).Name then
    begin
      aOption := TAnalysisSetting(FPreviousOptions[i]);
      FPreviousOptions.Delete(i);
      aOption.Free;
      Exit;
    end;
  end;
end;

function TAnalysisPrefDlg.CheckPreviousPickListOptionValid(aName: String; aPickList: String): Boolean;
var
  aSetting: TAnalysisSetting = nil;
  debug: String = '';
begin
  Result := True;
  aSetting := PreviousOption(aName);
  if Assigned(aSetting) then
  begin
    if aSetting is TPickListAnalysisSetting then
    begin
      debug := aSetting.StringValue;
      if Pos(aPickList, aSetting.StringValue) <= 0 then
      begin
        RemovePreviousOption(aName);
        Result := False;
      end;
    end
    else
    begin
      RemovePreviousOption(aName);
      Result := False;
    end;
  end;
end;

procedure TAnalysisPrefDlg.ShowActiveEllipsisPanels;
var
  ARow: Integer = -1;
begin
  SelectCodonPosPanel.Visible := HasSetting(opsCodonPosPanel2, ARow) and (GetSetting(opsCodonPosPanel2).IsApplicable);
  if SelectCodonPosPanel.Visible then
    PositionEllipsisPanel(SelectCodonPosPanel)
  else
    SelectCodonPosPanel.Top := Self.Top - 50;

  SelectLabelsPanel.Visible := (HasSetting(opsLabelledSites2, ARow));
  if SelectLabelsPanel.Visible then
    PositionEllipsisPanel(SelectLabelsPanel);
  MaxExecutionTimePanel.Visible := HasSetting(opsMaxExecutionTime2, ARow);
  if MaxExecutionTimePanel.Visible then
    PositionEllipsisPanel(MaxExecutionTimePanel);

  TransposeDataPanel.Visible := HasSetting(opsDataOrientation, ARow);
  if TransposeDataPanel.Visible then
    PositionEllipsisPanel(TransposeDataPanel);

  NumSamplesPanel.Visible := HasSetting(opsBootstrapSubSamples, ARow) and GetSetting(opsBootstrapSubSamples).IsApplicable;
  if NumSamplesPanel.Visible then
    PositionEllipsisPanel(NumSamplesPanel);

  NumReplicatesPanel.Visible := HasSetting(opsBootstrapRepsPerSample, ARow) and GetSetting(opsBootstrapRepsPerSample).IsApplicable;
  if NumReplicatesPanel.Visible then
    PositionEllipsisPanel(NumReplicatesPanel);

  PercentSitesPanel.Visible := HasSetting(opsPercentSitesPerSample, ARow) and GetSetting(opsPercentSitesPerSample).IsApplicable;
  if PercentSitesPanel.Visible then
    PositionEllipsisPanel(PercentSitesPanel)
end;

procedure TAnalysisPrefDlg.PositionEllipsisPanel(APanel: TPanel);
var
  ARow: Integer;
  ARect: TRect;
begin
  ARow := GetCustomPanelPosition(APanel);
  if ARow >= 0 then
  begin
    ARect := OptionsChosenGrid.CellRect(1, ARow);
    APanel.Left := ARect.Left;
    APanel.Top  := ARect.Top;
    APanel.Width  := OptionsChosenGrid.ColWidths[1];
    APanel.Height := OptionsChosenGrid.RowHeights[ARow];
    if ScrollbarVisible then
    begin
      if OptionsChosenGrid.Col = 0 then
        APanel.Width := APanel.Width - 12;
    end;
  end;
end;

function TAnalysisPrefDlg.GetCustomPanelPosition(APanel: TPanel): Integer;
var
  i: Integer;
  AName: String;
begin
  Result := -1;
  AName := GetCustomPanelRowName(APanel);
  if FEnabledOptions.Count > 0 then
  begin
    for i := 0 to FEnabledOptions.Count - 1 do
    begin
      if TAnalysisSetting(FEnabledOptions[i]).Name = AName then
      begin
        Result := i + 1;
        break;
      end;
    end;
  end;
end;

function TAnalysisPrefDlg.GetCustomPanelRowName(APanel: TPanel): String;
begin
  if APanel = MaxExecutionTimePanel then
    Result := opsMaxExecutionTime2
  else if APanel = SelectCodonPosPanel then
    Result := opsCodonPosPanel2
  else if APanel = SelectLabelsPanel then
    Result := opsSelectLabelsPanel2
  else if APanel = TransposeDataPanel then
    Result := opsDataOrientation
  else if APanel = NumSamplesPanel then
    Result := opsBootstrapSubSamples
  else if APanel = NumReplicatesPanel then
    Result := opsBootstrapRepsPerSample
  else if APanel = PercentSitesPanel then
    Result := opsPercentSitesPerSample
  else
    Assert(False, 'missing custom panel handler');
end;

function TAnalysisPrefDlg.IsTreeFileRow(ARow: Integer): Boolean;
var
  sRKey: String;
begin
  Result := False;
  sRKey:= TAnalysisSetting(FEnabledOptions[aRow - 1]).Name;
  if sRKey = opsTreeToUse2 then
    Result := True;
end;

procedure TAnalysisPrefDlg.UpdateTreeFileEdit(ARow: Integer);
begin
  with TreeFileNameEdit do
  begin
    Filter := NewickFilesFilter;
    if FileExists(PreviousOptionStr(opsPickStartTree2)) then
    begin
      FileName := PreviousOptionStr(opsPickStartTree2);
      InitialDir := ExtractFileDir(Filename);
    end
    else
    begin
      InitialDir := GetCurrentDir;
    end;
  end;
end;

function TAnalysisPrefDlg.isSpinRow(sRKey: String): Boolean;
begin
  result := false;
  if (sRKey = opsGammaPara2) or
  (sRKey = opsGammaCats2) or
  (sRKey = opsFixedRValue2) or
  (sRKey = opsSiteCoverage2) or
  (sRKey = opsBootReps2) or
  (sRKey = opsMonteCarloReps2) or
  (sRKey = opsMaxTrees2) or
  (sRKey = opsMinMiniSearchFactor2) or
  (sRKey = opsMESearchLevelPanel2) or
  (sRKey = opsMPSearchLevelPanel2) or
  (sRKey = opsMaxTrees2) or
  (sRKey = opsMPRandomAddTreesPanel2) or
  (srKey = opsMaxRateRatio2) or
  (srKey = opsImputeCutoff2) or
  (srKey = opsBootstrapRepsPerSample) or
  (srKey = opsBootstrapSampleSizeFactor) or
  (srKey = opsPercentSitesPerSample) or
  (srKey = opsBootstrapMaxSitesPerSample) or
  (srKey = opsBootstrapSubSamples) or
  (srKey = opsDeltaBicCutoff) or
  (srKey = opsDeltaAicCutoff) or
  (srKey = opsBootstrapPrecisionThreshold) or
  (sRKey = opsMLNumThreads) then
    result := true;
end;

function TAnalysisPrefDlg.spinRowIsInteger(sRKey: String): Boolean;
begin
  Result := False;
  if (sRKey = opsGammaCats2) or
   (sRKey = opsSiteCoverage2) or
   (sRKey = opsBootReps2) or
   (sRKey = opsMonteCarloReps2) or
   (sRKey = opsMaxTrees2) or
   (sRKey = opsMinMiniSearchFactor2) or
   (sRKey = opsMESearchLevelPanel2) or
   (sRKey = opsMPSearchLevelPanel2) or
   (sRKey = opsMaxTrees2) or
   (sRKey = opsMPRandomAddTreesPanel2) or
   (srKey = opsMaxRateRatio2) or
   (srKey = opsNumInitialTrees2) or
   (sRKey = opsMLNumThreads) then
     result := true
end;

function TAnalysisPrefDlg.spinRowMin(sRKey: String): double;
begin
  if (sRKey = opsGammaPara2) then
    result := 0.05
  else if (sRKey = opsGammaCats2) then
    result := 2
  else if (sRKey = opsFixedRValue2) then
    result    := 0.05
  else if (sRKey = opsSiteCoverage2) then
    result := 5
  else if (sRKey = opsBootReps2) then
    result := 1
  else if (sRKey = opsMonteCarloReps2) then
    result := 50
  else if (sRKey = opsMaxTrees2) then
    result := 1
  else if (sRKey = opsMinMiniSearchFactor2) then
    result := 0
  else if (sRKey = opsMESearchLevelPanel2) then
    result := 1
  else if (sRKey = opsMPSearchLevelPanel2) then
    result := 1
  else if (sRKey = opsMaxTrees2) then
    result := 1
  else if (sRKey = opsMPRandomAddTreesPanel2) then
    result := 1
  else if (sRKey = opsMaxExecutionTime2) then
    result := -1
  else if (sRKey = opsMLNumThreads) then
    result := 1
  else if srKey = opsMaxRateRatio2 then
    Result := 5
  else if srKey = opsImputeCutoff2 then
    Result := 0.0
  else if srKey = opsMaxSampleSites then
    Result := 1
  else if srKey = opsBootstrapRepsPerSample then
    Result := 1
  else if srKey = opsBootstrapSubSamples then
    Result := 1
  else if srKey = opsBootstrapSampleSizeFactor then
    Result := 0.5
  else if srKey = opsPercentSitesPerSample then
    Result := 0.1
  else if srKey = opsBootstrapMaxSitesPerSample then
    Result := ceil(Power(FNumSitesBeforeSubsetting, 0.5))
  else if srKey = opsLbsTargetPrecisionStdDev then
    Result := 0
  else if srKey = opsNumInitialTrees2 then
    Result := 1
  else if srKey = opsDeltaBicCutoff then
    Result := 1
  else if srKey = opsDeltaAicCutoff then
    Result := 1
  else if srKey = opsBootstrapPrecisionThreshold then
    Result := 0
  else
    result := -9999999;
end;

function TAnalysisPrefDlg.spinRowMax(sRKey: String): double;
var
  aSetting: TAnalysisSetting = nil;
begin
  if (sRKey = opsGammaPara2) then
    result := 999
  else if (sRKey = opsGammaCats2) then
    result := 16
  else if (sRKey = opsFixedRValue2) then
    result := 100
  else if (sRKey = opsSiteCoverage2) then
    result := 100
  else if (sRKey = opsBootReps2) or (sRKey = opsMonteCarloReps2) then
    result := 10000
  else if (sRKey = opsMaxTrees2) then
    result := 10000
  else if (sRKey = opsMinMiniSearchFactor2) then
    result := 100
  else if (sRKey = opsMESearchLevelPanel2) then
    result := 2
  else if (sRKey = opsMPSearchLevelPanel2) then
  begin
    if IsCurrentSetting(opsMPSearchLevelPanel2, SPRStr) or IsCurrentSetting(opsMPSearchLevelPanel2, TBRStr) then
      result := 5
    else
      result := 3;
  end
  else if (sRKey = opsMaxTrees2) then
    result := 10000
  else if (sRKey = opsMPRandomAddTreesPanel2) then
    result := 100000
  else if (sRKey = opsMaxExecutionTime2) then
    result := MaxInt
  else if (sRKey = opsMLNumThreads) then
   begin
     if IsPrototyper then
       result := 1000000
     else
     begin
       Result := GetNoOfProcessors;
       if FOperation in [dtdoEpML, dtdoMLInferAncSeqMyPeg, dtdoMLInferAncSeq,
                         dtdoMLComputePattern, dtdoMLInferSiteBySiteRates,
                         dtdoMLTsTvBias, dtdoMLGammRates, dtdoMLComputeUserTreeBLens,
                         dtdoMLClockTest, dtdoMLClockTestLocal, dtdoMLPredictLivingSeq,
                         dtdoRelTimeML, dtdoRtdtML] then
       begin
         Result := min(Result, 8); { Per Koichiro, for ML without search, use a max of 8 threads}
         aSetting := GetSetting(opsMLNumThreads);
         if Assigned(aSetting) then
           aSetting.Hint := 'For this analysis, the maximum number of threads that can be used is 8.';
       end;
     end;
   end
  else if srKey = opsMaxRateRatio2 then
    Result := 100
  else if srKey = opsImputeCutoff2 then
    Result := 100
  else if srKey = opsPercentSitesPerSample then
    Result := 100.0
  else if srKey = opsBootstrapMaxSitesPerSample then
  begin
    if IsPrototyper then
      Result := MaxInt
    else
      Result := FNumSitesBeforeSubsetting;
  end
  else if srKey = opsBootstrapSampleSizeFactor then
    Result := 1.0
  else if srKey = opsNumInitialTrees2 then
    Result := 100
  else if srKey = opsDeltaBicCutoff then
    Result := MaxInt
  else if srKey = opsDeltaAicCutoff then
    Result := MaxInt
  else if srKey = opsBootstrapPrecisionThreshold then
    Result := 100
  else
    Result := MaxInt;
end;

function TAnalysisPrefDlg.spinRowIncrement(AName: String): Double;
begin
  if (AName = opsMinMiniSearchFactor2) or
     (AName = opsGammaCats2) or
     (AName = opsMESearchLevelPanel2) or
     (AName = opsMPRandomAddTreesPanel2) or
     (AName = opsMLNumThreads) or
     (AName = opsDeltaAicCutoff) or
     (AName = opsDeltaBicCutoff) or
     (AName = opsMPSearchLevelPanel2) then
  begin
    Result := 1
  end
  else if (AName = opsSiteCoverage2) or (AName = opsMaxRateRatio2) then
    Result := 5
  else if (AName = opsMaxTrees2) or (AName = opsBootReps2) or (AName = opsMonteCarloReps2) then
    Result := 100
  else if (AName = opsGammaPara2) then
    Result := 0.1
  else if AName = opsImputeCutoff2 then
    Result := 1
  else if AName = opsMaxSampleSites then
    Result := 100
  else if AName = opsMaxExecutionTime2 then
    Result := 1
  else if AName = opsBootstrapRepsPerSample then
    Result := 1
  else if AName = opsBootstrapSampleSizeFactor then
    Result := 0.1
  else if AName = opsPercentSitesPerSample then
    Result := ceil(Power(FNumSitesBeforeSubsetting, 0.9))
  else if AName = opsBootstrapMaxSitesPerSample then
    Result := ceil(Power(FNumSitesBeforeSubsetting, 0.9))
  else if AName = opsBootstrapSubSamples then
    Result := 1
  else if AName = opsLbsTargetPrecisionStdDev then
    Result := 2.5
  else if AName = opsNumInitialTrees2 then
    Result := 10
  else if AName = opsBootstrapPrecisionThreshold then
    Result := 0.5
  else
    Result := 1;
end;

procedure TAnalysisPrefDlg.SubstTypeChange(Value: String);
var
  ADistComponent: TDistType;
begin
  CurSubstType := gdNone;
  if      Value = ValidCommonSitesPickStr then      CurSubstType := gdCommonSites
  else if Value = NucTsOnlyPickStr        then      CurSubstType := gdNucTsOnly
  else if Value = NucTvOnlyPickStr        then      CurSubstType := gdNucTvOnly
  else if Value = NucRatioTsTvPickStr     then      CurSubstType := gdNucRatioTsTv
  else if Value = SynOnlyPickStr          then      CurSubstType := gdSynOnly
  else if Value = NonsynOnlyPickStr       then      CurSubstType := gdNonsynOnly
  else if Value = NoOfSynSitesPickStr     then      CurSubstType := gdNoOfSynSites
  else if Value = NoOfNonsynSitesPickStr  then      CurSubstType := gdNoOfNonsynSites
  else if Value = DiffSynNonsynPickStr    then      CurSubstType := gdDiffSynNonsyn
  else if Value = DiffNonsynSynPickStr    then      CurSubstType := gdDiffNonsynSyn
  else if Value = Syn4FoldPickStr         then      CurSubstType := gdSyn4Fold
  else if Value = Nonsyn0FoldPickStr      then      CurSubstType := gdNonsyn0Fold
  else if Value = NoOf4FoldSitesPickStr   then      CurSubstType := gdNoOf4FoldSites
  else if Value = NoOf0FoldSitesPickStr   then      CurSubstType := gdNoOf0FoldSites
  else if Value = BhattacharyaPickStr     then      CurSubstType := gdBhattacharyaDist
  else if Value = PrevostiPickStr         then      CurSubstType := gdProvostiDist
  else if Value = RogersPickStr           then      CurSubstType := gdRogersDist
  else if Value = Nei1983PickStr          then      CurSubstType := gdNei1983Dist
  else if Value = KumarGadagkarPickStr    then      CurSubstType := gdKumarGadagkarDist
  else if Value = KumarGadagkarDisparityPickStr    then   CurSubstType := gdKumarGadagkarDisparityDist;

  ADistComponent := FDistPack.DistComponent;
  if ADistComponent = CurSubstType then
    Exit;
  FDistPack.Replace(ADistComponent, CurSubstType);
end;

procedure TAnalysisPrefDlg.AssignHelpContexts;
begin
  case foperation of
    dtdoPairwiseDist,
    dtdoOverallMean,
    dtdoWithinGroupMean,
    dtdoBetweenGroupMean,
    dtdoNetGroupMean,
    dtdoAvgDiversityWithinSubPops,
    dtdoAvgDiversityForEntirePop,
    dtdoInterPopDiversity,
    dtdoPropOfInterPopDiversity:  HelpContext := HC_Dist_Analysis_Option_Dialog;

    dtdoSelectionZTest       : HelpContext := HC_Z_test_Analysis_Options;
    dtdoSelectionExactTest   : HelpContext := HC_Fisher_Exact_Test_Analysis_Options;
    dtdoNJTree, dtdoUPGMATree: HelpContext := HC_NJ_Analysis_Options;
    dtdoMETree               : HelpContext := HC_ME_Analysis_Options;
    dtdoMPTree               : HelpContext := HC_MP_Analysis_Option_Dialog;
    dtdoCompositionDistance,
    dtdoDisparityIndex,
    dtdoDisparityIndexTest   : HelpContext := HC_Disparity_Index_Analysis_Options;
    dtdoMLTree               : HelpContext := HC_ML_Analysis_Options;
    dtdoMLCodonOmega         : HelpContext := HC_CodonOmega_Analysis_Options;
    dtdoTajimaClockTest      : HelpContext := HC_Tajima_Test_Relative_Rate;

    dtdoTajimaNeutralityTest    : HelpContext := HC_Tajima_Test_of_Neutrality;
    dtdoMLClockTest             : HelpContext := HC_ML_Analysis_Options;
    dtdoMLModelTest, dtdoMLModelTamer : HelpContext := HC_ML_Analysis_Options;
    dtdoMCLTsTvBias             : HelpContext := HC_Dist_Analysis_Option_Dialog;
    dtdoMPInferAncSeq           : HelpContext := HC_MP_Analysis_Option_Dialog;
    dtdoMLInferAncSeq           : HelpContext := HC_ML_Analysis_Options;
    dtdoMLInferAncSeqMyPeg      : HelpContext := HC_ML_Analysis_Options;
    dtdoMLTsTvBias              : HelpContext := HC_ML_Analysis_Options;
    dtdoMLGammRates             : HelpContext := HC_ML_Analysis_Options;
    dtdoMLInferSiteBySiteRates  : HelpContext := HC_ML_Analysis_Options;
    dtdoMPComputeUserTreeBLens  : HelpContext := HC_MP_Analysis_Option_Dialog;
    dtdoOLSComputeUserTreeBLens : HelpContext := HC_Dist_Analysis_Option_Dialog;
    dtdoMLComputeUserTreeBLens  : HelpContext := HC_ML_Analysis_Options;
    dtdoRelTimeML, dtdoCorrTestML, dtdoRtdtML : HelpContext := HC_ML_Analysis_Options;
    dtdoRelTimeLS, dtdoRtdtLS               : HelpContext := HC_Dist_Analysis_Option_Dialog;
  else
    HelpContext := Mega_Basics_HC;
  end;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
end;

procedure TAnalysisPrefDlg.SetIsCoding(AOption: Boolean);
begin
  FIsCoding := AOption and FIsNuc;
end;

procedure TAnalysisPrefDlg.SetIsNuc(AOption: Boolean);
begin
  FIsNuc   := AOption;
  FIsAmino := not AOption;
end;

procedure TAnalysisPrefDlg.SetIsAmino(AOption: Boolean);
begin
  FIsNuc   := not AOption;
  FIsAmino := AOption;
end;

procedure TAnalysisPrefDlg.SetIsDist(AOption: Boolean);
begin
  FIsDist := AOption;
  if AOption then
  begin
    FIsNuc   := False;
    FIsAmino := False;
  end;
end;

procedure TAnalysisPrefDlg.SetHasGps(AOption: Boolean);
begin
  FHasGps := AOption;
end;

function TAnalysisPrefDlg.GetIsSiteLabelTypeUsed(Index: Char): Boolean;
var
  IntIndex: Integer;
begin
  IntIndex := ChkLBx.Items.IndexOf(Index);
  Result   := ChkLBx.Checked[IntIndex];
end;

procedure TAnalysisPrefDlg.SetIsSiteLabelTypeUsed(Index: Char; Value: Boolean);
var
  IntIndex: Integer;
begin
  IntIndex := ChkLBx.Items.IndexOf(Index);
  ChkLBx.Checked[IntIndex] := Value;
end;

procedure TAnalysisPrefDlg.SetHasSiteLabelTypes(Value: Boolean);
begin
  FHasSiteLabelTypes := Value;
  if ChkLBx.Items.Count > 0 then
    ChkLBx.Items.Clear;
end;

function TAnalysisPrefDlg.GetIsIncOnlyLabelledSites: Boolean;
var
  ARow: Integer = -1;
begin
  Result := (HasSetting(opsSelectLabelsPanel2, ARow) and IsCurrentSetting(opsLabelledSites2, OnlyLabeledSitesStr));
end;

procedure TAnalysisPrefDlg.SetIsIncOnlyLabelledSites(Value: Boolean);
begin
  If HasSetting(opsSelectLabelsPanel2) then
    GetSetting(opsSelectLabelsPanel2).SetValueFromStr(OnlyLabeledSitesStr);
end;

function TAnalysisPrefDlg.GetIsIncOnlyUNLabelledSites: Boolean;
begin
  Result := HasSetting(opsSelectLabelsPanel2) and IsCurrentSetting(opsLabelledSites2, OnlyUnlabelledSitesStr);
end;

procedure TAnalysisPrefDlg.SetIsIncOnlyUNLabelledSites(Value: Boolean);
begin
  If HasSetting(opsSelectLabelsPanel2) then
    GetSetting(opsSelectLabelsPanel2).SetValueFromStr(OnlyUnlabelledSitesStr);
end;

function TAnalysisPrefDlg.GetIsComputeVar: Boolean;
var
  ARow: Integer = -1;
begin
  if HasSetting(opsTestPhylo2, ARow) then
    Result := GetSetting(opsTestPhylo2).StringValue <> NoneStr
  else  if HasSetting(opsEstimateVar2, ARow) then
  Result := GetSetting(opsEstimateVar2).StringValue <> NoneStr;
end;

function TAnalysisPrefDlg.GetIsAnalyticalVar: Boolean;
var
  ARow: integer = -1;
begin
  if HasSetting(opsEstimateVar2, ARow) then
    Result := (GetSetting(opsEstimateVar2).StringValue = AnalTestStr);
end;

function TAnalysisPrefDlg.GetReltimeVarianceMethod: TReltimeVarianceMethod;
begin
  Result := rvmNone;
  ShowMessage('GetReltimeVarianceMethod not yet implemented');
end;

function TAnalysisPrefDlg.GetBootVarReps: Integer;
var
  ASetting: TIntegerAnalysisSetting;
begin
  Result := 0;
  if HasSetting(opsBootReps2) and GetSetting(opsBootReps2).IsApplicable then
  begin
    ASetting := TIntegerAnalysisSetting(GetSetting(opsBootReps2));
    Result := ASetting.Value;
  end;
end;

function TAnalysisPrefDlg.GetBootVarRandSeed: Integer;
begin
  Result := Random(10000);
end;

function TAnalysisPrefDlg.GetIsCompleteDeletion: Boolean;
begin
  Result := False;
    if HasSetting(opsGapMissingHandle2) then
    Result := (GetSetting(opsGapMissingHandle2).StringValue = CompleteDelStr);
end;

function TAnalysisPrefDlg.GetIsPartialDeletion: Boolean;
var
  AStr: String;
begin
  AStr := GetSetting(opsGapMissingHandle2).StringValue;
  Result :=  (AStr = PartialDelStr);
end;

function TAnalysisPrefDlg.GetIsPairwiseDeletion: Boolean;
begin
  Result := (GetSetting(opsGapMissingHandle2).StringValue = PairwiseDelStr);
end;

function TAnalysisPrefDlg.GetSiteCoverage: Integer;
begin
  Result := -1;
  if IsPartialDeletion then
    Result := StrToIntWithInvalid(GetSetting(opsSiteCoverage2).StringValue);
end;

function TAnalysisPrefDlg.GetIncludeCodonPos: TIncludeCodonPos;
var
  ARow: Integer = -1;
begin
  Result := [];
  if HasSetting(opsCodonPosPanel2, ARow) then
  begin
    if Pos1ChkBx.Checked  then  Result :=  Result + [meg1stSite];
    if Pos2ChkBx.Checked  then  Result :=  Result + [meg2ndSite];
    if Pos3ChkBx.Checked  then  Result :=  Result + [meg3rdSite];
    if NoncodingChkBx.Checked then  Result := Result + [megNoncoding];
  end;
end;

function TAnalysisPrefDlg.GetClockType: Integer;
var
  ARow: Integer = -1;
  rowName: String;
begin
  rowName := EmptyStr;

  // find which row is included (if any)
  if HasSetting(opsClockType2, ARow) then   // Only 1 of these row types will ever be there at any one time.
    rowName := opsClockType2
  else if HasSetting(opsClockLevel2, ARow)then
    rowName := opsClockLevel2
  else if HasSetting(opsGlobalClockLevel2, ARow) then
    rowName := opsGlobalClockLevel2;

  if rowName <> EmptyStr then
  begin
    if (GetSetting(rowName).StringValue = GlobalClockStr) or
       (GetSetting(rowname).StringValue = AllClocksStr) then
      result := 0
    else if GetSetting(rowName).StringValue = ManyClocksStr then   // high stringency clock
      result := 1
    else if GetSetting(rowName).StringValue = FewClocksStr then   // low stringency clock
      result := 2
    else if GetSetting(rowName).StringValue = FewestClocksStr then
      result := 3
    else
      result := -1;
  end
  else
    result := -1;
end;

function TAnalysisPrefDlg.GetTreeReliability: TTreeType;
begin
  Result := ttNone;
end;

function TAnalysisPrefDlg.GetReliabilityTestReps: Integer;
begin
  Result := StrToInt(GetSetting(opsBootReps2).StringValue);
end;

function TAnalysisPrefDlg.GetReliabilityTestRandSeed: Integer;
begin
  Result := Random(10000);
end;

function TAnalysisPrefDlg.GetRateMergeOption: Boolean;
var
  ARow: Integer = -1;
  ChosenOption: String;
begin
  Result := False;
  Exit; { disabled in MEGA7 per Sudhir}
  if HasSetting(opsClockLevel2, ARow) then
  begin
    ChosenOption := GetSetting(opsClockLevel2).StringValue;
    if ChosenOption = AllClocksStr then
      Result := False
    else
      Result := True;
  end
  else
    Assert(False);
end;

function TAnalysisPrefDlg.GetMLSearchFilter: Extended;
var
  filterSetting: String;
  ARow: Integer = -1;
begin
  if HasSetting(opsSearchFilter2, ARow) then
  begin
    filterSetting := GetSetting(opsSearchFilter2).StringValue;
    if filterSetting = NoSearchFilterStr then
      Result := 1.0 + FP_CUTOFF
//    else if filterSetting = VeryStrongStr then
//      result := 0.5
    else if filterSetting = StrongStr then
      result := 0.5
    else if filterSetting = ModerateStr then
      result := 0.7
    else if filterSetting = WeakStr then
      result := 0.9
//    else if filterSetting = VeryWeakStr then
//      result := 0.9
    else
      raise Exception.Create('Invalid Branch length filter selected');
  end
  else
    result := -1;
end;

function TAnalysisPrefDlg.GetIsRecodeBases: Boolean;
var
   ARow: Integer = -1;
begin
  Result := False;
  if  HasSetting(opsRecodeBases2,ARow) then
  begin
    Result := not ((GetSetting(opsRecodeBases2).StringValue = AtomicNoRecodingPickStr) or
                   (GetSetting(opsRecodeBases2).StringValue = NotApplicableStr));
  end;
end;

function TAnalysisPrefDlg.GetRecodeScheme: String;
begin
  Result := EmptyStr;
  if GetIsRecodeBases then
     Result := GetSetting(opsRecodeBases2).StringValue;
end;

function TAnalysisPrefDlg.SelectionWidth: Integer;
var
  ARect: TRect;
begin
  ARect := OptionsChosenGrid.CellRect(1, OptionsChosenGrid.TopRow);
  result := (ARect.Right + 1) - ARect.Left;
end;

procedure TAnalysisPrefDlg.ResizeColumns;
const
  Margin = 10;
var
  i: Integer;
  MaxWidth: Integer;
  AWidth: Integer;
  aSetting: TAnalysisSetting;
  scalingFactor: Double = 1;
begin
  if PixelsPerInch > DesignTimePPI then
    scalingFactor := PixelsPerInch/DesignTimePPI;

  { do the first column}
  MaxWidth := OptionsChosenGrid.Canvas.TextWidth('Option');
  OptionsChosenGrid.Canvas.Font.Style := [fsBold];
  for i := 0 to FCurrentOptions.Count - 1 do
  begin
    if CompareValue(scalingFactor, 1.5, FP_CUTOFF) >= 0 then
      AWidth := OptionsChosenGrid.Canvas.TextWidth(TAnalysisSetting(FCurrentOptions[i]).DisplayName + '        ')
    else
      AWidth := OptionsChosenGrid.Canvas.TextWidth(TAnalysisSetting(FCurrentOptions[i]).DisplayName + '    ');
    if AWidth > MaxWidth then
      MaxWidth := AWidth;
  end;
  OptionsChosenGrid.Canvas.Font.Style := [];
  OptionsChosenGrid.ColWidths[0] := Max(100, MaxWidth + Margin + ArrowImg16.Width);

  { now the second column}
  MaxWidth := OptionsChosenGrid.Canvas.TextWidth('Setting');

  for i := 0 to FEnabledOptions.Count - 1 do
  begin
    aSetting := TAnalysisSetting(FEnabledOptions[i]);
    if (aSetting.Name = opsPickUserTree2) or (aSetting.Name = opsTreeToUse2) then
      continue;
    AWidth := OptionsChosenGrid.Canvas.TextWidth(TAnalysisSetting(FEnabledOptions[i]).StringValue);
    if AWidth > MaxWidth then
      MaxWidth := AWidth;
  end;
  if SelectCodonPosPanel.Visible and (SelectCodonPosPanel.Width > MaxWidth) then
    MaxWidth := SelectCodonPosPanel.Width + Margin;
  OptionsChosenGrid.ColWidths[1] := Max(200, MaxWidth + Margin);
  OptionsChosenGrid.ColWidths[2] := 4;
end;

procedure TAnalysisPrefDlg.AdjustColWidths;
var
  SumWidths: Integer = 0;
  WidthDiff: Integer = 0;
begin
  SumWidths := OptionsChosenGrid.ColWidths[0] + OptionsChosenGrid.ColWidths[2];
  WidthDiff := OptionsChosenGrid.Width - SumWidths;
  OptionsChosenGrid.ColWidths[1] := Max(50, WidthDiff);
  if TreeFileNameEdit.Visible then
    TreeFileNameEdit.Width := OptionsChosenGrid.ColWidths[1];
  if PercentSitesPanel.Visible then
    PercentSitesPanel.Width := OptionsChosenGrid.ColWidths[1];
  if NumSamplesPanel.Visible then
    NumSamplesPanel.Width := OptionsChosenGrid.ColWidths[1];
  if NumReplicatesPanel.Visible then
    NumReplicatesPanel.Width := OptionsChosenGrid.ColWidths[1];
  if SelectCodonPosPanel.Visible then
    SelectCodonPosPanel.Width := OptionsChosenGrid.ColWidths[1];
  if MaxExecutionTimePanel.Visible then
    MaxExecutionTimePanel.Width := OptionsChosenGrid.ColWidths[1];
  if SelectLabelsPanel.Visible then
    SelectLabelsPanel.Width := OptionsChosenGrid.ColWidths[1];
  if TransposeDataPanel.Visible then
    TransposeDataPanel.Width := OptionsChosenGrid.ColWidths[1];
end;

procedure TAnalysisPrefDlg.SizeFormToFitOptions;
var
  i: Integer;
  TabHeight: Integer;
begin
  try
    BeginFormUpdate;
    ResizeColumns;
    TabHeight := (OptionsPageControl.Height - OptionsSummaryTab.Height - OptionsSummaryTab.BorderWidth);
    Height := (OptionsChosenGrid.DefaultRowHeight*(FCurrentOptions.Count + 1)) + Panel1.Height + TabHeight + 8;
    {$IFDEF DARWIN}
    Height := (OptionsChosenGrid.DefaultRowHeight*(FCurrentOptions.Count + 1)) + Panel1.Height + TabHeight;
    {$ENDIF}
    Width := Max(ButtonsPanel.Width, (OptionsChosenGrid.ColWidths[0] + OptionsChosenGrid.ColWidths[1]) + OptionsChosenGrid.ColWidths[2] + 8);
    AdjustColWidths;
    if seIntegerSpinEdit.Visible then
      seIntegerSpinEdit.Width := SelectionWidth;
    if seFloatSpinEdit.Visible then
      seFloatSpinEdit.Width := SelectionWidth;
    for i:=0 to CustomPanels.Count-1 do
    begin
      if TPanel(CustomPanels.Items[i]^).Visible then
      begin
        TPanel(CustomPanels.Items[i]^).Width := SelectionWidth;
        PositionEllipsisPanel(TPanel(CustomPanels.Items[i]^));
      end;
    end;
    PositionButtonsPanel;
    if (Top + Height) > Screen.WorkAreaHeight then
      Top := 30;
    if (Top + Height) > Screen.WorkAreaHeight then
      Height := Screen.WorkAreaHeight - Top - 30;
  finally
    EndFormUpdate;
  end;
end;

function TAnalysisPrefDlg.GetPickList(RowName: String): String;
begin
  if RowName = opsScope2 then
  begin
    if FHasGps then
      Result := SelWithGpPickList
    else
      Result := SelWithNoGpPickList;
  end
  else if RowName = opsSelHypoToTest2 then
    Result := SelTestHypoPickList;
end;

function TAnalysisPrefDlg.GetLongestNameLength: Integer;
var
  i: Integer;
  ASetting: TAnalysisSetting;
begin
  Result := 0;
  if FEnabledOptions.Count > 0 then
    for i := 0 to FEnabledOptions.Count - 1 do
    begin
      ASetting := TAnalysisSetting(FEnabledOptions[i]);
      if ASetting.IsApplicable then
        Result := Max(Result, Length(ASetting.DisplayName));
    end;
end;

function TAnalysisPrefDlg.SettingIsEditable (aRow :Integer ):Boolean ;
var
  aSetting: TAnalysisSetting;
begin
  Result := False;
  if (aRow >= 0) and (aRow < (FEnabledOptions.Count - 1)) then
  begin
    ASetting := TAnalysisSetting(FEnabledOptions[aRow - 1]);
    if Assigned(ASetting) then
      Result := ((not ASetting.IsReadOnly) and aSetting.IsApplicable);
  end;
end;

procedure TAnalysisPrefDlg.SaveSettingsToFile;
var
  jobj: TJSONObject;
  i: Integer;
  aSetting: TAnalysisSetting;
  filename: String;
  aList: TStringList = nil;
begin
  try
    try
      AnalysisPrefsCriticalSection.Acquire;
      jobj := TJSONObject.Create;
      jobj.Add('numSettings', FPreviousOptions.Count);
      if FPreviousOptions.Count > 0 then
        for i := 0 to FPreviousOptions.Count - 1 do
        begin
          aSetting := TAnalysisSetting(FPreviousOptions[i]);
          jobj.Add('settings-' + IntToStr(i), aSetting.GetJson);
        end;
      filename := GetPrivateFile(MF_ANALYSIS_SETTINGS_FILE, False);
      aList := TStringList.Create;
      aList.Text := jobj.AsJSON;
      aList.SaveTofile(filename);
    except
      on E:Exception do
        ShowMessage('Failed to save state of analysis settings for future use: ' + E.Message);
    end;
  finally
    AnalysisPrefsCriticalSection.Release;
    if Assigned(aList) then
      aList.Free;
    if Assigned(jobj) then
      jobj.Free;
  end;
end;

procedure TAnalysisPrefDlg.LoadSettingsFromFile;
var
  filename: String;
  ajson: TJSONObject;
  aList: TStringList = nil;
  aParser: TJSONParser = nil;
  aData: TJSONData = nil;
  i: Integer;
  numSettings: Integer;
  aSetting: TAnalysisSetting;
begin
  Assert(Assigned(FPreviousOptions));
  filename := GetPrivateFile(MF_ANALYSIS_SETTINGS_FILE, False);
  if not FileExists(filename) then
    Exit;
  try
    try
      AnalysisPrefsCriticalSection.Acquire;
      aList := TStringList.Create;
      aList.LoadFromFile(filename);
      if Trim(aList.Text) <> EmptyStr then
      begin
        aParser := TJSONParser.Create(aList.Text, []);
        aData := aParser.Parse;
        if not Assigned(aData) then
          raise Exception.Create('corrupt analysis settings json file: ' + filename);
        ajson := TJSONObject(aData);
        aData := ajson.Find('numSettings', jtNumber);
        numSettings := aData.Value;
        if numSettings > 0 then
        begin
          FPreviousOptions.Clear;
          for i := 0 to numSettings - 1 do
          begin
            aData := ajson.Find('settings-' + IntToStr(i), jtObject);
            if not Assigned(aData) then
              raise Exception.Create('failed to parse JSON settings');
            aSetting := GetSettingFromJson(aData.AsJSON);
            if not Assigned(aSetting) then
              raise Exception.Create('Failed to load analysis setting from JSON');
            StoreOption(aSetting);
          end;
        end;
      end;
    except
      on E:Exception do
      begin
        ShowMessage('Failed to load saved analysis settings - restoring default settings: ' + E.Message);
        FPreviousOptions.Clear;
      end;
    end;
  finally
    AnalysisPrefsCriticalSection.Release;
    if Assigned(aList) then
      aList.Free;
    if Assigned(aParser) then
      aParser.Free;
  end;
end;

procedure TAnalysisPrefDlg.UpdatePickListValues;
var
  i: Integer;
  aSetting: TAnalysisSetting = nil;
begin
  if FPreviousOptions.Count > 0 then
    for i := 0 to FPreviousOptions.Count - 1 do
    begin
      aSetting := TAnalysisSetting(FPreviousOptions[i]);
      if aSetting.SettingType = stPicklist then
        UpdatePickListValue(TPickListAnalysisSetting(aSetting));
    end;
end;

procedure TAnalysisPrefDlg.UpdatePickListValue(aSetting: TPickListAnalysisSetting);
var
  valSetting: TAnalysisSetting = nil;
begin
  valSetting := PreviousOption(TPickListAnalysisSetting(aSetting).PickListName);
  if Assigned(valSetting) then
    TPickListValueSetting(valSetting).Value := aSetting.Value;
end;

procedure TAnalysisPrefDlg.TransferSettings(var destination: TFPHashList);
var
  i: Integer = -1;
  s: TAnalysisSetting = nil;
begin
  if FCurrentOptions.Count > 0 then
    for i := 0 to FCurrentOptions.Count -1 do
    begin
      s := TAnalysisSetting(FCurrentOptions[i]);
      destination.Add(s.Name, s);
    end;
  FCurrentOptions.Clear;
end;

function TAnalysisPrefDlg.GetPickListSettingValue(picklistName: String): String;
var
  s: TAnalysisSetting = nil;
begin
  Result := EmptyStr;
  if HasSetting(pickListName) then
  begin
    s := GetSetting(pickListName);
    if s is TPickListAnalysisSetting then
      with s as TPickListAnalysisSetting do
        Result := Value;
  end;
end;

function TAnalysisPrefDlg.GetIntegerSettingValue(settingName: String): Integer;
var
  s: TAnalysisSetting = nil;
begin
  Result := -1;
  if HasSetting(settingName) then
  begin
    s := GetSetting(settingName);
    if s is TIntegerAnalysisSetting then
      with s as TIntegerAnalysisSetting do
        Result := Value;
  end;
end;

function TAnalysisPrefDlg.GetFloatSettingValue(settingName: String): Double;
var
  s: TAnalysisSetting = nil;
begin
  Result := -1;
  if HasSetting(settingName) then
  begin
    s := GetSetting(settingName);
    if s is TFloatAnalysisSetting then
      with s as TFloatAnalysisSetting do
        Result := Value;
  end;
end;

function TAnalysisPrefDlg.GetStringSettingValue(settingName: String): String;
var
  s: TAnalysisSetting = nil;
begin
  Result := EmptyStr;
  if HasSetting(settingName) then
  begin
    s := GetSetting(settingName);
    if s is TStringAnalysisSetting then
      with s as TStringAnalysisSetting do
        Result := Value;
  end;
end;

procedure TAnalysisPrefDlg.AddAppOptions(optionsFrame: TOptionsGridFrame; tabCaption: String);
begin
  Assert(optionsFrame.Initialized, 'un-initialized app options grid frame');
  FOptionsGrid := optionsFrame;
  AppOptionsSheet.TabVisible := True;
  AppOptionsSheet.Caption := tabCaption;
  FOptionsGrid.Parent := AppOptionsSheet;
  FOptionsGrid.Align := alClient;
  OptionsPageControl.ActivePage := AppOptionsSheet;
  OptionsSummaryTab.TabVisible := False;
  FOptionsGrid.ResizeColumns;
  ClientWidth := max(ClientWidth, FOptionsGrid.SumColumnWidths);
  Height := FOptionsGrid.OptionsGrid.DefaultRowHeight*(FOptionsGrid.OptionsGrid.RowCount + 2) + Panel1.Height + OptionsPageControl.TabHeight + 8;
  FOptionsGrid.Invalidate;
end;

procedure TAnalysisPrefDlg.ClearAppOptions;
begin
  if Assigned(FOptionsGrid) then
    FOptionsGrid.Visible := False;
  OptionsPageControl.ActivePage := OptionsSummaryTab;
  AppOptionsSheet.TabVisible := False;
  if Assigned(FOptionsGrid) then
    FreeAndNil(FOptionsGrid);
end;

function TAnalysisPrefDlg.GetChosenOptionStrings: TStringList;
var
  i: Integer;
  ASetting: TAnalysisSetting;
  NameLength: Integer;
begin
  NameLength := GetLongestNameLength + 10;
  Result := TStringList.Create;
  Result.Add(opsOperationType1 + ' ' + opsPlus);
  if FCurrentOptions.Count > 0 then
    for i := 0 to FCurrentOptions.Count - 1 do
    begin
      ASetting := TAnalysisSetting(FCurrentOptions[i]);
      if ASetting.IsApplicable then
        Result.Add('  ' + ASetting.GetPrettyString(NameLength));
    end;
end;

procedure TAnalysisPrefDlg.SendTextualSettingsToWebtop;
begin
  Assert(False, 'deprecated');
end;

procedure TAnalysisPrefDlg.AddSiteLabelType(Value: Char);
begin
  ChkLBx.Items.Add(Value);
  ChkLBx.Checked[ChkLBx.Items.Count - 1] := True;
end;

procedure TAnalysisPrefDlg.SetTaxaList(AList: TStringList);
var
  i: Integer;
begin
  if FTaxaList = nil then
    FTaxaList := TStringList.Create;
  FTaxaList.Clear;

  if not IsPrototyper then
    with AList as TStrings do
      for i:=0 to Count-1 do
      begin
        if Length(Names[i]) = 0 then  // this means no group names
          FTaxaList.Add(AList[i])
        else if Length(Values[Names[i]]) > 0 then
          FTaxaList.Add(Names[i]+' {'+ Values[Names[i]]+'}')
        else
          FTaxaList.Add(Names[i]);
      end;

end;

procedure TAnalysisPrefDlg.GetThreeTaxa(var A, B, C: Integer);
var
  aSetting: TPickListAnalysisSetting = nil;
begin
  aSetting := TPickListAnalysisSetting(GetSetting(opsIngroupTaxonA2));
  A := aSetting.ItemIndex;
  aSetting := TPickListAnalysisSetting(GetSetting(opsIngroupTaxonB2));
  B := aSetting.ItemIndex;
  aSetting := TPickListAnalysisSetting(GetSetting(opsOutgroupTaxonC2));
  C := aSetting.ItemIndex;
  if (A=B) or (A=C) or (B=C) then
    Raise Exception.Create('The three selected taxa (A, B, and Outgroup) are not unique. Please ensure they are unique.');
end;

function TAnalysisPrefDlg.GetNumThreadsToUse: Integer;
begin
  Result := 1;
  if not HasSetting(opsMLNumThreads) then
    Exit;
  if (GetSetting(opsMLNumThreads).StringValue = NotApplicableStr) or
     (GetSetting(opsMLNumThreads).StringValue = MultithreadNotAvailableStr) or
     (trim(GetSetting(opsMLNumThreads).StringValue) = EmptyStr) then
    Result := 1
  else
    Result := StrToInt(GetSetting(opsMLNumThreads).StringValue);
end;

function TAnalysisPrefDlg.GetInitialTreeMethod: Integer;
var
  InitialTreeMethod: String;
begin
   if HasSetting(opsMLInitialTrees2) then
   begin
     InitialTreeMethod := GetSetting(opsMLInitialTrees2).StringValue;
     InitialTreeMethod := Trim(InitialTreeMethod);

     if (InitialTreeMethod = InitialTreeByNJStr) then
       result := NJInitTreeMethod
     else if (InitialTreeMethod = InitialTreeByBioNJStr) then
       result := BioNJInitTreeMethod
     else if (InitialTreeMethod = InitialTreeByParsimonyStr) then
       result := MPInitTreeMethod
     else if (InitialTreeMethod  = UserSpecifyFromFile) then
       result := UserProvidedInitTree
     else if (InitialTreeMethod = InitialParsimonyTreesStr) then
       Result := MultipleMPTreesMethod
     else
       result := DefaultInitTreeMethod;
   end
   else
     Result := DefaultInitTreeMethod;
end;

function TAnalysisPrefDlg.GetClockTestType: TClockType;
var
  ARow: Integer = -1;
begin
  Result := ctUnknown;

  if HasSetting(opsClockType2, ARow)then
  begin
    if IsCurrentSetting(opsClockType2, GlobalClockStr) then
      Result := ctGlobal
    else if IsCurrentSetting(opsClockType2, '') then
      Result := ctLocal
    else
      Result := ctUnknown;
  end;
end;

function TAnalysisPrefDlg.GetClockLevel: TClockLevel;
var
  ARow: Integer = -1;
  RowName: String = '';
begin
  Result := clUnknown;
  RowName := EmptyStr;

  if HasSetting(opsClockLevel2, ARow)then
    RowName := opsClockLevel2
  else if HasSetting(opsGlobalClockLevel2, ARow) then
    RowName := opsGlobalClockLevel2;

  if RowName <> EmptyStr then
  begin
    if IsCurrentSetting(rowName, AllClocksStr) then
      Result := clNoStdErr
    else if IsCurrentSetting(rowName, ManyClocksStr) then   // high stringency clock
      Result := clOneStdErr
    else if IsCurrentSetting(rowName, FewClocksStr) then   // low stringency clock
      Result := clTwoStdErr
    else if IsCurrentSetting(rowname, FewestClocksStr) then
      Result := clThreeStdErr
    else
      Result := clUnknown;
  end
  else
    Result := clUnknown;
end;

function TAnalysisPrefDlg.SubstitutionTypeString: String;
begin
  if HasSetting(opsNucSynAminoType2) then
    Result := GetSetting(opsNucSynAminoType2).StringValue
  else
    Result := EmptyStr;
end;

end.

