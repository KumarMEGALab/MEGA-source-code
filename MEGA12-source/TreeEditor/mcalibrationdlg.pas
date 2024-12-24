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

unit MCalibrationDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$IFDEF VISUAL_BUILD}
uses
  LCLType, LCLIntF, SysUtils, Variants,
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, StdActns, ActnList, Menus, ExtCtrls,
  Buttons, CheckLst, MAnalysisInfo, MTreeData, MCalibrationData, MTreeList,
  MTreeBox, MegaConsts, Types, mimageform;

const
  S_MARGIN = 20;
  VERT_MARGIN = 10;
  CALIB_CHECKMARK_IMAGE_INDEX = 1;
  CALIB_UNCHECKMARK_IMAGE_INDEX = 0;

  NONE_INDEX = 0;
  NORMAL_DIST_INDEX = 1;
  EXPONENTIAL_DIST_INDEX = 2;
  UNIFORM_DIST_INDEX = 3;
  MIN_TIME_INDEX = 4;
  MAX_TIME_INDEX = 5;
  FIXED_TIME_INDEX = 6;
  FIXED_RATE_INDEX = 7;

type
  TCalibrationDlgMode = (cdmBeforeConstructTree, cdmAfterConstructTree);
  TTreeSource = (tsTreeData, tsTreeBox);
  TCWCancelCallBack = procedure of object;
  TCWSaveChangesCallBack = procedure(Calibrations: TCalibrations) of object;
  TFinalizeCalibrationFunc = function(var Calibration: TCalibrationTime): Boolean of object;
  THasBadCalibsFunc = function(var Calibration: TCalibrations): Boolean of object; { Calibrations for nodes in the outgroup cluster will be discarded}

  { TCalibrationDlg }

  TCalibrationDlg = class(TForm)
    ActionAutoSizeVert: TAction;
    ActionAutoSizeHoriz: TAction;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    CalibrateNodePopupItem: TMenuItem;
    DeveloperMenu: TMenuItem;
    ShowInfoMenuItem: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OkAction: TAction;
    CancelAction: TAction;
    HelpAction2: TAction;
    ActionList1: TActionList;
    AddCalibrationBtn: TSpeedButton;
    CalibrateNodeBtn: TSpeedButton;
    DeveloperAction: TAction;
    HelpLbl1: TLabel;
    HelpLbl2: TLabel;
    HelpLbl3: TLabel;
    HelpLbl5: TLabel;
    HelpLbl6: TLabel;
    HelpLbl7: TLabel;
    HelpLbl8: TLabel;
    HelpLbl9: TLabel;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    HelpPanel: TPanel;
    RemoveInvalidCalibrationsAction: TAction;
    SampleTimesErrorMsg: TLabel;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    LogNormalDistItem: TMenuItem;
    LogNormalDistItem3: TMenuItem;
    LogNormalDistItem2: TMenuItem;
    ParseTipDatesAction: TAction;
    ActionClearCalibrations: TAction;
    ActionAddCalibration: TAction;
    ImageList1: TImageList;
    DensityParamsLabel: TLabel;
    ExponentialDistItem: TMenuItem;
    DistNameLabel: TLabel;
    EditCalibrationItem: TMenuItem;
    CalibrateLeafNodeItem: TMenuItem;
    BottomBottomPanel: TPanel;
    PleaseSelectLabel: TLabel;
    MaxTimeItem: TMenuItem;
    FixedTimeItem: TMenuItem;
    FixedRateItem: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    ExponentialDistItem2: TMenuItem;
    MaxTimeItem2: TMenuItem;
    FixedTimeItem2: TMenuItem;
    FixedRateItem2: TMenuItem;
    ExponentialDistItem3: TMenuItem;
    MaxTimeItem3: TMenuItem;
    FixedTimeItem3: TMenuItem;
    FixedRateItem3: TMenuItem;
    MinTimeItem3: TMenuItem;
    NewSampleTimeFromSelectedNodeBtn: TToolButton;
    NewSampleTimeFromMrcaBtn: TToolButton;
    ShowHelpBtn: TSpeedButton;
    ToolBar2: TToolBar;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    UniformDistItem3: TMenuItem;
    NormalDistItem3: TMenuItem;
    MinTimeItem2: TMenuItem;
    UniformDistItem2: TMenuItem;
    NormalDistItem2: TMenuItem;
    MinTimeItem: TMenuItem;
    SpeedButton3: TSpeedButton;
    UniformDistItem: TMenuItem;
    NormalDistItem: TMenuItem;
    MainMenu1: TMainMenu;
    ActionList16: TActionList;
    File1: TMenuItem;
    Edit1: TMenuItem;
    LoadCalibrationDataItem: TMenuItem;
    LoadDataAction: TAction;
    ExportCalibrationDataAction: TAction;
    AddCalibrationAction: TAction;
    CalibrationPopupMenu: TPopupMenu;
    RemoveCalibrationAction: TAction;
    SaveCalibrationDataItem: TMenuItem;
    NewCalibrationItem: TMenuItem;
    helpItem: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    HelpAction: TAction;
    FileExit1: TFileExit;
    ExitItem: TMenuItem;
    DeleteCalibrationItem: TMenuItem;
    CreateTimeTreeAction: TAction;
    CloseAction: TAction;
    SampleTimesCalibrateNodeBtn: TSpeedButton;
    SampleTimesNewCalibrationBtn: TSpeedButton;
    TreePanel: TPanel;
    EditorPanel: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    NewCalibrationFromMrcaButton: TToolButton;
    ToolButton4: TToolButton;
    ToolBar3: TToolBar;
    ToolButton7: TToolButton;
    ToolBar4: TToolBar;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    NewCalibrationFromSelectedNodeButton: TToolButton;
    BottomPanel: TPanel;
    ToolButton6: TToolButton;
    Finished1: TMenuItem;
    CalibrateSelectedNode1: TMenuItem;
    ActionList18: TActionList;
    CalibrateSelectedNodeAction: TAction;
    TopologyOnlyAction: TAction;
    ShowInfoAction: TAction;
    CopyToClipboardAction: TAction;
    PointerAction: TAction;
    SwapAction: TAction;
    FlipAction: TAction;
    SearchAction: TAction;
    AutoSizeAction: TAction;
    NodeIdsAction: TAction;
    ResizeAction: TAction;
    Edit2: TMenuItem;
    CopytoClipboard1: TMenuItem;
    Search1: TMenuItem;
    Find1: TMenuItem;
    View1: TMenuItem;
    ShowTopologyOnly1: TMenuItem;
    SwapSubtree1: TMenuItem;
    FlipSubtree1: TMenuItem;
    AutoSizeAction1: TMenuItem;
    ManualResize1: TMenuItem;
    NodeIDs1: TMenuItem;
    Splitter1: TSplitter;
    TreePopupMenu: TPopupMenu;
    SwapSubtree2: TMenuItem;
    FlipSubtree2: TMenuItem;
    Find2: TMenuItem;
    AutoSizeAction2: TMenuItem;
    ManualResize2: TMenuItem;
    Pointer1: TMenuItem;
    PageControl1: TPageControl;
    CalibrationsTab: TTabSheet;
    SampleTimesTab: TTabSheet;
    CalibrationTimesScrollbox: TScrollBox;
    GroupBox1: TGroupBox;
    CalibrationsCheckListBox: TCheckListBox;
    GroupBox2: TGroupBox;
    TaxonALabel: TLabel;
    TaxonBLabel: TLabel;
    TaxonAComboBox: TComboBox;
    TaxonBComboBox: TComboBox;
    MrcaNameEdit: TLabeledEdit;
    DivergenceTimeEdit: TLabeledEdit;
    CalibrationNameEdit: TLabeledEdit;
    MaxDivTimeEdit: TLabeledEdit;
    SampleTimesScrollbox: TScrollBox;
    GroupBox3: TGroupBox;
    SampleTimesCheckListBox: TCheckListBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    SampleTimesTaxonComboBox: TComboBox;
    SampleTimeEdit: TLabeledEdit;
    SampleTimesNameEdit: TLabeledEdit;
    ShowHelpBtn2: TSpeedButton;
    HelpLabel1: TLabel;
    HelpLabel2: TLabel;
    HelpLabel3: TLabel;
    HelpLabel4: TLabel;
    HelpLabel5: TLabel;
    HelpLabel6: TLabel;
    procedure ActionAddCalibrationExecute(Sender: TObject);
    procedure ActionAutoSizeHorizExecute(Sender: TObject);
    procedure ActionAutoSizeVertExecute(Sender: TObject);
    procedure ActionClearCalibrationsExecute(Sender: TObject);
    procedure ActionList18Update(AAction: TBasicAction; var Handled: Boolean);
    procedure AddCalibrationBtnClick(Sender: TObject);
    procedure AutoSizeActionExecute(Sender: TObject);
    procedure BottomPanelClick(Sender: TObject);
    procedure CalibrateLeafNodeItemClick(Sender: TObject);
    procedure CalibrateNodeBtnClick(Sender: TObject);
    procedure CalibrationsCheckListBoxSelectionChange(Sender: TObject;
      User: boolean);
    procedure DeveloperActionExecute(Sender: TObject);
    procedure ExponentialDistItem2Click(Sender: TObject);
    procedure ExponentialDistItem3Click(Sender: TObject);
    procedure ExponentialDistItemClick(Sender: TObject);
    procedure FixedRateItem2Click(Sender: TObject);
    procedure FixedRateItem3Click(Sender: TObject);
    procedure FixedRateItemClick(Sender: TObject);
    procedure FixedTimeItem2Click(Sender: TObject);
    procedure FixedTimeItem3Click(Sender: TObject);
    procedure FixedTimeItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure LogNormalDistItem2Click(Sender: TObject);
    procedure LogNormalDistItem3Click(Sender: TObject);
    procedure LogNormalDistItemClick(Sender: TObject);
    procedure MaxTimeItem2Click(Sender: TObject);
    procedure MaxTimeItem3Click(Sender: TObject);
    procedure MaxTimeItemClick(Sender: TObject);
    procedure EditCalibrationItemClick(Sender: TObject);
    procedure MinTimeItem2Click(Sender: TObject);
    procedure MinTimeItem3Click(Sender: TObject);
    procedure MinTimeItemClick(Sender: TObject);
    procedure NewCalibrationFromMrcaButtonClick(Sender: TObject);
    procedure NewCalibrationFromMrcaButtonContextPopup(Sender: TObject;
      MousePos: TPoint; var Handled: Boolean);
    procedure NewCalibrationFromMrcaButtonMouseEnter(Sender: TObject);
    procedure NewCalibrationFromSelectedNodeButtonClick(Sender: TObject);
    procedure NewCalibrationFromSelectedNodeButtonMouseEnter(Sender: TObject);
    procedure NewSampleTimeFromMrcaBtnClick(Sender: TObject);
    procedure NewSampleTimeFromSelectedNodeBtnClick(Sender: TObject);
    procedure NormalDistItem2Click(Sender: TObject);
    procedure NormalDistItem3Click(Sender: TObject);
    procedure NormalDistItemClick(Sender: TObject);
    procedure ParseTipDatesActionExecute(Sender: TObject);
    procedure RemoveInvalidCalibrationsActionExecute(Sender: TObject);
    procedure SampleTimesCalibrateNodeBtnClick(Sender: TObject);
    procedure SampleTimesNewCalibrationBtnClick(Sender: TObject);
    procedure SaveChangesActionExecute(Sender: TObject);
    procedure DivergenceTimeEditChange(Sender: TObject);
    procedure MrcaNameEditChange(Sender: TObject);
    procedure CalibrationNameEditChange(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure TaxonBComboBoxChange(Sender: TObject);
    procedure TaxonAComboBoxChange(Sender: TObject);
    procedure LoadDataActionExecute(Sender: TObject);
    procedure ExportCalibrationDataActionExecute(Sender: TObject);
    procedure AddCalibrationActionExecute(Sender: TObject);
    procedure RemoveCalibrationActionExecute(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
    procedure CancelChangesActionExecute(Sender: TObject);
    procedure CreateTimeTreeActionExecute(Sender: TObject);
    procedure DoCWSaveChangesCallBack;
    procedure FormClose(Sender: TObject; var aCloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure CalibrationsCheckListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure CalibrationsCheckListBoxClickCheck(Sender: TObject);
    procedure CalibrationsCheckListBoxClick(Sender: TObject);
    procedure MaxDivTimeEditChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure ShowHelpBtnClick(Sender: TObject);
    procedure CalibrateSelectedNodeActionExecute(Sender: TObject);
    procedure ShowInfoActionExecute(Sender: TObject);
    procedure NewCalibrationFromSelectedNodeButtonContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure TopologyOnlyActionExecute(Sender: TObject);
    procedure CopyToClipboardActionExecute(Sender: TObject);
    procedure PointerActionExecute(Sender: TObject);
    procedure SwapActionExecute(Sender: TObject);
    procedure FlipActionExecute(Sender: TObject);
    procedure SearchActionExecute(Sender: TObject);
    procedure NodeIdsActionExecute(Sender: TObject);
    procedure ResizeActionExecute(Sender: TObject);
    procedure ShowHelpBtn2Click(Sender: TObject);
    procedure SampleTimeEditChange(Sender: TObject);
    procedure SampleTimesNameEditChange(Sender: TObject);
    procedure SampleTimesTaxonComboBoxChange(Sender: TObject);
    procedure ClearCalibrationsActionExecute(Sender: TObject);
    procedure UniformDistItem2Click(Sender: TObject);
    procedure UniformDistItem3Click(Sender: TObject);
    procedure UniformDistItemClick(Sender: TObject);
  private
    FListATaxaUsed: TStringList;
    FListBTaxaUsed: TStringList;
    FCalibrationIndices: ArrayOfInteger;
    FFileName: String; { the alignment file name, for displaying in the status bar}
    FCalibrations: TCalibrations;
    FDivergenceTimeNeedsChecked: Boolean;
    FFixedEvolutionaryRate: Double;
    FIgnoreOnChangeEvent: Boolean;
    FDisplayMode: TCalibrationDlgMode;
    FCalibrationType: TCalibrationTarget;
    FIsClosing: Boolean;
    FPopupParent: TToolButton;
    FUseFixedEvolutionaryRate: Boolean;
    procedure UpdateUsedTaxaLists;
    procedure CreateFixedRateTree;
    procedure EditCalibration(Sender: TObject);
    procedure PromptForCalibrationParams(Sender: TObject; Index: Integer);
    procedure InitMenus;
    function GetTreeListNodeIdForTaxonName(aName: String): Integer;
    function GetTreeListTaxonNameForNodeId(aId: Integer): String;
    procedure CancelChangesForCalibrations;
    procedure CancelChangesForSampleTimes;
    procedure AddCalibrationForCalibrations(Sender: TObject);
    procedure AddCalibrationForSampleTimes(Sender: TObject);
    function ValidateFormData: Boolean;
    function ValidateFormDataForCalibrations: Boolean;
    function ValidateFormDataForSampleTimes: Boolean;
    procedure UpdateActionList;
    procedure DoRemoveCalibration(Index: Integer);
    procedure DoRemoveCalibrationFromCalibrations(Index: Integer);
    procedure DoRemoveCalibrationFromSampleTimes(Index: Integer);

    function PrevalidateCalibrations: Boolean; { just ensures that the user has entered what appears to be valid data}
    function GetNumCalibrations: Integer;
    procedure SetDisplayMode(Mode: TCalibrationDlgMode);
    function IndicesDiffer(First, Second: ArrayOfInteger): Boolean;
    function IndexRemoved(OrigIndices, NewIndices: ArrayOfInteger): Boolean;
    procedure ReloadChecklistBoxes;
    procedure ReloadChecklistBoxForCalibrations;
    procedure ReloadChecklistBoxForSampleTimes;
    procedure SetCalibrationType(const Value: TCalibrationTarget);
    procedure SetAnalysisInfoForCalibrations(MAI: TAnalysisInfo);
    procedure SetAnalysisInfoForSampleTimes(MAI: TAnalysisInfo);
    procedure SetOtuNamesForCalibrations(aList: TStringList);
    procedure SetOtuNamesForSampleTimes(aList: TStringList);
    procedure NewCalibrationExternalForCalibrations(Sender: TObject; TreeViewIndex: Integer);
    procedure NewCalibrationExternalForSampleTimes(Sender: TObject; TreeViewIndex: Integer; TaxonName: String);
    procedure SetCalibrationIndexForCalibrations(Index: Integer);
    procedure SetCalibrationIndexForSampleTimes(Index: Integer);
    function GetCurrentCalibrationListBoxSelection: Integer;
    function CalibrationIsSelected: Boolean;
    function MenuItemToCalibrationCategory(aItem: TObject): TCalibrationCategory;
    procedure UpdateDivTimeEditLabels(aCalib: TCalibrationTime);
    procedure ClearDivTimeEdits;
    procedure UpdateProgress(aProgress: Integer; aStatus: String);
    procedure UpdateStatusBar;
    procedure UpdateStatusBarLayout;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    Tree: TTreeBox;
    CalibrationFunc: TCalibrationFunc;
    CalibrationWizard: TObject;
    TreeSource: TTreeSource;
    CWCancelCallBack: TCWCancelCallBack;
    CWSaveChangesCallBack: TCWSaveChangesCallBack;
    CalibCheckMark : TBitmap;
    CalibUnCheckMark : TBitmap;

    { The TAnalysisInfo that is owned by TTreeViewForm. Treat it as read-only,
      do not modify it and do not free it!}
    AnalysisInfo: TAnalysisInfo;
    FinalizeCalibrationFunc: TFinalizeCalibrationFunc;
    HasBadCalibsFunc: THasBadCalibsFunc;
    { NodeLabels are stored in TTreeList but it is not visible to us. To overcome
      this, we set GetInternalNodeLabelFunc to TTreeList.GetInternalNodeLabel.}
    GetInternalNodeLabelFunc: TGetInternalNodeLabelFunc;
    TreeViewForm: TObject;
    CancelledByTreeViewer: Boolean; { so we can clean things up correctly, depending on where a user cancels from}
    StatusBar: TStatusBar;
    ProgressBar: TProgressBar;
    procedure RefreshCalibrationMarkers;
    function MyCalibrationsFunc(const nodeIndex: Integer; var aMinTime: Double; var aMaxTime: Double; var isSelected: Boolean): Boolean;
    procedure TryLoadData(filename: String); overload;
    procedure TryLoadData(aList: TStringList); overload;
    procedure PromptIfTipDatesInNames;
    procedure WarnToCheckInferredTipDates;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function GetCalibrations: TCalibrations;
    procedure SetTreeList(AList: TTreeList);
    procedure AssignCalibrations(var Destination: TCalibrations);
    procedure SetCalibrations(Calibrations: TCalibrations);
    procedure ExportSelectedCalibrationsToEditor;
    procedure ClearCalibrations;
    procedure ClearForm;
    procedure ClearFields;
    procedure ResetForm;
    procedure SetAnalysisInfo(MAI: TAnalysisInfo);
    procedure SetOtuNames(aList: TStringList);
    procedure NewCalibrationExternal(Sender: TObject; TreeViewIndex: Integer); overload;
    procedure NewCalibrationExternal(Sender: TObject; TreeViewIndex: Integer; TaxonName: String); overload;
    procedure SetFileName(FileName: String);
    procedure SetBottomBars(AStatusBar: TStatusBar; AProgressBar: TProgressBar);
    procedure UpdateFormSizeForTree(TreeWidth: Integer; TreeHeight: Integer);
    procedure SetCalibrationIndex(Index: Integer);
    procedure UpdateForLittleBootstraps(hasUserTree: Boolean);
    procedure FocusedNodeChanged(newFocusedNode: Integer);
    property NumCalibrations: Integer read GetNumCalibrations;
    property DisplayMode: TCalibrationDlgMode read FDisplayMode write SetDisplayMode;
    property CalibrationsType: TCalibrationTarget read FCalibrationType write SetCalibrationType;
    property UseFixedEvolutionaryRate: Boolean read FUseFixedEvolutionaryRate;
    property FixedEvolutionaryRate: Double read FFixedEvolutionaryRate;
  end;

var
  CalibrationDlg: TCalibrationDlg;
{$ENDIF}
implementation
{$IFDEF VISUAL_BUILD}
uses
  MTreeViewForm, Mega_Main, MegaUtils, StringUtils, MTimeTreeWizard,
  MEditorForm, Math, DateUtils, MMegaWindowInfo, mdensitydistdlg,
  ContextHelp_HC, mhelpfiles, mhelpkeywords, mcalibrationdensity,
  mparsetipdatesdlg, mtipdatesfinder, mshortcutshelper, contnrs,
  MegaVerConsts, mdistpack;

{$R *.lfm}

procedure TCalibrationDlg.AddCalibrationActionExecute(Sender: TObject);
begin
  if not Visible then
    Exit;
  DensityParamsLabel.Caption := EmptyStr;
  DistNameLabel.Caption := EmptyStr;
  if FCalibrationType = ctInternalNode then
    AddCalibrationForCalibrations(Sender)
  else
  if FCalibrationType = ctLeafNode then
    AddCalibrationForSampleTimes(Sender)
  else
    raise Exception.Create('invalid calibration type - this is a bug');
end;

procedure TCalibrationDlg.NewCalibrationExternal(Sender: TObject; TreeViewIndex: Integer);
begin
  if FCalibrationType = ctInternalNode then
    NewCalibrationExternalForCalibrations(Sender, TreeViewIndex)
  else
    raise Exception.Create('invalid calibration type - this is a bug');
  Invalidate;
end;

procedure TCalibrationDlg.NewCalibrationExternal(Sender: TObject; TreeViewIndex: Integer; TaxonName: String);
begin
   if FCalibrationType = ctLeafNode then
     NewCalibrationExternalForSampleTimes(Sender, TreeViewIndex, TaxonName)
  else
    raise Exception.Create('invalid calibration type - this is a bug');
   Invalidate;
end;

procedure TCalibrationDlg.NewCalibrationExternalForCalibrations(Sender: TObject; TreeViewIndex: Integer);
var
  Calibration: TCalibrationTime;
  Index: Integer;
begin
  try
    Calibration := TCalibrationTime.Create;
    Calibration.CalibrationCategory := MenuItemToCalibrationCategory(Sender);
    FCalibrations.Add(Calibration);
    Calibration.Index := FCalibrations.Count;
    if Assigned(AnalysisInfo) then
      Calibration.NumTaxa := AnalysisInfo.NoOfTaxa
    else
      Calibration.NumTaxa := FCalibrations.NumOtusInTree;
    Calibration.NeedsMRCAResolved := False;
    Calibration.SetNodeIdFromTreeViewFormId(TreeViewIndex);
    Assert(Assigned(FinalizeCalibrationFunc), 'missing finalize calibration func');
    FinalizeCalibrationFunc(Calibration);
    if Assigned(Tree) then
    begin
      Tree.UpdateCalibratedNodes(FCalibrations);
      Tree.DrawCalibratedNodeMarkers;
    end;
    Index := CalibrationsCheckListBox.Items.AddObject(Calibration.CalibrationName, Calibration);
    CalibrationsCheckListBox.ItemIndex := Index;
    CalibrationsCheckListBox.Checked[Index] := True;
    ClearDivTimeEdits;
    MrcaNameEdit.Text := GetInternalNodeLabelFunc(Calibration.NodeID);

    CalibrationNameEdit.Text := Calibration.CalibrationName;
    SetCalibrationIndex(CalibrationsCheckListBox.Items.Count - 1);
    TaxonAComboBox.ItemIndex := TaxonAComboBox.Items.IndexOf(Calibration.NodeA);
    TaxonBComboBox.ItemIndex := TaxonBComboBox.Items.IndexOf(Calibration.NodeB);
    UpdateActionList;
    PromptForCalibrationParams(Sender, Index);
    Invalidate;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when creating a new calibration: ' + E.Message);
  end;
end;

procedure TCalibrationDlg.NewCalibrationExternalForSampleTimes(Sender: TObject; TreeViewIndex: Integer; TaxonName: String);
var
  Calibration: TCalibrationTime;
  Index: Integer;
begin
  try
    Calibration := TCalibrationTime.Create(True);
    FCalibrations.Add(Calibration);
    Calibration.Index := FCalibrations.Count;
    Calibration.NumTaxa := AnalysisInfo.MyNoOfSeqs;
    Calibration.NodeName := TaxonName;
    Calibration.NeedsMRCAResolved := False;
    Calibration.SetNodeIdFromTreeViewFormId(TreeViewIndex);
    FinalizeCalibrationFunc(Calibration);
    Calibration.NodeA := Calibration.NodeName;
    Calibration.NodeB := Calibration.NodeName;
    FCalibrationIndices := FCalibrations.CalibrationIndicesForTE;
    Tree.CalibratedNodes := FCalibrationIndices;
    Tree.DrawCalibratedNodeMarkers;
    Index := SampleTimesCheckListBox.Items.AddObject(Calibration.CalibrationName, Calibration);
    SampleTimesCheckListBox.ItemIndex := Index;
    SampleTimesCheckListBox.Checked[Index] := True;
    SampleTimeEdit.Text := EmptyStr;
    SampleTimesNameEdit.Text := Calibration.CalibrationName;
    SetCalibrationIndex(SampleTimesCheckListBox.Items.Count - 1);
    SampleTimesTaxonComboBox.ItemIndex := SampleTimesTaxonComboBox.Items.IndexOf(Calibration.NodeA);
    UpdateActionList;
    SampleTimeEdit.SetFocus;
    SampleTimeEdit.SelectAll;
    Invalidate;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when creating a new calibration: ' + E.Message);
  end;
end;

procedure TCalibrationDlg.NodeIdsActionExecute(Sender: TObject);
begin
  TTreeViewForm(TreeViewForm).ActionNodeIdsDisplayExecute(Self);
end;

procedure TCalibrationDlg.PointerActionExecute(Sender: TObject);
begin
  TTreeViewForm(TreeViewForm).ActionPointExecute(Self);
end;

function TCalibrationDlg.PrevalidateCalibrations: Boolean;
var
  i: Integer;
  ACalib: TCalibrationTime;
  calibratedTaxa: TFPHashList = nil;
begin
  if Assigned(AnalysisInfo) and AnalysisInfo.UseFixedEvolutionaryRate then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
  try
    try
      FIgnoreOnChangeEvent := True;
      if FCalibrations.Count > 0 then
      begin
        Result := True;
        for i := 0 to FCalibrations.Count - 1 do
        begin
          if not FCalibrations.GetCalibration(i).IsSelected then
            continue;
          SetCalibrationIndex(i);
          Result := (Result and ValidateFormData);
          if not Result then
            break;
          ACalib := FCalibrations.GetCalibration(i);
          if FCalibrationType = ctInternalNode then
          begin
            ACalib.CalibrationName := CalibrationNameEdit.Text;
            ACalib.MrcaName := MrcaNameEdit.Text;
          end
          else if FCalibrationType = ctLeafNode then
          begin
            ACalib.CalibrationName := SampleTimesNameEdit.Text;
            ACalib.MrcaName := EmptyStr;
          end
          else
            raise Exception.Create('invalid calibration type - this is a bug');
          UpdateProgress(Round(i/FCalibrations.Count*100), 'checking calibrations...');
        end;
      end
      else
        Result := True;
    except
      on E:Exception do
      begin
        Result := False;
        ShowMessage('Prevalidation of calibrations failed: ' + E.Message);
      end;
    end;
  finally
    FIgnoreOnChangeEvent := False;
    if Assigned(calibratedTaxa) then
      calibratedTaxa.Free;
  end;
end;

procedure TCalibrationDlg.AddCalibrationForCalibrations(Sender: TObject);
var
  Calibration: TCalibrationTime;
  Index, i: Integer;
begin
  UpdateUsedTaxaLists;
  Calibration := TCalibrationTime.Create;
  if Assigned(Sender) then
    Calibration.CalibrationCategory := MenuItemToCalibrationCategory(Sender);
  Calibration.Index := FCalibrations.Count;
  if Assigned(AnalysisInfo) then
    Calibration.NumTaxa := AnalysisInfo.NoOfTaxa
  else
    Calibration.NumTaxa := FCalibrations.NumOtusInTree;
  Calibration.NeedsMRCAResolved := True;
  FCalibrations.Add(Calibration);
  Index := CalibrationsCheckListBox.Items.AddObject(Calibration.CalibrationName, Calibration);
  CalibrationsCheckListBox.Checked[Index] := True;
  CalibrationsCheckListBox.ItemIndex := CalibrationsCheckListBox.Items.Count - 1;
  ClearDivTimeEdits;
  MrcaNameEdit.Text := EmptyStr;
  SetCalibrationIndex(CalibrationsCheckListBox.Items.Count - 1);
  TaxonAComboBox.Enabled := True;
  TaxonBComboBox.Enabled := True;
  for i := 0 to TaxonAComboBox.Items.Count - 1 do
  begin
    if FListATaxaUsed.IndexOf(TaxonAComboBox.Items[i]) < 0 then
      break;
  end;
  TaxonAComboBox.ItemIndex := i;
  Calibration.NodeA := TaxonAComboBox.Items[i];
  CalibrationNameEdit.Text := Calibration.CalibrationName;
  MrcaNameEdit.Text := Calibration.NodeLabel;
  PromptForCalibrationParams(Sender, Index);
  if not FIsClosing then { true if use specified a fixed rate in which case no calibrations are used}
  begin
    TaxonBComboBox.ItemIndex := -1;
    {$IFNDEF DARWIN}
    TaxonAComboBox.SetFocus;
    {$ENDIF}
    FCalibrationIndices := FCalibrations.CalibrationIndicesForTE;
    if Assigned(Tree) then
    begin
      Tree.CalibratedNodes := FCalibrationIndices;
      Tree.ShowCalibratedNodeMarker := True;
      Tree.Invalidate;
    end;
  end;
end;

procedure TCalibrationDlg.AddCalibrationForSampleTimes(Sender: TObject);
var
  Calibration: TCalibrationTime;
  Index, i: Integer;
begin
  UpdateUsedTaxaLists;
  Calibration := TCalibrationTime.Create(True);
  Calibration.Index := FCalibrations.Count;
  Calibration.NumTaxa := AnalysisInfo.NoOfSeqs;
  Calibration.NeedsMRCAResolved := False;
  FCalibrations.Add(Calibration);
  Index := SampleTimesCheckListBox.Items.AddObject(Calibration.CalibrationName, Calibration);
  SampleTimesCheckListBox.Checked[Index] := True;
  SampleTimesCheckListBox.ItemIndex := SampleTimesCheckListBox.Items.Count - 1;
  SampleTimeEdit.Text := EmptyStr;
  SetCalibrationIndex(SampleTimesCheckListBox.Items.Count - 1);
  SampleTimesTaxonComboBox.Enabled := True;
  for i := 0 to SampleTimesTaxonComboBox.Items.Count - 1 do
  begin
    if FListATaxaUsed.IndexOf(SampleTimesTaxonComboBox.Items[i]) < 0 then
      break;
  end;
  SampleTimesTaxonComboBox.ItemIndex := i;
  Calibration.NodeA := SampleTimesTaxonComboBox.Items[i];
  Calibration.NodeB := Calibration.NodeA;
  Calibration.NodeName := Calibration.NodeA;
  FinalizeCalibrationFunc(Calibration);
  SampleTimesNameEdit.Text := Calibration.CalibrationName;
  FCalibrationIndices := FCalibrations.CalibrationIndicesForTE;
  Tree.CalibratedNodes := FCalibrationIndices;
  Tree.ShowCalibratedNodeMarker := True;
  Tree.Invalidate;
  UpdateActionList;
end;

procedure TCalibrationDlg.AssignCalibrations(var Destination: TCalibrations);
begin
  Destination.Assign(FCalibrations);
end;

procedure TCalibrationDlg.CalibrateSelectedNodeActionExecute(Sender: TObject);
begin
  if Tree.IsFocusedOnRoot then
  begin
    ShowMessage('Calibrations are only allowed for ingroup nodes but the root node is currently selected');
    Exit;
  end;

  DensityParamsLabel.Caption := EmptyStr;
  DistNameLabel.Caption := EmptyStr;
  TTreeViewForm(TreeViewForm).ActionAddCalibrationExecute(Sender);
end;

procedure TCalibrationDlg.CalibrationNameEditChange(Sender: TObject);
var
  Index: Integer;
  ACalib: TCalibrationTime;
begin
  if FIgnoreOnChangeEvent then
    Exit;
  Index := CalibrationsCheckListBox.ItemIndex;
  if (Index < 0) or (Index >= FCalibrations.Count) then
    Exit;
  ACalib := FCalibrations.GetCalibration(Index);
  if ACalib.CalibrationName <> CalibrationNameEdit.Text then
    ACalib.CalibrationName := CalibrationNameEdit.Text;
  CalibrationsCheckListBox.Items[Index] := ACalib.CalibrationName;
  CalibrationsCheckListBox.Invalidate;
end;

procedure TCalibrationDlg.SpeedButton3Click(Sender: TObject);
begin
  EditCalibration(SpeedButton3);
end;

procedure TCalibrationDlg.CalibrationsCheckListBoxClick(Sender: TObject);
var
  Index: Integer;
begin
  if FCalibrationType = ctInternalNode then
    Index := CalibrationsCheckListBox.ItemIndex
  else
  begin
    Index := SampleTimesCheckListBox.ItemIndex;
    SampleTimesErrorMsg.Visible := False;
  end;

  if (Index < 0) or (Index >= FCalibrations.Count) then
    Exit;
  try
    FDivergenceTimeNeedsChecked := False;
    FIgnoreOnChangeEvent := True;
    SetCalibrationIndex(Index);
  finally
    FIgnoreOnChangeEvent := False;
  end;
  UpdateActionList;
end;

procedure TCalibrationDlg.CalibrationsCheckListBoxClickCheck(Sender: TObject);
var
  Index: Integer;
begin
  if FCalibrationType = ctInternalNode then
  begin
    Index := CalibrationsCheckListBox.ItemIndex;
    if (Index < 0) or (Index >= FCalibrations.Count) then
      Exit;
    if not FCalibrations.GetCalibration(Index).IsValid then
      CalibrationsCheckListBox.Checked[Index] := False;
    if CalibrationsCheckListBox.Checked[Index] then
      FCalibrations.GetCalibration(Index).IsSelected := True
    else
      FCalibrations.GetCalibration(Index).IsSelected := False;
  end
  else
  begin
    Index := SampleTimesCheckListBox.ItemIndex;
    if (Index < 0) or (Index >= FCalibrations.Count) then
      Exit;
    if not FCalibrations.GetCalibration(Index).IsValid then
      SampleTimesCheckListBox.Checked[Index] := False;
    if SampleTimesCheckListBox.Checked[Index] then
      FCalibrations.GetCalibration(Index).IsSelected := True
    else
      FCalibrations.GetCalibration(Index).IsSelected := False;
  end;
  Tree.Invalidate;
end;

procedure TCalibrationDlg.CalibrationsCheckListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Calibration: TCalibrationTime = nil;
  Offset: Integer;
begin
  if FIsClosing or (not Visible) then
    Exit;
  if not Assigned(SampleTimesCheckListBox.Items.Objects[Index]) then
    Exit;

  if FCalibrationType = ctInternalNode then
  begin
    if CalibrationsCheckListBox.Items.Count <= Index then
      Exit;
    Calibration := TCalibrationTime(CalibrationsCheckListBox.Items.Objects[Index])
  end
  else
  begin
    if SampleTimesCheckListBox.Items.Count <= Index then
      Exit;
    Assert(Assigned(SampleTimesCheckListBox.Items.Objects[Index]));
    Calibration := TCalibrationTime(SampleTimesCheckListBox.Items.Objects[Index]);
  end;

  with (Control as TCheckListBox).Canvas do
  begin
    FillRect(Rect);       { Clear the rectangle. }
    if Assigned(Calibration) then
    begin
      if Calibration.IsValid then
        Font.Color := clBlack
      else
        Font.Color := clGray;
    end;

    Offset := 2;          { Provide default offset. }

    {$IFDEF WINDOWS}
     if Assigned(Calibration) and Calibration.IsSelected then
       Draw(Rect.Left + 1, Rect.Top + (Rect.Bottom - Rect.Top - CalibCheckMark.Height) div 2, CalibCheckMark)
     else if Assigned(Calibration) then
       Draw(Rect.Left + 1, Rect.Top + (Rect.Bottom - Rect.Top - CalibCheckMark.Height) div 2, CalibUnCheckMark);
     TextOut(Rect.Left + Offset + CalibCheckMark.Width, Rect.Top, (Control as TCheckListBox).Items[Index]);  { Display the text. }
    {$ENDIF}
    {$IFDEF UNIX}
     TextOut(Rect.Left + Offset, Rect.Top, (Control as TCheckListBox).Items[Index]);  { Display the text. }
    {$ENDIF}
  end;
end;

procedure TCalibrationDlg.CancelChangesActionExecute(Sender: TObject);
var
  Index: Integer;
  Calibration: TCalibrationTime;
begin
  if (CalibrationsCheckListBox.Items.Count = 0) or (not Visible) then
    Exit;

  Index := CalibrationsCheckListBox.ItemIndex;
  Calibration := FCalibrations.GetCalibration(Index);

  { If the calibration is a manually created one that has never been saved,
    delete it}
  if Calibration.NeedsMRCAResolved or (Calibration.MinTime < 0) then
  begin
    DoRemoveCalibration(Index);
    Exit;
  end;

  { If it is one that has been saved or imported from a text file, just reload
    it}
  CalibrationsCheckListBox.Items[Index] := Calibration.CalibrationName;
  MrcaNameEdit.Text := Calibration.NodeLabel;
  CalibrationNameEdit.Text := Calibration.CalibrationName;
  DivergenceTimeEdit.Text := Format('%.2f', [Calibration.MinTime]);
  MaxDivTimeEdit.Text := Format('%.2f', [Calibration.MaxTime]);
  UpdateDivTimeEditLabels(Calibration);
  UpdateActionList;
end;

procedure TCalibrationDlg.CancelChangesForCalibrations;
var
  Index: Integer;
  Calibration: TCalibrationTime;
begin
  if (CalibrationsCheckListBox.Items.Count = 0) or (not Visible) then
    Exit;

  Index := CalibrationsCheckListBox.ItemIndex;
  Calibration := FCalibrations.GetCalibration(Index);

  { If the calibration is a manually created one that has never been saved,
    delete it}
  if Calibration.NeedsMRCAResolved or (Calibration.MinTime < 0) then
  begin
    DoRemoveCalibration(Index);
    Exit;
  end;

  { If it is one that has been saved or imported from a text file, just reload
    it}
  CalibrationsCheckListBox.Items[Index] := Calibration.CalibrationName;
  MrcaNameEdit.Text := Calibration.NodeLabel;
  CalibrationNameEdit.Text := Calibration.CalibrationName;
  DivergenceTimeEdit.Text := Format('%.2f', [Calibration.MinTime]);
  MaxDivTimeEdit.Text := Format('%.2f', [Calibration.MaxTime]);
  UpdateDivTimeEditLabels(Calibration);
end;

procedure TCalibrationDlg.CancelChangesForSampleTimes;
var
  Index: Integer;
  Calibration: TCalibrationTime;
begin
  if (SampleTimesCheckListBox.Items.Count = 0) or (not Visible) then
    Exit;

  Index := SampleTimesCheckListBox.ItemIndex;
  Calibration := FCalibrations.GetCalibration(Index);

  { If the calibration is a manually created one that has never been saved,
    delete it}
  if Calibration.MinTime < 0 then
  begin
    DoRemoveCalibration(Index);
    Exit;
  end;

  { If it is one that has been saved or imported from a text file, just reload
    it}
  SampleTimesCheckListBox.Items[Index] := Calibration.CalibrationName;
  SampleTimesNameEdit.Text := Calibration.CalibrationName;
  SampleTimeEdit.Text := Format('%.2f', [Calibration.MinTime]);
end;

procedure TCalibrationDlg.ClearCalibrations;
begin
  if FCalibrations.Count = 0 then
    Exit;

  FDivergenceTimeNeedsChecked := False;
  if FCalibrationType = ctInternalNode then
  begin
    CalibrationNameEdit.Text := EmptyStr;
    MrcaNameEdit.Text := EmptyStr;
    ClearDivTimeEdits;
    CalibrationsCheckListBox.Items.Clear;
  end
  else if FCalibrationType = ctLeafNode then
  begin
    SampleTimesNameEdit.Text := EmptyStr;
    SampleTimeEdit.Text := EmptyStr;
    SampleTimesCheckListBox.Items.Clear;
  end;
  FCalibrations.Clear;
end;

procedure TCalibrationDlg.ClearCalibrationsActionExecute(Sender: TObject);
begin
  ClearCalibrations;
  Tree.CalibratedNodes := FCalibrations.CalibrationIndicesForTE;
  Tree.Invalidate;
end;

procedure TCalibrationDlg.UniformDistItem2Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromSelectedNodeButton;
  UniformDistItemClick(Sender);
end;

procedure TCalibrationDlg.UniformDistItem3Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromMrcaButton;
  UniformDistItemClick(Sender);
end;

procedure TCalibrationDlg.UniformDistItemClick(Sender: TObject);
begin
  if FPopupParent = NewCalibrationFromMrcaButton then
    AddCalibrationActionExecute(UniformDistItem)
  else
    CalibrateSelectedNodeActionExecute(UniformDistItem);
end;

procedure TCalibrationDlg.TryLoadData(filename: String);
var
  aList: TStringList = nil;
begin
  try
    try
      aList := TStringList.Create;
      aList.LoadFromFile(filename);
      TryLoadData(aList);
    except
      on E:Exception do
        ShowMessage('An error occurred while parsing calibration data: ' + E.Message);
    end;
  finally
    EndFormUpdate;
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TCalibrationDlg.TryLoadData(aList: TStringList);
var
  TempCalibs: TCalibrations = nil;
  ACalib: TCalibrationTime = nil;
  i: Integer;
  Index: Integer;
  AlreadyHasCalibs: Boolean;
  ErrorMsg: String = '';
  tempStatus: String = '';
  updateTime: TDateTime;
  numInvalid: Integer = 0;
  response: Integer = 0;
  overwriteExisting: Boolean = False;
begin
  try
    try
      BeginFormUpdate;
      updateTime := Now;
      if Assigned(StatusBar) and (StatusBar.Panels.Count > 0) then
        tempStatus := StatusBar.Panels[0].Text;
      AlreadyHasCalibs := (FCalibrations.Count > 0);

      { First, do everything with temp arrays so that if something goes wrong, we don't corrupt any existing calibrations}
      TempCalibs := TCalibrations.Create;
      TempCalibs.CalibrationsType := FCalibrationType;
      TempCalibs.SetInfo(AnalysisInfo);
      TempCalibs.AssignTreeList(FCalibrations.GetTreeList);
      TempCalibs.LoadFromList(aList);
      if Assigned(Tree) then
      begin
        for i := 0 to TempCalibs.Count - 1 do
        begin
          ACalib := TempCalibs.GetCalibration(i);
          if not FinalizeCalibrationFunc(ACalib) then
            raise Exception.Create('Unable to validate calibration. Incorrect taxon name?');
          if MillisecondsBetween(Now, updateTime) > 200 then
          begin
            UpdateProgress(Round(i/TempCalibs.Count*100), 'loading calibrations...');
            updateTime := Now;
          end;
        end;
      end;

      UpdateProgress(0, 'Validating calibrations...');
      TempCalibs.ProgressUpdateProc := UpdateProgress;
      if Assigned(Tree) and (not TempCalibs.DoAllValidations(ErrorMsg, False, AnalysisInfo.MyOtuNames)) then
      begin
        if Length(ErrorMsg) > 200 then
          ErrorMsg := Copy(ErrorMsg, 1, 200);
        ShowMessage('Problems were encountered when validating calibrations. ' + ErrorMsg + ' Invalid calibrations will be disabled.');
      end;

      ErrorMsg := EmptyStr;
      if Assigned(Tree) then
      begin
        if FCalibrations.NewCalibrationsHaveDuplicates(ErrorMsg, TempCalibs) then
        begin
          response := QuestionDlg('Overwrite', 'The input file has calibrations for nodes that already have calibrations. Overwrite existing calibrations?', mtCustom, [mrYes, 'Overwrite Existing', mrNo, 'Ignore Duplicates'], -1);
          if response = mrYes then
            overwriteExisting := True;
        end;
        if not overwriteExisting then
        begin
          numInvalid := TempCalibs.CountAndUnSelectDuplicates;
          FCalibrations.UnselectNewDuplicates(TempCalibs);
        end;
        if numInvalid > 0 then
          ShowMessage('Duplicate calibration(s) found in the input file have been unselected');
        FCalibrations.InitFastNodeIdLookupList(FCalibrations.NumOtusInTree*2 - 1);
      end;

      for i := 0 to TempCalibs.Count - 1 do
      begin
        if MillisecondsBetween(Now, updateTime) > 200 then
        begin
          UpdateProgress(Round(i/TempCalibs.Count*100), 'finalizing calibrations...');
          UpdateTime := Now;
        end;
        ACalib := TCalibrationTime.Create;
        ACalib.Assign(TempCalibs.GetCalibration(i));
        if FCalibrations.Add(ACalib, overwriteExisting) < 0 then
          ACalib.Free
        else
        begin
          if FCalibrationType = ctInternalNode then
          begin
            Index := CalibrationsCheckListBox.Items.AddObject(ACalib.CalibrationName, ACalib);
            CalibrationsCheckListBox.Checked[Index] := ACalib.IsSelected;
          end
          else
          begin
            Index := SampleTimesCheckListBox.Items.AddObject(ACalib.CalibrationName, ACalib);
            SampleTimesCheckListBox.Checked[Index] := ACalib.IsSelected;
          end;
        end;
      end;
      UpdateProgress(0, tempStatus);
      if Assigned(Tree) then
        RefreshCalibrationMarkers;

      if FCalibrationType = ctInternalNode then
        CalibrationsCheckListBox.ItemIndex := 0
      else
        SampleTimesCheckListBox.ItemIndex := 0;
      if not AlreadyHasCalibs then
        SetCalibrationIndex(0)
      else if GetCurrentCalibrationListBoxSelection >= 0 then
        SetCalibrationIndex(GetCurrentCalibrationListBoxSelection);
      UpdateActionList;
    except
      on E:Exception do
        ShowMessage('An error occurred while parsing calibration data: ' + E.Message);
    end;
  finally
    EndFormUpdate;
    FCalibrations.ClearFastNodeIdLookupList;
    if Assigned(TempCalibs) then
      TempCalibs.Free;
    Tree.Invalidate;
  end;
end;

procedure TCalibrationDlg.UpdateUsedTaxaLists;
var
  i: Integer;
  c: TCalibrationTime;
begin
  FListATaxaUsed.Clear;
  FListBTaxaUsed.Clear;
  if (not Assigned(FCalibrations)) or (FCalibrations.Count = 0) then
    exit;
  for i := 0 to FCalibrations.Count - 1 do
  begin
    c := FCalibrations.GetCalibration(i);
    case FCalibrationType of
      ctInternalNode:
        begin
          if c.NodeA <> EmptyStr then
            FListATaxaUsed.Add(c.NodeA);
          if c.NodeB <> EmptyStr then
            FListBTaxaUsed.Add(c.NodeB);
        end;
      ctLeafNode:
        begin
          if c.NodeA <> EmptyStr then
            FListATaxaUsed.Add(c.NodeA);
        end;
    end;
  end;
end;

procedure TCalibrationDlg.CreateFixedRateTree;
begin
  if not Visible then
    Exit;
  try
    FIgnoreOnChangeEvent := True;
    FIsClosing := True;
    CWCancelCallBack := nil;
    if not Assigned(CalibrationWizard) then
    begin
      Assert(Assigned(CWSaveChangesCallBack), 'not implemented - need to handle the case when using tree explorer');
      Tree.ClearCalibratedNodeMarkers;
      CWSaveChangesCallBack(nil);
    end;
  except
    on E:Exception do
    begin
      FIsClosing := False;
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  end;
end;

procedure TCalibrationDlg.EditCalibration(Sender: TObject);
var
  Index: Integer;
  aCalib: TCalibrationTime;
  paramsDlg: TCalibrationParamsDlg = nil;
begin
  Index := CalibrationsCheckListBox.ItemIndex;
  if (Index < 0) or (Index >= FCalibrations.Count) then
    Exit;
  try
    aCalib := FCalibrations.GetCalibration(Index);
    paramsDlg := TCalibrationParamsDlg.Create(Self);
    paramsDlg.Initialize(aCalib);
    if paramsDlg.ShowModal = mrOk then
      CalibrationsCheckListBoxClick(Sender);
    FDivergenceTimeNeedsChecked := True;
    Tree.Invalidate;
  except
    on E: Exception do
      ShowMessage('Application error: ' + E.Message);
  end;
end;

procedure TCalibrationDlg.PromptForCalibrationParams(Sender: TObject; Index: Integer);
var
  paramsDlg: TCalibrationParamsDlg = nil;
  aDensity: TCalibrationDensity = nil;
  ACalib: TCalibrationTime = nil;
  aCategory: TCalibrationCategory;
  Response: Integer;
  aRate: Double;
  doStrictClockTree: Boolean = False;
begin
  if Index < 0 then
    Exit;
  try
    try
      aCategory := MenuItemToCalibrationCategory(Sender);
      ACalib := FCalibrations.GetCalibration(Index);
      paramsDlg := TCalibrationParamsDlg.Create(Self);
      aDensity := ACalib.GetCalibrationDensity;
      paramsDlg.CalibrationCategory := aCategory;
      paramsDlg.Initialize(ACalib);
      if paramsDlg.ShowModal = mrOk then
      begin
        try
          FIgnoreOnChangeEvent := True;
          case aCategory of
            ccDensityNormal, ccDensityLogNormal, ccDensityUniform:
              begin
                DivergenceTimeEdit.Text := fds(aCalib.MinTime, 2, 8);
                MaxDivTimeEdit.Text := fds(aCalib.MaxTime, 2, 8);
                DensityParamsLabel.Caption := aCalib.GetCalibrationDensity.ParamsCaptionString;
                DistNameLabel.Caption := aCalib.GetCalibrationDensity.DistributionNameString;
              end;
            ccDensityExponential:
              begin
                DivergenceTimeEdit.Text := fds(aCalib.MinTime, 2, 8);
                MaxDivTimeEdit.Text := EmptyStr;
                DensityParamsLabel.Caption := aCalib.GetCalibrationDensity.ParamsCaptionString;
                DistNameLabel.Caption := aCalib.GetCalibrationDensity.DistributionNameString;
              end;
            ccMinTimeOnly:
              begin
                DivergenceTimeEdit.Text := fds(aCalib.MinTime, 2, 8);
              end;
            ccMaxTimeOnly:
              begin
                MaxDivTimeEdit.Text := fds(aCalib.MaxTime, 2, 8);
              end;
            ccFixedTime, ccMinMaxTime:
              begin
                DivergenceTimeEdit.Text := fds(aCalib.MinTime, 2, 8);
                MaxDivTimeEdit.Text := fds(aCalib.MaxTime, 2, 8);
              end;
            ccFixedRate:
              begin
                aRate := aCalib.MinTime;
                DoRemoveCalibration(Index);
                doStrictClockTree := True;
                if FCalibrations.Count > 0 then
                begin
                  Response := MessageDlg('Delete calibrations?', 'Existing calibrations will be deleted and only a strict evolutionary rate will be used to calibrate the tree. Continue?', mtConfirmation, mbYesNo, 0);
                  if Response = mrYes then
                  begin
                    ActionClearCalibrationsExecute(Sender);
                    AnalysisInfo.UseFixedEvolutionaryRate := True;
                    AnalysisInfo.FixedEvolutionaryRate := aRate;
                  end
                  else
                  begin
                    FIgnoreOnChangeEvent := False;
                    FreeAndNil(ParamsDlg);
                    AnalysisInfo.UseFixedEvolutionaryRate := False;
                    doStrictClockTree := False;
                    Exit;
                  end;
                end;
                FFixedEvolutionaryRate := aRate;
                FUseFixedEvolutionaryRate := True;
                AnalysisInfo.UseFixedEvolutionaryRate := True;
                AnalysisInfo.FixedEvolutionaryRate := aRate;
              end;
          end;
          UpdateDivTimeEditLabels(aCalib);
        finally
          FIgnoreOnChangeEvent := False;
        end;
        if aCategory <> ccFixedRate then
          SetCalibrationIndex(Index);
      end
      else
      begin
        DoRemoveCalibration(Index);
        if not Assigned(aDensity) then
        begin
          DensityParamsLabel.Caption := EmptyStr;
          DistNameLabel.Caption := EmptyStr;
        end;
      end;
    except
      on E:Exception do
      begin
        DoRemoveCalibration(Index);
        ShowMessage('Application Error when generating calibration: ' + E.Message);
      end;
    end;
  finally
    if Assigned(paramsDlg) then
      paramsDlg.Free;
  end;
  if (aCategory = ccFixedRate) and doStrictClockTree then
    CreateFixedRateTree;
end;

procedure TCalibrationDlg.InitMenus;
begin
  {$IFNDEF DARWIN}
  MainMenu1.OwnerDraw := True;
  MainMenu1.OnMeasureItem := MegaForm.MeasureMenuItem;
  MainMenu1.OnDrawItem := MegaForm.DrawMenuItem;
  {$ENDIF}
  MegaForm.AddSettingsToPopupMenu(TreePopupMenu);
end;

procedure TCalibrationDlg.ClearFields;
begin
  if FCalibrationType = ctInternalNode then
  begin
    ClearDivTimeEdits;
    MrcaNameEdit.Text := EmptyStr;
    CalibrationNameEdit.Text := EmptyStr;
  end
  else if FCalibrationType = ctLeafNode then
  begin
    SampleTimeEdit.Text := EmptyStr;
    SampleTimesNameEdit.Text := EmptyStr;
  end;
end;

procedure TCalibrationDlg.ClearForm;
begin
  if FCalibrations.Count > 0 then
    Exit;
  if FCalibrationType = ctInternalNode then
  begin
    CalibrationsCheckListBox.Items.Clear;
    TaxonAComboBox.ItemIndex := -1;
    TaxonBComboBox.ItemIndex := -1;
    CalibrationNameEdit.Text := EmptyStr;
    MrcaNameEdit.Text := EmptyStr;
    ClearDivTimeEdits;
  end
  else if FCalibrationType = ctLeafNode then
  begin
    SampleTimesCheckListBox.Items.Clear;
    SampleTimesTaxonComboBox.ItemIndex := -1;
    SampleTimesNameEdit.Text := EmptyStr;
    SampleTimeEdit.Text := EmptyStr;
  end;
end;

procedure TCalibrationDlg.CloseActionExecute(Sender: TObject);
begin
  if FCalibrations.Count > 0 then
    FCalibrations.Clear;
  if not Assigned(CalibrationWizard) then { then the callback procedure is the one in TTreeViewForm}
  begin
    Tree.ClearCalibratedNodeMarkers;
    CWCancelCallBack;
  end;
  MegaForm.RemoveWindowFromTray(Self);
  ModalResult := mrCancel;
end;

procedure TCalibrationDlg.ResetForm;
begin
  ClearCalibrations;
  CalibrationsCheckListBox.Items.Clear;
  TaxonAComboBox.ItemIndex := -1;
  TaxonAComboBox.Enabled := False;
  TaxonBComboBox.ItemIndex := -1;
  TaxonBComboBox.Enabled := False;
  CalibrationNameEdit.Text := EmptyStr;
  MrcaNameEdit.Text := EmptyStr;
  ClearDivTimeEdits;
  DistNameLabel.Caption := EmptyStr;
  DensityParamsLabel.Caption := EmptyStr;

  SampleTimesCheckListBox.Items.Clear;
  SampleTimesTaxonComboBox.ItemIndex := -1;
  SampleTimesTaxonComboBox.Enabled := False;
  SampleTimesNameEdit.Text := EmptyStr;
  SampleTimeEdit.Text := EmptyStr;
  FUseFixedEvolutionaryRate := False;
  FFixedEvolutionaryRate := -1;
end;

procedure TCalibrationDlg.ResizeActionExecute(Sender: TObject);
begin
  TTreeViewForm(TreeViewForm).ActionResizeExecute(Self);
end;

procedure TCalibrationDlg.CopyToClipboardActionExecute(Sender: TObject);
begin
  if not Visible then
    Exit;
  TTreeViewForm(TreeViewForm).ActionCopyExecute(Self);
end;

procedure TCalibrationDlg.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TCalibrationDlg.RefreshCalibrationMarkers;
begin
  FCalibrationIndices := FCalibrations.CalibrationIndicesForTE(True);
  Tree.CalibratedNodes := FCalibrationIndices;
  Tree.ShowCalibratedNodeMarker := True;
  Tree.DrawCalibratedNodeMarkers;
end;

function TCalibrationDlg.MyCalibrationsFunc(const nodeIndex: Integer; var aMinTime: Double; var aMaxTime: Double; var isSelected: Boolean): Boolean;
begin
  if Assigned(FCalibrations) then
    Result := FCalibrations.GetDisplayData(nodeIndex, aMinTime, aMaxTime, isSelected)
  else
    Result := False;
end;

procedure TCalibrationDlg.CreateTimeTreeActionExecute(Sender: TObject);
var
  ErrorMsg: String = '';
begin
  if (not Visible) or (not OkAction.Enabled) or FIsClosing then
  begin
    ModalResult := mrAbort;
    Exit;
  end;

  OkAction.Enabled := False;
  if FCalibrations.Count = 0 then
  begin
    ShowMessage('Please create at least one calibration constraint or close this window to abort the operation.');
    OkAction.Enabled := True;
    Exit;
  end;

  if not PrevalidateCalibrations then
  begin
    OkAction.Enabled := True;
    Exit;
  end;

  if FCalibrations.NumSelectedDuplicates(ErrorMsg) > 0 then
  begin
    UpdateProgress(0, EmptyStr);
    ShowMessage('Cannot continue because duplicate calibrations are selected for one or more nodes: ' + ErrorMsg);
    OkAction.Enabled := True;
    Exit;
  end;

  try
    try
      FIsClosing := True;
      SaveChangesActionExecute(nil);
      FCalibrations.ProgressUpdateProc := UpdateProgress;
      if FCalibrations.NumValidCalibrations > 0 then
      begin
        if not FCalibrations.DoAllValidations(ErrorMsg, True, AnalysisInfo.MyOtuNames) then
        begin
          FIsClosing := False;
          ShowMessage(ErrorMsg);
        end
        else
        begin
          CWCancelCallBack := nil;
          FCalibrations.RemoveInvalids;
          if Assigned(CalibrationWizard) then
            TTimeTreeWizard(CalibrationWizard).AssignCalibrationTimes(FCalibrations)
          else { then the callback procedure is the one in TTreeViewForm}
          begin
            Tree.ClearCalibratedNodeMarkers;
            CWSaveChangesCallBack(FCalibrations);
          end;
          FCalibrations.ProgressUpdateProc := nil;
          ModalResult := mrOk;
        end;
      end
      else
        ShowMessage('Please provide at least one valid divergence time calibration');
    except
      on E:Exception do
      begin
        FIsClosing := False;
        ShowMessage('An error has occurred: ' + E.Message);
      end;
    end;
  finally
    OkAction.Enabled := True;
  end;
end;

procedure TCalibrationDlg.DoCWSaveChangesCallBack;
begin
  CWSaveChangesCallBack(FCalibrations);
end;

procedure TCalibrationDlg.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  if Button = mbRight then
  begin
    p.X := TreePanel.Left + ToolBar3.Width + X;
    p.Y := TreePanel.Top + Toolbar4.Height + Y;
    p := ClientToScreen(p);
    ShowInfoMenuItem.Enabled := (Tree.NodeFocused or Tree.BranchFocused);
    CalibrateNodePopupItem.Enabled := (Tree.NodeFocused and (not Tree.IsFocusedOnRoot));
    TreePopupMenu.Popup(p.X, p.Y);
  end;
end;

procedure TCalibrationDlg.DoRemoveCalibration(Index: Integer);
begin
  DistNameLabel.Caption := EmptyStr;
  DensityParamsLabel.Caption := EmptyStr;
  if FCalibrationType = ctInternalNode then
    DoRemoveCalibrationFromCalibrations(Index)
  else if FCalibrationType = ctLeafNode then
    DoRemoveCalibrationFromSampleTimes(Index)
  else
    raise Exception.Create('invalid calibration type - this is a bug');
end;

procedure TCalibrationDlg.DoRemoveCalibrationFromCalibrations(Index: Integer);
begin
  FCalibrations.Delete(Index);
  CalibrationsCheckListBox.Items.Delete(Index);
  if CalibrationsCheckListBox.Items.Count > 0 then
    SetCalibrationindex(0);
  if CalibrationsCheckListBox.Items.Count = 0 then
  begin
    CalibrationNameEdit.Text := EmptyStr;
    MrcaNameEdit.Text := EmptyStr;
    ClearDivTimeEdits;
    DensityParamsLabel.Caption := EmptyStr;
    DistNameLabel.Caption := EmptyStr;
  end;
  Tree.CalibratedNodes := FCalibrations.CalibrationIndicesForTE;
  Tree.Invalidate;
  UpdateActionList;
  if FCalibrations.Count = 0 then
  begin
    TaxonAComboBox.ItemIndex := -1;
    TaxonAComboBox.Enabled := False;
    TaxonBComboBox.ItemIndex := -1;
    TaxonBComboBox.Enabled := False;
  end;
end;

procedure TCalibrationDlg.DoRemoveCalibrationFromSampleTimes(Index: Integer);
begin
  FCalibrations.Delete(Index);
  SampleTimesCheckListBox.Items.Delete(Index);
  if SampleTimesCheckListBox.Items.Count > 0 then
    SetCalibrationindex(0);
  if SampleTimesCheckListBox.Items.Count = 0 then
  begin
    SampleTimesNameEdit.Text := EmptyStr;
    SampleTimeEdit.Text := EmptyStr;
  end;
  Tree.CalibratedNodes := FCalibrations.CalibrationIndicesForTE;
  Tree.Invalidate;
  UpdateActionList;
  if FCalibrations.Count = 0 then
  begin
    SampleTimesTaxonComboBox.ItemIndex := -1;
    SampleTimesTaxonComboBox.Enabled := False;
  end;
end;

procedure TCalibrationDlg.ExportCalibrationDataActionExecute(Sender: TObject);
var
  StringList: TStringList;
begin
  if not Visible then
    Exit;
  StringList := nil;
  if not ValidateFormData then
    Exit;

  try
    try
      StringList := TStringList.Create;
      FCalibrations.SaveToList(StringList);
      OpenStringList(StringList, 'Calibrations Export');
    except
      on E: Exception do
        ShowMessage('Oh no! An error occurred when saving calibration data to a file: ' + E.Message);
    end;
  finally
    if Assigned(StringList) then
      StringList.Free;
  end;
end;

procedure TCalibrationDlg.ExportSelectedCalibrationsToEditor;
var
  StringList: TStringList = nil;
begin
  if Self.Visible then
  begin
    if not ValidateFormData then
      Exit;

    if not PrevalidateCalibrations then
      Exit;
  end;

  try
    try
      StringList := TStringList.Create;
      FCalibrations.SaveSelectedToList(StringList);
      OpenStringList(StringList, 'Calibrations Export');
    except
      on E: Exception do
        ShowMessage('Oh no! An error occurred when saving calibration data to a file: ' + E.Message);
    end;
  finally
    if Assigned(StringList) then
      StringList.Free;
  end;
end;

procedure TCalibrationDlg.FlipActionExecute(Sender: TObject);
begin
  TTreeViewForm(TreeViewForm).ActionFlipExecute(Self);
end;

procedure TCalibrationDlg.FormClose(Sender: TObject; var aCloseAction: TCloseAction);
begin
  if DisplayMode <> cdmBeforeConstructTree then
  begin
    if Assigned(CWCancelCallBack) then
    begin
      if Assigned(Tree) then
        Tree.ClearCalibratedNodeMarkers;
      CWCancelCallBack;
    end;
  end;
end;

procedure TCalibrationDlg.FormCreate(Sender: TObject);
begin
  CalibrationFunc := MyCalibrationsFunc;
  SampleTimesErrorMsg.Font.Color := clRed;
  SampleTimesErrorMsg.Visible := False;
  UpdateShortcutsForMacOs(ActionList16);
  UpdateShortCutsForMacOs(ActionList18);
  FListATaxaUsed := TStringList.Create;
  FListATaxaUsed.Sorted := True;
  FListATaxaUsed.Duplicates := dupIgnore;
  FListBTaxaUsed := TStringList.Create;
  FListBTaxaUsed.Sorted := True;
  FListBTaxaUsed.Duplicates := dupIgnore;
  {$IFDEF UNIX}
    {$IFDEF DARWIN}
    DensityParamsLabel.Font.Name := 'Courier';
    DistNameLabel.Font.Name := 'Courier';
    {$ELSE}
    DensityParamsLabel.Font.Name := 'Monospace';
    DistNameLabel.Font.Name := 'Monospace';
    {$ENDIF}
  {$ELSE}
    DensityParamsLabel.Font.Name := 'Courier New';
    DensityParamsLabel.Font.Size := 10;
    DistNameLabel.Font.Name := 'Courier New';
    DistNameLabel.Font.Size := 10;
  {$ENDIF}
  DensityParamsLabel.Caption := EmptyStr;
  DensityParamsLabel.Font.Color := HOVER_PANEL_COLOR;
  DensityParamsLabel.Font.Style := [fsBold];
  DistNameLabel.Caption := EmptyStr;
  DistNameLabel.Font.Color := HOVER_PANEL_COLOR;
  DistNameLabel.Font.Style := [fsBold];
  FIsClosing := False;
  HelpContext := HC_CALIBRATIONS_EDITOR;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
  FIgnoreOnChangeEvent := False;
  ProgressBar := nil;
  StatusBar := nil;
  AnalysisInfo := nil;
  FDivergenceTimeNeedsChecked := False;
  FFileName := EmptyStr;
  SetDisplayMode(cdmAfterConstructTree);
  FCalibrations := TCalibrations.Create;
  CWSaveChangesCallBack := nil;
  TreeSource := tsTreeData; // default
  CancelledByTreeViewer := False;
  SetCalibrationType(ctInternalNode);
  CalibCheckMark   := TBitMap.Create;
  CalibUnCheckMark := TBitMap.Create;
  ImageList1.GetBitmap(CALIB_CHECKMARK_IMAGE_INDEX, CalibCheckMark);
  ImageList1.GetBitmap(CALIB_UNCHECKMARK_IMAGE_INDEX, CalibUnCheckMark);
  {$IFNDEF DARWIN}
  InitMenus;
  {$ENDIF}
  DoubleBuffered := True;
  Font.Size := 8;
  Toolbar1.Color := RGB($f7, $f8, $f8);
  Toolbar3.Color := RGB($f7, $f8, $f8);
  Toolbar4.Color := RGB($f7, $f8, $f8);
  FUseFixedEvolutionaryRate := False;
  FFixedEvolutionaryRate := -1;
  PleaseSelectLabel.Visible := False;
  ImageForm.UpdateImgList(Self);
  {$IFDEF DEBUG}
  DeveloperAction.Enabled := True;
  DeveloperAction.Visible := True;
  DeveloperMenu.Visible := True;
  {$ELSE}
  DeveloperAction.Enabled := IsDeveloper;
  DeveloperAction.Visible := IsDeveloper;
  DeveloperMenu.Visible := IsDeveloper;
  {$ENDIF}
end;

procedure TCalibrationDlg.HelpBtnClick(Sender: TObject);
begin
  HelpActionExecute(Sender);
end;

procedure TCalibrationDlg.LogNormalDistItem2Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromSelectedNodeButton;
  LogNormalDistItemClick(Sender);
end;

procedure TCalibrationDlg.LogNormalDistItem3Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromMrcaButton;
  LogNormalDistItemClick(Sender);
end;

procedure TCalibrationDlg.LogNormalDistItemClick(Sender: TObject);
begin
  if FPopupParent = NewCalibrationFromMrcaButton then
    AddCalibrationActionExecute(LogNormalDistItem)
  else
    CalibrateSelectedNodeActionExecute(LogNormalDistItem);
end;

procedure TCalibrationDlg.MaxTimeItem2Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromSelectedNodeButton;
  MaxTimeItemClick(Sender);
end;

procedure TCalibrationDlg.MaxTimeItem3Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromMrcaButton;
  MaxTimeItemClick(Sender);
end;

procedure TCalibrationDlg.MaxTimeItemClick(Sender: TObject);
begin
  if FPopupParent = NewCalibrationFromMrcaButton then
    AddCalibrationActionExecute(MaxTimeItem)
  else
    CalibrateSelectedNodeActionExecute(MaxTimeItem);
end;

procedure TCalibrationDlg.EditCalibrationItemClick(Sender: TObject);
begin
  EditCalibration(SpeedButton3);
end;

procedure TCalibrationDlg.MinTimeItem2Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromSelectedNodeButton;
  MinTimeItemClick(Sender);
end;

procedure TCalibrationDlg.MinTimeItem3Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromMrcaButton;
  MinTimeItemClick(Sender);
end;

procedure TCalibrationDlg.MinTimeItemClick(Sender: TObject);
begin
  if FPopupParent = NewCalibrationFromMrcaButton then
    AddCalibrationActionExecute(MinTimeItem)
  else
    CalibrateSelectedNodeActionExecute(MinTimeItem);
end;

procedure TCalibrationDlg.NewCalibrationFromMrcaButtonClick(Sender: TObject);
var
  p: TPoint;
begin
  p.X := NewCalibrationFromMrcaButton.Left + 2;
  p.Y := NewCalibrationFromMrcaButton.Top + NewCalibrationFromMrcaButton.Height;
  p := ClientToScreen(p);
  CalibrationPopupMenu.Popup(p.X, p.Y);
end;

procedure TCalibrationDlg.NewCalibrationFromMrcaButtonContextPopup(
  Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  FPopupParent := NewCalibrationFromMrcaButton;
end;

procedure TCalibrationDlg.NewCalibrationFromMrcaButtonMouseEnter(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromMrcaButton;
end;

procedure TCalibrationDlg.NewCalibrationFromSelectedNodeButtonClick(Sender: TObject);
var
  p: TPoint;
begin
  p.X := NewCalibrationFromSelectedNodeButton.Left + 2 + EditorPanel.Width;
  p.Y := NewCalibrationFromSelectedNodeButton.Top + NewCalibrationFromSelectedNodeButton.Height;
  p := ClientToScreen(p);
  CalibrationPopupMenu.PopUp(p.X, p.Y);
end;

procedure TCalibrationDlg.NewCalibrationFromSelectedNodeButtonMouseEnter(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromSelectedNodeButton;
end;

procedure TCalibrationDlg.NewSampleTimeFromMrcaBtnClick(Sender: TObject);
begin
  ActionAddCalibrationExecute(Sender);
end;

procedure TCalibrationDlg.NewSampleTimeFromSelectedNodeBtnClick(Sender: TObject);
begin
  CalibrateSelectedNodeActionExecute(Sender);
end;

procedure TCalibrationDlg.NormalDistItem2Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromSelectedNodeButton;
  NormalDistItemClick(Sender);
end;

procedure TCalibrationDlg.NormalDistItem3Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromMrcaButton;
  NormalDistItemClick(Sender);
end;

procedure TCalibrationDlg.NormalDistItemClick(Sender: TObject);
begin
  if FPopupParent = NewCalibrationFromMrcaButton then
    AddCalibrationActionExecute(NormalDistItem)
  else
    CalibrateSelectedNodeActionExecute(NormalDistItem);
end;

procedure TCalibrationDlg.ParseTipDatesActionExecute(Sender: TObject);
var
  dlg: TParseTipDatesForm = nil;
  finder: TTipDatesFinder = nil;
  labels: TStringList = nil;
  calibs: TStringList = nil;
begin
  try
    try
      dlg := TParseTipDatesForm.Create(Self);
      finder := TTipDatesFinder.Create;
      dlg.Finder := finder;
      labels := AnalysisInfo.GetOtuNamesList;
      dlg.Labels := labels;
      if dlg.ShowModal = mrOk then
      begin
        calibs := finder.GetCalibrationStrings;
        if calibs.Count > 0 then
          TryLoadData(calibs)
        else
          ShowMessage('No tip dates were inferred from the taxa names');
      end;
    except
      on E:Exception do
        ShowMessage('Application Error: ' + E.Message);
    end;
  finally
    if Assigned(dlg) then
      dlg.Free;
    if Assigned(finder) then
      finder.Free;
    if Assigned(labels) then
      labels.Free;
    if Assigned(calibs) then
      calibs.Free;
  end;
end;

procedure TCalibrationDlg.RemoveInvalidCalibrationsActionExecute(Sender: TObject);
var
  Response: Integer;
begin
  if (FCalibrations.Count = 0) or (not Visible) then
    Exit;
  Response := MessageDlg('Are you sure you want to delete all invalid calibrations?',mtConfirmation, mbOKCancel, 0);
  if Response = mrCancel then
    Exit;
  try
    FIgnoreOnChangeEvent := True;
    FCalibrations.RemoveInvalids;

    if FCalibrationType = ctInternalNode then
    begin
      CalibrationsCheckListBox.Items.Clear;
      CalibrationNameEdit.Text := EmptyStr;
      MrcaNameEdit.Text := EmptyStr;
      ClearDivTimeEdits;
      DensityParamsLabel.Caption := EmptyStr;
      DistNameLabel.Caption := EmptyStr;
      Tree.CalibratedNodes := FCalibrations.CalibrationIndicesForTE;
      Tree.Invalidate;
      UpdateActionList;
      TaxonAComboBox.ItemIndex := -1;
      TaxonAComboBox.Enabled := False;
      TaxonBComboBox.ItemIndex := -1;
      TaxonBComboBox.Enabled := False;
    end
    else
    begin
      SampleTimesCheckListBox.Items.Clear;
      SampleTimesNameEdit.Text := EmptyStr;
      SampleTimeEdit.Text := EmptyStr;
      Tree.CalibratedNodes := FCalibrations.CalibrationIndicesForTE;
      Tree.Invalidate;
      UpdateActionList;
      SampleTimesTaxonComboBox.ItemIndex := -1;
      SampleTimesTaxonComboBox.Enabled := False;
    end;
    ReloadChecklistBoxes;
  finally
    FIgnoreOnChangeEvent := False;
    Invalidate;
  end;
end;

procedure TCalibrationDlg.SampleTimesCalibrateNodeBtnClick(Sender: TObject);
begin
  CalibrateSelectedNodeActionExecute(Sender);
end;

procedure TCalibrationDlg.SampleTimesNewCalibrationBtnClick(Sender: TObject);
begin
  AddCalibrationActionExecute(Sender);
end;

procedure TCalibrationDlg.FormDestroy(Sender: TObject);
begin
  try
    RemoveWindowFromTray(Self);
  except
    // just have to live with it
  end;
  if Assigned(FCalibrations) then
    FreeAndNil(FCalibrations);
  if Assigned(CalibCheckmark) then
    FreeAndNil(CalibCheckmark);
  if Assigned(CalibUnCheckmark) then
    FreeAndNil(CalibUnCheckmark);
  if Assigned(FListATaxaUsed) then
    FListATaxaUsed.Free;
  if Assigned(FListBTaxaUsed) then
    FListBTaxaUsed.Free;
end;

procedure TCalibrationDlg.FormResize(Sender: TObject);
begin
  UpdateStatusBarLayout;
end;

procedure TCalibrationDlg.FormShow(Sender: TObject);
begin
  FIsClosing := False;
  if SampleTimeEdit.Text = EmptyStr then
    SampleTimesErrorMsg.Visible := False;
  UpdateActionlist;
  try
    if MegaForm.WindowbuttonInTray(Self) = nil then
      MegaForm.AddWindowToTray(Self, False);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when adding a new window tray icon: ' + E.Message);
  end;
  SampleTimesCalibrateNodeBtn.Caption := EmptyStr;
  SampleTimesNewCalibrationBtn.Caption := EmptyStr;
  SampleTimesCalibrateNodeBtn.Caption := EmptyStr;
  SampleTimesNewCalibrationBtn.Caption := EmptyStr;
  if Assigned(Tree) then
  begin
    Tree.ShowCalibratedNodeMarker := True;
    Tree.Refresh;
    Tree.Invalidate;
  end;
  //{$IFDEF DEBUG}
  //if IsGlenDevMode then
  //begin
  //  TryLoadData(GlenCalibrationFile);
  //  CreateTimeTreeActionExecute(nil);
  //end;
  //{$ENDIF}
  Invalidate;
end;

procedure TCalibrationDlg.ActionAddCalibrationExecute(Sender: TObject);
begin
  if not Visible then
    Exit;
  if FCalibrationType = ctInternalNode then
    AddCalibrationForCalibrations(Sender)
  else
  if FCalibrationType = ctLeafNode then
    AddCalibrationForSampleTimes(Sender)
  else
    raise Exception.Create('invalid calibration type - this is a bug');
  Tree.Invalidate;
end;

procedure TCalibrationDlg.ActionAutoSizeHorizExecute(Sender: TObject);
begin
  if Tree.NoOfOTUs <= 0 then
    Exit;

  Case Tree.TreeStyle of
    tsTraditional:
    begin
      Tree.TreeWidth := TreePanel.ClientWidth - GetSystemMetrics(SM_CXVSCROLL)*3 - S_MARGIN*2 - Tree.LargestWidthOfOTUNames;
      Tree.SetAttrIndex;
      Tree.Refresh;
    end;
    tsCircle:
    begin
      ShowMessage('Auto fit is not available for circle trees.');
    end;
    tsRadiation:
    begin
      ShowMessage('Auto fit is not available for radiation trees.');
    end;
  end;
end;

procedure TCalibrationDlg.ActionAutoSizeVertExecute(Sender: TObject);
var
  PPOTU: Integer;
  aScaleBarHeight: Integer = 0;
begin
  if Tree.NoOfOTUs <= 0 then
    Exit;

  Case Tree.TreeStyle of
    tsTraditional:
    begin
      if (Tree.ShowScale or Tree.ShowTimeScale) and (not Tree.ShowTopologyOnly) then
        aScaleBarHeight := Tree.ScaleBarHeight;
      PPOTU := Max(1, (TreePanel.ClientHeight - VERT_MARGIN*3 - aScaleBarHeight) div Tree.NoOfOTUs + 1);
      Tree.PixelsPerOTU := PPOTU;
      Tree.SetAttrIndex;
      Tree.Refresh;
    end;
    tsCircle:
    begin
      ShowMessage('Auto fit is not available for circle trees.');
    end;
    tsRadiation:
    begin
      ShowMessage('Auto fit is not available for radiation trees.');
    end;
  end;
end;

procedure TCalibrationDlg.ActionClearCalibrationsExecute(Sender: TObject);
var
  Response: Integer;
begin
  if (FCalibrations.Count = 0) or (not Visible) then
    Exit;
  Response := MessageDlg('Are you sure you want to delete all calibrations?',mtConfirmation, mbOKCancel, 0);
  if Response = mrCancel then
    Exit;
  try
    FIgnoreOnChangeEvent := True;
    FCalibrations.Clear;

    if FCalibrationType = ctInternalNode then
    begin
      CalibrationsCheckListBox.Items.Clear;
      CalibrationNameEdit.Text := EmptyStr;
      MrcaNameEdit.Text := EmptyStr;
      ClearDivTimeEdits;
      DensityParamsLabel.Caption := EmptyStr;
      DistNameLabel.Caption := EmptyStr;
      Tree.CalibratedNodes := FCalibrations.CalibrationIndicesForTE;
      Tree.Invalidate;
      UpdateActionList;
      TaxonAComboBox.ItemIndex := -1;
      TaxonAComboBox.Enabled := False;
      TaxonBComboBox.ItemIndex := -1;
      TaxonBComboBox.Enabled := False;
    end
    else
    begin
      SampleTimesCheckListBox.Items.Clear;
      SampleTimesNameEdit.Text := EmptyStr;
      SampleTimeEdit.Text := EmptyStr;
      Tree.CalibratedNodes := FCalibrations.CalibrationIndicesForTE;
      Tree.Invalidate;
      UpdateActionList;
      SampleTimesTaxonComboBox.ItemIndex := -1;
      SampleTimesTaxonComboBox.Enabled := False;
    end;
  finally
    FIgnoreOnChangeEvent := False;
    Invalidate;
  end;
end;

procedure TCalibrationDlg.ActionList18Update(AAction: TBasicAction;
  var Handled: Boolean);
begin
  SpeedButton3.Enabled := CalibrationIsSelected;
  Handled := True;
end;

procedure TCalibrationDlg.AddCalibrationBtnClick(Sender: TObject);
var
  p: TPoint;
begin
  FPopupParent := NewCalibrationFromMrcaButton;
  p.X := AddCalibrationBtn.Left;
  p.Y := AddCalibrationBtn.Top - Toolbar1.Height - PageControl1.TabHeight;
  p := ClientToScreen(p);
  CalibrationPopupMenu.Popup(p.X, p.Y);
end;

procedure TCalibrationDlg.AutoSizeActionExecute(Sender: TObject);
var
  PPOTU: Integer;
  newHeight: Integer;
  aScaleBarHeight: Integer;
begin
  with Tree do
  begin
    if NoOfOTUs <= 0 then
      Exit;
    if (Tree.Height > TreePanel.Height) and (Tree.Width > TreePanel.Width) then
      Exit;
    if Tree.ShowScale or Tree.ShowTimeScale then
      aScaleBarHeight := Tree.ScaleBarHeight
    else
      aScaleBarHeight := 0;
    Assert(Tree.TreeStyle = tsTraditional, 'invalid call to AdjustTreeForBestFitToWindow');
    PPOTU := max(DEFAULT_PPOTU, ((TreePanel.ClientHeight - TopMargin - aScaleBarHeight) div NoOfOtus + 1));
    newHeight := (PPOTU+1) * NoOfOTUs;
    if (TreeHeight - TopMargin) < newHeight then
      PixelsPerOtu := PPOTU;
    TreeWidth := TreePanel.ClientWidth - Toolbar3.Width - GetSystemMetrics(SM_CXVSCROLL)*3 - LeftMargin*4 - LargestWidthOfOTUNames;
    SetAttrIndex;
    Refresh;
  end;
end;

procedure TCalibrationDlg.BottomPanelClick(Sender: TObject);
begin

end;

procedure TCalibrationDlg.CalibrateLeafNodeItemClick(Sender: TObject);
begin
  if Tree.FocusedOnOtuNode and (not Tree.FocusedOnOutgroup) then
    CalibrateSelectedNodeActionExecute(Sender)
  else
    AddCalibrationActionExecute(Sender);
end;

procedure TCalibrationDlg.CalibrateNodeBtnClick(Sender: TObject);
var
  p: TPoint;
begin
  FPopupParent := NewCalibrationFromSelectedNodeButton;
  p.X := CalibrateNodeBtn.Left;
  p.Y := CalibrateNodeBtn.Top - Toolbar1.Height - PageControl1.TabHeight;
  p := ClientToScreen(p);
  CalibrationPopupMenu.Popup(p.X, p.Y);
end;

procedure TCalibrationDlg.CalibrationsCheckListBoxSelectionChange(Sender: TObject; User: boolean);
var
  index: Integer = -1;
  c: TCalibrationTime = nil;
begin
  if FIgnoreOnChangeEvent then Exit;
  index := CalibrationsCheckListBox.ItemIndex;
  if index >= 0 then
  begin
    c := FCalibrations.GetCalibration(index);
    if Assigned(Tree) and (c.TreeViewFormIndex >= 0) then
      Tree.FocusOnNode(c.TreeViewFormIndex);
  end;
end;

procedure TCalibrationDlg.DeveloperActionExecute(Sender: TObject);
begin
  //ShowMessage(Format('width0=%d, text0=%s, width1=%d text1=%s', [StatusBar.Panels[0].Width, StatusBar.Panels[0].Text, StatusBar.Panels[1].Width, StatusBar.Panels[1].Text]));
  ShowMessage('Show calibration markers = ' + BoolToStr(Tree.ShowCalibratedNodeMarker, True));
end;

procedure TCalibrationDlg.ExponentialDistItem2Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromSelectedNodeButton;
  ExponentialDistItemClick(Sender);
end;

procedure TCalibrationDlg.ExponentialDistItem3Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromMrcaButton;
  ExponentialDistItemClick(Sender);
end;

procedure TCalibrationDlg.ExponentialDistItemClick(Sender: TObject);
begin
  if FPopupParent = NewCalibrationFromMrcaButton then
    AddCalibrationActionExecute(ExponentialDistItem)
  else
    CalibrateSelectedNodeActionExecute(ExponentialDistItem);
end;

procedure TCalibrationDlg.FixedRateItem2Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromSelectedNodeButton;
  FixedRateItemClick(Sender);
end;

procedure TCalibrationDlg.FixedRateItem3Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromMrcaButton;
  FixedRateItemClick(Sender);
end;

procedure TCalibrationDlg.FixedRateItemClick(Sender: TObject);
begin
  if FPopupParent = NewCalibrationFromMrcaButton then
    AddCalibrationActionExecute(FixedRateItem)
  else
    CalibrateSelectedNodeActionExecute(FixedRateItem);
end;

procedure TCalibrationDlg.FixedTimeItem2Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromSelectedNodeButton;
  FixedTimeItemClick(Sender);
end;

procedure TCalibrationDlg.FixedTimeItem3Click(Sender: TObject);
begin
  FPopupParent := NewCalibrationFromMrcaButton;
  FixedTimeItemClick(Sender);
end;

procedure TCalibrationDlg.FixedTimeItemClick(Sender: TObject);
begin
  if FPopupParent = NewCalibrationFromMrcaButton then
    AddCalibrationActionExecute(FixedTimeItem)
  else
    CalibrateSelectedNodeActionExecute(FixedTimeItem);
end;

procedure TCalibrationDlg.FormActivate(Sender: TObject);
begin
  ToolBar2.Images := ImageForm.GetDialogButtonImageList;
  ToolBar2.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar2.ImagesWidth := ToolBar2.ButtonWidth;
  UpdateStatusBarLayout;
  Invalidate;
end;

function TCalibrationDlg.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
begin
  try
    Result := True;
    CallHelp := False;
    ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
  except
    on E:Exception do
      ShowMessage('Application error: failed to initialize the help browser: ' + E.Message);
  end;
end;

function TCalibrationDlg.GetCalibrations: TCalibrations;
begin
  Result := FCalibrations;
end;

function TCalibrationDlg.GetNumCalibrations: Integer;
begin
  if Assigned(FCalibrations) then
    Result := FCalibrations.Count
  else
    Result := 0;
end;

function TCalibrationDlg.GetTreeListNodeIdForTaxonName(aName: String): Integer;
begin
  Result := AnalysisInfo.MyOriTreeList.OTUNameList.IndexOf(aName);
end;

function TCalibrationDlg.GetTreeListTaxonNameForNodeId(aId: Integer): String;
begin
  Assert((aId >= 0) and Assigned(AnalysisInfo) and Assigned(AnalysisInfo.MyOriTreeList) and (aId < AnalysisInfo.MyOriTreeList.OTUNameList.Count));
  Result := AnalysisInfo.MyOriTreeList.OTUNameList[aId];
end;

procedure TCalibrationDlg.HelpActionExecute(Sender: TObject);
begin
  {$IFDEF DARWIN}
  if not (Screen.ActiveForm = Self) then Exit;
  {$ENDIF}
  ShowContextSensitiveHelp(HelpKeyword);
end;

function TCalibrationDlg.IndexRemoved(OrigIndices, NewIndices: ArrayOfInteger): Boolean;
var
  i, j: Integer;
  Found: Boolean;
begin
  Result := False;
  if Length(OrigIndices) = 0 then
    Exit;
  if Length(NewIndices) < Length(OrigIndices) then
  begin
    Result := True;
    Exit;
  end;
  for i := 0 to Length(OrigIndices) - 1 do
  begin
    Found := False;
    for j := 0 to Length(NewIndices) - 1 do
    begin
      if OrigIndices[i] = NewIndices[i] then
      begin
        Found := True;
        break;
      end;
    end;
    if not Found then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TCalibrationDlg.IndicesDiffer(First, Second: ArrayOfInteger): Boolean;
var
  i, j: Integer;
  Found: Boolean;
begin
  Result := True;
  if Length(First) <> Length(Second) then
    Exit;
  if Length(First) > 0 then
  begin
    for i := 0 to Length(First) - 1 do
    begin
      Found := False;
      for j := 0 to Length(Second) do
      begin
        if First[i] = Second[j] then
        begin
          Found := True;
          break;
        end;
      end;
      if not Found then
        Exit; { iterated overall of Second and did not find a match}
    end;
  end;
  Result := False;
end;

procedure TCalibrationDlg.LoadDataActionExecute(Sender: TObject);
var
  InitDir: String;
begin
  if not Visible then
    Exit;
  if FileExists(MegaForm.DataFileName) then
  begin
    InitDir := ChangeInitialDirectorySaveDialogForMac(OpenDialog1.InitialDir);    // The same directory as TaxaGpsDlg used in the previous step
    OpenDialog1.InitialDir := InitDir;
  end;
  OpenDialog1.Filter := 'Text Files|*.txt|All Files|*.*';
  OpenDialog1.FilterIndex := 1;
  if not OpenDialog1.Execute then
    Exit;
  TryLoadData(OpenDialog1.Filename);
end;

procedure TCalibrationDlg.DivergenceTimeEditChange(Sender: TObject);
var
  Index: Integer;
  ATime: Double;
  ACalib: TCalibrationTime;
begin
  if FIgnoreOnChangeEvent then
    Exit;

  Index := CalibrationsCheckListBox.ItemIndex;
  if (Index < 0) or (Index >= FCalibrations.Count) then
    Exit;

  if TryStrToFloat(DivergenceTimeEdit.Text, ATime) then
  begin
    ACalib := FCalibrations.GetCalibration(Index);
    ACalib.MinTime := ATime;
  end;
  FDivergenceTimeNeedsChecked := True;
end;

procedure TCalibrationDlg.MaxDivTimeEditChange(Sender: TObject);
var
  Index: Integer;
  ATime: Double;
  ACalib: TCalibrationTime;
begin
  if FIgnoreOnChangeEvent then
    Exit;
  Index := CalibrationsCheckListBox.ItemIndex;
  if (Index < 0) or (Index >= FCalibrations.Count) then
    Exit;

  if FDivergenceTimeNeedsChecked then
  begin
    if TryStrToFloat(MaxDivTimeEdit.Text, ATime) then
    begin
      ACalib := FCalibrations.GetCalibration(Index);
      ACalib.MaxTime := ATime;
    end;
  end;
  FDivergenceTimeNeedsChecked := True;
end;

procedure TCalibrationDlg.MrcaNameEditChange(Sender: TObject);
var
  Index: Integer;
  ACalib: TCalibrationTime;
begin
  if FIgnoreOnChangeEvent then
    Exit;
  Index := CalibrationsCheckListBox.ItemIndex;
  if (Index < 0) or (Index >= FCalibrations.Count) then
    Exit;
  ACalib := FCalibrations.GetCalibration(Index);
  if MrcaNameEdit.Text <> ACalib.NodeLabel then
    ACalib.NodeName := MrcaNameEdit.Text;
end;

procedure TCalibrationDlg.SampleTimeEditChange(Sender: TObject);
var
  Index: Integer;
  ATime: Double = -1;
  ACalib: TCalibrationTime;
begin
  if FIgnoreOnChangeEvent then
    Exit;
  SampleTimesErrorMsg.Visible := False;
  Index := SampleTimesCheckListBox.ItemIndex;
  if (Index < 0) or (Index >= FCalibrations.Count) then
    Exit;

  if TryStrToFloat(SampleTimeEdit.Text, ATime) and (CompareValue(ATime, 0.0, FP_CUTOFF) > 0) then
  begin
    if SampleTimesErrorMsg.Visible then
      SampleTimesErrorMsg.Visible := False;
    ACalib := FCalibrations.GetCalibration(Index);
    ACalib.MinTime := ATime;
    ACalib.MaxTime := ATime;
  end
  else
    SampleTimesErrorMsg.Visible := True;
  FDivergenceTimeNeedsChecked := True;
end;

procedure TCalibrationDlg.SampleTimesNameEditChange(Sender: TObject);
var
  Index: Integer;
  ACalib: TCalibrationTime;
begin
  if FIgnoreOnChangeEvent then
    Exit;
  Index := SampleTimesCheckListBox.ItemIndex;
  if (Index < 0) or (Index >= FCalibrations.Count) then
    Exit;
  ACalib := FCalibrations.GetCalibration(Index);
  if ACalib.CalibrationName <> SampleTimesNameEdit.Text then
    ACalib.CalibrationName := SampleTimesNameEdit.Text;
  SampleTimesCheckListBox.Items[Index] := ACalib.CalibrationName;
  SampleTimesCheckListBox.Invalidate;
end;

procedure TCalibrationDlg.SampleTimesTaxonComboBoxChange(Sender: TObject);
var
  Index: Integer;
  ACalib: TCalibrationTime;
begin
  if FIgnoreOnChangeEvent then
    Exit;
  SampleTimesErrorMsg.Visible := False;
  Index := SampleTimesCheckListBox.ItemIndex;
  if Index < 0 then
    Exit;
  ACalib := FCalibrations.GetCalibration(Index);
  ACalib.NeedsMRCAResolved := False;
  ACalib.NodeA := SampleTimesTaxonComboBox.Items[SampleTimesTaxonComboBox.ItemIndex];
  ACalib.NodeB := ACalib.NodeA;
  ACalib.NodeName := ACalib.NodeA;
  if ACalib.NodeID >= 0 then
    ACalib.NodeID := -1; { if we don't reset it, upstream procedures treat it as already resolved}
  SampleTimesNameEdit.Text := ACalib.CalibrationName;
  SampleTimesCheckListBox.Items[Index] := ACalib.CalibrationName;
  SampleTimesCheckListBox.Invalidate;
  if SampleTimesTaxonComboBox.ItemIndex >= 0 then
  begin
    FinalizeCalibrationFunc(ACalib);
    FCalibrationIndices := FCalibrations.CalibrationIndicesForTE;
    Tree.UpdateCalibratedNodes(FCalibrations);
    if not IndexRemoved(Tree.CalibratedNodes, FCalibrationIndices) then { in this case we can avoid redrawing the whole tree}
      Tree.DrawCalibratedNodeMarkers
    else
      Tree.Invalidate; { have to redraw the tree in order to clear the marker for the deleted/changed calibration}
  end;
end;

procedure TCalibrationDlg.SaveChangesActionExecute(Sender: TObject);
var
  ACalib: TCalibrationTime;
  i: Integer;
begin
  if not Visible then
    Exit;
  if AnalysisInfo.MyUsrOperation = dtdoLbsInference then
    ModalResult := mrOk
  else
  begin
    if FCalibrations.Count > 0 then
      for i := 0 to FCalibrations.Count - 1 do
      begin
        ACalib := FCalibrations.GetCalibration(i);
        if ACalib.NeedsMRCAResolved then
        begin
          FinalizeCalibrationFunc(ACalib);
          ACalib.NeedsMRCAResolved := False;
        end;
      end;
    ReloadChecklistBoxes;
  end;
end;

procedure TCalibrationDlg.SearchActionExecute(Sender: TObject);
begin
  TTreeViewForm(TreeViewForm).ActionFindTaxaNameExecute(Self);
end;

procedure TCalibrationDlg.SetAnalysisInfo(MAI: TAnalysisInfo);
begin
  if FCalibrationType = ctInternalNode then
    SetAnalysisInfoForCalibrations(MAI)
  else if FCalibrationType = ctLeafNode then
    SetAnalysisInfoForSampleTimes(MAI)
  else
    raise Exception.Create('invalid calibration type - this is a bug');
end;

procedure TCalibrationDlg.SetAnalysisInfoForCalibrations(MAI: TAnalysisInfo);
var
  i: Integer;
  aName: String;
begin
  AnalysisInfo := MAI;
  if TaxonAComboBox.Items.Count > 0 then
    TaxonAComboBox.Clear;
  if TaxonBComboBox.Items.Count > 0 then
    TaxonBComboBox.Clear;
  if MAI.MyUsrOperation = dtdoLbsInference then
  begin
    for i := 0 to AnalysisInfo.MyOtuNames.Count - 1 do
    begin
      if AnalysisInfo.MyOutgroupMembers[i] then
        continue;
      aName := AnalysisInfo.MyOtuNames[i];
      TrimTaxaName2(aname);
      TaxonAComboBox.Items.Add(aName);
      TaxonBComboBox.Items.Add(aName);
    end;
  end
  else
  begin
    for i := 0 to AnalysisInfo.MyOriTreeList.NoOfOtus - 1 do
    begin
      if AnalysisInfo.MyOriTreeList[0].IsOutgroupMember[i] then
        continue;
      aName := AnalysisInfo.MyOriTreeList.OtuName[i];
      TrimTaxaName2(aname);
      TaxonAComboBox.Items.Add(aName);
      TaxonBComboBox.Items.Add(aName);
    end;
  end;

  TaxonAComboBox.ItemIndex := 0;
  TaxonBComboBox.ItemIndex := 1;
  FCalibrations.SetInfo(MAI);
end;

procedure TCalibrationDlg.SetAnalysisInfoForSampleTimes(MAI: TAnalysisInfo);
var
  i: Integer;
  aName: String;
begin
  AnalysisInfo := MAI;
  if SampleTimesTaxonComboBox.Items.Count > 0 then
    SampleTimesTaxonComboBox.Clear;
  for i := 0 to AnalysisInfo.MyOriTreeList.NoOfOtus - 1 do
  begin
    aName := AnalysisInfo.MyOriTreeList.OtuName[i];
    TrimTaxaName2(aname);
    SampleTimesTaxonComboBox.Items.Add(aName);
  end;
  SampleTimesTaxonComboBox.ItemIndex := 0;
  FCalibrations.SetInfo(MAI);
end;

procedure TCalibrationDlg.SetCalibrationIndex(Index: Integer);
begin
  if FCalibrationType = ctInternalNode then
    SetCalibrationIndexForCalibrations(Index)
  else if FCalibrationType = ctLeafNode then
    SetCalibrationIndexForSampleTimes(Index)
  else
    raise Exception.Create('invalid calibration type - this is a bug');
  UpdateActionList;
end;

procedure TCalibrationDlg.UpdateForLittleBootstraps(hasUserTree: Boolean);
const
  COMPONENT_SPACING = 10;
begin
  if not hasUserTree then
  begin
    Edit2.Visible := False;
    Toolbar3.Visible := False;
    Toolbar4.Visible := False;
    HelpPanel.Height := AddCalibrationBtn.Height + COMPONENT_SPACING*2;
    HelpLbl2.Visible := False;
    HelpLbl3.Visible := False;
    HelpLbl5.Visible := False;
    HelpLbl6.Visible := False;
    HelpLbl7.Visible := False;
    HelpLbl8.Visible := False;
    HelpLbl9.Visible := False;
    AddCalibrationBtn.Top := COMPONENT_SPACING;
    AddCalibrationBtn.Left := COMPONENT_SPACING;
    HelpLbl1.Caption := 'Add Calibration...';
    HelpLbl1.Font.Color := RGB(31, 156, 157);
    HelpLbl1.Top := AddCalibrationBtn.Top + Round(AddCalibrationBtn.Height/2);
    HelpLbl1.Left := AddCalibrationBtn.Left + AddCalibrationBtn.Width + COMPONENT_SPACING;
    HelpPanel.Parent := TreePanel;
    HelpPanel.Align := alTop;
    ShowHelpBtn.Visible := False;
    GroupBox2.Parent := TreePanel;
    GroupBox2.Align := alClient;
    GroupBox2.Caption := 'Calibration Parameters';
    GroupBox1.Align := alClient;
    ClientHeight := HelpPanel.Height + GroupBox2.Height + BottomBottomPanel.Height + 3*COMPONENT_SPACING;
    View1.Visible := False;
    CalibrateSelectedNode1.Visible := False;
    Search1.Visible := False;
    ClientWidth := CalibrationTimesScrollbox.Width + HelpPanel.Width + Splitter1.Width + 3*COMPONENT_SPACING;
    CalibrationsCheckListBox.Height := GroupBox1.Height - COMPONENT_SPACING*2;
    CalibrationsCheckListBox.BorderStyle := bsNone;
  end;
end;

procedure TCalibrationDlg.FocusedNodeChanged(newFocusedNode: Integer);
var
  c: TCalibrationTime = nil;
  i: Integer = -1;
begin
  if Assigned(FCalibrations) and (FCalibrations.Count > 0) then
  begin
    try
      FIgnoreOnChangeEvent := True;
      for i := 0 to FCalibrations.Count - 1 do
      begin
        c := FCalibrations.GetCalibration(i);
        if c.TreeViewFormIndex = newFocusedNode then
        begin
          if PageControl1.ActivePage = CalibrationsTab then
            CalibrationsCheckListBox.ItemIndex := i
          else
            SampleTimesCheckListBox.ItemIndex := i;
          FDivergenceTimeNeedsChecked := False;
          SetCalibrationIndex(i);
          break;
        end;
      end;
    finally
      FIgnoreOnChangeEvent := False;
    end;
  end;
end;

procedure TCalibrationDlg.SetCalibrationIndexForCalibrations(Index: Integer);
var
  Calibration: TCalibrationTime;
begin
  try
    FIgnoreOnChangeEvent := True;
    Calibration := FCalibrations.GetCalibration(Index);
    if Calibration.GetCalibrationDensity <> nil then
    begin
      DensityParamsLabel.Caption := Calibration.GetCalibrationDensity.ParamsCaptionString;
      DistNameLabel.Caption := Calibration.GetCalibrationDensity.DistributionNameString;
    end
    else
    begin
      DensityParamsLabel.Caption := EmptyStr;
      DistNameLabel.Caption := EmptyStr;
    end;
    TaxonAComboBox.ItemIndex := TaxonAComboBox.Items.IndexOf(Calibration.NodeA);
    TaxonBComboBox.ItemIndex := TaxonBComboBox.Items.IndexOF(Calibration.NodeB);
    if TaxonBComboBox.ItemIndex >= 0 then
      PleaseSelectLabel.Visible := False
    else
      PleaseSelectLabel.Visible := True;
    if Calibration.NeedsMRCAResolved then
    begin
      TaxonAComboBox.Enabled := True;
      TaxonBComboBox.Enabled := True;
    end
    else
    begin
      TaxonAComboBox.Enabled := False;
      TaxonBComboBox.Enabled := False;
    end;
    CalibrationNameEdit.Text := Calibration.CalibrationName;

    MrcaNameEdit.Text := Calibration.NodeLabel;
    if CompareValue(Calibration.MinTime, 0.0, FP_CUTOFF) >= 0 then
    begin
      DivergenceTimeEdit.Visible := True;
      DivergenceTimeEdit.Text := Format('%.2f', [Calibration.MinTime]);
      UpdateDivTimeEditLabels(Calibration);
    end
    else
      DivergenceTimeEdit.Visible := False;


    if CompareValue(Calibration.MaxTime, 0.0, FP_CUTOFF) > 0 then
    begin
      MaxDivTimeEdit.Visible := True;
      MaxDivTimeEdit.Text := Format('%.2f', [Calibration.MaxTime]);
    end
    else
      MaxDivTimeEdit.Visible := False;

    CalibrationsCheckListBox.ItemIndex := Index;
  finally
    FIgnoreOnChangeEvent := False;
  end;
end;

procedure TCalibrationDlg.SetCalibrationIndexForSampleTimes(Index: Integer);
var
  Calibration: TCalibrationTime;
begin
  try
    FIgnoreOnChangeEvent := True;
    Calibration := FCalibrations.GetCalibration(Index);
    SampleTimesTaxonComboBox.ItemIndex := SampleTimesTaxonComboBox.Items.IndexOf(Calibration.NodeA);
    SampleTimesTaxonComboBox.Enabled := False;
    SampleTimesNameEdit.Text := Calibration.CalibrationName;

    if Calibration.MinTime > 0.0 then
      SampleTimeEdit.Text := Format('%.2f', [Calibration.MinTime])
    else
      SampleTimeEdit.Text := EmptyStr;
    SampleTimesCheckListBox.ItemIndex := Index;
  finally
    FIgnoreOnChangeEvent := False;
  end;
end;

function TCalibrationDlg.GetCurrentCalibrationListBoxSelection: Integer;
begin
  if FCalibrationType = ctInternalNode then
    Result := CalibrationsCheckListBox.ItemIndex
  else
    Result := SampleTimesCheckListBox.ItemIndex;
end;

function TCalibrationDlg.CalibrationIsSelected: Boolean;
begin
  Result := False;
  if (not Assigned(FCalibrations)) or (FCalibrations.Count = 0) then
    Exit;
  if CalibrationsCheckListBox.ItemIndex < 0 then
    Exit;
  Result := True;
end;

function TCalibrationDlg.MenuItemToCalibrationCategory(aItem: TObject): TCalibrationCategory;
begin
  if not (aItem is TMenuItem) then
    raise Exception.Create('invalid call to MenuItemToCalibrationCategory - expected a TMenuItem but got ' + aItem.ClassName);
  if (aItem = NormalDistItem) or (aItem = NormalDistItem2) or (aItem = NormalDistItem3) then
    Result := ccDensityNormal
  else if (aItem = LogNormalDistItem) or (aItem = LogNormalDistItem2) or (aItem = LogNormalDistItem3) then
    Result := ccDensityLogNormal
  else if (aItem = ExponentialDistItem)  or (aItem = ExponentialDistItem2) or (aItem = ExponentialDistItem3) then
    Result := ccDensityExponential
  else if (aItem = UniformDistItem) or (aItem = UniformDistItem2) or (aItem = UniformDistItem3) then
    Result := ccDensityUniform
  else if (aItem = MinTimeItem) or (aItem = MinTimeItem2) or (aItem = MinTimeItem3) then
    Result := ccMinTimeOnly
  else if (aItem = MaxTimeItem) or (aItem = MaxTimeItem2) or (aItem = MaxTimeItem3) then
    Result := ccMaxTimeOnly
  else if (aItem = FixedTimeItem) or (aItem = FixedTimeItem2) or (aItem = FixedTimeItem3) then
    Result := ccFixedTime
  else if (aItem = FixedRateItem) or (aItem = FixedRateItem2) or (aItem = FixedRateItem3) then
    Result := ccFixedRate
  else
    raise Exception.Create('invalid call to MenuItemToCalibrationCategory - did not get one of the expected menu items');

  { #todo -oglen -ccalibrations : add cMinMaxTime }
end;

procedure TCalibrationDlg.UpdateDivTimeEditLabels(aCalib: TCalibrationTime);
begin
  case aCalib.CalibrationCategory of
    ccDensityNormal, ccDensityExponential, ccDensityUniform, ccDensityLogNormal:
      begin
        DivergenceTimeEdit.EditLabel.Caption := 'CI Lower';
        MaxDivTimeEdit.EditLabel.Caption := 'CI Upper';
      end;
    ccMinTimeOnly:
      begin
        DivergenceTimeEdit.EditLabel.Caption := 'Minimum Time';
      end;
    ccMaxTimeOnly:
      begin
        MaxDivTimeEdit.EditLabel.Caption := 'Maximum Time';
      end;
    ccMinMaxTime:
      begin
        DivergenceTimeEdit.EditLabel.Caption := 'Minimum Time';
        MaxDivTimeEdit.EditLabel.Caption := 'Maximum Time';
      end;
  end;
end;

procedure TCalibrationDlg.ClearDivTimeEdits;
begin
  DivergenceTimeEdit.Text := EmptyStr;
  DivergenceTimeEdit.EditLabel.Caption := 'Minimum Time';
  MaxDivTimeEdit.Text := EmptyStr;
  MaxDivTimeEdit.EditLabel.Caption := 'Maximum Time';
end;

procedure TCalibrationDlg.UpdateProgress(aProgress: Integer; aStatus: String);
begin
  {$IFDEF DARWIN}Exit;{$ENDIF}
  if Assigned(ProgressBar) then
    ProgressBar.Position := Min(100, aProgress);
  if Assigned(StatusBar) and (StatusBar.Panels.Count > 0) then
    StatusBar.Panels[0].Text := aStatus;
  {$IFDEF VISUAL_BUILD}
  if Assigned(ProgressBar) then
  begin
    ProgressBar.Invalidate;
    Application.ProcessMessages;
  end;
  {$ENDIF}
end;

procedure TCalibrationDlg.UpdateStatusBar;
var
  numSelected: Integer = 0;
  numValid: Integer = 0;
  numInvalid: Integer = 0;
begin
  if not Assigned(FCalibrations) then
    Exit;
  FCalibrations.GetCounts(numSelected, numValid, numInvalid);
  if Assigned(StatusBar) and (StatusBar.Panels.Count > 1) then
  begin
    StatusBar.Panels[1].Text := Format('%.0n selected, %.0n valid, %.0n invalid', [numSelected*1.0, numValid*1.0, numInvalid*1.0]);
    StatusBar.InvalidatePanel(1, [ppText]);
  end;
end;

procedure TCalibrationDlg.UpdateStatusBarLayout;
var
  AWidth: Integer;
begin
  if (not Assigned(StatusBar)) or (not Assigned(ProgressBar)) then
    Exit;
  AWidth := Max(100, StatusBar.Canvas.TextWidth(StatusBar.Panels[1].Text)) + 4;
  StatusBar.Panels[1].Width := Max(150, AWidth);
  StatusBar.Panels[0].Width := StatusBar.Width - StatusBar.Panels[1].Width - 2;
end;

procedure TCalibrationDlg.PromptIfTipDatesInNames;
var
  Response: Integer;
  {$IFNDEF DARWIN}{$IFNDEF MSWINDOWS}
  x, y: Integer;
  {$ENDIF}{$ENDIF}
begin
  {$IFNDEF DARWIN}
  {$IFDEF MSWINDOWS}
  Response := MessageDlg('Search for Tip Dates?', 'If sample times are encoded in taxa names, MEGA can extract those dates. Do you want MEGA to search for tip dates in taxa names?', mtInformation, mbYesNo, 0);
  {$ELSE}
  x := Left + Width div 2;
  y := Top + height div 2;
  Response := MessageDlgPos('If sample times are encoded in taxa names, MEGA can extract those dates. Do you want MEGA to search for tip dates in taxa names?', mtInformation, mbYesNo, 0, x, y);
  {$ENDIF}
  if Response = mrYes then
    ParseTipDatesActionExecute(Self);
  {$ENDIF}
end;

procedure TCalibrationDlg.WarnToCheckInferredTipDates;
begin
  ShowMessage('Sample times have been automatically inferred by MEGA. Please carefully check that the inferred sample times are correct');
end;

procedure TCalibrationDlg.SetCalibrations(Calibrations: TCalibrations);
begin
  FCalibrations.Assign(Calibrations);
  ReloadChecklistBoxes;
  if Assigned(Tree) then
  begin
    FCalibrationIndices := FCalibrations.CalibrationIndicesForTE(True);
    Tree.CalibratedNodes := FCalibrationIndices;
    Tree.ShowCalibratedNodeMarker := True;
    Tree.UpdateCalibratedNodes(FCalibrations, True);
    Tree.DrawCalibratedNodeMarkers;
  end;
end;

procedure TCalibrationDlg.SetCalibrationType(const Value: TCalibrationTarget);
begin
  FCalibrationType := Value;
  FCalibrations.CalibrationsType := Value;
  case FCalibrationType of
    ctInternalNode:
      begin
        Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Node Calibration Editor';
        PageControl1.ActivePage := CalibrationsTab;
        CalibrationsTab.TabVisible := True;
        SampleTimesTab.TabVisible := False;

        NewCalibrationFromMrcaButton.Visible := True;
        NewCalibrationFromSelectedNodeButton.Visible := True;
        NewSampleTimeFromMrcaBtn.Visible := False;
        NewSampleTimeFromSelectedNodeBtn.Visible := False;

        NewCalibrationItem.Visible := True;
        CalibrateSelectedNode1.Visible := True;
        CalibrateLeafNodeItem.Visible := False;
        EditCalibrationItem.Visible := True;

        ParseTipDatesAction.Visible := False;
        ParseTipDatesAction.Enabled := False;
      end;
    ctLeafNode:
      begin
        Caption := 'Tip Dates Editor';
        PageControl1.ActivePage := SampleTimesTab;
        SampleTimesTab.TabVisible := True;
        CalibrationsTab.TabVisible := False;
        NewCalibrationFromMrcaButton.Visible := False;
        NewCalibrationFromSelectedNodeButton.Visible := False;
        NewSampleTimeFromMrcaBtn.Visible := True;
        NewSampleTimeFromSelectedNodeBtn.Visible := True;

        NewCalibrationItem.Visible := False;
        CalibrateSelectedNode1.Visible := False;
        CalibrateLeafNodeItem.Visible := True;
        EditCalibrationItem.Visible := False;

        ParseTipDatesAction.Visible := True;
        ParseTipDatesAction.Enabled := True;
      end;
  end;
end;

procedure TCalibrationDlg.SetDisplayMode(Mode: TCalibrationDlgMode);
begin
  FDisplayMode := Mode;
end;

procedure TCalibrationDlg.SetFileName(FileName: String);
begin
  if FCalibrationType = ctLeafNode then
  begin
    if Trim(Filename) = EmptyStr then
    begin
      Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Tip Dates Editor';
      Exit;
    end;
    Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Tip Dates Editor - ' + ExtractFileName(FileName);
  end
  else
  begin
    if Trim(Filename) = EmptyStr then
    begin
      Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Node Calibration Editor';
      Exit;
    end;
    Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Node Calibration Editor - ' + ExtractFileName(FileName);
  end;
end;

procedure TCalibrationDlg.SetOtuNames(aList: TStringList);
begin
  if FCalibrationType = ctInternalNode then
    SetOtuNamesForCalibrations(aList)
  else if FCalibrationType = ctLeafNode then
    SetOtuNamesForSampleTimes(aList)
  else
    raise Exception.Create('invalid calibration type - this is a bug');
end;

procedure TCalibrationDlg.SetOtuNamesForCalibrations(aList: TStringList);
var
  i: Integer;
  aName: String;
begin
  if TaxonAComboBox.Items.Count > 0 then
    TaxonAComboBox.Clear;
  if TaxonBComboBox.Items.Count > 0 then
    TaxonBComboBox.Clear;
  for i := 0 to aList.Count - 1 do
  begin
    aName := aList[i];
    TrimTaxaName2(aName);
    TaxonAComboBox.Items.Add(aName);
    TaxonBComboBox.Items.Add(aName);
  end;
  TaxonAComboBox.ItemIndex := 0;
  TaxonBComboBox.ItemIndex := 1;
end;

procedure TCalibrationDlg.SetOtuNamesForSampleTimes(aList: TStringList);
var
  i: Integer;
  aName: String;
begin
  if SampleTimesTaxonComboBox.Items.Count > 0 then
    SampleTimesTaxonComboBox.Clear;
  for i := 0 to aList.Count - 1 do
  begin
    aName := aList[i];
    TrimTaxaName2(aName);
    SampleTimesTaxonComboBox.Items.Add(aName);
  end;
  SampleTimesTaxonComboBox.ItemIndex := 0;
end;

procedure TCalibrationDlg.SetBottomBars(AStatusBar: TStatusBar; AProgressBar: TProgressBar);
begin
  StatusBar := AStatusBar;
  ProgressBar := AProgressBar;
  AStatusBar.Parent := BottomBottomPanel;
  AStatusBar.Align := alTop;
  AProgressBar.Parent := BottomBottomPanel;
  AProgressBar.Align := alClient;
  AProgressBar.BorderWidth := 1;
  StatusBar.Panels[0].Width := 100;
end;

procedure TCalibrationDlg.UpdateFormSizeForTree(TreeWidth: Integer; TreeHeight: Integer);
var
  aMinHeight: Integer = 0;
begin
  if PageControl1.ActivePage = CalibrationsTab then
    aMinHeight := GroupBox1.Height + GroupBox2.Height + Toolbar1.Height*3 + BottomPanel.Height
  else
    aMinHeight := GroupBox3.Height + GroupBox4.Height + Toolbar1.Height*3 + BottomPanel.Height;
  ClientWidth := EditorPanel.Width + Toolbar3.Width + TreeWidth;
  ClientHeight := Max(aMinHeight, TreeHeight + Toolbar4.Height + BottomPanel.Height);
end;

procedure TCalibrationDlg.SetTreeList(AList: TTreeList);
var
  i: Integer;
begin
  FCalibrations.AssignTreeList(AList);
  if FCalibrationType = ctLeafNode then
  begin
    SampleTimesTaxonComboBox.Items.Clear;
    if AList.NoOfOTUs > 0 then
    begin
      for i := 0 to AList.NoOfOtus - 1 do
        SampleTimesTaxonComboBox.Items.Add(AList.OTUName[i]);
      SampleTimesTaxonComboBox.ItemIndex := 0;
    end;
  end
  else if not Assigned(AnalysisInfo) then
  begin
    TaxonAComboBox.Items.Clear;
    TaxonBComboBox.Items.Clear;
    for i := 0 to AList.NoOfOtus - 1 do
    begin
      TaxonAComboBox.Items.Add(AList.OTUName[i]);
      TaxonBComboBox.Items.Add(AList.OTUName[i]);
    end;
  end;
end;

procedure TCalibrationDlg.ShowInfoActionExecute(Sender: TObject);
begin
  if not Visible then
    Exit;
  TTreeViewForm(TreeViewForm).ActionInfoExecute(Self);
end;

procedure TCalibrationDlg.NewCalibrationFromSelectedNodeButtonContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  FPopupParent := NewCalibrationFromSelectedNodeButton;
end;

procedure TCalibrationDlg.ShowHelpBtnClick(Sender: TObject);
begin
  if ShowHelpBtn.Caption = '<< hide' then
  begin
    ShowHelpBtn.Caption := 'show >>';
    CalibrateNodeBtn.Visible := False;
    AddCalibrationBtn.Visible := False;
    HelpLbl1.Visible := False;
    HelpLbl2.Visible := False;
    HelpLbl3.Visible := False;
    HelpLbl5.Visible := False;
    HelpLbl6.Visible := False;
    HelpLbl7.Visible := False;
    HelpLbl8.Visible := False;
    HelpLbl9.Visible := False;
  end
  else
  begin
    ShowHelpBtn.Caption := '<< hide';
    CalibrateNodeBtn.Visible := True;
    AddCalibrationBtn.Visible := True;
    HelpLbl1.Visible := True;
    HelpLbl2.Visible := True;
    HelpLbl3.Visible := True;
    HelpLbl5.Visible := True;
    HelpLbl6.Visible := True;
    HelpLbl7.Visible := True;
    HelpLbl8.Visible := True;
    HelpLbl9.Visible := True;
  end;
end;

procedure TCalibrationDlg.ShowHelpBtn2Click(Sender: TObject);
begin
  if ShowHelpBtn2.Caption = '<< hide' then
  begin
    ShowHelpBtn2.Caption := 'show >>';
    SampleTimesNewCalibrationBtn.Visible := False;
    SampleTimesCalibrateNodeBtn.Visible := False;
    HelpLabel1.Visible := False;
    HelpLabel2.Visible := False;
    HelpLabel3.Visible := False;
    HelpLabel4.Visible := False;
    HelpLabel5.Visible := False;
    HelpLabel6.Visible := False;
  end
  else
  begin
    ShowHelpBtn2.Caption := '<< hide';
    SampleTimesCalibrateNodeBtn.Visible := True;
    SampleTimesNewCalibrationBtn.Visible := True;
    HelpLabel1.Visible := True;
    HelpLabel2.Visible := True;
    HelpLabel3.Visible := True;
    HelpLabel4.Visible := True;
    HelpLabel5.Visible := True;
    HelpLabel6.Visible := True;
  end;
end;

procedure TCalibrationDlg.SwapActionExecute(Sender: TObject);
begin
  TTreeViewForm(TreeViewForm).ActionSwapExecute(Self);
end;

procedure TCalibrationDlg.TaxonAComboBoxChange(Sender: TObject);
var
  Index: Integer;
  ACalib: TCalibrationTime;
begin
  if FIgnoreOnChangeEvent then
    Exit;
  Index := CalibrationsCheckListBox.ItemIndex;
  if Index < 0 then
    Exit;
  ACalib := FCalibrations.GetCalibration(Index);
  ACalib.NeedsMRCAResolved := True;
  ACalib.NodeA := TaxonAComboBox.Items[TaxonAComboBox.ItemIndex];
  if ACalib.NodeID >= 0 then
    ACalib.NodeID := -1; { if we don't reset it, upstream procedures treat it as already resolved}
  CalibrationNameEdit.Text := ACalib.CalibrationName;
  MrcaNameEdit.Text := ACalib.NodeLabel;
  CalibrationsCheckListBox.Items[Index] := ACalib.CalibrationName;
  CalibrationsCheckListBox.Invalidate;
  if (TaxonAComboBox.ItemIndex >= 0) and (TaxonBComboBox.ItemIndex >= 0) then
  begin
    FinalizeCalibrationFunc(ACalib);
    FCalibrationIndices := FCalibrations.CalibrationIndicesForTE;
    Tree.UpdateCalibratedNodes(FCalibrations);
    if not IndexRemoved(Tree.CalibratedNodes, FCalibrationIndices) then { in this case we can avoid redrawing the whole tree}
      Tree.DrawCalibratedNodeMarkers
    else
      Tree.Invalidate; { have to redraw the tree in order to clear the marker for the deleted/changed calibration}
  end;
end;

procedure TCalibrationDlg.TaxonBComboBoxChange(Sender: TObject);
var
  Index: Integer;
  ACalib: TCalibrationTime;
begin
  if FIgnoreOnChangeEvent then
    Exit;
  Index := CalibrationsCheckListBox.ItemIndex;
  if Index < 0 then
    Exit;
  ACalib := FCalibrations.GetCalibration(Index);
  ACalib.NeedsMRCAResolved := True;
  ACalib.NodeB := TaxonBComboBox.Items[TaxonBComboBox.ItemIndex];
  PleaseSelectLabel.Visible := False;
  if ACalib.NodeID >= 0 then
    ACalib.NodeID := -1; { if we don't reset it, upstream procedures treat it as already resolved}
  CalibrationNameEdit.Text := ACalib.CalibrationName;
  MrcaNameEdit.Text := ACalib.NodeLabel;
  CalibrationsCheckListBox.Items[Index] := ACalib.CalibrationName;
  CalibrationsCheckListBox.Invalidate;
  if Assigned(Tree) and (TaxonAComboBox.ItemIndex >= 0) and (TaxonBComboBox.ItemIndex >= 0) then
  begin
    FinalizeCalibrationFunc(ACalib);
    FCalibrationIndices := FCalibrations.CalibrationIndicesForTE;
    Tree.UpdateCalibratedNodes(FCalibrations);
    if not IndexRemoved(Tree.CalibratedNodes, FCalibrationIndices) then { in this case we can avoid redrawing the whole tree}
      Tree.DrawCalibratedNodeMarkers
    else
      Tree.Invalidate; { have to redraw the tree in order to clear the marker for the deleted/changed calibration}
  end;
end;

procedure TCalibrationDlg.TopologyOnlyActionExecute(Sender: TObject);
begin
  TTreeViewForm(TreeViewForm).ActionTopologyExecute(Self);
end;

procedure TCalibrationDlg.UpdateActionList;
var
  checkListBox: TCheckListBox = nil;
begin
  if FCalibrationType = ctInternalNode then
    checkListBox := CalibrationsCheckListBox
  else if FCalibrationType = ctLeafNode then
    checkListBox := SampleTimesCheckListBox
  else
    raise Exception.Create('invalid calibration type - this is a bug');
  ActionClearCalibrations.Enabled := (FCalibrations.Count > 0);
  case FDisplayMode of
    cdmBeforeConstructTree:
      begin
        if Assigned(FCalibrations) and (FCalibrations.Count > 0) then
        begin
          ExportCalibrationDataAction.Enabled := True;
          if checkListBox.ItemIndex >= 0 then
          begin
            CreateTimeTreeAction.Enabled := True;
            RemoveCalibrationAction.Enabled := True;
          end;
        end
        else
        begin
          ExportCalibrationDataAction.Enabled := False;
          CreateTimeTreeAction.Enabled := False;
          RemoveCalibrationAction.Enabled := False;
        end;
      end;
    cdmAfterConstructTree:
      begin
        if Assigned(FCalibrations) and (FCalibrations.Count > 0) then
        begin
          ExportCalibrationDataAction.Enabled := True;
          if checkListBox.ItemIndex >= 0 then
            RemoveCalibrationAction.Enabled := True;
        end
        else
        begin
          ExportCalibrationDataAction.Enabled := False;
          RemoveCalibrationAction.Enabled := False;
        end;
      end;
  end;
  UpdateStatusBar;
end;

function TCalibrationDlg.ValidateFormData: Boolean;
begin
  if FCalibrationType = ctInternalNode then
    Result := ValidateFormDataForCalibrations
  else if FCalibrationType = ctLeafNode then
    Result := ValidateFormDataForSampleTimes
  else
    raise Exception.Create('Invalid calibrations type expected - this is a bug');
end;

function TCalibrationDlg.ValidateFormDataForCalibrations: Boolean;
var
  TempFloat: Double;
  TempFloat2: Double;
begin
  UpdateActionList;
  Result := False;
  TempFloat := 0.0;
  TempFloat2 := 0.0;

  if (TaxonAComboBox.ItemIndex < 0) or (TaxonAComboBox.Items[TaxonAComboBox.ItemIndex] = EmptyStr) then
  begin
    ShowMessage('Please select a taxon from the list for Taxon A');
    TaxonAComboBox.Enabled := True;
    TaxonAComboBox.SetFocus;
    Exit;
  end;

  if (TaxonBComboBox.ItemIndex < 0) or (TaxonBComboBox.Items[TaxonBComboBox.ItemIndex] = EmptyStr) then
  begin
    ShowMessage('Please select a taxon from the list for Taxon B');
    TaxonBComboBox.Enabled := True;
    TaxonBComboBox.SetFocus;
    Exit;
  end;

  if TaxonAComboBox.Items[TaxonAComboBox.ItemIndex] = TaxonBComboBox.Items[TaxonBComboBox.ItemIndex] then
  begin
    ShowMessage('Taxon B cannot be the same as Taxon A');
    TaxonAComboBox.Enabled := True;
    TaxonBComboBox.Enabled := True;
    TaxonBComboBox.SetFocus;
    Exit;
  end;

  if Trim(CalibrationNameEdit.Text) = EmptyStr then
  begin
    ShowMessage('Please provide a calibration name');
    CalibrationNameEdit.SetFocus;
    Exit;
  end;

  if Pos(#34, CalibrationNameEdit.Text) > 0 then
  begin
    ShowMessage('Invalid character in Calibration Name (double quotes are not supported)');
    CalibrationNameEdit.SetFocus;
    CalibrationNameEdit.SelectAll;
    Exit;
  end;

  if Pos(#39, CalibrationNameEdit.Text) > 0 then
  begin
    ShowMessage('Invalid character in Calibration Name (single quotes are not supported)');
    CalibrationNameEdit.SetFocus;
    CalibrationNameEdit.SelectAll;
    Exit;
  end;

  if Pos(#61, CalibrationNameEdit.Text) > 0 then
  begin
    ShowMessage('Invalid character in Calibration Name ("=" symbol is not supported)');
    CalibrationNameEdit.SetFocus;
    CalibrationNameEdit.SelectAll;
    Exit;
  end;

  if Trim(MrcaNameEdit.Text) <> EmptyStr then
  begin
    if Pos(#34, MrcaNameEdit.Text) > 0 then
    begin
      ShowMessage('Invalid character in MRCA Node Label (double quotes are not supported)');
      MrcaNameEdit.SetFocus;
      MrcaNameEdit.SelectAll;
      Exit;
    end;

    if Pos(#39, MrcaNameEdit.Text) > 0 then
    begin
      ShowMessage('Invalid character in MRCA Node Label (single quotes are not supported)');
      MrcaNameEdit.SetFocus;
      MrcaNameEdit.SelectAll;
      Exit;
    end;

    if Pos(#61, MrcaNameEdit.Text) > 0 then
    begin
      ShowMessage('Invalid character in MRCA Node Label ("=" symbol is not supported)');
      MrcaNameEdit.SetFocus;
      MrcaNameEdit.SelectAll;
      Exit;
    end;
  end;

  if DivergenceTimeEdit.Visible and (Trim(DivergenceTimeEdit.Text) = EmptyStr) and (Trim(MaxDivTimeEdit.Text) = EmptyStr) then
  begin
    ShowMessage('Please provide a valid integer or floating point value (greater than zero) for either the Min or Max Divergence Time');
    DivergenceTimeEdit.SetFocus;
    DivergenceTimeEdit.SelectAll;
    Exit;
  end;

  if DivergenceTimeEdit.Visible and (Trim(DivergenceTimeEdit.Text) <> EmptyStr) then
  begin
    if TryStrToFloat(DivergenceTimeEdit.Text, TempFloat) then
    begin
      if TempFloat <= 0 then
      begin
        ShowMessage('Please provide a valid integer or floating point value (greater than zero) for Min Divergence Time');
        DivergenceTimeEdit.SetFocus;
        DivergenceTimeEdit.SelectAll;
        Exit;
      end;
    end
    else
    begin
      ShowMessage('Please provide a valid integer or floating point value (greater than zero) for Min Divergence Time');
      DivergenceTimeEdit.SetFocus;
      DivergenceTimeEdit.SelectAll;
      Exit;
    end;
  end;

  if MaxDivTimeEdit.Visible and (Trim(MaxDivTimeEdit.Text) <> EmptyStr) then
  begin
    if TryStrToFloat(MaxDivTimeEdit.Text, TempFloat2) then
    begin
      if TempFloat2 <= 0 then
      begin
        ShowMessage('If providing a Max Divergence Time value, it must be a valid integer or floating point value (greater than zero)');
        MaxDivTimeEdit.SetFocus;
        MaxDivTimeEdit.SelectAll;
        Exit;
      end;
      if (Trim(DivergenceTimeEdit.Text) <> EmptyStr) and (TempFloat2 < TempFloat) then
      begin
        ShowMessage('If providing Max and Min Divergence Time values, the Max Divergence Time must be greater than or equal to the Min Divergence Time');
        MaxDivTimeEdit.SetFocus;
        MaxDivTimeEdit.SelectAll;
        Exit;
      end;
    end
    else
    begin
      ShowMessage('If providing a Max Divergence Time value, it must be a valid integer or floating point value (greater than zero)');
      MaxDivTimeEdit.SetFocus;
      MaxDivTimeEdit.SelectAll;
      Exit;
    end;
  end;

  Result := True;
end;

function TCalibrationDlg.ValidateFormDataForSampleTimes: Boolean;
var
  TempFloat: Double;
begin
  UpdateActionList;
  Result := False;
  TempFloat := 0.0;

  if (SampleTimesTaxonComboBox.ItemIndex < 0) or (SampleTimesTaxonComboBox.Items[SampleTimesTaxonComboBox.ItemIndex] = EmptyStr) then
  begin
    ShowMessage('Please select a taxon from the list');
    SampleTimesTaxonComboBox.Enabled := True;
    SampleTimesTaxonComboBox.SetFocus;
    Exit;
  end;

  if Trim(SampleTimesNameEdit.Text) = EmptyStr then
  begin
    ShowMessage('Please provide a calibration name');
    SampleTimesNameEdit.SetFocus;
    Exit;
  end;

  if Pos(#34, SampleTimesNameEdit.Text) > 0 then
  begin
    ShowMessage('Invalid character in Calibration Name (double quotes are not supported)');
    SampleTimesNameEdit.SetFocus;
    SampleTimesNameEdit.SelectAll;
    Exit;
  end;

  if Pos(#39, SampleTimesNameEdit.Text) > 0 then
  begin
    ShowMessage('Invalid character in Calibration Name (single quotes are not supported)');
    SampleTimesNameEdit.SetFocus;
    SampleTimesNameEdit.SelectAll;
    Exit;
  end;

  if Pos(#61, SampleTimesNameEdit.Text) > 0 then
  begin
    ShowMessage('Invalid character in Calibration Name ("=" symbol is not supported)');
    SampleTimesNameEdit.SetFocus;
    SampleTimesNameEdit.SelectAll;
    Exit;
  end;

  if Trim(SampleTimeEdit.Text) = EmptyStr then
  begin
    ShowMessage('Please provide a valid integer or floating point value (greater than zero) for the Sample Time');
    SampleTimeEdit.SetFocus;
    SampleTimeEdit.SelectAll;
    Exit;
  end;

  if Trim(SampleTimeEdit.Text) <> EmptyStr then
  begin
    if TryStrToFloat(SampleTimeEdit.Text, TempFloat) then
    begin
      if (TempFloat < 0) or (TempFloat > (YearOf(Now) + 1)) then
      begin
        ShowMessage(Format('Please provide a valid integer or floating point value (0 < y < %d) for Sample Time', [YearOf(Now)]));
        SampleTimeEdit.SetFocus;
        SampleTimeEdit.SelectAll;
        Exit;
      end;
    end
    else
    begin
      ShowMessage(Format('Please provide a valid integer or floating point value (0 < y < %d) for Sample Time', [YearOf(Now) + 1]));
      SampleTimeEdit.SetFocus;
      SampleTimeEdit.SelectAll;
      Exit;
    end;
  end;

  Result := True;
end;

procedure TCalibrationDlg.ReloadChecklistBoxes;
begin
  if FCalibrationType = ctInternalNode then
    ReloadChecklistBoxForCalibrations
  else if FCalibrationType = ctLeafNode then
    ReloadChecklistBoxForSampleTimes
  else
    raise Exception.Create('invalid calibration type - this is a bug');
  Invalidate;
end;

procedure TCalibrationDlg.ReloadChecklistBoxForCalibrations;
var
  i: Integer;
  ACalib: TCalibrationTime;
begin
  CalibrationsCheckListBox.Clear;
  if FCalibrations.Count = 0 then
  begin
    UpdateActionList;
    Exit;
  end;

  for i := 0 to FCalibrations.Count - 1 do
  begin
    ACalib := FCalibrations.GetCalibration(i);
    CalibrationsCheckListBox.Items.AddObject(ACalib.CalibrationName, ACalib);
    CalibrationsCheckListBox.Checked[i] := ACalib.IsSelected;
  end;
  SetCalibrationIndex(0);
  CalibrationsCheckListbox.ItemIndex := 0;
  CalibrationsCheckListbox.Invalidate;
end;

procedure TCalibrationDlg.ReloadChecklistBoxForSampleTimes;
var
  i: Integer;
  ACalib: TCalibrationTime;
begin
  SampleTimesCheckListBox.Clear;
  if FCalibrations.Count = 0 then
  begin
    UpdateActionList;
    Exit;
  end;

  for i := 0 to FCalibrations.Count - 1 do
  begin
    ACalib := FCalibrations.GetCalibration(i);
    SampleTimesCheckListBox.Items.AddObject(ACalib.CalibrationName, ACalib);
    SampleTimesCheckListBox.Checked[i] := ACalib.IsSelected;
  end;
  SetCalibrationIndex(0);
  SampleTimesCheckListbox.ItemIndex := 0;
  SampleTimesCheckListbox.Invalidate
end;

procedure TCalibrationDlg.RemoveCalibrationActionExecute(Sender: TObject);
var
  Index: Integer;
  Response: Integer;
begin
  if (FCalibrations.Count = 0) or (not Visible) then
    Exit;
  if FCalibrationType = ctInternalNode then
    Index := CalibrationsCheckListBox.ItemIndex
  else
    Index := SampleTimesCheckListBox.ItemIndex;
  if (Index < 0) or (Index >= FCalibrations.Count) then
    Exit;
  Response := MessageDlg('Are you sure you want to delete the selected calibration?',mtConfirmation, mbOKCancel, 0);
  if Response = mrCancel then
    Exit;
  DoRemoveCalibration(Index);
end;
{$ENDIF}


end.
