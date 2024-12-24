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

unit MTreeInputForm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
{$IFDEF VISUAL_BUILD}
uses
  LCLType, LCLIntf, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, MTreeBox, ComCtrls, MegaConsts, MegaVerConsts,
  MegaUtils, MTreeData, ActnList, Menus, ClipBrd, MLongIntList, Math,
  CheckLst, IniPropStorage, MVirtualScrollbox,
  MFindDlg, TreeExplorer_HC, Types, mimageform, MegaPrivateFiles,
  mstringbuilder;

const
  TOO_MANY_TAXA_CUTOFF = 1000; { don't do things like redraw the tree on resize if we have too many taxa}

type
  TEditType = (ERoot, EFlip, EDelete, EAdd);

type

  { TTreeInputForm }

  TTreeInputForm = class(TForm)
    ActionToggleRoot: TAction;
    ActionToggleTopology: TAction;
    ActionBLenEditMode: TAction;
    DoneEditingAction: TAction;
    ActionMrca: TAction;
    ButtonImages: TImageList;
    HelpBtnImages: TImageList;
    ProgressLabel: TLabel;
    MainMenu1: TMainMenu;
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
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    ProgressBar: TProgressBar;
    Separator3: TMenuItem;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    OkAction: TAction;
    CancelAction: TAction;
    HelpAction: TAction;
    ActionList2: TActionList;
    ActionToggleHistory: TAction;
    ActionResize: TAction;
    ActionAutosize: TAction;
    ActionCopy: TAction;
    ActionFind: TAction;
    ActionFont: TAction;
    ActionSave: TAction;
    ActionNewTree: TAction;
    ActionUpload: TAction;
    ActionRemoveOTU: TAction;
    ActionRoot: TAction;
    ActionFlip: TAction;
    ActionAddOTU: TAction;
    ActionUndo: TAction;
    IniPropStorage1: TIniPropStorage;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    someSpacer: TMenuItem;
    MenuItem1: TMenuItem;
    HideMappedTreeNamesChk: TCheckBox;
    OnlyUnMappedChk: TCheckBox;
    NameMapListView: TListView;
    OKCancelPanel: TPanel;
    LeftPanel: TPanel;
    RootTBtn: TToolButton;
    FlipTBtn: TToolButton;
    AutosizeBtn: TToolButton;
    MouseScrollResizeBtn: TToolButton;
    HistoryVisibleBtn: TToolButton;
    NewTreeBtn: TToolButton;
    SaveTBtn: TToolButton;
    FontTBtn: TToolButton;
    FindTBtn: TToolButton;
    CopyTBtn: TToolButton;
    AdvancedMap: TToolButton;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    OkBtn: TToolButton;
    BLenEditBtn: TToolButton;
    RootToggleBtn: TToolButton;
    TopologyOnlyBtn: TToolButton;
    UndoTBtn: TToolButton;
    ToolButton1: TToolButton;
    Splitter1: TSplitter;
    RightPanel: TPanel;
    Splitter2: TSplitter;
    TreeContainer: TPanel;
    TreeToolBar: TToolBar;
    TreeEditBox: TTreeEditBox;
    CheckImageList: TImageList;
    LeftSideLabel: TLabel;
    TreeToActiveMatchLBx: TCheckListBox;
    OpenDlg: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ActionList1: TActionList;
    FontDialog1: TFontDialog;
    TreeRightClick: TPopupMenu;
    DeleteOTU1: TMenuItem;
    SetasRoot1: TMenuItem;
    FlipSubbranch1: TMenuItem;
    Find1: TMenuItem;
    Undo1: TMenuItem;
    ChangeFont1: TMenuItem;
    AddOTU1: TMenuItem;
    EnterExitPanMode: TAction;
    ScrollSizeContainer: TPanel;
    EditHistory: TListBox;
    EditHistoryLbl: TLabel;
    RecentTreesBtn: TToolButton;
    RecentMenu: TPopupMenu;
    RecentTrees1: TMenuItem;
    UploadTBtn: TToolButton;
    EditHistoryPanel: TPanel;
    procedure ActionToggleRootExecute(Sender: TObject);
    procedure ActionToggleTopologyExecute(Sender: TObject);
    function IsEditingText: Boolean;
    function EditOperationHintText: AnsiString;
    procedure ActionAutosizeExecute(Sender: TObject);
    procedure ActionBLenEditModeExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionFindExecute(Sender: TObject);
    procedure ActionFontExecute(Sender: TObject);
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionMrcaExecute(Sender: TObject);
    procedure ActionNewTreeExecute(Sender: TObject);
    procedure ActionResizeExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionToggleHistoryExecute(Sender: TObject);
    procedure ActionUploadExecute(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DoneEditingActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure TreeEditBoxClick(Sender: TObject);
    procedure TreeEditBoxDblClick(Sender: TObject);
    procedure NameMapListViewClick(Sender: TObject);
    procedure TreeToActiveMatchLBxClickCheck(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MouseScrollResizeBtnClick(Sender: TObject);
    { DO NOT REMOVE -  needed for the context menu and highlighting taxa names}
    procedure TreeEditBoxMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeEditBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    { DO NOT REMOVE -  these are needed for dragging names from the list view to the tree}
    procedure TreeEditBoxNameDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TreeEditBoxNameDragDrop(Sender, Source: TObject; X, Y: Integer);
    { END DO NOT REMOVE}
    procedure ActionUndoExecute(Sender: TObject);
    procedure ActionAddOTUExecute(Sender: TObject);
    procedure ActionRemoveOTUExecute(Sender: TObject);
    procedure ActionRootExecute(Sender: TObject);
    procedure ActionFlipExecute(Sender: TObject);
    procedure CopyTBtnClick(Sender: TObject);
    procedure FindTBtnClick(Sender: TObject);
    procedure FontTBtnClick(Sender: TObject);
    procedure NewTreeBtnClick(Sender: TObject);
    procedure TreeEditBoxEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure NameMapListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure NameMapListViewCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    function SelectTreeFile: Boolean;
    procedure NameMapListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure NameMapListViewMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure InstructionsREditMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure OKCancelPanelMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure OKBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CancelBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure HelpBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TreeToolBarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ToolButton1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure HelpBtnClick(Sender: TObject);
    procedure OnlyUnmappedChkClick(Sender: TObject);
    procedure NameMapListViewCompare(Sender: TObject; Item1,
      Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure HideMappedTreeNamesChkClick(Sender: TObject);
    procedure HistoryVisibleBtnClick(Sender: TObject);

    procedure RecentTreesClick(Sender: TObject; const ItemTextW: String);
    procedure EditHistoryDblClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FNodeLabelFont: TFont;
    FOTU_Font: TFont;
    FCaptionFont: TFont;
    FPrivateFont: TFont;
    FStatsFont: TFont;
    FBLensFont: TFont;
    FTimesFont: TFont;
    FScaleFont: TFont;
    FCharStateFont: TFont;

    FLastUpdateTime: TDateTime;
    FLastProgressUpdateTime: TDateTime;
    FRecentTreesList: TStringList;
    FIsRootedTree: Boolean;
    FSourceDataFilename: String;
    userCanceled: Boolean;
    openFileOnShow: String;
    MyFindDlg : TFindDlg;

    TreeEdited: Boolean;
    NoOfUserDefOTUs: Integer;
    DispTaxaList: TLongintList;
    FTreeNameHighlighted: String;
    FCurrentNewickFile: String;
    FNewNodeNum: Integer;
    FActiveNames: TStringList;  // Each active name will have a value from TreeName, or blank if nothing mapped
    FTreeNames  : TStringList;  // Each tree name will have a value from ActiveName or blank if ActiveName dropped on tree
    FStartPixelsPerOTU: Integer;
    FStartTreeWidth: Integer;
    FIsUpdating: Boolean;
    FDragStartPoint: TPoint;
    FTTAChkState: Array of TCheckBoxState; // used in conjunction with SetStateOfTTAChk and FindChangedTTAIndex to track the state of the checks and grays in the TreToActiveMatchLBx
    HiddenList: TStringList;
    FShowAsTopologyEditor: Boolean;
    AutosizeActive: Boolean;

    { the 5 below are just stubs to be compatible with TTreeBox}
    procedure BeginRefresh;
    procedure EndRefresh;
    procedure SetShowAsTopologyEditor(AValue: Boolean);
    procedure UpdateRefreshProgress(AProgress: Integer);
    procedure UpdateRefreshStatus(AStatus: TTreeRebuildPhase);
    { end stubs}
    procedure InitTreeBoxFonts;
    procedure ClearTreeBoxFonts;
    procedure SetupTreeBox;
    procedure ScrollBoxDblClick(Sender: TObject);
    procedure ScrollBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure addTreeToHistory(EType: TEditType);
    procedure ShowMapDropdownAt(CursorPoint: TPoint);
    procedure UpdateDispTaxaList;
    function ConstructSelTaxaList: Boolean;
    procedure SetActiveDataAndTreeNameMatch(AActiveName, ATreeName: AnsiString);
    procedure DropDownAtItem(SelectedMapListItem: TListItem);
    procedure SetHighlight;
    function TreeJumpToTreeName(TreeName: AnsiString; ColorResult: Boolean = True): Boolean;
    procedure ResizeTreeToFit;
    procedure SetTreeNameForActive(ActiveName: AnsiString; TreeName: AnsiString);
    procedure ClearActiveNameOnTreeName(TreeName: AnsiString);
    procedure ClearTreeNameFromNameMapList(TreeName: AnsiString);
    procedure SetStateOfTTAChkArray;
    procedure SetupTTAChkStates;
    function FindChangedTTAIndex: Integer;
    function GenerateRandomTree(NumNodes: Integer = 0): TTreeData;
    function GenerateScratchTree: TTreeData;
    procedure ClearTree;
    function ClearPrevHighlightedTaxa: Boolean;
    procedure EnterResizeMode;
    procedure QuitResizeMode;
    function InResizeMode: Boolean;
    function AdvancedAutomaticMap(TreeStr:AnsiString; ActiveStrList: TStringList):Integer;
    procedure CollectUnusedSequences;
    procedure RecentFilesMenuItemClick(Sender: TObject);
    procedure RestoreRecentFilesList;
    function SaveRecentFilesList: Boolean;
    procedure DoneEditingNotify(Sender: TObject);
    procedure UpdateStatusBar;
    procedure RefreshListView;
    {$IFDEF DEBUG}
    procedure UndoStackToDeveloperConsole(aMsg: String);
    {$ENDIF}
  public
    HasUnusedTreeTaxa: Boolean;
    UnusedSequences: TStringList;
    AskForTreeOnShow: Boolean;

    function NumTaxa: Integer;
    function HasUnusedSequenceData: Boolean;
    procedure TreeToOpenTreeOnShow(FileName: String);
    procedure SetActiveDataNames(AList: TStringList);
    procedure SetTreeFromFile(ATreeFile: String);
    procedure SetTreeFromNewick(ANewickTree: AnsiString);
    procedure SetTreeData(MyTreeData: TTreeData; ANamesList: TStringList; IsRooted: Boolean = False);
    procedure InitializeActiveDataAndTreeNameMatches;
    function  DoesTreeHaveAllActiveNames: Boolean;
    procedure PruneUnwantedTreeNodesAndNames;
    function PruneUnwantedTreeNodesAndNamesNoGui(AFileName: String; AExpNameStrList: TStringList;var aNewickStr: String; var aIsRooted: Boolean; var aMsg: String): Boolean;
    procedure RebuildTreeNames;
    procedure OpenTree(aFileName: String);
    procedure GetTreeData(var treedata: TTreeData);
    function  GetNewickTree: AnsiString;
    procedure UpdateProgress(aStatus: String; aProgress: Integer);
    procedure ClearProgress;

    property NewickTree: AnsiString read GetNewickTree;
    property FileName: String read FCurrentNewickFile;
    property SourceDataFilename: String read FSourceDataFilename write FSourceDataFilename;
    property IsRootedTree: Boolean read FIsRootedTree;
    property ShowAsTopologyEditor: Boolean read FShowAsTopologyEditor write SetShowAsTopologyEditor;
  end;

type

  { TTreeInputSession }

  TTreeInputSession = class(TObject)
    private
      FStrBuilder: TMegaStringBuilder;
    public
      Newick: String;
      TaxaNamesMap: TStringList;
      Constructor Create;
      Destructor Destroy; override;
      procedure FillFromForm(Form: TTreeInputForm);
      procedure FillUsingNamesList(aNamesList: TStringList);
      procedure RestoreToForm(Form: TTreeInputForm);
      function Serialize: String;
      procedure Deserialize(Ser: String);
end;

var
  TempSession: TTreeInputSession;
  TreeInputForm: TTreeInputForm;
  SX: Integer;
  SY: Integer;
  LX: Integer;
  LY: Integer;

  function StringHeight(TheStr: String; TheFont: TFont): Integer;

{$ENDIF}
implementation

{$IFDEF VISUAL_BUILD}
uses
  {$IFDEF DEBUG}mdeveloper_console, {$ENDIF}
  MTreeList, RegExpr, Levenshtein, mhelpfiles, mhelpkeywords, mshortcutshelper,
  dateutils, mega_main, MMegaWindowInfo, StringUtils;

{$R *.lfm}

function StringHeight(TheStr: String; TheFont: TFont): Integer;
var
  BM: TBitmap = nil;
begin
  try
    Result := 0;
    BM := TBitmap.Create;
    BM.Canvas.Font := TheFont;
    Result := BM.Canvas.TextHeight(TheStr);
  finally
    if Assigned(BM) then
      BM.Free;
  end;
end;

procedure TTreeInputForm.FormCreate(Sender: TObject);
begin
  FRecentTreesList := TStringList.Create;
  FLastUpdateTime := Now;
  FLastProgressUpdateTime := Now;
  FIsRootedTree := False;
  UpdateShortcutsForMacOs(ActionList1);
  HasUnusedTreeTaxa := False;
  FIsUpdating := False;
  HiddenList := TStringList.Create;
  UnusedSequences := TStringList.Create;
  userCanceled := False;
  myFindDlg := nil;
  DispTaxaList := TLongIntList.Create;
  FActiveNames := TStringList.Create;
  FTreeNames   := TStringList.Create;
  FNewNodeNum := 1;
  FShowAsTopologyEditor := False;
  openFileOnShow := EmptyStr;
  SetupTreeBox;
  HelpContext := RH_Tree_Topology_Editor;
  ImageForm.UpdateImgList(Self);
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
  RestoreRecentFilesList;
  SaveDialog1.Filter := NewickFilesFilter;
  SaveDialog1.FilterIndex := 0;
  AutoSizeActive := False;
end;

procedure TTreeInputForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  nCount     : integer;
begin
  nCount := Length(FileNames);
  if nCount > 1 then
  begin
    ShowMessage('You may only drag and drop 1 tree file at a time.');
    Exit;
  end;
  OpenTree(Filenames[0]);
end;

function TTreeInputForm.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
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
      ShowMessage('Application Error - failed to initialize the help browser: ' + E.Message);
  end;
end;

procedure TTreeInputForm.ActionUploadExecute(Sender: TObject);
begin
  SelectTreeFile;
end;

procedure TTreeInputForm.CancelBtnClick(Sender: TObject);
begin
  if (fsModal in FormState) then
    ModalResult := mrCancel
  else
    Close;
end;

procedure TTreeInputForm.DoneEditingActionExecute(Sender: TObject);
begin
  DoneEditingNotify(Sender);
end;

procedure TTreeInputForm.FormActivate(Sender: TObject);
begin
  if FShowAsTopologyEditor then
  begin
    if TreeEditBox.ShowBLen then
      ActionBLenEditMode.Hint := 'Toggle editing of branch lengths'
    else if TreeEditBox.ShowDivergenceTimes then
      ActionBLenEditMode.Hint := 'Toggle editing of node ages';
  end
  else
    ActionBLenEditMode.Hint := 'Toggle display of branch lengths';
  ActionToggleHistory.Visible := IsDeveloper;
  Toolbar1.Images := ImageForm.GetDialogButtonImageList;
  Toolbar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  Toolbar1.ImagesWidth := Toolbar1.ButtonWidth;
  UpdateStatusBar;
end;

procedure TTreeInputForm.FormClose(Sender: TObject;var CloseAction: TCloseAction);
begin
  RemoveWindowFromTray(Self);
end;

procedure TTreeInputForm.ActionNewTreeExecute(Sender: TObject);
begin
  GenerateRandomTree;
end;

procedure TTreeInputForm.ActionResizeExecute(Sender: TObject);
begin
  if not InResizeMode then
    EnterResizeMode
  else
    QuitResizeMode;
end;

procedure TTreeInputForm.ActionFontExecute(Sender: TObject);
begin
  FontDialog1.Font.Assign(TreeEditBox.OTU_Font);
  if FontDialog1.Execute then
  begin
    TreeEditBox.OTU_Font.Assign(FontDialog1.Font);
    TreeEditBox.AttribList[0].Font.Assign(FontDialog1.Font);
  end;
  TreeEditBox.Invalidate;
end;

procedure TTreeInputForm.ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
begin
  Handled := True;
  if MilliSecondsBetween(Now, FLastUpdateTime) < 300 then
    Exit;
  FLastUpdateTime := Now;

  ActionToggleTopology.Checked := TreeEditBox.ShowTopologyOnly;
  ActionToggleRoot.Checked := TreeEditBox.IsRooted;
  if ActionToggleRoot.Checked then
    ActionToggleRoot.Hint := 'Currently treating as rooted tree. Will export rooted Newick files and automatically adjust branch lengths when rearranging nodes.'
  else
    ActionToggleRoot.Hint := 'Currently treating as unrooted tree. Will export unrooted Newick files and leave branch lengths as-is when rearranging nodes.';
  ActionAutosize.Checked := AutosizeActive;
  ActionResize.Checked := InResizeMode;

  if IsEditingText then
  begin
    ActionCopy.Enabled := False;
    ActionRoot.Enabled := False;
    ActionFlip.Enabled := False;
    ActionUndo.Enabled := False;
    ActionAddOTU.Enabled := False;
    ActionRemoveOTU.Enabled := False;
    ActionAutosize.Enabled := False;
  end
  else
  begin
    ActionCopy.Enabled := True;
    ActionRoot.Enabled := TreeEditBox.FocusedIndex > 0;
    ActionFlip.Enabled := TreeEditBox.FocusedIndex > TreeEditBox.NoOfOTUs;
    ActionUndo.Enabled := TreeEditBox.CanUndo;

    if not FShowAsTopologyEditor then
      ActionAddOTU.Enabled :=  (TreeEditBox.NoOfOTUs < NameMapListView.Items.Count) and (TreeEditBox.FocusedIndex > 0)
    else
      ActionAddOTU.Enabled := True;
    ActionRemoveOTU.Enabled :=  ((TreeEditBox.FocusedNameIndex > 0) or TreeEditBox.FocusedOnOtuBranch) and (TreeEditBox.NoOfOTUs > 2);
    ActionAutosize.Enabled := True;
  end;
  ActionUndo.Hint := 'Undo ' + EditOperationHintText;
  ActionUndo.Caption := '&Undo ' + EditOperationHintText;
  RecentTreesBtn.Enabled := RecentMenu.Items.Count > 0;
end;

function TTreeInputForm.IsEditingText: Boolean;
begin
  Result := False;
  if Screen.ActiveControl is TEdit then
    Result := True;
end;

procedure TTreeInputForm.ActionToggleTopologyExecute(Sender: TObject);
begin
  TreeEditBox.ShowTopologyOnly := not TreeEditBox.ShowTopologyOnly;
  TreeEditBox.Refresh;
end;

procedure TTreeInputForm.ActionToggleRootExecute(Sender: TObject);
begin
  TreeEditBox.SetIsRooted(not TreeEditBox.IsRooted);
  TreeEditBox.Invalidate;
end;

function TTreeInputForm.EditOperationHintText: AnsiString;
var
  EditOp: TTreeEditOperation = nil;
begin
  Result := '';

  if (TreeEditBox.UndoStack.Count < 1) then
    Exit;

  EditOp := TTreeEditOperation(TreeEditBox.UndoStack.Peek);

  case EditOp.OperationType of
    uSetOTUName, uSetOTUPrivateName: Result := 'Rename Taxon';
    uMoveNode: Result := 'Move Node';
    uRemoveOTU: Result := 'Delete Taxon';
    uInsertOTU: Result := 'Add Taxon';
    uFlipCluster: Result := 'Flip Subtree';
    uFlipAllCluster: Result := 'Flip Subtree';
    uMakeRootOnMidPoint: Result := 'Root On Midpoint';
    uMakeRootByOutgroup: Result := 'Root By Outgroup';
    uMakeRootOnBranch: Result := 'Root On Branch';
    uEditBranchLength: Result := 'Edit Branch Length';
  end;
end;

procedure TTreeInputForm.ActionMrcaExecute(Sender: TObject);
var
  temp: String = '';
  tokens: TStringList = nil;
  mrca: Integer = -1;
  aName: String = '';
  i: Integer = -1;
begin
  try
    try
      if InputQuery('Find MRCA of taxa', 'Enter a comma delimited list of taxa names whose MRCA is to be found', temp) then
      begin
        tokens := TStringList.Create;
        SplitOnSingleCharFaster(temp, ',', tokens, False);
        if tokens.Count > 0 then
          for i := 0 to tokens.Count - 1 do
          begin
            aName := tokens[i];
            TrimTaxaName2(aName);
            tokens[i] := aName;
          end;
        mrca := TreeEditBox.FindMrca(tokens);
        if mrca >= 1 then
        begin
          TreeEditBox.FocusOnNode(mrca);
          TreeEditBox.Invalidate;
        end
        else
          ShowMessage('MRCA was not found for the specified taxa');
      end;
    except
      on E:Exception do
        ShowMessage('Application error when navigating to MRCA of taxa group: ' + E.Message);
    end;
  finally
    if Assigned(tokens) then
      tokens.Free;
  end;
end;

procedure TTreeInputForm.ActionFindExecute(Sender: TObject);
begin
  if MyFindDlg = nil then
  begin
    MyFindDlg := TFindDlg.Create(Self);
    MyFindDlg.IsNodeSearch := False;
    MyFindDlg.Tree := TreeEditBox;
  end;
  MyFindDlg.Show;
end;

procedure TTreeInputForm.ActionCopyExecute(Sender: TObject);
begin
  TreeEditBox.CopyImageToClipBoard;
end;

procedure TTreeInputForm.ActionAutosizeExecute(Sender: TObject);
begin
  AutosizeActive := not AutosizeActive;
  if AutosizeActive then
    ResizeTreeToFit;
end;

procedure TTreeInputForm.ActionBLenEditModeExecute(Sender: TObject);
begin
  if FShowAsTopologyEditor then
  begin
    if TreeEditBox.ShowBLen then
    begin
       TreeEditBox.ShowBLen := False;
       TreeEditBox.ShowDivergenceTimes := True;
       ActionBLenEditMode.Hint := 'Toggle editing of node ages';
    end
    else
    begin
      TreeEditBox.ShowBLen := True;
      TreeEditBox.ShowDivergenceTimes := False;
      ActionBLenEditMode.Hint := 'Toggle editing of branch lengths';
    end;
  end
  else
    TreeEditBox.ShowBLen := not TreeEditBox.ShowBlen;
  ActionBLenEditMode.Checked := TreeEditBox.ShowBlen;
  TreeEditBox.Invalidate;
end;

procedure TTreeInputForm.ActionSaveExecute(Sender: TObject);
var
  aWriter: TStringList = nil;
begin
  if (not TreeEditBox.IsRooted) then
  begin
     if MessageDlg('Your tree is currently set as an unrooted tree.  Some branch length information at the root may be lost if you attempt to save it as unrooted.  Would you like to change it to a rooted tree before saving?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      TreeEditBox.SetIsRooted(True);
      TreeEditBox.Invalidate;
    end;
  end;

  try
    try
      if DirectoryExists(ExtractFileDir(SaveDialog1.FileName)) then
        SaveDialog1.Filename := ExtractFileName(SaveDialog1.Filename);
      aWriter := TStringList.Create;
      if FShowAsTopologyEditor then
      begin
        if SaveDialog1.Execute then
        begin
          aWriter.Add(TreeEditBox.NewickTree);
          aWriter.SaveToFile(SaveDialog1.FileName);
        end;
      end
      else
      begin
        if SaveDialog1.Execute then
        begin
          with TStringList.Create do
          begin
            aWriter.Add(TreeEditBox.NewickPrivateTree);
            aWriter.SaveToFile(SaveDialog1.FileName);
            aWriter.Clear;
            if(MessageDlg('Would you like to save a copy of the tree with the original tree names also?', mtConfirmation, [mbYes, mbNo], 0) in [mrYes]) then
            begin
              SaveDialog1.FileName := NextAvailableFilename(ChangeFileExt(SaveDialog1.Filename, '_orig.nwk'));
              if SaveDialog1.Execute then
              begin
                aWriter.Add(TreeEditBox.NewickTree);
                aWriter.SaveToFile(SaveDialog1.FileName);
              end;
            end;
          end;
        end;
      end;
    except
      on E:Exception do
        ShowMessage('Application error when trying to save the newick tree to a file: ' + E.Message);
    end;
  finally
    if Assigned(aWriter) then
      aWriter.Free;
  end;
end;

procedure TTreeInputForm.ActionToggleHistoryExecute(Sender: TObject);
begin
  EditHistoryPanel.Visible := not EditHistoryPanel.visible;
end;

procedure TTreeInputForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DispTaxaList);
  FreeAndNil(FActiveNames);
  FreeAndNil(FTreeNames);
  FreeAndNil(MyFindDlg);
  if Assigned(UnusedSequences) then
    UnusedSequences.Free;
  if Assigned(HiddenList) then
    HiddenList.Free;
  if Assigned(FRecentTreesList) then
    FRecentTreesList.Free;
  ClearTreeBoxFonts;
end;

procedure TTreeInputForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
const
  WHEEL_DELTA = 120;
var
  Placeholder: Integer;
  MyDelta: Integer;
begin
  Placeholder := 0;
  MyDelta := WheelDelta * -15 div WHEEL_DELTA;
  TreeEditBox.ScrollBy(Placeholder, MyDelta);
end;

procedure TTreeInputForm.SetActiveDataNames(AList: TStringList);
var
  i: Integer;
  aItem: TListItem = nil;
begin
  FActiveNames.Clear;
  if AList.Count > 0 then
    for i := 0 to AList.Count - 1 do
    begin
      FActiveNames.Add(AList[i] + '=');
      aItem := NameMapListView.Items.Add;
      aItem.Caption := AList[i];
      aItem.SubItems.Add(EmptyStr);
    end;
end;

procedure TTreeInputForm.SetTreeFromNewick(ANewickTree: AnsiString);
var
  i: Integer;
  AInputTree : TTreeList = nil;
  ANamesList : TStringList = nil;
begin
  try
    AInputTree := TTreeList.Create;
    ANamesList := TStringList.Create;
    AInputTree.ImportFromNewick(ANewickTree, nil);
    if AInputTree.Count = 0 then
    begin
      ShowMessage('There was an error reading in the newick tree.  Please inspect the input tree file.');
      Exit;
    end;
    FIsRootedTree := AInputTree.isRooted;
    for i:= 0 to AInputTree.NoOfOTUs-1 do
      ANamesList.Add(AInputTree.OTUName[i]);
    UpdateDispTaxaList;
    SetTreeData(AInputTree[0], ANamesList, AInputTree.isRooted);
    TreeEdited := False;
  finally
    FreeAndNil(ANamesList);
    FreeAndNil(AInputTree);
  end;
end;

procedure TTreeInputForm.SetTreeFromFile(ATreeFile: String);
var
  FileSList: TStringList;
begin
  if not FileExists(ATreeFile) then
    raise Exception.Create(ATreeFile+' does not exist.');
  FileSList := TStringList.Create;
  try
    FileSList.LoadFromFile(ATreeFile);
    SetTreeFromNewick(FileSList.Text);
    FCurrentNewickFile := ATreeFile;
  finally
    FreeAndNil(FileSList);
  end;
end;

procedure TTreeInputForm.SetTreeData(MyTreeData: TTreeData; ANamesList: TStringList; IsRooted: Boolean = False);
var
  AEmptyList: TStringList = nil;
  i: Integer;
begin
  try
    AEmptyList := TStringList.Create; // this indicates that there are no private names
    for i:=0 to ANamesList.Count-1 do
      AEmptyList.Add(EmptyStr);
    TreeEditBox.SetTreeData(FShowAsTopologyEditor, MyTreeData, ANamesList, AEmptyList, IsRooted);
    TreeEditBox.ShowBLen := (MyTreeData.isBLen and FShowAsTopologyEditor);
    TreeEditBox.BLenDecimals := 2;
    TreeEditBox.DecimalDisplayMode := ffGeneral;
    TreeEditBox.ShowTopologyOnly := True;
    FTreeNames.Clear;
    for i:=0 to ANamesList.Count-1 do
    begin
      UpdateProgress('Updating View...', Round((i + 1)/ANamesList.Count*100));
      if Pos('=', ANamesList[i]) <> 0 then
        FTreeNames.Add(ANamesList[i])
      else
        FTreeNames.Add(ANamesList[i]+'=');
    end;
  finally
    ClearProgress;
    if Assigned(AEmptyList) then
      AEmptyList.Free;
  end;
end;

procedure TTreeInputForm.InitializeActiveDataAndTreeNameMatches;
var
  i: Integer = -1;
  treeNameIndex: Integer = -1;
  tindex: Integer = -1;
  AActiveName : AnsiString = '';
  cleanActiveNames: TStringList = nil;
  cleanTreeNames: TStringList = nil;
  aItem: TListItem = nil;
begin
  if FShowAsTopologyEditor then
    Exit;
  if FActiveNames.Count = 0 then
    raise Exception.Create('Empty list of active data names');

  try
    cleanActiveNames := RegexReplaceAStringList(FActiveNames, '[^A-Za-z0-9]', '_'); // strip away all but alphanumeric
    cleanTreeNames := RegexReplaceAStringList(FTreeNames, '[^A-Za-z0-9]', '_');

    UpdateProgress('Mapping taxa names', 1);
    for i := 0 to FActiveNames.Count - 1 do
    begin
      UpdateProgress('Mapping taxa names', Round((i + 1)/FActiveNames.Count*100));
      AActiveName := cleanActiveNames.Names[i];
      //see if this name appears in the primary TreeNames anywhere
      treeNameIndex   := cleanTreeNames.IndexOfName(AActiveName);
      if treeNameIndex >= 0 then
      begin
        AActiveName := FActiveNames.Names[i]; // Use the real name, rather than the sanitized name.
        if (Length(FTreeNames.Values[AActiveName]) > 0) or
           (Length(FActiveNames.Values[AActiveName]) > 0) then
              raise Exception.Create('Duplicate names found: '+ AActiveName);
        FActiveNames.Values[AActiveName]  := AActiveName;
        FTreeNames.Values[AActiveName]    := AActiveName;
        aItem := NameMapListView.FindCaption(0, AActiveName, False, True, False);
        if Assigned(aItem) then
          aItem.SubItems[0] := AActiveName;
        TreeEditBox.SetOTUPrivateName(treeNameIndex + 1, AActiveName, True);
      end;
    end;

    UpdateProgress('Locating unmatched taxa', 1);
    for i := 1 to TreeEditBox.NoOfOTUs do
    begin
      UpdateProgress('Locating unmatched taxa', Round(i/TreeEditBox.NoOfOTUs*100));
      if TreeEditBox.OTUPrivateName[i] = EmptyStr then
      begin
        if isDeveloper then
        begin
          tindex := AdvancedAutomaticMap(cleanTreeNames.Names[i-1], cleanActiveNames);
          if tindex <> -1 then
          begin
            AActiveName := FActiveNames.Names[tindex];
            FActiveNames.Values[AActiveName] := TreeEditBox.OtuName[i];
            FTreeNames.Values[TreeEditBox.OtuName[i]] := AActiveName;
            NameMapListView.FindCaption(0, AActiveName, false, True, false).SubItems.Strings[0] := TreeEditBox.OtuName[i];
            TreeEditBox.SetOTUPrivateName(i, AActiveName, True);
          end
          else
            TreeEditBox.SetOTUPrivateName(i, UndefinedStr, True);
        end
        else
          TreeEditBox.SetOTUPrivateName(i, UndefinedStr, True);
      end;
    end;
  finally
    ClearProgress;
    if Assigned(cleanActiveNames) then
      cleanActiveNames.Free;
    if Assigned(cleanTreeNames) then
      cleanTreeNames.Free;
  end;
end;

function TTreeInputForm.InResizeMode: Boolean;
begin
  Result := (MouseScrollResizeBtn.ImageIndex = 151);
end;

procedure TTreeInputForm.SetActiveDataAndTreeNameMatch(AActiveName, ATreeName: AnsiString);
var
  PrevActiveName, PrevTreeName: AnsiString;
begin
  // if the current ActiveName has previous association with ATreeName then disconect it
  if Length(AActiveName) > 0 then
    PrevTreeName := FActiveNames.Values[AActiveName];
  if Length(PrevTreeName) > 0 then
    FTreeNames.Values[PrevTreeName] := EmptyStr; // so it is disconnected

  // if the curent ATreeName has previous assocaition then  disconnect it
  if Length(ATreeName) > 0 then
    PrevActiveName := FTreeNames.Values[ATreeName];
  if Length(PrevActiveName) > 0 then
    FActiveNames.Values[PrevActiveName] := EmptyStr; // so it is disconnected

  // now assign as appropriate
  if Length(AActiveName) > 0 then
    FActiveNames.Values[AActiveName] := ATreeName;
  if Length(ATreeName) > 0 then
    FTreeNames.Values[ATreeName]     := AActiveName;
end;

// tests if the object contains all active names
procedure TTreeInputForm.CollectUnusedSequences;
var
  i: Integer;
begin
  UnusedSequences.Clear;
  with FActiveNames do
    for i := 0 to Count - 1 do
      if Length(Values[Names[i]]) = 0 then
        UnusedSequences.Add(Names[i]);
end;

procedure TTreeInputForm.RecentFilesMenuItemClick(Sender: TObject);
var
  aItem: TMenuItem = nil;
  treeName: String = '';
  SessionStr : String;
  aSession: TTreeInputSession = nil;
begin
  try
    if Sender is TMenuItem then
    begin
      aItem := TMenuItem(Sender);
      treeName := aItem.Caption;
      if FRecentTreesList.IndexOfName(treeName) >= 0 then
      begin
        SessionStr := FRecentTreesList.Values[treeName];
        aSession := TTreeInputSession.Create;
        aSession.Deserialize(SessionStr);
        aSession.RestoreToForm(Self);
        FCurrentNewickFile := treeName;
      end
      else
        ShowMessage('MEGA was unable to locate the saved tree session');
    end;
  finally
    if Assigned(aSession) then
      aSession.Free;
  end;
end;

procedure TTreeInputForm.RestoreRecentFilesList;
var
  i: Integer = -1;
  aFilename: String = '';
  aMenuItem: TMenuItem = nil;
begin
  try
    aFilename := GetPrivateFile(mfTreeEditorMruFiles, False);
    if FileExists(aFilename) then
    begin
      FRecentTreesList.LoadFromFile(aFilename);
      if FRecentTreesList.Count > 0 then
      begin
        for i := 0 to FRecentTreesList.Count - 1 do
        begin
          aMenuItem := TMenuItem.Create(RecentMenu);
          aMenuItem.Caption := FRecentTreesList.Names[i];
          aMenuItem.OnClick := RecentFilesMenuItemClick;
          RecentMenu.Items.Add(aMenuItem);
        end;
      end;
    end;
  except
    on E:Exception do
    begin
      ShowMessage('Application eror when retrieving previously edited tree files: ' + E.Message);
    end;
  end;
end;

function TTreeInputForm.SaveRecentFilesList: Boolean;
var
  aFilename: String = '';
begin
  Result := True;
  try
    if Assigned(FRecentTreesList) and (FRecentTreesList.Count > 0) then
    begin
      aFilename := GetPrivateFile(mfTreeEditorMruFiles, False);
      FRecentTreesList.SaveToFile(aFilename);
      Result := FileExists(aFilename);
    end;
  except
    on E:Exception do
      ShowMessage('Application error when saving the recently edited trees list: ' + E.Message);
  end;
end;

procedure TTreeInputForm.DoneEditingNotify(Sender: TObject);
var
  NewSession: TTreeInputSession = nil;
  NameToAdd: String = '';
  SessionToAdd: String = '';
  PreviouslyEdited: Boolean = False;
  aMenuItem: TMenuItem = nil;
  i: Integer = -1;
  aList: TStringList = nil;
begin
  try
    aList := TStringList.Create;

    // Add this tree to the most recent trees list
    NameToAdd := FileName;
    PreviouslyEdited := False;
    if Pos('(edited)', FileName) <> 0 then
    begin
      Delete(FCurrentNewickFile, Pos('(edited)', FileName), Length(FileName));
      PreviouslyEdited := True;
      FCurrentNewickFile := TrimRight(FileName);
    end;
    if Pos('::', FileName) <> 0 then
      Delete(FCurrentNewickFile, Pos('::', FileName), Length(FileName));
    i := 1;
    while (FRecentTreesList.IndexOfName(NameToAdd) >= 0) or (FRecentTreesList.IndexOfName(NameToAdd + ' (edited)') >= 0) do
    begin
      NameToAdd := FileName + '::' + IntToStr(i);
      inc(i);
    end;

    if TreeEdited or PreviouslyEdited then
      NameToAdd := NameToAdd + ' (edited)';

    NewSession := TTreeInputSession.Create;
    NewSession.FillFromForm(self);
    for i := 0 to NewSession.TaxaNamesMap.Count - 1 do
      aList.Add(NewSession.TaxaNamesMap.Names[i]);
    NewSession.FillUsingNamesList(aList);
    SessionToAdd := NewSession.Serialize;
    if FRecentTreesList.Count > 0 then
      for i := 0 to FRecentTreesList.Count - 1 do
        if FRecentTreesList.ValueFromIndex[i] = sessionToAdd then
          break;
    FRecentTreesList.Values[nameToAdd] := sessionToAdd;
    aMenuItem := TMenuItem.Create(RecentMenu);
    aMenuItem.Caption := NameToAdd;
    aMenuItem.OnClick := RecentFilesMenuItemClick;
    RecentMenu.Items.Add(aMenuItem);
    SaveRecentFilesList;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
  Close;
end;

procedure TTreeInputForm.UpdateStatusBar;
begin
  if FShowAsTopologyEditor then
    StatusBar1.Panels[0].Text := 'Editing topology'
  else
    StatusBar1.Panels[0].Text := 'Mapping taxa names';
  StatusBar1.Panels[1].Text := Format('%.0n taxa', [TreeEditBox.NoOfOTUs*1.0]);
  StatusBar1.Panels[2].Text := ExtractFileName(FileName);
  StatusBar1.Panels[2].Width := StatusBar1.ClientWidth - StatusBar1.Panels[0].Width - StatusBar1.Panels[1].Width;
end;

procedure TTreeInputForm.RefreshListView;
var
  op: TTreeEditOperation = nil;
  i: Integer = -1;
  aItem: TListItem = nil;
begin
  try
    if Assigned(TreeEditBox.UndoStack) and (TreeEditBox.UndoStack.Count > 0) then
    begin
      op := TTreeEditOperation(TreeEditBox.UndoStack.Peek);
      op.UpdateActiveNamesMap(FActiveNames);
      NameMapListView.OnChange := nil;
      NameMapListView.Clear;
      if FActiveNames.Count > 0 then
        for i := 0 to FActiveNames.Count - 1 do
        begin
          aItem := NameMapListView.Items.Add;
          aItem.Caption := FActiveNames.Names[i];
          if FActiveNames.ValueFromIndex[i] <> EmptyStr then
            aItem.SubItems.Add(FActiveNames.ValueFromIndex[i])
          else
            aItem.SubItems.Add(EmptyStr);
        end;
      NameMapListView.Invalidate;
    end;
  finally
    NameMapListView.OnChange := NameMapListViewChange;
  end;
end;

{$IFDEF DEBUG}
procedure TTreeInputForm.UndoStackToDeveloperConsole(aMsg: String);
var
  aList: TStringList = nil;
  i: Integer = -1;
begin
  try
    aList := TreeEditBox.UndoStackDebugStrings;
    WriteToDevConsole('UndoStack: ' + aMsg);
    if aList.Count > 0 then
      for i := 0 to aList.Count - 1 do
        WriteToDevConsole(aList[i]);
    WriteToDevConsole('-------------------');

    with TreeEditBox do
    begin
      for i := 1 to NoOfOTUs do
        WriteToDevConsole(GetOtuPrivateName(i) + '  ' + GetTaxonName(i));
    end;
    WriteToDevConsole(LineEnding);
  finally
    if Assigned(aList) then
      aList.Free;
  end;

end;
{$ENDIF}

function TTreeInputForm.NumTaxa: Integer;
begin
  if Assigned(TreeEditBox) then
    Result := TreeEditBox.NoOfOTUs
  else
    Result := 0;
end;

function TTreeInputForm.DoesTreeHaveAllActiveNames: Boolean;
var
  i: Integer;
begin
  //assumes that SetData or SetFile has been called and
  //that taxa are matched
  Result := False;
  with FActiveNames do
    for i:=0 to FActiveNames.Count-1 do
      if Length(Values[Names[i]]) = 0 then
        Exit;
  Result := True;
end;

procedure TTreeInputForm.PruneUnwantedTreeNodesAndNames;
var
  i: Integer;
begin
  RebuildTreeNames;
  with TreeEditBox do
  begin
    i:=1;
    while (i <= NoOfOTUs) and (NoOfOTUs > 2) do
      if (Length(FTreeNames.Values[OTUName[i]]) = 0) or (OTUPrivateName[i] = UndefinedStr) then
      begin
        RemoveOTU(i);
        if not HasUnusedTreeTaxa then
          HasUnusedTreeTaxa := True;
      end
      else
        inc(i);
  end;
  FTreeNames.Clear;
end;

function TTreeInputForm.PruneUnwantedTreeNodesAndNamesNoGui(AFileName: String; AExpNameStrList: TStringList;var aNewickStr: String; var aIsRooted: Boolean; var aMsg: String): Boolean;
var
  tempList: TStringList = nil;
  AInputTree: TTreeList = nil;
  ANamesList: TStringList = nil;
  i: Integer = -1;
begin
  Result := False;
  try
    aMsg := EmptyStr;
    aNewickStr := EmptyStr;
    aIsRooted := False;
    AInputTree := TTreeList.Create;
    ANamesList := TStringList.Create;
    AInputTree.ImportFromNewickFile(AFileName, nil);
    for i:= 0 to AInputTree.NoOfOTUs-1 do
      ANamesList.Add(AInputTree.OTUName[i]);
    tempList := RegexReplaceAStringList(ANamesList, '[^A-Za-z0-9]', '_');
    ANamesList.Free;
    ANamesList := tempList;
    tempList := RegexReplaceAStringList(AExpNameStrList, '[^A-Za-z0-9]', '_');
    AExpNameStrList.Assign(tempList);
    for i := 0 to AExpNameStrList.Count - 1 do
    begin
      if ANamesList.IndexOf(AExpNameStrList.Strings[i]) < 0 then
        begin
          Result := False;
          aMsg += 'The list of taxa in the tree file is different from the list of taxa in the sequence file, this must be corrected to complete this analysis.  Tree missing:' + AExpNameStrList[i] + LineEnding;
          {$IFNDEF VISUAL_BUILD}
          Error_NV(Trim(aMsg));
          {$ENDIF}
        end;
    end;

    for i := 0 to ANamesList.Count - 1 do
    begin
      if AExpNameStrList.IndexOf(ANamesList[i]) < 0 then
      begin
        {$IFDEF VISUAL_BUILD}
        aMsg += Format('pruning unused taxon from input tree: "%s"%s', [ANamesList[i], LineEnding]);
        {$ELSE}
        warn_nv(Format('pruning unused taxon from input tree: %s', [ANamesList[i]]));
        {$ENDIF}
        AInputTree.RemoveOtu(ANamesList[i], False, aMsg);
      end;
    end;
    AInputTree.OutputNodeLabels := True;
    aNewickStr := AInputTree.OutputNewickTree(0, AInputTree.isBLen, AInputTree.isStats, 0.0);
    aIsRooted := AInputTree.isRooted;
    Result := True;
  finally
    if Assigned(ANamesList) then
      ANamesList.Free;
    if Assigned(AInputTree) then
      AInputTree.Free;
  end;
end;

procedure TTreeInputForm.FormShow(Sender: TObject);
const
  CR_FILE = 1000;
  CR_SCRATCH = 1001;
  CR_RANDOM = 1002;
  CR_CANCEL = 1003;
var
  CustomRes: Integer;
begin
  FLastUpdateTime := Now;
  HelpContext := RH_Tree_Topology_Editor;
  if openFileOnShow <> EmptyStr then // If we have already supplied a tree but it needs to be edited we do NOT want to ask the user for a tree again, instead automaticlly show the tree which needs mapping.
  begin
    OpenTree(openFileOnShow);
  end
  else
  begin
    CustomRes := QuestionDlg('Tree Editor', 'How would you like to edit a tree:', mtCustom, [CR_FILE, 'From a File', CR_SCRATCH, 'From Scratch', CR_RANDOM, 'Random Tree From Active Data', CR_CANCEL, 'Cancel', 'IsCancel'], 0);
    if CustomRes = CR_FILE then
    begin
      if (not SelectTreeFile) then
      begin
        if fsModal in FormState then
          ModalResult := mrCancel
        else
        begin
          Close;
          Exit;
        end;
      end;
    end
    else
    begin
      if CustomRes = CR_SCRATCH then
      begin
        GenerateScratchTree;
      end;
      if CustomRes = CR_RANDOM then
      begin
        GenerateRandomTree;
      end
      else
      begin
        if CustomRes = CR_CANCEL then
        begin
          userCanceled := True;
          if fsModal in FormState then
            ModalResult := mrCancel
          else
          begin
            Close;
            Exit;
          end;
        end;
      end;
    end;
  end;
  SetHighLight;
  TreeEditBox.Refresh;

  try
    BeginFormUpdate;
    if FShowAsTopologyEditor then
    begin
      TreeEditBox.ShowDivergenceTimes := False;
      LeftPanel.Visible := False;
      Splitter1.Visible := False;
      OnlyUnmappedChk.Visible := False;
      HideMappedTreeNamesChk.Visible := False;
      CancelAction.Visible := False;
      ActionBLenEditMode.Visible:=False;
      OkBtn.Visible := True;
      ShowHint := True;
      TreeEditBox.EditNameEnabled := True;
      TreeEditBox.EditTimesEnabled :=True;
      TreeEditBox.EditEnabled := True;
      TreeEditBox.SetIsRooted(True);
      TreeEditBox.Invalidate;
      Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Tree Topology Editor';
    end
    else
      Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Tree Input Dialog';
    if isDeveloper then
    begin
      AdvancedMap.Visible := True;
      HistoryVisibleBtn.Visible := True;
    end;
  finally
    EndFormUpdate;
    ActionAutosizeExecute(Self);
  end;
end;

procedure TTreeInputForm.SetHighlight;
var
  i: integer;
begin
  with TreeEditBox do
  begin
    for i:= 1 to NoOfOTUs do
      HilightOTUName[i] := (OTUPrivateName[i] = UndefinedStr);
    TreeEditBox.Invalidate;
  end;
end;

function TTreeInputForm.GetNewickTree: AnsiString;
begin
  RebuildTreeNames;
  PruneUnwantedTreeNodesAndNames;
  Result := TreeEditBox.NewickPrivateTree;
end;

procedure TTreeInputForm.UpdateProgress(aStatus: String; aProgress: Integer);
var
  p: Integer = -1;
begin
  if MilliSecondsBetween(Now, FLastProgressUpdateTime) < 100 then
    Exit;
  p := min(100, aProgress);
  if ProgressBar.Position <> p then
  begin
    ProgressBar.Position := p;
    ProgressBar.Invalidate;
  end;
  if ProgressLabel.Caption <> aStatus then
  begin
    ProgressLabel.Caption := aStatus;
    ProgressLabel.Invalidate;
  end;
  Application.ProcessMessages;
  FLastProgressUpdateTime := Now;
end;

procedure TTreeInputForm.ClearProgress;
begin
  if ProgressBar.Position <> 0 then
    ProgressBar.Position := 0;
  if ProgressLabel.Caption <> EmptyStr then
    ProgressLabel.Caption := EmptyStr;
  Invalidate;
end;

procedure TTreeInputForm.GetTreeData(var treedata: TTreeData);
begin
  PruneUnwantedTreeNodesAndNames;
  TreeEditBox.GetTreeData(treedata);
end;

procedure TTreeInputForm.OKBtnClick(Sender: TObject);
var
  SessionToAdd: AnsiString = '';
  NewSession: TTreeInputSession = nil;
  i: Integer = -1;
  missingTaxaCount: Integer = 0;
  foundTaxaCount: Integer = 0;
  NameToAdd: AnsiString = '';
  PreviouslyEdited: Boolean = False;
  Response: Integer = -1;
  query: String = '';
  doAbort: Boolean = False;
  aMenuItem: TMenuItem = nil;
begin
  if Assigned(FActiveNames) and (FActiveNames.Count > 0) then
  begin
    for i := 0 to FActiveNames.Count - 1 do
      if FActiveNames.ValueFromIndex[i] = EmptyStr then
        Inc(missingTaxaCount)
      else
        Inc(foundTaxaCount);

    if (not FShowAsTopologyEditor) and ((foundTaxaCount < 2) or (missingTaxaCount = FActiveNames.Count)) then
    begin
      ShowMessage('At least 2 names must be resolved in the tree to continue');
      Exit;
    end;
  end;

  if missingTaxaCount > 0 then
  begin
    if missingTaxaCount = 1 then
      query := '1 name in the active data is not included in the tree. Continue without using sequence data for that name?'
    else
      query := IntToStr(missingTaxaCount) + ' names in the active data are not included in the tree. Continue without using sequence data for those names?';
    Response := MessageDlg(query, mtConfirmation, mbYesNo, 0);
    if Response = mrYes then
      CollectUnusedSequences
    else
      doAbort := True;
  end;

  if not DoAbort then
  begin
    // Add this tree to the most recent trees list
    NameToAdd := FileName;
    PreviouslyEdited := False;
    if Pos('(edited)', FileName) <> 0 then
    begin
      Delete(FCurrentNewickFile, Pos('(edited)', FileName), Length(FileName));
      PreviouslyEdited := True;
      FCurrentNewickFile := TrimRight(FileName);
    end;
    if Pos('::', FileName) <> 0 then
      Delete(FCurrentNewickFile, Pos('::', FileName), Length(FileName));
    i := 1;
    while (FRecentTreesList.IndexOfName(NameToAdd) >= 0) or (FRecentTreesList.IndexOfName(NameToAdd + ' (edited)') >= 0) do
    begin
      NameToAdd := FileName + '::' + IntToStr(i);
      inc(i);
    end;

    if TreeEdited or PreviouslyEdited then
      NameToAdd := NameToAdd + ' (edited)';

    NewSession := TTreeInputSession.Create;
    NewSession.FillFromForm(self);
    SessionToAdd := NewSession.Serialize;
    FRecentTreesList.Values[nameToAdd] := sessionToAdd;
    aMenuItem := TMenuItem.Create(RecentMenu);
    aMenuItem.Caption := NameToAdd;
    aMenuItem.OnClick := RecentFilesMenuItemClick;
    RecentMenu.Items.Add(aMenuItem);
    SaveRecentFilesList;
    ModalResult := mrOk;
  end
  else
    modalResult := mrNone
end;

procedure TTreeInputForm.RebuildTreeNames;
var
  i: Integer;
begin
  with TreeEditBox do
  begin
    FTreeNames.Clear;
    for i:=1 to NoOfOTUs do
      FTreeNames.Add(OTUName[i] + '=' + OTUPrivateName[i]);
  end;
end;

procedure TTreeInputForm.TreeEditBoxClick(Sender: TObject);
var
  CursorPos: TPoint;
begin
  CursorPos := TPoint.Create(0,0);
  GetCursorPos(CursorPos);
  CursorPos := TreeEditBox.ScreenToClient(CursorPos);

  if (FDragStartPoint.X = CursorPos.X) and (FDragStartPoint.Y = CursorPos.Y) and (not InResizeMode) then
  begin
    TreeEditBox.FocusOnPoint(CursorPos);
    if TreeEditBox.FocusedNameIndex > 0 then
    begin
      NameMapListView.OnChange := nil;
      NameMapListView.Selected := NameMapListView.FindCaption(0, TreeEditBox.OTUPrivateName[TreeEditBox.FocusedNameIndex], false, True, false);
      NameMapListView.OnChange := NameMapListViewChange;
      ClearPrevHighlightedTaxa;
      TreeEditBox.SetCustomHilightColorOTUName(TreeEditBox.FocusedNameIndex, true, clBlue);
      TreeEditBox.DrawBoxForHighlightedNames := True;
      FTreeNameHighlighted := TreeEditBox.OTUName[TreeEditBox.FocusedNameIndex];
      TreeEditBox.Invalidate;
    end
    else
      if ClearPrevHighlightedTaxa then
        TreeEditBox.Invalidate;
    TreeToActiveMatchLBx.Hide;
  end;
end;

procedure TTreeInputForm.TreeEditBoxDblClick(Sender: TObject);
var
  SelectedMapListItem: TListItem;
begin
  // if we have double clicked on a name then open the dropdown box in the NameMapListView
  if (TreeEditBox.FocusedNameIndex > 0) and (not FShowAsTopologyEditor) then
  begin
    SelectedMapListItem := NameMapListView.FindCaption(0, TreeEditBox.OTUPrivateName[TreeEditBox.FocusedNameIndex], false, True, false);
    if (SelectedMapListItem <> nil) and (Pos(UserAddedStr, SelectedMapListItem.SubItems.Strings[0]) <> 1) then
      DropDownAtItem(SelectedMapListItem);
  end
  else
    TreeToActiveMatchLBx.Hide;
end;

procedure TTreeInputForm.DropDownAtItem(SelectedMapListItem: TListItem);
var
  i: Integer;
begin
  // Position the TreeToActiveMatchLBx component
  TreeToActiveMatchLBx.Left := SelectedMapListItem.DisplayRect(drLabel).Right;
  TreeToActiveMatchLBx.Top := SelectedMapListItem.DisplayRect(drLabel).Top+15;
  TreeToActiveMatchLBx.Width := NameMapListView.Column[1].Width;

  // Build latest TreeOTUs list, later this might just come from FTreeNames.
  TreeToActiveMatchLBx.Items.Clear;
  for i:=1 to TreeEditBox.NoOfOTUs do
    if TreeEditBox.OTUName[i] <> UserAddedStr then
      TreeToActiveMatchLBx.ITems.Add(TreeEditBox.OTUName[i]);

  SetupTTAChkStates;

  SetStateOfTTAChkArray;
  With TreeToActiveMatchLBx do
  begin
    if (ItemHeight * Count) > (Self.Height - Top - OKCancelPanel.Height) then
      Height := Self.Height - top -OKCancelPanel.Height
    else
      Height := ItemHeight * Count + 4;
    Show;
  end;
end;

procedure TTreeInputForm.NameMapListViewClick(Sender: TObject);
var
  CursorPoint: TPoint;
  SelectedMapListItem: TListItem = nil;
begin
  CursorPoint := TPoint.Create(0, 0);
  // Do some cleanup in the case where we haven't clicked on any valid entry.
  GetCursorPos(CursorPoint);
  CursorPoint := NameMapListView.ScreenToClient(CursorPoint);
  SelectedMapListItem := NameMapListView.GetItemAt(CursorPoint.X, CursorPoint.Y);
  if (SelectedMapListItem = nil) then
    ClearPrevHighlightedTaxa;
  TreeToActiveMatchLBx.Hide;
  ShowMapDropdownAt(CursorPoint);
end;


function TTreeInputForm.TreeJumpToTreeName(TreeName: AnsiString; ColorResult: Boolean): Boolean;
var
  p: TPoint;
  TreeNameIndex: Integer;
  TreeNameCoords: TRect;
begin
  result := false;
  with TreeEditBox do
  begin
    TreeNameIndex := IndexOfName[TreeName];  // If name exists in the tree
    if TreeNameIndex = -1 then
      exit;

    TreeNameCoords := NodeCoords(TreeNameIndex);
    p.X := TreeNameCoords.Left;
    p.Y := TreeNameCoords.Top - TreeEditBox.ScrollTop;
    //if not FEmfScrollBox.IsVisible(Point(TreeNameCoords.Left, TreeNameCoords.Top)) then
      TreeEditBox.ScrollTo(p.X, p.Y);
    if ColorResult then
    begin
      // Now highlight the selected TreeName
      if FTreeNameHighlighted <> TreeName then
        ClearPrevHighlightedTaxa;
      SetCustomHilightColorOTUName(IndexOfName[TreeName], true, clBlue);
      FTreeNameHighlighted := TreeName;
      NameMapListView.OnChange := nil;
      FocusOnName(IndexOfName[TreeName]);
      NameMapListView.OnChange := NameMapListViewChange;
    end;
    Refresh;
    result := true;
  end;
end;

procedure TTreeInputForm.TreeToActiveMatchLBxClickCheck(Sender: TObject);
var
  TreeIndexOfChecked, TTAIndexChanged: Integer;
  Checked: Boolean = False;
  TreeNameChecked: AnsiString;
begin
  TTAIndexChanged := FindChangedTTAIndex;
  TreeNameChecked := TreeToActiveMatchLBx.Items.Strings[TTAIndexChanged];
  TreeIndexOfChecked := TreeEditBox.IndexOfName[TreeNameChecked];
  if TreeIndexOfChecked < 0 then
        showmessage('MEGA has encountered an error; TreeIndexOfChecked not defined:please send a bug report for this issue');

  Case TreeToActiveMatchLBx.State[TTAIndexChanged] of // We need to correct the state a bit since the component treats it as a 3-way state and we only want a toggle with gray to indicate the tree name is used by a different Active Name.
    cbChecked:
      begin
        Checked := true;
      end;
    cbGrayed:
      begin
        if FTTAChkState[TTAIndexChanged] = cbUnchecked then
        begin
          TreeToActiveMatchLBx.OnClickCheck := nil;
          TreeToActiveMatchLBx.State[TTAIndexChanged] := cbChecked;
          TreeToActiveMatchLBx.OnClickCheck := TreeToActiveMatchLBxClickCheck;
          Checked := true;
        end;
      end;
    cbUnchecked:
      Checked := false;
  end;

  if Checked then
  begin
    if (TreeEditBox.OTUPrivateName[TreeIndexOfChecked] <> EmptyStr) and (TreeEditBox.OTUPrivateName[TreeIndexOfChecked] <> UndefinedStr) then
    begin
      if (MessageDlg('This tree name is already associated with another MEGA name, would you like to take change the name association?', mtWarning, [mbYes, mbNo], 0) = mrYes) then
      begin // if YES, then we go through with the assignment.  Same code as assigning when there is no conflict.
        SetTreeNameForActive(NameMapListView.Selected.Caption, TreeNameChecked);
        SetHighlight;
      end
      else
      begin  // if NO, then we uncheck the checkmark that was just made, and remove it from the CheckedList.
        TreeToActiveMatchLBx.OnClickCheck := nil;
        TreeToActiveMatchLBx.State[TTAIndexChanged] := FTTAChkState[TTAIndexChanged];
        TreeToActiveMatchLBx.OnClickCheck := TreeToActiveMatchLBxClickCheck;
      end;
    end
    else // else we are just checking something and there is no conflict.
    begin
      SetTreeNameForActive(NameMapListView.Selected.Caption, TreeNameChecked);
      SetHighlight;
    end;
  end
  else // they unchecked something, set the meganame associated with that tree name to ''
  begin
    ClearActiveNameOnTreeName(TreeNameChecked);
    SetHighlight;
    TreeEditBox.Refresh;
  end;
  SetupTTAChkStates;
  SetStateOfTTAChkArray; // Set the state to reflect the change.
  TreeToActiveMatchLBx.Hide;
  TreeEdited := True;
end;

procedure TTreeInputForm.BeginRefresh;
begin
  { just a stub for compatability with TTreeBox}
end;

procedure TTreeInputForm.ResizeTreeToFit;
var
  PPOTU: Integer;
begin
  with TreeEditBox do
  begin
    Top := 0;
    Left := 0;
    if NoOfOTUs <= 0 then Exit;
    if ((TreeEditBox.Height-21) div NoOfOTUs) > 1 then
    begin
      PPOTU := ((TreeEditBox.Height-21) div NoOfOTUs);
    end
    else
    begin
      PPOTU := 1; // at the very least you need 1px per otu.
    end;
    PPOTU := Max(PPOTU, StringHeight(' ', TreeEditBox.OTU_Font));  // When uncommented this draws the tree so that all names can be read (not overlapping).
    Height := Min((PPOTU + 1)*NoOfOTUs, Screen.WorkAreaHeight);
    if Height < (ScrollSizeContainer.Height - GetSystemMetrics(SM_CXHSCROLL)) then
     Height := ScrollSizeContainer.Height - GetSystemMetrics(SM_CXHSCROLL);
    PixelsPerOTU := PPOTU;
    Width := ScrollSizeContainer.Width - GetSystemMetrics(SM_CXVSCROLL) - 100;
    TreeWidth := ScrollSizeContainer.Width - GetSystemMetrics(SM_CXVSCROLL) - 15 - LargestWidthOfOTUNames- 100;  // 15 for the tree's padding on each side.
    SetAttrIndex;
    Refresh;
  end;
end;

procedure TTreeInputForm.FormResize(Sender: TObject);
begin
  if AutosizeActive then
    ResizeTreeToFit;
  TreeEditBox.Invalidate;
end;

procedure TTreeInputForm.MouseScrollResizeBtnClick(Sender: TObject);
begin
  if not InResizeMode then
    EnterResizeMode
  else
    QuitResizeMode;
end;

procedure TTreeInputForm.TreeEditBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with TreeEditBox do
  begin
    GetCursorPos(FDragStartPoint);
    FDragStartPoint := ScreenToClient(FDragStartPoint);
    FStartPixelsPerOTU := PixelsPerOTU;
    FStartTreeWidth := TreeWidth;
    if Button = mbRight then
      FocusOnPoint(FDragStartPoint);
    if InResizeMode then
      if (Button = mbLeft) and (not Dragging) then
        BeginDrag(true);
  end;
end;

procedure TTreeInputForm.TreeEditBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CursorPos: TPoint;
begin
  CursorPos := TPoint.Create(0,0);
  if Button = mbRight then
  begin
    GetCursorPos(CursorPos);
    TreeRightClick.Popup(CursorPos.X, CursorPos.Y);
  end;
end;

procedure TTreeInputForm.TreeEditBoxNameDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  CursorPos: TPoint;
  SrcListItemPt: TPoint;
begin
  CursorPos := TPoint.Create(0,0);
  GetCursorPos(CursorPos);
  CursorPos := TreeEditBox.ScreenToClient(CursorPos);

  if (Source.ClassType = TListView) and (not FShowAsTopologyEditor) then // draging an active name to the tree from ListView
  begin
    Accept := false;
    SrcListItemPt := Point(X,Y);
    if (TreeEditBox.FocusedIndex > 0) or (TreeEditBox.NameFocused) then
      TreeEditBox.Repaint;
    TreeEditBox.FocusOnClosest(SrcListItemPt, 10); // Added new functionality to focus on the closest point within 10px, rather than just the one under the cursor.
    if (TreeEditBox.FocusedIndex > 0) or (TreeEditBox.NameFocused) then
      Accept := true;
  end;
end;

procedure TTreeInputForm.TreeEditBoxNameDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Source.ClassType = TListView then
  begin
    if (TreeEditBox.FocusedNameIndex <> 0) then // Dragging to a name, associate the MEGA name with the tree name.
    begin

      if Pos(UserAddedStr, TreeEditBox.OTUName[TreeEditBox.FocusedNameIndex]) <> 1 then
      begin
        SetTreeNameForActive(TListView(Source).Selected.Caption, TreeEditBox.OTUName[TreeEditBox.FocusedNameIndex]);
        TreeEdited := True;
      end
      else
        MessageDlg('Sorry, OTUs added by the user can not be remapped.', mtWarning, [mbOK], 0);
    end
    else
    begin  // Insert on the focused branch.
      if (TListView(Source).Selected.SubItems.Strings[0] = EmptyStr) then
      begin
        AddTreeToHistory(EAdd);
        TreeEditBox.InsertOTU(TreeEditBox.FocusedIndex, UserAddedStr + '-' + IntToStr(NoOfUserDefOTUs));
        TreeEditBox.Refresh;
        SetTreeNameForActive(TListView(Source).Selected.Caption, UserAddedStr + '-' + IntToStr(NoOfUserDefOTUs)); 
        inc(NoOfUserDefOTUs);
        TreeEdited := True;
      end
      else
        ShowMessage('Sorry, you may not add this OTU since it already has an association.');
    end;
  end;
end;

procedure TTreeInputForm.SetTreeNameForActive(ActiveName: AnsiString; TreeName: AnsiString);
var
  ActiveItem: TListItem = nil;
  TreeIndex: Integer = -1;
  PrevActiveIndex: Integer = -1;
  i: Integer = -1;
begin
  if TreeToActiveMatchLBx.Visible then
  begin
    TreeToActiveMatchLBx.Visible := False;
    NameMapListView.Invalidate;
  end;

  // first ensure that both of the names are valid.
  ActiveItem := NameMapListView.FindCaption(0, ActiveName, False, True, False);
  if ActiveItem = nil then
    exit;
  TreeIndex := TreeEditBox.IndexOfName[TreeName];
  if TreeIndex = -1 then
    exit;

  repeat
    begin
      PrevActiveIndex := TreeEditBox.IndexOfPrivateName[ActiveName];
      if PrevActiveIndex <> -1 then
        ClearActiveNameOnTreeName(TreeEditBox.OTUName[PrevActiveIndex]);
    end;
  until PrevActiveIndex  = -1;

  // Also clear out the association of the Old Active name to the TreeName
  if TreeName <> UserAddedStr then
    ClearTreeNameFromNameMapList(TreeName);

  // Set the private name in the tree to be the ActiveName
  TreeEditBox.SetOTUPrivateName(TreeIndex, ActiveName);

  // Set the tree name in the NameMapListView to be the tree name
  ActiveItem.SubItems.Strings[0] := TreeName;
  if FActiveNames.Count > 0 then
    for i := 0 to FActiveNames.Count - 1 do
      if FActiveNames.ValueFromIndex[i] = TreeName then
      begin
        FActiveNames[i] := FActiveNames.Names[i] + '=';
        break;
      end;
  FActiveNames.Values[ActiveName] := TreeName;
  NameMapListView.Invalidate;
  SetHighlight;
  TreeEditBox.Invalidate;
  UpdateDispTaxaList;
end;

function TTreeInputForm.FindChangedTTAIndex: Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=0 to high(FTTAChkState) do
    if TreeToActiveMatchLBx.State[i] <> FTTAChkState[i] then
    begin
      result := i;
      break;
    end;
end;

procedure TTreeInputForm.SetStateOfTTAChkArray;
var
  i: Integer = 0;
begin
  SetLength(FTTAChkState, TreeToActiveMatchLBx.Count);
  for i := 0 to high(FTTAChkState) do
    FTTAChkState[i] := TreeToActiveMatchLBx.State[i];
end;

procedure TTreeInputForm.ClearActiveNameOnTreeName(TreeName: AnsiString);
var
  TreeNameIndex, i: Integer;
begin
  // Clear the active name out of the tree
  TreeNameIndex := TreeEditBox.IndexOfName[TreeName];
  if TreeNameIndex <> -1 then
  begin
    TreeEditBox.SetOTUPrivateName(TreeNameIndex, UndefinedStr);
    TreeEditBox.Invalidate;
    // Find which active name (if any) contains the tree name then remove the association of the tree  name with that active name
    for i:=0 to NameMapListView.Items.Count-1 do
      if (NameMapListView.Items.Item[i].SubItems.Strings[0] = TreeName) then
      begin
        NameMapListView.Items.item[i].SubItems.Strings[0] := EmptyStr;
        NameMapListView.Invalidate;
        break;
      end;
    for i:=0 to FActiveNames.Count-1 do
    begin
      with FActiveNames do
        if Values[Names[i]] = TreeName then
          Strings[i] := Names[i] + '=';
    end;
    for i := 0 to HiddenList.Count - 1 do
    begin
      if (HiddenList.ValueFromIndex[i] = TreeName) then
      begin
        HiddenList.ValueFromIndex[i] := EmptyStr;
        break;
      end;
    end;
  end;
  UpdateDispTaxaList;
end;

procedure TTreeInputForm.SetupTTAChkStates;
var
  i: Integer;
  SubItemIndexInLBx: Integer;
begin
  UpdateProgress('Updating States...', 1);
  for i:=0 to TreeToActiveMatchLBx.Count-1 do
    TreeToActiveMatchLBx.State[i] := cbUnchecked;

  for i:=0 to NameMapListView.Items.Count-1 do
  begin
    UpdateProgress('Updating States...', Round((i + 1)/NameMapListView.Items.Count*100));
    SubitemIndexInLBx := TreeToActiveMatchLBx.Items.IndexOf(NameMapListView.Items.Item[i].SubItems.Strings[0]);
    if SubitemIndexInLBx >= 0 then
      if NameMapListView.Selected = NameMapListView.Items.Item[i] then
        TreeToActiveMatchLBx.State[SubItemIndexInLBx] := cbChecked
      else
        TreeToActiveMatchLBx.State[SubItemIndexInLBx] := cbGrayed;
  end;
  ClearProgress;
end;

procedure TTreeInputForm.ClearTreeNameFromNameMapList(TreeName: AnsiString);
var
  i: Integer;
begin
  for i:=0 to NameMapListView.items.count-1 do
  begin
    if NameMapListView.Items.Item[i].SubItems.Strings[0] = TreeName then
      NameMapListView.Items.Item[i].SubItems.Strings[0] := EmptyStr;
  end;
end;

procedure TTreeInputForm.ActionUndoExecute(Sender: TObject);
begin
  try
    if TreeEditBox.CanUndo then
    begin
      TreeEditBox.Undo;
      SetHighlight;
      TreeEditBox.Refresh;
      if not FShowAsTopologyEditor then
        RefreshListView;
      UpdateStatusBar;
    end;
  except
    on E:Exception do
      ShowMessage('Application error when undoing the last action: ' + E.Message);
  end;
end;

procedure TTreeInputForm.ActionAddOTUExecute(Sender: TObject);
var
  addToIndex: Integer;
begin
  with TreeEditBox do
  begin
    AddTreeToHistory(EAdd);
    addToIndex := FocusedIndex;
    if addToIndex = 0 then
      addToIndex := 1;
    InsertOTU(addToIndex, 'New Node ' + IntToStr(FNewNodeNum));
    if not FShowAsTopologyEditor then
      SetOTUPrivateName(IndexOfName['New Node ' + IntToStr(FNewNodeNum)], UndefinedStr);
    inc(FNewNodeNum);
    Refresh;
    SetHighlight;
  end;
  if AutosizeActive then
    ResizeTreeToFit;
  TreeEdited := True;
  UpdateStatusBar;
end;

procedure TTreeInputForm.ActionRemoveOTUExecute(Sender: TObject);
begin
  with TreeEditBox do
  begin
    if (FocusedNameIndex = 0) and (not TreeEditBox.FocusedOnOtuBranch) then
      Exit;
    AddTreeToHistory(EDelete);
    if FocusedNameIndex > 0 then
    begin
      ClearActiveNameOnTreeName(OTUName[FocusedNameIndex]);
      RemoveOTU(FocusedNameIndex);
    end
    else
    begin
      ClearActiveNameOnTreeName(OTUName[TreeEditbox.FocusedIndex]);
      RemoveOTU(TreeEditBox.FocusedIndex);
    end;
    Refresh;
    SetHighlight;
  end;
  if AutosizeActive then
    ResizeTreeToFit;
  TreeEdited := True;
  UpdateStatusBar;
end;

procedure TTreeInputForm.ActionRootExecute(Sender: TObject);
begin
  AddTreeToHistory(ERoot);
  TreeEditBox.MakeRootOnBranch;
  TreeEdited := True;
end;

procedure TTreeInputForm.ActionFlipExecute(Sender: TObject);
begin
  if TreeEditBox.BranchFocused then TreeEditBox.FocusOnNode(TreeEditBox.FocusedIndex);
  if TreeEditBox.NodeFocused then
  begin
    AddTreeToHistory(EFlip);
    TreeEditBox.FlipAllCluster;
    TreeEdited := True;
  end;
end;

procedure TTreeInputForm.CopyTBtnClick(Sender: TObject);
begin
  TreeEditBox.CopyImageToClipBoard;
end;

procedure TTreeInputForm.FindTBtnClick(Sender: TObject);
begin
  if MyFindDlg = nil then
  begin
    MyFindDlg := TFindDlg.Create(Self);
    MyFindDlg.Tree := TreeEditBox;
  end;
  MyFindDlg.Show;
end;

procedure TTreeInputForm.FontTBtnClick(Sender: TObject);
begin
  FontDialog1.Font := TreeEditBox.OTU_Font;
  if FontDialog1.Execute then
    TreeEditBox.OTU_Font := FontDialog1.Font;
  TreeEditBox.Invalidate;
end;

procedure TTreeInputForm.NewTreeBtnClick(Sender: TObject);
begin
  GenerateRandomTree;
end;

function TTreeInputForm.GenerateScratchTree: TTreeData;
begin
  Result := GenerateRandomTree(2);
end;

function TTreeInputForm.GenerateRandomTree(NumNodes: Integer = 0): TTreeData;
var
  tree: TTreeData = nil;
  TaxaPool: TStringList = nil;
  k: Integer = -1;
  NumRandomTaxa: Integer = -1;
  NumRandomTaxaStr: String = '';
  TreeArray: TIntArray = nil;

  procedure RandMergeTaxa(aTree: TTreeData; IntArray: TIntArray; startingI: Integer; NumOrigOTUs: Integer);
  var
    RandomDes1, RandomDes2: Integer;
    SelDes1, SelDes2: Integer;
    i, j, s, NumOfNodesToCreate: Integer;
    NextArray: TIntArray = nil;
  begin
    NumOfNodesToCreate := (Length(IntArray) div 2);
    SetLength(NextArray, NumOfNodesToCreate);

    for i := 0 to NumOfNodesToCreate - 1 do
    begin
      RandomDes1 := -1;
      While RandomDes1 = -1 do
      begin
        SelDes1 := Random(Length(IntArray));
        RandomDes1 := IntArray[SelDes1];
      end;
      IntArray[SelDes1] := -1; // it's been used, don't use it again.

      RandomDes2 := -1;
      While RandomDes2 = -1 do
      begin
        SelDes2 := Random(Length(IntArray));
        RandomDes2 := IntArray[SelDes2];
      end;
      IntArray[SelDes2] := -1; // it's been used, don't use it again.

      aTree.NodeArray[startingI + i].des1 := RandomDes1;
      aTree.NodeArray[startingI + i].des2 := RandomDes2;
      NextArray[i] := NumOrigOTUs + i + startingI;
      s := i;
    end;
    for j := 0 to Length(IntArray) - 1 do
      if IntArray[j] <> -1 then
      begin
        SetLength(NextArray, Length(NextArray) + 1); // increment the size of NextArray by 1.
        NextArray[Length(NextArray) - 1] := IntArray[j];
      end;
    if Length(NextArray) > 1 then
      RandMergeTaxa(Tree, NextArray, startingI + s + 1, NumOrigOTUs);
  end;

begin
  Result := nil;
  try
    try
    TreeEdited := False;
    if FileExists(FSourceDataFilename) then
      FCurrentNewickFile := ExtractFileName(FSourceDataFilename)
    else
      FCurrentNewickFile := 'Randomly Generated Tree';
    Randomize;

    TaxaPool := TStringList.Create;
    if (not FShowAsTopologyEditor) and (NumNodes < 1) then
      TaxaPool.AddStrings(FActiveNames)
    else
    begin
      NumRandomTaxa := NumNodes;
      if FActiveNames.Count > 0 then
        NumRandomTaxaStr := IntToStr(FActiveNames.Count);
      if NumRandomTaxa < 1 then
      begin
        NumRandomTaxa := 0;
        repeat
          if not InputQuery('Number of taxa needed', Format('How many taxa would you like in this random tree? (max is %.0n)', [MaxInt*1.0]), NumRandomTaxaStr) then
            Exit;
          TryStrToInt(NumRandomTaxaStr, NumRandomTaxa)
        until ((NumRandomTaxa > 0) and (NumRandomTaxa < MaxInt));
      end;
      for k:=1 to NumRandomTaxa do
      begin
        UpdateProgress('Updating View...', Round(k/NumRandomTaxa*100));
        if FActiveNames.Count > 0 then
        begin
          if k <= FActiveNames.Count then
            TaxaPool.Add(FActiveNames.Strings[k-1] + '=' + FActiveNames.Strings[k-1])
          else
            TaxaPool.Add('Taxon ' + IntToStr(k) + '=' + 'Taxon ' + IntToStr(k));
        end
        else
          TaxaPool.Add('Taxon ' + IntToStr(k) + '=' + 'Taxon ' + IntToStr(k));
      end;
    end;

    tree := TTreeData.Create(TaxaPool.Count,false,false,false);

    SetLength(TreeArray, TaxaPool.Count);
    for k := 0 to Length(TreeArray) - 1 do
      TreeArray[k] := k;
    RandMergeTaxa(Tree, TreeArray, 0, Length(TreeArray));
    ClearTree;
    for k := 0 to TaxaPool.Count - 1 do
      TaxaPool[k] := TaxaPool.Names[k];
    SetTreeData(tree, TaxaPool);
    if (not FShowAsTopologyEditor) then
      InitializeActiveDataAndTreeNameMatches;

    ResizeTreeToFit;
    SetHighLight;
    tree.Free;
    TaxaPool.Free;
    UpdateStatusBar;
    except
      on E:Exception do
        ShowMessage('Application Error in GenerateRandomTree: ' + E.Message);
    end;
  finally
    ClearProgress;
  end;
end;

procedure TTreeInputForm.TreeEditBoxEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if InResizeMode then
    QuitResizeMode;
  Invalidate;
end;

procedure TTreeInputForm.NameMapListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if not Visible then Exit;
  NameMapListView.Canvas.Font.Style := [fsBold];
end;

procedure TTreeInputForm.NameMapListViewCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if not Visible then Exit;
  NameMapListView.Canvas.Font.Style := [];
end;

function TTreeInputForm.SelectTreeFile: Boolean;
begin
  Result := False;
  try
    OpenDlg.Filter := NewickFilesFilter;
    if OpenDlg.Execute then
    begin
      OpenTree(OpenDlg.FileName);
      Result := True;
    end;
  except
    on E:Exception do
      ShowMessage('Error opening tree file: ' + E.Message);
  end;
end;

procedure TTreeInputForm.ClearTree;
var
  i: Integer;
begin
  TreeEditBox.Clear;
  TreeEditBox.Invalidate;
  for i:=0 to NameMapListView.Items.Count-1 do
  begin
    NameMapListView.Items.Item[i].SubItems.Strings[0] := EmptyStr;
    FActiveNames.Strings[i] := FActiveNames.Names[i] + '=';
  end;
end;

procedure TTreeInputForm.ScrollBoxDblClick(Sender: TObject);
begin
  TreeEditBox.DoDblClick;
end;

procedure TTreeInputForm.ScrollBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  TreeEditBox.DoKeyDown(Key, Shift);
end;

procedure TTreeInputForm.NameMapListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  okJump: Boolean;
begin
  if FIsUpdating then
    Exit;
  if (NameMapListView.Selected <> nil) and (NameMapListView.Selected.SubItems.count > 0) and (Length(NameMapListView.Selected.SubItems.Strings[0]) > 0) then
    okJump := TreeJumpToTreeName(NameMapListView.Selected.SubItems.Strings[0])
  else
    okJump := false;
  if not okJump then
    ClearPrevHighlightedTaxa;
end;

function TTreeInputForm.ClearPrevHighlightedTaxa: Boolean;
begin
  result := false;
  if FTreeNameHighlighted <> EmptyStr then
  begin
    TreeEditBox.SetCustomHilightColorOTUName(TreeEditBox.IndexOfName[FTreeNameHighlighted], True, clWhite);
    SetHighlight;
    result := true;
  end;
end;

procedure TTreeInputForm.EndRefresh;
begin
   { just a stub for compatability with TTreeBox}
end;

procedure TTreeInputForm.SetShowAsTopologyEditor(AValue: Boolean);
begin
  if FShowAsTopologyEditor = AValue then Exit;
    FShowAsTopologyEditor := AValue;
  if FShowAsTopologyEditor then
    OkBtn.OnClick := DoneEditingActionExecute;
  if not FShowAsTopologyEditor then
  begin
    TreeEditBox.ShowBLen := False;
    ActionBLenEditMode.Checked := False;
  end;
end;

procedure TTreeInputForm.EnterResizeMode;
begin
  if AutosizeActive then
  begin
    AutosizeActive := False;
    ActionAutosize.Checked := False;
  end;

  with TreeEditBox do
  begin
    BeginResize;
    MouseScrollResizeBtn.ImageIndex := ENTERRESIZEMODE_IMAGE;
    TreeEditBox.Cursor := crSizeNWSE;
    TreeEditBox.DragCursor := crSizeNWSE;
    EditEnabled := false;
    ShowSelection := false;
  end;
end;

procedure TTreeInputForm.QuitResizeMode;
begin
  with TreeEditBox do
  begin
    EndResize;
    MouseScrollResizeBtn.ImageIndex := QUITRESIZEMODE_IMAGE;
    TreeEditBox.Cursor := crDefault;
    DragCursor := crDefault;
    EditEnabled := True;
    ShowSelection := True;
    TreeEditBox.SetAttrIndex;
    TreeEditBox.ResetScrollBounds;
    TreeEditBox.Invalidate;
  end;
end;

procedure TTreeInputForm.NameMapListViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TTreeInputForm.InstructionsREditMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TTreeInputForm.OKCancelPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 Screen.Cursor := crDefault;
end;

procedure TTreeInputForm.OKBtnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TTreeInputForm.CancelBtnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TTreeInputForm.HelpBtnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TTreeInputForm.TreeToolBarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TTreeInputForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  Screen.Cursor := crDefault;
end;

procedure TTreeInputForm.ToolButton1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

function TTreeInputForm.HasUnusedSequenceData: Boolean;
begin
  Result := (UnusedSequences.Count > 0);
end;

procedure TTreeInputForm.HelpBtnClick(Sender: TObject);
var
  helpTopic: String;
begin
  try
    helpTopic := MapHelpContextToKeyword(HelpContext);
    if Trim(helpTopic) <> EmptyStr then
      ShowContextSensitiveHelp(helpTopic)
    else
      ShowContextSensitiveHelp(HelpKeyword);
  except
    on E:Exception do
      ShowMessage('Application error when initializing the help browser: ' + E.Message);
  end;
end;

procedure TTreeInputForm.UpdateDispTaxaList;
begin
  if ConstructSelTaxaList then
    NameMapListView.AlphaSort; // we need to sort the NameMapListView when we add back hidden sequences so they are in the same order as when they were removed.
  NameMapListView.Invalidate;
end;

procedure TTreeInputForm.UpdateRefreshProgress(AProgress: Integer);
begin
   { just a stub for compatability with TTreeBox}
end;

procedure TTreeInputForm.UpdateRefreshStatus(AStatus: TTreeRebuildPhase);
begin
   { just a stub for compatability with TTreeBox}
end;

procedure TTreeInputForm.InitTreeBoxFonts;
begin
  FOTU_Font := TFont.Create;
  FOTU_Font.Color := clWindowText;
  FOTU_Font.Name := DEFAULT_FONT_NAME;
  FOTU_Font.Style := [];
  FOTU_Font.Size := 12;
  FOTU_Font.Orientation := 0;
  FOTU_Font.Quality := fqProof;
  FOTU_Font.CharSet := DEFAULT_CHARSET;

  FNodeLabelFont := TFont.Create;
  FNodeLabelFont.Assign(FOTU_Font);
  FCaptionFont := TFont.Create;
  FCaptionFont.Assign(FOTU_Font);
  FPrivateFont := TFont.Create;
  FPrivateFont.Assign(FOTU_Font);
  FStatsFont := TFont.Create;
  FStatsFont.Assign(FOTU_Font);
  FBLensFont := TFont.Create;
  FBlensFont.Assign(FOTU_Font);
  FTimesFont := TFont.Create;
  FTimesFont.Assign(FOTU_Font);
  FCharStateFont := TFont.Create;
  FCharStateFont.Assign(FOTU_Font);
  FScaleFont := TFont.Create;
  FScaleFont.Assign(FOTU_Font);

  FStatsFont.Size := 10;
  FTimesFont.Size := 10;
  FBlensFont.Size := 10;
  FScaleFont.Size := 10;

  TreeEditBox.NodeLabelFont := FNodeLabelFont;
  TreeEditBox.OTU_Font := FOTU_Font;
  TreeEditBox.CaptionFont := FCaptionFont;
  TreeEditBox.PrivateFont := FPrivateFont;
  TreeEditBox.StatsFont := FStatsFont;
  TreeEditBox.BLensFont := FBLensFont;
  TreeEditBox.TimesFont := FTimesFont;
  TreeEditBox.ScaleFont := FScaleFont;
  TreeEditBox.CharStateFont := FCharStateFont;
end;

procedure TTreeInputForm.ClearTreeBoxFonts;
begin
  TreeEditBox.NodeLabelFont := nil;
  TreeEditBox.OTU_Font := nil;
  TreeEditBox.CaptionFont := nil;
  TreeEditBox.PrivateFont := nil;
  TreeEditBox.StatsFont := nil;
  TreeEditBox.BLensFont := nil;
  TreeEditBox.TimesFont := nil;
  TreeEditBox.ScaleFont := nil;
  TreeEditBox.CharStateFont := nil;

  FreeAndNil(FNodeLabelFont);
  FreeAndNil(FOTU_Font);
  FreeAndNil(FCaptionFont);
  FreeAndNil(FPrivateFont);
  FreeAndNil(FStatsFont);
  FreeAndNil(FBLensFont);
  FreeAndNil(FTimesFont);
  FreeAndNil(FScaleFont);
  FreeAndNil(FCharStateFont);
end;

function TTreeInputForm.ConstructSelTaxaList: Boolean;
var
  i: Longint;
  foundItem: TListItem;
  aItem: TListItem;
begin
  result := (HiddenList.Count > 0);
  if DispTaxaList = nil then
    Exit;
  try
    FIsUpdating := True;
    DispTaxaList.Clear;
    { add everything from the hidden list to the NameMapListView}
    for i:=0 to HiddenList.Count-1 do
    begin
      aItem := NameMapListView.Items.Add;
      aItem.Caption := HiddenList.Names[i];
      aItem.SubItems.Add(HiddenList.ValueFromIndex[i]);
    end;
    HiddenList.Clear;

    if OnlyUnmappedChk.Checked then
    begin
      { move unchecked items from NameMapListView to the HiddenList}
      i:=0;
      while i <= FActiveNames.Count-1 do
      begin
        UpdateProgress('Updating View...', Round((i + 1)/FActiveNames.Count*100));
        foundItem := NameMapListView.FindCaption(0, FActiveNames.Names[i], false, True, false);
        if foundItem = nil then
          inc(i)
        else
        if (foundItem.SubItems.Count > 0) and (foundItem.SubItems.Strings[0] = '') then
        begin
          inc(i);
        end
        else
        begin
          HiddenList.Add(foundItem.Caption + '=' + foundItem.SubItems[0]);
          NameMapListView.Items.Delete(foundItem.Index);
          inc(i);
        end;
      end;
    end;
  finally
    FIsUpdating := False;
    ClearProgress;
  end;
  NameMapListView.Invalidate;
end;

procedure TTreeInputForm.OnlyUnmappedChkClick(Sender: TObject);
begin
  UpdateDispTaxaList;
end;

procedure TTreeInputForm.NameMapListViewCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  // custom sort.  Sorts by the order items exist in FActiveNames.
  Compare := FActiveNames.IndexOfName(Item1.Caption) - FActiveNames.IndexOfName(Item2.Caption)
end;

procedure TTreeInputForm.HideMappedTreeNamesChkClick(Sender: TObject);
begin
  TreeEditBox.ShowMappedTreeNames := not HideMappedTreeNamesChk.Checked;
  TreeEditBox.Invalidate;
end;

procedure TTreeInputForm.HistoryVisibleBtnClick(Sender: TObject);
begin
  EditHistoryPanel.Visible := not EditHistoryPanel.visible;
end;

procedure TTreeInputForm.OpenTree(aFileName: String);
begin
  try
    ClearTree; // Clears the tree and removes all mapping of active names to tree names.
    TreeEditBox.Visible := false;
    SetTreeFromFile(aFileName); // Reads in the new tree and shows it in the TreeEditBox
    if not FShowAsTopologyEditor then
      InitializeActiveDataAndTreeNameMatches; // Match names which are the same
    FCurrentNewickFile := aFileName;
    if not FShowAsTopologyEditor then
      SetHighlight;
    ResizeTreeToFit;
    TreeEditBox.Visible := true;
    UpdateStatusBar;
  Except on E: Exception do
     MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

{ TTreeInputSession }

constructor TTreeInputSession.Create;
begin
  inherited;
  TaxaNamesMap := TStringList.Create;
  FStrBuilder := TMegaStringBuilder.Create;
end;

procedure TTreeInputSession.Deserialize(Ser: String);
var
  Session: TStringList = nil;
  MapList: TStringList = nil;
begin
  Session := TStringList.Create;
  MapList := TStringList.Create;
  try
    Session.Delimiter := '>';
    Session.QuoteChar := '"';
    Session.DelimitedText := Ser;
    if Session.Count <> 2 then
      Raise Exception.Create('Invalid or corrupt session');
    Newick := Session.Strings[0];
    MapList.Delimiter := '+';
    MapList.QuoteChar := '^';
    MapList.DelimitedText := Session.Strings[1];
    TaxaNamesMap.Text := MapList.Text;
  finally
    if Assigned(Session) then
      Session.Free;
    if Assigned(MapList) then
      MapList.Free;
  end;
end;

destructor TTreeInputSession.Destroy;
begin
  if Assigned(FStrBuilder) then
    FStrBuilder.Free;
  if Assigned(TaxaNamesMap) then
    TaxaNamesMap.Free;
  inherited Destroy;
end;

procedure TTreeInputSession.FillFromForm(Form: TTreeInputForm);
var
  i: Integer;
begin
  if (Form = nil) or (Form.TreeEditBox = nil) or (Form.NameMapListView = nil) then
    Raise Exception.Create('Error: You can not fill a TreeInputSession from a form when one or more of the components are nil.');
  with Form do
  begin
    if TreeEditBox.NoOfNodes > 0 then
      Newick := TreeEditBox.NewickTree
    else
      Newick := '()';
    TaxaNamesMap.Clear;
    for i := 0 to NameMapListView.Items.Count-1 do
      TaxaNamesMap.Add(NameMapListView.Items[i].Caption + '=' + NameMapListView.Items[i].SubItems.Strings[0]);
  end;
end;

procedure TTreeInputSession.FillUsingNamesList(aNamesList: TStringList);
var
  i: Integer = -1;
begin
  TaxaNamesMap.Clear;
  if aNamesList.Count > 0 then
    for i := 0 to aNamesList.Count - 1 do
      TaxaNamesMap.Add(Format('%s=%s', [aNamesList[i], aNamesList[i]]));
end;

procedure TTreeInputSession.RestoreToForm(Form: TTreeInputForm);
var
  i: Integer = -1;
  aListItem: TListItem = nil;
begin
  Form.ClearTree;
  try
    with Form do
    begin
      if Length(Newick) > 2 then
      begin
        SetTreeFromNewick(Newick);
        InitializeActiveDataAndTreeNameMatches; // Match names which are the same
        FCurrentNewickFile := OpenDlg.FileName;
        SetHighlight;
        ResizeTreeToFit;
      end;
      Form.UpdateProgress('Mapping Names...', 1);
      for i := 0 to TaxaNamesMap.Count-1 do
      begin
        aListItem := NameMapListView.FindCaption(0, TaxaNamesMap[i], False, True, True);
        if aListItem <> nil then
          aListItem.SubItems[0] := TaxaNamesMap.Values[TaxaNamesMap.Names[i]];
      end;
    end;
  finally
    Form.ClearProgress;
  end;
end;

function TTreeInputSession.Serialize: String;
var
  i: Integer  = -1;
  aName: String = '';
  aValue: String = '';
begin
  FStrBuilder.Clean;
  FStrBuilder.Add('"');
  FStrBuilder.Add(Newick);
  FStrBuilder.Add('">"');
  for i := 0 to TaxaNamesMap.Count-1 do
  begin
    aName := TaxaNamesMap.Names[i];
    aValue := TaxaNamesMap.ValueFromIndex[i];
    FStrBuilder.Add('^');
    FStrBuilder.Add(aName);
    FStrBuilder.Add('=');
    FStrBuilder.Add(aValue);
    FStrBuilder.Add('^');
    if i < TaxaNamesMap.Count - 1 then
      FStrBuilder.Add('+');
  end;
  FStrBuilder.Add('"');
  Result := FStrBuilder.GenerateString;
end;

procedure TTreeInputForm.RecentTreesClick(Sender: TObject; const ItemTextW: String);
var
  aRow: Integer = -1;
  SessionStr: AnsiString = '';
  Session: TTreeInputSession = nil;
  aKey: AnsiString = '';
begin
  try
    try
      aKey := AnsiString(ItemTextW);
      aRow := FRecentTreesList.IndexOfName(aKey);
      if aRow >= 0 then
      begin
        SessionStr := FRecentTreesList.Values[aKey];
        Session := TTreeInputSession.Create;
        Session.Deserialize(SessionStr);
        Session.RestoreToForm(Self);
        FCurrentNewickFile := aKey;
      end
      else
        MessageDlg('We were unable to locate the saved tree and associated name mapping (MEGA names to tree names).', mtError, [mbOK], 0);
    except
      on E:Exception do
        ShowMessage('Application error: ' + E.Message);
    end;
  finally
    if Assigned(Session) then
      Session.Free;
  end;
end;

procedure TTreeInputForm.TreeEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  try
    if TreeEditBox.NoOfOTUs < TOO_MANY_TAXA_CUTOFF then
      TreeEditBox.Refresh;
    if InResizeMode then
    begin
      QuitResizeMode;
    end;
  except
    on E:Exception do
      ShowMessage('Application error in TreeEndDrag: ' + E.Message);
  end;
end;

procedure TTreeInputForm.TreeToOpenTreeOnShow(FileName: String);
begin
  openFileOnShow := FileName;
end;

procedure TTreeInputForm.ShowMapDropdownAt(CursorPoint: TPoint);
var
  SelectedMapListItem: TListItem = nil;
begin
  if CursorPoint.X < NameMapListView.Column[0].Width then
    Exit; // Only allow the menu to open when over the right column.
  SelectedMapListItem := NameMapListView.GetItemAt(CursorPoint.X, CursorPoint.Y);
  if (SelectedMapListItem <> nil) and (Pos(UserAddedStr, SelectedMapListItem.SubItems.Strings[0]) <> 1) then //and (not PtInRect(SelectedMapListItem.DisplayRect(drLabel), CursorPoint)) then
    DropDownAtItem(SelectedMapListItem);
end;

procedure TTreeInputForm.SetupTreeBox;
begin
  TreeEditBox := TTreeEditBox.Create(Self, tttEditBox);
  InitTreeBoxFonts;
  TreeEditBox.Parent := ScrollSizeContainer;
  TreeEditBox.Align := alClient;
  TreeEditBox.Visible := True;
  TreeEditBox.DoubleBuffered := True;
  TreeEditBox.HilightColor := clSkyBlue;
  TreeEditBox.BeginRefreshCallback := BeginRefresh;
  TreeEditBox.EndRefreshCallback := EndRefresh;
  TreeEditBox.ProgressCallback := UpdateRefreshProgress;
  TreeEditBox.StatusCallback := UpdateRefreshStatus;
  TreeEditBox.DragMode := dmManual;
  TreeEditBox.OnMouseDown := TreeEditBoxMouseDown;
  TreeEditBox.OnMouseUp := TreeEditBoxMouseUp;
  TreeEditBox.OnDragOver := TreeEditBoxNameDragOver;
  TreeEditBox.OnDragDrop := TreeEditBoxNameDragDrop;
  TreeEditBox.OnEndDrag := TreeEditBoxEndDrag;
  TreeEditBox.OnClick := TreeEditBoxClick;
  TreeEditBox.OnDblClick := TreeEditBoxDblClick;
  TreeEditBox.BringToFront;
  TreeEditBox.Canvas.Pen.JoinStyle := pjsBevel;
end;

procedure TTreeInputForm.EditHistoryDblClick(Sender: TObject);
var
  i, selectedNum: Integer;
begin
  SelectedNum := -1;
  for i := 0 to EditHistory.Items.Count-1 do
    if EditHistory.Selected[i] then
    begin
      selectedNum := i;
    end;
  if SelectedNum = -1 then
    Exit;
  TTreeInputSession(EditHistory.Items.Objects[selectedNum]).RestoreToForm(self);
  for i := selectedNum to EditHistory.Items.Count-1 do
    EditHistory.Items.Delete(EditHistory.Items.Count-1);
end;

procedure TTreeInputForm.addTreeToHistory(EType: TEditType);
var
  session: TTreeInputSession;
  h, m, s, ms: Word;
  Human: AnsiString;
begin
  if not isDeveloper then
    Exit;
  session := TTreeInputSession.Create;
  case EType of
    ERoot   : Human := 'A root was placed';
    EFlip   : Human := 'A node was flipped';
    EDelete : Human := 'A taxon was deleted';
    EAdd    : Human := 'A taxon was added';
  end;
  session.FillFromForm(Self);
  decodeTime(now, h, m, s, ms);
  EditHistory.AddItem(Human + ' at ' + IntToStr(h) + ':' + IntToStr(m) + ':' + IntToStr(s), session);
end;

function TTreeInputForm.AdvancedAutomaticMap(TreeStr: AnsiString; ActiveStrList: TStringList): Integer;
var
  bestName: Integer = -1;
  bestDist: Integer = 99999999;
  tempDist, i: Integer;
  ActiveStr: AnsiString = '';
  LDist : TDistance = nil;
begin
  Result := -1;
  // This feature is testing to see if we can improve automatic mapping of taxa from MEGA file to tree.  It is not visible to users (isDeveloper)
  if AdvancedMap.Down then
  begin
    LDist := TDistance.Create;
    try
      for i := 0 to ActiveStrList.Count-1 do
      begin
        ActiveStr := ActiveStrList.Names[i];
        tempDist := LDist.LD(ActiveStr, TreeStr);
        if tempDist < bestDist then
        begin
          bestName := i;
          bestDist := tempDist;
          if bestDist = 1 then
            break; // we can't get any better than 1, so just stop here.
        end;
      end;
    finally
      if Assigned(LDist) then
        LDist.Free;
    end;
  end;
  result := bestName;
end;
{$ENDIF}
end.

