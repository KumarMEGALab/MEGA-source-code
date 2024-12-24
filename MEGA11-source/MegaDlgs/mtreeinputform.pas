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

unit MTreeInputForm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
{$IFDEF VISUAL_BUILD}
uses
  LCLType, LCLIntf, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, MTreeBox, ComCtrls, MegaConsts,
  MegaVerConsts, MegaUtils, MTreeData, ActnList, Menus,
  ClipBrd, MLongIntList, Math, Grids, ValEdit, CheckLst, IniPropStorage,
  MVirtualScrollbox, MPleaseWait, MFindDlg,
  TreeExplorer_HC, Types, mimageform, MegaPrivateFiles;

const
  TOO_MANY_TAXA_CUTOFF = 1000; { don't do things like redraw the tree on resize if we have too many taxa}

type
  TEditType = (ERoot, EFlip, EDelete, EAdd);

type

  { TTreeInputForm }

  TTreeInputForm = class(TForm)
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
    Image1: TImage;
    ButtonImages: TImageList;
    HelpBtnImages: TImageList;
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
    OkBtn: TImage;
    CancelBtn: TImage;
    HelpBtn: TImage;
    MenuItem1: TMenuItem;
    HideMappedTreeNamesChk: TCheckBox;
    OnlyUnMappedChk: TCheckBox;
    NameMapListView: TListView;
    OKCancelPanel: TPanel;
    LeftPanel: TPanel;
    PanPanel: TPanel;
    ScrollAssist: TPanel;
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
    RecentList: TValueListEditor;
    Timer1: TTimer;
    UploadTBtn: TToolButton;
    EditHistoryPanel: TPanel;
    procedure ActionAutosizeExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionFindExecute(Sender: TObject);
    procedure ActionFontExecute(Sender: TObject);
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionNewTreeExecute(Sender: TObject);
    procedure ActionResizeExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionToggleHistoryExecute(Sender: TObject);
    procedure ActionUploadExecute(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure CancelBtnMouseEnter(Sender: TObject);
    procedure CancelBtnMouseLeave(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InitImages;
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpBtnMouseEnter(Sender: TObject);
    procedure HelpBtnMouseLeave(Sender: TObject);
    procedure OkBtnMouseEnter(Sender: TObject);
    procedure OkBtnMouseLeave(Sender: TObject);
    procedure TreeEditBoxMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OKBtnClick(Sender: TObject);
    procedure TreeEditBoxClick(Sender: TObject);
    procedure TreeEditBoxDblClick(Sender: TObject);
    procedure NameMapListViewClick(Sender: TObject);
    procedure TreeToActiveMatchLBxClickCheck(Sender: TObject);
    procedure AutoSizeBtnClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MouseScrollResizeBtnClick(Sender: TObject);
    procedure TreeEditBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeEditBoxDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TreeEditBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
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
    procedure UploadTBtnClick(Sender: TObject);
    procedure SaveTBtnClick(Sender: TObject);
    procedure NameMapListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    {procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer); }
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
    //procedure EnterExitPanModeExecute(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OnlyUnmappedChkClick(Sender: TObject);
    procedure NameMapListViewCompare(Sender: TObject; Item1,
      Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure HideMappedTreeNamesChkClick(Sender: TObject);
    procedure HistoryVisibleBtnClick(Sender: TObject);
     {procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);   }
    {procedure Image2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BirdsEyePanelResize(Sender: TObject);}
    //procedure RecentTreesClick(Sender: TObject; const ItemTextW: String;
    //  var Action: TOvcMRUClickAction);
    procedure TreeEditBoxSearch(Sender: TObject);
    procedure EditHistoryDblClick(Sender: TObject);
    procedure TreeEditBoxChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FLastUpdateTime: TDateTime;
    FIsRootedTree: Boolean;
    userCanceled: Boolean;
    openFileOnShow: String;
    PW: TPleaseWait;
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
    FDragLastPoint : TPoint;
    FTTAChkState: Array of TCheckBoxState; // used in conjunction with SetStateOfTTAChk and FindChangedTTAIndex to track the state of the checks and grays in the TreToActiveMatchLBx
    HiddenList: TStringList;
    { the 5 below are just stubs to be compatible with TTreeBox}
    procedure BeginRefresh;
    procedure EndRefresh;
    procedure UpdateRefreshProgress(AProgress: Integer);
    procedure UpdateRefreshStatus(AStatus: TTreeRebuildPhase);

    procedure SetupTreeBox;
    procedure ScrollBoxDblClick(Sender: TObject);
    procedure ScrollBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ScrollBoxDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ScrollBoxEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ScrollBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure addTreeToHistory(EType: TEditType);
    {procedure UpdateBirdsEyeSelection(X: Integer; Y: Integer); }
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
    procedure TreeSizeUpdate;
    procedure ClearTree;
    function ClearPrevHighlightedTaxa: Boolean;
    procedure EnterResizeMode;
    procedure QuitResizeMode;
    function InResizeMode: Boolean;
    {function NAWhilePanning: Boolean;
    procedure StartPanMode;
    procedure EndPanMode;
    procedure MoveViewer(X, Y: Integer);
    procedure ScrollTo(X, Y: integer); }
    function AdvancedAutomaticMap(TreeStr:AnsiString; ActiveStrList: TStringList):Integer;
    procedure CollectUnusedSequences;
  public
    HasUnusedTreeTaxa: Boolean;
    UnusedSequences: TStringList;
    AskForTreeOnShow: Boolean;
    ShowAsTopologyEditor: Boolean;
    function HasUnusedSequenceData: Boolean;
    procedure TreeToOpenTreeOnShow(FileName: String);
    procedure SetActiveDataNames(AList: TStringList);
    procedure SetTreeFromFile(ATreeFile: String);
    procedure SetTreeFromNewick(ANewickTree: AnsiString);
    procedure SetTreeData(MyTreeData: TTreeData; ANamesList: TStringList; IsRooted: Boolean = False);
    procedure InitializeActiveDataAndTreeNameMatches;
    function  DoesTreeHaveAllActiveNames: Boolean;
    procedure PruneUnwantedTreeNodesAndNames;
    procedure RebuildTreeNames;
    procedure OpenTree(aFileName: String);
    procedure GetTreeData(var treedata: TTreeData);
    function  GetNewickTree: AnsiString;

    property NewickTree: AnsiString read GetNewickTree;
    property FileName: String read FCurrentNewickFile;
    property IsRootedTree: Boolean read FIsRootedTree;
  end;

type
  TTreeInputSession = class(TObject)
    Newick: String;
    NameMap: TStringList;
    Constructor Create;
    Destructor Destroy; override;
    procedure FillFromForm(Form: TTreeInputForm);
    procedure RestoreToForm(Form: TTreeInputForm);
    function Serialize: String;
    procedure Deserialize(Ser: String);
end;

  //const
  //  OpenHand = 1;
  //  ClosedHand = 2;
var
  TempSession: TTreeInputSession;
  TreeInputForm: TTreeInputForm;
  LastSearchIndex: Integer;
  SX: Integer;
  SY: Integer;
  LX: Integer;
  LY: Integer;

  function StringHeight(TheStr: String; TheFont: TFont): Integer;

{$ENDIF}
implementation

{$IFDEF VISUAL_BUILD}
uses
  MTreeList, RegExpr, Levenshtein, mhelpfiles, mhelpkeywords, mshortcutshelper,
  dateutils;

{$R *.lfm}

function StringHeight(TheStr: String; TheFont: TFont): Integer;
var
  BM: TBitmap;
begin
  Result := 0;
  BM := TBitmap.Create;
  BM.Canvas.Font := TheFont;
  Result := BM.Canvas.TextHeight(TheStr);
  BM.Free;
end;

procedure TTreeInputForm.FormCreate(Sender: TObject);
begin
  FLastUpdateTime := Now;
  FIsRootedTree := False;
  UpdateShortcutsForMacOs(ActionList1);
  HasUnusedTreeTaxa := False;
  FIsUpdating := False;
  HiddenList := TStringList.Create;
  UnusedSequences := TStringList.Create;
  userCanceled := False;
  PW := TPleaseWait.Create(Self);
  myFindDlg := nil;
  DispTaxaList := TLongIntList.Create;
  FActiveNames := TStringList.Create;
  FTreeNames   := TStringList.Create;
  FNewNodeNum := 1;
  //Screen.Cursors[OpenHand] := LoadCursor(hInstance,'OPENHAND');
  //Screen.Cursors[ClosedHand] := LoadCursor(hInstance,'CLOSEDHAND');
  ShowAsTopologyEditor := False;
  openFileOnShow := EmptyStr;
  SetupTreeBox;
  HelpContext := RH_Tree_Topology_Editor;
  InitImages;
  ImageForm.UpdateImgList(Self);
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
end;

procedure TTreeInputForm.InitImages;
begin
  {$IFDEF DARWIN}
  HelpBtn.Proportional:=True;
  CancelBtn.Proportional:=True;
  OkBtn.Proportional:=True;
  {$ENDIF}
end;

procedure TTreeInputForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  nCount     : integer;
begin
  // find out how many files we're accepting
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
      ShowMessage('Oh no! Failed to initialize the help browser: ' + E.Message);
  end;
end;

procedure TTreeInputForm.ActionUploadExecute(Sender: TObject);
begin
  OpenDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(OpenDlg.InitialDir);
  if OpenDlg.Execute then
  begin
    OpenTree(OpenDlg.FileName);
  end;
end;

procedure TTreeInputForm.CancelBtnClick(Sender: TObject);
begin
  if (fsModal in FormState) then
    ModalResult := mrCancel
  else
    Close;
end;

procedure TTreeInputForm.CancelBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(3, CancelBtn.Picture.Bitmap);
end;

procedure TTreeInputForm.CancelBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(2, CancelBtn.Picture.Bitmap);
end;

procedure TTreeInputForm.FormActivate(Sender: TObject);
begin
  ActionToggleHistory.Visible := IsDeveloper;
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
  FontDialog1.Font := TreeEditBox.OTU_Font;
  if FontDialog1.Execute then
    TreeEditBox.OTU_Font := FontDialog1.Font;
  TreeEditBox.Refresh;
end;

procedure TTreeInputForm.ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
begin
  Handled := True;
  if MilliSecondsBetween(Now, FLastUpdateTime) < 300 then
    Exit;
  FLastUpdateTime := Now;
  if not ShowAsTopologyEditor then
    ActionAddOTU.Enabled :=  (TreeEditBox.NoOfOTUs < NameMapListView.Items.Count) and (TreeEditBox.FocusedIndex > 0)
  else
    ActionAddOTU.Enabled := True;
  ActionRemoveOTU.Enabled :=  (TreeEditBox.FocusedNameIndex > 0) and (TreeEditBox.NoOfOTUs > 2);
  ActionRoot.Enabled := TreeEditBox.FocusedIndex > 0;
  ActionFlip.Enabled := TreeEditBox.FocusedIndex > TreeEditBox.NoOfOTUs;
  OKBtn.Enabled := (not ActionAddOTU.Enabled);
  ActionUndo.Enabled := TreeEditBox.CanUndo;
end;

procedure TTreeInputForm.ActionFindExecute(Sender: TObject);
begin
  if MyFindDlg = nil then
  begin
    MyFindDlg := TFindDlg.Create(Self);
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
  ResizeTreeToFit;
end;

procedure TTreeInputForm.ActionSaveExecute(Sender: TObject);
begin
  SaveDialog1.InitialDir := ChangeInitialDirectorySaveDialogForMac(SaveDialog1.InitialDir);
  if ShowAsTopologyEditor then
  begin
    with TStringList.Create do
    begin
      try
        SaveDialog1.InitialDir := ChangeInitialDirectorySaveDialogForMac(SaveDialog1.InitialDir);
        if SaveDialog1.Execute then
        begin
          Add(TreeEditBox.NewickTree);
          SaveToFile(SaveDialog1.FileName);
        end;
      finally
        Free;
      end;
    end;
  end
  else
  begin
    if SaveDialog1.Execute then
    begin
      with TStringList.Create do
      begin
        try
            Add(TreeEditBox.NewickPrivateTree);
            SaveToFile(SaveDialog1.FileName);
            Clear;
            if(MessageDlg('Would you like to save a copy of the tree with the original tree names also?', mtConfirmation, [mbYes, mbNo], 0) in [mrYes]) then
            begin
              SaveDialog1.InitialDir := ChangeInitialDirectorySaveDialogForMac(SaveDialog1.InitialDir);
              if SaveDialog1.Execute then
              begin
                Add(TreeEditBox.NewickTree);
                SaveToFile(SaveDialog1.FileName);
              end;
            end;
        finally
          Free;
        end;
      end;
    end;
  end;
end;

procedure TTreeInputForm.ActionToggleHistoryExecute(Sender: TObject);
begin
  EditHistoryPanel.Visible := not EditHistoryPanel.visible;
end;

procedure TTreeInputForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(PW);
  FreeAndNil(DispTaxaList);
  FreeAndNil(FActiveNames);
  FreeAndNil(FTreeNames);
  FreeAndNil(MyFindDlg);
  if Assigned(UnusedSequences) then
    UnusedSequences.Free;
  if Assigned(HiddenList) then
    HiddenList.Free;
end;

procedure TTreeInputForm.HelpBtnMouseEnter(Sender: TObject);
begin
  HelpBtnImages.GetBitmap(1, HelpBtn.Picture.Bitmap);
end;

procedure TTreeInputForm.HelpBtnMouseLeave(Sender: TObject);
begin
  HelpBtnImages.GetBitmap(0, HelpBtn.Picture.Bitmap);
end;

procedure TTreeInputForm.OkBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(1, OkBtn.Picture.Bitmap);
end;

procedure TTreeInputForm.OkBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(0, OkBtn.Picture.Bitmap);
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
  aItem: TListItem;
begin
  FActiveNames.Clear;
  if AList.Count > 0 then
    for i:=0 to AList.Count-1 do
    begin
      FActiveNames.Add(AList[i]+'=');
      aItem := NameMapListView.Items.Add;
      aItem.Caption := AList[i];
      aItem.SubItems.Add(EmptyStr);
      //NameMapListView.AddItem(AList[i], nil);
      //NameMapListView.FindCaption(-1, AList[i], false, false, false).SubItems.Add(EmptyStr);
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
    PW.Action := 'Reading Tree from file';
    FileSList.LoadFromFile(ATreeFile);
    PW.Action := 'Loading tree';
    SetTreeFromNewick(FileSList.Text);
    FCurrentNewickFile := ATreeFile;
  finally
    FreeAndNil(FileSList);
  end;
end;

procedure TTreeInputForm.SetTreeData(MyTreeData: TTreeData; ANamesList: TStringList; IsRooted: Boolean = False);
var
  AEmptyList: TStringList;
  i: Integer;
begin
  AEmptyList := nil;
  try
    AEmptyList := TStringList.Create; // this indicates that there are no private names
    for i:=0 to ANamesList.Count-1 do
      AEmptyList.Add(EmptyStr);
    TreeEditBox.SetTreeData(MyTreeData, ANamesList, AEmptyList, IsRooted);
    FTreeNames.Clear;
    for i:=0 to ANamesList.Count-1 do
      if Pos('=', ANamesList[i]) <> 0 then
        FTreeNames.Add(ANamesList[i])
      else
        FTreeNames.Add(ANamesList[i]+'=');
  finally
    FreeAndNil(AEmptyList)
  end;
end;

procedure TTreeInputForm.InitializeActiveDataAndTreeNameMatches;
var
  i, iTreeName, tindex: Integer;
  AActiveName : AnsiString;
  aActiveAlphaNum: TStringList = nil;
  aTreeAlphaNum: TStringList = nil;
begin
  if ShowAsTopologyEditor then
    Exit;
  if FActiveNames.Count = 0 then
    raise Exception.Create('Empty list of active data names');

  try
    PW.Show;
    PW.Action := 'Removing non alphanumerics for matching';
    aActiveAlphaNum := RegexReplaceAStringList(FActiveNames, '[^A-Za-z0-9]', ''); // strip away all but alphanumeric
    aTreeAlphaNum := RegexReplaceAStringList(FTreeNames, '[^A-Za-z0-9]', '');

    for i:=0 to FActiveNames.Count-1 do
    begin
      if (i mod 5) = 0 then // we dont' need to update the visual status for every taxa, that would slow things down.
        PW.Action := 'Mapping Taxon ' + IntToStr(i+1) + '/' + IntToStr(FActiveNames.Count);
      AActiveName := aActiveAlphaNum.Names[i];
      //see if this name appears in the primary TreeNames anywhere
      iTreeName   := aTreeAlphaNum.IndexOfName(AActiveName);
      if iTreeName >= 0 then // found the name in the tree list
      begin
        AActiveName := FActiveNames.Names[i]; // Use the real name, rather than the sanatized name.
        if (Length(FTreeNames.Values[AActiveName]) > 0) or
           (Length(FActiveNames.Values[AActiveName]) > 0) then
              raise Exception.Create('Duplicate names found: '+ AActiveName);
        FActiveNames.Values[AActiveName]  := AActiveName; // painfully obvious maps
        FTreeNames.Values[AActiveName]    := AActiveName;
        NameMapListView.FindCaption(-1, AActiveName, false, false, false).SubItems.Strings[0] := AActiveName;
        // now initialize the Private Names for the TreeEditBox
        TreeEditBox.SetOTUPrivateName(iTreeName+1, AActiveName, True);
      end;
    end;
    PW.Action := 'Locating unmatched taxa';
    for i:=1 to TreeEditBox.NoOfOTUs do
    begin
      if TreeEditBox.OTUPrivateName[i] = EmptyStr then
      begin
        if isDeveloper then
        begin
          tindex := AdvancedAutomaticMap(aTreeAlphaNum.Names[i-1], aActiveAlphaNum);
          if tindex <> -1 then
          begin
            AActiveName := FActiveNames.Names[tindex];
            FActiveNames.Values[AActiveName] := TreeEditBox.OtuName[i];
            FTreeNames.Values[TreeEditBox.OtuName[i]] := AActiveName;
            NameMapListView.FindCaption(-1, AActiveName, false, false, false).SubItems.Strings[0] := TreeEditBox.OtuName[i];
            TreeEditBox.SetOTUPrivateName(i, AActiveName, True);
          end
          else
            TreeEditBox.SetOTUPrivateName(i, UndefinedStr);
        end
        else
          TreeEditBox.SetOTUPrivateName(i, UndefinedStr);
      end;
    end;
    PW.Hide;
  finally
    if Assigned(aActiveAlphaNum) then
      aActiveAlphaNum.Free;
    if Assigned(aTreeAlphaNum) then
      aTreeAlphaNum.Free;
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
    while i <= NoOfOTUs do
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

procedure TTreeInputForm.FormShow(Sender: TObject);
const
  CR_FILE = 1000;
  CR_SCRATCH = 1001;
  CR_RANDOM = 1002;
  CR_CANCEL = 1003;
var
  CustomRes: Integer;
begin
  HelpContext := RH_Tree_Topology_Editor;
  if openFileOnShow <> EmptyStr then // If we have already supplied a tree but it needs to be edited we do NOT want to ask the user for a tree again, instead automaticlly show the tree which needs mapping.
  begin
    OpenTree(openFileOnShow);
  end
  else
  begin
    CustomRes := QuestionDlg('Tree Construction', 'How would you like to edit a tree:', mtCustom, [CR_FILE, 'From a File', CR_SCRATCH, 'From Scratch', CR_RANDOM, 'Random Tree From Active Data', CR_CANCEL, 'Cancel', 'IsCancel'], 0);
    if CustomRes = CR_FILE then
    begin
      UploadTBtnClick(nil);
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
          Timer1.Enabled := True;
          if fsModal in FormState then
            ModalResult := mrCancel;
          Exit;
        end;
      end;
    end;
  end;
  SetHighLight;
  TreeEditBox.Refresh;

  try
    BeginFormUpdate;
    if ShowAsTopologyEditor then
    begin
      LeftPanel.Visible              := False;
      Splitter1.Visible              := False;
      OnlyUnmappedChk.Visible        := False;
      HideMappedTreeNamesChk.Visible := False;
      OKBtn.Visible                  := False;
      CancelBtn.Visible              := False;
      ShowHint                       := True;
      HelpBtn.Left := ClientWidth - HelpBtn.Width - 8;
      HelpBtn.Anchors := [akTop, akRight];
      TreeEditBox.EditNameEnabled := true;
      Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Tree Topology Editor';
    end
    else
      Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Tree Input Dialog';
    if isDeveloper then
    begin
  //    AdvancedMap.Visible := True;
      HistoryVisibleBtn.Visible := True;
    end;
  finally
    EndFormUpdate;
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

procedure TTreeInputForm.GetTreeData(var treedata: TTreeData);
begin
  PruneUnwantedTreeNodesAndNames;
  TreeEditBox.GetTreeData(treedata);
end;

procedure TTreeInputForm.OKBtnClick(Sender: TObject);
var
  SessionToAdd: AnsiString;
  NewSession: TTreeInputSession;
  i, MatchCount, Row: Integer;
  NameToAdd: AnsiString;
  PreviouslyEdited: Boolean;
  Response: Integer;
  query: String;
  doAbort: Boolean;
begin
  doAbort := False;
  MatchCount := 0;
  with FActiveNames do
    for i:=0 to Count-1 do
      if Length(Values[Names[i]]) = 0 then
        Inc(MatchCount);

  if MatchCount > 0 then
  begin
    if MatchCount = 1 then
      query := '1 name in the active data is not included in the tree. Continue without using sequence data for that name?'
    else
      query := IntToStr(MatchCount) + ' names in the active data are not included in the tree. Continue without using sequence data for those names?';
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
    while RecentList.FindRow(NameToAdd, Row) or RecentList.FindRow(NameToAdd + ' (edited)', Row) do
    begin
      NameToAdd := FileName + '::' + IntToStr(i);
      inc(i);
    end;

    if TreeEdited or PreviouslyEdited then
      NameToAdd := NameToAdd + ' (edited)';

    NewSession := TTreeInputSession.Create;
    NewSession.FillFromForm(self);
    SessionToAdd := NewSession.Serialize;
    for i := 0 to RecentList.RowCount-1 do
      if RecentList.Cells[1, i] = SessionToAdd then
        break;  // If this exact newick tree already exists then no need to add it to the list again.
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
      NameMapListView.Selected := NameMapListView.FindCaption(-1, TreeEditBox.OTUPrivateName[TreeEditBox.FocusedNameIndex], false, false, false);
      NameMapListView.OnChange := NameMapListViewChange;
      ClearPrevHighlightedTaxa;
      TreeEditBox.SetCustomHilightColorOTUName(TreeEditBox.FocusedNameIndex, true, clBlue);
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
  if (TreeEditBox.FocusedNameIndex > 0) and (not ShowAsTopologyEditor) then
  begin
    SelectedMapListItem := NameMapListView.FindCaption(-1, TreeEditBox.OTUPrivateName[TreeEditBox.FocusedNameIndex], false, false, false);
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
  SelectedMapListItem: TListItem;
begin
    CursorPoint := TPoint.Create(0,0);
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
  TreeNameIndex: Integer;
  TreeNameCoords: TRect;
begin
  result := false;
  with TreeEditBox do
  begin
    TreeNameIndex := IndexOfName[TreeName];  // If name exists in the tree
    if TreeNameIndex = -1 then
      exit;
    // Set the ScrollAssist over the name and scroll so that it is in view.
    TreeNameCoords := NodeCoords(TreeNameIndex);
    ScrollAssist.Left := TreeNameCoords.Left;
    ScrollAssist.Top := TreeNameCoords.Top - TreeEditBox.ScrollTop;
    //if not FEmfScrollBox.IsVisible(Point(TreeNameCoords.Left, TreeNameCoords.Top)) then
      TreeEditBox.ScrollTo(ScrollAssist.Left, ScrollAssist.Top);
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
      if (MessageDlg('This tree name is already associated with another MEGA name, would you like to take ownership or leave it as it is?', mtWarning, [mbYes, mbNo], 0) = mrYes) then
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

procedure TTreeInputForm.AutoSizeBtnClick(Sender: TObject);
begin
  {if NAWhilePanning then
    exit;}
  ResizeTreeToFit;
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
    Height := (PPOTU+1) * NoOfOTUs;
    if Height < (ScrollSizeContainer.Height - GetSystemMetrics(SM_CXHSCROLL)) then
     Height := ScrollSizeContainer.Height - GetSystemMetrics(SM_CXHSCROLL);
    PixelsPerOTU := PPOTU;
    Width := ScrollSizeContainer.Width - GetSystemMetrics(SM_CXVSCROLL);
    TreeWidth := ScrollSizeContainer.Width - GetSystemMetrics(SM_CXVSCROLL) - 15 - LargestWidthOfOTUNames;  // 15 for the tree's padding on each side.
    SetAttrIndex;
    Refresh;
  end;
end;

procedure TTreeInputForm.FormResize(Sender: TObject);
begin
  TreeEditBox.Left := 0;
  TreeEditBox.Top := 0;
  TreeEditBox.Invalidate;
end;

procedure TTreeInputForm.MouseScrollResizeBtnClick(Sender: TObject);
begin
  {if NAWhilePanning then
    exit;}
  if not InResizeMode then
    EnterResizeMode
  else
    QuitResizeMode;
end;

procedure TTreeInputForm.TreeEditBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CursorPos: TPoint;
begin
  CursorPos := TPoint.Create(0,0);
  if InResizeMode then
    TreeEditBox.EndDrag(true);
  if Button = mbRight then
  begin
    GetCursorPos(CursorPos);
    TreeRightClick.Popup(CursorPos.X, CursorPos.Y);
  end
  else
    TreeEditBoxClick(Sender);
end;

procedure TTreeInputForm.TreeEditBoxDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  PPOTU: Integer;
  CursorPos: TPoint;
  DoRefresh: Boolean;
  SrcListItemPt: TPoint;
begin
  CursorPos := TPoint.Create(0,0);
  GetCursorPos(CursorPos);
  CursorPos := TreeEditBox.ScreenToClient(CursorPos);
  DoRefresh := false;
  if (Source.ClassType = TListView) and (not ShowAsTopologyEditor) then // draging an active name to the tree from ListView
  begin
    Accept := false;
    SrcListItemPt := Point(X,Y);
    if (TreeEditBox.FocusedIndex > 0) or (TreeEditBox.NameFocused) then
      TreeEditBox.Repaint;
    TreeEditBox.FocusOnClosest(SrcListItemPt, 10); // Added new functionality to focus on the closest point within 10px, rather than just the one under the cursor.
    if (TreeEditBox.FocusedIndex > 0) or (TreeEditBox.NameFocused) then
      Accept := true;
    exit;
  end;
  DoRefresh := true;
  if not InResizeMode then
  begin
//    TreeEditBox.DoMouseMove([ssLeft], X, Y);
    Exit;
  end;
  with TreeEditBox do
  begin
    DoRefresh := (not PointsEqual(FDragLastPoint, CursorPos)); // only refresh if we have moved since the last test, this prevents excessive redrawing.
    FDragLastPoint := CursorPos;
    { TODO 4 -oDan : When someone has time, add a check here to see if the mouse is STILL down.  This is because you may have released the mouse earlier but there was a backlog of messages to the DragOver so when they are processed they pick up the cursor position of after you've let up on the mouse, which can be bad. }
    PPOTU := FStartPixelsPerOTU - ((FDragStartPoint.Y - CursorPos.Y) div 4);
    Height := (PPOTU+1) * NoOfOTUs;
    PixelsPerOTU := PPOTU;
    Width := FStartTreeWidth - ((FDragStartPoint.X - CursorPos.X) div 1) + (GetSystemMetrics(SM_CXVSCROLL) + LargestWidthOfOTUNames);
    if Width < (TreeEditBox.Width - GetSystemMetrics(SM_CXVSCROLL)) then
      Width := TreeEditBox.Width - GetSystemMetrics(SM_CXVSCROLL);
    if Height < TreeEditBox.Height - GetSystemMetrics(SM_CXVSCROLL) then
      Height := TreeEditBox.Height - GetSystemMetrics(SM_CXVSCROLL);

    TreeWidth := FStartTreeWidth - ((FDragStartPoint.X - CursorPos.X) div 1);

    if DoRefresh then // allows us to refresh the tree while dragging and avoiding a side effect from the MouseUp action.
    begin
      OnMouseUp := nil;
      Refresh;
      OnMouseUp := TreeEditBoxMouseUp;
    end;
  end;     
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

procedure TTreeInputForm.TreeEditBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  TargetBranch: Integer = -1;
begin
  if Source.ClassType = TListView then
  begin
    if (TreeEditBox.FocusedNameIndex <> 0) then // Dragging to a name, assocaite the MEGA name with the tree name.
    begin

      if Pos(UserAddedStr, TreeEditBox.OTUName[TreeEditBox.FocusedNameIndex]) <> 1 then
      begin
        TreeEditBox.SetOTUPrivateName(TreeEditBox.FocusedNameIndex, TListView(Source).Selected.Caption);
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
   // SetHighlight;
     //TreeEditBox.Refresh;
     //UpdateDispTaxaList;
  end
  else
  begin
    TreeEditBox.ClearTrailers;
    if TreeEditBox.BranchFocused then
      TargetBranch := TreeEditBox.CheckTargetBranch(TreeEditBox.DropLocation.X, TreeEditBox.DropLocation.Y);
    if TreeEditBox.BranchFocused and (TargetBranch > 0) then
    begin
      TreeEditBox.MoveNode(TreeEditBox.FocusedIndex, TargetBranch);
      TreeEditBox.Refresh;
      TreeEditBox.FocusOnBranch(TargetBranch);
    end
  end;

end;

procedure TTreeInputForm.SetTreeNameForActive(ActiveName: AnsiString; TreeName: AnsiString);
var
  ActiveItem: TListItem;
  TreeIndex, PrevActiveIndex: Integer;
  i: Integer;
begin
  TreeToActiveMatchLBx.Visible := false;
  NameMapListView.Invalidate;
  // first ensure that both of the names are valid.
  ActiveItem := NameMapListView.FindCaption(-1, ActiveName, false, false, false);
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
  TreeEditBox.Refresh;
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
  i: Integer;
begin
  SetLength(FTTAChkState, TreeToActiveMatchLBx.Count);
  for i:=0 to high(FTTAChkState) do
  begin
    FTTAChkState[i] := TreeToActiveMatchLBx.State[i];
    {case TreeToActiveMatchLBx.State[i] of
      cbUnchecked: FTTAChkState[i] := 0;
      cbChecked  : FTTAChkState[i] := 1;
      cbGrayed   : FTTAChkState[i] := 2;
    end;}
  end;
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
  for i:=0 to TreeToActiveMatchLBx.Count-1 do
    TreeToActiveMatchLBx.State[i] := cbUnchecked;

  for i:=0 to NameMapListView.Items.Count-1 do
  begin
    SubitemIndexInLBx := TreeToActiveMatchLBx.Items.IndexOf(NameMapListView.Items.Item[i].SubItems.Strings[0]);
    if SubitemIndexInLBx >= 0 then
      if NameMapListView.Selected = NameMapListView.Items.Item[i] then
        TreeToActiveMatchLBx.State[SubItemIndexInLBx] := cbChecked
      else
        TreeToActiveMatchLBx.State[SubItemIndexInLBx] := cbGrayed;
  end;
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
  {if NAWhilePanning then
    exit;}
  // NOTE: This undo only will undo moving of branches, undeleting, etc., NOT associations made by the NameMapViewList
  TreeEditBox.Undo;
  SetHighlight;
  TreeEditBox.Refresh;
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
    if not ShowAsTopologyEditor then
      SetOTUPrivateName(IndexOfName['New Node ' + IntToStr(FNewNodeNum)], UndefinedStr);
    inc(FNewNodeNum);
    Refresh;
    SetHighlight;
  end;
  TreeEdited := True;
end;

procedure TTreeInputForm.ActionRemoveOTUExecute(Sender: TObject);
begin
  {if NAWhilePanning then
    exit;}
  with TreeEditBox do
  begin
    if FocusedNameIndex = 0 then Exit;
    AddTreeToHistory(EDelete);
    ClearActiveNameOnTreeName(OTUName[FocusedNameIndex]);  // needed to update the list if there was a association on the removed OTU!
    RemoveOTU(FocusedNameIndex);
    Refresh;
    SetHighlight;
  end;
  TreeEdited := True;
end;

procedure TTreeInputForm.ActionRootExecute(Sender: TObject);
begin
  {if NAWhilePanning then
    exit;}
  AddTreeToHistory(ERoot);
  TreeEditBox.MakeRootOnBranch;
  TreeEdited := True;
end;

procedure TTreeInputForm.ActionFlipExecute(Sender: TObject);
begin
  {if NAWhilePanning then
    exit;}

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
  {if NAWhilePanning then
    exit;}
  TreeEditBox.CopyImageToClipBoard;
end;

procedure TTreeInputForm.FindTBtnClick(Sender: TObject);
begin
  {if NAWhilePanning then
    exit;}
  if MyFindDlg = nil then
  begin
    MyFindDlg := TFindDlg.Create(Self);
    MyFindDlg.Tree := TreeEditBox;
  end;
  MyFindDlg.Show;
end;

procedure TTreeInputForm.FontTBtnClick(Sender: TObject);
begin
  {if NAWhilePanning then
    exit; }
  FontDialog1.Font := TreeEditBox.OTU_Font;
  if FontDialog1.Execute then
    TreeEditBox.OTU_Font := FontDialog1.Font;
  TreeEditBox.Invalidate;
end;

procedure TTreeInputForm.NewTreeBtnClick(Sender: TObject);
begin
  {if NAWhilePanning then
    exit;}
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
  k: Integer;
  NumRandomTaxa: Integer = -1;
  NumRandomTaxaStr: String = '';
  TreeArray: TIntArray;

  procedure RandMergeTaxa(aTree: TTreeData; IntArray: TIntArray; startingI: Integer; NumOrigOTUs: Integer);
  var
    RandomDes1, RandomDes2: Integer;
    SelDes1, SelDes2: Integer;
    i, j, s, NumOfNodesToCreate: Integer;
    NextArray: TIntArray;
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
  PW.Show;
  try
    try
    TreeEdited := False;
    FCurrentNewickFile := 'Randomly Generated Tree';
    Randomize;
    if (not ShowAsTopologyEditor) then
      PW.Action := 'Adding MEGA supplied names'
    else
      PW.Action := 'Adding random names to taxa pool.';
    PW.Show;
    TaxaPool := TStringList.Create;
    if (not ShowAsTopologyEditor) and (NumNodes < 1) then
      TaxaPool.AddStrings(FActiveNames)
    else
    begin
      NumRandomTaxa := NumNodes;
      if NumRandomTaxa < 1 then
      begin
        NumRandomTaxa := 0;  // Using imperical tests it appears that the Tree system can handle a max of 16k nodes, this is set as the max.
        repeat
          if not InputQuery('Number of taxa needed', 'How many taxa would you like in this random tree? (max is 16000)', NumRandomTaxaStr) then
            Exit;
          TryStrToInt(NumRandomTaxaStr, NumRandomTaxa)
        until ((NumRandomTaxa > 0) and (NumRandomTaxa < 16000));
      end;
      for k:=1 to NumRandomTaxa do
      begin
        if FActiveNames.Count > 0 then
        begin
          if k < FActiveNames.Count then
            TaxaPool.Add(FActiveNames.Strings[k-1] + '=' + FActiveNames.Strings[k-1])
          else
            TaxaPool.Add('Taxon ' + IntToStr(k) + '=' + 'Taxon ' + IntToStr(k));
        end
        else
          TaxaPool.Add('Taxon ' + IntToStr(k) + '=' + 'Taxon ' + IntToStr(k));
      end;
    end;
    PW.Action := 'Setting up empty tree';
    tree := TTreeData.Create(TaxaPool.Count,false,false,false);

    SetLength(TreeArray, TaxaPool.Count);
    for k := 0 to Length(TreeArray) - 1 do
      TreeArray[k] := k;
    PW.Action := 'Generating Random Tree';
    RandMergeTaxa(Tree, TreeArray, 0, Length(TreeArray));
    ClearTree;
    for k := 0 to TaxaPool.Count - 1 do
      TaxaPool[k] := TaxaPool.Names[k];
    PW.Action := 'Activating the random tree';
    SetTreeData(tree, TaxaPool);
    if (not ShowAsTopologyEditor) then
    begin
      PW.Action := 'Mapping MEGA names to Tree names';
      InitializeActiveDataAndTreeNameMatches;
    end;
    FCurrentNewickFile := 'Randomly Generated Tree';
    PW.Action := 'Resizing Tree to fit window';
    ResizeTreeToFit;
    SetHighLight;
    tree.Free;
    TaxaPool.Free;
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    PW.hide;
  end;
end;

{procedure TTreeInputForm.Search(Query: String; Contains: Boolean; QueryChanged: Boolean);
var
 i, Start: Integer;
 FoundAMatch: Boolean;
begin
  FoundAMatch := false;
  if (TreeEditBox.FocusedIndex > 0) and (TreeEditBox.FocusedIndex < TreeEditBox.NoOfOTUs) and (not QueryChanged) then
    Start := TreeEditBox.FocusedIndex +1
  else
    Start := 1;
 for i:=1 to TreeEditBox.NoOfOTUs do
 begin
   TreeEditBox.SetCustomHilightColorOTUName(i, False, clWhite); // unhighlight the search color for everything.
 end;
 //SetHighlight;
 TreeEditBox.ClearFocus;
 if Query = '' then
 begin
   TreeEditBox.Refresh;
   Exit;
 end;

 for i:=Start to TreeEditBox.NoOfOTUs do
 begin
   if (Contains and AnsiContainsText(TreeEditBox.OTUName[i], Query)) OR
   ((not Contains) and (Pos(uppercase(Query), uppercase(TreeEditBox.OTUName[i]))=1)) then
   begin
     if Find.HighlightAllChk.Checked then
     begin
      TreeEditBox.SetCustomHilightColorOTUName(i, true, clLime);
     end
     else
     begin
       TreeEditBox.SetCustomHilightColorOTUName(i, true, clLime);
       TreeEditBox.FocusOnNode(i);
       TreeJumpToTreeName(TreeEditBox.OTUName[i], False);
       FoundAMatch := true;
       LastSearchIndex := i;
       break;
     end;
   end;
 end;
 if (Start <> 1) and (not FoundAMatch) then
 begin
   TreeEditBox.FocusOnNode(0);
   Search(Query, Contains, QueryChanged);
 end;
 TreeEditBox.Refresh;
end;
  }


procedure TTreeInputForm.TreeEditBoxEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  if InResizeMode then // resizing tree
  begin
    QuitResizeMode;
    //TreeSizeUpdate;
  end;    
end;

procedure TTreeInputForm.TreeSizeUpdate;
begin
  //TreeEditBox.Width := FEmfScrollBox.Width - GetSystemMetrics(SM_CXVSCROLL);
  //TreeEditBox.Height := FEmfScrollBox.Height - GetSystemMetrics(SM_CXVSCROLL);
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

procedure TTreeInputForm.UploadTBtnClick(Sender: TObject);
begin
  {if NAWhilePanning then
    exit;}

  OpenDlg.InitialDir := ChangeInitialDirectorySaveDialogForMac(OpenDlg.InitialDir);
  OpenDlg.Filter := NewickFilesFilter;
  if OpenDlg.Execute then
  begin
    OpenTree(OpenDlg.FileName);
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

procedure TTreeInputForm.SaveTBtnClick(Sender: TObject);
begin
  SaveDialog1.InitialDir := ChangeInitialDirectorySaveDialogForMac(SaveDialog1.InitialDir);
  if ShowAsTopologyEditor then
  begin
    with TStringList.Create do
    begin
      try
        SaveDialog1.InitialDir := ChangeInitialDirectorySaveDialogForMac(SaveDialog1.InitialDir);
        if SaveDialog1.Execute then
        begin
          Add(TreeEditBox.NewickTree);
          SaveToFile(SaveDialog1.FileName);
        end;
      finally
        Free;
      end;
    end;
  end
  else
  begin
    if SaveDialog1.Execute then
    begin
      with TStringList.Create do
      begin
        try
            Add(TreeEditBox.NewickPrivateTree);
            SaveToFile(SaveDialog1.FileName);
            Clear;
            if(MessageDlg('Would you like to save a copy of the tree with the original tree names also?', mtConfirmation, [mbYes, mbNo], 0) in [mrYes]) then
            begin
              SaveDialog1.InitialDir := ChangeInitialDirectorySaveDialogForMac(SaveDialog1.InitialDir);
              if SaveDialog1.Execute then
              begin
                Add(TreeEditBox.NewickTree);
                SaveToFile(SaveDialog1.FileName);
              end;
            end;
        finally
          Free;
        end;
      end;
    end;
  end;
end;

procedure TTreeInputForm.ScrollBoxDblClick(Sender: TObject);
begin
  TreeEditBox.DoDblClick;
end;

procedure TTreeInputForm.ScrollBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
//var
//  BoxX, BoxY: Integer;
begin
  //if FEmfScrollbox.ClientToBox(X, Y, BoxX, BoxY) then
  //  TreeEditBoxDragDrop(Sender, Source, X, Y);
end;

procedure TTreeInputForm.ScrollBoxDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
//var
//  BoxX, BoxY: Integer;
begin
  //if FEmfScrollbox.ClientToBox(X, Y, BoxX, BoxY) then
  //  TreeEditBox.DoDragOver(Source, BoxX, BoxY, State, Accept);
end;

procedure TTreeInputForm.ScrollBoxEndDrag(Sender, Target: TObject; X, Y: Integer);
//var
//  BoxX, BoxY: Integer;
begin
  //if FEmfScrollbox.ClientToBox(X, Y, BoxX, BoxY) then
  //begin
  //  TreeEditBox.DoDragEnd(Target, BoxX, BoxY);
  //  TreeEndDrag(Sender, Target, BoxX, BoxY);
  //end;
end;

procedure TTreeInputForm.ScrollBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  TreeEditBox.DoKeyDown(Key, Shift);
end;

procedure TTreeInputForm.ScrollBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//var
//  BoxX, BoxY: Integer;
begin
//  TreeEditBox.BeginDrag(True);
  //if FEmfScrollbox.ClientToBox(X, Y, BoxX, BoxY) then
  //  TreeEditBox.DoMouseDown(Button, Shift, BoxX, BoxY);
end;

procedure TTreeInputForm.ScrollBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
//var
  //BoxX, BoxY: Integer;
begin
  //if FEmfScrollbox.ClientToBox(X, Y, BoxX, BoxY) then
  //  TreeEditBox.DoMouseMove(Shift, X, Y);
end;

procedure TTreeInputForm.ScrollBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//var
//  BoxX, BoxY: Integer;
begin
  //if FEmfScrollbox.ClientToBox(X, Y, BoxX, BoxY) then
  //  TreeEditBox.DoMouseUp(Button, Shift, X, Y);
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

procedure TTreeInputForm.EnterResizeMode;
begin
  with TreeEditBox do
  begin
    MouseScrollResizeBtn.ImageIndex := 151;
    Cursor := crSizeNWSE;
    DragCursor := crSizeNWSE;
    EditEnabled := false;
    ShowSelection := false;
  end;
end;

procedure TTreeInputForm.QuitResizeMode;
begin
  with TreeEditBox do
  begin
    MouseScrollResizeBtn.ImageIndex := 152;
    Cursor := crDefault;
    DragCursor := crDefault;
    EditEnabled := True;
    ShowSelection := True;
    TreeEditBox.SetAttrIndex;
  end;
end;
{
procedure TTreeInputForm.Image1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if UploadTBtn.ImageIndex = 24 then
  begin
    SX := X;
    SY := Y;

   Screen.Cursor := ClosedHand;
   // Application.Processmessages;
  end;
end;

procedure TTreeInputForm.Image1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if UploadTBtn.ImageIndex = 24 then
  begin
    if not (ssLeft in Shift) then
    begin
      Screen.Cursor := OpenHand;
      Exit;
    end;
    Screen.Cursor := ClosedHand;

    updateBirdsEyeSelection(X, Y);
  end;
end;


procedure TTreeInputForm.ScrollTo(X, Y : integer);
begin
  PanPanel.Left := Round(-(PanPanel.Width * (X / (OkBtn.Width+1))));
  PanPanel.Top  := Round(-(PanPanel.Height * (Y / (OkBtn.Height+1))));
end;

procedure TTreeInputForm.MoveViewer(X, Y : Integer);
begin
        X := X - XOffset;    //To prevent viewer's top left corner from jumping to cursor
        Y := Y - YOffset;

        //Left Line
        LeftBevel.Left := X;
        LeftBevel.Top  := Y;
        //Top Line
        TopBevel.Left := X;
        TopBevel.Top  := Y;
        //Right Line
        RightBevel.Left := X + TopBevel.Width;
        RightBevel.Top := Y;
        //Bottom Line
        BottomBevel.Left := X;
        BottomBevel.Top := Y + LeftBevel.Height;
end;

function TTreeInputForm.NAWhilePanning: Boolean;
begin
  result := false;
  if UploadTBtn.ImageIndex = 24 then
  begin
    if (MessageDlg('This action is not permitted while you are in pan mode.'+LineEnding+''+LineEnding+'Would you like to exit panning mode?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      EndPanMode;
    end
    else
      result := true;
  end;
end;

procedure TTreeInputForm.StartPanMode;
var
  FLeft, FTop: Integer;
begin
  UploadTBtn.ImageIndex := 24;
  TreeEditBox.CopyToClipBoard;
  sleep(100); // Since we're using a system call program execution continues without waiting for the image to go to memory, thus if the system is busy when we try to access it next it will still be in write mode and we get an error message about not being ready.
  try
    if Clipboard.HasFormat(CF_PICTURE) then
      Image1.Picture.Assign(Clipboard);
  Except
    on E: Exception do
      MessageDlg('There was an error in the Tree Panning function, please report this to the MEGA authors. Technical Details: ' + e.Message, mtWarning, [mbOK], 0);
  end;
  FLeft := - (TreeScrollBox.HorzScrollBar.Position);
  FTop := - (TreeScrollBox.VertScrollBar.Position);
  TreeScrollBox.VertScrollBar.Visible := false;
  TreeScrollBox.HorzScrollBar.Visible := false;

  PanPanel.Width := Image1.Width;
  PanPanel.Height := Image1.Height;
  LX := (PanPanel.Width - TreeScrollBox.ClientWidth) * -1;
  LY := (PanPanel.Height - TreeScrollBox.ClientHeight) * -1;
  PanPanel.Visible := true;

  //ShowMessage('now move: ' + IntToStr(FLeft) + ' left; and ' + IntToStr(FTop) + ' down;');
  PanPanel.Left := Fleft;
  PanPanel.Top := FTop;
  Image1.Cursor := OpenHand;
  TreeEditBox.Visible := false;
  BirdsEyePanel.Visible := True;
  UpdateBirdsEyeSelection(0, 0);
end;

procedure TTreeInputForm.EndPanMode;
begin
  TreeEditBox.Visible := true;
  FocusControl(TreeEditBox); // Nick's fix for scrolling bug.
  ToolButton1.ImageIndex := 23;
  //TreeEditBox.Width := TreeEditBox.Width + (TreeEditBox.Left - PanPanel.Left);
  //TreeEditBox.Height := TreeEditBox.Height + (TreeEditBox.Top - PanPanel.Top);
  TreeEditBox.Refresh;
  TreeScrollBox.VertScrollBar.Visible := true;
  TreeScrollBox.HorzScrollBar.Visible := true;
  TreeScrollBox.HorzScrollBar.Position := - (PanPanel.Left);
  TreeScrollBox.VertScrollBar.Position := - (PanPanel.Top);
  PanPanel.left := 0;
  PanPanel.top := 0;
  Screen.Cursor := crDefault;
  PanPanel.Visible := false;
  BirdsEyePanel.Visible := False;
end; }

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

{
procedure TTreeInputForm.EnterExitPanModeExecute(Sender: TObject);
begin
  if UploadTBtn.ImageIndex = 23 then
  begin // starting pan
    StartPanMode;
  end
  else
  begin // ending pan
    EndPanMode;
  end;
end; }

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
      ShowMessage('Oh no! Failed to initialize the help browser: ' + E.Message);
  end;
end;

{procedure TTreeInputForm.Image2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (x >= LeftBevel.Left) and (x < RightBevel.Left) and (y >= TopBevel.Top) and (y < BottomBevel.Top) then
  begin
    XOffset := X - LeftBevel.Left;
    YOffset := Y - TopBevel.Top;
    FMovingViewer := True;
  end;
end;

procedure TTreeInputForm.Image2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FMovingViewer then
  begin
    if (X < 0) or (Y < 0) or (X > OkBtn.Width) or (Y > OkBtn.Height) then
    begin
      exit;
    end;
    MoveViewer(X, Y);
    ScrollTo(X-XOffset, Y-YOffset);
  end;

end;

procedure TTreeInputForm.Image2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMovingViewer := False;
end; }


procedure TTreeInputForm.UpdateDispTaxaList;
begin
  PW.Action := 'Updating Displayed taxa list';
  PW.Show;
  if ConstructSelTaxaList then
  begin
    PW.Action := 'Sorting taxa list';
    NameMapListView.AlphaSort; // Uses custom sort function (it isn't doing an alpha sort) // we need to sort the NameMapListView when we add back hidden sequences so they are in the same order as when they were removed.
  end;
  PW.Action := 'Redrawing taxa list';
  NameMapListView.Invalidate;
  PW.Hide;
end;

procedure TTreeInputForm.UpdateRefreshProgress(AProgress: Integer);
begin
   { just a stub for compatability with TTreeBox}
end;

procedure TTreeInputForm.UpdateRefreshStatus(AStatus: TTreeRebuildPhase);
begin
   { just a stub for compatability with TTreeBox}
end;

function TTreeInputForm.ConstructSelTaxaList: Boolean;
var
  i: Longint;
  foundItem: TListItem;
  aItem: TListItem;
begin
  result := false;
  if DispTaxaList = nil then
    Exit;
  try
    FIsUpdating := True;
    //NameMapListView.Canvas.Lock;
    DispTaxaList.Clear;
    { add everything from the hidden list to the NameMapListView}
    for i:=0 to HiddenList.Count-1 do
    begin
      aItem := NameMapListView.Items.Add;
      aItem.Caption := HiddenList.Names[i];
      aItem.SubItems.Add(HiddenList.ValueFromIndex[i]);
      Result := True;
    end;
    HiddenList.Clear;

    if OnlyUnmappedChk.Checked then
    begin
      { move unchecked items from NameMapListView to the HiddenList}
      i:=0;
      while i <= FActiveNames.Count-1 do
      begin
        foundItem := NameMapListView.FindCaption(-1, FActiveNames.Names[i], false, false, false);
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
    //NameMapListView.Canvas.Unlock;
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

{procedure TTreeInputForm.UpdateBirdsEyeSelection(X: Integer; Y: Integer);
var
  NX, NY: Integer;
begin
  //Left Line
  LeftBevel.Left := round((-PanPanel.Left / PanPanel.Width) * OkBtn.Width+1);
  LeftBevel.Top := round((-PanPanel.Top / PanPanel.Height) * OkBtn.Height+1);
  LeftBevel.Height := round((TreeScrollBox.Height * (OkBtn.height / PanPanel.Height)));
  //Top Line
  TopBevel.Top := LeftBevel.Top;
  TopBevel.Left := LeftBevel.Left;
  TopBevel.Width :=   round((TreeScrollBox.Width * (OkBtn.Width / PanPanel.Width)));
  //Right Line
  RightBevel.Top := LeftBevel.Top;
  RightBevel.Left := LeftBevel.Left + TopBevel.Width;
  RightBevel.Height := LeftBevel.Height;
  //Bottom Line
  BottomBevel.Top := RightBevel.top + RightBevel.Height;
  BottomBevel.Left := LeftBevel.Left;
  BottomBevel.Width := TopBevel.Width;
  NX := PanPanel.Left + X - SX;
  NY := PanPanel.Top + Y - SY;
  if (NX < 0) and (NX > LX) then
    PanPanel.Left := NX;
  if (NY < 0) and (NY > LY) then
    PanPanel.Top := NY;
end;

procedure TTreeInputForm.BirdsEyePanelResize(Sender: TObject);
begin
  updateBirdsEyeSelection(0, 0);
end; }

//procedure TTreeInputForm.AcceptFiles( var msg : TMessage );
//const
//  cnMaxFileNameLen = 255;
//var
//  i,
//  nCount     : integer;
//  acFileName : array [0..cnMaxFileNameLen] of Char;
//begin
//  // find out how many files we're accepting
//  nCount := DragQueryFile( msg.WParam,
//                           $FFFFFFFF,
//                           acFileName,
//                           cnMaxFileNameLen );
//
//  if nCount > 1 then
//  begin
//    ShowMessage('You may only drag and drop 1 tree file at a time.');
//    Exit;
//  end;
//
//  DragQueryFile( msg.WParam, 0,
//                 acFileName, cnMaxFileNameLen );
//  OpenTree(acFileName);
//
//  // let Windows know that you're done
//  DragFinish( msg.WParam );
//end;

procedure TTreeInputForm.OpenTree(aFileName: String);
begin
  PW.show;
  try    
  try
    ClearTree; // Clears the tree and removes all mapping of active names to tree names.
    TreeEditBox.Visible := false;
    SetTreeFromFile(aFileName); // Reads in the new tree and shows it in the TreeEditBox
    if not ShowAsTopologyEditor then
      InitializeActiveDataAndTreeNameMatches; // Match names which are the same
    FCurrentNewickFile := OpenDlg.FileName;
    if not ShowAsTopologyEditor then
    begin
      PW.Action := 'Setting up highlighting for unmatched taxa.';
      SetHighlight;
    end;  
    PW.Action := 'Resizing tree to fit window';
    ResizeTreeToFit;
    TreeEditBox.Visible := true;
  Except on E: Exception do
     MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
  finally
    PW.Hide;
  end;
end;

{ TTreeInputSession }

constructor TTreeInputSession.Create;
begin
  inherited;
  NameMap := TStringList.Create;
end;

procedure TTreeInputSession.Deserialize(Ser: String);
var
  Session: TStringList;
  MapList: TStringList;
begin
  Session := TStringList.Create;
  try
    Session.Delimiter := '>';
    Session.QuoteChar := '"';
    Session.DelimitedText := Ser;
    if Session.Count <> 2 then
      Raise Exception.Create('Invalid or corrupt session');
    Newick := Session.Strings[0];
    MapList := TStringList.Create;
    try
      MapList.Delimiter := '+';
      MapList.QuoteChar := '^';
      MapList.DelimitedText := Session.Strings[1];
      NameMap.Text := MapList.Text;
    finally
      MapList.Free;
    end;
  finally
    Session.Free;
  end;
end;

destructor TTreeInputSession.Destroy;
begin
  FreeAndNil(NameMap);
  inherited destroy;
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
    NameMap.Clear;
    for i := 0 to NameMapListView.Items.Count-1 do
      NameMap.Add(NameMapListView.Items[i].Caption + '=' + NameMapListView.Items[i].SubItems.Strings[0]);
  end;
end;

procedure TTreeInputSession.RestoreToForm(Form: TTreeInputForm);
var
  i: Integer;
begin
  Form.ClearTree;  // bugfix, I have to clear the tree before I restore this new one.
  Form.PW.Show;
  try
    with Form do
    begin
      if Length(Newick) > 2 then
      begin
        PW.Action := 'Loading tree from file.';
        SetTreeFromNewick(Newick);
        InitializeActiveDataAndTreeNameMatches; // Match names which are the same
        FCurrentNewickFile := OpenDlg.FileName;
        PW.Action := 'Setting up highlighting';
        SetHighlight;
        PW.Action := 'Resizing tree to fit window';
        ResizeTreeToFit;
      end;
      PW.Action := 'Mapping names';
      for i := 0 to NameMap.Count-1 do
      begin
        if NameMapListView.FindCaption(-1, NameMap.Strings[i], False, True, True) <> nil then
          NameMapListView.FindCaption(-1, NameMap.Strings[i], False, True, True).SubItems.Strings[0] := NameMap.Values[NameMap.Names[i]];
      end;
    end;
  finally
    Form.PW.Hide;
  end;
end;

function TTreeInputSession.Serialize: String;
var
  Ser: String;
  i: Integer;
begin
  Ser := '"' + Newick + '">"';
  for i := 0 to NameMap.Count-1 do
  begin
    Ser := Ser + '^' + NameMap.Names[i] + '=' + NameMap.Values[NameMap.Names[i]] + '^';
    if i < NameMap.Count-1 then
      Ser := Ser + '+';
  end;
  Ser := Ser + '"';
  Result := Ser;
end;

//procedure TTreeInputForm.RecentTreesClick(Sender: TObject;
//  const ItemTextW: String; var Action: TOvcMRUClickAction);
//var
//  Row: Integer;
//  SessionStr, NewName: AnsiString;
//  Session: TTreeInputSession;
//  Edited : Boolean;
//  ItemText: AnsiString;
//begin
//  ItemText := AnsiString(ItemTextW);
//  if RecentList.FindRow(ItemText, Row) then
//  begin
//    SessionStr := RecentList.Values[ItemText];
//    Session := TTreeInputSession.Create;
//    try
//      Session.Deserialize(SessionStr);
//      Session.RestoreToForm(Self);
//      FCurrentNewickFile := ItemText;
//    finally
//      Session.Free;
//    end;
//  end
//  else
//    MessageDlg('We were unable to locate the saved tree and associated name mapping (MEGA names to tree names).', mtError, [mbOK], 0);
//end;

procedure TTreeInputForm.TreeEditBoxSearch(Sender: TObject);
begin
  with TreeEditBox do
  begin
    if (FocusedIndex > 0) and (FocusedIndex <= NoOfOTUs) then
      TreeJumpToTreeName(OTUName[FocusedIndex], False)
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
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TTreeInputForm.TreeToOpenTreeOnShow(FileName: String);
begin
  openFileOnShow := FileName;
end;

procedure TTreeInputForm.ShowMapDropdownAt(CursorPoint: TPoint);
var
  SelectedMapListItem: TListItem;
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
  TreeEditBox.Parent := ScrollSizeContainer;
  TreeEditBox.Align := alClient;
  TreeEditBox.Visible := True;
  PanPanel.Parent := TreeEditBox;
  TreeEditBox.DoubleBuffered := True;
  TreeEditBox.HilightColor := clSkyBlue;
  TreeEditBox.Cursor := crDefault;

  TreeEditBox.BeginRefreshCallback := BeginRefresh;
  TreeEditBox.EndRefreshCallback := EndRefresh;
  TreeEditBox.ProgressCallback := UpdateRefreshProgress;
  TreeEditBox.StatusCallback := UpdateRefreshStatus;

  TreeEditBox.DragMode := dmManual;
  TreeEditBox.OnMouseDown := TreeEditBoxMouseDown;
  TreeEditBox.OnMouseUp := TreeEditBoxMouseUp;
  TreeEditBox.OnDragOver := TreeEditBoxDragOver;
  TreeEditBox.OnDragDrop := TreeEditBoxDragDrop;
  TreeEditBox.OnEndDrag := TreeEditBoxEndDrag;
  TreeEditBox.OnSearch := TreeEditBoxSearch;
  TreeEditBox.OnClick := TreeEditBoxClick;
  TreeEditBox.OnDblClick := TreeEditBoxDblClick;

  ScrollAssist.Parent := TreeEditBox;
  ScrollAssist.Visible := False;
  TreeEditBox.EditBox.Parent := ScrollSizeContainer;
  TreeEditBox.BringToFront;
  //DragAcceptFiles(TreeEditBox.Handle, True);
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
  // Don't free the session here, it has to be stored in the EditHistory listbox!  When the form closes loop through the listbox freeing then!
end;

procedure TTreeInputForm.TreeEditBoxChange(Sender: TObject);
begin
  //addTreeToHistory;  history function is disabled because I haven't finished making it or tested it at all.
end;

procedure TTreeInputForm.Timer1Timer(Sender: TObject);
begin
  if userCanceled then
  begin
    Close;
  end;
end;

function TTreeInputForm.AdvancedAutomaticMap(TreeStr: AnsiString; ActiveStrList: TStringList): Integer;
var
  bestName: Integer;
  bestDist: Integer;
  tempDist, i: Integer;
  ActiveStr: AnsiString;
  LDist : TDistance;
begin
  // This feature is my testing to see if we can improve automatic mapping of taxa from MEGA file to tree.  It is not visible to users (isDeveloper)
  bestName := -1;
  bestDist := 99999999;
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
      LDist.Free;
    end;
  end;
  result := bestName;
end;
{$ENDIF}
end.

