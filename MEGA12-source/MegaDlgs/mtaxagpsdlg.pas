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

unit MTaxaGpsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ActnList, ExtCtrls, Buttons, StdCtrls, IniPropStorage, MegaConsts, MOtuInfo,
  MD_InputSeqData, MD_InputDistData, mimageform, MegaPrivateFiles;

type

  { TTaxaGpsDlg }

  TTaxaGpsDlg = class(TForm)
    SaveGroupDefsAction: TAction;
    OkAction2: TAction;
    HelpAction2: TAction;
    ExportAction2: TAction;
    ImportAction2: TAction;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    UngroupAction2: TAction;
    DeleteAction: TAction;
    NewAction: TAction;
    ActionList2: TActionList;
    GroupTagSourceComboBox: TComboBox;
    HelpAction: TAction;
    CloseFormAction: TAction;
    EditNameAction: TAction;
    AvailableOutgroupTaxaLb: TListBox;
    FileOpenDialog: TOpenDialog;
    AddTaxaToOutgroupBtn: TSpeedButton;
    Buttons: TImageList;
    StateImages64: TImageList;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    GroupSourcePage: TTabSheet;
    StateImages: TImageList;
    RemoveTaxaFromOutgroupBtn: TSpeedButton;
    StatusBar1: TStatusBar;
    TaxaInOutgroupLb: TListBox;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Splitter2: TSplitter;
    TaxaList: TListBox;
    MoveRightAction: TAction;
    MoveLeftAction: TAction;
    ExportGroupsAction: TAction;
    ImportGroupsAction: TAction;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    NoOfGpTaxaPanel: TPanel;
    Panel7: TPanel;
    SpeedButton10: TSpeedButton;
    MoveLeftBtn: TSpeedButton;
    MoveRightBtn: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    Splitter1: TSplitter;
    GroupTreeView: TTreeView;
    UngroupAction: TAction;
    DeleteGroupAction: TAction;
    AddGroupAction: TAction;
    ActionList1: TActionList;
    BottomPanel: TPanel;
    TaxaGroupsTab: TTabSheet;
    ClustersTab: TTabSheet;
    OutgroupTaxaSheet: TTabSheet;
    ThePageControl: TPageControl;
    SmallImagesList: TImageList;
    procedure AddGroupActionExecute(Sender: TObject);
    procedure AvailableOutgroupTaxaLbDblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CloseFormActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure GroupTagSourceComboBoxChange(Sender: TObject);
    procedure DeleteGroupActionExecute(Sender: TObject);
    procedure EditNameActionExecute(Sender: TObject);
    procedure ExportGroupsActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormShow(Sender: TObject);
    procedure GroupTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure GroupTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure GroupTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure GroupTreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure GroupTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure GroupTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GroupTreeViewNodeChanged(Sender: TObject; Node: TTreeNode;
      ChangeReason: TTreeNodeChangeReason);
    procedure HelpActionExecute(Sender: TObject);
    procedure ImportGroupsActionExecute(Sender: TObject);
    procedure MoveLeftActionExecute(Sender: TObject);
    procedure MoveRightActionExecute(Sender: TObject);
    procedure AddTaxaToOutgroupBtnClick(Sender: TObject);
    procedure RemoveTaxaFromOutgroupBtnClick(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure TaxaInOutgroupLbDblClick(Sender: TObject);
    procedure UngroupActionExecute(Sender: TObject);
  private
    FJumpToOutgroup: Boolean;
    FOutgroupExists: Boolean;
    FNormalMoveHandled: Boolean;
    FOutgroupMoveHandled: Boolean;

    TempGroupName: String;
    IsInit: Boolean;
    FNoOfClusters: Integer;
    IsSettingUpClusterTab : Boolean;
    procedure UngroupTaxon(aInfo: TOtuInfo);
    function HasCheckedImage(aNode: TTreeNode): Boolean;
    function HasUncheckedImage(aNode: TTreeNode): Boolean;
    function HasPartialCheckedImage(aNode: TTreeNode): Boolean;
    function GetClusterView: Boolean;
    function GetOtuInfo: TAllOtuInfo;
    procedure SetClusterView(AValue: Boolean);
    procedure SetOtuInfo(AValue: TAllOtuInfo);

    function HasSelected(ABox: TListBox): Boolean;
    function AddOutgroupGroup: TTreeNode;
    function SelectOutgroupGroup: Boolean;
    procedure SetSelectedInTaxaList(TaxonName: String);
    procedure SetSelectedInGroupTreeView(TaxonName: String);
    function IsGroup(SomeNode: TTreeNode): Boolean;
    function IsOutgroup(aNode: TTreeNode): Boolean;
    function IsTaxon(SomeNode: TTreeNode): Boolean;
    procedure UpdateAllNodesWithModifiedData;
    procedure CreateOtuOrderList;
    procedure UpdateParents(Node: TTreeNode);
    procedure UpdateChildren(aNode: TTreeNode);
    procedure UpdateUngroupedTaxaList;
    procedure UpdateStateIndexDisplay(Node: TTreeNode);
    procedure UpdateOutgroupLists;
    procedure MoveTaxonToOutgroupList(aInfo: TOtuInfo);
  public
    ShowingModal: Boolean;
    GroupsChanged: Boolean;
    procedure AddTaxonToOutgroup(AInfo: TOtuInfo);
    procedure RemoveTaxaFromOutgroup(AInfo: TOtuInfo);
    procedure JumpToOutgroupPage;
    procedure JumpToTaxaPage;
    procedure fillGroupTreeViewFromOtuInfo(OnlyIncludeUsed: Boolean=False);
    procedure Initialize;
    procedure UpdateDlg;  // Call if OtuInfo changed outside
    function  GetAbsoluteIndex(AObj: Pointer): Integer; // to sort as per TaxaGp order
    function  GetTreeNode(AOtuInfo: TOtuInfo): TTreeNode;

    procedure UsedClusters(MyClusters: TStringList; IsIncludeOtulist: Boolean);
    property OtuInfo: TAllOtuInfo read  GetOtuInfo write SetOtuInfo;
    property NoOfClusters: Integer read FNoOfClusters write FNoOfClusters;
    property ClusterView: Boolean read  GetClusterView write SetClusterView;
  end;

var
  TaxaGpsDlg: TTaxaGpsDlg;

implementation

{$R *.lfm}

uses
  ContextHelp_HC, MEditorForm, math,
  MegaUtils, MPleaseWait, MegaVerConsts, MVS_SeqDataExplorer,
  MVS_DistDataExplorer, mhelpfiles, mhelpkeywords, mshortcutshelper;

const
  GROUP_IMG_CHECKED = 0;
  GROUP_IMG_PARTIAL = 1;
  GROUP_IMG_UNCHECKED = 2;
  TAXON_IMG_CHECKED = 3;
  TAXON_IMG_UNCHECKED = 4;
  OUTGROUP_IMG_CHECKED = 5;
  OUTGROUP_IMG_PARTIAL = 6;
  OUTGROUP_IMG_UNCHECKED = 7;
  STATE_CHECKED = 0;
  STATE_UNCHECKED = 1;
  STATE_PARTCHECKED = 2;

{ TTaxaGpsDlg }

procedure TTaxaGpsDlg.DeleteGroupActionExecute(Sender: TObject);
var
  aNode: TTreeNode;
  aInfo: TOtuInfo;
begin
  try
    GroupTreeView.Items.BeginUpdate;
    TaxaList.Items.BeginUpdate;
    with GroupTreeview do
    begin
      if(not Assigned(Selected)) or (Selected.Parent = nil) or (not IsGroup(Selected)) then
        Exit;
      if Selected.HasChildren then
      begin
        if MessageDlg('Group contains taxa, OK to Ungroup and then Delete group?', mtConfirmation, [mbYes,mbNo],0) <> mrYes then
          Exit;
      end;

      while Selected.HasChildren do
      begin
        aNode := Selected[0];
        if aNode.Data <> nil then
        begin
          aInfo := TOtuInfo(aNode.Data);
          aInfo.OutgroupMember := False;
          aInfo.GpName := EmptyStr;
        end;
        Selected[0].MoveTo(Items[0], naAddChild);// where it was; Selected.Item[0].MoveTo(Selected, naInsert);// where it was
      end;
      if SameText(Selected.Text, 'outgroup') then
        FOutgroupExists := False;
      Items.Delete(Selected);
      OtuInfo.IsDirty := True;
    end;
    UpdateUngroupedTaxaList;
    UpdateOutgroupLists;
    UpdateStateIndexDisplay(GroupTreeview.Items[0]);
  finally
    GroupTreeView.Items.EndUpdate;
    TaxaList.Items.EndUpdate;
  end;
end;

procedure TTaxaGpsDlg.EditNameActionExecute(Sender: TObject);
begin
  TempGroupName := GroupTreeview.Selected.Text;
  GroupTreeview.Selected.EditText;
end;

procedure TTaxaGpsDlg.ExportGroupsActionExecute(Sender: TObject);
var
  sl: TStringList;
  str: AnsiString;
  i: Longint;
begin
  sl := nil;
  try
    sl := TStringList.Create;
    for i:=0 to OtuInfo.NoOfOtus-1 do
      with TOtuInfo(OtuInfo[i]) do
        if Length(GpName) >0 then
        begin
          str := Name+'='+GpName;
          while pos(' ', str) > 0 do
            str[pos(' ', str)] := '_';
          sl.Add(str);
        end;
    if sl.Count > 0 then
      OpenStringList(sl, 'group names.grp', true)
    else
      ShowMessage('There are no groups containing least one taxa to export');
  finally
    if sl <> nil then
      sl.Free;
  end;
end;

procedure TTaxaGpsDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if GroupTreeview.IsEditing then
    CloseAction := caNone
  else
    CloseAction := caHide;
{  if ThhePageControl.ActivePage = ClusterView then
     UpdateCurrentClusterContents(ClusterCBx.ItemIndex);
}
  CreateOtuOrderList;
  if ShowingModal then
    ModalResult := mrOk;
end;

procedure TTaxaGpsDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  //if GroupTreeview.ItemState[0] = csUnchecked then
  //begin
  //  ShowMessage('All data cannot be unselected.  Please correct the problem.');
  //  CanClose := False;
  //end;
end;

procedure TTaxaGpsDlg.FormCreate(Sender: TObject);
begin
  GroupsChanged := False;
  UpdateShortcutsForMacOs(ActionList1);
  DoubleBuffered := True;
  FJumpToOutgroup := False;
  FOutgroupMoveHandled := False;
  FNormalMoveHandled := False;
  ShowingModal := False;
  FOutgroupExists := False;
  HelpContext := HC_Setup_Taxa_Groups_Dlg;
  IsInit := False;
  IsSettingUpClusterTab := True;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Select/Edit Taxa/Groups';
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
end;

procedure TTaxaGpsDlg.FormDestroy(Sender: TObject);
begin
  //with MemberListView do
  //  if Items.Count > 0 then
  //    Items.Clear;
end;

function TTaxaGpsDlg.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
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

procedure TTaxaGpsDlg.FormShow(Sender: TObject);
var
  scalingFactor: Double = -1;
begin
  if PixelsPerInch > DesignTimePPI then
    scalingFactor := PixelsPerInch/DesignTimePPI;
  if CompareValue(scalingFactor, 1.5, FP_CUTOFF) >= 0 then
    GroupTreeView.Images := StateImages64;
  Initialize;
  UpdateAllNodesWithModifiedData;
  if FJumpToOutgroup then
  begin
    if AvailableOutgroupTaxaLb.Count > 0 then
    begin
      AvailableOutgroupTaxaLb.Selected[0] := True;
      {$IFNDEF DARWIN}AvailableOutgroupTaxaLb.SetFocus{$ENDIF};
    end;
  end;
end;

procedure TTaxaGpsDlg.GroupTreeViewEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  if Node = GroupTreeView.Items[0] then
    AllowEdit := False
  else
    AllowEdit := True;
end;

procedure TTaxaGpsDlg.GroupTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if Node = nil then
    Exit;
  if DeleteGroupAction.Enabled then
  begin
    if Node.Count = 1 then
      NoOfGpTaxaPanel.Caption := IntToStr(Node.Count)+' taxon in the group'
    else
      NoOfGpTaxaPanel.Caption := IntToStr(Node.Count)+' taxa in the group';
  end
  else
    NoOfGpTaxaPanel.Caption := EmptyStr;
end;

procedure TTaxaGpsDlg.GroupTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  ToNode, PrevParent: TTreeNode;
  AttachMode: TNodeAttachMode;
  HT: THitTests;
  SelItemInfo: TOtuInfo;
  SelExpand, ToNodeExpand: Boolean;
begin
  if (Sender <> GroupTreeview) then
    Exit;

  if Source = TaxaList then
  begin
    ToNode := GroupTreeView.GetNodeAt(X, Y);
    if not Assigned(ToNode) then
      Exit;
   GroupTreeView.Select(ToNode);
   MoveLeftActionExecute(MoveLeftBtn);
   Exit;
  end;

  with GroupTreeview do
  begin
    if Selected = nil then
       Exit;
    HT := GetHitTestInfoAt(X,Y);
    ToNode := DropTarget; //GetNodeAt(X, Y);
    if ToNode = nil then
      Exit;

    if ToNode.AbsoluteIndex > 0 then // not dropping on the root
    begin
      if Selected = ToNode        then Exit; // don't accept drop on itself
      if Selected = ToNode.Parent then Exit; // don't allow drop into own child
      if IsGroup(Selected) then
        if ToNode.Level > 1 then
          Exit; // allow genes to be dropped only at level 0 or 1
    end;

    SelExpand := True;
    ToNodeExpand := True;
    if (HT - [htOnItem, htOnRight, htOnIcon, htOnButton, htNowhere, htOnIndent] <> HT) then
    begin
      SelItemInfo := TOtuInfo(Selected.Data);

      // standard processing
      if (htOnItem in HT) or (htOnIcon in HT) then
        AttachMode := naAddChildFirst
      else if htNowhere in HT then
        AttachMode := naAdd
      else if (htOnIndent in HT) or (htOnButton in HT) or (htOnRight in HT) then
        AttachMode := naInsert
      else
        AttachMode := naAddChildFirst;  //is this OK

      if ToNode.AbsoluteIndex = 0 then // force child for root
        AttachMode := naAddChildFirst;

      // alter processing if attributes of sel and AnItem have some relationship
      if ToNode.AbsoluteIndex <> 0 then // not the root
      begin
        if IsTaxon(ToNode) then // dropping on a taxon
          AttachMode := naInsert
        else  // Dropping taxon/gp ON a group
        begin
          if IsTaxon(Selected) and
             (not ((htOnButton in HT) or (htOnIndent in HT) or (htOnRight in HT))) then
             AttachMode := naAddChildFirst
          else
            AttachMode := naInsert;
        end;
      end;

      PrevParent := Selected.Parent;
      SelExpand   := Selected.Expanded;
      ToNodeExpand := ToNode.Expanded;

      Selected.MoveTo(ToNode, AttachMode);
      if (Selected.Level = ToNode.Level) then  // fixes bug in Delphi if exists
      begin
        if ToNode.GetNextSibling <> Selected then
          ToNode.MoveTo(Selected, AttachMode);
      end;

      if IsTaxon(Selected) and (Selected.Parent <> PrevParent) then
      begin
        if Selected.Parent.AbsoluteIndex = 0 then
          SelItemInfo.GpName := EmptyStr
        else
        begin
          SelItemInfo.GpName := Selected.Parent.Text;
          if SameText(SelItemInfo.GpName, 'outgroup') then
            AddTaxonToOutgroup(SelItemInfo)
          else
            SelItemInfo.OutgroupMember := False;
        end;
        OtuInfo.IsDirty := True;
      end;
      UpdateParents(Selected);
      if Selected.Parent <> PrevParent then
        UpdateParents(PrevParent.GetFirstChild);
    end;
    Selected.Expanded:= SelExpand;
    ToNode.Expanded := ToNodeExpand;
  end;
  UpdateUngroupedTaxaList;
  UpdateOutgroupLists;
  //GroupTreeView.UpdateStateIndexDisplay(GroupTreeView.Selected);
  GroupTreeView.Invalidate;
end;

procedure TTaxaGpsDlg.GroupTreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ANode: TTreeNode;
begin
  Accept := False;

  if (Source <> GroupTreeview) and (Source <> TaxaList) then
    Exit;
  if Source = TaxaList then
  begin
    ANode := GroupTreeView.GetNodeAt(X, Y);
    if Assigned(ANode) then
      Accept := IsGroup(ANode)
    else
      Accept := False;
    Exit;
  end;

  with GroupTreeview do
  begin
    //GetScrollRange(Handle, SB_HORZ, RMin, RMax );
    //if RMin = RMax then HOffset := 0 else HOffset := 16;
    //GetScrollRange( Handle, SB_VERT, RMin, RMax );
    //if RMin = RMax then VOffset := 0 else VOffset := 16;
    ////--------------------------------------------------------------------
    //// Near an edge means at a maximum of half a line, i-e half the pixels
    //// of the current font.                                               }
    ////--------------------------------------------------------------------
    //NbPixels := Abs( ( Font.Height ) Div 2 );
    //
    //if ( Y < NbPixels ) then
    //  Perform( WM_VSCROLL, SB_LINEUP, 0 );
    //
    //if ( Y > Height - VOffset - NbPixels ) then
    //  Perform( WM_VSCROLL, SB_LINEDOWN, 0 );
    //
    //if ( X < NbPixels ) then
    //  Perform( WM_HSCROLL, SB_PAGELEFT, 0 );
    //
    //if ( X > Width - HOffset - NbPixels ) Then
    //  Perform( WM_HSCROLL, SB_PAGERIGHT, 0 );
    //Refresh;

    // now we work on dropping
    if DropTarget = nil then
      Exit;
    if Selected.AbsoluteIndex = 0 then
      Exit; // don't allow first to move
    Accept := True;
  end
end;

procedure TTaxaGpsDlg.GroupTreeViewEdited(Sender: TObject; Node: TTreeNode; var S: string);
var
  i: Integer;
  AChild: TTreeNode;
  aStr : String;
begin
  if Node = GroupTreeView.Items[0] then
  begin
    Assert(False, 'cannot allow users to edit the root node name');
    Exit;
  end;
  aStr := String(S);
  TrimTaxaName(aStr);
  if Length(aStr) = 0 then
  begin
    ShowMessage('No name specified. Please give a unique name.');
    aStr := Node.Text;
    Exit;
  end;

  with GroupTreeview, Items do
    for i:= 0 to Count-1 do
    begin
      if (Items[i] <> Node) and (CompareStr(Items[i].Text, aStr) = 0) then
      begin
        ShowMessage('Name : '+aStr+' is not unique. Please give a unique name.');
        S := TempGroupName;
        Exit;
      end;
    end;
  if SameText(aStr, 'outgroup') then
  begin
    if IsGroup(Node) then
    begin
      if HasCheckedImage(Node) then
      begin
        Node.SelectedIndex := OUTGROUP_IMG_CHECKED;
        Node.ImageIndex := OUTGROUP_IMG_CHECKED;
      end
      else if HasUncheckedImage(Node) then
      begin
        Node.SelectedIndex := OUTGROUP_IMG_UNCHECKED;
        Node.ImageIndex := OUTGROUP_IMG_UNCHECKED;
      end
      else
      begin
        Node.SelectedIndex := OUTGROUP_IMG_PARTIAL;
        Node.ImageIndex := OUTGROUP_IMG_PARTIAL;
      end;
    end;
    FOutgroupExists := True;
  end;
  Node.Text := aStr;
  if IsTaxon(Node) then
    TOtuInfo(Node.Data).Name := aStr
  else  // it may have children
  begin
    if Node.HasChildren then
    begin
      AChild := Node.GetFirstChild;
      while AChild <> nil do
      begin
        TOtuInfo(AChild.Data).GpName := aStr;
        AChild := Node.GetNextChild(AChild);
      end;
    end;
  end;
  OtuInfo.IsDirty := True;
  UpdateUngroupedTaxaList;
end;

procedure TTaxaGpsDlg.GroupTreeViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  htest: THitTests;
  node: TTreeNode = nil;
begin
  if not Visible then
    Exit;
  node := GroupTreeView.GetNodeAt(x, y);
  if node = nil then
    Exit;
  try
    BeginFormUpdate;
    htest := GroupTreeView.GetHitTestInfoAt(x, y);
    if htOnIcon in htest then
    begin
      case node.ImageIndex of
        GROUP_IMG_CHECKED: node.ImageIndex := GROUP_IMG_UNCHECKED;
        GROUP_IMG_UNCHECKED, GROUP_IMG_PARTIAL: node.ImageIndex := GROUP_IMG_CHECKED;
        TAXON_IMG_CHECKED: node.ImageIndex := TAXON_IMG_UNCHECKED;
        TAXON_IMG_UNCHECKED: node.ImageIndex := TAXON_IMG_CHECKED;
        OUTGROUP_IMG_CHECKED: node.ImageIndex := OUTGROUP_IMG_UNCHECKED;
        OUTGROUP_IMG_PARTIAL, OUTGROUP_IMG_UNCHECKED: node.ImageIndex := OUTGROUP_IMG_CHECKED;
      end;
      node.SelectedIndex := node.ImageIndex;
      GroupTreeView.Invalidate;
    end;
    if OtuInfo <> nil then
       OtuInfo.IsDirty := True;
    if (Node.Data <> nil) and IsTaxon(Node) then
      TOtuInfo(Node.Data).IsUsed := (Node.ImageIndex = TAXON_IMG_CHECKED);
    UpdateParents(Node);
    if IsGroup(Node) then
      UpdateChildren(Node);
  finally
    EndFormUpdate;
  end;
  Application.ProcessMessages;
end;

procedure TTaxaGpsDlg.GroupTreeViewNodeChanged(Sender: TObject; Node: TTreeNode; ChangeReason: TTreeNodeChangeReason);
begin
  //if OtuInfo <> nil then
  //   OtuInfo.IsDirty := True;
  //if Node.Data <> nil then
  //  TOtuInfo(Node.Data).IsUsed := (Node.ImageIndex = TAXON_IMG_CHECKED);
end;

procedure TTaxaGpsDlg.HelpActionExecute(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
end;

procedure TTaxaGpsDlg.ImportGroupsActionExecute(Sender: TObject);
var
  sl, curGpList: TStringList;
  str, newGp: AnsiString;
  NodeToMove, newGpNode: TTreeNode;
  i, AIndex: Longint;
  AInfo: TOtuInfo;
begin
  sl := nil;
  CurGpList := nil;
  try
    FileOpenDialog.DefaultExt := 'TXT';
    FileOpenDialog.Filter := 'Text File & Group File(*.txt,*.grp)|*.txt;*.grp';
    FileOpenDialog.FileName := '';
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

    CurGpList := TStringList.Create;

//    GroupTreeView.Invalidate;
    // make a list of existing group names
    with GroupTreeView.Items[0] do
      for i:=0 to Count-1 do
      begin
        StatusBar1.SimpleText := 'Scanning '+IntToStr(i+1);
        if IsGroup(Items[i]) then
          CurGpList.AddObject(Items[i].Text, Items[i]);
      end;

    // now remove any lines with empty strings
    for i:= sl.count-1 downto 0 do
    begin
      str := Trim(sl[i]);
      if Str = EmptyStr then
        sl.Delete(i);
    end;

    if sl.count = 0 then
    begin
      sl.Free;
      CurGpList.Free;
      Exit;
    end;

    OtuInfo.IsDirty := True;

    // add groupnames whenever we can
    // retain old names
    try
      GroupTreeView.Items.BeginUpdate;
      Taxalist.Items.BeginUpdate;
      for i:= 0 to sl.count-1 do
      begin
        if length(Trim(sl[i])) = 0 then
          Continue;
        str := Trim(sl.Names[i]);
        NewGp := Trim(sl.Values[str]);
        if length(str) = 0 then
          Continue;
        TrimTaxaName(str);
        TrimTaxaName(newGp);
        if SameText(newGp, 'outgroup') and (not FOutgroupExists) then
        begin
          newGpNode := AddOutgroupGroup;
          CurGpList.AddObject(NewGp, newGpNode);
        end;
        StatusBar1.SimpleText := 'Importing '+sl[i];

        AInfo := OtuInfo.GetOtuInfoByName(str) ;
        if AInfo = nil then
          Continue;
        if SameText(newGp, 'outgroup') then
        begin
          AInfo.OutgroupMember := True;
          MoveTaxonToOutgroupList(AInfo);
        end;
        if AInfo.GpName = NewGp then
          Continue;

        NodeToMove := GetTreeNode(AInfo);
        if (length(AInfo.GpName)>0) and (Length(newGp)=0) then // Group -> UNgroup
        begin
          NodeToMove.MoveTo(GroupTreeView.Items[0], naAddChild);
          AInfo.GpName := EmptyStr;
        end
        else if (Length(newGp) > 0) then // Group change or group unclassified
        begin
          newGpNode := nil;
          AIndex := CurGpList.IndexOf(newGp);
          if AIndex >= 0 then
            newGpNode := TTreeNode(CurGpList.objects[AIndex])
          else // create the group node
          begin
            newGpNode := GroupTreeView.Items.AddChildFirst(GroupTreeView.Items[0], newGp);
            if SameText(newGp, 'outgroup') then
            begin
              newGpNode.SelectedIndex := OUTGROUP_IMG_CHECKED;
              newGpNode.ImageIndex    := OUTGROUP_IMG_CHECKED;
            end
            else
            begin
              newGpNode.SelectedIndex := GROUP_IMG_CHECKED;
              newGpNode.ImageIndex    := GROUP_IMG_CHECKED;
            end;
            CurGpList.AddObject(newGp, newGpNode);
          end;
          NodeToMove.MoveTo(newGpNode, naAddChild);
          AInfo.GpName := newGp;
        end
        else if (Length(AInfo.GpName)=0) and (Length(newGp)=0) then // for whatever reason if the above fails
          Continue
      end;
    finally
      GroupTreeView.Items.EndUpdate;
      Taxalist.Items.EndUpdate;
    end;
    StatusBar1.SimpleText := 'Updating';
    UpdateUngroupedTaxaList;
    UpdateStateIndexDisplay(GroupTreeview.Items[0]);
    StatusBar1.SimpleText := EmptyStr;
  finally
    if sl <> nil then
      sl.Free;
    if CurGpList <> nil then
      CurGpList.Free;
  end;
end;

procedure TTaxaGpsDlg.MoveLeftActionExecute(Sender: TObject);
var
  NewSelected, i: Integer;
  NodeToMove: TTreeNode;
  AInfo: TOtuInfo;
begin
  FNormalMoveHandled := False;
  with GroupTreeview do
  begin
    if not IsGroup(Selected) then
    begin
      if (Selected.Level < 2) then
        Exit;
    end;
    if GroupTreeview.Selected = GroupTreeView.Items[0] then
      Exit;
    try
      GroupTreeView.Items.BeginUpdate;
      TaxaList.Items.BeginUpdate;
      NodeToMove := nil;
      NewSelected := -1;
      for i:= 0 to TaxaList.items.Count-1 do // moves all selected taxa
      begin
        if TaxaList.Selected[i] then
        begin
          if NewSelected < 0 then
            NewSelected := i;
          NodeToMove := TTreeNode(TaxaList.Items.Objects[i]);
          AInfo := TOtuInfo(NodeToMove.Data);
          if IsGroup(Selected) then
          begin
             { Ensure that the node is not in edit mode.Otherwise the TreeView will thrown an invalid index error when a taxa is added }
             Selected.EndEdit(False);
             AInfo.GpName := Selected.Text;
             if SameText(AInfo.GpName, 'outgroup') and (not FOutgroupMoveHandled) then
               AddTaxonToOutgroup(AInfo);
             NodeToMove.MoveTo(Selected, naAddChild)
          end
          else
          begin
             AInfo.GpName := Selected.Parent.Text;
             if SameText(AInfo.GpName, 'outgroup') then
               AddTaxonToOutgroup(AInfo);
             NodeToMove.MoveTo(Selected, naInsert);
          end;
          UpdateStateIndexDisplay(NodeToMove);
        end;
      end;
      if NodeToMove <> nil then
        UpdateParents(NodeToMove); // only need to move the parent of the last node
    finally
      GroupTreeView.Items.EndUpdate;
      TaxaList.Items.EndUpdate;
    end;
  end;
  UpdateUngroupedTaxaList;
  UpdateOutgroupLists;
  UpdateStateIndexDisplay(GroupTreeView.Selected);
  if NewSelected > (TaxaList.Items.Count-1) then
    NewSelected := TaxaList.Items.Count-1;
  if NewSelected >= 0 then
    TaxaList.Selected[NewSelected] := True
end;

procedure TTaxaGpsDlg.MoveRightActionExecute(Sender: TObject);
var
  NextToFocus, ItemToMove: TTreeNode;
  AInfo: TOtuInfo;
begin
  if (not Assigned(GroupTreeView.Selected)) or (not IsTaxon(GroupTreeView.Selected)) then
    Exit;
  AInfo := TOtuInfo(GroupTreeView.Selected.Data);
  if not Assigned(AInfo) then
    Exit;
  if TaxaList.Items.IndexOf(AInfo.Name) >= 0 then
    Exit;
  try
    GroupTreeView.Items.BeginUpdate;
    TaxaList.Items.BeginUpdate;
    FNormalMoveHandled := false;
    with GroupTreeview do
    begin
      if SameText(AInfo.GpName, 'outgroup') and (not FOutgroupMoveHandled) then
        RemoveTaxaFromOutgroup(AInfo);
      AInfo.GpName := EmptyStr;
      TaxaList.Items.AddObject(TOtuInfo(Selected.Data).Name, Selected);
      // find a sibling to focus on
      NextToFocus := Selected.Parent.GetNextChild(Selected);
      if NextToFocus = nil then
        NextToFocus := Selected.Parent.GetPrevChild(Selected);
      if NextToFocus = nil then
        NextToFocus := Selected.Parent;
      ItemToMove := Selected;
      Selected := NextToFocus;
      ItemToMove.MoveTo(Items[0], naAddChild);
      UpdateStateIndexDisplay(ItemToMove);
      OtuInfo.IsDirty := True;
      UpdateParents(Selected); // only need to move the parent of the last node
      UpdateOutgroupLists;
    end;
  finally
    GroupTreeView.Items.EndUpdate;
    TaxaList.Items.EndUpdate;
  end;
end;

procedure TTaxaGpsDlg.AddTaxaToOutgroupBtnClick(Sender: TObject);
var
  i: Integer;
  AInfo: TOtuInfo;
  CurrSel: Integer;
begin
  if (AvailableOutgroupTaxaLb.Count <= 0) or (not HasSelected(AvailableOutgroupTaxaLb)) then
    Exit;

  try
    AvailableOutgroupTaxaLb.Items.BeginUpdate;
    TaxaInOutgroupLb.Items.BeginUpdate;
    TaxaList.ClearSelection;
    for i := AvailableOutgroupTaxaLb.Count - 1 downto 0 do
      if AvailableOutgroupTaxaLb.Selected[i] then
      begin
        AInfo := TOtuInfo(AvailableOutgroupTaxaLb.Items.Objects[i]);
        UngroupTaxon(AInfo);
        AInfo.OutgroupMember := True;
        OtuInfo.IsDirty := True;
        TaxaInOutgroupLb.Items.AddObject(AInfo.Name, AInfo);
        AvailableOutgroupTaxaLb.Items.Delete(i);
        SetSelectedInTaxaList(AInfo.Name);
        CurrSel := i;
      end;
      if not FOutgroupExists then
        AddOutgroupGroup;
      SelectOutgroupGroup;
      FOutgroupMoveHandled := True;
      if not FNormalMoveHandled then
        MoveLeftActionExecute(Self);
      if AvailableOutgroupTaxaLb.Count > 0 then
      begin
        if CurrSel >= AvailableOutgroupTaxaLb.Count then
          AvailableOutgroupTaxaLb.Selected[AvailableOutgroupTaxaLb.Count - 1] := True
        else
          AvailableOutgroupTaxaLb.Selected[CurrSel] := True;
      end;
      if TaxaInOutgroupLb.Count = 1 then
        TaxaInOutgroupLb.Selected[0] := True;
      FOutgroupMoveHandled := False;
  finally
    AvailableOutgroupTaxaLb.Items.EndUpdate;
    TaxaInOutgroupLb.Items.EndUpdate;
  end;
end;

procedure TTaxaGpsDlg.RemoveTaxaFromOutgroupBtnClick(Sender: TObject);
var
  i: Integer;
  AInfo: TOtuInfo;
  CurrSel: Integer;
begin
  if (TaxaInOutgroupLb.Count <= 0) or (not HasSelected(TaxaInOutgroupLb)) then
    Exit;
  if GroupTreeView.SelectionCount > 0 then
    GroupTreeView.Selected.Selected := False;
  try
    AvailableOutgroupTaxaLb.Items.BeginUpdate;
    TaxaInOutgroupLb.Items.BeginUpdate;
    FOutgroupMoveHandled := True;
    for i := TaxaInOutgroupLb.Count - 1 downto 0 do
      if TaxaInOutgroupLb.Selected[i] then
      begin
        AInfo := TOtuInfo(TaxaInOutgroupLb.Items.Objects[i]);
        AInfo.OutgroupMember := False;
        OtuInfo.IsDirty := True;
        AvailableOutgroupTaxaLb.Items.AddObject(AInfo.Name, AInfo);
        TaxaInOutgroupLb.Items.Delete(i);
        SetSelectedInGroupTreeView(AInfo.Name);
        CurrSel := i;
      end;

      if not FNormalMoveHandled then
        MoveRightActionExecute(Self);
      if TaxaInOutgroupLb.Count > 0 then
      begin
        if CurrSel >= TaxaInOutgroupLb.Count then
          TaxaInOutgroupLb.Selected[TaxaInOutgroupLb.Count - 1] := True
        else
          TaxaInOutgroupLb.Selected[CurrSel] := True;
      end;
      if AvailableOutgroupTaxaLb.Count = 1 then
        AvailableOutgroupTaxaLb.Selected[0] := True;
      FOutgroupMoveHandled := False;
  finally
    AvailableOutgroupTaxaLb.Items.EndUpdate;
    TaxaInOutgroupLb.Items.EndUpdate;
  end;
end;

procedure TTaxaGpsDlg.Splitter1Moved(Sender: TObject);
begin
  Invalidate;
end;

procedure TTaxaGpsDlg.Splitter2Moved(Sender: TObject);
begin
  Invalidate;
end;

procedure TTaxaGpsDlg.TaxaInOutgroupLbDblClick(Sender: TObject);
begin
  if TaxaInOutgroupLb.ItemIndex >= 0 then
    RemoveTaxaFromOutgroupBtnClick(Sender);
end;

procedure TTaxaGpsDlg.UngroupActionExecute(Sender: TObject);
var
  aNode: TTreeNode;
  aInfo: TOtuInfo;
begin
  if (GroupTreeView.Selected = nil) or (not IsGroup(GroupTreeView.Selected)) then
  begin
    ShowMessage('A group must be selected before performing this action');
    Exit;
  end;
  if GroupTreeView.Selected.Parent = nil then { don't let the user ungroup from the root}
    Exit;
  try
    GroupTreeView.Items.BeginUpdate;
    with GroupTreeview do
    begin
      if SameText(Selected.Text, 'outgroup') then
        FOutgroupExists := False;
      while Selected.HasChildren do
      begin
        aNode := Selected[0];
        if aNode.Data <> nil then
        begin
          aInfo := TOtuInfo(aNode.Data);
          aInfo.OutgroupMember := False;
          aInfo.GpName := EmptyStr;
        end;
        Selected[0].MoveTo(Items[0], naAddChild);
      end;
      OtuInfo.IsDirty := True;
    end;
    UpdateUngroupedTaxaList;
    UpdateOutgroupLists;
    UpdateStateIndexDisplay(GroupTreeview.Items[0]);
  finally
    GroupTreeView.Items.EndUpdate;
  end;
end;

procedure TTaxaGpsDlg.UngroupTaxon(aInfo: TOtuInfo);
var
  i: Integer;
  aNode: TTreeNode;
begin
  if (GroupTreeView.Items.Count = 0) or (not Assigned(aInfo)) then
    Exit;
  for i := 0 to GroupTreeView.Items.Count - 1 do
  begin
    aNode := GroupTreeView.Items[i];
    if TOtuInfo(aNode.Data) = aInfo then
    begin
      aNode.MoveTo(GroupTreeView.Items[0], naAddChild);
      Exit;
    end;
  end;
end;

function TTaxaGpsDlg.HasCheckedImage(aNode: TTreeNode): Boolean;
begin
  Result := ((aNode.ImageIndex = TAXON_IMG_CHECKED) or
             (aNode.ImageIndex = GROUP_IMG_CHECKED) or
             (aNode.ImageIndex = OUTGROUP_IMG_CHECKED));
end;

function TTaxaGpsDlg.HasUncheckedImage(aNode: TTreeNode): Boolean;
begin
  Result := ((aNode.ImageIndex = TAXON_IMG_UNCHECKED) or
             (aNode.ImageIndex = GROUP_IMG_UNCHECKED) or
             (aNode.ImageIndex = OUTGROUP_IMG_UNCHECKED));
end;

function TTaxaGpsDlg.HasPartialCheckedImage(aNode: TTreeNode): Boolean;
begin
  Result := ((aNode.ImageIndex = GROUP_IMG_PARTIAL) or
             (aNode.ImageIndex = OUTGROUP_IMG_PARTIAL));
end;

procedure TTaxaGpsDlg.AddGroupActionExecute(Sender: TObject);
var
  PrevSel, ANode: TTreeNode;
  NewString: AnsiString;
  TempSuffixGpInt : LongInt;
  UpdateStates: Boolean;

  function GroupNotExists(group : AnsiString):boolean;
  var
    i : integer;
  begin
    Result := True;
    for i:=0 to GroupTreeView.Items.Count-1 do
      if GroupTreeView.Items[i].Text = group then
      begin
        Result := False;
        exit;
      end;
  end;
begin
  UpdateStates := True;
  ActiveControl := GroupTreeview;
  TempSuffixGpInt := 0;
  repeat
    TempSuffixGpInt := TempSuffixGpInt + 1;
    NewString :=  'New Group '+IntToStr(TempSuffixGpInt);
  until  GroupNotExists(NewString);
  with GroupTreeview, Items do
  begin
    PrevSel := Selected;
    if not Assigned(PrevSel) then
      GroupTreeView.Selected := Items[0];
    if (Selected.AbsoluteIndex = 0) then
    begin
      ANode := AddChildFirst(Items[0], NewString);
      UpdateStates := False; { no need to update states as this }
    end
    else if IsGroup(Selected) then
    begin
      ANode := Insert(Selected, NewString);
      if Selected.GetNextSibling <> ANode then
        Selected.MoveTo(ANode, naInsert);
    end
    else if IsTaxon(Selected) then
    begin
      if Selected.Parent.AbsoluteIndex = 0 then
      begin
        ANode := Insert(Selected, NewString);
        if Selected.GetNextSibling <> ANode then
          Selected.MoveTo(ANode, naInsert);
      end
      else
        ANode := Insert(Selected.Parent, NewString);
    end
    else
     ANOde := nil;

    if ANode <> nil then
    begin
      TempGroupName := ANode.Text;
      ANode.SelectedIndex := GROUP_IMG_CHECKED;
      ANode.ImageIndex    := GROUP_IMG_CHECKED;
      Items[0].Expanded := True;
      Selected := ANode;
      Selected.EditText;
    end
    else
      raise Exception.Create('Unexpected error');
  end;
  OtuInfo.IsDirty := True;
  if UpdateStates then
    UpdateStateIndexDisplay(PrevSel);
end;

procedure TTaxaGpsDlg.AvailableOutgroupTaxaLbDblClick(Sender: TObject);
begin
  if AvailableOutgroupTaxaLb.ItemIndex >= 0 then
    AddTaxaToOutgroupBtnClick(Sender);
end;

procedure TTaxaGpsDlg.Button1Click(Sender: TObject);
var
  infos: TAllOtuInfo = nil;
  index: Integer;
begin
  index := GroupTagSourceComboBox.ItemIndex;
  infos := GetOtuInfo;
  case index of
    RSV_TAG_INDEX: infos.SetGroupSource(gtiRsv);
    SPECIES_TAG_INDEX: infos.SetGroupSource(gtiSpecies);
    POPULATION_TAG_INDEX: infos.SetGroupSource(gtiPopulation);
    CONTINENT_TAG_INDEX: infos.SetGroupSource(gtiContinent);
    COUNTRY_TAG_INDEX: infos.SetGroupSource(gtiCountry);
    CITY_TAG_INDEX: infos.SetGroupSource(gtiCity);
    YEAR_TAG_INDEX: infos.SetGroupSource(gtiYear);
    MONTH_TAG_INDEX: infos.SetGroupSource(gtiMonth);
    DAY_TAG_INDEX: infos.SetGroupSource(gtiDay);
    TIME_TAG_INDEX: infos.SetGroupSource(gtiTime);
    NONE_TAG_INDEX: infos.SetGroupSource(gtiNone);
  end;
  CloseFormActionExecute(Sender);
end;

procedure TTaxaGpsDlg.CloseFormActionExecute(Sender: TObject);
var
  i: Integer;
begin
  if GroupTreeview.IsEditing then
    Exit;

  if ShowingModal then
  begin
    if TaxaInOutgroupLb.Count > 0 then
    begin
      for i := 0 to TaxaInOutgroupLb.Count - 1 do
      begin
        TOtuInfo(TaxaInOutgroupLb.Items.Objects[i]).OutgroupMember := True;
        TOtuInfo(TaxaInOutgroupLb.Items.Objects[i]).GpName := 'outgroup';
      end;
    end;
    if AvailableOutgroupTaxaLb.Count > 0 then
    begin
      for i := 0 to AvailableOutgroupTaxaLb.Count - 1 do
      begin
        TOtuInfo(AvailableOutgroupTaxaLb.Items.Objects[i]).OutgroupMember := False;
        if TOtuInfo(AvailableOutgroupTaxaLb.Items.Objects[i]).GpName = 'outgroup' then
          TOtuInfo(AvailableOutgroupTaxaLb.Items.Objects[i]).GpName := EmptyStr;
      end;
    end;
    ModalResult := mrOk;
  end;
  Close;
end;

procedure TTaxaGpsDlg.FormActivate(Sender: TObject);
begin
  Toolbar1.Images := ImageForm.GetDialogButton2ImageList;
  Toolbar1.HotImages := ImageForm.GetDailogButton2HoverImageList;
  Toolbar1.ImagesWidth := Toolbar1.ButtonWidth;
  if BottomPanel.Width > Toolbar1.Width then
    Toolbar1.Left := Round((BottomPanel.Width - Toolbar1.Width)/2);
  Constraints.MinWidth := Toolbar1.Width + 20;

  ToolBar2.Images := ImageForm.GetDialogButton2ImageList;
  ToolBar2.HotImages := ImageForm.GetDailogButton2HoverImageList;
  ToolBar2.ImagesWidth := ToolBar2.ButtonWidth;
  if Panel8.Width > ToolBar2.Width then
    ToolBar2.Left := Round((Panel8.Width - ToolBar2.Width)/2);

  Toolbar3.Images := ImageForm.GetDialogButton2ImageList;
  Toolbar3.HotImages := ImageForm.GetDailogButton2HoverImageList;
  Toolbar3.ImagesWidth := Toolbar3.ButtonWidth;
end;

procedure TTaxaGpsDlg.GroupTagSourceComboBoxChange(Sender: TObject);
begin
  GroupsChanged := True;
end;

function TTaxaGpsDlg.GetClusterView: Boolean;
begin
  Result := (ThePageControl.ActivePage = ClustersTab);
end;

function TTaxaGpsDlg.GetOtuInfo: TAllOtuInfo;
begin
  if D_InputSeqData <> nil then
    Result := D_InputSeqData.OtuInfos
  else if D_InputDistData <> nil then
    Result := D_InputDistData.OtuInfos;
end;

procedure TTaxaGpsDlg.SetClusterView(AValue: Boolean);
//var
// i: Integer;
// AListItem: TListItem;
// PleaseWait : TPleaseWait;
begin
  //ClustersTab.TabVisible := Value;
  //TaxaGroupsTab.TabVisible := not Value;
  //
  //if ClustersTab.TabVisible then
  //begin
  //  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Define Clusters';
  //end
  //else
  //begin
  //  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Select/Edit Taxa Groups';
  //  HelpContext := HC_Setup_Taxa_Groups_Dlg;
  //end;
  //
  //if Value then
  //  ThePageControl.ActivePage := ClustersTab
  //else
  //  ThePageControl.ActivePage := TaxaGroupsTab;
  //
  //if not ClustersTab.TabVisible then
  //  Exit;
  //
  //ClusterDTab.TabVisible := FNoOfClusters > 3;
  //
  //PleaseWait := TPleaseWait.Create(Self);
  //PleaseWait.Show;
  //IsSettingUpClusterTab := True;
  //if MemberListView.Items.Count = 0 then
  //begin
  //  with GroupTreeView do
  //  begin
  //    for i:=1 to Items.Count-1 do
  //      if not IsGroup(Items[i]) then
  //      begin
  //        AListItem := MemberListView.Items.Add;
  //        AListItem.Data := Items[i].Data;
  //        with TOtuInfo(AListItem.Data) do
  //        begin
  //          AListItem.Caption  := Name;
  //          AListItem.SubItems.Add(EmptyStr);
  //          AListItem.SubItems.Add(GpName);
  //        end;
  //      end;
  //  end;
  //end
  //else  // we just update things that may have changed
  //begin
  //  if not ClusterDTab.TabVisible then // cluster D contents are lost
  //    ClusterDLBx.Clear;
  //
  //  for i:=0 to MemberListView.Items.Count-1 do
  //  begin
  //    AListItem := MemberListView.Items[i];
  //    AListItem.Caption     := TOtuInfo(AListItem.Data).Name;
  //    AListItem.SubItems[1] := TOtuInfo(AListItem.Data).GpName;
  //    if not ClusterDTab.TabVisible then
  //    begin
  //      if length(AListItem.SubItems[0]) > 0 then
  //      begin
  //        if AListItem.SubItems[0][1] = 'D' then
  //            AListItem.SubItems[0] := EmptyStr;  // remove it
  //      end;
  //    end;
  //  end;
  //
  //  with ClusterALBx do
  //    for i:=0 to Items.Count-1 do
  //      with TOtuInfo(Items.Objects[i]) do
  //        if Length(GpName) > 0 then Items[i] := Name+' {'+GpName+')'
  //        else                       Items[i] := Name;
  //
  //  with ClusterBLBx do
  //    for i:=0 to Items.Count-1 do
  //      with TOtuInfo(Items.Objects[i]) do
  //        if Length(GpName) > 0 then Items[i] := Name+' {'+GpName+')'
  //        else                       Items[i] := Name;
  //
  //  with ClusterCLBx do
  //    for i:=0 to Items.Count-1 do
  //      with TOtuInfo(Items.Objects[i]) do
  //        if Length(GpName) > 0 then Items[i] := Name+' {'+GpName+')'
  //        else                       Items[i] := Name;
  //
  //  with ClusterDLBx do
  //    for i:=0 to Items.Count-1 do
  //      with TOtuInfo(Items.Objects[i]) do
  //        if Length(GpName) > 0 then Items[i] := Name+' {'+GpName+')'
  //        else                       Items[i] := Name;
  //end;
  //IsSettingUpClusterTab := False;
  //FreeAndNil(PleaseWait);
end;

procedure TTaxaGpsDlg.SetOtuInfo(AValue: TAllOtuInfo);
begin
  ShowMessage('Not finished');
end;

function TTaxaGpsDlg.HasSelected(ABox: TListBox): Boolean;
var
  i: Integer;
begin
  Result := False;
  if ABox.Count > 0 then
    for i := 0 to ABox.Count - 1 do
      if ABox.Selected[i] then
      begin
        Result := True;
        break;
      end;
end;

function TTaxaGpsDlg.AddOutgroupGroup: TTreeNode;
begin
  Result := GroupTreeView.Items.AddChildFirst(GroupTreeView.Items[0], 'outgroup');
  Result.SelectedIndex := OUTGROUP_IMG_CHECKED;
  Result.ImageIndex    := OUTGROUP_IMG_CHECKED;
  GroupTreeView.Select(Result);
  FOutgroupExists := True;
end;

function TTaxaGpsDlg.SelectOutgroupGroup: Boolean;
var
  ANode: TTreeNode;
begin
  Result := False;
  ANode := GroupTreeView.Items.GetFirstNode.getFirstChild;
  while ANode <> nil do
  begin
    if SameText(ANode.Text, 'outgroup') then
    begin
      Result := True;
      GroupTreeView.ClearSelection;
      ANode.Selected := True;
      Exit;
    end;
    ANode := ANode.getNextSibling;
  end;
end;

procedure TTaxaGpsDlg.SetSelectedInTaxaList(TaxonName: String);
var
  i: Integer;
  AInfo: TOtuInfo;
  NodeToMove: TTreeNode;
begin
  Assert(TaxaList.Count > 0);
  for i := 0 to TaxaList.Count - 1 do
  begin
    NodeToMove := TTreeNode(TaxaList.Items.Objects[i]);
    AInfo := TOtuInfo(NodeToMove.Data);
    if AInfo.Name = TaxonName then
    begin
      TaxaList.Selected[i] := True;
      Exit;
    end;
  end;
end;

procedure TTaxaGpsDlg.SetSelectedInGroupTreeView(TaxonName: String);
var
  AInfo: TOtuInfo;
  ANode: TTreeNode;

  function ProcessNode(BNode: TTreeNode): Boolean;
  var
    CNode: TTreeNode;
  begin
    Result := False;
    CNode := BNode.getFirstChild;
    while CNode <> nil do
    begin
      AInfo := TOtuInfo(CNode.Data);
      if AInfo.Name = TaxonName then
      begin
        Result := True;
        CNode.Selected := True;
        Exit;
      end;
      CNode := CNode.getNextSibling;
    end;
  end;

begin
  Assert(GroupTreeView.Items.Count > 0);

  ANode := GroupTreeView.Items[0];
  ANode := ANode.getFirstChild;
  while ANode <> nil do
  begin
    if IsGroup(ANode) then
    begin
      if ProcessNode(ANode) then
        break;
    end
    else
    begin
      if Assigned(ANode.Data) then
      begin
        AInfo := TOtuInfo(ANode.Data);
        if AInfo.Name = TaxonName then
        begin
          ANode.Selected := True;
          break;
        end;
      end;
    end;
    ANode := ANode.getNextSibling;
  end;
end;

function TTaxaGpsDlg.IsGroup(SomeNode: TTreeNode): Boolean;
begin
  Result := False;
  if not Assigned(SomeNode) then
    Exit;
  Result := ((SomeNode.ImageIndex = GROUP_IMG_CHECKED) or
             (SomeNode.ImageIndex = GROUP_IMG_UNCHECKED) or
             (SomeNode.ImageIndex = GROUP_IMG_PARTIAL) or
             (SomeNode.ImageIndex = OUTGROUP_IMG_CHECKED) or
             (SomeNode.ImageIndex = OUTGROUP_IMG_UNCHECKED) or
             (SomeNode.ImageIndex = OUTGROUP_IMG_PARTIAL) or
             (SomeNode.Text = 'All'));
end;

function TTaxaGpsDlg.IsOutgroup(aNode: TTreeNode): Boolean;
begin
  Result := (IsGroup(aNode) and SameText(aNode.Text, 'outgroup'));
end;

function TTaxaGpsDlg.IsTaxon(SomeNode: TTreeNode): Boolean;
begin
  Result := False;
  if not Assigned(SomeNode) then
    Exit;
  Result := ((SomeNode.ImageIndex <> GROUP_IMG_CHECKED) and
             (SomeNode.ImageIndex <> GROUP_IMG_UNCHECKED) and
             (SomeNode.ImageIndex <> GROUP_IMG_PARTIAL) and
             (SomeNode.ImageIndex <> OUTGROUP_IMG_CHECKED) and
             (SomeNode.ImageIndex <> OUTGROUP_IMG_UNCHECKED) and
             (SomeNode.ImageIndex <> OUTGROUP_IMG_PARTIAL) and
             (SomeNode.Text <> 'All'));
end;

procedure TTaxaGpsDlg.UpdateAllNodesWithModifiedData;
var
  i, k: LongInt;
  OtuModified, Valid : Boolean;
begin
  // first fix OTU name text if it has changed by change and StateIndex
  with GroupTreeview, Items do
    for i:= 1 to Count-1 do
      with TOtuInfo(Items[i].Data) do
      begin
        OtuModified := False;
        valid := False;
        for k:=0 to OtuInfo.NoOfOtus-1 do
        begin
          if TOtuInfo(items[i].data) = OtuInfo[k] then
          begin
            valid := True;
            break;
          end;
        end;
        if Valid then
        begin
          if Item[i].Text <> Name then
          begin
            Item[i].Text := Name;
            OtuModified := True;
          end;
          if ((IsUsed and (Items[i].ImageIndex = TAXON_IMG_UNCHECKED)) or (not IsUsed and (Items[i].ImageIndex = TAXON_IMG_CHECKED)))  then
          begin
            if IsUsed then
              Items[i].ImageIndex := TAXON_IMG_CHECKED
            else
              Items[i].ImageIndex := TAXON_IMG_UNCHECKED;
            OtuModified := True;
            Items[i].SelectedIndex := Items[i].ImageIndex;
          end;
          if OtuModified then
            UpdateParents(Item[i]); // this is a node
        end;
      end;
end;

procedure TTaxaGpsDlg.CreateOtuOrderList;
var
  i : integer;
begin
  if VS_SeqDataExplorer <> nil then
    begin
      with GroupTreeView do
      for i:=0 to Items.Count-1 do
        begin
          if Items[i].data <> nil then
            VS_SeqDataExplorer.TaxaGpsDlgOrder[TOtuInfo(Items[i].Data).Id] :=  Items[i].AbsoluteIndex;
        end;
    end
    else if VS_DistDataExplorer <> nil then
    begin
      with GroupTreeView do
      for i:=0 to Items.Count-1 do
        begin
          if Items[i].data <> nil then
            VS_DistDataExplorer.TaxaGpsDlgOrder[TOtuInfo(Items[i].Data).Id] :=  Items[i].AbsoluteIndex;
        end;
    end
    else
      Raise(Exception.Create('Error: Neither the Sequence Viewer nor Distance Input Viewer are allocated'));
end;

procedure TTaxaGpsDlg.UpdateParents(Node: TTreeNode);
var
  CheckedCount, UnCheckedCount, NewState: Integer;
begin
  NewState := STATE_UNCHECKED;

  while (Node <> nil) and (Node.Parent <> nil) do
  begin
    Node := Node.Parent.GetFirstChild;
    CheckedCount := 0;
    UnCheckedCount := 0;
    while True do
    begin
      if HasCheckedImage(Node) then
        inc(CheckedCount);
      if HasUncheckedImage(Node) then
        inc(UncheckedCount);
      if (Node.ImageIndex = GROUP_IMG_PARTIAL) or
         (Node.ImageIndex = OUTGROUP_IMG_PARTIAL) or
         ((CheckedCount > 0) and (UnCheckedCount > 0)) then
      begin
        NewState := STATE_PARTCHECKED;
        Break;
      end;
      if Node.GetNextSibling = nil then
      begin
        if CheckedCount > 0 then
          NewState := STATE_CHECKED
        else
          NewState := STATE_UNCHECKED;
        Break;
      end
      else
        Node := Node.GetNextSibling;
    end;
    Node := Node.Parent;
    if Node <> nil then
    begin
      if IsGroup(Node) then
      begin
        if IsOutgroup(Node) then
        begin
          case NewState of
            STATE_CHECKED: Node.ImageIndex := OUTGROUP_IMG_CHECKED;
            STATE_UNCHECKED: Node.ImageIndex := OUTGROUP_IMG_UNCHECKED;
            STATE_PARTCHECKED: Node.ImageIndex := OUTGROUP_IMG_PARTIAL;
          end;
        end
        else
        begin
          case NewState of
            STATE_CHECKED: Node.ImageIndex := GROUP_IMG_CHECKED;
            STATE_UNCHECKED: Node.ImageIndex := GROUP_IMG_UNCHECKED;
            STATE_PARTCHECKED: Node.ImageIndex := GROUP_IMG_PARTIAL;
          end;
        end;
      end
      else
        case NewState of
          STATE_CHECKED: Node.ImageIndex := TAXON_IMG_CHECKED;
          STATE_UNCHECKED: Node.ImageIndex := TAXON_IMG_UNCHECKED;
          else
            raise Exception.Create('invalid taxon node state');
        end;
      Node.SelectedIndex := Node.ImageIndex;
    end;
  end;
end;

procedure TTaxaGpsDlg.UpdateChildren(aNode: TTreeNode);
var
  taxonImgIndex: Integer;
  grpImgIndex: Integer;
  outgrpImgIndex: Integer;
  i: Integer;
  aChild: TTreeNode;
begin
  if (not Assigned(aNode)) or (not IsGroup(aNode)) or (aNode.ImageIndex = GROUP_IMG_PARTIAL) or (aNode.ImageIndex = OUTGROUP_IMG_PARTIAL) then
    Exit;
  if aNode.HasChildren then
  begin
    case aNode.ImageIndex of
      GROUP_IMG_CHECKED, OUTGROUP_IMG_CHECKED:
        begin
          taxonImgIndex := TAXON_IMG_CHECKED;
          grpImgIndex := GROUP_IMG_CHECKED;
          outgrpImgIndex := OUTGROUP_IMG_CHECKED;
        end;
      GROUP_IMG_UNCHECKED, OUTGROUP_IMG_UNCHECKED:
        begin
          taxonImgIndex := TAXON_IMG_UNCHECKED;
          grpImgIndex := GROUP_IMG_UNCHECKED;
          outgrpImgIndex := OUTGROUP_IMG_UNCHECKED;
        end;
    end;
    for i := 0 to aNode.Count - 1 do
    begin
      aChild := aNode.Items[i];
      if IsTaxon(aChild) then
      begin
        aChild.ImageIndex := taxonImgIndex;
        aChild.SelectedIndex := taxonImgIndex;
      end
      else if IsOutgroup(aChild) then
      begin
        aChild.ImageIndex := outgrpImgIndex;
        aChild.SelectedIndex := outgrpImgIndex;
      end
      else
      begin
        aChild.ImageIndex := grpImgIndex;
        aChild.SelectedIndex := grpImgIndex;
      end;

      if assigned(aChild.Data) then
      begin
        case aChild.ImageIndex of
          TAXON_IMG_CHECKED: TOtuInfo(aChild.Data).IsUsed := True;
          TAXON_IMG_UNCHECKED: TOtuInfo(aChild.Data).IsUsed := False;
        end;
      end;
      if aChild.HasChildren then
        UpdateChildren(aChild);
    end;
  end;
end;

procedure TTaxaGpsDlg.UpdateUngroupedTaxaList;
var
  AChild: TTreeNode;
begin
  try
    TaxaList.Items.BeginUpdate;
    TaxaList.Clear;
    AChild := GroupTreeview.Items[0].GetFirstChild;
    while AChild <> nil do
    begin
      if IsTaxon(AChild) then
        TaxaList.Items.AddObject(TOtuInfo(AChild.Data).Name, AChild);
      AChild := GroupTreeview.Items[0].GetNextChild(AChild);
    end;
  finally
    TaxaList.Items.EndUpdate;
  end;
end;

procedure TTaxaGpsDlg.UpdateStateIndexDisplay(Node: TTreeNode);
//var
//  I: Integer;
  //Item: TTVItem;
  //Value: Integer;
begin
  GroupTreeView.Invalidate;
  { TODO 1 -oglen -ctaxagps : handle TTVItem }
  //Value := Node.ImageIndex;
  //if Value >= 0 then
  //  Dec(Value);
  //with Item do
  //begin
  //  mask := TVIF_STATE or TVIF_HANDLE;
  //  stateMask := TVIS_STATEIMAGEMASK;
  //  hItem := Node.ItemId;
  //  state := IndexToStateImageMask(Value + 1);
  //end;
  //TreeView_SetItem(Node.TreeView.Handle, Item);
  //
  //try
  //  GroupTreeView.Items.BeginUpdate;
  //  for i := 0 to Node.Count - 1 do
  //  begin
  //    if Node.Item[i].HasChildren then
  //      UpdateStateIndexDisplay(Node.Item[i])
  //    else
  //    begin
  //      Value := Node.Item[i].StateIndex;
  //      if Value >= 0 then
  //        Dec(Value);
  //      with Item do
  //      begin
  //        mask := TVIF_STATE or TVIF_HANDLE;
  //        stateMask := TVIS_STATEIMAGEMASK;
  //        hItem := Node.Item[i].ItemId;
  //        state := IndexToStateImageMask(Value + 1);
  //      end;
  //      TreeView_SetItem(GroupTreeView.Handle, Item);
  //    end;
  //  end;
  //finally
  //  GroupTreeView.Items.EndUpdate;
  //end;
  //Value := Node.StateIndex;
  //if Value >= 0 then
  //  Dec(Value);
  //with Item do
  //begin
  //  mask := TVIF_STATE or TVIF_HANDLE;
  //  stateMask := TVIS_STATEIMAGEMASK;
  //  hItem := Node.ItemId;
  //  state := IndexToStateImageMask(Value + 1);
  //end;
  //TreeView_SetItem(Node.TreeView.Handle, Item);
  //
  //try
  //  GroupTreeView.Items.BeginUpdate;
  //  for i := 0 to Node.Count - 1 do
  //  begin
  //    if Node.Item[i].HasChildren then
  //      UpdateStateIndexDisplay(Node.Item[i])
  //    else
  //    begin
  //      Value := Node.Item[i].StateIndex;
  //      if Value >= 0 then
  //        Dec(Value);
  //      with Item do
  //      begin
  //        mask := TVIF_STATE or TVIF_HANDLE;
  //        stateMask := TVIS_STATEIMAGEMASK;
  //        hItem := Node.Item[i].ItemId;
  //        state := IndexToStateImageMask(Value + 1);
  //      end;
  //      TreeView_SetItem(GroupTreeView.Handle, Item);
  //    end;
  //  end;
  //finally
  //  GroupTreeView.Items.EndUpdate;
  //end;
end;

procedure TTaxaGpsDlg.UpdateOutgroupLists;
var
  i: Integer;
begin
  try
    TaxaInOutgroupLb.Items.BeginUpdate;
    AvailableOutgroupTaxaLb.Items.BeginUpdate;
    TaxaInOutgroupLb.Clear;
    AvailableOutgroupTaxaLb.Clear;
    for i := 0 to OtuInfo.NoOfOtus - 1 do
    begin
      if not OtuInfo[i].IsUsed then { unused taxa cannot be in the outgroup}
        continue;
      if OtuInfo[i].OutgroupMember then
      begin
        TaxaInOutgroupLb.Items.AddObject(OtuInfo[i].Name, OtuInfo[i]);
      end
      else
      begin
        if Trim(OtuInfo[i].GpName) = EmptyStr then
          AvailableOutgroupTaxaLb.Items.AddObject(OtuInfo[i].Name, OtuInfo[i]);
      end;
    end;
  finally
    TaxaInOutgroupLb.Items.EndUpdate;
    AvailableOutgroupTaxaLb.Items.EndUpdate;
  end;
end;

procedure TTaxaGpsDlg.MoveTaxonToOutgroupList(aInfo: TOtuInfo);
var
  i: Integer;
begin
  if AvailableOutgroupTaxaLb.Count <= 0 then
    Exit;

  try
    AvailableOutgroupTaxaLb.Items.BeginUpdate;
    TaxaInOutgroupLb.Items.BeginUpdate;
    for i := AvailableOutgroupTaxaLb.Count - 1 downto 0 do
      if TOtuInfo(AvailableOutgroupTaxaLb.Items.Objects[i]) = AInfo then
      begin
        AInfo.OutgroupMember := True;
        OtuInfo.IsDirty := True;
        TaxaInOutgroupLb.Items.AddObject(AInfo.Name, AInfo);
        AvailableOutgroupTaxaLb.Items.Delete(i);
      end;
      if not FOutgroupExists then
        AddOutgroupGroup;
      if TaxaInOutgroupLb.Count = 1 then
        TaxaInOutgroupLb.Selected[0] := True;
  finally
    AvailableOutgroupTaxaLb.Items.EndUpdate;
    TaxaInOutgroupLb.Items.EndUpdate;
  end;
end;

procedure TTaxaGpsDlg.AddTaxonToOutgroup(AInfo: TOtuInfo);
var
  BInfo: TOtuInfo;
  i: Integer;
begin

  if AvailableOutgroupTaxaLb.Count = 0 then
    Exit;
  Assert(AvailableOutgroupTaxaLb.Count > 0);
  try
    AvailableOutgroupTaxaLb.Items.BeginUpdate;
    for i := 0 to AvailableOutgroupTaxaLb.Count - 1 do
    begin
      BInfo := TOtuInfo(AvailableOutgroupTaxaLb.Items.Objects[i]);
      if BInfo.Name = AInfo.Name then
        AvailableOutgroupTaxaLb.Selected[i] := True
      else
        AvailableOutgroupTaxaLb.Selected[i] := False;
    end;
    FNormalMoveHandled := True;
    AddTaxaToOutgroupBtnClick(Self);
    FNormalMoveHandled := False;
  finally
    AvailableOutgroupTaxaLb.Items.EndUpdate;
  end;
end;

procedure TTaxaGpsDlg.RemoveTaxaFromOutgroup(AInfo: TOtuInfo);
var
  i: Integer;
  BInfo: TOtuInfo;
begin
  if TaxaInOutgroupLb.Count = 0 then
    Exit;
  try
    TaxaInOutgroupLb.Items.BeginUpdate;
    for i := 0 to TaxaInOutgroupLb.Count - 1 do
    begin
      BInfo := TOtuInfo(TaxaInOutgroupLb.Items.Objects[i]);
      if AInfo.Name = BInfo.Name then
        TaxaInOutgroupLb.Selected[i] := True
      else
        TaxaInOutgroupLb.Selected[i] := False;
    end;
    FNormalMoveHandled := True;
    RemoveTaxaFromOutgroupBtnClick(Self);
    FNormalMoveHandled := False;
  finally
    TaxaInOutgroupLb.Items.EndUpdate;
  end;
end;

procedure TTaxaGpsDlg.JumpToOutgroupPage;
begin
  ThePageControl.ActivePage := OutgroupTaxaSheet;
  FJumpToOutgroup := True; { so we can set focus on the AvailableTaxaLb in the onshow procedure}
end;

procedure TTaxaGpsDlg.JumpToTaxaPage;
begin
  ThePageControl.ActivePage := TaxaGroupsTab;
  FJumpToOutgroup := False;
end;

procedure TTaxaGpsDlg.fillGroupTreeViewFromOtuInfo(OnlyIncludeUsed: Boolean=False);
var
  i, j, imgIndex: Integer;
  ANode: TTreeNode;
begin
  FOutgroupExists := False;
  if Assigned(GroupTreeView.Items[0]) then
    GroupTreeView.Items[0].DeleteChildren;
  TaxaList.Clear;

  try
    GroupTreeView.Items.BeginUpdate;
    with GroupTreeview, Items do
      for i := 0 to OtuInfo.NoOfOtus-1 do
      begin
        if (not OtuInfo[i].IsUsed) and (OnlyIncludeUsed) then
          continue;
        if Length(OtuInfo[i].GpName) > 0 then
        begin
          ANode := nil;
          for j := 0 to Items[0].Count-1 do
            if IsGroup(Items[0][j]) then
              if CompareStr(Items[0][j].Text, OtuInfo[i].GpName) = 0 then
              begin
                ANode := Items[0][j];
                break;
              end;
          if ANode = nil then // i.e., new group, then add it
          begin
            ANode := AddChildFirst(Items[0], OtuInfo[i].GpName);
            if SameText(OtuInfo[i].GpName, 'outgroup') then
            begin
              if not FOutgroupExists then
                FOutgroupExists := True;
              ANode.SelectedIndex := OUTGROUP_IMG_UNCHECKED;
              ANode.ImageIndex := OUTGROUP_IMG_UNCHECKED;
            end
            else
            begin
              ANode.SelectedIndex := GROUP_IMG_UNCHECKED;
              ANode.ImageIndex    := GROUP_IMG_UNCHECKED;
            end;
          end; // otherwise just move on
        end
        else
          ANode := Items[0]; // add on the starting stuff
        ANode := AddChildObject(ANode, OtuInfo[i].Name, OtuInfo[i]); // this contains original info
        if OtuInfo[i].IsUsed then
        begin
          ANode.ImageIndex := TAXON_IMG_CHECKED;
          ANode.SelectedIndex := TAXON_IMG_CHECKED;
        end
        else
        begin
          ANode.ImageIndex := TAXON_IMG_UNCHECKED;
          ANode.SelectedIndex := TAXON_IMG_UNCHECKED;
        end;

        if OtuInfo[i].IsUsed then
        begin
          ANode.ImageIndex := TAXON_IMG_CHECKED;
          aNode.SelectedIndex := aNode.ImageIndex;
          if (aNode.Parent.ImageIndex = GROUP_IMG_UNCHECKED) or (aNode.Parent.ImageIndex = OUTGROUP_IMG_UNCHECKED) then
          begin
            if aNode.Parent.Count > 1 then
            begin
              if aNode.Parent.ImageIndex = GROUP_IMG_UNCHECKED then
                imgIndex := GROUP_IMG_PARTIAL
              else
                imgIndex := OUTGROUP_IMG_PARTIAL;
            end
            else
            begin
              if aNode.Parent.ImageIndex = GROUP_IMG_UNCHECKED then
                imgIndex := GROUP_IMG_CHECKED
              else
                imgIndex := OUTGROUP_IMG_CHECKED;
            end;
            aNode.Parent.ImageIndex := imgIndex;
            aNode.Parent.SelectedIndex := imgIndex;
          end;
        end
        else
        begin
          ANode.ImageIndex := TAXON_IMG_UNCHECKED;
          ANode.SelectedIndex := ANode.ImageIndex;
          if (aNode.Parent.ImageIndex = GROUP_IMG_CHECKED) or (aNode.Parent.ImageIndex = OUTGROUP_IMG_CHECKED) then
          begin
            if aNode.Parent.ImageIndex = GROUP_IMG_UNCHECKED then
              imgIndex := GROUP_IMG_PARTIAL
            else
              imgIndex := OUTGROUP_IMG_PARTIAL;
            aNode.Parent.ImageIndex := imgIndex;
            aNode.Parent.SelectedIndex := imgIndex;
          end;
        end;
    end;
    UpdateUngroupedTaxaList;

    // Group tree view
    with GroupTreeview do
    begin
      Selected := Items[0];
      Selected.Expanded := True;
    end;
    UpdateOutgroupLists;
  finally
    GroupTreeView.Items.EndUpdate;
  end;
end;

procedure TTaxaGpsDlg.Initialize;
var
  pw: TPleaseWait = nil;
begin
  if IsInit then
    Exit;
  try
    IsInit := True;
    if Assigned(OtuInfo) and (OtuInfo.NoOfOtus > 1000) then
    begin
      pw := TPleaseWait.Create(Self);
      pw.Caption := 'Please Wait...';
      pw.Action := 'Organizing taxa groups';
      pw.SetToMarqueeMode;
      pw.Show;
    end;

    GroupTreeview.Items[0].StateIndex := -1;
    GroupTreeview.Items[0].StateIndex := ord(nsSelected);
    fillGroupTreeViewFromOtuInfo;
    ClusterView := False;
  finally
    if Assigned(pw) then
      pw.Free;
  end;
end;

procedure TTaxaGpsDlg.UpdateDlg;
var
  i: Longint;
begin
  try
    GroupTreeView.Items.BeginUpdate;
    with GroupTreeview, Items do
      for i:= 1 to Count-1 do
        if IsTaxon(Items[i]) then
          with TOtuInfo(Items[i].Data) do
          begin
            Item[i].Text := Name;
            if IsUsed then
              Items[i].ImageIndex := TAXON_IMG_CHECKED
            else
              Items[i].ImageIndex := TAXON_IMG_UNCHECKED;
          end;
  finally
    GroupTreeView.Items.EndUpdate;
  end;
end;

function TTaxaGpsDlg.GetAbsoluteIndex(AObj: Pointer): Integer;
var
  i: Integer;
begin
  Result := -1;
  with GroupTreeView do
    for i:=0 to Items.Count-1 do
      if Items[i].Data = AObj then
      begin
        Result := Items[i].AbsoluteIndex;
        exit;
      end;
end;

function TTaxaGpsDlg.GetTreeNode(AOtuInfo: TOtuInfo): TTreeNode;
var
  i: Integer;
begin
  Result := nil;
  with GroupTreeview, Items do
    for i:= 1 to Count-1 do
      if TOtuInfo(Items[i].Data) = AOtuInfo then
      begin
        Result := Items[i];
        Exit;
      end;
end;

procedure TTaxaGpsDlg.UsedClusters(MyClusters: TStringList; IsIncludeOtulist: Boolean);
{var
  i, j, AnItem, entries: Integer;
  AList, NewList: TList;
}
begin
  {  MyClusters.Clear;
    NewList := nil;
    try
      with ClustersLBx do
        for i:=0 to ClustersLBx.count-1 do
        begin

          AList := TList(Items.Objects[ItemIndex]);
          if AList.Count = 0 then
            continue;
          if IsIncludeOtuList then
            NewList := TList.Create;
          entries := 0;
          for j:=0 to AList.Count-1 do
          begin
            if not TOtuInfo(AList[j]).IsUsed then
              continue;
            if NewList <> nil then
              NewList.Add(AList[j]);
            Inc(entries);
          end;

          if entries = 0 then
          begin
            if NewList <> nil then NewList.Free;
            continue;
          end;
          AnItem := MyClusters.Add(Items[ItemIndex]);
          MyClusters.Objects[AnItem] := NewList;
          NewList := nil;
        end;
    finally
       if NewList <> nil then NewList.Free;
    end;}
end;

end.

