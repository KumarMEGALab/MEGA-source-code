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

unit MV_DistDataExplorer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF VISUAL_BUILD}
  MEditorForm,
  {$ENDIF}
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Buttons,
  ExtCtrls, ComCtrls, Grids, Menus, MegaConsts, MDistPack,
  MOtuInfo, MLongIntList, ImgList, Spin, IniPropStorage, StdCtrls, ActnList,
  MD_InputDistData, ExcelWrite, MDisplayMatrixDlg, MPleaseWait,
  mimageform, mstringbuilder, Types;

type

  { TV_DistDataExplorer }

  TOpenFrom = (OActivate, OAlign, OOther);

  TV_DistDataExplorer = class(TForm)
    ActionSortByGroup: TAction;
    ActionMoveToBottom: TAction;
    ActionMoveToTop: TAction;
    ActionSortAlphabetical: TAction;
    ActionSortByDistToFocalOtu: TAction;
    ActionRestoreInputOrder: TAction;
    Button1: TButton;
    FindFirstAction: TAction;
    ActionToggleSearchTools: TAction;
    FindNextAction: TAction;
    FindPreviousAction: TAction;
    CloseAction: TAction;
    ActionList1: TActionList;
    DataGrid: TDrawGrid;
    GridHorizScrollbar: TScrollBar;
    GridVertScrollbar: TScrollBar;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    NamesColumnPopupMenu: TPopupMenu;
    SearchEdit: TEdit;
    FileMenu: TMenuItem;
    FontDialog1: TFontDialog;
    ImageList32: TImageList;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    GridPanel: TPanel;
    SearchPanel: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    WindowsMenuItem: TMenuItem;
    MenuItem4: TMenuItem;
    SaveSession: TMenuItem;
    Panel1: TPanel;
    StatusBar: TStatusBar;
    MainMenu1: TMainMenu;
    FileSelectEditTaxaGroupsItem: TMenuItem;
    FileQuitViewerItem: TMenuItem;
    SaveToFileItem: TMenuItem;
    DisplayMenu: TMenuItem;
    DispSelTaxaItem: TMenuItem;
    DisplayN1: TMenuItem;
    SortTaxaSubmenu: TMenuItem;
    DispSortGpItem: TMenuItem;
    DispAsInInputFileItem: TMenuItem;
    DispSortAlphaItem: TMenuItem;
    DisplayN2: TMenuItem;
    DispGpNamesItem: TMenuItem;
    DisplayN3: TMenuItem;
    DispChangeFontItem: TMenuItem;
    AverageMenu: TMenuItem;
    AvgAllItem: TMenuItem;
    AverageN1: TMenuItem;
    EditTaxaGroupsSBtn: TToolButton;
    DecPrecisionSBtn: TToolButton;
    IncPrecisionSBtn: TToolButton;
    ExportDataSBtn: TToolButton;
    ToolButton5: TToolButton;
    WithinGpsAvgItem: TMenuItem;
    BetweenGpsAvgItem: TMenuItem;
    NetBetweenGpsAvgItem: TMenuItem;
    HelpMenu: TMenuItem;
    MainToolBar: TToolBar;
    ToolButton6: TToolButton;
    ExportXLTBtn: TToolButton;
    ExportCSVTBtn: TToolButton;
    ExportMEGATBtn: TToolButton;
    ExportTextTBtn: TToolButton;
    ImageList2: TImageList;
    SaveDialog1: TSaveDialog;

    procedure ActionMoveToBottomExecute(Sender: TObject);
    procedure ActionMoveToTopExecute(Sender: TObject);
    procedure ActionRestoreInputOrderExecute(Sender: TObject);
    procedure ActionSortAlphabeticalExecute(Sender: TObject);
    procedure ActionSortByDistToFocalOtuExecute(Sender: TObject);
    procedure ActionSortByGroupExecute(Sender: TObject);
    procedure ActionToggleSearchToolsExecute(Sender: TObject);
    procedure AvgGpsItemClick(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure DataGridColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure DataGridContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure DataGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure DataGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DataGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DataGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure DecPrecisionSBtnClick(Sender: TObject);
    procedure DispChangeFontItemClick(Sender: TObject);
    procedure DispGpNamesItemClick(Sender: TObject);
    procedure DispSelTaxaItemClick(Sender: TObject);
    procedure DispSortItemClick(Sender: TObject);
    procedure FileSelectEditTaxaGroupsItemClick(Sender: TObject);
    procedure FindFirstActionExecute(Sender: TObject);
    procedure FindNextActionExecute(Sender: TObject);
    procedure FindPreviousActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure DataGridDrawCell(Sender: TObject; ACol, ARow: Longint; aRect: TRect; State: TGridDrawState);
    procedure DataGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DataGridRowMoved(Sender: TObject; FromIndex,ToIndex: Longint);

    procedure FileQuitViewerItemClick(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridHorizScrollbarChange(Sender: TObject);
    procedure GridHorizScrollbarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure GridVertScrollbarChange(Sender: TObject);
    procedure GridVertScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure HelpMenuClick(Sender: TObject);
    procedure IncPrecisionSBtnClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure SaveSessionClick(Sender: TObject);
    procedure FileSaveToFileItemClick(Sender: TObject);

    procedure AvgAllItemClick(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FMouseGridCoords: TPoint;
    FStrBuilder: TMegaStringBuilder;
    FSearchResult: Integer;
    FPrecision: Integer;
    FScrollStarted: Boolean;
    FSelfScrolling: Boolean;
    FVScrollPos: Integer;
    FHScrollPos: Integer;
    FLastRow: Integer;
    CheckMark      : TBitmap;
    UnCheckMark    : TBitmap;
    FHasDataSubsetChanged : Boolean;
    FHasDataViewChanged   : Boolean;

    procedure CheckGroupWiseActionsEnabled;
    function ConstructRowLabel(aRow: Integer): String;
    procedure UnhighlightSearchResult;
    function SearchLabels(searchQuery: String; startRow: Integer; searchForward: Boolean): Integer;
    function FindTaxon(searchQuery: String; startRow: Integer; searchForward: Boolean): Integer;
    function RowVisible(aRow: Integer): Boolean;
    function ColVisible(aCol: Integer): Boolean;
    function ValidSelectableRow(aRow: Integer): Integer;
    function ValidSelectableCol(aCol: Integer): Integer;
    procedure InitScrollbars(setPosition: Boolean);
    procedure UpdateImages;
    procedure InitMainMenu;
    procedure ComputeLeftOfDecimalLen;
    procedure AdjustEnabledStatus;         // Enabled/Disable menu/toolbar items
    procedure AssignHelpContext;
    procedure UpdateStatusBar(ARow, ACol: Integer);
    procedure UpdateMaxTaxaNamePixels;
    procedure UpdateMaxGpNamePixels;
    procedure UpdateFirstColWidth;       // automaticallycalls the above two
    procedure UpdateColWidths;
  public
    function NumTaxa: Integer;
    procedure Initialize;
    procedure ApplyUpdate;

    property HasDataSubsetChanged: Boolean read FHasDataSubsetChanged;
    property HasDataViewChanged:   Boolean read FHasDataViewChanged;
  end;

  procedure AvgGpsCalculation(MyComputeType: TDistType);
var
  V_DistDataExplorer: TV_DistDataExplorer;

implementation

{$R *.lfm}

uses
  {$IFDEF VISUAL_BUILD}
  MMegaWindowInfo, LCLType,
  {$ENDIF}
  Printers, MegaUtils, Mega_Main, MegaErrUtils, math,
  ContextHelp_HC, ErrorMessages_HC, MTaxaGpsDlg, DataExplorerHelp_HC,
  MegaVerConsts, MVS_DistDataExplorer, mhelpfiles, mhelpkeywords,
  MegaPrivateFiles, mcrossprojectutils, MOutputMatrixDlg;



{ TV_DistDataExplorer }

procedure AvgGpsCalculation(MyComputeType: TDistType);
var
  DispDlg : TD_DisplayMatrix;
  DispDlgVS : TVS_DisplayMatrix;
  DispDlgV : TDisplayMatrixDlg;
  i : Integer;
  ResultsWindowSetupOptions : TResultsWindowSetupOptions;
begin
  DispDlg      := nil;
  DispDlgV      := nil;
  DispDlgVS     := nil;

  // setup the type of average into MyComputeType
  if (not ((MyComputeType = gdWithinGroupMean) or (MyComputeType = gdBetweenGroupMean) or (MyComputeType = gdNetGroupMean))) then
    Exit;   //Not one of the three statistical menu items that this function supports

  try
    ResultsWindowSetupOptions := D_InputDistData.GetAvgGps(MyComputeType);  //Call the corresponding data function providing the computation type and the UsedOTUInfos

    DispDlg := TD_DisplayMatrix.Create;
    DispDlgVS := TVS_DisplayMatrix.Create;

    DispDlgV := TDisplayMatrixDlg.Create(Application);
    DispDlgV.LinkDataClass(DispDlg);
    DispDlgV.LinkVisualStatusClass(DispDlgVS);

    DispDlg.LinkVisualStatusClass(DispDlgVS);
    DispDlg.Title := 'Input data was averaged';
    DispDlg.DAcronym := 'Avg.';
    DispDlg.FullName:= 'Average distances from Input distance data';
    DispDlg.ComputationType := MyComputeType;
    DispDlg.NoOfGps:= ResultsWindowSetupOptions.MyNoOfGps; //this is the best estimate FNoOfGps;

    for i := 0 to ResultsWindowSetupOptions.GpNames.Count -1 do   //Fill in the list of Groups to the dialog
      DispDlg.GpName[i] := ResultsWindowSetupOptions.GpNames.Strings[i];
    DispDlg.DistMat := ResultsWindowSetupOptions.DistanceMatrix;
    DispDlg.DistArray := ResultsWindowSetupOptions.DistArray;  // This dist array is empty, I'm pretty sure this is what causes the error while displaying since we are now giving it an empty DistArray.
    DispDlg.Initialize;
    DispDlgV.Caption := ResultsWindowSetupOptions.Caption;
    DispDlgV.CaptionBtn.Visible := False;
    DispDlgV.Show;
  except
    On E: Exception do
    begin
      if (DispDlgV <> nil) AND (DispDlgV.Showing) then //make sure its already showing otherwise if we get an exception before its shown and try to hide it we would get an Access Violation
       DispDlgV.Hide;

      FreeAndNil(DispDlg);
      FreeAndNil(DispDlgV);
      FreeAndNil(DispDlgVS);
      E.HelpContext := HC_Unexpected_Error;
      ShowErrorMessage(E);
    end;
  end;
end;


procedure TV_DistDataExplorer.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;


procedure TV_DistDataExplorer.FileSelectEditTaxaGroupsItemClick(Sender: TObject);
begin
  if TaxaGpsDlg = nil then
    Exit;
  TaxaGpsDlg.ShowModal;
  if D_InputDistData.FOtuInfos.IsDirty then
  begin
    try
      ApplyUpdate;
      D_InputDistData.FOtuInfos.IsDirty := False;
    except
      on E: Exception do ShowMessage(E.Message);
    end
  end;
  DataGrid.Invalidate;
  CheckGroupWiseActionsEnabled;
end;

procedure TV_DistDataExplorer.FindFirstActionExecute(Sender: TObject);
var
  searchQuery: String = '';
  alreadyOpen: Boolean = False;
begin
  UnhighlightSearchResult;
  if not ActionToggleSearchTools.Checked then
    ActionToggleSearchToolsExecute(Sender)
  else
    alreadyOpen := True;

  if SearchEdit.Text <> EmptyStr then
  begin
    searchQuery := Trim(SearchEdit.Text);
    if searchQuery = EmptyStr then
    begin
      ShowMessage('Please enter a query to search for');
      Exit;
    end;

    FSearchResult := FindTaxon(SearchEdit.Text, 0, True);
  end
  else
  begin
    if alreadyOpen then
      ShowMessage('Please enter a query to search for');
    if SearchEdit.Visible then
      SearchEdit.SetFocus;
  end;
end;

procedure TV_DistDataExplorer.FindNextActionExecute(Sender: TObject);
var
  startRow: Integer = 0;
  searchQuery: String = '';
  alreadyOpen: Boolean = False;
begin
  UnhighlightSearchResult;
  if not ActionToggleSearchTools.Checked then
    ActionToggleSearchToolsExecute(Sender)
  else
    alreadyOpen := True;

  if SearchEdit.Text <> EmptyStr then
  begin
    searchQuery := Trim(SearchEdit.Text);
    if searchQuery = EmptyStr then
    begin
      ShowMessage('Please enter a query to search for');
      Exit;
    end;

    if DataGrid.Row = (DataGrid.RowCount - 1) then
    begin
      if (MessageDlg('Already at the last row. Do you want to search from the beginning?', mtConfirmation, [mbYes, mbNo],0) = mrYes) then
        FSearchResult := FindTaxon(SearchEdit.Text, 0, True);
      Exit;
    end;
    if DataGrid.Row >= 1 then
      startRow := min(DataGrid.RowCount - 2, DataGrid.Row);
    FSearchResult := FindTaxon(SearchEdit.Text, startRow, True);
  end
  else
  begin
    if alreadyOpen then
      ShowMessage('Please enter a query to search for');
    if SearchEdit.Visible then
      SearchEdit.SetFocus;
  end;
end;

procedure TV_DistDataExplorer.FindPreviousActionExecute(Sender: TObject);
var
  startRow: Integer = 0;
  searchQuery: String = '';
  alreadyOpen: Boolean = False;
begin
  UnhighlightSearchResult;
  if not ActionToggleSearchTools.Checked then
    ActionToggleSearchToolsExecute(Sender)
  else
    alreadyOpen := True;

  if SearchEdit.Text <> EmptyStr then
  begin
    searchQuery := Trim(SearchEdit.Text);
    if searchQuery = EmptyStr then
    begin
      ShowMessage('Please enter a query to search for');
      Exit;
    end;

    if DataGrid.Row = 1 then
    begin
      if (MessageDlg('Already at the first row. Do you want to search from the end?', mtConfirmation, [mbYes, mbNo],0) = mrYes) then
        FSearchResult := FindTaxon(SearchEdit.Text, DataGrid.RowCount - 2, False);
      Exit;
    end;

    if DataGrid.Row >= 2 then
      startRow := DataGrid.Row - 2
    else
      startRow := DataGrid.RowCount - 2;
    FSearchResult := FindTaxon(SearchEdit.Text, startRow, False);
  end
  else
  begin
    if alreadyOpen then
      ShowMessage('Please enter a query to search for');
    if SearchEdit.Visible then
      SearchEdit.SetFocus;
  end;
end;

procedure TV_DistDataExplorer.FormActivate(Sender: TObject);
begin
  If VS_DistDataExplorer.IsFormInit = False then
  begin
    UpdateColWidths;
    VS_DistDataExplorer.IsFormInit := True;
  end;
  {$IFDEF DARWIN}
  MainToolbar.Invalidate;
  {$ENDIF}
  InitScrollBars(False);
  DataGrid.Invalidate;
  MegaForm.UpdateMenuForPinupItems(WindowsMenuItem, Self);
  CheckGroupWiseActionsEnabled;
end;

procedure TV_DistDataExplorer.DispChangeFontItemClick(Sender: TObject);
begin
  FontDialog1.Font.Assign(DataGrid.Canvas.Font);
  FontDialog1.Font.Color := clBlack; //Forcing color to be black, even though we don't set the color it sometimes turns to white.
  if FontDialog1.Execute then
  begin
    DataGrid.Font.Assign(FontDialog1.Font);
    DataGrid.Canvas.Font.Assign(FontDialog1.Font);
    DataGrid.ParentFont := False;
    VS_DistDataExplorer.CurFont.Assign(DataGrid.Canvas.Font);
    UpdateColWidths;
  end;
  FHasDataViewChanged := True;
  Invalidate;
end;

procedure TV_DistDataExplorer.DataGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if DataGrid.Cursor = crHSplit then  // Only start resizing, once the cursor is over the correct place and you get a click, this way you can't "slide" into it accidently.
    VS_DistDataExplorer.FIsResizing := true;
end;

procedure TV_DistDataExplorer.AvgGpsItemClick(Sender: TObject);
begin
  if      Sender = WithinGpsAvgItem     then  AvgGpsCalculation(gdWithinGroupMean)
  else if Sender = BetweenGpsAvgItem    then  AvgGpsCalculation(gdBetweenGroupMean)
  else if Sender = NetBetweenGpsAvgItem then  AvgGpsCalculation(gdNetGroupMean)
end;

procedure TV_DistDataExplorer.CloseActionExecute(Sender: TObject);
begin
  if ActionToggleSearchTools.Checked then
    ActionToggleSearchToolsExecute(Sender);
end;

procedure TV_DistDataExplorer.ActionToggleSearchToolsExecute(Sender: TObject);
begin
  DataGrid.Invalidate;
  ActionToggleSearchTools.Checked := not ActionToggleSearchTools.Checked;
  SearchPanel.Visible := ActionToggleSearchTools.Checked;
end;

procedure TV_DistDataExplorer.ActionRestoreInputOrderExecute(Sender: TObject);
begin
  DispSortItemClick(DispAsInInputFileItem);
end;

procedure TV_DistDataExplorer.ActionMoveToTopExecute(Sender: TObject);
begin
  if FMouseGridCoords.Y > 1 then
    DataGridRowMoved(Sender, FMouseGridCoords.Y, 1);
end;

procedure TV_DistDataExplorer.ActionMoveToBottomExecute(Sender: TObject);
begin
  if FMouseGridCoords.Y < DataGrid.RowCount then
    DataGridRowMoved(Sender, FMouseGridCoords.Y, D_InputDistData.DispTaxaList.Count);
end;

procedure TV_DistDataExplorer.ActionSortAlphabeticalExecute(Sender: TObject);
begin
  DispSortItemClick(DispSortAlphaItem)
end;

procedure TV_DistDataExplorer.ActionSortByDistToFocalOtuExecute(Sender: TObject);
var
  aId: Integer = -1;
begin
  if FSearchResult >= 0 then
    aId := D_InputDistData.FOtuInfos[D_InputDistData.DispTaxaList[FSearchResult]].Id;
  D_InputDistData.FOtuInfos.CountPairwiseDistFromFirstVisibleOtu;
  D_InputDistData.FOtuInfos.SortBySimilarityToFirstOtu;
  D_InputDistData.UpdateDispTaxaList;
  if aId <> -1 then
  begin
    FSearchResult := D_InputDistData.FOtuInfos.FindById(aId);
    FSearchResult := D_InputDistData.DispTaxaList.FindFirst(FSearchResult);
  end;
  DataGrid.Invalidate;
end;

procedure TV_DistDataExplorer.ActionSortByGroupExecute(Sender: TObject);
begin
  DispSortItemClick(DispSortGpItem);
end;

procedure TV_DistDataExplorer.DataGridColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if not IsColumn then
    DataGridRowMoved(Sender, sIndex, tIndex);
end;

procedure TV_DistDataExplorer.DataGridContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  coords: TPoint;
begin
  coords := ClientToScreen(MousePos);
  FMouseGridCoords := DataGrid.MouseToCell(MousePos);
  if FMouseGridCoords.X = 0 then
    NamesColumnPopupMenu.PopUp(coords.X, coords.Y);
  Handled := True
end;

procedure TV_DistDataExplorer.DataGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
        cellCoord := DataGrid.LeftCol - GridHorizScrollbar.LargeChange;
        DataGrid.LeftCol := ValidSelectableCol(cellCoord);
        DataGrid.Col := ValidSelectableCol(DataGrid.Col - GridHorizScrollbar.LargeChange + 1);
      end
      else if Key = VK_RIGHT then
      begin
        delta := DataGrid.Col - DataGrid.LeftCol;
        cellCoord := DataGrid.LeftCol + GridHorizScrollbar.LargeChange;
        DataGrid.LeftCol := ValidSelectableCol(cellCoord);
        if delta > 0 then
        begin
          cellCoord := DataGrid.Col + GridHorizScrollbar.LargeChange - 1;
          DataGrid.Col := ValidSelectableCol(cellCoord);
        end
        else
          DataGrid.Col := ValidSelectableCol(DataGrid.LeftCol);
      end
      else if Key = VK_UP then
      begin
        delta := DataGrid.Row - DataGrid.TopRow;
        cellCoord := DataGrid.TopRow - GridVertScrollbar.LargeChange;
        DataGrid.TopRow := ValidSelectableRow(cellCoord);
        cellCoord := DataGrid.Row - GridVertScrollbar.LargeChange + 1;
        if delta > 0 then
          DataGrid.Row := ValidSelectableRow(cellCoord)
        else
          DataGrid.Row := DataGrid.TopRow;
      end
      else if Key = VK_DOWN then
      begin
        delta := DataGrid.Row - DataGrid.TopRow;
        cellCoord := DataGrid.TopRow + GridVertScrollbar.LargeChange;
        DataGrid.TopRow := ValidSelectableRow(cellCoord);
        if delta > 0 then
        begin
          cellCoord := DataGrid.Row + GridVertScrollbar.LargeChange - 1;
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

procedure TV_DistDataExplorer.DataGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const
  DRAG_INCREMENT = 5;
var
  aCol: Integer = -1;
  aRow: Integer = -1;
begin
  if VS_DistDataExplorer.FIsResizing then
    begin
      if X > 15 then // Can't have col 0 smaller than 15 pixels
      DataGrid.ColWidths[0] := X;
    end;

  if (abs(X - DataGrid.ColWidths[0]) < 5) or (VS_DistDataexplorer.FIsResizing) then
  begin
    DataGrid.Cursor := crHSplit;
    DataGrid.Options := DataGrid.Options - [goRowMoving]; //If we show them the split arrow they can split but not move rows otherwise rows are accidentally moved
  end
  else
  begin
   DataGrid.Options := DataGrid.Options + [goRowMoving]; //They are not at a location to resize the first column so allow them to move rows
   DataGrid.Cursor := crArrow;
  end;

  DataGrid.MouseToCell(X, Y, aCol, aRow);
  if (aRow <> FLastRow) and (goRowMoving in DataGrid.Options) then
  begin
    if ssLeft in Shift then
    begin
      if (aRow < FLastRow) and (GridVertScrollbar.Position > 0) then { dragging upward}
      begin
        if abs(aRow - GridVertScrollbar.Position) <= DRAG_INCREMENT then
          GridVertScrollbar.Position := max(0, GridVertScrollbar.Position - 2);
      end
      else if (aRow > FLastRow) and (GridVertScrollbar.Position < GridVertScrollbar.Max) then { dragging downward}
      begin
        if abs(aRow - GridVertScrollbar.Position - GridVertScrollbar.PageSize) <= DRAG_INCREMENT then
          GridVertScrollbar.Position := min(GridVertScrollbar.Max, GridVertScrollbar.Position + 2);
      end;
    end;
  end;
  FLastRow := aRow;
end;

procedure TV_DistDataExplorer.DataGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  if (not ColVisible(GridHorizScrollbar.Position)) or (not ColVisible(aCol)) then
  begin
    if aCol < GridHorizScrollbar.Position then
      GridHorizScrollbar.Position := ValidSelectableCol(aCol)
    else if aCol > GridHorizScrollbar.Position then
      GridHorizScrollbar.Position := ValidSelectableCol(max(1, aCol - DataGrid.VisibleColCount));
  end;

  if (not RowVisible(GridVertScrollbar.Position)) or (not RowVisible(aRow)) then
  begin
    if (aRow < GridVertScrollbar.Position) then
      GridVertScrollbar.Position := ValidSelectableRow(aRow)
    else if (aRow > GridVertScrollbar.Position) then
      GridVertScrollbar.Position := ValidSelectableRow(max(1, aRow - DataGrid.VisibleRowCount));
  end;
end;

procedure TV_DistDataExplorer.DecPrecisionSBtnClick(Sender: TObject);
var
  Col0Width: Integer;
begin
  if VS_DistDataExplorer.FPrecision <= 0 then
    Exit;
  VS_DistDataExplorer.FPrecision := VS_DistDataExplorer.FPrecision - 1;
  DecPrecisionSBtn.Enabled := VS_DistDataExplorer.FPrecision > 0;
  IncPrecisionSBtn.Enabled := VS_DistDataExplorer.FPrecision < 10;
  Col0Width := DataGrid.ColWidths[0];
  UpdateColWidths;
  DataGrid.ColWidths[0] := Col0Width;
  DataGrid.Invalidate;
  FPrecision := VS_DistDataExplorer.FPrecision;

  FHasDataViewChanged := True;
end;

procedure TV_DistDataExplorer.AvgAllItemClick(Sender: TObject);
begin
  MessageDlg('The overall average is '+
     FloatToStrF(D_InputDistData.GetAvgAll,ffFixed,VS_DistDataExplorer.FPrecision+4,VS_DistDataExplorer.FPrecision),mtInformation, [mbOK], 0);
end;

procedure TV_DistDataExplorer.SearchEditChange(Sender: TObject);
begin
  if FSearchResult <> -1 then
  begin
    FSearchResult := -1;
    DataGrid.Invalidate;
  end;
end;

procedure TV_DistDataExplorer.SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (SearchEdit.Text <> EmptyStr) then
    FindFirstActionExecute(Sender);
end;

procedure TV_DistDataExplorer.CheckGroupWiseActionsEnabled;
var
  hasGroups: Boolean = False;
begin
  hasGroups := D_InputDistData.FOtuInfos.HasGroupNames;
  ActionSortByGroup.Enabled := hasGroups;
  WithinGpsAvgItem.Enabled := hasGroups;
  BetweenGpsAvgItem.Enabled := hasGroups;
  NetBetweenGpsAvgItem.Enabled := hasGroups;
  DispGpNamesItem.Enabled := hasGroups;
  if not hasGroups then
    DispGpNamesItem.Checked := False;
  DispSortGpItem.Enabled := hasGroups;
end;

function TV_DistDataExplorer.ConstructRowLabel(aRow: Integer): String;
begin
  FStrBuilder.Clean;
  if DispGpNamesItem.Checked then
  begin
    if Length(D_InputDistData.GpName[aRow - 1]) > 0 then
    begin
      FStrBuilder.Add(IntToStr(aRow));
      FStrBuilder.Add('. ');
      FStrBuilder.Add(D_InputDistData.TaxaName[aRow - 1]);
      FStrBuilder.Add(' {');
      FStrBuilder.Add(D_InputDistData.GpName[aRow - 1]);
      FStrBuilder.Add('}');
    end
    else
    begin
      FStrBuilder.Add(IntToStr(aRow));
      FStrBuilder.Add('. ');
      FStrBuilder.Add(D_InputDistData.TaxaName[aRow - 1]);
    end;
  end
  else
  begin
    FStrBuilder.Add(IntToStr(aRow));
    FStrBuilder.Add('. ');
    FStrBuilder.Add(D_InputDistData.TaxaName[aRow - 1]);
  end;
  Result := FStrBuilder.GenerateString;
end;

procedure TV_DistDataExplorer.UnhighlightSearchResult;
var
  temp: Integer = -1;
begin
  if FSearchResult <> -1 then
  begin
    temp := FSearchResult;
    FSearchResult := -1;
    DataGrid.InvalidateRow(temp + 1);
    DataGrid.InvalidateCol(temp + 1);
  end;
end;

function TV_DistDataExplorer.SearchLabels(searchQuery: String; startRow: Integer; searchForward: Boolean): Integer;
var
  i: Integer = -1;
  aName: String = '';
  query: String = '';
begin
  Result := -1;
  if startRow >= D_InputDistData.DispTaxaList.Count then
    Exit;
  if D_InputDistData.DispTaxaList.Count > 0 then
  begin
    query := LowerCase(searchQuery);
    if searchForward then
    begin
      for i := startRow to D_InputDistData.DispTaxaList.Count - 1 do
      begin
        aName := LowerCase(ConstructRowLabel(i + 1));
        if Pos(query, aName) > 0 then
          Exit(i);
      end;
    end
    else
    begin
      for i := startRow downto 0 do
      begin
        aName := LowerCase(ConstructRowLabel(i + 1));
        if Pos(query, aName) > 0 then
          Exit(i);
      end;
    end;
  end;
end;

function TV_DistDataExplorer.FindTaxon(searchQuery: String; startRow: Integer; searchForward: Boolean): Integer;
var
  wrapSearch: Boolean = False;
begin
  Result := -1;
  FSearchResult := SearchLabels(searchQuery, startRow, searchForward);
  if FSearchResult = -1 then
  begin
    if searchForward and (startRow <> 0) then
    begin
      wrapSearch := (MessageDlg(Format('Query "%s" not found. Do you want to go to the top and search again?', [searchQuery]), mtConfirmation, [mbYes, mbNo],0) = mrYes);
      if wrapSearch then
        FSearchResult := SearchLabels(SearchEdit.Text, 0, True);
    end
    else if (not SearchForward) and (startRow <> (DataGrid.RowCount - 1)) then
    begin
      wrapSearch := (MessageDlg(Format('Query "%s" not found. Do you want to go to the bottom and search again?', [searchQuery]), mtConfirmation, [mbYes, mbNo],0) = mrYes);
      if wrapSearch then
        FSearchResult := SearchLabels(SearchEdit.Text, DataGrid.RowCount - 2, False);
    end;
  end;

  if (FSearchResult = -1) then
  begin
    if wrapSearch or (searchForward and (startRow = 0)) then
      ShowMessage(Format('Query "%s" not found.', [searchQuery]));
    Exit;
  end
  else
  begin
    DataGrid.Row := FSearchResult + 1;
    Invalidate;
  end;
  Result := FSearchResult;
end;

function TV_DistDataExplorer.RowVisible(aRow: Integer): Boolean;
begin
  Result := False;
  if aRow < DataGrid.TopRow then
    Exit;
  if aRow > (DataGrid.TopRow + DataGrid.VisibleRowCount) then
    Exit;
  Result := True;
end;

function TV_DistDataExplorer.ColVisible(aCol: Integer): Boolean;
begin
  Result := False;
  if aCol < DataGrid.LeftCol then
    Exit;
  if aCol > (DataGrid.LeftCol + DataGrid.VisibleColCount) then
    Exit;
  Result := True;
end;

function TV_DistDataExplorer.ValidSelectableRow(aRow: Integer): Integer;
begin
  Result := aRow;
  if aRow < DataGrid.FixedRows then
    Result := DataGrid.FixedRows
  else if aRow >= DataGrid.RowCount then
    Result := DataGrid.RowCount - 1;
end;

function TV_DistDataExplorer.ValidSelectableCol(aCol: Integer): Integer;
begin
  Result := aCol;
  if aCol < DataGrid.FixedCols then
    Result := DataGrid.FixedCols
  else if aCol >= DataGrid.ColCount then
    Result := DataGrid.ColCount - 1;
end;

procedure TV_DistDataExplorer.InitScrollbars(setPosition: Boolean);
var
  p: Integer = -1;
  aPosition: Integer = -1;
begin
  if setPosition then
    aPosition := 1
  else
    aPosition := GridHorizScrollbar.Position;
  if DataGrid.ColCount > (DataGrid.VisibleColCount + DataGrid.FixedCols) then
  begin
    GridHorizScrollbar.Visible := True;
    GridHorizScrollbar.SmallChange := 1;
    GridHorizScrollbar.LargeChange := max(DataGrid.VisibleColCount, GridHorizScrollbar.SmallChange);
    if DataGrid.ColCount > DataGrid.VisibleColCount then
      p := max(1, DataGrid.VisibleColCount - 1)
    else
      p := 1;
    GridHorizScrollbar.SetParams(aPosition, 1, DataGrid.ColCount - DataGrid.FixedCols, max(1, p));
  end
  else
    GridHorizScrollbar.Visible := False;

  if setPosition then
    aPosition := 1
  else
    aPosition := GridVertScrollbar.Position;
  if DataGrid.RowCount > (DataGrid.VisibleRowCount + DataGrid.FixedRows) then
  begin
    GridVertScrollbar.Visible := True;
    GridVertScrollbar.SmallChange := 1;
    GridVertScrollbar.LargeChange := max(DataGrid.VisibleRowCount, GridVertScrollbar.SmallChange);
    if DataGrid.RowCount > DataGrid.VisibleRowCount then
      p := max(1, DataGrid.VisibleRowCount - 1)
    else
      p := 1;
    GridVertScrollbar.SetParams(aPosition, 1, DataGrid.RowCount - DataGrid.FixedRows, max(1, p));
  end
  else
    GridVertScrollbar.Visible := False;
end;

procedure TV_DistDataExplorer.UpdateImages;
var
  scalingFactor: Double = -1;
begin
  if PixelsPerInch > DesignTimePPI then
    scalingFactor := PixelsPerInch/DesignTimePPI;
  if CompareValue(scalingFactor, 1.5, FP_CUTOFF) >= 0 then
  begin
    ImageList32.GetBitmap(0, CheckMark);
    ImageList32.GetBitmap(1, UnCheckMark);
  end
  else
  begin
    ImageList2.GetBitmap(0, CheckMark);
    ImageList2.GetBitmap(1, UnCheckMark);
  end;
end;

procedure TV_DistDataExplorer.InitMainMenu;
begin
  {$IFNDEF DARWIN}
  MainMenu1.OwnerDraw := True;
  MainMenu1.OnMeasureItem := MegaForm.MeasureMenuItem;
  MainMenu1.OnDrawItem := MegaForm.DrawMenuItem
  {$ENDIF}
end;

procedure TV_DistDataExplorer.DispGpNamesItemClick(Sender: TObject);
begin
  DispGpNamesItem.Checked := not DispGpNamesItem.Checked;
  VS_DistDataExplorer.DispShowGpNames := DispGpNamesItem.Checked;
  UpdateFirstColWidth;
  DataGrid.Invalidate;

  FHasDataViewChanged := True;
end;

procedure TV_DistDataExplorer.DispSelTaxaItemClick(Sender: TObject);
var
  aId: Integer = -1;
  i: Integer = -1;
begin
  if FSearchResult >= 0 then
    aId := D_InputDistData.FOtuInfos[D_InputDistData.DispTaxaList[FSearchResult]].Id;
  with Sender as TMenuItem do
    Checked := not Checked;
  VS_DistDataExplorer.DispSelTaxaItem := (Sender as TMenuItem).Checked;
  if VS_DistDataExplorer.DispSelTaxaItem then
    D_InputDistData.FOtuInfos.HideUnselected
  else
    D_InputDistData.FOtuInfos.UnhideAll;
  D_InputDistData.UpdateDispTaxaList;
  DataGrid.RowCount := D_InputDistData.DispTaxaList.Count+1;
  DataGrid.ColCount := D_InputDistData.DispTaxaList.Count+1;
  if aId <> -1 then
  begin
    FSearchResult := D_InputDistData.FOtuInfos.FindById(aId);
    FSearchResult := D_InputDistData.DispTaxaList.FindFirst(FSearchResult);
  end;
  DataGrid.Invalidate;

  FHasDataViewChanged := True;
end;

procedure TV_DistDataExplorer.DispSortItemClick(Sender: TObject);
var
  aId: Integer = -1;
begin
  try
    if FSearchResult >= 0 then
      aId := D_InputDistData.FOtuInfos[D_InputDistData.DispTaxaList[FSearchResult]].Id;
    if Sender = DispAsInInputFileItem then
      D_InputDistData.FOtuInfos.SortByInputOrder
    else if Sender = DispSortAlphaItem then
      D_InputDistData.FOtuInfos.SortByTaxaNameAsc
    else if Sender = DispSortGpItem then
      D_InputDistData.FOtuInfos.SortByGroupNameAsc;

    D_InputDistData.UpdateDispTaxaList;
    FHasDataViewChanged := True;
    if aId <> -1 then
    begin
      FSearchResult := D_InputDistData.FOtuInfos.FindById(aId);
      FSearchResult := D_InputDistData.DispTaxaList.FindFirst(FSearchResult);
    end;
  finally
    DataGrid.Invalidate;
  end;
end;

procedure TV_DistDataExplorer.FileQuitViewerItemClick(Sender: TObject);
begin
  inherited;
  Close;
end;

function TV_DistDataExplorer.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
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

procedure TV_DistDataExplorer.FormResize(Sender: TObject);
begin
  InitScrollbars(False);
  DataGrid.Invalidate;
end;

procedure TV_DistDataExplorer.FormShow(Sender: TObject);
begin
  UpdateImages;
  if VS_DistDataExplorer.CurFont = nil then
    VS_DistDataExplorer.CurFont := TFont.Create;

  VS_DistDataExplorer.CurFont.Assign(DataGrid.Canvas.Font);
  if VS_DistDataExplorer.FPrecision = -1 then //If we do not already have a precision set in the visual status (saved sessions have a precision in visual status and we don't want to overwrite them)
  begin
    VS_DistDataExplorer.FPrecision := FPrecision;
    VS_DistDataExplorer.DispShowGpNames := DispGpNamesItem.Checked;
    VS_DistDataExplorer.DispSelTaxaItem := DispSelTaxaItem.Checked;
    DecPrecisionSBtn.Enabled := VS_DistDataExplorer.FPrecision > 0;
    IncPrecisionSBtn.Enabled := VS_DistDataExplorer.FPrecision < 10;
    DataGrid.DefaultColWidth :=  DataGrid.Canvas.TextWidth('W')*4 + 2;  //since FIXED fonts, it must work
    DataGrid.DefaultRowHeight := DataGrid.Canvas.TextHeight('W')+4; //since FIXED fonts, it must work
    UpdateFirstColWidth;
  end;
  Invalidate;
end;

procedure TV_DistDataExplorer.GridHorizScrollbarChange(Sender: TObject);
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
      SendMessage(GridHorizScrollbar.Handle, WM_CANCELMODE, WM_CANCELMODE, 0);
      FScrollStarted := False;
      Exit;
    end;
    { end workaround}

    FHScrollPos := GridHorizScrollbar.Position;
    if (FHScrollPos >= DataGrid.FixedCols) and (FHScrollPos <= DataGrid.ColCount) then
    begin
      DataGrid.LeftCol := min(FHScrollPos, DataGrid.ColCount - DataGrid.VisibleColCount);
      FScrollStarted := True;
    end;
  except
    FScrollStarted := False;
  end;
end;

procedure TV_DistDataExplorer.GridHorizScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
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

procedure TV_DistDataExplorer.GridVertScrollbarChange(Sender: TObject);
var
  mouseLeftButtonDown: Boolean = False;
begin
  if FSelfScrolling then Exit;

  try
    { begin workaround to stop the scrollbar from chasing the cursor when the mouseup event is lost}
    mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
    if (not mouseLeftButtonDown) and FScrollStarted then
    begin
      SendMessage(GridVertScrollbar.Handle, WM_CANCELMODE, WM_CANCELMODE, 0);
      FScrollStarted := False;
      Exit;
    end;
    { end workaround}
    FVScrollPos := GridVertScrollbar.Position;
    if (FVScrollPos >= DataGrid.FixedRows) and (FVScrollPos <= DataGrid.RowCount) then
    begin
      DataGrid.TopRow := min(FVScrollPos, DataGrid.RowCount - DataGrid.VisibleRowCount);
      FScrollStarted := True;
    end;
  except
    FScrollStarted := False;
  end;
end;

procedure TV_DistDataExplorer.GridVertScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
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

procedure TV_DistDataExplorer.HelpMenuClick(Sender: TObject);
begin
 ShowContextSensitiveHelp(MapHelpContextToKeyword(HC_Distance_Data_Explorer));
end;

procedure TV_DistDataExplorer.IncPrecisionSBtnClick(Sender: TObject);
begin
  if VS_DistDataExplorer.FPrecision >= 10 then
    Exit;
  VS_DistDataExplorer.FPrecision := VS_DistDataExplorer.FPrecision + 1;
  DecPrecisionSBtn.Enabled := VS_DistDataExplorer.FPrecision > 0;
  IncPrecisionSBtn.Enabled := VS_DistDataExplorer.FPrecision < 10;
  FPrecision := VS_DistDataExplorer.FPrecision;
  FHasDataViewChanged := True;
  UpdateColWidths;
end;

procedure TV_DistDataExplorer.MenuItem2Click(Sender: TObject);
begin

end;


procedure TV_DistDataExplorer.SaveSessionClick(Sender: TObject);
var
  PleaseWait : TPleaseWait;
begin
  PleaseWait := nil;
  try
    PleaseWait := TPleaseWait.Create(nil);
    SaveDialog1.Filter := MegaSessionFilesFilter;
    SaveDialog1.FileName := '*.mdsx';
    SaveDialog1.Title := 'Please Choose a name for your Mega Data Session file';
    SaveDialog1.Options := [ofOverwritePrompt];
    SaveDialog1.InitialDir := ChangeInitialDirectorySaveDialogForMac(SaveDialog1.InitialDir);
    if NOT SaveDialog1.Execute then
      Exit;

    if ExtractFileExt(SaveDialog1.FileName) <> '.mdsx' then //Add the .mdsx if they don't have it on there
      SaveDialog1.Filename := SaveDialog1.Filename + '.mdsx';
    SaveDialog1.InitialDir := ChangeInitialDirectorySaveDialogForMac(ExtractFilePath(SaveDialog1.FileName));
    PleaseWait.Caption := 'Saving Distance Data Session File';
    PleaseWait.Action := 'Please Wait .... ';
    PleaseWait.show;
    D_InputDistData.SaveSession(SaveDialog1.FileName);
    PleaseWait.hide;
  finally
    FreeAndNil(PleaseWait);
  end;
end;


procedure TV_DistDataExplorer.FileSaveToFileItemClick(Sender: TObject);
var
  OutputMatrixDlg: TOutputMatrixDlg = nil;
  Results: TStringList = nil;
  ExportOptions: TDistExportOptions = nil;
begin
  try
    try
      OutputMatrixDlg := TOutputMatrixDlg.Create(Self);
      OutputMatrixDlg.NoOfRows := VS_DistDataExplorer.NoOfSelTaxa;
      OutputMatrixDlg.NoOfCols := VS_DistDataExplorer.NoOfSelTaxa;
      OutputMatrixDlg.AllowStandardError := False;
      OutputMatrixDlg.HelpContext := HC_Dist_Exp_Options_dialog_box;
      OutputMatrixDlg.AllowMEGAFormat := True;

      with OutputMatrixDlg do
      begin
        AllowNegative := False;
        Precision := VS_DistDataExplorer.FPrecision;
        if Sender = ExportMEGATBtn then         SetExportToMEGA
        else if Sender = ExportXLTBtn then      SetExportToXL
        else if Sender = ExportCSVTBtn then     SetExportToCSV
        else if Sender = ExportTextTBtn then    SetExportToText;

        if ShowModal = mrOk then
        begin
          ExportOptions := OutputMatrixDlg.GetExportOptions;
          if ExportOptions.IsSpreadsheetFormat and (ExportOptions.OutputFormat <> ExportCSV) then
            ExportOptions.SaveExcelFileTo := GetSaveLocation(ExportOptions.OutputFormat);
          Results := D_InputDistData.WriteExportToFile(ExportOptions);
          if ExportOptions.IsSpreadsheetFormat and (ExportOptions.OutputFormat <> ExportCSV) then
            RunAProgram(ExportOptions.SaveExcelFileTo)
          else if (not ExportOptions.IsSpreadsheetFormat) or (ExportOptions.OutputFormat = ExportCSV) then
            OpenStringList(Results, 'Distance Data');
        end;
      end;
    except
      On E: Exception do
      begin
        ShowMessage('Application error when exporting distance data: ' + E.Message);
      end;
    end;
  finally
    if Assigned(OutputMatrixDlg) then
      OutputMatrixDlg.Free;
   if Assigned(ExportOptions) then
     ExportOptions.Free;
   if Assigned(Results) then
     Results.Free;
  end;
end;

procedure TV_DistDataExplorer.DataGridRowMoved(Sender: TObject; FromIndex,ToIndex: Longint);
var
  aId: Integer = -1;
begin
  if FSearchResult >= 0 then
    aId := D_InputDistData.FOtuInfos[D_InputDistData.DispTaxaList[FSearchResult]].Id;
  UnhighlightSearchResult;
  if (FromIndex <= 0) or (ToIndex <= 0) then
    Exit;
  if FromIndex = ToIndex then
    Exit;
  D_InputDistData.FOtuInfos.Move(D_InputDistData.DispTaxaList[FromIndex-1], D_InputDistData.DispTaxaList[ToIndex-1]);
  D_InputDistData.UpdateDispTaxaList;
  if aId <> -1 then
  begin
    FSearchResult := D_InputDistData.FOtuInfos.FindById(aId);
    FSearchResult := D_InputDistData.DispTaxaList.FindFirst(FSearchResult);
  end;

  DataGrid.Invalidate;
  FHasDataViewChanged := True;
end;

procedure TV_DistDataExplorer.DataGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol: Integer = -1;
  ARow: Integer = -1;
  Rect: TRect;
begin
   if VS_DistDataExplorer.FIsResizing then
  begin
    VS_DistDataExplorer.FIsResizing := false;
    FHasDataViewChanged := True;
    Exit;
  end;

  with DataGrid do
  begin
    MouseToCell(X, Y, ACol, ARow);
    if (ACol = 0) and (ARow > 0) and (Button = mbLeft) then // toggle the status
    begin
      Rect := CellRect(ACol, ARow);
      X := X - Rect.Left-1;
      Y := Y - (Rect.Top + (Rect.Bottom - Rect.Top - CheckMark.Height) div 2);

      if (X >= 0) and (X <= CheckMark.Width) and
         (Y >= 0) and (Y <= CheckMark.Height) then
      begin
        if (VS_DistDataExplorer.NoOfSelTaxa = 2) and D_InputDistData.TaxaUsed[ARow-1] then
        begin
          ShowMessage('Sorry, at least two taxa must be selected at all times.');
          Exit;
        end;
        with D_InputDistData.FOtuInfos[D_InputDistData.DispTaxaList[ARow-1]] do
        begin
          IsUsed := not IsUsed;
          D_InputDistData.FOtuInfos.IsDirty := True;
        end;
        ApplyUpdate;
        D_InputDistData.FOtuInfos.IsDirty := False;
      end;
    end;
    inherited;
  end;
  DataGrid.Invalidate;
end;

procedure TV_DistDataExplorer.DataGridDrawCell(Sender: TObject; ACol, ARow: Longint;
  aRect: TRect; State: TGridDrawState);
var
  d: Double;
  InvalidNum: Boolean = False;
  X,Y: LongInt;
  aTextStyle: TTextStyle;
  rowLabel: String = '';
begin
  if not Visible then Exit;
  VS_DistDataExplorer.DispStr := '';
  with DataGrid.Canvas do
  begin
    Pen.Color   := clblack;
    if (ACol > DataGrid.ColCount) or (ARow > DataGrid.RowCount) then // cell is outside the range
      Exit;
    aTextStyle := TextStyle;
    if (ACol = 0) and (ARow = 0) then  // Top-Left cell
    begin
      Brush.Color := DataGrid.FixedColor;
      FillRect(aRect);
    end
    else if (ACol = 0) then  // fixed column (Leftmost); used to display Taxa Labels
    begin
      if (FSearchResult + 1) = ARow then
        Brush.Color := clAqua
      else
        Brush.Color := DataGrid.FixedColor;
      FillRect(aRect);

      X := aRect.Left + 1;               // write the check box
      Y := aRect.Top + (aRect.Bottom - aRect.Top - CheckMark.Height) div 2;
      if D_InputDistData.TaxaUsed[ARow-1] then
        Draw(X, Y, CheckMark)
      else
        Draw(X, Y, UnCheckMark);

      X := X + CheckMark.Width + 2;
      Y := aRect.Top + (aRect.Bottom - aRect.Top - TextHeight(' ')) div 2;
      aRect.Left := X;
      aTextStyle.Alignment := taLeftJustify;
      TextStyle := aTextStyle;
      rowLabel := ConstructRowLabel(aRow);
      TextRect(aRect, X, Y, rowLabel, TextStyle);
    end
    else if (ARow = 0) then  // fixed row (Top Row);
    begin
      VS_DistDataExplorer.DispStr := D_InputDistData.ColName[ACol-1];
      if (FSearchResult + 1) = aCol then
        Brush.Color := clAqua
      else
        Brush.Color := DataGrid.FixedColor;
      FillRect(aRect);
      aTextStyle.Alignment := taCenter;
      aTextStyle.Layout := tlCenter;
      TextStyle := aTextStyle;
      TextRect(aRect, 0, 0, VS_DistDataExplorer.DispStr, TextStyle);
    end
    else if (ARow > 0) and (ACol > 0) then  // if a cell with data
    begin
      d := D_InputDistData.CellContent(ARow-1, ACol-1, InvalidNum);
      Font.Color := clGray;
      if D_InputDistData.TaxaUsed[ARow-1] and D_InputDistData.TaxaUsed[ACol-1] then
        Font.Color := clBlack;

      if gdFocused in State then
      begin
        Brush.Color := clNavy;
        Font.Color  := clWhite;
      end
      else
      begin
        if (((FSearchResult + 1) = ARow) or ((FSearchResult + 1) = ACol)) and (ARow > ACol) then
          Brush.Color := clAqua
        else
          Brush.Color := clWhite;
      end;
      Brush.Style := bsSolid;
      FillRect(aRect);

      if (ARow <= ACol) then
      begin
        // nothing to do, already filled in with a white background
      end
      else
      begin
        if InValidNum then
        begin
          VS_DistDataExplorer.DispStr := 'n/c';
          Font.Color := clRed;
          aTextStyle.Alignment := taCenter;
          TextStyle := aTextStyle;
          TextRect(aRect, (aRect.Left+aRect.Right) div 2, aRect.Top+2, VS_DistDataExplorer.DispStr, TextStyle);
        end
        else if VS_DistDataExplorer.FPrecision = 0 then
        begin
          VS_DistDataExplorer.DispStr := IntToStr(Round(d));
          aTextStyle.Alignment := taRightJustify;
          TextStyle := aTextStyle;
          TextRect(aRect, aRect.Right-1, aRect.Top+2, VS_DistDataExplorer.DispStr + ' ', TextStyle);
        end
        else
        begin
          VS_DistDataExplorer.DispStr := Format('%.' + IntToStr(VS_DistDataExplorer.FPrecision) + 'f ', [d]);
          aTextStyle.Alignment := taRightJustify;
          TextStyle := aTextStyle;
          TextRect(aRect, aRect.Left + 2, aRect.Top + 2, VS_DistDataExplorer.DispStr, TextStyle);
        end;
      end;
    end;
    if (gdFocused in State) then
    begin
      if (ARow > 0) and (ACol > 0) then
        UpdateStatusBar(ARow, ACol);
    end;
  end;
end;

procedure TV_DistDataExplorer.FormCreate(Sender: TObject);
begin
  FStrBuilder := TMegaStringBuilder.Create;
  FSearchResult := -1;
  SearchPanel.Visible := False;
  FPrecision := 3;
  FScrollStarted := False;
  FSelfScrolling := False;
  FVScrollPos := 0;
  FHScrollPos := 0;
  FLastRow := 0;
  HelpContext := HC_Distance_Data_Explorer;
  FHasDataSubsetChanged := False;
  FHasDataViewChanged := False;
  CheckMark := TBitMap.Create;
  UnCheckMark := TBitMap.Create;
  ImageList2.GetBitmap(0, CheckMark);
  ImageList2.GetBitmap(1, UnCheckMark);
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+': Distance Data Explorer (' + ExtractFileName(GetActiveDataFileName) + ')';
  if not IsPrototyper then
    AddWindowToTray(Self);
  {$IFNDEF DARWIN}
  InitMainMenu;
  {$ENDIF}
  DataGrid.Invalidate;
  ImageForm.UpdateImgList(Self);
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
end;

procedure TV_DistDataExplorer.FormDestroy(Sender: TObject);
begin
  CheckMark.Free;
  UnCheckMark.Free;
  if Assigned(FStrBuilder) then
    FStrBuilder.Free;
  RemoveWindowFromTray(Self);
end;

procedure TV_DistDataExplorer.ComputeLeftOfDecimalLen;
var
  IntMaxD: Integer;
begin
  with DataGrid, Canvas do
  begin
    VS_DistDataExplorer.LeftOfDecimalLen := TextWidth('0');
    IntMaxD := Floor(D_InputDistData.MaxD/10);
    while IntMaxD <> 0 do
    begin
      IntMaxD := IntMaxD div 10;
      VS_DistDataExplorer.LeftOfDecimalLen := VS_DistDataExplorer.LeftOfDecimalLen + TextWidth('0');
    end;
  end;
end;

procedure TV_DistDataExplorer.AdjustEnabledStatus;
begin
 DispGpNamesItem.Visible := True;
 DispSortGpItem.Visible := True;
end;

procedure TV_DistDataExplorer.AssignHelpContext;
begin
  FileMenu.HelpContext            := HC_Dist_Exp_File_Menu;
  FileSelectEditTaxaGroupsItem.HelpContext  := HC_Setup_Taxa_Groups_Dlg;
  SaveToFileItem.HelpContext    := HC_Dist_Exp_File_Menu;
  FileQuitViewerItem.HelpContext:= HC_Dist_Exp_File_Menu;
  DisplayMenu.HelpContext         := HC_Dist_Exp_Display_Menu;
  DispSelTaxaItem.HelpContext   := HC_Dist_Exp_Display_Menu;
  SortTaxaSubmenu.HelpContext   := HC_Dist_Exp_Display_Menu;
  DispSortGpItem.HelpContext  := HC_Dist_Exp_Display_Menu;
  DispAsInInputFileItem.HelpContext := HC_Dist_Exp_Display_Menu;
  DispSortAlphaItem.HelpContext     := HC_Dist_Exp_Display_Menu;
  DispGpNamesItem.HelpContext := HC_Dist_Exp_Display_Menu;
  DispChangeFontItem.HelpContext := HC_Dist_Exp_Display_Menu;
  AverageMenu.HelpContext       := HC_Dist_Exp_Average_Menu;
  AvgAllItem.HelpContext      := HC_Dist_Exp_Average_Menu;
  WithinGpsAvgItem.HelpContext:= HC_Dist_Exp_Average_Menu;
  BetweenGpsAvgItem.HelpContext:=HC_Dist_Exp_Average_Menu;
  NetBetweenGpsAvgItem.HelpContext := HC_Dist_Exp_Average_Menu;
end;

procedure TV_DistDataExplorer.UpdateStatusBar(ARow, ACol: Integer);
begin
  StatusBar.SimpleText := D_InputDistData.TaxaName[ARow-1]+'-'+D_InputDistData.TaxaName[ACol-1];
end;

procedure TV_DistDataExplorer.UpdateMaxTaxaNamePixels;
var
  i: LongInt;
begin
  VS_DistDataExplorer.MaxTaxaNamePixels := 0;
  VS_DistDataExplorer.MaxTaxaNameLen:=0;
  with DataGrid, Canvas do
  begin
    for i:=0 to D_InputDistData.FNoOfTaxa -1 do
    begin
      if not D_InputDistData.FOtuInfos[i].IsUsed then
        Continue;
      if Canvas.TextWidth(D_InputDistData.OtuInfos[i].Name) > VS_DistDataExplorer.MaxTaxaNamePixels then
        VS_DistDataExplorer.MaxTaxaNamePixels := Canvas.TextWidth(D_InputDistData.FOtuInfos[i].Name);
      if Length(D_InputDistData.FOtuInfos[i].name) > VS_DistDataExplorer.MaxTaxaNameLen then
        VS_DistDataExplorer.MaxTaxaNameLen := Length(D_InputDistData.FOtuInfos[i].name);
    end;
    VS_DistDataExplorer.MaxTaxaNamePixels := VS_DistDataExplorer.MaxTaxaNamePixels + TextWidth(IntToStr(RowCount)+'. ');
  end;
end;

procedure TV_DistDataExplorer.UpdateMaxGpNamePixels;
var
  i: LongInt;
begin
  VS_DistDataExplorer.MaxGpNamePixels := 0;
  VS_DistDataExplorer.MaxGpNameLen:=0;
  with DataGrid, Canvas do
    for i:=0 to D_InputDistData.FNoOfTaxa -1 do
    begin
      if not D_InputDistData.FOtuInfos[i].IsUsed then
        Continue;
      if TextWidth(D_InputDistData.FOtuInfos[i].GpName) > VS_DistDataExplorer.MaxGpNamePixels then
        VS_DistDataExplorer.MaxGpNamePixels := TextWidth(D_InputDistData.FOtuInfos[i].GpName);
      if Length(D_InputDistData.FOtuInfos[i].GpName) > VS_DistDataExplorer.MaxGpNameLen then
        VS_DistDataExplorer.MaxGpNameLen := Length(D_InputDistData.FOtuInfos[i].GpName);
    end;
end;

procedure TV_DistDataExplorer.UpdateFirstColWidth;
var
  TempInt: Longint;
begin
  UpdateMaxTaxaNamePixels;
  UpdateMaxGpNamePixels;
  TempInt := CheckMark.Width + 4;
  TempInt := TempInt + VS_DistDataExplorer.MaxTaxaNamePixels;
  if DispGpNamesItem.Checked then
    TempInt := TempInt + VS_DistDataExplorer.MaxGpNamePixels;
  if DispGpNamesItem.Checked then
    with DataGrid do
      TempInt := TempInt + Canvas.TextWidth('{ }');
  DataGrid.ColWidths[0] := TempInt;
end;

procedure TV_DistDataExplorer.UpdateColWidths;
var
  temp: Double = -1;
  invalidNum: Boolean = False;
  aStr: String = '';
  aRow: Integer = 1;
  aCol: Integer = 1;
  aWidth: Integer = 0;
  n: Integer = -1;
begin
  n := D_InputDistData.DispTaxaList.Count;
  for aCol := 0 to n - 1 do
    for aRow := aCol + 1 to n - 2 do
    begin
      temp := D_InputDistData.CellContent(aRow, aCol, invalidNum);
      if invalidNum then
        aStr := 'n/c'
      else  if VS_DistDataExplorer.FPrecision = 0 then
        aStr := IntToStr(Round(temp))
      else
        aStr := Format(' %.' + IntToStr(VS_DistDataExplorer.FPrecision + 1) + 'f ', [temp]);
      if DataGrid.Canvas.TextWidth(aStr) > aWidth then
        aWidth := DataGrid.Canvas.TextWidth(aStr);
    end;

  DataGrid.DefaultColWidth :=  aWidth + 6;
  DataGrid.DefaultRowHeight := DataGrid.Canvas.TextHeight('W')+4;
  UpdateFirstColWidth;
  DataGrid.Invalidate;
end;

function TV_DistDataExplorer.NumTaxa: Integer;
begin
  if Assigned(D_InputDistData) and Assigned(D_InputDistData.DispTaxaList) then
    Result := D_InputDistData.DispTaxaList.Count
  else
    Result := 0;
end;

procedure TV_DistDataExplorer.Initialize;
begin
  try
    VS_DistDataExplorer.FPrecision := 4;
    FPrecision := VS_DistDataExplorer.FPrecision;
    DispSelTaxaItem.Checked := False;
    DispGpNamesItem.Checked := True;
    with DataGrid do
    begin
      RowCount := D_InputDistData.FNoOfTaxa+1;
      ColCount := D_InputDistData.FNoOfTaxa+1;
      DefaultRowHeight := Canvas.TextHeight('W')+4;
      UpdateColWidths;
    end;
    AdjustEnabledStatus;
    AssignHelpContext;

    ComputeLeftOfDecimalLen;
    AvgAllItem.Enabled := True;
    InitScrollbars(True);
  except
    On E: Exception do
    begin
      RaiseErrorMessage(HC_Unexpected_Error, E.Message+ ' in '+ClassName+' initialization function.');
      Close;
    end;
  end;
  AverageMenu.Visible := True;
end;

procedure TV_DistDataExplorer.ApplyUpdate;
begin
  if D_InputDistData.FOtuInfos.IsDirty then
  begin
    FHasDataSubsetChanged := True;
    FHasDataViewChanged := True;
    D_InputDistData.UpdateDispTaxaList;
    DataGrid.RowCount := D_InputDistData.DispTaxaList.Count+1;
    DataGrid.ColCount := D_InputDistData.DispTaxaList.Count+1;
    VS_DistDataExplorer.NoOfSelTaxa := D_InputDistData.FOtuInfos.NoOfSelOtus;
    UpdateColWidths;
    DataGrid.Invalidate;
  end;
end;

end.

