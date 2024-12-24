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

unit MutationExplorer;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ActnList, Menus, ComCtrls, Grids, ExtCtrls, StdCtrls, Buttons,
  IniPropStorage, MMyPegMessageDlg, MMyPegSearchDlg, EvoD, MMultiStageProgress,
  MutationDetailView, MyPegThreads, RegExpr, MutationDataCache,
  mgridcolumnresizer, ExcelWrite, mimageform, MegaPrivateFiles, MegaUtils;

const
  MAX_ROWS_PER_REQUEST = 1000;
  MAX_PRECISION = 10;
  MIN_PRECISION = 0;
  DEFAULT_PRECISION = 2;
  GENE_NAME_INDEX = 0;
  PEPTIDE_ID_INDEX = 1;
  GENE_PRODUCT_INDEX = 2;
  DIAGNOSE_INDEX = 3;
  GENE_SEARCH_PAGE_INDEX = 0;
  PREDICTION_DATA_PAGE_INDEX = 1;
  DELTA = 0.000001;

type

  TCurrentState = (tcsIdle, tcsWaitingForData, tcsUnitialized);
  TQueryType = (qtNONE, qtRSID, qtNMID, qtNPID, qtGENE, qtGENE_OR_FUNC, qtFUNC);

  PFileName = ^String;

  { TMutationExplorerForm }

  TMutationExplorerForm = class(TForm)
    AboutAction: TAction;
    HelpAction: TAction;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
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
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem6: TMenuItem;
    QuitAction: TAction;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    WindowsMenuItem: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    SearchBtn: TBitBtn;
    ToggleAutoWidthAction: TAction;
    ReplaceNASymbolsAction: TAction;
    CopyAction: TAction;
    PredictionsDrawGrid: TDrawGrid;
    GeneSearchDrawGrid: TDrawGrid;
    Edit1: TEdit;
    FindAction: TAction;
    FindNextAction: TAction;
    ImportQueryDataFileAction: TAction;
    FilterAction: TAction;
    ExportExcelAction: TAction;
    ExportCsvAction: TAction;
    Panel1: TPanel;
    ShowSDEAction: TAction;
    DecreasePrecisionAction: TAction;
    IncreasePrecisionAction: TAction;
    ClearTableAction: TAction;
    ProgressWheelTimer: TTimer;
    ResponseQueueTimer: TTimer;
    ResultsQueueTimeoutTimer: TTimer;
    GeneSearchTimeoutTimer: TTimer;
    GeneSearchTimer: TTimer;
    ToggleDetailsOnTopAction: TAction;
    ShowToolbarAction: TAction;
    SelectAllAction: TAction;
    BestFitColsAction: TAction;
    GeneSearchAction: TAction;
    ShowDetailViewAction: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    PageControl: TPageControl;
    StatusBar1: TStatusBar;
    GeneSearchPage: TTabSheet;
    PredictionDataPage: TTabSheet;
    ToolBar1: TToolBar;
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
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure FormActivate(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure GeneSearchDrawGridClick(Sender: TObject);
    procedure GeneSearchDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure GeneSearchDrawGridMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GeneSearchDrawGridMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GeneSearchDrawGridSelectCell(Sender: TObject; aCol,
      aRow: Integer; var CanSelect: Boolean);
    procedure HelpActionExecute(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PredictionsDrawGridAfterSelection(Sender: TObject; aCol,
      aRow: Integer);
    procedure PredictionsDrawGridDblClick(Sender: TObject);
    procedure PredictionsDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure PredictionsDrawGridSelectCell(Sender: TObject; aCol,
      aRow: Integer; var CanSelect: Boolean);
    procedure ProgressWheelTimerTimer(Sender: TObject);
    procedure QuitActionExecute(Sender: TObject);
    procedure ResponseQueueTimerTimer(Sender: TObject);
    procedure ResultsQueueTimeOutTimerTimer(Sender: TObject);
    procedure GeneSearchTimeOutTimerTimer(Sender: TObject);
    procedure GeneSearchTimerTimer(Sender: TObject);
    procedure GeneSearchPageShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure KeepDetailListOnTop1Click(Sender: TObject);
    procedure ShowToolbar1Click(Sender: TObject);
    procedure ReplaceNASymbolsActionExecute(Sender: TObject);
    procedure DecreasePrecisionActionExecute(Sender: TObject);
    procedure IncreasePrecisionActionExecute(Sender: TObject);
    procedure ExportExcelActionExecute(Sender: TObject);
    procedure ExportCSVActionExecute(Sender: TObject);
    procedure FilterActionExecute(Sender: TObject);
    procedure ImportQueryDataFileActionExecute(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure ToggleDetailsOnTopActionExecute(Sender: TObject);
    procedure FindActionExecute(Sender: TObject);
    procedure FindNextActionExecute(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure SelectAllActionExecute(Sender: TObject);
    procedure ClearTableActionExecute(Sender: TObject);
    procedure BestFitColsActionExecute(Sender: TObject);
    procedure GeneSearchActionExecute(Sender: TObject);
    procedure ProcessGeneQueryDlgRequest(Filename: String);
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ToggleAutoWidthActionExecute(Sender: TObject);
    procedure AboutItemClick(Sender: TObject);
    procedure ShowDetailViewActionExecute(Sender: TObject);
    procedure SearchBtnClick(Sender: TObject);
    procedure GeneSearchTreeListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure SubmitCoordinatesActionExecute(Sender: TObject);
    procedure ShowSDEActionExecute(Sender: TObject);
    procedure ActionList1Update(aAction: TBasicAction; var Handled: Boolean);
    procedure FormDeactivate(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);

  private
    FSearchRow: Integer;
    FSearchCol: Integer;
    FGridColor1: TColor;
    FGridColor2: TColor;
    FGridFocusedColor: TColor;
    FGridSelectedColor: TColor;
    FFileList: TStringList;
    FMasterViewIsCreated: Boolean;
    ResponseList: TStringList;
    FNumRequestsNeeded: Integer;
    FNumRequestsCompleted: Integer;
    FSourceFile: String;
    FShowNAStrings: Boolean;
    FPrecision: Integer;
    FNumQueries: Integer;
    FCurrentState: TCurrentState;
    FShowErrorMessages: Boolean;
    FCustomErrorMessage: String;
    FindDlg: TMyPegSearchDlg;
    FSearchTerm: String;
    FTempFileList: TStringList;
    FDetailsCanUpdate: Boolean;
    FJumpToNode: Boolean;
    FProgressWheel: array[0..3] of String;
    FProgressWheelIndex: Integer;
    FProgressMessage: String;
    FDetailsOnTop: Boolean;
    FGeneSearchErrorMsg: String;
    FMSProgress: TMultiStageProgressForm;
    FDownloadErrorMsg: String;
    FIsDangerousQuery: Boolean; // if the user manually enters a protein id and aa position instead of using the seq data explorer, then the query may be invalid
    FRefSeqIdRegex: TRegExpr;
    FIntRegex: TRegExpr;
    FAARegex: TRegExpr;
    FTokens: TStringList;
    FDiagnoses: TList;
    FGeneEntries: TList;
    FGridColumnSizer: TGridColumnResizer;
    function RowIsInRect(aRow: Integer; aRect: TRect): Boolean;
    procedure FocusPredictionsGridCell(aRow, aCol: Integer);
    procedure InitMainMenu;
    procedure InitMutationDetailViewForm;
    function BuildExcelExport: TExcelWrite;
    function PredictionsGridHeaderString(aCol: Integer): String;
    function PredictionsGridDiagnosisString(diagnosis: TMutationDiagnosis; aCol: Integer): String;
    function PredictionsGridAlignment(aCol: Integer): TAlignment;
    function SelectedMutation: TMutationDiagnosis;
    procedure UpdateGeneSearchDrawGridColSizes;
    procedure UpdatePredictionsGridColSizes;
    function LongestGeneEntryStrings: TStringList;
    function LongestMutationDataStrings: TStringList;
    procedure InitDrawGrids;
    procedure ClearDiagnoses;
    procedure ClearGeneEntries;
    procedure InitRegexes;
    function ValidateInputFile(Filename: String): Boolean;
    function IsValidLine(Line: String): Boolean;
    procedure RefreshTree;
    procedure CreateMasterSourceView;
    procedure AppendMasterSourceView;
    procedure DownloadErrorNotify(Msg: String; AThread: TThread);
    procedure DoDownloadErrorNotify;
    function UpdateCachingProgress(Progress: Integer): Boolean;
    procedure CachingCompletedEvent(FL: TStringList; HasBadLines: Boolean);
    procedure CachingCancelledEvent(FL: TstringList);
    procedure UpdateStatusBar(ActionPanel: String);
    procedure UpdateActionList;
    procedure DisableActionsDuringQuery;
    procedure ReEnableActionsAfterQuery;
    procedure StartGeneSearchTimers;
    procedure StopGeneSearchTimers;
    procedure ErrorMsgCallBack(Msg: String; AThread: TThread);
    procedure UpdateErrorMsgLabel(Msg: String);
    procedure DoUpdateGeneSearchErrorMsgLabel;
    procedure UpdateGeneSearchView(SearchResult: TEvoDSearchResult);
    function LaunchQueryThread: Boolean;
    function ValidateResponse: Boolean;
    function TryMakeNewFloat(Input: String; var IsSuccess: Boolean): Double;
    function TryMakeNewInt(Input: String; var IsSuccess: Boolean): Integer;
    function ConvertFieldListToArray(Input: TStringlist; var Output: TFieldArray): Boolean;
    function CompareNatural(First, Second: String): Integer;
    procedure MyPegThreadDone(aThread: TObject);
  public
    function NumMutations: Int64;
    procedure ImportDataQueryFileDirect(QueryFile: String);
  end;

var
  MutationExplorerForm: TMutationExplorerForm;

implementation

uses
  Math, MV_SeqDataExplorer, Mega_Main, MAboutBox, OutputUtils,
  MegaVerConsts, StringUtils, syncobjs, variants,
  mhelpfiles, ContextHelp_HC, Clipbrd, mshortcutshelper;

{$R *.lfm}

procedure TMutationExplorerForm.CreateMasterSourceView;
var
  i: Integer;
  aDiagnosis: TMutationDiagnosis;
  FieldsList : TStringList;
  Fields: TFieldArray;
begin
  SetLength(Fields, 0);
  FMSProgress.UpdateCurrentOpProgress(Trunc(FNumRequestsCompleted / FNumRequestsNeeded * 100));
  FieldsList := TStringList.Create;
  inc(FNumQueries);

  { Here we validate the result for the case where the user has manually entered
    coordinate information. This is in contrast to using the Sequence Data Explorer
    where we already have the alignment and they select an amino acid position}
  if FIsDangerousQuery then
  begin
    FIsDangerousQuery := False;
    if Assigned(ResponseList) then
    begin
      FieldsList.CommaText := ResponseList[0];
      Assert(CI_MRNAID <= High(Fields), 'index out of bounds error');
      if SameText(Fields[CI_MRNAID], 'NA') then
      begin
        ReEnableActionsAfterQuery;
        UpdateStatusBar('Query returned no results ');
        StatusBar1.Panels[1].Text := '  ' + Format('Displaying %u prediction(s)', [NumMutations]) + '   ';
        FMSProgress.StopProgress;
        FMSProgress.Hide;
        ShowMessage('No results returned for the given coordinates');
        Exit;
      end;
    end
    else
    begin
      ReEnableActionsAfterQuery;
      UpdateStatusBar('Query returned no results ');
      StatusBar1.Panels[1].Text := '  ' + Format('Displaying %u prediction(s)', [NumMutations]) + '   ';
      FMSProgress.StopProgress;
      FMSProgress.Hide;
      ShowMessage('Oh no! The MEGA-MD server did not return a response');
      Exit;
    end;
  end;

  try
    try
      BeginFormUpdate;
      if Assigned(ResponseList) then
      begin
        for i := 0 to ResponseList.Count - 1 do
        begin
          FieldsList.CommaText := ResponseList[i] + ',';
          if ConvertFieldListToArray(FieldsList, Fields) then
          begin
            aDiagnosis := TMutationDiagnosis.Create(Fields);
            FDiagnoses.Add(aDiagnosis);
          end
          else
            ShowMessage('Oops! An error occured when loading query results into the table');
        end;
      end;
      //MutationDetailViewForm.Height := Self.Height;
      StatusBar1.Panels[1].Text := '  ' + Format('Displaying %u prediction(s)', [NumMutations]) + '   ';
      PredictionsDrawGrid.RowCount := NumMutations + 1;
      if NumMutations > 0 then
      begin
        PredictionsDrawGrid.Selection := Rect(1, 1, 1, 1);
        MutationDetailViewForm.UpdateDetailView(SelectedMutation);
      end;
      UpdatePredictionsGridColSizes;
      PredictionsDrawGrid.Invalidate;
      if FNumRequestsCompleted = FNumRequestsNeeded then
      begin
        FMSProgress.StopProgress;
        FMSProgress.Hide;
        Self.Show;
        MutationDetailViewForm.Show;
        FProgressMessage := EmptyStr;
        UpdateStatusBar('Request completed');
        ImportQueryDataFileAction.Enabled := True;
        if Assigned(MutationDetailViewForm) then
          MutationDetailViewForm.SetActionsEnabled(True);
      end;
    except
      on E:Exception do
      begin
         ShowMessage('Error in MutationExplorer.CreateMasterView: ' + E.Message);
      end;
    end;
  finally
    EndFormUpdate;
    ReEnableActionsAfterQuery;
    if Assigned(FieldsList) then
      FieldsList.Free;
  end;
end;

procedure TMutationExplorerForm.PageControlChange(Sender: TObject);
var
  Msg: String;
begin
  if PageControl.TabIndex = GENE_SEARCH_PAGE_INDEX then
  begin
    if Self.Visible then
      Edit1.SetFocus;
    Msg := 'Displaying ' + IntToStr(GeneSearchDrawGrid.RowCount - 1) + ' query result(s)' + '   ';
    StatusBar1.Panels[1].Text := Msg;
  end
  else
  begin
    Msg := 'Displaying ' + IntToStr(PredictionsDrawGrid.RowCount - 1) + ' prediction(s)' + '   ';
    StatusBar1.Panels[1].Text := Msg;
  end;
  UpdateActionList;
end;

procedure TMutationExplorerForm.PredictionsDrawGridAfterSelection(
  Sender: TObject; aCol, aRow: Integer);
begin
  PredictionsDrawGrid.Invalidate;
  if not FDetailsCanUpdate then
    Exit;
  try
    if Assigned(MutationDetailViewForm) then
      MutationDetailViewForm.UpdateDetailView(SelectedMutation);
  except
    on E:Exception do
     ShowMessage('Error in MutationExplorer.SelectCell: ' + E.Message);
  end;
end;

procedure TMutationExplorerForm.PredictionsDrawGridDblClick(Sender: TObject);
begin
  ShowDetailViewActionExecute(Sender);
end;

procedure TMutationExplorerForm.PredictionsDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  aText: String;
  x, y: Integer;
  aTextStyle: TTextStyle;
  diagnosis: TMutationDiagnosis;
  padding: String;
begin
  if not Visible then Exit;
  padding := '  ';
  try
    BeginFormUpdate;
    with PredictionsDrawGrid.Canvas do
    begin
      aTextStyle := PredictionsDrawGrid.Canvas.TextStyle;
      aTextStyle.SystemFont := False;
      aTextStyle.Layout := tlCenter;
      x := aRect.Left + 2;
      y := aRect.Top + 2;
      Font.Style := [];
      Brush.Style := bsSolid;

      if aCol = 0 then
      begin
        aTextStyle.Alignment := taCenter;
        Brush.Color := clBtnFace;
        FillRect(aRect);
        if PredictionsDrawGrid.Selection.Top = aRow then
        begin
          Font.Color := clMaroon;
          aText := '>';
          Font.Style := [fsBold];
          TextRect(aRect, x, y, aText, aTextStyle);
        end;
      end
      else if aRow = 0 then
      begin
        Brush.Color := clBtnFace;
        FillRect(aRect);
        aText := PredictionsGridHeaderString(aCol);
        aTextStyle.Alignment := taCenter;
        TextRect(aRect, x, y, aText, aTextStyle);
      end
      else
      begin
        if RowIsInRect(aRow, PredictionsDrawGrid.Selection) and (PredictionsDrawGrid.Selection.Left <> 0) then
        begin
          if aRow = PredictionsDrawGrid.Selection.Top then
            Brush.Color := FGridFocusedColor
          else
            Brush.Color := FGridSelectedColor;
          Font.Color := clWhite;
          if PredictionsDrawGrid.Selection.Left = aCol then
            Font.Style := [fsBold];
        end
        else if odd(aRow) then
          Brush.Color := FGridColor1
        else
          Brush.Color := FGridColor2;
        FillRect(aRect);
        diagnosis := TMutationDiagnosis(FDiagnoses[aRow - 1]);
        aText := PredictionsGridDiagnosisString(diagnosis, aCol);
        aTextStyle.Alignment := PredictionsGridAlignment(aCol);
        if aTextStyle.Alignment = taRightJustify then
          aText := aText + padding;
        TextRect(aRect, x, y, aText, aTextStyle);
      end;
    end;
  finally
    EndFormUpdate;
  end;
end;

procedure TMutationExplorerForm.PredictionsDrawGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  //PredictionsDrawGrid.Invalidate;
  //if not FDetailsCanUpdate then
  //  Exit;
  //try
  //  if Assigned(MutationDetailViewForm) then
  //    MutationDetailViewForm.UpdateDetailView(SelectedMutation);
  //except
  //  on E:Exception do
  //   ShowMessage('Error in MutationExplorer.SelectCell: ' + E.Message);
  //end;
end;

procedure TMutationExplorerForm.GeneSearchDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  aEntry: TGeneEntry;
  x, y: Integer;
  aText: String;
  aTextStyle: TTextStyle;
begin
  if not Visible then Exit;
  aTextStyle := GeneSearchDrawGrid.Canvas.TextStyle;
  aTextStyle.SystemFont := False;
  aTextStyle.Layout := tlCenter;
  x := aRect.Left + 2;
  y := aRect.Top + 2;

  with GeneSearchDrawGrid.Canvas do
  begin
    Font.Style := [];
    Font.Color := clBlack;
    if aRow = 0 then
    begin
      Font.Style := Font.Style + [fsBold];
      aTextStyle.Alignment := taCenter;
      if aCol = 0 then
        aText := 'Gene Name'
      else if aCol = 1 then
        aText := 'Peptide ID'
      else if aCol = 2 then
        aText := 'Gene Product';
      Brush.Color := clBtnFace;
      Brush.Style := bsSolid;
      FillRect(aRect);
      if aCol <= 2 then
        TextRect(aRect, x, y, aText, aTextStyle);
    end
    else
    begin
      if (GeneSearchDrawGrid.Selection.Top = aRow) and (GeneSearchDrawGrid.Selection.Left <> 3) then
      begin
        Brush.Color := FGridFocusedColor;
        Font.Color := clWhite;
        if GeneSearchDrawGrid.Selection.Left = aCol then
          Font.Style := [fsBold];
      end
      else if odd(aRow) then
        Brush.Color := FGridColor2
      else
        Brush.Color := FGridColor1;
      FillRect(aRect);
      if (aRow - 1) >= FGeneEntries.Count then
        Exit;
      aEntry := TGeneEntry(FGeneEntries[aRow - 1]);
      if aCol = 0 then
      begin
        aText := aEntry.GeneName;
      end
      else if aCol = 1 then
      begin
        aText := aEntry.PeptideID;
      end
      else if aCol = 2 then
      begin
        aText := aEntry.Product;
      end
      else if aCol = 3 then
      begin
        aText := 'Diagnose Variant';
        Font.Color := clBlue;
        Font.Style := Font.Style + [fsUnderline];
      end;
      aTextStyle.Alignment := taLeftJustify;
      if aCol <= 3 then
        TextRect(aRect, x, y, aText, aTextStyle);
    end;
  end;
end;

procedure TMutationExplorerForm.GeneSearchDrawGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aCol: Integer = -1;
  aRow: Integer = -1;
  aEntry: TGeneEntry;
begin
  GeneSearchDrawGrid.MouseToCell(X, Y, aCol, aRow);
  if (aCol = 3) and (aRow > 0) and ((aRow - 1) <= FGeneEntries.Count) then
  begin
    aEntry := TGeneEntry(FGeneEntries[aRow - 1]);

  end;
end;

procedure TMutationExplorerForm.GeneSearchDrawGridClick(Sender: TObject);
var
  aEntry: TGeneEntry;
  aCol: Integer = -1;
  aRow: Integer = -1;
  screenCoords: TPoint;
begin
  screenCoords := GeneSearchDrawGrid.ScreenToClient(Mouse.CursorPos);
  GeneSearchDrawGrid.MouseToCell(screenCoords.x, screenCoords.y, aCol, aRow);
  if (aCol <> DIAGNOSE_INDEX) or (aRow < 1) or (aRow > FGeneEntries.Count)  then
    Exit;

  try
    if not Assigned(MutationDetailViewForm) then
      MutationDetailViewForm := TMutationDetailViewForm.Create(MutationExplorerForm);

    aEntry := TGeneEntry(FGeneEntries[aRow - 1]);
    MutationDetailViewForm.ActivateAlignment(aEntry.MRnaID, aEntry.PeptideId);
    UpdateActionList;
    Screen.Cursor := crDefault;
  except
    on E: Exception do
      ShowMessage('An error occurred when activating the specified alignment: ' + E.Message);
  end;
end;

function TMutationExplorerForm.FormHelp(Command: Word; Data: PtrInt;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := False;
  Result := True;
  ShowContextSensitiveHelp(HelpKeyword);
end;

procedure TMutationExplorerForm.FormActivate(Sender: TObject);
begin
  MegaForm.UpdateMenuForPinupItems(WindowsMenuItem, Self);
end;

procedure TMutationExplorerForm.GeneSearchDrawGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  aRow: Integer = -1;
  aCol: Integer = -1;
begin
  GeneSearchDrawGrid.MouseToCell(X, Y, aCol, aRow);
  if (aCol = 3) and (aRow > 0) and (aRow < GeneSearchDrawGrid.RowCount) then
    GeneSearchDrawGrid.Cursor:= crHandPoint
  else
    GeneSearchDrawGrid.Cursor := crDefault;
end;

procedure TMutationExplorerForm.GeneSearchDrawGridSelectCell(Sender: TObject;
  aCol, aRow: Integer; var CanSelect: Boolean);
begin
  Invalidate;
end;

procedure TMutationExplorerForm.HelpActionExecute(Sender: TObject);
begin
  ShowContextSensitiveHelp(HelpKeyword);
end;

procedure TMutationExplorerForm.ProcessGeneQueryDlgRequest(Filename: String);
var
  CachingThread: TDataCacheThread;
begin
  try
    InitMutationDetailViewForm;
    CachingThread := TDataCacheThread.Create(True);
    CachingThread.SourceFile := FileName;
    CachingThread.CacheUpdateEvent := @UpdateCachingProgress;
    CachingThread.CachingCompleteEvent := @CachingCompletedEvent;
    CachingThread.FreeOnTerminate := True;
    CachingThread.Start;
  except
    on E: Exception do
      ShowMessage('Error encountered during POST: ' + E.Message);
  end;
end;

procedure TMutationExplorerForm.ProgressWheelTimerTimer(Sender: TObject);
begin
  if FProgressWheelIndex = 3 then
    FProgressWheelIndex := 0
  else
    inc(FProgressWheelindex);
  StatusBar1.Panels[0].Text := FProgressMessage + FProgressWheel[FProgressWheelIndex];
end;

procedure TMutationExplorerForm.QuitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TMutationExplorerForm.ResponseQueueTimerTimer(Sender: TObject);
var
  TempList: TStringList = nil;
begin
  try
    ResultsCriticalSection.Acquire;
    if ResultsQueue.Count > 0 then
      ResultsQueueTimeOutTimer.Enabled := False;

    while ResultsQueue.Count > 0 do
    begin
      TempList := TStringList(ResultsQueue[0]);
      if not Assigned(ResponseList) then
        ResponseList := TStringList.Create;
      ResponseList.Clear;
      ResponseList.AddStrings(TempList);
      ResultsQueue.Delete(0);
      AppendMasterSourceView;
    end;
  finally
    ResultsCriticalSection.Release;
    if FNumRequestsNeeded = FNumRequestsCompleted then
    begin
      FProgressMessage := EmptyStr;
      FIsDangerousQuery := False;
    end;
  end;
end;


procedure TMutationExplorerForm.ResultsQueueTimeOutTimerTimer(Sender: TObject);
begin
  try
    MyPegCriticalSection.Acquire;
    ThreadsCancelled := True;
  finally
    MyPegCriticalSection.Release;
  end;

  FMSProgress.StopProgress;
  FMSProgress.Hide;
  ShowMessage('Predictions retrieval timed out. The server has not responded');
end;

procedure TMutationExplorerForm.GeneSearchPageShow(Sender: TObject);
begin
  Edit1.SetFocus;
end;

procedure TMutationExplorerForm.GeneSearchTreeListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

procedure TMutationExplorerForm.GeneSearchTimeOutTimerTimer(Sender: TObject);
begin
  try
    GeneSearchCS.Acquire;
    GeneSearchCancelled := True;
  finally
    GeneSearchCS.Release;
  end;

  StopGeneSearchTimers;
  FMSProgress.StopProgress;
  FMSProgress.Hide;
  ShowMessage('Gene search timed Out: the server has not responded');
end;

procedure TMutationExplorerForm.GeneSearchTimerTimer(Sender: TObject);
begin
  try
    GeneSearchCS.Acquire;
    if GeneQueryResult.Count > 0 then
    begin
      try
        FMSProgress.StopProgress;
        FMSProgress.Hide;
        UpdateGeneSearchView(TEvoDSearchResult(GeneQueryResult[0]));
      finally
        GeneQueryResult.Clear;
        GeneSearchCancelled := False;
      end;
    end;
  finally
    GeneSearchCS.Release;
  end;
end;

procedure TMutationExplorerForm.ToggleAutoWidthActionExecute(Sender: TObject);
begin
  ToggleAutoWidthAction.Checked := not ToggleAutoWidthAction.Checked;
  if ToggleAutoWidthAction.Checked then
    BestFitColsActionExecute(Self);
end;

procedure TMutationExplorerForm.ToggleDetailsOnTopActionExecute(Sender: TObject);
begin
  ToggleDetailsOnTopAction.Checked := not ToggleDetailsOnTopAction.Checked;
  if ToggleDetailsOnTopAction.Checked then
    MutationDetailViewForm.FormStyle := fsStayOnTop
  else
  begin
    MutationDetailViewForm.FormStyle := fsNormal;
    Self.BringToFront;
  end;
end;

procedure TMutationExplorerForm.DecreasePrecisionActionExecute(Sender: TObject);
begin
  if FPrecision > MIN_PRECISION then
    dec(FPrecision);
  if FPrecision = MIN_PRECISION then
    DecreasePrecisionAction.Enabled := False
  else
    DecreasePrecisionAction.Enabled := True;
  IncreasePrecisionAction.Enabled := True;
  MutationDetailViewForm.Precision := FPrecision;
  RefreshTree;
  if Assigned(MutationDetailViewForm) then
    MutationDetailViewForm.UpdateDetailView(SelectedMutation);
end;

procedure TMutationExplorerForm.GeneSearchActionExecute(Sender: TObject);
begin
  if not GeneSearchPage.TabVisible then
    GeneSearchPage.TabVisible := True;

  PageControl.TabIndex := GENE_SEARCH_PAGE_INDEX;
  UpdateActionList;
end;

procedure TMutationExplorerForm.DisableActionsDuringQuery;
begin
  DecreasePrecisionAction.Enabled := False;
  ReplaceNASymbolsAction.Enabled := False;
  IncreasePrecisionAction.Enabled := False;
  ExportExcelAction.Enabled := False;
  ExportCSVAction.Enabled := False;
  FilterAction.Enabled := False;
  ClearTableAction.Enabled := False;
  ImportQueryDataFileAction.Enabled := False;
  GeneSearchAction.Enabled := False;
  BestFitColsAction.Enabled := False;
  ToggleAutoWidthAction.Enabled := False;
  if PredictionsDrawGrid.Selection.Top > 0 then
    CopyAction.Enabled := True
  else
    CopyAction.Enabled := False;
  SelectAllAction.Enabled := False;
end;

procedure TMutationExplorerForm.DoDownloadErrorNotify;
begin
  FMSProgress.StopProgress;
  FMSProgress.Hide;
  try
    MyPegCriticalSection.Acquire;
    ThreadsCancelled := True;
  finally
    MyPegCriticalSection.Release;
  end;

  try
    ResultsCriticalSection.Acquire;
    ResultsQueue.Clear;
  finally
    ResultsCriticalSection.Release;
  end;

  ShowMessage('An error has occurred when retrieving prediction results from the server: ' + FDownloadErrorMsg);
end;

procedure TMutationExplorerForm.DoUpdateGeneSearchErrorMsgLabel;
begin
  ShowMessage(FGeneSearchErrorMsg);
  StatusBar1.Panels[0].Text := 'Idle';
end;

procedure TMutationExplorerForm.DownloadErrorNotify(Msg: String; AThread: TThread);
begin
  FDownloadErrorMsg := Msg;
  TThread.Synchronize(AThread, @DoDownloadErrorNotify);
end;

procedure TMutationExplorerForm.AboutItemClick(Sender: TObject);
var
  AboutBox: TAboutBox;
begin
    AboutBox := TAboutBox.Create(Self);
    AboutBox.Caption := 'About MEGA-MD (Build#: ' + VER_MEGA_BUILD + ')';
    AboutBox.ShowModal;
    AboutBox.Free;
end;

procedure TMutationExplorerForm.ActionList1Update(aAction: TBasicAction;
  var Handled: Boolean);
begin
  if MegaForm.HasActiveData or MutationDetailViewForm.DidActivateData then
    ShowSDEAction.Enabled := True
  else
    ShowSDEAction.Enabled := False;
end;

procedure TMutationExplorerForm.AppendMasterSourceView;
var
  aDiagnosis: TMutationDiagnosis;
  FieldsList : TStringList;
  Fields: TFieldArray;
  i: Integer;
  DoStop: Boolean;
  newRow: Integer = -1;
begin
  SetLength(Fields, 0);
  DoStop := False;
  try
    MyPegCriticalSection.Acquire;
    if ThreadsCancelled then
      DoStop := True;
  finally
    MyPegCriticalSection.Release;
  end;
  if DoStop then
    Exit;

  try
    if not ValidateResponse then
    begin
      FMSProgress.StopProgress;
      FMSProgress.Hide;
      ReEnableActionsAfterQuery;
      try
        MyPegCriticalSection.Acquire;
        ThreadsCancelled := True;
      finally
        MyPegCriticalSection.Release;
      end;
      if Assigned(MutationDetailViewForm) then
        MutationDetailViewForm.SetActionsEnabled(True);
      if FShowErrorMessages then
      begin
        if ResponseList.Count > 0 then
        begin
          if Trim(ResponseList[0]) <> EmptyStr then
            ShowMessage(ResponseList[0])
          else
            ShowMessage('Oops! The MEGA-MD server did not return a valid response. ' + ResponseList.Text);
        end
        else
          ShowMessage('Oops! The MEGA-MD server did not return a valid response.');
      end;
      FShowErrorMessages := False; // once is enough because even though we set threads to be terminate, they may not stop immediately
      Exit;
    end;
  except
    on E:Exception do
    begin
      if FShowErrorMessages then
        ShowMessage('Error: ' + E.Message);
    end;
  end;

  inc(FNumRequestsCompleted);
  if (not FMSProgress.UpdateCurrentOpProgress(Trunc(FNumRequestsCompleted / FNumRequestsNeeded * 100))) or
     (not FMSProgress.UpdateOverallProgress(25 + Trunc(FNumRequestsCompleted / FNumRequestsNeeded * 75))) then
  begin
    try
      MyPegCriticalSection.Acquire;
      ThreadsCancelled := True;
    finally
      MyPegCriticalSection.Release;
    end;


    FProgressMessage := EmptyStr;
    if Assigned(MutationDetailViewForm) then
      MutationDetailViewForm.SetActionsEnabled(True);

    ReEnableActionsAfterQuery;
    UpdateStatusBar('Cancelled - cleaned up thread pool');
    StatusBar1.Panels[1].Text := '  ' + Format('Displaying %u prediction(s)', [NumMutations]) + '   ';
    FMSProgress.StopProgress;
    FMSProgress.Hide;
    DoStop := True;
  end;

  if DoStop then
    Exit;

  FieldsList := TStringList.Create;

  { Here we validate the result for the case where the user has manually entered
    coordinate information. This is in contrast to using the Sequence Data Explorer
    where we already have the alignment and they select an amino acid position}
  if FIsDangerousQuery then
  begin
    FIsDangerousQuery := False;
    if Assigned(ResponseList) then
    begin
      FieldsList.CommaText := ResponseList[0];
      if SameText(FieldsList[CI_MRNAID], 'NA') then
      begin
        ReEnableActionsAfterQuery;
        UpdateStatusBar('Query returned no results ');
        StatusBar1.Panels[1].Text := '  ' + Format('Displaying %u prediction(s)', [NumMutations]) + '   ';
        FMSProgress.StopProgress;
        FMSProgress.Hide;
        ShowMessage('No results returned for the given coordinates');
        Exit;
      end;
    end
    else
    begin
      ReEnableActionsAfterQuery;
      UpdateStatusBar('Query returned no results ');
      StatusBar1.Panels[1].Text := '  ' + Format('Displaying %u prediction(s)', [NumMutations]) + '   ';
      FMSProgress.StopProgress;
      FMSProgress.Hide;
      ShowMessage('Oh no! The MEGA-MD server did not return a response');
      Exit;
    end;
  end;

  try
    try
      PageControl.TabIndex := PREDICTION_DATA_PAGE_INDEX;
      if not FMasterViewIsCreated then
      begin
        CreateMasterSourceView({ResponseList});
        FMasterViewIsCreated := True;
      end
      else
      begin
        newRow := PredictionsDrawGrid.RowCount;
        if Assigned(ResponseList) then
        begin
          for i := 0 to ResponseList.Count - 1 do
            begin
              FieldsList.CommaText := ResponseList[i];
              if ConvertFieldListToArray(FieldsList, Fields) then
              begin
                aDiagnosis := TMutationDiagnosis.Create(Fields);
                FDiagnoses.Add(aDiagnosis);
              end
            end;
          PredictionsDrawGrid.RowCount := NumMutations + 1;
          PredictionsDrawGrid.Row := newRow;
        end
        else
          ShowMessage('Oh no! The MEGA-MD server did not return any results');

        StatusBar1.Panels[1].Text := '  ' + Format('Displaying %u prediction(s)', [NumMutations]) + '   ';
        if FNumQueries = 0 then
        begin
          if PredictionsDrawGrid.RowCount > 1 then
            PredictionsDrawGrid.Selection := Rect(1, 2, 1, 2);
          MutationDetailViewForm.Show;
        end;
        inc(FNumQueries);
        if FNumRequestsCompleted = FNumRequestsNeeded then
        begin
          FProgressMessage := EmptyStr;
          UpdateStatusBar('Request completed');
          ImportQueryDataFileAction.Enabled := True;
          if Assigned(MutationDetailViewForm) then
            MutationDetailViewForm.SetActionsEnabled(True);
          if FJumpToNode then
          begin
            //if ResponseList.Count = 1 then
            //  TreeList.FocusedNode := TreeList.Items[TreeList.Count - 1]
            //else
            //  TreeList.FocusedNode := TreeList.Items[TreeList.Count - ResponseList.Count];
            //TreeList.FocusedNode.MakeVisible;
          end;
          Self.Show;
          MutationDetailViewForm.Show;
        end;
        Application.ProcessMessages;
      end;
    except
      on E: Exception do
      begin
        FMSProgress.StopProgress;
        FMSProgress.Hide;
        ReEnableActionsAfterQuery;
        ShowMessage('Oops. An error occured when updating the tree view: ' + E.Message);
      end;
    end;
  finally
    if Assigned(FieldsList) then
      FieldsList.Free;
    SetLength(Fields, 0);
    if FNumRequestsCompleted = FNumRequestsNeeded then
    begin
      ReEnableActionsAfterQuery;
      FIsDangerousQuery := False;
      FMSProgress.StopProgress;
      FMSProgress.Hide;
      FProgressMessage := EmptyStr;
    end;
  end;
end;

procedure TMutationExplorerForm.BestFitColsActionExecute(Sender: TObject);
begin
  UpdatePredictionsGridColSizes;
end;

procedure TMutationExplorerForm.Button1Click(Sender: TObject);
var
  Progress: TMultiStageProgressForm;
begin
  Progress := TMultiStageProgressForm.Create(Self);
  Progress.SetToMarqueeMode;
  Progress.Show;
end;

procedure TMutationExplorerForm.CachingCancelledEvent(FL: TstringList);
var
  NumFiles: Integer;
  Progress: Integer;
begin
  try
    FMSProgress.UpdateCurrentOpStatus('Cleaning up cached data');
    FMSProgress.UpdateCurrentOpProgress(0);
    DisableActionsDuringQuery;
    NumFiles := FL.Count;
    FProgressMessage := 'Cleaning up cached data';
    while FL.Count > 0 do
    begin
      if FileExists(FL[0]) then
      begin
        try
          DeleteFile(FL[0]);
          FL.Delete(0);
          Progress := Trunc((1 - FL.Count / NumFiles) * 100);
          FMSProgress.UpdateCurrentOpProgress(Progress);
        except
        {$IFDEF DEBUG}
        on E:Exception do
          ShowMessage('Error in MutationExplorer.CachingCancelledEvent: ' + E.Message);
        {$ENDIF}
          // fugidaboudit
        end;
      end;
    end;
  finally
    FMSProgress.StopProgress;
    FMSProgress.Hide;
    ReEnableActionsAfterQuery;
    FProgressMessage := EmptyStr;
    UpdateStatusBar('Cleaned up cached data');
  end;
end;

procedure TMutationExplorerForm.CachingCompletedEvent(FL: TStringList; HasBadLines: Boolean);
var
  MyPegMsgDlg: TMyPegMessageDlg;
  i: Integer;
  TempLineNum: String;
  TempStr: String;
begin
  if HasBadLines then
  begin
    FMSProgress.StopProgress;
    FMSProgress.Hide;
    try
      MyPegMsgDlg := TMyPegMessageDlg.Create(Self);
      if FL[0] = 'Error' then
      begin
        MyPegMsgDlg.Memo1.Lines.Append('An error occurred when validating the input file:');
        MyPegMsgDlg.Memo1.Lines.Append(FL[1]);
      end
      else
      begin
        MyPegMsgDlg.Memo1.Lines.Append('Validation of the input file failed.');
        if FL.Count < MAX_BAD_LINES_TO_SHOW then
          MyPegMsgDlg.Memo1.Lines.Append('The following lines are not formatted correctly:')
        else
          MyPegMsgDlg.Memo1.Lines.Append('The following lines are not formatted correctly (only first ' + IntToStr(MAX_BAD_LINES_TO_SHOW) + ' shown)');

        for i := 0 to Min(FL.Count - 1, MAX_BAD_LINES_TO_SHOW - 1) do
        begin
          TempLineNum := FL.Names[i];
          TempStr := FL.Values[TempLineNum];
          MyPegMsgDlg.Memo1.Lines.Append(Format('Line: %-10s  Value: %s', [TempLineNum, TempStr]));
        end;
      end;
      i := MyPegMsgDlg.ShowModal;
    finally
      if Assigned(MyPegMsgDlg) then
        MyPegMsgDlg.Free;
      FProgressMessage := EmptyStr;
    end;
  end;
  if HasBadLines then
  begin
    FIsDangerousQuery := False;
    Exit;
  end;

  if not Assigned(FFileList) then
    FFileList := TStringList.Create;
  FFileList.Clear;
  FFileList.Assign(FL);
  FTempFileList.AddStrings(FL); // just in case something goes wrong, any remaining temp files will be cleaned up
  FNumRequestsNeeded := FFileList.Count;
  if not LaunchQueryThread then
  begin
    ShowMessage('Oops! An error occured when launching the query thread');
    FIsDangerousQuery := False;
  end;
end;

procedure TMutationExplorerForm.ClearTableActionExecute(Sender: TObject);
begin
  FNumQueries := 0;
  try
    BeginFormUpdate;
    FDetailsCanUpdate := False;
    if Assigned(MutationDetailViewForm) then
      MutationDetailViewForm.Hide;
    PredictionsDrawGrid.RowCount := 1;
    PredictionsDrawGrid.Invalidate;
    ClearDiagnoses;
    UpdateActionList;
    FSourceFile := EmptyStr;
    UpdateStatusBar('Table Cleared');
    FPrecision := DEFAULT_PRECISION;
  finally
    FDetailsCanUpdate := True;
    EndFormUpdate;
  end;
end;

function TMutationExplorerForm.CompareNatural(First, Second: String): Integer;
var
  b: Boolean;

  function ExtractNumeric(Index: Integer; var SourceStr: String): Double;
  begin
    while (Index <= Length(SourceStr)) and (SourceStr[Index] in ['0'..'9', '.']) do
      inc(Index);
    Result := StrToFloatDef(Copy(SourceStr, 1, (Index - 1)), 0);
    Delete(SourceStr, 1, (Index - 1));
  end;

begin
  Result := 0;
  First := LowerCase(First);
  Second := LowerCase(Second);

  if (First = Second) then // many will be the same so we can return quickly
    Exit;

  if (First <> EmptyStr) and (Second <> EmptyStr) then begin
    b := False;
    while (not b) do
    begin
      if ((First[1] in ['0'..'9']) and (Second[1]  in ['0'..'9'])) then
        Result:= Sign(ExtractNumeric(1, First) - ExtractNumeric(1, Second))
      else
        Result:= Sign(Integer(First[1]) - Integer(Second[1]));
      b := (Result <> 0) or (Min(Length(First), Length(Second)) < 2);
      if not b then begin
        Delete(First,1,1);
        Delete(Second,1,1);
      end;
    end;
  end;
  if Result = 0 then
  begin
    if (Length(First) = 1) and (Length(Second) = 1) then
      Result:= Sign(Integer(First[1]) - Integer(Second[1]))
    else
      Result:= Sign(Length(First) - Length(Second));
  end;
end;

procedure TMutationExplorerForm.MyPegThreadDone(aThread: TObject);
var
  TempList: TStringList = nil;
begin
  try
    ResultsCriticalSection.Acquire;
    while ResultsQueue.Count > 0 do
    begin
      TempList := TStringList(ResultsQueue[0]);
      if not Assigned(ResponseList) then
        ResponseList := TStringList.Create;
      ResponseList.Clear;
      ResponseList.AddStrings(TempList);
      ResultsQueue.Delete(0);
      AppendMasterSourceView;
    end;
  finally
    ResultsCriticalSection.Release;
    FProgressMessage := EmptyStr;
    FIsDangerousQuery := False;
  end;
end;

function TMutationExplorerForm.ConvertFieldListToArray(Input: TStringlist; var Output: TFieldArray): Boolean;
var
  i: Integer;
  TempInt: Integer;
  TempFloat: Double;
  TempBool: Boolean = False;
begin
  Result := True;
  try
    SetLength(Output, Input.Count);
    for i := 0 to Input.Count - 1 do
    begin
      TempInt := TryMakeNewInt(Input[i], TempBool);
      if TempBool then
        Output[i] := TempInt
      else
      begin
        TempFloat := TryMakeNewFloat(Input[i], TempBool);
        if TempBool then
          Output[i] := TempFloat
        else
          Output[i] := VarToStr(Input[i]);
      end;
    end;
  except
    on E:Exception do
    begin
      {$IFDEF DEBUG}
       ShowMessage('Error in MutationExplorer.ConvertFieldListToArray: ' + E.Message);
      {$ENDIF}
      Result := False;
    end;
  end;
end;

procedure TMutationExplorerForm.CopyActionExecute(Sender: TObject);
var
  aList: TStringList = nil;
  toCopy: String = '';
  writer: TExcelWrite = nil;
  i: Integer;
begin
  if (PredictionsDrawGrid.Selection.Top < 0) or (PredictionsDrawGrid.Selection.Bottom < 0) then
    Exit;
  try
    writer := BuildExcelExport;
    aList := writer.GetAsTabDelimitedList;
    for i := PredictionsDrawGrid.Selection.Top to PredictionsDrawGrid.Selection.Bottom do
      toCopy := toCopy + aList[i] + LineEnding;
    Clipboard.AsText := Trim(toCopy);
  finally
    if Assigned(writer) then
      writer.Free;
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TMutationExplorerForm.UpdateActionList;
begin
  DecreasePrecisionAction.Enabled := True;
  ReplaceNASymbolsAction.Enabled := True;
  IncreasePrecisionAction.Enabled := True;
  ExportExcelAction.Enabled := True;
  ExportCSVAction.Enabled := True;
  FilterAction.Enabled := True;

  ImportQueryDataFileAction.Enabled := True;
  ToggleDetailsOnTopAction.Enabled := True;
  ShowToolBarAction.Enabled := True;
  if PageControl.TabIndex = PREDICTION_DATA_PAGE_INDEX then
  begin
    FindAction.Enabled := True;
    FindNextAction.Enabled := True;
    ClearTableAction.Enabled := True;
    BestFitColsAction.Enabled := True;
    //if TreeList.SelectionCount > 0 then
    //  CopyAction.Enabled := True
    //else
    //  CopyAction.Enabled := False;
    SelectAllAction.Enabled := True;
    ExportExcelAction.Enabled := True;
    ExportCSVAction.Enabled := True;
  end
  else
  begin
    FindAction.Enabled := False;
    FindNextAction.Enabled := False;
    ClearTableAction.Enabled := False;
    BestFitColsAction.Enabled := False;
    CopyAction.Enabled := False;
    SelectAllAction.Enabled := False;
    ExportExcelAction.Enabled := False;
    ExportCSVAction.Enabled := False;
  end;
  ShowDetailViewAction.Enabled := True;

  if not Assigned(MutationDetailViewForm)  then
    ShowSDEAction.Enabled := False
  else if (not MegaForm.HasActiveData) and (not MutationDetailViewForm.DidActivateData) then
    ShowSDEAction.Enabled := False
  else
    ShowSDEAction.Enabled := True;

end;

function TMutationExplorerForm.UpdateCachingProgress(Progress: Integer): Boolean;
begin
  Result := (FMSProgress.UpdateCurrentOpProgress(Progress) and FMSProgress.UpdateOverallProgress(Progress div 4));
end;

procedure TMutationExplorerForm.UpdateErrorMsgLabel(Msg: String);
begin
  ShowMessage(Msg);
end;

procedure TMutationExplorerForm.UpdateGeneSearchView(SearchResult: TEvoDSearchResult);
var
  i: Integer;
  aEntry: TGeneEntry;
begin
  if SearchResult.NumResults = 0 then
  begin
    StopGeneSearchTimers;
    StatusBar1.Panels[0].Text := 'Idle';
    ShowMessage('No results were found for the given query');
  end
  else
  begin
    try
      BeginFormUpdate;
      for i := 0 to SearchResult.NumResults - 1 do
      begin
        aEntry := TGeneEntry.Create(SearchResult.GetResult(i));
        FGeneEntries.Add(aEntry);
      end;
      GeneSearchDrawGrid.RowCount := (FGeneEntries.Count + 1);
      UpdateGeneSearchDrawGridColSizes;
      GeneSearchDrawGrid.Invalidate;
      StatusBar1.Panels[1].Text := 'Displaying ' + IntToStr(SearchResult.NumResults) + ' result(s)  ';
      UpdateStatusBar('Idle');
      UpdateActionList;
    finally
      EndFormUpdate;
      StopGeneSearchTimers;
    end;
  end;
end;

procedure TMutationExplorerForm.ErrorMsgCallBack(Msg: String; AThread: TThread);
begin
  FGeneSearchErrorMsg := 'Oops! An error occurred when querying the server: ' + Msg;
  TThread.Synchronize(AThread, @DoUpdateGeneSearchErrorMsgLabel);
  StopGeneSearchTimers;
  FMSProgress.StopProgress;
  FMSProgress.Hide;
end;


procedure TMutationExplorerForm.ExportCSVActionExecute(Sender: TObject);
var
  OutFile: TStringList = nil;
  OutFileName: String;
  Writer: TExcelWrite = nil;
  Filters: TStringList = nil;
begin
  try
    try
      Filters := TStringList.Create;
      Filters.Values['CSV files (*.csv)'] := '*csv';
      Filters.Values['All files (*.*)'] := '*.*';
      OutFileName := AskUserForFileSaveLocation(Filters);
      if OutFileName = EmptyStr then
      begin
        FreeAndNil(Filters);
        Exit;
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! An error occured when displaying the save dialog: ' + E.Message);
    end;
  finally
    if Assigned(Filters) then
      Filters.Free;
  end;

  OutFileName := ChangeFileExt(OutFileName,'.csv');
  try
    try
      Writer := BuildExcelExport;
      OutFile := Writer.GetCsvList;
      OutFile.SaveToFile(OutFileName);
    except
      on E: Exception do
        ShowMessage('Oh no! An error occurred when writing the csv file: ' + E.Message);
    end;
  finally
    if Assigned(Writer) then
      Writer.Free;
    if Assigned(OutFile) then
      OutFile.Free;
  end;
end;

procedure TMutationExplorerForm.ExportExcelActionExecute(Sender: TObject);
var
  Writer: TExcelWrite = nil;
  OutFile: String;
  Filters: TStringList = nil;
  Progress: TMultiStageProgressForm = nil;
begin
  if FDiagnoses.Count = 0 then
  begin
    ShowMessage('Oops! There is no data yet to export');
    Exit;
  end;
  Filters := TStringList.Create;
  Filters.Values['Excel files (*.xls, *.xlsx)'] := '*xls;*xlsx';
  Filters.Values['All files (*.*)'] := '*.*';
  try
    OutFile := AskUserForFileSaveLocation(Filters);
    if OutFile = EmptyStr then
      Exit;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occured when displaying the save dialog: ' + E.Message);
  end;
  OutFile := ChangeFileExt(OutFile, '.xls');

  Writer := BuildExcelExport;
  if not Assigned(Writer) then
    Exit;

  try
    try
      Progress := TMultiStageProgressForm.Create(Self);
      Progress.CancelButton.Enabled := False;
      Progress.Show;
      Progress.StartProgress('Exporting to Excel', 'Preparing Data');
      Progress.UpdateCurrentOpStatus('Launching Excel Writer');
      FDetailsOnTop := ToggleDetailsOnTopAction.Checked;
      if FDetailsOnTop then
        ToggleDetailsOnTopActionExecute(nil);
      Writer.MSProgress := Progress;
      Writer.SaveFile(OutFile, OutputFileTypeForFileExt(ExtractFileExt(OutFile)));
    except
      on E: Exception do
        ShowMessage('Oh no! An error occurred when writing the Excel file: ' + E.Message);
    end;
  finally
    if Assigned(Filters) then
      Filters.Free;
    if Assigned(Progress) then
      Progress.Free;
    if Assigned(Writer) then
      Writer.Free;
  end;
end;

procedure TMutationExplorerForm.FilterActionExecute(Sender: TObject);
begin
  { TODO 2 -oglen -cmypeg : implement the filtering action. For the myPEG only we won't include this but for the future MEGA version we might }
end;

procedure TMutationExplorerForm.FormClose(Sender: TObject;var aAction: TCloseAction);
begin
  {$IFDEF MYPEG_ONLY}
  Application.Terminate;
  {$ENDIF}
  if Assigned(MutationDetailViewForm) then
    MutationDetailViewForm.Hide;
end;

procedure TMutationExplorerForm.FormCreate(Sender: TObject);
{$IFDEF MYPEG_ONLY}
var
  IconFile: String;
{$ENDIF}
begin
  UpdateShortcutsForMacOs(ActionList1);
  FSearchCol := -1;
  FSearchRow := -1;
  FSearchTerm := EmptyStr;
  HelpKeyword := 'Mutation_Explorer.htm';
  HelpContext := HC_MUTATION_EXPLORER;
  FGridColumnSizer := TGridColumnResizer.Create;
  InitDrawGrids;
  FDiagnoses := TList.Create;
  FGeneEntries := TList.Create;

  InitRegexes;
  //ManualEntryPage.Visible := False;
  {$IFDEF MYPEG_ONLY}
  IconFile := GetPrivateFile(mfMyPegIconFile);
  if FileExists(IconFile) then
    try
      Self.Icon.LoadFromFile(IconFile);
    except
      {$IFDEF DEBUG}
       on E: Exception do
         ShowMessage('Error in MutationExplorer.FormCreate: ' + E.Message);
      {$ENDIF}
     // don't sweat the small stuff
    end;
  {$ENDIF}
  Self.Caption := 'Mutation Explorer';

  FIsDangerousQuery := False;
  PageControl.TabIndex := 0;
  FProgressWheel[0] := '  -';
  FProgressWheel[1] := '  \';
  FProgressWheel[2] := '  |';
  FProgressWheel[3] := '  /';
  FJumpToNode := False;
  FDetailsCanUpdate := True;
  FTempFileList := TStringList.Create;
  FShowErrorMessages := True;
  FNumQueries := 0;
  FFileList := TStringList.Create;
  FMasterViewIsCreated := False;
  UpdateStatusBar('Idle');
  FNumRequestsNeeded := 0;
  FNumRequestsCompleted := 0;
  FShowNAStrings := True;
  FPrecision := DEFAULT_PRECISION;
  FCurrentState := tcsIdle;
  MutationDetailViewForm := TMutationDetailViewForm.Create(Self);
  MutationDetailViewForm.Precision := FPrecision;
  MutationDetailViewForm.Left := Self.Left + Self.Width + 10;
  MutationDetailViewForm.Top := Self.Top;
  MutationDetailViewForm.Hide;

  FMSProgress := TMultiStageProgressForm.Create(Self);
  if Assigned(V_SeqDataExplorer) then
   V_SeqDataExplorer.MutationQueryHandler := @CachingCompletedEvent;
  FindDlg := TMyPegSearchDlg.Create(Self);
  FindDlg.Hide;

  GeneSearchCS := TCriticalSection.Create;
  try
    GeneSearchCS.Acquire;
    GeneQueryResult := TList.Create;
  finally
    GeneSearchCS.Release;
  end;

  FDetailsOnTop := True;
  MyPegCriticalSection := TCriticalSection.Create;
  try
    MyPegCriticalSection.Acquire;
    ThreadsCancelled := False;
    QueryFiles := TStringList.Create;
  finally
    MyPegCriticalSection.Release;
  end;

  ResultsCriticalSection := TCriticalSection.Create;
  try
    ResultsCriticalSection.Acquire;
    ResultsQueue := TList.Create;
  finally
    ResultsCriticalSection.Release;
  end;
  UpdateActionList;
  {$IFNDEF DARWIN}
  InitMainMenu;
  {$ENDIF}
  ImageForm.UpdateImgList(Self);
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
  MegaForm.AddWindowToTray(Self, False);
end;

procedure TMutationExplorerForm.FormDeactivate(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

procedure TMutationExplorerForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(FDiagnoses) then
  begin
    ClearDiagnoses;
    FDiagnoses.Free;
  end;
  if Assigned(FGeneEntries) then
  begin
    ClearGeneEntries;
    FGeneEntries.Free;
  end;
  if Assigned(FGridColumnSizer) then
    FGridColumnSizer.Free;;
  if Assigned(FTokens) then
    FTokens.Free;
  if Assigned(FRefSeqIdRegex) then
    FRefSeqIdRegex.Free;
  if Assigned(FIntRegex) then
    FIntRegex.Free;
  if Assigned(FAARegex) then
    FAARegex.Free;
  if Assigned(FMSProgress) then
    FMSProgress.Free;
  if Assigned(FFileList) then
    FFileList.Free;
  if Assigned(MutationDetailViewForm) then
    FreeAndNil(MutationDetailViewForm);
  if Assigned(FindDlg) then
    FindDlg.Free;
  if Assigned(FTempFileList) then
  begin
    for i := 0 to FTempFileList.Count - 1 do
    begin
      if FileExists(FTempFileList[i]) then
        try
          DeleteFile(FTempFileList[i]);
        except
          {$IFDEF DEBUG}
           on E: Exception do
             ShowMessage('Error in MutationExplorer.FormDestroy: ' + E.Message);
          {$ENDIF}
          // do nothing
        end;
    end;
    FTempFileList.Free;
  end;

  try
    MyPegCriticalSection.Acquire;
    if Assigned(QueryFiles) then
      QueryFiles.Free
  finally
    MyPegCriticalSection.Release;
  end;

  try
    ResultsCriticalSection.Acquire;
    if Assigned(ResultsQueue) then
    begin
      ResultsQueue.Clear;
      ResultsQueue.Free;
    end;
  finally
    ResultsCriticalSection.Release;
  end;

  try
    GeneSearchCS.Acquire;
    if GeneQueryResult.Count > 0 then
    begin
      for i := 0 to GeneQueryResult.Count - 1 do
        if Assigned(GeneQueryResult[i]) then
          TEvoDSearchResult(GeneQueryResult[i]).Free;
    end;
    GeneQueryResult.Free;
  finally
    GeneSearchCS.Release;
  end;

  if Assigned(GeneSearchCS) then
    FreeAndNil(GeneSearchCS);
  if Assigned(MyPegCriticalSection) then
    FreeAndNil(MyPegCriticalSection);
  if Assigned(ResultsCriticalSection) then
    FreeAndNil(ResultsCriticalSection);
end;

procedure TMutationExplorerForm.FormHide(Sender: TObject);
begin
  if Assigned(MutationDetailViewForm) then
    MutationDetailViewForm.Hide;
end;

procedure TMutationExplorerForm.FormMouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

function TMutationExplorerForm.RowIsInRect(aRow: Integer; aRect: TRect): Boolean;
begin
  Result := False;
  if (aRect.Top < 0) or (aRect.Bottom < 0) or (aRow < 0) then
    Exit;
  if (aRow < aRect.Top) or (aRow > aRect.Bottom) then
    Exit;
  Result := True;
end;

procedure TMutationExplorerForm.FocusPredictionsGridCell(aRow, aCol: Integer);
begin
  PredictionsDrawGrid.Row := aRow;
  PredictionsDrawGrid.Col := aCol;
  PredictionsDrawGrid.Invalidate;
end;

procedure TMutationExplorerForm.InitMainMenu;
begin
  {$IFNDEF DARWIN}
  MainMenu1.OwnerDraw := True;
  MainMenu1.OnDrawItem := @MegaForm.DrawMenuItem;
  MainMenu1.OnMeasureItem := @MegaForm.MeasureMenuItem;
  {$ENDIF}
end;

procedure TMutationExplorerForm.InitMutationDetailViewForm;
begin
  if not Assigned(MutationDetailViewForm) then
  begin
    MutationDetailViewForm := TMutationDetailViewForm.Create(Self);
    MutationDetailViewForm.Precision := FPrecision;
    MutationDetailViewForm.Top := 20;
    MutationDetailViewForm.Left := Self.Left + Self.Width + 10;
    if (MutationDetailViewForm.Left + MutationDetailViewForm.Width) > Screen.DesktopWidth then
      MutationDetailViewForm.Left := Self.Left + 10;
    MutationDetailViewForm.Hide;
  end;
end;

function TMutationExplorerForm.BuildExcelExport: TExcelWrite;
var
  i, j, TempInt: Integer;
  TempFloat: Double;
  TempString: String;
  diagnosis: TMutationDiagnosis;
  Progress: TMultiStageProgressForm = nil;
  UserCancelled: Boolean;
begin
  UserCancelled := False;
  Progress := TMultiStageProgressForm.Create(Self);
  Progress.CancelButton.Enabled := False;
  Progress.Show;
  Progress.StartProgress('Exporting to Excel', 'Preparing Data');
  Result := TExcelWrite.Create(Self, 'MEGA_MD Data');
  Result.IsXLS := True;
  try
    try
      for i := 1 to PredictionsDrawGrid.ColCount - 1 do
        Result.Add(PredictionsGridHeaderString(i));
      Result.WriteLine();
      Progress.UpdateOverallProgress(1);

      for i := 0 to FDiagnoses.Count - 1 do
      begin
        diagnosis := TMutationDiagnosis(FDiagnoses[i]);
        for j := 1 to PredictionsDrawGrid.ColCount - 1 do
        begin
          TempString := PredictionsGridDiagnosisString(diagnosis, j);
          case j of
            1, 3, 4: // these columns have integer values
              begin
                if TryStrToInt(TempString, TempInt) then
                  Result.Add(TempInt)
                else
                  Result.Add(TempString);
              end;
            2, 5, 6, 7, 8, 9, 10, 12, 14, 16, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
            30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40:  // these columns have floating point values
              begin
                if TryStrToFloat(TempString, TempFloat) then
                  Result.Add(TempFloat)
                else
                  Result.Add(TempString);
              end;
            else
              Result.Add(TempString);
          end;
        end;
        Result.WriteLine();
        if not Progress.UpdateCurrentOpProgress(Trunc(i / FDiagnoses.Count * 100)) then
        begin
          UserCancelled := True;
          break;
        end;
      end;

      if UserCancelled then
        Exit;
    except
      on E: Exception do
        ShowMessage('Oh no! An error occurred when writing the Excel file: ' + E.Message);
    end;
  finally
    if Assigned(Progress) then
      Progress.Free;
  end;
end;

function TMutationExplorerForm.PredictionsGridHeaderString(aCol: Integer): String;
begin
  case aCol of
    0: Result := EmptyStr;
    1: Result := 'rsID';
    2: Result := 'Peptide ID';
    3: Result := 'mRNA ID';
    4: Result := 'AA Pos';
    5: Result := 'Reference (AA)';
    6: Result := 'Mutant (AA)';
    7: Result := 'Consensus';
    8: Result := 'EvoD';
    9: Result := 'EvoD P-value';
    10: Result := 'Polyphen-2 Original';
    11: Result := 'Polyphen-2 Balanced';
    12: Result := 'SIFT Original';
    13: Result := 'SIFT Balanced';
    14: Result := 'EvoD';
    15: Result := 'Polyphen-2';
    16: Result := 'SIFT';
    17: Result := 'Grantham Distance';
    18: Result := 'Blosum 62';
    else
      Result := EmptyStr;
  end;
end;

function TMutationExplorerForm.PredictionsGridDiagnosisString(diagnosis: TMutationDiagnosis; aCol: Integer): String;
var
  floatFormatStr: String;
begin
  floatFormatStr := '%.' + IntToStr(FPrecision) + 'f';

  case aCol of
    0: Result := EmptyStr;
    1: Result := Format('%s', [diagnosis.Rsid]);
    2: Result := Format('%s', [diagnosis.Peptideid]);
    3: Result := Format('%s', [diagnosis.Mrnaid]);
    4: Result := Format('%d', [diagnosis.Aapos]);
    5: Result := Format('%s', [diagnosis.Wildallele]);
    6: Result := Format('%s', [diagnosis.Mutantallele]);
    7: Result := Format('%s', [diagnosis.Conspred]);
    8: Result := Format('%s', [diagnosis.Evodpred]);
    9:
      begin
        if CompareValue(diagnosis.Evodpval, BAD_NUMBER, DELTA) <> 0 then
          Result := Format('%.3e', [diagnosis.Evodpval])
        else
          Result := 'NA';
      end;
    10: Result := Format('%s', [diagnosis.Polyphenpred]);
    11: Result := Format('%s', [diagnosis.Polyphenpred_Balanced]);
    12: Result := Format('%s', [diagnosis.Siftpred]);
    13: Result := Format('%s', [diagnosis.Siftpred_Balanced]);
    14:
      begin
        if CompareValue(diagnosis.Evodscore, BAD_NUMBER, DELTA) <> 0 then
          Result := Format(floatFormatStr, [diagnosis.Evodscore])
        else
          Result := 'NA';
      end;
    15:
      begin
        if CompareValue(diagnosis.Polyphenscore, BAD_NUMBER, DELTA) <> 0 then
          Result := Format(floatFormatStr, [diagnosis.Polyphenscore])
        else
          Result := 'NA';
      end;
    16:
      begin
        if CompareValue(diagnosis.siftscore, BAD_NUMBER, DELTA) <> 0 then
          Result := Format(floatFormatStr, [diagnosis.siftscore])
        else
          Result := 'NA';
      end;
    17:
      begin
        if CompareValue(diagnosis.Granthamdist, BAD_NUMBER, DELTA) <> 0 then
          Result := Format(floatFormatStr, [diagnosis.Granthamdist])
        else
          Result := 'NA';
      end;
    18:
      begin
        if CompareValue(diagnosis.Blosum62, BAD_NUMBER, DELTA) <> 0 then
          Result := Format(floatFormatStr, [diagnosis.Blosum62])
        else
          Result := 'NA';
      end
    else
      Result := EmptyStr;
  end;
end;

function TMutationExplorerForm.PredictionsGridAlignment(aCol: Integer): TAlignment;
begin
  case aCol of
    0, 4, 5, 8: Result := taCenter;
    1, 13, 14, 15, 16, 17: Result := taRightJustify;
    2, 3, 6, 7, 9, 10, 11, 12: Result := taLeftJustify;
    else
      Result := taLeftJustify;
  end;
end;

function TMutationExplorerForm.SelectedMutation: TMutationDiagnosis;
begin
  if (PredictionsDrawGrid.Selection.Top > 0) and (PredictionsDrawGrid.Selection.Top <= FDiagnoses.Count) then
    Result := TMutationDiagnosis(FDiagnoses[PredictionsDrawGrid.Selection.Top - 1])
  else
    Result := nil;
end;

procedure TMutationExplorerForm.UpdateGeneSearchDrawGridColSizes;
var
  aList: TStringList;
begin
  try
    BeginFormUpdate;
    aList := LongestGeneEntryStrings;
    FGridColumnSizer.ResizeGridColumns(GeneSearchDrawGrid, aList);
  finally
    EndFormUpdate;
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TMutationExplorerForm.UpdatePredictionsGridColSizes;
var
  aList: TStringList = nil;
begin
  try
    BeginFormUpdate;
    aList := LongestMutationDataStrings;
    FGridColumnSizer.ResizeGridColumns(PredictionsDrawGrid, aList);
  finally
    EndFormUpdate;
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TMutationExplorerForm.LongestGeneEntryStrings: TStringList;
var
  i: Integer;
  aEntry: TGeneEntry;
begin
  Result := TStringList.Create;
  Result.Add('Gene Name');
  Result.Add('Peptide ID');
  Result.Add('Gene Product');
  Result.Add('Diagnose Variant');
  Result.Add(EmptyStr);
  if Assigned(FGeneEntries) and (FGeneEntries.Count > 0) then
    for i := 0 to FGeneEntries.Count - 1 do
    begin
      aEntry := TGeneEntry(FGeneEntries[i]);
      if Length(aEntry.GeneName) > Length(Result[GENE_NAME_INDEX]) then
        Result[GENE_NAME_INDEX] := aEntry.GeneName;
      if Length(aEntry.PeptideID) > Length(Result[PEPTIDE_ID_INDEX]) then
        Result[PEPTIDE_ID_INDEX] := aEntry.PeptideID;
      if Length(aEntry.Product) > Length(Result[GENE_PRODUCT_INDEX]) then
        Result[GENE_PRODUCT_INDEX] := aEntry.Product;
    end;
end;

function TMutationExplorerForm.LongestMutationDataStrings: TStringList;
var
  i, j: Integer;
  diagnosis: TMutationDiagnosis;
  temp: String;
begin
  Result := TStringList.Create;
  Result.Add('>');
  for i := 1 to PredictionsDrawGrid.ColCount - 1 do
    Result.Add(PredictionsGridHeaderString(i));
  if FDiagnoses.Count > 0 then
    for i := 0 to Min(FDiagnoses.Count - 1, PredictionsDrawGrid.VisibleRowCount) do
    begin
      diagnosis := TMutationDiagnosis(FDiagnoses[i]);
      for j := 1 to PredictionsDrawGrid.ColCount - 1 do
      begin
        temp := PredictionsGridDiagnosisString(diagnosis, j);
        if Length(temp) > Length(Result[j]) then
          Result[j] := temp;
      end;
    end;
end;

procedure TMutationExplorerForm.InitDrawGrids;
begin
  FGridColor1 := RGB($e9, $e9, $e9);
  //FGridColor2 := RGB($a0, $c7, $d2);
  FGridColor2 := $00f7f8f8;
  FGridFocusedColor := RGB($17, $85, $a6);
  FGridSelectedColor := RGB($a0, $c7, $d2);
  GeneSearchDrawGrid.Align := alClient;
  GeneSearchDrawGrid.RowCount := 1;
  GeneSearchDrawGrid.Color := FGridColor1;
  PredictionsDrawGrid.Align := alClient;
  PredictionsDrawGrid.RowCount := 1;
  PredictionsDrawGrid.ColCount := 19;
  PredictionsDrawGrid.Color := FGridColor1;
  UpdateGeneSearchDrawGridColSizes
end;

function TMutationExplorerForm.NumMutations: Int64;
begin
  if Assigned(FDiagnoses) then
    Result := FDiagnoses.Count
  else
    Result := 0;
end;

procedure TMutationExplorerForm.ClearDiagnoses;
var
  i: Integer;
begin
  if not Assigned(FDiagnoses) then
    Exit;
  if FDiagnoses.Count > 0 then
    for i := 0 to FDiagnoses.Count - 1 do
      TMutationDiagnosis(FDiagnoses[i]).Free;
  FDiagnoses.Clear;
end;

procedure TMutationExplorerForm.ClearGeneEntries;
var
  i: Integer;
begin
  if not Assigned(FGeneEntries) then
    Exit;
  if FGeneEntries.Count > 0 then
    for i := 0 to FGeneEntries.Count - 1 do
      TGeneEntry(FGeneEntries[i]).Free;
  FGeneEntries.Clear;
end;

procedure TMutationExplorerForm.FormShow(Sender: TObject);
begin
//  if FNeedsCleared and Assigned(TreeList) then
//    TreeList.Clear;
end;

procedure TMutationExplorerForm.ImportDataQueryFileDirect(QueryFile: String);
var
  CachingThread: TDataCacheThread;
begin
  try
    InitMutationDetailViewForm;
    FMSProgress.SetToProgressMode;
    FMSProgress.StartProgress('Processing mutation data file', 'Caching data');
    FMSProgress.Show;
    FJumpToNode := True;
    FNumRequestsNeeded := 1;
    FNumRequestsCompleted := 0;
    FSourceFile := QueryFile;
    FTempFileList.Add(QueryFile); // so we can delete it when this form is destroyed
    CachingThread := TDataCacheThread.Create(True);
    CachingThread.SourceFile := FSourceFile;
    CachingThread.CacheUpdateEvent := @UpdateCachingProgress;
    CachingThread.CachingCompleteEvent := @CachingCompletedEvent;
    CachingThread.FreeOnTerminate := True;
    CachingThread.Start;
  except
    on E: Exception do
      ShowMessage('Error encountered during POST: ' + E.Message);
  end;
end;

procedure TMutationExplorerForm.ImportQueryDataFileActionExecute(Sender: TObject);
var
  OpenDialog : TOpenDialog;
  CachingThread: TDataCacheThread;
begin
  FJumpToNode := False;
  FNumRequestsCompleted := 0;
  OpenDialog := TOpenDialog.Create(self);
  OpenDialog.Title := 'Please select a text file with coordinate info';
  OpenDialog.InitialDir := GetCurrentDir;

  if OpenDialog.Execute then
  try
    try
      if ValidateInputFile(OpenDialog.FileName) then
      begin
        DisableActionsDuringQuery;
        FMSProgress.SetToProgressMode;
        FMSProgress.StartProgress('Processing mutation data file', 'Caching data');
        FMSProgress.Show;
        FSourceFile := OpenDialog.FileName;
        InitMutationDetailViewForm;
        MutationDetailViewForm.SetActionsEnabled(False);
        try
          MyPegCriticalSection.Acquire;
          ThreadsCancelled := False;
        finally
          MyPegCriticalSection.Release;
        end;

        FProgressMessage := 'Caching data';
        CachingThread := TDataCacheThread.Create(True);
        CachingThread.SourceFile := FSourceFile;
        CachingThread.CacheUpdateEvent := @UpdateCachingProgress;
        CachingThread.CachingCompleteEvent := @CachingCompletedEvent;
        CachingThread.CachingCancelledEvent := @CachingCancelledEvent;
        CachingThread.FreeOnTerminate := True;
        CachingThread.Start;
      end;
    except
      on E: Exception do
        ShowMessage('Error encountered during POST: ' + E.Message);
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TMutationExplorerForm.IncreasePrecisionActionExecute(Sender: TObject);
begin
  if FPrecision < MAX_PRECISION then
    inc(FPrecision);
  if FPrecision = MAX_PRECISION then
    IncreasePrecisionAction.Enabled := False
  else
    IncreasePrecisionAction.Enabled := True;
  DecreasePrecisionAction.Enabled := True;
  MutationDetailViewForm.Precision := FPrecision;
  RefreshTree;
  if Assigned(MutationDetailViewForm) then
    MutationDetailViewForm.UpdateDetailView(SelectedMutation);
end;

procedure TMutationExplorerForm.InitRegexes;
begin
  FRefSeqIdRegex := TRegExpr.Create;
  FRefSeqIdRegex.Expression := 'NP_[\d]+$';
  FRefSeqIdRegex.ModifierI := True;
  FIntRegex := TRegExpr.Create;
  FIntRegex.Expression := '^[\d]+$';
  FAARegex := TRegExpr.Create;
  FAARegex.Expression := '[ACDEFGHIKLMNPQRSTVWY]';
  FTokens := TStringList.Create;
end;

function TMutationExplorerForm.IsValidLine(Line: String): Boolean;
begin
  Result := False;
  try
    if SplitOnSingleCharFaster(Line, ',', FTokens, True) then
    begin
      if FTokens.Count <> 3 then
        Exit;
      if not FRefSeqIdRegex.Exec(FTokens[0]) then
        Exit;
      if not FIntRegex.Exec(FTokens[1]) then
        Exit;
      if (not FAARegex.Exec(FTokens[2])) or (Length(Trim(FTokens[2])) <> 1) then
        Exit;
      Result := True;
    end
  except
    Result := False;
  end;
end;

procedure TMutationExplorerForm.KeepDetailListOnTop1Click(Sender: TObject);
begin
  ToggleDetailsOnTopAction.Checked := not ToggleDetailsOnTopAction.Checked;
  if ToggleDetailsOnTopAction.Checked then
    MutationDetailViewForm.FormStyle := fsStayOnTop
  else
    MutationDetailViewForm.FormStyle := fsNormal;
end;

function TMutationExplorerForm.LaunchQueryThread: Boolean;
var
  QueryThread: TMyPegThread;
begin
  Result := False;

  try
    MyPegCriticalSection.Acquire;
    ThreadsCancelled := False;
    QueryFiles.Clear;
    QueryFiles.AddStrings(FFileList);
  finally
    MyPegCriticalSection.Release;
  end;

  FShowErrorMessages := True;
  try
    FMSProgress.UpdateCurrentOpStatus('Retrieving predictions');
    UpdateStatusBar('Retrieving predictions');
    FProgressMessage := 'Retrieving predictions';
    FProgressWheelIndex := 1;
    QueryThread := TMyPegThread.Create(True);
    QueryThread.OnTerminate := @MyPegThreadDone;
    QueryThread.Start;
    Result := True;
  except
    on E: Exception do
    begin
      ShowMessage('Failed to launch query thread: ' + E.Message);
    end;
  end;
end;

procedure TMutationExplorerForm.ReEnableActionsAfterQuery;
begin
  DecreasePrecisionAction.Enabled := True;
  IncreasePrecisionAction.Enabled := True;
  ExportExcelAction.Enabled := True;
  ExportCSVAction.Enabled := True;
  ClearTableAction.Enabled := True;
  ImportQueryDataFileAction.Enabled := True;
  ToggleDetailsOnTopAction.Enabled := True;
  ToggleAutoWidthAction.Enabled := True;
  ShowToolBarAction.Enabled := True;
  FindAction.Enabled := True;
  FindNextAction.Enabled := True;
  BestFitColsAction.Enabled := True;
  GeneSearchAction.Enabled := True;
  CopyAction.Enabled := (PredictionsDrawGrid.Selection.Top > 0);
  SelectAllAction.Enabled := True;
  UpdateActionList;
end;

procedure TMutationExplorerForm.RefreshTree;
begin
  try
    BeginFormUpdate;
    PredictionsDrawGrid.Invalidate;
  finally
    EndFormUpdate;
  end;
end;

procedure TMutationExplorerForm.ReplaceNASymbolsActionExecute(Sender: TObject);
//var
//  i: Integer;
begin
//  ReplaceNAStringItem.Checked := not ReplaceNAStringItem.Checked;
//  if ReplaceNAStringItem.Checked then
//    FShowNAStrings := True
//  else
//    FShowNAStrings := False;
////  cxTreeList2.Invalidate;
//  for i := 0 to TreeList.ColumnCount - 1 do
//  begin
//    TreeList.Columns[i].Width := TreeList.Columns[i].Width + 1;
//    TreeList.Columns[i].Width := TreeList.Columns[i].Width - 1;
//  end;
//  TreeList.Refresh;
//  Application.ProcessMessages;
end;

procedure TMutationExplorerForm.FindActionExecute(Sender: TObject);
var
  Response: Integer;
  rowIndex: Integer;
  Found: Boolean;
  diagnosis: TMutationDiagnosis = nil;
  colIndex: Integer;
begin
  if FDiagnoses.Count = 0 then
    Exit;
  Found := False;
  Response := FindDlg.ShowModal;
  if Response = mrOk then
  begin
    FSearchTerm := FindDlg.Edit1.Text;
    FSearchRow := 0;
    if Trim(FSearchTerm) <> EmptyStr then
    begin
      for rowIndex := 0 to FDiagnoses.Count - 1 do
      begin
        diagnosis := TMutationDiagnosis(FDiagnoses[rowIndex]);
        colIndex := diagnosis.TextSearch(FSearchTerm);
        if colIndex <> -1 then
        begin
          FocusPredictionsGridCell(rowIndex + 1, colIndex);
          if colIndex = (PredictionsDrawGrid.ColCount - 1) then
          begin
            FSearchRow := rowIndex + 1;
            FSearchCol := 1;
          end
          else
          begin
            FSearchRow := rowIndex;
            FSearchCol := colIndex + 1;
          end;
          Found := True;
          break;
        end;
      end;
    end;

    if Found then
      FindNextAction.Enabled := True
    else
    begin
      FindNextAction.Enabled := False;
      ShowMessage(Format('The search term "%s" was not found', [FSearchTerm]));
    end;
  end;
end;

procedure TMutationExplorerForm.FindNextActionExecute(Sender: TObject);
var
  Found: Boolean;
  rowIndex, colIndex: Integer;
  diagnosis: TMutationDiagnosis = nil;
begin
  if FDiagnoses.Count = 0 then
    Exit;
  if FSearchTerm = EmptyStr then
  begin
    FindActionExecute(Self);
    Exit;
  end;
  Found := False;
  for rowIndex := FSearchRow to FDiagnoses.Count - 1 do
  begin
    diagnosis := TMutationDiagnosis(FDiagnoses[rowIndex]);
    if rowIndex = FSearchRow then
      colIndex := diagnosis.TextSearch(FSearchTerm, FSearchCol)
    else
      colIndex := diagnosis.TextSearch(FSearchTerm);
    if colIndex <> -1 then
    begin
      FocusPredictionsGridCell(rowIndex + 1, colIndex);
      if colIndex = (PredictionsDrawGrid.ColCount - 1) then
      begin
        FSearchRow := rowIndex + 1;
        FSearchCol := 1;
      end
      else
      begin
        FSearchRow := rowIndex;
        FSearchCol := colIndex + 1;
      end;
      Found := True;
      break;
    end;
  end;
  if not Found then
    ShowMessage(Format('The search term "%s" was not found', [FSearchTerm]));
end;

procedure TMutationExplorerForm.SearchBtnClick(Sender: TObject);
var
  Query: String;
  QueryType: TQueryType;
  RsIDRegex: TRegExpr;
  MRnaIDRegex: TRegExpr;
  PeptideIdRegex: TRegExpr;
  i: Integer;
  Url: String;
  SearchThread: TGeneSearchThread = nil;
begin
  Query := UpperCase(Trim(Edit1.Text));
  if Query = EmptyStr then
  begin
    ShowMessage('Please enter a valid search term');
    StatusBar1.Panels[0].Text := 'Idle';
    Exit;
  end;

  try
    try
      if FGeneEntries.Count > 0 then
        ClearGeneEntries;
      StatusBar1.Panels[0].Text := 'Sending request to server...';
      QueryType := qtNONE;
      RsIDRegex := TRegExpr.Create;
      RsIDRegex.Expression := 'RS\d\d*';
      MRnaIdRegex := TRegExpr.Create;
      MrnaIdRegex.Expression := 'NM_\d\d*';
      PeptideIdRegex := TRegExpr.Create;
      PeptideIdRegex.Expression := 'NP_\d\d*';
      if RsIDRegex.Exec(Query) then
      begin
        QueryType := qtRSID;
        Query := Copy(Query, 3, Length(Query)); // remove the 'rs' from the query name since the DB entries don't have 'rs' on them.
      end
      else if MRnaIdRegex.Exec(Query) then
        QueryType := qtNMID
      else if PeptideIDRegex.Exec(Query) then
        QueryType := qtNPID
      else if (length(Query) > 1) then
        QueryType := qtGENE_OR_FUNC;

      if QueryType <> qtNONE then
      begin
        FMSProgress.SetToMarqueeMode;
        FMSProgress.StartProgress('Sending request to the server', EmptyStr);
        FMSProgress.Show;
        Url := 'http://' + MYPEG_HOST + MYPEG_PATH + 'searchForSeq.php?';
        if Querytype = qtRSID then
          Url := Url + 'rsid=' + Query
        else if QueryType = qtNMID then
          Url := Url + 'nmid=' + Query
        else if QueryType = qtNPID then
          Url := Url + 'npid=' + Query
        else if QueryType = qtGENE_OR_FUNC then
          Url := Url + 'geneorfunc=' + Query;

        try
          GeneSearchCS.Acquire;
          GeneSearchCancelled := False;
          if GeneQueryResult.Count > 0 then
          begin
            for i := 0 to GeneQueryResult.Count - 1 do
              TEvodSearchResult(GeneQueryResult[i]).Free;
          end;
          GeneQueryResult.Clear;
        finally
          GeneSearchCS.Release;
        end;
        StartGeneSearchTimers;
        SearchThread := TGeneSearchThread.Create(True);
        SearchThread.SearchQuery := Url;
        SearchThread.SearchErrorMsgCall := @ErrorMsgCallback;
        StatusBar1.Panels[0].Text := 'Sending request to the server';
        SearchThread.Start;
      end
      else
      begin
        try
          BeginFormUpdate;
          ClearGeneEntries;
          GeneSearchDrawGrid.Invalidate;
        finally
          EndFormUpdate;
        end;

        if Length(Query) <= 3 then
        begin
          ShowMessage('Search term must have at least 4 characters');
        end
        else
          ShowMessage('Invalid query');
        StatusBar1.Panels[0].Text := 'Idle';
      end;
    except
      on E:Exception do
      begin
        try
          BeginFormUpdate;
          ClearGeneEntries;
          GeneSearchDrawGrid.Invalidate;
        finally
          EndFormUpdate;
        end;
        ShowMessage('An error occurred when querying the server: ' + E.Message);
        StatusBar1.Panels[0].Text := 'Idle';
      end;
    end;
  finally
    if Assigned(RsIDRegex) then
      RsIDRegex.Free;
    if Assigned(MRnaIDRegex) then
      MRnaIDRegex.Free;
    if Assigned(PeptideIdRegex) then
      PeptideIdRegex.Free;
  end;
end;

procedure TMutationExplorerForm.SelectAllActionExecute(Sender: TObject);
begin
  PredictionsDrawGrid.Selection := Rect(1, 1, PredictionsDrawGrid.ColCount - 1, PredictionsDrawGrid.RowCount - 1);
  PredictionsDrawGrid.Invalidate;
end;

procedure TMutationExplorerForm.ShowDetailViewActionExecute(Sender: TObject);
begin
  if not Assigned(MutationDetailViewForm) then
    MutationDetailViewForm := TMutationDetailViewForm.Create(MegaForm);
  if MutationDetailViewForm.WindowState = wsMinimized then
    MutationDetailViewForm.WindowState := wsNormal;
  MutationDetailViewForm.Show;
end;

procedure TMutationExplorerForm.ShowSDEActionExecute(Sender: TObject);
begin
  if Assigned(V_SeqDataExplorer) and MegaForm.HasActiveData then
  begin
    if V_SeqDataExplorer.WindowState = wsMinimized then
      V_SeqDataExplorer.WindowState := wsNormal;
    V_SeqDataExplorer.Show;
    V_SeqDataExplorer.BringToFront;
    V_SeqDataExplorer.SetFocus;
  end;
end;

procedure TMutationExplorerForm.ShowToolbar1Click(Sender: TObject);
begin
  ShowToolbarAction.Checked := not ShowToolbarAction.Checked;
  if not ShowToolbarAction.Checked then
    ToolBar1.Visible := False
  else
    ToolBar1.Visible := True;
end;

procedure TMutationExplorerForm.StartGeneSearchTimers;
begin
  GeneSearchTimer.Enabled := True;
  GeneSearchTimeOutTimer.Enabled := True;
  SearchBtn.Enabled := False;
  ImportQueryDataFileAction.Enabled := False;
end;

procedure TMutationExplorerForm.StopGeneSearchTimers;
begin
  GeneSearchTimer.Enabled := False;
  GeneSearchTimeOutTimer.Enabled := False;
  SearchBtn.Enabled := True;
  SearchBtn.Default := True;
  if PredictionsDrawGrid.RowCount = 1 then
    StatusBar1.Panels[0].Text := 'Idle';
  ImportQueryDataFileAction.Enabled := True;
end;

procedure TMutationExplorerForm.SubmitCoordinatesActionExecute(Sender: TObject);
//var
//  RefSeqId: String;
//  MutantAllele: String;
//  PositionStr: String;
//  InputValidator: TDataCacheThread; // we won't launch the thread, we just want to use if for validating input data
//  FileWriter: TStringList;
//  TempFileName: String;
//  i: Integer;

  function IsPositiveInteger(InString: String): Boolean;
  var
    TempInt: Integer;
  begin
    Result := False;
    if not TryStrToInt(InString, TempInt) then
      Exit;
    if not (TempInt > 0) then
      Exit;
    Result := True;
  end;

begin
  //InputValidator := nil;
  //FileWriter := nil;
  //RefSeqId := Trim(RefSeqIdEdit.Text);
  //PositionStr := Trim(AAPositionEdit.Text);
  //MutantAllele := Trim(MutantAllelesComboBox.Items[MutantAllelesComboBox.ItemIndex]);
  //
  //InputValidator := TDataCacheThread.Create(True);
  //InputValidator.FreeOnTerminate := False; // we will take care of freeing it here since we won't launch it
  //if not InputValidator.MatchesRefSeqIdRegex(RefSeqId) then
  //begin
  //  ShowMessage('Please provide a valid RefSeq protein id. It should be something like ' + #34 + 'NP_000509' + #34);
  //  RefSeqIdEdit.SelectAll;
  //  RefSeqIdEdit.SetFocus;
  //  InputValidator.Free;
  //  Exit;
  //end;
  //
  //if (not InputValidator.MatchesIntegerRegex(PositionStr)) or (not IsPositiveInteger(PositionStr)) then
  //begin
  //  ShowMessage('Please provide a valid positive integer for the amino acid position value');;
  //  AAPositionEdit.SelectAll;
  //  AAPositionEdit.SetFocus;
  //  InputValidator.Free;
  //  Exit;
  //end;
  //InputValidator.Free;
  //
  //try
  //  try
  //    FileWriter := TStringList.Create;
  //    if SameText(Trim(MutantAllele), 'All') then
  //    begin
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' A');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' C');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' D');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' E');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' F');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' G');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' H');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' I');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' K');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' L');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' M');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' N');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' P');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' Q');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' R');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' S');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' T');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' V');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' W');
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' Y');
  //    end
  //    else
  //    begin
  //      MutantAllele := MutantAllele[1];
  //      FileWriter.Add(RefSeqId + ' ' + PositionStr + ' ' + MutantAllele);
  //    end;
  //    i := 0;
  //    TempFileName := getTemp + '\' + RefSeqId + '_' + PositionStr + '_' + IntToStr(i) + '_query.xml';
  //    while FileExists(TempFileName) do
  //    begin
  //      inc(i);
  //      TempFileName := getTemp + '\' + RefSeqId + '_' + PositionStr + '_' + IntToStr(i) + '_query.xml';
  //    end;
  //    FileWriter.SaveToFile(TempFileName);
  //    FIsDangerousQuery := True;
  //    ImportDataQueryFileDirect(TempFileName);
  //  except
  //    on E:Exception do
  //      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  //  end;
  //finally
  //  if Assigned(FileWriter) then
  //    FileWriter.Free;
  //end;
end;

function TMutationExplorerForm.TryMakeNewFloat(Input: String; var IsSuccess: Boolean): Double;
begin
  Result := 1.0;
  IsSuccess := False;
  if not TryStrToFloat(Input, Result) then
  begin
    IsSuccess := False;
    Exit;
  end;
  IsSuccess := True;
end;

function TMutationExplorerForm.TryMakeNewInt(Input: String; var IsSuccess: Boolean): Integer;
begin
  Result := 1;
  if not TryStrToInt(Input, Result) then
  begin
    IsSuccess := False;
    Exit;
  end;
  IsSuccess := True;
end;

procedure TMutationExplorerForm.UpdateStatusBar(ActionPanel: String);
begin
  StatusBar1.Panels[0].Text := '  ' + ActionPanel;
end;

function TMutationExplorerForm.ValidateInputFile(Filename: String): Boolean;
var
  InFile: TextFile;
  TempStr: String;
  BadLinesFound: Boolean;
  LineNumber: Integer;
  MyPegMsgDlg: TMyPegMessageDlg;
begin
  MyPegMsgDlg := nil;
  LineNumber := 1;
  BadLinesFound := False;
  Result := False;
  try
    try
      AssignFile(InFile, Filename);
      Reset(InFile);
      while not Eof(InFile) do
      begin
        ReadLn(InFile, TempStr);
        if not IsValidLine(TempStr) then
        begin
          BadLinesFound :=  True;
          MyPegMsgDlg := TMyPegMessageDlg.Create(Self);
          MyPegMsgDlg.Memo1.Lines.Append('Oh no! A failure occurred when validating the mutation data input file:');
          MyPegMsgDlg.Memo1.Lines.Append('Invalid data at line: ' + IntToStr(LineNumber));
          MyPegMsgDlg.Memo1.Lines.Append(' ');
          MypegMsgDlg.Memo1.Lines.Append(TempStr);
          MyPegMsgDlg.ShowModal;
          break;
        end;
        inc(LineNumber);
      end;
      if not BadLinesFound then
        Result := True;
    except
      on E:Exception do
      begin
        ShowMessage('Oh no! An error occurred when processing the input data file: ' + E.Message);
      end;
    end;
  finally
    CloseFile(InFile);
    if Assigned(MyPegMsgDlg) then
      MyPegMsgDlg.Free;
  end;
end;

function TMutationExplorerForm.ValidateResponse: Boolean;
var
  TempStr: String;
  ErrorCodeStr: String;
  ErrorCode: Integer;
  ErrorMessage: String;
begin
  FCustomErrorMessage := EmptyStr;
  Result := False;
  try
    if ResponseList.Count > 0 then
      TempStr := UpperCase(Trim(ResponseList[0]))
    else
    begin
     // then no data returned so handle it (is it even possible?)
      Exit;
    end;

    if Pos('ERROR', TempStr) > 0 then   // if there is a problem, the myPEG server will report it on the first line
    begin
      Delete(TempStr, 1, Pos('ERROR', TempStr) + 1);
      ErrorCodeStr := Copy(TempStr, 1, Pos(#34, TempStr));
      if not TryStrToInt(ErrorCodeStr, ErrorCode) then
        ErrorCode := 28;
      ErrorMessage := Copy(TempStr, Pos(#34, TempStr) + 1, Length(TempStr));
      ResponseList[0] := 'Oops! The MEGA-MD server returned an error message instead of results: ' + ErrorMessage;
    end
    else if (not (Pos('chr', ResponseList[0]) = 1)) and (not (Pos('NA', ResponseList[0]) = 1)) then
      Result := False
    else
      Result := True;
  except
    on E: Exception do
    begin
      ShowMessage('Oops! MEGA-MD encountered an error: ' + E.Message);
    end;
  end;
end;

end.

