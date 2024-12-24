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
  MOtuInfo, MLongIntList, ImgList, Spin, IniPropStorage,
  MD_InputDistData, ExcelWrite, MDisplayMatrixDlg, MPleaseWait,
  cef3intf, mimageform;

type

  { TV_DistDataExplorer }

  TOpenFrom = (OActivate, OAlign, OOther);

  TV_DistDataExplorer = class(TForm)
    FontDialog1: TFontDialog;
    FileMenu: TMenuItem;
    IniPropStorage1: TIniPropStorage;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
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
    SpinEdit1: TSpinEdit;
    MainToolBar: TToolBar;
    ToolButton6: TToolButton;
    ExportXLTBtn: TToolButton;
    ExportCSVTBtn: TToolButton;
    ExportMEGATBtn: TToolButton;
    ExportTextTBtn: TToolButton;
    DataGrid: TDrawGrid;
    ImageList2: TImageList;
    SaveDialog1: TSaveDialog;

    procedure AvgGpsItemClick(Sender: TObject);
    procedure DataGridColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure DataGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DataGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DecPrecisionSBtnClick(Sender: TObject);
    procedure DispChangeFontItemClick(Sender: TObject);
    procedure DispGpNamesItemClick(Sender: TObject);
    procedure DispSelTaxaItemClick(Sender: TObject);
    procedure DispSortItemClick(Sender: TObject);
    procedure FileSelectEditTaxaGroupsItemClick(Sender: TObject);
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
    procedure HelpMenuClick(Sender: TObject);
    procedure IncPrecisionSBtnClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure SaveSessionClick(Sender: TObject);
    procedure FileSaveToFileItemClick(Sender: TObject);

    procedure AvgAllItemClick(Sender: TObject);


  private
    CheckMark      : TBitmap;
    UnCheckMark    : TBitmap;
    FHasDataSubsetChanged : Boolean;
    FHasDataViewChanged   : Boolean;
    procedure InitMainMenu;
    procedure ProcessWebOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
    procedure ComputeLeftOfDecimalLen;
    procedure AdjustEnabledStatus;         // Enabled/Disable menu/toolbar items
    procedure AssignHelpContext;
    procedure UpdateStatusBar(ARow, ACol: Integer);
    procedure UpdateMaxTaxaNamePixels;
    procedure UpdateMaxGpNamePixels;
    procedure UpdateFirstColWidth;       // automaticallycalls the above two
  private
    // Private properites used for display functions; they are context free
    // and must be used for display etc.  They are all 0-based and work only on
    // nonfixed Rows and columns for only displayed data

  public
    procedure Initialize;
    procedure ApplyUpdate;

    property HasDataSubsetChanged: Boolean read FHasDataSubsetChanged;
    property HasDataViewChanged:   Boolean read FHasDataViewChanged;
  end;

  procedure AvgGpsCalculation(MyComputeType: TDistType);
var
  V_DistDataExplorer: TV_DistDataExplorer;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  MMegaWindowInfo,
  {$ENDIF}
  Printers, MegaUtils, Mega_Main, MegaErrUtils, math,
  ContextHelp_HC, ErrorMessages_HC, MTaxaGpsDlg, DataExplorerHelp_HC,
  MegaVerConsts, MVS_DistDataExplorer, mhelpfiles, mhelpkeywords,
  mbrowserutils, htmloptionsdlg, MegaPrivateFiles, mcrossprojectutils;

{$R *.lfm}

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
       DispDlgV.Hide; //Since OVC saves state, we don't want it showing when we create it before we populate its data

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
end;

procedure TV_DistDataExplorer.FormActivate(Sender: TObject);
begin
  If VS_DistDataExplorer.IsFormInit = False then
  begin
    UpdateFirstColWidth;
    VS_DistDataExplorer.IsFormInit := True;
  end;
  {$IFDEF DARWIN}
  MainToolbar.Invalidate;
  {$ENDIF}
  DataGrid.Invalidate;
end;

procedure TV_DistDataExplorer.DispChangeFontItemClick(Sender: TObject);
begin
  FontDialog1.Font := DataGrid.Canvas.Font;
  FontDialog1.Font.Color := clBlack; //Forcing color to be black, even though we don't set the color it sometimes turns to white.
  if FontDialog1.Execute then
  begin
    DataGrid.Font := FontDialog1.Font;
    with DataGrid, Canvas do
    begin
      Font := FontDialog1.Font;
      DataGrid.DefaultColWidth :=  Canvas.TextWidth('W')*4 + 2;  //since FIXED fonts, it must work
      DataGrid.DefaultRowHeight := TextHeight('W')+4; //since FIXED fonts, it must work
      UpdateFirstColWidth;
      DataGrid.Refresh;
      VS_DistDataExplorer.CurFont := DataGrid.Font;
    end;
  end;

  FHasDataViewChanged := True;
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

procedure TV_DistDataExplorer.DataGridColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if not IsColumn then
    DataGridRowMoved(Sender, sIndex, tIndex);
end;

procedure TV_DistDataExplorer.DataGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if VS_DistDataExplorer.FIsResizing then
    begin
      if X > 15 then // Can't have col 0 smaller than 15 pixels
      DataGrid.ColWidths[0] := X;
    end;

  if (abs(X-DataGrid.ColWidths[0]) < 5) or (VS_DistDataexplorer.FIsResizing) then
  begin
    DataGrid.Cursor := crHSplit;
    DataGrid.Options := DataGrid.Options - [goRowMoving]; //If we show them the split arrow they can split but not move rows otherwise rows are accidentally moved
 end
 else
 begin
   DataGrid.Options := DataGrid.Options + [goRowMoving]; //They are not at a location to resize the first column so allow them to move rows
   DataGrid.Cursor := crArrow;
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
  DataGrid.DefaultColWidth := 15;
  DataGrid.ColWidths[0] := Col0Width;
  DataGrid.Invalidate;
  SpinEdit1.Value := VS_DistDataExplorer.FPrecision;

  FHasDataViewChanged := True;
end;

procedure TV_DistDataExplorer.AvgAllItemClick(Sender: TObject);
begin
  MessageDlg('The overall average is '+
     FloatToStrF(D_InputDistData.GetAvgAll,ffFixed,VS_DistDataExplorer.FPrecision+4,VS_DistDataExplorer.FPrecision),mtInformation, [mbOK], 0);
end;

procedure TV_DistDataExplorer.InitMainMenu;
begin
  {$IFNDEF DARWIN}
  MainMenu1.OwnerDraw := True;
  MainMenu1.OnMeasureItem := MegaForm.MeasureMenuItem;
  MainMenu1.OnDrawItem := MegaForm.DrawMenuItem
  {$ENDIF}
end;

procedure TV_DistDataExplorer.ProcessWebOptionsMessage(const Browser: ICefBrowser; const message: ICefProcessMessage; out Result: Boolean);
var
  ExportOptions : TDistExportOptions = nil;
  fileType: String;
  Results: TStringList = nil;
  layoutStr: String;
  includeStdErrs: String;
  temp: String;
begin
  try
    try
      HtmlOptionsDialog.Hide;
      temp := message.ArgumentList.GetString(0);
      if Pos(RENDER_PROCESS_ERROR, temp) > 0 then
        raise Exception.Create(temp);
      ExportOptions := TDistExportOptions.Create;
      ExportOptions.AllowNegative := False;
      fileType := message.ArgumentList.GetString(DMEO_FILE_FORMAT_INDEX);
      ExportOptions.OutputFormat := StringToOutputFileType(fileType);
      layoutStr := message.ArgumentList.GetString(DMEO_EXPORT_TYPE_INDEX);
      if SameText(layoutStr, DMEO_MATRIX) then
      begin
        ExportOptions.OutputAsCols := False;
        ExportOptions.FIsColumnar := False;
      end
      else
      begin
        ExportOptions.OutputAsCols := True;
        ExportOptions.FIsColumnar := False;
      end;

      layoutStr := message.ArgumentList.GetString(DMEO_MATRIX_LAYOUT1_INDEX);
      if SameText(layoutStr, DMEO_UPPER_RIGHT) then
        ExportOptions.LowerLeft := False
      else
        ExportOptions.LowerLeft := True;

      layoutStr := message.ArgumentList.GetString(DMEO_MATRIX_LAYOUT2_INDEX);
      if SameText(layoutStr, DMEO_OPPOSITE_SIDES) then
        ExportOptions.OppositeSides := True
      else
        ExportOptions.OppositeSides := False;

      ExportOptions.Precision := message.ArgumentList.GetInt(DMEO_PRECISION_INDEX);
      includeStdErrs := message.ArgumentList.GetString(DMEO_SHOW_DISTANCES_AND_STD_ERRORS_INDEX);
      ExportOptions.AllowStandardError := StrToBool(includeStdErrs);

      if ExportOptions.IsSpreadsheetFormat and (ExportOptions.OutputFormat <> ExportCSV) then
        ExportOptions.SaveExcelFileTo := GetSaveLocation(ExportOptions.OutputFormat);
      Results := D_InputDistData.WriteExportToFile(ExportOptions);
      if ExportOptions.IsSpreadsheetFormat and (ExportOptions.OutputFormat <> ExportCSV) then
        RunAProgram(ExportOptions.SaveExcelFileTo)
      else if (not ExportOptions.IsSpreadsheetFormat) or (ExportOptions.OutputFormat = ExportCSV) then
        OpenStringList(Results, 'Distance Data');
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(ExportOptions) then
      ExportOptions.Free;
    if Assigned(Results) then
      Results.Free;
  end;
end;

procedure TV_DistDataExplorer.DispGpNamesItemClick(Sender: TObject);
begin
  DispGpNamesItem.Checked := not DispGpNamesItem.Checked;
  VS_DistDataExplorer.DispShowGpNames := DispGpNamesItem.Checked;
  UpdateFirstColWidth;
  DataGrid.Refresh;

  FHasDataViewChanged := True;
end;

procedure TV_DistDataExplorer.DispSelTaxaItemClick(Sender: TObject);
begin
  with Sender as TMenuItem do
    Checked := not Checked;
  VS_DistDataExplorer.DispSelTaxaItem := (Sender as TMenuItem).Checked;
  D_InputDistData.UpdateDispTaxaList;
  DataGrid.RowCount := D_InputDistData.DispTaxaList.Count+1;
  DataGrid.ColCount := D_InputDistData.DispTaxaList.Count+1;
  DataGrid.Refresh;

  FHasDataViewChanged := True;
end;

procedure TV_DistDataExplorer.DispSortItemClick(Sender: TObject);
begin
  try
    if Sender = DispAsInInputFileItem then
      D_InputDistData.FOtuInfos.SortByInputOrder
    else if Sender = DispSortAlphaItem then
      D_InputDistData.FOtuInfos.SortByTaxaNameAsc
    else if Sender = DispSortGpItem then
      D_InputDistData.FOtuInfos.SortByGroupNameAsc;

    D_InputDistData.UpdateDispTaxaList;  //added 12/12/08 Nick P. fixing bug prior to mega 4.0 where un-checked taxa would show when 'show taxa selected only' was turned on and created session saving problems
    FHasDataViewChanged := True;
  finally
    DataGrid.Refresh;
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
  DataGrid.Invalidate;
end;

procedure TV_DistDataExplorer.FormShow(Sender: TObject);
begin
 if VS_DistDataExplorer.CurFont = nil then
  begin
    VS_DistDataExplorer.CurFont := TFont.Create;
    VS_DistDataExplorer.CurFont.Assign(DataGrid.Font);
  end
  else
    VS_DistDataExplorer.CurFont.Assign(DataGrid.Font);
  if VS_DistDataExplorer.FPrecision = -1 then //If we do not already have a precision set in the visual status (saved sessions have a precision in visual status and we don't want to overwrite them)
  begin
    VS_DistDataExplorer.FPrecision := Trunc(SpinEdit1.Value);
    VS_DistDataExplorer.DispShowGpNames := DispGpNamesItem.Checked;
    VS_DistDataExplorer.DispSelTaxaItem := DispSelTaxaItem.Checked;
    DecPrecisionSBtn.Enabled := VS_DistDataExplorer.FPrecision > 0;
    IncPrecisionSBtn.Enabled := VS_DistDataExplorer.FPrecision < 10;
    Canvas.Font := DataGrid.Font;
    DataGrid.DefaultColWidth :=  Canvas.TextWidth('W')*4 + 2;  //since FIXED fonts, it must work
    DataGrid.DefaultRowHeight := Canvas.TextHeight('W')+4; //since FIXED fonts, it must work
    UpdateFirstColWidth;
  end;
  Invalidate;
  Application.ProcessMessages;
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
  DataGrid.Invalidate;
  SpinEdit1.Value := VS_DistDataExplorer.FPrecision;

  FHasDataViewChanged := True;
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
  js: String;
begin
  try
    HtmlOptionsDialog.ProcessMessageReceivedProc := ProcessWebOptionsMessage;
    HtmlOptionsDialog.DomProcedureName := VISITDOMPROC_DISTEXPORTOPTIONS;
    HtmlOptionsDialog.Show;

    js := EmptyStr;
    if Sender = ExportMEGATBtn then
      js := Format('$("#%s").val("%s").trigger("change");', [DMEO_FILE_FORMAT_ID, MEF_MEGA]) + LineEnding
    else if Sender = ExportXLTBtn then
      js := Format('$("#%s").val("%s").trigger("change");', [DMEO_FILE_FORMAT_ID, MEF_EXCEL]) + LineEnding
    else if Sender = ExportCSVTBtn then
      js := Format('$("#%s").val("%s").trigger("change");', [DMEO_FILE_FORMAT_ID, MEF_CSV]) + LineEnding
    else if Sender = ExportTextTBtn then
      js := Format('$("#%s").val("%s").trigger("change");', [DMEO_FILE_FORMAT_ID, MEF_TEXT]) + LineEnding;
    js := js + Format('$("#%s").hide();', [DMEO_SHOW_DISTANCES_AND_STD_ERRORS_DIV_ID]) + LineEnding;
    js := js + Format('$("#%s").remove();', [DMEO_DIST_STD_ERRS]) + LineEnding;
    js := Trim(js + LineEnding + Format('$("#%s").val("%d");', [DMEO_PRECISION_ID, VS_DistDataExplorer.FPrecision]));
    HtmlOptionsDialog.LoadOptionsFile(wofDistDataExportFile, js, 'Distance Data Export Options', 390, 390, HC_Export_Data);
  except
    On E: Exception do
    begin
      E.HelpContext := HC_Unexpected_Error;
      ShowErrorMessage(E);
    end;
  end;
end;

procedure TV_DistDataExplorer.DataGridRowMoved(Sender: TObject; FromIndex,ToIndex: Longint);
begin
  if FromIndex = ToIndex then
    Exit;
  D_InputDistData.FOtuInfos.Move(D_InputDistData.DispTaxaList[FromIndex-1], D_InputDistData.DispTaxaList[ToIndex-1]);
  D_InputDistData.UpdateDispTaxaList;
  DataGrid.Refresh;
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
    Exit; // If you were resizing, you couldn't have done anything else.
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
  PrevBrushColor: TColor;
  Col0Width: Integer;
  aTextStyle: TTextStyle;
begin
  if not Visible then Exit;
  with DataGrid, Canvas do
  begin
    if (ACol > DataGrid.ColCount) or (ARow > DataGrid.RowCount) then // cell is outside the range
      Exit;
    aTextStyle := TextStyle;
    aTextStyle.SystemFont := False;
    PrevBrushColor := Brush.Color;
    if (ACol = 0) and (ARow = 0) then  // Top-Left cell
    begin
      Brush.Color := DataGrid.FixedColor;
      VS_DistDataExplorer.DispStr := '';
      FillRect(aRect);
      TextOut(aRect.Left+2, aRect.Top+2, VS_DistDataExplorer.DispStr);
    end
    else if (ACol = 0) then  // fixed column (Leftmost); used to display Taxa Labels
    begin
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
      TextRect(aRect, aRect.Left, aRect.Left, VS_DistDataExplorer.DispStr, TextStyle);
      if DispGpNamesItem.Checked then
      begin
        if Length(D_InputDistData.GpName[ARow-1]) > 0 then
          TextRect(aRect, X, Y, IntToStr(ARow)+'. '+ D_InputDistData.TaxaName[ARow-1]+' {'+D_InputDistData.GpName[ARow-1]+'}', TextStyle)
        else
          TextRect(aRect, X, Y, IntToStr(ARow)+'. '+D_InputDistData.TaxaName[ARow-1], TextStyle);
      end
      else
        TextRect(aRect, X, Y, D_InputDistData.TaxaName[ARow-1], TextStyle);
    end
    else if (ARow = 0) then  // fixed row (Top Row);
    begin
      //if ACol = 1 then DataGrid.ColWidths[ACol] := 8;
      VS_DistDataExplorer.DispStr := D_InputDistData.ColName[ACol-1];
      Brush.Color := DataGrid.FixedColor;
      FillRect(aRect);
      aTextStyle.Alignment := taCenter;
      TextStyle := aTextStyle;
      TextRect(aRect, (aRect.Left+aRect.Right) div 2, aRect.Top + 2, VS_DistDataExplorer.DispStr, TextStyle);
    end
    else if (ARow > 0) and (ACol > 0) then  // if a cell with data
    begin
      Font.Color := clGray;
      if D_InputDistData.TaxaUsed[ARow-1] and D_InputDistData.TaxaUsed[ACol-1] then
        Font.Color := clBlack;

      if gdFocused in State then
      begin
        Brush.Color := clNavy;
        Font.Color  := clWhite;
      end;
      FillRect(aRect);

      if (ARow <= ACol) then
      begin
        FillRect(aRect);
        VS_DistDataExplorer.DispStr := EmptyStr;
        TextRect(aRect, aRect.Left+2, aRect.Top+2, VS_DistDataExplorer.DispStr);
      end
      else
      begin
        d := D_InputDistData.CellContent(ARow-1, ACol-1, InvalidNum);
        if InValidNum then
        begin
          FillRect(aRect);
          VS_DistDataExplorer.DispStr := 'n/c';
          Font.Color := clRed;
          aTextStyle.Alignment := taCenter;
          TextStyle := aTextStyle;
          TextRect(aRect, (aRect.Left+aRect.Right) div 2, aRect.Top+2, VS_DistDataExplorer.DispStr, TextStyle);
        end
        else if VS_DistDataExplorer.FPrecision = 0 then
        begin
          FillRect(aRect);
          VS_DistDataExplorer.DispStr := IntToStr(Round(d));  // Round's off, not floor it
          aTextStyle.Alignment := taRightJustify;
          TextStyle := aTextStyle;
          TextRect(aRect, aRect.Right-1, aRect.Top+2, VS_DistDataExplorer.DispStr + ' ', TextStyle);  // used to be right-2
        end
        else
        begin
          FillRect(aRect);
          VS_DistDataExplorer.DispStr := Format('%.' + IntToStr(VS_DistDataExplorer.FPrecision) + 'f ', [d]);
          aTextStyle.Alignment := taRightJustify;
          TextStyle := aTextStyle;
          TextRect(aRect, aRect.Left + 2, aRect.Top + 2, VS_DistDataExplorer.DispStr, TextStyle);
        end;
      end;
      Brush.Color := PrevBrushColor;
      Pen.Color   := clblack;
      if DataGrid.Canvas.TextWidth(VS_DistDataExplorer.DispStr)+10 > DataGrid.DefaultColWidth then // If the current strin we're writing out is longer than the default column width, then expand the width
      begin
        Col0Width := DataGrid.ColWidths[0]; // Col0's width is independent so back it up and restore it.
        DataGrid.DefaultColWidth := DataGrid.Canvas.TextWidth(VS_DistDataExplorer.DispStr)+10;
        DataGrid.ColWidths[0] := Col0Width;
      end;
    end;
    if (gdFocused in State) then
    begin
      //DrawFocusRect(aRect);
      UpdateStatusBar(ARow, ACol);
    end;
  end;
end;

procedure TV_DistDataExplorer.FormCreate(Sender: TObject);
begin
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

//  if FDistMat   <> nil then  FreeDistMatrix(FDistMat, FNoOfTaxa); FDistMat := nil;
  //if OutputMatrixDlg <> nil then OutputMatrixDlg.Destroy; OutputMatrixDlg := nil;
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
 DispGpNamesItem.Visible := True; //(FNoOfGps > 0);
 DispSortGpItem.Visible := True; //(FNoOfGps > 0);
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
  //Get rid of the D_InputDistData.GpName issue that I made by replacing
  if DispGpNamesItem.Checked then
    TempInt := TempInt + VS_DistDataExplorer.MaxGpNamePixels;
  if DispGpNamesItem.Checked then
    with DataGrid do
      TempInt := TempInt + Canvas.TextWidth('{ }');
  DataGrid.ColWidths[0] := TempInt;
end;

procedure TV_DistDataExplorer.Initialize;
begin
  try
    // Setup the displayed taxa; default: keep all taxa
    VS_DistDataExplorer.FPrecision := 4;
    SpinEdit1.Value := VS_DistDataExplorer.FPrecision;
    DispSelTaxaItem.Checked := False;
    // Displaying Names etc.
    DispGpNamesItem.Checked := True;
    with DataGrid do
    begin
      RowCount := D_InputDistData.FNoOfTaxa+1;
      ColCount := D_InputDistData.FNoOfTaxa+1;
      //DefaultColWidth  := Canvas.TextWidth('W')*4 + 2; //since FIXED fonts, it must work
      DefaultRowHeight := Canvas.TextHeight('W')+4; //since FIXED fonts, it must work
      UpdateFirstColWidth;
      //DistCellSzTrackBar.Position := Ceil(DefaultColWidth/5);
    end;
    AdjustEnabledStatus;
    AssignHelpContext;

    // computes the decimal point position
    ComputeLeftOfDecimalLen;
    AvgAllItem.Enabled := True;
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
    UpdateFirstColWidth;
    DataGrid.Invalidate;
  end;
end;

end.

