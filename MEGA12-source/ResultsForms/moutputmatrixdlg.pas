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

unit MOutputMatrixDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Spin, Buttons, StdCtrls, IniPropStorage, ComCtrls, ActnList,
  MegaConsts, ExcelWrite, MD_InputDistData;

type

  { TOutputMatrixDlg }

  TOutputMatrixDlg = class(TForm)
    ExportTypeLabel: TLabel;
    HelpAction: TAction;
    CancelAction: TAction;
    Label2: TLabel;
    OkAction: TAction;
    ActionList1: TActionList;
    IniPropStorage1: TIniPropStorage;
    MatrixSideComboBx: TComboBox;
    Panel1: TPanel;
    DistancesBtn: TRadioButton;
    StdErrBtn: TRadioButton;
    SaveDlg: TSaveDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    OutputFormatComboBox: TComboBox;
    MatrixFormatComboBx: TComboBox;
    Label4: TLabel;
    PrecisionLabel: TLabel;
    MatrixFormatLabel: TLabel;
    MatrixSideLabel: TLabel;
    PrecisionSE: TSpinEdit;
    TypeCombo: TComboBox;
    procedure CancelActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
    procedure OkActionExecute(Sender: TObject);
    procedure OutputFormatComboBoxChange(Sender: TObject);
    procedure StdErrBtnChange(Sender: TObject);
    procedure TypeComboChange(Sender: TObject);
    procedure WriteRdGpClick(Sender: TObject);
  private
    EntriesSEValue: Integer;
    FNoOfRows: Integer;
    FNoOfCols: Integer;
    FRowNames: TStringList;

    FTitle: String;
    FDescription: TStringList;
    FPrecision: Integer;
    FEPrecision: String;
    FAllowNegative: Boolean;
    FIsColumnar: Boolean;

    FAllowStandardError: Boolean;
    FAllowMEGAFormat: Boolean;
    FAllowExcelFormat: Boolean;
    FAllowCsvFormat: Boolean;
    FAllowTextFormat: Boolean;
    FEntriesPerLine: Integer;
    FDistMat: PDistanceMatrix;
    FStdErrMat: PDistanceMatrix;
    FSilentWrite: Boolean;
    FDSide: Integer;

    FDAcronym: String;
    FVAcronym: String;

    function  GetMaxRowNameLen: Longint;
    procedure SetAllowStandardError(AValue: Boolean);

    procedure SetRowName(Index: Integer; Value: String);
    procedure SetDescription(AValue: TStringList);
    procedure UpdateForm;
  public
    disableMatrixFormat: Boolean;
    procedure  SetExportToMEGA;
    procedure  SetExportToXL;
    procedure  SetExportToODS;
    procedure  SetExportToCSV;
    procedure  SetExportToText;

    function  IsExportToMEGA: Boolean;
    function  IsExportToXL: Boolean;
    function  IsExportToCSV: Boolean;
    function  IsExportToODS: Boolean;
    function  IsExportToText: Boolean;
    {$IFDEF VISUAL_BUILD}
    function GetExportOptions: TDistExportOptions;
    {$ENDIF}
    property NoOfRows: Integer read FNoOfRows write FNoOfRows;
    property NoOfCols: Integer read FNoOfCols write FNoOfCols;
    property AllowMEGAFormat: Boolean read FAllowMEGAFormat write FAllowMEGAFormat;
    property AllowExcelFormat: Boolean read  FAllowExcelFormat write FAllowExcelFormat;
    property AllowCsvFormat: Boolean read FAllowCsvFormat write FAllowCsvFormat;
    property AllowTextFormat: Boolean read FAllowTextFormat write FAllowTextFormat;

    property AllowStandardError: Boolean read FAllowStandardError write SetAllowStandardError;
    property IsColumnar: Boolean read FIsColumnar write FIsColumnar;

    property Title: String read FTitle write FTitle;
    property Description: TStringList write SetDescription;

    property DAcronym: String read FDAcronym write FDAcronym;
    property VAcronym: String read FVAcronym write FVAcronym;

    property RowName[Index: Integer]: String write SetRowName;

    property DistMat: PDistanceMatrix read FDistMat write FDistMat;
    property StdErrMat: PDistanceMatrix read FStdErrMat write FStdErrMat;

    property Precision: Integer read FPrecision write FPrecision;
    property AllowNegative: Boolean read FAllowNegative write FAllowNegative;
    property SilentWrite: Boolean read FSilentWrite write FSilentWrite;
    property DSide: Integer read FDSide write FDSide;
  end;

var
  OutputMatrixDlg: TOutputMatrixDlg;
  PrevMatrixFormat: Integer;

implementation

uses
  mimageform, MegaErrUtils, MegaVerConsts, MegaUtils, DataExplorerHelp_HC, ErrorMessages_HC,
  mhelpfiles, mhelpkeywords;

{$R *.lfm}

procedure TOutputMatrixDlg.FormCreate(Sender: TObject);
begin
  HelpContext := HC_Dist_Exp_Options_dialog_box;
  HelpKeyword := MapHelpContextToKeyword(HelpContext);
  FNoOfRows   := 0;
  FNoOfCols   := 0;
  FRowNames    := TStringList.Create;
  FIsColumnar := False;

  FTitle       := EmptyStr;
  FDescription := TStringList.Create;

  FPrecision     := 2;
  FEPrecision    := '0.00'; // Excel's precision format
  FDistMat       := nil;
  FStdErrMat     := nil;
  FAllowNegative := False;
  FAllowMEGAFormat:= True;
  FAllowExcelFormat := True;
  FAllowCsvFormat := True;
  FAllowTextFormat := True;

  OutputFormatComboBox.Items.Clear;
  OutputFormatComboBox.Items.Add(XLSX_FORMAT_STR);
  OutputFormatComboBox.Items.Add(XLS_FORMAT_STR);
  OutputFormatComboBox.Items.Add(ODS_FORMAT_STR);
  OutputFormatComboBox.Items.Add(CSV_FORMAT_STR);
  OutputFormatComboBox.Items.Add(NO_FORMAT_STR);
  OutputFormatComboBox.Items.Add(MEGA_FORMAT_STR);
  OutputFormatComboBox.ItemIndex := 3;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+': Distance Data Export Options';
  PrevMatrixFormat := MatrixFormatComboBx.ItemIndex;
  {$IFDEF VISUAL_BUILD}
  ImageForm.UpdateImgList(Self);
  {$ENDIF}
end;

procedure TOutputMatrixDlg.FormActivate(Sender: TObject);
begin
  if FNoOfRows > 1 then
    EntriesSEValue := 2
  else
    EntriesSEValue := 1;

  if not FAllowMEGAFormat then
    OutputFormatComboBox.Items.Delete(OutputFormatComboBox.Items.IndexOf(MEGA_FORMAT_STR));

  if not AllowExcelFormat then
    OutputFormatComboBox.Items.Delete(OutputFormatComboBox.Items.IndexOf(XLSX_FORMAT_STR));

  if not AllowCsvFormat then
    OutputFormatComboBox.Items.Delete(OutputFormatComboBox.Items.IndexOf(CSV_FORMAT_STR));

  if not AllowTextFormat then
    OutputFormatComboBox.Items.Delete(OutputFormatComboBox.Items.IndexOf(NO_FORMAT_STR));

  FEntriesPerLine := FNoOfRows;
  EntriesSEvalue := FEntriesPerLine;

  // information about dist or dist+stdErr
  MatrixSideComboBx.ItemIndex := 0;
  if FAllowStandardError then
    StdErrBtn.Checked := True
  else
    StdErrBtn.Checked := False;

  // lower left or upper right
  MatrixSideComboBx.Enabled := FAllowMEGAFormat and (TypeCombo.ItemIndex = TypeCombo.Items.IndexOf('Matrix'));
  MatrixSideLabel.Enabled := MatrixSideComboBx.Visible;

  //same side or opossite sides
  MatrixFormatComboBx.Enabled := FAllowMEGAFormat and AllowStandardError and (TypeCombo.ItemIndex = TypeCombo.Items.IndexOf('Matrix'));
  MatrixFormatLabel.Enabled := MatrixFormatComboBx.Enabled;
  TypeCombo.Enabled := (not IsExportToMega);
  ExportTypeLabel.Enabled := (not IsExportToMega);
  PrecisionSE.Value := FPrecision;
  UpdateForm;

  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TOutputMatrixDlg.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TOutputMatrixDlg.UpdateForm;
var
  isMatrixSelected: Boolean;
begin
  if IsExportToMega then
    TypeCombo.ItemIndex := TypeCombo.Items.IndexOf('Matrix');
  isMatrixSelected := (TypeCombo.ItemIndex = TypeCombo.Items.IndexOf('Matrix'));
  TypeCombo.Enabled := (not IsExportToMega);
  ExportTypeLabel.Enabled := TypeCombo.Enabled;

  MatrixSideComboBx.Enabled := FAllowMEGAFormat and isMatrixSelected;
  MatrixSideLabel.Enabled   := MatrixSideComboBx.Enabled;

  MatrixFormatComboBx.Enabled := StdErrBtn.Checked and FAllowMEGAFormat and AllowStandardError and (TypeCombo.ItemIndex = TypeCombo.Items.IndexOf('Matrix'));
  MatrixFormatLabel.Enabled := MatrixFormatComboBx.Enabled;
  MatrixFormatComboBx.ItemIndex := PrevMatrixFormat;
  if ((FNoOfCols > 16384) and (isExportToXL or isExportToODS)) or (FNoOfCols = 1) or disableMatrixFormat then
  begin
   // If there's going to be more than 16384 Columns then the square view wouldn't fit in Excel, so we force them to use the column format.
    TypeCombo.ItemIndex := 1;
    TypeCombo.Enabled := False;
  end;
  if ((FNoOfRows > 1048576) or (((FNoOfRows * FNoOfRows) > 1048576) and (not isMatrixSelected))) and isExportToXL then
    ShowMessage('MEGA has a limit of 1,048,576 rows for worksheets and because your data exceeds this limit, it will be truncated.' + LineEnding + 'Alternatively, you may export this in CSV format so the data will not be truncated.');
end;

procedure TOutputMatrixDlg.FormDestroy(Sender: TObject);
begin
  FRowNames.Free;
  FDescription.Free;
end;

procedure TOutputMatrixDlg.FormResize(Sender: TObject);
begin
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TOutputMatrixDlg.HelpActionExecute(Sender: TObject);
begin
  ShowContextSensitiveHelp(HelpKeyword);
end;

procedure TOutputMatrixDlg.OkActionExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

function TOutputMatrixDlg.GetMaxRowNameLen: Longint;
var
  i: Integer;
begin
  Result := -1;
  for i:=0 to FNoOfRows-1 do
    if Length(FRowNames[i]) > Result then
      Result := Length(FRowNames[i]);
end;

procedure TOutputMatrixDlg.SetAllowStandardError(AValue: Boolean);
begin
  FAllowStandardError := AValue;
  StdErrBtn.Enabled := FAllowStandardError;
end;

procedure TOutputMatrixDlg.SetRowName(Index: Integer; Value: String);
begin
  FRowNames.Add(Value);
end;

procedure TOutputMatrixDlg.SetDescription(AValue: TStringList);
var
  i: Integer;
begin
  FDescription.Clear;
  for i:=0 to AValue.Count-1 do
    FDescription.Add(AValue[i]);
end;

{$IFDEF VISUAL_BUILD}
function TOutputMatrixDlg.GetExportOptions:TDistExportOptions;
var
  aFormat: String;
begin
  Result := TDistExportOptions.Create;
  aFormat := OutputFormatComboBox.Items[OutputFormatComboBox.ItemIndex];
  if aFormat = XLSX_FORMAT_STR then
    Result.OutputFormat := ExportExelXML
  else if aFormat = XLS_FORMAT_STR then
    Result.OutputFormat := ExportExcel
  else if aFormat = ODS_FORMAT_STR then
    Result.OutputFormat := ExportODS
  else if aFormat = CSV_FORMAT_STR then
    Result.OutputFormat := ExportCSV
  else if aFormat = NO_FORMAT_STR then
    Result.OutputFormat := ExportText
  else if aFormat = MEGA_FORMAT_STR then
    Result.OutputFormat := ExportMega
  else
    RaiseErrorMessage(HC_Unexpected_Error, 'Invalid export format');
  Result.AllowStandardError := AllowStandardError;
  Result.FDAcronym := FDAcronym;
  Result.FVAcronym := FVAcronym;
  If IsWorkbookFormat(Result.OutputFormat) then
  begin
    Result.SaveExcelFileTo := ExcelWrite.GetSaveLocation(Result.OutputFormat); //handles if they don't have excel
    if Result.SaveExcelFileTo = EmptyStr then
      Exit;
  end;
  Result.NoOfRows := NoOfRows;
  Result.Precision      := Trunc(PrecisionSE.Value);
  Result.OutputAsCols := (TypeCombo.ItemIndex = TypeCombo.Items.IndexOf('Column')) and (Result.OutputFormat <> ExportMega); // Square may be deleted and the index value may change so we check this way.
  Result.FIsColumnar := FIsColumnar;
  Result.AllowNegative := FAllowNegative;
  Result.LowerLeft := (MatrixSideComboBx.ItemIndex = 0);
  Result.WithStdErr := StdErrBtn.Checked;
  if FIsColumnar then
    Result.FEntriesPerLine := FNoOfRows
  else
    Result.FEntriesPerLine := EntriesSEValue;
  Result.PrevMatrixFormat := MatrixFormatComboBx.ItemIndex;
end;
{$ENDIF}

procedure TOutputMatrixDlg.SetExportToMEGA;
begin
  OutputFormatComboBox.ItemIndex := OutputFormatComboBox.Items.IndexOf(MEGA_FORMAT_STR);
end;

function TOutputMatrixDlg.IsExportToMEGA: Boolean;
begin
  if OutputFormatComboBox.ItemIndex >= 0 then
    Result := OutputFormatComboBox.Items[OutputFormatComboBox.ItemIndex] = MEGA_FORMAT_STR
  else
    Result := False;
end;

procedure TOutputMatrixDlg.SetExportToXL;
begin
  OutputFormatComboBox.ItemIndex :=  OutputFormatComboBox.Items.IndexOf(XLSX_FORMAT_STR);
  if TypeCombo.Items.Count > 1 then  // if excel is available select column format by default.
    TypeCombo.ItemIndex := 1;
end;

procedure TOutputMatrixDlg.SetExportToODS;
begin
  OutputFormatComboBox.ItemIndex := OutputFormatComboBox.Items.IndexOf(ODS_FORMAT_STR);
end;

function TOutputMatrixDlg.IsExportToXL: Boolean;
begin
  Result := OutputFormatComboBox.Items[OutputFormatComboBox.ItemIndex] = XLSX_FORMAT_STR;
end;

procedure TOutputMatrixDlg.SetExportToCSV;
begin
  OutputFormatComboBox.ItemIndex := OutputFormatComboBox.Items.IndexOf(CSV_FORMAT_STR);
end;

function TOutputMatrixDlg.IsExportToCSV: Boolean;
begin
  Result := OutputFormatComboBox.Items[OutputFormatComboBox.ItemIndex] = CSV_FORMAT_STR;
end;

function TOutputMatrixDlg.IsExportToODS: Boolean;
begin
  Result := OutputFormatComboBox.Items[OutputFormatComboBox.ItemIndex] = ODS_FORMAT_STR;
end;

procedure  TOutputMatrixDlg.SetExportToText;
begin
  OutputFormatComboBox.ItemIndex := OutputFormatComboBox.Items.IndexOf(NO_FORMAT_STR);
end;

function TOutputMatrixDlg.IsExportToText: Boolean;
begin
  Result := OutputFormatComboBox.Items[OutputFormatComboBox.ItemIndex] = NO_FORMAT_STR;
end;

procedure TOutputMatrixDlg.OutputFormatComboBoxChange(Sender: TObject);
begin
  UpdateForm;
end;

procedure TOutputMatrixDlg.StdErrBtnChange(Sender: TObject);
begin
  UpdateForm;
end;

procedure TOutputMatrixDlg.TypeComboChange(Sender: TObject);
begin
  UpdateForm;
end;

procedure TOutputMatrixDlg.WriteRdGpClick(Sender: TObject);
begin
  UpdateForm;
end;

end.

