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

unit MOutputMatrixDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Spin, Buttons, StdCtrls, MegaConsts, ExcelWrite,
  MD_InputDistData;

type

  { TOutputMatrixDlg }

  TOutputMatrixDlg = class(TForm)
    MatrixSideComboBx: TComboBox;
    SaveDlg: TSaveDialog;
    TypeCombo: TComboBox;
    Label1: TLabel;
    OutputFormatComboBox: TComboBox;
    HelpBtn: TBitBtn;
    CancelBtn: TBitBtn;
    OKBtn: TBitBtn;
    MatrixFormatComboBx: TComboBox;
    Label4: TLabel;
    MaxEntriesLabel: TLabel;
    ExportType: TPanel;
    PrecisionLabel: TLabel;
    MatrixFormatLabel: TLabel;
    MatrixSideLabel: TLabel;
    PrecisionSE: TSpinEdit;
    WriteRdGp: TRadioGroup;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OutputFormatComboBoxChange(Sender: TObject);
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

    property AllowStandardError: Boolean read FAllowStandardError write FAllowStandardError;
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
  {$IFDEF VISUAL_BUILD}
  mimageform,
  {$ENDIF}
  MegaErrUtils, MegaVerConsts, MegaUtils,
  DataExplorerHelp_HC, ErrorMessages_HC;

{$R *.lfm}

procedure TOutputMatrixDlg.FormCreate(Sender: TObject);
begin
  HelpContext := HC_Dist_Exp_Options_dialog_box;
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
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+': Distance Write-out Options';
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
    OutputFormatComboBox.Items.Delete(OutputFormatComboBox.Items.IndexOf(MEGA_FORMAT_STR)); // 3 is MEGA

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
  if (not FAllowStandardError) then
  begin
    WriteRdGp.ItemIndex := 0;
    if WriteRdGp.Items.Count > 1
      then WriteRdGp.Items.Delete(1);
  end
  else
    WriteRdGp.ItemIndex := 1;

  // lower left or upper right
  MatrixSideComboBx.Visible := FAllowMEGAFormat AND (TypeCombo.ItemIndex = TypeCombo.Items.IndexOf('Matrix [255 cols max]'));
  MatrixSideLabel.Visible   := MatrixSideComboBx.Visible;

  //same side or opossite sides
  MatrixFormatComboBx.Visible := (WriteRdGp.Items.Count = 2) AND (FAllowMEGAFormat) and (AllowStandardError) and (TypeCombo.ItemIndex = TypeCombo.Items.IndexOf('Matrix [255 cols max]'));
  MatrixFormatLabel.Visible := MatrixFormatComboBx.Visible;
  //MatrixFormatComboBx.ItemIndex := PrevMatrixFormat;
  ExportType.Visible := (not IsExportToMega);
  PrecisionSE.Value := FPrecision;
  UpdateForm;
end;

procedure TOutputMatrixDlg.UpdateForm;
var
  isMatrixSelected: Boolean;
begin
  isMatrixSelected := (TypeCombo.ItemIndex = TypeCombo.Items.IndexOf('Matrix [255 cols max]'));
  ExportType.Visible := (not IsExportToMega);
  if not ExportType.Visible then
    TypeCombo.ItemIndex := 0;

  MatrixSideComboBx.Visible := FAllowMEGAFormat AND isMatrixSelected;
  MatrixSideLabel.Visible   := MatrixSideComboBx.Visible;

  //same side or opposite sides
  MatrixFormatComboBx.Visible := (WriteRdGp.Items.Count = 2) AND (WriteRdGp.ItemIndex = 1) AND (FAllowMEGAFormat) and (AllowStandardError) and (TypeCombo.ItemIndex = TypeCombo.Items.IndexOf('Matrix [255 cols max]'));
  MatrixFormatLabel.Visible := MatrixFormatComboBx.Visible;
  MatrixFormatComboBx.ItemIndex := PrevMatrixFormat;
  if ((FNoOfCols > 1024) and (isExportToXL or isExportToODS)) or (FNoOfCols = 1) or disableMatrixFormat then
  begin
   // If there's going to be more than 1024 Columns then the square view wouldn't fit in Excel, so we force them to use the column format.
    TypeCombo.ItemIndex := 1;
    TypeCombo.Enabled := False;
  end
  else
    TypeCombo.Enabled := True;
  if ( (FNoOfRows > 1048576) or (((FNoOfRows * FNoOfRows) > 1048576) and (not isMatrixSelected) ) ) and isExportToXL then
    ShowMessage('MEGA has a limit of 1,048,576 rows for worksheets and because your data exceeds this limit, it will be truncated.' + LineEnding + 'Alternatively, you may export this in CSV format so the data will not be truncated.');
end;

procedure TOutputMatrixDlg.FormDestroy(Sender: TObject);
begin
  FRowNames.Free;
  FDescription.Free;
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

procedure TOutputMatrixDlg.SetRowName(Index: Integer; Value: AnsiString);
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
  Result.WithStdErr := (WriteRdGp.ItemIndex = 1);
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
procedure TOutputMatrixDlg.TypeComboChange(Sender: TObject);
begin
  UpdateForm;
end;

procedure TOutputMatrixDlg.WriteRdGpClick(Sender: TObject);
begin
  UpdateForm;
end;

end.

