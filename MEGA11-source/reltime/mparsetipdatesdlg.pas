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

unit mparsetipdatesdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Grids, StdCtrls, Spin, ActnList, IniPropStorage, Types,
  mtipdatesfinder, mimageform, MegaUtils, MegaPrivateFiles;

type

  { TParseTipDatesForm }

  TParseTipDatesForm = class(TForm)
    ApplyDateBtn: TButton;
    ApplyDelimBtn: TButton;
    IniPropStorage1: TIniPropStorage;
    SaveAction: TAction;
    CancelAction: TAction;
    HelpAction: TAction;
    ActionList1: TActionList;
    PositionComboBox: TComboBox;
    DateFormatEdit: TEdit;
    DelimiterEdit: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    IsNumberBtn: TRadioButton;
    IsDateBtn: TRadioButton;
    Label1: TLabel;
    DigitsEdit: TSpinEdit;
    Label4: TLabel;
    PositionBtn: TRadioButton;
    DelimitedBtn: TRadioButton;
    TipDatesDrawGrid: TDrawGrid;
    HelpBtn: TImage;
    CancelBtn: TImage;
    OkBtn: TImage;
    ButtonImages: TImageList;
    HelpImages: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    StatusBar1: TStatusBar;
    procedure ApplyDelimBtnClick(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure CancelBtnMouseEnter(Sender: TObject);
    procedure CancelBtnMouseLeave(Sender: TObject);
    procedure ApplyDateBtnClick(Sender: TObject);
    procedure DateFormatEditChange(Sender: TObject);
    procedure DateFormatEditEditingDone(Sender: TObject);
    procedure DelimitedBtnChange(Sender: TObject);
    procedure DelimiterEditChange(Sender: TObject);
    procedure DelimiterEditEditingDone(Sender: TObject);
    procedure DigitsEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure HelpBtnMouseEnter(Sender: TObject);
    procedure HelpBtnMouseLeave(Sender: TObject);
    procedure IsDateBtnChange(Sender: TObject);
    procedure IsNumberBtnChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure OkBtnMouseEnter(Sender: TObject);
    procedure OkBtnMouseLeave(Sender: TObject);
    procedure PositionBtnChange(Sender: TObject);
    procedure PositionComboBoxChange(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure TipDatesDrawGridColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
    procedure TipDatesDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
  private
    FIgnoreEvents: Boolean;
    FLabels: TStringList;
    FLatestDate: Double;
    FTipDates: TList;
    FFinder: TTipDatesFinder;
    function ValidateDateFormat: boolean;
    procedure FindLatestDate;
    procedure SetFinder(AValue: TTipDatesFinder);
    procedure SetLabels(AValue: TStringList);
    procedure UpdateColWidths;
  public
    procedure RefreshTipDatesSearch;
    procedure SetTipDates(aList: TList);
    property Finder: TTipDatesFinder read FFinder write SetFinder;
    property Labels: TStringList read FLabels write SetLabels;
  end;

var
  ParseTipDatesForm: TParseTipDatesForm;

implementation

uses
  StringUtils, mshortcutshelper, MegaVerConsts;

{$R *.lfm}

{ TParseTipDatesForm }

procedure TParseTipDatesForm.OkBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(1, OkBtn.Picture.Bitmap);
end;

procedure TParseTipDatesForm.CancelBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(3, CancelBtn.Picture.Bitmap);
end;

procedure TParseTipDatesForm.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TParseTipDatesForm.ApplyDelimBtnClick(Sender: TObject);
begin
  if FIgnoreEvents then
    Exit;
  try
    FIgnoreEvents := True;
    if DelimitedBtn.Checked then
      RefreshTipDatesSearch;
  finally
    FIgnoreEvents := False;;
  end;
end;

procedure TParseTipDatesForm.CancelBtnClick(Sender: TObject);
begin
  CancelActionExecute(Sender);
end;

procedure TParseTipDatesForm.CancelBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(2, CancelBtn.Picture.Bitmap);
end;

procedure TParseTipDatesForm.ApplyDateBtnClick(Sender: TObject);
begin
  if FIgnoreEvents then
    Exit;
  try
    FIgnoreEvents := True;
    if IsDateBtn.Checked then
    begin
      if ValidateDateFormat then
      begin
        RefreshTipDatesSearch;
      end;
    end;
  finally
    FIgnoreEvents := False
  end;
end;

procedure TParseTipDatesForm.DateFormatEditChange(Sender: TObject);
begin
  {$IFNDEF DARWIN}
  if FIgnoreEvents then
    Exit;
  try
    FIgnoreEvents := True;
    if IsDateBtn.Checked then
      ApplyDateBtn.Enabled := (DateFormatEdit.Text <> EmptyStr);
  finally
    FIgnoreEvents := False;
  end;
  {$ENDIF}
end;

procedure TParseTipDatesForm.DateFormatEditEditingDone(Sender: TObject);
begin
  if FIgnoreEvents then
    Exit;
  try
    FIgnoreEvents := True;
    if IsDateBtn.Checked then
      ApplyDateBtn.Enabled := (DateFormatEdit.Text <> EmptyStr);
  finally
    FIgnoreEvents := False
  end;
end;

procedure TParseTipDatesForm.DelimitedBtnChange(Sender: TObject);
begin
  if FIgnoreEvents then
    Exit;
  try
    FIgnoreEvents := True;
    if DelimitedBtn.Checked then
      PositionBtn.Checked := False;
    ApplyDelimBtn.Enabled := (DelimitedBtn.Checked and (DelimiterEdit.Text <> EmptyStr));
    if ApplyDelimBtn.Enabled then
      RefreshTipDatesSearch;
  finally
    FIgnoreEvents := False
  end;
end;

procedure TParseTipDatesForm.DelimiterEditChange(Sender: TObject);
begin
  {$IFNDEF DARWIN}
  try
    FIgnoreEvents := True;
    if DelimitedBtn.Checked then
      ApplyDelimBtn.Enabled := (DelimiterEdit.Text <> EmptyStr);
  finally
    FIgnoreEvents := False;
  end;
  {$ENDIF}
end;

procedure TParseTipDatesForm.DelimiterEditEditingDone(Sender: TObject);
begin
  if FIgnoreEvents then
    Exit;
  try
    FIgnoreEvents := True;
    if DelimitedBtn.Checked then
      ApplyDelimBtn.Enabled := (DelimiterEdit.Text <> EmptyStr);
  finally
    FIgnoreEvents := False
  end;
end;

procedure TParseTipDatesForm.DigitsEditChange(Sender: TObject);
begin
  if FIgnoreEvents then
    Exit;
  try
    FIgnoreEvents := True;
    if IsNumberBtn.Checked then
      RefreshTipDatesSearch;
  finally
    FIgnoreEvents := False
  end;
end;

procedure TParseTipDatesForm.FormCreate(Sender: TObject);
begin
  UpdateShortcutsForMacOs(ActionList1);
  FTipDates := TList.Create;
  FIgnoreEvents := False;
  {$IFDEF DARWIN}
  OkBtn.Proportional := True;
  CancelBtn.Proportional := True;
  HelpBtn.Proportional := True;
  {$ENDIF}
  ImageForm.UpdateImgList(Self);
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Parse Tip Dates';
end;

procedure TParseTipDatesForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FTipDates) then
  begin
    FTipDates.Clear;
    FTipDates.Free;
  end;
end;

procedure TParseTipDatesForm.FormResize(Sender: TObject);
begin
  if TipDatesDrawGrid.ColCount = 4 then
    TipDatesDrawGrid.ColWidths[3] := (TipDatesDrawGrid.Width - TipDatesDrawGrid.ColWidths[0] - TipDatesDrawGrid.ColWidths[1] - TipDatesDrawGrid.ColWidths[2]);
end;

procedure TParseTipDatesForm.FormShow(Sender: TObject);
begin
  RefreshTipDatesSearch;
  TipDatesDrawGrid.RowCount := FLabels.Count + 1;
  FormResize(Sender);
end;

procedure TParseTipDatesForm.HelpActionExecute(Sender: TObject);
begin
  ShowMessage('not implemented');
end;

procedure TParseTipDatesForm.HelpBtnClick(Sender: TObject);
begin
  HelpActionExecute(Sender);
end;

procedure TParseTipDatesForm.HelpBtnMouseEnter(Sender: TObject);
begin
  HelpImages.GetBitmap(1, HelpBtn.Picture.Bitmap);
end;

procedure TParseTipDatesForm.HelpBtnMouseLeave(Sender: TObject);
begin
  HelpImages.GetBitmap(0, HelpBtn.Picture.Bitmap);
end;

procedure TParseTipDatesForm.IsDateBtnChange(Sender: TObject);
begin
  if FIgnoreEvents then
    Exit;
  try
    FIgnoreEvents := True;
    ApplyDateBtn.Enabled := (IsDateBtn.Checked and (DateFormatEdit.Text <> EmptyStr));
    if Trim(DateFormatEdit.Text) <> EmptyStr then
      RefreshTipDatesSearch;
  finally
    FIgnoreEvents := False
  end;
end;

procedure TParseTipDatesForm.IsNumberBtnChange(Sender: TObject);
begin
  if FIgnoreEvents then
    Exit;
  try
    FIgnoreEvents := True;
    if IsNumberBtn.Checked then 
    begin
      RefreshTipDatesSearch;
      ApplyDateBtn.Enabled := False
    end;
  finally
    FIgnoreEvents := False
  end;
end;

procedure TParseTipDatesForm.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TParseTipDatesForm.OkBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(0, OkBtn.Picture.Bitmap);
end;

procedure TParseTipDatesForm.PositionBtnChange(Sender: TObject);
begin
  if FIgnoreEvents then
    Exit;
  try
    FIgnoreEvents := True;
    if PositionBtn.Checked then
      DelimitedBtn.Checked := False;
    ApplyDelimBtn.Enabled := (DelimitedBtn.Checked and (DelimiterEdit.Text <> EmptyStr));
    if ApplyDelimBtn.Enabled then
      RefreshTipDatesSearch;
  finally
    FIgnoreEvents := False
  end;
end;

procedure TParseTipDatesForm.PositionComboBoxChange(Sender: TObject);
begin
  if FIgnoreEvents then
    Exit;
  try
    FIgnoreEvents := True;
    if PositionBtn.Checked then
      RefreshTipDatesSearch;
  finally
    FIgnoreEvents := False
  end;
end;

procedure TParseTipDatesForm.SaveActionExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TParseTipDatesForm.TipDatesDrawGridColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  TipDatesDrawGrid.Invalidate;
  FormResize(Sender);
end;

procedure TParseTipDatesForm.TipDatesDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  txt: String;
  ts: TTextStyle;
  x, y: Integer;
  td: TTipDate = nil;
begin
  if FIgnoreEvents then
    exit;
  ts := TipDatesDrawGrid.Canvas.TextStyle;
  with TipDatesDrawGrid.Canvas do
  begin
    Brush.Style := bsSolid;
    if aRow = 0 then
    begin
      Brush.Color := clBtnFace;
      FillRect(aRect);
      ts.Alignment := taCenter;
      case aCol of
        0: txt := 'Taxon Label';
        1: txt := 'Sample Date';
        2: txt := 'Height';
      end;
      x := aRect.Left + 2;
      y := aRect.Top + 2;
      Font.Style := [fsBold];
      if txt <> EmptyStr then
        TextRect(aRect, x, y, txt, ts);
    end
    else
    begin
      if (FTipDates.Count < aRow) and (FLabels.Count < aRow) then
        Exit;
      Brush.Color := clWhite;
      FillRect(aRect);
      if (FTipDates.Count < aRow) or (FTipDates[aRow - 1] = nil) then
      begin
        case aCol of
          0:
            begin
              txt := FLabels[aRow - 1];
              ts.Alignment := taLeftJustify;
            end;
          1, 2:
            begin
              txt := '?  ';
              ts.Alignment := taRightJustify;
            end;
        end;
      end
      else
      begin
        td := TTipDate(FTipDates[aRow - 1]);
        case aCol of
          0:
            begin
              txt := td.TaxonName;
              ts.Alignment := taLeftJustify;
            end;
          1:
            begin
              txt := Format('%s  ', [td.DateTimeString]);
              ts.Alignment := taRightJustify;
            end;
          2:
            begin
              txt := Format('%.4f  ', [FLatestDate - td.SampleTime]);
              ts.Alignment := taRightJustify;
            end;
        end;
      end;

      x := aRect.Left + 2;
      y := aRect.Top + 2;
      Font.Style := [];
      if txt = '?' then
        Font.Color := clRed
      else
        Font.Color := clBlack;
      if txt <> EmptyStr then
        TextRect(aRect, x, y, txt, ts);
    end;
  end;
end;

function TParseTipDatesForm.ValidateDateFormat: boolean;
const
  VALID_CHARS = ['y', 'm', 'd', '/', '-', ' '];
var
  temp: String;
  i: Integer;
  month_count: Integer = 0;
  day_count: Integer = 0;
  year_count: Integer = 0;
  error: String = '';
  isValid: boolean = True;
begin

  temp := DateFormatEdit.Text;
  if Length(temp) > 0 then
    for i := 1 to Length(temp) do
    begin
      if not (temp[i] in VALID_CHARS) then
      begin
        ShowMessage('The specified data format will probably not work - the set of valid characters that can be used are: "y", "m", "d", "/", "-", and the space character');
        exit(False)
      end;
      if temp[i] = 'y' then year_count += 1;
      if temp[i] = 'm' then month_count += 1;
      if temp[i] = 'd' then day_count += 1;
    end;
    if not ((day_count = 0) or ((day_count = 2) and (pos('dd',temp) > 0))) then begin
      error := 'Error with day format in date.' + LineEnding;
      isValid := False;
    end;
    if not ((month_count = 0) or ((month_count = 2) and (pos('mm',temp) > 0))) then begin
      error := error + 'Error with month format in date.' + LineEnding;
      isValid := False
    end;
    if not ((year_count = 0) or ((year_count = 2) and (pos('yy',temp) > 0)) or ((year_count = 4) and (pos('yyyy',temp) > 0))) then begin
      error := error + 'Error with year format in date.' + LineEnding;
      isValid := False
    end;
    if not isValid then begin
      ShowMessage(error);
    end;
    result := isValid;
end;

procedure TParseTipDatesForm.FindLatestDate;
var
  i: Integer;
  td: TTipDate;
begin
  FLatestDate := 0;
  if FTipDates.Count > 0 then
    for i := 0 to FTipDates.Count - 1 do
    begin
      td := TTipDate(FTipDates[i]);
      if Assigned(td) and (td.SampleTime > FLatestDate) then
        FLatestDate := td.SampleTime;
    end;
end;

procedure TParseTipDatesForm.SetFinder(AValue: TTipDatesFinder);
begin
  if FFinder=AValue then Exit;
  FFinder:=AValue;
end;

procedure TParseTipDatesForm.SetLabels(AValue: TStringList);
begin
  if FLabels=AValue then Exit;
  FLabels:=AValue;
  UpdateColWidths;
end;

procedure TParseTipDatesForm.UpdateColWidths;
var
  longestName: String;
begin
  longestName := LongestString(FLabels);
  TipDatesDrawGrid.ColWidths[0] := TipDatesDrawGrid.Canvas.TextWidth(longestName) + 20;
  TipDatesDrawGrid.ColWidths[1] := TipDatesDrawGrid.Canvas.TextWidth('Sample Date') + 20;
  TipDatesDrawGrid.ColWidths[2] := TipDatesDrawGrid.Canvas.TextWidth('Height ') + 20;
  TipDatesDrawGrid.ColWidths[3] := TipDatesDrawGrid.Width - TipDatesDrawGrid.ColWidths[0] - TipDatesDrawGrid.ColWidths[1] - TipDatesDrawGrid.ColWidths[2];
end;

procedure TParseTipDatesForm.RefreshTipDatesSearch;
begin
  if not Assigned(FFinder) then
    Exit;
  try
    if (IsDateBtn.Checked and (Trim(DateFormatEdit.Text) = EmptyStr)) or (DelimitedBtn.Checked and (DelimiterEdit.Text = EmptyStr)) then
      Exit;
    BeginFormUpdate;
    FFinder.Digits := DigitsEdit.Value;
    FFinder.IsCalendarDate := IsDateBtn.Checked;
    FFinder.DateFormat := Trim(DateFormatEdit.Text);
    FFinder.UsingDelimiter := DelimitedBtn.Checked;
    FFinder.Delimiter := DelimiterEdit.Text;
    FFinder.TipDatePosition := IntToDatePosition(PositionComboBox.ItemIndex);
    FFinder.ParseRule := tdprCustom;
    Enabled := False;
    FIgnoreEvents := True;
    if FFinder.FindTipDates(FLabels) then
    begin
      FFinder.AssignTipDatesToList(FTipDates);
      StatusBar1.SimpleText := Format('Found %d tip dates', [FTipDates.Count]);
    end
    else
    begin
      FFinder.AssignTipDatesToList(FTipDates);
      StatusBar1.SimpleText := Format('Failed to find all tip dates - %d dates out of %d labels were found', [Finder.NumberOfTipDates, FLabels.Count]);
    end;
    FindLatestDate;
    TipDatesDrawGrid.Invalidate;
  finally
    FIgnoreEvents := False;
    Enabled := True;
    EndFormUpdate;
    Invalidate;
  end;
end;

procedure TParseTipDatesForm.SetTipDates(aList: TList);
begin
  FTipDates.Clear;
  FTipDates.AddList(aList);
  FindLatestDate;
  TipDatesDrawGrid.RowCount := FTipDates.Count + 1;
  TipDatesDrawGrid.Invalidate;
end;

end.

