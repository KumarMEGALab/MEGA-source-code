{
	Copyright 1992-2025 Sudhir Kumar and Koichiro Tamura

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

unit mmega_font_dialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, ActnList;

type

  { TMegaFontDialog is intented for use on Linux systems because the Lazarus
    TFontDialog does not have a color selector. TMegaFontDialog provides a color
    selector.}

  { TMegaFontDialog }

  TMegaFontDialog = class(TForm)
    ColorButton: TColorButton;
    GroupBox2: TGroupBox;
    ColorLabel: TLabel;
    SampleLabel: TLabel;
    StrikethroughCheckbox: TCheckBox;
    UnderlineCheckbox: TCheckBox;
    GroupBox1: TGroupBox;
    SaveAction: TAction;
    CancelAction: TAction;
    ActionList1: TActionList;
    edtFont: TLabeledEdit;
    edtStyle: TLabeledEdit;
    edtSize: TLabeledEdit;
    lbFonts: TListBox;
    lbStyles: TListBox;
    lbSizes: TListBox;
    BottomPanel: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure CancelActionExecute(Sender: TObject);
    procedure ColorButtonColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject);
    procedure CheckboxChange(Sender: TObject);
    procedure UpdatePreview;
  private
    FMyStyles: TStringList;
    FIgnoreEvents: Boolean;
    procedure LoadFontNames;
    procedure LoadStyles;
    procedure LoadSizes;
    procedure ApplyFilters(sender: TObject);
    function MyColorName(c: TColor): String;
  public
    function GetSelectedFont: TFont;
    procedure Initialize(aFont: TFont);
    function Execute: Boolean;
  end;

var
  MegaFontDialog: TMegaFontDialog;

implementation

{$R *.lfm}

{ TMegaFontDialog }

procedure TMegaFontDialog.FormCreate(Sender: TObject);
begin
  try
    FMyStyles := TStringList.Create;
    FIgnoreEvents := True;
    LoadFontNames;
    LoadStyles;
    LoadSizes;
  finally
    FIgnoreEvents := False;
  end;
  UpdatePreview;
end;

procedure TMegaFontDialog.FormDestroy(Sender: TObject);
begin
  if Assigned(FMyStyles) then
    FMyStyles.Free;
end;

procedure TMegaFontDialog.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TMegaFontDialog.ColorButtonColorChanged(Sender: TObject);
begin
  ColorLabel.Caption := MyColorName(ColorButton.ButtonColor);
  UpdatePreview;
end;

procedure TMegaFontDialog.SaveActionExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TMegaFontDialog.edtFilterChange(Sender: TObject);
begin
  ApplyFilters(Sender);
end;

procedure TMegaFontDialog.ListBoxSelectionChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TMegaFontDialog.CheckboxChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TMegaFontDialog.UpdatePreview;
var
  style: TFontStyles;
begin
  if FIgnoreEvents then
    Exit;
  if lbFonts.ItemIndex >= 0 then
    SampleLabel.Font.Name := lbFonts.Items[lbFonts.ItemIndex];
  if lbSizes.ItemIndex >= 0 then
    SampleLabel.Font.Size := StrToIntDef(lbSizes.Items[lbSizes.ItemIndex], 12);
  style := [];
  if lbStyles.ItemIndex >= 0 then
  begin
    case lbStyles.Items[lbStyles.ItemIndex] of
      'Bold': Include(style, fsBold);
      'Italic': Include(style, fsItalic);
      'Bold Italic': style := [fsBold, fsItalic];
    end;
  end;
  if UnderLineCheckbox.Checked then Include(style, fsUnderline);
  if StrikethroughCheckBox.Checked then Include(style, fsStrikeOut);
  SampleLabel.Font.Style := style;
  SampleLabel.Font.Color := ColorButton.ButtonColor;
end;

procedure TMegaFontDialog.LoadFontNames;
var
  aList: TStringList = nil;
begin
  try
    aList := TStringList.Create;
    aList.Add('default');
    aList.AddStrings(Screen.Fonts);
    aList.Sort;
    lbFonts.Items.AddStrings(aList);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TMegaFontDialog.LoadStyles;
begin
  lbStyles.Items.Clear;
  lbStyles.Items.Add('Regular');
  lbStyles.Items.Add('Bold');
  lbStyles.Items.Add('Italic');
  lbStyles.Items.Add('Bold Italic');
  FMyStyles.Assign(lbStyles.Items);
end;

procedure TMegaFontDialog.LoadSizes;
var
  i: Integer;
begin
  lbSizes.Items.Clear;
  for i := 8 to 72 do
    lbSizes.Items.Add(IntToStr(i));
end;

procedure TMegaFontDialog.ApplyFilters(sender: TObject);
var
  filter: String;
  i: Integer;
  tempInt: Integer = -1;
begin
  if Sender = edtFont then
  begin
    filter := LowerCase(Trim(edtFont.Text));
    if filter = EmptyStr then
      LoadFontNames
    else
    begin
      lbFonts.Clear;
      for i := 0 to Screen.Fonts.Count - 1 do
        if Pos(filter, LowerCase(Screen.Fonts[i])) > 0 then
          lbFonts.Items.Add(Screen.Fonts[i]);
    end;
  end
  else if Sender = edtStyle then
  begin
    filter := LowerCase(Trim(edtStyle.Text));
    if filter = EmptyStr then
      LoadStyles
    else
    begin
      lbStyles.Clear;
      for i := 0 to FMyStyles.Count - 1 do
        if Pos(filter, LowerCase(FMyStyles[i])) > 0 then
          lbStyles.Items.Add(FMyStyles[i]);
    end;
  end
  else if Sender = edtSize then
  begin
    filter := Trim(edtSize.Text);
    if filter = EmptyStr then
      LoadSizes
    else
    begin
      if TryStrToInt(filter, tempInt) then
      begin
        lbSizes.Clear;
        for i := 8 to 72 do
          if Pos(filter, IntToStr(i)) = 1 then
            lbSizes.Items.Add(IntToStr(i));
      end
      else
        LoadSizes;
    end;
  end;
end;

function TMegaFontDialog.MyColorName(c: TColor): String;
begin
  Result := EmptyStr;
  case c of
    clBlack: Result := 'Black';
    clMaroon: Result := 'Maroon';
    clGreen: Result := 'Green';
    clOlive: Result := 'Olive Green';
    clNavy: Result := 'Navy Blue';
    clPurple: Result := 'Purple';
    clTeal: Result := 'Teal';
    clGray: Result := 'Gray';
    clSilver: Result := 'Silver';
    clRed: Result := 'Red';
    clLime: Result := 'Lime';
    clYellow: Result := 'Yellow';
    clBlue: Result := 'Blue';
    clFuchsia: Result := 'Fuchsia';
    clAqua: Result := 'Aqua';
    clWhite: Result := 'White';
    clMoneyGreen: Result := 'Money Green';
    clSkyBlue: Result := 'Sky Blue';
    clCream: Result := 'Cream';
    clMedGray: Result := 'Medium Gray';
    else
      Result := ColorToString(c);
  end;
end;

function TMegaFontDialog.GetSelectedFont: TFont;
begin
  Result := TFont.Create;
  Result.Assign(SampleLabel.Font);
end;

procedure TMegaFontDialog.Initialize(aFont: TFont);
var
  index: Integer = 0;
  tempStr: String = '';
begin
  try
    FIgnoreEvents := True;
    index := lbFonts.Items.IndexOf(aFont.Name);
    if index >= 0 then
      lbFonts.ItemIndex := index;

    tempStr := IntToStr(aFont.Size);
    index := lbSizes.Items.IndexOf(tempStr);
    if index >= 0 then
      lbSizes.ItemIndex := index;

    if aFont.Style = [] then
      lbStyles.ItemIndex := 0
    else if (fsBold in aFont.Style) and (fsItalic in aFont.Style) then
      lbStyles.ItemIndex := 3
    else if fsBold in aFont.Style then
      lbStyles.ItemIndex := 1
    else if fsItalic in aFont.Style then
      lbStyles.ItemIndex := 2
    else
      lbStyles.ItemIndex := 0;

    StrikethroughCheckbox.Checked := (fsStrikeOut in aFont.Style);
    UnderlineCheckbox.Checked := (fsUnderline in aFont.Style);

    if aFont.Color <> clDefault then
      ColorButton.ButtonColor := aFont.Color;
    SampleLabel.Font.Assign(aFont);
    UpdatePreview;
  finally
    FIgnoreEvents := False
  end;
end;

function TMegaFontDialog.Execute: Boolean;
var
  response: Integer = -1;
begin
  response := Self.ShowModal;
  if response = mrOK then
    Result := True
  else
    Result := False;
end;

end.

