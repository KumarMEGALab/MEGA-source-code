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

unit MInputDataOptionsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, IniPropStorage, ActnList, ComCtrls, KeywordConsts,
  mimageform;

type

  { TInputDataOptionsDlg }

  TInputDataOptionsDlg = class(TForm)
    DataTypeLBx: TListBox;
    GapSymEdit: TEdit;
    IdenticalSymEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MissingSymEdit: TEdit;
    OkAction: TAction;
    CancelAction: TAction;
    HelpAction: TAction;
    ActionList1: TActionList;
    IniPropStorage1: TIniPropStorage;
    Panel1: TPanel;
    LowerLeftBtn: TRadioButton;
    UpperRightBtn: TRadioButton;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure DataTypeLBxClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);

  private
    function GetDataFormat: TSnTokenCode;
    function GetDataType: TSnTokenCode;
    function GetGapSym: Char;
    function GetIdenSym: Char;
    function GetMissSym: Char;
    function ValidSpecialIs(TheChar: Char): Boolean;
    function ExamineThreeSymbols(gap,miss,iden: Char): Boolean;
    { private declarations }
  public
    procedure HideDistDataOption;
    property DataType    : TSnTokenCode     read GetDataType;
    property DataFormat  : TSnTokenCode     read GetDataFormat;
    property IdenSym     : Char         read GetIdenSym;
    property MissSym     : Char         read GetMissSym;
    property GapSym      : Char         read GetGapSym;
  end;

var
  InputDataOptionsDlg: TInputDataOptionsDlg;

implementation

uses
  ContextHelp_HC,
  MegaVerConsts, mhelpfiles, mhelpkeywords;
{$R *.lfm}

{ TInputDataOptionsDlg }

procedure TInputDataOptionsDlg.DataTypeLBxClick(Sender: TObject);
begin
  if DataTypeLBx.ItemIndex = 2 then
  begin
    LowerLeftBtn.Enabled := True;
    UpperRightBtn.Enabled := True;
    MissingSymEdit.Enabled := False;
    GapSymEdit.Enabled := False;
    IdenticalSymEdit.Enabled := False;
  end
  else
  begin
    LowerLeftBtn.Enabled := False;
    UpperRightBtn.Enabled := False;
    MissingSymEdit.Enabled := True;
    GapSymEdit.Enabled := True;
    IdenticalSymEdit.Enabled := True;
  end;
end;

procedure TInputDataOptionsDlg.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  if Panel1.Width > ToolBar1.Width then
    ToolBar1.Left := Round((Panel1.Width - ToolBar1.Width)/2);
end;

procedure TInputDataOptionsDlg.FormCreate(Sender: TObject);
begin
  DataTypeLBx.ItemIndex := 0; // Nucleotide sequence
  LowerLeftBtn.Enabled := False;
  UpperRightBtn.Enabled := False;
  MissingSymEdit.Text := '?';
  GapSymEdit.Text := '-';
  IdenticalSymEdit.Text := '.';
  HelpContext := HC_Input_Data_Format;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Input Data';
end;

procedure TInputDataOptionsDlg.FormResize(Sender: TObject);
begin
  if Panel1.Width > ToolBar1.Width then
    ToolBar1.Left := Round((Panel1.Width - ToolBar1.Width)/2);
end;

procedure TInputDataOptionsDlg.Image1Click(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
end;

procedure TInputDataOptionsDlg.Image2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TInputDataOptionsDlg.OkBtnClick(Sender: TObject);
begin
  case DataType of
    snNucleotide,
    snProtein:
    begin
      if Trim(GapSymEdit.Text) = EmptyStr then
      begin
        ShowMessage('Please provide a gap symbol');
        GapSymEdit.SetFocus;
        ModalResult := mrNone;
      end
      else if Trim(MissingSymEdit.Text) = EmptyStr then
      begin
        ShowMessage('Please provide a missing data symbol');
        MissingSymEdit.SetFocus;
        ModalResult := mrNone;
      end
      else if Trim(IdenticalSymEdit.Text) = EmptyStr then
      begin
        ShowMessage('Please provide an identical data symbol');
        IdenticalSymEdit.SetFocus;
        ModalResult := mrNone;
      end
      else if not ExamineThreeSymbols(AnsiChar(GapSymEdit.Text[1]),AnsiChar(MissingSymEdit.Text[1]),AnsiChar(IdenticalSymEdit.Text[1])) then
        ModalResult := mrNone
      else
        ModalResult := mrOk;
    end
    else
      ModalResult := mrOk;
  end;
  if ModalResult = mrNone then  // we are back to OK button
    Exit;
end;

function TInputDataOptionsDlg.GetDataFormat: TSnTokenCode;
begin
  Result := snNoToken;
  if DataType = snDistance then
    if LowerLeftBtn.Checked then
      Result := snLowerMatrix
    else
      Result := snUpperMatrix;
end;

function TInputDataOptionsDlg.GetDataType: TSnTokenCode;
begin
  Result := snNoToken;
  case DataTypeLBx.ItemIndex of
    0: Result := snNucleotide;
    1: Result := snProtein;
    2: Result := snDistance;
    3: Result := snGenotypeFreq;
    4: Result := snAlleleFreq;
    5: Result := snMicrosatellites;
    6: Result := snRestrictionSite;
    7: Result := snRFLP;
    8: Result := snRAPD;
  end;
end;

function TInputDataOptionsDlg.GetGapSym: Char;
begin
  Result := Char(GapSymEdit.Text[1]);
end;

function TInputDataOptionsDlg.GetIdenSym: Char;
begin
  Result := Char(IdenticalSymEdit.Text[1]);
end;

function TInputDataOptionsDlg.GetMissSym: Char;
begin
  Result := Char(MissingSymEdit.Text[1]);
end;

function TInputDataOptionsDlg.ValidSpecialIs(TheChar: Char): Boolean;
begin
  case DataType of
    snNucleotide,
    snProtein:
      begin
         case UpCase(TheChar) of
           'A'..'Z':
             case TheChar of
               'A'..'D','G','H','K','M','R','S', 'T', 'U', 'V','W','Y':
                  Result := False;
             else
                  Result := True;
             end;
           '?','-','.':
             Result := True;
         else
           Result := False;
         end
      end
      else
      begin
         case UpCase(TheChar) of
           'A'..'Z':
             case TheChar of
               'J','O','U','X':
                  Result := False;
             else
               Result := True;
             end;
           '?','-','.':
           Result := True;
         else
           Result := False;
         end;
      end
    end;
end;

function TInputDataOptionsDlg.ExamineThreeSymbols(gap, miss, iden: Char): Boolean;
var
  ErrorStr: String;
begin
  Result := True;
  if not ValidSpecialIs(gap) then
  begin
    ErrorStr := 'Invalid alignment gap symbol: ' + GapSymEdit.Text;
    Result := False;
  end;
  if not ValidSpecialIs(miss) then
  begin
    ErrorStr := 'Invalid Missing data symbol: ' + MissingSymEdit.Text;
    Result := False;
  end;
  if not ValidSpecialIs(iden) then
  begin
    ErrorStr := 'Invalid identical symbol: ' + IdenticalSymEdit.Text;
    Result := False;
  end;

  if (gap=miss) or (gap=iden) or (miss=iden) then
  begin
    ErrorStr := 'Three symbols must be different: ' + GapSymEdit.Text + ',' +
                MissingSymEdit.Text + ',' + IdenticalSymEdit.Text;
    Result := False;
  end;

  if Result = False then
    ShowMessage(ErrorStr);
end;

procedure TInputDataOptionsDlg.HideDistDataOption;
begin
  DataTypeLBx.Items.Delete(2);
  LowerLeftBtn.Visible := False;
  UpperRightBtn.Visible := False;
  DataTypeLBx.Height := IdenticalSymEdit.Top + IdenticalSymEdit.Height - DataTypeLBx.Top;
  ClientHeight := Panel1.Height + DataTypeLBx.Top + DataTypeLBx.Height + 10;
end;

end.

