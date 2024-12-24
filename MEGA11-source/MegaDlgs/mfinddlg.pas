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

unit MFindDlg;

interface
{$IFDEF VISUAL_BUILD}
uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, MTreeBox, mimageform;
  type

  { TFindDlg }

  TFindDlg = class(TForm)
    BitBtn1: TBitBtn;
    SearchBitBtn: TBitBtn;
    TaxaNameEdit: TEdit;
    SearchLbl: TLabel;
    GroupBox1: TGroupBox;
    ContainsTermBtn: TRadioButton;
    StartsWithTermBtn: TRadioButton;
    procedure DoSearchClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Search(AStr: String);
    procedure SearchBitBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);

  public
    PrevQuery   : String;
    Tree: TTreeCustomControl;
  end;

{$ENDIF}
implementation
{$IFDEF VISUAL_BUILD}
{$R *.lfm}

procedure TFindDlg.FormCreate(Sender: TObject);
begin
  {$IFDEF DARWIN}
  BitBtn1.AutoSize := True;
  SearchBitBtn.AutoSize := True;
  {$ENDIF}
  ImageForm.UpdateImgList(Self);
end;

procedure TFindDlg.FormShow(Sender: TObject);
begin
  TaxaNameEdit.SetFocus;
end;

procedure TFindDlg.DoSearchClick(Sender: TObject);
begin
  Search(TaxaNameEdit.Text);
end;

procedure TFindDlg.Search(AStr: String);
var
  QueryChanged: Boolean;
begin
  QueryChanged := (AnsiCompareText(AStr, PrevQuery) <> 0);
  PrevQuery := AStr;
  Tree.Search(AStr, ContainsTermBtn.Checked, True, QueryChanged);
end;

procedure TFindDlg.SearchBitBtnClick(Sender: TObject);
begin
  Search(TaxaNameEdit.Text);
  Close;
end;

procedure TFindDlg.BitBtn1Click(Sender: TObject);
begin
  Search(EmptyStr);
  Close;
end;
{$ENDIF}
end.
