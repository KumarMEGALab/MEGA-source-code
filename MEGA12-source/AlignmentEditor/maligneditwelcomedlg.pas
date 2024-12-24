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

unit maligneditwelcomedlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, ComCtrls , IniPropStorage;

type

  { TWelcomeForm }

  TWelcomeForm = class(TForm)
    CancelAction: TAction;
    IniPropStorage1 : TIniPropStorage ;
    Label1: TLabel;
    OkAction: TAction;
    ActionList1: TActionList;
    Panel1: TPanel;
    NewAlignmentBtn: TRadioButton;
    OpenSessionBtn: TRadioButton;
    OpenFileBtn: TRadioButton;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CancelBtnImgClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OkBtnImgClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  WelcomeForm: TWelcomeForm;

implementation

{$R *.lfm}

uses
  MegaVerConsts, mimageform;

{ TWelcomeForm }

procedure TWelcomeForm.FormCreate(Sender: TObject);
begin
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Alignment Editor';
  Label1.Font.Style := [fsBold];
end;

procedure TWelcomeForm.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  Constraints.MinWidth := ToolBar1.Width + 20;
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TWelcomeForm.CancelBtnImgClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TWelcomeForm.FormResize(Sender: TObject);
begin
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TWelcomeForm.OkBtnImgClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.

