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

unit mega_info_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ActnList, ComCtrls;

type

  { TMegaInfoForm }

  TMegaInfoForm = class(TForm)
    OkAction: TAction;
    ActionList1: TActionList;
    Panel1: TPanel;
    StaticText1: TStaticText;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private

  public
    procedure SetInfoText(aInfo: String);
  end;

var
  MegaInfoForm: TMegaInfoForm;

implementation

uses
  MegaVerConsts, MMegaWindowInfo, mimageform, math;

{$R *.lfm}

{ TMegaInfoForm }

procedure TMegaInfoForm.Image1Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TMegaInfoForm.FormCreate(Sender: TObject);
begin
  Font.size := 12;
  Font.Color := $00333333;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX;
  AddWindowToTray(Self);
end;

procedure TMegaInfoForm.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  Constraints.MinWidth := ToolBar1.Width + 20;
  if Panel1.Width > ToolBar1.Width then
    ToolBar1.Left := Round((Panel1.Width - ToolBar1.Width)/2);
end;

procedure TMegaInfoForm.FormDestroy(Sender: TObject);
begin
  RemoveWindowFromTray(Self);
end;

procedure TMegaInfoForm.FormResize(Sender: TObject);
begin
  if Panel1.Width > ToolBar1.Width then
    ToolBar1.Left := Round((Panel1.Width - ToolBar1.Width)/2);
end;

procedure TMegaInfoForm.SetInfoText(aInfo: String);
var
  numLines: Integer;
begin
  numLines := aInfo.CountChar(#10);
  StaticText1.Caption := aInfo;
  ClientHeight := max(ClientHeight, Panel1.Height + StaticText1.BorderSpacing.Around*2 + ((numLines + 3)*Panel1.Canvas.TextHeight('W')));
end;

end.

