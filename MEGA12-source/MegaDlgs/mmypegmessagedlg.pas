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

unit mmypegmessagedlg;

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ActnList, ComCtrls;

type

  { TMyPegMessageDlg }

  TMyPegMessageDlg = class(TForm)
    CloseAction: TAction;
    ActionList1: TActionList;
    Memo1: TMemo;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure CloseActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MyPegMessageDlg: TMyPegMessageDlg;

implementation

uses
  mimageform;

{$R *.lfm}

{ TMyPegMessageDlg }

procedure TMyPegMessageDlg.CloseActionExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TMyPegMessageDlg.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  if Panel1.Width > ToolBar1.Width then
    ToolBar1.Left := Round((Panel1.Width - ToolBar1.Width)/2);
end;

procedure TMyPegMessageDlg.FormResize(Sender: TObject);
begin
  if Panel1.Width > ToolBar1.Width then
    ToolBar1.Left := Round((Panel1.Width - ToolBar1.Width)/2);
end;

end.
