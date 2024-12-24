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

unit mcutoff_dlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, ActnList, ComCtrls;

type

  { TTreeCutoffForm }

  TTreeCutoffForm = class(TForm)
    OkAction: TAction;
    CancelAction: TAction;
    HelpAction: TAction;
    ActionList1: TActionList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    CondensedSEdit: TSpinEdit;
    ConsensusSEdit: TSpinEdit;
    ButtonsPanel: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure CancelActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
    procedure OkActionExecute(Sender: TObject);
  private

  public

  end;

var
  TreeCutoffForm: TTreeCutoffForm;

implementation

{$R *.lfm}

uses
  mimageform, mhelpfiles, mhelpkeywords, MegaVerConsts, TreeExplorer_HC;

{ TTreeCutoffForm }

procedure TTreeCutoffForm.FormActivate(Sender: TObject);
begin
  Toolbar1.Images := ImageForm.GetDialogButtonImageList;
  Toolbar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  Toolbar1.ImagesWidth := Toolbar1.ButtonWidth;
  Constraints.MinWidth := ToolBar1.Width + 20;
end;

procedure TTreeCutoffForm.FormCreate(Sender: TObject);
begin
  HelpContext := HC_Format_dialog_box_in_Tree_Explorer;
  HelpAction.HelpContext := HC_Format_dialog_box_in_Tree_Explorer;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Tree Cutoff Options';
end;

procedure TTreeCutoffForm.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TTreeCutoffForm.FormResize(Sender: TObject);
begin
  if ButtonsPanel.Width > ToolBar1.Width then
    ToolBar1.Left := Round((ButtonsPanel.Width - ToolBar1.Width)/2);
end;

procedure TTreeCutoffForm.HelpActionExecute(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
end;

procedure TTreeCutoffForm.OkActionExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.

