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

unit mgroup_names_editor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, ActnList;

type

  { TGroupNamesEditor }

  TGroupNamesEditor = class(TForm)
    FilterEdit: TLabeledEdit;
    NamesListBox: TListBox;
    SaveAction: TAction;
    CancelAction: TAction;
    ActionList1: TActionList;
    GroupEdit: TLabeledEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure CancelActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure NamesListBoxDblClick(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
  private
    function GetGroupName: String;

  public
    procedure SetGroupNames(aNames: TStringList);
    procedure SetTaxonGroupName(aTaxonName: String; aGroupName: String);
    property GroupName: String read GetGroupName;
  end;

var
  GroupNamesEditor: TGroupNamesEditor;

implementation

{$R *.lfm}

uses
  mimageform, MegaVerConsts;

{ TGroupNamesEditor }

procedure TGroupNamesEditor.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel
end;

procedure TGroupNamesEditor.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  Constraints.MinWidth := ToolBar1.Width + 20;
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TGroupNamesEditor.FormCreate(Sender: TObject);
begin
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Group Names Editor';
end;

procedure TGroupNamesEditor.FormResize(Sender: TObject);
begin
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TGroupNamesEditor.NamesListBoxDblClick(Sender: TObject);
var
  aName: String = '';
begin
  if NamesListBox.ItemIndex >= 0 then
    GroupEdit.Text := NamesListBox.Items[NamesListBox.ItemIndex];
end;

procedure TGroupNamesEditor.SaveActionExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

function TGroupNamesEditor.GetGroupName: String;
begin
  Result := GroupEdit.Text;
end;

procedure TGroupNamesEditor.SetGroupNames(aNames: TStringList);
begin
  NamesListBox.Items.Clear;
  if aNames.Count > 0 then
    NamesListBox.Items.AddStrings(aNames);
end;

procedure TGroupNamesEditor.SetTaxonGroupName(aTaxonName: String;
  aGroupName: String);
begin
  GroupEdit.EditLabel.Caption := Format('Group Name for %s', [aTaxonName]);
  GroupEdit.Text := aGroupName;
  GroupEdit.SelectAll;
end;

end.

