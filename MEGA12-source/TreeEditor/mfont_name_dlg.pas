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

unit mfont_name_dlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ActnList, ComCtrls, MTreeBox;

const
  PREVIEW_TEXT = 'AaBbYyZz 0123456789';
type

  { TMegaFontNameDialog }

  TMegaFontNameDialog = class(TForm)
    OverwriteGroupFontsCheckbox: TCheckBox;
    OverwriteNodeFontsCheckbox: TCheckBox;
    SearchEdit: TEdit;
    Label2: TLabel;
    OkAction: TAction;
    CancelAction: TAction;
    ActionList1: TActionList;
    Label1: TLabel;
    FontsListBox: TListBox;
    PreviewPanel: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure CancelActionExecute(Sender: TObject);
    procedure FontsListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OkActionExecute(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
  private
    FIgnoreEvents: Boolean;
    FTree: TTreeCustomControl;
    function GetOverwriteGroupSpecificFonts: Boolean;
    function GetOverwriteNodeSpecificFonts: Boolean;
    procedure Initialize;
    procedure SetTree(AValue: TTreeCustomControl);
    procedure SetToSameFontAsTree;
  public
    function SelectedFontName: String;
    property Tree: TTreeCustomControl read FTree write SetTree;
    property OverwriteNodeSpecificFonts: Boolean read GetOverwriteNodeSpecificFonts;
    property OverwriteGroupSpecificFonts: Boolean read GetOverwriteGroupSpecificFonts;
  end;

//var
//  MegaFontNameDialog: TMegaFontNameDialog;

implementation

{$R *.lfm}

uses
  MegaVerConsts;

{ TMegaFontNameDialog }

procedure TMegaFontNameDialog.FontsListBoxSelectionChange(Sender: TObject; User: boolean);
var
  aFontName: String = '';
begin
  if FIgnoreEvents then
    Exit;

  try
    FIgnoreEvents := True;
    aFontName := SelectedFontName;
    if aFontName <> EmptyStr then
    begin
      PreviewPanel.Font.Name := aFontName;
      SearchEdit.Text := aFontName;
    end;
    PreviewPanel.Invalidate;
  finally
    FIgnoreEvents := False;
  end;
end;

procedure TMegaFontNameDialog.FormActivate(Sender: TObject);
begin
  Initialize;
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TMegaFontNameDialog.FormCreate(Sender: TObject);
begin
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Tree Explorer Font';
end;

procedure TMegaFontNameDialog.FormResize(Sender: TObject);
begin
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TMegaFontNameDialog.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TMegaFontNameDialog.OkActionExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TMegaFontNameDialog.SearchEditChange(Sender: TObject);
var
  i: Integer = -1;
begin
  try
    FIgnoreEvents := True;
    if SearchEdit.Text <> EmptyStr then
    begin
      for i := 0 to FontsListBox.Items.Count - 1 do
        if LowerCase(FontsListBox.Items[i]).StartsWith(LowerCase(SearchEdit.Text)) then
        begin
          FontsListBox.ItemIndex := i;
          PreviewPanel.Font.Name := SelectedFontName;
          PreviewPanel.Invalidate;
          break;
        end;
    end
    else
      FontsListBox.ItemIndex := 0;
  finally
    FIgnoreEvents := False;
  end;
end;

procedure TMegaFontNameDialog.Initialize;
var
  index: Integer;
  aFontName: String = '';
begin
  FIgnoreEvents := False;
  PreviewPanel.Caption := PREVIEW_TEXT;
  FontsListBox.Clear;
  FontsListBox.Items.Assign(Screen.Fonts);
  aFontName := Tree.Font.Name;
  index := FontsListBox.Items.IndexOf(aFontName);
  if index >= 0 then
    FontsListBox.ItemIndex := index;
  if SelectedFontName <> EmptyStr then
    PreviewPanel.Font.Name := SelectedFontName;
  PreviewPanel.Invalidate;
end;

function TMegaFontNameDialog.GetOverwriteGroupSpecificFonts: Boolean;
begin
  Result := OverwriteGroupFontsCheckbox.Checked;
end;

function TMegaFontNameDialog.GetOverwriteNodeSpecificFonts: Boolean;
begin
  Result := OverwriteNodeFontsCheckbox.Checked;
end;

function TMegaFontNameDialog.SelectedFontName: String;
begin
  if FontsListBox.itemIndex >= 0 then
    Result := FontsListBox.Items[FontsListBox.ItemIndex]
  else
    Result := EmptyStr;
end;

procedure TMegaFontNameDialog.SetTree(AValue: TTreeCustomControl);
var
  aFontName: String = '';
  index: Integer = -1;
begin
  FTree := AValue;
  SetToSameFontAsTree;
end;

procedure TMegaFontNameDialog.SetToSameFontAsTree;
var
  aFontName: String = '';
  index: Integer = -1;
begin
  if Assigned(FTree) then
  begin
    aFontName := FTree.Font.Name;
    index := FontsListBox.Items.IndexOf(aFontName);
    if index >= 0 then
      FontsListBox.ItemIndex := index;
  end;

end;

end.

