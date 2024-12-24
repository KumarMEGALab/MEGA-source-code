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

unit mselectoutgrouptaxadlg;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ImgList, ActnList, StdCtrls,
  ExtCtrls, Buttons, IniPropStorage, ComCtrls, Menus, mimageform, MegaUtils,
  MegaPrivateFiles;

type

  { TSelectOutgroupTaxaDlg }

  TSelectOutgroupTaxaDlg = class(TForm)
    NecessaryDummyMenu: TMainMenu;
    SaveAction: TAction;
    CancelAction: TAction;
    IniPropStorage1: TIniPropStorage;
    Panel1: TPanel;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    OutgroupListBox: TListBox;
    Panel6: TPanel;
    ActionList1: TActionList;
    MoveLeftAction: TAction;
    MoveRightAction: TAction;
    ImportGroupsAction: TAction;
    IngroupListBox: TListBox;
    OpenDialog: TOpenDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure MoveLeftActionExecute(Sender: TObject);
    procedure MoveRightActionExecute(Sender: TObject);
    procedure ImportGroupsActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IngroupListBoxDblClick(Sender: TObject);
    procedure OutgroupListBoxDblClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    FIngroupTaxa: TStringList;
    FOutgroupTaxa: TStringList;
    procedure UpdateTaxaLists;
  public

    procedure SetIngroupTaxa(aList: TStringList);
    property IngroupTaxa: TStringList read FIngroupTaxa;
    property OutgroupTaxa: TStringList read FOutgroupTaxa;
  end;

var
  SelectOutgroupTaxaDlg: TSelectOutgroupTaxaDlg;

implementation

{$R *.lfm}

{ TSelectOutgroupTaxaDlg }

procedure TSelectOutgroupTaxaDlg.FormCreate(Sender: TObject);
begin
  FIngroupTaxa := TStringList.Create;
  FOutgroupTaxa := TStringList.Create;
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
end;

procedure TSelectOutgroupTaxaDlg.FormDestroy(Sender: TObject);
begin
  if Assigned(FIngroupTaxa) then
    FIngroupTaxa.Free;
  if Assigned(FOutgroupTaxa) then
    FOutgroupTaxa.Free;
end;

procedure TSelectOutgroupTaxaDlg.ImportGroupsActionExecute(Sender: TObject);
var
  aList: TStringList;
  aTaxon: String;
  i: Integer;
  index: Integer;
begin
  OpenDialog.InitialDir := GetCurrentDir;
  OpenDialog.Filter := 'Text Files|*.txt|All Files|*.*';
  if not OpenDialog.Execute then
    Exit;

  try
    try
      aList := TStringList.Create;
      aList.LoadFromFile(OpenDialog.Filename);
      if aList.Count > 0 then
      begin
        for i := 0 to aList.Count - 1 do
        begin
          aTaxon := aList.Names[i];
          TrimTaxaName(aTaxon);
          if (aList.ValueFromIndex[i] <> EmptyStr) and (not SameText(aList.ValueFromIndex[i], 'outgroup')) then
            continue;
          index := IngroupListBox.Items.IndexOf(aTaxon);
          if index >= 0 then
          begin
            OutgroupListBox.Items.Add(aTaxon);
            IngroupListBox.Items.Delete(index);
          end
          else
            raise Exception.Create('Unknown taxon name found: ' + aTaxon);
        end;
        OutgroupListBox.Invalidate;
        IngroupListBox.Invalidate;
      end;
      UpdateTaxaLists;
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TSelectOutgroupTaxaDlg.IngroupListBoxDblClick(Sender: TObject);
begin
  if IngroupListBox.ItemIndex >= 0 then
    MoveLeftActionExecute(Sender);
end;

procedure TSelectOutgroupTaxaDlg.MoveLeftActionExecute(Sender: TObject);
var
  aTaxon: String;
  i, index: Integer;
begin
  if IngroupListBox.Items.Count = 0 then
    Exit;
  index := OutgroupListBox.Items.Count;
  for i := IngroupListBox.Items.Count - 1 downto 0 do
  begin
    if IngroupListBox.Selected[i] then
    begin
      aTaxon := IngroupListBox.Items[i];
      OutgroupListBox.Items.Insert(index, aTaxon);
      IngroupListBox.Items.Delete(i);
    end;
  end;
  OutgroupListBox.Invalidate;
  IngroupListBox.Invalidate;
  UpdateTaxaLists;
end;

procedure TSelectOutgroupTaxaDlg.CancelBtnClick(Sender: TObject);
begin
  modalResult := mrCancel;
end;

procedure TSelectOutgroupTaxaDlg.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  if Panel1.Width > ToolBar1.Width then
    ToolBar1.Left := Round((Panel1.Width - ToolBar1.Width)/2);
  Constraints.MinWidth := ToolBar1.Width + 20;
end;

procedure TSelectOutgroupTaxaDlg.MoveRightActionExecute(Sender: TObject);
var
  aTaxon: String;
  i, index: Integer;
begin
  if OutgroupListBox.Items.Count = 0 then
    Exit;
  index := IngroupListBox.Items.Count;
  for i := OutgroupListBox.Items.Count - 1 downto 0 do
  begin
    if OutgroupListBox.Selected[i] then
    begin
      aTaxon := OutgroupListBox.Items[i];
      IngroupListBox.Items.Insert(index, aTaxon);
      OutgroupListBox.Items.Delete(i);
    end;
  end;
  OutgroupListBox.Invalidate;
  IngroupListBox.Invalidate;
  UpdateTaxaLists;
end;

procedure TSelectOutgroupTaxaDlg.OutgroupListBoxDblClick(Sender: TObject);
begin
  if OutgroupListBox.ItemIndex >= 0 then
    MoveRightActionExecute(Sender);
end;

procedure TSelectOutgroupTaxaDlg.SaveBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TSelectOutgroupTaxaDlg.SetIngroupTaxa(aList: TStringList);
begin
  FIngroupTaxa.Clear;
  FIngroupTaxa.AddStrings(aList);
  OutgroupListBox.Items.Clear;
  IngroupListBox.Items.Clear;
  IngroupListBox.Items.AddStrings(FIngroupTaxa);
end;

procedure TSelectOutgroupTaxaDlg.UpdateTaxaLists;
begin
  FOutgroupTaxa.Clear;
  FOutgroupTaxa.AddStrings(OutgroupListBox.Items);
  FIngroupTaxa.Clear;
  FIngroupTaxa.AddStrings(IngroupListBox.Items);
end;

end.
