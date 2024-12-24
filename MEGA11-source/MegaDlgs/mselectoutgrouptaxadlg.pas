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

unit mselectoutgrouptaxadlg;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ImgList, ActnList, StdCtrls,
  ExtCtrls, Buttons, IniPropStorage, mimageform, MegaUtils,
  MegaPrivateFiles;

type

  { TSelectOutgroupTaxaDlg }

  TSelectOutgroupTaxaDlg = class(TForm)
    Buttons: TImageList;
    CancelBtn: TImage;
    IniPropStorage1: TIniPropStorage;
    SaveBtn: TImage;
    Panel1: TPanel;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
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
    procedure CancelBtnClick(Sender: TObject);
    procedure CancelBtnMouseEnter(Sender: TObject);
    procedure CancelBtnMouseLeave(Sender: TObject);
    procedure MoveLeftActionExecute(Sender: TObject);
    procedure MoveRightActionExecute(Sender: TObject);
    procedure ImportGroupsActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IngroupListBoxDblClick(Sender: TObject);
    procedure OutgroupListBoxDblClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SaveBtnMouseEnter(Sender: TObject);
    procedure SaveBtnMouseLeave(Sender: TObject);
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
  //Panel4.Font.Style := [fsBold];
  //Panel5.Font.Style := [fsBold];
  {$IFDEF DARWIN}
  CancelBtn.Proportional:=True;
  SaveBtn.Proportional:=True;
  {$ENDIF}
  ImageForm.UpdateImgList(Self);
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

procedure TSelectOutgroupTaxaDlg.CancelBtnMouseEnter(Sender: TObject);
begin
  Buttons.GetBitmap(3, CancelBtn.Picture.Bitmap);
end;

procedure TSelectOutgroupTaxaDlg.CancelBtnMouseLeave(Sender: TObject);
begin
  Buttons.GetBitmap(2, CancelBtn.Picture.Bitmap);
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

procedure TSelectOutgroupTaxaDlg.SaveBtnMouseEnter(Sender: TObject);
begin
  Buttons.GetBitmap(1, SaveBtn.Picture.Bitmap);
end;

procedure TSelectOutgroupTaxaDlg.SaveBtnMouseLeave(Sender: TObject);
begin
  Buttons.GetBitmap(0, SaveBtn.Picture.Bitmap);
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
