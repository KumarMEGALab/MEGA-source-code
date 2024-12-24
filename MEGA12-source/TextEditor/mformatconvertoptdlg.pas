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

unit MFormatConvertOptDlg;

//Phasing out the usage of Dream Company components because they are known to leak memory and dont' work with newer delphi versions.

interface

uses
  LCLIntF, LCLType, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Dialogs, ActnList, ComCtrls,
  MFormatConvertToMega;

type

  { TFormatConvertOptDlg }

  TFormatConvertOptDlg = class(TForm)
    OkAction: TAction;
    CancelAction: TAction;
    HelpAction: TAction;
    ActionList1: TActionList;
    FormatCbx: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    FileNameEdit: TEdit;
    BitBtn4: TBitBtn;
    OpenDialog1: TOpenDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FileNameEditChange(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormResize(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    procedure SetImportFileName(AValue: String);
    function  GetImportFileName: String;
    function  GetImportFileType: TImportFormatType;
  public
    property ImportFileName: String read GetImportFileName write SetImportFileName;
    property ImportFileType: TImportFormatType read GetImportFileType;
  end;

implementation

{$R *.lfm}

uses
  TextEditorHelp_HC, MegaVerConsts, mhelpfiles, mhelpkeywords, mimageform;

procedure TFormatConvertOptDlg.FormCreate(Sender: TObject);
var
  i : TImportFormatType;
begin
  with FormatCBx do
  begin
    Items.Clear;
    for i := Low(FormatNames) to High(FormatNames) do
      Items.Add(FormatNames[i]);
    ItemIndex := 0;
  end;

  HelpContext := HC_Convert_to_MEGA_Format_Dialog_Box;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Select File and Format';
end;

procedure TFormatConvertOptDlg.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormatConvertOptDlg.FormActivate(Sender: TObject);
begin
ToolBar1.Images := ImageForm.GetDialogButtonImageList;
ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
Constraints.MinWidth := ToolBar1.Width + 20;
if ClientWidth > ToolBar1.Width then
  ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TFormatConvertOptDlg.SetImportFileName(AValue: String);
//var
//  i: Integer;
begin
  FileNameEdit.Text := AValue;
{
  with FilenameCBx do //setup the existing files
  begin
    i := Items.IndexOf(AValue);
    if i < 0 then
      i := Items.Add(AValue);
    ItemIndex := i;
    Text := Items[i];
    FormatCBx.ItemIndex := Ord(GetInFileType(Text)); // default
  end;
  FilenameCBxChange(nil);
}
end;

function  TFormatConvertOptDlg.GetImportFileName: String;
begin
  Result := FileNameEdit.Text;
end;

function  TFormatConvertOptDlg.GetImportFileType: TImportFormatType;
begin
  Result := TImportFormatType(FormatCBx.ItemIndex);
end;

procedure TFormatConvertOptDlg.BitBtn1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if not (FileExists(FileNameEdit.Text) and (FormatCBx.ItemIndex <> 0)) then ShowMessage('Your file does not exist or you have not set a valid data format.');
  Exit;
end;

procedure TFormatConvertOptDlg.FileNameEditChange(Sender: TObject);
begin
    FormatCBx.ItemIndex := ord(GetInFileType(FileNameEdit.Text));
    FormatCBx.Text := FormatCBx.Items[FormatCBx.ItemIndex];
end;

procedure TFormatConvertOptDlg.BitBtn4Click(Sender: TObject);
begin
  with OpenDialog1 do
  begin
    if FileExists(FileNameEdit.Text) then
      FileName := FileNameEdit.Text;
    if Execute then
      FileNameEdit.Text := FileName;
  end;
end;

function TFormatConvertOptDlg.FormHelp(Command: Word; Data: PtrInt;
  var CallHelp: Boolean): Boolean;
begin
  HelpBtnClick(Self);
  CallHelp := False;
  Result := True;
end;

procedure TFormatConvertOptDlg.FormResize(Sender: TObject);
begin
if ClientWidth > ToolBar1.Width then
  ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TFormatConvertOptDlg.HelpBtnClick(Sender: TObject);
var
  aTopic: String;
begin
  aTopic := MapHelpContextToKeyword(HC_Convert_to_MEGA_Format_Dialog_Box);
  ShowContextSensitiveHelp(aTopic);
end;

procedure TFormatConvertOptDlg.SaveBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.


