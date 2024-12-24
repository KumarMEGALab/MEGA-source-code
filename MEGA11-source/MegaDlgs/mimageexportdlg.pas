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

unit mimageexportdlg;

{$mode objfpc}{$H+}

interface

uses
  LCLIntF, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, MegaConsts;

type

  { TImageExportForm }

  TImageExportForm = class(TForm)
    FormatListBox: TComboBox;
    Label2: TLabel;
    SaveBtn: TImage;
    BrowseBtn: TImage;
    CancelBtn: TImage;
    ImageList1: TImageList;
    ButtonImages: TImageList;
    BrowseImages: TImageList;
    SaveAction: TAction;
    GetFilenameAction: TAction;
    ActionList1: TActionList;
    FilenameEdit: TEdit;
    Label1: TLabel;
    SaveDialog: TSaveDialog;
    procedure BrowseBtnMouseEnter(Sender: TObject);
    procedure BrowseBtnMouseLeave(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure CancelBtnMouseEnter(Sender: TObject);
    procedure CancelBtnMouseLeave(Sender: TObject);
    procedure FormatListBoxSelectionChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GetFilenameActionExecute(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure SaveBtnMouseEnter(Sender: TObject);
    procedure SaveBtnMouseLeave(Sender: TObject);
  private
    function GetFilename: String;
    function ValidateForm: Integer;
  public
    function GetExportFormat: TImageFileFormat;
    procedure SetExportFormat(aFormat: TImageFileFormat);
    property Filename: String read GetFilename;
    { public declarations }
  end;

var
  ImageExportForm: TImageExportForm;

implementation

uses
  LazFileUtils, mshortcutshelper;

{$R *.lfm}

{ TImageExportForm }

procedure TImageExportForm.FormatListBoxSelectionChanged(Sender: TObject);
var
  ext: String;
begin
  if Trim(FilenameEdit.Text) <> EmptyStr then
  begin
    case FormatListBox.ItemIndex of
      0: ext := '.bmp';
      1: ext := '.pdf';
      2: ext := '.png';
      3: ext := '.svg';
      4: ext := '.tif';
      5: ext := '.emf';
      else
        raise Exception.Create('Invalid image file format');
    end;
    FilenameEdit.Text := ChangeFileExt(FilenameEdit.Text, ext);
  end;
end;

procedure TImageExportForm.FormCreate(Sender: TObject);
begin
  UpdateShortcutsForMacOs(ActionList1);
  {$IFNDEF MSWINDOWS}
  FormatListBox.Items.Delete(FormatListBox.Items.Count - 1);
  {$ENDIF}
  Color := RGB($f7, $f8, $f8);
  Label1.Font.Style := [fsBold];
  Label1.Font.Color := clGrayText;
  Label2.Font.Style := [fsBold];
  Label2.Font.Color := clGrayText;
  {$IFDEF DEBUG}
    FilenameEdit.Text := GetUserDir + 'Downloads' + PathDelim + 'imageExportTest';
  {$ENDIF}
end;

procedure TImageExportForm.CancelBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(3, CancelBtn.Picture.Bitmap);
end;

procedure TImageExportForm.BrowseBtnMouseEnter(Sender: TObject);
begin
  BrowseImages.GetBitmap(1, BrowseBtn.Picture.Bitmap);
end;

procedure TImageExportForm.BrowseBtnMouseLeave(Sender: TObject);
begin
  BrowseImages.GetBitmap(0, BrowseBtn.Picture.Bitmap);
end;

procedure TImageExportForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TImageExportForm.CancelBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(2, CancelBtn.Picture.Bitmap);
end;

procedure TImageExportForm.GetFilenameActionExecute(Sender: TObject);
var
  filter: String;
begin
  SaveDialog.InitialDir := GetCurrentDirUTF8;
  case FormatListBox.ItemIndex of
    0: filter := 'Windows Bitmap(.bmp)|*.bmp|All files|*.*';
    1: filter := 'Portable Document Format(.pdf)|*.pdf|All files|*.*';
    2: filter := 'Portable Network Graphics(.png)|*.png|All files|*.*';
    3: filter := 'Scalable Vector Graphics (.svg)|*.svg|All files|*.*';
    4: filter := 'Tagged Image Format(.tif)|*.tif|All files|*.*';
    5: filter := 'Enhanced Meta File (.emf)|*.emf|All files|*.*';
  end;
  SaveDialog.Filter := filter;
  SaveDialog.FilterIndex := 0;
  if SaveDialog.Execute then
    FilenameEdit.Text := SaveDialog.Filename;
end;

procedure TImageExportForm.SaveActionExecute(Sender: TObject);
begin
  ModalResult := ValidateForm;
end;

procedure TImageExportForm.SaveBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(1, SaveBtn.Picture.Bitmap);
end;

procedure TImageExportForm.SaveBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(0, SaveBtn.Picture.Bitmap);
end;

function TImageExportForm.GetFilename: String;
begin
  Result := FilenameEdit.Text;
end;

function TImageExportForm.ValidateForm: Integer;
var
  Response: Integer;
  valid: Boolean;
begin
  valid := DirectoryExistsUTF8(ExtractFilePath(FilenameEdit.Text));
  if not valid then
  begin
    ShowMessage('The specified save location does not exist');
    Result := mrCancel;
    Exit;
  end;
  case FormatListBox.ItemIndex of
    0: FilenameEdit.Text := ChangeFileExt(FilenameEdit.Text, '.bmp');
    1: FilenameEdit.Text := ChangeFileExt(FilenameEdit.Text, '.pdf');
    2: FilenameEdit.Text := ChangeFileExt(FilenameEdit.Text, '.png');
    3: FilenameEdit.Text := ChangeFileExt(FilenameEdit.Text, '.svg');
    4: FilenameEdit.Text := ChangeFileExt(FilenameEdit.Text, '.tif');
    5: FilenameEdit.Text := ChangeFileExt(FilenameEdit.Text, '.emf');
  end;
  if FileExists(FilenameEdit.Text) then
  begin
    Response := MessageDlg('Overwrite File?', 'The specified file already exists. Overwrite it?', mtConfirmation, mbYesNoCancel, 0);
    if Response = mrYes then
      Result := mrOk
    else
      Result := mrCancel;
  end
  else
    Result := mrOk;
end;

function TImageExportForm.GetExportFormat: TImageFileFormat;
begin
  case FormatListBox.ItemIndex of
    0: Result := iffBitmap;
    1: Result := iffPdf;
    2: Result := iffPng;
    3: Result := iffSvg;
    4: Result := iffTiff;
    5: Result := iffEmf;
    else
      raise Exception.Create('Invalid image file format');
  end;
end;

procedure TImageExportForm.SetExportFormat(aFormat: TImageFileFormat);
begin
  case aFormat of
    iffPng: FormatListBox.ItemIndex := 2;
    iffBitmap: FormatListBox.ItemIndex := 0;
    iffTiff: FormatListBox.ItemIndex := 4;
    iffPdf: FormatListBox.ItemIndex := 1;
    iffSvg: FormatListBox.ItemIndex := 3;
    iffEmf: FormatListBox.ItemIndex :=5;
    else
      raise Exception.Create('Invalid image file format');
  end;
  FormatListBoxSelectionChanged(Self);
end;

end.

