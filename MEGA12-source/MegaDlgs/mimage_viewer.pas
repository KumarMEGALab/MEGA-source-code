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

unit mimage_viewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  ComCtrls, ActnList;

type

  { TMegaImageViewer }

  TMegaImageViewer = class(TForm)
    Image1: TImage;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    OpenDialog1: TOpenDialog;
    QuitAction: TAction;
    LoadFromFileAction: TAction;
    SaveDialog1: TSaveDialog;
    SaveToFileAction: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LoadFromFileActionExecute(Sender: TObject);
    procedure QuitActionExecute(Sender: TObject);
    procedure SaveToFileActionExecute(Sender: TObject);
  private

  public
    OnCloseNotify: TNotifyEvent;
    procedure SetCaption(aCaption: String);
    procedure LoadImageFromFile(filename: String);
  end;

var
  MegaImageViewer: TMegaImageViewer;

implementation

uses
  MegaVerConsts;

{$R *.lfm}

{ TMegaImageViewer }

procedure TMegaImageViewer.FormCreate(Sender: TObject);
begin
  OpenDialog1.Filter := 'Images|*.png;*.jpg;*jpeg;*.bmp|All files|*.*';
  SaveDialog1.Filter := 'PNG Image|*.png';
  Caption := Format('%s: %s', [VER_MEGA_CAPTION, 'Image Viewer']);
end;

procedure TMegaImageViewer.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  if Assigned(OnCloseNotify) then
    OnCloseNotify(Self);
end;

procedure TMegaImageViewer.LoadFromFileActionExecute(Sender: TObject);
begin
  try
    if OpenDialog1.Execute then
    begin
      LoadImageFromFile(OpenDialog1.FileName);
    end;
  except
    on E:Exception do
      ShowMessage('Application error: ' + E.Message);
  end;
end;

procedure TMegaImageViewer.QuitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TMegaImageViewer.SaveToFileActionExecute(Sender: TObject);
begin
  try
    if SaveDialog1.Execute then
    begin
      Image1.Picture.SaveToFile(SaveDialog1.FileName);
      StatusBar1.SimpleText := SaveDialog1.FileName;
    end;
  except
    on E:Exception do
      ShowMessage('Application error: ' + E.Message);
  end;
end;

procedure TMegaImageViewer.SetCaption(aCaption: String);
begin
  Caption := Format('%s: %s', [VER_MEGA_WIN_CAPTION_PREFIX, aCaption]);
end;

procedure TMegaImageViewer.LoadImageFromFile(filename: String);
begin
  Image1.Picture.LoadFromFile(filename);
  Image1.Width := Image1.Picture.Width;
  Image1.Height := Image1.Picture.Height;
  ClientWidth := Image1.Width + 20;
  ClientHeight := Image1.Height + StatusBar1.Height + 40;
  StatusBar1.SimpleText := filename;
end;


end.

