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

unit mtreeinfoform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Menus, IniPropStorage, mimageform, MegaUtils,
  MegaPrivateFiles, tree_info_frame;

type

  { TTreeInfoForm }

  TTreeInfoForm = class(TForm)
    FontDialog1: TFontDialog;
    CopyItem: TMenuItem;
    IniPropStorage1: TIniPropStorage;
    myPopupMenu: TPopupMenu;
    StayOnTopChkBx: TCheckBox;
    Panel1: TPanel;
    procedure CopyItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure myPopupMenuPopup(Sender: TObject);
    procedure StayOnTopChkBxClick(Sender: TObject);
  private
    FTreeLength: Integer;
    function GetTreeLength: Integer;

    procedure SetStayOnTop;
  public
    InfoFrame: TTreeInfoFrame;
  published
    property TreeLength: Integer read GetTreeLength write FTreeLength; { used by TLegendGenerator for the MP user tree caption}
  end;

var
  TreeInfoForm: TTreeInfoForm;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  MTreeViewForm,
  {$ENDIF}
  TreeExplorer_HC, MegaVerConsts, Clipbrd;

{$R *.lfm}

{ TTreeInfoForm }

procedure TTreeInfoForm.SetStayOnTop;
begin
  if StayOnTopChkBx.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TTreeInfoForm.FormCreate(Sender: TObject);
begin
  HelpContext := HC_TEx_Information_Box;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Information';
  SetStayOnTop;
  ImageForm.UpdateImgList(Self);
  IniPropStorage1.IniFileName := GetPrivateFile(MEGASessionFile);
  InfoFrame := nil;
end;

procedure TTreeInfoForm.FormDestroy(Sender: TObject);
begin
  if Assigned(InfoFrame) then
    InfoFrame.IsClosing := True;
end;

procedure TTreeInfoForm.FormResize(Sender: TObject);
begin
  if Assigned(InfoFrame) and (not InfoFrame.IsClosing) then
    InfoFrame.UpdateColumnWidths;
end;

procedure TTreeInfoForm.FormShow(Sender: TObject);
begin
  if Assigned(InfoFrame) then
    InfoFrame.IsClosing := False;
end;

procedure TTreeInfoForm.myPopupMenuPopup(Sender: TObject);
begin
  if Assigned(InfoFrame) and (InfoFrame.GetSelectedText <> EmptyStr) then
    CopyItem.Enabled := True
  else
    CopyItem.Enabled := False;
end;

procedure TTreeInfoForm.CopyItemClick(Sender: TObject);
begin
  if Assigned(InfoFrame) then
    Clipboard.AsText := InfoFrame.GetSelectedText;
end;

procedure TTreeInfoForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  InfoFrame.IsClosing := True;
  CloseAction := caHide;
  (Owner as TTreeViewForm).ActionInfo.Checked := False;
end;

procedure TTreeInfoForm.StayOnTopChkBxClick(Sender: TObject);
begin
  SetStayOnTop;
end;

function TTreeInfoForm.GetTreeLength: Integer;
begin
  Result := FTreeLength;
end;

end.

