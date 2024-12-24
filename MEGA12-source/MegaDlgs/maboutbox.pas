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

unit maboutbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ActnList, mimageform;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    Button1: TButton;
    CloseAction: TAction;
    ActionList1: TActionList;
    Image1: TImage;
    Image2: TImage;
    KoichiroLabel: TLabel;
    GlenLabel: TLabel;
    DebugLabel: TLabel;
    AuthorLabel: TLabel;
    MaxLabel: TLabel;
    MikeLabel: TLabel;
    SudipLabel: TLabel;
    SudhirLabel: TLabel;
    VersionLabel: TLabel;
    CopyRightLabel: TLabel;
    procedure CloseActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image2DblClick(Sender: TObject);
  private

  public
    authenticated: Boolean;
    function authenticate: Boolean;
  end;

var
  AboutBox: TAboutBox;

procedure ShowAboutBox;

implementation

{$R *.lfm}

uses
  MegaVerConsts, MegaConsts, md5, mega_main;

procedure ShowAboutBox;
begin
  try
    AboutBox := TAboutBox.Create(MegaForm);
    AboutBox.ShowModal;
  finally
    FreeAndNil(AboutBox);
  end;
end;

{ TAboutBox }

procedure TAboutBox.FormCreate(Sender: TObject);
var
  Year, Month, Day: Word;
begin
  {$IFNDEF DEBUG}
  DebugLabel.Visible := False;
  {$ENDIF}
  Color := clDefault;
  DecodeDate(Date, Year, Month, Day);
  CopyrightLabel.Caption := Format('%s 1993-%d by authors and institutions', ['©',Year]);
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': About MEGA (Build#: ' + VER_MEGA_BUILD + ')';
  VersionLabel.Caption := 'Version ' + VER_MEGA_MAJOR_CHAR + VER_MEGA_MINOR;
  CopyrightLabel.Font.Color := clBlack;
  VersionLabel.Font.Color := clBlack;
  KoichiroLabel.Font.Color := clBlack;
  SudhirLabel.Font.Color := clBlack;
  GlenLabel.Font.Color := clBlack;
  SudipLabel.Font.Color := clBlack;
  DebugLabel.Font.Color := clBlack;
  AuthorLabel.Font.Color := clBlack;
  MikeLabel.Font.Color := clBlack;
  MaxLabel.Font.Color := clBlack;
  ImageForm.UpdateImgList(Self);
end;

procedure TAboutBox.CloseActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.Image2DblClick(Sender: TObject);
begin
  if authenticate then
    MegaForm.DevSettingsActionExecute(Sender)
  else
    ShowMessage('Authentication failed');
end;

function TAboutBox.authenticate: Boolean;
var
  password: String;
  hashed: String;
begin
  if not authenticated then
  begin
    password := PasswordBox('Authentication Needed', 'Password');
    if Trim(password) <> EmptyStr then
    begin
      hashed := MD5Print(MD5String(password));
      if hashed = '9c86d795e1412bb824d86a916f70a822' then
        authenticated := True;
    end;
  end;
  result := authenticated;
end;

end.

