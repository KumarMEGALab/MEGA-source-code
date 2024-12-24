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

unit mupdatesdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLIntf;

type

  { TUpdatesDlg }

  TUpdatesDlg = class(TForm)
    CancelBtn: TButton;
    DownloadBtn: TButton;
    HeaderLbl: TLabel;
    IgnoreUpdateCheckBox: TCheckBox;
    MessagesMemo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure CancelBtnClick(Sender: TObject);
    procedure DownloadBtnClick(Sender: TObject);
  private
    FDownloadUrl: String;
    procedure SetDownloadUrl(AValue: String);
    { private declarations }
  public

    property DownloadUrl: String read FDownloadUrl write SetDownloadUrl;
    { public declarations }
  end;

var
  UpdatesDlg: TUpdatesDlg;

implementation

{$R *.lfm}

{ TUpdatesDlg }

procedure TUpdatesDlg.DownloadBtnClick(Sender: TObject);
begin
  try
    OpenURL(FDownloadUrl);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when attempting to download the update: ' + E.Message);
  end;
end;

procedure TUpdatesDlg.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TUpdatesDlg.SetDownloadUrl(AValue: String);
begin
  if FDownloadUrl=AValue then Exit;
  FDownloadUrl:=AValue;
end;

end.

