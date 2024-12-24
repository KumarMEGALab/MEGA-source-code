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

unit MPleaseWaitDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Buttons, mimageform;

type

  { TPleaseWaitDlg }

  TPleaseWaitDlg = class(TForm)
    CancelBtn: TBitBtn;
    ProgressBar1: TProgressBar;
    StringDispPanel: TPanel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure SetMyAction(Value: String);
    procedure SetPercentDone(Value: Integer);
    function GetIsCancel: Boolean;
  public
    { public declarations }
    procedure SetShowCancel(Value: Boolean=True);
    procedure UseProgressTimer(Value: Boolean=True);
    procedure SetToMarqueeMode;
    property IsCancel: Boolean read GetIsCancel;

    property MyAction: String  write SetMyAction;
    property PercentDone: Integer write SetPercentDone;
  end;

var
  PleaseWaitDlg: TPleaseWaitDlg;

implementation

uses
  math;

{$R *.lfm}

{ TPleaseWaitDlg }

procedure TPleaseWaitDlg.Timer1Timer(Sender: TObject);
begin
  if ProgressBar1.Position < 100 then
    ProgressBar1.StepIt
  else
    ProgressBar1.Position := 0;
end;

procedure TPleaseWaitDlg.FormCreate(Sender: TObject);
begin
  ProgressBar1.Position := 0;
  Timer1.Enabled := True;
  StringDispPanel.Font.Size := 10;
  StringDispPanel.Font.Color := clNavy;
  StringDispPanel.Font.Style := [fsItalic];
  FormStyle := fsStayOnTop;
  ImageForm.UpdateImgList(Self);
end;

procedure TPleaseWaitDlg.SetMyAction(Value: String);
begin
  StringDispPanel.Caption := Value;
  StringDispPanel.Invalidate;
end;

procedure TPleaseWaitDlg.SetPercentDone(Value: Integer);
begin
  ProgressBar1.Position := Min(100, Value);
end;

function TPleaseWaitDlg.GetIsCancel: Boolean;
begin
  Result := CancelBtn.Visible;
end;

procedure TPleaseWaitDlg.SetShowCancel(Value: Boolean);
begin
  if Value then
    CancelBtn.Visible := True
  else
  begin
    CancelBtn.Visible := False;
    BorderIcons := [];
  end;
end;

procedure TPleaseWaitDlg.UseProgressTimer(Value: Boolean);
begin
  if Value then
    Timer1.Enabled := True
  else
    Timer1.Enabled := False;
end;

procedure TPleaseWaitDlg.SetToMarqueeMode;
begin
  ProgressBar1.Style := pbstMarquee;
end;

end.

