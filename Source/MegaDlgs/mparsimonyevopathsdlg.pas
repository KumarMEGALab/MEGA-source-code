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

unit MParsimonyEvoPathsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, mimageform;

type

  { TEvoPathsDlg }

  TEvoPathsDlg = class(TForm)
    CheckBox1: TCheckBox;
    NumPathsEdit: TEdit;
    SkipBtn: TButton;
    ContinueBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Image1: TImage;
    InvalidPathsLabel: TStaticText;
    Label3: TLabel;
    Label4: TLabel;
    CountLabel: TLabel;
    Timer1: TTimer;
    procedure ContinueBtnClick(Sender: TObject);
    procedure SkipBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    CountDown: Integer;
    { Private declarations }
  public
    NumPathsToTry: Integer;
    DoPromptAgain: Boolean;
    procedure SetNumPathsToTry(Value: LongInt);
    { Public declarations }
  end;

var
  EvoPathsDlg: TEvoPathsDlg;

implementation

{$R *.lfm}

procedure TEvoPathsDlg.ContinueBtnClick(Sender: TObject);
begin
  if TryStrToInt(NumPathsEdit.Text, NumPathsToTry) then
  begin
    InvalidPathsLabel.Visible := False;
    if CheckBox1.Checked then
      DoPromptAgain := True
    else
      DoPromptAgain := False;
    Self.ModalResult := mrOk;
  end
  else
    InvalidPathsLabel.Visible := True;
end;

procedure TEvoPathsDlg.FormCreate(Sender: TObject);
begin
  CountDown := 30;
  ImageForm.UpdateImgList(Self);
end;

procedure TEvoPathsDlg.SetNumPathsToTry(Value: Integer);
begin
  NumPathsEdit.Text := IntToStr(Value);
  NumPathsToTry := Value;
end;

procedure TEvoPathsDlg.SkipBtnClick(Sender: TObject);
begin
  if TryStrToInt(NumPathsEdit.Text, NumPathsToTry) then
  begin
    InvalidPathsLabel.Visible := False;
    if CheckBox1.Checked then
      DoPromptAgain := True
    else
      DoPromptAgain := False;
    Self.ModalResult := mrAbort;
  end
  else
    InvalidPathsLabel.Visible := True;
end;

procedure TEvoPathsDlg.Timer1Timer(Sender: TObject);
begin
  if Countdown > 0 then
  begin
    dec(Countdown);
    CountLabel.Caption := IntToStr(Countdown)
  end
  else
  begin
    Timer1.Enabled:= False;
    if Checkbox1.Checked then
      DoPromptAgain := True
    else
      DoPromptAgain := False;
    Self.ModalResult := mrOk;
  end;
end;

end.

