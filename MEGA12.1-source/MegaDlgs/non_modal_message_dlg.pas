{
	Copyright 1992-2025 Sudhir Kumar and Koichiro Tamura

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

unit non_modal_message_dlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, ActnList;

type

  { TNonModalMessageDlg }

  TNonModalMessageDlg = class(TForm)
    DeveloperAction: TAction;
    OkAction: TAction;
    ActionList1: TActionList;
    Label1: TLabel;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure DeveloperActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure OkActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FClickedButton: Integer;
    FDoneCallback: TNotifyEvent;
    FButtons: array of TButton;
    procedure ChoiceButtonClicked(Sender: TObject);
  public
    procedure SetChoices(choices: TStringList; callback: TNotifyEvent);
    function DebugMsg: String;
    function ClickedButton: Integer;
  end;

  function ShowNonModalMessage(aMsg: String; aTitle: String; aPosition: TPoint): TNonModalMessageDlg;
  procedure ShowNonModalQuestionDlg(choices: TStringList; aTitle: String; aMsg: String; aPosition: TPoint; callback: TNotifyEvent);

var
  NonModalMessageDlg: TNonModalMessageDlg;

implementation

uses
  mimageform;

function ShowNonModalMessage(aMsg: String; aTitle: String; aPosition: TPoint): TNonModalMessageDlg;
begin
  Result := TNonModalMessageDlg.Create(nil);
  Result.Caption := aTitle;
  Result.Label1.Caption := aMsg;
  Result.Left := aPosition.X;
  Result.Top := aPosition.Y;
  Result.Show;
  Result.ClientHeight := Result.Panel1.Height + Result.Label1.Height + 50;
end;

procedure ShowNonModalQuestionDlg(choices: TStringList; aTitle: String; aMsg: String; aPosition: TPoint; callback: TNotifyEvent);
var
  dlg: TNonModalMessageDlg = nil;
begin
  dlg := TNonModalMessageDlg.Create(nil);
  dlg.Caption := aTitle;
  dlg.Label1.Caption := aMsg;
  dlg.Label1.Align := alTop;
  dlg.Label1.Height := dlg.Canvas.TextHeight(aMsg);
  dlg.SetChoices(choices, callback);
  dlg.Left := aPosition.X;
  dlg.Top := aPosition.Y;
  dlg.Show;
  dlg.ClientHeight := dlg.Panel1.Height + dlg.Label1.Height + 50;
end;

{$R *.lfm}

{ TNonModalMessageDlg }

procedure TNonModalMessageDlg.OkActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TNonModalMessageDlg.DeveloperActionExecute(Sender: TObject);
begin
  ShowMessage(DebugMsg);
end;

procedure TNonModalMessageDlg.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
end;

procedure TNonModalMessageDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TNonModalMessageDlg.FormCreate(Sender: TObject);
begin
  FormStyle := fsStayOnTop;
  BorderStyle := bsSizeToolWin;
end;

procedure TNonModalMessageDlg.ChoiceButtonClicked(Sender: TObject);
var
  i: Integer = 0;
begin
  FClickedButton := -1;
  for i := Low(FButtons) to High(FButtons) do
  begin
    if FButtons[i] = Sender then
      FClickedButton := i;
  end;
  if Assigned(FDoneCallback) then
    FDoneCallback(Self);
  Close;
end;

procedure TNonModalMessageDlg.SetChoices(choices: TStringList; callback: TNotifyEvent);
var
  i: Integer = 0;
  button: TButton = nil;
  aLeft: Integer = 10;
  aChoice: String = '';
begin
  Toolbutton1.Visible := False;
  SetLength(FButtons, choices.Count);
  for i := 0 to choices.Count - 1 do
  begin
    aChoice := choices[i];
    button := TButton.Create(Self);
    button.AutoSize := True;
    button.Caption := aChoice;
    button.Width := Canvas.TextWidth(aChoice) + 10;
    button.OnClick := @ChoiceButtonClicked;
    button.Parent := Panel1;
    button.Left := aLeft;
    FButtons[i] := button;
    aLeft := aLeft + button.Width + 20;
  end;
  Width := aLeft + 10;
  FDoneCallback := callback;
end;

function TNonModalMessageDlg.DebugMsg: String;
var
  activeForm: TCustomForm = nil;
  aName: String = 'nil';
begin
  ActiveForm := Screen.ActiveForm;
  if Assigned(ActiveForm) then
    aName := ActiveForm.Name;
  Result := Format('ok action enabled = %s %s active form = %s', [BoolToStr(OkAction.Enabled, True), LineEnding, aName]);
end;

function TNonModalMessageDlg.ClickedButton: Integer;
begin
  Result := FClickedButton;
end;

end.

