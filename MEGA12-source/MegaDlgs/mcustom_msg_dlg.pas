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

unit mcustom_msg_dlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, ActnList;

const
  OK_IMG = 5;
  CANCEL_IMG = 0;
  YES_IMG = 7;
  NO_IMG = 4;
  CLOSE_MEGA_IMG = 11;

  QUESTION_ICON = 0;
  WARNING_ICON = 1;

type

  { TMegaCustomMsgDlg }

  TMegaCustomMsgDlg = class(TForm)
    CheckBox1: TCheckBox;
    IconImage: TImage;
    MsgLabel: TLabel;
    OkAction: TAction;
    CancelAction: TAction;
    ActionList1: TActionList;
    IconImagelist: TImageList;
    ButtonsImageList: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Spacer: TPanel;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure ToolBar1Click(Sender: TObject);
  private
    FButtonClicked: Boolean;
    FUsingYesNoButtons: Boolean;

    function GetIsChecked: Boolean;
    procedure SetIsChecked(AValue: Boolean);
    procedure SetUsingYesNoButtons(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent; aParent: TCustomForm; const ACaption, ACheckCaption: String);
    function DoCustomShowModal(const aTitle: String; const cBoxLabel: String; const msg: String; const helpCtx: LongInt; var isChecked: Boolean): Integer;
    procedure SetToUseCloseMegaButtons(value: Boolean);

    property UsingYesNoButtons: Boolean read FUsingYesNoButtons write SetUsingYesNoButtons;
    property IsChecked: Boolean read GetIsChecked write SetIsChecked;
  end;

var
  MegaCustomMsgDlg: TMegaCustomMsgDlg;

implementation

{$R *.lfm}

uses
  mimageform, math;

{ TMegaCustomMsgDlg }

procedure TMegaCustomMsgDlg.FormCreate(Sender: TObject);
begin

end;

procedure TMegaCustomMsgDlg.FormResize(Sender: TObject);
begin
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := max(Panel2.Width + 5, Round((ClientWidth - ToolBar1.Width)/2) - Panel2.Width);
end;

procedure TMegaCustomMsgDlg.OkBtnClick(Sender: TObject);
begin
  FButtonClicked := True;
  if FUsingYesNoButtons then
    ModalResult := mrYes
  else
    ModalResult := mrOK;
end;

procedure TMegaCustomMsgDlg.ToolBar1Click(Sender: TObject);
begin

end;

procedure TMegaCustomMsgDlg.CancelBtnClick(Sender: TObject);
begin
  FButtonClicked := True;
  if FUsingYesNoButtons then
    ModalResult := mrNo
  else
    ModalResult := mrAbort;
end;

procedure TMegaCustomMsgDlg.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  Toolbar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
  Constraints.MinWidth := ToolBar1.Width + 10;
end;

procedure TMegaCustomMsgDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if not FButtonClicked then
    if FUsingYesNoButtons then
      ModalResult := mrNo
    else
      ModalResult := mrAbort;
end;

procedure TMegaCustomMsgDlg.SetUsingYesNoButtons(AValue: Boolean);
begin
  if FUsingYesNoButtons = AValue then Exit;
  FUsingYesNoButtons := AValue;
  if FUsingYesNoButtons then
  begin
    OkAction.ImageIndex := YES_IMG;
    CancelAction.ImageIndex := NO_IMG;
    IconImageList.GetBitmap(QUESTION_ICON, IconImage.Picture.Bitmap);
  end
  else
  begin
    OkAction.ImageIndex := OK_IMG;
    CancelAction.ImageIndex := CANCEL_IMG;
    IconImageList.GetBitmap(WARNING_ICON, IconImage.Picture.Bitmap);
  end;
end;

function TMegaCustomMsgDlg.GetIsChecked: Boolean;
begin
  Result := CheckBox1.Checked;
end;

procedure TMegaCustomMsgDlg.SetIsChecked(AValue: Boolean);
begin
  CheckBox1.Checked := AValue
end;

constructor TMegaCustomMsgDlg.Create(AOwner: TComponent; aParent: TCustomForm; const ACaption, ACheckCaption: String);
begin
  inherited Create(AOwner);
  FUsingYesNoButtons := False;
end;

function TMegaCustomMsgDlg.DoCustomShowModal(const aTitle: String; const cBoxLabel: String; const msg: String; const helpCtx: LongInt; var isChecked: Boolean): Integer;
begin
  FButtonClicked := False;
  Caption := aTitle;
  MsgLabel.Caption := ' ' + msg;
  CheckBox1.Caption := cBoxLabel;
  CheckBox1.Checked := isChecked;
  HelpContext := helpCtx;
  Result := ShowModal;
  isChecked := CheckBox1.Checked;
end;

procedure TMegaCustomMsgDlg.SetToUseCloseMegaButtons(value: Boolean);
begin
  if value then
    OkAction.ImageIndex := CLOSE_MEGA_IMG
  else
    OkAction.ImageIndex := OK_IMG;
  CancelAction.ImageIndex := 0;
end;

end.

