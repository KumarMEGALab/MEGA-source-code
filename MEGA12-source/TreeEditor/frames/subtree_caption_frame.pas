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

unit subtree_caption_frame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, EditBtn, Dialogs,
  Spin, frame_utils, MTreebox, Graphics;

type

  { TSubtreeCaptionFrame }

  TSubtreeCaptionFrame = class(TFrame, ITreeToolbarFrame, IDisablesEventNotifications)
    ShowCaptionCBx: TCheckBox;
    AlignVerticallyCheckbox: TCheckBox;
    NameEditBx: TEditButton;
    FontDialog1: TFontDialog;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure NameEditBxButtonClick(Sender: TObject);
  private
    FDisableEventNotifications: Boolean;
    FOptionsChangedNotify: TNotifyEvent;
    FPanel2Height: Integer;
    function GetAlignCaption: Boolean;
    procedure SetAlignCaption(AValue: Boolean);
    procedure SetOptionsChangedNotify(AValue: TNotifyEvent);
  public
    function GetHeaderPanel: TPanel;
    function GetBodyPanel: TPanel;
    function GetExpandCollapseImage: TImage;
    procedure ToggleCollapseBodyPanel;
    procedure SetParentControl(aParent: TWinControl);
    procedure SetAlignVal(aAlign: TAlign);

    function GetCaption: String;
    procedure GetCaptionFont(var aFont: TFont);
    function GetDisplayCaption: Boolean;
    function GetAlignVertically: Boolean;


    procedure SetSubtreeAttrib(NodeAttrib: TNodeAttrib);
    procedure GetSubtreeAttrib(var NodeAttrib: TNodeAttrib);
    function NotificationsDisabled: Boolean;

    property OptionsChangedNotify: TNotifyEvent read FOptionsChangedNotify write SetOptionsChangedNotify;
    property AlignCaption: Boolean read GetAlignCaption write SetAlignCaption;
  end;

implementation

{$R *.lfm}

{ TSubtreeCaptionFrame }

procedure TSubtreeCaptionFrame.NameEditBxButtonClick(Sender: TObject);
begin
  if FontDialog1.Execute and Assigned(FOptionsChangedNotify) then
    FOptionsChangedNotify(Sender);
end;

procedure TSubtreeCaptionFrame.SetOptionsChangedNotify(AValue: TNotifyEvent);
begin
  if FOptionsChangedNotify = AValue then Exit;
  FOptionsChangedNotify := AValue;
  NameEditBx.OnEditingDone := FOptionsChangedNotify;
  ShowCaptionCBx.OnChange := FOptionsChangedNotify;
  AlignVerticallyCheckbox.OnChange := FOptionsChangedNotify;
end;

function TSubtreeCaptionFrame.GetAlignCaption: Boolean;
begin
  Result := AlignVerticallyCheckbox.Checked;
end;

procedure TSubtreeCaptionFrame.SetAlignCaption(AValue: Boolean);
begin
  AlignVerticallyCheckbox.Checked := AValue;
end;

function TSubtreeCaptionFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TSubtreeCaptionFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TSubtreeCaptionFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TSubtreeCaptionFrame.ToggleCollapseBodyPanel;
begin
  if Panel2.Visible then FPanel2Height := Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := FPanel2Height
  else
    Height := Panel1.Height;
end;

procedure TSubtreeCaptionFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
end;

procedure TSubtreeCaptionFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

function TSubtreeCaptionFrame.GetCaption: String;
begin
  Result := NameEditBx.Text;
end;

procedure TSubtreeCaptionFrame.GetCaptionFont(var aFont: TFont);
begin
  aFont.Assign(FontDialog1.Font);
end;

function TSubtreeCaptionFrame.GetDisplayCaption: Boolean;
begin
  Result := ShowCaptionCBx.Checked;
end;

function TSubtreeCaptionFrame.GetAlignVertically: Boolean;
begin
  Result := AlignVerticallyCheckbox.Checked;
end;

procedure TSubtreeCaptionFrame.SetSubtreeAttrib(NodeAttrib: TNodeAttrib);
begin
  if FDisableEventNotifications then Exit;
  try
    FDisableEventNotifications := True;
    ShowCaptionCBx.Checked := NodeAttrib.ShowCaption;
    NameEditBx.Text := NodeAttrib.Caption;
    FontDialog1.Font.Assign(NodeAttrib.CaptionFont);
  finally
    FDisableEventNotifications := False;
  end;
end;

procedure TSubtreeCaptionFrame.GetSubtreeAttrib(var NodeAttrib: TNodeAttrib);
begin
  NodeAttrib.Caption := NameEditBx.Text;
  NodeAttrib.ShowCaption := ShowCaptionCBx.Checked;
  NodeAttrib.CaptionFont.Assign(FontDialog1.Font);
end;

function TSubtreeCaptionFrame.NotificationsDisabled: Boolean;
begin
  Result := FDisableEventNotifications;
end;

end.

