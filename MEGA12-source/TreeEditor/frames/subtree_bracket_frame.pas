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

unit subtree_bracket_frame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Dialogs, Spin,
  frame_utils, Types, MegaConsts, Graphics, MTreeBox;

type

  { TSubtreeBracketFrame }

  TSubtreeBracketFrame = class(TFrame, ITreeToolbarFrame, IDisablesEventNotifications)
    ShowBracketCBx: TCheckBox;
    BracketColorCmbBx: TColorButton;
    BracketStyleCmbBx: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    BracketWidthSEdit: TSpinEdit;
    procedure BracketStyleCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    FDisableEventNotifications: Boolean;
    FOptionsChangedNotify: TNotifyEvent;
    FPanel2Height: Integer;
    procedure SetOptionsChangedNotify(AValue: TNotifyEvent);
  public
    function GetHeaderPanel: TPanel;
    function GetBodyPanel: TPanel;
    function GetExpandCollapseImage: TImage;
    procedure ToggleCollapseBodyPanel;
    procedure SetParentControl(aParent: TWinControl);
    procedure SetAlignVal(aAlign: TAlign);

    function GetBracketStyle: TBracketStyle;
    function GetBracketLineColor: TColor;
    function GetBracketLineWidth: Integer;

    procedure SetSubtreeAttrib(NodeAttrib: TNodeAttrib);
    procedure GetSubtreeAttrib(var NodeAttrib: TNodeAttrib);
    function NotificationsDisabled: Boolean;

    property OptionsChangedNotify: TNotifyEvent read FOptionsChangedNotify write SetOptionsChangedNotify;
  end;

implementation

{$R *.lfm}

{ TSubtreeBracketFrame }

procedure TSubtreeBracketFrame.BracketStyleCmbBxDrawItem(Control: TWinControl;Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  point : array[0..3] of TPoint;
begin
  with BracketStyleCmbBx do begin
    Canvas.FillRect(arect);
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clBlack;
    case Index of
      0: begin
           point[0].x := aRect.left+3;
           point[1].x := aRect.left+7;
           point[2].x := aRect.left+7;
           point[3].x := aRect.left+2;
           point[0].y := (aRect.top+2);
           point[1].y := (aRect.top+2);
           point[2].y := (aRect.bottom-1);
           point[3].y := (aRect.bottom-1);
           Canvas.PolyLine(point);
         end;
      1: begin
           point[0].x := aRect.left+5;
           point[1].x := aRect.left+5;
           point[0].y := (aRect.top+2);
           point[1].y := (aRect.bottom-1);
           Canvas.PolyLine(Slice(point,2));
         end;
    end;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(aRect.left+12, aRect.top+1, Items[Index]);
  end;
end;

procedure TSubtreeBracketFrame.SetOptionsChangedNotify(AValue: TNotifyEvent);
begin
  if FOptionsChangedNotify = AValue then Exit;
  FOptionsChangedNotify := AValue;
  ShowBracketCBx.OnChange := FOptionsChangedNotify;
  BracketWidthSEdit.OnChange := FOptionsChangedNotify;
  BracketStyleCmbBx.OnChange := FOptionsChangedNotify;
  BracketColorCmbBx.OnColorChanged := FOptionsChangedNotify;
end;

function TSubtreeBracketFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TSubtreeBracketFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TSubtreeBracketFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TSubtreeBracketFrame.ToggleCollapseBodyPanel;
begin
  if Panel2.Visible then FPanel2Height := Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := FPanel2Height
  else
    Height := Panel1.Height;
end;

procedure TSubtreeBracketFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
end;

procedure TSubtreeBracketFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

function TSubtreeBracketFrame.GetBracketStyle: TBracketStyle;
begin
  case BracketStyleCmbBx.ItemIndex of
    0: Result := brsSquare;
    1: Result := brsLine;
    2: Result := brsNone;
    else
      ShowMessage('missing bracket style');
  end;
end;

function TSubtreeBracketFrame.GetBracketLineColor: TColor;
begin
  Result := BracketColorCmbBx.ButtonColor;
end;

function TSubtreeBracketFrame.GetBracketLineWidth: Integer;
begin
  Result := BracketWidthSEdit.Value;
end;

procedure TSubtreeBracketFrame.SetSubtreeAttrib(NodeAttrib: TNodeAttrib);
begin
  try
    FDisableEventNotifications := True;
    ShowBracketCBx.Checked     := NodeAttrib.ShowBracket;
    case NodeAttrib.BracketStyle of
     brsSquare: BracketStyleCmbBx.ItemIndex := 0;
     brsLine  : BracketStyleCmbBx.ItemIndex := 1;
     brsNone  : BracketStyleCmbBx.ItemIndex := 2;
    end;
    BracketColorCmbBx.ButtonColor := NodeAttrib.BracketColor;
    BracketWidthSEdit.Value := NodeAttrib.BracketLineWidth;
  finally
    FDisableEventNotifications := False;
  end;
end;

procedure TSubtreeBracketFrame.GetSubtreeAttrib(var NodeAttrib: TNodeAttrib);
begin
  NodeAttrib.ShowBracket := ShowBracketCBx.Checked;
  case BracketStyleCmbBx.ItemIndex of
    0: NodeAttrib.BracketStyle := brsSquare;
    1: NodeAttrib.BracketStyle := brsLine;
    2: NodeAttrib.BracketStyle := brsNone;
  end;
  NodeAttrib.BracketColor := BracketColorCmbBx.ButtonColor;
  NodeAttrib.BracketLineWidth := BracketWidthSEdit.Value;
end;

function TSubtreeBracketFrame.NotificationsDisabled: Boolean;
begin
  Result := FDisableEventNotifications;
end;

end.

