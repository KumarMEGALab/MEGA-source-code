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

unit subtree_markers_frame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Dialogs, frame_utils,
  MTreeBox, MegaConsts, Types;

type

  { TSubtreeMarkersFrame }

  TSubtreeMarkersFrame = class(TFrame, ITreeToolbarFrame, IDisablesEventNotifications)
    OverwriteMarkerCkBx: TCheckBox;
    MarkerColorCmbBx: TColorButton;
    MarkerShapeCmbBx: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure MarkerShapeCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    FDisableNotifyEvents: Boolean;
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

    procedure SetSubtreeAttrib(NodeAttrib: TNodeAttrib);
    procedure GetSubtreeAttrib(var NodeAttrib: TNodeAttrib);
    function NotificationsDisabled: Boolean;

    property OptionsChangedNotify: TNotifyEvent read FOptionsChangedNotify write SetOptionsChangedNotify;
  end;

implementation

uses
  Graphics;

{$R *.lfm}

{ TSubtreeMarkersFrame }

procedure TSubtreeMarkersFrame.MarkerShapeCmbBxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  d,c : integer;
  p : array[0..3] of TPoint;
begin
  with MarkerShapeCmbBx.Canvas do
  begin
    d := (aRect.Bottom - aRect.Top) div 2;
    c := Canvas.TextWidth('N')+d;

    FillRect(aRect);
    Pen.Color := clBlack;
    Brush.Color := clBlack;
    case Index of
      1,3,5,7,9  : Brush.Style := bsClear;
      2,4,6,8,10 : Brush.Style := bsSolid;
    end;
    case Index of
      1,2 : Ellipse(c-d+2, aRect.Top+2, c+d-1, aRect.Bottom-1);
      3,4 : Rectangle(c-d+2, aRect.Top+2, c+d-2, aRect.Bottom-2);
      5,6 : begin
              p[0].x := c;
              p[0].y := aRect.Top +3;
              p[1].x := c-d +3;
              p[1].y := aRect.Bottom -2;
              p[2].x := c+d -3;
              p[2].y := aRect.Bottom -2;
              Polygon(Slice(p,3));
            end;
      7,8 : begin
              p[0].x := c-d +3;
              p[0].y := aRect.Top +3;
              p[1].x := c+d -3;
              p[1].y := aRect.Top +3;
              p[2].x := c;
              p[2].y := aRect.Bottom -2;
              Polygon(Slice(p,3));
            end;
      9,10: begin
              p[0].x := c;
              p[0].y := aRect.Top +2;
              p[1].x := c-d +2;
              p[1].y := aRect.Top +d;
              p[2].x := c;
              p[2].y := aRect.Bottom -2;
              p[3].x := c+d -2;
              p[3].y := aRect.Top +d;
              Polygon(p);
            end;
    end;
    Brush.Style := bsClear;
    TextOut(aRect.Left+2, aRect.Top, MarkerShapeCmbBx.Items[Index]);
  end;
end;

procedure TSubtreeMarkersFrame.SetOptionsChangedNotify(AValue: TNotifyEvent);
begin
  if FOptionsChangedNotify = AValue then Exit;
  FOptionsChangedNotify := AValue;
  MarkerShapeCmbBx.OnChange := FOptionsChangedNotify;
  MarkerColorCmbBx.OnColorChanged := FOptionsChangedNotify;
  OverwriteMarkerCkBx.OnChange := FOptionsChangedNotify;
end;

function TSubtreeMarkersFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TSubtreeMarkersFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TSubtreeMarkersFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TSubtreeMarkersFrame.ToggleCollapseBodyPanel;
begin
  if Panel2.Visible then FPanel2Height := Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := FPanel2Height
  else
    Height := Panel1.Height;
end;

procedure TSubtreeMarkersFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
end;

procedure TSubtreeMarkersFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

procedure TSubtreeMarkersFrame.SetSubtreeAttrib(NodeAttrib: TNodeAttrib);
begin
  try
    FDisableNotifyEvents := True;
    //ShowTaxonMarkerCkBx.Checked := NodeAttrib.ShowTaxonMarker;
    OverwriteMarkerCkBx.Checked := NodeAttrib.OverwriteMarker;
    MarkerShapeCmbBx.ItemIndex := Integer(NodeAttrib.Marker.Shape);
    MarkerColorCmbBx.ButtonColor := NodeAttrib.Marker.Color;
  finally
    FDisableNotifyEvents := False;
  end;
end;

procedure TSubtreeMarkersFrame.GetSubtreeAttrib(var NodeAttrib: TNodeAttrib);
begin
  //NodeAttrib.ShowNodeMarker  := ShowNodeMarkerCkBx.Checked;
  //NodeAttrib.ShowTaxonMarker := ShowTaxonMarkerCkBx.Checked;
  NodeAttrib.Marker.Shape := TNodeMarkerShape(MarkerShapeCmbBx.ItemIndex);
  NodeAttrib.Marker.Color := MarkerColorCmbBx.ButtonColor;
  NodeAttrib.OverwriteMarker := OverwriteMarkerCkBx.Checked;
end;

function TSubtreeMarkersFrame.NotificationsDisabled: Boolean;
begin
  Result := FDisableNotifyEvents;
end;

end.

