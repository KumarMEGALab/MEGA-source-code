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

unit subtree_branches_frame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, Types,
  MTreeBox, frame_utils, Graphics, MegaConsts;

type

  { TSubtreeBranchesFrame }

  TSubtreeBranchesFrame = class(TFrame, ITreeToolbarFrame, IDisablesEventNotifications)
    BranchOptionCmbBx: TComboBox;
    BranchStyleCmbBx: TComboBox;
    BranchWidthCmbBx: TComboBox;
    ColorCmbBx: TColorButton;
    Image1: TImage;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure BranchOptionCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure BranchStyleCmbBxChange(Sender: TObject);
    procedure BranchStyleCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure BranchWidthCmbBxChange(Sender: TObject);
    procedure BranchWidthCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    FDisableEventNotifications: Boolean;
    FPanel2Height: Integer;
    FOptionsChangedNotify: TNotifyEvent;
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

    function GetPenStyle: TPenStyle;
    function GetBrushStyle: TBrushStyle;
    function GetBranchStyle: TBranchOption;
    function GetBranchLineColor: TColor;
    property OptionsChangedNotify: TNotifyEvent read FOptionsChangedNotify write SetOptionsChangedNotify;
  end;

implementation


{$R *.lfm}

{ TSubtreeBranchesFrame }

procedure TSubtreeBranchesFrame.BranchOptionCmbBxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var point : array[0..3] of TPoint;
begin
  with BranchOptionCmbBx do begin
    Canvas.FillRect(arect);
    if Index = 3 then
    begin
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := clBlack;
    end
    else
    begin
      Canvas.Pen.Width := 3;
      Canvas.Pen.Color := clRed;
    end;
    point[0].x := arect.left+55;
    point[1].x := arect.left+30;
    point[2].x := arect.left+30;
    point[3].x := arect.left+55;
    point[0].y := (arect.top+3);
    point[1].y := (arect.top+3);
    point[2].y := (arect.bottom-3);
    point[3].y := (arect.bottom-3);
    Canvas.PolyLine(point);

    point[0].x := arect.left+5;
    point[1].x := arect.left+5;
    point[2].x := arect.left+30;
    point[0].y := arect.bottom-3;
    point[1].y := (arect.top+arect.bottom) div 2;
    point[2].y := (arect.top+arect.bottom) div 2;
    case Index of
      0: begin
           point[0].x := arect.left+5;
           point[1].x := arect.left+5;
           point[2].x := arect.left+30;
           point[0].y := arect.bottom-3;
           point[1].y := (arect.top+arect.bottom) div 2;
           point[2].y := (arect.top+arect.bottom) div 2;
           Canvas.PolyLine(Slice(point,3));
         end;
      1,3: begin
           Canvas.Pen.Width := 3;
           Canvas.Pen.Color := clRed;
           point[0].x := arect.left+5;
           point[1].x := arect.left+30;
           point[0].y := (arect.top+arect.bottom) div 2;
           point[1].y := (arect.top+arect.bottom) div 2;
           Canvas.PolyLine(Slice(point,2));
           Canvas.Pen.Width := 1;
           Canvas.Pen.Color := clBlack;
           point[0].x := arect.left+5;
           point[1].x := arect.left+5;
           point[0].y := arect.bottom-3;
           point[1].y := (arect.top+arect.bottom) div 2 -2;
           Canvas.PolyLine(Slice(point,2));
         end;
      2: begin
           Canvas.Pen.Width := 1;
           Canvas.Pen.Color := clBlack;
           point[0].x := arect.left+5;
           point[1].x := arect.left+5;
           point[2].x := arect.left+30;
           point[0].y := arect.bottom-3;
           point[1].y := (arect.top+arect.bottom) div 2;
           point[2].y := (arect.top+arect.bottom) div 2;
           Canvas.PolyLine(Slice(point,3));
         end;
    end;
  end;
end;

procedure TSubtreeBranchesFrame.BranchStyleCmbBxChange(Sender: TObject);
begin
  if BranchStyleCmbBx.ItemIndex <> 0 then
    BranchWidthCmbBx.ItemIndex := 1;
  if Assigned(FOptionsChangedNotify) then
    FOptionsChangedNotify(Sender);
end;

procedure TSubtreeBranchesFrame.BranchStyleCmbBxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  point : array[0..1] of TPoint;
  textsize : integer;
begin
  with BranchStyleCmbBx do begin
    Canvas.FillRect(arect);
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;
    textsize := Canvas.TextWidth(Items[1]);
    point[0].x := aRect.left+4;
    point[1].x := aRect.right-textsize-6;
    case Index of
      0   : begin
              Canvas.Pen.Style := psSolid;
              point[0].y := (aRect.top+aRect.bottom) div 2;
              point[1].y := (aRect.top+aRect.bottom) div 2;
              Canvas.Polyline(point);
            end;
      1   : begin
              Canvas.Pen.Style := psDash;
              point[0].y := (aRect.top+aRect.bottom) div 2;
              point[1].y := (aRect.top+aRect.bottom) div 2;
              Canvas.Polygon(point);
            end;
      2   : begin
              Canvas.Pen.Style := psDot;
              point[0].y := (aRect.top+aRect.bottom) div 2;
              point[1].y := (aRect.top+aRect.bottom) div 2;
              Canvas.Polygon(point);
            end;
    end;
    Canvas.TextOut(aRect.right-textsize-2, aRect.top+2, Items[Index]);
  end;
end;

procedure TSubtreeBranchesFrame.BranchWidthCmbBxChange(Sender: TObject);
begin
  if BranchWidthCmbBx.ItemIndex <> 1 then
    BranchStyleCmbBx.ItemIndex := 0;
  if Assigned(FOptionsChangedNotify) then
    FOptionsChangedNotify(Sender);
end;

procedure TSubtreeBranchesFrame.BranchWidthCmbBxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  point : array[0..3] of TPoint;
  textsize : integer;
begin
  with BranchWidthCmbBx do
  begin
    Canvas.FillRect(arect);
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := clBlack;
    Canvas.Brush.Style := bsSolid;
    textsize := Canvas.TextWidth(Items[Index]);
    point[0].x := arect.left+4;
    point[1].x := arect.right-textsize-4;
    point[2].x := arect.right-textsize-4;
    point[3].x := arect.left+4;
    case Index of
      0,1 : begin
              point[0].y := (arect.top+arect.bottom) div 2;
              point[1].y := (arect.top+arect.bottom) div 2;
              Canvas.Polyline(Slice(point,2));
            end;
      2   : begin
              point[0].y := (arect.top+arect.bottom) div 2;
              point[1].y := (arect.top+arect.bottom) div 2;
              point[2].y := (arect.top+arect.bottom) div 2 +1;
              point[3].y := (arect.top+arect.bottom) div 2 +1;
              Canvas.Polygon(point);
            end;
      3   : begin
              point[0].y := (aRect.top+aRect.bottom) div 2 -1;
              point[1].y := (aRect.top+aRect.bottom) div 2 -1;
              point[2].y := (aRect.top+aRect.bottom) div 2 +1;
              point[3].y := (aRect.top+aRect.bottom) div 2 +1;
              Canvas.Polygon(point);
            end;
      4   : begin
              point[0].y := (aRect.top+aRect.bottom) div 2 -1;
              point[1].y := (aRect.top+aRect.bottom) div 2 -1;
              point[2].y := (aRect.top+aRect.bottom) div 2 +2;
              point[3].y := (aRect.top+aRect.bottom) div 2 +2;
              Canvas.Polygon(point);
            end;
      5   : begin
              point[0].y := (aRect.top+aRect.bottom) div 2 -2;
              point[1].y := (aRect.top+aRect.bottom) div 2 -2;
              point[2].y := (aRect.top+aRect.bottom) div 2 +2;
              point[3].y := (aRect.top+aRect.bottom) div 2 +2;
              Canvas.Polygon(point);
            end;
      6   : begin
              point[0].y := (aRect.top+aRect.bottom) div 2 -2;
              point[1].y := (aRect.top+aRect.bottom) div 2 -2;
              point[2].y := (aRect.top+aRect.bottom) div 2 +3;
              point[3].y := (aRect.top+aRect.bottom) div 2 +3;
              Canvas.Polygon(point);
            end;
      7   : begin
              point[0].y := (aRect.top+aRect.bottom) div 2 -3;
              point[1].y := (aRect.top+aRect.bottom) div 2 -3;
              point[2].y := (aRect.top+aRect.bottom) div 2 +4;
              point[3].y := (aRect.top+aRect.bottom) div 2 +4;
              Canvas.Polygon(point);
            end;
      8  : begin
              point[0].y := (aRect.top+aRect.bottom) div 2 -4;
              point[1].y := (aRect.top+aRect.bottom) div 2 -4;
              point[2].y := (aRect.top+aRect.bottom) div 2 +5;
              point[3].y := (aRect.top+aRect.bottom) div 2 +5;
              Canvas.Polygon(point);
            end;
    end;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(aRect.right-textsize-2, aRect.top+2, Items[Index]);
  end;
end;

procedure TSubtreeBranchesFrame.SetOptionsChangedNotify(AValue: TNotifyEvent);
begin
  if FOptionsChangedNotify = AValue then Exit;
  FOptionsChangedNotify := AValue;
  BranchOptionCmbBx.OnChange := FOptionsChangedNotify;
  ColorCmbBx.OnColorChanged := FOptionsChangedNotify;
end;

function TSubtreeBranchesFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TSubtreeBranchesFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TSubtreeBranchesFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TSubtreeBranchesFrame.ToggleCollapseBodyPanel;
begin
  if Panel2.Visible then FPanel2Height := Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := FPanel2Height
  else
    Height := Panel1.Height;
end;

procedure TSubtreeBranchesFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
end;

procedure TSubtreeBranchesFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

procedure TSubtreeBranchesFrame.SetSubtreeAttrib(NodeAttrib: TNodeAttrib);
begin
  try
    FDisableEventNotifications := True;
    ColorCmbBx.ButtonColor := NodeAttrib.LineColor;
    BranchWidthCmbBx.ItemIndex := NodeAttrib.LineWidth;

    case NodeAttrib.LineStyle of
      psSolid : BranchStyleCmbBx.ItemIndex := 0;
      psDash  : BranchStyleCmbBx.ItemIndex := 1;
      psDot   : BranchStyleCmbBx.ItemIndex := 2;
    end;

    case NodeAttrib.BranchOption of
      boFullBranch: BranchOptionCmbBx.ItemIndex := 0;
      boHalfBranch: BranchOptionCmbBx.ItemIndex := 1;
      boNoBranch  : BranchOptionCmbBx.ItemIndex := 2;
      boBranchOnly: BranchOptionCmbBx.ItemIndex := 3;
    end;
  finally
    FDisableEventNotifications := False;
  end;
end;

procedure TSubtreeBranchesFrame.GetSubtreeAttrib(var NodeAttrib: TNodeAttrib);
begin
  NodeAttrib.LineColor := ColorCmbBx.ButtonColor;
  NodeAttrib.LineWidth := BranchWidthCmbBx.ItemIndex;

  case BranchStyleCmbBx.ItemIndex of
    0: NodeAttrib.LineStyle := psSolid;
    1: NodeAttrib.LineStyle := psDash;
    2: NodeAttrib.LineStyle := psDot;
  end;

  case BranchOptionCmbBx.ItemIndex of
    0: NodeAttrib.BranchOption := boFullBranch;
    1: NodeAttrib.BranchOption := boHalfBranch;
    2: NodeAttrib.BranchOption := boNoBranch;
    3: NodeAttrib.BranchOption := boBranchOnly;
  end;
end;

function TSubtreeBranchesFrame.NotificationsDisabled: Boolean;
begin
  Result := FDisableEventNotifications;
end;

function TSubtreeBranchesFrame.GetPenStyle: TPenStyle;
begin
  case BranchStyleCmbBx.ItemIndex of
    0: Result := psSolid;
    1: Result := psDash;
    2: Result := psDot;
  end;
end;

function TSubtreeBranchesFrame.GetBrushStyle: TBrushStyle;
begin
  case BranchStyleCmbBx.ItemIndex of
    0: Result := bsSolid;
    1: Result := bsClear;
    2: Result := bsBDiagonal;
    3: Result := bsFDiagonal;
    4: Result := bsCross;
    5: Result := bsDiagCross;
    6: Result := bsHorizontal;
    7: Result := bsVertical;
    else
      ShowMessage('missing brush style');
  end;
end;

function TSubtreeBranchesFrame.GetBranchStyle: TBranchOption;
begin
  case BranchOptionCmbBx.ItemIndex of
    0: Result := boFullBranch;
    1: Result := boHalfBranch;
    2: Result := boNoBranch;
    3: Result := boBranchOnly;
  end;
end;

function TSubtreeBranchesFrame.GetBranchLineColor: TColor;
begin
  Result := ColorCmbBx.ButtonColor;
end;


end.

