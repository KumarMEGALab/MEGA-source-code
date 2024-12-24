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

unit subtree_compress_frame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, frame_utils, Types,
  Graphics, MTreeBox;

type

  { TSubtreeCompressFrame }

  TSubtreeCompressFrame = class(TFrame, ITreeToolbarFrame, IDisablesEventNotifications)
    CompressSubtreeCBx: TCheckBox;
    FillStyleCmbBx: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    PixelsPerGroupSEdit: TSpinEdit;
    procedure FillStyleCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    FDisableEventNotifications: Boolean;
    FOptionsChangedNotify: TNotifyEvent;
    FPanel2Height: Integer;
    function GetPixelsPerGroupMember: Integer;
    procedure SetOptionsChangedNotify(AValue: TNotifyEvent);
    procedure SetPixelsPerGroupMember(AValue: Integer);
  public
    function GetHeaderPanel: TPanel;
    function GetBodyPanel: TPanel;
    function GetExpandCollapseImage: TImage;
    procedure ToggleCollapseBodyPanel;
    procedure SetParentControl(aParent: TWinControl);
    procedure SetAlignVal(aAlign: TAlign);

    function GetCompressSubtree: Boolean;
    function GetVerticalUnit: Integer;
    function GetFillPattern: TBrushStyle;

    procedure SetSubtreeAttrib(NodeAttrib: TNodeAttrib);
    procedure GetSubtreeAttrib(var NodeAttrib: TNodeAttrib);
    function NotificationsDisabled: Boolean;

    property OptionsChangedNotify: TNotifyEvent read FOptionsChangedNotify write SetOptionsChangedNotify;
    property PixelsPerGroupMember: Integer read GetPixelsPerGroupMember write SetPixelsPerGroupMember;
  end;

implementation

uses
  Dialogs;

{$R *.lfm}

{ TSubtreeCompressFrame }

procedure TSubtreeCompressFrame.FillStyleCmbBxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var point : array[0..3] of TPoint;
begin
  with FillStyleCmbBx do begin
    Canvas.FillRect(arect);
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := clBlack;
    point[0].x := aRect.left+2;
    point[1].x := aRect.right-2;
    point[2].x := aRect.right-2;
    point[3].x := aRect.left+2;
    point[0].y := (aRect.top+2);
    point[1].y := (aRect.top+2);
    point[2].y := (aRect.bottom-2);
    point[3].y := (aRect.bottom-2);
    case Index of
      0: Canvas.Brush.Style := bsSolid;
      1: Canvas.Brush.Style := bsClear;
      2: Canvas.Brush.Style := bsBDiagonal;
      3: Canvas.Brush.Style := bsFDiagonal;
      4: Canvas.Brush.Style := bsCross;
      5: Canvas.Brush.Style := bsDiagCross;
      6: Canvas.Brush.Style := bsHorizontal;
      7: Canvas.Brush.Style := bsVertical;
    end;
    Canvas.Polygon(point);
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(aRect.left+90, aRect.top+2, Items[Index]);
  end;
end;

procedure TSubtreeCompressFrame.SetOptionsChangedNotify(AValue: TNotifyEvent);
begin
  if FOptionsChangedNotify = AValue then Exit;
  FOptionsChangedNotify := AValue;
  CompressSubtreeCBx.OnChange := FOptionsChangedNotify;
  PixelsPerGroupSEdit.OnChange := FOptionsChangedNotify;
  FillStyleCmbBx.OnChange := FOptionsChangedNotify;
end;

function TSubtreeCompressFrame.GetPixelsPerGroupMember: Integer;
begin
  Result := PixelsPerGroupSEdit.Value;
end;

procedure TSubtreeCompressFrame.SetPixelsPerGroupMember(AValue: Integer);
begin
  try
    FDisableEventNotifications := True;
    PixelsPerGroupSEdit.Value := AValue;
  finally
    FDisableEventNotifications := False;
  end;
end;

function TSubtreeCompressFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TSubtreeCompressFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TSubtreeCompressFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TSubtreeCompressFrame.ToggleCollapseBodyPanel;
begin
  if Panel2.Visible then FPanel2Height := Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := FPanel2Height
  else
    Height := Panel1.Height;
end;

procedure TSubtreeCompressFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
end;

procedure TSubtreeCompressFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

function TSubtreeCompressFrame.GetCompressSubtree: Boolean;
begin
  Result := CompressSubtreeCBx.Checked;
end;

function TSubtreeCompressFrame.GetVerticalUnit: Integer;
begin
  Result := PixelsPerGroupSEdit.Value;
end;

function TSubtreeCompressFrame.GetFillPattern: TBrushStyle;
begin
  case FillStyleCmbBx.ItemIndex of
    0: Result := bsSolid;
    1: Result := bsClear;
    2: Result := bsBDiagonal;
    3: Result := bsFDiagonal;
    4: Result := bsCross;
    5: Result := bsDiagCross;
    6: Result := bsHorizontal;
    7: Result := bsVertical;
    else
      ShowMessage('missing fill pattern');
  end;
end;

procedure TSubtreeCompressFrame.SetSubtreeAttrib(NodeAttrib: TNodeAttrib);
begin
  if FDisableEventNotifications then Exit;
  try
    FDisableEventNotifications := True;
    CompressSubtreeCBx.Checked := NodeAttrib.ManualCompressed;
    case NodeAttrib.FillStyle of
      bsSolid: FillStyleCmbBx.ItemIndex := 0;
      bsClear: FillStyleCmbBx.ItemIndex := 1;
      bsHorizontal: FillStyleCmbBx.ItemIndex := 6;
      bsVertical: FillStyleCmbBx.ItemIndex := 7;
      bsFDiagonal: FillStyleCmbBx.ItemIndex := 3;
      bsBDiagonal: FillStyleCmbBx.ItemIndex :=2;
      bsCross: FillStyleCmbBx.ItemIndex := 4;
      bsDiagCross: FillStyleCmbBx.ItemIndex := 5;
      else
        ShowMessage('missing fill pattern handler');
    end;
  finally
    FDisableEventNotifications := False;
  end;
end;

procedure TSubtreeCompressFrame.GetSubtreeAttrib(var NodeAttrib: TNodeAttrib);
begin

  //NodeAttrib.LineColor := ColorCmbBx.ButtonColor;
  case FillStyleCmbBx.ItemIndex of
    0: NodeAttrib.FillStyle := bsSolid;
    1: NodeAttrib.FillStyle := bsClear;
    2: NodeAttrib.FillStyle := bsBDiagonal;
    3: NodeAttrib.FillStyle := bsFDiagonal;
    4: NodeAttrib.FillStyle := bsCross;
    5: NodeAttrib.FillStyle := bsDiagCross;
    6: NodeAttrib.FillStyle := bsHorizontal;
    7: NodeAttrib.FillStyle := bsVertical;
  end;

  NodeAttrib.ManualCompressed   := CompressSubtreeCBx.Checked;
end;

function TSubtreeCompressFrame.NotificationsDisabled: Boolean;
begin
  Result := FDisableEventNotifications;
end;

end.

