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

unit MVirtualScrollbox;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  LCLIntf, LCLType, Graphics, Controls, Forms, Classes, Messages, SysUtils, Math,
  Dialogs, MegaConsts;

type

  TPolyLineData =  record
    Points: TPointArray;
    NumPoints: Integer;
    color: TColor;
    lineWidth: Integer;
  end;

  TBoxPlacement = (bpCenter, bpLeftTop);

  TVirtualPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  // The TVirtScrollBox is a windowed control that can virtually scroll over its
  // scrollable area, indicated by ScrollWidth and ScrollHeight. The left and top
  // position of the visible part is indicated by ScrollLeft and ScrollTop. Set
  // them all together using SetScrollBounds() in order to avoid flicker
  TVirtualScrollBox = class(TCustomControl)
  private
    FAutoScroll: Boolean;    // If set, the control will automatically scroll
    FBorderStyle: TBorderStyle; // Border style for this scrollbox (bsNone or bsSingle)
    FBoxPlacement: TBoxPlacement; // Default placement when scrollbox is smaller than client
    FScrollLeft: integer;    // Left position of scroll window on virtual window
    FScrollTop: integer;     // Top position of scroll window on virtual window
    FScrollWidth: integer;   // Total width of scrollable area
    FScrollHeight: integer;  // Total height of scrollable area
    FScrollScale: single;    // Scale on scrolling in case Width or Height > 32767 (handled automatically)
    FTracking: boolean;      // If set (default), the window updates immediately when scrollbars are moved
    FIncrement: integer;     // Increment (in pixels) when arrows on scrollbar are clicked
    FOnUpdateScrollPosition: TNotifyEvent;
    FOnPaint: TVirtualPaintEvent;
    procedure SetAutoScroll(const Value: Boolean);
    procedure ScrollMessage(const AMessage: TWMHScroll; ACode: word; var APos: integer; ASize, AClient: integer);
    procedure WMEraseBkgnd(var m: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    function CalculateThumbPosition(const Requested, Size, Client: integer): integer;
    procedure SetBoxPlacement(const Value: TBoxPlacement);
  protected
    procedure SetBorderStyle(Value: TBorderStyle); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure RemoveScrollbars;
    procedure UpdateScrollbars;
    procedure UpdateScrollPosition; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ScrollBy(var DeltaX, DeltaY: Integer);
    // Use ClientToBox to determine the position of mouse coordinates X and Y in
    // box coordinates. If the mouse is outside the box, the function returns False.
    function ClientToBox(X, Y: integer; var BoxX, BoxY: integer): boolean;
    function BoxToClient(X, Y: Integer; var ClientX, ClientY: Integer): Boolean;
    procedure SetScrollBounds(ALeft, ATop, AWidth, AHeight: integer); virtual;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property BoxPlacement: TBoxPlacement read FBoxPlacement write SetBoxPlacement default bpCenter;
    property Increment: integer read FIncrement write FIncrement default 8;
    property ScrollLeft: integer read FScrollLeft;
    property ScrollTop: integer read FScrollTop;
    property ScrollWidth: integer read FScrollWidth;
    property ScrollHeight: integer read FScrollHeight;
    property Tracking: boolean read FTracking write FTracking default True;
    // Event OnUpdateScrollPosition is fired whenever the user has scrolled.
    property OnUpdateScrollPosition: TNotifyEvent read FOnUpdateScrollPosition write FOnUpdateScrollPosition;
    property OnPaint: TVirtualPaintEvent read FOnPaint write FOnPaint;
  end;

  { TEmfScrollbox }

  TEmfScrollbox = class(TVirtualScrollbox)
  private
    FTempPolygons: array of TPolyLineData;
    FTempPolylines: array of TPolyLineData;
    FAllowMouseDrag: boolean;
    FAllowArrowKeys: boolean;
    FBitmap: Graphics.TBitmap;
    FOldDragPos: TPoint;
    procedure CMWantSpecialKey(var Message: TMessage); message CM_WANTSPECIALKEY;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    function PixelFormatToString(AFormat: TPixelFormat): String;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
    procedure ConvertToBoxCoords(const Points: array of TPoint; var BoxCoords: Array of TPoint; NumPoints: Integer);
    procedure ConvertToClientCoords(const BoxCoords: array of TPoint; var ClientCoords: Array of TPoint; NumPoints: Integer); overload;
    procedure ConvertToClientCoords(const BoxCoords: TRect; var ClientCoords: TRect); overload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoInvalidate(Sender: TObject);
    procedure LoadFromMetafile(AFile: THandle; ARect: TRect);
    procedure LoadFromCanvas(aCanvas: TCanvas; ARect: TRect);
    procedure CopyToClipboard(aCanvas: TCanvas; ARect: TRect); virtual;
    function GetOptimalPixelFormat(ARect: TRect): TPixelFormat;
    function IsVisible(aPoint: TPoint): Boolean;
    procedure ScrollTo(NewX: Integer; NewY: Integer);
    procedure ScrollToBottom;
    property ScrollLeft;
    property ScrollTop;
    property ScrollWidth;
    property ScrollHeight;
  published
    procedure DrawPolygon(AColor: TColor; LineWidth: Integer; Points: array of TPoint; NumPoints: Integer);
    procedure DrawTempPolygon(AColor: TColor; LineWidth: Integer; Points: TPointArray; NumPoints: Integer);
    procedure DrawFixedPolygon(AColor: TColor; LineWidth: Integer; Points: array of TPoint; NumPoints: Integer);
    procedure DrawLine(AColor: TColor; LineWidth: Integer; Points: array of TPoint);
    procedure DrawTempLine(AColor: TColor; LineWidth: Integer; Points: TPointArray);
    procedure ClearTempPolygons;
    procedure ClearTempPolylines;
    // If AllowArrowKeys is true, the arrow keys will also scroll the image (default)
    property AllowArrowKeys: boolean read FAllowArrowKeys write FAllowArrowKeys default True;
    // If AllowMouseDrag is true, the user can drag the image around with the mouse
    // inside the control (default).
    property AllowMouseDrag: boolean read FAllowMouseDrag write FAllowMouseDrag default True;
    property Align;
    property Anchors;
    property AutoScroll;
    property BorderStyle;
    property BoxPlacement;
    property Color;
    property Constraints;
    //property Ctl3D;
    property DragCursor;
    property Enabled;
    //property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tracking;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUnDock;
    property OnUpdateScrollPosition;
  end;

  procedure Register;

implementation

uses
  MegaUtils, Clipbrd;

procedure Register;
begin
  RegisterComponents('BD3 Software', [TEmfScrollbox]);
end;

{ TVirtualScrollBox }

function TVirtualScrollBox.BoxToClient(X, Y: Integer; var ClientX, ClientY: Integer): Boolean;
begin
  ClientX := 0;
  ClientY := 0;
  Result := False;
  if (FScrollWidth <= 0) or (FScrollHeight <= 0) then
    exit;
  ClientX := X - FScrollLeft;
  ClientY := Y - FScrollTop;
  if (ClientX >= 0) and (ClientX < FScrollWidth) and (ClientY >= 0) and (ClientY < FScrollHeight) then
    Result := True;
end;

function TVirtualScrollBox.CalculateThumbPosition(const Requested, Size, Client: integer): integer;
var
  OverShoot: integer;
begin
  if FBoxPlacement = bpCenter then
  begin
    if Size = 0 then
      OverShoot := 0
    else
      OverShoot := Max(0, Client - Size);
  end else
    OverShoot := 0;
  Result := Max(-OverShoot div 2, Min(Requested, Size - Client));
end;

function TVirtualScrollBox.ClientToBox(X, Y: integer; var BoxX, BoxY: integer): boolean;
begin
  BoxX := 0;
  BoxY := 0;
  Result := False;
  if (FScrollWidth <= 0) or (FScrollHeight <= 0) then
    exit;
  BoxX := X + FScrollLeft;
  BoxY := Y + FScrollTop;
  if (BoxX >= 0) and (BoxX < FScrollWidth) and (BoxY >= 0) and (BoxY < FScrollHeight) then
    Result := True;
end;

constructor TVirtualScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FAutoScroll := True;
  FIncrement := 9;
  FScrollScale  := 1.0;
  FTracking := True;
  Color := clAppWorkspace;
  Width := 150;
  Height := 250;
  Align:= alClient;
  FBorderStyle := bsSingle;
end;

procedure TVirtualScrollBox.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    with WindowClass do
      style := style and not (CS_HREDRAW or CS_VREDRAW);
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;


// Override this method in descendants. Call "inherited" if you want to automatically
// clear the area that is outside of the scrollbox
procedure TVirtualScrollBox.Paint;
var
  R, B, C, Dest: TRect;

  procedure PaintRect;
  begin
    if IsRectEmpty(R) then
      exit;
    IntersectRect(Dest, R, C);
    if IsRectEmpty(Dest) then
      exit;
    Canvas.FillRect(Dest);
  end;

// main
begin
  // Onpaint event
  if assigned(FOnPaint) then
    FOnPaint(Self, Canvas);
  // Paint area around scrolled area (if any is visible)
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  C := Canvas.ClipRect;
  B := Rect(0, 0, FScrollWidth, FScrollHeight);
  OffsetRect(B, -FScrollLeft, -FScrollTop);
  R := Rect(0, 0, ClientWidth, B.Top);
  PaintRect;
  R := Rect(0, B.Top, B.Left, B.Bottom);
  PaintRect;
  R := Rect(B.Right, B.Top, ClientWidth, B.Bottom);
  PaintRect;
  R := Rect(0, B.Bottom, ClientWidth, ClientHeight);
  PaintRect;
end;

procedure TVirtualScrollBox.RemoveScrollbars;
var
  ScrollInfo: TScrollInfo;
begin
  if not HandleAllocated then
    exit;
  // Horizontal scrollbar
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
  // Vertical scrollbar
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
end;

// Call this procedure to scroll the window and update the scrollbars, all in
// one command
procedure TVirtualScrollBox.ScrollBy(var DeltaX, DeltaY: Integer);
var
  NewX, NewY: integer;
  ThumbPosX, ThumbPosY: integer;
begin
  // Calculate new position in X and Y
  NewX := CalculateThumbPosition(FScrollLeft + DeltaX, FScrollWidth, ClientWidth);
  DeltaX := NewX - FScrollLeft;
  NewY := CalculateThumbPosition(FScrollTop  + DeltaY, FScrollHeight, ClientHeight);
  DeltaY := NewY - FScrollTop;
  if (DeltaX = 0) and (DeltaY = 0) then
    exit; // no changes

  FScrollLeft := NewX;
  FScrollTop  := newY;
  UpdateScrollPosition;

  // Scroll the window
  ScrollWindow(Handle, -DeltaX, -DeltaY, nil, nil);

  // Set scrollbar positions
  ThumbPosX := round(NewX * FScrollScale);
  ThumbPosY := round(NewY * FScrollScale);
  if GetScrollPos(Handle, SB_HORZ) <> ThumbPosX then
    SetScrollPos(Handle, SB_HORZ, ThumbPosX, True);
  if GetScrollPos(Handle, SB_VERT) <> ThumbPosY then
    SetScrollPos(Handle, SB_VERT, ThumbPosY, True);
end;

procedure TVirtualScrollBox.ScrollMessage(const AMessage: TWMHScroll; ACode: word; var APos: integer; ASize, AClient: integer);
  // local
  procedure SetPosition(NewPos: single);
  var
    ANewPos: single;
    ADelta: integer;
    AIntPos: integer;
  begin
    // Calculate new position
    ANewPos := Min(Max(0, NewPos), Max(0, ASize - AClient));
    ADelta := round(ANewPos - APos);
    if ADelta = 0 then
      exit; // no changes

    APos := round(ANewPos);
    UpdateScrollPosition;

    // Scroll the window
    case ACode of
    SB_HORZ: ScrollWindow(Handle, -ADelta, 0, nil, nil);
    SB_VERT: ScrollWindow(Handle, 0, -ADelta, nil, nil);
    end;//case

    // Set scrollbar position
    AIntPos := round(NewPos * FScrollScale);
    if GetScrollPos(Handle, ACode) <> AIntPos then
      SetScrollPos(Handle, ACode, AIntPos, True);
  end;
// main
begin
  if not AutoScroll then
    exit;
  with AMessage do
  begin
    case ScrollCode of
    SB_LINEUP:        SetPosition(APos - Increment);
    SB_LINEDOWN:      SetPosition(APos + Increment);
    SB_PAGEUP:        SetPosition(APos - AClient);
    SB_PAGEDOWN:      SetPosition(APos + AClient);
    SB_THUMBPOSITION: SetPosition(Pos / FScrollScale);
    SB_THUMBTRACK:    if Tracking then SetPosition(Pos / FScrollScale);
    SB_TOP:           SetPosition(0);
    SB_BOTTOM:        SetPosition(ASize - AClient);
    SB_ENDSCROLL:     Repaint;
    end;//case
  end;
end;

procedure TVirtualScrollBox.SetAutoScroll(const Value: Boolean);
begin
  if FAutoScroll <> Value then
  begin
    FAutoScroll := Value;
    if Value then
      UpdateScrollBars
    else
    begin
      RemoveScrollbars;
      FScrollLeft := 0;
      FScrollTop  := 0;
    end;
  end;
end;

procedure TVirtualScrollBox.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd(Self);
  end;
end;

procedure TVirtualScrollBox.SetBoxPlacement(
  const Value: TBoxPlacement);
begin
  if FBoxPlacement <> Value then
  begin
    FBoxPlacement := Value;
    UpdateScrollBars;
  end;
end;

procedure TVirtualScrollBox.SetScrollBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  if (FScrollLeft <> ALeft) or (FScrollTop <> ATop) or
     (FScrollWidth <> AWidth) or (FScrollHeight <> AHeight) then
  begin
    if (FScrollLeft <> ALeft) or (FScrollTop <> ATop) then
    begin
      FScrollLeft   := ALeft;
      FScrollTop    := ATop;
      UpdateScrollPosition;
    end;
    FScrollWidth  := AWidth;
    FScrollHeight := AHeight;
    UpdateScrollbars;
  end;
end;

procedure TVirtualScrollBox.UpdateScrollbars;
var
  ScrollInfo: TScrollInfo;
  AMax: integer;
  AScrollLeft, AScrollTop: integer;
begin
  if not HandleAllocated then
    exit;
  // Adjust scale
  AMax := Max(FScrollWidth, FScrollHeight);
  if AMax > 30000 then
    FScrollScale := 30000 / AMax
  else
    FScrollScale := 1.0;

  // Check limits on Pos
  AScrollLeft := CalculateThumbPosition(FScrollLeft, FScrollWidth, ClientWidth);
  AScrollTop := CalculateThumbPosition(FScrollTop, FScrollHeight, ClientHeight);
  if (AScrollLeft <> FScrollLeft) or (AScrollTop <> FScrollTop) then
  begin
    FScrollLeft := AScrollLeft;
    FScrollTop  := AScrollTop;
    UpdateScrollPosition;
    // We need an extra invalidate here, the standard WinControl seems to
    // forget this case
    Invalidate;
  end;
  if not AutoScroll then
    exit;

  // Horizontal scrollbar
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin  := 0;
  ScrollInfo.nMax  := round(FScrollWidth * FScrollScale);
  ScrollInfo.nPage := round(ClientWidth  * FScrollScale);
  ScrollInfo.nPos  := round(FScrollLeft  * FScrollScale);
  ScrollInfo.nTrackPos := ScrollInfo.nPos;
  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);

  // Vertical scrollbar
  ScrollInfo.nMin  := 0;
  ScrollInfo.nMax  := round(FScrollHeight * FScrollScale);
  ScrollInfo.nPage := round(ClientHeight  * FScrollScale);
  ScrollInfo.nPos  := round(FScrollTop    * FScrollScale);
  ScrollInfo.nTrackPos := ScrollInfo.nPos;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
end;

// Override in descendants to update a label that displays scroll position etc
procedure TVirtualScrollBox.UpdateScrollPosition;
begin
  // Default fires event
  if assigned(FOnUpdateScrollPosition) then
    FOnUpdateScrollPosition(Self);
end;

// This message handler is called when windows is about to work on the background
// of the window, and this procedure signals not to "erase" (or fill) it, to
// avoid flicker
procedure TVirtualScrollBox.WMEraseBkgnd(var m: TWMEraseBkgnd);
begin
  // No automatic erase of background
  m.Result := LRESULT(False);
end;

procedure TVirtualScrollBox.WMHScroll(var Message: TWMHScroll);
begin
  ScrollMessage(Message, SB_HORZ, FScrollLeft, FScrollWidth, ClientWidth);
end;

// React to a resize
procedure TVirtualScrollBox.WMSize(var Message: TWMSize);
begin
  // use the info to update the scrollbars
  UpdateScrollbars;
  // and call inherited method
  inherited;
end;

procedure TVirtualScrollBox.WMVScroll(var Message: TWMVScroll);
begin
  ScrollMessage(Message, SB_VERT, FScrollTop, FScrollHeight, ClientHeight);
end;

{ TEmfScrollbox }

procedure TEmfScrollbox.ClearTempPolygons;
var
  i: Integer;
  tp: TPolyLineData;
begin
  if Length(FTempPolygons) > 0 then
  begin
    for i := Length(FTempPolygons) - 1 downto 0 do
    begin
      tp := FTempPolygons[i];
      DrawPolygon(tp.color, tp.lineWidth, tp.Points, tp.NumPoints);
    end;
    SetLength(FTempPolygons, 0);
    Invalidate;
  end;
end;

procedure TEmfScrollbox.ClearTempPolylines;
var
  i: Integer;
  tp: TPolyLineData;
begin
  if Length(FTempPolyLines) > 0 then
  begin
    for i := Length(FTempPolyLines) - 1 downto 0 do
    begin
      tp := FTempPolyLines[i];
      with FBitmap.Canvas do
      begin
        Pen.Style := psDot;
        {$IFNDEF DARWIN}Pen.Mode := pmNotXor;{$ENDIF}
        Pen.Color := tp.Color;
        Pen.Width := tp.lineWidth;
        Brush.Style := bsClear;
        Polyline(tp.Points);
      end;
    end;
    SetLength(FTempPolyLines, 0);
    Invalidate;
  end;
end;

procedure TEmfScrollbox.CMWantSpecialKey(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TEmfScrollbox.ConvertToBoxCoords(const Points: array of TPoint; var BoxCoords: array of TPoint; NumPoints: Integer);
var
  i: Integer;
begin
  if Length(BoxCoords) > 0 then
  begin
    for i := 0 to NumPoints - 1 do
      ClientToBox(Points[i].X, Points[i].Y, BoxCoords[i].X, BoxCoords[i].Y);
  end;
end;

procedure TEmfScrollbox.ConvertToClientCoords(const BoxCoords: array of TPoint; var ClientCoords: array of TPoint; NumPoints: Integer);
var
  i: Integer;
begin
  if Length(BoxCoords) > 0 then
  begin
    for i := 0 to NumPoints - 1 do
      BoxToClient(BoxCoords[i].X, BoxCoords[i].Y, ClientCoords[i].X, ClientCoords[i].Y);
  end;
end;

procedure TEmfScrollbox.ConvertToClientCoords(const BoxCoords: TRect; var ClientCoords: TRect);
begin
  BoxToClient(BoxCoords.Left, BoxCoords.Top, ClientCoords.Left, ClientCoords.Top);
  BoxToClient(BoxCoords.Right, BoxCoords.Bottom, ClientCoords.Right, ClientCoords.Bottom);
end;

constructor TEmfScrollbox.Create(AOwner: TComponent);
begin
  inherited;
//  FAutoScroll := False;
  Color := clWhite;
  FBoxPlacement := bpLeftTop;
  FBorderStyle := bsNone;
  FAllowArrowKeys := False;
  FAllowMouseDrag := False;
  FBitmap := nil;
  SetLength(FTempPolygons, 0);
  SetLength(FTempPolyLines, 0);
end;

destructor TEmfScrollbox.Destroy;
begin
  SetLength(FTempPolygons, 0);
  SetLength(FTempPolyLines, 0);
  if Assigned(FBitmap) then
    FreeAndNil(FBitmap);
  inherited;
end;

procedure TEmfScrollbox.DoInvalidate(Sender: TObject);
begin
  Self.Invalidate;
end;

procedure TEmfScrollbox.DrawFixedPolygon(AColor: TColor; LineWidth: Integer; Points: array of TPoint; NumPoints: Integer);
begin
  if (not Assigned(FBitMap)) or (not Visible) then { when loading timetree sessions, this can get called before we are ready}
    Exit;
  FBitMap.Canvas.Pen.Style := psSolid;
  FBitMap.Canvas.Pen.Mode := pmCopy;
  FBitMap.Canvas.Brush.Style := bsClear;
  FBitMap.Canvas.Pen.Color := AColor;
  FBitMap.Canvas.Pen.Width := LineWidth;
  FBitMap.Canvas.Polygon(Slice(Points, NumPoints));
  Invalidate;
end;

procedure TEmfScrollbox.DrawLine(AColor: TColor; LineWidth: Integer; Points: array of TPoint);
begin
  if (not Assigned(FBitmap)) or (not Visible) then Exit;
  with FBitmap.Canvas do
  begin
    Pen.Style := psDot;
    Pen.Mode := pmCopy;
    Polyline(Points);
  end;
end;

procedure TEmfScrollbox.DrawPolygon(AColor: TColor; LineWidth: Integer; Points: array of TPoint; NumPoints: Integer);
begin
  if not Assigned(FBitmap) then { when loading timetree sessions, this can get called before things are initialized}
    Exit;
  FBitMap.Canvas.Pen.Style := psSolid;
  {$IFNDEF DARWIN}FBitMap.Canvas.Pen.Mode := pmNotXor;{$ENDIF}
  FBitMap.Canvas.Brush.Style := bsClear;
  FBitMap.Canvas.Pen.Color := AColor;
  FBitMap.Canvas.Pen.Width := LineWidth;
  FBitMap.Canvas.Polygon(Slice(Points, NumPoints));
  Invalidate;
end;

procedure TEmfScrollbox.DrawTempLine(AColor: TColor; LineWidth: Integer; Points: TPointArray);
begin
  if (not Assigned(FBitmap)) or (not Visible) then { when loading timetree sessions, this can get called before things are initialized}
    Exit;
  SetLength(FTempPolyLines, Length(FTempPolyLines) + 1);
  FTempPolyLines[Length(FTempPolyLines) - 1].Points := Points;
  FTempPolyLines[Length(FTempPolyLines) - 1].NumPoints := Length(Points);
  FTempPolyLines[Length(FTempPolyLines) - 1].Color := AColor;
  FTempPolyLines[Length(FTempPolyLines) - 1].lineWidth := LineWidth;
  with FBitmap.Canvas do
  begin
    Pen.Style := psDot;
    {$IFNDEF DARWIN}Pen.Mode := pmNotXor;{$ENDIF}
    Pen.Color := AColor;
    Pen.Width := LineWidth;
    Brush.Style := bsClear;
    Polyline(Points);
  end;
end;

procedure TEmfScrollbox.DrawTempPolygon(AColor: TColor; LineWidth: Integer; Points: TPointArray; NumPoints: Integer);
begin
  if (not Assigned(FBitmap)) or (not Visible) then { when loading timetree sessions, this can get called before things are initialized}
    Exit;
  SetLength(FTempPolygons, Length(FTempPolygons) + 1);
  FTempPolygons[Length(FTempPolygons) - 1].Points := Points;
  FTempPolygons[Length(FTempPolygons) - 1].NumPoints := NumPoints;
  FTempPolygons[Length(FTempPolygons) - 1].Color := AColor;
  FTempPolygons[Length(FTempPolygons) - 1].lineWidth := LineWidth;
  DrawPolygon(AColor, LineWidth, Points, NumPoints);
end;

function TEmfScrollbox.GetOptimalPixelFormat(ARect: TRect): TPixelFormat;
const
  _2GB = 2147483647;
var
  MemRequired: Int64;
  TempBitmap: Graphics.TBitmap;
begin
  try
    try
      { test if we can draw a device dependent bitmap}
      TempBitmap := Graphics.TBitmap.Create;
      TempBitmap.SetSize(RectWidth(ARect), RectHeight(ARect));
      Result := pfDevice;
    except
      { it failed so try and draw a device independent bitmap}
      MemRequired := Int64(RectWidth(ARect)) * Int64(RectHeight(ARect));
      if MemRequired * 4 < _2GB then
        Result := pf32bit
      else if MemRequired * 3 < _2GB then
        Result := pf24bit
      else if MemRequired * 2 < _2GB then
        Result := pf16bit
      else if MemRequired * 1 < _2GB then
        Result := pf8bit
      else if MemRequired * 0.5 < _2GB then
        Result := pf4bit
      else if MemRequired < _2GB then
        Result := pf1bit
      else
        raise Exception.Create('The memory required to draw the image exceeds the 2GB limit set by Windows OS');
    end;
  finally
    TempBitmap.Free;
  end;
end;

function TEmfScrollbox.IsVisible(aPoint: TPoint): Boolean;
begin
  Result := (aPoint.X >= FScrollLeft) and (aPoint.X <= (FScrollLeft + Width));
  Result := Result and (aPoint.Y >= FScrollTop) and (aPoint.Y <= (FScrollTop + Height));
end;

procedure TEmfScrollbox.KeyDown(var Key: Word; Shift: TShiftState);
var
  DeltaX, DeltaY: integer;
begin
  if FAllowArrowKeys then begin
    DeltaX := 0;
    DeltaY := 0;
    case Key of
    VK_LEFT:  DeltaX := -Increment;
    VK_RIGHT: DeltaX :=  Increment;
    VK_UP:    DeltaY := -Increment;
    VK_DOWN:  DeltaY :=  Increment;
    end;//case
    if (DeltaX <> 0) or (DeltaY <> 0) then
      ScrollBy(DeltaX, DeltaY);
  end;
  inherited;
end;

procedure TEmfScrollbox.LoadFromMetafile(AFile: THandle; ARect: TRect);
var
  Temp: Graphics.TBitmap;
  ATop, ALeft: Integer;
begin
  try
    ATop := ScrollTop;
    ALeft := ScrollLeft;
    Temp := Graphics.TBitmap.Create;
    Temp.PixelFormat := GetOptimalPixelFormat(ARect);
    Temp.SetSize(ARect.Right, ARect.Bottom);
    //PlayEnhMetaFile(Temp.Canvas.Handle, AFile, ARect);
    SetScrollBounds(ALeft, ATop, ARect.Right, ARect.Bottom);

    if Assigned(FBitMap) then
      FreeAndNil(FBitMap);
    FBitMap := Temp;
    Invalidate;
  Except
    on E: EOutOfResources do
    begin
      ShowMessage('Oh no! The tree is too large to be drawn as formatted: ' + PixelFormatToString(Temp.PixelFormat));
      Temp.Free;
    end;
    on E:Exception do
      ShowMessage('Oh no! An error occurred when loading the emf file: ' + E.Message);
  end;
end;

procedure TEmfScrollbox.LoadFromCanvas(aCanvas: TCanvas; ARect: TRect);
var
  Temp: Graphics.TBitmap;
begin
  try
    Temp := Graphics.TBitmap.Create;
    Temp.PixelFormat := GetOptimalPixelFormat(ARect);
    Temp.SetSize(ARect.Right, ARect.Bottom);
    Temp.Canvas.CopyRect(ARect, aCanvas, aRect);
    if Assigned(FBitMap) then
      FreeAndNil(FBitMap);
    FBitMap := Temp;
    Invalidate;
  Except
    on E: EOutOfResources do
    begin
      ShowMessage('Oh no! The tree is too large to be drawn as formatted: ' + PixelFormatToString(Temp.PixelFormat));
      Temp.Free;
    end;
    on E:Exception do
      ShowMessage('Oh no! An error occurred when loading the drawing canvas: ' + E.Message);
  end;
end;

procedure TEmfScrollbox.CopyToClipboard(aCanvas: TCanvas; ARect: TRect);
var
  temp:  TBitmap;
begin
  try
    Temp := Graphics.TBitmap.Create;
    Temp.SetSize(ARect.Right, ARect.Bottom);
    Temp.Canvas.CopyRect(ARect, aCanvas, aRect);
    Clipboard.Assign(temp);
  finally
    if Assigned(temp) then
      temp.Free;
  end;
end;

procedure TEmfScrollbox.Paint;
var
  R, B, C: TRect;
  //debug: Boolean;
begin
  inherited;
  C := Rect(0,0,0,0);
  R := Canvas.ClipRect;
  if IsRectEmpty(R) then exit;
  OffsetRect(R, ScrollLeft, ScrollTop);

  // Boundsrect of metafile
  B := Rect(0, 0, 0, 0);
  if assigned(FBitmap) then begin
    B.Right := FBitmap.Width;
    B.Bottom := FBitmap.Height;
  end;
  // Find clipped area
  //C := Rect(0, 0, 0, 0);
  IntersectRect(C, R, B);
  if IsRectEmpty(C) then exit;

  // And the area in the control
  R := C;
  OffsetRect(R, -ScrollLeft, -ScrollTop);

  try
    Canvas.Lock;
    //PatBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight,WHITENESS);
    {debug :=} BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight, FBitmap.Canvas.Handle, R.Left, R.Top, SRCCOPY);
    //Canvas.CopyRect(R, FBitmap.Canvas, C);
  finally
    Canvas.Unlock;
  end;
end;

function TEmfScrollbox.PixelFormatToString(AFormat: TPixelFormat): String;
begin
  case AFormat of
    pfDevice: Result := 'Device';
    pf1bit:  Result := 'Single bit';
    pf4bit:  Result := '4 bit';
    pf8bit: Result := '8 bit';
    pf15bit:  Result := '15 bit';
    pf16bit:  Result := '16 bit';
    pf24bit:  Result := '24 bit';
    pf32bit: Result := '32 bit';
    pfCustom:  Result := 'Custom';
  end;
end;

procedure TEmfScrollbox.ScrollTo(NewX: Integer; NewY: Integer);
var
  //ANewX: Integer = -1;
  //ANewY: Integer = -1;
  DeltaX: Integer;
  DeltaY: Integer;
  ThumbPosX: Integer;
  ThumbPosY: Integer;
begin
  UpdateScrollPosition;
  // Calculate new position in X and Y
  //ANewX := CalculateThumbPosition(NewX, FScrollWidth, ClientWidth);
  DeltaX := NewX - FScrollLeft;
  //ANewY := CalculateThumbPosition(NewY, FScrollHeight, ClientHeight);
  DeltaY := NewY - FScrollTop;
  if (DeltaX = 0) and (DeltaY = 0) then
    exit; // no changes
  FScrollLeft := NewX;
  FScrollTop  := newY;
  // Scroll the window
  ScrollWindow(Handle, -DeltaX, -DeltaY, nil, nil);

  // Set scrollbar positions
  ThumbPosX := round(NewX * FScrollScale);
  ThumbPosY := round(NewY * FScrollScale);
  if GetScrollPos(Handle, SB_HORZ) <> ThumbPosX then
    SetScrollPos(Handle, SB_HORZ, ThumbPosX, True);
  if GetScrollPos(Handle, SB_VERT) <> ThumbPosY then
    SetScrollPos(Handle, SB_VERT, ThumbPosY, True);
  Invalidate;
end;

procedure TEmfScrollbox.ScrollToBottom;
begin
  ScrollTo(ScrollLeft, FScrollHeight - ClientHeight);
end;

procedure TEmfScrollbox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  SetFocus;
  if not FAllowMouseDrag then exit;
  FOldDragPos.X := Message.XPos;
  FOldDragPos.Y := Message.YPos;
end;

procedure TEmfScrollbox.WMLButtonUp(var Message: TWMLButtonUp);
var
  DeltaX, DeltaY: integer;
begin
  inherited;
  if not FAllowMouseDrag then exit;
  DeltaX := round(FOldDragPos.X - Message.XPos);
  DeltaY := round(FOldDragPos.Y - Message.YPos);
  ScrollBy(DeltaX, DeltaY);
end;

procedure TEmfScrollbox.WMMouseMove(var Message: TWMMouseMove);
var
  DeltaX, DeltaY: integer;
begin
  inherited;
  if not FAllowMouseDrag then exit;
  // Left mouse button down?
  if not (Message.Keys AND MK_LBUTTON <> 0) then exit;

  DeltaX := round(FOldDragPos.X - Message.XPos);
  DeltaY := round(FOldDragPos.Y - Message.YPos);
  ScrollBy(DeltaX, DeltaY);
  FOldDragPos.X := Message.XPos;
  FOldDragPos.Y := Message.YPos;
end;

end.

