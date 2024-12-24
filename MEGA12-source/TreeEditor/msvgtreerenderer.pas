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

unit msvgtreerenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mabstracttreerenderer, Graphics;

const
  DBLQ = '"';

type

  { TSvgTreeRenderer }

  TSvgTreeRenderer = class(TAbstractTreeRenderer)
    private
      FTreeSvgFile: TextFile;
      FFilename: String;
      FInitialized: Boolean;
      function ColorStr(aColor: TColor): String;
      function LineStyleToStrokeDashArrayString(s: TPenStyle): String;
      procedure CubicBezier(const p: array of TPoint; const s: Integer=0; const Filled: Boolean=False);
      procedure OpenSVG(aName: String; aWidth, aHeight: Integer; y: Integer=0; id: String='');
      procedure DrawBackground(const aWidth: Integer; const aHeight: Integer);
      procedure CloseSVG;

    public
      constructor Create(aCanvas: TCanvas);
      procedure Initialize(const aFilename: String; const aWidth: Integer; const aHeight: Integer);
      procedure Finalize;
      procedure MTextOut(const x: Integer; const y: Integer; const str: String; const rotation: Integer = 0); override;
      procedure MPolyline(const p: array of TPoint); override;
      procedure MPolygon(const p: array of TPoint); override;
      procedure MPolyBezier(const p: array of TPoint; const n: Integer; Filled: Boolean=False; Continuous: Boolean=False); override;
      procedure MRectangle(const p: array of TPoint; const Filled: Boolean); override;
      procedure MRoundRectangle(const aRect: TRect; const Filled: Boolean; const rx, ry: Integer); override;
      procedure MDrawImage(const x: Integer; const y: Integer; const aBitmap: TBitmap); override;
      procedure MDrawArc(const aLeft: Integer; const aTop: Integer; const aRight: Integer; const aBottom: Integer; const startAngle16deg: Integer; const angleLength16deg: Integer); override;
      procedure MDrawEllipse(const aRect: TRect); override;
      procedure MDrawSquare(const x: Integer; const y: Integer; const r: Integer; const a: Double; const fill: Boolean); override;
      property Filename: String read FFilename;
  end;

implementation

uses
  MegaUtils, msvgstrings;

{ TSvgTreeRenderer }

function TSvgTreeRenderer.ColorStr(aColor: TColor): String;
begin
  Result := SColorToHtmlColor(aColor);
end;

function TSvgTreeRenderer.LineStyleToStrokeDashArrayString(s: TPenStyle): String;
begin
  Result := EmptyStr;
  case s of
    psSolid, psClear, psInsideframe, psPattern: Result := EmptyStr;
    psDash: Result := 'stroke-dasharray="4"';
    psDot: Result := 'stroke-dasharray="2"';
    psDashDot: Result := 'stroke-dasharray="4 2"';
    psDashDotDot: Result := 'stroke-dasharray="4 2 2"';
  end;
end;

procedure TSvgTreeRenderer.CubicBezier(const p: array of TPoint; const s: Integer; const Filled: Boolean);
var
  aFill: String='none';
  temp: String;
begin
  if Length(p) < (s + 3) then
    Exit;
  if Filled then
    aFill := ColorStr(FCanvas.Brush.Color);
  temp := Format('<path fill=%s%s%s ', [DBLQ, aFill, DBLQ]);
  temp := temp + Format('stroke=%s%s%s ', [DBLQ, ColorStr(FCanvas.Pen.Color) ,DBLQ]);
  temp := temp + Format('stroke-width=%s%d%s ', [DBLQ, Canvas.Pen.Width, DBLQ]);
  temp := temp + Format('d=%sM%d,%d C%d,%d %d,%d %d,%d%s />', [DBLQ, p[s].X, p[s].Y, p[s+1].X, p[s+1].Y, p[s+2].X, p[s+2].Y, p[s+3].X, p[s+3].Y, DBLQ]);
  WriteLn(FTreeSvgFile, temp);
end;

procedure TSvgTreeRenderer.OpenSVG(aName: String; aWidth, aHeight: Integer; y: Integer; id: String);
var
  temp: String;
begin
  temp := '<svg xmlns=' + DBLQ + 'http://www.w3.org/2000/svg' + DBLQ + ' xmlns:xlink=' + DBLQ + 'http://www.w3.org/1999/xlink' + DBLQ + ' version=' + DBLQ + '1.1' + DBLQ + ' ';
  temp := temp + 'width=' + DBLQ + IntToStr(aWidth) + DBLQ + ' height=' + DBLQ + IntToStr(aHeight) + DBLQ + ' ';
  temp := temp + 'name=' + dblq + aName + dblq + ' ';
  temp := temp + 'y=' + dblq + IntToStr(y) + dblq + ' ';
  if id <> EmptyStr then
    temp := temp + 'id=' + dblq + id + dblq + ' ';
  temp := temp + 'viewBox=' + DBLQ + '0 0 ' + IntToStr(aWidth) + ' ' + IntToStr(aHeight) + DBLQ + '>';
  WriteLn(FTreeSvgFile, temp);
  DrawBackground(aWidth, aHeight);
end;

procedure TSvgTreeRenderer.DrawBackground(const aWidth: Integer; const aHeight: Integer);
var
  j: Integer;
  Points: array[0..4] of TPoint;
  Temp: String;
begin
  Points[0].X := aWidth;
  Points[0].Y := aHeight;
  Points[1].X := 0;
  Points[1].Y := aHeight;
  Points[2].X := 0;
  Points[2].Y := 0;
  Points[3].X := Points[0].X;
  Points[3].Y := 0;
  Points[4].X := aWidth;
  Points[4].Y := aHeight;

  Temp := '<polyline points=' + DBLQ;
  for j := 0 to 4 do
    Temp := Temp + IntToStr(Points[j].X) + ',' + IntToStr(Points[j].Y) + ' ';
  Temp := Trim(Temp) + DBLQ + ' ';
  Temp := Temp + 'fill=' + DBLQ + 'white' + DBLQ + ' />';
  WriteLn(FTreeSvgFile, Temp);
end;

procedure TSvgTreeRenderer.CloseSVG;
begin
  WriteLn(FTreeSvgFile, '</svg>');
end;

constructor TSvgTreeRenderer.Create(aCanvas: TCanvas);
begin
  inherited Create(aCanvas);
  FFilename := EmptyStr;
  FInitialized := False;
end;

procedure TSvgTreeRenderer.Initialize(const aFilename: String; const aWidth: Integer; const aHeight: Integer);
begin
  FFilename := aFilename;
  AssignFile(FTreeSvgFile, FFilename);
  Rewrite(FTreeSvgFile);
  FInitialized := True;
  OpenSVG(ExtractFileName(aFilename), aWidth, aHeight);
end;

procedure TSvgTreeRenderer.Finalize;
begin
  CloseSVG;
  CloseFile(FTreeSvgFile);
  FInitialized := False;
end;

procedure TSvgTreeRenderer.MTextOut(const x: Integer; const y: Integer; const str: String; const rotation: Integer = 0);
var
  Temp: String;
  degrees: Integer;
  dx, dy: Integer;
begin
  if not FInitialized then
    Exit;
  if rotation = 0 then
  begin
    dx := Abs(Canvas.Font.Height) div 4;
    dy := Abs(Canvas.Font.Height);
  end
  else
  begin
    dx := 0;
    dy := 0;
  end;
  Temp := Format('<text x="%d" ', [x + dx]);
  Temp := Temp + Format('y="%d" ',[ y + dy]);
  if rotation <> 0 then
  begin
    degrees := 360 - SanitizeDegrees(rotation);
    Temp := Temp + Format('transform="rotate(%d %d,%d)" ', [degrees, x + dx, y + dy]);
  end;
  Temp := Temp + Format('fill="%s" ', [ColorStr(FCanvas.Font.Color)]);
  Temp := Temp + Format('font-size="%d" ', [abs(FCanvas.Font.Height)]);
  Temp := Temp + Format('font-family="%s" >', [Canvas.Font.Name]);
  Temp := Temp + HtmlEntities(String(str)) + '</text>';
  WriteLn(FTreeSvgFile, Temp);
end;

procedure TSvgTreeRenderer.MPolyline(const p: array of TPoint);
var
  Temp: String;
  i: Integer;
  numPoints: Integer;
  strokeArrayStr: String;
begin
  if not FInitialized then
    Exit;
  numPoints := Length(p);
  Assert(NumPoints >= 2);
  strokeArrayStr := LineStyleToStrokeDashArrayString(FCanvas.Pen.Style);
  if NumPoints = 2 then
  begin
    Temp := '<line x1=' + DBLQ + IntToStr(p[0].X) + DBLQ + ' ';
    Temp := Temp + 'y1=' + DBLQ + IntToStr(p[0].Y) + DBLQ + ' ';
    Temp := Temp + 'x2=' + DBLQ + IntToStr(p[1].X) + DBLQ + ' ';
    Temp := Temp + 'y2=' + DBLQ + IntToStr(p[1].Y) + DBLQ + ' ';
    if strokeArrayStr <> EmptyStr then
      Temp := Temp + strokeArrayStr + ' ';
    Temp := Temp + 'stroke=' + DBLQ + ColorStr(Canvas.Pen.Color) + DBLQ + ' stroke-width=' + DBLQ + IntToStr(FCanvas.Pen.Width) + DBLQ + ' stroke-linecap=' + DBLQ + 'square' + DBLQ + '/>';
  end
  else
  begin
    Temp := '<polyline points=' + DBLQ;
    for i := 0 to NumPoints - 1 do
      Temp := Temp + IntToStr(p[i].X) + ',' + IntToStr(p[i].Y) + ' ';
    Temp := Trim(Temp) + DBLQ + ' ';
    Temp := Temp + 'fill="none" ';
    if strokeArrayStr <> EmptyStr then
      Temp := Temp + strokeArrayStr + ' ';
    Temp := Temp + 'stroke=' + DBLQ + ColorStr(Canvas.Pen.Color) + DBLQ + ' stroke-width=' + DBLQ + IntToStr(FCanvas.Pen.Width) + DBLQ + ' stroke-linecap=' + DBLQ + 'square' + DBLQ + '/>';
  end;
  WriteLn(FTreeSvgFile, Temp);
end;

procedure TSvgTreeRenderer.MPolygon(const p: array of TPoint);
var
  Temp: String;
  i: Integer;
  numPoints: Integer;
begin
  if not FInitialized then
    Exit;
  numPoints := Length(p);
  Assert(NumPoints > 2);
  Temp := '<polygon points=' + DBLQ;
  for i := 0 to NumPoints - 1 do
    Temp := Temp + IntToStr(p[i].X) + ',' + IntToStr(p[i].Y) + ' ';
  Temp := Trim(Temp) + DBLQ + ' ';
  if FCanvas.Brush.Style = bsClear then
    Temp := Temp + 'fill="white" '
  else
    Temp := Temp + 'fill=' + DBLQ + ColorStr(FCanvas.Brush.Color) + DBLQ + ' ';
  Temp := Temp + 'stroke=' + DBLQ + ColorStr(FCanvas.Pen.Color) + DBLQ + ' stroke-width=' + DBLQ + IntToStr(FCanvas.Pen.Width) + DBLQ + ' stroke-linecap=' + DBLQ + 'square' + DBLQ + '/>';
  WriteLn(FTreeSvgFile, Temp);
end;

procedure TSvgTreeRenderer.MPolyBezier(const p: array of TPoint; const n: Integer; Filled: Boolean; Continuous: Boolean);
var
  index: Integer;
begin
  if not FInitialized then
    Exit;
  if n < 4 then
    Exit;
  CubicBezier(p, 0, Filled);
  if Continuous then
  begin
    index := 3;
    while ((index + 3) <= n) do
    begin
      CubicBezier(p, index, Filled);
      inc(index, 3);
    end;
  end
  else
  begin
    index := 4;
    while ((index + 4) <= n) do
    begin
      CubicBezier(p, index, Filled);
      inc(index, 4);
    end;
  end;
end;

procedure TSvgTreeRenderer.MRectangle(const p: array of TPoint; const Filled: Boolean);
begin
  MPolygon(p);
end;

procedure TSvgTreeRenderer.MRoundRectangle(const aRect: TRect; const Filled: Boolean; const rx, ry: Integer);
var
  Temp: String;
  aWidth: Integer = -1;
  aHeight: Integer = -1;
begin
  if not FInitialized then
    Exit;
  aWidth := aRect.Right - aRect.Left;
  aHeight := aRect.Bottom - aRect.Top;
  Temp := Format('<rect x="%d" y="%d" rx="%d" ry="%d" width="%d" height="%d"', [aRect.Left, aRect.Top, rx, ry, aWidth, aHeight]);
  if Filled then
    Temp += Format('fill="%s" ', [ColorStr(FCanvas.Brush.Color)])
  else
    Temp += 'fill="white" ';
  Temp += Format('stroke="%s" stroke-width="%d" stroke-linecap="square" />', [ColorStr(FCanvas.Pen.Color), FCanvas.Pen.Width]);
  WriteLn(FTreeSvgFile, Temp);
end;

procedure TSvgTreeRenderer.MDrawImage(const x: Integer; const y: Integer; const aBitmap: TBitmap);
begin
  if not FInitialized then
    Exit;
  Assert(False, 'not implemented - still need to add images to svg exports');
end;

procedure TSvgTreeRenderer.MDrawArc(const aLeft: Integer; const aTop: Integer; const aRight: Integer; const aBottom: Integer; const startAngle16deg: Integer; const angleLength16deg: Integer);
var
  data: TArcDataArray;
  d: TArcData;
  i: Integer;
begin
  if not FInitialized then
    Exit;
  data := ArcCoordsToBezierCoords(aLeft, aTop, aRight, aBottom, startAngle16deg, angleLength16deg);
  if Length(data) > 0 then
    for i := 0 to Length(data) - 1 do
    begin
      d := data[i];
      MPolyBezier(d.points, 4, False, False);
      d.Free;
    end;
  SetLength(data, 0);
end;

procedure TSvgTreeRenderer.MDrawEllipse(const aRect: TRect);
var
  temp: String;
  cx, cy, rx, ry: Integer;
  fill: String;
  lineColor: String;
begin
  if not FInitialized then
    Exit;
  lineColor := ColorStr(FCanvas.Pen.Color);
  if FCanvas.Brush.Style = bsClear then
    fill := 'white'
  else
    fill := ColorStr(FCanvas.Brush.Color);
  rx := Round((aRect.Right - aRect.Left)/2);
  ry := Round((aRect.Bottom - aRect.Top)/2);
  cx := aRect.Left + rx;
  cy := aRect.Top + ry;
  temp := Format('<ellipse cx="%d" cy="%d" rx="%d" ry="%d" fill="%s" stroke="%s" />', [cx, cy, rx, ry, fill, lineColor]);
  WriteLn(FTreeSvgFile, temp);
end;

procedure TSvgTreeRenderer.MDrawSquare(const x: Integer; const y: Integer; const r: Integer; const a: Double; const fill: Boolean);
var
  temp: String;
begin
  if not FInitialized then
    Exit;
  temp := Format('<rect width="%d" height="%d" x="%d" y="%d"', [2*r, 2*r, x, y]);
  if fill then
    temp := temp + Format(' fill="%s" />', [ColorStr(FCanvas.Brush.Color)])
  else
    temp := temp + ' file="none" />';
  WriteLn(FTreeSvgFile, temp);
end;

end.

