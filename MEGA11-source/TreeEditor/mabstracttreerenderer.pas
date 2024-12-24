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

unit mabstracttreerenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type

  { TArcData }

  TArcData = class(TObject)
    public
      points: array[1..4] of TPoint;
      origin: TPoint;
      constructor Create;
  end;

  TArcDataArray = array of TArcData;

  { TAbstractTreeRenderer }

  TAbstractTreeRenderer = class abstract(TObject)
    private

    protected
      FCanvas: TCanvas;
      procedure SetCanvas(AValue: TCanvas);
      function ArcCoordsToBezierCoords(const aLeft: Integer; const aTop: Integer; const aRight: Integer; const aBottom: Integer; const startAngle16deg: Integer; const angleLength16deg: Integer): TArcDataArray;
      function CreateArcData(const origin: TPoint; const radius: Double; const startAngle: Double; const endAngle: Double): TArcDataArray;
      function CreateSmallArc(const origin: TPoint; const r: Double; const a1: Double; const a2: Double): TArcData;
      function SanitizeDegrees(degrees: Integer): Integer; virtual;
      procedure ConvertOrientationOfDegrees(var sAngle: Double; var eAngle: Double);
    public
      constructor Create(aCanvas: TCanvas);

      procedure MTextOut(const x: Integer; const y: Integer; const str: String; const rotation: Integer = 0); virtual; abstract;
      procedure MPolyline(const p: array of TPoint); virtual; abstract;
      procedure MPolygon(const p: array of TPoint); virtual; abstract;
      procedure MRectangle(const p: array of TPoint; const Filled: Boolean); virtual; abstract;
      procedure MPolyBezier(const p: array of TPoint; const n: Integer; Filled: Boolean=False; Continuous: Boolean=False); virtual; abstract;
      procedure MDrawImage(const x: Integer; const y: Integer; const aBitmap: TBitmap); virtual; abstract;
      procedure MDrawArc(const aLeft: Integer; const aTop: Integer; const aRight: Integer; const aBottom: Integer; const startAngle16deg: Integer; const angleLength16deg: Integer); virtual; abstract;
      procedure MDrawEllipse(const aRect: TRect); virtual; abstract;
      procedure MDrawSquare(const x: Integer; const y: Integer; const r: Integer; const a: Double; const fill: Boolean); virtual; abstract;
      property Canvas: TCanvas read FCanvas write SetCanvas;
  end;

implementation

uses
  math;

{ TArcData }

constructor TArcData.Create;
var
  i: Integer;
begin
  for i := Low(points) to High(points) do
  begin
    points[i].x := 0;
    points[i].y := 0;
  end;
  origin.x := 0;
  origin.y := 0;
end;

{ TAbstractTreeRenderer }

procedure TAbstractTreeRenderer.SetCanvas(AValue: TCanvas);
begin
  if FCanvas=AValue then Exit;
  FCanvas:=AValue;
end;

function TAbstractTreeRenderer.ArcCoordsToBezierCoords(const aLeft: Integer; const aTop: Integer; const aRight: Integer; const aBottom: Integer; const startAngle16deg: Integer; const angleLength16deg: Integer): TArcDataArray;
var
  origin: TPoint;
  radius: Double;
  angleStart: Double;
  angleEnd: Double;
begin
  radius := (aRight - aLeft)/2 + (aBottom - aTop)/2;
  radius := radius/2;
  origin.x := aLeft + Round((aRight - aLeft)/2);
  origin.y := aTop + Round((aBottom - aTop)/2);
  angleStart := startAngle16deg;
  angleEnd := angleLength16deg;
  ConvertOrientationOfDegrees(angleStart, angleEnd);
  Result := CreateArcData(origin, radius, angleStart*pi/180, angleEnd*pi/180);
end;

function TAbstractTreeRenderer.CreateArcData(const origin: TPoint; const radius: Double; const startAngle: Double; const endAngle: Double): TArcDataArray;
var
  s, e: Double;
  sgn: Integer;
  a1, a2: Double;
  totalAngle: Double;
  data: TArcData;
begin
  s := startAngle;
  e := endAngle;
  SetLength(Result, 0);
  if startAngle < endAngle then
    sgn := 1
  else
    sgn := -1;

  a1 := s;
  a2 := 1;
  totalAngle := abs(e - s);
  while (totalAngle > 0.00001) and (a2 > 0.00001) do
  begin
    a2 := a1 + sgn*Min(totalAngle, pi/4);
    SetLength(Result, Length(Result) + 1);
    data := CreateSmallArc(origin, radius, a1, a2);
    Result[Length(Result) - 1] := data;
    totalAngle := totalAngle - abs(a2 - a1);
    a1 := a2;
  end;
end;

function TAbstractTreeRenderer.CreateSmallArc(const origin: TPoint; const r: Double; const a1: Double; const a2: Double): TArcData;
var
  x1, x2, x3, x4: Double;
  y1, y2, y3, y4: Double;
  a, k, f: Double;
  ar: Double;
  cos_ar, sin_ar: Double;
begin
  Result := TArcData.Create;
  Result.origin := origin;
  a := (a2 - a1)/2.0;
  x4 := r*cos(a);
  y4 := r*sin(a);
  x1 := x4;
  y1 := -1*y4;
  k := 0.5522847498;
  f := k*tan(a);
  x2 := x1 + f*y4;
  y2 := y1 + f*x4;
  x3 := x2;
  y3 := -1*y2;
  ar := a + a1;
  cos_ar := cos(ar);
  sin_ar := sin(ar);

  Result.points[1].x := Round(r*cos(a1)) + origin.x;
  Result.points[1].y := Round(r*sin(a1)) + origin.y;
  Result.points[2].x := Round(x2*cos_ar - y2*sin_ar) + origin.x;
  Result.points[2].y := Round(x2*sin_ar + y2*cos_ar) + origin.y;
  Result.points[3].x := Round(x3*cos_ar - y3*sin_ar) + origin.x;
  Result.points[3].y := Round(x3*sin_ar + y3*cos_ar) + origin.y;
  Result.points[4].x := Round(r*cos(a2)) + origin.x;
  Result.points[4].y := Round(r*sin(a2)) + origin.y;
end;

function TAbstractTreeRenderer.SanitizeDegrees(degrees: Integer): Integer;
begin
  Result := degrees;
  if degrees = 0 then
    Exit;
  if Result > 0 then
  begin
    while Result > 3600 do
      Result := Result - 3600;
  end
  else if Result < 0 then
  begin
    while Result < -3600 do
      Result := Result + 3600;
  end;
  Result := Round(Result/10);
end;

procedure TAbstractTreeRenderer.ConvertOrientationOfDegrees(var sAngle: Double; var eAngle: Double);
var
  delta: Double;
  isClockwise: Boolean;
begin
  if eAngle < 0 then
    isClockwise := True
  else
    isClockwise := False;
  sAngle := sAngle/16; {was given as 1/16th degrees because that is what TCanvas.Arc expects}
  eAngle := eAngle/16;
  delta := abs(eAngle);
  if (sAngle >= 0) and (not isClockwise) then
  begin
    sAngle := 360 - sAngle - delta;
    eAngle := sAngle + delta;

  end
  else if sAngle >= 0 then
  begin
    sAngle := 360 - sAngle;
    eAngle := sAngle + delta;
  end
  else if eAngle >= 0 then
  begin
    sAngle := 360 - delta + abs(sAngle);
    eAngle := sAngle + delta;
  end
  else
  begin
    sAngle := abs(sAngle);
    eAngle := sAngle + delta;
  end;
end;

constructor TAbstractTreeRenderer.Create(aCanvas: TCanvas);
begin
  FCanvas := aCanvas;
end;

end.

