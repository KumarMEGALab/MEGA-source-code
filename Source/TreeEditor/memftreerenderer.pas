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

unit memftreerenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mabstracttreerenderer, Graphics;

type

  { TEmfTreeRenderer }

  TEmfTreeRenderer = class(TAbstractTreeRenderer)
    private
      FInitialized: Boolean;
      FFilename: String;
    protected

    public
      constructor Create(aCanvas: TCanvas);
      procedure Initialize(const aFilename: String; const aWidth: Integer; const aHeight: Integer);
      procedure Finalize;
      procedure MTextOut(const x: Integer; const y: Integer; const str: String; const rotation: Integer = 0); override;
      procedure MPolyline(const p: array of TPoint); override;
      procedure MPolygon(const p: array of TPoint); override;
      procedure MRectangle(const p: array of TPoint; const Filled: Boolean); override;
      procedure MPolyBezier(const p: array of TPoint; const n: Integer; Filled: Boolean=False; Continuous: Boolean=False); override;
      procedure MDrawImage(const x: Integer; const y: Integer; const aBitmap: TBitmap); override;
      procedure MDrawArc(const aLeft: Integer; const aTop: Integer; const aRight: Integer; const aBottom: Integer; const startAngle16deg: Integer; const angleLength16deg: Integer); override;
      procedure MDrawEllipse(const aRect: TRect); override;
      procedure MDrawSquare(const x: Integer; const y: Integer; const r: Integer; const a: Double; const fill: Boolean); override;
  end;

implementation

{ TEmfTreeRenderer }

constructor TEmfTreeRenderer.Create(aCanvas: TCanvas);
begin
  inherited Create(aCanvas);

end;

procedure TEmfTreeRenderer.Initialize(const aFilename: String; const aWidth: Integer; const aHeight: Integer);
begin
  FFilename := aFilename;
  FInitialized := True;
end;

procedure TEmfTreeRenderer.Finalize;
begin
  FInitialized := False;
end;

procedure TEmfTreeRenderer.MTextOut(const x: Integer; const y: Integer; const str: String; const rotation: Integer = 0);
begin
  FCanvas.TextOut(x, y, str);
end;

procedure TEmfTreeRenderer.MPolyline(const p: array of TPoint);
begin
  FCanvas.Polyline(p);
end;

procedure TEmfTreeRenderer.MPolygon(const p: array of TPoint);
begin
  FCanvas.Polygon(p);
end;

procedure TEmfTreeRenderer.MRectangle(const p: array of TPoint; const Filled: Boolean);
begin
  MPolygon(p);
end;

procedure TEmfTreeRenderer.MPolyBezier(const p: array of TPoint; const n: Integer; Filled: Boolean; Continuous: Boolean);
begin
  FCanvas.PolyBezier(p, n, Filled, Continuous);
end;

procedure TEmfTreeRenderer.MDrawImage(const x: Integer; const y: Integer; const aBitmap: TBitmap);
begin
  FCanvas.Draw(x, y, aBitmap);
end;

procedure TEmfTreeRenderer.MDrawArc(const aLeft: Integer;
  const aTop: Integer; const aRight: Integer; const aBottom: Integer;
  const startAngle16deg: Integer; const angleLength16deg: Integer);
begin
  FCanvas.Arc(aLeft, aTop, aRight, aBottom, startAngle16deg, angleLength16deg);
end;

procedure TEmfTreeRenderer.MDrawEllipse(const aRect: TRect);
begin
  FCanvas.Ellipse(aRect);
end;

procedure TEmfTreeRenderer.MDrawSquare(const x: Integer; const y: Integer; const r: Integer; const a: Double; const fill: Boolean);
begin
  FCanvas.Rectangle(x, y, x + r, x + r);
end;

end.

