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

unit mpdftreerenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mabstracttreerenderer, fpPdf, Graphics;

type
  TQuadrant = (qFirst, qSecond, qThird, qFourth);

  { TPdfTreeRenderer }

  TPdfTreeRenderer = class(TAbstractTreeRenderer)
    private
      FPage: TPDFPage;
      FDocument: TPDFDocument;
      FFontIndex: Integer;
      procedure SetPage(AValue: TPDFPage);
      function ConvertCoords(p: array of TPoint): TPDFCoordArray;
      function GetLineStyleIndex: Integer;
      procedure UpdateFontStyleIndex;
      function GetPdfPenStyle: TPDFPenStyle;
      function GetQuadrant(angle: Single): TQuadrant;
      function ConvertColor(c: TColor): Cardinal;
      function GetFontsDirectory: String;
      function GetEmbeddablePostscriptFontName(const fontName: String; var postscriptName: String; var fileName: String): Boolean;
    public
      constructor Create(aCanvas: TCanvas);
      procedure Initialize(const aWidth: Integer; const aHeight: Integer; const aTitle: String; const aName: String);
      procedure Finalize;
      procedure MTextOut(const x: Integer; const y: Integer; const str: String; const rotation: Integer = 0); override;
      procedure MTextOut(const x: Integer; const y: Integer; const str: String; const rotation: Integer = 0; const underline: Boolean=False; const strikethrough: Boolean=False); overload;
      procedure MPolyline(const p: array of TPoint); override;
      procedure MPolygon(const p: array of TPoint); override;
      procedure MPolyBezier(const p: array of TPoint; const n: Integer; Filled: Boolean=False; Continuous: Boolean=False); override;
      procedure MRectangle(const p: array of TPoint; const Filled: Boolean); override;
      procedure MRoundRectangle(const aRect: TRect; const Filled: Boolean; const rx, ry: Integer); override;
      procedure MDrawImage(const x: Integer; const y: Integer; const aBitmap: TBitmap); override;
      procedure MDrawArc(const aLeft: Integer; const aTop: Integer; const aRight: Integer; const aBottom: Integer; const startAngle16deg: Integer; const angleLength16deg: Integer); override;
      procedure MDrawEllipse(const aRect: TRect); override;
      procedure MDrawSquare(const x: Integer; const y: Integer; const r: Integer; const a: Double; const fill: Boolean); override;
      property Document: TPDFDocument read FDocument;
  end;

implementation

uses
  {$IFDEF DEBUG}Dialogs,{$ENDIF}
  {$IFDEF MSWINDOWS}windirs,{$ENDIF}
  math, strutils, MegaUtils, fpTTF, FileUtil;

{ TPdfTreeRenderer }

procedure TPdfTreeRenderer.SetPage(AValue: TPDFPage);
begin
  if FPage=AValue then Exit;
  FPage:=AValue;
end;

function TPdfTreeRenderer.ConvertCoords(p: array of TPoint): TPDFCoordArray;
var
  i: Integer;
begin
  SetLength(Result, Length(p));
  if Length(Result) > 0 then
    for i := 0 to Length(Result) - 1 do
    begin
      Result[i].X := p[i].x;
      Result[i].Y := p[i].y;
    end;
end;

function TPdfTreeRenderer.GetLineStyleIndex: Integer;
begin
  Result := FDocument.AddLineStyleDef(FCanvas.Pen.Width, ConvertColor(FCanvas.Pen.Color), GetPdfPenStyle);
  FPage.SetPenStyle(GetPdfPenStyle, FCanvas.Pen.Width);
  FPage.SetColor(ConvertColor(FCanvas.Pen.Color));
end;

procedure TPdfTreeRenderer.UpdateFontStyleIndex;
var
  fname: String = '';
  filename: String = '';
begin
  FFontIndex := 0;
  try
    if not FDocument.IsStandardPDFFont(FCanvas.Font.Name) then
    begin
      if GetEmbeddablePostscriptFontName(FCanvas.Font.Name, fname, fileName) then
      begin
        if FileExists(filename) then
          FFontIndex := FDocument.AddFont(filename, fname)
        else
          FFontIndex := 0;
      end
      else
        FFontIndex := 0;
    end
    else
      FFontIndex := FDocument.AddFont(FCanvas.Font.Name);
  except
    on E:Exception do
    begin
      {$IFDEF DEBUG}
      ShowMessage('Exception in UpdateFontStyleIndex' + E.Message);
      {$ENDIF}
      FFontIndex := 0;
    end;
  end;
end;

function TPdfTreeRenderer.GetPdfPenStyle: TPDFPenStyle;
begin
  case FCanvas.Pen.Style of
    psSolid: Result := ppsSolid;
    psDash: Result := ppsDash;
    psDot: Result := ppsDot;
    psDashDot: Result := ppsDashDot;
    psDashDotDot: Result := ppsDashDotDot;
    psinsideFrame: Result := ppsSolid;
    psPattern: Result := ppsDashDotDot;
    psClear: Result := ppsSolid;
  end;
end;

function TPdfTreeRenderer.GetQuadrant(angle: Single): TQuadrant;
var
  a: Single;
begin
  a := angle;
  while a > 360 do
    a := a - 360;
  if a < 90 then
    Result := qFirst
  else if a < 180 then
    Result := qSecond
  else if a < 270 then
    Result := qThird
  else
    Result := qFourth;
end;

function TPdfTreeRenderer.ConvertColor(c: TColor): Cardinal;
var
  r, g, b: Byte;
  h: String;
begin
  Result := ColorToRGB(c);
  RedGreenBlue(Result, r, g, b);
  h := IntToHex(r, 2) + IntToHex(g, 2) + IntToHex(b, 2);
  Result := Hex2Dec(h);
end;

function TPdfTreeRenderer.GetFontsDirectory: String;
begin
  {$IFDEF MSWINDOWS}
  Result := GetWindowsSpecialDir(CSIDL_WINDOWS) + 'Fonts';
  {$ELSE}
    {$IFDEF DARWIN}
    if DirectoryExists('/System/Library/Fonts') then
      Result := 'System/Library/Fonts'
    else if DirectoryExists('/Library/Fonts') then
      Result := '/Library/Fonts'
    else if DirectoryExists(GetUserDir + 'Library/Fonts') then
      Result := GetUserDir + 'Library/Fonts'
    else
      Result := EmptyStr;
    {$ELSE}
    if DirectoryExists('/usr/share/fonts/truetype') then
      Result := '/usr/share/fonts/truetype'
    else if DirectoryExists('/usr/share/fonts') then
      Result := '/usr/share/fonts'
    else if DirectoryExists(GetUserDir + '.fonts') then
      Result := GetUserDir + '.fonts'
    else
      Result := EmptyStr;
    {$ENDIF}
  {$ENDIF}
end;

function TPdfTreeRenderer.GetEmbeddablePostscriptFontName(const fontName: String; var postscriptName: String; var fileName: String): Boolean;
var
  i: Integer;
  f: TFPFontCacheItem;
begin
  Result := False;
  if gTTFontCache.Count > 0 then
    for i := 0 to gTTFontCache.Count - 1 do
    begin
      f := TFPFontCacheItem(gTTFontCache.Items[i]);
      if SameText(f.HumanFriendlyName, fontName) then
      begin
        if f.FontData.Embeddable then { we are embedding fonts in the PDF so they must be embeddable}
        begin
          postscriptName := f.PostScriptName;
          fileName := f.FileName;
          Result := True;
        end;
        Exit;
      end;
    end;
end;

constructor TPdfTreeRenderer.Create(aCanvas: TCanvas);
begin
  inherited Create(aCanvas);
  FPage := nil;
  FDocument := nil;
end;

procedure TPdfTreeRenderer.Initialize(const aWidth: Integer; const aHeight: Integer; const aTitle: String; const aName: String);
var
  paper: TPDFPaper;
  section: TPDFSection;
  lOpts: TPDFOptions;
  fontsDir: String;
  aList: TStringList;
  doRecursive: Boolean = False;
begin
  {$IFDEF UNIX}
  doRecursive := True;
  {$ENDIF}
  FDocument := TPDFDocument.Create(Nil);
  FDocument.Infos.Title := aTitle;
  FDocument.Infos.Author := 'Institute for Genomics and Evolutionary Medicine';
  FDocument.Infos.Producer := 'MEGA';
  FDocument.Infos.ApplicationName := aName;
  FDocument.Infos.CreationDate := Now;
  //FDocument.LineCapStyle := plcsProjectingSquareCap;
  lOpts := [poPageOriginAtTop];
  Include(lOpts,poCompressImages);
  Include(lOpts, poSubsetFont);
  FDocument.Options := lOpts;
  FDocument.StartDocument;
  section := FDocument.Sections.AddSection;
  FPage := FDocument.Pages.AddPage;
  FPage.PaperType := ptCustom;
  section.AddPage(FPage);
  paper := FPage.Paper;
  paper.W := aWidth;
  paper.H := aHeight;
  FPage.Paper := paper;
  FPage.UnitOfMeasure := uomPixels;
  FDocument.AddFont('Helvetica');
  fontsDir := GetFontsDirectory;
  aList := FindAllFiles(fontsDir, '*.ttf;*.otf', doRecursive);
  FDocument.FontFiles := aList;
  gTTFontCache.SearchPath.Add(fontsDir);
  gTTFontCache.BuildFontCache;
  UpdateFontStyleIndex;
end;

procedure TPdfTreeRenderer.Finalize;
begin
  if Assigned(FDocument) then
    FreeAndNil(FDocument);
end;

procedure TPdfTreeRenderer.MTextOut(const x: Integer; const y: Integer; const str: String; const rotation: Integer = 0);
begin
  MTextOut(x, y, str, rotation, False, False);
end;

procedure TPdfTreeRenderer.MTextOut(const x: Integer; const y: Integer; const str: String; const rotation: Integer = 0; const underline: Boolean=False; const strikethrough: Boolean=False);
var
  h: Integer = 0;
  degrees: Single;
begin
  UpdateFontStyleIndex;
  degrees := SanitizeDegrees(rotation)*1.0;
  FPage.SetFont(FFontIndex, abs(FCanvas.Font.Size));
  FPage.SetColor(ConvertColor(FCanvas.Font.Color), False);
  if CompareValue(rotation, 0, 0.0000001) = 0 then
  begin
    h := Round(FCanvas.TextHeight(str)/1.5);
    FPage.WriteText(x, y + h, str, degrees, underline, strikethrough);
  end
  else
  begin
    FPage.WriteText(x, y, str, degrees, underline, strikethrough);
  end;
end;

procedure TPdfTreeRenderer.MPolyline(const p: array of TPoint);
var
  coords: TPDFCoordArray;
  lstyle: Integer;
  i: Integer;
begin
  Assert(Length(p) >= 2, 'invalid call to MPolyline - not enough points');
  if Length(p) < 2 then
    Exit;
  coords := ConvertCoords(p);
  lstyle := GetLineStyleIndex;
  FPage.MoveTo(coords[0].X, coords[0].Y);
  for i := Low(coords)+1 to High(coords) do
    FPage.DrawLineStyle(coords[i-1], coords[i], lstyle);
end;

procedure TPdfTreeRenderer.MPolygon(const p: array of TPoint);
begin
  FPage.SetColor(ConvertColor(FCanvas.Pen.Color), True);
  if FCanvas.Brush.Style <> bsClear then
    FPage.SetColor(ConvertColor(FCanvas.Brush.Color), False)
  else
    FPage.SetColor(ConvertColor(clWhite), False);
  FPage.ResetPath;
  FPage.DrawPolygon(ConvertCoords(p), FCanvas.Pen.Width);
  FPage.FillStrokePath;
end;

procedure TPdfTreeRenderer.MPolyBezier(const p: array of TPoint; const n: Integer; Filled: Boolean; Continuous: Boolean);
var
  index: Integer;
begin
  GetLineStyleIndex;
  if Length(p) < 4 then
    Exit;
  FPage.MoveTo(p[0].x, p[0].y);
  FPage.CubicCurveTo(p[1].x, p[1].y, p[2].x, p[2].y, p[3].x, p[3].y, FCanvas.Pen.Width, Filled);
  index := 4;
  if Continuous then
  begin
    while ((index + 3) <= n) do
    begin
      FPage.CubicCurveTo(p[index].x, p[index].y, p[index + 1].x, p[index + 1].y, p[index + 2].x, p[index + 2].y, FCanvas.Pen.Width, Filled);
      inc(index, 3);
    end;
  end
  else
  begin
    while ((index + 4) <= n) do
    begin
      FPage.MoveTo(p[index].x, p[index].y);
      FPage.CubicCurveTo(p[index + 1].x, p[index + 1].y, p[index + 2].x, p[index + 2].y, p[index + 3].x, p[index + 3].y, FCanvas.Pen.Width, Filled);
      inc(index, 4);
    end;
  end;
end;

procedure TPdfTreeRenderer.MRectangle(const p: array of TPoint; const Filled: Boolean);
begin
  MPolygon(p);
end;

procedure TPdfTreeRenderer.MRoundRectangle(const aRect: TRect; const Filled: Boolean; const rx, ry: Integer);
var
  aWidth: Integer = -1;
  aHeight: Integer = -1;
begin
  aWidth := aRect.Right - aRect.Left;
  aHeight := aRect.Bottom - aRect.Top;
  FPage.SetColor(ConvertColor(FCanvas.Pen.Color), True);
  if FCanvas.Brush.Style <> bsClear then
    FPage.SetColor(ConvertColor(FCanvas.Brush.Color), False)
  else
    FPage.SetColor(ConvertColor(clWhite), False);
  FPage.ResetPath;
  FPage.DrawRoundedRect(aRect.Left, aRect.Top, aWidth, aHeight, rx, FCanvas.Pen.Width, Filled, True);
  if Filled then
    FPage.FillStrokePath;
end;

procedure TPdfTreeRenderer.MDrawImage(const x: Integer; const y: Integer; const aBitmap: TBitmap);
var
  index: Integer = -1;
begin
  Assert(False, 'not implemented - still need to add image to document and get image index');
  FPage.DrawImage(x, y, aBitmap.Width, aBitmap.Height, index);
end;

procedure TPdfTreeRenderer.MDrawArc(const aLeft: Integer; const aTop: Integer; const aRight: Integer; const aBottom: Integer; const startAngle16deg: Integer; const angleLength16deg: Integer);
var
  data: TArcDataArray;
  d: TArcData;
  i: Integer;
  doFill: Boolean;
begin
  doFill := ((startAngle16deg = 0) and (angleLength16deg = 360*16));
  data := ArcCoordsToBezierCoords(aLeft, aTop, aRight, aBottom, startAngle16deg, angleLength16deg);
  if Length(data) > 0 then
    for i := 0 to Length(data) - 1 do
    begin
      d := data[i];
      MPolyBezier(d.points, 4, doFill, False);
      d.Free;
    end;
  SetLength(data, 0);
end;

procedure TPdfTreeRenderer.MDrawEllipse(const aRect: TRect);
var
  w, h: Integer;
  doFill: Boolean;
begin
  w := RectWidth(aRect);
  h := RectHeight(aRect);
  doFill := (FCanvas.Brush.Style = bsSolid);
  GetLineStyleIndex;
  FPage.SetColor(ConvertColor(FCanvas.Pen.Color), True);
  FPage.SetColor(ConvertColor(FCanvas.Brush.Color), False);
  FPage.DrawEllipse(aRect.Left, aRect.Bottom, w, h, FCanvas.Pen.Width, doFill);
end;

procedure TPdfTreeRenderer.MDrawSquare(const x: Integer; const y: Integer; const r: Integer; const a: Double; const fill: Boolean);
begin
  FPage.SetColor(ConvertColor(FCanvas.Pen.Color), True);
  FPage.SetColor(ConvertColor(FCanvas.Brush.Color), False);
  FPage.DrawRect(x, y + 2*r, 2*r, 2*r, Canvas.Pen.Width, fill, True, a*180/pi);
end;

end.

