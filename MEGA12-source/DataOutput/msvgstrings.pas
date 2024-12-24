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

unit msvgstrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MegaConsts;

function PointsToSvgLine(Points: array of TPoint; Attributes: array of TXmlAttribute): String;
function PointsToSvgPolygon(Points: array of TPoint; Attributes: array of TXmlAttribute): String;
function TextToSvgText(x, y: Integer; aText: String; Attributes: array of TXmlAttribute): String;
function TextToVerticalSvgText(x, y: Integer; aText: String; Attributes: array of TXmlAttribute): String;
function RectToSvgRect(aRect: TRect; Attributes: array of TXmlAttribute): String;
function RectToPolyline(aRect: TRect; LineAttributes: array of TXMLAttribute): String;
function CircleToSvgCircle(x,y,r: Integer; Attributes: array of TXmlAttribute): String;
function AddSvgTextToRect(aRect: TRect; aText: String; Attributes: array of TXMLAttribute; aFontHeight: Integer=16): String;
function AddWrappedSvgTextToRect(aRect: TRect; aText: String; Attributes: array of TXmlAttribute; aFontHeight: Integer=16): String;
function AddVerticalSvgTextToRect(aRect: TRect; aText: String; fHeight: String; Attributes: array of TXmlAttribute): String;
function WrapTextInTspan(aText: String; x: Integer; dy: Integer; CharWidth: Integer): String;
function SvgImageTag(xStr, yStr, widthStr, heightStr, uriStr: String): String; overload;
function SvgImageTag(x, y, width, height: Integer; uri: String): String; overload;
function HtmlEntities(aText: String): String;
function EscapeSqlSpecialChars(aText: String): String;
function RemoveTabChars(aText: String): String;
function CustomTextWidth(aText: String; CharWidth: Integer=8): Integer;
function CustomTextHeight(aText: String; CharHeight: Integer=16): Integer;

implementation

uses
  StringUtils;

function PointsToSvgLine(Points: array of TPoint; Attributes: array of TXmlAttribute): String;
var
  i: Integer;
begin
  Result := '<polyline points=' + DBLQ;
  for i := 0 to Length(Points) - 1 do
    Result := Result + IntToStr(Points[i].X) + ',' + IntToStr(Points[i].Y) + ' ';
  Result := Trim(Result) + DBLQ + ' ';
  if Length(Attributes) > 0 then
  for i := 0 to Length(Attributes) - 1 do
    Result := Result + Attributes[i].Name + '=' + dblq + Attributes[i].Value + dblq + ' ';
  Result := Result + '/>';
end;

function PointsToSvgPolygon(Points: array of TPoint;Attributes: array of TXmlAttribute): String;
var
  i: Integer;
begin
  Result := '<polygon points=' + DBLQ;
  for i := 0 to Length(Points) - 1 do
    Result := Result + IntToStr(Points[i].X) + ',' + IntToStr(Points[i].Y) + ' ';
  Result := Trim(Result) + DBLQ + ' ';
  if Length(Attributes) > 0 then
  for i := 0 to Length(Attributes) - 1 do
    Result := Result + Attributes[i].Name + '=' + dblq + Attributes[i].Value + dblq + ' ';
  Result := Result + '/>';
end;

function TextToSvgText(x, y: Integer; aText: String; Attributes: array of TXmlAttribute): String;
var
  i: Integer;
begin
  Result := '<text x=' + DBLQ + IntToStr(x) + DBLQ + ' ';
  Result := Result + 'y=' + DBLQ + IntToStr(y) + DBLQ + ' ';
  if Length(Attributes) > 0 then
  begin
    for i := 0 to Length(Attributes) - 1 do
      Result := Result + Attributes[i].Name + '=' + dblq + Attributes[i].Value + dblq + ' ';
  end;
  Result := Result + '>' + aText + '</text>';
end;

function TextToVerticalSvgText(x, y: Integer; aText: String;Attributes: array of TXmlAttribute): String;
var
  i: Integer;
begin
  Result := '<text x=' + DBLQ + IntToStr(x) + DBLQ + ' ';
  Result := Result + 'y=' + DBLQ + IntToStr(y) + DBLQ + ' ';
  if Length(Attributes) > 0 then
  begin
    for i := 0 to Length(Attributes) - 1 do
      Result := Result + Attributes[i].Name + '=' + dblq + Attributes[i].Value + dblq + ' ';
  end;
  Result := Result + 'transform=' + dblq + 'rotate(90 ' + IntToStr(x) + ',' + IntToStr(y) + ')' + dblq + ' ';
  Result := Result + '>' + aText + '</text>';

end;

function RectToSvgRect(aRect: TRect; Attributes: array of TXmlAttribute): String;
var
  i: Integer;
begin
  Result := '<rect x=' + dblq + IntToStr(aRect.Left + 1) + dblq + ' ';
  Result := Result + 'y=' + dblq + IntToStr(aRect.Top) + dblq + ' ';
  Result := Result + 'width=' + dblq + IntToStr(aRect.Right - aRect.Left) + dblq + ' ';
  Result := Result + 'height=' + dblq + IntToStr(aRect.Bottom - aRect.Top) + dblq + ' ';
  if Length(Attributes) > 0 then
    for i := 0 to Length(Attributes) - 1 do
      Result := Result + Attributes[i].Name + '=' + dblq + Attributes[i].Value + dblq + ' ';
  Result := Trim(Result) + ' />';
end;

function RectToPolyline(aRect: TRect; LineAttributes: array of TXMLAttribute): String;
var
  linePoints: array[0..4] of TPoint;
begin
  linePoints[0] := aRect.TopLeft;
  linePoints[1].X := aRect.Right;
  linePoints[1].Y := aRect.Top;
  linePoints[2] := aRect.BottomRight;
  linePoints[3].X := aRect.Left;
  linePoints[3].Y := aRect.Bottom;
  linePoints[4] := aRect.TopLeft;
  Result := PointsToSvgLine(linePoints, LineAttributes);
end;

function CircleToSvgCircle(x, y, r: Integer; Attributes: array of TXmlAttribute): String;
var
  i: Integer;
begin
  Result := '<circle cx=' + DBLQ + IntToStr(x) + DBLQ + ' ';
  Result := Result + 'cy=' + DBLQ + IntToStr(y) + DBLQ + ' ';
  Result := Result + 'r=' + DBLQ + IntToStr(r) + DBLQ + ' ';
  if Length(Attributes) > 0 then
    for i := 0 to Length(Attributes) - 1 do
      Result := Result + Attributes[i].Name + '=' + dblq + Attributes[i].Value + dblq + ' ';
  Result := Result + '/>';
end;

function AddSvgTextToRect(aRect: TRect; aText: String; Attributes: array of TXMLAttribute; aFontHeight: Integer): String;
var
  aWidth, aHeight: Integer;
  tWidth: Integer;
  MaxWidth: Integer;
  Temp: String;
  x, y: Integer;
begin
  Result := EmptyStr;
  if Trim(aText) = EmptyStr then
    Exit;

  aWidth := aRect.Right - aRect.Left;
  aHeight := aRect.Bottom - aRect.Top;
  if aWidth < 24 then
    Exit;
  if CustomTextWidth(aText) > aWidth then
  begin
    MaxWidth := Round(aWidth / 8);
    Temp := Mince(aText, MaxWidth);
    if Temp = '..' then
      Temp := EmptyStr;
  end
  else
    Temp := aText;
  tWidth := CustomTextWidth(Temp);
  x := aRect.Left + ((aRect.Right - aRect.Left) div 2);
  //x := aRect.Left + Round((aWidth - tWidth) * 0.5);
  y := Round(aRect.Top + (aHeight div 2) + (aFontHeight div 4));
  if (Temp[1] <> '.') and (aWidth >= tWidth) then
    Result := TextToSvgText(x, y, Temp, Attributes)
  else
    Result := EmptyStr;
end;

function AddWrappedSvgTextToRect(aRect: TRect; aText: String; Attributes: array of TXmlAttribute; aFontHeight: Integer): String;
var
  Temp: String;
  x, y: Integer;
  NumLines: Integer;
  Space: Integer;
  aHeight: Integer;
begin
  NumLines := NumOccurences(aText, '</tspan>', True);
  Result := EmptyStr;
  aHeight := aRect.Bottom - aRect.Top;
  Space := aHeight - (NumLines * aFontHeight);
  Temp := aText;
  x := aRect.Left + ((aRect.Right - aRect.Left) div 2);
  if Space > 0 then
    y := Round(aRect.Top + (Space div 2))
  else
    y := aRect.Top;
  Result := TextToSvgText(x, y, Temp, Attributes)
end;

function AddVerticalSvgTextToRect(aRect: TRect; aText: String; fHeight: String; Attributes: array of TXmlAttribute): String;
var
  aWidth, aHeight, maxHeight: Integer;
  tHeight, tWidth: Integer;
  i, x, y: Integer;
  Temp: String;
begin
  Result := EmptyStr;
  aWidth := aRect.Right - aRect.Left;
  aHeight := aRect.Bottom - aRect.Top;
  tHeight := CustomTextHeight(aText, StrToInt(fHeight));
  if tHeight > aWidth then
    Exit;
  tWidth := CustomTextWidth(aText);

  if tWidth > aHeight then
  begin
    MaxHeight := Round(aHeight / 8);
    Temp := Mince(aText, MaxHeight);
    if Temp = '..' then
      Temp := EmptyStr;
  end
  else
    Temp := aText;

  x := aRect.Left + (Round((aWidth - tHeight) * 0.5)+2);
  y := aRect.Top + ((aRect.Bottom - aRect.Top) div 2);

  Result := '<text x=' + DBLQ + IntToStr(x) + DBLQ + ' ';
  Result := Result + 'y=' + DBLQ + IntToStr(y) + DBLQ + ' ';
  Result := Result + 'transform=' + dblq + 'rotate(90 ' + IntToStr(x) + ',' + IntToStr(y) + ')' + dblq + ' ';
  if Length(Attributes) > 0 then
    for i := 0 to Length(Attributes) - 1 do
      Result := Result + Attributes[i].Name + '=' + dblq + Attributes[i].Value + dblq + ' ';
  Result := Result + '>';
  Result := Result + Temp + '</text>';
end;

function WrapTextInTspan(aText: String; x: Integer; dy: Integer; CharWidth: Integer): String;
var
  aList: TStringList;
  i: Integer;
  Temp: String;
begin
  try
    aList := SplitOnWhiteSpace(aText);
    if aList.Count > 0 then
    begin
      Result := EmptyStr;
      Temp := EmptyStr;
      i := 0;
      while i < aList.Count do
      begin
        if CustomTextWidth(Temp + ' ' + aList[i], CharWidth) <= 2 * x then
        begin
          Temp := Temp + ' ' + aList[i];
          inc(i);
        end
        else
        begin
          if Temp = EmptyStr then
          begin
            Temp := aList[i];
            inc(i);
          end;
          Result := Result + '<tspan x=' + dblq + IntToStr(x) + dblq + ' dy=' + dblq + IntToStr(dy) + dblq + '>' + Temp + '</tspan>';
          Temp := EmptyStr;
        end;
      end;
      if Temp <> EmptyStr then
        Result := Result + '<tspan x=' + dblq + IntToStr(x) + dblq + ' dy=' + dblq + IntToStr(dy) + dblq + '>' + Temp + '</tspan>';
    end
    else
      Result := aText;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function SvgImageTag(xStr, yStr, widthStr, heightStr, uriStr: String): String;
begin
  Result := '<image x=' + DBLQ + xStr + DBLQ + ' y=' + DBLQ + yStr + DBLQ + ' width=' + DBLQ + widthStr + DBLQ + ' height=' + DBLQ + heightStr + DBLQ+ ' xlink:href=' + DBLQ + uriStr + DBLQ + ' />';
end;

function SvgImageTag(x, y, width, height: Integer; uri: String): String;
begin
  Result := SvgImageTag(IntToStr(x), IntToStr(y), IntToStr(width), IntToStr(height), uri);
end;

function HtmlEntities(aText: String): String;
begin
  Result := StringReplace(aText, #34, '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
end;

function EscapeSqlSpecialChars(aText: String): String;
begin
  Result := StringReplace(aText, #34, '\' + #34, [rfReplaceAll]);
  Result := StringReplace(Result, ',', '\,', [rfReplaceAll]);
  Result := StringReplace(Result, ';', '\;', [rfReplaceAll]);
end;

function RemoveTabChars(aText: String): String;
begin
  Result := StringReplace(aText, #9, ' ', [rfReplaceAll]);
end;

function CustomTextWidth(aText: String; CharWidth: Integer=8): Integer;
begin
  Result := Length(aText) * CharWidth; { TODO 1 -oglen -cmegatt : implement customtextwidth }
end;

function CustomTextHeight(aText: String; CharHeight: Integer=16): Integer;
begin
  Result := CharHeight; { TODO 1 -oglen -cmegatt : implement CustomTextHeight }
end;

end.

