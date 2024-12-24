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

unit mmega_std_out;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure DoTextOut(x, y: Word; const s: String; aColor: Word = 15); overload;
procedure DoTextOut(x, y: Word; const key: String; const value: String); overload;
procedure ClearMegaStdOut;
function HasColorCapability: Boolean;
procedure GetColorForOutputType(const aType: String; var nameColor: Word; var valueColor: Word);
function NextProgressLine: Word;

const
  STATUS_MARGIN = 4;
  CURRENT_PROGRESS = 'Current Progress';

var
  MegaStdOut: TStringList;
  MegaProgressStrings: TStringList;
  ProgressNameColumnWidth: Integer;
  ProgressScreenWidth: Integer;
  ProgressScreenHeight: Integer;
  ProgressCurrentPosition: Integer;
  StaticProgressPosition: Integer;
  NumLinesWritten: Integer;
  LastKeyWritten: String;
  LastValueWritten: String;

implementation

uses
   MD_MegaMain,Video, StringUtils, MegaConsts, math, MegaVerConsts, MegaAnalysisPrefStrings;

procedure DoTextOut(x, y: Word; const s: String; aColor: Word = 15);
var
  newY: Integer;
  currentChar: Word = 0;
  startChar: Integer = 0;
  numChars: Word = 0;
  isLastLine: Boolean = False;
begin
  if not UseFormattedConsoleOutput then
  begin
    if LastValueWritten <> s then
    begin
      LastValueWritten := s;
      WriteLn(s);
    end;
    LastKeyWritten := EmptyStr;
    Exit;
  end;

  startChar := ((X-1) + (Y-1)*ScreenWidth);
  numChars := Length(s);
  if startChar + numChars > ScreenWidth*ScreenHeight then
  begin
    isLastLine := True;
    newY := Y - 2;
    startChar := ((X - 1) + newY*ScreenWidth);
    while (startChar + numChars > ScreenWidth*ScreenHeight) and (newY > 0) do
    begin
      dec(newY);
      startChar := ((X - 1) + newY*ScreenWidth);
    end;
  end
  else
    newY := Y - 1;

  For currentChar := 1 to numChars do
    VideoBuf^[startChar + currentChar - 1] := Ord(s[currentChar]) + (aColor shl 8);
  ProgressCurrentPosition := Max(ProgressCurrentPosition, startChar + numChars);
  SetCursorPos(numChars, ProgressCurrentPosition div screenWidth);
  inc(currentChar);
  if isLastLine and ((startChar + currentChar - 1) < screenWidth*screenHeight) then
  begin
    while (startChar + currentChar - 1) < screenWidth*screenHeight do
    begin
      VideoBuf^[startChar + currentChar - 1] := Ord(' ') + (aColor shl 8);
      inc(currentChar);
    end;
  end;
  NumLinesWritten := max(NumLinesWritten, Y);
end;

procedure DoTextOut(x, y: Word; const key: String; const value: String);
const
  ERASER = '                                      ';
var
  keyColor: Word = 15;
  valueColor: Word  = 15;
  outString: String = '';
  i: Integer;
begin
  if value = NotApplicableStr then
    Exit;
  if not UseFormattedConsoleOutput then
  begin
    if LastKeyWritten = key then
    begin
      if LastValueWritten <> value then
      begin
        outString := value + ERASER;
        if Length(outString) < (Length(LastValueWritten) + 2) then
        begin
          SetLength(outString, Length(LastValueWritten) + 2);
          for i := (Length(outString) + 1) to Length(LastValueWritten) do
            outString[i] := ' ';
        end;
        LastValueWritten := value;
        Write(#13 + Format('%-' + IntToStr(ProgressNameColumnWidth) + 's: %s', [key, outString]));
      end;
    end
    else
    begin
      LastKeyWritten := key;
      WriteLn();
      Write(Format('%-' + IntToStr(ProgressNameColumnWidth) + 's: %s', [key, value]));
    end;
    Exit;
  end;
  if HasColorCapability then
    GetColorForOutputType(key, keyColor, valueColor);
  DoTextOut(x, y, key, keyColor);
  DoTextout(x + ProgressNameColumnWidth, y, value, valueColor);
end;

procedure ClearMegaStdOut;
var
  temp: String = '';
begin
  MegaStdOut.Clear;
  if not UseFormattedConsoleOutput then
    Exit;
  temp := VER_MEGA_MAJOR + VER_MEGA_MINOR + ' Molecular Evolutionary Genetics Analysis';
  ProgressCurrentPosition := Length(temp);
  MegaStdOut.Add(temp);
  temp := 'Build#: ' + VER_MEGA_BUILD;
  ProgressCurrentPosition += Length(temp);
  NumLinesWritten := 2;
  if UseFormattedConsoleOutput then
    StaticProgressPosition := ScreenWidth*2;
  MegaStdOut.Add(temp);
end;

function HasColorCapability: Boolean;
var
  capabilities: Word;
begin
  capabilities := GetCapabilities;
  Result := (capabilities and cpColor) = cpColor;
end;

procedure GetColorForOutputType(const aType: String; var nameColor: Word; var valueColor: Word);
begin
  if (MegaProgressStrings.IndexOf(aType) >= 0) or (Contains(aType, '% BCL')) then
  begin
    nameColor := LightGreen;
    valueColor := LightGreen;
  end
  else
  begin
    nameColor := LightGray;
    valueColor := LightGray;
  end;
end;

function NextProgressLine: Word;
begin
  Result := NumLinesWritten + 1;
end;

initialization
  MegaStdOut := TStringList.Create;
  MegaProgressStrings := TStringList.Create;
  MegaProgressStrings.Add('Avg BCL');
  MegaProgressStrings.Add(CURRENT_PROGRESS);
  MegaProgressStrings.Add('Current Step');
  MegaProgressStrings.Add(opsBootstrapSubSamples);
  MegaProgressStrings.Add(opsBootstrapRepsPerSample);
  MegaProgressStrings.Add(opsBoostrapSitesPerSample);
  MegaProgressStrings.Add('Step');
  MegaProgressStrings.Add('Status');
  MegaProgressStrings.Add('status');
  MegaProgressStrings.Add('Start time');

finalization
  if Assigned(MegaStdOut) then
    FreeAndNil(MegaStdOut);
  if Assigned(MegaProgressStrings) then
    MegaProgressStrings.Free;

end.

