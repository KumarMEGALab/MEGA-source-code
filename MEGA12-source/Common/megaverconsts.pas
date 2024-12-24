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

unit MegaVerConsts;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  TSessionFileType = (sftMas, sftMsdx, sftMts);

const


  {$IFDEF CPU32}
  ARCHITECTURE = 'i386';
  {$ELSE}
    {$IFDEF CPUAARCH64}
    ARCHITECTURE = 'arm64';
    {$ELSE}
    ARCHITECTURE = 'x86_64';
    {$ENDIF}
  {$ENDIF}


  VER_SAVED_MEGA_ANALYIS          = 0;
  {$IFDEF MSWINDOWS}
  MY_PLATFORM = 'MS Windows';
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFDEF DARWIN}
    MY_PLATFORM = 'macOS';
    {$ELSE}
    MY_PLATFORM = 'Linux';
    {$ENDIF}
  {$ENDIF}
  MAJOR_VERSION = 12;
  MINOR_VERSION = 0;
  RELEASE_NUMBER = 4;
  BUILD_CODE = 12241219;
  AUTHOR_LIST = 'Koichiro Tamura, Glen Stecher, Sudhir Kumar';

  function VER_MEGA_BUILD: String;
  function VER_MEGA_MAJOR: String; //             = 'MEGA 7';
  function VER_MEGA_MINOR: String; //             = '.0.8';
  function VER_MEGA_MAJOR_CHAR: String; //        = '7';
  function VER_MEGA_MINOR_STR: String;  //       = '.0.8';
  function VER_MEGA_TEST: String;  //             = '(' + VER_MEGA_BUILD + ')';
  function VER_MEGA_WIN_CAPTION_PREFIX: String;// = 'M7';
  function VERSION_LABEL_CAPTION: String;
  function MEGA_USER_DIR: String;
  function APPLICATION_NAME: String;
  function OPERATING_SYSTEM: String;
  function USER_INTERFACE: String;
  function WEBSITE_URL: String;
  function VER_MEGA_CAPTION: String;          //= VER_MEGA_MAJOR + VER_MEGA_MINOR + VER_MEGA_TEST;
  {$IFDEF DARWIN}
  function BUILD_TIME: String;
  {$ENDIF}
  function MapSessionVersionToMegaVersionString(FileType: TSessionFileType; SessionVersion: Integer): String;
  function MapSessionVersionToMegaVersion(FileType: TSessionFileType; SessionVersion: Integer): Integer;

implementation

uses
  MegaConsts, math;

function VER_MEGA_BUILD: String;
begin
  Result := IntToStr(BUILD_CODE) {$IFDEF DARWIN} + '-' + BUILD_TIME {$ENDIF} + '-' + ARCHITECTURE;
  if IsDeveloper then
    Result += '-DEVELOPER';
  {$IFDEF DEBUG}
   Result += '-DEBUG';
  {$ENDIF}
end;

function VER_MEGA_MAJOR: String;
begin
  {$IFDEF VISUAL_BUILD}
  Result := Format('MEGA %d', [MAJOR_VERSION]);
  {$ELSE}
  Result := 'MEGA-CC ' + IntToStr(MAJOR_VERSION);
  {$ENDIF}
end;

function VER_MEGA_MINOR: String;
begin
  Result := '.' + IntToStr(MINOR_VERSION) + '.' + IntToStr(RELEASE_NUMBER);
end;

function VER_MEGA_MAJOR_CHAR: String;
begin
  Result := IntToStr(MAJOR_VERSION);
end;

function VER_MEGA_MINOR_STR: String;
begin
  Result := '.' + IntToStr(MINOR_VERSION) + '.' + IntToStr(RELEASE_NUMBER);
end;

function VER_MEGA_TEST: String;
begin
  Result := '(' + VER_MEGA_BUILD + ')';
end;

function VER_MEGA_WIN_CAPTION_PREFIX: String;
begin
  Result := Format('M%d', [MAJOR_VERSION]);
end;

function VERSION_LABEL_CAPTION :String;
begin
  Result := 'MEGA version ' + IntToStr(MAJOR_VERSION) + VER_MEGA_MINOR + LineEnding;
  {$IFDEF CPU32}
  Result := Result + 'For 32-bit ';
  {$ELSE}
  Result := Result + 'For 64-bit ';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := Result + 'Windows' + LineEnding;
  {$ELSE}
    {$IFDEF DARWIN}
      {$IFDEF CPUAARCH64}
      Result := 'ARM-based macOS' + LineEnding;
      {$ELSE}
      Result := 'x86-based macOS' + LineEnding;
      {$ENDIF}
    {$ELSE}
    Result := Result + 'Linux' + LineEnding;
    {$ENDIF}
  {$ENDIF}
  Result := Result + 'Build ' + IntToStr(BUILD_CODE) {$IFDEF DARWIN} + '-' + BUILD_TIME {$ENDIF};
end;

function MEGA_USER_DIR: String;
begin
  {$IFDEF VISUAL_BUILD}
  Result := 'MEGA' + VER_MEGA_MAJOR_CHAR + '_' + VER_MEGA_BUILD + PathDelim;
  {$ELSE}
  Result := 'MEGA' + VER_MEGA_MAJOR_CHAR + '_' + VER_MEGA_BUILD + PathDelim;
  {$ENDIF}
end;

function APPLICATION_NAME: String;
begin
  {$IFNDEF CALTEST}
    Result := 'mega';
  {$ELSE}
    Result := 'caltester';
  {$ENDIF}
end;


function OPERATING_SYSTEM: String;
begin
  {$IFDEF MSWINDOWS}
  Result := 'windows';
  {$ELSE}
    {$IFDEF DARWIN}
    Result := 'mac';
    {$ELSE}
      {$IFDEF UNIX}
      Result := 'linux';
      {$ELSE}
      Result := 'unknown';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

function USER_INTERFACE: String;
begin
  {$IFDEF VISUAL_BUILD}
  Result := 'Graphical';
  {$ELSE}
  Result := 'CommandLine';
  {$ENDIF}
end;

function WEBSITE_URL: String;
begin
  Result := 'http://www.megasoftware.net';
end;

function VER_MEGA_CAPTION: String;
begin
  Result := 'Molecular Evolutionary Genetics Analysis'
end;

{$IFDEF DARWIN}
function BUILD_TIME: String;
begin
  Result := {$I %DATE%} + '_' + {$I %TIME%};
end;
{$ENDIF}

function MapSessionVersionToMegaVersionString(FileType: TSessionFileType; SessionVersion: Integer): String;
var
  aMajVersion: Integer = -1;
begin
  Result := 'Unknown';
  aMajVersion := MapSessionVersionToMegaVersion(FileType, SessionVersion);
  if aMajVersion <> -1 then
  begin
    if aMajVersion = 10 then
      Result := 'MEGA X'
    else
      Result := Format('MEGA %d', [aMajVersion]);
  end;
end;

function MapSessionVersionToMegaVersion(FileType: TSessionFileType; SessionVersion: Integer): Integer;
begin
  Result := -1;
  case FileType of
    sftMas:
      begin
        case SessionVersion of
          110: Result := 4;
          111: Result := 5;
          112: Result := 6;
          1011: Result := 11;
          1200: Result := 12;
        end;
      end;
    sftMsdx:
      begin
        case SessionVersion of
          1: Result := 5;
          2: Result := 6;
          1013: Result := 11;
          1200: Result := 12
        end;
      end;
    sftMts:
      begin
        if InRange(SessionVersion, 500, 550) then
          Result := 4
        else if InRange(SessionVersion, 551, 600) then
          Result := 5
        else if InRange(SessionVersion, 601, 699) then
          Result := 6
        else if InRange(SessionVersion, 700, 799) then
          Result := 7
        else if InRange(SessionVersion, 1000, 1001) then
          Result := 10
        else if InRange(SessionVersion, 1002, 1199) then
          Result := 11
        else if InRange(SessionVersion, 1200, 1299) then
          Result := 12
        else
          Result := -1;
      end;
  end;
end;

end.
