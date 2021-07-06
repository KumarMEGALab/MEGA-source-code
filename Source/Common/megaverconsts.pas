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
  ARCHITECTURE = 'x86_64';
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

  MAJOR_VERSION = 11;
  MINOR_VERSION = 0;
  RELEASE_NUMBER = 7;
  BUILD_CODE = 11210625;
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

  function MapSessionVersionToMegaVersion(FileType: TSessionFileType; SessionVersion: Integer): String;

implementation

  uses
    MegaConsts;

  function VER_MEGA_BUILD: String;
  begin
    Result := IntToStr(BUILD_CODE) + '-' + ARCHITECTURE;
    if IsDeveloper then
      Result += '-DEVELOPER';
  end;

  function VER_MEGA_MAJOR: String;
  begin
    {$IFDEF VISUAL_BUILD}
    //Result := 'MEGA ' + IntToStr(MAJOR_VERSION);
    Result := 'MEGA 11';
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
      Result := Result + 'macOS' + LineEnding;
      {$ELSE}
      Result := Result + 'Linux' + LineEnding;
      {$ENDIF}
    {$ENDIF}
    Result := Result + 'Build ' + IntToStr(BUILD_CODE);
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

  function MapSessionVersionToMegaVersion(FileType: TSessionFileType; SessionVersion: Integer): String;
  begin
    case FileType of
      sftMas:
        begin
          case SessionVersion of
            110: Result := 'MEGA 4';
            111: Result := 'MEGA 5';
            112: Result := 'MEGA 6';
          end;
        end;
      sftMsdx:
        begin
          case SessionVersion of
            1: Result := 'MEGA 5';
            2: Result := 'MEGA 6';
          end;
        end;
      sftMts:
        begin
          case SessionVersion of
            500: Result := 'MEGA 4';
            600: Result := 'MEGA 5';
            601, 602, 603: Result := 'MEGA 6';
            else
              Result := 'MEGA 6';
          end;
        end;
    end;
  end;
end.
