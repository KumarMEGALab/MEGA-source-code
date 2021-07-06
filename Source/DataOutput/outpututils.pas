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

unit OutputUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}FileUtil,{$ENDIF}
  Dialogs, Classes, SysUtils, Controls;

function AskUserForFileSaveLocation(Filters: TStringList = nil): String; overload;
function AskUserForFileSaveLocation(Filename: String; Filters: TStringList = nil): String; overload;


implementation

///  <summary>Ask the user to specify a path/name for saving a file. If the specified
///  file already exists, the user will be prompted for permission to overwrite the
///  existing file.</summary>
///  <params name="Filters">An optional TStringList whose name/value pairs define
///  the filters to be used by the TSaveDialog. Each name is a string that describes
///  the type of file to filter for and each value defines the filter (e.g. '*.meg').
///  If no Filters param is provided, all files in the active directory will
///  be shown.</params>
///  <returns>The full path to the user-specified file, or EmptyStr</returns>
///  <note>If the Filters param is provided, TSaveDialog.FilterIndex will be
///  set to 1 (the first filter in the list).</note>
function AskUserForFileSaveLocation(Filters: TStringList = nil): String;
var
  SaveDialog: TSaveDialog;
  i: Integer;
  FilterString: String;
  Mr: Word;
begin
  SaveDialog := TSaveDialog.Create(nil);
  SaveDialog.Title := 'Please specify a file to save to';
  SaveDialog.InitialDir := GetCurrentDir;
  if Filters <> nil then
  begin
    for i := 0 to Filters.Count - 1 do
    begin
      FilterString := FilterString + trim(Filters.Names[i]) + '|' + trim(Filters.Values[Filters.Names[i]]);
      if i <> (Filters.Count - 1) then
        FilterString := FilterString + '|';
    end;
    SaveDialog.Filter := FilterString;
    SaveDialog.FilterIndex := 1;
  end;

  if not SaveDialog.Execute then
    Result := EmptyStr
  else
  begin
    Result := SaveDialog.FileName;
  end;

  while FileExists(SaveDialog.FileName) do
  begin
    mr := MessageDlg('That file exists. Shall we overwrite it?', mtWarning, [mbYes, mbNo, mbCancel], 0);
    if  mr = mrYes then
      Exit
    else if mr = mrNo then
    begin
      if not SaveDialog.Execute then
      begin
        Result := EmptyStr;
        Exit;
      end
    end
    else
    begin
      Result := EmptyStr;
      Exit;
    end
  end;

  FreeAndNil(SaveDialog);
end;

function AskUserForFileSaveLocation(Filename: String; Filters: TStringList = nil): String;
var
  SaveDialog: TSaveDialog;
  i: Integer;
  FilterString: String;
  Mr: Word;
begin
  SaveDialog := TSaveDialog.Create(nil);
  SaveDialog.Title := 'Please specify a file to save to';
  SaveDialog.InitialDir := GetCurrentDir;
  SaveDialog.FileName := Filename;
  if Filters <> nil then
  begin
    for i := 0 to Filters.Count - 1 do
    begin
      FilterString := FilterString + trim(Filters.Names[i]) + '|' + trim(Filters.Values[Filters.Names[i]]);
      if i <> (Filters.Count - 1) then
        FilterString := FilterString + '|';
    end;
    SaveDialog.Filter := FilterString;
    SaveDialog.FilterIndex := 1;
  end;

  if not SaveDialog.Execute then
    Result := EmptyStr
  else
  begin
    Result := SaveDialog.FileName;
  end;

  while FileExists(SaveDialog.FileName) { *Converted from FileExists*  } do
  begin
    mr := MessageDlg('That file exists. Shall we overwrite it?', mtWarning, [mbYes, mbNo, mbCancel], 0);
    if  mr = mrYes then
      Exit
    else if mr = mrNo then
    begin
      if not SaveDialog.Execute then
      begin
        Result := EmptyStr;
        Exit;
      end
    end
    else
    begin
      Result := EmptyStr;
      Exit;
    end
  end;

  FreeAndNil(SaveDialog);
end;


end.
