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

unit mshortcutshelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, ActnList, Menus;

  procedure UpdateShortcutsForMacOs(aList: TActionList); overload;
  procedure UpdateShortcutsForMacOs(aMenu: TMainMenu); overload;
  procedure UpdateShortcutsForMacOs(aMenu: TPopupMenu); overload;
  procedure UpdateShortCutsForMacOs(aItem: TMenuItem); overload;
  function ShortcutSanitizedForMacOs(origShortcutStr: String): String;

implementation

uses
  Dialogs;

procedure UpdateShortcutsForMacOs(aList: TActionList); overload;
{$IFDEF DARWIN}
var
   temp: String;
   i: Integer;
   a: TAction;
{$ENDIF}
 begin
   {$IFDEF DARWIN}
   try
     if aList.ActionCount > 0 then
       for i := 0 to aList.ActionCount - 1 do
       begin
         a := TAction(aList.Actions[i]);
         temp := ShortCutToText(a.ShortCut);
         if Pos('Ctrl', temp) > 0 then
         begin
           temp := ShortcutSanitizedForMacOs(temp);
           a.ShortCut := TextToShortCut(temp);
         end;
       end;
   except
     on E:Exception do
       ShowMessage('Application Error - failed to updated keyboard shortcuts for macOS. Windows keyboard shortcuts will be used instead');
   end;
   {$ENDIF}
end;

procedure UpdateShortcutsForMacOs(aMenu: TMainMenu); overload;
{$IFDEF DARWIN}
var
  i: Integer;
  aItem: TMenuItem;
{$ENDIF}
begin
  {$IFDEF DARWIN}
  try
    if aMenu.Items.Count > 0 then
      for i := 0 to aMenu.Items.Count - 1 do
      begin
        aItem := aMenu.Items[i];
        UpdateShortcutsForMacOs(aItem);
      end;
  except
    on E:Exception do
      ShowMessage('Application error - failed to updated keyboard shortcuts for macOS. Windows keyboard shortcuts will be used instead');
  end;
 {$ENDIF}
end;

procedure UpdateShortcutsForMacOs(aMenu: TPopupMenu);
{$IFDEF DARWIN}
var
  i: Integer;
  aItem: TMenuItem;
{$ENDIF}
begin
  {$IFDEF DARWIN}
  if aMenu.Items.Count > 0 then
    for i := 0 to aMenu.Items.Count - 1 do
    begin
      aItem := aMenu.Items[i];
      UpdateShortcutsForMacOs(aItem);
    end;
   {$ENDIF}
end;

procedure UpdateShortCutsForMacOs(aItem: TMenuItem);
{$IFDEF DARWIN}
var
  temp: String;
  i: Integer;
{$ENDIF}
begin
  {$IFDEF DARWIN}
  temp := ShortCutToText(aItem.ShortCut);
  if Pos('Ctrl', temp) > 0 then
  begin
    temp := ShortcutSanitizedForMacOs(temp);
    aItem.ShortCut := TextToShortCut(temp);
  end;
  if aItem.Count > 0 then
    for i := 0 to aItem.Count - 1 do
      UpdateShortCutsForMacOs(aItem.Items[i]);
  {$ENDIF}
end;

function ShortcutSanitizedForMacOs(origShortcutStr: String): String;
begin
  Result := origShortcutStr;
  if Pos('Ctrl', Result) > 0 then
    Result := StringReplace(Result, 'Ctrl', 'Meta', [rfIgnoreCase]);
  if Pos('Alt+F4', Result) > 0 then
    Result := StringReplace(Result, 'Alt+F4', 'Meta+W', [rfIgnoreCase]);
end;

end.

