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

unit MegaMainPreferences;

interface

uses
  {$IFDEF VISUAL_BUILD}mega_main,{$ENDIF}
  Menus, mcustominifile;

procedure UpdatePreferencesMainMenu(cap: String; che: Boolean; option: String);
function  TryToGetPreferencesMainMenu(cap: String; var che: Boolean; var option: String): Boolean;
procedure LoadMainMenuPreferencesOnCreate(AIniFileName: String);
procedure SaveMainMenuPreferencesOnDestroy;
procedure UpdateDataCollectPrefs(IsAllowed: Boolean);
function CheckIfDataCollectPrefSpecified: Boolean;
function CollectUsageDataIsAllowed: Boolean;
function ClosePrototyperPopup: Boolean;

var
  PrefMenu: TPopupMenu;

implementation

uses
  LCLIntF, LCLType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MegaConsts, MegaUtils, ExtCtrls, Buttons;

var
  FDoNotPromptSettings: TMegaIniFile;

procedure RestoreSettingsFromIni(Ini: TMegaIniFile);
var
  i: Integer;
  settings, checked: TStringList;
  menuToAdd: TMenuItem;
  ABool: Boolean;
begin
  {$IFNDEF CALTEST}
  if Ini.SectionExists('settings') and Ini.SectionExists('checked') then
  begin
    settings := TStringList.Create;  // we dont' need to restore the settings, just to verify that there is a setting for each entry in 'checked'
    checked := TStringList.Create;
    try
      Ini.ReadSection('settings', settings);
      Ini.ReadSection('checked', checked);
      for i:=0 to checked.Count-1 do
      begin
        menuToAdd := TMenuItem.Create(nil);
        menuToAdd.Caption := checked.Names[i];
        if TryStrToBool(Checked.ValueFromIndex[i], ABool) then
          menuToAdd.Checked := ABool
        else
          menuToAdd.Checked := False;
        menuToAdd.AutoCheck := true;
        PrefMenu.Items.Add(menuToAdd);
        {$IFDEF VISUAL_BUILD}
        menuToAdd.OnClick:=@MegaForm.MenuItem91Click;
        {$ENDIF}
      end;
    finally
      FreeAndNil(settings);
      FreeAndNil(checked);
    end;
  end;
  {$ENDIF}
end;

procedure SaveSettingsToIni; // Saves the preferences for do not ask to an INI file.  NEEDS access to MEGAMain
var
  i: Integer;
  ButtonCaption: String;
begin
  if not Assigned(FDoNotPromptSettings) then
    raise Exception.Create('Application Error: invalid call to SaveSettingsToIni - write is nil');
  {$IFNDEF CALTEST}
  for i:=0 to PrefMenu.Items.Count-1 do
  begin
    ButtonCaption := PrefMenu.Items[i].Caption;
    if Pos('&', ButtonCaption) > 0 then
      Delete(ButtonCaption, pos('&', ButtonCaption), 1); // just removing the & which delphi appends to a menu items caotuib which is NOT checked.
    FDoNotPromptSettings.WriteString('checked', ButtonCaption, BoolToStr(PrefMenu.Items[i].Checked, True));
    FDoNotPromptSettings.WriteString('settings', ButtonCaption, BoolToStr(PrefMenu.Items[i].Checked, True));
  end;
  {$ENDIF}
end;

procedure LoadMainMenuPreferencesOnCreate(AIniFileName: String);
begin
  FDoNotPromptSettings := TMegaIniFile.Create(AIniFileName);
  RestoreSettingsFromIni(FDoNotPromptSettings);
end;

// Everything is freed using this function which is called when the main MEGA form is destroyed.
procedure SaveMainMenuPreferencesOnDestroy;
begin
  try
    if Assigned(FDoNotPromptSettings) then
    begin
      SaveSettingsToIni;
      FreeAndNil(FDoNotPromptSettings);
    end;
  Except
    on E:Exception do
      ShowMessage('Application Error when saving preferences: ' + E.Message);
  end;
end;

procedure UpdatePreferencesMainMenu(cap: String; che: Boolean; option: String);
var
  foundMenuItem: TMenuItem;
begin
  foundMenuItem := PrefMenu.Items.Find(cap); //PreferencesDoNotAskSubmenu.Find(cap);
  if FoundMenuItem = nil then
  begin
    FoundMenuItem := TMenuItem.Create(PrefMenu);
    FoundMenuItem.Caption := cap;
    FoundMenuItem.Checked := che;
    FoundMenuItem.AutoCheck := True;
    PrefMenu.Items.Add(FoundMenuItem);
  end
  else
    FoundMenuItem.Checked := che;
  try
    if Assigned(FDoNotPromptSettings) then
      FDoNotPromptSettings.WriteString('settings', cap, option);
  except
    on E: Exception do
      ShowMessage('Application Error when saving preferences: ' + E.Message);
  end;
end;

function TryToGetPreferencesMainMenu(cap: String; var che: Boolean; var option: String): Boolean; // returns true if the cap is in the options list.
var
  foundMenuItem: TMenuItem;
begin
  try
    foundMenuItem := PrefMenu.Items.Find(cap);
    if (FoundMenuItem = nil) or (not Assigned(FDoNotPromptSettings)) then
    begin
      result := false;
      exit;
    end
    else
    begin
      result := true;
      che := foundMenuItem.Checked;
      option := FDoNotPromptSettings.ReadString('settings', cap, option);
    end;
  except
    on E: Exception do
    begin
      result := false;
      option := EmptyStr;
    end;
  end;
end;


procedure UpdateDataCollectPrefs(IsAllowed: Boolean);
var
  PrefsFile: TextFile;
  PrefsFileString: String;
begin
  try
    try
      PrefsFileString := GetMegaGlobalFile('') + 'MegaDataCollectPrefs.txt';
      AssignFile(PrefsFile, PrefsFileString);
      Rewrite(PrefsFile);
      if IsAllowed then
        WriteLn(PrefsFile, 'allowed')
      else
        WriteLn(PrefsFile, 'disallowed');
    except
      // don't sweat it
    end;
  finally
    CloseFile(PrefsFile);
  end;
end;

// This function should only be used by MEGA-Proto since it ignores version numbers and build numbers
function CheckIfDataCollectPrefSpecified: Boolean;
 var
  PrefsFile: TextFile;
  PrefsFileString: String;
  PrefList: TStringList;
begin
  Result := False;
  PrefList := nil;

  PrefsFileString := GetMegaGlobalFile('') + 'MegaDataCollectPrefs.txt';

  try
    try
      if FileExists(PrefsFileString) then
      begin
        PrefList := TStringList.Create;
        PrefList.LoadFromFile(PrefsFileString);
        if PrefList.Count > 0 then
          if (PrefList[0] = 'allowed') or (PrefList[0] = 'disallowed') then
            Result := True
      end
      else
      begin
        Result := False;
        if not DirectoryExists(ExtractFileDir(PrefsFileString)) then
          if ForceDirectories(ExtractFileDir(PrefsFileString)) then
          begin
            AssignFile(PrefsFile, PrefsFileString);
            Rewrite(PrefsFile);
            WriteLn(PrefsFile, 'not specified');
            CloseFile(PrefsFile);
          end
          else
            Result := True; // if we fail to create the directory then returning true will cause us to just give up. We won't collect statistics and we won't prompt the user any more
      end;
    Except
      Result := False;
    end;
  finally
    if PrefList <> nil then
      FreeAndNil(PrefList);
  end;
end;

function ClosePrototyperPopup: Boolean;
var
  CloseProtoPopupMenuItem: TMenuItem;
begin
  CloseProtoPopupMenuItem := nil;
  Result := False;
  if Assigned(PrefMenu) then
    CloseProtoPopupMenuItem := PrefMenu.Items.Find(UserPref_CloseProtoyperPopupStr);
  if CloseProtoPopupMenuItem = nil then
    Exit
  else
    Result := CloseProtoPopupMenuItem.Checked;
  CloseProtoPopupMenuItem.Enabled:=True;
end;

function CollectUsageDataIsAllowed: Boolean;
{$IFDEF VISUAL_BUILD}
var
  FoundMenuItem: TMenuItem;
begin
  FoundMenuItem := nil;
  Result := False;
  if Assigned(PrefMenu) then
    FoundMenuItem := PrefMenu.Items.Find(UserPref_AllowCollectUsageData);
  if FoundMenuItem = nil then
    Exit
  else
    Result := FoundMenuItem.Checked;
end;
{$ELSE}
 var
  PrefList: TStringList;
  PrefsFileString: String;
begin
  Result := False;
  PrefList := nil;

  PrefsFileString := GetMegaGlobalFile('') + 'MegaDataCollectPrefs.txt';

  try
    try
      if FileExists(PrefsFileString) then
      begin
        PrefList := TStringList.Create;
        PrefList.LoadFromFile(PrefsFileString);
        if PrefList.Count > 0 then
          if LowerCase(trim(PrefList[0])) = 'allowed' then
            Result := True;
      end;
    Except
      Result := False;
    end;
  finally
    if Assigned(PrefList) then
      PrefList.Free;
  end;
end;
{$ENDIF}

end.


