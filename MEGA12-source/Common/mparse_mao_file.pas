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

unit mparse_mao_file;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MProcessPack;

function ParseAnalysisPreferencesFile(const filename: String; var aProcessPack: TProcessPack; var shouldContainCoding: Boolean): Boolean;
function ParseAnalysisPreferencesStrings(const maoStrings: TStringList; var aProcessPack: TProcessPack; var shouldContainCoding: Boolean): Boolean;
function ParseSectionFromAnalyisPreferencesFile(const filename: String; const sectionName: String): TStringList;
function ParseVersionInfo(const infoString: String; var major: Integer; var build: Integer; var arch: String; var operatingSystem: String; var aMsg: String): Boolean;

implementation

uses
  {$IFNDEF VISUAL_BUILD}MegaUtils_NV, MD_MegaMain,{$ENDIF}
  mcustominifile, typinfo, MegaConsts, MegaAnalysisPrefStrings;

function ParseAnalysisPreferencesFile(const filename: String; var aProcessPack: TProcessPack; var shouldContainCoding: Boolean): Boolean;
var
  Parser: TMegaIniFile = nil;
  TextualSettingValue: AnsiString;
  TextualSettingKey: AnsiString;
  SectionValues: TStringList = nil; // holds the key/value pairs from each section
  i: Integer;
  aStream: TFileStream = nil;
  numReps: Integer = 0;
  major: Integer = -1;
  build: Integer = -1;
  arch: String = '';
  os: String = '';
  aMsg: String = '';
begin
  Result := False;
  try
    try
      if not FileExists(filename) then
        raise Exception.Create('Unable to open the specified mao file (' + filename + ')');
      aStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
      Parser := TMegaIniFile.Create(aStream);
      SectionValues := TStringList.Create;
      Parser.ReadSectionValues('MEGAinfo', SectionValues);


      SectionValues.Clear;
      Parser.ReadSectionValues('ProcessTypes', SectionValues);  //Read in each ProcessType to the process pack, these will determine which analsysis we do i.e. ppInfer, ppML will infer an Maximum Likelihood tree, the other settings are in text, we read those later
      if SectionValues.Count = 0 then
        raise Exception.Create('Could not find the entry [ProcessTypes] in the MEGA Analysis Options (.mao) file provided');
      for i := 0 to SectionValues.Count - 1 do
      begin
        TextualSettingValue := SectionValues.ValueFromIndex[i];
        if not SameText(TextualSettingValue, 'false') then
          aProcessPack.AddProcessType(TProcessType(GetEnumValue(TypeInfo(TProcessType), SectionValues.Names[i])));
      end;

      SectionValues.Clear;
      Parser.ReadSectionValues('AnalysisSettings', SectionValues);
      if (SectionValues.Count = 0) and (not aProcessPack.HasProcessType(ppRelTimeBLens)) and (not aProcessPack.HasProcessType(ppCorrTestBlens)) then
        raise Exception.Create('Could not find the entry [AnalysisSettings] in the MEGA Analysis Options (.mao) file provided');
      if SectionValues.Count > 0 then
        for i := 0 to SectionValues.Count - 1 do
        begin
          TextualSettingKey := SectionValues.Names[i];
          TextualSettingValue := SectionValues.ValueFromIndex[i];
          aProcessPack.TextualSettingsList.Values[TextualSettingKey] := TextualSettingValue;  //Add the Key and Value to the StringList so we can access the key with .Strings[i] and the value with .Values[i]  where i is the index
        end;

      SectionValues.Clear;
      Parser.ReadSectionValues('DataSettings', SectionValues);
      if SectionValues.Count = 0 then
        raise Exception.Create('Could not find entry [DataSettings] in the MEGA Analysis Options (.mao) file provided');
      for i := 0 to SectionValues.Count - 1 do
      begin
        TextualSettingKey := SectionValues.Names[i];
        TextualSettingValue := SectionValues.ValueFromIndex[i];
        aProcessPack.TextualSettingsList.Values[TextualSettingKey] := TextualSettingValue;  //Add the Key and Value to the StringList so we can access the key with .Strings[i] and the value with .Values[i]  where i is the index
      end;
      if SameText(aProcessPack.TextualSettingsList.Values[containsCodingNucStr], 'True') then
        shouldContainCoding := True
      else
        shouldContainCoding := False;
      Result := True;
    except on E:Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        raise E;
        {$ELSE}
        Error_NV('Failed to parse the MEGA Analysis Options (.mao) file: ' + E.Message, E);
        {$ENDIF}
      end;
    end;
  finally
    if Assigned(aStream) then
      aStream.Free;
    if Assigned(Parser) then
      Parser.Free;
    if Assigned(SectionValues) then
      SectionValues.Free;
  end;
end;

function ParseAnalysisPreferencesStrings(const maoStrings: TStringList; var aProcessPack: TProcessPack; var shouldContainCoding: Boolean): Boolean;
var
  Parser: TMegaIniFile = nil;
  TextualSettingValue: AnsiString;
  TextualSettingKey: AnsiString;
  SectionValues: TStringList = nil; // holds the key/value pairs from each section
  i: Integer;
  aStream: TStringStream = nil;
begin
  Result := False;
  try
    try
      if Trim(maoStrings.Text) = EmptyStr then
        raise Exception.Create('analysis settings strings are empty');
      aStream := TStringStream.Create(maoStrings.Text);
      Parser := TMegaIniFile.Create(aStream);
      SectionValues := TStringList.Create;
      Parser.ReadSectionValues('MEGAinfo', SectionValues);

      SectionValues.Clear;
      Parser.ReadSectionValues('ProcessTypes', SectionValues);  //Read in each ProcessType to the process pack, these will determine which analsysis we do i.e. ppInfer, ppML will infer an Maximum Likelihood tree, the other settings are in text, we read those later
      if SectionValues.Count = 0 then
        raise Exception.Create('Could not find the entry [ProcessTypes] in the MEGA Analysis Options strings');
      for i := 0 to SectionValues.Count - 1 do
      begin
        TextualSettingValue := SectionValues.ValueFromIndex[i];
        if not SameText(TextualSettingValue, 'false') then
          aProcessPack.AddProcessType(TProcessType(GetEnumValue(TypeInfo(TProcessType), SectionValues.Names[i])));
      end;

      SectionValues.Clear;
      Parser.ReadSectionValues('AnalysisSettings', SectionValues);
      if (SectionValues.Count = 0) and (not aProcessPack.HasProcessType(ppRelTimeBLens)) and (not aProcessPack.HasProcessType(ppCorrTestBlens)) then
        raise Exception.Create('Could not find the entry [AnalysisSettings] in the MEGA Analysis Options strings');
      if SectionValues.Count > 0 then
        for i := 0 to SectionValues.Count - 1 do
        begin
          TextualSettingKey := SectionValues.Names[i];
          TextualSettingValue := SectionValues.ValueFromIndex[i];
          aProcessPack.TextualSettingsList.Values[TextualSettingKey] := TextualSettingValue;  //Add the Key and Value to the StringList so we can access the key with .Strings[i] and the value with .Values[i]  where i is the index
        end;

      SectionValues.Clear;
      Parser.ReadSectionValues('DataSettings', SectionValues);
      if SectionValues.Count = 0 then
        raise Exception.Create('Could not find entry [DataSettings] in the MEGA Analysis Options strings');
      for i := 0 to SectionValues.Count - 1 do
      begin
        TextualSettingKey := SectionValues.Names[i];
        TextualSettingValue := SectionValues.ValueFromIndex[i];
        aProcessPack.TextualSettingsList.Values[TextualSettingKey] := TextualSettingValue;  //Add the Key and Value to the StringList so we can access the key with .Strings[i] and the value with .Values[i]  where i is the index
      end;
      if SameText(aProcessPack.TextualSettingsList.Values[containsCodingNucStr], 'True') then
        shouldContainCoding := True
      else
        shouldContainCoding := False;
      Result := True;
    except on E:Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        raise E;
        {$ELSE}
        Error_NV('Failed to parse the MEGA Analysis Options strings: ' + E.Message, E);
        {$ENDIF}
      end;
    end;
  finally
    if Assigned(aStream) then
      aStream.Free;
    if Assigned(Parser) then
      Parser.Free;
    if Assigned(SectionValues) then
      SectionValues.Free;
  end;
end;

function ParseSectionFromAnalyisPreferencesFile(const filename: String; const sectionName: String): TStringList;
var
  Parser: TMegaIniFile = nil;
  TextualSettingValue: AnsiString;
  TextualSettingKey: AnsiString;
  SectionValues: TStringList = nil;
  i: Integer;
  aStream: TFileStream = nil;
begin
  Result := TStringList.Create;
  try
    try
      if not FileExists(filename) then
        raise Exception.Create('Unable to open the specified mao file (' + filename + ')');
      aStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
      Parser := TMegaIniFile.Create(aStream);
      SectionValues := TStringList.Create;
      Parser.ReadSectionValues(sectionName, SectionValues);
      if SectionValues.Count = 0 then
        raise Exception.Create(Format('Could not find the entry %s in the MEGA Analysis Options (.mao) file provided', [sectionName]));
      for i := 0 to SectionValues.Count - 1 do
      begin
        TextualSettingKey := SectionValues.Names[i];
        TextualSettingValue := SectionValues.ValueFromIndex[i];
        Result.Values[TextualSettingKey] := TextualSettingValue;
      end;
    except on E:Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        raise E;
        {$ELSE}
        Error_NV('Failed to parse the MEGA Analysis Options (.mao) file: ' + E.Message, E);
        {$ENDIF}
      end;
    end;
  finally
    if Assigned(aStream) then
      aStream.Free;
    if Assigned(Parser) then
      Parser.Free;
    if Assigned(SectionValues) then
      SectionValues.Free;
  end;
end;

function ParseVersionInfo(const infoString: String; var major: Integer; var build: Integer; var arch: String; var operatingSystem: String; var aMsg: String): Boolean;
var
  tempInt: Integer = -1;
  tempStr: String = '';
begin
  Result := False;
  tempStr := Copy(Trim(infoString), 1, 2);
  if not TryStrToInt(tempStr, tempInt)then
  begin
    aMsg := Format('could not parse major version from first 2 characters (%s)', [tempStr]);
    Exit;
  end
  else
    major := tempInt;

  tempStr := Copy(Trim(infoString), 3, 6);
  if not TryStrToInt(tempStr, tempInt)then
  begin
    aMsg := Format('could not parse build number from %s', [tempStr]);
    Exit;
  end
  else
    build := tempInt;

  Result := True; // the rest is not worth failing over as they are not used in any meaningful way

  tempStr := LowerCase(Trim(infoString));
  if tempStr.Contains('-x86_64') then
    arch := 'x86_64'
  else if tempStr.Contains('i386') then
    arch := 'i386'
  else if tempStr.Contains('arm64') then
    arch := 'arm64'
  else
    arch := 'unknown';

  if tempStr.Contains('windows') then
    operatingSystem := 'MS Windows'
  else if tempStr.Contains('linux') then
    operatingSystem := 'Linux'
  else if tempStr.Contains('macos') or tempStr.Contains('darwin') then
    operatingSystem := 'macOS'
  else
    operatingSystem := 'unknown';
end;

end.

