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

unit mversioninfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { TMegaVersionInfo }

  TMegaVersionInfo = class(TObject)
  private
    FApplicationName: String;
    FBuildCode: Integer;
    FCpuArchitecture: String;
    FMajor: Integer;
    FMinor: Integer;
    FOs: String;
    FRelease: Integer;
    FReleaseType: String;
    FMessage: String;

    function GetApplicationName: String;
    function GetBuildCode: Integer;
    function GetCpuArchitecture: String;
    function GetMajor: Integer;
    function GetMinor: Integer;
    function GetOs: String;
    function GetOsProperCase: String;
    function GetRelease: Integer;
    procedure InitDefaultValues;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateInfo(aMajor, aMinor, aRelease, aBuildCode: Integer; aAppName, aOs, aArch, aRelType, aMsg: String);
    function VersionInfoString: String;
    function VersionInfoToFile(f: String): Boolean;
    function IsCompatible(aOs: String; aArch: String; aReleaseType: String): Boolean; overload;
    function IsCompatible(aInfo: TMegaVersionInfo): Boolean; overload;
    function IsCompatibleUpgrade(aInfo: TMegaVersionInfo): Boolean;
    property Major: Integer read GetMajor;
    property Minor: Integer read GetMinor;
    property Release: Integer read GetRelease;
    property BuildCode: Integer read GetBuildCode;
    property ApplicationName: String read GetApplicationName;
    property OperatingSystem: String read GetOs;
    property CpuArchitecture: String read GetCpuArchitecture;
    property ReleaseType: String read FReleaseType;
    property Message: String read FMessage;
  end;

  TMegaVersionInfoList = specialize TFPGList<TMegaVersionInfo>;

  { TMegaReleases }

  TMegaReleases = class(TObject)
    private
      FIsSuccess: Boolean;
      FLog: TStringList;
      FReleases: TMegaVersionInfoList;
      FWebResponse: String;
      procedure Clear;
      function GetCount: Integer;
      function GetFilteredList(aOS, aCpuArch, aReleaseType: String): TMegaVersionInfoList;
      function GetLogText: String;
      function ParseWebResponse: Boolean;
      function FindNewestRelease(aList: TMegaVersionInfoList): TMegaVersionInfo;
    public
      constructor Create(webResponse: String);
      destructor Destroy; override;
      function LatestVersion(aOS: String; aCpuArch: String; aReleaseType: String): TMegaVersionInfo;
      property IsSuccess: Boolean read FIsSuccess;
      property LogText: String read GetLogText;
      property Count: Integer read GetCount;
  end;

  var
    MegaVersionInfo: TMegaVersionInfo;

implementation

uses
  MegaVerConsts, MegaConsts, jsonparser, fpjson;

{ TMegaReleases }

function TMegaReleases.GetFilteredList(aOS, aCpuArch, aReleaseType: String): TMegaVersionInfoList;
var
  i: Integer;
  aInfo: TMegaVersionInfo = nil;
begin
  Result := TMegaVersionInfoList.Create;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      aInfo := FReleases[i];
      if aInfo.IsCompatible(aOS, aCpuArch, aReleaseType) then
        Result.Add(aInfo);
    end;
end;

procedure TMegaReleases.Clear;
var
  i: Integer;
begin
  if Assigned(FReleases) and (FReleases.Count > 0) then
    for i := 0 to FReleases.Count - 1 do
      FReleases[i].Free;
  FReleases.Clear;
end;

function TMegaReleases.GetCount: Integer;
begin
  Result := FReleases.Count;
end;

function TMegaReleases.GetLogText: String;
begin
  if Assigned(FLog) then
    Result := FLog.Text;
end;

function TMegaReleases.ParseWebResponse: Boolean;
var
  parser: TJSONParser = nil;
  j: TJSONObject = nil;
  d: TJSONData = nil;
  temp: TJSONData = nil;
  i: Integer;
  aInfo: TMegaVersionInfo = nil;
  major, minor, release, build: Integer;
  os, arch, releaseType, appName, aMsg: String;
begin
  Result := False;
  try
    try
      parser := TJsonParser.Create(FWebResponse, []);
      d := parser.Parse;
      if (not Assigned(d)) or (not (d.JSONType = jtArray)) then
         raise Exception.Create('failed to parse JSON web response');
      {$IFDEF DEBUG}
      aInfo := TMegaVersionInfo.Create;
      aInfo.UpdateInfo(99, 88, 77, 99999999, 'FAKE-MEGA-UPDATE-TEST', OPERATING_SYSTEM, ARCHITECTURE, 'alpha', 'a dummy release for testing the update system in MEGA');
      FReleases.Add(aInfo);
      {$ENDIF}
      if d.Count > 0 then
        for i := 0 to d.Count - 1 do
        begin
          j := TJSONObject(d.Items[i]);
          temp := j.Find('major', jtNumber);
          major := temp.AsInteger;
          temp := j.Find('minor', jtNumber);
          minor := temp.AsInteger;
          temp := j.Find('release', jtNumber);
          release := temp.AsInteger;
          temp := j.Find('build_code', jtNumber);
          build := temp.AsInteger;
          temp := j.Find('operating_system', jtString);
          os := temp.AsString;
          temp := j.Find('cpu_arch', jtString);
          arch := temp.AsString;
          if StrToInt(arch) = 64 then
            arch := 'x86_64'
          else if StrToInt(arch) = 32 then
            arch := 'i386'
          else
            arch := '?';
          temp := j.find('app_name', jtString);
          appName := temp.AsString;
          temp := j.Find('release_type', jtString);
          releaseType := temp.AsString;
          temp := j.find('message', jtString);
          aMsg := temp.AsString;

          aInfo := TMegaVersionInfo.Create;
          aInfo.UpdateInfo(major, minor, release, build, appName, os, arch, releaseType, aMsg);
          FReleases.Add(aInfo);
        end;
      Result := FReleases.Count > 0;
    except
      on E:Exception do
      begin
        Result := False;
        FLog.Add(E.Message);
      end;
    end;
  finally
    if Assigned(parser) then
      parser.Free;
    if Assigned(j) then
      j.Free;
  end;
end;

function TMegaReleases.FindNewestRelease(aList: TMegaVersionInfoList): TMegaVersionInfo;
var
  i: Integer;
  aInfo: TMegaVersionInfo = nil;
begin
  Result := nil;
  if aList.Count > 0 then
  begin
    Result := aList[0];
    if aList.Count > 1 then
      for i := 1 to aList.Count - 1 do
      begin
        aInfo := aList[i];
        if aInfo.BuildCode > Result.BuildCode then
          Result := aInfo;
      end;
  end;
end;

constructor TMegaReleases.Create(webResponse: String);
begin
  FLog := TStringList.Create;
  FWebResponse := webResponse;
  FReleases := TMegaVersionInfoList.Create;
  FIsSuccess := ParseWebResponse;
end;

destructor TMegaReleases.Destroy;
begin
  if Assigned(FLog) then
    FLog.Free;
  if Assigned(FReleases) then
  begin
    Clear;
    FReleases.Free;
  end;
  inherited Destroy;
end;

function TMegaReleases.LatestVersion(aOS: String; aCpuArch: String; aReleaseType: String): TMegaVersionInfo;
var
  aList: TMegaVersionInfoList = nil;
begin
  try
    aList := GetFilteredList(aOS, aCpuArch, aReleaseType);
    Result := FindNewestRelease(aList);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

{ TMegaVersionInfo }

function TMegaVersionInfo.GetApplicationName: String;
begin
  Result := FApplicationName;
end;

function TMegaVersionInfo.GetBuildCode: Integer;
begin
  Result := FBuildCode;
end;

function TMegaVersionInfo.GetCpuArchitecture: String;
begin
  Result := FCpuArchitecture;
end;

function TMegaVersionInfo.GetMajor: Integer;
begin
  Result := FMajor;
end;

function TMegaVersionInfo.GetMinor: Integer;
begin
  Result := FMinor;
end;

function TMegaVersionInfo.GetOs: String;
begin
  Result := FOs;
end;

function TMegaVersionInfo.GetOsProperCase: String;
begin
  if FOs = 'windows' then
    Result := 'MS Windows'
  else if FOs = 'mac' then
    Result := 'macOS'
  else if FOs = 'linux' then
    Result := 'Linux'
  else
    Result := 'Unknown';
end;

function TMegaVersionInfo.GetRelease: Integer;
begin
  Result := FRelease;
end;

procedure TMegaVersionInfo.InitDefaultValues;
begin
  FRelease := RELEASE_NUMBER;
  FCpuArchitecture := ARCHITECTURE;
  FMinor := MINOR_VERSION;
  FMajor := MAJOR_VERSION;
  FBuildCode := BUILD_CODE;
  FApplicationName := VER_MEGA_MAJOR;
  FOs := OPERATING_SYSTEM;
  case MegaReleaseType of
    mrtAlpha: FReleaseType := 'alpha';
    mrtBeta: FReleaseType := 'beta';
    mrtStable: FReleaseType := 'stable';
    mrtDeveloper: FReleaseType := 'developer';
  end;
end;

constructor TMegaVersionInfo.Create;
begin
  InitDefaultValues;
end;

destructor TMegaVersionInfo.Destroy;
begin
  inherited Destroy;
end;

procedure TMegaVersionInfo.UpdateInfo(aMajor, aMinor, aRelease, aBuildCode: Integer; aAppName, aOs, aArch, aRelType, aMsg: String);
begin
  FMajor := aMajor;
  FMinor := aMinor;
  FRelease := aRelease;
  FBuildCode := aBuildCode;
  FApplicationName := aAppName;
  FOs := aOs;
  FCpuArchitecture := aArch;
  FReleaseType := aRelType;
  FMessage := aMsg;
end;

function TMegaVersionInfo.VersionInfoString: String;
begin
  Result := Format('version %d.%d.%d for %s - %s', [GetMajor, GetMinor, GetRelease, GetOsProperCase, GetCpuArchitecture]);
end;

function TMegaVersionInfo.VersionInfoToFile(f: String): Boolean;
var
  filename: String;
  target: Text;
begin
  Result := False;
  filename := ExpandFileName(f);
  try
    AssignFile(target, filename);
    Rewrite(target);
    WriteLn(target, GetMajor);
    WriteLn(target, GetMinor);
    WriteLn(target, GetRelease);
    WriteLn(target, GetBuildCode);
    WriteLn(target, FReleaseType);
    WriteLn(target, GetOsProperCase);
    WriteLn(target, GetCpuArchitecture);
    Result := FileExists(filename);
  finally
    CloseFile(target);
  end;
end;

function TMegaVersionInfo.IsCompatible(aOs: String; aArch: String; aReleaseType: String): Boolean;
begin
  Result := (OperatingSystem = aOs) and (CpuArchitecture = aArch);
end;

function TMegaVersionInfo.IsCompatible(aInfo: TMegaVersionInfo): Boolean;
begin
  Result := IsCompatible(aInfo.OperatingSystem, aInfo.CpuArchitecture, aInfo.ReleaseType);
end;

function TMegaVersionInfo.IsCompatibleUpgrade(aInfo: TMegaVersionInfo): Boolean;
begin
  Result := False;
  if (FReleaseType = 'stable') and (aInfo.ReleaseType <> 'stable') then
    Exit;
  if (FReleaseType = 'beta') and ((aInfo.ReleaseType = 'alpha') or (aInfo.ReleaseType = 'developer')) then
    Exit;
  if (FReleaseType = 'alpha') and (aInfo.ReleaseType = 'developer') then
    Exit;
  Result := (OperatingSystem = aInfo.OperatingSystem) and (CpuArchitecture = aInfo.CpuArchitecture);
end;

end.

