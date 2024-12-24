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

unit mupdates;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
{$IFDEF VISUAL_BUILD}
uses
  LCLIntf, fphttpclient,
  SysUtils, Classes, Forms, INIFiles,
  {$IFDEF DARWIN}macos_files,{$ENDIF}
  MegaVerConsts, MegaUtils, mversioninfo;

type

  { TUpdateThread }

  TUpdateThread = class(TThread)
  private
    FMegaReleases: TMegaReleases;
    FBuildCode: Integer;
    FDownloadUrl: String;
    FHasNewVersion: Boolean;
    FHasUpdate: Boolean;
    FIsSuccess: Boolean;
    FMajorVersion: Integer;
    FVersionId: Integer;
    FMessage: String;
    FMinorVersion: Integer;
    FReleaseNumber: Integer;
    FReleaseType: String;
    FServerResponse: String;
    FIgnoreUpdate: Boolean;
    FGuidStr: String;
    function GetVersionId: Integer;
    function GetVersionString: String;
  protected

    procedure Execute; override;

    function CheckIfThisUpdateIgnored: Boolean;
    function CheckIfUpdatesAreDisabled: Boolean;
    function CheckForUpdateQueryStr: String;
    function GetCurrentReleaseListQueryStr: String;
    function SendUpdateQuery: String;
    {$IFDEF DARWIN}function SendMacUpdateQuery: String;{$ENDIF}
    function HandleRequestForCurrentReleasesResponse: Boolean;
    procedure CheckGuidStr;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure RegisterDownload;
    function RegisterDownloadUrl: String;
    function SetIgnoreThisUpdate: Boolean;
    property IgnoreUpdate: Boolean read FIgnoreUpdate;
    property HasUpdate: Boolean read FHasUpdate;
    property HasNewVersion: Boolean read FHasNewVersion;
    property MajorVersion: Integer read FMajorVersion;
    property MinorVersion: Integer read FMinorVersion;
    property ReleaseNumber: Integer read FReleaseNumber;
    property ReleaseType: String read FReleaseType;
    property BuildCode: Integer read FBuildCode;
    property Message: String read FMessage;
    property DownloadUrl: String read FDownloadUrl;
    property ServerResponse: String read FServerResponse;
    property IsSuccess: Boolean read FIsSuccess;
    property VersionString: String read GetVersionString;
    property VersionId: Integer read GetVersionId;
  end;



{$ENDIF}
implementation
{$IFDEF VISUAL_BUILD}
uses
  Dialogs, Controls, DateUtils, StringUtils, fpjson, jsonparser, MegaConsts, opensslsockets;

{ MUpdateNotify }

function TUpdateThread.GetVersionString: String;
begin
  Result := IntToStr(MajorVersion) + '.' + IntToStr(MinorVersion) + '.' + IntToStr(ReleaseNumber);
end;

function TUpdateThread.GetVersionId: Integer;
begin
  Result := FVersionId;
end;

procedure TUpdateThread.Execute;
var
  Temp: String;
begin
  FIgnoreUpdate := CheckIfUpdatesAreDisabled; { first, check if all updates are ignored}
  if not FIgnoreUpdate  then
  begin
    CheckGuidStr;
    Temp := {$IFDEF DARWIN}SendMacUpdateQuery{$ELSE}SendUpdateQuery{$ENDIF};
    if Trim(Temp) <> EmptyStr then
    begin
      FServerResponse := Temp;
      FIsSuccess := HandleRequestForCurrentReleasesResponse;
      FIgnoreUpdate := CheckIfThisUpdateIgnored; { second, check if this update is blacklisted by the user}
    end;
  end;
end;

function TUpdateThread.SetIgnoreThisUpdate: Boolean;
var
  IgnoreINI: TINIFile = nil;
begin
  try
    try
      FIgnoreUpdate := True;
      IgnoreINI := TINIFile.Create(GetPrivateFile('Private' + PathDelim + 'Ini' + PathDelim + 'ignore_updates.ini'));  // CHECK if the user has set the IgnoreINI file to disable the update system. (Usefull for schools or other environments where users don't have the permissions or control their systems)
      IgnoreINI.WriteBool('builds_to_ignore', IntToStr(FBuildCode), True);
      Result := True;
    except
      Result := False;
    end;
  finally
    if Assigned(IgnoreINI) then
      IgnoreINI.Free;
  end;
end;

function TUpdateThread.CheckIfThisUpdateIgnored: Boolean;
var
  IgnoreINI: TINIFile = nil;
begin
  try
    try
      IgnoreINI := TINIFile.Create(GetPrivateFile('Private' + PathDelim + 'Ini' + PathDelim + 'ignore_updates.ini'));  // CHECK if the user has set the IgnoreINI file to disable the update system. (Usefull for schools or other environments where users don't have the permissions or control their systems)
      Result := IgnoreINI.ReadBool('builds_to_ignore', IntToStr(FBuildCode), False);
      FIgnoreUpdate := Result;
    except
      Result := False;
    end;
  finally
    if Assigned(IgnoreINI) then
      IgnoreINI.Free;
  end;
end;

function TUpdateThread.CheckIfUpdatesAreDisabled: Boolean;
var
  IgnoreINI: TINIFile = nil;
begin
  try
    try
      IgnoreINI := TINIFile.Create(GetPrivateFile('Private' + PathDelim + 'Ini' + PathDelim + 'ignore_updates.ini'));  // CHECK if the user has set the IgnoreINI file to disable the update system. (Usefull for schools or other environments where users don't have the permissions or control their systems)
      Result := IgnoreINI.ReadBool('settings', 'ignore_all_updates', false);
    except
      Result := False;
    end;
  finally
    if Assigned(IgnoreINI) then
      IgnoreINI.Free;
  end;
end;

function TUpdateThread.CheckForUpdateQueryStr: String;
var
  aJson: TJSONObject;
  Temp: String;
begin
  aJson:= nil;

  try
    aJson := TJsonObject.Create;
    aJson.Add('build', IntToStr(BUILD_CODE));
    aJson.Add('name', APPLICATION_NAME);
    aJson.Add('os', OPERATING_SYSTEM);
    aJson.Add('major', IntToStr(MAJOR_VERSION));
    aJson.Add('minor', IntToStr(MINOR_VERSION));
    aJson.Add('release', IntToStr(RELEASE_NUMBER));
    aJson.Add('interface', USER_INTERFACE);
    aJson.Add('guid', FGuidStr);
    Temp := RemoveWhiteSpace(aJson.AsJson);
    Temp := URLEncode(Temp);
    {$IFDEF DEBUG}
    Result := 'http://update.megasoftware.net/index.php/check_update/'+ Temp;
    {$ELSE}
    Result := 'http://update.megasoftware.net/index.php/check_update/'+ Temp;
    {$ENDIF}
  finally
    if Assigned(aJson) then
      aJson.Free;
  end;
end;

function TUpdateThread.GetCurrentReleaseListQueryStr: String;
begin
  {$IFDEF DARWIN}
  Result := 'https://www.megasoftware.net/current_release/';
  {$ELSE}
  Result := 'http://www.megasoftware.net/current_release/';
  {$ENDIF}
end;

function TUpdateThread.SendUpdateQuery: String;
var
  Client: TFPHttpClient = nil;
  Request, Response: String;
begin
  Result := EmptyStr;

  try
    try
       Client := TFPHttpClient.Create(Nil);
       Client.AllowRedirect := True;
       Request := GetCurrentReleaseListQueryStr;
       Response := Client.Get(Request);
       Result := Response;
    except
      on E:Exception do
      begin
        // just return an empty string
      end;
    end;
  finally
    if Assigned(Client) then
      Client.Free;
  end;
end;


{$IFDEF DARWIN}
function TUpdateThread.SendMacUpdateQuery: String;
var
  Client: TMacHTTPSRequest = nil;
  Temp: String;
begin
  try
    try
      Client := TMacHTTPSRequest.Create();
      // NSURLSession does redirects by default
      Client.PerformRequest(GetCurrentReleaseListQueryStr);
      Temp := Client.GetResponse;
      Result := Client.GetHTML;
    except
    end;
  finally
    if Assigned(Client) then
      Client.Free;
  end;
end;
{$ENDIF}

function TUpdateThread.HandleRequestForCurrentReleasesResponse: Boolean;
var
  NewestVersionInfo: TMegaVersionInfo = nil;
  currentVersionInfo: TMegaVersionInfo = nil;
  releaseType: String = 'unknown';
begin
  case MegaReleaseType of
    mrtAlpha: releaseType := 'alpha';
    mrtBeta: releaseType := 'beta';
    mrtStable: releaseType := 'stable';
    mrtDeveloper: releaseType := 'developer';
  end;

  try
   try
     FMegaReleases := TMegaReleases.Create(FServerResponse);
     if FMegaReleases.IsSuccess then
     begin
       NewestVersionInfo := FMegaReleases.LatestVersion(OPERATING_SYSTEM, ARCHITECTURE, releaseType);
       if Assigned(newestVersionInfo) then
       begin
         FIsSuccess := True;
         currentVersionInfo := TMegaVersionInfo.Create;
         if currentVersionInfo.IsCompatibleUpgrade(NewestVersionInfo) then
         begin
           FHasUpdate := (NewestVersionInfo.BuildCode > currentVersionInfo.BuildCode);
           FMajorVersion := NewestVersionInfo.Major;
           FMinorVersion := NewestVersionInfo.Minor;
           FReleaseNumber := NewestVersionInfo.Release;
           FBuildCode := NewestVersionInfo.BuildCode;
           FReleaseType := NewestVersionInfo.ReleaseType;
           FMessage := NewestVersionInfo.Message;
           FDownloadUrl := WEBSITE_URL;
         end;
         NewestVersionInfo := nil; {FMegaReleases will free it}
       end;
     end;
     Result := Assigned(FMegaReleases) and FMegaReleases.IsSuccess;
   except

   end;
  finally
    if Assigned(currentVersionInfo) then
      currentVersionInfo.Free;
  end;
end;

procedure TUpdateThread.CheckGuidStr;
begin
 FGuidStr := GetGuidString;
 FGuidStr := Copy(FGuidStr, 2, Length(FGuidStr) - 2);
end;

constructor TUpdateThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FIgnoreUpdate := False;
  FBuildCode := -1;
  FDownloadUrl := EmptyStr;
  FHasNewVersion := False;
  FHasUpdate := False;
  FMajorVersion := -1;
  FVersionId := -1;
  FMessage := EmptyStr;
  FMinorVersion := -1;
  FReleaseNumber := -1;
  FServerResponse := EmptyStr;
  FIsSuccess := False;
  FReleaseType := EmptyStr;
  FGuidStr := EmptyStr;
  FMegaReleases :=  nil;
end;

destructor TUpdateThread.Destroy;
begin
  if Assigned(FMegaReleases) then
    FreeAndNil(FMegaReleases);
  inherited Destroy;
end;

procedure TUpdateThread.RegisterDownload;
var
  Client: TFPHttpClient = nil;
  Request, Response: String;
begin
  try
    try
      Client := TFPHttpClient.Create(Nil);
      Request := RegisterDownloadUrl;
      Response := Client.Get(Request);
    except
     on E:Exception do
     begin
       // eat the exception
     end;
    end;
  finally
    if Assigned(Client) then
      Client.Free;
  end;
end;

function TUpdateThread.RegisterDownloadUrl: String;
begin
   {$IFDEF DEBUG}
 //  Result := 'http://localhost/megasoftware_legacy/register_download.php?';
   Result := 'http://dl-572-87.slc.westdc.net/index.php/register_download/';
   {$ELSE}
   Result := 'http://www.megasoftware.net/index.php/register_download/';
   {$ENDIF}
   Result := WEBSITE_URL + '/register_download/';
   Result := Result + IntToStr(FVersionId);
   Result := Result + '/' + FGuidStr;
end;
{$ENDIF}

end.
