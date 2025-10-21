{
	Copyright 1992-2025 Sudhir Kumar and Koichiro Tamura

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

unit MUsageStatistics;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  FileUtil, Classes, SysUtils, MegaUtils_NV;

const

//  BASEURL = 'http://127.0.0.1/megasoftware/current_release/';   // for debugging/development
  BASEURL = 'https://www.megasoftware.net/current_release';
  // MUST be https or MacOS updates fail; http is not always automatically upgraded to https

type

  { TStatisticsReporter }

  TStatisticsReporter = class(TMegaThread)
    private
      FUsageStatistics: String;
      FDataCollectionIsAllowed: Boolean;
      {$IFDEF DARWIN}
      procedure UploadStatisticsMacOS;
      {$ENDIF}
      procedure UploadStatistics;
    protected
      procedure Execute; override;
    public
      constructor Create(CreateSuspended: Boolean);
      property UsageStatistics: String read FUsageStatistics write FUsageStatistics;
      property DataCollectionIsAllowed: Boolean read FDataCollectionIsAllowed write FDataCollectionIsAllowed;
  end;

  TOptedInOrOutReporter = class(TMegaThread)

    public constructor Create(CreateSuspended: Boolean);
    procedure Execute; override;
  end;

  { TDemographicsReporter }

  TDemographicsReporter = class(TMegaThread)
    protected
      FJsonStr: String;
      {$IFDEF DARWIN}
      procedure UploadStatisticsMacOS;
      {$ENDIF}
      procedure Execute; override;
    public
      constructor Create(jsonStr: String);
  end;

procedure ValidateMegaGlobalDirEnvironment;
procedure UploadUserPref;
procedure UploadDemographics(demographicsJson: String);

implementation

uses
  {$IFDEF DEBUG}
  Dialogs,
  {$ENDIF}
  {$IFNDEF VISUAL_BUILD}
  MD_MegaMain,
  {$ENDIF}
  MegaUtils, MegaVerConsts, MegaMainPreferences, DateUtils,
  {$IFDEF DARWIN}ns_url_request,{$ENDIF}
  fphttpclient, openssl, opensslsockets,
  MegaConsts {$IFDEF VISUAL_BUILD}, Menus{$ENDIF};

procedure TDemographicsReporter.Execute;
var
  client: TFPHTTPClient = nil;
  aResponse: TStringList = nil;
  temp: String = '';
begin
  {$IFDEF DARWIN}
  UploadStatisticsMacOS;
  Exit;
  {$ENDIF}
  try
    try
      aResponse := TStringList.Create;
      client := TFPHTTPClient.Create(nil);
      client.RequestHeaders.Add('Content-Type: application/json');
      client.RequestBody := TRawByteStringStream.Create(FJsonStr);
      client.AllowRedirect := True;
      client.Post(BASEURL + '/uploadDemographics.php', aResponse);
      temp := client.ResponseStatusText;
    Except
      { purposely eat the exception}
      {$IFDEF DEBUG}{$IFDEF VISUAL_BUILD}
      on E:Exception do
      begin
        if Assigned(client) then
          temp := client.ResponseStatusText;
        ShowMessage('Error when uploading demographics: ' + E.Message + ' ' + temp)
      end;
      {$ENDIF}{$ENDIF}
    end;
  finally
    if Assigned(client) then
      client.Free;
    if Assigned(aResponse) then
      aResponse.Free;
  end;
end;

{$IFDEF DARWIN}
procedure TDemographicsReporter.UploadStatisticsMacOS;
var
  HTTP: TNSHTTPSendAndReceive = nil;
  UsageDataStream: TRawByteStringStream = nil;
  ResponseStream: TMemoryStream = nil;
  Headers: TStringList = nil;
  AResponse: String;
begin
  try
    try
      UsageDataStream := TRawByteStringStream.Create(FJsonStr);
      ResponseStream := TMemoryStream.Create;
      Headers := TStringList.Create;

      Headers.AddPair('Content-Type', 'application/json');
      HTTP := TNSHTTPSendAndReceive.Create;
      HTTP.Method   := 'POST';
      HTTP.Address  := BASEURL + '/uploadUsageStatistics2024.php';
      if not (HTTP.SendAndReceive(UsageDataStream, ResponseStream, Headers)) then
        raise Exception.Create(HTTP.LastErrMsg)
      else
      begin
        SetLength(AResponse, ResponseStream.Size);
        if ResponseStream.Size > 0 then
          ResponseStream.Read(AResponse[1], ResponseStream.Size);
      end;
    except
      on E:Exception do
      begin
        // just return an empty string
      end;
    end;
  finally
    if Assigned(HTTP) then
      HTTP.Free;
    if Assigned(UsageDataStream) then
      UsageDataStream.Free;
    if Assigned(ResponseStream) then
      ResponseStream.Free;
    if Assigned(Headers) then
      Headers.Free;
  end;
end;
{$ENDIF}

constructor TDemographicsReporter.Create(jsonStr: String);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FJsonStr := jsonStr;
end;

constructor TStatisticsReporter.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FDataCollectionIsAllowed := CollectUsageDataIsAllowed; // get the user's preference
end;

procedure TStatisticsReporter.Execute;
begin
  {$IFNDEF VISUAL_BUILD}
  if D_MegaMain.IsRunningInTestHarness then
    Exit;
  {$ENDIF}
  if not FDataCollectionIsAllowed then
    Exit;
  try
    UploadStatistics;
  Except
    // eat the error. We don't want to bother the user with this
  end;
end;

procedure TStatisticsReporter.UploadStatistics;
var
  Connection: TFPHTTPClient = nil;
  aResponse: TStringList = nil;
  response: String = '';
begin
  {$IFDEF DARWIN}
  UploadStatisticsMacOS;
  Exit;
  {$ENDIF}
  try
    try
      aResponse := TStringList.Create;
      Connection := TFPHTTPClient.Create(nil);
      Connection.RequestHeaders.Add('Content-Type: application/json');
      Connection.RequestBody := TRawByteStringStream.Create(FUsageStatistics);
      Connection.AllowRedirect := True;
      Connection.Post(BASEURL + '/uploadUsageStatistics2024.php', aResponse);
      response := Connection.ResponseStatusText;
    Except
      { eat the exception}
    end;
  finally
    if Assigned(Connection.RequestBody) then
      Connection.RequestBody.Free;
    if Assigned(Connection) then
      Connection.Free;
    if Assigned(aResponse) then
      aResponse.Free;
  end;
end;

{$IFDEF DARWIN}
procedure TStatisticsReporter.UploadStatisticsMacOS;
var
  HTTP: TNSHTTPSendAndReceive = nil;
  UsageDataStream: TRawByteStringStream = nil;
  ResponseStream: TMemoryStream = nil;
  Headers: TStringList = nil;
  AResponse: String;
begin
  try
    try
      UsageDataStream := TRawByteStringStream.Create(FUsageStatistics);
      ResponseStream := TMemoryStream.Create;
      Headers := TStringList.Create;

      //Headers.Add('Content-Type: application/json');
      Headers.AddPair('Content-Type', 'application/json');
      HTTP := TNSHTTPSendAndReceive.Create;
      HTTP.Method   := 'POST';
      HTTP.Address  := BASEURL + '/uploadUsageStatistics2024.php';
      if not (HTTP.SendAndReceive(UsageDataStream, ResponseStream, Headers)) then
        raise Exception.Create(HTTP.LastErrMsg)
      else
      begin
        SetLength(AResponse, ResponseStream.Size);
        if ResponseStream.Size > 0 then
          ResponseStream.Read(AResponse[1], ResponseStream.Size);
      end;
    except
      on E:Exception do
      begin
        // just return an empty string
      end;
    end;
  finally
    if Assigned(HTTP) then
      HTTP.Free;
    if Assigned(UsageDataStream) then
      UsageDataStream.Free;
    if Assigned(ResponseStream) then
      ResponseStream.Free;
    if Assigned(Headers) then
      Headers.Free;
  end;
end;
{$ENDIF}

procedure UploadUserPref;
var
  UploadThread: TOptedInOrOutReporter = nil;
begin
  {$IFDEF DEBUG}
  Exit;
  {$ENDIF}
  if FileExists(GetMegaGlobalFile('') + 'prefsSaved.txt') then // if it is already done, don't bother - it will just be ignored as a duplicate when the GUID is checked
    Exit;

  UploadThread := TOptedInOrOutReporter.Create(True);
  try
    UploadThread.Start;
  except
    // just the cost of doing business
  end;
end;

procedure UploadDemographics(demographicsJson: String);
var
  reporter: TDemographicsReporter = nil;
begin
  try
    reporter := TDemographicsReporter.Create(demographicsJson);
    reporter.Start;
  except
    // just eat the exception
  end;
end;

procedure ValidateMegaGlobalDirEnvironment;
var
  GlobalDir: String = '';
  TempFile: TextFile;
  FileName: String;
  Guid: TGuid;
  IdString: String;
begin
  try
    GlobalDir := GetMegaGlobalFile('');
    FileName := GlobalDir + DATA_COLLECT_PREFS_FILE;
    if not FileExists(FileName) then
    begin
      try
        try
          AssignFile(TempFile, FileName);
          Rewrite(TempFile);
          {$IFDEF VISUAL_BUILD}
          if CollectUsageDataIsAllowed then
            WriteLn(TempFile, 'allowed')
          else
            WriteLn(TempFile, 'disallowed');
          {$ELSE}
          WriteLn(TempFile, 'not specified');
          {$ENDIF}
        except
          // let it go
        end;
      finally
        CloseFile(TempFile);
      end;
    end;

    FileName := GetMegaGlobalFile('') + 'guid.txt';
    if not FileExists(FileName) then
    begin
      CreateGuid(Guid);
      IdString := GUIDToString(Guid);
      try
        try
          AssignFile(TempFile, FileName);
          Rewrite(TempFile);
          WriteLn(TempFile, IdString);
        except
          // let it go
        end;
      finally
        CloseFile(TempFile);
      end;
    end;
  Except
    // eat these errors as the code is for data collection which is not a user concern
  end;
end;

{ TOptedInOrOutReporter }

constructor TOptedInOrOutReporter.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TOptedInOrOutReporter.Execute;
var
  client: TFPHTTPClient = nil;
  UrlString: String;
  Guid: String;
  Version: String;
  Major: String;
  Minor: String;
  Os: String;
  Rt: String;
  TempList: TStringList = nil;
  Filename: String;
  response: String = '';
begin
  try try
    TempList := TStringList.Create;
    Guid := GetGuidString;
    Version := VER_MEGA_BUILD;
    Major := VER_MEGA_MAJOR_CHAR;
    Minor := VER_MEGA_MINOR_STR;
    Os := OPERATING_SYSTEM;

    if MegaReleaseType = mrtAlpha then
      Rt := 'Alpha'
    else if MegaReleaseType = mrtBeta then
      Rt := 'Beta'
    else
      Rt := 'Stable';


    FileName := GetMegaGlobalFile('') + 'optedOut.txt';
    if FileExists(FileName) then
    begin
      UrlString := BASEURL + '/updateDataCollectPrefs.php?pref=optout';
    end
    else
    begin
      FileName := GetMegaGlobalFile('') + 'optedIn.txt';
      if FileExists(FileName) then
      begin
        UrlString := BASEURL + '/updateDataCollectPrefs.php?pref=optin';
      end
      else // then the file got deleted so replace it according to the user's preference
      begin
        if CollectUsageDataIsAllowed then
        begin
          TempList.Add('ok to collect anonymous usage data');
          TempList.SaveToFile(GetMegaGlobalFile('') + 'optedIn.txt');
          UrlString := BASEURL + '/updateDataCollectPrefs.php?pref=optin';
        end
        else
        begin
          TempList.Add('not ok to collect anonymous usage data');
          TempList.SaveToFile(GetMegaGlobalFile('') + 'optedOut.txt');
          UrlString := BASEURL + '/updateDataCollectPrefs.php?pref=optout';
        end;
      end;
    end;

    UrlString := UrlString + '&version=' + Version + '&major=' + Major + '&minor=' + Minor + '&guid=' + Guid + '&rt=' + Rt + '&os=' + Os;
    Client := TFPHTTPClient.Create(nil);
    Client.AllowRedirect := True;
    Client.Get(UrlString);
    response := Client.ResponseStatusText;
    TempList.Add('user preference uploaded to server');
    TempList.SaveToFile(GetMegaGlobalFile('') + 'prefsSaved.txt');
  Except
    { purposely eat the exception}
    {$IFDEF DEBUG}{$IFDEF VISUAL_BUILD}
    on E:Exception do
    begin
      if Assigned(Client) then
        response := Client.ResponseStatusText;
      ShowMessage('Error when uploading user prefs: ' + E.Message + ' ' + response)

    end;
    {$ENDIF}{$ENDIF}
  end;
  finally
    if Assigned(client) then
      client.Free;
    if Assigned(TempList) then
      TempList.Free;
  end;
end;

end.

