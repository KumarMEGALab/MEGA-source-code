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

unit macos_files;

{$mode objfpc}{$H+}
{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$modeswitch cblocks}
{$ENDIF}

interface

{$IFDEF DARWIN}
uses
  Classes, SysUtils, CocoaUtils, CocoaAll, MacOSAll;

type
  // setup cBlock for completion handler
  tblock = reference to procedure(data: NSData; response: NSURLResponse; connectionError: NSError); cdecl; cblock;

  // redefine version from packages/cocoaint/src/foundation/NSURLSession.inc
  NSURLSession = objcclass external (NSObject)
  public
    class function sessionWithConfiguration(configuration: NSURLSessionConfiguration): NSURLSession; message 'sessionWithConfiguration:';
  end;

  NSURLSessionAsynchronousConvenience = objccategory external (NSURLSession)
    function dataTaskWithURL_completionHandler(url: NSURL; completionHandler: tBlock): NSURLSessionDataTask; message 'dataTaskWithURL:completionHandler:';
  end;

  TMacHTTPSRequest = class
  private
    FResponse: String;
    FHTML: String;
    FStatusCode: Integer;
    FError: String;
    didFinish: PRTLEvent;
    procedure CompletionHandler(data: NSData; response: NSURLResponse; connectionError: NSError);
  public
    constructor Create;
    procedure PerformRequest(const AURL: String);
    function GetResponse: String;
    function GetHTML: String;
    function GetError: String;
  end;

function GetMainBundlePath: String;
function GetMainBundleDir: String;
function GetExecutablePath: String;
function GetResourcesPath: String;
function GetBundleIdentifier: String;
function GetPathForImageResources(Image: String): String;
function GetFileFromAppSupport(aValue: String): String;
function GetPrivateFileMacOS(aValue: String; CopyIfNeeded: Boolean = True): String;
function GetDevInstallFilesFolder: String;
function GetPrivateFilesDir: String;
function GetAppSupportDir: String;
{$ENDIF}

implementation

{$IFDEF DARWIN}
uses
  MegaUtils, Forms, MegaConsts, MegaPrivateFiles;

constructor TMacHTTPSRequest.Create;
begin
  FResponse := '';
  FHTML := '';
  FStatusCode := 0;
  FError := '';
  didFinish := nil;
end;

procedure TMacHTTPSRequest.PerformRequest(const AURL: String);
var
  url: NSURL = nil;
  session: NSURLSession = nil;
  configuration: NSURLSessionConfiguration = nil;
begin
  didFinish := RTLEventCreate;

  url := NSURL.URLWithString(NSSTR(PAnsiChar(AURL)));
  if(Url = Nil) then
      raise Exception.Create('web request URL is empty');

  configuration := NSURLSessionConfiguration.defaultSessionConfiguration;
  session := NSURLSession.sessionWithConfiguration(configuration);

  session.dataTaskWithURL_completionHandler(url, @CompletionHandler).resume;
  RTLEventWaitFor(didFinish);
  RTLeventdestroy(didFinish);
end;

function TMacHTTPSRequest.GetResponse: String;
begin
  Result := FResponse;
end;

function TMacHTTPSRequest.GetHTML: String;
begin
  Result := FHTML;
end;

function TMacHTTPSRequest.GetError: String;
begin
  Result := FError;
end;

procedure TMacHTTPSRequest.CompletionHandler(data: NSData; response: NSURLResponse; connectionError: NSError);
var
  httpResponse: NSHTTPURLResponse;
begin
  if((data.Length > 0) and (connectionError = Nil)) then
    begin
      httpResponse := NSHTTPURLResponse(response);
      FStatusCode :=  httpResponse.statusCode;
      FResponse   :=  NSStringToString(response.description);
      FHTML       :=  NSSTringToString(NSString.alloc.initWithData_encoding(data,NSASCIIStringEncoding));
    end
  else
    begin
      FError := 'Error description: ' + LineEnding +  NSStringToString(connectionError.description);
    end;
  RTLEventSetEvent(didFinish);
end;

function GetPrivateFilesDir: String;
begin
  Result := GetResourcesPath + PathDelim + 'Private';
end;

function GetAppSupportDir: String;
const
  MAX_PATH = 4096;
var
  theError: OSErr;
  theRef: FSRef;
  pathBuffer: PChar = nil;
begin
  Result := EmptyStr;
  theRef := Default(FSRef);

  try
    pathBuffer := Allocmem(MAX_PATH);
    Fillchar(theRef, Sizeof(theRef), #0);
    theError := FSFindFolder(kUserDomain , kApplicationSupportFolderType, kDontCreateFolder, theRef);
    if (pathBuffer <> nil) and (theError = noErr) then
    begin
      theError := FSRefMakePath(theRef, pathBuffer, MAX_PATH);
      if theError = noErr then
        Result := UTF8ToAnsi(StrPas(pathBuffer)) + PathDelim + ApplicationName + PathDelim;
      if not DirectoryExists(Result) then
        if not ForceDirectories(Result) then
          raise Exception.Create(Format('failed to create application support directory: %s', [Result]));
    end;
  finally
    Freemem(pathBuffer);
  end;
end;

function GetDevInstallFilesFolder: String;
begin
  Result := ExtractFileDir(GetMainBundlePath) + PathDelim + 'MEGA7_Install';
end;

function GetFileFromAppSupport(aValue:String): String;
var
  UserDir : String;
  FullPath: String;
begin
  UserDir := GetAppSupportDir;
  if not DirectoryExists(UserDir) then
    raise Exception.Create(Format('failed to find application support directory: %s', [UserDir]));
  FullPath := UserDir + aValue;
  UserDir := ExtractFilePath(FullPath);
  if not DirectoryExists(UserDir) then
    if not ForceDirectories(UserDir) then
      raise Exception.Create(Format('failed to create application support sub-directory: %s', [UserDir]));
  if not FileExists(FullPath)  then // If the file doesn't exist copy the file from (in most cases 'private/Ini') to /temp as a template
    CopyFileFromRES(String(AValue), FullPath, False);
  Result := FullPath;
end;

function GetPrivateFileMacOS(aValue: String; CopyIfNeeded: Boolean = True): String;

  function IsWebOptionsDialog(aPath: String): Boolean;
  var
    aDir: String;
  begin
    Result := False;
    if Pos('OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim, aPath) > 0 then
      Exit(True);
  end;

  function NeedsAppSupportDirectory(Path : String) : Boolean;
  begin
    Result := (Pos('Ini', Path) > 0) or
              (Path = mfJobQueueIDs) or
              (Path = mfBrowserCacheFile) or
              (Path = mfBrowserLogFile) or
              (Path = mfJobQueueIDs) or
              (path = MF_ANALYSIS_SETTINGS_FILE) or
              (Pos(MuscleRunLog, aValue) > 0) or
              (Pos(MuscleInput, aValue) > 0) or
              (Pos(MuscleOutput, aValue) > 0)
              {$IFDEF DARWIN}or IsWebOptionsDialog(Path){$ENDIF};

  end;

var
  UserDir : String;
  FullPath: String;
  InIde: Boolean = False;
begin
  {$IFDEF DEBUG}
  //InIde := True;
 {$ENDIF}
  if InIde then
  begin
    Result := GetDevInstallFilesFolder + PathDelim + AValue;
    Exit; // When running in the IDE we don't copy anything, it is completely seperate from the user version.
  end;
  UserDir := ExtractFilePath(GetResourcesPath + PathDelim);
  FullPath := UserDir + AValue;

  if AnsiCompareText(ExtractFileName(AValue), '') = 0 then // If no file is specified the just return the path where the file would be stored.
  begin
    result := userDir;
    Exit;
  end;

  if NeedsAppSupportDirectory(aValue) then
  begin
    UserDir := GetAppSupportDir;
    if not DirectoryExists(UserDir) then
      raise Exception.Create(Format('failed to find application support directory: %s', [UserDir]));
    FullPath := UserDir + aValue;
    UserDir := ExtractFilePath(FullPath);
    if not DirectoryExists(UserDir) then
      if not ForceDirectories(UserDir) then
        raise Exception.Create(Format('failed to create application support sub-directory: %s', [UserDir]));
    if not FileExists(FullPath)  then // If the file doesn't exist copy the file from (in most cases 'private/Ini') to /temp as a template
      CopyFileFromRES(String(AValue), FullPath, CopyIfNeeded);
    Result := FullPath;
  end
  else
  begin
    if not DirectoryExists(userDir) then  //If the folder in ~/Library/Application Support is not there
    begin
      // Need to handle inability to create dir
      if not ForceDirectories(userDir) then  // For some reason we couldn't create the Folder structure in APPDATA, so go to a backup plan.
      begin
        userDir := ExtractFileDir(ExtractFilePath(Application.ExeName) + 'temp' + PathDelim + AValue); // On failure of using the %APPDATA%/MEGA_BUILD to store, use ExeLocation/temp to store the files, NOTE: This is NOT user indepenent, meaning all users share the same settings if this happens
        if ForceDirectories(userDir) then // Create needed directories for ExeLocation/MEGA/Temp
        begin
          if not FileExists(ExtractFilePath(Application.ExeName) + 'temp' + PathDelim + AValue)  then // If the file doesn't exist copy the file from (in most cases 'private/Ini') to /temp as a template
            CopyFileFromRES(String(AValue), ExtractFilePath(Application.ExeName) + 'temp' + PathDelim + AValue, CopyIfNeeded);
          Result := ExtractFilePath(Application.ExeName) + 'temp' + PathDelim + AValue;
          exit;
        end
        else
          Raise Exception.Create('Unable to extract necessary file ' + AValue + ' from RES file, creation of %appdata% folders, and temp folders failed');
      end;
    end;
  end;

  // need to handle a non-zero returned
  if not FileExists(FullPath) then
    CopyFileFromRES(AValue, FullPath, CopyIfNeeded);
  Result := FullPath;
end;

function GetMainBundlePath: String;
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
  status: Boolean = False;
begin
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  status := CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  if status then
    Result := pathStr
  else
    raise Exception.Create('Error in GetBundlePath()');
end;

function GetMainBundleDir: String;
begin
  Result := ExtractFileDir(GetMainBundlePath);
end;

function GetExecutablePath : String;
begin
  Result := GetMainBundlePath + PathDelim + 'Contents' + PathDelim + 'MacOS' + PathDelim + ExtractFileName(Application.ExeName);
end;

function GetResourcesPath: String;
begin
  Result := GetMainBundlePath + PathDelim + 'Contents' + PathDelim + 'Resources';
end;

function GetBundleIdentifier : String;
begin
  raise Exception.Create('not implemented');
end;

function GetSharedSupportPath : String;
begin
  raise Exception.Create('not implemented');
  //Result := NSStringToString(NSBundle.mainBundle.sharedSupportPath);
end;

function GetPathForImageResources(Image: String) : String;
begin
  raise Exception.Create('not implemented');
  //Result := NSStringToString(NSBundle.mainBundle.pathForImageResource(Image));
end;
{$ENDIF}

end.

