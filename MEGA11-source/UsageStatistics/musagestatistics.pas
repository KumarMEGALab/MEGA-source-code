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

unit MUsageStatistics;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{
   A brief overview of how the usage statistics collection system works (or 'How I Tested the Usage Statistics Collection System')
     ALL:
       1. MEGA_Global directory must be autocreated (if missing) whenever GetMegaGlobalDir is called.

     MEGA-Proto:

       1. If MEGA_Global\MegaDataCollectPrefs.txt doesn't exist (the first time MEGA-Proto is used), then the following conditions must be true:
            it is auto-created
            it contains a single string = 'not specified'
            MegaForm.AskDataCollectPanel.Visible := True.

       2. If MEGA_Global\MegaDataCollectPrefs.txt does exist, then it can contain one of 3 possible values:
            'not specified' - In this case, the following conditions must be true:
               MegaForm.AskDataCollectPanel.Visible := True
               The 'Private\Ini\PrototyperSettings.ini' file must exist and its single preference set to False (0).
               On the preferences menu, the 'Allow MEGA to collect anonymous usage data' menu item must NOT be checked

            'allowed' - In this case, the following conditions must be true:
               MegaForm.AskDataCollectPanel.Visible := False
               The 'Private\Ini\PrototyperSettings.ini' file must exist and its single preference set to True (-1).
               On the preferences menu, the 'Allow MEGA to collect anonymous usage data' menu item must be checked

            'disallowed' - In this case, the following conditions must be true:
               MegaForm.AskDataCollectPanel.Visible := False
               The 'Private\Ini\PrototyperSettings.ini' file must exist and its single preference set to False (0).
               On the preferences menu, the 'Allow MEGA to collect anonymous usage data' menu item must NOT be checked

       3. When the user changes the value of 'Allow MEGA to collect anonymous usage data' on the preferences menu:
             a) MEGA_Global\MegaDataCollectPrefs.txt must be immediately updated with the user's preference so that it contains
                a single string. Either 'allowed' or 'disallowed' as needed.
             b) When the prototyper is closed 'Private\Ini\PrototyperSettings.ini' file must have its single preference updated to match the user's preference

       4. Whenever the prototyper is launched, the preferences menu is set using the value contained in Private\Ini\PrototyperSettings.ini
          If that file does not exist, it is extracted from the res file which has the default setting of 'allow MEGA to collect anonymous usage data = False.

     MEGA-CC:

       From MD_MegaMain.Run, 'CollectUsageStatisticsIfItsTime' will be called
         1. If MEGA_Global\MegaDataCollectPrefs.txt does not exist or does NOT contain the string 'allowed', then:
              a) Do NOT collect statistics or upload data

         2. If MEGA_Global\MegaDataCollectPrefs.txt does exist and contains the string 'allowed', then
              a) If the file MEGA_Global\lastDataUpload.txt exists and it has a valid date string, check
                 if number of days between today and that date is greater than some value (originally set to 30).
                   If it is, then:
                    1. Upload data
                    2. Change the date in MEGA_Global\lastDataUpload.txt to today's date
                   Else
                    2. Do NOT try to upload data
              b) If the file MEGA_Global\lastDataUpload.txt does NOT exist:
                    1. Create it
                    2. Write the current date to the file
                    3. Do NOT collect usage statistics or upload data

         3. If MEGA-Global\MegaDataCollectPrefs.txt does exist and contains anything OTHER THAN the string 'allowed'
              a) Do NOT collect statistics or upload data

       From TD_Align.MuscleAlignExecute, or TD_Align.ClustalAlignExecute, or TAnalysisInfo.GetAnalysisOptions, TUsageStatistics.UpdateUsageStatistics will be called
         1. If MEGA_Global\MegaDataCollectPrefs.txt does exist and contains the string 'allowed', then
            a) Update the file MEGA_Global\MEGA-CC_5.1_UsageStatistics.txt with data describing the current analysis


     MEGA-Visual:
       For MEGA-Visual, the user's preference for allowing usage data collection is determined from the preferences menu.
       However, MEGA-Visual will also update the file MEGA-Global\MegaDataCollectPrefs.txt which is used by MEGA-Proto
       and MEGA-CC.

       From TMegaForm.FormCreate, 'CollectUsageStatisticsIfItsTime' will be called
         1. If the 'allow MEGA to collect anonymous usage data' menu item is NOT checked, then:
              a) Do NOT collect statistics or upload data

         2. If the 'allow MEGA to collect anonymous usage data' menu item IS checked, then
              a) If the file MEGA_Global\lastDataUpload.txt exists and it has a valid date string, check
                 if number of days between today and that date is greater than some value (originally set to 30).
                   If it is, then:
                    1. Upload data
                    2. Change the date in MEGA_Global\lastDataUpload.txt to today's date
                   Else
                    2. Do NOT try to upload data
              b) If the file MEGA_Global\lastDataUpload.txt does NOT exist:
                    1. Create it
                    2. Write the current date to the file
                    3. Do NOT collect usage statistics or upload data

         3. If MEGA-Global\MegaDataCollectPrefs.txt does exist and contains anything OTHER THAN the string 'allowed'
              a) Do NOT collect statistics or upload data

       From TAlignEditMainForm.ActionMuscleAlignExecute, or TAlignEditMainForm.ActionClustalAlignExecute,
       or TAnalysisInfo.GetAnalysisOptions, TUsageStatistics.UpdateUsageStatistics will be called
         1. If the 'allow MEGA to collect anonymous usage data' menu item IS checked, then
            a) Update the file MEGA_Global\MEGA5.1_UsageStatistics.txt with data describing the current analysis

     MEGA-Installer:
       The installer for MEGA-Visual allows the user to set their preference for data collection via a checkbox (checked by default).
       When the installer runs, if the user has opted to allow the collection of data, the installer will copy
       a 'Settings.ini' file into 'Private\Ini' and this file will have the data collection preference set to true. Otherwise
       when MEGA-Visual runs and doesn't find 'Private\Ini\Settings.ini', it will extract it from the res file. This extracted
       file has the 'allow MEGA to collect anonymous usage data' preference set to false.

}

interface

uses
  FileUtil, Classes, SysUtils;

const

   { All setting/value pairs are concatenated together with this delimiter in
     between so that we use strings of the form 'setting;;value=count' when
     building our TStringList. Then we can reliably split them apart later, knowing
     the correct delimiter. If this value is changed, the php script on the
     www.megasoftware.net server (/var/www/html/usageStatistics/uploadUsageStatistics.php)
     will need to be updated as well.}
  NameValueSeparator = ';;';


   { When submitting usage data to the www.megasoftware.net server, we will just
     send one string, which is all of the 'setting;;value=count' strings concatenated
     together using ElementSeparator in between each string. The php script on the
     www.megasoftware.net server relies on this delimitor, so if it is ever changed,
     that script (/var/www/html/usageStatistics/uploadUsageStatistics.php) will need to be updated as well. }
  ElementSeparator = '||';

   { This is a list of strings which we want to exclude from the MegaUsageStatistics.txt
     file. Some of these strings don't have an associated value and others we just
     don't want to know (for instance, the name of a tree file that is used as initial tree).
     *** always use lower case for these ***
   }
  BlackList: array[0..10] of AnsiString = ('data_subset_to_use',
                                          'genetic_code',
                                          'rates_and_patterns',
                                          'substitution_model',
                                          'system_resource_usage',
                                          'phylogeny_test',
                                          'data_subset_to_use',
                                          'tree_inference_options',
                                          'initial_tree_file',
                                          'user_tree_file',
                                          'tree_to_use');

type
  /// <summary>Maintains a running count of the number of times that the user
  /// has run any analysis with a given setting/value pair. These values are
  /// saved in a text file in the Private/ini/MegaUsageStatistics.txt file which
  /// is updated every time the user launches a calculation</summary>
  TUsageStatistics = class
    private
      FMegaVersion: AnsiString;
      FBuildNumber: AnsiString;
      FFileName: String;
      FStatistics: TStringList;
      FDataCollectionIsAllowed: Boolean;
      FGuid: AnsiString;
    public
      constructor Create;
      destructor Destroy; override;
      function GetRawString: AnsiString;
      function UpdateUsageStatistics(AnalysisSettings: TStrings): Boolean; overload;
      function UpdateUsageStatistics(Key: AnsiString; Value: AnsiString): Boolean; overload;
      function LoadUsageStatistics: Boolean;
      property FileName: String read FFileName write FFileName;
      property RawString: AnsiString read GetRawString;
      property DataCollectionIsAllowed: Boolean read FDataCollectionIsAllowed write FDataCollectionIsAllowed;
      property GuidString: AnsiString read FGuid write FGuid;
  end;


  /// <summary>Handles reporting of anonymous usage data by submitting a GET request
  /// to the www.megasoftware.net server. If data is submitted successfully, the
  /// local MegaUsageStatistics.txt file is wiped out and data collection begins
  /// anew. If data submission fails, the MegaUsageStatistics.txt file is left
  /// alone and will continue to be updated upon every analysis launch.</summary>
  TStatisticsReporter = class(TThread)
    private
      FUsageStatistics: TUsageStatistics;
      FDataCollectionIsAllowed: Boolean;
    public
      constructor Create(CreateSuspended: Boolean);
      procedure Execute; override;
      function UploadStatistics: Boolean;
      property UsageStatistics: TUsageStatistics read FUsageStatistics write FUsageStatistics;
      property DataCollectionIsAllowed: Boolean read FDataCollectionIsAllowed write FDataCollectionIsAllowed;
  end;

  TOptedInOrOutReporter = class(TThread)

    public constructor Create(CreateSuspended: Boolean);
    procedure Execute; override;
  end;


function ConvertToUsageStatisticsFormat(Source: TStrings; var Target: TStringList): Boolean;
function CompressNameValuePairString(Source: AnsiString): Ansistring;
function IsBlackListed(Source: AnsiString): Boolean;
procedure CollectUsageStatisticsIfItsTime;
procedure ValidateMegaGlobalDirEnvironment;
procedure UploadUserPref;

implementation

uses
  MegaUtils, MegaVerConsts, StringUtils, MegaMainPreferences, DateUtils, fphttpclient,
  MegaConsts {$IFDEF VISUAL_BUILD}, Menus{$ENDIF};

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//                              TUsageStatistics
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
constructor TUsageStatistics.Create;
begin
  FStatistics := TStringList.Create;
  FMegaVersion := ReplaceWhiteSpace('__version=' + VER_MEGA_MAJOR + VER_MEGA_MINOR, '_');
  FBuildNumber := '__build=' + VER_MEGA_BUILD;
  FGuid := GetGuidString;

  FStatistics.Add(FBuildNumber);
  FStatistics.Add('__id=' + FGuid);
  FStatistics.Add(FMegaVersion);

  {$IFDEF RELTIME_ONLY}
  FDataCollectionIsAllowed := False;
  {$ELSE}
  FDataCollectionIsAllowed := CollectUsageDataIsAllowed;
  {$ENDIF}


  if FDataCollectionIsAllowed then
  begin
    FFileName := GetMegaGlobalFile('') + ReplaceWhiteSpace(VER_MEGA_MAJOR + VER_MEGA_MINOR + '_' + VER_MEGA_BUILD, '_') + '_UsageStatistics.txt';
    if not FileExists(FFileName) then
    begin
      FStatistics.SaveToFile(FFileName);
    end;
  end;
end;

/// <summary>Updates the Private/ini/MegaUsageStatistics.txt file with values
/// from the currently running analysis</summary>
function TUsageStatistics.UpdateUsageStatistics(AnalysisSettings: TStrings): Boolean;
var
  i: Integer;
  Value: Integer;
  CleanList: TStringList;
begin
  {$IFDEF MYPEG_ONLY}
    Exit;
  {$ENDIF}

  {$IFDEF RELTIME_ONLY}
    Exit;
  {$ENDIF}
  Result := False;
  if not FDataCollectionIsAllowed then
    Exit;

  CleanList := nil;
  try try
    CleanList := TStringList.Create;
    if not ConvertToUsageStatisticsFormat(AnalysisSettings, CleanList) then
      Exit;

    FStatistics.LoadFromFile(FFileName);
    for i := 0 to CleanList.Count - 1 do
    begin
      if FStatistics.Values[CleanList[i]] = EmptyStr then
        FStatistics.Add(CleanList[i] + '=1')
      else
      begin
        try
          Value := StrToInt(FStatistics.Values[CleanList[i]]);
        Except
          Value := 0;
        end;
        inc(Value);
        FStatistics.Values[CleanList[i]] := IntToStr(Value);
      end;
    end;
    FStatistics.Sort;
    FStatistics.SaveToFile(FFileName);

  Except
    // do nothing, if we lose statistics from some users it is ok, we just want a sample anyway
  end;
  finally
    if CleanList <> nil then
      FreeAndNil(CleanList);
  end;
end;

/// <summary>Updates the Private/ini/MegaUsageStatistics.txt file for the given
/// setting/value pair. This is intened for analyses that don't use the MAnalysisInfo,
/// such as alignments.</summary>
function TUsageStatistics.UpdateUsageStatistics(Key: AnsiString; Value: AnsiString): Boolean;
var
  OptionCount: Integer;
  TempString: AnsiString;
begin
  Result := True;
  {$IFDEF MYPEG_ONLY}
   Exit;
  {$ENDIF}

  {$IFDEF RELTIME_ONLY}
    Exit;
  {$ENDIF}

  if not FDataCollectionIsAllowed then
    Exit;

  try
    FStatistics.LoadFromFile(FFileName);
    TempString := trim(Key) + NameValueSeparator + trim(Value);
    TempString := ReplaceWhiteSpace(TempString, '_');
    if FStatistics.Values[TempString] = EmptyStr then
      FStatistics.Add(TempString + '=1')
    else
    begin
      try
        OptionCount := StrToInt(FStatistics.Values[TempString]);
      Except
        OptionCount := 0;
      end;
      inc(OptionCount);
      FStatistics.Values[TempString] := IntToStr(OptionCount);
    end;
    FStatistics.Sort;
    FStatistics.SaveToFile(FFileName);
    Result := FileExists(FFileName);
  Except
    // do nothing, if we lose statistics from some users it is ok, we just want a sample anyway
  end;
end;

/// <summary>This concatenates all of the lines in the Private/ini/MegaUsageStatistics.txt
/// file so that we can upload the usage data using a GET request and giving it
/// one string. If this is changed, the php script on the www.megasoftware.net server
/// will probably also need to be updated because it requires the data string to be
/// formatted as done below.</summary>
/// <note>The php script that depends on this function is /var/www/html/usageStatistics/uploadUsageStatistics.php</note>
function TUsageStatistics.GetRawString: AnsiString;
var
  i: Integer;
begin
  Result := EmptyStr;

  if LoadUsageStatistics then
  begin
    for i := 0 to FStatistics.Count - 1 do
      Result := Result + FStatistics[i] + ElementSeparator;
  end;
end;


destructor TUsageStatistics.Destroy;
begin
  if Assigned(FStatistics) then
    FStatistics.Free;
  inherited;
end;

/// <summary>Reads the Private/ini/MegaUsageStatistics.txt file</summary>
function TUsageStatistics.LoadUsageStatistics: Boolean;
begin
  Result := False;
  if not FileExists(FFileName) then
    Exit;
  FStatistics.LoadFromFile(FFileName);
  if FStatistics.Count = 0 then
    Exit
  else
    Result := True;
end;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//                          TStatisticsReporter
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

constructor TStatisticsReporter.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FDataCollectionIsAllowed := CollectUsageDataIsAllowed; // get the user's preference
end;

/// <summary>Launches the TStatisticsReporter. If the upload fails, we do nothing.
/// If the upload succeeds, we wipe out the saved usage statistics so that data
/// collection will begin anew</summary>
procedure TStatisticsReporter.Execute;
begin
  if not FDataCollectionIsAllowed then
    Exit;

  try
    if UploadStatistics then
      DeleteFile(UsageStatistics.FileName);
  Except
    // fugidaboudit!!!
  end;
end;

/// <summary>Try and upload the current usage data to the www.megasoftware.net server.
/// Basically, we just make one string from the usage data and submit it via a
/// GET request to a php script on the server.</summary>
function TStatisticsReporter.UploadStatistics: Boolean;
var
  Connection: TFPHTTPClient = nil;
  Response: String;
  TempList: TStringList = nil;
  DateString: String;
  DateFileName: String;
  UrlString: String;
begin
  Result := False;
  try try
    UrlString := 'http://www.megasoftware.net/usageStatistics/uploadUsageStatistics.php?data=' + FUsageStatistics.RawString;
    Connection := TFPHTTPClient.Create(nil);
    Connection.AllowRedirect := True;
    Response := Connection.Get(UrlString);

    // rewrite the file that shows when we last uploaded stats
    DateFileName := GetMegaGlobalFile('') + 'lastDataUpload.txt';
    DateString := DateToStr(Date);
    TempList := TStringList.Create;
    TempList.Add(DateString);
    TempList.SaveToFile(DateFileName);
    Result := True;
  Except
    { eat the exception}
  end;
  finally
    if Assigned(Connection) then
      Connection.Free;
    if Assigned(TempList) then
      FreeAndNil(TempList);
  end;
end;


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//                               Static Stuff
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/// <summary>Takes a TStringList (Source) of analysis settings and concatenates the name/value
///  pairs using Delimiter so that we can convert the name/value pair into just a name.
///  Then, the updated TStringList (Target) is used for counting the number of times
///  an option/setting (name/value) pair has been selected by the user.</summary>

function ConvertToUsageStatisticsFormat(Source: TStrings; var Target: TStringList): Boolean;
var
  i: Integer;
  TempString: AnsiString;
begin
  Result := False;
  Target.Clear;
  for i := 0 to Source.Count - 1 do
  begin
    TempString := CompressNameValuePairString(Source[i]);
    if (TempString <> EmptyStr) and (not IsBlackListed(TempString)) then
      Target.Add(TempString);
  end;
  Result := True;
end;

/// <summary>This function removes unwanted characters from from our source
/// string. Source strings originate from either TProcessPack.TextualSettingsList
/// or from TAnalysisInfo.AnalysisPrefsChosen. In the case of strings from
/// TAnalysisInfo.AnalysisPrefsChosen, the strings are formatted for display in the
/// TRuntimeProgressDlg so we have to do some extra cleanup.</summary>
/// <remarks>This might seem like a shaky way to get our setting/value pairs but
/// it has the advantage of being impervious to changes in analysis options and
/// even to the addition/deletion of analyses. Whatever is changed will always
/// wind up in the TStringLists that provide source strings to this function</remarks>
function CompressNameValuePairString(Source: AnsiString): Ansistring;
var
  CurrentPosition: Integer;
  Delimiter: AnsiString;
  TempString: AnsiString;
  CleanString: String;
begin
  Result := EmptyStr;
  TempString := EmptyStr;
  CleanString := trim(Source);
  if Pos('=', CleanString) <> 0 then
  begin
    Delimiter := '=';
    CurrentPosition := Pos(Delimiter, CleanString);
    while CurrentPosition <> 0 do
    begin
      TempString := trim(copy(CleanString, 1, CurrentPosition - 1));
      if TempString <> EmptyStr then
        Result := Result + trim(TempString) + NameValueSeparator;
      delete(CleanString, 1, CurrentPosition + Length(Delimiter) - 1);
      CurrentPosition := Pos(Delimiter, CleanString);
    end;
    if CleanString <> EmptyStr then
      Result := Result + trim(CleanString);
  end
  else if Pos('--', CleanString) <> 0 then
  begin
    Delimiter := '--';
    CurrentPosition := Pos(Delimiter, CleanString);
    while CurrentPosition <> 0 do
    begin
      TempString := trim(copy(CleanString, 1, CurrentPosition - 1));
      if TempString <> EmptyStr then
        Result := Result + trim(TempString) + NameValueSeparator;
      delete(CleanString, 1, CurrentPosition + Length(Delimiter) - 1);
      while (Length(CleanString) > 0) and (CleanString[1] = '-') do // to get the cases where there are an odd number of '-' characters
        delete(CleanString, 1, 1);
      CurrentPosition := Pos(Delimiter, CleanString);
    end;
    if CleanString <> EmptyStr then
      Result := Result + trim(CleanString);
  end
  else
  begin
    Result := EmptyStr;
    Exit;
  end;

  Result := ReplaceWhiteSpace(Result, '_');
end;

/// <summary>Determines whether or not the given Source string is blacklisted
/// from inclusion in the usage data which ensures that the MegaUsageStatistics.txt
/// file is always small in size</summary>
function IsBlackListed(Source: AnsiString): Boolean;
var
  i: Integer;
  TempString: AnsiString;
begin
  Result := False;
  TempString := LowerCase(trim(Source));
  for i := 0 to Length(BlackList) - 1 do
  begin
    if BeginsWith(TempString,BlackList[i]) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure UploadUserPref;
var
  UploadThread: TOptedInOrOutReporter;
begin
  if FileExists(GetMegaGlobalFile('') + 'prefsSaved.txt') then // if it is already done, don't bother - this is especially important for MEGA-CC
    Exit;

  UploadThread := TOptedInOrOutReporter.Create(True);
  try
    UploadThread.Start;
  except
    //  fugidaboudit
  end;
end;

procedure CollectUsageStatisticsIfItsTime;
var
  Reporter: TStatisticsReporter = nil;
  TodaysDate: TDateTime;
  LastUploadDate: TDateTime;
  FileName: String;
  DateFile: TextFile;
  DateString: AnsiString;
  TempList: TStringList = nil;
  Stats: TUsageStatistics = nil;
  DaysSinceUpdate: Integer;
begin
  {$IFDEF MYPEG_ONLY}
   Exit;
  {$ENDIF}

  {$IFDEF RELTIME_ONLY}
   Exit;
  {$ENDIF}

  {$IFNDEF VISUAL_BUILD}
  Exit;
  {$ENDIF}

  ValidateMegaGlobalDirEnvironment;

  if not CollectUsageDataIsAllowed then
    Exit;

  {$IFDEF DEBUG}
  Exit; // When running in the IDE we don't want to report anything
  {$ENDIF}

  try
    try
      TodaysDate := Date;
      FileName := GetMegaGlobalFile('') + 'lastDataUpload.txt';

      if not FileExists(FileName)  then
      begin
        DateString := DateToStr(TodaysDate);
        AssignFile(DateFile, FileName);
        Rewrite(DateFile);
        WriteLn(DateFile, DateString);
        CloseFile(DateFile);
        Exit;
      end
      else
      begin
        TempList := TStringList.Create;
        TempList.LoadFromFile(FileName);
        if TempList.Count > 0 then
        begin
          DateString := trim(TempList[0]);
          LastUploadDate := StrToDateTime(DateString);
          DaysSinceUpdate := DaysBetween(TodaysDate, LastUploadDate);
          if DaysSinceUpdate > 7 then
          begin
            Reporter := TStatisticsReporter.Create(True);
            Stats := TUsageStatistics.Create;
            Stats.LoadUsageStatistics;
            Reporter.UsageStatistics := Stats;
            Reporter.Start;
          end;
        end;
      end;
    Except
      // just be cool :)
    end;
  Finally
    if TempList <> nil then
      FreeAndNil(TempList);
  end;
end;

procedure ValidateMegaGlobalDirEnvironment;
var
  GlobalDir: String;
  TempFile: TextFile;
  DateString: String;
  TodaysDate: TDateTime;
  FileName: String;
  Guid: TGuid;
  IdString: String;
begin
  GlobalDir := EmptyStr;

  try
    GlobalDir := GetMegaGlobalFile('');

    FileName := GlobalDir + 'MegaDataCollectPrefs.txt';
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

    FileName := GlobalDir + 'lastDataUpload.txt';
    if not FileExists(FileName) then
    begin
      try
        try
          TodaysDate := Date;
          DateString := DateToStr(TodaysDate);
          AssignFile(TempFile, FileName);
          Rewrite(TempFile);
          WriteLn(TempFile, DateString);
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
    // just be cool :)
  end;
end;

{ TOptedInOrOutReporter }

constructor TOptedInOrOutReporter.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TOptedInOrOutReporter.Execute;
const
//  BaseUrl = 'http://127.0.0.1/megasoftware/usageStatistics/';   // for debugging/development
   BaseUrl = 'http://www.megasoftware.net/usageStatistics/';   // livewire!
var
  client: TFPHTTPClient = nil;
  Response: String;
  UrlString: String;
  Guid: String;
  Version: String;
  Major: String;
  Minor: String;
  Os: String;
  Rt: String;
  UsageStats: TUsageStatistics = nil;
  TempList: TStringList = nil;
  Filename: String;
begin
  try try
    TempList := TStringList.Create;
    UsageStats := TUsageStatistics.Create;
    Guid := UsageStats.GuidString;
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
      UrlString := BaseUrl + 'updateDataCollectPrefs.php?pref=optout';
    end
    else
    begin
      FileName := GetMegaGlobalFile('') + 'optedIn.txt';
      if FileExists(FileName) then
      begin
        UrlString := BaseUrl + 'updateDataCollectPrefs.php?pref=optin';
      end
      else // then the file got deleted so replace it according to the user's preference
      begin
        if CollectUsageDataIsAllowed then
        begin
          TempList.Add('ok to collect anonymous usage data');
          TempList.SaveToFile(GetMegaGlobalFile('') + 'optedIn.txt');
          UrlString := BaseUrl + 'updateDataCollectPrefs.php?pref=optin';
        end
        else
        begin
          TempList.Add('not ok to collect anonymous usage data');
          TempList.SaveToFile(GetMegaGlobalFile('') + 'optedOut.txt');
          UrlString := BaseUrl + 'updateDataCollectPrefs.php?pref=optout';
        end;
      end;
    end;

    UrlString := UrlString + '&version=' + Version + '&major=' + Major + '&minor=' + Minor + '&guid=' + Guid + '&rt=' + Rt + '&os=' + Os;
    Client := TFPHTTPClient.Create(nil);
    Client.AllowRedirect := True;
    Response := Client.Get(UrlString);
    TempList.Add('user preference uploaded to server');
    TempList.SaveToFile(GetMegaGlobalFile('') + 'prefsSaved.txt');
  Except
    { purposely eat the exception}
  end;
  finally
    if Assigned(UsageStats) then
      UsageStats.Free;
    if Assigned(client) then
      client.Free;
    if Assigned(TempList) then
      TempList.Free;
  end;
end;

end.

