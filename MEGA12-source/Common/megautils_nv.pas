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

unit MegaUtils_NV;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, LazFileUtils, Forms, SysUtils, MegaConsts,  Classes,
  StdCtrls, controls, Graphics, variants, syncobjs;

type

  { TMEGAThread }

  TMEGAThread = class(TThread)
  protected
    FAInfo: TObject;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FStartMem: Double;
    FEndMem: Double;
    FIsSuccess: Boolean;
    function GetLogText: String;
    procedure Execute; override;
    procedure StartExecute; virtual;
    procedure EndExecute; virtual;
    {$IFNDEF VISUAL_BUILD}
    {$IFNDEF MSWINDOWS}
    procedure UpdatePeakMemUsage;
    {$ENDIF}
    {$ENDIF}
    function AnalysisDescription: String; virtual;
  public
    SkipSummaryUpdate: Boolean;
    MessagesLog: TStringList;
    constructor Create(Suspended:boolean);
    destructor Destroy; override;
    procedure UpdateAnalysisSummary; virtual;
    {$IFDEF DEBUG}
    procedure AddDebugMessage(aMsg: String; e: Exception);
    {$ENDIF}
    property LogText: String read GetLogText;
    property IsSuccess: Boolean read FIsSuccess;
    property Info: TObject read FAInfo write FAInfo;
end;

function GroupsFileHasOutgroup(groupsFile: String): Boolean;
function DumpIntArrayToFile(a: TArrayOfInteger; filename: String): Boolean;
function DumpPIntArrayToFile(a: PArrayOfInt; n: Integer; filename: String): Boolean;
function DumpPArrayOfNodeData(p: PArrayOfNodeData; n: Integer; filename: String): Boolean;
function DirectoryExistsCreate(Directory:String):boolean;
{$IFNDEF VISUAL_BUILD}
function GetPathToDefaultResultsDirectory: String;
function NextAvailableFilenameNV(NewExtension: String): AnsiString;
function GetFileOutputExtension: String;
procedure error_warning_nv(errorwarning: AnsiString; isError : boolean);
procedure warn_NV(warning: AnsiString);
procedure error_nv(error: AnsiString; E: Exception=nil);
{$ENDIF}


function LastPos(SubStr, S: AnsiString): Integer;
function OutputFileTypeToExportType(OutputFileType:TOutputFileType):TExportType;

var
  RunningThreadCount : integer;
  NumActiveThreadsCS: TCriticalSection;

implementation

uses
   {$IFDEF VISUAL_BUILD}Mega_Main, MAnalysisInfo,{$ELSE}MD_MegaMain, MegaVerConsts, mglobalsettings, mmega_std_out, Video,{$ENDIF}
  MegaUtils, Dialogs, MAnalysisSummary, DateUtils, math, StringUtils, mmemutils;

{$IFNDEF VISUAL_BUILD}
function GetPathToDefaultResultsDirectory: String;
var
  PathToDataFile: String;
begin
  PathToDataFile := D_MegaMain.DataFileName;
  Result := ExtractFilePath(PathToDataFile) + 'M' + VER_MEGA_MAJOR_CHAR + 'CC_Out' + PathDelim;
end;

procedure error_warning_nv(errorwarning: AnsiString; isError : boolean);
var
  i: integer;
  LogErrorMessage : String;
  ConsoleMessage: String;
  AppName: String;
  isMapReduce: Boolean = False;
begin
  AppName := 'MEGA-CC';
  errorWarning := StringReplace(Trim(errorWarning), LineEnding, LineEnding + ';', [rfReplaceAll]);
  if IsError then
    ConsoleMessage := AppName + ' has logged the following error:' + LineEnding
  else
    ConsoleMessage := AppName + ' has logged the following warning:' + LineEnding;


  LogErrorMessage  := DateTimeToStr(Now);
  ConsoleMessage := ConsoleMessage + Format('%-15s', ['  When']) + '= ' + DateTimeToStr(Now) + LineEnding;

  LogErrorMessage := LogErrorMessage + ' | ';
  try
    LogErrorMessage := LogErrorMessage + D_MegaMain.DataFileName;
    ConsoleMessage := ConsoleMessage + Format('%-15s',['  Data file']) + '= ' + D_MegaMain.DataFileName + LineEnding;
  except
    on E : Exception do
    begin
    LogErrorMessage := LogErrorMessage + 'Data file not yet opened';
    ConsoleMessage := ConsoleMessage + Format('%-15s', ['  Data file']) + '= not yet opened' + LineEnding;
    end;
  end;
  LogErrorMessage := LogErrorMessage + ' | ';
  try
    LogErrorMessage := LogErrorMessage + D_MegaMain.AnalysisPreferencesFileName;
    ConsoleMessage := ConsoleMessage + Format('%-15s', ['  AnalysisFile']) + '= ' + D_MegaMain.AnalysisPreferencesFileName + LineEnding;
  except
    on E : Exception do
    begin
    LogErrorMessage := LogErrorMessage + 'Analysis file not yet opened';
    ConsoleMessage := ConsoleMessage + Format('%-15s', ['  AnalysisFile']) + '= not yet opened' + LineEnding;
    end;
  end;
  if (D_MegaMain <> nil) and (D_MegaMain.ProcessPack <> nil) and (D_MegaMain.ProcessPack.TreeFile <> EmptyStr) then
  begin
    LogErrorMessage := LogErrorMessage + ' | ';
    try
      LogErrorMessage := LogErrorMessage + D_MegaMain.ProcessPack.TreeFile;
      ConsoleMessage := ConsoleMessage + Format('%-15s', ['  Tree File']) + '= ' + D_MegaMain.ProcessPack.TreeFile + LineEnding;
    except
      on E : Exception do
      begin
      LogErrorMessage := LogErrorMessage + 'Tree file not yet opened';
      ConsoleMessage := ConsoleMessage + Format('%-15s', ['  Tree File']) + '= not yet opened' + LineEnding;
      end;
    end;
  end;
  LogErrorMessage := LogErrorMessage + ' | ';
  i := 1;
  while i <= ParamCount do
  begin
    if (ParamStr(i) = '-d') or (ParamStr(i) = '-a') then
    begin
      inc(i, 2);
      Continue;
    end
    else
    begin
      LogErrorMessage := LogErrorMessage + ' ' + ParamStr(i) + ' ';
      ConsoleMessage := ConsoleMessage + Format('%-15s', ['  Parameter']) + '= ' + ParamStr(i) + LineEnding;
      inc(i);
    end;
  end;

  LogErrorMessage := LogErrorMessage + ' | ';
  if isError then
    LogErrorMessage := LogErrorMessage + 'Error: '
  else
    LogErrorMessage := LogErrorMessage + 'Warning: ';
  LogErrorMessage := LogErrorMessage + errorwarning;

  ConsoleMessage := ConsoleMessage + Format('%-15s', ['  Message']) + '= ' + ErrorWarning + LineEnding;

  {$IFDEF DEBUG}
  if isError and isMapReduce then
  begin
    WriteLn(ConsoleMessage);
    WriteLn('  Please see the summary file for warnings/messages');
  end;
  {$ENDIF}

  if not D_MegaMain.RunSilent then
  begin
    DoTextOut(1, NextProgressLine, ConsoleMessage, Red);   //Write any exception out to the screen
    DoTextOut(1, NextProgressLine, '  Please see the summary file for warnings/messages', Red);
    DoTextOut(1, NextProgressLine, '  Please validate all your inputs and try again. If you think this is a', Red);
    DoTextOut(1, NextProgressLine, '  bug, please report it at ' + WEBSITE_URL, Red);
  end;

  if IsError then
  begin
    D_MegaMain.AnalysisSummary.AddMessage('Error: ' + errorwarning);
    D_MegaMain.AnalysisSummary.WriteToFile(ChangeFileExt(NextAvailableFileNameNV('.txt'), '_summary.txt')); // because the application is about to be halted!
  end
  else
  begin
    D_MegaMain.AnalysisSummary.AddMessage('Warning: ' + errorwarning);
  end;
end;

procedure warn_NV(warning: AnsiString);
begin
  Error_warning_nv(warning, false);
end;

procedure error_nv(error: AnsiString; E: Exception=nil);
{$IFDEF DEBUG}
var
  aList: TStringList = nil;
{$ENDIF}
begin
  {$IFDEF DEBUG} { this only works if debug information is available}
  if Assigned(E) then
  begin
    try
      aList := DumpExceptionCallStack(E);
      Error_warning_nv(aList.Text, false);
    finally
      if Assigned(aList) then
        aList.Free;
    end;
  end;
  {$ENDIF}
  if Assigned(E) and (Pos(E.Message, error) <= 0) then
    Error_warning_nv(Format('%s: %s', [error, E.Message]), True)
  else
    Error_warning_nv(error, true);
  Flush(Output);
  if not D_MegaMain.IsFileIterator then
    Halt(99);
end;


function GetFileOutputExtension:String;
begin
  if D_MegaMain.OutputFormat = ExportExcel then
    Result := '.xls'
  else if D_MegaMain.OutputFormat = ExportCSV then
    Result := '.csv'
  else if D_MegaMain.OutputFormat = ExportText then
    Result := '.txt'
  else if D_MegaMain.OutputFormat = ExportMega then
    Result := '.meg'
  else
    Result := EmptyStr;
end;

function NextAvailableFilenameNV(NewExtension: String): String;
var
  TempDataFileName: String = '';
  TempFileExtension: String = '';
  NewFileIndex: Integer = 0;
  NeedNewFileName: Boolean = False;
  FilenameWithNewExt: String = '';
  CurrentDataFile: String = '';
  ResultsDirectory: String = '';
begin
  CurrentDataFile := D_MegaMain.DataInfoGridGetInfo('Data File'); //Get the full path of the currently activated file
  ResultsDirectory := GetPathToDefaultResultsDirectory;

  if D_MegaMain.OutputFileName <> EmptyStr then  //If we have a user specified file name attempt to use it
  begin
    if (D_MegaMain.OutputFileName[length(D_MegaMain.OutputFileName)] = PathDelim) or DirectoryExists(D_MegaMain.OutputFileName)  then
    begin    //The user specified a directory to save to
      if  D_MegaMain.OutputFileName[length(D_MegaMain.OutputFileName)] <> PathDelim then
        D_MegaMain.OutputFileName := D_MegaMain.OutputFileName + PathDelim;
      if DirectoryExistsCreate(D_MegaMain.OutputFileName) then
        ResultsDirectory := D_MegaMain.OutputFileName;
    end
    else
    begin   //The user specified a file to save to
      if (ExtractFileExt(D_MegaMain.OutputFileName) = EmptyStr) then   //If the user didn't specify an extension, add the proper extension now
        Result := D_MegaMain.OutputFileName + NewExtension
      else
        Result := ChangeFileExt(D_MegaMain.OutputFileName, NewExtension); // otherwise, force the correct file extension
      if FileExistsUTF8(Result) then
      begin
        NewFileIndex := 2;
        NeedNewFileName := True;
        while NeedNewFileName do
        begin
          TempFileExtension := ExtractFileExt(Result);  // because tajima's clock test passes in '_withClock.txt' as a new extension so we can't trust 'NewExtension'
          TempDataFileName := ChangeFileExt(Result, '');
          TempDataFileName := TempDataFileName + '-' + IntToStr(NewFileIndex) + TempFileExtension;
          if FileExistsUTF8(TempDataFileName) then
            inc(NewFileIndex)
          else
            NeedNewFileName := False;
        end;
        Result := TempDataFileName;
        if (Length(Result) > 0) and (Result[1] = '-') then
          Result[1] := '_';
        exit;
      end
      else   //the directory the file is in might not be there check to see if it exists and make it if necessary
      begin
        if DirectoryExistsCreate(ExtractFileDir(D_MegaMain.OutputFileName)) then
        begin
          Result := ChangeFileExt(D_MegaMain.OutputFileName, NewExtension); // GS - we must make sure that the correct file name is used, the user may specify it incorrectly
          exit;
        end;
      end;
    end;
  end;
  NewExtension := '-' + IntToStr(GetCurrentProcessId) + NewExtension;
  FilenameWithNewExt := ExtractFileName(ChangeFileExt(CurrentDataFile, NewExtension));  //i.e. determine filename for new file i.e. activefile.xls where NewExtension is .xls
  if not DirectoryExistsUTF8(ResultsDirectory) then
    MkDir(ResultsDirectory);
  result := NextAvailableFilename(ResultsDirectory + FilenameWithNewExt);
  if (Length(result) > 0) and (result[1] = '-') then
    result[1] := '_';
end;
{$ENDIF}

function OutputFileTypeToExportType(OutputFileType:TOutputFileType):TExportType;
begin
  if OutputFileType = ExportExcel then
    result := ExexcelSave
  else if OutputFileType = ExportCSV then
    result := EXcsvSave
  else if OutputFileType = ExportText then
    result := EXtext
  else if OutputFileType = ExportFasta then
    result := EXfasta
  else
    Result := EXnone;
end;


function LastPos(SubStr, S: AnsiString): Integer;
var
  Found, Len, Pos: integer;
begin
  Pos := Length(S);
  Len := Length(SubStr);
  Found := 0;
  while (Pos > 0) and (Found = 0) do
  begin
    if Copy(S, Pos, Len) = SubStr then
      Found := Pos;
    Dec(Pos);
  end;
  LastPos := Found;
end;

function GroupsFileHasOutgroup(groupsFile: String): Boolean;
var
  aList: TStringList = nil;
  i: Integer = -1;
begin
  Result := False;
  try
    aList := TStringList.Create;
    aList.LoadFromFile(groupsFile);
    if aList.Count > 0 then
      for i := 0 to aList.Count - 1 do
      begin
        if SameText(aList.ValueFromIndex[i], 'outgroup') then
        begin
          Result := True;
          break;
        end;
      end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function DumpIntArrayToFile(a: TArrayOfInteger; filename: String): Boolean;
var
  aFile: TextFile;
  i: Integer;
begin
  try
    try
      AssignFile(aFile, filename);
      Rewrite(aFile);
      if Length(a) > 0 then
        for i := Low(a) to High(a) do
          WriteLn(aFile, IntToStr(a[i]));
      Result := FileExists(filename);
    except
      Result := False;
    end;
  finally
    CloseFile(aFile);
  end;
end;

function DumpPIntArrayToFile(a: PArrayOfInt; n: Integer; filename: String): Boolean;
var
  aFile: TextFile;
  i: Integer;
begin
  try
    try
      AssignFile(aFile, filename);
      Rewrite(aFile);
      if n > 0 then
        for i := 0 to n - 1 do
          WriteLn(aFile, IntToStr(a^[i]));
      Result := FileExists(filename);
    except
      Result := False;
    end;
  finally
    CloseFile(aFile);
  end;
end;

function DumpPArrayOfNodeData(p: PArrayOfNodeData; n: Integer; filename: String): Boolean;
var
  aFile: TextFile;
  i: Integer;
  t: TNodeData;
begin
  try
    try
      AssignFile(aFile, filename);
      Rewrite(aFile);
      WriteLn(aFile, Format('%8s %8s', ['Des1', 'Des2']));
      for i := 0 to n - 1 do
      begin
        t := TNodeData(p^[i]);
        WriteLn(aFile, Format('%8d %8d', [t.des1, t.des2]));
      end;
      Result := FileExists(filename);
    except
      Result := False;
    end;
  finally
    CloseFile(aFile);
  end;
end;

function DirectoryExistsCreate(Directory:String):boolean;
var
  subdirectory : String;
begin
  Result := false;
  if not DirectoryExistS(Directory) then
  begin
    Subdirectory := copy(directory, 0, LastPos(PathDelim,Copy(Directory, 0, length(Directory)-1)));
    if not DirectoryExists(subdirectory) then
      DirectoryExistsCreate(subdirectory);
    try
      mkdir(Directory);
    except
     on E:Exception do
       {$IFDEF VISUAL_BUILD}
       MessageDlg('Unable to create directory: ' + Directory, mtWarning, [mbOK], 0);
       {$ELSE}
       warn_nv('unable to create directory: ' + Directory);
       {$ENDIF}
    end;
  end
  else
    result := true;

  if DirectoryExists(Directory) then  //Make sure it was created correctly just in case
    Result := true;
end;


{ TMEGAThread }


constructor TMEGAThread.Create(Suspended: boolean);
begin
  try
    inherited Create(Suspended);
    SkipSummaryUpdate := False;
    FStartMem := -1;
    FStartTime := MinDateTime;
    FIsSuccess := True;
    MessagesLog := TStringList.Create;
    {$IFNDEF VISUAL_BUILD}
    try
      NumActiveThreadsCS.Acquire;
      RunningThreadCount := RunningThreadCount + 1;
    finally
      NumActiveThreadsCS.Release;
    end;
    {$ENDIF}
  except
    on E:Exception do
    {$IFDEF VISUAL_BUILD}
     ShowMessage('Oh no! An error has occurred: ' + E.Message)
    {$ELSE}
     warn_nv('An error has occurred: ' + E.Message)
    {$ENDIF}
  end;
end;

destructor TMEGAThread.Destroy;
begin
  if Assigned(MessagesLog) then
    MessagesLog.Free;
  inherited Destroy;
  {$IFNDEF VISUAL_BUILD}
  if Assigned(NumActiveThreadsCS) then
  begin
    try
      try
        NumActiveThreadsCS.Acquire;
        RunningThreadCount := RunningThreadCount - 1;
      finally
        NumActiveThreadsCS.Release;
      end;
    except
      //Eat the error
    end;
  end;
  {$ENDIF}
end;

procedure TMEGAThread.UpdateAnalysisSummary;
var
  aSummary: TAnalysisSummary = nil;
  {$IFDEF VISUAL_BUILD}
  aInfo: TAnalysisInfo = nil;
  {$ENDIF}
begin
  if SkipSummaryUpdate then Exit;
  {$IFDEF VISUAL_BUILD}
  if Assigned(FAInfo) then
  begin
    aInfo := TAnalysisInfo(FAInfo);
    if Assigned(aInfo.AnalysisSummary) then
      aSummary := aInfo.AnalysisSummary;
  end;
  {$ELSE}
  aSummary := D_MegaMain.AnalysisSummary;
  {$ENDIF}

  if Assigned(aSummary) then
  begin
    aSummary.AddCalculatedValue(Format('%s Start Time', [AnalysisDescription]), DateTimeToStr(FStartTime));
    aSummary.AddCalculatedValue(Format('%s End Time', [AnalysisDescription]), DateTimeToStr(FEndTime));
    aSummary.AddCalculatedValue(Format('%s Execution Time', [AnalysisDescription]), FormatDoubleSafe(SecondSpan(FEndTime, FStartTime), 3, 8) + ' (seconds)');
    {$IFDEF MSWINDOWS}
    if (FStartMem > 0) and (FEndMem > 0) then
    begin
      if CompareValue(FEndMem, FStartMem, FP_CUTOFF) > 0 then
        aSummary.AddCalculatedValue(Format('%s Memory Used', [AnalysisDescription]), FormatDoubleSafe(FEndMem - FStartMem, 3, 8) + ' (MB)')
      else
        aSummary.AddCalculatedValue(Format('%s Memory Used', [AnalysisDescription]), 'N/A');
    end;
    {$ENDIF}
  end;
end;

{$IFDEF DEBUG}
procedure TMEGAThread.AddDebugMessage(aMsg: String; e: Exception);
var
  aList: TStringList = nil;
begin
  try
    aList := DumpExceptionCallStack(e);
    MessagesLog.Add(aMsg);
    if Assigned(aList) and (aList.Text <> EmptyStr) then
      MessagesLog.Add(aList.Text)
    else
      MessagesLog.Add('unable to get call stack');
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;
{$ENDIF}

function TMEGAThread.GetLogText: String;
begin
  if Assigned(MessagesLog) then
    Result := MessagesLog.Text;
end;

procedure TMEGAThread.Execute;
begin
  inherited;
end;

procedure TMEGAThread.StartExecute;
begin
  { check if start time and start mem are already initialized because to avoid these values being reset after thread construction}
  if SecondSpan(MinDateTime, FStartTime) = 0 then
  begin
    FStartTime := Now;
    FStartMem := GetCurrentProcessMemoryMB;
  end;
end;

procedure TMEGAThread.EndExecute;
begin
 FEndTime := Now;
 FEndMem := GetCurrentProcessMemoryMB;
end;

function TMEGAThread.AnalysisDescription: String;
begin
  Result := 'MEGA Thread';
end;

{$IFNDEF VISUAL_BUILD}
{$IFNDEF MSWINDOWS}
procedure TMEGAThread.UpdatePeakMemUsage;
var
  memUsed: QWord;
  hs: TFPCHeapStatus;
begin
 try
   PeakMemoryCS.Acquire;
   hs := GetFPCHeapStatus;
   memUsed := hs.MaxHeapUsed;
   PeakMemoryUsedByMega := PeakMemoryUsedByMega + memUsed;
 finally
   PeakMemoryCS.Release;
 end;
end;
{$ENDIF}
{$ENDIF}

end.


