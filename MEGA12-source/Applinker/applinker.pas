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

unit AppLinker;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Process, LCLIntF, LCLType, laz2_XMLRead, laz2_DOM, SysUtils, mruntimeprogressdlg,
  Variants, Classes, Graphics, Controls, Forms, MegaUtils_NV,
  MD_Sequences, MegaUtils, KeywordConsts, UTF8Process,
  MAnalysisInfo, MegaConsts, Pipes{$IFDEF VISUAL_BUILD}, AppOptionsDlg, Menus, mimage_viewer{$ENDIF};

type
  AppError = (noError, noOutput, notAbleToSave, notFinished, notEnoughMemory, notEnoughMemorySpecified, noLogFile, UnknownErr, FatalException, UnknownOption);  // notAbleToSave is a MUSCLE specific error.
  TNewLineCheckCancelFunc = function (NewLine: String): Boolean of object;
  TUpdateStatusProc = procedure (StatusType: String; StatusInfo: String) of object;
  TShowAnalysisOptionsProc = procedure(aOptions: TStringList; showOptions: Boolean = True) of object;

  TAppLink = class;

  TLinkDoneThread = class(TThread)
  private
    procedure Finished;
  public
    AppLink : TAppLink;
    ProcessHandle : Cardinal;
    {$IFDEF MSWINDOWS}
    ProcessInfo: PROCESS_INFORMATION;
    {$ENDIF}
    constructor Create(CreateSuspended: Boolean);
  protected
    procedure Execute; override;
  end;

  TAppLinkerFinished = procedure(AppLink : TAppLink);

  { TAppLink }

  TAppLink = class abstract (TObject)
  private
    FStartTime: TDateTime;
  protected
    FCommandLine: String;
    FExe: UTF8String;
    FMemStream: TMemoryStream;
    LinkFinished : TLinkDoneThread;
    FApp_HC: Integer;
    FOwner: TComponent;
    DNA: Boolean;
    cDNA: Boolean;
    Protein: Boolean;
    stdOutEnd, stdErrEnd: TStringList;
    FNewLine: String;
    function SaveCommandLine(aProcess: TProcess): String;
    procedure OptionsFinalize; virtual; abstract;
    procedure RunAppLink; virtual;
    procedure StopAppLink;
    procedure OnServerPipeMessage(Sender: TObject; Pipe: Cardinal; Stream: TStream);
    procedure OnNewStatusMessage(Sender: TObject; Pipe: NativeUInt; Stream: TStream);
    function ComputeCmdArgs(KeyValues: TStrings): AnsiString; overload; virtual;
    function ComputeCmdArgs(KeyValues: TStrings; var Args: TStringList): Boolean; overload; virtual;
    function GetCmdFromSMAKey(Root: TDOMNode; SMAkey: AnsiString; SMAvalue : AnsiString): AnsiString;
  public
    NewLineCheckCancelFunc: TNewLineCheckCancelFunc;
    NewLineProc: TNewLineProc;
    UpdateStatusProc: TUpdateStatusProc;
    UpdateProgressProc: TUpdateProgressProc;
    ShowAnalysisOptionsProc: TShowAnalysisOptionsProc;
    Process: TProcessUTF8;
    MyLinkName: String;
    ExecDone: Boolean;
    WorkDir: PAnsiChar;
    redirectStdOut: Boolean;
    readOutputsFromFile: Boolean;
    DontAskForCalcOptions: Boolean; // Determines whether we are reading the options from an XML file or they are already selected and we are just executing an exe with predefined arguments.
    BaseCommand: AnsiString; // This is the command which the flags are added onto in the case where we just directly use the options selected as flags.  ex. base= muscle.exe -in selectedData.txt -out muscledData.txt
    OutputFilename: String;
    ProcessID : Integer;
    FullCmdLineString : AnsiString;
    LogFilename: String;
    OnTerm : TAppLinkerFinished;
    UserAborted: Boolean;
    ErrorCode: AppError;
    AppExitCode: Integer;
    LastCmdLineUpdateLine: Integer; // When using the timer, we keep track of the line number the last update ended on, so we can look at only the new lines for warnings
    {$IFDEF VISUAL_BUILD}
    AppOptionsDlg: TAppOptions;
    {$ENDIF}
    OptionsFile: String;
    TempDir: String;
    MAI: TAnalysisInfo;
    ErrorsReported: TStringList; // Any errors are added to this list, duplicate errors are discarded.
    Paused: Boolean;
    function ShowAppOptions(Modal: Boolean = true): Boolean;
    procedure Execute;
    destructor Destroy; override;
    function TerminateProcess: Boolean;
    constructor Create(Owner: TComponent);
    function GetOptions: TStrings; virtual;
    procedure AppTerminated(Sender: TObject; ExitCode: Cardinal);
    procedure AppNewLine(Sender: TObject; NewLine: AnsiString); virtual;
    procedure UseUpdateTimer(UseTimer: Boolean = true);
    procedure UpdateTimer(Sender: TObject);
    procedure UpdateStatus(NewLine: AnsiString); virtual; abstract;
    procedure DisplayAppCmdLine;
    procedure setDNA(adna: Boolean);
    procedure stdOut(sl: TStringList);
    procedure stdErr(sl: TStringList);
    function DataType: TSnTokenCode;
  published
    property IsDNA: Boolean read DNA write setDNA;
    property IscDNA: Boolean read cDNA write cDNA;
    property IsProtein: Boolean read Protein write Protein;
    property App_HC: Integer read FApp_HC write FApp_HC;
    property StartTime: TDateTime read FStartTime;
    property CommandLine: String read FCommandLine;
  end;

  procedure OnTermPlacholder(AppLink: TAppLink);
var
  AppLink: TAppLink;

type

  { TAppLinkThread }

  TAppLinkThread = class abstract(TMegaThread)
    protected
      FDoingCheckCancel: Boolean;
      FIsCancelled: Boolean;
      FNewLine: String;
      FStatusType: String;
      FStatusInfo: String;
      FProgress: Integer;
      FAnalysisOptions: TStringList;
      procedure DoNewLine; virtual;
      procedure ProgressNewLine(NewLine: String); virtual;
      procedure UpdateStatusInfo(StatusType: String; StatusInfo: String); virtual;
      procedure UpdateProgress(aProgress: Integer); virtual;
      procedure ShowAnalysisOptions(aOptions: TStringList; showOptions: Boolean = True); virtual;
      procedure DoUpdateStatusInfo; virtual;
      procedure DoUpdateProgress; virtual;
      procedure DoShowAnalysisOptions; virtual;
      function AppLinkerNewLineCheckCancel(NewLine: String): Boolean; virtual;
      procedure DoNewLineCheckCancel; virtual;
      {$IFDEF MSWINDOWS}
      function FindProcessByParentID(const AParentID: DWORD): DWORD;
      function KillProcess(const AProcessID: DWORD): Boolean;
      {$ENDIF}
    public
      ApplinkerProcess: TProcessUTF8;
      UpdateStatusProc: TUpdateStatusProc;
      UpdateProgressProc: TUpdateProgressProc;
      NewLineProc: TNewLineProc;
      NewLineCheckCancelFunc: TNewLineCheckCancelFunc;
      ShowAnalysisOptionsProc: TShowAnalysisOptionsProc;
      constructor Create(createSuspended: Boolean);
      destructor Destroy; override;
      function TerminateAppLinkProcess: Boolean; virtual;
      property IsCancelled: Boolean read FIsCancelled;
  end;

  { TMuscleLink }

  TMuscleLink = class(TAppLink)
  private
    FFeedback: TStringList;
    FWarnings: TStringList;
    function GetWarningMessages: String;
    function ParseMuscleFeedback(const Feedback: String; var PercMemRem: Integer; var MemUsed: Integer; var Iteration: Integer; var Progress: Double; var aTask: String): Boolean;
    procedure ShowAnalysisInfo;
    function IsShellMode: Boolean;
    procedure UpdateXSites;
  protected
    function ComputeCmdArgs(KeyValues: TStrings; var Args: TStringList): Boolean; overload; override;
    procedure RunAppLink; override;
  public
    ParentThread: TAppLinkThread;
    XSites: AnsiString;
    SeqList: TSequenceList;
    MuscleOptions: TStringList;
    InputFileName : String;
    ShowAnalysisOptionsProc: TShowAnalysisOptionsProc;
    procedure AddWarning(warning: String);
    function GetOptions: TStrings; override;
    function GetXSites: AnsiString;
    procedure OptionsFinalize; override;
    procedure OptionsFinalizeGuiMode;
    Constructor Create(Owner: TComponent);
    destructor Destroy; override;
    procedure UpdateStatus(NewLine: AnsiString); override;

    procedure CheckUnhandledError;
    function ErrorCodeMessage: String;
    procedure SetSequenceData(var aSeqList : TSequenceList);
    procedure Execute;
    property WarningMessages: String read GetWarningMessages;
  end;

  { TMuscleLinkThread }

  TMuscleLinkThread = class(TAppLinkThread)
    private
      FErrorMsg: String;
      FErrorClass: String;
      FMuscleLink: TMuscleLink;
      function RunTargetApplication: Boolean;
      procedure DoSynchronizeErrorMessage;
    public
      constructor Create(CreateSuspended: Boolean);
      destructor Destroy; override;
      procedure Execute; override;
      function UserAborted: Boolean;
      procedure SynchronizeErrorMessage(E: Exception);
      function TerminateAppLinkProcess: Boolean; override;
      property IsSuccess: Boolean read FIsSuccess;
      property MuscleLink: TMuscleLink read FMuscleLink write FMuscleLink;
  end;


type
  TUnzipLink = class(TAppLink)
  public
    filename: String;
    outputDirectory: AnsiString;
    Constructor Create(Owner: TComponent);
    procedure Execute;
  end;

  function LaunchMuscleLinkThread(MuscleLink: TMuscleLink; MAI: TAnalysisInfo; aOnTerminate: TNotifyEvent; var IsSuccess: Boolean; var aMsg: String): TMuscleLinkThread;

var
  EslAppLinksList: TList;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  MAlignEditMainForm, mega_main, MEditorForm, MTreeViewForm, Dialogs, uMegaBrowser,
  {$ELSE}
   ProcessCodonOmegaCmds,
  {$ENDIF}
  {$IFDEF MSWINDOWS} JwaTlHelp32,  {$ENDIF}
  MegaErrUtils,  AlnBuilder_HC, MLegendGenerator, StringUtils,
  MDistPack, MProcessPack, FileUtil, LazFileUtils;

procedure OnTermPlacholder(AppLink: TAppLink);
begin
  // Does nothing.
  // This is used when we don't have an onTerm function we want to run.
  // If no function is assigned to onTerm you will get an access violation.
end;

function LaunchMuscleLinkThread(MuscleLink: TMuscleLink; MAI: TAnalysisInfo; aOnTerminate: TNotifyEvent; var isSuccess: Boolean; var aMsg: String): TMuscleLinkThread;
var
  MuscleExeFile: String;
begin
  Result := nil;
  try
    MuscleExeFile := GetMuscleExe;
    if not FileExists(MuscleExeFile) then
      raise Exception.Create('MEGA installation appears to be corrupted; critical MUSCLE files are missing. Please reinstall MEGA.');

    Result := TMuscleLinkThread.Create(True);
    Result.OnTerminate := aOnTerminate;
    Result.MuscleLink := MuscleLink;
    MuscleLink.MAI := MAI;
    MAI.ARP.StdOutLabelCaption := 'Executing MUSCLE sequence alignment...';
    Result.UpdateProgressProc := MAI.ARP.UpdatePercentProgress;
    Result.UpdateStatusProc := MAI.ARP.UpdateRunStatusInfo;
    Result.NewLineProc := MAI.ARP.AddCommandLine;
    Result.NewLineCheckCancelFunc := MAI.ARP.AddCommandLineCheckCancel;
    Result.ShowAnalysisOptionsProc := MAI.ARP.AddAnalysisOptions;
    {$IFDEF VISUAL_BUILD}
    MAI.ARP.UseTimer := True;
    MAI.ARP.TimerInterval := 1000;
    MAI.ARP.SetKeepOnTop(True);
    MAI.ARP.Thread := Result;
    MAI.ARP.Show;
    {$ENDIF}
    isSuccess := True;
    Result.Start;
  except
    on E:Exception do
    begin
      isSuccess := False;
      aMsg := E.Message;
    end;
  end;
end;

{ TAppLinkThread }

procedure TAppLinkThread.DoNewLine;
begin
  if Assigned(NewLineProc) then
    NewLineProc(FNewline);
end;

procedure TAppLinkThread.ProgressNewLine(NewLine: String);
begin
  FNewLine := NewLine;
  Synchronize(DoNewLine);
end;


procedure TAppLinkThread.UpdateStatusInfo(StatusType: String; StatusInfo: String);
begin
  FStatusType := StatusType;
  FStatusInfo := StatusInfo;
  Synchronize(DoUpdateStatusInfo);
end;

procedure TAppLinkThread.UpdateProgress(aProgress: Integer);
begin
  FProgress := aProgress;
  Synchronize(DoUpdateProgress);
end;

procedure TAppLinkThread.ShowAnalysisOptions(aOptions: TStringList; showOptions: Boolean = True);
begin
  FAnalysisOptions := aOptions;
  Synchronize(DoShowAnalysisOptions);
end;

procedure TAppLinkThread.DoUpdateStatusInfo;
begin
  if Assigned(UpdateStatusProc) then
    UpdateStatusProc(FStatusType, FStatusInfo);
end;

procedure TAppLinkThread.DoUpdateProgress;
begin
  if Assigned(UpdateProgressProc) then
    UpdateProgressProc(FProgress);
end;

procedure TAppLinkThread.DoShowAnalysisOptions;
begin
  if Assigned(ShowAnalysisOptionsProc) then
    ShowAnalysisOptionsProc(FAnalysisOptions, False);
end;

function TAppLinkThread.AppLinkerNewLineCheckCancel(NewLine: String): Boolean;
begin
  if FDoingCheckCancel then
    Exit(False);
  try
    FDoingCheckCancel := True;
    FNewLine := NewLine;
    Synchronize(DoNewLineCheckCancel);
    Result := FIsCancelled;
  finally
    FDoingCheckCancel := False;
  end;
end;

procedure TAppLinkThread.DoNewLineCheckCancel;
var
  aCancelled: Boolean = False;
begin
  if Assigned(NewLineCheckCancelFunc) then
  begin
    aCancelled := NewLineCheckCancelFunc(FNewLine);
    if not FIsCancelled then { if the user has cancelled, do not let timing issues reset FIsCancelled}
      FIsCancelled := aCancelled;
  end;
end;

{$IFDEF MSWINDOWS}
function TAppLinkThread.FindProcessByParentID(const AParentID: DWORD): DWORD;
var
  Proc: TPROCESSENTRY32;
  hSnap: HWND;
  Looper: BOOL;
begin
  Result := 0;
  Proc.dwSize := SizeOf(Proc);
  hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);
  Looper := Process32First(hSnap, Proc);
  while Integer(Looper) <> 0 do
  begin
    if Proc.th32ParentProcessID = AParentID then
    begin
      Result := Proc.th32ProcessID;
      Break;
    end;
    Looper := Process32Next(hSnap, Proc);
  end;
  CloseHandle(hSnap);
end;

function TAppLinkThread.KillProcess(const AProcessID: DWORD): Boolean;
var
  ProcHandle: THandle;
begin
  Result := False;
  ProcHandle := OpenProcess(PROCESS_TERMINATE, False, AProcessID);
  if ProcHandle > 0 then
  try
    Result := TerminateProcess(ProcHandle, 0);
  finally
    CloseHandle(ProcHandle);
  end;
end;
{$ENDIF}

constructor TAppLinkThread.Create(createSuspended: Boolean);
begin
  inherited Create(createSuspended);
  FDoingCheckCancel := False;
end;

destructor TAppLinkThread.Destroy;
begin
  inherited Destroy;
end;

function TAppLinkThread.TerminateAppLinkProcess: Boolean;
{$IFDEF MSWINDOWS}
var
  childProcId: DWord;
{$ENDIF}
begin
  Result := False;
  if not Assigned(ApplinkerProcess) then
    Exit;

  FIsCancelled := True;

  {$IFDEF MSWINDOWS}
  childProcId := FindProcessByParentID(ApplinkerProcess.ProcessID);
  while childProcId > 0 do
  begin
    KillProcess(childProcId);
    childProcId := FindProcessByParentID(ApplinkerProcess.ProcessID); { ApplinkerProcess can be parent to multiple child process. This is true when running MyESL.exe}
  end;
  {$ENDIF}

  if Assigned(ApplinkerProcess) and ApplinkerProcess.Active then
    Result := ApplinkerProcess.Terminate(USER_ABORTED);
end;

{ TMuscleLinkThread }

function TMuscleLinkThread.RunTargetApplication: Boolean;
begin
  Result := False;
  try
    FMuscleLink.NewLineProc := ProgressNewLine;
    FMuscleLink.NewLineCheckCancelFunc := AppLinkerNewLineCheckCancel;
    FMuscleLink.UpdateStatusProc := UpdateStatusInfo;
    FMuscleLink.UpdateProgressProc := UpdateProgress;
    FMuscleLink.ShowAnalysisOptionsProc := ShowAnalysisOptions;
    FMuscleLink.ParentThread := Self;
    FMuscleLink.Execute;
    FMuscleLink.ParentThread := nil;
    if FMuscleLink.ErrorCode <> noError then
    begin
      Result := False;
      MessagesLog.Add('MUSCLE failed with error: ' + FMuscleLink.ErrorCodeMessage);
    end
    else
      Result := True;
  except
    on E:Exception do
    begin
      MessagesLog.Add('Failed to run MUSCLE: ' + E.Message);
      FIsSuccess := False;
    end;
  end;
end;

procedure TMuscleLinkThread.DoSynchronizeErrorMessage;
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FMuscleLink.MAI) and Assigned(FMuscleLink.MAI.ARP) then
    FMuscleLink.MAI.ARP.Hide;
  ShowMessage(Format('Error (%s) in MUSCLE align thread: %s', [FErrorClass, FErrorMsg]));
  {$ELSE}
  error_nv(Format('Error (%s) in MUSCLE align thread: %s', [FErrorClass, FErrorMsg]));
  {$ENDIF}
end;

constructor TMuscleLinkThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  ApplinkerProcess := nil;
  FIsSuccess := False;
  FIsCancelled := False;
  FreeOnTerminate := True;
  NewLineProc := nil;
  NewLineCheckCancelFunc := nil;
  UpdateStatusProc := nil;
  UpdateProgressProc := nil;
end;

destructor TMuscleLinkThread.Destroy;
begin
  if Assigned(FMuscleLink) then
    FMuscleLink.Free;
  inherited Destroy;
end;

procedure TMuscleLinkThread.Execute;
begin
  if not Assigned(FMuscleLink) then
  begin
    FIsSuccess := False;
    MessagesLog.Add('MUSCLE application linker not initialized');
    Terminate;
  end
  else
    FIsSuccess := RunTargetApplication;
end;

function TMuscleLinkThread.UserAborted: Boolean;
begin
  Result := FMuscleLink.UserAborted;
end;

procedure TMuscleLinkThread.SynchronizeErrorMessage(E: Exception);
begin
  FErrorMsg := E.Message;
  FErrorClass := E.ClassName;
  Synchronize(DoSynchronizeErrorMessage);
end;

function TMuscleLinkThread.TerminateAppLinkProcess: Boolean;
begin
  Result := inherited TerminateAppLinkProcess;
  FMuscleLink.UserAborted := True;
end;


{ TAppLink }

destructor TAppLink.Destroy;
begin
  if Assigned(ErrorsReported) then
    ErrorsReported.Free;
  {$IFDEF VISUAL_BUILD}
  if Assigned(AppOptionsDlg) then
    FreeAndNil(AppOptionsDlg);
  {$ELSE}
  try
    NumActiveThreadsCS.Acquire;
    RunningThreadCount := RunningThreadCount - 1;
  finally
    NumActiveThreadsCS.Release;
  end;
  {$ENDIF}
  inherited;
end;

function TAppLink.TerminateProcess: Boolean;
begin
  StopAppLink;
  Result := not Process.Running;
end;

constructor TAppLink.Create(Owner: TComponent);
begin
  Process := TProcessUTF8.Create(nil);
  FMemStream := nil;
  UserAborted := False;
  NewLineCheckCancelFunc := nil;
  NewLineProc := nil;
  UpdateStatusProc := nil;
  UpdateProgressProc := nil;
  AppExitCode := -1;
  Paused := False;
  MyLinkName := 'TAppLink';
  FApp_HC := 3051; // in case we mess something up, take the user to the help index, TODO - GS, replace this number with a valid reference to a constant
  onTerm := OnTermPlacholder;
  {$IFDEF VISUAL_BUILD}
  AppOptionsDlg := nil;
  {$ENDIF}
  TempDir := GetTempDir(False);
  try
    ErrorsReported := TStringList.Create;
    ErrorsReported.Duplicates := dupError; // Ignore duplicate errors
    FOwner := Owner;
    {$IFDEF VISUAL_BUILD}
    if not DontAskForCalcOptions then
      AppOptionsDlg := TAppOptions.Create(Owner);
    {$ELSE}
    try
      NumActiveThreadsCS.Acquire;
      RunningThreadCount := RunningThreadCount + 1;
    finally
      NumActiveThreadsCS.Release;
    end;
    {$ENDIF}
  except
    on E: Exception do
    begin
      ErrorsReported.Add(E.Message + 'Aborting Application Linker operation!');
      {$IFDEF VISUAL_BUILD}
      if Assigned(AppOptionsDlg) then
        FreeAndNil(AppOptionsDlg);
      {$ELSE}
      error_nv(E.Message + ' Aborting Application Linker Operation', E);
      {$ENDIF}
    end;
  end;
end;

function TAppLink.ShowAppOptions(Modal: Boolean): Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := False;
  if (not DontAskForCalcOptions) and (AppOptionsDlg <> nil) then
  begin
    if not FileExists(OptionsFile) then
      raise Exception.Create('A required internal config file for Application Linker is missing (' + OptionsFile + ').');
    AppOptionsDlg.HelpContext := App_HC;
    AppOptionsDlg.LoadJson(OptionsFile, DataType);
    AppOptionsDlg.isDNA := self.IsDNA;
    AppOptionsDlg.iscDNA := self.IscDNA;
    if Modal then
    begin
      if AppOptionsDlg.ShowModal <> mrOK then
       begin
         Result := False;
         Exit;
       end;
    end
    else
      AppOptionsDlg.Show;
  end;
  {$ENDIF}
  Result := True;
end;

procedure TAppLink.AppTerminated(Sender: TObject; ExitCode: Cardinal);
begin
  ExecDone := True;
end;

{Gets the command parameter name for the given command option along
 with the given parameter value appended to it. Used for building up a command
 line string with the user's chosen options.}
function TAppLink.GetCmdFromSMAKey(Root: TDOMNode; SMAkey: AnsiString; SMAvalue : AnsiString): AnsiString;
var
  k: Integer;
  Node: TDOMNode;

  function HasAttribute(Attr: AnsiString): Integer;
  var
    j: Integer;
  begin
    Result := -1;
    for j := 0 to Root.Attributes.Length - 1 do
    begin
      if SameText(Attr, Trim(Root.Attributes[j].NodeName)) then
      begin
        Result := j;
        Exit;
      end;
    end;
  end;

begin
  Result := EmptyStr;
  if not Assigned(Root) then  //If we have no starting location to search we can do nothing
    exit;

  if Root.HasAttributes then
  begin
    k := HasAttribute('name');
    if (k >= 0) and (trim(Root.Attributes[k].NodeValue) = SMAkey) then //If the node we are looking at has the key defined in it  i.e. Presets == Presets
    begin
      k := HasAttribute('cmd');
      if (k >= 0) then  //If it has an associated command return it  i.e. <double name="Gap Open" cmd="-gaopen">-12</double>  returns -gaopen
      begin
        Result := Root.Attributes[k].NodeValue;
        if (Root.NodeName = 'integer') or (Root.NodeName = 'double') then
          Result := Result + '=' + SMAvalue;
        exit;
      end
      else if Root.HasChildNodes then   //Its value must be in a child node
      begin
        Node := Root.FirstChild;
        while Assigned(Node) do
        begin
          Result := GetCmdFromSMAKey(Node, SMAKey, SMAValue);
          if Result <> EmptyStr then
            Exit;
          Node := Node.NextSibling;
        end;
      end
    end
    else if Root.HasChildNodes then   //Its value must be in a child node
      begin
        Node := Root.FirstChild;
        while Assigned(Node) do
        begin
          Result := GetCmdFromSMAKey(Node, SMAKey, SMAValue);
          if Result <> EmptyStr then
            Exit;
          Node := Node.NextSibling;
        end;
      end
  end;
end;

/// <summary>Parses the appropriate Private/MUSCLE/Muscle_*_.txt xml file and builds up the cmdargs string</summary>
/// <remarks>Although we could build up this portion of the full command string without using the
/// xml file, this approach gives us extendibility for future linked-in applications.</remark>
function TAppLink.ComputeCmdArgs(KeyValues : TStrings): AnsiString;
begin
  Result := EmptyStr;
end;

function TAppLink.ComputeCmdArgs(KeyValues: TStrings; var Args: TStringList): Boolean;
var
  Doc: TXMLDocument;
  Node: TDOMNode;
  i: Integer;
  Value: String;
begin
  Result := False;
  Doc := nil;
  try
    try
      if not FileExists(OptionsFile) then
        raise Exception.Create('Missing config file for applinker: ' + OptionsFile);
      ReadXMLFile(Doc, OptionsFile);
      Args.Clear;
      if KeyValues.Count > 0 then
      begin
        Node := Doc.DocumentElement;
        for i := 0 to KeyValues.Count - 1 do
        begin
          Value := GetCmdFromSMAkey(Node, KeyValues.Names[i], KeyValues.Values[KeyValues.Names[i]]);
          if Value <> EmptyStr then
            Args.Add(Value);
        end;
        Result := True;
      end;
    except
      on E:Exception do
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
      {$ELSE}
      error_nv('Oh no! An error has occurred: ' + E.Message, E);
      {$ENDIF}
    end;
  finally
    if Assigned(Doc) then
      Doc.Free;
  end;
end;

function TAppLink.GetOptions: TStrings;
begin
  {$IFDEF VISUAL_BUILD}
  Result := nil;
  {$ELSE}
   Result := MAI.MyProcessPack.TextualSettingsList;
  {$ENDIF}
end;

procedure TAppLink.Execute;
var
  CmdsFromAppOptions: AnsiString;
  {$IFNDEF VISUAL_BUILD}Options: TStrings = nil;{$ENDIF}
begin
  FStartTime := Now;
  {$IFNDEF VISUAL_BUILD}
  FullCmdLineString := EmptyStr;
  {$ENDIF}
  try
    if FileExists(OutputFilename) then
      DeleteFile(OutputFilename);
    if FullCmdLineString <> EmptyStr then
      RunAppLink
    else if ShowAppOptions then
    begin
      try
        OptionsFinalize;
        if UserAborted then
          Exit;
        {$IFDEF VISUAL_BUILD}
        if AppOptionsDlg <> nil then
          CmdsFromAppOptions := AppOptionsDlg.cmdargs;
  	{$ELSE}
        if MyLinkName <> 'THyPhyLink' then
        begin
          Options := GetOptions;
          CmdsFromAppOptions := ComputeCmdArgs(Options);  // For HyPhy this isn't needed.  I'm pretty sure we can just comment out this line.
        end;
  	{$ENDIF}
        FullCmdLineString := BaseCommand + ' ' + CmdsFromAppOptions;
        RunAppLink;
      finally
      {$IFDEF VISUAL_BUILD}
        if Assigned(AppOptionsDlg) then
          FreeAndNil(AppOptionsDlg);
       {$ENDIF}
      end;
    end
    else
      UserAborted := true;
  except
    on E: Exception do
      ShowErrorMessage(Exception.Create('Error in the Application Linker during the Execute command. Error is: ' + E.Message));
  end;
end;

procedure TAppLink.AppNewLine(Sender: TObject; NewLine: AnsiString);
var
  isCancelled: Boolean;
begin
  isCancelled := False;
  FNewLine := NewLine;
  if Assigned(NewLineCheckCancelFunc) then
    isCancelled := NewLineCheckCancelFunc(FNewLine);
  if isCancelled then
  begin
   UserAborted := True;
   StopAppLink;
   Exit;
  end;
  UpdateStatus(NewLine);
end;

procedure TAppLink.UseUpdateTimer(UseTimer: Boolean); // True makes updates show in the command line output by adding the last stdout line every X seconds.  False adds each line (this can slow down computation significantly!)
begin
  {$IFDEF VISUAL_BUILD}
  raise Exception.Create('UseUpdateTimer is deprecated. Instances of TAppLink should be run in a separate thread so updates must be done with calls to Synchronize');
  {$ENDIF}
end;


procedure TAppLink.UpdateTimer(Sender: TObject);
begin

end;


procedure TAppLink.DisplayAppCmdLine;  // used for debugging when there is an error we can not recover from.
begin
  //OpenStringList(App.Lines, 'StdOut Direct Output');    //NRP show command line output
end;

procedure TAppLink.OnNewStatusMessage(Sender: TObject; Pipe: NativeUInt; Stream: TStream);
var
  S : AnsiString = '';
begin
  if Assigned(Stream) then
  begin
    SetLength(S, Stream.Size);
    if Length(S) > 0 then
    begin
      Stream.Read(S[1], Length(S));
      if (s <> EmptyStr)  then
        AppNewLine(nil, S);
    end;
  end;
end;

function TAppLink.SaveCommandLine(aProcess: TProcess): String;
var
  params: String = '';
begin
  params := aProcess.Parameters.Text;
  params := StringReplace(params, LineEnding, ' ', [rfReplaceAll]);
  FCommandLine := Format('%s %s', [aProcess.Executable, params]);
  Result := FCommandLine;
end;

procedure TAppLink.RunAppLink;
const
  READ_BYTES = 2048;
var
  NumBytes: LongInt;
  BytesRead: LongInt;
  Args: TStringList = nil;
  Options: TStrings = nil;
  i: Integer;
  {$IFDEF DARWIN}
  Buffer: array[1..2048] of byte;
  OutputString: String = '';
  {$ELSE}
  Buffer: AnsiString = '';
  {$ENDIF}
  APath: AnsiStrinG = '';
  UserDir: String;
begin
  try
    try
      UserDir := GetCurrentDirUTF8;
      SetCurrentDirUTF8(ExtractFileDir(FExe));
      FMemStream := TMemoryStream.Create;
      BytesRead := 0;
      Process.Executable := UTF8Encode(FExe);
      APath := ExtractFilePath(FExe);
      Delete(APath, Length(APath), 1);
      Process.Name:=ExtractFileNameOnly(FExe);
      Process.CurrentDirectory:=ExtractFileDir(FExe);
      Process.CurrentDirectory := APath;
      Args := TStringList.Create;
      Options := GetOptions;
      if MyLinkName = 'THyPhyLink' then
      begin
        Process.Parameters.Add(BaseCommand);
      end
      else if MyLinkName = 'TIQTREELink' then
      begin
        //take in command line that was already formatted
      end
      else
      begin
        if not ComputeCmdArgs(Options, Args) then
          raise Exception.Create('unable to generate command-line arguments');
        if Args.Count > 0 then
        begin
          for i := 0 to Args.Count - 1 do
          begin
            Process.Parameters.Add(Args.Names[i]);
            if Trim(Args.Values[Args.Names[i]]) <> EmptyStr then
              Process.Parameters.Add(Args.Values[Args.Names[i]]);
          end;
        end;
      end;
      Process.Options := [poUsePipes, poStdErrToOutPut, poNoConsole];
      FCommandLine := SaveCommandLine(Process);
      Process.Execute;
      SetCurrentDirUTF8(UserDir);

      {$IFNDEF DARWIN}
      {
         the code in the next block for macOS does not work for MUSCLE in megacc (at least on Windows)
         because there is some kind of timing issue with the MUSCLE log file which
         needs to be read and checked for warning and error messages. The log file is often
         not found until much later after Process.Running is false. The code in this block
         runs more reliably but still is not a solution to the late appearance of the log file.
         If the log file is not found in TMuscleLink.CheckUnhandledError then an error is raised
         and alignment fails, although some small delay is added in that function.
      }
      SetLength(Buffer, READ_BYTES);
      while true do
      begin
        FMemStream.SetSize(BytesRead + READ_BYTES); // make sure we have room
        NumBytes := Process.Output.Read((FMemStream.Memory + BytesRead)^, READ_BYTES); // try reading from the Process to our stream
        if NumBytes > 0 then // All read() calls will block, except the final one.
        begin
          Inc(BytesRead, NumBytes);
          SetLength(Buffer, NumBytes);
          FMemStream.Read(Buffer[1], NumBytes);
          AppNewLine(nil, Buffer);
          if UserAborted then
            Break
        end
        else
          BREAK // Program has finished execution.
      end;
      {$ELSE}
      {
         on macOS, if NumBytes = 0 because the child process isn't writing new output
         MEGA mistakenly thinks the process exited with code -1 because Process.ExitStatus
         retuns -1 if the process is still running this breaks AppLinker on MacOS
         so use Process.Running to check
      }
      while Process.Running do
      begin
        while Process.Output.NumBytesAvailable > 0 do
        begin
          NumBytes := Process.Output.Read(Buffer, READ_BYTES);
          if NumBytes > 0 then
          begin
            SetString(OutputString, PChar(@Buffer[1]), NumBytes);
            AppNewLine(nil, OutputString);
            if UserAborted then
               Break
          end;
        end;
        if UserAborted then
               Break
      end;
      {$ENDIF}
      if not UserAborted then
        AppExitCode := Process.ExitStatus;
      if (AppExitCode <= 0) or (AppExitCode = USER_ABORTED) then
      begin
        if Assigned(OnTerm) then
          OnTerm(Self);
      end
      else
      if not ((AppExitCode <= 0) or (AppExitCode = USER_ABORTED)) then
         raise Exception.Create('External process finished with exit code ' + IntToStr(AppExitCode));
      SetCurrentDirUTF8(UserDir);
    except
      on E:Exception do
      begin
        ErrorsReported.Add(E.Message);
        {$IFNDEF VISUAL_BUILD}
        error_nv('An error has occurred: ' + E.Message + '. (' + FExe + ')', E);
        {$ENDIF}
        if Assigned(OnTerm) then
          OnTerm(Self);
        SetCurrentDirUTF8(UserDir);
      end;
    end;
  finally
    if Assigned(Process) then
      Process.Free;
    if Assigned(FMemStream) then
      FMemStream.Free;
    if Assigned(Args) then
      Args.Free;
  end;
end;

procedure TAppLink.setDNA(adna: Boolean);
begin
  DNA := adna;
end;

procedure TAppLink.StopAppLink;  // Any special requirements when stopping an applink should be overwritten here in decendants.
begin
  if Process.Active then
    Process.Terminate(USER_ABORTED);
  AppExitCode := USER_ABORTED;
end;


procedure TAppLink.OnServerPipeMessage(Sender: TObject; Pipe: Cardinal; Stream: TStream);
begin

end;

procedure TAppLink.stdOut(sl: TStringList);
begin
  if redirectStdOut and (stdOutEnd <> nil) and (stdOutEnd.Count > 0) then
    sl.AddStrings(stdOutEnd);
end;

procedure TAppLink.stdErr(sl: TStringList);
begin
  if redirectStdOut and (stdErrEnd <> nil) and (stdErrEnd.Count > 0) then
    sl.AddStrings(stdErrEnd);
end;

function TAppLink.DataType: TSnTokenCode;
begin
  if IsProtein then
    Result := snProtein
  else if IscDNA then
    Result := snCodingDna
  else
    Result := snNucleotide;
end;

{ TMuscleLink }
procedure TMuscleLink.CheckUnhandledError; // We check for errors that have occured but were not reported and handled durring the programs execution.
var
  ErrorLog: TStringList;
  MaxWait: Integer;
  CurrentWait: Integer;
begin
  MaxWait := 5000;
  CurrentWait := 0;
  ErrorCode := noError;
  if not FileExists(OutputFilename)  then
    ErrorCode := noOutput;
  try
    try
      ErrorLog := TStringList.Create;
      // First check if the file exists and is not locked (readable), if it is not then ask the user if they want to view the output anyway
      while not FileExistsUTF8(LogFilename) do
      begin
        sleep(20);
        inc(CurrentWait, 20);
        if CurrentWait > MaxWait then
          raise Exception.Create('Muscle did not produce a log file');
      end;
      ErrorLog.LoadFromFile(LogFilename)
    except
      on E: Exception do
        ErrorCode := noLogFile;
    end;
    if ErrorLog.Count > 0 then
    begin
      if Pos('Finished', ErrorLog.Strings[ErrorLog.Count-1]) <> 1 then // Check the last line for "Finished ...".  Finished followed by the date shows on a successfull computation.
        ErrorCode := notFinished;
      if Pos('Alignment not completed', ErrorLog.Strings[ErrorLog.Count-1]) = 1 then  // if alignment not completed is not in the error log and the last line starts with finished then we do NOT have an error.
        ErrorCode := notAbleToSave;
      if Pos('Fatal error', ErrorLog.Text) <> 0 then
        ErrorCode := FatalException;
      if Pos('*** OUT OF MEMORY ***', ErrorLog.Text) <> 0 then
        ErrorCode := notEnoughMemory;
      if Pos('*** Max MEMORY SPECIFIED EXCEEDED***', ErrorLog.Text) <> 0 then
        ErrorCode := notEnoughMemorySpecified;
    end
    else
      ErrorCode := noLogFile;
  finally
    ErrorLog.Free;
  end;
end;

function TMuscleLink.ErrorCodeMessage: String;
begin
  case ErrorCode of
     noOutput: Result := 'MUSCLE application did not produce any output!';
     notAbleToSave: Result := 'An error has occurred during the Alignment Process.';
     notFinished: Result := 'MUSCLE Log file did not end properly, suggesting an unhandled exception.';
     noLogFile: Result := 'No alignment log file was found.';
     AppLinker.FatalException: Result := 'MUSCLE has encountered a Fatal Exception.';
     notEnoughMemory: Result := 'Computer reported lack of enough memory (RAM). Try freeing up memory by closing other programs and starting over.';
     notEnoughMemorySpecified: Result := 'MUSCLE has encountered a Fatal Exception. Not enough memory available for analysis';
     noError: Result := 'no error';
     else
       Result := 'unknown error code';
  end;
end;

constructor TMuscleLink.Create(Owner: TComponent);
begin
  inherited;
  ParentThread := nil;
  FWarnings := TStringList.Create;
  SeqList := TSequenceList.Create;
  FExe := GetMuscleExe;
  SetCurrentDirUTF8(ExtractFileDir(FExe));
  MyLinkName := 'TMuscleLink';
  FApp_HC := About_Muscle;
  MuscleOptions := TStringList.Create;
  OutputFilename := GetMuscleOutputFile;
  InputFileName := GetMuscleInputFile;
  LogFilename := GetMuscleRunLog;
  FFeedback := TStringList.Create;
end;

destructor TMuscleLink.Destroy;
begin
  if Assigned(MuscleOptions) then
    MuscleOptions.Free;
  if Assigned(FFeedback) then
    FFeedback.Free;
  if Assigned(SeqList) then
    SeqList.Free;
  if Assigned(MAI) then
  begin
    MAI.MyProcessPack := nil;
    MAI.Free;
  end;
  if Assigned(FWarnings) then
    FWarnings.Free;
  inherited Destroy;
end;

procedure TMuscleLink.Execute;
begin
  FStartTime := Now;
  {$IFNDEF VISUAL_BUILD}
  WriteLn();
  ShowRunStatusInfoStatic('status', 'Executing multiple sequence alignment (MUSCLE)');
  FullCmdLineString := EmptyStr;
  inherited;
  {$ELSE}
  if FileExists(OutputFilename) then
    DeleteFile(OutputFilename); // If there is an unhandled error we don't want to read in a past result.
  OptionsFinalize;
  ShowAnalysisInfo;
  if Assigned(UpdateStatusProc) then
    UpdateStatusProc('Status', 'Initializing MUSCLE (may take a while)');
  RunAppLink;
  {$ENDIF}
end;

{ Adds the muscle-specific arguments that are needed (e.g. logfilename, etc...) but are not user preferences}
function TMuscleLink.ComputeCmdArgs(KeyValues: TStrings; var Args: TStringList): Boolean;
begin
  Args.AddStrings(MuscleOptions);
  Args.Add('-in=' + InputFileName);
  Args.Add('-out=' + OutputFileName);
  Args.Add('-log=' + LogFilename);
  Result := True;
end;

procedure TMuscleLink.RunAppLink;
begin
  if Assigned(ParentThread) then
    ParentThread.ApplinkerProcess := Process;
  inherited RunAppLink;
end;

function TMuscleLink.GetWarningMessages: String;
begin
  Result := FWarnings.Text;
end;

function TMuscleLink.ParseMuscleFeedback(const Feedback: String; var PercMemRem: Integer; var MemUsed: Integer; var Iteration: Integer; var Progress: Double; var aTask: String): Boolean;
var
  tempInt: Integer;
  tempFloat: Double;
  NewLine: String;
  subString: String;
begin
  Result := False;
  NewLine := Trim(Feedback);
  subString := Trim(Copy(NewLine, pos('% ', NewLine)-6, 6));
  if TryStrToFloat(subString, tempFloat) then
    Progress := TempFloat
  else
    Exit;

  subString := Trim(Copy(NewLine, (Pos('(', newline)+1), (Pos('%', newline) - Pos('(', newline)-1)));
  if TryStrToInt(subString, tempInt) then
    PercMemRem := tempInt
  else
    Exit;

  subString := Trim(Copy(newline, pos(' ', newline), (pos('MB', newline) - pos(' ', newline))-1));
  if TryStrToInt(subString, tempInt) then
    MemUsed := tempInt
  else
    Exit;

  subString := Trim(Copy(NewLine, 29, 4));
  if TryStrToInt(subString, tempInt) then
    Iteration := tempInt
  else
    Exit;

  aTask := Copy(NewLine, 43, Length(NewLine));

  Result := True;
end;

procedure TMuscleLink.ShowAnalysisInfo;
var
  i: Integer;
  tempStr: String;
  aOptions: TStringList = nil;
begin
  try
    aOptions := TStringList.Create;
    aOptions.Add('Analysis=MUSCLE Multiple Sequence Alignment[fsBold]');
    aOptions.Add('Parameters=[fsBold]');
    if MuscleOptions.Count > 0 then
      for i := 0 to MuscleOptions.Count - 1 do
      begin
        tempStr := Copy(MuscleOptions.Names[i], 2, Length(MuscleOptions.Names[i]));
        aOptions.Add(tempStr + '=' + MuscleOptions.Values[MuscleOptions.Names[i]]);
      end;
    if Assigned(ShowAnalysisOptionsProc) then
      ShowAnalysisOptionsProc(aOptions, False);
  finally
    if Assigned(aOptions) then
      aOptions.Free;
  end;
end;

function TMuscleLink.IsShellMode: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  Result := MAI.MyProcessPack.IsWorkflowPack;
  {$ELSE}
   Result := True;
  {$ENDIF}
end;

procedure TMuscleLink.UpdateXSites;
var
  str: TSequence = nil;
  i, j: Integer;
begin
  XSites := EmptyStr; // Make sure it is empty.
  for i := 0 to SeqList.Count - 1 do
  begin
    str := SeqList[i];
    for j := 1 to str.NoOfSites do
      if IsDNA then
      begin
        if (not (upcase(str[j]) in DNABases)) and (str[j] <> '-') then
        begin
          XSites := XSites +str[j];
          str[j] := 'N';
        end
      end
      else if (not (upcase(str[j]) in AABases)) and (str[j] <> '-') then // Substitute any non AA bases with X.
      begin
        XSites := XSites +str[j];
        str[j] := 'X';
      end;
    SeqList[i] := str;
  end;
end;

procedure TMuscleLink.AddWarning(warning: String);
begin
  FWarnings.Add(warning);
  {$IFNDEF VISUAL_BUILD}
  warn_nv(warning);
  {$ENDIF}
end;

function TMuscleLink.GetOptions: TStrings;
begin
  Result := MuscleOptions;
end;

function TMuscleLink.GetXSites: AnsiString;
begin
  result := XSites;
end;

procedure TMuscleLink.OptionsFinalize;
var
  Col: Integer = -1;
  Row: Integer = -1;
  Filename: String;
  i: Integer;
  TempMuscleInputFile: TStringList = nil;
begin
  if not IsShellMode then
    OptionsFinalizeGuiMode
  else
  begin
    Filename := InputFileName; // Changed to use the input file name we specified earlier (includes process information so multiple can run at once).;
    if IscDNA then
    begin
      SeqList.Translate;
      if MegaUtils.CheckStopCodons(SeqList, Row, Col) then
        AddWarning('Stop Codon(s) are found in the translated sequences. Is the correct genetic code selected?');
    end
    else if IsProtein then
    begin
      if MegaUtils.CheckStopCodons(SeqList, Row, Col) then
        AddWarning('Stop Codon(s) are found in the sequences. Is the correct genetic code selected.');
    end;

    if SeqList.Count <= 1 then
      raise Exception.Create('Error: No sequences were found for muscle alignment.  Please check your input data file.');
    try
      UpdateXSites;
      TempMuscleInputFile := TStringList.Create;
      for i := 0 to SeqList.Count-1 do
      begin
        TempMuscleInputFile.Add('>TX' + IntToStr(i));
        TempMuscleInputFile.Add(SeqList[i].SeqData);
      end;
      try
        TempMuscleInputFile.SaveToFile(FileName);
      except
        on E : Exception do
        begin
          raise Exception.Create('Error while attempting to save the selected data to a Fasta File: ' + FileName + '.  Please ensure another program is not currently using this file.');
        end;
      end;
    finally
      FreeAndNil(TempMuscleInputFile);
    end;

    LogFilename := GetPrivateFile('Private' + PathDelim + 'temp' + PathDelim + 'MuscleRunLog_' + IntToStr(GetCurrentProcessId) + '.txt', False);
    MuscleOptions.Add(Format('-log=%s', [LogFilename]));
    if (IscDNA or IsProtein)   then
      MuscleOptions.Add('-seqtype=protein');
  end;
end;

procedure TMuscleLink.OptionsFinalizeGuiMode;
var
  SeqType: String;
begin
  if IsDNA and (not IscDNA) then
    SeqType := DNAStr
  else
    SeqType := ProteinStr;
  if IscDNA then
    SeqType := ProteinStr;
  MuscleOptions.Add('-seqtype=' + SeqType);
end;

procedure TMuscleLink.SetSequenceData(var aSeqList: TSequenceList);
begin
  if Assigned(SeqList) then
    FreeAndNil(SeqList);
  SeqList := aSeqList;
end;

procedure TMuscleLink.UpdateStatus(NewLine: AnsiString);
{$IFDEF VISUAL_BUILD}
var
  aTask: String = '';
  aProgress: Double = -1;
  aPercMemUsed: Integer = -1;
  aMemUsed: Integer = -1;
  aIteration: Integer = -1;
  tempLine: String;
  isParsed: Boolean = False;
{$ENDIF}
begin
  {$IFNDEF VISUAL_BUILD}
  Write(NewLine);
  {$ELSE}
  FFeedback.Clear;
  FFeedback.Text := Trim(NewLine);
  if FFeedback.Count > 0 then
  begin
    while (FFeedback.Count > 0) and (not isParsed) do
    begin
      tempLine := FFeedback[FFeedback.Count - 1];
      FFeedback.Delete(FFeedback.Count - 1);
      isParsed := ParseMuscleFeedback(tempLine, aPercMemUsed, aMemUsed, aIteration, aProgress, aTask);
      if isParsed then
      begin
        if Assigned(NewLineProc) then
          NewLineProc(tempLine);

        if Assigned(UpdateProgressProc) then
          UpdateProgressProc(trunc(aProgress));

        if Assigned(UpdateStatusProc) then
        begin
          if (aPercMemUsed > 80) then
            UpdateStatusProc('Warning', 'Memory low, only ' + IntToStr(100-aPercMemUsed) + '% left');
          UpdateStatusProc('Memory Used (MB)', IntToStr(aMemUsed));
          if aTask <> '' then
            UpdateStatusProc('Status', aTask);
          UpdateStatusProc('Iteration:', IntToStr(aIteration));
        end;
      end;
    end;
  end;
  {$ENDIF}
end;

{ TLinkDoneThread }
constructor TLinkDoneThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  Applink := nil;
end;

procedure TLinkDoneThread.Execute;
begin
  Synchronize(Finished);
end;

procedure TLinkDoneThread.Finished;
begin
   if AppLink.UserAborted then
   begin
     {$IFDEF VISUAL_BUILD}
     if Applink.MyLinkName = 'TMuscleLink' then
     begin
       AlignEditMainForm.Enabled := True; {TODO -oDan : This should be in the "MuscleFinished" function which is called onTerm otherwise it effects HyPhy and other components}
       AlignEditMainForm.AlignmentIsInProgress := False;
       AlignEditMainForm.BringToFront;
     end;
     {$ENDIF}
     Exit;
   end;
  AppLink.ExecDone := true;
  if Assigned(AppLink.OnTerm) then
    AppLink.OnTerm(AppLink);
  AppLink.MAI.ARP.Hide;

  if not (AppLink.MAI.MyUsrOperation = dtdoPhyloQ) then
  begin
    if Assigned(AppLink.MAI) and Assigned(AppLink.MAI.ARP) then
      AppLink.MAI.ARP.Free;
    FreeAndNil(AppLink.MAI);
  end;

  FreeAndNil(AppLink);
end;

{ TUnzipLink }

constructor TUnzipLink.Create(Owner: TComponent);
begin
  DontAskForCalcOptions := true;
  inherited;
  FullCmdLineString := '"' + GetPrivateExecutableFile('Private' + PathDelim + '7za.exe') + '" x '; // Still need filename
end;

procedure TUnzipLink.Execute;
begin
  FullCmdLineString := FullCmdLineString + '"' + filename + '"';
  if outputDirectory <> EmptyStr then
    FullCmdLineString := FullCmdLineString + ' -o"' + outputDirectory + '" -y'; // the -y flag says 'yes' to all questions.  This is needed.
  {$IFDEF VISUAL_BUILD}
  ShowMessage(FullCmdLineString);
  {$ENDIF}
  inherited;
end;


end.
