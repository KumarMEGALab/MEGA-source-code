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

unit esl_linker;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses

{$IFDEF VISUAL_BUILD}
  uMegaBrowser,
{$ENDIF}
  Classes, SysUtils, MegaConsts, AppLinker, MegaUtils, zipper, Menus, applinker_result_file,
  mesl_input_data, MD_InputSeqData;

type

  { TEslLinker }

  TEslLinker = class(TAppLink)
    private
      FCladeSize: Integer;
      FGridImage: String;
      FCommandLineOptions: TStringList;
      FDataSource: String;
      FNumDomains: Integer;
      FNumGridColumns: Integer;
      FNumGridRows: Integer;
      FNumSequences: Integer;
      FPrepareEslInputData: IPrepareEslInputData;
      FSegmentSize: Integer;
      FTargetNodeName: String;
      FUnzipper: TUnZipper;
      FNewickString: String;
      FStartTime: TDateTime;
      FEndTime: TDateTime;
      FIsSuccess: Boolean;
      FLog: TStringList;
      FOrigWorkingDir: String;
      FEslInputDirectory: String;
      FUserInputDataDirectory: String;
      FInputAlignmentsDirectory: String;
      FEslRootDirectory: String;
      FCladeListFile: String;
      FUserTreeFile: String;
      FResultsFiles: TList;
      FNeedToSaveResults: Boolean;
      FAlignmentsListFile: String;
      FEslOutputDirectory: String;
      {$IFDEF VISUAL_BUILD}
      FCaptionViewer: TMegaBrowserFrm;
      function GetGridImageFile: String;
      {$ENDIF}
    protected

      function ComputeCmdArgs(KeyValues: TStrings; var Args: TStringList): Boolean; overload; override;
      function CopyDataToExecEnvironment: Boolean;
      function CopyCladeListToExecEnvironment: Boolean;
      function CopyNewickToExecEnvironment: Boolean;
      function CollectResultsFiles: Boolean;
      function GetFilePath(displayName: String): String;
      function GetResultsFile(displayName: String): TAppLinkerResultsFile;
      function ExtractDrPhyloArchive: Boolean;
      procedure ExtractDrPhyloProgress(Sender : TObject; const ATotPos, ATotSize: Int64);
      function RemoveTempResultsDirectory: Boolean;
      procedure CloseResultsFiles;
      function SaveResultsFiles: Boolean;
      function CheckOverwriteExistingFiles(const targetDir: String; var filenameSuffix: String): Integer;
      procedure CleanUpTempFiles;
      procedure RunAppLink; override;
      procedure PrepareInputData;
      {$IFDEF VISUAL_BUILD}
      procedure ShowFileInImageViewer(filename: String; aCaption: String);
      procedure ViewerClosed(Sender: TObject);
      {$ENDIF}
    public
      ParentThread: TAppLinkThread;
      ResultsPopup: TPopupMenu;
      constructor Create(Owner: TComponent);
      destructor Destroy; override;

      procedure Execute;
      {$IFNDEF VISUAL_BUILD}
      function MoveResultsFiles(targetDir: String): Boolean;
      {$ENDIF}
      function GetResultsFilePaths: TStringList;
      function InitializeEslExecutionDir: Boolean;
      function RemoveTempExecutionDir: Boolean;
      function GetOptions: TStrings; override;
      procedure OptionsFinalize; override;
      procedure UpdateStatus(NewLine: AnsiString); override;
      function ErrorCodeMessage: String;
      function DoResultsCommand(aCmd: String): Boolean;
      function GetInputAlignmentsPath: String;
      property TempRootDirectory: String read FEslRootDirectory write FEslRootDirectory;
      property UserInputDataDirectory: String read FUserInputDataDirectory write FUserInputDataDirectory;
      property TempEslDirectory: String read FEslInputDirectory write FEslInputDirectory;
      property UserTreeFile: String read FUserTreeFile write FUserTreeFile;
      property NewickString: String read FNewickString write FNewickString;
      property CommandLineOptions: TStringList read FCommandLineOptions write FCommandLineOptions;
      property IsSuccess: Boolean read FIsSuccess;
      property StartTime: TDateTime read FStartTime;
      property EndTime: TDateTime read FEndTime;
      property ResultsFiles: TList read FResultsFiles;
      property TargetNodeName: String read FTargetNodeName write FTargetNodeName;
      property InputDataPrep: IPrepareEslInputData read FPrepareEslInputData write FPrepareEslInputData;
      property NumDomains: Integer read FNumDomains;
      property DataSource: String read FDataSource write FDataSource;
      property SegmentSize: Integer read FSegmentSize write FSegmentSize;
      property CladeSize: Integer read FCladeSize write FCladeSize;
      property NumSequences: Integer read FNumSequences;
      property NumGridRows: Integer read FNumGridRows write FNumGridRows;
      property NumGridColumns: Integer read FNumGridColumns write FNumGridColumns;
      {$IFDEF VISUAL_BUILD}
      property GridImageFile: String read GetGridImageFile;
      {$ENDIF}
  end;

  { TEslLinkerThread }

  TEslLinkerThread = class(TAppLinkThread)
  private
    FCladeSize: Integer;
    FSeqData: TD_InputSeqData;
    FEslRootDir: String;
    FAlignmentFilesDir: String;
    FDataSource: String;
    FErrorMsg: String;
    FErrorClass: String;
    FEslLink: TEslLinker;
    FNewickString: String;
    FSegmentSize: Integer;
    FUserTreeFile: String;
    function GetTargetNodeName: String;
    function RunTargetApplication: Boolean;
    procedure DoSynchronizeErrorMessage;
    procedure SetDataSource(AValue: String);
    procedure SetTargetNodeName(AValue: String);
    function GetDataPreparator: IPrepareEslInputData;
    procedure DoConstruction(aDataSource: String);
  public
    constructor CreateForFastaFiles(aDataSource: String; fastaFilesDir: String);
    constructor CreateForSegmentedData(aDataSource: String; seqData: TD_InputSeqData; segmentSize: Integer);
    constructor CreateForDomainData(aDataSource: String; seqData: TD_InputSeqData);
    destructor Destroy; override;
    function CleanUpTempEslFiles: Boolean;
    procedure Execute; override;
    function UserAborted: Boolean;
    procedure SynchronizeErrorMessage(E: Exception);
    procedure UpdateCommandLineOptions(eslParams: TStringList);
    property IsSuccess: Boolean read FIsSuccess;
    function GetLog: TStringList;
    procedure SetNewLineProc(aNewLineProc: TNewLineProc);
    property EslLink: TEslLinker read FEslLink write FEslLink;
    property AlignmentFilesDir: String read FAlignmentFilesDir write FAlignmentFilesDir;
    property UserTreeFile: String read FUserTreeFile write FUserTreeFile;
    property NewickString: String read FNewickString write FNewickString;
    property CladeSize: Integer read FCladeSize write FCladeSize;
    property TargetNodeName: String read GetTargetNodeName write SetTargetNodeName;
    property DataSource: String read FDataSource write SetDataSource;
    property SegmentSize: Integer read FSegmentSize write FSegmentSize;
  end;

  function GetEslExePath: String;
  function CompareEslResultsFiles(Item1: Pointer; Item2: Pointer): Integer;
  {$IFNDEF VISUAL_BUILD}
  function RunEslLinker(targetNodeName: String; treeFile: String; alignmentsDir: String; aOptions: TStringList): Boolean;
  {$ENDIF}

implementation

uses
  {$IFDEF VISUAL_BUILD}
  mega_main, mdrphylo_caption,
  {$ELSE}
  MD_MegaMain, mruntimeprogressdlg, MAnalysisInfo, MProcessPack,
  {$ENDIF}
  FileUtil, LazFileUtils, MegaUtils_NV, Dialogs, MEditorForm, Controls, math,
  Forms, RegExpr;

function GetEslExePath: String;
begin
  {$IFDEF DARWIN}
  Result := GetPrivateExecutableFile('Private' + PathDelim + 'MyESL' + PathDelim + ESL_EXE, False);
  {$ELSE}
  Result := GetPrivateExecutableFile('Private' + PathDelim + 'DrPhylo' + PathDelim + ESL_EXE, False);
  {$ENDIF}
end;

function CompareEslResultsFiles(Item1: Pointer; Item2: Pointer): Integer;
var
  f1: TAppLinkerResultsFile = nil;
  f2: TAppLinkerResultsFile = nil;
begin
  f1 := TAppLinkerResultsFile(Item1);
  f2 := TAppLinkerResultsFile(Item2);
  Result := CompareValue(f1.DisplayOrder, f2.DisplayOrder);
end;

{$IFNDEF VISUAL_BUILD}
function RunEslLinker(targetNodeName: String; treeFile: String; alignmentsDir: String; aOptions: TStringList): Boolean;
var
  fastaFiles: TStringList = nil;
  t: TEslLinkerThread = nil;
  ARP: TRuntimeProgress = nil;
  MAI: TAnalysisInfo = nil;
  newick: TStringList = nil;
  isNuc: Boolean = False;
  tempStr: String = '';
begin
  Result := False;
  try
    fastaFiles := FindAllFiles(alignmentsDir);
    if fastaFiles.Count = 0 then
      raise Exception.Create('no alignment files were found');

    t := TEslLinkerThread.CreateForFastaFiles(FASTA_FILES, alignmentsDir);
    t.AlignmentFilesDir := alignmentsDir;
    t.UpdateCommandLineOptions(aOptions);

    newick := TStringList.Create;
    newick.LoadFromFile(treeFile);
    t.NewickString := newick.Text;
    t.TargetNodeName := targetNodeName;
    t.FreeOnTerminate := False;
    t.OnTerminate := D_MegaMain.EslThreadDone;

    ARP := TRuntimeProgress.Create(nil);
    MAI := TAnalysisInfo.Create;
    MAI.MyProcessPack := TProcessPack.Create;
    MAI.MyProcessPack.AddProcessType(ppDrPhylo);
    MAI.ARP := ARP;
    MAI.ARP.HasCmdLineOutput := True;
    ARP := nil;
    t.EslLink.MAI := MAI;
    t.UpdateProgressProc := MAI.ARP.UpdatePercentProgress;
    t.UpdateStatusProc := MAI.ARP.UpdateRunStatusInfo;
    t.NewLineProc := MAI.ARP.AddCommandLine;
    t.NewLineCheckCancelFunc := MAI.ARP.AddCommandLineCheckCancel;
    t.ShowAnalysisOptionsProc := MAI.ARP.AddAnalysisOptions;

    MAI.AnalysisSummary.AnalysisOptions.Add('Data Segmented By=fasta_files');
    isNuc := (aOptions.Values['--data_type'] = 'nucleotide');
    MAI.AnalysisSummary.AnalysisOptions.Add(Format('Encode Nucleotide Chars=%s', [BoolToStr(isNuc, True)]));
    MAI.AnalysisSummary.AnalysisOptions.Add(Format('Encode Protein Chars=%s', [BoolToStr(not isNuc, True)]));
    tempStr := aOptions.Values['--m_grid'];
    t.EslLink.NumGridRows := StrToInt(Copy(tempStr, 1, Pos(',', tempStr) - 1));
    t.EslLink.NumGridColumns := StrToInt(Copy(tempStr, Pos(',', tempStr) + 1, Length(tempStr)));
    t.Start;
    t.WaitFor;
    Result := t.IsSuccess;
  finally
    if Assigned(fastaFiles) then
      fastaFiles.Free;
    if Assigned(newick) then
      newick.Free;
  end;
end;
{$ENDIF}

{ TEslLinkerThread }

function TEslLinkerThread.RunTargetApplication: Boolean;
begin
  Result := False;
  try
    FEslLink.NewickString := FNewickString;
    FEslLink.NewLineProc := ProgressNewLine;
    FEslLink.NewLineCheckCancelFunc := AppLinkerNewLineCheckCancel;
    FEslLink.UpdateStatusProc := UpdateStatusInfo;
    FEslLink.UpdateProgressProc := UpdateProgress;
    FEslLink.ShowAnalysisOptionsProc := ShowAnalysisOptions;
    FEslLink.ParentThread := Self;
    FEslLink.Execute;
    FEslLink.ParentThread := nil;

    if FEslLink.ErrorCode <> noError then
    begin
      Result := False;
      MessagesLog.Add('ESL failed with error: ' + FEslLink.ErrorCodeMessage);
    end
    else
    begin
      Result := True;
      FUserTreeFile := FEslLink.UserTreeFile;
    end;
  except
    on E:Exception do
    begin
      MessagesLog.Add('Failed to run ESL: ' + E.Message);
      FIsSuccess := False;
    end;
  end;
end;

function TEslLinkerThread.GetTargetNodeName: String;
begin
  Result := FEslLink.TargetNodeName;
end;

procedure TEslLinkerThread.DoSynchronizeErrorMessage;
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FEslLink.MAI) and Assigned(FEslLink.MAI.ARP) then
    FEslLink.MAI.ARP.Hide;
  ShowMessage(Format('Error (%s) in ESL thread: %s', [FErrorClass, FErrorMsg]));
  {$ELSE}
  error_nv(Format('Error (%s) in ESL thread: %s', [FErrorClass, FErrorMsg]));
  {$ENDIF}
end;

procedure TEslLinkerThread.SetDataSource(AValue: String);
begin
  if FDataSource = AValue then Exit;
  FDataSource := AValue;
end;

procedure TEslLinkerThread.SetTargetNodeName(AValue: String);
begin
  FEslLink.TargetNodeName := AValue;
end;

function TEslLinkerThread.GetDataPreparator: IPrepareEslInputData;
begin
  if FDataSource = FASTA_FILES then
    Result := TCopyFastaFiles.Create(FAlignmentFilesDir, FEslLink.TempEslDirectory)
  else if FDataSource = ALIGNMENT_DOMAINS then
    Result := TDomainsToFastaFiles.Create(FSeqData, FEslLink.GetInputAlignmentsPath)
  else if FDataSource = ALIGNMENT_SEGMENTS then
    Result := TSegmentsToFastaFiles.Create(FSeqData, FEslLink.GetInputAlignmentsPath, FSegmentSize)
  else
    Result := nil;
end;

procedure TEslLinkerThread.DoConstruction(aDataSource: String);
begin
  inherited Create(True);
  FCladeSize := 0;
  FDataSource := aDataSource;
  ApplinkerProcess := nil;
  FIsSuccess := False;
  FIsCancelled := False;
  FreeOnTerminate := True;
  FEslLink := TEslLinker.Create(nil);
  FEslLink.DataSource := aDataSource;
  NewLineProc := nil;
  NewLineCheckCancelFunc := nil;
  UpdateStatusProc := nil;
  UpdateProgressProc := nil;
end;

constructor TEslLinkerThread.CreateForFastaFiles(aDataSource: String; fastaFilesDir: String);
begin
  if aDataSource <> FASTA_FILES then
    raise Exception.Create(Format('constructor reserved for %s but got %s', [FASTA_FILES, aDataSource]));
  DoConstruction(FASTA_FILES);
  FAlignmentFilesDir := fastaFilesDir;
end;

constructor TEslLinkerThread.CreateForSegmentedData(aDataSource: String; seqData: TD_InputSeqData; segmentSize: Integer);
begin
  if aDataSource <> ALIGNMENT_SEGMENTS then
    raise Exception.Create(Format('constructor is reserved for %s but got %s', [ALIGNMENT_SEGMENTS, aDataSource]));
  DoConstruction(ALIGNMENT_SEGMENTS);
  FSegmentSize := segmentSize;
  FSeqData := seqData;
  FEslLink.SegmentSize := FSegmentSize;
end;

constructor TEslLinkerThread.CreateForDomainData(aDataSource: String; seqData: TD_InputSeqData);
begin
  if aDataSource <> ALIGNMENT_DOMAINS then
    raise Exception.Create(Format('constructor reserved for %s but got %s', [ALIGNMENT_DOMAINS, aDataSource]));
  DoConstruction(ALIGNMENT_DOMAINS);
  FSeqData := seqData;
end;

destructor TEslLinkerThread.Destroy;
begin
  NewLineProc := nil;
  NewLineCheckCancelFunc := nil;
  UpdateStatusProc := nil;
  UpdateProgressProc := nil;
  if Assigned(FEslLink) then
    FEslLink.Free;
  inherited Destroy;
end;

function TEslLinkerThread.CleanUpTempEslFiles: Boolean;
begin
  if Assigned(FEslLink) then
    Result := FEslLink.RemoveTempExecutionDir
  else
    Result := True;
end;

procedure TEslLinkerThread.Execute;
begin
  if not Assigned(FEslLink) then
  begin
    FIsSuccess := False;
    MessagesLog.Add('ESL application linker not initialized');
    Terminate;
    Exit;
  end
  else
  begin
    {$IFDEF VISUAL_BUILD}
    FEslRootDir := GetPrivateOutputFile('Private' + PathDelim + 'DrPhylo');
    {$ELSE}
    FEslRootDir := 'C:\Users\gstecher\Documents\NetBeansProjects\megaTestSuite\dev\testCases\dr_phylo\';
    {$ENDIF}
    FEslLink.CladeSize := FCladeSize;
    if not DirectoryExists(FEslRootDir) then
      if not ForceDirectory(FEslRootDir) then
        raise Exception.Create('failed to initialize the DrPhylo directory');
    FEslLink.TempRootDirectory := ChompPathDelim(FEslRootDir);
    if not FEslLink.InitializeEslExecutionDir then
    begin
      FIsSuccess := False;
      MessagesLog.Add('ESL execution directory was not initialized properly');
      Terminate;
      Exit;
    end;

    FEslLink.InputDataPrep := GetDataPreparator;
    FEslLink.InputDataPrep.SetNewLineProc(NewLineProc);
    FIsSuccess := RunTargetApplication;
  end;
end;

function TEslLinkerThread.UserAborted: Boolean;
begin
  Result := FEslLink.UserAborted;
end;

procedure TEslLinkerThread.SynchronizeErrorMessage(E: Exception);
begin
  FErrorMsg := E.Message;
  FErrorClass := E.ClassName;
  Synchronize(DoSynchronizeErrorMessage);
end;

procedure TEslLinkerThread.UpdateCommandLineOptions(eslParams: TStringList);
begin
  FEslLink.CommandLineOptions.Assign(eslParams);
end;

function TEslLinkerThread.GetLog: TStringList;
begin
  Result := TStringList.Create;
  if Assigned(FEslLink) and Assigned(FEslLink.FLog) then
    Result.Assign(FEslLink.FLog);
end;

procedure TEslLinkerThread.SetNewLineProc(aNewLineProc: TNewLineProc);
begin
  NewLineProc := aNewLineProc;
  if Assigned(FEslLink) then
    FEslLink.NewLineProc := aNewLineProc;
end;

{ TEslLinker }

function TEslLinker.GetFilePath(displayName: String): String;
var
  i: Integer = -1;
  f: TAppLinkerResultsFile = nil;
begin
  Result := EmptyStr;
  if FResultsFiles.Count > 0 then
    for i := 0 to FResultsFiles.Count - 1 do
    begin
      f := TAppLinkerResultsFile(FResultsFiles[i]);
      if f.DisplayName = displayName then
      begin
        Result := f.FilePath;
        Exit;
      end;
    end;
end;

function TEslLinker.GetResultsFile(displayName: String): TAppLinkerResultsFile;
var
  i: Integer = -1;
  f: TAppLinkerResultsFile = nil;
begin
  Result := nil;
  if FResultsFiles.Count > 0 then
    for i := 0 to FResultsFiles.Count - 1 do
    begin
      f := TAppLinkerResultsFile(FResultsFiles[i]);
      if f.DisplayName = displayName then
      begin
        Result := f;
        Exit;
      end;
    end;
end;

function TEslLinker.ExtractDrPhyloArchive: Boolean;
var
  archive: String = '';
begin
  Result := False;
  UpdateStatus('Checking execution environment...');

  {$IFDEF VISUAL_BUILD}
  FExe := GetEslExePath;
  {$ELSE}
  FExe := 'C:\Users\gstecher\Documents\NetBeansProjects\megaTestSuite\dev\testCases\dr_phylo\' + ESL_EXE;
  {$ENDIF}

  if FileExists(FExe) then
  begin
    Result := True;
    Exit;
  end
  else
  begin
  {$IFDEF DARWIN}
    raise Exception.Create('Unexpected error: ESL binaries missing from application bundle');
  {$ENDIF}
  end;

  UpdateStatus('Extracting ESL binary and shared libraries...');
  if Assigned(NewLineProc) then
    NewLineProc('Extracting ESL binary and shared libraries...');

  { on Windows, the installer copies this file to the same directory as the
    binaries, instead of the user's app data directory. This is needed because
    some users have to install as admin, in which case, user directories are
    unknown to the installer. We would prefer to compile DrPhylo.zip into the
    application resources but Lazarus hangs when binary files (such as DrPhylo.zip/MyEsl.exe)
    are included in the resource file. To work around that problem, MEGA installers copy
    problematic resources such as DrPhylo.zip to the local file system}
  {$IFDEF MSWINDOWS}
  archive := ExtractFilePath(Application.ExeName) + 'DrPhylo.zip';
  {$ELSE}
  archive := GetPrivateFile('Private' + PathDelim + 'DrPhylo' + PathDelim + 'DrPhylo.zip', False);
  {$ENDIF}

  if FileExists(archive) then
  begin
    FUnzipper.Filename := archive;
    FUnzipper.OutputPath := ExtractFileDir(FExe);
    FUnzipper.OnProgressEx := ExtractDrPhyloProgress;
    FUnzipper.Examine;
    FUnzipper.UnZipAllFiles;
    Result := FileExists(FExe);
  end
  else
    raise Exception.Create('Required ESL resources are missing. Please try reinstalling MEGA.');
end;

procedure TEslLinker.ExtractDrPhyloProgress(Sender: TObject; const ATotPos, ATotSize: Int64);
begin
  if Assigned(NewLineProc) then
  begin
    if ATotSize > 0 then
      NewLineProc(Format('%.2f%% Extracting DrPhylo binaries', [ATotPos/ATotSize*100]))
    else
      NewLineProc(Format('%d - %d Extracting DrPhylo binaries', [ATotPos, ATotSize]));
  end;
end;

function TEslLinker.RemoveTempResultsDirectory: Boolean;
begin
  Result := True;
  try
    if DirectoryExists(FEslOutputDirectory) then
      DeleteDirectory(FEslOutputDirectory, False);
  except
    Result := False;
  end;
end;

procedure TEslLinker.CloseResultsFiles;
var
  f: TAppLinkerResultsFile = nil;
  i: Integer = -1;
  ext: String = '';
  {$IFNDEF DEBUG}
  aResponse: Integer = -1;
  {$ENDIF}
begin
  {$IFNDEF DEBUG}
  if FNeedToSaveResults then
  begin
    aResponse := QuestionDlg('Save DrPhylo Results Files?', 'DrPhylo analysis results have not been saved. Save results files to store information for the future?', mtCustom, [mrYes, 'Save DrPhylo results files?', mrNo, 'Discard Results', mrCancel, 'Cancel'], 0);
    case aResponse of
      mrYes: if not SaveResultsFiles then Exit;
      mrNo: FNeedToSaveResults := False;
      mrCancel: Exit;
    end;
  end;
  {$ENDIF}
  if Assigned(ResultsPopup) then
  begin
    {$IFDEF VISUAL_BUILD}
    MegaForm.RemoveDropdownFromTray(ResultsPopup);
    FreeAndNil(ResultsPopup);
    {$ENDIF}

    if FResultsFiles.Count > 0 then
      for i := 0 to FResultsFiles.Count - 1 do
      begin
        f := TAppLinkerResultsFile(FResultsFiles[i]);
        if FileExists(f.FilePath) then
        begin
          ext := ExtractFileExt(f.FilePath);
          if ext = '.txt' then
          begin
            {$IFDEF VISUAL_BUILD}
            if Assigned(EditorForm) and StringListIsOpen(f.FullDisplayName) then
              CloseStringListTab(f.FullDisplayName);
            {$ENDIF}

            DeleteFile(f.FilePath);
          end
          else if ext = '.png' then
          begin
            {$IFDEF VISUAL_BUILD}
            if FileIsOpenInFileBrowser(f.FilePath) then
              CloseBrowserFile(f.FilePath);
            {$ENDIF}
            DeleteFile(f.FilePath);
          end;
        end;
      end;

    {$IFDEF VISUAL_BUILD}
    if Assigned(editorForm) then
    begin
      if EditorForm.NumOpenFiles = 0 then
        EditorForm.Hide;
    end;
    {$ENDIF}

    if DirectoryExists(FEslOutputDirectory) then
      DeleteDirectory(FEslOutputDirectory, False);
  end;
end;

function TEslLinker.SaveResultsFiles: Boolean;
var
  saveDlg: TSelectDirectoryDialog = nil;
  targetDir: String = '';
  targetFile: String = '';
  baseName: String = '';
  status: Integer = mrOk;
  filenameSuffix: String = '';
  i: Integer = -1;
  f: TAppLinkerResultsFile = nil;

  function AddSuffixToFilename(filename: String): String;
  var
    name: String;
    ext: String;
  begin
    if Trim(filenameSuffix) = EmptyStr then
      Result := targetDir + PathDelim + filename
    else
    begin
      name := ExtractFileNameWithoutExt(filename);
      ext := ExtractFileExt(filename);
      Result := targetDir + PathDelim + name + filenameSuffix + ext;
    end;
  end;

begin
  Result := True;
  try
    try
      saveDlg := TSelectDirectoryDialog.Create(nil);
      saveDlg.InitialDir := GetCurrentDirUTF8;
      saveDlg.Options := [ofPathMustExist, ofCreatePrompt, ofForceShowHidden];
      if saveDlg.Execute then
      begin
        targetDir := saveDlg.FileName;
        if not DirectoryExistsUTF8(targetDir) then
          if not ForceDirectoriesUTF8(targetDir) then
            raise Exception.Create('Application Error: unable to create target directory for saving DrPhylo results files');
        status := CheckOverwriteExistingFiles(targetDir, filenameSuffix);
        if status <> mrCancel then
        begin
          for i := 0 to FResultsFiles.Count - 1 do
          begin
            f := TAppLinkerResultsFile(FResultsFiles[i]);
            baseName := ExtractFileName(f.FilePath);
            baseName := StringReplace(baseName, '_esl_output', EmptyStr, []);
            targetFile := targetDir + PathDelim + baseName;
            if status = mrNo then
              targetFile := AddSuffixToFilename(baseName);
            CopyFile(f.FilePath, targetFile, [cffOverwriteFile], True);
          end;
        end;

        if status <> mrCancel then
          FNeedToSaveResults := False;
      end
      else
        Result := False;
    except
      on E:Exception do
        ShowMessage('Error when saving DrPhylo results files: ' + E.Message)
    end;
  finally
    if Assigned(saveDlg) then
      saveDlg.Free;
  end;
end;

function TEslLinker.CheckOverwriteExistingFiles(const targetDir: String; var filenameSuffix: String): Integer;
var
  aList: TStringList = nil;
  suffix: Integer = -1;
  i: Integer = -1;
  sourceFile: TAppLinkerResultsFile = nil;
begin
  try
    Result := mrOK;
    aList := TStringList.Create;
    if FResultsFiles.Count > 0 then
    begin
      for i := 0 to FResultsFiles.Count - 1 do
      begin
        sourceFile := TAppLinkerResultsFile(FResultsFiles[i]);
        if FileExists(sourceFile.FilePath) then
          aList.Add(ExtractFileName(sourceFile.FilePath));
      end;
    end;

    suffix := NextAvailableFileNumber(targetDir, aList);
    if suffix >= 0 then
    begin
      filenameSuffix := Format('-%d', [suffix]);
      Result := MessageDlg('Overwrite Existing File(s)?', 'One or more of the files already exist in the target directory. Overwrite file(s)?', mtWarning, mbYesNoCancel, 0);
    end
    else
      filenameSuffix := EmptyStr;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TEslLinker.CleanUpTempFiles;
var
  i: Integer = -1;
  filename: String = '';
  aList: TStringList = nil;
  aNodeName: String = '';
  aExt: String = '';
begin
  try
    try
      aList := FindAllFiles(FEslRootDirectory);
      if aList.Count > 0 then
      begin
        aNodeName := LowerCase(FTargetNodeName);
        for i := 0 to aList.Count - 1 do
        begin
          filename := LowerCase(ExtractFilename(aList[i]));
          if not DirectoryExists(aList[i]) then
          begin
            if filename.Contains(aNodeName) then
            begin
              aExt := LowerCase(ExtractFileExt(aList[i]));
              if (aExt = '.txt') or (aExt = '.png') then
                DeleteFile(aList[i]);
            end;
          end;
        end;
      end;
    except
      on E:Exception do
        FLog.Add('error when removing temp files: ' + E.Message);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TEslLinker.RunAppLink;
begin
  if Assigned(ParentThread) then
    ParentThread.ApplinkerProcess := Process;
  inherited RunAppLink;
end;

procedure TEslLinker.PrepareInputData;
begin
  if not FPrepareEslInputData.PrepareEslData then
    raise Exception.Create('failed to prepare input data');
  FNumSequences := FPrepareEslInputData.NumSequences;
  if not CopyDataToExecEnvironment then
    raise Exception.Create('failed to copy input data');
  FNumDomains := FPrepareEslInputData.NumFiles;
end;

{$IFDEF VISUAL_BUILD}
procedure TEslLinker.ShowFileInImageViewer(filename: String; aCaption: String);
var
  captionStr: String = '';
begin
  try
    if not Assigned(FCaptionViewer) then
    begin
      captionStr := GenerateDrPhyloCaption(Self);
      FCaptionViewer := CreateNewChromiumBrowserWindow(bmCaption, True);
      FCaptionViewer.DoNotAddToWindowTray := True;
      FCaptionViewer.ButtonHint := Format('DrPhylo analysis of %.0n groups', [NumDomains*1.0]);
      FCaptionViewer.LoadHtmlFromString(captionStr);
      FCaptionViewer.DisplayImage(GridImageFile);
      FCaptionViewer.SetTargetDimensions(900, min(Screen.WorkAreaHeight - 100, 800));
      FCaptionViewer.Caption := aCaption;
      FCaptionViewer.OnCloseNotify := ViewerClosed;
      FCaptionViewer.Show;
    end
    else
    begin
      if FCaptionViewer.WindowState = wsMinimized then
        FCaptionViewer.WindowState := wsNormal;
      FCaptionViewer.BringToFront;
    end;
  except
    on E:Exception do
      ShowMessage('Application error when displaying PNG image: ' + E.Message);
  end;
end;

procedure TEslLinker.ViewerClosed(Sender: TObject);
begin
  FCaptionViewer := nil;
end;

{$ENDIF}

function TEslLinker.DoResultsCommand(aCmd: String): Boolean;
var
  filename: String = '';
  ext: String = '';
  aList: TStringList = nil;
  f: TAppLinkerResultsFile = nil;
begin
  if aCmd = CLOSE_RESULTS_FILES then
  begin
    CloseResultsFiles;
    Exit(True);
  end
  else if aCmd = SAVE_RESULTS_FILES then
  begin
    SaveResultsFiles;
    Exit(True);
  end;

  try
    f := GetResultsFile(aCmd);
    if Assigned(f) then
    begin
      filename := f.FilePath;
      if FileExists(filename) then
      begin
        Result := True;
        ext := ExtractFileExt(filename);
        if (ext = '.txt') or (ext = '.log') then
        begin
          aList := TStringList.Create;
          aList.LoadFromFile(filename);
          {$IFDEF VISUAL_BUILD}
          OpenStringList(aList, f.FullDisplayName);
          {$ENDIF}
        end
        else if ext = '.png' then
        begin
          {$IFDEF VISUAL_BUILD}
          ShowFileInImageViewer(filename, Format('Gene Contribution Weights for %s', [TargetNodeName]));
          {$ENDIF}
        end;
      end
      else
        Result := False;
    end
    else
      Result := False;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TEslLinker.GetInputAlignmentsPath: String;
begin
  Result := TempEslDirectory + PathDelim + FInputAlignmentsDirectory;
end;

{$IFDEF VISUAL_BUILD}
function TEslLinker.GetGridImageFile: String;
begin
  Result := FGridImage;
end;
{$ENDIF}

function TEslLinker.ComputeCmdArgs(KeyValues: TStrings; var Args: TStringList): Boolean;
begin
  Args.Clear;
  Args.Assign(FCommandLineOptions);
  Args.Insert(0, '--tree=' + FUserTreeFile);
  Args.Add('--output=' + ExtractFileName(FEslOutputDirectory));
  Args.Add('--clade_list=' + FCladeListFile);
  Args.Add(FAlignmentsListFile + '= ');
  Result := True;
end;

function TEslLinker.CopyDataToExecEnvironment: Boolean;
begin
  Result := False;
  if FNewickString <> EmptyStr then
    if not CopyNewickToExecEnvironment then
      raise Exception.Create('failed to copy tree to temp directory');
  if not CopyCladeListToExecEnvironment then
    raise Exception.Create('failed to copy clade list');
  FAlignmentsListFile := FEslInputDirectory + PathDelim + ExtractFileName(FInputAlignmentsDirectory) + '.txt';
  Result := True;
end;

function TEslLinker.CopyCladeListToExecEnvironment: Boolean;
var
  aFile: TextFile;
begin
  Result := False;
  try
    try
      FCladeListFile := ChompPathDelim(FEslInputDirectory) + PathDelim + 'clade_list.txt';
      AssignFile(aFile, FCladeListFile);
      Rewrite(aFile);
      WriteLn(aFile, FTargetNodeName);
    except
      on E:Exception do
        FLog.Add('Error writing --clade_list file. Will continue with the analysis anyway as this file is normally not essential. Error was: ' + E.Message);
    end;
  finally
    CloseFile(aFile);
  end;
  Result := FileExists(FCladeListFile);
end;

function TEslLinker.CopyNewickToExecEnvironment: Boolean;
var
  aFile: TextFile;
begin
  Result := False;
  try
    try
      FUserTreeFile := FEslInputDirectory + PathDelim + ESL_NEWICK;
      AssignFile(aFile, FUserTreeFile);
      Rewrite(aFile);
      WriteLn(aFile, FNewickString);
      Result := FileExists(FUserTreeFile);
    except
      on E:Exception do
      begin
        Result := False;
        FLog.Add('Error when writing newick string to file: ' + E.Message);
      end;
    end;
  finally
    CloseFile(aFile);
  end;
end;

function TEslLinker.CollectResultsFiles: Boolean;
var
  i: Integer = -1;
  aList: TStringList = nil;
  f: TAppLinkerResultsFile = nil;
  aPath: String = '';
  aName: String = '';
  regex: TRegExpr = nil;
begin
  Result := False;
  if not DirectoryExists(FEslOutputDirectory) then
  begin
    FLog.Add(Format('Results directory (%s) not found', [FEslOutputDirectory]));
    Exit;
  end;

  try
    aList := FindAllFiles(FEslOutputDirectory);
    if aList.Count > 0 then
    begin
      { if MyESL used a partition-wise approach we need to skip all partition-specific files and only keep the final results files}
      regex := TRegExpr.Create('_part\d+$'); { matches strings that end with "_part" followed directly by an integer}
      for i := aList.Count - 1 downto 0 do
      begin
        aPath := aList[i];
        if DirectoryExists(aPath) then
        begin
          if regex.Exec(aPath) then
            aList.Delete(i);
        end
        else
        begin
          aPath := ExtractFileDir(aPath);
          if regex.Exec(aPath) then
            aList.Delete(i);
        end;
      end;

      for i := 0 to aList.Count - 1 do
      begin
        f := nil;
        aPath := aList[i];
        aName := ExtractFileNameOnly(aPath);
        if aName.StartsWith('M-Grid') then
        begin
          if ExtractFileExt(aPath) = '.png' then
          begin
            f := TAppLinkerResultsFile.Create(aPath, DR_PHYLO_MGRID, aName, FTargetNodeName);
            f.DisplayOrder := 1;
            FGridImage := f.FilePath;
          end
          else
          begin
            f := TAppLinkerResultsFile.Create(aPath, 'Model Grid (Gene Contribution Weights - Textual)', aName, FTargetNodeName);
            f.DisplayOrder := 2;
          end;
        end
        else if aName.StartsWith('PSS') and aName.EndsWith('_summary') then
        begin
          f := TAppLinkerResultsFile.Create(aPath, 'PSS (Position Sparsity Score)', aName, FTargetNodeName);
          f.DisplayOrder := 3;
        end
        else if aName.StartsWith('SPS') and aName.EndsWith('_summary') then
        begin
          f := TAppLinkerResultsFile.Create(aPath, 'SPS-SPP (Species Prediction Score and Probability)', aName, FTargetNodeName);
          f.DisplayOrder := 4;
        end
        else if aName.StartsWith('GSS') and aName.EndsWith('_summary') then
        begin
          f := TAppLinkerResultsFile.Create(aPath, 'GSS (Group Sparsity Score)', aName, FTargetNodeName);
          f.DisplayOrder := 5;
        end
        else if aName.EndsWith('DrPhylo') and (ExtractFileExt(aPath) = '.log') then
        begin
          f := TAppLinkerResultsFile.Create(aPath, 'DrPhylo Run Log', aName, FTargetNodeName);
          f.DisplayOrder := 6;
        end;

        if Assigned(f) then
        begin
          f.TargetNodeName := FTargetNodeName;
          FResultsFiles.Add(f);
        end;
      end;
    end;
    Result := FResultsFiles.Count > 0;
    if Result then
      FResultsFiles.Sort(@CompareEslResultsFiles);
  finally
    if Assigned(aList) then
      aList.Free;
    if Assigned(regex) then
      regex.Free;
  end;
end;

constructor TEslLinker.Create(Owner: TComponent);
begin
  inherited Create(nil);
  {$IFDEF VISUAL_BUILD}
  FCaptionViewer := nil;
  {$ENDIF}
  FCladeSize := 0;
  FNumSequences := 0;
  FNumDomains := 0;
  FNumGridRows := 0;
  FNumGridColumns := 0;
  FSegmentSize := 0;
  ParentThread := nil;
  FNeedToSaveResults := False;
  FUnzipper := TUnZipper.Create;
  FUserTreeFile := EmptyStr;
  FUserInputDataDirectory := EmptyStr;
  FLog := TStringList.Create;
  FOrigWorkingDir := GetCurrentDir;
  FInputAlignmentsDirectory := 'alignment_files';
  FIsSuccess := False;
  FResultsFiles := TList.Create;
  MyLinkName := 'TEslLinker';
  FCommandLineOptions := TStringList.Create;
end;

destructor TEslLinker.Destroy;
var
  i: Integer = -1;
begin
  FPrepareEslInputData := nil; // automatically freed due to reference counting of interfaces
  NewLineProc := nil;
  NewLineCheckCancelFunc := nil;
  UpdateStatusProc := nil;
  UpdateProgressProc := nil;
  ShowAnalysisOptionsProc := nil;
  if Assigned(MAI) then
  begin
    if Assigned(MAI.ARP) then
    begin
      MAI.ARP.Free;
      MAI.ARP := nil;
    end;
    FreeAndNil(MAI);
  end;
  if Assigned(FLog) then
    FLog.Free;

  if Assigned(FResultsFiles) then
  begin
    if FResultsFiles.Count > 0 then
    begin
      for i := FResultsFiles.Count - 1 downto 0 do
        TAppLinkerResultsFile(FResultsFiles[i]).Free;
      FResultsFiles.Clear;
    end;
    FResultsFiles.Free;
  end;

  if Assigned(FUnzipper) then
    FUnzipper.Free;
  if Assigned(FCommandLineOptions) then
    FCommandLineOptions.Free;
  RemoveTempResultsDirectory;
  inherited Destroy;
end;

procedure TEslLinker.Execute;
{$IFNDEF VISUAL_BUILD}
var
  filename: String = '';
{$ENDIF}
begin
  FStartTime := Now;

  try
     PrepareInputData;
     RunAppLink;
     FIsSuccess := (AppExitCode = 0);
     {$IFDEF VISUAL_BUILD}
     if FLog.Count > 0 then
       FLog.SaveToFile(FEslOutputDirectory + PathDelim + 'DrPhylo.log');
     {$ENDIF}
     if FIsSuccess then
     begin
       if not CollectResultsFiles then
         raise Exception.Create('failed to retrieve results files');
     end
     else
     begin
       ErrorCode := UnknownErr;
       FLog.Add(Format('ESL process finished with abnormal exit code %d', [AppExitCode]));
       FLog.Add('ESL process was launched with the following command line:');
       FLog.Add(#9 + FCommandLine);
       CleanUpTempFiles
     end;
  except
    on E: Exception do
    begin
      ErrorCode := FatalException;
      FIsSuccess := False;
      FLog.Add('Error when executing the MEGA ESL linker: ' + E.Message);
    end;
  end;
  {$IFNDEF VISUAL_BUILD}
  filename := NextAvailableFilenameNV('_esl.log');
  FLog.SaveToFile(filename);
  {$ENDIF}
  FEndTime := Now;
end;


{$IFNDEF VISUAL_BUILD}
function TEslLinker.MoveResultsFiles(targetDir: String): Boolean;
var
  f: TAppLinkerResultsFile = nil;
  i: Integer = -1;
  ext: String = '';
  newPath: String = '';
begin
  Result := True;
  if FResultsFiles.Count > 0 then
    for i := 0 to FResultsFiles.Count - 1 do
    begin
      f := TAppLinkerResultsFile(FResultsFiles[i]);
      if FileExists(f.FilePath) then
      begin
        newPath := targetDir + PathDelim + ExtractFileName(f.FilePath);
        Result := Result and CopyFile(f.FilePath, newPath);
      end;
    end;

  if DirectoryExists(FEslOutputDirectory) then
    DeleteDirectory(FEslOutputDirectory, False);
end;
{$ENDIF}


function TEslLinker.GetResultsFilePaths: TStringList;
var
  i: Integer = -1;
  f: TAppLinkerResultsFile = nil;
begin
  Result := TStringList.Create;
  if FResultsFiles.Count > 0 then
    for i := 0 to FResultsFiles.Count - 1 do
    begin
      f := TAppLinkerResultsFile(FResultsFiles[i]);
      Result.Add(f.FilePath);
      if ExtractFileExt(f.FilePath) = '.png' then
        FGridImage := f.FilePath;
    end;
end;

function TEslLinker.InitializeEslExecutionDir: Boolean;
begin
  Result := False;
  try
    if not DirectoryExists(FEslRootDirectory) then
      raise Exception.Create(Format('failed to initialize execution environment because the ESL root directory (%s) does not exist', [FEslRootDirectory]));

    if not DirectoryIsWritable(FEslRootDirectory) then
      FLog.Add('ESL rot directory appears to NOT be writeable. Will try to continue anyway.');

    FEslInputDirectory := NextAvailableFilename(FEslRootDirectory + PathDelim + 'esl_input');
    if not ForceDirectory(FEslInputDirectory) then
      raise Exception.Create('failed to create the ESL input data directory');

    FEslOutputDirectory := NextAvailableFilename(FEslRootDirectory + PathDelim + 'esl_output');
    if not ForceDirectory(FEslOutputDirectory) then
      raise Exception.Create('failed to create the ESL output directory');

    if not ExtractDrPhyloArchive then
      raise Exception.Create('failed to extract the ESL binary files');
    Result := True;
  except
    on E:Exception do
    begin
      FLog.Add(E.Message);
      FIsSuccess := False;
      Result := False;
    end;
  end;
end;

function TEslLinker.RemoveTempExecutionDir: Boolean;
begin
  try
    if DirectoryExists(FEslInputDirectory) then
      Result := DeleteDirectory(FEslInputDirectory, False)
    else
      Result := False;
  except
    on E:Exception do
    begin
      FLog.Add('Failed to remove temp files: ' + E.Message);
    end;
  end;
end;

function TEslLinker.GetOptions: TStrings;
begin
  Result := inherited GetOptions;
end;

procedure TEslLinker.OptionsFinalize;
begin

end;

procedure TEslLinker.UpdateStatus(NewLine: AnsiString);
begin
  FLog.Add(NewLine);
  {$IFNDEF VISUAL_BUILD}
  WriteLn(NewLine);
  {$ENDIF}
end;

function TEslLinker.ErrorCodeMessage: String;
begin
  case ErrorCode of
     noOutput: Result := 'ESL binary did not produce any output!';
     notAbleToSave: Result := 'ESL not able to write results files.';
     notFinished: Result := 'ESL did not complete.';
     noLogFile: Result := 'ESL log missing.';
     AppLinker.FatalException: Result := 'ESL has encountered a Fatal Exception.';
     notEnoughMemory: Result := 'Computer reported lack of enough memory (RAM).';
     noError: Result := 'no error';
     else
       Result := 'unknown error code';
  end;
end;

end.

