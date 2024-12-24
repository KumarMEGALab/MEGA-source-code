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

unit MD_MegaMain;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$M+}
interface
uses
  LCLIntf, LCLType, LResources,

  KeywordConsts, MD_InputSeqData, MD_InputDistData, MVS_SeqDataExplorer,
  Classes, SysUtils, Controls, Forms, Dialogs, Menus, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, ActnList, FileUtil, MLegendGenerator,
  DateUtils, StrUtils, MGlobalSettings, MegaConsts, MProcessPack, MAnalysisSummary,
  MOtuInfo, mruntimeprogressdlg, syncobjs;

type
  TMegaAction = (maFindAlignment, maFindTrees, maFindTipDates, maProcessCommands, maConcatenateAlignments,
                 maCloneFinder, maDeveloper, maCompareBootstrapTrees, maFindSitePatterns, maCompareNodeHeights,
                 maNewickToTabular, maCompareTopologies, maComputeRfDistance, maPruneTimetree, maDrPhylo,
                 maDomainsToFasta, maSegmentsToFasta);

  { TD_MegaMain }

  TD_MegaMain = class
    private
      FBicDelta: Double;
      FAiccDelta: Double;
      FMaxExecutionTimeInSeconds: Integer;
      FHasTimeLimit: Boolean;
      FDateFormatIsUserDefined: Boolean;
      FDateFormatStr: String;
      FConcatenateFilesList: TStringList;
      FRuntimeProgress: TRuntimeProgress;
      FMegaAction: TMegaAction;
      FOtuInfos: TAllOtuInfo;
      FAnalysisSummary: TAnalysisSummary;
      AnalysisPreferences: TStringList;
      FProcessPack: TProcessPack;
      FMapReducePartitionsFile: String;
      FMapReduceMaoFile: String;
      FMapReduceSeqsFile: String;
      FMapReduceOutputDataFile: String;

      {$IFNDEF MSWINDOWS}
      procedure UpdatePeakMemUsage;
      {$ENDIF}
      procedure DomainsToFasta;
      procedure SegmentsToFasta;
    function ThreadsStillRunning: Boolean;
    function GetCodeTable: AnsiString;
    function GetCodeTableName: AnsiString;
    function GetDataDescr: AnsiString;
    function GetDataFileName: AnsiString;
    procedure SetDataFileName(FileName: AnsiString);
    function GetDataTitle: AnsiString;
    procedure SetDataTitle(DataTitle: AnsiString);
    procedure SetCodeTable(AValue: AnsiString);
    procedure SetCodeTableName(AName: AnsiString);
    function GetAnalysisValue(Option: AnsiString): AnsiString;
    function NormalizeFilePath(FileName: AnsiString): AnsiString;
    function ParseCommandLine: Boolean;
    procedure OutputVersionInfo;
    procedure VerifyGroupsFile;
    function VerifyInputDataFile: Boolean;
    function VerifyAnalysisPreferencesFile: Boolean;
    function VerifyControlFile: Boolean;
    function VerifyTreeFile: Boolean;
    function VerifyPruneTimetreeFiles: Boolean;
    function VerifyAlignmentConcatenationInputs: Boolean;
    function ConstructProcessPack: Boolean;
    procedure ConstructDummyProcessPack;
    procedure CheckDeprecatedMaoFileOptions;
    function GetDefaultOutputFormat:TOutputFileType;
    procedure InitializeAnalysisSummary;
    procedure InitializeData(FileName: String);
    procedure InitializeGroups;
    procedure SetOtuInfos;
    procedure FindTipDates;
    procedure FindAlignment;

    procedure FindTrees;
    procedure ComputeRfDistance;
    procedure ConcatenateAlignments;
    procedure CompareBootstrapTrees;
    procedure CompareNodeHeights;
    procedure CompareTopologies;

    function CommandLineString: String;
    function CheckCancelStub(Progress: integer; Status: AnsiString): boolean;
    procedure InitializeMegaStdOut;
    procedure FinalizeMegaStdOut;
    procedure WriteFinalProgress;
    procedure NewickToTabular;
  public
    ForceMaxLbsReps: Boolean;
    ForceMaxLbsSamples: Boolean;
    TargetNodeName: String;
    NumStartingMpTrees: Integer;
    MultipleStartTrees: Boolean;
    TargetDirectory: String;
    SegmentSize: Integer;
    AddBclStdDevToNewick: Boolean;
    FirstTree: String;
    SecondTree: String;
    ExportSearchTrees: Boolean;
    SkipReplicateTreeSwapping: Boolean;
    DoBootstrapDoubleCheck: Boolean;
    PercentOfSitePatterns: Double;
    KeepTreeFixed: Boolean;
    SnvFreqsFile: String;
    PhyloQFileName: AnsiString;
    FocalSequence: String;
    OtuNamesFile: String;
    IsFileIterator: Boolean;
    BenchMarksFile: String;
    DoAllFiles: Boolean;
    DataFileList: String;
    SkipMinMax: Boolean;
    PartitionFrequencyCutoff: Double;
    DoPruneTreeSlow: Boolean;
    NamesMapFile: String;
    NodeIdsFile: String;
    ExportToSvg: Boolean;
    PruneTree: Boolean;
    FDataType :TSnTokenCode;
    FFileShouldContainCoding : Boolean;
    FRunSilent: Boolean;
    ReadFastaAs : array of TSnTokenCode;
    GroupsFilename: String;
    OutputFileName : AnsiString;
    OutputFormat : TOutputFileType;
    AnalysisPreferencesFileName : AnsiString;
    CommandLineTreeFileName: AnsiString;
    CommandLineControlFileName: AnsiString;
    recursiveSearch: Boolean;
    DataInfoGridNV : TStringList;
    MaxNumResults: Integer;
    IsRunningInTestHarness: Boolean;
    constructor Create;
    destructor Destroy; override;
    function GetCommandLineStr: String;
    function UserHasProvidedOutgroupFile: Boolean;
    procedure SiteCoverageThreadDone(aThread: TObject);
    procedure ModelTestDone(aThread: TObject);
    procedure AdaptiveModelTestDone(aThread: TObject);
    procedure DistCommandThreadDone(aThread: TObject);
    procedure DistTreeThreadDone(aThread: TObject);
    procedure EslThreadDone(aThread: TObject);
    procedure RunDrPhylo;
    procedure PrintHelp;
    procedure Run;
    procedure ExecuteCustomMegaAction;
    procedure ConcatenateAlignmentsDone(aThread: TObject);
    function MegaFileDataTypeString: String;
    function HasActiveData: Boolean;
    function HasAminoAcidData: Boolean;
    function HasCodonData: Boolean;
    function HasDistanceData: Boolean;
    function HasMultiFileData: Boolean;
    function HasNucleotideData: Boolean;
    function HasSequenceData: Boolean;
    procedure DataInfoGridClearAll;
    procedure InitializeIfNotInitialized;
    procedure DataInfoGridSetInfo(InfoType, Info: AnsiString);
    procedure DataInfoGridAddIfDoesntExist(InfoType: AnsiString);
    function DataInfoGridHasInfo(InfoType: AnsiString): Boolean;
    procedure DataInfoGridSetInfoObject(InfoType: AnsiString; Info: Pointer);
    procedure DataInfoGridRemoveInfo(InfoType: AnsiString);
    function DataInfoGridGetInfo(InfoType: AnsiString): AnsiString;
    function DataInfoGridGetInfoObject(InfoType: AnsiString): Pointer;
    function IsAnalysisAllowed(AnalysisName: AnsiString): Boolean;
    procedure UpdateFileInformation(FileName : AnsiString = ''; Title : AnsiString = ''; Description: AnsiString = ''; FDType : TSnTokenCode  = snNucleotide);
    function parseDirectoryOrLoadAFileList(Filename : AnsiString; var dataFiles, TreeFiles: TStringList; requireTrees, recursive: Boolean): Boolean;
    function LoadTreeFileList(RootDir: AnsiString; var TreeFileList: TStringList; DoRecursive: Boolean): Boolean;
    property AnalysisSummary: TAnalysisSummary read FAnalysisSummary write FAnalysisSummary;
  published

    property CodeTable:       AnsiString  read GetCodeTable     write SetCodeTable;
    property CodeTableName:   AnsiString  read GetCodeTableName write SetCodeTableName;
    property DataFileName:    AnsiString  read GetDataFileName  write SetDataFileName;
    property DataTitle:       AnsiString  read GetDataTitle     write SetDataTitle;
    property DataDescription: AnsiString  read GetDataDescr;
    property RunSilent: Boolean read FRunSilent write FRunSilent;
    property ProcessPack: TProcessPack read FProcessPack;
    property MegaAction: TMegaAction read FMegaAction;
    property DateFormatStr: String read FDateFormatStr;
    property DateFormatIsUserDefined: Boolean read FDateFormatIsUserDefined;
  end;

  {$IFDEF DEBUG}
  procedure WriteToDebugLog(aMsg: String);
  {$ENDIF}
var
  D_MegaMain: TD_MegaMain;
  ExecutionStartTime: TDateTime;
  LastDeveloperDataDumpTime: TDateTime;
  DeveloperDataDumpInterval: Integer = -1;
  UseFormattedConsoleOutput: Boolean = False;

implementation

uses
  mstringbuilder, MUsageStatistics, MAnalysisWizard, mseqdataexport,
  MegaUtils, MegaUtils_NV, ProcessInputData, MD_Align,
  TypInfo, MegaAnalysisPrefStrings, MegaVerConsts, MFileUtils, math,
  mtipdatesfinder, mnexusalignmentconverter, nexustreeconverter, esl_linker,
  alignmentconcatenator, mparse_mao_file, mega_citation, mcompare_bootstrap_trees,
  mdist_command_finalize, mdist_command_threads, Video, mmega_std_out, ShowTrees, MTreeList,
  mtabular_tree_export, MTreeData, mseqexportoptions, MDomainInfo, msitecoverage,
  mtree_display_setup, madaptive_model_test, MLSearchThread, MAnalysisInfo,
  mrobinson_foulds_metric, mtimetree_pruner;

procedure WriteToDebugLog(aMsg: String);
var
  aFile: TextFile;
  filename: String;
begin
  try
    filename := ExpandFileName('megacc.log');
    AssignFile(aFile, filename);
    Rewrite(aFile);
    WriteLn(aFile, aMsg);
  finally
    CloseFile(aFile);
  end;
end;

procedure TD_MegaMain.Run;
var
  dataFileList: TStringList =  nil;
  treeFileList: TStringList = nil;
  i: Integer;
  {$IFDEF DEBUG}
  FPUExceptionMask: TFPUExceptionMask;
  {$ENDIF}
begin
  LastDeveloperDataDumpTime := Now;
  {$IFDEF DEBUG}
  {$SafeFPUExceptions ON}
  FPUExceptionMask := GetExceptionMask;
  SetExceptionMask(FPUExceptionMask - [exOverflow, exUnderFlow, exZeroDivide]);
  {$ENDIF}
  D_InputSeqData := nil;
  D_InputDistData := nil;
  BenchmarksFile := EmptyStr;
  Randomize;
  NumActiveThreadsCS := TCriticalSection.Create;
  SkipMinMax := False;
  SuppressStopCodonsMessage := False;
  PartitionFrequencyCutoff := 0.5;
  DoPruneTreeSlow := False;
  ExportToSvg := False;
  PruneTree := False;
  CommandLineControlFileName := EmptyStr;
  GroupsFilename := EmptyStr;
  DataInfoGridNV := nil;
  FAnalysisSummary := TAnalysisSummary.Create; // Will be re-initalized when we call 'InitializeAnalysisSummary';  Left this in because I'm not sure if we ALWAYS call InitializeAnalysisSummary.
  FormatSettings.DecimalSeparator := '.';
  RunSilent := False;
  FProcessPack := nil;
  FFileShouldContainCoding := False; //When user using fasta file we assume non coding unless we see coding flag
  OutputFormat := ExportNone; //By default we don't specify a type;
  MaxNumResults := DefaultMaxResults;
  RunVerbose := true; // true by default, but the user can set to false from the command line.

  if not ParseCommandLine() then
    Exit;
  try
    if UseFormattedConsoleOutput then
      InitializeMegaStdOut;
  except
    on E:Exception do
      warn_NV('Error when initializing stdout: ' + E.Message);
  end;

  ExecutionStartTime := Now;
  if FMegaAction = maProcessCommands then
  begin
    if not VerifyAnalysisPreferencesFile() then
      Exit;
    VerifyControlFile;
    VerifyGroupsFile;

    if not RunSilent then
    begin
      DoTextOut(1, NextProgressLine, VER_MEGA_MAJOR + VER_MEGA_MINOR + ' Molecular Evolutionary Genetics Analysis', LightCyan);
      DoTextOut(1, NextProgressLine, 'Build#: ' + VER_MEGA_BUILD, LightCyan);
      DoTextOut(1, NextProgressLine, Format('%s %s', [DateToStr(Now), TimeToStr(Now)]), LightCyan);
    end;

    if (OutputFormat = ExportInvalid) then
      Warn_NV('Invalid output format specifed, you may choose from "Excel", "CSV", or "Text" where applicable... using default format');

    try
      try
        ConstructProcessPack();
        dataFileList := TStringList.Create;
        treeFileList := TStringList.Create;
        VerifyInputDataFile;
        {$IFNDEF MSWINDOWS}
        UpdatePeakMemUsage;
        {$ENDIF}
        if FProcessPack.NeedsTreeFileOnly then
        begin
          DataFileName := FProcessPack.TreeFile;
          if not LoadTreeFileList(FProcessPack.TreeFile, DataFileList, recursiveSearch) then
            DataFileList.Text := FProcessPack.TreeFile;
          if dataFileList.Count = 0 then
            Error_nv('Failure: No tree files found to process.');
          if DataFileList.Count > 1 then
            IsFileIterator := True;
          for i := 0 to DataFileList.Count-1 do
          begin
            While ThreadsStillRunning do  //Reference count every thread
            begin
              Application.ProcessMessages;
              CheckSynchronize;
            end;
            try
              FProcessPack.TreeFile := DataFileList.Strings[i];
              InitializeAnalysisSummary;
              FProcessPack.PerformAnalysis();  //Using the ProcessPack call the appropriate analysis method, this takes care of EVERYTHING else in MEGA
            except
              on E:Exception do
              begin
                if not IsFileIterator then
                  error_nv('failed to run analysis', E);
              end;
            end;
          end;
        end
        else
        begin
          if not parseDirectoryOrLoadAFileList(DataFileName, dataFileList, treeFileList, FProcessPack.needsTree, recursiveSearch) then
          begin
            dataFileList.Text := DataFileName;
            VerifyTreeFile;
            treeFileList.Text := FProcessPack.TreeFile;
          end;

          if dataFileList.Count = 0 then
            Error_nv('Failure: No data files found to process.');
          if dataFileList.Count > 1 then
            IsFileIterator := True;
          for i := 0 to dataFileList.Count-1 do
          begin
            While ThreadsStillRunning do  //Reference count every thread
            begin
              Application.ProcessMessages;
              CheckSynchronize;
            end;
            setDataFileName(dataFileList.Strings[i]);
            if FProcessPack.needsTree then
            begin
              FProcessPack.TreeFile := treeFileList.Strings[i];
              if trim(FProcessPack.TreeFile) = EmptyStr then
                Error_nv('Failure: No tree file specified.  Tree file is required for this specific analysis.');
            end;
            try
              InitializeData(DataFileName);
              InitializeAnalysisSummary;
              FProcessPack.PerformAnalysis();
              Sleep(2000); // a kludge to allow for some finalization to be done in the main thread. Otherwise, the next analysis may be launched and new data loaded before the previous analysis is fully completed
            except
              on E: Exception do
              begin
                if not IsFileIterator then
                  error_nv('Failed to run analysis', E);
              end;
            end;
          end;
        end;
       except
      on E : Exception do
        begin
          error_nv(E.Message, E);
        end;
      end;

      try
        While ThreadsStillRunning do
        begin
          if Assigned(Application) then
          begin
            Application.ProcessMessages;
            CheckSynchronize;
          end;
        end;
        if Assigned(Application) then
        begin
          Application.ProcessMessages;
          CheckSynchronize;
          Application.ProcessMessages;
        end;
        {$IFDEF DEBUG}
        if Assigned(DeveloperLog) and (DeveloperLog.Count > 0) then
          DeveloperLog.SaveToFile(NextAvailableFilenameNV('_dev_log.txt'));
        {$ENDIF}
      except
        on E:Exception do
          error_nv('uncaught exception in main loop', E);
      end;
    finally
      if Assigned(FAnalysisSummary) then
        FreeAndNil(FAnalysisSummary);
      //FLAG That we are done
      //if (RunFromWebTOP) then
      //  SendToWebtop('Progress: DONE');
      if Assigned(D_InputSeqData) then
        D_InputSeqData.Free;
      if Assigned(D_InputDistData) then
        D_InputDistData.Free;
      if isDeveloper then
        if ProcessMemoryLog <> nil then
          ProcessMemoryLog.Destroy;
      if Assigned(DataInfoGridNV) then
        DataInfoGridNV.Free;
      if Assigned(FProcessPack) then
        FProcessPack.Free;
      if Assigned(VS_SeqDataExplorer) then
        VS_SeqDataExplorer.Free;
      if Assigned(NumActiveThreadsCS) then
        FreeAndNil(NumActiveThreadsCS);
      if not UseFormattedConsoleOutput then
        WriteLn(EmptyStr);
      ExitCode := 0;
      {$IFNDEF MPI_MEGA}
      WriteFinalProgress;
      {$ELSE}
      if CURRENT_PROCESS_ID = MASTER_PROCESS_ID then
        WriteFinalProgress;
      {$ENDIF}
    end;
  end
  else
  begin
    try
      try
        {$IFNDEF MSWINDOWS}
        UpdatePeakMemUsage;
        {$ENDIF}
        ExecuteCustomMegaAction;
      except
        on E:Exception do
          error_nv('uncaught exception in main loop', E);
      end;
    finally
      if Assigned(DataFileList) then
        DataFileList.Free;
      if Assigned(treeFileList) then
        treeFileList.Free;
      if Assigned(FAnalysisSummary) then
        FAnalysisSummary.Free;
      if Assigned(DataInfoGridNV) then
        DataInfoGridNV.Free;
      if Assigned(NumActiveThreadsCS) then
        FreeAndNil(NumActiveThreadsCS);
      if Assigned(D_InputSeqData) then
        D_InputSeqData.Free;
      if Assigned(D_InputDistData) then
        D_InputDistData.Free;
      {$IFNDEF MPI_MEGA}
      WriteFinalProgress;
      {$ELSE}
      if CURRENT_PROCESS_ID = MASTER_PROCESS_ID then
        WriteFinalProgress;
      {$ENDIF}
    end;
  end;
end;

procedure TD_MegaMain.ExecuteCustomMegaAction;
begin
  case FMegaAction of
    maFindAlignment, maFindTrees, maFindTipDates, maFindSitePatterns:
      begin
        if (not FileExists(DataFileName)) and (not DirectoryExists(DataFileName)) then
          error_nv(Format('Input data file not found: %s', [DataFileName]));
      end;
    maConcatenateAlignments:
      begin
        if (not DirectoryExists(DataFileName)) and (not FileExists(DataFileList)) then
          error_nv(Format('Input data files not found: %s %s', [DataFileName, DataFileList]));
      end;
  end;

  case FMegaAction of
    maDomainsToFasta:
      begin
        DomainsToFasta;
      end;
    maSegmentsToFasta:
      begin
        SegmentsToFasta;
      end;
    maDrPhylo:
      begin
        RunDrPhylo;
      end;
    maFindAlignment:
      begin
        FindAlignment;
      end;
    maFindTrees:
      begin
        FindTrees;
      end;
    maFindTipDates:
      begin
        FindTipDates;
      end;
    maConcatenateAlignments:
      begin
        VerifyAlignmentConcatenationInputs;
        ConcatenateAlignments;
        //Without being put in a while loop to CheckSynchronize, MEGA dies when it encounters a Synchronize in a threaded process (used tons in trees)
        //Just keep checking Synchronize forever, have no good way of telling when MEGA is done because some analyses are threaded
        While ThreadsStillRunning do
        begin
          Sleep(10);
          if Assigned(Application) then
          begin
            Application.ProcessMessages;
            CheckSynchronize;
          end;
        end;
      end;
    maCompareBootstrapTrees:
      begin
        CompareBootstrapTrees;
      end;
    maCompareNodeHeights:
      begin
        CompareNodeHeights;
      end;
    maCompareTopologies:
      begin
        CompareTopologies;
      end;
    maNewickToTabular:
      begin
        NewickToTabular;
      end;
    maComputeRfDistance:
      begin
        ComputeRfDistance;
      end
    else
      error_nv('Application error: invalid call to ExecuteCustomMegaAction - THIS IS A BUG!');
  end;
end;

procedure TD_MegaMain.InitializeAnalysisSummary;
var
  tempInt: Integer;
begin
  if not Assigned(FAnalysisSummary) then
    FAnalysisSummary := TAnalysisSummary.Create;
  FAnalysisSummary.DataFileName := '''' + DataFileName + '''';
  FAnalysisSummary.SettingsFileName := '''' + AnalysisPreferencesFileName + '''';
  FAnalysisSummary.DataType := FDataType;
  FAnalysisSummary.CommandLineString := GetCommandLineStr;
  if Assigned(FProcessPack) then
  begin
    if FProcessPack.TextualSettingsList.IndexOfName('Analysis') >= 0 then
      FAnalysisSummary.AnalysisName := FProcessPack.TextualSettingsList.Values['Analysis'];
    if FProcessPack.TextualSettingsList.IndexOfName('Statistical Method') >= 0 then
      FAnalysisSummary.Method := FProcessPack.TextualSettingsList.Values['Statistical Method'];
    if FProcessPack.TextualSettingsList.IndexOfName(opsMaxSampleSites) >= 0 then
    begin
      if TryStrToInt(FProcessPack.TextualSettingsList.Values[opsMaxSampleSites], tempInt) then
        FAnalysisSummary.BootstrapSites := tempInt;
    end;
  end;
end;

function TD_MegaMain.GetDefaultOutputFormat:TOutputFileType;
begin
  if FProcessPack.ContainsProcessType(ppPorportionInterDiversity) or
          FProcessPack.ContainsProcessType(ppAvgInSubPops) or
          FProcessPack.ContainsProcessType(ppInterPopDiversity) or
          FProcessPack.ContainsProcessType(ppWithinGpAvg) or
          FProcessPack.ContainsProcessType(ppAvgOverallPops) then
    Result := ExportText
  else if (FProcessPack.ContainsProcessType(ppDistEst) and FProcessPack.ContainsProcessType(ppPairTaxa)) or
          (FProcessPack.ContainsProcessType(ppDistEst) and FProcessPack.ContainsProcessType(ppBetweenGpAvg)) then
    Result := ExportMega
  else if FProcessPack.ContainsProcessType(ppFisherExact) then
    Result := ExportMega
  else if FProcessPack.ContainsProcessType(ppZtest) then
    Result := ExportMega
  else if FProcessPack.ContainsProcessType(ppNetBetweenGpAvg) then
    Result := ExportMega
 else Result := ExportCSV; // default output format
end;

function TD_MegaMain.parseDirectoryOrLoadAFileList(Filename: AnsiString;
  var dataFiles, TreeFiles: TStringList; requireTrees, recursive: Boolean
  ): Boolean;
var
  dirs, fileList: TStringList;
  i, j: Integer;
  curDir, curFilepath, curFilename, curExt: String;
  treeForDataFound: Boolean;
  tempDataFile, tempTreeFile: String;
begin
  result := false;

  // Check if file exists, if not check if it is a directory.
  if (not FileExists(Filename)) and DirectoryExists(Filename) then
  begin
    result := true;
    // parse the string to multiple directories
    dirs := TStringList.Create;
    dirs.Delimiter := '|';
    dirs.DelimitedText := filename;

    fileList := TStringList.Create;
    for i := 0 to dirs.Count-1 do
    begin
      curDir := dirs.strings[i];
      FindAllFilesNotFolders(curDir, faAnyFile, fileList, recursive);
    end;

    dirs.Free;

    // If we have a tree (same name as datafile, but with diff extension), add it to the list (will only use if asked for it)
    for i := 0 to fileList.Count-1 do
    begin
      curFilepath := fileList.Strings[i];

      curFilename := ExtractFileName(curFilePath);
      curExt := ExtractFileExt(curFilename);

      // Now we clasify the types of files we have based on extension
      if fileExtValidForAnalysis(curExt) then
      begin
        dataFiles.Add(curFilepath);
      end
      else if fileExtValidForTree(curExt) then
        treeFiles.Add(curFilepath);
    end;

    fileList.Free;

    if requireTrees then  // Should check if the analysis asks for tree files, if it does then we add tree files.
    begin
      i := 0;
      while i < dataFiles.Count do
      begin
        treeForDataFound := false;
        for j := 0 to treeFiles.Count-1 do
          if AnsiCompareText(ExtractFilePath(dataFiles.Strings[i]) + ChangeFileExt(ExtractFileName(dataFiles.Strings[i]), EmptyStr), ExtractFilePath(treeFiles.Strings[j]) + ChangeFileExt(ExtractFileName(treeFiles.Strings[j]), EmptyStr))  = 0 then
          begin
            treeForDataFound := true;
            treeFiles.Move(j,i);
            break;
          end;
        if not treeForDataFound then
        begin
          if (CommandLineTreeFileName <> EmptyStr) and FileExists(CommandLineTreeFileName) then
          begin
            treeFiles.Add(CommandLineTreeFileName);
            treeFiles.Move(treeFiles.Count-1, i);
            inc(i);
            //Warn_nv('Using tree file passed in from command line.');
          end
          else
          begin
            Warn_nv('Data file "' + dataFiles.Strings[i] + '" is missing a tree file.  Skipping datafile.');
            dataFiles.Delete(i);
          end;
        end
        else
          inc(i); // Increment ONLY if we do NOT remove an index.
      end;
      // Prune the extra tree files (which don't match up)
      while treeFiles.count > dataFiles.Count do
          treeFiles.delete(treeFiles.Count-1);
    end;


    //ShowMessage(IntToStr(dataFiles.Count) + 'data files (pruned for ones with trees if required).');
  end
  else
  begin
    if FileExists(Filename) and (ExtractFileExt(Filename) = '.txt') and (Filename <> SnvFreqsFile) then
    begin
      result := true;
      // read in the file, delimited by |
      fileList := TStringList.Create;
      fileList.LoadFromFile(Filename);
      for i := 0 to fileList.Count-1 do
      begin
        treeForDataFound := false;
        // Extract datafile and tree file (if tree file is there)
        if pos('|', fileList.Strings[i]) > 0 then
        begin
          tempDataFile := trim(copy(fileList.Strings[i], 0, pos('|', fileList.Strings[i])-1));
          tempTreeFile := trim(copy(fileList.Strings[i], pos('|', fileList.Strings[i])+1, length(fileList.Strings[i])));
          treeForDataFound := true;
        end
        else
          tempDataFile := trim(fileList.Strings[i]);

        // check if files exist and add to lists.
        if FileExists(tempDataFile) then
          dataFiles.Add(tempDataFile)
        else
          Warn_nv('Data filename specified on line ' + IntToStr(i + 1) + ' does not exist or can not be accessed at this time.');

        if TreeForDataFound and FileExists(tempTreeFile) then
        begin
          treeFiles.Add(tempTreeFile);
        end
        else
        begin
          if requireTrees then
          begin
            if (CommandLineTreeFileName <> EmptyStr) and FileExists(CommandLineTreeFileName) then
            begin
              treeFiles.Add(CommandLineTreeFileName);
            end
            else
            begin
              Warn_nv('Skipping File: The tree file on line ' + IntToStr(i + 1) + ' is missing or not a valid file path/name.  Trees are required for this analysis, so the data file is being skipped.');
              if DataFiles.Count > TreeFiles.Count then
                dataFiles.delete(dataFiles.Count-1);  // Take the data file out, which we just added in.
            end;
          end;
        end;
      end;
    end
  end;
end;

function TD_MegaMain.ConstructProcessPack: Boolean;
var
  settings: TStringList = nil;
begin
  Result := false;
  FProcessPack := TProcessPack.Create();
  FProcessPack.TreeFile := CommandLineTreeFileName; //Link the tree file specified into the process pack for later use
  if not ParseAnalysisPreferencesFile(AnalysisPreferencesFileName, FProcessPack, FFileShouldContainCoding) then
    raise Exception.Create('failed to parse the MEGA Analysis Options (.mao) file');
  FProcessPack.CalibrationFile := CommandLineControlFileName;
  FProcessPack.GroupsFile := GroupsFilename;
  if SkipMinMax then
    FProcessPack.AddProcessType(ppSkipMinMax);

  settings := FProcessPack.TextualSettingsList;

  if DoBootstrapDoubleCheck then
  begin
    if settings.Values[opsSubsamplingVerifyResult] <> EmptyStr then
      settings.Values[opsSubsamplingVerifyResult] := YesSlowStr
    else
      settings.Add(Format('%s=%s', [opsSubsamplingVerifyResult, YesSlowStr]));
  end;

  if MultipleStartTrees then
  begin
    if NumStartingMpTrees <= 0 then
      raise Exception.Create(Format('Invalid number of starting trees (%d) for -mst option', [NumStartingMpTrees]));
    if settings.Values[opsNumInitialTrees2] <> EmptyStr then
      settings.Values[opsNumInitialTrees2] := IntToStr(NumStartingMpTrees)
    else
      settings.Add(Format('%s=%s', [opsNumInitialTrees2, IntToStr(NumStartingMpTrees)]));

    if settings.Values[opsMLInitialTrees2] <> EmptyStr then
      settings.Values[opsMLInitialTrees2] := InitialParsimonyTreesStr
    else
      settings.Add(Format('%s=%s', [opsMLInitialTrees2, InitialParsimonyTreesStr]));
  end;

  if CompareValue(FBicDelta, 0, FP_CUTOFF) > 0 then
  begin
    if settings.Values[opsDeltaBicCutoff] <> EmptyStr then
      settings.Values[opsDeltaBicCutoff] := FloatToStr(FBicDelta)
    else
      settings.Add(Format('%s=%.8f', [opsDeltaBicCutoff, FBicDelta]));
  end;

  if CompareValue(FAiccDelta, 0, FP_CUTOFF) > 0 then
  begin
    if settings.Values[opsDeltaAicCutoff] <> EmptyStr then
      settings.Values[opsDeltaAicCutoff] := FloatToStr(FAiccDelta)
    else
      settings.Add(Format('%s=%.8f', [opsDeltaAicCutoff, FAiccDelta]));
  end;

  CheckDeprecatedMaoFileOptions;
  Result := true;
end;

procedure TD_MegaMain.ConstructDummyProcessPack;
begin
  FProcessPack := TProcessPack.Create;
  FProcessPack.TextualSettingsList.Add(Format('%s=%s', [IdenticalBaseSymbolStr, '.']));
  FProcessPack.TextualSettingsList.Add(Format('%s=%s', [MissingBaseSymbolStr, '?']));
  FProcessPack.TextualSettingsList.Add(Format('%s=%s', [GapSymbolStr, '-']));
end;

procedure TD_MegaMain.CheckDeprecatedMaoFileOptions;
begin
  if FProcessPack.TextualSettingsList.IndexOfName('Threshold for Support gte 95') >= 0 then
    error_nv('The .mao file you are using has obsolete options. Please use the MEGA prototyper to generate new .mao files');
  if FProcessPack.TextualSettingsList.IndexOfName('Threshold for Std Err of Support gte 95') >= 0 then
    error_nv('The .mao file you are using has obsolete options. Please use the MEGA prototyper to generate new .mao files');
end;

procedure TD_MegaMain.InitializeData(FileName: String);
begin
  DataFilename := Filename;
  case FProcessPack.ProcessTypes[0] of  // assumes that if alignment is to be done, it is the first process type in ProcessPack
    ppAlign:
      LoadAlignmentDataFromFile(DataFileName, FDataType);
    ppAnalyzeUserTree:
    begin
      if FProcessPack.HasProcessType(ppLbsAnalyzeTree) then
        DoOpenDataFile(FileName)
      else
      if (FProcessPack.ProcessTypes[1] = ppML) or (FProcessPack.ProcessTypes[1] = ppLeastSquares) then
        DoOpenDataFile(FileName)
      else if (FProcessPack.ProcessTypes[1] = ppMP) then
        DoOpenDataFile(FileName)
      else if FProcessPack.ProcessTypes[2] = ppPhyloQ then
      begin
        LoadAlignmentDataFromFile(DataFileName, FDataType); // for PhyloQ analysis we need both
        DoOpenDataFile(FileName);
      end
      else
        Error_NV('Could not determine statistical method to use for user tree analysis');
    end
    else
      DoOpenDataFile(FileName); //Open the file that was specified in the command line for analysis
  end;

  //If a genetic code is specified for analysis set the code table as well as the name
  if (FProcessPack.TextualSettingsList.Values['Genetic Code'] <> EmptyStr) and (FProcessPack.TextualSettingsList.Values['Genetic Code'] <> 'Not Applicable') and (ProcessPack.TextualSettingsList.Values['Genetic Code Table'] <> EmptyStr) and (ProcessPack.TextualSettingsList.Values['Genetic Code Table'] <> 'Not Applicable') then
  begin
    D_InputSeqData.CodeName := FProcessPack.TextualSettingsList.Values['Genetic Code Table'];  //Set the Code Name
    D_InputSeqData.CodeTable := FProcessPack.TextualSettingsList.Values['Genetic Code'];
  end
  //If a genetic code is specified for alignment set the code table
  else if (FProcessPack.TextualSettingsList.Values['Genetic Code (when using cDNA)'] <> EmptyStr) and (FProcessPack.TextualSettingsList.Values['Genetic Code (when using cDNA)'] <> 'Not Applicable') then
    SetGeneticCode(FProcessPack.TextualSettingsList.Values['Genetic Code (when using cDNA)'])
  else if (FProcessPack.TextualSettingsList.Values['GeneticCodeTable'] <> EmptyStr) and (FProcessPack.TextualSettingsList.Values['GeneticCodeTable'] <> 'Not Applicable') then
    SetGeneticCode(FProcessPack.TextualSettingsList.Values['GeneticCodeTable']);


  if D_InputSeqData <> nil then
  begin
    if (not (FProcessPack.ProcessTypes[0] = ppAlign)) and D_InputSeqData.isCoding and D_InputSeqData.IsNuc and (ProcessPack.TextualSettingsList.IndexOfName('Select Codon Positions') = -1)  then
      FProcessPack.AddKeyValuePairSetting('Select Codon Positions', 'Select Codon Positions=1st, 2nd, 3rd, Non-Coding');
  end;

  // if the user did not give an output format or it was invalid, use the default type
  if (OutputFormat = ExportNone) or (OutputFormat = ExportInvalid) then
  begin
    OutputFormat := GetDefaultOutputFormat;
  end;
  if GroupsFilename <> EmptyStr then
    InitializeGroups;
end;

procedure TD_MegaMain.InitializeGroups;
var
  GroupsList: TStringList;
  i: Integer;
  AInfo: TOtuInfo;
  Taxon, Group: String;
begin
  GroupsList := nil;

  if Trim(GroupsFileName) = EmptyStr then
    Exit;
  if not FileExists(GroupsFilename) then
    error_nv('The specified groups file (' + GroupsFilename + ') was not found');

  try
    try
      SetOtuInfos; { sets FOtuInfos for the correct data type (sequence or distance data}
      GroupsList := TStringList.Create;
      GroupsList.LoadFromFile(GroupsFilename);
      if GroupsList.Count > 0 then
        for i := 0 to GroupsList.Count - 1 do
        begin
          Taxon := GroupsList.Names[i];
          Taxon := Trim(StringReplace(Taxon, '_', ' ', [rfReplaceAll])); { replace underscores with spaces because are names are handled internally this way}
          Group := GroupsList.ValueFromIndex[i];
          Group := Trim(StringReplace(Group, '_', ' ', [rfReplaceAll]));
          if (Taxon = EmptyStr) or (Group = EmptyStr) then
            error_nv('Invalid group specification: ' + GroupsList.Names[i] + '=' + GroupsList.ValueFromIndex[i]);
          AInfo := FOtuInfos.GetOtuInfoByName(Taxon);
          if Assigned(AInfo) then
          begin
            AInfo.GpName := Group;
            if SameText(Group, 'outgroup') then
              AInfo.OutgroupMember := True;
          end
          else
            warn_nv('Invalid group information: ' + GroupsList.Names[i] + '=' + GroupsList.ValueFromIndex[i]);
        end;
    except
      on E:Exception do
        error_nv('An error occurred when loading the groups list: ' + E.Message, E);
    end;
  finally
    if Assigned(GroupsList) then
      GroupsList.Free;
  end;
end;

function TD_MegaMain.VerifyInputDataFile: Boolean;
begin
  Result := false;
  if FProcessPack.NeedsTreeFileOnly then
   begin
     Result := True;
     Exit;
   end;

  if not FileExists(DataFileName) then
  begin
    if DataFileName = EmptyStr then
    begin
      PrintHelp();
      error_nv('Error: Data file Not found.');
    end;
    If FileExists('./' + DataFileName) then
      DataFileName := './' + DataFileName
    else
    begin
      error_NV('The data file you specified (' + DataFileName + ') does not exist.  Please check your spelling and try again.');
      exit;
    end;
  end;
  Result := true;
end;

function TD_MegaMain.VerifyAnalysisPreferencesFile: Boolean;
begin
  Result := false;
  if Trim(AnalysisPreferencesFileName) = EmptyStr then
    Error_NV('The analysis (MAO) file you specified does not exist.  Please check your spelling and try again.');

  if (not FileExists(AnalysisPreferencesFileName)) or (ExtractFilePath(AnalysisPreferencesFileName) = EmptyStr) then
  begin
    if FileExists('./' + AnalysisPreferencesFileName) then
      AnalysisPreferencesFileName := './' + AnalysisPreferencesFileName
    else
    begin
      PrintHelp();
      FAnalysisSummary.CommandLineString := GetCommandLineStr;
      Error_NV('The analysis (MAO) file you specified does not exist.  Please check your spelling and try again.');
    end;
  end;
  Result := true;
end;

function TD_MegaMain.VerifyControlFile: Boolean;
begin
  if CommandLineControlFileName <> EmptyStr then
    If not FileExists(CommandLineControlFileName) then
    begin
      PrintHelp();
     error_nv('The specified calibration constraints file does not exist: ' + CommandLineControlFileName);
    end;
  Result := True;
end;

function TD_MegaMain.VerifyTreeFile: Boolean;
begin
  Result := false;
  if not FProcessPack.needsTree then
  begin
    Result := True;
    Exit;
  end;

  if not FileExists(CommandLineTreeFileName) then
  begin
   if CommandLineTreeFileName = EmptyStr then
   begin
     PrintHelp();
     error_nv('Error: Required tree file NOT found.');
   end;
   If FileExists('./' + CommandLineTreeFileName) then
     CommandLineTreeFileName := './' + CommandLineTreeFileName
   else
   begin
     error_NV('The tree file you specified (' + CommandLineTreeFileName + ') does not exist.  Please check your spelling and try again.');
     exit;
   end;
  end;
  Result := true;
end;

function TD_MegaMain.VerifyPruneTimetreeFiles: Boolean;
begin
  Result := False;
  if not FileExists(CommandLineTreeFileName) then
    error_nv('input tree file not found');
  if not FileExists(GroupsFilename) then
    error_nv('groups file for specifying outgroup not found');
  if not FileExists(DataFileName) then
    error_nv('alignment file not found');
  Result := True;
end;

function TD_MegaMain.VerifyAlignmentConcatenationInputs: Boolean;
begin
  Result := False;
  if (not DirectoryExists(GetDataFileName)) and (not FileExists(DataFileList)) then
    error_nv('Neither a directory of input files or a file that lists all input files was found. Aborting!');
  Result := True;
end;

function TD_MegaMain.NormalizeFilePath(FileName: AnsiString): AnsiString;
var
  NormalizedFileName: AnsiString;
begin
  NormalizedFileName := FileName;
  {$IFDEF DARWIN}
  if (StartsStr('.', FileName) or StartsStr('/', FileName)) then
     NormalizedFileName := ExpandFileName(FileName)
  else
     NormalizedFileName := GetCurrentDir + '/' + FileName;
  {$ELSE}
     NormalizedFileName := ExpandFileName(FileName);
  {$ENDIF}
  Result := NormalizedFileName;
end;

function TD_MegaMain.ParseCommandLine: Boolean;
var
  TempStr: AnsiString;
  i: Integer;
  j: Integer = -1;
  builder: TMegaStringBuilder = nil;
  aList: TStringList = nil;
begin
  FocalSequence := EmptyStr;
  KeepTreeFixed := False;
  PercentOfSitePatterns := -1;
  ExportSearchTrees := False;
  DoBootstrapDoubleCheck := False;
  SkipReplicateTreeSwapping := True;
  DoAllFiles := False;
  Result := false;
  CommandLineTreeFileName := EmptyStr;
  CommandLineControlFileName := EmptyStr;
  for i := 0 to ParamCount do
  begin
    TempStr := AnsiLowerCase(ParamStr(i));
    if (TempStr = '-d') or (TempStr = '--data') or (TempStr = '--directory') then
      SetDataFileName(NormalizeFilePath(ParamStr(i+1)))
    else if (TempStr = '-developer') or (TempStr = '--developer') then
    begin
      IsDeveloper := True;
    end
    else if (TempStr = '-test-harness') or (TempStr = '--test-harness') then
      IsRunningInTestHarness := True
    else if (TempStr = '-ebp') or (TempStr = '--export-bootstrap-precision') then
      AddBclStdDevToNewick := True
    else if (TempStr = '-eud') or (TempStr = '--export-usage-data') then
      ExportUsageData := True
    else if (TempStr = '-max-time-in-seconds') or (TempStr = '--max-time-in-seconds') then
    begin
      FHasTimeLimit := True;
      FMaxExecutionTimeInSeconds := StrToInt(ParamStr(i + 1));
    end
    else if (TempStr = '--newick-to-tabular') then
      FMegaAction := maNewickToTabular
    else if (TempStr = '--otu-names') then
      OtuNamesFile := NormalizeFilePath(ParamStr(i + 1))
    else if (TempStr = '-find-unique-site-patterns') or (TempStr = '--find-unique-site-patterns') then
      FMegaAction := maFindSitePatterns
    else if (TempStr = '-compare-bootstrap-trees') or (TempStr = '--compare-bootstrap-trees') or (TempStr = '-cbt') then
    begin
      FMegaAction := maCompareBootstrapTrees;
      FirstTree := NormalizeFilePath(ParamStr(i + 1));
      SecondTree := NormalizeFilePath(ParamStr(i + 2));
      if not FileExists(FirstTree) then
        error_nv('missing first tree file');
      if not FileExists(SecondTree) then
        error_nv('missing second tree file');
    end
    else if (TempStr = '-compare-node-heights') or (TempStr = '--compare-node-heights') then
    begin
      FMegaAction := maCompareNodeHeights;
      FirstTree := NormalizeFilePath(ParamStr(i + 1));
      SecondTree := NormalizeFilePath(ParamStr(i + 2));
      if not FileExists(FirstTree) then
        error_nv('missing first tree file');
      if not FileExists(SecondTree) then
        error_nv('missing second tree file');
      DataFileName := NormalizeFilePath(ParamStr(i + 3));
      if not FileExists(DataFileName) then
        error_nv('missing input data file');
    end
    else if (TempStr = '-compare-topologies') or (TempStr = '--compare-topologies') then
    begin
      FMegaAction := maCompareTopologies;
      FirstTree := NormalizeFilePath(ParamStr(i + 1));
      SecondTree := NormalizeFilePath(ParamStr(i + 2));
      if not FileExists(FirstTree) then
        error_nv('missing first tree file');
      if not FileExists(SecondTree) then
        error_nv('missing second tree file');
    end
    else if (TempStr = '-keep-tree-blens') or (TempStr = '--keep-tree-blens') then
      KeepUserTreeBLens := True
    else if (TempStr = '-ddi') or (TempStr = '--data-dump-interval') then
    begin
      DeveloperDataDumpInterval := StrToInt(ParamStr(i + 1));
    end
    else if (TempStr = '--all-seqs') or (TempStr = '-all-seqs') then
      DoAllFiles := True
    else if (TempStr = '-benchmarks-file') or (TempStr = '--benchmarks-file') then
      BenchMarksFile := NormalizeFilePath(ParamStr(i + 1))
    else if (TempStr = '-wlf') or (TempStr = '--write-log-files') then
      ShowLogFiles := True
    else if (TempStr = '-fce') or (TempStr = '--force-captions-export') then
      ForceCaptionsExport := True
    else if (TempStr = '-l') or (TempStr = '--list') then
    begin
      DataFileName := NormalizeFilePath(ParamStr(i + 1));
      DataFileList := NormalizeFilePath(ParamStr(i + 1));
    end
    else if (TempStr = '-pfc') or (TempStr = '--partition-frequency-cutoff') then
    begin
      try
        PartitionFrequencyCutoff := StrToFloat(ParamStr(i + 1));
        if (PartitionFrequencyCutoff < 0.0) or (PartitionFrequencyCutoff > 1.0) then
          raise Exception.Create(' valid values are between 0.0 and 1.0 (inclusive)');
      except
        on E:Exception do
          error_nv('An error occurred when parsing the partition frequency cutoff value: ' + E.Message);
      end;
    end
    else if (TempStr = '-g') or (TempStr = '--groups') or (TempStr = '--group') then
      GroupsFilename := NormalizeFilePath(ParamStr(i + 1))
    else if (TempStr = '-a') or (TempStr = '--analysisoptions') then
      AnalysisPreferencesFileName := NormalizeFilePath(ParamStr(i+1))
    else if (TempStr = '-t') or (TempStr = '--tree') then
      CommandLineTreeFileName := NormalizeFilePath(ParamStr(i+1))
    else if (TempStr = '-r') or (TempStr = '--recursive') then
      recursiveSearch := true
    else if (TempStr = '-skip_init_min_max') or (TempStr = '--skip_init_min_max') then
      SkipMinMax := True
    else if (TempStr = '-m') or (TempStr = '--maxresults') then
      try
        MaxNumResults := StrToInt(ParamStr(i + 1));
        if MaxNumResults <= 0 then
          raise Exception.Create('');
      Except
        MaxNumResults := DefaultMaxResults;
        Warn_NV('Unable to set maxResults, using default=' + IntToStr(DefaultMaxResults) + '. Was given: ''' + ParamStr(i + 1) + '''');
      end
    else if (TempStr = '-names-map') or (TempStr = '--names-map') then
      NamesMapFile := NormalizeFilePath(Trim(ParamStr(i+1)))
    else if (TempStr = '-ids') or (TempStr = '--ids') then
    begin
      NodeIdsFile := NormalizeFilePath(ParamStr(i+1));
    end
    else if (TempStr = '-o') or (TempStr = '--outfile') then
    begin
      OutputFileName := NormalizeFilePath(ParamStr(i+1));

      if ExtractFilePath(OutputFileName) <> EmptyStr then  //If they specified an absolute path
      begin
       TempStr := OutputFileName;
       if not((TempStr[length(TempStr)] = PathDelim) or DirectoryExists(TempStr))  then
         TempStr := ExtractFilePath(TempStr);

        if TempStr[length(TempStr)] <> PathDelim then
          TempStr := TempStr + PathDelim;
        if DirectoryExistsCreate(TempStr) then
        begin

        end;
      end
      else
      begin
        OutputFileName := ExtractFilePath(Application.ExeName) + OutputFileName;
      end;
    end
    else if (TempStr = '-c') or (TempStr = '--calibration') then
    begin
      CommandLineControlFileName := NormalizeFilePath(ParamStr(i+1));
    end
    else if (TempStr = '-fco') or (TempStr = '--formatted-console-output') then
      UseFormattedConsoleOutput := True
    else if (TempStr = '-f') or (TempStr = '--format') then
    begin
      if SameText(ParamStr(i+1), 'mega') then
        OutputFormat := ExportMega
      else if SameText(ParamStr(i+1), 'fasta') then
        OutputFormat := ExportFasta
      else
        OutputFormat := ExportInvalid;
      end
      else if (ParamStr(i) = '-h') OR (ParamStr(i) = '--help') OR (ParamStr(i) = '-help') then
      begin
        PrintHelp;
        exit;
      end
      else if (ParamStr(i) = '-n') or (ParamStr(i) = '--noSummary') then
      begin
        RunVerbose := False;
      end
      else if (ParamStr(i) = '-s') or (ParamStr(i) = '--silent') then
      begin
        RunSilent := True;
      end
      else if (ParamStr(i) = '-svg') or (ParamStr(i) = '--svg') then
      begin
        ExportToSvg := True;
      end
      else if (ParamStr(i) = '-pd') or (ParamStr(i) = '--phyloQData') then
      begin
        PhyloQFileName := ParamStr(i + 1);
      end
      else if (AnsiCompareText(ParamStr(i), '-nucleotide') = 0)    OR    //In case we get Fasta files we need to determine if Nucleotide or protein
              (AnsiCompareText(ParamStr(i), '-nuc') = 0)           OR
              (AnsiCompareText(ParamStr(i), '--nucleotide') = 0)   OR
              (AnsiCompareText(ParamStr(i), '--nuc') = 0)          then
      begin
        FDataType := snNucleotide;
      end
      else if (AnsiCompareText(ParamStr(i), '-amino') = 0)         OR    //In case we get Fasta files we need to determine if Nucleotide or protein
              (AnsiCompareText(ParamStr(i), '-aminoacid') = 0)     OR
              (AnsiCompareText(ParamStr(i), '-protein') = 0)       OR
              (AnsiCompareText(ParamStr(i), '-prot') = 0)          OR
              (AnsiCompareText(ParamStr(i), '--amino') = 0)        OR
              (AnsiCompareText(ParamStr(i), '--aminoacid') = 0)    OR
              (AnsiCompareText(ParamStr(i), '--protein') = 0)      OR
              (AnsiCompareText(ParamStr(i), '--prot') = 0)         then
      begin
         FDataType := snProtein;
      end
      else if (AnsiCompareText(ParamStr(i), '-coding') = 0)     OR
              (AnsiCompareText(ParamStr(i), '--coding') = 0)   then
      begin
        FFileShouldContainCoding := true;
        FDataType := snNucleotide;

      end
      else if (AnsiCompareText(ParamStr(i), '-ptt') = 0) or (AnsiCompareText(ParamStr(i), '--prune-timetree') = 0) then
      begin
        FMegaAction := maPruneTimetree;
      end
      else if (AnsiCompareText(ParamStr(i), '-verbose') = 0) then
        RunVerbose := True
      else if (ParamStr(i) = '-v') or (ParamStr(i) = '--version') then
      begin
       OutputVersionInfo;
       exit;
      end
      else if (ParamStr(i) = '-date-format') or (ParamStr(i) = '--date-format') then
      begin
        FDateFormatStr := ParamStr(i+1);
        FDateFormatIsUserDefined := True;
      end
      else if (ParamStr(i) = '-find-tip-dates') or (ParamStr(i) = '--find-tip-dates') then
      begin
        SetDataFileName(NormalizeFilePath(ParamStr(i+1)));
        FMegaAction := maFindTipDates;
      end
      else if (ParamStr(i) = '-find-alignment') or (ParamStr(i) = '--find-alignment') then
      begin
        SetDataFileName(NormalizeFilePath(ParamStr(i+1)));
        FMegaAction := maFindAlignment;
        FindAlignment;
        Exit;
      end
      else if (ParamStr(i) = '-find-trees') or (ParamStr(i) = '--find-trees') then
      begin
        SetDataFileName(NormalizeFilePath(ParamStr(i+1)));
        FMegaAction := maFindTrees;
        FindTrees;
        Exit;
      end
      else if (ParamStr(i) = '-dtf') or (ParamStr(i) = '--domains-to-fasta') then
      begin
        SetDataFileName(NormalizeFilePath(ParamStr(i + 1)));
        TargetDirectory := NormalizeFilePath(ParamStr(i + 2));
        FMegaAction := maDomainsToFasta;
      end
      else if (ParamStr(i) = '-stf') or (ParamStr(i) = '--segments-to-fasta') then
      begin
        SetDataFileName(NormalizeFilePath(ParamStr(i + 1)));
        TargetDirectory := NormalizeFilePath(ParamStr(i + 2));
        SegmentSize := StrToInt(ParamStr(i + 3));
        FMegaAction := maSegmentsToFasta;
      end
      else if (ParamStr(i) = '-rfd') or (ParamStr(i) = '--robinson-foulds-distance') then
      begin
        FirstTree := NormalizeFilePath(ParamStr(i + 1));
        SecondTree := NormalizeFilePath(ParamStr(i + 2));
        FMegaAction := maComputeRfDistance;
      end
      else if (ParamStr(i) = '-eis') or (ParamStr(i) = '--export-included-sites') then
        ExportIncludedSitesSummary := True
      else if (ParamStr(i) = '-ca') or (ParamStr(i) = '--concatenate-alignments') then
        FMegaAction := maConcatenateAlignments
      else if (ParamStr(i) = '-ms') or (ParamStr(i) = '--missing-base-symbol') then
        DataInfoGridSetInfo(MissingBaseSymbolStr, ParamStr(i+1))
      else if (ParamStr(i) = '-gs') or (ParamStr(i) = '--gap-symbol') then
        DataInfoGridSetInfo(GapSymbolStr, ParamStr(i+1))
      else if (ParamStr(i) = '-is') or (ParamStr(i) = '--identical-base-symbol') then
        DataInfoGridSetInfo(IdenticalBaseSymbolStr, ParamStr(i+1))
      else if (ParamStr(i) = '-fs') or (ParamStr(i) = '--focal-sequence') or (ParamStr(i) = '--focal-species') then
      begin
        FocalSequence := ParamStr(i + 1);
        TrimTaxaName(FocalSequence);
      end
      else if (ParamStr(i) = '-bicd') or (ParamStr(i) = '--bic-delta') then
      begin
        FBicDelta := StrToFloat(ParamStr(i + 1));
        warn_nv(Format('Using custom BIC delta = %.4f for model selection', [FBicDelta]));
      end
      else if (ParamStr(i) = '-aicd') or (ParamStr(i) = '--aic-delta') then
      begin
        FAiccDelta := StrToFloat(ParamStr(i + 1));
        warn_nv(Format('Using custom AIC delta = %.4f for model selection', [FAiccDelta]));
      end
      else if (ParamStr(i) = '-drp') or (ParamStr(i) = '--dr-phylo') then
      begin
        FMegaAction := maDrPhylo;
      end;
  end;
  Result := true;
end;

procedure TD_MegaMain.OutputVersionInfo;
var
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond: Word;
begin
  DecodeDateTime(Now, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  WriteLn('');
  WriteLn(VER_MEGA_CAPTION);
  WriteLn('Molecular Evolutionary Genetics Analysis - Computational Core');
  WriteLn('Version: ' + VER_MEGA_MAJOR_CHAR + VER_MEGA_MINOR_STR);
  WriteLn('Build: ' + VER_MEGA_BUILD);
  WriteLn('Authors: ' + AUTHOR_LIST);
  WriteLn('Copyright 2011-' + IntToStr(AYear));
  WriteLn('Web: http://www.megasoftware.net');
end;

procedure TD_MegaMain.VerifyGroupsFile;
begin
  if Trim(GroupsFilename) <> EmptyStr then
    if not FileExists(GroupsFilename) then
      error_nv('The specified groups file (' + GroupsFilename + ') does not exist');
end;

function TD_MegaMain.GetAnalysisValue(Option: AnsiString): AnsiString;
var
  i: integer;
begin
  for i := 0 to AnalysisPreferences.Count-1 do
    begin
      if AnsiContainsText(AnalysisPreferences.Strings[i], Option) then
      begin
        Result := Copy(AnalysisPreferences.Strings[i], Pos('=', AnalysisPreferences.Strings[i])+1, 9999999);
        exit;
      end;
    end;
  Raise Exception.Create('Unable to find Analysis Option: ' + Option + ' in the provided Analysis Preferences file.  Please check to make sure your Analysis Preferences file is complete');
end;

function TD_MegaMain.HasActiveData: Boolean;
begin
  Result := FDataType <> snNoToken;
end;

function TD_MegaMain.GetCodeTable: AnsiString;
begin
  if not DataInfoGridHasInfo('Code Table') then
  begin
    SetCodeTableName('Standard');
    SetCodeTable(GetStandardGeneticCode);
  end;
  Result := AnsiString(DataInfoGridGetInfo('Code Table'));
end;


procedure TD_MegaMain.SetCodeTable(AValue: AnsiString);
begin
  DataInfoGridSetInfoObject('Code Table', Pointer(AValue));
end;

function TD_MegaMain.GetCodeTableName: AnsiString;
begin
  if not DataInfoGridHasInfo('Code Table') then
  begin
    SetCodeTableName('Standard');
    SetCodeTable(GetStandardGeneticCode);
  end;
  Result := DataInfoGridGetInfo('Code Table');
end;
procedure TD_MegaMain.SetCodeTableName(AName: AnsiString);
begin
  DataInfoGridSetInfo('Code Table', AName);
end;

function TD_MegaMain.GetDataFileName: AnsiString;
begin
  Result := DataInfoGridGetInfo('Data File');
end;

function TD_MegaMain.GetDataTitle: AnsiString;
begin
  Result := DataInfoGridGetInfo('Title');
end;

procedure TD_MegaMain.SetDataTitle(DataTitle: AnsiString);
begin
  DataInfoGridSetInfo('Title', DataTitle);
end;

function TD_MegaMain.GetDataDescr: AnsiString;
begin
  Result := DataInfoGridGetInfo('Description');
end;

function TD_MegaMain.HasSequenceData: Boolean;
begin
  Result := D_InputSeqData <> nil;
end;

function TD_MegaMain.HasDistanceData: Boolean;
begin
  Result := D_InputDistData <> nil;
end;

function TD_MegaMain.HasMultiFileData: Boolean;
begin
  Result := False;
end;

function TD_MegaMain.HasNucleotideData: Boolean;
begin
  Result := D_InputSeqData <> nil;
  if Result then
    Result := D_InputSeqData.IsNuc;
end;

function TD_MegaMain.HasCodonData: Boolean;
begin
  Result := D_InputSeqData <> nil;
  if Result then
     Result := D_InputSeqData.IsNuc and D_InputSeqData.IsCoding;
end;

function TD_MegaMain.HasAminoAcidData: Boolean;
begin
  Result := D_InputSeqData <> nil;
  if Result then
    Result := D_InputSeqData.IsAmino or HasCodonData;
end;

function TD_MegaMain.IsAnalysisAllowed(AnalysisName: AnsiString): Boolean;
begin
  if
     (AnalysisName = 'SaveDataSession')                         or
     (AnalysisName = 'ExportData')                              or
     (AnalysisName = 'CloseData')                               or
     (AnalysisName = 'ExploreData')                             or
     (AnalysisName = 'SelectTaxaGps')                           then    Result := HasActiveData
  else if
     (AnalysisName = 'SelectGeneticCode')                       then    Result := HasNucleotideData
  else if
     (AnalysisName = 'SelectGenesDomains')                      then    Result := HasSequenceData
  else if
      (AnalysisName = 'ComputeBasicStatsSubmenu')               then    Result := HasSequenceData or not HasActiveData
  else if                                                   
      (AnalysisName = 'ComputeNucleotideComposition')           then    Result := ((HasNucleotideData and (VS_SeqDataExplorer.IsTranslated = False)) or (not HasActiveData))
  else if
      (AnalysisName = 'ComputeAminoAcidComposition')            then	Result := (HasSequenceData and (D_InputSeqData.IsAmino or (HasNucleotideData and VS_SeqDataExplorer.IsTranslated)))or not HasActiveData
  else if
      (AnalysisName =  'ComputeCodonUsageAction')               then    Result := (HasSequenceData and HasCodonData) or not HasActiveData
  else if
     (AnalysisName = 'ComputePairwise')                         or
     (AnalysisName = 'ComputeOverallMean')                      or
     (AnalysisName = 'ComputeMeanInterpopDiversity')            or
     (AnalysisName = 'ComputeMeanDiversityWithinSubpop')        or
     (AnalysisName = 'ComputeMeanDiversityEntirePop')           or
     (AnalysisName = 'ComputeCoeffOfDifferentiation')           or
     (AnalysisName = 'TajimaRelativeRateTest')                  or
     (AnalysisName = 'TajimaTestOfNeutrality')                  or
     (AnalysisName = 'DisparityIndexTestOfPatternHeterogeneity')or
     (AnalysisName = 'ComputeCompositionDistances')             or
     (AnalysisName = 'ComputePatternDisparityIndex')            or
     (AnalysisName = 'ComputeInteriorBranchTestbyLeastSquares') or
     (AnalysisName = 'ConstructMLTreeAction')                   or
     (AnalysisName = 'SelectBestModel')                         or
     (AnalysisName = 'ModelParaRateVar')                        or
     (AnalysisName = 'MolClockTest')                            or
     (AnalysisName = 'InferAncSeqML')                           or
     (AnalysisName = 'EstimateRateSitebySite')                  or
     (AnalysisName = 'AnalyzeTreeByML')                         or
     (AnalysisName = 'CodonOmega')                              or
     (AnalysisName = 'ConstructMPTree')                         or
     (AnalysisName = 'InferAncSeqMP')                           or
     (AnalysisName = 'AnalyzeTreeByMP')                         then    Result := HasSequenceData or not HasActiveData
  else if
     (AnalysisName = 'SelectionZTest')                          or
     (AnalysisName = 'SelectionFisherExact')                    or
     (AnalysisName = 'ComputeMCLSubstitutionMatrix')            or
     (AnalysisName = 'ComputeMCLTsTvBias')                      or
     (AnalysisName = 'ModelParaMatrix')                         or
     (AnalysisName = 'ModelParaTsTv')                           then    Result := HasNucleotideData or not HasActiveData
  else
     Result := False; //If it is unknown then don't show it
end;

function TD_MegaMain.LoadTreeFileList(RootDir: AnsiString; var TreeFileList: TStringList; DoRecursive: Boolean): Boolean;
var
  DirList: TStringList;
  i: Integer;
  FilterList: TStringList;
begin
  DirList := nil;
  FilterList := nil;
  Result := False;

  if not DirectoryExists(RootDir) then
    Exit;

  if not Assigned(TreeFileList) then
    TreeFileList := TStringList.Create;

  FilterList := TStringList.Create;
  FilterList.Add('.nwk');
  FilterList.Add('.tre');
  FilterList.Add('.dnd');
  FilterList.Add('.ph');
  FilterList.Add('.phb');

  try
    try
      DirList := TStringList.Create;
      if not FindAllDirs(RootDir, DirList, DoRecursive) then
        raise Exception.Create('FindAllDirs Failed');

      for i := 0 to DirList.Count - 1 do
      begin
        Result := Result and FindAllFilesInDir(DirList[i], TreeFileList, FilterList);
      end;

    Except
      Result := False;
    end;
  finally
    if Assigned(DirList) then
      DirList.Free;
    if Assigned(FilterList) then
      FilterList.Free;
  end;
end;

procedure TD_MegaMain.InitializeIfNotInitialized;
begin
   if DataInfoGridNV = nil then
   begin
     try
       DataInfoGridNV := TStringList.Create;
     except
       on E : Exception do
         Raise Exception.Create('Unable to create DataInfoGrid to hold information about file Error: ' + E.Message);
       end;
   end;
end;

procedure TD_MegaMain.DataInfoGridClearAll;
begin
  InitializeIfNotInitialized;
  DataInfoGridNV.Clear;
end;

function TD_MegaMain.DataInfoGridHasInfo(InfoType: AnsiString): Boolean;
begin
  InitializeIfNotInitialized;
  try
    Result := DataInfoGridNV.IndexOf(InfoType) >= 0;
  except
    on E:Exception do
      error_nv('error in DataInfoGridHasInfo: ' + E.Message, E);
  end;
end;

procedure TD_MegaMain.DataInfoGridAddIfDoesntExist(InfoType : AnsiString);
begin
    if not DataInfoGridHasInfo(InfoType)  then
      DataInfoGridNV.Add(InfoType);
end;

procedure TD_MegaMain.DataInfoGridSetInfo(InfoType, Info: AnsiString);
begin
  InitializeIfNotInitialized;
  DataInfoGridAddIfDoesntExist(InfoType);
  DataInfoGridNV.Values[InfoType] := Info;
end;


function  TD_MegaMain.DataInfoGridGetInfoObject(InfoType: AnsiString): Pointer;
begin
  InitializeIfNotInitialized;
 if DataInfoGridHasInfo(InfoType) then
   Result := Pointer(DataInfoGridNV.Values[InfoType])
 else
   Raise Exception.Create('Unable to get value ' + InfoType + ' from the DataInfoGrid that contains information about the currently active file');
end;


function  TD_MegaMain.DataInfoGridGetInfo(InfoType: AnsiString): AnsiString;
begin
 InitializeIfNotInitialized;
 if DataInfoGridHasInfo(InfoType) then
   Result := DataInfoGridNV.Values[InfoType]
 else
   Result := EmptyStr;
end;

procedure TD_MegaMain.DataInfoGridRemoveInfo(InfoType: AnsiString);
begin
  InitializeIfNotInitialized;
  DataInfoGridNV.Delete(DataInfoGridNV.IndexOf(InfoType));
end;

procedure TD_MegaMain.DataInfoGridSetInfoObject(InfoType: AnsiString; Info: Pointer);
begin
  InitializeIfNotInitialized;
  DataInfoGridAddIfDoesntExist(InfoType);
  DataInfoGridNV.Values['InfoType'] := AnsiString(Info);

end;

procedure TD_MegaMain.UpdateFileInformation(FileName: AnsiString;
  Title: AnsiString; Description: AnsiString; FDType: TSnTokenCode);
begin
  if (D_InputSeqData <> nil) or (D_InputDistData <> nil) then
    begin
      FDataType := FDType;
      DataInfoGridClearAll;
      DataInfoGridSetInfo('Data File', FileName);
      DataInfoGridSetInfo('Title', Title);
      DataInfoGridSetInfo('Description', Description);

      case FDataType of
        snDistance:   DataInfoGridSetInfo('Data Type', 'Distance');
        snNucleotide: DataInfoGridSetInfo('Data Type', 'Nucleotide');
        snProtein:    DataInfoGridSetInfo('Data Type', 'Protein');
      end;
    end;
     if D_InputSeqData <> nil then
      begin
       DataInfoGridSetInfo('Sequences', IntToStr(D_InputSeqData.OtuInfos.NoOfOtus));
       DataInfoGridSetInfo('Sites',     IntToStr(D_InputSeqData.NoOfSites));
      end;
       if (D_InputSeqData <> nil) and (D_InputSeqData.CodeName <> EmptyStr) then
           DataInfoGridSetInfo('Code Table', D_InputSeqData.CodeName); // We MUST have the name here rather than the value since the Genetic Code Dlg uses the name.
 if D_InputDistData <> nil then
        DataInfoGridSetInfo('Taxa', IntToStr(D_InputDistData.OtuInfos.NoOfOtus));
end;

procedure TD_MegaMain.SetDataFileName(FileName: AnsiString);
begin
  DataInfoGridSetInfo('Data File', FileName);
end;

procedure TD_MegaMain.PrintHelp;
var
  year, month, day: Word;
begin
  DecodeDate(Now, year, month, day);
  WriteLn('---------------------------------------------------------------------');
  WriteLn(VERSION_LABEL_CAPTION);
  WriteLn('');
  {$IFDEF MSWINDOWS}
  WriteLn('Usage: megacc -a c:\pathTo\analysisFile.mao -d c:\pathTo\dataFile.meg [-t c:\pathTo\treeFile.nwk][OPTIONS]');
  {$ELSE}
  WriteLn('Usage: megacc -a /pathTo/analysisFile.mao -d /pathTo/dataFile.meg [-t /pathTo/treeFile.nwk][OPTIONS]');
  {$ENDIF}
  writeLn('');
  writeLn('   -a --analysisOptions ');
  writeLn('       MEGA Analysis Options File     *required*');
  writeLn('       Specify the full path to the Mega Analysis Options (.mao) file.  ');
  writeLn('       This file tells MEGA-CC which analysis to perform as well as');
  writeLn('       which options to use');
  writeLn('');
  if IsDeveloper then
  begin
    writeLn(' The following 10 parameters (-bns, -bnsr, -bnss, -bnms, -bssf, -cdt, -fmr, -btpsd, -srts, and -bdc) are for using the Little Bootstraps method for');
    writeLn(' ML tree construction and ML user tree analysis. A brief communication describing the Little Bootstraps method');
    writeLn(' can be found at:');
    writeLn('     ' + LittleBootstrapsCitation('one-line'));
    writeLn('   -bns --bootstrap-num-samples');
    writeLn('       Specifies the maximum number of sub-samples to use. By default the analysis starts with 3 sub-samples and adaptively adds sub-samples until either a stable result or the maximum number of subsamples is reached.');
    writeLn('   -bnsr --bootstrap-num-sample-reps');
    writeLn('       Specifies the maximum number of bootstrap replicates to perform for each sub-sample. By default the analysis starts with 3 reps per sub-sample and adaptively adds reps to each sub-sample until either a stable result or the maximum number of reps per sub-sample is reached.');
    writeLn('   -bpss --bootstrap-percent-sample-sites');
    writeLn('       Specifies starting sub-sample size as the percentage of randomly sampled sites (without replacement) for generating each sub-sample. This parameter should be a value that exists inside of the range from 0 and 100');
    writeLn('   -bnms --bootstrap-num-max-sites');
    writeLn('       Specifies the maximum number of sub-sample sites to use. When using the --robust-adaptive-speed option, the number of sites used will be double the original number of sites per sub-sample but can be constrained using this option.');
    writeLn('   -bssf --bootstrap-sample-size-factor');
    writeLn('       Specifies the starting sub-sample size as a fractional power of the total number of sites. Must be a value between 0.5 and 1.0 (inclusive).');
    writeLn('       Using this option, the starting sub-sample size is calculated as ceiling(alignment_length^-bssf). For example, if the input alignment has 190,000 sites and -bssf is set to 0.8 then');
    writeLn(Format('       each sub-sample would be generated using %.0n sites (190,000 raised to the power 0.8).', [1.0*ceil(power(190000, 0.8))]));
    writeLn(' Note that the -bnss and -bssf parameters are alternative ways to specify the starting sub-sample size and you should only use one or the other.');
    writeLn('   -cdt --compute-divergence-times');
    writeLn('       Estimate divergence times using bootstrap replicates. The Reltime method will be applied to each bootstrap replicate tree and average divergence times will be computed from these replicates.');
    writeLn('   -fmr --force-max-reps');
    writeLn('       For Little Bootstraps, the number of sub-samples and number of reps per sub-sample will both be the maximum specified.');
    writeLn('   -ktf --keep-tree-fixed');
    writeLn('       For user tree analysis using Little Bootstraps, the phylogeny will be kept fixed (with the user-provided tree) for all bootstrap replicates and only the input sequence data will be randomly sampled using the Little Bootstraps approach.');
    writeLn('   -btpsd --bootstrap-target-precision-std-dev');
    writeLn('       Defines a threshold for increasing the number of replicates per sub-sample when doing the Little Bootstraps analysis. If the std dev of the precision for avg BCL is greater than this threshold the number of replicates per sub-sample will be increased.');
    writeLn('   -srts --skip-replicate-tree-swapping');
    writeLn('       Turns off default behavior where during Little Bootstraps tree inference, if for a given replicate X, there is a replicate tree T within the same sub sample that is a better fit for the replicate X bootstrap upsample, then the tree in replicate X is replaced by T.');
    writeLn('   -ras --robust-adaptive-speed');
    writeLn('       For Little Bootstraps, once a stable result is achieved, double number of sites per sub-sample and repeat the analysis using the number of sub-samples used in the original result while increasing the number of replicates per sub-sample as needed in order to stabilize the BCL precision');
    writeLn('       When used for the Model Tamer analysis, iteratively increase the number of site patterns used and re-run the model test until the same best-fit model is found between consecutive iterations');
    writeLn('');
  end;
  writeLn('   -c --calibration ');
  writeLn('       Calibration file *optional*');
  writeLn('       Specify the full path to a calibration file that you wish to use. The calibration ');
  writeLn('       file is used to provide calibration data for tree timing methods.');
  writeLn('');
  writeLn('   -ca --concatenate-alignments');
  writeLn('       Command to concatenate multiple sequence alignments into a single sequence alignment file');
  writeLn('       The files to be concatenated should either be located in a directory specified by the -d option');
  writeLn('       or the -l option can be used to specify the files to concatenate via a text file that has the');
  writeLn('       full path to each file on a separate line.');
  writeLn('');
  writeLn('   -d --data ');
  writeLn('       Data File         *required for most analyses*');
  writeLn('       Specify the full  or relative path to the data file you wish to ');
  writeLn('       analyze.  MEGA (.meg), and Fasta files are supported for ');
  writeLn('       all analyses. For distance matrices the MEGA (.meg) format is required.');
  writeLn('');
  writeLn('   -date-format --date-format *optional*');
  writeLn('       Specify a date format for parsing tip dates from taxa names when doing the RTDT analysis.');
  writeLn('       If tip dates (sample times) are encoded in taxa names, MEGA can try to parse those dates and');
  writeLn('       auto-generate tip date calibrations. The set of valid characters for the date format is {y m d - /}');
  writeLn('       Examples of valid date formats are: ');
  writeLn('         dd-mm-yyyy (this would match dates that look like 02-12-1809, i.e. birthday of Charles Darwin)');
  writeLn('         yyyymmdd   (this would match dates that look like 20201231, i.e. December 31st, 2020)');
  writeLn('       Please note that all taxa must use the same date format for this to work.');
  writeLn('       SEE ALSO -find-tip-dates');
  writeLn('');
  writeLn('   -ebp --export-bootstrap-precision');
  writeLn('       For tree construction with bootstrap test of phylogeny, Newick results files will include the ');
  writeLn('       estimated precision of each support value as a comment in the Newick file when this flag is used.');
  writeLn('       The output Newick file can be read by MEGA-GUI and the range of bootstrap support for each node');
  writeLn('       can then be displayed in the Tree Explorer window.');
  writeLn('');
  writeLn('   -f --format *applies to sequence alignment only*');
  writeLn('       Export format for sequence alignment');
  writeLn('       Sequence alignments can be exported in either the native .meg');
  writeLn('       or FASTA format.');
  writeLn('       Format values:');
  writeLn('         MEGA');
  writeLn('         Fasta');
  writeLn('');
  writeLn('   -fco --formatted-console-output *EXPERIMENTAL*');
  writeLn('       Format console output for readability. This is an experimental feature and may break in certain');
  writeLn('       situations. For instance, launching megacc using Popen in Python will lead to failure.');
  writeLn('   -find-tip-dates --find-tip-dates');
  writeLn('       Parse tip dates from taxa names and generate a file that contains auto-generated sample time');
  writeLn('       calibration constraints that can be later used for running the RTDT (RelTime with Date Tips)');
  writeLn('       analysis. This requires the -date-format parameter to be specified');
  writeLn('');
  writeLn('   -g --groups ');
  writeLn('       Groups file *optional*');
  writeLn('       Specify the full path to the groups file that you wish to use. This file organizes ');
  writeLn('       taxa into groups where each line in the file is a key value pair of the form ');
  writeLn('             taxonName=groupName ');
  writeLn('       Group information is used for certain analyses, for instance, specifying which taxon/taxa ');
  writeLn('       comprise the outgroup for the timetree analysis ');
  writeLn('');
  writeLn('   -gs --gap-symbol');
  writeLn('       The character that represents indels in the sequence data file that is being analyzed');
  writeLn('       If this is provided, it will override the value that is provided in the .mao file');
  writeLn('');
  writeLn('   -h --help ');
  writeLn('       Help');
  writeLn('       Prints this help file document');
  writeLn('');
  writeLn('   -is --identical-base-symbol');
  writeLn('       The character that represents identical bases in the sequence data file that is being analyzed');
  writeLn('       If this is provided, it will override the value that is provided in the .mao file');
  writeLn('');
  writeLn('   -l --list');
  writeLn('       Input File List');
  writeLn('       Specifies a text file which has a list of input data files to be analyzed.  This option can be  used');
  writeLn('       instead of -d or -t to specify input data, in which case, the same analysis will be performed on all');
  writeLn('       input files listed in the text file and each output results files will be named using  the  name  of');
  writeLn('       its corresponding input file.  The indicated text file must be formatted such that each line has the');
  writeLn('       full path to the sequence data file to be used and if a tree file is also provided it is on the same');
  writeLn('       line but separated by a two pipe characters (e.g. || ).  See EXAMPLES and LIST FORMAT below.');
  writeLn('');
  writeLn('   -ms --missing-base-symbol');
  writeLn('       The character that represents missing bases in the sequence data file that is being analyzed');
  writeLn('       If this is provided, it will override the value that is provided in the .mao file');
  writeLn('');
  writeLn('   -n --noSummary: Do not write out the analysis summary file');
  writeLn('       By default a file that gives an analysis summary is written.');
  writeLn('       This option suppresses the export of that file. However, if');
  writeLn('       any important messages are generated by the application, they');
  writeLn('       will be written to this file regardless.');
  writeLn('');
  writeLn('   -o --outfile     *optional*');
  writeLn('       Output Path / Output Dir');
  {$IFDEF MSWINDOWS}
  writeLn('       Specify the full path and base filename (e.g. c:\myResultsDirectory\myResultName) or');
  writeLn('       simply the full path and directory of where to save the file');
  writeLn('       (e.g. C:\myResultsDirectory) in which case, a unique filename will be chosen');
  {$ELSE}
  writeLn('       Specify the full path and base filename (e.g. /myResultsDirectory/myResultName) or');
  writeLn('       simply the full path and directory of where to save the file');
  writeLn('       (e.g. /myResultsDirectory) in which case, a unique filename will be chosen');
  {$ENDIF}
  writeLn('       automatically for you.');
  writeLn('   -pfc --partition-frequency-cutoff');
  writeLn('       Partition Frequency Cutoff (a value between 0.0 and 1.0 - default is 0.5) *optional*');
  writeLn('       When bootstrapping is used for tree construction a list of partitions and');
  writeLn('       frequencies is written to a text file. The partition frequency cutoff');
  writeLn('       causes any partitions whose frequency is less than the cutoff value');
  writeLn('       to be ommited from this text file. Set this value to 0.0 to include');
  writeLn('       all partitions.');
  writeLn('   -psp --percent-site-patterns');
  writeLn('       For the ModelTamer analysis, sets the percentage (0.1 - 100.0) of unique site patterns in the multiple sequence alignment that');
  writeLn('       that will be randomly sampled and used for selecting the best-fit model.');
  writeLn('       More information regarding the ModelTamer analysis can be found at ' + ModelTamerCitation('one-line'));
  writeLn('   -r --recursive');
  writeLn('       Recursive directory search *optional*');
  writeLn('       If a directory is specified for analysis by default MEGA only searches');
  writeLn('       the contents of that folder and not any of it''s children.  To include the');
  writeLn('       contents of all folders under the one specified, use this option.');
  writeLn('');
  writeLn('   -s --silent: Do not write out the progress updates');
  writeLn('       This option prevents progress updates from being written to stdout.');
  writeLn('');
  writeLn('   -t --tree     *required for some analyses*');
  writeLn('       Tree File');
  writeLn('       Specify the full path to the tree file you wish to use. (Some ');
  writeLn('       analyses requires a user provided tree, or allow you to provide ');
  writeLn('       your own)');
  writeLn('');
  writeLn('  If no output path is specified, results will be saved in the same directory');
  writeLn('  as the input data file, with a unique name.');
  writeLn('       ');
  writeLn('');
  WriteLn('EXAMPLES');
  writeLn('');
  WriteLn('   This example performs a multiple sequence alignment on codons (it assumes that you have created');
  WriteLn('   the file "Clustal_Codon_Alignment.mao" using the prototyper (megaproto). A fasta file with coding ');
  WriteLn('   data is used as input and the resulting alignment is output');
  WriteLn('   in the MEGA format:');
  writeLn('');
  WriteLn('      megacc -a ~/Documents/Clustal_Codon_Alignment.mao -d ~/Documents/codingData.fas -o ~/Documents/codingDataAligned.meg');
  writeLn('');
  WriteLn('   This example shows how to construct a neighbor-joining phylogeny for each of a list of sequence data files. ');
  WriteLn('   The analysis will be performed for each file listed in "listOfDataFiles.txt" and all results will be written to ');
  WriteLn('   the ~/Documents/outputDirectory/ directory:');
  WriteLn('      megacc -a ~/Documents/NJ_Tree_Settings.mao -l ~/Documents/listOfDataFiles.txt -o ~/Documents/outputDirectory/');
  writeLn('');
  writeLn('   This example shows how to generate a calibrations file for RTDT by having MEGA parse tip dates from taxa names');
  writeLn('   in a multiple sequence alignment file.');
  writeLn('      megacc -find-tip-dates ~/Documents/MEGA X/Examples/reltime_with_dated_tips_RTDT_alignment.meg -date-format yyyy-mm-dd -o example_calibrations.tipdates');
  WriteLn('   When executing this command, MEGA will produce 2 output files, one that is the input sequence alignment with tip dates');
  WriteLn('   added to taxa names command statements and the other is a file that has tip dates given in the MEGA calibration file format');
  WriteLn('   The RTDT analysis can then be run using either file. If the new alignment file is used, a separate calibrations file is not');
  WriteLn('   required because the tip dates are included in the new alignment file. Otherwise the calibrations file can be used with the -c option');
  WriteLn('');
  WriteLn('   This example shows how run the RTDT (RelTime with Dated Tips) analysis to construct a time tree using temporally sampled data. This');
  WriteLn('   example assumes that you have created the rtdt_ML_nucleotide.mao file using the MEGA GUI in prototype mode and saved it to ~/Documents/MEGA X/Examples');
  WriteLn('      cd ~/Documents/MEGA X/Examples');
  WriteLn('      megacc -a rtdt_ML_nucleotide.mao -d reltime_with_dated_tips_RTDT_alignment.meg -t reltime_with_dated_tips_RTDT_tree.nwk -g reltime_with_dated_tips_RTDT_outgroup.txt -c reltime_with_dated_tips_RTDT_sample_times.txt -o rtdt_example_time_tree');
  WriteLn('   The options used are:');
  WriteLn('      -a rtdt_ML_nucleotide.mao is the MEGA Analysis Options file created using MEGA GUI in prototype mode');
  WriteLn('      -d reltime_with_dated_tips_RTDT_alignment.meg is a multiple sequence alignment file - a FASTA file could be used instead of a .meg file');
  WriteLn('      -t reltime_with_dated_tips_RTDT_tree.nwk is a Newick formatted tree file giving the topology of the time tree');
  WriteLn('      -g reltime_with_dated_tips_RTDT_outgroup.txt is a text file that specifies which taxa in the tree comprise the outgroup - can be one or more taxa');
  WriteLn('      -c reltime_with_dated_tips_RTDT_sample_times.txt is a text file that contains sampling dates for each sequence in the sequence alignment');
  WriteLn('      -o rtdt_example_time_tree is the output base file name. MEGA will produce multiple results files and their names will be prefixed with this value');
  WriteLn('');
  WriteLn('   This example shows how run the RelTime analysis to construct a time tree using divergence time constraints to calibrate the tree. This');
  WriteLn('   example assumes that you have created the reltime_ML_nucleotide.mao file using the MEGA GUI in prototype mode and saved it to ~/Documents/MEGA X/Examples');
  WriteLn('      cd ~/Documents/MEGA X/Examples');
  WriteLn('      megacc -a reltime_ML_nucleotide.mao -d mtCDNA.meg -t mtCDNA.nwk -g mtCDNA_outgroup.txt -c mtCDNACalibrationDensities.txt -o reltime_example_time_tree');
  WriteLn('   The options used are:');
  WriteLn('      -a reltime_ML_nucleotide.mao is the MEGA Analysis Options file created using MEGA GUI in prototype mode');
  WriteLn('      -d mtCDNA.meg is a multiple sequence alignment file - a FASTA file could be used instead of a .meg file');
  WriteLn('      -t mtCDNA.nwk is a Newick formatted tree file giving the topology of the time tree');
  WriteLn('      -g mtCDNA_outgroup.txt is a text file that specifies which taxa in the tree comprise the outgroup - can be one or more taxa');
  WriteLn('      -c mtCDNACalibrationDensities.txt is a text file that contains calibration density distributions for calibrating the time tree');
  WriteLn('      -o reltime_example_time_tree is the output base file name. MEGA will produce multiple results files and their names will be prefixed with this value');
  WriteLn('');
  WriteLn('LIST FORMAT');
  WriteLn('   When using the -l option, each file to be analyzed must be on its own line. For example:');
  WriteLn('      ~/Documents/myData/seqData1.fas	');
  WriteLn('      ~/Documents/myData/seqData2.fas');
  WriteLn('      ~/Documents/myData/seqData3.fas');
  writeLn('');
  WriteLn('   If the analyses are to use a user-provided Newick tree file, then the tree files are given on the same line as ');
  WriteLn('   the data files, following two pipe characters. For example:');
  WriteLn('      ~/Documents/myData/seqData1.fas || ~/Documents/myData/treeFile1.nwk');
  WriteLn('      ~/Documents/myData/seqData2.fas || ~/Documents/myData/treeFile2.nwk');
  WriteLn('      ~/Documents/myData/seqData3.fas || ~/Documents/myData/treeFile3.nwk	');
  writeLn('');
  WriteLn('RELTIME CALIBRATION FORMAT');
  WriteLn('   For the Reltime analysis, multiple calibration points can be used to convert relative times to absolute ');
  WriteLn('   times. Calibrations are given in a text file which is specified using the -c option and is formatted using');
  WriteLn('   one of three formats. In the first format, the node in the tree for a given calibration is specified');
  WriteLn('   by its name, which must be included as an internal node label in the newick tree file used:');
  WriteLn('      !NodeName1="name1" minTime=1.75 maxTime=2.25;');
  WriteLn('      !NodeName2="name2" minTime=3.0;');
  WriteLn('      !NodeName3="name3" time=2.5;');
  WriteLn('   In the second format, the node in the tree for a given calibration point is indicated by specifying two ');
  WriteLn('   taxa whose most recent common ancestor is the node to calibrate:');
  WriteLn('      !MRCA="some name1" TaxonA="taxon1 name" TaxonB="taxon2 name" minTime=1.75 maxTime=2.25;');
  WriteLn('      !MRCA="some name2" TaxonA="taxon3 name" TaxonB="taxon4 name" minTime=3.0;');
  WriteLn('      !MRCA="some name3" TaxonA="taxon5 name" TaxonB="taxon6 name" time=2.5;');
  WriteLn('   In the third format, target nodes are specified in the same way as the above examples but');
  WriteLn('   probability distributions are specifed for divergence time constraints instead of min and max times:');
  WriteLn('      !NodeName1="name1" Distribution=normal mean=6.4 stddev=1.2;');
  WriteLn('      !MRCA="some name2" TaxonA="taxon1 name" TaxonB="taxon2 name" Distribution=exponential time=8.2 lambda=0.25;');
  WriteLn('      !MRCA="some name3" TaxonA="taxon1 name" TaxonB="taxon2 name" Distribution=uniform mintime=4 maxtime=6;');
  WriteLn('      !MRCA="some name4" TaxonA="taxon1 name" TaxonB="taxon2 name" Distribution=lognormal offset=7.0 mean=2.38 stddev=0.15;');
  WriteLn('RTDT (RelTime with Date Tips) CALIBRATION FORMAT');
  WriteLn('   For the RelTime with Dated Tips analysis, sample time calibrations for leaf nodes are required and are given in');
  WriteLn('   a text file which is specified using the -c option and has calibrations formatted as follows:');
  WriteLn('      !Taxon="taxon1" sampleTime=2020-10-31');
  WriteLn('      !Taxon="taxon2" sampleTime=2020-11-24');
  WriteLn('   SEE ALSO -find-tip-dates');
  WriteLn('');
  WriteLn('CITING MEGACC');
  WriteLn('   ' + MEGA_CC_Citation('one-line'));
  WriteLn('   ' + MEGA_GUI_Citation('one-line'));
  WriteLn('');
  WriteLn('AUTHORS');
  WriteLn('   Koichiro Tamura, Glen Stecher, and Sudhir Kumar');
  WriteLn('COPYRIGHT');
  WriteLn(Format('   copyright 2011-%d by the authors ', [year]));
  WriteLn('');
end;

{$IFNDEF MSWINDOWS}
procedure TD_MegaMain.UpdatePeakMemUsage;
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

procedure TD_MegaMain.DomainsToFasta;
var
  fname: String = '';
begin
  fname := GetDataFileName;
  if not FileExists(fname) then
    error_nv('aligment file not found: ' + fname);
  if not DirectoryExists(TargetDirectory) then
    error_nv('target directory not found: ' + TargetDirectory);
  ConstructDummyProcessPack;
  FProcessPack.TextualSettingsList.Add(Format('%s=%s', ['Analysis', 'Domains to Fasta Files']));
  InitializeAnalysisSummary;
  DoOpenDataFile(fname, False, nil);
  if not ExportActiveDomainsToFastaFiles(D_InputSeqData, TargetDirectory, nil) then
    error_nv('failed to export active domains to fasta files');
end;

procedure TD_MegaMain.SegmentsToFasta;
var
  fname: String = '';
begin
  fname := GetDataFileName;
  if not FileExists(fname) then
    error_nv('aligment file not found: ' + fname);
  if not DirectoryExists(TargetDirectory) then
    error_nv('target directory not found: ' + TargetDirectory);
  ConstructDummyProcessPack;
  FProcessPack.TextualSettingsList.Add(Format('%s=%s', ['Analysis', 'Domains to Fasta Files']));
  InitializeAnalysisSummary;
  DoOpenDataFile(fname, False, nil);
  if not ExportDataInSegmentsToFastaFiles(D_InputSeqData, TargetDirectory, SegmentSize, nil) then
    error_nv('failed to export data segments to fasta files');
end;

function TD_MegaMain.ThreadsStillRunning: Boolean;
var
  aCount: Integer = -1;
begin
  try
    NumActiveThreadsCS.Acquire;
    aCount := RunningThreadCount;
  finally
    NumActiveThreadsCs.Release;
  end;

  Result := aCount > 0;
  if Result then
  begin
    if FHasTimeLimit then
    begin
      if SecondsBetween(Now, ExecutionStartTime) > FMaxExecutionTimeInSeconds then
        error_nv(Format('max execution time of %0n seconds exceeded. Aborting the analysis.', [FMaxExecutionTimeInSeconds*1.0]));
    end;
  end;
end;

function TD_MegaMain.GetCommandLineStr: String;
var
  i: Integer;
begin
  Result := EmptyStr;
 for i := 0 to ParamCount do
   Result := Result + ParamStr(i) + ' ';
 Result := Trim(Result);
end;

procedure TD_MegaMain.SetOtuInfos;
begin
  Assert(Assigned(D_InputSeqData) or Assigned(D_InputDistData));
  if Assigned(D_InputSeqData) then
     FOtuInfos := D_InputSeqData.OtuInfos
  else if Assigned(D_InputDistData) then
     FOtuInfos := D_InputDistData.OtuInfos;
end;

procedure TD_MegaMain.FindTipDates;
var
  TipDatesFinder: TTipDatesFinder = nil;
  IsSuccess: Boolean = False;
  calibrations: TStringList = nil;
  outFile: String;
begin
  if not FileExists(DataFileName) then
    error_nv('Input data file not found');
  if (FDateFormatStr[1] = #34) and (FDateFormatStr[Length(FDateFormatStr)] = #34) then
    FDateFormatStr := copy(FDateFormatStr, 2, Length(FDateFormatStr) - 2);
  if (FDateFormatStr[1] = #39) and (FDateFormatStr[Length(FDateFormatStr)] = #39) then
    FDateFormatStr := copy(FDateFormatStr, 2, Length(FDateFormatStr) - 2);
  try
    try
      TipDatesFinder := TTipDatesFinder.Create;
      if FDateFormatIsUserDefined then
      begin
        TipDatesFinder.DateFormat := FDateFormatStr;
        TipDatesFinder.IsCalendarDate := True;
        TipDatesFinder.ParseRule := tdprCustom;
      end;

      calibrations := TipDatesFinder.FindTipDates(DataFileName, IsSuccess);
      if calibrations.Count > 0 then
      begin
        outFile := NextAvailableFilenameNV('.tipdates');
        calibrations.SaveToFile(outFile);
        if not FileExists(outFile) then
          raise Exception.Create('failed to save found calibrations');
      end;
      if not IsSuccess then
        raise Exception.Create(TipDatesFinder.LogStrings.Text);
    except
      on E:Exception do
        error_nv('Failed to find tip dates: ' + E.Message, E);
    end;
  finally
    if Assigned(TipDatesFinder) then
      TipDatesFinder.Free;
    if Assigned(calibrations) then
      calibrations.Free;
  end;
end;

procedure TD_MegaMain.FindAlignment;
var
  converter: TNexusAlignmentConverter = nil;
  alignment: TStringList = nil;
begin
  if not FileExists(DataFileName) then
    raise Exception.Create(Format('Input data file not found (%s)', [DataFileName]));
  try
    try
      converter := TNexusAlignmentConverter.Create;
      alignment := TStringList.Create;
      if converter.ConvertNexusToMegaAlignment(DataFileName, alignment) then
        alignment.SaveToFile(NextAvailableFilenameNV('.meg'))
      else
      begin
        DoTextout(1, NextProgressLine, 'Oh no! Failed to extract sequence alignment', Red);
        raise Exception.Create(converter.LogStrings.Text);
      end;
    except
      on E:Exception do
        error_nv('failed to extract alignment: ' + E.Message, E);
    end;
  finally
    if Assigned(converter) then
      converter.Free;
    if Assigned(alignment) then
      alignment.Free;
  end;
end;

procedure TD_MegaMain.FindTrees;
var
  converter: TNexusTreeConverter = nil;
  newickStrings: TStringList = nil;
begin
  if not FileExists(DataFileName) then
    raise Exception.Create(Format('Input data file not found (%s)', [DataFileName]));
  try
    try
      converter := TNexusTreeConverter.Create;
      newickStrings := TStringList.Create;
      if converter.ConvertNexusToNewick(DataFileName, newickStrings) then
        newickStrings.SaveToFile(NextAvailableFilenameNV('.nwk'))
      else
      begin
        DoTextOut(1, NextProgressLine, 'Oh no! Failed to extract newick strings', Red);
        raise Exception.Create(converter.LogStrings.Text);
      end;
    except
      on E:Exception do
        error_nv('failed to extract newick strings: ' + E.Message, E);
    end;
  finally
    if Assigned(converter) then
      converter.Free;
    if Assigned(newickStrings) then
      newickStrings.Free;
  end;
end;

procedure TD_MegaMain.ComputeRfDistance;
var
  i: Integer;
  rfMetric: TRobinsonFouldsMetric = nil;
  aTreeList: TTreeList = nil;
  otuNames: TStringList = nil;
  data1: TTreeData = nil;
  data2: TTreeData = nil;
  rfDist: Double = -1;
  aFile: TextFile;
  filename: String = '';
begin
  if not FileExists(FirstTree) then
    error_nv('missing tree file1: ' + FirstTree);
  if not FileExists(SecondTree) then
    error_nv('missing tree file2: ' + SecondTree);

  try
    aTreeList := TTreeList.Create;
    if not aTreeList.ImportFromNewickFile(FirstTree, nil, False) then
      error_nv('failed to import tree1 from file: ' + FirstTree);
    data1 := TTreeData.Create(aTreeList.NoOfOTUs, aTreeList.isBLen, aTreeList.isSE, aTreeList.isStats);
    data1.Assign(aTreeList[0]);
    otuNames := TStringList.Create;
    otuNames.Assign(aTreeList.OTUNameList);
    aTreeList.Clear;
    if not aTreeList.ImportFromNewickFile(SecondTree, otuNames, False) then
      error_nv('failed to import tree2 from file (hint: must contain same taxa as first tree): ' + SecondTree);
    data2 := TTreeData.Create(aTreeList.NoOfOTUs, aTreeList.isBLen, aTreeList.isSE, aTreeList.isStats);
    data2.Assign(aTreeList[0]);

    rfMetric := TRobinsonFouldsMetric.Create(False);
    rfDist := rfMetric.Compute_RF_distance(data1, data2);

    filename := NextAvailableFilenameNV('_rf_dist.txt');
    WriteLn('writing RF results to ' + filename);
    AssignFile(aFile, filename);
    Rewrite(aFile);
    WriteLn(aFile, Format('Robinson-Foulds metric = %.4f', [rfDist]));
  finally
    CloseFile(aFile);
    if Assigned(rfMetric) then
      rfMetric.Free;
    if Assigned(aTreeList) then
      aTreeList.Free;
    if Assigned(otuNames) then
      otuNames.Free;
    if Assigned(data1) then
      data1.Free;
    if Assigned(data2) then
      data2.Free;
  end;
end;

procedure TD_MegaMain.ConcatenateAlignments;
var
  sl: TStringList;
  aList: TStringList = nil;
  i: Integer;
begin
  FRuntimeProgress := TRuntimeProgress.Create(nil);
  FProcessPack := TProcessPack.Create;
  sl := ProcessPack.TextualSettingsList;
  case FDataType of
    snNucleotide:
    begin
      sl.Add('datatype=snNucleotide');
      sl.Add('containsCodingNuc=False');
    end;
    snProtein:
    begin
      sl.Add('datatype=snProtein');
      sl.Add('containsCodingNuc=False');
    end;
    snCoding:
    begin
      sl.Add('datatype=snNucleotide');
      sl.Add('containsCodingNuc=True');
    end;
    snNoToken:
    begin
      { will be ok for .meg files};
    end
    else
      error_nv('invalid input datatype for alignment concatenation: ' + MapTokenCodeToDataTypeString(FDataType));
  end;

  if DataInfoGridHasInfo(MissingBaseSymbolStr) then
    sl.Add(DataInfoGridGetInfo(MissingBaseSymbolStr))
  else
    sl.Add(MissingBaseSymbolStr + '=?');
  if DataInfoGridHasInfo(IdenticalBaseSymbolStr) then
    sl.Add(DataInfoGridGetInfo(IdenticalBaseSymbolStr))
  else
    sl.Add(IdenticalBaseSymbolStr + '=.');
  if DataInfoGridHasInfo(GapSymbolStr) then
    sl.Add(DataInfoGridGetInfo(GapSymbolStr))
  else
    sl.Add(GapSymbolStr + '=-');

  if DirectoryExists(GetDataFileName) then
  begin
    LaunchConcatenateAlignmentsThread(GetDataFileName, CheckCancelStub, FRuntimeProgress);
  end
  else if FileExists(DataFileList) then
  begin
    FConcatenateFilesList := TStringList.Create;
    aList := TStringList.Create;
    aList.LoadFromFile(DataFileList);
    if aList.Count > 0 then
    begin
      for i := aList.Count - 1 downto 0 do
        if Trim(aList[i]) <> EmptyStr then
          FConcatenateFilesList.Add(NormalizeFilePath(aList[i]));
    end
    else
      raise Exception.Create('no files found for alignment concatenation');
    if Trim(OutputFileName) = EmptyStr then
      SetDataFileName(DataFileList);
    LaunchConcatenateAlignmentsThread(FConcatenateFilesList, CheckCancelStub, FRuntimeProgress);
  end
  else
    error_nv('required input directory or file list missing for alignment concatenation');
end;

procedure TD_MegaMain.CompareBootstrapTrees;
var
  c: TCompareTrees = nil;
  groups: TStringList = nil;
begin
  try
    try
      c := TCompareTrees.Create(FirstTree, SecondTree);
      if FileExists(GroupsFilename) then
      begin
        groups := TStringList.Create;
        groups.LoadFromFile(GroupsFilename);
        c.AddOutgroupTaxon(groups[0]);
      end;

      //if not c.CompareNodeHeights then todo: add a procedure to run c.CompareNodeHeights
      //  raise Exception.Create(c.LogText);
      if not c.CompareBootstrapSupport then
        raise Exception.Create(c.LogText);
    except
      on E:Exception do
        error_nv('failed to compare bootstrap trees', E);
    end;
  finally
    if Assigned(c) then
      c.Free;
    if Assigned(groups) then
     groups.Free;
  end;
end;

procedure TD_MegaMain.CompareNodeHeights;
var
  c: TCompareTrees = nil;
  groups: TStringList = nil;
begin
  try
    try
      FProcessPack := TProcessPack.Create;
      FProcessPack.TextualSettingsList.Add(Format('%s=snNucleotide', [DataTypeStr]));
      FProcessPack.TextualSettingsList.Add(Format('%s=?', [MissingBaseSymbolStr2]));
      FProcessPack.TextualSettingsList.Add(Format('%s=.', [IdenticalBaseSymbolStr2]));
      FProcessPack.TextualSettingsList.Add(Format('%s=-', [GapSymbolStr2]));
      FProcessPack.TextualSettingsList.Add(Format('%s=False', [ContainsCodingNucStr]));
      c := TCompareTrees.Create(FirstTree, SecondTree);

      if FileExists(GroupsFilename) then
      begin
        groups := TStringList.Create;
        groups.LoadFromFile(GroupsFilename);
        c.AddOutgroupTaxon(groups[0]);
      end;

      if not c.CompareNodeHeights(DataFileName) then
        raise Exception.Create(c.LogText);
    except
      on E:Exception do
        error_nv('failed to compare bootstrap trees', E);
    end;
  finally
    if Assigned(c) then
      c.Free;
    if Assigned(groups) then
     groups.Free;
  end;
end;

procedure TD_MegaMain.CompareTopologies;
var
  c: TCompareTrees = nil;
  areSame: Boolean = False;
begin
  try
    try
      c := TCompareTrees.Create(FirstTree, SecondTree);
      if not c.CompareTopologies(NextAvailableFilenameNV('.txt'), areSame) then
        raise Exception.Create(c.LogText);
    except
      on E:Exception do
        error_nv('failed to compare topologies', E);
    end;
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

function TD_MegaMain.CommandLineString: String;
var
  i: Integer = -1;
begin
  Result := EmptyStr;
  for i := 1 to ParamCount do
    Result := Result + ParamStr(i) + ' ';
  Result := Trim(Result);
end;


procedure TD_MegaMain.ConcatenateAlignmentsDone(aThread: TObject);
var
  aList: TStringList = nil;
  cThread: TAlignConcatThread;
  progress: TRuntimeProgress = nil;
  options: TSeqExportOptions = nil;
  aInfo: TAllDomainInfo = nil;
begin
 try
   cThread := TAlignConcatThread(aThread);
   if not cThread.IsSuccess then
     error_nv('Concatenation of alignment files failed: ' + Copy(cThread.LogMessages, 1, 200));
   progress := TRuntimeProgress.Create;
   aInfo := cThread.GetAllDomainInfo;
   SetupConcatenatedSequencesInformation(cThread.GetSequences, aInfo, progress, False);
   options := TSeqExportOptions.Create;
   options.isNucData := D_InputSeqData.IsNuc;
   aList := TStringList.Create;
   if not D_InputSeqData.WriteExportDataToFile(options, NextAvailableFilenameNV('.meg'), aList, nil) then
     error_nv('Failed to export concatenated alignment data');
 finally
   if Assigned(aList) then
     aList.Free;
 end;
end;

function TD_MegaMain.MegaFileDataTypeString: String;
begin
 case FDataType of
   snNucleotide:
     Result := 'DNA';
   snCoding:
     Result := 'DNA';
   snProtein:
     Result := 'Protein';
   snDistance:
     Result := 'Distance';
   snTree:
     Result := 'Tree';
   else
     Result := EmptyStr;
 end;
end;

function TD_MegaMain.CheckCancelStub(Progress: integer; Status: AnsiString): boolean;
begin
  Result := False;
  if not Assigned(FRuntimeProgress) then
    Exit;
  FRuntimeProgress.UpdatePercentProgress(Progress);
end;

procedure TD_MegaMain.InitializeMegaStdOut;
begin
 InitVideo;
 ClearMegaStdOut;
 ProgressScreenWidth := ScreenWidth;
 ProgressScreenHeight := ScreenHeight;
end;

procedure TD_MegaMain.FinalizeMegaStdOut;
begin
 DoneVideo;
end;

procedure TD_MegaMain.WriteFinalProgress;
begin
 DoTextOut(1, NextProgressLine, Format('%10d%% %-80s', [100, 'Analysis Complete']), LightBlue);
 DoTextOut(1, NextProgressLine, ' ', LightGreen);
 DoTextout(1, NextProgressLine, 'MEGA has completed the requested action', LightGreen);
 if not UseFormattedConsoleOutput then
   WriteLn(EmptyStr);
 if ExitCode = 0 then
   DoTextOut(1, NextProgressLine, 'Terminating the megacc process with successful exit status', LightBlue)
 else
   DoTextOut(1, NextProgressLine, 'Terminating the megacc process with bad exit code = ' + IntToStr(ExitCode), Red);
 if UseFormattedConsoleOutput then
   UpdateScreen(False);
 if UseFormattedConsoleOutput then
   FinalizeMegaStdOut;
end;

procedure TD_MegaMain.NewickToTabular;
var
  aList: TTreeList = nil;
  aData: TTreeData = nil;
  exporter: TTabularTreeExport = nil;
  otuNames: TStringList = nil;
begin
  try
    if not FileExists(OtuNamesFile) then
      error_nv('missing otunames file: ' + OtuNamesFile);
    if not FileExists(CommandLineTreeFileName) then
      error_nv('missing tree file: ' + CommandLineTreeFileName);
    otuNames := TStringList.Create;
    otuNames.LoadFromFile(OtuNamesFile);
    aList := TTreeList.Create;
    aList.ImportFromNewickFile(CommandLineTreeFileName, otuNames);
    exporter := TTabularTreeExport.Create(alist);
    exporter.ExportTreeTabular(0, NextAvailableFilenameNV('.txt'), aList.isBLen);
  finally
    if Assigned(aList) then
      aList.Free;
    if Assigned(exporter) then
      exporter.Free;
    if Assigned(otuNames) then
      otuNames.Free;
  end;
end;

constructor TD_MegaMain.Create;
begin
  IsRunningInTestHarness := False;
  AddBclStdDevToNewick := False;
  FBicDelta := -1;
  FAiccDelta := -1;
  MultipleStartTrees := False;
  NumStartingMpTrees := 0;
  FHasTimeLimit := False;
  FMaxExecutionTimeInSeconds := -1;
  FDataType := snNoToken;
  ForceMaxLbsReps := False;
  ForceMaxLbsSamples := False;
  AnalysisWizardCC := nil;
  FDateFormatStr := DEFAULT_DATE_FORMAT;
  FDateFormatIsUserDefined := False;
  IsFileIterator := False;
  FMegaAction := maProcessCommands;
  DataFileList := EmptyStr;
  FRuntimeProgress := nil;
  FConcatenateFilesList := nil;
  DataInfoGridNV := nil;
  {$IFNDEF MSWINDOWS}
  PeakMemoryCS := TCriticalSection.Create;
  {$ENDIF}
end;

destructor TD_MegaMain.Destroy;
begin
  if not UseFormattedConsoleOutput then
    writeLn(EmptyStr);
  {$IFNDEF MSWINDOWS}
  if Assigned(PeakMemoryCS) then
    PeakMemoryCS.Free;
  {$ENDIF}
  if Assigned(AnalysisWizardCC) then
    AnalysisWizardCC.Free;
  inherited Destroy;
end;

function TD_MegaMain.UserHasProvidedOutgroupFile: Boolean;
var
  aList: TStringList = nil;
  i: Integer = -1;
  aValue: String = '';
begin
  Result := False;
  if not FileExists(GroupsFilename) then
    Exit;

  try
    aList := TStringList.Create;
    aList.LoadFromFile(GroupsFilename);
    if aList.Count > 0 then
      for i := 0 to aList.Count - 1 do
      begin
        aValue := aList.ValueFromIndex[i];
        if SameText(aValue, 'outgroup') then
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

procedure TD_MegaMain.SiteCoverageThreadDone(aThread: TObject);
var
  t: TSiteCoverageThread = nil;
  setup: TTreeDisplaySetup = nil;
begin
  try
    try
      if not (aThread is TSiteCoverageThread) then
        raise Exception.Create(Format('expected TSiteCoverageThread but got %s', [aThread.ClassName]));
      t := TSiteCoverageThread(aThread);
      if t.IsSuccess then
      begin
        setup := TTreeDisplaySetup.Create;
        t.ExportDataCoverage(NextAvailableFilenameNV('_data_coverage.csv'));
        setup.ShowSiteCoverage(t.AnalysisInfo);
      end
      else
        warn_nv('site coverage compuation failed: ' + t.Log.Text);
    except
      on E:Exception do
        error_nv(Format('Application error: %s', [E.Message]), E);
    end;
  finally
    if Assigned(setup) then
      setup.Free;
  end;
end;
procedure TD_MegaMain.ModelTestDone(aThread: TObject);
var
  t: TModelTestThread = nil;
  aInfo: TAnalysisInfo = nil;
begin
  Assert(aThread is TModelTestThread);
  try
    t := TModelTestThread(aThread);
    aInfo := TAnalysisInfo(t.ProgressDlg.FMAI);
    if not t.IsSuccess then
      raise Exception.Create(t.MessagesLog.Text);
    ShowModelTestResults(aInfo);
  except
    on E:Exception do
      error_nv('Error preparing model selection results', E);
  end;
end;

procedure TD_MegaMain.AdaptiveModelTestDone(aThread: TObject);
 var
   t: TAdaptiveModelTestThread = nil;
 begin
   Assert(aThread is TAdaptiveModelTestThread);
   try
     t := TAdaptiveModelTestThread(aThread);
     t.AnalysisInfo.AnalysisSummary.AddCalculatedValue(opsNumEvaluationsPerformed, IntToStr(t.TotalModelsEvaluated));
     if t.IsCancelled then
       warn_NV('Model selection analysis cancelled')
     else
     begin
       if not t.IsSuccess then
         raise Exception.Create(t.MessagesLog.Text);
       ShowModelTestResults(t.AnalysisInfo);
       if t.Log.Count > 1 then
         t.Log.SaveToFile(NextAvailableFilenameNV('_model_sel_log.txt'));
     end;
   except
     on E:Exception do
       error_nv('Error preparing model selection results: ' + E.Message);
   end;
end;

procedure TD_MegaMain.DistCommandThreadDone(aThread: TObject);
var
  f: TDistCommandFinalize = nil;
  t: TDistCommandThread = nil;
begin
  try
    try
      if not (aThread is TDistCommandThread) then
        raise Exception.Create('expected TDistCommandThread but got ' + aThread.ClassName);
      t := TDistCommandThread(aThread);
      if not t.IsSuccess then
        raise Exception.Create('TDistCommandThread reported failure. ' + t.Log.Text);
      f := TDistCommandFinalize.CreateFromThread(t);
      if not f.Execute then
        raise Exception.Create('failed to finalize distance calculation display. ' + f.Log.Text);
    except
      on E:Exception do
        error_nv('Application Error: ' + E.Message, E);
    end;
  finally
     if Assigned(f) then
       f.Free;
  end;
end;

procedure TD_MegaMain.DistTreeThreadDone(aThread: TObject);
var
  t: TDistTreeSearchThread = nil;
begin
  if not (aThread is TDistTreeSearchThread) then
    error_nv('Distance tree construction error', Exception.Create('invalid dist tree search thread class'));
  t := TDistTreeSearchThread(aThread);
  if not t.IsSuccess then
    error_nv('Distance tree calculation failed - ' + t.Log.Text);
end;

procedure TD_MegaMain.EslThreadDone(aThread: TObject);
var
  t: TEslLinkerThread = nil;
  aLink: TEslLinker = nil;
  actualResultsDir: String = '';
begin
 try
   try
     t := TEslLinkerThread(aThread);
     aLink := t.EslLink;

     if Assigned(aLink.MAI) and Assigned(aLink.MAI.ARP) then
     begin
       aLink.MAI.ARP.Free;
       aLink.MAI.ARP := nil;
     end;

     if not t.IsSuccess then
       error_nv('DrPhylo analysis failed with an unknown error. Please review the log file for more information');

     aLink.MAI.AnalysisSummary.AddAnalysisInfo(aLink.MAI);
     aLink.MAI.AnalysisSummary.AnalysisName := 'DrPhylo';
     aLink.MAI.AnalysisSummary.AnalysisOptions.Add(Format('No. of Rows=%d', [aLink.NumGridRows]));
     aLink.MAI.AnalysisSummary.AnalysisOptions.Add(Format('No. of Columns=%d', [aLink.NumGridColumns]));
     aLink.MAI.AnalysisSummary.AnalysisOptions.Add(Format('Target Clade Size=%d', [aLink.CladeSize]));
     aLink.MAI.AnalysisSummary.AnalysisOptions.Add(Format('No. of Taxa in Tree=%d', [aLink.NumSequences]));
     aLink.MAI.AnalysisSummary.NumTaxa := aLink.NumSequences;
     aLink.MAI.AnalysisSummary.WriteToFile(NextAvailableFilenameNV('_summary.json'), True);
     t.CleanUpTempEslFiles;
     actualResultsDir := ExtractFilePath(AnalysisPreferencesFileName) + 'actual_results';
     aLink.MoveResultsFiles(actualResultsDir); { moving results to where the test harness expects files to be because MyEsl.exe cannot handle paths properly}
     t.EslLink := nil;
   except
     on E:Exception do
     begin
       if Assigned(aLink.MAI) then
       begin
         if Assigned(aLink.MAI.ARP) then
         begin
           aLink.MAI.ARP.Free;
           aLink.MAI.ARP := nil;
         end;
       end;
       error_nv('error in EslThreadDone', E);
     end;
   end;
 finally
   try
     if {$IFDEF DEBUG}t.IsSuccess{$ELSE}True{$ENDIF}then { if not successful, leave the files in place for debugging}
       t.CleanUpTempEslFiles;
     t.Free;
     Halt(0);
   except
   {$IFDEF DEBUG}
    on E: Exception do
    begin
      error_nv('Error when cleaning up DrPhylo analysis files: ', E);
    end;
   {$ENDIF}
   end;
 end;
end;

procedure TD_MegaMain.RunDrPhylo;
var
  aList: TStringList = nil;
  index: Integer = -1;
begin
  try
    aList := TStringList.Create;
    aList.LoadFromFile(AnalysisPreferencesFileName);
    index := aList.IndexOfName('TargetNodeName');
    if index < 0 then
      error_nv('missing TargetNodeName');
    TargetNodeName := aList.ValueFromIndex[index];
    aList.Delete(index);

    if not RunEslLinker(TargetNodeName, CommandLineTreeFileName, DataFileName, aList) then
      error_nv('failed to run EslLinker');
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;


Initialization

{$IFDEF UNIX}
  {$IFDEF DARWIN}
   {$I resources_mac.lrs}
  {$ELSE}
  {$I resources_unix.lrs}
  {$ENDIF}
{$ELSE}
  {$I resources_windows.lrs}
{$ENDIF}

end.


