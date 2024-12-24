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

unit MLSearchThread;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Classes, StdCtrls, ExtCtrls, SysUtils, Math, SyncObjs,
  MegaConsts, MLTree, MLTreeAnalyzer, MTreeData, MTreeList, MPartitionList,
  MLModels, MAnalysisInfo, MTreeSearchThread, Dialogs, StrHashMap;

type

  { TMLTreeThread }

  TMLTreeThread = class(TTreeSearchThread)
    MLTreeAnalyzer: TMLTreeAnalyzer;

  protected
    FAnalysisInfo: TAnalysisInfo;
    FCanComputeDataCoverage: Boolean;
    FSubTasksCheckCancel: TCheckCancelFunc;
    FSubtaskProgress: Integer;
    FSubtaskStatus: AnsiString;
    FExceptionName: AnsiString;
    FExceptionMessage: AnsiString;
    FLastUpdateTime: TDateTime; { for limiting progress updates}
    procedure DoOnProgress; override;
    function Initialize: Boolean; virtual; abstract;
    procedure Execute; override;
    function SubTaskCheckCancelFunc(Progress: Integer; Status: AnsiString): Boolean; virtual;
    procedure DoSubTaskCheckCancel;
    procedure DoSynchronizeErrorMessage;
    procedure DoSynchronizeAbortMessage;
    function AnalysisDescription: String; override;
  public
    procedure SynchronizeErrorMessage(E: Exception); { GS - switch context to main thread to display error message because the LCL is not thread safe}
    procedure SynchronizeAbortMessage(aMsg: String);
    property SubTasksCheckCancel: TCheckCancelFunc read FSubTasksCheckCancel write FSubTasksCheckCancel;
    property CanComputeDataCoverage: Boolean read FCanComputeDataCoverage write FCanComputeDataCoverage;
    constructor Create(Analyzer: TMLTreeAnalyzer);
    destructor Destroy; override;
    property Status: AnsiString read FStatus write FStatus;
  end;

  { TMLTreeAnalyzeThread }

  TMLTreeAnalyzeThread = class(TMLTreeThread)
  private
    FOptimizeBLens:  boolean;
    FOptimizeParams: boolean;
    FOrigNumSites: Integer;
    MLACheckCancel: TCheckCancelFunc;
  protected
    function CheckCancel(Progress: integer; Status: AnsiString): boolean;
    procedure Search; override;
    function Initialize: Boolean; override;
    function AnalysisDescription: String; override;
  public
    LbsBootTable: PArrayOfInt;
    GroupNames: TStringList;
    property OptimizeBLens: boolean read FOptimizeBLens write FOptimizeBLens;
    property OptimizeParams: boolean read FOptimizeParams write FOptimizeParams;
    property OrigNumSites: Integer read FOrigNumSites write FOrigNumSites;
    constructor Create(Analyzer: TMLTreeAnalyzer);
    destructor Destroy; override;
  end;

  TMLTreeSearchThread = class;

  { TMLTreeSearchOnNodeThread }

  TMLTreeSearchOnNodeThread = class(TMLTreeThread)
  private
    FTempLogLikelihood: Extended;
    FDoExportSearchTrees: Boolean;
    ThreadIndex: integer;
    SwapList: TList;

    MLTree : TMLTree;
    SearchLevel: integer;
    SearchFilter: extended;
    deltaL: extended;
    MaxIter: integer;

    MainThread: TMLTreeSearchThread;
    MyModel: TGammaRateVariationModel;

    p: array of TMLTreeNode;
    q: array of TMLTreeNode;
    FSearchTreeExportNodes: array of TMLTreeNode;
//    cfg: PArrayOfInt;
//    L0: extended;
//    vb,sb: extended;
    CI: double;

    d: integer;

    Done: boolean;

    function SetInitialSubtree(node: TMLTreeNode): boolean;
    function IncSubtreeDown(index: integer): boolean;
    function IncSubtreeUp(index: integer): boolean;
    procedure DecSubtreeDown(index: integer);
    procedure ResetBLens;
    procedure InitMem;
    procedure InitSearchTreeExportNodes;
    procedure UpdateSearchTreeExportNodes;
    procedure UpdateSearchTreeExportList(logL: Extended; comment: String); overload;
    procedure UpdateSearchTreeExportList(logL: Extended; aData: TTreeData; comment: String); overload;
    function SearchEachNode(node: TMLTreeNode):boolean;
    procedure CheckNeedToFreeMem;
  protected
    procedure Search; override;
    procedure Execute; override;
    function Initialize: Boolean; override;
  public
    SearchTrees: TTreeList;
    OtuNames: TStringList;
    SearchTreeNewickStrings: TStringList;
    IsInitialized: Boolean;
    constructor Create(parent: TMLTreeSearchThread;
                       index: integer;
                       MLA: TMLTreeAnalyzer);

    destructor Destroy; override;
    function ExportTreesSearched(filename: String): Boolean;
    function SearchTreeExportNewickString: String;
    property DoExportSearchTrees: Boolean read FDoExportSearchTrees write FDoExportSearchTrees;
  end;

  { TMLTreeSearchThread }

  TMLTreeSearchThread = class(TMLTreeSearchOnNodeThread)
  private
    FTempModel: TGammaRateVariationModel;
    ThreadLock: TCriticalSection;
    ChildThread: array of TMLTreeSearchOnNodeThread;
    MasterSwapList: TList;
    MLACheckCancel: TCheckCancelFunc;

    FNoOfThreads: integer;

    MakeInitialTree: boolean;
    NodeStarted: array of boolean;
    ChildStarted: boolean;

    OptimizeMsg: string;

    function GetNodeForSearch: TMLTreeNode;
    function CheckChildThreadDone: boolean;
    function CheckChildThreadFailed: Boolean;
    procedure RestartChildThreads;
    function GetProgress: integer;
    function DumpStartingTreesData(startingTrees: TTreeList; filename: String): Boolean;
  protected
    procedure ReInitialize;
    procedure UpdateMessagesLog(aStr: String);
    function GenerateStartingNJTree: TMPMLTreeData;
    function GenerateParsimonyStartingTrees(var aMpTreeList: TTreeList; const minLnL: Double; var numTreesSearched: Integer): String;
    function Initialize: Boolean; override;{ moved computational stuff (i.e. CreateSubstitutionModel) out of ProcessSeqMLPhylogenyCommand and into here because it freezes the gui when using large datasets}
    function InitializeForMultiStartTreeSearch(aStartingTree: TMPMLTreeData): Boolean;
    function GetIsInitialTree: boolean;
    function CheckCancel(Progress: integer; Status: AnsiString): boolean;
    procedure Search; override;
    procedure ExecuteMultiStartTreeSearch;
    function SearchMultiStartTree(var aTree: TTreeData): Double;
    procedure Execute; override;
  public

    IsBootstrapReplicate: Boolean;
    property IsInitialTree: boolean read GetIsInitialTree;
    property NoOfThreads: integer read FNoOfThreads write FNoOfThreads;

    constructor Create(MLA: TMLTreeAnalyzer);
    destructor Destroy; override;
  end;

  { TBootstrapMLThread }

  TBootstrapMLThread = class(TMLTreeThread)
  private
    FIsAdaptiveBootstrap: Boolean;
  protected
    FTempPrecision: TArrayOfExt;
    Rep: integer;
    FNoOfReplication: integer;
    FBootstrapTrees: TPartitionList;
    FSavedReplicates: TList;
    FBootTable: PArrayOfInt;
    FNoOfThreads: integer;
    function OrigTotalNumSites: Integer;
    function MyCheckCancel(Progress: integer; Status: AnsiString): boolean;
    function Initialize: Boolean; override;{ moved computational stuff (i.e. CreateSubstitutionModel) out of ProcessSeqMLPhylogenyCommand and into here because it freezes the gui when using large datasets}
    procedure ComputeTempBclPrecision(finalTree: TTreeData);
    procedure ComputeFinalBclPrecision(var finalTree: TTreeData);
    procedure GetRandomSampleOfSavedReplicates(const sampleSize: Integer; var aList: TList);

    procedure Search; override;
    procedure AddBootTree(nodeData: PArrayOfNodeData; freq: Integer);
    function AnalysisDescription: String; override;
    procedure ClearSavedReplicates;
    procedure SaveReplicate(aReplicate: TTreeData);
    function PrecisionHasStabilized(const OriTree: TTreeData; var numNodesStable: Integer): Boolean;
    function PrecisionThreshold: Double;
  public

    property NoOfReplication: integer read FNoOfReplication write FNoOfReplication;
    property BootstrapTrees: TPartitionList read FBootstrapTrees write FBootstrapTrees;
    property NoOfThreads: integer read FNoOfThreads write FNoOfThreads;

    constructor Create(MLA: TMLTreeAnalyzer);
    destructor Destroy; override;
    property IsAdaptiveBootstrap: Boolean read FIsAdaptiveBootstrap write FIsAdaptiveBootstrap;
  end;

{
  TBootstrapMLBLenThread = class(TMLTreeThread)
    SE: array of extended;
  protected
    FNoOfReplication: integer;
    FMolecularClock: boolean;

    procedure Search; override;
    procedure DoOnProgress; override;

    function BootstrapCheckCancel(Progress: integer; Status: string): boolean;
  public
    property NoOfReplication: integer read FNoOfReplication write FNoOfReplication;
    property MolecularClock: boolean read FMolecularClock write FMolecularClock;
  end;
}

  { TModelTestThread }

  TModelTestThread = class(TMLTreeThread)
    private
      FDevMsg: String;
      FBootTable: PArrayOfInt;
      FHasResultsToDisplay: Boolean;
      FIsAdaptiveModelTest: Boolean;
      FOrigNumSites: Integer;
      FProgressMessage: String;
      procedure SetBootTable(AValue: PArrayOfInt);
    protected
      FNoOfThreads: Integer;
      MLACheckCancel: TCheckCancelFunc;
      FModelsToTest: TGammaRateVariationModelArray;
      function CheckCancel(Progress: integer; Status: AnsiString): boolean;
      function Initialize: Boolean; override;
      procedure DoOnProgress; override;
      procedure Search; override;
      procedure MyWriteToDevConsole(aMsg: String);
      procedure DoWriteToDevConsole;
      function AnalysisDescription: String; override;
    public
      constructor Create(Analyzer: TMLTreeAnalyzer; numThreads: Integer);
      constructor CreateForAdaptiveModelTest(aModels: TGammaRateVariationModelArray; numThreads: Integer);
      destructor Destroy; override;
      property NoOfThreads: Integer read FNoOfThreads write FNoOfThreads;
      property BootTable: PArrayOfInt read FBootTable write SetBootTable;
      property OrigNumSites: Integer read FOrigNumSites write FOrigNumSites;
      property IsAdaptiveModelTest: Boolean read FIsAdaptiveModelTest;
      property HasResultsToDisplay: Boolean read FHasResultsToDisplay;
      property ProgressMessage: String read FProgressMessage write FProgressMessage;
  end;

  { TMLClockTestThread }

  TMLClockTestThread = class(TMLTreeThread)
  private
    MLACheckCancel: TCheckCancelFunc;
    FOptimizeBLens:  boolean;
    FOptimizeParams: boolean;
    FNumBootReps: Integer;

    function CheckCancel(Progress: Integer; Status: AnsiString): Boolean;
    procedure InferOutgroup;
  protected
    procedure DoOnProgress; override;
    procedure Search; override;
    function Initialize: Boolean; override;
    function AnalysisDescription: String; override;
  public
    IsSuccess: Boolean;
    constructor Create(MLTreeAnalyzer: TMLTreeAnalyzer);
    destructor Destroy; override;
    property OptimizeBLens: boolean read FOptimizeBLens write FOptimizeBLens;
    property OptimizeParams: boolean read FOptimizeParams write FOptimizeParams;
    property AnalysisInfo: TAnalysisInfo read FAnalysisInfo write FAnalysisInfo;
    property NumBootReps: Integer read FNumBootReps write FNumBootReps;
  end;

  procedure BootStrap(f: PArrayOfInt; n: integer);

implementation

uses
  {$IFDEF DEBUG}mdeveloper_console,{$ENDIF}
  {$IFDEF VISUAL_BUILD}
  Mega_Main, {$IFDEF DEBUG}MegaUtils_NV,{$ENDIF}
  {$ELSE}
  MegaUtils_NV, MD_MegaMain, LazFileUtils,
  {$ENDIF}
  MegaUtils, MegaErrUtils, MSubstitutionModelUtils, MTreePack, MDistPack,
  dateutils, StringUtils, MegaAnalysisPrefStrings, msitecoverage, mextendedlist,
  mstringbuilder, MNewickExportOptions, mtree_display_setup, MTreeDataAdapter;

constructor TMLTreeThread.Create(Analyzer: TMLTreeAnalyzer);
begin
  inherited Create;
  FAnalysisInfo := nil;
  FLastUpdateTime := Time;
  FExceptionName := EmptyStr;
  FExceptionMessage := EmptyStr;
  MLTreeAnalyzer := Analyzer;
  FSubtasksCheckCancel := nil;
  FStatus := 'Preparing ML Analysis...';
  FCanComputeDataCoverage := False;
end;

destructor TMLTreeThread.Destroy;
begin
  if FCanceled then
  begin
    if Assigned(MLTreeAnalyzer) then
      MLTreeAnalyzer.Free;
  end;
  inherited;
end;

procedure TMLTreeThread.Execute;
begin
  try
    StartExecute;
    ReturnValue := 1;
    if Terminated then begin
      Canceled := true;
      Exit;
    end;
    Search;
    if Terminated then
      Canceled := true
    else
      ReturnValue := 0;
    {$IFNDEF VISUAL_BUILD}{$IFNDEF MSWINDOWS}
    UpdatePeakMemUsage;
    {$ENDIF}{$ENDIF}
    EndExecute;
    UpdateAnalysisSummary;
    {$IFDEF VISUAL_BUILD}
    if Assigned(FAnalysisInfo) and FCanComputeDataCoverage then
      ComputeSiteCoverage(FAnalysisInfo);
    {$ENDIF}
  Except
    on E: EAbort do
    begin
      Canceled := True;
      MessagesLog.Add('calculation aborted by user');
    end;
    On E : Exception do
    begin
      FIsSuccess := False;
      MessagesLog.Add(E.ClassName);
      MessagesLog.Add(E.Message);
    end;

  end;
end;

function TMLTreeThread.SubTaskCheckCancelFunc(Progress: Integer; Status: AnsiString): Boolean;
begin
  Result := Terminated;
  if (not Terminated) and Assigned(FSubtasksCheckCancel) then
  begin
    FSubtaskProgress := Progress;
    FSubtaskStatus := Status;
    Synchronize(DoSubTaskCheckCancel);
  end;
end;

procedure TMLTreeThread.DoOnProgress;
begin
  if FStatus <> EmptyStr then
    FProgressDlg.UpdateRunStatusInfo('Status', FStatus);
  //  FProgressDlg.Progress := FProgress;
  FProgressDlg.UpdatePercentProgress(FProgress);
  if Assigned(MLTreeAnalyzer) and Assigned(MLTreeAnalyzer.MLTree) then
    if MLTreeAnalyzer.LogLikelihood < -0.000000000001 then
      FProgressDlg.UpdateRunStatusInfo('Log Likelihood', Format('%.2n', [MLTreeAnalyzer.LogLikelihood]));
  FProgressDlg.Refresh;
end;

procedure TMLTreeThread.DoSubTaskCheckCancel;
begin
  if Assigned(MLTreeAnalyzer) and Assigned(MLTreeAnalyzer.MLTree) then
    if MLTreeAnalyzer.LogLikelihood < -0.000000000001 then
      FProgressDlg.UpdateRunStatusInfo('Log Likelihood', Format('%.2n', [MLTreeAnalyzer.LogLikelihood]));
  if Assigned(FSubtasksCheckCancel) then
    Canceled := FSubtasksCheckCancel(FSubtaskProgress, FSubtaskStatus);
  if Canceled then
    Terminate;
end;

procedure TMLTreeThread.DoSynchronizeErrorMessage;
begin
  {$IFDEF VISUAL_BUILD}
  ShowMessage(Format('Error (%s) in ML search thread: %s', [FExceptionName, FExceptionMessage]));
  {$ELSE}
  error_nv(Format('Error (%s) in ML search thread: %s', [FExceptionName, FExceptionMessage]));
  {$ENDIF}
end;

procedure TMLTreeThread.DoSynchronizeAbortMessage;
begin
  {$IFDEF VISUAL_BUILD}
  ShowMessage(Format('%s : %s', [FExceptionName, FExceptionMessage]));
  {$ELSE}
  error_nv(Format('%s: %s', [FExceptionName, FExceptionMessage]));
  {$ENDIF}
end;


function TMLTreeThread.AnalysisDescription: String;
begin
  Result := 'ML Tree Search';
end;

procedure TMLTreeThread.SynchronizeErrorMessage(E: Exception);
begin
  {$IFDEF VISUAL_BUILD}
  FExceptionName := E.ClassName;
  {$IFDEF DEBUG}
  FExceptionMessage := ExceptionCallStack(E);
  {$ELSE}
  FExceptionMessage := E.Message;
  {$ENDIF}
  Synchronize(DoSynchronizeErrorMessage);
  {$ELSE}
  error_nv('failure in TMLTreeThread', E);
  {$ENDIF}
end;

procedure TMLTreeThread.SynchronizeAbortMessage(aMsg: String);
begin
  {$IFDEF VISUAL_BUILD}
  FExceptionName := 'ML calculation cancelled';
  FExceptionMessage := aMsg;
  Synchronize(DoSynchronizeAbortMessage);
  {$ELSE}
  error_nv('failure in TMLTreeThread: user cancelled');
  {$ENDIF}
end;

///////////////////////
//  Model test
///////////////////////

constructor TModelTestThread.Create(Analyzer: TMLTreeAnalyzer; numThreads: Integer);
begin
  inherited Create(Analyzer);
  FHasResultsToDisplay := False;
  FIsAdaptiveModelTest := False;
  FNoOfThreads := numThreads;
  {$IFNDEF VISUAL_BUILD}
  FreeOnTerminate := true;
  {$ENDIF}
  {$IFDEF DEBUG}{$IFDEF VISUAL_BUILD}
  if Assigned(Analyzer) then
    Analyzer.DevConsoleProc := MyWriteToDevConsole;
  {$ENDIF}{$ENDIF}
  SkipSummaryUpdate := False;
end;

constructor TModelTestThread.CreateForAdaptiveModelTest(aModels: TGammaRateVariationModelArray; numThreads: Integer);
begin
  Create(nil, numThreads);
  FModelsToTest := aModels;
  FIsAdaptiveModelTest := True;
end;

destructor TModelTestThread.Destroy;
begin
  if MLTreeAnalyzer <> nil then
    MLTreeAnalyzer.CheckCancel := MLACheckCancel;
  inherited;
end;

procedure TModelTestThread.SetBootTable(AValue: PArrayOfInt);
begin
  if FBootTable = AValue then Exit;
  FBootTable := AValue;
end;

function TModelTestThread.CheckCancel(Progress: integer; Status: AnsiString): boolean;
begin
  FProgress := Progress;
  FStatus   := Status;
  try
    if (FProgressDlg <> nil) {$IFDEF VISUAL_BUILD}and (FProgressDlg.PercentGauge <> nil) {$ENDIF} then
      OnProgress(Progress);
  Except on E: Exception do
    // do nothing, it's not worth it to have the program crash if we lose a progress update.
  end;
  result := Terminated;
end;

function TModelTestThread.Initialize: Boolean;
var
  MAI : TAnalysisInfo;
  AModel: TGammaRateVariationModel;
  ATree: TTreeData;
  i: Integer = -1;
begin
  Result := False;

  MAI := FProgressDlg.FMAI;
  AModel := CreateSubstitutionModel(MAI, SubTaskCheckCancelFunc);
  ProgressDlg.RegisterCancellable(AModel);
  if Assigned(FBootTable) then
  begin
    AModel.BootTable := FBootTable;
    AModel.OrigNoOfSites := FOrigNumSites;
  end;
  MAI.MyOriTreeList := TTreeList.Create;
  if MAI.MyTreePack.DoesContain(ttUserTree) then
  begin
    MAI.MyOriTreeList.ImportFromNewick(MAI.MyUserNewickTree, MAI.MyOtuNames, False);
    ATree := TTreeData.Create(MAI.NoOfSeqs,false,false,false);
    ATree.Assign(MAI.MyOriTreeList[0]);
    MLTreeAnalyzer := TMLTreeAnalyzer.Create(MAI.MySeqStrings, ATree, AModel);  //  to specify initial tree
  end
  else
  begin
    MLTreeAnalyzer := TMLTreeAnalyzer.Create(MAI.MySeqStrings, nil, AModel);  //  to force making initial tree
  end;
  MLACheckCancel := MLTreeAnalyzer.CheckCancel;
  MLTreeAnalyzer.CheckCancel := CheckCancel;
  {$IFDEF DEBUG}
  MLTreeAnalyzer.DevConsoleProc := MyWriteToDevConsole;
  for i := 0 to MLTreeAnalyzer.ModelInfoList.Count - 1 do
    MLTreeAnalyzer.ModelInfoList[i].DevConsoleProc := MyWriteToDevConsole;
  {$ENDIF}
  //MLTreeAnalyzer.SubTaskCheckCancel := SubTaskCheckCancelFunc;
  if MAI.MLSearchFilter > -1 then
    MLTreeAnalyzer.SearchFilter := MAI.MLSearchFilter;
  UpdateRunStatusInfo('Status', 'Making initial tree');

  MAI.MyMLAnalysisPack := MLTreeAnalyzer;
  MLTreeAnalyzer.SeqNames := MAI.MyOtuNames;
  MLTreeAnalyzer.InitialTreeOption := MAI.MyInitialTreeMethod;   //0: NJ; 1: BIONJ; 2: ME; 3: MP; 4: NJ/BIONJ(default)
  MAI.MyTreePack.MLInitialTreesMethod := MAI.MyInitialTreeMethod; // so that citation expert can easily get the value
  MLACheckCancel := MLTreeAnalyzer.CheckCancel;
  MLTreeAnalyzer.CheckCancel := CheckCancel;
  FNoOfTreesExamined := 0;
  MLTreeAnalyzer.RuntimeProgress := FProgressDlg;
  FProgressDlg.RegisterCancellable(MLTreeAnalyzer);
  Result := True;
end;

procedure TModelTestThread.Search;
begin
  FLastUpdateTime := Time;
  if not Initialize then
    raise Exception.Create('Failed to initialize TModelTestThread');
  try
    if Assigned(FBootTable) then
    begin
      MLTreeAnalyzer.IsModelTamer := True;
      MLTreeAnalyzer.ModelTamerBootTable := FBootTable;
      MLTreeAnalyzer.OrigNoOfSites := FOrigNumSites;
    end;

    if MLTreeAnalyzer.Model is TProteinMatrixModel then
    begin
      UpdateRunStatusInfo('Status', FProgressMessage);
      if FIsAdaptiveModelTest then
      begin
        Assert(Length(FModelsToTest) > 0, 'Got zero models to test but should have between 2 and 60 protein models to test');
        MLTreeAnalyzer.TestProteinModels(True, FModelsToTest, FNoOfThreads);
      end
      else
        MLTreeAnalyzer.TestProteinModels(FNoOfThreads);
    end
    else
    begin
      UpdateRunStatusInfo('Status', FProgressMessage);
      if FIsAdaptiveModelTest then
      begin
        Assert(Length(FModelsToTest) > 1, 'Got zero models to test but should have between 2 and 24 DNA models to test');
        MLTreeAnalyzer.TestDNAModels(True, FModelsToTest, FNoOfThreads);
      end
      else
        MLTreeAnalyzer.TestDNAModels(FNoOfThreads);
    end;
  except
    {$IFDEF VISUAL_BUILD}
    on E:EAbort do
    begin
      HideProgress;
      FIsSuccess := False;
      if MLTreeAnalyzer.NumModelsEvaluated > 0 then
        FHasResultsToDisplay := True;
      Canceled := True;
      MessagesLog.Add('Model test cancelled by user');
    end;
    {$ENDIF}
    on E:Exception do
    begin
      HideProgress;
      FIsSuccess := False;
      MessagesLog.Add(E.Message);
      {$IFNDEF VISUAL_BUILD}
       error_nv('Error in TModelTestThread', E);
      {$ENDIF}
    end;
  end;
end;

procedure TModelTestThread.MyWriteToDevConsole(aMsg: String);
begin
  {$IFDEF DEBUG}{$IFDEF VISUAL_BUILD}
  FDevMsg := aMsg;
  Synchronize(DoWriteToDevConsole);
  {$ENDIF}{$ENDIF}
end;

procedure TModelTestThread.DoWriteToDevConsole;
begin
  {$IFDEF DEBUG}
  WriteToDevConsole(FDevMsg);
  {$ENDIF}
end;

function TModelTestThread.AnalysisDescription: String;
begin
  Result := 'Model Test';
end;

procedure TModelTestThread.DoOnProgress;
begin
  if Assigned(FProgressDlg) then
  begin
    FProgressDlg.Progress := FProgress;
    FProgressDlg.UpdateRunStatusInfo('Status', FStatus);
    if MLTreeAnalyzer.MLTree <> nil then
      if MLTreeAnalyzer.LogLikelihood < -0.000000000001 then
        FprogressDlg.UpdateRunStatusInfo('Log Likelihood', Format('%.2n', [MLTreeAnalyzer.LogLikelihood]));
    {$IFDEF VISUAL_BUILD}
      FProgressDlg.Refresh;
    {$ENDIF}
  end;
end;

///////////////////////
//  Analyze ML tree
///////////////////////

constructor TMLTreeAnalyzeThread.Create(Analyzer: TMLTreeAnalyzer);
begin
  inherited Create(Analyzer);
  FCanComputeDataCoverage := True;
  LbsBootTable := nil;
  OrigNumSites := -1;
end;

destructor TMLTreeAnalyzeThread.Destroy;
begin
  inherited;
end;

function TMLTreeAnalyzeThread.CheckCancel(Progress: integer; Status: AnsiString): boolean;
begin
  FProgress := Progress;
  FStatus   := Status;
  try
    if (FProgressDlg <> nil) {$IFDEF VISUAL_BUILD}and (FProgressDlg.PercentGauge <> nil){$ENDIF} then
      OnProgress(Progress);
  Except on E: Exception do
    // do nothing, it's not worth it to have the program crash if we lose a progress update.
  end;
  result := Terminated;
end;

procedure TMLTreeAnalyzeThread.Search;
var
  {$IFNDEF VISUAL_BUILD}
  aInfo: TModelInfo = nil;
  {$ENDIF}
  aTree: TTreeData = nil;
  NodeLabels: TStringList = nil;
begin
  {$IFDEF DEBUG}SaveMLlog(' MLTreeAnalyzeThread.Search started');{$ENDIF}
  try
    FLastUpdateTime := Time;
    if not Initialize then
      raise Exception.Create('Failed to initialize TMLTreeAnalyzeThread');
    NodeLabels := TStringList.Create;

    OnProgress(FProgress);
    if MLTreeAnalyzer.IsLittleBootstrap then
    begin
      MLTreeAnalyzer.OptimizeMessage := SKIP_STATUS_UPDATE;
      FStatus := SKIP_STATUS_UPDATE;
    end
    else
    begin
      MLTreeAnalyzer.OptimizeMessage := 'Optimizing tree';
      FStatus := 'Optimizing tree';
    end;
    if FOrigNumSites > 0 then
    begin
      MLTreeAnalyzer.SetIsLittleBootstrap(True, FOrigNumSites);
      MLTreeAnalyzer.OrigNoOfSites := FOrigNumSites;
    end;
//    MLTreeAnalyzer.Initialize;
    NodeLabels.Assign(FAnalysisInfo.MyOriTreeList[0].GetInternalNodeLabels);
    ProgressDlg.RegisterCancellable(MLTreeAnalyzer);
    MLTreeAnalyzer.IsDoingTreeSearch := False;
    if OptimizeBlens then
    begin
      if OptimizeParams then
        MLTreeAnalyzer.Optimize
      else
        MLTreeAnalyzer.OptimizeBLens;
    end
    else if OptimizeParams then
      MLTreeAnalyzer.OptimizeParams;

    if FAnalysisInfo.ClockType = ctGlobal then
    begin
      aTree := TTreeData.Create(MLTreeAnalyzer.NoOfSeqs, True, True, False, False);
      MLTreeAnalyzer.MLTree.GetTreeData(aTree);
      MLTreeAnalyzer.IsGlobalClock := True;
      MLTreeAnalyzer.GlobalClockLevel := 0;
      if IsDeveloper then
        MLTreeAnalyzer.MakeClockTree(aTree, FAnalysisInfo.MaxRateRatio, FAnalysisInfo.MyOtuNames)
      else
        MLTreeAnalyzer.MakeClockTree(aTree, DEFAULT_MAX_RATE_RATIO, FAnalysisInfo.MyOtuNames);
      FAnalysisInfo.MyOriTreeList.Delete(0);
      aTree.GetInternalNodeLabels.Assign(NodeLabels);
      FAnalysisInfo.MyOriTreeList.Add(aTree);
    end
    else if FAnalysisInfo.MyUsrOperation <> dtdoEpML then
    begin
      FAnalysisInfo.MyOriTreeList.Clear;
      aTree := TTreeData.Create(MLTreeAnalyzer.NoOfSeqs, True, True, False, False);
      MLTreeAnalyzer.MLTree.GetTreeData(aTree);
      aTree.GetInternalNodeLabels.Assign(NodeLabels);
      FAnalysisInfo.MyOriTreeList.Add(aTree);
    end;

    ReturnValue := 0;

  except
    on E:EAbort do { if the user cancelled, exit gracefully, otherwise, let the exception propogate}
    begin
      Canceled := True;
      MessagesLog.Add('user tree analysis cancelled');
    end;
  end;
  {$IFDEF DEBUG}SaveMLlog(' MLTreeAnalyzeThread.Search ended'+chr(13)+chr(10));{$ENDIF}
end;

function TMLTreeAnalyzeThread.Initialize: Boolean;
var
  AModel: TGammaRateVariationModel;
  ATree: TTreeData;
  //LampreyFound: Boolean;
  i: Integer;
begin
  {$IFDEF DEBUG}SaveMLlog(' MLTreeAnalyzeThread.Initialize started');{$ENDIF}
  FProgress := 1;
  OnProgress(FProgress);
  Result := False;
  FAnalysisInfo := TAnalysisInfo(FProgressDlg.FMAI);
  AModel := CreateSubstitutionModel(FAnalysisInfo, SubtaskCheckCancelFunc);
  if Assigned(LbsBootTable) then
  begin
    AModel.BootTable := LbsBootTable;
    Assert(FOrigNumSites > 0, 'orig number of sites not set');
    AModel.OrigNoOfSites := FOrigNumSites;
  end;
  SubtaskCheckCancelFunc(0, 'Initializing tree');
  FAnalysisInfo.MyOriTreeList := TTreeList.Create;
  if FAnalysisInfo.MyTreePack.DoesContain(ttUserTree) then
  begin
    if not FAnalysisInfo.MyOriTreeList.ImportFromNewick(FAnalysisInfo.MyUserNewickTree, FAnalysisInfo.MyOtuNames, False) then
      raise Exception.Create('failed to load the newick string: ' + FAnalysisInfo.MyUserNewickTree);
    if (FAnalysisInfo.MyUsrOperation = dtdoEpML) or FAnalysisInfo.MyTreePack.DoesContain(ttClock) or (FAnalysisInfo.MyUsrOperation = dtdoMLInferAncSeq) then
    begin
      Assert(FAnalysisInfo.MyOriTreeList.NoOfOTUs = FAnalysisInfo.MyOtuNames.Count, Format('expected %d names for tree but got %d', [FAnalysisInfo.MyOriTreelist.NoOfOTUs, FAnalysisInfo.MyOtuNames.Count]));
      Assert(Assigned(GroupNames));
      for i := 0 to GroupNames.Count - 1 do
        if SameText(GroupNames.ValueFromIndex[i], 'outgroup') then
          FAnalysisInfo.MyOriTreeList.MarkTaxonAsOutgroupMember(GroupNames.Names[i]);
    end;
    //if FAnalysisInfo.MyUsrOperation = dtdoMLInferAncSeqMyPeg then
    //begin
    //  LampreyFound := FAnalysisInfo.MyOriTreeList.MarkTaxonAsOutgroupMember('Lamprey');
    //  Assert(LampreyFound);
    //end;
    ATree := TTreeData.Create(FAnalysisInfo.NoOfSeqs,false,false,false);
    ATree.Assign(FAnalysisInfo.MyOriTreeList[0]);
    MLTreeAnalyzer := TMLTreeAnalyzer.Create(FAnalysisInfo.MySeqStrings, ATree, AModel);  //  to specify initial tree
  end
  else
    MLTreeAnalyzer := TMLTreeAnalyzer.Create(FAnalysisInfo.MySeqStrings, nil, AModel);                    //  to force making initial tree

  if (FAnalysisInfo.MyUsrOperation = dtdoMLInferAncSeq) or (FAnalysisInfo.MyUsrOperation = dtdoMLInferAncSeqMyPeg) then
  begin
    if FAnalysisInfo.OutgroupDefined then
    begin
      MLTreeAnalyzer.NeedsRootByOutgroup := True;
      FAnalysisInfo.MyOriTreeList.IsRooted := True;
    end;
  end;

  if (FAnalysisInfo.MyUsrOperation = dtdoEpML) and (not FAnalysisInfo.KeepUserTreeTimes) then
  begin
    MLTreeAnalyzer.NeedsRootByOutgroup := True;
    FAnalysisInfo.MyOriTreeList.IsRooted := True;
  end;
  MLACheckCancel := MLTreeAnalyzer.CheckCancel;
  MLTreeAnalyzer.CheckCancel := CheckCancel;
  FProgressDlg.RegisterCancellable(MLTreeAnalyzer);
  MLTreeAnalyzer.SubTaskCheckCancel := SubtaskCheckCancelFunc;
  MLTreeAnalyzer.NoOfThreadsToUse := FAnalysisInfo.MyNumThreadsToUse;
  MLTreeAnalyzer.SeqNames := FAnalysisInfo.MyOtuNames;
  if Assigned(LbsBootTable) then
    MLTreeAnalyzer.SetIsLittleBootstrap(True, Length(FAnalysisInfo.MySeqStrings[0]));
  if FAnalysisInfo.MLSearchFilter > -1 then
    MLTreeAnalyzer.SearchFilter := FAnalysisInfo.MLSearchFilter;
  if FAnalysisInfo.MyUsrOperation = dtdoMLComputeUserTreeBLens then
    MLTreeAnalyzer.IsDoingTreeSearch := False;
  if (FAnalysisInfo.MyUsrOperation = dtdoMLComputeUserTreeBLens) and (FAnalysisInfo.MyTreePack.DoesContain(ttClock)) then
  begin
    MLTreeAnalyzer.IsGlobalClock := True;
    FAnalysisInfo.ClockType := ctGlobal;
    FAnalysisInfo.ClockTypeSet := True;
    FAnalysisInfo.ClockLevel := clNoStdErr;
  end;
  FAnalysisInfo.MyMLAnalysisPack := MLTreeAnalyzer;
  FProgress := 2;
  Result := True;
  {$IFDEF DEBUG}SaveMLlog(' MLTreeAnalyzeThread.Initialize ended');{$ENDIF}
end;

function TMLTreeAnalyzeThread.AnalysisDescription: String;
begin
  Result := 'ML Tree Analysis';
end;

///////////////////////
//  Search ML tree
///////////////////////

type
  TSwapData = class
    swapindex: integer;
    dL  : extended;
    node: array of integer;
    blen: array of extended;
    ase : array of extended;
    flag: boolean;
  private
    nnodes: integer;

    procedure SetNoOfNodes(value: integer);
  public
    property NoOfNodes: integer read nnodes write SetNoOfNodes;

    constructor Create(n: integer);
    destructor Destroy; override;
  end;

constructor TSwapData.Create(n: integer);
begin
  inherited Create;

  setlength(node, n+1);
  setlength(blen, n+1);
  setlength(ase, n+1);

  nnodes := n;
end;

destructor TSwapData.Destroy;
begin
  setlength(node, 0);
  setlength(blen, 0);
  setlength(ase, 0);

  inherited;
end;

procedure TSwapData.SetNoOfNodes(value: integer);
begin
  if value = nnodes then
    exit;

  setlength(node, value+1);
  setlength(blen, value+1);
  setlength(ase, value+1);

  nnodes := value;
end;

procedure TMLTreeSearchOnNodeThread.CheckNeedToFreeMem;
var
  i: Integer;
begin
  if Length(q) > 0 then
    for i := Low(q) to High(q) do
      if Assigned(q[i]) then
        FreeAndNil(q[i]);
  if Assigned(MyModel) then
    FreeAndNil(MyModel);
  if Assigned(SwapList) then
  begin
    if SwapList.Count > 0 then
      for i := 0 to SwapList.Count-1 do
        TSwapData(SwapList[i]).Free;
    FreeAndNil(SwapList);
  end;
end;

constructor TMLTreeSearchOnNodeThread.Create(parent: TMLTreeSearchThread; index: integer; MLA: TMLTreeAnalyzer);
begin
  inherited Create(MLA);
  FDoExportSearchTrees := False;
  SearchTrees := TTreeList.Create;
  SearchTreeNewickStrings := TStringList.Create;
  FTempLogLikelihood := 0;
  IsInitialized := False;
  Priority := tpNormal;
  ThreadIndex     := index;
  MainThread      := parent;
  FreeOnTerminate := true;
  MyModel := nil;
  SwapList := nil;
end;

destructor TMLTreeSearchOnNodeThread.Destroy;
var
  i: integer;
begin
  if MyModel <> nil then
    FreeAndNil(MyModel);
  if Assigned(SearchTrees) then
    SearchTrees.Free;
  if Assigned(SearchTreeNewickStrings) then
    SearchTreeNewickStrings.Free;
  if Assigned(SwapList) then
  begin
    if SwapList.Count > 0 then
      for i := 0 to SwapList.Count-1 do
        TSwapData(SwapList[i]).Free;
    FreeAndNil(SwapList);
  end;

  if length(q) > 0 then
    for i := 0 to length(q)-1 do
      q[i].Free;

  setlength(q, 0);
  setlength(p, 0);

  inherited;
end;

function TMLTreeSearchOnNodeThread.ExportTreesSearched(filename: String): Boolean;
{$IFNDEF VISUAL_BUILD}
var
  aFile: TextFile;
  newick: String = '';
  i: Integer = -1;
{$ENDIF}
begin
  Result := False;
  {$IFNDEF VISUAL_BUILD}
  try
    AssignFile(aFile, filename);
    if not FileExists(filename) then
      Rewrite(aFile)
    else
      Append(aFile);
    if SearchTreeNewickStrings.Count > 0 then
      for i := 0 to SearchTreeNewickStrings.Count - 1 do
        WriteLn(aFile, SearchTreeNewickStrings[i]);
    if not Assigned(SearchTrees.OTUNameList) then
      SearchTrees.OTUNameList := TStringList.Create;
    SearchTrees.OTUNameList.Assign(OtuNames);
    if SearchTrees.Count > 0 then
      for i := 0 to SearchTrees.Count - 1 do
      begin
        newick := SearchTrees.OutputNewickTree(i, True, False, 0.0);
        WriteLn(aFile, Format('[debug: %.2f] %s', [SearchTrees[i].Value, newick]));
      end;
  finally
    CloseFile(aFile);
  end;
  Result := FileExists(filename);
  {$ENDIF}
end;

function TMLTreeSearchOnNodeThread.SearchTreeExportNewickString: String;
const
  Precision = 8;
  ValueDigits = 16;
var
  aRoot: TMLTreeNode = nil;

  function NewickStringRecursive(ANode: TMLTreeNode): String;
  begin
    if aNode = aRoot then
    begin
      if aNode.des1.index < MLTree.NoOfSeqs then
      begin
        Result := '(' + TTreeList.CleanOtuNameForExport(OtuNames[aNode.des1.index]) + ':' + Trim(FormatDoubleSafe(aNode.des1.blen + aNode.des2.blen, Precision, ValueDigits)) + ',' + NewickStringRecursive(aNode.des2) + ')';
      end
      else if aNode.des2.index < MLTree.NoOfSeqs then
        Result := '(' + NewickStringRecursive(aNode.des1) + ',' + TTreeList.CleanOtuNameForExport(OtuNames[aNode.des2.index]) + ':' + Trim(FormatDoubleSafe(aNode.des1.blen + aNode.des2.blen, Precision, ValueDigits)) + ')'
      else
        Result := '(' + NewickStringRecursive(ANode.des1) + ':' + Trim(FormatDoubleSafe(aNode.des1.blen, Precision, ValueDigits)) + ',' + NewickStringRecursive(ANode.des2) + ':' + Trim(FormatDoubleSafe(aNode.des2.blen, Precision, ValueDigits)) + ')'
    end
    else if ANode.OTU then
      Result := TTreeList.CleanOtuNameForExport(OtuNames[ANode.Index])
    else
      Result := '(' + NewickStringRecursive(ANode.des1) + ':' + Trim(FormatDoubleSafe(aNode.des1.blen, Precision, ValueDigits)) + ',' + NewickStringRecursive(ANode.des2) + ':' + Trim(FormatDoubleSafe(aNode.des2.blen, Precision, ValueDigits)) + ')';
  end;

begin
  UpdateSearchTreeExportNodes;
  aRoot := FSearchTreeExportNodes[0];
  while Assigned(aRoot.anc) do
    aRoot := aRoot.anc;
  Result := NewickStringRecursive(aRoot) + ';';
end;

function TMLTreeSearchOnNodeThread.Initialize: Boolean;
var
  i: Integer;
  aNoOfSites: Int64;
begin
  Result := False;
  CheckNeedToFreeMem;
  if Assigned(MLTreeAnalyzer.MLTree) then
  begin
    MLTree          := MLTreeAnalyzer.MLTree;
    aNoOfSites := Length(MLTree.Seqs[0]);  { using aNoOfSites here because for the LBS analysis we sub-sample the data but FNoOfSites still needs to match the full data dimension}
  end
  else
    aNoOfSites := Length(MLTreeAnalyzer.Seqs[0]);
  SearchLevel     := MLTreeAnalyzer.SearchLevel;
  SearchFilter      := MLTreeAnalyzer.SearchFilter;
//  deltaL          := MLTree.deltaL;         // deltaL and MaxIter for ML search are independent of MLTree
//  MaxIter         := MLTree.MaxIter;
  MyModel  := CopyModel(MLTreeAnalyzer.Model);
  SwapList := TList.Create;
  setlength(p, SearchLevel*2+4);
  setlength(q, SearchLevel*2+4);
  if MyModel.SeqDataType = protein then
    for i := 0 to length(q)-1 do
      q[i] := TMLTreeNode.Create(i, false, MLTreeAnalyzer.NoOfSeqs, aNoOfSites, MyModel.NoOfRates, 20, aNoOfSites)
  else
    for i := 0 to length(q)-1 do
      q[i] := TMLTreeNode.Create(i, false, MLTreeAnalyzer.NoOfSeqs, aNoOfSites, MyModel.NoOfRates, 4, aNoOfSites);
  Result := True;
  IsInitialized := True;
end;

procedure TMLTreeSearchOnNodeThread.InitMem;
var
  i: integer;
begin
//  cfg := MLTree.Root.c0;

  if SwapList.Count > 0 then
    for i := SwapList.Count-1 downto 0 do
    begin
      TSwapData(SwapList[i]).Free;
      SwapList.Delete(i);
    end;
end;

procedure TMLTreeSearchOnNodeThread.InitSearchTreeExportNodes;
var
  i: Integer;
  n: TMLTreeNode = nil;
begin
  SetLength(FSearchTreeExportNodes, Length(MLTree.Node));
  for i := Low(FSearchTreeExportNodes) to High(FSearchTreeExportNodes) do
  begin
    n := MLTree.Node[i];
    if Assigned(n) then
    begin
      FSearchTreeExportNodes[i] := TMLTreeNode.Create(n.index, n.OTU, n.NoOfSeqs, n.NoOfSites, MYModel.NoOfRates, n.NoOfStates, n.OrigNoOfSites);
      FSearchTreeExportNodes[i].Assign(n);
    end;
  end;
end;

procedure TMLTreeSearchOnNodeThread.UpdateSearchTreeExportNodes;
var
  i: Integer;
  n: TMLTreeNode = nil;
  n2: TMLTreeNode = nil;
begin
  for i := Low(FSearchTreeExportNodes) to High(FSearchTreeExportNodes) do
    if Assigned(FSearchTreeExportNodes[i]) then
    begin
      n := MLTree.Node[i];
      n2 := FSearchTreeExportNodes[i];
      if Assigned(n.anc) then
        n2.anc := FSearchTreeExportNodes[n.anc.index]
      else
        n2.anc := nil;

      if not n.OTU then
      begin
        n2.des1 := FSearchTreeExportNodes[n.des1.index];
        n2.des2 := FSearchTreeExportNodes[n.des2.index];
      end
      else
      begin
        n.des1 := nil;
        n.des2 := nil;
      end;
      n.blen := n2.blen;
    end;

  for i := Low(q) to High(q) do
  begin
    n := q[i];
    n2 := FSearchTreeExportNodes[n.index];
    if Assigned(n.anc) then
      n2.anc := FSearchTreeExportNodes[n.anc.index]
    else
      n2.anc := nil;

    if n.OTU then
    begin
      n2.des1 := nil;
      n2.des2 := nil;
    end
    else
    begin
      n2.des1 := FSearchTreeExportNodes[n.des1.index];
      n2.des2 := FSearchTreeExportNodes[n.des2.index];
    end;
    n2.blen := n.blen;
  end;
end;

procedure TMLTreeSearchOnNodeThread.UpdateSearchTreeExportList(logL: Extended; comment: String);
var
  n: String = '';
begin
  n := SearchTreeExportNewickString;
  SearchTreeNewickStrings.Add(Format('[%-30s: %.2f] %s', [comment, logL, n]));
end;

procedure TMLTreeSearchOnNodeThread.UpdateSearchTreeExportList(logL: Extended; aData: TTreeData; comment: String);
var
  d: TTreeData = nil;
  newick: String = '';
begin
  if not Assigned(SearchTrees.OTUNameList) then
  begin
    SearchTrees.OTUNameList := TStringList.Create;
    SearchTrees.OTUNameList.Assign(OtuNames);
  end;
  if SearchTrees.OtuNameList.Count <> MLTree.NoOfSeqs then
    SearchTrees.OTUNameList.Assign(OtuNames);
  d := TTreeData.Create(aData.NoOfOTUs, True, False, False);
  d.Assign(aData);
  SearchTrees.Add(d);
  newick := SearchTrees.OutputNewickTree(0, True, False, 0);
  SearchTreeNewickStrings.Add(Format('[%-30s: %.2f] %s', [comment, logL, newick]));
  SearchTrees.DeleteAll;
end;

function TMLTreeSearchOnNodeThread.SetInitialSubtree(node: TMLTreeNode): boolean;
begin
  result := false;

  if node.anc = nil then exit;
  if node.anc.anc = nil then exit;

  p[0] := node.anc.anc;
  p[1] := node.anc;
  p[3] := node;
  if node = node.anc.des1 then
    p[2] := node.anc.des2
  else
    p[2] := node.anc.des1;

  q[0].Assign(p[0]);
  q[1].Assign(p[1]);
  q[3].Assign(p[3]);

  if node.anc = node.anc.anc.des1 then
    q[0].des1 := q[1]
  else
    q[0].des2 := q[1];
  q[1].anc := q[0];
  if node = node.anc.des1 then
  begin
    q[1].des1 := q[3];
    q[1].des2 := q[2];
    q[2].Assign(node.anc.des2);
  end
  else
  begin
    q[1].des1 := q[2];
    q[1].des2 := q[3];
    q[2].Assign(node.anc.des1);
  end;
  q[2].anc := q[1];
  q[3].anc := q[1];

  q[1].blen := q[1].blen;
  if node = node.anc.des1 then
    q[2].blen := node.anc.des2.blen
  else
    q[2].blen := node.anc.des1.blen;
  q[3].blen := q[3].blen;

  if q[1] = q[0].des1 then
    q[0].des2.flag := true
  else
    q[0].des1.flag := true;
  q[0].flag := false;
  q[1].flag := false;
  q[2].flag := true;
  q[3].flag := true;

//  L0 := MLTree.LogLikelihood;

  d := 1;
  result := true;
end;

procedure ResetSiteConfig(MLTree: TMLTree);

  procedure ResetSiteConfigUp(n: TMLTreeNode);
  begin
    if n.OTU or n.flag then
      exit;

    ResetSiteConfigUp(n.des1);
    ResetSiteConfigUp(n.des2);

    if not (n.des1.flag and n.des2.flag) then
    begin
      MLTree.ResetSiteConfig1OfNodeFast(n, true);
      MLTree.ComputeL1OfNode(n);
    end;
  end;


  procedure ResetSiteConfigDown(n: TMLTreeNode);
  begin
    MLTree.ResetSiteConfig0OfNodeFast(n);
    MLTree.ComputeL0OfNode(n);

    if not n.des1.OTU then
      ResetSiteConfigDown(n.des1);
    if not n.des2.OTU then
      ResetSiteConfigDown(n.des2);
  end;

begin
  ResetSiteConfigUp(MLTree.root);
  ResetSiteConfigDown(MLTree.root);
end;

procedure AddBranch(r, n, b : TMLTreeNode);
begin
  n.anc := r;
  r.des1 := n;
  r.des2 := b;
  r.anc := b.anc;
  if b = b.anc.des1 then
      b.anc.des1 := r
  else
      b.anc.des2 := r;
  b.anc := r;
end;

procedure RemoveBranch(n : TMLTreeNode);
var
  a, aa, b : TMLTreeNode;
begin
  a := n.anc;
  aa := a.anc;
  if n = a.des1 then
    b := a.des2
  else
    b := a.des1;
  if a = aa.des1 then
    aa.des1 := b
  else
    aa.des2:= b;
  b.anc := aa;
end;

procedure CopyBlen(source, destiny: TMLTreeNode);
var
  i,j,k: integer;
begin
  destiny.blen := source.blen;
  for i := 0 to length(source.Prob)-1 do
    for j := 0 to source.NoOfStates-1 do
      for k := 0 to source.NoOfStates-1 do
        destiny.Prob[i][j,k] := source.Prob[i][j,k];
end;

function compare(Item1, Item2: Pointer): Integer;
begin
  result := 0;
  if TSwapData(Item1).dL > TSwapData(Item2).dL then
    result := -1
  else if TSwapData(Item2).dL > TSwapData(Item1).dL then
    result := 1;
end;

function TMLTreeSearchOnNodeThread.IncSubtreeDown(index: integer): boolean;
begin
  result := false;

  if p[index].OTU then
    exit;

  inc(d);
//  sb := sb +p[index].blen;
//  vb := vb +p[index].ase*p[index].ase;
  CI := p[index].CI;

  p[2*d  ] := p[index].des1;
  p[2*d+1] := p[index].des2;

  q[2*d  ].Assign(q[index].des1);
  q[2*d+1].Assign(q[index].des2);
  q[2*d  ].anc  := q[index];
  q[2*d+1].anc  := q[index];
  q[index].des1 := q[2*d  ];
  q[index].des2 := q[2*d+1];

  q[index].flag := false;
  q[2*d  ].flag := true;
  q[2*d+1].flag := true;

  result := true;
end;

function TMLTreeSearchOnNodeThread.IncSubtreeUp(index: integer): boolean;
begin
  result := false;
  if p[0].anc = nil then
    exit;

  inc(d);
//  sb := sb +p[index].blen;
//  vb := vb +p[index].ase*p[index].ase;
  CI := p[index].CI;

  p[2*d] := p[0];
  p[0]   := p[0].anc;
  if p[index] = p[2*d].des1 then
    p[2*d+1] := p[2*d].des2
  else
    p[2*d+1] := p[2*d].des1;

  q[0    ].Assign(p[0]);
  q[2*d  ].Assign(p[2*d]);
  q[2*d+1].Assign(p[2*d+1]);

  q[2*d].anc := q[0];
  if p[2*d] = p[0].des1 then
    q[0].des1 := q[2*d]
  else
    q[0].des2 := q[2*d];

  q[index].anc := q[2*d];
  q[2*d+1].anc := q[2*d];
  if p[index] = p[2*d].des1 then
  begin
    q[2*d].des1 := q[index];
    q[2*d].des2 := q[2*d+1];
  end
  else
  begin
    q[2*d].des1 := q[2*d+1];
    q[2*d].des2 := q[index];
  end;

  if q[2*d] = q[0].des1 then
    q[0].des2.flag := true
  else
    q[0].des1.flag := true;
  q[index].flag := false;
  q[2*d  ].flag := false;
  q[2*d+1].flag := true;

  result := true;
end;

procedure TMLTreeSearchOnNodeThread.DecSubtreeDown(index: integer);
var
  a: TMLTreeNode;
begin
  a := q[index].anc;
  q[index].Assign(p[index]);
  q[index].anc := a;
  q[index].flag := true;

//  sb := sb -p[index].blen;
//  vb := vb -p[index].ase*p[index].ase;
  CI := p[index].CI;

  dec(d);
end;

procedure TMLTreeSearchOnNodeThread.ResetBLens;
var
  i: integer;
begin
  for i := 1 to 2*d+1 do
//    q[i].blen := p[i].blen;
    CopyBlen(p[i], q[i]);
end;
{
procedure TMLTreeSearchOnNodeThread.SetBLens;
begin
  bl1 := p[1].blen;
  bl3 := p[3].blen;
  bl4 := p[2*d  ].blen;
  bl5 := p[2*d+1].blen;
end;
}

function TMLTreeSearchOnNodeThread.SearchEachNode(node: TMLTreeNode):boolean;

  function InitSubtree(n: TMLTreeNode): extended;

    procedure InitSubtreeUp(n: TMLTreeNode);
    begin
      if n.flag then
        exit;

      InitSubtreeUp(n.des1);
      InitSubtreeUp(n.des2);

      MLTree.ResetSiteConfig1OfNodeFast(n, true);
      MLTree.ComputeL1OfNode(n);
    end;

    procedure InitSubtreeDown(n: TMLTreeNode);
    begin
      MyModel.ComputeProbOfNode(n, n.blen);

      if n.OTU or n.flag then
        exit;

      MLTree.ResetSiteConfig0OfNodeFast(n);
      MLTree.ComputeL0OfNode(n);

      InitSubtreeDown(n.des1);
      InitSubtreeDown(n.des2);
    end;

  begin
    InitSubtreeUp(n);
    InitSubtreeDown(n);

    MLTree.ResetSiteConfig1OfNodeFast(n.anc, true);
    MLTree.ComputeL1OfNode(n.anc);

    result := MLTree.ComputeLogLikelihoodOfNodeWithModel(node.anc, MyModel);
  end;

  procedure IterateSubtreeBLens(n1: TMLTreeNode; var L: extended);

    procedure IterateSubtreeOfNode(n: TMLTreeNode);
    begin
      if not n.flag then
      begin
        MLTree.ComputeL0OfNode(n);

        IterateSubtreeOfNode(n.des1);
        IterateSubtreeOfNode(n.des2);
      end;

       L := MLTree.IterateBlenOfNodeWithModel(n, MyModel, L, 0.01);
    end;

  var
    i: integer;
    L0: extended;
  begin
    i := 0;
    repeat
      L0 := MLTree.LogLikelihood;
      IterateSubtreeOfNode(n1);
      inc(i);
    until (abs(MLTree.LogLikelihood-L0) < deltaL) or (i = MaxIter);

{
    IterateSubtreeOfNode(n1);
    IterateSubtreeOfNode(n1);
    IterateSubtreeOfNode(n1);
}
  end;

  procedure AddSwapData(swapindex: integer; dL: extended; n: integer);
  var
    data: TSwapData;
    i: integer;
  begin
    for i := SwapList.Count-1 downto 0 do
      if TSwapData(SwapList[i]).node[2] = q[2].index then
        if  TSwapData(SwapList[i]).dL < dL then
        begin
          TSwapData(SwapList[i]).Free;
          SwapList.Delete(i);
        end
        else
          exit;

    data := TSwapData.Create(n);
    data.swapindex := swapindex;
    data.dL        := dL;

    for i := 0 to n do
      data.node[i] := q[i].index;
    for i := 1 to n do
      data.blen[i] := q[i].blen;
    for i := 1 to n do
      data.ase[i]  := q[i].ase;

    SwapList.Add(data);
  end;

  function SPR: boolean;
  var
    L, L1, L2, dL: extended;
    n: TMLTreeNode;
    i: integer;
  begin
    Result := true;

    L1 := MLTree.LogLikelihood;

    RemoveBranch(q[2]);
    AddBranch(q[1], q[2], q[2*d  ]);
    CopyBlen(p[1], q[3]);
    CopyBlen(p[3], q[1]);

    if q[0].des1.flag then
      n := q[0].des2
    else
      n := q[0].des1;

    L := InitSubtree(n);
    i := 0;
    repeat
      L2 := L;
      IterateSubtreeBLens(n, L);

      inc(i);

      if (L1-L) > (L-L2)*(MaxIter -i) then
        break;
    until (abs(L-L2) < deltaL) or (i = MaxIter);

    FTempLogLikelihood := L;

    dL := L-L1;
    if dL > 0.000000001 then
    begin
      AddSwapData(1, dL, 2*d+1);
      result := false;
      if FDoExportSearchTrees then
        UpdateSearchTreeExportList(FTempLogLikelihood, Format('swap branch - keep %.3e', [L-L1]));
    end
    else if FDoExportSearchTrees then
      UpdateSearchTreeExportList(FTempLogLikelihood, 'swap branch - discard');


    RemoveBranch(q[2]);
    AddBranch(q[1], q[2], q[2*d+1]);
    CopyBlen(p[1], q[3]);
    CopyBlen(p[3], q[1]);
    CopyBlen(p[2*d  ], q[2*d  ]);
    CopyBlen(p[2*d+1], q[2*d+1]);

    if q[0].des1.flag then
      n := q[0].des2
    else
      n := q[0].des1;

    L := InitSubtree(n);
    i := 0;
    repeat
      L2 := L;
      IterateSubtreeBLens(n, L);

      inc(i);

      if (L1-L) > (L-L2)*(MaxIter -i) then
        break;
    until (abs(L-L2) < deltaL) or (i = MaxIter);

    FTempLogLikelihood := L;

    dL := L-L1;
    if dL > 0.000000001 then
    begin
      AddSwapData(2, dL, 2*d+1);
      result := false;
      if FDoExportSearchTrees then
        UpdateSearchTreeExportList(FTempLogLikelihood, Format('swap branch - keep %.3e', [L-L1]));
    end
    else if FDoExportSearchTrees then
      UpdateSearchTreeExportList(FTempLogLikelihood, 'swap branch - discard');

    RemoveBranch(q[2]);
    AddBranch(q[1], q[2], q[3]);

    ResetBLens;
    L := InitSubtree(n);
  end;

  function SPRRecursiveDown: boolean;
  var
    flag: boolean;
  begin
    result := true;
    if d > SearchLevel then exit;

    if CI < SearchFilter then
    begin
      if IncSubtreeDown(2*d) then
      begin
        flag   := SPR;
        result := result and flag;
        flag   := SPRRecursiveDown;
        result := result and flag;
        DecSubtreeDown(2*(d-1));
      end;
    end;

    if CI < SearchFilter then
    begin
      if IncSubtreeDown(2*d+1) then
      begin
        flag   := SPR;
        result := result and flag;
        flag   := SPRRecursiveDown;
        result := result and flag;
        DecSubtreeDown(2*(d-1)+1);
      end;
    end;
  end;

  function SPRRecursiveUp: boolean;
  var
    flag: boolean;
  begin
    result := true;
    if d > SearchLevel then exit;

    if d > 2 then
    begin
      flag   := SPR;
      result := result and flag;
    end;

    if CI < SearchFilter then
    begin
      if IncSubtreeDown(2*d+1) then
      begin
        flag   := SPR;
        result := result and flag;
        flag   := SPRRecursiveDown;
        result := result and flag;
        DecSubtreeDown(2*(d-1)+1);
      end;
    end;

    if CI < SearchFilter then
    begin
      if IncSubtreeUp(2*d) then
      begin
        flag   := SPRRecursiveUp;
        result := result and flag;
      end;
    end;

  end;

var
  flag: boolean;
begin
  result := true;

  if not SetInitialSubtree(node) then
    exit;
//  sb := q[3].blen;
//  vb := q[3].ase*q[3].ase;
  CI := q[3].CI;

  if CI < SearchFilter then
  begin
    if IncSubtreeDown(3) then
    begin
      flag   := SPR;
      result := result and flag;
      flag   := SPRRecursiveDown;
      result := result and flag;
      DecSubtreeDown(3);
    end;
  end;

//  sb := q[1].blen;
//  vb := q[1].ase*q[1].ase;
  CI := q[1].CI;

  if CI < SearchFilter then
  begin
    if IncSubtreeUp(1) then
    begin
      flag   := SPRRecursiveUp;
      result := result and flag;
    end;
  end;
  node.done := result;
end;

procedure TMLTreeSearchOnNodeThread.Search;
var
  n: TMLTreeNode;
begin
  if not Initialize then
    raise Exception.Create('Failed to initialize MLTreeSearchOnNodeThread');
  InitMem;
  if FDoExportSearchTrees then
    InitSearchTreeExportNodes;
  repeat
    n := MainThread.GetNodeForSearch;
    if n <> nil then
      SearchEachNode(n);

    if ThreadIndex = 0 then
    begin
      if MainThread.CheckCancel(-1, 'Searching ML tree') then
      begin
        Terminate;
        break;
      end;
      SubTaskCheckCancelFunc(MainThread.GetProgress, 'Searching...');
    end;
  until n = nil;
end;

procedure TMLTreeSearchOnNodeThread.Execute;
begin
  try
    repeat
      Search;
      while not Done do
        Done := true;
      while (not terminated) and Done do
        ThreadSwitch;
    until Terminated;
  except
    on E:EAbort do
    begin
      Done := True;
      FIsSuccess := False;
      Canceled := True;
      MessagesLog.Add(E.Message);
      Terminate;
    end;
    on E:Exception do
    begin
      Done := True;
      FIsSuccess := False;
      MessagesLog.Add(E.Message);
      {$IFDEF DEBUG}
      MessagesLog.Add(ExceptionCallStack(E));
      {$ENDIF}
      while not terminated do
        ThreadSwitch;
    end;
  end;
end;

///////////////////

constructor TMLTreeSearchThread.Create(MLA: TMLTreeAnalyzer);
begin
  inherited create(self, 0, MLA);
  FCanComputeDataCoverage := True;
  FAnalysisInfo := nil;
  FTempModel := nil;
  IsBootstrapReplicate := False;
  ThreadLock := TCriticalSection.Create;
  MasterSwapList := TList.Create;
  if Assigned(MLA) then { in the case of bootstrap, MLTreeAnalyzer is already created}
  begin
    IsInitialized := True
  end
  else
    IsInitialized := False;
  deltaL  := 1.0E-2;
  MaxIter := 5;
end;

destructor TMLTreeSearchThread.Destroy;
var
  i: integer;
begin
  FAnalysisInfo := nil; { freed downstream after it is finished being used}
  ThreadLock.Free;
  //for i := Low(ChildThread) to High(ChildThread) do
  //  if Assigned(ChildThread[i]) then
  //    ChildThread[i].Free;
  SetLength(ChildThread, 0);
  SetLength(NodeStarted, 0);
{
  for i := 0 to  NoOfThreads-1 do
  begin
    for j := 0 to SwapLists[i].Count-1 do
      TSwapData(SwapLists[i][j]).Free;
    FreeAndNil(SwapLists[i]);
  end;
}
  if Assigned(MasterSwapList) then
  begin
    if MasterSwapList.Count > 0 then
      for i := 0 to MasterSwapList.Count-1 do
        TSwapData(MasterSwapList[i]).Free;
    FreeAndNil(MasterSwapList);
  end;

  if MLTreeAnalyzer <> nil then
    MLTreeAnalyzer.CheckCancel := MLACheckCancel;

  inherited;
end;

function TMLTreeSearchThread.GetIsInitialTree: boolean;
begin
  result := MLTreeAnalyzer.IsInitialTree;
end;

function TMLTreeSearchThread.GetProgress: integer;
var
  i,n: integer;
begin
  n := 0;
  with MLTree do
  begin
    for i := 0 to 2*NoOfSeqs-2 do
      if (Node[i] <> Root) and Node[i].done then
        inc(n);
    result := trunc(n/(2*NoOfSeqs-3)*100);
  end;
end;

function TMLTreeSearchThread.DumpStartingTreesData(startingTrees: TTreeList; filename: String): Boolean;
var
  aFile: TextFile;
  i: Integer = -1;
  aTree: TMPMLTreeData = nil;
begin
  try
    AssignFile(aFile, filename);
    Rewrite(aFile);
    WriteLn(aFile, 'TreeLength,StartingTreeLnl,FinalTreeLnl,FinalTreeSBL');
    if startingTrees.Count > 0 then
      for i := 0 to startingTrees.Count - 1 do
      begin
        aTree := TMPMLTreeData(startingTrees[i]);
        WriteLn(aFile, Format('%d,%.8f,%.8f,%.8f', [aTree.TreeLength, aTree.InitialLogLikelihood, aTree.LogLikelihood, aTree.SBL]));
      end;
  finally
    CloseFile(aFile);
  end;
  Result := FileExists(filename);
end;

procedure TMLTreeSearchThread.ReInitialize;
begin

  IsInitialized := False;
  Initialize;
end;

procedure TMLTreeSearchThread.UpdateMessagesLog(aStr: String);
begin
  MessagesLog.Add(Format('%-20s %s', [FormatDateTime('YYYY-MM-DD hh:mm:ss.zzz', Now), aStr]));
end;

function TMLTreeSearchThread.GenerateStartingNJTree: TMPMLTreeData;
var
  aMLTree: TMLTree = nil;
  aTreeAnalyzer: TMLTreeAnalyzer = nil;

  procedure ExportNJStartingTree(filename: String);
  var
    aList: TTreeList = nil;
    aData: TTreeData = nil;
  begin
    try
      aList := TTreeList.Create;
      aList.OTUNameList.Assign(FAnalysisInfo.MyOtuNames);
      aData := Result.Clone;
      aList.Add(aData);
      aList.ExportATreeToNewickFile(0, filename, True, False, 0.0, Format('[%.8f]', [Result.InitialLogLikelihood]));
    finally
      if Assigned(aList) then
        aList.Free;
    end;
  end;

begin
  try
    UpdateRunStatusInfo('Status', 'Making initial NJ tree');
    Result := TMPMLTreeData.Create(FAnalysisInfo.NoOfSeqs);
    aTreeAnalyzer := TMLTreeAnalyzer.Create(FAnalysisInfo.MySeqStrings, nil, FTempModel);
    aTreeAnalyzer.InitialTreeOption := NJInitTreeMethod;
    aTreeAnalyzer.MakeInitialTree(NJInitTreeMethod);
    Result.Assign(aTreeAnalyzer.InitTree);
    Result.InitialLogLikelihood := aTreeAnalyzer.LogLikelihood;
    aMLTree := TMLTree.Create(aTreeAnalyzer.SeqData, Result, FTempModel);
    aMLTree.InitNodeData(Result);
    aMLTree.Initialize(False, true);
    aMLTree.OptimizeAllBlens(nil);
    Result.InitialLogLikelihood := aMLTree.LogLikelihood;
    Result.SumOfBranchLengths := Result.SBL;
    aTreeAnalyzer.SeqData := nil;
    Assert(CompareValue(Result.InitialLogLikelihood, 0.0, FP_CUTOFF) < 0, Format('invalid LnL value %.4n', [Result.InitialLogLikelihood]));
    {$IFNDEF VISUAL_BUILD}
    ExportNJStartingTree(NextAvailableFilenameNV('_NJ_start_tree.nwk'));
    {$ENDIF}
  finally
    if Assigned(aTreeAnalyzer) then
    begin
      aTreeAnalyzer.Model := nil;
      aTreeAnalyzer.Free;
    end;
    if Assigned(aMLTree) then
      aMLTree.Free;
  end;
end;

function TMLTreeSearchThread.GenerateParsimonyStartingTrees(var aMpTreeList: TTreeList; const minLnL: Double; var numTreesSearched: Integer): String;
var
  aMsg: String = '';
  aAnalyzer: TMLTreeAnalyzer = nil;
  exportOptions: TNewickExportOptions;
  allParsimTrees: TStringList = nil;
begin
  try
    UpdateRunStatusInfo('Status', 'Making initial parsimony trees');
    aAnalyzer := TMLTreeAnalyzer.Create(FAnalysisInfo.MySeqStrings, nil, FTempModel);
    aAnalyzer.SubTaskCheckCancel := SubTaskCheckCancelFunc;
    aAnalyzer.RunStatusProc := UpdateRunStatusInfo;
    aAnalyzer.RemoveStatusProc := RemoveRunStatusInfo;
    Result := aAnalyzer.MakeParsimonyTrees(FAnalysisInfo.NumStartingMpTrees, aMpTreeList, MessagesLog);
    if aMsg <> EmptyStr then
      UpdateMessagesLog(aMsg);
    aMpTreeList.Sort(CompareTreeLenAndInitLnl);
    if aMpTreeList.Count > FAnalysisInfo.NumStartingMpTrees then
    begin
      while aMpTreeList.Count > FAnalysisInfo.NumStartingMpTrees do
        aMpTreeList.Delete(aMpTreeList.Count - 1);
    end;
    {$IFNDEF VISUAL_BUILD}
    exportOptions := GetDefaultNewickExportOptions(aMpTreeList);
    allParsimTrees := aMpTreeList.OutputAllNewickTrees(exportOptions, 0.0, [tdvTreeLength, tdvInitialLogLikelihood]);
    allParsimTrees.SaveToFile(NextAvailableFilenameNV('_MP_start_trees.nwk'));
    {$ENDIF}
    aAnalyzer.SeqData := nil;
    aAnalyzer.Model := nil;
  finally
    if Assigned(aAnalyzer) then
      aAnalyzer.Free;
    if Assigned(allParsimTrees) then
      allParsimTrees.Free;
  end;
end;

function TMLTreeSearchThread.Initialize: Boolean;
var
  AModel: TGammaRateVariationModel = nil;
  ATree: TTreeData = nil;
begin
  Result := False;
  if not IsInitialized then
  begin
    if not Assigned(FAnalysisInfo) then
      FAnalysisInfo := FProgressDlg.FMAI;
    AModel := CreateSubstitutionModel(FAnalysisInfo, SubTaskCheckCancelFunc);
    if not Assigned(FAnalysisInfo.MyOriTreeList) then
      FAnalysisInfo.MyOriTreeList := TTreeList.Create;
    if not Assigned(MLTreeAnalyzer) then
    begin
      if FAnalysisInfo.MyTreePack.DoesContain(ttUserTree) then
      begin
        FAnalysisInfo.MyOriTreeList.ImportFromNewick(FAnalysisInfo.MyUserNewickTree, FAnalysisInfo.MyOtuNames, False);
        ATree := TTreeData.Create(FAnalysisInfo.NoOfSeqs,false,false,false);
        ATree.Assign(FAnalysisInfo.MyOriTreeList[0]);
        MLTreeAnalyzer := TMLTreeAnalyzer.Create(FAnalysisInfo.MySeqStrings, ATree, AModel);  //  to specify initial tree
        AModel := nil;
      end
      else
      begin
        UpdateRunStatusInfo('Status', 'Making initial tree');
        MLTreeAnalyzer := TMLTreeAnalyzer.Create(FAnalysisInfo.MySeqStrings, nil, AModel);  //  to force making initial tree
        AModel := nil;
      end;
    end;
    MLTreeAnalyzer.NoOfThreadsToUse := FAnalysisInfo.MyNumThreadsToUse;
    if FAnalysisInfo.IsLargeDataForML then
      MLTreeAnalyzer.SubTaskCheckCancel := SubTaskCheckCancelFunc
    else
      MLTreeAnalyzer.SubTaskCheckCancel := nil;
    if FAnalysisInfo.MLSearchFilter > -1 then
      MLTreeAnalyzer.SearchFilter := FAnalysisInfo.MLSearchFilter;

    FAnalysisInfo.MyMLAnalysisPack := MLTreeAnalyzer;

  {
    if FAnalysisInfo.MyTreePack.DoesContain(ttCNI) then
      FAnalysisInfo.MyMLAnalysisPack.SearchLevel := 2
    else
      FAnalysisInfo.MyMLAnalysisPack.SearchLevel := 1;
    FAnalysisInfo.MyMLAnalysisPack.SearchMLTree(true);       // Please do not erase this comments. It's non-thread version for debuging. KT.
    ShowMLTree(FAnalysisInfo);
  }

    if FAnalysisInfo.MyTreePack.DoesContain(ttSPRFast) then
      MLTreeAnalyzer.SearchLevel := 3 // this will be the fast search option
    else if FAnalysisInfo.MyTreePack.DoesContain(ttSPRExtensive) then // this will be the extensive search option
      MLTreeAnalyzer.SearchLevel := 5
    else
      MLTreeAnalyzer.SearchLevel := 1;     // 1: NNI; >2: SPR (5: same to RAxML)

    MLTreeAnalyzer.SeqNames := FAnalysisInfo.MyOtuNames;
    MLTreeAnalyzer.InitialTreeOption := FAnalysisInfo.MyInitialTreeMethod;   //0: NJ; 1: BIONJ; 2: ME; 3: MP; 4: NJ/BIONJ(default)
    FAnalysisInfo.MyTreePack.MLInitialTreesMethod := FAnalysisInfo.MyInitialTreeMethod; // so that citation expert can easily get the value
//    if FProgressDlg.IsMarqueeMode then
//      FProgressDlg.IsMarqueeMode := False;
  end;
  if not IsInitialized then
    inherited Initialize; { can't call this until MLTreeAnalyzer is created}

  MLACheckCancel := MLTreeAnalyzer.CheckCancel;
  MLTreeAnalyzer.CheckCancel := CheckCancel;
  FProgressDlg.RegisterCancellable(MLTreeAnalyzer);
  if FDoExportSearchTrees then
    MLTreeAnalyzer.NoOfThreadsToUse := 1;
  NoOfThreads := MLTreeAnalyzer.NoOfThreadsToUse;
  SetLength(NodeStarted, 2*MLTreeAnalyzer.NoOfSeqs-1);
  SetLength(ChildThread, NoOfThreads);
  ChildThread[0] := self;
  MakeInitialTree := (not IsInitialTree);
  FNoOfTreesExamined := 0;
  if Assigned(AModel) then
    AModel.Free;
  Result := True;
end;

function TMLTreeSearchThread.InitializeForMultiStartTreeSearch(aStartingTree: TMPMLTreeData): Boolean;
begin
  Result := False;
  if Assigned(MLTreeAnalyzer) then
  begin
    MLTreeAnalyzer.SeqData := nil;
    MLTreeAnalyzer.Model := nil;
    FreeAndNil(MLTreeAnalyzer);
  end;

  if FAnalysisInfo.MyTreePack.DoesContain(ttUserTree) then
    raise Exception.Create('user tree cannot be used for multi-starting tree search')
  else
  begin
    MLTreeAnalyzer := TMLTreeAnalyzer.Create(FAnalysisInfo.MySeqStrings, aStartingTree, FTempModel);
  end;
  MLTreeAnalyzer.NoOfThreadsToUse := FAnalysisInfo.MyNumThreadsToUse;
  if FAnalysisInfo.IsLargeDataForML then
    MLTreeAnalyzer.SubTaskCheckCancel := SubTaskCheckCancelFunc
  else
    MLTreeAnalyzer.SubTaskCheckCancel := nil;
  if FAnalysisInfo.MLSearchFilter > -1 then
    MLTreeAnalyzer.SearchFilter := FAnalysisInfo.MLSearchFilter;

  FAnalysisInfo.MyMLAnalysisPack := MLTreeAnalyzer;

  if FAnalysisInfo.MyTreePack.DoesContain(ttSPRFast) then
    MLTreeAnalyzer.SearchLevel := 3
  else if FAnalysisInfo.MyTreePack.DoesContain(ttSPRExtensive) then
    MLTreeAnalyzer.SearchLevel := 5
  else
    MLTreeAnalyzer.SearchLevel := 1;     // 1: NNI; >2: SPR (5: same to RAxML)

  MLTreeAnalyzer.SeqNames := FAnalysisInfo.MyOtuNames;
  MLTreeAnalyzer.InitialTreeOption := DefaultInitTreeMethod;
  FAnalysisInfo.MyTreePack.MLInitialTreesMethod := MultipleMPTreesMethod; // so that citation expert can easily get the value


  if not IsInitialized then
    inherited Initialize; { can't call this until MLTreeAnalyzer is created}

  MLACheckCancel := MLTreeAnalyzer.CheckCancel;
  MLTreeAnalyzer.CheckCancel := CheckCancel;
  FProgressDlg.RegisterCancellable(MLTreeAnalyzer);
  NoOfThreads := MLTreeAnalyzer.NoOfThreadsToUse;
  SetLength(NodeStarted, 2*MLTreeAnalyzer.NoOfSeqs-1);
  SetLength(ChildThread, NoOfThreads);
  ChildThread[0] := Self;
  MakeInitialTree := True;
  FNoOfTreesExamined := 0;
  Result := True;
end;

function TMLTreeSearchThread.CheckCancel(Progress: integer; Status: AnsiString): boolean;
begin
  Result := False;
  if Progress >= 0 then
    FProgress := Progress;
  Assert(Trim(Status) <> EmptyStr, 'empty status string');
  FStatus   := Status;

  if IsBootstrapReplicate then
  begin
    SubTaskCheckCancelFunc(Progress, Status);
    Result := False;
  end
  else
  begin
    if Terminated then
      Canceled := True;

    if (FProgressDlg <> nil) {$IFDEF VISUAL_BUILD}and (FProgressDlg.PercentGauge <> nil){$ENDIF} then
      OnProgress(FProgress);
    result := Terminated;
  end;
end;


function TMLTreeSearchThread.GetNodeForSearch: TMLTreeNode;
var
  i: integer;
begin
  result := nil;

  ThreadLock.Acquire;
  try
    for i := 0 to 2*MLTree.NoOfSeqs-2 do
      if not NodeStarted[i] then
      begin
        NodeStarted[i] := true;
        result := MLTree.Node[i];
        break;
      end;
  finally
    ThreadLock.Release;
  end;
end;

function TMLTreeSearchThread.CheckChildThreadDone: boolean;
var
  i: integer;
begin
  result := true;
  for i := 1 to NoOfThreads-1 do
    result := result and ChildThread[i].Done;
end;

function TMLTreeSearchThread.CheckChildThreadFailed: Boolean;
var
  i: Integer;
begin
  Result := False;
  if NoOfThreads > 1 then
    for i := 1 to NoOfThreads - 1 do
      if not ChildThread[i].IsSuccess then
        Exit(True);
end;

procedure TMLTreeSearchThread.RestartChildThreads;

  procedure ResetFlags;
  var
    i: integer;
  begin
    for i := 0 to 2*MLTree.NoOfSeqs-2 do
      NodeStarted[i] := false;
    NodeStarted[MLTree.Root.index] := true;
    NodeStarted[MLTree.Root.des1.index] := true;
    for i := 1 to MLTree.NoOfThreads-1 do
      ChildThread[i].Done := false;
  end;

var
  i: integer;
begin
  ResetFlags;
  if not ChildStarted then
  begin
    for i := 1 to MLTree.NoOfThreads-1 do
      ChildThread[i].Start;
    ChildStarted := true;
  end;
end;

procedure TMLTreeSearchThread.Search;
var
  devModelInfo: TModelInfo = nil;

  procedure ResetLikelihood(node: TMLTreeNode);

    procedure InitL1OfNode(n: TMLTreeNode);
    begin
      if n.OTU or n.flag then
        exit;

      InitL1OfNode(n.des1);
      InitL1OfNode(n.des2);

      if not (n.des1.flag and n.des2.flag) then
        MLTree.ComputeL1OfNode(n);
    end;

  begin
    InitL1OfNode(node);
    MLTree.ResetL0;
  end;

  procedure SearchMLTreeMultiThread;

    procedure SetFlag(index: integer);
    var
      p: TMLTreeNode;
    begin
      p := MLTree.Node[TSwapData(MasterSwapList[index]).node[0]];
      while (p <> nil) and p.flag do
      begin
        p.flag := false;
        p := p.anc;
      end;
    end;

    procedure ClearMasterSwapList;
    var
      i: integer;
    begin
      i := MasterSwapList.Count;
      while i > 0 do
      begin
        TSwapData(MasterSwapList[i-1]).Free;
        MasterSwapList.Delete(i-1);
        dec(i);
      end;
    end;

    function SwapBranches: integer;
    var
      tmpTree: TTreeData;

      function SwapEachBranch(index: integer): boolean;
      var
        p: array of TMLTreeNode;
        i,n: integer;
      begin
        n := TSwapData(MasterSwapList[index]).NoOfNodes;
        setlength(p, n+1);
        for i := 0 to n do
          p[i] := MLTree.Node[TSwapData(MasterSwapList[index]).node[i]];

        result := true;
        for i := 1 to n do
          if not p[i].flag then
          begin
            result := false;
            break;
          end;
        if not result then exit;

        case TSwapData(MasterSwapList[index]).swapindex of
          1: begin
               RemoveBranch(p[2]);
               AddBranch(p[1], p[2], p[n-1]);
             end;
          2: begin
               RemoveBranch(p[2]);
               AddBranch(p[1], p[2], p[n]);
             end;
        end;

        for i := 1 to n do
        begin
          p[i].blen := TSwapData(MasterSwapList[index]).blen[i];
          p[i].ase  := TSwapData(MasterSwapList[index]).ase[i];
          MLTree.ResetProbOfNode(p[i]);
          p[i].flag := false;
//          p[i].done := false;
        end;
      end;

    var
      L0: extended;
      i: integer;
    begin
      tmpTree := TTreeData.Create(MLTree.NoOfSeqs, true, true, false);
      MLTree.GetTreeData(tmpTree);
      L0 := MLTree.LogLikelihood;

      for i := 0 to 2*MLTree.NoOfSeqs-2 do
        MLTree.Node[i].flag := true;

      result := 0;

      for i := 0 to MasterSwapList.Count-1 do
      begin
        TSwapData(MasterSwapList[i]).flag := SwapEachBranch(i);

        if TSwapData(MasterSwapList[i]).flag then
          inc(result);
      end;

      for i := 0 to MasterSwapList.Count-1 do
        if TSwapData(MasterSwapList[i]).flag then
          SetFlag(i);

      ResetSiteConfig(MLTree);
      ResetLikelihood(MLTree.Root);

      MLTree.LogLikelihood := MLTree.ComputeLogLikelihood;

      if MLTree.LogLikelihood < L0 then
      begin
        MLTree.InitNodeData(tmpTree);
        MLTree.Initialize(false, true);
        for i := 0 to 2*MLTree.NoOfSeqs-2 do
          MLTree.Node[i].flag := true;
        for i := 0 to MasterSwapList.Count-1 do
          TSwapData(MasterSwapList[i]).flag := true;

//        SwapEachBranch(0);
//        SetFlag(0);
        ResetSiteConfig(MLTree);
        ResetLikelihood(MLTree.Root);

        MLTree.LogLikelihood := MLTree.ComputeLogLikelihood;
        result := 0;
      end;

      MLTreeAnalyzer.ResetSearchFilter;

      ClearMasterSwapList;
      if DoExportSearchTrees then
        UpdateSearchTreeExportList(MLTree.LogLikelihood, tmpTree, '*** reset tree ***');
      tmpTree.Free;
    end;

    function ResetMLTree: integer;
    var
      tmpTree: TTreeData;
      n,i,j: integer;
    begin
      result := 0;
      ClearMasterSwapList;
      for i := 0 to NoOfThreads-1 do
      begin
        n := ChildThread[i].SwapList.Count;
        for j := n-1 downto 0 do
        begin
          MasterSwapList.Add(ChildThread[i].SwapList[j]);
          ChildThread[i].SwapList.Delete(j);
        end;
      end;

//      for i := 0 to 2*MLTree.NoOfSeqs-2 do
//        MLTree.Node[i].done := true;

      n := MasterSwapList.Count;
      if n > 0 then
      begin
        MasterSwapList.Sort(compare);
        result := SwapBranches;
      end;
    end; { end ResetMLTree}

  var
    i,it,n: integer;
    dL: extended;
    {$IFNDEF VISUAL_BUILD}
    exportTreesFile: String;
    tmpTree: TTreeData = nil;
    {$ENDIF}
  begin  // begin SearchMLTreeMultiThread
    ReturnValue := 1;

    for i := 1 to MLTreeAnalyzer.NoOfThreadsToUse-1 do
      ChildThread[i] := nil;

    FProgress := 40;
    if CheckCancel(FProgress, 'Preparing ML analysis...') then
    begin
      Canceled := True;
      exit;
    end;
    MLTree := MLTreeAnalyzer.MLTree;
    for i := 0 to 2*MLTree.NoOfSeqs-2 do
      MLTree.Node[i].flag := true;
    MyModel.Assign(MLTree.Model);

    for i := 1 to MLTreeAnalyzer.NoOfThreadsToUse-1 do
    begin
      ChildThread[i] := TMLTreeSearchOnNodeThread.Create(self, i, MLTreeAnalyzer);
      ChildThread[i].deltaL  := deltaL;
      ChildThread[i].MaxIter := MaxIter;
    end;

    {$IFNDEF VISUAL_BUILD}
    if DoExportSearchTrees then
    begin
      for i := 1 to MLTreeAnalyzer.NoOfThreadsToUse - 1 do
        ChildThread[i].OtuNames := OtuNames;
      tmpTree := TTreeData.Create(MLTree.NoOfSeqs, True, False, False);
      MLTree.GetTreeData(tmpTree);
      UpdateSearchTreeExportList(MLTree.LogLikelihood, tmpTree, 'initial tree');
      tmpTree.Free;
    end;
    {$ENDIF}

    try
      FProgress := 40;
      if CheckCancel(FProgress, 'Searching ML tree') then
      begin
        Canceled := True;
        exit;
      end;
      it := 0;
      repeat
        if FCanceled then
          break;
        dL := MLTree.LogLikelihood;

        MLTree.ResetL0;

        RestartChildThreads;

        inherited Search;

        while (not Terminated) and (not CheckChildThreadDone) do
          ThreadSwitch;

         if Terminated then
         begin
           Canceled := True;
           break;
         end;
        if CheckChildThreadFailed then
          n := 0
        else
          n := ResetMLTree;

        inc(it);
        FProgress := round(it/MaxIter*50)+40;
        CheckCancel(FProgress, 'Searching ML tree');
        if Assigned(MLTree.SubtaskCheckCancel) then
          MLTree.SubtaskCheckCancel(Round(it/MaxIter*100), 'Searching...');
        {$IFDEF DEBUG}SaveMLlog(FloatToStr(MLTree.LogLikelihood)+' Search cycle: '+IntToStr(it));{$ENDIF}
      until Terminated or (n = 0) or (abs(MLTree.LogLikelihood-dL) < DeltaL) or (it = MaxIter);

      if Terminated or CheckChildThreadFailed then
        ThreadSwitch
      else
      begin //      if not Terminated then
{
        FProgress := 90;
        CheckCancel(FProgress, 'Optimizing ML tree');

        MLTree.MinProgress := 90;
        MLTree.MaxProgress := 100;

        if MLTree.Model.BootTable = nil then
          if it > 1 then
            MLTree.Optimize(CheckCancel, OptimizeMsg);

        MLTree.ResetL0;
        MLTree.Root.des1.ase := MLTree.Root.des2.ase;

        FProgress := 100;
        CheckCancel(FProgress, 'Optimizing ML tree');
}
        ReturnValue := 0;
      end;
    finally
      for i := Low(ChildThread) to High(ChildThread) do
        if not ChildThread[i].IsSuccess then
        begin
          FIsSuccess := False;
          MessagesLog.Add(ChildThread[i].LogText);
        end;
      {$IFNDEF VISUAL_BUILD}
      if FDoExportSearchTrees then
      begin
        exportTreesFile := NextAvailableFilenameNV('_search_trees.nwk');
        ExportTreesSearched(exportTreesFile);
        if MLTree.NoOfThreads > 1 then
          for i := 1 to MLTree.NoOfThreads - 1 do
            ChildThread[i].ExportTreesSearched(exportTreesFile);
      end;
      {$ENDIF}
      if ChildStarted then
      begin
        for i := 1 to MLTree.NoOfThreads-1 do
          ChildThread[i].Terminate;
      end
      else
      begin
        for i := 1 to NoOfThreads-1 do
          if ChildThread[i] <> nil then
            FreeAndNil(ChildThread[i]);
      end;

      if ReturnValue <> 0 then
        Canceled := true;
    end;
  end;

begin
  {$IFDEF DEBUG}SaveMLlog(' MLTreeSearchThread.Search started');{$ENDIF}
  try
    try
      FLastUpdateTime := Time;
      ChildStarted := false;
//      if not Initialize then
//        raise Exception.Create('Initialization of main ML search thread failed');
      SearchMLTreeMultiThread;
      if (CompareValue(MLTreeAnalyzer.MLTree.LogLikelihood, 0.0, FP_CUTOFF) >= 0) then
        raise Exception.Create(Format('APPLICATION ERROR: Invalid log likelihood value: %.3e', [MLTreeAnalyzer.MLTree.LogLikelihood]));
    except
      on E:EAbort do
      begin
        Canceled := True;
        FIsSuccess := False;
        MessagesLog.Add(E.Message);
      end;
      on E:Exception do
      begin
        FIsSuccess := False;
        MessagesLog.Add(E.Message);
      end;
    end;
  finally
    MLTreeAnalyzer.SubTaskCheckCancel := nil;
    if IsDeveloper and Assigned(devModelInfo) then
      devModelInfo.Free;
  end;
  {$IFDEF DEBUG}SaveMLlog(' MLTreeSearchThread.Search ended');{$ENDIF}
end;

procedure TMLTreeSearchThread.ExecuteMultiStartTreeSearch;
const
  STEP_STR = 'Step';
var
  bestInitialML: Double = -1;
  numTreesSearched: Integer = 0;
  aModelInfo: TModelInfo = nil;
  aMsg: String = '';
  i: Integer = -1;
  aFile: TextFile;
  njTree: TMPMLTreeData = nil;
  mpTree: TMPMLTreeData = nil;
  bestTree: TMPMLTreeData = nil;
  mpTreeList: TTreeList = nil;
  njLnL: Double = 0;
  mpLnL: Double = 0;
  bestTreeIsMp: Boolean = False;
  searchResults: TTreeList = nil;
  aResult: TMPMLTreeData = nil;

begin
  try
    try
      StartExecute;
      CheckCancel(0, 'Initializing');
      searchResults := TTreeList.Create;
      searchResults.OTUNameList.Assign(FAnalysisInfo.MyOtuNames);
      UpdateMessagesLog('Executing ML tree search with multiple starting trees');
      { generate initial trees}
      FTempModel := CreateSubstitutionModel(FAnalysisInfo, SubTaskCheckCancelFunc);
      mpTreeList := TTreeList.Create;
      mpTreeList.OTUNameList.Assign(FAnalysisInfo.MyOtuNames);
      njTree := GenerateStartingNJTree;
      IsInitialized := False;
      UpdateRunStatusInfo('Status', 'Generating initial MP trees');
      aMsg := GenerateParsimonyStartingTrees(mpTreeList, njTree.LogLikelihood, numTreesSearched);
      UpdateMessagesLog(aMsg);
      UpdateMessagesLog(Format('Initial NJ tree has LnL=%.4n', [njTree.InitialLogLikelihood]));
      UpdateMessagesLog(Format('Generated %d MP initial search trees', [mpTreeList.Count]));

      { do search with the NJ tree as the starting tree}
      UpdateMessagesLog('Performing search with NJ tree as starting tree');
      UpdateRunStatusInfo('Status', 'Searching NJ tree');
      if Assigned(MLTreeAnalyzer) then
        MLTreeAnalyzer.InitTree := nil;
      UpdateRunStatusInfo(STEP_STR, 'NJ tree');
      InitializeForMultiStartTreeSearch(njTree);
      OptimizeMsg := 'Optimizing initial NJ tree';
      njLnL := SearchMultiStartTree(TTreeData(njTree));
      njTree.LogLikelihood := njLnL;

      { set NJ tree as the initial best tree}
      bestTree := TMPMLTreeData.Create(njTree.NoOfOTUs);
      bestTree.Assign(njTree);
      aResult := TMPMLTreeData.Create(njTree.NoOfOTUs);
      aResult.Assign(njTree);
      searchResults.Add(aResult);
      {$IFNDEF VISUAL_BUILD}
      searchResults.ExportToNewickFile(NextAvailableFilenameNV('_NJ_finish_tree.nwk'), True, False, 0.0, [tdvLogLikelihood]);
      {$ENDIF}
      UpdateMessagesLog(Format('Search with NJ starting tree completed with LnL = %.8n', [njLnL]));
      UpdateMessagesLog(Format('Performing search with %d MP starting trees', [mpTreeList.Count]));

      { prepare to do search on MP tree candidates}
      searchResults.DeleteAll;
      bestInitialML := TMPMLTreeData(mpTreeList[0]).InitialLogLikelihood;
      for i := 0 to mpTreeList.Count - 1 do
      begin
        UpdateRunStatusInfo(STEP_STR, Format('MP tree #%d of %d', [i + 1, mpTreeList.Count]));
        mpTree := TMPMLTreeData(mpTreeList[i]);
        if CompareValue(mpTree.InitialLogLikelihood, bestInitialML, FP_CUTOFF) > 0 then
          bestInitialML := mpTree.InitialLogLikelihood;

        { only do search on MP tree candidates whose log likelihood is close enough to the current best initial log likelihood - or at least the 1st MP tree candidate}
        if (i = 0) or (CompareValue(abs(mpTree.InitialLogLikelihood) - abs(bestTree.InitialLogLikelihood), 2.0, FP_CUTOFF) <= 0) then
        begin
          UpdateMessagesLog(Format('Performing search with MP tree #%d (%.4f) as starting tree', [i + 1, mpTree.InitialLogLikelihood]));
          if Assigned(MLTreeAnalyzer) then
            MLTreeAnalyzer.InitTree := nil;
          InitializeForMultiStartTreeSearch(mpTree);
          mpLnL := SearchMultiStartTree(TTreeData(mpTree));
          mpTree.LogLikelihood := mpLnL;
          aResult := TMPMLTreeData.Create(mpTree.NoOfOTUs);
          aResult.Assign(mpTree);
          searchResults.Add(aResult);
          {$IFNDEF VISUAL_BUILD}
          UpdateMessagesLog(Format('Initial MP tree #%d has LnL=%.8n and TreeLength=%d', [i + 1, TMPMLTreeData(mpTreeList[i]).InitialLogLikelihood, Round(TMPMLTreeData(mpTreeList[i]).TreeLength)]));
          {$ENDIF}
          UpdateMessagesLog(Format('Search with MP tree #%d completed with LnL = %0.4n', [i + 1, mpLnL]));
          if CompareValue(abs(mpLnl), abs(bestTree.LogLikelihood), FP_CUTOFF) < 0 then
          begin
            UpdateMessagesLog(Format('New best tree with LnL=%.6n vs LnL=%.6n found', [mpLnL, bestTree.LogLikelihood]));
            FreeAndNil(bestTree);
            bestTree := TMPMLTreeData.Create(mpTree.NoOfOTUs);
            bestTree.Assign(mpTree);
            bestTreeIsMp := True;
          end
          else
            UpdateMessagesLog(Format('Inferior or worse MP tree with LnL=%.6n vs best LnL=%.6n found', [mpLnL, bestTree.LogLikelihood]));
        end
        else
          UpdateMessagesLog(Format('skipping ML search for tree #%d with initial Lnl = %.2n worse than best initial ML = %.2n', [i + 1, mpTree.InitialLogLikelihood, bestInitialMl]));
      end;
      RemoveRunStatusInfo(STEP_STR);
      numTreesSearched := searchResults.Count + 1;
      mpTree := nil;
      MLTree.InitNodeData(bestTree);
      MLTree.LogLikelihood := bestTree.LogLikelihood;
      MLTreeAnalyzer.InitTree := bestTree.Clone;
      if not Assigned(FAnalysisInfo.MyOriTreeList) then
        FAnalysisInfo.MyOriTreeList := TTreeList.Create;
      bestTree.Value := bestTree.LogLikelihood;
      FAnalysisInfo.MyOriTreeList.Add(bestTree);
      {$IFNDEF VISUAL_BUILD}
      searchResults.ExportToNewickFile(NextAvailableFilenameNV('_MP_finish_trees.nwk'), searchResults.isBLen, searchResults.isStats, 0.0, [tdvTreeLength, tdvLogLikelihood]);
      D_MegaMain.AnalysisSummary.AddCalculatedValue(NUM_TREES_SEARCHED, IntToStr(numTreesSearched));
      {$ELSE}
      FAnalysisInfo.AnalysisSummary.AddCalculatedValue(NUM_TREES_SEARCHED, IntToStr(numTreesSearched));
      {$ENDIF}
      EndExecute;
      UpdateAnalysisSummary;
    except
      on E:EAbort do
      begin
        Canceled := True;
        FIsSuccess := False;
        MessagesLog.Add(E.Message);
      end;
      on E:Exception do
      begin
        FIsSuccess := False;
        MessagesLog.Add(E.Message);
      end;
    end;
  finally
    {$IFNDEF VISUAL_BUILD}
    MessagesLog.SaveToFile(NextAvailableFilenameNV('_multi-start-trees.log'));
    {$ENDIF}
    if Assigned(njTree) then
      njTree.Free;
    if Assigned(mpTree) then
      mpTree.Free;
    if Assigned(mpTreeList) then
      mpTreeList.Free;
    if Assigned(searchResults) then
      searchResults.Free;
  end;
end;

function TMLTreeSearchThread.SearchMultiStartTree(var aTree: TTreeData): Double;
var
  L0: Double = 0;
  origValue: Double = 0.0;
begin
  if not Initialize then
    raise Exception.Create('Initialization of main ML search thread failed');
  origValue := aTree.Value;
  OptimizeMsg := 'Optimizing initial NJ tree';

  MLTreeAnalyzer.PrepareSearchMLTree(False);
  MLTree := MLTreeAnalyzer.MLTree;
  MyModel.Assign(MLTree.Model);

  OptimizeMsg := 'Optimizing tree';
  Search;

  MLTree.GetTreeData(aTree);
  L0 := MLTree.LogLikelihood;
  if MLTreeAnalyzer.NoOfInfoSites > MLTreeAnalyzer.NoOfSeqs*10 then
    MLTreeAnalyzer.InitializeBLens(1)
  else
    MLTreeAnalyzer.InitializeBLens(2);
  MLTree.OptimizeAllBLens(nil, 0, 'Optimizing tree');
  if MLTree.LogLikelihood < L0-deltaL then
  begin
    MLTree.SetBLens(aTree);
    MLTree.OptimizeAllBLens(nil, 0, 'Optimizing tree');
  end;

  OptimizeMsg := 'Optimizing final tree';
  Search;
  Result := MLTree.LogLikelihood;
  MLTree.GetTreeData(aTree);
  aTree.Value := origValue;
end;

procedure TMLTreeSearchThread.Execute;
var
  treedata: TTreeData = nil;
  L0: extended = 0.0;
  {$IFNDEF VISUAL_BUILD}aModelInfo: TModelInfo = nil;{$ENDIF}
begin
  {$IFDEF DEBUG}SaveMLlog(' MLTreeSearchThread.Execute started');{$ENDIF}
  try
    FAnalysisInfo := FProgressDlg.FMAI;
    FAInfo := FAnalysisInfo;
    if FAnalysisInfo.MyInitialTreeMethod = MultipleMPTreesMethod then
    begin
      ExecuteMultiStartTreeSearch;
      Exit;
    end;

    StartExecute;
    {$IFDEF DEBUG}WriteToDevLog('begin TMLTreeSearchThread.Execute');{$ENDIF}
    if FDoExportSearchTrees then
      FNoOfThreads := 1;

    {$IFDEF DEBUG}SaveMLlog(' MLTreeSearchThread.Initialize started');{$ENDIF}
    if not Initialize then
      raise Exception.Create('Initialization of main ML search thread failed');
    {$IFDEF DEBUG}SaveMLlog(' MLTreeSearchThread.Initialize ended');{$ENDIF}

    if not IsBootstrapReplicate  then
      UpdateRunStatusInfo('Log Likelihood', 'Calculating...');

    OptimizeMsg := 'Preparing ML analysis...';
    MLTreeAnalyzer.CheckCancel := CheckCancel;
    MLTreeAnalyzer.PrepareSearchMLTree(MakeInitialTree);
    MLTree := MLTreeAnalyzer.MLTree;
    // TODO: Disable subtask checking\cancelling on MacOS until a way can be found to make them thread-safe
    MLTree.HideSubtaskProgress := {$IFDEF DARWIN}True{$ELSE}False{$ENDIF};
    if IsBootstrapReplicate then
      MLTree.SubtaskCheckCancel := nil
    else
      MLTree.SubtaskCheckCancel := SubTaskCheckCancelFunc;

    MLTree.MinProgress := 40;
    MLTree.MaxProgress := 90;

    MLTree.OptimizePrecision := 8;
    MLTree.MaxIter := 5;
    deltaL  := MLTree.deltaL;
  //  if deltaL = 0.01 then
  //    MaxIter := 10
  //  else
  //    MaxIter := 5;
    MaxIter :=  MLTree.MaxIter*2;

    L0 := MLTree.LogLikelihood;

    MyModel.Assign(MLTree.Model);
    OptimizeMsg := 'Searching ML tree';
    FProgress := 40;
    CheckCancel(FProgress, OptimizeMsg);
    Search;

    FProgress := 90;
    OptimizeMsg := 'Optimizing final tree';
    CheckCancel(FProgress, OptimizeMsg);

    MLTree.MinProgress := 90;
    MLTree.MaxProgress := 100;

    if MLTree.Model.BootTable = nil then
      if L0 < MLTree.LogLikelihood then
        MLTree.Optimize(CheckCancel, OptimizeMsg, false);

  //  MLTree.ResetL0;
    MLTree.Root.des1.ase := MLTree.Root.des2.ase;

  {
    treedata := TTreeData.Create(MLTree.NoOfSeqs,true,true,false,false);
    MLTree.GetTreeData(treedata);
    L0 := MLTree.LogLikelihood;

    MLTreeAnalyzer.InitializeBLens(0);
    MLTree.OptimizeAllBLens(nil, 0, 'Optimizing tree');
    if MLTree.LogLikelihood < L0-deltaL then
    begin
      MLTree.SetBLens(treedata);
      MLTree.ResetL0;
    end;

    MLTree.OptimizePrecision := 9;
    MLTree.MaxIter := 20;
    deltaL  := MLTree.deltaL;
    MaxIter := MLTree.MaxIter;

    OptimizeMsg := 'Optimizing final tree';
    MyModel.Assign(MLTree.Model);
    Rep := 1;
    Search;

    treedata.Free;
  }
  //  EndTime := now;
  //  ShowMessage ('Computation time: '+TimeToStr(EndTime-StartTime));
    EndExecute;
    UpdateAnalysisSummary;
    treedata := TTreeData.Create(MLTree.NoOfSeqs, True, True, False, False);
    MLTree.GetTreeData(treedata);
    FAnalysisInfo.MyOriTreeList.Clear;
    FAnalysisInfo.MyOriTreeList.Add(treedata);
    {$IFDEF VISUAL_BUILD}
    if FCanComputeDataCoverage then
    begin
      ComputeSiteCoverage(FAnalysisInfo);
      MLTree.SetDataCoverage(treedata); { otherwise site coverage gets lost for bootstrap thread}
    end;
    {$ENDIF}

  except
    on E:Exception do
    begin
      { exceptions DO NOT propagate to parent threads so we MUST catch exceptions
        here and callers of this thread must check downstream if any errors were saved}
      FIsSuccess := False;
      MessagesLog.Add(E.Message);
    end;
  end;
  {$IFDEF DEBUG}SaveMLlog(' MLTreeSearchThread.Execute ended'+chr(13)+chr(10));{$ENDIF}
end;

///////////////////////
//  Bootstrap ML tree
///////////////////////

constructor TBootstrapMLThread.Create(MLA: TMLTreeAnalyzer);
begin
  Randomize;
  inherited Create(MLA);
  FCanceled := False;
  FSavedReplicates := TList.Create;
  FCanComputeDataCoverage := True;
  FIsAdaptiveBootstrap := False;
//  GetMem(FBootTable, sizeof(integer)*(MLA.Model.NoOfSites+1));
end;

destructor TBootstrapMLThread.Destroy;
begin
  if FBootTable <> nil then
    FreeMem(FBootTable);

  inherited;
end;

function TBootstrapMLThread.OrigTotalNumSites: Integer;
var
  aInfo: TAnalysisInfo = nil;
begin
  if Assigned(FProgressDlg) and Assigned(FProgressDlg.FMAI) then
  begin
    aInfo := TAnalysisInfo(FProgressDlg.FMAI);
    Result := aInfo.MyNoOfSeqs*aInfo.MyNoOfSites;
  end
  else
    Result := 0;
end;

function TBootstrapMLThread.MyCheckCancel(Progress: integer; Status: AnsiString): boolean;
begin
  Assert(Trim(Status) <> EmptyStr, 'empty status string');
  FProgress := Progress;
  FStatus   := Status;
  try
    if Assigned(FProgressDlg) then
      OnProgress(Progress);
  Except on E: Exception do
    // do nothing, it's not worth it to have the program crash if we lose a progress update.
  end;
  Result := Terminated;
end;

function TBootstrapMLThread.Initialize: Boolean;
var
  MAI : TAnalysisInfo = nil;
  AModel: TGammaRateVariationModel = nil;
  ATree: TTreeData = nil;
begin
  Result := False;
  MAI := FProgressDlg.FMAI;
  if (MAI.isAminoAcid and (OrigTotalNumSites > 10000)) or ((not MAI.isAminoAcid) and (OrigTotalNumSites > 50000)) then
    AModel := CreateSubstitutionModel(MAI, nil)
  else
    AModel := CreateSubstitutionModel(MAI, SubTaskCheckCancelFunc);
  MAI.MyOriTreeList := TTreeList.Create;
  if MAI.MyTreePack.DoesContain(ttUserTree) then
  begin
    MAI.MyOriTreeList.ImportFromNewick(MAI.MyUserNewickTree, MAI.MyOtuNames, False);
    ATree := TTreeData.Create(MAI.NoOfSeqs,false,false,false);
    ATree.Assign(MAI.MyOriTreeList[0]);
  end;
  MLTreeAnalyzer := TMLTreeAnalyzer.Create(MAI.MySeqStrings, ATree, AModel);
  GetMem(FBootTable, sizeof(integer)*(MLTreeAnalyzer.Model.NoOfSites+1));
  MLTreeAnalyzer.CheckCancel := MyCheckCancel;
  MLTreeAnalyzer.SubTaskCheckCancel := SubtaskCheckCancelFunc;
  if MAI.MLSearchFilter > -1 then
    MLTreeAnalyzer.SearchFilter := MAI.MLSearchFilter;
  MAI.MyMLAnalysisPack := MLTreeAnalyzer;
  if not FProgressDlg.Visible then FProgressDlg.Show;
  if MAI.MyTreePack.DoesContain(ttSPRFast) then
    MLTreeAnalyzer.SearchLevel := 3 // this will be the medium search option
  else if MAI.MyTreePack.DoesContain(ttSPRExtensive) then // this will be the extensive search option
    MLTreeAnalyzer.SearchLevel := 5
  else
    MLTreeAnalyzer.SearchLevel := 1;     // 1: NNI; >2: SPR (5: same to RAxML)
  MLTreeAnalyzer.SeqNames := MAI.MyOtuNames;
  MLTreeAnalyzer.NoOfThreadsToUse := MAI.MyNumThreadsToUse;
  MLTreeAnalyzer.InitialTreeOption := MAI.MyInitialTreeMethod;   //0: NJ; 1: BIONJ; 2: ME; 3: MP; 4: NJ/BIONJ(default)
  MAI.MyTreePack.MLInitialTreesMethod := MAI.MyInitialTreeMethod; // so that citation expert can easily get the value
  Result := True;
end;

procedure TBootstrapMLThread.ComputeTempBclPrecision(finalTree: TTreeData);
var
  {$IFDEF DEBUG}
  debug: Boolean = True;
  aMean: Double = -1;
  aMedian: Double = -1;
  {$ENDIF}
  tempData: TTreeData = nil;
  aRep: Integer;
  p: TPartitionList = nil;
  supportVals: T2dArrayOfExtended;
  node: Integer = -1;
  numNodes: Integer = -1;
  aSample: TList = nil;
  i: Integer = -1;
  aData: TTreeData = nil;
  numReps: Integer = 100;
begin
  try
    aSample := TList.Create;
    tempData := TTreeData.Create(finalTree.NoOfOTUs, finalTree.isBLen, finalTree.isSE, True);
    tempData.Assign(finalTree);
    numNodes := 2*(finalTree.NoOfOTUs - 1);

    SetLength(supportVals, numNodes - tempData.NoOfOTUs);
    for node := Low(supportVals) to High(supportVals) do
      SetLength(supportVals[node], numReps);

    SetLength(FTempPrecision, numNodes - tempData.NoOfOTUs);

    for aRep := 0 to numReps - 1 do
    begin
      GetRandomSampleOfSavedReplicates(FSavedReplicates.Count, aSample);
      p := TPartitionList.Create(finalTree.NoOfOTUs, 0, False);
      for i := 0 to aSample.Count - 1 do
      begin
        aData := TTreeData(aSample[i]);
        p.Add(aData.NodeArray, 0.0);
      end;
      p.Compare(tempData.NodeArray, PArrayOfDouble(@tempData.StatsArray^[tempData.NoOfOTUs]));

      for node := tempData.NoOfOTUs to (numNodes - 1) do
        supportVals[node - tempData.NoOfOTUs][aRep] := tempData.Stats[node]/FSavedReplicates.Count;

      FreeAndNil(p);
    end;

    for node := Low(FTempPrecision) to High(FTempPrecision) do
    begin
      {$IFDEF DEBUG}
      debug := CheckSupportValuesMakeSense(finalTree.Stats[node + finalTree.NoOfOTUs]/FSavedReplicates.Count, supportVals[node], aMedian, aMean);
      Assert(debug, Format('support values nonsensical: BCL=.0n mean=%.4f median=%.4f', [aData.Stats[node + aData.NoOfOTUs], aMean, aMedian]));
      {$ENDIF}
      FTempPrecision[node] := ComputeBootstrapStdDev(finalTree.Stats[node + finalTree.NoOfOTUs]/FSavedReplicates.Count, supportVals[node])*100;
    end;
  finally
    if Assigned(aSample) then
    begin
      aSample.Clear;
      aSample.Free;
    end;
    if Assigned(tempData) then
      tempData.Free;
    if Assigned(p) then
      p.Free;
    for node := Low(supportVals) to High(supportVals) do
      SetLength(supportVals[node], 0);
    SetLength(supportVals, 0);
  end;
end;

procedure TBootstrapMLThread.ComputeFinalBclPrecision(var finalTree: TTreeData);
var
  i: Integer = -1;
  aPrecision: Double = 0;
begin
  for i := finalTree.NoOfOTUs to 2*(finalTree.NoOfOTUs - 1) - 1 do
  begin
    aPrecision := ExpectedBclPrecision(trunc(finalTree.Stats[i]/FNoOfReplication*100), FNoOfReplication);
    finalTree.StatsStdDev[i] := aPrecision
  end;
end;

procedure TBootstrapMLThread.GetRandomSampleOfSavedReplicates(const sampleSize: Integer; var aList: TList);
var
  i: Integer = 1;
  index: Integer;
  data: TTreeData = nil;
begin
  aList.Clear;
  while i <= sampleSize do
  begin
    index := random(FSavedReplicates.Count);
    data := TTreeData(FSavedReplicates[index]);
    aList.Add(data);
    inc(i);
  end;
  Assert(aList.Count = sampleSize, Format('expected %d trees but got %d', [sampleSize, aList.Count]));
end;

procedure BootStrap(f: PArrayOfInt; n: integer);
var
  i: integer;
begin
  Randomize;
  for i := 0 to n do
    f[i] := 0;
  for i := 1 to n do
    Inc(f[Random(n)+1]);
end;

procedure TBootstrapMLThread.Search;
var
  {$IFNDEF VISUAL_BUILD}
  aInfo: TModelInfo = nil;
  output: String = '';
  {$ENDIF}
  OriTree: TTreeData = nil;
  TmpTree: TTreeData = nil;
  OriModel: TGammaRateVariationModel = nil;
  i: integer = -1;
  OriLogLikelihood: Extended = -1;
  numNodesStable: Integer = 0;
  oriCommonSites: Integer = 0;
  oriInvarSites: Integer = 0;
begin
  FLastUpdateTime := Time;
  if not Initialize then
    raise Exception.Create('Failed to initialize TBootstrapMLThread');

  Rep := 0;
  FProgress := 0;

  try
    with MLTreeAnalyzer do
    begin
      RuntimeProgress := ProgressDlg;
      FAnalysisInfo := TAnalysisInfo(ProgressDlg.FMAI);
      FProgress := 0;
      MLTreeAnalyzer.StartTime := Now;
      MLTreeAnalyzer.EndTime := Now;
      begin
        if IsInitialTree then
          Optimize
        else
          SearchMLTree(true, False);
        MLTreeAnalyzer.EndTime := Now;
        OriTree := TTreeData.Create(NoOfSeqs, true, false, false);
        GetTreeData(OriTree);
        oriCommonSites := Model.NoOfCommonSites;
        oriInvarSites := Model.NoOfInvarSites;
        OriModel := CopyModel(Model);
        OriLogLikelihood := MLTreeAnalyzer.LogLikelihood;
        if Terminated then exit;
        RuntimeProgress := nil;
        FStatus   := 'Bootstrapping ML tree';

        if MillisecondsBetween(MLTreeAnalyzer.EndTime, MLTreeAnalyzer.StartTime) > MAX_INTERVAL_NO_BSPROG_UPDATE then
          UpdateRunStatusInfo('Operation', 'Bootstrapping ML tree')
        else
          UpdateRunStatusInfo('Status', FStatus);
        FProgress := 0;
        OnProgress(FProgress);
        {$IFNDEF VISUAL_BUILD}
        MLTree.SubtaskCheckCancel := nil;
        {$ENDIF}
      end;

      begin
        SearchLevel := 1;
        MaxNoOfIteration  := 5;
//        MinDeltaLogLikelihood := 1.0E-3;
        Model.BootTable := FBootTable;
        Model.UpdateProgressFunc := SubTaskCheckCancelFunc;
        TmpTree := TTreeData.Create(NoOfSeqs, true, false, false);
        MyCheckCancel(FProgress, 'Bootstrapping');
        for i := 0 to FNoOfReplication-1 do
        begin
          Rep := i;
          UpdateRunStatusInfo('Replicate No.  ', IntToStr(Rep + 1) + ' of ' + IntToStr(FNoOfReplication));
          BootStrap(FBootTable, Model.NoOfSites);
          Model.SetParamsFromSeqs(Seqs);
          MLTreeAnalyzer.SubTaskCheckCancel := SubTaskCheckCancelFunc;
          if not SearchMLTree(True, True, ProgressDlg) then Terminate;

          if Terminated then
          begin
            Canceled := True;
            Break;
          end;
          FProgress := trunc((Rep + 1)/FNoOfReplication*100);
          MyCheckCancel(FProgress, 'Bootstrapping');

          GetTreeData(TmpTree);
          SaveReplicate(TmpTree);
          FBootstrapTrees.Add(TmpTree.NodeArray, 0.0);
        end;

        while (Rep < DEFAULT_MAX_ADAPTIVE_REPS) and (not PrecisionHasStabilized(OriTree, numNodesStable)) do
        begin
          inc(Rep);
          UpdateRunStatusInfo('Replicate No.  ', IntToStr(Rep) + ' - adaptive');
          UpdateRunStatusInfo('Stable BCL Values', Format('%.0n of %.0n', [numNodesStable*1.0, OriTree.NumInternalNodes(False)*1.0]));
          BootStrap(FBootTable, Model.NoOfSites);
          Model.SetParamsFromSeqs(Seqs);
          MLTreeAnalyzer.SubTaskCheckCancel := SubTaskCheckCancelFunc;
          if not SearchMLTree(True, True, ProgressDlg) then Terminate;

          if Terminated then
          begin
            Canceled := True;
            Break;
          end;
          FProgress := trunc(Rep/DEFAULT_MAX_ADAPTIVE_REPS*100);
          MyCheckCancel(FProgress, Format('Bootstrap (threshold = %.1f%%)', [PrecisionThreshold]));

          GetTreeData(TmpTree);
          SaveReplicate(TmpTree);
          FBootstrapTrees.Add(TmpTree.NodeArray, 0.0);
          inc(FNoOfReplication);
        end;
      end;
      if FIsAdaptiveBootstrap then
        FAnalysisInfo.MyBootReps := FNoOfReplication;
      FBootstrapTrees.BuildPartitionsSummary(FNoOfReplication);
      FProgress := 100;
      {$IFDEF VISUAL_BUILD}
      FStatus   := 'Preparing display';
      {$ELSE}
      FStatus := 'Preparing output';
      {$ENDIF}
      //OnProgress(FProgress);

      Model.BootTable := nil;
      Model.Assign(OriModel);
      Model.NoOfCommonSites := oriCommonSites;
      Model.NoOfInvarSites := oriInvarSites;
      { the seemingly redundant SetTreeData and GetTreeData below are necessary to not lose SE for branch lengths}
      OriTree.isSE:=False;
      MLTree.InitNodeData(OriTree);
      OriTree.isSE := True;
      GetTreeData(OriTree);
      MLTree.LogLikelihood :=  OriLogLikelihood;
      FBootstrapTrees.Compare(OriTree.NodeArray, PArrayOfDouble(@OriTree.StatsArray[OriTree.NoOfOTUs]));
      OriTree.IsStatsStdDev := True;
      ComputeFinalBclPrecision(OriTree);
      FAnalysisInfo.MyOriTreeList.Clear;
      FAnalysisInfo.MyOriTreeList.Add(OriTree);
      if FIsAdaptiveBootstrap then
        FAnalysisInfo.AnalysisSummary.AddCalculatedValue('No. of Replicates', IntToStr(FNoOfReplication));
      OriTree := nil;

      FIsSuccess := True;
    end;
  finally
    if Assigned(TmpTree) then
      FreeAndNil(TmpTree);
    if Assigned(OriTree) then
      FreeAndNil(OriTree);
    if Assigned(OriModel) then
      FreeAndNil(OriModel);
  end;
end;


procedure TBootstrapMLThread.AddBootTree(nodeData: PArrayOfNodeData; freq: Integer);
begin
  FBootstrapTrees.AddWithFreq(nodeData, 0.0, freq);
end;

function TBootstrapMLThread.AnalysisDescription: String;
begin
  Result := 'ML Bootstrap';
end;

procedure TBootstrapMLThread.ClearSavedReplicates;
var
  i: Integer = -1;
  d: TTreeData = nil;
begin
  if FSavedReplicates.Count > 0 then
    for i := FSavedReplicates.Count - 1 downto 0 do
    begin
      d := TTreeData(FSavedReplicates[i]);
      d.Free;
    end;
  FSavedReplicates.Clear;
end;

procedure TBootstrapMLThread.SaveReplicate(aReplicate: TTreeData);
var
  d: TTreeData = nil;
begin
  d := TTreeData.Create(aReplicate.NoOfOTUs, aReplicate.isBLen, aReplicate.IsSE, True);
  d.Assign(aReplicate);
  FSavedReplicates.Add(d);
end;

function TBootstrapMLThread.PrecisionHasStabilized(const OriTree: TTreeData; var numNodesStable: Integer): Boolean;
var
  aData: TTreeData = nil;
  {$IFDEF DEBUG}
  meanPrec: Double = -1;
  {$ENDIF}
  i: Integer = -1;
  numNodesUnstable: Integer = 0;
begin
  Result := True;
  numNodesStable := 0;
  if not FIsAdaptiveBootstrap then
    Exit;

  try

    aData := TTreeData.Create(OriTree.NoOfOTUs, OriTree.isBLen, OriTree.isSE, True);
    aData.Assign(OriTree);
    FBootstrapTrees.Compare(aData.NodeArray, PArrayOfDouble(@aData.StatsArray[aData.NoOfOTUs]));
    ComputeTempBclPrecision(aData);
    {$IFDEF DEBUG}
    meanPrec := math.Mean(FTempPrecision);
    {$ENDIF}
    for i := Low(FTempPrecision) to High(FTempPrecision) do
      if CompareValue(FTempPrecision[i], PrecisionThreshold, FP_CUTOFF) > 0 then
      begin
        Result := False;
        inc(numNodesUnStable);
      end;
    numNodesStable := Length(FTempPrecision) - numNodesUnstable;
  finally
    if Assigned(aData) then
      aData.Free;
  end;
end;

function TBootstrapMLThread.PrecisionThreshold: Double;
begin
  {$IFDEF VISUAL_BUILD}
  Result := FAnalysisInfo.GetFloatSetting(opsBootstrapPrecisionThreshold)
  {$ELSE}
  if FAnalysisInfo.HasFloatSetting(opsBootstrapPrecisionThreshold) then // to support older mao files, fallback to default if not found
    Result := FAnalysisInfo.GetFloatSetting(opsBootstrapPrecisionThreshold)
  else
    Result := DEFAULT_ADAPTIVE_BOOTSTRAP_TARGET_PRECISION;
  {$ENDIF}
end;

{
procedure TBootstrapMLBLenThread.Search;
var
  b2: array of extended;
  TmpTree: TTreeData;
  f: PArrayOfInt;
  i,j: integer;
begin
  try
    with MLTreeAnalyzer do
    begin
      TmpTree := nil;
      TmpTree := TTreeData.Create(NoOfSeqs, true, false, false);

      f := nil;
      GetMem(f, SizeOf(integer)*(NoOfSites+2));
      SetLength(b2, 2*NoOfSeqs-1);

      FStatus   := 'Bootstrapping ML branch lengths';
      FProgress := 0;

      Model.BootTable := f;

      for i := 1 to FNoOfReplication do
      begin
        BootStrap(f, Model.NoOfSites);

//        if Model is TProteinMatrixModel then
//        begin
//          if TProteinMatrixModel(Model).UseFreq then
//            Model.SetParamsFromSeqs(Seqs);
//        end
//        else if not((Model is TJCModel) or (Model is TK2Model)) then
//          Model.SetParamsFromSeqs(Seqs);

        Model.SetParamsFromSeqs(Seqs);


        if MolecularClock then
          MakeClockTree(nil)
        else
          Optimize;

        if Terminated then Break;

        GetTreeData(TmpTree);

        for j := 0 to 2*NoOfSeqs-2 do
          b2[j] := b2[j] +TmpTree.BLen[j]*TmpTree.BLen[j];

        FProgress := trunc(i/(FNoOfReplication+1)*100);
      end;

      if Terminated then Exit;

      Model.BootTable := nil;

//      if Model is TProteinMatrixModel then
//      begin
//        if TProteinMatrixModel(Model).UseFreq then
//          Model.SetParamsFromSeqs(Seqs);
//      end
//     else if not((Model is TJCModel) or (Model is TK2Model)) then
//        Model.SetParamsFromSeqs(Seqs);

      Model.SetParamsFromSeqs(Seqs);

      if MolecularClock then
        MakeClockTree(nil)
      else
        Optimize;
      MLTree.ResetL0;

      GetTreeData(TmpTree);
      SetLength(SE, 2*NoOfSeqs-1);
      for j := 0 to 2*NoOfSeqs-2 do
        SE[j]   := sqrt(b2[j]/NoOfReplication -TmpTree.BLen[j]*TmpTree.BLen[j]);

      FProgress := 100;
      FStatus   := 'Preparing display';
      OnProgress(FProgress);
    end;
  finally
    FreeMemAndNil(f);
    FreeAndNil(TmpTree);
    SetLength(b2, 0);
  end;
end;

procedure TBootstrapMLBLenThread.DoOnProgress;
begin
  FProgressDlg.UpdateRunStatusInfo('Status', FStatus);
  FProgressDlg.PercentGauge.Progress := FProgress;
  if MLTreeAnalyzer.MLTree <> nil then
    if MLTreeAnalyzer.LogLikelihood < -0.000000000001 then
      FProgressDlg.UpdateRunStatusInfo('Log Likelihood', AnsiString(FloatToStrF(MLTreeAnalyzer.LogLikelihood,ffFixed,12,4)));
  FProgressDlg.Refresh;
end;

function TBootstrapMLBLenThread.BootstrapCheckCancel(Progress: integer; Status: string): boolean;
begin
  OnProgress(FProgress);
  result := Terminated;
end;
}

{ TMLClockTestThread }

function TMLClockTestThread.CheckCancel(Progress: Integer;
  Status: AnsiString): Boolean;
begin
  FProgress := Progress;
  FStatus   := Status;
  try
    if (FProgressDlg <> nil) {$IFDEF VISUAL_BUILD}and (FProgressDlg.PercentGauge <> nil){$ENDIF} then
      OnProgress(Progress);
  Except on E: Exception do
    // do nothing, it's not worth it to have the program crash if we lose a progress update.
  end;
  result := Terminated;
end;

procedure TMLClockTestThread.InferOutgroup;
var
  Adapter: TSimpleTreeDataAdapter = nil;
  aData: TTreeData = nil;
begin
  Assert(FAnalysisInfo.MyOriTreeList.Count = 1);
  Assert(FAnalysisInfo.MyOriTreeList.IsRooted); { Adapter assumes that it is true}

  try
    Adapter := TSimpleTreeDataAdapter.Create;
    aData := FAnalysisInfo.MyOriTreeList[0];
    Adapter.GuessOutgroup(aData);
  finally
    if Assigned(Adapter) then
      Adapter.Free;
  end;
end;

constructor TMLClockTestThread.Create(MLTreeAnalyzer: TMLTreeAnalyzer);
begin
  inherited;
  FreeOnTerminate := True;
  IsSuccess := False;
end;

destructor TMLClockTestThread.Destroy;
begin
  inherited;
end;

procedure TMLClockTestThread.DoOnProgress;
begin
  inherited;
end;

procedure TMLClockTestThread.Search;
const
  lnLDigits = 3;
  paraDigits = 2;
var
  ClockTree: TTreeData;
  ClockExport: TStringList;
  DegreesOfFreedom: Integer;
  TestStatistic: Double;
  NodeLbls: TStringList;
begin
  ClockTree := nil;
  ClockExport := nil;
  NodeLbls := nil;
  FLastUpdateTime := Time;
  if not Initialize then
    raise Exception.Create('Failed to initialize MLClockTestThread');
  try
    MLTreeAnalyzer.InitTree.isSE := True;
    FAnalysisInfo.MyOriTreeList.isSE := True;
    MLTreeAnalyzer.InitTree.isStats := False;
    FAnalysisInfo.MyOriTreeList.isStats := False;
    FStatus := 'Optimizing user tree';
    OnProgress(FProgress);
    MLTreeAnalyzer.OptimizeMessage := 'Optimizing user tree';
    OptimizeBlens := True;
    if OptimizeBlens then
      MLTreeAnalyzer.ComputeInitialBLens(2);
    MLTreeAnalyzer.Initialize;
    if OptimizeBlens then
    begin
      if OptimizeParams then
        MLTreeAnalyzer.Optimize
      else
        MLTreeAnalyzer.OptimizeBLens;
    end
    else if OptimizeParams then
      MLTreeAnalyzer.OptimizeParams;
    // end of user tree analysis


    // now make a clock tree from the ML optimized user tree
    FAnalysisInfo.MyOriTreeList.ValueName := 'LogL';
    NodeLbls := TStringList.Create;
    NodeLbls.Assign(FAnalysisInfo.MyOriTreeList[0].GetInternalNodeLabels);
    MLTreeAnalyzer.ModelInfoList.Add(TModelInfo.Create);
    MLTreeAnalyzer.GetModelInfo(MLTreeAnalyzer.ModelInfoList[0]);

    MLTreeAnalyzer.RuntimeProgress := FAnalysisInfo.ARP;
    UpdateRunStatusInfo('status', 'Constructing clock tree');
    ClockTree := TTreeData.Create(FAnalysisInfo.NoOfSeqs, FAnalysisInfo.MyOriTreeList[0].isBLen, FAnalysisInfo.MyOriTreeList[0].isSE, FAnalysisInfo.MyOriTreeList[0].isStats);
    MLTreeAnalyzer.MLTree.GetTreeData(ClockTree);
    if IsDeveloper then
      MLTreeAnalyzer.MakeClockTree(ClockTree, ClockExport, FAnalysisInfo.MyOtuNames, NodeLbls, FAnalysisInfo.MaxRateRatio)
    else
      MLTreeAnalyzer.MakeClockTree(ClockTree, ClockExport, FAnalysisInfo.MyOtuNames, NodeLbls, DEFAULT_MAX_RATE_RATIO);
    FAnalysisInfo.ClockTreeExport := ClockExport;
    FAnalysisInfo.MyOriTreeList.Add(ClockTree);

    MLTreeAnalyzer.ModelInfoList.Add(TModelInfo.Create);
    MLTreeAnalyzer.GetModelInfo(MLTreeAnalyzer.ModelInfoList[1]); // copy the active model info (for clock tree) into the newly created model info
    FAnalysisInfo.LogLikelihoodWithClock  := Format('%.3f', [MLTreeAnalyzer.ModelInfoList[1].LogL]);
    FAnalysisInfo.LogLikelihoodWithoutClock := Format('%.3f', [MLTreeAnalyzer.ModelInfoList[0].LogL]);
    FAnalysisInfo.InvarWithClock := AnsiString(FloatToStrF(MLTreeAnalyzer.ModelInfoList[1].Invar, ffFixed, 12, paraDigits));
    FAnalysisInfo.GammaWithClock := AnsiString(FloatToStrF(MLTreeAnalyzer.ModelInfoList[1].Gamma, ffFixed, 12, lnLDigits));
    FAnalysisInfo.InvarWithoutClock := AnsiString(FloatToStrF(MLTreeAnalyzer.ModelInfoList[0].Invar, ffFixed, 12, paraDigits));
    FAnalysisInfo.GammaWithoutClock := AnsiString(FloatToStrF(MLTreeAnalyzer.ModelInfoList[0].Gamma, ffFixed, 12, paraDigits));
    FAnalysisInfo.FullModelName := MLTreeAnalyzer.ModelInfoList[0].FullName;
    DegreesOfFreedom := MLTreeAnalyzer.NoOfClockOTUs-1;
    FAnalysisInfo.DegreesOfFreedom := AnsiString(IntToStr(abs(MLTreeAnalyzer.ModelInfoList[0].NoOfParams - (MLTreeAnalyzer.ModelInfoList[1].NoOfParams - (MLTreeAnalyzer.NoOfSeqs - 2)))));
    TestStatistic := MLTreeAnalyzer.ModelInfoList[0].LogL - MLTreeAnalyzer.ModelInfoList[1].LogL;
    if (TestStatistic < 0.0) and (PercentDifference(MLTreeAnalyzer.ModelInfoList[0].LogL, MLTreeAnalyzer.ModelInfoList[1].LogL) < 3.0) then
      TestStatistic := 0.0;

    if TestStatistic < 0.0 then
      raise Exception.Create('Calculation error: Test statistic cannot be negative (was ' + Format('%.3f', [TestStatistic]) + ')');

    FAnalysisInfo.MolecularClockTest := Format('%.4e', [GammaQ(DegreesOfFreedom / 2, TestStatistic)]);
    IsSuccess := True;
  Except
    on E:EAbort do
    begin
      Canceled := True;
    end;
    On E: Exception do
    begin
      IsSuccess := False;
      ShowErrorMessage(E);
    end;
  end;
end;

function TMLClockTestThread.Initialize: Boolean;
var
  MAI : TAnalysisInfo = nil;
  AModel: TGammaRateVariationModel = nil;
  ATree: TTreeData = nil;
  useOutgroupToRootTree: Boolean = False;
begin
  Result := False;
  MAI := FProgressDlg.FMAI;
  AModel := CreateSubstitutionModel(MAI, SubTaskCheckCancelFunc);
  MAI.MyOriTreeList := TTreeList.Create;
  if MAI.MyTreePack.DoesContain(ttUserTree) then
  begin
    MAI.MyOriTreeList.ImportFromNewick(MAI.MyUserNewickTree, MAI.MyOtuNames, False);
    if MAI.MyOriTreeList.isRooted then
    begin
      {$IFNDEF VISUAL_BUILD}
      if D_MegaMain.UserHasProvidedOutgroupFile then
      begin
        warn_NV(Format('A rooted tree was provided for the molecular clock test but a list of outgroup taxa was also provided. The tree will be rooted on the outgroup defined in the %s file', [D_MegaMain.GroupsFilename]));
        useOutgroupToRootTree := True;
      end
      else if MAI.OutgroupDefined then
      begin
        warn_NV('A rooted tree was provided for the molecular clock test but an outgroup was also defined in the input data. The tree will be rooted on the outgroup that is defined in the input data');
        useOutgroupToRootTree := True;
      end
      else
      begin
        InferOutgroup;
        useOutgroupToRootTree := False;
      end;
      {$ELSE}
      InferOutgroup;
      useOutgroupToRootTree := False
      {$ENDIF}
    end
    else
      useOutgroupToRootTree := True;

    if useOutgroupToRootTree then
      MAI.GroupInfo.SetTreeOutgroupFromOtuInfos;

    ATree := TTreeData.Create(MAI.NoOfSeqs, True, False, False);
    if not MAI.MyOriTreeList[0].isBLen then
      MAI.MyOriTreeList[0].isBlen := True;
    ATree.Assign(MAI.MyOriTreeList[0]);
    MLTreeAnalyzer := TMLTreeAnalyzer.Create(MAI.MySeqStrings, ATree, AModel);
  end
  else
    raise Exception.Create('required tree for molecular clock test is missing');

  MLACheckCancel := MLTreeAnalyzer.CheckCancel;
  MLTreeAnalyzer.CheckCancel := CheckCancel;
  MLTreeAnalyzer.SubTaskCheckCancel := SubTaskCheckCancelFunc;
  MLTreeAnalyzer.NoOfThreadsToUse := MAI.MyNumThreadsToUse;
  if MAI.MLSearchFilter > -1 then
    MLTreeAnalyzer.SearchFilter := MAI.MLSearchFilter;
  MLTreeAnalyzer.IsGlobalClock := True;
  MAI.ClockTypeSet := True;
  MAI.MyMLAnalysisPack := MLTreeAnalyzer;
  MLTreeAnalyzer.NeedsRootByOutgroup := useOutgroupToRootTree;
  {$IFDEF VISUAL_BUILD}
  if MAI.MyNumThreadsToUse > 1 then
    MegaForm.MultithreadedCalcIsRunning := True
  else
    MegaForm.NumSingleThreadedMLAnalyses := MegaForm.NumSingleThreadedMLAnalyses + 1;
{$ENDIF}
  Result := True;
end;

function TMLClockTestThread.AnalysisDescription: String;
begin
  Result := 'ML Clock Test';
end;

end.
