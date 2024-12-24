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

unit MLSearchThread;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType,
  Classes, StdCtrls, ExtCtrls, SysUtils, Math, SyncObjs,
  MegaConsts, MLTree, MLTreeAnalyzer, MTreeData, MTreeList, MPartitionList,
  MLModels, MAnalysisInfo, MTreeSearchThread, Dialogs;

type

  { TMLTreeThread }

  TMLTreeThread = class(TTreeSearchThread)
    MLTreeAnalyzer: TMLTreeAnalyzer;

  protected
    FSubTasksCheckCancel: TCheckCancelFunc;
    FSubtaskProgress: Integer;
    FSubtaskStatus: AnsiString;
    FExceptionName: AnsiString; // GS - added 12-22-2011, gives us a way to handle uncaught exceptions in the onterm procedure
    FExceptionMessage: AnsiString; // GS - added 12-22-2011, gives us a way to handle uncaught exceptions in the onterm procedure
    FStatus: Ansistring;
    FLastUpdateTime: TDateTime; { for limiting progress updates}
    FRunStatusType: AnsiString;
    FRunStatusValue: AnsiString;
    procedure DoOnProgress; override;
    function Initialize: Boolean; virtual; abstract;
    procedure Execute; override;
    function SubTaskCheckCancelFunc(Progress: Integer; Status: AnsiString): Boolean; virtual;
    procedure DoSubTaskCheckCancel;
    procedure UpdateRunStatusInfo(StatusType: AnsiString; StatusValue: AnsiString);
    procedure DoUpdateRunStatusInfo;
    procedure DoSynchronizeErrorMessage;
  public
    procedure SynchronizeErrorMessage(E: Exception); { GS - switch context to main thread to display error message because the LCL is not thread safe}
    property MyExceptionName: AnsiString read FExceptionName;  // GS - added 12-22-2011, gives us a way to handle uncaught exceptions in the onterm procedure
    property MyExceptionMessage: AnsiString read FExceptionMessage; // GS - added 12-22-2011, gives us a way to handle uncaught exceptions in the onterm procedure
    property SubTasksCheckCancel: TCheckCancelFunc read FSubTasksCheckCancel write FSubTasksCheckCancel;
    constructor Create(Analyzer: TMLTreeAnalyzer);
    destructor Destroy; override;
  end;

  { TMLTreeAnalyzeThread }

  TMLTreeAnalyzeThread = class(TMLTreeThread)
  private
    FOptimizeBLens:  boolean;
    FOptimizeParams: boolean;
    MLACheckCancel: TCheckCancelFunc;
  protected
    function CheckCancel(Progress: integer; Status: AnsiString): boolean;
    procedure Search; override;
    function Initialize: Boolean; override;
  public
    GroupNames: TStringList;
    property OptimizeBLens: boolean read FOptimizeBLens write FOptimizeBLens;
    property OptimizeParams: boolean read FOptimizeParams write FOptimizeParams;

    constructor Create(Analyzer: TMLTreeAnalyzer);
    destructor Destroy; override;
  end;

  TMLTreeSearchThread = class;

  TMLTreeSearchOnNodeThread = class(TMLTreeThread)
  private
    ThreadIndex: integer;
    SwapList: TList;

    MLTree : TMLTree;
    SearchLevel: integer;
    BLenFilter: extended;
    deltaL: extended;
    MaxIter: integer;

    MainThread: TMLTreeSearchThread;
    MyModel: TGammaRateVariationModel;

    p: array of TMLTreeNode;
    q: array of TMLTreeNode;
//    cfg: PArrayOfInt;
//    L0: extended;
    vb,sb: extended;
    d: integer;

    Done: boolean;

    function SetInitialSubtree(node: TMLTreeNode): boolean;
    function IncSubtreeDown(index: integer): boolean;
    function IncSubtreeUp(index: integer): boolean;
    procedure DecSubtreeDown(index: integer);
    procedure ResetBLens;
    procedure InitMem;

    function SearchEachNode(node: TMLTreeNode):boolean;
    procedure CheckNeedToFreeMem;
  protected
    procedure Search; override;
    procedure Execute; override;
    function Initialize: Boolean; override;
  public
    IsInitialized: Boolean;
    constructor Create(parent: TMLTreeSearchThread;
                       index: integer;
                       MLA: TMLTreeAnalyzer);

    destructor Destroy; override;
  end;

  TMLTreeSearchThread = class(TMLTreeSearchOnNodeThread)
  private
    ThreadLock: TCriticalSection;
    ChildThread: array of TMLTreeSearchOnNodeThread;
    MasterSwapList: TList;
    MLACheckCancel: TCheckCancelFunc;

    FNoOfThreads: integer;

    MakeInitialTree: boolean;
    NodeStarted: array of boolean;
    ChildStarted: boolean;
    function GetNodeForSearch: TMLTreeNode;
    function CheckChildThreadDone: boolean;
    procedure RestartChildThreads;
    function GetProgress: integer;
  protected
    function Initialize: Boolean; override;{ moved computational stuff (i.e. CreateSubstitutionModel) out of ProcessSeqMLPhylogenyCommand and into here because it freezes the gui when using large datasets}
    function GetIsInitialTree: boolean;
    function CheckCancel(Progress: integer; Status: AnsiString): boolean;
    procedure Search; override;
    procedure Execute; override;
  public
    IsBootstrapReplicate: Boolean;
    SuppressProgUpdates: Boolean; { if true, do not update progress, set this to true for bootstrap where the data set is not large, so that progress updates are not given too often}
    property IsInitialTree: boolean read GetIsInitialTree;
    property NoOfThreads: integer read FNoOfThreads write FNoOfThreads;

    constructor Create(MLA: TMLTreeAnalyzer);
    destructor Destroy; override;
  end;

  { TBootstrapMLThread }

  TBootstrapMLThread = class(TMLTreeThread)
  protected
    Rep: integer;
    FNoOfReplication: integer;
    FBootstrapTrees: TPartitionList;
    FBootTable: PArrayOfInt;
    FNoOfThreads: integer;
    function MyCheckCancel(Progress: integer; Status: AnsiString): boolean;
    function Initialize: Boolean; override;{ moved computational stuff (i.e. CreateSubstitutionModel) out of ProcessSeqMLPhylogenyCommand and into here because it freezes the gui when using large datasets}
    procedure Search; override;
  public

    property NoOfReplication: integer read FNoOfReplication write FNoOfReplication;
    property BootstrapTrees: TPartitionList read FBootstrapTrees write FBootstrapTrees;
    property NoOfThreads: integer read FNoOfThreads write FNoOfThreads;

    constructor Create(MLA: TMLTreeAnalyzer);
    destructor Destroy; override;
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
    FNoOfThreads: Integer;
    MLACheckCancel: TCheckCancelFunc;
  protected
    function CheckCancel(Progress: integer; Status: AnsiString): boolean;
    function Initialize: Boolean; override;
    procedure DoOnProgress; override;
    procedure Search; override;
  public
    constructor Create(Analyzer: TMLTreeAnalyzer; numThreads: Integer);
    destructor Destroy; override;
    property NoOfThreads: Integer read FNoOfThreads write FNoOfThreads;
  end;

  { TMLClockTestThread }

  TMLClockTestThread = class(TMLTreeThread)
  private
    MLACheckCancel: TCheckCancelFunc;
    FOptimizeBLens:  boolean;
    FOptimizeParams: boolean;
    FAnalysisInfo: TAnalysisInfo;
//    FMergeRates: Boolean;
    FNumBootReps: Integer;

    function CheckCancel(Progress: Integer; Status: AnsiString): Boolean;
  protected
    procedure DoOnProgress; override;
    procedure Search; override;
    function Initialize: Boolean; override;
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
  procedure BootStrapSubset(motherTable: PArrayOfInt; subsetTable: PArrayOfInt; numTotalSites, beginSite, endSite: LongInt);

implementation

uses
  {$IFDEF VISUAL_BUILD}
  Mega_Main,
  {$ELSE}
  MegaUtils_NV, ShowTrees, MD_MegaMain,
  {$ENDIF}
 {$IFDEF DEBUG}LazLoggerBase,{$ENDIF}
  MegaUtils, MegaErrUtils, MSubstitutionModelUtils, MTreePack, MDistPack, mmlcalculatedvalues,
  dateutils;



/// <summary>Construct an instance of TMLTreeThread</summary>
/// <remarks>Glen added the FExceptionName and FExceptionMessage fields on 12-22-2011 as
/// part of a way to handle certain uncaught exceptions in the onterm procedure. In doing so,
/// we can handle the exceptions gracefully and avoid application crashes (or what appear to
/// be crashes to the user when we display the bug report dialog).</remarks>
constructor TMLTreeThread.Create(Analyzer: TMLTreeAnalyzer);
begin
  inherited Create;
  FLastUpdateTime := Time;
  FStatus := EmptyStr;
  FExceptionName := 'none'; // GS - added 12-22-2011, gives us a way to handle uncaught exceptions in the onterm procedure
  FExceptionMessage := 'none'; // GS - added 12-22-2011, gives us a way to handle uncaught exceptions in the onterm procedure
  MLTreeAnalyzer := Analyzer;
  FSubtasksCheckCancel := nil;
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

/// <summary>Execute this code when the thread is launched</summary>
/// <remarks>Glen added the try/except on 12-22-2011. This gives us a way to trap any
/// uncaught exceptions that we know how to handle. Then we can handle them in the
/// thread termination code and exit gracefully instead of crashing the application.
/// If we don't know how to handle an exception, we let MadExcept deal with it.</remarks>
/// <note>Ideally, this would have been done in TTreeSearchThread but there exists some
/// kind of naming conflict in that unit with SysUtils (same problem caused this try/except
/// code to be added to TParsimSearchThread.Execute instead of in TTreeSearchThread)</note>
/// <note>In the future, we may want to register our own exception handlers with MadExcept
/// which would give us more control. For instance, we can customize the bug report dialog
/// so that the user knows that Mega didn't crash but rather it just couldn't complete the analysis.</note>
procedure TMLTreeThread.Execute;
begin
  try
    ReturnValue := 1;
    if Terminated then begin
      FCanceled := true;
      Exit;
    end;
    Search;
    if Terminated then
      FCanceled := true
    else
      ReturnValue := 0;
    {$IFNDEF VISUAL_BUILD}{$IFNDEF MSWINDOWS}
    UpdatePeakMemUsage;
    {$ENDIF}{$ENDIF}
  Except
    on E: EAbort do
    begin
      FCanceled := True;
    end;
    On E : EOutOfMemory do // trap out of memory exceptions and handle it ourselves in the onterm procedure
    begin
      FExceptionName := E.ClassName;
      FExceptionMessage := E.Message;
    end;
    {$IFDEF VISUAL_BUILD}
    On E : Exception do
    begin
      FExceptionName := 'HandledException';  // so the onterm procedure won't try to display tree explorer
      FExceptionMessage := 'Already taken care of.';
    end;
    {$ELSE}
    On E : Exception do
    begin
      FExceptionName := E.ClassName;
      FExceptionMessage := E.Message;
    end;
    {$ENDIF}
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

procedure TMLTreeThread.UpdateRunStatusInfo(StatusType: AnsiString;
  StatusValue: AnsiString);
begin
  FRunStatusType := StatusType;
  FRunStatusValue := StatusValue;
  Synchronize(DoUpdateRunStatusInfo);
end;

procedure TMLTreeThread.DoOnProgress;
begin
  FProgressDlg.UpdateRunStatusInfo('Status', FStatus);
  FProgressDlg.Progress := FProgress;
  if Assigned(MLTreeAnalyzer) and Assigned(MLTreeAnalyzer.MLTree) then
    if MLTreeAnalyzer.LogLikelihood < -0.000000000001 then
      FProgressDlg.UpdateRunStatusInfo('Log Likelihood', AnsiString(FloatToStrF(MLTreeAnalyzer.LogLikelihood,ffFixed,12,4)));
  FProgressDlg.Refresh;
end;

procedure TMLTreeThread.DoSubTaskCheckCancel;
begin
  if Assigned(FSubtasksCheckCancel) then
    FCanceled := FSubtasksCheckCancel(FSubtaskProgress, FSubtaskStatus);
  if FCanceled then
    Terminate;
end;

procedure TMLTreeThread.DoUpdateRunStatusInfo;
begin
  if Assigned(ProgressDlg) then
  begin
    ProgressDlg.UpdateRunStatusInfo(FRunStatusType, FRunStatusValue);
    {$IFDEF VISUAL_BUILD}
    ProgressDlg.Refresh;
    {$ENDIF}
  end;
end;

procedure TMLTreeThread.DoSynchronizeErrorMessage;
begin
  {$IFDEF VISUAL_BUILD}
  ShowMessage(Format('Error (%s) in ML search thread: %s', [FExceptionName, FExceptionMessage]));
  {$ELSE}
  error_nv(Format('Error (%s) in ML search thread: %s', [FExceptionName, FExceptionMessage]));
  {$ENDIF}
end;

procedure TMLTreeThread.SynchronizeErrorMessage(E: Exception);
begin
  FExceptionName := E.ClassName;
  FExceptionMessage := E.Message;
  Synchronize(DoSynchronizeErrorMessage);
end;

///////////////////////
//  Model test
///////////////////////

constructor TModelTestThread.Create(Analyzer: TMLTreeAnalyzer; numThreads: Integer);
begin
  inherited Create(Analyzer);
  FNoOfThreads := numThreads;
end;

destructor TModelTestThread.Destroy;
begin
  if MLTreeAnalyzer <> nil then
    MLTreeAnalyzer.CheckCancel := MLACheckCancel;

  inherited;
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
begin
  Result := False;

  MAI := FProgressDlg.FMAI;
//  FProgressDlg.IsMarqueeMode := True;
  AModel := CreateSubstitutionModel(MAI, SubTaskCheckCancelFunc);
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
  //MLTreeAnalyzer.SubTaskCheckCancel := SubTaskCheckCancelFunc;
  if MAI.MLBLensFilter > -1 then
    MLTreeAnalyzer.BLenFilter := MAI.MLBLensFilter;
  UpdateRunStatusInfo('Status', 'Making initial tree');

  MAI.MyMLAnalysisPack := MLTreeAnalyzer;
  MLTreeAnalyzer.SeqNames := MAI.MyOtuNames;
  MLTreeAnalyzer.InitialTreeOption := MAI.MyInitialTreeMethod;   //0: NJ; 1: BIONJ; 2: ME; 3: MP; 4: NJ/BIONJ(default)
  MAI.MyTreePack.MLInitialTreesMethod := MAI.MyInitialTreeMethod; // so that citation expert can easily get the value
  MLACheckCancel := MLTreeAnalyzer.CheckCancel;
  MLTreeAnalyzer.CheckCancel := CheckCancel;
  FNoOfTreesExamined := 0;
  {$IFNDEF VISUAL_BUILD}
  FreeOnTerminate := true; // GS - setting this to false causes a deadlock in model selection as the call to free never returns.
  {$ENDIF}
  MLTreeAnalyzer.RuntimeProgress := FProgressDlg;
  Result := True;
end;

procedure TModelTestThread.Search;
begin
  FLastUpdateTime := Time;
  if not Initialize then
    raise Exception.Create('Failed to initialize TModelTestThread');
  if MLTreeAnalyzer.Model is TProteinMatrixModel then
    MLTreeAnalyzer.TestProteinModels(FNoOfThreads)
  else
    MLTreeAnalyzer.TestDNAModels(FNoOfThreads);
end;

procedure TModelTestThread.DoOnProgress;
begin
  FProgressDlg.Progress := FProgress;
  FProgressDlg.UpdateRunStatusInfo('Status', FStatus);
  if MLTreeAnalyzer.MLTree <> nil then
    if MLTreeAnalyzer.LogLikelihood < -0.000000000001 then
      FprogressDlg.UpdateRunStatusInfo('Log Likelihood', AnsiString(FloatToStrF(MLTreeAnalyzer.LogLikelihood,ffFixed,12,4)));
  {$IFDEF VISUAL_BUILD}
  FProgressDlg.Refresh;
  {$ENDIF}
end;

///////////////////////
//  Analyze ML tree
///////////////////////

constructor TMLTreeAnalyzeThread.Create(Analyzer: TMLTreeAnalyzer);
begin
  inherited Create(Analyzer);
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
  ClockTree: TTreeData = nil;
  Mai: TAnalysisInfo = nil;
  //{*BEGIN_CLOSED_SOURCE}
  //{ TODO 1 -oglen -cbabybootstrap : remove the dev variables below - they are only used for testing the baby bootstrap analysis }
  //devModelInfo: TModelInfo = nil;
  //devPartitionStrings: TStringList = nil;
  //devPartitionList: TPartitionList = nil;
  //devBootstrapBLens: TLittleBootstrapCalculator = nil;
  //devTreeData: TTreeData = nil;
  //{*END_CLOSED_SOURCE}
begin
  try
    try
      FLastUpdateTime := Time;
      if not Initialize then
        raise Exception.Create('Failed to initialize TMLTreeAnalyzeThread');
      FStatus := 'Optimizing tree';
      OnProgress(FProgress);
      MLTreeAnalyzer.OptimizeMessage := 'Optimizing tree';
      MLTreeAnalyzer.Initialize;
      Mai := TAnalysisInfo(ProgressDlg.FMAI);
      if OptimizeBlens then
      begin
        if OptimizeParams then
          MLTreeAnalyzer.Optimize
        else
          MLTreeAnalyzer.OptimizeBLens;
      end
      else if OptimizeParams then
        MLTreeAnalyzer.OptimizeParams;

      if mai.ClockType = ctGlobal then
      begin
        ClockTree := TTreeData.Create(MLTreeAnalyzer.NoOfSeqs, True, True, False, False);
        MLTreeAnalyzer.MLTree.GetTreeData(ClockTree);
        MLTreeAnalyzer.IsGlobalClock := True;
        MLTreeAnalyzer.GlobalClockLevel := 0;
        if IsDeveloper then
          MLTreeAnalyzer.MakeClockTree(ClockTree, Mai.MaxRateRatio, MAI.MyOtuNames)
        else
          MLTreeAnalyzer.MakeClockTree(ClockTree, DEFAULT_MAX_RATE_RATIO, MAI.MyOtuNames);
        Mai.MyOriTreeList.Delete(0);
        Mai.MyOriTreeList.Add(ClockTree);
      end;
      ReturnValue := 0;
    except
      on E:EAbort do { if the user cancelled, exit gracefully, otherwise, let the exception propogate}
        FCanceled := True
    end;
  finally
  end;
end;

function TMLTreeAnalyzeThread.Initialize: Boolean;
var
  MAI : TAnalysisInfo;
  AModel: TGammaRateVariationModel;
  ATree: TTreeData;
  LampreyFound: Boolean;
  i: Integer;
begin
  FProgress := 1;
  OnProgress(FProgress);
  Result := False;
  MAI := FProgressDlg.FMAI;
  AModel := CreateSubstitutionModel(MAI, SubtaskCheckCancelFunc);
  SubtaskCheckCancelFunc(0, 'Model generated');
  MAI.MyOriTreeList := TTreeList.Create;
  if MAI.MyTreePack.DoesContain(ttUserTree) then
  begin
    MAI.MyOriTreeList.ImportFromNewick(MAI.MyUserNewickTree, MAI.MyOtuNames, False);
    if (MAI.MyUsrOperation = dtdoEpML) or MAI.MyTreePack.DoesContain(ttClock) or (MAI.MyUsrOperation = dtdoMLInferAncSeq) then
    begin
      Assert(Assigned(GroupNames));
      for i := 0 to GroupNames.Count - 1 do
        if SameText(GroupNames.ValueFromIndex[i], 'outgroup') then
          MAI.MyOriTreeList.MarkTaxonAsOutgroupMember(GroupNames.Names[i]);
    end;
    if MAI.MyUsrOperation = dtdoMLInferAncSeqMyPeg then
    begin
      LampreyFound := MAI.MyOriTreeList.MarkTaxonAsOutgroupMember('Lamprey');
      Assert(LampreyFound);
    end;
    ATree := TTreeData.Create(MAI.NoOfSeqs,false,false,false);
    ATree.Assign(MAI.MyOriTreeList[0]);
    MLTreeAnalyzer := TMLTreeAnalyzer.Create(MAI.MySeqStrings, ATree, AModel);  //  to specify initial tree
  end
  else
    MLTreeAnalyzer := TMLTreeAnalyzer.Create(MAI.MySeqStrings, nil, AModel);                    //  to force making initial tree
  if (MAI.MyUsrOperation = dtdoMLInferAncSeq) or (MAI.MyUsrOperation = dtdoMLInferAncSeqMyPeg) then
  begin
    MLTreeAnalyzer.NeedsRootByOutgroup := True;
    MAI.MyOriTreeList.IsRooted := True;
  end;
  MLTreeAnalyzer.SubTaskCheckCancel := SubtaskCheckCancelFunc;
  MLTreeAnalyzer.NoOfThreadsToUse := MAI.MyNumThreadsToUse;
  MLACheckCancel := MLTreeAnalyzer.CheckCancel;
  MLTreeAnalyzer.SeqNames := MAI.MyOtuNames;
  MLTreeAnalyzer.CheckCancel := CheckCancel;
  if MAI.MLBLensFilter > -1 then
    MLTreeAnalyzer.BLenFilter := MAI.MLBLensFilter;
  if (MAI.MyUsrOperation = dtdoMLComputeUserTreeBLens) and (MAI.MyTreePack.DoesContain(ttClock)) then
  begin
    MLTreeAnalyzer.IsGlobalClock := True;
    MAI.ClockType := ctGlobal;
    MAI.ClockTypeSet := True;
    MAI.ClockLevel := clNoStdErr;
  end;
  MAI.MyMLAnalysisPack := MLTreeAnalyzer;
  FProgress := 2;
  Result := True;
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

  { Somehow an EInvalidPointer operation can occur on this free when bootstrapping starts.
    I cannot duplicate this but we have recieved several bug reports where mad except
    shows that the crash happens here - Glen 2-17-2014. Something is probably being
    freed more than once}
  try
    if MyModel <> nil then
      FreeAndNil(MyModel);
  except
    // let it go
  end;

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
    aNoOfSites := Length(MLTree.Seqs[0]);  { using aNoOfSites here because for the SUE analysis we sub-sample the data but FNoOfSites still needs to match the full data dimension}
  end
  else
    aNoOfSites := Length(MLTreeAnalyzer.Seqs[0]);
  SearchLevel     := MLTreeAnalyzer.SearchLevel;
  BLenFilter      := MLTreeAnalyzer.BLenFilter;
  deltaL          := MLTreeAnalyzer.MinDeltaLogLikelihood;
  MaxIter         := MLTreeAnalyzer.MaxNoOfIteration;
  MyModel  := CopyModel(MLTreeAnalyzer.Model);
  SwapList := TList.Create;
  setlength(p, SearchLevel*2+4);
  setlength(q, SearchLevel*2+4);
  if MyModel.SeqDataType = protein then
    for i := 0 to length(q)-1 do
      q[i] := TMLTreeNode.Create(i, false, MLTreeAnalyzer.NoOfSeqs, aNoOfSites, MyModel.NoOfRates, 20)
  else
    for i := 0 to length(q)-1 do
      q[i] := TMLTreeNode.Create(i, false, MLTreeAnalyzer.NoOfSeqs, aNoOfSites, MyModel.NoOfRates, 4);
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
      MLTree.ResetSiteConfig1OfNode(n, true);
      MLTree.ComputeL1OfNode(n);
    end;
  end;


  procedure ResetSiteConfigDown(n: TMLTreeNode);
  begin
    MLTree.ResetSiteConfig0OfNode(n);
    MLTree.ComputeL0OfNode(n);

    if not (n.des1.OTU or n.des1.flag) then
      ResetSiteConfigDown(n.des1);
    if not (n.des2.OTU or n.des2.flag) then
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
  sb := sb +p[index].blen;
  vb := vb +p[index].ase*p[index].ase;

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
  sb := sb +p[index].blen;
  vb := vb +p[index].ase*p[index].ase;

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

  sb := sb -p[index].blen;
  vb := vb -p[index].ase*p[index].ase;

  dec(d);
end;

procedure TMLTreeSearchOnNodeThread.ResetBLens;
var
  i: integer;
begin
  for i := 1 to 2*d+1 do
    q[i].blen := p[i].blen;
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

  function InitSubtreeLikelihood(node: TMLTreeNode): extended;

    procedure RenewL1OfAnc(n: TMLTreeNode);
    begin
      if n.anc = nil then exit;

      MLTree.ComputeL1OfNode(n.anc)
    end;

    procedure InitL1OfNode(n: TMLTreeNode);
    begin
      if n.flag then
        exit;

      InitL1OfNode(n.des1);
      InitL1OfNode(n.des2);

      MLTree.ComputeL1OfNode(n);
    end;

  begin
    InitL1OfNode(node);

    RenewL1OfAnc(node);

    result := MLTree.ComputeLogLikelihoodOfNodeWithModel(node.anc, MyModel);
  end;

  function InitSubtree(n: TMLTreeNode): extended;

    procedure InitSubtreeUp(n: TMLTreeNode);
    begin
      if n.flag then
        exit;

      InitSubtreeUp(n.des1);
      InitSubtreeUp(n.des2);

      MLTree.ResetSiteConfig1OfNode(n, true);
    end;

    procedure InitSubtreeDown(n: TMLTreeNode);
    begin
      MyModel.ComputeProbOfNode(n, n.blen);

      if n.OTU or n.flag then
        exit;

      MLTree.ResetSiteConfig0OfNode(n);

      InitSubtreeDown(n.des1);
      InitSubtreeDown(n.des2);
    end;

  begin
    InitSubtreeUp(n);
    InitSubtreeDown(n);

    result := InitSubtreeLikelihood(n);
  end;

  procedure IterateSubtreeBLens(node: TMLTreeNode; var L: extended);

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

  begin
    IterateSubtreeOfNode(node);
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
        end;

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
    L, L1, L2: extended;
    n: TMLTreeNode;
    i: integer;
  begin
    Result := true;

    L1 := MLTree.LogLikelihood;

//    SetBLens;

    RemoveBranch(q[2]);
    AddBranch(q[1], q[2], q[2*d  ]);
    q[3].blen := p[1].blen;
    q[1].blen := p[3].blen;

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

    if L > L1+deltaL then
    begin
      AddSwapData(1, L-L1, 2*d+1);
      result := false;
    end;

    RemoveBranch(q[2]);
    AddBranch(q[1], q[2], q[2*d+1]);
    q[3].blen     := p[1].blen;
    q[1].blen     := p[3].blen;
    q[2*d  ].blen := p[2*d  ].blen;
    q[2*d+1].blen := p[2*d+1].blen;

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

    if L > L1+deltaL then
    begin
      AddSwapData(2, L-L1, 2*d+1);
      result := false;
    end;

    RemoveBranch(q[2]);
    AddBranch(q[1], q[2], q[3]);

    ResetBLens;
  end;

  function SPRRecursiveDown: boolean;
  var
    flag: boolean;
  begin
    result := true;
    if d > SearchLevel then exit;

    if {(not q[2*d].done) or} ((vb < 0.000000000001) or (sb < BLenFilter*sqrt(vb))) then
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

    if {(not q[2*d+1].done) or} ((vb < 0.000000000001) or (sb < BLenFilter*sqrt(vb))) then
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

    if {(not q[2*d+1].done) or} ((vb < 0.000000000001) or (sb < BLenFilter*sqrt(vb))) then
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

    if {(not q[2*d].done) or} ((vb < 0.000000000001) or (sb < BLenFilter*sqrt(vb))) then
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
  sb := q[3].blen;
  vb := q[3].ase*q[3].ase;

  if {(not q[3].done) or} ((vb < 0.000000000001) or (sb < BLenFilter*sqrt(vb))) then
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

  sb := q[1].blen;
  vb := q[1].ase*q[1].ase;

  if {(not q[1].done) or} ((vb < 0.000000000001) or (sb < BLenFilter*sqrt(vb))) then
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
  repeat
    n := MainThread.GetNodeForSearch;
    if n <> nil then
      SearchEachNode(n);

    if ThreadIndex = 0 then
      if MainThread.CheckCancel(MainThread.GetProgress,'Searching ML tree') then
      begin
        Terminate;
        break;
      end;
  until n = nil;
end;

procedure TMLTreeSearchOnNodeThread.Execute;
begin
  repeat
    Search;
    Done := true;
    while (not terminated) and Done do
      sleep(0);
  until Terminated;
end;

///////////////////

constructor TMLTreeSearchThread.Create(MLA: TMLTreeAnalyzer);
begin
  inherited create(self, 0, MLA);
  SuppressProgUpdates := False;
  IsBootstrapReplicate := False;
  ThreadLock := TCriticalSection.Create;
  MasterSwapList := TList.Create;
  if Assigned(MLA) then { in the case of bootstrap, MLTreeAnalyzer is already created}
  begin
    IsInitialized := True
  end
  else
    IsInitialized := False;
end;

destructor TMLTreeSearchThread.Destroy;
var
  i: integer;
begin
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

function TMLTreeSearchThread.Initialize: Boolean;
var
  MAI : TAnalysisInfo;
  AModel: TGammaRateVariationModel;
  ATree: TTreeData;
begin
  AModel := nil;
  MAI := nil;
  ATree := nil;
  Result := False;
  if not IsInitialized then
  begin
    MAI := FProgressDlg.FMAI;
    AModel := CreateSubstitutionModel(MAI, CheckCancel);
    if not Assigned(MAI.MyOriTreeList) then
      MAI.MyOriTreeList := TTreeList.Create;
    if not Assigned(MLTreeAnalyzer) then
    begin
      if MAI.MyTreePack.DoesContain(ttUserTree) then
      begin
        MAI.MyOriTreeList.ImportFromNewick(MAI.MyUserNewickTree, MAI.MyOtuNames, False);
        ATree := TTreeData.Create(MAI.NoOfSeqs,false,false,false);
        ATree.Assign(MAI.MyOriTreeList[0]);
        MLTreeAnalyzer := TMLTreeAnalyzer.Create(MAI.MySeqStrings, ATree, AModel);  //  to specify initial tree
        AModel := nil;
      end
      else
      begin
        UpdateRunStatusInfo('Status', 'Making initial tree');
        MLTreeAnalyzer := TMLTreeAnalyzer.Create(MAI.MySeqStrings, nil, AModel);  //  to force making initial tree
        AModel := nil;
      end;
    end;
    MLTreeAnalyzer.NoOfThreadsToUse := MAI.MyNumThreadsToUse;
    MLTreeAnalyzer.SubTaskCheckCancel := SubTaskCheckCancelFunc;
    if MAI.MLBLensFilter > -1 then
      MLTreeAnalyzer.BLenFilter := MAI.MLBLensFilter;

    MAI.MyMLAnalysisPack := MLTreeAnalyzer;
    if not MAI.ARP.Visible then MAI.ARP.Show;

  {
    if MAI.MyTreePack.DoesContain(ttCNI) then
      MAI.MyMLAnalysisPack.SearchLevel := 2
    else
      MAI.MyMLAnalysisPack.SearchLevel := 1;
    MAI.MyMLAnalysisPack.SearchMLTree(true);       // Please do not erase this comments. It's non-thread version for debuging. KT.
    ShowMLTree(MAI);
  }

    if MAI.MyTreePack.DoesContain(ttSPRFast) then
      MLTreeAnalyzer.SearchLevel := 3 // this will be the fast search option
    else if MAI.MyTreePack.DoesContain(ttSPRExtensive) then // this will be the extensive search option
      MLTreeAnalyzer.SearchLevel := 5
    else
      MLTreeAnalyzer.SearchLevel := 1;     // 1: NNI; >2: SPR (5: same to RAxML)

    MLTreeAnalyzer.SeqNames := MAI.MyOtuNames;
    MLTreeAnalyzer.InitialTreeOption := MAI.MyInitialTreeMethod;   //0: NJ; 1: BIONJ; 2: ME; 3: MP; 4: NJ/BIONJ(default)
    MAI.MyTreePack.MLInitialTreesMethod := MAI.MyInitialTreeMethod; // so that citation expert can easily get the value
//    if FProgressDlg.IsMarqueeMode then
//      FProgressDlg.IsMarqueeMode := False;
  end;
  if not IsInitialized then
    inherited Initialize; { can't call this until MLTreeAnalyzer is created}

  MLACheckCancel := MLTreeAnalyzer.CheckCancel;
  MLTreeAnalyzer.CheckCancel := CheckCancel;
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

function TMLTreeSearchThread.CheckCancel(Progress: integer; Status: AnsiString): boolean;
begin
  if Terminated then
    FCanceled := True;
  Result := False;
  if SuppressProgUpdates or (MillisecondsBetween(Time, FLastUpdateTime) < PROG_UPDATE_INTERVAL) then
    Exit;
  FProgress := Progress;
  FStatus   := Status;
  FLastUpdateTime := Time;
  if (FProgressDlg <> nil) {$IFDEF VISUAL_BUILD}and (FProgressDlg.PercentGauge <> nil){$ENDIF} then
    OnProgress(FProgress);
  result := Terminated;
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
        MLTree.Initialize(false);
        for i := 0 to 2*MLTree.NoOfSeqs-2 do
          MLTree.Node[i].flag := true;
        for i := 0 to MasterSwapList.Count-1 do
          TSwapData(MasterSwapList[i]).flag := true;

        SwapEachBranch(0);
        SetFlag(0);
        ResetSiteConfig(MLTree);
        ResetLikelihood(MLTree.Root);

        MLTree.LogLikelihood := MLTree.ComputeLogLikelihood;
        result := 1;
      end;

      ClearMasterSwapList;
      tmpTree.Free;
    end;

    function ResetMLTree: integer;
    var
      i,j: integer;
    begin
      ClearMasterSwapList;
      for i := 0 to NoOfThreads-1 do
        for j := ChildThread[i].SwapList.Count-1 downto 0 do
        begin
          MasterSwapList.Add(ChildThread[i].SwapList[j]);
          ChildThread[i].SwapList.Delete(j);
        end;

//      for i := 0 to 2*MLTree.NoOfSeqs-2 do
//        MLTree.Node[i].done := true;

      result := 0;
      if MasterSwapList.Count > 0 then
      begin
        MasterSwapList.Sort(compare);
        result := SwapBranches;
      end;
    end; { end ResetMLTree}

  var
    i,it,n: integer;
    dL: extended;
  begin  // begin SearchMLTreeMultiThread
    ReturnValue := 1;
    FProgress := 0;

    for i := 1 to MLTreeAnalyzer.NoOfThreadsToUse-1 do
      ChildThread[i] := nil;

    if not MLTreeAnalyzer.PrepareSearchMLTree(MakeInitialTree) then
    begin
      if Terminated then
        FCanceled := True;
      exit;
    end;

    MLTree := MLTreeAnalyzer.MLTree;
    for i := 0 to 2*MLTree.NoOfSeqs-2 do
      MLTree.Node[i].flag := true;

    MyModel.Assign(MLTree.Model);
    for i := 1 to MLTreeAnalyzer.NoOfThreadsToUse-1 do
      ChildThread[i] := TMLTreeSearchOnNodeThread.Create(self, i, MLTreeAnalyzer);

    try
      FProgress := 0;
      if CheckCancel(0, 'Searching ML tree') then
      begin
        FCanceled := True;
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
          sleep(0);

         if Terminated then
         begin
           FCanceled := True;
           break;
         end;
        n := ResetMLTree;

        inc(it);
      until Terminated or (n = 0) or (abs(MLTree.LogLikelihood-dL) < DeltaL) or (it = MaxIter);

      if Terminated then
        sleep(100)
      else
      begin //      if not Terminated then
        CheckCancel(100, 'Searching ML tree');

        if MLTree.Model.BootTable = nil then
          MLTree.Optimize(CheckCancel, 'Optimizing final tree');

        MLTree.ResetL0;
        MLTree.Root.des1.ase := MLTree.Root.des2.ase;

        ReturnValue := 0;
      end;
    finally
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
        FCanceled := true;
    end;
  end;

begin
  try
    FLastUpdateTime := Time;
    ChildStarted := false;
    if not Initialize then
      raise Exception.Create('Initialization of main ML search thread failed');
    SearchMLTreeMultiThread;
    if (CompareValue(MLTreeAnalyzer.MLTree.LogLikelihood, 0.0, FP_CUTOFF) = 0) or (CompareValue(MLTreeAnalyzer.MLTree.LogLikelihood, 0.0, FP_CUTOFF) > 0) then
      raise Exception.Create(Format('APPLICATION ERROR: Invalid log likelihood value: %.3e', [MLTreeAnalyzer.MLTree.LogLikelihood]));
    //if IsDeveloper and (not TAnalysisInfo(ProgressDlg.FMAI).IsBootstrapSubSampling) and (not (TAnalysisInfo(ProgressDlg.FMAI).MyBootReps > 0)) then
    //begin
    //  devModelInfo := TModelInfo.Create;
    //  MLTreeAnalyzer.GetModelInfo(devModelInfo);
    //  {$IFNDEF VISUAL_BUILD}
    //  TMLCalculatedValues.ExportModelInfoToCsvFile(TAnalysisInfo(ProgressDlg.FMAI).isAminoAcid, devModelInfo, NextAvailableFilenameNV('_ML_data.csv'), MLTreeAnalyzer.DevInitialLogLikelihood);
    //  {$ENDIF}
    //end;
  finally
    MLTreeAnalyzer.SubTaskCheckCancel := nil;
    if IsDeveloper and Assigned(devModelInfo) then
      devModelInfo.Free;
  end;
end;

procedure TMLTreeSearchThread.Execute;
begin
  Search;
end;

///////////////////////
//  Bootstrap ML tree
///////////////////////

constructor TBootstrapMLThread.Create(MLA: TMLTreeAnalyzer);
begin
  inherited Create(MLA);

//  GetMem(FBootTable, sizeof(integer)*(MLA.Model.NoOfSites+1));
end;

destructor TBootstrapMLThread.Destroy;
begin
  if FBootTable <> nil then
    FreeMem(FBootTable);

  inherited;
end;

function TBootstrapMLThread.MyCheckCancel(Progress: integer; Status: AnsiString): boolean;
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

function TBootstrapMLThread.Initialize: Boolean;
var
  MAI : TAnalysisInfo = nil;
  AModel: TGammaRateVariationModel = nil;
  ATree: TTreeData = nil;
begin
  Result := False;
  MAI := FProgressDlg.FMAI;
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
  if MAI.MLBLensFilter > -1 then
    MLTreeAnalyzer.BLenFilter := MAI.MLBLensFilter;
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

procedure BootStrap(f: PArrayOfInt; n: integer);
var
  i: integer;
begin
  for i := 0 to n do
    f[i] := 0;
  for i := 1 to n do
    Inc(f[Random(n)+1]);
end;

procedure BootstrapUpsample(f: PArrayOfInt; numSampleSites, numTotalSites: Integer; sitesUsed: TBoolArray; sampleSites: TIntArray);
var
  i: integer;
  count: Integer = 1;
  site: LongInt;
begin
  for i := 0 to numSampleSites do
    f[i] := 0;
  while count < numTotalSites do
  begin
    site := Random(numSampleSites)+1;
    Inc(f[site]);
    Assert((site >= 1) and (sampleSites[site - 1] >= 0) and (sampleSites[site - 1] <= (High(sitesUsed) + 1)), Format('bad upsample site: %d maps to %d', [site, sampleSites[site - 1]]));
    sitesUsed[sampleSites[site - 1] - 1] := sitesUsed[sampleSites[site - 1] - 1] or True;
    inc(count);
  end;
end;


procedure BootStrapSubset(motherTable: PArrayOfInt; subsetTable: PArrayOfInt; numTotalSites, beginSite, endSite: LongInt);
var
  i, rn, range: Longint;
  totalFreq: LongInt = 0;
begin
  for i := 0 to numTotalSites do
    subsetTable[i] := 0;
  range := (endSite - beginSite);
  if (beginSite + range) > endSite then
    range := numTotalSites - beginSite;
  while totalFreq < numTotalSites do
  begin
    rn := beginSite + Random(range); { we will get a number somewhere in the range beginSite:endSite}
    if rn >= numTotalSites then
      raise Exception.Create('APPLICATION ERROR: out of range when bootstrap sampling');
    if (totalFreq + motherTable[rn]) > numTotalSites then
    begin
      inc(subsetTable[rn], numTotalSites - totalFreq);
      inc(totalFreq, numTotalSites - totalFreq);
    end
    else
    begin
      inc(subsetTable[rn], motherTable[rn]);
      inc(totalFreq, motherTable[rn]);
    end;
  end;
  Assert(totalFreq = numTotalSites, Format('APPLICATION ERROR: bad bootstrap subset. Alignment data has %d sites but total bootstrap frequency is %d', [numTotalSites, totalFreq]));
end;

procedure TBootstrapMLThread.Search;
var
  OriTree,TmpTree: TTreeData;
  OriModel: TGammaRateVariationModel;
  i: integer;
  aInfo: TAnalysisInfo = nil;
begin
  FLastUpdateTime := Time;
  if not Initialize then
    raise Exception.Create('Failed to initialize TBootstrapMLThread');
  TmpTree  := nil;
  OriTree  := nil;
  OriModel := nil;
  Rep := 0;
  FProgress := 0;

  try
    with MLTreeAnalyzer do
    begin
      RuntimeProgress := ProgressDlg;
      FProgress := 0;
      MLTreeAnalyzer.StartTime := Now;
      MLTreeAnalyzer.EndTime := Now;
      if IsInitialTree then
        Optimize
      else
        SearchMLTree(true, False);
      MLTreeAnalyzer.EndTime := Now;
      OriTree := TTreeData.Create(NoOfSeqs, true, false, false);
      GetTreeData(OriTree);
      OriModel := CopyModel(Model);

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
      SearchLevel := 1;
      MaxNoOfIteration  := 10;
      MinDeltaLogLikelihood := 1.0E-3;
      Model.BootTable := FBootTable;
      Model.CheckCancel := SubTaskCheckCancelFunc;
      TmpTree := TTreeData.Create(NoOfSeqs, true, false, false);
      MyCheckCancel(FProgress, 'Bootstrapping Tree...');
      for i := 0 to FNoOfReplication-1 do
      begin
        Rep := i;
        UpdateRunStatusInfo('Replicate No.  ', IntToStr(Rep + 1) + ' of ' + IntToStr(FNoOfReplication));
        BootStrap(FBootTable, Model.NoOfSites);
        Model.SetParamsFromSeqs(Seqs);

        if not SearchMLTree(True, True, ProgressDlg) then Terminate;

        if Terminated then
        begin
          FCanceled := True;
          Break;
        end;
        FProgress := trunc((Rep + 1)/FNoOfReplication*100);
        MyCheckCancel(FProgress, 'Bootstrapping Tree...');

        GetTreeData(TmpTree);
        FBootstrapTrees.Add(TmpTree.NodeArray, 0.0);
        //{*BEGIN_CLOSED_SOURCE}
        //if IsDeveloper then
        //begin
        //  MLTreeAnalyzer.GetModelInfo(devModelInfo);
        //  calculatedValues.AddModelInfo(devModelInfo, MLTreeAnalyzer.DevInitialLogLikelihood);
        //  devPartitionList.Add(TmpTree, 0.0);
        //end;
        //{*END_CLOSED_SOURCE}
      end;
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

      SetTreeData(OriTree);
      OriTree := nil;

//      MLTree.Initialize(false);
      MLTree.ResetL0;
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

constructor TMLClockTestThread.Create(MLTreeAnalyzer: TMLTreeAnalyzer);
begin
  inherited;
//  MLACheckCancel := MLTreeAnalyzer.CheckCancel;
//  MLTreeAnalyzer.CheckCancel := CheckCancel;
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
      MLTreeAnalyzer.ComputeInitialBLens;
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
    if FAnalysisInfo.MyOriTreeList.HasInternalNodeLbls then
      NodeLbls.Assign(FAnalysisInfo.MyOriTreeList.InternalNodeLbls)
    else
      NodeLbls.Add(EmptyStr);
    MLTreeAnalyzer.ModelInfoList.Add(TModelInfo.Create);
    MLTreeAnalyzer.GetModelInfo(MLTreeAnalyzer.ModelInfoList[0]);

    MLTreeAnalyzer.RuntimeProgress := FAnalysisInfo.ARP;
    FAnalysisInfo.ARP.UpdateRunStatusInfo('status', 'Constructing clock tree');
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
      FCanceled := True;
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
  MAI : TAnalysisInfo;
  AModel: TGammaRateVariationModel;
  ATree: TTreeData;
begin
  Result := False;
  MAI := FProgressDlg.FMAI;
  AModel := CreateSubstitutionModel(MAI, SubTaskCheckCancelFunc);
  MAI.MyOriTreeList := TTreeList.Create;
  if MAI.MyTreePack.DoesContain(ttUserTree) then
  begin
    MAI.MyOriTreeList.ImportFromNewick(MAI.MyUserNewickTree, MAI.MyOtuNames, False);
    MAI.GroupInfo.SetTreeOutgroupFromOtuInfos;
    ATree := TTreeData.Create(MAI.NoOfSeqs,false,false,false);
    ATree.Assign(MAI.MyOriTreeList[0]);
    MLTreeAnalyzer := TMLTreeAnalyzer.Create(MAI.MySeqStrings, ATree, AModel);  //  to specify initial tree
  end
  else
  begin
    MLTreeAnalyzer := TMLTreeAnalyzer.Create(MAI.MySeqStrings, nil, AModel);                    //  to force making initial tree
  end;
  MLACheckCancel := MLTreeAnalyzer.CheckCancel;
  MLTreeAnalyzer.CheckCancel := CheckCancel;
  MLTreeAnalyzer.SubTaskCheckCancel := SubTaskCheckCancelFunc;
  MLTreeAnalyzer.NoOfThreadsToUse := MAI.MyNumThreadsToUse;
  if MAI.MLBLensFilter > -1 then
    MLTreeAnalyzer.BLenFilter := MAI.MLBLensFilter;
  MLTreeAnalyzer.IsGlobalClock := True;
  MAI.ClockTypeSet := True;
  MAI.MyMLAnalysisPack := MLTreeAnalyzer;

	{$IFDEF VISUAL_BUILD}
    if MAI.MyNumThreadsToUse > 1 then
      MegaForm.MultithreadedCalcIsRunning := True
    else
      MegaForm.NumSingleThreadedMLAnalyses := MegaForm.NumSingleThreadedMLAnalyses + 1;
	{$ENDIF}
  Result := True;
end;

end.
