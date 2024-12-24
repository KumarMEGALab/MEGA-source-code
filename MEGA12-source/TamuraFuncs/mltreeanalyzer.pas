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

unit MLTreeAnalyzer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  MegaUtils_NV,
  LCLIntf, LCLType, Classes, SysUtils, Math, Forms, SyncObjs, mdistpack,
  MegaConsts, MegaUtils, MTreeEstFunc, MTreeData, MLTree, MLModels, MTreeList,
  MPTree, MPartitionList, MRelTimeComputer, MRuntimeProgressDlg, mcancellable, mmodel_info_list;


type

  { TMLTreeAnalyzer }

  TMLTreeAnalyzer = class(TCancellable)
    SeqNames: TStringList;
    OptimizeMessage: AnsiString;
    CheckCancel: TCheckCancelFunc;
    SubTaskCheckCancel: TCheckCancelFunc;
    RemoveStatusProc: TSendMessageProc;
    RuntimeProgress: TRuntimeProgress;
    InitTree : TTreeData;
    MLTree   : TMLTree;
    SeqData  : TStringList;
    ModelInfoList : TModelInfoList;
    NonClockTree: TTreeData;
    NonClockModel: TGammaRateVariationModel;
    SearchFilter: extended;
    InitialTreeOption: integer;
    SamplingTime: array of extended;
  private
    FInitialNJTreeModelUsed: TDistType;
    FMLTreeAnalyzerVersion: Integer;
    FIsDoingTreeSearch: Boolean;
    FIsModelTamer: Boolean;
    FModel   : TGammaRateVariationModel;
    FNeedsRootByOutgroup: Boolean;
    FNoOfThreadsToUse : integer;
    FNoOfRates  : integer;
    FOrigNoOfSites: Integer;
    FSearchLevel: integer;
    FNoOfInfoSites: integer;

//    FDoubleBLenOpt: boolean;
//    FMaxIter: integer;           // discontinued to dynamically optimized in MLTree
//    FDeltaL : extended;          // discontinued to dynamically optimized in MLTree
    FIsGlobalClock: boolean;
    FGlobalClockLevel: integer;
    FNoOfClockOTUs: integer;
    FIsUserSuppliedTree: boolean;
    FCanceled: boolean;
    FHasRateStdErrs: Boolean; // currently we only have rate variances for bootstrap { TODO 1 -oglen -creltime : remove FHasRateStdErrs as soon as we can get stderrs for rates using an analytical method }
    FOriginalReltimes: ArrayOfExtended; { in the case of anchoring a clocktree, save original reltimes for in-house use to see how reltimes might have changed}
    FIsLittleBootstrap: Boolean;
    FNumTotalSites: Int64;
	FMPSeed: Cardinal;
    FAncestralStatesSorter: TList;
    FTempAncestralStates: TAncStateRecArray;
    function GetModelForInitialTree: TGammaRateVariationModel; deprecated; { this was tried previously but tends to get searching stuck in local optima}
    function GetDevInitialLogLikelihood: Extended;
    procedure SetFreqTable(f: PArrayOfInt);
    function GetFreqTable: PArrayOfInt;
    function GetModel: TGammaRateVariationModel;
    procedure SetIsDoingTreeSearch(AValue: Boolean);
    procedure SetNeedsRootByOutgroup(AValue: Boolean);
    procedure SetSeqs(newseqs: TStringList);
    function GetSeqs: TStringList;

    function GetLogLikelihood: extended;
    function GetIsInitialTree: boolean;
    function GetIsUserSuppliedTree: boolean;
    function GetNoOfSeqs: integer;
    function GetNoOfSites: integer;
    function GetNoOfConfs: integer;
    function GetRate(index: integer): extended;

    function GetMaxIter: integer;
    function GetDeltaL: extended;
    procedure SetMaxIter(value: integer);
//    procedure SetDeltaL(value: extended);

    procedure TieProc(var c1,c2: integer; c: array of Integer);

    procedure SetNoOfThreadsToUse(numThreads: integer);
    function GetNoOfThreadsToUse: integer;
    function MapNodeIndexToTreeDataIndex(NodeIndex: Integer): Integer;
    procedure CheckAbort;
    procedure MyWriteToDevConsole(aMsg: String);
  public
    InitialBlenMethod: integer;
    RunStatusProc: TRunStatusProc;
    DevConsoleProc: TSendMessageProc;
    ModelTamerBootTable: PArrayOfInt;
    StartTime, EndTime: TDateTime; { for bootstrap, if the first tree we search takes a long time, we can set TMLSearchThread to give progress for each search so that progress gets some kind of regular updates}
    function MakeNJTreeForModelTestOrLbsParamsEstimation(isDnaModel: Boolean): TTreeData;
    function NumModelsEvaluated: Integer;
    procedure ClearUnEvaluatedModels(freeMem: Boolean);
    procedure SetCancelled(AValue: Boolean); override;
    function TimeTreeToNewickString(OtuNames: TStringList; UseTimeFactor: Boolean): String;
    procedure SetIsLittleBootstrap(aIsLittleBootstrap: Boolean; aNumTotalSites: Int64);
    property MLTreeAnalyzerVersion: Integer read FMLTreeAnalyzerVersion;
    property IsLittleBootstrap: Boolean read FIsLittleBootstrap;
    property NumSitesPerSubSample: Int64 read FNumTotalSites;
    property NoOfThreadsToUse : integer read GetNoOfThreadsToUse write SetNoOfThreadsToUse;
    property LogLikelihood: extended read GetLogLikelihood;
    property IsInitialTree: boolean read GetIsInitialTree;
    property IsUserSuppliedTree: boolean read GetIsUserSuppliedTree;
    property SearchLevel: integer read FSearchLevel write FSearchLevel;    //  SearchLevel(>2) = Dt4 or SearchLevel(<=2) = Dt2
    property Model: TGammaRateVariationModel read GetModel write FModel;
    property DefaultNoOfRates: integer read FNoOfRates write FNoOfRates;
    property NoOfRates: Integer read FNoOfRates write FNoOfRates;
    property Seqs: TStringList read GetSeqs write SetSeqs;
    property NoOfSeqs: integer read GetNoOfSeqs;
    property NoOfSites: integer read GetNoOfSites;
    property OrigNoOfSites: Integer read FOrigNoOfSites write FOrigNoOfSites; { OrigNoOfSites used here because ModelTamer and LBS only keep unique site configurations in SeqData}
    property NoOfConfs: integer read GetNoOfConfs;
    property FreqTable: PArrayOfInt read GetFreqTable write SetFreqTable;
    property Rate[index: integer]: extended read GetRate;
    property NoOfInfoSites: integer read FNoOfInfoSites;
    property MPSeed: cardinal read FMPSeed write FMPSeed;
//    property DoubleBLenOpt: boolean read FDoubleBLenOpt write FDoubleBLenOpt;

    property MaxNoOfIteration: integer read GetMaxIter write SetMaxIter;
    property MinDeltaLogLikelihood: extended read GetDeltaL; // write SetDeltaL;

    property IsGlobalClock: boolean read FIsGlobalClock write FIsGlobalClock;
    property GlobalClockLevel: integer read FGlobalClockLevel write FGlobalClockLevel;
//    property NoOfClocks: integer read FNoOfClocks;
    property NoOfClockOTUs: integer read FNoOfClockOTUs;
    property IsDoingTreeSearch: Boolean read FIsDoingTreeSearch write SetIsDoingTreeSearch;
    property InitialNJTreeModelUsed: TDistType read FInitialNJTreeModelUsed;

    procedure GetModelInfo(ModelInfo: TModelInfo);

    procedure SetTreeData(tree: TTreeData);
    procedure GetTreeData(tree: TTreeData);

    procedure ComputeInitialBLens(method: integer);  //1:Parsimony, 2:FastOLS
    procedure InitializeBLens(mode: integer);
    procedure MakeInitialTree(method: integer);  // 0: NJ, 1: BIONJ, 2: ME, 3: MP;
    function MakeParsimonyTrees(const numTrees: Integer; var aTreeList: TTreeList; var aLog: TStringList): String;
    procedure Initialize;
    procedure Optimize;
    procedure OptimizeBLens;
    procedure OptimizeParams;

    procedure MakeClockTree(Tree: TTreeData; MaxRateRatio: Extended; namesList: TStringList); overload;

    procedure MakeClockTree(Tree: TTreeData;
                            var ExportList: TStringList;
                            const NamesList: TStringList;
                            const NodeLabels: TStringList;
                            const MaxRateRatio: Extended); overload;
    function AnchorClockTree(Tree: TTreeData;
                             var minTime, maxTime: array of extended;
                             const MaxRateRatio: Extended): boolean;
               // give divergence times of node[0]..node[2*NoOfSeqs-2].
               // do nothing and return FALSE if impossible divergence time (max < min, etc) is given.
               // minTime, maxTime are recomputed to keep consistency among all nodes.
               // Check minTime[i] < maxTime[i] to identify the wrong divergence time if returned with FALSE.

    function GetRelTimes: ArrayOfExtended;

    function AnchorExistingClockTree(aTree: TTreeData; var MinTime, MaxTime: array of Extended; MaxRateRatio: Extended): Boolean;
    procedure MakeNonClockTree;

    procedure GenerateClockTreeExport(var ExportList: TStringList;
                                      const NamesList: TStringList;
                                      const NodeLabels: TStringList); overload;


    procedure GenerateCalibratedClockTreeExport(var ExportList: TStringList;
                                      const NamesList: TStringList;
                                      const NodeLabels: TStringList;
                                      const MinTimes: array of Extended;
                                      const MaxTimes: array of Extended); overload;

    procedure GenerateSampledTimesClockTreeExport(var ExportList: TStringList;
                                                  const NamesList: TStringList;
                                                  const NodeLabels: TStringList;
                                                  const SampleTimes: array of Extended);

    function GenerateNexusReltimeExport(NamesList: TStringList; HasCalibrations: Boolean): TStringList;
    function GenerateNodeMap(NamesList: TStringList): TStringList;
    procedure OutputBootstrapHeights(BSHeights: T2DExtArray);
    procedure OutputBootstrapRates(BSRates: T2DExtArray);

    { Given an id for an internal node, finds two extant taxa for whom the internal
      node is their most recent common ancestor }
    function FindTwoExtantTaxa(InternalNodeId: Integer;
                               TaxaNamesList: TStringList;
                               var TaxaAId: Integer;
                               var TaxaAName: String;
                               var TaxaBId: Integer;
                               var TaxaBName: String): Boolean;


    function GetProbOfRateCategoryAtSite(SiteIndex: integer; var p: array of extended): boolean;
    function GetMLRateCategoryAtSite(SiteIndex: integer): integer;
    function GetMLRateAtSite(SiteIndex: integer): extended;

    procedure InitAncStateEst;
    function GetAncStateProb(NodeIndex, SiteIndex: integer; Probs: TStringList): boolean; overload;
    function GetAncStateProb(NodeIndex, SiteIndex: Int64; var Probs: TAncStateRecArray): Boolean; overload;
    function GetAncStateProbAlphabetical(NodeIndex, SiteIndex: integer; var State: array of TAncStateRec): boolean;
    function GetAncStateProbNotSorted(NodeIndex, SiteIndex: integer; var State: array of TAncStateRec): boolean;
    {$IFNDEF VISUAL_BUILD}
    procedure ExportAllAncStates(NamesList: TStringList; isRooted: Boolean);
    procedure ExportPredictLivingSequences(index: Integer=1);
    {$ENDIF}

    function GetExpectedStateProb(NodeIndex, SiteIndex: integer; Probs: TStringList): boolean; overload;
    function GetExpectedStateProb(NodeIndex, SiteIndex: integer): TAncStateRecArray; overload;
    function GetExpectedStateProbArray(NodeIndex: Integer): TAncStateRecArrayArray;
    function GetExpectedStateProbAlphabetical(NodeIndex, SiteIndex: integer; var Probs: TStringList): boolean;

    procedure TestDNAModels(numThreads: Integer; aBootTable: PArrayOfInt = nil); overload;
    procedure TestDNAModels(isAdaptiveTest: Boolean; aModels: TGammaRateVariationModelArray; numThreads: Integer; aBootTable: PArrayoFInt = nil); overload;
    procedure TestProteinModels(numThreads: Integer; aBootTable: PArrayOfInt = nil); overload;
    procedure TestProteinModels(isAdaptiveTest: Boolean; aModels: TGammaRateVariationModelArray; numThreads: Integer; aBootTable: PArrayOfInt = nil); overload;

    procedure ResetSearchFilter;
    function PrepareSearchMLTree(maketree: boolean): boolean;
    function SearchMLTree(makeinitialtree: boolean; IsBStrapRep: Boolean; AProgress: TRuntimeProgress = nil): boolean;
    procedure ResetMLTree;
    procedure CancelSearch;

    function ReadFromFile(var data: File; SessionVersion: Integer; OutGroupMembers: TBoolArray):boolean;
    procedure WriteToFile(var data: File);
    function StateToStringList(Comment: AnsiString = ''): TStringList;
    constructor Create(seqs: TStringList; initialtree: TTreeData; model: TGammaRateVariationModel);
    destructor Destroy; override;
    procedure Assign(Source: TMLTreeAnalyzer);
    property Cancelled: Boolean read FCanceled write SetCancelled;
    property DevInitialLogLikelihood: Extended read GetDevInitialLogLikelihood; { only used for development of baby bootstrap - later we can remove this}
    property NeedsRootByOutgroup: Boolean read FNeedsRootByOutgroup write SetNeedsRootByOutgroup;
    property IsModelTamer: Boolean read FIsModelTamer write FIsModelTamer;
  end;

  function SortAlpha(List: TStringList; Index1, Index2: Integer): Integer;

var
  ModelSelectionCS: TCriticalSection;

implementation
uses
  {$IFDEF DEBUG}mdeveloper_console,{$ENDIF}
  {$IFNDEF VISUAL_BUILD}
  MD_MegaMain, Graphics,ExcelWrite, Types,
  {$ENDIF}
  MGlobalSettings, mnexuswriter, dateutils, MLongintList,
  Dialogs, MLSearchThread, mmemutils, MNewickExportOptions, mtree_display_setup,
  mextendedlist, mrobinson_foulds_metric, mstringbuilder;

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

constructor TMLTreeAnalyzer.Create(seqs: TStringList; initialtree: TTreeData; model: TGammaRateVariationModel);
var
  i: integer;
begin
  inherited Create;
  {$IFDEF DEBUG}WriteToDevLog('TMLTreeAnalyzer.Create');{$ENDIF}
  FInitialNJTreeModelUsed := gdNone;
  FMLTreeAnalyzerVersion := 4;
  FAncestralStatesSorter := nil;
  OptimizeMessage := 'Optimizing tree';
  FIsDoingTreeSearch := True;
  FIsModelTamer := False;
  ModelTamerBootTable := nil;
  FNeedsRootByOutgroup := False;
  SubTaskCheckCancel := nil;
  RemoveStatusProc := nil;
  MLTree := nil;

  SeqData  := seqs;
  FModel   := model;
  InitTree := initialtree;
  if InitTree <> nil then
    begin
      InitTree.isBLen := true;
      FIsUserSuppliedTree := true;
    end
  else
    FIsUserSuppliedTree := false;

//  FMaxIter := 5;
//  FDeltaL  := 1.0E-2; // 1.0E-4;
  FGlobalClockLevel := 2;
  FNoOfClockOTUs       := 0;

  FNoOfRates   := 5;
  FHasRateStdErrs := False;
  FSearchLevel := 2;

  ModelInfoList := TModelInfoList.Create;

  SearchFilter := 1.0+FP_CUTOFF;
  InitialTreeOption := DefaultInitTreeMethod;  


  // set the default number of threads to use, although the user may change it later
  FNoOfThreadsToUse := Max(1, GetNoOfProcessors); // KT: no need to subtract one anymore
  FAncestralStatesSorter := nil;
  if Assigned(SeqData) then // we don't have seqdata yet if a tree session is being opened
  begin
    setlength(SamplingTime, SeqData.Count);
    for i := 0 to SeqData.Count-1 do
      SamplingTime[i] := 0;
  end;
  StartTime := Now;
  EndTime := Now;

  FMPSeed := 7777;
end;

destructor TMLTreeAnalyzer.Destroy;
var
  i: integer;
begin
  {$IFDEF DEBUG}
  WriteToDevLog('TMLTreeAnalyzer.Destroy');
  MyWriteToDevConsole('TMLTreeAnalyzer.Destroy');
  {$ENDIF}

  if Length(FTempAncestralStates) <> 0 then
    SetLength(FTempAncestralStates, 0);
  SetLength(SamplingTime, 0);
  if MLTree <> nil then
    MLTree.Free;

  if SeqData <> nil then
    SeqData.Free;

  if FModel <> nil then
    FModel.Free;

  if InitTree <> nil then
    InitTree.Free;

  if NonClockTree <> nil then
    NonClockTree.Free;
  if NonClockModel <> nil then
    NonClockModel.Free;

  if ModelInfoList.Count > 0 then
    for i := 0 to ModelInfoList.Count-1 do
      ModelInfoList[i].Free;
  ModelInfoList.Free;
  SetLength(FOriginalReltimes, 0);
  if Assigned(FAncestralStatesSorter) then
    FAncestralStatesSorter.Free;
  inherited;
end;

function TMLTreeAnalyzer.FindTwoExtantTaxa(InternalNodeId: Integer;
                                           TaxaNamesList: TStringList;
                                           var TaxaAId: Integer;
                                           var TaxaAName: String;
                                           var TaxaBId: Integer;
                                           var TaxaBName: String): Boolean;

  function FindOneLeafNode(AncestorId: Integer): Boolean;
  var
    CurrIndex: Integer;
  begin
    Result := False;
    CurrIndex := AncestorId;
    while True do
    begin
      if not Assigned(MLTree.Node[CurrIndex].des1.des1) then // found a leaf node
      begin
        CurrIndex := MLTree.Node[CurrIndex].des1.index;
        break;
      end;
      CurrIndex := MLTree.Node[CurrIndex].des1.index;
    end;
    if CurrIndex <> AncestorId then
    begin
      Result := True;
      TaxaAId := CurrIndex;
      TaxaAName := TaxaNamesList[TaxaAId];
    end;
  end;

  function FindAnotherLeafNode(AncestorId: Integer): Boolean;
  var
    CurrIndex: Integer;
  begin
    Result := False;
    CurrIndex := AncestorId;
    while True do
    begin
      if not Assigned(MLTree.Node[CurrIndex].des2.des2) then // found a leaf node
      begin
        CurrIndex := MLTree.Node[CurrIndex].des2.index;
        break;
      end;
      CurrIndex := MLTree.Node[CurrIndex].des2.index;
    end;
    if CurrIndex <> AncestorId then
    begin
      Result := True;
      TaxaBId := CurrIndex;
      TaxaBName := TaxaNamesList[TaxaBId];
    end;
  end;
begin
  Result := FindOneLeafNode(InternalNodeId) and FindAnotherLeafNode(InternalNodeId);
end;

procedure TMLTreeAnalyzer.SetNoOfThreadsToUse(numThreads: integer);
var
  NumberOfProcessors: integer;
begin
  NumberOfProcessors := Max(1, GetNoOfProcessors);
  if (NumberOfProcessors > 0) and (numThreads > NumberOfProcessors) then  // just in case!
    FNoOfThreadsToUse := NumberOfProcessors
  else
    FNoOfThreadsToUse := numThreads;
  if assigned(MLTree) then
    MLTree.NoOfThreads := FNoOfThreadsToUse;
end;

function TMLTreeAnalyzer.GetNoOfThreadsToUse: integer;
begin
  result := FNoOfThreadsToUse;
end;

function TMLTreeAnalyzer.GetMaxIter: integer;
begin
  result := 0;
  if MLTree <> nil then
    result := MLTree.MaxIter;
end;

function TMLTreeAnalyzer.GetDeltaL: extended;
begin
  result := 0;
  if MLTree <> nil then
    result := MLTree.deltaL;
end;

procedure TMLTreeAnalyzer.SetMaxIter(value: integer);
begin
//  FMaxIter := value;
//  if Model <> nil then
//    Model.MaxIter := value;
  if MLTree <> nil then
    MLTree.MaxIter := value;
end;

//procedure TMLTreeAnalyzer.SetDeltaL(value: extended);
//begin
//  FDeltaL := value;
//  if MLTree <> nil then
//    MLTree.deltaL := value;
//  if Model <> nil then
//    Model.deltaL := value/10;
//end;

procedure TMLTreeAnalyzer.SetSeqs(newseqs: TStringList);
var
  i: integer;
begin
  if newseqs.Count <> NoOfSeqs then exit;

  SeqData  := seqs;

  if MLTree <> nil then
    MLTree.Initialize(true, true);

  setlength(SamplingTime, SeqData.Count);
  for i := 0 to SeqData.Count-1 do
    SamplingTime[i] := 0;
end;

function TMLTreeAnalyzer.GetFreqTable: PArrayOfInt;
begin
  if Model = nil then
    result := nil
  else
    result := Model.BootTable;
end;

procedure TMLTreeAnalyzer.SetFreqTable(f: PArrayOfInt);
begin
  if Model = nil then
    exit;
  Model.BootTable := f;
end;

function TMLTreeAnalyzer.MakeNJTreeForModelTestOrLbsParamsEstimation(isDnaModel: Boolean): TTreeData;
var
  AModel: TGammaRateVariationModel = nil;
  D: PDistanceMatrix = nil;
begin
  try
    if isDnaModel then
      AModel := TTN93Model.Create(0, False, 1)
    else
      AModel := TJTTModel.Create(0, False, False, 1);
    D  := CreateDistanceMatrix(NoOfSeqs);
    if not AModel.ComputePDistanceMatrix(Seqs, D) then
      CorrectDistanceMatrix(D, NoOfSeqs);
    Result := TTreeData.Create(Seqs.Count, True, False, False);
    MakeNJTree(Result, D, False);
  finally
    if Assigned(D) then
      FreeMemAndNil(D);
    if Assigned(AModel) then
      AModel.Free;
  end;
end;

function TMLTreeAnalyzer.GetModelForInitialTree: TGammaRateVariationModel; deprecated; { this was tried previously but tends to get searching stuck in local optima - if chosen model is available for distance methods, use it for initial tree as well}
begin
  raise Exception.Create('Application error - call to deprecated method TMLTreeAnalyzer.GetModelForInitialTree');
  if Model.NoOfStates = 4 then
  begin
    if FModel is TJCModel then
      Result := TJCModel.Create(0, FModel.UseInvar, FModel.NoOfRates)
    else if FModel is TT3Model then
      Result := TT3Model.Create(0, FModel.UseInvar, FModel.NoOfRates)
    else if FModel is TTN93Model then
      Result := TTN93Model.Create(0, FModel.UseInvar, FModel.NoOfRates)
    else
      Result := THKYModel.create(0, False, 4)
  end
  else
  begin
    if FModel is TJTTModel then
      Result := TJTTModel.Create(0, FModel.UseInvar, False, FModel.NoOfRates)
    else if FModel is TDayhoffmodel then
      Result := TDayhoffModel.Create(0, FModel.UseInvar, False, FModel.NoOfRates)
    else if FModel is TPoissonModel then
      Result := TPoissonModel.Create(0, FModel.UseInvar, False, FModel.NoOfRates)
    else if FModel is TPoissonModel then
      Result := TPoissonModel.Create(0, FModel.UseInvar, True, FModel.NoOfRates)
    else
      Result := TPoissonModel.create(0, False, False, 4);
  end;
end;

function TMLTreeAnalyzer.GetDevInitialLogLikelihood: Extended;
begin
  Result := MLTree.DevInitialLogLikelihood;
end;

procedure TMLTreeAnalyzer.SetCancelled(AValue: Boolean);
begin
  FCanceled := AValue;
  if Assigned(MLTree) then
    MLTree.Cancelled := FCanceled;
end;

function TMLTreeAnalyzer.GetIsInitialTree: boolean;
begin
  result := InitTree <> nil;
end;

function TMLTreeAnalyzer.GetIsUserSuppliedTree: boolean; // GS - needed for a captionExpert bug fix
begin
  result := FIsUserSuppliedTree;
end;

function TMLTreeAnalyzer.GetSeqs: TStringList;
begin
  result := SeqData;
end;

function TMLTreeAnalyzer.GetModel: TGammaRateVariationModel;
begin
  result := FModel;
end;

procedure TMLTreeAnalyzer.SetIsDoingTreeSearch(AValue: Boolean);
begin
  FIsDoingTreeSearch := AValue;
  if Assigned(MLTree) then
    if MLTree.IsDoingTreeSearch <> AValue then
      MLTree.IsDoingTreeSearch := AValue;
end;

procedure TMLTreeAnalyzer.SetNeedsRootByOutgroup(AValue: Boolean);
begin
  if FNeedsRootByOutgroup = AValue then Exit;
  FNeedsRootByOutgroup := AValue;
end;

function TMLTreeAnalyzer.GetNoOfSeqs: integer;
begin
  result := SeqData.Count
end;

function TMLTreeAnalyzer.GetNoOfSites: integer;
begin    { TODO -oglen -cmodel tamer : update num sites }
  if FIsLittleBootstrap then
    Result := FNumTotalSites
  else
    result := length(SeqData[0]);
end;

function TMLTreeAnalyzer.GetNoOfConfs: integer;
begin
  result := NoOfSites;
  if assigned(MLTree) then
    result := MLTree.NoOfConfs;
end;

procedure TMLTreeAnalyzer.GetModelInfo(ModelInfo: TModelInfo);
var
  s: extended;
  {$IFDEF CPU64}
  k,i,j: QWord;
  n: QWord;
  {$ELSE}
  k,i,j: Integer;
  n: Integer;
  {$ENDIF}
begin
  Model.GetInfo(ModelInfo);

  ModelInfo.LogL := LogLikelihood;

  k := 2*NoOfSeqs-3 +ModelInfo.NoOfParams;
  if Model.NoOfRates > 1 then
    inc(k);
  if Model.UseInvar then
    inc(k);
  if Model is TProteinMatrixModel then
    if TProteinMatrixModel(Model).UseFreq then
      inc(k,19);

  n := Model.TotalNoOfSites;
  ModelInfo.AICc := -2*ModelInfo.LogL +2*k +2*k*(k+1)/(n-k-1);
  ModelInfo.BIC  := -2*ModelInfo.LogL +k*ln(n);

  ModelInfo.NoOfParams  := k;

  if ModelInfo.DataType = 'DNA' then
    n := 4
  else if ModelInfo.DataType = 'Protein' then
    n := 20;

  s := 0;
  for i := 0 to n-1 do
    for j := 0 to n-1 do
      if i <> j then
        s := s +ModelInfo.Matrix[i,j];

  for i := 0 to n-1 do
    for j := 0 to n-1 do
      ModelInfo.Matrix[i,j] := ModelInfo.Matrix[i,j]/s;

  for i := 0 to n-1 do
  begin
    ModelInfo.Matrix[i,i] := 0;
    for j := 0 to n-1 do
      if i <> j then
        ModelInfo.Matrix[i,i] := ModelInfo.Matrix[i,i] -ModelInfo.Matrix[i,j];
  end;
end;

procedure GetModelInfo(ModelInfo: TModelInfo; MLTree: TMLTree);
var
  s: extended;
  {$IFDEF CPU64}
  k,i,j: QWord;
  n: QWord;
  {$ELSE}
  k,i,j: Integer;
  n: Integer;
  {$ENDIF}
begin
  MLTree.Model.GetInfo(ModelInfo);

  ModelInfo.LogL := MLTree.LogLikelihood;

  k := 2*MLTree.NoOfSeqs-3 +ModelInfo.NoOfParams;
  if MLTree.Model.NoOfRates > 1 then
    inc(k);
  if MLTree.Model.UseInvar then
    inc(k);
  if MLTree.Model is TProteinMatrixModel then
    if TProteinMatrixModel(MLTree.Model).UseFreq then
      inc(k,19);

  n := MLTree.Model.TotalNoOfSites;
  ModelInfo.AICc := -2*ModelInfo.LogL +2*k +2*k*(k+1)/(n-k-1);
  ModelInfo.BIC  := -2*ModelInfo.LogL +k*ln(n);

  ModelInfo.NoOfParams  := k;

  if ModelInfo.DataType = 'DNA' then
    n := 4
  else if ModelInfo.DataType = 'Protein' then
    n := 20;

  s := 0;
  for i := 0 to n-1 do
    for j := 0 to n-1 do
      if i <> j then
        s := s +ModelInfo.Matrix[i,j];

  for i := 0 to n-1 do
    for j := 0 to n-1 do
      ModelInfo.Matrix[i,j] := ModelInfo.Matrix[i,j]/s;

  for i := 0 to n-1 do
  begin
    ModelInfo.Matrix[i,i] := 0;
    for j := 0 to n-1 do
      if i <> j then
        ModelInfo.Matrix[i,i] := ModelInfo.Matrix[i,i] -ModelInfo.Matrix[i,j];
  end;
end;

type
  TTestModelsThread = class;

  { TTestModelsChildThread }

  TTestModelsChildThread = class(TThread)
  private
    FIsModelTamer: Boolean;
    FIsSuccess: Boolean;
    FOrigNoOfSites: Integer;
    ThreadIndex: integer;
    MainThread: TTestModelsThread;
    Done: boolean;
    InitTree: TTreeData;
    SeqData : TStringList;
    FCancelled: Boolean;

    function GetLogText: String;
    procedure TestModels;
  protected
    procedure Execute; override;
  public
    ProgressDlg: TRuntimeProgress;
    MessagesLog: TStringList;
    constructor Create(parent: TTestModelsThread; index: integer; seqs: TStringList; tree: TTreeData; aIsModelTamer: Boolean; aOrigNumSites: Integer);
    destructor Destroy; override;
    function GetApproximateSizeInBytes(const tree: TTreeData; var nsites: Integer; var nrates: Integer; const IsProteinData: Boolean): Int64;
    function CheckCancelProxy(Progress: Integer; Status: AnsiString): Boolean;
    property Cancelled: Boolean read FCancelled write FCancelled;
    property LogText: String read GetLogText;
    property IsSuccess: Boolean read FIsSuccess;
    property IsModelTamer: Boolean read FIsModelTamer;
    property OrigNoOfSites: Integer read FOrigNoOfSites;
  end;

  { TTestModelsThread }

  TTestModelsThread = class(TTestModelsChildThread)
    ProgressMessage: string;
  private
    FDevMsg: String;
    FMaxPercentAvailableRamToUse: Integer;
    FModelName: String;
    FNoOfModels: integer;
    FNoOfThreads: integer;
    ChildThread: array of TTestModelsChildThread;
    Models:    TGammaRateVariationModelArray;
    ModelInfoList: TModelInfoList;
    CurrentModelIndex: integer;
    ThreadMemSize: extended;
    FChildThreadIndex: Integer;
    function GetModelIndex(childIndex: Integer): integer;
    function CheckChildThreadDone: boolean;
    function NumThreadsWorking: Integer;
    procedure SetNoOfThreads(n: integer);
    procedure UpdateProgress;
    procedure MyWriteToDevConsole(aMsg: String);
    procedure DoWriteToDevConsole;
  protected
    procedure Execute; override;
  public
    MyCheckCancel: TProgressAndStatusCheckCancel;
    procedure NotifyChildThreadDone(index: Integer);
    property NoOfModels: integer read FNoOfModels;
    property NoOfThreads: integer read FNoOfThreads write FNoOfThreads;
    property MaxPercentAvailableRamToUse: Integer read FMaxPercentAvailableRamToUse;
    constructor Create(seqs: TStringList; tree: TTreeData; m: TModelInfoList; n: integer; numThreads: Integer; aIsModelTamer: Boolean; aOrigNumSites: Integer; MaxPercentRam: Integer = DEFAULT_MAX_PERCENT_RAM);
    destructor Destroy; override;
  end;

function TTestModelsChildThread.CheckCancelProxy(Progress: Integer; Status: AnsiString): Boolean;
begin
  Result := FCancelled;
end;

constructor TTestModelsChildThread.Create(parent: TTestModelsThread; index: integer; seqs: TStringList; tree: TTreeData; aIsModelTamer: Boolean; aOrigNumSites: Integer);
begin
  inherited Create(true);
  FIsModelTamer := aIsModelTamer;
  FOrigNoOfSites := aOrigNumSites;
  MessagesLog := TStringList.Create;
  FIsSuccess := True;
  SeqData := seqs;
  InitTree := TTreeData.Create(tree.NoOfOTUs, tree.isBLen, tree.isSE, tree.isStats);
  InitTree.Assign(tree);
  Priority := tpNormal;
  MainThread  := parent;
  ThreadIndex := index;
  Done := false;
  FreeOnTerminate := False;
end;

destructor TTestModelsChildThread.Destroy;
begin
  if Assigned(MessagesLog) then
    MessagesLog.Free;
  if Assigned(InitTree) then
    InitTree.Free;
  inherited;
end;

function TTestModelsChildThread.GetApproximateSizeInBytes(const tree: TTreeData; var nsites: Integer; var nrates: Integer; const IsProteinData: Boolean): Int64;
var
  aMLTree: TMLTree;
  aModel: TGammaRateVariationModel;
  numSeqs: Integer;
begin
  try
    Result := 0;
    numSeqs := SeqData.Count;
    if numSeqs > 0 then
      nsites := Length(SeqData[0])
    else
      nsites := 0;
    Result := Result + SizeOf(TStringList); // SeqData
    Result := Result + numSeqs*nsites*SizeOf(Char); // SeqData
    Result := Result + InitTree.GetApproximateSizeInBytes;
    Result := Result + SizeOf(TUpdateRunStatusInfoEvent);
    Result := Result + SizeOf(TTestModelsThread); // reference to main thread
    Result := Result + SizeOf(TTestModelsChildThread); // self
    Result := Result + SizeOf(Integer); // threadIndex
    Result := Result + SizeOf(Boolean); // Done
    aModel := TDayhoffModel.Create(0,True,true,5);
    aModel.SetParamsFromSeqs(SeqData);
    nsites := aModel.NoOfSites;
    nrates := aModel.NoOfRates;
    if FIsModelTamer then
      aMLTree := TMLTree.CreateWithCompressedData(aModel.OrigNoOfSites, SeqData, tree, aModel, False)
    else
      aMLTree := TMLTree.Create(SeqData, tree, aModel, False);
    Result := Result + aMLTree.GetApproximateSizeInBytes;
    if IsProteinData then
      Result := Round(Result*2.1)
    else
      Result := Round(Result*1.1);
  finally
    if Assigned(aMLTree) then
      aMLTree.Free;
  end;
end;

procedure TTestModelsChildThread.TestModels;
var
  mindex: integer;
  AModel: TGammaRateVariationModel;
  AMLTree: TMLTree;
  modelName: String;
begin
  repeat
    if Terminated or FCancelled then
    begin
      Done := True;
      FIsSuccess := False;
      Exit;
    end;
    ModelSelectionCS.Acquire;

    try
      mindex := MainThread.GetModelIndex(ThreadIndex);
      if mindex < 0 then
        AModel := nil
      else
        AModel := MainThread.Models[mindex];
    finally
      ModelSelectionCS.Leave;
    end;

    if AModel <> nil then
    begin
      Assert(Assigned(ProgressDlg), 'acccessing nil ProgressDlg');
      ProgressDlg.RegisterCancellable(AModel);
      if FIsModelTamer then
      begin
        Assert(AModel.BootTable <> nil, 'ModelTamer model is missing the needed boot table');
        AModel.OrigNoOfSites := FOrigNoOfSites;
      end;
      modelName := AModel.GetDescription;
      if AModel is TProteinMatrixModel then
        if TProteinMatrixModel(AModel).UseFreq then
          modelName := modelName + ' + F';

      if Terminated then
        FCancelled := True;
      if FCancelled then
      begin
        Exit;
      end
      else
      begin
        AModel.SetParamsFromSeqs(SeqData);
        try
          try
            if FIsModelTamer then
              aMLTree := TMLTree.CreateWithCompressedData(aModel.OrigNoOfSites, SeqData, InitTree, aModel, False)
            else
              AMLTree := TMLTree.Create(SeqData, InitTree, AModel, False);
            if Assigned(ProgressDlg) then
              ProgressDlg.RegisterCancellable(aMLTree);
            AMLTree.HideSubtaskProgress := True;
            AMLTree.SubtaskCheckCancel := CheckCancelProxy;
            AMLTree.NoOfThreads := 1;
            AMLTree.Initialize(true, true);

            if AMLTree.Optimize(CheckCancelProxy, '', false) then
              GetModelInfo(MainThread.ModelInfoList[mindex], AMLTree)
            else
            begin
              FCancelled := True;
              MainThread.Cancelled := True;
              MainThread.Terminate;
              Terminate;
            end;
          except
            on E:EAbort do
            begin
              Terminate;
              FCancelled := True;
              MainThread.Cancelled := True;
              MainThread.Terminate;
            end;
          end;
        finally
          if Assigned(AMLTree) then
          begin
            if Assigned(ProgressDlg) then
              ProgressDlg.UnregisterCancellable(AMLTree);
            AMLTree.Free;
          end;
        end;
      end;
    end;
  until (AModel = nil) or Terminated or FCancelled;
end;

function TTestModelsChildThread.GetLogText: String;
begin
  if Assigned(MessagesLog) then
    Result := MessagesLog.Text
  else
    Result := EmptyStr;
end;

procedure TTestModelsChildThread.Execute;
begin
  try
    repeat
      TestModels;
      Done := true;
      MainThread.NotifyChildThreadDone(ThreadIndex);
      while not Terminated do
        sleep(100);
    until Terminated;
    if Assigned(MainThread) and Assigned(MainThread.ChildThread) and (ThreadIndex < Length(MainThread.ChildThread)) then
      MainThread.ChildThread[ThreadIndex] := nil;
  except
    on E:EAbort do
    begin
      FIsSuccess := False;
      FCancelled := True;
      Done := True;
      MessagesLog.Add(E.Message);
    end;
    on E:Exception do
    begin
      FIsSuccess := False;
      Done := True;
      MessagesLog.Add(E.Message);
    end;
  end;
end;

//////////////////

constructor TTestModelsThread.Create(seqs: TStringList; tree: TTreeData; m: TModelInfoList; n: integer; numThreads: Integer; aIsModelTamer: Boolean; aOrigNumSites: Integer; MaxPercentRam: Integer = DEFAULT_MAX_PERCENT_RAM);
var
  tempMemAvailable: Int64 = 0;
  i: integer;
  approximateRamUsage: Int64 = 0;
  nsites: Integer = -1;
  nrates: Integer = -1;
  aInfo: TModelInfo = nil;
begin
  Assert((MaxPercentRam > 25) and (MaxPercentRam <= 90), 'MaxPercentRam must be a value between 25 and 90');
  FCancelled := False;
  FMaxPercentAvailableRamToUse := MaxPercentRam;
  inherited Create(self, 0, seqs, tree, aIsModelTamer, aOrigNumSites);
  {$IFNDEF DEBUG}
  ThreadMemSize := GetApproximateSizeInBytes(tree, nsites, nrates, (n > 24));
  {$ELSE}
  ThreadMemSize := 1;
  {$ENDIF}
  FreeOnTerminate := false;
  ModelSelectionCS := TCriticalSection.Create;
  ModelInfoList := m;
  if ModelInfoList.Count > 0 then
    for i := 0 to ModelInfoList.Count-1 do
      ModelInfoList[i].Free;
  ModelInfoList.Clear;
  for i := 0 to n-1 do
  begin
    aInfo := TModelInfo.Create;
    {$IFDEF DEBUG}
    aInfo.DevConsoleProc := MyWriteToDevConsole;
    {$ENDIF}
    ModelInfoList.Add(aInfo);
  end;

  FNoOfThreads := numThreads;
  if FNoOfThreads > n then
    FNoOfThreads := n;

  approximateRamUsage := trunc(FNoOfThreads*ThreadMemSize);

  if GetAvailablePhysicalMemory(tempMemAvailable) then { hard to guarantee on Unix-based systems}
  begin
    tempMemAvailable := trunc(tempMemAvailable*MaxPercentRam / 100);
    if n > 24 then
      tempMemAvailable := tempMemAvailable - n*GetApproximateSizeOfProteinModelInBytes(nsites, nrates)
    else
      tempMemAvailable := tempMemAvailable - n*GetApproximateSizeOfDnaModelInBytes(nsites, nrates);

    if approximateRamUsage > tempMemAvailable then
      FNoOfThreads :=  Max(1, trunc(tempMemAvailable / ThreadMemSize));
  end;
  FNoOfModels := n;
  CurrentModelIndex := -1;

  setlength(ChildThread, FNoOfThreads);
  setlength(Models, n);
end;

destructor TTestModelsThread.Destroy;
var
  i: integer;
begin
  if Assigned(ModelSelectionCS) then
    ModelSelectionCS.Free;
  if Length(Models) > 0 then
  begin
    for i := 0 to  Length(Models) - 1 do
    begin
      if Assigned(Models[i]) then
      begin
        if Assigned(ProgressDlg) then
          ProgressDlg.UnregisterCancellable(Models[i]);
        Models[i].Free;
      end;
    end;
    setlength(Models, 0);
  end;
  setlength(ChildThread, 0);
  inherited;
end;

procedure TTestModelsThread.SetNoOfThreads(n: integer);
var
  tempMemAvailable: Int64 = 0;
begin
  if ChildThread[NoOfThreads-1] <> nil then exit;

  if n > NoOfModels then
    FNoOfThreads := NoOfModels
  else
    FNoOfThreads := n;

  if GetAvailablePhysicalMemory(tempMemAvailable) then
  begin
    if FNoOfThreads*ThreadMemSize > tempMemAvailable then
      FNoOfThreads :=  trunc(tempMemAvailable/ThreadMemSize);
  end
  else
    FNoOfThreads := 1;
  setlength(ChildThread, NoOfThreads);
end;

function TTestModelsThread.CheckChildThreadDone: boolean;
var
  i: integer;
begin
  result := true;
  for i := 1 to NoOfThreads-1 do
  begin
    if not ChildThread[i].IsSuccess then
      raise Exception.Create(Format('TTestModelsChildThread #%d failed with message: %s', [ChildThread[i].ThreadIndex, ChildThread[i].LogText]));
    result := result and ChildThread[i].Done;
  end;
end;

function TTestModelsThread.NumThreadsWorking: Integer;
var
  i: integer;
begin
  Result := 0;
  if not Done then
    Result := 1;
  for i := 1 to NoOfThreads-1 do
  begin
    if Assigned(ChildThread[i]) and (not ChildThread[i].Done) then
      inc(Result);
  end;
end;

function TTestModelsThread.GetModelIndex(childIndex: Integer): integer;
var
  aModel: TGammaRateVariationModel;
begin
  result := -1;

  inc(CurrentModelIndex);
  if CurrentModelIndex < NoOfModels then
  begin
    result := CurrentModelIndex;
    aModel := Models[result];
    FChildThreadIndex := childIndex;
    FModelName := aModel.GetDescription;
    if aModel is TProteinMatrixModel then
      if TProteinMatrixModel(aModel).UseFreq and (not FModelName.EndsWith('+F')) then
        FModelName := FModelName + ' + F';
    if Terminated then
      result := -1;
  end;
  if assigned(MyCheckCancel) then
    Synchronize(UpdateProgress);
end;

procedure TTestModelsThread.UpdateProgress;
var
  progress: Integer = 0;
  aStr: String = '';
begin
  if Terminated then Exit;
  progress := trunc((CurrentModelIndex - NumThreadsWorking)/NoOfModels*100);
  if progress < 0 then
    progress := trunc(CurrentModelIndex/NoOfModels*100);
  if Assigned(MyCheckCancel) then
  begin
    if Assigned(ChildThread[FChildThreadIndex]) and ChildThread[FChildThreadIndex].Done then
      aStr := 'Done'
    else
      aStr := FModelName;
    if MyCheckCancel(progress, Format('Thread-%d', [FChildThreadIndex + 1]), aStr) then
      Terminate;
  end;
end;

procedure TTestModelsThread.MyWriteToDevConsole(aMsg: String);
begin
  {$IFDEF DEBUG}
  FDevMsg := aMsg;
  Synchronize(DoWriteToDevConsole);
  {$ENDIF}
end;

procedure TTestModelsThread.DoWriteToDevConsole;
begin
  {$IFDEF DEBUG}{$IFDEF VISUAL_BUILD}
  WriteToDevConsole(FDevMsg);
  {$ENDIF}{$ENDIF}
end;

procedure TTestModelsThread.Execute;
var
  i: integer;
  flag: boolean;
begin
  Randomize;
  ChildThread[0] := self;
  for i := 1 to NoOfThreads-1 do
  begin
    ChildThread[i] := TTestModelsChildThread.Create(self, i, SeqData, InitTree, FIsModelTamer, FOrigNoOfSites);
    ChildThread[i].ProgressDlg := ProgressDlg;
  end;

  try
    try
      for i := 1 to NoOfThreads-1 do
        ChildThread[i].Start;
      TestModels;
      while (not Terminated) and (not CheckChildThreadDone) do
        sleep(100);
    except
      on E:EAbort do
      begin
        FIsSuccess := False;
        FCancelled := True;
        MessagesLog.Add(E.Message);
      end;
      on E:Exception do
      begin
        FIsSuccess := False;
        MessagesLog.Add(E.Message);
      end;
    end;
  finally
    for i := 1 to NoOfThreads-1 do
    begin
      ChildThread[i].Terminate;
      FCancelled := FCancelled or ChildThread[i].Cancelled;
    end;
    if not Terminated then
      repeat
        flag := true;
        for i := 1 to NoOfThreads-1 do
          flag := (flag and ChildThread[i].done);
        sleep(100);
      until flag;
  end;
  if Terminated then
  begin
    ReturnValue := 1;
    FIsSuccess := False;
    FCancelled := True;
  end
  else
    ReturnValue := 0;
  if NoOfThreads > 1 then
    for i := 1 to NoOfThreads - 1 do
      ChildThread[i].Free;
end;

procedure TTestModelsThread.NotifyChildThreadDone(index: Integer);
begin
  FChildThreadIndex := index;
  FModelName := 'Done';
  if assigned(MyCheckCancel) then
    Synchronize(UpdateProgress);
end;

procedure TMLTreeAnalyzer.TestDNAModels(numThreads: Integer; aBootTable: PArrayOfInt);
var
  aModels: TGammaRateVariationModelArray;
begin
  SetLength(aModels, 24);
  aModels[ 0] := TGTRModel.Create(0, False, 1);
  aModels[ 1] := TGTRModel.Create(0, True,  1);
  aModels[ 2] := TGTRModel.Create(0, False, FNoOfRates);
  aModels[ 3] := TGTRModel.Create(0, True, FNoOfRates);
  aModels[ 4] := TTN93Model.Create(0, False, 1);
  aModels[ 5] := TTN93Model.Create(0, True,  1);
  aModels[ 6] := TTN93Model.Create(0, False, FNoOfRates);
  aModels[ 7] := TTN93Model.Create(0, True, FNoOfRates);
  aModels[ 8] := THKYModel.Create(0, False, 1);
  aModels[ 9] := THKYModel.Create(0, True,  1);
  aModels[10] := THKYModel.Create(0, False, FNoOfRates);
  aModels[11] := THKYModel.Create(0, True, FNoOfRates);
  aModels[12] := TT3Model.Create(0, False, 1);
  aModels[13] := TT3Model.Create(0, True,  1);
  aModels[14] := TT3Model.Create(0, False, FNoOfRates);
  aModels[15] := TT3Model.Create(0, True, FNoOfRates);
  aModels[16] := TK2Model.Create(0, False, 1);
  aModels[17] := TK2Model.Create(0, True,  1);
  aModels[18] := TK2Model.Create(0, False, FNoOfRates);
  aModels[19] := TK2Model.Create(0, True, FNoOfRates);
  aModels[20] := TJCModel.Create(0, False, 1);
  aModels[21] := TJCModel.Create(0, True,  1);
  aModels[22] := TJCModel.Create(0, False, FNoOfRates);
  aModels[23] := TJCModel.Create(0, True, FNoOfRates);
  TestDNAModels(False, aModels, numThreads, aBootTable);
end;

procedure TMLTreeAnalyzer.TestDNAModels(isAdaptiveTest: Boolean; aModels: TGammaRateVariationModelArray;numThreads: Integer; aBootTable: PArrayoFInt);
var
  aInitTree: TTreeData = nil;
  i: Integer = -1;
  TestModelsThread: TTestModelsThread = nil;
begin
  try
    if Assigned(InitTree) then
    begin
      aInitTree := TTreeData.Create(InitTree.NoOfOTUs, InitTree.isBLen, InitTree.isSE, InitTree.isStats);
      aInitTree.Assign(InitTree);
    end
    else
      aInitTree := MakeNJTreeForModelTestOrLbsParamsEstimation(True);
    TestModelsThread := TTestModelsThread.Create(Seqs, aInitTree, ModelInfoList, Length(aModels), numThreads, FIsModelTamer, FOrigNoOfSites);
    RuntimeProgress.Thread := TestModelsThread;
    TestModelsThread.ProgressDlg := RuntimeProgress;
    for i := 0 to Length(aModels) - 1 do
      TestModelsThread.Models[i] := aModels[i];

    if FIsModelTamer then
    begin
      Assert(ModelTamerBootTable <> nil);
      for i := Low(TestModelsThread.Models) to High(TestModelsThread.Models) do
      begin
        TestModelsThread.Models[i].BootTable := ModelTamerBootTable;
        TestModelsThread.Models[i].OrigNoOfSites := FOrigNoOfSites;
      end;
    end;

    TestModelsThread.MyCheckCancel := RuntimeProgress.ProgressAndStatusCheckCancel;
    if isAdaptiveTest then
      TestModelsThread.ProgressMessage := 'Testing base DNA models'
    else
      TestModelsThread.ProgressMessage := 'Testing DNA models';
    TestModelsThread.Start;
    TestModelsThread.WaitFor;
    if isAdaptiveTest then
      SetLength(TestModelsThread.Models, 0); { so the models do not get freed by the thread}

    FCanceled := (TestModelsThread.ReturnValue = 1) or TestModelsThread.Cancelled;
    if (not TestModelsThread.IsSuccess) and (not TestModelsThread.Cancelled) then
      raise Exception.Create(TestModelsThread.LogText);

    if FCanceled then
      raise EAbort.Create('model selection analysis cancelled')
    else
      if assigned(CheckCancel) then CheckCancel(100, 'Finishing model test');
  finally
    if Assigned(TestModelsThread) then
      TestModelsThread.Free;
    if Assigned(aInitTree) then
      aInitTree.Free;
  end;
end;

procedure TMLTreeAnalyzer.TestProteinModels(numThreads: Integer; aBootTable: PArrayOfInt);
var
  aModels: TGammaRateVariationModelArray;
begin
  SetLength(aModels, 64);
  aModels[ 0] := TDayhoffModel.Create(0,false,false,1);
  aModels[ 1] := TDayhoffModel.Create(0, true,false, 1);
  aModels[ 2] := TDayhoffModel.Create(0, false,false, FNoOfRates);
  aModels[ 3] := TDayhoffModel.Create(0, true,false, FNoOfRates);
  aModels[ 4] := TDayhoffModel.Create(0,false,true,1);
  aModels[ 5] := TDayhoffModel.Create(0, true,true, 1);
  aModels[ 6] := TDayhoffModel.Create(0, false,true, FNoOfRates);
  aModels[ 7] := TDayhoffModel.Create(0, true,true, FNoOfRates);
  aModels[ 8] := TJTTModel.Create(0, false,false, 1);
  aModels[ 9] := TJTTModel.Create(0, true,false, 1);
  aModels[10] := TJTTModel.Create(0, false, false, FNoOfRates);
  aModels[11] := TJTTModel.Create(0, true, false, FNoOfRates);
  aModels[12] := TJTTModel.Create(0, false, true, 1);
  aModels[13] := TJTTModel.Create(0, true, true, 1);
  aModels[14] := TJTTModel.Create(0, false, true, FNoOfRates);
  aModels[15] := TJTTModel.Create(0, true, true, FNoOfRates);
  aModels[16] := TWAGModel.Create(0, false,false, 1);
  aModels[17] := TWAGModel.Create(0, true,false, 1);
  aModels[18] := TWAGModel.Create(0, false,false, FNoOfRates);
  aModels[19] := TWAGModel.Create(0, true,false, FNoOfRates);
  aModels[20] := TWAGModel.Create(0, false,true, 1);
  aModels[21] := TWAGModel.Create(0, true,true, 1);
  aModels[22] := TWAGModel.Create(0, false,true, FNoOfRates);
  aModels[23] := TWAGModel.Create(0, true,true, FNoOfRates);
  aModels[24] := TLGModel.Create(0, false,false, 1);
  aModels[25] := TLGModel.Create(0, true,false, 1);
  aModels[26] := TLGModel.Create(0, false,false, FNoOfRates);
  aModels[27] := TLGModel.Create(0, true,false, FNoOfRates);
  aModels[28] := TLGModel.Create(0, false,true, 1);
  aModels[29] := TLGModel.Create(0, true,true, 1);
  aModels[30] := TLGModel.Create(0, false,true, FNoOfRates);
  aModels[31] := TLGModel.Create(0, true,true, FNoOfRates);
  aModels[32] := TmtREV24Model.Create(0, false, false, 1);
  aModels[33] := TmtREV24Model.Create(0, true, false, 1);
  aModels[34] := TmtREV24Model.Create(0, false, false, FNoOfRates);
  aModels[35] := TmtREV24Model.Create(0, true, false, FNoOfRates);
  aModels[36] := TmtREV24Model.Create(0, false, true, 1);
  aModels[37] := TmtREV24Model.Create(0, true, true, 1);
  aModels[38] := TmtREV24Model.Create(0, false, true, FNoOfRates);
  aModels[39] := TmtREV24Model.Create(0, true, true, FNoOfRates);
  aModels[40] := TcpREVModel.Create(0, false, false, 1);
  aModels[41] := TcpREVModel.Create(0, true, false, 1);
  aModels[42] := TcpREVModel.Create(0, false, false, FNoOfRates);
  aModels[43] := TcpREVModel.Create(0, true, false, FNoOfRates);
  aModels[44] := TcpREVModel.Create(0, false, true, 1);
  aModels[45] := TcpREVModel.Create(0, true, true, 1);
  aModels[46] := TcpREVModel.Create(0, false, true, FNoOfRates);
  aModels[47] := TcpREVModel.Create(0, true, true, FNoOfRates);

  aModels[48] := TrtREVModel.Create(0, false, false, 1);
  aModels[49] := TrtREVModel.Create(0, true, false, 1);
  aModels[50] := TrtREVModel.Create(0, false, false, FNoOfRates);
  aModels[51] := TrtREVModel.Create(0, true, false, FNoOfRates);
  aModels[52] := TrtREVModel.Create(0, false, true, 1);
  aModels[53] := TrtREVModel.Create(0, true, true, 1);
  aModels[54] := TrtREVModel.Create(0, false, true, FNoOfRates);
  aModels[55] := TrtREVModel.Create(0, true, true, FNoOfRates);

  aModels[56] := TPoissonModel.Create(0, false, false, 1);
  aModels[57] := TPoissonModel.Create(0, true, false, 1);
  aModels[58] := TPoissonModel.Create(0, false, false, FNoOfRates);
  aModels[59] := TPoissonModel.Create(0, true, false, FNoOfRates);
  aModels[60] := TPoissonModel.Create(0, false, true, 1);
  aModels[61] := TPoissonModel.Create(0, true, true, 1);
  aModels[62] := TPoissonModel.Create(0, false, true, FNoOfRates);
  aModels[63] := TPoissonModel.Create(0, true, true, FNoOfRates);

  // aModels[].SetParamsFromSeqs must be called for using the +F option.

  TestProteinModels(False, aModels, numThreads, aBootTable);
end;

procedure TMLTreeAnalyzer.TestProteinModels(isAdaptiveTest: Boolean; aModels: TGammaRateVariationModelArray; numThreads: Integer; aBootTable: PArrayOfInt);
var
  TestModelsThread: TTestModelsThread = nil;
  aInitTree: TTreeData = nil;
  i: Integer = -1;
begin
  try
    if Assigned(InitTree) then
    begin
      aInitTree := TTreeData.Create(InitTree.NoOfOTUs, InitTree.isBLen, InitTree.isSE, InitTree.isStats);
      aInitTree.Assign(InitTree);
    end
    else
      aInitTree := MakeNJTreeForModelTestOrLbsParamsEstimation(False);
    TestModelsThread := TTestModelsThread.Create(Seqs, aInitTree, ModelInfoList, Length(aModels), numThreads, FIsModelTamer, FOrigNoOfSites);
    RuntimeProgress.Thread := TestModelsThread;

    TestModelsThread.ProgressDlg := RuntimeProgress;
    for i := 0 to Length(aModels) - 1 do
      TestModelsThread.Models[i] := aModels[i];

    if FIsModelTamer then
    begin
      Assert(ModelTamerBootTable <> nil);
      for i := Low(TestModelsThread.Models) to High(TestModelsThread.Models) do
      begin
        TestModelsThread.Models[i].BootTable := ModelTamerBootTable;
        TestModelsThread.Models[i].OrigNoOfSites := FOrigNoOfSites;
      end;
    end;

    for i := Low(TestModelsThread.Models) to High(TestModelsThread.Models) do
      if TestModelsThread.Models[i] is TProteinMatrixModel then
        with TestModelsThread.Models[i] as TProteinMatrixModel do
          if UseFreq then
            SetParamsFromSeqs(Seqs);
    TestModelsThread.MyCheckCancel := RuntimeProgress.ProgressAndStatusCheckCancel;
    if isAdaptiveTest then
      TestModelsThread.ProgressMessage := 'Testing base protein models'
    else
      TestModelsThread.ProgressMessage := 'Testing protein models';

    TestModelsThread.Start;
    TestModelsThread.WaitFor;
    if isAdaptiveTest then
      SetLength(TestModelsThread.Models, 0);

    FCanceled := (TestModelsThread.ReturnValue = 1) or TestModelsThread.Cancelled;
    if (not TestModelsThread.IsSuccess) and (not TestModelsThread.Cancelled) then
      raise Exception.Create(TestModelsThread.LogText);

    if FCanceled then
      raise EAbort.Create('model selection analysis cancelled')
    else
      if assigned(CheckCancel) then CheckCancel(100, 'Finishing model test');
  finally
    if Assigned(TestModelsThread) then
      TestModelsThread.Free;
    if Assigned(aInitTree) then
      aInitTree.Free;
  end;
end;

procedure TMLTreeAnalyzer.SetTreeData(tree: TTreeData);
begin
  if tree.NoOfOTUs <> NoOfSeqs then
    exit;

  if InitTree <> nil then
    InitTree.Free;

  InitTree := tree;
  InitTree.IsBLen := true;
//  if InitTree.SBL < 0.000000000001 then
    ComputeInitialBLens(2);

  if MLTree <> nil then
    MLTree.InitNodeData(InitTree);
end;

procedure TMLTreeAnalyzer.GetTreeData(tree: TTreeData);
begin
  if MLTree <> nil then
    MLTree.GetTreeData(tree);
end;

procedure TMLTreeAnalyzer.ComputeInitialBLens(method: integer);
var
  D:  PDistanceMatrix;
  MPTree: TMPTree;
  b0: array of Extended;
  L0,L1: Extended;
  r: extended;
  n,i: integer;
begin
  if InitTree = nil then
    exit;
  {$IFDEF DEBUG}WriteToDevLog('begin TMLTreeAnalyzer.ComputeInitialBlens');{$ENDIF}
  setlength(b0, 2*NoOfSeqs-1);

  D      := nil;
  MPTree := nil;
  try
    if method = 1 then
    begin
      InitTree.isBLen := true;
      {$IFDEF DEBUG}WriteToDevLog('begin TMPTree.ComputeApproxBlens');{$ENDIF}
      MPtree := TMPTree.Create(SeqData, Model.NoOfStates);
      MPTree.ComputeApproxBLens(InitTree);
      InitialBlenMethod := 1;
      {$IFDEF DEBUG}WriteToDevLog('end TMPTree.ComputeApproxBlens');{$ENDIF}
    end
    else if method = 2 then
    begin
      r := Model.SeqCoverageRate(seqs);
      n := Model.EffectiveNoOfSites(seqs);

      if not Model.Initialized then
        Model.SetParamsFromSeqs(Seqs);

      D  := CreateDistanceMatrix(NoOfSeqs);
      if r < 0.95 then
      begin
        if not Model.ComputePDistanceMatrix(seqs, D) then
          CorrectDistanceMatrix(D, NoOfSeqs);
      end
      else
      if n < 500 then
      begin
        if not Model.ComputePDistanceMatrix(seqs, D) then
          CorrectDistanceMatrix(D, NoOfSeqs);
      end
      else
      begin
        if not Model.ComputePDistanceMatrix(seqs, D) then
          CorrectDistanceMatrix(D, NoOfSeqs);
      end;
      CheckAbort;

      FastOLSBranchLength(InitTree, D);
      for i := 0 to 2*NoOfSeqs-2 do
        if InitTree.BLen[i] < 0 then
          InitTree.BLen[i] := 0;

      InitialBlenMethod := 2;
    end;

    CheckAbort;
    if not assigned(MLTree) then
    begin
      if FIsModelTamer or FIsLittleBootstrap then
        MLTree := TMLTree.CreateWithCompressedData(FOrigNoOfSites, Seqs, InitTree, FModel, FNeedsRootByOutgroup)
      else
        MLTree := TMLTree.Create(Seqs, InitTree, FModel, FNeedsRootByOutgroup);
      MLTree.SubtaskCheckCancel := SubTaskCheckCancel;
      MLTree.NoOfThreads := NoOfThreadsToUse; // GS - let the user decide
//      MLTree.deltaL  := FdeltaL;
//      MLTree.MaxIter := FMaxIter;
    end
    else
      MLTree.InitNodeData(InitTree);
  finally
    if Assigned(D) then
      DestroyDistanceMatrix(D, NoOfSeqs);
    if Assigned(MPtree) then
      MPTree.Free;
    setlength(b0, 0);
  end;
end;

procedure TMLTreeAnalyzer.InitializeBLens(mode: integer);
var
  D:  PDistanceMatrix;
  MPTree: TMPTree;
  tmpTreeData: TTreeData;
  r: extended;
  n,i: integer;
begin
  if MLTree = nil then
    exit;
  {$IFDEF DEBUG}WriteToDevLog('begin TMLTreeAnalyzer.ComputeInitialBlens');{$ENDIF}

  if mode = 0 then
    if InitialBlenMethod = 0 then
      exit
    else
      mode := InitialBlenMethod;

  tmpTreeData := nil;
  D      := nil;
  MPTree := nil;
  try
    tmpTreeData := TTreeData.Create(SeqData.Count, true, false, false, false);
    MLTree.GetTreeData(tmpTreeData);
    if mode = 1 then
    begin
      {$IFDEF DEBUG}WriteToDevLog('begin TMPTree.ComputeApproxBlens');{$ENDIF}
      MPtree := TMPTree.Create(SeqData, Model.NoOfStates);
      MPTree.ComputeApproxBLens(tmpTreeData);
      InitialBlenMethod := 1;
      {$IFDEF DEBUG}WriteToDevLog('end TMPTree.ComputeApproxBlens');{$ENDIF}
    end
    else if mode = 2 then
    begin
      {$IFDEF DEBUG}WriteToDevLog('begin ComputeOLSBranchLengths');{$ENDIF}
//      D  := CreateDistanceMatrix(NoOfSeqs);
//      if not Model.ComputePDistanceMatrix(seqs, D) then
//        CorrectDistanceMatrix(D, NoOfSeqs);
//      CheckAbort;

      r := Model.SeqCoverageRate(seqs);
      n := Model.EffectiveNoOfSites(seqs);

      if not Model.Initialized then
//        Model.InitGammaParamByParsimony(Seqs, tmpTreeData);
        Model.SetParamsFromSeqs(Seqs);

      D  := CreateDistanceMatrix(NoOfSeqs);

      if r < 0.95 then
      begin
        if not Model.ComputePDistanceMatrix(seqs, D) then
          CorrectDistanceMatrix(D, NoOfSeqs);
      end
      else
      if n < 500 then
      begin
        if not Model.ComputePDistanceMatrix(seqs, D) then
          CorrectDistanceMatrix(D, NoOfSeqs);
      end
      else
      begin
        if not Model.ComputePDistanceMatrix(seqs, D) then
          CorrectDistanceMatrix(D, NoOfSeqs);
      end;

      CheckAbort;

      FastOLSBranchLength(tmpTreeData, D);

      for i := 0 to 2*NoOfSeqs-2 do
        if tmpTreeData.BLen[i] < 0 then
          tmpTreeData.BLen[i] := 0;

      InitialBlenMethod := 2;
     {$IFDEF DEBUG}WriteToDevLog('end ComputeOLSBranchLengths');{$ENDIF}
    end;
    CheckAbort;
    MLTree.SetBLens(tmpTreeData);
  finally
    if Assigned(tmpTreeData) then
      tmpTreeData.Free;
    if Assigned(D) then
      DestroyDistanceMatrix(D, NoOfSeqs);
    if Assigned(MPtree) then
      MPTree.Free;
  end;
end;

procedure TMLTreeAnalyzer.TieProc(var c1,c2: integer; c: array of Integer);
var
  i,j,k,d,mind: integer;
  n1: Char = #0;
  n2: char = #0;
  flag: boolean;
begin
  mind := -1;
  for i := 1 to c[0]-1 do
    for j := i+1 to c[0] do
    begin
      d := 0;
      if Model.SeqDataType = DNA then
      begin
        flag := true;
        for k := 1 to NoOfSites do
        begin
          case upcase(seqs[c[i]][k]) of
            'A',
            'T',
            'U',
            'C',
            'G': n1 := upcase(seqs[c[i]][k]);
          else
            flag := false;
          end;
          case upcase(seqs[c[j]][k]) of
            'A',
            'T',
            'U',
            'C',
            'G': n2 := upcase(seqs[c[j]][k]);
          else
            flag := false;
          end;
          if flag then
            if n1 <> n2 then
              inc(d);
        end;
      end
      else if Model.SeqDataType = Protein then
      begin
        flag := true;
        for k := 1 to NoOfSites do
        begin
          case upcase(seqs[c[i]][k]) of
            'A',
            'C',
            'D',
            'E',
            'F',
            'G',
            'H',
            'I',
            'K',
            'L',
            'M',
            'N',
            'P',
            'Q',
            'R',
            'S',
            'T',
            'V',
            'W',
            'Y': n1 := upcase(seqs[c[i]][k]);
          else
            flag := false
          end;
          case upcase(seqs[c[j]][k]) of
            'A',
            'C',
            'D',
            'E',
            'F',
            'G',
            'H',
            'I',
            'K',
            'L',
            'M',
            'N',
            'P',
            'Q',
            'R',
            'S',
            'T',
            'V',
            'W',
            'Y': n2 := upcase(seqs[c[j]][k]);
          else
            flag := false
          end;
          if flag then
            if n1 <> n2 then
              inc(d);
        end;
      end;
      if (mind < 0) or (d < mind) then
      begin
        c1 := i;
        c2 := j;
        mind := d;
      end;
    end;
end;

function TMLTreeAnalyzer.TimeTreeToNewickString(OtuNames: TStringList; UseTimeFactor: Boolean): String;
begin
  Result := MLTree.ReltimeComputer.TimeTreeToNewickString(OtuNames, UseTimeFactor);
end;

procedure TMLTreeAnalyzer.SetIsLittleBootstrap(aIsLittleBootstrap: Boolean; aNumTotalSites: Int64);
begin
  FIsLittleBootstrap := aIsLittleBootstrap;
  FNumTotalSites := aNumTotalSites;
end;

{TInitMPTreeSearchChildThread}

type
TInitMPTreeSearchThread = class;

TInitMPTreeSearchChildThread = class(TThread)
protected
  FErrorMsg: String;
  FIsSuccess: Boolean;
  Seqs: TStringList;
  NoOfBits: integer;
  NoOfSeqs: integer;
  tree: TTreeData;

  FThreadIndex: Integer;
  FMainThread: TInitMPTreeSearchThread;
  procedure Execute; override;
  function GenerateMpTreeSearcher: TMPTree;
public
  MPTree: TMPTree;
  property MainThread: TInitMPTreeSearchThread read FMainThread;
  property ThreadIndex: Integer read FThreadIndex;

  constructor Create(AMainThread: TInitMPTreeSearchThread;
                     AThreadIndex: integer;
                     ASeqData: TStringList;
                     ANoOfBits: integer);
  destructor Destroy; override;
  property IsSuccess: Boolean read FIsSuccess;
  property ErrorMsg: String read FErrorMsg;
end;

TInitMPTreeSearchThread = class(TInitMPTreeSearchChildThread)
  RandNum: T2DArrayOfInteger;
private
  FMaxNoOfTrees: integer;
  FNoOfThreads: integer;
  CurNoOfTrees: integer;
  ChildThread: array of TInitMPTreeSearchChildThread;
  InitMPTrees: TPartitionList;
protected
  LockThread: TCriticalSection;

  CheckCancel: TCheckCancelFunc;

  procedure Execute; override;

  function CheckChildThreadDone: Boolean;
public
  constructor Create(APartitionList: TPartitionList;
                     Seqs: TStringList;
                     NoOfBits: integer;
                     numTrees: integer;
                     numThreads: integer);
  destructor Destroy; override;
  procedure AddMPTree(tree: TTreeData);
  property MaxNoOfTrees: integer read FMaxNoOfTrees;
  property NoOfThreads: Integer read FNoOfThreads;
end;

{TInitMPTreeSearchChildThread}

constructor TInitMPTreeSearchChildThread.Create(
                     AMainThread: TInitMPTreeSearchThread;
                     AThreadIndex: integer;
                     ASeqData: TStringList;
                     ANoOfBits: integer);
begin
  inherited Create(false);
  FreeOnTerminate := false;
  FIsSuccess := True;
  FErrorMsg := EmptyStr;
  FMainThread  := AMainThread;
  FThreadIndex := AThreadIndex;
  Seqs := ASeqData;
  NoOfSeqs := Seqs.Count;
  NoOfBits := ANoOfBits;

  tree := TTreeData.Create(NoOfSeqs, true, false, false);
  MPTree := GenerateMpTreeSearcher;
end;

destructor TInitMPTreeSearchChildThread.Destroy;
begin
  tree.Free;

  inherited Destroy;
end;

procedure TInitMPTreeSearchChildThread.Execute;
begin
  try
    try
      MPTree.RandomArray := MainThread.RandNum[ThreadIndex];
      MPTree.SearchMPTree;
      MPTree.GetTreeData(tree, 0);
      MainThread.AddMPTree(tree);
      FreeAndNil(MPTree);
      repeat
        ThreadSwitch;
      until Terminated;
    except
      on E:Exception do
      begin
        FIsSuccess := False;
        FErrorMsg := E.Message;
      end;
    end;
  finally
    if Assigned(MPTree) then
      FreeAndNil(MPTree);
    Terminate;
  end;
end;

function TInitMPTreeSearchChildThread.GenerateMpTreeSearcher: TMPTree;
begin
  Result := TMPTree.Create(Seqs, NoOfBits);
  Result.SearchMethod  := SPR;
  Result.RandomOrder   := true;
  Result.NoOfInitTrees := 1;
  Result.SearchLevel   := 5;
  Result.MaxNoOfTrees  := 1;
end;

{TInitMPTreeSearchThread}

constructor TInitMPTreeSearchThread.Create(
                     APartitionList: TPartitionList;
                     Seqs: TStringList;
                     NoOfBits: integer;
                     numTrees: integer;
                     numThreads: integer);
begin
  inherited Create(self, 0, Seqs, NoOfBits);
  LockThread  := TCriticalSection.Create;
  InitMPTrees   := APartitionList;
  FMaxNoOfTrees := numTrees;
  FNoOfThreads  := numThreads;
  CurNoOfTrees  := 0;
  setlength(ChildThread, NoOfThreads);
end;

destructor TInitMPTreeSearchThread.Destroy;
begin
  LockThread.Free;
  setlength(ChildThread, 0);
  inherited;
end;

procedure TInitMPTreeSearchThread.AddMPTree(tree: TTreeData);
begin
  try
    LockThread.Acquire;
    InitMPTrees.Add(tree.NodeArray, 0.0);
    inc(CurNoOfTrees);
  finally
    LockThread.Release;
  end;
  if assigned(CheckCancel) then
    CheckCancel(CurNoOfTrees*10,'Making MP trees...');
end;

function TInitMPTreeSearchThread.CheckChildThreadDone: boolean;
var
  i: integer;
begin
  Result := True;
  if NoOfThreads > 1 then
    for i := 0 to NoOfThreads-1 do
      Result := Result and (ChildThread[i].MPTree = nil);
end;

procedure TInitMPTreeSearchThread.Execute;
var
  i: integer;
begin
  MPTree := GenerateMpTreeSearcher;
  MPTree.RandomArray := RandNum[0];

  ChildThread[0] := self;

  try
    try
      for i := 1 to NoOfThreads-1 do
      begin
        ChildThread[i] := TInitMPTreeSearchChildThread.Create(self, i, Seqs, NoOfBits);
        ChildThread[i].Start;
      end;

      MPTree.SearchMPTree;
      MPTree.GetTreeData(tree, 0);
      AddMPTree(tree);

      FreeAndNil(MPTree);

      while not CheckChildThreadDone do
        ThreadSwitch;

      { exceptions DO NOT propagate from child threads to parent threads, they just disappear
        if not handled. The code for child threads has been updated to catch exceptions and
        save the error messsages - the code below checks if we have errors and if so, saves
        them in this thread (they can then be propagated further up the thread hierarchy, all
        the way to the GUI.}
      for i := 1 to NoOfThreads - 1 do
        if not ChildThread[i].IsSuccess then
          raise Exception.Create(ChildThread[i].ErrorMsg);
    except
      on E:Exception do
      begin
        FIsSuccess := False;
        FErrorMsg := E.Message;
      end;
    end;
  finally
    for i := 1 to NoOfThreads - 1 do
      ChildThread[i].Terminate;
    Terminate;
  end;
end;

procedure TMLTreeAnalyzer.MakeInitialTree(method: integer);

  function MakeParsimonyTree(treedata: TTreeData; ARandNum: TIntArray): boolean;
  var
    MPTree: TMPTree;
    i: integer;
  begin
    result := false;
    MPTree := nil;
    try
      if Model.SeqDataType = DNA then
        MPTree := TMPTree.Create(Seqs, 8)
      else
        MPTree := TMPTree.Create(Seqs, 32);
      if Model.BootTable <> nil then
      begin
        MPTree.SearchMethod  := SPR;
        MPTree.RandomOrder   := true;
        MPTree.RandomArray   := ARandNum;
        MPTree.NoOfInitTrees := 1;
        MPTree.SearchLevel   := 5;
        MPTree.MaxNoOfTrees  := 1;
        MPTree.SetFreqTable(Model.BootTable);
        MPTree.SearchMPTree;
      end
      else
      begin
        MPTree.SearchMethod  := SPR;
        MPTree.RandomOrder   := true;
        MPTree.RandomArray   := ARandNum;
        MPTree.NoOfInitTrees := 1;
        MPTree.SearchLevel   := 5;
        MPTree.MaxNoOfTrees  := 1;
        MPTree.SearchMPTree;
      end;
      treedata.isBLen := true;
      MPTree.GetTreeData(treedata, 0);

//      MPTree.ComputeApproxBLens(InitTree);

      result := true;
    finally
      if assigned(MPTree) then
        FreeAndNil(MPTree);
    end;
  end;

var
  D: PDistanceMatrix = nil;
  tmpMPTree: TMPTree = nil;
  tmpModel: TGammaRateVariationModel = nil;
  tmpMLTree: TMLTree = nil;
  i,j,n: integer;
  nbits: Integer = -1;
  t0: TTreeData = nil;
  t1: TTreeData = nil;
  L0,L1,r: extended;
  bootstrap: boolean = False;
  InitialMPTrees: TPartitionList = nil;
  MPTreeSearchThread: TInitMPTreeSearchThread = nil;
  RandNum: T2DArrayOfInteger = nil;
begin
  {$IFDEF DEBUG}SaveMLlog(' MLTreeAnalyzer.MakeInitialTree started');{$ENDIF}
  if method = UserProvidedInitTree then
    raise Exception.Create('Developer error: should not generate an initial tree when the user has already provided one');
  if method = MultipleMPTreesMethod then
    raise Exception.Create('Developer error: call to wrong initial trees procedure - use the MakeParsimonyTrees function instead');
  if assigned(CheckCancel) then
    CheckCancel(0, 'Preparing initial tree');

  try
    if InitTree = nil then
      InitTree := TTreeData.Create(NoOfSeqs,true,true,false);

    {$IFDEF DEBUG}SaveMLlog(' tmpMPTree := TMPTree.Create started');{$ENDIF}
    if Model.SeqDataType = DNA then
      tmpMPTree := TMPTree.Create(Seqs, 8)
    else
      tmpMPTree := TMPTree.Create(Seqs, 32);
    FNoOfInfoSites := tmpMPTree.NoOfInfoSites;
    {$IFDEF DEBUG}SaveMLlog(' tmpMPTree := TMPTree.Create ended');{$ENDIF}

    if (method = MPInitTreeMethod) or (method = DefaultInitTreeMethod) then
    begin
      if assigned(CheckCancel) then
        CheckCancel(0, 'Making initial MP trees');
      t0 := TTreeData.Create(NoOfSeqs,true,true,false);
      InitialMPTrees := TPartitionList.Create(NoOfSeqs, 10, false);

     RandSeed := MPSeed;
     setlength(RandNum, 10);
     for i := 0 To 9 do
      begin
        SetLength(RandNum[i], NoOfSeqs);
        RandNum[i][0] := 0;
        for j := NoOfSeqs-1 downto 1 do
          RandNum[i][j] := Random(j+1);
      end;

////  Multi-thread for 10 MP trees
      if Model.SeqDataType = DNA then
        nbits := 8
      else
        nbits := 32;
      MPTreeSearchThread := TInitMPTreeSearchThread.Create(InitialMPTrees, Seqs, nbits, 1, 10);
      MPTreeSearchThread.CheckCancel := SubtaskCheckCancel;
      MPTreeSearchThread.RandNum := RandNum;
      MPTreeSearchThread.Start;
      MPTreeSearchThread.WaitFor;

      { exceptions in child threads DO NOT propagate, they just disappear (with
        unpredicable behavior) if not handled. So we have to check the thread
        here for errors after it completes and if there were errors, pass the
        message along as there is no good way to handle errors here.}
      if not MPTreeSearchThread.IsSuccess then
        raise Exception.Create(MPTreeSearchThread.ErrorMsg);
////
////   Single-tread for 10 MP trees
{
      for i := 0 to 9 do
      begin
        if assigned(SubtaskCheckCancel) then
          SubtaskCheckCancel(i*10, 'Making MP trees...');

        if MakeParsimonyTree(t0, RandNum[i]) then
          InitialMPTrees.Add(t0.NodeArray, 0.0);

        if assigned(CheckCancel) then
          CheckCancel(i+1, 'Making initial MP trees');
      end;
}
////

      n := InitialMPTrees.NoOfTrees;

      t1 := InitialMPTrees.ExtractTreeData(0);
      t0.Assign(t1);
      FreeAndNil(t1);

      L0 := tmpMPTree.ComputeApproxBLens(t0);
      InitialBlenMethod := 1;

      if n > 1 then
        for i := 1 to n-1 do
        begin
          t1 := InitialMPTrees.ExtractTreeData(i);
          L1 := tmpMPTree.ComputeApproxBLens(t1);
          If L1 < L0 then
          begin
            t0.Assign(t1);
            L0 := L1;
          end;
          FreeAndNil(t1);
        end;
      if method = MPInitTreeMethod then
      begin
        InitTree.Assign(t0);
        if assigned(SubtaskCheckCancel) then
          SubtaskCheckCancel(0, 'Optimizing...');
        exit;
      end;
    end;
    if (method = NJInitTreeMethod) or (method = DefaultInitTreeMethod) then
    begin
      if assigned(CheckCancel) then
        if method = DefaultInitTreeMethod then
          CheckCancel(10, 'Making initial NJ tree')
        else
          CheckCancel(0, 'Making initial NJ tree');
      if assigned(SubtaskCheckCancel) then
        SubtaskCheckCancel(0, 'Making NJ tree...');

      {$IFDEF DEBUG}SaveMLlog(' FModel.SeqCoverageRate started');{$ENDIF}
      r := FModel.SeqCoverageRate(seqs);
      {$IFDEF DEBUG}SaveMLlog(' FModel.SeqCoverageRate ended');{$ENDIF}
      {$IFDEF DEBUG}SaveMLlog(' FModel.EffectiveNoOfSites started');{$ENDIF}
      n := FModel.EffectiveNoOfSites(seqs);
      {$IFDEF DEBUG}SaveMLlog(' FModel.SeqCoverageRate ended');{$ENDIF}

      {$IFDEF DEBUG}SaveMLlog(' tmpModel := CopyModel(FModel) started');{$ENDIF}
      tmpModel := CopyModel(FModel);
//      tmpModel.SiteCoverage := FModel.SiteCoverage;
//      tmpModel.SetParamsFromSeqs(Seqs);
      tmpModel.NoOfThreads := FNoOfThreadsToUse;

      tmpModel.BootTable := Model.BootTable;
      bootstrap := Model.BootTable <> nil;
      {$IFDEF DEBUG}SaveMLlog(' tmpModel := CopyModel(FModel) ended');{$ENDIF}

      D  := CreateDistanceMatrix(NoOfSeqs);

      if r < 0.95 then
      begin
       FInitialNJTreeModelUsed := gdPropDist;
        if not tmpModel.ComputePDistanceMatrix(seqs, D) then
          CorrectDistanceMatrix(D, NoOfSeqs);
      end
      else
      if n < 500 then
      begin
       FInitialNJTreeModelUsed := gdPropDist;
        if not tmpModel.ComputePDistanceMatrix(seqs, D) then
          CorrectDistanceMatrix(D, NoOfSeqs);
      end
      else
      begin
        {$IFDEF DEBUG}SaveMLlog(' ComputeDistanceMatrix started');{$ENDIF}
        FInitialNJTreeModelUsed := gdNone; { by setting to gdNone, the downstream code (caption) will use what ever model tmpModel is}
//        if not tmpModel.ComputeDistanceMatrix(seqs, D, false) then
        if not tmpModel.ComputePDistanceMatrix(seqs, D) then
          CorrectDistanceMatrix(D, NoOfSeqs);
        {$IFDEF DEBUG}SaveMLlog(' ComputeDistanceMatrix ended');{$ENDIF}
      end;

      t1 := TTreeData.Create(NoOfSeqs,true,false,false);
      MakeNJTree(t1, D, bootstrap);
//      L1 := tmpMPTree.ComputeApproxBLens(t1);

      InitialBlenMethod := 2;

      if assigned(SubtaskCheckCancel) then
        SubtaskCheckCancel(0, 'Optimizing...');

      if  method = DefaultInitTreeMethod then
      begin
        if assigned(CheckCancel) then
          CheckCancel(10, 'Comparing NJ and MP for initial tree');

        tmpMLTree := TMLTree.Create(seqs, t0, tmpModel, FNeedsRootByOutgroup);
        tmpMLTree.NoOfThreads := NoOfThreadsToUse;
        tmpMLTree.OptimizePrecision := 7;
        tmpMLTree.MaxIter := 5;
        tmpMLTree.Initialize(false, true);
        tmpMLTree.OptimizeAllBLens(nil, 0, 'Optimizing initial tree');
        L0 := tmpMLTree.LogLikelihood;
        tmpMLTree.GetTreeData(t0);

        if assigned(CheckCancel) then
          CheckCancel(15, 'Comparing NJ and MP for initial tree');

        tmpMLTree.InitNodeData(t1);
        tmpMLTree.Initialize(false, true);
        tmpMLTree.OptimizeAllBLens(nil, 0, 'Optimizing initial tree');

        L1 := tmpMLTree.LogLikelihood;
        tmpMLTree.GetTreeData(t1);
        if L0 > L1 then
        begin
          InitTree.Assign(t0);
          InitialBlenMethod := 1;
        end
        else
        begin
          InitTree.Assign(t1);
          InitialBlenMethod := 2;
        end;
        if assigned(CheckCancel) then
          CheckCancel(20, 'Comparing NJ and MP for initial tree');
      end
      else
      begin
        InitTree.Assign(t1);
        if assigned(CheckCancel) then
          CheckCancel(10, 'Making initial NJ tree');
      end;
    end;
  finally
    if assigned(tmpModel) then
      tmpModel.Free;
    if assigned(tmpMPTree) then
      tmpMPTree.Free;
    if assigned(tmpMLTree) then
      tmpMLTree.Free;
    if assigned(D) then
      DestroyDistanceMatrix(D,NoOfSeqs);
    if assigned(InitialMPTrees) then
      InitialMPTrees.Free;
    if assigned(t0) then
      FreeAndNil(t0);	  
    if assigned(t1) then
      FreeAndNil(t1);	  
    if length(RandNum) > 0 then
    begin
      for i := 0 to 9 do
        SetLength(RandNum[i], 0);
      setlength(RandNum, 0);
    end;
  end;
  {$IFDEF DEBUG}SaveMLlog(' MLTreeAnalyzer.MakeInitialTree ended');{$ENDIF}
end;

function TMLTreeAnalyzer.MakeParsimonyTrees(const numTrees: Integer; var aTreeList: TTreeList; var aLog: TStringList): String;
const
  STATUS_STR = 'Initial Trees';
var
  rfCoalesced: Boolean = False;
  avgRfDists: array of Double;
  numIdentical: Integer = 0;
  aMaxTopologies: Int64 = -1;
  newTrees: TPartitionList = nil;
  MPTree: TMPTree = nil;
  aMLTree: TMLTree = nil;
  i: Integer = -1;
  aData: TMPMLTreeData = nil;
  allParsimTrees: TStringList = nil;
  {$IFNDEF VISUAL_BUILD}
  exportOptions: TNewickExportOptions;
  {$ENDIF}

  procedure AddToLog(aStr: String);
  begin
    aLog.Add(Format('%-20s %s', [FormatDateTime('YYYY-MM-DD hh:mm:ss.zzz', Now), aStr]));
  end;

  function IntArrayToCsvString(ints: ArrayOfInteger): String;
  var
    j: Integer = -1;
  begin
    Result := EmptyStr;
    for j := Low(ints) to High(ints) do
      Result := Result + IntToStr(ints[j]) + ',';
  end;

  function TreeAlreadyInList(aTree: TTreeData): Boolean;
  var
    t: TTreeData = nil;
  begin
    try
      if aTreeList.Count = 0 then
        Exit(False);
      if not Assigned(newTrees) then
        newTrees := TPartitionList.Create(aTree.NoOfOTUs, aMaxTopologies, False);
      t := aTree.Clone;
      Result := (not newTrees.AddIfNewTree(t.NodeArray, 0, 1));
    finally
      if Assigned(t) then
        t.Free;
    end;
  end;

  function UpdateAndCheckAvgRfDistsCoalesced: Boolean;
  var
    t1: TTreeData = nil;
    t2: TTreeData = nil;
    j: Integer = -1;
    k: Integer = -1;
    index: Integer = -1;
    tempDists: TExtendedList = nil;
    rfComputer: TRobinsonFouldsMetric = nil;
    rf: Double = -1;
  begin
    Result := False;
    if aTreeList.Count <= 1 then
      Exit;

    try
      tempDists := TExtendedList.Create;
      rfComputer := TRobinsonFouldsMetric.Create(False);
      index := Length(avgRfDists);
      SetLength(avgRfDists, index + 1);
      for j := 0 to aTreeList.Count - 2 do
      begin
        t1 := aTreeList[j];
        for k := j + 1 to aTreeList.Count - 1 do
        begin
          t2 := aTreeList[k];
          rf := rfComputer.Compute_RF_distance(t1, t2);
          tempDists.Add(rf);
        end;
      end;
      avgRfDists[index] := tempDists.GetMean;
      if Length(avgRfDists) >= 3 then
      begin
        Result := True;
        for j := High(avgRfDists) downto High(avgRfDists) - 1 do
        begin
          if CompareValue(avgRfDists[j], avgRfDists[j - 1], FP_CUTOFF) <> 0 then
            Result := False;
        end;
      end;
      if Result then
        AddToLog(Format('avg Robinson-Foulds distance between %d MP trees has coalesced', [aTreeList.Count]));
    finally
      if Assigned(tempDists) then
        tempDists.Free;
      if Assigned(rfComputer) then
        rfComputer.Free;
    end;
  end;


begin
  Result := EmptyStr;
  if Model.BootTable <> nil then
    raise Exception.Create('developer error: TMLTreeAnalyzer.MakeParsimonyTrees is not intended for bootstrap analyses');
  if Assigned(CheckCancel) then
    CheckCancel(0, 'Making Initial Parsimony Trees');

  SetLength(avgRfDists, 0);
  aTreeList.isBLen := True;
  aMaxTopologies := max(100, numTrees*2);
  AddToLog(Format('attempting to find %d MP start trees', [numTrees]));
  AddToLog(Format('maximum number of MP searches to try is %d', [aMaxTopologies]));
  i := 0;
  while (not rfCoalesced) and (aTreeList.Count < numTrees) and (i < aMaxTopologies) and (numIdentical < numTrees) do
  begin
    inc(i);
    try
      if Model.SeqDataType = DNA then
        MPTree := TMPTree.Create(Seqs, 8)
      else
        MPTree := TMPTree.Create(Seqs, 32);
      MPTree.SearchMethod := SPR;
      MPTree.NoOfInitTrees := 10;
      MPTree.RandomOrder := True;
      MPTree.SearchLevel := 5;
      MPTree.MaxNoOfTrees := 10;
      MPTree.ProgressMessage := 'Searching for initial MP trees';
      MPTree.SearchMPTree;
      aData := TMPMLTreeData.Create(Seqs.Count);
      if MPTree.NoOfTrees = 1 then
      begin
        MPTree.GetTreeData(TTreeData(aData), 0);
      end
      else
      begin
        AddToLog(Format('MP search found %d trees. Selecting one tree at random', [MPTree.NoOfTrees]));
        MPTree.GetTreeData(TTreeData(aData), Random(MPTree.NoOfTrees));
      end;

      if not TreeAlreadyInList(aData) then
      begin
        aData.SumOfBranchLengths := MPTree.ComputeApproxBLens(TTreeData(aData)); { do we really need blens at this point, or can we wait until we know for sure the tree will be used}
        aData.TreeLength := MPTree.TreeLength;
        aTreeList.Add(aData);
        AddToLog(Format('adding MP tree #%d', [aTreeList.Count]));
        if UpdateAndCheckAvgRfDistsCoalesced then
          rfCoalesced := True;
      end
      else
      begin
        AddToLog('skipping duplicate tree');
        inc(numIdentical);
        aData.Free;
      end;

      if Assigned(RunStatusProc) then
        RunStatusProc(STATUS_STR, Format('%d trees found and %d duplicates discarded', [aTreeList.Count, numIdentical]));
      if Assigned(SubTaskCheckCancel) then
        if SubTaskCheckCancel(Round(aTreeList.Count/numTrees*100), 'Making Initial Parsimony Trees') then
          raise EAbort.Create('user cancelled');
    finally
      if assigned(MPTree) then
        FreeAndNil(MPTree);
    end;
    if rfCoalesced then
      AddToLog('terminating generation of MP trees because avg RF distance has coalesced')
    else if aTreeList.Count = numTrees then
      AddToLog(Format('completed generation of %d MP trees', [numTrees]))
    else if i = aMaxTopologies then
      AddToLog(Format('terminating generation of MP trees because the max(%d) number of attemps have been made', [aMaxTopologies]))
    else if numIdentical = numTrees then
      AddToLog(Format('terminating generation of MP trees because %d searches produced identical trees', [numIdentical]));
  end;

  if numIdentical > 0 then
    AddToLog(Format('%d non-unique parsimony tree(s) were discarded', [numIdentical]));

  try
    aMLTree := TMLTree.Create(SeqData, aTreeList[0], Model);
    for i := 0 to aTreeList.Count - 1 do
    begin
      if Assigned(RunStatusProc) then
        RunStatusProc(STATUS_STR, Format('Analyzing Initial Parsimony Trees (%d/%d)', [i + 1, numTrees]));
      aData := TMPMLTreeData(aTreeList[i]);
      aMLTree.InitNodeData(aData);
      aMLTree.Initialize(False, true);
      aMLTree.OptimizeAllBLens(nil);
      aData.InitialLogLikelihood := aMLTree.LogLikelihood;
      if Assigned(SubTaskCheckCancel) then
        if SubTaskCheckCancel(Round((i + 1)/aTreeList.Count*100), 'Analyzing Initial Parsimony Trees') then
          raise EAbort.Create('user cancelled');
    end;
    Result := Format('%d random order MP trees were generated.', [aTreeList.Count]);
    if Assigned(RemoveStatusProc) then
      RemoveStatusProc(STATUS_STR);
    {$IFNDEF VISUAL_BUILD}
    exportOptions := GetDefaultNewickExportOptions(aTreeList);
    allParsimTrees := aTreeList.OutputAllNewickTrees(exportOptions, 0.0, [tdvTreeLength, tdvInitialLogLikelihood]);
    allParsimTrees.SaveToFile(NextAvailableFilenameNV('_all_start_MP_tree_candidates.nwk'));
    {$ENDIF}
  finally
    if Assigned(aMLTree) then
      aMLTree.Free;
    if Assigned(allParsimTrees) then
      allParsimTrees.Free;
    if Assigned(newTrees) then
      newTrees.Free;
  end;
end;

procedure TMLTreeAnalyzer.Assign(Source: TMLTreeAnalyzer);
var
  i: Integer;
begin
  FNeedsRootByOutgroup := Source.NeedsRootByOutgroup;
  FInitialNJTreeModelUsed := Source.InitialNJTreeModelUsed;
  SeqNames := TStringList.Create;
  if Assigned(Source.SeqNames) then
    SeqNames.Assign(Source.SeqNames);

  OptimizeMessage := Source.OptimizeMessage;
  if Assigned(Source.InitTree) then
  begin
    InitTree := TTreeData.Create(Source.InitTree.NoOfOtus, Source.InitTree.IsBlen, Source.InitTree.IsSE, Source.InitTree.IsStats);
    InitTree.Assign(Source.InitTree);
  end;
  SeqData := TStringList.Create;
  if Assigned(Source.SeqData) then
    SeqData.Assign(Source.SeqData);
  FModel := TGammaRateVariationModel.Create(Source.FModel.gamma, Source.FModel.UseInvar, Source.FModel.NoOfRates, Source.FModel.NoOfThreads);
  if Assigned(Source.MLTree) then
  begin
    if FIsModelTamer or FIsLittleBootstrap then
      MLTree := TMLTree.CreateWithCompressedData(FOrigNoOfSites, Seqs, InitTree, FModel, FNeedsRootByOutgroup)
    else
      MLTree := TMLTree.Create(SeqData, InitTree, FModel, FNeedsRootByOutgroup);
    MLTree.Assign(Source.MLTree);
  end;
  if Assigned(Source.ModelInfoList) then
  begin
    ModelInfoList := TModelInfoList.Create;
    ModelInfoList.Assign(Source.ModelInfoList);
  end;
  if Assigned(Source.NonClockTree) then
  begin
    NonClockTree := TTreeData.Create(Source.NonClockTree.NoOfOtus, Source.NonClockTree.IsBlen, Source.NonClockTree.IsSE, Source.NonClockTree.IsStats);
    NonClockTree.Assign(Source.NonClockTree);
  end;
  if Assigned(Source.NonClockModel) then
  begin
    NonClockModel := TGammaRateVariationModel.Create(Source.NonClockModel.gamma, Source.NonClockModel.UseInvar, Source.NonClockModel.NoOfRates, Source.NonClockModel.NoOfThreads);
    NonClockModel.Assign(Source.NonClockModel);
  end;
  SearchFilter := Source.SearchFilter;
  InitialTreeOption := Source.InitialTreeOption;
  if Length(Source.SamplingTime) > 0 then
  begin
    SetLength(SamplingTime, Length(Source.SamplingTime));
    for i := 0 to Length(SamplingTime) - 1 do
      SamplingTime[i] := Source.SamplingTime[i];
  end;
  FNoOfThreadsToUse := Source.FNoOfThreadsToUse;
  FNoOfRates  := Source.FNoOfRates;
  FSearchLevel := Source.FSearchLevel;
//  FMaxIter := Source.FMaxIter;
//  FDeltaL := Source.FDeltaL;
  FIsGlobalClock := Source.FIsGlobalClock;
  FGlobalClockLevel := Source.FGlobalClockLevel;
  FNoOfClockOTUs := Source.FNoOfClockOTUs;
  FIsUserSuppliedTree := Source.FIsUserSuppliedTree;
  FCanceled := Source.FCanceled;
  FHasRateStdErrs := Source.FHasRateStdErrs;
end;

procedure TMLTreeAnalyzer.CancelSearch;
begin
  FCanceled := true;
end;

procedure TMLTreeAnalyzer.CheckAbort;
begin
  if (not FCanceled) and Assigned(SubtaskCheckCancel) then
    FCanceled := SubtaskCheckCancel(CHECK_CANCEL_NO_PROG_VAL, EmptyStr);
  if FCanceled then
    Abort;
end;

procedure TMLTreeAnalyzer.MyWriteToDevConsole(aMsg: String);
begin
  {$IFDEF DEBUG}{$IFDEF VISUAL_BUILD}
  if Assigned(DevConsoleProc) then
    DevConsoleProc(aMsg);
  {$ENDIF}{$ENDIF}
end;

function TMLTreeAnalyzer.NumModelsEvaluated: Integer;
var
  i: Integer = -1;
begin
  Result := 0;
  if Assigned(ModelInfoList) and (ModelInfoList.Count > 0) then
    for i := 0 to ModelInfoList.Count - 1 do
      if Assigned(ModelInfoList[i]) and (CompareValue(ModelInfoList[i].BIC, 0.0, FP_CUTOFF) > 0) then
        inc(Result);
end;

procedure TMLTreeAnalyzer.ClearUnEvaluatedModels(freeMem: Boolean);
var
  i: Integer = -1;
  aInfo: TModelInfo = nil;
begin
  if Assigned(ModelInfoList) and (ModelInfoList.Count > 0) then
  begin
    for i := ModelInfoList.Count - 1 downto 0 do
      if CompareValue(ModelInfoList[i].BIC, 0, FP_CUTOFF) <= 0 then
      begin
        aInfo := ModelInfoList[i];
        if Assigned(aInfo) and freeMem then
          aInfo.Free;
        ModelInfoList.Delete(i);
      end;
    ModelInfoList.Pack;
  end;
end;

function TMLTreeAnalyzer.GetLogLikelihood: extended;
begin
  result := 0;
  if InitTree = nil then exit;
  if MLTree = nil then exit;
  result := MLTree.LogLikelihood;
end;

procedure TMLTreeAnalyzer.Initialize;
begin
  if InitTree = nil then exit;
  if Assigned(RuntimeProgress) and Assigned(FModel) then
    RuntimeProgress.RegisterCancellable(FModel);
  if KeepUserTreeBLens then
  begin
    if InitTree.SBL < FP_CUTOFF then
      ComputeInitialBLens(2);
  end
  else
    ComputeInitialBLens(2);
  if FCanceled then
    Exit;
  if MLTree = nil then
  begin
    if FIsModelTamer or FIsLittleBootstrap then
      MLTree := TMLTree.CreateWithCompressedData(FOrigNoOfSites, Seqs, InitTree, FModel, FNeedsRootByOutgroup)
    else
      MLTree := TMLTree.Create(SeqData, InitTree, FModel, FNeedsRootByOutgroup);
    MLTree.NoOfThreads := NoOfThreadsToUse;
  end;
  if not FIsDoingTreeSearch then
    MLTree.IsDoingTreeSearch := False;
  MLTree.Initialize(true, true);
  {$IFDEF VISUAL_BUILD}
  CheckAbort;
  if MLTree.Cancelled then
    FCanceled := True;
  {$ENDIF}
end;

procedure TMLTreeAnalyzer.Optimize;
begin
  if InitTree = nil then exit;
  if MLTree = nil then Initialize;
  MLTree.NoOfThreads := NoOfThreadsToUse; // GS = let the user decide
  MLTree.SubtaskCheckCancel := SubTaskCheckCancel;
  CheckAbort;
  if KeepUserTreeBLens and (MLTree.NoOfSites = 1) then
    MLTree.OptimizeTreeSize
  else
    MLTree.Optimize(CheckCancel, OptimizeMessage, NeedsRootByOutgroup);
  CheckAbort;
end;

procedure TMLTreeAnalyzer.OptimizeBLens;
begin
  if InitTree = nil then exit;

  if MLTree = nil then Initialize;
  CheckAbort;
  MLTree.OptimizeAllBLens(nil);
  CheckAbort;
  if Assigned(NonClockTree) then
    FreeAndNil(NonClockTree);
  if Assigned(NonClockModel) then
    FreeAndNil(NonClockModel);
end;

procedure TMLTreeAnalyzer.OptimizeParams;
begin
  if InitTree = nil then exit;
  if MLTree = nil then Initialize;
  CheckAbort;
  MLTree.OptimizeParameters;
  CheckAbort
end;

{
procedure TMLTreeAnalyzer.Linearize;
begin
  if InitTree = nil then exit;

  if MLTree = nil then Initialize;

  MLTree.Linearize(CheckCancel);
end;
}

procedure TMLTreeAnalyzer.OutputBootstrapHeights(BSHeights: T2DExtArray);
var
  {$IFNDEF VISUAL_BUILD}
  OutFile: String = '';
  {$ENDIF}
  Rep, NodeIndex, i: Integer;
  OutList: TStringList;
  Line: String;
  NumSeqs: Integer;
begin
  OutList := TStringList.Create;
  NumSeqs := Length(BSHeights) + 1;
  try
    try
      // generate the headers
      Line := 'NodeId,';  //need to add the std errs
      for i := 0 to Length(BSHeights[0]) - 2 do
        Line := Line + 'Rep_' + IntToStr(i + 1) + ',';
      Line := Line + 'StdErr';
      OutList.Add(Line);

      // add the heights
      for NodeIndex := Low(BSHeights) to High(BSHeights) do
      begin
        Line := IntToStr(NodeIndex + 1 + NumSeqs);
        for Rep := 0 to Length(BSHeights[NodeIndex]) - 1 do
        begin
          Line := Line + ',' + Format('%.8f', [BSHeights[NodeIndex][Rep]]);
        end;
        OutList.Add(Line);
      end;

      // write the results to disk
      {$IFNDEF VISUAL_BUILD}
        OutFile := NextAvailableFilenameNV('.csv');
        Insert('_heights', OutFile, Length(OutFile) - 3);
        OutList.SaveToFile(OutFile);
      {$ENDIF}
    except
    {$IFDEF VISUAL_BUILD}

    {$ELSE}
      on E:Exception do
        Error_NV(E.Message);
    {$ENDIF}
    end;
  finally
    if Assigned(OutList) then
      OutList.Free;
  end;
end;

procedure TMLTreeAnalyzer.OutputBootstrapRates(BSRates: T2DExtArray);
var
  {$IFNDEF VISUAL_BUILD}
  OutFile: String = '';
  {$ENDIF}
  Rep, NodeIndex, i: Integer;
  OutList: TStringList;
  Line: String;
begin
  OutList := TStringList.Create;
  try
    try
      // generate the headers
      Line := 'NodeId,';  //need to add the std errs
      for i := 0 to Length(BSRates[0]) - 2 do
        Line := Line + 'Rep_' + IntToStr(i + 1) + ',';
      Line := Line + 'StdErr';
      OutList.Add(Line);

      // add the heights
      for NodeIndex := Low(BSRates) to High(BSRates) do
      begin
        Line := IntToStr(NodeIndex + 1);
        for Rep := 0 to Length(BSRates[NodeIndex]) - 1 do
        begin
          Line := Line + ',' + Format('%.8f', [BSRates[NodeIndex][Rep]]);
        end;
        OutList.Add(Line);
      end;

      // write the results to disk
      {$IFNDEF VISUAL_BUILD}
        OutFile := NextAvailableFilenameNV('.csv');
        Insert('_rates', OutFile, Length(OutFile) - 3);
        OutList.SaveToFile(OutFile);
      {$ENDIF}
    except
    {$IFDEF VISUAL_BUILD}

    {$ELSE}
      on E:Exception do
        Error_NV(E.Message);
    {$ENDIF}
    end;
  finally
    if Assigned(OutList) then
      OutList.Free;
  end;

end;

function TMLTreeAnalyzer.AnchorClockTree(Tree: TTreeData; var minTime, maxTime: array of extended; const MaxRateRatio: Extended): boolean;
var
  tmptree: TTreeData;
  //ah,vh: array of extended;
  //ar,vr: array of extended;
  f: PArrayOfInt;
  //rep,i: integer;
begin
  tmptree := nil;
  f := nil;

  if tree = nil then
  begin
    tmptree := TTreeData.Create(NoOfSeqs, true, true, false);
    tmpTree.AssignDataCoverage(Tree);
    MLTree.GetTreeData(tmptree);
    tree := tmptree;
  end;

  try
    try
{
      if NumReplicates > 0 then
      begin
        FHasRateStdErrs := True;
        setlength(ah, NoOfSeqs);
        setlength(vh, NoOfSeqs);
        setlength(ar, 2 * NoOfSeqs - 1);
        setlength(vr, 2 * NoOfSeqs - 1);

        GetMem(f, SizeOf(integer)*(NoOfSites+2));
        MLTree.Model.BootTable := f;
        for i := 0 to NoOfSeqs-1 do
        begin
          ah[i] := 0;
          vh[i] := 0;
        end;

        for i := 0 to 2 * NoOfSeqs - 2 do
        begin
          ar[i] := 0.0;
          vr[i] := 0.0;
        end;

        for rep := 0 to NumReplicates-1 do
        begin
          for i := 0 to NoOfSites do
            f[i] := 0;
          for i := 1 to NoOfSites do
            Inc(f[Random(NoOfSites)+1]);

          if Assigned(CheckCancel) then
            if CheckCancel(trunc(rep / NumReplicates * 100), 'Bootstrapping ML clock tree (' + IntToStr(rep) + '/' + IntToStr(NumReplicates) + ')') then
              break;

          FNoOfClocks := MLTree.MakeClockTree(tree, false, 0, nil, 0, false, SamplingTime);
          result := MLTree.AnchorClockTree(tree, minTime, maxTime);

          for i := 0 to NoOfSeqs-2 do
          begin
            ah[i] := ah[i] + MLTree.Node[i+NoOfSeqs].height;
            vh[i] := vh[i] + MLTree.Node[i+NoOfSeqs].height * MLTree.Node[i+NoOfSeqs].height;
          end;

          for i := 0 to 2 * NoOfSeqs - 2 do
          begin
            ar[i] := ar[i] + MLTree.Node[i].rate;
            vr[i] := vr[i] + MLTree.Node[i].rate * MLTree.Node[i].rate;
          end;
        end;
        MLTree.Model.BootTable := nil;
        FNoOfClocks := MLTree.MakeClockTree(tree, false, 0, CheckCancel, 0, false, SamplingTime);
        result := MLTree.AnchorClockTree(tree, minTime, maxTime);

        for i := 0 to NoOfSeqs-2 do
        begin
          MLTree.Node[i+NoOfSeqs].vh := vh[i]/NumReplicates -(ah[i]/NumReplicates)*(ah[i]/NumReplicates);
          MLTree.Node[i+NoOfSeqs].ase := sqrt(MLTree.Node[i+NoOfSeqs].vh);
        end;

        for i := 0 to 2 * NoOfSeqs - 2 do
          MLTree.Node[i].rse := sqrt(vr[i]/NumReplicates - (ar[i]/NumReplicates) * (ar[i]/NumReplicates));

        setlength(ah, 0);
        setlength(vh, 0);
        setlength(ar, 0);
        setlength(vr, 0);
      end
      else
      begin
}
//        FHasRateStdErrs := False;
//        FNoOfClocks := MLTree.MakeClockTree(tree, false, 0, CheckCancel, 0, MergeRates, SamplingTime);
        result := MLTree.AnchorClockTree(tree, minTime, maxTime, MaxRateRatio);
        if IsDeveloper then
          FOriginalReltimes := GetRelTimes;

        if not Result then
          raise Exception.Create('Failed to anchor the timetree due to invalid calibrations. At least one max time calibration is less than the min time calibration of a descendent node');

//      end;
    except
      on E:Exception do
      begin
          {$IFNDEF VISUAL_BUILD}
          error_nv('Exception in AnchorClockTree: ' + E.Message, E);
          {$ENDIF}
        raise E; { needs to be handled upstream}
      end;
    end;
  finally
    if Assigned(tmptree) then
      tmptree.Free;
    if Assigned(f) then
      FreeMemAndNil(f);
    MLTree.Model.BootTable := nil;
  end;
end;

procedure TMLTreeAnalyzer.MakeClockTree(Tree: TTreeData; MaxRateRatio: Extended; namesList: TStringList);
begin
  if MLTree = nil then
    Initialize;
  FHasRateStdErrs := False;

  if not Assigned(NonClockTree) then
  begin
    NonClockTree := TTreeData.Create(NoOfSeqs, true, true, false);
    MLTree.GetTreeData(NonClockTree);

    if Assigned(NonClockModel) then
      FreeAndNil(NonClockModel);
    NonClockModel := CopyModel(MLTree.Model);
  end;

  MLTree.OtuNamesList := namesList;
  MLTree.NeedsRootedOnOutgroup := FNeedsRootByOutgroup;
  FNoOfClockOTUs := MLTree.MakeClockTree(Tree, IsGlobalClock, GlobalClockLevel, CheckCancel, MaxRateRatio, SamplingTime);
  CheckCancel := nil;
end;

 { TODO : Consider combining NamesList (OTUs) and NodeLabels into a single object. }
procedure TMLTreeAnalyzer.MakeClockTree(Tree: TTreeData;
                                        var ExportList: TStringList;
                                        const NamesList: TStringList;
                                        const NodeLabels: TStringList;
                                        const MaxRateRatio: Extended);
begin
  MakeClockTree(Tree, MaxRateRatio, namesList);
  GenerateClockTreeExport(ExportList, NamesList, NodeLabels);
end;

function TMLTreeAnalyzer.GetRelTimes: ArrayOfExtended;
begin
  Result := MLTree.RelTimeComputer.GetReltimesArray;
end;

procedure TMLTreeAnalyzer.MakeNonClockTree;
begin
  if Assigned(NonClockTree) then
  begin
    MLTree.Model.Assign(NonClockModel);
    MLTree.InitNodeData(NonClockTree);
    MLTree.Initialize(false, true);
  end
  else
    Optimize;

  FNoOfClockOTUs := 0;
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

procedure SetDepth(p : TMLTreeNode);
begin
  if p.OTU then
    exit;

  SetDepth(p.des1);
  SetDepth(p.des2);

  if p.des1.depth > p.des2.depth then
    p.depth := p.des1.depth +1
  else
    p.depth := p.des2.depth +1;
end;
{
procedure ChangeRoot(root, newposition: TMLTreeNode);
var
  a, p, d : TMLTreeNode;
  len0, len1, len2: extended;
  ci0, ci1, ci2: double;
  fix0, fix1, fix2: boolean;
begin

  if newposition.anc = root then
  begin
    if newposition = root.des2 then
    begin
      p := root.des2;
      root.des2 := root.des1;
      root.des1 := p;
    end;
    root.des2.blen := root.des1.blen+root.des2.blen;
    root.des1.blen := 0;
//    oldposition := newposition;
    Exit;
  end;
  len0 := root.des1.blen +root.des2.blen;
  ci0  := max(root.des1.CI, root.des2.CI);
  fix0 := root.des1.fixed and root.des2.fixed;
  d := newposition;
  p := d.anc;
  a := p.anc;
  len2 := d.blen;
  ci2  := d.CI;
  fix2 := d.fixed;
  while p <> root do
  begin
    len1 := p.blen;
    p.blen := len2;
    len2 := len1;
    ci1  := p.CI;
    p.CI := ci2;
    ci2  := ci1;
    fix1 := p.fixed;
    p.fixed := fix2;
    fix2 := fix1;

    p.anc := d;
    if d = p.des1 then
      p.des1 := a
    else
      p.des2 := a;
    d := p;
    p := a;
    a := a.anc;
  end;
  if d = p.des1 then
  begin
    p.des2.anc := d;
    p.des2.blen := len0;
    p.des2.CI := ci0;
    p.des2.fixed := fix0;
    if p = d.des1 then
      d.des1 := p.des2
    else
      d.des2 := p.des2;
//    oldposition := p.des2;
  end
  else
  begin
    p.des1.anc := d;
    p.des1.blen := len0;
    p.des1.CI := ci0;
    p.des1.fixed := fix0;
    if p = d.des1 then
      d.des1 := p.des1
    else
      d.des2 := p.des1;
//    oldposition := p.des1;
  end;
  len0 := newposition.blen;
  fix0 := newposition.fixed;
  p := newposition.anc;
  p.anc := root;
  p.blen := len0;
  p.CI := ci0;
  p.fixed := fix0;
  newposition.anc := root;
  newposition.blen := 0;
  newposition.fixed := p.fixed;
  root.des1 := newposition;     // OTU with the longest branch
  root.des2 := p;

  SetDepth(root);
end;
}
procedure TMLTreeAnalyzer.ResetSearchFilter;
var
  tmpTreeData: TTreeData = nil;
  tmpMPTree: TMPTree = nil;
  i: integer;
begin
  try
    tmpTreeData := TTreeData.Create(NoOfSeqs, true, false, true, false);
    if Model.SeqDataType = DNA then
      tmpMPTree := TMPTree.Create(Seqs, 8)
    else
      tmpMPTree := TMPTree.Create(Seqs, 32);

    MLTree.GetTreeData(tmpTreeData);
    tmpMPTree.ComputeConsistency(tmpTreeData);

    for i := 0 to NoOfSeqs-2 do
      MLTree.Node[NoOfSeqs+i].CI := tmpTreeData.Stats[i];

  finally
    if assigned(tmpMPTree) then
      tmpMPTree.Free;
    if assigned(tmpTreeData) then
      tmpTreeData.Free;
  end;
end;

function TMLTreeAnalyzer.PrepareSearchMLTree(maketree: boolean): boolean;
var
  bmax: extended;
  i,j,d: integer;
begin
  {$IFDEF DEBUG}SaveMLlog(' MLTreeAnalyzer.PrepareSearchMLTree started');{$ENDIF}
  result := false;
  if maketree or (InitTree = nil) then
  begin
    if CheckCancel(0, 'Making initial tree') then
      exit;
    MakeInitialTree(InitialTreeOption);
  end
  else //if (not InitTree.isBLen) or (InitTree.SBL < FP_CUTOFF) then
    ComputeInitialBLens(2);

  if not assigned(MLTree) then
  begin
    if FIsModelTamer then
      MLTree := TMLTree.CreateWithCompressedData(FOrigNoOfSites, Seqs, InitTree, FModel, FNeedsRootByOutgroup)
    else
      MLTree := TMLTree.Create(Seqs, InitTree, FModel, FNeedsRootByOutgroup);
  end
  else
    MLTree.InitNodeData(InitTree);

  if assigned(CheckCancel) then
    if InitialTreeOption = DefaultInitTreeMethod then
    begin
      if CheckCancel(20, 'Optimizing initial tree') then
        exit;
    end
    else
      if CheckCancel(10, 'Optimizing initial tree') then
        exit;

  ResetSearchFilter;

//  if CheckCancel(-1, 'Optimizing initial tree') then
//    exit;

  with MLTree do
  begin
    bmax := node[0].blen;
    j := 0;
    for i := 1 to NoOfSeqs-1 do   // find the max branch length
      if node[i].blen > bmax then
      begin
        j := i;
        bmax := node[i].blen;
      end;
  end;
//    ChangeRoot(MLTree.root, MLTree.node[j]);  // Root on the largest branch length
  MLTree.node[j].flag := false;   // To make node[j] = root.des1 fixed.
  MLTree.ChangeRoot(MLTree.node[j]);

  MLTree.Initialize(true, true);
{
  d := trunc(log10(-LogLikelihood));

  if d >= 5 then
    FdeltaL := 1.0E-2
  else if d <= 3 then
    FdeltaL := 1.0E-4
  else
    FdeltaL := 1.0E-3;

  MLTree.deltaL  := FdeltaL;
  MLTree.MaxIter := FMaxIter;
}
  MLTree.NoOfThreads := NoOfThreadsToUse; // GS - let the user specify
  MLTree.SubtaskCheckCancel := SubtaskCheckCancel;

  if InitialTreeOption = DefaultInitTreeMethod then
    MLTree.MinProgress := 20
  else
    MLTree.MinProgress := 10;
  MLTree.MaxProgress := 40;
  if assigned(CheckCancel) then
    if CheckCancel(MLTree.MinProgress, 'Optimizing initial tree') then
      exit;

  result := MLTree.Optimize(CheckCancel, 'Optimizing initial tree', false);

  {$IFDEF DEBUG}SaveMLlog(' MLTreeAnalyzer.PrepareSearchMLTree ended');{$ENDIF}
end;

procedure ResetSiteConfig(MLTree: TMLTree);

  procedure ResetSiteConfigUp(n: TMLTreeNode);
  begin
    if n.OTU or n.flag then
      exit;

    ResetSiteConfigUp(n.des1);
    ResetSiteConfigUp(n.des2);

    MLTree.ResetSiteConfig1OfNodeFast(n, true);
  end;

  procedure ResetSiteConfigDown(n: TMLTreeNode);
  begin
    MLTree.ResetSiteConfig0OfNodeFast(n);

    if not n.des1.OTU then
      ResetSiteConfigDown(n.des1);
    if not n.des2.OTU then
      ResetSiteConfigDown(n.des2);
  end;

begin
  ResetSiteConfigUp(MLTree.root);
  ResetSiteConfigDown(MLTree.root);
end;

function compare(Item1, Item2: Pointer): Integer;
begin
  result := 0;
  if TSwapData(Item1).dL > TSwapData(Item2).dL then
    result := -1
  else if TSwapData(Item2).dL > TSwapData(Item1).dL then
    result := 1;
end;

//function SearchTree(MLTree : TMLTree;
//                    SearchLevel: integer;
//                    SearchFilter: extended;
//                    deltaL: extended;
//                    MaxIter: integer;
//                    CheckCancel: TCheckCancelFunc): boolean;
//var
//  SwapList  : TList;
//  q: array of TMLTreeNode;
//  Canceled: boolean;
//  progress, aprogress: integer;
//
//  procedure ResetLikelihood(node: TMLTreeNode);
//
//    procedure InitL12OfNode(n: TMLTreeNode);
//    begin
//      if n.OTU or n.flag then
//        exit;
//
//      InitL12OfNode(n.des1);
//      InitL12OfNode(n.des2);
//
////      if not n.des1.flag then
////        MLTree.ComputeL1OfNode(n);
////      if not n.des2.flag then
//        MLTree.ComputeL1OfNode(n);
//    end;
//
//  begin
//    InitL12OfNode(node);
//  end;
//
//  function InitSubtreeLikelihood(node: TMLTreeNode): extended;
//
//    procedure InitL12OfNode(n: TMLTreeNode);
//    begin
//      if n.flag then
//        exit;
//
//      InitL12OfNode(n.des1);
//      InitL12OfNode(n.des2);
//
//      MLTree.ComputeL1OfNode(n);
//    end;
//
//    procedure RenewL12OfAnc(n: TMLTreeNode);
//    begin
//      if n.anc = nil then exit;
//
//        MLTree.ComputeL1OfNode(n.anc);
//    end;
//
//  begin
//    InitL12OfNode(node);
//
//    RenewL12OfAnc(node);
//
//    MLTree.ComputeL0OfNode(node.anc);
//    result := MLTree.ComputeLogLikelihoodOfNode(node.anc);
//  end;
//
//  procedure IterateSubtreeBLens(node: TMLTreeNode; var L: extended);
//
//    procedure IterateSubtreeOfNode(n: TMLTreeNode);
//    begin
//      if not n.flag then
//      begin
//        MLTree.ComputeL0OfNode(n);
//
//        IterateSubtreeOfNode(n.des1);
//        IterateSubtreeOfNode(n.des2);
//      end;
//
//      MLTree.IterateBlenOfNode(n, L, 0.01);
//    end;
//
//  begin
//    IterateSubtreeOfNode(node);
//  end;
//
//  procedure ClearSwapList;
//  begin
//    while SwapList.Count > 0 do
//    begin
//      TSwapData(SwapList[SwapList.Count-1]).Free;
//      SwapList.Delete(SwapList.Count-1);
//    end;
//  end;
//
//  procedure SetFlag(index: integer);
//  var
//    p: TMLTreeNode;
//  begin
//
//    p := MLTree.Node[TSwapData(SwapList[index]).node[0]];
//    while (p <> nil) and p.flag do
//    begin
//      p.flag := false;
//      p := p.anc;
//    end;
//  end;
//
//  procedure AddSwapData(swapindex: integer; dL: extended; n: integer);
//  var
//    data: TSwapData;
//    i : integer;
//  begin
//    data := TSwapData.Create(n);
//    data.swapindex := swapindex;
//    data.dL        := dL;
//
//    for i := 0 to n do
//      data.node[i] := q[i].index;
//    for i := 1 to n do
//      data.blen[i] := q[i].blen;
//    for i := 1 to n do
//      data.ase[i]  := q[i].ase;
//
//    SwapList.Add(data);
//  end;
//
//  function SearchEachNode(node: TMLTreeNode):boolean;
//  var
//    p: array of TMLTreeNode;
//    d: integer;
//    vb,sb: extended;
//
//    function InitSubtree(node: TMLTreeNode): extended;
//
//      procedure InitSubtreeUp(n: TMLTreeNode);
//      begin
//        if n.flag then
//          exit;
//
//        InitSubtreeUp(n.des1);
//        InitSubtreeUp(n.des2);
//
//        MLTree.ResetSiteConfig1OfNode(n, true);
//      end;
//
//      procedure InitSubtreeDown(n: TMLTreeNode);
//      begin
//        MLTree.ResetProbOfNode(n);
//
//        if n.flag then
//          exit;
//
//        InitSubtreeDown(n.des1);
//        InitSubtreeDown(n.des2);
//      end;
//
//    begin
//      InitSubtreeUp(node);
//      InitSubtreeDown(node);
//
//      result := InitSubtreeLikelihood(node);
//    end;
//
//    procedure SetInitialSubtree;
//    begin
//      p[0] := node.anc.anc;
//      p[1] := node.anc;
//      p[3] := node;
//      if node = node.anc.des1 then
//        p[2] := node.anc.des2
//      else
//        p[2] := node.anc.des1;
//
//      q[0].Assign(p[0]);
//      q[1].Assign(p[1]);
//      q[3].Assign(p[3]);
//
//      if node.anc = node.anc.anc.des1 then
//        q[0].des1 := q[1]
//      else
//        q[0].des2 := q[1];
//      q[1].anc := q[0];
//      if node = node.anc.des1 then
//      begin
//        q[1].des1 := q[3];
//        q[1].des2 := q[2];
//        q[2].Assign(node.anc.des2);
//      end
//      else
//      begin
//        q[1].des1 := q[2];
//        q[1].des2 := q[3];
//        q[2].Assign(node.anc.des1);
//      end;
//      q[2].anc := q[1];
//      q[3].anc := q[1];
//
//      q[1].blen := q[1].blen;
//      if node = node.anc.des1 then
//        q[2].blen := node.anc.des2.blen
//      else
//        q[2].blen := node.anc.des1.blen;
//      q[3].blen := q[3].blen;
//
//      if q[1] = q[0].des1 then
//        q[0].des2.flag := true
//      else
//        q[0].des1.flag := true;
//      q[0].flag := false;
//      q[1].flag := false;
//      q[2].flag := true;
//      q[3].flag := true;
//
//      d := 1;
//    end;
//
//    function IncSubtreeDown(index: integer): boolean;
//    begin
//      result := false;
//      if p[index].OTU then
//        exit;
//
//      inc(d);
//      sb := sb +p[index].blen;
//      vb := vb +p[index].ase*p[index].ase;
//
//      p[2*d  ] := p[index].des1;
//      p[2*d+1] := p[index].des2;
//
//      q[2*d  ].Assign(q[index].des1);
//      q[2*d+1].Assign(q[index].des2);
//      q[2*d  ].anc  := q[index];
//      q[2*d+1].anc  := q[index];
//      q[index].des1 := q[2*d  ];
//      q[index].des2 := q[2*d+1];
//
//      q[index].flag := false;
//      q[2*d  ].flag := true;
//      q[2*d+1].flag := true;
//
//      result := true;
//    end;
//
//    function IncSubtreeUp(index: integer): boolean;
//    begin
//      result := false;
//      if p[0].anc = nil then
//        exit;
//
//      inc(d);
//      sb := sb +p[index].blen;
//      vb := vb +p[index].ase*p[index].ase;
//
//      p[2*d] := p[0];
//      p[0]   := p[0].anc;
//      if p[index] = p[2*d].des1 then
//        p[2*d+1] := p[2*d].des2
//      else
//        p[2*d+1] := p[2*d].des1;
//
//      q[0    ].Assign(p[0]);
//      q[2*d  ].Assign(p[2*d]);
//      q[2*d+1].Assign(p[2*d+1]);
//
//      q[2*d].anc := q[0];
//      if p[2*d] = p[0].des1 then
//        q[0].des1 := q[2*d]
//      else
//        q[0].des2 := q[2*d];
//
//      q[index].anc := q[2*d];
//      q[2*d+1].anc := q[2*d];
//      if p[index] = p[2*d].des1 then
//      begin
//        q[2*d].des1 := q[index];
//        q[2*d].des2 := q[2*d+1];
//      end
//      else
//      begin
//        q[2*d].des1 := q[2*d+1];
//        q[2*d].des2 := q[index];
//      end;
//
//      if q[2*d] = q[0].des1 then
//        q[0].des2.flag := true
//      else
//        q[0].des1.flag := true;
//      q[index].flag := false;
//      q[2*d  ].flag := false;
//      q[2*d+1].flag := true;
//
//      result := true;
//    end;
//
//    procedure DecSubtreeDown(index: integer);
//    var
//      a: TMLTreeNode;
//    begin
//      a := q[index].anc;
//      q[index].Assign(p[index]);
//      q[index].anc := a;
//      q[index].flag := true;
//
//      sb := sb -p[index].blen;
//      vb := vb -p[index].ase*p[index].ase;
//      dec(d);
//    end;
//
//    function SPR: boolean;
//    var
//      L0, L, dL: extended;
//      n: TMLTreeNode;
//      i: integer;
//    begin
//      Result := true;
//
//      L0 := MLTree.LogLikelihood;
//
//      RemoveBranch(q[2]);
//      AddBranch(q[1], q[2], q[2*d  ]);
//      q[3].blen := p[1].blen;
//      q[1].blen := p[3].blen;
//
//      if q[0].des1.flag then
//        n := q[0].des2
//      else
//        n := q[0].des1;
//
//      L := InitSubtree(n);
//      i := 0;
//      repeat
//        dL := L;
//        IterateSubtreeBLens(n, L);
//        inc(i);
//
//        if (L0-L) > (L-dL)*(MLTree.MaxIter -i) then
//          break;
//      until (abs(L-dL) < deltaL) or (i = MaxIter);
//
//      if L > L0+deltaL then
//      begin
//        AddSwapData(1, L-L0, 2*d+1);
//        result := false;
//      end;
//
//      RemoveBranch(q[2]);
//      AddBranch(q[1], q[2], q[2*d+1]);
//      q[3].blen := p[1].blen;
//      q[1].blen := p[3].blen;
//      q[2*d  ].blen := p[2*d  ].blen;
//      q[2*d+1].blen := p[2*d+1].blen;
//
//      if q[0].des1.flag then
//        n := q[0].des2
//      else
//        n := q[0].des1;
//
//      L := InitSubtree(n);
//      i := 0;
//      repeat
//        dL := L;
//        IterateSubtreeBLens(n, L);
//        inc(i);
//
//        if (L0-L) > (L-dL)*(MLTree.MaxIter -i) then
//          break;
//      until (abs(L-dL) < deltaL) or (i = MaxIter);
//
//      if L > L0+deltaL then
//      begin
//        AddSwapData(2, L-L0, 2*d+1);
//        result := false;
//      end;
//
//      RemoveBranch(q[2]);
//      AddBranch(q[1], q[2], q[3]);
//      for i := 1 to 2*d+1 do
//        q[i].blen := p[i].blen;
//
//      if not result then
//        for i := 1 to 2*d+1 do
//          if p[i].done then
//          begin
//            p[i].done := false;
//            dec(aprogress);
//          end;
//
//      if aprogress > progress then
//        progress := aprogress;
//      if assigned(CheckCancel) then
//        Canceled := CheckCancel(round(progress/(2*MLTree.NoOfSeqs-3)*100), 'Searching ML tree');
//      if Canceled then
//        CheckCancel(round(progress/(2*MLTree.NoOfSeqs-3)*100), 'Canceling ML Search');
//
//    end;
//
//    function SPRRecursiveDown: boolean;
//    var
//      flag: boolean;
//    begin
//      result := true;
//      if d > SearchLevel then exit;
//
////      if (q[2*d].ase  < 0.000000000001) or (q[2*d].blen < 3*q[2*d].ase) then
//      if ((vb < 0.000000000001) or (sb < BLenFilter*sqrt(vb))) then
//        if IncSubtreeDown(2*d) then
//        begin
//          flag   := SPR;
//          result := result and flag;
//          flag   := SPRRecursiveDown;
//          result := result and flag;
//          DecSubtreeDown(2*(d-1));
//        end;
////      if (q[2*d+1].ase  < 0.000000000001) or (q[2*d+1].blen < 3*q[2*d+1].ase) then
//      if ((vb < 0.000000000001) or (sb < BLenFilter*sqrt(vb))) then
//        if IncSubtreeDown(2*d+1) then
//        begin
//          flag   := SPR;
//          result := result and flag;
//          flag   := SPRRecursiveDown;
//          result := result and flag;
//          DecSubtreeDown(2*(d-1)+1);
//        end;
//    end;
//
//    function SPRRecursiveUp: boolean;
//    var
//      flag: boolean;
//    begin
//      result := true;
//      if d > SearchLevel then exit;
//
//      if d > 2 then
//      begin
//        flag   := SPR;
//        result := result and flag;
//      end;
//
////      if (q[2*d+1].ase  < 0.000000000001) or (q[2*d+1].blen < 3*q[2*d+1].ase) then
//      if ((vb < 0.000000000001) or (sb < BLenFilter*sqrt(vb))) then
//        if IncSubtreeDown(2*d+1) then
//        begin
//          flag   := SPR;
//          result := result and flag;
//          flag   := SPRRecursiveDown;
//          result := result and flag;
//          DecSubtreeDown(2*(d-1)+1);
//        end;
//
////      if (q[2*d].ase  < 0.000000000001) or (q[2*d].blen < 3*q[2*d].ase) then
//      if ((vb < 0.000000000001) or (sb < BLenFilter*sqrt(vb))) then
//        if IncSubtreeUp(2*d) then
//        begin
//          flag   := SPRRecursiveUp;
//          result := result and flag;
//        end;
//    end;
//    
//  var
//    flag: boolean;
//  begin
//    result := true;
//
//    if Canceled then exit;
//    if node.anc = nil then exit;
//    if node.anc.anc = nil then exit;
//
//    setlength(p, SearchLevel*2+4);
//
//    SetInitialSubtree;
//
//    sb := q[3].blen;
//    vb := q[3].ase*q[3].ase;
//
//    if (q[3].ase  < 0.000000000001) or (q[3].blen < BLenFilter*q[3].ase) then
//      if IncSubtreeDown(3) then
//      begin
//        flag   := SPR;
//        result := result and flag;
//
//        flag   := SPRRecursiveDown;
//        result := result and flag;
//
//        DecSubtreeDown(3);
//      end;
//
//    sb := q[1].blen;
//    vb := q[1].ase*q[1].ase;
//
//    if (q[1].ase  < 0.000000000001) or (q[1].blen < BLenFilter*q[1].ase) then
//      if IncSubtreeUp(1) then
//      begin
//        flag   := SPRRecursiveUp;
//        result := result and flag;
//      end;
//
//    if result then
//    begin
//      if not node.done then
//        inc(aprogress);
//      node.done := true;
//    end;
//  end;
//
//  function SwapBranches: integer;
//  var
//    tmpTree: TTreeData;
//
//    function SwapEachBranch(index: integer): boolean;
//    var
//      p: array of TMLTreeNode;
//      i,n: integer;
//    begin
//      n := TSwapData(SwapList[index]).NoOfNodes;
//      setlength(p, n+1);
//      for i := 0 to n do
//        p[i] := MLTree.Node[TSwapData(SwapList[index]).node[i]];
//
//      result := true;
//      for i := 1 to n do
//        if not p[i].flag then
//        begin
//          result := false;
//          break;
//        end;
//      if not result then exit;
//
//      case TSwapData(SwapList[index]).swapindex of
//        1: begin
//             RemoveBranch(p[2]);
//             AddBranch(p[1], p[2], p[n-1]);
//           end;
//        2: begin
//             RemoveBranch(p[2]);
//             AddBranch(p[1], p[2], p[n]);
//           end;
//      end;
//
//      for i := 1 to n do
//      begin
//        p[i].blen := TSwapData(SwapList[index]).blen[i];
//        p[i].ase  := TSwapData(SwapList[index]).ase[i];
//        MLTree.ResetProbOfNode(p[i]);
//        p[i].flag := false;
//      end;
//    end;
//
//    function SwapBranchOneByOne: integer;
//    var
//      L0: extended;
//      flag0,flag1: array of boolean;
//      i,j: integer;
//    begin
//      result := 0;
//      for i := 0 to SwapList.Count-1 do
//      begin
//        setlength(flag0, TSwapData(SwapList[i]).NoOfNodes);
//        setlength(flag1, TSwapData(SwapList[i]).NoOfNodes);
//
//        MLTree.GetTreeData(tmpTree);
//        L0 := MLTree.LogLikelihood;
//        for j := 0 to length(flag0)-1 do
//          flag0[j] := MLTree.Node[TSwapData(SwapList[i]).node[j]].flag;
//
//        TSwapData(SwapList[i]).flag := SwapEachBranch(i);
//        if TSwapData(SwapList[i]).flag then
//        begin
//          for j := 0 to length(flag1)-1 do
//            flag1[j] := MLTree.Node[TSwapData(SwapList[i]).node[j]].flag;
//          SetFlag(i);
//          ResetSiteConfig(MLTree);
//          ResetLikelihood(MLTree.Root);
//
//          MLTree.LogLikelihood := MLTree.ComputeLogLikelihood;
//
//          if MLTree.LogLikelihood < L0 then
//          begin
//            MLTree.InitNodeData(tmpTree);
//            MLTree.Initialize(false);
//            TSwapData(SwapList[i]).flag := false;
//            for j := 0 to length(flag0)-1 do
//              MLTree.Node[TSwapData(SwapList[i]).node[j]].flag := flag0[j];
//          end
//          else
//            for j := 0 to length(flag1)-1 do
//              MLTree.Node[TSwapData(SwapList[i]).node[j]].flag := flag1[j];
//
//          if TSwapData(SwapList[i]).flag then
//            inc(result);
//        end;
//      end;
//   end;
//
//  var
//    L0: extended;
//    i: integer;
//  begin
//    tmpTree := TTreeData.Create(MLTree.NoOfSeqs, true, true, false);
//    MLTree.GetTreeData(tmpTree);
//    L0 := MLTree.LogLikelihood;
//
//    for i := 0 to 2*MLTree.NoOfSeqs-2 do
//      MLTree.Node[i].flag := true;
//
//    SwapList.Sort(compare);
//
//    result := 0;
//    for i := 0 to SwapList.Count-1 do
//    begin
//      TSwapData(SwapList[i]).flag := SwapEachBranch(i);
//
//      if TSwapData(SwapList[i]).flag then
//        inc(result);
//    end;
//
//    for i := 0 to SwapList.Count-1 do
//      if TSwapData(SwapList[i]).flag then
//        SetFlag(i);
//
//    ResetSiteConfig(MLTree);
//    ResetLikelihood(MLTree.Root);
//
//    MLTree.LogLikelihood := MLTree.ComputeLogLikelihood;
//
//    if MLTree.LogLikelihood < L0 then
//    begin
//      MLTree.InitNodeData(tmpTree);
//      MLTree.Initialize(false);
//
////      for i := 0 to 2*MLTree.NoOfSeqs-2 do
////        MLTree.Node[i].flag := true;
////      for i := 0 to SwapList.Count-1 do
////        TSwapData(SwapList[i]).flag := true;
////      result := SwapBranchOneByOne;
//
//    end;
//
//    ClearSwapList;
//    tmpTree.Free;
//  end;
//
//
//  function SearchOnAllNodes: boolean;
//  var
//    i: integer;
//    flag: boolean;
//  begin
//    result := true;
//
//    with MLTree do
//      for i := 0 to 2*NoOfSeqs-2 do
//      begin
//        if Canceled then
//          exit;
//
//        if Node[i] = Root then
//          continue;
//
//        if Node[i].anc = Root then
//          continue;
//
//        flag := SearchEachNode(Node[i]);
//
//        if Canceled then
//          break;
//
//        result := result and flag;
//      end;
//  end;
//
//var
//  tmpTree: TTreeData;
//  i,n: integer;
//  dL: extended;
//  flag: boolean;
//  aNoOfSites: Int64;
//begin
//  tmpTree := nil;
//  SwapList := nil;
//  aprogress := 0;
//  progress := 0;
//  Canceled := false;
//  aNoOfSites := Length(MLTree.Seqs[0]); { using aNoOfSites here because for the Little Bootstraps analysis we sub-sample the data but FNoOfSites still needs to match the full data dimension}
//  { TODO -oglen -cmodel tamer : handle modeltamer num sites }
//  try
//    SwapList  := TList.Create;
//    tmpTree := TTreeData.Create(MLTree.NoOfSeqs, true, true, false);
//    with MLTree do
//    begin
//      setlength(q, SearchLevel*2+4);
//      if Model.SeqDataType = protein then
//        for i := 0 to length(q)-1 do
//          q[i] := TMLTreeNode.Create(i, false, NoOfSeqs, aNoOfSites, Model.NoOfRates, 20)
//      else
//        for i := 0 to length(q)-1 do
//          q[i] := TMLTreeNode.Create(i, false, NoOfSeqs, aNoOfSites, Model.NoOfRates, 4);
//
//
//      i := 0;
//      repeat
//        GetTreeData(tmpTree);
//        dL := LogLikelihood;
//
//        ResetL0;
//        flag := SearchOnAllNodes;
//        
////        n := 0;
////        if not flag then
//          n := SwapBranches;
//
//        if LogLikelihood < dL then
//        begin
//          InitNodeData(tmpTree);
//          Initialize(false);
//        end;
//
//        if Canceled then
//          break;
//
//        inc(i);
//      until (n = 0) or (abs(LogLikelihood-dL) < DeltaL) or (i = MaxIter);
//
//    end;
//  finally
//    for i := 0 to length(q)-1  do
//      if q[i] <> nil then
//        q[i].Free;
//    setlength(q, 0);
//
//    while SwapList.Count > 0 do
//    begin
//      TSwapData(SwapList.Count-1).Free;
//      SwapList.Delete(SwapList.Count-1);
//    end;
//    if Assigned(SwapList) then
//      SwapList.Free;
//    if Assigned(tmpTree) then
//      tmpTree.Free;
//  end;
//  result := not Canceled;
//end;

function TMLTreeAnalyzer.SearchMLTree(makeinitialtree: boolean; IsBStrapRep: Boolean; AProgress: TRuntimeProgress = nil): boolean;
var
  MLTreeSearchThread: TMLTreeSearchThread = nil;
begin
  {$IFDEF DEBUG}WriteToDevLog('begin TMLTreeAnalyzer.SearchMLTree');{$ENDIF}
  result := false;
  try try
    if makeinitialtree and assigned(InitTree) then
      FreeAndNil(InitTree);
    MLTreeSearchThread := TMLTreeSearchThread.Create(Self);
    if IsBStrapRep then
    begin
      MLTreeSearchThread.SkipSummaryUpdate := True;
      MLTreeSearchThread.CanComputeDataCoverage := False;
    end;
    MLTreeSearchThread.IsInitialized := False;
    MLTreeSearchThread.ProgressDlg := RuntimeProgress;
    MLTreeSearchThread.IsBootstrapReplicate := IsBStrapRep;
    if Assigned(SubTaskCheckCancel) then
      MLTreeSearchThread.SubTasksCheckCancel := SubTaskCheckCancel;
    if Assigned(AProgress) then
      MLTreeSearchThread.ProgressDlg := AProgress;
    MLTreeSearchThread.FreeOnTerminate := False;
    MLTreeSearchThread.Start;
    MLTreeSearchThread.WaitFor;
    result := not MLTreeSearchThread.Canceled;
    if not MLTreeSearchThread.IsSuccess then
      raise Exception.Create(MLTreeSearchThread.LogText);
    {$IFDEF DEBUG}WriteToDevLog('end TMLTreeAnalyzer.SearchMLTree');{$ENDIF}
  except
    on E: EOutOfMemory do
    begin
    {$IFDEF VISUAL_BUILD}
      raise Exception.Create('''MEGA has run out of addressable memory.  This means that the analysis was not able to be completed.  The analysis may be able to run by adjusting the analysis preferences to consume less memory.''');
    {$ELSE}
      error_NV('MEGA has run out of addressable memory.  This means that the analysis was not able to be completed.  The analysis may be able to run by adjusting the analysis preferences to consume less memory.');
    {$ENDIF}
    end;
    on E: Exception do
      raise Exception.Create(E.Message);
  end;
  finally
    if MLTreeSearchThread <> nil then
    begin
      MLTreeSearchThread.MLTreeAnalyzer := nil;
      MLTreeSearchThread.Free;
    end;
  end;
end;

procedure TMLTreeAnalyzer.ResetMLTree;
begin
  if Assigned(MLTree) then
  begin
    MLTree.SubtaskCheckCancel := nil;
    FreeAndNil(MLTree);
  end;
  Initialize;
end;

{
procedure TMLTreeAnalyzer.SearchMLTree(makeinitialtree: boolean);
var
  D,DD: PMatrixOfExtended;
  dL1,dL2: array of PMatrixOfExtended;
  progress: integer;
  i,j: integer;
begin
  progress := 0;
  FCanceled := false;
  if assigned(CheckCancel) then
    CheckCancel(progress, 'Preparing ML tree search');

  if MLTree <> nil then
    MLTree.Free;

  try try
    CreateMatrix(D, 5);
    CreateMatrix(DD, 5);

    setlength(dL1, Model.NoOfRates);
    setlength(dL2, Model.NoOfRates);
    for i := 0 to Model.NoOfRates-1 do
    begin
      GetMem(dL1[i], SizeOf(Pointer)*(NoOfSites+1));
      for j := 0 to NoOfSites do
        GetMem(dL1[i][j], SizeOf(extended)*Model.NoOfStates);
    end;
    for i := 0 to Model.NoOfRates-1 do
    begin
      GetMem(dL2[i], SizeOf(Pointer)*(NoOfSites+1));
      for j := 0 to NoOfSites do
        GetMem(dL2[i][j], SizeOf(extended)*Model.NoOfStates);
    end;

    PrepareSearchMLTree(makeinitialtree);

    if assigned(CheckCancel) then
      FCanceled := CheckCancel(progress, 'Searching ML tree');

    with MLTree do
    begin
      if SearchLevel >= 2 then
        SearchLevel := 5
      else
        SearchLevel := 1;

     i := 0;
     repeat
       dL := MLTree.LogLikelihood;


        FCanceled := not SearchTree(MLTree,
                                    SearchLevel,
                                    SearchFilter,
                                    FdeltaL,
                                    FMaxIter,
                                    CheckCancel);


        if FCanceled then
          break;

        inc(i);
      until (n = 0) or (abs(MLTree.LogLikelihood-dL) < FDeltaL) or (i = MaxIter);

    end;

    if FCanceled then
      FreeAndNil(MLTree)
    else
    begin
      if assigned(CheckCancel) then
        FCanceled := CheckCancel(0, 'Optimizing final tree');
      if Model.BootTable = nil then
      begin
        MLTree.NoOfThreads := NoOfThreadsToUse; // GS - let the user decide
        MLTree.Optimize(CheckCancel, 'Optimizing final tree');
      end;
      MLTree.ResetL0;
      MLTree.Root.des1.ase := MLTree.Root.des2.ase;
    end;
  except on E: EOutOfMemory do
    begin
      MessageDlg('''MEGA has run out of addressable memory.  This means that the analysis was not able to be completed.  The analysis may be able to run by adjusting the analysis preferences to consume less memory.''', mtError, [mbOK], 0);
    end;
  end;
  finally
    DestroyMatrix(D, 5);
    DestroyMatrix(DD, 5);

    for i := 0 to Model.NoOfRates-1 do
    begin
      if dL1[i] <> nil then
      begin
        for j := 0 to NoOfSites do
          FreeMemAndNil(dL1[i,j]);
        FreeMemAndNil(dL1[i]);
      end;
    end;
    for i := 0 to Model.NoOfRates-1 do
    begin
      if dL2[i] <> nil then
      begin
        for j := 0 to NoOfSites do
          FreeMemAndNil(dL2[i,j]);
        FreeMemAndNil(dL2[i]);
      end;
    end;
    setlength(dL1, 0);
    setlength(dL2, 0);

  end;
end;
}
{
procedure TMLTreeAnalyzer.SearchMLTree(makeinitialtree: boolean);
var
  MLTreeSearchThread: TMLTreeSearchThread;
begin
  MLTreeSearchThread := nil;
  try
    MLTreeSearchThread := TMLTreeSearchThread.Create(Self);
    MLTreeSearchThread.NoOfThreads := NoOfThreadsToUse;
    MLTreeSearchThread.Start;
    MLTreeSearchThread.WaitFor;
  finally
    if MLTreeSearchThread <> nil then
      MLTreeSearchThread.Free;
  end;

end;
}
function TMLTreeAnalyzer.GetProbOfRateCategoryAtSite(SiteIndex: integer; var p: array of extended): boolean;
begin
  result := MLTree.GetProbOfRateCategoryAtSite(SiteIndex, p);
end;

function TMLTreeAnalyzer.GetMLRateCategoryAtSite(SiteIndex: integer): integer;
begin
  result := MLTree.GetMLRateCategoryAtSite(SiteIndex);
end;

function TMLTreeAnalyzer.GetMLRateAtSite(SiteIndex: integer): extended;
begin
  result := MLTree.GetMLRateAtSite(SiteIndex);
end;

procedure TMLTreeAnalyzer.InitAncStateEst;
begin
//  MLTree.ResetL0;
end;

function TMLTreeAnalyzer.GetRate(index: integer): extended;
begin
  result := Model.Rate[index];
end;

function SortByProb(item1,item2: pointer):integer;
begin
  if TAncStateRec(item1^).Prob < TAncStateRec(item2^).Prob then
    result := 1
  else if TAncStateRec(item1^).Prob > TAncStateRec(item2^).Prob then
    result := -1
  else
    result := 0;
end;

function SortByName(item1,item2:pointer):integer;
begin
  Result := AnsiCompareStr(TAncStateRec(item1^).Name, TAncStateRec(item2^).Name);
end;


function TMLTreeAnalyzer.GetAncStateProbNotSorted(NodeIndex,
  SiteIndex: integer; var State: array of TAncStateRec
  ): boolean;
begin
  result := MLTree.GetAncStateProb(NodeIndex, SiteIndex, state);
end;

procedure TMLTreeAnalyzer.GenerateCalibratedClockTreeExport(var ExportList: TStringList; const NamesList: TStringList; const NodeLabels: TStringList; const MinTimes: array of Extended; const MaxTimes: array of Extended);
begin
  MLTree.ReltimeComputer.GenerateCalibratedClockTreeExport(ExportList, NamesList, NodeLabels, MinTimes, MaxTimes);
end;

procedure TMLTreeAnalyzer.GenerateSampledTimesClockTreeExport(var ExportList: TStringList; const NamesList: TStringList; const NodeLabels: TStringList; const SampleTimes: array of Extended);
begin
  MLTree.ReltimeComputer.GenerateSampledTimesClockTreeExport(ExportList, NamesList, NodeLabels, SampleTimes);
end;

function TMLTreeAnalyzer.GenerateNexusReltimeExport(NamesList: TStringList; HasCalibrations: Boolean): TStringList;
var
  Writer: TNexusWriter;
begin
  Writer := nil;

  try
    Writer := TNexusWriter.Create;
    if HasCalibrations then
      Writer.SetData(MLTree.RelTimeComputer.Root, MLTree.RelTimeComputer.NodeArray, MLTree.RelTimeComputer.TimeFactor, NamesList, NoOfSeqs)
    else
      Writer.SetData(MLTree.RelTimeComputer.Root, MLTree.RelTimeComputer.NodeArray, NamesList, NoOfSeqs);
    Writer.MinDivTimeFunc := MLTree.RelTimeComputer.MinDivTime;
    Writer.MaxDivTimeFunc := MLTree.RelTimeComputer.MaxDivTime;
    Writer.DivTimeFunc := MLTree.RelTimeComputer.DivTime;
    Writer.HasReltimeStdErr := True;
    Writer.HasDataCoverage := True;
    Result := Writer.BuildNexusStrings;
  finally
    if Assigned(Writer) then
      Writer.Free;
  end;
end;

procedure TMLTreeAnalyzer.GenerateClockTreeExport(var ExportList: TStringList; const NamesList: TStringList; const NodeLabels: TStringList);
begin
  MLTree.ReltimeComputer.GenerateClockTreeExport(Exportlist, NamesList, NodeLabels);
end;

function TMLTreeAnalyzer.GenerateNodeMap(NamesList: TStringList): TStringList;
const
  IdFormatString = '%6d';
  SIdFormatString = '%6s';
var
  i: Integer;
  TempString: AnsiString;
  OtuName: AnsiString;
  TaxonColumnWidth: Integer;
  NodeLabel: AnsiString;
begin
  Result := TStringList.Create;

  // determine the width of the first column where taxa names will be written
  TaxonColumnWidth := 0;
  for i := 0 to NoOfSeqs - 1 do
  begin
    OtuName := NamesList[i];
    if Length(OtuName) > TaxonColumnWidth then
      TaxonColumnWidth := Length(OtuName);
  end;

  if TaxonColumnWidth > 100 then
    TaxonColumnWidth := 100
  else if TaxonColumnWidth < 15 then
    TaxonColumnWidth := 15
  else
    TaxonColumnWidth := TaxonColumnWidth + 3;

  // Write out a header line that gives names for each colum
  TempString := MakePrettyTaxonNameString('NodeLabel', TaxonColumnWidth) + #9;
  TempString := TempString + Format(SIdFormatString, ['NodeId']) + #9;
  TempString := TempString + Format(SIdFormatString, ['Des1']) + #9;
  TempString := TempString + Format(SIdFormatString, ['Des2']) + #9;

  Result.Add(TempString);

  // Write out the information for the leaf nodes
  for i := 0 to NoOfSeqs-1 do
  begin
    OtuName := NamesList[i];
    OtuName := MakePrettyTaxonNameString(OtuName, TaxonColumnWidth);
    TempString := OtuName + #9; // Taxon name
    TempString := TempString + Format(IdFormatString, [MLTree.Node[i].index + 1]) + #9; // NodeId
    TempString := TempString + Format(SIdFormatString, ['-']) + #9; // Desc1
    TempString := TempString + Format(SIdFormatString, ['-']) + #9; // Desc2

    Result.Add(Trim(TempString));
  end;

  // Write out the information for the internal nodes
  for i := NoOfSeqs to 2*NoOfSeqs-2 do
  begin
    NodeLabel := 'Node_' + IntToStr(MLTree.Node[i].index + 1);
    TempString := MakePrettyTaxonNameString(NodeLabel, TaxonColumnWidth) + #9;

    TempString := TempString + Format(IdFormatString, [MLTree.Node[i].index + 1]) + #9;
    TempString := TempString + Format(IdFormatString, [MLTree.Node[i].des1.index+1]) + #9;
    TempString := TempString + Format(IdFormatString, [MLTree.Node[i].des2.index+1]) + #9;

    Result.Add(Trim(TempString));
  end;
end;

function TMLTreeAnalyzer.GetAncStateProb(NodeIndex, SiteIndex: integer; Probs: TStringList): boolean;
var
  ResultList : array of TAncStateRec;
  SortedResults : TList;
  i : integer;
begin
  try
    probs.clear;
    if Model.SeqDataType = DNA then
      SetLength(ResultList, 4)
    else
      SetLength(ResultList, 20);
    SortedResults := TList.Create;
    Result := GetAncStateProbNotSorted(NodeIndex, SiteIndex, ResultList);
    for i := 0 to Length(ResultList)-1 do
      SortedResults.Add(@resultList[i]);
    SortedResults.Sort(SortByProb);
    for i := 0 to SortedResults.Count-1 do
      Probs.Add(TAncStateRec(SortedResults[i]^).Name+'='+FloatToStrF(TAncStateRec(SortedResults[i]^).Prob,ffFixed,12,6));
  finally
    FreeAndNil(SortedResults);
  end;
end;

function TMLTreeAnalyzer.GetAncStateProb(NodeIndex, SiteIndex: Int64; var Probs: TAncStateRecArray): Boolean;
var
  i : integer = 0;
  aRec: TAncStateRec;
  p: Pointer = nil;
begin
  try
    if not Assigned(FAncestralStatesSorter) then
      FAncestralStatesSorter := TList.Create
    else
      FAncestralStatesSorter.Clear;
    if (Model.SeqDataType = DNA) then
    begin
      if (Length(FTempAncestralStates) <> 4) then
        SetLength(FTempAncestralStates, 4)
    end
    else if (Length(FTempAncestralStates) <> 20) then
      SetLength(FTempAncestralStates, 20);

    Result := GetAncStateProbNotSorted(NodeIndex, SiteIndex, FTempAncestralStates);
    for i := 0 to Length(FTempAncestralStates) - 1 do
      FAncestralStatesSorter.Add(@FTempAncestralStates[i]);
    FAncestralStatesSorter.Sort(SortByProb);
    for i := 0 to FAncestralStatesSorter.Count - 1 do
    begin
      p := FAncestralStatesSorter[i];
      aRec := TAncStateRec(p^);
      Probs[i].Name := aRec.Name;
      Probs[i].Prob := aRec.Prob;
    end;
  finally
    FAncestralStatesSorter.Clear;
  end;
end;

function TMLTreeAnalyzer.GetExpectedStateProb(NodeIndex, SiteIndex: integer; Probs: TStringList): boolean;
var
  ResultList : array of TAncStateRec;
  SortedResults : TList;
  i : integer;
begin
  try
    probs.clear;
    if Model.SeqDataType = DNA then
      SetLength(ResultList, 4)
    else
      SetLength(ResultList, 20);
    SortedResults := TList.Create;
    Result :=  MLTree.GetExpectedStateProb(NodeIndex, SiteIndex, ResultList);
    for i := 0 to Length(ResultList)-1 do
      SortedResults.Add(@resultList[i]);
    SortedResults.Sort(SortByProb);
    for i := 0 to SortedResults.Count-1 do
      Probs.Add(TAncStateRec(SortedResults[i]^).Name+'='+FloatToStrF(TAncStateRec(SortedResults[i]^).Prob,ffFixed,12,10));
  finally
    FreeAndNil(SortedResults);
  end;
end;

function TMLTreeAnalyzer.GetExpectedStateProb(NodeIndex, SiteIndex: integer): TAncStateRecArray;
var
  i: Integer;
  temp : Array[0..19] of TAncStateRec;
  status: Boolean = True;
begin
  if Model.SeqDataType = DNA then
    SetLength(Result, 4)
  else
    SetLength(Result, 20);
  status := MLTree.GetExpectedStateProb(NodeIndex, SiteIndex, Result);
  if not status then
    for i := Low(Result) to High(Result) do
      Result[i].Prob := 0;
  if not Assigned(FAncestralStatesSorter) then
    FAncestralStatesSorter := TList.Create
  else
    FAncestralStatesSorter.Clear;
  for i := 0 to Length(Result) - 1 do
    FAncestralStatesSorter.Add(@Result[i]);
  FAncestralStatesSorter.Sort(SortByName);
  for i := 0 to Length(Result) - 1 do
    temp[i] := TAncStateRec(FAncestralStatesSorter[i]^);
  for i := 0 to Length(Result) - 1 do
  begin
    Result[i].Prob := temp[i].Prob;
    Result[i].Name := temp[i].Name;
  end;
end;

function TMLTreeAnalyzer.GetExpectedStateProbArray(NodeIndex: Integer): TAncStateRecArrayArray;
var
  i: Integer;
begin
  SetLength(Result, NoOfSites);
  for i := 1 to NoOfSites do
    Result[i - 1] := GetExpectedStateProb(NodeIndex, i);
end;


function TMLTreeAnalyzer.GetAncStateProbAlphabetical(NodeIndex, SiteIndex: integer; var State: array of TAncStateRec): boolean;
var
  TempArray : Array[0..19] of TAncStateRec;
  i : integer;
  DnaIndices: array[0..3] of Integer = (0,2,3,1);
  AminoIndices: array[0..19] of Integer = (0,4,3,6,13,7,8,9,11,10,12,2,14,5,1,15,16,19,17,18);
begin
  Result := GetAncStateProbNotSorted(NodeIndex, SiteIndex, TempArray);
  if FModel.SeqDataType = DNA then
  begin
    for i := 0 to 3 do
      State[i] := TempArray[DnaIndices[i]];
  end
  else
  begin
    for i := 0 to 19 do
      State[i] := TempArray[AminoIndices[i]];
  end
end;


{$IFNDEF VISUAL_BUILD}
procedure TMLTreeAnalyzer.ExportPredictLivingSequences(index: Integer=1);
var
  RowColor: TColor;
  TempStr: String;
  i: integer;
  site:integer;
  DataStringGrid : TStringList = nil;
  TextOut: TStringList = nil;
  ExportType : TExportType;
  ExcelComponent : TExcelWrite = nil;
  Probability : double;
  Rect : TRect;
  AncStateProbabilities : Array of TAncStateRec;
  ExpectedStateProb: TStringList = nil;
  IsML, IsText: Boolean;
  selNodeIndex: Integer;
  SelNodeName: String;
  status: Boolean = False;
begin
  SelNodeIndex := index;
  SelNodeName :=  SeqNames[index - 1];
  IsML := true;

  try try
    if FModel.SeqDataType <> DNA then
      SetLength(AncStateProbabilities, 20)
    else
      SetLength(AncStateProbabilities, 4);

    ExportType := OutputFileTypeToExportType(D_MegaMain.OutputFormat);
    if ExportType = EXnone then
      Exit;
    IsText := (ExportType = ExText);

    if IsText then
      Error_NV('Text format not supported, try Excel or CSV')
    else
    begin
      ExcelComponent := TExcelWrite.Create(nil);
      ExcelComponent.IsXLS := True;
      ExcelComponent.Add(Format('Living Sequence Prediction Results For "%s"', [SelNodeName]));
      ExcelComponent.WriteLine();
      Rect := ExcelComponent.LastCellWriteXY();
      Rect.Right := Rect.Right + 2 + Length(AncStateProbabilities);
      ExcelComponent.MergeCells(Rect, aCenter);
      ExcelComponent.BoldCells(Rect);
      ExcelComponent.Add('Site #');
      if isML then
        ExcelComponent.Add('Probability')
      else
        ExcelComponent.Add('State');
      ExcelComponent.WriteLine();
      Rect := ExcelComponent.LastCellWriteXY();
      ExcelComponent.AddBlankCell;

      status := GetAncStateProbAlphabetical(0, 1, AncStateProbabilities);
      for i :=0 to Length(AncStateProbabilities)-1 do
      begin
        TempStr := AncStateProbabilities[i].Name;
        if (FModel.SeqDataType = DNA) AND (TempStr = 'T') then
          TempStr := 'T/U';
        ExcelComponent.Add(TempStr);
        rect := ExcelComponent.LastCellWriteXY();
        Rect.Left := Rect.Left -1;
        Rect.Right := Rect.Right -1;
        ExcelComponent.AlignCells(Rect, aRight);
      end;

      ExcelComponent.WriteLine();
      Rect.Left := 0;
      Rect.Right := Rect.Left;
      Rect.Top := Rect.Top -1;
      ExcelComponent.AlignCells(Rect, aCenter, aTop);
      Rect.Left := 1;
      Rect.Right := Rect.Left;
      ExcelComponent.AlignCells(Rect, aCenter, aTop);
      Rect.Left := 2;
      Rect.Right := Rect.Left;
      ExcelComponent.AlignCells(Rect, aCenter, aTop);
      Rect.Left := 2;

      Rect.Right := Rect.Left+ Length(AncStateProbabilities)-1;
      Rect.Bottom := Rect.Top;
      ExcelComponent.MergeCells(Rect, aCenter, aTop);
      Rect.Bottom := Rect.Bottom + 1;
      Rect.Top := Rect.Bottom;
      Rect.Left := 0;
      Rect.Right := 6;
      ExcelComponent.ColorCells(Rect, RGB(0,0,0), xlBorderBottom);
    end;

    for site:=1 to NoOfSites do //Tree.MaxSiteIndex+1 do     //check to make sure that nodes aren't different node number on different sites
    begin
      if not IsText then
      begin
        if (site mod 2) = 0 then
          RowColor := TColor($C3E5EB)
        else
          RowColor := RGB(255,255,255);
        ExcelComponent.Add(site, RowColor);
        Rect := ExcelComponent.LastCellWriteXY();
        Rect.Left := 0;
        Rect.Right := 0;
        ExcelComponent.MergeCells(Rect, aCenter);
        Rect := ExcelComponent.LastCellWriteXY();
        Rect.Left := Rect.Left -1;
        Rect.Right := Rect.Left;
        ExcelComponent.AlignCells(Rect, aLeft);
        Rect := ExcelComponent.LastCellWriteXY();
        Rect.Left := Rect.Left -1;
        Rect.Right := Rect.Left;
        ExcelComponent.AlignCells(Rect, aLeft);

        try
          ExpectedStateProb := TStringList.Create;
          status := GetExpectedStateProbAlphabetical(SelNodeIndex-1, site, ExpectedStateProb);
          for i := 0 to Length(AncStateProbabilities) - 1 do
          begin
            if status then
              Probability := StrToFloat(ExpectedStateProb.Values[ExpectedStateProb.Names[i]])
            else
              Probability := 0;
            if CompareValue(Probability, 0, FP_CUTOFF) = 0 then
              ExcelComponent.Add(0, RowColor)
            else
              ExcelComponent.Add(Probability, RowColor);
          end;
          finally
            FreeAndNil(ExpectedStateProb);
          end;

        ExcelComponent.WriteLine();
      end;
    end;
    //if not IsText then
    //  ExcelComponent.SaveFile(SaveLocation);
    if (ExportType = EXcsvSave) then
      ExcelComponent.SaveFile(NextAvailableFilenameNV(Format('_%s.csv', [SelNodeName])), ExportCsv)
    else if (ExportType = EXtext) then
      TextOut.SaveToFile(NextAvailableFilenameNV(Format('_%s.txt', [SelNodeName])))
    else
      raise Exception.Create('invalid export type(csv or txt required)');
  except on E: Exception do
    Error_NV('MEGA has encountered an exception: ' + E.Message);
  end;
  finally
    FreeAndNil(DataStringGrid);
    FreeAndNil(ExcelComponent);
    FreeAndNil(TextOut);
  end;
end;

procedure TMLTreeAnalyzer.ExportAllAncStates(NamesList: TStringList; isRooted: Boolean);
var
  Base: integer = -1;
  Node, Site:integer;
  ExportType : TExportType;
  AncStateProbabilities : Array of TAncStateRec;
  probs: TAncStateRecArrayArray;
  NodeMap: TStringList = nil;
  AncStateFile: TextFile;
  Delim: Char = #0;
  sb: TMegaStringBuilder = nil;
  status: Boolean = False;
begin
  try
    SetLength(probs, Length(MLTree.Node) - 1);
    sb := TMegaStringBuilder.Create;
    AssignFile(AncStateFile, NextAvailableFilenameNV('.csv'));
    Rewrite(AncStateFile);
    if FModel.SeqDataType <> DNA then
      SetLength(AncStateProbabilities, 20)
    else
      SetLength(AncStateProbabilities, 4);
    for Node := Low(probs) to High(probs) do
      SetLength(probs[Node], Length(AncStateProbabilities));
    ExportType := OutputFileTypeToExportType(D_MegaMain.OutputFormat);
    if ExportType = EXcsvSave then
      Delim := ','
    else
      Delim := ' ';
    sb.Add('site');
    sb.Add(Delim);
    sb.Add('base');
    for Node :=0 to Length(MLTree.Node)- 2 do
    begin
      if isRooted or (MLTree.Node[Node] <> MLTree.Root) then
      begin
        sb.Add(Delim);
        sb.Add('Node_');
        sb.Add(IntToStr(Node + 1));
      end;
    end;
    WriteLn(AncStateFile, sb.GenerateString);
    sb.Clean;

    for Site := 0 to MLTree.NoOfSites-1 do
    begin
      for Node := 0 to Length(MLTree.Node) - 2 do
      begin
        if isRooted or (MLTree.Node[Node] <> MLTree.Root) then
        begin
          status := GetAncStateProbAlphabetical(Node, (Site + 1), AncStateProbabilities);
          for base := Low(AncStateProbabilities) to High(AncStateProbabilities) do
            if status then
            begin
              probs[Node][base].Name := AncStateProbabilities[base].Name;
              probs[Node][base].Prob := AncStateProbabilities[base].Prob;
            end
            else
            begin
              probs[Node][base].Prob := 0;
              probs[Node][base].Name := AncStateProbabilities[base].Name;
            end;
        end;
      end;

      for base := 0 to Length(AncStateProbabilities) - 1 do
      begin
        sb.Add(IntToStr(Site + 1));
        sb.Add(Delim);
        sb.Add(AncStateProbabilities[base].Name);
        for Node := Low(probs) to High(probs) do
        begin
          if isRooted or (MLTree.Node[Node] <> MLTree.Root) then
          begin
            sb.Add(Delim);
            sb.Add(Format('%.3e', [probs[Node][Base].Prob]));
          end;
        end;

        WriteLn(AncStateFile, sb.GenerateString);
        sb.Clean;
      end;
   end;
   NodeMap := GenerateNodeMap(NamesList);
   NodeMap.SaveToFile(NextAvailableFilenameNV('_nodeMap.txt'));
  finally
    if Assigned(sb) then
      sb.Free;
    CloseFile(AncStateFile);
    if Assigned(NodeMap) then
      NodeMap.Free;
  end;
end;
{$ENDIF}


procedure TMLTreeAnalyzer.WriteToFile(var data: File);
type
  TArrayOfChar = array [0..MaxPointerElts-1] of AnsiChar;
  PArrayOfChar = ^TArrayOfChar;
var
  tree: TTreeData;
  buffer : PArrayOfChar;
  n: integer;
  i,AInt: integer;
  AExt: Extended;
  b: Boolean;
begin
  AInt := 0;
  AExt := 0;
  BlockWrite(data, FMLTreeAnalyzerVersion, sizeof(integer));
  BlockWrite(data, FNeedsRootByOutgroup, SizeOf(FNeedsRootByOutgroup));
  BlockWrite(data, FIsLittleBootstrap, SizeOf(Boolean));
  BlockWrite(data, FNoOfRates, sizeof(integer));
  BlockWrite(data, FSearchLevel, sizeof(integer));
  BlockWrite(data, AInt, sizeof(integer));
  BlockWrite(data, FIsGlobalClock, sizeof(boolean));
  BlockWrite(data, FGlobalClockLevel, sizeof(integer));
  BlockWrite(data, FNoOfClockOTUs, sizeof(integer));
  BlockWrite(data, AExt, SizeOf(extended));

  // values for timetrees, need to do it this way or else every thing needs to be recalculated
  if Assigned(MLTree) and Assigned(MLTree.ReltimeComputer) then
    n := MLTree.RelTimeComputer.NumNodes
  else
    n := 0;
  BlockWrite(data, n, SizeOf(Integer));
  if n > 0 then
    for i := 0 to n - 1 do
    begin
      AExt := MLTree.RelTimeComputer.Node[i].rate;
      BlockWrite(data, AExt, SizeOf(Extended));
      AExt := MLTree.RelTimeComputer.Node[i].vh;
      BlockWrite(data, AExt, SizeOf(Extended));
      AExt := MLTree.RelTimeComputer.Node[i].vb;
      BlockWrite(data, AExt, SizeOf(Extended));
      AExt := MLTree.RelTimeComputer.Node[i].vr;
      BlockWrite(data, AExt, SizeOf(Extended));

      // From ver. 3
      AExt := MLTree.RelTimeComputer.Node[i].height;
      BlockWrite(data, AExt, SizeOf(Extended));
      AExt := MLTree.RelTimeComputer.Node[i].maxh;
      BlockWrite(data, AExt, SizeOf(Extended));
      AExt := MLTree.RelTimeComputer.Node[i].minh;
      BlockWrite(data, AExt, SizeOf(Extended));
    end;

// SeqData
  n := NoOfSeqs;
  BlockWrite(data, n, sizeof(integer));
  n := NoOfSites;
  BlockWrite(data, n, sizeof(integer));

  GetMem(buffer, NoOfSites+1);
  for i := 0 to NoOfSeqs-1 do
  begin
    StrPCopy(PAnsiChar(buffer), Seqs[i]);
    BlockWrite(data, buffer^, NoOfSites);
  end;
  FreeMemAndNil(buffer);

// Model
  n := 0;
  if Model.SeqDataType = DNA then
  begin
    if Model is TJCModel then
      n := 1
    else if Model is TK2Model then
      n := 2
    else if Model is TT3Model then
      n := 3
    else if Model is THKYModel then
      n := 4
    else if Model is TTN93Model then
      n := 5
    else if Model is TGTRModel then
      n := 6
  end
  else
  begin
    if Model is TDayhoffModel then
      n := 11
    else if Model is TJTTModel then
      n := 12
    else if Model is TWAGModel then
      n := 13
    else if Model is TmtREV24Model then
      n := 14
    else if Model is TcpREVModel then
      n := 15
    else if Model is TrtREVModel then
      n := 16
    else if Model is TPoissonModel then
      n := 17
    else if Model is TLGModel then
      n := 18;
  end;
  BlockWrite(data, n, SizeOf(Integer));
  Model.WriteToFile(data);

// MLTree
  tree := TTreeData.Create(NoOfSeqs, true, false, true);
  MLTree.GetTreeData(tree);
  tree.WriteToFile(data);
  tree.Free;

  if assigned(InitTree) then
    n := 1
  else
    n := 0;
  BlockWrite(data, n, SizeOf(Integer));
  if n = 1 then
  begin
    b := InitTree.isSE;
    BlockWrite(data, b, SizeOf(Boolean));
    InitTree.WriteToFile(data);
  end;

// Sampling Times
  i := Length(SamplingTime);
  BlockWrite(data, i, SizeOf(Integer));
  if i > 0 then
    for i := 0 to NoOfSeqs-1 do
      BlockWrite(data, SamplingTime[i], SizeOf(Extended));
  AExt := MLTree.LogLikelihood;
  BlockWrite(data, AExt, SizeOf(Extended));

// From ver. 3
  AExt := MLTree.RelTimeComputer.TimeFactor;
  BlockWrite(data, AExt, SizeOf(Extended));
  AExt := MLTree.RelTimeComputer.LatestTime;
  BlockWrite(data, AExt, SizeOf(Extended));
  AExt := MLTree.RelTimeComputer.MaxTimeFactor;
  BlockWrite(data, AExt, SizeOf(Extended));
  AExt := MLTree.RelTimeComputer.MinTimeFactor;
  BlockWrite(data, AExt, SizeOf(Extended));
end;

function TMLTreeAnalyzer.ReadFromFile(var data: File; SessionVersion: Integer; OutGroupMembers: TBoolArray):boolean;
type
  TArrayOfChar = array [0..MaxPointerElts-1] of AnsiChar;
  PArrayOfChar = ^TArrayOfChar;
var
  tree: TTreeData = nil;
  buffer : PArrayOfChar = nil;
  i: Integer = -1;
  n: Integer = -1;
  m: integer = -1;
  TempRates: array of Extended;
  TempVarH: array of Extended; // variance for node heights
  TempVarB: array of Extended; // variance for branch lengths
  TempVarR: array of Extended; // variance for rates
  TempHeight: array of Extended; // variance for rates
  TempMaxH: array of Extended; // variance for rates
  TempMinH: array of Extended; // variance for rates
  AInt: integer = -1;
  AExt: Extended = -1;
  b: Boolean =  False;
  origInvar: Extended = 0;
  origGamma: Extended = 0;
begin
  SetLength(TempRates, 0);
  SetLength(TempVarH, 0);
  SetLength(TempVarB, 0);
  SetLength(TempVarR, 0);
  SetLength(TempHeight, 0);
  SetLength(TempMaxH, 0);
  SetLength(TempMinH, 0);

  if assigned(SeqData) then
    SeqData.Clear
  else
    SeqData := TStringList.Create;

  if assigned(FModel) then
    FreeAndNil(FModel);

  if assigned(MLTree) then
    FreeAndNil(MLTree);

  if assigned(InitTree) then
    FreeAndNil(InitTree);

  result := false;

  BlockRead(data, FMLTreeAnalyzerVersion, sizeof(integer));
  if SessionVersion > 1004 then
    BlockRead(data, FNeedsRootByOutgroup, SizeOf(FNeedsRootByOutgroup));
  if SessionVersion >= 1207 then
    BlockRead(data, FIsLittleBootstrap, SizeOf(Boolean));

  BlockRead(data, FNoOfRates, sizeof(integer));
  BlockRead(data, FSearchLevel, sizeof(integer));

  BlockRead(data, AInt, sizeof(integer));

  BlockRead(data, FIsGlobalClock, sizeof(boolean));
  BlockRead(data, FGlobalClockLevel, sizeof(integer));
  BlockRead(data, FNoOfClockOTUs, sizeof(integer));

  BlockRead(data, AExt, SizeOf(extended));

  if SessionVersion >= 602 then
  begin
    // rates for reltime trees
    BlockRead(data, n, SizeOf(Integer));
    SetLength(TempRates, n);
    SetLength(TempVarH, n);
    SetLength(TempVarB, n);
    SetLength(TempVarR, n);
    SetLength(TempHeight, n);
    SetLength(TempMaxH, n);
    SetLength(TempMinH, n);
    if (SessionVersion < 701) or (n > 0) then
      for i := 0 to n - 1 do
      begin
        BlockRead(data, TempRates[i], SizeOf(Extended));
        BlockRead(data, TempVarH[i], SizeOf(Extended));
        BlockRead(data, TempVarB[i], SizeOf(Extended));
        BlockRead(data, TempVarR[i], SizeOf(Extended));

        // From ver. 3
        if FMLTreeAnalyzerVersion >= 3 then
        begin
          BlockRead(data, TempHeight[i], SizeOf(Extended));
          BlockRead(data, TempMaxH[i], SizeOf(Extended));
          BlockRead(data, TempMinH[i], SizeOf(Extended));
        end;
      end;
  end;

// SeqData
  BlockRead(data, n, sizeof(integer));
  BlockRead(data, m, sizeof(integer));
  GetMem(buffer, m+1);
  buffer[m] := #0;
  for i := 0 to n-1 do
  begin
    BlockRead(data, buffer^, m);
    Seqs.Add(AnsiString(buffer^));
  end;
  FreeMemAndNil(buffer);

// Model
  BlockRead(data, n, sizeof(integer));

  case n of
     1: FModel := TJCModel.Create(0, false, 1);
     2: FModel := TK2Model.Create(0, false, 1);
     3: FModel := TT3Model.Create(0, false, 1);
     4: FModel := THKYModel.Create(0, false, 1);
     5: FModel := TTN93Model.Create(0, false, 1);
     6: FModel := TGTRModel.Create(0, false, 1);
    11: FModel := TDayhoffModel.Create(0, false, false, 1);
    12: FModel := TJTTModel.Create(0, false, false, 1);
    13: FModel := TWAGModel.Create(0, false, false, 1);
    14: FModel := TmtREV24Model.Create(0, false, false, 1);
    15: FModel := TcpREVModel.Create(0, false, false, 1);
    16: FModel := TrtREVModel.Create(0, false, false, 1);
    17: FModel := TPoissonModel.Create(0, false, false, 1);
    18: FModel := TLGModel.Create(0, false, false, 1);
  else
    raise Exception.Create('failed to read model type');
  end;

  if not FModel.ReadFromFile(data) then
    raise Exception.Create('failed to read ML model data');
  origInvar := FModel.Invar;
  origGamma := FModel.Gamma;
  FModel.SetParamsFromSeqs(Seqs);
  FModel.Invar := origInvar; // needs reset to final value from the analysis
  FModel.Gamma := origGamma;
// MLTree
  tree := TTreeData.Create(NoOfSeqs, true, true, false);
  tree.ReadFromFile(data, SessionVersion);
  if Length(OutgroupMembers) > 0 then
    for i := Low(OutgroupMembers) to High(OutgroupMembers) do
      tree.IsOutgroupMember[i] := OutgroupMembers[i];
  MLTree := TMLTree.Create(Seqs, tree, FModel, FNeedsRootByOutgroup);
  MLTree.Initialize(false, true);
//    MLTree.Optimize(nil, ''); // takes as long as the original analysis but with this commented out, log likelihood differs
  InitAncStateEst;

  if (SessionVersion >= 602) and (Length(TempRates) > 0) then
  begin
    MLTree.RelTimeComputer.InitFromSessionFile(tree);
    for i := 0 to Length(TempRates) - 1 do
    begin
      MLTree.RelTimeComputer.Node[i].rate := TempRates[i];
      MLTree.RelTimeComputer.Node[i].vh := TempVarH[i];
      MLTree.RelTimeComputer.Node[i].vb := TempVarB[i];
      MLTree.RelTimeComputer.Node[i].vr := TempVarR[i];

      // From ver. 3
      if FMLTreeAnalyzerVersion >= 3 then
      begin
        MLTree.RelTimeComputer.Node[i].Height := TempHeight[i];
        MLTree.RelTimeComputer.Node[i].maxh := TempMaxH[i];
        MLTree.RelTimeComputer.Node[i].minh := TempMinH[i];
      end;
    end;
  end;

  BlockRead(data, n, SizeOf(Integer));
  if n = 1 then
  begin
    if SessionVersion >= 608 then
    begin
      BlockRead(data, b, SizeOf(Boolean));
      InitTree := TTreeData.Create(NoOfSeqs, tree.isBLen, b, tree.isStats);
    end
    else
      InitTree := TTreeData.Create(NoOfSeqs, tree.isBLen, false, tree.isStats);
    InitTree.ReadFromFile(data, SessionVersion);
  end;
  tree.Free;

  // Sampling Times
  if FMLTreeAnalyzerVersion >= 2 then
  begin
    BlockRead(data, n, SizeOf(Integer));
    if n > 0 then
    begin
      SetLength(Samplingtime, n);
      for i := 0 to NoOfSeqs-1 do
        BlockRead(data, SamplingTime[i], SizeOf(Extended));
    end;
  end;
  if SessionVersion >= 605 then
  begin
    BlockRead(data, AExt, SizeOf(Extended));
    MLTree.LogLikelihood := AExt;
  end;

  // From ver. 3
  BlockRead(data, AExt, SizeOf(Extended));
  MLTree.RelTimeComputer.TimeFactor := AExt;
  BlockRead(data, AExt, SizeOf(Extended));
  MLTree.RelTimeComputer.LatestTime := AExt;
  BlockRead(data, AExt, SizeOf(Extended));
  MLTree.RelTimeComputer.MaxTimeFactor := AExt;
  BlockRead(data, AExt, SizeOf(Extended));
  MLTree.RelTimeComputer.MinTimeFactor := AExt;

  result := true;
end;

function TMLTreeAnalyzer.GetExpectedStateProbAlphabetical(NodeIndex,
  SiteIndex: integer; var Probs: TStringList): boolean;
begin
  GetExpectedStateProb(NodeIndex, SiteIndex, Probs);
  Probs.CustomSort(SortAlpha); // sorts by name ACGT
  Result := Probs.Count > 0;
end;

function TMLTreeAnalyzer.MapNodeIndexToTreeDataIndex(NodeIndex: Integer): Integer;
begin
  if NodeIndex >= NoOfSeqs then
    Result := NodeIndex - NoOfSeqs
  else
    Result := NodeIndex + NoOfSeqs - 1;
end;


function SortAlpha(List: TStringList; Index1,
  Index2: Integer): Integer;
begin
  result := CompareText(List.Names[Index1], List.Names[Index2]);
end;

/// <summary>Return a TStringList that contains strings representing the current
/// values of all fields in this instance of TMLTreeAnalyzer.</summary>
/// <param name="Comment">Optional comment that will be added to the string list.</param>
/// <return>sTStringList of strings that can be treated as key/value pairs representing
/// the current state of all fields comprising this TMLTreeAnalyzer.</returns>
function TMLTreeAnalyzer.StateToStringList(Comment: AnsiString = ''): TStringList;
var
  MyStringList: TStringList;
  NewLine: AnsiString;
  TempStringList: TStringList;
  i: Integer;
begin
  NewLine := LineEnding;
  MyStringList := TStringList.Create();
  TempStringList := TStringList.Create();
  MyStringList.Add('TMLTreeAnalyzer=MyName');

  if Comment <> EmptyStr then
	MyStringList.Add('TMLTreeAnalyzer.Comment=' + Comment);

  MyStringList.Add('TMLTreeAnalyzer.InitialTreeOption=' + IntToStr(InitialTreeOption));
  MyStringList.Add('TMLTreeAnalyzer.FNoOfThreadsToUse=' + IntToStr(FNoOfThreadsToUse));
  MyStringList.Add('TMLTreeAnalyzer.FNoOfRates=' + IntToStr(FNoOfRates));
//  MyStringList.Add('TMLTreeAnalyzer.FNoOfClocks=' + IntToStr(FNoOfClocks));
  MyStringList.Add('TMLTreeAnalyzer.FSearchLevel=' + IntToStr(FSearchLevel));
//  MyStringList.Add('TMLTreeAnalyzer.FMaxIter=' + IntToStr(FMaxIter));
  MyStringList.Add('TMLTreeAnalyzer.FGlobalClockLevel=' + IntToStr(FGlobalClockLevel));
  MyStringList.Add('TMLTreeAnalyzer.SearchFilter=' + FloatToStrF(SearchFilter, ffFixed, 5, 5));
//  MyStringList.Add('TMLTreeAnalyzer.FDeltaL=' + FloatToStrF(FDeltaL, ffFixed, 5, 5));
  MyStringList.Add('TMLTreeAnalyzer.FIsGlobalClock=' + BoolToStr(FIsGlobalClock, True));
  MyStringList.Add('TMLTreeAnalyzer.FIsUserSuppliedTree=' + BoolToStr(FIsUserSuppliedTree, True));

  if SeqNames <> nil then
  begin
    MyStringList.Add('TMLTreeAnalyzer.SeqNames');
    for i := 0 to SeqNames.Count - 1 do
      MyStringList.Add('TMLTreeAnalyzer.SeqNames[' + IntToStr(i) + ']=' + SeqNames[i]);
    MyStringList.Add(NewLine);
  end;


//  MyStringList.Add('[Seqs (truncated)]');
//  for i := 0 to SeqData.Count - 1 do
//  begin
//    TempString := Truncate(SeqData[i]);
//	MyStringList.Add(TempString + '...');
//  end;
//  MyStringList.Add(NewLine);

  MyStringList.Add('TMLTreeAnalyzer.OptimizeMessage=' + OptimizeMessage);

  if InitTree <> nil then
  begin
	  MyStringList.Add(NewLine);
    MyStringList.Add('TMLTreeAnalyzer.InitTree=MyName');
	  TempStringList := InitTree.StateToStringList(False, Comment);
	  for i := 0 to TempStringList.Count - 1 do
		  MyStringList.Add(TempStringList[i]);
  end
  else
    MyStringList.Add('TMLTreeAnalyzer.InitTree=nil');

  if NonClockTree <> nil then
  begin
	  MyStringList.Add(NewLine);
    MyStringList.Add('TMLTreeAnalyzer.NonClockTree=MyName');
	  TempStringList := NonClockTree.StateToStringList(False, '');
	  for i := 0 to TempStringList.Count - 1 do
		MyStringList.Add(TempStringList[i]);
  end
  else
    MyStringList.Add('TMLTreeAnalyzer.NonClockTree=nil');

  if MLTree <> nil then
  begin
	  MyStringList.Add(NewLine);
    MyStringList.Add('TMLTreeAnalyzer.MLTree=MyName');
	  TempStringList := MLTree.StateToStringList(Comment);
	  for i := 0 to TempStringList.Count - 1 do
		MyStringList.Add(TempStringList[i]);
  end
  else
    MyStringList.Add('TMLTreeAnalyzer.MLTree=nil');


  if (ModelInfoList <> nil) and (ModelInfoList.Count > 0) then
  begin
    for i := 0 to ModelInfoList.Count - 1 do
    begin
      MyStringList.Add(NewLine);
      MyStringList.Add('TMLTreeAnalyzer.TModelInfo_' + IntToStr(i) + '=MyName');
      ModelInfoToStringList(ModelInfoList.Items[i], MyStringList);
    end;
  end
  else
    MyStringList.Add('TMLTreeAnalyzer.ModelInfoList=nil');


//
//  if FModel <> nil then
//  begin
//      MyStringList.Add(NewLine);
//	  MyStringList.Add('[~~~~ FModel ~~~~~]');
//	  TempStringList := FModel.StateToStringList();
//	  for i := 0 to TempStringList.Count - 1 do
//		MyStringList.Add(TempStringList[i]);
//	  MyStringList.Add(NewLine);
//  end;

  Result := MyStringList;
  FreeAndNil(TempStringList);
end;

function TMLTreeAnalyzer.AnchorExistingClockTree(aTree: TTreeData; var MinTime, MaxTime: array of Extended; MaxRateRatio: Extended): Boolean;
begin
  Result := MLTree.AnchorClockTree(aTree, MinTime, MaxTime, MaxRateRatio);
end;


end.
