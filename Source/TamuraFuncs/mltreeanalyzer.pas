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

unit MLTreeAnalyzer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFNDEF VISUAL_BUILD}
   MegaUtils_NV,
  {$ENDIF}
  LCLIntf, LCLType, Classes, SysUtils, Math, Forms, SyncObjs,
  MegaConsts, MegaUtils, MTreeEstFunc, MTreeData, MLTree, MLModels,
  MPTree, MRelTimeComputer, MRuntimeProgressDlg;


type
  TModelInfoList = class(TList)
  private
    function GetItems(index: integer): TModelInfo;
    procedure SetItems(index: integer; newitem: TModelInfo);
  public
    property Items[Index: integer]: TModelInfo read GetItems write SetItems; default;
    procedure Assign(Source: TModelInfoList);
  end;

  { TMLTreeAnalyzer }

  TMLTreeAnalyzer = class
    SeqNames: TStringList;
    OptimizeMessage: AnsiString;
    CheckCancel: TCheckCancelFunc;
    SubTaskCheckCancel: TCheckCancelFunc;
    RuntimeProgress: TRuntimeProgress;
    InitTree : TTreeData;
    MLTree   : TMLTree;
    SeqData  : TStringList;
    ModelInfoList : TModelInfoList;
    NonClockTree: TTreeData;
    NonClockModel: TGammaRateVariationModel;
    BLenFilter: extended;
    InitialTreeOption: integer;
    SamplingTime: array of extended;
  private
    FAncestralStatesSorter: TList;
    FModel   : TGammaRateVariationModel;
    FNeedsRootByOutgroup: Boolean;
    FNoOfThreadsToUse : integer;
    FNoOfRates  : integer;
    FSearchLevel: integer;

//    FDoubleBLenOpt: boolean;
    FMaxIter: integer;
    FDeltaL : extended;
    FIsGlobalClock: boolean;
    FGlobalClockLevel: integer;
    FNoOfClockOTUs: integer;
    FIsUserSuppliedTree: boolean;
    FCanceled: boolean;
    FHasRateStdErrs: Boolean; // currently we only have rate variances for bootstrap { TODO 1 -oglen -creltime : remove FHasRateStdErrs as soon as we can get stderrs for rates using an analytical method }
    FOriginalReltimes: ArrayOfExtended; { in the case of anchoring a clocktree, save original reltimes for in-house use to see how reltimes might have changed}
    FIsLittleBootstrap: Boolean;
    FNumTotalSites: Int64;
    function GetModelForInitialTree: TGammaRateVariationModel;
    function GetDevInitialLogLikelihood: Extended;
    procedure SetFreqTable(f: PArrayOfInt);
    function GetFreqTable: PArrayOfInt;
    function GetModel: TGammaRateVariationModel;
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

    procedure SetMaxIter(value: integer);
    procedure SetDeltaL(value: extended);

    procedure TieProc(var c1,c2: integer; c: array of Integer);

    procedure SetNoOfThreadsToUse(numThreads: integer);
    function GetNoOfThreadsToUse: integer;
    function MapNodeIndexToTreeDataIndex(NodeIndex: Integer): Integer;
    procedure CheckAbort;
  public
    StartTime, EndTime: TDateTime; { for bootstrap, if the first tree we search takes a long time, we can set TMLSearchThread to give progress for each search so that progress gets some kind of regular updates}
    function TimeTreeToNewickString(OtuNames: TStringList; UseTimeFactor: Boolean): String;
    procedure SetIsLittleBootstrap(aIsLittleBootstrap: Boolean; aNumTotalSites: Int64);
    property IsLittleBootstrap: Boolean read FIsLittleBootstrap;
    property NumSitesPerSubSample: Int64 read FNumTotalSites;
    property NoOfThreadsToUse : integer read GetNoOfThreadsToUse write SetNoOfThreadsToUse; // GS, added 8-17-2011 so we can give the user some control over this
    property LogLikelihood: extended read GetLogLikelihood;
    property IsInitialTree: boolean read GetIsInitialTree;
    property IsUserSuppliedTree: boolean read GetIsUserSuppliedTree; // this resolves a problem with caption expert. Initially, 'InitTree' was used to determine if the user supplied a tree or one was automatically generated. However, that variable gets modified in other places.
    property SearchLevel: integer read FSearchLevel write FSearchLevel;    //  SearchLevel(>2) = Dt4 or SearchLevel(<=2) = Dt2
    property Model: TGammaRateVariationModel read GetModel;
    property DefaultNoOfRates: integer read FNoOfRates write FNoOfRates;
    property NoOfRates: Integer read FNoOfRates write FNoOfRates;
    property Seqs: TStringList read GetSeqs write SetSeqs;
    property NoOfSeqs: integer read GetNoOfSeqs;
    property NoOfSites: integer read GetNoOfSites;
    property NoOfConfs: integer read GetNoOfConfs;
    property FreqTable: PArrayOfInt read GetFreqTable write SetFreqTable;
    property Rate[index: integer]: extended read GetRate;
//    property DoubleBLenOpt: boolean read FDoubleBLenOpt write FDoubleBLenOpt;

    property MaxNoOfIteration: integer read FMaxIter write SetMaxIter;
    property MinDeltaLogLikelihood: extended read FDeltaL write SetDeltaL;

    property IsGlobalClock: boolean read FIsGlobalClock write FIsGlobalClock;
    property GlobalClockLevel: integer read FGlobalClockLevel write FGlobalClockLevel;
//    property NoOfClocks: integer read FNoOfClocks;
    property NoOfClockOTUs: integer read FNoOfClockOTUs;

    procedure GetModelInfo(ModelInfo: TModelInfo);

    procedure SetTreeData(tree: TTreeData);
    procedure GetTreeData(tree: TTreeData);

    procedure ComputeInitialBLens;
    procedure MakeTree(method: integer);  // 0: NJ, 1: BIONJ, 2: ME, 3: MP;

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
    function GetAllAncSeqs: TAncestralSeqArray;
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
//    procedure Linearize;

    procedure TestDNAModels(numThreads: Integer);
    procedure TestProteinModels(numThreads: Integer);

    function PrepareSearchMLTree(makeinitialtree: boolean): boolean;
    function SearchMLTree(makeinitialtree: boolean; IsBStrapRep: Boolean; AProgress: TRuntimeProgress = nil): boolean;
    procedure ResetMLTree;
    procedure CancelSearch;

    function ReadFromFile(var data: File; SessionVersion: Integer; OutGroupMembers: TBoolArray):boolean;
    procedure WriteToFile(var data: File);
    function StateToStringList(Comment: AnsiString = ''): TStringList;
    constructor Create(seqs: TStringList; initialtree: TTreeData; model: TGammaRateVariationModel);
    destructor Destroy; override;
    procedure Assign(Source: TMLTreeAnalyzer);
    property Cancelled: Boolean read FCanceled;
    property DevInitialLogLikelihood: Extended read GetDevInitialLogLikelihood; { only used for development of baby bootstrap - later we can remove this}
    property NeedsRootByOutgroup: Boolean read FNeedsRootByOutgroup write SetNeedsRootByOutgroup;
  end;

  function SortAlpha(List: TStringList; Index1, Index2: Integer): Integer;

var
  ModelSelectionCS: TCriticalSection;

implementation
uses
  {$IFDEF DEBUG}LazLoggerBase,{$ENDIF}
  {$IFNDEF VISUAL_BUILD}
  MD_MegaMain, Graphics,ExcelWrite, Types,
  {$ENDIF}
  MGlobalSettings, mnexuswriter, dateutils,
  Dialogs, MLSearchThread, mmemutils;

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

procedure TModelInfoList.Assign(Source: TModelInfoList);
var
  i: Integer;
  AInfo: TModelInfo;
begin
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      AInfo := TModelInfo.Create;
      AInfo.Assign(TModelInfo(Source.Items[i]));
      Add(AInfo);
    end;
  end;
end;

function TModelInfoList.GetItems(index: integer): TModelInfo;
begin
  result := inherited Items[index];
end;

procedure TModelInfoList.SetItems(index: integer; newitem: TModelInfo);
begin
  inherited Items[index] := newitem;
end;

constructor TMLTreeAnalyzer.Create(seqs: TStringList; initialtree: TTreeData; model: TGammaRateVariationModel);
var
  i: integer;
begin
  inherited Create;
  FNeedsRootByOutgroup := False;
  SubTaskCheckCancel := nil;
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

  FMaxIter := 20;
  FDeltaL  := 1.0E-4;
  FGlobalClockLevel := 2;
  FNoOfClockOTUs       := 0;

  FNoOfRates   := 5;
  FHasRateStdErrs := False;
  FSearchLevel := 2;

  ModelInfoList := TModelInfoList.Create;

  BLenFilter := 3.5;
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
end;

destructor TMLTreeAnalyzer.Destroy;
var
  i: integer;
begin
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

procedure TMLTreeAnalyzer.SetMaxIter(value: integer);
begin
  FMaxIter := value;
  if Model <> nil then
    Model.MaxIter := value;
  if MLTree <> nil then
    MLTree.MaxIter := value;
end;

procedure TMLTreeAnalyzer.SetDeltaL(value: extended);
begin
  FDeltaL := value;
  if MLTree <> nil then
    MLTree.deltaL := value;
  if Model <> nil then
    Model.deltaL := value/10;
end;

procedure TMLTreeAnalyzer.SetSeqs(newseqs: TStringList);
var
  i: integer;
begin
  if newseqs.Count <> NoOfSeqs then exit;

  SeqData  := seqs;

  if MLTree <> nil then
    MLTree.Initialize(true);

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

function TMLTreeAnalyzer.GetModelForInitialTree: TGammaRateVariationModel; { per Sudhir's request, if chosen model is available for distance methods, use it for initial tree as well}
begin
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

procedure TMLTreeAnalyzer.SetNeedsRootByOutgroup(AValue: Boolean);
begin
  if FNeedsRootByOutgroup = AValue then Exit;
  FNeedsRootByOutgroup := AValue;
end;

function TMLTreeAnalyzer.GetNoOfSeqs: integer;
begin
  result := SeqData.Count;
end;

function TMLTreeAnalyzer.GetNoOfSites: integer;
begin
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
  k,i,j: QWord;
  n: QWord;
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
  k,i,j: QWord;
  n: QWord;
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
    ThreadIndex: integer;
    MainThread: TTestModelsThread;
    Done: boolean;
    InitTree: TTreeData;
    SeqData : TStringList;
    FCancelled: Boolean;

    procedure TestModels;
  protected
    procedure Execute; override;
  public
    constructor Create(parent: TTestModelsThread; index: integer; seqs: TStringList; tree: TTreeData);
    destructor Destroy; override;
    function GetApproximateSizeInBytes(const tree: TTreeData; var nsites: Integer; var nrates: Integer; const IsProteinData: Boolean): Int64;
    function CheckCancelProxy(Progress: Integer; Status: AnsiString): Boolean;
    property Cancelled: Boolean read FCancelled write FCancelled;
  end;

  { TTestModelsThread }

  TTestModelsThread = class(TTestModelsChildThread)
    ProgressMessage: string;
  private
    FMaxPercentAvailableRamToUse: Integer;
    FModelName: String;
    FNoOfModels: integer;
    FNoOfThreads: integer;
    ChildThread: array of TTestModelsChildThread;
    Models:    array of TGammaRateVariationModel;
    ModelInfoList: TModelInfoList;
    CurrentModelIndex: integer;
    ThreadMemSize: extended;
    FChildThreadIndex: Integer;
    function GetModelIndex(childIndex: Integer): integer;
    function CheckChildThreadDone: boolean;
    procedure SetNoOfThreads(n: integer);
    procedure UpdateProgress;
  protected
    procedure Execute; override;
  public
    MyCheckCancel: TProgressAndStatusCheckCancel;
    property NoOfModels: integer read FNoOfModels;
    property NoOfThreads: integer read FNoOfThreads write FNoOfThreads;
    property MaxPercentAvailableRamToUse: Integer read FMaxPercentAvailableRamToUse;
    constructor Create(seqs: TStringList; tree: TTreeData; m: TModelInfoList; n: integer; numThreads: Integer; MaxPercentRam: Integer = DEFAULT_MAX_PERCENT_RAM);
    destructor Destroy; override;
  end;

function TTestModelsChildThread.CheckCancelProxy(Progress: Integer; Status: AnsiString): Boolean;
begin
  Result := FCancelled;
end;

constructor TTestModelsChildThread.Create(parent: TTestModelsThread; index: integer; seqs: TStringList; tree: TTreeData);
begin
  inherited Create(true);

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
      modelName := AModel.GetDescription;
      if AModel is TProteinMatrixModel then
        if TProteinMatrixModel(AModel).UseFreq then
          modelName := modelName + ' + F';

        if FCancelled then
        begin
          Exit;
        end
        else
        begin
          AModel.SetParamsFromSeqs(SeqData);
          AMLTree := TMLTree.Create(SeqData, InitTree, AModel, False);
          try
            try
              AMLTree.IsFineGrainedTask := True;
              AMLTree.SubtaskCheckCancel := CheckCancelProxy;
              AMLTree.NoOfThreads := 1;
              AMLTree.Initialize(true);
              if AMLTree.Optimize(CheckCancelProxy) then
                GetModelInfo(MainThread.ModelInfoList[mindex], AMLTree)
              else
              begin
                FCancelled := True;
                MainThread.Cancelled := True;
                MainThread.Terminate;
              end;
            except
              on E:EAbort do
              begin
                FCancelled := True;
                MainThread.Cancelled := True;
                MainThread.Terminate;
              end;
            end;
          finally
            if Assigned(AMLTree) then
              AMLTree.Free;
          end;
        end;
    end;
  until (AModel = nil) or Terminated;
end;

procedure TTestModelsChildThread.Execute;
begin
  repeat
    TestModels;

    Done := true;
    while not Terminated do
      sleep(Random(500));
  until Terminated;
  if Assigned(MainThread) and Assigned(MainThread.ChildThread) and (ThreadIndex < Length(MainThread.ChildThread)) then
    MainThread.ChildThread[ThreadIndex] := nil;
end;

//////////////////

constructor TTestModelsThread.Create(seqs: TStringList; tree: TTreeData; m: TModelInfoList; n: integer; numThreads: Integer; MaxPercentRam: Integer = DEFAULT_MAX_PERCENT_RAM);
var
  tempMemAvailable: Int64 = 0;
  i: integer;
  approximateRamUsage: Int64;
  nsites: Integer = -1;
  nrates: Integer = -1;
begin
  Assert((MaxPercentRam > 25) and (MaxPercentRam <= 90), 'MaxPercentRam must be a value between 25 and 90');
  FCancelled := False;
  FMaxPercentAvailableRamToUse := MaxPercentRam;
  inherited Create(self, 0, seqs, tree);
  ThreadMemSize := GetApproximateSizeInBytes(tree, nsites, nrates, (n > 24));
  FreeOnTerminate := false;
  ModelSelectionCS := TCriticalSection.Create;
  ModelInfoList := m;
  if ModelInfoList.Count > 0 then
    for i := 0 to ModelInfoList.Count-1 do
      ModelInfoList[i].Free;
  ModelInfoList.Clear;
  for i := 0 to n-1 do
    ModelInfoList.Add(TModelInfo.Create);

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
  for i := 0 to  length(Models)-1 do
    Models[i].Free;

  setlength(Models, 0);
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
    result := result and ChildThread[i].Done;
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
      if TProteinMatrixModel(aModel).UseFreq then
        FModelName := FModelName + ' + F';
    if Terminated then
      result := -1;
  end;
  if assigned(MyCheckCancel) then
    Synchronize(UpdateProgress);
end;

procedure TTestModelsThread.UpdateProgress;
var
  progress: Integer;
begin
  progress := trunc((CurrentModelIndex)/NoOfModels*100);
  if progress < 0 then
    progress := 0;
  if Assigned(MyCheckCancel) then
    if MyCheckCancel(progress, Format('Thread-%d', [FChildThreadIndex + 1]), FModelName) then
      Terminate;
end;

procedure TTestModelsThread.Execute;
var
  i: integer;
  flag: boolean;
begin
  Randomize;
  ChildThread[0] := self;
  for i := 1 to NoOfThreads-1 do
    ChildThread[i] := TTestModelsChildThread.Create(self, i, SeqData, InitTree);
  try
    for i := 1 to NoOfThreads-1 do
      ChildThread[i].Start;
    TestModels;
    while (not Terminated) and (not CheckChildThreadDone) do
      sleep(Random(1000));
  finally
    for i := 1 to NoOfThreads-1 do
      ChildThread[i].Terminate;
    repeat
      flag := true;
      for i := 1 to NoOfThreads-1 do
        flag := flag and (ChildThread[i].done);
      sleep(Random(1000));
    until flag;
  end;
  if Terminated then
    ReturnValue := 1
  else
    ReturnValue := 0;
  if NoOfThreads > 1 then
    for i := 1 to NoOfThreads - 1 do
      ChildThread[i].Free;
end;

//////////////////

procedure TMLTreeAnalyzer.TestDNAModels(numThreads: Integer);
var
  InitTree: TTreeData = nil;

  procedure MakeInitialTree;
  var
    AModel: TTN93Model;
    D: PDistanceMatrix;
  begin
    AModel := TTN93Model.Create(0, false, 1);
    D  := CreateDistanceMatrix(NoOfSeqs);
    if not AModel.ComputePDistanceMatrix(Seqs, D) then
      CorrectDistanceMatrix(D, NoOfSeqs);
    InitTree := TTreeData.Create(Seqs.Count, true, false, false);
    MakeNJTree(InitTree, D, false);
    FreeMemAndNil(D);
    AModel.Free;
  end;

var
  TestModelsThread: TTestModelsThread;
begin
  MakeInitialTree;
  TestModelsThread := TTestModelsThread.Create(Seqs, InitTree, ModelInfoList, 24, numThreads);
  RuntimeProgress.Thread := TestModelsThread;
  TestModelsThread.Models[ 0] := TGTRModel.Create(0, false, 1);
  TestModelsThread.Models[ 1] := TGTRModel.Create(0, true,  1);
  TestModelsThread.Models[ 2] := TGTRModel.Create(0, false, FNoOfRates);
  TestModelsThread.Models[ 3] := TGTRModel.Create(0, true, FNoOfRates);
  TestModelsThread.Models[ 4] := TTN93Model.Create(0, false, 1);
  TestModelsThread.Models[ 5] := TTN93Model.Create(0, true,  1);
  TestModelsThread.Models[ 6] := TTN93Model.Create(0, false, FNoOfRates);
  TestModelsThread.Models[ 7] := TTN93Model.Create(0, true, FNoOfRates);
  TestModelsThread.Models[ 8] := THKYModel.Create(0, false, 1);
  TestModelsThread.Models[ 9] := THKYModel.Create(0, true,  1);
  TestModelsThread.Models[10] := THKYModel.Create(0, false, FNoOfRates);
  TestModelsThread.Models[11] := THKYModel.Create(0, true, FNoOfRates);
  TestModelsThread.Models[12] := TT3Model.Create(0, false, 1);
  TestModelsThread.Models[13] := TT3Model.Create(0, true,  1);
  TestModelsThread.Models[14] := TT3Model.Create(0, false, FNoOfRates);
  TestModelsThread.Models[15] := TT3Model.Create(0, true, FNoOfRates);
  TestModelsThread.Models[16] := TK2Model.Create(0, false, 1);
  TestModelsThread.Models[17] := TK2Model.Create(0, true,  1);
  TestModelsThread.Models[18] := TK2Model.Create(0, false, FNoOfRates);
  TestModelsThread.Models[19] := TK2Model.Create(0, true, FNoOfRates);
  TestModelsThread.Models[20] := TJCModel.Create(0, false, 1);
  TestModelsThread.Models[21] := TJCModel.Create(0, true,  1);
  TestModelsThread.Models[22] := TJCModel.Create(0, false, FNoOfRates);
  TestModelsThread.Models[23] := TJCModel.Create(0, true, FNoOfRates);

  TestModelsThread.MyCheckCancel := RuntimeProgress.ProgressAndStatusCheckCancel;
  TestModelsThread.ProgressMessage := 'Testing DNA models';

  TestModelsThread.Start;
  TestModelsThread.WaitFor;
  FCanceled := (TestModelsThread.ReturnValue = 1);
  if not FCanceled then
    if assigned(CheckCancel) then CheckCancel(100, 'Finishing test');
  TestModelsThread.Free;

  InitTree.Free;
end;

procedure TMLTreeAnalyzer.TestProteinModels(numThreads: Integer);
var
  TestModelsThread: TTestModelsThread;
  InitTree: TTreeData;

  procedure MakeInitialTree;
  var
    AModel: TJTTModel;
    D: PDistanceMatrix;
  begin
    AModel := TJTTModel.Create(0, false, false, 1);
    D  := CreateDistanceMatrix(NoOfSeqs);
    if not AModel.ComputePDistanceMatrix(Seqs, D) then
      CorrectDistanceMatrix(D, NoOfSeqs);
    InitTree := TTreeData.Create(Seqs.Count, true, false, false);
    MakeNJTree(InitTree, D, false);
    FreeMemAndNil(D);
    AModel.Free;
  end;

begin
  MakeInitialTree;
  TestModelsThread := TTestModelsThread.Create(Seqs, InitTree, ModelInfoList, 56, numThreads);
  RuntimeProgress.Thread := TestModelsThread;
  TestModelsThread.Models[ 0] := TDayhoffModel.Create(0,false,false,1);
  TestModelsThread.Models[ 1] := TDayhoffModel.Create(0, true,false, 1);
  TestModelsThread.Models[ 2] := TDayhoffModel.Create(0, false,false, FNoOfRates);
  TestModelsThread.Models[ 3] := TDayhoffModel.Create(0, true,false, FNoOfRates);
  TestModelsThread.Models[ 4] := TDayhoffModel.Create(0,false,true,1);
  TestModelsThread.Models[ 4].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[ 5] := TDayhoffModel.Create(0, true,true, 1);
  TestModelsThread.Models[ 5].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[ 6] := TDayhoffModel.Create(0, false,true, FNoOfRates);
  TestModelsThread.Models[ 6].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[ 7] := TDayhoffModel.Create(0, true,true, FNoOfRates);
  TestModelsThread.Models[ 7].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[ 8] := TJTTModel.Create(0, false,false, 1);
  TestModelsThread.Models[ 9] := TJTTModel.Create(0, true,false, 1);
  TestModelsThread.Models[10] := TJTTModel.Create(0, false, false, FNoOfRates);
  TestModelsThread.Models[11] := TJTTModel.Create(0, true, false, FNoOfRates);
  TestModelsThread.Models[12] := TJTTModel.Create(0, false, true, 1);
  TestModelsThread.Models[12].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[13] := TJTTModel.Create(0, true, true, 1);
  TestModelsThread.Models[13].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[14] := TJTTModel.Create(0, false, true, FNoOfRates);
  TestModelsThread.Models[14].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[15] := TJTTModel.Create(0, true, true, FNoOfRates);
  TestModelsThread.Models[15].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[16] := TWAGModel.Create(0, false,false, 1);
  TestModelsThread.Models[17] := TWAGModel.Create(0, true,false, 1);
  TestModelsThread.Models[18] := TWAGModel.Create(0, false,false, FNoOfRates);
  TestModelsThread.Models[19] := TWAGModel.Create(0, true,false, FNoOfRates);
  TestModelsThread.Models[20] := TWAGModel.Create(0, false,true, 1);
  TestModelsThread.Models[20].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[21] := TWAGModel.Create(0, true,true, 1);
  TestModelsThread.Models[21].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[22] := TWAGModel.Create(0, false,true, FNoOfRates);
  TestModelsThread.Models[22].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[23] := TWAGModel.Create(0, true,true, FNoOfRates);
  TestModelsThread.Models[23].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[24] := TLGModel.Create(0, false,false, 1);
  TestModelsThread.Models[25] := TLGModel.Create(0, true,false, 1);
  TestModelsThread.Models[26] := TLGModel.Create(0, false,false, FNoOfRates);
  TestModelsThread.Models[27] := TLGModel.Create(0, true,false, FNoOfRates);
  TestModelsThread.Models[28] := TLGModel.Create(0, false,true, 1);
  TestModelsThread.Models[28].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[29] := TLGModel.Create(0, true,true, 1);
  TestModelsThread.Models[29].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[30] := TLGModel.Create(0, false,true, FNoOfRates);
  TestModelsThread.Models[30].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[31] := TLGModel.Create(0, true,true, FNoOfRates);
  TestModelsThread.Models[31].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[32] := TmtREV24Model.Create(0, false, false, 1);
  TestModelsThread.Models[33] := TmtREV24Model.Create(0, true, false, 1);
  TestModelsThread.Models[34] := TmtREV24Model.Create(0, false, false, FNoOfRates);
  TestModelsThread.Models[35] := TmtREV24Model.Create(0, true, false, FNoOfRates);
  TestModelsThread.Models[36] := TmtREV24Model.Create(0, false, true, 1);
  TestModelsThread.Models[36].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[37] := TmtREV24Model.Create(0, true, true, 1);
  TestModelsThread.Models[37].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[38] := TmtREV24Model.Create(0, false, true, FNoOfRates);
  TestModelsThread.Models[38].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[39] := TmtREV24Model.Create(0, true, true, FNoOfRates);
  TestModelsThread.Models[39].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[40] := TcpREVModel.Create(0, false, false, 1);
  TestModelsThread.Models[41] := TcpREVModel.Create(0, true, false, 1);
  TestModelsThread.Models[42] := TcpREVModel.Create(0, false, false, FNoOfRates);
  TestModelsThread.Models[43] := TcpREVModel.Create(0, true, false, FNoOfRates);
  TestModelsThread.Models[44] := TcpREVModel.Create(0, false, true, 1);
  TestModelsThread.Models[44].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[45] := TcpREVModel.Create(0, true, true, 1);
  TestModelsThread.Models[45].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[46] := TcpREVModel.Create(0, false, true, FNoOfRates);
  TestModelsThread.Models[46].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[47] := TcpREVModel.Create(0, true, true, FNoOfRates);
  TestModelsThread.Models[47].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[48] := TrtREVModel.Create(0, false, false, 1);
  TestModelsThread.Models[49] := TrtREVModel.Create(0, true, false, 1);
  TestModelsThread.Models[50] := TrtREVModel.Create(0, false, false, FNoOfRates);
  TestModelsThread.Models[51] := TrtREVModel.Create(0, true, false, FNoOfRates);
  TestModelsThread.Models[52] := TrtREVModel.Create(0, false, true, 1);
  TestModelsThread.Models[52].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[53] := TrtREVModel.Create(0, true, true, 1);
  TestModelsThread.Models[53].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[54] := TrtREVModel.Create(0, false, true, FNoOfRates);
  TestModelsThread.Models[54].SetParamsFromSeqs(Seqs);
  TestModelsThread.Models[55] := TrtREVModel.Create(0, true, true, FNoOfRates);
  TestModelsThread.Models[55].SetParamsFromSeqs(Seqs);

  TestModelsThread.MyCheckCancel     := RuntimeProgress.ProgressAndStatusCheckCancel;
  TestModelsThread.ProgressMessage := 'Testing Protein models';

  TestModelsThread.Start;
  TestModelsThread.WaitFor;
  FCanceled := (TestModelsThread.ReturnValue = 1);
  if not FCanceled then
    if assigned(CheckCancel) then CheckCancel(100, 'Finishing test');
  TestModelsThread.Free;

  InitTree.Free;
end;

procedure TMLTreeAnalyzer.SetTreeData(tree: TTreeData);
begin
  if tree.NoOfOTUs <> NoOfSeqs then
    exit;

  if InitTree <> nil then
    InitTree.Free;

  InitTree := tree;
  InitTree.IsBLen := true;
  if InitTree.SBL < 0.000000000001 then
    ComputeInitialBLens;

  if MLTree <> nil then
    MLTree.InitNodeData(InitTree);
end;

procedure TMLTreeAnalyzer.GetTreeData(tree: TTreeData);
begin
  if MLTree <> nil then
    MLTree.GetTreeData(tree);
end;

procedure TMLTreeAnalyzer.ComputeInitialBLens;
var
  D:  PDistanceMatrix;
  i: integer;
  MPTree: TMPTree;
  b0: array of Extended;
  L0,L1: Extended;
begin
  if InitTree = nil then
    exit;
  if not InitTree.isBLen then
    InitTree.isBLen := true;

  setlength(b0, 2*NoOfSeqs-1);

  D      := nil;
  MPTree := nil;
  try
    MPtree := TMPTree.Create(SeqData, Model.NoOfStates);
    MPTree.ComputeApproxBLens(InitTree);
    CheckAbort;
    if not assigned(MLTree) then
    begin
      if FIsLittleBootstrap then
        MLTree := TMLTree.Create(Seqs, InitTree, FModel, FNeedsRootByOutgroup, FNumTotalSites)
      else
        MLTree := TMLTree.Create(Seqs, InitTree, FModel, FNeedsRootByOutgroup);
      MLTree.NoOfThreads := NoOfThreadsToUse; // GS - let the user decide
      MLTree.deltaL  := FdeltaL;
      MLTree.MaxIter := FMaxIter;
    end
    else
      MLTree.InitNodeData(InitTree);
    CheckAbort;
    if Assigned(SubTaskCheckCancel) then
      MLTree.SubtaskCheckCancel := SubTaskCheckCancel;
    MLTree.Initialize(true);
    L0 := MLTree.LogLikelihood;
    for i := 0 to 2*NoOfSeqs-2 do
      b0[i] := InitTree.BLen[i];
    CheckAbort;
    D  := CreateDistanceMatrix(NoOfSeqs);
    if not Model.ComputeDistanceMatrix(seqs, D, true) then
      CorrectDistanceMatrix(D, NoOfSeqs);
    CheckAbort;
    FastOLSBranchLength(InitTree, D);
    for i := 0 to 2*NoOfSeqs-2 do
      if InitTree.BLen[i] < 0 then
        InitTree.BLen[i] := 0;
    CheckAbort;
    MLTree.InitNodeData(InitTree);
    MLTree.Initialize(true);
    L1 := MLTree.LogLikelihood;
    CheckAbort;
    if L0 > L1 then
    begin
      for i := 0 to 2*NoOfSeqs-2 do
        InitTree.BLen[i] := b0[i];
      MLTree.InitNodeData(InitTree);
      MLTree.Initialize(true);
    end;
    CheckAbort;
  finally
    if Assigned(D) then
      DestroyDistanceMatrix(D, NoOfSeqs);
    if Assigned(MPtree) then
      MPTree.Free;
    setlength(b0, 0);
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

procedure TMLTreeAnalyzer.MakeTree(method: integer);

  function MakeParsimonyTree(treedata: TTreeData) : boolean;
  var
    MPTree: TMPTree;
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
        MPTree.NoOfInitTrees := 1;
        MPTree.SearchLevel   := 5;
        MPTree.MaxNoOfTrees  := 1;
        MPTree.CheckCancel   := CheckCancel;
        MPTree.ProgressMessage := 'Searching initial MP tree';
        MPTree.SetFreqTable(Model.BootTable);
        MPTree.SearchMPTree;
      end
      else
      begin
        MPTree.SearchMethod  := SPR;
        MPTree.NoOfInitTrees := 1;
        MPTree.SearchLevel   := 5;
        MPTree.MaxNoOfTrees  := 1;
        MPTree.CheckCancel   := CheckCancel;
        MPTree.ProgressMessage := 'Searching initial MP tree';
        MPTree.SearchMPTree;
      end;
      InitTree.isBLen := true;
      MPTree.GetTreeData(InitTree, 0);

      result := true;
    finally
      if assigned(MPTree) then
        FreeAndNil(MPTree);
    end;
  end;

var
  D: PDistanceMatrix = nil;
  tmpMLTree: TMLTree = nil;
  tmpModel: TGammaRateVariationModel = nil;
  n: integer;
  t1,t2: TTreeData;
  L1,L2,r: extended;
  bootstrap: boolean;
begin
  if assigned(CheckCancel) then
    CheckCancel(0, 'Computing initial parameter values');

  try
    if InitTree = nil then
      InitTree := TTreeData.Create(NoOfSeqs,true,false,false);

    if method = MPInitTreeMethod then
      if MakeParsimonyTree(nil) then
        exit;

    r := FModel.SeqCoverageRate(seqs);
    n := FModel.EffectiveNoOfSites(seqs);

    tmpModel := GetModelForInitialTree;
    if Assigned(CheckCancel) then
      tmpModel.CheckCancel := CheckCancel;
    tmpModel.SiteCoverage := FModel.SiteCoverage;
    tmpModel.SetParamsFromSeqs(Seqs);
    tmpModel.NoOfThreads := FNoOfThreadsToUse; // GS - because the user will specify how many to spawn

    tmpModel.BootTable := Model.BootTable;
    bootstrap := Model.BootTable <> nil;

    if assigned(CheckCancel) then
      CheckCancel(0, 'Making initial tree');

    D  := CreateDistanceMatrix(NoOfSeqs);

    if r < 0.95 then
    begin
      if not tmpModel.ComputePDistanceMatrix(seqs, D) then
        CorrectDistanceMatrix(D, NoOfSeqs);
    end
    else
    if n < 1000 then
    begin
      if not tmpModel.ComputePDistanceMatrix(seqs, D) then
        CorrectDistanceMatrix(D, NoOfSeqs);
    end
    else
    begin
      if not tmpModel.ComputeDistanceMatrix(seqs, D, false) then
        CorrectDistanceMatrix(D, NoOfSeqs);
    end;

    case method of
      NJInitTreeMethod: MakeNJTree(InitTree, D, bootstrap);
      BioNJInitTreeMethod: MakeBIONJTree(InitTree, D, bootstrap);
      MEInitTreeMethod: MakeMETree(InitTree, D, bootstrap);

      DefaultInitTreeMethod, MPInitTreeMethod:
      begin
    	 if n >= 2000 then
             MakeNJTree(InitTree, D, bootstrap)
           else if n < 500 then
             MakeBIONJTree(InitTree, D, bootstrap)
           else
           begin
             t1 := TTreeData.Create(NoOfSeqs,true,false,false);

             MakeNJTree(t1, D, bootstrap);
             tmpMLTree := TMLTree.Create(seqs, t1, tmpModel, FNeedsRootByOutgroup);
             tmpMLTree.NoOfThreads := NoOfThreadsToUse;  // GS - let the user decide
             tmpMLTree.Initialize(false);
             if Assigned(SubTaskCheckCancel) then
               tmpMLTree.SubtaskCheckCancel := SubTaskCheckCancel;
             tmpMLTree.OptimizeAllBLens(nil);
             L1 := tmpMLTree.LogLikelihood;
             tmpMLTree.GetTreeData(t1);

             t2 := TTreeData.Create(NoOfSeqs,true,false,false);

             MakeBIONJTree(t2, D, bootstrap);
             tmpMLTree.InitNodeData(t2);
             tmpMLTree.Initialize(false);
             tmpMLTree.OptimizeAllBLens(nil);
             L2 := tmpMLTree.LogLikelihood;
             tmpMLTree.GetTreeData(t2);

             if L2 > L1 then
               InitTree.Assign(t2)
             else
               InitTree.Assign(t1);

             FreeAndNil(t1);
             FreeAndNIl(t2);

           end;
         end;
    end;
  finally
    if assigned(tmpModel) then
      tmpModel.Free;
    if assigned(tmpMLTree) then
      tmpMLTree.Free;
    if assigned(D) then
      DestroyDistanceMatrix(D,NoOfSeqs);
  end;
end;

procedure TMLTreeAnalyzer.Assign(Source: TMLTreeAnalyzer);
var
  i: Integer;
begin
  FNeedsRootByOutgroup := Source.NeedsRootByOutgroup;
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
    if FIsLittleBootstrap then
      MLTree := TMLTree.Create(SeqData, InitTree, FModel, FNeedsRootByOutgroup, FNumTotalSites)
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
  BLenFilter := Source.BLenFilter;
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
  FMaxIter := Source.FMaxIter;
  FDeltaL := Source.FDeltaL;
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
  if Assigned(SubtaskCheckCancel) then
  begin
    FCanceled := SubtaskCheckCancel(CHECK_CANCEL_NO_PROG_VAL, EmptyStr);
    {$IFDEF VISUAL_BUILD} { problematic for non visual as there is no main loop to return to}
    if FCanceled then
      Abort;
    {$ENDIF}
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

  if KeepUserTreeBLens then
  begin
    if InitTree.SBL < FP_CUTOFF then
      ComputeInitialBLens;
  end
  else
    ComputeInitialBLens;
  if FCanceled then
    Exit;
  if MLTree = nil then
  begin
    if FIsLittleBootstrap then
      MLTree := TMLTree.Create(SeqData, initTree, FModel, FNeedsRootByOutgroup, FNumTotalSites)
    else
      MLTree := TMLTree.Create(SeqData, InitTree, FModel, FNeedsRootByOutgroup);
    MLTree.NoOfThreads := NoOfThreadsToUse; // GS - let the user decide
    MLTree.deltaL  := FdeltaL;
    MLTree.MaxIter := FMaxIter;
    MLTree.NoOfThreads := NoOfThreadsToUse;
  end;
  MLTree.Initialize(true);
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
  CheckAbort;
  if KeepUserTreeBLens and (MLTree.NoOfSites = 1) then
    MLTree.OptimizeTreeSize
  else
    MLTree.Optimize(CheckCancel, OptimizeMessage);
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
    MLTree.Initialize(false);
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

procedure ChangeRoot(root, newposition: TMLTreeNode);
var
  a, p, d : TMLTreeNode;
  len0, len1, len2: extended;
  fix0, fix1, fix2: boolean;
begin

  if newposition.anc = root then
  begin
    if newposition = root.des2 then
    begin
      p := root.des1;
      root.des1 := root.des2;
      root.des2 := p;
    end;
    root.des2.blen := root.des1.blen+root.des2.blen;
    root.des1.blen := 0;
//    oldposition := newposition;
    Exit;
  end;
  len0 := root.des1.blen +root.des2.blen;
  fix0 := root.des1.fixed and root.des2.fixed;
  d := newposition;
  p := d.anc;
  a := p.anc;
  len2 := d.blen;
  fix2 := d.fixed;
  while p <> root do
  begin
    len1 := p.blen;
    p.blen := len2;
    len2 := len1;
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
  p.fixed := fix0;
  newposition.anc := root;
  newposition.blen := 0;
  newposition.fixed := p.fixed;
  root.des1 := newposition;     // OTU with the longest branch
  root.des2 := p;

  SetDepth(root);
end;

function TMLTreeAnalyzer.PrepareSearchMLTree(makeinitialtree: boolean): boolean;
var
  bmax: extended;
  i,j: integer;
begin
  result := false;
  if makeinitialtree or (InitTree = nil) then
    MakeTree(InitialTreeOption);

  ComputeInitialBLens;

  if CheckCancel(0, 'Optimizing initial tree') then
    exit;

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

    ChangeRoot(root, node[j]);  // Root on the largest branch length
    MLTree.InitNodeData(InitTree);
    MLTree.Initialize(true);
    if assigned(CheckCancel) then
      if CheckCancel(0, 'Optimizing initial tree') then
        exit;
    MLTree.NoOfThreads := NoOfThreadsToUse; // GS - let the user specify    
    result := MLTree.Optimize(CheckCancel, 'Optimizing initial tree');
  end;
end;

procedure ResetSiteConfig(MLTree: TMLTree);

  procedure ResetSiteConfigUp(n: TMLTreeNode);
  begin
    if n.OTU or n.flag then
      exit;

    ResetSiteConfigUp(n.des1);
    ResetSiteConfigUp(n.des2);

    MLTree.ResetSiteConfig1OfNode(n, true);
  end;

  procedure ResetSiteConfigDown(n: TMLTreeNode);
  begin
    MLTree.ResetSiteConfig0OfNode(n);

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

function SearchTree(MLTree : TMLTree;
                    SearchLevel: integer;
                    BLenFilter: extended;
                    deltaL: extended;
                    MaxIter: integer;
                    CheckCancel: TCheckCancelFunc): boolean;
var
  SwapList  : TList;
  q: array of TMLTreeNode;
  Canceled: boolean;
  progress, aprogress: integer;

  procedure ResetLikelihood(node: TMLTreeNode);

    procedure InitL12OfNode(n: TMLTreeNode);
    begin
      if n.OTU or n.flag then
        exit;

      InitL12OfNode(n.des1);
      InitL12OfNode(n.des2);

//      if not n.des1.flag then
//        MLTree.ComputeL1OfNode(n);
//      if not n.des2.flag then
        MLTree.ComputeL1OfNode(n);
    end;

  begin
    InitL12OfNode(node);
  end;

  function InitSubtreeLikelihood(node: TMLTreeNode): extended;

    procedure InitL12OfNode(n: TMLTreeNode);
    begin
      if n.flag then
        exit;

      InitL12OfNode(n.des1);
      InitL12OfNode(n.des2);

      MLTree.ComputeL1OfNode(n);
//      MLTree.ComputeL2OfNode(n);
    end;

    procedure RenewL12OfAnc(n: TMLTreeNode);
    begin
      if n.anc = nil then exit;

//      if n = n.anc.des1 then
//        MLTree.ComputeL12OfNode(n.anc)
//      else
        MLTree.ComputeL1OfNode(n.anc);
    end;

  //var
  //  L0: array of PMatrixOfExtended;
  begin
    InitL12OfNode(node);

    RenewL12OfAnc(node);

    MLTree.ComputeL0OfNode(node.anc);
    result := MLTree.ComputeLogLikelihoodOfNode(node.anc);
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

      MLTree.IterateBlenOfNode(n, L, 0.01);
    end;

  begin
    IterateSubtreeOfNode(node);
  end;

  procedure ClearSwapList;
  begin
    while SwapList.Count > 0 do
    begin
      TSwapData(SwapList[SwapList.Count-1]).Free;
      SwapList.Delete(SwapList.Count-1);
    end;
  end;

  procedure SetFlag(index: integer);
  var
    p: TMLTreeNode;
  begin

    p := MLTree.Node[TSwapData(SwapList[index]).node[0]];
    while (p <> nil) and p.flag do
    begin
      p.flag := false;
      p := p.anc;
    end;
  end;

  procedure AddSwapData(swapindex: integer; dL: extended; n: integer);
  var
    data: TSwapData;
    i : integer;
  begin
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

  function SearchEachNode(node: TMLTreeNode):boolean;
  var
    p: array of TMLTreeNode;
    d: integer;
    vb,sb: extended;

    function InitSubtree(node: TMLTreeNode): extended;

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
        MLTree.ResetProbOfNode(n);

        if n.flag then
          exit;

        InitSubtreeDown(n.des1);
        InitSubtreeDown(n.des2);
      end;

    begin
      InitSubtreeUp(node);
      InitSubtreeDown(node);

      result := InitSubtreeLikelihood(node);
    end;

    procedure SetInitialSubtree;
    begin
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

      d := 1;
    end;

    function IncSubtreeDown(index: integer): boolean;
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

    function IncSubtreeUp(index: integer): boolean;
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

    procedure DecSubtreeDown(index: integer);
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

    function SPR: boolean;
    var
      L0, L, dL: extended;
      n: TMLTreeNode;
      i: integer;
    begin
      Result := true;

      L0 := MLTree.LogLikelihood;

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
        dL := L;
        IterateSubtreeBLens(n, L);
        inc(i);

        if (L0-L) > (L-dL)*(MLTree.MaxIter -i) then
          break;
      until (abs(L-dL) < deltaL) or (i = MaxIter);

      if L > L0+deltaL then
      begin
        AddSwapData(1, L-L0, 2*d+1);
        result := false;
      end;

      RemoveBranch(q[2]);
      AddBranch(q[1], q[2], q[2*d+1]);
      q[3].blen := p[1].blen;
      q[1].blen := p[3].blen;
      q[2*d  ].blen := p[2*d  ].blen;
      q[2*d+1].blen := p[2*d+1].blen;

      if q[0].des1.flag then
        n := q[0].des2
      else
        n := q[0].des1;

      L := InitSubtree(n);
      i := 0;
      repeat
        dL := L;
        IterateSubtreeBLens(n, L);
        inc(i);

        if (L0-L) > (L-dL)*(MLTree.MaxIter -i) then
          break;
      until (abs(L-dL) < deltaL) or (i = MaxIter);

      if L > L0+deltaL then
      begin
        AddSwapData(2, L-L0, 2*d+1);
        result := false;
      end;

      RemoveBranch(q[2]);
      AddBranch(q[1], q[2], q[3]);
      for i := 1 to 2*d+1 do
        q[i].blen := p[i].blen;

      if not result then
        for i := 1 to 2*d+1 do
          if p[i].done then
          begin
            p[i].done := false;
            dec(aprogress);
          end;

      if aprogress > progress then
        progress := aprogress;
      if assigned(CheckCancel) then
        Canceled := CheckCancel(round(progress/(2*MLTree.NoOfSeqs-3)*100), 'Searching ML tree');
      if Canceled then
        CheckCancel(round(progress/(2*MLTree.NoOfSeqs-3)*100), 'Canceling ML Search');

    end;

    function SPRRecursiveDown: boolean;
    var
      flag: boolean;
    begin
      result := true;
      if d > SearchLevel then exit;

//      if (q[2*d].ase  < 0.000000000001) or (q[2*d].blen < 3*q[2*d].ase) then
      if ((vb < 0.000000000001) or (sb < BLenFilter*sqrt(vb))) then
        if IncSubtreeDown(2*d) then
        begin
          flag   := SPR;
          result := result and flag;
          flag   := SPRRecursiveDown;
          result := result and flag;
          DecSubtreeDown(2*(d-1));
        end;
//      if (q[2*d+1].ase  < 0.000000000001) or (q[2*d+1].blen < 3*q[2*d+1].ase) then
      if ((vb < 0.000000000001) or (sb < BLenFilter*sqrt(vb))) then
        if IncSubtreeDown(2*d+1) then
        begin
          flag   := SPR;
          result := result and flag;
          flag   := SPRRecursiveDown;
          result := result and flag;
          DecSubtreeDown(2*(d-1)+1);
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

//      if (q[2*d+1].ase  < 0.000000000001) or (q[2*d+1].blen < 3*q[2*d+1].ase) then
      if ((vb < 0.000000000001) or (sb < BLenFilter*sqrt(vb))) then
        if IncSubtreeDown(2*d+1) then
        begin
          flag   := SPR;
          result := result and flag;
          flag   := SPRRecursiveDown;
          result := result and flag;
          DecSubtreeDown(2*(d-1)+1);
        end;

//      if (q[2*d].ase  < 0.000000000001) or (q[2*d].blen < 3*q[2*d].ase) then
      if ((vb < 0.000000000001) or (sb < BLenFilter*sqrt(vb))) then
        if IncSubtreeUp(2*d) then
        begin
          flag   := SPRRecursiveUp;
          result := result and flag;
        end;
    end;
    
  var
    flag: boolean;
  begin
    result := true;

    if Canceled then exit;
    if node.anc = nil then exit;
    if node.anc.anc = nil then exit;

    setlength(p, SearchLevel*2+4);

    SetInitialSubtree;

    sb := q[3].blen;
    vb := q[3].ase*q[3].ase;

    if (q[3].ase  < 0.000000000001) or (q[3].blen < BLenFilter*q[3].ase) then
      if IncSubtreeDown(3) then
      begin
        flag   := SPR;
        result := result and flag;

        flag   := SPRRecursiveDown;
        result := result and flag;

        DecSubtreeDown(3);
      end;

    sb := q[1].blen;
    vb := q[1].ase*q[1].ase;

    if (q[1].ase  < 0.000000000001) or (q[1].blen < BLenFilter*q[1].ase) then
      if IncSubtreeUp(1) then
      begin
        flag   := SPRRecursiveUp;
        result := result and flag;
      end;

    if result then
    begin
      if not node.done then
        inc(aprogress);
      node.done := true;
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
      n := TSwapData(SwapList[index]).NoOfNodes;
      setlength(p, n+1);
      for i := 0 to n do
        p[i] := MLTree.Node[TSwapData(SwapList[index]).node[i]];

      result := true;
      for i := 1 to n do
        if not p[i].flag then
        begin
          result := false;
          break;
        end;
      if not result then exit;

      case TSwapData(SwapList[index]).swapindex of
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
        p[i].blen := TSwapData(SwapList[index]).blen[i];
        p[i].ase  := TSwapData(SwapList[index]).ase[i];
        MLTree.ResetProbOfNode(p[i]);
        p[i].flag := false;
      end;
    end;

    function SwapBranchOneByOne: integer;
    var
      L0: extended;
      flag0,flag1: array of boolean;
      i,j: integer;
    begin
      result := 0;
      for i := 0 to SwapList.Count-1 do
      begin
        setlength(flag0, TSwapData(SwapList[i]).NoOfNodes);
        setlength(flag1, TSwapData(SwapList[i]).NoOfNodes);

        MLTree.GetTreeData(tmpTree);
        L0 := MLTree.LogLikelihood;
        for j := 0 to length(flag0)-1 do
          flag0[j] := MLTree.Node[TSwapData(SwapList[i]).node[j]].flag;

        TSwapData(SwapList[i]).flag := SwapEachBranch(i);
        if TSwapData(SwapList[i]).flag then
        begin
          for j := 0 to length(flag1)-1 do
            flag1[j] := MLTree.Node[TSwapData(SwapList[i]).node[j]].flag;
          SetFlag(i);
          ResetSiteConfig(MLTree);
          ResetLikelihood(MLTree.Root);

          MLTree.LogLikelihood := MLTree.ComputeLogLikelihood;

          if MLTree.LogLikelihood < L0 then
          begin
            MLTree.InitNodeData(tmpTree);
            MLTree.Initialize(false);
            TSwapData(SwapList[i]).flag := false;
            for j := 0 to length(flag0)-1 do
              MLTree.Node[TSwapData(SwapList[i]).node[j]].flag := flag0[j];
          end
          else
            for j := 0 to length(flag1)-1 do
              MLTree.Node[TSwapData(SwapList[i]).node[j]].flag := flag1[j];

          if TSwapData(SwapList[i]).flag then
            inc(result);
        end;
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

    SwapList.Sort(compare);

    result := 0;
    for i := 0 to SwapList.Count-1 do
    begin
      TSwapData(SwapList[i]).flag := SwapEachBranch(i);

      if TSwapData(SwapList[i]).flag then
        inc(result);
    end;

    for i := 0 to SwapList.Count-1 do
      if TSwapData(SwapList[i]).flag then
        SetFlag(i);

    ResetSiteConfig(MLTree);
    ResetLikelihood(MLTree.Root);

    MLTree.LogLikelihood := MLTree.ComputeLogLikelihood;

    if MLTree.LogLikelihood < L0 then
    begin
      MLTree.InitNodeData(tmpTree);
      MLTree.Initialize(false);

//      for i := 0 to 2*MLTree.NoOfSeqs-2 do
//        MLTree.Node[i].flag := true;
//      for i := 0 to SwapList.Count-1 do
//        TSwapData(SwapList[i]).flag := true;
//      result := SwapBranchOneByOne;

    end;

    ClearSwapList;
    tmpTree.Free;
  end;


  function SearchOnAllNodes: boolean;
  var
    i: integer;
    flag: boolean;
  begin
    result := true;

    with MLTree do
      for i := 0 to 2*NoOfSeqs-2 do
      begin
        if Canceled then
          exit;

        if Node[i] = Root then
          continue;

        if Node[i].anc = Root then
          continue;

        flag := SearchEachNode(Node[i]);

        if Canceled then
          break;

        result := result and flag;
      end;
  end;

var
  tmpTree: TTreeData;
  i,n: integer;
  dL: extended;
  flag: boolean;
  aNoOfSites: Int64;
begin
  tmpTree := nil;
  SwapList := nil;
  aprogress := 0;
  progress := 0;
  Canceled := false;
  aNoOfSites := Length(MLTree.Seqs[0]); { using aNoOfSites here because for the SUE analysis we sub-sample the data but FNoOfSites still needs to match the full data dimension}

  try
    SwapList  := TList.Create;
    tmpTree := TTreeData.Create(MLTree.NoOfSeqs, true, true, false);
    with MLTree do
    begin
      setlength(q, SearchLevel*2+4);
      if Model.SeqDataType = protein then
        for i := 0 to length(q)-1 do
          q[i] := TMLTreeNode.Create(i, false, NoOfSeqs, aNoOfSites, Model.NoOfRates, 20)
      else
        for i := 0 to length(q)-1 do
          q[i] := TMLTreeNode.Create(i, false, NoOfSeqs, aNoOfSites, Model.NoOfRates, 4);


      i := 0;
      repeat
        GetTreeData(tmpTree);
        dL := LogLikelihood;

        ResetL0;
        flag := SearchOnAllNodes;
        
//        n := 0;
//        if not flag then
          n := SwapBranches;

        if LogLikelihood < dL then
        begin
          InitNodeData(tmpTree);
          Initialize(false);
        end;

        if Canceled then
          break;

        inc(i);
      until (n = 0) or (abs(LogLikelihood-dL) < DeltaL) or (i = MaxIter);

    end;
  finally
    for i := 0 to length(q)-1  do
      if q[i] <> nil then
        q[i].Free;
    setlength(q, 0);

    while SwapList.Count > 0 do
    begin
      TSwapData(SwapList.Count-1).Free;
      SwapList.Delete(SwapList.Count-1);
    end;
    if Assigned(SwapList) then
      SwapList.Free;
    if Assigned(tmpTree) then
      tmpTree.Free;
  end;
  result := not Canceled;
end;

function TMLTreeAnalyzer.SearchMLTree(makeinitialtree: boolean; IsBStrapRep: Boolean; AProgress: TRuntimeProgress = nil): boolean;
var
  MLTreeSearchThread: TMLTreeSearchThread;
begin
  MLTreeSearchThread := nil;
  result := false;
  try try
    if makeinitialtree and assigned(InitTree) then
      FreeAndNil(InitTree);
    MLTreeSearchThread := TMLTreeSearchThread.Create(Self);
    MLTreeSearchThread.IsInitialized := False;
    MLTreeSearchThread.ProgressDlg := RuntimeProgress;
    MLTreeSearchThread.IsBootstrapReplicate := IsBStrapRep;

    if Assigned(AProgress) then
    begin
      MLTreeSearchThread.ProgressDlg := AProgress;
      { if tree searches are going quickly, don't have the search
        thread update progress, but just let the bootstrap thread do it,
        otherwise it is really ugly. If searches are slow, let the thread
        update it, otherwise it appears that there is no progress}
      if MillisecondsBetween(EndTime, StartTime) < MAX_INTERVAL_NO_BSPROG_UPDATE then
        MLTreeSearchThread.SupPressProgUpdates := True;
    end;
    MLTreeSearchThread.FreeOnTerminate := False;
    MLTreeSearchThread.Start;
    MLTreeSearchThread.WaitFor;
    result := not MLTreeSearchThread.Canceled;
  except
    on E: EOutOfMemory do
    begin
    {$IFDEF VISUAL_BUILD}
      MessageDlg('''MEGA has run out of addressable memory.  This means that the analysis was not able to be completed.  The analysis may be able to run by adjusting the analysis preferences to consume less memory.''', mtError, [mbOK], 0);
    {$ELSE}
      error_NV('MEGA has run out of addressable memory.  This means that the analysis was not able to be completed.  The analysis may be able to run by adjusting the analysis preferences to consume less memory.');
    {$ENDIF}
    end;
    on E: Exception do
      raise Exception.Create(E.Message);
  end;
  finally
    if MLTreeSearchThread <> nil then
      MLTreeSearchThread.Free;
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
                                    BLenFilter,
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

function TMLTreeAnalyzer.GetAllAncSeqs: TAncestralSeqArray;
var
  ResultList : array of TAncStateRec;
  SortedResults : TList;
  i, j, state: integer;
  seqdata: String;
begin
  try
    //probs.clear;
    SetLength(Result, NoOfSeqs);
    for i := 0 to Length(Result) - 1 do
      Result[i] := TAncestralSeq.Create;
    if Model.SeqDataType = DNA then
      SetLength(ResultList, 4)
    else
      SetLength(ResultList, 20);
    SortedResults := TList.Create;
    for i := NoOfSeqs to Length(MLTree.Node) - 2 do
    begin
      SetLength(seqdata, NoOfSites);
      Result[i - NoOfSeqs].NodeIndex := i;
      for j := 1 to NoOfSites do
      begin
        GetAncStateProbNotSorted(i, j, ResultList);
        for state := 0 to Length(ResultList)-1 do
          SortedResults.Add(@resultList[state]);
        SortedResults.Sort(SortByProb);
        //for k := 0 to SortedResults.Count-1 do
          seqdata[j] := TAncStateRec(SortedResults[0]^).Name[1];
      end;
      Result[i - NoOfSeqs].SeqData := seqdata;
    end;
  finally
    FreeAndNil(SortedResults);
  end;
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
  ResultList : array of TAncStateRec;
  SortedResults: TList = nil;
  i : integer;
  aRec: TAncStateRec;
  p: Pointer;
begin
  try
    if Model.SeqDataType = DNA then
      SetLength(ResultList, 4)
    else
      SetLength(ResultList, 20);
    SortedResults := TList.Create;
    Result := GetAncStateProbNotSorted(NodeIndex, SiteIndex, ResultList);
    for i := 0 to Length(ResultList) - 1 do
      SortedResults.Add(@ResultList[i]);
    SortedResults.Sort(SortByProb);
    for i := 0 to SortedResults.Count - 1 do
    begin
      p := SortedResults[i];
      aRec := TAncStateRec(p^);
      Probs[i].Name := aRec.Name;
      Probs[i].Prob := aRec.Prob;
    end;
  finally
    if Assigned(SortedResults) then
      SortedResults.Free;
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
begin
  if Model.SeqDataType = DNA then
    SetLength(Result, 4)
  else
    SetLength(Result, 20);
  MLTree.GetExpectedStateProb(NodeIndex, SiteIndex, Result);
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
    Result[i] := temp[i];
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
  if Result then
  begin
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

      GetAncStateProbAlphabetical(0, 1, AncStateProbabilities);
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
      Application.ProcessMessages;

      //PleaseWait.PercentDone := Math.Ceil(100* ((site * 1.0) / (Tree.MaxSiteIndex+1)));

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
          GetExpectedStateProbAlphabetical(SelNodeIndex-1, site, ExpectedStateProb);
        for i :=0 to Length(AncStateProbabilities)-1 do
        begin
          Probability := StrToFloat(ExpectedStateProb.Values[ExpectedStateProb.Names[i]]);
          if Probability = 0 then //Don't show 0's
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

/// <summary>Export all ancestral states to a comma separated value text file.</summary>
/// <remarks>Loops through each site in the alignment and for each possible base, gives
/// the calculated ancestral probability for each node.</remarks>
/// <note>refactored on 10-6-2011 so that the output format is usable in a spreadsheet.
/// The previous format was not compatible with spreadsheet functions such as pivot tables
/// and summary analysis</note>
procedure TMLTreeAnalyzer.ExportAllAncStates(NamesList: TStringList; isRooted: Boolean);
var
  Base: integer;
  Node, Site:integer;
  DataStringGrid : TStringList = nil;
  ExportType : TExportType;
  AncStateProbabilities : Array of TAncStateRec;
  NodeMap: TStringList = nil;
  AncStateFile: TextFile;
  Delim: Char;
begin
  try
    DataStringGrid := TStringList.Create;
    AssignFile(AncStateFile, NextAvailableFilenameNV('.csv'));
    Rewrite(AncStateFile);
    if FModel.SeqDataType <> DNA then
      SetLength(AncStateProbabilities, 20)
    else
      SetLength(AncStateProbabilities, 4);

    ExportType := OutputFileTypeToExportType(D_MegaMain.OutputFormat);
    if ExportType = EXcsvSave then
      Delim := ','
    else
      Delim := ' ';
    GetAncStateProbAlphabetical(1, 1, AncStateProbabilities);

    Write(AncStateFile,'site',Delim,'base');
    for Node :=0 to Length(MLTree.Node)- 2 do
    begin
      if isRooted or (MLTree.Node[Node] <> MLTree.Root) then
        write(AncStateFile,Delim,'Node_',IntToStr(Node + 1));
    end;
    WriteLn(AncStateFile);

    for Site := 0 to MLTree.NoOfSites-1 do
      begin
        for base := 0 to Length(AncStateProbabilities) - 1 do
        begin
          Write(AncStateFile,IntToStr(Site + 1),Delim, AncStateProbabilities[base].Name);
          for Node := 0 to Length(MLTree.Node) - 2 do
          begin
            if isRooted or (MLTree.Node[Node] <> MLTree.Root) then
            begin
              GetAncStateProbAlphabetical(Node, (Site+1), AncStateProbabilities);
              Write(AncStateFile,Delim,Format('%.3e', [AncStateProbabilities[Base].Prob]));
            end;
          end;
          WriteLn(AncStateFile,'');
        end;
   end;
   NodeMap := GenerateNodeMap(NamesList);
   NodeMap.SaveToFile(NextAvailableFilenameNV('_nodeMap.txt'));
  finally
    CloseFile(AncStateFile);
    if Assigned(DataStringGrid) then
      DataStringGrid.Free;
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
  version: integer;
  n: integer;
  i: integer;
  AExt: Extended;
  b: Boolean;
begin
  version := 3;
  BlockWrite(data, version, sizeof(integer));
  BlockWrite(data, FNeedsRootByOutgroup, SizeOf(FNeedsRootByOutgroup));
  BlockWrite(data, FNoOfRates, sizeof(integer));
  BlockWrite(data, FSearchLevel, sizeof(integer));
  BlockWrite(data, FMaxIter, sizeof(integer));
  BlockWrite(data, FIsGlobalClock, sizeof(boolean));
  BlockWrite(data, FGlobalClockLevel, sizeof(integer));
  BlockWrite(data, FNoOfClockOTUs, sizeof(integer));
  BlockWrite(data, FDeltaL, SizeOf(extended));

  // values for timetrees, need to do it this way or else every thing needs to be recalculated
  n := MLTree.RelTimeComputer.NumNodes;
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
  version: integer = -1;
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
  AExt: Extended = -1;
  b: Boolean =  False;
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

  BlockRead(data, version, sizeof(integer));
  if SessionVersion > 1004 then
    BlockRead(data, FNeedsRootByOutgroup, SizeOf(FNeedsRootByOutgroup));
  BlockRead(data, FNoOfRates, sizeof(integer));
  BlockRead(data, FSearchLevel, sizeof(integer));

  BlockRead(data, FMaxIter, sizeof(integer));

  BlockRead(data, FIsGlobalClock, sizeof(boolean));
  BlockRead(data, FGlobalClockLevel, sizeof(integer));
  BlockRead(data, FNoOfClockOTUs, sizeof(integer));

  BlockRead(data, FDeltaL, SizeOf(extended));

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
        if version >= 3 then
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

  FModel.SetParamsFromSeqs(Seqs);
// MLTree
  tree := TTreeData.Create(NoOfSeqs, true, true, false);
  tree.ReadFromFile(data, SessionVersion);
  if Length(OutgroupMembers) > 0 then
    for i := Low(OutgroupMembers) to High(OutgroupMembers) do
      tree.IsOutgroupMember[i] := OutgroupMembers[i];
  MLTree := TMLTree.Create(Seqs, tree, FModel, FNeedsRootByOutgroup);
  MLTree.Initialize(false);
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
      if version >= 3 then
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
  if version >= 2 then
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
  result := AnsiCompareText(List.Names[Index1], List.Names[Index2]);
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
  NewLine := #13#10;
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
  MyStringList.Add('TMLTreeAnalyzer.FMaxIter=' + IntToStr(FMaxIter));
  MyStringList.Add('TMLTreeAnalyzer.FGlobalClockLevel=' + IntToStr(FGlobalClockLevel));
  MyStringList.Add('TMLTreeAnalyzer.BLenFilter=' + FloatToStrF(BLenFilter, ffFixed, 5, 5));
  MyStringList.Add('TMLTreeAnalyzer.FDeltaL=' + FloatToStrF(FDeltaL, ffFixed, 5, 5));
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
	  TempStringList := InitTree.StateToStringList(Comment);
	  for i := 0 to TempStringList.Count - 1 do
		  MyStringList.Add(TempStringList[i]);
  end
  else
    MyStringList.Add('TMLTreeAnalyzer.InitTree=nil');

  if NonClockTree <> nil then
  begin
	  MyStringList.Add(NewLine);
    MyStringList.Add('TMLTreeAnalyzer.NonClockTree=MyName');
	  TempStringList := NonClockTree.StateToStringList('');
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
