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

unit MLTree;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$IFDEF DEBUG}
  {$ASSERTIONS ON}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Classes, Math, SyncObjs,
  MPTree, MTreeData, MegaConsts, MRelTimeComputer, mcancellable;

const
  FP_CUTOFF = 0.000000000001; { Floating Point Cutoff - if a value is less than this, we are worried about overflow and div by zero errors}

type
  TSeqDataType = (DNA, Protein);
  TMLTreeNode = class;
  TMLTree = class;

  { TModelInfo }

  TModelInfo = class
    public
      DevConsoleProc: TSendMessageProc;
      ModelName: AnsiString;
      FullName: AnsiString;
      DataType: AnsiString;
      NoOfParams: integer;
      NoOfSamples: QWord;
      LogL:  double;
      AICc:  double;
      BIC:   double;
      Gamma: double;
      Invar: double;
      NumSites: Integer;
      NumCommonSites: Integer;
      NumInvarSites: Integer;
      NumGapSites: Integer;
      SiteCoverage: Integer;
      UseInvar: Boolean;
      SVR  : double;
      NoOfRates:  integer;
      Rate:  array[0..19] of double;
      Freq:  array[0..19] of double;
      Matrix: array[0..19,0..19] of double;
      constructor Create;
      destructor Destroy; override;
      function Letter(index: integer): AnsiChar;
      function ToStringList(numClockParams: Integer = 0): TStringList;
      function SaveToFile(filename: String): Boolean;
      function ToTabularString: String;
      function CatRatesCaption: String;
      function FrequenciesCaption: String;
      function BaseModelName: String;
      function FullBaseModelName: String;
      function IsPlusFModel: Boolean;
      function IsSameBaseModel(aModelInfo: TModelInfo): Boolean;
      class function TabularHeaderString: String; static;
      procedure Assign(Source: TModelInfo);
      procedure MyWriteToDevConsole(aMsg: String);
      procedure WriteToFile(var SessionFile: File);
      function ReadFromFile(var SessionFile: File; const SessionVersion: Integer): Boolean;
  end;

  TAncStateRec = record
    Name : AnsiString;
    Prob : double;
  end;

  TAncStateRecArray = array of TAncStateRec; { each element is a probability for an allele}
  TAncStateRecArrayArray = array of TAncStateRecArray; { each element is the probabilities for possible alleles at a site}
  TAncStateRecArrayArrayArray = array of TAncStateRecArrayArray; { used for multiple nodes or for the first sequence across multiple trees in the case of EP calculation}

  { TSubstitutionModel }

  TArrayOfLongint3D = array [0..MaxPointerElts-1] of PArrayOfLongint2D;
  PArrayOfLongint3D = ^TArrayOfLongint3D;

  TSubstitutionModel = class abstract (TCancellable)
    deltaL: extended;
    MaxIter: integer;
    f0: array of integer;
  private
    FCancelled: Boolean;
    FOrigNoOfSites: QWord;
    procedure SetBootTable(AValue: PArrayOfInt);
  protected
    SC: array of integer;
    ch: array of integer;

    FNoOfRates: integer;
    FNoOfStates: integer;
    FRate: array of extended;
    FFreq: array of extended;
    FSeqDataType: TSeqDataType;

    FUseInvar: boolean;
    FInvar: extended;
    FNoOfInvarSites: integer;
    FSiteCoverage: integer;
    FNoOfGapSites: integer;

    FBootTable: PArrayOfInt;
    FNoOfSites: QWord;
    FTotalNoOfSites: QWord;
    FNoOfCommonSites: QWord;

    FInitialized: boolean;

    function GetFreq(index: integer): extended;
    function GetRate(index: integer): extended;

    procedure SortSeqs(seqs: TStringList; var h: array of integer);
    procedure GetParamsFromSeqs(seqs: TStringList; diff: PArrayOfLongint3D; var s,s2: Extended; var n,n0: integer); { DO NOT change s and s2 to double, otherwise compiling on Linux fails. You cannot mix different types when using var. It only works on Windows because double end extended are aliases on WIndows}
    procedure IterateInvarParameter(tree: TMLTree);
    procedure SetSiteCover(seqs: TStringList);
    procedure CheckAbort;
  public
    UpdateProgressFunc: TCheckCancelFunc;
    property NoOfRates: integer read FNoOfRates;
    property NoOfStates: integer read FNoOfStates;
    property Rate[index: integer]:extended read GetRate;

    property SeqDataType: TSeqDataType read FSeqDataType;
    property Freq[index: integer]: extended read GetFreq;

    property UseInvar: boolean read FUseInvar;
    property Invar: extended read FInvar write FInvar;
    property NoOfInvarSites: integer read FNoOfInvarSites write FNoOfInvarSites;
    property NoOfGapSites: integer read FNoOfGapSites;

    property BootTable: PArrayOfInt read FBootTable write SetBootTable;
    property NoOfSites: QWord read FNoOfSites;
    property OrigNoOfSites: QWord read FOrigNoOfSites write FOrigNoOfSites; { used for ModelTamer and Little Bootstraps where we are compressing the data}
    property TotalNoOfSites: QWord read FTotalNoOfSites;
    property NoOfCommonSites: QWord read FNoOfCommonSites write FNoOfCommonSites;

    property SiteCoverage: integer read FSiteCoverage write FSiteCoverage;

    property Initialized: boolean read FInitialized;

    procedure SetCancelled(AValue: Boolean); override;
    procedure SetParamsFromSeqs(seqs: TStringList); virtual;
    procedure OptimizeParameters(tree: TMLTree; aCheckCancel: TCheckCancelFunc = nil); virtual; abstract;

    procedure ComputeProb(Prob: PMatrixOfExtended; b: extended); virtual; abstract;
    procedure ComputeProbOfNode(node: TMLTreeNode; b: extended);

    function OptimumSiteCoverage(seqs: TStringList): integer;
    function EffectiveNoOfSites(seqs: TStringList): integer;
    function SeqCoverageRate(seqs: TStringList): extended;
    function ComputePDistanceMatrix(seqs: TStringList; D: PDistanceMatrix): boolean;

//    function ComputeDistance(M: PMatrixOfExtended): extended; virtual; abstract;
//    function ComputeDistanceMatrix(seqs: TStringList; D: PDistanceMatrix): boolean; virtual; abstract;

//    function PatternBias: Extended; virtual; abstract;

    constructor Create(inv: boolean; ncat: integer);
    destructor Destroy; override;

    property Cancelled: Boolean read FCancelled write SetCancelled;
  end;

  TGammaRateVariationModel = class abstract (TSubstitutionModel)
  private
    FNoOfThreads: integer;
    procedure GenerateDiscreteGamma;
    procedure FitGammaToRates;
    procedure SetGamma(value: extended);
    procedure SortSite(var f: array of integer; n: integer);
  protected
    FFixedGamma: boolean;
    IniGamma: extended;
    FGamma: extended;
    gamma0: extended;

    function CompositeLikelihood(M: PMatrixOfExtended; b: extended; usegamma: boolean): extended; virtual;

    procedure InitGamma(s,s2: double; n,n0: integer);
    procedure InitGammaParameter(seqs: TStringList; tree: TTreeData);
    procedure InitGammaParamFromSeqs(seqs: TStringList);

    procedure IterateGammaParameter(tree: TMLTree);

    procedure ComputeProbWithGamma(P: PMatrixOfExtended; b: extended); virtual; abstract;
  public
    property NoOfThreads: integer read FNoOfThreads write FNoOfThreads;
    property Gamma: extended read FGamma write SetGamma;
    property FixedGamma: boolean read FFixedGamma write FFixedGamma;

    procedure Assign(source: TGammaRateVariationModel); virtual;

    procedure GetInfo(ModelInfo: TModelInfo); virtual;

    procedure SetParamsFromSeqs(seqs: TStringList); virtual;
    procedure InitGammaParamByParsimony(seqs: TStringList; tree: TTreeData);
    procedure OptimizeParameters(tree: TMLTree; aCheckCancel: TCheckCancelFunc = nil); override;

    function ComputeDistance(M: PMatrixOfExtended; usegamma: boolean): extended; virtual;
    function ComputeDistanceMatrix(seqs: TStringList; D: PDistanceMatrix; usegamma: boolean): boolean; virtual;
    function ComputeDistMatrixCustom(seqs: TStringList; D: PDistanceMatrix; usegamma: boolean; n0,n1: integer): boolean;
    function GetName: String; virtual;
    function GetDescription: String; virtual;
    function ReadFromFile(var data: File):boolean; virtual;
    procedure WriteToFile(var data: File); virtual;
    constructor Create(aGamma: extended; inv: boolean; ncat: integer; numThreads: integer); Overload;
    constructor Create(aGamma: extended; inv: boolean; ncat: integer); Overload;
  end;

  TGammaRateVariationModelArray = array of TGammaRateVariationModel;

  TArrayOfByte = array [0..(High(Longint) div sizeOf(Byte))-1] of Byte;
  PArrayOfByte = ^TArrayOfByte;

  { TMLTreeNode }

  TMLTreeNode = class
    {$IFDEF DARWIN}NodeCriticalSection: TCriticalSection; static;{$ENDIF}
    index : integer;
    anc   : TMLTreeNode;
    des1  : TMLTreeNode;
    des2  : TMLTreeNode;
    blen  : extended;
    vb    : extended;
    height: extended;
    depth : integer;
    size  : integer;

    flag  : boolean;
    done  : boolean;
    anchored: boolean;
    fixed : boolean;

    CI: double; { Consistency Index }

    DataCoverage: Double; { the proportion of sites for which at least one taxon from each descendent lineage has informative data}
    NoOfSeqs: integer;
    NoOfSites: integer;
    OrigNoOfSites: Integer;
    NoOfStates: integer;
    nc0 : integer;
    nc1 : integer;

    dL     : extended;

    state : integer;

    Prob: array of PMatrixOfExtended;
    L0:   array of PMatrixOfExtended;
    L1:   array of PMatrixOfExtended;
    c0: PArrayOfInt;
    c1: PArrayOfInt;
    g0: PArrayOfByte;
    g1: PArrayOfByte;
    g2: PArrayOfByte;

    cs0: PArrayOfInt;
    cs1: PArrayOfInt;

    IsOutgroupMember: boolean;

    tmpL0: array of PArrayOfExtended;
    tmpL1: array of PArrayOfExtended;

    function GetSisterNode: TMLTreeNode;
    function GetASE: extended;
    procedure SetASE(value: extended);
  private
    procedure AllocNodeMem0;
    procedure DeallocNodeMem0;
    procedure AllocNodeMem1;
    procedure DeallocNodeMem1;
    procedure InitOTUMem;
  public
    function GetApproximateSizeInBytes: Int64;
    property sister: TMLTreeNode read GetSisterNode;
    property ase: extended read GetASE write SetASE;    // For compatibility to old version

    function OTU: boolean;
    procedure Assign(source: TMLTreeNode);
    function Assigned: boolean;
    function DebugString: String;

    constructor Create(AIndex: integer; aOtu: boolean; nseqs, nsites, nrates, nstates, norigsites: integer);
    destructor Destroy; override;
    {$IFDEF DARWIN}
    class constructor Initialize;
    class destructor Finalize;
    {$ENDIF}
  end;
  TMLTreeNodeArray = array of TMLTreeNode;

  TComputeL0L1Thread = class;

  TMLTree = class(TCancellable)
  private
    FDevInitialLogLikelihood: Extended;
    FIsDoingTreeSearch: Boolean;
    FNeedsRootedOnOutgroup: Boolean;
    FOrigNumSites: Integer;
    FSeqs: TStringList;
    FModel: TGammaRateVariationModel;
    FReltimeComputer: TReltimeComputer;

    FNoOfSeqs: integer;
    FNoOfSites: integer;
    FLogLikelihood: extended;
    FNoOfThreads: integer;
    FOptimizePrecision: integer;
    FMaxIter: integer;
    FdeltaL: extended;

    FTempAncestralStateProbs: array[0..19] of extended;

    FInitialized: boolean;
    FCancelled: Boolean;
    FSubTaskProgress: Integer;
    FSubTaskstatus: AnsiString;
    FHideSubtaskProgress: Boolean;

    nc: array of integer;
    bc: array of integer;

    ComputeL0L1Thread: TComputeL0L1Thread;

    OriginalRoot: TPoint;

    function GetPropagateConstraints: Boolean;
    procedure SetLogLikelihood(AValue: extended);
    procedure SetModel(newmodel: TGammaRateVariationModel);
    procedure ResetNodes;

    procedure InitOTUConfig;
    procedure ResetSiteConfig;
    procedure InitSiteConfig;

    function GetNoOfConfs: integer;

//    function IterateBlenOfNode(ANode: TMLTreeNode; AModel: TGammaRateVariationModel; L0: extended; delta: extended): extended;
//    function IterateBlenOfNodeDown(ANode: TMLTreeNode; AModel: TGammaRateVariationModel; L0: extended; delta: extended): extended;

    function IterateAllHeights(reverseorder: boolean; d: extended): integer;
    function GetNoOfThreads: integer;

    function FinalizeOptimizeAllBLens(Cancel: TCheckCancelFunc; Progress: integer; ProgressMsg: AnsiString): boolean;

    procedure SetNeedsRootedOnOutgroup(AValue: Boolean);
    procedure SetPropagateConstraints(AValue: Boolean);
    procedure SetSizeDepth;
    procedure SubtaskProgress(aProgress: Integer; aStatus: AnsiString);
    procedure CheckAbort;


    procedure ResetL0OfNode(ANode: TMLTreeNode);
    procedure ResetL0L1OfSite(SiteIndex: integer; IsEP: boolean);
    {$IFNDEF VISUAL_BUILD}
    procedure CheckDumpDeveloperData;
    {$ENDIF}
    procedure DoConstruction(origSeqLength: Integer; seqs: TStringList; tree: TTreeData; model: TGammaRateVariationModel; needsOutgroupRoot: Boolean = False);
    function HasOutgroupInClade(n: TMLTreeNode): Boolean;
  public
    OtuNamesList: TStringList;
    SubtaskCheckCancel: TCheckCancelFunc;
    Root:  TMLTreeNode;
    Node:  TMLTreeNodeArray;
    MinProgress: integer;
    MaxProgress: integer;
    procedure SetCancelled(AValue: Boolean); override;
    function GetApproximateSizeInBytes: Int64;
    function FlagOutgroupNodes: Integer; { for all nodes in the outgroup cluster, set flag := True. Used for exporting timetrees where div times in outgroup cluster are not to be used}
    function DebugHeaderString: String;
    function DebugStrings: TStringList;
    function ExportDebugStrings(AName: String): Boolean;
    property Seqs: TStringList read FSeqs;
    property Model: TGammaRateVariationModel read FModel write SetModel;
    property NoOfSites: integer read FNoOfSites;
    property OrigNumSites: Integer read FOrigNumSites write FOrigNumSites; { used to accomodate ModelTamer and LittleBootstraps which compress the sequence data. For all other ML analyses, FNoOfSites and FOrigNumSites are equal}
    property NoOfConfs: integer read GetNoOfConfs;
    property NoOfSeqs: integer read FNoOfSeqs;
    property LogLikelihood: extended read FLogLikelihood write SetLogLikelihood;
    property DevInitialLogLikelihood: Extended read FDevInitialLogLikelihood; { only used for development of baby bootstrap - later we can remove this}
    property OptimizePrecision: integer read FOptimizePrecision write FOptimizePrecision;
    property deltaL: extended read FdeltaL;
    property MaxIter: integer read FMaxIter write FMaxIter;
    property NoOfThreads: integer read GetNoOfThreads write FNoOfThreads;
    property Initialized: boolean read FInitialized;

    procedure ResetSiteConfig0OfNode(ANode: TMLTreeNode); deprecated; { use ResetSiteConfig0OfNodeFast instead}
    procedure ResetSiteConfig0OfNodeFast(ANode: TMLTreeNode);
    procedure ResetGOfNode(aNode: TMLTreeNode);
    procedure ResetSiteConfig1OfNode(ANode: TMLTreeNode; AllocL1: boolean); deprecated; { use ResetSiteConfig1OfNodeFast instead}
    procedure ResetSiteConfig1OfNodeFast(ANode: TMLTreeNode; AllocL1: boolean);
    procedure ResetProbOfNode(ANode: TMLTreeNode);
    procedure ResetProb;

    procedure ResetL0;
    function  ResetL1: extended;

    function ComputeL0OfNodeRange(ANode: TMLTreeNode; LBound, UBound: integer): boolean;
    function ComputeL0OfNode(ANode: TMLTreeNode): boolean;
    function ComputeL1OfNodeRange(ANode: TMLTreeNode; LBound, UBound: integer): boolean;
    function ComputeL1OfNode(ANode: TMLTreeNode): boolean;

    function ComputeLogLikelihoodOfNodeWithModel(ANode: TMLTreeNode; AModel: TGammaRateVariationModel): extended;
    function ComputeLogLikelihoodOfNode(ANode: TMLTreeNode): extended;
    function ComputeLogLikelihood: extended;

    procedure Initialize(InitModel: boolean; InitConfig: boolean);
    procedure InitRateParams;

    procedure ChangeRoot(newroot: TMLTreeNode);
    procedure ChangeRootWithBalancedBlens(newroot: TMLTreeNode);
    procedure ChangeRootByOutgroup;
    procedure ChangeRootOnMidPoint;

    function IterateBlenOfNodeWithModel(ANode: TMLTreeNode; AModel: TGammaRateVariationModel; L0: extended; delta: extended): extended;
    function IterateBlenOfNode(ANode: TMLTreeNode; L0: extended; delta: extended): extended;
    procedure IterateAllBLens;
    procedure IterateHeightOfNode(ANode: TMLTreeNode; var L: extended; delta: extended);
    function IterateTreeSize: extended;

//    procedure OptimizeBlenOfNode(node: TMLTreeNode);
    procedure OptimizeAllBLens(Cancel: TCheckCancelFunc; Progress: Integer=0; ProgressMsg: Ansistring='');
    procedure OptimizeParameters;
    procedure OptimizeTreeSize;

    function Optimize(Cancel: TCheckCancelFunc; ProgressMsg: Ansistring; KeepRootPosition: boolean): boolean;

//    procedure Linearize(Cancel: TCheckCancelFunc);

    property ReltimeComputer: TReltimeComputer read FReltimeComputer;

    function MakeClockTree(tree: TTreeData;
                           IsGlobalClock: boolean;
                           ClockLevel: extended;
                           Cancel: TCheckCancelFunc;
                           MaxRateRatio: Extended;
                           var SamplingTime: array of extended): integer;
                                                           // return the number of local clocks.

    function AnchorClockTree(var minTime, maxTime: array of extended; MaxRateRatio: Extended): boolean; overload;
    function AnchorClockTree(tree: TTreeData; var minTime, maxTime: array of extended; MaxRateRatio: Extended): boolean;  overload;
               // give divergence times of node[0]..node[2*NoOfSeqs-2].
               // do nothing and return FALSE if impossible divergence time (max < min, etc) is given.
               // minTime, maxTime are recomputed to keep consistency among all nodes.
               // Check minTime[i] < maxTime[i] to identify the wrong divergence time if returned with FALSE.

    function NodeHeight(index: integer): extended;

    procedure MakeNonClockTree(tree: TTreeData);

    procedure SetBLens(tree: TTreeData);
    procedure InitNodeData(tree: TTreeData);
    function GetTreeData(tree: TTreeData): boolean;
    procedure SetDataCoverage(tree: TTreeData);

//    procedure MakeInitialTree(tree: TTreeData);
//    function  OptimizeDistance(var L1, L2: array of PMatrixOfExtended; d: extended): extended;

    function GetProbOfRateCategoryAtSite(index: integer; var p: array of extended): boolean;
    function GetMLRateCategoryAtSite(index: integer): integer;
    function GetMLRateAtSite(index: integer): extended;

    function GetAncStateProb(NodeIndex, SiteIndex: integer; var state: array of TAncStateRec):boolean;
    function GetExpectedStateProb(NodeIndex, SiteIndex: integer; var state: array of TAncStateRec):boolean;
    function DescString: AnsiString;
    function StateToStringList(Comment: AnsiString=''): TStringList;
    {$IFDEF DEBUG}{$IFNDEF VISUAL_BUILD}
    function DumpSiteConfigData(filename: String): Boolean;
    {$ENDIF}{$ENDIF}
    function StateToFile(filename: String; Comment: AnsiString = ''): Boolean;
    constructor Create(seqs: TStringList; tree: TTreeData; model: TGammaRateVariationModel; needsOutgroupRoot: Boolean = False);
    constructor CreateWithCompressedData(origSeqLength: Integer; seqs: TStringlist; tree: TTreeData; model: TGammaRateVariationModel; needsOutgroupRoot: Boolean = False);
    destructor Destroy; override;
    procedure Assign(Source: TMLTree);
    property Cancelled: Boolean read FCancelled write SetCancelled;
    property HideSubtaskProgress: Boolean read FHideSubtaskProgress write FHideSubtaskProgress; { for improved responsiveness when user cancels, for instance with parallelized model selection}
    property PropagateConstraints: Boolean read GetPropagateConstraints write SetPropagateConstraints;
    property NeedsRootedOnOutgroup: Boolean read FNeedsRootedOnOutgroup write SetNeedsRootedOnOutgroup;
    property IsDoingTreeSearch: Boolean read FIsDoingTreeSearch write FIsDoingTreeSearch;
  end;

  { TComputeL0L1ChildThread }

  TComputeL0L1ChildThread = class(TThread)
  private
    FErrMsg: String;
    FIsCancelled: Boolean;
    FIsSuccess: Boolean;
    ThreadIndex: integer;
    MainThread: TComputeL0L1Thread;
    L0Mode: boolean;

    MLTree: TMLTree;
    Node: TMLTreeNode;
    Done: boolean;
    Flag: boolean;

    LBound: integer;
    UBound: integer;

    procedure ComputeL0;
    procedure ComputeL1;

  protected
    procedure Execute; override;
  public
    constructor Create(parent: TComputeL0L1Thread; index: integer; tree: TMLTree);

    destructor Destroy; override;
    property IsCancelled: Boolean read FIsCancelled;
    property IsSuccess: Boolean read FIsSuccess;
    property ErrMsg: String read FErrMsg;
  end;

  TComputeL0L1Thread = class(TComputeL0L1ChildThread)
  private
    FNoOfThreads: integer;
    ChildThread: array of TComputeL0L1ChildThread;

    function CheckChildThreadDone: boolean;
  protected
    procedure Execute; override;
  public
    property NoOfThreads: integer read FNoOfThreads write FNoOfThreads;

    constructor Create(tree: TMLTree);
    destructor Destroy; override;
  end;

  procedure ModelInfoToStringList(Model: TModelInfo; ModelOut: TStringList);
  procedure ModelInfoToFile(Model: TModelInfo; FileName: String); Overload;
  procedure ModelInfoToFile(Model: TModelInfo; FileName: String; NumTreesExplored: integer); Overload;

  function ApproximateSizeOfTreeNodeInBytes(otu: boolean; nseqs, nsites, nrates, nstates: integer): Int64;
  function AncStatesStr(a: TAncStateRecArray): String;
  
  procedure CreateArrayOfLongint3D(var X: PArrayOfLongint3D; n, nstates: integer);
  procedure DestroyArrayOfLongint3D(var X: PArrayOfLongint3D; n, nstates: integer);

  {$IFDEF DEBUG}
 procedure SaveMLlog(msg: string);
  {$ENDIF}
  procedure InitKTtime;

  var
    DeveloperSeqNamesList: TStringList = nil;
  {$IFDEF DEBUG}
  MLLogCriticalSection: TCriticalSection;
  {$ENDIF}


implementation
uses
  {$IFDEF DEBUG}mdeveloper_console, Forms,{$ENDIF}
  {$IFNDEF VISUAL_BUILD}
  MTreeList, MD_MegaMain, dateutils, MegaUtils_NV,
  {$ENDIF}
  SysUtils, MLModels, MegaUtils, StringUtils, GammaFuncs, MatrixFuncs, contnrs, mstringbuilder;

var
  KTtime0: TDateTime;

procedure CreateArrayOfLongint3D(var X: PArrayOfLongint3D; n, nstates: integer);
var
  i,j,k: integer;
begin
  GetMem(X, sizeof(pointer)*n);
  for i := 0 to n-1 do
  begin
    GetMem(X[i], sizeof(pointer)*nstates);
    for j := 0 to nstates-1 do
      GetMem(X[i,j], sizeof(longint)*nstates);
  end;
  for i := 0 to n-1 do
    for j := 0 to nstates-1 do
      for k := 0 to nstates-1 do
        X[i,j,k] := 0;
end;

procedure DestroyArrayOfLongint3D(var X: PArrayOfLongint3D; n, nstates: integer);
var
  i,j: integer;
begin
  for i := 0 to n-1 do
  begin
    for j := 0 to nstates-1 do
      FreeMem(X[i,j]);
    FreeMem(X[i]);
  end;
  FreeMem(X);
  X := nil;
end;

procedure InitKTtime;
begin
  {$IFDEF DEBUG}KTtime0 := Time;{$ENDIF}
end;

{$IFDEF DEBUG}
procedure SaveMLlog(msg: string);
var
  f: TextFile;
  t: TDateTime;
  filename: String = '';
begin
  try
    MLLogCriticalSection.Acquire; { a critical section must be used, otherwise things like multi-threaded model selection lead to collisions}
    filename := ExtractFilePath(ExtractFileDir(Application.ExeName)) + 'ML.log'; { changed to use an absolute path because using a relative path is leaving log files all over the place on other developers systems}
    AssignFile(f, filename);
    if FileExists(filename) then
      Append(f)
    else
      Rewrite(f);
    t := Time;
    Writeln(f, TimeToStr(t)+' ('+TimeToStr(t-KTtime0)+') '+msg);
  finally
    CloseFile(f);
    MLLogCriticalSection.Release;
  end;
end;
{$ENDIF}

function ApproximateSizeOfTreeNodeInBytes(otu: boolean; nseqs, nsites, nrates, nstates: integer): Int64;
begin
  Result := TMLTreeNode.InstanceSize;
  Result := Result + nrates*(SizeOf(Pointer)*nstates*SizeOf(Extended)*nstates*nstates); // Prob
  Result := Result + 2*SizeOf(Pointer)*(nsites + 1); // L0, L1
  if otu then
  begin
    Result := Result + (nsites + 1)*SizeOf(Pointer);
    Result := Result + (nsites + 1)*SizeOf(Extended)*nstates; // L1
  end
  else
  begin
    Result := Result + nrates*(SizeOf(Pointer)*(nsites + 1)) + nrates*(nsites + 1)*SizeOf(Extended)*nstates; // L0
    Result := Result + nrates*(SizeOf(Pointer)*(nsites + 1)) + nrates*(nsites + 1)*SizeOf(Extended)*nstates; // L1
  end;
  Result := Result + (nsites + 1)*SizeOf(Integer); // c0
  Result := Result + (nsites + 1)*SizeOf(Integer); // c1
  Result := Result + (nsites + 1)*SizeOf(Byte); // g0
  Result := Result + (nsites + 1)*SizeOf(Byte); // g1
  Result := Result + (nsites + 1)*SizeOf(Byte); // g2
end;

function AncStatesStr(a: TAncStateRecArray): String;
var
  i: Integer = -1;
begin
  Result := EmptyStr;
  for i := Low(a) to High(a) do
    Result += Format('%s=%.2f ', [a[i].Name, a[i].Prob]);
end;

////////////////////////////
//  TModelInfo
///////////////////////////

procedure ModelInfoToStringList(Model: TModelInfo; ModelOut: TStringList);
var
  i, j: Integer;
begin
  ModelOut.Add('Model=' +  Model.FullName);
  ModelOut.Add('Num of params=' + IntToStr(Model.NoOfParams));
  ModelOut.Add('AICc=' + FloatToStr(Model.AICc));
  ModelOut.Add('BIC=' + FloatToStr(Model.BIC));
  ModelOut.Add('LnL=' + FloatToStr(Model.LogL));
  ModelOut.Add('Invar=' + FloatToStr(Model.invar));
  ModelOut.Add('Gamma=' + FloatToStr(Model.gamma));
  ModelOut.Add('R=' + FloatToStr(Model.SVR));  // MIGHT be transitions transversions rate
  for i := 0 to 3 do // for now only DNA so 4x4 transformation matrix.
  begin
    for j := 0 to 3 do
    begin
      if i = j then
        continue; // we don't transform from X to X.

      ModelOut.Add('r(' + IntMapToNuc(i) + IntMapToNuc(j) + ')=' + FloatToStr(Model.Matrix[i][j]));
    end;
  end;
end;

procedure ModelInfoToFile(Model: TModelInfo; FileName: String; NumTreesExplored: integer);
var
  ModelOut: TStringList;
begin
//  ModelOut := TStringList.Create();
  ModelOut := Model.ToStringList;
  ModelOut.Insert(0, 'NumTreesExplored=' + IntToStr(NumTreesExplored));
  try
    ModelOut.SaveToFile(FileName);
  finally
    FreeAndNil(ModelOut);
  end;
end;

/// <summary>Write model information to a text file</summary>
procedure ModelInfoToFile(Model: TModelInfo; FileName: String);
var
  ModelOut: TStringList;
begin
  ModelOut := Model.ToStringList;
  try
    ModelOut.Insert(0, '[ modelInfo ]');
    ModelOut.SaveToFile(FileName);
  finally
    FreeAndNil(ModelOut);
  end;
end;

procedure TModelInfo.Assign(Source: TModelInfo);
var
  i, j: Integer;
begin
  {$IFDEF DEBUG}
  MyWriteToDevConsole(Format('TModelInfo.Assign - ', [Source.FullName]));
  if Assigned(Source.DevConsoleProc) then
    DevConsoleProc := Source.DevConsoleProc;
  {$ENDIF}
  ModelName := Source.ModelName;
  FullName := Source.FullName;
  DataType := Source.DataType;
  NoOfParams := Source.NoOfParams;
  NoOfSamples := Source.NoOfSamples;
  LogL := Source.LogL;
  AICc := Source.AICc;
  BIC := Source.BIC;
  Gamma := Source.Gamma;
  Invar := Source.Invar;
  SVR := Source.SVR;
  NoOfRates := Source.NoOfRates;
  for i := 0 to Length(Source.Rate) - 1 do
    Rate[i] := Source.Rate[i];
  for i := 0 to Length(Source.Freq) - 1 do
    Freq[i] := Source.Freq[i];
  for i := 0 to Length(Matrix) - 1 do
    for j := 0 to Length(Matrix[i]) - 1 do
      Matrix[i][j] := Source.Matrix[i][j];
end;

procedure TModelInfo.MyWriteToDevConsole(aMsg: String);
begin
  {$IFDEF DEBUG}{$IFDEF VISUAL_BUILD}
  if Assigned(DevConsoleProc) then
    DevConsoleProc(aMsg);
  {$ENDIF}{$ENDIF}
end;

procedure TModelInfo.WriteToFile(var SessionFile: File);
var
  i: Integer = -1;
  j: Integer = -1;
  buffer: AnsiString = '';
begin
  buffer := ModelName;
  i := Length(buffer);
  BlockWrite(SessionFile, i, SizeOf(Integer));
  if i > 0 then
    BlockWrite(SessionFile, buffer[1], i);

  buffer := FullName;
  i := Length(buffer);
  BlockWrite(SessionFile, i, SizeOf(Integer));
  if i > 0 then
    BlockWrite(SessionFile, buffer[1], i);

  buffer := DataType;
  i := Length(buffer);
  BlockWrite(SessionFile, i, SizeOf(Integer));
  if i > 0 then
    BlockWrite(SessionFile, buffer[1], i);

  BlockWrite(SessionFile, NoOfParams, SizeOf(Integer));
  BlockWrite(SessionFile, NoOfSamples, SizeOf(QWord));
  BlockWrite(SessionFile, LogL, SizeOf(Double));
  BlockWrite(SessionFile, AICc, SizeOf(Double));
  BlockWrite(SessionFile, BIC, SizeOf(Double));
  BlockWrite(SessionFile, Gamma, SizeOf(Double));
  BlockWrite(SessionFile, Invar, SizeOf(Double));
  BlockWrite(SessionFile, NumSites, SizeOf(Integer));
  BlockWrite(SessionFile, NumCommonSites, SizeOf(Integer));
  BlockWrite(SessionFile, NumInvarSites, SizeOf(Integer));
  BlockWrite(SessionFile, NumGapSites, SizeOf(Integer));
  BlockWrite(SessionFile, SiteCoverage, SizeOf(Integer));
  BlockWrite(SessionFile, UseInvar, SizeOf(Boolean));
  BlockWrite(SessionFile, SVR, SizeOf(Double));
  BlockWrite(SessionFile, NoOfRates, SizeOf(integer));
  for i := Low(Rate) to High(Rate) do
    BlockWrite(SessionFile, Rate[i], SizeOf(Double));
  for i := Low(Freq) to High(Freq) do
    BlockWrite(SessionFile, Freq[i], SizeOf(Double));
  for i := Low(Matrix) to High(Matrix) do
    for j := Low(Matrix[i]) to High(Matrix[i]) do
      BlockWrite(SessionFile, Matrix[i][j], SizeOf(Double));
end;

function TModelInfo.ReadFromFile(var SessionFile: File; const SessionVersion: Integer): Boolean;
var
  i: Integer = -1;
  j: Integer = -1;
  str: AnsiString = '';
  c : AnsiChar = #0;
begin
  BlockRead(SessionFile, i, SizeOf(Integer));
  if i > 0 then
  begin
    SetLength(str, i);
    for j := 1 to i do
    begin
      BlockRead(SessionFile, c, SizeOf(AnsiChar));
      str[j] := c;
    end;
    ModelName := str;
  end;

  BlockRead(SessionFile, i, SizeOf(Integer));
  if i > 0 then
  begin
    SetLength(str, i);
    for j := 1 to i do
    begin
      BlockRead(SessionFile, c, SizeOf(AnsiChar));
      str[j] := c;
    end;
    FullName := str;
  end;

  BlockRead(SessionFile, i, SizeOf(Integer));
  if i > 0 then
  begin
    SetLength(str, i);
    for j := 1 to i do
    begin
      BlockRead(SessionFile, c, SizeOf(AnsiChar));
      str[j] := c;
    end;
    DataType := str;
  end;

  BlockRead(SessionFile, NoOfParams, SizeOf(Integer));
  BlockRead(SessionFile, NoOfSamples, SizeOf(QWord));
  BlockRead(SessionFile, LogL, SizeOf(Double));
  BlockRead(SessionFile, AICc, SizeOf(Double));
  BlockRead(SessionFile, BIC, SizeOf(Double));
  BlockRead(SessionFile, Gamma, SizeOf(Double));
  BlockRead(SessionFile, Invar, SizeOf(Double));
  BlockRead(SessionFile, NumSites, SizeOf(Integer));
  BlockRead(SessionFile, NumCommonSites, SizeOf(Integer));
  BlockRead(SessionFile, NumInvarSites, SizeOf(Integer));
  BlockRead(SessionFile, NumGapSites, SizeOf(Integer));
  BlockRead(SessionFile, SiteCoverage, SizeOf(Integer));
  BlockRead(SessionFile, UseInvar, SizeOf(Boolean));
  BlockRead(SessionFile, SVR, SizeOf(Double));
  BlockRead(SessionFile, NoOfRates, SizeOf(integer));
  for i := Low(Rate) to High(Rate) do
    BlockRead(SessionFile, Rate[i], SizeOf(Double));
  for i := Low(Freq) to High(Freq) do
    BlockRead(SessionFile, Freq[i], SizeOf(Double));
  for i := Low(Matrix) to High(Matrix) do
    for j := Low(Matrix[i]) to High(Matrix[i]) do
      BlockRead(SessionFile, Matrix[i][j], SizeOf(Double));
  Result := True;
end;

constructor TModelInfo.Create;
var
  i: Integer = -1;
  j: Integer = -1;
begin
  DevConsoleProc := nil;
  ModelName := EmptyStr;
  FullName := EmptyStr;
  DataType := EmptyStr;
  NoOfParams := 0;
  NoOfSamples := 0;
  LogL := 0;
  AICc := 0;
  BIC := 0;
  Gamma := 0;
  Invar := 0;
  NumSites := 0;
  NumCommonSites := 0;
  NumInvarSites := 0;
  NumGapSites := 0;
  SiteCoverage := 0;
  UseInvar := False;
  SVR := 0;
  NoOfRates := 0;
  for i := Low(Rate) to High(Rate) do
  begin
    Rate[i] := 0;
    Freq[i] := 0;
    for j := Low(Matrix[i]) to High(Matrix[i]) do
      Matrix[i][j] := 0;
  end;
end;

destructor TModelInfo.Destroy;
begin
  DevConsoleProc := nil;
  inherited Destroy;
end;

function TModelInfo.Letter(index: integer): AnsiChar;
var
  str: AnsiString;
begin
  if DataType = 'DNA' then
    str := 'ATCG'
  else
    str := 'ARNDCQEGHILKMFPSTWYV';
  result := str[index+1];
end;

////////////////////////////
//  TSubstitutionModel
///////////////////////////

constructor TSubstitutionModel.Create(inv: boolean; ncat: integer);
var
  i: integer;
begin
  inherited Create;
  FCancelled := False;
  UpdateProgressFunc := nil;
  deltaL  := 1.0E-5;
  MaxIter := 20;

  if ncat < 1 then
    FNoOfRates := 1
  else
    FNoOfRates := ncat;
  setlength(ch, FNoOfRates);
  setlength(FRate, FNoOfRates);
  for i := 0 to FNoOfRates-1 do
    FRate[i] := 1;

  FUseInvar := inv;
  FInvar    := 0;
  FBootTable := nil;
  FNoOfSites := 0;
  FTotalNoOfSites := 0;
  FSiteCoverage := 0;
  FOrigNoOfSites := 0;
  FInitialized := false;
end;

destructor TSubstitutionModel.Destroy;
begin
  Setlength(f0, 0);

  inherited;
end;

procedure TSubstitutionModel.SetBootTable(AValue: PArrayOfInt);
begin
  FBootTable := AValue;
end;

function TSubstitutionModel.GetFreq(index: integer): extended;
begin
  result := FFreq[index];
end;

function TSubstitutionModel.GetRate(index: integer): extended;
begin
  result := 0;
  if (index < 0) or (index >= NoOfRates) then
    exit;
  result := FRate[index];
end;

procedure TSubstitutionModel.ComputeProbOfNode(node: TMLTreeNode; b: extended);
var
  i: integer;
begin
  for i := 0 to NoOfRates-1 do
    ComputeProb(node.Prob[i], b*FRate[i]/(1-FInvar));
end;

procedure TSubstitutionModel.SortSeqs(seqs: TStringList; var h: array of integer);
var
  n: Integer = -1;
  mdif: Integer = -1;
  i,j,k,dif: integer;
  s1,s2: AnsiString;
  c1,c2: AnsiChar;
begin
  for i := 0 to seqs.Count-1 do
    h[i] := i;

  for i := 0 to seqs.Count-2 do
  begin
    for j := i+1 to seqs.Count-1 do
    begin
      s1 := seqs[h[i]];
      s2 := seqs[h[j]];
      dif := 0;
      for k := 1 to length(seqs[0]) do
      begin
        c1 := s1[k];
        c2 := s2[k];
        if (c1 = '-') or (c2 = '-') then
          continue;
        if (c1 = '?') or (c2 = '?') then
          continue;
        if upcase(c1) <> upcase(c2) then
          inc(dif);
      end;
      if j = i+1 then
      begin
        mdif := dif;
        n := j;
      end
      else if dif < mdif then
      begin
        mdif := dif;
        n := j;
      end;
    end;
    if n <> i+1 then
    begin
      j := h[i+1];
      h[i+1] := h[n];
      h[n] := j;
    end;
  end;

end;

procedure TSubstitutionModel.GetParamsFromSeqs(seqs: TStringList; diff: PArrayOfLongint3D; var s,s2: Extended; var n,n0: integer);
var
  h: array of integer;
  i,j,k,ni,d,nn,nseqs,nsites: integer;
  r,v,m,p,p0: extended;
  c1,c2: integer;
  flag: boolean;
begin
  {$IFDEF DEBUG}SaveMLlog(' TSubstitutionModel.GetParamsFromSeqs started');{$ENDIF}

  SiteCoverage := OptimumSiteCoverage(seqs);

  nseqs  := seqs.Count;
  setlength(h, nseqs);
  nsites := length(seqs[0]);
  Setlength(f0, nsites+1);

  FNoOfSites := nsites;
  if FOrigNoOfSites = 0 then
    FOrigNoOfSites := FNoOfSites;

  if SeqDataType = DNA then
    FNoOfStates := 4
  else if SeqDataType = Protein then
    FNoOfStates := 20;
  setlength(FFreq, FNoOfStates);
  for i := 0 to FNoOfStates-1 do
    FFreq[i] := 0;

  setlength(h, nseqs);
  for i := 0 to nseqs-1 do
    h[i] := 0;

  SortSeqs(seqs, h);

  FTotalNoOfSites  := 0;
  FNoOfCommonSites := 0;
  FNoOfInvarSites  := 0;
  FNoOfGapSites    := 0;
  if Assigned(UpdateProgressFunc) then
    UpdateProgressFunc(0, 'Setting model parameters');

  p0 := 0;
  n  := 0;
  n0 := 0;
  nn := 0;
  s  := 0;
  s2 := 0;
  for k := 1 to nsites do
  begin
    if SC[k] < SiteCoverage then
      continue;
    if FBootTable = nil then
      d := 1
    else if FBootTable[k] = 0 then
      continue
    else
      d := FBootTable[k];

    CheckAbort;

    f0[k] := -3;
    ni := 0;
    r  := 0;
    flag := true;
    for i := 0 to nseqs-1 do
    begin
      if SeqDataType = DNA then
        case upcase(seqs[h[i]][k]) of
          'A': c1 := 0;
          'T',
          'U': c1 := 1;
          'C': c1 := 2;
          'G': c1 := 3;
          '-': c1 := -2;
        else
          c1 := -3;
        end
      else if SeqDataType = Protein then
        case upcase(seqs[h[i]][k]) of
          'A': c1 := 0;
          'R': c1 := 1;
          'N': c1 := 2;
          'D': c1 := 3;
          'C': c1 := 4;
          'Q': c1 := 5;
          'E': c1 := 6;
          'G': c1 := 7;
          'H': c1 := 8;
          'I': c1 := 9;
          'L': c1 := 10;
          'K': c1 := 11;
          'M': c1 := 12;
          'F': c1 := 13;
          'P': c1 := 14;
          'S': c1 := 15;
          'T': c1 := 16;
          'W': c1 := 17;
          'Y': c1 := 18;
          'V': c1 := 19;
          '-': c1 := -2;
        else
          c1 := -3;
        end;
      if c1 = -3 then
        flag := false;

      if c1 <> f0[k] then
        if f0[k] < -1 then
          f0[k] := c1
        else if (f0[k] >= 0) and (c1 >= 0) then
          f0[k] := -1;

      if c1 >= 0 then
      begin
        inc(FTotalNoOfSites, d);
        FFreq[c1] := FFreq[c1] +d;

        if i = nseqs-1 then
          j := 0
        else
          j := i+1;
        repeat
          if SeqDataType = DNA then
            case upcase(seqs[h[j]][k]) of
              'A': c2 := 0;
              'T',
              'U': c2 := 1;
              'C': c2 := 2;
              'G': c2 := 3;
            else
              c2 := -1;
            end
          else if SeqDataType = Protein then
            case upcase(seqs[h[j]][k]) of
              'A': c2 := 0;
              'R': c2 := 1;
              'N': c2 := 2;
              'D': c2 := 3;
              'C': c2 := 4;
              'Q': c2 := 5;
              'E': c2 := 6;
              'G': c2 := 7;
              'H': c2 := 8;
              'I': c2 := 9;
              'L': c2 := 10;
              'K': c2 := 11;
              'M': c2 := 12;
              'F': c2 := 13;
              'P': c2 := 14;
              'S': c2 := 15;
              'T': c2 := 16;
              'W': c2 := 17;
              'Y': c2 := 18;
              'V': c2 := 19;
            else
              c2 := -1;
            end;
          if c2 >= 0 then
            break;
          j := j + 1;
        until j = nseqs;

        if c2 >= 0 then
        begin
          if c1 <> c2 then
            r := r +1;
          ni := ni +1;
          if diff <> nil then
            diff[i][c1,c2] := diff[i][c1,c2] +d;
        end;
      end;
    end;
    if flag then
      inc(FNoOfCommonSites, d);
    if f0[k] >= 0 then
      inc(FNoOfInvarSites, d)
    else if f0[k] = -2 then
      inc(FNoOfGapSites);

    if ni >= 3 then
    begin
      n  := n +d;
      nn := nn +ni*d;
      if r < 0.00000000001 then
        n0 := n0 +d
      else
      begin
        if SeqDataType = DNA then
          p := r/ni*4/3
        else
          p := r/ni*20/19;

        if p >= 1 then
          p := (ni-1)/ni;

        r := -ln(1 -p);
        p0 := p0 +exp(-r*ni)*d;

        s  := s  +r*ni*d;
        s2 := s2 +r*r*ni*ni*d;
      end;
    end;
  end;

  r := 0;
  for i := 0 to FNoOfStates-1 do
    r := r +FFreq[i];
  for i := 0 to FNoOfStates-1 do
    FFreq[i] := FFreq[i]/r;

  If not UseInvar then
    FInvar := 0
  else
  begin
    FInvar := FNoOfInvarSites/FNoOfSites -p0/(n-n0);
    if FInvar < 0 then
      FInvar := 0
    else if FInvar > FNoOfInvarSites/FNoOfSites then
      FInvar := FNoOfInvarSites/FNoOfSites;
  end;

  setlength(h, 0);
  {$IFDEF DEBUG}SaveMLlog(' TSubstitutionModel.GetParamsFromSeqs ended');{$ENDIF}
end;

procedure TSubstitutionModel.SetParamsFromSeqs(seqs: TStringList);
var
  n0,n: integer;
  s,s2 : Extended;
begin
  {$IFDEF DEBUG}WriteToDevLog('begin TSubstitutionModel.SetParamsFromSeqs');{$ENDIF}
  {$IFDEF DEBUG}SaveMLlog(' TSubstitutionModel.SetParamsFromSeqs started');{$ENDIF}

  GetParamsFromSeqs(seqs, nil, s,s2, n,n0);

//  if UseInvar then
//    InitInvar(s,s2, n,n0);
  {$IFDEF DEBUG}SaveMLlog(' TSubstitutionModel.SetParamsFromSeqs ended');{$ENDIF}
end;

procedure TSubstitutionModel.IterateInvarParameter(tree: TMLTree);
var
  p0,d,dL,ddL,dL0,dL1,dL2: extended;
begin
  if not UseInvar or (NoOfInvarSites = 0) then
    exit;

  dL0 := tree.LogLikelihood;

  if FInvar < 0.000000001 then
  begin
    FInvar := 0.000000001;
    tree.ResetProb;
    dL0 := tree.ResetL1;
  end;

  p0 := FInvar;
  d := p0/1000;

  FInvar := p0+d;
  tree.ResetProb;
  dL1 := tree.ResetL1;

  FInvar := p0-d;
  tree.ResetProb;
  dL2 := tree.ResetL1;

  dL  := (dL1-dL2)/d/2;
  ddL := ((dL1-dL0)-(dL0-dL2))/d/d;

  if abs(ddL) > 0 then
    FInvar := FInvar -dL/ddL;
  if FInvar < 0 then
    FInvar := 0
  else if FInvar > FNoOfInvarSites/FNoOfSites then { OrigNumSites used here because ModelTamer and LBS only keep unique site configurations in SeqData}
    FInvar := FNoOfInvarSites/FNoOfSites; { According to the biological definition, FNoOfSites (kept in self) should be used in any case}

  tree.ResetProb;
  tree.LogLikelihood := tree.ResetL1;
  if tree.LogLikelihood < dL0 then
  begin
    FInvar := p0;
    tree.ResetProb;
    tree.LogLikelihood := tree.ResetL1;
  end;
  Assert(CompareValue(FInvar, 1, FP_CUTOFF) <= 0, Format('Proportion of invariant sites is invalid %.4f', [FInvar]));
end;

procedure TSubstitutionModel.SetSiteCover(seqs: TStringList);
var
  i,k: integer;
begin
  if Assigned(UpdateProgressFunc) then
    UpdateProgressFunc(1, 'Setting site coverage');
  setlength(SC, length(seqs[0])+1);
  for i := 0 to high(SC) do
    SC[i] := 0;

  if SeqDataType = DNA then
  begin
    for i := 0 to seqs.Count-1 do
    begin
      if Assigned(UpdateProgressFunc) then
        UpdateProgressFunc(Round(i / (Seqs.Count - 1) * 100), 'Setting site coverage');
      for k := 1 to length(seqs[0]) do
        case upcase(seqs[i][k]) of
          'A',
          'T',
          'U',
          'C',
          'G': inc(SC[k]);
        end;
    end;
  end
  else if SeqDataType = Protein then
  begin
    for i := 0 to seqs.Count-1 do
    begin
      if Assigned(UpdateProgressFunc) then
        UpdateProgressFunc(Round(i / (Seqs.Count - 1) * 100), 'Setting site coverage');
      for k := 1 to length(seqs[0]) do
        case upcase(seqs[i][k]) of
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
          'Y': inc(SC[k]);
        end;
    end;
  end;
  if Assigned(UpdateProgressFunc) then
    UpdateProgressFunc(100, 'Setting site coverage');
end;

procedure TSubstitutionModel.CheckAbort;
begin
  if FCancelled then
    Abort;
end;

procedure TSubstitutionModel.SetCancelled(AValue: Boolean);
begin
  FCancelled := AValue;
end;

function TSubstitutionModel.EffectiveNoOfSites(seqs: TStringList): integer;
var
  i: integer;
begin
  SetSiteCover(seqs);

  result := 0;
  for i := 1 to High(SC) do
    if SC[i] >= SiteCoverage then
      inc(result);
end;

function TSubstitutionModel.OptimumSiteCoverage(seqs: TStringList): integer;
var
  c: array of integer;
  i,j,nseqs,nsites: integer;
begin
  SetSiteCover(seqs);

  nseqs  := seqs.Count;
  nsites := length(seqs[0]);
  setlength(c, nseqs+1);   // GS - changed length of c to nseqs+1, 12-19-2011. Was incorrectly set as numSites + 1 before

  for i := 0 to nseqs do
    c[i] := 0;
  for i := 1 to nsites do
    inc(c[SC[i]]);

  for i := nseqs-1 downto 0 do
    c[i] := c[i] +c[i+1];

  result := 0;
  j := 0;
  for i := 1 to nseqs do
  begin
    if c[i]*2 < nsites then
      break;
    if c[i]*i >= j then
    begin
      j := c[i]*i;
      result := i;
    end;
  end;

  setlength(c, 0);
end;

function TSubstitutionModel.SeqCoverageRate(seqs: TStringList): extended;
var
  n,nn,nseqs,nsites,k: integer;
begin
  SetSiteCover(seqs);

  nseqs  := seqs.Count;
  nsites := length(seqs[0]);
  n  := 0;
  nn := 0;
  for k := 1 to nsites do
    if SC[k] >= SiteCoverage then
    begin
      n  := n +1;
      nn := nn +SC[k];
    end;

  result := nn/n/nseqs;
end;

function TSubstitutionModel.ComputePDistanceMatrix(seqs: TStringList; D: PDistanceMatrix): boolean;
var
  n1: Integer = -1;
  n2: Integer = -2;
  i,j,k,dif,n,dd: integer;
  NumComparisons: LongInt = 0;
begin
  result := true;
  if Assigned(UpdateProgressFunc) then
  begin
    UpdateProgressFunc(0, 'Computing distance matrix');
    NumComparisons := Seqs.Count * (Seqs.Count - 1) div 2;
  end;
  SetSiteCover(seqs);

  for i := 1 to seqs.Count-1 do
  begin
    if Assigned(UpdateProgressFunc) then
        UpdateProgressFunc(Round((i * (i - 1) / 2) / NumComparisons * 100), 'Computing distance matrix');
    for j := 0 to i-1 do
    begin
      dif := 0;
      n := 0;
      for k := 1 to length(seqs[0]) do
      begin
        if FBootTable = nil then
          dd := 1
        else if FBootTable[k] = 0 then
          continue
        else
          dd := FBootTable[k];

        if SC[k] < SiteCoverage then
          continue;

        if SeqDataType = DNA then
        begin
          case upcase(seqs[i][k]) of
            'A': n1 := 0;
            'T',
            'U': n1 := 1;
            'C': n1 := 2;
            'G': n1 := 3;
          else
            continue;
          end;
          case upcase(seqs[j][k]) of
            'A': n2 := 0;
            'T',
            'U': n2 := 1;
            'C': n2 := 2;
            'G': n2 := 3;
          else
            continue;
          end;
        end
        else if SeqDataType = Protein then
        begin
          case upcase(seqs[i][k]) of
            'A': n1 :=  0;
            'C': n1 :=  1;
            'D': n1 :=  2;
            'E': n1 :=  3;
            'F': n1 :=  4;
            'G': n1 :=  5;
            'H': n1 :=  6;
            'I': n1 :=  7;
            'K': n1 :=  8;
            'L': n1 :=  9;
            'M': n1 := 10;
            'N': n1 := 11;
            'P': n1 := 12;
            'Q': n1 := 13;
            'R': n1 := 14;
            'S': n1 := 15;
            'T': n1 := 16;
            'V': n1 := 17;
            'W': n1 := 18;
            'Y': n1 := 19;
          else
            continue;
          end;
          case upcase(seqs[j][k]) of
            'A': n2 :=  0;
            'C': n2 :=  1;
            'D': n2 :=  2;
            'E': n2 :=  3;
            'F': n2 :=  4;
            'G': n2 :=  5;
            'H': n2 :=  6;
            'I': n2 :=  7;
            'K': n2 :=  8;
            'L': n2 :=  9;
            'M': n2 := 10;
            'N': n2 := 11;
            'P': n2 := 12;
            'Q': n2 := 13;
            'R': n2 := 14;
            'S': n2 := 15;
            'T': n2 := 16;
            'V': n2 := 17;
            'W': n2 := 18;
            'Y': n2 := 19;
          else
            continue;
          end;
        end;
        if n1 <> n2 then
          dif := dif +dd;
        n := n +dd;
      end;

      if n = 0 then
        D[i,j] := -1
      else
        D[i,j] := dif/n;
      D[j,i] := D[i,j];

      result := result and (n >= 10);
    end;
  end;
end;

///////////////////////////////
//  Gamma Variable rate models
///////////////////////////////

/// <summary>Construct a TGammaRateVariationModel</summary>
constructor TGammaRateVariationModel.Create(aGamma: extended; inv: boolean; ncat: integer; numThreads: integer);
var
  NumProcessorsAvailable: integer;
begin
  Self.Create(aGamma, inv, ncat);
  NumProcessorsAvailable := GetNoOfProcessors;
  if (NumProcessorsAvailable > 0) and (numThreads >= NumProcessorsAvailable) then
    FNoOfThreads := NumProcessorsAvailable
  else if numThreads > 1 then
      FNoOfThreads := numThreads
  else
      FNoOfThreads := 1;
end;

/// <summary>Construct a TGammaRateVariationModel</summary>
constructor TGammaRateVariationModel.Create(aGamma: extended; inv: boolean; ncat: integer);
begin
  inherited Create(inv, ncat);

  if aGamma > FP_CUTOFF then
    FixedGamma := true
  else
    FixedGamma := false;

  FNoOfThreads := 1;
  FGamma := aGamma;
end;

/// <summary>Clone a TGammaRateVariationModel</summary>
procedure  TGammaRateVariationModel.Assign(source: TGammaRateVariationModel);
var
  i: integer;
begin
  deltaL  := source.deltaL;
  MaxIter := source.MaxIter;

  FSeqDataType := source.FSeqDataType;
  FNoOfStates  := source.FNoOfStates;
  for i := 0 to FNoOfStates-1 do
    FFreq[i] := source.FFreq[i];
  FNoOfRates := source.FNoOfRates;
  setlength(FRate, FNoOfRates);
  for i := 0 to FNoOfRates-1 do
    FRate[i] := source.FRate[i];

  FUseInvar := source.FUseInvar;
  FInvar    := source.FInvar;
  FBootTable := source.FBootTable;

  FNoOfSites      := source.FNoOfSites;
  OrigNoOfSites   := source.OrigNoOfSites;
  FTotalNoOfSites := source.FTotalNoOfSites;
  FSiteCoverage   := source.FSiteCoverage;
  FNoOfCommonSites := source.FNoOfCommonSites;
  FNoOfInvarSites  := source.FNoOfInvarSites;
  FNoOfGapSites    := source.FNoOfGapSites;

  setlength(SC, length(source.SC));
  for i := 0 to length(SC)-1 do
    SC[i] := source.SC[i];
  setlength(ch, length(source.ch));
  for i := 0 to length(ch)-1 do
    ch[i] := source.ch[i];

  FixedGamma   := source.FixedGamma;
  FGamma       := source.FGamma;
  gamma0       := source.gamma0;
  IniGamma     := source.IniGamma;
  FNoOfThreads := source.FNoOfThreads;

  setlength(f0, FNoOfSites+1);
  for i := 0 to FNoOfSites do
    f0[i] := source.f0[i];

  FInitialized := source.FInitialized;
end;

procedure TGammaRateVariationModel.SetGamma(value: extended);
begin
  if FNoOfRates = 1 then
    exit;

  FGamma := value;
  GenerateDiscreteGamma;

  if (FGamma < 0.04) or (FGamma > 200) then
    FixedGamma := false;
end;

procedure TGammaRateVariationModel.FitGammaToRates;
begin
  FGamma := FitGammaRates(NoOfRates, FRate);

  if FGamma < 0.04 then
    FGamma := 0.04
  else if FGamma > 200 then
    FGamma := 200;

  GenerateDiscreteGamma;
end;

procedure TGammaRateVariationModel.SortSite(var f: array of integer; n: integer);

  procedure Sort(L, R: integer);
  var
    i,j: integer;
    x,t: integer;
  begin
    x := f[(L+R) div 2];
    i := L-1;
    j := R+1;
    repeat
      repeat inc(i) until x <= f[i];
      repeat dec(j) until f[j] <= x;
      t := f[i];
      f[i] := f[j];
      f[j] := t;
    until i >= j;
    f[j] := f[i];
    f[i] := t;
    if L < i-1 then Sort(L, i-1);
    if j+1 < R then Sort(j+1, R);
  end;

begin
  Sort(1, n);
end;

procedure TGammaRateVariationModel.InitGammaParameter(seqs: TStringList; tree: TTreeData);
var
  h: array of integer;
begin
  if not Initialized then
//    if assigned(tree) then
//      InitGammaParamByParsimony(seqs, tree)
//    else
    InitGammaParamFromSeqs(seqs)
  else if abs(FGamma-IniGamma) > 0.000000000001 then
  begin
    FGamma := IniGamma;
    GenerateDiscreteGamma;
  end;
end;

procedure TGammaRateVariationModel.GenerateDiscreteGamma;
begin
  GenerateDiscreteGammaRates(FGamma, NoOfRates, FRate);
end;

procedure TGammaRateVariationModel.IterateGammaParameter(tree: TMLTree);
var
  g0,d,dL,ddL,dL0,dL1,dL2,dg,eg: extended;
begin
  if (NoOfRates = 1) or FixedGamma then
    exit;

  dL0 := tree.LogLikelihood;

  d := FGamma/1000;
  g0 := FGamma;

  GenerateDiscreteGammaRates(g0+d, NoOfRates, FRate);
  tree.ResetProb;
  dL1 := tree.ResetL1;

  GenerateDiscreteGammaRates(g0-d, NoOfRates, FRate);
  tree.ResetProb;
  dL2 := tree.ResetL1;

  dL  := (dL1-dL2)/d/2;
  ddL := ((dL1-dL0)-(dL0-dL2))/d/d;
  if abs(ddL) < 0.000000000001 then
  begin
    dg := 0;
    eg := 0;
  end
  else
  begin
    dg  := -dL/ddL;
    eg := sqrt(abs(1/ddL));
  end;
  if (dL1 > dL0) and (dg < 0) then
    dg := FGamma/100
  else if (dL2 > dL0) and (dg > 0) then
    dg := -FGamma/100;
  if dg < 0 then
  begin
    if dg < -eg then
      dg := -eg;
  end
  else
  begin
    if dg > 3*eg then
      dg := 3*eg;
  end;
  FGamma := FGamma +dg;

  if (abs(dg) > 0) and (abs(dg)/2 < d) then
    d := abs(dg)/2;

  if FGamma < 0.04 then
    FGamma := 0.04
  else if FGamma > 200 then
    FGamma := 200;

  GenerateDiscreteGammaRates(FGamma, NoOfRates, FRate);
  tree.ResetProb;
  tree.LogLikelihood := tree.ResetL1;

  if tree.LogLikelihood < dL0 then
  begin
    FGamma := g0;
    GenerateDiscreteGammaRates(FGamma, NoOfRates, FRate);
    tree.ResetProb;
    tree.LogLikelihood := tree.ResetL1;
  end;

end;

procedure TGammaRateVariationModel.OptimizeParameters(tree: TMLTree; aCheckCancel: TCheckCancelFunc = nil);
var
  L0: extended;
  iter: integer;
begin
  iter := 0;
  repeat
    if assigned(aCheckCancel) then
      aCheckCancel(trunc(iter/MaxIter*100),'Optimizing rate parameters');
    L0  := tree.LogLikelihood;
    IterateGammaParameter(tree);
    IterateInvarParameter(tree);
    inc(iter);
  until (abs(tree.LogLikelihood-L0) < deltaL) or (iter = MaxIter);
  FInitialized := true;
end;

function TGammaRateVariationModel.CompositeLikelihood(M: PMatrixOfExtended; b: extended; usegamma: boolean): extended;
var
  p: PMatrixOfExtended = nil;
  i,j,n: integer;
  r0,r1: extended;
begin
  result := 0;

  CreateMatrix(p, NoOfStates);

  if FGamma < 0.000000000001 then
    n := 1
  else
    n := NoOfRates;

  if usegamma and (Gamma > 0.001) then
    ComputeProbWithGamma(p, b/(1-FInvar))
  else
    ComputeProb(p, b/(1-FInvar));

  for i := 0 to NoOfStates-1 do
    if (Freq[i]*p[i,i] > 0) and (m[i,i] > 0) then
      result := result +m[i,i]*ln(Freq[i]*(p[i,i]*(1-FInvar) +FInvar));

  for i := 1 to NoOfStates-1 do
    for j := 0 to i-1 do
    begin
      r0 := (Freq[i]*p[i,j]+Freq[j]*p[j,i])*(1-FInvar);
      r1 := m[i,j]+m[j,i];
      if (r0 > 0) and (r1 > 0) then
        result := result +r1*ln(r0);
    end;

  DestroyMatrix(p, NoOfStates);
end;

function TGammaRateVariationModel.ComputeDistance(M: PMatrixOfExtended; usegamma: boolean): extended;
var
  d0,b,d,db,dL,dL0,dL1,dL2,L,n,se0,se1: extended;
  i,j,it: integer;
begin
  result := 0;

  n := 0;
  for i := 0 to NoOfStates-1 do
    for j := 0 to NoOfStates-1 do
      n := n +M[i,j];

  if n < 10 then exit;

  d0 := n;
  for i := 0 to NoOfStates-1 do
    d0 := d0 -M[i,i];
  d0 := d0/n;

  if d0 < 0.000000000001 then exit;

  b := 1;
  for i := 0 to NoOfStates-1 do
    b := b -Freq[i]*Freq[i];

  if d0 > b*(n-1)/n then
    d0 := b*(n-1)/n;

//  if usegamma and (Gamma > 0.001) then
//    d0 := b*FGamma*(exp(-ln(1 -d0/b)/FGamma)-1)
//  else
    d0 := -b*ln(1 -d0/b);

  if d0 > 2 then
    d0 := 2;

  L := CompositeLikelihood(M,d0, usegamma);

  b := d0;
  d := b/1000;

  it := 0;
  repeat
    d0 := b;
    dL0 := L;

    dL1 := CompositeLikelihood(M, b+d, usegamma);
    dL2 := CompositeLikelihood(M, b-d, usegamma);

    dL := (dL1-dL0)-(dL0-dL2);
    if abs(dL) > 0 then
    begin
      db := d*(dL1-dL2)/2/dL;
      se0 := sqrt(abs(d*d/dL));

      if b < db then
        b := 0
      else
        b := b -db;

      L := CompositeLikelihood(M, b, usegamma);

      if abs(db)/10 < d then
        d := abs(db)/10;

      if L < dL0 then
      begin
        b := d0;
        L := dL0;
      end
      else if b > 1.0 then
      begin
        dL1 := CompositeLikelihood(M, b+d, usegamma);
        dL2 := CompositeLikelihood(M, b-d, usegamma);
        dL  := (dL1-L)-(L-dL2);
        if abs(dL) > 0 then
        begin
          se1 := sqrt(abs(d*d/dL));
          if (se1-se0) > abs(db) then
          begin
            b := d0;
            L := dL0;
          end;
        end
        else
        begin
          b := d0;
          L := dL0;
        end;
      end;
    end;

    inc(it);
  until (abs(L-dL0) < deltaL) or (it = MaxIter);

  result := b;
end;

type
  TComputeDistMatrixThread = class;

  { TComputeDistMatrixCustomThread }

  TComputeDistMatrixCustomThread = class(TThread)
  private
    FErrMsg: String;
    FIsCancelled: Boolean;
    FIsSuccess: Boolean;
    ThreadIndex: integer;
    MainThread: TComputeDistMatrixThread;
    Model: TGammaRateVariationModel;

    NoOfThreads: integer;
    FSeqs: TStringList;
    FD: PDistanceMatrix;
    UseGamma: boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(parent: TComputeDistMatrixThread; index: integer; seqs: TStringList; D: PDistanceMatrix; ug: boolean);
    destructor Destroy; override;
    property IsCancelled: Boolean read FIsCancelled;
    property IsSuccess: Boolean read FIsSuccess;
    property ErrMsg: String read FErrMsg;
  end;

  TComputeDistMatrixThread = class(TComputeDistMatrixCustomThread)
  private
    EachThread: array of TComputeDistMatrixCustomThread;
    Done: array of integer;
  protected
    procedure Execute; override;
  public
    function CheckDone: boolean;

    constructor Create(seqs: TStringList; m: TGammaRateVariationModel; D: PDistanceMatrix; ug: boolean);
  end;

constructor TComputeDistMatrixCustomThread.Create(parent: TComputeDistMatrixThread; index: integer; seqs: TStringList; D: PDistanceMatrix; ug: boolean);
var
  NumThreads: Integer;
begin
  inherited Create(true);

  Priority := tpNormal;

  MainThread  := parent;
  ThreadIndex := index;
  NumThreads := GetNoOfProcessors; { not guaranteed to succeed on Unix systems!}
  if NumThreads = -1 then { failed to detect the number of processors}
    NumThreads := 2; { 64-bit single processor systems are not longer produced so assume at least 2 available}
  if index = 0 then
    NoOfThreads := NumThreads
  else
    NoOfThreads := MainThread.NoOfThreads;
  FSeqs    := seqs;
  FD       := D;
  UseGamma := ug;

  if ThreadIndex > 0 then
    Model := CopyModel(parent.Model);

  FreeOnTerminate := true;
  FIsCancelled := False;
  FIsSuccess := True;
  FErrMsg := EmptyStr;
end;

destructor TComputeDistMatrixCustomThread.Destroy;
begin
  if ThreadIndex > 0 then
    Model.Free;

  inherited;
end;

procedure TComputeDistMatrixCustomThread.Execute;
var
  nn,n0,n1,r: integer;
  flag: boolean;
begin
  try
    nn := FSeqs.Count*(FSeqs.Count-1) div 2;

    n0 := (nn div NoOfThreads)*ThreadIndex;
    n1 := (nn div NoOfThreads)*(ThreadIndex+1)-1;
    r  := nn mod NoOfThreads;
    if r > 0 then
      if ThreadIndex < r then
      begin
        n0 := n0 +ThreadIndex;
        n1 := n1 +ThreadIndex +1;
      end
      else
      begin
        n0 := n0 +r;
        n1 := n1 +r;
      end;

    flag := Model.ComputeDistMatrixCustom(FSeqs, FD, UseGamma, n0, n1);

    if flag then
      MainThread.Done[ThreadIndex] := 1
    else
      MainThread.Done[ThreadIndex] := -1;
  except
    on E:EAbort do
    begin
      FIsCancelled := True;
      FIsSuccess := False;
      FErrMsg := 'user cancelled';
    end;
    on E:Exception do
    begin
      FIsSuccess := False;
      FErrMsg := E.Message;
    end;
  end;
end;

constructor TComputeDistMatrixThread.Create(seqs: TStringList; m: TGammaRateVariationModel; D: PDistanceMatrix; ug: boolean);
begin
  inherited Create(self,0,seqs,D,ug);

  Model   := m;

  FreeOnTerminate := false;	 // KT: needed for main process to access the ReturnValue before destroyed.
end;

procedure TComputeDistMatrixThread.Execute;
var
  i: integer;
begin
  try
    try
      setlength(EachThread, NoOfThreads);
      setlength(Done, NoOfThreads);

      EachThread[0] := self;
      for i := 1 to NoOfThreads-1 do
        EachThread[i] := nil;

      for i := 0 to NoOfThreads-1 do
        Done[i] := 0;
      for i := 1 to NoOfThreads-1 do
        EachThread[i] := TComputeDistMatrixCustomThread.Create(self, i, FSeqs, FD, UseGamma);

      for i := 1 to NoOfThreads-1 do
      begin
        EachThread[i].Start;
        EachThread[i] := nil;
      end;

      inherited Execute;

      while not CheckDone do
        ThreadSwitch;

      ReturnValue := 0;
      for i := 0 to NoOfThreads-1 do
        if Done[i] < 0 then
          ReturnValue := -1;
    except
      on E:EAbort do
      begin
        FIsCancelled := True;
        FIsSuccess := False;
        FErrMsg := 'user cancelled';
      end;
      on E:Exception do
      begin
        FIsSuccess := False;
        FErrMsg := E.Message;
      end;
    end;
  finally
    for i := 1 to NoOfThreads-1 do
      if EachThread[i] <> nil then
      begin
        FIsSuccess := FIsSuccess and EachThread[i].IsSuccess;
        FIsCancelled := FIsCancelled or EachThread[i].IsCancelled;
        FreeAndNil(EachThread[i]);
      end;
  end;
end;

function TComputeDistMatrixThread.CheckDone: boolean;
var
  i: integer;
begin
  result := true;
  for i := 0 to NoOfThreads-1 do
    result := result and (Done[i] <> 0);
end;

function TGammaRateVariationModel.ComputeDistMatrixCustom(seqs: TStringList; D: PDistanceMatrix; usegamma: boolean; n0,n1: integer): boolean;
var
  m: PMatrixOfExtended = nil;

  function SetMatrix(i,j: integer): boolean;
  var
    n2: Integer = -1;
    k,n,n1: integer;
    d,nn: extended;
  begin
    for n1 := 0 to NoOfStates-1 do
      for n2 := 0 to NoOfStates-1 do
        m[n1,n2] := 0;

    n  := length(seqs[0]);
    nn := 0;
    for k := 1 to n do
    begin
      if FBootTable = nil then
        d := 1
      else if FBootTable[k] = 0 then
        continue
      else
        d := FBootTable[k];

      if SC[k] < SiteCoverage then
        continue;

      if SeqDataType = DNA then
      begin
        case upcase(seqs[i][k]) of
          'A': n1 := 0;
          'T',
          'U': n1 := 1;
          'C': n1 := 2;
          'G': n1 := 3;
        else
          continue;
        end;
        case upcase(seqs[j][k]) of
          'A': n2 := 0;
          'T',
          'U': n2 := 1;
          'C': n2 := 2;
          'G': n2 := 3;
        else
          continue;
        end;
      end
      else if SeqDataType = Protein then
      begin
        case upcase(seqs[i][k]) of
          'A': n1 :=  0;
          'C': n1 :=  1;
          'D': n1 :=  2;
          'E': n1 :=  3;
          'F': n1 :=  4;
          'G': n1 :=  5;
          'H': n1 :=  6;
          'I': n1 :=  7;
          'K': n1 :=  8;
          'L': n1 :=  9;
          'M': n1 := 10;
          'N': n1 := 11;
          'P': n1 := 12;
          'Q': n1 := 13;
          'R': n1 := 14;
          'S': n1 := 15;
          'T': n1 := 16;
          'V': n1 := 17;
          'W': n1 := 18;
          'Y': n1 := 19;
        else
          continue;
        end;
        case upcase(seqs[j][k]) of
          'A': n2 :=  0;
          'C': n2 :=  1;
          'D': n2 :=  2;
          'E': n2 :=  3;
          'F': n2 :=  4;
          'G': n2 :=  5;
          'H': n2 :=  6;
          'I': n2 :=  7;
          'K': n2 :=  8;
          'L': n2 :=  9;
          'M': n2 := 10;
          'N': n2 := 11;
          'P': n2 := 12;
          'Q': n2 := 13;
          'R': n2 := 14;
          'S': n2 := 15;
          'T': n2 := 16;
          'V': n2 := 17;
          'W': n2 := 18;
          'Y': n2 := 19;
        else
          continue;
        end;
      end;
      m[n1,n2] := m[n1,n2] +d;
      nn := nn +d;
    end;
    result := nn >= 10;
  end;

var
  i,j,n: integer;
begin
  result := true;

  SetSiteCover(seqs);
  CreateMatrix(m, NoOfStates);

  n := 0;
  for i := 1 to seqs.Count-1 do
  begin
    if n > n1 then  break;

    for j := 0 to i-1 do
    begin
      if (n >= n0) then
      begin
        if SetMatrix(i,j) then
          D[i,j] := ComputeDistance(m, usegamma)
        else
        begin
          D[i,j] := -1;
          result := false;
        end;
        D[j,i] := D[i,j];
      end;

      inc(n);
      if n > n1 then break;
    end;
  end;

  DestroyMatrix(m, NoOfStates);
end;

function TGammaRateVariationModel.ComputeDistanceMatrix(seqs: TStringList; D: PDistanceMatrix; usegamma: boolean): boolean;
var
  ComputeDistMatrixThread: TComputeDistMatrixThread;
  //i,j: integer;
begin
  result := true;

  SetSiteCover(seqs);

//  if (nt > 1) and (seqs.Count*(seqs.Count-1) > 100*nt) then
  begin
    try
      ComputeDistMatrixThread := TComputeDistMatrixThread.Create(seqs, Self, D, usegamma);
      ComputeDistMatrixThread.NoOfThreads := NoOfThreads;
      ComputeDistMatrixThread.Start;
      ComputeDistMatrixThread.WaitFor;
      result := ComputeDistMatrixThread.ReturnValue = 0;
      if ComputeDistMatrixThread.IsCancelled then
        raise EAbort.Create('user cancelled');
    finally
      ComputeDistMatrixThread.Free;
    end;
    exit;
  end;
{
  else
  begin
    CreateMatrix(m, NoOfStates);
    for i := 1 to seqs.Count-1 do
      for j := 0 to i-1 do
      begin
        if SetMatrix(i, j) then
          D[i,j] := ComputeDistance(m, usegamma)
        else
        begin
          D[i,j] := -1;
          result := false;
        end;
        D[j,i] := D[i,j];
      end;
    DestroyMatrix(m, NoOfStates);
  end;
}
end;

procedure TGammaRateVariationModel.InitGamma(s,s2: double; n,n0: integer);
var
  i: integer;
  v,m: extended;
  gm,im,fm,g0,i0,f0,f1,df,df0: double;
begin
  if NoOfRates = 1 then
    exit;
  if UseInvar then
  begin
    m := s/n/(1-FInvar);
    v := s2/n/(1-FInvar) -m*m;
  end
  else
  begin
    m := s/n;
    v := s2/n -m*m;
  end;

  if v > m then
    FGamma := m*m/(v -m)
  else
    FGamma := 200;

  if FGamma < 0.04 then
    FGamma := 0.04
  else if FGamma > 200 then
    FGamma := 200;

  IniGamma := FGamma;

  GenerateDiscreteGamma;

  m := s/n;
  v := s2/n -m*m;
  if v > m then
    gamma0 := m*m/(v -m)
  else
    gamma0 := 200;
  if gamma0 < 0.04 then
    gamma0 := 0.04
  else if gamma0 > 200 then
    gamma0 := 200;

end;

procedure TGammaRateVariationModel.InitGammaParamFromSeqs(seqs: TStringList);
var
  n,n0: integer;
  s, s2: extended;
begin
  {$IFDEF DEBUG}SaveMLlog(' TGammaRateVariationModel.InitGammaParamFromSeqs started');{$ENDIF}

  GetParamsFromSeqs(seqs, nil, s,s2, n,n0);

  InitGamma(s, s2, n, n0);

  FInitialized := true;
  {$IFDEF DEBUG}SaveMLlog(' TGammaRateVariationModel.InitGammaParamFromSeqs ended');{$ENDIF}
end;

procedure TGammaRateVariationModel.SetParamsFromSeqs(seqs: TStringList);
var
  s,s2: Extended;
  n,n0: integer;
begin
  {$IFDEF DEBUG}SaveMLlog(' TGammaRateVariationModel.SetParamsFromSeqs started');{$ENDIF}
  GetParamsFromSeqs(seqs, nil, s,s2, n,n0);

  InitGamma(s,s2,n,n0);
  FInitialized := true;
  {$IFDEF DEBUG}SaveMLlog(' TGammaRateVariationModel.SetParamsFromSeqs ended');{$ENDIF}
end;

procedure TGammaRateVariationModel.InitGammaParamByParsimony(seqs: TStringList; tree: TTreeData);
var
  MPTree: TMPTree;
  h: array of integer;
  i,n,nh: integer;
  r,v,m,p,s,gm,im,fm,g0,i0,f0,f1,df,df0,m0,v0: double;
begin
  if NoOfRates = 1 then
    exit;
  MPTree := nil;
  try
    setlength(h, seqs.count);
    MPTree := TMPTree.Create;
    MPtree := TMPTree.Create(seqs, NoOfStates);
    nh := MPTree.LengthHistogram(tree, h);

    m0 := 0;
    v0 := 0;
    for i := 1 to nh do
    begin
      m0 := m0 +i*h[i];
      v0 := v0 +i*i*h[i];
    end;
    n := NoOfSites;

    if UseInvar then
    begin
      i0 := FInvar;
      f0 := h[0]/n -FInvar;
      fm := 1;
      i := 0;
      repeat
        m := m0/n/(1-i0);
        v := v0/n/(1-i0) -m*m;
        if v > m then
          g0 := m*m/(v -m)
        else
          g0 := 200;

        f1 := exp(-g0*ln(1+m/g0));

        df := abs(f0-f1);
        if df < fm then
        begin
          gm := g0;
          im := i0;
          fm := df;
        end;

        df0 := f0;
        f0 := (f0+f1)/2;
        if f0 > h[0]/n then
          f0 := h[0]/n;
        if f0 > FNoOfInvarSites/FNoOfSites then
          f0 := FNoOfInvarSites/FNoOfSites;
        i0 := h[0]/n -f0;
        df0 := abs(df0-f0);
        inc(i);
      until (df0 < 0.00001) or (i = 100);
      FGamma := gm;
      FInvar := im;
    end
    else
    begin
      m := m0/n;
      v := v0/n -m*m;

      if v > m then
        FGamma := m*m/(v -m)
      else
        FGamma := 200;

      if FGamma < 0.04 then
        FGamma := 0.04
      else if FGamma > 200 then
        FGamma := 200;
    end;
  finally
    setlength(h, 0);
    if assigned(MPTree) then
      MPTree.Free;
  end;
end;

function TGammaRateVariationModel.GetDescription: String;
begin
  Result := GetName;
  if NoOfRates > 1 then
    Result := Result + ' + G';
  if UseInvar then
    Result := Result + ' + I';
end;

procedure TGammaRateVariationModel.GetInfo(ModelInfo: TModelInfo);
var
  i,j: integer;
begin
  if ModelInfo = nil then exit;

  for i := 0 to 19 do
  begin
    ModelInfo.Rate[i] := 0;
    ModelInfo.Freq[i] := 0;
    for j := 0 to 19 do
      ModelInfo.Matrix[i,j] := 0;
  end;

  ModelInfo.DataType   := '';
  ModelInfo.LogL       := 0;
  ModelInfo.NoOfParams := 0;
  ModelInfo.NoOfRates  := NoOfRates;
  ModelInfo.Gamma      := Gamma;
  ModelInfo.Invar      := Invar;

  ModelInfo.UseInvar := UseInvar;
  ModelInfo.NumCommonSites := NoOfCommonSites;
  ModelInfo.NumGapSites := NoOfGapSites;
  ModelInfo.NumInvarSites := NoOfInvarSites;
  ModelInfo.NumSites := NoOfSites;
  ModelInfo.SiteCoverage := SiteCoverage;

  for i := 0 to NoOfRates-1 do
    ModelInfo.Rate[i] := Rate[i];

  if NoOfRates > 1 then
    if UseInvar then
    begin
      ModelInfo.ModelName := '+G+I';
      ModelInfo.FullName  := ' (+G+I)'
    end
    else
    begin
      ModelInfo.ModelName := '+G';
      ModelInfo.FullName  := ' (+G)'
    end
  else if UseInvar then
  begin
    ModelInfo.ModelName := '+I';
    ModelInfo.FullName  := ' (+I)'
  end;

end;

function TGammaRateVariationModel.GetName: String;
begin
  Result := 'Gamma Rate Variation Model';
end;

procedure TGammaRateVariationModel.WriteToFile(var data: File);
var
  i,version: integer;
begin
  version := 2;

  BlockWrite(data, version, sizeof(integer));

  BlockWrite(data, deltaL, sizeof(extended));
  BlockWrite(data, MaxIter, sizeof(integer));

  BlockWrite(data, FSeqDataType, sizeof(TSeqDataType));

  BlockWrite(data, FNoOfRates, sizeof(integer));
  for i := 0 to FNoOfRates-1 do
    BlockWrite(data, FRate[i], sizeof(extended));

  BlockWrite(data, FNoOfStates, sizeof(integer));
  for i := 0 to FNoOfStates-1 do
    BlockWrite(data, FFreq[i], sizeof(extended));

  BlockWrite(data, FUseInvar, sizeof(boolean));
  BlockWrite(data, FInvar, sizeof(extended));

  BlockWrite(data, FFixedGamma, sizeof(boolean));
  BlockWrite(data, FGamma, sizeof(extended));

  BlockWrite(data, FNoOfInvarSites, sizeof(integer));

  BlockWrite(data, FTotalNoOfSites, sizeof(QWord));
  BlockWrite(data, FNoOfCommonSites, sizeof(QWord));
end;

function TGammaRateVariationModel.ReadFromFile(var data: File):boolean;
var
  i: Integer;
  version: Integer = -1;
begin
  result := false;
  try
    BlockRead(data, version, sizeof(integer));

    BlockRead(data, deltaL, sizeof(extended));
    BlockRead(data, MaxIter, sizeof(integer));

    BlockRead(data, FSeqDataType, sizeof(TSeqDataType));

    BlockRead(data, FNoOfRates, sizeof(integer));
    setlength(ch, FNoOfRates);
    setlength(FRate, FNoOfRates);
    for i := 0 to FNoOfRates-1 do
      BlockRead(data, FRate[i], sizeof(extended));

    BlockRead(data, FNoOfStates, sizeof(integer));
    for i := 0 to FNoOfStates-1 do
      BlockRead(data, FFreq[i], sizeof(extended));

    BlockRead(data, FUseInvar, sizeof(boolean));
    BlockRead(data, FInvar, sizeof(extended));

    BlockRead(data, FFixedGamma, sizeof(boolean));
    BlockRead(data, FGamma, sizeof(extended));

    BlockRead(data, FNoOfInvarSites, sizeof(integer));
    if version > 1 then
    begin
      BlockRead(data, FTotalNoOfSites, sizeof(QWord));
      BlockRead(data, FNoOfCommonSites, sizeof(QWord));
    end
    else
    begin
      BlockRead(data, FTotalNoOfSites, sizeof(Integer));
      BlockRead(data, FNoOfCommonSites, sizeof(Integer));
    end;
    result := true;
  except
    result := false;
  end;
end;

/////////////////////////
// TMLTreeNode
/////////////////////////

constructor TMLTreeNode.Create(AIndex: integer; aOtu: boolean; nseqs, nsites, nrates, nstates, norigsites: integer);
var
  i,j: integer;
begin
  inherited Create;

  index := AIndex;

  NoOfSites  := nsites;
  NoOfStates := nstates;
  NoOfSeqs   := nseqs;
  OrigNoOfSites := norigsites;

  setlength(Prob, nrates);
  for i := 0 to nrates-1 do
  begin
    GetMem(Prob[i], SizeOf(Pointer)*nstates);
    for j := 0 to nstates-1 do
      GetMem(Prob[i][j], SizeOf(extended)*nstates);
  end;

  setlength(L0, nrates);
  setlength(L1, nrates);
  for i := 0 to nrates-1 do
    L0[i] := nil;
  for i := 0 to nrates-1 do
    L1[i] := nil;

  c1 := AllocMem(SizeOf(integer)*(nsites+1));
  g1 := AllocMem(SizeOf(byte)*(nsites+1));
  if aOtu then
  begin
    c0 := c1;
    g0 := g1;
    g2 := g1;
  end
  else
  begin
    c0 := AllocMem(SizeOf(integer)*(nsites+1));
    g0 := AllocMem(SizeOf(byte)*(nsites+1));
    g2 := AllocMem(SizeOf(byte)*(nsites+1));
  end;

  setlength(tmpL0, nrates);
  setlength(tmpL1, nrates);
  for i := 0 to nrates-1 do
  begin
    tmpL0[i] := AllocMem(SizeOf(extended)*nstates);
    tmpL1[i] := AllocMem(SizeOf(extended)*nstates);
  end;

end;

destructor TMLTreeNode.Destroy;
var
  i,j,k: integer;
begin
  if Prob <> nil then
  begin
    for i := 0 to length(Prob)-1 do
    begin
      if Prob[i] <> nil then
      begin
        for j := 0 to NoOfStates-1 do
          FreeMemAndNil(Prob[i][j]);
        FreeMemAndNil(Prob[i]);
      end;
    end;
    setlength(Prob,0);
  end;

  if nc0 > 0 then
  begin
    try
    {$IFDEF DARWIN}NodeCriticalSection.Acquire;{$ENDIF}
    for i := 0 to length(L0)-1 do
      if L0[i] <> nil then
      begin
        for k := 0 to nc0 do
          FreeMemAndNil(L0[i,k]);
        FreeMemAndNil(L0[i]);
      end;
  finally
    {$IFDEF DARWIN}NodeCriticalSection.Release;{$ENDIF}
  end;
    FreeMemAndNil(cs0);
  end;
  if nc1 > 0 then
  begin
    try
    {$IFDEF DARWIN}NodeCriticalSection.Acquire;{$ENDIF}
    for i := 1 to length(L1)-1 do
      if (L1[i] <> nil) and (L1[i] <> L1[0]) then
      begin
        for k := 0 to nc1 do
          FreeMemAndNil(L1[i,k]);
        FreeMemAndNil(L1[i]);
      end;
    if L1[0] <> nil then
    begin
      for k := 0 to nc1 do
        FreeMemAndNil(L1[0,k]);
      FreeMemAndNil(L1[0]);
    end;
  finally
    {$IFDEF DARWIN}NodeCriticalSection.Release;{$ENDIF}
  end;
    FreeMemAndNil(cs1);
  end;

  SetLength(L0, 0);
  SetLength(L1, 0);

  if not (c0 = c1) then
    FreeMemAndNil(c0);
  FreeMemAndNil(c1);
  if not (g0 = g1) then
    FreeMemAndNil(g0);
  if not (g2 = g1) then
    FreeMemAndNil(g2);
  FreeMemAndNil(g1);

  for i := 0 to length(tmpL0)-1 do
    FreeMemAndNil(tmpL0[i]);
  for i := 0 to length(tmpL1)-1 do
    FreeMemAndNil(tmpL1[i]);
  setlength(tmpL0, 0);
  setlength(tmpL1, 0);

  inherited;
end;

procedure TMLTreeNode.AllocNodeMem0;
var
  i,k: integer;
begin
  if nc0 > 0 then
  try
    {$IFDEF DARWIN}NodeCriticalSection.Acquire;{$ENDIF}
    for i := 0 to length(L0)-1 do
    begin
      L0[i] := AllocMem(SizeOf(Pointer)*(nc0+1));
      for k := 0 to nc0 do
        L0[i][k] := AllocMem(SizeOf(extended)*NoOfStates);
    end;
  finally
    {$IFDEF DARWIN}NodeCriticalSection.Release;{$ENDIF}
  end;
end;


procedure TMLTreeNode.DeallocNodeMem0;
var
  i,k: integer;
begin
  try
    {$IFDEF DARWIN}NodeCriticalSection.Acquire;{$ENDIF}
    for i := 0 to length(L0)-1 do
    if L0[i] <> nil then
    begin
      for k := 0 to nc0 do
        FreeMemAndNil(L0[i,k]);
      FreeMemAndNil(L0[i]);
    end;
  finally
    {$IFDEF DARWIN}NodeCriticalSection.Release;{$ENDIF}
  end;
end;

procedure TMLTreeNode.AllocNodeMem1;
var
  i,k: integer;
begin
  try
    {$IFDEF DARWIN}NodeCriticalSection.Acquire;{$ENDIF}
    if nc1 > 0 then
      for i := 0 to length(L1)-1 do
      begin
        L1[i] := AllocMem(SizeOf(Pointer)*(nc1+1));
        for k := 0 to nc1 do
          L1[i][k] := AllocMem(SizeOf(extended)*NoOfStates);
      end;
  finally
    {$IFDEF DARWIN}NodeCriticalSection.Release;{$ENDIF}
  end;
end;

procedure TMLTreeNode.DeallocNodeMem1;
var
  i,k: integer;
begin
  try
    {$IFDEF DARWIN}NodeCriticalSection.Acquire;{$ENDIF}
    for i := 1 to length(L1)-1 do
      if (L1[i] <> nil) and (L1[i] <> L1[0]) then
      begin
        for k := 0 to nc1 do
          FreeMemAndNil(L1[i,k]);
        FreeMemAndNil(L1[i]);
      end;
    if L1[0] <> nil then
    begin
      for k := 0 to nc1 do
        FreeMemAndNil(L1[0,k]);
      FreeMemAndNil(L1[0]);
    end;
  finally
    {$IFDEF DARWIN}NodeCriticalSection.Release;{$ENDIF}
  end;
end;

procedure TMLTreeNode.InitOTUMem;
var
  i,k: integer;
begin
  if nc0 > 0 then
  begin
      try
	    {$IFDEF DARWIN}NodeCriticalSection.Acquire;{$ENDIF}
	    for i := 0 to length(L0)-1 do
	    if L0[i] <> nil then
	    begin
	      for k := 0 to nc0 do
	        FreeMemAndNil(L0[i,k]);
	      FreeMemAndNil(L0[i]);
	    end;
	  finally
	    {$IFDEF DARWIN}NodeCriticalSection.Release;{$ENDIF}
	  end;
    FreeMemAndNil(cs0);
    nc0 := 0;
  end;

  if nc1 > 0 then
  begin
    try
	    {$IFDEF DARWIN}NodeCriticalSection.Acquire;{$ENDIF}
	    for i := 1 to length(L1)-1 do
	      if (L1[i] <> nil) and (L1[i] <> L1[0]) then
	      begin
	        for k := 0 to nc1 do
	          FreeMemAndNil(L1[i,k]);
	        FreeMemAndNil(L1[i]);
	      end;
	    if L1[0] <> nil then
	    begin
	      for k := 0 to nc1 do
	        FreeMemAndNil(L1[0,k]);
	      FreeMemAndNil(L1[0]);
	    end;
	  finally
	    {$IFDEF DARWIN}NodeCriticalSection.Release;{$ENDIF}
	  end;
    FreeMemAndNil(cs1);
  end;

  if NoOfStates = 4 then
    nc1 := 16
  else if NoOfStates = 20 then
    nc1 := 22;
  cs1 := AllocMem(SizeOf(integer)*(nc1+1));
  L1[0] := GetMem(SizeOf(Pointer)*(nc1+1));
  for k := 0 to nc1 do
    L1[0][k] := AllocMem(SizeOf(extended)*NoOfStates);
  for i := 1 to length(L1)-1 do
    L1[i] := L1[0];
end;

function TMLTreeNode.OTU: boolean;
begin
  result := (des1 = nil) or (des2 = nil);
end;

procedure TMLTreeNode.Assign(source: TMLTreeNode);
var
  i,j,k: integer;
begin
  if NoOfSites <> source.NoOfSites then
  begin
    Assert(false, 'Illigal TMLTreeNode assignment');
    exit;
  end;

  index  := source.index;
  anc    := source.anc;
  des1   := source.des1;
  des2   := source.des2;
  size   := source.size;
  depth  := source.depth;
  blen   := source.blen;
  vb     := source.vb;
  CI     := source.CI;

  flag     := source.flag;
  done     := source.done;
  anchored := source.anchored;
  fixed    := source.fixed;

  NoOfSites  := source.NoOfSites;
  OrigNoOfSites := source.OrigNoOfSites;
  NoOfStates := source.NoOfStates;
  NoOfSeqs   := source.NoOfSeqs;

  DataCoverage := source.DataCoverage;

  for i := 0 to length(Prob)-1 do
    for j := 0 to NoOfStates-1 do
      for k := 0 to NoOfStates-1 do
        Prob[i][j,k] := source.Prob[i][j,k];

  if OTU then
  begin
    if c0 <> c1 then
      FreeMemAndNil(c0);
    c0 := c1;
    if g0 <> g1 then
      FreeMemAndNil(g0);
    g0 := g1;
    if g2 <> g1 then
      FreeMemAndNil(g2);
    g2 := g1;
  end
  else
  begin
    if c0 = c1 then
      c0 := AllocMem(SizeOf(integer)*(NoOfSites+1));
    if g0 = g1 then
      g0 := AllocMem(SizeOf(byte)*(NoOfSites+1));
    if g2 = g1 then
      g2 := AllocMem(SizeOf(byte)*(NoOfSites+1));
  end;

  for k := 0 to NoOfSites do
  begin
    c1[k]  := source.c1[k];
    g1[k]  := source.g1[k];
  end;
  if not OTU then
    for k := 0 to NoOfSites do
    begin
      c0[k]  := source.c0[k];
      g0[k]  := source.g0[k];
      g2[k]  := source.g2[k];
    end;

  if OTU then
    InitOTUMem
  else
  begin
    if nc0 > 0 then
    begin
      DeallocNodeMem0;
      FreeMemAndNil(cs0);
    end;
    nc0 := source.nc0;
    if nc0 > 0 then
    begin
      AllocNodeMem0;
      cs0 := AllocMem(SizeOf(integer)*(nc0+1));
    end;
    if nc1 > 0 then
    begin
      DeallocNodeMem1;
      FreeMemAndNil(cs1);
    end;
    nc1 := source.nc1;
    if nc1 > 0 then
    begin
      AllocNodeMem1;
      cs1 := AllocMem(SizeOf(integer)*(nc1+1));
    end;
  end;

  if nc0 > 0 then
    for k := 0 to nc0 do
      cs0[k] := source.cs0[k];
  if nc1 > 0 then
    for k := 0 to nc1 do
      cs1[k] := source.cs1[k];

  if nc1 > 0 then
    for k := 0 to nc1 do
      for j := 0 to NoOfStates-1 do
        L1[0,k,j] := source.L1[0,k,j];

  if not OTU then
  begin
    if nc0 > 0 then
      for i := 0 to length(L0)-1 do
        if source.L0[0] <> nil then
          for k := 0 to nc0 do
            for j := 0 to NoOfStates-1 do
              L0[i,k,j] := source.L0[i,k,j];

    if nc1 > 0 then
      for i := 1 to length(L1)-1 do
        for k := 0 to nc1 do
          for j := 0 to NoOfStates-1 do
            L1[i,k,j] := source.L1[i,k,j];
  end;

end;

function TMLTreeNode.Assigned: boolean;
begin
  result := (anc <> nil) or ((des1 <> nil) and (des2 <> nil));
end;

function TMLTreeNode.DebugString: String;
begin
  { index, anc, des1, des2, depth, blen, rate, Ixx, vh, vb vr, height, minh, maxh, ho, dl}
  Result := Format('%-6d', [index]) + #9;
  if anc <> nil then
    Result := Result + Format('%-4d', [anc.index]) + #9
  else
    Result := Result + Format('%s', ['-']) + #9;
  if des1 <> nil then
    Result := Result + Format('%-4d', [des1.index]) + #9
  else
    Result := Result + Format('%s', ['-']) + #9;
  if des2 <> nil then
    Result := Result + Format('%-4d', [des2.index]) + #9
  else
    Result := Result + Format('%s', ['-']) + #9;
  Result := Result + Format('%-6d', [depth]) + #9;
  Result := Result + Format('%.5f', [blen]) + #9;
//  Result := Result + Format('%.5f', [rate]) + #9;
//  Result := Result + Format('%.8f', [vh]) + #9;
//  Result := Result + Format('%.8f', [hse]) + #9;
  Result := Result + Format('%.8f', [vb]) + #9;
//  Result := Result + Format('%.6f', [height]);
end;

function TMLTreeNode.GetSisterNode: TMLTreeNode;
begin
  if anc = nil then
    result := nil
  else if self = anc.des1 then
    result := anc.des2
  else
    result := anc.des1;
end;

function TMLTreeNode.GetASE: extended;
begin
  result := sqrt(abs(vb));
end;
{
function TMLTreeNode.GetHSE: extended;
begin
  result := sqrt(abs(vh));
  Assert(not IsNan(Result), 'not a number encountered');
end;
}
procedure TMLTreeNode.SetASE(value: extended);
begin
  vb := value*value;
end;

function TMLTreeNode.GetApproximateSizeInBytes: Int64;
var
  nrates: Integer;
begin
  nrates := Length(Prob);
  Result := TMLTreeNode.InstanceSize; // self
  Result := Result + SizeOf(Pointer)*nrates + SizeOf(Pointer)*nrates*NoOfStates + SizeOf(Extended)*NoOfStates*NoOfStates; // Prob

  if otu then
  begin
    Result := Result + (SizeOf(Pointer) + SizeOf(Extended)*NoOfStates)*(nc1+1);   // L1
    Result := Result + (SizeOf(Integer)*(NoOfSites + 1)); // c1
    Result := Result + (SizeOf(Byte)*(NoOfSites + 1)); // g1
  end
  else
  begin
    Result := Result + (SizeOf(Pointer) + SizeOf(Extended)*NoOfStates)*(nc1+1)*nrates; // L1
    Result := Result + 2*(SizeOf(Integer)*(NoOfSites + 1)); // c0, c1
    Result := Result + 3*(SizeOf(Byte)*(NoOfSites + 1)); // g0, g1, g2
  end;
//  Result := Result + SizeOf(Extended)*(NoOfSites + 1); // L
end;

{
procedure TMLTreeNode.SetHSE(const Value: extended);
begin
  vh := value*value;
end;

function TMLTreeNode.GetRSE: extended;
begin
  result := sqrt(abs(vr));
end;

procedure TMLTreeNode.SetRSE(value: extended);
begin
  vr := value*value;
end;
}
//////////////////////////////
//  TMLTree
//////////////////////////////

constructor TMLTree.Create(seqs: TStringList; tree: TTreeData; model: TGammaRateVariationModel; needsOutgroupRoot: Boolean = False);
begin
  inherited Create;
  DoConstruction(Length(seqs[0]), seqs, tree, model, needsOutgroupRoot);
  FOptimizePrecision := 8;
end;

constructor TMLTree.CreateWithCompressedData(origSeqLength: Integer; seqs: TStringlist; tree: TTreeData; model: TGammaRateVariationModel; needsOutgroupRoot: Boolean);
begin
  DoConstruction(origSeqLength, seqs, tree, model, needsOutgroupRoot);
end;

destructor TMLTree.Destroy;
var
  i: integer;
begin
//  for i := 0 to 2*NoOfSeqs-2 do    // this is not freeing the last node
  for i := Low(node) to High(node) do
    if Assigned(node[i]) then
      FreeAndNil(node[i]);

  setlength(nc, 0);
  setlength(bc, 0);

  setlength(Node, 0);

  FReltimeComputer.Free;

  inherited;
end;

/// <summary>Get the number of threads to use.</summary>
function TMLTree.GetNoOfThreads: integer;
begin
  Result := FNoOfThreads;
end;

procedure TMLTree.SetCancelled(AValue: Boolean);
begin
  FCancelled := AValue;
end;

procedure TMLTree.SetModel(newmodel: TGammaRateVariationModel);
begin
  FreeAndNil(FModel);
  FModel := newmodel;
  ResetProb;
  FLogLikelihood := ResetL1;
end;

function TMLTree.GetPropagateConstraints: Boolean;
begin
  Result := False;
  if Assigned(FReltimeComputer) then
    Result := FReltimeComputer.PropagateConstraints;
end;

procedure TMLTree.SetLogLikelihood(AValue: extended);
begin
  if AValue > 0 then
    raise Exception.Create(Format('log likelihood cannot be a positive value but got %.3e', [AValue]));
  if FLogLikelihood = AValue then Exit;
  FLogLikelihood := AValue;
end;

function TMLTree.GetNoOfConfs: integer;
begin
  result := Root.nc1;
end;

procedure TMLTree.ResetNodes;
var
  i: integer;
begin
  for i := 0 to FNoOfSeqs-1 do
  begin
//    node[i].index := i;
    node[i].size  := 1;
    node[i].depth := 0;
    node[i].blen  := 0.0;
    node[i].anc   := nil;
    node[i].des1  := nil;
    node[i].des2  := nil;
    node[i].flag  := false;
    node[i].IsOutgroupMember := false;
  end;
  for i := FNoOfSeqs to 2*FNoOfSeqs-2 do
  begin
//    node[i].Index := i;
    node[i].size  := 0;
    node[i].depth := 0;
    node[i].blen  := 0.0;
    node[i].anc   := nil;
    node[i].des1  := nil;
    node[i].des2  := nil;
    node[i].flag  := false;
    node[i].IsOutgroupMember := false;
  end;
end;

procedure TMLTree.InitOTUConfig;
var
  i,k: integer;
begin
  {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.InitOTUConfig');{$ENDIF}

  for i := 0 to NoOfSeqs-1 do
    with node[i] do
    begin
      k := Round(i/NoOfSeqs*100);
      if k mod 10 = 0 then                // To relax flickering
        SubtaskProgress(k, 'Initializing site configuration');
      InitOTUMem;
      c1[0] := 0;
      case Model.SeqDataType of
        DNA:
          begin
            for k := Length(Seqs[0]) downto 1 do
            begin
              case upcase(Seqs[i][k]) of
                'A': begin c1[k] := 1;  cs1[1]  := k; end;
                'T',
                'U': begin c1[k] := 2;  cs1[2]  := k; end;
                'C': begin c1[k] := 3;  cs1[3]  := k; end;
                'G': begin c1[k] := 4;  cs1[4]  := k; end;
                'R': begin c1[k] := 5;  cs1[5]  := k; end;
                'Y': begin c1[k] := 6;  cs1[6]  := k; end;
                'M': begin c1[k] := 7;  cs1[7]  := k; end;
                'K': begin c1[k] := 8;  cs1[8]  := k; end;
                'W': begin c1[k] := 9;  cs1[9]  := k; end;
                'S': begin c1[k] := 10; cs1[10] := k; end;
                'B': begin c1[k] := 11; cs1[11] := k; end;
                'V': begin c1[k] := 12; cs1[12] := k; end;
                'D': begin c1[k] := 13; cs1[13] := k; end;
                'H': begin c1[k] := 14; cs1[14] := k; end;
                '-': begin c1[k] := 15; cs1[15] := k; end;
                else
                begin
                  c1[k] := 16;  cs1[16] := k;
                end;
                if c1[k] = 15 then
                  g1[k] := 2
                else
                  g1[k] := 1;
              end;
            end;
            L1[0,1,0]  := 1; L1[0,1,1]  := 0; L1[0,1,2]  := 0; L1[0,1,3]  := 0;   // A
            L1[0,2,0]  := 0; L1[0,2,1]  := 1; L1[0,2,2]  := 0; L1[0,2,3]  := 0;   // T, U
            L1[0,3,0]  := 0; L1[0,3,1]  := 0; L1[0,3,2]  := 1; L1[0,3,3]  := 0;   // C
            L1[0,4,0]  := 0; L1[0,4,1]  := 0; L1[0,4,2]  := 0; L1[0,4,3]  := 1;   // G
            L1[0,5,0]  := 1; L1[0,5,1]  := 0; L1[0,5,2]  := 0; L1[0,5,3]  := 1;   // R
            L1[0,6,0]  := 0; L1[0,6,1]  := 1; L1[0,6,2]  := 1; L1[0,6,3]  := 0;   // Y
            L1[0,7,0]  := 1; L1[0,7,1]  := 0; L1[0,7,2]  := 1; L1[0,7,3]  := 0;   // M
            L1[0,8,0]  := 0; L1[0,8,1]  := 1; L1[0,8,2]  := 0; L1[0,8,3]  := 1;   // K
            L1[0,9,0]  := 1; L1[0,9,1]  := 1; L1[0,9,2]  := 0; L1[0,9,3]  := 0;   // W
            L1[0,10,0] := 0; L1[0,10,1] := 0; L1[0,10,2] := 1; L1[0,10,3] := 1;   // S
            L1[0,11,0] := 0; L1[0,11,1] := 1; L1[0,11,2] := 1; L1[0,11,3] := 1;   // B
            L1[0,12,0] := 1; L1[0,12,1] := 0; L1[0,12,2] := 1; L1[0,12,3] := 1;   // V
            L1[0,13,0] := 1; L1[0,13,1] := 1; L1[0,13,2] := 0; L1[0,13,3] := 1;   // D
            L1[0,14,0] := 1; L1[0,14,1] := 1; L1[0,14,2] := 1; L1[0,14,3] := 0;   // H
            L1[0,15,0] := 1; L1[0,15,1] := 1; L1[0,15,2] := 1; L1[0,15,3] := 1;   // -
            L1[0,16,0] := 1; L1[0,16,1] := 1; L1[0,16,2] := 1; L1[0,16,3] := 1;   // N
          end;
        Protein:
          begin
            for k := Length(Seqs[0]) downto 1 do
            begin
              case upcase(Seqs[i][k]) of
                'A': begin c1[k] := 1;  cs1[1]  := k; end;
                'R': begin c1[k] := 2;  cs1[2]  := k; end;
                'N': begin c1[k] := 3;  cs1[3]  := k; end;
                'D': begin c1[k] := 4;  cs1[4]  := k; end;
                'C': begin c1[k] := 5;  cs1[5]  := k; end;
                'Q': begin c1[k] := 6;  cs1[6]  := k; end;
                'E': begin c1[k] := 7;  cs1[7]  := k; end;
                'G': begin c1[k] := 8;  cs1[8]  := k; end;
                'H': begin c1[k] := 9;  cs1[9]  := k; end;
                'I': begin c1[k] := 10; cs1[10] := k; end;
                'L': begin c1[k] := 11; cs1[11] := k; end;
                'K': begin c1[k] := 12; cs1[12] := k; end;
                'M': begin c1[k] := 13; cs1[13] := k; end;
                'F': begin c1[k] := 14; cs1[14] := k; end;
                'P': begin c1[k] := 15; cs1[15] := k; end;
                'S': begin c1[k] := 16; cs1[16] := k; end;
                'T': begin c1[k] := 17; cs1[17] := k; end;
                'W': begin c1[k] := 18; cs1[18] := k; end;
                'Y': begin c1[k] := 19; cs1[19] := k; end;
                'V': begin c1[k] := 20; cs1[20] := k; end;
                '-': begin c1[k] := 21; cs1[21] := k; end;
                else
                begin
                  c1[k] := 22; cs1[22] := k;
                end;
              end;
              if c1[k] = 21 then
                g1[k] := 2
              else
                g1[k] := 1;
            end;
            L1[0,1,0]   := 1; L1[0,1,1]   := 0; L1[0,1,2]   := 0; L1[0,1,3]   := 0; L1[0,1,4]   := 0;
            L1[0,1,5]   := 0; L1[0,1,6]   := 0; L1[0,1,7]   := 0; L1[0,1,8]   := 0; L1[0,1,9]   := 0;
            L1[0,1,10]  := 0; L1[0,1,11]  := 0; L1[0,1,12]  := 0; L1[0,1,13]  := 0; L1[0,1,14]  := 0;
            L1[0,1,15]  := 0; L1[0,1,16]  := 0; L1[0,1,17]  := 0; L1[0,1,18]  := 0; L1[0,1,19]  := 0;  // A
            L1[0,2,0]   := 0; L1[0,2,1]   := 1; L1[0,2,2]   := 0; L1[0,2,3]   := 0; L1[0,2,4]   := 0;
            L1[0,2,5]   := 0; L1[0,2,6]   := 0; L1[0,2,7]   := 0; L1[0,2,8]   := 0; L1[0,2,9]   := 0;
            L1[0,2,10]  := 0; L1[0,2,11]  := 0; L1[0,2,12]  := 0; L1[0,2,13]  := 0; L1[0,2,14]  := 0;
            L1[0,2,15]  := 0; L1[0,2,16]  := 0; L1[0,2,17]  := 0; L1[0,2,18]  := 0; L1[0,2,19]  := 0;  // R
            L1[0,3,0]   := 0; L1[0,3,1]   := 0; L1[0,3,2]   := 1; L1[0,3,3]   := 0; L1[0,3,4]   := 0;
            L1[0,3,5]   := 0; L1[0,3,6]   := 0; L1[0,3,7]   := 0; L1[0,3,8]   := 0; L1[0,3,9]   := 0;
            L1[0,3,10]  := 0; L1[0,3,11]  := 0; L1[0,3,12]  := 0; L1[0,3,13]  := 0; L1[0,3,14]  := 0;
            L1[0,3,15]  := 0; L1[0,3,16]  := 0; L1[0,3,17]  := 0; L1[0,3,18]  := 0; L1[0,3,19]  := 0;  // N
            L1[0,4,0]   := 0; L1[0,4,1]   := 0; L1[0,4,2]   := 0; L1[0,4,3]   := 1; L1[0,4,4]   := 0;
            L1[0,4,5]   := 0; L1[0,4,6]   := 0; L1[0,4,7]   := 0; L1[0,4,8]   := 0; L1[0,4,9]   := 0;
            L1[0,4,10]  := 0; L1[0,4,11]  := 0; L1[0,4,12]  := 0; L1[0,4,13]  := 0; L1[0,4,14]  := 0;
            L1[0,4,15]  := 0; L1[0,4,16]  := 0; L1[0,4,17]  := 0; L1[0,4,18]  := 0; L1[0,4,19]  := 0;  // D
            L1[0,5,0]   := 0; L1[0,5,1]   := 0; L1[0,5,2]   := 0; L1[0,5,3]   := 0; L1[0,5,4]   := 1;
            L1[0,5,5]   := 0; L1[0,5,6]   := 0; L1[0,5,7]   := 0; L1[0,5,8]   := 0; L1[0,5,9]   := 0;
            L1[0,5,10]  := 0; L1[0,5,11]  := 0; L1[0,5,12]  := 0; L1[0,5,13]  := 0; L1[0,5,14]  := 0;
            L1[0,5,15]  := 0; L1[0,5,16]  := 0; L1[0,5,17]  := 0; L1[0,5,18]  := 0; L1[0,5,19]  := 0;  // C
            L1[0,6,0]   := 0; L1[0,6,1]   := 0; L1[0,6,2]   := 0; L1[0,6,3]   := 0; L1[0,6,4]   := 0;
            L1[0,6,5]   := 1; L1[0,6,6]   := 0; L1[0,6,7]   := 0; L1[0,6,8]   := 0; L1[0,6,9]   := 0;
            L1[0,6,10]  := 0; L1[0,6,11]  := 0; L1[0,6,12]  := 0; L1[0,6,13]  := 0; L1[0,6,14]  := 0;
            L1[0,6,15]  := 0; L1[0,6,16]  := 0; L1[0,6,17]  := 0; L1[0,6,18]  := 0; L1[0,6,19]  := 0;  // Q
            L1[0,7,0]   := 0; L1[0,7,1]   := 0; L1[0,7,2]   := 0; L1[0,7,3]   := 0; L1[0,7,4]   := 0;
            L1[0,7,5]   := 0; L1[0,7,6]   := 1; L1[0,7,7]   := 0; L1[0,7,8]   := 0; L1[0,7,9]   := 0;
            L1[0,7,10]  := 0; L1[0,7,11]  := 0; L1[0,7,12]  := 0; L1[0,7,13]  := 0; L1[0,7,14]  := 0;
            L1[0,7,15]  := 0; L1[0,7,16]  := 0; L1[0,7,17]  := 0; L1[0,7,18]  := 0; L1[0,7,19]  := 0;  // E
            L1[0,8,0]   := 0; L1[0,8,1]   := 0; L1[0,8,2]   := 0; L1[0,8,3]   := 0; L1[0,8,4]   := 0;
            L1[0,8,5]   := 0; L1[0,8,6]   := 0; L1[0,8,7]   := 1; L1[0,8,8]   := 0; L1[0,8,9]   := 0;
            L1[0,8,10]  := 0; L1[0,8,11]  := 0; L1[0,8,12]  := 0; L1[0,8,13]  := 0; L1[0,8,14]  := 0;
            L1[0,8,15]  := 0; L1[0,8,16]  := 0; L1[0,8,17]  := 0; L1[0,8,18]  := 0; L1[0,8,19]  := 0;  // G
            L1[0,9,0]   := 0; L1[0,9,1]   := 0; L1[0,9,2]   := 0; L1[0,9,3]   := 0; L1[0,9,4]   := 0;
            L1[0,9,5]   := 0; L1[0,9,6]   := 0; L1[0,9,7]   := 0; L1[0,9,8]   := 1; L1[0,9,9]   := 0;
            L1[0,9,10]  := 0; L1[0,9,11]  := 0; L1[0,9,12]  := 0; L1[0,9,13]  := 0; L1[0,9,14]  := 0;
            L1[0,9,15]  := 0; L1[0,9,16]  := 0; L1[0,9,17]  := 0; L1[0,9,18]  := 0; L1[0,9,19]  := 0;  // H
            L1[0,10,0]  := 0; L1[0,10,1]  := 0; L1[0,10,2]  := 0; L1[0,10,3]  := 0; L1[0,10,4]  := 0;
            L1[0,10,5]  := 0; L1[0,10,6]  := 0; L1[0,10,7]  := 0; L1[0,10,8]  := 0; L1[0,10,9]  := 1;
            L1[0,10,10] := 0; L1[0,10,11] := 0; L1[0,10,12] := 0; L1[0,10,13] := 0; L1[0,10,14] := 0;
            L1[0,10,15] := 0; L1[0,10,16] := 0; L1[0,10,17] := 0; L1[0,10,18] := 0; L1[0,10,19] := 0;  // I
            L1[0,11,0]  := 0; L1[0,11,1]  := 0; L1[0,11,2]  := 0; L1[0,11,3]  := 0; L1[0,11,4]  := 0;
            L1[0,11,5]  := 0; L1[0,11,6]  := 0; L1[0,11,7]  := 0; L1[0,11,8]  := 0; L1[0,11,9]  := 0;
            L1[0,11,10] := 1; L1[0,11,11] := 0; L1[0,11,12] := 0; L1[0,11,13] := 0; L1[0,11,14] := 0;
            L1[0,11,15] := 0; L1[0,11,16] := 0; L1[0,11,17] := 0; L1[0,11,18] := 0; L1[0,11,19] := 0;  // L
            L1[0,12,0]  := 0; L1[0,12,1]  := 0; L1[0,12,2]  := 0; L1[0,12,3]  := 0; L1[0,12,4]  := 0;
            L1[0,12,5]  := 0; L1[0,12,6]  := 0; L1[0,12,7]  := 0; L1[0,12,8]  := 0; L1[0,12,9]  := 0;
            L1[0,12,10] := 0; L1[0,12,11] := 1; L1[0,12,12] := 0; L1[0,12,13] := 0; L1[0,12,14] := 0;
            L1[0,12,15] := 0; L1[0,12,16] := 0; L1[0,12,17] := 0; L1[0,12,18] := 0; L1[0,12,19] := 0;  // K
            L1[0,13,0]  := 0; L1[0,13,1]  := 0; L1[0,13,2]  := 0; L1[0,13,3]  := 0; L1[0,13,4]  := 0;
            L1[0,13,5]  := 0; L1[0,13,6]  := 0; L1[0,13,7]  := 0; L1[0,13,8]  := 0; L1[0,13,9]  := 0;
            L1[0,13,10] := 0; L1[0,13,11] := 0; L1[0,13,12] := 1; L1[0,13,13] := 0; L1[0,13,14] := 0;
            L1[0,13,15] := 0; L1[0,13,16] := 0; L1[0,13,17] := 0; L1[0,13,18] := 0; L1[0,13,19] := 0;  // M
            L1[0,14,0]  := 0; L1[0,14,1]  := 0; L1[0,14,2]  := 0; L1[0,14,3]  := 0; L1[0,14,4]  := 0;
            L1[0,14,5]  := 0; L1[0,14,6]  := 0; L1[0,14,7]  := 0; L1[0,14,8]  := 0; L1[0,14,9]  := 0;
            L1[0,14,10] := 0; L1[0,14,11] := 0; L1[0,14,12] := 0; L1[0,14,13] := 1; L1[0,14,14] := 0;
            L1[0,14,15] := 0; L1[0,14,16] := 0; L1[0,14,17] := 0; L1[0,14,18] := 0; L1[0,14,19] := 0;  // F
            L1[0,15,0]  := 0; L1[0,15,1]  := 0; L1[0,15,2]  := 0; L1[0,15,3]  := 0; L1[0,15,4]  := 0;
            L1[0,15,5]  := 0; L1[0,15,6]  := 0; L1[0,15,7]  := 0; L1[0,15,8]  := 0; L1[0,15,9]  := 0;
            L1[0,15,10] := 0; L1[0,15,11] := 0; L1[0,15,12] := 0; L1[0,15,13] := 0; L1[0,15,14] := 1;
            L1[0,15,15] := 0; L1[0,15,16] := 0; L1[0,15,17] := 0; L1[0,15,18] := 0; L1[0,15,19] := 0;  // P
            L1[0,16,0]  := 0; L1[0,16,1]  := 0; L1[0,16,2]  := 0; L1[0,16,3]  := 0; L1[0,16,4]  := 0;
            L1[0,16,5]  := 0; L1[0,16,6]  := 0; L1[0,16,7]  := 0; L1[0,16,8]  := 0; L1[0,16,9]  := 0;
            L1[0,16,10] := 0; L1[0,16,11] := 0; L1[0,16,12] := 0; L1[0,16,13] := 0; L1[0,16,14] := 0;
            L1[0,16,15] := 1; L1[0,16,16] := 0; L1[0,16,17] := 0; L1[0,16,18] := 0; L1[0,16,19] := 0;  // S
            L1[0,17,0]  := 0; L1[0,17,1]  := 0; L1[0,17,2]  := 0; L1[0,17,3]  := 0; L1[0,17,4]  := 0;
            L1[0,17,5]  := 0; L1[0,17,6]  := 0; L1[0,17,7]  := 0; L1[0,17,8]  := 0; L1[0,17,9]  := 0;
            L1[0,17,10] := 0; L1[0,17,11] := 0; L1[0,17,12] := 0; L1[0,17,13] := 0; L1[0,17,14] := 0;
            L1[0,17,15] := 0; L1[0,17,16] := 1; L1[0,17,17] := 0; L1[0,17,18] := 0; L1[0,17,19] := 0;  // T
            L1[0,18,0]  := 0; L1[0,18,1]  := 0; L1[0,18,2]  := 0; L1[0,18,3]  := 0; L1[0,18,4]  := 0;
            L1[0,18,5]  := 0; L1[0,18,6]  := 0; L1[0,18,7]  := 0; L1[0,18,8]  := 0; L1[0,18,9]  := 0;
            L1[0,18,10] := 0; L1[0,18,11] := 0; L1[0,18,12] := 0; L1[0,18,13] := 0; L1[0,18,14] := 0;
            L1[0,18,15] := 0; L1[0,18,16] := 0; L1[0,18,17] := 1; L1[0,18,18] := 0; L1[0,18,19] := 0;  // W
            L1[0,19,0]  := 0; L1[0,19,1]  := 0; L1[0,19,2]  := 0; L1[0,19,3]  := 0; L1[0,19,4]  := 0;
            L1[0,19,5]  := 0; L1[0,19,6]  := 0; L1[0,19,7]  := 0; L1[0,19,8]  := 0; L1[0,19,9]  := 0;
            L1[0,19,10] := 0; L1[0,19,11] := 0; L1[0,19,12] := 0; L1[0,19,13] := 0; L1[0,19,14] := 0;
            L1[0,19,15] := 0; L1[0,19,16] := 0; L1[0,19,17] := 0; L1[0,19,18] := 1; L1[0,19,19] := 0;  // Y
            L1[0,20,0]  := 0; L1[0,20,1]  := 0; L1[0,20,2]  := 0; L1[0,20,3]  := 0; L1[0,20,4]  := 0;
            L1[0,20,5]  := 0; L1[0,20,6]  := 0; L1[0,20,7]  := 0; L1[0,20,8]  := 0; L1[0,20,9]  := 0;
            L1[0,20,10] := 0; L1[0,20,11] := 0; L1[0,20,12] := 0; L1[0,20,13] := 0; L1[0,20,14] := 0;
            L1[0,20,15] := 0; L1[0,20,16] := 0; L1[0,20,17] := 0; L1[0,20,18] := 0; L1[0,20,19] := 1;  // V
            L1[0,21,0]  := 1; L1[0,21,1]  := 1; L1[0,21,2]  := 1; L1[0,21,3]  := 1; L1[0,21,4]  := 1;
            L1[0,21,5]  := 1; L1[0,21,6]  := 1; L1[0,21,7]  := 1; L1[0,21,8]  := 1; L1[0,21,9]  := 1;
            L1[0,21,10] := 1; L1[0,21,11] := 1; L1[0,21,12] := 1; L1[0,21,13] := 1; L1[0,21,14] := 1;
            L1[0,21,15] := 1; L1[0,21,16] := 1; L1[0,21,17] := 1; L1[0,21,18] := 1; L1[0,21,19] := 1;   // -
            L1[0,22,0]  := 1; L1[0,22,1]  := 1; L1[0,22,2]  := 1; L1[0,22,3]  := 1; L1[0,22,4]  := 1;
            L1[0,22,5]  := 1; L1[0,22,6]  := 1; L1[0,22,7]  := 1; L1[0,22,8]  := 1; L1[0,22,9]  := 1;
            L1[0,22,10] := 1; L1[0,22,11] := 1; L1[0,22,12] := 1; L1[0,22,13] := 1; L1[0,22,14] := 1;
            L1[0,22,15] := 1; L1[0,22,16] := 1; L1[0,22,17] := 1; L1[0,22,18] := 1; L1[0,22,19] := 1;  // X
          end;
      end;
    end;
  SubtaskProgress(100, 'Initializing site configuration');
  {$IFDEF DEBUG}WriteToDevLog('end TMLTree.InitOTUConfig');{$ENDIF}
end;

////////////////////

type
  TInitSiteConfigThread = class;

{ TInitSiteConfigChildThread }

TInitSiteConfigChildThread = class(TThread)
private
  FErrorMsg: String;
  FIsSuccess: Boolean;
  ThreadIndex: integer;
  MainThread: TInitSiteConfigThread;
  MLTree: TMLTree;
  Mode: integer;
  Done: boolean;
protected
   procedure Execute; override;
public
   constructor Create(parent: TInitSiteConfigThread; index: integer; tree: TMLTree; AMode:integer);

   destructor Destroy; override;
   property IsSuccess: Boolean read FIsSuccess;
   property ErrorMsg: String read FErrorMsg;
end;

TInitSiteConfigThread = class(TInitSiteConfigChildThread)
   LockThread: TCriticalSection;
private
   FNoOfThreads: integer;
   ChildThread: array of TInitSiteConfigChildThread;
   NodeStarted: array of boolean;

   ChildStart: boolean;

   function GetTreeNode: TMLTreeNode;
   function CheckAllNodeDone: boolean;
   function CheckChildThreadDone: boolean;
protected
   procedure Execute; override;
public
   property NoOfThreads: integer read FNoOfThreads write FNoOfThreads;

   constructor Create(tree: TMLTree);
   destructor Destroy; override;
end;

constructor TInitSiteConfigChildThread.Create(parent: TInitSiteConfigThread; index: integer; tree: TMLTree; AMode:integer);
begin
   inherited Create(true);
   Priority := tpNormal;
   MainThread  := parent;
   ThreadIndex := index;
   MLTree      := tree;
   Mode        := AMode;
   Done        := false;
   FreeOnTerminate := true;
   FIsSuccess := True;
   FErrorMsg := EmptyStr;
end;

destructor TInitSiteConfigChildThread.Destroy;
begin
   inherited;
end;


procedure TInitSiteConfigChildThread.Execute;
var
  node: TMLTreeNode;
begin
  try
    repeat
      node := MainThread.GetTreeNode;
      if node <> nil then
      begin
        if Mode = 1 then
          MLTree.ResetSiteConfig1OfNodeFast(node, false)
        else if Mode = 0 then
          MLTree.ResetSiteConfig0OfNodeFast(node);
        node.flag := true;
      end
      else if MainThread.CheckAllNodeDone then
        while Done <> true do
          Done := true;
      while (not terminated) and Done do
        ThreadSwitch;
    until Terminated;
  except
    on E:Exception do
    begin
      FIsSuccess := False;
      FErrorMsg := E.Message;
      Terminate;
      Done := True;
    end;
  end;
end;

constructor TInitSiteConfigThread.Create(tree: TMLTree);
begin
   inherited Create(self, 0, tree, 1);

   LockThread := TCriticalSection.Create;

   FreeOnTerminate := false;
   ChildStart := false;

   MLTree := tree;
   NoOfThreads := MLTree.NoOfThreads;

   setlength(ChildThread, NoOfThreads);
   setlength(NodeStarted, 2*MLTree.NoOfSeqs);
end;

destructor TInitSiteConfigThread.Destroy;
begin
   FreeAndNil(LockThread);

   setlength(ChildThread, 0);
   setlength(NodeStarted, 0);

   inherited;
end;

function TInitSiteConfigThread.CheckAllNodeDone: boolean;
var
   i: integer;
begin
  result := false;
  if not ChildStart then
    exit;
  result := true;
  with  MLTree do
    for i := NoOfSeqs to 2*NoOfSeqs-2 do
      result := result and Node[i].flag;
end;

function TInitSiteConfigThread.CheckChildThreadDone: boolean;
var
   i: integer;
begin
   result := true;
   for i := 1 to NoOfThreads-1 do
     result := result and ChildThread[i].Done;
end;

function TInitSiteConfigThread.GetTreeNode: TMLTreeNode;
var
   i: integer;
   flag: boolean;
begin
   result := nil;
   if not ChildStart then
     exit;
   try
     LockThread.Acquire;

     result := nil;
     with MLTree do
       for i :=  NoOfSeqs to 2*NoOfSeqs-2 do
         if not NodeStarted[i] then
         begin
           flag := false;
           if Mode = 1 then
           begin
             if Node[i].des1.flag and Node[i].des2.flag then
               flag := true;
           end
           else if Mode = 0 then
             if Node[i].anc = nil then
               flag := true
             else if Node[i].anc.flag then
               flag := true;
           if flag then
           begin
             result := MLTree.Node[i];
             NodeStarted[i] := true;
             break;
           end;
         end;

   finally
     LockThread.Release;
   end;
end;

procedure TInitSiteConfigThread.Execute;
var
  node: TMLTreeNode;
  i: integer;
  flag: boolean;
begin
  try
    ChildStart := false;

    for i := MLTree.NoOfSeqs to 2*MLTree.NoOfSeqs-2 do
      NodeStarted[i] := false;
    for i := 0 to MLTree.NoOfSeqs-1 do
      MLTree.Node[i].flag := true;
    for i := MLTree.NoOfSeqs to 2*MLTree.NoOfSeqs-2 do
      MLTree.Node[i].flag := false;

    self.Mode := 1;
    ChildThread[0] := self;

    for i := 1 to MLTree.NoOfThreads-1 do
    begin
      ChildThread[i] := TInitSiteConfigChildThread.Create(self, i, MLTree, 1);
      ChildThread[i].start;
    end;

    ChildStart := true;

    repeat
      node := GetTreeNode;
      if node <> nil then
      begin
        if Mode = 1 then
          MLTree.ResetSiteConfig1OfNodeFast(node, false)
        else if Mode = 0 then
          MLTree.ResetSiteConfig0OfNodeFast(node);
        node.flag := true;
      end
      else if CheckAllNodeDone then
        while Done <> true do
          Done := true;
      ThreadSwitch;
    until Done;

    while not CheckChildThreadDone do
      ThreadSwitch;

    ChildStart := false;

    for i := MLTree.NoOfSeqs to 2*MLTree.NoOfSeqs-2 do
      NodeStarted[i] := false;
    for i := MLTree.NoOfSeqs to 2*MLTree.NoOfSeqs-2 do
      MLTree.Node[i].flag := false;
    for i := 0 to MLTree.NoOfThreads-1 do
    begin
      ChildThread[i].Done := false;
      ChildThread[i].mode := 0;
    end;

    ChildStart := true;

    repeat
      node := GetTreeNode;
      if node <> nil then
      begin
        if Mode = 1 then
          MLTree.ResetSiteConfig1OfNodeFast(node, false)
        else if Mode = 0 then
          MLTree.ResetSiteConfig0OfNodeFast(node);
        node.flag := true;
      end
      else if CheckAllNodeDone then
        while Done <> true do
          Done := true;
      ThreadSwitch;
    until Done;

    while not CheckChildThreadDone do
      ThreadSwitch;

    ChildStart := false;
  finally
    for i := 1 to MLTree.NoOfThreads-1 do
      if assigned(ChildThread[i]) then
        ChildThread[i].Terminate;
    Terminate;
    if FIsSuccess then
    begin
      for i := 1 to MLTree.NoOfThreads-1 do
        if assigned(ChildThread[i]) and (not ChildThread[i].IsSuccess) then
        begin
          FIsSuccess := False;
          FErrorMsg := ChildThread[i].ErrorMsg;
          break;
        end;
    end;
  end;
  if Terminated then
    ReturnValue := 1
  else
    ReturnValue := 0;
end;


////////////////////
procedure TMLTree.ResetSiteConfig;
var
  nodesProcessed: Integer;

    procedure InitSiteConfigOfNode(node: TMLTreeNode);
    var
      i,k: integer;
    begin
      if node.OTU or node.flag then exit;

      InitSiteConfigOfNode(node.des1);
      InitSiteConfigOfNode(node.des2);

      with node do
        for k := 0 to FNoOfSites do
        begin
          c0[k] := 0;
          c1[k] := 0;
        end;

      ResetSiteConfig1OfNodeFast(node, false);
      inc(nodesProcessed);
      i := Round(nodesProcessed/(NoOfSeqs - 1)*50);
      if i mod 10 = 0 then                                     // To relax flickering
        SubtaskProgress(i, 'Iterating site configuration');
    end;

    procedure InitSiteConfigOfNodeFromAnc(node: TMLTreeNode);
    var
      i: integer;
    begin
      ResetSiteConfig0OfNodeFast(node);
      inc(nodesProcessed);
      i := Round(nodesProcessed/(NoOfSeqs - 1)*50)+50;
      if i mod 10 = 0 then                                     // To relax flickering
        SubtaskProgress(i, 'Iterating site configuration');

      if not (node.des1.OTU or node.des1.flag) then
        InitSiteConfigOfNodeFromAnc(node.des1);
      if not (node.des2.OTU or node.des2.flag) then
        InitSiteConfigOfNodeFromAnc(node.des2);
    end;

    procedure InitSiteConfigMultiThread;
    var
      InitSiteConfigThread: TInitSiteConfigThread;
    begin
      InitSiteConfigThread := nil;
      try
        InitSiteConfigThread := TInitSiteConfigThread.Create(Self);
        InitSiteConfigThread.Start;
        InitSiteConfigThread.WaitFor;
        if not InitSiteConfigThread.IsSuccess then
          raise Exception.Create('Error in TMLTree.ResetSiteConfig: ' + InitSiteConfigThread.ErrorMsg);
      finally
        if InitSiteConfigThread <> nil then
          InitSiteConfigThread.Destroy;
      end;
    end;

  var
    i,k: integer;
  begin
//    if not FIsDoingTreeSearch then { then we are just computing branch lengths on a fixed tree so no need to keep initializing this}
//      Exit;
    {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.InitSiteConfig');{$ENDIF}

{
    for i := NoOfSeqs to 2*NoOfSeqs-2 do
      with node[i] do
        for k := 0 to FNoOfSites do
        begin
          c0[k] := 0;
          c1[k] := 0;
        end;
}
    NodesProcessed := 0;


  //  {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.InitSiteConfigOfNode');{$ENDIF}
  //  InitSiteConfigOfNode(root);
  //  {$IFDEF DEBUG}WriteToDevLog('end TMLTree.InitSiteConfigOfNode');{$ENDIF}
  //  {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.InitSiteConfigOfNodeFromAnc');{$ENDIF}
  //  InitSiteConfigOfNodeFromAnc(root);
  //  {$IFDEF DEBUG}WriteToDevLog('end TMLTree.InitSiteConfigOfNodeFromAnc');{$ENDIF}

    InitSiteConfigMultiThread;

    setlength(nc, NoOfConfs+1);
    setlength(bc, NoOfConfs+1);
    for k := 0 to NoOfConfs do
    begin
      nc[k] := 0;
      bc[k] := 0;
    end;
    for k := 1 to FNoOfSites do
      inc(nc[Root.c1[k]]);
    if Model.BootTable <> nil then
      for k := 1 to FNoOfSites do
        inc(bc[Root.c1[k]], Model.BootTable[k]);

    SubtaskProgress(100, 'Iterating site configuration');
    {$IFDEF DEBUG}{$IFNDEF VISUAL_BUILD}
    WriteToDevLog('end TMLTree.InitSiteConfig');
    //DumpSiteConfigData(NextAvailableFilenameNV('_site-configs-updated.csv'));
    {$ENDIF}{$ENDIF}
end;

procedure TMLTree.InitSiteConfig;
var
  i,k: integer;
begin
//  if not FIsDoingTreeSearch then { then we are just computing branch lengths on a fixed tree so no need to keep initializing this}
//    Exit;
  {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.InitSiteConfig');{$ENDIF}
  {$IFDEF DEBUG}SaveMLlog(' MLTree.InitSiteConfig started');{$ENDIF}

  for i := NoOfSeqs to 2*NoOfSeqs-2 do
    node[i].Flag := false;

  for i := NoOfSeqs to 2*NoOfSeqs-2 do
    with node[i] do
      for k := 0 to FNoOfSites do
      begin
        c0[k] := 0;
        c1[k] := 0;
      end;

//  NodesProcessed := 0;

//  {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.InitSiteConfigOfNode');{$ENDIF}
//  InitSiteConfigOfNode(root);
//  {$IFDEF DEBUG}WriteToDevLog('end TMLTree.InitSiteConfigOfNode');{$ENDIF}
//  {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.InitSiteConfigOfNodeFromAnc');{$ENDIF}
//  InitSiteConfigOfNodeFromAnc(root);
//  {$IFDEF DEBUG}WriteToDevLog('end TMLTree.InitSiteConfigOfNodeFromAnc');{$ENDIF}

  ResetSiteConfig;

  {$IFDEF DEBUG}SaveMLlog(' MLTree.InitSiteConfig ended');{$ENDIF}
end;

procedure TMLTree.ResetSiteConfig1OfNode(ANode: TMLTreeNode; AllocL1: boolean);
var
  cs: array of integer;
  i,j,n,m1,m2,j0: integer;
  check: boolean;
begin
  raise Exception.Create('call to deprecated ResetSiteConfig1OfNode procedure. Use ResetSiteConfig1OfNodeFast instead');
  if ANode.OTU then
    exit;
  with ANode do
  begin
    if Model.NoOfStates = 4 then
      setlength(cs, max(FNoOfSites, 16)+1)
    else if Model.NoOfStates = 20 then
      setlength(cs, max(FNoOfSites, 22)+1);

    n := 0;
    m1 := 0;
    m2 := 0;
    for i := 1 to FNoOfSites do
    begin
      check := true;
      if (des1.c1[i] <= m1) and (des2.c1[i] <= m2) then
      begin
        j0 := c1[max(des1.cs1[des1.c1[i]], des2.cs1[des2.c1[i]])];
        if j0 > 0 then
          for j := j0 to n do
            if (des1.c1[cs[j]] = des1.c1[i]) and
               (des2.c1[cs[j]] = des2.c1[i]) then
            begin
              c1[i] := c1[cs[j]];
              check := false;
              break;
            end;
      end;
      if check then
      begin
        inc(n);
        c1[i] := n;
        cs[n] := i;
        m1 := max(m1, des1.c1[i]);
        m2 := max(m2, des2.c1[i]);
      end;
    end;

    if nc1 > 0 then
      DeallocNodeMem1;
    if n <> nc1 then
      FreeMemAndNil(cs1);

    nc1 := n;
    if (nc1 > 0) and AllocL1 then
      AllocNodeMem1;
    if nc1 > 0 then
    begin
      if cs1 <> nil then
        FreeMemAndNil(cs1); { to avoid memory leak}
      cs1 := AllocMem(SizeOf(integer)*(nc1+1));
      for i := 1 to nc1 do
        cs1[i] := cs[i];
    end;
    setlength(cs, 0);
  end;
end;

procedure TMLTree.ResetSiteConfig1OfNodeFast(ANode: TMLTreeNode;AllocL1: boolean);
var
  configSiteIndices: array of integer;
  i,n: integer;
  fastLookup: TFPHashList = nil;
  config: TStringCount = nil;
  configList: TList = nil;
  aPatternStr: String = '';
  lookupIndex: Integer = - 1;
begin
  if ANode.OTU then
    exit;
  CheckAbort;
  try
    fastLookup := TFPHashList.Create;
    configList := TList.Create;
    with ANode do
    begin
      if Model.NoOfStates = 4 then
        setlength(configSiteIndices, max(FNoOfSites, 16)+1)
      else if Model.NoOfStates = 20 then
        setlength(configSiteIndices, max(FNoOfSites, 22)+1);

      n := 0;
      for i := 1 to FNoOfSites do
      begin
        aPatternStr := Format('%d.%d',[des1.c1[i], des2.c1[i]]);
        lookupIndex := fastLookup.FindIndexOf(aPatternStr);
        if lookupIndex < 0 then
        begin
          config := TStringCount.Create(aPatternStr, n + 1);
          configList.Add(config);
          fastLookup.Add(aPatternStr, config);
          inc(n);
          c1[i] := n;
          configSiteIndices[n] := i;
        end
        else
        begin
          config := TStringCount(fastLookup[lookupIndex]);
          c1[i] := c1[configSiteIndices[config.Index]];
        end;
      end;

      if nc1 > 0 then
        DeallocNodeMem1;
      if n <> nc1 then
        FreeMemAndNil(cs1);

      nc1 := n;
      if (nc1 > 0) and AllocL1 then
        AllocNodeMem1;
      if nc1 > 0 then
      begin
        if cs1 <> nil then
          FreeMemAndNil(cs1); { to avoid memory leak}
        cs1 := AllocMem(SizeOf(integer)*(nc1+1));
        for i := 1 to nc1 do
          cs1[i] := configSiteIndices[i];
      end;
      setlength(configSiteIndices, 0);
    end;

  finally
    if Assigned(fastLookup) then
      fastLookup.Free;
    if Assigned(configList) then
    begin
      if configList.Count > 0 then
        for i := 0 to configList.Count - 1 do
        begin
          config := TStringCount(configList[i]);
          if Assigned(config) then
            config.Free;
        end;
      configList.Free;
    end;
  end;
  CheckAbort;
end;

procedure TMLTree.ResetSiteConfig0OfNode(ANode: TMLTreeNode);
var
  cs: array of integer;
  i,j,k,n,m1,m2,j0: integer;
  check: boolean;
begin
  raise Exception.Create('call to deprecated ResetSiteConfig0OfNode procedure. Use ResetSiteConfig0OfNodeFast instead.');
  if ANode.OTU then
    exit;
  if Model.NoOfStates = 4 then
    setlength(cs, max(FNoOfSites, 16)+1)
  else if Model.NoOfStates = 20 then
    setlength(cs, max(FNoOfSites, 22)+1);
  with ANode do
  begin
    if anc = nil then
    begin
      n := nc1;
      for i := 1 to n do
        cs[i] := cs1[i];
      for i := 1 to FNoOfSites do
        c0[i] := c1[i];
    end
    else
      if anc.anc = nil then
        if ANode = anc.des1 then
        begin
          n := anc.des2.nc1;
          for i := 1 to n do
            cs[i] := anc.des2.cs1[i];
          for i := 1 to FNoOfSites do
            c0[i] := anc.des2.c1[i];
        end
        else
        begin
          n := anc.des1.nc1;
          for i := 1 to n do
            cs[i] := anc.des1.cs1[i];
          for i := 1 to FNoOfSites do
            c0[i] := anc.des1.c1[i];
        end
      else if ANode = anc.des1 then
      begin
        m1 := 0;
        m2 := 0;
        n := 0;
        for i := 1 to FNoOfSites do
        begin
          check := true;
          if (anc.c0[i] <= m1) and (anc.des2.c1[i] <= m2) then
          begin
            j0 := c0[max(anc.cs0[anc.c0[i]], anc.des2.cs1[anc.des2.c1[i]])];
            if j0 > 0 then
              for j := j0 to n do
                if (anc.c0[i] = anc.c0[cs[j]]) and
                   (anc.des2.c1[i] = anc.des2.c1[cs[j]]) then
                begin
                  c0[i] := c0[cs[j]];
                  check := false;
                  break;
                end;
            end;
            if check then
            begin
              inc(n);
              c0[i] := n;
              cs[n] := i;
              m1 := max(m1, anc.c0[i]);
              m2 := max(m2, anc.des2.c1[i]);
            end;
          end;
      end
      else // if node[index] = node[index].anc.des2 then
      begin
        m1 := 0;
        m2 := 0;
        n := 0;
        for i := 1 to FNoOfSites do
        begin
          check := true;
          if (anc.c0[i] <= m1) and (anc.des1.c1[i] <= m2) then
          begin
            j0 := c0[max(anc.cs0[anc.c0[i]], anc.des1.cs1[anc.des1.c1[i]])];
            if j0 > 0 then
            for j := j0 to n do
              if (anc.c0[i] = anc.c0[cs[j]]) and
                 (anc.des1.c1[i] = anc.des1.c1[cs[j]]) then
              begin
                c0[i] := c0[cs[j]];
                check := false;
                break;
              end;
          end;
          if check then
          begin
            inc(n);
            c0[i] := n;
            cs[n] := i;
            m1 := max(m1, anc.c0[i]);
            m2 := max(m2, anc.des1.c1[i]);
          end;
        end;
      end;
//    node.SetNodeNum0(n);

    DeallocNodeMem0;
    if n <> nc0 then
      FreeMemAndNil(cs0);
    if n > 0 then
    begin
      if cs0 <> nil then
        FreeMemAndNil(cs0);
      cs0 := AllocMem(SizeOf(integer)*(n+1));
      for i := 1 to n do
        cs0[i] := cs[i];
    end;
    setlength(cs, 0);

    nc0 := n;
    if (anc = nil) and (L0[0] = nil) then
    begin
      AllocNodeMem0;
      for i := 0 to length(L0)-1 do
        for k := 1 to nc0 do
          for j := 0 to NoOfStates-1 do
            L0[i,k,j] := 1;
    end;

    if anc = nil then
      for i := 1 to FNoOfSites do
      begin
        g1[i] := des1.g0[i];
        g2[i] := des2.g0[i];
      end
    else if ANode = anc.des1 then
      for i := 1 to FNoOfSites do
      begin
        g1[i] := des1.g0[i] and anc.g2[i];
        if g1[i] = 0 then
          g1[i] := des1.g0[i] or anc.g2[i];
        g2[i] := des2.g0[i] and anc.g2[i];
        if g2[i] = 0 then
          g2[i] := des2.g0[i] or anc.g2[i];
      end
    else  // if node = node.anc.des2 then
      for i := 1 to FNoOfSites do
      begin
        g1[i] := des1.g0[i] and anc.g1[i];
        if g1[i] = 0 then
          g1[i] := des1.g0[i] or anc.g1[i];
        g2[i] := des2.g0[i] and anc.g1[i];
        if g2[i] = 0 then
          g2[i] := des2.g0[i] or anc.g1[i];
      end;
  end;
end;

procedure TMLTree.ResetSiteConfig0OfNodeFast(ANode: TMLTreeNode);
var
  configSiteIndices: array of integer;
  i,j,k,n: integer;
  fastLookup: TFPHashList = nil;
  config: TStringCount = nil;
  configList: TList = nil;
  aPatternStr: String = '';
  lookupIndex: Integer = -1;
begin
  if ANode.OTU then
    exit;
  CheckAbort;
  if Model.NoOfStates = 4 then
    setlength(configSiteIndices, max(FNoOfSites, 16)+1)
  else if Model.NoOfStates = 20 then
    setlength(configSiteIndices, max(FNoOfSites, 22)+1);

  try
    fastLookup := TFPHashList.Create;
    configList := TList.Create;
    n := 0;
    with ANode do
    begin
      if anc = nil then
      begin
        n := nc1;
        for i := 1 to n do
          configSiteIndices[i] := cs1[i];
        for i := 1 to FNoOfSites do
          c0[i] := c1[i]; {c0 assigned}
      end
      else
        if anc.anc = nil then { a direct des of root}
          if ANode = anc.des1 then
          begin
            n := anc.des2.nc1;
            for i := 1 to n do
              configSiteIndices[i] := anc.des2.cs1[i];
            for i := 1 to FNoOfSites do
              c0[i] := anc.des2.c1[i]; {c0 assigned}
          end
          else
          begin
            n := anc.des1.nc1;
            for i := 1 to n do
              configSiteIndices[i] := anc.des1.cs1[i];
            for i := 1 to FNoOfSites do
              c0[i] := anc.des1.c1[i]; {c0 assigned}
          end
        else if ANode = anc.des1 then
        begin
          for i := 1 to FNoOfSites do
          begin
            aPatternStr := Format('des1.%d.%d',[anc.c0[i], anc.des2.c1[i]]);
            lookupIndex := fastLookup.FindIndexOf(aPatternStr);
            if lookupIndex < 0 then
            begin
              config := TStringCount.Create(aPatternStr, n + 1);
              configList.Add(config);
              fastLookup.Add(aPatternStr, config);
              inc(n);
              c0[i] := n;
              configSiteIndices[n] := i;
            end
            else
            begin
              config := TStringCount(fastLookup[lookupIndex]);
              c0[i] := c0[configSiteIndices[config.Index]];
            end;
          end;
        end
        else
        begin
          for i := 1 to FNoOfSites do
          begin
            aPatternStr := Format('des2.%d.%d',[anc.c0[i], anc.des1.c1[i]]);
            lookupIndex := fastLookup.FindIndexOf(aPatternStr);
            if lookupIndex < 0 then
            begin
              config := TStringCount.Create(aPatternStr, n + 1);
              configList.Add(config);
              fastLookup.Add(aPatternStr, config);
              inc(n);
              c0[i] := n;
              configSiteIndices[n] := i;
            end
            else
            begin
              config := TStringCount(fastLookup[lookupIndex]);
              c0[i] := c0[configSiteIndices[config.Index]];
            end;
          end;
        end;

      DeallocNodeMem0;
      if n <> nc0 then
        FreeMemAndNil(cs0);
      if n > 0 then
      begin
        if cs0 <> nil then
          FreeMemAndNil(cs0);
        cs0 := AllocMem(SizeOf(integer)*(n+1));
        for i := 1 to n do
          cs0[i] := configSiteIndices[i];
      end;
      setlength(configSiteIndices, 0);

      nc0 := n;
      if (anc = nil) and (L0[0] = nil) then
      begin
        AllocNodeMem0;
        for i := 0 to length(L0)-1 do
          for k := 1 to nc0 do
            for j := 0 to NoOfStates-1 do
              L0[i,k,j] := 1;
      end;

      ResetGOfNode(ANode);
    end;
  finally
    if Assigned(fastLookup) then
      fastLookup.Free;
    if Assigned(configList) then
    begin
      if configList.Count > 0 then
        for i := 0 to configList.Count - 1 do
        begin
          config := TStringCount(configList[i]);
          if Assigned(config) then
            config.Free;
        end;
      configList.Free;
    end;
  end;
  CheckAbort;
end;

procedure TMLTree.ResetGOfNode(aNode: TMLTreeNode);
var
  i: Integer = -1;
begin
  CheckAbort;
  with aNode do
  begin
    if anc = nil then
      for i := 1 to FNoOfSites do
      begin
        g1[i] := des1.g0[i];
        g2[i] := des2.g0[i];
      end
    else if aNode = anc.des1 then
      for i := 1 to FNoOfSites do
      begin
        g1[i] := des1.g0[i] and anc.g2[i];
        if g1[i] = 0 then
          g1[i] := des1.g0[i] or anc.g2[i];
        g2[i] := des2.g0[i] and anc.g2[i];
        if g2[i] = 0 then
          g2[i] := des2.g0[i] or anc.g2[i];
      end
    else
      for i := 1 to FNoOfSites do
      begin
        g1[i] := des1.g0[i] and anc.g1[i];
        if g1[i] = 0 then
          g1[i] := des1.g0[i] or anc.g1[i];
        g2[i] := des2.g0[i] and anc.g1[i];
        if g2[i] = 0 then
          g2[i] := des2.g0[i] or anc.g1[i];
      end;
  end;
end;

procedure TMLTree.ResetProbOfNode(ANode: TMLTreeNode);
begin
  if ANode.anc <> nil then
    Model.ComputeProbOfNode(ANode, ANode.blen);
end;

procedure TMLTree.ResetProb;
var
  i: integer;
begin
  for i := 0 to 2*NoOfSeqs-2 do
    ResetProbOfNode(node[i]);
end;

function TMLTree.ComputeL0OfNodeRange(ANode: TMLTreeNode; LBound, UBound: integer): boolean;
var
  cat,i,j,k,k0,k1,k2: integer;
  p1,p2,pL0,pL1,pL2: PMatrixOfExtended;
  r1,s: extended;
  r2: array[0..20] of extended;
begin
  //{$IFDEF DEBUG}WriteToDevLog('begin TMLTree.ComputeL0OfNode');{$ENDIF}
  for i := Low(r2) to High(r2) do
    r2[i] := 0; { to get the compiler to stop warning that r2 is not initialized}
  if ANode.OTU then
    exit;
  if (ANode.anc <> nil) and (ANode.anc.L0 = nil) then
    exit;

  result := true;
  with ANode do
  begin
    if ANode.anc = nil then
      for cat := 0 to length(L0)-1 do
      begin
        pL0 := L0[cat];
        for k1 := LBound to UBound do
          for i := 0 to NoOfStates-1 do
            pL0[k1,i] := 1;
      end
    else if ANode = anc.des1 then
      for cat := 0 to length(L0)-1 do
      begin
        pL0 := L0[cat];
        pL1 := anc.L0[cat];
        pL2 := anc.des2.L1[cat];
        p1  := Prob[cat];
        p2  := anc.des2.Prob[cat];
        for k0 := LBound to UBound do
        begin
          k  := cs0[k0];
          if k = 0 then continue;
          k1 := anc.c0[k];
          k2 := anc.des2.c1[k];
          for i := 0 to NoOfStates-1 do
          begin
            r2[i] := 0;
            for j := 0 to NoOfStates-1 do
              r2[i] := r2[i] +pL2[k2,j]*p2[i,j];
          end;
          s := 0;
          for i := 0 to NoOfStates-1 do
          begin
            r1 := 0;
            for j := 0 to NoOfStates-1 do
              r1 := r1 +r2[j]*pL1[k1,j]*p1[i,j];
            pL0[k0,i] := r1;
            s := s +r1;
          end;
          if s < 2.5E-307 then
            result := false;
//
          if NoOfSites = 1 then   // For group analysis
          begin
            r1 := 0;
            for i := 0 to NoOfStates-1 do
              r1 := r1 + pL0[k0,i];
            if r1 < 2.5E-307 then
            begin
              for i := 0 to NoOfStates-1 do
              begin
                r1 := 0;
                for j := 0 to NoOfStates-1 do
                  r1 := r1 +r2[j]*(pL1[k1,j]+p1[i,j]);
                pL0[k0,i] := r1;
              end;
            end;
          end;
//
        end;
      end
    else // if ANode = anc.des2 then
      for cat := 0 to length(L0)-1 do
      begin
        pL0 := L0[cat];
        pL1 := anc.L0[cat];
        pL2 := anc.des1.L1[cat];
        p1  := Prob[cat];
        p2  := anc.des1.Prob[cat];
        for k0 := LBound to UBound do
        begin
          k  := cs0[k0];
          if k = 0 then continue;
          k1 := anc.c0[k];
          k2 := anc.des1.c1[k];
          for i := 0 to NoOfStates-1 do
          begin
            r2[i] := 0;
            for j := 0 to NoOfStates-1 do
              r2[i] := r2[i] +pL2[k2,j]*p2[i,j];
          end;
          s := 0;
          for i := 0 to NoOfStates-1 do
          begin
            r1 := 0;
            for j := 0 to NoOfStates-1 do
              r1 := r1 +r2[j]*pL1[k1,j]*p1[i,j];
            pL0[k0,i] := r1;
            s := s +r1;
          end;
          if s < 2.5E-307 then
            result := false;
//
          if NoOfSites = 1 then   // For group analysis
          begin
            r1 := 0;
            for i := 0 to NoOfStates-1 do
              r1 := r1 + pL0[k0,i];
            if r1 < 2.5E-307 then
            begin
              for i := 0 to NoOfStates-1 do
              begin
                r1 := 0;
                for j := 0 to NoOfStates-1 do
                  r1 := r1 +r2[j]*(pL1[k1,j]+p1[i,j]);
                pL0[k0,i] := r1;
              end;
            end;
          end;
//
        end;
      end;

  end;

{$IFDEF DEBUG}if not result then SaveMLlog(' MLTree.ComputeL0 underflowed on Node '+IntToStr(ANode.index));{$ENDIF}

{$IFDEF DEBUG}WriteToDevLog('end TMLTree.ComputeL0OfNode');{$ENDIF}
end;

function TMLTree.ComputeL1OfNodeRange(ANode: TMLTreeNode; LBound, UBound: integer): boolean;
var
  i,j,k,cat,k0,k1,k2: integer;
  p1,p2,L01,L11,L12: PMatrixOfExtended;
  r1,r2,s: extended;
begin
  result := true;
  with ANode do
  begin
    for cat := 0 to length(L1)-1 do
    begin
      L01 := L1[cat];
      L11 := des1.L1[cat];
      L12 := des2.L1[cat];

      p1 := des1.Prob[cat];
      p2 := des2.Prob[cat];
      for k0 := LBound to UBound do
      begin
        k  := cs1[k0];
        k1 := des1.c1[k];
        k2 := des2.c1[k];
        s := 0;
        for i := 0 to NoOfStates-1 do
        begin
          r1 := 0;
          r2 := 0;
          for j := 0 to NoOfStates-1 do
          begin
            r1 := r1 +L11[k1,j]*p1[i,j];
            r2 := r2 +L12[k2,j]*p2[i,j];
          end;
          if (r1 > 0) and (r2 > 0) and (log10(r1) + log10(r2) < -300) then
            L01[k0,i] := 1.0E-300
          else
            L01[k0,i] := r1*r2;
          s := s + L01[k0,i];
        end;
//
        if (NoOfSites = 1) and (s < 2.5E-307) then   // For group analysis
          for i := 0 to NoOfStates-1 do
          begin
            r1 := 0;
            r2 := 0;
            for j := 0 to NoOfStates-1 do
            begin
              r1 := r1 +L11[k1,j]*p1[i,j];
              r2 := r2 +L12[k2,j]*p2[i,j];
            end;
            L01[k0,i] := r1+r2;
          end;
//
        if s < 2.5E-307 then
          result := false;
      end;

    end;
  end;
  {$IFDEF DEBUG}if not result then SaveMLlog(' MLTree.ComputeL1 underflowed on Node '+IntToStr(ANode.index));{$ENDIF}
end;

constructor TComputeL0L1ChildThread.Create(parent: TComputeL0L1Thread; index: integer; tree: TMLTree);
begin
  inherited Create(true);
  Priority := tpNormal;

  MainThread  := parent;
  ThreadIndex := index;
  L0Mode := false;

  MLTree := tree;
  Node := nil;

  LBound := 0;
  UBound := 0;

//  Done := true;
  Flag := true;

  FreeOnTerminate := true;
  FIsCancelled := False;
  FIsSuccess := True;
  FErrMsg := EmptyStr;
end;

destructor TComputeL0L1ChildThread.Destroy;
begin
  inherited;
end;

procedure TComputeL0L1ChildThread.ComputeL1;
begin
  {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.ComputeL1 in a thread');{$ENDIF}
  if Node <> nil then
  begin
    if LBound < UBound then
      Flag := MainThread.MLTree.ComputeL1OfNodeRange(Node, LBound, UBound);
  end;
  {$IFDEF DEBUG}WriteToDevLog('end TMLTree.ComputeL1 in a thread');{$ENDIF}
end;

procedure TComputeL0L1ChildThread.ComputeL0;
begin
  {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.ComputeL0 in a thread');{$ENDIF}
  if Node <> nil then
  begin
    if LBound < UBound then
      MainThread.MLTree.ComputeL0OfNodeRange(Node, LBound, UBound);
    Flag := true;
  end;
  {$IFDEF DEBUG}WriteToDevLog('end TMLTree.ComputeL0 in a thread');{$ENDIF}
end;

procedure TComputeL0L1ChildThread.Execute;
begin  { TODO -oglen -cml : fix an infinite loop here when doing standard bootstrap for ML inference }
  try
    repeat
      if Node <> nil then
      begin
//        Done := false;
        Flag := true;
        if L0Mode then
          ComputeL0
        else
          ComputeL1;
//        while Done <> true do
//          Done := true;
        while Node <> nil do
          Node := nil;
      end;
      ThreadSwitch;
    until Terminated;
  except
    on E:EAbort do
    begin
      FIsCancelled := True;
      FIsSuccess := False;
      FErrMsg := 'user cancelled';
    end;
    on E:Exception do
    begin
      FIsSuccess := False;
      FErrMsg := E.Message;
    end;
  end;
end;

///////////////////////

constructor TComputeL0L1Thread.Create(tree: TMLTree);
begin
  inherited Create(self, 0, tree);
  FreeOnTerminate := false;
  NoOfThreads := MLTree.NoOfThreads;
  setlength(ChildThread, NoOfThreads);
end;

destructor TComputeL0L1Thread.Destroy;
begin
  setlength(ChildThread, 0);

  inherited;
end;

function TComputeL0L1Thread.CheckChildThreadDone: boolean;
var
  i: integer;
begin
  result := true;
  for i := 1 to NoOfThreads-1 do
    result := result and (ChildThread[i].Node = nil);
end;

procedure TComputeL0L1Thread.Execute;
var
  nt,n0,n1,i: integer;
begin
  ChildThread[0] := self;
  try
    try
      for i := 1 to MLTree.NoOfThreads-1 do
      begin
        ChildThread[i] := TComputeL0L1ChildThread.Create(self, i, MLTree);
        ChildThread[i].Start;
      end;

      repeat
        if Node <> nil then
        begin
          if L0Mode then
            if Node.nc0*length(Node.L0) mod 5 = 0 then
              nt := Node.nc0*length(Node.L0) div 5
            else
              nt := Node.nc0*length(Node.L0) div 5 +1
          else
            if Node.nc1*length(Node.L1) mod 10 = 0 then
              nt := Node.nc1*length(Node.L1) div 10
            else
              nt := Node.nc1*length(Node.L1) div 10 +1;
          if nt > MLTree.NoOfThreads then
            nt := MLTree.NoOfThreads;

          Flag := true;
          if nt > 1 then
          begin
            if L0Mode then
            begin
              n1 := Node.nc0 div (nt-1);
              n0 := Node.nc0 mod (nt-1);
            end
            else
            begin
              n1 := Node.nc1 div (nt-1);
              n0 := Node.nc1 mod (nt-1);
            end
          end
          else
          begin
            if L0Mode then
            begin
              n1 := 0;
              n0 := Node.nc0;
            end
            else
            begin
              n1 := 0;
              n0 := Node.nc1;
            end;
          end;

          if n1 > 0 then
            for i := 1 to nt-1 do
            begin
              ChildThread[i].L0Mode := L0Mode;
              ChildThread[i].LBound := (i-1)*n1 +1;
              ChildThread[i].UBound := i*n1;
//              ChildThread[i].Done := false;

              ChildThread[i].Node := Node;
            end;

          if n0 > 0 then
          begin
            LBound := (nt-1)*n1 +1;
            UBound := (nt-1)*n1 +n0;

//            Done := false;
            if L0Mode then
              Flag := MLTree.ComputeL0OfNodeRange(Node, LBound, UBound)
            else
              Flag := MLTree.ComputeL1OfNodeRange(Node, LBound, UBound);
          end;

          while not CheckChildThreadDone do
            ThreadSwitch;

          if not L0Mode then
            for i := 1 to nt-1 do
              Flag := Flag and ChildThread[i].Flag;

          LBound := 0;
          UBound := 0;
          Node := nil;
//          Done := true;
        end;
        ThreadSwitch;
      until Terminated;

      while not CheckChildThreadDone do
        ThreadSwitch;
    except
      on E:EAbort do
      begin
        FIsCancelled := True;
        FIsSuccess := False;
        FErrMsg := 'user cancelled';
      end;
      on E:Exception do
      begin
        FIsSuccess := False;
        FErrMsg := E.Message;
      end;
    end;
  finally
    for i := 1 to MLTree.NoOfThreads-1 do
    begin
      ChildThread[i].Terminate;
      FIsSuccess := FIsSuccess and ChildThread[i].IsSuccess;
      FIsCancelled := FIsCancelled or ChildThread[i].IsCancelled;
    end;
  end;
end;

function TMLTree.ComputeL0OfNode(ANode: TMLTreeNode): boolean;
begin
  if ANode.L0[0] = nil then
    ANode.AllocNodeMem0;

  if Assigned(ComputeL0L1Thread) then
  begin
    ComputeL0L1Thread.L0Mode := true;
    ComputeL0L1Thread.Node := ANode;
    while ComputeL0L1Thread.Node <> nil do
      ThreadSwitch;
    ComputeL0L1Thread.L0Mode := false;
    result := ComputeL0L1Thread.Flag;
  end
  else
    result := ComputeL0OfNodeRange(ANode, 1, ANode.nc0);
end;

function TMLTree.ComputeL1OfNode(ANode: TMLTreeNode): boolean;
begin
  if ANode.L1[0] = nil then
    ANode.AllocNodeMem1;

  if Assigned(ComputeL0L1Thread) then
  begin
    ComputeL0L1Thread.L0Mode := false;
    ComputeL0L1Thread.Node := ANode;
    while ComputeL0L1Thread.Node <> nil do
      ThreadSwitch;
    result := ComputeL0L1Thread.Flag;
  end
  else
    result := ComputeL1OfNodeRange(ANode, 1, ANode.nc1);
end;

function TMLTree.GetTreeData(tree: TTreeData): boolean;
var
  i: integer;
begin
  result := false;
  tree.isBLen := true;
  tree.isSE   := true;
  for i := FNoOfSeqs to 2*FNoOfSeqs-2 do
  begin
    Tree.NodeArray[i-FNoOfSeqs].des1 := node[i].des1.index;
    Tree.NodeArray[i-FNoOfSeqs].des2 := node[i].des2.index;
    Tree.DataCoverage[i-FNoOfSeqs] := node[i].DataCoverage;
    Tree.BLenArray[node[i].des1.index] := node[i].des1.blen;
    Tree.BLenArray[node[i].des2.index] := node[i].des2.blen;
    Tree.SEArray[node[i].des1.index] := node[i].des1.ase;
    Tree.SEArray[node[i].des2.index] := node[i].des2.ase;
  end;
  Tree.Value := LogLikelihood;
  for i := 0 to FNoOfSeqs - 1 do
    Tree.IsOutgroupMember[i] := node[i].IsOutgroupMember;
  for i := 0 to NoOfSeqs - 2 do
    Tree.DataCoverage[i] := node[i + NoOfSeqs].DataCoverage;
  result := true;
end;

procedure TMLTree.SetDataCoverage(tree: TTreeData);
var
  i: Integer = -1;
begin
  for i := 0 to NoOfSeqs - 2 do
    node[i + NoOfSeqs].DataCoverage := Tree.DataCoverage[i];
end;

procedure TMLTree.SetSizeDepth;

  procedure SetSizeDepthOfNode(node: TMLTreeNode);
  begin
    if node.OTU then exit;

    SetSizeDepthOfNode(node.des1);
    SetSizeDepthOfNode(node.des2);

    node.size  := node.des1.size +node.des2.size;
    node.depth := max(node.des1.depth, node.des2.depth) +1;
  end;

begin
  SetSizeDepthOfNode(root);
end;

function TMLTree.GetApproximateSizeInBytes: Int64;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to 2*NoOfSeqs-2 do
    Result := Result + node[i].GetApproximateSizeInBytes;
  Result := Result + SizeOf(TStringList) + SizeOf(Char)*NoOfSeqs*OrigNumSites; // FSeqs
  Result := Result + SizeOf(Integer)*4; // FMaxIter, FNoOfSeqs, FNoOfSites, FNoOfThreads
  Result := Result + 2*SizeOf(Extended); // FDeltaL, FLogLikelihood
  Result := Result + SizeOf(Boolean); // FInitialized
  Result := Result + SizeOf(TReltimeComputer); // FReltimeComputer - not initialized as this function is only intended for model selection and bootstrap tree search
  Result := Result + 2*SizeOf(Integer)*20; // site
  if FModel.SeqDataType = Protein then
    Result := Result + GetApproximateSizeOfProteinModelInBytes(OrigNumSites, FModel.NoOfRates)
  else
    Result := Result + GetApproximateSizeOfDnaModelInBytes(OrigNumSites, FModel.NoOfRates);
end;

procedure TMLTree.InitNodeData(tree: TTreeData);
var
  b: extended;
  i,j,n: integer;
begin
  ResetNodes;
  n := FNoOfSeqs;
  for i := 0 to n-2 do
  begin
    node[n+i].des1 := node[tree[i].des1];
    node[n+i].des2 := node[tree[i].des2];
    node[n+i].DataCoverage := tree.DataCoverage[i];
    node[tree[i].des1].anc := node[n+i];
    node[tree[i].des2].anc := node[n+i];
    node[n+i].des1.blen := tree.BLen[tree[i].des1];
    if node[n+i].des1.blen < 0 then
      node[n+i].des1.blen := 0;
    node[n+i].des2.blen := tree.BLen[tree[i].des2];
    if node[n+i].des2.blen < 0 then
      node[n+i].des2.blen := 0;
    if tree.isSE then
    begin
      node[n+i].des1.ase := tree.SE[tree[i].des1];
      node[n+i].des2.ase := tree.SE[tree[i].des2];
    end;
    node[n+i].CI := tree.Stats[i];

    node[n+i].des1.fixed := false; //(tree.BLen[tree[i].des1] > 0) and (tree.Stats[tree[i].des1] > 0);
    node[n+i].des2.fixed := false; //(tree.BLen[tree[i].des2] > 0) and (tree.Stats[tree[i].des2] > 0);
  end;

  for i := 0 to FNoOfSeqs - 1 do
    node[i].IsOutgroupMember := tree.IsOutgroupMember[i];

  for i := 0 to 2*FNoOfSeqs-2 do
    if node[i].anc = nil then
    begin
      root := node[i];
      break;
    end;
  OriginalRoot.X := root.des1.index;
  OriginalRoot.Y := root.des2.index;
//  if FNeedsRootedOnOutgroup then       // Root must be on the longest external branch.
//    ChangeRootByOutgroup               // ChangeRootByOutgroup afterward if necessary.
//  else
//  begin
    b := 0;
    j := 0;
    for i := 0 to FNoOfSeqs-1 do
    begin
      if node[i] = root then continue;
      if node[i].blen > b then
      begin
        j := i;
        b := node[i].blen;
      end;
    end;
    node[j].flag := true;
    ChangeRoot(node[j]);
//  end;

{
  root.des2.blen := root.des1.blen +root.des2.blen;
  root.des1.blen := 0;
  if tree.isSE then
  begin
    root.des2.ase := root.des1.ase +root.des2.ase;
    root.des1.ase := 0;
  end;
  root.des1.fixed := true;
}

  SetSizeDepth;

//  Initialize;
  FInitialized := false;
end;

procedure TMLTree.SetBLens(tree: TTreeData);
var
  i,n: integer;
begin
  n := FNoOfSeqs;
  for i := 0 to n-2 do
  begin
    node[n+i].des1.blen := tree.BLen[tree[i].des1];
    if node[n+i].des1.blen < 0 then
      node[n+i].des1.blen := 0;
    node[n+i].des2.blen := tree.BLen[tree[i].des2];
    if node[n+i].des2.blen < 0 then
      node[n+i].des2.blen := 0;
    if tree.isSE then
    begin
      node[n+i].des1.ase := tree.SE[tree[i].des1];
      node[n+i].des2.ase := tree.SE[tree[i].des2];
    end;

    node[n+i].des1.fixed := false; //(tree.BLen[tree[i].des1] > 0) and (tree.Stats[tree[i].des1] > 0);
    node[n+i].des2.fixed := false; //(tree.BLen[tree[i].des2] > 0) and (tree.Stats[tree[i].des2] > 0);
  end;
  node[2*n-2].des2.blen := node[2*n-2].des1.blen+node[2*n-2].des2.blen;
  node[2*n-2].des1.blen := 0;
  node[2*n-2].des2.ase := node[2*n-2].des1.ase+node[2*n-2].des2.ase;
  node[2*n-2].des1.ase := 0;

  ResetProb;
  FLogLikelihood := ResetL1;
end;

procedure TMLTree.InitRateParams;
var
  gamma, invar, LG: extended;
begin
  if (Model.NoOfRates > 1) and Model.UseInvar then
  begin;
    gamma := Model.Gamma;
    invar := Model.Invar;
    Model.Gamma := Model.gamma0;
    Model.Invar := 0;
    ResetProb;
    LG := ResetL1;
    if LG < FLogLikelihood then
    begin
      Model.Gamma := gamma;
      Model.Invar := invar;
      ResetProb;
      FLogLikelihood := ResetL1;
    end;
  end;
end;

procedure TMLTree.Initialize(InitModel: boolean; InitConfig: boolean);
var
  tree: TTreeData;
begin
  {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.Initialize');{$ENDIF}
  {$IFDEF DEBUG}SaveMLlog(' MLTree.Initialize started');{$ENDIF}
  if InitConfig then
    InitSiteConfig;

  if InitModel then
    if Model is TGammaRateVariationModel then
      if not Model.Initialized then
        TGammaRateVariationModel(Model).SetParamsFromSeqs(seqs);

  ResetProb;
  FLogLikelihood := ResetL1;
  if IsDeveloper then
    FDevInitialLogLikelihood := FLogLikelihood;

  FInitialized := true;
  {$IFDEF DEBUG}WriteToDevLog('end TMLTree.Initialize');{$ENDIF}
  {$IFDEF DEBUG}SaveMLlog(' MLTree.Initialize ended');{$ENDIF}
end;


function TMLTree.ComputeLogLikelihoodOfNodeWithModel(ANode: TMLTreeNode; AModel: TGammaRateVariationModel): extended;
var
  Li: Extended;
  i,j,k,k0,k1,c,n0: integer;
begin
  result := 0;
  if ANode.L0[0] = nil then
  begin
    exit;
  end;
  with ANode do
    for c := 1 to NoOfConfs do
    begin
      CheckAbort;
      k  := Root.cs1[c];
      k0 := c0[k];
      k1 := c1[k];
      Li := 0;
      if (ANode.anc = nil) or ANode.OTU then
        for i := 0 to AModel.NoOfRates-1 do
          for j := 0 to NoOfStates-1 do
            Li := Li +AModel.Freq[j]*L1[i,k1,j]
      else
        for i := 0 to AModel.NoOfRates-1 do
          for j := 0 to NoOfStates-1 do
            Li := Li +AModel.Freq[j]*L0[i,k0,j]*L1[i,k1,j];

      Li := Li/AModel.NoOfRates*(1-AModel.Invar);

//      L[k] := Li;

      if AModel.UseInvar and (AModel.NoOfInvarSites > 0) then
      begin
        n0 := AModel.f0[Root.cs1[c]];
        if n0 >= 0 then
          Li := Li +AModel.Freq[n0]*AModel.Invar;
      end;
      if Li > 0 then
        if FModel.BootTable <> nil then
          result := result +bc[c]*ln(Li)
        else
          result := result +nc[c]*ln(Li)
      else
        result := result -nc[c]*706; // ln(2.225E-307) = -706.1 for double precision
    end;
  CheckAbort;
end;

function TMLTree.ComputeLogLikelihoodOfNode(ANode: TMLTreeNode): extended;
begin
  result := ComputeLogLikelihoodOfNodeWithModel(ANode, Model);
end;

function TMLTree.ComputeLogLikelihood: extended;
var
  i,j,c,n0: integer;
  Li: Extended;
begin
  result := 0;
  with root do
    for c := 1 to NoOfConfs do
    begin
      CheckAbort;
      Li := 0;
      for i := 0 to Model.NoOfRates-1 do
        for j := 0 to NoOfStates-1 do
          Li := Li +Model.Freq[j]*L1[i,c,j];
      Assert((IsNan(Li) = False) and (IsInfinite(Li) = False), 'invalid value in ComputeLogLikelihood');
      Assert(CompareValue(Model.NoOfRates*(1-Model.Invar), 0, FP_CUTOFF) <> 0, 'division by zero in TMLTree.ComputeLogLikelihood');
      Li := Li/Model.NoOfRates*(1-Model.Invar);
      Assert((IsNan(Li) = False) and (IsInfinite(Li) = False), 'invalid value in ComputeLogLikelihood');
      if Model.UseInvar and (Model.NoOfInvarSites > 0) then
      begin
        n0 := Model.f0[cs1[c]];
        if n0 >= 0 then
          Li := Li +Model.Freq[n0]*Model.Invar;
      end;
      Assert((IsNan(Li) = False) and (IsInfinite(Li) = False), 'invalid value in ComputeLogLikelihood');

      if Li > 0 then
        if Model.BootTable <> nil then
          result := result +bc[c]*ln(Li)
        else
          result := result +nc[c]*ln(Li)
      else
        result := result -nc[c]*706; // ln(2.225E-307) = -706.1 for double precision
    end;
end;

//////////////////

function TMLTree.IterateBlenOfNodeWithModel(ANode: TMLTreeNode; AModel: TGammaRateVariationModel; L0: extended; delta: extended): extended;
var
  flag: boolean;

  function ComputeLikelihood(ANode: TMLTreeNode): extended;
  begin
    flag := ComputeL1OfNode(ANode.anc);

    result := ComputeLogLikelihoodOfNodeWithModel(ANode.anc, AModel);
  end;

var
  L, b,d,db,dL,dL0,dL1,dL2,b0,v0,minb: extended;
begin
  L := L0;
  Result := L;

  if ANode.fixed then exit;

  minb := 1/ANode.OrigNoOfSites/(2*ANode.NoOfSeqs-3);

  b0 := ANode.blen;
  v0 := ANode.vb;
  b  := b0;
  if b < minb then
    d := minb*delta
  else
    d := b*delta;

  d := max(d, 1.0E-8);

  dL0  := L;

  AModel.ComputeProbOfNode(ANode, (b+d));
  dL1 := ComputeLikelihood(ANode);
  CheckAbort;
  AModel.ComputeProbOfNode(ANode, (b+d/2));
  dL2 := ComputeLikelihood(ANode);
  CheckAbort;
  dL := (dL1-dL2)-(dL2-dL0);     //  dL1-2*dL2+dL0 causes loss of trailing digits
  if (abs(dL) > 0) then
  begin
    db := d*(dL1-dL0)/4/dL;
    if b < db then
      if dL1 > dL0 then
        b := b + d
      else if dL2 > dL0 then
        b := b + d/2
      else
        b := 0
    else
    begin
      b := b -db;
      if (delta > 1/20000) and (dL < 0) then
        ANode.vb  := -d*d/dL;
    end;
  end;

  ANode.blen := b;

  AModel.ComputeProbOfNode(ANode, b);
  L := ComputeLikelihood(ANode);
  CheckAbort;
  if (not flag) and (b < minb) then
  begin
    ANode.blen := minb;
    ANode.vb   := 0;
    AModel.ComputeProbOfNode(ANode, minb);
    L := ComputeLikelihood(ANode);
    CheckAbort;
  end;
  if L < dL0 then
  begin
    ANode.blen := b0;
    ANode.vb   := v0;
    AModel.ComputeProbOfNode(ANode, b0);
    L := ComputeLikelihood(ANode);
    CheckAbort;
  end;

  Result := L;
end;

{
function TMLTree.IterateBlenOfNodeDown(ANode: TMLTreeNode; AModel: TGammaRateVariationModel; L0: extended; delta: extended): extended;

  function ComputeLikelihood(ANode: TMLTreeNode): extended;
  begin
    ComputeL0OfNode(ANode);

    result := ComputeLogLikelihoodOfNodeWithModel(ANode, AModel);
  end;

var
  L,b,d,db,dL,dL0,dL1,dL2,b0,v0, minb: extended;
begin
  ComputeL0OfNode(ANode.anc);

  Result := L;

  if ANode.fixed then exit;

  minb := 1/ANode.OrigNoOfSites/(2*ANode.NoOfSeqs-3);

  b0 := ANode.blen;
  v0 := ANode.vb;
  b  := b0;
  if b < minb then
  begin
    b := minb;
    AModel.ComputeProbOfNode(ANode, b);
    L := ComputeLikelihood(ANode.anc);
  end;

  d := b*delta;

  dL0  := L;

  AModel.ComputeProbOfNode(ANode, (b+d));

  dL1 := ComputeLikelihood(ANode.anc);

  AModel.ComputeProbOfNode(ANode, (b-d));

  dL2 := ComputeLikelihood(ANode.anc);

  dL := (dL1-2*dL0+dL2);
  if (abs(dL) > 0) then
  begin
    db := d*(dL1-dL2)/2/dL;

    if -db > b then
      b := 2*b
    else if (b > 0.0000001) and (db > b/2) then
      b := b/2
    else if b < db then
      b := 0
    else
    begin
      b := b -db;
      if (delta > 1/20000) and (dL < 0) then
        ANode.vb  := -d*d/dL;
    end;

  end;

  ANode.blen := b;

  AModel.ComputeProbOfNode(ANode, b);

  L := ComputeLikelihood(ANode.anc);

  if L < dL0 then
  begin
    ANode.blen := b0;
    ANode.vb   := v0;
    AModel.ComputeProbOfNode(ANode, b0);
    L := ComputeLikelihood(ANode.anc);
  end;

  Result := L;
end;
}
//////////////////

function TMLTree.IterateBlenOfNode(ANode: TMLTreeNode; L0: extended; delta: extended): extended;
var
  flag: boolean;

  function ComputeLikelihood(n: TMLTreeNode): extended;
  begin
    flag := ComputeL1OfNode(n.anc);

    result := ComputeLogLikelihoodOfNode(n.anc);
  end;

var
  L,b,d,db,dL,dL0,dL1,dL2,b0,v0, minb: extended;
  //it: integer;
begin
  result := L0;
  flag := true;
  if ANode.fixed then
    exit;
//  it := 0;
//  repeat

    L0 := ComputeLikelihood(ANode);
    L := L0;

    minb := 1/OrigNumSites/(2*NoOfSeqs-3); { OrigNumSites used here because ModelTamer and LBS only keep unique site configurations in SeqData}

    b0 := ANode.blen;
    v0 := ANode.vb;
    b  := b0;
    if b < minb then
      d := minb*delta
    else
      d := b*delta;

    d := max(d, 1.0E-8);

    dL0  := L;

    Model.ComputeProbOfNode(ANode, (b+d));
    dL1 := ComputeLikelihood(ANode);
    CheckAbort;
    Model.ComputeProbOfNode(ANode, (b+d/2));
    dL2 := ComputeLikelihood(ANode);
    CheckAbort;
    dL := (dL1-dL2)-(dL2-dL0);     //  dL1-2*dL2+dL0 causes loss of trailing digits
    if (abs(dL) > 0) then
    begin
      db := d*(dL1-dL0)/4/dL;
      if b < db then
        if dL1 > dL0 then
          b := b + d
        else if dL2 > dL0 then
          b := b + d/2
        else
          b := 0
      else if b/2 < db then
        b := b/2
      else
      begin
        b := b -db;
        if (delta > 1/20000) and (dL < 0) then
          ANode.vb  := -d*d/dL;
      end;
    end;

    ANode.blen := b;
    Model.ComputeProbOfNode(ANode, b);
    L := ComputeLikelihood(ANode);
    CheckAbort;
{
    if (not flag) and (b < minb) then
    begin
      ANode.blen := minb;
      ANode.vb   := 0;
      Model.ComputeProbOfNode(ANode, minb);
      L := ComputeLikelihood(ANode);
      CheckAbort;
    end;
}
    if L < dL0 then
    begin
      ANode.blen := b0;
      ANode.vb   := v0;
      Model.ComputeProbOfNode(ANode, b0);
      L := ComputeLikelihood(ANode);
      CheckAbort;
    end;

    result := L;
//    inc(it);
//  until (abs(L-dL0) < deltaL) or (it = MaxIter);
//{$IFDEF DEBUG}SaveMLlog(FloatToStr(FLogLikelihood)+' MLTree.IterateBlenOfNode '+IntToStr(ANode.index));{$ENDIF}
end;

function TMLTree.IterateTreeSize: extended;
var
  d,dL,ddL,dL0,dL1,dL2,r: extended;
  i: integer;
begin
  dL0 := LogLikelihood;
  d := deltaL;

  for i := 0 to 2*NoOfSeqs-2 do
    Node[i].blen := Node[i].blen*(1+d);
  ResetProb;
  dL1 := ResetL1;

  for i := 0 to 2*NoOfSeqs-2 do
    Node[i].blen := Node[i].blen*(1-d)/(1+d);
  ResetProb;
  dL2 := ResetL1;

  dL  := (dL1-dL2)/d/2;
  ddL := ((dL1-dL0)-(dL0-dL2))/d/d;

  if abs(ddL) > 0 then
    r := 1 -dL/ddL
  else
    r := 1;

  for i := 0 to 2*NoOfSeqs-2 do
    Node[i].blen := Node[i].blen*r/(1-d);
  ResetProb;
  LogLikelihood := ResetL1;
  if LogLikelihood < dL0 then
  begin
    for i := 0 to 2*NoOfSeqs-2 do
      Node[i].blen := Node[i].blen/r;
    ResetProb;
    LogLikelihood := ResetL1;
    r := 1;
  end;

  result := r;
end;

function TMLTree.ResetL1: extended;

  procedure ResetRecursive(n: TMLTreeNode);
  var
    flag: boolean;
  begin
    if n.OTU then
      exit;

    ResetRecursive(n.des1);
    ResetRecursive(n.des2);

    flag := ComputeL1OfNode(n);

    if not flag then
      if (n.des1.blen < FP_CUTOFF) and (n.des2.blen < FP_CUTOFF) then
      begin
        n.des1.blen := 1/OrigNumSites/2; { OrigNumSites used here because ModelTamer and LBS only keep unique site configurations in SeqData}
        n.des2.blen := 1/OrigNumSites/2;
        ResetProbOfNode(n.des1);
        ResetProbOfNode(n.des2);
        ComputeL1OfNode(n);
      end;
  end;

begin
  ResetRecursive(root);
  CheckAbort;
  result := ComputeLogLikelihood;
  CheckAbort;
end;

procedure TMLTree.ResetL0OfNode(ANode: TMLTreeNode);

  procedure ResetRecursive(n: TMLTreeNode);
  begin
    if (n.anc <> nil) and (n.anc.L0[0] = nil) then
      ResetRecursive(n.anc);
    if n.L0[0] = nil then
      ComputeL0OfNode(n);
  end;

begin
  ResetRecursive(ANode);
end;

procedure TMLTree.ResetL0;

  procedure ResetRecursive(n: TMLTreeNode);
  var
    flag: boolean;
  begin
    if n.OTU then
      exit;

    flag := ComputeL0OfNode(n);

    ResetRecursive(n.des1);
    ResetRecursive(n.des2);
  end;

begin
  ResetRecursive(root.des1);
  ResetRecursive(root.des2);
end;

procedure TMLTree.OptimizeParameters;
begin
  {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.OptimizeParameters');{$ENDIF}

  if (NoOfThreads > 1) and (not assigned(ComputeL0L1Thread)) then
  begin
    ComputeL0L1Thread := TComputeL0L1Thread.Create(self);
    ComputeL0L1Thread.Start;
  end;

  if FHideSubtaskProgress then
    Model.OptimizeParameters(self, nil)
  else
    Model.OptimizeParameters(self, SubtaskCheckCancel);


  if assigned(ComputeL0L1Thread) then
  begin
    try
      ComputeL0L1Thread.Terminate;
      while not ComputeL0L1Thread.Finished do
        ThreadSwitch;
      if ComputeL0L1Thread.IsCancelled then
        raise EAbort.Create(ComputeL0L1Thread.ErrMsg);
      if not ComputeL0L1Thread.IsSuccess then
        raise Exception.Create(ComputeL0L1Thread.ErrMsg);
    finally
      FreeAndNil(ComputeL0L1Thread);
    end;
  end;

  {$IFDEF DEBUG}WriteToDevLog('end TMLTree.OptimizeParameters');{$ENDIF}
end;

procedure TMLTree.OptimizeTreeSize;
var
  L0,r: extended;
  iter,i: integer;
begin
  iter := 0;
  r := 1.0;
  repeat
    L0  := LogLikelihood;
    r := r*IterateTreeSize;
    inc(iter);
    CheckAbort;
  until (abs(LogLikelihood-L0) < deltaL) or (iter = MaxIter);
  for i := 0 to 2*NoOfSeqs-2 do
    Node[i].vb := Node[i].vb*r*r;
end;

function TMLTree.Optimize(Cancel: TCheckCancelFunc; ProgressMsg: Ansistring; KeepRootPosition: boolean): boolean;

  procedure CheckVB(n: TMLTreeNode);
  begin
    if not n.OTU then
    begin
      CheckVB(n.des1);
      CheckVB(n.des2);
    end;

    if (n <> root) and (n.anc <> root) then
      if (n.vb < 0) or (2*sqrt(n.vb) > n.blen) then
        n.vb := n.blen*n.blen/4;
  end;

var
  p: TMLTreeNode;
  L0,L1,dL1,dL2,ddL: extended;
  it,i: Integer;
  Progress: integer = 0;
  n: integer;
begin
  {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.Optimize');{$ENDIF}
  Result := False;

  if assigned(Cancel) then
    if Cancel(Progress, ProgressMsg) then
      Exit;

  for i := 0 to 2*NoOfSeqs -2 do
    Node[i].fixed := false;
  if root.des1.blen > root.des2.blen then
    root.des2.fixed := true
  else
    root.des1.fixed := true;

//  Initialize(true,true);  Should be call separately to avoid duplication

  Progress := 2;
  if Assigned(Cancel) then
    if Cancel(Progress, ProgressMsg) then
      Exit;
  Result := true;
  CheckAbort;

// To optimize the deltaL value.

  FdeltaL := -FLogLikelihood/power(10, OptimizePrecision);
  if FdeltaL > 1.0 then
    FdeltaL := 1.0
  else if deltaL < 1.0E-4 then
    FdeltaL := 1.0E-4;

  {$IFDEF DEBUG}SaveMLlog(FloatToStr(FLogLikelihood)+' MLTree.Optimize started G: '+FloatToStr(Model.Gamma)+', I: '+FloatToStr(Model.Invar));{$ENDIF}

  InitRateParams;

  it := 0;
  n := MaxIter*2;
  repeat
    L0 := FLogLikelihood;

    SubtaskProgress(Round(it/n*100), 'Optimizing branch lengths');

    {$IFDEF DEBUG}WriteToDevLog(Format('Delta LnL (%d) = %.8n [%.8n - %.8n]', [it + 1, LogLikelihood - L0, LogLikelihood, L0]));{$ENDIF}

    OptimizeAllBLens(Cancel, Progress, ProgressMsg);

   {$IFDEF DEBUG}SaveMLlog(FloatToStr(FLogLikelihood)+' MLTree.Optimize: optimize branch lengths finished');{$ENDIF}

    {$IFDEF DEBUG}WriteToDevLog(Format('Delta LnL (%d) = %.8n [%.8n - %.8n]', [it + 1, LogLikelihood - L0, LogLikelihood, L0]));{$ENDIF}

    if assigned(Cancel) then
    begin
      Progress := round(100*(it+1/2)/n);
      if Cancel(Progress, ProgressMsg) then
      begin
        Cancel(Progress, 'Canceling tree optimization');
        Result := False;
        break;
      end;
    end;

    SubtaskProgress(0, 'Optimizing parameters');

    OptimizeParameters;

  {$IFDEF DEBUG}SaveMLlog(FloatToStr(FLogLikelihood)+' MLTree.Optimize: optimize parameters finsished G: '+FloatToStr(Model.Gamma)+', I: '+FloatToStr(Model.Invar));{$ENDIF}

    if assigned(Cancel) then
    begin
      Progress := round(100*(it+1)/n);
      if Cancel(Progress, ProgressMsg) then
      begin
        Cancel(Progress, 'Canceling tree optimization');
        Result := False;
        break;
      end;
    end;
    inc(it);
    {$IFNDEF VISUAL_BUILD}
    if IsDeveloper then
      CheckDumpDeveloperData;
    {$ENDIF}
    {$IFDEF DEBUG}WriteToDevLog(Format('Delta LnL2 (%d) = %.8n [%.8n - %.8n]', [it, FLogLikelihood - L0, FLogLikelihood, L0]));{$ENDIF}

   if it = 2*MaxIter-1 then
     dL1 := FLogLikelihood-L0
   else if it >= MaxIter*2 then
   begin
     dL2 := FLogLikelihood-L0;
     ddL := dL1-dL2;
     dL1 := dL2;
     n := it +ceil(dL2/ddL);
   end;

  until (abs(FLogLikelihood-L0) < deltaL*2) or (it >= n);    //  *2 for OptimizeAllBLens + OptimizeParameters

  CheckVB(root);
{
  if (not FNeedsRootedOnOutgroup) and FIsDoingTreeSearch then
    ChangeRoot(OriRoot);
}
  CheckAbort;

  if KeepRootPosition then
  begin
    n := -1;
    if (Root.des1.index <> OriginalRoot.X) and (Root.des1.index <> OriginalRoot.Y) then
    begin
      if Node[OriginalRoot.X].anc.index = OriginalRoot.Y then
        n := OriginalRoot.X
      else if Node[OriginalRoot.Y].anc.index = OriginalRoot.X then
        n := OriginalRoot.Y;
    end
    else if Root.des1.index = OriginalRoot.X then
      n := OriginalRoot.X
    else if Root.des1.index = OriginalRoot.Y then
      n := OriginalRoot.Y;
    if n > -1 then
    begin
      ChangeRootWithBalancedBlens(Node[n]);
      if not FInitialized then
        Initialize(false, true);
    end;
  end
  else if root.des1.size < NoOfSeqs/2 then
  begin
    p := root.des1;
    root.des1 := root.des2;
    root.des2 := p;
  end;

//  Initialize(false);

  SubtaskProgress(0, 'Preparing tree display');

  {$IFDEF DEBUG}WriteToDevLog('end TMLTree.Optimize');{$ENDIF}
  {$IFDEF DEBUG}SaveMLlog(FloatToStr(FLogLikelihood)+' MLTree.Optimize ended');{$ENDIF}
end;

procedure TMLTree.OptimizeAllBLens(Cancel: TCheckCancelFunc; Progress: Integer; ProgressMsg: Ansistring);
begin
  {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.OptimizeAllBlens');{$ENDIF}

  if (NoOfThreads > 1) and not assigned(ComputeL0L1Thread) then
  begin
    ComputeL0L1Thread := TComputeL0L1Thread.Create(self);
    ComputeL0L1Thread.Start;
  end;

  SubtaskProgress(0, 'Optimizing branch lengths');

//  if not Initialized then
//    Initialize(false);

  if Assigned(Cancel) then
  begin
    if not Cancel(Progress, ProgressMsg) then
      FinalizeOptimizeAllBLens(Cancel, Progress, ProgressMsg);
  end
  else
    FinalizeOptimizeAllBLens(nil, Progress, ProgressMsg);

  SubtaskProgress(100, 'Optimizing branch lengths');

  if assigned(ComputeL0L1Thread) then
  begin
    try
      ComputeL0L1Thread.Terminate;
      while not ComputeL0L1Thread.Finished do
        ThreadSwitch;
      if ComputeL0L1Thread.IsCancelled then
        raise EAbort.Create(ComputeL0L1Thread.ErrMsg);
      if not ComputeL0L1Thread.IsSuccess then
        raise Exception.Create(ComputeL0L1Thread.ErrMsg);
    finally
      FreeAndNil(ComputeL0L1Thread);
    end;
  end;

  {$IFDEF DEBUG}WriteToDevLog('end TMLTree.OptimizeAllBlens');{$ENDIF}
end;

function TMLTree.FinalizeOptimizeAllBLens(Cancel: TCheckCancelFunc; Progress: integer; ProgressMsg: AnsiString): boolean;
var
  L0,dL,d: extended;
  it: integer;

  procedure IterateBlenUp(n: TMLTreeNode);
  begin
    if not n.OTU then
    begin
        ComputeL0OfNode(n);

      if n.des1.blen > n.des2.blen then
      begin
        IterateBlenUp(n.des1);
        IterateBlenUp(n.des2);
      end
      else
      begin
        IterateBlenUp(n.des2);
        IterateBlenUp(n.des1);
      end;
    end;

    if not n.fixed then
      FLogLikelihood := IterateBlenOfNode(n, FLogLikelihood, d);

    if not n.OTU then
      n.DeallocNodeMem0;
  end;

  procedure IterateBlenDown(n: TMLTreeNode);
  begin
    if not n.fixed then
      FLogLikelihood := IterateBlenOfNode(n, FLogLikelihood, d);

    if n.OTU then exit;

    ComputeL0OfNode(n);

    if n.des1.blen > n.des2.blen then
    begin
      IterateBlenDown(n.des1);
      IterateBlenDown(n.des2);
    end
    else
    begin
      IterateBlenDown(n.des2);
      IterateBlenDown(n.des1);
    end;

    n.DeallocNodeMem0;
  end;

  procedure CheckVB(n: TMLTreeNode);
  begin
    if not n.OTU then
    begin
      CheckVB(n.des1);
      CheckVB(n.des2);
    end;

    if (n <> root) and (n.anc <> root) then
      if (n.vb < 0) or (2*sqrt(n.vb) > n.blen) then
        n.vb := n.blen*n.blen/4;
  end;

  var
    b0,v0: array of extended;

  procedure SaveBLens;
  var
    i: integer;
  begin
    for i := 0 to 2*NoOfSeqs-2 do
    begin
      b0[i] := Node[i].blen;
      v0[i] := Node[i].vb;
    end;
  end;

  procedure RetrieveBLens;
  var
    i: integer;
  begin
    for i := 0 to 2*NoOfSeqs-2 do
    begin
      Node[i].blen := b0[i];
      Node[i].vb   := v0[i];
    end;
    ResetProb;
    FLogLikelihood := ResetL1;
  end;

var
  dp: double;
begin
  {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.FinalizeOptimizeAllBlens');{$ENDIF}

  setlength(b0,2*NoOfSeqs-1);
  setlength(v0,2*NoOfSeqs-1);

  result := true;
  dp := (100-Progress)/MaxIter;

  d := 1/1000;
  it := 0;
  repeat
    if Assigned(Cancel) then
    begin
      if Cancel(Progress, ProgressMsg) then
        break;
    end;
    SubtaskProgress(Round(it/MaxIter*100), 'Optimizing branch lengths');

    SaveBlens;

    L0 := FLogLikelihood;

    IterateBlenUp(Root.des1);
    CheckAbort;

    IterateBlenUp(Root.des2);
    CheckAbort;

    IterateBlenDown(Root.des1);
    CheckAbort;

    IterateBlenDown(Root.des2);
    CheckAbort;

    if L0 > FLogLikelihood then
    begin
      RetrieveBLens;
      L0 := FLogLikelihood;
      dL := 0;
    end
    else
      dL := abs(FLogLikelihood-L0);

    inc(it);

    if assigned(Cancel) then
      if Cancel(Progress, ProgressMsg) then
      begin
        result := false;
        break;
      end;
//    {$IFDEF DEBUG}SaveMLlog(FloatToStr(FLogLikelihood)+' MLTree.FinalizeOptimizeAllBLens cycle '+IntToStr(it));{$ENDIF}
  until (dL < deltaL) or (it = MaxIter);

  SubtaskProgress(100, 'Optimizing branch lengths');
  {$IFDEF DEBUG}WriteToDevLog('end TMLTree.FinalizeOptimizeAllBlens');{$ENDIF}
end;

function TMLTree.FlagOutgroupNodes: Integer;
var
  numMembers: Integer = 0;

  procedure ProcessNode(ANode: TMLTreeNode);
  var
    n: TMLTreeNode;
  begin
    n := ANode;
    if n.IsOutgroupMember then
    begin
      inc(numMembers);
      n.flag := True;
      while (n.anc <> nil) and (not n.anc.flag) do
      begin
        n.Anc.flag := True;
        n := n.Anc;
      end;
    end;
  end;

var
  i: Integer;
begin
  for i := 0 to 2 * NoOfSeqs - 2 do
    Node[i].flag := False;
  for i := 0 to NoOfSeqs-1 do
    ProcessNode(Node[i]);
  Assert(numMembers > 0, 'missing outgroup information in TMLTree.FlagOutgroupnodes');
  Result := numMembers;
end;

procedure TMLTree.ChangeRoot(newroot: TMLTreeNode);
var
  a, p, d : TMLTreeNode;
  len0, len1, len2: extended;
  ci0, ci1, ci2: double;
  fix0, fix1, fix2: boolean;
begin
  if (newroot = root) then exit;

  if newroot.anc = root then
  begin
    if newroot = root.des2 then
    begin
      root.des2 := root.des1;
      root.des1 := newroot;
    end;
    if newroot.flag then                                // For optimization
    begin
      root.des1.blen := root.des1.blen +root.des2.blen;
      root.des2.blen := 0;
      root.des1.ase := root.des1.ase +root.des2.ase;
      root.des2.ase := 0;
      root.des2.fixed := true;
    end
    else                                                // For tree search
    begin
      root.des2.blen := root.des1.blen +root.des2.blen;
      root.des1.blen := 0;
      root.des2.ase := root.des1.ase +root.des2.ase;
      root.des1.ase := 0;
      root.des1.fixed := true;
    end;
    exit;
  end;

  len0 := root.des1.blen +root.des2.blen;
  ci0 := max(root.des1.CI, root.des2.CI);
  fix0 := root.des1.fixed and root.des2.fixed;
  d := newroot;
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
    ci1 := p.CI;
    p.CI := ci2;
    ci2 := ci1;
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
  end;
  len0 := newroot.blen;
  ci0  := newroot.CI;
  fix0 := newroot.fixed;
  p := newroot.anc;
  p.anc := root;
  p.blen := len0;
  p.CI := ci0;
  p.fixed := fix0;
  newroot.anc := root;
  newroot.blen := 0;
  newroot.CI := ci0;
//  newroot.fixed := p.fixed;
  root.des1 := newroot;
  root.des2 := p;
  if newroot.flag then
  begin
    root.des1.blen := root.des1.blen +root.des2.blen;
    root.des2.blen := 0;
    root.des1.ase := root.des1.ase +root.des2.ase;
    root.des2.ase := 0;
    root.des2.fixed := true;
  end
  else
  begin
    root.des2.blen := root.des1.blen +root.des2.blen;
    root.des1.blen := 0;
    root.des2.ase := root.des1.ase +root.des2.ase;
    root.des1.ase := 0;
    root.des1.fixed := true;
  end;

  SetSizeDepth;

  FInitialized := false;
end;

procedure TMLTree.ChangeRootWithBalancedBlens(newroot: TMLTreeNode);
var
  b1,b2,b12: extended;

  function DistanceToTip(node: TMLTreeNode): extended;
  var
    r1,r2: extended;
  begin
    if node.OTU then
      result := node.blen
    else
    begin
      r1 := DistanceToTip(node.des1);
      r2 := DistanceToTip(node.des2);
      result := max(r1,r2) +node.blen;
    end;
  end;

begin
  if newroot.anc <> root then
    ChangeRoot(newroot);
  b1 := DistanceToTip(root.des1);
  b2 := DistanceToTip(root.des2);
  if abs(b1-b2) < root.des1.blen then
  begin
    root.des1.blen := root.des1.blen -abs(b1-b2)/2;
    root.des2.blen := abs(b1-b2)/2;
  end
  else if abs(b1-b2) < root.des2.blen then
  begin
    root.des1.blen := abs(b1-b2)/2;
    root.des2.blen := root.des2.blen -abs(b1-b2)/2;
  end
  else if b1 < b2 then
  begin
    root.des1.blen := root.des1.blen+root.des2.blen;
    root.des2.blen := 0;
  end
  else
  begin
    root.des2.blen := root.des1.blen+root.des2.blen;
    root.des1.blen := 0;
  end;
end;

procedure TMLTree.ChangeRootByOutgroup;
var
  numFlagged: Integer = 0;
  newroot: TMLTreeNode = nil;
  maxsize: integer;

  procedure FindRoot0(node: TMLTreeNode);
  begin
    if node.OTU then
    begin
      if not node.flag then
        if maxsize = 0 then
        begin
          newroot := node;
          maxsize := 1;
        end;
    end
    else if node.flag then
    begin
      FindRoot0(node.des1);
      FindRoot0(node.des2);
    end
    else if maxsize < node.size then
    begin
      newroot := node;
      maxsize := node.size;
    end;
  end;

  procedure FindRoot1(node: TMLTreeNode);
  begin
    if node.OTU then
    begin
      if node.flag then
        newroot := node;
    end
    else
    begin
      if node.des1.flag then
        FindRoot1(node.des1);
      if node.des2.flag then
        FindRoot1(node.des2);
      if node.des1.flag and node.des2.flag then
        newroot := node;
    end;
  end;

begin
  numFlagged := FlagOutgroupNodes;
  maxsize := 0;
  if not root.flag then
    newroot := root
  else if root.des1.flag and root.des2.flag then
    FindRoot0(root)
  else if root.des1.flag then
    FindRoot1(root.des1)
  else if root.des2.flag then
    FindRoot1(root.des2);
  Assert(Assigned(newroot));
  ChangeRoot(newroot);
end;

procedure TMLTree.ChangeRootOnMidPoint;
var
  newroot: TMLTreeNode = nil;
  i,n: integer;
begin
  n := Root.size;
  for i := NoOfSeqs to 2*NoOfSeqs-2 do
    if node[i] <> root then
      if abs(node[i].size -(Root.size div 2)) < n then
      begin
        newroot := node[i];
        n := abs(node[i].size -(Root.size div 2));
      end;

  ChangeRoot(newroot);
end;

procedure TMLTree.SetNeedsRootedOnOutgroup(AValue: Boolean);
begin
  if FNeedsRootedOnOutgroup = AValue then Exit;
  FNeedsRootedOnOutgroup := AValue;
end;

procedure TMLTree.SetPropagateConstraints(AValue: Boolean);
begin
  FReltimeComputer.PropagateConstraints := AValue;
end;

procedure TMLTree.SubtaskProgress(aProgress: Integer; aStatus: AnsiString);
begin
  if Assigned(SubtaskCheckCancel) then
  begin
    if not FHideSubtaskProgress then
      FCancelled := SubtaskCheckCancel(aProgress, aStatus);
  end;
  if FCancelled then
    Abort;
end;

procedure TMLTree.CheckAbort;
begin
  if FCancelled then
    Abort;
end;

procedure TMLTree.IterateAllBLens;
var
  blen, blen0: array of extended;
  minb, delta: extended;

  procedure IterateOnEachNode(ANode: TMLTreeNode);
  var
    q,q0: TMLTreeNode;
    L,b0,d: extended;
  begin
    if (ANode.anc = root) and (ANode.blen < ANode.sister.blen) then
      blen[ANode.index] := ANode.blen
    else
    begin
      q  := TMLTreeNode.Create(0, false, NoOfSeqs, NoOfSites, Model.NoOfRates, Model.NoOfStates, OrigNumSites);
      q0 := TMLTreeNode.Create(0, false, NoOfSeqs, NoOfSites, Model.NoOfRates, Model.NoOfStates, OrigNumSites);

      q.Assign(ANode);
      q0.Assign(ANode.anc);
      q.anc := q0;
      if ANode = ANode.anc.des1 then
        q0.des1 := q
      else
        q0.des2 := q;

      b0 := ANode.blen;
      if q.blen < minb then
        d := minb*delta
      else
        d := q.blen*delta;

      d := max(d, 1.0E-8);

      L := IterateBlenOfNode(q, FLogLikelihood, d);

      if L > LogLikelihood then
        blen[q.index] := q.blen
      else
        blen[q.index] := b0;

      q.Free;
      q0.Free;
    end;
  end;

  procedure IterateBLens(ANode: TMLTreeNode);
  begin
    if not ANode.OTU then
    begin
      ResetL0OfNode(ANode);

      IterateBLens(ANode.des1);
      IterateBLens(ANode.des2);

      IterateOnEachNode(ANode.des1);
      IterateOnEachNode(ANode.des2);

      if ANode <> root then
        ANode.DeallocNodeMem0;
    end;
  end;

var
  L,L0,dL: extended;
  i,it: integer;
begin
  {$IFDEF DEBUG}WriteToDevLog('begin TMLTree.IterateAllBlens');{$ENDIF}

  setlength(blen, 2*NoOfSeqs-1);
  setlength(blen0, 2*NoOfSeqs-1);

  minb := 1/OrigNumSites/(2*NoOfSeqs-3);
  delta := 1/1000;

  it := 0;
  repeat
    for i := 0 to 2*NoOfSeqs-2 do
    begin
      if Node[i] = Root then
        continue;
      blen0[i] := Node[i].blen;
    end;

    L0 := LogLikelihood;

    IterateBLens(root);

    for i := 0 to 2*NoOfSeqs-2 do
    begin
      if Node[i] = Root then
        continue;
      Node[i].blen := blen[i];
      Model.ComputeProbOfNode(Node[i], Node[i].blen);
    end;

    ResetL1;
    LogLikelihood := ComputeLogLikelihood;

    if LogLikelihood < L0 then
    begin
      for i := 0 to 2*NoOfSeqs-2 do
      begin
        if Node[i] = Root then
          continue;
        Node[i].blen := blen0[i];
        Model.ComputeProbOfNode(Node[i], Node[i].blen);
      end;
      ResetL1;
      LogLikelihood := ComputeLogLikelihood;
    end;

    inc(it);
    dL := LogLikelihood -L0;
  until (dL < deltaL) or (it = MaxIter);

  setlength(blen, 0);
  setlength(blen0, 0);
  {$IFDEF DEBUG}WriteToDevLog('end TMLTree.IterateAllBlens');{$ENDIF}
end;

procedure TMLTree.ResetL0L1OfSite(SiteIndex: integer; IsEP: boolean);

  procedure ComputeL1(ANode: TMLTreeNode);
  var
    i,j,cat: integer;
    p1,p2: PMatrixOfExtended;
    L01,L11,L12: PArrayOfExtended;
    r1,r2: extended;
  begin
    if ANode.OTU then
      exit;
    with ANode do
    begin
      for cat := 0 to length(tmpL1)-1 do
      begin
        L01 := tmpL1[cat];
        L11 := des1.tmpL1[cat];
        L12 := des2.tmpL1[cat];
        p1  := des1.Prob[cat];
        p2  := des2.Prob[cat];
        for i := 0 to NoOfStates-1 do
        begin
          r1 := 0;
          r2 := 0;
          for j := 0 to NoOfStates-1 do
          begin
            r1 := r1 +L11[j]*p1[i,j];
            r2 := r2 +L12[j]*p2[i,j];
          end;
          L01[i] := r1*r2;
        end;

//
        if NoOfSites = 1 then   // For group analysis
        begin
          r1 := 0;
          for i := 0 to NoOfStates-1 do
            r1 := r1 + L01[i];
          if r1 < FP_CUTOFF then
            for i := 0 to NoOfStates-1 do
            begin
              r1 := 0;
              r2 := 0;
              for j := 0 to NoOfStates-1 do
              begin
                r1 := r1 +L11[j]*p1[i,j];
                r2 := r2 +L12[j]*p2[i,j];
              end;
              L01[i] := r1+r2;
            end;
        end;
//

      end;
    end;
  end;

  procedure ComputeL0(ANode: TMLTreeNode);
  var
    cat,i,j: integer;
    p1,p2: PMatrixOfExtended;
    pL0,pL1,pL2: PArrayOfExtended;
    r1: extended;
    r2: array[0..20] of extended;
  begin
    for i := Low(r2) to High(r2) do
      r2[i] := 0; { to get the compiler to stop warning that r2 is not initialized}
    if ANode.anc = nil then
      exit;
    with ANode do
    begin
      if ANode = anc.des1 then
        for cat := 0 to length(L0)-1 do
        begin
          pL0 := tmpL0[cat];
          pL1 := anc.tmpL0[cat];
          pL2 := anc.des2.tmpL1[cat];
          p1  := Prob[cat];
          p2  := anc.des2.Prob[cat];
          for i := 0 to NoOfStates-1 do
          begin
            r2[i] := 0;
            for j := 0 to NoOfStates-1 do
              r2[i] := r2[i] +pL2[j]*p2[i,j];
          end;
          for i := 0 to NoOfStates-1 do
          begin
            r1 := 0;
            for j := 0 to NoOfStates-1 do
              r1 := r1 +r2[j]*pL1[j]*p1[i,j];
            pL0[i] := r1;
          end;
//
          if NoOfSites = 1 then   // For group analysis
          begin
            r1 := 0;
            for i := 0 to NoOfStates-1 do
              r1 := r1 + pL0[i];
            if r1 < FP_CUTOFF then
            begin
              for i := 0 to NoOfStates-1 do
              begin
                r1 := 0;
                for j := 0 to NoOfStates-1 do
                  r1 := r1 +(r2[j]+pL1[j])*p1[i,j];
                pL0[i] := r1;
              end;
            end;
          end;
//
        end
      else // if ANode = anc.des2 then
        for cat := 0 to length(L0)-1 do
        begin
          pL0 := tmpL0[cat];
          pL1 := anc.tmpL0[cat];
          pL2 := anc.des1.tmpL1[cat];
          p1  := Prob[cat];
          p2  := anc.des1.Prob[cat];
          for i := 0 to NoOfStates-1 do
          begin
            r2[i] := 0;
            for j := 0 to NoOfStates-1 do
              r2[i] := r2[i] +pL2[j]*p2[i,j];
          end;
          for i := 0 to NoOfStates-1 do
          begin
            r1 := 0;
            for j := 0 to NoOfStates-1 do
              r1 := r1 +r2[j]*pL1[j]*p1[i,j];
            pL0[i] := r1;
          end;
//
          if NoOfSites = 1 then   // For group analysis
          begin
            r1 := 0;
            for i := 0 to NoOfStates-1 do
              r1 := r1 + pL0[i];
            if r1 < FP_CUTOFF then
            begin
              for i := 0 to NoOfStates-1 do
              begin
                r1 := 0;
                for j := 0 to NoOfStates-1 do
                  r1 := r1 +(r2[j]+pL1[j])*p1[i,j];
                pL0[i] := r1;
              end;
            end;
          end;
//
        end;
    end;
  end;

  procedure ResetL1Recursive(n: TMLTreeNode);
  begin
    if n.OTU then
      exit;

    ResetL1Recursive(n.des1);
    ResetL1Recursive(n.des2);

    ComputeL1(n);
  end;

  procedure ResetL0Recursive(n: TMLTreeNode);
  begin
    if (not IsEP) and n.OTU then
      exit;

    ComputeL0(n);

    if n.OTU then
      exit;

    ResetL0Recursive(n.des1);
    ResetL0Recursive(n.des2);
  end;

var
  i,j,k: integer;
begin
  for i := 0 to NoOfSeqs-1 do
    for j := 0 to Model.NoOfRates-1 do
      for k := 0 to Model.NoOfStates-1 do
        Node[i].tmpL1[j,k] := Node[i].L1[0,Node[i].c1[SiteIndex],k];

  ResetL1Recursive(root.des1);
  ResetL1Recursive(root.des2);

  for j := 0 to Model.NoOfRates-1 do
    for k := 0 to Model.NoOfStates -1 do
      Root.tmpL0[j,k] := 1;

  ResetL0Recursive(root.des1);
  ResetL0Recursive(root.des2);
end;

{$IFNDEF VISUAL_BUILD}
procedure TMLTree.CheckDumpDeveloperData;
var
  filename: String;
  data: TTreeData;
  aList: TTreeList;
begin
  if not IsDeveloper then
    Exit;
  if (DeveloperDataDumpInterval < 0) or (MillisecondsBetween(Now, LastDeveloperDataDumpTime) < DeveloperDataDumpInterval) then
    Exit;
  try
    try
      data := TTreeData.Create(FNoOfSeqs, True, False, False);
      GetTreeData(data);
      aList := TTreeList.Create;
      aList.OTUNameList.AddStrings(DeveloperSeqNamesList);
      aList.Add(data);
      filename := NextAvailableFilenameNV('_temp.nwk');
      aList.ExportToNewickFile(filename, True, False, 0.0);
      LastDeveloperDataDumpTime := Now;
    except
      on E:Exception do
        warn_nv('Exception when dumping developer data: ' + E.Message);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

{$ENDIF}

procedure TMLTree.DoConstruction(origSeqLength: Integer; seqs: TStringList; tree: TTreeData; model: TGammaRateVariationModel; needsOutgroupRoot: Boolean = False);
var
  i: Integer;
begin
  FIsDoingTreeSearch := True;
  FNeedsRootedOnOutgroup := needsOutgroupRoot;
  FDevInitialLogLikelihood := 0.0;
  FHideSubtaskProgress := False;
  FCancelled  := False;
  FSeqs       := seqs;
  FNoOfSeqs   := seqs.Count;
  FNoOfSites  := length(seqs[0]);
  FOrigNumSites := origSeqLength; { OrigNumSites used here because ModelTamer and LBS only keep unique site configurations in SeqData}
  FModel      := model;
//  FdeltaL     := 1.0E-2; // 10E-4;
  FMaxIter    := 5;
  //  Zvalue      := 1.96;

  FNoOfThreads := GetNoOfProcessors;	// KT: default.	no need to subtract one anymore.
  if FNoOfThreads = -1 then { not guaranteed to succeed on Unix systems!}
    FNoOfThreads := 2; { failed to detect but it should be fairly safe to assume at least 2 available processors/cores}
  setlength(Node, 2*FNoOfSeqs);

  for i := 0 to FNoOfSeqs-1 do
    node[i] := TMLTreeNode.Create(i, true, FNoOfSeqs, FNoOfSites, Model.NoOfRates, Model.NoOfStates, FOrigNumSites); { OrigNumSites used here because ModelTamer and LBS only keep unique site configurations in SeqData}
  for i := FNoOfSeqs to 2*FNoOfSeqs-2 do
    node[i] := TMLTreeNode.Create(i, false, FNoOfSeqs, FNoOfSites, Model.NoOfRates, Model.NoOfStates, FOrigNumSites);
  InitOTUConfig;

  FReltimeComputer := TReltimeComputer.Create;

  InitNodeData(tree);

//  InitSiteConfig;

  if Model is TGammaRateVariationModel then
    if not Model.Initialized then
      TGammaRateVariationModel(Model).SetParamsFromSeqs(seqs);
  {
  ResetProb;
  FLogLikelihood := ResetL1;
  }
  FInitialized  := false;

  FSubtaskProgress := 0;
  FSubtaskStatus := EmptyStr;
  SubtaskCheckCancel := nil;
end;

function TMLTree.HasOutgroupInClade(n: TMLTreeNode): Boolean;
begin
  if n.IsOutgroupMember then
    Exit(True);
  Result := False;
  if Assigned(n.des1) then
    Result := Result or HasOutgroupInClade(n.des1);
  if Result then
    Exit(True);
  if Assigned(n.des1) then
    Result := Result or HasOutgroupInClade(n.des2)
end;

function TMLTree.GetProbOfRateCategoryAtSite(index: integer; var p: array of extended): boolean;
var
  Li: extended;
  i,j,k,n: integer;
begin
  result := false;
  k := index;

  ResetL0L1OfSite(index, false);

  if Model.f0[k] = -2 then
  begin
    for i := 0 to Model.NoOfRates-1 do
      p[i] := 0;
    exit;
  end;

  for i := 0 to Model.NoOfRates-1 do
    p[i] := 0;

  for n := NoOfSeqs to 2*NoOfSeqs-2 do
    with Node[n] do
    begin
      Li := 0;
      for j := 0 to Model.NoOfStates-1 do
        Li := Li +tmpL0[0,j]+tmpL1[0,j];

      if Li < Model.NoOfStates then
      begin
        for i := 0 to Model.NoOfRates-1 do
        begin
          Li := 0;
          for j := 0 to Model.NoOfStates-1 do
            Li := Li +tmpL0[i,j]*tmpL1[i,j];

          p[i] := Li;
        end;
        p[Model.NoOfRates] := 0;
        if Model.UseInvar and (Model.f0[k] >= 0) then
          p[Model.NoOfRates] := Model.Invar/Model.NoOfInvarSites*OrigNoOfSites;
          
        break;
      end;
    end;

  Li := 0;
  for i := 0 to Model.NoOfRates-1 do
    Li := Li +p[i];

  if Li > 0  then
  begin
    for i := 0 to Model.NoOfRates-1 do
      p[i] := p[i]/Li*(1 -p[Model.NoOfRates]);
    result := true;
  end
  else
    for i := 0 to Model.NoOfRates-1 do
      p[i] := 0;

end;

function TMLTree.GetMLRateCategoryAtSite(index: integer): integer;
var
  p: array [0..20] of extended;
  pmax: extended;
  i,imax: integer;
begin
  for i := Low(p) to High(p) do
    p[i] := 0; { to get the compiler to stop warning that p is not initialized}
  result := 0;

  GetProbOfRateCategoryAtSite(index, p);

  imax := 0;
  pmax := p[0];
  for i := 1 to Model.NoOfRates do
    if p[i] > pmax then
    begin
      imax := i;
      pmax := p[i];
    end;

  result := imax;
end;

function TMLTree.GetMLRateAtSite(index: integer): extended;
var
  i: integer;
begin
  i := GetMLRateCategoryAtSite(index);
  if i = Model.NoOfRates then
    result := 0
  else
    result := Model.Rate[i];
end;

function TMLTree.GetAncStateProb(NodeIndex, SiteIndex: integer; var state: array of TAncStateRec):boolean;
var
  i,j: integer;
  Li: extended;
  r: extended;
  g: byte;
begin
  for i := Low(FTempAncestralStateProbs) to High(FTempAncestralStateProbs) do
    FTempAncestralStateProbs[i] := 0; { to get the compiler to stop warning that FTempAncestralStateProbs is not initialized}
  result := false;
  if node[nodeindex] = root then exit;

  ResetL0L1OfSite(SiteIndex, false);

  with node[nodeindex] do
    if OTU then
    begin
      Li := 0;
      for j := 0 to Model.NoOfStates-1 do
      begin
        FTempAncestralStateProbs[j] := Model.Freq[j]*tmpL1[0,j];
        Li   := Li +FTempAncestralStateProbs[j];
      end;
      for j := 0 to Model.NoOfStates-1 do
        FTempAncestralStateProbs[j] := FTempAncestralStateProbs[j]/Li;

      r := 0;
      for j := 0 to Model.NoOfStates-1 do
        r := r +tmpL1[0,j];
      result := r < Model.NoOfStates;
    end
    else
    begin
      for j := 0 to Model.NoOfStates-1 do
      begin
        FTempAncestralStateProbs[j] := 0;
        for i := 0 to Model.NoOfRates-1 do
          FTempAncestralStateProbs[j] := FTempAncestralStateProbs[j] +Model.Freq[j]*tmpL0[i,j]*tmpL1[i,j];
      end;

      Li := 0;
      for j := 0 to Model.NoOfStates-1 do
        Li := Li +FTempAncestralStateProbs[j];

      if Li > 0 then {do not use FP_CUTOFF here or our comparison will fail for small values of FTempAncestralStateProbs and Li}
        for j := 0 to Model.NoOfStates-1 do
          FTempAncestralStateProbs[j] := FTempAncestralStateProbs[j]/Li

      else if NoOfSites = 1 then  // For group analysis
      begin
        for j := 0 to Model.NoOfStates-1 do
        begin
          FTempAncestralStateProbs[j] := 0;
          for i := 0 to Model.NoOfRates-1 do
            FTempAncestralStateProbs[j] := FTempAncestralStateProbs[j] +Model.Freq[j]*(tmpL0[i,j]+tmpL1[i,j]);
        end;
        Li := 0;
        for j := 0 to Model.NoOfStates-1 do
          Li := Li +FTempAncestralStateProbs[j];
        if Li > 0 then
          for j := 0 to Model.NoOfStates-1 do
            FTempAncestralStateProbs[j] := FTempAncestralStateProbs[j]/Li
        else
          for j := 0 to Model.NoOfStates-1 do
            FTempAncestralStateProbs[j] := 0;
      end
//
      else
        for j := 0 to Model.NoOfStates-1 do
          FTempAncestralStateProbs[j] := 0;

      g := g0[SiteIndex] and g1[SiteIndex] and g2[SiteIndex];
      if g = 0 then
        g := g0[SiteIndex] or g1[SiteIndex] or g2[SiteIndex];
      result := g <> 2;
    end;

  case Model.SeqDataType of
    DNA:
      begin
        state[0].name := 'A';
        state[1].name := 'T';
        state[2].name := 'C';
        state[3].name := 'G';
        if result then
          for i := 0 to 3 do
            state[i].Prob := FTempAncestralStateProbs[i]
        else
          for i := 0 to 3 do
            state[i].Prob := 0;
      end;
    Protein:
      begin
        state[0].name  := 'A';
        state[1].name  := 'R';
        state[2].name  := 'N';
        state[3].name  := 'D';
        state[4].name  := 'C';
        state[5].name  := 'Q';
        state[6].name  := 'E';
        state[7].name  := 'G';
        state[8].name  := 'H';
        state[9].name  := 'I';
        state[10].name := 'L';
        state[11].name := 'K';
        state[12].name := 'M';
        state[13].name := 'F';
        state[14].name := 'P';
        state[15].name := 'S';
        state[16].name := 'T';
        state[17].name := 'W';
        state[18].name := 'Y';
        state[19].name := 'V';
        if result then
          for i := 0 to 19 do
            state[i].Prob := FTempAncestralStateProbs[i]
        else
          for i := 0 to 19 do
            state[i].Prob := 0;
      end;
    end;
end;

function TMLTree.GetExpectedStateProb(NodeIndex, SiteIndex: integer; var state: array of TAncStateRec):boolean;
var
  i,j: integer;
  Li: extended;
  p: array[0..19] of extended;
  r: extended;
  g: byte;
begin
  for i := Low(p) to High(p) do
    p[i] := 0; { to get the compiler to stop warning that p is not initialized}
  result := false;
  if node[nodeindex] = root then exit;

  ResetL0L1OfSite(SiteIndex, true);

  with node[nodeindex] do
    if OTU then
    begin
      for j := 0 to Model.NoOfStates-1 do
      begin
        p[j] := 0;
        for i := 0 to Model.NoOfRates-1 do
          p[j] := p[j] +Model.Freq[j]*tmpL0[i,j];
      end;

      Li := 0;
      for j := 0 to Model.NoOfStates-1 do
        Li := Li +p[j];

      if Li > 0 then
        for j := 0 to Model.NoOfStates-1 do
          p[j] := p[j]/Li
      else
        for j := 0 to Model.NoOfStates-1 do
          p[j] := 0;

      r := 0;
      for j := 0 to Model.NoOfStates-1 do
        r := r +tmpL1[0,j];
      result := CompareValue(r, Model.NoOfStates, FP_CUTOFF) <= 0;
    end
    else
    begin
      for j := 0 to Model.NoOfStates-1 do
      begin
        p[j] := 0;
        for i := 0 to Model.NoOfRates-1 do
          p[j] := p[j] +Model.Freq[j]*tmpL0[i,j]*tmpL1[i,j];
      end;

      Li := 0;
      for j := 0 to Model.NoOfStates-1 do
        Li := Li +p[j];

      if Li > FP_CUTOFF then
        for j := 0 to Model.NoOfStates-1 do
          p[j] := p[j]/Li
//
      else if NoOfSites = 1 then  // For group analysis
      begin
        for j := 0 to Model.NoOfStates-1 do
        begin
          p[j] := 0;
          for i := 0 to Model.NoOfRates-1 do
            p[j] := p[j] +Model.Freq[j]*(tmpL0[i,j]+tmpL1[i,j]);
        end;
        Li := 0;
        for j := 0 to Model.NoOfStates-1 do
          Li := Li +p[j];
        if Li > 0 then
          for j := 0 to Model.NoOfStates-1 do
            p[j] := p[j]/Li
        else
          for j := 0 to Model.NoOfStates-1 do
            p[j] := 0;
      end
//
      else
        for j := 0 to Model.NoOfStates-1 do
          p[j] := 0;

      g := g0[SiteIndex] and g1[SiteIndex] and g2[SiteIndex];
      if g = 0 then
        g := g0[SiteIndex] or g1[SiteIndex] or g2[SiteIndex];
      result := g <> 2;
    end;

  case Model.SeqDataType of
    DNA:
      begin
        state[0].name := 'A';
        state[1].name := 'T';
        state[2].name := 'C';
        state[3].name := 'G';
        if result then
          for i := 0 to 3 do
            state[i].Prob := p[i]
        else
          for i := 0 to 3 do
            state[i].Prob := 0;
      end;
    Protein:
      begin
        state[0].name  := 'A';
        state[1].name  := 'R';
        state[2].name  := 'N';
        state[3].name  := 'D';
        state[4].name  := 'C';
        state[5].name  := 'Q';
        state[6].name  := 'E';
        state[7].name  := 'G';
        state[8].name  := 'H';
        state[9].name  := 'I';
        state[10].name := 'L';
        state[11].name := 'K';
        state[12].name := 'M';
        state[13].name := 'F';
        state[14].name := 'P';
        state[15].name := 'S';
        state[16].name := 'T';
        state[17].name := 'W';
        state[18].name := 'Y';
        state[19].name := 'V';
        if result then
          for i := 0 to 19 do
            state[i].Prob := p[i]
        else
          for i := 0 to 19 do
            state[i].Prob := 0;
      end;
    end;
end;
{
function TMLTree.GetIsMultiSamplingTimes: Boolean;
begin
  Result := RelTimeComputer.IsSamplingTime;
end;
}
procedure TMLTree.Assign(Source: TMLTree);
var
  i: Integer;
begin
  if Assigned(Source.FSeqs) then
  begin
    FSeqs := TStringList.Create;
    FSeqs.Assign(Source.FSeqs);
  end;
  if Assigned(Source.FModel) then
  begin
    FModel := TGammaRateVariationModel.Create(Source.FModel.gamma, Source.FModel.FUseInvar, Source.FModel.NoOfRates, Source.FModel.NoOfThreads);
    FModel.Assign(Source.FModel);
  end;
  FNoOfSeqs := Source.FNoOfSeqs;
  FNoOfSites := Source.FNoOfSites;
  FLogLikelihood := Source.FLogLikelihood;
  FNoOfThreads := Source.FNoOfThreads;
  FInitialized := Source.FInitialized;
  SetLength(Node, Length(Source.Node));

  if Length(Source.Node) > 0 then
  begin
    for i := 0 to Length(Node) - 1 do
    begin
      if Assigned(Source.Node[i]) then
      begin
        Node[i] := TMLTreeNode.Create(i, Source.Node[i].Otu, FNoOfSeqs, FNoOfSites, FModel.NoOfRates, Source.Node[i].NoOfStates, FOrigNumSites);
        Node[i].Assign(Source.Node[i]);
      end;
    end;
  end;
  Root := Node[Source.Root.Index];
{
  for i := 0 to Length(Node) - 1 do
  begin
    Node[i].cfg := Root.c0;
    Node[i].nc  := nc;
  end;
}
end;

procedure TMLTree.IterateHeightOfNode(ANode: TMLTreeNode; var L: extended; delta: extended);

  function ComputeLikelihood(n: TMLTreeNode): extended;
  begin
    ComputeL1OfNode(n);
//    ComputeL2OfNode(n);

    if assigned(n.anc) then
//      if n = n.anc.des1 then
//        ComputeL1OfNode(n.anc)
//      else
        ComputeL1OfNode(n.anc);

    if assigned(n.anc) then
      result := ComputeLogLikelihoodOfNode(n.anc)
    else
      result := ComputeLogLikelihoodOfNode(n);
    CheckAbort;
  end;

var
  b,d,db,dL,dL0,dL1,dL2,b0,b01,b02: extended;
begin
  dL0 := L;

  if ANode.des1.height < ANode.des2.height then
    b := ANode.height -ANode.des2.height
  else
    b := ANode.height -ANode.des1.height;
  if ANode.anc <> nil then
    if (ANode.anc.height -ANode.height) < b then
      b := ANode.anc.height -ANode.height;

  if b < FP_CUTOFF then
    exit;

  b := b/2;

  b0  := ANode.blen;
  b01 := ANode.des1.blen;
  b02 := ANode.des2.blen;

  d   := b*delta;

  L := dL0;
  if ANode.anc = nil then
  begin
    if (b01 < d) or (b02 < d) then
    begin
      b01 := b01+d;
      b02 := b02+d;
      Model.ComputeProbOfNode(ANode.des1, b01);
      Model.ComputeProbOfNode(ANode.des2, b02);

      L := ComputeLikelihood(ANode);
    end;
  end
  else if b0 < d then
  begin
    b0  := b0 +d;
    b01 := b01-d;
    b02 := b02-d;
    Model.ComputeProbOfNode(ANode, b0+d);
    Model.ComputeProbOfNode(ANode.des1, b01);
    Model.ComputeProbOfNode(ANode.des2, b02);

    L := ComputeLikelihood(ANode);
  end
  else if (b01 < d) or (b02 < d) then
  begin
    b0  := b0 -d;
    b01 := b01+d;
    b02 := b02+d;
    Model.ComputeProbOfNode(ANode, b0);
    Model.ComputeProbOfNode(ANode.des1, b01);
    Model.ComputeProbOfNode(ANode.des2, b02);

    L := ComputeLikelihood(ANode);
  end;
  if L > dL0 then
  begin
    ANode.blen      := b0;
    ANode.des1.blen := b01;
    ANode.des2.blen := b02;
    ANode.height    := ( (ANode.des1.height +ANode.des1.blen)
                        +(ANode.des2.height +ANode.des2.blen))/2;
  end;
  dL0 := L;

  Model.ComputeProbOfNode(ANode.des1, (b01+d));
  Model.ComputeProbOfNode(ANode.des2, (b02+d));
  if assigned(ANode.anc) then
    Model.ComputeProbOfNode(ANode, (b0-d));
  dL1 := ComputeLikelihood(ANode);

  Model.ComputeProbOfNode(ANode.des1, (b01-d));
  Model.ComputeProbOfNode(ANode.des2, (b02-d));
  if assigned(ANode.anc) then
    Model.ComputeProbOfNode(ANode, (b0+d));
  dL2 := ComputeLikelihood(ANode);

  dL := (dL1-dL0)-(dL0-dL2);
  if dL < 0 then
  begin
    db := d*(dL1-dL2)/2/dL;

    if (ANode.anc <> nil) then
      if b0 < -db then
        db := -b0;
    if b01 < db then
      db := b01;
    if b02 < db then
      db := b02;
  end
  else
    db := 0;

  if assigned(ANode.anc) then
  begin
    b0 := b0 +db;
    Model.ComputeProbOfNode(ANode, b0);
  end;
  b01 := b01 -db;
  b02 := b02 -db;
  Model.ComputeProbOfNode(ANode.des1, b01);
  Model.ComputeProbOfNode(ANode.des2, b02);

  L := ComputeLikelihood(ANode);

  if L > dL0 then
  begin
    ANode.des1.blen := b01;
    ANode.des2.blen := b02;
    if assigned(ANode.anc) then
      ANode.blen := b0;
  end
  else
  begin
    if assigned(ANode.anc) then
      Model.ComputeProbOfNode(ANode, ANode.blen);
    Model.ComputeProbOfNode(ANode.des1, ANode.des1.blen);
    Model.ComputeProbOfNode(ANode.des2, ANode.des2.blen);
    L := ComputeLikelihood(ANode);
  end;

  ANode.height := ( (ANode.des1.height +ANode.des1.blen)
                   +(ANode.des2.height +ANode.des2.blen))/2;

end;

function TMLTree.IterateAllHeights(reverseorder: boolean; d: extended): integer;

  procedure IterateHeightRecursiveUp(ANode: TMLTreeNode);
  begin
    if ANode.OTU then
    begin
      inc(result);
      exit;
    end;

    ComputeL0OfNode(ANode);

    if reverseorder then
    begin
      IterateHeightRecursiveUp(ANode.des2);
      IterateHeightRecursiveUp(ANode.des1);
    end
    else
    begin
      IterateHeightRecursiveUp(ANode.des1);
      IterateHeightRecursiveUp(ANode.des2);
    end;

    IterateHeightOfNode(ANode, FLogLikelihood, d);

    if not ANode.OTU then
      ANode.DeallocNodeMem0;
  end;

  procedure IterateHeightRecursiveDown(ANode: TMLTreeNode);
  begin
    if ANode.OTU then
    begin
      inc(result);
      exit;
    end;

    IterateHeightOfNode(ANode, FLogLikelihood, d);

    ComputeL0OfNode(ANode);

    if reverseorder then
    begin
      IterateHeightRecursiveDown(ANode.des2);
      IterateHeightRecursiveDown(ANode.des1);
    end
    else
    begin
      IterateHeightRecursiveDown(ANode.des1);
      IterateHeightRecursiveDown(ANode.des2);
    end;

    ANode.DeallocNodeMem0;
  end;

  procedure ResetNodeHeight(ANode: TMLTreeNode; r: extended);
  begin
    if ANode.OTU then
    begin
      ANode.height := 0;
      exit;
    end;

    ResetNodeHeight(ANode.des1, r);
    ResetNodeHeight(ANode.des2, r);

    ANode.height := ANode.height*r;

    ANode.des1.blen := ANode.height - ANode.des1.height;
    ANode.des2.blen := ANode.height - ANode.des2.height;
  end;

  procedure IterateTreeSize;
  var
    dL,dL0,dL1,dL2,dd: extended;
   begin
    dL0 := FLogLikelihood;

    ResetNodeHeight(Root, 1+d);
    ResetProb;
    dL1 := ResetL1;

    ResetNodeHeight(Root, (1-d)/(1+d));
    ResetProb;
    dL2 := ResetL1;

    dL := (dL1-dL0)-(dL0-dL2);
    if dL < 0 then
      dd := -d*(dL1-dL2)/2/dL
    else
      dd := 0;

    if dd < -0.1 then
      dd := -0.1
    else if dd > 0.1 then
      dd := 0.1;

    ResetNodeHeight(Root, (1+dd)/(1-d));
    ResetProb;
    FLogLikelihood := ResetL1;
  end;

begin
  result := 0;
  IterateHeightRecursiveUp(Root);
//  IterateHeightRecursiveDown(Root);
  IterateTreeSize;
end;

procedure TMLTree.MakeNonClockTree(tree: TTreeData);
var
  i: integer;
begin
  InitNodeData(tree);

  if not Initialized then
    Initialize(true, true);

  OptimizeParameters;

  for i := NoOfSeqs to 2*NoOfSeqs-2 do
    node[i].ase := sqrt(abs(node[i].vb));
end;
{
procedure ConstrainMinMaxHeight(n: TMLTreeNode);
var
  h1,h2: extended;
begin
  if n.anc.maxh+FP_CUTOFF > n.anc.height then
    if n.maxh > n.anc.maxh then
      n.maxh := n.anc.maxh;

  if n.OTU then exit;

  ConstrainMinMaxHeight(n.des1);
  ConstrainMinMaxHeight(n.des2);

  if n.minh < n.des1.minh then
    n.minh := n.des1.minh;
  if n.minh < n.des2.minh then
    n.minh := n.des2.minh;
end;
}
function TMLTree.MakeClockTree(tree: TTreeData;
                               IsGlobalClock: boolean;
                               ClockLevel: extended;
                               Cancel: TCheckCancelFunc;
                               MaxRateRatio: Extended;
                               var SamplingTime: array of extended): integer;
var
  OriginalTree: TTreeData;
  RelTimeTree: TTreeData;



  function CheckSamplingTime: boolean;
  var
    s: extended;
    i: integer;
  begin
    s := 0;
    for i := 0 to NoOfSeqs-1 do
      s := s + SamplingTime[i];
    result := s > FP_CUTOFF;
  end;

  procedure SetNodeHeight(node: TMLTreeNode);
  var
    h0,h1: extended;
  begin
    if node.OTU then
    begin
      node.height := 0;
      exit;
    end;

    SetNodeHeight(node.des1);
    SetNodeHeight(node.des2);

    h0 :=     ( (node.des1.height + node.des1.blen)
               +(node.des2.height + node.des2.blen))/2;
    h1 := sqrt( (node.des1.height + node.des1.blen)
               *(node.des2.height + node.des2.blen));

    node.height := max(h0, h1);
    if node.height - node.des1.height < 1/NoOfSites then
      node.height := node.des1.height + 1/NoOfSites;
    if node.height - node.des2.height < 1/NoOfSites then
      node.height := node.des2.height + 1/NoOfSites;

    node.des1.blen := node.height - node.des1.height;
    node.des2.blen := node.height - node.des2.height;
  end;

var
  i,it: integer;
  d,dL0: extended;
  aMaxIter: Integer;
begin
  aMaxIter := MaxIter*10;
  result := 0;
  OriginalTree := nil;
  RelTimeTree  := nil;
  ReltimeComputer.MaxRateRatio := MaxRateRatio;
  try
    OriginalTree := TTreeData.Create(NoOfSeqs, true,  true, false);
    GetTreeData(OriginalTree);
    if not OriginalTree.Identical(Tree) then
    begin
      OriginalTree.Assign(Tree);
      InitNodeData(OriginalTree);
      Initialize(false, true);
    end
    else
    begin
      for i := 0 to NoOfSeqs-1 do
        OriginalTree.IsOutgroupMember[i] := Tree.IsOutgroupMember[i];
      OriginalTree.AssignDataCoverage(Tree);
    end;
    if FNeedsRootedOnOutgroup then
      ChangeRootByOutgroup
    else
      ReltimeComputer.NeedsRootByOutgroup := False;

    if CheckSamplingTime then
      RelTimeTree := ReltimeComputer.ComputeRelTimeBLens(OriginalTree, SamplingTime, MaxRateRatio, OtuNamesList)
    else
      RelTimeTree := ReltimeComputer.ComputeRelTimeBLens(OriginalTree, MaxRateRatio);
    if not Assigned(RelTimeTree) then
      raise Exception.Create('Reltime analysis failed: ' + ReltimeComputer.Log.Text);

    if IsGlobalClock then
    begin
      SetNodeHeight(root);
      Initialize(true, true);

      d := 1/1000;
      it := 0;
      repeat
        dL0 := FLogLikelihood;

        result := IterateAllHeights(it mod 6 < 3, d);

        if (it mod 4) = 0 then
          OptimizeParameters;

//        if (abs(FLogLikelihood-dL0) < d) and (d > FP_CUTOFF) then
//          d := d/10;

        inc(it);

        if assigned(Cancel) then
          if Cancel(round(it/aMaxIter*100), 'Optimizing ML tree') then
            it := aMaxIter;

      until (abs(FLogLikelihood-dL0) < deltaL) or (it = aMaxIter);

      OptimizeParameters;

      if assigned(Cancel) then
        Cancel(100, 'Optimizing ML tree');

      GetTreeData(tree);
    end
    else
    begin
      InitNodeData(RelTimeTree);
      Initialize(true, true);
      GetTreeData(tree);
      for i := 0 to 2*NoOfSeqs-2 do
      begin
        if Node[i].anc = nil then
          continue;
        tree.BLen[i] := (RelTimeComputer.Node[i].anc.height - RelTimeComputer.Node[i].height);
        tree.SE[i]   := sqrt(RelTimeComputer.Node[i].vh);
      end;
      for i := 0 to NoOfSeqs - 2 do
        tree.DataCoverage[i] := RelTimeComputer.Node[i + NoOfSeqs].DataCoverage;
    end;

  finally
    ReltimeComputer.NeedsRootByOutgroup := True;
    if assigned(RelTimeTree) then
      RelTimeTree.Free;
    if assigned(OriginalTree) then
      OriginalTree.Free;
  end;
end;

function TMLTree.AnchorClockTree(var minTime, maxTime: array of extended; MaxRateRatio: Extended): boolean;

  function PreventOutgroupCalibrations: Boolean;
  var
    i: Integer;

    function NodeIsInOutGroup(NodeIndex: Integer): Boolean;

      function NodeIsInOutgroupRecursive(ANode: Integer): Boolean;
      begin
        Result := False;
        if Node[ANode].OTU then
        begin
          if Node[ANode].IsOutgroupMember then
            Result := True;
          exit;
        end;
        Result := Result or NodeIsInOutgroupRecursive(Node[ANode].des1.index);
        if not Result then // we can stop early if we already find out
          Result := Result or NodeIsInOutgroupRecursive(Node[ANode].des2.index);
      end;
    begin
      Result := False;
      Result := Result or NodeIsInOutgroupRecursive(NodeIndex);
    end;

  begin
    Result := True;
    for i := FNoOfSeqs to (FNoOfSeqs * 2 - 2) do
    begin
      if Node[i].anc = nil then // the root node
        continue;
      if NodeIsInOutGroup(i) and ((MinTime[i] > 0) or (MaxTime[i] > 0)) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;

var
  RelTimeTree: TTreeData;
begin
  result := false;
  Assert(PreventOutgroupCalibrations, 'Failed to validate calibrations. This is a bug'); // we don't want to use calibrations that are in the outgroup and they should have not been allowed
  RelTimeTree := nil;
  try
    RelTimeTree := ReltimeComputer.AnchorClockTree(minTime, maxTime);
    result := assigned(RelTimeTree);
  finally
    if assigned(RelTimeTree) then
      RelTimeTree.Free;
  end;
end;

function TMLTree.AnchorClockTree(tree: TTreeData; var minTime,
  maxTime: array of extended; MaxRateRatio: Extended): boolean;
var
  i: integer;
  OriginalTree: TTreeData;
  RelTimeTree: TTreeData;
begin
  result := false;

  OriginalTree := nil;
  RelTimeTree  := nil;
  try
    OriginalTree := TTreeData.Create(NoOfSeqs, true,  true, false);
    GetTreeData(OriginalTree);
    if not OriginalTree.Identical(Tree) then
    begin
      OriginalTree.Assign(Tree);
      InitNodeData(OriginalTree);
      Initialize(false, true);
    end
    else
    begin
      for i := 0 to NoOfSeqs-1 do
        OriginalTree.IsOutgroupMember[i] := Tree.IsOutgroupMember[i];
      OriginalTree.AssignDataCoverage(Tree);
    end;
    ChangeRootByOutgroup;

    RelTimeTree := ReltimeComputer.ComputeRelTimeBLens(OriginalTree, MaxRateRatio);
    if assigned(RelTimeTree) then
      result := AnchorClockTree(minTime, maxTime, MaxRateRatio);

    if not result then exit;

    GetTreeData(tree);
    for i := 0 to 2*NoOfSeqs-2 do
    begin
      if Node[i].anc = nil then
        continue;
      tree.BLen[i] := (ReltimeComputer.Node[i].anc.height - ReltimeComputer.Node[i].height);
      tree.SE[i]   := sqrt(ReltimeComputer.Node[i].vh);
    end;
    for i := 0 to NoOfSeqs - 2 do
      tree.DataCoverage[i] := ReltimeComputer.Node[i + NoOfSeqs].DataCoverage;
  finally
    if assigned(RelTimeTree) then
      RelTimeTree.Free;
    if assigned(OriginalTree) then
      OriginalTree.Free;
  end;
end;

function TMLTree.NodeHeight(index: integer): extended;
begin
  result := ReltimeComputer.Node[index].height;
end;

function TMLTree.DebugHeaderString: String;
begin
  Result := Format('%6s', ['index']) + #9;
  Result := Result + Format('%4s', ['anc']) + #9;
  Result := Result + Format('%4s', ['des1']) + #9;
  Result := Result + Format('%4s', ['des2']) + #9;
  Result := Result + Format('%6s', ['depth']) + #9;
  Result := Result + Format('%5s', ['blen']) + #9;
  Result := Result + Format('%5s', ['rate']) + #9;
  Result := Result + Format('%5s', ['Ixx']) + #9;
  Result := Result + Format('%5s', ['vh']) + #9;
  Result := Result + Format('%8s', ['hse']) + #9;
  Result := Result + Format('%5s', ['vb']) + #9;
  Result := Result + Format('%6s', ['height']) + #9;
end;

function TMLTree.DebugStrings: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  Result.Add(DebugHeaderString);
  for i := 0 to (2 * NoOfSeqs - 2) do
    Result.Add(Node[i].DebugString);
end;

function TMLTree.ExportDebugStrings(AName: String): Boolean;
{$IFNDEF VISUAL_BUILD}
var
  AList: TStringList = nil;
  Filename: String;
{$ENDIF}
begin
  Result := False;
  {$IFNDEF VISUAL_BUILD}
  try
    try
      AList := DebugStrings;
      Filename := NextAvailableFilenameNV(AName + '.txt');
      AList.SaveToFile(Filename);
      Result := FileExists(Filename);
    except
      on E:Exception do
        error_nv('Error when exporting debugstrings: ' + E.Message, E);
    end;
  finally
    if Assigned(DebugStrings) then
      DebugStrings.Free;
  end;
  {$ENDIF}
end;


{
function TMLTree.OptimizeDistance(var L1, L2: array of PMatrixOfExtended; d: extended): extended;
var
  P: PMatrixOfExtended;

  function ComputeLikelihood: extended;
  var
    Lk: array[0..19] of extended;
    Li: extended;
    i,j,k,j1: integer;
  begin
    result := 0;
    for k := 1 to NoOfSites do
    begin
      Li := 0;
      for i := 0 to Model.NoOfRates-1 do
        for j := 0 to Model.NoOfStates-1 do
        begin
          Lk[j] := 0;
          for j1 := 0 to Model.NoOfStates-1 do
            Lk[j] := Lk[j] +L1[i,k,j1]*P[j,j1];
          Li := Li +Model.Freq[j]*Lk[j]*L2[i,k,j];
        end;
      Li := Li/Model.NoOfRates*(1-Model.Invar);

      if Model.UseInvar and (Model.NoOfInvarSites > 0) then
        if f0[k] >= 0 then
          Li := Li +Model.Freq[f0[k]]*Model.Invar;

      if Li > 0 then
        if Model.BootTable <> nil then
          result := result +ln(Li)*Model.BootTable[k]
        else
          result := result +ln(Li)
      else
        result := result -11399; // ln(3.6E-4951) = -11398.8
    end;
  end;

var
  L,dL,dL0,dL1,dL2,d0,delta,dd: extended;
  it: integer;
begin
  CreateMatrix(P, 20);
  delta := 0.000001;

  it := 0;
  d0 := d;
  repeat
    if d < delta then
      d := delta;

    Model.ComputeProb(P, d);
    dL0 := ComputeLikelihood;

    Model.ComputeProb(P, (d+delta));
    dL1 := ComputeLikelihood;

    Model.ComputeProb(P, (d-delta));
    dL2 := ComputeLikelihood;

    dL := (dL1-2*dL0+dL2);
    if (abs(dL) > 0) then
    begin
      dd := delta*(dL1-dL2)/2/dL;
      if d < dd then
        d := 0
      else
        d := d -dd;
    end;
    if abs(dd) < delta then
      delta := dd/10;

    Model.ComputeProb(P, d);
    L := ComputeLikelihood;

    if L < dL0 then
    begin
      d := d0;
      Model.ComputeProb(P, d);

      L := ComputeLikelihood;
    end;
    inc(it);
  until (abs(L-dL0) < deltaL) or (it = MaxIter);

  result := d;

  DestroyMatrix(P, 20);
end;

procedure TMLTree.MakeInitialTree(tree: TTreeData);
var
  AMIN, BMIN: integer;
  H, C: PArrayOfInt;
  R: PArrayOfDouble;
  D: PDistanceMatrix;
  M: PMatrixOfExtended;

  procedure ChooseNeighbor(N : integer);
  var S,sij : double;
      i,j,k : integer;
  begin
    for i := 0 to N-1 do
    begin
      R[i] := 0.0;
      if i > 0 then
        for j := 0 to i-1 do
          R[i] := R[i] + D[H[i]][H[j]];
      if i < (N-1) then
        for j := i+1 to N-1 do
          R[i] := R[i] + D[H[j]][H[i]];
    end;

    AMIN := 0;
    BMIN := 1;
    S := (N - 2)*D[H[1]][H[0]] -R[0] -R[1];
    for i := 1 to N-1 do
      for j := 0 to i-1 do
      begin
        sij := (N - 2)*D[H[i]][H[j]] -R[i] -R[j];
        if sij <= S then
        begin
          S := sij;
          AMIN := j;
          BMIN := i;
        end;
      end;
  end;

  procedure MakeNode(N : integer);
  begin
    Tree.NodeArray[Tree.NoOfOTUs-N].des1 := C[AMIN];
    Tree.NodeArray[Tree.NoOfOTUs-N].des2 := C[BMIN];
    if N = 2 then
    begin
      Tree.BlenArray[C[AMIN]] := D[H[1]][H[0]]/2;
      Tree.BlenArray[C[BMIN]] := D[H[1]][H[0]]/2;
    end
    else
    begin
      Tree.BlenArray[C[AMIN]] := ((N-2)*D[H[BMIN]][H[AMIN]] +R[AMIN] -R[BMIN])/(N-2)/2;
      Tree.BlenArray[C[BMIN]] := ((N-2)*D[H[BMIN]][H[AMIN]] -R[AMIN] +R[BMIN])/(N-2)/2;
    end;
  end;

  procedure MatrixReconst(N : integer);
  var
    i,j,k,i1,j1,n1,n2 : integer;
    b0,b1,b2,s,dd : extended;
  begin
    if N = 2 then exit;

    s := 0;
    for i := 1 to N-1 do
    begin
      if i = AMIN then continue;
      if i = BMIN then continue;
      for j := 0 to i-1 do
      begin
        if j = AMIN then continue;
        if j = BMIN then continue;
        s := s +D[H[i],H[j]];
      end;
    end;


    for k := 1 to NoOfsites do
      for i := 0 to Model.NoOfRates-1 do
      begin
        for j := 0 to Model.NoOfStates-1 do
          node[2*Tree.NoOfOTUs-2].L0[i,k,j] := 1;

        for i1 := 0 to N-1 do
        begin
          if i1 = AMIN then continue;
          if i1 = BMIN then continue;

          for j := 0 to Model.NoOfStates-1 do
          begin
            node[2*Tree.NoOfOTUs-2].L1[i,k,j] := 0;
            for j1 := 0 to Model.NoOfStates-1 do
              node[2*Tree.NoOfOTUs-2].L1[i,k,j] := node[2*Tree.NoOfOTUs-2].L1[i,k,j] +node[C[i1]].L1[i,k,j1]*node[C[i1]].L2[i,k,j1]*node[C[i1]].Prob[i][j,j1];
          end;
          for j := 0 to Model.NoOfStates-1 do
            node[2*Tree.NoOfOTUs-2].L0[i,k,j] := node[2*Tree.NoOfOTUs-2].L0[i,k,j]*node[2*Tree.NoOfOTUs-2].L1[i,k,j];
        end;
      end;

    if N = 3 then
      b0 := (R[AMIN] +R[BMIN] -3*D[H[BMIN]][H[AMIN]])/2
    else
      b0 := (R[AMIN] +R[BMIN] -N*D[H[BMIN]][H[AMIN]] -2*s/(N-3))/(N-2)/2;
    b1 := Tree.BlenArray[C[AMIN]];
    b2 := Tree.BlenArray[C[BMIN]];

    if b0 < 0 then b0 := 0;
    if b1 < 0 then b1 := 0;
    if b2 < 0 then b2 := 0;

    node[C[AMIN]].blen := b1;
    ResetProbOfNode(node[C[AMIN]]);
    node[C[BMIN]].blen := b2;
    ResetProbOfNode(node[C[BMIN]]);
    node[2*Tree.NoOfOTUs-N].blen := b0;
    ResetProbOfNode(node[2*Tree.NoOfOTUs-N]);

    for k := 1 to NoOfsites do
      for i := 0 to Model.NoOfRates-1 do
      begin
        for j := 0 to Model.NoOfStates-1 do
        begin
          node[2*Tree.NoOfOTUs-N].L1[i,k,j] := 0;
          for j1 := 0 to Model.NoOfStates-1 do
            node[2*Tree.NoOfOTUs-N].L1[i,k,j] := node[2*Tree.NoOfOTUs-N].L1[i,k,j] +node[C[AMIN]].L1[i,k,j1]*node[C[AMIN]].L2[i,k,j1]*node[C[AMIN]].Prob[i][j,j1];
        end;
        for j := 0 to Model.NoOfStates-1 do
        begin
          node[2*Tree.NoOfOTUs-N].L2[i,k,j] := 0;
          for j1 := 0 to Model.NoOfStates-1 do
            node[2*Tree.NoOfOTUs-N].L2[i,k,j] := node[2*Tree.NoOfOTUs-N].L2[i,k,j] +node[C[BMIN]].L1[i,k,j1]*node[C[BMIN]].L2[i,k,j1]*node[C[BMIN]].Prob[i][j,j1];
        end;
        for j := 0 to Model.NoOfStates-1 do
        begin
          node[2*Tree.NoOfOTUs-N].L0[i,k,j] := 0;
          for j1 := 0 to Model.NoOfStates-1 do
            node[2*Tree.NoOfOTUs-N].L0[i,k,j] := node[2*Tree.NoOfOTUs-N].L0[i,k,j] +node[2*Tree.NoOfOTUs-2].L0[i,k,j1]*node[2*Tree.NoOfOTUs-N].Prob[i][j,j1];
        end;

        for j := 0 to Model.NoOfStates-1 do
          node[2*Tree.NoOfOTUs-N].L0[i,k,j] := node[2*Tree.NoOfOTUs-N].L0[i,k,j]*node[2*Tree.NoOfOTUs-N].L1[i,k,j]*node[2*Tree.NoOfOTUs-N].L2[i,k,j];

        s := 0;
        for j := 0 to Model.NoOfStates-1 do
          s := s +node[2*Tree.NoOfOTUs-N].L0[i,k,j];
        if s > 1.000000000001 then
          for j := 0 to Model.NoOfStates-1 do
            node[2*Tree.NoOfOTUs-N].L0[i,k,j] := 0
        else
          for j := 0 to Model.NoOfStates-1 do
            node[2*Tree.NoOfOTUs-N].L0[i,k,j] := node[2*Tree.NoOfOTUs-N].L0[i,k,j]/s;
      end;

    for i := 0 to N-1 do
    begin
      if i = AMIN then continue;
      if i = BMIN then continue;
      for n1 := 0 to Model.NoOfStates-1 do
        for n2 := 0 to Model.NoOfStates-1 do
          m[n1,n2] := 0;

      for k := 1 to NoOfsites do
      begin
        if Model.BootTable = nil then
          dd := 1
        else if Model.BootTable[k] = 0 then
          continue
        else
          dd := Model.BootTable[k];

        for n1 := 0 to Model.NoOfStates-1 do
          for n2 := 0 to Model.NoOfStates-1 do
            for j := 0 to Model.NoOfRates-1 do
              if C[i] < NoOfSeqs then
              begin
                s := 0;
                for j1 := 0 to Model.NoOfStates-1 do
                  s := s +node[C[i]].L1[j,k,j1];
                if s < 1.000000000001 then
                  m[n1,n2] := m[n1,n2] +node[2*Tree.NoOfOTUs-N].L0[j,k,n1]*node[C[i]].L1[j,k,n2]*dd;
              end
              else
                m[n1,n2] := m[n1,n2] +node[2*Tree.NoOfOTUs-N].L0[j,k,n1]*node[C[i]].L0[j,k,n2]*dd;
      end;

      if i < AMIN then
        D[H[AMIN]][H[i]] := Model.ComputeDistance(m)
      else
        D[H[i]][H[AMIN]] := Model.ComputeDistance(m);
    end;

    C[AMIN] := 2*Tree.NoOfOTUs-N;
    if BMIN < N-1 then
      for i := BMIN to N-2 do
      begin
        H[i] := H[i+1];
        C[i] := C[i+1];
      end;
  end;

var
  i,j,k,i1,j1 : integer;
  s: extended;
begin
  GetMem(R, SizeOf(double)*Tree.NoOfOTUs);
  GetMem(H, SizeOf(Integer)*Tree.NoOfOTUs);
  GetMem(C, SizeOf(Integer)*Tree.NoOfOTUs);

  CreateMatrix(M, Model.NoOfStates);
  D := CreateDistanceMatrix(NoOfSeqs);

  Model.ComputeDistanceMatrix(seqs, D);

  for i := 0 to Tree.NoOfOTUs-1 do
    H[i] := i;
  for i := 0 to Tree.NoOfOTUs-1 do
    C[i] := i;

  s := 0;
  for i := 0 to Tree.NoOfOTUs-1 do
  begin
    R[i] := 0.0;
    if i > 0 then
      for j := 0 to i-1 do
        R[i] := R[i] + D[H[i]][H[j]];
    if i < (Tree.NoOfOTUs-1) then
      for j := i+1 to Tree.NoOfOTUs-1 do
        R[i] := R[i] + D[H[j]][H[i]];
    s := s +R[i];
  end;

  for i := 0 to Tree.NoOfOTUs-1 do
    node[i].blen := (R[i] -s/2/(Tree.NoOfOTUs-1))/(Tree.NoOfOTUs-2);
  for i := 0 to Tree.NoOfOTUs-1 do
    ResetProbOfNode(node[i]);

  for k := 1 to NoOfsites do
    for i := 0 to Model.NoOfRates-1 do
    begin
      for j := 0 to Model.NoOfStates-1 do
        node[2*Tree.NoOfOTUs-2].L0[i,k,j] := 1;

      for i1 := 0 to NoOfSeqs-1 do
      begin
        for j := 0 to Model.NoOfStates-1 do
        begin
          node[2*Tree.NoOfOTUs-2].L1[i,k,j] := 0;
          for j1 := 0 to Model.NoOfStates-1 do
            node[2*Tree.NoOfOTUs-2].L1[i,k,j] := node[2*Tree.NoOfOTUs-2].L1[i,k,j] +node[i1].L1[i,k,j1]*node[i1].Prob[i][j,j1];
        end;
        for j := 0 to Model.NoOfStates-1 do
          node[2*Tree.NoOfOTUs-2].L0[i,k,j] := node[2*Tree.NoOfOTUs-2].L0[i,k,j]*node[2*Tree.NoOfOTUs-2].L1[i,k,j];
      end;
    end;

  for i := Tree.NoOfOTUs downto 2 do
  begin
    ChooseNeighbor(i);
    MakeNode(i);
    MatrixReconst(i);
  end;

  for i := 1 to Tree.NoOfOTUs-1 do
    for j := 0 to i-1 do
      D[i][j] := D[j][i];

  DestroyMatrix(M, Model.NoOfStates);
  DestroyDistanceMatrix(D, NoOfSeqs);

  FreeMem(R);
  FreeMem(H);
  FreeMem(C);

end;
}

function TMLTree.DescString: AnsiString;
var
  MyString: AnsiString;
begin
  MyString := '[TMLTree.ToString()] ' + LineEnding;

  MyString := MyString + '[NoOfSites] ' + IntToStr(NoOfSites) + LineEnding;
  MyString := MyString + '[NoOfSeqs] ' + IntToStr(NoOfSeqs) + LineEnding;
  MyString := MyString + '[MaxIter] ' + IntToStr(MaxIter) + LineEnding;
  MyString := MyString + '[NoOfThreads] ' + IntToStr(NoOfThreads) + LineEnding;

  MyString := MyString + '[LogLikelihood] ' + FloatToStrF(LogLikelihood, ffFixed, 5, 5) + LineEnding;
  MyString := MyString + '[deltaL] ' + FloatToStrF(deltaL, ffFixed, 5, 5) + LineEnding;

  MyString := MyString + '[TGammaRateVariationModel] need toString for model' + LineEnding;
  MyString := MyString + '[Seqs] need toString sequences' + LineEnding;
  Result := MyString;
end;

function TMLTree.StateToStringList(Comment: AnsiString=''): TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  Result.Add('TMLTree=MyName');

  if Comment <> '' then
    Result.Add('TMLTree.Comment=' + Comment);

  Result.Add('TMLTree.NoOfSites=' + IntToStr(NoOfSites));
  Result.Add('TMLTree.NoOfSeqs=' + IntToStr(NoOfSeqs));
  Result.Add('TMLTree.MaxIter=' + IntToStr(MaxIter));
  Result.Add('TMLTree.NoOfThreads=' + IntToStr(NoOfThreads));

  Result.Add('TMLTree.LogLikelihood=' + FloatToStrF(LogLikelihood, ffFixed, 5, 5));
  Result.Add('TMLTree.deltaL=' + FloatToStrF(deltaL, ffFixed, 5, 5));

  Result.Add('TMLTree.TGammaRateVariationModel=need StateToStringList for model');
  Result.Add('TMLTree.Seqs=need StateToStringList for sequences');
  for i := Low(Node) to High(Node) do
    if Assigned(Node[i]) then
      Result.Add(Node[i].DebugString)
    else
      Result.Add(Format('node[%d] is nil', [i]));
end;

{$IFDEF DEBUG}{$IFNDEF VISUAL_BUILD}
function TMLTree.DumpSiteConfigData(filename: String): Boolean;
var
  i: Integer = -1;
  site: Integer = -1;
  n: TMLTreeNode = nil;
  aFile: TextFile;
begin
  try
    try
      AssignFile(aFile, filename);
      Rewrite(aFile);
      WriteLn(aFile, Format('Num Configs = %d', [GetNoOfConfs]));
      Write(aFile, 'site');
      for i := 1 to FNoOfSites do
        Write(aFile, Format(',%d',[i]));
      WriteLn(aFile);
      for i := 0 to 2*NoOfSeqs-2 do
      begin
        n := Node[i];
        if n.OTU then
          WriteLn(aFile, Format('Node %d (OTU), nc0=%d, nc1=%d', [n.index, n.nc0, n.nc1]))
        else if n = Root then
          WriteLn(aFile, Format('Root,%d,%d,nc0=%d,nc1=%d', [n.des1.index, n.des2.index, n.nc0, n.nc1]))
        else
          WriteLn(aFile, Format('Node %d,%d,%d,nc0=%d,nc1=%d', [n.index, n.des1.index, n.des2.index, n.nc0, n.nc1]));
        if n.OTU then
          Write(aFile, 'c0=base(ATCG ->1 2 3 4)')
        else
          Write(aFile, 'c0=combined');
        for site := 1 to FNoOfSites do
          Write(aFile, Format(',%d', [n.c0[site]]));
        WriteLn(aFile);

        Write(aFile, 'cs0=combined');
        for site := 1 to n.nc0 do
          Write(aFile, Format(',%d', [n.cs0[site]]));
        WriteLn(aFile);

        Write(aFile, 'c1=config Id for each site');
        for site := 1 to FNoOfSites do
          Write(aFile, Format(',%d', [n.c1[site]]));
        WriteLn(aFile);

        Write(aFile, 'cs1=config location');
        for site := 1 to n.nc1 do
          Write(aFile, Format(',%d', [n.cs1[site]]));
        WriteLn(aFile);

      end;
      WriteLn(aFile);
      Write(aFile,'nc=config counts');
      for site := 1 to GetNoOfConfs do
        Write(aFile, Format(',%d', [nc[site]]));
    except
      on E:Exception do
        error_nv('failed to save site config data', E);
    end;
  finally
    CloseFile(aFile);
  end;
  Result := FileExists(filename);
end;
{$ENDIF}{$ENDIF}

function TMLTree.StateToFile(filename: String; Comment: AnsiString): Boolean;
var
  aList: TStringList = nil;
begin
  Result := False;
  try
    aList := StateToStringList(Comment);
    aList.SaveToFile(filename);
    Result := FileExists(filename);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TModelInfo.ToStringList(numClockParams: Integer = 0): TStringList;
var
  MyStringList: TStringList;
  i: Integer;
  j: Integer;
  NumBases: Integer;
  MyTempString: String;
const
  FirstColumnWidth = 40;
begin
  if DataType = 'DNA' then
    NumBases := 4
  else if DataType = 'Protein' then
    NumBases := 20
  else
    NumBases := 0;

  MyStringList := TStringList.Create;
  MyStringList.Add(MakePrettyString('Model', FirstColumnWidth) + '= ' +  FullName);
  MyStringList.Add(MakePrettyString('No. of params', FirstColumnWidth) + '= ' + Format('%8d', [NoOfParams - numClockParams]));
  MyStringList.Add(MakePrettyString('No. of rates', FirstColumnWidth) + '= ' + Format('%8d', [NoOfRates]));
  MyStringList.Add(MakePrettyString('AICc', FirstColumnWidth) + '= ' + FormatDoubleSafe(AICc, 3, 8));
  MyStringList.Add(MakePrettyString('BIC', FirstColumnWidth) + '= ' + FormatDoubleSafe(BIC, 3, 8));
  MyStringList.Add(MakePrettyString('LnL', FirstColumnWidth) + '= ' + FormatDoubleSafe(LogL, 3, 8));
  if NumSites > 0 then
  begin
    MyStringList.Add(MakePrettyString('No. of sites', FirstColumnWidth) + '= ' + Format('%.0n', [NumSites*1.0]));
    MyStringList.Add(MakePrettyString('No. of common sites', FirstColumnWidth) + '= ' + Format('%.0n', [NumCommonSites*1.0]));
    MyStringList.Add(MakePrettyString('No. of invar sites', FirstColumnWidth) + '= ' + Format('%.0n', [NumInvarSites*1.0]));
    MyStringList.Add(MakePrettyString('No. of gap sites', FirstColumnWidth) + '= ' + Format('%.0n', [NumGapSites*1.0]));
    MyStringList.Add(MakePrettyString('Site coverage', FirstColumnWidth) + '= ' + Format('%.0n', [SiteCoverage*1.0]));
  end;
  MyStringList.Add(MakePrettyString('Use invar', FirstColumnWidth) + '= ' + Format('%s', [BoolToStr(UseInvar, True)]));
  if CompareValue(Invar, 0.001, FP_CUTOFF) > 0 then
    MyStringList.Add(MakePrettyString('Invar', FirstColumnWidth) + '= ' + Format('%.5f', [invar]))
  else
    MyStringList.Add(MakePrettyString('Invar', FirstColumnWidth) + '= n/a');
  if CompareValue(Gamma, 0, FP_CUTOFF) > 0 then
    MyStringList.Add(MakePrettyString('Gamma', FirstColumnWidth) + '= ' + Format('%.5f', [gamma]))
  else
    MyStringList.Add(MakePrettyString('Gamma', FirstColumnWidth) + '= n/a');
  if DataType = 'DNA' then
    MyStringList.Add(MakePrettyString('Ts/Tv', FirstColumnWidth) + '= ' + FormatDoubleSafe(SVR, 3, 8));

  if NoOfRates > 0 then
    for i := 0 to NoOfRates - 1 do
      MyStringList.Add(MakePrettyString(Format('Rate[%d]',[i + 1]), FirstColumnWidth) + Format('= %.2f', [Rate[i]]));

  for i := 0 to NumBases - 1 do
  begin
    for j := 0 to NumBases - 1 do
    begin
      if i = j then
        continue; // we don't transform from X to X.
      MyTempString := MakePrettyString('r(' + Letter(i) + '->' + Letter(j) + ')', FirstColumnWidth);
      MyStringList.Add(MyTempString +  '= ' + FloatToStr(Matrix[i][j]))
    end;
  end;
  Result := MyStringList;
end;

function TModelInfo.SaveToFile(filename: String): Boolean;
var
  aList: TStringList = nil;
begin
  try
    aList := Self.ToStringList;
    aList.SaveToFile(filename);
    Result := FileExists(filename);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TModelInfo.ToTabularString: String;
begin
  Result := Format('%12s %16.2n %16.2n %16.2n %8.2f %8.2f %8.2f %8d %8d %8d', [ModelName, BIC, AICc, LogL, Gamma, Invar, SVR, NoOfParams, NoOfRates, NoOfSamples]);
end;

function TModelInfo.CatRatesCaption: String;
var
  i: Integer = -1;
begin
  Result := ' and mean evolutionary rates in these categories were ';
  Result += FloatToStrF(Rate[0], ffFixed, 12, paraDigits);
  for i := 1 to NoOfRates - 1 do
  begin
    if (NoOfRates > 1) and (i = (NoOfRates - 1)) then
      Result := Result + ', and ' + FloatToStrF(Rate[i], ffFixed, 12, paraDigits)
    else
      Result := Result + ', ' + FloatToStrF(Rate[i], ffFixed, 12, paraDigits);
  end;
  Result += ' substitutions per site.';
end;

function TModelInfo.FrequenciesCaption: String;
var
  b: TMegaStringBuilder = nil;
begin
  Result := EmptyStr;
  try
     b := TMegaStringBuilder.Create;
    if DataType <> 'DNA' then
    begin
      b.Add(' The amino acid frequencies are');
      b.Add(Format(' %s%% (A),', [FloatToStrF(Self.Freq[0]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (R),', [FloatToStrF(Self.Freq[1]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (N),', [FloatToStrF(Self.Freq[2]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (D),', [FloatToStrF(Self.Freq[3]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (C),', [FloatToStrF(Self.Freq[4]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (Q),', [FloatToStrF(Self.Freq[5]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (E),', [FloatToStrF(Self.Freq[6]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (G),', [FloatToStrF(Self.Freq[7]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (H),', [FloatToStrF(Self.Freq[8]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (I),', [FloatToStrF(Self.Freq[9]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (L),', [FloatToStrF(Self.Freq[10]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (K),', [FloatToStrF(Self.Freq[11]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (M),', [FloatToStrF(Self.Freq[12]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (F),', [FloatToStrF(Self.Freq[13]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (P),', [FloatToStrF(Self.Freq[14]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (S),', [FloatToStrF(Self.Freq[15]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (T),', [FloatToStrF(Self.Freq[16]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (W),', [FloatToStrF(Self.Freq[17]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (Y),', [FloatToStrF(Self.Freq[18]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' and %s (V).', [FloatToStrF(Self.Freq[19]*100, ffFixed, 12, paraDigits)]));
    end
    else
    begin
      b.Add(' The nucleotide frequencies are');
      b.Add(Format(' %s%% (A),', [FloatToStrF(Self.Freq[0]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (T/U),', [FloatToStrF(Self.Freq[1]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' %s%% (C),', [FloatToStrF(Self.Freq[2]*100, ffFixed, 12, paraDigits)]));
      b.Add(Format(' and %s%% (G).', [FloatToStrF(Self.Freq[3]*100, ffFixed, 12, paraDigits)]));
    end;

     Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

function TModelInfo.BaseModelName: String;
begin
  Result := ModelName;
  Result := StringReplace(Result, '+G', EmptyStr, []);
  Result := StringReplace(Result, '+I', EmptyStr, []);
  Result := StringReplace(Result, '+F', EmptyStr, []);
  Result := StringReplace(Result, '()', EmptyStr, []);
  Result := Trim(Result);
end;

function TModelInfo.FullBaseModelName: String;
begin
  Result := FullName;
  Result := StringReplace(Result, '+Gamma ', EmptyStr, []);
  Result := StringReplace(Result, '+Invar ', EmptyStr, []);

  Result := StringReplace(Result, '+G', EmptyStr, []);
  Result := StringReplace(Result, '+I', EmptyStr, []);
  Result := StringReplace(Result, '()', EmptyStr, []);
  Result := Trim(Result);
end;

function TModelInfo.IsPlusFModel: Boolean;
begin
  Result := False;
  if Pos('+F', ModelName) > 0 then
    Result := True;
end;

function TModelInfo.IsSameBaseModel(aModelInfo: TModelInfo): Boolean;
begin
  Result := (BaseModelName = aModelInfo.BaseModelName);
end;

class function TModelInfo.TabularHeaderString: String;
begin
  Result := Format('%12s %16s %16s %16s %8s %8s %8s %8s %8s %8s', ['model', 'BIC', 'AICc', 'LogL', 'Gamma', 'Invar', 'SVR', 'nParams', 'nRates', 'nSamples']);
end;

{$IFDEF DARWIN}
class constructor TMLTreeNode.Initialize;
begin
  NodeCriticalSection := TCriticalSection.Create;
end;

class destructor TMLTreeNode.Finalize;
begin
  if NodeCriticalSection <> nil then
    NodeCriticalSection.Free;
end;
{$ENDIF DARWIN}


{$IFDEF DEBUG}
initialization
  MLLogCriticalSection := TCriticalSection.Create;

finalization;
  if Assigned(MLLogCriticalSection) then
    MLLogCriticalSection.Free;

{$ENDIF}

end.
