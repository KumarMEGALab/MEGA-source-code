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

unit MTreeSearchThread;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF VISUAL_BUILD}Dialogs,{$ENDIF}
  LCLIntf, LCLType, Classes, Controls, Forms, Math, SyncObjs,
  MegaConsts, MTreeData, MTreeList, MPartitionList, MTreeEstFunc, MegaUtils_NV, MRuntimeProgressDlg, MSeqDistBase,
  SysUtils, MSimpleTreeNode, DateUtils;

type

  { TTreeSearchThread }

  TTreeSearchThread = class abstract (TMEGAThread)
  private
    procedure SetCanceled(AValue: boolean);
  protected
    FProgressDlg: TRuntimeProgress;
    FRemainingTimeStr: string;
    FLastProgressUpdate: TDateTime;
    FShowProgress: boolean;
    FSearchProgressStr: string;
    FNoOfTreesExamined: integer;
    FProgress : integer;
    FStatus: Ansistring;
    FRunStatusType: AnsiString;
    FRunStatusValue: AnsiString;
    FCanceled: boolean;
    procedure HideProgress;
    procedure DoHideProgress;
    procedure SafeShowProgressDlg;
    procedure SetRemainigTime(AProgress: integer);
    procedure SetProgressDlg(AProgressDlg: TRuntimeProgress); virtual;
    procedure UpdateRunStatusInfo(StatusType: AnsiString; StatusValue: AnsiString); virtual;
    procedure DoUpdateRunStatusInfo; virtual;
    procedure RemoveRunStatusInfo(StatusType: AnsiString);
    procedure DoRemoveRunStatusInfo;
    procedure DoOnProgress; virtual;
    procedure Search; virtual; abstract;
    procedure Execute; override;
    procedure OnProgress(AProgress: integer); virtual;
    procedure clearDistCalcPerc; virtual; deprecated;
    procedure DistCalcFinishedProc; virtual; deprecated;
    procedure clearTreeSearchPerc; virtual; deprecated;
    procedure TreeSearchFinishedProc; virtual; deprecated;
  public
    DoFullProgress: Boolean; { if true, show progress for dist calc and tree search instead of just replicates, otherwise for large data sets progress increments take hours to happen}
    property Canceled: boolean read FCanceled write SetCanceled;
    property SearchProgressStr: string read FSearchProgressStr write FSearchProgressStr;
    property RemainingTimeStr: string read FRemainingTimeStr;

    property ProgressDlg: TRuntimeProgress read FProgressDlg write SetProgressDlg;
    property ShowProgress: boolean read FShowProgress write FShowProgress;

    property NoOfTreesExamined: integer read FNoOfTreesExamined;

    constructor Create;
  end;

  TDrawGeneDupsEvent = procedure(AGeneTree: TSimpleTreeNodeArray) of object;
  TStartGeneDupsSearchFrom = (gdsTreeExplorer, gdsMegaMain);

  { TGeneDuplicationThread }

  TGeneDuplicationThread = class(TTreeSearchThread)
    private
      FErrorMsg: String;
      FSpeciesTreeList: TTreeList; // can't put the species tree data in the tree list because they may have differing numbers of otus
      FGeneTree: TSimpleTreeNodeArray;
      FGeneTreeRoot: TSimpleTreeNode;
      FSpeciesTree: TSimpleTreeNodeArray;
      FSpeciesTreeRoot: TSimpleTreeNode;
      FGTNoOfOtus: Integer; // number of otus in the gene tree
      FSTNoOfOtus: Integer; // number of otus in the species tree
      FGeneTreeList: TTreeList; // has the genetreedata.
      FIsSuccess: Boolean;
      FStartingFrom: TStartGeneDupsSearchFrom;
      TempIndex: Integer;
      FSearchingAllTrees: Boolean;
      procedure InitGeneTree(SpNames: TStringList); overload;
      procedure InitGeneTree; overload;
      procedure InitSpeciesTree(SpNames: TStringList); overload;
      procedure InitSpeciesTree(TreeList: TTreeList); overload;
      procedure InitSpeciesTree; overload;
      procedure InitSpeciesNamesLists;
      procedure InitSpeciesNamesList(RootNode: TSimpleTreeNode);
      procedure UpdateGeneTree(Data: TTreeData; SpNames: TStringList);
      procedure UpdateTreeData(var Data: TTreeData); overload;
      procedure UpdateTreeData(var Data: TTreeData; const AGeneTree: TSimpleTreeNodeArray); overload;
      procedure UpdateEmptyTreeData(var Data: TTreeData); overload;
      procedure UpdateEmptyTreeData(var Data: TTreeData; const AGeneTree: TSimpleTreeNodeArray); overload;
      procedure UpdateTreeListNames;
      procedure UpdateTempIndices(RootNode: TSimpleTreeNode); // number the nodes in the species tree using a pre-order traversal
      procedure InitProgress;
      procedure InitAncSpeciesIndices(AGeneTree: TSimpleTreeNodeArray; ASpeciesTree: TSimpleTreeNodeArray); // maps extant nodes in the gene tree to their corresponding nodes in the species tree
      procedure FindGeneTreeRoot; overload;
      procedure FindSpeciesTreeRoot; overload;
      function FindSpeciesTreeRoot(ASpeciesTree: TSimpleTreeNodeArray): Integer; overload;
      procedure SearchWithSpeciesTree; overload;
      procedure SearchWithSpeciesTree(AGeneTree, ASpeciesTree: TSimpleTreeNodeArray; GTRootNode, STRootNode: TSimpleTreeNode; DoInitSpTree: Boolean=True); overload;
      procedure SearchWithoutSpeciesTree; overload;
      procedure SearchWithoutSpeciesTree(RootNode: TSimpleTreeNode); overload;
      procedure SearchAllNodesWithSpeciesTree;
      procedure SearchAllNodesWithoutSpeciesTree;
      procedure LoadGeneTreesToTreeList(AList: TSimpleTreeNodeArrayArray);
      function FindGeneTreeRoot(GeneTree: TSimpleTreeNodeArray): Integer; overload;
      function CountDups(GeneTree: TSimpleTreeNodeArray): Integer;
      function MinDups(DupsArray: TIntArray): Integer;
      function FindAncestralSpecies(GeneTreeNode: TSimpleTreeNode): TSimpleTreeNode; // finds the ancestral node in the species tree for the provided gene tree node
      function IsSubsetOf(SmallList: TStringList; BigList: TStringList): Boolean;
      function HasASameString(First: TStringList; Second: TStringList): Boolean;
      function CreateNewSimpleTreeNodeArray(NumNodes: Integer): TSimpleTreeNodeArray;

      procedure DestroySimpleTreeNodeArray(ATree: TSimpleTreeNodeArray);
    protected
      procedure DoOnProgress; override;
      procedure OnProgress(AProgress: Integer); override;
      function AnalysisDescription: String; override;
    public
      TreeExplorer: TObject;
      GeneTreeFile: AnsiString;
      SpeciesTreeFile: AnsiString;
      DrawGeneDupsEvent: TDrawGeneDupsEvent; { in this case we only mark nodes in the existing tree}
      UsingSpeciesTree: Boolean;
      GeneTreeIsRooted: Boolean; // if false, we have to find a node to root on (a node that minimizes the number of gene duplications found)
      NumGeneDups: Integer;
      constructor Create(StartFrom: TStartGeneDupsSearchFrom; AOwner: TObject);
      destructor Destroy; override;

      procedure InitToUseTrees(GeneTree: TSimpleTreeNodeArray; GeneTreeList, SpeciesTreeList: TTreeList);
      procedure Initialize(GeneTree, SpeciesTree: TSimpleTreeNodeArray; GeneTreeList, SpeciesTreeList: TTreeList);
      procedure Search; override;
      procedure DoDrawGeneDupsEvent;
      property IsSuccess: Boolean read FIsSuccess;
      property StartedFrom: TStartGeneDupsSearchFrom read FStartingFrom;
      property GeneTreeList: TTreeList read FGeneTreeList write FGeneTreeList;
      property GTNoOfOtus: Integer read FGTNoOfOtus write FGTNoOfOtus;
      property STNoOfOtus: Integer read FSTNoOfOtus write FSTNoOfOtus;
      property ErrorMsg: String read FErrorMsg;
  end;


  TCallBackFunc  = function(Amessage: string; Astatus, Aprogress: integer): boolean of object;

  TTreeComputer = class
  private
    FCallBackFunc: TCallBackFunc;
    SearchRandomOrder: boolean;
  protected
    function  MaxProgress: integer; virtual; abstract;
  public
    property CallBackFunc: TCallBackFunc read FCallBackFunc write FCallBackFunc;

    procedure MakeTree; virtual; abstract;
  end;

  TCustomTreeComputer = class(TTreeComputer)
  private
    FTree : TTreeData;
    FD: PDistanceMatrix;
  public
    constructor Create(ATree: TTreeData;           // must be allocated.
                       ADmat: PDistanceMatrix);
  end;

//=== For UPGMA, NJ and ME tree construction

  TUPGMATreeComputer = class(TCustomTreeComputer)
  protected
    function  MaxProgress: integer; override;
  public
    procedure MakeTree; override;
  end;

  TNJTreeComputer = class(TCustomTreeComputer)
  protected
    function  MaxProgress: integer; override;
  public
    procedure MakeTree; override;
  end;

  TMETreeComputer = class(TCustomTreeComputer)
  private
    FTreeList : TTreeList;
    FMaxNoOfTrees: integer;
    FUseInitialNJTree: boolean;
    FThreshold: Double;
    Node: TpMENodeArray;
    FNoOfTreesExamined: integer;
  protected
    function  MaxProgress: integer; override;
    procedure InitNodes;
    function SearchDt2Trees(n: TpMENode; OnlyME: boolean):boolean;
    function SearchDt4Trees1(n: TpMENode; OnlyME: boolean):boolean;
    function SearchDt4Trees2(n: TpMENode; OnlyME: boolean):boolean;
    function SearchDt4Trees3(n: TpMENode; OnlyME: boolean):boolean;
    procedure RetrieveDt4Trees4(n1, n2: TpMENode);
    procedure SaveTree(s: double);
    procedure ResetTree(s: double);
  public
    property UseInitialNJTree: boolean read FUseInitialNJTree write FUseInitialNJTree;
    property MaxNoOfTrees: integer read FMaxNoOfTrees write FMaxNoOfTrees;
    property Threshold: double read FThreshold write FThreshold;
    property NoOfTreesExamined: integer read FNoOfTreesExamined;

    procedure MakeTree; override;
    procedure ComputeBLens;
    procedure SearchLastOTUPosition;

    constructor Create(ATreeList: TTreeList;
                       ADmat: PDistanceMatrix);
    destructor Destroy; override;
  end;

  TBootstrapFunc = function: boolean of object;

  TCustomTreeSearchThread = class(TTreeSearchThread)
  protected
    FTree : TTreeData;
    FTreeList : TTreeList;
    FD: PDistanceMatrix;
    FCallBackFunc: TCallBackFunc;
    FTreeComputer: TTreeComputer;
    FBootstrapFunc: TBootstrapFunc;

    SearchRandomOrder: boolean;

    function  GetMaxNoOfTrees: integer;
    Procedure SetMaxNoOfTrees(value: integer);
    function  GetUseInitialNJTree: boolean;
    Procedure SetUseInitialNJTree(value: boolean);
    function  GetThreshold: Double;
    Procedure SetThreshold(value: Double);

    function DefaultCallBackFunc(Amessage: string; Astatus, Aprogress: integer): boolean;
    function GetMaxProgress: integer;
    procedure Search; override;
  public
    property CallBackFunc: TCallBackFunc read FCallBackFunc write FCallBackFunc;
    property MaxProgress: integer read GetMaxProgress;

    property UseInitialNJTree: boolean read GetUseInitialNJTree write SetUseInitialNJTree;  // For ME
    property MaxNoOfTrees: integer read GetMaxNoOfTrees write SetMaxNoOfTrees;              // For ME
    property Threshold: double read GetThreshold write SetThreshold;                        // For ME

    constructor Create(ATree: TTreeData;      // must be allocated.
                       ADmat: PDistanceMatrix);
 end;

  TBootstrapCustomTreeThread = class;

  { TBootstrapCustomTreeChildThread }

  TBootstrapCustomTreeChildThread = class(TCustomTreeSearchThread)
  private
    ThreadIndex: integer;
    MainThread: TBootstrapCustomTreeThread;
    Done: boolean;

    FDistComputer: TSeqDistBase;
//    FTreeMakingProc: TTreeMakingProc;
    FNoOfBootstraps: integer;
    FBootstrapRepNum: Integer;
    FreqTable: ArrayOfInteger;
    FreqTableLen: integer;
    function ComputeBootD: boolean;
//    procedure CreateDistComputer(Source: TSeqDistBase);
  protected
    procedure Search; override;
    procedure Execute; override;
  public
    property DistComputer: TSeqDistBase read FDistComputer write FDistComputer;
    property NoOfBootstraps: integer read FNoOfBootstraps write FNoOfBootstraps;
//    property TreeMakingProc: TTreeMakingProc read FTreeMakingProc write FTreeMakingProc;

    constructor Create(parent: TBootstrapCustomTreeThread; index: integer;
                       ATree: TTreeData;      // must be allocated.
                       ADmat: PDistanceMatrix);
    destructor Destroy; override;
  end;

  TOnBootstrapDoneProc = procedure of object;

  { TBootstrapCustomTreeThread }

  TBootstrapCustomTreeThread = class(TBootstrapCustomTreeChildThread)
  private
    FTotalBootstrapReps: Integer;
    LockThread: TCriticalSection;

    FNoOfThreads: integer;
    ThreadMemSize: extended;

    ChildThread: array of TBootstrapCustomTreeChildThread;
    CurrentRep: integer;
    NoOfCurReps: integer;
    OriTree: TTreeData;

    FBootstrapProgress: integer;
    FShowBootstrapProgress: boolean;
    FBootstrapTrees: TPartitionList;
    FOnBootstrapDoneProc: TOnBootstrapDoneProc;
    procedure AddBootTree(Tree: TTreeData); overload;
    procedure AddBootTree(nodeData: PArrayOfNodeData; freq: Integer); overload;
    procedure OnBootstrapDone;
    procedure ComputeBclStdDev;
    function GetCurrentRep: integer;
    function CheckChildThreadDone: boolean;
    procedure SetNoOfThreads(n: integer);
  protected
    procedure SetProgressDlg(AProgressDlg: TRuntimeProgress); override;
    function EstimateThreadMemSize: extended; virtual; abstract;
    procedure Execute; override;
    procedure OnBootstrapProgress(AProgress: Integer);
    procedure DoOnBootstrapProgress;
  public
    property TreeList: TTreeList read FTreeList write FTreeList;
    property NoOfThreads: integer read FNoOfThreads write SetNoOfThreads;
    property ShowBootstrapProgress: boolean read FShowBootstrapProgress write FShowBootstrapProgress;

    property BootstrapTrees: TPartitionList read FBootstrapTrees write FBootstrapTrees;

    property OnBootstrapDoneProc: TOnBootstrapDoneProc read FOnBootstrapDoneProc write FOnBootstrapDoneProc;

    constructor Create(ATree: TTreeData;      // must be allocated.
                       ADmat: PDistanceMatrix;
                       MaxPercentRam: Integer = DEFAULT_MAX_PERCENT_RAM);
    destructor Destroy; override;
  end;

//=== UPGMA and UPGMA-Bootstrap

  { TUPGMATreeSearchThread }

  TUPGMATreeSearchThread = class(TCustomTreeSearchThread)
  private
    TreeComputer: TUPGMATreeComputer;
  protected
    function AnalysisDescription: String; override;
  public
    constructor Create(ATree: TTreeData;      // must be allocated.
                       ADmat: PDistanceMatrix);
    destructor Destroy; override;
  end;

  { TBootstrapUPGMAThread }

  TBootstrapUPGMAThread = class(TBootstrapCustomTreeThread)
  protected
    function EstimateThreadMemSize: extended; override;
    function AnalysisDescription: String; override;
  public
    constructor Create(ATree: TTreeData;      // must be allocated.
                       ADmat: PDistanceMatrix);
  end;

//=== NJ and NJ-Bootstrap

  { TNJTreeSearchThread }

  TNJTreeSearchThread = class(TCustomTreeSearchThread)
  private
    TreeComputer: TNJTreeComputer;
  protected
    function AnalysisDescription: String; override;
  public
    constructor Create(ATree: TTreeData;      // must be allocated.
                       ADmat: PDistanceMatrix);
    destructor Destroy; override;
  end;

  { TBootstrapNJThread }

  TBootstrapNJThread = class(TBootstrapCustomTreeThread)
  protected
    function EstimateThreadMemSize: extended; override;
    function AnalysisDescription: String; override;
  public
    constructor Create(ATree: TTreeData;      // must be allocated.
                       ADmat: PDistanceMatrix);
  end;

//=== ME and ME-Bootstrap

  { TMETreeSearchThread }

  TMETreeSearchThread = class(TCustomTreeSearchThread)
  protected
    function AnalysisDescription: String; override;
  public
    procedure ComputeBLens;

    constructor Create(ATreeList: TTreeList;    // TreeList must contain a tree.
                       ADmat: PDistanceMatrix);
    destructor Destroy; override;
  end;

  TBootstrapMEThread = class(TBootstrapCustomTreeThread)
  protected
    function EstimateThreadMemSize: extended; override;
  public
    constructor Create(ATreeList: TTreeList;    // TreeList must contain a tree.
                       ADmat: PDistanceMatrix);
    destructor Destroy; override;
  end;

  { TOLSBLenThread }

  TOLSBLenThread = class(TMETreeSearchThread)
  protected
    procedure Search; override;
    function AnalysisDescription: String; override;
  public
    constructor Create(ATreeList: TTreeList;    // TreeList must contain a tree.
                       ADmat: PDistanceMatrix);
  end;

  TOLSSearchLastOTUPositionThread = class(TMETreeSearchThread)
  protected
    procedure Search; override;
  public
    constructor Create(ATreeList: TTreeList;    // TreeList must contain a tree.
                       ADmat: PDistanceMatrix);
  end;


  //===User-specific-tasks-for-bootstrapped distance matrix

  TInitialTree = (itUser, itNJ, itME);

  TBootstrapProc = function(ARP: TRuntimeProgress): Boolean; // changed

  TBootstrapUserTreeDistThread = class(TBootstrapMEThread)
  protected
//    FNoOfReplication: integer;
//    FBootstrapDistanceProc: TBootstrapProc;
    FInitialTree: TInitialTree;

//    procedure DoOnBootstrapProgress;
    procedure ComputeBLens;
  public
    property InitialTree: TInitialTree read FInitialTree write FInitialTree;
//    property NoOfReplication: integer read FNoOfReplication write FNoOfReplication;
//    property BootstrapDistanceProc: TBootstrapProc read FBootstrapDistanceProc write FBootstrapDistanceProc;
    procedure Search; override;
      // to control the type of computation (CP vs root-to-tip test etc, just introduce new variableds);

    constructor Create(ATreeList: TTreeList;    // TreeList must contain a tree.
                       ADmat: PDistanceMatrix);
  end;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  Mega_Main,
  {$ELSE}
  MD_MegaMain,
  {$ENDIF}
  MegaUtils, MGlobalSettings, mmemutils,
  MDistPack, MNucDist, MAminoDist, MSynNonsynDist,
  MTreeProc, mstringbuilder, MAnalysisInfo, msitecoverage;

{ TTreeSearchThread }
procedure TTreeSearchThread.clearDistCalcPerc;
begin
  raise Exception.Create('clearDistCalcPerc is deprecated');
  FProgressDlg.UpdateRunStatusInfo('Dist Calc', 'In Progress...');
end;

procedure TTreeSearchThread.clearTreeSearchPerc;
begin
  raise Exception.Create('clearTreeSearchPerc is deprecated');
  FProgressDlg.UpdateRunStatusInfo(SearchProgressStr, Format('%3d%%', [0]));
end;

constructor TTreeSearchThread.Create;
begin
  inherited Create(True);
  StartExecute;
  FProgressDlg := nil;
  FreeOnTerminate := true; 
  Canceled := false;
  FShowProgress := true;
  FNoOfTreesExamined := 0;
  DoFullProgress := False;
  FSearchProgressStr := 'Searching Tree';
end;

procedure TTreeSearchThread.SetProgressDlg(AProgressDlg: TRuntimeProgress);
begin
  FProgressDlg := AProgressDlg;
end;

procedure TTreeSearchThread.UpdateRunStatusInfo(StatusType: AnsiString; StatusValue: AnsiString);
begin
  Assert(Trim(StatusValue) <> EmptyStr, 'empty status string');
  FRunStatusType := StatusType;
  FRunStatusValue := StatusValue;
  Synchronize(DoUpdateRunStatusInfo);
end;

procedure TTreeSearchThread.DoUpdateRunStatusInfo;
begin
  if Assigned(ProgressDlg) then
  begin
    ProgressDlg.UpdateRunStatusInfo(FRunStatusType, FRunStatusValue);
    {$IFDEF VISUAL_BUILD}
    ProgressDlg.Refresh;
    {$ENDIF}
  end;
end;

procedure TTreeSearchThread.RemoveRunStatusInfo(StatusType: AnsiString);
var
  tempStatus: AnsiString = '';
begin
  try
    tempStatus := FStatus;
    FStatus := StatusType;
    Synchronize(DoRemoveRunStatusInfo);
  finally
    FStatus := tempStatus;
  end;
end;

procedure TTreeSearchThread.DoRemoveRunStatusInfo;
begin
  if Assigned(ProgressDlg) then
    ProgressDlg.RemoveRunStatusInfo(FStatus);
end;

procedure TTreeSearchThread.Execute;
begin
  try
    ReturnValue := 1;
    if Terminated then
    begin
      FEndTime := Now;
      Canceled := true;
      Exit;
    end;
    Search;
    if Terminated then
      Canceled := true
    else
      ReturnValue := 0;
    FRemainingTimeStr := 'estimating time';
    {$IFNDEF VISUAL_BUILD}{$IFNDEF MSWINDOWS}
    UpdatePeakMemUsage;
    {$ENDIF}{$ENDIF}
    EndExecute;
    UpdateAnalysisSummary;
  except
    on E:Exception do
    begin
      FIsSuccess := False;
      MessagesLog.Add(E.Message);
      HideProgress;
    end;
  end;
end;

procedure TTreeSearchThread.DistCalcFinishedProc;
begin
  raise Exception.Create('DistCalcFinishedProc is deprecated');
  FProgressDlg.UpdateRunStatusInfo('Dist Calc', 'Done');
end;

procedure TTreeSearchThread.SetCanceled(AValue: boolean);
begin
  if FCanceled = AValue then Exit;
  FCanceled := AValue;

end;

procedure TTreeSearchThread.HideProgress;
begin
  Synchronize(DoHideProgress);
end;

procedure TTreeSearchThread.DoHideProgress;
begin
  if Assigned(FProgressDlg) and FProgressDlg.Visible then
    FProgressDlg.Hide;
end;

procedure TTreeSearchThread.SafeShowProgressDlg;
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FProgressDlg) then
    begin
      if not FProgressDlg.Visible then Synchronize(FProgressDlg.Show);
    end;
  {$ENDIF}
end;


procedure TTreeSearchThread.SetRemainigTime(AProgress: integer);
var
  h,m,s: Int64;
begin
  if AProgress <= 0 then exit;
  if SecondsBetween(FStartTime, Now) <= 5 then Exit; { highly inaccurate until a small amount of time has passed}
  s  := Trunc(SecondsBetween(FStartTime,Now)/AProgress*(100-AProgress))+1;
  h  := s div 3600;
  m  := (s div 60) -h*60;
  s  := s mod 60;

  FRemainingTimeStr := Format('%.2d:%.2d:%.2d', [h, m, s]);
end;

procedure TTreeSearchThread.DoOnProgress;
begin
  SetRemainigTime(FProgress);
  FProgressDlg.Progress := FProgress;
  //FProgressDlg.AddRunStatusInfo(SearchProgressStr, Format('%3d%%', [FProgress])); this is freezing the progress dialog!
end;

procedure TTreeSearchThread.OnProgress(AProgress: integer);
begin
  FProgress := AProgress;
  if (FProgressDlg <> nil) and ShowProgress then
    try
      {$IFDEF VISUAL_BUILD}
      Synchronize(DoOnProgress);
      {$ELSE}
      FProgressDlg.Progress := FProgress;
      {$ENDIF}
    except
    {$IFDEF DEBUG}
      {$IFDEF VISUAL_BUILD}
      on E: Exception do
        ShowMessage('Developer warning; Error in OnProgress: ' {+ E.Message});
      {$ENDIF}
    {$ENDIF}
    end;
end;

procedure TTreeSearchThread.TreeSearchFinishedProc;
begin
  raise Exception.Create('TreeSearchFinishedProc is deprecated');
  FProgressDlg.UpdateRunStatusInfo(SearchProgressStr, '100%');
end;

//==============================================================================
// TCustomTreeSearchThread
//==============================================================================
constructor TCustomTreeSearchThread.Create(ATree: TTreeData;
                                           ADmat: PDistanceMatrix);
begin
  inherited Create;
  FTree := ATree;
  if not FTree.isBLen then
    FTree.isBlen := True; { otherwise memory FTree.BLen will is not allocated}
  FD := ADmat;
  SearchRandomOrder := false;
//  MaxProgress := 100;
  FCallbackFunc := DefaultCallBackFunc;
end;

procedure TCustomTreeSearchThread.Search;
var
  aAnalysisInfo: TAnalysisInfo = nil;
begin
  FTreeComputer.MakeTree;
  if Assigned(Info) then
  begin
    aAnalysisInfo := TAnalysisInfo(Info);
    {$IFDEF VISUAL_BUILD}
    ComputeSiteCoverage(aAnalysisInfo);
    {$ENDIF}
  end;
end;

{
procedure TCustomTreeSearchThread.MakeTree;
begin
  case TreeMakingMethod of
    dtmmUPGMA: MakeUPGMATree;
    dtmmNJ:    MakeNJTree;
    dtmmME:    MakeMETree;
  end;
end;
}
function TCustomTreeSearchThread.DefaultCallBackFunc(Amessage: string; Astatus, Aprogress: integer): boolean;
begin
//  if Astatus <> 0 then {Do something if necessary}

  FSearchProgressStr := Amessage;
  OnProgress(trunc(Aprogress/MaxProgress*100));
  result := Terminated;
end;

function TCustomTreeSearchThread.GetMaxProgress: integer;
begin
  result := 0;
  if Assigned(FTreeComputer) then
    result := FTreeComputer.MaxProgress;
end;

function  TCustomTreeSearchThread.GetMaxNoOfTrees: integer;
begin
  result := 0;
  if FTreeComputer is TMETreeComputer then
    result := TMETreeComputer(FTreeComputer).MaxNoOfTrees;
end;

Procedure TCustomTreeSearchThread.SetMaxNoOfTrees(value: integer);
begin
  if FTreeComputer is TMETreeComputer then
    TMETreeComputer(FTreeComputer).MaxNoOfTrees := value;
end;

function  TCustomTreeSearchThread.GetUseInitialNJTree: boolean;
begin
  result := false;
  if FTreeComputer is TMETreeComputer then
    result := TMETreeComputer(FTreeComputer).UseInitialNJTree;
end;

Procedure TCustomTreeSearchThread.SetUseInitialNJTree(value: boolean);
begin
  if FTreeComputer is TMETreeComputer then
    TMETreeComputer(FTreeComputer).UseInitialNJTree := value;
end;

function  TCustomTreeSearchThread.GetThreshold: Double;
begin
  result := 0.0;
  if FTreeComputer is TMETreeComputer then
    result := TMETreeComputer(FTreeComputer).Threshold;
end;

Procedure TCustomTreeSearchThread.SetThreshold(value: Double);
begin
  if FTreeComputer is TMETreeComputer then
    TMETreeComputer(FTreeComputer).Threshold := value;
end;

//==============================================================================
// Construct and bootstrap UPGMA
//==============================================================================

function TUPGMATreeSearchThread.AnalysisDescription: String;
begin
  Result := 'UPGMA Tree Search';
end;

constructor TUPGMATreeSearchThread.Create(ATree: TTreeData;
                                          ADmat: PDistanceMatrix);
begin
  inherited Create(ATree, ADmat);

  FSearchProgressStr := 'Searching UPGMA Tree';
//  MaxProgress := Tree.NoOfOTUs*(Tree.NoOfOTUs-1);

  FTreeComputer := TUPGMATreeComputer.Create(ATree, ADmat);
  FTreeComputer.CallBackFunc := DefaultCallBackFunc;
end;

destructor TUPGMATreeSearchThread.Destroy;
begin
  TreeComputer.Free;

  inherited;
end;

constructor TBootstrapUPGMAThread.Create(ATree: TTreeData; ADmat: PDistanceMatrix);
begin
  inherited Create(ATree, ADMat);

  FSearchProgressStr := 'Searching UPGMA Tree';

  FTreeComputer := TUPGMATreeComputer.Create(ATree, ADmat);
  FTreeComputer.CallBackFunc := DefaultCallBackFunc;
  FBootstrapFunc := ComputeBootD;
end;

function TBootstrapUPGMAThread.EstimateThreadMemSize: extended;
begin
  result := SizeOf(Double)*(FTree.NoOfOTUs+1)*(FTree.NoOfOTUs+6);
end;

function TBootstrapUPGMAThread.AnalysisDescription: String;
begin
  Result := 'UPGMA Bootstrap';
end;


//==============================================================================
// Construct and bootstrap NJ
//==============================================================================

function TNJTreeSearchThread.AnalysisDescription: String;
begin
  Result := 'NJ Tree Search';
end;

constructor TNJTreeSearchThread.Create(ATree: TTreeData;
                                       ADmat: PDistanceMatrix);
begin
  inherited Create(ATree, ADmat);

  FSearchProgressStr := 'Searching NJ Tree';

  FTreeComputer := TNJTreeComputer.Create(ATree, ADmat);
  FTreeComputer.CallBackFunc := DefaultCallBackFunc;
end;

destructor TNJTreeSearchThread.Destroy;
begin
  TreeComputer.Free;

  inherited;
end;

constructor TBootstrapNJThread.Create(ATree: TTreeData; ADmat: PDistanceMatrix);
begin
  inherited Create(ATree, ADmat);

  FSearchProgressStr := 'Searching NJ Tree';
  FTreeComputer := TNJTreeComputer.Create(FTree, FD);
  FTreeComputer.CallBackFunc := DefaultCallBackFunc;
  FBootstrapFunc := ComputeBootD;
end;

function TBootstrapNJThread.EstimateThreadMemSize: extended;
begin
  result := SizeOf(Double)*(FTree.NoOfOTUs+1)*(FTree.NoOfOTUs+6);
end;

function TBootstrapNJThread.AnalysisDescription: String;
begin
  Result := 'NJ Bootstrap';
end;

//==============================================================================
// Construct and bootstrap ME
//==============================================================================

constructor TMETreeSearchThread.Create(ATreeList: TTreeList; ADmat: PDistanceMatrix);
begin
  inherited Create(ATreeList[0], ADmat);

  FTreeList := ATreeList;
  FSearchProgressStr := 'Searching ME Tree';
  //FTreeComputer := nil;
  FTreeComputer := TMETreeComputer.Create(FTreeList, FD);
  FTreeComputer.CallBackFunc := DefaultCallBackFunc;
end;

destructor TMETreeSearchThread.Destroy;
begin
  //if Assigned(TreeComputer) then
  //  TreeComputer.Free;
  inherited;
end;

function TMETreeSearchThread.AnalysisDescription: String;
begin
  Result := 'ME Tree Search';
end;

procedure TMETreeSearchThread.ComputeBLens;
begin
  TMETreeComputer(FTreeComputer).ComputeBLens;
end;

constructor TBootstrapMEThread.Create(ATreeList: TTreeList;    // TreeList must contain a tree.
                                      ADmat: PDistanceMatrix);
begin
  inherited Create(ATreeList[0], ADmat);

  FTreeList := ATreeList;
  FSearchProgressStr := 'Searching ME Tree';
  FTreeComputer := TMETreeComputer.Create(FTreeList, FD);
  FTreeComputer.CallBackFunc := DefaultCallBackFunc;
  FBootstrapFunc := ComputeBootD;
end;

destructor TBootstrapMEThread.Destroy;
begin
  FreeAndNil(FTreeComputer);

  inherited;
end;

function TBootstrapMEThread.EstimateThreadMemSize: extended;
begin
  result := SizeOf(Double)*(FTree.NoOfOTUs+1)*(FTree.NoOfOTUs+6);
end;

//==============================================================================
// TBootstrapDistTreeChildThread
//==============================================================================
constructor TBootstrapCustomTreeChildThread.Create(parent: TBootstrapCustomTreeThread;
                                                   index: integer;
                                                   ATree: TTreeData;      // must be allocated.
                                                   ADmat: PDistanceMatrix);
begin
  inherited Create(ATree, ADmat);
  if index > 0 then
    FProgressDlg := parent.FProgressDlg;
  Priority := tpNormal;

  MainThread  := parent;
  ThreadIndex := index;
  Done := false;
  FreeOnTerminate := true;

  if index > 0 then
  begin
    SearchRandomOrder := true;
    DoFullProgress := false;
    ShowProgress   := false;

    if Assigned(parent.DistComputer) then
    begin
      if parent.DistComputer is TNucDist then
        DistComputer := TNucDist.Create
      else if parent.DistComputer is TSynNonsynDist then
        DistComputer := TSynNonsynDist.Create
      else if parent.DistComputer is TAminoDist then
        DistComputer := TAminoDist.Create;
      DistComputer.Assign(parent.DistComputer);
      DistComputer.D := FD;
      FreqTableLen := DistComputer.FreqTableLen;
      setlength(FreqTable, FreqTableLen);

      FBootstrapFunc := ComputeBootD;
    end;

    if parent.FTreeComputer is TUPGMATreeComputer then
      FTreeComputer := TUPGMATreeComputer.Create(ATree, ADmat)
    else if parent.FTreeComputer is TNJTreeComputer then
      FTreeComputer := TNJTreeComputer.Create(ATree, ADmat)
    else if parent.FTreeComputer is TMETreeComputer then
    begin
      FTreeList := TTreeList.Create;
      FTreeList.Add(FTree);
      FTreeComputer := TMETreeComputer.Create(FTreeList, ADmat);
      TMETreeComputer(FTreeComputer).UseInitialNJTree := true;
    end;
  end;
end;

destructor TBootstrapCustomTreeChildThread.Destroy;
begin
  if ThreadIndex > 0 then
  begin
    FProgressDlg := nil; // owned by the parent thread
    DestroyDistanceMatrix(FD, FTree.NoOfOTUs);
    FD := nil;
    FreeAndNil(FDistComputer);
    if FTreeComputer is TMETreeComputer then
    begin
      FTreeList[0] := nil;
      FTreeList.Free;
    end;
    FreeAndNil(FTreeComputer);
  end;
  inherited;
end;

function TBootstrapCustomTreeChildThread.ComputeBootD: boolean;
var
  i,j: integer;
begin
  Resample(FreqTable, FreqTableLen);

  for i := 0 to FreqTableLen-1 do
    DistComputer.FreqTable[i] := FreqTable[i];
  try
    if DistComputer is TNucDist then
    begin
      if DistComputer.DistPack.DoesContain(gdMCL) then
        Result := TNucDist(DistComputer).ComputeDistancesSE
      else
        Result := TNucDist(DistComputer).ComputeDistances;
    end
    else if DistComputer is TSynNonsynDist then
      Result := TSynNonsynDist(DistComputer).ComputeDistances
    else if DistComputer is TAminoDist then
      Result := TAminoDist(DistComputer).ComputeDistances;

    for i := 0 to FTree.NoOfOTUs-1 do
      FD[i,i] := 0;
    for i := 0 to FTree.NoOfOTUs-1 do
      for j := 0 to i-1 do
        FD[j,i] := FD[i,j];

  except
    on E:Exception do
    begin
//      MainThread.BootPartitionList.ErrorMessages.Add(E.Message);
      Result := False;
    end;
  end;

end;


procedure TBootstrapCustomTreeChildThread.Search;
begin
  repeat
    FBootstrapRepNum := MainThread.GetCurrentRep;
    if FBootstrapRepNum < 0 then break;

    if not FBootstrapFunc then
      continue;

    FTreeComputer.MakeTree;
    MainThread.AddBootTree(FTree);
  until (FBootstrapRepNum < 0) or Terminated;
  Done := true;
end;

procedure TBootstrapCustomTreeChildThread.Execute;
begin
  repeat
    Search;

    while not Terminated do
      ThreadSwitch;
  until Terminated;
  MainThread.ChildThread[ThreadIndex] := nil;
end;

//==============================================================================
// TBootstrapDistTreeThread
//==============================================================================

constructor TBootstrapCustomTreeThread.Create(ATree: TTreeData; ADmat: PDistanceMatrix; MaxPercentRam: Integer = DEFAULT_MAX_PERCENT_RAM);
var
  mem: Int64 = -1;
begin
  inherited Create(self, 0, ATree, ADmat);
  FreeOnTerminate := false;

  LockThread := TCriticalSection.Create;

  GetAvailablePhysicalMemory(mem); // cross platform way so we can compile for Linux and Mac OS X
  mem := trunc(mem*MaxPercentRam/100);
  ThreadMemSize := EstimateThreadMemSize;
  FNoOfThreads := Max(1, GetNoOfProcessors-1);
  if FNoOfThreads*ThreadMemSize > mem then
    FNoOfThreads :=  trunc(mem/ThreadMemSize);

  FShowBootstrapProgress := true;

//// For test
//FNoOfThreads := 1;

  setlength(ChildThread, FNoOfThreads);
  {$IFDEF VISUAL_BUILD}
  if FNoOfThreads > 1 then
    MegaForm.MultithreadedCalcIsRunning := True;
  {$ENDIF}
  OnBootstrapDoneProc := OnBootstrapDone;
end;

destructor TBootstrapCustomTreeThread.Destroy;
begin
  LockThread.Free;
  setlength(ChildThread, 0);
  inherited;
end;

procedure TBootstrapCustomTreeThread.SetProgressDlg(AProgressDlg: TRuntimeProgress);
begin
  inherited;

  if Assigned(FProgressDlg) then
    FDistComputer := TAnalysisInfo(ProgressDlg.FMAI).DistComputer;
end;

procedure TBootstrapCustomTreeThread.SetNoOfThreads(n: integer);
var
  tempMemAvailable: Int64 = 0;
begin
  if (High(ChildThread) >= n) and (ChildThread[NoOfThreads-1] <> nil) then exit;

  FNoOfThreads := n;
  if GetAvailablePhysicalMemory(tempMemAvailable) then
  begin
    if FNoOfThreads*ThreadMemSize > tempMemAvailable then
      FNoOfThreads :=  trunc(tempMemAvailable/ThreadMemSize);
  end
  else
    FNoOfThreads := 1;
  setlength(ChildThread, FNoOfThreads);
end;


function TBootstrapCustomTreeThread.CheckChildThreadDone: boolean;
var
  i: integer;
begin
  result := true;
  if NoOfThreads > 1 then
    for i := 1 to NoOfThreads-1 do
      result := result and ChildThread[i].Done;
end;

function TBootstrapCustomTreeThread.GetCurrentRep: integer;
begin
  result := -1;
  try
    LockThread.Acquire;
    inc(CurrentRep);
    if CurrentRep <= NoOfBootstraps then
    begin
      result := CurrentRep;
      if Terminated then
        result := -1;
    end;
  finally
    LockThread.Release;
  end;
end;

procedure TBootstrapCustomTreeThread.Execute;
var
  i: integer;
  flag: boolean;
  aAnalysisInfo: TAnalysisInfo = nil;
begin
  OriTree := nil;
  if ProgressDlg = nil then exit;

  DistComputer := TAnalysisInfo(ProgressDlg.FMAI).DistComputer;
  FreqTableLen := DistComputer.FreqTableLen;
  setlength(FreqTable, FreqTableLen);

  FTreeComputer.SearchRandomOrder := false;
  FTreeComputer.MakeTree;

  OriTree := TTreeData.Create(FTree.NoOfOTUs, true, false, false);
  OriTree.Assign(FTree);

  UseInitialNJTree := true;
  SearchRandomOrder := true;
  DoFullProgress := false;
  ShowProgress   := false;
  DistComputer.ShowInProgressBar := false;
  ChildThread[0] := self;
  if NoOfThreads > 1 then
    for i := 1 to NoOfThreads-1 do
      ChildThread[i] := TBootstrapCustomTreeChildThread.Create(self, i, TTreeData.Create(FTree.NoOfOTUs, true, false, false), CreateDistanceMatrix(FTree.NoOfOTUs));

  try
    Randomize;
    FStartTime := Now;

    NoOfCurReps := 0;
    FBootstrapProgress := 0;
    OnBootstrapProgress(0);
    if NoOfThreads > 1 then
      for i := 1 to NoOfThreads-1 do
        ChildThread[i].Start;

    Search;

    while not (Terminated or CheckChildThreadDone) do
      ThreadSwitch;
    if Assigned(OnBootstrapDoneProc) then
      OnBootstrapDone;
    if Assigned(Info) then
    begin
      aAnalysisInfo := TAnalysisInfo(Info);
      {$IFDEF VISUAL_BUILD}
      if aAnalysisInfo.MyNoOfSites > 0 then
        ComputeSiteCoverage(aAnalysisInfo);
      {$ENDIF}
    end;
  finally
    if NoOfThreads > 1 then
      for i := 1 to NoOfThreads-1 do
        if ChildThread[i] <> nil then
          ChildThread[i].Terminate;
    repeat
      flag := true;
      if NoOfThreads > 1 then
        for i := 1 to NoOfThreads-1 do
          flag := flag and (ChildThread[i] = nil);
      ThreadSwitch;
    until flag;
    FreeAndNil(OriTree);
    FRemainingTimeStr := '00:00:00';
  end;

  if Terminated then
    ReturnValue := 1
  else
    ReturnValue := 0;
  EndExecute;
  UpdateAnalysisSummary;
end;

procedure TBootstrapCustomTreeThread.OnBootstrapDone;
begin
  FTree.Assign(OriTree);
  FBootstrapTrees.BuildPartitionsSummary(NoOfCurReps);
  FBootstrapTrees.Compare(FTree.NodeArray, PArrayOfDouble(@FTree.StatsArray[FTree.NoOfOTUs]));
  ComputeBclStdDev;
end;

procedure TBootstrapCustomTreeThread.ComputeBclStdDev;
var
  i: Integer = -1;
  aStdDev: Double = 0;
  numReps: Integer = 0;
begin
  FTree.IsStatsStdDev := True;
  for i := FTree.NoOfOTUs to 2*(FTree.NoOfOTUs - 1) - 1 do
  begin
    numReps := Round(FBootstrapTrees.TotalFrequency);
    aStdDev := ExpectedBclPrecision(trunc(FTree.Stats[i]/numReps*100), numReps);
    FTree.StatsStdDev[i] := aStdDev
  end;
end;

procedure TBootstrapCustomTreeThread.AddBootTree(Tree: TTreeData);
var
  newick: String = '';
begin
  try
    LockThread.Acquire;
    if assigned(FBootstrapTrees) then
      FBootstrapTrees.Add(Tree.NodeArray, 0.0);
    inc(NoOfCurReps);
    OnBootstrapProgress(trunc(NoOfCurReps/NoOfBootstraps*100));
  finally
    LockThread.Release;
  end;
end;

procedure TBootstrapCustomTreeThread.AddBootTree(nodeData: PArrayOfNodeData; freq: Integer);
begin
  try
    LockThread.Acquire;

    if assigned(FBootstrapTrees) then
    begin
      FBootstrapTrees.AddWithFreq(nodeData, 0.0, freq);
      inc(NoOfCurReps, freq);
    end;
    OnBootstrapProgress(trunc(NoOfCurReps/NoOfBootstraps*100));
  finally
    LockThread.Release;
  end;
end;

procedure TBootstrapCustomTreeThread.OnBootstrapProgress(AProgress: Integer);
begin
  FBootstrapProgress := AProgress;
  SetRemainigTime(AProgress);
  if (FProgressDlg <> nil) and ShowBootstrapProgress then
    try
      Synchronize(DoOnBootstrapProgress);
    except
    {$IFDEF DEBUG}
      {$IFDEF VISUAL_BUILD}
      on E: Exception do
        ShowMessage('Developer warning; Error in OnProgress: ' {+ E.Message});
      {$ENDIF}
    {$ENDIF}
    end;

end;

procedure TBootstrapCustomTreeThread.DoOnBootstrapProgress;
begin
  FProgressDlg.Progress := FBootstrapProgress;
  FProgressDlg.AddRunStatusInfo('Bootstrap Rep #', Format('%' + IntToStr(length(IntToStr(NoOfCurReps))) + 'd', [NoOfCurReps]) + '/' + IntToStr(NoOfBootstraps) +' ['+RemainingTimeStr+' remaining]');
end;

//==============================================================================
// TDistTreeComputer
//==============================================================================

constructor TCustomTreeComputer.Create(ATree: TTreeData;           // must be allocated.
                                     ADmat: PDistanceMatrix);
begin
  inherited Create;

  FTree := ATree;
  FD    := ADmat;
end;

//==============================================================================
// For TUPGMATreeComputer
//==============================================================================

function TUPGMATreeComputer.MaxProgress: integer;
begin
  result := FTree.NoOfOTUs*(FTree.NoOfOTUs-1);
end;

procedure TUPGMATreeComputer.MakeTree;
var AMIN, BMIN : integer;
    S, H, node, Order : PArrayOfInt;

    procedure ChooseNeighbor(N : integer);
    var mind, dd : double;
        i,j,k : integer;
    begin
        if SearchRandomOrder then
        begin
          for i := 0 to N-1 do
            Order[i] := i;
          for i := 0 to N-1 do
          begin
            j := Random(N-i)+i;
            k := Order[i];
            Order[i] := Order[j];
            Order[j] := k;
          end;
        end;

        if Order[0] < Order[1] then
        begin
          AMIN := Order[0];
          BMIN := Order[1];
          mind := FD[H[Order[1]]][H[Order[0]]];
        end
        else
        begin
          AMIN := Order[1];
          BMIN := Order[0];
          mind := FD[H[Order[0]]][H[Order[1]]];
        end;
        for i := 1 to N-1 do
          for j := 0 to i-1 do
          begin
            if Order[i] > Order[j] then
            begin
              dd := FD[H[Order[i]]][H[Order[j]]];
              if dd <= mind then
              begin
                mind := dd;
                AMIN := Order[j];
                BMIN := Order[i];
              end;
            end
            else
            begin
              dd := FD[H[Order[j]]][H[Order[i]]];
              if dd <= mind then
              begin
                mind := dd;
                AMIN := Order[i];
                BMIN := Order[j];
              end;
            end;
          end;
    end;

    procedure MakeNode(N : integer);
    begin
        FTree.NodeArray[FTree.NoOfOTUs-N].des1 := node[AMIN];
        FTree.NodeArray[FTree.NoOfOTUs-N].des2 := node[BMIN];
        FTree.BlenArray[node[AMIN]] := FD[H[BMIN]][H[AMIN]]/2;
        FTree.BlenArray[node[BMIN]] := FD[H[BMIN]][H[AMIN]]/2;
    end;

    procedure MatrixReconst(N : integer);
    var i : integer;
    begin
        for i := 0 to AMIN-1 do
            FD[H[AMIN]][H[i]] := (FD[H[AMIN]][H[i]]*S[AMIN] +FD[H[BMIN]][H[i]]*S[BMIN])/(S[AMIN]+S[BMIN]);
        for i := AMIN+1 to BMIN-1 do
            FD[H[i]][H[AMIN]] := (FD[H[i]][H[AMIN]]*S[AMIN] +FD[H[BMIN]][H[i]]*S[BMIN])/(S[AMIN]+S[BMIN]);
        for i := BMIN+1 to N-1 do
            FD[H[i]][H[AMIN]] := (FD[H[i]][H[AMIN]]*S[AMIN] +FD[H[i]][H[BMIN]]*S[BMIN])/(S[AMIN]+S[BMIN]);

        node[AMIN] := 2*FTree.NoOfOTUs-N;
        S[AMIN] := S[AMIN] +S[BMIN];
        if BMIN < N-1 then
            for i := BMIN to N-2 do begin
                H[i] := H[i+1];
                node[i] := node[i+1];
                S[i] := S[i+1];
            end;
    end;

    procedure ComputeBranchLength(i: integer);
    begin
      if i < FTree.NoOfOTUs then Exit;
      FTree.BlenArray[i] := FTree.BlenArray[i] -FTree.BlenArray[FTree.NodeArray[i-FTree.NoOfOTUs].des1];
      ComputeBranchLength(FTree.NodeArray[i-FTree.NoOfOTUs].des1);
      ComputeBranchLength(FTree.NodeArray[i-FTree.NoOfOTUs].des2);
    end;

var i,j : integer;
begin
    GetMem(S, SizeOf(Integer)*FTree.NoOfOTUs);
    GetMem(H, SizeOf(Integer)*FTree.NoOfOTUs);
    GetMem(node, SizeOf(Integer)*FTree.NoOfOTUs);
    GetMem(Order, SizeOf(Integer)*FTree.NoOfOTUs);

    for i := 0 to FTree.NoOfOTUs-1 do
        S[i] := 1;
    for i := 0 to FTree.NoOfOTUs-1 do
        H[i] := i;
    for i := 0 to FTree.NoOfOTUs-1 do
        node[i] := i;

    for i := 0 to FTree.NoOfOTUs-1 do
      Order[i] := i;

    for i := FTree.NoOfOTUs downto 2 do
    begin
        ChooseNeighbor(i);
        MakeNode(i);
        MatrixReconst(i);
        if Assigned(CallBackFunc) then
          if CallBackFunc('Searching UPGMA Tree', 0, MaxProgress-(i-1)*(i-2)) then
            break;
    end;

    ComputeBranchLength(FTree.NodeArray[FTree.NoOfOTUs-2].des1);
    ComputeBranchLength(FTree.NodeArray[FTree.NoOfOTUs-2].des2);

    for i := 1 to FTree.NoOfOTUs-1 do
        for j := 0 to i-1 do
            FD[i][j] := FD[j][i];

    FreeMemAndNil(S);
    FreeMemAndNil(H);
    FreeMemAndNil(node);
    FreeMemAndNil(Order);

    if Assigned(CallBackFunc) then
      CallBackFunc('Searching UPGMA Tree', 0, MaxProgress);
end;

//==============================================================================
// For TNJTreeComputer
//==============================================================================

function TNJTreeComputer.MaxProgress: integer;
begin
  result := FTree.NoOfOTUs*(FTree.NoOfOTUs-1);
end;

procedure TNJTreeComputer.MakeTree;
var
    AMIN, BMIN : integer;
    H, node, Order : PArrayOfInt;
    R : PArrayOfDouble;

    procedure ChooseNeighbor(N : integer);
    var S,sij : double;
        i,j,k : integer;
    begin
        for i := 0 to N-1 do
        begin
            R[i] := 0.0;
            if i > 0 then
                for j := 0 to i-1 do
                    R[i] := R[i] + FD[H[i]][H[j]];
            if i < (N-1) then
                for j := i+1 to N-1 do
                    R[i] := R[i] + FD[H[j]][H[i]];
        end;

        if SearchRandomOrder then
        begin
          for i := 0 to N-1 do
            Order[i] := i;
          for i := 0 to N-1 do
          begin
            j := Random(N-i)+i;
            k := Order[i];
            Order[i] := Order[j];
            Order[j] := k;
          end;
        end;

        if Order[0] < Order[1] then
        begin
          AMIN := Order[0];
          BMIN := Order[1];
          S := (N - 2)*FD[H[Order[1]]][H[Order[0]]] - R[Order[0]] - R[Order[1]];
        end
        else
        begin
          AMIN := Order[1];
          BMIN := Order[0];
          S := (N - 2)*FD[H[Order[0]]][H[Order[1]]] - R[Order[0]] - R[Order[1]];
        end;
        for i := 1 to N-1 do
          for j := 0 to i-1 do
          begin
            if Order[i] > Order[j] then
            begin
              sij := (N - 2)*FD[H[Order[i]]][H[Order[j]]] - R[Order[i]] - R[Order[j]];
              if sij <= S then
              begin
                S := sij;
                AMIN := Order[j];
                BMIN := Order[i];
              end;
            end
            else
            begin
              sij := (N - 2)*FD[H[Order[j]]][H[Order[i]]] - R[Order[i]] - R[Order[j]];
              if sij <= S then
              begin
                S := sij;
                AMIN := Order[i];
                BMIN := Order[j];
              end;
            end;
          end;
    end;

    procedure MakeNode(N : integer);
    begin
        FTree.NodeArray[FTree.NoOfOTUs-N].des1 := node[AMIN];
        FTree.NodeArray[FTree.NoOfOTUs-N].des2 := node[BMIN];
        if N = 2 then
        begin
            FTree.BlenArray[node[AMIN]] := FD[H[1]][H[0]]/2;
            FTree.BlenArray[node[BMIN]] := FD[H[1]][H[0]]/2;
        end
        else
        begin
            FTree.BlenArray[node[AMIN]] := ((N-2)*FD[H[BMIN]][H[AMIN]] +R[AMIN] -R[BMIN])/(N-2)/2;
            FTree.BlenArray[node[BMIN]] := ((N-2)*FD[H[BMIN]][H[AMIN]] -R[AMIN] +R[BMIN])/(N-2)/2;
        end;
    end;

    procedure MatrixReconst(N : integer);
    var
        i : integer;
        k : double;
    begin
        k := FD[H[BMIN]][H[AMIN]];
        for i := 0 to AMIN-1 do
            FD[H[AMIN]][H[i]] := (FD[H[AMIN]][H[i]] +FD[H[BMIN]][H[i]] -k)/2;
        for i := AMIN+1 to BMIN-1 do
            FD[H[i]][H[AMIN]] := (FD[H[i]][H[AMIN]] +FD[H[BMIN]][H[i]] -k)/2;
        for i := BMIN+1 to N-1 do
            FD[H[i]][H[AMIN]] := (FD[H[i]][H[AMIN]] +FD[H[i]][H[BMIN]] -k)/2;

        node[AMIN] := 2*FTree.NoOfOTUs-N;
        if BMIN < N-1 then
            for i := BMIN to N-2 do
            begin
                H[i] := H[i+1];
                node[i] := node[i+1];
            end;
    end;

var
    i,j : integer;
begin
    GetMem(R, SizeOf(double)*FTree.NoOfOTUs);
    GetMem(H, SizeOf(Integer)*FTree.NoOfOTUs);
    GetMem(node, SizeOf(Integer)*FTree.NoOfOTUs);
    GetMem(Order, SizeOf(Integer)*FTree.NoOfOTUs);


    for i := 0 to FTree.NoOfOTUs-1 do
        H[i] := i;
    for i := 0 to FTree.NoOfOTUs-1 do  // TODO : Eliminate this step.  Use 'Copy' function on H[] which copies arrays FAST.
        node[i] := i;

    for i := 0 to FTree.NoOfOTUs-1 do  // TODO : Eliminate this step.  Use 'Copy' function on H[] which copies arrays FAST.
      Order[i] := i;

    for i := FTree.NoOfOTUs downto 2 do
    begin
        ChooseNeighbor(i);
        MakeNode(i);
        MatrixReconst(i);
        if Assigned(CallBackFunc) then
          if CallBackFunc('Searching NJ Tree', 0, MaxProgress-(i-1)*(i-2)) then
            break;
    end;

    for i := 1 to FTree.NoOfOTUs-1 do
        for j := 0 to i-1 do
            FD[i][j] := FD[j][i];

    FreeMemAndNil(R);
    FreeMemAndNil(H);
    FreeMemAndNil(node);
    FreeMemAndNil(Order);

    if Assigned(CallBackFunc) then
      CallBackFunc('Searching NJ Tree', 0, MaxProgress);
end;

//==============================================================================
// For TMETreeComputer
//==============================================================================

procedure InitTree(node: TpMENodeArray; tree: PArrayOfNodeData; n: integer);
var i : integer;
begin
    for i := 0 to n-1 do begin
        node[i].Index := i;
        node[i].length := 0.0;
        node[i].dd := 0.0;
        node[i].OTU := true;
        node[i].anc := nil;
        node[i].sib := nil;
        node[i].des1 := nil;
        node[i].des2 := nil;
        node[i].flag := false;
        node[i].N[0] := 1;
        node[i].N[1] := i;
    end;
    for i := n to 2*n-2 do begin
        node[i].Index := i;
        node[i].length := 0.0;
        node[i].dd := 0.0;
        node[i].OTU := false;
        node[i].anc := nil;
        node[i].sib := nil;
        node[i].flag := false;
        node[i].N[0] := 0;
    end;
    for i := 0 to n-2 do begin
        node[n+i].des1 := node[tree[i].des1];
        node[n+i].des2 := node[tree[i].des2];
        node[tree[i].des1].anc := node[n+i];
        node[tree[i].des2].anc := node[n+i];
    end;
end;

procedure SetSib(p : TpMENode);
begin
    if not p^.OTU then begin
        SetSib(p^.des1);
        SetSib(p^.des2);
    end;
    if p^.anc = nil then Exit;
    if p = p^.anc^.des1 then
        p^.sib := p^.anc^.des2
    else
        p^.sib := p^.anc^.des1;
end;

procedure ChangeRoot(root, newposition : TpMENode);
var a, p, d : TpMENode;
    len, len1, len2 : double;
begin
    if newposition^.anc = root then begin
        if newposition = root^.des2 then begin
            p := root^.des1;
            root^.des1 := root^.des2;
            root^.des2 := p;
        end;
        Exit;
    end;
    len := root^.des1^.length +root^.des2^.length;
    d := newposition;
    p := d^.anc;
    a := p^.anc;
    len2 := d^.length;
    while p <> root do begin
        len1 := p^.length;
        p^.length := len2;
        len2 := len1;
        p^.anc := d;
        if d = p^.des1 then
            p^.des1 := a
        else
            p^.des2 := a;
        d := p;
        p := a;
        a := a^.anc;
    end;
    if d = p^.des1 then begin
        p^.des2^.anc := d;
        p^.des2^.length := len;
        if p = d^.des1 then
            d^.des1 := p^.des2
        else
            d^.des2 := p^.des2;
    end
    else begin
        p^.des1^.anc := d;
        p^.des1^.length := len;
        if p = d^.des1 then
            d^.des1 := p^.des1
        else
            d^.des2 := p^.des1;
    end;
    len := newposition^.length;
    p := newposition^.anc;
    p^.anc := root;
    p^.length := len;
    newposition^.anc := root;
    newposition^.length := 0;
    root^.des1 := newposition;
    root^.des2 := p;
end;

procedure SetN(p : TpMENode);
var i: integer;
begin
    for i := 0 to p.des1.N[0] do
        p.N[i] := p.des1.N[i];
    for i := 1 to p.des2.N[0] do
        p.N[p.N[0]+i] := p.des2.N[i];
    Inc(p.N[0], p.des2.N[0]);
end;

procedure SetAllN(p : TpMENode);
begin
    if p.OTU then Exit;
    SetAllN(p.des1);
    SetAllN(p.des2);
    SetN(p);
end;

procedure SetNodeDD(p: TpMENode; d: PDistanceMatrix);
var i,j: integer;
    dd: double;
begin
  dd := 0.0;
  for i := 1 to p.des1.N[0] do
    for j := 1 to p.des2.N[0] do
      dd := dd +d[p.des1.N[i], p.des2.N[j]];
  p.dd := p.des1.dd +p.des2.dd -2*dd;
end;

procedure SetAllDD(node: TpMENodeArray; n:integer; d: PDistanceMatrix);

  procedure SetDD(p: TpMENode);
  begin
    if not p.des1.OTU then SetDD(p.des1);
    if not p.des2.OTU then SetDD(p.des2);
    SetNodeDD(p, d);
  end;

var i,j: integer;
    p: TpMENode;
begin
  for i := 0 to n-1 do begin
    node[i].dd := 0.0;
    for j := 0 to n-1 do
      node[i].dd := node[i].dd +d[i,j];
  end;
  p := node[n-1];
  while p.anc <> nil do p := p.anc;
  SetDD(p.des2);
end;

procedure SetNRecursive(p : TpMENode);
begin
    if p.des1.N[0] = 0 then SetNRecursive(p.des1);
    if p.des2.N[0] = 0 then SetNRecursive(p.des2);
    if p.N[0] = 0 then SetN(p);
end;

procedure SetDDRecursive(p: TpMENode; d: PDistanceMatrix);
begin
    if p.des1.dd = 0.0 then SetDDRecursive(p.des1, d);
    if p.des2.dd = 0.0 then SetDDRecursive(p.des2, d);
    if p.DD = 0.0 then SetNodeDD(p, d);
end;

function GetExteriorBranchLength(p: TpMENode; n: integer):double;
var nj,nk: double;
begin
  nk := p.sib.N[0];
  nj := n -p.sib.N[0] -1;
  Result := ( (1 +nj +nk)*p.dd
             -(1 +nj -nk)*p.anc.dd
             -(1 -nj +nk)*p.sib.dd)/4/nj/nk;
end;

function GetInteriorBranchLength(p: TpMENode; n: integer):double;
var nj,nk,nl,nm: double;
begin
  if p.anc.anc = nil then begin
    nk := p.anc.des2.des2.N[0];
    nj := p.anc.des2.des1.N[0];
    Result := ( (1 +nj +nk)*p.anc.des1.dd
               -(1 +nj -nk)*p.anc.des2.des1.dd
               -(1 -nj +nk)*p.anc.des2.des2.dd)/4/nj/nk;
  end
  else begin
    nj := p.des1.N[0];
    nk := p.des2.N[0];
    nl := p.sib.N[0];
    nm := n -nj -nk -nl;
    Result := ( (n/nj +n/nk +n/nl +n/nm -4)*p.dd
                +(nj+nk)/nj/nk*((2*nk-n)*p.des1.dd +(2*nj-n)*p.des2.dd)
                +(nl+nm)/nl/nm*((2*nm-n)*p.sib.dd +(2*nl-n)*p.anc.dd))
             /4/(nj+nk)/(nl+nm);
  end;
end;

procedure ResetNodeFlag(node: TpMENodeArray; n: integer);
var i : integer;
begin
    for i := 0 to 2*n-2 do
        node[i].flag := false;
end;

function GetOLSBranchLength(p: TpMENode; n: integer; d: PDistanceMatrix):double;
begin
  Result := 0.0; //SK mega2b4
  if p.anc = nil then
    Result := 0.0
  else if p.OTU then
    if p.anc.anc = nil then
      Result := 0.0
    else
      Result := GetExteriorBranchLength(p, n)
  else if p.anc <> nil then
    Result := GetInteriorBranchLength(p, n);
end;

function GetSumOfOLSBranchLength(node: TpMENodeArray; NoOfOTUs: integer; d: PDistanceMatrix):double;
var i : integer;
begin
  for i := 0 to NoOfOTUs-1 do
    if node[i].anc.anc = nil then
      node[i].length := 0.0
    else
      node[i].length := GetExteriorBranchLength(node[i], NoOfOTUs);
  for i := NoOfOTUs to 2*NoOfOTUs-2 do begin
    if node[i].anc = nil then Continue;
    node[i].length := GetInteriorBranchLength(node[i], NoOfOTUs);
  end;
  Result := 0.0;  // added in M2.1
  for i:=0 to 2*NoOfOtus-2 do
    Result := Result + node[i].length;
end;

procedure AddBranch(r, n, b : TpMENode);
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

    r.sib := b.sib;
    b.sib.sib := r;
    b.sib := n;
    n.sib := b;
end;

procedure RemoveBranch(n : TpMENode);
var a, aa, b : TpMENode;
begin
    a := n.anc;
    aa := a.anc;
    b := n.sib;
    if a = aa.des1 then
        aa.des1 := b
    else
        aa.des2:= b;
    b.anc := aa;

    b.sib := a.sib;
    a.sib.sib := b;
end;

constructor TMETreeComputer.Create(ATreeList: TTreeList;
                                   ADmat: PDistanceMatrix);
var
  n,i: integer;
begin
  inherited Create(ATreeList[0], ADmat);

  FTreeList := ATreeList;
  FMaxNoOfTrees := 1; // default
  FThreshold := 1e-10;
  FUseInitialNJTree := true;

  n := FTree.NoOfOTUs;
  node := nil;
  GetMem(node, SizeOf(TpMENode)*2*n);
  for i := 0 to 2*n-2 do
    NEW(node[i]);
  for i := 0 to n-1 do
    GetMem(node[i].N, SizeOf(Integer)*2);
  for i := n to 2*n-2 do
    GetMem(node[i].N, SizeOf(Integer)*(n+1));
end;

destructor TMETreeComputer.Destroy;
var
  n,i: integer;
begin
  n := FTree.NoOfOTUs;
  for i := 0 to 2*n-2 do
    if node[i] <> nil then
    begin
      FreeMemAndNil(node[i].N);
      Dispose(node[i]);
    end;
  FreeMemAndNil(node);

  inherited;
end;

procedure TMETreeComputer.InitNodes;
var
  n: integer;
begin
  n := FTree.NoOfOTUs;
  InitTree(node, FTree.NodeArray, n);
  ResetNodeFlag(node, n);
  ChangeRoot(node[2*n-2], node[n-1]);
  SetSib(node[2*n-2]);
  SetAllN(node[2*n-2].des2);
  SetAllDD(node, n, FD);
end;

procedure TMETreeComputer.ResetTree(s: double);
var tree: TTreeData;
    i, n: integer;
begin
  n := FTreeList[0].NoOfOTUs;
  while FTreeList.NoOfTrees > 1 do
    FTreeList.Delete(FTreeList.NoOfTrees-1);

  tree := FTreeList[0];
  for i := 0 to n-2 do
  begin
    tree.NodeArray[i].des1 := Node[i+n].des1.index;
    tree.NodeArray[i].des2 := Node[i+n].des2.index;
  end;
  for i := 0 to 2*n-3 do
    tree.BLenArray[i] := Node[i].Length;
  tree.Freq := 1;  // for consensus building for ME
  tree.Value := s;
end;

procedure TMETreeComputer.SaveTree(s: double);
var tree: TTreeData;
    i,n : integer;
begin
  if s > (FTreeList[0].Value*(1.0+FThreshold)) then
    Exit;

  if FTreeList.NoOfTrees = FMaxNoOfTrees then
    if FTreeList[FMaxNoOfTrees-1].Value > s then
      FTreeList.Delete(FMaxNoOfTrees-1)
    else
      Exit;

  n := FTree.NoOfOTUs;
  tree := TTreeData.Create(n,true,false,false);
  for i := 0 to n-2 do begin
      tree.NodeArray[i].des1 := Node[i+n].des1.index;
      tree.NodeArray[i].des2 := Node[i+n].des2.index;
  end;
  for i := 0 to 2*n-3 do
      tree.BLenArray[i] := Node[i].Length;
  tree.Freq :=  1; // MEGA2.1
  tree.Value := s;

  if FTreeList.NoOfTrees = 0 then
      FTreeList.Add(tree)
  else
  begin
    i := FTreeList.NoOfTrees-1;
    while (i >= 0) and (FTreeList[i].Value > tree.Value) do
      dec(i);
    FTreeList.Insert(i+1, tree);
  end;
  FTree := FTreeList[0];
end;

function TMETreeComputer.SearchDt2Trees(n: TpMENode; OnlyME: boolean):boolean;
var p : array[0..5] of TpMENode;
    b : array[1..5] of double;
    s0: Double = 0;
    s : double;
    i,j, m : integer;

    function CalcS:double;
    var i : integer;
    begin
        Result := 0.0;
        for i := 1 to 5 do begin
            p[i].Length := GetOLSBranchLength(p[i], FTree.NoOfOTUs, FD);
            Result := Result +p[i].length;
        end;
    end;

    procedure SaveBLen;
    var i : integer;
    begin
        s0 := 0.0;
        for i := 1 to 5 do begin
            b[i] := p[i].Length;
            s0 := s0 +b[i];
        end;
    end;

    procedure ResetN;
    begin
        p[4].N[0] := 0;
        p[5].N[0] := 0;
        SetNRecursive(p[0]);
        p[4].dd := 0.0;
        p[5].dd := 0.0;
        SetDDRecursive(p[0], FD);
    end;

    procedure ClearFlag;
    var i: integer;
    begin
        for i := 1 to 5 do
            p[i].flag := false;
    end;

begin
  for j := Low(b) to High(b) do
    b[j] := 0;
    Result := false;
    if n.anc.anc = nil then Exit;
    p[0] := n.anc.anc;
    p[1] := n.des1;
    p[2] := n.des2;
    p[3] := n.sib;
    p[4] := n.anc;
    p[5] := n;

    m := 5;
    SaveBLen;
    RemoveBranch(p[3]);
    for i := 1 to 2 do begin
        AddBranch(p[4], p[3], p[i]);
        ResetN;
        s := CalcS;
        if (s0-s) >= 0.00000000000001 then begin
            ResetTree(FTreeList[0].Value-s0+s);
            m := i;
            SaveBLen;
            ClearFlag;
            Result := true;
        end
        else begin
            n.ds[i] := s -s0;
            if not OnlyME then
                SaveTree(FTreeList[0].Value-s0+s);
        end;
        RemoveBranch(p[3]);
    end;
    AddBranch(p[4], p[3], p[m]);
    ResetN;
    for i := 1 to 5 do
        p[i].length := b[i];

    Inc(FNoOfTreesExamined, 4);
end;

function TMETreeComputer.SearchDt4Trees1(n: TpMENode; OnlyME: boolean):boolean;
var p : array[0..7] of TpMENode;
    b : array[1..7] of double;
    s0: Double = 0;
    s : double = 0;
    mi,mj,i,j : integer;

    function CalcS:double;
    var i: integer;
    begin
        Result := 0.0;
        for i := 1 to 7 do begin
            p[i].Length := GetOLSBranchLength(p[i], FTree.NoOfOTUs, FD);
            Result := Result +p[i].length;
        end;
    end;

    procedure SaveBLen;
    var i : integer;
    begin
        s0 := 0.0;
        for i := 1 to 7 do begin
            b[i] := p[i].Length;
            s0 := s0 +b[i];
        end;
    end;

    procedure ResetN(n1,n2,n3: integer);
    begin
        p[n1].N[0] := 0;
        p[n2].N[0] := 0;
        p[n3].N[0] := 0;
        SetNRecursive(p[0]);
        p[n1].dd := 0.0;
        p[n2].dd := 0.0;
        p[n3].dd := 0.0;
        SetDDRecursive(p[0] ,FD);
    end;

    procedure ClearFlag;
    var i: integer;
    begin
        for i := 1 to 7 do
            p[i].flag := false;
    end;

begin
    for j := Low(b) to High(b) do
      b[j] := 0;
    Result := false;
    if n.anc.anc = nil then Exit;
    if n.anc.anc.anc = nil then Exit;

    p[0] := n.anc.anc.anc;
    p[1] := n.des1;
    p[2] := n.des2;
    p[3] := n.sib;
    p[4] := n.anc;
    p[5] := n;
    p[6] := n.anc.sib;
    p[7] := n.anc.anc;

    mi := 5;
    mj := 4;
    SaveBLen;

    RemoveBranch(p[3]);
    RemoveBranch(p[6]);
    for i := 1 to 2 do begin
        AddBranch(p[4], p[3], p[i]);
        for j := 1 to 4 do begin
            AddBranch(p[7], p[6], p[j]);
            ResetN(4, 5, 7);
            s := CalcS;
            if (s0-s) >= 0.00000000000001 then begin
                ResetTree(FTreeList[0].Value-s0+s);
                mi := i;
                mj := j;
                SaveBLen;
                ClearFlag;
                Result := true;
            end
            else if not OnlyME then
                SaveTree(FTreeList[0].Value-s0+s);
            RemoveBranch(p[6]);
        end;
        RemoveBranch(p[3]);
    end;
    AddBranch(p[4], p[3], p[5]);
    for j := 1 to 2 do begin
        AddBranch(p[7], p[6], p[j]);
        ResetN(4, 5, 7);
        s := CalcS;
        if (s0-s) >= 0.00000000000001 then begin
            ResetTree(FTreeList[0].Value-s0+s);
            mi := 5;
            mj := j;
            SaveBLen;
            ClearFlag;
            Result := true;
        end
        else if not OnlyME then
            SaveTree(FTreeList[0].Value-s0+s);
        RemoveBranch(p[6]);
    end;
    RemoveBranch(p[3]);

    AddBranch(p[4], p[3], p[mi]);
    AddBranch(p[7], p[6], p[mj]);
    ResetN(4, 5, 7);
    for i := 1 to 7 do
        p[i].length := b[i];

    Inc(FNoOfTreesExamined, 10);
end;

function TMETreeComputer.SearchDt4Trees2(n: TpMENode; OnlyME: boolean):boolean;
var p : array[0..7] of TpMENode;
    b : array[1..7] of double;
    s0, s : double;
    mi,mj,i,j : integer;

    function CalcS:double;
    var i: integer;
    begin
        Result := 0.0;
        for i := 1 to 7 do begin
            p[i].Length := GetOLSBranchLength(p[i], FTree.NoOfOTUs, FD);
            Result := Result +p[i].length;
        end;
    end;

    procedure SaveBLen;
    var i : integer;
    begin
        s0 := 0.0;
        for i := 1 to 7 do begin
            b[i] := p[i].Length;
            s0 := s0 +b[i];
        end;
    end;

    procedure ResetN(n1,n2,n3: integer);
    begin
        p[n1].N[0] := 0;
        p[n2].N[0] := 0;
        p[n3].N[0] := 0;
        SetNRecursive(p[0]);
        p[n1].dd := 0.0;
        p[n2].dd := 0.0;
        p[n3].dd := 0.0;
        SetDDRecursive(p[0] ,FD);
    end;

    procedure ClearFlag;
    var i: integer;
    begin
        for i := 1 to 7 do
            p[i].flag := false;
    end;

begin
    for j := Low(b) to High(b) do
      b[j] := 0;
    Result := false;
    if n.des1.OTU or n.des2.OTU then Exit;

    p[0] := n.anc;
    p[1] := n.des1.des1;
    p[2] := n.des1.des2;
    p[3] := n.des1;
    p[4] := n;
    p[5] := n.des2;
    p[6] := n.des2.des1;
    p[7] := n.des2.des2;


    mi := 3;
    mj := 6;
    SaveBLen;

    RemoveBranch(p[6]);
    RemoveBranch(p[7]);

    for i := 1 to 2 do begin
        AddBranch(p[4], p[6], p[i]);
        for j := 1 to 4 do begin
            AddBranch(p[5], p[7], p[j]);
            ResetN(3, 4, 5);
            s := CalcS;
            if (s0-s) >= 0.00000000000001 then begin
                ResetTree(FTreeList[0].Value-s0+s);
                mi := i;
                mj := j;
                SaveBLen;
                ClearFlag;
                Result := true;
            end
            else if not OnlyME then
                SaveTree(FTreeList[0].Value-s0+s);
            RemoveBranch(p[7]);
        end;
        RemoveBranch(p[6]);
    end;
    AddBranch(p[4], p[6], p[3]);
    for j := 1 to 2 do begin
        AddBranch(p[5], p[7], p[j]);
        ResetN(3, 4, 5);
        s := CalcS;
        if (s0-s) >= 0.00000000000001 then begin
            ResetTree(FTreeList[0].Value-s0+s);
            mi := 3;
            mj := j;
            SaveBLen;
            ClearFlag;
            Result := true;
        end
        else if not OnlyME then
            SaveTree(FTreeList[0].Value-s0+s);
        RemoveBranch(p[7]);
    end;
    RemoveBranch(p[6]);

    AddBranch(p[4], p[6], p[mi]);
    AddBranch(p[5], p[7], p[mj]);
    ResetN(3, 4, 5);
    for i := 1 to 7 do
        p[i].length := b[i];

    Inc(FNoOfTreesExamined, 10);
end;

function TMETreeComputer.SearchDt4Trees3(n: TpMENode; OnlyME: boolean):boolean;
var p : array[0..9] of TpMENode;
    b : array[1..9] of double;
    s0: Double = 0;
    s: double = 0;
    mi,mj,i,j : integer;

    function CalcS:double;
    var i: integer;
    begin
        Result := 0.0;
        for i := 1 to 9 do begin
            p[i].Length := GetOLSBranchLength(p[i], FTree.NoOfOTUs, FD);
            Result := Result +p[i].length;
        end;
    end;

    procedure SaveBLen;
    var i : integer;
    begin
        s0 := 0.0;
        for i := 1 to 9 do begin
            b[i] := p[i].Length;
            s0 := s0 +b[i];
        end;
    end;

    procedure ResetN(n1,n2,n3,n4: integer);
    begin
        p[n1].N[0] := 0;
        p[n2].N[0] := 0;
        p[n3].N[0] := 0;
        p[n4].N[0] := 0;
        SetNRecursive(p[0]);
        p[n1].dd := 0.0;
        p[n2].dd := 0.0;
        p[n3].dd := 0.0;
        p[n4].dd := 0.0;
        SetDDRecursive(p[0] ,FD);
    end;

    procedure ClearFlag;
    var i: integer;
    begin
        for i := 1 to 9 do
            p[i].flag := false;
    end;

begin
    Result := false;
    if n.anc.anc = nil then Exit;
    if n.anc.anc.anc = nil then Exit;
    if n.anc.anc.anc.anc <> nil then begin
        p[0] := n.anc.anc.anc.anc;
        p[1] := n.des1;
        p[2] := n.des2;
        p[3] := n.sib;
        p[4] := n.anc;
        p[5] := n;
        p[6] := n.anc.sib;
        p[7] := n.anc.anc;
        p[8] := n.anc.anc.sib;
        p[9] := n.anc.anc.anc;

        mi := 5;
        mj := 7;
        SaveBLen;

        RemoveBranch(p[3]);
        RemoveBranch(p[8]);
        for i := 1 to 2 do begin
            AddBranch(p[4], p[3], p[i]);
            for j := 5 to 6 do begin
                AddBranch(p[9], p[8], p[j]);
                ResetN(4, 5, 7, 9);
                s := CalcS;
                if (s0-s) >= 0.00000000000001 then begin
                    ResetTree(FTreeList[0].Value-s0+s);
                    mi := i;
                    mj := j;
                    SaveBLen;
                    ClearFlag;
                    Result := true;
                end
                else if not OnlyME then
                    SaveTree(FTreeList[0].Value-s0+s);
                RemoveBranch(p[8]);
            end;
            RemoveBranch(p[3]);
        end;

        AddBranch(p[4], p[3], p[mi]);
        AddBranch(p[9], p[8], p[mj]);
        ResetN(4, 5, 7, 9);
        for i := 1 to 9 do
            p[i].length := b[i];

        Inc(FNoOfTreesExamined, 4);
    end;

    if not n.anc.sib.OTU then begin
        p[0] := n.anc.anc.anc;
        p[1] := n.des1;
        p[2] := n.des2;
        p[3] := n.sib;
        p[4] := n.anc;
        p[5] := n;
        p[6] := n.anc.sib.des1;
        p[7] := n.anc.sib.des2;
        p[8] := n.anc.anc;
        p[9] := n.anc.sib;

        mi := 9;
        mj := 5;
        SaveBLen;

        RemoveBranch(p[3]);
        RemoveBranch(p[5]);
        for i := 6 to 7 do begin
            AddBranch(p[8], p[5], p[i]);
            for j := 1 to 2 do begin
                AddBranch(p[4], p[3], p[j]);
                ResetN(4, 5, 8, 9);
                s := CalcS;
                if (s0-s) >= 0.00000000000001 then begin
                    ResetTree(FTreeList[0].Value-s0+s);
                    mi := i;
                    mj := j;
                    SaveBLen;
                    ClearFlag;
                    Result := true;
                end
                else if not OnlyME then
                    SaveTree(FTreeList[0].Value-s0+s);
                RemoveBranch(p[3]);
            end;
            RemoveBranch(p[5]);
        end;

        AddBranch(p[8], p[5], p[mi]);
        AddBranch(p[4], p[3], p[mj]);
        ResetN(4, 5, 8, 9);
        for i := 1 to 9 do
            p[i].length := b[i];

        Inc(FNoOfTreesExamined, 4);
    end;
end;

procedure TMETreeComputer.RetrieveDt4Trees4(n1, n2: TpMENode);
var p1,p2 : array[0..5] of TpMENode;
    b1,b2 : array[1..5] of double;
    i,j : integer;
    ds0,ds : double;

    procedure CalcBLen;
    var i : integer;
    begin
        for i := 1 to 5 do
            p1[i].Length := GetOLSBranchLength(p1[i], FTree.NoOfOTUs, FD);
        for i := 1 to 5 do
            p2[i].Length := GetOLSBranchLength(p2[i], FTree.NoOfOTUs, FD);
    end;

    procedure ResetN;
    begin
        p1[4].N[0] := 0;
        p1[5].N[0] := 0;
        SetNRecursive(p1[0]);
        p1[4].dd := 0.0;
        p1[5].dd := 0.0;
        SetDDRecursive(p1[0], FD);
        p2[4].N[0] := 0;
        p2[5].N[0] := 0;
        SetNRecursive(p2[0]);
        p2[4].dd := 0.0;
        p2[5].dd := 0.0;
        SetDDRecursive(p2[0], FD);
    end;

    procedure SaveBLen;
    var i : integer;
    begin
        for i := 1 to 5 do
            b1[i] := p1[i].Length;
        for i := 1 to 5 do
            b2[i] := p2[i].Length;
    end;

    procedure RearrangeBranch(i, j: integer);
    begin
        RemoveBranch(p1[3]);
        AddBranch(p1[4], p1[3], p1[i]);
        RemoveBranch(p2[3]);
        AddBranch(p2[4], p2[3], p2[j]);
        ResetN;
        CalcBLen;
    end;

    procedure ResetBranch;
    var i: integer;
    begin
        RemoveBranch(p1[3]);
        AddBranch(p1[4], p1[3], p1[5]);
        RemoveBranch(p2[3]);
        AddBranch(p2[4], p2[3], p2[5]);
        ResetN;
        for i := 1 to 5 do
            p1[i].Length := b1[i];
        for i := 1 to 5 do
            p2[i].Length := b2[i];
    end;

begin
    if n1.anc.anc = nil then Exit;
    if n2.anc.anc = nil then Exit;
    if n1.sib = n2 then Exit;
    if n1.anc = n2 then Exit;
    if n2.anc = n1 then Exit;
    if n1.anc.anc = n2 then Exit;
    if n2.anc.anc = n1 then Exit;
    if n1.anc.sib = n2 then Exit;
    if n2.anc.sib = n1 then Exit;

    p1[0] := n1.anc.anc;
    p1[1] := n1.des1;
    p1[2] := n1.des2;
    p1[3] := n1.sib;
    p1[4] := n1.anc;
    p1[5] := n1;
    p2[0] := n2.anc.anc;
    p2[1] := n2.des1;
    p2[2] := n2.des2;
    p2[3] := n2.sib;
    p2[4] := n2.anc;
    p2[5] := n2;

    SaveBLen;
    ds0 := FTreeList[FTreeList.NoOfTrees-1].Value -FTreeList[0].Value;
    for i := 1 to 2 do
        for j := 1 to 2 do begin
            ds := n1.ds[i]+n2.ds[j];
            if (ds0-ds) >= 0.00000000000001 then begin
                RearrangeBranch(i, j);
                SaveTree(FTreeList[0].Value+ds);
                ds0 := FTreeList[FTreeList.NoOfTrees-1].Value -FTreeList[0].Value;
                ResetBranch;
            end;
        end;

    Inc(FNoOfTreesExamined, 4);
end;

procedure TMETreeComputer.MakeTree;
var
  NJTreeComputer: TNJTreeComputer;
  i,j,aprogress,c,n : integer;
  flag : boolean;
begin
  if FUseInitialNJTree then
  begin
    NJTreeComputer := TNJTreeComputer.Create(FTree, FD);
    NJTreeComputer.MakeTree;
    NJTreeComputer.Free;

    if FMaxNoOfTrees > 1 then
      aprogress := trunc(MaxProgress/3)
    else
      aprogress := trunc(MaxProgress/2);
  end
  else
    aprogress := 0;

  try
    n := FTree.NoOfOTUs;

    InitNodes;

    ResetTree(GetSumOfOLSBranchLength(node, n, FD));

    FNoOfTreesExamined := 1;
    repeat
      c := 0; //SK mega2b4
      for i := n to 2*n-3 do
      begin
        flag := true;
        if node[i].flag then
            Continue;

        if SearchDt4Trees3(node[i], true) then
            flag := false;

        if SearchDt4Trees2(node[i], true) then
            flag := false;

        if SearchDt4Trees1(node[i], true) then
            flag := false;

        if SearchDt2Trees(node[i], true) then
            flag := false;

        node[i].flag := flag;

        c := 0;
        for j := n to 2*n-3 do
            if node[j].flag then Inc(c);

        if FMaxNoOfTrees > 1 then
          aprogress := trunc(MaxProgress/3 +c/(n-2)*MaxProgress/3)
        else
          aprogress := trunc(MaxProgress/2 +c/(n-2)*MaxProgress/2);

        if Assigned(CallBackFunc) then
          if CallBackFunc('Searching ME Tree', 0, aprogress) then
            break;

      end;
    until c = n-2;

    if FMaxNoOfTrees > 1 then
    begin
        for i := n to 2*n-3 do
        begin
            SearchDt2Trees(node[i], false);
            SearchDt4Trees1(node[i], false);
            SearchDt4Trees2(node[i], false);
            SearchDt4Trees3(node[i], false);

            if i > n then
                for j := n to i-1 do
                    RetrieveDt4Trees4(node[i], node[j]);

        aprogress := trunc(MaxProgress*2/3 +c/(n-2)*MaxProgress/3);
        if Assigned(CallBackFunc) then
          if CallBackFunc('Searching ME Tree', 0, aprogress) then
            break;
        end;
    end;
  except
    raise Exception.Create('ME tree making failed.');
  end;
end;

procedure TMETreeComputer.ComputeBLens;
var
  n: integer;
begin
  n := FTree.NoOfOTUs;
  InitNodes;
  ResetTree(GetSumOfOLSBranchLength(node, n, FD));
end;

procedure TMETreeComputer.SearchLastOTUPosition;
var
  i, n: integer;
begin
  n := FTree.NoOfOTUs;

  Threshold    := 1000;
  MaxNoOfTrees := 2*n-5;

  InitTree(node, FTree.NodeArray, n);
  ResetNodeFlag(node, n);
  ChangeRoot(node[2*n-2], node[n-2]);
  SetSib(node[2*n-2]);

  FTreeList.DeleteAll;
  for i := 0 to 2*n-2 do
  begin
    if i = n-1 then
      continue;
    if i = n-2 then
      continue;
    if i = node[n-1].anc.index then
      continue;
    if node[i].anc = nil then
      continue;

    if Assigned(FCallBackFunc) then
      FCallBackFunc('',0,0);

    RemoveBranch(node[n-1]);
    AddBranch(node[n-1].anc, node[n-1], node[i]);

    ResetNodeFlag(node, n);
    SetSib(node[2*n-2]);
    SetAllN(node[2*n-2].des2);
    SetAllDD(node, n, FD);

    SaveTree(GetSumOfOLSBranchLength(node, n, FD));
  end;
  if Assigned(FCallBackFunc) then
    FCallBackFunc('',0,0);
end;

function TMETreeComputer.MaxProgress: integer;
begin
  result := 100;
end;


//==============================================================================
//TBootstrapUserTreeDistThread
//==============================================================================

constructor TBootstrapUserTreeDistThread.Create(ATreeList: TTreeList;    // TreeList must contain a tree.
                                      ADmat: PDistanceMatrix);
begin
  inherited Create(ATreeList, ADmat);

  NoOfThreads := 1;
  OnBootstrapDoneProc := nil;
end;

//procedure TBootstrapUserTreeDistThread.DoOnBootstrapProgress;
//begin
//  FProgressDlg.Progress := Trunc(FProgress/NoOfBootstraps*100);
//{$IFDEF VISUAL_BUILD}
//  FProgressDlg.Refresh;
//  {$ENDIF}
//end;


procedure TBootstrapUserTreeDistThread.ComputeBLens;
begin
  TMETreeComputer(FTreeComputer).ComputeBLens;
{
  SetAllDD(node, FTree.NoOfOTUs, FD);
  GetSumOfOLSBranchLength(node, FTree.NoOfOTUs, FD); // this computes BLens
}
end;

procedure TBootstrapUserTreeDistThread.Search;
var
  ATreeComputer: TTreeComputer;
  ATreeList: TTreeList;
  i, j, NBranches, root, n: Integer;
  OriBL, BootBL, BLx, BLxx: array of Double;
  aAnalysisInfo: TAnalysisInfo = nil;
begin
{
  if UseInitialNJTree then
  begin
    ShowProgress := false;
    inherited Search; // makes NJ tree
    if Terminated then Exit;
  end;
}
  if InitialTree <> itUser then
  begin
    ShowProgress := false;
    if InitialTree = itNJ then
    begin
      ATreeComputer := TNJTreeComputer.Create(FTree, FD);
      ATreeComputer.MakeTree;
      FreeAndNil(ATreeComputer);
    end
    else if InitialTree = itME then
    begin
      ATreeList := TTreeList.Create;
      ATreeList.Add(FTree);
      ATreeComputer := TMETreeComputer.Create(ATreeList, FD);
      TMETreeComputer(ATreeComputer).UseInitialNJTree := true;
      ATreeComputer.MakeTree;
      FreeAndNil(ATreeComputer);
      ATreeList[0] := nil;
      FreeAndNil(ATreeList);
    end;
    if Terminated then Exit;
  end;

  FTreeList[0].isSE := true;
  FTreeList[0].isStats := true;

  try
    n := FTreeList[0].NoOfOtus;

//    SetLength(H, 2*n-1);
//    for i := 0 to 2*n-2 do
//        H[i] := i;
{
    InitTree(node, FTreeList[0].NodeArray, n);

    root := -1; //@SK mega2b4
    for i := n to 2*n-2 do
        if node[i].anc = nil then root := i;

    if node[n-1].anc.index <> root then
    begin
      p := node[n-1];
      while p.anc <> nil do
      begin
        H[p.index] := p.anc.index;
        p := p.anc;
      end;
    end;

    ResetNodeFlag(node, n);
    ChangeRoot(node[2*n-2], node[n-1]);
    SetSib(node[2*n-2]);
    SetAllN(node[2*n-2].des2);

    BootBL := nil;
    OriBL  := nil;
  //
}

    NBranches := 2*FTreeList[0].NoOfOTUs-2;
    root := FTreeList[0].RootIndex;

    SetLength(OriBL, NBranches);
    SetLength(BootBL,NBranches);
    SetLength(BLx, NBranches);
    SetLength(BLxx,NBranches);
    for j:=0 to NBranches-1 do
    begin
      BLx[j]   := 0;
      BLxx[j]  := 0;
    end;

    ComputeBLens; // inherited function

    for j := 0 to NBranches-1 do
      if (j <> root) then
        OriBL[j] := FTree.BLen[j];

    FStartTime := Now;
    for i := 1 to NoOfBootstraps do
    begin
//      if not BootstrapDistanceProc(FProgressDlg) then

      if not FBootstrapFunc then
        continue;

      ComputeBLens;

      for j := 0 to NBranches-1 do
        if (j <> root) then
            BootBL[j] := FTree.BLen[j];
        for j:=0 to NBranches-1 do
        begin
          BLx[j]   := BLx[j]  + BootBL[j]/NoOfBootstraps;
          BLxx[j]  := BLxx[j] + BootBL[j]*BootBL[j]/NoOfBootstraps;
        end;

      OnBootstrapProgress(trunc(i/NoOfBootstraps*100));
{
      if Terminated then Break;
      FProgress := i;
      if FProgressDlg <> nil then
        Synchronize(DoOnBootstrapProgress);
}
    end;

    //==============
    for j:=0 to NBranches-1 do
    begin
      FTreeList[0].BLen[j] := OriBL[j];
      if (BLxx[j]- BLx[j]*BLx[j]) <= 0 then
      begin
        FTreeList[0].SE[j] := 0;
        if BLxx[j] <= 0 then
          FTreeList[0].Stats[j] := 0
        else
          FTreeList[0].Stats[j] := 99.99;
      end
      else
      begin
        FTreeList[0].SE[j]    := Sqrt(NoOfBootstraps*(BLxx[j]- BLx[j]*BLx[j])/(NoOfBootstraps-1));
        if FTreeList[0].BLen[j] <= 0 then
          FTreeList[0].Stats[j] := 0
        else if FTreeList[0].SE[j] <= 0.0000000000001 then
          FTreeList[0].Stats[j] := 99.99
        else
          FTreeList[0].Stats[j] := 100 -100*2*zTest(FTreeList[0].BLen[j]/FTreeList[0].SE[j]);
      end;
    end;
    FTreeList[0].SE[FTreeList[0].Node[root-n].des1] := Max(FTreeList[0].SE[FTreeList[0].Node[root-n].des1],FTreeList[0].SE[FTreeList[0].Node[root-n].des2]);
    FTreeList[0].SE[FTreeList[0].Node[root-n].des2] := FTreeList[0].SE[FTreeList[0].Node[root-n].des1];
    FTreeList[0].Stats[FTreeList[0].Node[root-n].des1] := Max(FTreeList[0].Stats[FTreeList[0].Node[root-n].des1],FTreeList[0].Stats[FTreeList[0].Node[root-n].des2]);
    //if FtreeList[0].stats[FTreeList[0].Node[root-NoOfOTUs].des1] = 624 then ShowMessage('error exists, second');
    FTreeList[0].Stats[FTreeList[0].Node[root-n].des2] := FTreeList[0].Stats[FTreeList[0].Node[root-n].des1];
    //if FTreeList[0].Stats[FTreeList[0].Node[root-NoOfOTUs].des2] = 624 then ShowMessage('error exists, third');
    FTreeList.MaxStats := 100;

    {$IFDEF VISUAL_BUILD}
    if Assigned(Info) then
    begin
      aAnalysisInfo := TAnalysisInfo(Info);
      ComputeSiteCoverage(aAnalysisInfo);
    end;
    {$ENDIF}
  finally
    OriBL  := nil;
    BootBL := nil;
    BLx := nil;
    BLxx := nil;
//    H := nil;
  end;
end;


{ TOLSBLenThread }

constructor TOLSBLenThread.Create(ATreeList: TTreeList; ADmat: PDistanceMatrix);
begin
  inherited Create(ATreeList, ADmat);

  UseInitialNJTree := false;
end;

procedure TOLSBLenThread.Search;
var
  aAnalysisInfo: TAnalysisInfo = nil;
begin
  ComputeBLens;
  {$IFDEF VISUAL_BUILD}
  if Assigned(Info) then
  begin
    aAnalysisInfo := TAnalysisInfo(Info);
    ComputeSiteCoverage(aAnalysisInfo);
  end;
  {$ENDIF}
end;

function TOLSBLenThread.AnalysisDescription: String;
begin
  Result := 'OLS tree analysis';
end;

{ TOLSSearchLastPlaceThread }

constructor TOLSSearchLastOTUPositionThread.Create(ATreeList: TTreeList; ADmat: PDistanceMatrix);
begin
  inherited;

  UseInitialNJTree := false;
end;

procedure TOLSSearchLastOTUPositionThread.Search;
//var
//  i,CurProgress,n: integer;
begin
  TMETreeComputer(FTreeComputer).SearchLastOTUPosition;
//  n := FTree.NoOfOTUs;
//  CurProgress := 0;
//
//  FThreshold    := 1000;
//  FMaxNoOfTrees := 2*n-5;
//  MaxProgress   := 100;
//
//  InitTree(node, FTree.NodeArray, n);
//  ResetNodeFlag(node, n);
//  ChangeRoot(node[2*n-2], node[n-2]);
//  SetSib(node[2*n-2]);
//
// FTreeList.DeleteAll;
//  for i := 0 to 2*n-2 do
//  begin
//    if i = n-1 then
//      continue;
//    if i = n-2 then
//     continue;
//    if i = node[n-1].anc.index then
//      continue;
//    if node[i].anc = nil then
//      continue;
//
//   {$IFNDEF VISUAL_BUILD}
//    ShowProgressIncrementStatic;
//    {$ENDIF}
//    RemoveBranch(node[n-1]);
//    AddBranch(node[n-1].anc, node[n-1], node[i]);
//
//   ResetNodeFlag(node, n);
//    SetSib(node[2*n-2]);
//    SetAllN(node[2*n-2].des2);
//    SetAllDD(node, n, FD);
//
//    SaveTree(GetSumOfOLSBranchLength(node, n, FD));
//
//    CurProgress := trunc((i+1)/(2*n-5)*100);
//  end;
//  {$IFNDEF VISUAL_BUILD}
//  ShowProgressIncrementStatic;
//  {$ENDIF}
end;


{ TGeneDuplicationThread }

function TGeneDuplicationThread.CountDups(GeneTree: TSimpleTreeNodeArray): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := FGTNoOfOtus to Length(GeneTree) - 1 do
  begin
    if GeneTree[i].IsDuplicationEvent then
      inc(Result);
  end;
end;

constructor TGeneDuplicationThread.Create(StartFrom: TStartGeneDupsSearchFrom; AOwner: TObject);
begin
  inherited Create;
  GeneTreeIsRooted := False;
  UsingSpeciesTree := False;
  FGeneTreeList := nil;
  FSpeciesTreeList := nil;
  FStartingFrom := StartFrom;
  FSearchingAllTrees := False;
  FErrorMsg := EmptyStr;
end;

function TGeneDuplicationThread.CreateNewSimpleTreeNodeArray(NumNodes: Integer): TSimpleTreeNodeArray;
var
  i: Integer;
  ANode: TSimpleTreeNode;
begin
  SetLength(Result, NumNodes);
  for i := 0 to NumNodes - 1 do
  begin
    ANode := TSimpleTreeNode.Create;
    ANode.NodeIndex := i;
    Result[i] := ANode;
  end;
end;

destructor TGeneDuplicationThread.Destroy;
var
  i: Integer;
begin
  if Length(FGeneTree) > 0 then
    for i := 0 to Length(FGeneTree) - 1 do
      if Assigned(FGeneTree[i]) then
        FreeAndNil(FGeneTree[i]);

  if Length(FSpeciesTree) > 0 then
  for i := 0 to Length(FSpeciesTree) - 1 do
    if Assigned(FSpeciesTree[i]) then
      FreeAndNil(FSpeciesTree[i]);

  if Assigned(FSpeciesTreeList) then
    FSpeciesTreeList.Free;

  if Assigned(FGeneTreeList) then
    FGeneTreeList.Free;
  inherited;
end;

procedure TGeneDuplicationThread.DestroySimpleTreeNodeArray(ATree: TSimpleTreeNodeArray);
var
  i: Integer;
begin
  if Length(ATree) > 0 then
    for i := 0 to Length(ATree) - 1 do
      FreeAndNil(ATree[i]);
  SetLength(ATree, 0);
end;

procedure TGeneDuplicationThread.DoDrawGeneDupsEvent;
begin
    DrawGeneDupsEvent(FGeneTree); { this is the easy case where we can just mark the nodes appropriately}
end;

procedure TGeneDuplicationThread.DoOnProgress;
begin
  if Assigned(FProgressDlg) then
  begin
    if FShowProgress then
  	  FProgressDlg.Progress := FProgress
    else
    begin
      FProgressDlg.AddRunStatusInfo('Searching Gene Tree', Format('%3d%%', [FProgress]));
    end;
  end;
end;

function TGeneDuplicationThread.FindAncestralSpecies(GeneTreeNode: TSimpleTreeNode): TSimpleTreeNode;
var
  i: Integer;
  CurrDepth: Integer;
begin
  Result := nil;
  if GeneTreeNode.IsOtu then
  begin
    for i := 0 to FSTNoOfOtus - 1 do
    begin
      if SameText(GeneTreeNode.SpeciesName, FSpeciesTree[i].SpeciesName) then
      begin
        Result := FSpeciesTree[i];
        Exit;
      end;
    end;
  end
  else
  begin
    Result := FSpeciesTreeRoot;
    CurrDepth := 1;
    for i := (Length(FSpeciesTree) - 1) downto (FSTNoOfOtus - 2) do {}
    begin
      if FSpeciesTree[i].Depth < CurrDepth then
        continue; // we only want to compare nodes that are deeper in the tree
      if FSpeciesTree[i].ExtantSpecies.Count < GeneTreeNode.ExtantSpecies.Count then
        continue; // then we know that GeneTreeNode.ExtantSpecies will not be a subset so don't bother comparing
      if IsSubsetOf(GeneTreeNode.ExtantSpecies, FSpeciesTree[i].ExtantSpecies) then
      begin
        Result := FSpeciesTree[i];
        CurrDepth := FSpeciesTree[i].Depth;
      end;
    end;
  end;
end;

function TGeneDuplicationThread.FindGeneTreeRoot(GeneTree: TSimpleTreeNodeArray): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(GeneTree) - 1 do
    if GeneTree[i].Ancestor = nil then
    begin
      Result := i;
      Exit;
    end;
end;

function TGeneDuplicationThread.FindSpeciesTreeRoot(ASpeciesTree: TSimpleTreeNodeArray): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(ASpeciesTree) - 1 do
    if ASpeciesTree[i].Ancestor = nil then
    begin
      Result := i;
      Exit;
    end;
end;

procedure TGeneDuplicationThread.FindSpeciesTreeRoot;
var
  Index: Integer;
begin
  Index := FindSpeciesTreeRoot(FSpeciesTree);
  FSpeciesTreeRoot := FSpeciesTree[Index];

end;

procedure TGeneDuplicationThread.FindGeneTreeRoot;
var
  Index: Integer;
begin
  Index := FindGeneTreeRoot(FGeneTree);
  FGeneTreeRoot := FGeneTree[Index];
end;

function TGeneDuplicationThread.HasASameString(First: TStringList;
  Second: TStringList): Boolean;
var
  i, Index: Integer;
begin
  Result := True;
  for i := 0 to First.Count - 1 do
  begin
    if Second.Find(First[i], Index) then
      Exit;
  end;
  Result := False;
end;

procedure TGeneDuplicationThread.InitAncSpeciesIndices(AGeneTree: TSimpleTreeNodeArray; ASpeciesTree: TSimpleTreeNodeArray);
var
  i, j: Integer;
begin
  for i := 0 to FGTNoOfOtus - 1 do
  begin
    for j := 0 to FSTNoOfOtus - 1 do
    begin
      if AGeneTree[i].SpeciesName = ASpeciesTree[j].SpeciesName then
      begin
        AGeneTree[i].AncSpeciesIndex := ASpeciesTree[j].TempIndex;
        Break;
      end;
    end;
  end;
end;

procedure TGeneDuplicationThread.InitGeneTree;
var
  i: Integer;
  Data: TTreeData;
begin
  Data := FGeneTreeList[0];
  FGTNoOfOtus := Data.NoOfOTUs;
  SetLength(FGeneTree, 2 * FGTNoOfOtus - 1);

  for i := 0 to Length(FGeneTree) - 1 do
  begin
    FGeneTree[i] := TSimpleTreeNode.Create;
    FGeneTree[i].NodeIndex := i;
  end;

  for i := 0 to FGTNoOfOtus - 2 do
  begin
    FGeneTree[FGTNoOfOtus+i].Des1 := FGeneTree[Data[i].Des1];
    FGeneTree[FGTNoOfOtus+i].Des2 := FGeneTree[Data[i].Des2];
    FGeneTree[Data[i].Des1].Ancestor := FGeneTree[FGTNoOfOtus+i];
    FGeneTree[Data[i].Des2].Ancestor := FGeneTree[FGTNoOfOtus+i];

//    if (i mod 50) = 0 then
//    begin
//      EstProgress := trunc(i / (n - 2) * 100);
//      if Assigned(ProgressProc) then
//        ProgressProc(trunc(i / (n - 2) * 100));
//    end;
  end;

  for i := 0 to 2 * FGTNoOfOtus - 2 do
    if FGeneTree[i].Ancestor = nil then
    begin
      FGeneTreeRoot := FGeneTree[i];
      break;
    end;

  for i := 0 to FGTNoOfOtus - 1 do
  begin
    FGeneTree[i].SpeciesName := LowerCase(FGeneTreeList.SpeciesName[i]);
    FGeneTree[i].IsOtu := True;
  end;
end;

procedure TGeneDuplicationThread.Initialize(GeneTree, SpeciesTree: TSimpleTreeNodeArray; GeneTreeList, SpeciesTreeList: TTreeList);
var
  Data: TTreeData;
  SpNames: TStringList;
begin
  FGeneTree := GeneTree;
  if not GeneTreeIsRooted then
  begin
    Data := GeneTreeList[0];
    SpNames := GeneTreeList.SpeciesNamesList;
    UpdateGeneTree(Data, SpNames);
  end;
  FGeneTreeList := GeneTreeList;

  if Assigned(SpeciesTree) then
  begin
    FSpeciesTree := SpeciesTree;
    FindSpeciesTreeRoot;
  end;
  if Assigned(SpeciesTreeList) then
  begin
    FSpeciesTreeList := SpeciesTreeList;
    FSTNoOfOtus := FSpeciesTreeList.NoOfOTUs;
  end;
  FindGeneTreeRoot;
end;

procedure TGeneDuplicationThread.InitProgress;
begin
  ProgressDlg.AddAnalysisOptions('Analysis', 'Gene duplication inferrence');
  ProgressDlg.AddAnalysisOptions('No. of taxa', IntToStr(FGTNoOfOtus));
  ProgressDlg.AddAnalysisOptions('Gene tree is rooted', BoolToStr(GeneTreeIsRooted, True));
  ProgressDlg.AddAnalysisOptions('Using species tree', BoolToStr(UsingSpeciesTree, True));
end;

procedure TGeneDuplicationThread.InitGeneTree(SpNames: TStringList);
var
  i: Integer;
begin
  InitGeneTree;
  for i := 0 to FGTNoOfOtus - 1 do
  begin
    FGeneTree[i].SpeciesName := LowerCase(SpNames[i]);
    FGeneTree[i].IsOtu := True;
  end;
end;

procedure TGeneDuplicationThread.InitSpeciesNamesList(RootNode: TSimpleTreeNode);

  procedure InitNamesPostOrder(ANode: TSimpleTreeNode);
  begin
    if ANode.IsOtu then
      Exit;

    InitNamesPostOrder(ANode.Des1);
    InitNamesPostOrder(ANode.Des2);

    if ANode.Des1.ExtantSpecies.Count = 0 then
      ANode.Des1.ExtantSpecies.Add(ANode.Des1.SpeciesName);
    ANode.ExtantSpecies.AddStrings(ANode.Des1.ExtantSpecies);

    if ANode.Des2.ExtantSpecies.Count = 0 then
      ANode.Des2.ExtantSpecies.Add(ANode.Des2.SpeciesName);
    ANode.ExtantSpecies.AddStrings(ANode.Des2.ExtantSpecies);
  end;

begin
  InitNamesPostOrder(RootNode);
end;

procedure TGeneDuplicationThread.InitSpeciesNamesLists;
begin
  InitSpeciesNamesList(FSpeciesTreeRoot);
  InitSpeciesNamesList(FGeneTreeRoot);
end;

procedure TGeneDuplicationThread.InitSpeciesTree(TreeList: TTreeList);
var
  i: Integer;
  Data: TTreeData;
begin
  Data := FSpeciesTreeList[0];
  FSTNoOfOtus := Data.NoOfOTUs;
  SetLength(FSpeciesTree, 2 * FSTNoOfOtus - 1);

  for i := 0 to Length(FSpeciesTree) - 1 do
  begin
    FSpeciesTree[i] := TSimpleTreeNode.Create;
    FSpeciesTree[i].NodeIndex := i;
  end;

  for i := 0 to FSTNoOfOtus - 2 do
  begin
    FSpeciesTree[FSTNoOfOtus+i].Des1 := FSpeciesTree[Data[i].Des1];
    FSpeciesTree[FSTNoOfOtus+i].Des2 := FSpeciesTree[Data[i].Des2];
    FSpeciesTree[Data[i].Des1].Ancestor := FSpeciesTree[FSTNoOfOtus+i];
    FSpeciesTree[Data[i].Des2].Ancestor := FSpeciesTree[FSTNoOfOtus+i];
  end;

  for i := 0 to 2 * FSTNoOfOtus - 2 do
    if FSpeciesTree[i].Ancestor = nil then
    begin
      FSpeciesTreeRoot := FSpeciesTree[i];
      break;
    end;

  for i := 0 to FSTNoOfOtus - 1 do
  begin
    FSpeciesTree[i].SpeciesName := LowerCase(FSpeciesTreeList.SpeciesName[i]);
    FSpeciesTree[i].IsOtu := True;
  end;
  UsingSpeciesTree := True;
end;

procedure TGeneDuplicationThread.InitSpeciesTree(SpNames: TStringList);
var
  i: Integer;
begin
  InitSpeciesTree;
  for i := 0 to FSTNoOfOtus - 1 do
  begin
    FSpeciesTree[i].SpeciesName := LowerCase(SpNames[i]);
    FSpeciesTree[i].IsOtu := True;
  end;
end;

procedure TGeneDuplicationThread.InitToUseTrees(GeneTree: TSimpleTreeNodeArray; GeneTreeList, SpeciesTreeList: TTreeList);
begin
  FGeneTree := GeneTree;
  FindGeneTreeRoot;
  FGeneTreeList := TTreeList.Create;
  FGeneTreeList.Assign(GeneTreeList);
  FGeneTreeList.IsRooted := True;
  if Assigned(SpeciesTreeList) then
  begin
    FSpeciesTreeList := TTreeList.Create;
    FSPeciesTreeList.Assign(SpeciesTreeList);
    InitSpeciesTree;
  end;
end;

function TGeneDuplicationThread.IsSubsetOf(SmallList: TStringList;
  BigList: TStringList): Boolean;
var
  i: Integer;
  TempInt: Integer;
begin
  TempInt := 0;
  Result := False;
  for i := 0 to SmallList.Count - 1 do
  begin
    if not BigList.Find(SmallList[i], TempInt) then
      Exit;
  end;
  Result := True;
end;

procedure TGeneDuplicationThread.LoadGeneTreesToTreeList(AList: TSimpleTreeNodeArrayArray);
var
  TempData: TTreeData;
  i: Integer;
begin
  if Length(AList) > 0 then
  begin
    FGeneTreeList.DeleteAll;
    for i := 0 to Length(AList) - 1 do
    begin
      TempData := TTreeData.Create(FGTNoOfOtus, False, False, False);
      UpdateTreeData(TempData, AList[i]);
      FGeneTreeList.Add(TempData);
    end;
    FGeneTreeList.isRooted := True;
  end;
end;

function TGeneDuplicationThread.MinDups(DupsArray: TIntArray): Integer;
var
  i: Integer;
begin
  Result := DupsArray[0];
  if Length(DupsArray) = 1 then
    Exit;
  for i := 1 to Length(DupsArray) - 1 do
  begin
    if DupsArray[i] < Result then
      Result := DupsArray[i];
  end;
end;

procedure TGeneDuplicationThread.OnProgress(AProgress: Integer);
begin
  FProgress := AProgress;
  if Assigned(FProgressDlg) then
    try
      Synchronize(DoOnProgress);
    except
    {$IFDEF DEBUG}
      {$IFDEF VISUAL_BUILD}
      on E: Exception do
        ShowMessage('Developer warning; Error in OnProgress: ' + E.Message);
      {$ENDIF}
    {$ENDIF}
    end;
end;

function TGeneDuplicationThread.AnalysisDescription: String;
begin
  Result := 'Gene duplication search';
end;

procedure TGeneDuplicationThread.Search;
begin
  try
    if GeneTreeIsRooted then
    begin
      if UsingSpeciesTree then
        SearchWithSpeciesTree
      else
        SearchWithoutSpeciesTree;
    end
    else
    begin
      if UsingSpeciesTree then
        SearchAllNodesWithSpeciesTree
      else
        SearchAllNodesWithoutSpeciesTree;
    end;
    FGeneTreeList.IsRooted := True;
    OnProgress(100);
  except
    on E:Exception do
    begin
      FIsSuccess := False;
      FErrorMsg := E.Message;
      Terminate;
      {$IFNDEF VISUAL_BUILD}
      error_nv('gene duplication inference failed', E);
      {$ENDIF}
    end;
  end;
end;

procedure TGeneDuplicationThread.SearchAllNodesWithoutSpeciesTree;
var
  i: Integer;
  TempTree: TSimpleTreeNodeArray;
  TempRoot: TSimpleTreeNode;
  TempList: TSimpleTreeNodeArrayArray; // array of TSimpleTreeNodeArray configurations which have the minimum number of gene duplications
  MinNumDups: Integer = MaxInt;
  NumNodes: Integer;
  NodesSearched: Integer;
  NodesToSearch: Integer;
  Cancelled: Boolean;
  LastUpdateTime: TDateTime;
  LastAdded: Integer; { so we can be sure to free temp tree if it does not get added to the temp list}

  procedure ClearTempList;
  var
    k: Integer;
  begin
    if Length(TempList) > 0 then
    begin
      for k := Low(TempList) to High(TempList) do
      begin
        DestroySimpleTreeNodeArray(TempList[k]);
      end;
      SetLength(TempList, 0);
    end;
  end;

  procedure AddTree(ATree: TSimpleTreeNodeArray);
  begin
    SetLength(TempList, Length(TempList) + 1);
    TempList[Length(TempList) - 1] := ATree;
  end;

  function FindRoot: TSimpleTreeNode;
  var
    k: Integer;
  begin
    Result := nil;
    for k := Low(TempTree) to High(TempTree) do
    begin
      if TempTree[k].Ancestor = nil then
      begin
        Result := TempTree[k];
        Result.IsRoot := True;
        break;
      end;
    end;
  end;

begin
  FSearchingAllTrees := True;
  Cancelled := False;
  SetLength(TempList, 0);
  OnProgress(0);
  LastUpdateTime := Now;

  try
    NumNodes := 2 * FGTNoOfOtus - 1;
    NodesSearched := 0; { this is just for calculating progress}
    NodesToSearch := NumNodes - FGTNoOfOtus;
    TempTree := CreateNewSimpleTreeNodeArray(NumNodes);
    for i := FGTNoOfOtus to NumNodes - 1 do
    begin
      if (MilliSecondsBetween(Now, LastUpdateTime) > 500) then
      begin
        if Terminated then
        begin
          Cancelled := True;
          break;
        end
        else
        begin
          OnProgress(Trunc((NodesSearched / NodesToSearch) * 100));
          LastUpdateTime := Now;
        end;
      end;
      inc(NodesSearched);
      CloneTreeNodeArray(FGeneTree, TempTree);
      TempRoot := FindRoot;
      ChangeRootSimple(TempRoot, TempTree[i]);
      TempRoot := FindRoot;
      SearchWithoutSpeciesTree(TempRoot);
      if i <> FGTNoOfOtus then
      begin
        if CountDups(TempTree) < MinNumDups then // then this is better than all previous ones so get rid of them and add this one to the list
        begin
          MinNumDups := CountDups(TempTree);
          ClearTempList; // clears the list and deallocates the memory that was used
          AddTree(TempTree);
          LastAdded := i;
          TempTree := CreateNewSimpleTreeNodeArray(NumNodes);
        end
        else if CountDups(TempTree) = MinNumDups then // then this is at least as good as any previous ones so add it to the list
        begin
          AddTree(TempTree);
          LastAdded := i;
          TempTree := CreateNewSimpleTreeNodeArray(NumNodes);
        end;
      end
      else // then this is the first one searched so by default it is the best so far
      begin
        MinNumDups := CountDups(TempTree);
        AddTree(TempTree);
        LastAdded := i;
        TempTree := CreateNewSimpleTreeNodeArray(NumNodes);
      end;
      { TODO 1 -oglen -cgenedups : what if we find one with zero dups? Then we should just quit? }
    end;
    if not Cancelled then
    begin
      LoadGeneTreesToTreeList(TempList);
      FIsSuccess := True;
    end;
  finally
    if Length(TempList) > 0 then
      ClearTempList;
    if LastAdded < NumNodes - 1 then
      DestroySimpleTreeNodeArray(TempTree);
  end;
end;

procedure TGeneDuplicationThread.SearchAllNodesWithSpeciesTree;
var
  i: Integer;
  TempTree: TSimpleTreeNodeArray;
  TempRoot: TSimpleTreeNode;
  TempList: TSimpleTreeNodeArrayArray;
  MinNumDups: Integer = MaxInt;
  NumDups: Integer;
  NumNodes: Integer;
  NodesSearched: Integer;
  NodesToSearch: Integer;
  Cancelled: Boolean;
  LastUpdateTime: TDateTime;
  LastAdded: Integer; { so we can be sure to free temp tree if it does not get added to the temp list}

  procedure ClearTempList;
  var
    k: Integer;
  begin
    if Length(TempList) > 0 then
    begin
      for k := Low(TempList) to High(TempList) do
        DestroySimpleTreeNodeArray(TempList[k]);
      SetLength(TempList, 0);
    end;
  end;

  procedure AddTree(ATree: TSimpleTreeNodeArray);
  begin
    SetLength(TempList, Length(TempList) + 1);
    TempList[Length(TempList) - 1] := ATree;
  end;

  function FindRoot: TSimpleTreeNode;
  var
    k: Integer;
  begin
    Result := nil;
    for k := Low(TempTree) to High(TempTree) do
    begin
      if TempTree[k].Ancestor = nil then
      begin
        Result := TempTree[k];
        Result.IsRoot := True;
        break;
      end;
    end;
  end;

begin
  FSearchingAllTrees := True;
  Cancelled := False;
  SetLength(TempList, 0);
  OnProgress(0);
  LastUpdateTime := Now;

  try
    NumNodes := 2 * FGTNoOfOtus - 1;
    NodesSearched := 0; { this is just for calculating progress}
    NodesToSearch := NumNodes - FGTNoOfOtus;
    TempTree := CreateNewSimpleTreeNodeArray(NumNodes);
    for i := FGTNoOfOtus to NumNodes - 1 do
    begin
      if (MilliSecondsBetween(Now, LastUpdateTime) > 500) then
      begin
        if Terminated then
        begin
          Cancelled := True;
          break;
        end
        else
        begin
          OnProgress(Trunc((NodesSearched / NodesToSearch) * 100));
          LastUpdateTime := Now;
        end;
      end;
      inc(NodesSearched);
      CloneTreeNodeArray(FGeneTree, TempTree);
      TempRoot := FindRoot;
      ChangeRootSimple(TempRoot, TempTree[i]);
      TempRoot := FindRoot;
      SearchWithSpeciesTree(TempTree, FSpeciesTree, TempRoot, FSpeciesTreeRoot, (i = FGTNoOfOtus));
      if i <> FGTNoOfOtus then
      begin
        NumDups := CountDups(TempTree);
        if NumDups < MinNumDups then // then this is better than all previous ones so get rid of them and add this one to the list
        begin
          MinNumDups := NumDups;
          ClearTempList; // clears the list and deallocates the memory that was used
          AddTree(TempTree);
          LastAdded := i;
          TempTree := CreateNewSimpleTreeNodeArray(NumNodes);
        end
        else if NumDups = MinNumDups then // then this is at least as good as any previous ones so add it to the list
        begin
          AddTree(TempTree);
          LastAdded := i;
          TempTree := CreateNewSimpleTreeNodeArray(NumNodes);
        end;
      end
      else // then this is the first one searched so by default it is the best so far
      begin
        MinNumDups := CountDups(TempTree);
        AddTree(TempTree);
        LastAdded := i;
        TempTree := CreateNewSimpleTreeNodeArray(NumNodes);
      end;
      { TODO 1 -oglen -cgenedups : what if we find one with zero dups? Then we should just quit? }
    end;
    if not Cancelled then
    begin
      LoadGeneTreesToTreeList(TempList);
      FIsSuccess := True;
    end;
  finally
    if Length(TempList) > 0 then
      ClearTempList;
    if LastAdded < NumNodes - 1 then
      DestroySimpleTreeNodeArray(TempTree);
  end;
end;

procedure TGeneDuplicationThread.SearchWithoutSpeciesTree;
var
  TempData: TTreeData;
begin
  SearchWithoutSpeciesTree(FGeneTreeRoot);
  TempData := FGeneTreeList[0];
  UpdateTreeData(TempData);
  FIsSuccess := True;
end;

procedure TGeneDuplicationThread.SearchWithoutSpeciesTree(RootNode: TSimpleTreeNode);
var
  NodesProcessed: Integer;
  NumNodes: Integer;
  Cancelled: Boolean;
  LastUpdateTime: TDateTime;

  procedure ProcessNode(ANode: TSimpleTreeNode);
  begin
    NodesProcessed := NodesProcessed + 1;
    if ANode.IsOtu or Cancelled then
      Exit;
    if HasASameString(ANode.Des1.ExtantSpecies, ANode.Des2.ExtantSpecies) then
      ANode.IsDuplicationEvent := True;
    if (not FSearchingAllTrees) and (MilliSecondsBetween(Now, LastUpdateTime) > 500) then
    begin
      if Terminated then
      begin
        Cancelled := True;
      end
      else
      begin
        OnProgress(Trunc((NodesProcessed / NumNodes) * 100) - 10);
        LastUpdateTime := Now;
      end;
    end;
    ProcessNode(ANode.Des1);
    ProcessNode(ANode.Des2);
  end;

begin
  Cancelled := False;
  NodesProcessed := 0;
  NumNodes := (2 * FGTNoOfOtus - 1);
  LastUpdateTime := Now;

  if not FSearchingAllTrees then
    OnProgress(0);
  InitSpeciesNamesList(RootNode);
  if not FSearchingAllTrees then
    OnProgress(10);
  ProcessNode(RootNode);
end;

procedure TGeneDuplicationThread.SearchWithSpeciesTree( AGeneTree, ASpeciesTree: TSimpleTreeNodeArray; GTRootNode, STRootNode: TSimpleTreeNode; DoInitSpTree: Boolean=True);
var
  NodesProcessed: Integer;
  NumNodes: Integer;
  LastUpdateTime: TDateTime;
  Cancelled: Boolean;

  procedure ProcessNode(ANode: TSimpleTreeNode);
  var
    a, b: TSimpleTreeNode; // a = M(ANode.Des1) and b = M(ANode.Des2) e.g. ancestral species
    a1, b1: TSimpleTreeNode; // so we don't have to search for them more than 1 time
  begin
    NodesProcessed := NodesProcessed + 1;
    if ANode.IsOtu or Cancelled then
      Exit;
    ProcessNode(ANode.Des1);
    ProcessNode(ANode.Des2);
    a := FindAncestralSpecies(ANode.Des1);
    b := FindAncestralSpecies(ANode.Des2);
    a1 := a;
    b1 := b;
    while a <> b do
    begin
      if a.TempIndex > b.TempIndex then
        a := a.Ancestor
      else
        b := b.Ancestor;
    end;
    if (a = a1) or (a = b1) then
    begin
      ANode.IsDuplicationEvent := True;
      ANode.IsSpeciationEvent := False;
    end
    else
    begin
      ANode.IsDuplicationEvent := False;
      ANode.IsSpeciationEvent := True;
    end;
    if (not FSearchingAllTrees) and (MilliSecondsBetween(Now, LastUpdateTime) > 500) then
    begin
      if Terminated then
      begin
        Cancelled := True;
      end
      else
      begin
        OnProgress(Trunc((NodesProcessed / NumNodes) * 100) - 25);
        LastUpdateTime := Now;
      end;
    end;
  end;

begin
  Cancelled := False;
  NumNodes := (2 * FGTNoOfOtus - 1);
  NodesProcessed := 0;
  LastUpdateTime := Now;

  if not FSearchingAllTrees then
    OnProgress(0);
  if DoInitSpTree then
    UpdateTempIndices(STRootNode); // numbers the nodes in the species tree using a pre-order traversal
  if not FSearchingAllTrees then
    OnProgress(10);
  InitAncSpeciesIndices(AGeneTree, ASpeciesTree); // maps extant nodes in the gene tree to their corresponding nodes in the species tree
  if not FSearchingAllTrees then
    OnProgress(15);
  InitSpeciesNamesList(GTRootNode);
  if not FSearchingAllTrees then
    OnProgress(20);
  if DoInitSpTree then // for the case where we are trying every possible root in the gene tree, we don't need to setup the species tree every time
    InitSpeciesNamesList(STRootNode);
  if not FSearchingAllTrees then
    OnProgress(25);
  ProcessNode(GTRootNode);
  if not FSearchingAllTrees then
    OnProgress(100);
  FIsSuccess := True;
end;

procedure TGeneDuplicationThread.SearchWithSpeciesTree;
var
  TempData: TTreeData;
  NodesProcessed: Integer;
  NumNodes: Integer;
  LastUpdateTime: TDateTime;
  Cancelled: Boolean;

  procedure ProcessNode(ANode: TSimpleTreeNode);
  var
    a, b: TSimpleTreeNode; // a = M(ANode.Des1) and b = M(ANode.Des2) e.g. ancestral species
    a1, b1: TSimpleTreeNode; // so we don't have to search for them more than 1 time
  begin
    NodesProcessed := NodesProcessed + 1;
    if ANode.IsOtu or Cancelled then
      Exit;
    ProcessNode(ANode.Des1);
    ProcessNode(ANode.Des2);
    a := FindAncestralSpecies(ANode.Des1);
    b := FindAncestralSpecies(ANode.Des2);
    Assert(Assigned(a));
    Assert(Assigned(b)); { if they are both nil, processing would continue otherwise}

    a1 := a;
    b1 := b;
    while a <> b do
    begin
      if a.TempIndex > b.TempIndex then
        a := a.Ancestor
      else
        b := b.Ancestor;
    end;
    if (a = a1) or (a = b1) then
    begin
      ANode.IsDuplicationEvent := True;
      ANode.IsSpeciationEvent := False;
    end
    else
    begin
      ANode.IsDuplicationEvent := False;
      ANode.IsSpeciationEvent := True;
    end;
    if (not FSearchingAllTrees) and (MilliSecondsBetween(Now, LastUpdateTime) > 500) then
    begin
      if Terminated then
      begin
        Cancelled := True;
      end
      else
      begin
        OnProgress(Trunc((NodesProcessed / NumNodes) * 100) - 30);
        LastUpdateTime := Now;
      end;
    end;
  end;

begin
  Cancelled := False;
  NodesProcessed := 0;
  NumNodes := (2 * FGTNoOfOtus - 1);
  LastUpdateTime := Now;

  if not FSearchingAllTrees then
    OnProgress(0);
  UpdateTempIndices(FSpeciesTreeRoot); // numbers the nodes in the species tree using a pre-order traversal
  if not FSearchingAllTrees then
    OnProgress(10);
  InitAncSpeciesIndices(FGeneTree, FSpeciesTree); // maps extant nodes in the gene tree to their corresponding nodes in the species tree
  if not FSearchingAllTrees then
    OnProgress(20);
  InitSpeciesNamesLists;
  if not FSearchingAllTrees then
    OnProgress(30);
  ProcessNode(FGeneTreeRoot);
  TempData := FGeneTreeList[0];
  if not FSearchingAllTrees then
    OnProgress(95);
  UpdateTreeData(TempData);
  if not FSearchingAllTrees then
    OnProgress(100);
  FIsSuccess := True;
end;

procedure TGeneDuplicationThread.UpdateEmptyTreeData(var Data: TTreeData);
begin
  UpdateEmptyTreeData(Data, FGeneTree);
end;

procedure TGeneDuplicationThread.UpdateEmptyTreeData(var Data: TTreeData;const AGeneTree: TSimpleTreeNodeArray);
var
  i: Integer;
begin
  for i := FGTNoOfOtus to 2*FGTNoOfOtus-2 do
  begin
    Data.NodeArray[i-FGTNoOfOtus].des1 := AGeneTree[i].des1.NodeIndex;
    Data.NodeArray[i-FGTNoOfOtus].des2 := AGeneTree[i].des2.NodeIndex;
  end;
end;

procedure TGeneDuplicationThread.UpdateGeneTree(Data: TTreeData; SpNames: TStringList);
var
  i: Integer;
begin
  { update the otus}
  for i := 0 to FGTNoOfOtus  - 1 do
  begin
    FGeneTree[i].IsOtu := True;
    FGeneTree[i].SpeciesName := SpNames[i];
  end;

  { update the internal nodes}
  for i := FGTNoOfOtus to 2*FGTNoOfOtus-2 do
  begin
    FGeneTree[i].Des1 := FGeneTree[Data[i-FGTNoOfOtus].des1];
    FGeneTree[i].Des2 := FGeneTree[Data[i-FGTNoOfOtus].des2];
    FGeneTree[i].Des1.Ancestor := FGeneTree[i];
    FGeneTree[i].Des2.Ancestor := FGeneTree[i];
    FGeneTree[i].IsOtu := False;
  end;
end;

procedure TGeneDuplicationThread.UpdateTempIndices(RootNode: TSimpleTreeNode);

  procedure UpdateIndices(ANode: TSimpleTreeNode; Depth: Integer);
  begin
    ANode.TempIndex := TempIndex;
    inc(TempIndex);
    ANode.Depth := Depth;
    if Assigned(ANode.Des1) then
      UpdateIndices(ANode.Des1, Depth + 1);
    if Assigned(ANode.Des2) then
      UpdateIndices(ANode.Des2, Depth + 1);
  end;

begin
  TempIndex := 1;
  RootNode.TempIndex := TempIndex;
  RootNode.Depth := 1;
  inc(TempIndex);
  if Assigned(RootNode.Des1) then
    UpdateIndices(RootNode.Des1, RootNode.Depth + 1);
  if Assigned(RootNode.Des2) then
    UpdateIndices(RootNode.Des2, RootNode.Depth + 1);
end;

procedure TGeneDuplicationThread.UpdateTreeData(var Data: TTreeData);
var
  i: integer;
begin
  for i := FGTNoOfOtus to 2*FGTNoOfOtus-2 do
  begin
    Data.IsGeneDupEvent[FGeneTree[i].NodeIndex] := FGeneTree[i].IsDuplicationEvent;
    Data.IsSpeciationEvent[FGeneTree[i].NodeIndex] := FGeneTree[i].IsSpeciationEvent;
  end;
end;

procedure TGeneDuplicationThread.UpdateTreeData(var Data: TTreeData;const AGeneTree: TSimpleTreeNodeArray);
var
  i: integer;
begin
  for i := FGTNoOfOtus to 2*FGTNoOfOtus-2 do
  begin
    Data.NodeArray[i-FGTNoOfOtus].des1 := AGeneTree[i].des1.NodeIndex;
    Data.NodeArray[i-FGTNoOfOtus].des2 := AGeneTree[i].des2.NodeIndex;
    Data.IsGeneDupEvent[AGeneTree[i].NodeIndex] := AGeneTree[i].IsDuplicationEvent;
    Data.IsSpeciationEvent[AGeneTree[i].NodeIndex] := AGeneTree[i].IsSpeciationEvent;
  end;
end;

procedure TGeneDuplicationThread.UpdateTreeListNames;
var
  i: Integer;
begin
  for i := 0 to FGTNoOfOtus - 1 do
    FGeneTreeList.OTUName[i] := FGeneTree[i].SequenceName;
end;


procedure TGeneDuplicationThread.InitSpeciesTree;
var
  i: Integer;
  Data: TTreeData;
begin
  Data := FSpeciesTreeList[0];
  FSTNoOfOtus := Data.NoOfOTUs;
  SetLength(FSpeciesTree, 2 * FSTNoOfOtus - 1);

  for i := 0 to Length(FSpeciesTree) - 1 do
  begin
    FSpeciesTree[i] := TSimpleTreeNode.Create;
    FSpeciesTree[i].NodeIndex := i;
  end;

  for i := 0 to FSTNoOfOtus - 2 do
  begin
    FSpeciesTree[FSTNoOfOtus+i].Des1 := FSpeciesTree[Data[i].Des1];
    FSpeciesTree[FSTNoOfOtus+i].Des2 := FSpeciesTree[Data[i].Des2];
    FSpeciesTree[Data[i].Des1].Ancestor := FSpeciesTree[FSTNoOfOtus+i];
    FSpeciesTree[Data[i].Des2].Ancestor := FSpeciesTree[FSTNoOfOtus+i];
  end;

  for i := 0 to 2 * FSTNoOfOtus - 2 do
    if FSpeciesTree[i].Ancestor = nil then
    begin
      FSpeciesTreeRoot := FSpeciesTree[i];
      break;
    end;
  for i := 0 to FSTNoOfOtus - 1 do
  begin
    FSpeciesTree[i].SpeciesName := LowerCase(FSpeciesTreeList.SpeciesName[i]);
    FSpeciesTree[i].IsOtu := True;
  end;
  UsingSpeciesTree := True;
end;

end.
