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

unit MPTree;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
   Classes, Math, SysUtils, MTreeList, MTreeData, MPartitionList, MegaConsts,
   MTreeSearchThread, SyncObjs;

type
  TMPTree = class;

  TSearchMethod = (SPR, TBR);   // NNI: SPR w/ SearchLevel = 1

  TMPTreeNode = class
  private
    tree:  TMPTree;
    index: integer;

    anc:  TMPTreeNode;
    des1: TMPTreeNode;
    des2: TMPTreeNode;

    flag  : boolean;
    done  : boolean;

    InfoSite1: PArrayOfLongInt;
    InfoSite2: PArrayOfLongInt;
    InfoSite3: PArrayOfLongInt;

    TreeLen1: integer;
    TreeLen2: integer;
    TreeLen3: integer;
    Singletons: integer;

    function GetOTU: boolean;
    function GetNoOfInfoSites: integer;
  public
    property OTU: boolean read GetOTU;
    property NoOfInfoSites: integer read GetNoOfInfoSites;

    procedure Assign(source: TMPTreeNode);

    constructor Create(t: TMPTree; i: integer);
    destructor Destroy; override;
  end;

  TMPTreeNodeArray = array of TMPTreeNode;
  TCheckCancelFunc = function (Progress: integer; Status: AnsiString): boolean of object;

  TMPTree = class
    CheckCancel: TCheckCancelFunc;
    ProgressMessage: AnsiString;
  private
    NodeArray:  TMPTreeNodeArray;

    FNoOfSeqs :     integer;
    FNoOfInfoSites: integer;
    FNoOfInitTrees: integer;
    FMaxNoOfTrees: integer;
    FSearchLevel  : integer;
    FSearchMethod : TSearchMethod;

    FNoOfBits: integer;
    FNoOfSingletons: integer;
    FNoOfTotalSites: integer;

    IsInfoSite: array of boolean;
    FreqTable: PArrayOfInt;
    ParsimTrees: TPartitionList;
    InitTrees  : TTreeList;
    RandomOrder: boolean;

    procedure SetInfoSitesDNA(OriSeqs: TStringList);
    procedure SetInfoSitesAA(OriSeqs: TStringList);
    procedure SetInfoSitesDNAMap(MappedSites: TList);
    procedure SetInfoSitesAAMap(MappedSites: TList);

    procedure SetNoOfInitTrees(n :integer);
    procedure SetMaxNoOfTrees(n :integer);

//    procedure SaveParsimTree;

    function GetNoOfInfoSites: integer;
    function GetNoOfTrees: integer;
    function GetTreeLength: integer;
    function CountTreeLength:integer;
    function CountTreeLengthOfSubtree(node: TMPTreeNode):integer;
  public
    property NoOfBits: Integer read FNoOfBits;
    property NoOfInfoSites: integer read GetNoOfInfoSites;
    property NoOfSeqs: integer read FNoOfSeqs;
    property NoOfSingletons: integer read FNoOfSingletons;
    property NoOfTotalSites: integer read FNoOfTotalSites;

    property NoOfTrees: integer read GetNoOfTrees;
    property TreeLength: integer read GetTreeLength;
    property NoOfInitTrees: integer read FNoOfInitTrees write SetNoOfInitTrees;
    property MaxNoOfTrees: integer read FMaxNoOfTrees write SetMaxNoOfTrees;
    property SearchLevel: integer read FSearchLevel write FSearchLevel;
    property SearchMethod: TSearchMethod read FSearchMethod write FSearchMethod; // 0: SPR; 1: TBR

    procedure SetFreqTable(BootTable: PArrayOfInt);

    procedure MakeInitialTree;
    function SearchMPTree: boolean;

    procedure GetTreeData(tree: TTreeData; index: integer);
    procedure GetTreeList(treelist: TTreeList);

    procedure ComputeApproxBLens(tree: TTreeData);

    constructor Create(OriSeqs: TStringList; NoOfBits: integer); overload;
    constructor Create(MappedSites: TList; NoOfSites:integer; NoOfBits: integer); overload;

    destructor Destroy; override;
  end;

  TMPTreeSearchThread = class(TTreeSearchThread)
    CheckCancelFunc: TCheckCancelFunc;
  private
    MPTree: TMPTree;
    FTreeList: TTreeList;
    FStatus: string;

    procedure SetMaxNoOfTrees(n :integer);
    procedure SetNoOfInitTrees(n :integer);
    procedure SetSearchLevel(n :integer);
    procedure SetSearchMethod(m: TSearchMethod);
    procedure SetProgressMessage(str: AnsiString);

    function GetNoOfSeqs: integer;
    function GetNoOfTrees: integer;
    function GetMaxNoOfTrees: integer;
    function GetTreeLength: integer;
    function GetNoOfInfoSites: integer;
    function GetNoOfSingletons: integer;
    function GetNoOfTotalSites: integer;
    function GetProgressMessage: AnsiString;

    function GetSearchMethod: TSearchMethod;
    function GetNoOfInitTrees: integer;
    function GetSearchLevel: integer;
  protected
    FExceptionName: String;
    FExceptionMessage: String;
    FOriSeqs: TStringList;

    procedure Execute; override;
    procedure Search; override;
    procedure DoOnProgress; override;

    function CheckCancel(Progress: integer; Status: AnsiString): boolean; virtual;
  public
    property MyExceptionName: String read FExceptionName; // GS - added 12-22-2011, gives us a way to handle uncaught exceptions in the onterm procedure
    property MyExceptionMessage: String read FExceptionMessage; // GS - added 12-22-2011, gives us a way to handle uncaught exceptions in the onterm procedure
    property NoOfTrees: integer read GetNoOfTrees;
    property MaxNoOfTrees: integer read GetMaxNoOfTrees write SetMaxNoOfTrees;

    property NoOfSeqs: integer read GetNoOfSeqs;
    property NoOfInfoSites: integer read GetNoOfInfoSites;
    property NoOfSingletons: integer read GetNoOfSingletons;
    property NoOfTotalSites: integer read GetNoOfTotalSites;

    property NoOfInitTrees: integer read GetNoOfInitTrees write SetNoOfInitTrees;
    property SearchLevel: integer read GetSearchLevel write SetSearchLevel;
    property SearchMethod: TSearchMethod read GetSearchMethod write SetSearchMethod; // 0: SPR; 1: TBR
    property ProgressMessage: AnsiString read GetProgressMessage write SetProgressMessage;

    property TreeLength: integer read GetTreeLength;

    procedure GetTreeData(tree: TTreeData; index: integer);
    procedure GetTreeList(treelist: TTreeList);

    constructor Create(TreeList: TTreeList;
                       MappedSites: TList;
                       NoOfSites:integer;
                       NoOfBits: integer);

    destructor Destroy; override;
  end;

  TBootstrapMPTreeSearchThread = class;

  { TMPTreeSearchChildThread }

  TMPTreeSearchChildThread = class(TMPTreeSearchThread)
    protected
      FThreadIndex: Integer;
      FMainThread: TBootstrapMPTreeSearchThread;
      FDone: Boolean;
      procedure Search; override;
      procedure Execute; override;
      function BootstrapSites(aFreqTable: PArrayOfInt): Integer;
    public
      constructor Create(TreeList: TTreeList;
                         MappedSites: TList;
                         NoOfSites:integer;
                         NoOfBits: integer);
      destructor Destroy; override;
      property MainThread: TBootstrapMPTreeSearchThread read FMainThread write FMainThread;
      property ThreadIndex: Integer read FThreadIndex write FThreadIndex;
      property Done: Boolean read FDone;
  end;

  { TBootstrapMPTreeSearchThread }

  TBootstrapMPTreeSearchThread = class(TMPTreeSearchChildThread)
    BootstrapTrees: TPartitionList;
  private
    FMappedSites: TList;
    FNoOfBootstraps: integer;
    FCurrentRep: integer;
    FPartiallyCompleted: Boolean;

    FNoOfThreads: integer;
    FThreadMemSize: extended;

    FChildThread: array of TMPTreeSearchChildThread;
    FNoOfCurReps: integer;
  protected
    LockThread: TCriticalSection;
    procedure Search; override;
    procedure DoInitialSearch;
    procedure DoOnProgress; override;
    function EstimateThreadMemSize: Extended;
    function CheckCancel(Progress: integer; Status: AnsiString): boolean; override;
    function CheckChildThreadDone: Boolean;
    procedure Execute; override;
  public
    constructor Create(TreeList: TTreeList;
                       MappedSites: TList;
                       NoOfSites:integer;
                       NoOfBits: integer;
                       numThreads: Integer;
                       MaxPercentRam: Integer = DEFAULT_MAX_PERCENT_RAM);
    destructor Destroy; override;
    procedure AddBootTrees(trees: TPartitionList);
    function RepsCompleted: Integer;
    function GetCurrentRep: Integer;
    property NoOfBootstraps: integer read FNoOfBootstraps write FNoOfBootstraps;
    property PartiallyCompleted: Boolean read FPartiallyCompleted write FPartiallyCompleted;
    property NoOfThreads: Integer read FNoOfThreads write FNoOfThreads;
  end;

implementation

uses
  {$IFDEF VISUAL_BUILD}Mega_Main,{$ELSE}MD_MegaMain,{$ENDIF}
  Forms, MegaUtils, mmemutils,
  MGlobalSettings;

type

  TSwapData = record
    TreeLen: integer;
    node: array[0..2] of integer;
    root: integer;
  end;

////////////////////

constructor TMPTreeNode.Create(t: TMPTree; i: integer);
begin
  inherited Create;

  tree  := t;
  index := i;

  flag  := false;
  done  := false;

  InfoSite1 := nil;
  InfoSite2 := nil;
  InfoSite3 := nil;

  TreeLen1 := 0;
  TreeLen2 := 0;
  TreeLen3 := 0;

  Singletons := 0;
end;

destructor TMPTreeNode.Destroy;
begin
  if Assigned(InfoSite1) then
    FreeMem(InfoSite1);
  if Assigned(InfoSite2) then
    FreeMem(InfoSite2);
  if Assigned(InfoSite3) then
    FreeMem(InfoSite3);

  inherited;
end;

function TMPTreeNode.GetOTU: boolean;
begin
  result := (des1 = nil) or (des2 = nil);
end;

function TMPTreeNode.GetNoOfInfoSites: integer;
begin
  result := tree.FNoOfInfoSites;
end;

procedure TMPTreeNode.Assign(source: TMPTreeNode);
var
  i: Integer = 0;
  n: integer = 0;
begin
  tree := source.tree;

  TreeLen1 := source.TreeLen1;
  TreeLen2 := source.TreeLen2;
  TreeLen3 := source.TreeLen3;

  index  := source.index;
  anc    := source.anc;
  des1   := source.des1;
  des2   := source.des2;
  flag   := source.flag;

  n := tree.FNoOfInfoSites;
  if Self.InfoSite1 = nil then
    GetMem(Self.InfoSite1, SizeOf(LongInt)*n);
  if Self.InfoSite2 = nil then
    GetMem(Self.InfoSite2, SizeOf(LongInt)*n);
  if Self.InfoSite3 = nil then
    GetMem(Self.InfoSite3, SizeOf(LongInt)*n);

  for i := 0 to n-1 do
    InfoSite1[i] := source.InfoSite1[i];
  for i := 0 to n-1 do
    InfoSite2[i] := source.InfoSite2[i];
  for i := 0 to n-1 do
    InfoSite3[i] := source.InfoSite3[i];
end;

/////////////////////////

constructor TMPTree.Create(OriSeqs: TStringList; NoOfBits: integer);
var
  i: integer;
begin
  inherited Create;

  FNoOfTotalSites := length(OriSeqs[0]);
  setlength(IsInfoSite, FNoOfTotalSites);
  FNoOfSeqs       := OriSeqs.Count;
  setlength(NodeArray,  2*NoOfSeqs-1);
  for i := 0 to 2*NoOfSeqs-2 do
    NodeArray[i] := TMPTreeNode.Create(self, i);

  FNoOfBits := NoOfBits;
  if NoOfBits <= 8 then
    SetInfoSitesDNA(OriSeqs)
  else
    SetInfoSitesAA(OriSeqs);

  FSearchMethod  := SPR;
  FNoOfInitTrees := 1;
  FMaxNoOfTrees  := 1;
  FSearchLevel   := 3;

  InitTrees   := TTreeList.Create;
  ParsimTrees := TPartitionList.Create(NoOfSeqs, FMaxNoOfTrees, false);

  ProgressMessage := 'Searching MP tree';
  RandomOrder := false;
end;

constructor TMPTree.Create(MappedSites: TList; NoOfSites:integer; NoOfBits: integer);
var
  i: integer;
begin
  inherited Create;

  FNoOfTotalSites := NoOfSites;
  setlength(IsInfoSite, FNoOfTotalSites);
  FNoOfSeqs       := MappedSites.Count;
  setlength(NodeArray,  2*NoOfSeqs-1);
  for i := 0 to 2*NoOfSeqs-2 do
    NodeArray[i] := TMPTreeNode.Create(self, i);

  FNoOfBits := NoOfBits;
  if NoOfBits = 8 then
    SetInfoSitesDNAMap(MappedSites)
  else
    SetInfoSitesAAMap(MappedSites);

  FSearchMethod  := SPR;
  FNoOfInitTrees := 1;
  FMaxNoOfTrees  := 1;
  FSearchLevel   := 3;

  InitTrees   := TTreeList.Create;
  ParsimTrees := TPartitionList.Create(NoOfSeqs, FMaxNoOfTrees, false);

  ProgressMessage := 'Searching MP tree';
end;

destructor TMPTree.Destroy;
var
  i: integer;
begin
  for i := 0 to 2*FNoOfSeqs-2 do
    NodeArray[i].Free;
  if Assigned(InitTrees) then
    InitTrees.Free;
  if Assigned(ParsimTrees) then
    ParsimTrees.Free;
  setlength(IsInfoSite, 0);
  if FreqTable <> nil then
    FreeMem(FreqTable);
  inherited;
end;

procedure TMPTree.SetMaxNoOfTrees(n :integer);
begin
  if n = 0 then exit;

  FMaxNoOfTrees := n;
  ParsimTrees.MaxNoOfTrees := n;
end;

procedure TMPTree.SetNoOfInitTrees(n :integer);
begin
  if n = 0 then exit;

  FNoOfInitTrees := n;
end;

function TMPTree.GetNoOfTrees: integer;
begin
  result := ParsimTrees.NoOfTrees;
end;
{
function TMPTree.CurTreeLength: integer;
begin
  result := NodeArray[2*NoOfSeqs-2].TreeLen3 +NoOfSingletons;
end;
}
function TMPTree.GetTreeLength: integer;
begin
  if ParsimTrees.NoOfTrees > 0 then
    result := Round(ParsimTrees.Value[0])
  else
    result := NodeArray[2*NoOfSeqs-2].TreeLen3 +NoOfSingletons;
end;

function TMPTree.GetNoOfInfoSites: integer;
var
  i: integer;
begin
  if assigned(FreqTable) then
  begin
    result := 0;
    for i := 0 to FNoOfInfoSites-1 do
      inc(result, FreqTable[i]);
  end
  else
    result := FNoOfInfoSites;
end;

procedure TMPTree.SetFreqTable(BootTable: PArrayOfInt);
var
  i,n: integer;
begin
  if FreqTable = nil then
    GetMem(FreqTable, sizeof(Integer)*FNoOfInfoSites);

  n := 0;
  for i := 1 to NoOfTotalSites do
    if IsInfoSite[i-1] then { Glen - changed this to [i-1] as it is indexed from zero}
    begin
      FreqTable[n] := BootTable[i];
      inc(n);
    end;
end;


procedure TMPTree.SetInfoSitesAA(OriSeqs: TStringList);

  function MapAAsite(c: char): integer;
  begin
    case upcase(c) of
      'A': result := $00000001;
      'C': result := $00000002;
      'D': result := $00000004;
      'E': result := $00000008;
      'F': result := $00000010;
      'G': result := $00000020;
      'H': result := $00000040;
      'I': result := $00000080;
      'K': result := $00000100;
      'L': result := $00000200;
      'M': result := $00000400;
      'N': result := $00000800;
      'P': result := $00001000;
      'Q': result := $00002000;
      'R': result := $00004000;
      'S': result := $00008000;
      'T': result := $00010000;
      'V': result := $00020000;
      'W': result := $00040000;
      'Y': result := $00080000;
      else
        result := $000FFFFF;
    end;
  end;

var
  i,j,k,n,s: integer;
  CharState, NoOfStates, SeqIndex: array[0..32] of integer;
  flag: boolean;
begin
  for i := Low(SeqIndex) to High(SeqIndex) do
    SeqIndex[i] := 0;
  FNoOfInfoSites := 0;
  FNoOfSingletons := 0;
  for i := 0 to NoOfTotalSites-1 do
    IsInfoSite[i] := false;

  for i := 0 to NoOfTotalSites-1 do
  begin
    for j := 1 to 32 do
      CharState[j] := 0;
    NoOfStates[0] := 0;
    n := 0;
    j := 0;
    while (j < NoOfSeqs) and (MapAAsite(OriSeqs[j][i+1]) = pmResiX) do Inc(j);
    if j < NoOfSeqs then
    begin
      NoOfStates[0] := 1;
      NoOfStates[1] := 1;
      CharState[1] := MapAAsite(OriSeqs[j][i+1]);
      SeqIndex[1] := j;
      n := 1;
    end;
    Inc(j);
    if j < NoOfSeqs then
    begin
      for j := j to NoOfSeqs-1 do
      begin
        s := MapAAsite(OriSeqs[j][i+1]);
        if s = pmResiX then Continue;
        flag := false;
        for k := 1 to NoOfStates[0] do
          if s = CharState[k] then
          begin
            Inc(NoOfStates[k]);
            flag := true;
            Break;
          end;
        if not flag then
        begin
          Inc(NoOfStates[0]);
          NoOfStates[NoOfStates[0]] := 1;
          CharState[NoOfStates[0]] := s;
          SeqIndex[NoOfStates[0]] := j;
        end;
        Inc(n);
      end;
    end;
    if NoOfStates[0] > 1 then
    begin
      j := 0;
      s := 0;
      for k := 1 to NoOfStates[0] do
        if NoOfStates[k] = 1 then
        begin
          Inc(s);
          Inc(NodeArray[SeqIndex[k]].Singletons);
        end
        else
          Inc(j);
      if s = n then Dec(s);
      if j > 1 then
      begin
        IsInfoSite[i] := true;
        Inc(FNoOfInfoSites);
      end
      else
        Inc(FNoOfSingletons, s);
    end;
  end;

  for i := 0 to 2*NoOfSeqs-2 do
  begin
    GetMem(NodeArray[i].InfoSite1, SizeOf(LongInt)*FNoOfInfoSites);
    GetMem(NodeArray[i].InfoSite2, SizeOf(LongInt)*FNoOfInfoSites);
    GetMem(NodeArray[i].InfoSite3, SizeOf(LongInt)*FNoOfInfoSites);
  end;

  k := 0;
  for i := 0 to NoOfTotalSites-1 do
    if IsInfoSite[i] then
    begin
      for j := 0 to NoOfSeqs-1 do
        NodeArray[j].InfoSite3[k] := MapAAsite(OriSeqs[j][i+1]);
      Inc(k);
    end;
end;

procedure TMPTree.SetInfoSitesAAMap(MappedSites: TList);
var
  i,j,k,n,s: integer;
  CharState, NoOfStates, SeqIndex: array[0..32] of integer;
  flag: boolean;
begin
  for i := Low(SeqIndex) to High(SeqIndex) do
    SeqIndex[i] := 0;
  FNoOfInfoSites := 0;
  FNoOfSingletons := 0;
  for i := 0 to NoOfTotalSites-1 do
    IsInfoSite[i] := false;

  for i := 0 to NoOfTotalSites-1 do
  begin
    for j := 1 to 32 do
      CharState[j] := 0;
    NoOfStates[0] := 0;
    n := 0;
    j := 0;
    while (j < NoOfSeqs) and (PArrayOfInt(MappedSites[j])[i] = pmResiX) do Inc(j);
    if j < NoOfSeqs then
    begin
      NoOfStates[0] := 1;
      NoOfStates[1] := 1;
      CharState[1] := PArrayOfInt(MappedSites[j])[i];
      SeqIndex[1] := j;
      n := 1;
    end;
    Inc(j);
    if j < NoOfSeqs then
    begin
      for j := j to NoOfSeqs-1 do
      begin
        s := PArrayOfInt(MappedSites[j])[i];
        if s = pmResiX then Continue;
        flag := false;
        for k := 1 to NoOfStates[0] do
          if s = CharState[k] then
          begin
            Inc(NoOfStates[k]);
            flag := true;
            Break;
          end;
        if not flag then
        begin
          Inc(NoOfStates[0]);
          NoOfStates[NoOfStates[0]] := 1;
          CharState[NoOfStates[0]] := s;
          SeqIndex[NoOfStates[0]] := j;
        end;
        Inc(n);
      end;
    end;
    if NoOfStates[0] > 1 then
    begin
      j := 0;
      s := 0;
      for k := 1 to NoOfStates[0] do
        if NoOfStates[k] = 1 then
        begin
          Inc(s);
          Inc(NodeArray[SeqIndex[k]].Singletons);
        end
        else
          Inc(j);
      if s = n then Dec(s);
      if j > 1 then
      begin
        IsInfoSite[i] := true;
        Inc(FNoOfInfoSites);
      end
      else
        Inc(FNoOfSingletons, s);
    end;
  end;

  for i := 0 to 2*NoOfSeqs-2 do
  begin
    GetMem(NodeArray[i].InfoSite1, SizeOf(LongInt)*FNoOfInfoSites);
    GetMem(NodeArray[i].InfoSite2, SizeOf(LongInt)*FNoOfInfoSites);
    GetMem(NodeArray[i].InfoSite3, SizeOf(LongInt)*FNoOfInfoSites);
  end;

  k := 0;
  for i := 0 to NoOfTotalSites-1 do
    if IsInfoSite[i] then
    begin
      for j := 0 to NoOfSeqs-1 do
        NodeArray[j].InfoSite3[k] := PArrayOfInt(MappedSites[j])[i];
      Inc(k);
    end;
end;

procedure TMPTree.SetInfoSitesDNA(OriSeqs: TStringList);

  function MapDNAsite(c: char): integer;
  begin
    case upcase(c) of
      'A': result :=  1;
      'T',
      'U': result :=  2;
      'C': result :=  4;
      'G': result :=  8;
      'R': result :=  9;
      'Y': result :=  6;
      'M': result :=  5;
      'K': result := 10;
      'S': result := 12;
      'W': result :=  3;
      'B': result := 14;
      'D': result := 11;
      'H': result :=  7;
      'V': result := 13;
      else
        result := 15;
    end;
  end;

var
  i,j,k,n,s: integer;
  NoOfStates, CharState, SeqIndex: array[0..8] of integer;
  flag: boolean;
begin
  for i := Low(SeqIndex) to High(SeqIndex) do
    SeqIndex[i] := 0;
  FNoOfInfoSites := 0;
  FNoOfSingletons := 0;
  for i := 0 to NoOfTotalSites-1 do
    IsInfoSite[i] := false;

  for i := 0 to NoOfTotalSites-1 do
  begin
    for j := 1 to 8 do
      CharState[j] := 0;
    NoOfStates[0] := 0;
    n := 0;
    j := 0;
//    while (j < NoOfSeqs) and (PAnsiChar(OriSeqs[j])[i+1] = pmBaseN) do Inc(j);
    while (j < NoOfSeqs) and (not IsUnambiguousNucleotide(AnsiChar(OriSeqs[j][i+1]))) do Inc(j);
    if j < NoOfSeqs then
    begin
      NoOfStates[0] := 1;
      NoOfStates[1] := 1;
      CharState[1] := MapDNAsite(OriSeqs[j][i+1]);
      SeqIndex[1] := j;
      n := 1;
    end;
    Inc(j);
    if j < NoOfSeqs then
    begin
      for j := j to NoOfSeqs-1 do
      begin
//        if PAnsiChar(OriSeqs[j])[i+1] = pmBaseN then Continue;
        if not IsUnambiguousNucleotide(AnsiChar(OriSeqs[j][i+1])) then Continue;
        flag := false;
        s := MapDNAsite(OriSeqs[j][i+1]);
        for k := 1 to NoOfStates[0] do
          if s = CharState[k] then
          begin
            Inc(NoOfStates[k]);
            flag := true;
            Break;
          end;
        if not flag then
        begin
          Inc(NoOfStates[0]);
          NoOfStates[NoOfStates[0]] := 1;
          CharState[NoOfStates[0]] := s;
          SeqIndex[NoOfStates[0]] := j;
        end;
        Inc(n);
      end;
    end;
    if NoOfStates[0] > 1 then
    begin
      j := 0;
      s := 0;
      for k := 1 to NoOfStates[0] do
        if NoOfStates[k] = 1 then
        begin
          Inc(s);
          Inc(NodeArray[SeqIndex[k]].Singletons);
        end
        else
          Inc(j);
      if s = n then Dec(s);
      if j > 1 then
      begin
        IsInfoSite[i] := true;
        Inc(FNoOfInfoSites);
      end
      else
        Inc(FNoOfSingletons, s);
    end;
  end;

  for i := 0 to 2*NoOfSeqs-2 do
  begin
    GetMem(NodeArray[i].InfoSite1, SizeOf(LongInt)*FNoOfInfoSites);
    GetMem(NodeArray[i].InfoSite2, SizeOf(LongInt)*FNoOfInfoSites);
    GetMem(NodeArray[i].InfoSite3, SizeOf(LongInt)*FNoOfInfoSites);
  end;

  k := 0;
  for i := 0 to NoOfTotalSites-1 do
    if IsInfoSite[i] then
    begin
      for j := 0 to NoOfSeqs-1 do
        NodeArray[j].InfoSite3[k] := MapDNAsite(OriSeqs[j][i+1]);
      Inc(k);
    end;
end;

procedure TMPTree.SetInfoSitesDNAMap(MappedSites: TList);
var
  i,j,k,n,s: integer;
  NoOfStates, CharState, SeqIndex: array[0..8] of integer;
  flag: boolean;
begin
  for i := Low(SeqIndex) to High(SeqIndex) do
    SeqIndex[i] := 0;
  FNoOfInfoSites := 0;
  FNoOfSingletons := 0;
  for i := 0 to NoOfTotalSites-1 do
    IsInfoSite[i] := false;

  for i := 0 to NoOfTotalSites-1 do
  begin
    for j := 1 to 8 do
      CharState[j] := 0;
    NoOfStates[0] := 0;
    n := 0;
    j := 0;
//    while (j < NoOfSeqs) and (PAnsiChar(MappedSites[j])[i] = pmBaseN) do Inc(j);
    while (j < NoOfSeqs) and (not IsUnambiguousMappedNucleotide(PAnsiChar(MappedSites[j])[i])) do Inc(j);
    if j < NoOfSeqs then
    begin
      NoOfStates[0] := 1;
      NoOfStates[1] := 1;
      CharState[1] := Integer(PAnsiChar(MappedSites[j])[i]);
      SeqIndex[1] := j;
      n := 1;
    end;
    Inc(j);
    if j < NoOfSeqs then
    begin
      for j := j to NoOfSeqs-1 do
      begin
//        if PAnsiChar(MappedSites[j])[i] = pmBaseN then Continue;
        if not IsUnambiguousMappedNucleotide(PAnsiChar(MappedSites[j])[i]) then Continue;
        flag := false;
        s := Integer(PAnsiChar(MappedSites[j])[i]);
        for k := 1 to NoOfStates[0] do
          if s = CharState[k] then
          begin
            Inc(NoOfStates[k]);
            flag := true;
            Break;
          end;
        if not flag then
        begin
          Inc(NoOfStates[0]);
          NoOfStates[NoOfStates[0]] := 1;
          CharState[NoOfStates[0]] := s;
          SeqIndex[NoOfStates[0]] := j;
        end;
        Inc(n);
      end;
    end;
    if NoOfStates[0] > 1 then
    begin
      j := 0;
      s := 0;
      for k := 1 to NoOfStates[0] do
        if NoOfStates[k] = 1 then
        begin
          Inc(s);
          Inc(NodeArray[SeqIndex[k]].Singletons);
        end
        else
          Inc(j);
      if s = n then Dec(s);
      if j > 1 then
      begin
        IsInfoSite[i] := true;
        Inc(FNoOfInfoSites);
      end
      else
        Inc(FNoOfSingletons, s);
    end;
  end;

  for i := 0 to 2*NoOfSeqs-2 do
  begin
    GetMem(NodeArray[i].InfoSite1, SizeOf(LongInt)*FNoOfInfoSites);
    GetMem(NodeArray[i].InfoSite2, SizeOf(LongInt)*FNoOfInfoSites);
    GetMem(NodeArray[i].InfoSite3, SizeOf(LongInt)*FNoOfInfoSites);
  end;

  k := 0;
  for i := 0 to NoOfTotalSites-1 do
    if IsInfoSite[i] then
    begin
      for j := 0 to NoOfSeqs-1 do
        NodeArray[j].InfoSite3[k] := Integer(PAnsiChar(MappedSites[j])[i]);
      Inc(k);
    end;
end;

function ComputePLongMPSteps(Anc, Des1, Des2: PArrayOfLongInt; Len: Longint; FreqTable: PArrayOfInt): Longint;
var
  i: Longint;
  c: LongInt;
begin
  Result := 0;

  if Anc = nil then
    if FreqTable = nil then
      for i:=0 to Len-1 do
      begin
        c := Des1[i] and Des2[i];
        if c = 0 then
          Inc(Result);
      end
    else
      for i:=0 to Len-1 do
      begin
        if FreqTable[i] <> 0 then
        begin
          c := Des1[i] and Des2[i];
          if c = 0 then
            Inc(Result, FreqTable[i]);
        end;
      end
  else if FreqTable = nil then
    for i:=0 to Len-1 do
    begin
      c := Des1[i] and Des2[i];
      if c = 0 then
      begin
        Anc[i] := Des1[i] or Des2[i];
        Inc(Result);
      end
      else
        Anc[i] := c;
    end
  else
    for i:=0 to Len-1 do
    begin
      if FreqTable[i] <> 0 then
      begin
        c := Des1[i] and Des2[i];
        if c = 0 then
        begin
          Anc[i] := Des1[i] or Des2[i];
          Inc(Result, FreqTable[i]);
        end
        else
           Anc[i] := c;
      end;
    end;
end;

function TMPTree.CountTreeLength:integer;

  procedure CountStepUp(p : TMPTreeNode);
  var
    q, d1, d2: PArrayOfLongint;
  begin
    if not p.des1.OTU then
      CountStepUp(p.des1);
    if not p.des2.OTU then
      CountStepUp(p.des2);

    q := p.InfoSite3;
    d1 := p.des1.InfoSite3;
    d2 := p.des2.InfoSite3;
    p.TreeLen3 := p.des1.TreeLen3 +p.des2.TreeLen3
                 +ComputePLongMPSteps(q, d1, d2, FNoOfInfoSites, FreqTable);
  end;

  procedure CountStepDown(p : TMPTreeNode);
  var
    a, q, d: PArrayOfLongint;
  begin
    if p = p.anc.des1 then
    begin
      q := p.InfoSite1;
      a := p.anc.InfoSite2;
      d := p.des1.InfoSite3;
      p.TreeLen1 := p.anc.TreeLen2 +p.des1.TreeLen3
                   +ComputePLongMPSteps(q, a, d, FNoOfInfoSites, FreqTable);

      q := p.InfoSite2;
      a := p.anc.InfoSite2;
      d := p.des2.InfoSite3;
      p.TreeLen2 := p.anc.TreeLen2 +p.des2.TreeLen3
                   +ComputePLongMPSteps(q, a, d, FNoOfInfoSites, FreqTable);
    end
    else
    begin
      q := p.InfoSite1;
      a := p.anc.InfoSite1;
      d := p.des1.InfoSite3;
      p.TreeLen1 := p.anc.TreeLen1 +p.des1.TreeLen3
                   +ComputePLongMPSteps(q, a, d, FNoOfInfoSites, FreqTable);

      q := p.InfoSite2;
      a := p.anc.InfoSite1;
      d := p.des2.InfoSite3;
      p.TreeLen2 := p.anc.TreeLen1 +p.des2.TreeLen3
                   +ComputePLongMPSteps(q, a, d, FNoOfInfoSites, FreqTable);
    end;

    if not p.des1.OTU then
      CountStepDown(p.des1);
    if not p.des2.OTU then
      CountStepDown(p.des2);
  end;

var
  i: integer;
begin
  CountStepUp(NodeArray[2*NoOfSeqs-2]);

  for i := 0 to FNoOfInfoSites-1 do
    NodeArray[2*NoOfSeqs-2].InfoSite1[i] := NodeArray[2*NoOfSeqs-2].des1.InfoSite3[i];
  for i := 0 to FNoOfInfoSites-1 do
    NodeArray[2*NoOfSeqs-2].InfoSite2[i] := NodeArray[2*NoOfSeqs-2].des2.InfoSite3[i];

  NodeArray[2*NoOfSeqs-2].TreeLen1 := 0;
  NodeArray[2*NoOfSeqs-2].TreeLen2 := 0;

  if not NodeArray[2*NoOfSeqs-2].des1.OTU then
    CountStepDown(NodeArray[2*NoOfSeqs-2].des1);
  if not NodeArray[2*NoOfSeqs-2].des2.OTU then
    CountStepDown(NodeArray[2*NoOfSeqs-2].des2);

  Result := NodeArray[2*NoOfSeqs-2].TreeLen3;
end;

function TMPTree.CountTreeLengthOfSubtree(node: TMPTreeNode):integer;

  procedure CountStep(p : TMPTreeNode);
  var
    q, d1, d2: PArrayOfLongint;
  begin
    if p.flag or p.OTU then exit;

    if not p.des1.flag then
      CountStep(p.des1);
    if not p.des2.flag then
      CountStep(p.des2);

    q := p.InfoSite3;
    d1 := p.des1.InfoSite3;
    d2 := p.des2.InfoSite3;
    p.TreeLen3 := p.des1.TreeLen3 +p.des2.TreeLen3
                 +ComputePLongMPSteps(q, d1, d2, FNoOfInfoSites, FreqTable);
    p.flag := true;
  end;

begin
  CountStep(node);

  if node.anc = nil then
  begin
    result := node.TreeLen3;
    exit;
  end;

  if node.index = node.anc.des1.index then
    result := node.TreeLen3 +node.anc.TreeLen2
             +ComputePLongMPSteps(nil, node.InfoSite3, node.anc.InfoSite2, FNoOfInfoSites, FreqTable)
  else
    result := node.TreeLen3 +node.anc.TreeLen1
             +ComputePLongMPSteps(nil, node.InfoSite3, node.anc.InfoSite1, FNoOfInfoSites, FreqTable);
end;

function ChangeRoot(root, newposition: TMPTreeNode): TMPTreeNode;
var
  a, p, d : TMPTreeNode;
  i,n: integer;
  q: PArrayOfLongInt;
begin
  if (newposition = root) or (newposition.anc = root) then
  begin
    Result := newposition;
    Exit;
  end;
  d := newposition;
  p := d.anc;
  a := p.anc;
  if d = p.des1 then
    n := 1
  else
    n := 2;
  while p <> root do
  begin
    p.anc := d;
    if d = p.des1 then
    begin
      p.des1 := a;
      i := p.TreeLen2;
      q := p.InfoSite2;
      p.TreeLen2  := p.TreeLen3;
      p.InfoSite2 := p.InfoSite3;
      p.TreeLen3  := i;
      p.InfoSite3 := q;
    end
    else
    begin
      p.des2 := a;
      i := p.TreeLen1;
      q := p.InfoSite1;
      p.TreeLen1  := p.TreeLen3;
      p.InfoSite1 := p.InfoSite3;
      p.TreeLen3  := i;
      p.InfoSite3 := q;
    end;
    d := p;
    p := a;
    a := a.anc;
  end;

  if d = p.des1 then
  begin
    p.des2.anc := d;
    if p = d.des1 then
      d.des1 := p.des2
    else
      d.des2 := p.des2;
  end
  else
  begin
    p.des1.anc := d;
    if p = d.des1 then
      d.des1 := p.des1
    else
      d.des2 := p.des1;
  end;
  p := newposition.anc;
  p.anc := root;
  newposition.anc := root;
  if n = 1 then
  begin
    root.des1 := newposition;
    root.des2 := p;
  end
  else
  begin
    root.des2 := newposition;
    root.des1 := p;
  end;
  Result := d;
end;

procedure AddBranch(r, n, b : TMPTreeNode);
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

function RemoveBranch(n : TMPTreeNode): TMPTreeNode;
var
  a, aa, b : TMPTreeNode;
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
  result := aa;
end;

procedure SetFlag(p, a: TMPTreeNode);
begin
  while p <> nil do
  begin
    p.flag := false;
    if p = a then
      break;
    p := p.anc;
  end;
end;

procedure TMPTree.MakeInitialTree;
var
  h : array of TMPTreeNode;
  tmptreelist: TTreeList;

  procedure InitTree;
  var
    i : integer;
  begin
    NodeArray[2*NoOfSeqs-2].des1 := NodeArray[0];
    NodeArray[2*NoOfSeqs-2].des2 := NodeArray[NoOfSeqs];
    NodeArray[0].anc := NodeArray[2*NoOfSeqs-2];
    NodeArray[NoOfSeqs].anc := NodeArray[2*NoOfSeqs-2];
    NodeArray[NoOfSeqs].des1 := NodeArray[1];
    NodeArray[NoOfSeqs].des2 := NodeArray[2];
    NodeArray[1].anc := NodeArray[NoOfSeqs];
    NodeArray[2].anc := NodeArray[NoOfSeqs];

    for i := 0 to 2*NoOfSeqs-2 do
      NodeArray[i].TreeLen3 := 0;
  end;

  procedure ResetFlag;
  var
    i: integer;
  begin
    for i := 0 to 2*NoOfSeqs-2 do
      NodeArray[i].flag := false;
  end;

  function MakeEachTree(t, n: integer): integer;
  var
    i,j,k,b,MTL,TL0: integer;
    TL: Integer = 0;
    p : TMPTreeNode;
  begin
    if RandomOrder then
    begin
      for i := NoOfSeqs-1 downto 1 do
      begin
        j := Random(i+1);
        p := NodeArray[i];
        NodeArray[i] := NodeArray[j];
        NodeArray[j] := p;
      end;
    end
    else
    begin
      i := 0;
      j := t;
      k := 0;
      repeat
        NodeArray[k] := h[i+j];
        inc(i,n);
        if (i+j) >= NoOfSeqs then
        begin
          i := 0;
          inc(j);
          if j >= n then
            j := 0;
        end;
        inc(k);
      until k = NoOfSeqs;
    end;

    InitTree;

    for i := 3 to NoOfSeqs-1 do
    begin
      TL0 := CountTreeLength;
      p := NodeArray[NoOfSeqs+i-2];
      MTL := NoOfSeqs*FNoOfInfoSites;
      for j := 1 to i-1 do
      begin
        AddBranch(p, NodeArray[i], NodeArray[j]);
        p.TreeLen3 := p.des1.TreeLen3 +p.des2.TreeLen3
                     +ComputePLongMPSteps(p.InfoSite3, p.des1.InfoSite3, p.des2.InfoSite3, FNoOfInfoSites, FreqTable);
        if p = p.anc.des1 then
          TL := p.TreeLen3 +p.anc.TreeLen2
               +ComputePLongMPSteps(nil, p.InfoSite3, p.anc.InfoSite2, FNoOfInfoSites, FreqTable)
        else if p = p.anc.des2 then
          TL := p.TreeLen3 +p.anc.TreeLen1
               +ComputePLongMPSteps(nil, p.InfoSite3, p.anc.InfoSite1, FNoOfInfoSites, FreqTable);
        if TL < MTL then
        begin
          MTL := TL;
          b := j;
        end;
        RemoveBranch(NodeArray[i]);
        if TL = TL0 then
          break;
      end;
      if TL > TL0 then
        for j := NoOfSeqs to NoOfSeqs+i-3 do
        begin
          AddBranch(p, NodeArray[i], NodeArray[j]);
          p.TreeLen3 := p.des1.TreeLen3 +p.des2.TreeLen3
                       +ComputePLongMPSteps(p.InfoSite3, p.des1.InfoSite3, p.des2.InfoSite3, FNoOfInfoSites, FreqTable);
          if p = p.anc.des1 then
            TL := p.TreeLen3 +p.anc.TreeLen2
                 +ComputePLongMPSteps(nil, p.InfoSite3, p.anc.InfoSite2, FNoOfInfoSites, FreqTable)
          else if p = p.anc.des2 then
            TL := p.TreeLen3 +p.anc.TreeLen1
                 +ComputePLongMPSteps(nil, p.InfoSite3, p.anc.InfoSite1, FNoOfInfoSites, FreqTable);
          if TL < MTL then
          begin
            MTL := TL;
            b := j;
          end;
          RemoveBranch(NodeArray[i]);
          if TL = TL0 then
            break;
        end;
      AddBranch(p, NodeArray[i], NodeArray[b]);

//      if assigned(CheckCancel) then
//        CheckCancel(round((t+(n*(n-1) div 2) +i*i/NoOfSeqs/NoOfSeqs)/NoOfInitTrees*50), ProgressMessage);
    end;
    result := CountTreeLength;

    for i := 0 to NoOfSeqs-1 do
      NodeArray[i] := h[i];
  end;

  procedure SaveInitTree(tree: TTreeData);
  var
    i: integer;
  begin
    for i := 0 to NoOfSeqs-2 do
    begin
      tree.NodeArray[i].des1 := NodeArray[i+NoOfSeqs].des1.index;
      tree.NodeArray[i].des2 := NodeArray[i+NoOfSeqs].des2.index;
    end;
    tree.Freq := 1.0;
    tree.Value := NodeArray[2*NoOfSeqs-2].TreeLen3 +NoOfSingletons;

    tmptreelist.Add(tree);

    if (ParsimTrees.NoOfTrees > 0) and (Round(tree.Value) < TreeLength) then
      ParsimTrees.Reset;
    if (ParsimTrees.NoOfTrees < ParsimTrees.MaxNoOfTrees) and
       not (Round(tree.Value) > TreeLength) then
      ParsimTrees.AddTreeData(tree, true);
  end;

var
  tree: TTreeData;
  i,j,k,n: integer;
  pl: TPartitionList;
begin
  if NoOfInitTrees > (NoOfSeqs*(NoOfSeqs-1) div 2) then
    NoOfInitTrees := NoOfSeqs*(NoOfSeqs-1) div 2;

  tmptreelist := TTreeList.Create;

  setlength(h, NoOfSeqs);
  for i := 0 to NoOfSeqs-1 do
    h[i] := NodeArray[i];

  n := NoOfInitTrees;
  j := 0;
  k := 1;
  repeat
    for i := 0 to k-1 do
    begin
      tree := TTreeData.Create(NoOfSeqs,false,false,false);
      tree.Value := MakeEachTree(i, k);
      SaveInitTree(tree);

      inc(j);

      if assigned(CheckCancel) then
        CheckCancel(round(j/NoOfInitTrees*10), ProgressMessage);

      if j = n then
        break;
    end;
    inc(k);
  until j = n;

  pl := TPartitionList.Create(NoOfSeqs, NoOfInitTrees*10, false);
  pl.AddTreeData(tmptreelist[0], false);

  InitTrees.Add(tmptreelist.Remove(0));
  if NoOfInitTrees > 1 then
    for i := 0 to tmptreelist.NoOfTrees-1 do
    begin
      if pl.AddTreeData(tmptreelist[0], true) then
        InitTrees.Add(tmptreelist.Remove(0));

      if InitTrees.NoOfTrees = NoOfInitTrees then
        break;
    end;


  pl.Free;
  tmptreelist.Free;
  setlength(h, 0);
end;
{
procedure TMPTree.SaveTree(TL: TTreeList);
var
  tree: TTreeData;
  i: integer;
begin
  tree := TTreeData.Create(NoOfSeqs,false,false,false);
  for i := 0 to NoOfSeqs-2 do
  begin
    tree.NodeArray[i].des1 := NodeArray[i+NoOfSeqs].des1.index;
    tree.NodeArray[i].des2 := NodeArray[i+NoOfSeqs].des2.index;
  end;
  tree.Freq := 1.0;
  tree.Value := NodeArray[2*NoOfSeqs-2].TreeLen3;
  TL.Add(tree);
end;
}

function TMPTree.SearchMPTree: boolean;
var
  SwapData: TSwapData;
  q, r: array of TMPTreeNode;
  Canceled: boolean;
  progress, aprogress, rep: integer;

  procedure SaveParsimTree;
  var
    tree: TTreeData;
    i, CurTreeLength: integer;
  begin
    CurTreeLength := NodeArray[2*NoOfSeqs-2].TreeLen3 +NoOfSingletons;
    if CurTreeLength > TreeLength then
      exit;
    if (CurTreeLength = TreeLength) and
       (ParsimTrees.NoOfTrees = ParsimTrees.MaxNoOfTrees) then
      exit;
    {$IFDEF VISUAL_BUILD}
    tree := TTreeData.Create(NoOfSeqs, false, false, false);
    {$ELSE}
    tree := TTreeData.Create(NoOfSeqs, true, false, false);
    {$ENDIF}
    for i := 0 to NoOfSeqs-2 do
    begin
      tree.NodeArray[i].des1 := NodeArray[i+NoOfSeqs].des1.index;
      tree.NodeArray[i].des2 := NodeArray[i+NoOfSeqs].des2.index;
    end;
    tree.Value := CurTreeLength;
    tree.Freq  := 1;
    if (CurTreeLength < TreeLength) then
      ParsimTrees.Reset;
    ParsimTrees.AddTreeData(tree, true);
    tree.Free;
  end;

  procedure LoadInitTree(index: integer);
  var
    tree: TTreeData;
    i: integer;
  begin
    tree := InitTrees[index];

    for i := 0 to NoOfSeqs-2 do
    begin
      NodeArray[i+NoOfSeqs].des1 := NodeArray[tree.NodeArray[i].des1];
      NodeArray[i+NoOfSeqs].des2 := NodeArray[tree.NodeArray[i].des2];
      NodeArray[i+NoOfSeqs].des1.anc := NodeArray[i+NoOfSeqs];
      NodeArray[i+NoOfSeqs].des2.anc := NodeArray[i+NoOfSeqs];
    end;

    ChangeRoot(NodeArray[2*NoOfSeqs-2], NodeArray[0]);
    CountTreeLength;

    SaveParsimTree;

  end;

  procedure SaveSwapData(n, r, TL: integer);
  begin
    swapdata.node[0] := q[1].index;
    swapdata.node[1] := q[2].index;
    swapdata.node[2] := q[n].index;
    swapdata.root    := r;
    swapdata.TreeLen := TL;
  end;

  function SearchEachNode(node: TMPTreeNode):boolean;
  var
    p: array of TMPTreeNode;
    TL0, d: integer;

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
      q[2].Assign(p[2]);
      q[3].Assign(p[3]);

      if p[1] = p[0].des1 then
        q[0].des1 := q[1]
      else
        q[0].des2 := q[1];
      q[1].anc := q[0];
      if p[3] = p[1].des1 then
      begin
        q[1].des1 := q[3];
        q[1].des2 := q[2];
      end
      else
      begin
        q[1].des1 := q[2];
        q[1].des2 := q[3];
      end;
      q[2].anc := q[1];
      q[3].anc := q[1];

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
      a: TMPTreeNode;
    begin
      a := q[index].anc;
      q[index].Assign(p[index]);
      q[index].anc := a;
      q[index].flag := true;

      dec(d);
    end;

    function CheckSubtreeLength(node: integer): integer;
    var
      MinTL: integer;

      procedure SetTreeLen3(n: TMPTreeNode);
      begin
        while (not n.OTU) and (n <> q[2]) do
        begin
          n.TreeLen3 := n.des1.TreeLen3 +n.des2.TreeLen3
                       +ComputePLongMPSteps(n.InfoSite3, n.des1.InfoSite3, n.des2.InfoSite3, FNoOfInfoSites, FreqTable);
          n := n.anc;
        end;
        q[2].TreeLen3 := q[2].des1.TreeLen3 +q[2].des2.TreeLen3
                        +ComputePLongMPSteps(q[2].InfoSite3, q[2].des1.InfoSite3, q[2].des2.InfoSite3, FNoOfInfoSites, FreqTable);
      end;

      procedure SetFlag;
      var
        n: TMPTreeNode;
      begin
        n := q[2];
        while n <> q[0] do
        begin
          n.flag := false;
          n := n.anc;
        end;
        q[0].flag := false;
        q[2].flag := true;
      end;

      function TBRRecursive(n: TMPTreeNode; dd: integer): integer;
      var
        r0: TMPTreeNode;
        TL: integer;
      begin
        Result := MaxInt;
        if dd > 0 then
        begin
          r0 := ChangeRoot(q[2], n);
          SetTreeLen3(r0);
          SetFlag;
          TL := CountTreeLengthOfSubtree(q[0]);
          if TL < MinTL then
          begin
            SaveSwapData(node, n.index, TL);
            MinTL := TL;
          end;
          Result := TL;
        end;

        if (not n.OTU) and (dd < SearchLevel) then
        begin
          r[2*(dd+1)  ].Assign(n.des1);
          r[2*(dd+1)  ].anc := n;
          n.des1 := r[2*(dd+1)  ];
          r[2*(dd+1)+1].Assign(n.des2);
          r[2*(dd+1)+1].anc := n;
          n.des2 := r[2*(dd+1)+1];

          TL := TBRRecursive(n.des1, dd+1);
          if TL < Result then
            Result := TL;
          TL := TBRRecursive(n.des2, dd+1);
          if TL < Result then
            Result := TL;
        end;
      end;

    var
      TL: integer;
      d1,d2: TMPTreeNode;
    begin
      MinTL  := TL0;
      TL := CountTreeLengthOfSubtree(q[0]);
      if TL < TL0 then
      begin
        SaveSwapData(node, q[2].index, TL);
        MinTL := TL;
      end;
      Result := TL;

      if SearchMethod = SPR then exit;
      if q[2].OTU then exit;
      if q[2].des1.OTU and q[2].des2.OTU then exit;

      d1 := q[2].des1;
      d2 := q[2].des2;

      r[0].Assign(q[2].des1);
      r[0].anc  := q[2];
      q[2].des1 := r[0];
      r[1].Assign(q[2].des2);
      r[1].anc  := q[2];
      q[2].des2 := r[1];

      TL := TBRRecursive(r[0], 0);
      if TL < Result then
        Result := TL;

      SetTreeLen3(ChangeRoot(q[2], r[1]));

      TL := TBRRecursive(r[1], 0);
      if TL < Result then
        Result := TL;

      q[2].des1 := d1;
      q[2].des2 := d2;
      SetTreeLen3(q[2]);
    end;

    function SPR: boolean;
    var
      TL, i: integer;
    begin
      Result := true;

      SetFlag(RemoveBranch(q[2]), q[0]);
      AddBranch(q[1], q[2], q[2*d]);
      SetFlag(q[1], q[0]);

      TL := CheckSubtreeLength(2*d);
      if TL < TL0 then
      begin
        TL0 := TL;
        result := false;
      end;

      SetFlag(RemoveBranch(q[2]), q[0]);
      AddBranch(q[1], q[2], q[2*d+1]);
      SetFlag(q[1], q[0]);

      TL := CheckSubtreeLength(2*d+1);
      if TL < TL0 then
      begin
        TL0 := TL;
        result := false;
      end;

      SetFlag(RemoveBranch(q[2]), q[0]);
      AddBranch(q[1], q[2], q[3]);
      SetFlag(q[1], q[0]);

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
        Canceled := CheckCancel(round((progress/(2*NoOfSeqs-3)+rep)/NoOfInitTrees*90)+10, ProgressMessage);
      if Canceled then
        CheckCancel(round((progress/(2*NoOfSeqs-3)+rep)/NoOfInitTrees*90)+10, ProgressMessage);

    end;

    function SPRRecursiveDown: boolean;
    var
      flag: boolean;
    begin
      result := true;
      if d > SearchLevel then exit;

      if IncSubtreeDown(2*d) then
      begin
        flag   := SPR;
        result := result and flag;
        flag   := SPRRecursiveDown;
        result := result and flag;
        DecSubtreeDown(2*(d-1));
      end;

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

      if IncSubtreeDown(2*d+1) then
      begin
        flag   := SPR;
        result := result and flag;
        flag   := SPRRecursiveDown;
        result := result and flag;
        DecSubtreeDown(2*(d-1)+1);
      end;

      if IncSubtreeUp(2*d) then
      begin
        flag   := SPRRecursiveUp;
        result := result and flag;
      end;
    end;

    procedure SwapBranch;
    var
      p: array[0..2] of TMPTreeNode;
    begin
      p[0] := NodeArray[SwapData.Node[0]];
      p[1] := NodeArray[SwapData.Node[1]];
      p[2] := NodeArray[SwapData.Node[2]];

      RemoveBranch(p[1]);
      AddBranch(p[0], p[1], p[2]);

      if SearchMethod = TBR then
        ChangeRoot(p[1], NodeArray[SwapData.root]);

      CountTreeLength;
      SaveParsimTree;
    end;

  var
    flag: boolean;
  begin
    result := true;
    TL0 := NodeArray[2*NoOfSeqs-2].TreeLen3;

    if Canceled then exit;
    if node.anc = nil then exit;
    if node.anc.anc = nil then exit;

    setlength(p, SearchLevel*2+4);

    SetInitialSubtree;

    if IncSubtreeDown(3) then
    begin
      flag   := SPR;
      result := result and flag;

      flag   := SPRRecursiveDown;
      result := result and flag;

      DecSubtreeDown(3);
    end;

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
    end
    else
      SwapBranch;
  end;


  function SearchOnAllNodes: boolean;
  var
    i: integer;
    flag: boolean;
  begin
    result := true;

    for i := 0 to 2*NoOfSeqs-3 do
    begin
      if Canceled then
        exit;

      if NodeArray[i].anc = NodeArray[2*NoOfSeqs-2] then
        continue;

      flag := SearchEachNode(NodeArray[i]);

      if Canceled then
        break;

      result := result and flag;
    end;
  end;

var
  i, j: integer;
  flag: boolean;
begin
  result    := false;
  aprogress := 0;
  progress  := 0;
  Canceled  := false;

  ParsimTrees.Reset;
  ParsimTrees.MaxNoOfTrees := MaxNoOfTrees;

  try
    MakeInitialTree;

    setlength(q, SearchLevel*2+4);
    for i := 0 to length(q)-1 do
      q[i] := TMPTreeNode.Create(self, i);

    setlength(r, SearchLevel*2+2);
    for i := 0 to length(r)-1 do
      r[i] := TMPTreeNode.Create(self, i);

    for i := 0 to InitTrees.NoOfTrees-1 do
    begin
      rep := i;

      aprogress := 0;
      progress  := 0;
      for j := 0 to 2*NoOfSeqs-2 do
        NodeArray[j].done := false;

      LoadInitTree(i);

      repeat
        flag := SearchOnAllNodes;
        if Canceled then
          break;
      until flag;

      if Canceled then
        break;
    end;
    ParsimTrees.BuildPartitionsSummary(1);
    result := (not Canceled);
  finally
    for i := 0 to length(q)-1  do
      if q[i] <> nil then
        q[i].Free;
    for i := 0 to length(r)-1  do
      if r[i] <> nil then
        r[i].Free;

    setlength(q, 0);
  end;
end;

procedure TMPTree.ComputeApproxBLens(tree: TTreeData);
var
  root: TMPTreeNode = nil;
  i,j,k,s,s0,s1: integer;
begin
  if tree.NoOfOTUs <> NoOfSeqs then exit;
  tree.isBLen := true;

  for i := 0 to NoOfSeqs-2 do
  begin
    NodeArray[i+NoOfSeqs].des1 := NodeArray[tree.NodeArray[i].des1];
    NodeArray[i+NoOfSeqs].des2 := NodeArray[tree.NodeArray[i].des2];
    NodeArray[i+NoOfSeqs].des1.anc := NodeArray[i+NoOfSeqs];
    NodeArray[i+NoOfSeqs].des2.anc := NodeArray[i+NoOfSeqs];
  end;

  tree.Value := CountTreeLength;

  for i := 2*NoOfSeqs-2 downto 0 do
    if NodeArray[i].anc = nil then
    begin
      root := NodeArray[i];
      break;
    end;

  for i := 0 to 2*NoOfSeqs-2 do
  begin
    tree.BLen[i] := 0;
    if NodeArray[i] = root then continue;
    if NodeArray[i] = root.des1 then continue;
    if NodeArray[i] = root.des2 then continue;

    for k := 0 to FNoOfInfoSites-1 do
    begin
      s0 := NodeArray[i].anc.InfoSite1[k] and NodeArray[i].anc.InfoSite2[k];
      if s0 = 0 then
        s0 := NodeArray[i].anc.InfoSite1[k] or NodeArray[i].anc.InfoSite2[k];
      s := s0 and NodeArray[i].anc.InfoSite3[k];
      if s = 0 then
        s0 := s0 or NodeArray[i].anc.InfoSite3[k]
      else
        s0 := s;

      if i < NoOfSeqs then
        s1 := NodeArray[i].InfoSite3[k]
      else
      begin
        s1 := NodeArray[i].InfoSite1[k] and NodeArray[i].InfoSite2[k];
        if s1 = 0 then
          s1 := NodeArray[i].InfoSite1[k] or NodeArray[i].InfoSite2[k];
        s := s1 and NodeArray[i].InfoSite3[k];
        if s = 0 then
          s1 := s1 or NodeArray[i].InfoSite3[k]
        else
          s1 := s;
      end;

      if s0 and s1 = 0 then
        tree.BLen[i] := tree.BLen[i] +1;
    end;
  end;
  i := root.des1.index;
  j := root.des2.index;
  for k := 0 to FNoOfInfoSites-1 do
  begin
    if i < NoOfSeqs then
      s0 := NodeArray[i].InfoSite3[k]
    else
    begin
      s0 := NodeArray[i].InfoSite1[k] and NodeArray[i].InfoSite2[k];
      if s0 = 0 then
        s0 := NodeArray[i].InfoSite1[k] or NodeArray[i].InfoSite2[k];
      s := s0 and NodeArray[i].InfoSite3[k];
      if s = 0 then
        s0 := s0 or NodeArray[i].InfoSite3[k]
      else
        s0 := s;
    end;

    if j < NoOfSeqs then
      s1 := NodeArray[j].InfoSite3[k]
    else
    begin
      s1 := NodeArray[j].InfoSite1[k] and NodeArray[j].InfoSite2[k];
      if s1 = 0 then
        s1 := NodeArray[j].InfoSite1[k] or NodeArray[j].InfoSite2[k];
      s := s1 and NodeArray[j].InfoSite3[k];
      if s = 0 then
        s1 := s1 or NodeArray[j].InfoSite3[k]
      else
        s1 := s;
    end;

    if s0 and s1 = 0 then
      tree.BLen[i] := tree.BLen[i] +1;
  end;

  for i := 0 to NoOfSeqs-1 do
    tree.BLen[i] := tree.BLen[i] +NodeArray[i].Singletons;

  for i := 0 to 2*NoOfSeqs-2 do
    tree.BLen[i] := tree.BLen[i]/NoOfTotalSites;
end;

procedure TMPTree.GetTreeData(tree: TTreeData; index: integer);
var
  tmptree: TTreeData;
begin
  if index >= ParsimTrees.NoOfTrees then exit;

  tmptree := ParsimTrees.ExtractTreeData(index);
  if tree.isBLen then
    ComputeApproxBLens(tmptree);

  tree.Assign(tmptree);

  tmptree.Free;
end;

procedure TMPTree.GetTreeList(treelist: TTreeList);
begin
  ParsimTrees.ExtractTreeList(treelist);
end;

///////////////////////////////

constructor TMPTreeSearchThread.Create(TreeList: TTreeList; MappedSites: TList; NoOfSites:integer; NoOfBits: integer);
begin
  inherited Create;
  FExceptionName := 'none';
  FExceptionMessage := 'none';
  FTreeList := TreeList;
  MPTree := TMPTree.Create(MappedSites, NoOfSites, NoOfBits);
  MPTree.CheckCancel := CheckCancel;
end;

destructor TMPTreeSearchThread.Destroy;
begin
  if Assigned(MPTree) then  
    MPTree.Free;
  inherited Destroy;
end;

procedure TMPTreeSearchThread.SetMaxNoOfTrees(n :integer);
begin
  MPTree.MaxNoOfTrees := n;
end;

function TMPTreeSearchThread.GetNoOfSeqs: integer;
begin
  result := MPTree.NoOfSeqs;
end;

function TMPTreeSearchThread.GetNoOfInfoSites: integer;
begin
  result := MPTree.NoOfInfoSites;
end;

function TMPTreeSearchThread.GetNoOfSingletons: integer;
begin
  result := MPTree.NoOfSingletons;
end;

function TMPTreeSearchThread.GetNoOfTotalSites: integer;
begin
  result := MPTree.NoOfTotalSites;
end;

function TMPTreeSearchThread.GetNoOfTrees: integer;
begin
  result := MPTree.ParsimTrees.NoOfTrees;
end;

function TMPTreeSearchThread.GetMaxNoOfTrees: integer;
begin
  result := MPTree.MaxNoOfTrees;
end;

procedure TMPTreeSearchThread.SetNoOfInitTrees(n :integer);
begin
  MPTree.NoOfInitTrees := n;
end;

function TMPTreeSearchThread.GetNoOfInitTrees: integer;
begin
  result := MPTree.NoOfInitTrees;
end;

function TMPTreeSearchThread.GetTreeLength: integer;
begin
  result := MPTree.TreeLength;
end;

function TMPTreeSearchThread.GetSearchLevel: integer;
begin
  result := MPTree.SearchLevel;
end;

procedure TMPTreeSearchThread.SetSearchLevel(n :integer);
begin
  MPTree.SearchLevel := n;
end;

function TMPTreeSearchThread.GetSearchMethod: TSearchMethod;
begin
  result := MPTree.SearchMethod;
end;

procedure TMPTreeSearchThread.SetSearchMethod(m: TSearchMethod);
begin
  MPTree.SearchMethod := m;
end;

function TMPTreeSearchThread.GetProgressMessage: AnsiString;
begin
  result := MPTree.ProgressMessage;
end;

procedure TMPTreeSearchThread.SetProgressMessage(str: AnsiString);
begin
  MPTree.ProgressMessage := str;
end;

procedure TMPTreeSearchThread.Execute;
begin
  try
    inherited;
  Except
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
      raise Exception.Create(E.Message);
      //{$IFNDEF WIN64}HandleException;{$ENDIF} // let MadExcept take from here because we want to get a bug report
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

procedure TMPTreeSearchThread.Search;
begin
  if MPTree.SearchMPTree then
    MPTree.GetTreeList(FTreeList);
end;

procedure TMPTreeSearchThread.GetTreeData(tree: TTreeData; index: integer);
begin
  MPTree.GetTreeData(tree, index);
end;

procedure TMPTreeSearchThread.GetTreeList(treelist: TTreeList);
begin
  treelist.Assign(FTreeList);
end;

procedure TMPTreeSearchThread.DoOnProgress;
begin
  FProgressDlg.UpdateRunStatusInfo('Status', FStatus);
  FProgressDlg.UpdateRunStatusInfo('Parsimony Trees', IntToStr(NoOfTrees));
  FProgressDlg.UpdateRunStatusInfo('Tree Length', IntToStr(TreeLength));

  inherited;
end;

function TMPTreeSearchThread.CheckCancel(Progress: integer; Status: AnsiString): boolean;
begin
  if assigned(CheckCancelFunc) then
    if CheckCancelFunc(Progress, Status) then
      Terminate;

  FStatus := Status;
  OnProgress(Progress);
  result := Terminated;
end;

/////////////////

procedure TBootstrapMPTreeSearchThread.Search;
begin
  inherited Search;
end;

procedure TBootstrapMPTreeSearchThread.AddBootTrees(trees: TPartitionList);
begin
  try
    LockThread.Acquire;
    BootstrapTrees.Join(trees);
  finally
    LockThread.Release;
  end;
end;

function TBootstrapMPTreeSearchThread.CheckCancel(Progress: integer; Status: AnsiString): boolean;
begin
  if FCurrentRep = 0 then
    Result := inherited CheckCancel(Progress, Status)
  else
    Result := inherited CheckCancel(round(FCurrentRep/NoOfBootstraps*100), 'Bootstrapping MP Trees');
end;

function TBootstrapMPTreeSearchThread.CheckChildThreadDone: Boolean;
var
  i: integer;
begin
  Result := True;
  if FNoOfThreads > 1 then
    for i := 1 to FNoOfThreads-1 do
      Result := Result and FChildThread[i].Done;
end;

constructor TBootstrapMPTreeSearchThread.Create(TreeList: TTreeList;
  MappedSites: TList; NoOfSites: integer; NoOfBits: integer;
  numThreads: Integer; MaxPercentRam: Integer = DEFAULT_MAX_PERCENT_RAM);
var
  mem: Int64 = 0;
  status: Boolean = False;
begin
  inherited Create(TreeList, MappedSites, NoOfSites, NoOfBits);
  FreeOnTerminate := True;
  FMappedSites := MappedSites;
  LockThread := TCriticalSection.Create;
  status := GetAvailablePhysicalMemory(mem);
  Assert(status, 'failed to detect available physical memory');
  mem := trunc(mem*MaxPercentRam/100);
  FThreadMemSize := EstimateThreadMemSize;
  FNoOfThreads := numThreads;
  if (mem > 0) and (FNoOfThreads*FThreadMemSize > mem) then
    FNoOfThreads :=  trunc(mem/FThreadMemSize);
  setlength(FChildThread, FNoOfThreads);
  {$IFDEF VISUAL_BUILD}
  if FNoOfThreads > 1 then
    MegaForm.MultithreadedCalcIsRunning := True;
  {$ENDIF}
end;

destructor TBootstrapMPTreeSearchThread.Destroy;
begin
  if Assigned(LockThread) then
    LockThread.Free;
  inherited;
end;

procedure TBootstrapMPTreeSearchThread.DoInitialSearch;
begin
  if MPTree.SearchMPTree then
    MPTree.GetTreeList(FTreeList);
  FProgress := 100;
  Synchronize(DoOnProgress);
end;

procedure TBootstrapMPTreeSearchThread.DoOnProgress;
begin
  if FCurrentRep = 0 then
    inherited DoOnProgress
  else
  begin
    FProgressDlg.UpdateRunStatusInfo('Status', FStatus);
    FProgressDlg.UpdateRunStatusInfo('Bootstrap Rep #', Format('%d of %d', [Min(FCurrentRep, FNoOfBootstraps), FNoOfBootstraps]));
    FProgressDlg.UpdatePercentProgress(FProgress);
    FProgressDlg.Refresh;
  end;
end;

function TBootstrapMPTreeSearchThread.EstimateThreadMemSize: Extended;
begin
  { TODO 1 -oGlen -cparallelized MP : implement TBootstrapMPTreeSearchThread.EstimateThreadMemSize }
  Result := 100000;
end;

procedure TBootstrapMPTreeSearchThread.Execute;
var
  i: Integer;
  AllThreadsDone: Boolean;
  ni,nm: integer;
  ro: boolean;
begin
  DoInitialSearch;
  FThreadIndex := 0;
  FChildThread[0] := self;
  FMainThread := Self;
  if FNoOfThreads > 1 then
    for i := 1 to FNoOfThreads-1 do
    begin
      FChildThread[i] := TMPTreeSearchChildThread.Create(FTreeList, FMappedSites, MPTree.NoOfTotalSites, MPTree.NoOfBits);
      FChildThread[i].MainThread := Self;
      FChildThread[i].ThreadIndex := i;
    end;

  GetMem(MPTree.FreqTable, Sizeof(Integer)*MPTree.FNoOfInfoSites);
  BootstrapTrees.Reset;

  Randomize;
  nm := MPTree.MaxNoOfTrees;
  ni := MPTree.NoOfInitTrees;
  ro := MPTree.RandomOrder;
  MPTree.MaxNoOfTrees  := 1;
  MPTree.NoOfInitTrees := 1;
  MPTree.RandomOrder   := true;

  try
    StartTime := Now;
    FNoOfCurReps := 0;
    if FNoOfThreads > 1 then
      for i := 1 to FNoOfThreads-1 do
        FChildThread[i].Start;

    Search;

    while not (Terminated or CheckChildThreadDone) do
      sleep(0);
  finally     
    if FNoOfThreads > 1 then
      for i := 1 to FNoOfThreads-1 do
        if FChildThread[i] <> nil then
          FChildThread[i].Terminate;
    repeat
      AllThreadsDone := True;
      if FNoOfThreads > 1 then
        for i := 1 to FNoOfThreads-1 do
          AllThreadsDone := AllThreadsDone and (FChildThread[i].Done);
      Sleep(0);
    until AllThreadsDone;
    {$IFDEF VISUAL_BUILD}
    if FNoOfThreads > 1 then
      MegaForm.MultithreadedCalcIsRunning := False;
    {$ENDIF}
    if FNoOfThreads > 1 then
      for i := 1 to FNoOfThreads - 1 do
        FChildThread[i].Free;
    FRemainingTimeStr := '00:00:00';
    FStatus := 'Finalizing...';
    FProgress := 99;
    Synchronize(DoOnProgress);
    BootstrapTrees.BuildPartitionsSummary(Min(FCurrentRep, FNoOfBootstraps));
    MPTree.MaxNoOfTrees  := nm;
    MPTree.NoOfInitTrees := ni;
    MPTree.RandomOrder   := ro;
    FTreeList.isStats := true;
    if FTreeList.Count > 0 then
      for i := 0 to FTreeList.NoOfTrees-1 do
        BootstrapTrees.Compare(FTreeList[i].NodeArray, @FTreeList[i].StatsArray[FTreeList.NoOfOTUs]);
    FreememAndNil(MPTree.FreqTable);
  end;
end;

function TBootstrapMPTreeSearchThread.GetCurrentRep: Integer;
begin
  Result := -1;
  try
    LockThread.Acquire;
    inc(FCurrentRep);
    if FCurrentRep <= FNoOfBootstraps then
    begin
      Result := FCurrentRep;
      if Terminated then
        Result := -1;
    end;
  finally
    LockThread.Release;
  end;
  //if not Terminated then { never put a call to synchronize inside of code that is working with critical sections (LockThread) as it can lead to a deadlock }
  //  Synchronize(DoOnProgress);
end;


function TBootstrapMPTreeSearchThread.RepsCompleted: Integer;
begin
  Result := FCurrentRep;
end;

{ TMPTreeSearchChildThread }

function TMPTreeSearchChildThread.BootstrapSites(aFreqTable: PArrayOfInt): Integer;
var
  i,n: integer;
begin
  for i := 0 to MPTree.FNoOfInfoSites-1 do
    aFreqTable[i] := 0;
  for i := 0 to NoOfTotalSites-1 do { sample over total sites}
  begin
    n := Random(NoOfTotalSites);
    if n < MPTree.FNoOfInfoSites then { FreqTable only allocated enough memory to cover FNoOfInfoSites??????}
      Inc(aFreqTable[n]);
  end;
  if aFreqTable = MPTree.FreqTable then
    Result := NoOfInfoSites
  else
    Result := 1;
end;

constructor TMPTreeSearchChildThread.Create(TreeList: TTreeList;
  MappedSites: TList; NoOfSites: integer; NoOfBits: integer);
begin
  inherited Create(TreeList, MappedSites, NoOfSites, NoOfBits);
  FreeOnTerminate := False;
end;

destructor TMPTreeSearchChildThread.Destroy;
begin
  if Assigned(FBootstrapSampler) then
    FBootstrapSampler.Free;
  inherited Destroy;
end;


procedure TMPTreeSearchChildThread.Execute;
begin
  FDone := False;
  GetMem(MPTree.FreqTable, Sizeof(Integer)*MPTree.FNoOfInfoSites);
  inherited;
  FreememAndNil(MPTree.FreqTable);
end;

procedure TMPTreeSearchChildThread.Search;
var
  aRep: Integer;
  sampleNum: Integer = 0;
begin
  repeat
    aRep := MainThread.GetCurrentRep;
    if aRep < 0 then
      break;

    begin
      if not (BootstrapSites(MPTree.FreqTable) > 0) then
        continue;
    end;

    if MPTree.SearchMPTree then
    begin
      MainThread.AddBootTrees(MPTree.ParsimTrees);
    end;
  until (aRep < 0) or Terminated;
  FDone := true;
end;

end.


