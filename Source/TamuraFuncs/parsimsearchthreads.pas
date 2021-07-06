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

unit ParsimSearchThreads;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType,
  Classes, StdCtrls, ExtCtrls, SysUtils, Math,
  MegaConsts, syncobjs,
  MParsNode, MTreeData, MTreeList, MPartitionList,
  MTreeSearchThread, Dialogs;

// Koichiro I have replaced all $FFFFFFFF by pmResiX (defined in Megacosnts) because
// the compiler says that all if expressions always evaluate to False otherwise
// For consistency, I have also replaced all Chr($FF) by pmBaseN

type
//  TPointerArray = array[0..MaxInt div SizeOf(pointer)-1] of pointer;

  TParsimSearchThread = class(TTreeSearchThread)
  protected
    FExceptionName: String;
    FExceptionMessage: String;
    FOriSeqs: TList;
    ParsimTrees: TTreeList;
    Node: TpParsNodeArray;
    SCD:PArrayOfInt;
    FreqTable: PArrayOfInt;
    NoOfInfoSites: integer;
    NoOfTotalSites: integer;
    NoOfSingletons: integer;
    Bound: integer;

    FNoOfBits: integer;
    FNoOfOTUs: integer;
    FMaxNoOfTrees: integer;

    CountTreeLength: function:integer of object;
    SelectFurthestSequence: procedure of object;
    SetSCD: procedure of object;

    searchIterations: Int64;

    procedure SetInfoSites8;
    procedure SetInfoSites32;
    procedure SetSCD8;
    procedure SetSCD32;
    function CountTreeLengthAA:integer; virtual;
    function CountTreeLengthDNA:integer; virtual;
    procedure SelectFurthestSequence32;
    procedure SelectFurthestSequence8;
    function MakeInitialTree:integer;

    procedure InitTree;
    procedure LoadTreeData(tree: TTreeData);
    procedure SaveParsimonyTree;
    procedure RenewParsimonyTree;

//    function GetNoOfOTUs:integer;
    function GetNoOfTrees:integer; virtual;
    function GetTreeLength:integer;
    procedure Execute; override;

    procedure DoOnProgress; override;
  public
    property MyExceptionName: String read FExceptionName; // GS - added 12-22-2011, gives us a way to handle uncaught exceptions in the onterm procedure
    property MyExceptionMessage: String read FExceptionMessage; // GS - added 12-22-2011, gives us a way to handle uncaught exceptions in the onterm procedure
    property NoOfOTUs: integer read FNoOfOTUs; //GetNoOfOTUs;
    property NoOfTrees: integer read GetNoOfTrees;
    property MaxNoOfTrees: integer read FMaxNoOfTrees write FMaxNoOfTrees;

    property TreeLength: integer read GetTreeLength;

    constructor Create(Tree: TTreeList;
                       OriSeqs: TList;
                       NoOfSites: integer;
                       NoOfBits: integer);

    destructor Destroy; override;
  end;

  TAnalyzeUserTreeThread = class(TParsimSearchThread)
  protected
    procedure Search; override; // aren't doing search, just need the branch lengths but Execute calls the 'search' function.
  end;

  TBranchBoundSearchThread = class(TParsimSearchThread)
  protected
    procedure Search; override;
  end;

  TBootstrapBranchBoundSearchThread = class;

  { TBranchBoundChildSearchThread }

  TBranchBoundChildSearchThread = class(TBranchBoundSearchThread)
  protected
    FThreadIndex: Integer;
    FMainThread: TBootstrapBranchBoundSearchThread;
    FDone: Boolean;
    procedure Search; override;
    procedure InitialSearch; virtual;
    procedure Execute; override;
  public
    constructor Create(Tree: TTreeList;
                       OriSeqs: TList;
                       NoOfSites: integer;
                       NoOfBits: integer);
    property MainThread: TBootstrapBranchBoundSearchThread read FMainThread write FMainThread;
    property ThreadIndex: Integer read FThreadIndex write FThreadIndex;
    property Done: Boolean read FDone;
  end;

  { TBootstrapBranchBoundSearchThread }

  TBootstrapBranchBoundSearchThread = class(TBranchBoundChildSearchThread)
  protected
    FChildThread: array of TBranchBoundChildSearchThread;
    FNoOfThreads: Integer;
    FCurrentRep: Integer;
    FOriSeqs: TList;
    FNoOfBits: Integer;
    FThreadMemSize: Int64;
    NoOfBootstrappings: integer;
    LockThread: TCriticalSection;
    function EstimateThreadMemSize: Int64;
    function CheckChildThreadDone: Boolean;
    procedure DoOnBootstrapProgress;
    procedure Search; override;
    procedure InitialSearch; override;
    procedure Initialize;
  public
    BootstrapTrees: TPartitionList;

    constructor Create(Tree: TTreeList;
                       OriSeqs: TList;
                       NoOfSites: integer;
                       NoOfBits: integer;
                       NoOfBootReps: integer;
                       MaxPercentRam: Integer = DEFAULT_MAX_PERCENT_RAM);
    destructor Destroy; override;
    procedure AddTreeList(aTreeList: TTreeList);
    function GetCurrentRep: Integer;
  end;

  TCNISearchThread = class(TParsimSearchThread)
  protected
    InitialTrees: TTreeList;
    PartitionList: TPartitionList;
    Dt: integer;

    function CompareDt2Trees(n: TpParsNode):boolean;
    function CompareDt4Trees(n: TpParsNode):boolean;
    function CompareDt6Trees(n: TpParsNode):boolean;
    procedure Search; override;
  public
    constructor Create(Tree: TTreeList;  // Initial tree(s) must be included.
                       OriSeqs: TList;
                       NoOfSites: integer;
                       NoOfBits: integer;
                       SearchLevel: integer);
  end;

  TMiniMini_CNISearchThread = class(TCNISearchThread)
  private
    FSearchFactor: integer;
    LocalBound: PArrayOfInt;
  protected
    function GetNoOfTrees:integer; override;
    procedure Search; override;
  public
    constructor Create(Tree: TTreeList;
                       OriSeqs: TList;
                       NoOfSites: integer;
                       NoOfBits: integer;
                       SearchLevel: integer;
                       SearchFactor: integer);
  end;
  TBootstrapMiniMini_CNISearchThread = class;

  { TBootstrapMiniMini_CNIChildSearchThread }

  TBootstrapMiniMini_CNIChildSearchThread = class(TMiniMini_CNISearchThread)
  protected
    FThreadIndex: Integer;
    FMainThread: TBootstrapMiniMini_CNISearchThread;
    FDone: Boolean;
    procedure Search; override;
    procedure InitialSearch; virtual;
    procedure Execute; override;
  public
    constructor Create(Tree: TTreeList;
                       OriSeqs: TList;
                       NoOfSites: integer;
                       NoOfBits: integer;
                       SearchLevel: integer;
                       SearchFactor: integer);
    property MainThread: TBootstrapMiniMini_CNISearchThread read FMainThread write FMainThread;
    property ThreadIndex: Integer read FThreadIndex write FThreadIndex;
    property Done: Boolean read FDone;
  end;

  { TBootstrapMiniMini_CNISearchThread }

  TBootstrapMiniMini_CNISearchThread = class(TBootstrapMiniMini_CNIChildSearchThread)
  protected
    FChildThread: array of TBootstrapMiniMini_CNIChildSearchThread;
    FNoOfThreads: Integer;
    FCurrentRep: Integer;
    FOriSeqs: TList;
    FNoOfBits: Integer;
    FThreadMemSize: Int64;
    FSearchLevel: Integer;
    NoOfBootstrappings: integer;
    LockThread: TCriticalSection;
    function EstimateThreadMemSize: Int64;
    function CheckChildThreadDone: Boolean;
    procedure DoOnBootstrapProgress;
    procedure Search; override;
    procedure InitialSearch; override;
    procedure Initialize;
  public
    BootstrapTrees: TPartitionList;

    constructor Create(Tree: TTreeList;
                       OriSeqs: TList;
                       NoOfSites: integer;
                       NoOfBits: integer;
                       SearchLevel: integer;
                       SearchFactor: integer;
                       NoOfBootReps: integer;
                       MaxPercentRam: Integer = DEFAULT_MAX_PERCENT_RAM);
    destructor Destroy; override;
    procedure AddTreeList(aTreeList: TTreeList);
    function GetCurrentRep: Integer;
  end;

  TRBA_CNISearchThread = class(TCNISearchThread)
  protected
    Rep: integer;
    FNoOfReps: integer;
    FFixedOrder: boolean;
    function GetNoOfTrees:integer; override;
    function MakeRandomAdditionTree:integer;
    procedure Search; override;
  public
    property NoOfReps: integer read FNoOfReps write FNoOfReps;
    property NoOfTrees: integer read GetNoOfTrees;
    property FixedOrder: boolean read FFixedOrder write FFixedOrder;

    constructor Create(Tree: TTreeList;
                       OriSeqs: TList;
                       NoOfSites: integer;
                       NoOfBits: integer;
                       SearchLevel: integer;
                       NoOfReplications: integer);
  end;

  TBootstrapRBA_CNISearchThread = class(TRBA_CNISearchThread)
  protected
    NoOfBootstrappings: integer;
    procedure DoOnBootstrapProgress;
    procedure Search; override;
  public
    BootstrapTrees: TPartitionList;

    constructor Create(Tree: TTreeList;
                       OriSeqs: TList;
                       NoOfSites: integer;
                       NoOfBits: integer;
                       SearchLevel: integer;
                       NoOfReplications: integer;
                       NoOfBootReps: integer);
  end;
{
function GetAncestralState(AncState: PArrayOfLongint;
                           NodeArray: PArrayOfNodeData; NoOfOTUs: integer;
                           HideAmbiguousState: boolean):integer;
         // Returns the no. of substitutions.

function GetAvgBLen(NodeArray: PArrayOfNodeData; BLen: PArrayOfDouble;
                    NoOfOTUs: integer;
                    InfoSite: PArrayOfLongInt2D;
                    NoOfInfoSites, NoOfCharStates: integer):double;
}

implementation

uses
  {$IFDEF VISUAL_BUILD}Mega_Main,{$ENDIF}
  MGlobalSettings, MegaUtils, mmemutils;

procedure BootStrap(FreqTable: PArrayOfInt; NoOfSites, NoOfBootstrappings: integer);
var i: integer;
begin
  for i := 0 to NoOfSites-1 do
    FreqTable[i] := 0;
  for i := 1 to NoOfSites do
    Inc(FreqTable[Random(NoOfSites)]);
end;

procedure BootstrapSubset(freqTable: PArrayOfInt; numTotalSites, numSampledSites: Integer);
var
  i: Integer;
  numSampled: Integer = 0;
  r: Integer;
  selectedSites: ArrayOfInteger;
begin
  SetLength(selectedSites, numSampledSites);
  for i := 0 to numTotalSites - 1 do
    freqTable[i] := 0;

  while numSampled < numSampledSites do
  begin
    r := Random(numTotalSites);
    if freqTable[r] = 0 then
    begin
      freqTable[r] := 1;
      selectedSites[numSampled] := r;
      inc(numSampled);
    end;
  end;

  while numSampled < numTotalSites do
  begin
    r := Random(numSampledSites);
    inc(freqTable[selectedSites[r]]);
    inc(numSampled);
  end;
end;

// the following two functions are recommended by SK
function KumarComputePCharMPSteps(Anc, Des1, Des2: PAnsiChar; Len: Longint; FreqTable: PArrayOfInt): Longint;
var
  i: Longint;
  a, b, c: AnsiChar;
begin
  Result := 0;
  if FreqTable = nil then
  begin
    for i:=0 to Len-1 do
    begin
      a := Des1[i];
      b := Des2[i];
      c := AnsiChar(Integer(a) and Integer(b)); // this construct tells compiler to optimize using a byte registers
      if c = #0 then
      begin
        Anc[i] := AnsiChar(Integer(a) or Integer(b));
        Inc(Result);
      end
      else
        Anc[i] := c;
    end;
    Exit;
  end;

  for i:=0 to Len-1 do
  begin
    if FreqTable[i] <> 0 then
    begin
      a := Des1[i];
      b := Des2[i];
      c := AnsiChar(Integer(a) and Integer(b)); // this construct tells compiler to optimize using a byte registers
      if c = #0 then
      begin
        Anc[i] := AnsiChar(Integer(a) or Integer(b));
        Inc(Result, FreqTable[i]);
      end
      else
        Anc[i] := c;
    end;
  end;
end;

// the following two functions are recommended by SK
function KumarComputePCharMPStepsWtd(Anc, Des1, Des2: PAnsiChar; Len: Longint; Weight: Integer; FreqTable: PArrayOfInt): Longint;
var
  i: Longint;
  a, b, c: AnsiChar;
begin
  Result := 0;
  if FreqTable = nil then
  begin
    for i:=0 to Len-1 do
    begin
      a := Des1[i];
      b := Des2[i];
      c := AnsiChar(Integer(a) and Integer(b)); // this construct tells compiler to optimize using a byte registers
      if c = #0 then
      begin
        Anc[i] := AnsiChar(Integer(a) or Integer(b));
        Inc(Result, Weight);
      end
      else
        Anc[i] := c;
    end;
    Exit;
  end;

  for i:=0 to Len-1 do
  begin
    if FreqTable[i] <> 0 then
    begin
      a := Des1[i];
      b := Des2[i];
      c := AnsiChar(Integer(a) and Integer(b)); // this construct tells compiler to optimize using a byte registers
      if c = #0 then
      begin
        Anc[i] := AnsiChar(Integer(a) or Integer(b));
        Inc(Result, FreqTable[i]*Weight);
      end
      else
        Anc[i] := c;
    end;
  end;
end;

function KumarComputePLongMPSteps(Anc, Des1, Des2: PArrayOfLongInt; Len: Longint; FreqTable: PArrayOfInt): Longint;
var
  i: Longint;
  c: LongInt;
begin
  Result := 0;
  if FreqTable = nil then
  begin
    for i:=0 to Len-1 do
    begin
      c := Des1[i] and Des2[i]; // this construct tells compiler to optimize using a byte registers
      if c = 0 then
      begin
        Anc[i] := Des1[i] or Des2[i];
        Inc(Result);
      end
      else
        Anc[i] := c;
    end;
    Exit;
  end;

  for i:=0 to Len-1 do
  begin
    if FreqTable[i] <> 0 then
    begin
      c := Des1[i] and Des2[i]; // this construct tells compiler to optimize using a byte registers
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

function CountTreeLength32(Node: TpParsNode; NoOfOTUs, NoOfInfoSites: integer):integer;

    procedure CountStep(p : TpParsNode);
    var a,d1,d2: PArrayOfLongint;
    begin
        if p.Length >= 0 then Exit;
        p.Length := 0;
        if p.des1.index >= NoOfOTUs then begin
            CountStep(p.des1);
            Inc(p.Length, p.des1.Length);
        end;
        if p.des2.index >= NoOfOTUs then begin
            CountStep(p.des2);
            Inc(p.Length, p.des2.Length);
        end;
        a := p.InfoSite;
        d1 := p.des1.InfoSite;
        d2 := p.des2.InfoSite;
        p.Length := p.Length + KumarComputePLongMPSteps(a, d1, d2, NoOfInfoSites, nil);
        //for i := 0 to NoOfInfoSites-1 do begin
        //    a[i] := d1[i] and d2[i];
        //    if a[i] = 0 then begin
        //        Inc(p.Length);
        //        a[i] := d1[i] or d2[i];
        //    end;
        //end;
    end;

begin
    CountStep(Node);
    Result := Node.Length;
end;

function CountTreeLength8(Node: TpParsNode; NoOfOTUs, NoOfInfoSites: integer):integer;

    procedure CountStep(p : TpParsNode);
    var a,d1,d2: PAnsiChar;
    begin
        if p.Length >= 0 then Exit;
        p.Length := 0;
        if p.des1.index >= NoOfOTUs then begin
            CountStep(p.des1);
            Inc(p.Length, p.des1.Length);
        end;
        if p.des2.index >= NoOfOTUs then begin
            CountStep(p.des2);
            Inc(p.Length, p.des2.Length);
        end;
        a := p.InfoSite;
        d1 := p.des1.InfoSite;
        d2 := p.des2.InfoSite;
        p.Length := p.Length + KumarComputePCharMPSteps(a, d1, d2, NoOfInfoSites, nil);
        //for i := 0 to NoOfInfoSites-1 do begin
        //    a[i] := Char(Integer(d1[i]) and Integer(d2[i]));
        //    if a[i] = #0 then begin
        //        Inc(p.Length);
        //        a[i] := Char(Integer(d1[i]) or Integer(d2[i]));
        //    end;
        //end;
    end;

begin
    CountStep(Node);
    Result := Node.Length;
end;

procedure ChangeRoot(root, newposition: TpParsNode);
var a, p, d : TpParsNode;

    function SibNode(p : TpParsNode):TpParsNode;
    begin
        if p.anc = nil then
            Result := nil
        else if p = p.anc.des1 then
            Result := p.anc.des2
        else if p = p.anc.des2 then
            Result := p.anc.des1
        else
            Result := nil;
    end;

begin
    if newposition.anc = root then Exit;

    p := newposition;
    while p.anc <> root do
        p := p.anc;

    d := newposition;
    p := d.anc;
    a := p.anc;
    while p <> root do begin
        p.anc := d;
        if d = p.des1 then
            p.des1 := a
        else
            p.des2 := a;
        d := p;
        p := a;
        a := a.anc;
    end;
    if d = p.des1 then begin
        p.des2.anc := d;
        if p = d.des1 then
            d.des1 := p.des2
        else
            d.des2 := p.des2;
    end
    else begin
        p.des1.anc := d;
        if p = d.des1 then
            d.des1 := p.des1
        else
            d.des2 := p.des1;
    end;

    p := newposition.anc;
    p.anc := root;
    newposition.anc := root;
    root.des1 := newposition;
    root.des2 := p;
end;

procedure ResetLength(p: TpParsNode);

    procedure ResetEachNode(p: TpParsNode);
    begin
        p.Length := -1;
        if (p.anc <> nil) and (p.anc.Length > -1) then
            ResetEachNode(p.anc);
    end;

begin
    ResetEachNode(p);
end;

procedure AddBranch(r, n, b : TpParsNode);
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

    ResetLength(r);
end;

procedure RemoveBranch(n : TpParsNode);
var a, aa, b : TpParsNode;
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

    ResetLength(aa);
end;

{ TBootstrapMiniMini_CNIChildSearchThread }

procedure TBootstrapMiniMini_CNIChildSearchThread.Search;
var
  aRep: Integer;
begin
  GetMem(FreqTable, SizeOf(Integer)*NoOfInfoSites);
  repeat
    aRep := MainThread.GetCurrentRep;
    if aRep < 0 then
      break;
    Bootstrap(FreqTable, NoOfInfoSites, -1);
    inherited Search;
    MainThread.AddTreeList(ParsimTrees);
    ParsimTrees.DeleteAll;
  until (aRep < 0) or Terminated;
  FDone := true;
end;

procedure TBootstrapMiniMini_CNIChildSearchThread.InitialSearch;
begin
  inherited Search;
end;

constructor TBootstrapMiniMini_CNIChildSearchThread.Create(Tree: TTreeList; OriSeqs: TList; NoOfSites, NoOfBits, SearchLevel, SearchFactor: integer);
begin
  inherited Create(Tree, OriSeqs, NoOfSites, NoOfBits, SearchLevel, SearchFactor);
  FreeOnTerminate := False;
end;

procedure TBootstrapMiniMini_CNIChildSearchThread.Execute;
begin
  inherited Execute;
end;

{ TBranchBoundChildSearchThread }

procedure TBranchBoundChildSearchThread.Search;
var
  aRep: Integer;
begin
  GetMem(FreqTable, SizeOf(Integer)*NoOfInfoSites);
  repeat
    aRep := MainThread.GetCurrentRep;
    if aRep < 0 then
      break;
    Bootstrap(FreqTable, NoOfInfoSites, -1);
    inherited Search;
    MainThread.AddTreeList(ParsimTrees);
    ParsimTrees.DeleteAll;
  until (aRep < 0) or Terminated;
  FDone := true;
end;

procedure TBranchBoundChildSearchThread.InitialSearch;
begin
  inherited Search;
end;

constructor TBranchBoundChildSearchThread.Create(Tree: TTreeList; OriSeqs: TList; NoOfSites, NoOfBits: integer);
begin
  inherited Create(Tree, OriSeqs, NoOfSites, NoOfBits);
  FreeOnTerminate := False;
end;

procedure TBranchBoundChildSearchThread.Execute;
begin
  inherited Execute;
end;


//////////////////////
// TParsimSearchThread
//////////////////////

constructor TParsimSearchThread.Create(Tree: TTreeList;
                                       OriSeqs: TList;
                                       NoOfSites: integer;
                                       NoOfBits: integer);
var i: integer;
begin
  inherited Create;
  searchIterations := 0;
  FExceptionName := 'none';
  FExceptionMessage := 'none';
  FreqTable := nil;
  FOriSeqs := OriSeqs;
  FNoOfOTUs := OriSeqs.Count;
  NoOfTotalSites := NoOfSites;
  FMaxNoOfTrees  := 0; // No limit by default;

  if NoOfBits > 8 then
      FNoOfBits := 32
  else
      FNoOfBits := 8;

  ParsimTrees := Tree;
  if ParsimTrees = nil then
    ParsimTrees := TTreeList.Create;

  GetMem(SCD, SizeOf(Integer)*NoOfOTUs);
  for i := 0 to NoOfOTUs-1 do
    SCD[i] := 0;
  GetMem(Node, SizeOf(TpParsNode)*(2*NoOfOTUs-1));
  for i := 0 to 2*NoOfOTUs-2 do begin
      NEW(Node[i]);
      Node[i].index := i;
      Node[i].InfoSite := nil;
      Node[i].anc := nil;
      Node[i].des1 := nil;
      Node[i].des2 := nil;
      Node[i].Length := -1;
  end;
end;

destructor TParsimSearchThread.Destroy;
var i : integer;
begin
  if FNoOfBits = 8 then begin
    for i := 0 to 2*NoOfOTUs-2 do
      if Node[i].InfoSite <> nil then
        StrDispose(PAnsiChar(Node[i].InfoSite));
  end
  else
//  for i := NoOfOTUs to 2*NoOfOTUs-2 do  <- @SK  Memory leak
    for i := 0 to 2*NoOfOTUs-2 do  // fixed by SK
      FreeMemAndNil(Node[i].InfoSite);
  for i := 0 to 2*NoOfOTUs-2 do
    Dispose(Node[i]);
  FreeMemAndNil(Node);
  FreeMemAndNil(SCD);
  inherited;
end;

procedure TParsimSearchThread.SetInfoSites32;
type TBooleanArray = array[0..MaxInt-1] of Boolean;
     PBooleanArray = ^TBooleanArray;

var IsInfoSite: PBooleanArray;
    i,j,k,n,s: integer;
    CharState, NoOfStates: array[0..32] of integer;
    flag: boolean;
begin
  GetMem(IsInfoSite, SizeOf(boolean)*NoOfTotalSites);

  NoOfInfoSites := 0;
  NoOfSingletons := 0;
  for i := 0 to NoOfTotalSites-1 do
    IsInfoSite[i] := false;

  for i := 0 to NoOfTotalSites-1 do begin
    for j := 1 to 32 do
      CharState[j] := 0;
    NoOfStates[0] := 0;
    n := 0;
    j := 0;
    while (j < NoOfOTUs) and (PArrayOfLongint(FOriSeqs[j])[i] = pmResiX) do Inc(j);  // SK swapped to statements as the latter is not valid if the first is false
    if j < NoOfOTUs then begin
      NoOfStates[0] := 1;
      NoOfStates[1] := 1;
      CharState[1] := PArrayOfLongint(FOriSeqs[j])[i];
      n := 1;
    end;
    Inc(j);
    if j < NoOfOTUs then begin
      for j := j to NoOfOTUs-1 do begin
        if PArrayOfLongint(FOriSeqs[j])[i] = pmResiX then Continue;
        flag := false;
        for k := 1 to NoOfStates[0] do
          if PArrayOfLongint(FOriSeqs[j])[i] = CharState[k] then begin
            Inc(NoOfStates[k]);
            flag := true;
            Break;
          end;
        if not flag then begin
          Inc(NoOfStates[0]);
          NoOfStates[NoOfStates[0]] := 1;
          CharState[NoOfStates[0]] := PArrayOfLongint(FOriSeqs[j])[i];
        end;
        Inc(n);
      end;
    end;
    if NoOfStates[0] > 1 then begin
      j := 0;
      s := 0;
      for k := 1 to NoOfStates[0] do
        if NoOfStates[k] = 1 then
          Inc(s)
        else
          Inc(j);
      if s = n then Dec(s);
      if j > 1 then begin
        IsInfoSite[i] := true;
        Inc(NoOfInfoSites);
      end
      else
        Inc(NoOfSingletons, s);
    end;
  end;

  for i := 0 to 2*NoOfOTUs-2 do
    GetMem(Node[i].InfoSite, SizeOf(LongInt)*NoOfInfoSites);
  k := 0;
  for i := 0 to NoOfTotalSites-1 do
    if IsInfoSite[i] then begin
      for j := 0 to NoOfOTUs-1 do
        PArrayOfLongint(Node[j].InfoSite)[k] := PArrayOfLongint(FOriSeqs[j])[i];
      Inc(k);
    end;

  CountTreeLength := CountTreeLengthAA;
  SelectFurthestSequence := SelectFurthestSequence32;
  SetSCD := SetSCD32;

  FreeMemAndNil(IsInfoSite);
end;

procedure TParsimSearchThread.SetInfoSites8;
type TBooleanArray = array[0..MaxInt-1] of Boolean;
     PBooleanArray = ^TBooleanArray;

var IsInfoSite: PBooleanArray;
    i,j,k,n,s: integer;
    NoOfStates: array[0..8] of integer;
    CharState: array[0..8] of AnsiChar;
    flag: boolean;
begin
  GetMem(IsInfoSite, SizeOf(boolean)*NoOfTotalSites);

  NoOfInfoSites := 0;
  NoOfSingletons := 0;
  for i := 0 to NoOfTotalSites-1 do
    IsInfoSite[i] := false;

  for i := 0 to NoOfTotalSites-1 do begin
    for j := 1 to 8 do
      CharState[j] := #0;
    NoOfStates[0] := 0;
    n := 0;
    j := 0;
    while (j < NoOfOTUs) and (PAnsiChar(FOriSeqs[j])[i] = pmBaseN) do Inc(j);
    if j < NoOfOTUs then begin
      NoOfStates[0] := 1;
      NoOfStates[1] := 1;
      CharState[1] := PAnsiChar(FOriSeqs[j])[i];
      n := 1;
    end;
    Inc(j);
    if j < NoOfOTUs then begin
      for j := j to NoOfOTUs-1 do begin
        if PAnsiChar(FOriSeqs[j])[i] = pmBaseN then Continue;
        flag := false;
        for k := 1 to NoOfStates[0] do
          if PAnsiChar(FOriSeqs[j])[i] = CharState[k] then begin
            Inc(NoOfStates[k]);
            flag := true;
            Break;
          end;
        if not flag then begin
          Inc(NoOfStates[0]);
          NoOfStates[NoOfStates[0]] := 1;
          CharState[NoOfStates[0]] := PAnsiChar(FOriSeqs[j])[i];
        end;
        Inc(n);
      end;
    end;
    if NoOfStates[0] > 1 then begin
      j := 0;
      s := 0;
      for k := 1 to NoOfStates[0] do
        if NoOfStates[k] = 1 then
          Inc(s)
        else
          Inc(j);
      if s = n then Dec(s);
      if j > 1 then begin
        IsInfoSite[i] := true;
        Inc(NoOfInfoSites);
      end
      else
        Inc(NoOfSingletons, s);
    end;
  end;

  for i := 0 to 2*NoOfOTUs-2 do
    {$IFDEF FPC}
      Node[i].InfoSite := StrAlloc(NoOfInfoSites);
    {$ELSE}
     Node[i].InfoSite := AnsiStrAlloc(NoOfInfoSites);
    {$ENDIF}
  k := 0;
  for i := 0 to NoOfTotalSites-1 do
    if IsInfoSite[i] then begin
      for j := 0 to NoOfOTUs-1 do
        PAnsiChar(Node[j].InfoSite)[k] := PAnsiChar(FOriSeqs[j])[i];
      Inc(k);
    end;
  CountTreeLength := CountTreeLengthDNA;
  SelectFurthestSequence := SelectFurthestSequence8;
  SetSCD := SetSCD8;

  FreeMemAndNil(IsInfoSite);
end;

procedure TParsimSearchThread.SetSCD32;
var CharState: array[1..32] of integer;
    i,j,k,n: integer;
    flag: boolean;
begin
  for i := Low(CharState) to High(CharState) do
    CharState[i] := 0;
  for i := 0 to NoOfOTUs-1 do
    SCD[i] := 0;

  if FreqTable <> nil then begin
    for j := 0 to NoOfInfosites-1 do begin
      if FreqTable[j] = 0 then Continue;
      n := 0;
      for i := 0 to NoOfOTUs-1 do begin
        if PArrayOfLongint(Node[i].InfoSite)[j] = pmResiX then Continue;
        if n = 0 then begin
          Inc(n);
          CharState[n] := PArrayOfLongint(Node[i].InfoSite)[j];
        end
        else begin
          flag := false;
          for k := 1 to n do
            if PArrayOfLongint(Node[i].InfoSite)[j] = CharState[k] then begin
              flag := true;
              Break;
            end;
          if not flag then begin
            Inc(n);
            CharState[n] := PArrayOfLongint(Node[i].InfoSite)[j];
          end;
        end;
        Inc(SCD[i], n*FreqTable[j]);
      end;
    end;
  end
  else begin
    for j := 0 to NoOfInfosites-1 do begin
      n := 0;
      for i := 0 to NoOfOTUs-1 do begin
        if PArrayOfLongint(Node[i].InfoSite)[j] = pmResiX then Continue;
        if n = 0 then begin
          Inc(n);
          CharState[n] := PArrayOfLongint(Node[i].InfoSite)[j];
        end
        else begin
          flag := false;
          for k := 1 to n do
            if PArrayOfLongint(Node[i].InfoSite)[j] = CharState[k] then begin
              flag := true;
              Break;
            end;
          if not flag then begin
            Inc(n);
            CharState[n] := PArrayOfLongint(Node[i].InfoSite)[j];
          end;
        end;
        Inc(SCD[i], n);
      end;
    end;
  end;

  for i := 0 to NoOfOTUs-1 do
    SCD[i] := SCD[NoOfOTUs-1] -SCD[i];
end;

procedure TParsimSearchThread.SetSCD8;
var CharState: array[1..8] of AnsiChar;
    i,j,k,n: integer;
    flag: boolean;
begin
  for i := Low(CharState) to High(CharState) do
    CharState[i] := #0;
  for i := 0 to NoOfOTUs-1 do
    SCD[i] := 0;

  if FreqTable <> nil then begin
    for j := 0 to NoOfInfosites-1 do begin
      if FreqTable[j] = 0 then Continue;
      n := 0;
      for i := 0 to NoOfOTUs-1 do begin
        if PAnsiChar(Node[i].InfoSite)[j] = pmBaseN then Continue;
        if n = 0 then begin
          Inc(n);
          CharState[n] := PAnsiChar(Node[i].InfoSite)[j];
        end
        else begin
          flag := false;
          for k := 1 to n do
            if PAnsiChar(Node[i].InfoSite)[j] = CharState[k] then begin
              flag := true;
              Break;
            end;
          if not flag then begin
            Inc(n);
            CharState[n] := PAnsiChar(Node[i].InfoSite)[j];
          end;
        end;
        Inc(SCD[i], n*FreqTable[j]);
      end;
    end;
  end
  else begin
    for j := 0 to NoOfInfosites-1 do begin
      n := 0;
      for i := 0 to NoOfOTUs-1 do begin
        if PAnsiChar(Node[i].InfoSite)[j] = pmBaseN then Continue;
        if n = 0 then begin
          Inc(n);
          CharState[n] := PAnsiChar(Node[i].InfoSite)[j];
        end
        else begin
          flag := false;
          for k := 1 to n do
            if PAnsiChar(Node[i].InfoSite)[j] = CharState[k] then begin
              flag := true;
              Break;
            end;
          if not flag then begin
            Inc(n);
            CharState[n] := PAnsiChar(Node[i].InfoSite)[j];
          end;
        end;
        Inc(SCD[i], n);
      end;
    end;
  end;

  for i := 0 to NoOfOTUs-1 do
    SCD[i] := SCD[NoOfOTUs-1] -SCD[i];
end;

function TParsimSearchThread.GetNoOfTrees:integer;
begin
  result := ParsimTrees.NoOfTrees;
end;

/// <summary>Code to execute whenever a TParsimSearchThread is started/resumed.</summary>
/// <remarks>Glen added the try/except block on 12-22-2011 as a way of trapping out of memory
/// exceptions so we can handle them gracefully. Ideally, this would have been done in
/// TTreeSearchThread but there exists some kind of naming conflict in that unit with
/// SysUtils (same problem caused this try/except code to be added to MLTreeThread.Execute).</remarks>
procedure TParsimSearchThread.Execute;
begin
  try
    if FNoOfBits = 8 then
      SetInfoSites8
    else
      SetInfoSites32;
    inherited;
  Except
    On E : EOutOfMemory do // trap out of memory exceptions and handle it ourselves in the onterm procedure
    begin
      FExceptionName := E.ClassName;
      FExceptionMessage := E.Message;
    end;
    On E : Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      FExceptionName := 'HandledException';  // so the onterm procedure won't try to display tree explorer
      FExceptionMessage := 'Already taken care of.';
      {$ELSE}
      FExceptionName := E.ClassName;
      FExceptionMessage := E.Message;
      {$ENDIF}
    end;
  end;
end;

function TParsimSearchThread.GetTreeLength:integer;
begin
  if Bound = 0 then
    Result := 0
  else
    Result := Bound +NoOfSingletons;
end;

function TParsimSearchThread.CountTreeLengthAA:integer;

    procedure CountStep(p : TpParsNode);
    var a, d1, d2: PArrayOfLongint;
    begin
        if p.Length >= 0 then Exit;
        p.Length := 0;
        if p.des1.index >= NoOfOTUs then begin
            CountStep(p.des1);
            Inc(p.Length, p.des1.Length);
        end;
        if p.des2.index >= NoOfOTUs then begin
            CountStep(p.des2);
            Inc(p.Length, p.des2.Length);
        end;
        a := p.InfoSite;
        d1 := p.des1.InfoSite;
        d2 := p.des2.InfoSite;
        p.Length := p.Length + KumarComputePLongMPSteps(a, d1, d2, NoOfInfoSites, FreqTable);
        //for i := 0 to InfoNum-1 do begin
        //    a[i] := d1[i] and d2[i];
        //    if a[i] = 0 then begin
        //       Inc(p.Length);
        //        a[i] := d1[i] or d2[i];
        //    end;
        //end;
    end;

begin
    CountStep(Node[2*NoOfOTUs-2]);
    Result := Node[2*NoOfOTUs-2].Length;
end;

function TParsimSearchThread.CountTreeLengthDNA:integer;

    procedure CountStep(p : TpParsNode);
    var a, d1, d2: PAnsiChar;
    begin
        if p.Length >= 0 then Exit;
        p.Length := 0;
        if p.des1.index >= NoOfOTUs then begin
            CountStep(p.des1);
            Inc(p.Length, p.des1.Length);
        end;
        if p.des2.index >= NoOfOTUs then begin
            CountStep(p.des2);
            Inc(p.Length, p.des2.Length);
        end;
        a := p.InfoSite;
        d1 := p.des1.InfoSite;
        d2 := p.des2.InfoSite;
        p.Length := p.Length + KumarComputePCharMPSteps(a, d1, d2, NoOfInfoSites, FreqTable);
    end;

begin
    CountStep(Node[2*NoOfOTUs-2]);
    Result := Node[2*NoOfOTUs-2].Length;
end;

procedure TParsimSearchThread.LoadTreeData(tree: TTreeData);

  procedure ResetNodeOrder;
  var p: TpParsNode;
      i,j: integer;
  begin
    for i := 0 to NoOfOTUs-2 do
      if Node[i].index <> i then
        for j := i+1 to NoOfOTUs-1 do
          if Node[j].index = i then begin
            p := Node[i];
            Node[i] := Node[j];
            Node[j] := p;
            Break;
          end;
  end;

var i : integer;
begin
    ResetNodeOrder;
    for i := 0 to NoOfOTUs-2 do begin
        Node[i+NoOfOTUs].des1 := Node[tree.NodeArray[i].des1];
        Node[i+NoOfOTUs].des2 := Node[tree.NodeArray[i].des2];
        Node[tree.NodeArray[i].des1].anc := Node[i+NoOfOTUs];
        Node[tree.NodeArray[i].des2].anc := Node[i+NoOfOTUs];
        Node[tree.NodeArray[i].des1].sib := Node[tree.NodeArray[i].des2];
        Node[tree.NodeArray[i].des2].sib := Node[tree.NodeArray[i].des1];
        Node[i+NoOfOTUs].Length := -1;
    end;
end;

procedure TParsimSearchThread.RenewParsimonyTree;
var tree: TTreeData;
    i : integer;
begin
    Bound := Node[2*NoOfOTUs-2].length;
    ParsimTrees.DeleteAll;
    tree := TTreeData.Create(NoOfOTUs,false,false,false);
    for i := 0 to NoOfOTUs-2 do begin
        tree.NodeArray[i].des1 := Node[i+NoOfOTUs].des1.index;
        tree.NodeArray[i].des2 := Node[i+NoOfOTUs].des2.index;
    end;
    tree.Freq := 1.0;
    tree.Value := TreeLength;
    ParsimTrees.Add(tree);
    OnProgress(FProgress);  //@SK
end;

procedure TParsimSearchThread.SaveParsimonyTree;
var tree: TTreeData;
    i : integer;
begin
    if (MaxNoOfTrees > 0) and (ParsimTrees.Count = MaxNoOfTrees) then
      exit;

    tree := TTreeData.Create(NoOfOTUs,false,false,false);
    for i := 0 to NoOfOTUs-2 do begin
        tree.NodeArray[i].des1 := Node[i+NoOfOTUs].des1.index;
        tree.NodeArray[i].des2 := Node[i+NoOfOTUs].des2.index;
    end;
    tree.Freq := 1.0;
    tree.Value := TreeLength;
    ParsimTrees.Add(tree);
    OnProgress(FProgress);  //@SK
end;

procedure TParsimSearchThread.SelectFurthestSequence32;
var i,j,k,m1,m2,max,count : integer;
    p : TpParsNode;
    p1,p2: PArrayOfLongint;
begin
    max := 0;
    m1 := 0;
    m2 := 1;
    for i := 0 to NoOfOTUs-2 do
        for j := i+1 to NoOfOTUs-1 do begin
            p1 := Node[i].InfoSite;
            p2 := Node[j].InfoSite;
            count := 0;
            for k := 0 to NoOfInfoSites-1 do
                if p1[k] <> p2[k] then
                    Inc(count);
            if count > max then begin
                max := count;
                m1 := i;
                m2 := j;
            end;
        end;

    p := Node[0];
    Node[0] := Node[m1];
    Node[m1] := p;
    p := Node[1];
    Node[1] := Node[m2];
    Node[m2] := p;

    max := 0;
    m1 := 2;
    for i := 2 to NoOfOTUs-1 do begin
        count := 0;
        for j := 0 to 1 do begin
            p1 := Node[i].InfoSite;
            p2 := Node[j].InfoSite;
            for k := 0 to NoOfInfoSites-1 do
                if p1[k] <> p2[k] then
                    INC(count);
        end;
        if count > max then begin
             max := count;
             m1 := i;
        end;
    end;
    p := Node[2];
    Node[2] := Node[m1];
    Node[m1] := p;
end;

procedure TParsimSearchThread.SelectFurthestSequence8;
var i,j,k,m1,m2,max,count : integer;
    p : TpParsNode;
    p1,p2: PAnsiChar;
begin
    max := 0;
    m1 := 0;
    m2 := 1;
    for i := 0 to NoOfOTUs-2 do
        for j := i+1 to NoOfOTUs-1 do begin
            p1 := Node[i].InfoSite;
            p2 := Node[j].InfoSite;
            count := 0;
            for k := 0 to NoOfInfoSites-1 do
                if p1[k] <> p2[k] then
                    Inc(count);
            if count > max then begin
                max := count;
                m1 := i;
                m2 := j;
            end;
        end;

    p := Node[0];
    Node[0] := Node[m1];
    Node[m1] := p;
    p := Node[1];
    Node[1] := Node[m2];
    Node[m2] := p;

    max := 0;
    m1 := 2;
    for i := 2 to NoOfOTUs-1 do begin
        count := 0;
        for j := 0 to 1 do begin
            p1 := Node[i].InfoSite;
            p2 := Node[j].InfoSite;
            for k := 0 to NoOfInfoSites-1 do
                if p1[k] <> p2[k] then
                    INC(count);
        end;
        if count > max then begin
             max := count;
             m1 := i;
        end;
    end;
    p := Node[2];
    Node[2] := Node[m1];
    Node[m1] := p;
end;

procedure TParsimSearchThread.InitTree;
var i : integer;
begin
    Node[2*NoOfOTUs-2].des1 := Node[0];
    Node[2*NoOfOTUs-2].des2 := Node[NoOfOTUs];
    Node[0].anc := Node[2*NoOfOTUs-2];
    Node[0].sib := Node[NoOfOTUs];
    Node[NoOfOTUs].anc := Node[2*NoOfOTUs-2];
    Node[NoOfOTUs].sib := Node[0];
    Node[NoOfOTUs].des1 := Node[1];
    Node[NoOfOTUs].des2 := Node[2];
    Node[1].anc := Node[NoOfOTUs];
    Node[2].anc := Node[NoOfOTUs];
    Node[1].sib := Node[2];
    Node[2].sib := Node[1];
    for i := NoOfOTUs to 2*NoOfOTUs-2 do
        Node[i].Length := -1;
end;

function TParsimSearchThread.MakeInitialTree:integer;
var i,j,k,n,b,bmin,sum,min1,min2,min3,lmin1,lmin2,lmin3: integer;
    p : TpParsNode;
begin
  bmin := -1; //@SK mega2b4
  n := -1; //@SK mega2b4
  b := -1; //@SK mega2b4
  for i := 3 to NoOfOTUs-1 do
  begin
    min1 := NoOfOTUs*NoOfInfoSites;
    min2 := NoOfOTUs*NoOfInfoSites;
    min3 := NoOfOTUs*NoOfInfoSites;
    for j := i to NoOfOTUs-1 do begin
        lmin1 := NoOfOTUs*NoOfInfoSites;
        lmin2 := NoOfOTUs*NoOfInfoSites;
        lmin3 := NoOfOTUs*NoOfInfoSites;
        for k := 1 to i-1 do begin
            AddBranch(Node[NoOfOTUs+j-2], Node[j], Node[k]);
            sum := CountTreeLength;
            if sum <= lmin1 then begin
                lmin3 := lmin2;
                lmin2 := lmin1;
                lmin1 := sum;
                b := k;
            end
            else if sum <= lmin2 then begin
                lmin3 := lmin2;
                lmin2 := sum;
            end
            else if sum < lmin3 then
                lmin3 := sum;
            RemoveBranch(Node[j]);
        end;
        for k := NoOfOTUs to NoOfOTUs+i-3 do begin
            AddBranch(Node[NoOfOTUs+j-2], Node[j], Node[k]);
            sum := CountTreeLength;
        if sum <= lmin1 then begin
                lmin3 := lmin2;
                lmin2 := lmin1;
                lmin1 := sum;
                b := k;
            end
            else if sum <= lmin2 then begin
                lmin3 := lmin2;
                lmin2 := sum;
            end
            else if sum < lmin3 then
                lmin3 := sum;
            RemoveBranch(Node[j]);
        end;
        if lmin1 < min1 then begin
            min1 := lmin1;
            min2 := lmin2;
            min3 := lmin3;
            n := j;
            bmin := b;  // b might be uninitialized
        end
    else if lmin1 = min1 then
            if lmin2 > min2 then begin
                min2 := lmin2;
                min3 := lmin3;
                n := j;
                bmin := b;
            end
            else if lmin2 = min2 then
                if lmin3 > min3 then begin
                    min3 := lmin3;
                    n := j;
                    bmin := b;
                end;
    end;
    if n <> i then begin  // variable n might be uninitialized
        p := Node[i];
        Node[i] := Node[n];
        Node[n] := p;
    end;
    AddBranch(Node[NoOfOTUs+i-2], Node[i], Node[bmin]); // bmin may be uninit
  end;
  sum := CountTreeLength;
  Result := sum;
end;

procedure TParsimSearchThread.DoOnProgress;
begin
  FProgressDlg.UpdateRunStatusInfo('Trees Examined',IntToStr(NoOfTreesExamined));
  FProgressDlg.UpdateRunStatusInfo('Parsimony Trees',IntToStr(NoOfTrees));
  FProgressDlg.UpdateRunStatusInfo('Tree Length',IntToStr(TreeLength));
  inherited;
end;

///////////////////////////
// TBranchBoundSearchThread
///////////////////////////

procedure TBranchBoundSearchThread.Search;
var Progress: integer;

    procedure BranchBound(n : integer);
    var i : integer;
    begin
        Inc(searchIterations);
        if (searchIterations mod 10000) = 0 then
          OnProgress((Progress*100) div 105);
        for i := 1 to n-1 do begin
            AddBranch(Node[NoOfOTUs+n-2], Node[n], Node[i]);
            CountTreeLength;
            if n = NoOfOTUs-1 then begin
                if Node[2*NoOfOTUs-2].length < Bound then
                    RenewParsimonyTree
                else if Node[2*NoOfOTUs-2].length = Bound then
                    SaveParsimonyTree;
                Inc(FNoOfTreesExamined);
            end
            else if Node[2*NoOfOTUs-2].length <= (Bound -SCD[n]) then
                BranchBound(n+1);
            RemoveBranch(Node[n]);
            if n = 5 then begin
                Inc(Progress);
                OnProgress((Progress*100) div 105);
            end;
            if Terminated then Exit;
        end;
        for i := NoOfOTUs to NoOfOTUs+n-3 do begin
            AddBranch(Node[NoOfOTUs+n-2], Node[n], Node[i]);
            CountTreeLength;
            if n = NoOfOTUs-1 then begin
                if Node[2*NoOfOTUs-2].length < Bound then
                    RenewParsimonyTree
                else if Node[2*NoOfOTUs-2].length = Bound then
                    SaveParsimonyTree;
                Inc(FNoOfTreesExamined);
            end
            else if Node[2*NoOfOTUs-2].length <= (Bound -SCD[n]) then
                BranchBound(n+1);
            RemoveBranch(Node[n]);
            if n = 5 then begin
                Inc(FProgress);
                OnProgress((Progress*100) div 105);
            end;
            if Terminated then Exit;
        end;
    end;

    procedure SortSequence;
    VAR i,j,k,n,b,bmax,sum,lmin1,lmin2,lmin3,max1,max2,max3 : integer;
        p : TpParsNode;
    begin
      bmax := -1; //@SK mega2b4
      n := -1; //@SK mega2b4
      b := -1; //@SK mega2b4
      for i := 3 to NoOfOTUs-1 do
      begin
        max1 := 0;
        max2 := 0;
        max3 := 0;
        for j := i to NoOfOTUs-1 do begin
            lmin1 := NoOfOTUs*NoOfInfoSites;
            lmin2 := NoOfOTUs*NoOfInfoSites;
            lmin3 := NoOfOTUs*NoOfInfoSites;
            for k := 1 to i-1 do begin
                AddBranch(Node[NoOfOTUs+j-2], Node[j], Node[k]);
                sum := CountTreeLength;
                if sum <= lmin1 then begin
                    lmin3 := lmin2;
                    lmin2 := lmin1;
                    lmin1 := sum;
                    b := k;
                end
                else if sum <= lmin2 then begin
                    lmin3 := lmin2;
                    lmin2 := sum;
                end
                else if sum < lmin3 then
                    lmin3 := sum;
                RemoveBranch(Node[j]);
            end;
            for k := NoOfOTUs to NoOfOTUs+i-3 do begin
                AddBranch(Node[NoOfOTUs+j-2], Node[j], Node[k]);
                sum := CountTreeLength;
                if sum <= lmin1 then begin
                    lmin3 := lmin2;
                    lmin2 := lmin1;
                    lmin1 := sum;
                    b := k;
                end
                else if sum <= lmin2 then begin
                    lmin3 := lmin2;
                    lmin2 := sum;
                end
                else if sum < lmin3 then
                    lmin3 := sum;
                RemoveBranch(Node[j]);
            end;
            if lmin1 > max1 then begin
                max1 := lmin1;
                max2 := lmin2;
                max3 := lmin3;
                n := j;
                bmax := b;
            end
            else if lmin1 = max1 then
                if lmin2 > max2 then begin
                    max2 := lmin2;
                    max3 := lmin3;
                    n := j;
                    bmax := b;
                end
                else if lmin2 = max2 then
                    if lmin3 > max3 then begin
                        max3 := lmin3;
                        n := j;
                        bmax := b;
                    end;
        end;
        if n <> i then begin
            p := Node[i];
            Node[i] := Node[n];
            Node[n] := p;
        end;
        AddBranch(Node[NoOfOTUs+i-2], Node[i], Node[bmax]);
      end;
    end;
begin
    FNoOfTreesExamined := 0;
    SelectFurthestSequence;

    InitTree;
    Bound := MakeInitialTree;
//    FValue := Bound+UniqNum;
    OnProgress(0);

    InitTree;
    SortSequence;
    SetSCD;

    InitTree;
    Progress := 0;
    BranchBound(3);
    OnProgress(100);
end;

//////////////////////////////
// TBootstrapBranchBoundThread
//////////////////////////////

constructor TBootstrapBranchBoundSearchThread.Create(Tree: TTreeList;
                                               OriSeqs: TList;
                                               NoOfSites: integer;
                                               NoOfBits: integer;
                                               NoOfBootReps: integer;
                                               MaxPercentRam: Integer = DEFAULT_MAX_PERCENT_RAM);
var
  mem: Int64 = 0;
begin
  inherited Create(Tree, OriSeqs, NoOfSites, NoOfBits);
  FreeOnTerminate := True;
  NoOfBootstrappings := NoOfBootReps;
  FOriSeqs := OriSeqs;
  FNoOfBits := NoOfBits;
  FCurrentRep := 0;
  LockThread := TCriticalSection.Create;
  GetAvailablePhysicalMemory(mem);
  mem := trunc(mem*MaxPercentRam/100);
  FThreadMemSize := EstimateThreadMemSize;
  FNoOfThreads := Max(1, GetNoOfProcessors-1);
  if FNoOfThreads*FThreadMemSize > mem then
    FNoOfThreads :=  trunc(mem/FThreadMemSize);
  setlength(FChildThread, FNoOfThreads);
  {$IFDEF VISUAL_BUILD}
  if FNoOfThreads > 1 then
    MegaForm.MultithreadedCalcIsRunning := True;
  {$ENDIF}
end;

destructor TBootstrapBranchBoundSearchThread.Destroy;
begin
  if Assigned(LockThread) then
    LockThread.Free;
  inherited Destroy;
end;

procedure TBootstrapBranchBoundSearchThread.AddTreeList(aTreeList: TTreeList);
begin
  try
    LockThread.Acquire;
    aTreeList.TotalFrequency := 1.0;
    BootstrapTrees.AddTreeList(aTreeList, False);
  finally
    LockThread.Release;
  end;
end;

function TBootstrapBranchBoundSearchThread.GetCurrentRep: Integer;
begin
  Result := -1;
  try
    LockThread.Acquire;
    inc(FCurrentRep);
    if FCurrentRep <= NoOfBootstrappings then
    begin
      Result := FCurrentRep;
      if Terminated then
        Result := -1
      else
        Synchronize(DoOnBootstrapProgress);
    end;
  finally
    LockThread.Release;
  end;
end;

function TBootstrapBranchBoundSearchThread.EstimateThreadMemSize: Int64;
begin
  { TODO 1 -oGlen -cparallelization : implement TBootstrapBranchBoundSearchThread.EstimateThreadMemSize }
  Result := 100000;
end;

function TBootstrapBranchBoundSearchThread.CheckChildThreadDone: Boolean;
var
  i: integer;
begin
  Result := True;
  if FNoOfThreads > 1 then
    for i := 1 to FNoOfThreads-1 do
      Result := Result and FChildThread[i].Done;
end;

procedure TBootstrapBranchBoundSearchThread.DoOnBootstrapProgress;
begin
  FProgressDlg.Progress := Trunc(Min(FCurrentRep, NoOfBootstrappings)/NoOfBootstrappings*100);
  {$IFDEF VISUAL_BUILD}
  FProgressDlg.UpdateRunStatusInfo('Bootstrap Rep #', Format('%d of %d', [Min(FCurrentRep, NoOfBootstrappings), NoOfBootstrappings]));
  FProgressDlg.Refresh;
  {$ENDIF}
end;

procedure TBootstrapBranchBoundSearchThread.Search;
var
  OriTreeList: TTreeList;
  i: integer;
  AllThreadsDone: Boolean;
begin
  Initialize;
  InitialSearch;
  ShowProgress := false;
  OriTreeList := TTreeList.Create;
  while ParsimTrees.NoOfTrees > 0 do
    OriTreeList.Add(ParsimTrees.Remove(0));

  try
    if FNoOfThreads > 1 then
      for i := 1 to FNoOfThreads-1 do
        FChildThread[i].Start;

    inherited Search;

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
    if FNoOfThreads > 1 then
      for i := 1 to FNoOfThreads - 1 do
        FChildThread[i].Free;
    FProgress := 99;
    Synchronize(DoOnBootstrapProgress);
    BootstrapTrees.BuildPartitionsSummary(Min(FCurrentRep, NoOfBootstrappings));
    BootstrapTrees.SortTreeByFreq;
    while OriTreeList.NoOfTrees > 0 do
      ParsimTrees.Add(OriTreeList.Remove(0));
    ParsimTrees.isStats := true;
    if ParsimTrees.NoOfTrees > 0 then
      for i := 0 to ParsimTrees.NoOfTrees-1 do
        BootstrapTrees.Compare(ParsimTrees[i].NodeArray, @ParsimTrees[i].StatsArray[ParsimTrees.NoOfOTUs]);
    OriTreeList.Free;
    FreememAndNil(FreqTable);
  end;
end;

procedure TBootstrapBranchBoundSearchThread.InitialSearch;
begin
  inherited InitialSearch;
end;

procedure TBootstrapBranchBoundSearchThread.Initialize;
var
  i: Integer;
begin
  FThreadIndex := 0;
  FChildThread[0] := self;
  FMainThread := Self;
  if FNoOfThreads > 1 then
    for i := 1 to FNoOfThreads-1 do
    begin
      FChildThread[i] := TBranchBoundChildSearchThread.Create(nil, FOriSeqs, NoOfTotalSites, FNoOfBits);
      FChildThread[i].MainThread := Self;
      FChildThread[i].ThreadIndex := i;
    end;
end;

///////////////////
// TCNISearchThread
///////////////////

constructor TCNISearchThread.Create(Tree: TTreeList;
                                    OriSeqs: TList;
                                    NoOfSites: integer;
                                    NoOfBits: integer;
                                    SearchLevel: integer);
begin
  inherited Create(Tree, OriSeqs, NoOfSites, NoOfBits);
  Dt := 2*SearchLevel;        //Changed by MS 9-15-2010 added 2*searchLevel because later on we seem to refer to the search level in an if then where we test >= 2, >=4, >= 6.  Also there was a comment by Tamura in the code for this function saying ot use 2, 4 or 6.
end;


function TCNISearchThread.CompareDt2Trees(n: TpParsNode):boolean;
var p : array[0..5] of TpParsNode;
    i : integer;
begin
    Result := false;
    if n.anc.anc = nil then Exit;
    p[0] := n.anc.anc;
    p[1] := n.des1;
    p[2] := n.des2;
    p[3] := n.sib;
    p[4] := n.anc;
    p[5] := n;

    RemoveBranch(p[3]);
    for i := 1 to 2 do begin
        AddBranch(p[4], p[3], p[i]);
        CountTreeLength;
        Inc(FNoOfTreesExamined);
        if Node[2*NoOfOTUs-2].length = Bound then
            SaveParsimonyTree
        else if Node[2*NoOfOTUs-2].length < Bound then begin
            RenewParsimonyTree;
            Result := true;
            Exit;
        end;
        RemoveBranch(p[3]);
    end;
    AddBranch(p[4], p[3], p[5]);
end;

function TCNISearchThread.CompareDt4Trees(n: TpParsNode):boolean;
var p : array[0..9] of TpParsNode;
    i,j : integer;
begin
    Result := false;
    if (n.des1.des1 <> nil) and(n.des2.des1 <> nil) then begin
        p[0] := n.anc;
        p[1] := n.des1.des1;
        p[2] := n.des1.des2;
        p[3] := n.des1;
        p[4] := n;
        p[5] := n.des2;
        p[6] := n.des2.des1;
        p[7] := n.des2.des2;

        RemoveBranch(p[6]);
        RemoveBranch(p[7]);

        for i := 1 to 2 do begin
            AddBranch(p[4], p[6], p[i]);
            for j := 1 to 4 do begin
                AddBranch(p[5], p[7], p[j]);
                CountTreeLength;
                Inc(FNoOfTreesExamined);
                if Node[2*NoOfOTUs-2].length = Bound then
                    SaveParsimonyTree
                else if Node[2*NoOfOTUs-2].length < Bound then begin
                    RenewParsimonyTree;
                    Result := true;
                    Exit;
                end;
                RemoveBranch(p[7]);
            end;
            RemoveBranch(p[6]);
        end;
        AddBranch(p[4], p[6], p[3]);
        for j := 1 to 2 do begin
            AddBranch(p[5], p[7], p[j]);
                CountTreeLength;
                Inc(FNoOfTreesExamined);
                if Node[2*NoOfOTUs-2].length = Bound then
                    SaveParsimonyTree
                else if Node[2*NoOfOTUs-2].length < Bound then begin
                    RenewParsimonyTree;
                    Result := true;
                    Exit;
                end;
            RemoveBranch(p[7]);
        end;
        RemoveBranch(p[6]);

        AddBranch(p[4], p[6], p[3]);
        AddBranch(p[5], p[7], p[6]);
    end;

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

    RemoveBranch(p[3]);
    RemoveBranch(p[6]);

    for i := 1 to 2 do begin
        AddBranch(p[4], p[3], p[i]);
        for j := 1 to 4 do begin
            AddBranch(p[7], p[6], p[j]);
            CountTreeLength;
            Inc(FNoOfTreesExamined);
            if Node[2*NoOfOTUs-2].length = Bound then
                SaveParsimonyTree
            else if Node[2*NoOfOTUs-2].length < Bound then begin
                RenewParsimonyTree;
                Result := true;
                Exit;
            end;
            RemoveBranch(p[6]);
        end;
        RemoveBranch(p[3]);
    end;
    AddBranch(p[4], p[3], p[5]);
    for j := 1 to 2 do begin
        AddBranch(p[7], p[6], p[j]);
        CountTreeLength;
        Inc(FNoOfTreesExamined);
        if Node[2*NoOfOTUs-2].length = Bound then
            SaveParsimonyTree
        else if Node[2*NoOfOTUs-2].length < Bound then begin
            RenewParsimonyTree;
            Result := true;
            Exit;
        end;
        RemoveBranch(p[6]);
    end;
    RemoveBranch(p[3]);

    AddBranch(p[4], p[3], p[5]);
    AddBranch(p[7], p[6], p[4]);

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

        RemoveBranch(p[3]);
        RemoveBranch(p[8]);

        for i := 1 to 2 do begin
            AddBranch(p[4], p[3], p[i]);
            for j := 5 to 6 do begin
                AddBranch(p[9], p[8], p[j]);
                CountTreeLength;
                Inc(FNoOfTreesExamined);
                if Node[2*NoOfOTUs-2].length = Bound then
                    SaveParsimonyTree
                else if Node[2*NoOfOTUs-2].length < Bound then begin
                    RenewParsimonyTree;
                    Result := true;
                    Exit;
                end;
                RemoveBranch(p[8]);
            end;
            RemoveBranch(p[3]);
        end;

        AddBranch(p[4], p[3], p[5]);
        AddBranch(p[9], p[8], p[7]);
    end;
    if n.anc.sib.des1 <> nil then begin
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

        RemoveBranch(p[3]);
        RemoveBranch(p[5]);

        for i := 6 to 7 do begin
            AddBranch(p[8], p[5], p[i]);
            for j := 1 to 2 do begin
                AddBranch(p[4], p[3], p[j]);
                CountTreeLength;
                Inc(FNoOfTreesExamined);
                if Node[2*NoOfOTUs-2].length = Bound then
                    SaveParsimonyTree
                else if Node[2*NoOfOTUs-2].length < Bound then begin
                    RenewParsimonyTree;
                    Result := true;
                    Exit;
                end;
                RemoveBranch(p[3]);
            end;
            RemoveBranch(p[5]);
        end;

        AddBranch(p[8], p[5], p[9]);
        AddBranch(p[4], p[3], p[5]);
    end;
end;

function TCNISearchThread.CompareDt6Trees(n: TpParsNode):boolean;
var p : array[0..9] of TpParsNode;
    i,j,k : integer;
begin
    Result := false;
    if n.anc.anc = nil then Exit;

    if (n.anc.anc.anc <> nil) and (n.anc.anc.anc.anc <> nil) then begin
        p[0] := n.anc.anc.anc.anc;
        p[1] := n.des1;
        p[2] := n.des2;
        p[3] := n;
        p[4] := n.anc.anc.sib;
        p[5] := n.anc.anc.anc;
        p[6] := n.anc.sib;
        p[7] := n.anc.anc;
        p[8] := n.sib;
        p[9] := n.anc;

        RemoveBranch(p[4]);
        RemoveBranch(p[6]);
        RemoveBranch(p[8]);

        for i := 1 to 2 do begin
            AddBranch(p[5], p[4], p[i]);
            for j := 1 to 5 do begin
                AddBranch(p[7], p[6], p[j]);
                for k := 1 to 7 do begin
                    AddBranch(p[9], p[8], p[k]);
                    CountTreeLength;
                    Inc(FNoOfTreesExamined); //@SK
                    if Node[2*NoOfOTUs-2].length = Bound then
                        SaveParsimonyTree
                    else if Node[2*NoOfOTUs-2].length < Bound then begin
                        RenewParsimonyTree;
                        Result := true;
                        Exit;
                    end;
                    RemoveBranch(p[8]);
                end;
                RemoveBranch(p[6]);
            end;
            RemoveBranch(p[4]);
        end;
        AddBranch(p[5], p[4], p[3]);
        for j := 1 to 2 do begin
            AddBranch(p[7], p[6], p[j]);
            for k := 4 to 5 do begin
                AddBranch(p[9], p[8], p[k]);
                CountTreeLength;
                Inc(FNoOfTreesExamined); //@SK
                if Node[2*NoOfOTUs-2].length = Bound then
                    SaveParsimonyTree
                else if Node[2*NoOfOTUs-2].length < Bound then begin
                    RenewParsimonyTree;
                    Result := true;
                    Exit;
                end;
                RemoveBranch(p[8]);
            end;
            RemoveBranch(p[6]);
        end;
        AddBranch(p[7], p[6], p[3]);
        AddBranch(p[9], p[8], p[3]);
    end;

    if (n.anc.anc.anc <> nil) and(n.anc.sib.des1 <> nil) then begin
        p[0] := n.anc.anc.anc;
        p[1] := n.des1;
        p[2] := n.des2;
        p[3] := n;
        p[4] := n.anc.anc;
        p[5] := n.anc.sib.des1;
        p[6] := n.anc.sib.des2;
        p[7] := n.anc.sib;
        p[8] := n.sib;
        p[9] := n.anc;

        RemoveBranch(p[5]);
        RemoveBranch(p[6]);
        RemoveBranch(p[8]);

        for i := 1 to 2 do begin
            AddBranch(p[4], p[5], p[i]);
            for j := 1 to 4 do begin
                AddBranch(p[7], p[6], p[j]);
                for k := 1 to 7 do begin
                    AddBranch(p[9], p[8], p[k]);
                    CountTreeLength;
                    Inc(FNoOfTreesExamined); //@SK
                    if Node[2*NoOfOTUs-2].length = Bound then
                        SaveParsimonyTree
                    else if Node[2*NoOfOTUs-2].length < Bound then begin
                        RenewParsimonyTree;
                        Result := true;
                        Exit;
                    end;
                    RemoveBranch(p[8]);
                end;
                RemoveBranch(p[6]);
            end;
            RemoveBranch(p[5]);
        end;
        AddBranch(p[4], p[5], p[3]);
        for j := 1 to 2 do begin
            AddBranch(p[7], p[6], p[j]);
            for k := 1 to 7 do begin
                AddBranch(p[9], p[8], p[k]);
                CountTreeLength;
                Inc(FNoOfTreesExamined); //@SK
                if Node[2*NoOfOTUs-2].length = Bound then
                    SaveParsimonyTree
                else if Node[2*NoOfOTUs-2].length < Bound then begin
                    RenewParsimonyTree;
                    Result := true;
                    Exit;
                end;
                RemoveBranch(p[8]);
            end;
            RemoveBranch(p[6]);
        end;
        RemoveBranch(p[5]);
        for i := 1 to 2 do begin
            AddBranch(p[4], p[5], p[i]);
            AddBranch(p[7], p[6], p[5]);
            for k := 5 to 6 do begin
                AddBranch(p[9], p[8], p[k]);
                CountTreeLength;
                Inc(FNoOfTreesExamined); //@SK
                if Node[2*NoOfOTUs-2].length = Bound then
                    SaveParsimonyTree
                else if Node[2*NoOfOTUs-2].length < Bound then begin
                    RenewParsimonyTree;
                    Result := true;
                    Exit;
                end;
                RemoveBranch(p[8]);
            end;
            RemoveBranch(p[6]);
            RemoveBranch(p[5]);
        end;

        AddBranch(p[4], p[5], p[3]);
        AddBranch(p[7], p[6], p[5]);
        AddBranch(p[9], p[8], p[3]);
    end;

    if (n.des1.des1 <> nil) and (n.des2.des1 <> nil) then begin
        p[0] := n.anc.anc;
        p[1] := n.des1.des1;
        p[2] := n.des1.des2;
        p[3] := n.des1;
        p[4] := n.sib;
        p[5] := n.anc;
        p[6] := n;
        p[7] := n.des2.des1;
        p[8] := n.des2.des2;
        p[9] := n.des2;

        RemoveBranch(p[4]);
        RemoveBranch(p[7]);
        RemoveBranch(p[8]);

        for i := 1 to 2 do begin
            AddBranch(p[5], p[4], p[i]);
            for j := 1 to 5 do begin
                AddBranch(p[6], p[7], p[j]);
                for k := 1 to 6 do begin
                    AddBranch(p[9], p[8], p[k]);
                    CountTreeLength;
                    Inc(FNoOfTreesExamined); //@SK
                    if Node[2*NoOfOTUs-2].length = Bound then
                        SaveParsimonyTree
                    else if Node[2*NoOfOTUs-2].length < Bound then begin
                        RenewParsimonyTree;
                        Result := true;
                        Exit;
                    end;
                    RemoveBranch(p[8]);
                end;
                RemoveBranch(p[7]);
            end;
            RemoveBranch(p[4]);
        end;

        AddBranch(p[5], p[4], p[3]);
        for j := 1 to 2 do begin
            AddBranch(p[6], p[7], p[j]);
            for k := 4 to 5 do begin
                AddBranch(p[9], p[8], p[k]);
                CountTreeLength;
                Inc(FNoOfTreesExamined); //@SK
                if Node[2*NoOfOTUs-2].length = Bound then
                    SaveParsimonyTree
                else if Node[2*NoOfOTUs-2].length < Bound then begin
                    RenewParsimonyTree;
                    Result := true;
                    Exit;
                end;
                RemoveBranch(p[8]);
            end;
            RemoveBranch(p[7]);
        end;
        for j := 4 to 5 do begin
            AddBranch(p[6], p[7], p[j]);
            for k := 1 to 2 do begin
                AddBranch(p[9], p[8], p[k]);
                CountTreeLength;
                Inc(FNoOfTreesExamined); //@SK
                if Node[2*NoOfOTUs-2].length = Bound then
                    SaveParsimonyTree
                else if Node[2*NoOfOTUs-2].length < Bound then begin
                    RenewParsimonyTree;
                    Result := true;
                    Exit;
                end;
                RemoveBranch(p[8]);
            end;
            RemoveBranch(p[7]);
        end;
        AddBranch(p[6], p[7], p[3]);
        AddBranch(p[9], p[8], p[7]);
    end;
end;

procedure TCNISearchThread.Search;
var
  i, j: integer;
  flag: boolean;
begin
  FNoOfTreesExamined := 0;
  PartitionList := TPartitionList.Create(NoOfOTUs, 1000, false);
  InitialTrees := TTreeList.Create;
  while ParsimTrees.NoOfTrees > 0 do
    InitialTrees.Add(ParsimTrees.Remove(0));

  for i := 0 to InitialTrees.NoOfTrees-1 do begin
    LoadTreeData(InitialTrees[i]);
    if i = 0 then begin
      Bound := CountTreeLength;
      SaveParsimonyTree;
    end
    else if CountTreeLength < Bound then
      RenewParsimonyTree
    else if CountTreeLength = Bound then
      SaveParsimonyTree;
    Inc(FNoOfTreesExamined);

    repeat
      flag := false;
      for j := NoOfOTUs to 2*NoOfOTUs-3 do begin
        if Dt >= 2 then
          if CompareDt2Trees(Node[j]) then flag := true;
        if (not flag) and (Dt >= 4) then
          if CompareDt4Trees(Node[j]) then flag := true;
        if (not flag) and (Dt >= 6) then
          if CompareDt6Trees(Node[j]) then flag := true;

        if flag then begin
          PartitionList.Reset;
          Break;
        end;
        if Terminated then Break;

        OnProgress(Trunc(((j-NoOfOTUs)/(NoOfOTUs-3)+i)/InitialTrees.NoOfTrees*100));
      end;
{
      if Dt >= 2 then
        for j := NoOfOTUs to 2*NoOfOTUs-3 do begin
          if CompareDt2Trees(Node[j]) then begin
            flag := true;
            Break;
          end;
          if Terminated then Break;
        end;
      if Terminated then Break;
      if (not flag) and (Dt >= 4) then
        for j := NoOfOTUs to 2*NoOfOTUs-3 do begin
          if CompareDt4Trees(Node[j]) then begin
           flag := true;
           Break;
          end;
          if Terminated then Break;
        end;
      if Terminated then Break;
      if (not flag) and (Dt >= 6) then
        for j := NoOfOTUs to 2*NoOfOTUs-3 do begin
          if CompareDt6Trees(Node[j]) then begin
            flag := true;
            Break;
          end;
          if Terminated then Break;
        end;
      if Terminated then Break;
      if flag then
        PartitionList.Reset;
 }
    until not flag;

    if Terminated then Break;
    if 0 < ParsimTrees.NoOfTrees then
    begin
      PartitionList.AddTreeList(ParsimTrees, true);
      ParsimTrees.DeleteAll;
      PartitionList.BuildPartitionsSummary(1);
//      OnProgress((i*100) div InitialTrees.NoOfTrees);
    end;
  end;

  PartitionList.ExtractTreeList(ParsimTrees);
  ParsimTrees.isStats := false;
  for i := 0 to ParsimTrees.NoOfTrees-1 do
    ParsimTrees[i].Freq := 1.0;

  InitialTrees.Free;
  PartitionList.Free;
end;

//////////////////////
// TRBA_CNISearchThread
//////////////////////

constructor TRBA_CNISearchThread.Create(Tree: TTreeList;
                                       OriSeqs: TList;
                                       NoOfSites: integer;
                                       NoOfBits: integer;
                                       SearchLevel: integer;
                                       NoOfReplications: integer);
begin
  inherited Create(Tree, OriSeqs, NoOfSites, NoOfBits, SearchLevel);
  FNoOfReps := NoOfReplications;
  FFixedOrder := false;
end;

function TRBA_CNISearchThread.MakeRandomAdditionTree:integer;
var i,j,k,n,b,sum,min: integer;
    p : TpParsNode;
    h : array of TpParsNode;
begin
  b := -1; //@SK mega2b4

  if FixedOrder then
  begin
    setlength(h, NoOfOTUs);
    for i := 0 to NoOfOTUs-1 do
      h[i] := Node[i];

    n := NoOfOTUs div NoOfReps;

    i := 0;
    j := Rep;
    k := 0;
    repeat
      Node[k] := h[i+j];
      inc(i, n);
      if (i+j) >= NoOfOTUs then
      begin
        i := 0;
        inc(j);
        if j >= n then
          j := 0;
      end;
      inc(k);
    until k = NoOfOTUs;
  end
  else
    for i := NoOfOTUs-1 downto 1 do
    begin
      j := Random(i+1);
      p := Node[i];
      Node[i] := Node[j];
      Node[j] := p;
    end;

  InitTree;

  for i := 3 to NoOfOTUs-1 do begin
      min := NoOfOTUs*NoOfInfoSites;
      for j := 1 to i-1 do begin
          AddBranch(Node[NoOfOTUs+i-2], Node[i], Node[j]);
          sum := CountTreeLength;
          if sum < min then begin
              min := sum;
              b := j;
          end;
          RemoveBranch(Node[i]);
      end;
      for j := NoOfOTUs to NoOfOTUs+i-3 do begin
          AddBranch(Node[NoOfOTUs+i-2], Node[i], Node[j]);
          sum := CountTreeLength;
          if sum < min then begin
              min := sum;
              b := j;
          end;
          RemoveBranch(Node[i]);
      end;
      AddBranch(Node[NoOfOTUs+i-2], Node[i], Node[b]);
  end;
  sum := CountTreeLength;
  Result := sum;

  if FixedOrder then
    for i := 0 to NoOfOTUs-1 do
      Node[i] := h[i];
end;

function TRBA_CNISearchThread.GetNoOfTrees:integer;
begin
  if PartitionList = nil then
    result := 0
  else
    result := PartitionList.NoOfTrees;
end;

procedure TRBA_CNISearchThread.Search;
var
  tmpPartitionList: TPartitionList;
  i: integer;
begin
  ParsimTrees.DeleteAll;
//  FProgressDlg.UpdateRunStatusInfo('Status', 'Searching Initial Trees');
  for i := 1 to NoOfReps do
  begin
    Rep := i-1;
    Bound := MakeRandomAdditionTree;
    if (ParsimTrees.Count = 0) then
      SaveParsimonyTree
    else if TreeLength < round(ParsimTrees[0].Value) then
      RenewParsimonyTree;
    FNoOfTreesExamined := i;
  end;

  tmpPartitionList := TPartitionList.Create(NoOfOTUs, NoOfReps, false);
  tmpPartitionList.AddTreeList(ParsimTrees, true);
  tmpPartitionList.ExtractTreeList(ParsimTrees);
  tmpPartitionList.Free;
//  FProgressDlg.UpdateRunStatusInfo('Status', 'CNI Searching');

  if Dt > 0 then
    inherited;
end;

////////////////////////////////
// TBootstrapRBA_CNISearchThread
////////////////////////////////

constructor TBootstrapRBA_CNISearchThread.Create(Tree: TTreeList;
                                                 OriSeqs: TList;
                                                 NoOfSites: integer;
                                                 NoOfBits: integer;
                                                 SearchLevel: integer;
                                                 NoOfReplications: integer;
                                                 NoOfBootReps: integer);
begin
  inherited Create(Tree, OriSeqs, NoOfSites, NoOfBits, SearchLevel, NoOfReplications);
  NoOfBootstrappings := NoOfBootReps;
end;

procedure TBootstrapRBA_CNISearchThread.DoOnBootstrapProgress;
begin
  FProgressDlg.Progress := Trunc(FProgress/NoOfBootstrappings*100);
  FProgressDlg.UpdateRunStatusInfo('Bootstraps Completed', IntToStr(FProgress) + ' / ' + IntToStr(NoOfBootstrappings));
  FProgressDlg.UpdateRunStatusInfo('Trees Examined', IntToStr(ParsimTrees.NoOfTrees));
  {$IFDEF VISUAL_BUILD}
  FProgressDlg.Refresh;
  {$ENDIF}
end;

procedure TBootstrapRBA_CNISearchThread.Search;
var OriTreeList: TTreeList;
    i: integer;
begin
  ShowProgress := false;
  inherited;

  GetMem(FreqTable, SizeOf(Integer)*NoOfInfoSites);

  OriTreeList := TTreeList.Create;
  while ParsimTrees.NoOfTrees > 0 do
    OriTreeList.Add(ParsimTrees.Remove(0));

  for i := 0 to NoOfBootstrappings-1 do begin
    BootStrap(FreqTable, NoOfInfoSites, NoOfBootstrappings);
    inherited;
    if Terminated then Break;

    ParsimTrees.TotalFrequency := 1.0;
    BootstrapTrees.AddTreeList(ParsimTrees, false);
    ParsimTrees.DeleteAll;

    FProgress := i;
    if FProgressDlg <> nil then
      Synchronize(DoOnBootstrapProgress);
  end;
  BootstrapTrees.BuildPartitionsSummary(NoOfBootstrappings);
  BootstrapTrees.SortTreeByFreq;

  while OriTreeList.NoOfTrees > 0 do
    ParsimTrees.Add(OriTreeList.Remove(0));

  ParsimTrees.IsStats := true;
  if ParsimTrees.NoOfTrees > 0 then
    for i := 0 to ParsimTrees.NoOfTrees-1 do
      BootstrapTrees.Compare(ParsimTrees[i].NodeArray, @ParsimTrees[i].StatsArray[ParsimTrees.NoOfOTUs]);

  OriTreeList.Free;
  FreeMemAndNil(FreqTable);
end;

///////////////////////////////////
// TMiniMini_CNISearchThread
///////////////////////////////////

constructor TMiniMini_CNISearchThread.Create(Tree: TTreeList;
                       OriSeqs: TList;
                       NoOfSites: integer;
                       NoOfBits: integer;
                       SearchLevel: integer;
                       SearchFactor: integer);
begin
  inherited Create(Tree, OriSeqs, NoOfSites, NoOfBits, SearchLevel);
  FSearchFactor := SearchFactor;
end;

function TMiniMini_CNISearchThread.GetNoOfTrees:integer;
begin
  if PartitionList = nil then
    result := ParsimTrees.NoOfTrees
  else
    result := PartitionList.NoOfTrees;
end;

procedure TMiniMini_CNISearchThread.Search;
var Progress: integer;

    procedure CorrectLocalBound;
    var i: integer;
    begin
        for i := 3 to NoOfOTUs-1 do
            if LocalBound[i] > (Bound -SCD[i]) then
                LocalBound[i] := (Bound -SCD[i]);
    end;

    procedure MiniMiniSearch(n : integer);
    var i : integer;
    begin
        Inc(searchIterations);
        if (searchIterations mod 10000) = 0 then
          OnProgress(FProgress);
        for i := 1 to n-1 do begin
            AddBranch(Node[NoOfOTUs+n-2], Node[n], Node[i]);
            CountTreeLength;
            if n = NoOfOTUs-1 then begin
                if Node[2*NoOfOTUs-2].length < Bound then begin
                    Bound := Node[2*NoOfOTUs-2].length;
                    CorrectLocalBound;
                    RenewParsimonyTree;
                end
                else if (Node[2*NoOfOTUs-2].length = Bound)then
                    SaveParsimonyTree;
                Inc(FNoOfTreesExamined);
            end
            else if Node[2*NoOfOTUs-2].length <= LocalBound[n] then
                MiniMiniSearch(n+1);
            RemoveBranch(Node[n]);
            if n = 5 then begin
                Inc(Progress);
                OnProgress((Progress*100) div 105);
            end;
            if Terminated then Exit;
        end;
        for i := NoOfOTUs to NoOfOTUs+n-3 do begin
            AddBranch(Node[NoOfOTUs+n-2], Node[n], Node[i]);
            CountTreeLength;
            if n = NoOfOTUs-1 then begin
                if Node[2*NoOfOTUs-2].length < Bound then begin
                    Bound := Node[2*NoOfOTUs-2].length;
                    CorrectLocalBound;
                    RenewParsimonyTree;
                end
                else if Node[2*NoOfOTUs-2].length = Bound then
                    SaveParsimonyTree;
                Inc(FNoOfTreesExamined); 
            end
            else if Node[2*NoOfOTUs-2].length <= LocalBound[n] then
                MiniMiniSearch(n+1);
            RemoveBranch(Node[n]);
            if n = 5 then begin
                Inc(FProgress);
                OnProgress((Progress*100) div 105);
            end;
            if Terminated then Exit;
        end;
    end;

    function InitBound:integer;
    var i,j,k,n,b,bmin,sum,min1,min2,min3,lmin1,lmin2,lmin3: integer;
        p : TpParsNode;
    begin
      bmin := -1; //@SK mega2b4
      n := -1;   //@SK
      b := -1;   //@SK mega2b4

      for i := 3 to NoOfOTUs-1 do begin
          min1 := NoOfOTUs*NoOfInfoSites;
          min2 := NoOfOTUs*NoOfInfoSites;
          min3 := NoOfOTUs*NoOfInfoSites;
          for j := i to NoOfOTUs-1 do begin
              lmin1 := NoOfOTUs*NoOfInfoSites;
              lmin2 := NoOfOTUs*NoOfInfoSites;
              lmin3 := NoOfOTUs*NoOfInfoSites;
              for k := 1 to i-1 do begin
                  AddBranch(Node[NoOfOTUs+j-2], Node[j], Node[k]);
                  sum := CountTreeLength;
                  if sum <= lmin1 then begin
                      lmin3 := lmin2;
                      lmin2 := lmin1;
                      lmin1 := sum;
                      b := k;
                  end
                  else if sum <= lmin2 then begin
                      lmin3 := lmin2;
                      lmin2 := sum;
                  end
                  else if sum < lmin3 then
                      lmin3 := sum;
                  RemoveBranch(Node[j]);
              end;
              for k := NoOfOTUs to NoOfOTUs+i-3 do begin
                  AddBranch(Node[NoOfOTUs+j-2], Node[j], Node[k]);
                  sum := CountTreeLength;
              if sum <= lmin1 then begin
                      lmin3 := lmin2;
                      lmin2 := lmin1;
                      lmin1 := sum;
                      b := k;
                  end
                  else if sum <= lmin2 then begin
                      lmin3 := lmin2;
                      lmin2 := sum;
                  end
                  else if sum < lmin3 then
                      lmin3 := sum;
                  RemoveBranch(Node[j]);
              end;
              if lmin1 < min1 then begin
                  min1 := lmin1;
                  min2 := lmin2;
                  min3 := lmin3;
                  n := j;
                  bmin := b;
                  LocalBound[i] := min1 +FSearchFactor;
              end
          else if lmin1 = min1 then
                  if lmin2 > min2 then begin
                      min2 := lmin2;
                      min3 := lmin3;
                      n := j;
                      bmin := b;
                  end
                  else if lmin2 = min2 then
                      if lmin3 > min3 then begin
                          min3 := lmin3;
                          n := j;
                          bmin := b;
                      end;
          end;
          if n <> i then begin
              p := Node[i];
              Node[i] := Node[n];
              Node[n] := p;
          end;
          AddBranch(Node[NoOfOTUs+i-2], Node[i], Node[bmin]);
      end;
      sum := CountTreeLength;
      Result := sum;
    end;

begin
    GetMem(LocalBound, SizeOf(Integer)*NoOfOTUs);

//    if Dt > 0 then
//      FProgressDlg.UpdateRunStatusInfo('Status', 'Searching initial trees');

    FNoOfTreesExamined := 0;

    SelectFurthestSequence;

    InitTree;
    Bound := InitBound;
    SetSCD;
    CorrectLocalBound;
    SaveParsimonyTree;

    OnProgress(0);

    InitTree;
    Progress := 0;


    MiniMiniSearch(3);

   // if no more MP tree is found then we have to add the initial tree

    OnProgress(100);


    FreeMemAndNil(LocalBound);

    if Dt > 0 then
    begin
//      FProgressDlg.UpdateRunStatusInfo('Status', 'CNI Searching');
      inherited;
    end;
end;

/////////////////////////////////////
// TBootstrapMiniMini_CNISearchThread
/////////////////////////////////////

constructor TBootstrapMiniMini_CNISearchThread.Create(Tree: TTreeList;
                                                      OriSeqs: TList;
                                                      NoOfSites: integer;
                                                      NoOfBits: integer;
                                                      SearchLevel: integer;
                                                      SearchFactor: integer;
                                                      NoOfBootReps: integer;
                                                      MaxPercentRam: Integer = DEFAULT_MAX_PERCENT_RAM);
var
  mem: Int64 = 0;
begin
  inherited Create(Tree, OriSeqs, NoOfSites, NoOfBits, SearchLevel, SearchFactor);
  NoOfBootstrappings := NoOfBootReps;
  FOriSeqs := OriSeqs;
  FNoOfBits := NoOfBits;
  FSearchLevel := SearchLevel;
  FCurrentRep := 0;
  LockThread := TCriticalSection.Create;
  GetAvailablePhysicalMemory(mem);
  mem := trunc(mem*MaxPercentRam/100);
  FThreadMemSize := EstimateThreadMemSize;
  FNoOfThreads := Max(1, GetNoOfProcessors-1);
  if FNoOfThreads*FThreadMemSize > mem then
    FNoOfThreads :=  trunc(mem/FThreadMemSize);
  setlength(FChildThread, FNoOfThreads);
  {$IFDEF VISUAL_BUILD}
  if FNoOfThreads > 1 then
    MegaForm.MultithreadedCalcIsRunning := True;
  {$ENDIF}
  FreeOnTerminate := True;
end;

destructor TBootstrapMiniMini_CNISearchThread.Destroy;
begin
  if Assigned(LockThread) then
    LockThread.Free;
  inherited Destroy;
end;

procedure TBootstrapMiniMini_CNISearchThread.AddTreeList(aTreeList: TTreeList);
begin
  try
    LockThread.Acquire;
    aTreeList.TotalFrequency := 1.0;
    BootstrapTrees.AddTreeList(aTreeList, False);
  finally
    LockThread.Release;
  end;
end;

function TBootstrapMiniMini_CNISearchThread.GetCurrentRep: Integer;
begin
  Result := -1;
  try
    LockThread.Acquire;
    inc(FCurrentRep);
    if FCurrentRep <= NoOfBootstrappings then
    begin
      Result := FCurrentRep;
      if Terminated then
        Result := -1
      else
        Synchronize(DoOnBootstrapProgress);
    end;
  finally
    LockThread.Release;
  end;
end;

function TBootstrapMiniMini_CNISearchThread.EstimateThreadMemSize: Int64;
begin
  { TODO 1 -oGlen -cparallelization : implement TBootstrapMiniMini_CNISearchThread.EstimateThreadMemSize }
  Result := 100000;
end;

function TBootstrapMiniMini_CNISearchThread.CheckChildThreadDone: Boolean;
var
  i: integer;
begin
  Result := True;
  if FNoOfThreads > 1 then
    for i := 1 to FNoOfThreads-1 do
      Result := Result and FChildThread[i].Done;
end;

procedure TBootstrapMiniMini_CNISearchThread.DoOnBootstrapProgress;
begin
  FProgressDlg.Progress := Trunc(Min(FCurrentRep, NoOfBootstrappings)/NoOfBootstrappings*100);
  {$IFDEF VISUAL_BUILD}
  FProgressDlg.UpdateRunStatusInfo('Bootstrap Rep #', Format('%d of %d', [Min(FCurrentRep, NoOfBootstrappings), NoOfBootstrappings]));
  FProgressDlg.Refresh;
  {$ENDIF}
end;

procedure TBootstrapMiniMini_CNISearchThread.Search;
var
  OriTreeList: TTreeList;
  i: integer;
  AllThreadsDone: Boolean;
begin
  ShowProgress := false;
  Initialize;
  InitialSearch;
  OriTreeList := TTreeList.Create;
  while ParsimTrees.NoOfTrees > 0 do
    OriTreeList.Add(ParsimTrees.Remove(0));

  try
    if FNoOfThreads > 1 then
      for i := 1 to FNoOfThreads-1 do
        FChildThread[i].Start;

    inherited Search;

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
    if FNoOfThreads > 1 then
      for i := 1 to FNoOfThreads - 1 do
        FChildThread[i].Free;
    FProgress := 99;
    Synchronize(DoOnBootstrapProgress);
    BootstrapTrees.BuildPartitionsSummary(Min(FCurrentRep, NoOfBootstrappings));
    BootstrapTrees.SortTreeByFreq;
    while OriTreeList.NoOfTrees > 0 do
      ParsimTrees.Add(OriTreeList.Remove(0));
    ParsimTrees.isStats := true;
    if ParsimTrees.NoOfTrees > 0 then
      for i := 0 to ParsimTrees.NoOfTrees-1 do
        BootstrapTrees.Compare(ParsimTrees[i].NodeArray, @ParsimTrees[i].StatsArray[ParsimTrees.NoOfOTUs]);
    OriTreeList.Free;
    FreememAndNil(FreqTable);
  end;
end;

procedure TBootstrapMiniMini_CNISearchThread.InitialSearch;
begin
  inherited InitialSearch;
end;

procedure TBootstrapMiniMini_CNISearchThread.Initialize;
var
  i: Integer;
begin
  FThreadIndex := 0;
  FChildThread[0] := self;
  FMainThread := Self;
  if FNoOfThreads > 1 then
    for i := 1 to FNoOfThreads-1 do
    begin
      FChildThread[i] := TBootstrapMiniMini_CNIChildSearchThread.Create(nil, FOriSeqs, NoOfTotalSites, FNoOfBits, FSearchLevel, FSearchFactor);
      FChildThread[i].MainThread := Self;
      FChildThread[i].ThreadIndex := i;
    end;
end;


{ TAnalyzeUserTreeThread }

procedure TAnalyzeUserTreeThread.Search;
begin
  // branch lengths to analyze the user tree, No actual searching takes place.
  CountTreeLength;
end;

end.
