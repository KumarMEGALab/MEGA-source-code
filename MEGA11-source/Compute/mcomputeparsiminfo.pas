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

unit MComputeParsimInfo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

//To do: Take care of polymorphism/Uncertainty in the BuildUpPass fn

uses
  LCLIntf, LCLType, Classes, Forms, Dialogs, Controls, MegaConsts, MTreeData, MDistPack,
  MLongintList;

const
  DEFAULT_PATHS_BEFORE_WARN = 50000000; // 50,000,000

type
  TComputeParsimInfo = Class
  private
    FCurrentSite  : LongInt;
    FNumPathsForPrompt: Integer; // in the case where there are many possible evolutionary paths for a given site, we will give the user the option to skip the site when computing average branch lengths
    FModel        : TDistType;  //Given by the user for Bayesian inference
    FIsNucData    : Boolean;    //Given by caller
    FSeqs         : TList;      //Given by user (CPI owns it)

    // Attributes useful in different functions
    Tree:       array of TPoint; // I use it to simplify code; it is exact as FTreeData
    GivenBLen:  array of Double;

    NoOfTaxa:   Integer;
    NoOfStates: Integer;
    NoOfNodes:  Integer;         // Total nodes in the tree
    MaxNodeId:  Integer;         // Maximum nodeId := NoOfNodes-1;
    RootNode:   Integer;         // index of the root

    // configuration of the currently focused site
    CurSiteConfig: array of LongInt;        // Extant  nuc/aa
    IsConst      : Boolean;
    IsValidConfig: Boolean;
    Freq         : array[0..19] of LongInt; // use for computing informative sites

    // for ancestral state reconstruction
    DownPass     : array of LongInt;
    UpPass       : array of LongInt;
    CurSiteAnc   : array of LongInt; // Ancestors for all internal nodes at a site
    CurBasePos   : array of LongInt; // Base pos at different nodes
    CurPathway   : array of LongInt; // Keeps full configuration for current pathway

    MprBaseLists : array of TLongIntList; // A list of TIntList objects for each node
    MprPathsList : TList;                  // Equivocal MPR pathways are kept here
    NoOfMpPaths  : Integer;
    MpPathCost   : Integer;
    CurPathCost  : Integer;

    // for MP branch length estimation
    CurPathBLen    : array of integer; // Given site; For current Pathway
    AllPathsBLen   : array of integer; // Given site; Over all MPRs
    AllSitesBLen   : array of double;  // All Sites over all MPRs

    // for MP Substitution matrix estimation
    CurPathMat     : array of integer; // Given site; Over cur Pathway
    AllPathsMat    : array of integer; // Given site; Over all MPRs
    AllSitesMat    : array of double;  // All sites over all MPRs

    // used in recursions for EvaluatePathway procedure
    sF, sX, sY, sXY : Integer;
    CurSiteNoOfSteps: Integer;  // used in recursion to find MP steps

    PromptAgain    : boolean;

  private
    procedure ClearMemory;
    procedure ClearMprBaseLists;

    procedure LoadCurSite(ASite: Integer); // fills ASite into CurSiteConfig
    function  CharToMap(AChar: AnsiChar): Integer; // maps a char to integer
    function  MapTochar(AInt: Integer): AnsiChar;

    procedure BuildDownPass(Node: Integer);
    procedure BuildUpPass(Node: Integer);

    procedure UpdateMprBaseLists(JustIndex: Boolean); // if a true map or a true index needed
    function  CalculateEquivocalPathways: Boolean;

    procedure EvaluatePathwaysWithRoot;  // this calls the following
    procedure EvaluatePathway(Node: Integer);
    procedure OutputPathway;

    function  GetBasicInfoForIndices(ASite: Integer; var si, mi, gi: Integer): Boolean;

    function  GetNoOfBases(ABase: Integer): Integer;
    function  GetCurMprBase(ANode: Integer): Integer;
    function  GetBaseIndex(ABase: Integer): Integer;
    function  CheckParsimInfo: Boolean; // uses current site
  public
    BranchLengthsAreComputed: Boolean;
    RefreshCounter: Integer;
    NoOfSites:  Integer;
    property IsNucData : Boolean read FIsNucData;    //Given by caller
    property Seqs      : TList read  FSeqs;      //Given by user (CPI owns it)

    constructor Create;
    destructor  Destroy; override;
    procedure   SetNucData(AValue : Boolean); // set by the caller
    procedure   SetTreeData(const ATree: TTreeData);
    procedure   SetSeqs(AList: TList);
    procedure   SetModel(AModel: TDistType);

    //====== Simple Parsimony functions
      // count of substitutions for each site or over all sites
    function  ComputeTreeLen: LongInt;
    function  GetStepsAt(ASite: Integer): LongInt;
    procedure GetStepsList(AList: TStringList);     // Name=sitenum, value=#steps
    procedure GetStepsHistogram(ASteps: TStringList); // Name=#steps; value=frequency

      // computes indices for a tree;  i-means only informative sites
    function  ComputeIndicesAtSite(ASite: Integer; var si: Integer; var ci, ri, rci: Double): Boolean;
    procedure ComputeTreeIndices(var CI, RI, RCI, iCI, iRI, iRCI: Double);
      // Computes gamma parameter and tabulates frequency histogram
    procedure GetMpGammaPara(var MeanL, VarL, mpG: Double; ASteps: TStringList);
      // Compute Branch lengths by MP method
    procedure ComputeAvgMPBLens(BLens: PArrayOfDouble);
      // substitution matrix
    procedure ComputeChangeMatrix(Pai: PArrayOfDouble; MatF: PDistanceMatrix);
      //Reconstructing ancestral states at a given site
    procedure ComputeSiteMPAncStates(ASite: Integer; var AStates: array of LongInt); // returns in an array
      // this is interesting
//    procedure ComputeSiteMPAncStatesUsingMPRFreq(ASite: Integer; const AStates: PArrayOfInt); // returns in an array
      //Get complete sequences for all nodes
    procedure ComputeMPAncSeqs(AList: TList);  // list of sequences, each seq is an array of LongInt
      //Output all MPR pathways: Only for test purposes
    procedure ComputeMPRPathways(ASite: Integer; AList: TList; var ACost: Integer);
  end;

implementation

uses
  SysUtils, MegaUtils, MPleaseWait, MGlobalSettings, MRuntimeProgressDlg, Math, DateUtils
  {$IFDEF VISUAL_BUILD}, MParsimonyEvoPathsDlg, Mega_Main, StdCtrls{$ELSE}, MD_MegaMain,  MegaUtils_NV{$ENDIF};

  var
      PleaseWait : TPleaseWait;
      Progress: TRuntimeProgress;

constructor TComputeParsimInfo.Create;
begin
  BranchLengthsAreComputed := False;
  FNumPathsForPrompt := DEFAULT_PATHS_BEFORE_WARN;
  FIsNucData := True;
  NoOfStates := 4;
  NoOfSites  := -1;
  FCurrentSite := -1;
  FSeqs := nil;
  Tree      := nil;

  // configuration the currently focused site
  CurSiteConfig := nil;
  DownPass     := nil;
  UpPass       := nil;
  CurSiteAnc   := nil;
  MprBaseLists := nil;
  CurBasePos   := nil;
  CurPathway   := nil;
  MprPathsList := nil;
  NoOfMpPaths  := 0;
  MpPathCost   := 0;
  CurPathCost  := 0;

  CurPathBLen  := nil;
  AllPathsBLen := nil;
  AllSitesBLen := nil;

  CurPathMat   := nil;
  AllPathsMat  := nil;
  AllSitesMat  := nil;

  PromptAgain  := True;
end;

destructor TComputeParsimInfo.Destroy;
begin
  if Assigned(Progress) then
    FreeAndNil(Progress);
  ClearMemory;
end;

procedure TComputeParsimInfo.ClearMemory;
var
  i: Integer;
begin
  try
    FSeqs := nil;
//    if FSeqs <> nil then
//    begin
//      for i := 0 to FSeqs.Count-1 do
//      begin
//        if FSeqs[i] <> nil then
//          FreeMem(FSeqs[i]);
//        FSeqs[i] := nil;
//      end;
//      FSeqs.Free;
//      FSeqs := nil;
//    end;

    if Tree <> nil then
      Tree := nil;

    GivenBLen := nil;

    CurSiteConfig := nil;
    CurSiteAnc := nil;
    DownPass := nil;
    UpPass := nil;

    if MprBaseLists <> nil then
      for i:=0 to High(MprBaseLists)-1 do
        MprBaseLists[i].Free;
    MprBaseLists := nil;
  Except on E: Exception do
    ShowMessage('Error: ' + E.Message);
  end;
end;

procedure TComputeParsimInfo.ClearMprBaseLists;
var
 i:Integer;
begin
  if MprBaseLists <> nil then
    for i:=0 to High(MprBaseLists)-1 do
      MprBaseLists[i].Free;
  MprBaseLists := nil;
end;

// ATree is indexed from 0
procedure TComputeParsimInfo.SetTreeData(const ATree: TTreeData);
var
  i: Integer;
begin
  NoOfTaxa  := ATree.NoOfOtus;
  NoOfNodes := 2*NoOfTaxa-1;
  MaxNodeId := NoOfNodes-1;
  RootNode  := MaxNodeId;

  SetLength(CurSiteConfig, NoOfTaxa);
  SetLength(CurSiteAnc,    NoOfNodes);
  SetLength(DownPass,      NoOfNodes);
  SetLength(UpPass,        NoOfNodes);
  SetLength(Tree,          NoOfNodes);

  for i:=0 to NoOfTaxa-1 do
  begin
    Tree[i].X := -1;
    Tree[i].Y := -1;
  end;

  for i:=NoOfTaxa to NoOfNodes-1 do
  begin
    Tree[i].X := ATree.Node[i-NoOfTaxa].des1;
    Tree[i].Y := ATree.Node[i-NoOfTaxa].des2;
  end;

  if ATree.isBLen then
  begin
    SetLength(GivenBLen, NoOfNodes);
    for i:=0 to NoOfNodes-1 do
      GivenBLen[i] := ATree.BLen[i];
  end;
end;

procedure TComputeParsimInfo.SetSeqs(AList: TList);
begin
  if FSeqs <> nil then
    FSeqs.Free;
  FSeqs := AList;
end;

procedure TComputeParsimInfo.SetNucData(AValue : Boolean); // set by the caller
begin
  FIsNucData := AValue;
  if AValue then NoOfStates := 4
            else NoOfStates := 20;
end;

procedure TComputeParsimInfo.SetModel(AModel : TDistType); // set by the caller
begin
  FModel := AModel;
end;

// also sets IsConst flag
procedure TComputeParsimInfo.LoadCurSite(ASite: Integer);
var
  i: Integer;
  OrBases:  LongInt;
  AndBases: LongInt;
begin
  FCurrentSite := ASite;
  OrBases := 0;
  AndBases := High(LongInt);
  for i:=0 to NoOfTaxa-1 do
  begin
    if NoOfStates <= 4 then
      CurSiteConfig[i] := LongInt(PAnsiChar(FSeqs[i])[ASite])
    else
      CurSiteConfig[i] := PArrayOfLongInt(FSeqs[i])[ASite];
    OrBases  := OrBases  or  CurSiteConfig[i];
    AndBases := AndBases and CurSiteConfig[i];
  end;
  IsConst       := (GetNoOfBases(OrBases) = 1);
  IsValidConfig := (GetNoOfBases(AndBases) <= NoOfStates);
  CurSiteNoOfSteps := 0; // just done here for safety
end;

function TComputeParsimInfo.CheckParsimInfo: Boolean;
var
  i, Kinds, Index: Integer;
begin
  for Index:=0 to NoOfStates-1 do
    Freq[Index] := 0;

  for i:=0 to NoOfTaxa-1 do
  begin
    if GetNoOfBases(CurSiteConfig[i]) <> 1 then
      Continue;
    Index := GetBaseIndex(CurSiteConfig[i]);
    Inc(Freq[Index]);
  end;

  Kinds := 0;
  for Index:=0 to NoOfStates-1 do
    if Freq[Index] >= 2 then
      Inc(Kinds);
  Result := (Kinds >= 2);
end;

function TComputeParsimInfo.CharToMap(AChar: AnsiChar): Integer; // maps a char to integer
begin
  if FIsNucData then
    Result := Integer(NucToParsimMap(AChar))
  else
    Result := Integer(AminoToParsimMap(AChar));
end;

function TComputeParsimInfo.MapToChar(AInt: Integer): AnsiChar;
begin
  if FIsNucData then
    Result := ParsimMapToNuc(AnsiChar(AInt))
  else
    Result := ParsimMapToAmino(AInt);
end;

function TComputeParsimInfo.GetNoOfBases(ABase: Integer): Integer;
var
  MovingMask: Integer;
begin
  MovingMask := $00000001;
  Result := 0;
  while ABase <> 0 do
  begin
    if (ABase and MovingMask) <> 0 then
    begin
      Inc(Result);
      ABase := ABase and not (ABase and MovingMask);
    end;
    MovingMask := MovingMask shl 1;
  end;
end;

// first the index of the first base from the right
function TComputeParsimInfo.GetBaseIndex(ABase: Integer): Integer;
var
  MovingMask: Integer;
begin
  MovingMask := $00000001;
  Result := 0;
  while ABase <> 0 do
  begin
    if (ABase and MovingMask) <> 0 then
      Exit;  // done
    Inc(Result);
    MovingMask := MovingMask shl 1;
  end;
end;

// number of MP steps at a site
function TComputeParsimInfo.GetStepsAt(ASite: Integer): Integer;
begin
  Result := -1;
  LoadCurSite(ASite);
  if not IsValidConfig then
    Exit;
  if IsConst then  // makes analysis faster
  begin
    Result := 0;
    Exit;
  end;
  // initializes
  CurSiteNoOfSteps := 0;  // shows that invalid site
  BuildDownPass(RootNode); // recursive call; automatically takes cuare
  Result := CurSiteNoOfSteps;
end;

// computes indices for the tree:
// Yet to decide if to include all variable sites or all parsimony informative sites only
// Koichiro let's discuss
// Only bases results on unambiguous bases/residues
function TComputeParsimInfo.GetBasicInfoForIndices(ASite: Integer; var si, mi, gi: Integer): Boolean;
var
  i, j, gTemp: Integer;
  x, OrBases, AndBases: LongInt;
begin
  mi := 0;  gi := 0;  si :=-1;

  Result := False;

  // instead of calling GetStepsAt, it modifies things
  LoadCurSite(ASite);
  si := 0;
  if IsValidConfig and not IsConst then
  begin
    // treat any ambiguity as missing data
    for i:=0 to NoOfTaxa-1 do
      if GetNoOfBases(CurSiteConfig[i]) > 1 then
      begin
        if NoofStates = 4 then       CurSiteConfig[i] := Integer(pmBaseN)
      else if NoofStates = 20 then   CurSiteConfig[i] := pmResiX
      else                           raise Exception.Create('Unanticipated number of states');
    end;
    // initializes
    CurSiteNoOfSteps := 0;  // shows that invalid site
    BuildDownPass(RootNode); // recursive call; automatically takes cuare
    si := CurSiteNoOfSteps;
  end;

  if si <= 0 then  // not useful site as either const or invalid
     Exit;
  // check if the site is parsim info or not
  //=== Compute m: minimum # of possible changes for any tree
  OrBases := 0;
  AndBases := High(LongInt);
  for i:=0 to NoOfTaxa-1 do
  begin
    x := CurSiteConfig[i];
    if GetNoofBases(x) > 1 then
      continue;
    AndBases := AndBases and x;
  end;

  // MISSING data
  if AndBases <> 0 then // wow, all states have at least one base in common
    mi := 0
  else // 0 or more substitutions are needed
  begin
    mi := 0;
    OrBases := 0;

    // do it for all taxa that contain unambiguous positions
    for i:=0 to NoOfTaxa-1 do
    begin
      x := CurSiteConfig[i];
      if GetNoofBases(x) > 1 then
        continue;
      if OrBases = 0 then
        OrBases := CurSiteConfig[i]
      else if (OrBases and CurSiteConfig[i]) = 0 then
      begin
        OrBases := OrBases or CurSiteConfig[i];
        Inc(mi);
      end;
    end;
  end;

  //=== Compute g: maximum # of possible changes for any conceivable tree
  // I do this by assigning every possible state at the central node
  gi := NoOfTaxa+1;
  for i:=0 to NoOfTaxa-1 do
  begin
    x := CurSiteConfig[i];
    if GetNoOfBases(x) > 1 then
      continue;
    OrBases  := x;
    gTemp    := 0;
    for j:= 0 to NoOfTaxa-1 do
    begin
      x := CurSiteConfig[j];
      if GetNoOfBases(x) > 1 then
        continue;
      if (OrBases xor x) <> 0 then
        Inc(gTemp);
    end;
    if gTemp < gi then
      gi := gTemp;
  end;
  Result := True;
end;

// Computes indices at a site
function TComputeParsimInfo.ComputeIndicesAtSite(ASite: Integer; var si: Integer; var ci, ri, rci: Double): Boolean;
var
  mi: Integer = -1;
  gi: Integer = -1;
begin
  ci := 0;
  ri := 0;
  rci := 0;
  si :=-1;

  Result := GetBasicInfoForIndices(ASite, si, mi, gi);
  if not Result then  // not useful site as either const or invalid
    Exit;
  ci := mi/si;
  if (gi-mi) > 0 then
    ri := (gi-si)/(gi-mi);
  rci := ci*ri;
end;

//Compute indices for the whole tree
procedure TComputeParsimInfo.ComputeTreeIndices(var CI, RI, RCI, iCI, iRI, iRCI: Double);
var
  site: Integer;
  si: Integer = -1;
  mi: Integer = -1;
  gi: Integer = -1;
  infoS, infoM, infoG     : Integer;
  S, M, G     : Integer;
  SitesPerPercent: Integer;
begin
  try
    CI := 0;  RI := 0;  RCI := 0;
    iCI := 0; iRI := 0; iRCI := 0;

    S := 0;      M := 0;      G := 0;
    infoS := 0;  infoM := 0;  infoG := 0;

    PleaseWait := TPleaseWait.Create(nil);
    PleaseWait.Caption := 'Parsimony Analysis';
    PleaseWait.Action := 'Computing tree indices...';

    SitesPerPercent := NoOfSites div 100;
    PleaseWait.PercentDone := 0;
    PleaseWait.Show;

    for site:=0 to NoOfSites-1 do
    begin
      if not GetBasicInfoForIndices(site, si, mi, gi) then
        continue;
      if si <= 0 then  // not useful site as either const or invalid
        Continue;
      S := S + si;
      M := M + mi;
      G := G + gi;

      //information for the parsimony informative sites only
      if not CheckParsimInfo then
        Continue;
      infoS := infoS + si;
      infoM := infoM + mi;
      infoG := infoG + gi;

      if (SitesPerPercent > 0) and ((site mod SitesPerPercent) = 0) then
        PleaseWait.PercentDone := site div sitesPerPercent;
    end;

    CI := M/S;
    if (G-M) > 0 then
      RI := (G-S)/(G-M);
    RCI := CI*RI;
    iCI := infoM/infoS;
    if (infoG-infoM) > 0 then
      iRI := (infoG-infoS)/(infoG-infoM);
    iRCI := iCI*iRI;
  finally
    FreeAndNil(PleaseWait);
  end;
end;

//=====Computations of number of subs and related information==
function TComputeParsimInfo.ComputeTreeLen: Integer;
var
  i, s: Integer;
  SitesPerPercent: Integer;
  PleaseWait : TPleaseWait;
begin
  try
    PleaseWait := TPleaseWait.Create(nil);
    PleaseWait.Caption := 'Parsimony Analysis';
    PleaseWait.Action := 'Computing tree length...';

    SitesPerPercent := NoOfSites div 100;
    PleaseWait.PercentDone := 0;
    PleaseWait.Show;

    Result := 0;
    for i:=0 to NoOfSites-1 do
    begin
      s := GetStepsAt(i);
      if s > 0 then // to avoid adding -1
        Result := Result + s;
      if (SitesPerPercent > 0) and ((i mod SitesPerPercent) = 0) then
        PleaseWait.PercentDone := i div SitesPerPercent;
    end;
  finally
    FreeAndNil(PleaseWait);
  end;
end;

  // number of MP steps at all sites
procedure TComputeParsimInfo.GetStepsList(AList: TStringList);
var
  i: Integer;
  SitesPerPercent: Integer;
  PleaseWait : TPleaseWait;
begin
  try
    PleaseWait := TPleaseWait.Create(nil);
    PleaseWait.Caption := 'Parsimony Analysis';
    PleaseWait.Action := 'Computing tree length...';

    SitesPerPercent := NoOfSites div 100;
    PleaseWait.PercentDone := 0;
    PleaseWait.Show;

    AList.Clear;
    for i:=0 to NoOfSites-1 do
    begin
      AList.Add(IntToStr(i)+'='+IntToStr(GetStepsAt(i)));
      if (SitesPerPercent > 0) and ((i mod SitesPerPercent) = 0) then
        PleaseWait.PercentDone := i div SitesPerPercent;
    end;
  finally
    FreeAndNil(PleaseWait);
  end;
end;

  // parsimony changes histogram
procedure TComputeParsimInfo.GetStepsHistogram(ASteps: TStringList);
var
  mpL, i: Integer;
  StepsFreq: array of Integer;
  SitesPerPercent: Integer;
  PleaseWait : TPleaseWait;
begin
  try
    PleaseWait := TPleaseWait.Create(nil);
    PleaseWait.Caption := 'Parsimony Analysis';
    PleaseWait.Action := 'Computing tree length...';

    SitesPerPercent := NoOfSites div 100;
    PleaseWait.PercentDone := 0;
    PleaseWait.Show;

    ASteps.Clear;
    SetLength(StepsFreq, NoOfNodes+1); // maximum number of steps is bounded by the number of branches
    for i:=0 to NoOfNodes do
      StepsFreq[i] := 0;

    for i:=0 to NoOfSites-1 do
    begin
      mpL := GetStepsAt(i);
      if mpL >= 0 then
        Inc(StepsFreq[i],1);

      if (SitesPerPercent > 0) and ((i mod SitesPerPercent) = 0) then
        PleaseWait.PercentDone := i div SitesPerPercent;
    end;

    for i:=0 to NoOfNodes do
      if StepsFreq[i] > 0 then
        ASteps.Add(IntToStr(i)+'='+IntToStr(StepsFreq[i]));
  finally
    StepsFreq := nil;
    FreeAndNil(PleaseWait);
  end;
end;

// Estimation of gamma parameter and minimum number of substitutions
procedure TComputeParsimInfo.GetMpGammaPara(var MeanL, VarL, mpG: Double; ASteps: TStringList);
var
  Cat, Freq, i, SumL, NSites: Integer;
begin
  ASteps.Clear;
  MeanL := -1; VarL := -1;
  mpG := -1;

  GetStepsHistogram(ASteps);

  // compute mean and var of no of steps
  SumL := 0;
  NSites := 0;
  VarL := 0;
  with ASteps do
    for i:=0 to Count-1 do
    begin
      Cat   := StrToInt(Names[i]);
      Freq :=  StrToInt(Values[Names[i]]);
      NSites := NSites + Freq;
      SumL   := SumL + Cat*Freq;
      VarL   := VarL + Cat*Cat*Freq; // this can be too large
    end;
  MeanL := sumL/NSites;
  VarL  := VarL/(NSites-1) - (MeanL/(NSites-1))*MeanL*NSites;
  if VarL - MeanL > 0.001 then
    mpG   := MeanL*MeanL/(VarL-MeanL)
  else
    mpG := 1000;
end;

procedure TComputeParsimInfo.ComputeChangeMatrix(Pai: PArrayOfDouble; MatF: PDistanceMatrix);
var
  i, j, k, TotalPairs, n: Integer;
  x, TotalChange, Total: Double;
  freq : array of double;
begin
  n := NoOfStates;
  TotalPairs := n*n;

  try
    SetLength(CurPathMat,  TotalPairs);
    SetLength(AllPathsMat, TotalPairs);
    SetLength(AllSitesMat, TotalPairs);

    for i:=0 to TotalPairs-1 do
      AllSitesMat[i] := 0;

    SetLength(Freq, n);
    for i:=0 to n-1 do
      Freq[i] := 0;

    // Place to hold ancestral states for equivocal paths
    SetLength(MprBaseLists, NoOfNodes);
    for i:= 0 to NoOfNodes-1 do
      MprBaseLists[i] := TLongIntList.Create;

    // place to hold moving index
    SetLength(CurBasePos, NoOfNodes);

    // Computations for all sites
    for k:=0 to NoOfSites-1 do
    begin
//      if k > 8 then break; // tester
      LoadCurSite(k);
      if not IsValidConfig then
        continue;
      if IsConst then  // makes analysis faster
      begin
        i := GetBaseIndex(CurSiteConfig[0]);
        AllSitesMat[i*n+i] := AllSitesMat[i*n+i] +  (MaxNodeId-1);  // that is it
        Freq[i] := Freq[i] + NoOfTaxa; // increment the freq by that much
        Continue;
      end;
      BuildDownPass(RootNode);
      BuildUpPass(RootNode);
      UpdateMprBaseLists(True); // just index of states for use later
      CalculateEquivocalPathways;
      // update information if site has valid pathways
      for i:=0 to TotalPairs-1 do
        AllSitesMat[i] := AllSitesMat[i] + (AllPathsMat[i]/NoOfMpPaths);
      // also update the freq estimates; as the sequence is loaded
      // at this time, we can compute

      // we may actually use MatF later???????????
      for i:=0 to NoOfTaxa-1 do
        if MprBaseLists[i].Count = 1 then
          Freq[MprBaseLists[i][0]] := Freq[MprBaseLists[i][0]] + 1;
    end;

    Total:=0;
    for i:= 0 to n-1 do
      Total := Total + Freq[i];

    if Total = 0 then
      raise Exception.Create('No valid sites in the data');

    for i:= 0 to n-1 do
      Freq[i] := Freq[i]/Total;

    // Count the total amount of change
    TotalChange := 0;
    for i:=0 to NoOfStates-1 do
      for j:=0 to i-1 do
        TotalChange := TotalChange + AllSitesMat[i*n + j] + AllSitesMat[i+j*n];

    //make the matrix of observed changes symmetrical as an unrooted tree
    for i:= 0 to n-1 do
    begin
      if MatF <> nil then
        MatF[i,i] := AllSitesMat[i*n+i];
      for j:=0 to i-1 do
      begin
        x := AllSitesMat[j*n+i] + AllSitesMat[j+i*n];
        AllSitesMat[i*n+j] := x/2;
        AllSitesMat[j*n+i] := x/2;
        if MatF <> nil then
        begin
          MatF[i,j] := x/2;
          MatF[j,i] := x/2;
        end;
      end;
    end;

    if Pai <> nil then
      for i:= 0 to NoOfStates-1 do
        Pai[i] := Freq[i];
  finally
    CurPathMat  := nil;
    AllPathsMat := nil;
    AllSitesMat := nil;
    Freq := nil;
    if MprBaseLists <> nil then
      ClearMprBaseLists;
    MprBaseLists := nil;
    CurBasePos := nil;
  end;
end;

//Get MPR pathways for a given site
procedure TComputeParsimInfo.ComputeMPRPathways(ASite: Integer; AList: TList; var ACost: Integer);
var
  i: Integer;
//  ProbSum: Double;
begin
  try
    MprPathsList := TList.Create;  // means we need all pathways stored
    SetLength(CurPathway, MaxNodeId);

    // Place to hold ancestral states for equivocal paths
    SetLength(MprBaseLists, NoOfNodes);
    for i:= 0 to NoOfNodes-1 do
      MprBaseLists[i] := TLongIntList.Create;

    // place to hold moving index
    SetLength(CurBasePos, NoOfNodes);

    // Computations for a site
    LoadCurSite(ASite);
    if not IsValidConfig then
      raise Exception.Create('Site does not have a valid configuration');
    BuildDownPass(RootNode);
    BuildUpPass(RootNode);
    UpdateMprBaseLists(False); // need actual states not just index; fixed in mega2 beta build 2
    CalculateEquivocalPathways;
    ACost := mpPathCost;
//    ProbSum := 0;
    for i:=0 to MprPathsList.Count-1 do
    begin
      AList.Add(MprPathsList[i]);
      MprPathsList[i] := nil;
    end;

//    if AProbList <> nil then
//      for i:=0 to AProbList.Count-1 do
//        AProbList[i] := AProbList[i]/ProbSum;
  finally
    if MprBaseLists <> nil then
      ClearMprBaseLists;
    MprBaseLists := nil;
    CurBasePos := nil;
    if MprPathsList <> nil then
    begin
      for i:= 0 to MprPathsList.Count-1 do
      begin
        if MprPathsList[i] <> nil then
          FreeMem(MprPathsList[i]);
        MprPathsList[i] := nil;
      end;
      MprPathsList.Free;
      MprPathsList := nil;
    end;
    CurPathway := nil;
  end;
end;

//Gets ancestral states for all nodes at a given site
procedure TComputeParsimInfo.ComputeSiteMPAncStates(ASite: Integer; var AStates: array of LongInt);
var
  i: Integer;
begin
  LoadCurSite(ASite);
  for i:=0 to MaxNodeId do
    AStates[i] := 0;
  if not IsValidConfig then
    Exit;
  if IsConst then
    for i:=0 to MaxNodeId do
      CurSiteAnc[i] := CurSiteConfig[0]
  else
  begin
    BuildDownPass(RootNode);
    BuildUpPass(RootNode);
  end;

  //
  for i:=0 to MaxNodeId do
    AStates[i] := CurSiteAnc[i];
end;

//Get complete sequences for all nodes and all sites
procedure TComputeParsimInfo.ComputeMPAncSeqs(AList: TList);
var
  i,j: Integer;
  TempPChar: PAnsiChar;
begin
  // assumes memory is given
  // now get ancestral states
  TempPChar := nil;
  try
    // allocate memory for outputting results
    for i:=0 to 2*NoOfTaxa-1 do
    begin
      GetMem(TempPChar, (NoOfSites+1)*sizeOf(AnsiChar));
      TempPChar[NoOfSites] := #0; // to ensure proper termination
      AList.Add(TempPChar);
      TempPChar := nil;
    end;
    // get information and finish up
    for i:=0 to NoOfSites-1 do
    begin
      LoadCurSite(i);  // fills the above
      BuildDownPass(RootNode);
      BuildUpPass(RootNode);
      for j:=0 to  MaxNodeId-1 do  // extant sequences
      begin
        TempPChar := AList[j];
        TempPChar[i] := MapToChar(CurSiteConfig[j]);
      end;
    end;
  finally
   // PleaseWait.Hide;
    if TempPChar <> nil then FreeMemAndNil(TempPChar);
  end;
end;

//------------- Computational functions
// Gets Anc states based solely on descendents
procedure TComputeParsimInfo.BuildDownPass(Node: Integer);
begin
  // first handle external nodes
  if Node < NoOfTaxa then
  begin
    DownPass[Node]   := CurSiteConfig[Node];
    CurSiteAnc[Node] := DownPass[Node];
    Exit;
  end;

  // Otherwise, it is an internal node
  BuildDownPass(Tree[Node].X); // recursive call
  BuildDownPass(Tree[Node].Y); // recursive call

  sX  := DownPass[Tree[Node].X];
  sY  := DownPass[Tree[Node].Y];
  sXY := sX and sY;
  if sXY = 0 then
  begin
    Inc(CurSiteNoOfSteps);
    sXY := sX or sY;
  end;
  DownPass[Node] := sXY;
end;

// finds Anc states based on sisters and cousins; and DownPass;
// Called it after BuildDownPass
procedure TComputeParsimInfo.BuildUpPass(Node: Integer);
  function AndElseOr(a,b: Integer): Integer;
  begin
    Result := a and b;
    if Result = 0 then
      Result := a or b;
  end;
begin
  if Node < NoOfTaxa then  // do not go down to leaves
  begin
//    if GetNoOfBases(CurSiteConfig[Node]) = 1 then
//      CurSiteAnc[Node] := CurSiteConfig[Node]
//    else
    CurSiteAnc[Node] := CurSiteConfig[Node]; //this is to fix possible states
    Exit;
  end;

  // Generate UpPass for root first
  if Node = (NoOfNodes-1) then // for root node; first upPass needed
    UpPass[Node] := DownPass[Node];

  // Generates UpPass for both descendents
  sXY := UpPass[Node];             // Parent state
  sX  := DownPass[Tree[Node].X];  // left child
  sY  := DownPass[Tree[Node].Y];  // right child

  UpPass[Tree[Node].X] := AndElseOr(sXY, sY);
  UpPass[Tree[Node].Y] := AndElseOr(sXY, sX);

  // Now we generate Up pass for the descendents of the two leaves
  BuildUpPass(Tree[Node].X);
  BuildUpPass(Tree[Node].Y);

  // now assign the final MPR sets for both descendents
  sXY := UpPass[Node];  // own state
  sX  := DownPass[Tree[Node].X]; // left child
  sY  := DownPass[Tree[Node].Y]; // right child
  SF := sXY and sX and sY;

  if SF = 0 then
  begin
    sF := (sXY and sX) or (sXY and sY) or (sX and sY);
    if SF = 0 then
      sF := sXY or sX or sY;
  end;
  CurSiteAnc[Node]  := sF;
end;

// computes Average Branch length
procedure TComputeParsimInfo.ComputeAvgMPBLens(BLens: PArrayOfDouble);
var
  i, k: Integer;

begin
  try try
    {$IFDEF VISUAL_BUILD}
    Progress := TRuntimeProgress.Create(MegaForm);
    Progress.DataFileName  :=  MegaForm.DataTitle;
    Progress.DataTitle     :=  MegaForm.DataTitle;
    Progress.SetKeepOnTop(True);
    {$ELSE}
    Progress := TRuntimeProgress.Create(nil);
    Progress.DataFileName  :=  D_MegaMain.DataFileName;
    Progress.DataTitle     :=  D_MegaMain.DataTitle;
    {$ENDIF}

    Progress.Caption := 'Maximum Parsimony Analysis';
    Progress.UpdateRunStatusInfo('Status', 'Computing average branch lengths');

    Progress.UpdatePercentProgress(0);
    {$IFDEF VISUAL_BUILD}
    Progress.Show;
    {$ENDIF}

    for i:=0 to NoOfNodes-2 do // this is here to ensure that exc raised properly
      BLens[i] := 0;

    SetLength(CurPathBLen,  NoOfNodes);
    SetLength(AllPathsBLen, NoOfNodes);
    SetLength(AllSitesBLen, NoOfNodes);

    // Initialize the overall blens
    for i:=0 to NoOfNodes-1 do
      AllSitesBLen[i] := 0;

    // Place to hold ancestral states for equivocal paths
    SetLength(MprBaseLists, NoOfNodes);
    for i:= 0 to NoOfNodes-1 do
      MprBaseLists[i] := TLongIntList.Create;

    // place to hold moving index
    SetLength(CurBasePos, NoOfNodes);

    // Computations for all sites
    FNumPathsForPrompt := DEFAULT_PATHS_BEFORE_WARN;
    Progress.SetFixedColWidthForStr('Equivocal Paths Found(current site)  ');
    Progress.UpdateRunStatusInfo('Current Site ', '1');
    Progress.UpdateRunStatusInfo('Max Paths to Explore(per site)', Format('%.3e', [FNumPathsForPrompt * 1.0]));
    Progress.UpdateRunStatusInfo('Paths Explored(current site)', '0');
    Progress.UpdateRunStatusInfo('Equivocal Paths Found(current site)', '0');
    for k:=0 to NoOfSites-1 do
    begin
      Application.ProcessMessages;
      {$IFDEF VISUAL_BUILD}
      if Progress.UserCancelled then
      begin
        if MessageDlg('Do you want to cancel calculation of average branch lengths?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          ShowMessage('Only estimates from the first '+IntToStr(k)+ ' sites will be used');
          break;
        end
        else
        begin
          Progress.SetStopButtonDown(False);
          Progress.Show;
        end;
      end;
      {$ENDIF}
      Progress.UpdateRunStatusInfo('Current Site ', IntToStr(k + 1));
      Application.ProcessMessages;
      LoadCurSite(k); // To do: also check if the site is var or not
      if not IsValidConfig then
        continue;
      if IsConst then  // no need to waste time
        continue;
      BuildDownPass(RootNode);
      BuildUpPass(RootNode);
      UpdateMprBaseLists(False); // Get actual states not their index
      if not CalculateEquivocalPathways then  // user has skipped the site
      begin
        {$IFDEF VISUAL_BUILD}
        Progress.Hide;
        ShowMessage('Only estimates from the first '+IntToStr(k)+ ' sites will be used');
        {$ELSE}
        Warn_NV('Only estimates from the first '+IntToStr(k)+ ' sites will be used');
        {$ENDIF}
        Break;
      end;

      // check if the AllPathsBLen is correct or not
//      ASum := 0;
//      for i:=0 to NoOfNodes-1 do
//        ASum := ASum + AllPathsBLen[i];
//      if ASum <> CurSiteNoOfSteps*NoOfMpPaths then
//      begin
//        raise Exception.Create('Error in site '+IntToStr(k+1));
//        Exit;
//      end;

      for i:=0 to NoOfNodes-1 do
        AllSitesBLen[i] := AllSitesBLen[i] + AllPathsBLen[i]/NoOfMpPaths;

      if ((k mod 5) = 0) then
        Progress.UpdatePercentProgress(Trunc(k/NoOfSites * 100));
    end;
    // provide information transmittal
    for i:=0 to NoOfNodes-2 do
      BLens[i] := AllSitesBLen[i];
    BranchLengthsAreComputed := True;
  except
    on E: Exception do raise;
  end;
  finally
    CurPathBLen := nil;
    AllPathsBLen := nil;
    AllSitesBLen := nil;
    if MprBaseLists <> nil then
    begin
      for i:= 0 to NoOfNodes-1 do
        MprBaseLists[i].Destroy;
      MprBaseLists := nil;
    end;
    CurBasePos := nil;
    if Assigned(Progress) then
      FreeAndNil(Progress);
  end;
end;

// Computes equivocal pathways for the currently loaded site
function TComputeParsimInfo.CalculateEquivocalPathways: Boolean;
var
  i: Integer;
  NumPathsForSite: Integer;
  {$IFDEF VISUAL_BUILD}
  EvoPathsDlg: TEvoPathsDlg;
  {$ENDIF}
  ProgressInterval: Integer;
  LastProgressUpdate: TDateTime;
begin
  MpPathCost  := 0;
  NoOfMpPaths := 0;
  NumPathsForSite := 0;
  ProgressInterval := 1000;
  LastProgressUpdate := Time;
  {$IFDEF VISUAL_BUILD}
  EvoPathsDlg := nil;
  {$ENDIF}

  // initializes the blen holder over all paths
  if AllPathsBLen <> nil then
    for i:=0 to NoOfNodes-1 do
      AllPathsBLen[i] := 0;

  // initializes all paths subst matrix
  if AllPathsMat <> nil then
    for i:=0 to (NoOfStates*NoOfStates)-1 do
      AllPathsMat[i] := 0;

  for i:=0 to NoOfNodes-1 do
    CurBasePos[i] := 0;
  try
    RefreshCounter := 0;
    while True do
    begin
      if CurPathBLen <> nil then
        for i:=0 to NoOfNodes-1 do
          CurPathBLen[i] := 0;

      if CurPathMat <> nil then
        for i:=0 to NoOfStates*NoOfStates-1 do
          CurPathMat[i] := 0;

      CurPathCost := 0;

      EvaluatePathwaysWithRoot;
      OutputPathway;
      inc(NumPathsForSite);
      if ((NumPathsForSite mod ProgressInterval) = 0) and (MillisecondsBetween(Time, LastProgressUpdate) > ProgressInterval) then
      begin
        Progress.UpdateRunStatusInfo('Paths Explored(current site)', Format('%.3e', [NumPathsForSite * 1.0]));
        Progress.UpdateRunStatusInfo('Equivocal Paths Found(current site)', Format('%.3e', [NoOfMpPaths * 1.0]));
        if NumPathsForSite > (DEFAULT_PATHS_BEFORE_WARN div 500) then
          ProgressInterval := Min(5000, ProgressInterval * 2); // to speed things up if it is taking a long time
        LastProgressUpdate := Time;
        end;
      {$IFDEF VISUAL_BUILD}
      if PromptAgain and (NumPathsForSite >= FNumPathsForPrompt) then
      begin
        Progress.UpdateRunStatusInfo('Paths Explored(current site)', Format('%.3e', [NumPathsForSite * 1.0]));
        NumPathsForSite := 0;
        if not Assigned(EvoPathsDlg) then
          EvoPathsDlg := TEvoPathsDlg.Create(MegaForm);
        EvoPathsDlg.SetNumPathsToTry(FNumPathsForPrompt);
        case EvoPathsDlg.ShowModal of
          mrOk:
            begin
              FNumPathsForPrompt := EvoPathsDlg.NumPathsToTry;
              Progress.UpdateRunStatusInfo('Max Paths to Explore(per site)', Format('%.3e', [FNumPathsForPrompt * 1.0]));
              if EvoPathsDlg.DoPromptAgain then
                PromptAgain := True
              else
                PromptAgain := False;
            end;
          mrAbort:
            begin
              FNumPathsForPrompt := EvoPathsDlg.NumPathsToTry;
              Progress.UpdateRunStatusInfo('Max Paths to Explore(per site)', Format('%.3e', [FNumPathsForPrompt * 1.0]));
              if EvoPathsDlg.DoPromptAgain then
                PromptAgain := True
              else
                PromptAgain := False;
              break;
            end;
        end;

      end;
      {$ELSE}
      if (NumPathsForSite >= FNumPathsForPrompt) then
      begin
        NumPathsforSite := 0;
        Warn_NV('Site number ' + IntToStr(FCurrentSite) + ' (of the sites included in the analysis) ' +
              'has many possible evolutionary paths. Only the first ' + IntToStr(FNumPathsForPrompt) + ' ' +
              'paths were evaluated when computing average branch lengths.');
        break;
      end;
      {$ENDIF}
      i := MaxNodeId-1;  // since root has the MaxNodeId;
      while True do
      begin
        if i = 0 then  // a sign that all below the root are done
          break;
        if CurBasePos[i] <> (TLongIntList(MprBaseLists[i]).Count-1) then
        begin
          Inc(CurBasePos[i]);  // advance it
          break;
        end;
        // otherwise reset to be below nodes
        if i < MaxNodeId then
          CurBasePos[i] := 0;
        Dec(i);
      end; // end  of internal while True

      if i=0 then // a sign that all below the root are done
        break;
      Application.ProcessMessages;
      {$IFDEF VISUAL_BUILD}
      if Assigned(Progress) and ((NumPathsForSite mod 1000) = 0) then
      begin
        if Progress.UserCancelled then
        begin
          Progress.Hide;
          if MessageDlg('Do you want to abort calculation of average branch lengths?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          begin
            Result := False;
            RefreshCounter := -1;
            Exit;
          end
          else
          begin
          Progress.SetStopButtonDown(False);
          Progress.Show;
          end;
        end;
      end;
      {$ENDIF}
    end; // End  of External while (True);
    Result := True;
  finally
    {$IFDEF VISUAL_BUILD}
    if Assigned(EvoPathsDlg) then
      EvoPathsDlg.Free;
    {$ENDIF}
  end;
end;

// Start evaluating pathway with root
procedure TComputeParsimInfo.EvaluatePathwaysWithRoot;
begin
  sX  := GetCurMprBase(Tree[RootNode].X);
  sY  := GetCurMprBase(Tree[RootNode].Y);

  sXY := sX; // for unrooted trees we assume this

  // Costs 1 if different, othewise 0
  if not ((sx=0) or (sy=0)) then
    if sX <> sY  then  Inc(CurPathCost);

  if not ((sx=0) or (sy=0)) then
  begin
    if CurPathMat <> nil then // Counts substitutions from -> to
      CurPathMat[sX*NoOfStates+sY] := CurPathMat[sX*NoOfStates+sY]+1;
      // let the branch empty that leads to left

    if CurPathBLen <> nil then
      if sX <> sY then CurPathBLen[Tree[RootNode].X] := CurPathBLen[Tree[RootNode].X]+1;
  end;

  // We write the states to the Pathway immediately, if needed
  if CurPathway <> nil then
  begin
    if sx > 0 then CurPathway[Tree[RootNode].X] := sX
              else CurPathway[Tree[RootNode].X] := pmResiX;
    if sy >0  then CurPathway[Tree[RootNode].Y] := sY
              else CurPathway[Tree[RootNode].Y] := pmResiX;
  end;

  //Now we evaluate this route further down
  if Tree[RootNode].X >= NoOfTaxa then EvaluatePathway(Tree[RootNode].X);
  if Tree[RootNode].Y >= NoOfTaxa then EvaluatePathway(Tree[RootNode].Y);
end;

// Generate and evalute a pathway to find MPRs
procedure TComputeParsimInfo.EvaluatePathway(Node: Integer);
begin
  if  RefreshCounter < 0 then
    Exit;

  Inc(RefreshCounter);
  if (RefreshCounter > 100) then
  begin
    RefreshCounter := 0;
    Application.ProcessMessages;
    if PleaseWait <> nil then
    begin
      if PleaseWait.IsCancel then
      begin
        if MessageDlg('Do you want to abort calculation of average branch lengths?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          RefreshCounter := -1;
          Exit;
        end;
      end;
    end;
  end;

  sX  := GetCurMprBase(Tree[Node].X);
  sY  := GetCurMprBase(Tree[Node].Y);
  sXY := GetCurMprBase(Node); // ancestor

  if (sX>0) and (sXY > 0) then
    if sXY <> sX  then   Inc(CurPathCost);

  if (sY>0) and (sXY > 0) then
    if sXY <> sY  then   Inc(CurPathCost);

  if (NoOfMpPaths > 0) and (CurPathCost > MpPathCost) then
    Exit;

  if CurPathMat <> nil then
  begin
    if (sX>0) and (sXY > 0) then CurPathMat[sXY*NoOfStates+sX] := CurPathMat[sXY*NoOfStates+sX] + 1; // from Par -> to Left
    if (sY>0) and (sXY > 0)  then CurPathMat[sXY*NoOfStates+sY] := CurPathMat[sXY*NoOfStates+sY] + 1; // from Par -> to Right
  end;

  if CurPathBLen <> nil then
  begin
    if (sX>0) and (sXY > 0) and (sX <> sXY) then
      CurPathBLen[Tree[Node].X] := CurPathBLen[Tree[Node].X]+1;
    if (sY>0) and (sXY > 0) and (sY <> sXY) then
      CurPathBLen[Tree[Node].Y] := CurPathBLen[Tree[Node].Y]+1;
  end;

  // We write the states to the Pathway immediately, if needed
  if CurPathway <> nil then
  begin
    if sx>0 then CurPathway[Tree[Node].X] := sX  // this even writes the extant states
            else CurPathway[Tree[Node].X] := pmResiX;
    if sy>0 then CurPathway[Tree[Node].Y] := sY  // this even writes the extant states
            else CurPathway[Tree[Node].Y] := pmResiX;
  end;

  //Now we evaluate this route further down
  if Tree[Node].X >= NoOfTaxa then EvaluatePathway(Tree[Node].X);
  if Tree[Node].Y >= NoOfTaxa then EvaluatePathway(Tree[Node].Y);
end;

// finds all equally expensive pathways
procedure TComputeParsimInfo.OutputPathway;
var
  i: Integer;
  TempMem: PArrayOfInt;
begin
  TempMem:= nil;
  try
    if (NoOfMpPaths=0) or (CurPathCost < MpPathCost) then
    begin
      if CurPathMat <> nil then  // for subst matrix
        for i:=0 to NoOfStates*NoOfStates-1 do
          AllPathsMat[i] := 0;

      if CurPathBLen <> nil then  // for branch length est
        for i:=0 to NoOfNodes-1 do
          AllPathsBLen[i] := 0;

      MpPathCost  := CurPathCost;
      NoOfMpPaths := 0; // must be done here
      // fall through so it can be done
    end;

    if CurPathCost > MpPathCost then
      Exit;

    if CurPathMat <> nil then  // for subst matrix
      for i:=0 to NoOfStates*NoOfStates-1 do
        AllPathsMat[i] := AllPathsMat[i]+CurPathMat[i];

    if CurPathBLen <> nil then  // for branch length est
      for i:=0 to NoOfNodes-1 do
        AllPathsBLen[i] := AllPathsBLen[i]+CurPathBLen[i];

    if MprPathsList <> nil then
    begin
      Getmem(TempMem, MaxNodeId*sizeOf(Integer));
      for i:=0 to MaxNodeId-1 do // leaves root out
        TempMem[i] := CurPathway[i]; //Integer(MapToChar(CurPathway[i]));
      MprPathsList.Add(TempMem);
      TempMem := nil;
    end;
    Inc(NoOfMpPaths);
  finally
    if TempMem <> nil then FreeMemAndNil(TempMem);
  end;
end;

// Gets the next Mpr base to try for the given node
function TComputeParsimInfo.GetCurMprBase(ANode: Integer): Integer;
begin
  Result := MprBaseLists[ANode][CurBasePos[ANode]];
end;

// Updates the indvBases array
// if JustIndex then it identifies bases by the index of the bit true
// else by the actual map
procedure TComputeParsimInfo.UpdateMprBaseLists(JustIndex: Boolean);
var
  MovingMask, i, x: Integer;
  TempList: TLongIntList;
  Index: Integer;
begin
  for i:=0 to MaxNodeId-1 do // no need to do it for the root
  begin
    TempList := MprBaseLists[i]; // for each node do it
    TempList.Clear;

    //if GetNoOfBases(CurSiteAnc[i]) AncJustIndex and IsLikelihood then
    x := CurSiteAnc[i];
    if NoOfStates = 4 then
    begin
      if x = Integer(pmBaseN) then  // this can happen only if a position and all desc are N
      begin
       if JustIndex then TempList.Add(-1)
       else              TempList.Add(0); // used in BLen estimation
       Continue;
      end;
    end
    else if NoOfStates = 20 then
    begin
      if x = pmResiX then        // this can happen only if a position and all desc are X
      begin
       if JustIndex then TempList.Add(-1)
       else              TempList.Add(0); // used in BLen estimation
       Continue;
      end;
    end
    else
      raise Exception.Create('Unable to handle the number of states');

    MovingMask := $00000001;
    Index := 0;
    while x <> 0 do
    begin
      if x and MovingMask <> 0 then
      begin
        if JustIndex then TempList.Add(Index)
        else              TempList.Add(x and MovingMask); // used in BLen estimation
        x := x and (not MovingMask); // clear that bit
      end;
      MovingMask := MovingMask shl 1;
      Inc(Index);
    end;
  end;
end;

{
THIS INFORMATION WILL BE USED LATER

      // for bayesian approach
//    LklIterator    : TLikelihoodIterator; // only use as temporary over set of related fns
//    PathwayProb    : array of double;     // running prob for cur iteration of a pathway
//    LklAmbiguous   : Boolean;             // flags if ambiguity is treated by Lkl
//    LklArrayList   : PDistanceMatrix;     // An array of PDoubleArray for prob. of states at each node
//    PMat           : array of array of double; // for all PMats (transition prob matrices) on the tree
//    OnePMat        : array of double;     //To be fixed
//    PValues        : array of double;     //Final prob. for each ancestral state at a site in a node


//    procedure FillPMat(Node: Integer);
//    procedure InternalComputeAncStateProb; //computes prob. for each state for each node
//    procedure InternalComputePathwayProb(Node: Integer);

    //===== Extended functions
      // gamma para; returns ML value
//    function GetYangKumarGammaPara(GammaCats: Integer; var MeanL, VarL, mpG, ykG: Double; ASteps: TIntList): Double;
//    procedure ComputeOLSBLens(BLens: PArrayOfDouble);  by p-distance
      // substitution matrix
//    procedure ComputeSubstPattern(Pai: PArrayOfDouble; MatF, MatP, MatQ: PDistanceMatrix; Rates: PArrayOfDouble);
      //Get complete sequences for all nodes
//    procedure ComputeBayesianAncSeqs(AList: TList; UpperCutoff, LowerCutoff, MinCutoff: Double);
      //Reconstructing ancestral states at a given site
//    procedure ComputeBayesianAncStates(ASite: Integer; AList: TList); // list is an array of PDoubleArray of prob

// === All model based treatment is here
function TComputeParsimInfo.GetYangKumarGammaPara(GammaCats: Integer; var MeanL, VarL, mpG, ykG: Double; ASteps: TIntList): Double;
var
  AIntList: TIntList;
  i: Integer;
  finalML: Double;
  YKP    : TYangKumarPattern;
begin
  AIntList := nil;
  Result := 0;
  try
    AIntList  := TIntList.Create;
    GetMpGammaPara(MeanL, VarL, mpG, AIntList); // histogram out

    YKP := TYangKumarPattern.Create;
    YKP.NTaxa   := FNoOfTaxa;
    YKP.NStates := FNoOfStates;
    YKP.NGammaCats := GammaCats;
    YKP.AHist := AIntList;
    YKP.ComputeAlphaMP(ykG, FinalML);
    Result := -FinalML;
    if ASteps <> nil then
    begin
      ASteps.Clear;
      for i:=0 to AIntList.Count-1 do
       ASteps.Add(AIntList[i]);
    end;
  finally
    if AIntList <> nil then AIntList.Free;
    if YKP <>      nil then YKP.Free;
  end;
end;

// substitution matrix
procedure TComputeParsimInfo.ComputeSubstPattern(Pai: PArrayOfDouble; MatF, MatP, MatQ: PDistanceMatrix; Rates: PArrayOfDouble);
var
  YKP: TYangKumarPattern;
begin
  ComputeChangeMatrix(Pai, MatF);
  YKP := nil;
  try
    YKP := TYangKumarPattern.Create;
    YKP.NTaxa   := FNoOfTaxa;
    YKP.NStates := FNoOfStates;
    YKP.ComputeSubstPattern(MatF, MatP, MatQ, Rates);
  finally
    if YKP <> nil then YKP.Free;
  end;
end;

procedure TComputeParsimInfo.ComputeBayesianAncSeqs(AList: TList; UpperCutoff, LowerCutoff, MinCutoff: Double); // prob
var
  i,j, Des: Integer;
  TempPChar: PChar;
  DoubleArr: PArrayOfDouble;
begin
  // assumes memory is given
  // now get ancestral states
  TempPChar := nil;
  try
    // allocate memory for outputting results
    for i:=0 to 2*FNoOfTaxa-1 do
    begin
      GetMem(TempPChar, (FNoOfSites+1)*sizeOf(Char));
      TempPChar[FNoOfSites] := #0; // to ensure proper termination
      AList.Add(TempPChar);
      TempPChar := nil;
    end;

    // Place to hold ancestral states for equivocal paths
    GetMem(MprBaseLists, sizeOf(TIntList)*NoOfNodes);
    for i:= 0 to NoOfNodes-1 do
      MprBaseLists[i] := TIntList.Create;

    // space for prob values
    GetMem(LklArrayList, sizeOf(PArrayOfDouble)*NoOfNodes);
    for i:= 0 to NoOfNodes-1 do
      LklArrayList[i] := nil;
    for i:= 0 to NoOfNodes-1 do
    begin
      Getmem(DoubleArr, sizeOf(Double)*FNoOfStates);
      LklArrayList[i] := DoubleArr;
      DoubleArr := nil;
    end;

    // Setup PMat for each node for use
//    if Model = if (com.seqtype==AAseq && com.model==Poisson)
    GetMem(PMat, sizeOf(PArrayOfDouble)*NoOfNodes);
    for i:= 0 to NoOfNodes-1 do
      PMat[i] := nil;
    for i:= 0 to NoOfNodes-1 do
    begin
      Getmem(DoubleArr, sizeOf(Double)*FNoOfStates*FNoOfStates);
      PMat[i] := DoubleArr;
      DoubleArr := nil;
    end;

    // fills PMat for use
    for i:=FNoOfTaxa to MaxNodeId-1 do
    begin
      FillPMat(FTree[i].X);
      FillPMat(FTree[i].Y);
    end;

    // we have to make sure that the Mpr path lists have
    // In the external nodes: 0 .. n-1 for each perfect base and n for otherwise

    // get information and finish up
    LklAmbiguous := True;
    for i:=0 to FNoOfSites-1 do
    begin
      LoadCurSite(i);  // fills the above

        // all possible assignable states at each node by MP method
      BuildDownPass(RootNode);
      BuildUpPass(RootNode);
        // Partial likelihood
      UpdateMprBaseLists(True); // just index of states for use

      InternalComputeAncStateProb; //computes prob. for each state for each node

      // now we need to do an iteration of probability
      for j:=0 to  MaxNodeId-1 do  // extant sequences
      begin
        TempPChar := AList[j];
        TempPChar[i] := MapToChar(CurSiteConfig[j]);
      end;
    end;
  finally
    if TempPChar <> nil then FreeMem(TempPChar);
//    if MprBaseLists then
    LklAmbiguous := False;
  end;
end;

procedure TComputeParsimInfo.ComputeBayesianAncStates(ASite: Integer; AList: TList); // list contains array of PDoubleArray
var
  i,j, Des: Integer;
  TempPChar: PChar;
  DoubleArr: PArrayOfDouble;
begin
  TempPChar := nil;
  AList.Clear;
  try
    // Place to hold ancestral states for equivocal paths
    GetMem(MprBaseLists, sizeOf(TIntList)*NoOfNodes);
    for i:= 0 to NoOfNodes-1 do
      MprBaseLists[i] := TIntList.Create;

    // space for prob values
    GetMem(LklArrayList, sizeOf(PArrayOfDouble)*NoOfNodes);
    for i:= 0 to NoOfNodes-1 do
      LklArrayList[i] := nil;
    for i:= 0 to NoOfNodes-1 do
    begin
      Getmem(DoubleArr, sizeOf(Double)*FNoOfStates);
      LklArrayList[i] := DoubleArr;
      DoubleArr := nil;
    end;

    // Setup PMat for each node for use
//    if Model = if (com.seqtype==AAseq && com.model==Poisson)
    GetMem(PMat, sizeOf(PArrayOfDouble)*NoOfNodes);
    for i:= 0 to NoOfNodes-1 do
      PMat[i] := nil;
    for i:= 0 to NoOfNodes-1 do
    begin
      Getmem(DoubleArr, sizeOf(Double)*FNoOfStates*FNoOfStates);
      PMat[i] := DoubleArr;
      DoubleArr := nil;
    end;

    // fills PMat for use
    for i:=FNoOfTaxa to MaxNodeId-1 do
    begin
      FillPMat(FTree[i].X);
      FillPMat(FTree[i].Y);
    end;

    // we have to make sure that the Mpr path lists have
    // In the external nodes: 0 .. n-1 for each perfect base and n for otherwise

    // get information and finish up
    LoadCurSite(ASite);  // fills the above

    // all possible assignable states at each node by MP method
    BuildDownPass(RootNode);
    BuildUpPass(RootNode);

    // Update the list of all plausible states at each site
    LklAmbiguous := True;
    UpdateMprBaseLists(True); // just index of states for use

    //computes prob. for each state for each node
    InternalComputeAncStateProb;

    //for this site we have MaxId nodes and prob for each case
    //plan: save all states with prob. of 5% or more to the output

    // now we need to do an iteration of probability
    for j:=FNoOfTaxa to  MaxNodeId-1 do  // extant sequences
    begin
      AList.Add(LklArrayList[j]);
      LklArrayList[j] := nil;
    end;
  finally
    if TempPChar <> nil then FreeMem(TempPChar);
//    if MprBaseLists then
    LklAmbiguous := False;
  end;
end;

procedure TComputeParsimInfo.FillPMat(Node: Integer);
var
  AMat: PArrayOfDouble;
  t: Double;
  i, n: Integer;
  pii, pij: Double;
begin
  AMat := PMat[Node];
  t    := FGivenBLens[Node];
  n    := FNoOfStates;
  // this is simple poisson model
  pii := 1.0/n + (1.0-1.0/n)*exp(-n/(n-1.0)*t);
  pij := (1.0-pii)/(n-1.0);
  for i:=0 to (n*n)-1 do
    AMat[i]:=pij;
  for i:=0 to n-1 do
    AMat[i*n+i]:=pii;
end;

// Computes probability for the all MP-based ancestral states
procedure TComputeParsimInfo.InternalComputeAncStateProb; //uses current loaded site
var
  i, j: Integer;
  AncLkl: PArrayOfDOuble;
begin
//  MpPathCost  := 0;
  for i:=0 to NoOfNodes-1 do
    CurBasePos[i] := 0;

  for i:=0 to NoOfNodes-1 do
    PathwayProb[i] := 1;

  while True do
  begin
    CurPathCost := 0;

    InternalComputePathwayProb(RootNode);

    // now update prob for different bases at different nodes
    // based on the current pathway
    for i:= FNoOfTaxa to MaxNodeId do
    begin
      AncLkl  := LklArrayList[i];
      AncLkl[CurPathway[i]] := AncLkl[CurPathway[i]] + PathwayProb[RootNode];
    end;

    i := MaxNodeId-1;  // since root has the MaxNodeId;
    while True do
    begin
      if i = 0 then  // a sign that all below the root are done
        break;
      if CurBasePos[i] <> (TIntList(MprBaseLists[i]).Count-1) then
      begin
        CurBasePos[i] := CurBasePos[i]+1;  // advance it
        break;
      end;
      // otherwise reset to be below nodes
      if i < MaxNodeId then
        CurBasePos[i] := 0;
      Dec(i);
    end; // end  of internal while True

    if i=0 then // a sign that all below the root are done
      break;
  end; // End  of External while (True);
end;

var
  icpp_DesX, icpp_DesY: Integer; // let it be put on stack
  icpp_p: Extended;
// Generate and evalute a pathway to find MPRs
procedure TComputeParsimInfo.InternalComputePathwayProb(Node: Integer);
begin
  icpp_DesX := FTree[Node].X;
  icpp_DesY := FTree[Node].Y;

  sX  := GetCurMprBase(icpp_DesX);
  sY  := GetCurMprBase(icpp_DesY);
  sXY := GetCurMprBase(Node); // ancestor

  // We write the states to the Pathway immediately, if needed
  CurPathway[icpp_DesX] := sX;  // this even writes the extant states
  CurPathway[icpp_DesY] := sY;  // this even writes the extant states

  if icpp_DesX >= FNoOfTaxa then
    PathwayProb[Node] := PathwayProb[icpp_DesX]*PMat[icpp_DesX][sXY*FNoOfStates+ sX];

  if icpp_DesY >= FNoOfTaxa then
    PathwayProb[Node] := PathwayProb[icpp_DesY]*PMat[icpp_DesY][sXY*FNoOfStates+ sY];

  //Now we evaluate this route further down
  if icpp_DesX >= FNoOfTaxa then InternalComputePathwayProb(icpp_DesX);
  if icpp_DesY >= FNoOfTaxa then InternalComputePathwayProb(icpp_DesY);
end;

// Iterate over only MPR bases
// For ambiguous/gap data, I consider them as additional state
var
  pl_isLeft, pl_i, pl_j, pl_k, pl_ison, pl_n: Integer;
  pl_anc, pl_des: Integer;
  pl_t : Double;
  pl_Anclist, pl_DesList : TIntList;
  pl_PMat: PArrayofDouble;
  pl_AncLkl, pl_DesLkl: PArrayOfDouble;
  pl_fillWith: Double;

function TComputeParsimInfo.PartialLikelihood(inode: Integer): Integer;
begin
  if FTree[inode].X >= FNoOfTaxa then PartialLikelihood(FTree[inode].X);
  if FTree[inode].Y >= FNoOfTaxa then PartialLikelihood(FTree[inode].Y);

  pl_n := FNoOfStates;

  // ancestral attributes
  pl_AncLkl  := LklArrayList[inode];
  pl_AncList := MprBaseLists[inode];
  pl_PMat    := PMat[inode];

  //initialize all prob for all possible states
  if inode < FNoOfTaxa then pl_fillWith := 0
                       else pl_fillWith := 1;
  for pl_j:=0 to pl_n-1 do
    pl_AncLkl[pl_j] := pl_fillWith;

  // first initialize all space for unambiguous states
  if inode < FNoOfTaxa then // extant Node
  begin
    pl_Anc := pl_AncList[0];
    if pl_Anc < pl_n then
      pl_AncLkl[pl_Anc] := 1;  // only observed state has non-zero prob
    Exit;
  end;

  // we fall here only if an internal node is encountered
  for pl_IsLeft:=0 to 1 do  // As all trees are bifurcating so only two states
  begin
    if pl_IsLeft = 0 then pl_ison := FTree[inode].X
      else                pl_ison := FTree[inode].Y;

    pl_DesList := MprBaseLists[pl_ison];
    pl_DesLkl  := LklArrayList[pl_ison];

    if pl_ison < FNoOfTaxa then  // for external branch
      for pl_j:=0 to pl_AncList.Count-1 do
      begin
        pl_anc := pl_AncList[pl_j];
        pl_des := pl_DesList[0];  // must be 0..n-1 if perfect else n
        if (pl_anc < pl_n) and (pl_des < pl_n) then
          pl_AncLkl[pl_j] := pl_AncLkl[pl_j]*pl_PMat[pl_anc*pl_n+pl_des];
      end
    else                         // internal branch
      for pl_j:=0 to pl_AncList.Count-1 do
      begin
        pl_t   := 0;
        pl_anc := pl_ancList[pl_j];
        if pl_anc >= pl_n then
          continue;
        for pl_k:=0 to pl_desList.Count-1 do
        begin
          pl_des := pl_DesList[pl_k];
          if pl_des < pl_n then
            pl_t   := pl_t + pl_PMat[pl_anc*pl_n+pl_des]*pl_DesLkl[pl_des];
        end;
        pl_AncLkl[pl_anc] := pl_t*pl_AncLkl[pl_anc];
      end;
  end;
end;


procedure TComputeParsimInfo.ComputeJTTPt(kappa: Double);
    // if AQt is non nil then user specific matrix
procedure TComputeParsimInfo.ComputeUserSpecificPt(APi, AQdt: PArrayOfDouble);
var
  ATemp: PArrayOfDouble;
  i: Integer;
  t: Double;

  procedure UpdatePt(APt: PArrayOfDouble);
  begin
    if
  end;

begin
  // must have branch lengths setup already
  try
    if FPt = nil then
    begin
      FPt := TList.Create;
      for i:=0 to MaxNodeId-1 do
      begin
        GetMem(ATemp, FNoOfStates*FNoOfStates*sizeOf(Double));
        FPt.Add(ATemp);
        ATemp := nil;
      end;
    end;
    for i:=FNoOfTaxa to MaxNodeId-1 do
    begin
      t := FGivenBLens[FTree[i].X]; // branch length to x
      UpdatePt(FPt[FTree[i].X]);

      t := FGivenBLens[FTree[i].Y]; // branch length to x
      UpdatePt(FPt[FTree[i].Y]);
    end;
    // MaxNodeId is the root
    t := FGivenBLens[FTree[RootNode].X] + FGivenBLens[FTree[RootNode].Y]; // branch length to x
    UpdatePt(FPt[FTree[RootNode].X]);
    UpdatePt(FPt[FTree[RootNode].Y]);
  finally
    if ATemp <> nil then FreeMem(ATemp);
  end;
end;


procedure TComputeParsimInfo.ComputeKimuraPt(kappa: Double);
var
  ATemp: PArrayOfDouble;
  i: Integer;
  t: Double;
  LklEst : TLikelihoodIterator;

  procedure UpdatePt(APt: PArrayOfDouble);
  begin
     LklEst.PMatK80(APt, t, kappa);
  end;
begin
  // must have branch lengths setup already
  LklEst := nil;
  try
    LklEst := TLikelihoodIterator.Create;
    if FPt = nil then
    begin
      FPt := TList.Create;
      for i:=0 to MaxNodeId-1 do
      begin
        GetMem(ATemp, FNoOfStates*FNoOfStates*sizeOf(Double));
        FPt.Add(ATemp);
        ATemp := nil;
      end;
    end;
    for i:=FNoOfTaxa to MaxNodeId-1 do
    begin
      t := FGivenBLens[FTree[i].X]; // branch length to x
      UpdatePt(FPt[FTree[i].X]);

      t := FGivenBLens[FTree[i].Y]; // branch length to x
      UpdatePt(FPt[FTree[i].Y]);
    end;
    // MaxNodeId is the root
    t := FGivenBLens[FTree[RootNode].X] + FGivenBLens[FTree[RootNode].Y]; // branch length to x
    UpdatePt(FPt[FTree[RootNode].X]);
    UpdatePt(FPt[FTree[RootNode].Y]);
  finally
    if ATemp <> nil then FreeMem(ATemp);
    if LklEst <> nil then LklEst.Free;
  end;
end;
}

end.



