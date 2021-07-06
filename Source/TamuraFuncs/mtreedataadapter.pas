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

unit MTreeDataAdapter;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MTreeData, MSimpleTreeNode, MTreeBox, MTreeProc,
  MegaConsts, MTreeList;

const
  REMOVED_FLAG = 99;

type
  { given a TTreeData object, provides a linked-tree interface that is easier to
    work with and then updates the TTreeData object as needed}

  { TSimpleTreeDataAdapter }

  TSimpleTreeDataAdapter = class(TObject)
  private
    FIsStats: Boolean;
    protected
      FList: TList;
      FNodes: TSimpleTreeNodeArray;
      FNumNodes: Integer;
      FNumTaxa: Integer;
      FRoot: TSimpleTreeNode;
      FEpRefSeq: TSimpleTreeNode;
      FValue: Double;
      procedure GetOtuNames(var aList: TStringList);
      function GetOtuNode(seqName: String): TSimpleTreeNode;
      function RemoveNode(n: TSimpleTreeNode): Integer;
      procedure MoveRoot(newRoot: TSimpleTreeNode);
      function CanPruneRefSeqSibling: Boolean;
      function PruneRefSeqSibling: Boolean;
      function PruneOneMore: Boolean;
      function ValidateAncestralRelationships(var BadCondition: String): Boolean;
      procedure InitNodes;
      procedure SetNumTaxa(AValue: Integer);
      procedure ClearNodes;
      procedure FindOutgroupMembers; { finds the subtree from the root with the fewest taxa and marks them as outgroup taxa, if a tie, select des2}
      procedure MarkOutgroupMembers;
      function PairwiseDist(mrca, node1, node2: Integer; IsLinearized: Boolean): Extended;
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetNodeSizes(aNode: TSimpleTreeNode); { call this by passing FRoot as aNode}
      function SumOfBlens: Double;
      procedure SetNodes(ANodes: TSimpleTreeNodeArray);
      procedure SetTreeData(const AData: TTreeData; IsRooted: Boolean); overload;
      procedure SetTreeData(const AData: TTreeData; IsRooted: Boolean; OtuNames: TStringList); overload;
      procedure GetTreeData(AData: TTreeData);
      function GetOtuIndex(OtuName: String): Integer;
      function RemoveLeafNodes(taxaNames: TStringList): Boolean;
      function FindDivergenceTimes: Boolean; { traverse the tree and set TSimpleTreeNode := 0 for all internal nodes, fails if tree is not linearized}
      function GetPrunedEpTreeList(var NewickStrings: TStringList; var NamesLists: TList): TList;
      procedure GuessOutgroup(var AData: TTreeData);
      procedure ExpandOutgroupFromOutgroupTaxa;
      function NumIngroupNodes: Integer;
      procedure CountIngroupAndOutgroupNodes(var numInOutgroup: Integer; var numInIngroup: Integer);
      function GetSimpleTreeReference: TSimpleTreeNodeArray; { TSimpleTreeDataAdapter owns it and will free the memory used for it. Other classes should only reference it}
      function GetMrcaMatrix: T2DArrayOfInteger;
      function GetPairwiseDistances(IsLinearized: Boolean): T2dArrayOfExtended; overload;
      function GetPairwiseDistances(aData: TTreeData; IsRooted: Boolean; IsLinearized: Boolean): PDistanceMatrix; overload;
      function DebugStrings: TStringList;
      property NumTaxa: Integer read FNumTaxa write SetNumTaxa;
      property NumNodes: Integer read FNumNodes;
      property Root: TSimpleTreeNode read FRoot;
      property Nodes: TSimpleTreeNodeArray read FNodes;
      property IsStats: Boolean read FIsStats;
  end;

  { TFpNodeTreeDataAdapter }

  TFpNodeTreeDataAdapter = class(TObject)
    private
      FNodes: ^TpNodeArray;
      FNumNodes: Integer;
      FNumTaxa: Integer;
      FRoot: TpNode;
      FIsBranchLength: Boolean;
      FIsSE: Boolean;
      FIsStats: Boolean;

      procedure InitTpNodes;
      procedure SetNumTaxa(AValue: Integer);
      procedure TurnNode(p : TpNode);
      procedure MarkOutgroupOtusAsFlagged;
      procedure MarkOutgroupClusterFromOutgroupRoot(outgroupRoot: TpNode);
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetTreeData(const AData: TTreeData);
      procedure GetTreeData(var AData: TTreeData);
      procedure RootOnOutgroup;
      procedure ExpandOutgroupFromOutgroupTaxa;
      procedure CountIngroupAndOutgroupNodes(var ingroupCount: Integer; var outgroupCount: Integer);
      property NumTaxa: Integer read FNumTaxa write SetNumTaxa;
      property NumNodes: Integer read FNumNodes;
      function GetTreeBoxTreeReference: TpNodeArray; { TSimpleTreeDataAdapter owns it and will free the memory used for it. Other classes should only reference it}

  end;

procedure RootTreeDataOnOutgroup(var aData: TTreeData);

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ENDIF}
  MegaUtils, Math, MGlobalSettings;

procedure RootTreeDataOnOutgroup(var aData: TTreeData);
var
  Rooter: TFpNodeTreeDataAdapter;
begin
  Rooter := nil;
  try
    Rooter := TFpNodeTreeDataAdapter.Create;
    Rooter.SetTreeData(aData);
    Rooter.RootOnOutgroup;
    Rooter.GetTreeData(aData);
  finally
    if Assigned(Rooter) then
      Rooter.Free;
  end;
end;

{ TFpNodeTreeDataAdapter }

procedure TFpNodeTreeDataAdapter.InitTpNodes;
var
  i: Integer;
begin
  Assert((NumNodes > 0) and (NumTaxa > 0));
  GetMem(FNodes,SizeOf(TpNode)*NumNodes);
  for i := 1 to NumTaxa do
  begin
      New(FNodes[i]);
      FNodes[i].index:= i;
      FNodes[i].minOTU := i;
      FNodes[i].size := 1;
      FNodes[i].depth := 0;
      FNodes[i].height := 0.0;
      FNodes[i].maxh := 0;
      FNodes[i].minh := 0;
      FNodes[i].rate := 0;
      FNodes[i].width := 0;
      FNodes[i].anc := nil;
      FNodes[i].des1 := nil;
      FNodes[i].des2 := nil;
      FNodes[i].OTU := true;
      FNodes[i].compressed := false;
      FNodes[i].marker.shape := msNone;
      FNodes[i].outgroup := false;
      FNodes[i].hidden := false;
      FNodes[i].flag := false;
      FNodes[i].hilighted := false;
      FNodes[i].emphasized := false;
      FNodes[i].angle := 0.0;
      FNodes[i].branch.length := 0.0;
      FNodes[i].branch.SE := 0.0;
      FNodes[i].branch.maxlen1 := 0.0;
      FNodes[i].branch.maxlen2 := 0.0;
      FNodes[i].branch.bootnum := 0;
      FNodes[i].branch.stats := 0.0;
      FNodes[i].branch.stat2 := 0;
      FNodes[i].attrindex := 0;
      FNodes[i].namesize.x := 0;
      FNodes[i].namesize.y := 0;
      FNodes[i].groupindex := -1;
      FNodes[i].capdepth   := 0;
      FNodes[i].bracket.Left   := 0;
      FNodes[i].bracket.Right  := 0;
      FNodes[i].bracket.Top    := 0;
      FNodes[i].bracket.Bottom := 0;
      FNodes[i].IsGeneDuplication := False;
      FNodes[i].IsSpeciationEvent := False;
      FNodes[i].DataCoverage := 0.0;
  end;
  for i := NumTaxa + 1 to NumNodes do
  begin
      NEW(FNodes[i]);
      FNodes[i].index:= i;
      FNodes[i].size := 0;
      FNodes[i].depth := 0;
      FNodes[i].height := 0.0;
      FNodes[i].maxh := 0;
      FNodes[i].minh := 0;
      FNodes[i].rate := 0;
      FNodes[i].width := 0;
      FNodes[i].anc := nil;
      FNodes[i].des1 := nil;
      FNodes[i].des2 := nil;
      FNodes[i].name := '';
      FNodes[i].SpeciesName := EmptyStr;
      FNodes[i].PrivateName := '';
      FNodes[i].OTU := false;
      FNodes[i].compressed := false;
      FNodes[i].marker.shape := msNone;
      FNodes[i].outgroup := false;
      FNodes[i].hidden := false;
      FNodes[i].flag := false;
      FNodes[i].hilighted := false;
      FNodes[i].emphasized := false;
      FNodes[i].angle := 0.0;
      FNodes[i].branch.length := 0.0;
      FNodes[i].branch.SE := 0.0;
      FNodes[i].branch.maxlen1 := 0.0;
      FNodes[i].branch.maxlen2 := 0.0;
      FNodes[i].branch.bootnum := 0;
      FNodes[i].branch.stats := 0.0;
      FNodes[i].branch.stat2 := 0;
      FNodes[i].attrindex := 0;
      FNodes[i].namesize.x := 0;
      FNodes[i].namesize.y := 0;
      FNodes[i].groupindex := -1;
      FNodes[i].capdepth   := 0;
      FNodes[i].bracket.Left   := 0;
      FNodes[i].bracket.Right  := 0;
      FNodes[i].bracket.Top    := 0;
      FNodes[i].bracket.Bottom := 0;
      FNodes[i].IsGeneDuplication := False;
      FNodes[i].IsSpeciationEvent := False;
      FNodes[i].DataCoverage := 0.0;
  end;
  FRoot := nil;
end;

procedure TFpNodeTreeDataAdapter.SetNumTaxa(AValue: Integer);
begin
  if FNumTaxa=AValue then
    Exit;
  FNumTaxa := AValue;
  FNumNodes := 2 * FNumTaxa - 1;
end;

procedure TFpNodeTreeDataAdapter.TurnNode(p: TpNode);
begin
  if not p.OTU then
  begin
    SwapNode(p.des1,p.des2);
    TurnNode(p.des1);
    TurnNode(p.des2);
  end;
end;

procedure TFpNodeTreeDataAdapter.MarkOutgroupOtusAsFlagged;
var
  i: Integer;
begin
  for i := 1 to FNumTaxa do
    if FNodes[i].outgroup then
      FNodes[i].flag := True
    else
      FNodes[i].flag := False;
end;

procedure TFpNodeTreeDataAdapter.MarkOutgroupClusterFromOutgroupRoot(outgroupRoot: TpNode);
begin
  outgroupRoot.outgroup := True;
  if Assigned(outgroupRoot.des1) then
    MarkOutgroupClusterFromOutgroupRoot(outgroupRoot.des1);
  if Assigned(outgroupRoot.des2) then
    MarkOutgroupClusterFromOutgroupRoot(outgroupRoot.des2);
end;

constructor TFpNodeTreeDataAdapter.Create;
begin
  FIsBranchLength := False;
  FIsSE := False;
  FIsStats := False;
end;

destructor TFpNodeTreeDataAdapter.Destroy;
var
  i: Integer;
begin
  if NumTaxa = 0 then
    Exit;
  if FNodes <> nil then
    for i := 1 to NumNodes do
      Dispose(FNodes[i]);
  FreeMemAndNil(FNodes,SizeOf(TpNode)*NumNodes);
  FNodes := nil;
  inherited Destroy;
end;

procedure TFpNodeTreeDataAdapter.SetTreeData(const AData: TTreeData);
var
  i,j,k : integer;
begin
  NumTaxa := AData.NoOfOtus;
  InitTpNodes;
  for i := 1 to NumNodes do
  begin
      FNodes[i].anc := nil;
      FNodes[i].compressed := false;
      FNodes[i].hidden := false;
      FNodes[i].flag := false;
  end;

  for i := 1 to NumTaxa do { mark the taxa that belong to the outgroup}
  begin
    FNodes[i].outgroup := AData.IsOutgroupMember[i - 1];
    FNodes[i].OTU := True;
  end;

  for i := 1 to NumTaxa - 1 do
  begin
      j := AData.NodeArray[i-1].des1+1;
      k := AData.NodeArray[i-1].des2+1;
      FNodes[NumTaxa+i].des1 := FNodes[j];
      FNodes[NumTaxa+i].des2 := FNodes[k];
      FNodes[j].anc := FNodes[NumTaxa + i];
      FNodes[k].anc := FNodes[NumTaxa + i];
      FNodes[NumTaxa+i].name := EmptyStr;
      FNodes[NumTaxa+i].charstate := EmptyStr;
  end;

  for i := NumNodes downto NumTaxa + 1 do
    if FNodes[i].anc = nil then
    begin
        FRoot := FNodes[i];
        Break;
    end;
  for i  := NumNodes downto NumTaxa + 1 do
    FNodes[i].dataCoverage := AData.DataCoverage[i - NumTaxa - 1];
  if AData.isBLen then
  begin
    FIsBranchLength := True;
    for i := 1 to NumNodes do
    begin
      if FNodes[i] = FRoot then
        Continue;
      FNodes[i].branch.length := AData.BLen[i-1];
    end;
  end
  else
    FIsBranchLength := False;

  if AData.isSE then
  begin
    FIsSE := True;
    for i := 1 to NumNodes do
    begin
        if FNodes[i] = FRoot then
          Continue;
        FNodes[i].branch.SE := AData.SE[i-1];
    end;
  end
  else
    FIsSE := False;

  if AData.isStats then
  begin
    FIsStats := True;
    for i := 1 to NumNodes do
    begin
      if FNodes[i] = FRoot then
        Continue;
      FNodes[i].branch.stats := AData.Stats[i-1];
    end;
  end;

  for i := (NumTaxa + 1) to NumNodes do
  begin
    FNodes[i].IsGeneDuplication := AData.IsGeneDupEvent[i-1];
    FNodes[i].IsSpeciationEvent := AData.IsSpeciationEvent[i-1];
  end;
end;

procedure TFpNodeTreeDataAdapter.GetTreeData(var AData: TTreeData);
var
  i: integer;
begin
  AData.NoOfOTUs := NumTaxa;
  AData.isBLen := FIsBranchLength;
  AData.isSE := FIsSE;
  AData.isStats := FIsStats;
  for i := 0 to NumTaxa - 2 do begin
    AData.NodeArray[i].des1 := FNodes[NumTaxa+i+1].des1.index-1;
    AData.NodeArray[i].des2 := FNodes[NumTaxa+i+1].des2.index-1;
  end;
  for i := 1 to NumTaxa do
    AData.IsOutgroupMember[i-1] := FNodes[i].outgroup;

  for i := 0 to NumNodes-2 do
  begin
    if AData.isBLen then
      AData.BLenArray[i] := FNodes[i+1].branch.length;
    if AData.isSE then
      AData.SE[i] := FNodes[i+1].branch.SE;
    if AData.isStats then
      AData.Stats[i] := FNodes[i+1].branch.stats;
  end;
end;

procedure TFpNodeTreeDataAdapter.RootOnOutgroup;
var
  NewRoot : TpNode;
  i : integer;
begin
  SetClusterSize(FRoot);
  for i := 1 to NumTaxa do
    FNodes[i].flag := FNodes[i].outgroup;
  NewRoot := SearchCommonAncestor(FRoot);
  if NewRoot = FRoot then
    Exit;
  ChangeRoot(FRoot, NewRoot, true);
  SetClusterSize(FRoot);
  if FRoot.des1.flag then
      TurnNode(FRoot);
end;

procedure TFpNodeTreeDataAdapter.ExpandOutgroupFromOutgroupTaxa;
var
  outgroupRoot: TpNode = nil;
begin
  MarkOutgroupOtusAsFlagged;
  outgroupRoot := FindCommonAncestorOfFlaggedTaxa(FRoot);
  MarkOutgroupClusterFromOutgroupRoot(outgroupRoot);
end;

procedure TFpNodeTreeDataAdapter.CountIngroupAndOutgroupNodes(var ingroupCount: Integer; var outgroupCount: Integer);

  procedure PreorderTraversal(n: TpNode);
  begin
    if Assigned(n.des1) then
      PreorderTraversal(n.des1);
    if Assigned(n.des2) then
      PreorderTraversal(n.des2);
    if n.outgroup then
      inc(outgroupCount)
    else
      inc(ingroupCount);
  end;

begin
  ingroupCount := 0;
  outgroupCount := 0;
  PreorderTraversal(FRoot.des1);
  PreorderTraversal(FRoot.des2);
end;

function TFpNodeTreeDataAdapter.GetTreeBoxTreeReference: TpNodeArray;
begin
  Result := FNodes^;
end;

{ TSimpleTreeDataAdapter }

procedure TSimpleTreeDataAdapter.InitNodes;
var
  i: Integer;
begin
  Assert((NumTaxa > 0), 'There are no taxa! Did you forget to set NumTaxa?');
  ClearNodes;
  SetLength(FNodes, NumNodes);
  for i := 0 to NumNodes - 1 do
  begin
    FNodes[i] := TSimpleTreeNode.Create;
    FNodes[i].NodeIndex := i;
  end;
end;

procedure TSimpleTreeDataAdapter.SetNumTaxa(AValue: Integer);
begin
  if FNumTaxa=AValue then
    Exit;
  FNumTaxa := AValue;
  FNumNodes := 2 * FNumTaxa - 1;
end;

procedure TSimpleTreeDataAdapter.SetTreeData(const AData: TTreeData; IsRooted: Boolean; OtuNames: TStringList);
var
  i: Integer;
begin
  SetTreeData(AData, IsRooted);
  if OtuNames.Count > 0 then
    for i := 0 to OtuNames.Count - 1 do
      FNodes[i].SequenceName := OtuNames[i];
end;

procedure TSimpleTreeDataAdapter.ClearNodes;
var
  i: Integer;
begin
  if Length(FNodes) > 0 then
  begin
    for i := 0 to Length(FNodes) - 1 do
      if Assigned(FNodes[i]) then
        FreeAndNil(FNodes[i]);
    SetLength(FNodes, 0);
  end;
end;

function TSimpleTreeDataAdapter.SumOfBlens: Double;
var
  i: Integer;
begin
  Result := 0.0;
  if NumNodes > 0 then
    for i := 0 to NumNodes - 1 do
      Result := Result + FNodes[i].Blen;
end;

procedure TSimpleTreeDataAdapter.GetOtuNames(var aList: TStringList);
var
  i: Integer;
begin
  aList.Clear;
  for i := 0 to NumTaxa - 1 do
    aList.Add(FNodes[i].SequenceName);
end;

function TSimpleTreeDataAdapter.GetOtuNode(seqName: String): TSimpleTreeNode;
var
  i: Integer;
begin
  Result := nil;
  if NumTaxa > 0 then
    for i := 0 to NumTaxa - 1 do
      if FNodes[i].SequenceName = seqName then
      begin
        Result := FNodes[i];
        Exit;
      end;
end;

function TSimpleTreeDataAdapter.RemoveNode(n: TSimpleTreeNode): Integer;
var
  aNode: TSimpleTreeNode;
  newNumNodes: Integer;
  temp: TSimpleTreeNodeArray;
  i: Integer;
  index: Integer;

  procedure ProcessNode(n1: TSimpleTreeNode);
  begin
    if n1.IsOtu then
      Exit;

    if Assigned(n1.Des1) then
    begin
      FList.Add(n1.Des1);
      FNodes[n1.Des1.NodeIndex].Value := REMOVED_FLAG;
      if n1.Des1.IsOtu then
        inc(Result);
    end;
    if Assigned(n1.Des2) then
    begin
      FList.Add(n1.Des2);
      FNodes[n1.Des2.NodeIndex].Value := REMOVED_FLAG;
      if n1.Des2.IsOtu then
        inc(Result);
    end;

    if Assigned(n1.Des1) and (not n1.Des1.IsOtu) then
      ProcessNode(n1.Des1);
    if Assigned(n1.Des2) and (not n1.Des2.IsOtu) then
      ProcessNode(n1.Des2);
  end;

begin
  Result := 0;
  if n.IsOtu then
    inc(Result);
  FList.Add(n);
  FNodes[n.NodeIndex].Value := REMOVED_FLAG;
  ProcessNode(n);
  newNumNodes := NumNodes - FList.Count;
  setLength(temp, newNumNodes);
  index := 0;
  for i := 0 to FNumNodes - 1 do
    if CompareValue(FNodes[i].Value, REMOVED_FLAG, 0.001) = 0 then
      continue
    else
    begin
      temp[index] := FNodes[i];
      temp[index].NodeIndex := index;
      inc(index);
    end;
  NumTaxa := NumTaxa - Result;
  for i := 0 to FList.Count - 1 do
  begin
    aNode := TSimpleTreeNode(FList[i]);
    FreeAndNil(aNode);
  end;
  SetLength(FNodes, 0);
  FNodes := temp;
  FList.Clear;
end;

procedure TSimpleTreeDataAdapter.MoveRoot(newRoot: TSimpleTreeNode);
var
  i, index: Integer;
  n: TSimpleTreeNode;
  newNumNodes: Integer;
  temp: TSimpleTreeNodeArray;
begin
  if newRoot = FRoot.Des1 then
    n := FRoot.Des2
  else
    n := FRoot.Des1;

  FList.Add(n);
  FList.Add(FRoot);
  FNodes[n.NodeIndex].Value := REMOVED_FLAG;
  FRoot.Value := REMOVED_FLAG;
  FRoot := newRoot;
  FRoot.BLen := 0.0;
  newNumNodes := NumNodes - FList.Count;
  setLength(temp, newNumNodes);
  index := 0;
  for i := 0 to FNumNodes - 1 do
    if CompareValue(FNodes[i].Value, REMOVED_FLAG, 0.001) = 0 then
      continue
    else
    begin
      temp[index] := FNodes[i];
      temp[index].NodeIndex := index;
      inc(index);
    end;

  for i := 0 to FList.Count - 1 do
  begin
    n := TSimpleTreeNode(FList[i]);
    if n.IsOtu then
      NumTaxa := NumTaxa - 1;
    FreeAndNil(n);
  end;
  SetLength(FNodes, 0);
  FNodes := temp;
  FList.Clear;
end;

function TSimpleTreeDataAdapter.CanPruneRefSeqSibling: Boolean;
begin
  Result := False;
  if (not Assigned(FEpRefSeq.Ancestor)) or (not Assigned(FEpRefSeq.Ancestor.Ancestor)) then
    Exit;
  Result := (FEpRefSeq.Ancestor <> FRoot) and (FEpRefSeq.Ancestor.Ancestor <> FRoot);
end;

function TSimpleTreeDataAdapter.PruneRefSeqSibling: Boolean;
var
  a, aa: TSimpleTreeNode;
begin
  Assert(FEpRefSeq.Ancestor <> FRoot);
  Result := (Assigned(FEpRefSeq.Ancestor) and Assigned(FEpRefSeq.Ancestor.Ancestor));
  if not Result then
    Exit;
  a := FEpRefSeq.Ancestor;
  aa := a.Ancestor;
  FEpRefSeq.Ancestor := aa;
  if aa.Des1 = a then
    aa.Des1 := FEpRefSeq
  else
    aa.Des2 := FEpRefSeq;
  if a.Des1 = FEpRefSeq then
    a.Des1 := nil
  else
    a.Des2 := nil;
  FEpRefSeq.BLen := FEpRefSeq.BLen + a.BLen;
  RemoveNode(a);
end;

function TSimpleTreeDataAdapter.PruneOneMore: Boolean;
var
  a, aa, s: TSimpleTreeNode;
  target: TSimpleTreeNode;
begin
  Result := False;
  a := FEpRefSeq.Ancestor;
  if FEpRefSeq = a.Des1 then
    target := a.Des2
  else
    target := a.Des1;
  if Assigned(target.Des1) then
  begin
    Result := True;
    while Assigned(target.Des1) do
      target := target.Des1;
  end
  else if Assigned(target.Des2) then
  begin
    Result := True;
    while Assigned(target.Des1) do
      target := target.Des2;
  end;
  a := target.Ancestor;
  aa := a.Ancestor;
  if target = a.Des1 then
    s := a.Des2
  else
    s := a.Des1;
  s.Ancestor := aa;
  if a = aa.Des1 then
  begin
    aa.Des1 := s;
  end
  else
  begin
    aa.Des2 := s;
  end;
  if a.Des1 = target then
    a.Des2 := nil
  else
    a.Des1 := nil;
  RemoveNode(a);
end;

function TSimpleTreeDataAdapter.ValidateAncestralRelationships(var BadCondition: String): Boolean;
var
  i: Integer;
  aNode: TSimpleTreeNode;
begin
  Result := False;
  BadCondition :=  EmptyStr;
  if Length(FNodes) > 0 then
  begin
    for i := 0 to  Length(FNodes) - 1 do
    begin
      aNode := FNodes[i];
      if aNode.IsOtu then
      begin
        if not Assigned(aNode.Ancestor) then
        begin
          BadCondition := 'Bad Condition: No ancestor on OTU.';
          Exit;
        end;
        if Assigned(aNode.Des1) or Assigned(aNode.Des2) then
        begin
          BadCondition := 'Bad Condition: Descendant exists on OTU.';
          Exit;
        end;
      end
      else if aNode = FRoot then
      begin
        if Assigned(aNode.Ancestor) then
        begin
          BadCondition := 'Bad Condition: Ancestor exists on root node.';
          Exit;
        end;
        if (not Assigned(aNode.Des1)) or (not Assigned(aNode.Des2)) then
        begin
          BadCondition := 'Bad Condition: No descendant on root node.';
          Exit;
        end;
      end
      else
      begin
        if (not Assigned(aNode.Ancestor)) or (not Assigned(aNode.Des1)) or (not Assigned(aNode.Des2)) then
        begin
          BadCondition := 'Bad Condition: No descendant or ancestor on internal node';
          Exit;
        end;
      end;
    end;
  end;
  Result := True;
end;

procedure TSimpleTreeDataAdapter.FindOutgroupMembers;
var
  des1Count, des2Count: LongInt;

  function CountTaxaRecursive(ANode: TSimpleTreeNode): Integer;
  begin
    Result := 0;
    if ANode.IsOtu then
    begin
      Result := 1;
      Exit;
    end;

    if Assigned(ANode.Des1) then
      Result := Result + CountTaxaRecursive(ANode.Des1);
    if Assigned(ANode.Des2) then
      Result := Result + CountTaxaRecursive(ANode.Des2);
  end;

  procedure MarkTaxaRecursive(ANode: TSimpleTreeNode);
  begin
    if ANode.IsOtu then
    begin
      ANode.IsOutgroupMember := True;
      Exit;
    end;

    if Assigned(ANode.Des1) then
      MarkTaxaRecursive(ANode.Des1);
    if Assigned(ANode.Des2) then
      MarkTaxaRecursive(ANode.Des2);
  end;

begin
  Assert(Assigned(FRoot));
  des1Count := CountTaxaRecursive(FRoot.Des1);
  des2Count := CountTaxaRecursive(FRoot.Des2);
  if des1Count < des2Count then
    MarkTaxaRecursive(FRoot.Des1)
  else
    MarkTaxaRecursive(FRoot.Des2); { mark the des2 lineage if its taxa count >= to that of the des1 lineage}
end;

procedure TSimpleTreeDataAdapter.MarkOutgroupMembers;

  procedure ProcessNode(n: TSimpleTreeNode);
  begin
    if Assigned(n.Des1) then
      ProcessNode(n.Des1);
    if Assigned(n.Des2) then
      ProcessNode(n.Des2);
    if not n.IsOtu then
      n.IsOutgroupMember := ((n.Des1.IsOutgroupMember) and (n.Des2.IsOutgroupMember));
  end;

begin
  ProcessNode(FRoot);
end;

function TSimpleTreeDataAdapter.PairwiseDist(mrca, node1, node2: Integer; IsLinearized: Boolean): Extended;
var
  anc: TSimpleTreeNode;
begin
  Assert(FNodes[node1].IsOtu and FNodes[node2].IsOtu, 'PairwiseDist only available for leaf nodes');
  Result := FNodes[node1].BLen;
  anc := FNodes[node1].Ancestor;
  while Assigned(anc) and (anc.NodeIndex <> mrca) do
  begin
    Result := Result + anc.BLen;
    anc := anc.Ancestor;
  end;
  if not IsLinearized then
  begin
    Result := Result + FNodes[node2].BLen;
    anc := FNodes[node2].Ancestor;
    while Assigned(anc) and (anc.NodeIndex <> mrca) do
    begin
      Result := Result + anc.BLen;
      anc := anc.Ancestor;
    end;
  end;
end;

constructor TSimpleTreeDataAdapter.Create;
begin
  NumTaxa := 0;
  FValue := 0.0;
  FList := nil;
  FIsStats := False;
end;

destructor TSimpleTreeDataAdapter.Destroy;
begin
  ClearNodes;
  inherited Destroy;
end;

procedure TSimpleTreeDataAdapter.SetNodeSizes(aNode: TSimpleTreeNode);
begin
  if aNode.IsOtu then
    aNode.Size := 1
  else
  begin
    SetNodeSizes(aNode.Des1);
    SetNodeSizes(aNode.Des2);
    aNode.Size := aNode.Des1.Size + aNode.Des2.Size;
  end;
end;

procedure TSimpleTreeDataAdapter.ExpandOutgroupFromOutgroupTaxa;
var
  i: Integer;
  aNode: TSimpleTreeNode;

  procedure ProcessNode(n: TSimpleTreeNode);
  var
    temp: TSimpleTreeNode;
  begin
    temp := n;
    while (temp.Ancestor <> nil) and (not temp.Ancestor.IsOutgroupMember) do
    begin
      temp := temp.Ancestor;
      temp.IsOutgroupMember := True;
    end;
  end;

  procedure MarkOutgroupTaxa(outgroupNode: TSimpleTreeNode);
  begin
    if not outgroupNode.IsOutgroupMember then
      outgroupNode.IsOutgroupMember := True;
    if Assigned(outgroupNode.Des1) then
      MarkOutgroupTaxa(outgroupNode.Des1);
    if Assigned(outgroupNode.Des2) then
      MarkOutgroupTaxa(outgroupNode.Des2);
  end;

begin
  for i := 0 to NumTaxa - 1 do
  begin
    aNode := FNodes[i];
    if aNode.IsOutgroupMember then { mark all ancestral outgroup nodes}
      ProcessNode(aNode);
  end;
  if FRoot.Des1.IsOutgroupMember then
    MarkOutgroupTaxa(FRoot.Des1)
  else
    MarkOutgroupTaxa(FRoot.Des2);
end;

function TSimpleTreeDataAdapter.NumIngroupNodes: Integer;

  procedure PreorderTraversal(n: TSimpleTreeNode);
  begin
    if Assigned(n.Des1) then
      PreorderTraversal(n.Des1);
    if Assigned(n.Des2) then
      PreorderTraversal(n.Des2);
    if not n.IsOutgroupMember then
      inc(Result);
  end;

begin
  Result := 0;
  PreorderTraversal(FRoot.Des1);
  PreorderTraversal(FRoot.Des2);
end;

procedure TSimpleTreeDataAdapter.CountIngroupAndOutgroupNodes(var numInOutgroup: Integer; var numInIngroup: Integer);

  procedure PreorderTraversal(n: TSimpleTreeNode);
  begin
    if Assigned(n.Des1) then
      PreorderTraversal(n.Des1);
    if Assigned(n.Des2) then
      PreorderTraversal(n.Des2);
    if n.IsOutgroupMember then
      inc(numInOutgroup)
    else
      inc(numInIngroup);
  end;

begin
  numInOutgroup := 0;
  numInIngroup := 0;
  PreorderTraversal(FRoot.Des1);
  PreorderTraversal(FRoot.Des2);
end;

procedure TSimpleTreeDataAdapter.SetNodes(ANodes: TSimpleTreeNodeArray);
begin
  ClearNodes;
  Assert(odd(Length(ANodes)));
  NumTaxa := (Length(ANodes) + 1) div 2;
  InitNodes;
  CloneTreeNodeArray(ANodes, FNodes);
end;

procedure TSimpleTreeDataAdapter.SetTreeData(const AData: TTreeData; IsRooted: Boolean);
var
  i,n: integer;
  BadCondition: String = '';
begin
  NumTaxa := AData.NoOfOtus;
  InitNodes;

  n := NumTaxa;
  for i := 0 to n-2 do { set up the child-parent releationships and get the branch lengths, stderr, depth}
  begin
    FNodes[n+i].des1 := FNodes[AData[i].des1];
    FNodes[n+i].des2 := FNodes[AData[i].des2];
    FNodes[n+i].DataCoverage := AData.DataCoverage[i];
    FNodes[AData[i].des1].Ancestor := FNodes[n+i];
    FNodes[AData[i].des2].Ancestor := FNodes[n+i];
    FNodes[n+i].des1.blen := AData.BLen[AData[i].des1];
    if IsNan(FNodes[n+i].des1.blen) or (FNodes[n+i].des1.blen < 0) then
      FNodes[n+i].des1.blen := 0;
    FNodes[n+i].des2.blen := AData.BLen[AData[i].des2];
    if IsNan(FNodes[n+i].des2.blen) or (FNodes[n+i].des2.blen < 0) then
      FNodes[n+i].des2.blen := 0;
    if AData.isSE then
    begin
      FNodes[n+i].des1.StdError := AData.SE[AData[i].des1];
      FNodes[n+i].des2.StdError := AData.SE[AData[i].des2];
    end;
    FNodes[n+i].depth := max(FNodes[n+i].des1.depth, FNodes[n+i].des2.depth) +1;
  end;

  for i := 0 to NumTaxa - 1 do { mark the taxa that belong to the outgroup}
  begin
    FNodes[i].IsOutgroupMember := AData.IsOutgroupMember[i];
    FNodes[i].IsOtu := True;
  end;

  if AData.isStats then
  begin
    FIsStats := True;
    for i := 0 to NumNodes - 1 do
    begin
      if FNodes[i] = FRoot then
        Continue;
      FNodes[i].Value := AData.Stats[i];
    end;
  end;

  for i := 2 * NumTaxa - 2 downto 0 do  { find the root}
    if FNodes[i].Ancestor = nil then
    begin
      FRoot := FNodes[i];
      break;
    end;

  FValue := AData.Value;

  if not IsRooted then
  begin
    { these are for unrooting the tree (in concept, not in reality)}
    FRoot.des2.blen := FRoot.des1.blen + FRoot.des2.blen;
    FRoot.des1.blen := 0;
    if AData.isSE then
    begin
      FRoot.des2.StdError := FRoot.des1.StdError + FRoot.des2.StdError;
      FRoot.des1.StdError := 0;
    end;
  end;
  SetNodeSizes(FRoot);
  if not ValidateAncestralRelationships(BadCondition) then
    raise Exception.Create('Invalid internal tree structure. ' + BadCondition);
end;

procedure TSimpleTreeDataAdapter.GetTreeData(AData: TTreeData);
var
  i: Integer;
begin
  for i := NumTaxa to NumNodes - 1 do
  begin
    AData.NodeArray[i-NumTaxa].des1 := FNodes[i].des1.NodeIndex;
    AData.NodeArray[i-NumTaxa].des2 := FNodes[i].des2.NodeIndex;
    AData.DataCoverage[i - NumTaxa] := FNodes[i].DataCoverage;
    if AData.isBLen then
    begin
      AData.BLenArray[FNodes[i].des1.NodeIndex] := FNodes[i].des1.blen;
      AData.BLenArray[FNodes[i].des2.NodeIndex] := FNodes[i].des2.blen;
    end;
    if AData.isSE then
    begin
      AData.SEArray[FNodes[i].des1.NodeIndex] := FNodes[i].des1.StdError;
      AData.SEArray[FNodes[i].des2.NodeIndex] := FNodes[i].des2.StdError;
    end;
  end;
  AData.Value := FValue;
  for i := 0 to NumTaxa - 1 do
    AData.IsOutgroupMember[i] := FNodes[i].IsOutgroupMember;
end;

function TSimpleTreeDataAdapter.FindDivergenceTimes: Boolean;
var
  Time1, Time2: Double;

  function ProcessNode(ANode: TSimpleTreeNode): Boolean;
  begin
    Time1 := 0;
    Time2 := 0;
    Result := True;
    if ANode.IsOtu then
      Exit;
    Result := (Result and ProcessNode(ANode.Des1)); { post order traversal is needed}
    Result := (Result and ProcessNode(ANode.Des2));
    if ANode.Des1.IsOtu then
      Time1 := ANode.Des1.BLen
    else
      Time1 := ANode.Des1.Value + ANode.Des1.BLen;

    if ANode.Des2.IsOtu then
      Time2 := ANode.Des2.BLen
    else
      Time2 := ANode.Des2.Value + ANode.Des2.BLen;

    if (ANode <> FRoot) and (CompareValue(Time1, Time2, 0.0001) <> 0) then { then the tree is not linearized}
      Result := False
    else
      ANode.Value := Time1;
  end;

begin
  Result := False;
  Assert(Assigned(FRoot)); { assigned when setting the tree structure which should have already been done}
  Result := ProcessNode(FRoot); { this calculates the divergence times for all internal nodes }
end;

function TSimpleTreeDataAdapter.GetPrunedEpTreeList(var NewickStrings: TStringList; var NamesLists: TList): TList;
var
  aData: TTreeData;
  otuNames: TStringList = nil;
  aList: TTreeList = nil;
  nwk: String;
begin
  try
    FList := TList.Create;
    FEpRefSeq := FNodes[0];
    Result := TList.Create;
    MarkOutgroupMembers;
    aData := TTreeData.Create(NumTaxa, True, False, False);
    GetTreeData(aData);
    Result.Add(aData);
    otuNames := TStringList.Create;
    aList := TTreeList.Create;
    GetOtuNames(otuNames);
    nwk := aList.DebugOutputNewickTree(aData, otuNames);
    NewickStrings.Add(nwk);
    NamesLists.Add(otuNames);

    while CanPruneRefSeqSibling do
    begin
      PruneRefSeqSibling;
      aData := TTreeData.Create(NumTaxa, True, False, False);
      GetTreeData(aData);
      Result.Add(aData);
      otuNames := TStringList.Create;
      GetOtuNames(otuNames);
      nwk := aList.DebugOutputNewickTree(aData, otuNames);
      NewickStrings.Add(nwk);
      NamesLists.Add(otuNames);
    end;
    if CanPruneRefSeqSibling then
      PruneRefSeqSibling;
    aData := TTreeData.Create(NumTaxa, True, False, False);
    GetTreeData(aData);
    Result.Add(aData);
    otuNames := TStringList.Create;
    GetOtuNames(otuNames);
    nwk := aList.DebugOutputNewickTree(aData, otuNames);
    NewickStrings.Add(nwk);
    NamesLists.Add(otuNames);
  finally
    if Assigned(FList) then
      FreeAndNil(FList);
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TSimpleTreeDataAdapter.GetOtuIndex(OtuName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  if NumTaxa > 0 then
    for i := 0 to NumTaxa - 1 do
      if OtuName = FNodes[i].SequenceName then
      begin
        Result := i;
        break;
      end;
end;

function TSimpleTreeDataAdapter.RemoveLeafNodes(taxaNames: TStringList): Boolean;
var
  i: Integer;
  aNode, a, aa, sib: TSimpleTreeNode;
begin
  try
    FList := TList.Create;
    Result := True;
    if taxaNames.Count > 0 then
      for i := 0 to taxaNames.Count - 1 do
      begin
        aNode := GetOtuNode(taxaNames[i]);
        if aNode.Ancestor = FRoot then
        begin
          if aNode = FRoot.Des1 then
            sib := FRoot.Des2
          else
            sib := FRoot.Des1;
          MoveRoot(sib);
        end
        else
        begin
          if (not Assigned(aNode)) or (not Assigned(aNode.Ancestor)) or (not Assigned(aNode.Ancestor.Ancestor)) then
            Result := False
          else
          begin
            a := aNode.Ancestor;
            aa := a.Ancestor;
            if aNode = a.Des1 then
              sib := a.Des2
            else
              sib := a.Des1;
            sib.Ancestor := aa;
            if aa.Des1 = a then
              aa.Des1 := sib
            else
              aa.Des2 := sib;
            if a.Des1 = sib then
              a.Des1 := nil
            else
              a.Des2 := nil;
            sib.BLen := sib.BLen + a.BLen;
            RemoveNode(a);
          end;
        end;
      end;
  finally
    if Assigned(FList) then
      FreeAndNil(FList);
  end;
end;

procedure TSimpleTreeDataAdapter.GuessOutgroup(var AData: TTreeData);
begin
  SetTreeData(AData, True); { set up the tree structure}
  Assert(Assigned(FRoot)); { assigned when setting the tree structure which should have already been done}
  FindOutgroupMembers; { for the smaller of the roots subtrees, mark all nodes as outgroup, if equal, pick des2 }
  GetTreeData(AData); { transfer the result}
end;

function TSimpleTreeDataAdapter.GetSimpleTreeReference: TSimpleTreeNodeArray;
begin
  Result := FNodes;
end;

function TSimpleTreeDataAdapter.GetMrcaMatrix: T2DArrayOfInteger;
var
  i, j: Integer;

  procedure FillMrcaMatrix(aNode: TSimpleTreeNode);
  var
    desList: TIntArray;
    desList2: TIntArray;
    index, index2: Integer;
  begin
    if Assigned(aNode.Des1) then
      FillMrcaMatrix(aNode.Des1);
    if Assigned(aNode.Des2) then
      FillMrcaMatrix(aNode.Des2);
    if not aNode.IsOtu then
    begin
      if aNode.BothDescendentsAreOtus then
      begin
        Result[aNode.Des1.NodeIndex][aNode.Des2.NodeIndex] := aNode.NodeIndex;
        Result[aNode.Des2.NodeIndex][aNode.Des1.NodeIndex] := aNode.NodeIndex;
      end
      else if aNode.Descendent1IsOtu then
      begin
        desList := aNode.Des2.GetLeafNodeIds;
        if Length(desList) > 0 then
          for index := 0 to Length(desList) - 1 do
          begin
            Result[aNode.Des1.NodeIndex][desList[index]] := aNode.NodeIndex;
            Result[desList[index]][aNode.Des1.NodeIndex] := aNode.NodeIndex;
          end;
      end
      else if aNode.Descendent2IsOtu then
      begin
        desList := aNode.Des1.GetLeafNodeIds;
        if Length(desList) > 0 then
          for index := 0 to Length(desList) - 1 do
          begin
            Result[aNode.Des2.NodeIndex][desList[index]] := aNode.NodeIndex;
            Result[desList[index]][aNode.Des2.NodeIndex] := aNode.NodeIndex;
          end;
      end
      else
      begin
        desList := aNode.Des1.GetLeafNodeIds;
        desList2 := aNode.Des2.GetLeafNodeIds;
        if (Length(desList) > 0) and (Length(desList2) > 0) then
        begin
          for index := 0 to Length(desList) - 1 do
            for index2 := 0 to Length(desList2) - 1 do
            begin
              Result[desList[index]][desList2[index2]] := aNode.NodeIndex;
              Result[desList2[index2]][desList[index]] := aNode.NodeIndex;
            end;
        end;
      end;
    end;
  end;

begin
  SetLength(Result, NumTaxa);
  for i := 0 to Length(Result) - 1 do
  begin
    SetLength(Result[i], NumTaxa);
    for j := 0 to Length(Result[i]) - 1 do
      Result[i][j] := -1;
  end;
  FillMrcaMatrix(FRoot);
  TwoDArrayOfIntToList(Result, 'MRCA for all leaf nodes');
end;

function TSimpleTreeDataAdapter.GetPairwiseDistances(IsLinearized: Boolean): T2dArrayOfExtended; overload;
var
  mrcaMatrix: T2DArrayOfInteger;
  i, j: Integer;
begin
  SetLength(Result, NumTaxa);
  for i := 0 to Length(Result) - 1 do
  begin
    SetLength(Result[i], NumTaxa);
    for j := 0 to Length(Result[i]) - 1 do
      Result[i][j] := 0;
  end;
  mrcaMatrix := GetMrcaMatrix;
  for i := 0 to Length(mrcaMatrix) - 1 do
    for j := 0 to Length(mrcaMatrix[i]) - 1 do
      Result[i][j] := PairwiseDist(mrcaMatrix[i][j], i, j, IsLinearized);
end;

function TSimpleTreeDataAdapter.GetPairwiseDistances(aData: TTreeData; IsRooted: Boolean; IsLinearized: Boolean): PDistanceMatrix;
var
  mrcaMatrix: T2DArrayOfInteger;
  i, j: Integer;
begin
  try
    SetTreeData(aData, IsRooted);
    Result := NewDistMatrix(NumTaxa, False);
    mrcaMatrix := GetMrcaMatrix;

    for i := 1 to Length(mrcaMatrix) - 1 do
      for j := 0 to i - 1 do
        Result[i][j] := PairwiseDist(mrcaMatrix[i][j], i, j, IsLinearized);
  finally
    SetLength(mrcaMatrix, 0);
  end;
end;

function TSimpleTreeDataAdapter.DebugStrings: TStringList;
begin
  Result := TreeAsTable(FRoot);
end;

end.

