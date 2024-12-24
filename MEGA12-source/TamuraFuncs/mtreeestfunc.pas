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

unit MTreeEstFunc;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Classes, Controls, StdCtrls, ExtCtrls, SysUtils,
  MegaConsts, MTreeData;

type
  TpMENode = ^TMENode;
  TMENode = record
    index : integer;
    length : double;
    dd: double;
    ds: array[1..2] of double;
    anc : TpMENode;
    sib : TpMENode;
    des1, des2 : TpMENode;
    flag : boolean;
    OTU : boolean;
    N : PArrayOfInt;
  end;
  TMENodeArray = array[0..(MaxInt div SizeOf(TpMENode) -1)] of TpMENode;
  TpMENodeArray = ^TMENodeArray;

function FastOLSBranchLength(tree: TTreeData; d: PDistanceMatrix):double;
procedure MakeNJTree(Tree: TTreeData; D: PDistanceMatrix; Bootstrap: boolean);
procedure MakeMETree(Tree: TTreeData; D: PDistanceMatrix; Bootstrap: boolean);
procedure MakeBIONJTree(Tree: TTreeData; D: PDistanceMatrix; Bootstrap: boolean);

procedure CorrectDistanceMatrix(D: PDistanceMatrix; NoOfSeqs: integer);

implementation

uses
  MGlobalSettings;

procedure MakeNJTree(Tree: TTreeData; D: PDistanceMatrix; Bootstrap: boolean);
var
  AMIN, BMIN : integer;
  H, node,O: PArrayOfInt;
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
          R[i] := R[i] + D[H[i]][H[j]];
      if i < (N-1) then
        for j := i+1 to N-1 do
          R[i] := R[i] + D[H[j]][H[i]];
    end;

    if Bootstrap then
    begin
      for i := 0 to N-1 do
        O[i] := i;
      for i := 0 to N-1 do
      begin
        j := Random(N-i)+i;
        k := O[i];
        O[i] := O[j];
        O[j] := k;
      end;
    end;

    if O[0] < O[1] then
    begin
      AMIN := O[0];
      BMIN := O[1];
      S := (N - 2)*D[H[O[1]]][H[O[0]]] - R[O[0]] - R[O[1]];
    end
    else
    begin
      AMIN := O[1];
      BMIN := O[0];
      S := (N - 2)*D[H[O[0]]][H[O[1]]] - R[O[0]] - R[O[1]];
    end;
    for i := 1 to N-1 do
      for j := 0 to i-1 do
      begin
        if O[i] > O[j] then
        begin
          sij := (N - 2)*D[H[O[i]]][H[O[j]]] - R[O[i]] - R[O[j]];
          if sij <= S then
          begin
            S := sij;
            AMIN := O[j];
            BMIN := O[i];
          end;
        end
        else
        begin
          sij := (N - 2)*D[H[O[j]]][H[O[i]]] - R[O[i]] - R[O[j]];
          if sij <= S then
          begin
            S := sij;
            AMIN := O[i];
            BMIN := O[j];
          end;
        end;
      end;
  end;

  procedure MakeNode(N : integer);
  begin
    Tree.NodeArray[Tree.NoOfOTUs-N].des1 := node[AMIN];
    Tree.NodeArray[Tree.NoOfOTUs-N].des2 := node[BMIN];
    if N = 2 then
    begin
      Tree.BlenArray[node[AMIN]] := D[H[1]][H[0]]/2;
      Tree.BlenArray[node[BMIN]] := D[H[1]][H[0]]/2;
    end
    else
    begin
      Tree.BlenArray[node[AMIN]] := ((N-2)*D[H[BMIN]][H[AMIN]] +R[AMIN] -R[BMIN])/(N-2)/2;
      Tree.BlenArray[node[BMIN]] := ((N-2)*D[H[BMIN]][H[AMIN]] -R[AMIN] +R[BMIN])/(N-2)/2;
    end;
  end;

  procedure MatrixReconst(N : integer);
  var i : integer;
      k : double;
  begin
    k := D[H[BMIN]][H[AMIN]];
    for i := 0 to AMIN-1 do
      D[H[AMIN]][H[i]] := (D[H[AMIN]][H[i]] +D[H[BMIN]][H[i]] -k)/2;
    for i := AMIN+1 to BMIN-1 do
      D[H[i]][H[AMIN]] := (D[H[i]][H[AMIN]] +D[H[BMIN]][H[i]] -k)/2;
    for i := BMIN+1 to N-1 do
      D[H[i]][H[AMIN]] := (D[H[i]][H[AMIN]] +D[H[i]][H[BMIN]] -k)/2;

    node[AMIN] := 2*Tree.NoOfOTUs-N;
    if BMIN < N-1 then
      for i := BMIN to N-2 do
      begin
        H[i] := H[i+1];
        node[i] := node[i+1];
      end;
  end;

var
  i,j: integer;
begin
  GetMem(R, SizeOf(double)*Tree.NoOfOTUs);
  GetMem(H, SizeOf(Integer)*Tree.NoOfOTUs);
  GetMem(node, SizeOf(Integer)*Tree.NoOfOTUs);
  GetMem(O, SizeOf(Integer)*Tree.NoOfOTUs);

  for i := 0 to Tree.NoOfOTUs-1 do
    H[i] := i;
  for i := 0 to Tree.NoOfOTUs-1 do
    node[i] := i;
  for i := 0 to Tree.NoOfOTUs-1 do
    O[i] := i;

  for i := Tree.NoOfOTUs downto 2 do
  begin
    ChooseNeighbor(i);
    MakeNode(i);
    MatrixReconst(i);
  end;

  for i := 1 to Tree.NoOfOTUs-1 do
    for j := 0 to i-1 do
      D[i][j] := D[j][i];

  FreeMemAndNil(R);
  FreeMemAndNil(H);
  FreeMemAndNil(node);
  FreeMemAndNil(O);
end;


procedure MakeBIONJTree(Tree: TTreeData; D: PDistanceMatrix; Bootstrap: boolean);
var
  AMIN, BMIN : integer;
  H,node,O : PArrayOfInt;
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
          R[i] := R[i] + D[H[i]][H[j]];
      if i < (N-1) then
        for j := i+1 to N-1 do
          R[i] := R[i] + D[H[j]][H[i]];
    end;

    if Bootstrap then
    begin
      for i := 0 to N-1 do
        O[i] := i;
      for i := 0 to N-1 do
      begin
        j := Random(N-i)+i;
        k := O[i];
        O[i] := O[j];
        O[j] := k;
      end;
    end;

    if O[0] < O[1] then
    begin
      AMIN := O[0];
      BMIN := O[1];
      S := (N - 2)*D[H[O[1]]][H[O[0]]] - R[O[0]] - R[O[1]];
    end
    else
    begin
      AMIN := O[1];
      BMIN := O[0];
      S := (N - 2)*D[H[O[0]]][H[O[1]]] - R[O[0]] - R[O[1]];
    end;
    for i := 1 to N-1 do
      for j := 0 to i-1 do
      begin
        if O[i] > O[j] then
        begin
          sij := (N - 2)*D[H[O[i]]][H[O[j]]] - R[O[i]] - R[O[j]];
          if sij <= S then
          begin
            S := sij;
            AMIN := O[j];
            BMIN := O[i];
          end;
        end
        else
        begin
          sij := (N - 2)*D[H[O[j]]][H[O[i]]] - R[O[i]] - R[O[j]];
          if sij <= S then
          begin
            S := sij;
            AMIN := O[i];
            BMIN := O[j];
          end;
        end;
      end;
  end;

  procedure MakeNode(N : integer);
  begin
    Tree.NodeArray[Tree.NoOfOTUs-N].des1 := node[AMIN];
    Tree.NodeArray[Tree.NoOfOTUs-N].des2 := node[BMIN];
    if N = 2 then
    begin
      Tree.BlenArray[node[AMIN]] := D[H[1]][H[0]]/2;
      Tree.BlenArray[node[BMIN]] := D[H[1]][H[0]]/2;
    end
    else
    begin
      Tree.BlenArray[node[AMIN]] := ((N-2)*D[H[BMIN]][H[AMIN]] +R[AMIN] -R[BMIN])/(N-2)/2;
      Tree.BlenArray[node[BMIN]] := ((N-2)*D[H[BMIN]][H[AMIN]] -R[AMIN] +R[BMIN])/(N-2)/2;
    end;
  end;

  procedure MatrixReconst(N : integer);
  var i : integer;
      lambda,k : double;
  begin
    if (D[H[BMIN]][H[AMIN]] < 0.000000000001) or (N=2) then
      lambda := 1/2
    else
      lambda := 1/2 +(R[BMIN] -R[AMIN])/2/(N-2)/D[H[BMIN]][H[AMIN]];

    k := lambda*(1-lambda)*D[H[BMIN]][H[AMIN]];

    for i := 0 to AMIN-1 do
      D[H[AMIN]][H[i]] := lambda*D[H[AMIN]][H[i]] +(1-lambda)*D[H[BMIN]][H[i]] -k;
    for i := AMIN+1 to BMIN-1 do
      D[H[i]][H[AMIN]] := lambda*D[H[i]][H[AMIN]] +(1-lambda)*D[H[BMIN]][H[i]] -k;
    for i := BMIN+1 to N-1 do
      D[H[i]][H[AMIN]] := lambda*D[H[i]][H[AMIN]] +(1-lambda)*D[H[i]][H[BMIN]] -k;

    node[AMIN] := 2*Tree.NoOfOTUs-N;
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
  GetMem(R, SizeOf(double)*Tree.NoOfOTUs);
  GetMem(H, SizeOf(Integer)*Tree.NoOfOTUs);
  GetMem(node, SizeOf(Integer)*Tree.NoOfOTUs);
  GetMem(O, SizeOf(Integer)*Tree.NoOfOTUs);

  for i := 0 to Tree.NoOfOTUs-1 do
    H[i] := i;
  for i := 0 to Tree.NoOfOTUs-1 do
    node[i] := i;
  for i := 0 to Tree.NoOfOTUs-1 do
    O[i] := i;

  for i := Tree.NoOfOTUs downto 2 do
  begin
    ChooseNeighbor(i);
    MakeNode(i);
    MatrixReconst(i);
  end;

  for i := 1 to Tree.NoOfOTUs-1 do
    for j := 0 to i-1 do
      D[i][j] := D[j][i];

  FreeMemAndNil(R);
  FreeMemAndNil(H);
  FreeMemAndNil(node);
  FreeMemAndNil(O);
end;


procedure InitTree(node: TpMENodeArray; tree: PArrayOfNodeData; n: integer);
var i : integer;
begin
  for i := 0 to n-1 do
  begin
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
  for i := n to 2*n-2 do
  begin
    node[i].Index := i;
    node[i].length := 0.0;
    node[i].dd := 0.0;
    node[i].OTU := false;
    node[i].anc := nil;
    node[i].sib := nil;
    node[i].flag := false;
    node[i].N[0] := 0;
  end;
  for i := 0 to n-2 do
  begin
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
  if p.des1.N[0] = 0 then
    SetNRecursive(p.des1);
  if p.des2.N[0] = 0 then
    SetNRecursive(p.des2);
  if p.N[0] = 0 then
    SetN(p);
end;

procedure SetDDRecursive(p: TpMENode; d: PDistanceMatrix);
begin
  if not p.des1.OTU and (p.des1.dd = 0.0) then
    SetDDRecursive(p.des1, d);
  if not p.des2.OTU and (p.des2.dd = 0.0) then
    SetDDRecursive(p.des2, d);
  if p.DD = 0.0 then
    SetNodeDD(p, d);
end;

function ComputeExteriorBranchLength(p: TpMENode; n: integer):double;
var nj,nk: double;
begin
  nk := p.sib.N[0];
  nj := n -p.sib.N[0] -1;
  Result := ( (1 +nj +nk)*p.dd
             -(1 +nj -nk)*p.anc.dd
             -(1 -nj +nk)*p.sib.dd)/4/nj/nk;
end;

function ComputeInteriorBranchLength(p: TpMENode; n: integer):double;
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

function FastSumOfBranchLength(node: TpMENodeArray; NoOfOTUs: integer; d: PDistanceMatrix):double;
var i : integer;
begin
    Result := 0.0;
    for i := 0 to NoOfOTUs-1 do
      if node[i].anc.anc = nil then
        node[i].length := 0.0
      else
        node[i].length := ComputeExteriorBranchLength(node[i], NoOfOTUs);
    for i := NoOfOTUs to 2*NoOfOTUs-2 do begin
      if node[i].anc = nil then Continue;
      node[i].length := ComputeInteriorBranchLength(node[i], NoOfOTUs);
    end;
end;

function FastOLSBranchLength(tree: TTreeData;
                             d: PDistanceMatrix):double;
var node: TpMENodeArray;
    p: TpMENode;
    H: PArrayOfInt;
    i,root : integer;
begin
  node := nil; //@SK mega2b4
  H    := nil; //@SK mega2b4
  try
    GetMem(node, SizeOf(TMENode)*(2*tree.NoOfOTUs-1));
    for i := 0 to 2*tree.NoOfOTUs-2 do
        NEW(node[i]);
    for i := 0 to tree.NoOfOTUs-1 do
        GetMem(node[i].N, SizeOf(Integer)*2);
    for i := tree.NoOfOTUs to 2*tree.NoOfOTUs-2 do
        GetMem(node[i].N, SizeOf(Integer)*tree.NoOfOTUs);

    GetMem(H, SizeOf(Integer)*(2*tree.NoOfOTUs-1));
    for i := 0 to 2*tree.NoOfOTUs-2 do
        H[i] := i;

    for i := 0 to tree.NoOfOTUs-1 do
        d[i,i] := 0.0;

    InitTree(node, tree.NodeArray, tree.NoOfOTUs);

    root := -1; //@SK mega2b4
    for i := tree.NoOfOTUs to 2*tree.NoOfOTUs-2 do
        if node[i].anc = nil then begin
            root := i;
            Break;
        end;
    if node[tree.NoOfOTUs-1].anc.index <> root then begin
        p := node[tree.NoOfOTUs-1];
        while p.anc <> nil do begin
            H[p.index] := p.anc.index;
            p := p.anc;
        end;
    end;

    ChangeRoot(node[root], node[tree.NoOfOTUs-1]);
    SetSib(node[root]);
    SetAllN(node[root].des2);
    SetAllDD(node, tree.NoOfOTUs, d);

    for i := 0 to tree.NoOfOTUs-2 do
      node[i].length := ComputeExteriorBranchLength(node[i], tree.NoOfOTUs);
    node[tree.NoOfOTUs-1].length := 0.0;
    for i := tree.NoOfOTUs to 2*tree.NoOfOTUs-2 do begin
      if i = root then Continue;
      node[i].length := ComputeInteriorBranchLength(node[i], tree.NoOfOTUs);
    end;

    Result := 0.0;
    for i := 0 to 2*tree.NoOfOTUs-2 do
      Result := Result +node[i].length;

    for i := 0 to 2*tree.NoOfOTUs-3 do
        if (H[i] = root) then
            tree.blen[i] := 0.0
        else
            tree.blen[i] := node[H[i]].length;

  finally
    if node <> nil then //@SK mega2b4
    begin
      for i := 0 to 2*tree.NoOfOTUs-2 do
      begin
        if node[i] <> nil then
          FreeMemAndNil(node[i].N);
        Dispose(node[i]);
      end;
      FreeMemAndNil(node);
    end;
    FreeMemAndNil(H);
  end;
end;

procedure ResetNodeFlag(node: TpMENodeArray; n: integer);
var i : integer;
begin
    for i := 0 to 2*n-2 do
        node[i].flag := false;
end;

function GetFastOLSBranchLength(p: TpMENode; n: integer; d: PDistanceMatrix):double;
begin
  if p.anc = nil then
    Result := 0.0
  else if p.OTU then
    if p.anc.anc = nil then
      Result := 0.0
    else
      Result := ComputeExteriorBranchLength(p, n)
  else if p.anc <> nil then
    Result := ComputeInteriorBranchLength(p, n);
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

function SearchDt2Trees(node: TpMENode; NoOfOTUs: integer; D: PDistanceMatrix):boolean;
var p : array[0..5] of TpMENode;
    b : array[1..5] of double;
    s0, s : double;
    i, m : integer;

    function CalcBLen:double;
    var i : integer;
    begin
        Result := 0.0;
        for i := 1 to 5 do begin
            p[i].Length := GetFastOLSBranchLength(p[i], NoOfOTUs, D);
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
        SetDDRecursive(p[0], D);
    end;

    procedure ClearFlag;
    var i: integer;
    begin
        for i := 1 to 5 do
            p[i].flag := false;
    end;

begin
    Result := false;
    if node.anc.anc = nil then Exit;
    p[0] := node.anc.anc;
    p[1] := node.des1;
    p[2] := node.des2;
    p[3] := node.sib;
    p[4] := node.anc;
    p[5] := node;

    m := 5;
    SaveBLen;
    RemoveBranch(p[3]);
    for i := 1 to 2 do begin
        AddBranch(p[4], p[3], p[i]);
        ResetN;
        s := CalcBLen;
        if (s0-s) >= 0.00000000000001 then begin
            m := i;
            SaveBLen;
            ClearFlag;
            Result := true;
        end;
        RemoveBranch(p[3]);
    end;
    AddBranch(p[4], p[3], p[m]);
    ResetN;
    for i := 1 to 5 do
        p[i].length := b[i];
end;

function SearchDt4Trees(node: TpMENode; NoOfOTUs: integer; D: PDistanceMatrix):boolean;
var p : array[0..7] of TpMENode;
    b : array[1..7] of double;
    s0, s : double;
    mi,mj,i,j : integer;

    function CalcBLen:double;
    var i: integer;
    begin
        Result := 0.0;
        for i := 1 to 7 do begin
            p[i].Length := GetFastOLSBranchLength(p[i], NoOfOTUs, D);
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
        SetDDRecursive(p[0] ,D);
    end;

    procedure ClearFlag;
    var i: integer;
    begin
        for i := 1 to 7 do
            p[i].flag := false;
    end;

begin
    Result := false;
    if (not node.des1.OTU) and (not node.des2.OTU) then begin
        p[0] := node.anc;
        p[1] := node.des1.des1;
        p[2] := node.des1.des2;
        p[3] := node.des1;
        p[4] := node;
        p[5] := node.des2;
        p[6] := node.des2.des1;
        p[7] := node.des2.des2;


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
                s := CalcBLen;
                if (s0-s) >= 0.00000000000001 then begin
                    mi := i;
                    mj := j;
                    SaveBLen;
                    ClearFlag;
                    Result := true;
                end;
                RemoveBranch(p[7]);
            end;
            RemoveBranch(p[6]);
        end;
        AddBranch(p[4], p[6], p[3]);
        for j := 1 to 2 do begin
            AddBranch(p[5], p[7], p[j]);
            ResetN(3, 4, 5);
            s := CalcBLen;
            if (s0-s) >= 0.00000000000001 then begin
                mi := 3;
                mj := j;
                SaveBLen;
                ClearFlag;
                Result := true;
            end;
            RemoveBranch(p[7]);
        end;
        RemoveBranch(p[6]);

        AddBranch(p[4], p[6], p[mi]);
        AddBranch(p[5], p[7], p[mj]);
        ResetN(3, 4, 5);
        for i := 1 to 7 do
            p[i].length := b[i];

    end;

    if node.anc.anc = nil then Exit;
    if node.anc.anc.anc = nil then Exit;

    p[0] := node.anc.anc.anc;
    p[1] := node.des1;
    p[2] := node.des2;
    p[3] := node.sib;
    p[4] := node.anc;
    p[5] := node;
    p[6] := node.anc.sib;
    p[7] := node.anc.anc;

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
            s := CalcBLen;
            if (s0-s) >= 0.00000000000001 then begin
                mi := i;
                mj := j;
                SaveBLen;
                ClearFlag;
                Result := true;
            end;
            RemoveBranch(p[6]);
        end;
        RemoveBranch(p[3]);
    end;
    AddBranch(p[4], p[3], p[5]);
    for j := 1 to 2 do begin
        AddBranch(p[7], p[6], p[j]);
        ResetN(4, 5, 7);
        s := CalcBLen;
        if (s0-s) >= 0.00000000000001 then begin
            mi := 5;
            mj := j;
            SaveBLen;
            ClearFlag;
            Result := true;
        end;
        RemoveBranch(p[6]);
    end;
    RemoveBranch(p[3]);

    AddBranch(p[4], p[3], p[mi]);
    AddBranch(p[7], p[6], p[mj]);
    ResetN(4, 5, 7);
    for i := 1 to 7 do
        p[i].length := b[i];
end;

function SearchDt4Trees2(node: TpMENode; NoOfOTUs: integer; D: PDistanceMatrix):boolean;
var p : array[0..9] of TpMENode;
    b : array[1..9] of double;
    s0, s: double;
    mi,mj,i,j : integer;

    function CalcBLen:double;
    var i: integer;
    begin
        Result := 0.0;
        for i := 1 to 9 do begin
            p[i].Length := GetFastOLSBranchLength(p[i], NoOfOTUs, D);
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
        SetDDRecursive(p[0], D);
    end;

    procedure ClearFlag;
    var i: integer;
    begin
        for i := 1 to 9 do
            p[i].flag := false;
    end;

begin
    Result := false;
    if node.anc.anc = nil then Exit;
    if node.anc.anc.anc = nil then Exit;
    if node.anc.anc.anc.anc <> nil then begin
        p[0] := node.anc.anc.anc.anc;
        p[1] := node.des1;
        p[2] := node.des2;
        p[3] := node.sib;
        p[4] := node.anc;
        p[5] := node;
        p[6] := node.anc.sib;
        p[7] := node.anc.anc;
        p[8] := node.anc.anc.sib;
        p[9] := node.anc.anc.anc;

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
                s := CalcBLen;
                if (s0-s) >= 0.00000000000001 then begin
                    mi := i;
                    mj := j;
                    SaveBLen;
                    ClearFlag;
                    Result := true;
                end;
                RemoveBranch(p[8]);
            end;
            RemoveBranch(p[3]);
        end;

        AddBranch(p[4], p[3], p[mi]);
        AddBranch(p[9], p[8], p[mj]);
        ResetN(4, 5, 7, 9);
        for i := 1 to 9 do
            p[i].length := b[i];
    end;

    if not node.anc.sib.OTU then begin
        p[0] := node.anc.anc.anc;
        p[1] := node.des1;
        p[2] := node.des2;
        p[3] := node.sib;
        p[4] := node.anc;
        p[5] := node;
        p[6] := node.anc.sib.des1;
        p[7] := node.anc.sib.des2;
        p[8] := node.anc.anc;
        p[9] := node.anc.sib;

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
                s := CalcBLen;
                if (s0-s) >= 0.00000000000001 then begin
                    mi := i;
                    mj := j;
                    SaveBLen;
                    ClearFlag;
                    Result := true;
                end;
                RemoveBranch(p[3]);
            end;
            RemoveBranch(p[5]);
        end;

        AddBranch(p[8], p[5], p[mi]);
        AddBranch(p[4], p[3], p[mj]);
        ResetN(4, 5, 8, 9);
        for i := 1 to 9 do
            p[i].length := b[i];
    end;
end;

function SearchMETree(tree : PArrayOfNodeData;
                      blen: PArrayOfDouble;
                      NoOfOTUs : integer;
                      D : PDistanceMatrix):double;
var node: TpMENodeArray;
    i: integer;
    flag1, flag2 : boolean;
begin
    try
      GetMem(node, SizeOf(Pointer)*(2*NoOfOTUs-1));
      for i := 0 to 2*NoOfOTUs-2 do
          NEW(node[i]);
      for i := 0 to NoOfOTUs-1 do
          GetMem(node[i].N, SizeOf(Integer)*2);
      for i := NoOfOTUs to 2*NoOfOTUs-2 do
          GetMem(node[i].N, SizeOf(Integer)*(NoOfOTUs+1));
      InitTree(node, tree, NoOfOTUs);
      ResetNodeFlag(node, NoOfOTUs);
      ChangeRoot(node[2*NoOfOTUs-2], node[NoOfOTUs-1]);
      SetSib(node[2*NoOfOTUs-2]);
      SetAllN(node[2*NoOfOTUs-2].des2);
      SetAllDD(node, NoOfOTUs, D);
      FastSumOfBranchLength(node, NoOfOTUs, D);
      repeat
          flag2 := true;
          for i := NoOfOTUs to 2*NoOfOTUs-3 do begin
              flag1 := true;
              if node[i].flag then
                  Continue;
              if SearchDt4Trees2(node[i], NoOfOTUs, D) then begin
                  flag1 := false;
                  flag2 := false;
              end;
              if SearchDt4Trees(node[i], NoOfOTUs, D) then begin
                  flag1 := false;
                  flag2 := false;
              end;
              if SearchDt2Trees(node[i], NoOfOTUs, D) then begin
                  flag1 := false;
                  flag2 := false;
              end;
              node[i].flag := flag1;
          end;
      until flag2;
      for i := NoOfOTUs to 2*NoOfOTUs-2 do begin
          tree[i-NoOfOTUs].des1 := node[i].des1.index;
          tree[i-NoOfOTUs].des2 := node[i].des2.index;
      end;
      Result := 0.0;
      for i := 0 to 2*NoOfOTUs-3 do begin
          blen[i] := node[i].length;
          Result := Result +blen[i];
      end;

    finally
      for i := 0 to 2*NoOfOTUs-2 do begin
        if node[i] <> nil then
        begin
          FreeMemAndNil(node[i].N);
          Dispose(node[i]);
        end;
      end;
      FreeMemAndNil(node);
    end;
end;

procedure MakeMETree(Tree: TTreeData; D: PDistanceMatrix; Bootstrap: boolean);
begin
  MakeNJTree(Tree, D, Bootstrap);

  SearchMETree(tree.NodeArray, tree.BLenArray, tree.NoOfOTUs, D);
end;

procedure CorrectDistanceMatrix(D: PDistanceMatrix; NoOfSeqs: integer);
var
  i,j,ii,jj,i1,i2,j1,j2: integer;
  r: double;
begin
  for i := 1 to NoOfSeqs-1 do
  begin
    for j := 0 to i-1 do
      if D[i,j] < 0 then
      begin
        r := 10;
        i1 := 0;
        i2 := 0;
        for ii := 0 to NoOfSeqs-1 do
        begin
          if ii = i then
            continue;
          if D[i, ii] < 0 then
            continue;
          if D[i, ii] <= r then
          begin
            i2 := i1;
            i1 := ii;
            r  := D[i, ii];
          end;
        end;
        r := 10;
        j1 := 0;
        j2 := 0;
        for jj := 0 to NoOfSeqs-1 do
        begin
          if jj = j then
            continue;
          if D[j, jj] < 0 then
            continue;
          if D[j, jj] <= r then
          begin
            j2 := j1;
            j1 := jj;
            r  := D[j, jj];
          end;
        end;

        if D[i,i1] < D[j,j1] then
          if D[i1,j] > -0.5 then
            D[i,j] := D[i1,j]
          else if (D[i,i2] < D[j,j1]) and (D[i2,j] > -0.5) then
            D[i,j] := D[i2,j]
          else if D[j,j1] > -0.5 then
            D[i,j] := D[i,j1];
        if D[i,j] < 0 then
          if D[i,j1] > -0.5 then
            D[i,j] := D[i,j1]
          else if (D[j,j2] <  D[i,i1]) and (D[i,j2] > -0.5) then
            D[i,j] := D[i,j2]
          else
            D[i,j] := D[i1,j1];
        if D[i,j] < 0 then
          if (D[i,i2] < D[j,j2]) and (D[i2,j1] > -0.5) then
            D[i,j] := D[i2,j1]
          else if (D[i1,j2] > -0.5) then
            D[i,j] := D[i1,j2]
          else
            D[i,j] := D[i2,j2];

        if D[i,j] < 0 then
          D[i,j] := 0;

        D[j,i] := D[i,j];
    end;
  end;
end;

end.
