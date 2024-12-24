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

unit MLTreeNode;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  MegaConsts, Classes;

type
  TMolecule = (DNA, AminoAcid);

  TLikelihoodArray = array [0..MaxPointerElts-1] of PArrayOfDouble;
  PLikelihoodArray = ^TLikelihoodArray;


  TDNAVector = array [0..3] of double;   // 0=A, 1=T, 2=C, 3=G
  TDNAMatrix = array [0..3,0..3] of double;

type
  TMLTree = class;

  TSubstitutionModel = class
  private
    FTree: TMLTree;
    FFreq: array of double;
    n: integer;

    function GetFreq(index: integer): double; virtual; abstract;
  public
    property Freq[index: integer]: double read GetFreq;

    procedure ComputeProb(p: PLikelihoodArray; b: double); virtual; abstract;

    function OptimizeParameters: double; virtual; abstract;

    constructor Create(tree: TMLTree);
  end;

  TTN93Model = class (TSubstitutionModel)
  private
    pR,pY: double;

    procedure ComputeTempProb(index: integer; b,k1,k2: double);
  public
    pA,pT,pC,pG,k1,k2: double;

    procedure ComputeProb(p: PLikelihoodArray; b: double); override;

    function OptimizeParameters: double; override;

    constructor Create(tree: TMLTree);
 end;

//  TArrayOfDNAVector = array[0..MaxDNAVector-1] of TDNAVector;
//  PArrayOfDNAVector = ^TArrayOfDNAVector;

  TMLTreeNode = class
  public
    index : integer;
    depth : integer;
    OTU : boolean;
    blen  : double;
    anc : TMLTreeNode;
    sib : TMLTreeNode;
    des1, des2 : TMLTreeNode;
    flag: boolean;

    Prob: PLikelihoodArray;
    dP  : PLikelihoodArray;
    L1:   PLikelihoodArray;
    L2:   PLikelihoodArray;
    dL1:  PLikelihoodArray;
    dL2:  PLikelihoodArray;
    c:  array of integer;


  end;
  TMLTreeNodeArray = array of TMLTreeNode;

  TMLTree = class
  private
    FMolecule: TMolecule;
    FNoOfSeqs: integer;
    FNoOfSites: integer;
    FNoOfStates: integer;
    Root: TMLTreeNode;
    nstates: integer;
    LogLikelihood: double;

  public
    Node:  TMLTreeNodeArray;
    Seqs:  TStringList;
    Model: TSubstitutionModel;

    property Molecule: TMolecule read FMolecule write FMolecule;
    property NoOfSites: integer read FNoOfSites;
    property NoOfSeqs: integer read FNoOfSeqs;

    procedure SetSiteConfigOfNode(index: integer);
    procedure ComputeL1OfNode(index: integer);
    procedure ComputeL2OfNode(index: integer);
    procedure ComputeDL1OfNode(index: integer);
    procedure ComputeDL2OfNode(index: integer);
    procedure ResetLogLikelihood;
    procedure ComputeLogLikelihood;

    function ComputeTemporalLogLikelihood: double;
    function GetTemporalLogLikelihood: double;

    function OptimizeParameters: double;
    function OptimizeBlenOfNode(index: integer): double;
    function OptimizeAllBLens: double;

    procedure InitSiteConfig;
    procedure InitTempProb;
    procedure InitTempProbOfNode(index: integer);

    constructor Create(nseqs,nsites: integer; mol: TMolecule);
    destructor Destroy; override;
  end;

implementation

uses
  MGlobalSettings;

constructor TSubstitutionModel.Create(tree: TMLTree);
begin
  inherited Create;

  FTree := tree;
end;

constructor TTN93Model.Create(tree: TMLTree);
begin
  inherited;
  n := 3;

  setlength(FFreq, 4);
end;

procedure TTN93Model.ComputeProb(p: PLikelihoodArray; b: double);
var
  P1,p2,Q: double;

  function EP1: double;
  begin
    result := (pR +pY*exp(-2*b) -exp(-2*(pR*k1 +pY)*b))/pR;
  end;

  function EP2: double;
  begin
    result := (pY +pR*exp(-2*b) -exp(-2*(pY*k2 +pR)*b))/pY;
  end;

  function EQ: double;
  begin
    result := 1 -exp(-2*b);
  end;

begin
  pR := pA+pG;
  pY := pT+pC;

  P1 := EP1;
  P2 := EP2;
  Q  := EQ;

  p[0,1] := pT*Q;
  p[0,2] := pC*Q;
  p[0,3] := pG*P1;
  p[0,0] := 1-p[0,1]-p[0,2]-p[0,3];

  p[1,0] := pA*Q;
  p[1,2] := pC*P2;
  p[1,3] := pG*Q;
  p[1,1] := 1-p[1,0]-p[1,2]-p[1,3];

  p[2,0] := pA*Q;
  p[2,1] := pT*P2;
  p[2,3] := pG*Q;
  p[2,2] := 1-p[2,0]-p[2,1]-p[2,3];

  p[3,0] := pA*P1;
  p[3,1] := pT*Q;
  p[3,2] := pC*Q;
  p[3,3] := 1-p[3,0]-p[3,1]-p[3,2];
end;

procedure TTN93Model.ComputeTempProb(index: integer; b,k1,k2: double);
var
  P1,p2,Q: double;

  function EP1: double;
  begin
    result := (pR +pY*exp(-2*b) -exp(-2*(pR*k1 +pY)*b))/pR;
  end;

  function EP2: double;
  begin
    result := (pY +pR*exp(-2*b) -exp(-2*(pY*k2 +pR)*b))/pY;
  end;

  function EQ: double;
  begin
    result := 1 -exp(-2*b);
  end;

begin
  pR := pA+pG;
  pY := pT+pC;

  P1 := EP1;
  P2 := EP2;
  Q  := EQ;

  with FTree.Node[index] do
  begin
    dP[0,1] := pT*Q;
    dP[0,2] := pC*Q;
    dP[0,3] := pG*P1;
    dP[0,0] := 1-dP[0,1]-dP[0,2]-dP[0,3];

    dP[1,0] := pA*Q;
    dP[1,2] := pC*P2;
    dP[1,3] := pG*Q;
    dP[1,1] := 1-dP[1,0]-dP[1,2]-dP[1,3];

    dP[2,0] := pA*Q;
    dP[2,1] := pT*P2;
    dP[2,3] := pG*Q;
    dP[2,2] := 1-dP[2,0]-dP[2,1]-dP[2,3];

    dP[3,0] := pA*P1;
    dP[3,1] := pT*Q;
    dP[3,2] := pC*Q;
    dP[3,3] := 1-dP[3,0]-dP[3,1]-dP[3,2];
  end;
end;

function TTN93Model.OptimizeParameters: double;

var
  dL0,dL1,dL2,Li,dL,ddL,d,deltaL: double;
  i,it,k: integer;
begin
  result := FTree.LogLikelihood;
  it := 0;
  deltaL := abs(FTree.LogLikelihood/1000000000000);
  repeat
    d := k1/1000;
    for i := 0 to  2*FTree.NoOfSeqs-2 do
    begin
      if FTree.Node[i] = FTree.Root then
        continue;
      ComputeTempProb(i, FTree.Node[i].blen, k1+d,k2);
    end;
    dL1 := FTree.ComputeTemporalLogLikelihood;

    for i := 0 to  2*FTree.NoOfSeqs-2 do
    begin
      if FTree.Node[i] = FTree.Root then
        continue;
      ComputeTempProb(i, FTree.Node[i].blen, k1-d,k2);
    end;
    dL2 := FTree.ComputeTemporalLogLikelihood;

    dL0 := result;
    dL  := (dL1-dL2)/d/2;
    ddL := ((dL1-2*dL0+dL2))/d/d;

    if abs(ddL) > 0 then
      k1 := k1 -dL/ddL;

    d := k2/1000;
    for i := 0 to  2*FTree.NoOfSeqs-2 do
    begin
      if FTree.Node[i] = FTree.Root then
        continue;
      ComputeTempProb(i, FTree.Node[i].blen, k1,k2+d);
    end;
    dL1 := FTree.ComputeTemporalLogLikelihood;

    for i := 0 to  2*FTree.NoOfSeqs-2 do
    begin
      if FTree.Node[i] = FTree.Root then
        continue;
      ComputeTempProb(i, FTree.Node[i].blen, k1,k2-d);
    end;
    dL2 := FTree.ComputeTemporalLogLikelihood;

    dL0 := result;
    dL  := (dL1-dL2)/d/2;
    ddL := ((dL1-2*dL0+dL2))/d/d;

    if abs(ddL) > 0 then
      k2 := k2 -dL/ddL;

    for i := 0 to  2*FTree.NoOfSeqs-2 do
    begin
      if FTree.Node[i] = FTree.Root then
        continue;

      ComputeProb(FTree.Node[i].Prob, FTree.Node[i].blen);
    end;
    FTree.ResetLogLikelihood;

    result := FTree.LogLikelihood;

    inc(it);
  until (abs(result-dL0) < deltaL) or (it = 20);
end;

constructor TMLTree.Create(nseqs,nsites: integer; mol: TMolecule);
var
  i,k: integer;
begin
  inherited Create;

  FNoOfSeqs   := nseqs;
  FNoOfSites  := nsites;
  case mol of
    DNA:       nstates :=  4;
    AminoAcid: nstates := 20;
  end;

  setlength(Node, 2*nseqs);
  for i := 0 to nseqs-1 do
  begin
    node[i].index := i;
    node[i].depth := 0;
    node[i].OTU := true;
    node[i].blen  := 0.0;
    node[i].anc := nil;
    node[i].sib := nil;
    node[i].des1 := nil;
    node[i].des2 := nil;
    node[i].flag := false;
    setlength(node[i].c, nsites+1);
    for k := 0 to nsites do
      node[i].c[k] := k;
  end;
  for i := nseqs to 2*nseqs-2 do
  begin
    node[i].Index := i;
    node[i].depth := 0;
    node[i].OTU := false;
    node[i].blen := 0.0;
    node[i].anc := nil;
    node[i].sib := nil;
    node[i].flag := false;
    setlength(node[i].c, nsites+1);
    for k := 0 to nsites do
      node[i].c[k] := k;
  end;
  for i := 0 to 2*nseqs-2 do
  begin
    GetMem(node[i].L1, SizeOf(TDNAVector)*(nsites+1));
    GetMem(node[i].L2, SizeOf(TDNAVector)*(nsites+1));
    GetMem(node[i].dL1, SizeOf(TDNAVector)*(nsites+1));
    GetMem(node[i].dL2, SizeOf(TDNAVector)*(nsites+1));
  end;

end;

destructor TMLTree.Destroy;
var
  i: integer;
begin
  for i := 0 to NoOfSeqs-2 do
  begin
    if node[i] <> nil then
    begin
      FreeMemAndNil(node[i].L1);
      FreeMemAndNil(node[i].L2);
      FreeMemAndNil(node[i].dL1);
      FreeMemAndNil(node[i].dL2);

      setlength(node[i].c, 0);
    end;
  end;

  setlength(Node, 0);
end;

procedure TMLTree.SetSiteConfigOfNode(index: integer);
var
  i,j: integer;
begin
  if node[index].OTU then
    exit;

  for i := 1 to FNoOfSites do
  begin
    node[index].c[i] := i;
    for j := 1 to i-1 do
      if (node[index].des1.c[i] = node[index].des1.c[j]) and
         (node[index].des2.c[i] = node[index].des2.c[j]) then
      begin
        node[index].c[i] := j;
        break;
      end;
  end;
end;

procedure TMLTree.InitSiteConfig;

  procedure InitSiteConfigOfNode(node: TMLTreeNode);
  begin
    if node.OTU then exit;
  
    InitSiteConfigOfNode(node.des1);
    InitSiteConfigOfNode(node.des2);
  
    SetSiteConfigOfNode(node.index);
  end;

var
  site: array [0..19] of integer;
  i,k: integer;
begin
  case Molecule of
    DNA:
    begin
      for i := 0 to FNoOfSeqs-1 do
      begin
        for k := 0 to 3 do
          site[k] := 0;
        for k := 1 to FNoOfSites do
          if upcase(Seqs[i][k]) = 'A' then
          begin
            site[0] := k;
            break;
          end;
        for k := 1 to FNoOfSites do
          if (upcase(Seqs[i][k]) = 'T') or (upcase(Seqs[i][k]) = 'U') then
          begin
            site[1] := k;
            break;
          end;
        for k := 1 to FNoOfSites do
          if upcase(Seqs[i][k]) = 'C' then
          begin
            site[2] := k;
            break;
          end;
        for k := 1 to FNoOfSites do
          if upcase(Seqs[i][k]) = 'G' then
          begin
            site[3] := k;
            break;
          end;
        for k := 1 to FNoOfSites do
          with node[i] do
            case upcase(Seqs[i][k]) of
              'A': begin
                     L1[k,0]  := 1; L1[k,1]  := 0; L1[k,2]  := 0;  L1[k,3] := 0;
                     L2[k,0]  := 1; L2[k,1]  := 0; L2[k,2]  := 0;  L2[k,3] := 0;
                     dL1[k,0] := 1; dL1[k,1] := 0; dL1[k,2] := 0; dL1[k,3] := 0;
                     dL2[k,0] := 1; dL2[k,1] := 0; dL2[k,2] := 0; dL2[k,3] := 0;
                     c[k] := site[0];
                   end;
              'T', 
			  'U': begin
                     L1[k,0]  := 0; L1[k,1]  := 1; L1[k,2]  := 0; L1[k,3]  := 0;
                     L2[k,0]  := 0; L2[k,1]  := 1; L2[k,2]  := 0; L2[k,3]  := 0;
                     dL1[k,0] := 0; dL1[k,1] := 1; dL1[k,2] := 0; dL1[k,3] := 0;
                     dL2[k,0] := 0; dL2[k,1] := 1; dL2[k,2] := 0; dL2[k,3] := 0;
                     c[k] := site[1];
                   end;
              'C': begin
                     L1[k,0]  := 0; L1[k,1]  := 0; L1[k,2]  := 1; L1[k,3]  := 0;
                     L2[k,0]  := 0; L2[k,1]  := 0; L2[k,2]  := 1; L2[k,3]  := 0;
                     dL1[k,0] := 0; dL1[k,1] := 0; dL1[k,2] := 1; dL1[k,3] := 0;
                     dL2[k,0] := 0; dL2[k,1] := 0; dL2[k,2] := 1; dL2[k,3] := 0;
                     c[k] := site[2];
                   end;
              'G': begin
                     L1[k,0]  := 0; L1[k,1]  := 0; L1[k,2]  := 0;  L1[k,3] := 1;
                     L2[k,0]  := 0; L2[k,1]  := 0; L2[k,2]  := 0;  L2[k,3] := 1;
                     dL1[k,0] := 0; dL1[k,1] := 0; dL1[k,2] := 0; dL1[k,3] := 1;
                     dL2[k,0] := 0; dL2[k,1] := 0; dL2[k,2] := 0; dL2[k,3] := 1;
                     c[k] := site[3];
                   end;
              else
              begin
                L1[k,0]  := 1; L1[k,1]  := 1; L1[k,2]  := 1; L1[k,3]  := 1;
                L2[k,0]  := 1; L2[k,1]  := 1; L2[k,2]  := 1; L2[k,3]  := 1;
                dL1[k,0] := 1; dL1[k,1] := 1; dL1[k,2] := 1; dL1[k,3] := 1;
                dL2[k,0] := 1; dL2[k,1] := 1; dL2[k,2] := 1; dL2[k,3] := 1;
                c[k] := k;
              end;
            end;
      end;
    end;
    AminoAcid:
    begin
    end;
  end;
  InitSiteConfigOfNode(root);
end;

procedure TMLTree.ComputeL1OfNode(index: integer);
var
  i,j,k: integer;
begin
  with node[index] do
    for k := 1 to FNoOfSites do
      if c[k] = k then
        for i := 0 to nstates-1 do
        begin
          L1[k,i] := 0;
          for j := 0 to nstates-1 do
            L1[k,i]  := L1[k,i] +des1.L1[des1.c[k],j]*des1.L2[des1.c[k],j]*des1.Prob[i,j];
          dL1[k,i] := L1[k,i];
        end;
end;

procedure TMLTree.ComputeL2OfNode(index: integer);
var
  i,j,k: integer;
begin
  with node[index] do
    for k := 1 to FNoOfSites do
      if c[k] = k then
        for i := 0 to nstates-1 do
        begin
          L2[k,i] := 0;
          for j := 0 to nstates-1 do
            L2[k,i] := L2[k,i] +des2.L1[des2.c[k],j]*des2.L2[des2.c[k],j]*des2.Prob[i,j];
          dL2[k,i] := L2[k,i];
        end;
end;

procedure TMLTree.ComputeDL1OfNode(index: integer);
var
  i,j,k: integer;
begin
  with node[index] do
    for k := 1 to FNoOfSites do
      if c[k] = k then
        for i := 0 to nstates-1 do
        begin
          dL1[k,i] := 0;
          for j := 0 to nstates-1 do
            dL1[k,i] := dL1[k,i] +des1.dL1[des1.c[k],j]*des1.dL2[des1.c[k],j]*des1.dP[i,j];
        end;
end;

procedure TMLTree.ComputeDL2OfNode(index: integer);
var
  i,j,k: integer;
begin
  with node[index] do
    for k := 1 to FNoOfSites do
      if c[k] = k then
        for i := 0 to nstates-1 do
        begin
          dL2[k,i] := 0;
          for j := 0 to nstates-1 do
            dL2[k,i] := dL2[k,i] +des2.dL1[des2.c[k],j]*des2.dL2[des2.c[k],j]*des2.dP[i,j];
        end;
end;

procedure TMLTree.ComputeLogLikelihood;
var
  k,i: integer;
  Li: double;
begin
  LogLikelihood := 0;
  for k := 1 to FNoOfSites do
  begin
    Li := 0;
    for i := 0 to nstates-1 do
      Li := Li +Model.Freq[i]*root.L1[root.c[k],i]*root.L2[root.c[k],i];
    LogLikelihood := LogLikelihood +ln(Li);
  end;
end;

procedure TMLTree.ResetLogLikelihood;

  procedure ResetRecursive(n: TMLTreeNode);

  var
    k: integer;
  begin
    if n.OTU then
      exit;
    ResetRecursive(n.des1);
    ResetRecursive(n.des2);

    ComputeL1OfNode(n.index);
    ComputeL2OfNode(n.index);
  end;

begin
  ResetRecursive(root);

  ComputeLogLikelihood;
end;

procedure TMLTree.InitTempProbOfNode(index: integer);
var
  i,j: integer;
begin
  with Node[index] do
    for i := 0 to nstates-1 do
      for j := 0 to nstates-1 do
        dP[i,j] := Prob[i,j];
end;

procedure TMLTree.InitTempProb;
var
  i,j,k: integer;
begin
  for i := 0 to 2*NoOfSeqs-2 do
    with Node[i] do
      for j := 0 to nstates-1 do
        for k := 0 to nstates-1 do
          dP[j,k] := Prob[j,k];
end;

function TMLTree.GetTemporalLogLikelihood: double;
var
  k,i: integer;
  Li: double;
begin
  result := 0;
  for k := 1 to FNoOfSites do
  begin
    Li := 0;
    for i := 0 to nstates-1 do
      Li := Li +Model.Freq[i]*root.dL1[root.c[k],i]*root.dL2[root.c[k],i];
    result := result +ln(Li);
  end;
end;

function TMLTree.ComputeTemporalLogLikelihood: double;

  procedure ResetRecursive(n: TMLTreeNode);
  var
    k: integer;
  begin
    if n.OTU then
      exit;
    ResetRecursive(n.des1);
    ResetRecursive(n.des2);

    ComputeDL1OfNode(n.index);
    ComputeDL2OfNode(n.index);
  end;

begin
  ResetRecursive(root);

  result := GetTemporalLogLikelihood;
end;

function TMLTree.OptimizeParameters: double;
begin
  result := Model.OptimizeParameters;
end;

function TMLTree.OptimizeBlenOfNode(index: integer): double;

  function RecomputeLikelihood(n: TMLTreeNode): double;
  begin
    while assigned(n.anc) do
    begin
      if n = n.anc.des1 then
        ComputeL1OfNode(node[index].anc.index)
      else
        ComputeL2OfNode(node[index].anc.index);
      n := n.anc;
    end;

    ComputeLogLikelihood;
    result := LogLikelihood;
  end;

  function ComputeDeltaLikelihood(n: TMLTreeNode): double;
  begin
    while n.anc <> nil do
    begin
      if n = n.anc.des1 then
        ComputeDL1OfNode(node[index].anc.index)
      else
        ComputeDL2OfNode(node[index].anc.index);
      n := n.anc;
    end;

    result := GetTemporalLogLikelihood;
  end;

var
  b,d,db,dL,dL0,dL1,dL2,deltaL,ddL0,b0: double;
  i,j: integer;
begin
  deltaL := abs(LogLikelihood/10000000000);
  dL0  := LogLikelihood;

  b0 := node[index].blen;
  b  := b0;
  if b < 0.0000000001 then
    b := 0.0000001;
  d := b/100;

  ddL0 := dL0;

  Model.ComputeProb(node[index].dP, b+d);
  dL1 := ComputeDeltaLikelihood(node[index]);

  Model.ComputeProb(node[index].dP, b-d);
  dL2 := ComputeDeltaLikelihood(node[index]);

  dL := (dL1-2*dL0+dL2);
  if (abs(dL) > 0) then
  begin
    db := d*(dL1-dL2)/2/dL;
    if b < db then
      db := b/2;

    b := b -db;

    if abs(db/2) < d then
      d := abs(db/2);
  end;

  Model.ComputeProb(node[index].dP, b);
  dL0 := ComputeDeltaLikelihood(node[index]);

  if dL0 < ddL0 then
  begin
    if abs(dL0-ddL0) > deltaL then
    begin
      b := b0;

      Model.ComputeProb(node[index].dP, b);
      dL0 := ComputeDeltaLikelihood(node[index]);
    end;


  end;

  with node[index] do
  begin
    blen := b;

    for i := 0 to nstates-1 do
      for j := 0 to nstates-1 do
        Prob[i,j] := dP[i,j];
  end;

  result := RecomputeLikelihood(node[index]);
end;


function TMLTree.OptimizeAllBLens: double;
var
  L0: double;

  procedure OptimizeRecursive(n: TMLTreeNode);
  begin
    if not n.OTU then
    begin
      OptimizeRecursive(n.des1);
      OptimizeRecursive(n.des2);
    end;

    L0 := OptimizeBlenOfNode(n.index);
  end;

var
  i,it: integer;
  deltaL,dL0: double;
begin
  InitTempProb;

  result := L0;
  deltaL := -L0/1000000;
  it := 0;
  repeat
    dL0 := result;
    OptimizeRecursive(root);
    result := L0;
    inc(it);
  until (abs(result-dL0)<deltaL) or (it = 5);
end;


////////////////////////

procedure SwapPointer(var p1, p2: pointer);
var
  p: pointer;
begin
  p  := p1;
  p1 := p2;
  p2 := p;
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

procedure SetSib(p : TMLTreeNode);
begin
  if not p.OTU then
  begin
    SetSib(p.des1);
    SetSib(p.des2);
  end;
  if p.anc = nil then Exit;
  if p = p.anc.des1 then
    p.sib := p.anc.des2
  else
    p.sib := p.anc.des1;
end;

procedure ChangeRoot(root, newposition : TMLTreeNode);
var a, p, d : TMLTreeNode;
  len, len1, len2 : double;
begin

  if newposition.anc = root then
  begin
    if newposition = root.des2 then
    begin
      p := root.des1;
      root.des1 := root.des2;
      root.des2 := p;
    end;
    Exit;
  end;
  len := root.des1.blen +root.des2.blen;
  d := newposition;
  p := d.anc;
  a := p.anc;
  len2 := d.blen;
  while p <> root do
  begin
    len1 := p.blen;
    p.blen := len2;
    len2 := len1;
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
    p.des2.blen := len;
    if p = d.des1 then
      d.des1 := p.des2
    else
      d.des2 := p.des2;
  end
  else
  begin
    p.des1.anc := d;
    p.des1.blen := len;
    if p = d.des1 then
      d.des1 := p.des1
    else
      d.des2 := p.des1;
  end;
  len := newposition.blen;
  p := newposition.anc;
  p.anc := root;
  p.blen := len;
  newposition.anc := root;
  newposition.blen := 0;
  root.des1 := newposition;
  root.des2 := p;

  SetDepth(root);
end;

end.
 
