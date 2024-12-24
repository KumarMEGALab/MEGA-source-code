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

unit MPartitionList;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Classes, StdCtrls, ExtCtrls, SysUtils, StrHashMap,
  MegaConsts, MPartition, MTreeData, MTreeList, mpartitions_blens_list;

type
  TPartitionArray = array of TPartition;

  TTopology = class(TObject)
    Freq: double;
    Value: double;
    Value2: double;
    Dt: integer;
    Partition: PArrayOfLongint;
    NoOfOTUs: integer;
    procedure AddFreq(value : double);
    procedure Assign(Source : TTopology);
    constructor Create(TreeSize : integer);
    destructor Destroy; override;
    function DescString: AnsiString;
  end;

  TTieProc = procedure(var c1,c2: integer; c: array of Integer) of object;

  { TPartitionList }

  TPartitionList = class(TObject)
    TieProc: TTieProc;
  protected
    FHashLookup: TStringHashMap;
    FInformation: TStringList;
    FValueName: AnsiString;
    FValue2Name: AnsiString;
    FFreqName: AnsiString;
    FStatsName: AnsiString;
    FDistanceMatrix: PDistanceMatrix;
    FOTUName: TStringList;
    FisRooted : boolean;

    FNoOfOTUs: integer;
    FMaxNoOfTrees: integer;

    Root: integer;

    PartArray: TPartitionArray;
    Topology: TTopology;

    lstPartition: TList;
    lstTopology: TList;

    BLenArray: PArrayOfDouble;
    FPartitionsSummary: TStringList;
    FErrorMessages: TStringList;

    procedure SetPartitionPatern(tree : PArrayOfNodeData);
    function SetTopology:boolean;
    procedure AddNewPartition(p : TPartition);
    function CompTopology:integer;

    procedure AddTopology;
    function GetNoOfTrees:integer;
    procedure SetMaxNoOfTrees(value : integer);
    function GetNoOfPartitions:integer;
    procedure SetValue(index: integer; value : double);
    function GetValue(index : integer):double;
    procedure SetValue2(index: integer; value : double);
    function GetValue2(index : integer):double;
    procedure SetTotalFrequency(value : double);
    function GetTotalFrequency:double;
    procedure SetFreq(index: integer; value : double);
    function GetFreq(index : integer):double;
    procedure SetBLen(index: integer; value : double);
    function GetBLen(index : integer):double;
    function GetDt(index : integer):integer;

    function GetStatsName:AnsiString;
    function GetMaxStats:double;
    //function GetNoOfOTUs:integer;
    function GetOTUName(index: integer):AnsiString;
    procedure SetOTUName(index: integer; value: AnsiString);

    procedure SetInformation(source: TStringList);

    function GetPartition(index: integer):TPartition;
    function GetIsBLen:boolean;

    function AddWithBLen(tree: PArrayOfNodeData; blen: PArrayOfDouble; OnlyNew: boolean; Value: double; F: double; treeSplits: TTreeData = nil):boolean;
    function GetPartitionsSummary: TStringList;
    function GetPartitionsSummaryHeader(NumBootReps: Integer; Cutoff: Double): TStringList;
    function  AddPartition(p : TPartition): boolean;
    procedure InitHashLookup;
  public
    procedure SortByPartitionString;
    procedure SortByPartitionFrequency;
    procedure BuildPartitionsSummary(NumBootReps: Integer; Cutoff: Double=0.5);
    property PartitionsSummary: TStringList read GetPartitionsSummary;
    property Information: TStringList read FInformation write SetInformation;
    property ValueName: AnsiString read FValueName write FValueName;
    property Value2Name: AnsiString read FValue2Name write FValue2Name;
    property FreqName: AnsiString read FFreqName write FFreqName;
    property StatsName: AnsiString read GetStatsName write FStatsName;
    property MaxStats: Double read GetMaxStats;
    property DistanceMatrix: PDistanceMatrix read FDistanceMatrix write FDistanceMatrix;
    property OTUName[Index: integer]: AnsiString read GetOTUName write SetOTUName;
    property isRooted: boolean read FisRooted;
    property isBLen: boolean read GetIsBLen;

    property NoOfOTUs: integer read FNoOfOTUs;//GetNoOfOTUs;
    property NoOfTrees: integer read GetNoOfTrees;
    property MaxNoOfTrees: integer read FMaxNoOfTrees write SetMaxNoOfTrees;
    property NoOfPartitions: integer read GetNoOfPartitions;
    property TotalFrequency: double read GetTotalFrequency write SetTotalFrequency;
    property Value[index: integer]: double read GetValue write SetValue;
    property Value2[index: integer]: double read GetValue2 write SetValue2;
    property Freq[index: integer]: double read GetFreq write SetFreq;
    property Dt[index: integer]: integer read GetDt;

    property Partition[index: integer]: TPartition read GetPartition; default;
    property BLen[index: integer]: double read GetBLen write SetBLen;

    function FindTreeWithValueGreaterThan(valueToTest: Double): Integer;
    procedure Reset;
    procedure ClearData;
    procedure RemoveTree(index: Integer);
    procedure Join(Source: TPartitionList);

    function ClonePartitions: TList;
    function  PartitionIndex(partition : TPartition):integer;
    function  Compare(tree: PArrayOfNodeData; freq : PArrayOfDouble):integer;
    function GetPartitionStringsForTreeData(tree: TTreeData): TStringList; overload;
    procedure GetPartitionStringsForTreeData(tree: TTreeData; var partitionStrings: TStringList); overload;
    function  Add(tree: PArrayOfNodeData; Value : double):boolean; overload;
    function Add(tree: TTreeData; Value: Double): Boolean; overload;
    function  AddWithFreq(tree: PArrayOfNodeData; Value: double; F: double):boolean;
    function  AddTreeData(treedata: TTreeData; OnlyNew: boolean):boolean;
    procedure AddTreeList(treelist: TTreeList; OnlyNew: boolean);
    function  AddIfNewTree(tree: PArrayOfNodeData; Value: double; F: double):boolean;

                  // The last node of tree must be the root.

    procedure DeletePartition(index: integer);
                  //  NoOfTrees must be 0

    function  ExtractTreeData(index: integer):TTreeData;
    function  ExtractTreeInfo(Tree: TTreeData):integer;
    procedure ExtractTreeList(TreeList: TTreeList);
    procedure GetConsensus(tree: TTreeData); virtual;
    procedure SetDt(RefTree: PArrayOfNodeData);
    procedure SortTreeByFreq;
    procedure SortTreeByValue;
    procedure SortTreeByDt;
    function ToInfoString(simple: Boolean = False): AnsiString;

    constructor Create(TreeSize, Max: integer; rooted: boolean; IsBabyBStrap: Boolean=False);  // KT: rooted no longer supported.
    destructor Destroy; override;
    procedure Assign(Source: TPartitionList);
    function ReleasePartitionBLensList: TPartitionsBLensList;
    procedure RemoveLowFrequencyPartitions(cutoff: Double);
    function GetPartitionByHashLookup(partitionString: String): TPartition;
    property ErrorMessages: TStringList read FErrorMessages write FErrorMessages;
  end;

  TPartitionListArray = array of TPartitionList;

  function ComparePartitionStrings(item1: Pointer; item2: Pointer): Integer;
  function ComparePartitionFrequencies(item1: Pointer; item2: Pointer): Integer;

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MD_MegaMain, {$IFDEF DEBUG}MegaUtils_NV,{$ENDIF}
  {$ENDIF}
  MGlobalSettings, MegaUtils, StringUtils, math;

function ComparePartitionStrings(item1: Pointer; item2: Pointer): Integer;
var
  str1: String = '';
  str2: String = '';
begin
  str1 := TPartition(item1).GetPartitionString;
  str2 := TPartition(item2).GetPartitionString;
  Result := CompareStr(str1, str2);
end;

function ComparePartitionFrequencies(item1: Pointer; item2: Pointer): Integer;
var
  freq1: Double = 0.0;
  freq2: Double = 0.0;
begin
  freq1 := TPartition(item1).Freq;
  freq2 := TPartition(item2).Freq;
  Result := CompareValue(freq2, freq1);
end;

/////////////////////
// TTopology
/////////////////////

constructor TTopology.Create(TreeSize : integer);
var i : integer;
begin
    Inherited Create;
    NoOfOTUs := TreeSize;
    Freq := 0.0;
    Value := 0.0;
    Dt := 0;
    if TreeSize > 0 then begin
        GetMem(Partition, SizeOf(longint)*(NoOfOTUs-3));
        for i := 0 to NoOfOTUs-4 do
            Partition[i] := 0;
    end
    else
        Partition := nil;
end;

destructor TTopology.Destroy;
begin
  FreeMemAndNil(Partition);
  Inherited Destroy;
end;

procedure TTopology.Assign(Source : TTopology);
var i : integer;
begin
  Freq := Source.Freq;
  Value := Source.Value;
  Value2 := Source.Value2;
  Dt := Source.Dt;
  if NoOfOTUs <> Source.NoOfOTUs then begin
    NoOfOTUs := Source.NoOfOTUs;
    FreeMemAndNil(Partition);
    GetMem(Partition, SizeOf(longint)*(NoOfOTUs-3));
  end;
  if NoOfOTUs > 0 then
    for i := 0 to NoOfOTUs-4 do
      Partition[i] := Source.Partition[i];
end;

procedure TTopology.AddFreq(value : double);
begin
    Freq := Freq +value;
end;

///////////////////
//  TPartitionList
///////////////////

constructor TPartitionList.Create(TreeSize, Max: integer; rooted: boolean; IsBabyBStrap: Boolean=False);
var
  i : integer;
begin
  Assert(TreeSize > 0);
  FHashLookup := nil;
  Assert(not rooted, 'rooted tree not supported by TPartitionList');
  FPartitionsSummary := TStringList.Create;
  FisRooted    := false;
  FInformation := TStringList.Create;
  FValueName := '';
  FFreqName := 'Frequency';
  FDistanceMatrix := nil;
  FNoOfOTUs := TreeSize;
  FMaxNoOfTrees := Max;
  FOTUName := TStringList.Create;
  lstPartition := TList.Create;
  lstTopology := TList.Create;
  SetLength(PartArray, FNoOfOTUs - 3);
  for i := 0 to FNoOfOTUs-4 do
  begin
    PartArray[i] := TPartition.Create;
    PartArray[i].Size := FNoOfOTUs;
  end;

  GetMem(BLenArray, SizeOf(double)*FNoOfOTUs);
  for i := 0 to FNoOfOTUs-1 do
    BLenArray[i] := 0.0;

  Topology := TTopology.Create(FNoOfOTUs);
  FErrorMessages := TStringList.Create;
  FErrorMessages.Duplicates := dupIgnore;

end;

destructor TPartitionList.Destroy;
var
  i : integer;
begin
    Reset;
    if Assigned(FPartitionsSummary) then
      FPartitionsSummary.Free;
    if PartArray <> nil then
    begin
      for i := 0 to NoOfOTUs-4 do
          FreeAndNil(PartArray[i]);
      SetLength(PartArray, 0);
    end;
    if Assigned(Topology) then
      FreeAndNil(Topology);
    if Assigned(FOtuName) then
      FreeAndNil(FOTUName);
    if Assigned(FInformation) then
      FreeAndNil(FInformation);
    if Assigned(lstPartition) then
      FreeAndNil(lstPartition);
    if Assigned(lstTopology) then
      FreeAndNil(lstTopology);
    if Assigned(BLenArray) then
      FreeMemAndNil(BLenArray);
    if Assigned(FDistanceMatrix) then
      FreeDistMatrix(FDistanceMatrix, NoOfOtus);
    if Assigned(FErrorMessages) then
      FreeAndNil(FErrorMessages);
    inherited Destroy;
end;

{
function TPartitionList.GetNoOfOTUs:integer;
begin
  if isRooted then
    Result := FNoOfOTUs -1
  else
    Result := FNoOfOTUs;
end;
}
procedure TPartitionList.Reset;
var i : integer;
    p: TPartition;
    t: TTopology;
begin
    if lstTopology.Count > 0 then
        for i := lstTopology.Count-1 downto 0 do begin
            t := lstTopology.Items[i];
            lstTopology.Delete(i);
            t.Free;
        end;
    if lstPartition.Count > 0 then
        for i := lstPartition.Count-1 downto 0 do begin
            p := lstPartition.Items[i];
            lstPartition.Delete(i);
            p.Free;
        end;
end;

procedure TPartitionList.ClearData;
begin
  if Assigned(lstTopology) then
    lstTopology.Clear;
  if Assigned(lstPartition) then
    lstPartition.Clear;
end;

procedure TPartitionList.RemoveTree(index: Integer);
begin
  if Assigned(lstTopology) then
    lstTopology.Delete(index);
end;

procedure SortIntArray(var p : TArrayOfLongint; L, R : integer);
var i,j,x,t : integer;
begin
    x := p[(L+R) div 2];
    i := L-1;
    j := R+1;
    repeat
        repeat Inc(i) until x <= p[i];
        repeat dec(j) until p[j] <= x;
        t := p[i]; p[i] := p[j]; p[j] := t;
    until i >= j;
    p[j] := p[i]; p[i] := t;
    if L < i-1 then SortIntArray(p, L, i-1);
    if j+1 < R then SortIntArray(p, J+1, R);
end;

procedure TPartitionList.AddTopology;
var nt : TTopology;
begin
    nt := TTopology.Create(FNoOfOTUs);
    nt.Assign(Topology);
    if lstTopology.Count = lstTopology.Capacity then
        lstTopology.Expand;
    lstTopology.Add(nt);
end;

function TPartitionList.ExtractTreeData(index : integer):TTreeData;
var
  tree: TTreeData;
  ni: array of integer;

  procedure MakeCluster(node, des1, des2: integer; p: TPartition);
  begin
    tree.NodeArray[node].des1 := ni[des1];
    tree.NodeArray[node].des2 := ni[des2];
    ni[des1] := node +NoOfOTUs;
    tree.BLenArray[node] := p.BLen;
    tree.StatsArray[node] := p.Freq;
  end;

var
  t: TTopology;
  p: TPartition;
  clustered, flag: array of boolean;
  c: array[0..2] of integer;
  n: array of array[0..1] of integer;
  i,j,k,s: integer;
  state: boolean;
begin
  result := nil;
  if (index < 0) or (index > NoOfTrees-1) then Exit;
  tree := nil;

  try
    tree := TTreeData.Create(NoOfOTUs, true, false, true);
    tree.Freq := Freq[index];
    tree.Value := Value[index];
    tree.Value2 := Value2[index];
    setlength(ni, NoOfOTUs);
    setlength(n, NoOfOTUs);
    setlength(clustered, NoOfOTUs);
    setlength(flag, NoOfOTUs);
    for i := 0 to NoOfOTUs-1 do
      ni[i] := i;
    for i := 0 to NoOfOTUs-1 do
      clustered[i] := false;
    for i := 0 to NoOfOTUs-1 do
      flag[i] := false;
    for i := 0 to NoOfOTUs-1 do
    begin
      n[i][0] := 0;
      n[i][1] := 0;
    end;

    t := lstTopology[index];
    for j := 0 to NoOfOTUs-4 do
    begin
      p := lstPartition[t.Partition[j]];
      for k := 0 to NoOfOTUs-1 do
        if p[k] then
          inc(n[j][1])
        else
          inc(n[j][0]);
    end;

    for i := 0 to NoOfOTUs-4 do
    begin
      c[0] := 0;
      for j := 0 to NoOfOTUs-4 do
      begin
        if flag[j] then continue;
        if (n[j][0] = 2) or (n[j][1] = 2) then
        begin
          if n[j][1] = 2 then
            s := 1
          else
            s := 0;
          state := s = 1;

          p := lstPartition[t.Partition[j]];
          for k := 0 to NoOfOTUs-1 do
          begin
            if clustered[k] then continue;
            if p[k] = state then
            begin
              inc(c[0]);
              c[c[0]] := k;
            end;
          end;
          flag[j] := true;
          break;
        end;
      end;
      MakeCluster(i, c[1], c[2], p);
      clustered[c[2]] := true;
      k := c[2];
      for j := 0 to NoOfOTUs-4 do
      begin
        if flag[j] then continue;
        p := lstPartition[t.Partition[j]];
        if p[k] then
          dec(n[j][1])
        else
          dec(n[j][0]);
      end;
    end;
    for i := NoOfOTUs-3 to NoOfOTUs-2 do
    begin
      c[0] := 0;
      for k := 0 to NoOfOTUs-1 do
      begin
        if clustered[k] then continue;
        inc(c[0]);
        c[c[0]] := k;
        if c[0] = 2 then
          break;
      end;
      MakeCluster(i, c[1], c[2], p);
      clustered[c[2]] := true;
    end;

    for i := 0 to FNoOfOTUs-1 do
    begin
      tree.BLenArray[i]  := BLenArray[i];
      tree.StatsArray[i] := 0.0;
    end;
    result := tree;
  finally
    setlength(ni, 0);
    setlength(n, 0);
    setlength(clustered, 0);
    setlength(flag, 0);
  end;
end;

function TPartitionList.GetStatsName:AnsiString;
begin
  if FStatsName = '' then
    Result := FFreqName
  else
    Result := FStatsName;
end;

function TPartitionList.GetMaxStats:double;
begin
  Result := TotalFrequency;
end;

procedure TPartitionList.ExtractTreeList(TreeList: TTreeList);
var tree: TTreeData;
    i : integer;
begin
    if NoOfTrees = 0 then Exit;
    if TreeList.NoOfTrees > 0 then
        TreeList.DeleteAll;
    for i := 0 to NoOfTrees-1 do begin
        tree := ExtractTreeData(i);
        TreeList.Add(tree);
    end;
end;

procedure TPartitionList.SetPartitionPatern(tree : PArrayOfNodeData);
var p : PArrayOfNodeData;
    i : integer;

    procedure SetPartition(index : integer);
    var i,j,k : integer;
    begin
        if index < FNoOfOTUs then Exit;

        i := index -FNoOfOTUs;
        j := p[i].des1-FNoOfOTUs;
        k := p[i].des2-FNoOfOTUs;
        if p[i].des1 >= FNoOfOTUs then
            SetPartition(p[i].des1);
        if p[i].des2 >= FNoOfOTUs then
            SetPartition(p[i].des2);

        if i = FNoOfOTUs-3 then begin
            if p[i].des1 < FNoOfOTUs then
                PartArray[Root].Bits[p[i].des1] := true
            else
                PartArray[Root].Assign(PartArray[j]);
            if p[i].des2 < FNoOfOTUs then
                PartArray[Root].Bits[p[i].des2] := true
            else
                PartArray[Root].Add(PartArray[k]);
        end
        else begin
            if p[i].des1 < FNoOfOTUs then
                PartArray[i].Bits[p[i].des1] := true
            else if j = FNoOfOTUs-3 then
                PartArray[i].Assign(PartArray[Root])
            else
            PartArray[i].Assign(PartArray[j]);
            if p[i].des2 < FNoOfOTUs then
                PartArray[i].Bits[p[i].des2] := true
            else if k =  FNoOfOTUs-3 then
                PartArray[i].Add(PartArray[Root])
            else
                PartArray[i].Add(PartArray[k]);
        end;
    end;

begin
    Assert(Length(PartArray) = FNoOfOTUs - 3, Format('PartArray has %d elements but expected %d', [Length(PartArray), FNoOfOTUs - 3]));
    for i := 0 to FNoOfOTUs-4 do
        PartArray[i].Reset;
{
    if isRooted then begin
        GetMem(p, SizeOf(TNodeData)*(FNoOfOTUs-1));
        for i := 0 to FNoOfOTUs-3 do begin
            if tree[i].des1 >= (FNoOfOTUs-1) then
                p[i].des1 := tree[i].des1 +1
            else
                p[i].des1 := tree[i].des1;
            if tree[i].des2 >= (FNoOfOTUs-1) then
                p[i].des2 := tree[i].des2 +1
            else
                p[i].des2 := tree[i].des2;
        end;
        p[FNoOfOTUs-2].des1 := FNoOfOTUs-1;
        p[FNoOfOTUs-2].des2 := 2*FNoOfOTUs-3;
    end
    else
}
        p := tree;

    if p[FNoOfOTUs-2].des1 < FNoOfOTUs then begin
        Root := p[FNoOfOTUs-2].des2-FNoOfOTUs;
        SetPartition(p[p[FNoOfOTUs-2].des2-FNoOfOTUs].des1);
        SetPartition(p[p[FNoOfOTUs-2].des2-FNoOfOTUs].des2);
    end
    else if p[FNoOfOTUs-2].des2 < FNoOfOTUs then begin
        Root := p[FNoOfOTUs-2].des1-FNoOfOTUs;
        SetPartition(p[p[FNoOfOTUs-2].des1-FNoOfOTUs].des1);
        SetPartition(p[p[FNoOfOTUs-2].des1-FNoOfOTUs].des2);
    end
    else if p[FNoOfOTUs-2].des1 = 2*FNoOfOTUs-3 then begin
        Root := p[FNoOfOTUs-2].des1-FNoOfOTUs;
        SetPartition(p[FNoOfOTUs-2].des2);
        SetPartition(p[p[FNoOfOTUs-2].des1-FNoOfOTUs].des1);
        SetPartition(p[p[FNoOfOTUs-2].des1-FNoOfOTUs].des2);
    end
    else begin
        Root := p[FNoOfOTUs-2].des2-FNoOfOTUs;
        SetPartition(p[FNoOfOTUs-2].des1);
        SetPartition(p[p[FNoOfOTUs-2].des2-FNoOfOTUs].des1);
        SetPartition(p[p[FNoOfOTUs-2].des2-FNoOfOTUs].des2);
    end;

    for i := 0 to FNoOfOTUs-4 do
      PartArray[i].Anchor(FNoOfOTUs-1);
//    if isRooted then
//        FreeMemAndNil(p);
end;

procedure TPartitionList.AddNewPartition(p : TPartition);
var
  np : TPartition;
begin
    np := TPartition.Create;
    np.Assign(p);
    if lstPartition.Count = lstPartition.Capacity then
        lstPartition := lstPartition.Expand;
    lstPartition.Add(np);
end;

function TPartitionList.CompTopology:integer;
var i, j : integer;
    t : TTopology;
begin
    if NoOfTrees = 0 then begin
        Result := -1;
        Exit;
    end;
    for i := 0 to NoOfTrees-1 do begin
        Result := i;
        t := lstTopology.Items[i];
        for j := FNoOfOTUs-4 downto 0 do
            if Topology.Partition[j] <> t.Partition[j] then begin
                Result := -1;
                Break;
            end;
        if Result = i then Exit;
    end;
    Result := -1;
end;

function TPartitionList.SetTopology:boolean;
var p : TPartition;
    i,j : integer;
    flag : boolean;
begin
  Result := true;
  flag   := False; //@SK mega2b4
  for i := 0 to FNoOfOTUs-4 do
  begin
    if lstPartition.Count = 0 then
        Result := false
    else
      for j := 0 to lstPartition.Count-1 do
      begin
          p := lstPartition.Items[j];
          flag := p.IsIdentical(PartArray[i]);
          if flag then
          begin
            Topology.Partition[i] := j;
            Break;
          end;
      end;
    if not flag then
    begin
      Topology.Partition[i] := -1;
      Result := false;
    end;
  end;
end;

function TPartitionList.Compare(tree : PArrayOfNodeData; freq : PArrayOfDouble):integer;
var i : integer;
    p : TPartition;
begin
    SetPartitionPatern(tree);
    if SetTopology then
        Result := 0
    else
        Result := -1;

    if freq <> nil then
    begin
        for i := 0 to FNoOfOTUs-4 do
            if Topology.Partition[i] = -1 then
                freq[i] := 0.0
            else
            begin
                p := lstPartition.Items[Topology.Partition[i]];
                freq[i] := p.Freq;
            end;

//        if not isRooted then
        begin
            freq[FNoOfOTUs-3] := freq[Root];
            if (tree[FNoOfOTUs-2].des1 < FNoOfOTUs)
            or (tree[FNoOfOTUs-2].des2 < FNoOfOTUs) then
              freq[Root] := 0.0
            else
            begin
                if tree[FNoOfOTUs-2].des1 = 2*FNoOfOTUs-3 then
                    i := tree[FNoOfOTUs-2].des2-FNoOfOTUs
                else
                    i := tree[FNoOfOTUs-2].des1-FNoOfOTUs;
                if Topology.Partition[i] = -1 then
                    freq[Root] := 0.0
                else
                begin
                    p := lstPartition.Items[Topology.Partition[i]];
                    freq[Root] := p.Freq;
                end;
            end;
        end;
    end;
    if Result = 0 then
    begin
        SortIntArray(Topology.Partition^, 0, FNoOfOTUs-4);
        Result := CompTopology;
    end;
end;

function TPartitionList.GetPartitionStringsForTreeData(tree: TTreeData): TStringList;
var
  i : integer;
  p : TPartition;
begin
  Result := nil;
  SetPartitionPatern(tree.NodeArray);
  SetTopology;
  Result := TStringList.Create;
  for i := 0 to FNoOfOTUs - 4 do
  begin
    if Topology.Partition[i] <> -1 then
    begin
      p := lstPartition.Items[Topology.Partition[i]];
      Result.Add(p.GetPartitionString);
    end
    else
      Result.Add(NOT_FOUND_STR);
  end;
end;

procedure TPartitionList.GetPartitionStringsForTreeData(tree: TTreeData;var partitionStrings: TStringList);
var
  i : integer;
  p : TPartition;
begin
  partitionStrings.Clear;
  SetPartitionPatern(tree.NodeArray);
  SetTopology;
  for i := 0 to FNoOfOTUs - 4 do
  begin
    if Topology.Partition[i] <> -1 then
    begin
      p := lstPartition.Items[Topology.Partition[i]];
      partitionStrings.Add(p.GetPartitionString);
    end
    else
      partitionStrings.Add(NOT_FOUND_STR);
  end;
end;

function TPartitionList.ExtractTreeInfo(Tree: TTreeData):integer;
var
  i : integer;
  p : TPartition;
begin
  SetPartitionPatern(tree.NodeArray);
  if SetTopology then
    Result := 0
  else
    Result := -1;

  p:= nil;

  if GetIsBLen then
    Tree.isBLen := true
  else
    Tree.isBLen := false;
//  Tree.isStats := true;

  for i := 0 to FNoOfOTUs-1 do
    Tree.StatsArray[i] := 0;
  if Tree.isBLen then
    for i := 0 to FNoOfOTUs-1 do
      Tree.BLenArray[i] := BLenArray[i];

  for i := 0 to FNoOfOTUs-4 do
    if Topology.Partition[i] = -1 then
    begin
      Tree.StatsArray[i+FNoOfOTUs] := 0.0;
      if Tree.isBLen then
        Tree.BLenArray[i+FNoOfOTUs]  := 0.0;
    end
    else
    begin
      p := lstPartition.Items[Topology.Partition[i]];
      Tree.StatsArray[i+FNoOfOTUs] := p.Freq;
      if Tree.isBLen then
        Tree.BLenArray[i+FNoOfOTUs]  := p.BLen;
    end;

//  if not isRooted then
  begin
    Tree.StatsArray[2*FNoOfOTUs-3] := Tree.StatsArray[Root];
    if (Tree[FNoOfOTUs-2].des1 < FNoOfOTUs)
    or (Tree[FNoOfOTUs-2].des2 < FNoOfOTUs) then
    begin
      Tree.StatsArray[Root+FNoOfOTUs] := 0.0;
      if Tree.isBLen then
        Tree.BLenArray[Root+FNoOfOTUs]  := p.BLen;
    end
    else
    begin
      if tree[FNoOfOTUs-2].des1 = 2*FNoOfOTUs-3 then
        i := Tree[FNoOfOTUs-2].des2-FNoOfOTUs
      else
        i := tree[FNoOfOTUs-2].des1-FNoOfOTUs;
      if Topology.Partition[i] = -1 then
      begin
        Tree.StatsArray[Root+FNoOfOTUs] := 0.0;
        if Tree.isBLen then
          Tree.BLenArray[Root+FNoOfOTUs]  := 0.0;
      end
      else
      begin
        p := lstPartition.Items[Topology.Partition[i]];
        Tree.StatsArray[Root+FNoOfOTUs] := p.Freq;
        if Tree.isBLen then
          Tree.BLenArray[Root+FNoOfOTUs]  := p.BLen;
      end;
    end;
  end;

  if Result = 0 then
  begin
    SortIntArray(Topology.Partition^, 0, FNoOfOTUs-4);
    Result := CompTopology;
  end;
end;

function TPartitionList.PartitionIndex(partition : TPartition):integer;
var p, q : TPartition;
    i : integer;
begin
    q := TPartition.Create;
    q.Size := FNoOfOTUs;
    Result := -1;
    for i := 0 to lstPartition.Count-1 do
    begin
      p := lstPartition.Items[i];
      q.Assign(p);
      q.Complement;
      if p.IsIdentical(partition) or q.IsIdentical(partition) then
      begin
        Result := i;
        Break;
      end;
    end;
    q.Free;
end;

procedure TPartitionList.SetDt(RefTree : PArrayOfNodeData);
var i,j,k,n : integer;
    t : TTopology;
begin
    if NoOfTrees = 0 then Exit;
    SetPartitionPatern(RefTree);
    SetTopology;
    SortIntArray(Topology.Partition^, 0, FNoOfOTUs-4);
    for i := 0 to NoOfTrees-1 do begin
        t := lstTopology[i];
        t.Dt := 0;
        n := FNoOfOTUs-4;
        for j := FNoOfOTUs-4 downto 0 do begin
            for k := n downto 0 do
                if Topology.Partition[k] > t.Partition[j] then
                    Continue
                else if Topology.Partition[k] = t.Partition[j] then begin
                    n := k-1;
                    Break;
                end
                else if Topology.Partition[k] < t.Partition[j] then begin
                    Inc(t.Dt, 2);
                    n := k;
                    Break;
                end;
        end;
    end;
end;

function TPartitionList.Add(tree : PArrayOfNodeData; Value : double):boolean;
begin
  Result := AddWithFreq(tree, Value, 1.0);
end;

function TPartitionList.Add(tree: TTreeData; Value: Double): Boolean;
begin
  Result := AddWithBLen(tree.NodeArray, tree.BLenArray, False, Value, 1.0, tree);
end;

function TPartitionList.AddTreeData(treedata: TTreeData; OnlyNew: boolean):boolean;
begin
  if treedata.NoOfOTUs <> FNoOfOTUs then
  begin
    Result := false;
    Exit;
  end;
  if treedata.isBLen then
    Result := AddWithBLen(treedata.NodeArray, treedata.BLenArray, OnlyNew, treedata.Value, treedata.Freq)
  else if OnlyNew then
    Result := AddIfNewTree(treedata.NodeArray, treedata.Value, treedata.Freq)
  else
    Result := AddWithFreq(treedata.NodeArray, treedata.Value, treedata.Freq);
end;

procedure TPartitionList.AddTreeList(treelist: TTreeList; OnlyNew: boolean);
var i : integer;
begin
    if treelist.NoOfOTUs <> NoOfOTUs then Exit;
    for i := 0 to treelist.NoOfTrees-1 do
      AddTreeData(treelist[i], OnlyNew);
end;

function  TPartitionList.AddPartition(p : TPartition): boolean;
var
  i: integer;
  q: TPartition;
begin
  result := false;
  if p.Size <> FNoOfOTUs then exit;

  p.Anchor(FNoOfOTUs-1);

  for i := 0 to lstPartition.Count-1 do
  begin
    q := lstPartition.Items[i];
    if p.IsIdentical(q) then
    begin
      q.BLen := (p.BLen*p.Freq +q.BLen*q.Freq)/(p.Freq +q.Freq);
      q.AddFreq(p.Freq);
      result := true;
    end;
  end;

  if not result then
  begin
    AddNewPartition(p);
    result := true;
  end;
end;

procedure TPartitionList.SortByPartitionString;
begin
  if Assigned(lstPartition) and (lstPartition.Count > 0) then
    lstPartition.Sort(ComparePartitionStrings);
end;

procedure TPartitionList.SortByPartitionFrequency;
begin
  if Assigned(lstPartition) and (lstPartition.Count > 0) then
    lstPartition.Sort(ComparePartitionFrequencies);
end;


function TPartitionList.AddWithBLen(tree: PArrayOfNodeData; blen: PArrayOfDouble; OnlyNew: boolean; Value: double; F: double; treeSplits: TTreeData = nil):boolean;
var i,j,n : integer;
    flag : boolean;
    p : TPartition;
    t : TTopology;
    tf: double;
    partitionString: String = '';
    branchIndex: Integer = -1;
begin
  Result := false;
  flag   := false;
  tf := GetTotalFrequency;
  SetPartitionPatern(tree);
  n := lstPartition.Count;


  for i := 0 to FNoOfOTUs-4 do
  begin
    if n = 0 then
      flag := false
    else
    begin
      for j := 0 to n - 1 do
      begin
        p := lstPartition.Items[j];
        flag := p.IsIdentical(PartArray[i]);

        if flag then
        begin
          Topology.Partition[i] := j;
          if not OnlyNew then
          begin
            p.BLen := (p.BLen*p.Freq +blen[i+FNoOfOTUs]*F)/(p.Freq +F);
            p.AddFreq(F);
          end;
          Break;
        end;
      end;
    end;
    if not flag then
    begin
      if OnlyNew then
        PartArray[i].Freq := 0.0
      else
      begin
        PartArray[i].Freq := F;
        PartArray[i].BLen := blen[i+FNoOfOTUs];
      end;
      AddNewPartition(PartArray[i]);
      Topology.Partition[i] := lstPartition.Count-1;
    end;
  end;

  if OnlyNew then
  begin
    SortIntArray(Topology.Partition^, 0, FNoOfOTUs-4);
    if lstPartition.Count > n then
      Result := true
    else
    begin
      if CompTopology = -1 then
        Result := true
      else
        Result := false;
    end;
    if Result then
    begin
      if NoOfTrees < MaxNoOfTrees then
      begin
        Topology.Freq := F;
        Topology.Value := Value;
        Topology.Dt := 0;
        AddTopology;
      end;
      for i := 0 to  FNoOfOTUs-4 do
      begin
        p := lstPartition.Items[Topology.Partition[i]];
        p.BLen := (p.BLen*p.Freq +blen[i+FNoOfOTUs]*F)/(p.Freq +F);
        p.Freq := p.Freq +F;
      end;
      for i := 0 to NoOfOTUs-1 do
        BLenArray[i] := (blen[i]*F +BlenArray[i]*tf)/(F+tf);
    end
  end
  else
  begin
    if lstPartition.Count > n then
    begin
      if NoOfTrees < MaxNoOfTrees then
      begin
        Topology.Freq := F;
        Topology.Value := Value;
        Topology.Dt := 0;
        SortIntArray(Topology.Partition^, 0, FNoOfOTUs-4);
        AddTopology;
        Result := true;
      end;
    end
    else
    begin
      Topology.Freq := F;
      Topology.Dt := 0;
      SortIntArray(Topology.Partition^, 0, FNoOfOTUs-4);
      i := CompTopology;
      if i > -1 then
      begin
        t := lstTopology.Items[i];
        t.AddFreq(Topology.Freq);
        Result := false;
      end
      else if NoOfTrees < MaxNoOfTrees then
      begin
        AddTopology;
        Result := true;
      end;
    end;
    if tf > 0 then
      for i := 0 to NoOfOTUs-1 do
        BLenArray[i] := (blen[i]*F +BlenArray[i]*tf)/(F+tf);
  end;
end;

function TPartitionList.AddWithFreq(tree : PArrayOfNodeData; Value : double; F : double):boolean;
var i,j,n : integer;
    flag : boolean;
    p : TPartition;
    t : TTopology;
begin
  Result := false;
  flag   := False; //@SK mega2b4
  SetPartitionPatern(tree);
  n := lstPartition.Count;
  for i := 0 to FNoOfOTUs-4 do
  begin
    if n = 0 then
      flag := false
    else
      for j := 0 to n-1 do
      begin
        p := lstPartition.Items[j];
        flag := p.IsIdentical(PartArray[i]);
        if flag then
        begin
          Topology.Partition[i] := j;
          p.AddFreq(F);
          Break;
        end;
      end;
    if not flag then
    begin
      PartArray[i].Freq := F;
      AddNewPartition(PartArray[i]);
      Topology.Partition[i] := lstPartition.Count-1;
    end;
  end;


  { TODO 5 -oDan -cSKumar An Access violation occurs if there are less than 4 seqs.  The problem shows itself at the SortIntArray which expects there to be 4 or more seqs.  Likely an option is missing it's check for # of seqs somewhere.}
  if lstPartition.Count > n then
  begin
    if NoOfTrees < MaxNoOfTrees then
    begin
      Topology.Freq := F;
      Topology.Value := Value;
      Topology.Dt := 0;
      SortIntArray(Topology.Partition^, 0, FNoOfOTUs-4);
      AddTopology;
      Result := true;
    end;
  end
  else
  begin
    Topology.Freq := F;
    Topology.Dt := 0;
    SortIntArray(Topology.Partition^, 0, FNoOfOTUs-4);
    i := CompTopology;
    if i > -1 then
    begin
      t := lstTopology.Items[i];
      t.AddFreq(Topology.Freq);
      Result := false;
    end
    else if NoOfTrees < MaxNoOfTrees then
    begin
      AddTopology;
      Result := true;
    end;
  end;
end;

procedure TPartitionList.Assign(Source: TPartitionList);
var
  i: Integer;
  APartition: TPartition;
  ATopology: TTopology;
begin
  FInformation := TStringList.Create;
  if Assigned(Source.FInformation) then
    FInformation.Assign(Source.FInformation);
  FValueName := Source.FValueName;
  FValue2Name := Source.FValue2Name;
  FFreqName := Source.FFreqName;
  FStatsName := Source.FStatsName;
  if Assigned(Source.FDistanceMatrix) then
  begin
    FDistanceMatrix := CreateDistanceMatrix(Source.FNoOFOtus);
    CopyDistanceMatrix(Source.FDistanceMatrix, FDistanceMatrix, Source.FNoOfOtus);
  end;

  FOTUName := TStringList.Create;
  if Assigned(Source.FOTUName) then
    FOTUName.Assign(Source.FOTUName);
  FisRooted := Source.FIsRooted;
  FNoOfOTUs := Source.FNoOfOTUs;
  FMaxNoOfTrees := Source.FMaxNoOfTrees;
  Root := Source.Root;
  if Assigned(Source.Topology) then
  begin
    Topology := TTopology.Create(Source.FNoOfOtus);
    Topology.Assign(Source.Topology);
  end;

  SetLength(PartArray, FNoOfOTUs - 3);
  for i := 0 to FNoOfOTUs - 4 do
  begin
    PartArray[i] := TPartition.Create;
    PartArray[i].Size := FNoOfOTUs;
    PartArray[i].Assign(Source.PartArray[i]);
  end;

  GetMem(BLenArray, SizeOf(double)*FNoOfOTUs);
  for i := 0 to FNoOfOTUs-1 do
    BLenArray[i] := Source.BLenArray[i];
  if Assigned(Source.lstPartition) then
  begin
    lstPartition := TList.Create;
    if Source.lstPartition.Count > 0 then
    begin
      for i := 0 to Source.lstPartition.Count - 1 do
      begin
        APartition := TPartition.Create;
        APartition.Assign(Source.lstPartition[i]);
        lstPartition.Add(APartition);
      end;
    end;
  end;
  if Assigned(Source.lstTopology) then
  begin
    lstTopology := TList.Create;
    if Source.lstTopology.Count > 0 then
    begin
      for i := 0 to Source.lstTopology.Count - 1 do
      begin
        ATopology := TTopology.Create(Source.FNoOfOtus);
        ATopology.Assign(Source.lstTopology[i]);
        lstTopology.Add(ATopology);
      end;
    end;
  end;
end;

function TPartitionList.ReleasePartitionBLensList: TPartitionsBLensList;
begin
end;

procedure TPartitionList.RemoveLowFrequencyPartitions(cutoff: Double);
var
  i: Integer;
  p: TPartition = nil;
begin
  if Assigned(lstPartition) and (lstPartition.Count > 0) then
    for i := lstPartition.Count - 1 downto 0 do
    begin
      p := lstPartition[i];
      if CompareValue(p.Freq, cutoff, FP_CUTOFF) < 0 then
      begin
        p.Free;
        lstPartition.Delete(i);
      end;
    end;
end;

procedure TPartitionList.InitHashLookup;
var
  p: TPartition = nil;
  i: Integer;
begin
  FHashLookup := TStringHashMap.Create;
  if lstPartition.Count > 0 then
    for i := 0 to lstPartition.Count - 1 do
    begin
      p := TPartition(lstPartition[i]);
      FHashLookup.Add(p.GetPartitionString, p);
    end;
end;

function TPartitionList.GetPartitionByHashLookup(partitionString: String): TPartition;
var
  p: Pointer = nil;
begin
  if not Assigned(FHashLookup) then
    InitHashLookup;
  if FHashLookup.Find(partitionString, p) then
    Result := TPartition(p)
  else
    Result := nil;
end;

procedure TPartitionList.BuildPartitionsSummary(NumBootReps: Integer; Cutoff: Double=0.5);
var
  i: Integer;
  Partition: TPartition;
  Temp: String;
  CutoffVal: Double;
  Header: TStringList;
begin
  Assert(NumBootReps > 0);
  Header := nil;
  {$IFNDEF VISUAL_BUILD}
  CutoffVal := D_MegaMain.PartitionFrequencyCutoff;
  {$ELSE}
  CutoffVal := Cutoff;
  {$ENDIF}
  try
    FPartitionsSummary.Clear;
    Header := GetPartitionsSummaryHeader(NumBootReps, CutoffVal);
    if lstPartition.Count > 0 then
      for i := 0 to lstPartition.Count - 1 do
      begin
        Partition := TPartition(lstPartition[i]);
        if (Partition.Freq / NumBootReps) < CutoffVal then
          continue;
        if Partition.Size = 0 then
          Temp := '-'
        else
          Temp := Partition.GetPartitionString;

        Temp := Temp + #9 + Format('%.0f%%', [Partition.Freq / NumBootReps * 100]);
        FPartitionsSummary.Add(Temp);
      end;
      FPartitionsSummary.CustomSort(PartitionsListStringsCompare);
      FPartitionsSummary.Insert(0, EmptyStr);
      for i := Header.Count - 1 downto 0 do
        FPartitionsSummary.Insert(0, Header[i]);
  finally
    if Assigned(Header) then
      Header.Free;
  end;
end;

function TPartitionList.FindTreeWithValueGreaterThan(valueToTest: Double): Integer;
var
  i: Integer = -1;
begin
  Result := -1;
  if NoOfTrees = 0 then
    Exit;
  for i := NoOfTrees - 1 downto 0 do
    if CompareValue(Value[i], valueToTest, FP_CUTOFF) > 0 then
    begin
      Result := i;
      Exit;
    end;
end;

function TPartitionList.AddIfNewTree(tree : PArrayOfNodeData; Value : double; F : double):boolean;
var i,j,n : integer;
    flag : boolean;
    p : TPartition;
begin
  SetPartitionPatern(tree);
  n := lstPartition.Count;
  flag   := False; //@SK mega2b4

  for i := 0 to FNoOfOTUs-4 do
  begin
    if n = 0 then
      flag := false
    else
      for j := 0 to n-1 do
      begin
        p := lstPartition.Items[j];
        flag := p.IsIdentical(PartArray[i]);
        if flag then
        begin
          Topology.Partition[i] := j;
          Break;
        end;
      end;
    if not flag then
    begin
      PartArray[i].Freq := 0.0;
      AddNewPartition(PartArray[i]);
      Topology.Partition[i] := lstPartition.Count-1;
    end;
  end;

  SortIntArray(Topology.Partition^, 0, FNoOfOTUs-4);
  if lstPartition.Count > n then
    Result := true
  else begin
    CompTopology;
    if CompTopology = -1 then
      Result := true
    else
      Result := false;
  end;

  if Result then begin
    if NoOfTrees < MaxNoOfTrees then
    begin
      Topology.Freq := F;
      Topology.Value := Value;
      Topology.Dt := 0;
      AddTopology;
    end;
    for i := 0 to  FNoOfOTUs-4 do
    begin
      p := lstPartition.Items[Topology.Partition[i]];
      p.Freq := p.Freq +F;
    end;
  end;
end;

procedure TPartitionList.Join(Source: TPartitionList);
var s,t: TTopology;
    p,q: TPartition;
    i,j,k,n: integer;
    h: PArrayOfLongInt;
begin
    if Source.NoOfOTUs <> NoOfOTUs then Exit;
//    if Source.isRooted <> isRooted then Exit;
    if Source.NoOfPartitions = 0 then Exit;

    GetMem(h, SizeOf(longint)*Source.lstPartition.Count);
    n := lstPartition.Count-1;
    for i := 0 to Source.lstPartition.Count-1 do begin
        h[i] := -1;
        p := Source.lstPartition[i];
        if lstPartition.Count > 0 then
            for j := 0 to n do begin
                q := lstPartition[j];
                if q.IsIdentical(p) then begin
                    q.BLen := (q.BLen*q.Freq +p.BLen*p.Freq)/(q.Freq +p.Freq);
                    q.Freq := q.Freq +p.Freq;
                    h[i] := j;
                    Break;
                end;
            end;
        if h[i] = -1 then begin
            AddNewPartition(p);
            h[i] := lstPartition.Count-1;
        end;
    end;

    if Source.NoOfTrees > 0 then
        for i := 0 to Source.lstTopology.Count-1 do begin
            s := Source.lstTopology[i];
            k := 0;
            Topology.Freq := s.Freq;
            Topology.Value := s.Value;
            Topology.Dt := s.Dt;
            for j := 0 to FNoOfOTUs-4 do begin
                Topology.Partition[j] := h[s.Partition[j]];
                if Topology.Partition[j] > n then k := -1;
            end;
            SortIntArray(Topology.Partition^, 0, FNoOfOTUs-4);
            if k = 0 then k := CompTopology;
            if k = -1 then begin
                if NoOfTrees < MaxNoOfTrees then AddTopology;
            end
            else begin
                t := lstTopology[k];
                t.Freq := t.Freq +s.Freq;
            end;
        end;

    FreeMemAndNil(h);
end;

function TPartitionList.ClonePartitions: TList;
var
  p: TPartition = nil;
  pClone: TPartition = nil;
  i: Integer = -1;
begin
  Result := TList.Create;
  if Assigned(lstPartition) and (lstPartition.Count > 0) then
    for i := 0 to lstPartition.Count - 1 do
    begin
      p := TPartition(lstPartition[i]);
      pClone := p.Clone;
      Result.Add(pClone);
    end;
end;

function CompFreq(Item1, Item2 : Pointer):integer;
var p1, p2 : TTopology;
begin
    p1 := Item1;
    p2 := Item2;
    if p1.Freq > p2.Freq then
        Result := -1
    else if  p1.Freq = p2.Freq then
        Result := 0
    else
        Result := 1;
end;

function CompValue(Item1, Item2 : Pointer):integer;
var p1, p2 : TTopology;
begin
    p1 := Item1;
    p2 := Item2;
    if p1.Value < p2.Value then
        Result := -1
    else if p1.Value = p2.Value then
        Result := 0
    else
        Result := 1;
end;

function CompDt(Item1, Item2 : Pointer):integer;
var p1, p2 : TTopology;
begin
    p1 := Item1;
    p2 := Item2;
    if p1.Dt < p2.Dt then
        Result := -1
    else if p1.Dt = p2.Dt then
        Result := 0
    else
        Result := 1;
end;

procedure TPartitionList.SortTreeByFreq;
begin
    lstTopology.Sort(CompFreq);
end;

procedure TPartitionList.SortTreeByValue;
begin
    lstTopology.Sort(CompValue);
end;

procedure TPartitionList.SortTreeByDt;
begin
    lstTopology.Sort(CompDt);
end;

function TPartitionList.GetNoOfTrees:integer;
begin
    Result := lstTopology.Count;
end;

procedure TPartitionList.SetMaxNoOfTrees(value : integer);
begin
    if value < NoOfTrees then
        FMaxNoOfTrees := NoOfTrees
    else
        FMaxNoOfTrees := value;
end;

function TPartitionList.GetNoOfPartitions:integer;
begin
    Result := lstPartition.Count;
end;

procedure TPartitionList.SetValue(index: integer; value : double);
var t : TTopology;
begin
    if (index < 0) or (index >= lstTopology.Count) then Exit;
    t := lstTopology.Items[index];
    t.Value := value;
end;

function TPartitionList.GetValue(index: integer):double;
var t : TTopology;
begin
    if (index < 0) or (index >= lstTopology.Count) then
        Result := 0.0
    else begin
        t := lstTopology.Items[index];
        Result := t.Value;
    end;
end;

procedure TPartitionList.SetValue2(index: integer; value : double);
var t : TTopology;
begin
    if (index < 0) or (index >= lstTopology.Count) then Exit;
    t := lstTopology.Items[index];
    t.Value2 := value;
end;

function TPartitionList.GetValue2(index: integer):double;
var t : TTopology;
begin
    if (index < 0) or (index >= lstTopology.Count) then
        Result := 0.0
    else begin
        t := lstTopology.Items[index];
        Result := t.Value2;
    end;
end;

procedure TPartitionList.SetFreq(index: integer; value : double);
var t : TTopology;
    p : TPartition;
    i : integer;
begin
    if (index < 0) or (index >= lstTopology.Count) then Exit;
    t := lstTopology.Items[index];
    if value = t.Freq then Exit;
    for i := 0 to FNoOfOTUs-4 do begin
      p := lstPartition.Items[t.Partition[i]];
      p.Freq := p.Freq -t.Freq +value;
    end;
    t.Freq := value;
end;

procedure TPartitionList.SetBLen(index: integer; value : double);
begin
  if (index < 0) or (index >= lstPartition.Count) then Exit;
  TPartition(lstPartition[index]).BLen := value;
end;

function TPartitionList.GetTotalFrequency:double;
var p : TPartition;
    i : integer;
begin
    Result := 0.0;
    for i := 0 to lstPartition.Count-1 do begin
        p := lstPartition[i];
        Result := Result +p.Freq/(NoOfOTUs-3);
    end;
end;

procedure TPartitionList.SetTotalFrequency(value : double);
var t : TTopology;
    p : TPartition;
    i : integer;
    r : double;
begin
    if value = TotalFrequency then Exit;
    r := value/TotalFrequency;
    if lstTopology.Count > 0 then
        for i := 0 to lstTopology.Count-1 do begin
          t := lstTopology.Items[i];
          t.Freq := t.Freq*r;
        end;
    if lstPartition.Count > 0 then
        for i := 0 to lstPartition.Count-1 do begin
          p := lstPartition.Items[i];
          p.Freq := p.Freq*r;
        end;
end;

function TPartitionList.GetFreq(index : integer):double;
var t : TTopology;
begin
    if (index < 0) or (index >= lstTopology.Count) then
        Result := 0.0
    else begin
        t := lstTopology.Items[index];
        Result := t.Freq;
    end;
end;

function TPartitionList.GetBLen(index : integer):double;
begin
  if (index < 0) or (index >= lstPartition.Count) then
    Result := 0.0
  else
    Result := TPartition(lstPartition[index]).BLen;
end;

function TPartitionList.GetDt(index : integer):integer;
var t : TTopology;
begin
    if (index < 0) or (index >= lstTopology.Count) then
        Result := 0
    else begin
        t := lstTopology.Items[index];
        Result := t.Dt;
    end;
end;

procedure TPartitionList.GetConsensus(tree :TTreeData);
var
  ni, h : array of integer;
  clustered, flag : array of boolean;

  procedure SortPartition(L, R : integer);
  var
    p: TPartition;
    x: double;
    i,j,t : integer;
  begin
    p := lstPartition[h[(L+R) div 2]];
    x := p.Freq;
    i := L-1;
    j := R+1;
    repeat
      repeat
        Inc(i);
        p := lstPartition[h[i]];
      until x >= p.Freq;
      repeat
        dec(j);
        p := lstPartition[h[j]];
      until p.Freq >= x;
      t := h[i]; h[i] := h[j]; h[j] := t;
    until i >= j;
    h[j] := h[i]; h[i] := t;
    if L < i-1 then SortPartition(L, i-1);
    if j+1 < R then SortPartition(J+1, R);
  end;

  procedure MakeCluster(node, des1, des2: integer; p: TPartition);
  begin
    tree.NodeArray[node].des1 := ni[des1];
    tree.NodeArray[node].des2 := ni[des2];
    ni[des1] := node +NoOfOTUs;
    if p = nil then
      tree.StatsArray[node] := 0
    else
      tree.StatsArray[node] := p.Freq;

    if assigned(p) and tree.isBLen then
      tree.BLenArray[node] := p.BLen;
  end;

  procedure MarkInconsistentPartition(n1,n2: integer);
  var
    i: integer;
    p: TPartition;
  begin
    for i := 0 to lstPartition.Count-1 do
    begin
      if flag[i] then continue;
      p  := lstPartition[h[i]];
      if p[n1] <> p[n2] then
        flag[i] := true;
    end;

  end;

var
  p: TPartition;
  i,j,k,s,m,mj,ms: integer;
  c: array[0..2] of integer;
  n: array of array[0..1] of integer;
  q: array of TPartition;
  state: boolean;
begin
  try  //@SK mega2b4
    setlength(ni, NoOfOTUs);
    setlength(clustered, NoOfOTUs);
    setlength(n, lstPartition.Count);
    setlength(flag, lstPartition.Count);
    setlength(h, lstPartition.Count);
    setlength(q, NoOfOTUs);
    for i := 0 to NoOfOTUs-1 do
    begin
      ni[i] := i;
      clustered[i] := false;
    end;

    for i := 0 to lstPartition.Count-1 do
    begin
      n[i][0] := 0;
      n[i][1] := 0;
      flag[i] := false;
      h[i] := i;
    end;

    SortPartition(0, lstPartition.Count-1);

    for j := 0 to lstPartition.Count-1 do
    begin
      p := lstPartition[h[j]];
      for k := 0 to NoOfOTUs-1 do
        if p[k] then
          inc(n[j][1])
        else
          inc(n[j][0]);
    end;

    for i := 0 to NoOfOTUs-4 do
    begin
      c[0] := 0;
      m    := NoOfOTUs;
      for j := 0 to lstPartition.Count-1 do
      begin
        if flag[j] then continue;
        if (n[j][0] = 2) or (n[j][1] = 2) then
        begin
          if n[j][1] = 2 then
            s := 1
          else
            s := 0;
          state := s = 1;

          p := lstPartition[h[j]];
          for k := 0 to NoOfOTUs-1 do
          begin
            if clustered[k] then continue;
            if p[k] = state then
            begin
              inc(c[0]);
              c[c[0]] := k;
            end;
          end;
          flag[j] := true;
          q[c[1]] := p;
          break;
        end
        else
        begin
          if (n[j][0] > 1) and (n[j][0] < m) then
          begin
            m  := n[j][0];
            mj := j;
            ms := 0;
          end;
          if (n[j][1] > 1) and (n[j][1] < m) then
          begin
            m  := n[j][1];
            mj := j;
            ms := 1;
          end;
        end;
      end;
      if c[0] = 0 then
        if m < NoOfOTUs then
        begin
          p := lstPartition[h[mj]];
          state := ms = 1;
          for k := 0 to NoOfOTUs-1 do
          begin
            if clustered[k] then continue;
            if p[k] = state then
            begin
              inc(c[0]);
              c[c[0]] := k;
              if c[0] = 2 then
                break;
            end;
          end;
        end
        else
        begin
          for k := 0 to NoOfOTUs-1 do
          begin
            if clustered[k] then continue;
            inc(c[0]);
            c[c[0]] := k;
            if c[0] = 2 then
              break;
          end;
          p := nil;
        end;

      MakeCluster(i, c[1], c[2], p);
      clustered[c[2]] := true;
      q[c[2]] := nil;
      for j := 0 to lstPartition.Count-1 do
      begin
        if flag[j] then continue;
        p := lstPartition[h[j]];
        if p[c[1]] <> p[c[2]] then
          flag[j] := true
        else if p[c[2]] then
          dec(n[j][1])
        else
          dec(n[j][0]);
      end;
    end;

    c[0] := 0;
    for k := 0 to NoOfOTUs-1 do
    begin
      if clustered[k] then continue;
      inc(c[0]);
      if c[0] = 3 then
      begin
        p := q[k];
        break;
      end
      else
        c[c[0]] := k;
    end;
    MakeCluster(NoOfOTUs-3, c[1], c[2], p);
    clustered[c[2]] := true;
    q[c[2]] := nil;

    c[0] := 0;
    for k := 0 to NoOfOTUs-1 do
    begin
      if clustered[k] then continue;
      inc(c[0]);
      c[c[0]] := k;
      if c[0] = 2 then break;
    end;
    MakeCluster(NoOfOTUs-2, c[1], c[2], p);
  finally
    setlength(ni, 0);
    setlength(clustered, 0);
    setlength(flag, 0);
    setlength(h, 0);
  end;
end;

function TPartitionList.GetOTUName(index: integer):AnsiString;
begin
  if index >= FOTUName.Count then
    result := ''
  else
    result := FOTUName[index];
end;

procedure TPartitionList.SetOTUName(index: integer; value: AnsiString);
begin
    while index > FOTUName.Count-1 do
        FOTUName.Append('');
    FOTUName[index] := value;
end;

procedure TPartitionList.SetInformation(source: TStringList);
begin
  FInformation.Assign(source);
end;

function TPartitionList.GetPartition(index: integer):TPartition;
begin
  if (index >= 0) and (index < NoOfPartitions) then
    Result := lstPartition[index]
  else
    Result := nil;
end;

function TPartitionList.GetPartitionsSummary: TStringList;
begin
  Result := TStringList.Create;
  Result.Assign(FPartitionsSummary);
end;

function TPartitionList.GetPartitionsSummaryHeader(NumBootReps: Integer; Cutoff: Double): TStringList;
begin
  Result := TStringList.Create;
  Result.Add('; Bootstrap partitions and frequencies are shown below.');
  Result.Add('; Each partition is described by a string consisting of 0' + #39 + 's and 1' + #39 + 's');
  Result.Add('; where the nth character in the partition string represents the');
  Result.Add('; nth sequence in the input data. For a given partition, all sequences');
  Result.Add('; represented by a 1 cluster together and all sequences represented by');
  Result.Add('; a 0 cluster together.');
  Result.Add('; The frequency indicates the proportion of bootstrap replicates');
  Result.Add('; that give the same partition of sequences.');
  Result.Add('; Any partitions whose frequency was less than the frequency');
  Result.Add('; cutoff value have been excluded from these results.');
  Result.Add('');
  Result.Add('No_of_Taxa=' + IntToStr(NoOfOTUs));
  Result.Add('No_of_bootstrap_reps=' + IntToStr(NumBootReps));
  Result.Add('Frequency_cutoff=' + Format('%.0f%%', [Cutoff * 100]));
end;

procedure TPartitionList.DeletePartition(index: integer);
begin
  if NoOfTrees > 0 then Exit;
  if (index < 0) or (index >= NoOfPartitions) then Exit;
  TPartition(lstPartition[index]).Free;
  lstPartition.Delete(index);
end;

function TPartitionList.GetIsBLen:boolean;
var
  i: integer;
begin
  result := false;
  i := 0;
  while (not result) and (i < lstPartition.Count) do
  begin
    if TPartition(lstPartition[i]).BLen > 0.000000000000001 then
      result := true;
    inc(i);
  end;
end;

function TPartitionList.ToInfoString(simple: Boolean = False): AnsiString;
var
  MyString: AnsiString;
  MyCount: Integer;
  i: Integer;
begin
  MyString := '';

  if Information <> nil then
  begin
    MyCount := Information.Count;
    for i := 0 to MyCount - 1 do
      MyString := MyString + '[Information_' + IntToStr(i) + '] ' + Information[i] + LineEnding;
  end;

  MyString := MyString + '[ValueName] ' + ValueName + LineEnding;
  MyString := MyString + '[Value2Name] ' + Value2Name + LineEnding;
  MyString := MyString + '[FreqName] ' + FreqName + LineEnding;
  MyString := MyString + '[StatsName] ' + StatsName + LineEnding;

  MyString := MyString + '[MaxStats]' + FloatToStrF(MaxStats, ffFixed, 4, 4) + LineEnding;
  MyString := MyString + '[TotalFrequency]' + FloatToStrF(TotalFrequency, ffFixed, 4, 4) + LineEnding;


   if (lstPartition <> nil) and (lstPartition.Count > 0) then
   begin
     MyCount := lstPartition.Count;
     for i := 0 to MyCount - 1 do
       MyString := MyString + TPartition(lstPartition[i]).DescString(simple);
   end;

   if (lstTopology <> nil) and (lstTopology.Count > 0) then
   begin
     MyCount := lstTopology.Count - 1;
     for i := 0 to MyCount do
       MyString := MyString + TTopology(lstTopology[i]).DescString;
   end;

  MyString := MyString + '[isRooted] ' + BoolToStr(isRooted) + LineEnding;
  MyString := MyString + '[isBLen] ' + BoolToStr(isBLen) + LineEnding;

  MyString := MyString + '[NoOfOTUs] ' + IntToStr(NoOfOTUs) + LineEnding;
  MyString := MyString + '[NoOfTrees] ' + IntToStr(NoOfTrees) + LineEnding;
  MyString := MyString + '[MaxNoOfTrees] ' + IntToStr(MaxNoOfTrees) + LineEnding;
  MyString := MyString + '[NoOfPartitions] ' + IntToStr(NoOfPartitions) + LineEnding;

  Result := MyString;
end;

function TTopology.DescString: AnsiString;
var
  MyString: AnsiString;
  MyCount: Integer;
  i: Integer;
begin
  MyString := '';

  MyString := MyString + '[Freq] ' + FloatToStrf(Freq,ffFixed, 5, 5) + LineEnding;
  MyString := MyString + '[Value] ' + FloatToStrf(Value,ffFixed, 5, 5) + LineEnding;
  MyString := MyString + '[Value2] ' + FloatToStrf(Value2,ffFixed, 5, 5) + LineEnding;

  MyString := MyString + '[Dt] ' + IntToStr(Dt) + LineEnding;
  MyString := MyString + '[NoOfOTUs] ' + IntToStr(NoOfOTUs) + LineEnding;

  MyCount := NoOfOtus - 4;
  for i := 0 to MyCount - 1 do
    MyString := MyString + '[Partition_' + IntToStr(i) + '] ' + IntToStr(Partition[i]) + LineEnding;

  Result := MyString;
end;


end.
