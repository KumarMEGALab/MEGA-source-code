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

unit MTreeData;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Classes, ExtCtrls, SysUtils, Math,
  MegaConsts, MCalibrationData;

{$M+}

type

  TArrayOfBoolean = array of Boolean;

  { TTreeData }

  TTreeData = class(TObject)
  private
    FFreq : double;
    FValue: double;
    FValue2: double;
    FNoOfOTUs : integer;
    FNode : PArrayOfNodeData;
    FBLen : PArrayOfDouble;
    FSE   : PArrayOfDouble;
    FStats: PArrayOfDouble;
    FIsGeneDupEvent: TArrayOfBoolean;
    FIsSpeciationEvent: TArrayOfBoolean;
    FIsOutgroupMember: TArrayOfBoolean;
    FDataCoverage: ArrayOfDouble; { the proportion of sites for which at least on taxa in each descendent lineage have usable data}
    FIsStats: boolean;

    function GetDataCoverage(Index: Integer): Double;
    procedure SetDataCoverage(Index: Integer; AValue: Double);
    procedure SetNoOfOTUs(value:integer);
    procedure SetNode(index: integer; nodedata: TNodeData);
    procedure SetBLen(index: integer; value: double);
    procedure SetSE(index: integer; value: double);
    procedure SetStats(index: integer; value: double);
    function  GetNode(index: integer):TNodeData;
    function  GetBLen(index: integer):double;
    function  GetSE(index: integer):double;
    function  GetStats(index: integer):double;
    function  GetIsBLen:boolean;
    function  GetIsSE:boolean;
    function  GetIsStats:boolean;
    procedure SetIsBLen(value: boolean);
    procedure SetIsSE(value: boolean);
    procedure SetIsStats(value: boolean);
    function  GetSBL:double;
    function  GetIsGeneDupEvent(Index: Integer): Boolean;
    procedure SetIsGeneDupEvent(Index: Integer; const Value: Boolean);
    function GetIsSpeciationEvent(Index: Integer): Boolean;
    procedure SetIsSpeciationEvent(Index: Integer; const Value: Boolean);
    procedure WriteGeneDupInfoToSession(var SessionFile: File);
    procedure ReadGeneDupInfoFromSession(var SessionFile: File);
    function GetIsOutgroupMember(Index: Integer): Boolean;
    procedure SetIsOutgroupMember(Index: Integer; const Value: Boolean);
    {
    function  GetNodeLabel(const Index: Integer): AnsiString;
    function  GetHasNodeLabels: Boolean;
    procedure SetNodeLabel(const Index: Integer; const Value: AnsiString);
    }
  published
    property NoOfOTUs: integer read FNoOfOTUs write SetNoOfOTUs;
    property Value: double read FValue write FValue;
    property Value2: double read FValue2 write FValue2;
    property Freq: double read FFreq write FFreq;
    property SBL: double read GetSBL;
    property isBLen: boolean read GetIsBLen write SetIsBLen;
    property isSE: boolean read GetIsSE write SetIsSE;
    property isStats: boolean read GetIsStats write SetIsStats;
  public
    function NumGeneDups: Integer;
    function NumSpeciations: Integer;
    property Node[Index:integer]: TNodeData read GetNode write SetNode; default;
    property BLen[Index:integer]: double read GetBLen write SetBLen;
    property SE[Index:integer]: double read GetSE write SetSE;
    property Stats[Index:integer]: double read GetStats write SetStats;
    property IsGeneDupEvent[Index: Integer]: Boolean read GetIsGeneDupEvent write SetIsGeneDupEvent;
    property IsSpeciationEvent[Index: Integer]: Boolean read GetIsSpeciationEvent write SetIsSpeciationEvent;
    property IsOutgroupMember[Index: Integer]: Boolean read GetIsOutgroupMember write SetIsOutgroupMember;
    property DataCoverage[Index: Integer]: Double read GetDataCoverage write SetDataCoverage;
    property NodeArray: PArrayOfNodeData read FNode write FNode;
    property BLenArray: PArrayOfDouble read FBLen write FBLen;
    property SEArray: PArrayOfDouble read FSE write FSE;
    property StatsArray: PArrayOfDouble read FStats write FStats;

    {property NodeLabelArray: TArrayOfString read FNodeL write FNodeL;}
    { These six properties break the protection of object data.
      Use very carefully only when the maximum efficiency is needed.
      Used in object TTrees.}

    function Clone: TTreeData;
    // These 2 functions are used in parsing of control file for calibration points
    function MRCA(taxaANodeID, taxaBNodeID: Integer): Integer; overload;
    function FindTwoExtantTaxa(const aNodeId: Integer; var taxonA: Integer; var taxonB: Integer): Boolean;
    function ResolveRelTimes(var Times: TCalibrations): Boolean;

    function RootIndex: integer;

    function Assign(Source: TTreeData):boolean;
    function AssignDataCoverage(Source: TTreeData): Boolean;
    function Identical(Source: TTreeData): boolean;

    procedure RemoveOTU(index: integer);
    procedure AddOTU(node: integer);

    procedure ReadFromFile(var SessionFile: File; SessionFileVersion: integer);
    procedure WriteToFile(var SessionFile: File);
    function StateToStringList(Comment: AnsiString): TStringList;
    function StateToDebugFile(filename: String; Comment: String = ''): Boolean;
    function HasNegativeBranchLengths: Boolean;
    function HasNonZeroBranchLengths: Boolean;
    constructor Create(N:integer; BLen, SE, Stats:boolean; NodeLabels: Boolean = True);
    destructor Destroy; override;
    function NumOutgroupMembers: Integer;
    function GetApproximateSizeInBytes: Int64;
    procedure RemoveOutgroupMembers;
    function BootstrapValsToCsvFile(filename: String; numBootstrapReps: Integer; isRooted: Boolean): Boolean;
    function NumBranches: Integer;
  end;

//  TBranchInfoFunc = function(tree: TTreeData):double of object;

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ENDIF}
  MGlobalSettings, MPartitionList;

Const
  FP_CUTOFF = 0.000000000001;

///////////////////////////
// TTreeData
///////////////////////////

function TTreeData.Clone: TTreeData;
begin
  Result := TTreeData.Create(FNoOfOtus, IsBlen, IsSE, IsStats);
  Result.Assign(Self);
end;

constructor TTreeData.Create(N:integer; BLen, SE, Stats:boolean; NodeLabels: Boolean = True);
var
  i: Integer;
begin
  if N < 2 then
    FNoOfOTUs := 2
  else
    FNoOfOTUs := N;
  FFreq  := 0.0;
  FValue := 0.0;
  FValue2 := 0.0;
  if NoOfOTUs > 0 then
  begin
    // Alloc space for FDataCoverage (stores data only for internal nodes)
    SetLength(FDataCoverage, NoOfOtus - 1);
    // Alloc space for FNode (stores only internal nodes)
    GetMem(FNode, SizeOf(TNodeData)*(NoOfOTUs-1));
    FillChar(FNode^, SizeOf(TNodeData)*(NoOfOTUs-1), 0);

    // Alloc space for FBlen (stores branch lengths for ALL nodes)
    if BLen then begin
      GetMem(FBLen, SizeOf(Double)*2*(NoOfOTUs-1));
      FillChar(FBLen^, SizeOf(Double)*2*(NoOfOTUs-1), 0);
    end
    else
      FBLen := nil;

    // Alloc space for FSE (stores standard error for ALL nodes)
    if SE then begin
      GetMem(FSE, SizeOf(Double)*2*(NoOfOTUs-1));
      FillChar(FSE^, SizeOf(Double)*2*(NoOfOTUs-1), 0);
    end
    else
      FSE := nil;

    SetLength(FIsGeneDupEvent, 2 * NoOfOTUs - 1); { this is larger because the root node is included}
    for i := Low(FIsGeneDupEvent) to High(FIsGeneDupEvent) do
      FIsGeneDupEvent[i] := False;

    SetLength(FIsSpeciationEvent, 2 * NoOfOtus - 1); { root node included}
    for i := Low(FIsSpeciationEvent) to High(FIsSpeciationEvent) do
      FIsSpeciationEvent[i] := False;

    SetLength(FIsOutgroupMember, NoOfOtus);
    for i := Low(FIsOutgroupMember) to High(FIsOutgroupMember) do
      FIsOutgroupMember[i] := False;
    {// Set the length of the NodeLabels string array (stores node labels for internal nodes)
    if NodeLabels then begin
      SetLength(FNodeL, NoOfOtus-1);
      for I := 0 to (NoOfOTUs-2) do
        FNodeL[i] := EmptyStr;
    end
    else
    begin
      SetLength(FNodeL, 0);
    end;}

    FIsStats := Stats;
    // Alloc space for FStats (Stores ???)
    GetMem(FStats, SizeOf(Double)*2*(NoOfOTUs-1));
    FillChar(FStats^, SizeOf(Double)*2*(NoOfOTUs-1), 0);
  end;
end;

destructor TTreeData.Destroy;
begin
  if NoOfOTUs > 0 then
  begin
    FreeMemAndNil(FNode);
    FreeMemAndNil(FBLen);
    FreeMemAndNil(FSE);
    FreeMemAndNil(FStats);
    SetLength(FIsGeneDupEvent, 0);
    SetLength(FIsSpeciationEvent, 0);
    SetLength(FIsOutgroupMember, 0);
    SetLength(FDataCoverage, 0);
  end;
  FNoOfOTUs := 0;
  {SetLength(FNodeL, 0);}
  inherited
end;

function TTreeData.FindTwoExtantTaxa(const aNodeId: Integer;
  var taxonA: Integer; var taxonB: Integer): Boolean;
var
  id1Found, id2Found: Boolean;
  desId: Integer;
begin
  Result := False;
  id1Found := False;
  id2Found := False;
  desId := aNodeId;
  while (not id1Found) and (desId >= 0) do
  begin
    if FNode[desId].des1 < FNoOfOtus then
    begin
      taxonA := FNode[desId].des1;
      id1Found := True;
      break;
    end
    else
      desId := FNode[desId].des1 - FNoOfOtus;
  end;

  desId := aNodeId;
  while (not id2Found) and (desId >= 0) do
  begin
    if FNode[desId].des2 < FNoOfOTUs then
    begin
      taxonB := FNode[desId].des2;
      id2Found := True;
      break;
    end
    else
      desId := FNode[desId].des2 - FNoOfOtus;
  end;
  Result := (id1Found and id2Found);
end;

function TTreeData.NumOutgroupMembers: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FNoOfOTUs - 1 do;
    if IsOutgroupMember[i] then
      Result := Result + 1;
end;

function TTreeData.GetApproximateSizeInBytes: Int64;
begin
  Result := 0;
  Result := Result + (NoOfOtus - 1) * SizeOf(Double); // FDataCoverage
  Result := Result + (NoOfOtus - 1) * SizeOf(TNodeData); // FNode
  if IsBLen then
    Result := Result + 2*(NoOfOtus - 1)*SizeOf(Double); // FBLen
  If IsSE then
    Result := Result + 2*(NoOfOtus - 1)*SizeOf(Double); // FSE
  Result := Result + (2*NoOfOtus - 1) * SizeOf(Boolean); // FIsGeneDupEvent
  Result := Result + (2*NoOfOtus - 1) * SizeOf(Boolean); // FIsSpeciationEvent
  Result := Result + NoOfOtus*SizeOf(Boolean); // FIsOutgroupMember
  Result := Result + 2*(NoOfOtus - 1)*SizeOf(Double);
  Result := Result + 3*SizeOf(Double); // FFreq, FValue, FValue2
  Result := Result + SizeOf(Integer); // FNoOfOtus
  Result := Result + 4 * SizeOf(Pointer); // Pointers to FNode, FBLen, FSE and FStats
  Result := Result + SizeOf(Boolean); // FIsStats
  Result := Result + SizeOf(TTreeData); // Self
end;

procedure TTreeData.RemoveOutgroupMembers;
var
  i: Integer;
begin
  for i := NoOfOTUs - 1 downto 0 do
    if IsOutgroupMember[i] then
      RemoveOTU(i);
end;

function TTreeData.BootstrapValsToCsvFile(filename: String; numBootstrapReps: Integer; isRooted: Boolean): Boolean;
var
  aFile: TextFile;
  i: Integer;
  partitionStrings: TStringList = nil;
  pList: TPartitionList = nil;
  sList: TStringList = nil;
begin
  Result := False;
  try
    try
      pList := TPartitionList.Create(FNoOfOTUs, 0, isRooted);
      pList.Add(Self.NodeArray, 0.0);
      partitionStrings := pList.GetPartitionStringsForTreeData(Self);
      sList := TStringList.Create;
      for i := FNoOfOTUs to NumBranches - 2 do
        if Assigned(partitionStrings) and ((i - FNoOfOTUs) < partitionStrings.Count) then
          sList.Add(Format('{ %s },%.3e,', [partitionStrings[i - FNoOfOTUs], Stats[i]/numBootstrapReps]))
        else
          sList.Add(Format('{ %s },%.3e', ['?', Stats[i]/numBootstrapReps]));
      sList.Sort;
      AssignFile(aFile, filename);
      Rewrite(aFile);
      WriteLn(aFile, 'partition,value');
      for i := 0 to sList.Count - 1 do
        WriteLn(aFile, sList[i]);
      Result := FileExists(filename);
    except
      on E:Exception do
      begin
        {$IFNDEF VISUAL_BUILD}
        warn_nv('Error when exporting bootstrap values to CSV file: ' + E.Message);
        {$ENDIF}
      end;
    end;
  finally
    CloseFile(aFile);
    if Assigned(partitionStrings) then
      partitionStrings.Free;
    if Assigned(pList) then
      pList.Free;
    if Assigned(sList) then
      sList.Free;
  end;
end;

function TTreeData.NumBranches: Integer;
begin
  Result := 2*(NoOfOTUs - 1);
end;

procedure TTreeData.SetNoOfOTUs(value:integer);
begin
  if (value < 2) or (value = NoOfOTUs) then exit;
  FNoOfOTUs := value;
  ReAllocMem(FNode, SizeOf(TNodeData)*(NoOfOTUs-1));
  if FBLen <> nil then
    ReAllocMem(FBLen, SizeOf(Double)*2*(NoOfOTUs-1));
  if FSE <> nil then
    ReAllocMem(FSE, SizeOf(Double)*2*(NoOfOTUs-1));
  if FStats <> nil then
   ReAllocMem(FStats, SizeOf(Double)*2*(NoOfOTUs-1));
  SetLength(FIsGeneDupEvent, 2 * NoOfOtus - 1); // root node included
  SetLength(FIsSpeciationEvent, 2 * NoOfOtus - 1); // root node included
  SetLength(FIsOutgroupMember, NoOfOtus);
  SetLength(FDataCoverage, NoOfOTUs - 1);
end;

function TTreeData.GetDataCoverage(Index: Integer): Double;
begin
  Assert((Index >= 0) and (Index <= NoOfOtus));
  Result := FDataCoverage[Index];
end;

procedure TTreeData.SetDataCoverage(Index: Integer; AValue: Double);
begin
  Assert((Index >= 0) and (Index <= NoOfOtus));
  FDataCoverage[Index] := AValue;
end;

procedure TTreeData.SetNode(index: integer; nodedata: TNodeData);
begin
  if (index < 0) or (index >= NoOfOTUs) then Exit;
  FNode[index].des1 := nodedata.des1;
  FNode[index].des2 := nodedata.des2;
end;

{procedure TTreeData.SetNodeLabel(const Index: Integer; const Value: AnsiString);
begin
  if (Index < 0) or (Index >= (NoOfOtus - 1)) or (not Assigned(FNodeL)) then
    Exit;
  if Length(FNodeL) < (NoOfOtus - 1) then
    SetLength(FNodeL, NoOfOtus - 1);
  FNodeL[Index] := Value;
end;}

procedure TTreeData.SetBLen(index: integer; value: double);
begin
  if (index < 0) or (index >= 2*(NoOfOTUs-1)) then Exit;
  if FBLen = nil then
    Raise Exception.Create('Error: Attempting to set BLen when BLen = nil.');
  FBLen[index] := value;
end;

procedure TTreeData.SetSE(index: integer; value: double);
begin
  if (index < 0) or (index >= 2*(NoOfOTUs-1)) then Exit;
  FSE[index] := value;
end;

procedure TTreeData.SetStats(index: integer; value: double);
begin
  if (index < 0) or (index >= 2*(NoOfOTUs-1)) then Exit;
  FStats[index] := value;
end;

function TTreeData.GetNode(index: integer):TNodeData;
begin
  Result.des1 := -1;
  Result.des2 := -1;
  if (index < 0) or (index >= NoOfOTUs) then Exit;
  Result.des1 := FNode[index].des1;
  Result.des2 := FNode[index].des2;
end;


{function TTreeData.GetNodeLabel(const Index: Integer): AnsiString;
begin
  Result := EmptyStr;
  if (Index < 0) or (Index >= 2 * (NoOfOtus - 1)) or (not Assigned(FNodeL)) then
    Exit;
  Result := FNodeL[Index];
end;}

function TTreeData.GetBLen(index: integer):double;
begin
  Result := 0.0;
  if (index < 0) or (index >= 2*(NoOfOTUs-1)) or (not isBLen) then Exit;
  Result := FBLen[index];
end;

{function TTreeData.GetHasNodeLabels: Boolean;
begin
  if Assigned(FNodeL) then
    Result := True
  else
    Result := False;
end;}

function TTreeData.GetSE(index: integer):double;
begin
  Result := 0.0;
  if (index < 0) or (index >= 2*(NoOfOTUs-1)) or (not isSE) then Exit;
  Result := FSE[index];
end;

function TTreeData.GetStats(index: integer):double;
begin
  Result := 0.0;
  if (index < 0) or (index >= 2*(NoOfOTUs-1)) {or (not isStats)} then Exit;
  Result := FStats[index];
end;

function TTreeData.GetIsBLen:boolean;
begin
  if FBlen = nil then
    Result := false
  else
    Result := true;
end;

function TTreeData.GetIsGeneDupEvent(Index: Integer): Boolean;
begin
  Result := FIsGeneDupEvent[Index];
end;

function TTreeData.GetIsOutgroupMember(Index: Integer): Boolean;
begin
  Result := FIsOutgroupMember[Index];
end;

function TTreeData.GetIsSE:boolean;
begin
  if FSE = nil then
    Result := false
  else
    Result := true;
end;

function TTreeData.GetIsSpeciationEvent(Index: Integer): Boolean;
begin
  Result := FIsSpeciationEvent[Index];
end;

procedure TTreeData.SetIsBLen(value: boolean);
begin
  if value = IsBLen then Exit;
  if value = true then begin
      GetMem(FBLen, SizeOf(Double)*2*(NoOfOTUs-1));
      FillChar(FBLen^, SizeOf(Double)*2*(NoOfOTUs-1), 0);
  end
  else begin
      FreeMemAndNil(FBLen);
  end;
end;

procedure TTreeData.SetIsGeneDupEvent(Index: Integer; const Value: Boolean);
begin
  if FIsGeneDupEvent[Index] = Value then
    Exit;
  FIsGeneDupEvent[Index] := Value;
end;

procedure TTreeData.SetIsOutgroupMember(Index: Integer; const Value: Boolean);
begin
  if FIsOutgroupMember[Index] <> value then
    FIsOutgroupMember[Index] := Value;
end;

procedure TTreeData.SetIsSE(value: boolean);
begin
  if value = IsSE then Exit;
  if value = true then begin
      GetMem(FSE, SizeOf(Double)*2*(NoOfOTUs-1));
      FillChar(FSE^, SizeOf(Double)*2*(NoOfOTUs-1), 0);
  end
  else begin
      FreeMemAndNil(FSE);
  end;
end;

procedure TTreeData.SetIsSpeciationEvent(Index: Integer; const Value: Boolean);
begin
  if FIsSpeciationEvent[Index] = Value then
    Exit;
  FIsSpeciationEvent[Index] := Value;
end;

procedure TTreeData.SetIsStats(value: boolean);
begin
    if value = IsStats then Exit;
    FillChar(FStats^, SizeOf(Double)*2*(NoOfOTUs-1), 0);
end;

function TTreeData.GetIsStats:boolean;
begin
  Result := FIsStats;
end;

function TTreeData.GetSBL:double;
var i: integer;
begin
  Result := 0.0;
  if isBLen then
    for i := 0 to 2*NoOfOTUs-3 do
      Result := Result +BLenArray[i];
end;

function TTreeData.Assign(Source: TTreeData):boolean;
var
  i: integer;
begin
  Result := false;
  if NoOfOTUs <> Source.NoOfOTUs then Exit;
  isBLen := Source.isBLen;
  isSE := Source.isSE;
  isStats := Source.isStats;
  for i := 0 to NoOfOTUs-2 do
  begin
    NodeArray[i] := Source.NodeArray[i];
    DataCoverage[i] := Source.DataCoverage[i];
  end;
  Freq := Source.Freq;
  Value := Source.Value;
  Value2 := Source.Value2;
  if IsBLen then
    for i := 0 to 2*NoOfOTUs-3 do
    begin
      BLenArray[i] := Source.BLenArray[i];
    end;
  if IsSE then
    for i := 0 to 2*NoOfOTUs-3 do
      SEArray[i] := Source.SEArray[i];
//  if IsStats then
    for i := 0 to 2*NoOfOTUs-3 do
      StatsArray[i] := Source.StatsArray[i];
  for i := 0 to Length(FIsGeneDupEvent) - 1 do
  begin
    FIsGeneDupEvent[i] := Source.FIsGeneDupEvent[i];
    FIsSpeciationEvent[i] := Source.FIsSpeciationEvent[i];
  end;
  for i := 0 to Length(FIsOutgroupMember) - 1 do
    FIsOutgroupMember[i] := Source.FIsOutgroupMember[i];
  {if Source.HasNodeLabels then
  begin
    // Create a connection to the node label created somewhere else.
    SetLength(FNodeL, NoOfOtus - 1);
    for i := 0 to NoOfOTUs-2 do
      NodeLabel[i] := Source.NodeLabel[i];
  end;}
//
//  if HasNodeLabels then
//    for i := 0 to NoOfOTUs-2 do
//      NodeLabel[i] := Source.NodeLabel[i];
  Result := true;
end;

function TTreeData.AssignDataCoverage(Source: TTreeData): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Source.NoOfOTUs = NoOfOtus then
    Exit;
  for i := Low(FDataCoverage) to High(FDataCoverage) do
    FDataCoverage[i] := Source.DataCoverage[i];
end;

function TTreeData.Identical(Source: TTreeData): boolean;
var
  d1,d2,i: integer;
begin
  result := false;
  if NoOfOTUs <> source.NoOfOTUs then exit;
  if source.RootIndex <> RootIndex then exit;
  d1 := Node[RootIndex].des1;
  d2 := Node[RootIndex].des2;

  result := true;
  for i := 0 to NoOfOTUs-2 do
  begin
    if (NodeArray[i].des1 <> source.NodeArray[i].des1) and
       (NodeArray[i].des1 <> source.NodeArray[i].des2) then
    begin
      result := false;
      break;
    end;
    if (NodeArray[i].des2 <> source.NodeArray[i].des1) and
       (NodeArray[i].des2 <> source.NodeArray[i].des2) then
    begin
      result := false;
      break;
    end;
  end;
  if not result then exit;
  if isBlen and (Source.isBLen) then
    for i := 0 to 2*NoOfOTUs-2 do
      if (i = d1) or (i = d2) then
      begin
        if CompareValue(BLen[d1]+BLen[d2], source.BLen[d1]+source.BLen[d2], FP_CUTOFF) <> 0 then
        begin
          result := false;
          break;
        end;
      end
      else if CompareValue(BLen[i], source.BLen[i], FP_CUTOFF) <> 0 then
      begin
        result := false;
        break;
      end;
end;

// MRCA finds the ancestors of the current NodeIDs and looks for an intersection.
// The first intersection found is the MRCA.
// NOTE: TTreeData nodes are (0:[n-1] are internal, n:[2*n-1] are leaf nodes]
// The Node[] array only contains internal nodes (size: 0:[n-1]).
function TTreeData.MRCA(taxaANodeID, taxaBNodeID: Integer): Integer;
var
  i, j, taxaA, taxaB, mrcaID: Integer;
  taxaAParents, taxaBParents: Array of Integer;
begin
  taxaAParents := nil;
  taxaBParents := nil;
  result := -1;
  mrcaID := -1;
  // Check that the NodeIDs are valid
  if (taxaANodeID > (NoOfOTUs-1)*2) or (taxaANodeID < 0) or (taxaBNodeID > (NoOfOTUs-1)*2) or (taxaBNodeID < 0) then
  begin
    exit;
  end;

  taxaA := taxaANodeID;
  taxaB := taxaBNodeID;

  repeat
    // find parent of taxaA.
    // NOTE: Node[0] in TTreeData is actually the first internal node.
    for i := 0 to NoOfOTUs - 1 do
    begin
      // locate direct ancestor of taxaA
      if (Node[i].des1 = taxaA) or (Node[i].des2 = taxaA) then
      begin
        SetLength(TaxaAParents, Length(TaxaAParents) + 1);
        TaxaAParents[Length(TaxaAParents) - 1] := i;
        break; // ONLY 1 possible direct parent
      end;
    end;

    // find parent of taxaB
    for i := 0 to NoOfOTUs - 1 do
    begin
      // locate direct ancestor of taxaB
      if (Node[i].des1 = taxaB) or (Node[i].des2 = taxaB) then
      begin
        SetLength(TaxaBParents, Length(TaxaBParents) + 1);
        TaxaBParents[Length(TaxaBParents) - 1] := i;
        break; // ONLY 1 possible direct parent
      end;
    end;

    // Look for an intersection, the intersection between taxaAParents and taxaBParents is the most recent common ancestor.
    for i := 0 to length(taxaAParents)-1 do
    begin
      for j := 0 to length(taxaBparents)-1 do
        if taxaAParents[i] = taxaBParents[j] then
        begin
          mrcaID := taxaAParents[i];
          break;
        end;
    end;


    // If no MRCA is found, we go up a level to the parents of the parents.
    if mrcaID = -1 then
    begin
      // NOTE: You need to add NoOfOTUs because we search for the NodeID, but we store/return the internal NodeID [0:n-1].
      taxaA := taxaAParents[length(taxaAParents)-1] + NoOfOTUs;
      taxaB := taxaBParents[length(taxaBParents)-1] + NoOfOTUs;
    end;

  until mrcaID > -1;

  result := mrcaID; // Return internal node ID (starts at 0 for the first internal node).
end;

function TTreeData.NumGeneDups: Integer;
var
  i: Integer;
begin
  Result := 0;
  if Length(FIsGeneDupEvent) > 0 then
   for i := 0 to Length(FIsGeneDupEvent) - 1 do
     if FIsGeneDupEvent[i] = True then
       inc(Result);
end;

function TTreeData.NumSpeciations: Integer;
var
  i: Integer;
begin
  Result := 0;
  if Length(FIsSpeciationEvent) > 0 then
    for i := 0 to Length(FIsSpeciationEvent) - 1 do
      if FIsSpeciationEvent[i] = True then
        inc(Result);
end;

procedure TTreeData.ReadFromFile(var SessionFile: File; SessionFileVersion: integer);
var
  i: Integer;
  n: Integer = -1;
begin
  BlockRead(SessionFile, FFreq, SizeOf(Double));
  BlockRead(SessionFile, FValue, SizeOf(Double));
  BlockRead(SessionFile, FValue2, SizeOf(Double));
  BlockRead(SessionFile, FNode^, SizeOf(TNodeData)*(NoOfOTUs-1));
  if isBLen then
    BlockRead(SessionFile, FBLen^, SizeOf(Double)*2*(NoOfOTUs-1));
  if isSE then
    BlockRead(SessionFile, FSE^, SizeOf(Double)*2*(NoOfOTUs-1));
  if SessionFileVersion > 400 then
    BlockRead(SessionFile, FStats^, SizeOf(Double)*2*(NoOfOTUs-1))
  else if isStats then
    BlockRead(SessionFile, FStats^, SizeOf(Double)*2*(NoOfOTUs-1));

  if SessionFileVersion >= 606 then
  begin
    BlockRead(SessionFile, n, SizeOf(n));
    if n > 0 then
      for i := 0 to n - 1 do
        BlockRead(SessionFile, FDataCoverage[i], SizeOf(Double));
  end;
 if SessionFileVersion >= 606 then
 begin
   BlockRead(SessionFile, n, SizeOf(Integer));
   if n > 0 then
   begin
     SetLength(FIsGeneDupEvent, n);
     SetLength(FIsSpeciationEvent, n);
     for i := 0 to Length(FIsGeneDupEvent) - 1 do
     begin
       BlockRead(SessionFile, FIsGeneDupEvent[i], SizeOf(Boolean));
       BlockRead(SessionFile, FIsSpeciationEvent[i], SizeOf(Boolean));
     end;
   end;
 end;
 if SessionFileVersion >= 701 then
 begin
   BlockRead(SessionFile, n, SizeOf(Integer));
   if n > 0 then
   begin
     SetLength(FIsOutgroupMember, n);
     for i := 0 to Length(FIsOutgroupMember) - 1 do
       BlockRead(SessionFile, FIsOutgroupMember[i], SizeOf(Boolean));
   end;
 end;
end;
{$IFDEF FPC}
procedure TTreeData.ReadGeneDupInfoFromSession(var SessionFile: File);
{$ELSE}
procedure TTreeData.ReadGeneDupInfoFromSession(var SessionFile: &File);
{$ENDIF}
var
  NumNodes: Integer;
  i: Integer;
  IsDup, IsSpec: Boolean;
begin
  NumNodes := 0;
  IsDup := False;
  IsSpec := False;

  BlockRead(SessionFile, NumNodes, SizeOf(Integer));
  SetLength(FIsGeneDupEvent, NumNodes);
  if NumNodes > 0 then
    for i := 0 to NumNodes - 1 do
    begin
      BlockRead(SessionFile, IsDup, SizeOf(Boolean));
      FIsGeneDupEvent[i] := IsDup;
    end;

  BlockRead(SessionFile, NumNodes, SizeOf(Integer));
  SetLength(FIsSpeciationEvent, NumNodes);
  if NumNodes > 0 then
    for i := 0 to NumNodes - 1 do
    begin
      BlockRead(SessionFile, IsSpec, SizeOf(Boolean));
      FIsSpeciationEvent[i] := IsSpec;
    end;
end;

{$IFDEF FPC}
procedure TTreeData.WriteGeneDupInfoToSession(var SessionFile: File);
{$ELSE}
procedure TTreeData.WriteGeneDupInfoToSession(var SessionFile: &File);
{$ENDIF}
var
  i: Integer;
  IsDup, IsSpec: Boolean;
begin
  i := length(FIsGeneDupEvent);
  BlockWrite(SessionFile, i, SizeOf(Integer));
  if i > 0 then
    for i := 0 to length(FIsGeneDupEvent) - 1do
    begin
      IsDup := FIsGeneDupEvent[i];
      BlockWrite(SessionFile, IsDup, SizeOf(Boolean));
    end;

  i := length(FIsSpeciationEvent);
  BlockWrite(SessionFile, i, SizeOf(Integer));
  if i > 0 then
    for i := 0 to Length(FIsSpeciationEvent) - 1 do
    begin
      IsSpec := FIsSpeciationEvent[i];
      BlockWrite(SessionFile, IsSpec, SizeOf(Boolean));
    end;
end;

procedure TTreeData.WriteToFile(var SessionFile: File);
var
  i, n: Integer;
begin
  BlockWrite(SessionFile, FFreq, SizeOf(Double));
  BlockWrite(SessionFile, FValue, SizeOf(Double));
  BlockWrite(SessionFile, FValue2, SizeOf(Double));
  BlockWrite(SessionFile, FNode^, SizeOf(TNodeData)*(NoOfOTUs-1));
  if isBLen then
    BlockWrite(SessionFile, FBLen^, SizeOf(Double)*2*(NoOfOTUs-1));
  if isSE then
    BlockWrite(SessionFile, FSE^, SizeOf(Double)*2*(NoOfOTUs-1));
  BlockWrite(SessionFile, FStats^, SizeOf(Double)*2*(NoOfOTUs-1));

  n := Length(FDataCoverage);
  BlockWrite(SessionFile, n, SizeOf(n));
  if Length(FDataCoverage) > 0 then
    for i := 0 to Length(FDataCoverage) - 1 do
      BlockWrite(SessionFile, FDataCoverage[i], SizeOf(Double));

  n := Length(FIsGeneDupEvent);
  BlockWrite(SessionFile, n, SizeOf(Integer));
  if n > 0 then
    for i := 0 to Length(FIsGeneDupEvent) - 1 do
    begin
      BlockWrite(SessionFile, FIsGeneDupEvent[i], SizeOf(Boolean));
      BlockWrite(SessionFile, FIsSpeciationEvent[i], SizeOf(Boolean));
    end;
  n := Length(FIsOutgroupMember);
  BlockWrite(SessionFile, n, SizeOf(Integer));
  if n > 0 then
    for i := 0 to Length(FIsOutgroupMember) - 1 do
      BlockWrite(SessionFile, FIsOutgroupMember[i], SizeOf(Boolean));
end;

procedure TTreeData.RemoveOTU(index: integer);
var
  i, j,node,sister: integer;
  newOutgroupMembers: TArrayOfBoolean;
begin
  if (index < 0) or (index >= NoOfOTUs) then Exit;
  SetLength(newOutgroupMembers, Length(FIsOutgroupMember) - 1);
  j := 0;
  for i := 0 to Length(FIsOutgroupMember) - 1 do
    if i <> index then
    begin
      newOutgroupMembers[j] := FIsOutgroupMember[i];
      inc(j);
    end;
  SetLength(FIsOutgroupMember, 0);
  FIsOutgroupMember := newOutgroupMembers;
  node := 0;
  sister := 0;
  for i := 0 to NoOfOTUs-2 do
    if (NodeArray[i].des1 = index) or (NodeArray[i].des2 = index) then
    begin
      node := i;
      if NodeArray[i].des1 = index then
        sister := NodeArray[i].des2
      else
        sister := NodeArray[i].des1;
      if IsBLen then
        BLenArray[sister] := BLenArray[sister] +BLenArray[node+NoOfOTUs];
      if IsSE then
        SEArray[sister] := Sqrt(SEArray[sister]*SEArray[sister] +SEArray[node+NoOfOTUs]*SEArray[node+NoOfOTUs]);
      Break;
    end;
  for i := 0 to NoOfOTUs-3 do
  begin
    if i >= node then
    begin
      NodeArray[i].des1 := NodeArray[i+1].des1;
      NodeArray[i].des2 := NodeArray[i+1].des2;
    end;
    if NodeArray[i].des1 = node+NoOfOTUs then
      NodeArray[i].des1 := sister
    else if NodeArray[i].des2 = node+NoOfOTUs then
      NodeArray[i].des2 := sister;

    if NodeArray[i].des1 > node+NoOfOTUs then
      NodeArray[i].des1 := NodeArray[i].des1 -2
    else if NodeArray[i].des1 > index then
      NodeArray[i].des1 := NodeArray[i].des1 -1;
    if NodeArray[i].des2 > node+NoOfOTUs then
      NodeArray[i].des2 := NodeArray[i].des2 -2
    else if NodeArray[i].des2 > index then
      NodeArray[i].des2 := NodeArray[i].des2 -1;

  end;

  if IsBLen then
    for i := 0 to 2*NoOfOTUs-3 do
      if i > node+NoOfOTUs then
        BLenArray[i-2] := BLenArray[i]
      else if i > index then
        BLenArray[i-1] := BLenArray[i];
  if IsSE then
    for i := 0 to 2*NoOfOTUs-3 do
      if i > node+NoOfOTUs then
        SEArray[i-2] := SEArray[i]
      else if i > index then
        SEArray[i-1] := SEArray[i];
//  if IsStats then
    for i := 0 to 2*NoOfOTUs-3 do
      if i > node+NoOfOTUs then
        StatsArray[i-2] := StatsArray[i]
      else if i > index then
        StatsArray[i-1] := StatsArray[i];
  NoOfOTUs := NoOfOTUs-1;
end;

// We need to find the NodeID of the internal node which is the most recent comman ancestor of taxaA and taxaB.
// Loop over all the control times calling mrca, return results in an array of TNodeAttributes.
function TTreeData.ResolveRelTimes(var Times: TCalibrations): Boolean;
var
  i, NodeMRCA: Integer;
begin
  Result := False;
  for i := 0 to Times.Count - 1 do
  begin
    if Times.GetCalibration(i).NodeID < 0 then // If NodeID = -1 then resolve with MRCA
    begin
      NodeMRCA := MRCA(Times.GetCalibration(i).NodeIDA, Times.GetCalibration(i).NodeIDB);
      Times.GetCalibration(i).NodeId := NodeMRCA;
      if Times.GetCalibration(i).NodeId >= 0 then
        Result := True;


      // Update the tree with the node name specified in the calibration file (if there was one)
      if times.GetCalibration(i).MRCAName <> EmptyStr then
      begin
       (* if (FNodeL[nodeMRCA] <> EmptyStr) and (CompareText(FNodeL[NodeMRCA], times[i].MRCAName) <> 0) then
        begin
          {$IFDEF VISUAL_BUILD}
           { TODO 1 -oDan -creltime : Handle this for visual }
          {$ELSE}
          warn_nv('Conflicting labels given for the same node - using the label given in the calibration file (' + times[i].MRCAName + '). Ignoring label given in the newick file (' + FNodeL[nodeMRCA] + ').');
          {$ENDIF}
        end;
        FNodeL[nodeMRCA] := times[i].MRCAName;         *)
      end;

    end
    else // NodeLabel (already have NodeID which was resolved earlier)
      Result := True;
  end;
end;

procedure TTreeData.AddOTU(node: integer);
var
  i: integer;
begin
  if (node < 0) or (node > 2*NoOfOTUs-2) then Exit;
  NoOfOTUs := NoOfOTUs +1;
  for i := 0 to NoOfOTUs-3 do
  begin
    if NodeArray[i].des1 = node then
      NodeArray[i].des1 := 2*NoOfOTUs-3
    else if NodeArray[i].des1 >= NoOfOTUs-1 then
      NodeArray[i].des1 := NodeArray[i].des1+1;
    if NodeArray[i].des2 = node then
      NodeArray[i].des2 := 2*NoOfOTUs-3
    else if NodeArray[i].des2 >= NoOfOTUs-1 then
      NodeArray[i].des2 := NodeArray[i].des2+1;
  end;
  if node = 2*(NoOfOTUs-1)-2 then
  begin
    if node < NoOfOTUs-1 then
      NodeArray[NoOfOTUs-2].des1 := node
    else
      NodeArray[NoOfOTUs-2].des1 := node+1;
    NodeArray[NoOfOTUs-2].des2 := NoOfOTUs-1;
  end
  else
  begin
    NodeArray[NoOfOTUs-2].des1 := NodeArray[NoOfOTUs-3].des1;
    NodeArray[NoOfOTUs-2].des2 := NodeArray[NoOfOTUs-3].des2;
    if node < NoOfOTUs-1 then
      NodeArray[NoOfOTUs-3].des1 := node
    else
      NodeArray[NoOfOTUs-3].des1 := node+1;
    NodeArray[NoOfOTUs-3].des2 := NoOfOTUs-1;
  end;

  if IsBLen then
  begin
    BLenArray[2*NoOfOTUs-3] := 0;
    for i := 2*NoOfOTUs-4 downto NoOfOTUs do
      BLenArray[i] := BLenArray[i-1];
    BLenArray[NoOfOTUs-1] := 0;
  end;
  if IsSE then
  begin
    SEArray[2*NoOfOTUs-3] := 0;
    for i := 2*NoOfOTUs-4 downto NoOfOTUs do
      SEArray[i] := SEArray[i-1];
    SEArray[NoOfOTUs-1] := 0;
  end;
//  if IsStats then
//  begin
    StatsArray[2*NoOfOTUs-3] := 0;
    for i := 2*NoOfOTUs-4 downto NoOfOTUs do
      StatsArray[i] := StatsArray[i-1];
    StatsArray[NoOfOTUs-1] := 0;
//  end;
end;

function TTreeData.RootIndex: integer;
var
  i: integer;
  flag: array of boolean;
begin
  result := -1;
  setlength(flag, 2*NoOfOTUs-1);
  for i := NoOfOTUs to 2*NoOfOTUs-2 do
    flag[i] := false;
  for i := 0 to NoOfOTUs-2 do
  begin
    flag[NodeArray[i].des1] := true;
    flag[NodeArray[i].des2] := true;
  end;
  for i := 2*NoOfOTUs-2 downto NoOfOTUs do
    if not flag[i] then
    begin
      result := i - NoOfOTUs;
      break;
    end;
  setlength(flag, 0);
end;

function TTreeData.StateToStringList(Comment: AnsiString): TStringList;
var
  MyStringList: TStringList;
  i: Integer;
  MyCount: Integer;
begin
  MyStringList := TStringList.Create();
  MyStringList.Add('TTreeData=MyName');

  if Comment <> '' then
	MyStringList.Add('TTreeData.Comment=' + Comment);

  MyStringList.Add('TTreeData.NoOfOTUs=' + IntToStr(NoOfOTUs));

  MyStringList.Add('TTreeData.Value=' + FloatToStrF(Value, ffFixed, 4, 4));
  MyStringList.Add('TTreeData.Value2=' + FloatToStrF(Value2, ffFixed, 4, 4));
  MyStringList.Add('TTreeData.Freq=' + FloatToStrF(Freq, ffFixed, 4, 4));
  MyStringList.Add('TTreeData.SBL=' + FloatToStrF(SBL, ffFixed, 4, 4));
  MyStringList.Add('TTreeData.isBLen=' + BoolToStr(isBLen, True));
  MyStringList.Add('TTreeData.isSE=' + BoolToStr(isSE, True));
  MyStringList.Add('TTreeData.isStats=' + BoolToStr(isStats, True));

  MyCount := (NoOfOTUs-1);
  for i := 0 to MyCount - 1 do
    MyStringList.Add('TTreeData.Node[' + IntToStr(i) + ']=des1:' + IntToStr(Node[i].des1) + ', des2:' + IntToStr(Node[i].des2));

  MyCount := 2*(NoOfOTUs-1);
  for i := 0 to MyCount - 1 do
    MyStringList.Add('BLen[' + IntToStr(i) + ']=' + FloatToStrF(BLen[i], ffFixed, 4, 4));

  if IsSe then
    for i := 0 to MyCount - 1 do
      MyStringList.Add('SE[' + IntToStr(i) + ']=' + FloatToStrF(FSE[i], ffFixed, 4, 4));

  if IsStats then
    for i := 0 to MyCount - 1 do
      MyStringList.Add('Stats[' + IntToStr(i) + ']=' + FloatToStrF(FStats[i], ffFixed, 4, 4));

  for i := 0 to Length(FIsOutgroupMember) - 1 do
    MyStringList.Add(Format('Outgroup[%d]=%s', [i, BoolToStr(FIsOutgroupMember[i], True)]));
  Result := MyStringList;
end;

function TTreeData.StateToDebugFile(filename: String; Comment: String): Boolean;
var
  aList: TStringList = nil;
begin
  try
    aList := StateToStringList(Comment);
    aList.SaveToFile(filename);
    Result := FileExists(filename);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TTreeData.HasNegativeBranchLengths: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to 2*(NoOfOTUs-1) - 1 do
    if BLen[i] < 0 then
    begin
      Result := True;
      break;
    end;
end;

function TTreeData.HasNonZeroBranchLengths: Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Assigned(FBLen) then
    Exit;
  for i := 0 to 2*(NoOfOTUs - 1) - 1 do
    if CompareValue(FBLen[i], 0, FP_CUTOFF) > 0 then
    begin
      Result := True;
      Exit;
    end;
end;

end.
