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

unit MTreeData;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Classes, ExtCtrls, SysUtils, Math,
  MegaConsts, MCalibrationData, mtree_split;

{$M+}

type

  TArrayOfBoolean = array of Boolean;

  { TTreeData }

  TTreeData = class(TObject)
  private
    FHasMeanBcl: Boolean;
    FIsDataCoverage: Boolean;
    FStatsStdDev: PArrayOfDouble;
    FTreeName: String;
    FTreeSplits: TTreeSplitArray;
    FFreq : double;
    FValue: double;
    FValue2: double;
    FNoOfOTUs : integer;
    FNode : PArrayOfNodeData;
    FBLen : PArrayOfDouble;
    FSE   : PArrayOfDouble;
    FStats: PArrayOfDouble;
    FMeanBcl: PArrayOfDouble;
    FIsGeneDupEvent: TArrayOfBoolean;
    FIsSpeciationEvent: TArrayOfBoolean;
    FIsOutgroupMember: TArrayOfBoolean;
    FDataCoverage: ArrayOfDouble; { the proportion of sites for which at least on taxa in each descendent lineage have usable data}
    FIsStats: boolean;
    FValue3: Double;
    FInternalNodeLabel: TStringList;

    function GetDataCoverage(Index: Integer): Double;
    function GetInternalNodeLabel(Index: Integer): String;
    function GetIsStatsStdDev: Boolean;
    function GetMeanBcl(Index: Integer): Double;
    function GetStatsStdDev(Index: Integer): Double;
    procedure SetDataCoverage(Index: Integer; AValue: Double);
    procedure SetHasMeanBcl(AValue: Boolean);
    procedure SetInternalNodeLabel(Index: Integer; AValue: String);
    procedure SetIsStatsStdDev(AValue: Boolean);
    procedure SetMeanBcl(Index: Integer; AValue: Double);
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
    procedure SetStatsStdDev(Index: Integer; AValue: Double);
    procedure SetValue(AValue: double);
    procedure SetValue2(AValue: double);
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
    property Value: double read FValue write SetValue;
    property Value2: double read FValue2 write SetValue2;
    property Freq: double read FFreq write FFreq;
    property SBL: double read GetSBL;
    property isBLen: boolean read GetIsBLen write SetIsBLen;
    property isSE: boolean read GetIsSE write SetIsSE;
    property isStats: boolean read GetIsStats write SetIsStats;
    property HasMeanBcl: Boolean read FHasMeanBcl write SetHasMeanBcl;
  public
    procedure ClearBlens;
    procedure InitTreeSplits;
    function ExportSplitsToFile(filename: String): Boolean;
    function IndexForPartition(partitionString: String; startAt: Integer): Integer;
    function NumGeneDups: Integer;
    function NumSpeciations: Integer;
    property Value3: Double read FValue3 write FValue3;
    property Node[Index:integer]: TNodeData read GetNode write SetNode; default;
    property BLen[Index:integer]: double read GetBLen write SetBLen;
    property SE[Index:integer]: double read GetSE write SetSE;
    property Stats[Index:integer]: double read GetStats write SetStats;
    property MeanBcl[Index: Integer]: Double read GetMeanBcl write SetMeanBcl;
    property IsGeneDupEvent[Index: Integer]: Boolean read GetIsGeneDupEvent write SetIsGeneDupEvent;
    property IsSpeciationEvent[Index: Integer]: Boolean read GetIsSpeciationEvent write SetIsSpeciationEvent;
    property IsOutgroupMember[Index: Integer]: Boolean read GetIsOutgroupMember write SetIsOutgroupMember;
    property DataCoverage[Index: Integer]: Double read GetDataCoverage write SetDataCoverage;
    property IsDataCoverage: Boolean read FIsDataCoverage write FIsDataCoverage;
    property InternalNodeLabel[Index: Integer]: String read GetInternalNodeLabel write SetInternalNodeLabel;
    property NodeArray: PArrayOfNodeData read FNode write FNode;
    property BLenArray: PArrayOfDouble read FBLen write FBLen;
    property SEArray: PArrayOfDouble read FSE write FSE;
    property StatsArray: PArrayOfDouble read FStats write FStats;
    property StatsStdDev[Index: Integer]: Double read GetStatsStdDev write SetStatsStdDev;
    property IsStatsStdDev: Boolean read GetIsStatsStdDev write SetIsStatsStdDev;
    property TreeName: String read FTreeName write FTreeName;

    function Clone: TTreeData;
    function GetInternalNodeLabels: TStringList;
    function GetInternalNodeLbl(index: Integer): AnsiString;
    procedure ClearInternalNodeLabels;

    function MRCA(taxaANodeID, taxaBNodeID: Integer): Integer; overload;
    function FindTwoExtantTaxa(const aNodeId: Integer; var taxonA: Integer; var taxonB: Integer): Boolean;
    function ResolveRelTimes(var Times: TCalibrations): Boolean;

    function RootIndex: integer;

    function Assign(Source: TTreeData):boolean; virtual;
    function AssignDataCoverage(Source: TTreeData): Boolean;
    function Identical(Source: TTreeData): boolean;
    function SameTopology(Source: TTreeData): Boolean;
    function RemoveOTU(otu: integer; resetOutgroup: Boolean; OtuName: String): String;
    procedure AddOTU(node: integer);
    procedure AssignInternalNodeLabels(newLabels: TStringList);
    function HasInternalNodeLbls: Boolean;
    procedure ReadFromFile(var SessionFile: File; SessionFileVersion: integer);
    procedure WriteToFile(var SessionFile: File);
    function StateToStringList(isBriefSummary: Boolean; Comment: AnsiString): TStringList;
    function StateToDebugFile(filename: String; Comment: String = ''): Boolean;
    function HasNegativeBranchLengths: Boolean;
    function HasNonZeroBranchLengths: Boolean;
    constructor Create(N:integer; BLen, SE, Stats:boolean; NodeLabels: Boolean = True);
    destructor Destroy; override;
    function NumOutgroupMembers: Integer;
    function GetOutgroupMembers: TArrayOfBoolean;
    function GetApproximateSizeInBytes: Int64;
    procedure RemoveOutgroupMembers;
    function BootstrapValsToCsvFile(filename: String; numBootstrapReps: Integer; isRooted: Boolean): Boolean;
    function NumBranches: Integer;
    function NumInternalNodes(isRooted: Boolean): Integer;
  end;

  { TTimetreeData }

  TTimetreeData = class(TTreeData)

    private
      FIsCalibrated: Boolean;
    public
      Reltimes: TArrayOfExt;
      MinReltimes: TArrayOfExt;
      MaxReltimes: TArrayOfExt;
      DivTimes: TArrayOfExt;
      MinDivTimes: TArrayOfExt;
      MaxDivTimes: TArrayOfExt;
      TimeFactor: Extended;
      MinTimeFactor: Extended;
      MaxTimeFactor: Extended;
      constructor Create(N:integer; BLen, SE, Stats, isCalibrated:boolean; NodeLabels: Boolean = True);
      destructor Destroy; override;
      procedure Clone(Source: TTimetreeData);
      property IsCalibrated: Boolean read FIsCalibrated;
  end;

  { TMPMLTreeData }

  TMPMLTreeData = class(TTreeData)
    private
      FInitialLogLikelihood: Extended;
      FLogLikelihood: Extended;
      FSumOfBranchLengths: Extended;
      FTreeLength: Integer;
      procedure SetInitialLogLikelihood(AValue: Extended);
      procedure SetLogLikelihood(AValue: Extended);
      procedure SetSumOfBranchLengths(AValue: Extended);
      procedure SetTreeLength(AValue: Integer);
    public
      constructor Create(N:integer; NodeLabels: Boolean = True);
      function Assign(Source: TMPMLTreeData):Boolean; overload;
      function Assign(Source: TTreeData): Boolean; overload;
      property LogLikelihood: Extended read FLogLikelihood write SetLogLikelihood;
      property InitialLogLikelihood: Extended read FInitialLogLikelihood write SetInitialLogLikelihood;
      property TreeLength: Integer read FTreeLength write SetTreeLength;
      property SumOfBranchLengths: Extended read FSumOfBranchLengths write SetSumOfBranchLengths;
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

{ TMPMLTreeData }

procedure TMPMLTreeData.SetLogLikelihood(AValue: Extended);
begin
  if CompareValue(AValue, 0.0, FP_CUTOFF) > 0 then
    raise Exception.Create(Format('log likelihood must be <= 0 but got %.3e', [AValue]));
  if FLogLikelihood = AValue then Exit;
  FLogLikelihood := AValue;
  FValue2 := AValue;
end;

procedure TMPMLTreeData.SetInitialLogLikelihood(AValue: Extended);
begin
  if CompareValue(AValue, 0.0, FP_CUTOFF) > 0 then
    raise Exception.Create(Format('initial log likelihood must be <= 0 but got %.3e', [AValue]));
  if FInitialLogLikelihood = AValue then Exit;
  FInitialLogLikelihood := AValue;
  FValue3 := AValue;
end;

procedure TMPMLTreeData.SetSumOfBranchLengths(AValue: Extended);
begin
  if CompareValue(AValue, 0.0, FP_CUTOFF) < 0 then
    raise Exception.Create(Format('sum of branch lengths must be >= 0 but got %.3e', [AValue]));
  if FSumOfBranchLengths = AValue then Exit;
  FSumOfBranchLengths := AValue;
end;

procedure TMPMLTreeData.SetTreeLength(AValue: Integer);
begin
  if AValue < 0 then
    raise Exception.Create(Format('tree length must be >= 0 but got %d', [AValue]));
  if FTreeLength = AValue then Exit;
  FTreeLength := AValue;
  FValue := AValue;
end;

constructor TMPMLTreeData.Create(N: integer; NodeLabels: Boolean);
begin
  inherited Create(N, True, True, True, NodeLabels);
  FInitialLogLikelihood := 0;
  FLogLikelihood := 0;
  FTreeLength := 0;
  FSumOfBranchLengths := 0;
end;

function TMPMLTreeData.Assign(Source: TMPMLTreeData): Boolean;
begin
  Result := inherited Assign(Source);
  FLogLikelihood := Source.FLogLikelihood;
  FInitialLogLikelihood := Source.FInitialLogLikelihood;
  FTreeLength := Source.FTreeLength;
  FSumOfBranchLengths := Source.FSumOfBranchLengths;
end;

function TMPMLTreeData.Assign(Source: TTreeData): Boolean;
begin
  Result := inherited Assign(Source);
end;

{ TTimetreeData }

constructor TTimetreeData.Create(N: integer; BLen, SE, Stats, isCalibrated: boolean; NodeLabels: Boolean);
var
  i: Integer;
begin
  inherited Create(N, Blen, SE, Stats, NodeLabels);
  FIsCalibrated := isCalibrated;
  SetLength(MinDivTimes, 2*(NoOfOTUs - 1));
  SetLength(MaxDivTimes, 2*(NoOfOTUs - 1));
  SetLength(DivTimes, 2*(NoOfOTUs - 1));
  SetLength(Reltimes, 2*(NoOfOTUs - 1));
  SetLength(MinReltimes, 2*(NoOfOTUs - 1));
  SetLength(MaxReltimes, 2*(NoOfOTUs - 1));
  for i := Low(MinDivTimes) to High(MinDivTimes) do
  begin
    MinDivTimes[i] := 0;
    MaxDivTimes[i] := 0;
    DivTimes[i] := 0;
    Reltimes[i] := 0;
    MinReltimes[i] := 0;
    MaxReltimes[i] := 0;
  end;
  TimeFactor := -1;
  MinTimeFactor := -1;
  MaxTimeFactor := -1;
end;

destructor TTimetreeData.Destroy;
begin
  SetLength(MinDivTimes, 0);
  SetLength(MaxDivTimes, 0);
  SetLength(DivTimes, 0);
  SetLength(Reltimes, 0);
  inherited Destroy;
end;

procedure TTimetreeData.Clone(Source: TTimetreeData);
var
  i: Integer = -1;
begin
  Assign(Source);
  FIsCalibrated := Source.FIsCalibrated;

  SetLength(Reltimes, Length(Source.Reltimes));
  if Length(Reltimes) > 0 then
    for i := Low(Reltimes) to High(Reltimes) do
      Reltimes[i] := Source.Reltimes[i];

  SetLength(MinReltimes, Length(Source.MinReltimes));
  if Length(MinReltimes) > 0 then
    for i := Low(MinReltimes) to High(MinReltimes) do
      MinReltimes[i] := Source.MinReltimes[i];

  SetLength(MaxReltimes, Length(Source.MaxReltimes));
  if Length(MaxReltimes) > 0 then
    for i := Low(MaxReltimes) to High(MaxReltimes) do
      MaxReltimes[i] := Source.MaxReltimes[i];

  SetLength(Divtimes, Length(Source.Divtimes));
  if Length(Divtimes) > 0 then
    for i := Low(Divtimes) to High(Divtimes) do
      Divtimes[i] := Source.Divtimes[i];

  SetLength(MinDivTimes, Length(Source.MinDivTimes));
  if Length(MinDivTimes) > 0 then
    for i := Low(MinDivTimes) to High(MinDivTimes) do
      MinDivTimes[i] := Source.MinDivTimes[i];

  SetLength(MaxDivTimes, Length(Source.MaxDivTimes));
  if Length(MaxDivTimes) > 0 then
    for i := Low(MaxDivTimes) to High(MaxDivTimes) do
      MaxDivTimes[i] := Source.MaxDivTimes[i];

  TimeFactor:= Source.TimeFactor;
  MinTimeFactor:= Source.MinTimeFactor;
  MaxTimeFactor:= Source.MaxTimeFactor;
end;

///////////////////////////
// TTreeData
///////////////////////////

function TTreeData.Clone: TTreeData;
begin
  Result := TTreeData.Create(FNoOfOtus, IsBlen, IsSE, IsStats);
  Result.Assign(Self);
end;

function TTreeData.GetInternalNodeLabels: TStringList;
begin
  Result := FInternalNodeLabel;
end;

function TTreeData.GetInternalNodeLbl(index: Integer): AnsiString;
begin
  Result := InternalNodeLabel[index];
end;

procedure TTreeData.ClearInternalNodeLabels;
var
  i: Integer = -1;
begin
  if Assigned(FInternalNodeLabel) then
  begin
    FInternalNodeLabel.Clear;
    for i := 0 to NoOfOTUs - 1 do
      FInternalNodeLabel.Add(EmptyStr);
  end;
end;

constructor TTreeData.Create(N:integer; BLen, SE, Stats:boolean; NodeLabels: Boolean = True);
var
  i: Integer;
begin
  FIsDataCoverage := False;
  FTreeName := EmptyStr;
  if N < 2 then
    FNoOfOTUs := 2
  else
    FNoOfOTUs := N;
  FFreq  := 0.0;
  FValue := 0.0;
  FValue2 := 0.0;
  FValue3 := 0.0;
  if NoOfOTUs > 0 then
  begin
    // Alloc space for FDataCoverage (stores data only for internal nodes)
    SetLength(FDataCoverage, NoOfOtus - 1);

    FInternalNodeLabel := TStringList.Create;
    for i := 0 to NoOfOtus - 2 do
      FInternalNodeLabel.Add(EmptyStr);

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
    FStatsStdDev := nil;

    SetLength(FIsGeneDupEvent, 2 * NoOfOTUs - 1); { this is larger because the root node is included}
    for i := Low(FIsGeneDupEvent) to High(FIsGeneDupEvent) do
      FIsGeneDupEvent[i] := False;

    SetLength(FIsSpeciationEvent, 2 * NoOfOtus - 1); { root node included}
    for i := Low(FIsSpeciationEvent) to High(FIsSpeciationEvent) do
      FIsSpeciationEvent[i] := False;

    SetLength(FIsOutgroupMember, NoOfOtus);
    for i := Low(FIsOutgroupMember) to High(FIsOutgroupMember) do
      FIsOutgroupMember[i] := False;

    FIsStats := Stats;

    GetMem(FStats, SizeOf(Double)*2*(NoOfOTUs-1));
    FillChar(FStats^, SizeOf(Double)*2*(NoOfOTUs-1), 0);

    FHasMeanBcl := False;
    FMeanBcl := nil;
  end;
end;

destructor TTreeData.Destroy;
var
  i: Integer  = -1;
begin
  if Assigned(FInternalNodeLabel) then
    FInternalNodeLabel.Free;
  if NoOfOTUs > 0 then
  begin
    FreeMemAndNil(FNode);
    FreeMemAndNil(FBLen);
    FreeMemAndNil(FSE);
    FreeMemAndNil(FStats);
    FreeMemAndNil(FMeanBcl);
    SetLength(FIsGeneDupEvent, 0);
    SetLength(FIsSpeciationEvent, 0);
    SetLength(FIsOutgroupMember, 0);
    SetLength(FDataCoverage, 0);
  end;
  FNoOfOTUs := 0;
  if Length(FTreeSplits) > 0 then
  begin
    for i := Low(FTreeSplits) to High(FTreeSplits) do
      if Assigned(FTreeSplits[i]) then
        FTreeSplits[i].Free;
    SetLength(FTreeSplits, 0);
  end;
  inherited
end;

function TTreeData.FindTwoExtantTaxa(const aNodeId: Integer;
  var taxonA: Integer; var taxonB: Integer): Boolean;
var
  id1Found: Boolean = False;
  id2Found: Boolean = False;
  desId: Integer = -1;
begin
  Result := False;
  if aNodeId >= FNoOfOTUs then
    desId := aNodeId - FNoOfOTUs
  else
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

  if aNodeId >= FNoOfOTUs then
    desId := aNodeId - FNoOfOTUs
  else
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
  i: Integer = -1;
begin
  Result := 0;
  for i := 0 to FNoOfOTUs - 1 do
    if IsOutgroupMember[i] then
      Result := Result + 1;
end;

function TTreeData.GetOutgroupMembers: TArrayOfBoolean;
var
  i: Integer = -1;
begin
  SetLength(Result, Length(FIsOutgroupMember));
  if Length(Result) > 0 then
    for i := Low(Result) to High(Result) do
      Result[i] := FIsOutgroupMember[i];
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
      RemoveOTU(i, False, EmptyStr);
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

function TTreeData.NumInternalNodes(isRooted: Boolean): Integer;
begin
  Result := 2*FNoOfOTUs - 1 - FNoOfOTUs;
  if isRooted then
    Result := Result + 1;
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

function TTreeData.GetInternalNodeLabel(Index: Integer): String;
begin
  Result := FInternalNodeLabel[Index];
end;

function TTreeData.GetIsStatsStdDev: Boolean;
begin
  Result := Assigned(FStatsStdDev);
end;

function TTreeData.GetMeanBcl(Index: Integer): Double;
begin
  Result := 0.0;
  if (index < 0) or (index >= 2*(NoOfOTUs-1)) then Exit;
  Result := FMeanBcl[index];
end;

function TTreeData.GetStatsStdDev(Index: Integer): Double;
begin
  Result := 0.0;
  if (Index < 0) or (Index >= 2*(NoOfOTUs - 1)) or (not IsStatsStdDev) then Exit;
  Result := FStatsStdDev[Index];
end;

procedure TTreeData.SetDataCoverage(Index: Integer; AValue: Double);
begin
  Assert((Index >= 0) and (Index <= NoOfOtus));
  FDataCoverage[Index] := AValue;
end;

procedure TTreeData.SetHasMeanBcl(AValue: Boolean);
begin
  if FHasMeanBcl = AValue then Exit;
  FHasMeanBcl := AValue;
  if FHasMeanBcl then
  begin
    GetMem(FMeanBcl, SizeOf(Double)*2*(NoOfOTUs-1));
    FillChar(FMeanBcl^, SizeOf(Double)*2*(NoOfOTUs-1), 0);
  end
  else
  begin
    if Assigned(FMeanBcl) then
      FreeMemAndNil(FMeanBcl);
  end;
end;

procedure TTreeData.SetInternalNodeLabel(Index: Integer; AValue: String);
begin
  FInternalNodeLabel[Index] := AValue;
end;

procedure TTreeData.SetIsStatsStdDev(AValue: Boolean);
begin
  if AValue then
  begin
    if not Assigned(FStatsStdDev) then
    begin
      GetMem(FStatsStdDev, SizeOf(Double)*2*(NoOfOTUs - 1));
      FillChar(FStatsStdDev^, SizeOf(Double)*2*(NoOfOTUs - 1), 0);
    end;
  end
  else
    if Assigned(FStatsStdDev) then
      FreeAndNil(FStatsStdDev);
end;

procedure TTreeData.SetMeanBcl(Index: Integer; AValue: Double);
begin
  if (index < 0) or (index >= 2*(NoOfOTUs - 1)) then Exit;
  FMeanBcl[index] := AValue;
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

procedure TTreeData.ClearBlens;
begin
  FillChar(FBLen^, SizeOf(Double)*2*(NoOfOTUs-1), 0);
end;

procedure TTreeData.InitTreeSplits;
var
  i: Integer = -1;
  j: Integer = -1;
  inClade1: array of Boolean;

  procedure AddToClade1(aSplit: TTreeSplit; nodeIndex: Integer);
  begin
    if nodeIndex < FNoOfOTUs then
      inClade1[nodeIndex] := True
    else
    begin
      AddToClade1(aSplit, Node[nodeIndex - FNoOfOTUs].des1);
      AddToClade1(aSplit, Node[nodeIndex - FNoOfOTUs].des2)
    end;
  end;

begin
  SetLength(FTreeSplits, 2*NoOfOTUs - 2);
  SetLength(inClade1, FNoOfOTUs);
  for i := 0 to FNoOfOTUs - 1 do
    inClade1[i] := False;
  for i := Low(FTreeSplits) to High(FTreeSplits) do
    FTreeSplits[i] := TTreeSplit.Create;

  for i := 0 to FNoOfOTUs - 2 do
  begin
    FTreeSplits[Node[i].des1].Ancestor := i + FNoOfOTUs;
    FTreeSplits[Node[i].des1].IsDes1 := True;
    FTreeSplits[Node[i].des1].BranchIndex := Node[i].des1;

    FTreeSplits[Node[i].des2].Ancestor := i + FNoOfOTUs;
    FTreeSplits[Node[i].des2].IsDes2 := True;
    FTreeSplits[Node[i].des2].BranchIndex := Node[i].des2;
  end;

  for i := Low(FTreeSplits) to High(FTreeSplits) do
  begin
    if i < FNoOfOTUs then
      FTreeSplits[i].IsOtu := True;
    if FTreeSplits[i].IsOtu then
    begin
      for j := 0 to FNoOfOTUs - 1 do
        if i = j then
          FTreeSplits[i].AddToClade1(j)
        else
          FTreeSplits[i].AddToClade2(j);
    end
    else if FTreeSplits[i].Ancestor >= FNoOfOTUs then
    begin
      for j := 0 to FNoOfOTUs - 1 do
        inClade1[j] := False;
      if FTreeSplits[i].IsDes1 then
      begin
        AddToClade1(FTreeSplits[i], Node[FTreeSplits[i].Ancestor - FNoOfOTUs].des1);
      end
      else if FTreeSplits[i].IsDes2 then
      begin
        AddToClade1(FTreeSplits[i], Node[FTreeSplits[i].Ancestor - FNoOfOTUs].des2);
      end
      else
        raise Exception.Create(Format('ancestor descendant relationships not set properly for branch %d', [FTreeSplits[i].BranchIndex]));
    end
    else { root node}
    begin
      AddToClade1(FTreeSplits[i], Node[FTreeSplits[i].BranchIndex].des1);
    end;
    if not FTreeSplits[i].IsOtu then
      for j := 0 to Length(inClade1) - 1 do
        if inClade1[j] then
          FTreeSplits[i].AddToClade1(j)
        else
          FTreeSplits[i].AddToClade2(j);
  end;
end;

function TTreeData.ExportSplitsToFile(filename: String): Boolean;
var
  aFile: TextFile;
  i: Integer;
begin
  try
    AssignFile(aFile, filename);
    if FileExists(filename) then
      Append(aFile)
    else
      Rewrite(aFile);
    WriteLn(aFile, TTreeSplit.DebugHeaderString);
    for i := Low(FTreeSplits) to High(FTreeSplits) do
      WriteLn(aFile, FTreeSplits[i].DebugStrings);
  finally
    CloseFile(aFile);
  end;
  Result := FileExists(filename);
end;

function TTreeData.IndexForPartition(partitionString: String; startAt: Integer): Integer;
var
  i: Integer = -1;
begin
  Result := -1;
  if not (Length(FTreeSplits) > 0) then
    raise Exception.Create('tree splits not initialized');
  Assert(startAt < High(FTreeSplits));

  for i := startAt to High(FTreeSplits) do
    if FTreeSplits[i].Matches(partitionString) then
    begin
      Result := i;
      Exit;
    end;

  for i := Low(FTreeSplits) to startAt - 1 do
    if FTreeSplits[i].Matches(partitionString) then
    begin
      Result := i;
      Exit;
    end;
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

procedure TTreeData.SetStatsStdDev(Index: Integer; AValue: Double);
begin
  if (Index < 0) or (Index >= 2*(NoOfOTUs - 1)) then Exit;
  FStatsStdDev[Index] := AValue;
end;

procedure TTreeData.SetValue(AValue: double);
begin
  if FValue = AValue then Exit;
  FValue := AValue;
end;

procedure TTreeData.SetValue2(AValue: double);
begin
  if FValue2 = AValue then Exit;
  FValue2 := AValue;
end;

procedure TTreeData.SetIsStats(value: boolean);
begin
    if value = IsStats then Exit;
    FIsStats := value;
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
  Assert(NoOfOTUs = Source.NoOfOTUs);
  if NoOfOTUs <> Source.NoOfOTUs then Exit;
  FInternalNodeLabel.Assign(Source.FInternalNodeLabel);
  isBLen := Source.isBLen;
  isSE := Source.isSE;
  isStats := Source.isStats;
  IsStatsStdDev := Source.IsStatsStdDev;
  for i := 0 to NoOfOTUs-2 do
  begin
    NodeArray[i] := Source.NodeArray[i];
    DataCoverage[i] := Source.DataCoverage[i];
  end;
  FIsDataCoverage := Source.IsDataCoverage;
  Freq := Source.Freq;
  Value := Source.Value;
  Value2 := Source.Value2;
  Value3 := Source.Value3;
  if IsBLen then
    for i := 0 to 2*NoOfOTUs-3 do
    begin
      BLenArray[i] := Source.BLenArray[i];
    end;
  if IsSE then
    for i := 0 to 2*NoOfOTUs-3 do
      SEArray[i] := Source.SEArray[i];

  HasMeanBcl := Source.HasMeanBcl;
  for i := 0 to 2*NoOfOTUs-3 do
  begin
    StatsArray[i] := Source.StatsArray[i];
    if FHasMeanBcl then
      MeanBcl[i] := Source.MeanBcl[i];
  end;

  if IsStatsStdDev then
    for i := 0 to 2*NoOfOTUs-3 do
      FStatsStdDev[i] := Source.FStatsStdDev[i];
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

function TTreeData.SameTopology(Source: TTreeData): Boolean;
var
  i: integer = -1;
begin
  Result := False;
  if NoOfOTUs <> source.NoOfOTUs then exit;
  if source.RootIndex <> RootIndex then exit;

  Result := True;
  for i := 0 to NoOfOTUs - 2 do
  begin
    if (NodeArray[i].des1 <> source.NodeArray[i].des1) and
       (NodeArray[i].des1 <> source.NodeArray[i].des2) then
    begin
      Result := False;
      break;
    end;
    if (NodeArray[i].des2 <> source.NodeArray[i].des1) and
       (NodeArray[i].des2 <> source.NodeArray[i].des2) then
    begin
      Result := False;
      break;
    end;
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
  b: Boolean = False;
begin
  BlockRead(SessionFile, FFreq, SizeOf(Double));
  BlockRead(SessionFile, FValue, SizeOf(Double));
  BlockRead(SessionFile, FValue2, SizeOf(Double));
  BlockRead(SessionFile, FNode^, SizeOf(TNodeData)*(NoOfOTUs-1));
  if SessionFileVersion >= 1202 then
  begin
    BlockRead(SessionFile, b, SizeOf(Boolean));
    isBlen := b;
  end;

  if isBLen then
    BlockRead(SessionFile, FBLen^, SizeOf(Double)*2*(NoOfOTUs-1));
  if SessionFileVersion >= 1202 then
  begin
    BlockRead(SessionFile, b, SizeOf(Boolean));
    isSE := b;
  end;
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

 if SessionFileVersion >= 1207 then
 begin
   BlockRead(SessionFile, b, SizeOf(Boolean));
   SetIsStatsStdDev(b);
   if b then
   begin
     for i := 0 to 2*(NoOfOTUs - 1) - 1 do
      BlockRead(SessionFile, FStatsStdDev[i], SizeOf(Double));
   end;
 end;

 if SessionFileVersion >= 1209 then
 begin
   BlockRead(SessionFile, b, SizeOf(Boolean));
   HasMeanBcl := b;
   if FHasMeanBcl then
     BlockRead(SessionFile, FMeanBcl^, SizeOf(Double)*2*(NoOfOTUs - 1));
 end;
end;

procedure TTreeData.ReadGeneDupInfoFromSession(var SessionFile: File);
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
  b: Boolean = False;
begin
  BlockWrite(SessionFile, FFreq, SizeOf(Double));
  BlockWrite(SessionFile, FValue, SizeOf(Double));
  BlockWrite(SessionFile, FValue2, SizeOf(Double));
  BlockWrite(SessionFile, FNode^, SizeOf(TNodeData)*(NoOfOTUs-1));
  b := isBlen;
  BlockWrite(SessionFile, b, SizeOf(Boolean));
  if isBLen then
    BlockWrite(SessionFile, FBLen^, SizeOf(Double)*2*(NoOfOTUs-1));
  b := isSE;
  BlockWrite(SessionFile, b, SizeOf(Boolean));
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
  if Assigned(FStatsStdDev) then
  begin
    b := True;
    BlockWrite(SessionFile, b, SizeOf(Boolean));
    for i := 0 to 2*(NoOfOTUs - 1) - 1 do
      BlockWrite(SessionFile, FStatsStdDev[i], SizeOf(Double));
  end
  else
  begin
    b := False;
    BlockWrite(SessionFile, b, SizeOf(Boolean));
  end;

  BlockWrite(SessionFile, FHasMeanBcl, SizeOf(Boolean));
  if FHasMeanBcl then
    BlockWrite(SessionFile, FMeanBcl^, SizeOf(Double)*2*(NoOfOTUs - 1));
end;

function TTreeData.RemoveOTU(otu: integer; resetOutgroup: Boolean; OtuName: String): String;
var
  i: Integer = -1;
  j: Integer = -1;
  parent: Integer = 0;
  sibling: Integer = 0;
  newOutgroupMembers: TArrayOfBoolean;
  outgroupTaxonRemoved: Boolean = False;
  newLabels: TStringList = nil;
begin
  Result := EmptyStr;
  if (otu < 0) or (otu >= NoOfOTUs) then Exit;

  newLabels := TStringList.Create;

  { update FIsOutgroupMember}
  if FIsOutgroupMember[otu] and resetOutgroup then
    outgroupTaxonRemoved := True;
  SetLength(newOutgroupMembers, Length(FIsOutgroupMember) - 1);
  j := 0;
  for i := 0 to Length(FIsOutgroupMember) - 1 do
    if i <> otu then
    begin
      newOutgroupMembers[j] := FIsOutgroupMember[i];
      inc(j);
    end;
  SetLength(FIsOutgroupMember, 0);
  FIsOutgroupMember := newOutgroupMembers;

  { find the parent and sibling of otu to remove. Also, update blens and SE if applicable}
  for i := 0 to NoOfOTUs - 2 do
    if (NodeArray[i].des1 = otu) or (NodeArray[i].des2 = otu) then
    begin
      parent := i;
      if NodeArray[i].des1 = otu then
        sibling := NodeArray[i].des2
      else
        sibling := NodeArray[i].des1;

      if IsBLen then
        BLenArray[sibling] := BLenArray[sibling] + BLenArray[parent + NoOfOTUs];
      if IsSE then
        SEArray[sibling] := Sqrt(SEArray[sibling]*SEArray[sibling] + SEArray[parent + NoOfOTUs]*SEArray[parent + NoOfOTUs]);
      Break;
    end;

  if FInternalNodeLabel[parent] <> EmptyStr then
  begin
    {$IFDEF VISUAL_BUILD}
    if OtuName <> EmptyStr then
      Result := Format('Internal node label "%s" was removed when the taxon labelled "%s" was removed%s', [FInternalNodeLabel[parent], OTUName, LineEnding])
    else
      Result := Format('Internal node label "%s" was removed when pruning taxa from the tree', [FInternalNodeLabel[parent]]);
    {$ELSE}
    if OtuName <> EmptyStr then
      warn_NV(Format('Internal node label "%s" was removed when the taxon labelled "%s" was removed', [FInternalNodeLabel[parent], OTUName]))
    else
      warn_NV(Format('Internal node label "%s" was removed when pruning taxa from the tree', [FInternalNodeLabel[parent]]));
    {$ENDIF};
    FInternalNodeLabel[parent] := EmptyStr;
  end;

  newLabels.Assign(FInternalNodeLabel);

  { mark the sibling as an outgroup member if also is an otu}
  if outgroupTaxonRemoved then
  begin
    if sibling <= High(FIsOutgroupMember) then
      FIsOutgroupMember[sibling] := True;
  end;

  { update the parent-child relationships}
  for i := 0 to NoOfOTUs - 3 do
  begin
    { parent gets removed along with otu so for NodeArray at parent and higher, shift des down}
    if i >= parent then
    begin
      NodeArray[i].des1 := NodeArray[i + 1].des1;
      NodeArray[i].des2 := NodeArray[i + 1].des2;
      newLabels[i] := FInternalNodeLabel[i + 1];
    end;


    if NodeArray[i].des1 = parent + NoOfOTUs then { NodeArray[i].des1 is the parent that is removed along with otu}
      NodeArray[i].des1 := sibling
    else if NodeArray[i].des2 = parent + NoOfOTUs then { NodeArray[i].des2 is the parent that is removed along with otu}
      NodeArray[i].des2 := sibling;

    if NodeArray[i].des1 > parent + NoOfOTUs then { NodeArray[i].des1 is a deeper des than parent}
      NodeArray[i].des1 := NodeArray[i].des1 - 2
    else if NodeArray[i].des1 > otu then { NodeArray[i].des1 is an otu higher in the list than otu}
      NodeArray[i].des1 := NodeArray[i].des1 - 1;

    if NodeArray[i].des2 > parent+NoOfOTUs then
      NodeArray[i].des2 := NodeArray[i].des2 - 2
    else if NodeArray[i].des2 > otu then
      NodeArray[i].des2 := NodeArray[i].des2 - 1;
  end;

  if IsBLen then
    for i := 0 to 2*NoOfOTUs - 3 do
      if i > parent + NoOfOTUs then
        BLenArray[i - 2] := BLenArray[i]
      else if i > otu then
        BLenArray[i - 1] := BLenArray[i];
  if IsSE then
    for i := 0 to 2*NoOfOTUs - 3 do
      if i > parent + NoOfOTUs then
        SEArray[i - 2] := SEArray[i]
      else if i > otu then
        SEArray[i - 1] := SEArray[i];

  for i := 0 to 2*NoOfOTUs - 3 do
    if i > parent + NoOfOTUs then
      StatsArray[i - 2] := StatsArray[i]
    else if i > otu then
      StatsArray[i - 1] := StatsArray[i];
  NoOfOTUs := NoOfOTUs - 1;
  FInternalNodeLabel.Assign(newLabels);
end;

// We may need to find the NodeID of the internal node which is the most recent comman ancestor of taxonA and taxonB.
function TTreeData.ResolveRelTimes(var Times: TCalibrations): Boolean;
var
  i: Integer = -1;
  aCalib: TCalibrationTime = nil;
begin
  Assert(Times.Count > 0, 'trying to resolve calibration node IDs for an empty list of calibrations');
  Result := True;
  for i := 0 to Times.Count - 1 do
  begin
    aCalib := Times.GetCalibration(i);
    if aCalib.NodeID < 0 then
    begin
      aCalib.NodeID := MRCA(aCalib.NodeIDA, aCalib.NodeIDB);
      if aCalib.NodeID < 0 then
        Result := False;
    end;
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

procedure TTreeData.AssignInternalNodeLabels(newLabels: TStringList);
var
  i: Integer = -1;
begin
  Assert(newLabels.Count = FNoOfOTUs - 1, Format('expected a list with %d node labels but got a list with %d', [FNoOfOTUs - 1, newLabels.Count]));
  FInternalNodeLabel.Clear;
  for i := 0 to FNoOfOTUs - 2 do
    FInternalNodeLabel.Add(EmptyStr);
  if newLabels.Count > 0 then
    for i := 0 to newLabels.Count - 1 do
      FInternalNodeLabel[i] := newLabels[i];
end;

function TTreeData.HasInternalNodeLbls: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if Assigned(FInternalNodeLabel) and (FInternalNodeLabel.Count > 0) then
    for i := 0 to FInternalNodeLabel.Count - 1 do
      if Trim(FInternalNodeLabel[i]) <> EmptyStr then
        Exit(True);
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
      result := i;
      break;
    end;
  setlength(flag, 0);
end;

function TTreeData.StateToStringList(isBriefSummary: Boolean; Comment: AnsiString): TStringList;
var
  MyStringList: TStringList;
  i: Integer;
  MyCount: Integer;
begin
  MyStringList := TStringList.Create();
  if isBriefSummary then
  begin
    MyStringList.Add(Format('Value=%.2n Value2=%.2n SBL=%.2n', [Value, Value2, SBL]));
    Result := MyStringList;
    Exit;
  end;

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
    MyStringList.Add('TTreeData.Node[' + IntToStr(i) + ']=des1:' + IntToStr(Node[i].des1) + ', des2:' + IntToStr(Node[i].des2) + ', ' + FInternalNodeLabel[i]);

  MyCount := 2*(NoOfOTUs-1);
  for i := 0 to MyCount - 1 do
    MyStringList.Add('BLen[' + IntToStr(i) + ']=' + FloatToStrF(BLen[i], ffFixed, 8, 8));

  if IsSe then
    for i := 0 to MyCount - 1 do
      MyStringList.Add('SE[' + IntToStr(i) + ']=' + FloatToStrF(FSE[i], ffFixed, 4, 4));

  if IsStats then
  begin
    for i := 0 to MyCount - 1 do
      MyStringList.Add('Stats[' + IntToStr(i) + ']=' + FloatToStrF(FStats[i], ffFixed, 4, 4));
    for i := 0 to MyCount - 1 do
      MyStringList.Add('MeanBCL[' + IntToStr(i) + ']=' + FloatToStrF(MeanBcl[i], ffFixed, 4, 4));
  end;

  for i := 0 to Length(FIsOutgroupMember) - 1 do
    MyStringList.Add(Format('Outgroup[%d]=%s', [i, BoolToStr(FIsOutgroupMember[i], True)]));
  Result := MyStringList;
end;

function TTreeData.StateToDebugFile(filename: String; Comment: String): Boolean;
var
  aList: TStringList = nil;
begin
  try
    aList := StateToStringList(False, Comment);
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
