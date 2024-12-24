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

unit MTreeList;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, FileUtil, Classes, StdCtrls, ExtCtrls, SysUtils, Math,
  MegaConsts, MTreeData, MPleaseWait, MCalibrationData,
  MNewickExportOptions;

const
  GENE_DUPLICATION_STR = '[GeneDuplication]';
  SPECIATION_STR = '[Speciation]';

type

  TOutgroupNameValuePair = record
    TaxonName: String;
    IsOutgroupMember: Boolean;
  end;

  TOutgroupNameValuePairArray = array of TOutgroupNameValuePair;

  { TTreeList }

  TTreeList = class(TList)
  private
    CurOTU, CurNode, CurPos: Integer;
    FErrorMsg: String;
    FInformation: TStringList;
    FFreqName: AnsiString;
    FValueName: AnsiString;
    FValue2Name: AnsiString;
    FStatsName: AnsiString;
    FMaxStats: double;
    FDistanceMatrix: PDistanceMatrix;
    FOTUName: TStringList;
    FSpeciesNames: TStringList;
    FisRooted : boolean;
    FNewickExportOptions: TNewickExportOptions;
    function GetInternalNodeLabelsForTree(TreeIndex: Integer): TStringList;
    function GetOutputNodeLabels: Boolean;
    procedure LogErrorMsg(aMsg: String);
    procedure InitDefaultNewickExportOptions;
    function HasGeneDupsOrSpeciationMarkers(NewickStr: String): Boolean;
    function FindRootIndex(TreeIndex: Integer): Integer;
    function GetItems(index: integer):TTreeData;
    procedure SetItems(index: integer; value: TTreeData);

    function GetNoOfTrees:integer;
    function GetIsBLen:boolean;
    function GetIsSE:boolean;
    function GetIsStats:boolean;
    procedure SetIsBLen(value: boolean);
    procedure SetIsSE(value: boolean);
    procedure SetIsStats(value: boolean);
    function GetTotalFrequency:double;
    procedure SetMaxStats(AValue: Double);
    procedure SetOutputNodeLabels(AValue: Boolean);
    procedure SetTotalFrequency(value : double);
    function GetOTUName(index: integer):AnsiString;
    procedure SetOTUName(index: integer; value: AnsiString);
    function GetNoOfOTUs:integer;
    function GetHasInternalNodeLbls(treeIndex: Integer): Boolean;
    procedure SetInformation(source: TStringList);
    function WriteATreeToNewickFile(index: integer; var f: TextFile; bl,st:boolean; CutOff: double): boolean;
    function GetSpeciesName(Index: Integer): AnsiString;
    procedure SetSpeciesName(Index: Integer; const Value: AnsiString);
    function WriteSpeciesNamesToSession(var SessionFile: File): Boolean;
    function ReadSpeciesNamesFromSession(var SessionFile: File): Boolean;
    function FindTwoExtantTaxaIDs(const aNodeId: Integer; var taxonA: Integer; var taxonB: Integer): Boolean;
    function ParseStatsAndStdDevStr(const input: AnsiString; var statsStr: AnsiString; var stdDevStr: AnsiString): Boolean;
  public
    class function CleanOtuNameForExport(aName: AnsiString): AnsiString; static;
    function HasIdenticalTree(aTree: TTreeData): Boolean;
    procedure SetOtuNames(aNames: TStringList);
    procedure ClearOutgroup;
    procedure ClearInternalNodeLabels;
    function NumOutgroupMembers(TreeIndex: Integer): Integer;
    function HasUnwantedTaxa(WantedTaxa: TStringList): Boolean; { checks if the TreeList has any taxa names that are not in WantedTaxa}
    function GetOutgroupNameValuePairs(TreeIndex: Integer = 0): TOutgroupNameValuePairArray;
    function GetTrimmedTaxaNames: TStringList;
    function GetOtuToSpeciesMapList: TStringList; { gets a tstringlist where each item is 'otuname=speciesname'}
    procedure AssignSpeciesNames(AList: TStringList);
    procedure SetSpeciesNameForOtu(SpeciesName: AnsiString; OtuName: AnsiString);
    function HasSpeciesNames: Boolean;
    function GetSpeciesNameForOtu(OtuName: AnsiString): AnsiString;
    procedure AddInformation(aKey: String; aValue: String);
    function GetInternalNodeLbl(treeIndex: Integer; nodeIndex: integer):AnsiString; // public so TCalibrationDlg can get a reference to it
    function GetInternalNodeLblNoQuotes(treeIndex: Integer; nodeIndex: Integer): AnsiString;
    function GetOutgroupTaxaNames(treeIndex: Integer): TStringArray; overload;
    function GetOutgroupTaxaNames(treeIndex: Integer; var aNames: TStringList): Boolean; overload;
    property Items[Index: integer]: TTreeData read GetItems write SetItems; default;
    property Information: TStringList read FInformation write SetInformation;
    property ValueName: AnsiString read FValueName write FValueName;
    property Value2Name: AnsiString read FValue2Name write FValue2Name;
    property FreqName: AnsiString read FFreqName write FFreqName;
    property StatsName: AnsiString read FStatsName write FStatsName;
    property MaxStats: Double read FMaxStats write SetMaxStats;
    property DistanceMatrix: PDistanceMatrix read FDistanceMatrix write FDistanceMatrix;
    property OTUName[Index: integer]: AnsiString read GetOTUName write SetOTUName;
    property SpeciesName[Index: Integer]: AnsiString read GetSpeciesName write SetSpeciesName;
    property OTUNameList: TStringList read FOTUName write FOTUName;
    property SpeciesNamesList: TStringList read FSpeciesNames;
    property isRooted: boolean read FisRooted write FisRooted;
    property isBLen: boolean read GetIsBLen write SetIsBLen;
    property isSE: boolean read GetIsSE write SetIsSE;
    property isStats:boolean read GetIsStats write SetIsStats;
    property NoOfTrees: integer read GetNoOfTrees;
    property NoOfOTUs: integer read GetNoOfOTUs;
    property TotalFrequency: double read GetTotalFrequency write SetTotalFrequency;
    property InternalNodeLbls[TreeIndex: Integer]: TStringList read GetInternalNodeLabelsForTree;
    property HasInternalNodeLbls[treeIndex: Integer]: boolean read GetHasInternalNodeLbls;
    procedure Add(tree: TTreeData);
    procedure AddParsimonyTree(tree: TTreeData);
    function ValidateParsimonyTrees: Boolean;
    procedure Insert(index: integer; tree: TTreeData);
    function Remove(index: integer):TTreeData;
    function RemoveOtu(aName: String; resetOutgroup: Boolean; var aMsg: String): Boolean;
    function HasOtu(aName: String): Boolean;
    procedure Delete(index: integer);
    procedure RemoveNoFree(index: Integer);
    procedure DeleteAll;
    procedure Clear; override;
    procedure Assign(Source: TTreeList);
    function ImportFromNewickFile(filename: String; NameList: TStringList; ShowProgress: Boolean=True): boolean;
    function ImportFromNewick(NewickTree: AnsiString; NameList: TStringList; ShowProgress: Boolean=True): boolean;
    {function ImportFromTableFile(filename: AnsiString; NameList: TStringList): boolean;
    function ImportFromTable(Table: TStringList): boolean;}
    function StripExtraneousParens(NewickString: AnsiString): AnsiString;
    function DebugExportToNewickFile(aFile: String; aData: TTreeData; aNames: TStringList): Boolean;
    function ExportToNewickFile(filename: String; bl,st:boolean; CutOff: double; MaxNumTrees:Integer = 0): boolean; overload;
    function ExportToNewickFile(filename: String; bl,st: Boolean; Cutoff: Double; valuesToInclude: TTreeDataValueSet): Boolean; overload;
    function ExportATreeToNewickFile(index: integer; filename :String; bl,st:boolean; CutOff: double; comment: String = ''): boolean;
    function DebugOutputNewickTree(aData: TTreeData; aNames: TStringList): String;
    function OutputNewickTree(TreeIndex: integer; UseBranchLengths, UseStats:boolean; CutOff: double): AnsiString; overload;
    function OutputNewickTree(index: integer; Options: TNewickExportOptions; CutOff: double): AnsiString; overload;
    function OutputAllNewickTrees(Options: TNewickExportOptions; Cutoff: Double; valuesToInclude: TTreeDataValueSet = []): TStringList;
    function OutputTable(index: integer; bl,st:boolean; CutOff: double): AnsiString;
    procedure ReadFromFile(var SessionFile: File; SessionFileVersion: integer);
    procedure WriteToFile(var data: File);
    function ToStringList(isBriefSummary: Boolean = False): TStringList;
    procedure SetOutgroupMembers(Members: TBoolArray); overload;
    procedure SetOutgroupMembers(MemberNames: TStringList); overload;

    procedure SetOutgroupMember(Index: Integer; IsOutgroup: Boolean);
    function MarkTaxonAsOutgroupMember(TaxonName: String): Boolean;
    procedure SetOutgroupMemberForRootedTree;
    constructor Create;
    destructor Destroy; override;
    function TranslateOtuNames(Translation: TStringList): Boolean; { Translation consist of key value pairs of the form 'oldName=newName'}
    function NodeNameToID(taxaName: AnsiString): Integer;
    function NodeLabelToID(NodeName: AnsiString): Integer;
    procedure ResolveCalibrationTimeNames(var times: TCalibrations);
    procedure ResolveCalibrationTimeName(var Calibration: TCalibrationTime);
    procedure ResolveGroupCalibrationTimeName(var Calibration: TGroupCalibrationTime);
    function FinalizeCalibration(var Calibration: TCalibrationTime): Boolean;
    function FindMrca(TaxonAName: AnsiString; TaxonBName: AnsiString): Integer;
    function IsOutgroupMember(aOtuName: String): Boolean;
    procedure ChangeOtuName(oldName: String; newName: String);
    property ErrorMsg: String read FErrorMsg;
    property OutputNodeLabels: Boolean read GetOutputNodeLabels write SetOutputNodeLabels;
  end;

  TTreeListArray = array of TTreeList;

function DebugOutputTreeToNewickFile(const aData: TTreeData; const aNames: TStringList; const filename: String; var msg: String): Boolean;
function TreeDataToString(aData: TTreeData; aNames: TStringList): String;
function CompareTreeDataValue(Item1: Pointer; Item2: Pointer): Integer;
function CompareTreeLenAndInitLnl(Item1: Pointer; Item2: Pointer): Integer;
function IsRootedTree(aNewickStr: String; aOtuNames: TStringList): Boolean;

implementation

uses
{$IFDEF VISUAL_BUILD}
  Dialogs,
{$ELSE}
  MD_MegaMain, MRuntimeProgressDlg,  MegaUtils_NV,
{$ENDIF}
  MegaUtils, StrUtils, MTreeDataAdapter, MLongintList, mtree_display_setup;

function DebugOutputTreeToNewickFile(const aData: TTreeData; const aNames: TStringList; const filename: String; var msg: String): Boolean;
var
  aList: TTreeList = nil;
  data: TTreeData = nil;
begin
  try
    try
      Result := False;
      aList := TTreeList.Create;
      data := TTreeData.Create(aData.NoOfOTUs, aData.isBLen, aData.isSE, aData.isStats);
      data.Assign(aData);
      aList.Add(data);
      aList.OTUNameList.Assign(aNames);
      aList.ExportATreeToNewickFile(0, filename, data.isBLen, data.isStats, 0.0);
      Result := FileExists(filename);
    except
      on E:Exception do
      begin
        Result := False;
        msg := E.Message;
      end;
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end
end;

function TreeDataToString(aData: TTreeData; aNames: TStringList): String;
var
  aList: TTreeList = nil;
  tempData: TTreeData = nil;
begin
  try
    Result := EmptyStr;
    aList := TTreeList.Create;
    aList.OTUNameList.AddStrings(aNames);
    tempData := TTreeData.Create(aData.NoOfOTUs, aData.isBLen, aData.isSE, aData.isStats);
    tempData.Assign(aData);
    aList.Add(tempData);
    Result := aList.OutputNewickTree(0, aData.isBLen, aData.isStats, 0.0);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function CompareTreeDataValue(Item1: Pointer; Item2: Pointer): Integer;
var
  d1: TTreeData = nil;
  d2: TTreeData = nil;
begin
  d1 := TTreeData(Item1);
  d2 := TTreeData(Item2);
  Result := CompareValue(d1.Value, d2.Value);
end;

function CompareTreeLenAndInitLnl(Item1: Pointer; Item2: Pointer): Integer;
var
  d1: TMPMLTreeData = nil;
  d2: TMPMLTreeData = nil;
begin
  d1 := TMPMLTreeData(Item1);
  d2 := TMPMLTreeData(Item2);
  Result := CompareValue(abs(d1.TreeLength), abs(d2.TreeLength));
  if Result = 0 then
    Result := CompareValue(abs(d1.InitialLogLikelihood), abs(d2.InitialLogLikelihood));
end;

function IsRootedTree(aNewickStr: String; aOtuNames: TStringList): Boolean;
var
  aList: TTreeList = nil;
begin
  try
    aList := TTreeList.Create;
    if not aList.ImportFromNewick(aNewickStr, aOtuNames, False) then
      Result := False
    else
      Result := aList.isRooted;
  finally
    if Assigned(aList) then
      aList.Free
  end;
end;

{ TTreeList }

constructor TTreeList.Create;
begin
  inherited Create;
  FInformation := TStringList.Create;
  FOTUName := TStringList.Create;
  FSpeciesNames := nil;
  FMaxStats := -1.0;
  InitDefaultNewickExportOptions;
end;

destructor TTreeList.Destroy;
begin
  inherited Destroy;
  if Assigned(FInformation) then
    FreeAndNil(FInformation);
  if Assigned(FOTUName) then
    FreeAndNil(FOTUName);
  if Assigned(FSpeciesNames) then
    FreeAndNil(FSpeciesNames);
end;

function TTreeList.TranslateOtuNames(Translation: TStringList): Boolean;
var
  i: Integer;
  Index: Integer;
begin
  Result := False;
  FErrorMsg := EmptyStr;
  if FOTUName.Count > Translation.Count then
  begin
    FErrorMsg := 'Found ' + IntToStr(FOTUName.Count) + ' OTUs in tree but there are ' + IntToStr(Translation.Count) + ' translations provided';
    Exit;
  end;
  for i := 0 to Translation.Count - 1 do
  begin
    Index := FOTUName.IndexOf(Translation.Names[i]);
    if Index >= 0 then
    begin
      FOTUName[Index] := Translation.ValueFromIndex[i];
    end;
    //else { name is not found in our list so make translation fail}
    //begin
    //  FErrorMsg := 'The translation ' + #39 + Translation[i] + #39 + ' could not be resolved';
    //  Exit;
    //end;
  end;
  Result := True;
end;

function TTreeList.GetItems(index: integer):TTreeData;
begin
  result := inherited Items[index];
end;

function TTreeList.GetOtuToSpeciesMapList: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  if Assigned(FOtuName) and Assigned(FSpeciesNames)and (FOtuName.Count = FSpeciesNames.Count) then
    if FOtuName.Count > 0 then
      for i := 0 to FOtuName.Count - 1 do
        Result.Add(FOtuName[i] + '=' + FSpeciesNames[i]);
end;

procedure TTreeList.SetItems(index: integer; value: TTreeData);
begin
  inherited Items[index] := value;
end;

function TTreeList.GetOTUName(index: integer):AnsiString;
begin
  if index >= FOTUName.Count then
    result := ''
  else
    result := FOTUName[index];
end;

function TTreeList.GetSpeciesName(Index: Integer): AnsiString;
begin
  if not Assigned(FSpeciesNames) then
    Result := EmptyStr
  else
  begin
    if index >= FSpeciesNames.Count then
      Result := EmptyStr
    else
      Result := FSpeciesNames[Index];
  end;
end;

function TTreeList.GetSpeciesNameForOtu(OtuName: AnsiString): AnsiString;
var
  Index: Integer;
begin
  Result := EmptyStr;
  Index := OTUNameList.IndexOf(OtuName);
  if Index < FSpeciesNames.Count then
    FSpeciesNames[Index];
end;

procedure TTreeList.AddInformation(aKey: String; aValue: String);
begin
  FInformation.Add(Format('%s%s%s', [aKey, FInformation.NameValueSeparator, aValue]));
end;

procedure TTreeList.SetOTUName(index: integer; value: AnsiString);
begin
  while index > FOTUName.Count-1 do
    FOTUName.Append('');
  FOTUName[index] := value;
end;

procedure TTreeList.SetOutgroupMember(Index: Integer; IsOutgroup: Boolean);
var
  i: Integer;
begin
  Assert(NoOfTrees > 0);
  for i := 0 to NoOfTrees - 1 do
    Items[i].IsOutgroupMember[Index] := IsOutgroup;
end;

procedure TTreeList.SetOutgroupMembers(MemberNames: TStringList);
var
  index: Integer;
  i, j: Integer;
begin
  ClearOutgroup;
  if MemberNames.Count > 0 then
    for i := 0 to MemberNames.Count - 1 do
    begin
      index := OTUNameList.IndexOf(MemberNames[i]);
      if index < 0 then
        raise Exception.Create('otu name not found in tree list');
      for j := 0 to NoOfTrees - 1 do
        Items[j].IsOutgroupMember[index] := True;
    end;
end;

procedure TTreeList.SetOutgroupMembers(Members: TBoolArray);
var
  i, j: Integer;
begin
  Assert(Length(Members) = (NoOfOtus));
  Assert(NoOfTrees > 0);
  if NoOfTrees = 0 then
    exit;
  for i := 0 to NoOfTrees - 1 do
  begin
    for j := 0 to NoOfOtus - 1 do
      Items[i].IsOutgroupMember[j] := Members[j];
  end;
end;

procedure TTreeList.SetOutgroupMemberForRootedTree;

  procedure SetEachOutgroupMember(index: integer);
  begin
    if index < NoOfOTUs then
      SetOutgroupMember(index, true)
    else
      SetEachOutgroupMember(items[0].node[index].des2);
  end;

var
  rootindex, i: integer;
begin
  for i := 0 to NoOfOTUs-1 do
    SetOutgroupMember(i, false);
  rootindex := FindRootIndex(0);
  SetEachOutgroupMember(rootindex);
end;

procedure TTreeList.SetSpeciesName(Index: Integer; const Value: AnsiString);
var
  AStr: AnsiString;
begin
  if not Assigned(FSpeciesNames) then
    FSpeciesNames := TStringList.Create;
  while Index > FSpeciesNames.Count - 1 do
    FSpeciesNames.Append(EmptyStr);
  AStr := Value;
  TrimTaxaName(AStr);
  FSpeciesNames[Index] := AStr;
end;

procedure TTreeList.SetSpeciesNameForOtu(SpeciesName: AnsiString;
  OtuName: AnsiString);
var
  Index: Integer;
  AName: AnsiString;
  BName: AnsiString;
begin
  if not Assigned(FSpeciesNames) then
    FSpeciesNames := TStringList.Create;
  AName := OtuName;
  TrimTaxaName(AName);
  Index := OTUNameList.IndexOf(AName);
  if Index < 0 then
    Index := OTUNameList.IndexOf(OtuName);
  Assert(Index >= 0);
  while Index > FSpeciesNames.Count - 1 do
    FSpeciesNames.Append(EmptyStr);
  BName := SpeciesName;
  TrimTaxaName(BName);
  FSpeciesNames[Index] := BName;
end;

function TTreeList.GetInternalNodeLbl(treeIndex: Integer; nodeIndex: integer):AnsiString;
begin
  result := GetItems(treeIndex).InternalNodeLabel[nodeIndex];
end;

function TTreeList.GetInternalNodeLblNoQuotes(treeIndex: Integer; nodeIndex: Integer): AnsiString;
var
  aLabel: AnsiString = '';
begin
  aLabel := GetItems(treeIndex).InternalNodeLabel[nodeIndex];
  if aLabel <> EmptyStr then
    TrimTaxaName(aLabel);
  Result := aLabel;
end;

function TTreeList.GetOutgroupTaxaNames(treeIndex: Integer): TStringArray;
var
  aData: TTreeData = nil;
  i: Integer = -1;
  nextIndex: Integer = -1;
begin
  SetLength(Result, 0);
  if treeIndex < Count then
  begin
    aData := Items[treeIndex];
    for i := 0 to NoOfOTUs - 1 do
      if aData.IsOutgroupMember[i] then
      begin
        nextIndex := Length(Result);
        SetLength(Result, nextIndex + 1);
        Result[nextIndex] := OTUName[i];
      end;
  end;
end;

function TTreeList.GetOutgroupTaxaNames(treeIndex: Integer; var aNames: TStringList): Boolean;
var
  aData: TTreeData = nil;
  i: Integer = -1;
  nextIndex: Integer = -1;
begin
  Assert(Assigned(aNames));
  aNames.Clear;
  if treeIndex < Count then
  begin
    aData := Items[treeIndex];
    for i := 0 to NoOfOTUs - 1 do
      if aData.IsOutgroupMember[i] then
        aNames.Add(OTUName[i]);
  end;
  Result := aNames.Count > 0;
end;

function TTreeList.GetHasInternalNodeLbls(treeIndex: Integer): Boolean;
begin
  Result := GetItems(treeIndex).HasInternalNodeLbls;
end;

function TTreeList.GetIsBLen:boolean;
begin
  if NoOfTrees = 0 then
    Result := false
  else
    Result := Items[0].isBLen;
end;

function TTreeList.GetIsSE:boolean;
begin
  if NoOfTrees = 0 then
    Result := false
  else
    Result := Items[0].isSE;
end;

function TTreeList.GetIsStats:boolean;
begin
  if NoOfTrees = 0 then
    Result := false
  else
    Result := (MaxStats > -0.000000001);
end;

procedure TTreeList.SetIsBLen(value: boolean);
var
  i: integer;
begin
  if value = IsBLen then Exit;
  for i := 0 to Count-1 do
    Items[i].IsBLen := value;
end;

procedure TTreeList.SetIsSE(value: boolean);
var
  i: integer;
begin
  if value = IsSE then Exit;
  for i := 0 to Count-1 do
    Items[i].IsSE := value;
end;

procedure TTreeList.SetIsStats(value: boolean);
begin
  if value = IsStats then Exit;
  if value then
    MaxStats := 0.0
  else
    MaxStats := -1.0;
{
  for i := 0 to Count-1 do
    Items[i].IsStats := value;
}
end;

function TTreeList.GetNoOfTrees:integer;
begin
  Result := Count;
end;

function TTreeList.GetNoOfOTUs:integer;
begin
  if NoOfTrees = 0 then
    Result := 0
  else
    Result := Items[0].NoOfOTUs;
end;

procedure TTreeList.Add(tree: TTreeData);
begin
  if NoOfTrees = 0 then
  begin
    if tree.NoOfOTUs = 0 then Exit;
    inherited Add(tree);
  end
  else
  begin
    Assert(tree.NoOfOTUs = NoOfOTUs, Format('expected %d taxa but got %d', [tree.NoOfOTUs, NoOfOTUs]));
    Assert(tree.isBLen = isBLen, 'TTreeList does not support both trees with and without branch lengths');
    if tree.NoOfOTUs <> NoOfOTUs then Exit;
    if tree.isBLen <> isBLen then Exit;

    inherited Add(tree);
  end;
end;

procedure TTreeList.AddParsimonyTree(tree: TTreeData);
var
  i: Integer;
begin
  if Count > 0 then
  begin
    if CompareValue(tree.Value, Items[0].Value, FP_CUTOFF) <> 0 then
      raise Exception.Create('Error: adding parsimony tree with different tree length');
    if Count > 1 then
      for i := 1 to Count - 1 do
      begin
        if CompareValue(tree.Value, Items[i].Value, FP_CUTOFF) <> 0 then
          raise Exception.Create('Error: adding parsimony tree with different tree length');
      end;
  end;
  Add(tree);
end;

function TTreeList.ValidateParsimonyTrees: Boolean;
var
  i: Integer;
  treeLen: Double;
begin
  Result := True;
  if Count > 1 then
  begin
    treeLen := Items[0].Value;
    for i := 1 to Count - 1 do
    begin
      if not (CompareValue(treeLen, Items[i].Value, FP_CUTOFF) = 0) then
        Exit(False);
    end;
  end;
end;

procedure TTreeList.LogErrorMsg(aMsg: String);
begin
  {$IFDEF DEBUG}
  FErrorMsg := Trim(FErrorMsg + LineEnding + Format('%s  CurPos: %d  CurOtu: %d  CurNode: %d', [aMsg, CurPos, CurOtu, CurNode]));
  {$ELSE}
  FErrorMsg := Trim(FErrorMsg + LineEnding + aMsg);
  {$ENDIF}
end;

function TTreeList.GetOutputNodeLabels: Boolean;
begin
  Result := FNewickExportOptions.NodeLabels;
end;

function TTreeList.GetInternalNodeLabelsForTree(TreeIndex: Integer): TStringList;
begin
  Result := nil;
  if TreeIndex < Count then
    Result := GetItems(TreeIndex).GetInternalNodeLabels;
end;

procedure TTreeList.InitDefaultNewickExportOptions;
begin
  FNewickExportOptions.BranchLengths := True;
  FNewickExportOptions.BootstrapVals := False;
  FNewickExportOptions.NodeLabels := True;
  FNewickExportOptions.GeneDuplications := False;
  FNewickExportOptions.SpeciationEvents := False;
  FNewickExportOptions.Reltimes := False;
  FNewickExportOptions.DivergenceTimes := False;
  FNewickExportOptions.UseQuotesForLabels := True;
end;

procedure TTreeList.Insert(index: integer; tree: TTreeData);
begin
  if NoOfTrees = 0 then
    Add(tree)
  else
  begin
    if tree.NoOfOTUs <> NoOfOTUs then Exit;
    if tree.isBLen <> isBLen then Exit;
//    if tree.isSE <> isSE then Exit;
//    if tree.isStats <> isStats then Exit;

    inherited Insert(index, tree);
  end;
end;

function TTreeList.IsOutgroupMember(aOtuName: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  i := OTUNameList.IndexOf(aOtuName);
  if i >= 0 then
    Result := Items[0].IsOutgroupMember[i]
  else
    raise Exception.Create('unknown OtuName');
end;

procedure TTreeList.ChangeOtuName(oldName: String; newName: String);
var
  index: Integer;
begin
  index := OTUNameList.IndexOf(oldName);
  if index >= 0 then
    OTUNameList[index] := newName;
end;

function TTreeList.MarkTaxonAsOutgroupMember(TaxonName: String): Boolean;
var
  TaxonIndex: Integer;
  TreeIndex: Integer;
begin
  Result := False;
  Assert(NoOfTrees > 0);
  TaxonIndex := FOTUName.IndexOf(TaxonName);
  if not TaxonIndex >= 0 then
    Exit;
  for TreeIndex := 0 to NoOfTrees - 1 do
    Items[TreeIndex].IsOutgroupMember[TaxonIndex] := True;
  Result := True;
end;

// Iterates over the OTUName array to find the index (also is the NodeID) of the name.
function TTreeList.NodeNameToID(taxaName: AnsiString): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to OTUNameList.Count - 1 do
    if compareText(OTUName[i], taxaName) = 0 then
    begin
      result := i;
      break;
    end;
end;

// Iterates over the Node label FNodeL array to find the index (also is the NodeID) of the node label.
function TTreeList.NodeLabelToID(NodeName: AnsiString): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to NoOfOTUs-2 do
    if compareText(GetItems(0).InternalNodeLabel[i], NodeName) = 0 then
    begin
      result := i;
      break;
    end;
end;

function TTreeList.Remove(index: integer):TTreeData;
begin
  Result := nil;
  if (index < 0) or (index >= NoOfTrees) then Exit;
  Result := Items[index];
  inherited Remove(Result);
end;

function TTreeList.RemoveOtu(aName: String; resetOutgroup: Boolean; var aMsg: String): Boolean;
var
  index: Integer = -1;
  i: Integer = -1;
  cleanName: String = '';
  tempMsg: String = '';
begin
  Result := False;
  cleanName := aName;
  TrimTaxaName(cleanName);
  index := OTUNameList.IndexOf(cleanName);
  if index < 0 then
    Exit;
  Result := True;
  OTUNameList.Delete(index);
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      tempMsg := Items[i].RemoveOTU(index, resetOutgroup, aName);
      if tempMsg <> EmptyStr then
        aMsg := aMsg + LineEnding + tempMsg;
    end;
end;

function TTreeList.HasOtu(aName: String): Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if Assigned(FOTUName) and (FOTUName.Count > 0) then
  begin
    for i := 0 to FOTUName.Count - 1 do
      if SameText(FOTUName[i], aName) then
        Exit(True);
  end;
end;

// Takes as input a calibration time instance with taxa names and their min-max times.
// Looks up the leaf nodeIDs associated with the names and adds them to the TCalibrationTime object.
// TTreeData nodes are (0:[n-1] are internal, n:[2*n-1] are external/leaf).
procedure TTreeList.ResolveCalibrationTimeName(var Calibration: TCalibrationTime);
var
  TempLbl: AnsiString = '';
  aName: AnsiString = '';
begin
  if Calibration is TIngroupRootCalibration then
  begin
    Calibration.NodeID := GetIngroupRootIndex(Items[0]);
  end;
  if Calibration.IsSamplingTime then
  begin
    Calibration.NodeID := NodeNameToID(Calibration.NodeName); // it is for a leaf node with a sampling time
    if Calibration.NodeID < 0 then
      raise Exception.Create('Invalid taxon name ' + #39 + Calibration.NodeName + #39 + ' provided for sampling time, not found in tree file');
    aName := Calibration.NodeName;
    TrimTaxaName2(aName);
    Calibration.NodeName := aName;
    Exit;
  end;

  if (Calibration.NodeName <> EmptyStr) and (not (Calibration is TIngroupRootCalibration)) then // the user has specified a NodeName given in the newick file
  begin
    Calibration.NodeID := NodeLabelToID(Calibration.NodeName);
    if Calibration.NodeID = -1 then
      raise Exception.Create('Invalid node label ''' + Calibration.NodeName + ''' provided for calibration, not found in tree file');
    GetItems(0).InternalNodeLabel[Calibration.NodeID] := Calibration.NodeName;
    if (Calibration.NodeA = EmptyStr) or (Calibration.NodeB = EmptyStr) then
    begin
      if not FindTwoExtantTaxaIDs(Calibration.NodeID, Calibration.NodeIDA, Calibration.NodeIDB) then
        raise Exception.Create('failed to resolve calibration node IDs');
      Calibration.NodeA := OTUNameList[Calibration.NodeIDA];
      Calibration.NodeB := OTUNameList[Calibration.NodeIDB];
    end;
  end
  else if Trim(Calibration.NodeA) <> EmptyStr then  // then the user specified an MRCA which has TaxonA and TaxonB as leaf descendents
  begin
    TrimTaxaName2(Calibration.NodeA);
    TrimTaxaName2(Calibration.NodeB);

    // get the node ids for the targeted leaf nodes
    if Calibration.NodeA <> EmptyStr then
      Calibration.NodeIDA := NodeNameToID(Calibration.NodeA);
    if Calibration.NodeB <> EmptyStr then
      Calibration.NodeIDB := NodeNameToID(Calibration.NodeB);

    // make sure we found the ids
    if Calibration.NodeIDA = -1 then
      raise Exception.Create('Invalid taxon name ''' + Calibration.NodeA);
    if Calibration.NodeIDB = -1 then
      raise Exception.Create('Invalid taxon name ''' + Calibration.NodeB);

    // so other developers don't break this
    Assert(Assigned(Items[0]), 'Cannot call TTreeList.ResolveRelTimeNames unless the list already contains a tree');

    // check for conflicting labels and if so, warn the user
    Calibration.NodeID := Items[0].MRCA(Calibration.NodeIDA, Calibration.NodeIDB);
    TempLbl := GetInternalNodeLbl(0, Calibration.NodeID);
    if (TempLbl <> EmptyStr) and (TempLbl <> '-') and (TempLbl <> Calibration.MRCAName) then
    begin
      {$IFDEF VISUAL_BUILD}
//      raise Exception.Create('Conflicting labels given for the same node: the calibration input has ' + #39 + Calibration.MRCAName + #39 + ' and the newick file has '+ #39  + TempLbl + #39 + '.');
      {$ELSE}
      warn_nv('Conflicting labels given for the same node - using the label given in the calibration file (' + Calibration.MRCAName + '). Ignoring label given in the newick file (' + TempLbl + ').');
      {$ENDIF}
    end;

    // set the node name for the mrca
    if Trim(Calibration.MRCAName) <> EmptyStr then
      GetItems(0).InternalNodeLabel[Calibration.NodeID] := Calibration.MRCAName;
  end
  else if Calibration.NodeID >= 0 then
  begin
    if FindTwoExtantTaxaIDs(Calibration.NodeID, Calibration.NodeIDA, Calibration.NodeIDB) then
    begin
      Calibration.NodeA := OTUNameList[Calibration.NodeIDA];
      Calibration.NodeB := OTUNameList[Calibration.NodeIDB];
    end;
  end;
end;

procedure TTreeList.ResolveGroupCalibrationTimeName(var Calibration: TGroupCalibrationTime);
var
  TempLbl: AnsiString = '';
  nodeIds: TLongintList = nil;
  i: Integer = -1;
  aId: Integer = -1;
begin
  Assert(Assigned(Items[0]), 'Cannot call TTreeList.ResolveRelTimeNames unless the list already contains a tree');
  try
    nodeIds := TLongintList.Create;
    for i := 0 to Calibration.TaxaCount - 1 do
    begin
      aId := NodeNameToID(Calibration.TaxaNames[i]);
      if aId >= 0 then
        nodeIds.Add(aId)
      else
        raise Exception.Create(Format('calibration taxa names not validated properly (%s) should have been removed from the list as it does not exist in the current data set ', [Calibration.TaxaNames[i]]));
    end;
    Calibration.FindMrcaId(Items[0], OTUNameList);

    //{$IFNDEF VISUAL_BUILD}
    //TempLbl := GetInternalNodeLbl(Calibration.NodeID);
    //if (TempLbl <> EmptyStr) and (TempLbl <> '-') and (TempLbl <> Calibration.MRCAName) then
    //  warn_nv('Conflicting labels given for the same node - using the label given in the calibration file (' + Calibration.MRCAName + '). Ignoring label given in the newick file (' + TempLbl + ').');
    //{$ENDIF}

    if Trim(Calibration.MRCAName) <> EmptyStr then
      GetItems(0).InternalNodeLabel[Calibration.NodeID] := Calibration.MrcaName;
    if FindTwoExtantTaxaIDs(Calibration.NodeID, Calibration.NodeIDA, Calibration.NodeIDB) then
    begin
      Assert(Calibration.NodeIDA >= 0, Format('calibration NodeIDA (%d) not initialized', [Calibration.NodeIDA]));
      Assert(Calibration.NodeIDB >= 0, Format('calibration NodeIDB (%d) not initialized', [Calibration.NodeIDB]));
      Calibration.NodeA := OTUNameList[Calibration.NodeIDA];
      Calibration.NodeB := OTUNameList[Calibration.NodeIDB];
    end;
  finally
    if Assigned(nodeIds) then
      nodeIds.Free;
  end;
end;

procedure TTreeList.ResolveCalibrationTimeNames(var times: TCalibrations);
var
  i: Integer = -1;
  ACalib: TCalibrationTime =  nil;
begin
  for i := 0 to Times.Count - 1 do
  begin
    try
      ACalib := Times.GetCalibration(i);
      if ACalib is TGroupCalibrationTime then
        ResolveGroupCalibrationTimeName(TGroupCalibrationTime(ACalib))
      else
        ResolveCalibrationTimeName(ACalib);
    except
      on E: Exception do
        raise Exception.Create('Error parsing calibration #' + IntToStr(i + 1) + ': ' + E.Message);
    end;
  end;
end;

procedure TTreeList.Delete(index: integer);
var
  tree: TTreeData;
begin
  if (index < 0) or (index >= NoOfTrees) then Exit;
  tree := Items[index];
  inherited Remove(tree);
  if Assigned(tree) then
    tree.Free;
end;

procedure TTreeList.RemoveNoFree(index: Integer);
var
  tree: TTreeData;
begin
  if (index < 0) or (index >= NoOfTrees) then Exit;
  tree := Items[index];
  inherited Remove(tree);
end;

procedure TTreeList.DeleteAll;
begin
  if Self <> nil then
  begin
    while NoOfTrees > 0 do
      Delete(Count-1);
  end;
end;

class function TTreeList.CleanOtuNameForExport(aName: AnsiString): AnsiString;
var
  i: integer;
begin
  Result := trim(aName);
  if (Pos('(', Result) > 0) or
     (Pos(')', Result) > 0) or
     (Pos(':', Result) > 0) or
     (Pos(';', Result) > 0) or
     (Pos('''', Result) > 0) then
  begin
    for i := length(Result) downto 1 do
      if Result[i] = '''' then
        system.Insert('''', Result, i);
    Result := '''' + Result + '''';
  end
  else
  begin
    while Pos(' ', Result) > 0 do
      Result[Pos(' ', Result)] := '_';
  end;
end;

function TTreeList.HasIdenticalTree(aTree: TTreeData): Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if Count = 0 then
    Exit;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Identical(aTree) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TTreeList.NumOutgroupMembers(TreeIndex: Integer): Integer;
begin
  Assert(TreeIndex < NoOfTrees);
  Result := 0;
  if TreeIndex < NoOfTrees then
    Result := Items[TreeIndex].NumOutgroupMembers;
end;

procedure TTreeList.Clear;
begin
  DeleteAll;

  FInformation.Clear;
  FOTUName.Clear;
  FValueName  := '';
  FValue2Name := '';
  FFreqName   := '';
  FStatsName  := '';
  FMaxStats   := -1.0;
  FDistanceMatrix := nil;
  FisRooted := false;

  inherited;
end;

procedure TTreeList.ClearOutgroup;
var
  i, j: Integer;
begin
  if NoOfTrees > 0 then
    for i := 0 to  NoOfTrees - 1 do
      for j := 0 to NoOfOTUs - 1 do
        Items[i].IsOutgroupMember[j] := False;
end;

procedure TTreeList.ClearInternalNodeLabels;
var
  i: Integer = -1;
begin
  if Count > 0 then
    for i := 0 to Count - 1 do
      GetItems(i).ClearInternalNodeLabels;
end;

function TTreeList.GetTotalFrequency:double;
var
  i: integer;
begin
  Result := 0.0;
  if NoOfTrees = 0 then Exit;
  for i := 0 to NoOfTrees-1 do
    Result := Result +Items[i].Freq;
end;

procedure TTreeList.SetMaxStats(AValue: Double);
begin
  if FMaxStats = AValue then Exit;
  FMaxStats := AValue;
end;

procedure TTreeList.SetOutputNodeLabels(AValue: Boolean);
begin
  if FNewickExportOptions.NodeLabels <> AValue then
    FNewickExportOptions.NodeLabels := AValue;
end;

function TTreeList.HasGeneDupsOrSpeciationMarkers(NewickStr: String): Boolean;
begin
  Result := ((Pos(GENE_DUPLICATION_STR, NewickStr) > 0) or (Pos(SPECIATION_STR, NewickStr) > 0));
end;

function TTreeList.HasSpeciesNames: Boolean;
begin
  Result := (Assigned(FSpeciesNames)) and (FSpeciesNames.Count > 0);
end;

function TTreeList.HasUnwantedTaxa(WantedTaxa: TStringList): Boolean;
var
  i: Integer;
begin
  Result := False;
  Assert((WantedTaxa.Count > 0) and (FOTUName.Count > 0));
  for i  := 0 to FOtuName.Count - 1 do
  begin
    if WantedTaxa.IndexOf(FOtuName[i]) < 0 then
    begin
      Result := True; { we found a taxon in our list that is not in WantedTaxa}
      break;
    end;
  end;
end;

procedure TTreeList.SetTotalFrequency(value : double);
var
  i: integer;
  r: double;
begin
  if value = TotalFrequency then Exit;
  if NoOfTrees = 0 then Exit;
  r := value/TotalFrequency;
  for i := 0 to NoOfTrees-1 do
    Items[i].Freq := Items[i].Freq*r;
end;



procedure TTreeList.ReadFromFile(var SessionFile: File; SessionFileVersion: integer);
var
  t: TTreeData = nil;
  i: Integer = -1;
  j: Integer = -1;
  ntree: Integer = -1;
  notu: longint = -1;
  flgBLen, flgSE, flgStats: boolean;
  buffer : array[0..8191] of AnsiChar;
  TempStr: AnsiString = '';
begin
  Clear;
  for i := Low(buffer) to High(buffer) do
    buffer[i] := #0;
  BlockRead(SessionFile, i, 4);     // for compatibility
  BlockRead(SessionFile, i, 4);     // for compatibility

  BlockRead(SessionFile, i, 4);
  if i > 0 then begin
      BlockRead(SessionFile, buffer, i);
      buffer[i] := #0;
      FInformation.CommaText := StrPas(buffer);
  end;

  BlockRead(SessionFile, i, 4);     // for compatibility
  BlockRead(SessionFile, i, 4);     // for compatibility
  BlockRead(SessionFile, i, 4);     // for compatibility
  BlockRead(SessionFile, i, 4);     // for compatibility

  BlockRead(SessionFile, i, 4);
  if i > 0 then begin
      BlockRead(SessionFile, buffer, i);
      buffer[i] := #0;
      FFreqName := StrPas(buffer);
  end;
  BlockRead(SessionFile, i, 4);
  if i > 0 then begin
      BlockRead(SessionFile, buffer, i);
      buffer[i] := #0;
      FValueName := StrPas(buffer);
  end;
  BlockRead(SessionFile, i, 4);
  if i > 0 then begin
      BlockRead(SessionFile, buffer, i);
      buffer[i] := #0;
      FValue2Name := StrPas(buffer);
  end;
  BlockRead(SessionFile, i, 4);
  if i > 0 then begin
      BlockRead(SessionFile, buffer, i);
      buffer[i] := #0;
      FStatsName := StrPas(buffer);
  end;

  BlockRead(SessionFile, i, 4);
  flgBLen := false;
  flgSE := false;
  flgStats := false;
  FisRooted := false;
  if (i and 1) = 1 then flgBLen := true;
  if (i and 2) = 2 then flgSE := true;
  if (i and 4) = 4 then flgStats := true;
  if (i and 8) = 8 then FisRooted := true;

  BlockRead(SessionFile, FMaxStats, 8);
  BlockRead(SessionFile, ntree, 4);
  BlockRead(SessionFile, notu, 4);
  for i := 0 to notu-1 do begin
    BlockRead(SessionFile, j, 4);
    BlockRead(SessionFile, buffer, j);
    buffer[j] := #0;
    OTUName[i] := StrPas(buffer);
    TempStr := OtuName[i];
//    while Pos('_', TempStr) > 0 do
//      TempStr[Pos('_', TempStr)] := ' ';
    TrimTaxaName(TempStr);
    OTUName[i] := TempStr;
  end;
  if ntree > 0 then
    for i := 1 to ntree do
    begin
      t := TTreeData.Create(notu, flgBLen, flgSE, flgStats);
      t.ReadFromFile(SessionFile, SessionFileVersion);
      Add(t);
    end;

  if SessionFileVersion >= 604 then
    ReadSpeciesNamesFromSession(SessionFile);
end;

function TTreeList.ReadSpeciesNamesFromSession(var SessionFile: File): Boolean;
var
  i, j: Integer;
  NameCount: Integer = -1;
  NameLength: Integer = -1;
  AChar: AnsiChar = #0;
  AStr: AnsiString = '';
begin
  Result := False;
  try
    BlockRead(SessionFile, NameCount, SizeOf(Integer));
    if NameCount > 0 then
    begin
      if not Assigned(FSpeciesNames) then
        FSpeciesNames := TStringList.Create
      else
        FSpeciesNames.Clear;

      for i := 0 to NameCount - 1 do
      begin
        BlockRead(SessionFile, NameLength, SizeOf(Integer));
        if NameLength > 0 then
        begin
          SetLength(AStr, NameLength);
          for j := 1 to NameLength do
          begin
            BlockRead(SessionFile, AChar, SizeOf(AnsiChar));
            AStr[j] := AChar;
          end;
          FSpeciesNames.Add(AStr);
        end;
      end;
    end;
    Result := True;
  except
    on E: Exception do
    {$IFDEF VISUAL_BUILD}
    ShowMessage('Oh no! An error occurred: ' + E.Message);
    {$ENDIF}
  end;
end;

function TTreeList.GetOutgroupNameValuePairs(TreeIndex: Integer = 0): TOutgroupNameValuePairArray;
var
  i: Integer;
  AName: AnsiString;
begin
  SetLength(Result, NoOfOTUs);
  for i := 0 to NoOfOtus - 1 do
  begin
    AName := GetOTUName(i);
    TrimTaxaName(AName);
    Result[i].TaxonName := AName;
    Result[i].IsOutgroupMember := Items[TreeIndex].IsOutgroupMember[i];
  end;
end;

function TTreeList.GetTrimmedTaxaNames: TStringList;
var
  aName: AnsiString;
  i: Integer;
begin
  Result := TStringList.Create;
  if FOTUName.Count > 0 then
    for i:= 0 to FOTUName.Count - 1 do
    begin
      aName := FOTUName[i];
      TrimTaxaName(AName);
      Result.Add(AName);
    end;
end;

procedure TTreeList.WriteToFile(var data: File);
var i,j: longint;
    buffer : array[0..8191] of AnsiChar;
begin
  for i := Low(buffer) to High(buffer) do
    buffer[i] := #0;
  i := 0;
  BlockWrite(data, i, 4);      // for compatibility
  BlockWrite(data, i, 4);      // for compatibility

  i := Length(FInformation.CommaText);
  BlockWrite(data, i, 4);
  if i > 0 then begin
    StrPCopy(buffer, FInformation.CommaText);
    BlockWrite(data, buffer, i);
  end;

  i := 0;
  BlockWrite(data, i, 4);     // for compatibility
  BlockWrite(data, i, 4);     // for compatibility
  BlockWrite(data, i, 4);     // for compatibility
  BlockWrite(data, i, 4);     // for compatibility

  i := Length(FFreqName);
  BlockWrite(data, i, 4);
  if i > 0 then begin
    StrPCopy(buffer, FFreqName);
    BlockWrite(data, buffer, i);
  end;
  i := Length(FValueName);
  BlockWrite(data, i, 4);
  if i > 0 then begin
    StrPCopy(buffer, FValueName);
    BlockWrite(data, buffer, i);
  end;
  i := Length(FValue2Name);
  BlockWrite(data, i, 4);
  if i > 0 then begin
    StrPCopy(buffer, FValue2Name);
    BlockWrite(data, buffer, i);
  end;
  i := Length(FStatsName);
  BlockWrite(data, i, 4);
  if i > 0 then begin
    StrPCopy(buffer, FStatsName);
    BlockWrite(data, buffer, i);
  end;
  i := 0;
  if isBLen then i := i or 1;
  if isSE then i := i or 2;
  if isStats then i := i or 4;

  if isRooted then i := i or 8;
  BlockWrite(data, i, 4);
  BlockWrite(data, FMaxStats, 8);
  i := NoOfTrees;
  BlockWrite(data, i, 4);
  if NoOfTrees = 0 then
    i := FOTUName.Count
  else
    i := NoOfOTUs;
  BlockWrite(data, i, 4);
  for i := 0 to FOTUName.Count-1 do begin
    j := Length(OTUName[i]);
    StrPCopy(buffer, OTUName[i]);
    BlockWrite(data, j, 4);
    BlockWrite(data, buffer, j);
  end;
  for i := 0 to NoOfTrees-1 do
    Items[i].WriteToFile(data);

   WriteSpeciesNamesToSession(data);
end;

procedure TTreeList.SetInformation(source: TStringList);
begin
  FInformation.Assign(source);
end;

procedure TTreeList.Assign(Source: TTreeList);
var
  i : integer;
  newtree : TTreeData;
begin
  Clear;
  FInformation.Assign(Source.FInformation );
  FOTUName.Assign(Source.FOTUName );
  if Assigned(Source.SpeciesNamesList) then
  begin
    if not Assigned(FSpeciesNames) then
      FSpeciesNames := TStringList.Create;
    FSpeciesNames.AddStrings(Source.SpeciesNamesList);
  end;
  ValueName := Source.ValueName;
  Value2Name := Source.Value2Name;
  FreqName := Source.FreqName;
  StatsName := Source.StatsName;
  isRooted := Source.isRooted;
  isBLen := Source.isBLen;
  isSE := Source.isSE;
  isStats := Source.isStats;
  FMaxStats := Source.FMaxStats;

  TotalFrequency := Source.TotalFrequency;
  for i := 0 to Source.Count-1 do
  begin
    newtree := TTreeData.Create(Source.NoOfOTUs, isBLen,isSE, true);
    newtree.Assign(Source[i]);
    Add(newtree);
  end;
end;

procedure TTreeList.AssignSpeciesNames(AList: TStringList);
begin
  if not Assigned(FSpeciesNames) then
    FSpeciesNames := TStringList.Create;
  FSpeciesNames.Assign(AList);
end;

// This function strips out any matched parenthesis which contain only a single
// descendent so that newick parsing will not fail(PhyML will sometimes write
// newick strings in this way).
function TTreeList.StripExtraneousParens(NewickString: AnsiString): AnsiString;
const
  SingleQuote = #39;
  DoubleQuote = #34;
var
  LeftParen: Integer;
  RightParen: Integer;
  QuoteChar: AnsiChar;

  function InsideQuotedTaxaName(NString: AnsiString): Boolean;
  var
    MyLeft: Integer;
    MyRight: Integer;
    StopChars: Set of AnsiChar;
    InsideQuotes: Boolean;
  begin
    Result := True; // this is the safe case, so if we fail we won't modify the string
    StopChars := [SingleQuote, DoubleQuote];
    MyLeft := LeftParen;
    MyRight := RightParen;

    while (MyLeft > 0) and (not (NString[MyLeft] in StopChars))  do
      dec(MyLeft);
    while (MyRight < Length(NString)) and (not (NString[MyRight] in StopChars)) do
      inc(MyRight);

    // if we found the same quote character to the left and to the right, then we MIGHT be inside of a pair of matched quotes
    if ((NString[MyLeft] = SingleQuote) and (NString[MyRight] = SingleQuote)) or
       ((NString[MyLeft] = DoubleQuote) and (NString[MyRight] = DoubleQuote)) then
    begin
      QuoteChar := NString[MyLeft];
      InsideQuotes := False;
      MyLeft := 1;

      // starting from the beginning of the string, see if our left paren is inside of matched quotes
      while MyLeft < LeftParen do
      begin
        if NString[MyLeft] = QuoteChar then
          InsideQuotes := not InsideQuotes; // keep toggling in/out of quotes until we reach our destination
        inc(MyLeft);
      end;
      Result := InsideQuotes;
    end
    else
      Result := False;
  end;

  function MoveToLeftParen(StartFrom: Integer; Source: AnsiString): Boolean;
  var
    InsideQuotedString: Boolean;
  begin
    InsideQuotedString := False;
    Result := False;
    LeftParen := StartFrom;

    while (LeftParen > 0) and ((Source[LeftParen] <> '(') or (InsideQuotedString)) do
    begin
      dec(LeftParen);
      if (Source[LeftParen] = SingleQuote) or (Source[LeftParen] = DoubleQuote) then
        InsideQuotedString := not InsideQuotedString;
    end;

    if Source[LeftParen] = '(' then
      Result := True;
  end;

  function MoveToNextRightParen(Source: AnsiString): Boolean;
  var
    InsideQuotedString: Boolean;
  begin
    Result := False;
    if RightParen > Length(Source) then
      Exit;
    InsideQuotedString := (Source[RightParen] = SingleQuote) or (Source[RightParen] = DoubleQuote);
    while (RightParen < Length(Source)) and ((Source[RightParen] <> ')') or InsideQuotedString) do
    begin
      inc(RightParen);
      if (Source[RightParen] = SingleQuote) or (Source[RightParen] = DoubleQuote) then
        InsideQuotedString := not InsideQuotedString;
    end;

    if Source[RightParen] = ')' then
      Result := True;
  end;

  function CommaFound(Source: AnsiString; StartFrom: Integer; RunTo: Integer): Boolean;
  var
    InsideQuotedString: Boolean;
    k: Integer;
  begin
    InsideQuotedString := False;
    Result := False;
    k := StartFrom;
    while k <= RunTo do
    begin
      if (Source[k] = SingleQuote) or (Source[k] = DoubleQuote) then
        InsideQuotedString := not InsideQuotedString;

      if (not InsideQuotedString) and (Source[k] = ',') then
      begin
        Result := True;
        Exit;
      end;
      inc(k);
    end;
  end;

begin
  Result := trim(NewickString);
  RightParen := 1;

  try
    while True do
    begin
      if MoveToNextRightParen(Result) then
      begin
        if MoveToLeftParen(RightParen, Result) then
        begin
          if (not CommaFound(Result, LeftParen, RightParen)) and (not InsideQuotedTaxaName(Copy(Result, LeftParen, RightParen - LeftParen))) then
          begin
            System.Delete(Result, RightParen, 1);
            System.Delete(Result, LeftParen, 1);
            dec(RightParen);
          end
          else
            inc(RightParen);
        end
        else
          Exit;
      end
      else
        Exit;
    end;
  Except
    Result := NewickString; // then just return the original string so the parser can do its thing
  end;
end;

function TTreeList.DebugExportToNewickFile(aFile: String; aData: TTreeData; aNames: TStringList): Boolean;
begin
  Clear;
  OTUNameList.Assign(aNames);
  Add(aData);
  Result := ExportToNewickFile(aFile, aData.isBLen, aData.isStats, 0.0);
end;

function TTreeList.ImportFromNewick(NewickTree : AnsiString; NameList: TStringList; ShowProgress: Boolean=True): boolean;
var
  numNodesProcessed: Integer = 0;
  filebuffer, buffer: AnsiString;
  CurTree, i, j: Integer;
  TotalPos, Progress: integer;
  temptree: TTreeData;
  dlg: TPleaseWait;

   function GetOTUindex:integer;
    var
      str: AnsiString;
      k,n: integer;
    begin
      result := -1;
      n := length(buffer);
      if i >= n then
      begin
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           LogErrorMsg('Failure at GetOtuIndex');
          {$ELSE}
           warn_nv('Failure at GetOtuIndex');
          {$ENDIF}
        {$ENDIF}
        exit;
      end;
      k := i;
      if buffer[i] = '''' then { then it is a quoted name}
      begin
        inc(i);
        repeat
          if buffer[i] = '''' then
            if (i < n-1) and (buffer[i+1] = '''') then
              inc(i,2)
            else
              break
          else
            inc(i);
        until i = n;
        inc(i);

        str := Copy(buffer, k+1, i-k-2);

        k := length(str);
        while k > 1 do
        begin
          if str[k] = '''' then
            if (k > 1) and (str[k-1] = '''') then
            begin
              system.Delete(str, k, 1);
              dec(k);
            end;
          dec(k);
        end;
      end
      else { the name is not quoted}
      begin
        repeat
          Inc(i);
        until (buffer[i] = ':') or (buffer[i] = AnsiChar(#9))
           or (buffer[i] = ',') or (buffer[i] = ')')
           or (i = length(buffer));
        str := Copy(buffer, k, i-k);
//        while Pos('_', str) > 0 do
//          str[Pos('_', str)] := ' ';
        TrimTaxaName(str);
      end;

      if NameList = nil then
      begin
        if FOTUName.Count = CurOTU then
        begin
          OTUName[CurOTU] := str;
          Result := CurOTU;
          Inc(CurOTU);
        end
        else
        begin
          for k := 0 to FOTUName.Count-1 do
            if OTUName[k] = str then
            begin
              result := k;
              Break;
            end;
        end
      end
      else
      begin
        for k := 0 to NameList.Count-1 do
          if NameList[k] = str then
          begin
            OTUName[k] := str;
            result := k;
            Break;
          end;
        if Result = -1 then { correct for discrepency when quoted labels are combined with underscores}
        begin
//          while Pos('_', str) > 0 do
//            str[Pos('_', str)] := ' ';
          TrimTaxaName(str);
          for k := 0 to NameList.Count-1 do
            if NameList[k] = str then
            begin
              OTUName[k] := str;
              result := k;
              Break;
            end;
        end;
      end;
    end;

    function SetBranchData(bn: integer):boolean;
    var
      str: AnsiString = '';
      stdDevStr: AnsiString = '';
      statsStr: AnsiString = '';
      j,n,d: integer;
    begin
      // if buffer[i] = #9 that means we have a clustal type bootstrap/stats value.
      // If bufer[i] = ' then we may have a quoted node label'
      result := false;
      if i >= length(buffer) then
      begin
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           LogErrorMsg('Failure at SetBranchData - ran past length of buffer: ' + IntToStr(i));
          {$ELSE}
           warn_nv('Failure at SetBranchData');
          {$ENDIF}
        {$ENDIF}
        exit;
      end;
      try
        n := length(buffer);
        while (i <= n) and (buffer[i] <> ')') and (buffer[i] <> ',') do
          if buffer[i] = '''' then
          begin
            inc(i);
            d := i;
            repeat
              if buffer[i] = '''' then
                if (i < n-1) and (buffer[i+1] = '''') then
                  inc(i,2)
                else
                  break
              else
                inc(i);
            until i = n;
            inc(i);
            str := Copy(buffer, d, i-d-1);
            temptree.InternalNodeLabel[bn-NoOfOTUs] := str;
          end
          else if buffer[i] = ':' then
          begin
            j := i+1;
            repeat
              Inc(i);
            until (buffer[i] = AnsiChar(#9)) or (buffer[i] = ',') or (buffer[i] = ')') or (i = n);
            str := Copy(buffer, j, i-j);
            Items[CurTree].BLenArray[bn] := StrToFloat(str);
          end
          else if buffer[i] = AnsiChar(#9) then
          begin
            j := i+1;
            repeat
              Inc(i);
            until (buffer[i] = ':') or (buffer[i] = ',') or (buffer[i] = ')') or (i = n);
            str := Copy(buffer, j, i-j);
            statsStr := EmptyStr;
            stdDevStr := EmptyStr;
            if Pos('[stddev=', str) > 0 then
            begin
              if not ParseStatsAndStdDevStr(str, statsStr, stdDevStr) then
                raise Exception.Create(Format('failed to parse bootstrap std dev from %s', [str]));
              str := statsStr;
              if not Items[CurTree].IsStatsStdDev then
                Items[CurTree].IsStatsStdDev := True;
              if stdDevStr <> EmptyStr then
                Items[CurTree].StatsStdDev[bn] := StrToFloat(stdDevStr);
            end;

            Items[CurTree].StatsArray[bn] := StrToFloat(str);
            if Pos(FormatSettings.DecimalSeparator, str) > 0 then
              if StrToFloat(str) > MaxStats then
                if StrToFloat(str) < 1 then
                  MaxStats := 1
                else
                  MaxStats := power(10, ceil(log10(StrToFloat(str))));
          end
          else
            Inc(i);
      except
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           LogErrorMsg('Failure at GetOtuIndex');
          {$ELSE}
           warn_nv('Failure at GetOtuIndex');
          {$ENDIF}
        {$ENDIF}
        exit;
      end;
      result := true;
    end;

    function DecodeNodeData:integer;
    var
      n1,n2: integer;
    begin
      result := -1;
      if i = length(buffer) then
      begin
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           LogErrorMsg('Failure at DecodeNodeData');
          {$ELSE}
           warn_nv('Failure at DecodeNodeData');
          {$ENDIF}
        {$ENDIF}
        exit;
      end;
      if buffer[i] = ';' then
      begin
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           LogErrorMsg('Failure at DecodeNodeData');
          {$ELSE}
           warn_nv('Failure at DecodeNodeData');
          {$ENDIF}
        {$ENDIF}
        exit;
      end;
      Inc(i);
      if buffer[i] = '(' then
        n1 := DecodeNodeData
      else
        n1 := GetOTUindex;
      if n1 = -1 then
      begin
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           LogErrorMsg('Failure at DecodeNodeData');
          {$ELSE}
           warn_nv('Failure at DecodeNodeData');
          {$ENDIF}
        {$ENDIF}
        exit;
      end;
      if not SetBranchData(n1) then
      begin
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           LogErrorMsg('Failure at DecodeNodeData');
          {$ELSE}
           warn_nv('Failure at DecodeNodeData');
          {$ENDIF}
        {$ENDIF}
        exit;
      end;
      if i >= length(buffer) then
      begin
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           LogErrorMsg('Failure at DecodeNodeData');
          {$ELSE}
           warn_nv('Failure at DecodeNodeData');
          {$ENDIF}
        {$ENDIF}
        exit;
      end;

      Inc(i);
      if buffer[i] = '(' then
        n2 := DecodeNodeData
      else
        n2 := GetOTUindex;
      if n2 = -1 then
      begin
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           LogErrorMsg('Failure at DecodeNodeData');
          {$ELSE}
           warn_nv('Failure at DecodeNodeData');
          {$ENDIF}
        {$ENDIF}
        exit;
      end;
      if not SetBranchData(n2) then
      begin
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           LogErrorMsg('Failure at DecodeNodeData');
          {$ELSE}
           warn_nv('Failure at DecodeNodeData');
          {$ENDIF}
        {$ENDIF}
        exit;
      end;

      Items[CurTree].NodeArray[CurNode].des1 := n1;
      Items[CurTree].NodeArray[CurNode].des2 := n2;

      Result := CurNode +NoOfOTUs;
      Inc(CurNode);

      isRooted := true;

      while buffer[i] = ',' do
      begin
        result := -1;
        n1 := CurNode +NoOfOTUs -1;

        Inc(i);
        if buffer[i] = '(' then
          n2 := DecodeNodeData
        else
          n2 := GetOTUindex;
        if n2 = -1 then
        begin
          {$IFDEF DEBUG}
            {$IFDEF VISUAL_BUILD}
             LogErrorMsg('Failure at DecodeNodeData');
            {$ELSE}
             warn_nv('Failure at DecodeNodeData');
            {$ENDIF}
          {$ENDIF}
          exit;
        end;
        if not SetBranchData(n2) then
        begin
          {$IFDEF DEBUG}
            {$IFDEF VISUAL_BUILD}
             LogErrorMsg('Failure at DecodeNodeData');
            {$ELSE}
             warn_nv('Failure at DecodeNodeData');
            {$ENDIF}
          {$ENDIF}
          exit;
        end;

        Items[CurTree].NodeArray[CurNode].des1 := n1;
        Items[CurTree].NodeArray[CurNode].des2 := n2;
        Result := CurNode +NoOfOTUs;
        Inc(CurNode);

//        Items[CurTree].StatsArray[n1] := -1.0;   // KT: This makes a problem in RelTime GUI

        isRooted := false;
      end;
      Inc(i);
      Inc(CurPos);

      if (ShowProgress) and (Progress <> CurPos*100 div TotalPos) then
      begin
        Progress := CurPos*100 div TotalPos;
        if dlg.Visible then
          dlg.PercentDone := Progress;
      end;
      inc(numNodesProcessed);
    end;

    function CheckPhylipFormat:boolean;
    var
      i,j: integer;
      flag0,flag1: boolean;
    begin
      Result := false;
      flag0 := true;
      flag1 := true;
      i := 1;
      while i < Length(filebuffer) do
      begin
        if filebuffer[i] = '[' then
          flag0 := false
        else if filebuffer[i] = ']' then
          flag0 := true;
        if flag0 then
          if filebuffer[i] = '''' then
            flag1 := not flag1;
        if flag0 and flag1 and (filebuffer[i] = ')') then
        begin
          j := 1;
          while ((i+j) < length(filebuffer)) and (filebuffer[i+j] <> ',') and (filebuffer[i+j] <> ')') do
          begin
            if filebuffer[i+j] = '[' then
            begin
              Result := true;
              exit;
            end;
            inc(j);
          end;
          i := i+j-1;
        end;
        inc(i);
      end;
    end;

    procedure ConvertPhylipFormat;
    var
      i,j : integer;
      flag0,flag1: boolean;
    begin
      flag0 := true;
      flag1 := true;
      i := 1;
      while i < Length(filebuffer) do
      begin
        if filebuffer[i] = '[' then
          flag0 := false
        else if filebuffer[i] = ']' then
          flag0 := true;
        if flag0 then
          if filebuffer[i] = '''' then
            flag1 := not flag1;
        if flag0 and flag1 and (filebuffer[i] = ')') then
        begin
          j := 1;
          while ((i+j) < length(filebuffer)) and (filebuffer[i+j] <> ',') and (filebuffer[i+j] <> ')') do
            if filebuffer[i+j] = '[' then
            begin
              filebuffer[i+j] := AnsiChar(#9);
              inc(j);
            end
            else if filebuffer[i+j] = ']' then
            begin
              System.Delete(filebuffer, i+j, 1);
              break;
            end
            else
              inc(j);
          i := i+j-1;
        end;
        inc(i);
      end;
    end;

    function CheckClustalFormat:boolean;
    var
      i,j,n: integer;
      flag0,flag1: boolean;
    begin
      result := false;
      n := Length(filebuffer);
      flag0 := true;
      flag1 := true;
      for i := 1 to n-1 do
      begin
        if filebuffer[i] = '[' then
          flag0 := false
        else if filebuffer[i] = ']' then
          flag0 := true;
        if flag0 then
          if filebuffer[i] = '''' then
            flag1 := not flag1;
        if not (flag0 and flag1) then
          continue;
        if (filebuffer[i] = ')') then
          if (Ord(filebuffer[i+1]) >= 48) and (Ord(filebuffer[i+1]) <= 57) then
          begin
            result := true;
            j := i+2;
            while (j < n) and (filebuffer[j] <> ',') and (filebuffer[j] <> ':') and (filebuffer[j] <> ')') and (filebuffer[j] <> ';') do
            begin
              if (Ord(filebuffer[i+1]) < 48) or (Ord(filebuffer[i+1]) > 57) then
              begin
                result := false;
                break;
              end;
              j := j+1;
            end;
            if not result then
              break;
          end
          else if filebuffer[i + 1] <> ':' then
            break;
      end;
    end;

    procedure ConvertClustalFormat;
    var
      i: integer;
      flag0,flag1: boolean;
    begin
      flag0 := true;
      flag1 := true;
      i := 1;
      while i < Length(filebuffer) - 1 do
      begin
        if filebuffer[i] = '[' then
          flag0 := false
        else if filebuffer[i] = ']' then
          flag0 := true;
        if flag0 then
          if filebuffer[i] = '''' then
            flag1 := not flag1;
        if flag0 and flag1 and
           (filebuffer[i]=')') and
           (Ord(filebuffer[i+1]) >= 48) and
           (Ord(filebuffer[i+1]) <= 57) then
          System.Insert(AnsiChar(#9), filebuffer, i+1);
        inc(i);
      end;
    end;

    function CheckPAUPFormat1:boolean;
    var
      i,n: integer;
      flag0,flag1: boolean;
    begin
      result   := false;
      n := Length(filebuffer);
      flag0 := true;
      flag1 := true;
      for i := 1 to n-1 do
      begin
        if filebuffer[i] = '[' then
          flag0 := false
        else if filebuffer[i] = ']' then
          flag0 := true;
        if flag0 then
          if filebuffer[i] = '''' then
            flag1 := not flag1;
        if not (flag0 and flag1) then
          continue;
        if filebuffer[i] = ')' then
        begin
          if filebuffer[i+1] = ':' then
            result := true;
        end
        else if filebuffer[i+1] = ':' then
        begin
          result := false;
          break;
        end;
      end;
    end;

    procedure CheckFormat2;
    var
      i,j: integer;
      s: double;
      flag: boolean;
    begin
      if not isBLen then
        exit;

      s := 0;
      for i := 0 to 2*NoOfOTUs-3 do
        s := s +Items[0].BLen[i];
      if s < 0.0000000000001 then
      begin
        isBLen := false;
        exit;
      end;

      flag := true;
      for i := NoOfOTUs to 2*NoOfOTUs-3 do
        if abs(Items[0].Stats[i]-Items[0].BLen[i]) > 0.0000000000001 then
        begin
          flag := false;
          break;
        end;
      if flag then
      begin
        if Items[0].BLen[0] > 0.00000000001 then
          MaxStats := Items[0].BLen[0];
        isBLen := false;
        exit;
      end;

      flag := true;
      for i := 1 to NoOfOTUs-1 do
      if abs(Items[0].BLen[i]-Items[0].BLen[0]) > 0.00000000000001 then
      begin
        flag := false;
        break;
      end;
      if flag then
      begin
        s := 0;
        for i := 0 to NoOfTrees-1 do
          for j := 0 to 2*NoOfOTUs-3 do
          begin
            if Items[i].Stats[j] > -0.0000000000001 then
              Items[i].Stats[j] := Items[i].BLen[j];
            if s < Items[i].Stats[j] then
              s := Items[i].Stats[j];
          end;

        if Items[0].BLen[0] > 0.00000000001 then
          MaxStats := Items[0].BLen[0]
        else if s < 1.00000000001 then
          MaxStats := 1.0
        else if s < 100.00000000001 then
          MaxStats := 100
        else
          MaxStats := 0;
        StatsName := '';
        isBLen := false;
      end;
    end;

    procedure ConvertPAUPFormat1;
    var
      i,n: integer;
      flag0,flag1: boolean;
    begin
      n := Length(filebuffer);
      flag0 := true;
      flag1 := true;
      for i := 1 to n do
      begin
        if filebuffer[i] = '[' then
          flag0 := false
        else if filebuffer[i] = ']' then
          flag0 := true;
        if flag0 then
          if filebuffer[i] = '''' then
            flag1 := not flag1;
        if flag0 and flag1 then
          if filebuffer[i] = ':' then
            filebuffer[i] := AnsiChar(#9);
      end;
    end;

    procedure ConvertPAUPFormat2;
    var
      i,j: integer;
    begin
      for i := 0 to NoOfTrees-1 do
        for j := 0 to 2*NoOfOTUs-3 do
          if Items[i].Stats[j] > -0.0000000000001 then
            Items[i].Stats[j] := Items[i].BLen[j];
      isBLen := false;
      MaxStats := 0;
      StatsName := 'Unknown';
    end;

    /// <summary>Performs some simple format checks on the given Newick buffer string.
    /// Tests that we have equal numbers of left and right parenthesis as well as the
    /// existence of multiple branches inside of parenthesis.</summary>
    function CheckFormat(buffer: AnsiString): boolean;
    var
      numLeftParens: Integer;
      numRightParens: Integer;
      numCommas: Integer;
      i,n: integer;
      insideComment: Boolean;
    begin
      result := true; // assume that we will be successful
      numLeftParens := 0;
      numRightParens := 0;
      numCommas := 0;  // number of commas (inside of the current pair of parenthesis)
      insideComment := false;
      n := length(buffer);
      i := 1;

      repeat
        if insideComment then
        begin
          if buffer[i] = ']' then
            insideComment := false; // we do not support nested comments
          inc(i);
          continue;
        end
        else if buffer[i] = '[' then
        begin
          insideComment := true;
          inc(i);
          continue;
        end;
        if (not insideComment) and (buffer[i] = '''') then
        begin   // keep going until we are no longer inside of a quoted string
          inc(i);
          repeat
            if buffer[i] = '''' then
              if (i < n-1) and (buffer[i+1] = '''') then
                inc(i,2)
              else
                break
            else
              inc(i);
          until i = n;
          inc(i);
        end;

        if buffer[i] = ';' then // reached the end of a tree
        begin
          if (numLeftParens = 0) then
          begin
            result := false;
            break;
          end;
          if numLeftParens <> numRightParens then
          begin
            result := false;
            break;
          end;
          numLeftParens := 0; // else, reset the counts for left and right parens because we are parsing another tree
          numRightParens := 0;
        end
        else if buffer[i] = '(' then
        begin
          numCommas := 0;
          inc(numLeftParens);
        end
        else if buffer[i] = ')' then
        begin
          if numCommas = 0 then // if there were no commas inside of the current pair of parens (not sure if this is correct, it is ok to just have a branch inside)
          begin
            result := false;
            break;
          end;
          inc(numRightParens);
        end
        else if buffer[i] = ',' then
          inc(numCommas);
        inc(i);
      until i >= n;

      if numLeftParens <> numRightParens then
        result := false;
    end;

    function CheckCurTree(tree:TTreeData): boolean;
    var
      names: array of integer;
      i,n1,n2: integer;
    begin
      setlength(names, tree.NoOfOTUs);

      for i := 0 to tree.NoOfOTUs-1 do
        names[i] := 0;
      for i := 0 to tree.NoOfOTUs-2 do
      begin
        n1 := tree[i].des1;
        n2 := tree[i].des2;
        if n1 < tree.NoOfOTUs then
          Inc(names[n1]);
        if n2 < tree.NoOfOTUs then
          Inc(names[n2]);
      end;
      result := true;
      for i := 0 to tree.NoOfOTUs-1 do
        result := result and (names[i] = 1);

      setlength(names, 0);
    end;

var
  str: AnsiString =  '';
  d,n, temp: integer;
  hasBranchLengths: Boolean = False;
  insideQuotedString: Boolean = False;
  debug: Integer = 0;
  hasStatsStdDev: Boolean = False;
begin
  Result := false;  // assume that parsing will fail
  if NewickTree = EmptyStr then
    Raise Exception.Create('The newick tree file you specified was empty.  Please check that your tree file is valid.');
  if Pos('[stddev=', NewickTree) > 0 then
    hasStatsStdDev := True;
  CurPos := 0;
  Progress := 0;
  dlg := nil;
  while Pos(#10, NewickTree) > 0 do  // Remove linebreaks as they cause a crash.
    System.Delete(NewickTree, Pos(#10, NewickTree), 1);
  while Pos(#13, NewickTree) > 0 do
    System.Delete(NewickTree, Pos(#13, NewickTree), 1);
  filebuffer := StripExtraneousParens(NewickTree);

  TotalPos := 0;
  for i := 1 to length(filebuffer) do   // Count total number of '(' in tree
    if filebuffer[i] = '(' then
      inc(TotalPos);

  if Pos(';', filebuffer) = 0 then
  begin
    {$IFDEF VISUAL_BUILD}
    if (MessageBox(0, 'While reading in your newick tree file we noticed it did not terminate with a semicolon as the newick standard requires.'+LineEnding+'This could indicate a corrupted file, would you like to continue?', 'Semicolon Missing', MB_ICONWARNING or MB_YESNO or MB_DEFBUTTON1) = idYes) then
    begin
      filebuffer := filebuffer + ';'; // just fix it for them so the parser doesn't freak.
      NewickTree := NewickTree + ';';
    end
    else
    begin
      result := false;
      exit;
    end;
    {$ELSE}
    warn_nv('While reading in your newick tree file we noticed it did not terminate with a semicolon as the newick standard requires.  We will attempt to continue reguardless. If this causes an error please fix the file by appending the semicolon.');
    {$ENDIF}
  end;
  Clear;
  if ShowProgress then
    dlg := TPleaseWait.Create(nil);

  try
    try
      while Pos(AnsiChar(#9), filebuffer) > 0 do  { remove all tab characters}
        System.Delete(filebuffer, Pos(AnsiChar(#9), filebuffer), 1);

        // Note: '''' = a string consisting of a single quote.
        i := length(filebuffer);
        repeat // This block removes extraneous spaces (probably trying to remove all white space), while leaving spaces in quoted names.
          if filebuffer[i] = '''' then
          begin
            dec(i);
            repeat
              if filebuffer[i] = '''' then
                if (i > 3) and (filebuffer[i-1] = '''') then
                  dec(i,2)
                else
                  break
              else
                dec(i);
            until i = 2;
          end
          else if filebuffer[i] = ' ' then
            System.Delete(filebuffer, i, 1);

          dec(i);
        until i = 1;
      if not CheckFormat(filebuffer) then
      begin
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           LogErrorMsg('Failure at CheckFormat');
          {$ELSE}
           warn_nv('Failure at CheckFormat');
          {$ENDIF}
        {$ENDIF}
        exit;
      end;

        MaxStats := 0;
        if (not HasGeneDupsOrSpeciationMarkers(filebuffer)) and (not hasStatsStdDev) then
        begin
          if CheckPAUPFormat1 then
          begin
            ConvertPAUPFormat1;
            StatsName := 'Unknown';
          end
          else if CheckPhylipFormat then
          begin
            ConvertPhylipFormat;
            StatsName := 'Unknown';
          end
          else if CheckClustalFormat then
          begin
            ConvertClustalFormat;
            StatsName := 'Unknown';
          end;
        end;

        if hasStatsStdDev then
          if CheckClustalFormat then
          begin
            ConvertClustalFormat;
            StatsName := 'Unknown';
          end;

        if not hasStatsStdDev then
        begin
          while Pos('[', filebuffer) > 0 do // remove comments from the buffer string
          begin
            i := Pos('[', filebuffer);
            j := Pos(']', filebuffer)-i+1;
            System.Delete(filebuffer, i, j);
          end;

          if (Pos('[', filebuffer) > 0) or (Pos(']', filebuffer) > 0) then // the case where we have unmatched square brackets
          begin
            {$IFDEF DEBUG}
              {$IFDEF VISUAL_BUILD}
               LogErrorMsg('Failure at DecodeNodeData');
              {$ELSE}
               warn_nv('Failure at DecodeNodeData');
              {$ENDIF}
            {$ENDIF}
            exit;
          end;
        end;

        // What is this section of code doing?  It appears to be removing node labels!!

       { insideQuotedString := false;
        for i := Length(filebuffer)-2 downto 1 do
        begin
          if filebuffer[i] = '''' then
          begin
            insideQuotedString := not insideQuotedString;
            continue;
          end;

          if insideQuotedString then continue;

          if (filebuffer[i] = ')') then
            if (filebuffer[i+1] <> ',') and
               (filebuffer[i+1] <> ':') and
               (filebuffer[i+1] <> chr(9)) and // tab characters are used to mark bootstrap/branch info values
               (filebuffer[i+1] <> ')') and

               (filebuffer[i+1] <> ';') then
            begin   // strip out any internal node labels
              j := 2;
              while (filebuffer[i+j] <> ',') and
                    (filebuffer[i+j] <> ':') and
                    (filebuffer[i+j] <> ')') and
                    (filebuffer[i+j] <> ';') and
                    (i+j < Length(filebuffer)) do
                j := j+1;

              System.Delete(filebuffer, i+1, j-1);
            end;
        end;      }

        n := 0;  // another check for ';' in the file
        for i := 1 to length(filebuffer) do
          if filebuffer[i] = ';' then Inc(n);
      if n = 0 then
      begin
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           ShowMessage('Failure to find semicolon');
          {$ELSE}
           warn_nv('Failure to find semicolon');
          {$ENDIF}
        {$ENDIF}
        exit;
      end;
      if Pos(':', filebuffer) > 0 then
        hasBranchLengths := true
      else
        hasBranchLengths := false;

      if MaxStats < 0.00000000001 then
        if (Pos(AnsiChar(#9), filebuffer) > 0) or hasStatsStdDev then
          MaxStats := 0
        else
          MaxStats := -1.0;

      buffer := Copy(filebuffer, 1, Pos(';', filebuffer));

      n := 0;
      insideQuotedString := false;
      for i := 1 to Length(buffer)-1 do
      begin
        if filebuffer[i] = '''' then
        begin
          insideQuotedString := not insideQuotedString;
          continue;
        end;

        if insideQuotedString then continue;

        if (buffer[i] = '(') or (buffer[i] = ',') then
          if buffer[i+1] <> '(' then
            Inc(n);
      end;

      if n <= 1 then
      begin
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           ShowMessage('Failure to parse newick tree');
          {$ELSE}
           warn_nv('Failure to parse newick tree');
          {$ENDIF}
        {$ENDIF}
        exit;
      end;

      CurTree := 0;
      while Pos(';', filebuffer) > 0 do
      begin
        buffer := Copy(filebuffer, 1, Pos(';', filebuffer));
        if Pos('(', buffer) > 1 then
          System.Delete(buffer, 1, Pos('(', buffer)-1); // delete anything before the first '('

        CurOTU := 0;
        CurNode := 0;

        temptree := TTreeData.Create(n, hasBranchLengths, false, true);
        temptree.Freq := 1.0;
        Add(temptree);

        i := 1;
        temp := DecodeNodeData;
        if temp = -1 then
        begin
          Clear;
          Result := false;
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           LogErrorMsg('Failure at DecodeNodeData');
          {$ELSE}
           warn_nv('Failure at DecodeNodeData');
          {$ENDIF}
        {$ENDIF}
          exit;
        end
        else if not CheckCurTree(temptree) then
        begin
          Clear;
          Result := false;
          {$IFDEF DEBUG}
            {$IFDEF VISUAL_BUILD}
             LogErrorMsg('Failure at DecodeNodeData');
            {$ELSE}
             warn_nv('Failure at DecodeNodeData');
            {$ENDIF}
          {$ENDIF}
             exit;
        end;

        // Handle any node label at the end (root) node.
        if buffer[i] = '''' then
        begin
          inc(i);
          d := i;
          repeat
            if buffer[i] = '''' then
              if (i < n-1) and (buffer[i+1] = '''') then
                inc(i,2)
              else
                break
            else
              inc(i);
          until i = n;
          inc(i);
          str := Copy(buffer, d, i-d-1);
          //SetInternalNodeLbl(temp - NoOfOTUs, str);
          tempTree.InternalNodeLabel[2*(NoOfOtus - 1) - NoOfOTUs - 1] := str;
        end;

        System.Delete(filebuffer, 1, Pos(';', filebuffer));
        Inc(CurTree);
      end;

      if i < Length(buffer) then
      begin
        // check if there is anything other than what could be a branch length
        for debug := i to Length(buffer) do
          if not (buffer[debug] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', '.', ';']) then
        raise Exception.Create(Format('Bad Newick string. Processing terminated early at character %.0n of %.0n in the Newick string', [(i + 1)*1.0, Length(buffer)*1.0]));
      end;

      if CurTree = 0 then
        Result := false
      else
      begin
        CheckFormat2;
        if CompareValue(MaxStats, 0, FP_CUTOFF) > 0 then
          StatsName := 'Branch Support';
        Result := true;
      end;
    except
      on E:Exception do
      begin
        {$IFDEF DEBUG}
          {$IFDEF VISUAL_BUILD}
           LogErrorMsg('Failure at ImportFromNewick: ' + E.Message);
          {$ELSE}
           warn_nv('Failure at ImportFromNewick');
          {$ENDIF}
        {$ENDIF}
        Clear;
        Result := false;
      end;
    end;
  finally
    if Assigned(dlg) then
      dlg.Free;
  end;
end;
  {
function TTreeList.ImportFromTable(Table : TStringList): boolean;
var
  i, j: integer;
  temptree: TTreeData;
  dlg: TPleaseWait;

  curCol: String;
  columns: TStringList;
  ancIndex, des1Index, des2Index, bl1Index, bl2Index: Integer;
  ancID: Integer;
  des1, des2: String;
  bl1, bl2: double;

  //ancIDArray: array of integer;
  desNameList: TStringList;
  nodeDataArray: TArrayOfNodeData;
  tempInt: Integer;

  procedure processLine(input: String; var ancID: integer; var des1: String; var des2: string; var bl1: double; var bl2: double);
  var
    inputList : TStringList;
  begin
    inputList := TStringList.Create;
    try
      inputList.StrictDelimiter := true;
      inputList.Delimiter := ',';
      inputList.DelimitedText := input;

      // Verify valid length
      if inputList.Count < 5 then
      begin
        raise Exception.Create('invalid string = ' + input);
      end;

      // Extract AncID
      if not TryStrToInt(trim(inputList.Strings[ancIndex]), ancID) then
        raise Exception.Create('Ancestor ID is not an integer.');

      // Extract Des1
      if trim(inputList.Strings[des1Index]) = '' then
        raise Exception.Create('Missing Desc1');
      des1 := trim(inputList.Strings[des1Index]);

      // Extract Des2
      if trim(inputList.Strings[des2Index]) = '' then
        raise Exception.Create('Missing Desc2');
      des2 := trim(inputList.Strings[des2Index]);

      // Extract Branch Length 1
      if not TryStrToFloat(trim(inputList.Strings[bl1Index]), bl1) then
        raise Exception.Create('Branch length 1 is not a valid number');

      // Extract Branch Length 2
      if not TryStrToFloat(trim(inputList.Strings[bl2Index]), bl2) then
        raise Exception.Create('Branch length 2 is not a valid number');

    finally
      inputList.Free;
    end;
  end;

  function ancIDExists(ancID: integer): boolean;
  var
    i: Integer;
  begin
    result := false;
    for i := 0 to length(nodeDataArray)-1 do
        if ancID = nodeDataArray[i] then
        begin
          result := true;
          break;
        end;
  end;

begin
  Result := false;  // assume that parsing will fail

  try
    if table.Count = 0 then
    begin
      Raise Exception.Create('The table file you specified is empty.');
    end;

  // Clean out any empty lines
    i := 0;
    while i < table.count do
    begin
      if Trim(table.strings[i]) = '' then
        table.Delete(i)
      else
        inc(i);

    end;

    // Read in the first line to determine that all the columns exist
    columns := TStringList.Create;
    columns.StrictDelimiter := true;
    columns.Delimiter := ',';
    columns.DelimitedText := table.Strings[0];

    // Identify column indexes.
    for i := 0 to columns.Count-1 do
    begin
      curCol := trim(columns.Strings[i]);

      if compareText(curCol, 'AncId') = 0  then
        ancIndex := i
      else
        if compareText(curCol, 'Desc1') = 0 then
          des1Index := i
        else
          if compareText(curCol, 'Desc2') = 0 then
            des2Index := i
          else
            if compareText(curCol, 'Branch Length 1') = 0 then
              bl1Index := i
            else
              if compareText(curCol, 'Branch Length 2') = 0 then
                bl2Index := i
              else
                begin
                  Raise Exception.Create('Invalid column name = ' + curCol);

                end;
    end;

    setLength(nodeDataArray, 0);
    desNameList := TStringList.Create;
    // Process each line of input into data we can use
    for i := 1 to table.count-1 do
    begin
      setLength(nodeDataArray, length(nodeDataArray)+1);
      processLine(table.Strings[i], ancID, des1, des2, bl1, bl2);
      //ANC - Check that ancID doesn't already exist.
      //TODO: NOT SURE IF THIS IS ACTUALLY VALID, CAN YOU HAVE MULTIPLE ANCESTOR ENTRIES?!!
      if ancIDExists(ancID) then
          raise Exception.Create('Duplicate ancestor ID = ' + IntToStr(ancID))
      else
      begin
        nodeDataArray[length(nodeDataArray)-1] := ancID;
      end;

      // DES 1 - Check if des1 is a number (referring to internal node IDs)
      if not TryStrToInt(des1, tempInt) then
      begin
        // Check that des1 or des2 names don't already exist (and des1 <> des2)
        if desNameList.IndexOf(des1) <> -1 then
          raise Exception.Create('Duplicate leaf taxon name detected = ' + des1);
        desNameList.Add(des1);
      end
      else
      begin
        // Verify that they are valid descendants. (must be a previously listed ancestor ID)
        if not ancIDExists(tempInt) then
          raise Exception.Create('Invalid decendant (should be listed as an ancestor before referenced). = ' + des1);
        nodeDataArray[length(nodeDataArray)-1].des1 := tempInt;
      end;

      // DES 2 - Check if des1 is a number (referring to internal node IDs)
      if not TryStrToInt(des2, tempInt) then
      begin
        // Check that des1 or des2 names don't already exist (and des1 <> des2)
        if desNameList.IndexOf(des2) <> -1 then
          raise Exception.Create('Duplicate leaf taxon name detected = ' + des2);
        desNameList.Add(des2);
      end
      else
      begin
        // Verify that they are valid descendants. (must be a previously listed ancestor ID)
        if not ancIDExists(tempInt) then
          raise Exception.Create('Invalid decendant (should be listed as an ancestor before referenced). = ' + des2);
        nodeDataArray[length(nodeDataArray)-1].des1 := tempInt;
      end;

      if des1 = des2 then
        raise Exception.Create('Both decendants are the same.  des1 = ' + des1 + '; des2 = ' + des2 + ';');
      
      
    end;


    temptree := TTreeData.Create(length(ancIdArray) + desNameList.Count, true, false, true);
    temptree.Freq := 1.0;
    Add(temptree);

    

  except
    Result := false;
  end;

end;

function  TTreeList.ImportFromTableFile(filename : AnsiString; NameList: TStringList):boolean;
var
  fileStringList: TStringList;
begin
  Result := false;
  fileStringList := TStringList.Create;
  try
    fileStringList.LoadFromFile(filename);
    Result := ImportFromTable(fileStringList);
  finally
    fileStringList.Free;
    fileStringList := nil;
  end;
end;
    }

function  TTreeList.ImportFromNewickFile(filename : String; NameList: TStringList; ShowProgress: Boolean=True):boolean;
var
  data: TextFile;
  filebuffer, buffer: AnsiString;
begin
  Result := false;
  try
    filebuffer := '';
    if FileExists(filename) then
    begin
      AssignFile(data, filename);
      Reset(data);

      while not Eof(data) do
      begin
        Readln(data, buffer);
        filebuffer := filebuffer +buffer;
      end;
      Result := ImportFromNewick(filebuffer, NameList, ShowProgress);
    end;
  finally
    if FileExists(filename) then
      CloseFile(data);
  end;
end;

function TTreeList.OutputNewickTree(TreeIndex: integer; UseBranchLengths, UseStats:boolean; CutOff: double): AnsiString;
var
  WriteBranchLengths: Boolean;
  WriteStats: Boolean;
  TreeData: TTreeData;

  function BranchLenString(blen: Extended): String;
  var
    mantissa: Double = 0;
  begin
    mantissa := Frac(blen);
    if CompareValue(mantissa, 0, FP_CUTOFF) = 0 then
      Result := ':' + FloatToStrF(blen, ffFixed, 8, 1) { this avoids converting some branch lengths to Integer types which breaks some Newick parsers}
    else
      Result := ':' + FloatToStrF(blen, ffGeneral, 15, 15);
  end;

  procedure ExportNode(nodeindex: integer);
  var
    KeepNode: boolean;
  begin
    KeepNode := TreeData.Stats[TreeData[nodeindex].des1]+0.000000000001 >= CutOff;
    if TreeData[nodeindex].des1 < NoOfOTUs then
      Result := Result + CleanOtuNameForExport(OTUName[TreeData[nodeindex].des1])
    else if KeepNode then
    begin
      Result := Result +'(';
      ExportNode(TreeData[nodeindex].des1-NoOfOTUs);
      if FNewickExportOptions.NodeLabels and (Trim(InternalNodeLbls[TreeIndex][TreeData[nodeindex].des1-NoOfOTUs]) <> EmptyStr) then
      begin
        if FNewickExportOptions.UseQuotesForLabels then
          Result := Result +')' + #39 + InternalNodeLbls[TreeIndex][TreeData[nodeindex].des1-NoOfOTUs] + #39
        else
          Result := Result + ')' + GetInternalNodeLblNoQuotes(TreeIndex, TreeData[nodeindex].des1 - NoOfOTUs);
      end
      else
        Result := Result + ')';
      if TreeData[nodeIndex].des1 >= NoOfOtus then
        if FNewickExportOptions.GeneDuplications and (TreeData.IsGeneDupEvent[TreeData[nodeindex].des1] = True) then
          Result := Result + GENE_DUPLICATION_STR
        else if FNewickExportOptions.SpeciationEvents and (TreeData.IsSpeciationEvent[TreeData[nodeindex].des1] = True) then
          Result := Result + SPECIATION_STR;
    end
    else
      ExportNode(TreeData[nodeindex].des1 - NoOfOTUs);

    if KeepNode then
    begin
      if WriteStats and (TreeData[nodeindex].des1 >= NoOfOTUs) then
        if abs(MaxStats) < 0.000000000001 then
          result := result +IntToStr(Trunc(TreeData.Stats[TreeData[nodeindex].des1]+0.0000000000001))
        else
          result := result + FloatToStrF(TreeData.Stats[TreeData[nodeindex].des1]/MaxStats,ffFixed,8,4);
      if (TreeData[nodeindex].des1 >= NoOfOTUs) and FNewickExportOptions.BootstrapStdDev and TreeData.IsStatsStdDev then
        Result := Result + Format('[stddev=%.8f]', [TreeData.StatsStdDev[TreeData[nodeindex].des1]]);
      if WriteBranchLengths then
        result := result + BranchLenString(TreeData.BLen[TreeData[nodeindex].des1]);
    end;

    result := result + ',';

    KeepNode := TreeData.Stats[TreeData[nodeindex].des2]+0.000000000001 >= CutOff;
    if TreeData[nodeindex].des2 < NoOfOTUs then
      result := result + CleanOtuNameForExport(OTUName[TreeData[nodeindex].des2])
    else if KeepNode then
    begin
      result := result +'(';
      ExportNode(TreeData[nodeindex].des2-NoOfOTUs);
      if FNewickExportOptions.NodeLabels and (Trim(InternalNodeLbls[TreeIndex][TreeData[nodeindex].des2-NoOfOTUs]) <> EmptyStr) then
      begin
        if FNewickExportOptions.UseQuotesForLabels then
          result := result +')' + #39 + InternalNodeLbls[TreeIndex][TreeData[nodeindex].des2 - NoOfOTUs] + #39
        else
          Result := Result + ')' + GetInternalNodeLblNoQuotes(TreeIndex, TreeData[nodeindex].des2 - NoOfOTUs);
      end
      else
        result := result + ')';
      if TreeData[nodeindex].des2 >= NoOfOTUs then
        if FNewickExportOptions.GeneDuplications and (TreeData.IsGeneDupEvent[TreeData[nodeindex].des2] = True) then
          result := result + GENE_DUPLICATION_STR
        else if FNewickExportOptions.SpeciationEvents and (TreeData.IsSpeciationEvent[TreeData[nodeindex].des2] = True) then
          result := result + SPECIATION_STR;
    end
    else
      ExportNode(TreeData[nodeindex].des2-NoOfOTUs);

    if KeepNode then
    begin
      if WriteStats and (TreeData[nodeindex].des2 >= NoOfOTUs) then
        if abs(MaxStats) < 0.000000000001 then
          result := result +IntToStr(Trunc(TreeData.Stats[TreeData[nodeindex].des2]+0.0000000000001))
        else
          result := result + FloatToStrF(TreeData.Stats[TreeData[nodeindex].des2]/MaxStats,ffFixed,8,4);
      if (TreeData[nodeindex].des2 >= NoOfOTUs) and FNewickExportOptions.BootstrapStdDev and TreeData.IsStatsStdDev then
        Result := Result + Format('[stddev=%.8f]', [TreeData.StatsStdDev[TreeData[nodeindex].des2]]);
      if WriteBranchLengths then
        result := result + BranchLenString(TreeData.BLen[TreeData[nodeindex].des2]);
    end;
  end;

var
  root, i, j: integer;
  flag: boolean;
begin
  result := '';
  TreeData := Items[TreeIndex];
  if not TreeData.HasInternalNodeLbls then
    FNewickExportOptions.NodeLabels := False;

  {$IFNDEF VISUAL_BUILD}
  if D_MegaMain.AddBclStdDevToNewick then
    FNewickExportOptions.BootstrapStdDev := True;
  {$ENDIF}
  if not TreeData.IsStatsStdDev then
    FNewickExportOptions.BootstrapStdDev := False;

  {$IFDEF DEBUG}
  if TreeData.TreeName <> EmptyStr then
    Result := Format('[%s]', [TreeData.TreeName]);
  {$ENDIF}
  try
    root := -1;
    WriteBranchLengths := (UseBranchLengths and isBLen and (TreeData.SBL > 0.0000000000001));
    WriteStats := (UseStats and IsStats);

    for i := 2*NoOfOTUs-2 downto NoOfOTUs do
    begin
      flag := true;
      for j := NoOfOTUs-2 downto 0 do
      begin
        if TreeData[j].des1 = i then
        begin
          flag := false;
          Break;
        end;
        if TreeData[j].des2 = i then
        begin
          flag := false;
          Break;
        end;
      end;
      if flag then
      begin
        root := i-NoOfOTUs;
        Break;
      end;
    end;

    Result := Result +'(';
    if IsRooted then
      ExportNode(root)
    else if TreeData[root].des1 < NoOfOTUs then { root.des1 is a leaf node}
    begin
      Result := Result + CleanOtuNameForExport(OTUName[TreeData[root].des1]);
      if WriteBranchLengths then
        Result := Result + BranchLenString(TreeData.BLen[TreeData[root].des1] + TreeData.BLen[TreeData[root].des2]);
      Result := Result + ',';

      ExportNode(TreeData[root].des2-NoOfOTUs);
    end
    else if TreeData[root].des2 < NoOfOTUs then { root.des2 is a leaf node}
    begin
      ExportNode(TreeData[root].des1-NoOfOTUs);
      result := result + ',' + CleanOtuNameForExport(OTUName[TreeData[root].des2]);
      if WriteBranchLengths then
        Result := Result + BranchLenString(TreeData.BLen[TreeData[root].des1] + TreeData.BLen[TreeData[root].des2]);
    end
    else { neither des of root node is a leaf node}
    begin
      ExportNode(TreeData[root].des1-NoOfOTUs);

      if TreeData.Stats[TreeData[root].des2]+0.000000000001 >= CutOff then
      begin
        Result := Result + ',(';
        ExportNode(TreeData[root].des2-NoOfOTUs);
        Result := Result +')';
        if FNewickExportOptions.GeneDuplications and (TreeData.IsGeneDupEvent[TreeData[Root].des2] = True) then
          Result := Result + GENE_DUPLICATION_STR
        else if FNewickExportOptions.SpeciationEvents and (TreeData.IsSpeciationEvent[TreeData[Root].des2] = True) then
          Result := Result + SPECIATION_STR;
        if WriteStats then
          if abs(MaxStats) < 0.000000000001 then
            Result := Result + IntToStr(Trunc(TreeData.Stats[TreeData[root].des2]+0.000000000001))
          else
            Result := Result + FloatToStrF(TreeData.Stats[TreeData[root].des2]/MaxStats,ffFixed,8,4);
        if FNewickExportOptions.BootstrapStdDev and TreeData.IsStatsStdDev then
          Result := Result + Format('[stddev=%f]', [TreeData.StatsStdDev[root]]);
        if WriteBranchLengths then
          Result := Result + BranchLenString(TreeData.BLen[TreeData[root].des1] + TreeData.BLen[TreeData[root].des2]);
      end
      else
        ExportNode(TreeData[root].des2-NoOfOTUs);
    end;
    Result := Result +')'; { closing parenthesis for the root node}

    if FNewickExportOptions.GeneDuplications and (TreeData.IsGeneDupEvent[Root + NoOfOtus] = True) then
      Result := Result + GENE_DUPLICATION_STR
    else if FNewickExportOptions.SpeciationEvents and (TreeData.IsSpeciationEvent[Root + NoOfOtus] = True) then
      Result := Result + SPECIATION_STR;
    Result := Result + ';';
  except
    Result := '';
    exit;
  end;
end;

function TTreeList.OutputAllNewickTrees(Options: TNewickExportOptions; Cutoff: Double; valuesToInclude: TTreeDataValueSet = []): TStringList;
var
  i: Integer;
  valStr: String = '';
begin
  Result := TStringList.Create;
  if NoOfTrees > 0 then
    for i := 0 to NoOfTrees - 1 do
    begin
      valStr := '';
      if (tdvValue in valuesToInclude) or (tdvTreeLength in valuesToInclude) then
        valStr += Format(' %.8f', [Items[i].Value]);
      if (tdvValue2 in valuesToInclude) or (tdvLogLikelihood in valuesToInclude) then
        valStr += Format(' %.8f', [Items[i].Value2]);
      if (tdvValue3 in valuesToInclude) or (tdvInitialLogLikelihood in valuesToInclude) then
        valStr += Format(' %.8f', [Items[i].Value3]);
      if valStr <> EmptyStr then
        valStr := '[' + valStr + ']';
      Result.Add(Trim(valStr) + OutputNewickTree(i, Options, Cutoff));
    end;
end;

function TTreeList.OutputNewickTree(index: integer; Options: TNewickExportOptions; CutOff: double): AnsiString;
begin
  FNewickExportOptions := Options;
  Result := OutputNewickTree(Index, Options.BranchLengths, Options.BootstrapVals, Cutoff);
end;

function TTreeList.OutputTable(index: integer; bl,st:boolean; CutOff: double): AnsiString;
var
  bflag: boolean;

  procedure ExportNode(nodeindex: integer);
  var
    flag: boolean;
  begin
    flag := Items[index].Stats[Items[index][nodeindex].des1]+0.000000000001 >= CutOff;
    if Items[index][nodeindex].des1 < NoOfOTUs then
      result := result + CleanOtuNameForExport(OTUName[Items[index][nodeindex].des1])
    else if flag then
    begin
      result := result +'(';
      ExportNode(Items[index][nodeindex].des1-NoOfOTUs);
      result := result +')';
    end
    else
      ExportNode(Items[index][nodeindex].des1-NoOfOTUs);
    if flag then
    begin
      if st and isStats and (Items[index][nodeindex].des1 >= NoOfOTUs) then
        if abs(MaxStats) < 0.000000000001 then
          result := result +IntToStr(Trunc(Items[index].Stats[Items[index][nodeindex].des1]+0.0000000000001))
        else
          result := result +FloatToStrF(Items[index].Stats[Items[index][nodeindex].des1]/MaxStats,ffFixed,8,4);  // Tamura change Jan / 15
      if bl and isBLen and bflag then
        result := result +':'+FloatToStrF(Items[index].BLen[Items[index][nodeindex].des1],ffFixed,15,8);
    end;

    result := result +',';

    flag := Items[index].Stats[Items[index][nodeindex].des2]+0.000000000001 >= CutOff;
    if Items[index][nodeindex].des2 < NoOfOTUs then
      result := result + CleanOtuNameForExport(OTUName[Items[index][nodeindex].des2])
    else if flag then
    begin
      result := result +'(';
      ExportNode(Items[index][nodeindex].des2-NoOfOTUs);
      result := result +')';
    end
    else
      ExportNode(Items[index][nodeindex].des2-NoOfOTUs);
    if flag then
    begin
      if st and isStats and (Items[index][nodeindex].des2 >= NoOfOTUs) then
        if abs(MaxStats) < 0.000000000001 then
          result := result +IntToStr(Trunc(Items[index].Stats[Items[index][nodeindex].des2]+0.0000000000001))
        else
          result := result +FloatToStrF(Items[index].Stats[Items[index][nodeindex].des2]/MaxStats,ffFixed,8,4);  // Tamura change Jan / 15
      if isBLen and bflag then
        result := result +':'+FloatToStrF(Items[index].BLen[Items[index][nodeindex].des2],ffFixed,15,8);
    end;
  end;

var root, i, j: integer;
    flag: boolean;
begin
  result := '';
  try
    root := -1;
    bflag := bl and (Items[index].SBL > 0.0000000000001);

    for i := 2*NoOfOTUs-2 downto NoOfOTUs do
    begin
      flag := true;
      for j := NoOfOTUs-2 downto 0 do
      begin
        if Items[index][j].des1 = i then
        begin
          flag := false;
          Break;
        end;
        if Items[index][j].des2 = i then
        begin
          flag := false;
          Break;
        end;
      end;
      if flag then
      begin
        root := i-NoOfOTUs;
        Break;
      end;
    end;
    //result := result +'(';
    if isRooted then
      ExportNode(root)
    else if Items[index][root].des1 < NoOfOTUs then
    begin
      result := result + CleanOtuNameForExport(OTUName[Items[index][root].des1]);
      if isBLen and bflag then
        result := result +':'+FloatToStrF(Items[index].BLen[Items[index][root].des1]+Items[index].BLen[Items[index][root].des2],ffFixed,15,8);
      result := result +',';

      ExportNode(Items[index][root].des2-NoOfOTUs);
    end
    else if Items[index][root].des2 < NoOfOTUs then
    begin
      ExportNode(Items[index][root].des1-NoOfOTUs);
      result := result +','+ CleanOtuNameForExport(OTUName[Items[index][root].des2]);
      if isBLen and bflag then
        result := result +':'+FloatToStrF(Items[index].BLen[Items[index][root].des1]+Items[index].BLen[Items[index][root].des2],ffFixed,15,8);
    end
    else
    begin
      ExportNode(Items[index][root].des1-NoOfOTUs);

      if Items[index].Stats[Items[index][root].des2]+0.000000000001 >= CutOff then
      begin
        result := result +',(';
        ExportNode(Items[index][root].des2-NoOfOTUs);
        result := result +')';
        if st and isStats  then
          if abs(MaxStats) < 0.000000000001 then
            result := result +IntToStr(Trunc(Items[index].Stats[Items[index][root].des2]+0.000000000001))
          else
            result := result +FloatToStrF(Items[index].Stats[Items[index][root].des2]/MaxStats,ffFixed,8,4);  // Tamura change Jan / 15
        if isBLen and bflag then
          result := result +':'+FloatToStrF(Items[index].BLen[Items[index][root].des1]+Items[index].BLen[Items[index][root].des2],ffFixed,15,8);
      end
      else
        ExportNode(Items[index][root].des2-NoOfOTUs);
    end;
    result := result +');';
  except
    result := '';
    exit;
  end;
end;


function TTreeList.WriteATreeToNewickFile(index: integer; var f: TextFile; bl,st:boolean; CutOff: double): boolean;
var
  buffer: AnsiString;
begin
  buffer := OutputNewickTree(index, bl, st, CutOff);
  result := buffer <> '';
  Writeln(f,buffer);
end;


function TTreeList.WriteSpeciesNamesToSession(var SessionFile: File): Boolean;
var
  i, j, NameLength: Integer;
  AChar: AnsiChar;
begin
  Result := False;
  try
    if Assigned(FSpeciesNames) then
    begin
      i := FSpeciesNames.Count;
      BlockWrite(SessionFile, i, SizeOf(Integer));
      if i > 0 then
      for i := 0 to FSpeciesNames.Count - 1 do
      begin
        NameLength := Length(FSpeciesNames[i]);
        BlockWrite(SessionFile, NameLength, SizeOf(Integer));
        if NameLength > 0 then
          for j := 1 to NameLength do
          begin
            AChar := AnsiString(FSpeciesNames[i])[j];
            BlockWrite(SessionFile, AChar, SizeOf(AnsiChar));
          end;
      end;
    end
    else
    begin
      i := 0;
      BlockWrite(SessionFile, i, SizeOf(Integer));
    end;
    Result := True;
  except
    on E: Exception do
    {$IFDEF VISUAL_BUILD}
    ShowMessage('Oh no! An error occurred: ' + E.Message);
    {$ENDIF}
  end;
end;

function TTreeList.ExportATreeToNewickFile(index: integer; filename :String; bl,st:boolean; CutOff: double; comment: String = ''): boolean;
var
  f : TextFile;
begin
  try
    try
      AssignFile(f, filename);
      ReWrite(f);
      if comment <> EmptyStr then
        Write(f, comment);
      result := WriteATreeToNewickFile(index, f, bl, st, CutOff);
    except
      result := false;
      exit;
    end;
  finally
    CloseFile(f);
  end;
end;

function TTreeList.DebugOutputNewickTree(aData: TTreeData; aNames: TStringList): String;
begin
  Clear;
  OTUNameList.Assign(aNames);
  Add(aData);
  Result := OutputNewickTree(0, aData.isBLen, aData.isStats, 0.0);
  Remove(Count - 1);
end;

function TTreeList.ExportToNewickFile(filename : String; bl,st:boolean; CutOff: double; MaxNumTrees: Integer):boolean;
var
  f : TextFile;
  SBLFile: TextFile;
  SBLFileName: AnsiString;
  SBL: double;
  i: integer;
  NumTreesToPrint: Integer;
begin
  try try
    AssignFile(f, filename);
    ReWrite(f);

    if IsPhyloQAnalysis then // in this case we are performing least squares user tree analysis for PhyloQ metagenomic simulations
    begin
      if Pos('.nwk', filename) > 0 then
        SBLFileName := AnsiReplaceStr(filename, '.nwk', '.txt')
      else
        SBLFileName := filename + '.txt';
      AssignFile(SBLFile, SBLFileName);
      ReWrite(SBLFile);
    end;
    if IsPhyloQAnalysis then
    begin
      if (MaxNumTrees < NoOfTrees) and (MaxNumTrees > 0) then
        NumTreesToPrint := MaxNumTrees
      else
        NumTreesToPrint := NoOfTrees;
    end
    else
      NumTreesToPrint := NoOfTrees;

    for i := 0 to NumTreesToPrint - 1 do
    begin
      if IsPhyloQAnalysis then
      begin
        SBL := getItems(i).SBL;
        WriteLn(SBLFile, FloatToStrF(SBL, ffNumber, 20, 12));
        {$IFNDEF VISUAL_BUILD}
        ShowProgressIncrementStatic;
        {$ENDIF}
      end;
      result := WriteATreeToNewickFile(i, f, bl, st, CutOff);
      if not result then
        break;
    end;
    if IsPhyloQAnalysis then
      {$IFNDEF VISUAL_BUILD}
      ShowProgressIncrementStatic;
      {$ENDIF}
    result := true;
  except
    result := false;
    exit;
  end;
  finally
    CloseFile(f);
    if IsPhyloQAnalysis then
      CloseFile(SBLFile);
  end;
end;

function TTreeList.ExportToNewickFile(filename: String; bl, st: Boolean; Cutoff: Double; valuesToInclude: TTreeDataValueSet): Boolean;
var
  options: TNewickExportOptions;
  aList: TStringList = nil;
begin
  try
    options := GetDefaultNewickExportOptions(Self);
    options.BranchLengths := bl;
    options.BootstrapVals := st;
    aList := OutputAllNewickTrees(options, Cutoff, valuesToInclude);
    aList.SaveToFile(filename);
    Result := FileExists(filename);
  finally
    if Assigned(aList) then
      aList := nil;
  end;
end;

function TTreeList.FinalizeCalibration(var Calibration: TCalibrationTime): Boolean;
begin
  Result := False;
  if Calibration.CalibrationTarget = ctInternalNode then
    ResolveCalibrationTimeName(Calibration)
  else
  begin
    if Calibration.NodeID < 0 then
    begin
      Assert(Calibration.NodeA = Calibration.NodeB, 'Invalid sample time calibration data');
      Calibration.NodeID := OTUNameList.IndexOf(Calibration.NodeA);
      if Calibration.NodeID < 0 then
      begin
        Calibration.IsValid := False;
        Calibration.IsSelected := False;
      end;
    end;
    Calibration.NeedsMRCAResolved := False;
  end;
  Result := True;
end;

function TTreeList.FindMrca(TaxonAName: AnsiString; TaxonBName: AnsiString
  ): Integer;
var
  TaxonAId, TaxonBId: Integer;
begin
  TaxonAId := NodeNameToId(TaxonAName);
  TaxonBId := NodeNameToId(TaxonBName);
  Result := Items[0].MRCA(TaxonAId, TaxonBId);
end;

function TTreeList.FindRootIndex(TreeIndex: Integer): Integer;
var
  TreeData: TTreeData;
begin
  TreeData := Items[TreeIndex];
  Result := TreeData.RootIndex;
end;

function TTreeList.FindTwoExtantTaxaIDs(const aNodeId: Integer;
  var taxonA: Integer; var taxonB: Integer): Boolean;
begin
  Result := Items[0].FindTwoExtantTaxa(aNodeId, taxonA, taxonB);
end;

function TTreeList.ParseStatsAndStdDevStr(const input: AnsiString; var statsStr: AnsiString; var stdDevStr: AnsiString): Boolean;
var
  tempFloat: Double;
begin
  Result := False;
  statsStr := Copy(input, 1, Pos('[stddev=', input) - 1);
  stdDevStr := Copy(input, Pos('=', input) + 1, Pos(']', input) - Pos('=', input) - 1);
  if not TryStrToFloat(statsStr, tempFloat) then
    Exit;
  if not TryStrToFloat(stdDevStr, TempFloat) then
    Exit;
  Result := True;
end;

procedure TTreeList.SetOtuNames(aNames: TStringList);
var
  i: Integer;
begin
  Assert((NoOfOTUs = 0) or (aNames.Count = NoOfOTUs), Format('expected %d taxa names but found %d', [NoofOTUs, aNames.Count]));
  if FOTUName.Count = 0 then
    FOTUName.AddStrings(aNames)
  else
    for i := 0 to aNames.Count - 1 do
      FOTUName[i] := aNames[i];
end;

function TTreeList.ToStringList(isBriefSummary: Boolean = False): TStringList;
var
  i: Integer;
  j: Integer;
  TempStringList: TStringList;
begin
  Result := TStringList.Create;
  TempStringList := nil;

  Result.Add(#9 + 'Info');
  Result.Add('[isRooted]=' + BoolToStr(isRooted, True));
  Result.Add('[isBLen]=' + BoolToStr(isBLen, True));
  Result.Add('[isSE]=' + BoolToStr(isSE, True));
  Result.Add('[isStats]=' + BoolToStr(isStats, True));
  Result.Add('[NoOfTrees]=' + IntToStr(NoOfTrees));
  Result.Add('[NoOfOTUs]=' + IntToStr(NoOfOTUs));
  Result.Add('[ValueName]=' + ValueName);
  Result.Add('[Value2Name]=' + Value2Name);
  Result.Add('[FreqName]=' + FreqName);
  Result.Add('[StatsName]=' + StatsName);
  Result.Add('[MaxStats]=' + FloatToStrF(MaxStats, ffFixed, 5, 5));
  Result.Add('[TotalFrequency]=' + FloatToStrF(TotalFrequency, ffFixed, 5, 5));

  Result.Add(' ');
  Result.Add(#9 + 'Trees');
  for i := 0 to NoOfTrees - 1 do
  begin
    TempStringList := TTreeData(Items[i]).StateToStringList(isBriefSummary, '');
    if isBriefSummary then
      Result.Add(Format('TreeData-%d: %s', [i + 1, Trim(TempStringList.Text)]))
    else
    begin
      Result.Add('[TreeData:[=' + IntToStr(i) + ']');
      for j := 0 to TempStringList.Count - 1 do
        Result.Add(TempStringList[j]);
    end;
  end;

  Result.Add(' ');
  Result.Add(#9 + 'OTUNames');
  for i := 0 to FOTUName.Count - 1 do
    Result.Add(IntToStr(i) + ' = ' + FOTUName[i]);

  if Information.Count > 0 then
  begin
    Result.Add(' ');
    Result.Add(#9 + 'Information');
    for i := 0 to Information.Count - 1 do
      Result.Add('[Information_' + IntToStr(i) + ']=' + Information[i]);
  end;

  if TempStringList <> nil then
    FreeAndNil(TempStringList);
end;


end.

