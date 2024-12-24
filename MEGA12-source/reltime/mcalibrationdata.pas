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

unit MCalibrationData;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType , FileUtil, Classes, SysUtils, MegaConsts,
  mcalibrationdensity, MSimpleTreeNode, StrHashMap;

const
  NUM_DEFAULT_CALIBRATION_SAMPLING_REPS = 10000;

type
{ Description is given below at TCalibrationDlg.GetInternalNodeLabelFunc}
  TGetInternalNodeLabelFunc = function(Index: Integer): AnsiString of object;

  TCalibrationTarget = (ctInternalNode, ctLeafNode);

  { TCalibrationTime }

  TCalibrationTime = Class(TObject)
    private
      FOrigMinTime: Extended;
      FOrigMaxTime: Extended;
      FNode: TSimpleTreeNode;
      FTreeViewFormNodeId: Integer;
      function GetMaxTime: Double;
      function GetMinTime: Double;
      procedure SetCalibrationCategory(AValue: TCalibrationCategory);
      procedure SetMaxTime(AValue: Double);
      procedure SetMinTime(AValue: Double);
      procedure SetMRCAName(AValue: String); virtual;
      procedure SetNode(AValue: TSimpleTreeNode);
      procedure SetNodeID(AValue: Integer);
      procedure SetTreeViewFormNodeId(AValue: Integer);
    protected
      FNodeID: Integer;
      FNodeName: String;
      FSamplingMinTimes: TDivTimesArray;
      FSamplingMaxTimes: TDivTimesArray;
      FSamplingDivTimes: TDivTimesArray;
      FCalibrationCategory: TCalibrationCategory;
      FMinTime: Double;
      FMaxTime: Double;
      FDensity: TCalibrationDensity;
      FMRCAName: String; // when the user has specified two taxa and we find the MRCA
      FCalibrationName: String; // user defined name if assigned, else we will assign a default
      FReltimeTreeIndexBLens: Integer;
      FCalibrationTarget: TCalibrationTarget; // for the case where we don't use an alignment. this fixes a bug with mapping calibrations correctly
      procedure SetNodeName(AValue: String); virtual;
      function GetNodeName: String; virtual;
      function GetCalibrationName: String;
      function GetNodeLabel: String; virtual; // the user may have labelled a node via the nodeName command or via the mrca command so we figure it out
      function GetTreeViewFormIndex: Integer;
      function GetRelTimeTreeIndex: Integer; virtual;
      function MaxSessionString: Integer;
      function GetMrcaName: String;
      procedure SetCalibrationName(const Value: String); virtual;
      procedure SetCalibrationTarget(const Value: TCalibrationTarget);
      function AsCalibrationString: String;
      function TaxaListString: String; virtual;
      function AsSampleTimeString: String;

    public
      Index: Integer; // my index in the array; used for making a unique name if the user has not assigned one

      NodeIDA: Integer; // TTreeData ID for taxon A
      NodeIDB: Integer;
      NodeA: String; // the name for taxon A
      NodeB: String;
      NeedsMRCAResolved: Boolean; // when the user has manually created a calibration point from the TCalibrationDlg, they have given two taxa names and we need to find the MRCA node
      NumTaxa: Integer;

      IsSamplingTime: Boolean;
      IsValid: Boolean;
      IsSelected: Boolean;
      constructor Create(IsSampleTime: Boolean = False);
      destructor Destroy; override;
      procedure SetCalibrationDensity(aDensity: TCalibrationDensity);
      function GetCalibrationDensity: TCalibrationDensity;
      procedure RemoveCalibrationDensity;
      function AsString: String; // formatted according to our spec for calibration data input files
      function AsShortString: String; // formatted for human readability
      function NeedsDescendentsResolved: Boolean; // when the user has identified the calibrated node using a node label in the newick string, we need to find a couple of descendents for whom the calibrated node is the MRCA
      procedure Assign(Other: TCalibrationTime);
      procedure SetNodeIdFromTreeViewFormId(TreeViewNodeId: Integer);
      procedure SetTaxonAFromTreeViewFormId(TreeViewNodeId: Integer);
      procedure SetTaxonBFromTreeViewFormId(TreeViewNodeId: Integer);
      function WriteToFile(var SessionFile: File): Boolean;
      function LoadFromFile(const mtsVersion: Integer; var SessionFile: File): Boolean;
      function GetSamplingMinTime(repNum: Integer): Extended;
      function GetSamplingMaxTime(repNum: Integer): Extended;
      function GetSamplingDivTime(repNum: Integer): Extended;
      procedure SetSamplingDivTime(repNum: Integer; divTime: Extended);
      function SamplingDataToList: TStringList;
      function SamplingDataToFile(filename: String): Boolean;
      function SamplingHistogramDataToFile(filename: String): Boolean;
      procedure DoCalibrationDensitySample(repNum: Integer);
      procedure FinalizeCalibrationDensity;
      function IsStatisticalDensity: Boolean;
      function DebugString: String;
      class function DebugHeaderString: String; static;
      property TreeViewFormNodeId: Integer read FTreeViewFormNodeId write SetTreeViewFormNodeId;
      property MrcaName: String read GetMrcaName write SetMRCAName;
      property CalibrationName: String read GetCalibrationName write SetCalibrationName; // a user-defined name for this calibration
      property ReltimeTreeIndex: Integer read GetRelTimeTreeIndex;
      property ReltimeTreeIndexBLens: Integer read FReltimeTreeIndexBLens write FRelTimeTreeIndexBLens;
      property TreeViewFormIndex: Integer read GetTreeViewFormIndex;
      property NodeLabel: String read GetNodeLabel;
      property CalibrationTarget: TCalibrationTarget read FCalibrationTarget write SetCalibrationTarget;
      property CalibrationCategory: TCalibrationCategory read FCalibrationCategory write SetCalibrationCategory;
      property MinTime: Double read GetMinTime write SetMinTime;
      property MaxTime: Double read GetMaxTime write SetMaxTime;
      property Node: TSimpleTreeNode read FNode write SetNode;
      property NodeName: String read GetNodeName write SetNodeName;
      property NodeID: Integer read FNodeID write SetNodeID;
  end;

  { TGroupCalibrationTime }

  TGroupCalibrationTime = class(TCalibrationTime)
    private
      FNameLookupInitialized: Boolean;
      FNameLookup: TStringHashMap;
      function GetTaxaCount: Integer;
      procedure InitNameLookup(aNames: TStringList);
    protected
      FTaxaNames: TStringList;
      function TaxaListString: String; override;
      function NodeNameToId(aName: String): Integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure FindMrcaId(aData: TObject; otuNames: TStringList);
      procedure AddTaxaFromCalibrationString(aCalibrationString: String);
      property TaxaNames: TStringList read FTaxaNames;
      property TaxaCount: Integer read GetTaxaCount;
  end;

  { TIngroupRootCalibration }

  TIngroupRootCalibration = class(TCalibrationTime)
    protected
      function GetNodeLabel: String; override;
      procedure SetNodeName(AValue: String); override;
      procedure SetCalibrationName(const Value: String); override;
      procedure SetMRCAName(AValue: String); override;
      function GetRelTimeTreeIndex: Integer; override;
  end;

  { TRateCalibration }

  TRateCalibration = class(TCalibrationTime)
  private
    FEvolutionaryRate: Extended;
    procedure SetEvolutionaryRate(AValue: Extended);
    public
      constructor Create;
      property EvolutionaryRate: Extended read FEvolutionaryRate write SetEvolutionaryRate;
  end;


  TFinalizeCalibrationFunc = function(var Calibration: TCalibrationTime): Boolean of object;
  TArrayOfCalibrationTime = array of TCalibrationTime;
  PArrayOfCalibrationTime = ^TCalibrationTime;
  T2DArrayOfCalibrationtime = array of TArrayOfCalibrationTime;


  { TCalibrationReader }

  TCalibrationReader = class(TObject)
    protected
      FNumTaxa: Integer;
      function ReadCalibrationStrings(CalibrationStrings: TStringList): TArrayOfCalibrationTime;
    public
      InFile: String;
      Message: String;
      constructor Create(NumTaxa: Integer);
      destructor Destroy; override;
      function ReadCalibrationFile: TArrayOfCalibrationTime;
      function ReadCalibrationsFileToList(Filename: String; var Calibrations: TList): Boolean;
      function ReadCalibrationsStringListToList(aList: TStringList; var Calibrations: TList): Boolean;
  end;



  { TCalibrations }

  TCalibrations = class(TObject)
    private
      FCalibrationsModified: Boolean;
      function GetIsBlensOnly: Boolean;
      procedure SetIsBLensOnly(AValue: Boolean);
    protected
      FIsBlensOnly: Boolean;
      FReader: TCalibrationReader;
      FValidator: TObject;
      FCalibrations: TList;
      FAnalysisInfo: TObject;
      FTreeList: TObject;
      FValidationDone: Boolean;
      FCalibrationsType: TCalibrationTarget;
      FFastNodeIdLookupList: TList;
      function NumTaxa: Integer;
      function NumNodes: Integer;

      procedure MarkInvalidConstraints(Invalids: TArrayOfInteger; var AMsg: String);
      procedure MarkInvalidSampleTimes(Invalids: TArrayOfInteger; var AMsg: String);
      procedure SetCalibrationsType(const Value: TCalibrationTarget);
      function CheckCalibrationsType(var aMsg: String): Boolean;
      procedure DoProgressUpdate(aProgress: Integer; aStatus: String);
    public
      ProgressUpdateProc: TProgressProc;
      ValidationMessages: TValidationMessages;
      constructor Create;
      destructor Destroy; override;
      function NewCalibrationsHaveDuplicates(var Msg: String; var NewCalibrations: TCalibrations): Boolean;
      procedure UnselectNewDuplicates(var newCalibrations: TCalibrations);
      procedure UnselectCalibrationsForNode(aNodeId: Integer);
      procedure InitFastNodeIdLookupList(maxNodeId: Integer);
      procedure ClearFastNodeIdLookupList;
      procedure UnSelectInvalidCalibrations;
      function GetExpandedCalibrationsList: TStringList;
      procedure GetCounts(var numSelected: Integer; var numValid: Integer; var numInvalid: Integer);
      function NumValidCalibrations: Integer;
      function NumSelectedCalibrations: Integer;
      function NumInvalidCalibrations: Integer;
      function NumInvalidCalibrationsThatAreSelected: Integer;
      function DoAllValidations(var Msg: String; const IgnoreUnselected: Boolean; otuNames: TStringList): Boolean;
      function Prevalidate(var Msg: String): Boolean; { validates all but min max times, for the case where the user is not done calibrating yet}
      function ValidateAndFilterOutgroupConstraint(var Msg: String; IsSampleTime: Boolean; otuNames: TStringList): Boolean; overload; { marks invalid constraints which reference nodes in the outgroup cluster}
      function ValidateAndFilterOutgroupConstraint(var Msg: String; AData: TObject; otuNames: TStringList): Boolean; overload;
      function ValidateAndFilterSampleTimesOutgroupConstraint(var Msg: String; AData: TObject): Boolean;
      function ValidateMinMaxTimes(var Msg: String): Boolean; overload; { validates that no max time is less than a min time}
      function ValidateMinMaxTimes(var Msg: String; AData: TObject): Boolean; overload;
      function ValidateCalibrations(var Msg: String; otuNames: TStringList): Boolean; { ensures we have at least one min and one max}
      function ValidateAllAreSampleTimes(var Msg: String): Boolean;
      function ValidateAllAreInternalNodes(var Msg: String): Boolean;
      function ValidateAllSampleTimesProvided(var Msg: String): Boolean;
      function LoadFromList(aList: TStringList): Boolean;
      function LoadFromFile(Filename: String): Boolean;
      {$IFNDEF VISUAL_BUILD}
      procedure FilterMissingTaxa(otuNames: TStringList);
      {$ENDIF}
      function SaveToFile(Filename: String): Boolean;
      procedure SaveToList(var AList: TStringlist);
      procedure SaveSelectedToList(var AList: TStringList);
      {$IFDEF DEBUG}
      function DebugStringsToFile(filename: String): Boolean;
      procedure DebugWriteToDevConsole;
      {$ENDIF}
      function NumOtusInTree: Integer;
      function GetStringList: TStringList;
      function GetMinTimesArray: TDivTimesArray;
      function GetMaxTimesArray: TDivTimesArray;
      procedure SetInfo(AnalysisInfo: TObject);
      procedure AssignTreeList(AList: TObject);
      function GetTreeList: TObject;
      procedure Assign(Source: TCalibrations);
      function GetCalibration(Index: Integer): TCalibrationTime;
      function Add(ACalibration: TCalibrationTime; overwriteExisting: Boolean = False): Integer;
      procedure Insert(ACalibration: TCalibrationTime; Index: Integer);
      procedure Delete(Index: Integer);
      procedure Clear;
      procedure RemoveInvalids;
      procedure RemoveUnselected;
      procedure RemoveDuplicates;
      function CountAndUnSelectDuplicates: Integer;
      function NumDuplicates: Integer;
      function NumSelectedDuplicates(var aMsg: String): Integer;
      function Count: Integer;
      function CalibrationIndicesForTE(includeUnselected: Boolean = False): ArrayOfInteger;
      function WriteToSessionFile(var SessionFile: File): Boolean;
      function ReadFromSessionFile(var SessionFile: File; SessionVersion: LongInt; NumCalibs: Integer): Boolean;
      function GetSelectedCalibrationsArray: TArrayOfCalibrationTime;
      function IsSampleTimes: Boolean;
      function GetUsesCalibrationDensities: Boolean;
      function GetLatestSamplingTime: Extended;
      function HasCalibrationForNode(aNodeId: Integer): Boolean;
      function GetCalibrationByNodeId(aNodeId: Integer): TCalibrationTime;
      function GetDisplayData(const TreeViewIndex: Integer; var minTime: Double; var maxTime: Double; var isSelected: Boolean): Boolean;
      procedure PrepareSamplingTimeArray(var SamplingTimes: TDivTimesArray; const NamesList: TStringList); overload;
      procedure PrepareSamplingTimeArray(var SamplingTimes: TDivTimesArray; var TempCalibs: TArrayOfCalibrationTime; const NamesList: TStringList); overload;
      procedure PrepareDivTimeArrays(var MinTimes, MaxTimes: TDivTimesArray; NumTaxa: Integer; IsBLensOnly: Boolean; var aNodeLabelList: TStringList; indexNeedsChecked: Boolean = True); overload;
      procedure PrepareDivTimeArrays(var MinTimes, MaxTimes: TDivTimesArray; NumTaxa: Integer; IsBLensOnly: Boolean; ingroupRootIndex: Integer; indexNeedsChecked: Boolean = True); overload;
      procedure PrepareSamplingDivTimeArrays(var minTimes, maxTimes: TDivTimesArray; NumTaxa: Integer; IsBLensOnly: Boolean; repNum: Integer);
      procedure GetCalibrationTypes(var HasDivTimes: Boolean; var HasSampleTimes: Boolean);
      property ValidationDone: Boolean read FValidationDone;
      property CalibrationsType: TCalibrationTarget read FCalibrationsType write SetCalibrationsType;
      property IsBLensOnly: Boolean read GetIsBlensOnly write SetIsBLensOnly; { the case where we are only using a user tree with blens (no sequence data). Adds a correction for mapping calibrations to nodes}
      property CalibrationsModified: Boolean read FCalibrationsModified;
  end;


implementation

uses
  {$IFDEF DEBUG}
  mdeveloper_console,
  {$ENDIF}
  {$IFNDEF VISUAL_BUILD}MegaUtils_NV, {$ENDIF}
  StringUtils, MegaUtils, MTreeData, MTimetreeValidation, MTreeList, MLongintList,
  MAnalysisInfo, Math, mhistogram, mtipdatesfinder, mstringbuilder, MTreeDataAdapter;

{ TGroupCalibrationTime }

function TGroupCalibrationTime.GetTaxaCount: Integer;
begin
  Result := FTaxaNames.Count;
end;

procedure TGroupCalibrationTime.InitNameLookup(aNames: TStringList);
var
  i: Integer = -1;
  sc: TStringCount = nil;
begin
  if aNames.Count > 0 then
  begin
    if Assigned(FNameLookup) then
    begin
      FreeAndNil(FNameLookup);
      FNameLookup := TStringHashMap.Create;
    end;

    for i := 0 to aNames.Count - 1 do
    begin
      sc := TStringCount.Create(aNames[i], i);
      FNameLookup.Add(aNames[i], sc);
    end;
    FNameLookupInitialized := True;
  end;
end;

function TGroupCalibrationTime.TaxaListString: String;
var
  i: Integer;
  builder: TMegaStringBuilder = nil;
begin
  try
    builder := TMegaStringBuilder.Create;
    builder.Add(' Taxa=');
    builder.Add(#34);
    if FTaxaNames.Count > 0 then
      for i := 0 to FTaxaNames.Count - 1 do
      begin
        builder.add(#39 + FTaxaNames[i] + #39);
        if i < (FTaxaNames.Count - 1) then
          builder.Add(',');
      end;
    builder.Add(#34);
    Result := builder.GenerateString;
  finally
    if Assigned(builder) then
      builder.Free;
  end;
end;

function TGroupCalibrationTime.NodeNameToId(aName: String): Integer;
var
  p: Pointer = nil;
begin
  Result := -1;
  if FNameLookup.Find(aName, p) then
    Result := TStringCount(p).Index;
end;

constructor TGroupCalibrationTime.Create;
begin
  inherited Create(False);
  FTaxaNames := TStringList.Create;
  FNameLookup := TStringHashMap.Create;
  FNameLookupInitialized := False;
end;

destructor TGroupCalibrationTime.Destroy;
begin
  if Assigned(FTaxaNames) then
    FTaxaNames.Free;
  if Assigned(FNameLookup) then
    FNameLookup.Free;
  inherited Destroy;
end;

procedure TGroupCalibrationTime.FindMrcaId(aData: TObject; otuNames: TStringList);
var
  i: Integer = -1;
  adapter: TSimpleTreeDataAdapter = nil;
  nodeIds: TLongintList = nil;
  aTree: TTreeData = nil;
  aId: Integer = -1;
begin
  try
    aTree := TTreeData(aData);
    adapter := TSimpleTreeDataAdapter.Create;
    adapter.SetTreeData(aTree, False, otuNames);
    nodeIds := TLongintList.Create;
    InitNameLookup(otuNames);
    for i := 0 to TaxaCount - 1 do
    begin
      aId := NodeNameToID(FTaxaNames[i]);
      if aId >= 0 then
        nodeIds.Add(aId)
      else
      begin
        {$IFNDEF VISUAL_BUILD}
        warn_NV(Format('ignoring taxon (%s) not found in the input data', [FTaxaNames[i]]));
        {$ENDIF}
      end;
    end;
    NodeID := adapter.GetGroupMrca(nodeIds) - aTree.NoOfOTUs;
    Assert(NodeID >= 0, Format('invalid node ID for calibration taxa list', [NodeID]));
  finally
    if Assigned(adapter) then
      adapter.Free;
    if Assigned(nodeIds) then
      nodeIds.Free;
  end;
end;

procedure TGroupCalibrationTime.AddTaxaFromCalibrationString(aCalibrationString: String);
var
  tokens: TStringList = nil;
  temp: String = '';
  i: Integer = -1;
  charIndex: Integer = -1;
begin
  try
    tokens := TStringList.Create;
    charIndex := Pos('Taxa=', aCalibrationString) + Length('Taxa="');
    temp := Copy(aCalibrationString, charIndex, Length(aCalibrationString) - charIndex + 1);
    if Pos(#34, temp) = 1 then
      temp := Copy(temp, 2, length(temp));
    charIndex := Pos(#34, temp);
    temp := Copy(temp, 1, charIndex);
    temp := StringReplace(temp, #34, EmptyStr, [rfReplaceAll]);
    if not SplitOnSingleCharFaster(temp, ',', tokens, False) then
      raise Exception.Create(Format('failed to split calibration string by comma separator: %s', [aCalibrationString]));
    for i := 0 to tokens.Count - 1 do
    begin
      temp := tokens[i];
      TrimTaxaName2(temp);
      FTaxaNames.Add(temp);
    end;
  finally
    if Assigned(tokens) then
      tokens.Free;
  end;
end;

{ TIngroupRootCalibration }

function TIngroupRootCalibration.GetNodeLabel: String;
begin
  Result := INGROUP_ROOT;
end;

procedure TIngroupRootCalibration.SetNodeName(AValue: String);
begin
  FNodeName := INGROUP_ROOT;
end;

procedure TIngroupRootCalibration.SetCalibrationName(const Value: String);
begin
  if Pos('ingroup-root', LowerCase(Value)) > 0 then
    FCalibrationName := Value
  else
    FCalibrationName := INGROUP_ROOT + '-' + Value;
end;

procedure TIngroupRootCalibration.SetMRCAName(AValue: String);
begin
  if Pos(INGROUP_ROOT, LowerCase(AValue)) > 0 then
    FMRCAName := AValue
  else
    FMRCAName := INGROUP_ROOT + '-' + AValue;
end;

function TIngroupRootCalibration.GetRelTimeTreeIndex: Integer;
begin
  Result := FNodeID;
end;

{ TRateCalibration }

procedure TRateCalibration.SetEvolutionaryRate(AValue: Extended);
begin
  if FEvolutionaryRate=AValue then Exit;
  FEvolutionaryRate:=AValue;
end;

constructor TRateCalibration.Create;
begin
  inherited Create;
  FCalibrationCategory := ccFixedRate;
end;


function TCalibrations.NumNodes: Integer;
begin
  Result := NumTaxa * 2 - 1;
end;

function TCalibrations.GetIsBlensOnly: Boolean;
begin
  Result := FIsBlensOnly;
end;

procedure TCalibrations.SetIsBLensOnly(AValue: Boolean);
begin
  FIsBlensOnly := AValue;
end;

function TCalibrations.NumTaxa: Integer;
begin
  //assert(Assigned(FAnalysisInfo));
  //assert(FAnalysisInfo.ClassNameIs('TAnalysisInfo'));
  //Result := TAnalysisInfo(FAnalysisInfo).NoOfTaxa;
  Result := NumOtusInTree;
end;

procedure TCalibrations.MarkInvalidConstraints(Invalids: TArrayOfInteger; var AMsg: String);
var
  i, j: Integer;
begin
  AMsg += ' The following divergence time constraints were disregarded because they are invalid:' + LineEnding;
  if ((Length(Invalids) > 0) and (FCalibrations.Count > 0)) then
  begin
    for i := 0 to Length(Invalids) - 1 do
    begin
      for j := 0 to FCalibrations.Count - 1 do
      begin
        if IsBLensOnly then
        begin
          if TCalibrationTime(FCalibrations[j]).Index = Invalids[i] then
          begin
            TCalibrationTime(FCalibrations[j]).IsValid := False;
            TCalibrationTime(FCalibrations[j]).IsSelected := False;
            AMsg := AMsg + TCalibrationTime(FCalibrations[j]).AsShortString + LineEnding;
            break;
          end;
        end
        else
        begin
          if TCalibrationTime(FCalibrations[j]).ReltimeTreeIndex = Invalids[i] then
          begin
            TCalibrationTime(FCalibrations[j]).IsValid := False;
            TCalibrationTime(FCalibrations[j]).IsSelected := False;
            AMsg := AMsg + TCalibrationTime(FCalibrations[j]).AsShortString + LineEnding;
            break;
          end;
        end;
      end;
    end;
  end;
  AMsg := Trim(AMsg);
end;


procedure TCalibrations.MarkInvalidSampleTimes(Invalids: TArrayOfInteger; var AMsg: String);
var
  i, j: Integer;
  aCalib: TCalibrationTime;
begin
  AMsg += ' The following divergence time constraints were disregarded because they are invalid:' + LineEnding;
  if ((Length(Invalids) > 0) and (FCalibrations.Count > 0)) then
  begin
    for i := 0 to Length(Invalids) - 1 do
    begin
      for j := 0 to FCalibrations.Count - 1 do
      begin
        aCalib := TCalibrationTime(FCalibrations[j]);
        if aCalib.Index = Invalids[i] then
        begin
          aCalib.IsValid := False;
          aCalib.IsSelected := False;
          AMsg := AMsg + TCalibrationTime(FCalibrations[j]).AsShortString + LineEnding;
          break;
        end;
      end;
    end;
  end;
  AMsg := Trim(AMsg);
end;

constructor TCalibrations.Create;
begin
  FCalibrationsModified := False;
  FFastNodeIdLookupList := nil;
  FValidationDone := False;
  FReader := nil;
  FValidator := TCalibrationsValidator.Create;
  FCalibrations := TList.Create;
  FIsBLensOnly := False;
  FCalibrationsType := ctInternalNode;
  FTreeList := nil;
  FAnalysisInfo := nil;
  ProgressUpdateProc := nil;
end;

procedure TCalibrations.Delete(Index: Integer);
var
  ACalib: TCalibrationTime;
begin
  if Index >= Count then
    Exit;
  ACalib := TCalibrationTime(FCalibrations[Index]);
  FreeAndNil(ACalib);
  FCalibrations.Delete(Index);
end;

destructor TCalibrations.Destroy;
var
  i: Integer;
begin
  FAnalysisInfo := nil;
  if Assigned(FReader) then
    FReader.Free;
  if Assigned(FValidator) then
    TCalibrationsValidator(FValidator).Free;
  if Assigned(FCalibrations) then
  begin
    if FCalibrations.Count > 0 then
      for i := 0 to FCalibrations.Count - 1 do
        if Assigned(FCalibrations[i]) then
          TCalibrationTime(FCalibrations[i]).Free;
    FCalibrations.Free;
  end;
  if Assigned(FTreeList) then
    TTreeList(FTreeList).Free;
  if Assigned(FFastNodeIdLookupList) then
    ClearFastNodeIdLookupList;
  inherited Destroy;
end;

procedure TCalibrations.UnSelectInvalidCalibrations;
var
  i: Integer;
  c: TCalibrationTime = nil;
begin
  if FCalibrations.Count > 0 then
    for i := 0 to FCalibrations.Count - 1 do
    begin
      c := GetCalibration(i);
      if not c.IsValid then
        c.IsSelected := False;
    end;
end;

function TCalibrations.NumValidCalibrations: Integer;
var
  i: Integer;
begin
  Result := 0;
  if FCalibrations.Count > 0 then
    for i := 0 to FCalibrations.Count - 1 do
      if TCalibrationTime(FCalibrations[i]).IsValid then
        inc(Result);
end;

function TCalibrations.NumSelectedCalibrations: Integer;
var
  i: Integer;
begin
  Result := 0;
  if FCalibrations.Count > 0 then
    for i := 0 to FCalibrations.Count - 1 do
      if TCalibrationTime(FCalibrations[i]).IsSelected then
        inc(Result);
end;

function TCalibrations.NumInvalidCalibrations: Integer;
var
  i: Integer;
begin
  Result := 0;
  if FCalibrations.Count > 0 then
    for i := 0 to FCalibrations.Count - 1 do
      if not TCalibrationTime(FCalibrations[i]).IsValid then
        inc(Result);
end;

function TCalibrations.NumInvalidCalibrationsThatAreSelected: Integer;
var
  i: Integer;
begin
  Result := 0;
  if FCalibrations.Count > 0 then
    for i := 0 to FCalibrations.Count - 1 do
      if (not TCalibrationTime(FCalibrations[i]).IsValid) and TCalibrationTime(FCalibrations[i]).IsSelected then
        inc(Result);
end;

function TCalibrations.DoAllValidations(var Msg: String; const IgnoreUnselected: Boolean; otuNames: TStringList): Boolean;
var
  TempMsg: String;
  TempResult: Boolean;
  numInvalid: Integer;
begin
  try
    Result := True;
    TempMsg := EmptyStr;
    Msg := EmptyStr;

    DoProgressUpdate(15, 'validating calibrations...');
    TempResult := ValidateCalibrations(TempMsg, otuNames);
    if not TempResult then
    begin
      Msg := TempMsg + LineEnding;
      Result := False;
    end;

    if IsSampleTimes then
    begin
      DoProgressUpdate(30, 'checking sample times...');
      TempResult := ValidateAllAreSampleTimes(TempMsg);
      if not TempResult then
      begin
        Msg := TempMsg + LineEnding;
        Result := False;
      end;
    end
    else
    begin
      DoProgressUpdate(30, 'checking calibration times...');
      TempResult := ValidateAllAreInternalNodes(TempMsg);
      if not TempResult then
      begin
        Msg := TempMsg + LineEnding;
        Result := False;
      end;
    end;

    if not IsSampleTimes then
    begin
      DoProgressUpdate(60, 'checking min and max constraints');
      TempResult := ValidateMinMaxTimes(TempMsg);
      if not TempResult then
      begin
        Msg :=  Msg + TempMsg + LineEnding;
        Result := False;
      end;
    end
    else
    begin
      DoProgressUpdate(60, 'checking sample times');
      TempResult := ValidateAllSampleTimesProvided(TempMsg);
      if not TempResult then
      begin
        Msg :=  Msg + TempMsg + LineEnding;
        Result := False;
      end;
    end;


    DoProgressUpdate(80, 'checking outgroup');
    TempResult := ValidateAndFilterOutgroupConstraint(TempMsg, IsSampleTimes, otuNames);
    if not TempResult then
    begin
      Msg :=  Msg + TempMsg;
      Result := False;
    end;

    DoProgressUpdate(95, 'checking sample times');
    Msg := RemoveDuplicateStrings(Trim(Msg));
    if Result then
    begin
      if IgnoreUnselected then
        numInvalid := NumInvalidCalibrationsThatAreSelected
      else
        numInvalid := NumInvalidCalibrations;
      if numInvalid > 0 then
      begin
        Result := False;
        Msg := Format('%s %d Invalid calibration(s) encountered.', [Msg, numInvalid]);
      end;
      FValidationDone := True;
    end;

    DoProgressUpdate(0, EmptyStr);
  except
    on E:Exception do
    begin
      Result := False;
      {$IFDEF VISUAL_BUILD}
      Msg := Trim(Msg + ' Oh no! An error occurred when validating calibrations: ' + E.Message);
      {$ELSE}
      error_nv('Oh no! An error occurred when validating calibrations: ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

function TCalibrations.ValidateAndFilterOutgroupConstraint(var Msg: String; IsSampleTime: Boolean; otuNames: TStringList): Boolean;
var
  AData: TTreeData;
begin
  Result := False;
  AData := TTreeList(FTreeList)[0];
  if IsSampleTime then
    Result := ValidateAndFilterSampleTimesOutgroupConstraint(Msg, AData)
  else
    Result := ValidateAndFilterOutgroupConstraint(Msg, AData, otuNames);
end;

function TCalibrations.ValidateMinMaxTimes(var Msg: String): Boolean;
var
  AData: TTreeData;
begin
  //Assert(Assigned(FAnalysisInfo));
  Assert(Assigned(FTreeList));
  //Assert(FAnalysisInfo.ClassNameIs('TAnalysisInfo'));
  Result := False;
  AData := TTreeList(FTreeList)[0];
  Result := ValidateMinMaxTimes(Msg, AData);
end;

function TCalibrations.ValidateAllAreInternalNodes(var Msg: String): Boolean;
var
  AValidator: TCalibrationsValidator;
  AData: TTreeData;
  TempCalibs: TArrayOfCalibrationTime;
  i: Integer;
  AMessage: TValidationMessage;
begin
  AValidator := nil;
  //Assert(Assigned(FAnalysisInfo));
  Assert(Assigned(FTreeList));
  //Assert(FAnalysisInfo.ClassNameIs('TAnalysisInfo'));
  Result := False;
  try
    AValidator := TCalibrationsValidator(FValidator);
    AData := TTreeList(FTreeList)[0];
    TempCalibs := GetSelectedCalibrationsArray;
    AValidator.SetData(AData, TempCalibs);
    Result := AValidator.ValidateAllAreInternalNodes(Msg);
    if not Result then
    begin
      MarkInvalidConstraints(AValidator.Invalids, Msg);
      for AMessage in AValidator.StatusMessages do
        ValidationMessages := ValidationMessages + [AMessage];
      if not (NumValidCalibrations > 0) then
      begin
        ValidationMessages := ValidationMessages + [vsError];
        Msg := Msg + LineEnding + 'Aborting analysis because all constraints are invalid.';
      end;
    end;
  finally
    if Assigned(TempCalibs) then
    begin
      for i := 0 to Length(TempCalibs) - 1 do
        TempCalibs[i].Free;
      SetLength(TempCalibs, 0);
    end;
  end;
end;

function TCalibrations.ValidateAllAreSampleTimes(var Msg: String): Boolean;
var
  AValidator: TCalibrationsValidator;
  AData: TTreeData;
  TempCalibs: TArrayOfCalibrationTime;
  i: Integer;
  AMessage: TValidationMessage;
begin
  AValidator := nil;
  //Assert(Assigned(FAnalysisInfo));
  Assert(Assigned(FTreeList));
  //Assert(FAnalysisInfo.ClassNameIs('TAnalysisInfo'));
  Result := False;
  try
    AValidator := TCalibrationsValidator(FValidator);
    AData := TTreeList(FTreeList)[0];
    TempCalibs := GetSelectedCalibrationsArray;
    AValidator.SetData(AData, TempCalibs);
    Result := AValidator.ValidateAllAreSampleTimes(Msg);
    if not Result then
    begin
      MarkInvalidSampleTimes(AValidator.Invalids, Msg);
      for AMessage in AValidator.StatusMessages do
        ValidationMessages := ValidationMessages + [AMessage];
      if not (NumValidCalibrations > 0) then
      begin
        ValidationMessages := ValidationMessages + [vsError];
        Msg := Msg + LineEnding + 'Aborting analysis because all constraints are invalid.';
      end;
    end;
  finally
    if Assigned(TempCalibs) then
    begin
      for i := 0 to Length(TempCalibs) - 1 do
        TempCalibs[i].Free;
      SetLength(TempCalibs, 0);
    end;
  end;
end;

function TCalibrations.ValidateAllSampleTimesProvided(var Msg: String): Boolean;
var
  AValidator: TCalibrationsValidator;
  AData: TTreeData;
  TempCalibs: TArrayOfCalibrationTime;
  i: Integer;
  AMessage: TValidationMessage;
  OtuNames: TStringList;
begin
  AValidator := nil;
  //Assert(Assigned(FAnalysisInfo));
  Assert(Assigned(FTreeList));
  //Assert(FAnalysisInfo.ClassNameIs('TAnalysisInfo'));
  Result := False;
  try
    AValidator := TCalibrationsValidator(FValidator);
    AData := TTreeList(FTreeList)[0];
    OtuNames := TTreeList(FTreeList).OTUNameList;
    TempCalibs := GetSelectedCalibrationsArray;
    AValidator.SetData(AData, TempCalibs, OtuNames);
    Result := AValidator.ValidateAllSampleTimesProvided(Msg);
    if not Result then
    begin
      ValidationMessages := ValidationMessages + [vsError];
      Msg := Msg + LineEnding + 'Sample times are required for all ingroup taxa. Please fix this to continue';
      for AMessage in AValidator.StatusMessages do
        ValidationMessages := ValidationMessages + [AMessage];
    end;
  finally
    if Assigned(TempCalibs) then
    begin
      for i := 0 to Length(TempCalibs) - 1 do
        TempCalibs[i].Free;
      SetLength(TempCalibs, 0);
    end;
  end;
end;

function TCalibrations.LoadFromList(aList: TStringList): Boolean;
var
  aMsg: String = '';
begin
  Result := False;
  if aList.Count = 0 then
    Exit;

  if not Assigned(FReader) then
    FReader := TCalibrationReader.Create(NumTaxa);
  Result := FReader.ReadCalibrationsStringListToList(aList, FCalibrations);
  if Result then
    if not CheckCalibrationsType(aMsg) then
      raise Exception.Create(aMsg);
end;

function TCalibrations.ValidateAndFilterOutgroupConstraint(var Msg: String; AData: TObject; otuNames: TStringList): Boolean;
var
  TempCalibs: TArrayOfCalibrationTime;
  i: Integer;
  AMessage: TValidationMessage;
  AValidator: TCalibrationsValidator;
  MinTimes: TDivTimesArray = nil;
  MaxTimes: TDivTimesArray = nil;
  labels: TStringList = nil;
  aTreeData: TTreeData = nil;
begin
  SetLength(MinTimes, 0);
  SetLength(MaxTimes, 0);
  Assert(AData.ClassNameIs('TTreeData'));
  Result := False;
  aTreeData := TTreeData(AData);
  labels := aTreeData.GetInternalNodeLabels;

  try
    AValidator := TCalibrationsValidator(FValidator);
    TempCalibs := GetSelectedCalibrationsArray;
    PrepareDivTimeArrays(Mintimes, MaxTimes, NumTaxa, IsBLensOnly, labels);
    AValidator.SetData(TTreeData(AData), TempCalibs, otuNames);
    AValidator.MinTimes := MinTimes;
    AValidator.MaxTimes := MaxTimes;
    if not AValidator.ValidateOutgroupConstraint(Msg) then
    begin
      MarkInvalidConstraints(AValidator.Invalids, Msg);
      for AMessage in AValidator.StatusMessages do
        ValidationMessages := ValidationMessages + [AMessage];
      if not (NumValidCalibrations > 0) then
      begin
        ValidationMessages := ValidationMessages + [vsError];
        Msg := Msg + LineEnding + 'Aborting analysis because all constraints are invalid. See https://www.megasoftware.net/web_help_11/index.htm#t=Calibration_File_Format.htm for a description of the calibration file format';
      end;
    end
    else
      Result := True;
  finally
    if Assigned(TempCalibs) then
    begin
      for i := 0 to Length(TempCalibs) - 1 do
        TempCalibs[i].Free;
      SetLength(TempCalibs, 0);
    end;
  end;
end;

function TCalibrations.ValidateAndFilterSampleTimesOutgroupConstraint(var Msg: String; AData: TObject): Boolean;
var
  TempCalibs: TArrayOfCalibrationTime = nil;
  i: Integer;
  AMessage: TValidationMessage;
  AValidator: TCalibrationsValidator = nil;
  SampleTimes: TDivTimesArray = nil;
  namesList: TStringList = nil;
begin
  SetLength(SampleTimes, 0);
  SetLength(TempCalibs, 0);
  Assert(AData.ClassNameIs('TTreeData'));
  Result := False;
  try
    AValidator := TCalibrationsValidator(FValidator);
    namesList := TTreeList(FTreeList).OTUNameList;
    PrepareSamplingTimeArray(SampleTimes, TempCalibs, namesList);
    AValidator.SetData(TTreeData(AData), TempCalibs);
    AValidator.MinTimes := SampleTimes;
    if not AValidator.ValidateSampledTimes(Msg) then
    begin
      MarkInvalidSampleTimes(AValidator.Invalids, Msg);
      for AMessage in AValidator.StatusMessages do
        ValidationMessages := ValidationMessages + [AMessage];
      if not (NumValidCalibrations > 0) then
      begin
        ValidationMessages := ValidationMessages + [vsError];
        Msg := Msg + LineEnding + 'Aborting analysis because all constraints are invalid. See https://www.megasoftware.net/web_help_11/index.htm#t=Calibration_File_Format.htm for a description of the calibration file format';
      end;
    end
    else
      Result := True;
  finally
    if Assigned(TempCalibs) then
    begin
      for i := 0 to Length(TempCalibs) - 1 do
        TempCalibs[i].Free;
      SetLength(TempCalibs, 0);
    end;
  end;
end;

function TCalibrations.ValidateCalibrations(var Msg: String; otuNames: TStringList): Boolean;
var
  AValidator: TCalibrationsValidator;
  AData: TTreeData;
  TempCalibs: TArrayOfCalibrationTime;
  i: Integer;
  AMessage: TValidationMessage;
begin
  AValidator := nil;
  Assert(Assigned(FTreeList));
  Result := False;
  try
    AValidator := TCalibrationsValidator(FValidator);
    AData := TTreeList(FTreeList)[0];
    TempCalibs := GetSelectedCalibrationsArray;
    AValidator.SetData(AData, TempCalibs, otunames);
    Result := AValidator.ValidateCalibrations(Msg);
    if not Result then
    begin
      for AMessage in AValidator.StatusMessages do
        ValidationMessages := ValidationMessages + [AMessage];
    end;
  finally
    if Assigned(TempCalibs) then
    begin
      for i := 0 to Length(TempCalibs) - 1 do
        TempCalibs[i].Free;
      SetLength(TempCalibs, 0);
    end;
  end;
end;

function TCalibrations.ValidateMinMaxTimes(var Msg: String; AData: TObject): Boolean;
var
  TreeData: TTreeData = nil;
  AValidator: TCalibrationsValidator = nil;
  TempCalibs: TArrayOfCalibrationTime = nil;
  i: Integer;
  MinTimes: TDivTimesArray = nil;
  MaxTimes: TDivTimesArray = nil;
  AMessage: TValidationMessage;
  labels: TStringList = nil;
begin
  SetLength(MinTimes, 0);
  SetLength(MaxTimes, 0);
  Assert(AData.ClassNameIs('TTreeData'));
  TreeData := TTreeData(AData);
  labels := TreeData.GetInternalNodeLabels;

  try
    AValidator := TCalibrationsValidator(FValidator);
    TempCalibs := GetSelectedCalibrationsArray;
    AValidator.SetData(TreeData, TempCalibs);
    PrepareDivTimeArrays(MinTimes, MaxTimes, NumTaxa, IsBLensOnly, labels);
    AValidator.MinTimes := MinTimes;
    AValidator.MaxTimes := MaxTimes;
    Result := AValidator.ValidateMinMaxTimes(Msg);
    if not Result then
    begin
      for AMessage in AValidator.StatusMessages do
        ValidationMessages := ValidationMessages + [AMessage];
    end;
  finally
    if Assigned(TempCalibs) then
    begin
      for i := 0 to Length(TempCalibs) - 1 do
        TempCalibs[i].Free;
      SetLength(TempCalibs, 0);
    end;
  end;
end;

function TCalibrations.LoadFromFile(Filename: String): Boolean;
var
  aMsg: String = '';
begin
  Result := False;
  if not FileExists(Filename) then
    Exit;

  if not Assigned(FReader) then
    FReader := TCalibrationReader.Create(NumTaxa);
  Result := FReader.ReadCalibrationsFileToList(Filename, FCalibrations);
  if Result then
    if not CheckCalibrationsType(aMsg) then
      raise Exception.Create(aMsg);
end;

{$IFNDEF VISUAL_BUILD}
procedure TCalibrations.FilterMissingTaxa(otuNames: TStringList);
var
  i: Integer = -1;
  c: TCalibrationTime = nil;
  g: TGroupCalibrationTime = nil;
  j: Integer = -1;
  aName: String = '';
begin
  if Count > 0 then
    for i := Count - 1 downto 0 do
    begin
      c := GetCalibration(i);
      if c is TIngroupRootCalibration then
        continue;
      if c is TGroupCalibrationTime then
      begin
        g := TGroupCalibrationTime(c);
        for j := g.TaxaCount - 1 downto 0 do
        begin
          aName := g.TaxaNames[j];
          if otuNames.IndexOf(aName) < 0 then
          begin
            warn_NV(Format('%s was removed from the calibration taxa list because it is not in the list of taxa being analyzed', [aName]));
            g.TaxaNames.Delete(j);
            FCalibrationsModified := True;
          end;
        end;
        if g.TaxaCount < 2 then
        begin
          warn_NV(Format('calibration #%d was removed because it references no taxa in the data being analyzed', [i + 1]));
          FCalibrations.Delete(i);
          FCalibrationsModified := True;
          g.Free;
        end;
      end
      else
      begin
        if (c.NodeName <> EmptyStr) and (c.NodeA = EmptyStr) and (c.NodeB = EmptyStr) then
          continue;
        if otuNames.IndexOf(c.NodeA) < 0 then
        begin
          warn_NV(Format('calibration #%d was removed because it references "%s" which is not in the list of taxa being analyzed', [i + 1, c.NodeA]));
          FCalibrations.Delete(i);
          FCalibrationsModified := True;
          c.Free;
          continue
        end;

        if otuNames.IndexOf(c.NodeB) < 0 then
        begin
          warn_NV(Format('calibration #%d was removed because it references "%s" which is not in the list of taxa being analyzed', [i + 1, c.NodeB]));
          FCalibrations.Delete(i);
          FCalibrationsModified := True;
          c.Free;
          continue
        end;
      end;
    end;
  {$IFNDEF VISUAL_BUILD}
  if FCalibrations.Count = 0 then
    error_nv('no calibrations remain after filtering missing taxa names. Aborting the analysis.');
  {$ENDIF}
end;
{$ENDIF}

procedure TCalibrations.SaveSelectedToList(var AList: TStringList);
var
  i: Integer;
begin
  AList.Clear;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if TCalibrationTime(FCalibrations[i]).IsSelected then
        AList.Add(TCalibrationTime(FCalibrations[i]).AsString);
end;

{$IFDEF DEBUG}
function TCalibrations.DebugStringsToFile(filename: String): Boolean;
var
  i: Integer;
  f: TextFile;
begin
  try
    AssignFile(f, filename);
    Rewrite(f);
    WriteLn(f, TCalibrationTime.DebugHeaderString);
    if Count > 0 then
      for i := 0 to Count - 1 do
        WriteLn(f, GetCalibration(i).DebugString);
  finally
    CloseFile(f);
  end;
  Result := FileExists(filename);
end;

procedure TCalibrations.DebugWriteToDevConsole;
var
  i: Integer = -1;
  c: TCalibrationTime = nil;
begin
  WriteToDevConsole(TCalibrationTime.DebugHeaderString);
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      c := GetCalibration(i);
      WriteToDevConsole(c.DebugString);
    end;
end;

{$ENDIF}

function TCalibrations.NumOtusInTree: Integer;
begin
  if Assigned(FAnalysisInfo) then
    Result := TAnalysisInfo(FAnalysisInfo).NoOfTaxa
  else if Assigned(FTreeList) then
    Result := TTreeList(FTreeList).NoOfOTUs
  else
    Result := 0;
end;

function TCalibrations.SaveToFile(Filename: String): Boolean;
var
  AList: TStringList;
begin
  AList := nil;
  Result := False;
  try
    try
      AList := TStringList.Create;
      SaveToList(AList);
      AList.SaveToFile(Filename);
      Result := FileExists(Filename);
    except
      Result := False;
    end;
  finally
    if Assigned(AList) then
      AList.Free;
  end;
end;

procedure TCalibrations.SaveToList(var AList: TStringlist);
var
  i: Integer;
begin
  AList.Clear;
  if Count > 0 then
    for i := 0 to Count - 1 do
      AList.Add(TCalibrationTime(FCalibrations[i]).AsString);
end;

function TCalibrations.GetStringList: TStringList;
begin
  Result := nil;
  Assert(False, 'not implemented');
end;

function TCalibrations.GetTreeList: TObject;
begin
  Result := FTreeList;
end;

function TCalibrations.GetMinTimesArray: TDivTimesArray;
begin
  SetLength(Result, 0);
  Assert(False, 'not implemented');
end;

function TCalibrations.GetMaxTimesArray: TDivTimesArray;
begin
  SetLength(Result, 0);
  Assert(False, 'not implemented');
end;

procedure TCalibrations.SetCalibrationsType(const Value: TCalibrationTarget);
begin
  FCalibrationsType := Value;
end;

function TCalibrations.CheckCalibrationsType(var aMsg: String): Boolean;
var
  i: Integer;
  calib: TCalibrationTime;
begin
  Result := True;
  if FCalibrations.Count > 0 then
  begin
    calib := TCalibrationTime(FCalibrations[0]);
    FCalibrationsType := calib.CalibrationTarget;
    if FCalibrations.Count > 1 then
      for i := 1 to FCalibrations.Count - 1 do
      begin
        calib := TCalibrationTime(FCalibrations[i]);
        if FCalibrationsType <> calib.CalibrationTarget then
        begin
          aMsg := 'Invalid calibrations - cannot mix tip dates with divergence times';
          Result := False;
          Exit;
        end;
      end;
  end;
end;

procedure TCalibrations.DoProgressUpdate(aProgress: Integer; aStatus: String);
begin
  if Assigned(ProgressUpdateProc) then
    ProgressUpdateProc(aProgress, aStatus);
end;

function TCalibrations.NewCalibrationsHaveDuplicates(var Msg: String; var NewCalibrations: TCalibrations): Boolean;
var
  i: Integer;
  c: TCalibrationTime = nil;
  newC: TCalibrationTime = nil;
  tempMsg: String = '';
  numDuplicates: Integer = 0;
begin
  try
    NewCalibrations.InitFastNodeIdLookupList(NumNodes + 1);
    Result := False;
    if (NewCalibrations.Count > 0) and (Self.Count > 0) then
      for i := 0 to Self.Count - 1 do
      begin
        c := GetCalibration(i);
        if NewCalibrations.HasCalibrationForNode(c.NodeID) then
        begin
          inc(numDuplicates);
          newC := NewCalibrations.GetCalibrationByNodeId(c.NodeID);
          tempMsg := tempMsg + 'Duplicate calibration: ' + newC.CalibrationName + LineEnding;
          Result := True;
        end;
      end;
    if Result then
      Msg := Format('%d duplicates found. %s', [numDuplicates, tempMsg]) + LineEnding + Msg;
  finally
    NewCalibrations.ClearFastNodeIdLookupList;
  end;
end;

procedure TCalibrations.UnselectNewDuplicates(var newCalibrations: TCalibrations);
var
  i: Integer;
  c: TCalibrationTime = nil;
begin
  try
    NewCalibrations.InitFastNodeIdLookupList(NumNodes + 1);
    if (NewCalibrations.Count > 0) and (Self.Count > 0) then
      for i := 0 to Self.Count - 1 do
      begin
        c := GetCalibration(i);
        if NewCalibrations.HasCalibrationForNode(c.NodeID) then
          NewCalibrations.UnselectCalibrationsForNode(c.NodeID);
      end;
  finally
    NewCalibrations.ClearFastNodeIdLookupList;
  end;
end;

procedure TCalibrations.UnselectCalibrationsForNode(aNodeId: Integer);
var
  i: Integer;
  c: TCalibrationTime = nil;
begin
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      c := GetCalibration(i);
      if c.NodeID = aNodeId then
        c.IsSelected := False;
    end;
end;

procedure TCalibrations.InitFastNodeIdLookupList(maxNodeId: Integer);
var
  i: Integer;
  c: TCalibrationTime = nil;
begin
  FFastNodeIdLookupList := TList.Create;
  if maxNodeId > 0 then
    for i := 0 to maxNodeId do
      FFastNodeIdLookupList.Add(nil);
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      c := GetCalibration(i);
      Assert((c.NodeID <= maxNodeId) and (c.NodeID >= 0));
      FFastNodeIdLookupList[c.NodeID] := c;
    end;
end;

procedure TCalibrations.ClearFastNodeIdLookupList;
begin
  if Assigned(FFastNodeIdLookupList) then
  begin
    FFastNodeIdLookupList.Clear;
    FreeAndNil(FFastNodeIdLookupList);
  end;
end;

procedure TCalibrations.SetInfo(AnalysisInfo: TObject);
var
  aTreeList: TTreeList;
  aInfo: TAnalysisInfo;
begin
  Assert((AnalysisInfo = nil) or AnalysisInfo.ClassNameIs('TAnalysisInfo'));
  FAnalysisInfo := AnalysisInfo;
  if not Assigned(AnalysisInfo) then
    Exit;
  aInfo := TAnalysisInfo(FAnalysisInfo);
  if Assigned(aInfo.MyOriTreeList) then
  begin
    aTreeList := aInfo.MyOriTreeList;
    AssignTreeList(aTreeList);
  end;
end;

procedure TCalibrations.Assign(Source: TCalibrations);
var
  i: Integer;
  ACalib: TCalibrationTime;
begin
  Clear;
  FCalibrationsModified := Source.CalibrationsModified;
  FCalibrationsType := Source.CalibrationsType;
  if Source.Count > 0 then
    for i := 0 to Source.Count - 1 do
    begin
      ACalib := TCalibrationTime.Create;
      ACalib.Assign(Source.GetCalibration(i));
      FCalibrations.Add(ACalib);
    end;
    IsBLensOnly := Source.IsBLensOnly;
    if not Assigned(FTreeList) then
      FTreeList := TTreeList.Create;
    TTreeList(FTreeList).DeleteAll;
    if Assigned(Source.FTreeList) then
      AssignTreeList(Source.FTreeList);
  FValidationDone := Source.FValidationDone;
end;

procedure TCalibrations.AssignTreeList(AList: TObject);
var
  aTreeList: TTreeList;
begin
  Assert(AList.ClassNameIs('TTreeList'));
  if not Assigned(FTreeList) then
  begin
    aTreeList := TTreeList.Create;
    FTreeList := aTreeList;
  end;
  TTreeList(FTreeList).Assign(TTreeList(AList));
end;

function TCalibrations.GetCalibration(Index: Integer): TCalibrationTime;
begin
  Assert((Index >= 0) and (Index < Count));
  Result := TCalibrationTime(FCalibrations[Index]);
end;

function TCalibrations.Add(ACalibration: TCalibrationTime; overwriteExisting: Boolean = False): Integer;
var
  c: TCalibrationTime;
begin
  if overwriteExisting then
  begin
    if HasCalibrationForNode(ACalibration.NodeID) then
    begin
      c := GetCalibrationByNodeId(ACalibration.NodeID);
      c.Assign(ACalibration);
      Result := -1;
    end
    else
    begin
      Result := FCalibrations.Add(ACalibration);
      if Assigned(FFastNodeIdLookupList) and (FFastNodeIdLookupList.Count > ACalibration.NodeID) then
        FFastNodeIdLookupList[ACalibration.NodeID] := ACalibration;
    end;
  end
  else
    Result := FCalibrations.Add(ACalibration);
  FValidationDone := False;
end;

procedure TCalibrations.Insert(ACalibration: TCalibrationTime; Index: Integer);
begin
  FCalibrations.Insert(Index, ACalibration);
end;

function TCalibrations.IsSampleTimes: Boolean;
begin
  Result := (FCalibrationsType = ctLeafNode);
end;

function TCalibrations.GetUsesCalibrationDensities: Boolean;
var
  i: Integer;
  aCalib: TCalibrationTime;
begin
  Result := False;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      aCalib := GetCalibration(i);
      if aCalib.IsStatisticalDensity then
      begin
        Result := True;
        Exit;
      end;
    end;
end;

function TCalibrations.GetLatestSamplingTime: Extended;
var
  i: Integer;
begin
  if not IsSampleTimes then
    raise Exception.Create('Application Error: trying to get sampling time for non-RTDT analysis');
  Result := 0.0;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if GetCalibration(i).MinTime > Result then
        Result := GetCalibration(i).MinTime;
end;

function TCalibrations.HasCalibrationForNode(aNodeId: Integer): Boolean;
begin
  if (not Assigned(FFastNodeIdLookupList)) or (aNodeId >= FFastNodeIdLookupList.Count) then
    raise Exception.Create('Fast node lookup list needs to be initialized before checking if a calibration already exists for a node ID');
  if Assigned(FFastNodeIdLookupList[aNodeId]) then
    Result := True
  else
    Result := False;
end;

function TCalibrations.GetCalibrationByNodeId(aNodeId: Integer): TCalibrationTime;
begin
  Assert(Assigned(FFastNodeIdLookupList) and (aNodeId < FFastNodeIdLookupList.Count), 'Fast node lookup list not initialized');
  Result := FFastNodeIdLookupList[aNodeId];
end;

function TCalibrations.GetDisplayData(const TreeViewIndex: Integer; var minTime: Double; var maxTime: Double; var isSelected: Boolean): Boolean;
var
  i: Integer = -1;
  c: TCalibrationTime = nil;
begin
  Result := False;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      c := GetCalibration(i);
      if c.TreeViewFormIndex = TreeViewIndex then
      begin
        Result := True;
        minTime := c.MinTime;
        maxTime := c.MaxTime;
        isSelected := c.IsSelected;
      end;
    end;
end;

function TCalibrations.CalibrationIndicesForTE(includeUnselected: Boolean = False): ArrayOfInteger;
var
  i: Integer;
  ACalib: TCalibrationTime;
begin
  SetLength(Result, 0);
  if FCalibrations.Count > 0 then
  begin
    for i := 0 to FCalibrations.Count - 1 do
    begin
      ACalib := TCalibrationTime(FCalibrations[i]);
      if (not ACalib.IsSelected) and (not includeUnselected) then
        continue;
      if (Trim(ACalib.NodeA) <> EmptyStr) and (Trim(ACalib.NodeB) <> EmptyStr) and (ACalib.TreeViewFormIndex >= 1) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := TCalibrationTime(FCalibrations[i]).TreeViewFormIndex;
      end;
    end;
  end;
end;

procedure TCalibrations.Clear;
var
  i: Integer;
  ACalib: TCalibrationTime;
begin
  FValidationDone := False;
  if Count = 0 then
    Exit;

  for i := (Count - 1) downto 0 do
  begin
    ACalib := TCalibrationTime(FCalibrations[i]);
    ACalib.Free;
  end;
  FCalibrations.Clear;
end;

function TCalibrations.Count: Integer;
begin
  if Assigned(FCalibrations) then
    Result := FCalibrations.Count
  else
    Result := 0;
end;

function TCalibrations.WriteToSessionFile(var SessionFile: File): Boolean;
var
  i: Integer;
begin
  i := ord(FCalibrationsType);
  BlockWrite(SessionFile, i, sizeOf(Integer));
  BlockWrite(SessionFile, FValidationDone, SizeOf(FValidationDone));
  BlockWrite(SessionFile, IsBLensOnly, SizeOf(IsBLensOnly));
  Result := True;
  if Count > 0 then
    for i := 0 to Count - 1 do
      Result := (Result and TCalibrationTime(FCalibrations[i]).WriteToFile(SessionFile));
end;

function TCalibrations.ReadFromSessionFile(var SessionFile: File; SessionVersion: LongInt; NumCalibs: Integer): Boolean;
var
  ACalib: TCalibrationTime;
  i: Integer = -1;
begin
  Result := True;
  if SessionVersion > 1005 then
  begin
    BlockRead(SessionFile, i, SizeOf(Integer));
    case i of
      0: FCalibrationsType := ctInternalNode;
      1: FCalibrationsType := ctLeafNode;
      else
        raise Exception.Create('Application Error: invalid calibrations type');
    end;
    BlockRead(SessionFile, FValidationDone, SizeOf(FValidationDone));
    BlockRead(SessionFile, FIsBLensOnly, SizeOf(FIsBLensOnly));
  end;

  if NumCalibs > 0 then
    for i := 0 to NumCalibs - 1 do
    begin
      ACalib := TCalibrationTime.Create;
      Result := (Result and ACalib.LoadFromFile(SessionVersion, SessionFile));
      FCalibrations.Add(ACalib);
    end;
end;

procedure TCalibrations.RemoveInvalids;
var
  i: Integer;
begin
  if Count = 0 then
    Exit;
  for i := (Count - 1) downto 0 do
    if  not TCalibrationTime(FCalibrations[i]).IsValid then
    begin
      TCalibrationTime(FCalibrations[i]).Free;
      FCalibrations.Delete(i);
    end;
end;

procedure TCalibrations.RemoveUnselected;
var
  i: Integer;
begin
  if Count = 0 then
    Exit;
  for i := (Count - 1) downto 0 do
    if  not TCalibrationTime(FCalibrations[i]).IsSelected then
    begin
      TCalibrationTime(FCalibrations[i]).Free;
      FCalibrations.Delete(i);
    end;
end;

procedure TCalibrations.RemoveDuplicates;
var
  i: Integer;
  c: TCalibrationTime = nil;
  toRemove: TList = nil;
begin
  try
    toRemove := TList.Create;
    FFastNodeIdLookupList := TList.Create;
    for i := 0 to NumNodes do
      FFastNodeIdLookupList.Add(nil);
    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        c := GetCalibration(i);
        if FFastNodeIdLookupList[c.NodeID] = nil then
          FFastNodeIdLookupList[c.NodeID] := c
        else
          toRemove.Add(c);
      end;
    if toRemove.Count > 0 then
      for i := toRemove.Count - 1 downto 0 do
      begin
        c := TCalibrationTime(toRemove[i]);
        FCalibrations.Remove(c);
        c.Free;
      end;
  finally
    if Assigned(FFastNodeIdLookupList) then
      FreeAndNil(FFastNodeIdLookupList);
    if Assigned(toRemove) then
    begin
      toRemove.Clear;
      toRemove.Free;
    end;
  end;
end;

function TCalibrations.CountAndUnSelectDuplicates: Integer;
var
  i: Integer;
  c: TCalibrationTime = nil;
begin
  Result := 0;
  try
    FFastNodeIdLookupList := TList.Create;
    for i := 0 to NumNodes do
      FFastNodeIdLookupList.Add(nil);
    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        c := GetCalibration(i);
        if FFastNodeIdLookupList[c.NodeID] = nil then
          FFastNodeIdLookupList[c.NodeID] := c
        else
        begin
          c.IsValid := False;
          c.IsSelected := False;
          inc(Result);
        end;
      end;
  finally
    if Assigned(FFastNodeIdLookupList) then
      FreeAndNil(FFastNodeIdLookupList);
  end;
end;

function TCalibrations.NumDuplicates: Integer;
var
  i: Integer;
  c: TCalibrationTime = nil;
begin
  Result := 0;
  try
    FFastNodeIdLookupList := TList.Create;
    if NumNodes > 0 then
      for i := 0 to NumNodes + 1 do
        FFastNodeIdLookupList.Add(nil);
    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        c := GetCalibration(i);
        if FFastNodeIdLookupList[c.NodeID] = nil then
          FFastNodeIdLookupList[c.NodeID] := c
        else
          inc(Result);
      end;
  finally
    if Assigned(FFastNodeIdLookupList) then
      FreeAndNil(FFastNodeIdLookupList);
  end;
end;

function TCalibrations.NumSelectedDuplicates(var aMsg: String): Integer;
var
  i: Integer;
  c: TCalibrationTime = nil;
begin
  Result := 0;
  aMsg := EmptyStr;
  if Assigned(FFastNodeIdLookupList) then
    raise Exception.Create('Application error: fast node ID lookup list already initialized');

  try
    FFastNodeIdLookupList := TList.Create;
    for i := 0 to NumNodes do
      FFastNodeIdLookupList.Add(nil);
    if Count > 0 then
    begin
      for i := 0 to Count - 1 do
      begin
        c := GetCalibration(i);
        if c.IsSelected then
        begin
          if Assigned(FFastNodeIdLookupList[c.NodeID]) then
          begin
            aMsg := aMsg + c.CalibrationName + LineEnding;
            inc(Result);
          end
          else
            FFastNodeIdLookupList[c.NodeID] := c;
        end;
      end;
    end;
  finally
    if Assigned(FFastNodeIdLookupList) then
      FreeAndNil(FFastNodeIdLookupList);
  end;
end;

function TCalibrations.GetSelectedCalibrationsArray: TArrayOfCalibrationTime;
var
  i: Integer;
  Index: Integer;
  aCalib: TCalibrationTime = nil;
begin
  SetLength(Result, 0);
  Index := 0;
  for i := 0 to FCalibrations.Count - 1 do
  begin
    if TCalibrationTime(FCalibrations[i]).IsValid and TCalibrationTime(FCalibrations[i]).IsSelected then
    begin
      SetLength(Result, Length(Result) + 1);
      aCalib := FCalibrations[i];
      if aCalib is TIngroupRootCalibration then
        Result[Index] := TIngroupRootCalibration.Create
      else
        Result[Index] := TCalibrationTime.Create;
      Result[Index].Assign(TCalibrationTime(FCalibrations[i]));
      inc(Index);
    end;
  end;
end;

{ TCalibrationReader }

function TCalibrationReader.ReadCalibrationStrings(CalibrationStrings: TStringList): TArrayOfCalibrationTime;
var
  SplitLine: TStringList = nil;
  i: Integer;
  j: Integer;
  MinTime: Double;
  MaxTime: double;
  TempCalibrationTime: TCalibrationTime = nil;
  CurrLine: String;
  Key: String;
  Value: String;
  densityType: TCalibrationDensityDistribution = cddNone;
  mean: Double = -1;
  aStddev: Double = -1;
  rate: Double = -1;
  lambda: Double = -1;
  time: Double = -1;
  temp: String = '';
begin
  SetLength(Result, 0);
  for i := 0 to CalibrationStrings.Count - 1 do
  begin
    MinTime := -1;
    MaxTime := -1;
    lambda := -1;
    mean := -1;
    time := -1;
    aStddev := -1;
    rate := -1;
    densityType := cddNone;

    CurrLine :=  Trim(CalibrationStrings.Strings[i]);
    if CurrLine = EmptyStr then
      Continue;
    if CurrLine[1] <> '!' then
      raise Exception.Create('Calibration commands must start with ''!''');
    Delete(CurrLine, 1, 1);

    try
      // Remove a ';' if there is one on the end.
      if CurrLine[Length(CurrLine)] = ';' then
        Delete(CurrLine, Length(CurrLine), 1);
      SplitLine := SplitOnWhiteSpaceRespQuotes(CurrLine);
      // Ensure that this starts with a known command
      if (CompareText(SplitLine.Names[0], 'MRCA') <> 0) and
         (CompareText(SplitLine.Names[0], 'NodeName') <> 0) and
         (CompareText(SplitLine.Names[0], 'Taxa') <> 0) and
         (CompareText(SplitLine[0], 'IngroupRoot') <> 0) and
         (CompareText(SplitLine.Names[0], 'Taxon') <> 0) then
        raise Exception.Create('Invalid command, expecting MRCA or NodeName.');
      TempCalibrationTime := TCalibrationTime.Create;

      // Loop over the key value pair entries.
      for j := 0 to SplitLine.Count - 1 do
      begin
        if SameText(SplitLine[j], 'IngroupRoot') then
          Key := 'IngroupRoot'
        else
          Key := Copy(SplitLine[j], 1, Pos('=', SplitLine[j]) - 1);
        if Trim(Key) = EmptyStr then
          raise Exception.Create('Invalid calibration entry.  Error on line# ' + IntToStr(i+1) + '. Parameter #' + IntToStr(j+1) + ' - ' + SplitLine[j]);
        if Pos('=', SplitLine[j]) > 0 then
          Value := Trim(Copy(SplitLine[j], Pos('=', SplitLine[j])+1, Length(SplitLine[j])))
        else
          Value := EmptyStr;

        case StringToCaseSelect(Key, ['TaxonA', 'TaxonB','minTime', 'time', 'maxTime', 'MRCA', 'NodeName', 'CalibrationName', 'Taxon', 'Distribution', 'mean', 'stddev', 'lambda', 'rate', 'offset', 'sd', 'sampleTime', 'tipDate', 'IngroupRoot', 'Taxa']) of
          0: { Taxon A}
          begin
            if TempCalibrationTime.NodeA <> EmptyStr then
              raise Exception.Create('Duplicate TaxonA specified.');
            if CompareText(Value, TempCalibrationTime.NodeB) = 0 then
              raise Exception.Create('TaxonA refers to the same name as Taxon B.');
            if Value = EmptyStr then
              raise Exception.Create('No name specified for TaxonA');
            TrimTaxaName2(Value);
            TempCalibrationTime.NodeA := Value;
          end;
          1: { Taxon B}
          begin
            if TempCalibrationTime.NodeB <> EmptyStr then
              raise Exception.Create('Duplicate TaxonB specified.');
            if CompareText(TempCalibrationTime.NodeA, Value) = 0 then
              raise Exception.Create('TaxonB refers to the same name as Taxon A.');
            if Value = EmptyStr then
              raise Exception.Create('No name specified for TaxonB');
            TrimTaxaName2(Value);
            TempCalibrationTime.NodeB := Value;
          end;
          2: { minTime}
          begin
            Value := StringReplace(Value, ',', '', [rfReplaceAll]);
            if TempCalibrationTime.MinTime <> -1 then
              raise Exception.Create('Duplicate minTime specified.');
            if Value = EmptyStr then
              raise Exception.Create('No time specified for the mintime entry.');
            if not TryStrToFloat(Value, MinTime) then
              raise Exception.Create('Minimum time was not a valid number format.   Error on line#' + IntToStr(i+1));
            if MinTime <= 0 then
              raise Exception.Create('Minimum time is required to be a number larger than 0.');
            if (MaxTime <> -1) and (MinTime > MaxTime) then
              raise Exception.Create('Minimum time is larger than maximum time.');
            TempCalibrationTime.MinTime := MinTime;
          end;
          3, 14: { time, offset}
          begin
            Value := StringReplace(Value, ',', '', [rfReplaceAll]);
            if TempCalibrationTime.MinTime <> -1 then
              raise Exception.Create('Duplicate time specified.');
            if Value = EmptyStr then
              raise Exception.Create('No value specified for the time entry.');
            if not TryStrToFloat(Value, MinTime) then
              raise Exception.Create('The time value was not a valid number format.   Error on line#' + IntToStr(i+1));
            if MinTime <= 0 then
              raise Exception.Create('The time value is required to be a number larger than 0.');
            TempCalibrationTime.MinTime := MinTime;
            TempCalibrationTime.MaxTime := MinTime; // it is a point divtime so set both max and min
            time := MinTime;
          end;
          4: { maxTime}
          begin
            Value := StringReplace(Value, ',', '', [rfReplaceAll]);
            if TempCalibrationTime.MaxTime <> -1 then
              raise Exception.Create('Duplicate maxTime specified.');
            if Value = emptyStr then
              raise Exception.Create('No time specified for the maxTime entry.');
            if not TryStrToFloat(Value, MaxTime) then
              raise Exception.Create('Maximum time was not a valid number format.   Error on line#' + IntToStr(i+1));
            if MaxTime <= 0 then
              raise Exception.Create('Maximum time is required to be a number larger than 0.');
            if (MinTime <> -1) and (MinTime > MaxTime) then
              raise Exception.Create('Minimum time is larger than maximum time.');
            TempCalibrationTime.MaxTime := MaxTime;
          end;
          5: { MRCA (includes node name)}
          begin
            if (TempCalibrationTime.MRCAName <> EmptyStr) and (TempCalibrationTime.MrCAName <> '-') then
              raise Exception.Create('Duplicate MRCA name specified.');

            TempCalibrationTime.MRCAName := Value;
          end;
          6: { NodeName}
          begin
            if (TempCalibrationTime.NodeName <> EmptyStr) and (TempCalibrationTime.NodeName <> INGROUP_ROOT) then
              raise Exception.Create('Duplicate NodeName specified.');

            TempCalibrationTime.NodeName := Value;
          end;
          7: { CalibrationName}
          begin
            TempCalibrationTime.CalibrationName := Value;
          end;
          8: { taxon}
          begin
            if TempCalibrationTime.NodeName <> EmptyStr then
              raise Exception.Create('Duplicate Taxon specified.');
            if Value = EmptyStr then
              raise Exception.Create('No name specified for Taxon');
            TrimTaxaName2(Value);
            TempCalibrationTime.NodeName := Value;
            TempCalibrationTime.NodeA := Value;
            TempCalibrationTime.NodeB := Value;
            TempCalibrationTime.IsSamplingTime := True;
            TempCalibrationTime.CalibrationTarget := ctLeafNode;
          end;
          9: { Distribution}
          begin
            if SameText(Value, 'normal') then
              densityType := cddNormal
            else if SameText(Value, 'exponential') then
              densityType := cddExponential
            else if SameText(Value, 'uniform') then
              densityType := cddUniform
            else if SameText(Value, 'lognormal') or SameText(Value, 'log-normal') or SameText(Value, 'log_normal') then
              densityType := cddLognormal
            else
              raise Exception.Create('invalid density distribution: ' + Value);
          end;
          10:  { mean}
            begin
              Value := StringReplace(Value, ',', '', [rfReplaceAll]);
              if not TryStrToFloat(Value, mean) then
                raise Exception.Create('invalid mean specified');
            end;
          11, 15:  { stddev}
            begin
              Value := StringReplace(Value, ',', '', [rfReplaceAll]);
              if not TryStrToFloat(Value, aStddev) then
                raise Exception.Create('invalid stddev specified');
            end;
          12:  { lambda}
            begin
              Value := StringReplace(Value, ',', '', [rfReplaceAll]);
              if not TryStrToFloat(Value, lambda) then
                raise Exception.Create('invalid lambda specified');
            end;
          13:  { rate}
            begin
              Value := StringReplace(Value, ',', '', [rfReplaceAll]);
              if not TryStrToFloat(Value, rate) then
                raise Exception.Create('invalid rate specified');
            end;
          16, 17: { sampleTime, tipDate}
            begin
              Value := StringReplace(Value, ',', '', [rfReplaceAll]);
              if TempCalibrationTime.MinTime <> -1 then
                raise Exception.Create('Duplicate time specified.');
              if Value = EmptyStr then
                raise Exception.Create('No value specified for the time entry.');
              MinTime := TTipDatesFinder.SampleTimeStrToSampleTime(Value);
              if MinTime <= 0 then
                raise Exception.Create('The time value is required to be a number larger than 0.');
              TempCalibrationTime.MinTime := MinTime;
              TempCalibrationTime.MaxTime := MinTime; // it is a point divtime so set both max and min
              time := MinTime;
            end;
          18: {IngroupRoot}
            begin
              if Assigned(TempCalibrationTime) then
                FreeAndNil(TempCalibrationTime);
              TempCalibrationTime := TIngroupRootCalibration.Create(False);
            end;
          19: {Taxa}
            begin
              if Assigned(TempCalibrationTime) then
              begin
                temp := TempCalibrationTime.NodeName;
                FreeAndNil(TempCalibrationTime);
              end
              else
                temp := EmptyStr;
              TempCalibrationTime := TGroupCalibrationTime.Create;
              TGroupCalibrationTime(TempCalibrationTime).AddTaxaFromCalibrationString(CurrLine);
              TempCalibrationTime.NodeName := temp;
            end
          else
            raise Exception.Create(Format('Invalid calibration key(%s).  Error on line# %d. Parameter # %d', [key, i + 1, j + 1]));
        end;
      end;

      if (not TempCalibrationTime.IsSamplingTime) and (not (TempCalibrationtime is TGroupCalibrationTime)) and (not (TempCalibrationTime is TIngroupRootCalibration)) then
      begin
        // Check to see that we have names for TaxonA and TaxonB if we have MRCA.
        if (TempCalibrationTime.NodeName = EmptyStr) and (TempCalibrationTime.NodeA = EmptyStr) then
          raise Exception.Create('No name for TaxonA was specified');

        if (TempCalibrationTime.NodeName = EmptyStr) and (TempCalibrationTime.NodeB = EmptyStr) then
          raise Exception.Create('No name for TaxonB was specified');
      end;

      SetLength(Result, Length(Result) + 1);
      TempCalibrationTime.Index := Length(Result);
      TempCalibrationTime.NumTaxa := FNumTaxa;
      case densityType of
        cddNormal:
          begin
            if (mean < 0) or (aStddev < 0) then
              raise Exception.Create('invalid params for normal distribution - need mean and stddev');
            TempCalibrationTime.SetCalibrationDensity(TCalibrationDensity.CreateNormalDist(mean, aStddev));
          end;
        cddLognormal:
          begin
            if (mean < 0) or (aStddev < 0) or (time < 0) then
              raise Exception.Create('invalid params for log-normal distribution - need offset, mean, and stddev');
            TempCalibrationTime.SetCalibrationDensity(TCalibrationDensity.CreateLogNormalDist(time, mean, aStddev));
          end;
        cddExponential:
          begin
            if (time < 0) or ((lambda < 0) and (mean < 0)) then
              raise Exception.Create('invalid params for exponential distribution - need time and decay');
            TempCalibrationTime.MinTime := -1;
            TempCalibrationTime.MaxTime := -1;
            if lambda > 0 then
              TempCalibrationTime.SetCalibrationDensity(TCalibrationDensity.CreateExponentialDistUsingLambda(time, lambda))
            else
              TempCalibrationTime.SetCalibrationDensity(TCalibrationDensity.CreateExponentialDistUsingMean(time, mean))
          end;
        cddUniform:
          begin
            if (minTime < 0) or (maxTime < 0) then
              raise Exception.Create('invalid params for uniform distribution - need minTime and maxTime');
            TempCalibrationTime.SetCalibrationDensity(TCalibrationDensity.CreateUniformDist(minTime, maxTime));
          end;
        cddNone:
          begin
            if (CompareValue(TempCalibrationTime.MinTime, 0, FP_CUTOFF) > 0) and (CompareValue(TempCalibrationTime.MaxTime, 0, FP_CUTOFF) > 0) THEN
            begin
              if CompareValue(TempCalibrationTime.MinTime, TempCalibrationTime.MaxTime, FP_CUTOFF) = 0 then
                TempCalibrationTime.CalibrationCategory := ccFixedTime
              else if CompareValue(TempCalibrationTime.MaxTime, TempCalibrationTime.MinTime, FP_CUTOFF) > 0 then
                TempCalibrationTime.CalibrationCategory := ccMinMaxTime;
            end;
          end;
      end;
      if TempCalibrationTime is TGroupCalibrationTime then
        if TempCalibrationTime.FMrcaName = EmptyStr then
          TempCalibrationTime.MrcaName := TempCalibrationTime.CalibrationName;
      Result[Length(Result) - 1] := TempCalibrationTime;
    finally
      if Assigned(SplitLine) then
        SplitLine.Free;;
    end;
  end;
end;

constructor TCalibrationReader.Create(NumTaxa: Integer);
begin
  FNumTaxa := NumTaxa;
  Message := EmptyStr;
end;

destructor TCalibrationReader.Destroy;
begin

  inherited Destroy;
end;

function TCalibrationReader.ReadCalibrationFile: TArrayOfCalibrationTime;
var
  CalibrationStrings: TStringlist = nil;
begin
  SetLength(Result, 0);
  if (InFile <> EmptyStr) and FileExists(InFile) then
  begin
    CalibrationStrings := TStringList.Create;
    try
      try
        CalibrationStrings.LoadFromFile(InFile);
        Result := ReadCalibrationStrings(CalibrationStrings);
      Except on E: Exception do
        {$IFDEF VISUAL_BUILD}
        // This isn't linked in visually yet.  For now pass the exception up the chain.
        raise Exception.Create(E.Message);
        {$ELSE}
        Error_NV('Reltime aborted.  Error in calibration file: ' + e.Message, E);
        {$ENDIF}
    end;
    finally
      if Assigned(CalibrationStrings) then
        CalibrationStrings.free;
    end;
  end;
end;

function TCalibrationReader.ReadCalibrationsFileToList(Filename: String; var Calibrations: TList): Boolean;
var
  TempCalibs: TArrayOfCalibrationTime;
  i: Integer;
begin
  Result := False;
  InFile := Filename;
  if not FileExists(InFile) then
    Exit;
  try
    TempCalibs := ReadCalibrationFile;
    if Length(TempCalibs) > 0 then
    begin
      for i := 0 to Length(TempCalibs) -1 do
        Calibrations.Add(TempCalibs[i]);
    end;
    Result := True;
  finally
    SetLength(TempCalibs, 0);
  end;
end;

function TCalibrationReader.ReadCalibrationsStringListToList(aList: TStringList; var Calibrations: TList): Boolean;
var
  TempCalibs: TArrayOfCalibrationTime;
  i: Integer;
begin
  Result := False;
  if aList.Count = 0 then
    Exit;
  try
    TempCalibs := ReadCalibrationStrings(aList);
    if Length(TempCalibs) > 0 then
    begin
      for i := 0 to Length(TempCalibs) -1 do
        Calibrations.Add(TempCalibs[i]);
    end;
    Result := True;
  finally
    SetLength(TempCalibs, 0);
  end;
end;

{ TCalibrationTime }

function TCalibrationTime.AsCalibrationString: String;
var
  MyLabel: String;
begin
  if MRCAName <> EmptyStr then
    MyLabel := #39 + MRCAName + #39
  else if NodeName <> EmptyStr then
    MyLabel := #39 + NodeName + #39
  else
  begin

    MyLabel := #39 + 'Node_' + IntToStr(GetTreeViewFormIndex) + #39;
  end;

  Result := '!MRCA=' + MyLabel + TaxaListString;
  if Assigned(FDensity) then
    Result := Result + ' ' + FDensity.AsCalibrationString
  else if (MinTime > 0.0) and (MaxTime > 0.0) and (CompareValue(MinTime, MaxTime, 0.00000001) <> EqualsValue) then
  begin
    Result := Result + ' MinTime=' + Format('%.8n', [MinTime]) + ' MaxTime=' + Format('%.8n', [MaxTime])
  end
  else if (MinTime > 0.0) and (MaxTime > 0.0) and (CompareValue(MaxTime, MinTime, 0.00000001) = EqualsValue) then
  begin
    Result := Result + ' Time=' + Format('%.8n', [MinTime]);
  end
  else if MinTime > 0 then
  begin
    Result := Result + ' MinTime=' + Format('%.8n', [MinTime])
  end
  else if MaxTime > 0 then
  begin
    Result := Result + ' MaxTime=' + Format('%.8n', [MaxTime])
  end;

  if FCalibrationName <> EmptyStr then
    Result := Result + ' calibrationName=' + #39 + CalibrationName + #39 + ';'
  else
    Result := Result + ';';
end;

function TCalibrationTime.TaxaListString: String;
begin
  Result := ' TaxonA=' + #39 + NodeA + #39 + ' TaxonB=' + #39 + NodeB + #39;
end;

function TCalibrationTime.AsSampleTimeString: String;
begin
  Result := Format('!Taxon=''%s'' Time=%.8n', [NodeA, MinTime]);
end;

function TCalibrationTime.AsShortString: String;
begin
  if IsSamplingTime then
    Result := AsSampleTimeString
  else
  begin
    Result := ' TaxonA=' + #39 + NodeA + #39 + ' TaxonB=' + #39 + NodeB + #39;
    if (MinTime > 0.0) and (MaxTime > 0.0) and (CompareValue(MinTime, MaxTime, 0.00000001) <> EqualsValue) then
    begin
      Result := Result + ' MinTime=' + Format('%.8n', [MinTime]) + ' MaxTime=' + Format('%.8n', [MaxTime])
    end
    else if (MinTime > 0.0) and (MaxTime > 0.0) and (CompareValue(MaxTime, MinTime, 0.00000001) = EqualsValue) then
    begin
      Result := Result + ' Time=' + Format('%.8n', [MinTime]);
    end
    else if MinTime > 0 then
    begin
      Result := Result + ' MinTime=' + Format('%.8n', [MinTime])
    end
    else if MaxTime > 0 then
    begin
      Result := Result + ' MaxTime=' + Format('%.8n', [MaxTime])
    end;

    if FCalibrationName <> EmptyStr then
      Result := Result + ' calibrationName=' + #39 + CalibrationName + #39 + ';'
    else
      Result := Result + ';';
  end;
end;

procedure TCalibrationTime.Assign(Other: TCalibrationTime);
var
  i: Integer;
begin
  Index              := Other.Index;
  FNodeID            := Other.FNodeID;
  MRCAName           := Other.MRCAName;
  NodeName           := Other.NodeName;
  NodeIDA            := Other.NodeIDA;
  NodeIDB            := Other.NodeIDB;
  NodeA              := Other.NodeA;
  NodeB              := Other.NodeB;
  NeedsMRCAResolved  := Other.NeedsMRCAResolved;
  NumTaxa            := Other.NumTaxa;
  CalibrationName    := Other.GetCalibrationName;
  FTreeViewFormNodeId := Other.TreeViewFormNodeId;
  IsValid            := Other.IsValid;
  IsSamplingTime     := Other.IsSamplingTime;
  IsSelected         := Other.IsSelected;
  FReltimeTreeIndexBLens := Other.FReltimeTreeIndexBLens;
  FCalibrationTarget := Other.FCalibrationTarget;
  FCalibrationCategory := Other.FCalibrationCategory;
  SetLength(FSamplingMinTimes, Length(Other.FSamplingMinTimes));
  SetLength(FSamplingMaxTimes, Length(Other.FSamplingMaxTimes));
  SetLength(FSamplingDivTimes, Length(Other.FSamplingDivTimes));
  if Length(FSamplingMinTimes) > 0 then
    for i := 0 to Length(FSamplingMinTimes) - 1 do
      FSamplingMinTimes[i] := Other.FSamplingMinTimes[i];
  if Length(FSamplingMaxTimes) > 0 then
    for i := 0 to Length(FSamplingMaxTimes) - 1 do
      FSamplingMaxTimes[i] := Other.FSamplingMaxTimes[i];
  if Length(FSamplingDivTimes) > 0 then
    for i := 0 to Length(FSamplingDivTimes) - 1 do
      FSamplingDivTimes[i] := Other.FSamplingDivTimes[i];

  if Assigned(Other.FDensity) then
  begin
    if not Assigned(FDensity) then
      FDensity := TCalibrationDensity.Create;
    FDensity.Assign(Other.FDensity);
  end
  else
  begin
    if Assigned(FDensity) then
      FreeAndNil(FDensity);
  end;
  if not Assigned(FDensity) then
  begin
    MinTime := Other.MinTime;
    MaxTime := Other.MaxTime;
  end;
end;

function TCalibrationTime.AsString: String;
begin
  if IsSamplingTime then
    Result := AsSampleTimeString
  else
    Result := AsCalibrationString;
end;

constructor TCalibrationTime.Create(IsSampleTime: Boolean = False);
begin
  FCalibrationCategory := ccNone;
  FOrigMinTime := -1;
  FOrigMaxTime := -1;
  FDensity := nil;
  MinTime            := -1.0;
  MaxTime            := -1.0;
  FNodeID             := -1;
  FMRCAName           := EmptyStr;
  FNodeName           := EmptyStr;
  NodeIDA            := -1;
  NodeIDB            := -1;
  NodeA              := EmptyStr;
  NodeB              := EmptyStr;
  FCalibrationName    := EmptyStr;
  Index              := -1;
  NeedsMRCAResolved  := False;
  FTreeViewFormNodeId := -1;
  IsSamplingTime     := IsSampleTime;
  IsValid            := True;
  IsSelected         := True;
  if IsSamplingTime then
    FCalibrationTarget := ctLeafNode
  else
  begin
    FCalibrationTarget   := ctInternalNode;
    SetLength(FSamplingMaxTimes, NUM_DEFAULT_CALIBRATION_SAMPLING_REPS);
    SetLength(FSamplingMinTimes, NUM_DEFAULT_CALIBRATION_SAMPLING_REPS);
    SetLength(FSamplingDivTimes, NUM_DEFAULT_CALIBRATION_SAMPLING_REPS);
  end;
  FNode := nil;
end;

destructor TCalibrationTime.Destroy;
begin
  if Assigned(FDensity) then
    FDensity.Free;
  SetLength(FSamplingMaxTimes, 0);
  SetLength(FSamplingMinTimes, 0);
  SetLength(FSamplingDivTimes, 0);
  FNode := nil;
  inherited Destroy;
end;

procedure TCalibrationTime.SetCalibrationDensity(aDensity: TCalibrationDensity);
begin
  if Assigned(FDensity) then
    FreeAndNil(FDensity);
  FDensity := aDensity;
  if Assigned(aDensity) then
    CalibrationCategory := DensityDistributToCalibrationCategory(aDensity.CalibrationDensityDistribution);
end;

function TCalibrationTime.GetCalibrationDensity: TCalibrationDensity;
begin
  Result := FDensity;
end;

procedure TCalibrationTime.RemoveCalibrationDensity;
begin
  if Assigned(FDensity) then
    FreeAndNil(FDensity);
end;

function TCalibrationTime.GetMaxTime: Double;
begin
  if Assigned(FDensity) then
    Result := FDensity.MaxTime
  else
    Result := FMaxTime;
end;

function TCalibrationTime.GetMinTime: Double;
begin
  if Assigned(FDensity) then
    Result := FDensity.MinTime
  else
    Result := FMinTime;
end;

function TCalibrationTime.GetNodeName: String;
begin
  Result := FNodeName;
end;

procedure TCalibrationTime.SetCalibrationCategory(AValue: TCalibrationCategory);
begin
  if FCalibrationCategory=AValue then Exit;
  FCalibrationCategory:=AValue;
end;

procedure TCalibrationTime.SetMaxTime(AValue: Double);
begin
  if Assigned(FDensity) then
    raise Exception.Create('cannot set max time when using a density distributiion');
  FMaxTime := AValue;
end;

procedure TCalibrationTime.SetMinTime(AValue: Double);
begin
  if Assigned(FDensity) then
    raise Exception.Create('cannot set min time when using a density distribution');
  FMinTime := aValue;
end;

procedure TCalibrationTime.SetMRCAName(AValue: String);
begin
  if FMRCAName <> AValue then
    FMRCAName := AValue;
end;

procedure TCalibrationTime.SetNode(AValue: TSimpleTreeNode);
begin
  if FNode=AValue then Exit;
  FNode:=AValue;
  FNode.MyObject := Self;
end;

procedure TCalibrationTime.SetNodeID(AValue: Integer);
begin
  if FNodeID = AValue then Exit;
  FNodeID := AValue;
end;

procedure TCalibrationTime.SetTreeViewFormNodeId(AValue: Integer);
begin
  if FTreeViewFormNodeId = AValue then Exit;
  FTreeViewFormNodeId := AValue;
end;

procedure TCalibrationTime.SetNodeName(AValue: String);
begin
  if FNodeName <> AValue then
    FNodeName := AValue;
end;

function TCalibrationTime.GetCalibrationName: String;
var
  AName: String;
begin
  if FCalibrationName = EmptyStr then
  begin
    if IsSamplingTime then
    begin
      AName := NodeA + '-sample_time';
      if Length(AName) <= 90 then
        Result := AName
      else
        Result := Mince(NodeA, 80) + '-sample_time';
    end
    else
    begin
      AName := NodeA + '-' + NodeB + '-split';
      if Length(AName) <= 90 then
        Result := AName
      else
        Result := Mince(NodeA, 40) + '-' + Mince(NodeB, 40) + '-split';
    end;
  end
  else
    Result := FCalibrationName;
end;

function TCalibrationTime.GetMrcaName: String;
var
  AName: String;
begin
  if FMRCAName <> EmptyStr then
    Result := FMRCAName
  else
  begin
    AName := NodeA + '-' + NodeB;
    if Length(AName) <= 90 then
      Result := AName
    else
      Result := Mince(NodeA, 40) + '-' + Mince(NodeB, 40);
  end;
end;

function TCalibrationTime.GetNodeLabel: String;
begin
  if FNodeName <> EmptyStr then
    Result := FNodeName
  else if FMRCAName <> EmptyStr then
    Result := FMRCAName
  else if FCalibrationName <> EmptyStr then
    Result := FCalibrationName
  else
    Result := MRCAName;
end;

function TCalibrationTime.GetRelTimeTreeIndex: Integer;
begin
  if IsSamplingTime then
  begin
    Result := FNodeID;
    Exit;
  end;

  if FNodeID < (NumTaxa - 1)then
    Result := NumTaxa + FNodeID
  else
    Result := FNodeID - NumTaxa + 1;
end;

function TCalibrationTime.GetTreeViewFormIndex: Integer;
begin
  if FNodeID = -1 then
  begin
    Result := -1;
    Exit;
  end;

  if TreeViewFormNodeId < 0 then
    Result := GetRelTimeTreeIndex + 1
  else
    Result := TreeViewFormNodeId;
end;

function TCalibrationTime.LoadFromFile(const mtsVersion: Integer; var SessionFile: File): Boolean;
var
  NumChars: Integer;
  Buffer: Array[0..4095] of AnsiChar;
  i: Integer;
begin
  Result := False;
  NumChars := 0;
  for i := 0 to Length(Buffer) - 1 do
    Buffer[i] := #0;

  if mtsVersion > 1005 then
    BlockRead(SessionFile, IsSamplingTime, SizeOf(IsSamplingTime));
  if mtsVersion > 606 then
  begin
    BlockRead(SessionFile, IsValid, SizeOf(IsValid));
    BlockRead(SessionFile, IsSelected, SizeOf(IsSelected));
  end;
  BlockRead(SessionFile, Index, SizeOf(Index));
  BlockRead(SessionFile, FMinTime, SizeOf(FMinTime));
  BlockRead(SessionFile, FMaxTime, SizeOf(FMaxTime));
  BlockRead(SessionFile, FNodeID, SizeOf(FNodeID));
  BlockRead(SessionFile, NodeIDA, SizeOf(NodeIDA));
  BlockRead(SessionFile, NodeIDB, SizeOf(NodeIDB));
  BlockRead(SessionFile, NeedsMRCAResolved, SizeOf(NeedsMRCAResolved));
  BlockRead(SessionFile, NumTaxa, SizeOf(NumTaxa));
  BlockRead(SessionFile, FTreeViewFormNodeId, SizeOf(FTreeViewFormNodeId));

  BlockRead(SessionFile, NumChars, SizeOf(Integer));
  if NumChars > Length(Buffer) then
    raise Exception.Create(Format('Application Error: invalid number of characters(%d) when loading calibration times', [NumChars]));
  if NumChars > 0 then
  begin
    BlockRead(SessionFile, Buffer, NumChars);
    Buffer[NumChars] := #0;
    MRCAName := StrPas(Buffer);
  end;

  BlockRead(SessionFile, NumChars, SizeOf(Integer));
  if NumChars > 0 then
  begin
    BlockRead(SessionFile, Buffer, NumChars);
    Buffer[NumChars] := #0;
    NodeName := StrPas(Buffer);
  end;

  BlockRead(SessionFile, NumChars, SizeOf(Integer));
  if NumChars > 0 then
  begin
    BlockRead(SessionFile, Buffer, NumChars);
    Buffer[NumChars] := #0;
    NodeA := StrPas(Buffer);
  end;

  BlockRead(SessionFile, NumChars, SizeOf(Integer));
  if NumChars > 0 then
  begin
    BlockRead(SessionFile, Buffer, NumChars);
    Buffer[NumChars] := #0;
    NodeB := StrPas(Buffer);
  end;

  BlockRead(SessionFile, NumChars, SizeOf(Integer));
  if NumChars > 0 then
  begin
    BlockRead(SessionFile, Buffer, NumChars);
    Buffer[NumChars] := #0;
    FCalibrationName := StrPas(Buffer);
  end;
  if mtsVersion >= 1002 then
  begin
    BlockRead(SessionFile, i, SizeOf(i));
    if i = 0 then
    begin
      FDensity := TCalibrationDensity.Create;
      FDensity.LoadFromFile(SessionFile);
    end;
    if mtsVersion >= 1004 then
      BlockRead(SessionFile, FCalibrationCategory, SizeOf(TCalibrationCategory));
  end;
  Result := True;
end;

function TCalibrationTime.GetSamplingMinTime(repNum: Integer): Extended;
begin
  Result := 0.0;
  if repNum < Length(FSamplingMinTimes) then
    Result := FSamplingMinTimes[repNum];
end;

function TCalibrationTime.GetSamplingMaxTime(repNum: Integer): Extended;
begin
  Result := 0.0;
  if repNum < Length(FSamplingMaxTimes) then
    Result := FSamplingMaxTimes[repNum];
end;

function TCalibrationTime.GetSamplingDivTime(repNum: Integer): Extended;
begin
  Result := 0.0;
  if repNum < Length(FSamplingDivTimes) then
    Result := FSamplingDivTimes[repNum];
end;

procedure TCalibrationTime.SetSamplingDivTime(repNum: Integer; divTime: Extended);
begin
  FSamplingDivTimes[repNum] := divTime;
end;

function TCalibrationTime.SamplingDataToList: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  Result.Add(Format('NodeID:       %d', [ReltimeTreeIndex + 1]));
  Result.Add(Format('OrigMinTime:  %.4f', [FOrigMinTime]));
  Result.Add(Format('OrigMaxTime:  %.4f', [FOrigMaxTime]));
  Result.Add(Format('FinalMinTime:      %.4f', [MinTime]));
  Result.Add(Format('FinalMaxTime:      %.4f', [MaxTime]));
  if Assigned(FDensity) then
  begin
    Result.Add(Format('Distribution: %s', [FDensity.DistributionNameString]));
    Result.Add('Params:');
    Result.Add(Format('%s', [FDensity.ParamsCaptionString]));
  end;

  Result.Add(EmptyStr);
  Result.Add('Calibration Density Sample Times:');
  Result.Add('Iteration,SampledMinTime,SampledMaxTime,ComputedDivTime');
  if Length(FSamplingMinTimes) > 0 then
    for i := 0 to Length(FSamplingMinTimes) - 1 do
      Result.Add(Format('%d,%.4f,%.4f,%.4f', [i + 1, FSamplingMinTimes[i], FSamplingMaxTimes[i], FSamplingDivTimes[i]]));
end;

function TCalibrationTime.SamplingDataToFile(filename: String): Boolean;
var
  aList: TStringList = nil;
begin
  try
    aList := SamplingDataToList;
    aList.SaveToFile(filename);
    Result := FileExists(filename);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TCalibrationTime.SamplingHistogramDataToFile(filename: String): Boolean;
var
  bins: TBinArray;
  allTimes: TDivTimesArray;
  histogram: TIntegerHistogram = nil;
  writer: THistogramWriter = nil;
  i: Integer;
begin
  try
    allTimes := ConcatenateDivTimeArrays(FSamplingMinTimes, FSamplingMaxTimes);
    histogram := TIntegerHistogram.Create;
    bins := histogram.GenerateHistogram(allTimes);
    writer := TCsvHistogramWriter.Create;
    Result := writer.WriteToFile(bins, filename);
  finally
    if Length(bins) > 0 then
      for i := Low(bins) to High(bins) do
        if Assigned(bins[i]) then
          bins[i].Free;
    if Assigned(histogram) then
      histogram.Free;
    if Assigned(writer) then
      writer.Free;
  end;
end;

procedure TCalibrationTime.DoCalibrationDensitySample(repNum: Integer);
var
  aMin: Extended = -1;
  aMax: Extended = -1;
begin
  if Assigned(FDensity) then
  begin
    FDensity.GetSampleTimesFromDistribution(aMin, aMax, repNum);
    FSamplingMinTimes[repNum] := aMin;
    FSamplingMaxTimes[repNum] := aMax;
  end
  else
  begin
    FSamplingMinTimes[repNum] := FMinTime;
    FSamplingMaxTimes[repNum] := FMaxTime;
  end;
end;

procedure TCalibrationTime.FinalizeCalibrationDensity;
var
  temp: TDivTimesArray = nil;
  i: Integer;
  minIndex, maxIndex: Integer;
begin
  if Assigned(FDensity) then
  begin
    FOrigMinTime := MinTime;
    FOrigMaxTime := MaxTime;
    SetLength(temp, Length(FSamplingDivTimes));
    for i := 0 to Length(temp) - 1 do
      temp[i] := FSamplingDivTimes[i];
    SortExtArray(temp, 0, Length(temp) - 1);
    minIndex := Round(Length(temp)*0.025) - 1;
    maxIndex := Round(Length(temp)*0.975) - 1;
    FDensity.FinalizedMinTime := temp[minIndex];
    FDensity.FinalizedMaxTime := temp[maxIndex]
  end;
end;

function TCalibrationTime.IsStatisticalDensity: Boolean;
begin
  Result := Assigned(FDensity);
end;

function TCalibrationTime.DebugString: String;
begin
  Result := Format('%8d %8d %8d %8d %20s %20s %20s %40s %8d %12d %8d %8d %30s %8.2f %8.2f %10s %10s %10s %30s', [Index, FNodeID, NodeIDA, NodeIDB, NodeA, NodeB, MrcaName, CalibrationName, ReltimeTreeIndex, ReltimeTreeIndexBLens, TreeViewFormIndex, TreeViewFormNodeId, NodeLabel, MinTime, MaxTime, BoolToStr(NeedsMRCAResolved, True), BoolToStr(IsValid, True), BoolToStr(IsSelected, True), NodeName]);
end;

class function TCalibrationTime.DebugHeaderString: String;
begin
  Result := Format('%8s %8s %8s %8s %20s %20s %20s %40s %8s %12s %8s %8s %30s %8s %8s %10s %10s %10s %30s', ['Index', 'NodeID', 'NodeIDA', 'NodeIDB', 'NodeA', 'NodeB', 'MrcaName', 'CalibName', 'Index', 'IndexBLens', 'TEIndex', 'TVFID', 'NodeLabel', 'MinTime', 'MaxTime', 'NeedsMRCA', 'Valid', 'Selected', 'NodeName']);
end;

function TCalibrationTime.MaxSessionString: Integer;
begin
  Result := Length(MRCAName);
  if Length(NodeName) > Result then
    Result := Length(NodeName);
  if Length(NodeA) > Result then
    Result := Length(NodeA);
  if Length(NodeB) > Result then
    Result := Length(NodeB);
  if Length(FCalibrationName) > Result then
    Result := Length(FCalibrationName);
end;

function TCalibrationTime.NeedsDescendentsResolved: Boolean;
begin
  Result := (NodeA = EmptyStr) and (NodeB = EmptyStr) and (NodeName <> EmptyStr);
end;

procedure TCalibrationTime.SetCalibrationName(const Value: String);
begin
  FCalibrationName := Value;
end;

procedure TCalibrationTime.SetCalibrationTarget(const Value: TCalibrationTarget);
begin
  FCalibrationTarget := Value;
  if FCalibrationTarget = ctLeafNode then
    FCalibrationCategory := ccFixedTime;
end;

procedure TCalibrationTime.SetNodeIdFromTreeViewFormId(TreeViewNodeId: Integer);
begin
  TreeViewFormNodeId := TreeViewNodeId;
  NodeID := TreeViewNodeId - 1;
  if not IsSamplingTime then
  begin
    if NodeID >= NumTaxa then
      NodeID := NodeID - NumTaxa
    else
      NodeID := NodeID + NumTaxa -1;
  end
  else
  begin
    NodeIDA := NodeID;
    NodeIDB := NodeID;
  end;
end;

procedure TCalibrationTime.SetTaxonAFromTreeViewFormId(TreeViewNodeId: Integer);
var
  TempId: Integer;
begin
  TempId := TreeViewNodeId - 1;
  if not IsSamplingTime then
    if TempId >= NumTaxa then
      TempId := TempId - NumTaxa
    else
      TempId := TempId + NumTaxa -1;
  NodeIdA := TempId;
end;

procedure TCalibrationTime.SetTaxonBFromTreeViewFormId(TreeViewNodeId: Integer);
var
  TempId: Integer;
begin
  TempId := TreeViewNodeId - 1;
  if not IsSamplingTime then
    if TempId >= NumTaxa then
      TempId := TempId - NumTaxa
    else
      TempId := TempId + NumTaxa -1;
  NodeIdB := TempId;
end;

function TCalibrationTime.WriteToFile(var SessionFile: File): Boolean;
var
  NumChars: Integer;
  Buffer: Array[0..4095] of AnsiChar;
  i: Integer;
begin
  Result := False;
  try
    BlockWrite(SessionFile, IsSamplingTime, SizeOf(IsSamplingTime));
    BlockWrite(SessionFile, IsValid, SizeOf(IsValid));
    BlockWrite(SessionFile, IsSelected, SizeOf(IsSelected));
    BlockWrite(SessionFile, Index, SizeOf(Index));
    BlockWrite(SessionFile, FMinTime, SizeOf(FMinTime));
    BlockWrite(SessionFile, FMaxTime, SizeOf(FMaxTime));
    BlockWrite(SessionFile, FNodeID, SizeOf(FNodeID));
    BlockWrite(SessionFile, NodeIDA, SizeOf(NodeIDA));
    BlockWrite(SessionFile, NodeIDB, SizeOf(NodeIDB));
    BlockWrite(SessionFile, NeedsMRCAResolved, SizeOf(NeedsMRCAResolved));
    BlockWrite(SessionFile, NumTaxa, SizeOf(NumTaxa));
    BlockWrite(SessionFile, TreeViewFormNodeId, SizeOf(TreeViewFormNodeId));

    NumChars := Length(MRCAName);
    BlockWrite(SessionFile, NumChars, SizeOf(Integer));
    if NumChars > 0 then
    begin
      StrPCopy(Buffer, MRCAName);
      BlockWrite(SessionFile, Buffer, NumChars);
    end;

    NumChars := Length(NodeName);
    BlockWrite(SessionFile, NumChars, SizeOf(Integer));
    if NumChars > 0 then
    begin
      StrPCopy(Buffer, NodeName);
      BlockWrite(SessionFile, Buffer, NumChars);
    end;

    NumChars := Length(NodeA);
    BlockWrite(SessionFile, NumChars, SizeOf(Integer));
    if NumChars > 0 then
    begin
      StrPCopy(Buffer, NodeA);
      BlockWrite(SessionFile, Buffer, NumChars);
    end;

    NumChars := Length(NodeB);
    BlockWrite(SessionFile, NumChars, SizeOf(Integer));
    if NumChars > 0 then
    begin
      StrPCopy(Buffer, NodeB);
      BlockWrite(SessionFile, Buffer, NumChars);
    end;

    NumChars := Length(FCalibrationName);
    BlockWrite(SessionFile, NumChars, SizeOf(Integer));
    if NumChars > 0 then
    begin
      StrPCopy(Buffer, FCalibrationName);
      BlockWrite(SessionFile, Buffer, NumChars);
    end;
    if Assigned(FDensity) then
    begin
      i := 0;
      BlockWrite(SessionFile, i, SizeOf(i));
      FDensity.WriteToFile(SessionFile);
    end
    else
    begin
      i := 1;
      BlockWrite(SessionFile, i, SizeOf(i));
    end;
    BlockWrite(SessionFile, FCalibrationCategory, SizeOf(TCalibrationCategory));
    Result := True;
  except
    Result := False;
  end;
end;


procedure TCalibrations.GetCalibrationTypes(var HasDivTimes: Boolean; var HasSampleTimes: Boolean);
var
  i: Integer;
  Calibrations: TArrayOfCalibrationTime;
begin
  HasDivTimes := False;
  HasSampleTimes := False;

  try
    Calibrations := GetSelectedCalibrationsArray;
    for i := 0 to Length(Calibrations) - 1 do
    begin
      if Calibrations[i].IsSamplingTime then
        HasSampleTimes := True
      else
        HasDivTimes := True;
    end;
  finally
    for i := 0 to Length(Calibrations) - 1 do
      Calibrations[i].Free;
    SetLength(Calibrations, 0);
  end;
end;

function TCalibrations.GetExpandedCalibrationsList: TStringList;
var
  AValidator: TCalibrationsValidator;
  AData: TTreeData;
  TempCalibs: TArrayOfCalibrationTime;
  NamesList: TStringList;
begin
  AValidator := TCalibrationsValidator.Create;
  AData := TTreeList(FTreeList)[0];
  TempCalibs := GetSelectedCalibrationsArray;
  NamesList := TTreeList(FTreeList).OTUNameList;
  Result := AValidator.PropogateCalibrationsOverAllNodes(AData, NamesList, TempCalibs);
  AValidator.Free;
end;

procedure TCalibrations.GetCounts(var numSelected: Integer; var numValid: Integer; var numInvalid: Integer);
var
  i: Integer;
  c: TCalibrationTime;
begin
  numSelected := 0;
  numValid := 0;
  numInvalid := 0;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      c := GetCalibration(i);
      if c.IsSelected then
        inc(numSelected);
      if c.IsValid then
        inc(numValid)
      else
        inc(numInvalid);
    end;
end;

procedure TCalibrations.PrepareSamplingTimeArray(var SamplingTimes: TDivTimesArray; const NamesList: TStringList);
var
  i: Integer;
  TempIndex: Integer;
  Calibrations: TArrayOfCalibrationTime;
begin
  try
    Calibrations := GetSelectedCalibrationsArray;
    SetLength(SamplingTimes, NamesList.Count);

    for i := Low(SamplingTimes) to High(SamplingTimes) do
      SamplingTimes[i] := 0.0;

    for i := Low(Calibrations) to High(Calibrations) do
    begin
      if (not Calibrations[i].IsSamplingTime) or (not Calibrations[i].IsValid) or (not Calibrations[i].IsSelected) then
        continue;

      TempIndex := NamesList.IndexOf(Calibrations[i].NodeName);
      if Calibrations[i].MinTime > 0 then
        SamplingTimes[TempIndex] := Calibrations[i].MinTime;
    end;
  finally
    for i := 0 to Length(Calibrations) - 1 do
      Calibrations[i].Free;
  end;
end;

function TCalibrations.Prevalidate(var Msg: String): Boolean;
var
  TempMsg: String;
  TempResult: Boolean;
  aInfo: TAnalysisInfo = nil;
begin
  Result := True;
  TempMsg := EmptyStr;
  Msg := EmptyStr;

  TempResult := ValidateMinMaxTimes(TempMsg);
  if not TempResult then
  begin
    Msg :=  Msg + TempMsg + LineEnding;
    Result := False;
  end;
  aInfo := TAnalysisInfo(FAnalysisInfo);
  TempResult := ValidateAndFilterOutgroupConstraint(TempMsg, IsSampleTimes, aInfo.MyOtuNames);
  if not TempResult then
  begin
    Msg :=  Msg + TempMsg;
    Result := False;
  end;

  Msg := Trim(Msg);
end;

procedure TCalibrations.PrepareDivTimeArrays(var MinTimes, MaxTimes: TDivTimesArray;
                                                  NumTaxa: Integer;
                                                  IsBLensOnly: Boolean;
                                                  var aNodeLabelList: TStringList;
                                                  indexNeedsChecked: Boolean = True);
var
  i: Integer;
  TempIndex: Integer;
  Calibrations: TArrayOfCalibrationTime;
  aCalib: TCalibrationTime;
begin
  try
    SetLength(MinTimes, 2*NumTaxa - 1);
    SetLength(MaxTimes, 2*NumTaxa - 1);

    for i := Low(MinTimes) to High(MinTimes) do
    begin
      MinTimes[i] := 0.0;
      MaxTimes[i] := 0.0;
    end;
    Calibrations := GetSelectedCalibrationsArray;
    for i := Low(Calibrations) to High(Calibrations) do
    begin
      aCalib := Calibrations[i];
      if (not aCalib.IsValid) or (not aCalib.IsSelected) then
        continue;
      if not indexNeedsChecked then
        TempIndex := aCalib.NodeID
      else if IsBLensOnly then
        TempIndex := aCalib.RelTimeTreeIndexBLens
      else if aCalib.IsSamplingTime then
        TempIndex := aCalib.NodeID
      else
        TempIndex := aCalib.RelTimeTreeIndex;

      if aCalib.MinTime > 0 then
        MinTimes[TempIndex] := aCalib.MinTime;
      if aCalib.MaxTime > 0 then
        MaxTimes[TempIndex] := aCalib.MaxTime;
      if Assigned(aNodeLabelList) and (aCalib.NodeLabel <> EmptyStr) then
      begin
        if TempIndex >= NumTaxa then
          TempIndex :=  TempIndex - NumTaxa;
        aNodeLabelList[TempIndex] := aCalib.NodeLabel;
      end;
    end;
  finally
    for i := 0 to Length(Calibrations) - 1 do
      Calibrations[i].Free;
  end;
end;

procedure TCalibrations.PrepareDivTimeArrays(var MinTimes, MaxTimes: TDivTimesArray; NumTaxa: Integer; IsBLensOnly: Boolean; ingroupRootIndex: Integer; indexNeedsChecked: Boolean);
var
  i: Integer;
  TempIndex: Integer;
  Calibrations: TArrayOfCalibrationTime;
  aCalib: TCalibrationTime;
begin

  try
  SetLength(MinTimes, 2*NumTaxa - 1);
  SetLength(MaxTimes, 2*NumTaxa - 1);

  for i := Low(MinTimes) to High(MinTimes) do
  begin
    MinTimes[i] := 0.0;
    MaxTimes[i] := 0.0;
  end;
  Calibrations := GetSelectedCalibrationsArray;
  for i := Low(Calibrations) to High(Calibrations) do
  begin
    aCalib := Calibrations[i];
    if (not aCalib.IsValid) or (not aCalib.IsSelected) then
      continue;
    if aCalib is TIngroupRootCalibration then
      TempIndex := ingroupRootIndex
    else if not indexNeedsChecked then
      TempIndex := aCalib.NodeID
    else if IsBLensOnly then
      TempIndex := aCalib.RelTimeTreeIndexBLens
    else if aCalib.IsSamplingTime then
      TempIndex := aCalib.NodeID
    else
      TempIndex := aCalib.RelTimeTreeIndex;

    if aCalib.MinTime > 0 then
      MinTimes[TempIndex] := aCalib.MinTime;
    if aCalib.MaxTime > 0 then
      MaxTimes[TempIndex] := aCalib.MaxTime;
  end;
  finally
    for i := 0 to Length(Calibrations) - 1 do
      Calibrations[i].Free;
  end;
end;

procedure TCalibrations.PrepareSamplingDivTimeArrays(var minTimes,
                                                          maxTimes: TDivTimesArray;
                                                          NumTaxa: Integer;
                                                          IsBLensOnly: Boolean;
                                                          repNum: Integer);
var
  i: Integer;
  TempIndex: Integer;
  Calibrations: TArrayOfCalibrationTime;
  aCalib: TCalibrationTime;
begin
  try
  SetLength(MinTimes, 2 * NumTaxa - 1);
  SetLength(MaxTimes, 2 * NumTaxa - 1);

  for i := Low(MinTimes) to High(MinTimes) do
  begin
    MinTimes[i] := 0.0;
    MaxTimes[i] := 0.0;
  end;
  Calibrations := GetSelectedCalibrationsArray;
  for i := Low(Calibrations) to High(Calibrations) do
  begin
    aCalib := Calibrations[i];
    if aCalib.IsValid and aCalib.IsSelected then
    begin
      if IsBLensOnly then
        TempIndex := aCalib.RelTimeTreeIndexBLens
      else
        TempIndex := aCalib.RelTimeTreeIndex;
      MinTimes[TempIndex] := aCalib.GetSamplingMinTime(repNum);
      MaxTimes[TempIndex] := aCalib.GetSamplingMaxTime(repNum);
    end;
  end;
  finally
    for i := 0 to Length(Calibrations) - 1 do
      Calibrations[i].Free;
  end;
end;

procedure TCalibrations.PrepareSamplingTimeArray(var SamplingTimes: TDivTimesArray; var TempCalibs: TArrayOfCalibrationTime; const NamesList: TStringList);
var
  i: Integer;
  TempIndex: Integer;
  Calibrations: TArrayOfCalibrationTime;
  calib: TCalibrationTime;
begin
  try
    Calibrations := GetSelectedCalibrationsArray;
    SetLength(SamplingTimes, NamesList.Count);
    SetLength(TempCalibs, NamesList.Count);
    for i := Low(SamplingTimes) to High(SamplingTimes) do
    begin
      SamplingTimes[i] := 0.0;
      TempCalibs[i] := nil;
    end;
    for i := Low(Calibrations) to High(Calibrations) do
    begin
      if (not Calibrations[i].IsSamplingTime) or (not Calibrations[i].IsValid) or (not Calibrations[i].IsSelected) then
        continue;

      TempIndex := NamesList.IndexOf(Calibrations[i].NodeName);
      if (CompareValue(Calibrations[i].MinTime, 0.0) > 0) and (TempIndex >= 0) then
      begin
        SamplingTimes[TempIndex] := Calibrations[i].MinTime;
        calib := TCalibrationTime.Create(True);
        calib.Assign(Calibrations[i]);
        TempCalibs[TempIndex] := calib;
      end;
    end;
  finally
    for i := 0 to Length(Calibrations) - 1 do
      Calibrations[i].Free;
  end;
end;

end.
