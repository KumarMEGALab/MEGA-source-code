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

unit manalysissettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fgl, MegaConsts;
Type

TSettingType = (stInteger, stFloat, stString, stPicklist, stPickListValue, stMultiPickList, stNone);

{ TAnalysisSetting }

TAnalysisSetting = class abstract(TObject)
  private
    function GetIsApplicable: Boolean;
  protected
    FIsAdaptive: Boolean;
    FHint: String;
    FIsVisible: Boolean;
    FIsAdvancedSetting: Boolean;
    FIsApplicable: Boolean;
    FIsReadOnly: Boolean;
    FIsChecked: Boolean;
    FIsOptional: Boolean;
    FSettingType: TSettingType;
    FName: String;
    FIndentLevel: Integer;
    FShortCmdFlag: String;
    FLongCmdFlag: String;
    FDescription: String;
    FDisplayName: String;
    function GetIsAdvancedSetting: Boolean;
    function GetIsReadOnly: Boolean;
    function GetIsVisible: Boolean;
    procedure SetIsAdaptive(AValue: Boolean);
    procedure SetIsAdvancedSetting(AValue: Boolean);
    procedure SetIsVisible(AValue: Boolean);
    procedure SetSettingType(AValue: TSettingType);
    procedure SetDescription(AValue: String);
    procedure SetDisplayName(AValue: String);
    function GetLongCmdFlag: String; virtual;
    function GetShortCmdFlag: String; virtual;
    procedure SetShortCmdFlag(AValue: String);
    procedure SetLongCmdFlag(AValue: String);
    procedure SetIsApplicable(AValue: Boolean);
    procedure SetIsReadOnly(AValue: Boolean);
    procedure SetIsChecked(AValue: Boolean);
    procedure SetIsOptional(AValue: Boolean);
    procedure SetName(AValue: String);
    function GetStringValue: String; virtual; abstract;
    function GetSettingType: TSettingType;
  public
    constructor Create(AIndentLevel: Integer); virtual;
    destructor Destroy; override;
    function GetClone: TAnalysisSetting; virtual; abstract;
    function SetValueFromStr(AValue: String): Boolean; virtual; abstract;
    function GetDisplayName: String; { sets the left indent according to the indent level}
    function NameValueString: String;
    function MaoString(FormatString: String): String;
    function StoredValueString: String; virtual; abstract;
    function GetJson: TJsonObject; virtual;
    function LoadJson(AJson: String): Boolean; virtual;
    function GetPrettyString(NameWidth: Integer): String;
    //procedure Assign(Source: TAnalysisSetting); virtual; abstract;
    //procedure AssignValues(Source: TAnalysisSetting); virtual; abstract;
    procedure SetState(AIsReadOnly: Boolean; AIsApplicable: Boolean);
    property Name: String read FName write SetName;
    property DisplayName: String read GetDisplayName write SetDisplayName;
    property Description: String read FDescription write SetDescription;
    property StringValue: String read GetStringValue;
    property SettingType: TSettingType read GetSettingType; // write SetSettingType;
    property IndentLevel: Integer read FIndentLevel write FIndentLevel;
    property IsReadOnly: Boolean read GetIsReadOnly write SetIsReadOnly;
    property IsApplicable: Boolean read GetIsApplicable write SetIsApplicable;
    property IsChecked: Boolean read FIsChecked write SetIsChecked;
    property IsOptional: Boolean read FIsOptional write SetIsOptional;
    property ShortCmdFlag: String read GetShortCmdFlag write SetShortCmdFlag; { short command-line flag (e.g. -v), used by applinker}
    property LongCmdFlag: String read GetLongCmdFlag write SetLongCmdFlag; { long command-line flag (e.g. --version), used by applinker}
    property IsVisible: Boolean read GetIsVisible write SetIsVisible;
    property IsAdvancedSetting: Boolean read GetIsAdvancedSetting write SetIsAdvancedSetting;
    property Hint: String read FHint write FHint;
    property IsAdaptive: Boolean read FIsAdaptive write SetIsAdaptive;
end;

{ TStringAnalysisSetting }

TStringAnalysisSetting = class(TAnalysisSetting)
  private
    FStringValue: String;
    procedure SetStringValue(AValue: String);
  protected
    function GetStringValue: String; override;
  public
    constructor Create(AIndentLevel: Integer); override;
    destructor Destroy; override;
    function GetJson: TJsonObject; override;
    function LoadJson(AJson: String): Boolean; override;
    function StoredValueString: String; override;
    function GetClone: TStringAnalysisSetting; override;
    function SetValueFromStr(AValue: String): Boolean; override;
    procedure Assign(Source: TStringAnalysisSetting);
    procedure AssignValues(Source: TStringAnalysisSetting);
    procedure Update(AName: String; AValue: String);
    property Value: String read GetStringValue write SetStringValue;
end;

{ TIntegerAnalysisSetting }

TIntegerAnalysisSetting = class(TAnalysisSetting)
  private

  protected
    FGivenValueString: String;
    FValue: Integer;
    FIncrement: Integer;
    FMax: Integer;
    FMin: Integer;
    function GetStringValue: String; override;
    procedure SetIncrement(AValue: Integer);
    procedure SetMax(AValue: Integer);
    procedure SetMin(AValue: Integer);
    procedure SetIntValue(AValue: Integer);
  public
    constructor Create(AIndentLevel: Integer); override;
    destructor Destroy; override;
    function GetJson: TJsonObject; override;
    function LoadJson(AJson: String): Boolean; override;
    function IsValidValue(aVal: Integer): Boolean;
    function StoredValueString: String; override;
    function GetClone: TIntegerAnalysisSetting; override;
    function SetValueFromStr(AValue: String): Boolean; override;
    procedure Assign(Source: TIntegerAnalysisSetting);
    procedure AssignValues(Source: TIntegerAnalysisSetting);
    property Value: Integer read FValue write SetIntValue;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property Increment: Integer read FIncrement write SetIncrement;
end;

{ TFloatAnalysisSetting }

TFloatAnalysisSetting = class(TAnalysisSetting)
  private

  protected
    FIncrement: Double;
    FMax: Double;
    FMin: Double;
    FPrecision: Integer;
    FValue: Double;
    procedure SetIncrement(AValue: Double);
    procedure SetMax(AValue: Double);
    procedure SetMin(AValue: Double);
    procedure SetPrecision(AValue: Integer);
    procedure SetFloatValue(AValue: Double);
    function GetStringValue: String; override;

  public
    constructor Create(AIndentLevel: Integer); override;
    destructor Destroy; override;
    function GetJson: TJsonObject; override;
    function LoadJson(AJson: String): Boolean; override;
    function IsValidValue(aVal: Double): Boolean;
    function StoredValueString: String; override;
    function GetClone: TFloatAnalysisSetting; override;
    procedure Assign(Source: TFloatAnalysisSetting);
    procedure AssignValues(Source: TFloatAnalysisSetting);
    function SetValueFromStr(AValue: String): Boolean; override;
    property Value: Double read FValue write SetFloatValue;
    property Min: Double read FMin write SetMin;
    property Max: Double read FMax write SetMax;
    property Increment: Double read FIncrement write SetIncrement;
    property Precision: Integer read FPrecision write SetPrecision;
end;

{ TPickListValueSetting }

TPickListValueSetting = class(TStringAnalysisSetting)
  public
    constructor Create(aIndentLevel: Integer); override;
    function GetClone: TPickListValueSetting; override;
    procedure Assign(Source: TPickListValueSetting);
end;

{ TPickListAnalysisSetting }

TPickListAnalysisSetting = class(TAnalysisSetting)
  private
    FItemIndex: Integer;
    FPickListName: String;
    FPickListString: String;
    FShortCmdFlagPickList: String;
    FLongCmdFlagPickList: String;
    FPickList: TStringList;
    FShortCmdPickList: TStringList;
    FLongCmdPickList: TStringList;
    FValue: String;
    FValueSetting: TPickListValueSetting;
    function GetDefaultValue: String;
    function GetIsPicklist: Boolean;
    procedure SetItemIndex(AValue: Integer);
    procedure SetLongCmdPickList(AValue: String);

    procedure SetShortCmdPickList(AValue: String);
    procedure SetStringValue(AValue: String);
    procedure SetValueSetting(AValue: TPickListValueSetting);
  protected
    procedure SetPicklist(AValue: String); virtual;
    function GetStringValue: String; override;
    function GetLongCmdFlag: String; override;
    function GetShortCmdFlag: String; override;
  public
    constructor Create(AIndentLevel: Integer; aPickListName: String); reintroduce;
    destructor Destroy; override;
    function GetJson: TJsonObject; override;
    function LoadJson(AJson: String): Boolean; override;
    function StoredValueString: String; override;
    function GetClone: TPickListAnalysisSetting; override;
    procedure Assign(Source: TPickListAnalysisSetting);
    procedure AssignValues(Source: TPickListAnalysisSetting);
    procedure Update(AName: String; AList: String);
    function SetValueFromStr(AValue: String): Boolean; override;
    function PickListStrings: TStringList;
    function GetShortCmdValue: String;
    function GetLongCmdValue: String; virtual;
    function HasValue(aValue: String): Boolean;
    procedure RemoveValue(aValue: String);
    function ValidatePickList: Boolean;
    property PickList: String read FPickListString write SetPicklist;
    property IsPicklist: Boolean read GetIsPicklist;
    property LongCmdPickList: String read FLongCmdFlagPickList write SetLongCmdPickList;
    property ShortCmdPickList: String read FShortCmdFlagPickList write SetShortCmdPickList;
    property DefaultValue: String read GetDefaultValue;
    property Value: String read FValue write SetStringValue;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property PickListName: String read FPickListName write FPickListName;
    property ValueSetting: TPickListValueSetting read FValueSetting write SetValueSetting;
end;

{ TMultiPickListSetting }

TMultiPickListSetting = class(TPickListAnalysisSetting)
  private
    FCheckStates: TArrayOfBoolean;
    function GetChecked(index: Integer): Boolean;
    procedure SetChecked(index: Integer; AValue: Boolean);
  protected
    procedure SetPicklist(AValue: String); override;
    function GetStringValue: String; override;
    function GetLongCmdFlag: String; override;
  public
    constructor Create(AIndentLevel: Integer; aPickListName: String);
    function GetLongCmdValue: String; override;
    function LoadJson(AJson: String): Boolean; override;
    property Checked[index: Integer]: Boolean read GetChecked write SetChecked;
end;

TAnalysisSettingsList = specialize TFPGList<TAnalysisSetting>;
TAppSettingsLoadedProc = procedure (aSettings: TAnalysisSettingsList) of object;
TValidateSettingsFunc = function (const aSettings: TStringList; var msg: String): Boolean of object;

function GetSettingFromJson(AJson: String): TAnalysisSetting;
function LengthOfLongestNameString(AList: TAnalysisSettingsList): Integer; overload;
function LengthOfLongestNameString(AList: TList): Integer; overload;

implementation

uses
  MegaAnalysisPrefStrings, typinfo, StringUtils, math;

function GetSettingFromJson(AJson: String): TAnalysisSetting;
var
  AParser: TJSONParser;
  Json: TJSONObject;
  AData: TJSONData;
  ASetting: TAnalysisSetting;
  AType: TSettingType;
  ALevel: Integer;
  AChecked: Boolean;
begin
  Result := nil;
  AParser := nil;
  Json := nil;
  AData := nil;

  try
    AParser := TJSONParser.Create(AJson, []);
    AData := AParser.Parse;

    if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
      raise Exception.Create('Failed to parse JSON file');
    Json := TJSONObject(AData);

    AData := Json.Find('FSettingType', jtString);
    if not Assigned(AData) then
      raise Exception.Create('failed to parse JSON settings');
    AType := TSettingType(GetEnumValue(TypeInfo(TSettingType), AData.Value));

    AData := Json.Find('FIndentLevel', jtNumber);
    if not Assigned(AData) then
      raise Exception.Create('failed to parse JSON settings');
    ALevel := AData.Value;

    AData := Json.Find('FChecked', jtBoolean);
    if Assigned(AData) then
      AChecked := AData.Value;

    case AType of
      stInteger:
        begin
          ASetting := TIntegerAnalysisSetting.Create(ALevel);
          if not ASetting.LoadJson(AJson) then
            raise Exception.Create('Failed to load analysis setting from JSON');
        end;
      stFloat:
        begin
          ASetting := TFloatAnalysisSetting.Create(ALevel);
          if not ASetting.LoadJson(AJson) then
            raise Exception.Create('Failed to load analysis setting from JSON');
        end;
      stString:
        begin
          ASetting := TStringAnalysisSetting.Create(ALevel);
          if not ASetting.LoadJson(AJson) then
            raise Exception.Create('Failed to load analysis setting from JSON');
        end;
      stPicklist:
        begin
          ASetting := TPicklistAnalysisSetting.Create(ALevel, EmptyStr);
          if not ASetting.LoadJson(AJson) then
            raise Exception.Create('Failed to load analysis setting from JSON');
        end;
      stMultiPickList:
        begin
          ASetting := TMultiPickListSetting.Create(ALevel, EmptyStr);
          if not ASetting.LoadJson(AJson) then
            raise Exception.Create('Failed to load analysis setting from JSON');
        end;
      stPickListValue:
        begin
          ASetting := TPickListValueSetting.Create(ALevel);
          if not ASetting.LoadJson(AJson) then
            raise Exception.Create('Failed to load analysis setting from JSON');
        end
      else
      begin
        Assert(False, 'missing handler for TSettingType');
      end;
      if (AChecked=True) then
        ASetting.IsChecked:=True;
    end;
    Result := ASetting;
  finally
    if Assigned(AParser) then
      AParser.Free;
    if Assigned(Json) then
      Json.Free;
  end;
end;

function LengthOfLongestNameString(AList: TAnalysisSettingsList): Integer;
var
  i: Integer;
begin
  Result := 0;
  if AList.Count = 0 then
    Exit;
  for i := 0 to AList.Count - 1 do
  begin
    if Length(AList[i].Name) > Result then
      Result := Length(AList[i].Name);
  end;
end;

function LengthOfLongestNameString(AList: TList): Integer;
var
  i: Integer;
begin
  Result := 0;
  if AList.Count = 0 then
    Exit;
  for i := 0 to AList.Count - 1 do
  begin
    if Length(TAnalysisSetting(AList[i]).Name) > Result then
      Result := Length(TAnalysisSetting(AList[i]).Name);
  end;
end;

{ TMultiPickListSetting }

function TMultiPickListSetting.GetChecked(index: Integer): Boolean;
begin
  Result := False;
  if (Length(FCheckStates) > 0) and (index >= Low(FCheckStates)) and (index <= High(FCheckStates)) then
    Result := FCheckStates[index];
end;

procedure TMultiPickListSetting.SetChecked(index: Integer; AValue: Boolean);
begin
    if (index >= Low(FCheckStates)) and (index <= High(FCheckStates)) then
      FCheckStates[index] := AValue
    else
      raise Exception.Create(Format('Application error: index (%d) out of bounds', [index]));
end;

procedure TMultiPickListSetting.SetPicklist(AValue: String);
var
  i: Integer;
begin
  inherited SetPicklist(AValue);
  SetLength(FCheckStates, FPickList.Count);
  if Length(FCheckStates) > 0 then
    for i := Low(FCheckStates) to High(FCheckStates) do
      FCheckStates[i] := False;
end;

function TMultiPickListSetting.GetStringValue: String;
var
  i: Integer;
begin
  Result := EmptyStr;
  if not IsApplicable then
    Result := NotApplicableStr
  else
  begin
    if FPickList.Count > 0 then
    begin
      Assert(FPickList.Count = Length(FCheckStates));
      for i := Low(FCheckStates) to High(FCheckStates) do
        if FCheckStates[i] then
        begin
          if Result <> EmptyStr then
            Result := Result + ',';
          Result := Result + FPickList[i];
        end;
    end;
  end;
end;

function TMultiPickListSetting.GetLongCmdFlag: String;
var
  i: Integer;
begin
  Assert(Length(FCheckStates) = FPickList.Count);
  Result := EmptyStr;
  if Length(FCheckStates) > 0 then
    for i := Low(FCheckStates) to High(FCheckStates) do
      if FCheckStates[i] then
      begin
        if Result <> EmptyStr then
          Result += ',';
        Result += FPickList[i];
      end;
end;

constructor TMultiPickListSetting.Create(AIndentLevel: Integer;
  aPickListName: String);
begin
  inherited Create(AIndentLevel, aPickListName);
  FSettingType := stMultiPickList;
end;

function TMultiPickListSetting.GetLongCmdValue: String;
begin
  Result := GetStringValue;
end;

function TMultiPickListSetting.LoadJson(AJson: String): Boolean;
begin
  Result := inherited LoadJson(AJson);
  SetPickList(FPickListString);
  SetLength(FCheckStates, FPickList.Count);
end;

{ TPickListValueSetting }

constructor TPickListValueSetting.Create(aIndentLevel: Integer);
begin
  inherited Create(aIndentLevel);
  FSettingType := stPickListValue;
end;

function TPickListValueSetting.GetClone: TPickListValueSetting;
begin
  Result := TPickListValueSetting.Create(FIndentLevel);
  Result.Assign(Self);
end;

procedure TPickListValueSetting.Assign(Source: TPickListValueSetting);
begin
  Value := Source.Value;
  FIndentLevel := Source.IndentLevel;
  Name := Source.Name;
  if FSettingType <> Source.SettingType then
    raise Exception.Create('invalid setting type');
  FIsReadOnly := Source.IsReadOnly;
  FIsApplicable := Source.IsApplicable;
  FIsChecked := Source.IsChecked;
  FIsOptional := Source.IsOptional;
end;


{ TPickListAnalysisSetting }

function TPickListAnalysisSetting.GetDefaultValue: String;
var
  AList: TStringList;
begin
  AList := nil;
  Result := EmptyStr;
  if FPickListString = EmptyStr then
    Exit;
  if FPickListString = StringReplace(DistNucPickList, '"', '', [rfReplaceAll]) then
    Result := Model_MCLStr
  else if FPickListString = StringReplace(DistAminoPickList, '"', '', [rfReplaceAll]) then
    Result := Model_PoissonStr
  else if FPickListString = StringReplace(MLNucPickList, '"', '', [rfReplaceAll]) then
    Result := Model_Tamura_Nei_ParaStr
  else if FPickListString = StringReplace(DistSynNonsynPickList, '"', '', [rfReplaceAll]) then
    Result := Model_NeiGojoboriJCDistStr
  else if FPickListString = StringReplace(DistFisherSelTestModelPickList, '"', '', [rfReplaceAll]) then
    Result := Model_NeiGojoboriPDistStr
  else if FPickListString = StringReplace(DistSynNonsynPickList, '"', '', [rfReplaceAll]) then
    Result := Model_NeiGojoboriJCDistStr
  else if FPickListString = StringReplace(MLAminoPickList, '"', '', [rfReplaceAll]) then
    Result := Model_JTTStr
  else if FPickListString = StringReplace(CompPairwisePartialDelPickList, '"', '', [rfReplaceAll]) then
    Result := PairwiseDelStr
  else if FPickListString = StringReplace(CompAllPartialDelPickList, '"', '', [rfReplaceAll]) then
    Result := UseAllStr
  else if FPickListString = StringReplace(YesFastNoSlowPickList, '"', '', [rfReplaceAll]) then
    Result := NoSlowStr
  else
  begin
    try
      AList := TStringList.Create;
      SplitOnSingleCharFaster(FPickListString, ',', AList, False);
      if AList.Count > 0 then
        Result := AList[0];
    finally
      if Assigned(AList) then
        AList.Free;
    end;
  end;
end;

function TPickListAnalysisSetting.GetIsPicklist: Boolean;
begin
  Result := (FPickListString <> EmptyStr);
end;

procedure TPickListAnalysisSetting.SetItemIndex(AValue: Integer);
begin
  SplitOnSingleCharFaster(FPickListString, ',', FPickList, False);
  Assert((AValue >= 0) and (AValue < FPickList.Count));
  FItemIndex:=AValue;
  Value := FPickList[AValue];
end;

procedure TPickListAnalysisSetting.SetLongCmdPickList(AValue: String);
begin
  if FLongCmdFlagPickList=AValue then Exit;
  FLongCmdFlagPickList:=AValue;
  SplitOnSingleCharFaster(FLongCmdFlagPickList, ',', FLongCmdPickList, False);
end;

procedure TPickListAnalysisSetting.SetPicklist(AValue: String);
var
  CleanStr: String;
begin
  CleanStr := StringReplace(AValue, '"', '', [rfReplaceAll]);
  FPickListString:=CleanStr;
  SplitOnSingleCharFaster(FPickListString, ',', FPickList, False);
  if FPickList.IndexOf(FValue) < 0 then
    FValue := DefaultValue;
  FItemIndex := FPickList.IndexOf(FValue);

  if FPickList.Count = 1 then { in some cases, the picklist has a single item}
    IsReadOnly := True
  else
    IsReadOnly := False;
end;

procedure TPickListAnalysisSetting.SetShortCmdPickList(AValue: String);
begin
  if FShortCmdFlagPickList = AValue then Exit;
  FShortCmdFlagPickList := AValue;
  SplitOnSingleCharFaster(FShortCmdFlagPickList, ',', FShortCmdPickList, False);
end;

procedure TPickListAnalysisSetting.SetStringValue(AValue: String);
begin
  if AValue = NotApplicableStr then
  begin
    FIsReadOnly := True;
  end
  else
  begin
    if FPickList.IndexOf(AValue) < 0 then
    begin
      if FPickListString <> EmptyStr then
        SetPicklist(FPickListString);
    end;
    Assert(FPickList.IndexOf(AValue) >= 0);
    if FValue=AValue then Exit;
    FValue:=AValue;
    FItemIndex := FPickList.IndexOf(AValue);
    if Assigned(FValueSetting) and (FValueSetting.Value <> FValue) then
      FValueSetting.Value := FValue;
  end;
end;

procedure TPickListAnalysisSetting.SetValueSetting(AValue: TPickListValueSetting);
begin
  if FValueSetting=AValue then Exit;
  FValueSetting:=AValue;
end;

function TPickListAnalysisSetting.GetStringValue: String;
begin
  if not IsApplicable then
    Result := NotApplicableStr
  else
  begin
    if FItemIndex < FPickList.Count then
      Result := FPickList[FItemIndex]
    else
      Result := FValue;
  end;
end;

function TPickListAnalysisSetting.GetLongCmdFlag: String;
begin
  if FItemIndex < FLongCmdPickList.Count then
    FLongCmdFlag := FLongCmdPickList.Names[FItemIndex];
  Result:=inherited GetLongCmdFlag;
end;

function TPickListAnalysisSetting.GetShortCmdFlag: String;
begin
  if FItemIndex < FShortCmdPickList.Count then
    FShortCmdFlag := FShortCmdPickList[FItemIndex];
  Result:=inherited GetShortCmdFlag;
end;

constructor TPickListAnalysisSetting.Create(AIndentLevel: Integer; aPickListName: String);
begin
  inherited Create(AIndentLevel);
  FPickListName := aPickListName;
  FSettingType := stPicklist;
  FPickListString := EmptyStr;
  FValue := EmptyStr;
  FPickList := TStringList.Create;
  FShortCmdPickList := TStringList.Create;
  FLongCmdPickList := TStringList.Create;
  FValueSetting := nil;
end;

destructor TPickListAnalysisSetting.Destroy;
begin
  if Assigned(FPickList) then
    FPickList.Free;
  if Assigned(FShortCmdPickList) then
    FShortCmdPickList.Free;
  if Assigned(FLongCmdPickList) then
    FLongCmdPickList.Free;
  inherited Destroy;
end;

function TPickListAnalysisSetting.GetJson: TJsonObject;
begin
  Result:=inherited GetJson;
  Result.Add('FItemIndex', FItemIndex);
  Result.Add('FPickListString', FPickListString);
  Result.Add('FShortCmdFlagPickList', ShortCmdPickList);
  Result.Add('FLongCmdFlagPickList', LongCmdPickList);
  Result.Add('FValue', FValue);
  Result.Add('FPickListName', FPickListName);
end;

function TPickListAnalysisSetting.LoadJson(AJson: String): Boolean;
var
  AParser: TJSONParser;
  Json: TJSONObject;
  AData: TJSONData;
begin
  AParser := nil;
  Json := nil;
  AData := nil;
  Result := False;

  try
    Result := inherited LoadJson(AJson);
    if not Result then
      raise Exception.Create('failed to load JSON for TAnalysisSetting');

    AParser := TJSONParser.Create(AJson, []);
    AData := AParser.Parse;
    if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
      raise Exception.Create('failed to parse JSON data');
    Json := TJSONObject(AData);

    AData := Json.Find('FValue', jtString);
    if not Assigned(AData) then
      raise Exception.Create('missing FValue');
    FValue := AData.Value;

    AData := Json.Find('FItemIndex', jtNumber);
    if not Assigned(AData) then
      raise Exception.Create('missing item index');
    FItemIndex := AData.Value;

    AData := Json.Find('FPickListString', jtString);
    if not Assigned(AData) then
      raise Exception.Create('missing picklist string');
    FPickListString := AData.Value;

    AData := Json.Find('FPickListName', jtString);

    if Assigned(AData) then
      FPickListName := AData.Value;

    AData := Json.Find('FShortCmdFlagPickList', jtString);
    if not Assigned(AData) then
      raise Exception.Create('missing short cmd picklist string');
    FShortCmdFlagPickList := AData.Value;

    AData := Json.Find('FLongCmdFlagPickList', jtString);
    if not Assigned(AData) then
      raise Exception.Create('missing long cmd picklist string');
    LongCmdPickList := AData.Value;

    Result := True;
  finally
    if Assigned(AParser) then
      AParser.Free;
    if Assigned(Json) then
      Json.Free;
  end;
end;

function TPickListAnalysisSetting.StoredValueString: String;
begin
  Result := FValue;
end;

function TPickListAnalysisSetting.GetClone: TPickListAnalysisSetting;
begin
  Result := TPickListAnalysisSetting.Create(Self.FIndentLevel, Self.FPickListName);
  Result.Assign(Self);
end;

procedure TPickListAnalysisSetting.Assign(Source: TPickListAnalysisSetting);
begin
  FIndentLevel := Source.IndentLevel;
  Name := Source.Name;
  if FSettingType <> Source.SettingType then
    raise Exception.Create('invalid setting type');
  if not Assigned(FPickList) then
    FPickList := TStringList.Create;
  FPickList.AddStrings(Source.FPicklist);
  PickList := Source.PickList;
  FPickListName := Source.FPickListName;
  if Assigned(Source.ValueSetting) then
  begin
    if not Assigned(FValueSetting) then
      FValueSetting := TPickListValueSetting.Create(FIndentLevel);
    FValueSetting.Assign(Source.FValueSetting);
  end
  else if Assigned(FValueSetting) then
    FreeAndNil(FValueSetting);
  Value := Source.Value;
  FIsReadOnly := Source.IsReadOnly;
  FIsApplicable := Source.IsApplicable;
  FIsChecked := Source.IsChecked;
  FIsOptional := Source.IsOptional;
end;

procedure TPickListAnalysisSetting.AssignValues(Source: TPickListAnalysisSetting);
begin
  Name := Source.Name;
  Value := Source.Value;
end;

procedure TPickListAnalysisSetting.Update(AName: String; AList: String);
begin
  Name := AName;
  PickList := AList;
  Value := DefaultValue;
end;

function TPickListAnalysisSetting.SetValueFromStr(AValue: String): Boolean;
begin
  Assert(Trim(AValue) <> EmptyStr, 'cannot set value from empty string');
  Result := True;
  SetStringValue(AValue);
  if AValue <> NotApplicableStr then
    FIsApplicable := True;
end;

function TPickListAnalysisSetting.PickListStrings: TStringList;
begin
  Result := TStringList.Create;
  SplitOnSingleCharFaster(FPickListString, ',', Result, False);
end;

function TPickListAnalysisSetting.GetShortCmdValue: String;
begin
  if not IsApplicable then
    Result := EmptyStr
  else
  begin
    if FItemIndex < FShortCmdPickList.Count then
      Result := FShortCmdPickList[FItemIndex]
    else
      Result := EmptyStr;
  end;
end;

function TPickListAnalysisSetting.GetLongCmdValue: String;
begin
  if not IsApplicable then
    Result := EmptyStr
  else
  begin
    if FItemIndex < FLongCmdPickList.Count then
      Result := FLongCmdPickList.ValueFromIndex[FItemIndex]
    else
      Result := EmptyStr;
  end;
end;

function TPickListAnalysisSetting.HasValue(aValue: String): Boolean;
var
  aList: TStringList = nil;
begin
  try
    aList := PickListStrings;
    Result := (aList.IndexOf(aValue) >= 0);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TPickListAnalysisSetting.RemoveValue(aValue: String);
var
  aList: TStringList = nil;
begin
  try
    if HasValue(aValue) then
    begin
      aList := PickListStrings;
      aList.Delete(aList.IndexOf(aValue));
      PickList := aList.CommaText;
    end;
  finally
    if Assigned(aList) then
      aList.Free
  end;
end;

function TPickListAnalysisSetting.ValidatePickList: Boolean;
begin
  { this developer code to test IQ-TREE2 settings. This function is not reliable for anything else}
  Result := FPickList.Count > 0;
  Result := FPickList.Count = FLongCmdPickList.Count;
  {$IFDEF DEBUG}
  if not Result then
    raise Exception.Create(Format('%s has invalid picklist settings. Got %d items and %d long command items', [FDisplayName, FPickList.Count, FLongCmdPickList.Count]));
  {$ENDIF}
end;

{ TFloatAnalysisSetting }

procedure TFloatAnalysisSetting.SetIncrement(AValue: Double);
begin
  if FIncrement=AValue then Exit;
  FIncrement:=AValue;
end;

procedure TFloatAnalysisSetting.SetMax(AValue: Double);
begin
  if FMax=AValue then Exit;
  FMax:=AValue;
end;

procedure TFloatAnalysisSetting.SetMin(AValue: Double);
begin
  if FMin=AValue then Exit;
  FMin:=AValue;
end;

procedure TFloatAnalysisSetting.SetPrecision(AValue: Integer);
begin
  if FPrecision=AValue then Exit;
  FPrecision:=AValue;
end;

procedure TFloatAnalysisSetting.SetFloatValue(AValue: Double);

begin
  Assert((CompareValue(AValue, Min, 0.001) >= 0) and (CompareValue(AValue, Max, 0.001) <= 0));
  if FValue=AValue then Exit;
  FValue:=AValue;
end;

function TFloatAnalysisSetting.GetStringValue: String;
begin
  if not IsApplicable then
    Result := NotApplicableStr
  else
    Result := Format('%.' + IntToStr(Precision) + 'f', [FValue]);
end;

constructor TFloatAnalysisSetting.Create(AIndentLevel: Integer);
begin
  inherited Create(AIndentLevel);
  FSettingType := stFloat;
  FValue := 0;
  FMin := 0;
  FMax := 100;
  FIncrement := 1;
  FPrecision := 2;
end;

destructor TFloatAnalysisSetting.Destroy;
begin
  inherited Destroy;
end;

function TFloatAnalysisSetting.GetJson: TJsonObject;
begin
  Result:=inherited GetJson;
  Result.Add('FIncrement', FIncrement);
  Result.Add('FMax', FMax);
  Result.Add('FMin', FMin);
  Result.Add('FPrecision', FPrecision);
  Result.Add('FValue', FValue);
end;

function TFloatAnalysisSetting.LoadJson(AJson: String): Boolean;
var
  AParser: TJSONParser;
  Json: TJSONObject;
  AData: TJSONData;
begin
  AParser := nil;
  Json := nil;
  AData := nil;
  Result := False;

  try
    Result := inherited LoadJson(AJson);
    if not Result then
      raise Exception.Create('failed to load JSON for TAnalysisSetting');

    AParser := TJSONParser.Create(AJson, []);
    AData := AParser.Parse;
    if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
      raise Exception.Create('failed to parse JSON data');
    Json := TJSONObject(AData);

    AData := Json.Find('FValue', jtNumber);
    if not Assigned(AData) then
      raise Exception.Create('missing FValue');
    FValue := AData.Value;

    AData := Json.Find('FIncrement', jtNumber);
    if not Assigned(AData) then
      raise Exception.Create('missing increment');
    FIncrement := AData.Value;

    AData := Json.Find('FMin', jtNumber);
    if not Assigned(AData) then
      raise Exception.Create('missing min value');
    FMin := AData.Value;

    AData := Json.Find('FMax', jtNumber);
    if not Assigned(AData) then
      raise Exception.Create('missing max value');
    FMax := AData.Value;

    AData := Json.Find('FPrecision', jtNumber);
    if not Assigned(AData) then
      raise Exception.Create('missing precision value');
    FPrecision := AData.Value;
    Result := True;
  finally
    if Assigned(AParser) then
      AParser.Free;
    if Assigned(Json) then
      Json.Free;
  end;
end;

function TFloatAnalysisSetting.IsValidValue(aVal: Double): Boolean;
begin
  Result := (CompareValue(aVal, FMin, FP_CUTOFF) >= 0) and (CompareValue(aVal, FMax) <= 0)
end;

function TFloatAnalysisSetting.StoredValueString: String;
begin
  Result := Format('%.' + IntToStr(Precision) + 'f', [FValue]);
end;

function TFloatAnalysisSetting.GetClone: TFloatAnalysisSetting;
begin
  Result := TFloatAnalysisSetting.Create(Self.FIndentLevel);
  Result.Assign(Self);
end;

procedure TFloatAnalysisSetting.Assign(Source: TFloatAnalysisSetting);
begin
  FIndentLevel := Source.IndentLevel;
  Name := Source.Name;
  if FSettingType <> Source.SettingType then
    raise Exception.Create('invalid setting type');
  Min := Source.Min;
  Max := Source.Max;
  if (Source.Value >= Min) and (Source.Value <= Max) then
    Value := Source.Value;
  Increment := Source.Increment;
  Precision := Source.Precision;
  FIsReadOnly := Source.IsReadOnly;
  FIsApplicable := Source.IsApplicable;
  FIsChecked := Source.IsChecked;
  FIsOptional := Source.IsOptional;
end;

procedure TFloatAnalysisSetting.AssignValues(Source: TFloatAnalysisSetting);
begin
  Name := Source.Name;
  if IsValidValue(Source.Value) then
    Value := Source.Value
  else if CompareValue(Source.Value, FMin, FP_CUTOFF) < 0 then
    Value := FMin
  else if CompareValue(Source.Value, FMax, FP_CUTOFF) > 0 then
    Value := FMax
  else
    raise Exception.Create('Application error - invalid float value');
end;

function TFloatAnalysisSetting.SetValueFromStr(AValue: String): Boolean;
var
  AFloat: Double;
begin
  Result := False;
  if AValue = NotApplicableStr then
  begin
    FIsReadOnly := True;
    FIsApplicable := False;
    Result := True;
    Exit;
  end;

  if TryStrToFloat(AValue, AFloat) then
  begin
    if ((AFloat >= Min) and (AFloat <= Max)) then
    begin
      Result := True;
      FIsApplicable := True;
      SetFloatValue(AFloat);
    end;
  end;
end;

{ TIntegerAnalysisSetting }

function TIntegerAnalysisSetting.GetStringValue: String;
begin
  if FGivenValueString = MultithreadNotAvailableStr then
    Result := MultithreadNotAvailableStr
  else if not IsApplicable then
    Result := NotApplicableStr
  else
    Result := IntToStr(FValue);
end;

procedure TIntegerAnalysisSetting.SetIncrement(AValue: Integer);
begin
  if FIncrement=AValue then Exit;
  FIncrement:=AValue;
end;

procedure TIntegerAnalysisSetting.SetMax(AValue: Integer);
begin
  if FMax=AValue then Exit;
  FMax:=AValue;
end;

procedure TIntegerAnalysisSetting.SetMin(AValue: Integer);
begin
  if FMin=AValue then Exit;
  FMin:=AValue;
end;

procedure TIntegerAnalysisSetting.SetIntValue(AValue: Integer);
var
  debug: Boolean = False;
begin
  {$IFDEF DEBUG}
  if (AValue < FMin) or (AValue > FMax) then
    debug := True;
  {$ENDIF}
  Assert((AValue >= FMin) and (AValue <= FMax), Format('Invalid value: val=%d, min=%d, max=%d', [AValue, FMin, FMax]));
  if FValue=AValue then Exit;
  FValue:=AValue;
end;

constructor TIntegerAnalysisSetting.Create(AIndentLevel: Integer);
begin
  inherited Create(AIndentLevel);
  FSettingType := stInteger;
  FValue := 0;
  FMin := 0;
  FMax := 100;
  FIncrement := 1;
end;

destructor TIntegerAnalysisSetting.Destroy;
begin
  inherited Destroy;
end;

function TIntegerAnalysisSetting.GetJson: TJsonObject;
begin
  Result:=inherited GetJson;
  Result.Add('FValue', FValue);
  Result.Add('FIncrement', FIncrement);
  Result.Add('FMax', FMax);
  Result.Add('FMin', FMin);
end;

function TIntegerAnalysisSetting.LoadJson(AJson: String): Boolean;
var
  AParser: TJSONParser;
  Json: TJSONObject;
  AData: TJSONData;
begin
  AParser := nil;
  Json := nil;
  AData := nil;
  Result := False;

  try
    Result := inherited LoadJson(AJson);
    if not Result then
      raise Exception.Create('failed to load JSON for TAnalysisSetting');

    AParser := TJSONParser.Create(AJson, []);
    AData := AParser.Parse;
    if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
      raise Exception.Create('failed to parse JSON data');
    Json := TJSONObject(AData);

    AData := Json.Find('FValue', jtNumber);
    if not Assigned(AData) then
      raise Exception.Create('missing FValue');
    FValue := AData.Value;

    AData := Json.Find('FIncrement', jtNumber);
    if not Assigned(AData) then
      raise Exception.Create('missing increment');
    FIncrement := AData.Value;

    AData := Json.Find('FMin', jtNumber);
    if not Assigned(AData) then
      raise Exception.Create('missing min value');
    FMin := AData.Value;

    AData := Json.Find('FMax', jtNumber);
    if not Assigned(AData) then
      raise Exception.Create('missing max value');
    FMax := AData.Value;
    Result := True;
  finally
    if Assigned(AParser) then
      AParser.Free;
    if Assigned(Json) then
      Json.Free;
  end;
end;

function TIntegerAnalysisSetting.IsValidValue(aVal: Integer): Boolean;
begin
  Result := (aVal >= FMin) and (aVal <= FMax);
end;

function TIntegerAnalysisSetting.StoredValueString: String;
begin
  Result := IntToStr(FValue);
end;

function TIntegerAnalysisSetting.GetClone: TIntegerAnalysisSetting;
begin
  Result := TIntegerAnalysisSetting.Create(Self.FIndentLevel);
  Result.Assign(Self);
end;

function TIntegerAnalysisSetting.SetValueFromStr(AValue: String): Boolean;
var
  i: Integer;
begin
  FGivenValueString := AValue;
  Result := False;
  if (AValue = NotApplicableStr) or (AValue = MultithreadNotAvailableStr) then
  begin
    FIsReadOnly := True;
    FIsApplicable := False;
    Result := True;
    Exit;
  end;

  if TryStrToInt(AValue, i) then
  begin
    if ((i >= FMin) and (i <= FMax)) then
    begin
      Result := True;
      SetIntValue(i);
    end;
  end;
end;

procedure TIntegerAnalysisSetting.Assign(Source: TIntegerAnalysisSetting);
begin
  Min := Source.Min;
  Max := Source.Max;
  Increment := Source.Increment;
  if (Source.Value >= Min) and (Source.Value <= Max) then
    Value := Source.Value;
  FIndentLevel := Source.IndentLevel;
  Name := Source.Name;
  if FSettingType <> Source.SettingType then
    raise Exception.Create('invalid setting type');
  FIsReadOnly := Source.IsReadOnly;
  FIsApplicable := Source.IsApplicable;
  FIsChecked := Source.IsChecked;
  FIsOptional := Source.IsOptional;
end;

procedure TIntegerAnalysisSetting.AssignValues(Source: TIntegerAnalysisSetting);
begin
  if IsValidValue(Source.Value) then
    Value := Source.Value
  else if Source.Value < FMin then
    Value := FMin
  else if Source.Value > FMax then
    Value := FMax
  else
    raise Exception.Create('Application error - invalid integer value');
  Name := Source.Name;
end;

{ TStringAnalysisSetting }

procedure TStringAnalysisSetting.SetStringValue(AValue: String);
var
  debug: Boolean = False;
begin
  if Pos(',', aValue) > 0 then
    debug := True;
  FStringValue := AValue;
end;

function TStringAnalysisSetting.GetStringValue: String;
begin
  if not FIsApplicable then
    Result := NotApplicableStr
  else
    Result := FStringValue;
end;

constructor TStringAnalysisSetting.Create(AIndentLevel: Integer);
begin
  inherited Create(AIndentLevel);
  FSettingType := stString;
  FStringValue := EmptyStr;
end;

destructor TStringAnalysisSetting.Destroy;
begin
  inherited Destroy;
end;

function TStringAnalysisSetting.GetJson: TJsonObject;
begin
  Result:=inherited GetJson;
  Result.Add('FStringValue', FStringValue);
end;

function TStringAnalysisSetting.LoadJson(AJson: String): Boolean;
var
  AParser: TJSONParser;
  Json: TJSONObject;
  AData: TJSONData;
begin
  AParser := nil;
  Json := nil;
  AData := nil;
  Result := False;

  try
    Result := inherited LoadJson(AJson);
    if not Result then
      raise Exception.Create('failed to load JSON for TAnalysisSetting');

    AParser := TJSONParser.Create(AJson, []);
    AData := AParser.Parse;
    if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
      raise Exception.Create('failed to parse JSON data');
    Json := TJSONObject(AData);

    AData := Json.Find('FStringValue', jtString);
    if not Assigned(AData) then
      raise Exception.Create('missing stringValue');
    FStringValue := AData.Value;
    Result := True;
  finally
    if Assigned(AParser) then
      AParser.Free;
    if Assigned(Json) then
      Json.Free;
  end;
end;

function TStringAnalysisSetting.StoredValueString: String;
begin
  Result := FStringValue;
end;

function TStringAnalysisSetting.GetClone: TStringAnalysisSetting;
begin
  Result := TStringAnalysisSetting.Create(FIndentLevel);
  Result.Assign(Self);
end;

function TStringAnalysisSetting.SetValueFromStr(AValue: String): Boolean;
begin
  SetStringValue(AValue);
  Result := True;
end;

procedure TStringAnalysisSetting.Assign(Source: TStringAnalysisSetting);
begin
  Value := Source.Value;
  FIndentLevel := Source.IndentLevel;
  Name := Source.Name;
  if FSettingType <> Source.SettingType then
    raise Exception.Create('invalid setting type');
  FIsReadOnly := Source.IsReadOnly;
  FIsApplicable := Source.IsApplicable;
  FIsChecked := Source.IsChecked;
  FIsOptional := Source.IsOptional;
end;

procedure TStringAnalysisSetting.AssignValues(Source: TStringAnalysisSetting);
begin
  Value := Source.Value;
  Name := Source.Name;
end;

procedure TStringAnalysisSetting.Update(AName: String; AValue: String);
begin
  Name:=AName;
  Value := AValue;
end;

{ TAnalysisSetting }

function TAnalysisSetting.GetSettingType: TSettingType;
begin
  Result := FSettingType;
end;

procedure TAnalysisSetting.SetIsReadOnly(AValue: Boolean);
begin
  if FIsReadOnly = AValue then
    Exit;
  FIsReadOnly:=AValue;
end;

procedure TAnalysisSetting.SetIsChecked(AValue: Boolean);
begin
  if FIsChecked = AValue then
    Exit;
  FIsChecked:=AValue;
end;

procedure TAnalysisSetting.SetIsOptional(AValue: Boolean);
begin
  if FIsOptional = AValue then
    Exit;
  FIsOptional:=AValue;
end;

procedure TAnalysisSetting.SetLongCmdFlag(AValue: String);
begin
  if FLongCmdFlag=AValue then Exit;
  FLongCmdFlag:=AValue;
end;

function TAnalysisSetting.GetIsApplicable: Boolean;
begin
  if FIsOptional and (not FIsChecked) then
    Result := False
  else
    Result := FIsApplicable;
end;

procedure TAnalysisSetting.SetIsAdaptive(AValue: Boolean);
begin
  if FIsAdaptive = AValue then Exit;
  FIsAdaptive := AValue;
end;

function TAnalysisSetting.GetIsAdvancedSetting: Boolean;
begin
  Result := FIsAdvancedSetting;
end;

function TAnalysisSetting.GetIsReadOnly: Boolean;
begin
  Result := FIsReadOnly;
end;

function TAnalysisSetting.GetIsVisible: Boolean;
begin
  Result := FIsVisible;
end;

procedure TAnalysisSetting.SetIsAdvancedSetting(AValue: Boolean);
begin
  if FIsAdvancedSetting <> AValue then
    FIsAdvancedSetting := AValue;
end;

procedure TAnalysisSetting.SetIsVisible(AValue: Boolean);
begin
  if FIsVisible <> AValue then
    FIsVisible := AValue;
end;

procedure TAnalysisSetting.SetSettingType(AValue: TSettingType);
begin
  if  FSettingType <> AValue then
    FSettingType := AValue;
end;

procedure TAnalysisSetting.SetDescription(AValue: String);
begin
  if FDescription=AValue then Exit;
  FDescription:=AValue;
end;

procedure TAnalysisSetting.SetDisplayName(AValue: String);
begin
  if FDisplayName=AValue then Exit;
  FDisplayName:=AValue;
end;

function TAnalysisSetting.GetLongCmdFlag: String;
begin
  Result := FLongCmdFlag;
end;

function TAnalysisSetting.GetShortCmdFlag: String;
begin
  Result := FShortCmdFlag;
end;

procedure TAnalysisSetting.SetShortCmdFlag(AValue: String);
begin
  if FShortCmdFlag=AValue then Exit;
  FShortCmdFlag:=AValue;
end;

procedure TAnalysisSetting.SetIsApplicable(AValue: Boolean);
begin
  if FIsApplicable=AValue then Exit;
  FIsApplicable:=AValue;
end;

procedure TAnalysisSetting.SetName(AValue: String);
begin
  if FName=AValue then Exit;
  FName:=AValue;
  if FDisplayName = EmptyStr then
    FDisplayName := FName;
end;

constructor TAnalysisSetting.Create(AIndentLevel: Integer);
begin
  FIndentLevel := AIndentLevel;
  FIsReadOnly := False;
  FIsApplicable := True;
  FIsChecked := False;
  FIsOptional := False;
  FName := EmptyStr;
  FDisplayName := EmptyStr;
  FShortCmdFlag := EmptyStr;
  FLongCmdFlag := EmptyStr;
  FDescription := EmptyStr;
  FIsAdvancedSetting := False;
  FIsVisible := True;
end;

destructor TAnalysisSetting.Destroy;
begin
  inherited Destroy;
end;

function TAnalysisSetting.GetDisplayName: String;
begin
  if FIndentLevel = 0 then
    Result := FDisplayName
  else if FIndentLevel = 1 then
    Result := '   ' + FDisplayName
  else if FIndentLevel = 2 then
    Result := '      ' + FDisplayName
  else
  begin
    Assert(False, 'missing indent level handler');
  end;
end;

function TAnalysisSetting.NameValueString: String;
begin
  Result := FName + '=' + StringValue;
end;

function TAnalysisSetting.MaoString(FormatString: String): String;
begin
  Result := Format(FormatString, [FName, StringValue]);
end;

function TAnalysisSetting.GetJson: TJsonObject;
begin
  Result := TJSONObject.Create;
  Result.Add('FName', FName);
  Result.Add('FDisplayName', FDisplayName);
  Result.Add('FSettingType', GetEnumName(TypeInfo(TSettingType), Ord(FSettingType)));
  Result.Add('FIsReadOnly', FIsReadOnly);
  Result.Add('FIsApplicable', FIsApplicable);
  Result.Add('FIsChecked', FIsChecked);
  Result.Add('FIsOptional', FIsOptional);
  Result.Add('FIndentLevel', FIndentLevel);
  Result.Add('FShortCmdFlag', ShortCmdFlag);
  Result.Add('FLongCmdFlag', LongCmdFlag);
  Result.Add('FIsAdvancedSetting', FIsAdvancedSetting);
  Result.Add('FIsVisible', FIsVisible);
  if Trim(FHint) <> EmptyStr then
    Result.Add('FHint', FHint);
end;

function TAnalysisSetting.LoadJson(AJson: String): Boolean;
var
  AParser: TJSONParser;
  Json: TJSONObject;
  AData: TJSONData;
begin
  AParser := nil;
  Json := nil;
  AData := nil;
  Result := False;

  try
    AParser := TJSONParser.Create(AJson, []);
    AData := AParser.Parse;
    if (not Assigned(AData)) or (not (AData.JSONType = jtObject)) then
      raise Exception.Create('failed to parse JSON data');
    Json := TJSONObject(AData);

    AData := Json.Find('FName', jtString);
    if not Assigned(AData) then
      raise Exception.Create('missing setting name');
    Name := AData.Value;
    AData := Json.Find('FDisplayName', jtString);
    if not Assigned(AData) then
      raise Exception.Create('missing setting display name');
    FDisplayName := AData.Value;

    AData := Json.Find('FIsApplicable', jtBoolean);
    if not Assigned(AData) then
      raise Exception.Create('missing FIsApplicable');
    FIsApplicable := AData.Value;

    AData := Json.Find('FIsReadOnly', jtBoolean);
    if not Assigned(AData) then
      raise Exception.Create('missing FIsReadOnly');
    FIsReadOnly := AData.Value;

    AData := Json.Find('FSettingType', jtString);
    if not Assigned(AData) then
      raise Exception.Create('missing setting type');
    FSettingType := TSettingType(GetEnumValue(TypeInfo(TSettingType), AData.Value));

    AData := Json.Find('FShortCmdFlag', jtString);
    if not Assigned(AData) then
      raise Exception.Create('missing short command flag');
    FShortCmdFlag := AData.Value;

    AData := Json.Find('FLongCmdFlag', jtString);
    if not Assigned(AData) then
      raise Exception.Create('missing long command flag');
    FLongCmdFlag := AData.Value;

    AData := Json.Find('FIndentLevel', jtNumber);
    if not Assigned(AData) then
      raise Exception.Create('missing indent level');
    FIndentLevel := AData.Value;

    AData := Json.Find('FIsChecked', jtBoolean);
    if Assigned(AData) then
       FIsChecked := AData.Value;

    AData := Json.Find('FIsOptional', jtBoolean);
    if Assigned(AData) then
       FIsOptional := AData.Value;

    AData := Json.Find('FIsAdvancedSetting', jtBoolean);
    if Assigned(AData) then
      FIsAdvancedSetting := AData.Value;

    AData := Json.Find('FIsVisible', jtBoolean);
    if Assigned(AData) then
      FIsVisible := AData.Value;

    AData := Json.Find('FHint', jtString);
    if Assigned(AData) then
      FHint := Trim(AData.Value);

    Result := True;
  finally
    if Assigned(AParser) then
      AParser.Free;
    if Assigned(Json) then
      Json.Free;
  end;
end;

function TAnalysisSetting.GetPrettyString(NameWidth: Integer): String;
begin
  if FIsAdaptive then
    Result := Format('%-' + IntToStr(NameWidth) + 's = %s', [DisplayName, AdaptiveStr])
  else
    Result := Format('%-' + IntToStr(NameWidth) + 's  = %s', [DisplayName, StringValue]);
end;

procedure TAnalysisSetting.SetState(AIsReadOnly: Boolean; AIsApplicable: Boolean);
begin
  IsReadOnly := AIsReadOnly;
  IsApplicable := AIsApplicable;
end;

end.

