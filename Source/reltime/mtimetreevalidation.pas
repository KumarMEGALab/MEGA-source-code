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

unit MTimeTreeValidation;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFNDEF VISUAL_BUILD}MTreeList,{$ENDIF}
  Classes, SysUtils, MCalibrationData, MRuntimeProgressDlg, MTreeData, MSimpleTreeNode,
  MTreeDataAdapter, MegaConsts;

const
  FP_CUTOFF = 0.000000000001;

type

{ TCalibrationsValidator }

TCalibrationsValidator = class(TObject)
  private
    FLastUpdateTime: TDateTime;
    FNumNodes: Integer;
    FNumInternalNodes: Integer;
    FNumTaxa: Integer;
    FCalibrations: TArrayOfCalibrationTime;
    FTree: TSimpleTreeNodeArray;
    FRoot: TSimpleTreeNode;
    FAdapter: TSimpleTreeDataAdapter;
    procedure SetRoot;
    procedure SetNumTaxa(AValue: Integer);
    procedure UpdateProgress(NodeIndex: Integer);
    function NumOutgroupNodes: Integer;
    procedure AddMessage(AMsg: TValidationMessage);
    function NodeIsRoot(Index: Integer): Boolean;
    procedure InitMinMaxTimeOfNode(n: TSimpleTreeNode);
    procedure SetOtuNames(OtuNames: TStringList);
    procedure SetCalibrationNames;
    function GetOtuIndex(OtuName: String): Integer;
    function debugExportMinMaxTimes: Boolean;
  public
    StatusMessages: TValidationMessages;
    MinTimes, MaxTimes: TDivTimesArray;
    Invalids: TArrayOfInteger;
    Progress: TRuntimeProgress;
    constructor Create;
    destructor Destroy; override;
    procedure SetData(const AData: TTreeData; const Calibs: TArrayOfCalibrationTime); overload;
    procedure SetData(const AData: TTreeData; const Calibs: TArrayOfCalibrationTime; const OtuName: TStringList); overload;
    function ValidateDivergenceTimes(var Msg: String): Boolean;
    function ValidateSampledTimes(var Msg: String): Boolean;
    function ValidateOutgroupConstraint(var Msg: String): Boolean;
    function ValidateMinMaxTimes(var Msg: String): Boolean;
    function ValidateCalibrations(var Msg: String): Boolean;
    function ValidateAllAreSampleTimes(var Msg: String): Boolean;
    function ValidateAllSampleTimesProvided(var Msg: String): Boolean;
    function ValidateAllAreInternalNodes(var Msg: String): Boolean;
    function QuickValidation(const Calibs: TArrayOfCalibrationTime; var Msg: String): Boolean;
    function PropogateCalibrationsOverAllNodes(const aData: TTreeData; OtuNames: TStringList; const Calibs: TArrayOfCalibrationTime): TStringList;
    property NumTaxa: Integer read FNumTaxa write SetNumTaxa;
    property NumNodes: Integer read FNumNodes;
end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ENDIF}
  dateutils, math;

{ TCalibrationsValidator }

procedure TCalibrationsValidator.SetRoot;
var
  i: Integer;
begin
  FRoot := nil;
  for i := 0 to FNumNodes - 1 do
    if not Assigned(FTree[i].Ancestor) then
      FRoot := FTree[i];
  Assert(Assigned(FRoot));
end;

procedure TCalibrationsValidator.SetNumTaxa(AValue: Integer);
begin
  if FNumTaxa=AValue then Exit;
  FNumTaxa :=AValue;
  FNumNodes := 2 * FNumTaxa - 1;
  FNumInternalNodes := FNumNodes - FNumTaxa;
end;

procedure TCalibrationsValidator.SetOtuNames(OtuNames: TStringList);
var
  i: Integer;
begin
  if not (FNumTaxa = OtuNames.Count)  then
    raise Exception.Create('Invalid otu names list');
  if OtuNames.Count > 0 then
    for i := 0 to OtuNames.Count - 1 do
      FTree[i].SequenceName := OtuNames[i];
end;

procedure TCalibrationsValidator.UpdateProgress(NodeIndex: Integer);
begin
  if MillisecondsBetween(Time, FLastUpdateTime) < 500 then
    Exit;
  FLastUpdateTime := Time;
  if Assigned(Progress) then
    Progress.SetProgress(Round((NodeIndex - NumTaxa) / FNumInternalNodes * 100));
end;

constructor TCalibrationsValidator.Create;
begin
  FNumTaxa := 0;
  FNumNodes := 0;
  SetLength(MinTimes, 0);
  SetLength(MaxTimes, 0);
  SetLength(Invalids, 0);
  StatusMessages := [];
end;

destructor TCalibrationsValidator.Destroy;
begin
  if Assigned(FAdapter) then
    FAdapter.Free;
  SetLength(MinTimes, 0);
  SetLength(MaxTimes, 0);
  SetLength(Invalids, 0);
  inherited Destroy;
end;

function TCalibrationsValidator.GetOtuIndex(OtuName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  if NumTaxa > 0 then
    for i := 0 to NumTaxa - 1 do
      if OtuName = FTree[i].SequenceName then
      begin
        Result := i;
        break;
      end;
end;

function TCalibrationsValidator.debugExportMinMaxTimes: Boolean;
{$IFNDEF VISUAL_BUILD}
var
  aFile: TextFile;
  filename: String;
  i: Integer;
{$ENDIF}
begin
  Result := False;
  {$IFDEF DEBUG}{$IFNDEF VISUAL_BUILD}
  filename := NextAvailableFilenameNV('_debug_calibs.csv');
  try
    AssignFile(aFile, filename);
    Rewrite(aFile);
    WriteLn(aFile, 'MinTime,MaxTime');
    for i := Low(MinTimes) to High(MinTimes) do
      WriteLn(aFile, Format('%.8f,%.8f', [MinTimes[i], MaxTimes[i]]));
    Result := FileExists(filename);
  finally
    CloseFile(aFile);
  end;
  {$ENDIF}{$ENDIF}
end;

procedure TCalibrationsValidator.InitMinMaxTimeOfNode(n: TSimpleTreeNode);
begin
  if n.IsOTU then
    Exit;
  if n.Ancestor <> nil then
    if (FCalibrations[n.Ancestor.NodeIndex - NumTaxa].MaxTime > FP_CUTOFF) and (FCalibrations[n.NodeIndex - NumTaxa].MaxTime < FP_CUTOFF) then
      FCalibrations[n.NodeIndex - NumTaxa].MaxTime := FCalibrations[n.Ancestor.NodeIndex - NumTaxa].MaxTime;

  InitMinMaxTimeOfNode(n.des1);
  InitMinMaxTimeOfNode(n.des2);

  if not n.des1.IsOTU then
    if FCalibrations[n.NodeIndex - NumTaxa].MinTime < FCalibrations[n.des1.NodeIndex - NumTaxa].MinTime then
      FCalibrations[n.NodeIndex - NumTaxa].MinTime := FCalibrations[n.des1.NodeIndex - NumTaxa].MinTime;
  if not n.des2.IsOTU then
    if FCalibrations[n.NodeIndex - NumTaxa].MinTime < FCalibrations[n.des2.NodeIndex - NumTaxa].MinTime then
      FCalibrations[n.NodeIndex - NumTaxa].MinTime := FCalibrations[n.des2.NodeIndex - NumTaxa].MinTime;

end;

procedure TCalibrationsValidator.SetCalibrationNames;
var
  d1Name, d2Name: String;
  i: Integer;

  function Des1Name(n: TSimpleTreeNode): String;
  begin
    if n.IsOtu then
    begin
      Result := n.SequenceName;
      Exit;
    end;
    Result := Des1Name(n.Des1);
  end;

  function Des2Name(n: TSimpleTreeNode): String;
  begin
    if n.IsOtu then
    begin
      Result := n.SequenceName;
      Exit;
    end;
    Result := Des2Name(n.Des2);
  end;

begin
  for i := NumTaxa to FNumNodes - 1 do
  begin
    d1Name := Des1Name(FTree[i]);
    d2Name := Des2Name(FTree[i]);
    FCalibrations[FTree[i].NodeIndex - NumTaxa].NodeA := d1Name;
    FCalibrations[FTree[i].NodeIndex - NumTaxa].NodeB := d2Name;
    FCalibrations[FTree[i].NodeIndex - NumTaxa].NodeName := d1Name + '-' + d2Name;
  end;
end;

procedure TCalibrationsValidator.SetData(const AData: TTreeData; const Calibs: TArrayOfCalibrationTime; const OtuName: TStringList);
begin
  NumTaxa := AData.NoOfOTUs;
  FAdapter := TSimpleTreeDataAdapter.Create;
  FAdapter.SetTreeData(AData, True, OtuName);

  FAdapter.ExpandOutgroupFromOutgroupTaxa;
  FTree := FAdapter.GetSimpleTreeReference; { Adapter owns it and will free the memory itself}
  FCalibrations := Calibs;
  Assert(Length(FTree) = FNumNodes);
  SetRoot;
end;

procedure TCalibrationsValidator.SetData(const AData: TTreeData; const Calibs: TArrayOfCalibrationTime);
begin
  NumTaxa := AData.NoOfOTUs;
  FAdapter := TSimpleTreeDataAdapter.Create;
  FAdapter.SetTreeData(AData, True);
  FAdapter.ExpandOutgroupFromOutgroupTaxa;
  FTree := FAdapter.GetSimpleTreeReference; { Adapter owns it and will free the memory itself}
  FCalibrations := Calibs;
  Assert(Length(FTree) = FNumNodes);
  SetRoot;
end;

function TCalibrationsValidator.ValidateDivergenceTimes(var Msg: String): Boolean;
begin
  Assert(FNumTaxa > 0);
  Assert(Length(FTree) = FNumNodes);

  Result := ValidateCalibrations(Msg);
  if Result then
    Result := ValidateOutgroupConstraint(Msg);
  if Result then
    Result := ValidateMinMaxTimes(Msg);
end;

function TCalibrationsValidator.ValidateSampledTimes(var Msg: String): Boolean;
var
  i: Integer;
begin
  Assert((NumOutgroupNodes > 0), 'Failure to setup outgroup nodes (maybe just in TCalibrationsValidator?)');
  if Assigned(Progress) then
    Progress.UpdateRunStatusInfo('Status', 'Validating outgroup constraint');
  FLastUpdateTime := Time;
  Msg := EmptyStr;
  Result := True;
  Assert(FNumTaxa > 0);

  try
    for i := 0 to FNumTaxa - 1 do
    begin
      if (MinTimes[i] > 0) and (MinTimes[i] > (YearOf(Now) + 1)) then
      begin
        Result := False;
        SetLength(Invalids, Length(Invalids) + 1);
        Invalids[Length(Invalids) - 1] := FCalibrations[i].Index;
        AddMessage(vsWarning);
        Msg := Trim(Msg + LineEnding + Format('Sample times must be dates that do not extend into the future but at least one calibration violates this (time is %.2f)', [MinTimes[i]]));
      end;
      UpdateProgress(i);
    end;
  except
    on E:Exception do
    begin
      Msg := 'An exception was encountered when validating constraints: ' + E.Message;
      Result := False;
    end;
  end;
end;


function TCalibrationsValidator.ValidateOutgroupConstraint(var Msg: String): Boolean;
var
  i: Integer;

  function NodeIsInOutGroup(NodeIndex: Integer): Boolean;

    function NodeIsInOutgroupRecursive(ANode: Integer): Boolean;
    begin
      Result := False;
      if FTree[ANode].IsOtu then
      begin
        if FTree[ANode].IsOutgroupMember then
          Result := True;
        exit;
      end;
      Result := Result or NodeIsInOutgroupRecursive(FTree[ANode].des1.NodeIndex);
      if not Result then // we can stop early if we already find out
        Result := Result or NodeIsInOutgroupRecursive(FTree[ANode].des2.NodeIndex);
    end;
  begin
    Result := False;
    Result := Result or NodeIsInOutgroupRecursive(NodeIndex);
  end;

begin
  Assert((NumOutgroupNodes > 0), 'Failure to setup outgroup nodes (maybe just in TCalibrationsValidator?)');
  if Assigned(Progress) then
    Progress.UpdateRunStatusInfo('Status', 'Validating outgroup constraint');
  FLastUpdateTime := Time;
  Msg := EmptyStr;
  Result := False;
  Assert(FNumTaxa > 0);

  try
    Result := True;
    for i := FNumTaxa to FNumNodes - 1 do
    begin
      if (NodeIsInOutGroup(i) or NodeIsRoot(i)) and ((MinTimes[i] > 0) or (MaxTimes[i] > 0)) then
      begin
        Result := False;
        SetLength(Invalids, Length(Invalids) + 1);
        Invalids[Length(Invalids) - 1] := i;
        AddMessage(vsWarning);
        if NodeIsRoot(i) then
          Msg := 'A calibration refers to the root node and this is not allowed'
        else
          Msg := 'At least one calibration refers to a node in the outgroup cluster and this is not allowed';
      end;
      UpdateProgress(i);
    end;
  except
    on E:Exception do
      Msg := 'An exception was encountered when validating constraints: ' + E.Message;
  end;
end;

function TCalibrationsValidator.ValidateMinMaxTimes(var Msg: String): Boolean;
const
  FP_CUTOFF = 0.000000000001;
var
  i: Integer;
  NumProcessed: Integer;

  procedure InitMinMaxTimeOfNode(n: TSimpleTreeNode);
  begin
    if n.IsOtu then
      Exit;
    inc(NumProcessed);
    UpdateProgress(NumProcessed);
    if n.Ancestor <> nil then
      if (MaxTimes[n.Ancestor.NodeIndex] > FP_CUTOFF) and (MaxTimes[n.NodeIndex] > MaxTimes[n.Ancestor.NodeIndex]) then
        MaxTimes[n.NodeIndex] := MaxTimes[n.Ancestor.NodeIndex];

    InitMinMaxTimeOfNode(n.des1);
    InitMinMaxTimeOfNode(n.des2);

    if MinTimes[n.NodeIndex] < MinTimes[n.des1.NodeIndex] then
      MinTimes[n.NodeIndex] := MinTimes[n.des1.NodeIndex];
    if MinTimes[n.NodeIndex] < MinTimes[n.des2.NodeIndex] then
      MinTimes[n.NodeIndex] := MinTimes[n.des2.NodeIndex];
  end;
begin
  Assert(Length(MinTimes) > 0, 'Did you forget to set min times?');
  Assert(Length(MaxTimes) > 0, 'Did you forget to set max times?');
  Assert(Length(MinTimes) = Length(MaxTimes), 'Did you forget to set min or max times?');

  NumProcessed := 0;
  if Assigned(Progress) then
    Progress.UpdateRunStatusInfo('Status', 'Validating min/max times');
  InitMinMaxTimeOfNode(FRoot);
  Result := True;
  for i := NumTaxa to FNumNodes - 1 do
    if (MaxTimes[i] > FP_CUTOFF) and (MaxTimes[i] < MinTimes[i]) then
    begin
      Result := False;
      Msg := 'Calibration(s) are invalid. At least one max time calibration is less than the min time calibration of a descendent node';
      AddMessage(vsError);
      {$IFDEF DEBUG}
      debugExportMinMaxTimes;
      {$ENDIF}
      break;
    end;
end;

function TCalibrationsValidator.ValidateAllAreInternalNodes(var Msg: String): Boolean;
var
  i: Integer;
  aCalib: TCalibrationTime;
begin
  Result := True;
  if Length(FCalibrations) > 0 then
    for i := Low(FCalibrations) to High(FCalibrations) do
    begin
      aCalib := FCalibrations[i];
      if aCalib.CalibrationTarget = ctInternalNode then
        continue;
      if (aCalib.NodeA = aCalib.NodeB) or (Trim(aCalib.NodeA) = EmptyStr) or (Trim(aCalib.NodeB) = EmptyStr) then
      begin
        Result := False;
        Msg := Trim(Msg + LineEnding + 'Invalid calibration: ' + aCalib.AsShortString);
        AddMessage(vsError);
        aCalib.IsValid := False;
        aCalib.IsSelected := False;
        SetLength(Invalids, Length(Invalids) + 1);
        Invalids[Length(Invalids) - 1] := aCalib.Index;
      end;
    end;
end;

function TCalibrationsValidator.ValidateAllAreSampleTimes(var Msg: String): Boolean;
var
  i: Integer;
  aCalib: TCalibrationTime;
begin
  Result := True;
  if Length(FCalibrations) > 0 then
    for i := Low(FCalibrations) to High(FCalibrations) do
    begin
      aCalib := FCalibrations[i];
      if not (aCalib.NodeA = aCalib.NodeB) then
      begin
        Result := False;
        Msg := Trim(Msg + LineEnding + 'Invalid calibration: ' + aCalib.AsShortString);
        AddMessage(vsError);
        aCalib.IsValid := False;
        aCalib.IsSelected := False;
        SetLength(Invalids, Length(Invalids) + 1);
        Invalids[Length(Invalids) - 1] := aCalib.Index;
      end;
    end;
end;

function TCalibrationsValidator.ValidateAllSampleTimesProvided(var Msg: String): Boolean;
var
  i, index: Integer;
  aCalib: TCalibrationTime;
begin
  Result := True;

  if Length(FCalibrations) > 0 then
  begin
    for i := Low(FCalibrations) to High(FCalibrations) do
    begin
      aCalib := FCalibrations[i];
      index := GetOtuIndex(aCalib.NodeA);
      if index < 0 then
      begin
        Result := False;
        Msg := Trim(Msg + LineEnding + 'Taxa name not found - ' + aCalib.NodeA);
      end
      else
        FTree[Index].Value := aCalib.MinTime;
    end;

    for i := 0 to FNumTaxa - 1 do
    begin
      if FTree[i].IsOutgroupMember then
        continue;

      if not (FTree[i].Value > 0) then
      begin
        Result := False;
        Msg := Trim(Msg + LineEnding + 'Invalid sample time');
        AddMessage(vsError);
        aCalib.IsValid := False;
        aCalib.IsSelected := False;
        SetLength(Invalids, Length(Invalids) + 1);
        Invalids[Length(Invalids) - 1] := aCalib.Index;
      end;
    end;
  end;
end;

function TCalibrationsValidator.ValidateCalibrations(var Msg: String): Boolean;
var
  i: Integer;
  HasMin, HasMax: Boolean;
begin
  Result := False;
  HasMin := False;
  HasMax := False;
  Msg := EmptyStr;

  if Length(FCalibrations) > 0 then
  begin
    for i := 0 to Length(FCalibrations) - 1 do
    begin
      if CompareValue(FCalibrations[i].MinTime, 0.0) > 0 then
        HasMin := True;
      if CompareValue(FCalibrations[i].MaxTime, 0.0) > 0 then
        HasMax := True;

      if HasMin and HasMax then
      begin
        if { (FCalibrations[i].MinTime > 0.0) and (FCalibrations[i].MaxTime > 0.0) and} (CompareValue(FCalibrations[i].MinTime, FCalibrations[i].MaxTime) > 0) then
        begin
          Msg := 'Invalid calibration: ' + FCalibrations[i].AsShortString;
          AddMessage(vsError);
          FCalibrations[i].IsValid := False;
          FCalibrations[i].IsSelected := False;
          SetLength(Invalids, Length(Invalids) + 1);
          Invalids[Length(Invalids) - 1] := i;
          Exit;
        end;
        Result := True;
        AddMessage(vsOk);
        Exit;
      end;
    end;
  end
  else
  begin
    Msg := 'No calibrations provided';
    AddMessage(vsError);
    Exit;
  end;
  Msg := 'When calibrations are used, at least one minimum time and one maximum time are required';
  AddMessage(vsError);
end;

function TCalibrationsValidator.NumOutgroupNodes: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FNumNodes - 1 do
    if FTree[i].IsOutgroupMember then
      inc(Result);
end;

function TCalibrationsValidator.PropogateCalibrationsOverAllNodes( const aData: TTreeData; OtuNames: TStringList; const Calibs: TArrayOfCalibrationTime): TStringList;
var
  i: Integer;
  temp: String;
begin
  Result := TStringList.Create;
  NumTaxa := AData.NoOfOTUs;
  FAdapter := TSimpleTreeDataAdapter.Create;
  FAdapter.SetTreeData(AData, True);
  FTree := FAdapter.GetSimpleTreeReference; { Adapter owns it and will free the memory itself}
  Assert(Length(FTree) = FNumNodes);
  SetLength(FCalibrations, FNumNodes - FNumTaxa);
  for i := 0 to Length(FCalibrations) - 1 do
    FCalibrations[i] := TCalibrationTime.Create;
  if Length(Calibs) > 0 then
    for i := 0 to Length(Calibs) - 1 do
    begin
      FCalibrations[Calibs[i].NodeID].Assign(Calibs[i]);
    end;
  SetRoot;
  SetOtuNames(OtuNames);
  SetCalibrationNames;
  InitMinMaxTimeOfNode(FRoot);
  for i := 0 to Length(FCalibrations) - 1 do
  begin
    temp := FCalibrations[i].AsString;
    Result.Add(temp);
  end;
end;

procedure TCalibrationsValidator.AddMessage(AMsg: TValidationMessage);
begin
  StatusMessages := StatusMessages + [AMsg];
end;

function TCalibrationsValidator.NodeIsRoot(Index: Integer): Boolean;
begin
  if FTree[Index].Ancestor = nil then
    Result := True
  else
    Result := False;
end;

function TCalibrationsValidator.QuickValidation(const Calibs: TArrayOfCalibrationTime; var Msg: String): Boolean;
begin
  FCalibrations := Calibs;
  Result := ValidateCalibrations(Msg);
end;

end.

