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

unit mcalibrationsampler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MCalibrationData, MTreeDataAdapter, MSimpleTreeNode, MTreeData,
  MegaConsts;

const
  MAX_ATTEMPS = 1000;

type

  { TCalibrationDensitySampler }

  TCalibrationDensitySampler = class(TObject)
    private
      FReltimeProgress: Integer;
      FReltimeStatus: String;
      FCancelled: Boolean;
      FTree: TSimpleTreeNodeArray;
      FNumReps: Integer;
      FTreeData: TTreeData;
      FAdapter: TSimpleTreeDataAdapter;
      FCalibrations: TCalibrations;
      FNames: TStringList;
      FIsBlensOnly: Boolean;
      FCorrectionUpSucceeded: Boolean;
      procedure GetDescendantsWithCalibs(aNode: TSimpleTreeNode; var aList: TList);
      procedure AddCalibsToNodes;
      function DoSampleReplicate(repNum: Integer): Boolean;
      procedure DoCorrectionUp(repNum: Integer; aNode: TSimpleTreeNode);
      function DoCorrectionDown(repNum: Integer; aNode: TSimpleTreeNode): Boolean;
      function TryCorrectionUp(ancCalib, desCalib: TCalibrationTime; repNum: Integer): Boolean;
      function TryCorrectionDown(ancCalib, desCalib: TCalibrationTime; repNum: Integer): Boolean;
      procedure DoCheckCancel;
    protected

    public
      CheckCancelFunc: TProgressAndStatusCheckCancel;
      constructor Create(calibs: TCalibrations; aTreeData: TTreeData; otuNames: TStringList; blensOnly: Boolean);
      destructor Destroy; override;
      function DoSampling: Boolean;
      procedure DoReltime(aTreeData: TTreeData; MaxRateRatio: Extended);
      procedure OutputDeveloperData;
  end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}MegaUtils_NV,{$ENDIF}
  mreltimecomputer;

{ TCalibrationDensitySampler }

procedure TCalibrationDensitySampler.GetDescendantsWithCalibs(aNode: TSimpleTreeNode; var aList: TList);
var
  des1: TSimpleTreeNode = nil;
  des2: TSimpleTreeNode = nil;
begin
  if Assigned(aNode.des1) then
  begin
    des1 := aNode.des1;
    if Assigned(des1.MyObject) and (des1.MyObject is TCalibrationTime) and (aList.IndexOf(des1) < 0) then
      aList.Add(des1);
  end;
  if Assigned(aNode.des2) then
  begin
    des2 := aNode.des2;
    if Assigned(des2.MyObject) and (des2.MyObject is TCalibrationTime) and (aList.IndexOf(des2) < 0) then
      aList.Add(des2);
  end;
  if Assigned(des1) then
    GetDescendantsWithCalibs(des1, aList);
  if Assigned(des2) then
    GetDescendantsWithCalibs(des2, aList);
end;

procedure TCalibrationDensitySampler.AddCalibsToNodes;
var
  i: Integer;
  c: TCalibrationTime;
begin
  if FCalibrations.Count > 0 then
    for i := 0 to FCalibrations.Count - 1 do
    begin
      c := FCalibrations.GetCalibration(i);
      FTree[c.ReltimeTreeIndex].MyObject := c;
    end;
end;

function TCalibrationDensitySampler.DoSampleReplicate(repNum: Integer): Boolean;
var
  i: Integer;
  c: TCalibrationTime;
begin
  Result := False;
  if FCalibrations.Count > 0 then
  begin
    Result := True;
    for i := 0 to FCalibrations.Count - 1 do
    begin
      c := FCalibrations.GetCalibration(i);
      c.DoCalibrationDensitySample(repNum);
    end;
  end;
end;

procedure TCalibrationDensitySampler.DoCorrectionUp(repNum: Integer; aNode: TSimpleTreeNode);
var
  ancCalib, myCalib: TCalibrationTime;
  anc: TSimpleTreeNode;
begin
  if Assigned(aNode.Des1) then
    DoCorrectionUp(repNum, aNode.Des1);
  if Assigned(aNode.Des2) then
    DoCorrectionUp(repNum, aNode.Des2);
  if Assigned(aNode.MyObject) and Assigned(aNode.Ancestor) then
  begin
    Assert(aNode.MyObject is TCalibrationTime);
    myCalib := TCalibrationTime(aNode.MyObject);
    anc := aNode.Ancestor;
    while Assigned(anc) do
    begin
      if Assigned(anc.MyObject) then
      begin
        ancCalib := TCalibrationTime(anc.MyObject);
        if ancCalib.GetSamplingMaxTime(repNum) < myCalib.GetSamplingMinTime(repNum) then
          if not TryCorrectionUp(ancCalib, myCalib ,repNum) then
            FCorrectionUpSucceeded := False;
      end;
      if not Assigned(anc.Ancestor) then
      begin
        Assert(anc = FAdapter.Root, 'invalid root node found when doing calibration correction');
      end;
      anc := anc.Ancestor;
    end;
  end;
end;

function TCalibrationDensitySampler.DoCorrectionDown(repNum: Integer; aNode: TSimpleTreeNode): Boolean;
var
  desCalib, myCalib: TCalibrationTime;
  des: TSimpleTreeNode;
  descendants: TList = nil;
  i: Integer;
begin
  Result := False;
  try
    if Assigned(aNode.MyObject) then
    begin
      descendants := TList.Create;
      GetDescendantsWithCalibs(aNode, descendants);
      if descendants.Count > 0 then
      begin
        myCalib := TCalibrationTime(aNode.MyObject);
        for i := 0 to descendants.Count - 1 do
        begin
          des := TSimpleTreeNode(descendants[i]);
          desCalib := TCalibrationTime(des.MyObject);
          if desCalib.GetSamplingMinTime(repNum) > myCalib.GetSamplingMaxTime(repNum) then
            if not TryCorrectionDown(myCalib, desCalib, repNum) then
              raise Exception.Create(Format('calibration sampling failed because after 1000 attempts, the max time for ancestral node %d is less than the min time for descendant node %d', [aNode.NodeIndex, des.NodeIndex]));
        end
      end
      else
        Result := True;
    end;
    if Assigned(aNode.Des1) then
      DoCorrectionDown(repNum, aNode.Des1);
    if Assigned(aNode.Des2) then
      DoCorrectionDown(repNum, aNode.Des2);
  finally
    if Assigned(descendants) then
      descendants.Free;
  end;
end;

function TCalibrationDensitySampler.TryCorrectionUp(ancCalib, desCalib: TCalibrationTime; repNum: Integer): Boolean;
var
  numAttempts: Integer = 0;
  desMin, ancMax: Extended;
begin
  Result := False;
  desMin := desCalib.GetSamplingMinTime(repNum);
  ancMax := ancCalib.GetSamplingMaxTime(repNum);
  while (ancMax < desMin) and (numAttempts < MAX_ATTEMPS) do
  begin
    ancCalib.DoCalibrationDensitySample(repNum);
    ancMax := ancCalib.GetSamplingMaxTime(repNum);
    inc(numAttempts);
  end;
  Result := (ancMax >= desMin);
end;

function TCalibrationDensitySampler.TryCorrectionDown(ancCalib, desCalib: TCalibrationTime; repNum: Integer): Boolean;
var
  numAttempts: Integer = 0;
  desMin, ancMax: Extended;
begin
  Result := False;
  desMin := desCalib.GetSamplingMinTime(repNum);
  ancMax := ancCalib.GetSamplingMaxTime(repNum);
  while (ancMax < desMin) and (numAttempts < MAX_ATTEMPS) do
  begin
    desCalib.DoCalibrationDensitySample(repNum);
    desMin := desCalib.GetSamplingMaxTime(repNum);
    inc(numAttempts);
  end;
  Result := (ancMax >= desMin);
end;

procedure TCalibrationDensitySampler.DoCheckCancel;
begin
  if Assigned(CheckCancelFunc) then
    FCancelled := CheckCancelFunc(FReltimeProgress, 'Status', FReltimeStatus);
end;

procedure TCalibrationDensitySampler.DoReltime(aTreeData: TTreeData; MaxRateRatio: Extended);
var
  clockTree: TTreeData = nil;
  minTimes, maxTimes: TDivTimesArray;
  i, j: Integer;
  computer: TReltimeComputer = nil;
  calib: TCalibrationTime = nil;
begin
  SetLength(minTimes, 0);
  SetLength(maxTimes, 0);
  FTreeData := aTreeData;
  if FIsBlensOnly then
    for i := 0 to FCalibrations.Count - 1 do
    begin
      calib := FCalibrations.GetCalibration(i);
      calib.ReltimeTreeIndexBLens := calib.NodeId + aTreeData.NoOfOtus;
    end;

  for i := 0 to FNumReps - 1 do
  begin
    FCalibrations.PrepareSamplingDivTimeArrays(minTimes, maxTimes, FAdapter.NumTaxa, FIsBlensOnly, i);
    try
      computer := TReltimeComputer.Create;
      if FIsBlensOnly then
        computer.IsBlensOnly := True;
      clockTree := computer.ComputeDensityDistributionTree(aTreeData, MaxRateRatio, minTimes, maxTimes);
      for j := 0 to FCalibrations.Count - 1 do
      begin
        calib := FCalibrations.GetCalibration(j);
        calib.SetSamplingDivTime(i, computer.DivTime(calib.ReltimeTreeIndex));
      end;
    finally
      if Assigned(computer) then
        FreeAndNil(computer);
      if Assigned(clockTree) then
        FreeAndNil(clockTree);
    end;

    if i mod 100 = 0 then
    begin
      FReltimeProgress := Round(i/FNumReps*100);
      DoCheckCancel;
    end;
  end;

  for i := 0 to FCalibrations.Count - 1 do
  begin
    calib := FCalibrations.GetCalibration(i);
    calib.FinalizeCalibrationDensity;
  end;
end;

procedure TCalibrationDensitySampler.OutputDeveloperData;
{$IFNDEF VISUAL_BUILD}
var
  i: Integer;
  c: TCalibrationTime = nil;
{$ENDIF}
begin
  {$IFNDEF VISUAL_BUILD}
  if IsDeveloper then
  begin
    for i := 0 to FCalibrations.Count - 1 do
    begin
      c := FCalibrations.GetCalibration(i);
      if c.IsStatisticalDensity then
      begin
        if FIsBlensOnly then
        begin
          c.SamplingDataToFile(NextAvailableFilenameNV(Format('_node_%d_density_samples.txt', [c.ReltimeTreeIndexBLens + 1])));
          c.SamplingHistogramDataToFile(NextAvailableFilenameNV(Format('_node_%d_histogram.csv', [c.ReltimeTreeIndexBLens + 1])));
        end
        else
        begin
          c.SamplingDataToFile(NextAvailableFilenameNV(Format('_node_%d_density_samples.txt', [c.ReltimeTreeIndex + 1])));
          c.SamplingHistogramDataToFile(NextAvailableFilenameNV(Format('_node_%d_histogram.csv', [c.ReltimeTreeIndex + 1])));
        end;
      end;
    end;
  end;
  {$ENDIF}
end;

constructor TCalibrationDensitySampler.Create(calibs: TCalibrations; aTreeData: TTreeData; otuNames: TStringList; blensOnly: Boolean);
begin
  CheckCancelFunc := nil;
  FCancelled := False;
  FReltimeProgress := 0;
  FReltimeStatus := 'Computing Reltime Density Trees';
  FIsBlensOnly := blensOnly;
  FCalibrations := calibs;
  FTreeData := aTreeData;
  FNames := otuNames;
  FAdapter := TSimpleTreeDataAdapter.Create;
  FAdapter.SetTreeData(FTreeData, True, FNames);
  FTree := FAdapter.GetSimpleTreeReference;
  FNumReps := NUM_DEFAULT_CALIBRATION_SAMPLING_REPS;
  AddCalibsToNodes;
end;

destructor TCalibrationDensitySampler.Destroy;
begin
  if Assigned(FAdapter) then
    FAdapter.Free;
  inherited Destroy;
end;

function TCalibrationDensitySampler.DoSampling: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FNumReps - 1 do
    DoSampleReplicate(i);

  for i := 0 to FNumReps - 1 do
  begin
    FCorrectionUpSucceeded := True;
    DoCorrectionUp(i, FAdapter.Root);
    if not FCorrectionUpSucceeded then
      DoCorrectionDown(i, FAdapter.Root);
  end;
  Result := True;
end;


end.

