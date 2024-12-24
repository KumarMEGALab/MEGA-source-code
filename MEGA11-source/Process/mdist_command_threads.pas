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

unit mdist_command_threads;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MAnalysisInfo, MegaConsts, MDistPack, MRuntimeProgressDlg,
  MSeqDistBase, MAminoDist, MNucDist, mdist_command_results_wrapper,
  MSynNonsynDist, MegaUtils_NV;

type

  { TDistCommandThread }

  TDistCommandThread = class(TMegaThread)
    private
      FInternalErrorId: Integer;
      FIsSuccess: Boolean;
    protected
      FIsOwnerOfResultsData: Boolean;
      FLog: TStringList;
      FIsWorkflowCalculation: Boolean;
      TempDouble: Double;
      MyD, MyV : PDistanceMatrix;
      MyMeanGpD, MyMeanGpV: ArrayOfDouble; // within gp means
      MyMeanD, MyMeanV: Double; // overall means
      FProgress: Integer;
      FStatus: AnsiString;
      FCancelled: Boolean;
      FStatusInfo: AnsiString;
      ARP : TRuntimeProgress;
      function GetPValueForStatTestUsesTempDouble(DiffValue, VarValue: Double): Double;
      procedure AllocateMatrices; virtual;
      procedure AllocateDistComputer; virtual;
      procedure ComputeDistances; virtual;
      procedure UpdateMatricesForExactTest; virtual;
      function DoExecute: Boolean; virtual;
      procedure Execute; override;
      procedure Clear; virtual;
      procedure DoDistComputerCheckCancel;
      function DistComputerCheckCancel(Progress: integer; Status: AnsiString): Boolean;
      procedure DoDistComputerUpdateRunStatusInfo;
      procedure DistComputerUpdateRunStatusInfo(aKey: AnsiString; aInfo: AnsiString);
      procedure DoDistComputerCheckAbort;
      function DistComputerCheckAbort: Boolean;
    public
      MAI: TAnalysisInfo;
      UsrOperation: TDistTreeDlgOption;
      DistComputer:  TSeqDistBase;
      IsDisparityTest: Boolean;
      IsSelTest: Boolean;
      constructor  Create(analysisInfo: TAnalysisInfo; aOperation: TDistTreeDlgOption);
      destructor Destroy; override;
      function GetResultsData: TDistCommandResultsData;
      property Log: TStringList read FLog;
      property IsSuccess: Boolean read FIsSuccess;
      property IsCancelled: Boolean read FCancelled;
      property InternalErrorId: Integer read FInternalErrorId;
  end;

  { TDistMatrixThread }

  TDistMatrixThread = class(TMEGAThread)
    private
      ARP: TRuntimeProgress;
      FBootstrapDoFullProgress: Boolean;
      FInternalErrorId: Integer;
      FIsCancelled: Boolean;
      FIsSuccess: Boolean;
      FProgress: Integer;
      FStatus: AnsiString;
      FStatusInfo: AnsiString;
      FLog: TStringList;
      DistComputer: TSeqDistBase;
      ADMat : PDistanceMatrix;
      UsrOperation: TDistTreeDlgOption;
    protected
      function DoGenerateDistMatrix: Boolean;
      procedure DoDistComputerCheckCancel;
      function DistComputerCheckCancel(Progress: integer; Status: AnsiString): Boolean;
      procedure DoDistComputerUpdateRunStatusInfo;
      procedure DistComputerUpdateRunStatusInfo(aKey: AnsiString; aInfo: AnsiString);
      procedure Execute; override;
    public
      MAI: TAnalysisInfo;
      constructor Create(aDistComputer: TSeqDistBase; aMAI: TAnalysisInfo; dMat: PDistanceMatrix; aUsrOperation: TDistTreeDlgOption);
      destructor Destroy; override;
      property IsSuccess: Boolean read FIsSuccess;
      property IsCancelled: Boolean read FIsCancelled;
      property Log: TStringList read FLog;
      property InternalErrorId: Integer read FInternalErrorId;
      property BootstrapDoFullProgress: Boolean read FBootstrapDoFullProgress;
  end;

  { TDistTreeSearchThread }

  TDistTreeSearchThread = class(TMegaThread)
    private
      FBootstrapDoFullProgress: Boolean;
    protected
      distMatrixThread: TDistMatrixThread;
      ARP: TRuntimeProgress;
      FIsCancelled: Boolean;
      FIsSuccess: Boolean;
      FProgress: Integer;
      FStatus: AnsiString;
      FStatusInfo: AnsiString;
      FLog: TStringList;
      DistComputer: TSeqDistBase;
      ADMat : PDistanceMatrix;
      UsrOperation: TDistTreeDlgOption;
      procedure Clear;
      function LaunchTreeSearchThread: Boolean;
      function DoExecute: Boolean;
      procedure Execute; override;
      procedure DoUpdateRunStatusInfo;
      procedure UpdateRunStatusInfo(aStatus: AnsiString; aInfo: AnsiString);
    public
      MAI: TAnalysisInfo;
      constructor Create(aDistComputer: TSeqDistBase; aMAI: TAnalysisInfo; dMat: PDistanceMatrix; aUsrOperation: TDistTreeDlgOption);
      destructor Destroy; override;
      property IsSuccess: Boolean read FIsSuccess;
      property IsCancelled: Boolean read FIsCancelled;
      property Log: TStringList read FLog;
      property BootstrapDoFullProgress: Boolean read FBootstrapDoFullProgress;
  end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MD_MegaMain,
  {$ENDIF}
  MegaUtils, MD_InputSeqData, mtreepack, dateutils, ProcessTreeCmds, MTreeList,
  MPartitionList;

{ TDistTreeSearchThread }

procedure TDistTreeSearchThread.Clear;
begin
  if Assigned(ADMat) and Assigned(MAI) then
    FreeDistMatrix(ADMat, MAI.NoOfSeqs);
  if Assigned(MAI) then
    FreeAndNil(MAI);
  if Assigned(DistComputer) then
    FreeAndNil(DistComputer);
  if Assigned(ARP) then
    ARP.Hide;
  ARP := nil;
  if Assigned(MAI) then
    FreeAndNil(MAI);
end;

function TDistTreeSearchThread.LaunchTreeSearchThread: Boolean;
var
  i,j: Integer;
  ATreeList: TTreeList = nil;
begin
  Result := True;
  try
    try
      ATreeList := TTreeList.Create;
      FBootstrapDoFullProgress := distMatrixThread.BootstrapDoFullProgress;
      if Assigned(distMatrixThread) then
        FreeAndNil(distMatrixThread);

      for i:=0 to MAI.MyNoOfSeqs-1 do
        ADMat[i,i] := 0;
      for i:=0 to MAI.MyNoOfSeqs-1 do
        for j:=0 to i-1 do
          ADMat[j,i] := ADMat[i,j];

      if not (UsrOperation = dtdoPhyloQ) then
      begin
        MAI.MyShowD := NewDistMatrix(MAI.MyNoOfSeqs, True);
        CopyDistMatrix(MAI.MyNoOfSeqs, MAI.MyShowD, ADMat, True);
      end;

      if not MAI.MyTreePack.DoesContain(ttBootstrap) then
        UpdateRunStatusInfo('Status', 'Computing tree');

      if ((UsrOperation = dtdoOLSComputeUserTreeBLens) or (UsrOperation = dtdoPhyloQ)) then
      begin
        MAI.MyOriD := ADMat;
        {$IFNDEF VISUAL_BUILD}
        if UsrOperation = dtdoPhyloQ then
          begin
            (MAI as TPhyloQAnalysisInfo).UpdateAugmentedMatrix;
             MAI.MyShowD := NewDistMatrix(MAI.MyNoOfSeqs + 1, True);
             CopyDistMatrix(MAI.MyNoOfSeqs + 1, MAI.MyShowD, MAI.MyOriD, True);
          end;
        {$ENDIF}

        ADMat := nil;
        ProcessTreeCmds.ComputeTree(MAI, True);
        ARP := nil;
        MAI := nil;
      end
      else if (UsrOperation = dtdoRelTimeLS) or (UsrOperation = dtdoRtdtLS) then
      begin
        MAI.MyOriD := ADMat;
        ADMat := nil;
        ProcessTreeCmds.ComputeRelTimeLSTree(MAI, True);
        MAI := nil;
        ARP := nil;
      end
      else if MAI.MyTreePack.DoesContain(ttBootstrap) then
      begin
        MAI.MyBootD := NewDistMatrix(MAI.MyNoOfSeqs, True);
        CopyDistMatrix(MAI.MyNoOfSeqs, MAI.MyBootD, ADMat, True);
        MAI.MyOriD := ADMat;
        ADMat := nil;
        UpdateRunStatusInfo('Status', 'Conducting Bootstrap Test');
        SetLength(MAI.MySiteFreqs, MAI.MyNoOfSites);
        DistComputer.FreqTable    := MAI.MySiteFreqs;
        DistComputer.FreqTableLen := MAI.MyNoOfSites;

        if MAI.MyOriTreeList = nil then
        begin
          MAI.MyOriTreeList  := ATreeList;
          ATreeList := nil;
        end;

        MAI.MyBootPartitionList := TPartitionList.Create(MAI.MyNoOfSeqs, 0, false);

        DistComputer.D    := MAI.MyBootD;
        MAI.DistComputer  := DistComputer;
        DistComputer := nil;
        MAI.MyNoOfReps    := MAI.MyTreePack.BootReps;

        ProcessTreeCmds.ComputeBootstrapTree(MAI, BootstrapDoFullProgress);

        ARP := nil;
        MAI := nil;
      end
      else if MAI.MyTreePack.DoesContain(ttCPTest) then
      begin
        MAI.MyBootD := NewDistMatrix(MAI.MyNoOfSeqs, True);
        CopyDistMatrix(MAI.MyNoOfSeqs, MAI.MyBootD, ADMat, True);
        MAI.MyOriD := ADMat;
        ADMat := nil;

        UpdateRunStatusInfo('Status', 'Conducting Interior-Branch Test');
        SetLength(MAI.MySiteFreqs, MAI.MyNoOfSites);
        DistComputer.FreqTable    := MAI.MySiteFreqs;
        DistComputer.FreqTableLen := MAI.MyNoOfSites;

        if MAI.MyOriTreeList = nil then
        begin
          MAI.MyOriTreeList := ATreeList;
          ATreeList := nil;
        end;

        DistComputer.D    := MAI.MyBootD;
        MAI.DistComputer  := DistComputer;
        DistComputer      := nil;
        MAI.MyNoOfReps    := MAI.MyTreePack.BootReps;

        ProcessTreeCmds.ConductCPTest(MAI);

        ARP := nil;
        MAI := nil;
      end
      else if not (MAI.MyTreePack.DoesContain(ttBootstrap)) then
      begin
        MAI.MyOriD := ADMat;
        ADMat := nil;
        MAI.MyOriTreeList := ATreeList;
        ATreeList := nil;
        ProcessTreeCmds.ComputeTree(MAI, True);
        MAI := nil;
        ARP := nil;
      end;
    except
      on E:Exception do
      begin
        FLog.Add(E.Message);
        Result := False;
      end;
    end;
  finally
    if Assigned(MAI) then
      MAI.Free;
    if Assigned(DistComputer) then
      DistComputer.Free;
    if Assigned(ADMat) then
      FreeDistMatrix(ADMat, MAI.MyNoOfSeqs);
    if Assigned(ATreeList) then
      ATreeList.Free;
  end;
end;

function TDistTreeSearchThread.DoExecute: Boolean;
begin
  Result := False;
  distMatrixThread := TDistMatrixThread.Create(DistComputer, MAI, ADMat, UsrOperation);
  distMatrixThread.Start;
  distMatrixThread.WaitFor;
  if distMatrixThread.IsCancelled then
  begin
    Clear;
    raise EAbort.Create('computation cancelled by user');
  end;
  if not distMatrixThread.IsSuccess then
  begin
    if distMatrixThread.InternalErrorId >= 0 then
      raise Exception.Create(Format('distance matrix could not be computed. Error Id is %d (%s)', [distMatrixThread.InternalErrorId, distMatrixThread.Log.Text]))
    else
      raise Exception.Create('dist matrix computation failed: ' + distMatrixThread.Log.Text);
  end;
  Result := LaunchTreeSearchThread;
end;

procedure TDistTreeSearchThread.Execute;
begin
  try
    FIsSuccess := DoExecute;
    Terminate;
    Exit;
  except
    on E:EAbort do
    begin
      FLog.Add(E.Message);
      Terminate;
      Exit;
    end;
    on E:Exception do
    begin
      FLog.Add(E.Message);
      FIsSuccess := False;
      Clear;
      Terminate;
      Exit;
    end;
  end;
end;

procedure TDistTreeSearchThread.DoUpdateRunStatusInfo;
begin
  if Assigned(ARP) then
    ARP.UpdateRunStatusInfo(FStatus, FStatusInfo);
end;

procedure TDistTreeSearchThread.UpdateRunStatusInfo(aStatus: AnsiString; aInfo: AnsiString);
begin
  FStatus := aStatus;
  FStatusInfo := aInfo;
  Synchronize(DoUpdateRunStatusInfo);
end;

constructor TDistTreeSearchThread.Create(aDistComputer: TSeqDistBase; aMAI: TAnalysisInfo; dMat: PDistanceMatrix; aUsrOperation: TDistTreeDlgOption);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FLog := TStringList.Create;
  MAI := aMAI;
  ARP := MAI.ARP;
  ADMat := dMat;
  DistComputer := aDistComputer;
  UsrOperation := aUsrOperation;
end;

destructor TDistTreeSearchThread.Destroy;
begin
  MAI := nil;
  ADMat := nil;
  DistComputer := nil;
  if Assigned(FLog) then
    FLog.Free;
  inherited Destroy;
end;

{ TDistMatrixThread }

function TDistMatrixThread.DoGenerateDistMatrix: Boolean;
var
  i, j: Integer;
  StartTime: TDateTime;
  EndTime: TDateTime;
begin
  Result := False;
  ARP := MAI.ARP;
  with MAI.MyDistPack do
    if      DoesContain(gdOneNuc)    then
      DistComputer := TNucDist.Create
    else if DoesContain(gdSynNonsyn)
      then  DistComputer := TSynNonsynDist.Create
    else if DoesContain(gdAmino)
      then  DistComputer := TAminoDist.Create;

  DistComputer.ProgressCheckCancelFunc := DistComputerCheckCancel;
  DistComputer.RunStatusProc := DistComputerUpdateRunStatusInfo;

  with DistComputer do
  begin
    DistPack  := MAI.MyDistPack;
    NoOfSeqs  := MAI.MyMappedData.Count;
    Sequences := MAI.MyMappedData;
    QuickExit := True;
    D         := ADMat;
    NoOfSites := MAI.MyNoOfSites;
  end;
  if DistComputer is TSynNonsynDist  then
    with DistComputer as TSynNonsynDist do
      CodeTable := D_InputSeqData.CodeTable;

  DistComputerUpdateRunStatusInfo('Status', 'Computing distances');
  StartTime := Now;
  if DistComputer is TNucDist then
  begin
    if MAI.MyDistPack.DoesContain(gdMCL) then
      TNucDist(DistComputer).ComputeDistancesSE
    else
      TNucDist(DistComputer).ComputeDistances;
  end
  else if DistComputer is TSynNonsynDist then
    TSynNonsynDist(DistComputer).ComputeDistances
  else if DistComputer is TAminoDist then
    TAminoDist(DistComputer).ComputeDistances;
  EndTime := Now;
  FBootstrapDoFullProgress := (MilliSecondsBetween(EndTime, StartTime) > 5000);
  for i:=0 to MAI.MyNoOfSeqs-1 do
    ADMat[i,i] := 0;
  for i:=0 to MAI.MyNoOfSeqs-1 do
    for j:=0 to i-1 do
      ADMat[j,i] := ADMat[i,j];

  if not (UsrOperation = dtdoPhyloQ) then
  begin
    MAI.MyShowD := NewDistMatrix(MAI.MyNoOfSeqs, True);
    CopyDistMatrix(MAI.MyNoOfSeqs, MAI.MyShowD, ADMat, True);
  end;
  Result := True;
  if not MAI.MyTreePack.DoesContain(ttBootstrap) then
    DistComputerUpdateRunStatusInfo('Status', 'Computing tree');
end;

procedure TDistMatrixThread.DoDistComputerCheckCancel;
begin
  if Assigned(ARP) then
    FIsCancelled := ARP.ProgressAndStatusCheckCancel(FProgress, 'Status', FStatusInfo);
end;

function TDistMatrixThread.DistComputerCheckCancel(Progress: integer; Status: AnsiString): Boolean;
begin
  FProgress := Progress;
  if Trim(Status) <> EmptyStr then
    FStatus := Status;
  Synchronize(DoDistComputerCheckCancel);
  Result := FIsCancelled;
end;

procedure TDistMatrixThread.DoDistComputerUpdateRunStatusInfo;
begin
  if Assigned(ARP) then
    ARP.UpdateRunStatusInfo(FStatus, FStatusInfo);
end;

procedure TDistMatrixThread.DistComputerUpdateRunStatusInfo(aKey: AnsiString; aInfo: AnsiString);
begin
  FStatus := aKey;
  if Trim(aInfo) <> EmptyStr then
    FStatusInfo := aInfo;
  Synchronize(DoDistComputerUpdateRunStatusInfo);
end;

procedure TDistMatrixThread.Execute;
begin
  try
    FIsSuccess := DoGenerateDistMatrix;
  except
    on E:EAbort do
    begin
      ARP := nil;
      MAI.ARP := nil;
      FIsSuccess := False;
      FIsCancelled := True;
      FLog.Add('distance matrix construction was aborted');
    end;
    on E:EDistComputationError do
    begin
      FInternalErrorId := E.ErrorId;
      FLog.Add(E.Message);
      FIsSuccess := False;
    end;
    on E:Exception do
    begin
      FIsSuccess := False;
      FLog.Add(E.Message);
    end;
  end;
  Terminate;
  Exit;
end;

constructor TDistMatrixThread.Create(aDistComputer: TSeqDistBase; aMAI: TAnalysisInfo; dMat: PDistanceMatrix; aUsrOperation: TDistTreeDlgOption);
begin
  inherited Create(True);
  FInternalErrorId := -1;
  FIsSuccess := True;
  FIsCancelled := False;
  FBootstrapDoFullProgress := False;
  MAI := aMAI;
  FreeOnTerminate := False;
  FLog := TStringList.Create;
  DistComputer := aDistComputer;
  aDMat := dMat;
  UsrOperation := aUsrOperation;
  FStatus := 'Initializing';
  FStatusInfo := 'Initializing...';
end;

destructor TDistMatrixThread.Destroy;
begin
  ARP := nil;
  MAI := nil;
  DistComputer := nil;
  if Assigned(FLog) then
    FLog.Free;
  inherited Destroy;
end;

{ TDistCommandThread }

function TDistCommandThread.GetPValueForStatTestUsesTempDouble(DiffValue, VarValue: Double): Double;
begin
  TempDouble := VarValue;
  // if invalid then move on
  if DiffValue <= InvalidDistValue then
  begin
     Result := DiffValue;
     Exit;
  end;
  // if no diff then nothing to test
  if DiffValue = 0 then
  begin
     Result := 1.0;
     Exit;
  end;
  // now do specific tests
  if MAI.MyDistpack.DoesContain(gdNeutralEvolTest) then
  begin
     if VarValue = 0 then
     begin
        Result := 0.0;  // does not matter what Diff is, as long as it is not 0
        TempDouble := 1000; // just to show a large value
        Exit;
     end;
     TempDouble := DiffValue/SQRT(VarValue);
     Result := tTest(abs(TempDouble));
     Exit;
  end;
  // otherwise we have two other tests
  TempDouble := DiffValue/SQRT(VarValue);
  // For both other tests, this statistic shoudl be >0 as it is either dN-dS (adapt) or ds-dn (purifying)
  if TempDouble <= 0 then
  begin
    Result := 1.0;
    Exit;
  end;
  Result := tTest(TempDouble)/2;
end;

procedure TDistCommandThread.AllocateMatrices;
begin
  case UsrOperation of
    dtdoPhyloQ:                   MyD := NewDistMatrix(MAI.MyNoOfSeqs - 1, False); // because the first sequence is not used for the initial dmatrix
    dtdoPairwiseDist,
    dtdoDisparityIndexTest,
    dtdoCompositionDistance,
    dtdoDisparityIndex, dtdoMCLComputePattern, dtdoMCLTsTvBias: MyD := NewDistMatrix(MAI.MyNoOfSeqs, False);
    dtdoWithinGroupMean:          SetLength(MyMeanGpD, MAI.MyNoOfGps);
    dtdoBetweenGroupMean:         MyD := NewDistMatrix(MAI.MyNoOfGps, False);
    dtdoNetGroupMean:             MyD := NewDistMatrix(MAI.MyNoOfGps, False);
    dtdoOverallMean:              SetLength(MyMeanGpD, 1);
    dtdoAvgDiversityForEntirePop: SetLength(MyMeanGpD, 1);
    dtdoAvgDiversityWithinSubPops:SetLength(MyMeanGpD, 1);
    dtdoInterPopDiversity:        SetLength(MyMeanGpD, 1);
    dtdoPropOfInterPopDiversity:  SetLength(MyMeanGpD, 1);
  end;

  if MAI.MyDistPack.VarType <> gdNone then
    case UsrOperation of
      dtdoPhyloQ:                   MyV := NewDistMatrix(MAI.MyNoOfSeqs - 1, False); // because the first sequence is not included in the original dmatrix
      dtdoPairwiseDist,
      dtdoDisparityIndexTest,
      dtdoCompositionDistance,
      dtdoDisparityIndex:           MyV := NewDistMatrix(MAI.MyNoOfSeqs, False);
      dtdoWithinGroupMean:          SetLength(MyMeanGpV, MAI.MyNoOfGps);
      dtdoBetweenGroupMean:         MyV := NewDistMatrix(MAI.MyNoOfGps, False);
      dtdoNetGroupMean:             MyV := NewDistMatrix(MAI.MyNoOfGps, False);
      dtdoOverallMean:              SetLength(MyMeanGpV, 1);
      dtdoAvgDiversityForEntirePop: SetLength(MyMeanGpV, 1);
      dtdoAvgDiversityWithinSubPops:SetLength(MyMeanGpV, 1);
      dtdoInterPopDiversity:        SetLength(MyMeanGpV, 1);
      dtdoPropOfInterPopDiversity:  SetLength(MyMeanGpV, 1);
      dtdoMCLTsTvBias, dtdoMCLComputePattern: MyV := NewDistMatrix(MAI.MyNoOfSeqs, False);
    end;
end;

procedure TDistCommandThread.AllocateDistComputer;
begin
  with MAI.MyDistPack do
    if DoesContain(gdOneNuc) then            DistComputer := TNucDist.Create
    else if DoesContain(gdSynNonsyn) then    DistComputer := TSynNonsynDist.Create
    else if DoesContain(gdAmino) then        DistComputer := TAminoDist.Create
    else
      raise Exception.Create('DistComputer not initialized');

  DistComputer.ProgressCheckCancelFunc := DistComputerCheckCancel;
  DistComputer.CheckAbortFunc := DistComputerCheckAbort;
  DistComputer.RunStatusProc := DistComputerUpdateRunStatusInfo;

  with DistComputer do
  begin
    DistPack  := MAI.MyDistPack;
    NoOfSeqs  := MAI.MyMappedData.Count;
    Sequences := MAI.MyMappedData;
    QuickExit := False;  //<- problematic as overall distance cannot be computed

    case UsrOperation of
      dtdoWithinGroupMean,
      dtdoBetweenGroupMean,
      dtdoNetGroupMean,
      dtdoAvgDiversityWithinSubPops,
      dtdoInterPopDiversity,
      dtdoPropOfInterPopDiversity:
      begin
        NoOfGps := MAI.MyNoOfGps;
        GpId    := MAI.MyGpIds;
      end;
    end;

    case UsrOperation of
      dtdoPairwiseDist,
      dtdoDisparityIndexTest,
      dtdoCompositionDistance,
      dtdoDisparityIndex, dtdoMCLComputePattern, dtdoMCLTsTvBias: D := MyD;
      dtdoOverallMean:               MeanD   := @MyMeanD;
      dtdoWithinGroupMean:           MeanGpD := MyMeanGpD;
      dtdoBetweenGroupMean:          GpD     := MyD;
      dtdoNetGroupMean:              GpD     := MyD;
      dtdoAvgDiversityForEntirePop:  MeanD := @MyMeanD;
      dtdoAvgDiversityWithinSubPops: MeanD := @MyMeanD;
      dtdoInterPopDiversity:         MeanD := @MyMeanD;
      dtdoPropOfInterPopDiversity:   MeanD := @MyMeanD;
    end;

    if MAI.MyDistPack.VarType <> gdNone then
      case UsrOperation of
        dtdoPairwiseDist:              V  := MyV;
        dtdoOverallMean:               MeanV := @MyMeanV;
        dtdoWithinGroupMean:           MeanGpV := MyMeanGpV;
        dtdoBetweenGroupMean:          GpV     := MyV;
        dtdoNetGroupMean:              GpV     := MyV;
        dtdoAvgDiversityForEntirePop:  MeanV   := @MyMeanV;
        dtdoAvgDiversityWithinSubPops: MeanV   := @MyMeanV;
        dtdoInterPopDiversity:         MeanV   := @MyMeanV;
        dtdoPropOfInterPopDiversity:   MeanV   := @MyMeanV;
        dtdoDisparityIndexTest,
        dtdoCompositionDistance,
        dtdoDisparityIndex:
          begin
            if MAI.MyDistPack.VarType = gdMonteCarloTest then
              V := MyV;
          end;
        dtdoMCLComputePattern, dtdoMCLTsTvBias:
          begin
            if MAI.MyDistPack.VarType = gdBootstrapVar then
              V := MyV;
          end;
      end;
  end;
end;

procedure TDistCommandThread.ComputeDistances;
begin
  with DistComputer do
  begin
    FProgress := 0;
    FStatusInfo := 'Computing distances';
    Synchronize(DoDistComputerUpdateRunStatusInfo);
    NoOfSites := MAI.MyNoOfSites;
    if DistComputer is TNucDist then
      with DistComputer as TNucDist do
        if DistPack.DoesContain(gdMCL) or DistPack.DoesContain(gdMCLComputePattern) or DistPack.DoesContain(gdMCLTsTvBias) then
          ComputeDistancesSE
        else
          ComputeDistances
    else
    if DistComputer is TSynNonsynDist  then
      with DistComputer as TSynNonsynDist do
      begin
        NoOfSites := MAI.MyNoOfSites;
        CodeTable := D_InputSeqData.CodeTable;
        ShowInProgressBar := True;
        ComputeDistances;
      end
    else
    if DistComputer is TAminoDist then
      with DistComputer as TAminoDist do
        ComputeDistances;
  end;
end;

procedure TDistCommandThread.UpdateMatricesForExactTest;
var
  i, j: Integer;
begin
  with MAI do
  begin
    if MyDistPack.DoesContain(gdTestRandom) and (MyV <> nil) then
    begin
      // this is a hidden function
      for i:=0 to MyNoOfSeqs-1 do
        for j:=0 to i-1 do
          if MyD[i][j] > InvalidDistValue then
          begin
            if MyV[i][j] = 0 then
              raise Exception.create('MyV variable is somehow zero');
            TempDouble := MyD[i][j]/SQRT(MyV[i][j]);
            MyV[i][j] := TempDouble;
            if TempDouble <= 0 then  MyD[i][j] := 1.0
            else                     MyD[i][j] := tTest(TempDouble)/2;
            MyV[i][j] := abs(TempDouble); // this is the test stat
          end;
    end
    else if IsSelTest and (not MyDistpack.DoesContain(gdExactTest)) then
    begin
      // for Z-test
      case UsrOperation of
        dtdoPairwiseDist:
          for i:=0 to MyNoOfSeqs-1 do
            for j:=0 to i-1 do
            begin
              MyD[i][j] := GetPValueForStatTestUsesTempDouble(MyD[i][j], MyV[i][j]);
              MyV[i][j] := TempDouble;
            end;
        dtdoBetweenGroupMean,
        dtdoNetGroupMean:
          for i:=0 to MyNoOfGps-1 do
            for j:=0 to i-1 do
            begin
              MyD[i][j] := GetPValueForStatTestUsesTempDouble(MyD[i][j], MyV[i][j]);
              MyV[i][j] := TempDouble;
            end;
          dtdoWithinGroupMean:
            for i:=0 to MyNoOfGps-1 do
            begin
              MyMeanGpD[i] := GetPValueForStatTestUsesTempDouble(MyMeanGpD[i], MyMeanGpV[i]);
              MyMeanGpV[i] := TempDouble;
            end;
         dtdoOverallMean:
            begin
              MyMeanD :=  GetPValueForStatTestUsesTempDouble(MyMeanD, MyMeanV);
              MyMeanV := TempDouble;
            end;
      end;
    end;
  end;
end;

function TDistCommandThread.DoExecute: Boolean;
begin
  try
    Result := True;
    AllocateMatrices;
    AllocateDistComputer;
    ComputeDistances;
    UpdateMatricesForExactTest;
    {$IFDEF VISUAL_BUILD}
    MAI.AnalysisSummary.AddCalculatedValue('Dist Compute Start Time', DateTimeToStr(DistComputer.StartTime));
    MAI.AnalysisSummary.AddCalculatedValue('Dist Compute End Time', DateTimeToStr(DistComputer.StartTime));
    MAI.AnalysisSummary.AddCalculatedValue('Dist Compute Run Time', Format('%.3f', [SecondSpan(DistComputer.EndTime, DistComputer.StartTime)]));
    {$ELSE}
    D_MegaMain.AnalysisSummary.AddCalculatedValue('Dist Compute Start Time', DateTimeToStr(DistComputer.StartTime));
    D_MegaMain.AnalysisSummary.AddCalculatedValue('Dist Compute End Time', DateTimeToStr(DistComputer.StartTime));
    D_MegaMain.AnalysisSummary.AddCalculatedValue('Dist Compute Run Time', Format('%.3f', [SecondSpan(DistComputer.EndTime, DistComputer.StartTime)]));
    {$ENDIF}
  except
    on E:EDistComputationError do
    begin
      ARP.Hide;
      FInternalErrorId := E.ErrorId;
      FLog.Add(E.Message);
      Result := False;
    end;
    on E:EAbort do
    begin
      Result := False;
      FCancelled := True;
      FLog.Add('Distance calculation aborted');
      if E.Message <> EmptyStr then
        FLog.Add(E.Message);
    end;
    on E:Exception do
    begin
      ARP.Hide;
      Result := False;
      FLog.Add('Error in TDistCommandThread.DoExecute: ' + E.Message);
    end;
  end;
end;

procedure TDistCommandThread.Execute;
begin
  FIsSuccess := DoExecute;
  Terminate;
  Exit;
end;

procedure TDistCommandThread.Clear;
begin
  if Assigned(FLog) then
    FLog.Free;
  if not FIsOwnerOfResultsData then
    Exit;

  if not FIsWorkflowCalculation then
  begin
    SetLength(MyMeanGpD, 0);
    SetLength(MyMeanGpV, 0);
    case UsrOperation of
      dtdoPairwiseDist, dtdoDisparityIndexTest, dtdoCompositionDistance, dtdoDisparityIndex:
        begin
           if MyD <> nil then  FreeDistMatrix(myD, MAI.MyNoOfSeqs);
           if MyV <> nil then  FreeDistMatrix(myV, MAI.MyNoOfSeqs);
        end;
      dtdoBetweenGroupMean,
      dtdoNetGroupMean:
        begin
          if MyD <> nil then  FreeDistMatrix(myD, MAI.MyNoOfGps);
          if MyV <> nil then  FreeDistMatrix(myV, MAI.MyNoOfGps);
        end;
    end;
  end;

  if DistComputer <> nil then
  begin
    if      DistComputer is TNucDist       then FreeAndNil(TNucDist(DistComputer))
    else if DistComputer is TSynNonsynDist then FreeAndNil(TSynNonsynDist(DistComputer))
    else if DistComputer is TAminoDist     then FreeAndNil(TAminoDist(DistComputer));
  end;

  if Assigned(MAI) then
    FreeAndNil(MAI);
end;

procedure TDistCommandThread.DoDistComputerCheckCancel;
begin
  FCancelled := ARP.ProgressAndStatusCheckCancel(FProgress, FStatus, FStatusInfo);
end;

function TDistCommandThread.DistComputerCheckCancel(Progress: integer; Status: AnsiString): Boolean;
begin
  FProgress := Progress;
  if Trim(Status) <> EmptyStr then
    FStatusInfo := Status;
  Synchronize(DoDistComputerCheckCancel);
  Result := FCancelled;
end;

procedure TDistCommandThread.DoDistComputerUpdateRunStatusInfo;
begin
  ARP.UpdateRunStatusInfo(FStatus, FStatusInfo);
end;

procedure TDistCommandThread.DistComputerUpdateRunStatusInfo(aKey: AnsiString; aInfo: AnsiString);
begin
  FStatus := aKey;
  if Trim(aInfo) <> EmptyStr then
    FStatusInfo := aInfo;
  Synchronize(DoDistComputerUpdateRunStatusInfo);
end;

procedure TDistCommandThread.DoDistComputerCheckAbort;
begin
  {$IFDEF VISUAL_BUILD}
  FCancelled := ARP.StopBtn.Down;
  {$ENDIF}
end;

function TDistCommandThread.DistComputerCheckAbort: Boolean;
begin
  Synchronize(DoDistComputerCheckAbort);
  Result := FCancelled;
end;

constructor TDistCommandThread.Create(analysisInfo: TAnalysisInfo; aOperation: TDistTreeDlgOption);
begin
  inherited Create(True);
  FInternalErrorId := -1;
  FLog := TStringList.Create;
  FreeOnTerminate := True;
  Priority := tpTimeCritical;
  MAI := analysisInfo;
  ARP := MAI.ARP;
  UsrOperation := aOperation;
  FIsSuccess := False;
  FCancelled := False;
  FIsOwnerOfResultsData := True;
  FStatus := 'Status';
  FStatusInfo := 'Initializing...';
end;

destructor TDistCommandThread.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TDistCommandThread.GetResultsData: TDistCommandResultsData;
begin
  Result := TDistCommandResultsData.Create;
  Result.MyD := MyD;
  Result.MyV := MyV;
  Result.MyMeanGpD := MyMeanGpD;
  Result.MyMeanGpV := MyMeanGpV;
  Result.MyMeanD := MyMeanD;
  Result.MyMeanV := MyMeanV;
  MyD := nil;
  MyV := nil;
  MyMeanGpD := nil;
  MyMeanGpV := nil;
  MAI := nil;
  ARP := nil;
  FIsOwnerOfResultsData := False;
end;

end.

