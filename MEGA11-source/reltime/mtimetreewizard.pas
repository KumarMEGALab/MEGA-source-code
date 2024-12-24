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

unit MTimeTreeWizard;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFNDEF VISUAL_BUILD}MegaUtils, ProcessInputData,{$ENDIF}
  LCLIntf, LCLType,
  MAnalysisInfo, MProcessPack, MDistPack, Classes,
  Dialogs, SysUtils, MTreeList, MTreeData, MCalibrationData, MCalibrationDlg, Controls,
  MD_InputSeqData, ExtCtrls, MegaConsts, Forms, ITimeTree;

type
  TSetupCalibrationsCallback = procedure of object;
  TOutgroupsAssignedCallback = procedure(GroupNames: TStringList) of object;
  TWizardFormUpdate = procedure(Step: TTtProcessStep) of object;
  TTimeTreeStartFrom = (ttsMegaMain, ttsTreeExplorer);

  { TTimeTreeWizard }

  TTimeTreeWizard = class(TInterfacedObject, ITimeTreeWizard)
    private
      FUsrOperation: TDistTreeDlgOption;
      FTimer: TTimer;
      FTreeViewer: TObject;
      FRootedNewickString: String;
      FGroupNamesAreSet: Boolean;
      FCalibrationType: TCalibrationTarget;
      FWizardMode: TTimeTreeWizardMode;
      FAnalysisType: TTimeTreeAnalysis;
      procedure OnTimer(Sender: TObject);
      procedure GetOutgroupDone;
      procedure UpdateOutgroupInfo(OutgroupTaxa: TStringList);
      procedure RootTreeOnOutgroup;
      procedure InferOutgroup(AData: TTreeData);
      procedure OnLoadTreeFinished;
      function LaunchMLAnalysis: Boolean;
      function LaunchLSAnalysis: Boolean;
      function LaunchBlensAnalysis: Boolean;
      function LaunchCorrtestMLAnalysis: Boolean;
      function LaunchCorrtestBLensAnalysis: Boolean;
      function LaunchEpAnalysis: Boolean;
      procedure InitProgress;
      procedure ValidateSequenceData;
      procedure FinalizeCalibrations;
      procedure SetWizardMode(const Value: TTimeTreeWizardMode);
      procedure SetUsrOperation(const Value: TDistTreeDlgOption);
      procedure SetCalibrationType(const Value: TCalibrationTarget);
      procedure SetAnalysisType(const Value: TTimeTreeAnalysis);
      procedure SeqDataInfoDoneCallback(Sender: TObject);
      procedure SeqDataInfoCancelCallback(Sender: TObject);
      function TryGetRTDTCalibrationsFromOtuInfos(var CalibrationStrings: TStringList): Boolean;
      function ValidateOutgroup(var aMsg: String): Boolean;
    public
      SetupCalibrationsCallback: TSetupCalibrationsCallback; // when launched from TTreeViewForm it is easier to do it there
      OutgroupsAssignedCallback: TOutgroupsAssignedCallback; // updates groups in TTreeViewForm
      ProcessStep: TTtProcessStep;
      Calibrations: TCalibrations;
      LaunchedFrom: TTimeTreeStartFrom;
      AnalysisInfo: TAnalysisInfo;
      TempTreeList: TTreeList;
      ProcessPack: TProcessPack;
      WizardFormUpdate: TWizardFormUpdate;
      function OutgroupIsDefined: Boolean;
      constructor Create;
      destructor Destroy; override;
      procedure Reset(step: TTtProcessStep);
      procedure InitAnalysisInfo;
      function TryParseTipDates(var calibrationStrings: TStringList): Boolean;
      function LoadTree: Boolean;
      function LoadSeqData: Boolean;
      function SetOutgroupViaTaxaGpsDlg: Boolean;
      function SetOutgroupViaSelectOutgroupDlg: Boolean;
      function SetOutgroupViaTE: Boolean;
      function GetCalibrations: Boolean;
      function GetAnalysisPreferences: Boolean;
      function LaunchAnalysis: Boolean;
      procedure AssignCalibrationTimes(ACalibs: TCalibrations);
      procedure CancelCalibrationChanges;
      property UsrOperation: TDistTreeDlgOption read FUsrOperation write SetUsrOperation;
      property WizardMode: TTimeTreeWizardMode read FWizardMode write SetWizardMode;
      property AnalysisType: TTimeTreeAnalysis read FAnalysisType write SetAnalysisType;
      property CalibrationType: TCalibrationTarget read FCalibrationType write SetCalibrationType;
  end;

  function InferCalibrationsType(var aType: TCalibrationTarget; const UserOp: TDistTreeDlgOption): Boolean;
  function PromptForCalibrationsType(var aType: TCalibrationTarget): Boolean;

implementation

uses
  {$IFDEF VISUAL_BUILD}mega_main, {$ENDIF}
  MTreePack, MFileUtils, MTaxaGpsDlg, htmloptionsdlg, mtipdatesfinder,
  MAnalysisPrefDlg, MRuntimeProgressDlg, ProcessMLTreeCmds, MPleaseWait,
  MegaErrUtils, ErrorMessages_HC, MTreeViewForm, Graphics, ProcessCodonOmegaCmds,
  MTreeDataAdapter, MOtuInfo, mtreeloaders, ProcessTreeCmds, syncobjs, mselectoutgrouptaxadlg;

function InferCalibrationsType( var aType: TCalibrationTarget; const UserOp: TDistTreeDlgOption): Boolean;
begin
  Result := True;
  case UserOp of
    dtdoRelTimeML, dtdoRelTimeBLens, dtdoRelTimeLS: aType := ctInternalNode;
    dtdoRtdtML, dtdoRtdtLS, dtdoRtdtBlens: aType := ctLeafNode;
    else
      raise Exception.Create('invalid user operation for Reltime analysis');
  end;
end;

function PromptForCalibrationsType( var aType: TCalibrationTarget): Boolean;
const
  MR_SAMPLE_CALIBS = 1000;
  MR_INTERNAL_NODE_CALIBS = 1001;
  MR_CANCEL_CALIBS = 1002;
var
  Response: Integer;
begin
  Result := True;
  Response := QuestionDlg('Calibrations Type', 'What kind of calibrations will be used:', mtCustom, [MR_INTERNAL_NODE_CALIBS, 'Internal Node Constraints', MR_SAMPLE_CALIBS, 'Sample Times', MR_CANCEL_CALIBS, 'Cancel', 'IsCancel'], 0);
  if Response = MR_INTERNAL_NODE_CALIBS then
    aType := ctInternalNode
  else if Response = MR_SAMPLE_CALIBS then
    aType := ctLeafNode
  else
    Result := False;
end;

{ TTimeTreeWizard }

procedure TTimeTreeWizard.AssignCalibrationTimes(ACalibs: TCalibrations);
begin
  Calibrations.Clear;
  Calibrations.Assign(ACalibs);
end;

procedure TTimeTreeWizard.CancelCalibrationChanges;
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTimer) then
    FreeAndNil(FTimer);
  try
    try
      AnchoredTimesCS.Acquire;
      {$IFDEF VISUAL_BUILD}
      if TreeViewerLives then
      begin
        if Assigned(FTreeViewer) then
        begin
          TTreeViewForm(FTreeViewer).FormIsClosing := True;
          FreeAndNil(TTreeViewForm(FTreeViewer));
        end;
        TreeViewerLives := False;
      end
      else
        FTreeViewer := nil; // so we don't double free it in our destructor
      {$ENDIF}
    except
      // do nothing
    end;
  finally
    AnchoredTimesCS.Release;
  end;
  {$ENDIF}
end;

constructor TTimeTreeWizard.Create;
begin
  WizardMode := ttwmSeqData;
  FCalibrationType := ctInternalNode;
  FUsrOperation := dtdoReltimeML;
  TempTreeList := nil;
  FRootedNewickString := EmptyStr;
  FGroupNamesAreSet := False;
  AnalysisInfo := nil;
  ProcessPack := nil;
  {$IFDEF VISUAL_BUILD}
  FTreeViewer := nil;
  {$ENDIF}
  Calibrations := TCalibrations.Create;
  FTimer := nil;
  {$IFDEF VISUAL_BUILD}
  if Assigned(AnchoredTimesCS) then
    FreeAndNil(AnchoredTimesCS);
  AnchoredTimesCS := TCriticalSection.Create;
  {$ENDIF}
end;

destructor TTimeTreeWizard.Destroy;
begin
  if Assigned(FTimer) then
    FTimer.Enabled := False;
  {$IFDEF VISUAL_BUILD}
  FreeAndNil(AnchoredTimesCS);
  if Assigned(FTreeViewer) then
  begin
    TTreeViewForm(FTreeViewer).Free;
    FTreeViewer := nil;
  end;
  {$ENDIF}
  if Assigned(FTimer) then
    FTimer.Free;
  if Assigned(TempTreeList) then
  begin
    if Assigned(AnalysisInfo) then
      AnalysisInfo.MyOriTreeList := nil;
    TempTreeList.Free;
    TempTreeList := nil;
  end;
  if Assigned(Calibrations) then
    Calibrations.Free;
  inherited;
end;

procedure TTimeTreeWizard.FinalizeCalibrations;
begin
  if Calibrations.Count > 0 then
  begin
    AnalysisInfo.MyOriTreeList.ResolveCalibrationTimeNames(Calibrations);
    AnalysisInfo.MyOriTreeList[0].resolveRelTimes(Calibrations);
    Calibrations.SetInfo(AnalysisInfo);
  end;
end;

procedure TTimeTreeWizard.InferOutgroup(AData: TTreeData);
var
  Adapter: TSimpleTreeDataAdapter = nil;
begin
  Assert(Assigned(AData));
  Assert(AnalysisInfo.MyOriTreeList.IsRooted); { Adapter assumes that it is true}

  try
    try
      Adapter := TSimpleTreeDataAdapter.Create;
      Adapter.GuessOutgroup(AData);
    except
      on E:Exception do
        ShowMessage('Oh no! An error occurred when inferring the outgroup in the tree: ' + E.Message);
    end;
  finally
    if Assigned(Adapter) then
      Adapter.Free;
  end;
end;

procedure TTimeTreeWizard.InitAnalysisInfo;
var
  Progress: TRuntimeProgress;
  i: Integer;
begin
  AnalysisInfo := TAnalysisInfo.Create;
  AnalysisInfo.DataFilename := MegaForm.DataFileName;
  Progress := TRuntimeProgress.Create(Application);
  Progress.DataFileName := MegaForm.DataFileName;
  Progress.DataTitle :=  MegaForm.DataTitle;
  AnalysisInfo.ARP := Progress;
  AnalysisInfo.MyProcessPack := ProcessPack;
  Progress.FMAI := AnalysisInfo;
  AnalysisInfo.MyTreePack := TTreePack.Create;
  AnalysisInfo.InitialUsrOperation := FUsrOperation;
  case FUsrOperation of
    dtdoRelTimeML, dtdoRtdtML: AnalysisInfo.SetupUsedOtuInfos(dtdoReltimeML);
    dtdoRelTimeBLens, dtdoRtdtBlens: ;
    dtdoRelTimeLS, dtdoRtdtLS: AnalysisInfo.SetupUsedOtuInfos(dtdoRelTimeLS);
    dtdoCorrtestML: AnalysisInfo.SetupUsedOtuInfos(dtdoCorrtestML);
    dtdoCorrtestBlens: ;
    dtdoEpML: AnalysisInfo.SetupUsedOtuInfos(dtdoEpML);
  end;

  AnalysisInfo.MyOriTreeList := TTreeList.Create;
  if not Assigned(AnalysisInfo.MyOtuNames) then
  begin
    AnalysisInfo.MyOtuNames := TStringList.Create;
    if (FUsrOperation = dtdoRelTimeML) or (FUsrOperation = dtdoRelTimeLS) or (FUsrOperation = dtdoRtdtML) or (FUsrOperation = dtdoRtdtLS) then
      for i := 0 to D_InputSeqData.OtuInfos.NoOfOtus - 1 do
        AnalysisInfo.MyOtuNames.Add(D_InputSeqData.OtuInfos[i].Name);
  end;
end;

function TTimeTreeWizard.TryParseTipDates(var calibrationStrings: TStringList): Boolean;
var
  taxaLabels: TStringList = nil;
  finder: TTipDatesFinder = nil;
  temp: TStringList = nil;
  pw: TPleaseWait = nil;
begin
  Result := False;
  try
    try
      pw := TPleaseWait.Create(nil);
      pw.Action := 'Searching for tip dates...';
      pw.Show;
      taxaLabels := AnalysisInfo.GetOtuNamesList;
      finder := TTipDatesFinder.Create;
      //finder.ParseRule := tdprLast4;
      temp := finder.FindTipDates(taxaLabels, Result);
      if Result then
        calibrationStrings.AddStrings(temp);
    except
      on E:Exception do
      begin
        Result := False;
      end;
    end;
  finally
    if Assigned(taxaLabels) then
      taxaLabels.Free;
    if Assigned(finder) then
      finder.Free;
    if Assigned(temp) then
      temp.Free;
    if Assigned(pw) then
      pw.Free;
  end;
end;

procedure TTimeTreeWizard.InitProgress;
begin
  Application.ProcessMessages;
  AnalysisInfo.ARP.WriteAnalysisOptionsToStdOut;
  AnalysisInfo.ARP.AddRunStatusInfo('Status', 'Preparing data');
  AnalysisInfo.ARP.Show;
end;

function TTimeTreeWizard.LaunchAnalysis: Boolean;
begin
  case FUsrOperation of
    dtdoRelTimeML, dtdoRtdtML: Result := LaunchMLAnalysis;
    dtdoRelTimeLS, dtdoRtdtLS: Result := LaunchLSAnalysis;
    dtdoRelTimeBLens, dtdoRtdtBlens: Result := LaunchBlensAnalysis;
    dtdoCorrtestML: Result := LaunchCorrtestMLAnalysis;
    dtdoCorrTestBlens: Result := LaunchCorrtestBlensAnalysis;
    dtdoEpML: Result := LaunchEpAnalysis;
    else
      raise Exception.Create('Invalid reltime method called');
  end;
end;

function TTimeTreeWizard.LaunchBlensAnalysis: Boolean;
var
  HasDivTimes, HasSampleTimes: Boolean;
begin
  HasDivTimes := False;
  HasSampleTimes := False;
  Result := False;
  try
    AnalysisInfo.MyUsrOperation := FUsrOperation;
    AnalysisInfo.ARP.AddRunStatusInfo('Status', 'Preparing data');
    AnalysisInfo.ARP.Show;
    AnalysisInfo.ARP.AddAnalysisOptions('No. of Taxa', IntToStr(AnalysisInfo.MyOriTreeList.NoOfOtus));
    AnalysisInfo.MyOriTreeList.UpdateNodeLabels(Calibrations);
    AnalysisInfo.CalibrationTimes := Calibrations;

    if Calibrations.Count > 0 then
    begin
      FinalizeCalibrations;
      Calibrations.GetCalibrationTypes(HasDivTimes, HasSampleTimes);
      if HasSampleTimes and (not HasDivTimes) then
      begin
        ComputeRelTimeBLens(AnalysisInfo, Calibrations);
      end
      else if (HasDivTimes and (not HasSampleTimes)) then
      begin
        ComputeRelTimeBLens(AnalysisInfo, Calibrations);
      end
      else
        raise Exception.Create('mixing sample times with divergence times is not supported');
    end
    else
    begin
      ComputeRelTimeBLens(AnalysisInfo);
    end;
    Calibrations := nil;
    AnalysisInfo := nil;
    Result := True;
  except
    on E:Exception do
    begin
      if Assigned(AnalysisInfo) then
      begin
        if Assigned(AnalysisInfo.ARP) then
          AnalysisInfo.ARP.Free;
        FreeAndNil(AnalysisInfo);
      end;

      ShowMessage('Oh no! An error occurred when launching the Reltime analysis: ' + E.Message);
    end;
  end;
end;

function TTimeTreeWizard.LaunchCorrtestBLensAnalysis: Boolean;
begin
  Result := False;
  try
    AnalysisInfo.MyUsrOperation := FUsrOperation;
    AnalysisInfo.ARP.AddRunStatusInfo('Status', 'Preparing data');
    AnalysisInfo.ARP.Show;
    AnalysisInfo.ARP.AddAnalysisOptions('No. of Taxa', IntToStr(AnalysisInfo.MyOriTreeList.NoOfOtus));
    AnalysisInfo.MyOriTreeList.UpdateNodeLabels(Calibrations);
    RunCorrTest(AnalysisInfo);
    AnalysisInfo := nil;
    Result := True;
  except
    on E:Exception do
    begin
      if Assigned(AnalysisInfo) then
      begin
        if Assigned(AnalysisInfo.ARP) then
          AnalysisInfo.ARP.Free;
        FreeAndNil(AnalysisInfo);
      end;
      ShowMessage('Oh no! An error occurred when launching the rate correlation test: ' + E.Message);
    end;
  end;
end;

function TTimeTreeWizard.LaunchEpAnalysis: Boolean;
begin
  Result := False;
  try
    AnalysisInfo.ARP.AddRunStatusInfo('Status', 'Preparing data');
    AnalysisInfo.ARP.Show;
    AnalysisInfo.ARP.Refresh;
    AnalysisInfo.MySeqStrings := TStringList.Create;
    AnalysisInfo.MyTreePack.TreeFile := AnalysisInfo.MyUserTreeFile;
    if FRootedNewickString = EmptyStr then
      RootTreeOnOutgroup;
    { TODO 1 -oglen -cpartitionedML : switch to PrepareDataForPartitionedMLAnalysis below as soon as we are ready }
  //  AnalysisInfo.MySeqPartitions := TListOfSeqPartitions.Create;
  //    D_InputSeqData.PrepareDataForPartitionedMLAnalysis(MAI.MySubsetOpt, MAI.MySeqPartitions, MAI.MyUsedOtuInfos,
  //                                            MAI.MyNoOfSeqs, MAI.MyNoOfSites,  MAI.MyLabelsUsed, MAI.RecodeScheme,MAI.SiteCoverage);

    D_InputSeqData.PrepareDataForMLAnalysis(AnalysisInfo);
    if CheckAbortIfMemoryUsageHigh(AnalysisInfo) then
    begin
      WizardFormUpdate(ttpsCancelAnalysis);
      AnalysisInfo.ARP.Hide;
      Abort;
    end
    else if CheckAbortForIdenticalSeqsML(AnalysisInfo) then
    begin
      WizardFormUpdate(ttpsCancelAnalysis);
      AnalysisInfo.ARP.Hide;
      Abort;
    end
    else
    begin
      AnalysisInfo.ARP.AddAnalysisOptions('No. of Seqs', IntToStr(AnalysisInfo.MyNoOfSeqs));
      AnalysisInfo.ARP.AddAnalysisOptions('No. of Sites', IntToStr(AnalysisInfo.MyNoOfSites));
      AnalysisInfo.ARP.AddRunStatusInfo('Status', 'Calculation EP');
      Application.ProcessMessages;
      {$IFDEF VISUAL_BUILD}
        AnalysisInfo.ARP.ProcessMessages;
      {$ENDIF}
      ComputeEpML(AnalysisInfo);
      AnalysisInfo := nil;
    end;
  except
    on E: EAbort do
    begin
      if Assigned(AnalysisInfo) then
      begin
        if Assigned(AnalysisInfo.ARP) then
          AnalysisInfo.ARP.Free;
        AnalysisInfo.Free;
      end;
    end;
    on E: Exception do
    begin
      if Assigned(AnalysisInfo) then
      begin
        if Assigned(AnalysisInfo.ARP) then
          AnalysisInfo.ARP.Free;
        AnalysisInfo.Free;
      end;
      ShowMessage('Oh no! An error has occurred while setting up the EP calculation: ' + E.Message);
    end;
  end;
end;

function TTimeTreeWizard.LaunchCorrtestMLAnalysis: Boolean;
begin
  Result := False;
  try
    AnalysisInfo.ARP.AddRunStatusInfo('Status', 'Preparing data');
    AnalysisInfo.ARP.Show;
    AnalysisInfo.ARP.Refresh;
    AnalysisInfo.MySeqStrings := TStringList.Create;
    AnalysisInfo.MyTreePack.TreeFile := AnalysisInfo.MyUserTreeFile;
    if FRootedNewickString = EmptyStr then
      RootTreeOnOutgroup;
    { TODO 1 -oglen -cpartitionedML : switch to PrepareDataForPartitionedMLAnalysis below as soon as we are ready }
  //  AnalysisInfo.MySeqPartitions := TListOfSeqPartitions.Create;
  //    D_InputSeqData.PrepareDataForPartitionedMLAnalysis(MAI.MySubsetOpt, MAI.MySeqPartitions, MAI.MyUsedOtuInfos,
  //                                            MAI.MyNoOfSeqs, MAI.MyNoOfSites,  MAI.MyLabelsUsed, MAI.RecodeScheme,MAI.SiteCoverage);

    D_InputSeqData.PrepareDataForMLAnalysis(AnalysisInfo);
    if CheckAbortIfMemoryUsageHigh(AnalysisInfo) then
    begin
      WizardFormUpdate(ttpsCancelAnalysis);
      AnalysisInfo.ARP.Hide;
      Abort;
    end
    else if CheckAbortForIdenticalSeqsML(AnalysisInfo) then
    begin
      WizardFormUpdate(ttpsCancelAnalysis);
      AnalysisInfo.ARP.Hide;
      Abort;
    end
    else
    begin
      AnalysisInfo.ARP.AddAnalysisOptions('No. of Seqs', IntToStr(AnalysisInfo.MyNoOfSeqs));
      AnalysisInfo.ARP.AddAnalysisOptions('No. of Sites', IntToStr(AnalysisInfo.MyNoOfSites));
      AnalysisInfo.ARP.AddRunStatusInfo('Status', 'Creating Substitution Model');
      Application.ProcessMessages;
      {$IFDEF VISUAL_BUILD}
        AnalysisInfo.ARP.ProcessMessages;
      {$ENDIF}
      RunCorrtestML(AnalysisInfo);
      AnalysisInfo := nil;
    end;
  except
    on E: EAbort do
    begin
      if Assigned(AnalysisInfo) then
      begin
        if Assigned(AnalysisInfo.ARP) then
          AnalysisInfo.ARP.Free;
        AnalysisInfo.Free;
      end;
    end;
    on E: Exception do
    begin
      if Assigned(AnalysisInfo) then
      begin
        if Assigned(AnalysisInfo.ARP) then
          AnalysisInfo.ARP.Free;
        AnalysisInfo.Free;
      end;
      ShowMessage('Oh no! An error has occurred while setting up the rate correlation test: ' + E.Message);
    end;
  end;
end;

function TTimeTreeWizard.LaunchLSAnalysis: Boolean;
var
  HasDivTimes, HasSampleTimes: Boolean;
begin
  Result := False;
  HasDivTimes := False;
  HasSampleTimes := False;

  try
    AnalysisInfo.MyUsrOperation := FUsrOperation;
    AnalysisInfo.ARP.AddRunStatusInfo('Status', 'Preparing data');
    AnalysisInfo.ARP.Show;
    AnalysisInfo.MyMappedData := TList.Create;
    D_InputSeqData.PrepareDataForDistAnalysis(AnalysisInfo.MySubsetOpt,
                                              AnalysisInfo.MyMappedData,
                                              AnalysisInfo.MyUsedOtuInfos,
                                              AnalysisInfo.MyNoOfSeqs,
                                              AnalysisInfo.MyNoOfSites,
                                              AnalysisInfo.MyLabelsUsed,
                                              AnalysisInfo.RecodeScheme,
                                              AnalysisInfo.SiteCoverage);
    if AnalysisInfo.MyNoOfSites < 1 then
      RaiseErrorMessage(HC_No_Common_Sites, 'No common sites found for computing distances. ' + NoCommonSitesStr);
    AnalysisInfo.ARP.UpdateRunStatusInfo('Status', 'Checking for duplicate sequences');
    AnalysisInfo.ARP.Show;
    Application.ProcessMessages;
    if CheckAbortForIdenticalSeqsLS(AnalysisInfo) then
    begin
      WizardFormUpdate(ttpsCancelAnalysis);
      AnalysisInfo.ARP.Hide;
      Abort;
    end;
    AnalysisInfo.ARP.AddAnalysisOptions('No. of Seqs', IntToStr(AnalysisInfo.MyNoOfSeqs));
    AnalysisInfo.ARP.AddAnalysisOptions('No. of Sites', IntToStr(AnalysisInfo.MyNoOfSites));

    // late validation
    if AnalysisInfo.MyTreePack.DoesContain(ttBootstrap) then
    begin
      if AnalysisInfo.MyNoOfSeqs < 4 then
        RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least four taxa are needed for bootstrapping.');
    end;
    AnalysisInfo.MyOriTreeList.UpdateNodeLabels(Calibrations);
    AnalysisInfo.CalibrationTimes := Calibrations;
    {$IFDEF VISUAL_BUILD}
      AnalysisInfo.ARP.ProcessMessages;
    {$ENDIF}

    if Calibrations.Count > 0 then
    begin
      FinalizeCalibrations;
      Calibrations.GetCalibrationTypes(HasDivTimes, HasSampleTimes);
      if HasSampleTimes and (not HasDivTimes) then
      begin
        DoReltimeLeastSquares(AnalysisInfo);
      end
      else if (HasDivTimes and (not HasSampleTimes)) then
      begin
        DoReltimeLeastSquares(AnalysisInfo);
      end
      else
        raise Exception.Create('mixing sample times with divergence times is not supported');
    end
    else
    begin
      DoReltimeLeastSquares(AnalysisInfo);
    end;
    Calibrations := nil;
    AnalysisInfo := nil;
    Result := True;
  except
    on E:EAbort do
    begin
      if Assigned(AnalysisInfo) then
      begin
        if Assigned(AnalysisInfo.ARP) then
        begin
          AnalysisInfo.ARP.Free;
          AnalysisInfo.ARP := nil;
          FreeAndNil(AnalysisInfo);
        end;
      end;
    end;
    on E:Exception do
    begin
      if Assigned(AnalysisInfo) then
      begin
        if Assigned(AnalysisInfo.ARP) then
        begin
          AnalysisInfo.ARP.Free;
          AnalysisInfo.ARP := nil;
          FreeAndNil(AnalysisInfo);
        end;
      end;
      ShowMessage('Oh no! An error occurred when launching the Reltime analysis: ' + E.Message);
    end;
  end;
end;

function TTimeTreeWizard.LaunchMLAnalysis: Boolean;
var
  MinTimes, MaxTimes: TDivTimesArray;
  SamplingTimes: TDivTimesArray;
  HasDivTimes: Boolean = False;
  HasSampleTimes: Boolean = False;
begin
  SetLength(MinTimes, 0);
  SetLength(MaxTimes, 0);
  SetLength(SamplingTimes, 0);
  Result := False;
  try
    AnalysisInfo.ARP.AddRunStatusInfo('Status', 'Preparing data');
    AnalysisInfo.ARP.Show;
    AnalysisInfo.ARP.Refresh;
    AnalysisInfo.MySeqStrings := TStringList.Create;
    AnalysisInfo.MyTreePack.TreeFile := AnalysisInfo.MyUserTreeFile;
    if FRootedNewickString = EmptyStr then
      RootTreeOnOutgroup;
    { TODO 1 -oglen -cpartitionedML : switch to PrepareDataForPartitionedMLAnalysis below as soon as we are ready }
  //  AnalysisInfo.MySeqPartitions := TListOfSeqPartitions.Create;
  //    D_InputSeqData.PrepareDataForPartitionedMLAnalysis(MAI.MySubsetOpt, MAI.MySeqPartitions, MAI.MyUsedOtuInfos,
  //                                            MAI.MyNoOfSeqs, MAI.MyNoOfSites,  MAI.MyLabelsUsed, MAI.RecodeScheme,MAI.SiteCoverage);

    D_InputSeqData.PrepareDataForMLAnalysis(AnalysisInfo);
    if CheckAbortIfMemoryUsageHigh(AnalysisInfo) then
    begin
      WizardFormUpdate(ttpsCancelAnalysis);
      AnalysisInfo.ARP.Hide;
      Abort;
    end
    else if CheckAbortForIdenticalSeqsML(AnalysisInfo) then
    begin
      WizardFormUpdate(ttpsCancelAnalysis);
      AnalysisInfo.ARP.Hide;
      Abort;
    end
    else
    begin
      AnalysisInfo.ARP.AddAnalysisOptions('No. of Seqs', IntToStr(AnalysisInfo.MyNoOfSeqs));
      AnalysisInfo.ARP.AddAnalysisOptions('No. of Sites', IntToStr(AnalysisInfo.MyNoOfSites));
      AnalysisInfo.ARP.AddRunStatusInfo('Status', 'Creating Substitution Model');
      Application.ProcessMessages;
      AnalysisInfo.MyOriTreeList.UpdateNodeLabels(Calibrations);
      AnalysisInfo.CalibrationTimes := Calibrations;
      {$IFDEF VISUAL_BUILD}
      AnalysisInfo.ARP.ProcessMessages;
      {$ENDIF}

      if Calibrations.Count > 0 then
      begin
        FinalizeCalibrations;
        Calibrations.GetCalibrationTypes(HasDivTimes, HasSampleTimes);
        if HasSampleTimes and (not HasDivTimes) then
        begin
          Calibrations.PrepareSamplingTimeArray(SamplingTimes, AnalysisInfo.MyOtuNames);
          EstimateSampledDivTimes(AnalysisInfo, SamplingTimes);
        end
        else if (HasDivTimes and (not HasSampleTimes)) then
        begin
          Calibrations.PrepareDivTimeArrays(MinTimes, MaxTimes, AnalysisInfo.NoOfSeqs, False);
          EstimateAnchoredDivTimes(AnalysisInfo, MinTimes, MaxTimes);
        end
        else
          raise Exception.Create('mixing sample times with divergence times is not supported');
      end
      else
      begin
        EstimateRelativeDivTimes(AnalysisInfo);
      end;
      Calibrations := nil;
      AnalysisInfo := nil;
    end;
  except
    on E: EAbort do
    begin
      if Assigned(AnalysisInfo) then
      begin
        if Assigned(AnalysisInfo.ARP) then
          AnalysisInfo.ARP.Free;
        AnalysisInfo.Free;
      end;
    end;
    on E: Exception do
    begin
      if Assigned(AnalysisInfo) then
      begin
        if Assigned(AnalysisInfo.ARP) then
          AnalysisInfo.ARP.Free;
        AnalysisInfo.Free;
      end;
      ShowMessage('Oh no! An error has occurred while setting up the timetree computation: ' + E.Message);
    end;
  end;
end;

function TTimeTreeWizard.LoadSeqData: Boolean;
begin
  Result := False;
  if not MegaForm.AskUserToActivateDataFile then
  begin
    WizardFormUpdate(ttpsLoadSeqData);
    HtmlOptionsDialog.AddObserver(SeqDataInfoCancelCallback, SeqDataInfoDoneCallback);
    Exit;
  end;

  if not MegaForm.HasSequenceData then
  begin
    ShowMessage('Maximum Likelihood Analysis requires sequence data.');
    WizardFormUpdate(ttpsLoadSeqData);
    Exit;
  end;
  InitAnalysisInfo;
  Result := True;
  WizardFormUpdate(ttpsLoadTree);
end;

function TTimeTreeWizard.LoadTree: Boolean;
var
  TreeLoader: ITreeLoader;
begin
  Result := False;

  try
    if not Assigned(AnalysisInfo) then
      InitAnalysisInfo;
    TreeLoader := TGuiTreeLoader.Create;
    if not TreeLoader.LoadTree(AnalysisInfo) then
    begin
      if Trim(TreeLoader.GetMsg) <> EmptyStr then
        ShowMessage(TreeLoader.GetMsg)
      else
        ShowMessage('An unknown error occurred when attempting to load the tree file. Please validate all input files');
      WizardFormUpdate(ttpsLoadTree);
    end
    else
    begin
      if WizardMode = ttwmBlens then
        if not AnalysisInfo.MyOriTreeList.isBLen then
          raise Exception.Create('The Reltime analysis requires a tree with branch lengths but the loaded tree has none');
      LaunchedFrom := ttsMegaMain;
      FUsrOperation := AnalysisInfo.InitialUsrOperation;
      Result := True;
      SwitchDirectory(AnalysisInfo.MyUserTreeFile);
      OnLoadTreeFinished;
    end;
  except
    on E: Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
      {$ELSE}
      Error_NV(E.Message);
      {$ENDIF}
      WizardFormUpdate(ttpsLoadTree);
    end;
  end;
end;

procedure TTimeTreeWizard.OnLoadTreeFinished;
begin
  GetOutgroupDone; { because if outgroup is defined, we will skip the dooutgroup step and go straight to do calibrations}
  if (WizardMode = ttwmSeqData) and D_InputSeqData.OtuInfos.OutgroupIsDefinedAndIsActive then
  begin
    AnalysisInfo.SetupOutgroupMembers;
    if AnalysisType = ttaReltime then
      WizardFormUpdate(ttpsDoCalibrations)
    else
      WizardFormUpdate(ttpsDoSettings);
  end
  else
  begin
    if WizardMode = ttwmBlens then
      AnalysisInfo.SetupOtuNamesFromTreeList;
    if not AnalysisInfo.MyOriTreeList.IsRooted then
      WizardFormUpdate(ttpsDoOutgroup)
    else
    begin
      InferOutgroup(AnalysisInfo.MyOriTreeList[0]);
      if WizardMode = ttwmSeqData then
        AnalysisInfo.TransferTreeOutgroupInfoToOtuInfos(0);
      WizardFormUpdate(ttpsDoOutgroup);
    end;
  end;
end;

procedure TTimeTreeWizard.OnTimer(Sender: TObject);
begin
  {$IFDEF VISUAL_BUILD}
  try
    try
      AnchoredTimesCS.Acquire;
      if CalibrationsAreSet then
      begin
        FTimer.Enabled := False;
        if Assigned(FTreeViewer) then
        begin
          TTreeViewForm(FTreeViewer).FormIsClosing := True;
          TTreeViewForm(FTreeViewer).Free;
          FTreeViewer := nil;
        end;
        if LaunchedFrom = ttsMegaMain then
          WizardFormUpdate(ttpsDoSettings)
        else
          WizardFormUpdate(ttpsLaunchAnalysis);
      end
      else if CalibrationsCancelled then
      begin
        FTimer.Enabled := False;
        if Assigned(FTreeViewer) and TreeViewerLives then
        begin
          TTreeViewForm(FTreeViewer).FormIsClosing := True;
          TTreeViewForm(FTreeViewer).Free;
        end;
        FTreeViewer := nil;
        WizardFormUpdate(ttpsDoCalibrations);
      end;
    except
      // do nothing, we only reach this if the user closes MEGA without completing the wizard
    end;
  finally
    AnchoredTimesCS.Release;
  end;
  {$ENDIF}
end;

procedure TTimeTreeWizard.Reset(step: TTtProcessStep);
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(TempTreeList) then
    FreeAndNil(TempTreeList);
  FRootedNewickString := EmptyStr;
  FGroupNamesAreSet := False;
  if Assigned(AnalysisInfo) then
    FreeAndNil(AnalysisInfo);
  if step = ttpsLoadTree then
    InitAnalysisInfo;
  FTreeViewer := nil;
  Calibrations.Clear;
  FTimer := nil;
  {$ENDIF}
end;

procedure TTimeTreeWizard.RootTreeOnOutgroup;
var
  Rooter: TFpNodeTreeDataAdapter;
  rootedTree: TTreeData;
begin
  Rooter := nil;
  try
    Rooter := TFpNodeTreeDataAdapter.Create;
    Rooter.SetTreeData(AnalysisInfo.MyOriTreeList[0]);
    Rooter.RootOnOutgroup;
    rootedTree := AnalysisInfo.MyOriTreeList[0];
    Rooter.GetTreeData(rootedTree);
    FRootedNewickString := AnalysisInfo.MyOriTreeList.OutputNewickTree(0, True, False, 0.0);
  finally
    if Assigned(Rooter) then
      Rooter.Free;
  end;
end;

function TTimeTreeWizard.GetAnalysisPreferences: Boolean;
var
  MaxRateRatio: Extended;
  mrrStr: String;
begin
  Result := False;
  if (LaunchedFrom = ttsTreeExplorer) or (WizardMode = ttwmBlens) then
  begin
    mrrStr := Format('%.0f', [DEFAULT_MAX_RATE_RATIO]);
    if not InputQuery('Maximum Relative Rate Ratio', 'Please provide a number between 5 and 100', mrrStr) then
    begin
      WizardFormUpdate(ttpsDoSettings);
      exit;
    end;
    while (not TryStrToFloat(mrrStr, MaxRateRatio)) or (MaxRateRatio < 5) or (MaxRateRatio > 100) do
      if not InputQuery('Maximum Relative Rate Ratio', 'Please provide a number between 5 and 100', mrrStr) then
      begin
        WizardFormUpdate(ttpsDoSettings);
        exit;
      end;
    Result := True;
    AnalysisInfo.MaxRateRatio := MaxRateRatio;
    WizardFormUpdate(ttpsLaunchAnalysis)
  end
  else
  begin
    AnalysisPrefDlg.UserTreeFile := AnalysisInfo.MyUserTreeFile;
    if AnalysisInfo.GetAnalysisOptions(FUsrOperation) then
    begin
      WizardFormUpdate(ttpsLaunchAnalysis);
      Result := True;
    end
    else
    begin
      WizardFormUpdate(ttpsDoSettings);
      Result := True;
    end;
  end;
end;

function TTimeTreeWizard.GetCalibrations: Boolean;
var
  TempTreeList: TTreeList = nil;
  ATreeViewer: TTreeViewForm = nil;
  calibrationStrings: TStringList = nil;
begin
  Result := False;
  if LaunchedFrom = ttsTreeExplorer then
  begin
    SetupCalibrationsCallback;
    Exit;
  end;
  RootTreeOnOutgroup;

  if not InferCalibrationsType(FCalibrationType, FUsrOperation) then
    Exit;
  Result := True;
  if (FCalibrationType = ctLeafNode) and (not (FUsrOperation = dtdoRtdtBlens)) then
  begin
    calibrationStrings := TStringList.Create;
    Result := TryGetRTDTCalibrationsFromOtuInfos(calibrationStrings);
    Calibrations.Clear;
    Calibrations.LoadFromList(calibrationStrings);
  end;
  FTreeViewer := TTreeViewForm.Create(Application);
  ATreeViewer := TTreeViewForm(FTreeViewer);
  ATreeViewer.KeepHidden := True;
  {$IFDEF VISUAL_BUILD}
  try
    try
      ATreeViewer.miCaptionExpert.Visible := False;
      TempTreeList := TTreeList.Create;
      TempTreeList.Assign(AnalysisInfo.MyOriTreeList);
      TempTreeList.IsStats := False;
      ATreeViewer.ImportTreeList(TempTreeList, False);
      if not FGroupNamesAreSet then
        ATreeViewer.SetGroupInfo(AnalysisInfo.MyGpNames);

      if FCalibrationType = ctLeafNode then
        ATreeViewer.CalibrationDlg.CalibrationsType := ctLeafNode;
      ATreeViewer.PrepareToWorkWithTimeTreeWizard(AnalysisInfo);
      ATreeViewer.CalibrationDlg.FinalizeCalibrationFunc := AnalysisInfo.MyOriTreeList.FinalizeCalibration;
      ATreeViewer.CalibrationDlg.CalibrationWizard := Self;
      ATreeViewer.CalibrationDlg.CWCancelCallBack := CancelCalibrationChanges;
      ATreeViewer.CalibrationDlg.HasBadCalibsFunc := ATreeViewer.HasInvalidCalibrations;
      ATreeViewer.Caption := 'Tree Explorer ('+ AnalysisInfo.MyUserTreeFile + ')';
      ATreeViewer.IsSetRelTimeCalibsMode := True;

      try
        AnchoredTimesCS.Acquire;
        CalibrationsAreSet := False;
        CalibrationsCancelled := False;
        TreeViewerLives := True;
      finally
        AnchoredTimesCS.Release;
      end;
      FTimer := TTimer.Create(nil);
      FTimer.OnTimer := OnTimer;
      FTimer.Interval := 100;
      FTimer.Enabled := True;

      ATreeViewer.CalibrationDlg.ResetForm;
      ATreeViewer.CalibrationDlg.ClearCalibrations;
      if Assigned(calibrationStrings) and (calibrationStrings.Count > 0) then
        ATreeViewer.CalibrationDlg.TryLoadData(calibrationStrings);
      ATreeViewer.CalibrationDlg.Show;
      ATreeViewer.CalibrationDlg.BringToFront;
      {$IFDEF DARWIN}
      ATreeViewer.CalibrationDlg.Tree.ScrollToBottom;
      ATreeViewer.CalibrationDlg.Tree.Invalidate;
      {$ENDIF}
      if CalibrationType = ctLeafNode then
      begin
        if Assigned(calibrationStrings) and (calibrationStrings.Count > 0) then
          ATreeViewer.CalibrationDlg.WarnToCheckInferredTipDates
        else
          ATreeViewer.CalibrationDlg.PromptIfTipDatesInNames;
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(calibrationStrings) then
      calibrationStrings.Free;
  end;
{$ENDIF}
end;

procedure TTimeTreeWizard.SetAnalysisType(const Value: TTimeTreeAnalysis);
begin
  FAnalysisType := Value;
  case FAnalysisType of
    ttaReltime:;
    ttaCorrtest:;
  end;
end;

procedure TTimeTreeWizard.SeqDataInfoDoneCallback(Sender: TObject);
begin
     WizardFormUpdate(ttpsLoadTree);
end;

procedure TTimeTreeWizard.SeqDataInfoCancelCallback(Sender: TObject);
begin
  WizardFormUpdate(ttpsLoadSeqData);
end;

function TTimeTreeWizard.TryGetRTDTCalibrationsFromOtuInfos(var CalibrationStrings: TStringList): Boolean;
var
  i: Integer;
  aInfo: TOtuInfo = nil;
  aYear, aMonth, aDay: Word;
begin
  Result := False;
  CalibrationStrings.Clear;
  if AnalysisInfo.MyUsedOtuInfos.Count > 0 then
  begin
    Result := True;
    for i := 0 to AnalysisInfo.MyUsedOtuInfos.Count - 1 do
    begin
      aInfo := TOtuInfo(AnalysisInfo.MyUsedOtuInfos[i]);
      if aInfo.HasDateInfo then
      begin
        aYear := aInfo.Year;
        aMonth := Ord(aInfo.Month);
        aDay := aInfo.Day;
        CalibrationStrings.Add(Format('!Taxon=%s%s%s sampleTime=%.4d-%.2d-%.2d', [#39, aInfo.Name, #39, aYear, aMonth, aDay]));
      end
      else
      begin
        Result := False;
        CalibrationStrings.Clear;
        Exit;
      end;
    end;
  end;
end;

function TTimeTreeWizard.ValidateOutgroup(var aMsg: String): Boolean;
var
  adapter: TFpNodeTreeDataAdapter = nil;
  ingroupCount: Integer = 0;
  outgroupCount: Integer = 0;
begin
  try
    Result := True;
    adapter := TFpNodeTreeDataAdapter.Create;
    adapter.SetTreeData(AnalysisInfo.MyOriTreeList[0]);
    adapter.ExpandOutgroupFromOutgroupTaxa;
    adapter.CountIngroupAndOutgroupNodes(ingroupCount, outgroupCount);

    if outgroupCount < 1 then
    begin
      Result := False;
      aMsg := 'Reltime requires at least 1 outgroup member but none is defined';
    end;
    if ingroupCount < 3 then
    begin
      Result := False;
      if ingroupCount = 1 then
        aMsg := 'Reltime requires at least 3 ingroup members but only 1 member is found'
      else
        aMsg := Format('Reltime requires at least 3 ingroup members but only %d members are found', [ingroupCount]);
    end;
  finally
    if Assigned(adapter) then
      adapter.Free;
  end;
end;

procedure TTimeTreeWizard.SetCalibrationType(const Value: TCalibrationTarget);
begin
  FCalibrationType := Value;
end;


function TTimeTreeWizard.OutgroupIsDefined: Boolean;
begin
  if LaunchedFrom = ttsTreeExplorer then
    Result := (TempTreeList.NumOutgroupMembers(0) > 0)
  else
    Result := D_InputSeqData.OtuInfos.OutgroupIsDefined;
end;

function TTimeTreeWizard.SetOutgroupViaSelectOutgroupDlg: Boolean;
var
  MResult: Integer;
  aMsg: String = '';
begin
  Result := False;
  SelectOutgroupTaxaDlg.SetIngroupTaxa(AnalysisInfo.MyOtuNames);
  MResult := SelectOutgroupTaxaDlg.ShowModal;
  if MResult = mrOk then
  begin
    UpdateOutgroupInfo(SelectOutgroupTaxaDlg.OutgroupTaxa);
    Result := ValidateOutgroup(aMsg);
    if not Result then
    begin
      ShowMessage(aMsg);
      WizardFormUpdate(ttpsDoOutgroup);
    end
    else
    begin
      Calibrations.Clear;
      GetOutgroupDone;
      if AnalysisType = ttaReltime then
        WizardFormUpdate(ttpsDoCalibrations)
      else
        WizardFormUpdate(ttpsDoSettings);
    end;
  end
  else
  begin
    ShowMessage('An outgroup must be defined for the timetree analysis');
    WizardFormUpdate(ttpsDoOutgroup);
  end;
end;

function TTimeTreeWizard.SetOutgroupViaTaxaGpsDlg: Boolean;
var
  MResult: Integer;
  aMsg: String = '';
begin
  Result := False;
  if not Assigned(TaxaGpsDlg) then
    TaxaGpsDlg := TTaxaGpsDlg.Create(Application);
  TaxaGpsDlg.fillGroupTreeViewFromOtuInfo(True);
  TaxaGpsDlg.JumpToOutgroupPage;
  TaxaGpsDlg.ShowingModal := True;
  MResult := TaxaGpsDlg.ShowModal;
  if MResult = mrOk then
  begin
    TaxaGpsDlg.JumpToTaxaPage; { reset to default page in case the user opens it again later}
    Result := D_InputSeqData.OtuInfos.OutgroupIsDefined;
    if not Result then
    begin
      ShowMessage('An outgroup must be defined for the timetree analysis');
      WizardFormUpdate(ttpsDoOutgroup);
    end
    else
    begin
      Calibrations.Clear;
      AnalysisInfo.GroupInfo.SetTreeOutgroupFromOtuInfos;

      Result := ValidateOutgroup(aMsg);
      if Result then
      begin
        GetOutgroupDone;
        if AnalysisType = ttaReltime then
          WizardFormUpdate(ttpsDoCalibrations)
        else
          WizardFormUpdate(ttpsDoSettings);
      end
      else
      begin
        ShowMessage(aMsg);
        WizardFormUpdate(ttpsDoOutgroup);
      end;
    end;
  end
  else
  begin
    ShowMessage('An outgroup must be defined for the timetree analysis');
    WizardFormUpdate(ttpsDoOutgroup);
  end;
end;

function TTimeTreeWizard.SetOutgroupViaTE: Boolean;
var
  ModalResult: Integer;
  ATreeExplorer: TTreeViewForm = nil;
  ATreeList: TTreeList = nil;
  OutgroupTaxa: TStringList = nil;
begin
  Result := False;

  try
    try
      OutgroupTaxa := TStringList.Create;
      if Assigned(TempTreeList) then // then the analysis was launched from Tree Explorer and it created a TTreeList for us
      begin
        AnalysisInfo.MyOriTreeList := TempTreeList;
        AnalysisInfo.MyUserNewickTree := TempTreeList.OutputNewickTree(0, True, TempTreeList[0].isStats, 0.0);
      end;
      ATreeList := TTreeList.Create;
      ATreeList.Assign(AnalysisInfo.MyOriTreeList);
      ATreeExplorer := TTreeViewForm.Create(Application); { use a temp tree explorer  }
      ATreeExplorer.ImportTreeList(ATreeList);
      ATreeExplorer.PrepareToRootTreeOnly(rtTimetree); { disables anything we don't want the user doing}
      ATreeExplorer.CaptionHideActionExecute(nil);
      ModalResult := ATreeExplorer.ShowModal; { make the user deal with it}
      if ModalResult = mrOk then
      begin
        ATreeExplorer.SetOutgroupCluster(clRed, -1);
        ATreeExplorer.GetOutgroupTaxa(OutgroupTaxa);
        AnalysisInfo.MyOriTreeList.Clear;
        ATreeExplorer.Tree.AssignTreeList(AnalysisInfo.MyOriTreeList);
        AnalysisInfo.MyOriTreeList.isRooted := True;
        UpdateOutgroupInfo(OutgroupTaxa);
        GetOutgroupDone;
        if AnalysisType = ttaReltime then
          WizardFormUpdate(ttpsDoCalibrations)
        else
          WizardFormUpdate(ttpsDoSettings);
        Result := True;
        Calibrations.Clear; { because the user may have already done it and jumped back to here}
      end
      else
      begin
        Result := False; { the user changed his/her mind}
        WizardFormUpdate(ttpsDoOutgroup);
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! an error occurred: ' + E.Message);
    end;
  finally
    if Assigned(OutgroupTaxa) then
      OutgroupTaxa.Free;
    if Assigned(TempTreeList) then
      AnalysisInfo.MyOriTreeList := nil;
  end;
end;

procedure TTimeTreeWizard.SetUsrOperation(const Value: TDistTreeDlgOption);
begin
  FUsrOperation := Value;
  case FUsrOperation of
    dtdoRelTimeML, dtdoRelTimeBLens, dtdoRelTimeLS: FCalibrationType := ctInternalNode;
    dtdoRtdtML, dtdoRtdtLS, dtdoRtdtBlens: FCalibrationType := ctLeafNode;
  end;
end;

procedure TTimeTreeWizard.SetWizardMode(const Value: TTimeTreeWizardMode);
begin
  FWizardMode := Value;
end;

procedure TTimeTreeWizard.GetOutgroupDone;
begin
  if LaunchedFrom = ttsTreeExplorer then
  begin
    Assert(Assigned(OutgroupsAssignedCallback));
    OutgroupsAssignedCallback(AnalysisInfo.MyGpNames);
  end;
end;

procedure TTimeTreeWizard.UpdateOutgroupInfo(OutgroupTaxa: TStringList);
begin
  if OutgroupTaxa.Count = 0 then
    Exit;
  AnalysisInfo.GroupInfo.UpdateOutgroupTaxa(OutgroupTaxa);
end;

procedure TTimeTreeWizard.ValidateSequenceData;
begin
  if AnalysisInfo.MyNoOfSites < 1 then
    RaiseErrorMessage(HC_No_Common_Sites, 'No common sites found. ' + LineEnding + NoCommonSitesStr);
  if AnalysisInfo.MyNoOfSeqs < 3 then
    RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least three sequences are needed for Likelihood RelTime analysis');
  if AnalysisInfo.MyTreePack.DoesContain(ttBootstrap) then
  begin
    if AnalysisInfo.MyNoOfSeqs < 4 then
     RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least four taxa are needed for bootstrapping.');
  end;
end;

end.
