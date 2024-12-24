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

unit ProcessMLTreeCmds;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, MOtuInfo, MAnalysisInfo, MDistPack, MLTreeAnalyzer, MLModels,
  MProcessPack, ProcessTreeCmds, MCalibrationData, MegaConsts,
  MSubstitutionModelUtils;

{$IFDEF DEBUG}
function CheckHasSitesWithNoData(aSeqs: TStringList): Boolean;
{$ENDIF}
procedure ProcessSeqMLPhylogenyCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack);
procedure ProcessSeqMLRelTimeCCCommand(UsrOperation: TDistTreeDlgOption; ProcessPack: TProcessPack);
procedure ProcessSeqMLRelTimeProtoCommand(UsrOperation: TDistTreeDlgOption; ProcessPack: TProcessPack);
procedure EstimateRelativeDivTimes(MAI: TAnalysisInfo);
procedure EstimateAnchoredDivtimes(MAI: TAnalysisInfo; MinTimes, MaxTimes: TDivTimesArray); overload;
procedure EstimateAnchoredDivTimes(MAI: TAnalysisInfo; MinTimes, MaxTimes, SamplingTimes: TDivTimesArray); overload;
procedure EstimateSampledDivTimes(MAI: TAnalysisInfo; SamplingTimes: TDivTimesArray);
procedure RunCorrTestML(MAI: TAnalysisInfo);
procedure ComputeEpML(MAI: TAnalysisInfo);
function CheckAbortIfMemoryUsageHigh(MAI: TAnalysisInfo): Boolean; deprecated;
function CheckAbortForIdenticalSeqsML(aInfo: TAnalysisInfo): Boolean;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  MutationDetailView, Mega_Main, MTaxaGpsDlg, MEditorForm,
  {$ELSE}
  MD_MegaMain, MegaUtils_NV, math, MAnalysisWizard, stringutils,
  {$ENDIF}
  LCLIntf, LCLType,
  SysUtils, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons,  MegaErrUtils, madaptive_model_test,
  MNucDist, MD_InputSeqData,  MRuntimeProgressDlg, mmodel_info_list,
  MTreeData, MTreeList, MPartitionList, MTreePack, MLSearchThread, ShowTrees,
  MLTree, ErrorMessages_HC, MLegendGenerator, mreltimethreads,
  MTreeDataAdapter, MPartitionData, mmemutils, mepthreads, mcheck_abort_reltime,
  MegaAnalysisPrefStrings, ml_thread_terminator;

procedure ComputeTree(MAI: TAnalysisInfo; IsUpdateARP: Boolean); forward;
procedure ComputeBootstrapTree(MAI: TAnalysisInfo); forward;
procedure AnalyzeUserTree(MAI: TAnalysisInfo; IsUpdateARP: Boolean); forward;
procedure TestModels(MAI: TAnalysisInfo; IsUpdateARP: Boolean); forward;
procedure TestModelsAdaptively(aInfo: TAnalysisInfo); forward;
procedure TestMolecularClock(MAI: TAnalysisInfo; IsUpdateARP: Boolean); forward;
procedure PromptForOutgroup(AnalysisInfo: TAnalysisInfo; var Response: Integer); forward;


{$IFDEF DEBUG}
function CheckHasSitesWithNoData(aSeqs: TStringList): Boolean;
var
  site, seq: Integer;
  aNumSites: Integer;
  dataFound: Boolean = False;
  siteData: String = '';
  output: TStringList = nil;
begin
  try
    Result := False;
    aNumSites := Length(aSeqs[0]);
    output := TStringList.Create;
    output.Add(Format('%12s config', ['site']));
    for site := 1 to aNumSites do
    begin
      dataFound := False;
      for seq := 0 to aSeqs.Count - 1 do
      begin
        if (aSeqs[seq][site] <> '?') and (aSeqs[seq][site] <> '-') and (aSeqs[seq][site] <> 'N') then
        begin
          dataFound := True;
          break;
        end;
      end;
      if not dataFound then
      begin
        siteData := EmptyStr;
        for seq := 0 to aSeqs.Count - 1 do
          siteData += aSeqs[seq][site];
        output.Add(Format('%12.0n %s', [site*1.0, siteData]));
      end;
    end;
    //{$IFDEF VISUAL_BUILD}
    //OpenStringList(output, 'Sites With No Data', False);
    //{$ELSE}
    //output.SaveToFile(NextAvailableFilenameNV('_sites_w_no_data.txt'));
    //{$ENDIF}
  finally
    if Assigned(output) then
      output.Free;
  end;
end;
{$ENDIF}

procedure ProcessSeqMLPhylogenyCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack);
var
  MAI : TAnalysisInfo = nil;
  AModelList: TModelInfoList = nil;  // used in model testing
  i: Integer;
  UserTreeIsRequired: Boolean = False;
  Response: Integer = 0;
  aTreeList: TTreeList = nil;
  data: TTreeData = nil;
  tda: TSimpleTreeDataAdapter = nil;
  outgroupInfo: TOutgroupNameValuePairArray;
begin
  {$IFDEF DEBUG}InitKTtime;{$ENDIF}
  MAI := TAnalysisInfo.CreateWithParams(UsrOperation, ProcessPack);
  MAI.PropagateConstraints := (not ProcessPack.HasProcessType(ppSkipMinMax));
  MAI.ARP := TRuntimeProgress.Create(Application);
  {$IFDEF VISUAL_BUILD}
  MAI.ARP.DataFileName  :=  MegaForm.DataFileName;
  MAI.ARP.DataTitle     :=  MegaForm.DataTitle;
  {$ELSE}
  MAI.ARP.DataFileName  :=  D_MegaMain.DataFileName;
  MAI.ARP.DataTitle     :=  D_MegaMain.DataTitle;
  {$ENDIF}

  MAI.ARP.FMAI := MAI;

  if not MAI.GetAnalysisOptions(UsrOperation) then
  begin
    if MAI.ARP.Visible then
      MAI.ARP.Hide;
    if Assigned(MAI.MyProcessPack) and MAI.MyProcessPack.IsGettingWorkflowSettings then
      MAI := nil;
    Exit;
  end;

  try
    try
    MAI.ARP.Show;
    MAI.ARP.WriteAnalysisOptionsToStdOut;
    MAI.ARP.AddRunStatusInfo('Status', 'Preparing data');
    MAI.ARP.Refresh;
    Application.ProcessMessages;

    MAI.MySeqStrings := TStringList.Create;
    if UsrOperation = dtdoMLInferAncSeqMyPeg then // if the user has unselected any taxa we reselect them because we are using a fixed tree with all 46 species
    begin
      for i := 0 to D_InputSeqData.FOtuInfos.NoOfOtus - 1 do
        D_InputSeqData.FOtuInfos[D_InputSeqData.DispTaxaList[i]].IsUsed := true;
      MAI.IsMyPegAnalysis := True;
      MAI.MyPegSite := MAI.MyProcessPack.MyPegPeptideSite; // don't like doing it this way but this information is lost in myprocesspack once the analysis thread is done
    end;
    MAI.MySeqPartitions := TListOfSeqPartitions.Create;

    { TODO 1 -oglen -cpartitionedML : switch to PrepareDataForPartitionedMLAnalysis below as soon as we are ready }
//    MAI.MyIncludedSites := D_InputSeqData.PrepareDataForPartitionedMLAnalysis(MAI.MySubsetOpt, MAI.MySeqPartitions, MAI.MyUsedOtuInfos,
//                                            MAI.MyNoOfSeqs, MAI.MyNoOfSites,  MAI.MyLabelsUsed, MAI.RecodeScheme,MAI.SiteCoverage);

    if MAI.MyTreePack.DoesContain(ttUserTree) then
    begin
      if UsrOperation = dtdoMLInferAncSeqMyPeg then
      begin
        MAI.GetUCSC_46_SpeciesTree;
      end
      else
      begin
        UserTreeIsRequired := (UsrOperation = dtdoMLInferAncSeq) or
                              (UsrOperation = dtdoMLComputeUserTreeBLens) or
                              (UsrOperation = dtdoEpML) or
                              (UsrOperation = dtdoMLPredictLivingSeq) or
                              (UsrOperation = dtdoMLClockTest); // if true, the user cannot request an auto generated tree

        if not MAI.GetUserTreeInput(UsrOperation,
                                    MAI.MyUserTreeFile,
                                    MAI.ARP,
                                    nil,
                                    ttNJ,
                                    UserTreeIsRequired) then  // This is for InferAncSeq or Analyze User Tree ML to disallow users to click 'No' to generate a tree automatically.
          Exit;
      end;
    end;
    if not MAI.ARP.Visible then
      MAI.ARP.Show;
    D_InputSeqData.ARP := MAI.ARP;
    MAI.MyIncludedSites := D_InputSeqData.PrepareDataForMLAnalysis(MAI, MAI.NeedsMissingDataCounted);
    {$IFDEF DEBUG}
    CheckHasSitesWithNoData(MAI.MySeqStrings);
    {$ENDIF}
    MAI.ARP.AddAnalysisOptions('No. of Seqs', Format('%.0n', [MAI.MyNoOfSeqs*1.0]));
    MAI.ARP.AddAnalysisOptions('No. of Sites', Format('%.0n', [MAI.MyNoOfSites*1.0]));

    if MAI.MyNoOfSites < 1 then
      RaiseErrorMessage(HC_No_Common_Sites, 'No common sites found. ' + LineEnding + NoCommonSitesStr);
    if MAI.MyNoOfSeqs < 3 then
      RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least three sequences are needed for likelihood analysis');

    if (UsrOperation <> dtdoMLModelTest) and (UsrOperation <> dtdoMLModelTamer) and (UsrOperation <>dtdoMLModelTestIQTree) then
    begin
      if (MAI.NoOfSeqs < 4) and ((MAI.MyInitialTreeMethod = DefaultInitTreeMethod) or (MAI.MyInitialTreeMethod = MultipleMPTreesMethod) or (MAI.MyInitialTreeMethod = MPInitTreeMethod) or (MAI.MyInitialTreeMethod = MultipleMPTreesMethod)) then
        RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, Format('At least 4 taxa are needed to use MP for creating the initial tree. Try setting the "%s" setting to "%s"', [opsMLInitialTrees2, InitialTreeByNJStr]));
    end;

    if MAI.MyTreePack.DoesContain(ttBootstrap) then
    begin
      if MAI.MyNoOfSeqs < 4 then
       RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least four taxa are needed for bootstrapping.');
    end;

    {$IFDEF VISUAL_BUILD}
    if (MAI.MyNumThreadsToUse > 1) or MAI.MyTreePack.DoesContain(ttModelTest) then
      MegaForm.MultithreadedCalcIsRunning := True
    else
      MegaForm.NumSingleThreadedMLAnalyses := MegaForm.NumSingleThreadedMLAnalyses + 1;
    {$ENDIF}

    { now call the appropriate handler for the selected analysis}
      if UsrOperation = dtdoMLModelTest then
        TestModels(MAI, True)
    else if UsrOperation = dtdoEpML then
      ComputeEpML(MAI)
    else if MAI.MyTreePack.DoesContain(ttInferTree) then
    begin
      if MAI.MyTreePack.DoesContain(ttBootstrap) then
      begin
        MAI.MyBootReps := MAI.MyTreePack.BootReps;
        MAI.MyBootPartitionList := TPartitionList.Create(MAI.MyNoOfSeqs, 0, false);
          ComputeBootstrapTree(MAI);
      end
      else
        ComputeTree(MAI, True);
    end
    else if  MAI.MyTreePack.DoesContain(ttModelTest) then
      TestModels(MAI, True)
    else if MAI.MyTreePack.DoesContain(ttClockTest) then
    begin
      if UsrOperation = dtdoMLClockTest then
      begin
        TestMolecularClock(MAI, True);
      end;
    end
    else if MAI.MyTreePack.DoesContain(ttAncState) or  // optimize everything but topology
            MAI.MyTreePack.DoesContain(ttBLens) then
    begin
      if MAI.MyTreePack.DoesContain(ttClock) then
      begin
        if not MAI.OutgroupDefined then
        begin
          if MAI.UserInputTreeIsRooted then
          begin
            try
              tda := TSimpleTreeDataAdapter.Create;
              aTreeList := TTreeList.Create;
              aTreeList.ImportFromNewick(MAI.MyUserNewickTree, MAI.MyOtuNames);
              data := aTreeList[0];
              tda.GuessOutgroup(data);
              outgroupInfo := aTreeList.GetOutgroupNameValuePairs(0);
              MAI.TransferTreeOutgroupInfoToOtuInfos(outgroupInfo);
            finally
              if Assigned(tda) then
                tda.Free;
              if Assigned(aTreeList) then
                aTreeList.Free;
              SetLength(outgroupInfo, 0);
            end;
          end;
          {$IFDEF VISUAL_BUILD}
          PromptForOutgroup(MAI, Response);
          if Response <> mrOk then
          begin
            ShowMessage('Molecular clock test requires that an outgroup is defined but it is not. Aborting the analysis.');
            Exit;
          end;
          {$ENDIF}
        end;
      end;
      AnalyzeUserTree(MAI, True);
    end
    else if MAI.MyTreePack.DoesContain(ttPattern) or
            MAI.MyTreePack.DoesContain(ttTsTvBias) or
            MAI.MyTreePack.DoesContain(ttSiteRates) or
            MAI.MyTreePack.DoesContain(ttGamma) then
    begin
      ComputeTree(MAI, True);
    end;
    MAI := nil;
  except
    on E: EAbort do
    begin
      {$IFDEF VISUAL_BUILD}
      if MAI.ARP <> nil then
        MAI.ARP.Hide;
      ShowMessage('ML analysis cancelled');
      if UsrOperation = dtdoMLInferAncSeqMyPeg then
      begin
        MutationDetailViewForm.AncMLAction.Enabled := True;
        MutationDetailViewForm.AncParsimonyAction.Enabled := True;
      end;
      {$ELSE}
      Error_NV(E.Message, E);
      {$ENDIF}
    end;
    on E: Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      if MAI.ARP <> nil then
        MAI.ARP.Hide;
      ShowErrorMessage(E);
      if UsrOperation = dtdoMLInferAncSeqMyPeg then
      begin
        MutationDetailViewForm.AncMLAction.Enabled := True;
        MutationDetailViewForm.AncParsimonyAction.Enabled := True;
      end;
      {$ELSE}
      Error_NV(E.Message, E);
      {$ENDIF}
    end;
  end;
  finally
    if MAI <> nil  then
    begin
      if MAI.ARP <> nil then
        MAI.ARP.Free;
      MAI.MyProcessPack := nil;
      MAI.Free;
    end;
    if AModelList <> nil then
      AModelList.Free;
  end;
end;

procedure ProcessSeqMLRelTimeCCCommand(UsrOperation: TDistTreeDlgOption; ProcessPack: TProcessPack);
var
  Progress : TRuntimeProgress = nil;
  AnalysisInfo : TAnalysisInfo = nil;
  AModel: TGammaRateVariationModel = nil;
  AMLTreeAnalyzer: TMLTreeAnalyzer = nil;
  ATree: TTreeData = nil;
  ClockLevel: TClockLevel;
  Calibrations: TCalibrations = nil;
  CalibrationFile: String = '';
  MinTimes, MaxTimes: TDivTimesArray;
  SamplingTimes: TDivTimesArray;
  HasSampleTimes: Boolean = False;
  HasDivTimes: Boolean = False;
begin
  SetLength(SamplingTimes, 0);
  SetLength(MinTimes, 0);
  SetLength(MaxTimes, 0);

  try try
    AnalysisInfo := TAnalysisInfo.Create;
    AnalysisInfo.MyProcessPack := ProcessPack;
    AnalysisInfo.PropagateConstraints := (not ProcessPack.HasProcessType(ppSkipMinMax));
    AnalysisInfo.InitialUsrOperation := UsrOperation;

    Progress := TRuntimeProgress.Create(Application);
    {$IFNDEF VISUAL_BUILD}
    Progress.DataFileName  :=  D_MegaMain.DataFileName;
    Progress.DataTitle     :=  D_MegaMain.DataTitle;
    {$ENDIF}
    AnalysisInfo.ARP := Progress;
    Progress.FMAI := AnalysisInfo;
    Progress := nil;

    if not AnalysisInfo.GetAnalysisOptions(UsrOperation) then
      Exit;
    AnalysisInfo.ARP.WriteAnalysisOptionsToStdOut;
    AnalysisInfo.ARP.AddRunStatusInfo('Status', 'Preparing data');
    AnalysisInfo.ARP.Show;
    AnalysisInfo.MySeqStrings := TStringList.Create;
    AnalysisInfo.MySeqPartitions := TListOfSeqPartitions.Create;

    { TODO 1 -oglen -cpartitionedML : switch to PrepareDataForPartitionedMLAnalysis below as soon as we are ready }
//    AnalysisInfo.MyIncludedSites := D_InputSeqData.PrepareDataForPartitionedMLAnalysis(AnalysisInfo.MySubsetOpt, AnalysisInfo.MySeqPartitions, AnalysisInfo.MyUsedOtuInfos, AnalysisInfo.MyNoOfSeqs, AnalysisInfo.MyNoOfSites,  AnalysisInfo.MyLabelsUsed, AnalysisInfo.RecodeScheme,AnalysisInfo.SiteCoverage);
    D_InputSeqData.ARP := Progress;
    AnalysisInfo.MyIncludedSites := D_InputSeqData.PrepareDataForMLAnalysis(AnalysisInfo, AnalysisInfo.NeedsMissingDataCounted);
    {$IFNDEF VISUAL_BUILD}
    CheckAbortForIdenticalSeqsML(AnalysisInfo);
    {$ENDIF}
    AnalysisInfo.ARP.AddAnalysisOptions('No. of Seqs', Format('%.0n', [AnalysisInfo.MyNoOfSeqs*1.0]));
    AnalysisInfo.ARP.AddAnalysisOptions('No. of Sites', Format('%.0n', [AnalysisInfo.MyNoOfSites*1.0]));
    if not AnalysisInfo.OutgroupDefined then { force the user to define an outgroup}
      raise Exception.Create('The timetree analysis requires that an outgroup be defined');
    // validate the data
    if AnalysisInfo.MyNoOfSites < 1 then
      RaiseErrorMessage(HC_No_Common_Sites, 'No common sites found. ' + LineEnding + NoCommonSitesStr);

    if AnalysisInfo.MyNoOfSeqs < 3 then
      RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least three sequences are needed for likelihood analysis');

    if AnalysisInfo.MyTreePack.DoesContain(ttBootstrap) then
    begin
      if AnalysisInfo.MyNoOfSeqs < 4 then
       RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least four taxa are needed for bootstrapping.');
    end;

    if not AnalysisInfo.GetUserTreeInput(UsrOperation,
                                AnalysisInfo.MyUserTreeFile,
                                AnalysisInfo.ARP,
                                nil,
                                ttNJ,
                                True) then
      Exit;

    //Now create the Treelist, model and setup the MLTreeAnalyzer
    AModel := CreateSubstitutionModel(AnalysisInfo);
    AnalysisInfo.MyOriTreeList := TTreeList.Create;
    AnalysisInfo.MyOriTreeList.ImportFromNewick(AnalysisInfo.MyUserNewickTree, AnalysisInfo.MyOtuNames);
    AnalysisInfo.GroupInfo.SetTreeOutgroupFromOtuInfos;
    {$IFNDEF VISUAL_BUILD}
    if not AnalysisInfo.IngroupDefined then
      error_nv('An ingroup must be defined for the timetree analysis');
    {$ENDIF}

    RootOnOutgroup(AnalysisInfo); { must be done before MLTreeAnalyzer is created}
    ATree := TTreeData.Create(AnalysisInfo.NoOfSeqs,false,false,false);
    ATree.Assign(AnalysisInfo.MyOriTreeList[0]);
    AMLTreeAnalyzer := TMLTreeAnalyzer.Create(AnalysisInfo.MySeqStrings, ATree, AModel);
    AMLTreeAnalyzer.NoOfThreadsToUse := AnalysisInfo.MyNumThreadsToUse;

    if AnalysisInfo.MLSearchFilter > 0 then
        AMLTreeAnalyzer.SearchFilter := AnalysisInfo.MLSearchFilter;

    AMLTreeAnalyzer.IsGlobalClock := False;
    AnalysisInfo.ClockTypeSet := True;
    ClockLevel := AnalysisInfo.ClockLevel;
    Case ClockLevel of
      clOneStdErr:
        AMLTreeAnalyzer.GlobalClockLevel := 1;
      clTwoStdErr:
        AMLTreeAnalyzer.GlobalClockLevel := 2;
      clThreeStdErr:
        AMLTreeAnalyzer.GlobalClockLevel := 3;
        End;

    AModel := nil;

    // Give MLTreeAnalyzer to AnalysisInfo and do the rest
    AnalysisInfo.MyMLAnalysisPack := AMLTreeAnalyzer;
    AMLTreeAnalyzer := nil;
    if AnalysisInfo.IsSubsampling then
      AnalysisInfo.MyBootPartitionList := TPartitionList.Create(AnalysisInfo.MyNoOfSeqs, 0, False);
    if AnalysisInfo.MyProcessPack.CalibrationFile <> EmptyStr then
      CalibrationFile := AnalysisInfo.MyProcessPack.CalibrationFile;
    if CalibrationFile <> EmptyStr then
    begin
      AnalysisInfo.CalibrationFile := CalibrationFile;
      Calibrations := TCalibrations.Create;
      Calibrations.SetInfo(AnalysisInfo);
      if not Calibrations.LoadFromFile(CalibrationFile) then
        raise Exception.Create('Failed to load calibrations');
      {$IFNDEF VISUAL_BUILD}
      Calibrations.FilterMissingTaxa(AnalysisInfo.MyOtuNames);
      {$ENDIF}
      AnalysisInfo.MyOriTreeList.ResolveCalibrationTimeNames(Calibrations);
      AnalysisInfo.MyOriTreeList[0].resolveRelTimes(Calibrations);
      AnalysisInfo.CalibrationTimes := Calibrations;
      Calibrations.GetCalibrationTypes(HasDivTimes, HasSampleTimes);
        if (HasSampleTimes and (not HasDivTimes)) then
          EstimateSampledDivTimes(AnalysisInfo, SamplingTimes)
      else if HasDivTimes then
        EstimateAnchoredDivtimes(AnalysisInfo, MinTimes, MaxTimes);
    end
    else
    begin
      if AnalysisInfo.MyUsrOperation = dtdoCorrTestML then
        RunCorrTestML(AnalysisInfo)
      else
        EstimateRelativeDivTimes(AnalysisInfo)
    end;

    AMLTreeAnalyzer := nil;
    AnalysisInfo := nil;
  except
    on E: Exception do
      {$IFNDEF VISUAL_BUILD}Error_NV(E.Message, E);{$ENDIF}
  end;
  finally
    if Progress <> nil then
      Progress.Free;
    if AnalysisInfo <> nil  then
    begin
      if AnalysisInfo.ARP <> nil then
        AnalysisInfo.ARP.Free;
      AnalysisInfo.Free;
    end;
    if AModel <> nil then
      AModel.Free;
    if AMLTreeAnalyzer <> nil then
      AMLTreeAnalyzer.Free;
  end;
end;

procedure ProcessSeqMLRelTimeProtoCommand(UsrOperation: TDistTreeDlgOption; ProcessPack: TProcessPack);
var
  AnalysisInfo: TAnalysisInfo;
begin
  AnalysisInfo := TAnalysisInfo.Create;
  AnalysisInfo.MyProcessPack := ProcessPack;
  AnalysisInfo.InitialUsrOperation := UsrOperation;
  AnalysisInfo.GetAnalysisOptions(dtdoRelTimeML)
end;

procedure TestModelsAdaptively(aInfo: TAnalysisInfo);
var
  modelTestThread: TAdaptiveModelTestThread = nil;
begin
  try
    if not aInfo.ARP.Visible then
      aInfo.ARP.Show;
    modelTestThread := TAdaptiveModelTestThread.Create(aInfo);
    modelTestThread.ProgressDlg := aInfo.ARP;
    {$IFDEF VISUAL_BUILD}
    modelTestThread.OnTerminate := MegaForm.AdaptiveModelTestDone;
    {$ELSE}
    modelTestThread.OnTerminate := D_MegaMain.AdaptiveModelTestDone;
    {$ENDIF}
    aInfo.ARP.Thread := modelTestThread;
    modelTestThread.Start;
    modelTestThread := nil;
  finally
    if Assigned(modelTestThread) then
      modelTestThread.Free;
  end;
end;

procedure TestMolecularClock(MAI: TAnalysisInfo; IsUpdateARP: Boolean);
var
  ClockTestThread: TMLClockTestThread;
  Terminator: TMLThreadTerminator;
  Response: Integer = -1;
  aMsg: String = '';
  isRooted: Boolean = False;
begin
  ClockTestThread := nil;
  Terminator := nil;

  try
    isRooted := IsRootedTree(MAI.MyUserNewickTree, MAI.MyOtuNames);
    if (not MAI.OutgroupDefined) and (not isRooted) then
    begin
      {$IFDEF VISUAL_BUILD}
      PromptForOutgroup(MAI, Response);
      if Response <> mrOk then
      begin
        ShowMessage('the molecular clock test requires a rooted tree but the input tree is not rooted. Aborting analysis.');
        Exit;
      end;
      {$ELSE}
      aMsg := 'The molecular clock test requires that the input tree is rooted.' + LineEnding;
      aMsg += #9 + 'You can provide a rooted tree or define an outgroup that MEGA will use to automatically root the tree.' + LineEnding;
      aMsg += #9 + 'To specify taxa that comprise an outgroup, create a text file (e.g. groups.txt) that specifies outgroup members in the form: ' + LineEnding;
      aMsg += #9 + '     taxon1=outgroup' + LineEnding;
      aMsg += #9 + '     taxon2=outgroup' + LineEnding;
      aMsg += #9 + 'The outgroup can be comprised of 1 or more taxa. The command line for running the molecular clock test analysis will then be something like:' + LineEnding;
      aMsg += #9 + Format('     megacc -a %s -d %s -t %s -g groups.txt -o %s', [ExtractFileName(D_MegaMain.AnalysisPreferencesFileName), ExtractFileName(D_MegaMain.DataFileName), ExtractFileName(D_MegaMain.CommandLineTreeFileName), ExtractFileName(D_MegaMain.OutputFileName)]);
      raise Exception.Create(aMsg);
      {$ENDIF}
    end;
    {$IFNDEF VISUAL_BUILD}
    if not isRooted then
      MAI.SetupOutgroupMembers;
    {$ENDIF}
    ClockTestThread := TMLClockTestThread.Create(nil);
    ClockTestThread.AnalysisInfo := MAI;
    ClockTestThread.Info := MAI;
    Terminator := TMLThreadTerminator.Create;
    ClockTestThread.OptimizeParams := true;
    ClockTestThread.ProgressDlg := MAI.ARP;
    {$IFDEF VISUAL_BUILD}
    if MAI.IsLargeDataForML  then
      ClockTestThread.SubTasksCheckCancel := MAI.ARP.AddSubTask('Initializing', 0).CheckCancelFunc;
    {$ENDIF}
    ClockTestThread.ShowProgress := true;
    ClockTestThread.OnTerminate := Terminator.OnThreadDone;
    MAI.ARP.Thread := ClocktestThread;
    ClockTestThread.Start;
  Except
    on E:Exception do
    begin
      if Assigned(Terminator) then
        Terminator.Free;
      if Assigned(ClockTestThread) then
        ClockTestThread.Free;
      raise Exception.Create(E.Message);
    end;
  end;
end;

procedure RunCorrTestML(MAI: TAnalysisInfo);
var
  CorrTestThread: TCorrTestMLThread;
  Terminator: TRelTimeTerminator;
begin
  CorrTestThread := nil;
  Terminator := nil;

  try
    if (not MAI.MyOriTreeList.IsRooted) and  (not MAI.OutgroupDefined) then
      raise Exception.Create('No outgroup has been defined but it is required for the corrtest analysis');
    if not MAI.MyOriTreeList.IsRooted then
      RootOnOutgroup(MAI);
    CorrTestThread := TCorrTestMLThread.Create(MAI.MyMLAnalysisPack);
    CorrTestThread.Info := MAI;
    CorrTestThread.AnalysisInfo := MAI;
    Terminator := TRelTimeTerminator.Create(MAI.InitialUsrOperation);
    CorrTestThread.OptimizeParams := true;
    CorrTestThread.ProgressDlg := MAI.ARP;
    {$IFDEF VISUAL_BUILD}
    if MAI.IsLargeDataForML then
      CorrTestThread.SubTasksCheckCancel := MAI.ARP.AddSubTask('Initializing', 0).CheckCancelFunc;
    {$ENDIF}
    CorrTestThread.ShowProgress := true;
    CorrTestThread.OnTerminate := Terminator.OnCorrTestThreadDone;
    MAI.ARP.Thread := CorrTestThread;

    {$IFDEF VISUAL_BUILD}
    if MAI.MyNumThreadsToUse > 1 then
      MegaForm.MultithreadedCalcIsRunning := True
    else
      MegaForm.NumSingleThreadedMLAnalyses := MegaForm.NumSingleThreadedMLAnalyses + 1;
    {$ENDIF}
    CorrTestThread.Start;
  Except
    on E:Exception do
      {$IFDEF VISUAL_BUILD}
      ShowMessage('An application error has occurred: ' + E.Message)
      {$ELSE}
      warn_NV('An error has occurred: ' + E.Message);
      {$ENDIF}
  end;
end;


function CheckAbortIfMemoryUsageHigh(MAI: TAnalysisInfo): Boolean; deprecated;
var
  nseqs, nrates, nsites, nstates: Integer;
  searchLevel: Integer;
  leafNodeBytes, InternalNodeBytes: Int64;
  totalBytes, mlTreeBytes, analyzerBytes: Int64;
  modelBytes: Int64;
  memAvailable: Int64 = 0;
  msgString: String = '';
  {$IFDEF VISUAL_BUILD}
  response: Integer = -1;
  {$ENDIF}
begin
  raise Exception.Create('call to deprecated method "CheckAbortIfMemoryUsageHigh". Estimates are no longer valid due to optimization of memory usage for the ML system.');
  Result := False;
  if MAI.IsSubsampling then
    Exit;
  Assert(Assigned(MAI.MyDistPack), 'missing TDistPack');
  if MAI.MyDistPack.DoesContain(gdGamma) then
    nrates := MAI.MyDistPack.NoOfGammaCategories
  else
    nrates := 1;
  nsites := MAI.MyNoOfSites;
  nseqs := MAI.MyNoOfSeqs;
  if MAI.MyTreePack.DoesContain(ttSPRFast) then
    searchLevel := 3
  else if MAI.MyTreePack.DoesContain(ttSPRExtensive) then
    searchLevel := 5
  else
    searchLevel := 1;
  if MAI.isAminoAcid or MAI.MyDistPack.DoesContain(gdAmino) then
    nstates := 20
  else
    nstates := 4;
  leafNodeBytes := ApproximateSizeOfTreeNodeInBytes(true, nseqs, nsites, nrates, nstates);
  leafNodeBytes := leafNodeBytes*(nseqs);
  internalNodeBytes := ApproximateSizeOfTreeNodeInBytes(false, nseqs, nsites, nrates, nstates);
  internalNodeBytes := internalNodeBytes*(nseqs - 1) + internalNodeBytes*(searchLevel*2 + 4)*(MAI.MyNumThreadsToUse + 1) + SizeOf(TMLTreeNode)*(searchLevel*2 + 4);
  mlTreeBytes := TMLTree.InstanceSize;
  analyzerBytes := TMLTreeAnalyzer.InstanceSize;

  if nstates = 20 then
    modelBytes := GetApproximateSizeOfProteinModelInBytes(nsites, nrates)
  else
    modelBytes := GetApproximateSizeOfDnaModelInBytes(nsites, nrates);

  totalBytes := leafNodeBytes + internalNodeBytes + mlTreeBytes + analyzerBytes + modelBytes*(MAI.MyNumThreadsToUse + 1);
  totalBytes := Round(totalBytes*1.05); { add a fudge factor}
  MAI.AnalysisSummary.EstimatedMemUsage := totalBytes;
  if GetAvailablePhysicalMemory(memAvailable) then
  begin
    if totalBytes > memAvailable*0.75 then
    begin
      if totalBytes > memAvailable*0.90 then
        msgString := Format('The analysis will use ~%.2f GB of system RAM and only ~%.2f GB are available so this may freeze your system. Continue anyway?', [totalBytes/(1024*1024*1024), memAvailable/(1024*1024*1024)])
      else
        msgString := Format('The analysis will use ~%.2f GB of system RAM and only ~%.2f GB are available so system performance may suffer. Continue anyway?', [totalBytes/(1024*1024*1024), memAvailable/(1024*1024*1024)]);;
      {$IFDEF VISUAL_BUILD}
      if Assigned(MAI.ARP) then
        MAI.ARP.Hide;
      Response := MessageDlg(msgString, mtConfirmation, mbYesNo, 0);
      if Response = mrYes then
      begin
        Result := False;
        if Assigned(MAI.ARP) then
          MAI.ARP.Show;
      end
      else
        Result := True;
      {$ELSE}
      Result := False;
      {$ENDIF}
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

function CheckAbortForIdenticalSeqsML(aInfo: TAnalysisInfo): Boolean;
var
  checkAbort: TCheckAbortReltime = nil;
begin
  Result := False;
  try
    checkAbort := TCheckAbortReltime.Create(aInfo.isAminoAcid);
    Result := checkAbort.CheckAbortML(aInfo.MySeqStrings, aInfo.MyOtuNames, aInfo.ARP);
  finally
    if Assigned(checkAbort) then
      checkAbort.Free;
  end;
end;


procedure PromptForOutgroup(AnalysisInfo: TAnalysisInfo; var Response: Integer);
{$IFDEF VISUAL_BUILD}
var
  i, MResult: Integer;

  procedure ResetMLCalcThreadInfo;
  begin
    if (AnalysisInfo.MyUsrOperation = dtdoMLTree) or (AnalysisInfo.MyUsrOperation = dtdoMLClockTest) or (AnalysisInfo.MyUsrOperation = dtdoEpML) or (AnalysisInfo.MyUsrOperation = dtdoMLComputeUserTreeBLens) then
    begin
      if MegaForm.MultithreadedCalcIsRunning then
        MegaForm.MultithreadedCalcIsRunning := False
      else if MegaForm.NumSingleThreadedMLAnalyses > 0 then
        MegaForm.NumSingleThreadedMLAnalyses := MegaForm.NumSingleThreadedMLAnalyses - 1;
    end;
  end;
{$ENDIF}
begin
  {$IFDEF VISUAL_BUILD}
  AnalysisInfo.ARP.Hide;
  if AnalysisInfo.MyUsrOperation = dtdoMLClockTest then
    Response := MessageDlg('The molecular clock test requires a rooted tree. You can select taxa to create an outgroup for rooting the tree. Do you want to continue?', mtCustom, mbOKCancel, 0)
  else if AnalysisInfo.DoLbsTimeEstimation then
    Response := MessageDlg('Subsampling time estimation requires that you specify a single outgroup taxon. Do you want to continue?', mtCustom, mbOKCancel, 0)
  else if AnalysisInfo.MyUsrOperation = dtdoEpML then
    Response := MessageDlg('EP calculation requires that you specify an outgroup. Do you want to continue?', mtCustom, mbOKCancel, 0)
  else if AnalysisInfo.MyUsrOperation = dtdoMLComputeUserTreeBLens then
    Response := MessageDlg('Assuming a molecular clock for user tree analysis requires that you specify an outgroup. Do you want to continue?', mtCustom, mbOkCancel, 0)
  else if AnalysisInfo.MyUsrOperation = dtdoMLInferAncSeq then
    Response := MessageDlg('For ancestral sequence inference you have the option to specify an outgroup for rooting the input tree (does not change the result). Do you want to specify an outgroup?', mtCustom, mbYesNoCancel, 0)
  else
    raise Exception.Create('invalid user operation');

  if (AnalysisInfo.MyUsrOperation <> dtdoMLInferAncSeq) and (Response <> mrOk) then
  begin
    ResetMLCalcThreadInfo;
    Exit;
  end;

  if AnalysisInfo.MyUsrOperation = dtdoMLInferAncSeq then
  begin
    if Response = mrCancel then
    begin
      ResetMLCalcThreadInfo;
      Exit;
    end
    else if Response = mrNo then
      Exit;
  end;

  TaxaGpsDlg.fillGroupTreeViewFromOtuInfo(True);
  TaxaGpsDlg.JumpToOutgroupPage;
  TaxaGpsDlg.ShowingModal := True;
  MResult := TaxaGpsDlg.ShowModal;
  if MResult = mrOk then
  begin
    TaxaGpsDlg.JumpToTaxaPage; { reset to default page in case the user opens it again later}
    if AnalysisInfo.DoLbsTimeEstimation and (D_InputSeqData.OtuInfos.NumActiveOutgroupMembers <> 1) then
    begin
      while (MResult = mrOk) and (D_InputSeqData.OtuInfos.NumActiveOutgroupMembers <> 1) do
      begin
        MResult := MessageDlg(Format('Subsampling time estimation requires a single outgroup taxon but %d have been specified. Do you want to update the outgroup?', [D_InputSeqData.OtuInfos.NumActiveOutgroupMembers]), mtCustom, mbOKCancel, 0);
        if MResult = mrOk then
        begin
          TaxaGpsDlg.JumpToOutgroupPage;
          MResult := TaxaGpsDlg.ShowModal;
        end;
      end;
      if MResult <> mrOK then
      begin
        ResetMLCalcThreadInfo;
        Response := mrAbort;
        Exit;
      end;
    end
    else if D_InputSeqData.OtuInfos.NumActiveOutgroupMembers = 0 then
    begin
      while (MResult = mrOk) and (D_InputSeqData.OtuInfos.NumActiveOutgroupMembers = 0) do
      begin
        MResult := MessageDlg(Format('The analysis requires an outgroup but 0 outgroup taxa been specified. Do you want to update the outgroup?', [D_InputSeqData.OtuInfos.NumActiveOutgroupMembers]), mtCustom, mbOKCancel, 0);
        if MResult = mrOk then
        begin
          TaxaGpsDlg.JumpToOutgroupPage;
          MResult := TaxaGpsDlg.ShowModal;
        end;
      end;
      if MResult <> mrOK then
      begin
        ResetMLCalcThreadInfo;
        Response := mrAbort;
        Exit;
      end;
    end;

    if not Assigned(AnalysisInfo.MyGpNames) then
      AnalysisInfo.MyGpNames := TStringList.Create
    else
      AnalysisInfo.MyGpNames.Clear;
    for i := 0 to D_InputSeqData.OtuInfos.NoOfOtus - 1 do
    begin
      if Trim(D_InputSeqData.OtuInfos[i].GpName) <> EmptyStr then
        AnalysisInfo.MyGpNames.Add(D_InputSeqData.OtuInfos[i].Name + '=' + D_InputSeqData.OtuInfos[i].GpName);
    end;
    AnalysisInfo.SetupOutgroupMembers;
    AnalysisInfo.GroupInfo.SetTreeOutgroupFromOtuInfos;
    Response := mrOk;
  end
  else
    Response := mrAbort;
  if Response <> mrOK then
    ResetMLCalcThreadInfo;
  AnalysisInfo.ARP.Show;
  {$ENDIF}
end;


procedure EstimateRelativeDivTimes(MAI: TAnalysisInfo);
var
  RelTimeThread: TRelTimeMLThread;
  Terminator: TRelTimeTerminator;
begin
  RelTimeThread := nil;
  Terminator := nil;

  try
    if (not MAI.MyOriTreeList.IsRooted) and  (not MAI.OutgroupDefined) then
      raise Exception.Create('No outgroup has been defined but it is required for the timetree analysis');
    if not MAI.MyOriTreeList.IsRooted then
      RootOnOutgroup(MAI);
    RelTimeThread := TRelTimeMLThread.Create(MAI.MyMLAnalysisPack);
    ReltimeThread.Info := MAI;
    RelTimeThread.MergeRates := MAI.MergeRates;
    RelTimeThread.AnalysisInfo := MAI;
    Terminator := TRelTimeTerminator.Create(MAI.InitialUsrOperation);
    RelTimeThread.OptimizeParams := true;
    RelTimeThread.ProgressDlg := MAI.ARP;
    {$IFDEF VISUAL_BUILD}
    if MAI.IsLargeDataForML then
      RelTimeThread.SubTasksCheckCancel := MAI.ARP.AddSubTask('Initializing', 0).CheckCancelFunc;
    {$ENDIF}
    RelTimeThread.ShowProgress := true;
    RelTimeThread.OnTerminate := Terminator.OnThreadDone;
    MAI.ARP.Thread := RelTimeThread;

    {$IFDEF VISUAL_BUILD}
    if MAI.MyNumThreadsToUse > 1 then
      MegaForm.MultithreadedCalcIsRunning := True
    else
      MegaForm.NumSingleThreadedMLAnalyses := MegaForm.NumSingleThreadedMLAnalyses + 1;
    {$ENDIF}
    RelTimeThread.Start;
  Except
    on E:Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      if Assigned(MAI.ARP) then
        MAI.ARP.Hide;
      ShowMessage('Oh no! An error has occurred: ' + E.Message)
      {$ELSE}
      warn_NV('An error has occurred: ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

procedure EstimateAnchoredDivTimes(MAI: TAnalysisInfo; MinTimes, MaxTimes, SamplingTimes: TDivTimesArray); overload;
begin
  Assert(False, 'EstimateAnchoredDivTimes with sample times has not yet been implemented');
end;

procedure EstimateSampledDivTimes(MAI: TAnalysisInfo; SamplingTimes: TDivTimesArray);
var
  RelTimeThread: TRelTimeMLThread;
  Terminator: TRelTimeTerminator;
  ErrorMsg: String;
begin
  RelTimeThread := nil;
  Terminator := nil;
  ErrorMsg := EmptyStr;

  try
    try
      RootOnOutgroup(MAI);
      if not MAI.CalibrationTimes.DoAllValidations(ErrorMsg, False, MAI.MyOtuNames) then
      begin
        Assert((vsError in MAI.CalibrationTimes.ValidationMessages) or (vsWarning in MAI.CalibrationTimes.ValidationMessages));
        if vsError in MAI.CalibrationTimes.ValidationMessages then
        begin
          {$IFDEF VISUAL_BUILD}
          raise Exception.Create('Validation of time constraints failed: ' + ErrorMsg);
          {$ELSE}
          error_nv('Validation of time Constraints failed: ' + ErrorMsg);
          {$ENDIF};
        end
        else if vsWarning in MAI.CalibrationTimes.ValidationMessages then
        begin
          {$IFDEF VISUAL_BUILD}
          ShowMessage('TimeConstraints validation warning: ' + ErrorMsg);
          {$ELSE}
          warn_nv('Time Constraints validation warning: ' + ErrorMsg);
          {$ENDIF};
        end;
      end;

      MAI.CalibrationTimes.PrepareSamplingTimeArray(SamplingTimes, MAI.MyOtuNames);
      RelTimeThread := TRelTimeMLThread.Create(nil);
      RelTimeThread.Info := MAI;
      RelTimeThread.SampleTimes := SamplingTimes;
      RelTimeThread.MergeRates := MAI.MergeRates;
      MAI.MyOriTreeList[0].isSE := True;
      if not MAI.MyOriTreeList[0].isBLen then
        MAI.MyOriTreeList.isBLen := True;
      RelTimeThread.AnalysisInfo := MAI;
      Terminator := TRelTimeTerminator.Create(MAI.InitialUsrOperation);
      RelTimeThread.OptimizeParams := true;
      RelTimeThread.ProgressDlg := MAI.ARP;
      {$IFDEF VISUAL_BUILD}
      if MAI.IsLargeDataForML then
        RelTimeThread.SubtasksCheckCancel := MAI.ARP.AddSubTask('Initializing', 0).CheckCancelFunc;
      {$ENDIF}
      RelTimeThread.ShowProgress := true;
      RelTimeThread.OnTerminate := Terminator.OnThreadDone;
      MAI.ARP.Thread := RelTimeThread;

    {$IFDEF VISUAL_BUILD}
      if MAI.MyNumThreadsToUse > 1 then
        MegaForm.MultithreadedCalcIsRunning := True
      else
        MegaForm.NumSingleThreadedMLAnalyses := MegaForm.NumSingleThreadedMLAnalyses + 1;
    {$ENDIF}
      MAI := nil;
      RelTimeThread.Start;
    except
      on E:Exception do
      {$IFDEF VISUAL_BUILD}
        ShowMessage('Oh no! An error occurred when launching the analysis: ' + E.Message);
      {$ELSE}
        error_nv('An error occurred when launching the analysis: ' + E.Message, E);
      {$ENDIF}
    end;
  finally
    if Assigned(MAI) then { probably validation of constraints failed}
    begin
      MAI.MyProcessPack := nil;
      MAI.Free;
      SetLength(SamplingTimes, 0);
    end;
  end;
end;

procedure EstimateAnchoredDivTimes(MAI: TAnalysisInfo; MinTimes, MaxTimes: TDivTimesArray);
var
  RelTimeThread: TAnchoredRelTimeMLThread;
  Terminator: TRelTimeTerminator;
  ErrorMsg: String;
  lbls: TStringList = nil;
begin
  RelTimeThread := nil;
  Terminator := nil;
  ErrorMsg := EmptyStr;

  try
    try
      MAI.MyOriTreeList[0].isSE := True;
      if not MAI.MyOriTreeList[0].isBLen then
        MAI.MyOriTreeList.isBLen := True;
      if not MAI.CalibrationTimes.DoAllValidations(ErrorMsg, False, MAI.MyOtuNames) then
      begin
        Assert((vsError in MAI.CalibrationTimes.ValidationMessages) or (vsWarning in MAI.CalibrationTimes.ValidationMessages));
        if vsError in MAI.CalibrationTimes.ValidationMessages then
        begin
          {$IFDEF VISUAL_BUILD}
          raise Exception.Create('Validation of time constraints failed: ' + ErrorMsg);
          {$ELSE}
          error_nv('Validation of time Constraints failed: ' + ErrorMsg);
          {$ENDIF};
        end
        else if vsWarning in MAI.CalibrationTimes.ValidationMessages then
        begin
          {$IFDEF VISUAL_BUILD}
          ShowMessage('TimeConstraints validation warning: ' + ErrorMsg);
          {$ELSE}
          warn_nv('Time Constraints validation warning: ' + ErrorMsg);
          {$ENDIF};
        end;
      end;
      lbls := MAI.MyOriTreeList.InternalNodeLbls[0];
      MAI.CalibrationTimes.PrepareDivTimeArrays(MinTimes, MaxTimes, MAI.NoOfSeqs, False, lbls); { at first this appears redundant but it is needed so that the original min/max times are included in the tabular output file}
      RelTimeThread := TAnchoredRelTimeMLThread.Create(nil);
      RelTimeThread.Info := MAI;
      RelTimeThread.MinTimes := MinTimes;
      RelTimeThread.MaxTimes := MaxTimes;
      RelTimeThread.MergeRates := MAI.MergeRates;
      RelTimeThread.AnalysisInfo := MAI;
      Terminator := TRelTimeTerminator.Create(MAI.InitialUsrOperation);
      RelTimeThread.OptimizeParams := true;
      RelTimeThread.ProgressDlg := MAI.ARP;
      {$IFDEF VISUAL_BUILD}
      if MAI.IsLargeDataForML then
        RelTimeThread.SubTasksCheckCancel := MAI.ARP.AddSubTask('Initializing', 0).CheckCancelFunc;
      {$ENDIF}
      RelTimeThread.ShowProgress := true;
      RelTimeThread.OnTerminate := Terminator.OnThreadDone;
      MAI.ARP.Thread := RelTimeThread;

  	{$IFDEF VISUAL_BUILD}
      if MAI.MyNumThreadsToUse > 1 then
        MegaForm.MultithreadedCalcIsRunning := True
      else
        MegaForm.NumSingleThreadedMLAnalyses := MegaForm.NumSingleThreadedMLAnalyses + 1;
  	{$ENDIF}
      MAI := nil;
      RelTimeThread.Start;
    except
      on E:Exception do
      begin
      {$IFDEF VISUAL_BUILD}
        if Assigned(MAI) and Assigned(MAI.ARP) and MAI.ARP.Visible then
          MAI.ARP.Hide;
        ShowMessage('Application error when launching the Reltime analysis: ' + E.Message);
      {$ELSE}
        error_nv('Application error when launching the Reltime analysis: ' + E.Message, E);
      {$ENDIF}
      end;
    end;
  finally
    if Assigned(MAI) then { then probably validation of constraints failed}
    begin
      if Assigned(MAI.ARP) and MAI.ARP.Visible then
        MAI.ARP.Hide;
      MAI.MyProcessPack := nil;
      MAI.Free;
      SetLength(MinTimes, 0);
      SetLength(MaxTimes, 0);
    end;
  end;
end;

procedure AnalyzeUserTree(MAI: TAnalysisInfo; IsUpdateARP: Boolean);
var
  AMLThread: TMLTreeAnalyzeThread = nil;
  AMLThreadTerminator: TMLThreadTerminator = nil;
begin
  try
    {$IFDEF DEBUG} InitKTtime;{$ENDIF}
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AMLThreadTerminator := TMLThreadTerminator.Create;
    AMLThread := TMLTreeAnalyzeThread.Create(nil);
    AMLThread.Info := MAI;
    if MAI.MyTreePack.DoesContain(ttClock) or (MAI.MyUsrOperation = dtdoMLInferAncSeq) or (MAI.MyUsrOperation = dtdoMLInferAncSeqMyPeg) then
    begin
      MAI.GroupInfo.SetTreeOutgroupFromOtuInfos;
      AMLThread.GroupNames := MAI.GroupInfo.GroupNames;
    end;
    AMLThread.OptimizeBLens := true;  {todo -oKumar: Optimize BLens Should be selectable in Analysis Preference}
    AMLThread.OptimizeParams := true;
    AMLThread.ProgressDlg := MAI.ARP;
    {$IFDEF VISUAL_BUILD}
    if MAI.IsLargeDataForML  then
      AMLThread.SubTasksCheckCancel := MAI.ARP.AddSubTask('Initializing', 0).CheckCancelFunc;
    {$ENDIF}
    AMLThread.ShowProgress := true;
    AMLThread.OnTerminate := AMLThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AMLThread;
    AMLThread.Start;
    AMLThread := nil;
    AMLThreadTerminator := nil;
  finally
    if AMLThreadTerminator <> nil then AMLThreadTerminator.Free;
    if AMLThread <> nil then AMLThread.Free;
  end;
end;

procedure TestModels(MAI: TAnalysisInfo; IsUpdateARP: Boolean);
var
  AMLThread: TModelTestThread = nil;
begin
  if MAI.UseSmartFiltersForModelTest then
  begin
    TestModelsAdaptively(MAI);
    Exit;
  end;

  try
    if not MAI.ARP.Visible then
      MAI.ARP.Show;
    AMLThread := TModelTestThread.Create(nil, MAI.MyNumThreadsToUse);
    AMLThread.Info := MAI;
    if MAI.isAminoAcid then
      AMLThread.ProgressMessage := 'Testing protein models'
    else
      AMLThread.ProgressMessage := 'Testing DNA models';
    AMLThread.ProgressDlg := MAI.ARP;
    AMLThread.ShowProgress := True;
    {$IFDEF VISUAL_BUILD}
    AMLThread.OnTerminate := MegaForm.ModelTestDone;
    {$ELSE}
    AMLThread.OnTerminate := D_MegaMain.ModelTestDone;
    {$ENDIF}
    MAI.ARP.Thread := AMLThread;
    AMLThread.Start;
    AMLThread := nil;
  finally
    if Assigned(AMLThread) then
      AMLThread.Free;
  end;
end;

procedure ComputeEpML(MAI: TAnalysisInfo);
var
  aThread: TEpThread = nil;
  aTerminator: TEpThreadTerminator = nil;
  {$IFDEF VISUAL_BUILD}
  Response: Integer = 0;
  {$ENDIF}
begin
  if (not MAI.OutgroupDefined) and (not MAI.KeepUserTreeTimes) then
  begin
    {$IFDEF VISUAL_BUILD}
    PromptForOutgroup(MAI, Response);
    if Response <> mrOk then
    begin
      if Assigned(MAI.ARP) and MAI.ARP.Visible then
        MAI.ARP.Hide;
      ShowMessage('EP calculation requires that an outgroup is defined but it is not. Aborting analysis.');
      MAI.MyProcessPack := nil;
      MAI.Free;
      Exit;
    end;
    {$ELSE}
    raise Exception.Create('EP calculation requires that an outgroup is defined. You can provide this information in a separate text file or in a .meg formatted sequence alignment file');
    {$ENDIF}
  end;
  if MAI.MyNoOfSites < 1 then
    RaiseErrorMessage(HC_No_Common_Sites, 'No common sites found. ' + LineEnding + NoCommonSitesStr);
  if MAI.MyNoOfSeqs < 3 then
    RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least three sequences are needed for likelihood EP analysis');

  MAI.MyOriTreeList := TTreeList.Create;
  if not MAI.MyOriTreeList.ImportFromNewick(MAI.MyUserNewickTree, MAI.MyOtuNames) then
    raise exception.Create('failed to import newick file: ' + MAI.MyOriTreeList.ErrorMsg);
  if not MAI.KeepUserTreeTimes then
    MAI.GroupInfo.SetTreeOutgroupFromOtuInfos;
  if not MAI.IngroupDefined then
    raise Exception.Create('An ingroup must be defined for the EP calculation');
  if not MAI.KeepUserTreeTimes then
  begin
    RootOnOutgroup(MAI);
    MAI.MyOriTreeList.isRooted := True;
  end;

  MAI.ARP.UpdateRunStatusInfo('Status', 'Initializing EP Calculation');
  if MAI.ARP.Visible then
    MAI.ARP.Refresh;
  aThread := TEpThread.Create(MAI);
  aThread.RunStatusProc := MAI.ARP.UpdateRunStatusInfo;
  aThread.StopCodonSitesChangedToMissingData := D_InputSeqData.GetStopCodonsSitesConvertedToMissingData;
  MAI.CodonsTreatedAsMissingData := Length(aThread.StopCodonSitesChangedToMissingData) > 0;
  {$IFDEF VISUAL_BUILD}
  if MAI.IsLargeDataForML then
    aThread.SubTasksCheckCancel := MAI.ARP.AddSubTask('Optimizing Branch Lengths', 0).CheckCancelFunc;
  {$ENDIF}
  aThread.CheckCancelFunc := MAI.ARP.ProgressCheckCancel;
  aTerminator := TEpThreadTerminator.Create;
  aThread.OnTerminate := aTerminator.OnThreadDone;
  MAI.ARP.Thread := aThread;
  aThread.Start;
end;

procedure ComputeTree(MAI: TAnalysisInfo; IsUpdateARP: Boolean);
var
  AMLThread: TMLTreeSearchThread;
  AMLThreadTerminator: TMLThreadTerminator;
begin
  AMLThread := nil;
  AMLThreadTerminator := nil;

  try
    AMLThreadTerminator := TMLThreadTerminator.Create;
    AMLThread := TMLTreeSearchThread.Create(nil);
    AMLThread.ProgressDlg := MAI.ARP;
    AMLThread.Info := MAI;
    {$IFNDEF VISUAL_BUILD}
    if D_MegaMain.ExportSearchTrees then
    begin
      AMLThread.OtuNames := MAI.MyOtuNames;
      AMLThread.DoExportSearchTrees := True;
    end;
    {$ENDIF}

    {$IFDEF VISUAL_BUILD}
    if MAI.IsLargeDataForML  then
      AMLThread.SubTasksCheckCancel := MAI.ARP.AddSubTask('Initializing', 0).CheckCancelFunc;
    {$ENDIF}
    MAI.ARP.UpdateRunStatusInfo('Status', 'Computing tree');
    if MAI.ARP.Visible then
      MAI.ARP.Refresh;
    Application.ProcessMessages;
    AMLThread.ShowProgress := true;
    AMLThread.OnTerminate := AMLThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AMLThread;
    AMLThread.Start;
    AMLThread := nil;
    AMLThreadTerminator := nil;
  finally
    if AMLThreadTerminator <> nil then AMLThreadTerminator.Free;
    if AMLThread <> nil then AMLThread.Free;
  end;
end;

procedure ComputeBootstrapTree(MAI: TAnalysisInfo);
var
  AMLBootThread: TBootstrapMLThread = nil;
  AMLThreadTerminator: TMLThreadTerminator = nil;
begin
  try
    AMLThreadTerminator := TMLThreadTerminator.Create;
    AMLBootThread := TBootstrapMLThread.Create(nil);
    if MAI.UseAdaptiveRepsPerSample then
    begin
      AMLBootThread.IsAdaptiveBootstrap := True;
      AMLBootThread.NoOfReplication := MIN_ADAPTIVE_BOOTSTRAP_REPS;
    end
    else
      AMLBootThread.NoOfReplication := MAI.MyBootReps;
    AMLBootThread.BootstrapTrees := MAI.MyBootPartitionList;

    AMLBootThread.NoOfThreads := MAI.MyNumThreadsToUse;
    AMLBootThread.ProgressDlg := MAI.ARP;
    {$IFDEF VISUAL_BUILD}
    if MAI.IsLargeDataForML  then
      AMLBootThread.SubTasksCheckCancel := MAI.ARP.AddSubTask('Initializing', 0).CheckCancelFunc;
    {$ENDIF}
    AMLBootThread.ShowProgress := true;
    AMLBootThread.OnTerminate := AMLThreadTerminator.OnStandardBootstrapThreadDone;
    MAI.ARP.Thread := AMLBootThread;
    AMLBootThread.Start;
    AMLBootThread := nil;
    AMLThreadTerminator := nil;
  finally
    if AMLThreadTerminator <> nil then AMLThreadTerminator.Free;
    if AMLBootThread <> nil then AMLBootThread.Free;
  end;
end;


end.
