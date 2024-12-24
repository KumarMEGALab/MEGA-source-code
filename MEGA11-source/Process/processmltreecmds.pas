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

unit ProcessMLTreeCmds;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, MOtuInfo, MAnalysisInfo, MDistPack, MLTreeAnalyzer, MLModels,
  MProcessPack, ProcessTreeCmds, MCalibrationData, MegaConsts,
  MSubstitutionModelUtils;

procedure ProcessSeqMLPhylogenyCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack);
procedure ProcessSeqMLRelTimeCCCommand(UsrOperation: TDistTreeDlgOption; ProcessPack: TProcessPack);
procedure ProcessSeqMLRelTimeProtoCommand(UsrOperation: TDistTreeDlgOption; ProcessPack: TProcessPack);
procedure EstimateRelativeDivTimes(MAI: TAnalysisInfo);
procedure EstimateAnchoredDivtimes(MAI: TAnalysisInfo; MinTimes, MaxTimes: TDivTimesArray); overload;
procedure EstimateAnchoredDivTimes(MAI: TAnalysisInfo; MinTimes, MaxTimes, SamplingTimes: TDivTimesArray); overload;
procedure EstimateSampledDivTimes(MAI: TAnalysisInfo; SamplingTimes: TDivTimesArray);
procedure RunCorrTestML(MAI: TAnalysisInfo);
procedure ComputeEpML(MAI: TAnalysisInfo);
function CheckAbortIfMemoryUsageHigh(MAI: TAnalysisInfo): Boolean;
function CheckAbortForIdenticalSeqsML(aInfo: TAnalysisInfo): Boolean;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  MutationDetailView, Mega_Main, MTaxaGpsDlg,
  {$ELSE}
  MD_MegaMain, MegaUtils_NV,
  {$ENDIF}
  LCLIntf, LCLType,
  SysUtils, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons,  MegaErrUtils,
  MNucDist, MD_InputSeqData,  MRuntimeProgressDlg,
  MTreeData, MTreeList, MPartitionList, MTreePack, MLSearchThread, ShowTrees,
  MLTree, ErrorMessages_HC, MLegendGenerator, mreltimethreads, stringutils,
  MTreeDataAdapter, MPartitionData, mmemutils, mepthreads, mcheck_abort_reltime;

type

  { TMLThreadTerminator }

  TMLThreadTerminator = class(TObject)
  public
    procedure OnThreadDone(Thread: TObject);
  end;

procedure ComputeTree(MAI: TAnalysisInfo; IsUpdateARP: Boolean); forward;
procedure ComputeBootstrapTree(MAI: TAnalysisInfo); forward;
procedure AnalyzeUserTree(MAI: TAnalysisInfo; IsUpdateARP: Boolean); forward;
procedure TestModels(MAI: TAnalysisInfo; IsUpdateARP: Boolean); forward;
procedure TestMolecularClock(MAI: TAnalysisInfo; IsUpdateARP: Boolean); forward;
procedure PromptForOutgroup(AnalysisInfo: TAnalysisInfo; var Response: Integer); forward;

procedure TMLThreadTerminator.OnThreadDone(Thread: TObject);
var
  MAI: TAnalysisInfo = nil;
  CurrentAnalysisClassName: String;
  MyExceptionName: String;
  MyExceptionMessage: String;
  isMLAnalysis: Boolean;
begin

  try
    try
       MAI := (Thread as TMLTreeThread).ProgressDlg.FMAI;
      if Assigned(MAI.WorkflowElementFinishedCallBack) then
      begin
        MAI.WorkflowElementFinishedCallBack(MAI);
        MAI := nil;
      end
      else
      begin
        CurrentAnalysisClassName := Thread.ClassName;
        isMLAnalysis := (CurrentAnalysisClassName = 'TMLTreeSearchThread') or
           (CurrentAnalysisClassName = 'TBootstrapMLThread') or
           (CurrentAnalysisClassName = 'TMLTreeAnalyzeThread') or
           (CurrentAnalysisClassName = 'TModelTestThread') or
           (CurrentAnalysisClassName = 'TMLClockTestThread');
        Assert(isMLAnalysis);
        MyExceptionName := TMLTreeThread(Thread).MyExceptionName;
        if MyExceptionName <> 'none' then
        begin
          {$IFNDEF VISUAL_BUILD}
            D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
          {$ELSE}
            MAI.AnalysisSummary.AddAnalysisInfo(MAI);
          {$ENDIF}
          if MyExceptionName = 'EOutOfMemory' then
          begin
            MyExceptionMessage := TMLTreeThread(Thread).MyExceptionMessage;
            ReportThreadCrash(MyExceptionName, MyExceptionMessage);
          end
          else // then MadExcept already took care of it in MLTreeThread.Execute but we cannot continue
          begin
            {$IFNDEF VISUAL_BUILD}
            error_nv('Error: ' + TMLTreeThread(Thread).MyExceptionMessage);
            {$ELSE}
            MyExceptionMessage := TMLTreeThread(Thread).MyExceptionMessage;
            raise Exception.Create(MyExceptionMessage);
            {$ENDIF}
          end;
        end
        else  // the thread terminated as expected so display the results
        begin
          if not (Thread as TMLTreeThread).Canceled then
          begin
            if Assigned(MAI.MyMLAnalysisPack) then
            begin
              MAI.MyMLAnalysisPack.SubTaskCheckCancel := nil;
              if Assigned(MAI.MyMLAnalysisPack.MLTree) then
                MAI.MyMLAnalysisPack.MLTree.SubtaskCheckCancel := nil;
            end;

            {$IFDEF VISUAL_BUILD}
            MAI.ARP.UpdateRunStatusInfo('Status', 'Preparing display');
            MAI.ARP.Refresh;
            {$ELSE}
            if not (MAI.MyTreePack.DoesContain(ttClock)) and (MAI.MyTreePack.DoesContain(ttUserTree)) then
              MAI.ARP.UpdateRunStatusInfo('Status', 'Preparing output');
            {$ENDIF}

            {todo -oKumar: we need to make te threadDone sensitive to all different types of options}
            {todo -oKumar: optimized bLen and specify clock -> ttClock and ttOptimizePreBLens}
            if MAI.MyTreePack.DoesContain(ttInferTree) then
            begin
              if MAI.MyTreePack.DoesContain(ttBootstrap) then
              begin
                if MAI.MyOriTreeList.Count = 0 then
                  MAI.MyOriTreeList.Add(TTreeData.Create(MAI.MyNoOfSeqs, true, false, true));
                MAI.MyOriTreeList[0].isStats := true;
                MAI.MyMLAnalysisPack.GetTreeData(MAI.MyOriTreeList[0]);
                MAI.MyBootPartitionList.Compare(MAI.MyOriTreeList[0].NodeArray, PArrayOfDouble(@MAI.MyOriTreeList[0].StatsArray[MAI.MyOriTreeList[0].NoOfOTUs]));
                MAI.MyValidReps := Round(MAI.MyBootPartitionList.TotalFrequency);
                ShowMLBootTree(MAI);
              end
              else
                ShowMLTree(MAI);
            end
            else if (MAI.MyTreePack.DoesContain(ttClock)) and (MAI.MyTreePack.DoesContain(ttUserTree)) then
            begin
              if MAI.MyProcessPack.ContainsProcessType(ppMLClock) then // need to check this because analyze user tree can have both ttClock and ttUserTree if assume molecular clock = true
                ShowClockTestResults(MAI)
              else
                ShowMLTree(MAI);
            end
            else if  MAI.MyTreePack.DoesContain(ttModelTest) then
            begin
              if not (Thread as TMLTreeThread).MLTreeAnalyzer.Cancelled then
                ShowModelTestResults(MAI);
            end
            else if MAI.MyTreePack.DoesContain(ttAncState) or MAI.MyTreePack.DoesContain(ttBLens) then
              ShowMLTree(MAI)
            else if MAI.MyTreePack.DoesContain(ttClockTest) then
            begin
              if MAI.InitialUsrOperation = dtdoMLClockTest then
              begin
                if (Thread as TMLClockTestThread).IsSuccess then
                  ShowClockTestResults(MAI) // otherwise, we already showed the user a failure message
                else
                begin
                  try
                    FreeAndNil(MAI.MyMLAnalysisPack);
                    MAI.ARP.Free;
                    MAI.MyProcessPack := nil;
                    MAI.Free;
                  except
                    // do nothing
                  end;
                end;
              end;
            end
            else if MAI.MyTreePack.DoesContain(ttPattern) or
                    MAI.MyTreePack.DoesContain(ttTsTvBias) or
                    MAI.MyTreePack.DoesContain(ttGamma) then
              ShowPatternResults(MAI)
            else if MAI.MyTreePack.DoesContain(ttSiteRates) then
              ShowSiteRateResults(MAI);

            MAI := nil;
          end
          else
            begin
              if Thread.ClassNameIs('TBootstrapMLThread') then
              begin
                if MAI.MyOriTreeList.Count = 0 then
                  MAI.MyOriTreeList.Add(TTreeData.Create(MAI.MyNoOfSeqs, true, false, true));
                MAI.MyOriTreeList[0].isStats := true;
                MAI.MyMLAnalysisPack.GetTreeData(MAI.MyOriTreeList[0]);
                MAI.MyBootPartitionList.Compare(MAI.MyOriTreeList[0].NodeArray, PArrayOfDouble(@MAI.MyOriTreeList[0].StatsArray[MAI.MyOriTreeList[0].NoOfOTUs]));
                MAI.MyValidReps := Round(MAI.MyBootPartitionList.TotalFrequency);
                if ShowMLBootTree(MAI) then
                  (Thread as TBootstrapMLThread).Canceled := False;
                MAI := nil;
              end
              else
                if Assigned(MAI) then
                  MAI.MyMLAnalysisPack := nil // will be free by TMLTreeThread
            end;
        end;
      end;
    except
      on E:Exception do
      begin
        MAI := nil; { freed in ShowTrees}
        {$IFDEF VISUAL_BUILD}
        (Thread as TMLTreeSearchThread).SynchronizeErrorMessage(E);
        {$ELSE}
        error_NV('Oh no! An error occurred when finalizing the analysis', E);
        {$ENDIF}
      end;
    end;
  finally
    {$IFDEF VISUAL_BUILD}
    if MegaForm.MultithreadedCalcIsRunning = True then
      MegaForm.MultithreadedCalcIsRunning := False
    else
      MegaForm.NumSingleThreadedMLAnalyses := MegaForm.NumSingleThreadedMLAnalyses - 1;

    if (Thread.ClassName = 'TMLTreeThread') and (Thread as TMLTreeThread).Canceled then
    begin
      if MAI.IsMyPegAnalysis then
      begin
        MutationDetailViewForm.AncMLAction.Enabled := True;
        MutationDetailViewForm.AncParsimonyAction.Enabled := True;
      end;
    end;
    {$ENDIF}
    if MAI <> nil then
    begin
      if MAI.ARP <> nil then
        MAI.ARP.Free;
      MAI.MyProcessPack := nil;
      MAI.Free;
    end;

    // GS - we should check for FreeOnTerminate below because we have gone back
    // and forth with setting it to either true or false (see MLSearchThread.pas).
    // This way, if we ever change it again, this code block will handle it correctly.
    if not (Thread as TMLTreeThread).FreeOnTerminate then
    begin
      (Thread as TMLTreeThread).MLTreeAnalyzer.CheckCancel := nil;   // Thread should not be freed on terminate to execute this line
      (Thread as TMLTreeThread).MLTreeAnalyzer := nil;               // Thread should not be freed on terminate to execute this line
      (Thread as TMLTreeThread).FreeOnTerminate := True;
      (Thread as TMLTreeThread).Terminate;
    end;
  end;
end;


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

  try try
    MAI := TAnalysisInfo.Create;
    MAI.MyProcessPack := ProcessPack;
    MAI.PropagateConstraints := (not ProcessPack.HasProcessType(ppSkipMinMax));
    MAI.InitialUsrOperation := UsrOperation;
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
      Exit;

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
//    D_InputSeqData.PrepareDataForPartitionedMLAnalysis(MAI.MySubsetOpt, MAI.MySeqPartitions, MAI.MyUsedOtuInfos,
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
    D_InputSeqData.PrepareDataForMLAnalysis(MAI);
    MAI.ARP.AddAnalysisOptions('No. of Seqs', IntToStr(MAI.MyNoOfSeqs));
    MAI.ARP.AddAnalysisOptions('No. of Sites', IntToStr(MAI.MyNoOfSites));

    if not CheckAbortIfMemoryUsageHigh(MAI) then
    begin
      if MAI.MyNoOfSites < 1 then
        RaiseErrorMessage(HC_No_Common_Sites, 'No common sites found. ' + LineEnding + NoCommonSitesStr);
      if MAI.MyNoOfSeqs < 3 then
        RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least three sequences are needed for likelihood analysis');
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
      if UsrOperation = dtdoEpML then
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
        if MAI.IsBootstrapSubSampling then
        begin
          MAI.MyBootReps := MAI.MyTreePack.BootReps;
          MAI.MyBootPartitionList := TPartitionList.Create(MAI.MyNoOfSeqs, 0, false);
          ComputeLittleBootstrapTree(MAI);
        end
        else
        begin
          if MAI.MyTreePack.DoesContain(ttClock) or (MAI.MyUsrOperation = dtdoMLInferAncSeq) then
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
                if MAI.MyUsrOperation = dtdoMLInferAncSeq then
                  ShowMessage('Ancestral sequence inference requires that an outgroup be defined but it is not. Aborting the analysis')
                else
                  ShowMessage('Molecular clock test requires that an outgroup is defined but it is not. Aborting the analysis.');
                Exit;
              end;
              {$ELSE}
              if MAI.MyUsrOperation = dtdoMLInferAncSeq then
              begin
                if MAI.UserInputTreeIsRooted then
                  warn_nv('assuming smaller descendant clade of root node is the outgroup. Please check results carefully.')
                else
                  error_nv('an outgroup must be defined for ancestral sequence inference');
              end;
              {$ENDIF}
            end;
          end;
          AnalyzeUserTree(MAI, True);
        end;
      end
      else if MAI.MyTreePack.DoesContain(ttPattern) or
              MAI.MyTreePack.DoesContain(ttTsTvBias) or
              MAI.MyTreePack.DoesContain(ttSiteRates) or
              MAI.MyTreePack.DoesContain(ttGamma) then
      begin
        ComputeTree(MAI, True);
      end;
      MAI := nil;
    end;
  except
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
        FreeAndNil(MAI.ARP);
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
//    D_InputSeqData.PrepareDataForPartitionedMLAnalysis(AnalysisInfo.MySubsetOpt, AnalysisInfo.MySeqPartitions, AnalysisInfo.MyUsedOtuInfos,
//                                            AnalysisInfo.MyNoOfSeqs, AnalysisInfo.MyNoOfSites,  AnalysisInfo.MyLabelsUsed, AnalysisInfo.RecodeScheme,AnalysisInfo.SiteCoverage);
    D_InputSeqData.ARP := Progress;
    D_InputSeqData.PrepareDataForMLAnalysis(AnalysisInfo);
    {$IFNDEF VISUAL_BUILD}
    CheckAbortForIdenticalSeqsML(AnalysisInfo);
    {$ENDIF}
    AnalysisInfo.ARP.AddAnalysisOptions('No. of Seqs', IntToStr(AnalysisInfo.MyNoOfSeqs));
    AnalysisInfo.ARP.AddAnalysisOptions('No. of Sites', IntToStr(AnalysisInfo.MyNoOfSites));
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

    if AnalysisInfo.MLBLensFilter > 0 then
        AMLTreeAnalyzer.BLenFilter := AnalysisInfo.MLBLensFilter;

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

    if AnalysisInfo.MyProcessPack.CalibrationFile <> EmptyStr then
      CalibrationFile := AnalysisInfo.MyProcessPack.CalibrationFile;
    if CalibrationFile <> EmptyStr then
    begin
      AnalysisInfo.CalibrationFile := CalibrationFile;
      Calibrations := TCalibrations.Create;
      Calibrations.SetInfo(AnalysisInfo);
      if not Calibrations.LoadFromFile(CalibrationFile) then
        raise Exception.Create('Failed to load calibrations');
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
        FreeAndNil(AnalysisInfo.ARP);
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

procedure TestMolecularClock(MAI: TAnalysisInfo; IsUpdateARP: Boolean);
var
  ClockTestThread: TMLClockTestThread;
  Terminator: TMLThreadTerminator;
  Response: Integer = -1;
begin
  ClockTestThread := nil;
  Terminator := nil;

  try
    if not MAI.OutgroupDefined then
    begin
      {$IFDEF VISUAL_BUILD}
      PromptForOutgroup(MAI, Response);
      if Response <> mrOk then
      begin
        ShowMessage('Molecular clock test requires that an outgroup is defined but it is not. Aborting analysis.');
        Exit;
      end;
      {$ELSE}
      raise Exception.Create('Molecular clock test requires that an outgroup is defined. You can provide this information in a separate text file or in a .meg formatted sequence alignment file');
      {$ENDIF}
    end;
    {$IFNDEF VISUAL_BUILD}
    MAI.SetupOutgroupMembers;
    {$ENDIF}
    ClockTestThread := TMLClockTestThread.Create(nil);
    ClockTestThread.AnalysisInfo := MAI;
    Terminator := TMLThreadTerminator.Create;
    ClockTestThread.OptimizeParams := true;
    ClockTestThread.ProgressDlg := MAI.ARP;
    {$IFDEF VISUAL_BUILD}
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
    CorrTestThread.AnalysisInfo := MAI;
    Terminator := TRelTimeTerminator.Create(MAI.InitialUsrOperation);
    CorrTestThread.OptimizeParams := true;
    CorrTestThread.ProgressDlg := MAI.ARP;
    {$IFDEF VISUAL_BUILD}
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
      ShowMessage('Oh no! An error has occurred: ' + E.Message)
      {$ELSE}
      warn_NV('An error has occurred: ' + E.Message);
      {$ENDIF}
  end;
end;


function CheckAbortIfMemoryUsageHigh(MAI: TAnalysisInfo): Boolean;
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
  Result := False;
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
        msgString := Format('The analysis will use ~%.1f GB of system RAM and only ~%.1f GB are available so this may freeze your system. Continue anyway?', [totalBytes/(1024*1024*1024), memAvailable/(1024*1024*1024)])
      else
        msgString := Format('The analysis will use ~%.1f GB of system RAM and only ~%.1f GB are available so system performance may suffer. Continue anyway?', [totalBytes/(1024*1024*1024), memAvailable/(1024*1024*1024)]);;
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
    if (AnalysisInfo.MyUsrOperation = dtdoMLClockTest) or (AnalysisInfo.MyUsrOperation = dtdoEpML) or (AnalysisInfo.MyUsrOperation = dtdoMLComputeUserTreeBLens) then
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
    Response := MessageDlg('Clock test requires that you specify an outgroup. Do you want to continue?', mtCustom, mbOKCancel, 0)
  else if AnalysisInfo.MyUsrOperation = dtdoEpML then
    Response := MessageDlg('EP calculation requires that you specify an outgroup. Do you want to continue?', mtCustom, mbOKCancel, 0)
  else if AnalysisInfo.MyUsrOperation = dtdoMLComputeUserTreeBLens then
    Response := MessageDlg('Assuming a molecular clock for user tree analysis requires that you specify an outgroup. Do you want to continue?', mtCustom, mbOkCancel, 0)
  else if AnalysisInfo.MyUsrOperation = dtdoMLInferAncSeq then
    Response := MessageDlg('Ancestral sequence inference analysis requires that you specify an outgroup for rooting the input tree. Do you want to continue?', mtCustom, mbOkCancel, 0)
  else
    raise Exception.Create('invalid user operation');
  if Response <> mrOk then
  begin
    ResetMLCalcThreadInfo;
    Exit;
  end;
  TaxaGpsDlg.fillGroupTreeViewFromOtuInfo(True);
  TaxaGpsDlg.JumpToOutgroupPage;
  TaxaGpsDlg.ShowingModal := True;
  MResult := TaxaGpsDlg.ShowModal;
  if MResult = mrOk then
  begin
    TaxaGpsDlg.JumpToTaxaPage; { reset to default page in case the user opens it again later}
    if not D_InputSeqData.OtuInfos.OutgroupIsDefined then
    begin
      Response := mrNo;
    end
    else
    begin
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
    end;
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
    RelTimeThread.MergeRates := MAI.MergeRates;
    RelTimeThread.AnalysisInfo := MAI;
    Terminator := TRelTimeTerminator.Create(MAI.InitialUsrOperation);
    RelTimeThread.OptimizeParams := true;
    RelTimeThread.ProgressDlg := MAI.ARP;
    {$IFDEF VISUAL_BUILD}
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
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Oh no! An error has occurred: ' + E.Message)
      {$ELSE}
      warn_NV('An error has occurred: ' + E.Message);
      {$ENDIF}
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
      if not MAI.CalibrationTimes.DoAllValidations(ErrorMsg, False) then
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
begin
  RelTimeThread := nil;
  Terminator := nil;
  ErrorMsg := EmptyStr;

  try
    try
      MAI.MyOriTreeList[0].isSE := True;
      if not MAI.MyOriTreeList[0].isBLen then
        MAI.MyOriTreeList.isBLen := True;
      if not MAI.CalibrationTimes.DoAllValidations(ErrorMsg, False) then
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
      MAI.CalibrationTimes.PrepareDivTimeArrays(MinTimes, MaxTimes, MAI.NoOfSeqs, False); { at first this appears redundant but it is needed so that the original min/max times are included in the tabular output file}
      RelTimeThread := TAnchoredRelTimeMLThread.Create(nil);
      RelTimeThread.MinTimes := MinTimes;
      RelTimeThread.MaxTimes := MaxTimes;
      RelTimeThread.MergeRates := MAI.MergeRates;
      RelTimeThread.AnalysisInfo := MAI;
      Terminator := TRelTimeTerminator.Create(MAI.InitialUsrOperation);
      RelTimeThread.OptimizeParams := true;
      RelTimeThread.ProgressDlg := MAI.ARP;
      {$IFDEF VISUAL_BUILD}
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
      //RelTimeThread.MLTreeAnalyzer.NoOfThreadsToUse := MAI.MyNumThreadsToUse;
      MAI := nil;
      RelTimeThread.Start;
    except
      on E:Exception do
      {$IFDEF VISUAL_BUILD}
        ShowMessage('Oh no! An error has occurred' + E.Message);
      {$ELSE}
        error_nv('An error occurred when launching the analysis: ' + E.Message, E);
      {$ENDIF}
    end;
  finally
    if Assigned(MAI) then { then probably validation of constraints failed}
    begin
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
    if not MAI.ARP.Visible then MAI.ARP.Show;

    AMLThreadTerminator := TMLThreadTerminator.Create;
    AMLThread := TMLTreeAnalyzeThread.Create(nil);
    if MAI.MyTreePack.DoesContain(ttClock) or (MAI.MyUsrOperation = dtdoMLInferAncSeq) or (MAI.MyUsrOperation = dtdoMLInferAncSeqMyPeg) then
    begin
      MAI.GroupInfo.SetTreeOutgroupFromOtuInfos;
      AMLThread.GroupNames := MAI.GroupInfo.GroupNames;
    end;
    AMLThread.OptimizeBLens := true;  {todo -oKumar: Optimize BLens Should be selectable in Analysis Preference}
    AMLThread.OptimizeParams := true;
    AMLThread.ProgressDlg := MAI.ARP;
    {$IFDEF VISUAL_BUILD}
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
  AMLThread: TModelTestThread;
  AMLThreadTerminator: TMLThreadTerminator;
begin
  AMLThread := nil;
  AMLThreadTerminator := nil;

  try
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AMLThreadTerminator := TMLThreadTerminator.Create;
    AMLThread := TModelTestThread.Create(nil, MAI.MyNumThreadsToUse);
    AMLThread.ProgressDlg := MAI.ARP;
    {$IFDEF VISUAL_BUILD}
    //AMLThread.SubTasksCheckCancel := MAI.ARP.AddSubTask('Initializing', 0).CheckCancelFunc;
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

procedure ComputeEpML(MAI: TAnalysisInfo);
var
  aThread: TEpThread = nil;
  aTerminator: TEpThreadTerminator = nil;
  {$IFDEF VISUAL_BUILD}
  Response: Integer = 0;
  {$ENDIF}
begin
  if not MAI.OutgroupDefined then
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
  MAI.MyOriTreeList.ImportFromNewick(MAI.MyUserNewickTree, MAI.MyOtuNames);
  MAI.GroupInfo.SetTreeOutgroupFromOtuInfos;
  if not MAI.IngroupDefined then
    raise Exception.Create('An ingroup must be defined for the EP calculation');
  RootOnOutgroup(MAI);
  MAI.MyOriTreeList.isRooted := True;
  MAI.ARP.UpdateRunStatusInfo('Status', 'Initializing EP Calculation');
  if MAI.ARP.Visible then
    MAI.ARP.Refresh;
  aThread := TEpThread.Create(MAI);
  {$IFDEF VISUAL_BUILD}
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
    {$IFDEF VISUAL_BUILD}
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
  AMLBootThread: TBootstrapMLThread;
  AMLThreadTerminator: TMLThreadTerminator;
begin
  AMLBootThread := nil;
  AMLThreadTerminator := nil;

  try
    AMLThreadTerminator := TMLThreadTerminator.Create;
    AMLBootThread := TBootstrapMLThread.Create(nil);
    AMLBootThread.BootstrapTrees := MAI.MyBootPartitionList;
    AMLBootThread.NoOfReplication := MAI.MyBootReps;
    AMLBootThread.NoOfThreads := MAI.MyNumThreadsToUse;
    AMLBootThread.ProgressDlg := MAI.ARP;
    {$IFDEF VISUAL_BUILD}
    AMLBootThread.SubTasksCheckCancel := MAI.ARP.AddSubTask('Initializing', 0).CheckCancelFunc;
    {$ENDIF}
    AMLBootThread.ShowProgress := true;
    AMLBootThread.OnTerminate := AMLThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AMLBootThread;
    AMLBootThread.Start;
    AMLBootThread := nil;
    AMLThreadTerminator := nil;
  finally
    if AMLThreadTerminator <> nil then AMLThreadTerminator.Free; // normally frees itself
    if AMLBootThread <> nil then AMLBootThread.Free; // normally freed by MLThreadTerminator
  end;
end;


end.
