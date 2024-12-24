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

unit ProcessParsimTreeCmds;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, MOtuInfo, MAnalysisInfo, MDistPack, MProcessPack, MPTree;

procedure ProcessSeqParsimPhylogenyCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack);
procedure PromptForOutgroup(AnalysisInfo: TAnalysisInfo; var Response: Integer); deprecated; { we no longer require that input trees for ancestral sequence analaysis are rooted}

implementation

uses
  {$IFDEF VISUAL_BUILD}
  Mega_Main, MutationDetailView, MTaxaGpsDlg,
  {$ELSE}
  MD_MegaMain, MegaUtils_NV,
  {$ENDIF}
  LCLIntf, LCLType, SysUtils, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Math, ComCtrls, mcrossprojectutils, MTreeData,
  MTreeList, MPartitionList, MegaUtils, MegaConsts, MegaErrUtils, ErrorMessages_HC,
  MD_InputSeqData, MRuntimeProgressDlg, MTreePack, ParsimSearchThreads, ProcessTreeCmds,
  MTreeSearchThread, ShowTrees, MTreeDataAdapter, msitecoverage, MegaAnalysisPrefStrings;

type
  TParsimThreadTerminator = class(TObject)
  public
    procedure OnThreadDone(Thread: TObject);
  end;

function ComputeTree(MAI: TAnalysisInfo; IsUpdateARP: Boolean): Boolean; forward;
procedure BranchBoundSearchDNA(  MAI: TAnalysisInfo); forward;
procedure BranchBoundSearchAmino(MAI: TAnalysisInfo); forward;
procedure HeuristicDtSearchDNA(  MAI: TAnalysisInfo); forward;
procedure HeuristicDtSearchAmino(MAI: TAnalysisInfo); forward;
procedure MinMiniSearchDNA(      MAI: TAnalysisInfo); forward;
procedure MinMiniSearchAmino(    MAI: TAnalysisInfo); forward;
procedure SPRSearchDNA(      MAI: TAnalysisInfo); forward;
procedure SPRSearchAmino(    MAI: TAnalysisInfo); forward;
procedure TBRSearchDNA(      MAI: TAnalysisInfo); forward;
procedure TBRSearchAmino(    MAI: TAnalysisInfo); forward;
procedure InferAncestralSeq(     MAI: TAnalysisInfo); forward;
procedure AnalyzeUserTree(       MAI: TAnalysisInfo); forward;
procedure ComputeBootstrapTree           (MAI: TAnalysisInfo); forward;
procedure BootstrapBranchBoundSearchDNA  (MAI: TAnalysisInfo); forward;
procedure BootstrapBranchBoundSearchAmino(MAI: TAnalysisInfo); forward;
procedure BootstrapHeuristicDtSearchDNA  (MAI: TAnalysisInfo); forward;
procedure BootstrapHeuristicDtSearchAmino(MAI: TAnalysisInfo); forward;
procedure BootstrapMinMiniSearchDNA      (MAI: TAnalysisInfo); forward;
procedure BootstrapMinMiniSearchAmino    (MAI: TAnalysisInfo); forward;
procedure BootstrapSPRSearchDNA(MAI: TAnalysisInfo); forward;
procedure BootstrapSPRSearchAmino(MAI: TAnalysisInfo); forward;
procedure BootstrapTBRSearchDNA(MAI: TAnalysisInfo); forward;
procedure BootstrapTBRSearchAmino(MAI: TAnalysisInfo); forward;

procedure TParsimThreadTerminator.OnThreadDone(Thread: TObject);
var
  t: TMPTreeSearchThread = nil;
  MAI: TAnalysisInfo;
  MyExceptionName: String;
  MyExceptionMessage: String;
  IgnoreCancel: Boolean; { for the case where bootstrapping was taking too long and the user cancelled but still wants partial results}
begin

  MAI := (Thread as TTreeSearchThread).ProgressDlg.FMAI;

  try
    if thread is TMPTreeSearchThread then
    begin
      t := TMPTreeSearchThread(Thread);
      {$IFDEF VISUAL_BUILD}
      MAI.AnalysisSummary.AddCalculatedValue(NUM_TREES_SEARCHED, IntToStr(t.NoOfTreesSearched))
      {$ELSE}
        D_MegaMain.AnalysisSummary.AddCalculatedValue(NUM_TREES_SEARCHED, IntToStr(t.NoOfTreesSearched));
      {$ENDIF}
    end;

    begin
      if Thread is TMPTreeSearchThread then
        MyExceptionName := TMPTreeSearchThread(Thread).MyExceptionName
      else
        MyExceptionName := TParsimSearchThread(Thread).MyExceptionName;
      if MyExceptionName <> 'none' then
      begin
        {$IFNDEF VISUAL_BUILD}
          D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
        {$ELSE}
        MAI.AnalysisSummary.AddAnalysisInfo(MAI);
        {$ENDIF}
        if MyExceptionName = 'EOutOfMemory' then
        begin
          MyExceptionMessage := TParsimSearchThread(Thread).MyExceptionMessage;
          ReportThreadCrash(MyExceptionName, MyExceptionMessage);
        end
        else // then MadExcept already took care of it in MPTreeThread.Execute but we cannot continue
        begin
          if MAI.IsMyPegAnalysis then
          begin
            {$IFDEF VISUAL_BUILD}
            MutationDetailViewForm.AncMLAction.Enabled := True;
            MutationDetailViewForm.AncParsimonyAction.Enabled := True;
            {$ENDIF}
          end;
          Exit;
        end;
      end
      else
      begin
        if (thread is TBootstrapMPTreeSearchThread) then
          IgnoreCancel := (thread as TBootstrapMPTreeSearchThread).PartiallyCompleted
        else
          IgnoreCancel := False;
        if (not (Thread as TTreeSearchThread).Canceled) or IgnoreCancel then
        begin
          {$IFDEF VISUAL_BUILD}
          MAI.ARP.UpdateRunStatusInfo('Status', 'Preparing display');
          MAI.ARP.Refresh;
          {$ELSE}
          MAI.ARP.UpdateRunStatusInfo('Status', 'Preparing output');
  	      {$ENDIF}
          if MAI.MyTreePack.DoesContain(ttBootstrap) then
          begin
            MAI.MyValidReps := Round(MAI.MyBootPartitionList.TotalFrequency);
            ShowParsimBootTree(MAI);
          end
          else
            ShowParsimTree(MAI);

          MAI := nil;
        end;
      end;
    end;
  finally
    {$IFDEF VISUAL_BUILD}
    if (Thread as TTreeSearchThread).Canceled then
    begin
      if Assigned(MAI) and MAI.IsMyPegAnalysis then
      begin
        MutationDetailViewForm.AncMLAction.Enabled := True;
        MutationDetailViewForm.AncParsimonyAction.Enabled := True;
      end;
    end;
    {$ENDIF}
    if MAI <> nil then
    begin
      if Assigned(MAI.ARP) then
        MAI.ARP.Free;
      MAI.MyProcessPack := nil;
      MAI.Free;
    end;
    Self.Free;
  end;
end;

//===
procedure ProcessSeqParsimPhylogenyCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack);
var
  ARP : TRuntimeProgress = nil;
  MAI : TAnalysisInfo = nil;
  i: Integer;
  isSuccess: Boolean = True;
begin
  try
    try
      MAI := TAnalysisInfo.Create;
      MAI.MyProcessPack := ProcessPack;
      MAI.InitialUsrOperation := UsrOperation;

      ARP := TRuntimeProgress.Create(Application);
      ARP.DataFileName:= GetActiveDataFileName;
      ARP.DataTitle := GetActiveDataTitle;
      MAI.DataFilename := GetActiveDataFilename;
      MAI.ARP := ARP;
      ARP.FMAI := MAI;

      if UsrOperation = dtdoMPInferAncSeqMyPeg then // so MAI will close the AnalysisPreferences dialog without the user seeing it (they cannot set any preferences)
        MAI.IsMyPegAnalysis := True;

      if not MAI.GetAnalysisOptions(UsrOperation) then
      begin
        if Assigned(MAI.MyProcessPack) and MAI.MyProcessPack.IsGettingWorkflowSettings then
          MAI := nil;
        Exit;
      end;

      if MAI.MyTreePack.DoesContain(ttBootstrap) then
        MAI.MyBootReps := MAI.MyTreePack.BootReps;

      ARP.WriteAnalysisOptionsToStdOut;
      ARP.AddRunStatusInfo('Status', 'Preparing data');
      ARP.Show;

      if UsrOperation = dtdoMPInferAncSeqMyPeg then // if the user has unselected any taxa we reselect them because we are using a fixed tree with all 46 species
      begin
        for i := 0 to D_InputSeqData.FOtuInfos.NoOfOtus - 1 do
          D_InputSeqData.FOtuInfos[D_InputSeqData.DispTaxaList[i]].IsUsed := true;
        MAI.MyPegSite := MAI.MyProcessPack.MyPegPeptideSite; // don't like doing it this way but this information is lost in myprocesspack once the analysis thread is done
      end;

    if MAI.MyTreePack.DoesContain(ttUserTree) then
    begin
      if UsrOperation = dtdoMPInferAncSeqMyPeg then
        MAI.GetUCSC_46_SpeciesTree
      else if not MAI.GetUserTreeInput(UsrOperation, MAI.MyUserTreeFile, MAI.ARP, nil, ttNJ, (UsrOperation = dtdoMPComputeUserTreeBLens) or (UsrOperation = dtdoMPInferAncSeq)) then
      begin
        Exit;
      end;
    end;

    MAI.MyInfoSites := TList.Create;
    D_InputSeqData.ARP := ARP;
    MAI.MyIncludedSites := D_InputSeqData.PrepareDataForParsimAnalysis(MAI.MySubsetOpt, MAI.MyInfoSites, MAI.MyUsedOtuInfos,
    MAI.MyNoOfSeqs, MAI.MyNoOfSites, MAI.MyNoOfInfoSites, MAI.MyConstContribute, MAI.MyLabelsUsed, MAI.RecodeScheme, MAI.SiteCoverage);

    if MAI.MyNoOfSites < 1 then
      RaiseErrorMessage(HC_No_Common_Sites, 'No common sites found. ' + NoCommonSitesStr);

    if MAI.MyNoOfInfoSites < 1 then
      RaiseErrorMessage(HC_No_Common_Sites, 'There are no parsimony informative sites');

    ARP.AddAnalysisOptions('No. of Sites', Format('%.0n', [MAI.MyNoOfSites*1.0]));

    if MAI.MyNoOfSeqs < 4 then
      RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least four taxa are needed for parsimony analysis');

    MAI.MyOriTreeList := TTreeList.Create;
    if MAI.MyTreePack.DoesContain(ttUserTree) then
    begin
      MAI.MyOriTreeList.ImportFromNewick(MAI.MyUserNewickTree, MAI.MyOtuNames);
    end;

    if MAI.MyTreePack.DoesContain(ttBootstrap) then
    begin
      MAI.MyBootPartitionList := TPartitionList.Create(MAI.MyNoOfSeqs, 0, false);
      ARP.UpdateRunStatusInfo('Status', 'Bootstrapping for MP trees');
      ComputeBootstrapTree(MAI);
    end
    else begin
      ARP.UpdateRunStatusInfo('Status', 'Searching for MP trees');
      isSuccess := ComputeTree(MAI, True);
    end;
    if isSuccess then
    begin
      ARP := nil;
      MAI := nil;
    end;
  except
    on E: Exception do
     ShowErrorMessage(E);
  end;
  finally
    if Assigned(MAI) and Assigned(MAI.ARP) then
    begin
      MAI.MyProcessPack := nil;
      MAI.ARP.Free;
      MAI.ARP := nil;
    end;
    if Assigned(MAI) then
      MAI.Free;
  end;
end;

procedure PromptForOutgroup(AnalysisInfo: TAnalysisInfo; var Response: Integer);
{$IFDEF VISUAL_BUILD}
var
  i, MResult: Integer;
{$ENDIF}
begin
  raise Exception.Create('Developer Error - call to deprecated PromptForOutgroup procedure');
  {$IFDEF VISUAL_BUILD}
  AnalysisInfo.ARP.Hide;
  if (AnalysisInfo.MyUsrOperation = dtdoMPInferAncSeq) or (AnalysisInfo.MyUsrOperation = dtdoMPInferAncSeqMyPeg) then
    Response := MessageDlg('Ancestral sequence inference analysis requires that you specify an outgroup for rooting the input tree. Do you want to continue?', mtCustom, mbOkCancel, 0)
  else
    raise Exception.Create('invalid user operation');
  if Response <> mrOk then
    Exit;
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
  AnalysisInfo.ARP.Show;
  {$ENDIF}
end;

function ComputeTree(MAI: TAnalysisInfo; IsUpdateARP: Boolean): Boolean;
var
  IsDNA: Boolean;
  {$IFDEF VISUAL_BUILD}
  response: Integer = 0;
  {$ENDIF}
  tda: TSimpleTreeDataAdapter = nil;
  data: TTreeData = nil;
begin
  Result := False;
  with MAI do
  begin
    IsDNA := not(dsoUseAmino in MAI.MySubsetOpt);

    if MyTreePack.DoesContain(ttWtParsim) then
      MyTvWeight := MyTreePack.TvWeight
    else MyTvWeight :=1;

    MyMaxNoOfTrees := MyTreePack.MaxTrees;

    if MyTreePack.DoesContain(ttSPR) or MyTreePack.DoesContain(ttTBR) then
    begin
      MyNoOfReps     := MyTreePack.RandomAddReps;
      MySearchFactor := 0;
      MySearchLevel  := MyTreePack.SearchLevel;
      if MyTreePack.DoesContain(ttSPR) then
        if IsDNA then
          SPRSearchDNA(MAI)
        else
          SPRSearchAmino(MAI)
      else if MyTreePack.DoesContain(ttTBR) then
        if IsDNA then
          TBRSearchDNA(MAI)
        else
          TBRSearchAmino(MAI);
    end
    else if MyTreePack.DoesContain(ttBranchBound) then
    begin
      if IsDNA then
        BranchBoundSearchDNA(MAI)
      else
        BranchBoundSearchAmino(MAI);
    end
    else if MyTreePack.DoesContain(ttDtSearch) then
    begin
      if MyTreePack.DoesContain(ttMPMinMini) then
      begin
        MyNoOfReps     := 0;
        MySearchFactor := Floor(MyTreePack.SearchFactor);
        MySearchLevel := MyTreePack.SearchLevel;
        if IsDNA then
          MinMiniSearchDNA(MAI)
        else
          MinMiniSearchAmino(MAI);
      end
      else
      begin
        MyNoOfReps     := MyTreePack.RandomAddReps;
        MySearchFactor := 0;
        MySearchLevel := MyTreePack.SearchLevel;
        if IsDNA then
          HeuristicDtSearchDNA(MAI)
        else
          HeuristicDtSearchAmino(MAI);
      end
    end
    else if MyTreePack.DoesContain(ttMPMinMini) then
    begin
      MySearchFactor := Floor(MyTreePack.SearchFactor);
      MySearchLevel := 0;
      if IsDNA then
        MinMiniSearchDNA(MAI)
      else
        MinMiniSearchAmino(MAI);
    end
    else if MyTreePack.DoesContain(ttBLens) then
    begin
      AnalyzeUserTree(MAI);  // Analyze the MP tree by it's Length (sum of minimum possible substitutions over all sites)
    end
    else if MyTreePack.DoesContain(ttAncState) then
      InferAncestralSeq(MAI);
  end;
  Result := True;
end;

//== local functions called from ComputeTree
procedure BranchBoundSearchDNA(MAI: TAnalysisInfo);
var
  ABranchBoundSearchThread: TBranchBoundSearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  ABranchBoundSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    ABranchBoundSearchThread := TBranchBoundSearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 8);
    ABranchBoundSearchThread.MaxNoOfTrees := MAI.MyMaxNoOfTrees;
    ABranchBoundSearchThread.ProgressDlg := MAI.ARP;
    ABranchBoundSearchThread.ShowProgress := true;
    ABranchBoundSearchThread.Info := MAI;
    ABranchBoundSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := ABranchBoundSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    ABranchBoundSearchThread.Start;

    ABranchBoundSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if ABranchBoundSearchThread <> nil then ABranchBoundSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure BranchBoundSearchAmino(MAI: TAnalysisInfo);
var
  ABranchBoundSearchThread: TBranchBoundSearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  ABranchBoundSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    ABranchBoundSearchThread := TBranchBoundSearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 32);
    ABranchBoundSearchThread.MaxNoOfTrees := MAI.MyMaxNoOfTrees;
    ABranchBoundSearchThread.ProgressDlg := MAI.ARP;
    ABranchBoundSearchThread.ShowProgress := true;
    ABranchBoundSearchThread.Info := MAI;
    ABranchBoundSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := ABranchBoundSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    ABranchBoundSearchThread.Start;

    ABranchBoundSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if ABranchBoundSearchThread <> nil then ABranchBoundSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure HeuristicDtSearchDNA(MAI: TAnalysisInfo);
var
  AHeuristicDtSearchThread: TRBA_CNISearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  AHeuristicDtSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    AHeuristicDtSearchThread := TRBA_CNISearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 8, MAI.MySearchLevel, MAI.MyNoOfReps);
                                 //MAI.MySearchFactor should be Dt = 2, 4 or 6 in this case. KT
    AHeuristicDtSearchThread.MaxNoOfTrees := MAI.MyMaxNoOfTrees;
    AHeuristicDtSearchThread.ProgressDlg := MAI.ARP;
    AHeuristicDtSearchThread.ShowProgress := true;
    AHeuristicDtSearchThread.Info := MAI;
    AHeuristicDtSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AHeuristicDtSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AHeuristicDtSearchThread.Start;

    AHeuristicDtSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if AHeuristicDtSearchThread <> nil then AHeuristicDtSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure HeuristicDtSearchAmino(MAI: TAnalysisInfo);
var
  AHeuristicDtSearchThread: TRBA_CNISearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  AHeuristicDtSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    AHeuristicDtSearchThread := TRBA_CNISearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 32, MAI.MySearchLevel, MAI.MyNoOfReps);
                                 //MAI.MySearchFactor should be Dt = 2, 4 or 6 in this case. KT
    AHeuristicDtSearchThread.MaxNoOfTrees := MAI.MyMaxNoOfTrees;
    AHeuristicDtSearchThread.ProgressDlg := MAI.ARP;
    AHeuristicDtSearchThread.ShowProgress := true;
    AHeuristicDtSearchThread.Info := MAI;
    AHeuristicDtSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AHeuristicDtSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AHeuristicDtSearchThread.Start;

    AHeuristicDtSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if AHeuristicDtSearchThread <> nil then AHeuristicDtSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure MinMiniSearchDNA(MAI: TAnalysisInfo);
var
  AMiniMiniSearchThread: TMiniMini_CNISearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  AMiniMiniSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    AMiniMiniSearchThread := TMiniMini_CNISearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 8, MAI.MySearchLevel, MAI.MySearchFactor);
                                 //MAI.MySearchFactor should be Dt = 2, 4 or 6 in this case. KT
    AMiniMiniSearchThread.MaxNoOfTrees := MAI.MyMaxNoOfTrees;
    AMiniMiniSearchThread.ProgressDlg := MAI.ARP;
    AMiniMiniSearchThread.ShowProgress := true;
    AMiniMiniSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AMiniMiniSearchThread;
    AMiniMiniSearchThread.Info := MAI;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AMiniMiniSearchThread.Start;

    AMiniMiniSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if AMiniMiniSearchThread <> nil then AMiniMiniSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure MinMiniSearchAmino(MAI: TAnalysisInfo);
var
  AMiniMiniSearchThread: TMiniMini_CNISearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  AMiniMiniSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    AMiniMiniSearchThread := TMiniMini_CNISearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 32, MAI.MySearchLevel, MAI.MySearchFactor);
                                 //MAI.MySearchFactor should be Dt = 2, 4 or 6 in this case. KT
    AMiniMiniSearchThread.MaxNoOfTrees := MAI.MyMaxNoOfTrees;
    AMiniMiniSearchThread.ProgressDlg := MAI.ARP;
    AMiniMiniSearchThread.ShowProgress := true;
    AMiniMiniSearchThread.Info := MAI;
    AMiniMiniSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AMiniMiniSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AMiniMiniSearchThread.Start;

    AMiniMiniSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if AMiniMiniSearchThread <> nil then AMiniMiniSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure SPRSearchDNA(      MAI: TAnalysisInfo);
var
  AMPTreeSearchThread: TMPTreeSearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  AMPTreeSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    AMPTreeSearchThread := TMPTreeSearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 8);
    AMPTreeSearchThread.SearchMethod  := SPR;
    AMPTreeSearchThread.NoOfInitTrees := MAI.MyNoOfReps;
    AMPTreeSearchThread.SearchLevel   := MAI.MySearchLevel;
    AMPTreeSearchThread.MaxNoOfTrees  := MAI.MyMaxNoOfTrees;
    AMPTreeSearchThread.ProgressDlg := MAI.ARP;
    AMPTreeSearchThread.ShowProgress := true;
    AMPTreeSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    AMPTreeSearchThread.Info := MAI;
    MAI.ARP.Thread := AMPTreeSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AMPTreeSearchThread.Start;

    AMPTreeSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if AMPTreeSearchThread <> nil then AMPTreeSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure SPRSearchAmino(    MAI: TAnalysisInfo);
var
  AMPTreeSearchThread: TMPTreeSearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  AMPTreeSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    AMPTreeSearchThread := TMPTreeSearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 32);
    AMPTreeSearchThread.SearchMethod  := SPR;
    AMPTreeSearchThread.NoOfInitTrees := MAI.MyNoOfReps;
    AMPTreeSearchThread.SearchLevel   := MAI.MySearchLevel;
    AMPTreeSearchThread.MaxNoOfTrees  := MAI.MyMaxNoOfTrees;
    AMPTreeSearchThread.ProgressDlg := MAI.ARP;
    AMPTreeSearchThread.ShowProgress := true;
    AMPTreeSearchThread.Info := MAI;
    AMPTreeSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AMPTreeSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AMPTreeSearchThread.Start;

    AMPTreeSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if AMPTreeSearchThread <> nil then AMPTreeSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure TBRSearchDNA(      MAI: TAnalysisInfo);
var
  AMPTreeSearchThread: TMPTreeSearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  AMPTreeSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    AMPTreeSearchThread := TMPTreeSearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 8);
    AMPTreeSearchThread.SearchMethod  := TBR;
    AMPTreeSearchThread.NoOfInitTrees := MAI.MyNoOfReps;
    AMPTreeSearchThread.SearchLevel   := MAI.MySearchLevel;
    AMPTreeSearchThread.MaxNoOfTrees  := MAI.MyMaxNoOfTrees;
    AMPTreeSearchThread.ProgressDlg := MAI.ARP;
    AMPTreeSearchThread.ShowProgress := true;
    AMPTreeSearchThread.Info := MAI;
    AMPTreeSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AMPTreeSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AMPTreeSearchThread.Start;

    AMPTreeSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if AMPTreeSearchThread <> nil then AMPTreeSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure TBRSearchAmino(    MAI: TAnalysisInfo);
var
  AMPTreeSearchThread: TMPTreeSearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  AMPTreeSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    AMPTreeSearchThread := TMPTreeSearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 32);
    AMPTreeSearchThread.SearchMethod  := TBR;
    AMPTreeSearchThread.NoOfInitTrees := MAI.MyNoOfReps;
    AMPTreeSearchThread.SearchLevel   := MAI.MySearchLevel;
    AMPTreeSearchThread.MaxNoOfTrees  := MAI.MyMaxNoOfTrees;
    AMPTreeSearchThread.ProgressDlg := MAI.ARP;
    AMPTreeSearchThread.ShowProgress := true;
    AMPTreeSearchThread.Info := MAI;
    AMPTreeSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AMPTreeSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AMPTreeSearchThread.Start;

    AMPTreeSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if AMPTreeSearchThread <> nil then AMPTreeSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

{
procedure MaxParsimonySearch(MAI: TAnalysisInfo);
var
  AParsimSearchThread: TParsimSearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  AParsimSearchThread := nil;
  AParsimThreadTerminator := nil;
  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;
    if MAI.isAminoAcid then
      AParsimSearchThread := TParsimSearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 32)
    else
      AParsimSearchThread := TParsimSearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 8);
    AParsimSearchThread.ProgressDlg := MAI.ARP;
    AParsimSearchThread.ShowProgress := True;
    AParsimSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AParsimSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AParsimSearchThread.Start;

    AParsimSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    FreeAndNil(AParsimSearchThread);
    FreeAndNil(AParsimThreadTerminator);
  end;
end;   }

procedure InferAncestralSeq(MAI: TAnalysisInfo);
begin
  ShowParsimTree(MAI);
end;

procedure AnalyzeUserTree(MAI: TAnalysisInfo);
begin
  if MAI.MyOriTreeList.IsBLen then { otherwise, branch lengths will not get recomputed}
    MAI.MyOriTreeList.IsBlen := False;
  ShowParsimTree(MAI);
end;

procedure ComputeBootstrapTree(MAI: TAnalysisInfo);
var
  IsDNA: Boolean;
begin
  with MAI do
  begin
    IsDNA := not(dsoUseAmino in MAI.MySubsetOpt);

    if MyTreePack.DoesContain(ttWtParsim) then MyTvWeight := MyTreePack.TvWeight
                                          else MyTvWeight :=1;

    MyMaxNoOfTrees   := MyTreePack.MaxTrees;

    if MyTreePack.DoesContain(ttSPR) or MyTreePack.DoesContain(ttTBR) then
    begin
      MyNoOfReps     := MyTreePack.RandomAddReps;
      MySearchFactor := 0;
      MySearchLevel  := MyTreePack.SearchLevel;
      if MyTreePack.DoesContain(ttSPR) then
        if IsDNA then
          BootstrapSPRSearchDNA(MAI)
        else
          BootstrapSPRSearchAmino(MAI)
      else if MyTreePack.DoesContain(ttTBR) then
        if IsDNA then
          BootstrapTBRSearchDNA(MAI)
        else
          BootstrapTBRSearchAmino(MAI);
    end
    else if MyTreePack.DoesContain(ttBranchBound) then
    begin
      if IsDNA then
        BootstrapBranchBoundSearchDNA(MAI)
      else
        BootstrapBranchBoundSearchAmino(MAI);
    end
    else if MyTreePack.DoesContain(ttDtSearch) then
    begin
      if MyTreePack.DoesContain(ttMPMinMini) then
      begin
        MyNoOfReps     := 0;
        MySearchFactor := Floor(MyTreePack.SearchFactor);
        MySearchLevel := MyTreePack.SearchLevel;
        if IsDNA then
          BootstrapMinMiniSearchDNA(MAI)
        else
          BootstrapMinMiniSearchAmino(MAI);
      end
      else
      begin
        MyNoOfReps     := MyTreePack.RandomAddReps;
        MySearchFactor := 0;
        MySearchLevel := MyTreePack.SearchLevel;
        if IsDNA then
          BootstrapHeuristicDtSearchDNA(MAI)
        else
          BootstrapHeuristicDtSearchAmino(MAI);
      end
    end
    else if MyTreePack.DoesContain(ttMPMinMini) then
    begin
      MySearchFactor := Floor(MyTreePack.SearchFactor);
      MySearchLevel := 0;
      if IsDNA then
        BootstrapMinMiniSearchDNA(MAI)
      else
        BootstrapMinMiniSearchAmino(MAI);
    end;
  end;
end;

procedure BootstrapBranchBoundSearchDNA(MAI: TAnalysisInfo);
var
  ABootstrapBranchBoundThread: TBootstrapBranchBoundSearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  ABootstrapBranchBoundThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    ABootstrapBranchBoundThread := TBootstrapBranchBoundSearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 8, MAI.MyBootReps);
    ABootstrapBranchBoundThread.MaxNoOfTrees := MAI.MyMaxNoOfTrees;
    ABootstrapBranchBoundThread.BootstrapTrees := MAI.MyBootPartitionList;
    ABootstrapBranchBoundThread.ProgressDlg := MAI.ARP;
    ABootstrapBranchBoundThread.ShowProgress := true;
    ABootstrapBranchBoundThread.Info := MAI;
    ABootstrapBranchBoundThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := ABootstrapBranchBoundThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    ABootstrapBranchBoundThread.Start;

    ABootstrapBranchBoundThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if ABootstrapBranchBoundThread <> nil then ABootstrapBranchBoundThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure BootstrapBranchBoundSearchAmino(MAI: TAnalysisInfo);
var
  ABootstrapBranchBoundThread: TBootstrapBranchBoundSearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  ABootstrapBranchBoundThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    ABootstrapBranchBoundThread := TBootstrapBranchBoundSearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 32, MAI.MyBootReps);
    ABootstrapBranchBoundThread.MaxNoOfTrees := MAI.MyMaxNoOfTrees;
    ABootstrapBranchBoundThread.BootstrapTrees := MAI.MyBootPartitionList;
    ABootstrapBranchBoundThread.ProgressDlg := MAI.ARP;
    ABootstrapBranchBoundThread.ShowProgress := true;
    ABootstrapBranchBoundThread.Info := MAI;
    ABootstrapBranchBoundThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := ABootstrapBranchBoundThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    ABootstrapBranchBoundThread.Start;

    ABootstrapBranchBoundThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if ABootstrapBranchBoundThread <> nil then ABootstrapBranchBoundThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure BootstrapHeuristicDtSearchDNA(MAI: TAnalysisInfo);
var
  AHeuristicDtSearchThread: TBootstrapRBA_CNISearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  AHeuristicDtSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    AHeuristicDtSearchThread := TBootstrapRBA_CNISearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 8, MAI.MySearchLevel, MAI.MyNoOfReps, MAI.MyBootReps);
                                 //MAI.MySearchFactor should be Dt = 2, 4 or 6 in this case. KT
    AHeuristicDtSearchThread.MaxNoOfTrees := MAI.MyMaxNoOfTrees;
    AHeuristicDtSearchThread.BootstrapTrees := MAI.MyBootPartitionList;
    AHeuristicDtSearchThread.ProgressDlg := MAI.ARP;
    AHeuristicDtSearchThread.ShowProgress := true;
    AHeuristicDtSearchThread.Info := MAI;
    AHeuristicDtSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AHeuristicDtSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AHeuristicDtSearchThread.Start;

    AHeuristicDtSearchThread := nil;
    AParsimThreadTerminator := nil;
  Except
    on E : exception do
      ShowMessage('BootstrapHeuristicDtSearchDNA exception : ' + E.Message);
  end;
  finally
    if AHeuristicDtSearchThread <> nil then AHeuristicDtSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure BootstrapHeuristicDtSearchAmino(MAI: TAnalysisInfo);
var
  AHeuristicDtSearchThread: TBootstrapRBA_CNISearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  AHeuristicDtSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    AHeuristicDtSearchThread := TBootstrapRBA_CNISearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 32, MAI.MySearchLevel, MAI.MyNoOfReps, MAI.MyBootReps);
                                 //MAI.MySearchFactor should be Dt = 2, 4 or 6 in this case. KT
    AHeuristicDtSearchThread.MaxNoOfTrees := MAI.MyMaxNoOfTrees;
    AHeuristicDtSearchThread.BootstrapTrees := MAI.MyBootPartitionList;
    AHeuristicDtSearchThread.ProgressDlg := MAI.ARP;
    AHeuristicDtSearchThread.ShowProgress := true;
    AHeuristicDtSearchThread.Info := MAI;
    AHeuristicDtSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AHeuristicDtSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AHeuristicDtSearchThread.Start;

    AHeuristicDtSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if AHeuristicDtSearchThread <> nil then AHeuristicDtSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure BootstrapMinMiniSearchDNA(MAI: TAnalysisInfo);
var
  AMiniMiniSearchThread: TBootstrapMiniMini_CNISearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  AMiniMiniSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    AMiniMiniSearchThread := TBootstrapMiniMini_CNISearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 8, MAI.MySearchLevel, MAI.MySearchFactor, MAI.MyBootReps);
                                 //MAI.MySearchFactor should be Dt = 2, 4 or 6 in this case. KT
    aMiniMiniSearchThread.NoOfThreads := MAI.MyNumThreadsToUse;
    AMiniMiniSearchThread.MaxNoOfTrees := MAI.MyMaxNoOfTrees;
    AMiniMiniSearchThread.BootstrapTrees := MAI.MyBootPartitionList;
    AMiniMiniSearchThread.ProgressDlg := MAI.ARP;
    AMiniMiniSearchThread.ShowProgress := true;
    AMiniMiniSearchThread.Info := MAI;
    AMiniMiniSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AMiniMiniSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AMiniMiniSearchThread.Start;

    AMiniMiniSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if AMiniMiniSearchThread <> nil then AMiniMiniSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure BootstrapMinMiniSearchAmino(MAI: TAnalysisInfo);
var
  AMiniMiniSearchThread: TBootstrapMiniMini_CNISearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  AMiniMiniSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    AMiniMiniSearchThread := TBootstrapMiniMini_CNISearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 32, MAI.MySearchLevel, MAI.MySearchFactor, MAI.MyBootReps);
                                 //MAI.MySearchFactor should be Dt = 2, 4 or 6 in this case. KT
    AMiniMiniSearchThread.MaxNoOfTrees := MAI.MyMaxNoOfTrees;
    AMiniMiniSearchThread.BootstrapTrees := MAI.MyBootPartitionList;
    AMiniMiniSearchThread.ProgressDlg := MAI.ARP;
    AMiniMiniSearchThread.ShowProgress := true;
    AMiniMiniSearchThread.Info := MAI;
    AMiniMiniSearchThread.OnTerminate := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AMiniMiniSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AMiniMiniSearchThread.Start;

    AMiniMiniSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if AMiniMiniSearchThread <> nil then AMiniMiniSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure BootstrapSPRSearchDNA(MAI: TAnalysisInfo);
var
  ABootstrapMPTreeSearchThread: TBootstrapMPTreeSearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  ABootstrapMPTreeSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    ABootstrapMPTreeSearchThread := TBootstrapMPTreeSearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 8, MAI.MyNumThreadsToUse);
    ABootstrapMPTreeSearchThread.BootstrapTrees := MAI.MyBootPartitionList;
    ABootstrapMPTreeSearchThread.NoOfBootstraps := MAI.MyBootReps;
    ABootstrapMPTreeSearchThread.SearchMethod   := SPR;
    ABootstrapMPTreeSearchThread.NoOfInitTrees  := MAI.MyNoOfReps;
    ABootstrapMPTreeSearchThread.SearchLevel    := MAI.MySearchLevel;
    ABootstrapMPTreeSearchThread.MaxNoOfTrees   := MAI.MyMaxNoOfTrees;
    ABootstrapMPTreeSearchThread.ProgressDlg    := MAI.ARP;
    ABootstrapMPTreeSearchThread.ShowProgress   := true;
    ABootstrapMPTreeSearchThread.Info := MAI;
    ABootstrapMPTreeSearchThread.OnTerminate    := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := ABootstrapMPTreeSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    ABootstrapMPTreeSearchThread.Start;

    ABootstrapMPTreeSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if ABootstrapMPTreeSearchThread <> nil then ABootstrapMPTreeSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure BootstrapSPRSearchAmino(MAI: TAnalysisInfo);
var
  ABootstrapMPTreeSearchThread: TBootstrapMPTreeSearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  ABootstrapMPTreeSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    ABootstrapMPTreeSearchThread := TBootstrapMPTreeSearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 32, MAI.MyNumThreadsToUse);
    ABootstrapMPTreeSearchThread.BootstrapTrees := MAI.MyBootPartitionList;
    ABootstrapMPTreeSearchThread.NoOfBootstraps := MAI.MyBootReps;
    ABootstrapMPTreeSearchThread.SearchMethod   := SPR;
    ABootstrapMPTreeSearchThread.NoOfInitTrees  := MAI.MyNoOfReps;
    ABootstrapMPTreeSearchThread.SearchLevel    := MAI.MySearchLevel;
    ABootstrapMPTreeSearchThread.MaxNoOfTrees   := MAI.MyMaxNoOfTrees;
    ABootstrapMPTreeSearchThread.ProgressDlg    := MAI.ARP;
    ABootstrapMPTreeSearchThread.ShowProgress   := true;
    ABootstrapMPTreeSearchThread.Info := MAI;
    ABootstrapMPTreeSearchThread.OnTerminate    := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := ABootstrapMPTreeSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    ABootstrapMPTreeSearchThread.Start;

    ABootstrapMPTreeSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if ABootstrapMPTreeSearchThread <> nil then ABootstrapMPTreeSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure BootstrapTBRSearchDNA(MAI: TAnalysisInfo);
var
  ABootstrapMPTreeSearchThread: TBootstrapMPTreeSearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  ABootstrapMPTreeSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    ABootstrapMPTreeSearchThread := TBootstrapMPTreeSearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 8, MAI.MyNumThreadsToUse);
    ABootstrapMPTreeSearchThread.BootstrapTrees := MAI.MyBootPartitionList;
    ABootstrapMPTreeSearchThread.NoOfBootstraps := MAI.MyBootReps;
    ABootstrapMPTreeSearchThread.SearchMethod   := TBR;
    ABootstrapMPTreeSearchThread.NoOfInitTrees  := MAI.MyNoOfReps;
    ABootstrapMPTreeSearchThread.SearchLevel    := MAI.MySearchLevel;
    ABootstrapMPTreeSearchThread.MaxNoOfTrees   := MAI.MyMaxNoOfTrees;
    ABootstrapMPTreeSearchThread.ProgressDlg    := MAI.ARP;
    ABootstrapMPTreeSearchThread.ShowProgress   := true;
    ABootstrapMPTreeSearchThread.Info := MAI;
    ABootstrapMPTreeSearchThread.OnTerminate    := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := ABootstrapMPTreeSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    ABootstrapMPTreeSearchThread.Start;

    ABootstrapMPTreeSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if ABootstrapMPTreeSearchThread <> nil then ABootstrapMPTreeSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

procedure BootstrapTBRSearchAmino(MAI: TAnalysisInfo);
var
  ABootstrapMPTreeSearchThread: TBootstrapMPTreeSearchThread;
  AParsimThreadTerminator: TParsimThreadTerminator;
begin
  ABootstrapMPTreeSearchThread := nil;
  AParsimThreadTerminator := nil;

  try
    AParsimThreadTerminator := TParsimThreadTerminator.Create;

    ABootstrapMPTreeSearchThread := TBootstrapMPTreeSearchThread.Create(MAI.MyOriTreeList, MAI.MyInfoSites, MAI.MyNoOfSites, 32, MAI.MyNumThreadsToUse);
    ABootstrapMPTreeSearchThread.SkipSummaryUpdate := False;
    ABootstrapMPTreeSearchThread.BootstrapTrees := MAI.MyBootPartitionList;
    ABootstrapMPTreeSearchThread.NoOfBootstraps := MAI.MyBootReps;
    ABootstrapMPTreeSearchThread.SearchMethod   := TBR;
    ABootstrapMPTreeSearchThread.NoOfInitTrees  := MAI.MyNoOfReps;
    ABootstrapMPTreeSearchThread.SearchLevel    := MAI.MySearchLevel;
    ABootstrapMPTreeSearchThread.MaxNoOfTrees   := MAI.MyMaxNoOfTrees;
    ABootstrapMPTreeSearchThread.ProgressDlg    := MAI.ARP;
    ABootstrapMPTreeSearchThread.ShowProgress   := true;
    ABootstrapMPTreeSearchThread.Info := MAI;
    ABootstrapMPTreeSearchThread.OnTerminate    := AParsimThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := ABootstrapMPTreeSearchThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    ABootstrapMPTreeSearchThread.Start;

    ABootstrapMPTreeSearchThread := nil;
    AParsimThreadTerminator := nil;
  finally
    if ABootstrapMPTreeSearchThread <> nil then ABootstrapMPTreeSearchThread.Free;
    if AParsimThreadTerminator <> nil then AParsimThreadTerminator.Free;
  end;
end;

end.

