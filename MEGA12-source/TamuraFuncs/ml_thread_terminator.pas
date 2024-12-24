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

unit ml_thread_terminator;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  { TMLThreadTerminator }

  TMLThreadTerminator = class(TObject)
  public
    procedure OnThreadDone(Thread: TObject);
    procedure OnStandardBootstrapThreadDone(Thread: TObject);
  end;
implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MD_MegaMain, MegaUtils_NV,
  {$ELSE}
  mega_main, MEditorForm, MutationDetailView,
  {$ENDIF}
  MAnalysisInfo, MLSearchThread, MegaConsts, mtreepack, MTreeData, MTreeList,
  ShowTrees, MProcessPack, mdistpack, ProcessMLTreeCmds;

{ TMLThreadTerminator }

procedure TMLThreadTerminator.OnThreadDone(Thread: TObject);
var
  MAI: TAnalysisInfo = nil;
  CurrentAnalysisClassName: String;
  isMLAnalysis: Boolean;
  t: TMLTreeThread = nil;
  needToAbort: Boolean = False;
begin

  try
    try
      MAI := (Thread as TMLTreeThread).ProgressDlg.FMAI;
      begin
        CurrentAnalysisClassName := Thread.ClassName;
        isMLAnalysis := (CurrentAnalysisClassName = 'TMLTreeSearchThread') or
           (CurrentAnalysisClassName = 'TBootstrapMLThread') or
           (CurrentAnalysisClassName = 'TMLTreeAnalyzeThread') or
           (CurrentAnalysisClassName = 'TModelTestThread') or
           (CurrentAnalysisClassName = 'TMLClockTestThread');
        Assert(isMLAnalysis);
        t := TMLTreeThread(Thread);
        {$IFNDEF VISUAL_BUILD}
          D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
        {$ELSE}
          if not t.Canceled then
            MAI.AnalysisSummary.AddAnalysisInfo(MAI);
        {$ENDIF}
        if CurrentAnalysisClassName = 'TBootstrapMLThread' then
        begin
          if t.Canceled then
            needToAbort := False
          else if not t.IsSuccess then
            needToAbort := True;
        end
        else
        begin
          if t.Canceled or  (not t.IsSuccess) then
            needToAbort := True;
        end;

        if needToAbort then
        begin
          if Assigned(MAI) and Assigned(MAI.MyMLAnalysisPack) then
            MAI.MyMLAnalysisPack := nil;
          if t.Canceled then
            raise EAbort.Create(t.LogText)
          else
            raise Exception.Create(t.LogText);
        end
        else  // the thread terminated as expected so display the results
        begin
          if not t.Canceled then
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
            else if MAI.MyTreePack.DoesContain(ttAncState) or MAI.MyTreePack.DoesContain(ttBLens) then
              ShowMLTree(MAI)
            else if MAI.MyTreePack.DoesContain(ttClockTest) then
            begin
              if MAI.InitialUsrOperation = dtdoMLClockTest then
                ShowClockTestResults(MAI);
            end
            else if MAI.MyTreePack.DoesContain(ttPattern) or
                    MAI.MyTreePack.DoesContain(ttTsTvBias) or
                    MAI.MyTreePack.DoesContain(ttGamma) then
              ShowPatternResults(MAI)
            else if MAI.MyTreePack.DoesContain(ttSiteRates) then
              ShowSiteRateResults(MAI);
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
              end
              else
                if Assigned(MAI) then
                  MAI.MyMLAnalysisPack := nil // will be free by TMLTreeThread
            end;
            MAI := nil;
        end;
      end;
    except
      on E:EAbort do
      begin
        if Assigned(MAI) and Assigned(MAI.ARP) and MAI.ARP.Visible then
          MAI.ARP.Hide;
        t.SynchronizeAbortMessage(E.Message);
      end;
      on E:Exception do
      begin
        if Assigned(MAI) and Assigned(MAI.ARP) and MAI.ARP.Visible then
          MAI.ARP.Hide;
        t.SynchronizeErrorMessage(E);
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
      begin
        MAI.ARP.Free;
        MAI.ARP := nil;
      end;
      MAI.MyProcessPack := nil;
      MAI.Free;
    end;

    if not (Thread as TMLTreeThread).FreeOnTerminate then
    begin
      (Thread as TMLTreeThread).MLTreeAnalyzer.CheckCancel := nil;   // Thread should not be freed on terminate to execute this line
      (Thread as TMLTreeThread).MLTreeAnalyzer := nil;               // Thread should not be freed on terminate to execute this line
      (Thread as TMLTreeThread).FreeOnTerminate := True;
      (Thread as TMLTreeThread).Terminate;
    end;
  end;
end;

procedure TMLThreadTerminator.OnStandardBootstrapThreadDone(Thread: TObject);
var
  MAI: TAnalysisInfo = nil;
  CurrentAnalysisClassName: String;
  isMLAnalysis: Boolean;
  t: TMLTreeThread = nil;
  needToAbort: Boolean = False;
  aData: TTreeData = nil;
begin
  try
    try
      MAI := (Thread as TMLTreeThread).ProgressDlg.FMAI;
      begin
        CurrentAnalysisClassName := Thread.ClassName;
        isMLAnalysis := (CurrentAnalysisClassName = 'TMLTreeSearchThread') or
           (CurrentAnalysisClassName = 'TBootstrapMLThread') or
           (CurrentAnalysisClassName = 'TMLTreeAnalyzeThread') or
           (CurrentAnalysisClassName = 'TModelTestThread') or
           (CurrentAnalysisClassName = 'TMLClockTestThread');
        Assert(isMLAnalysis);
        t := TMLTreeThread(Thread);
        {$IFNDEF VISUAL_BUILD}
          D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
        {$ELSE}
          if not t.Canceled then
            MAI.AnalysisSummary.AddAnalysisInfo(MAI);
        {$ENDIF}

        if CurrentAnalysisClassName = 'TBootstrapMLThread' then
        begin
          if t.Canceled then
            needToAbort := False
          else if not t.IsSuccess then
            needToAbort := True;
        end;

        if needToAbort then
        begin
          if Assigned(MAI) and Assigned(MAI.MyMLAnalysisPack) then
            MAI.MyMLAnalysisPack := nil;
          if t.Canceled then
            raise EAbort.Create(t.LogText)
          else
            raise Exception.Create(t.LogText);
        end
        else  // the thread terminated as expected so display the results
        begin
          if not t.Canceled then
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
            if MAI.MyTreePack.DoesContain(ttBootstrap) then
            begin
              if MAI.MyOriTreeList.Count = 0 then
                MAI.MyOriTreeList.Add(TTreeData.Create(MAI.MyNoOfSeqs, true, false, true));
              aData := MAI.MyOriTreeList[0];
              aData.isStats := true;
              MAI.MyMLAnalysisPack.GetTreeData(aData);
              MAI.MyBootPartitionList.Compare(aData.NodeArray, PArrayOfDouble(@aData.StatsArray[aData.NoOfOTUs]));
              MAI.MyValidReps := Round(MAI.MyBootPartitionList.TotalFrequency);
              ShowMLBootTree(MAI);
            end
            else
              raise exception.Create(Format('expected TBootstrapMLThread but got %s', [Thread.ClassName]));
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
              end
              else
                raise exception.Create(Format('expected TBootstrapMLThread but got %s', [Thread.ClassName]));
            end;
            MAI := nil;
        end;
      end;
    except
      on E:EAbort do
      begin
        if Assigned(MAI) and Assigned(MAI.ARP) and MAI.ARP.Visible then
          MAI.ARP.Hide;
        t.SynchronizeAbortMessage(E.Message);
      end;
      on E:Exception do
      begin
        if Assigned(MAI) and Assigned(MAI.ARP) and MAI.ARP.Visible then
          MAI.ARP.Hide;
        t.SynchronizeErrorMessage(E);
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
      begin
        MAI.ARP.Free;
        MAI.ARP := nil;
      end;
      MAI.MyProcessPack := nil;
      MAI.Free;
    end;

    if not (Thread as TMLTreeThread).FreeOnTerminate then
    begin
      (Thread as TMLTreeThread).MLTreeAnalyzer.CheckCancel := nil;   // Thread should not be freed on terminate to execute this line
      (Thread as TMLTreeThread).MLTreeAnalyzer := nil;               // Thread should not be freed on terminate to execute this line
      (Thread as TMLTreeThread).FreeOnTerminate := True;
      (Thread as TMLTreeThread).Terminate;
    end;
  end;
end;


end.

