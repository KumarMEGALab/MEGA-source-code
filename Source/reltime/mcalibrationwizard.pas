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

unit MCalibrationWizard;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLIntF, LCLType,
  {$ELSE}
  Windows,
  {$ENDIF}
  Forms, Classes, SysUtils, Dialogs, MTreeViewForm, MCalibrationDlg, MCalibrationData,
  MAnalysisInfo, MDistPack, ExtCtrls;

type


  TCalibrationWizard = class(TObject)
    private
      {$IFDEF VISUAL_BUILD}
      FTreeViewer: TTreeViewForm;
      {$ENDIF}
      FTimer: TTimer;
      procedure OnTimer(Sender: TObject);
    public
      AnalysisInfo: TAnalysisInfo;
      UsrOperation: TDistTreeDlgOption;
      Calibrations: TCalibrations;
      constructor Create;
      destructor Destroy; override;

      procedure AssignCalibrationTimes(Calibs: TCalibrations);

      { Sets up FTreeViewer and FCalibrationDlg and shows them so that the user
        can specify calibration points}
      procedure LaunchCalibrationTools;

      { Cleans everything up if the user aborts}
      procedure CancelCalibrationChanges;

      { Picks up where ProcessSeqMLRelTimeCommand left off}
      procedure LaunchTimeTreeAnalysis;

  end;


implementation

uses
  MegaVerConsts, MOtuInfo, MLTreeAnalyzer, MLModels,
  MMatrixExport, MProcessPack, ProcessTreeCmds, MLTree, MTreeData, MegaConsts,
  MD_InputSeqData, MPartitionData, MegaErrUtils, ErrorMessages_HC, MTreePack,
  ProcessMLTreeCmds, MTreeList, MPartitionList, MSubstitutionModelUtils, syncobjs
  {$IFDEF VISUAL_BUILD}, MAnalysisPrefDlg{$ENDIF} {$IFNDEF FPC}, System.AnsiStrings{$ENDIF};



{ TCalibrationWizard }

procedure TCalibrationWizard.CancelCalibrationChanges;
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTimer) then
    FreeAndNil(FTimer);
  try
    try
      AnchoredTimesCS.Acquire;
      {$IFDEF VISUAL_BUILD}
      if TreeViewerLives then
        FreeAndNil(FTreeViewer)
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


constructor TCalibrationWizard.Create;
begin
  {$IFDEF VISUAL_BUILD}
  FTreeViewer := nil;
  {$ENDIF}
  FTimer := nil;
  Calibrations := TCalibrations.Create;
  {$IFDEF VISUAL_BUILD}
  AnchoredTimesCS := TCriticalSection.Create;
  {$ENDIF}
end;

destructor TCalibrationWizard.Destroy;
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FTreeViewer) then
    FTreeViewer.Free;
  {$ENDIF}
  if Assigned(FTimer) then
    FTimer.Free;
  {$IFDEF VISUAL_BUILD}
  FreeAndNil(AnchoredTimesCS);
  {$ENDIF}
  inherited;
end;

procedure TCalibrationWizard.LaunchCalibrationTools;
var
  TempTreeList: TTreeList;
  NamesList: TStringList;
  TempName: String;
  i: Integer;
begin
  {$IFDEF VISUAL_BUILD}
  NamesList := nil;
  FTreeViewer := TTreeViewForm.Create(Application);
  try
    try
      FTreeViewer.miCaptionExpert.Visible := False;
      if not FTreeViewer.ImportNewickStandard(AnalysisInfo.MyUserTreeFile) then
      begin
        MessageDlg('MEGA encountered a problem importing your newick file, it may not be a valid newick standard file.', mtError, [mbOK], 0);
        exit;
      end;
      TempTreeList := TTreeList.Create;
      NamesList := TStringList.Create;
      for i := 0 to AnalysisInfo.MyOtuNames.Count - 1 do
      begin
        TempName := StringReplace(AnalysisInfo.MyOtuNames[i], '_', ' ', [rfReplaceAll]);
        NamesList.Add(TempName);
      end;
      if not TempTreeList.ImportFromNewickFile(AnalysisInfo.MyUserTreeFile, NamesList) then
        raise Exception.Create('Failed to parse newick file.');

      FTreeViewer.PrepareToWorkWithTimeTreeWizard(AnalysisInfo, TempTreeList[0]);
      FTreeViewer.CalibrationDlg.CalibrationWizard := Self;
      FTreeViewer.CalibrationDlg.CWCancelCallBack := CancelCalibrationChanges;
      FTreeViewer.Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Tree Explorer ('+ AnalysisInfo.MyUserTreeFile + ')';
      FTreeViewer.IsSetRelTimeCalibsMode := True;
      {$IFNDEF PROTOTYPER}
      try
        AnchoredTimesCS.Acquire;
        CalibrationsAreSet := False;
        CalibrationsCancelled := False;
      finally
        AnchoredTimesCS.Release;
      end;
      {$ENDIF}
      FTimer := TTimer.Create(nil);
      FTimer.OnTimer := OnTimer;
      FTimer.Interval := 500;

      FTreeViewer.Show;
      FTreeViewer.CalibrationDlg.Show;
      FTreeViewer.CalibrationDlg.ResetForm;
      FTimer.Enabled := True;
    except
      On E: Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(TempTreeList) then
      TempTreeList.Free;
    if Assigned(NamesList) then
      NamesList.Free;
  end;
  {$ENDIF}
end;

procedure TCalibrationWizard.LaunchTimeTreeAnalysis;
var
  Model: TGammaRateVariationModel;
  MLTreeAnalyzer: TMLTreeAnalyzer;
  TreeData: TTreeData;
  i: Integer;
  ClockLevel: TClockLevel;
  MinTimes, MaxTimes: TDivTimesArray;
  SamplingTimes: TDivTimesArray;
  NamesList: TStringList;
  TempName: String;
  HasDivTimes: Boolean;
  HasSampleTimes: Boolean;
begin
  HasDivTimes := False;
  HasSampleTimes := False;
  Model := nil;
  MLTreeAnalyzer := nil;
  TreeData := nil;
  NamesList := nil;
  SetLength(MinTimes, 0);
  SetLength(MaxTimes, 0);
  SetLength(SamplingTimes, 0);
  {$IFDEF VISUAL_BUILD}
  AnalysisPrefDlg.UserTreeFile := AnalysisInfo.MyUserTreeFile;

  if Assigned(FTreeViewer) then
  begin
    try
      FTreeViewer.IsSetRelTimeCalibsMode := False;
      FreeAndNil(FTreeViewer);
    except
      // life goes on
    end;
  end;
  {$ENDIF}
  if not AnalysisInfo.GetAnalysisOptions(dtdoRelTimeML) then
    Exit;

  try
    try
      Application.ProcessMessages;
      AnalysisInfo.ARP.WriteAnalysisOptionsToStdOut;
      AnalysisInfo.ARP.AddRunStatusInfo('Status', 'Preparing data');
      AnalysisInfo.ARP.Show;
      AnalysisInfo.MySeqStrings := TStringList.Create;
      AnalysisInfo.MyTreePack.TreeFile := AnalysisInfo.MyUserTreeFile;

      { TODO 1 -oglen -cpartitionedML : switch to PrepareDataForPartitionedMLAnalysis below as soon as we are ready }
    //  AnalysisInfo.MySeqPartitions := TListOfSeqPartitions.Create;
    //    D_InputSeqData.PrepareDataForPartitionedMLAnalysis(MAI.MySubsetOpt, MAI.MySeqPartitions, MAI.MyUsedOtuInfos,
    //                                            MAI.MyNoOfSeqs, MAI.MyNoOfSites,  MAI.MyLabelsUsed, MAI.RecodeScheme,MAI.SiteCoverage);

      D_InputSeqData.PrepareDataForMLAnalysis(AnalysisInfo.MySubsetOpt,
                                              AnalysisInfo.MySeqStrings,
                                              AnalysisInfo.MyUsedOtuInfos,
                                              AnalysisInfo.MyNoOfSeqs,
                                              AnalysisInfo.MyNoOfSites,
                                              AnalysisInfo.MyLabelsUsed,
                                              AnalysisInfo.RecodeScheme,
                                              AnalysisInfo.SiteCoverage);

      AnalysisInfo.ARP.AddAnalysisOptions('No. of Seqs', IntToStr(AnalysisInfo.MyNoOfSeqs));
      AnalysisInfo.ARP.AddAnalysisOptions('No. of Sites', IntToStr(AnalysisInfo.MyNoOfSites));

      { validate the data}
      if AnalysisInfo.MyNoOfSites < 1 then
        RaiseErrorMessage(HC_No_Common_Sites, 'No common sites found. ' + LineEnding + NoCommonSitesStr);
      if AnalysisInfo.MyNoOfSeqs < 3 then
        RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least three sequences are needed for Likelihood RelTime analysis');
      if AnalysisInfo.MyTreePack.DoesContain(ttBootstrap) then
      begin
        if AnalysisInfo.MyNoOfSeqs < 4 then
         RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least four taxa are needed for bootstrapping.');
      end;

      { Now create the Treelist, model, treedata and setup the MLTreeAnalyzer}
      Model := CreateSubstitutionModel(AnalysisInfo);
      AnalysisInfo.MyOriTreeList := TTreeList.Create;
      NamesList := TStringList.Create;
      for i := 0 to AnalysisInfo.MyOtuNames.Count - 1 do
      begin
        TempName := {$IFNDEF FPC}System.AnsiStrings.{$ENDIF}StringReplace(AnalysisInfo.MyOtuNames[i], '_', ' ', [rfReplaceAll]);
        NamesList.Add(TempName);
      end;
      if not AnalysisInfo.MyOriTreeList.ImportFromNewick(AnalysisInfo.MyUserNewickTree, NamesList) then
        raise Exception.Create('failed to parse/validate newick file');

      TreeData := TTreeData.Create(AnalysisInfo.NoOfSeqs,false,false,false);
      TreeData.Assign(AnalysisInfo.MyOriTreeList[0]);
      AnalysisInfo.MyOriTreeList.UpdateNodeLabels(Calibrations);
      AnalysisInfo.CalibrationTimes := Calibrations;
      MLTreeAnalyzer := TMLTreeAnalyzer.Create(AnalysisInfo.MySeqStrings, TreeData, Model);
      MLTreeAnalyzer.NoOfThreadsToUse := AnalysisInfo.MyNumThreadsToUse;
      MLTreeAnalyzer.IsGlobalClock := False;
      AnalysisInfo.ClockTypeSet := True;
      ClockLevel := AnalysisInfo.ClockLevel;
      Case ClockLevel of
        clOneStdErr:
          MLTreeAnalyzer.GlobalClockLevel := 1;
        clTwoStdErr:
          MLTreeAnalyzer.GlobalClockLevel := 2;
        clThreeStdErr:
          MLTreeAnalyzer.GlobalClockLevel := 3;
      End;
      if AnalysisInfo.MLBLensFilter > 0 then
          MLTreeAnalyzer.BLenFilter := AnalysisInfo.MLBLensFilter;
      AnalysisInfo.MyMLAnalysisPack := MLTreeAnalyzer;

      {$IFDEF VISUAL_BUILD}
        AnalysisInfo.ARP.ProcessMessages;
      {$ENDIF}

      if Calibrations.Count > 0 then
      begin
        AnalysisInfo.MyOriTreeList.ResolveCalibrationTimeNames(Calibrations);
        AnalysisInfo.MyOriTreeList[0].resolveRelTimes(Calibrations);

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
        EstimateRelativeDivTimes(AnalysisInfo)
      end;
    except
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
  finally
    if Assigned(NamesList) then
      FreeAndNil(NamesList);
  end;
end;


procedure TCalibrationWizard.OnTimer;
begin
  {$IFDEF VISUAL_BUILD}
  {$IFNDEF PROTOTYPER}
  try
    try
      AnchoredTimesCS.Acquire;
      if CalibrationsAreSet then
      begin
        FTimer.Enabled := False;
        LaunchTimeTreeAnalysis;
      end
      else if CalibrationsCancelled then
      begin
        CancelCalibrationChanges;
      end;
    except
      // do nothing, we only reach this if the user closes MEGA without completing the wizard
    end;
  finally
    AnchoredTimesCS.Release;
  end;
  {$ENDIF}
  {$ENDIF}
end;


procedure TCalibrationWizard.AssignCalibrationTimes(Calibs: TCalibrations);
begin
  if not Assigned(Calibrations) then
    Calibrations := TCalibrations.Create;
  Calibrations.Assign(Calibs);
end;

end.
