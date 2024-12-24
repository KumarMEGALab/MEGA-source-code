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

unit ProcessTestCmds;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, MDistPack, MProcessPack, ProcessDistCmds;

type

  { TMclCommandProcessor }

  TMclCommandProcessor = class(TDistCommandProcessor)
    protected
      function LaunchDistCommandThread: Boolean; override;
      procedure CheckForEnoughCommonSites; reintroduce;
    public
      function ProcessMclCommand(aUsrOperation: TDistTreeDlgOption; aProcessPack: TProcessPack): Boolean;
  end;

procedure ProcessTajimaClockCommand(ProcessPack : TProcessPack);
procedure ProcessTajimaNeutralityTestCommand(ProcessPack : TProcessPack);
procedure ProcessTwoClusterRelativeRateTestCommand;
function ProcessDisparityIndexTestCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack): Boolean;
function ProcessMCLCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack): Boolean;

implementation

uses
{$IFDEF VISUAL_BUILD}
  MTaxaGpsDlg, MEditorForm, Mega_Main,
{$ELSE}
 MD_MegaMain, MegaUtils_NV,
{$ENDIF}
  LCLIntf, LCLType, SysUtils, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons, mtajimaneutralitytestthread,
  MegaUtils,  MegaConsts, MegaErrUtils, ErrorMessages_HC,
  MD_InputSeqData, MRuntimeProgressDlg,
  MNucDist, MAnalysisInfo, MLegendGenerator,
  MegaVerConsts, mtajimaclocktestthread,
  mcl_command_threads;

procedure ProcessTajimaClockCommand(ProcessPack: TProcessPack);
var
  t: TTajimaClockTestThread = nil;
  MyThreeOtuInfos: TList = nil;
  ARP : TRuntimeProgress = nil;
  MAI: TAnalysisInfo = nil;
begin
  try try
    ARP := TRuntimeProgress.Create(Application);
    ARP.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Tajima''s relative rate test';
    MAI := TAnalysisInfo.Create;
    {$IFDEF VISUAL_BUILD}
    ARP.DataFileName  :=  MegaForm.DataTitle;
    ARP.DataTitle     :=  MegaForm.DataTitle;
    MAI.DataFilename := MegaForm.DataFilename;
    {$ELSE}
    ARP.DataFileName  :=  D_MegaMain.DataFileName; // changed to .DataFile;
    ARP.DataTitle     :=  D_MegaMain.DataTitle;
    MAI.DataFilename := D_MegaMain.DataFilename;
    {$ENDIF}

    MAI.ARP := ARP;
    MAI.InitialUsrOperation := dtdoTajimaClockTest;
    MAI.MyProcessPack := ProcessPack;
    if MAI.GetAnalysisOptions(dtdoTajimaClockTest) then
    begin
      ARP.WriteAnalysisOptionsToStdOut;
      ARP.AddRunStatusInfo('Status', 'Preparing data');
      ARP.Show;
      MAI.MyMappedData := TList.Create;
      MyThreeOtuInfos:= TList.Create;
      try
        MAI.FillThreeOutInfos(MyThreeOtuInfos);
      Except on E: Exception do
        begin
          {$IFDEF VISUAL_BUILD}
          MessageDlg('MEGA Encountered a problem in the Tajima Clock Test Details: ' + E.Message, mtInformation, [mbOK], 0);
          {$ELSE}
          Warn_nv('MEGA Encountered an error in the Tajima Clock Test Details: ' + E.message);
          {$ENDIF}
          Exit;
        end;
      end;
      D_InputSeqData.ARP := ARP;
      D_InputSeqData.PrepareDataForDistAnalysis(MAI.MySubsetOpt, MAI.MyMappedData, MyThreeOtuInfos, MAI.MyNoOfSeqs, MAI.MyNoOfSites, MAI.MyLabelsUsed, MAI.RecodeScheme, MAI.SiteCoverage);
      if MAI.MyNoOfSites < 1 then
        RaiseErrorMessage(HC_No_Common_Sites, 'No common sites were found. ' + LineEnding + NoCommonSitesStr);
      t := TTajimaClockTestThread.Create(MAI, MyThreeOtuInfos);
      t.CheckCancelFunc := ARP.ProgressCheckCancel;
      {$IFDEF VISUAL_BUILD}
      t.OnTerminate := MegaForm.TajimaClockTestDone;
      {$ENDIF}
      MAI := nil;
      ARP := nil;
      t.Start;
    end;
  except
    on E: Exception do
      ShowErrorMessage(E);
  end;
  finally
    if ARP <> nil then
      ARP.Free;
    if MAI <> nil then
    begin
      MAI.MyProcessPack := nil;
      MAI.Free;
    end;
  end;
end;

procedure ProcessTajimaNeutralityTestCommand(ProcessPack : TProcessPack);
var
  ARP : TRuntimeProgress = nil;
  MAI: TAnalysisInfo = nil;
  t: TTajimaNeutralityTestThread = nil;
begin
  try try
    ARP := TRuntimeProgress.Create(Application);
    ARP.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Tajima''s Test of Neutrality';
    MAI := TAnalysisInfo.Create;
    {$IFDEF VISUAL_BUILD}
    ARP.DataFileName  :=  MegaForm.DataTitle;
    ARP.DataTitle     :=  MegaForm.DataTitle;
    MAI.DataFilename := MegaForm.DataFilename;
    {$ELSE}
    ARP.DataFileName  :=  D_MegaMain.DataFileName;
    ARP.DataTitle     :=  D_MegaMain.DataTitle;
    MAI.DataFilename := D_MegaMain.DataFilename;
   {$ENDIF}

    MAI.MyProcessPack := ProcessPack;
    MAI.InitialUsrOperation := dtdoTajimaNeutralityTest;
    MAI.ARP := ARP;
    if not MAI.GetAnalysisOptions(dtdoTajimaNeutralityTest) then
        Exit;
    ARP.WriteAnalysisOptionsToStdOut;
    ARP.AddRunStatusInfo('Status', 'Preparing data');
    ARP.Show;
    MAI.MyMappedData := TList.Create;
    D_InputSeqData.ARP := ARP;
    D_InputSeqData.PrepareDataForDistAnalysis(MAI.MySubsetOpt, MAI.MyMappedData, MAI.MyUsedOtuInfos, MAI.MyNoOfSeqs, MAI.MyNoOfSites, MAI.MyLabelsUsed, MAI.RecodeScheme, MAI.SiteCoverage);
    D_InputSeqData.ARP := nil;
    if MAI.MyNoOfSites < 10 then
      RaiseErrorMessage(HC_No_Common_Sites, 'Less than 10 common sites found! ' + NoCommonSitesStr);
    ARP.AddAnalysisOptions('No. of Sites', IntToStr(MAI.MyNoOfSites));
    ARP.Progress := 0;
    ARP.UpdateRunStatusInfo('Status', 'Estimating the number of seqregating sites');
    ARP.Refresh;
    Application.ProcessMessages;
    t := TTajimaNeutralityTestThread.Create(MAI);
    t.CheckCancelFunc := ARP.ProgressCheckCancel;
    {$IFDEF VISUAL_BUILD}
    t.OnTerminate := MegaForm.TajimaNeutralityTestDone;
    {$ELSE}
    t.OnTerminate := t.OnTestDone;
    {$ENDIF}
    ARP := nil;
    MAI := nil;
    t.Start;
  except
    on E: Exception do
      ShowErrorMessage(E);
  end;
  finally
    if ARP <> nil then
      ARP.Free;
    if MAI <> nil then
    begin
      MAI.MyProcessPack := nil;
      MAI.Free;
    end;
  end;
end;

procedure ProcessTwoClusterRelativeRateTestCommand;
begin
   //Well, since this isn't implemented no need to refactor it, just write it without using TaxaGpsDlg directly
  {$IFDEF VISUAL_BUILD}
  TaxaGpsDlg.NoofClusters := 3;
  TaxaGpsDlg.ClusterView := True;
  if TaxaGpsDlg.ShowModal <> mrOK then
  begin
    TaxaGpsDlg.ClusterView := False;
    Exit;
  end;
  TaxaGpsDlg.ClusterView := False;

  ShowMessage('Not implemented');
  {$ENDIF}
  // take a tree in the linear format
  // make a system to compute everything, within distance method
  // In Distance estimation system, I should
  // first compute CurDx, then compute all the important things
  // For a two cluster test; for each node, we partition tree into A, B, and C.
  //   Then compute the deltaL
  // For root-to-tip test, we do the same thing, but this time compute
  // the OLS branch lengths first and then estimate all the things.
  // So, just give the linear tree structure to the
  // TSeqDistBase (later to any base).
  // Also provide facility to given any three gpIds and get the result.
end;

function ProcessDisparityIndexTestCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack): Boolean;
var
  processor: TDistCommandProcessor = nil;
begin
  try
    processor := TDistCommandProcessor.Create;
    Result := processor.ProcessDisparityIndexTestCommand(UsrOperation, ProcessPack);
  finally
    if Assigned(processor) then
      processor.Free;
  end;
end;

function ProcessMCLCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack): Boolean;
var
  mclProcessor: TMclCommandProcessor = nil;
begin
  try
    try
      mclProcessor := TMclCommandProcessor.Create;
      Result := mclProcessor.ProcessMclCommand(UsrOperation, ProcessPack);
    except
      on E: Exception do
      begin
        {$IFDEF VISUAL_BUILD}
         ShowMessage('Application Error: ' + E.Message);
        {$ELSE}
        error_nv('Error in ProcessMCLCommand', E);
        {$ENDIF}
      end;
    end;
  finally
    if Assigned(mclProcessor) then
      mclProcessor.Free;
  end;
end;

{ TMclCommandProcessor }

function TMclCommandProcessor.LaunchDistCommandThread: Boolean;
var
  t: TMclCommandThread = nil;
begin
  t := TMclCommandThread.Create(MAI, UsrOperation);
  {$IFDEF VISUAL_BUILD}
  t.OnTerminate := MegaForm.MCLCommandThreadDone;
  {$ENDIF}
  t.Start;
  Result := True;
end;

procedure TMclCommandProcessor.CheckForEnoughCommonSites;
begin
  if MAI.MyNoOfSites < 10 then
    RaiseErrorMessage(HC_No_Common_Sites, 'Less than 10 common sites found! ' + NoCommonSitesStr);
end;

function TMclCommandProcessor.ProcessMclCommand(aUsrOperation: TDistTreeDlgOption; aProcessPack: TProcessPack): Boolean;
begin
  Result := False;
  try
    UsrOperation := aUsrOperation;
    ProcessPack := aProcessPack;
    FIsWorkflowCalculation := ProcessPack.IsWorkflowPack;
    InitRuntimeProgress;
    InitAnalysisInfo;
    if MAI.GetAnalysisOptions(UsrOperation) then
    begin
      Result := True;
      ShowRuntimeProgress;
      SubsetTheData;
      if not LaunchDistCommandThread then
        raise Exception.Create('failed to launch MCL calculation thread');
    end
    else
      FLog.Add('user aborted when getting analysis options');
  except
    on E: Exception do
    begin
      FLog.Add(E.Message);
      if Assigned(ProcessPack) and ProcessPack.IsWorkflowPack then
        Result := False
      else
        ShowErrorMessage(E);
    end;
  end;
end;

end.

