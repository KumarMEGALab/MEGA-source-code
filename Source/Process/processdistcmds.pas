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

unit ProcessDistCmds;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, MDistPack, MProcessPack, MRuntimeProgressDlg, MAnalysisInfo, MLegendGenerator,
  MNucDist, MegaConsts;

type

  { TDistCommandProcessor }

  TDistCommandProcessor = class(TObject)
    protected
      FLog: TStringList;
      FIsWorkflowCalculation: Boolean;
      UsrOperation: TDistTreeDlgOption;
      ProcessPack: TProcessPack;
      IsSelTest: Boolean;
      IsDisparityTest: Boolean;
      MAI: TAnalysisInfo;
      ARP : TRuntimeProgress;
      procedure InitRuntimeProgress; virtual;
      procedure ShowRuntimeProgress;
      procedure InitAnalysisInfo;
      procedure CheckForSelectionTest;
      procedure CheckForEnoughCommonSites; virtual;
      procedure SubsetTheData;
      function LaunchDistCommandThread: Boolean; virtual;
    public
      constructor Create;
      destructor Destroy; override;
      function ProcessDistanceCommand(aUsrOperation: TDistTreeDlgOption; aProcessPack: TProcessPack): Boolean;
      function ProcessDisparityIndexTestCommand(aUsrOperation: TDistTreeDlgOption; aProcessPack : TProcessPack): Boolean;
      property Log: TStringList read FLog;
  end;

function ProcessDistanceCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack): Boolean;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  MEGA_Main,
  {$ELSE}
  MD_MegaMain,
  {$ENDIF}
  LCLIntf, LCLType, SysUtils, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons, MegaErrUtils,
  ErrorMessages_HC, MD_InputSeqData, mdist_command_threads;

function ProcessDistanceCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack): Boolean;
var
  processor: TDistCommandProcessor = nil;
begin
  try
    processor := TDistCommandProcessor.Create;
    Result := processor.ProcessDistanceCommand(UsrOperation, ProcessPack);
  finally
    if Assigned(processor) then
      processor.Free;
  end;
end;

{ TDistCommandProcessor }

procedure TDistCommandProcessor.InitRuntimeProgress;
begin
  ARP := TRuntimeProgress.Create(Application);
  {$IFDEF VISUAL_BUILD}
  ARP.DataFileName  :=  MegaForm.DataFileName;
  ARP.DataTitle     :=  MegaForm.DataTitle;
  {$ELSE}
  ARP.DataFileName  :=  D_MegaMain.DataFileName;
  ARP.DataTitle     :=  D_MegaMain.DataTitle;
  {$ENDIF}
end;

procedure TDistCommandProcessor.ShowRuntimeProgress;
begin
  ARP.Show;
  MAI.ARP.WriteAnalysisOptionsToStdOut;
  ARP.AddRunStatusInfo('Status', 'Preparing data');
  ARP.Refresh;
end;

procedure TDistCommandProcessor.InitAnalysisInfo;
begin
  {$IFNDEF VISUAL_BUILD}
  if UsrOperation = dtdoPhyloQ then
    MAI := TPhyloQAnalysisInfo.Create
  else
    MAI := TAnalysisInfo.Create;
  {$ELSE}
  MAI := TAnalysisInfo.Create;
  MAI.DataFilename := MegaForm.DataFilename;
  {$ENDIF}
  MAI.MyProcessPack := ProcessPack;
  MAI.PropagateConstraints := (not ProcessPack.HasProcessType(ppSkipMinMax));
  MAI.InitialUsrOperation := UsrOperation;
  MAI.ARP := ARP;
  ARP.FMAI := MAI;
end;

procedure TDistCommandProcessor.CheckForSelectionTest;
begin
 if (UsrOperation = dtdoSelectionZTest) or (UsrOperation = dtdoSelectionExactTest) then
 begin
   IsSelTest := True;
   if MAI.MyDistPack.DoesContain(gdPairwise) then
     UsrOperation := dtdoPairwiseDist
   else if MAI.MyDistPack.DoesContain(gdOverallMean) then
     UsrOperation := dtdoOverallMean
   else if MAI.MyDistPack.DoesContain(gdWithinGroupMean) then
     UsrOperation := dtdoWithinGroupMean;
 end;
end;

procedure TDistCommandProcessor.CheckForEnoughCommonSites;
begin
  case UsrOperation of
    dtdoPairwiseDist,
    dtdoWithinGroupMean,
    dtdoBetweenGroupMean,
    dtdoNetGroupMean,
    dtdoOverallMean,
    dtdoAvgDiversityWithinSubPops,
    dtdoAvgDiversityForEntirePop,
    dtdoInterPopDiversity,
    dtdoPropOfInterPopDiversity:
      begin
        if MAI.MyNoOfSites < 1 then
        begin
          if Assigned(ARP) then
            ARP.Hide;
          RaiseErrorMessage(HC_No_Common_Sites, 'No common sites were found. ' + LineEnding + NoCommonSitesStr);
        end;
      end;
    dtdoDisparityIndexTest, dtdoCompositionDistance, dtdoDisparityIndex:
      begin
        if MAI.MyNoOfSites < 10 then
        begin
          if Assigned(ARP) then
            ARP.Hide;
          RaiseErrorMessage(HC_No_Common_Sites, 'Less than 10 common sites found! ' +LineEnding + NoCommonSitesStr);
        end;
      end;
  end;
end;

procedure TDistCommandProcessor.SubsetTheData;
begin
  MAI.MyMappedData := TList.Create;
  D_InputSeqData.ARP := ARP;
  D_InputSeqData.PrepareDataForDistAnalysis(MAI.MySubsetOpt, MAI.MyMappedData, MAI.MyUsedOtuInfos, MAI.MyNoOfSeqs, MAI.MyNoOfSites, MAI.MyLabelsUsed, MAI.RecodeScheme, MAI.SiteCoverage);
  CheckForEnoughCommonSites;
  ARP.AddAnalysisOptions('No. of Sites', IntToStr(MAI.MyNoOfSites));
end;

function TDistCommandProcessor.LaunchDistCommandThread: Boolean;
var
  aThread: TDistCommandThread = nil;
begin
  ARP.WriteAnalysisOptionsToStdOut;
  Result := True;
  aThread := TDistCommandThread.Create(MAI, UsrOperation);
  aThread.IsDisparityTest := IsDisparityTest;
  aThread.IsSelTest := IsSelTest;
  {$IFDEF VISUAL_BUILD}
   aThread.OnTerminate := MegaForm.DistCommandThreadDone;
  {$ELSE}
  aThread.OnTerminate := D_MegaMain.DistCommandThreadDone;
  {$ENDIF}
  aThread.Start;
end;

constructor TDistCommandProcessor.Create;
begin
  FIsWorkflowCalculation := False;
  ARP := nil;
  IsSelTest := False;
  IsDisparityTest := False;
  MAI := nil;
  FLog := TStringList.Create;
end;

destructor TDistCommandProcessor.Destroy;
begin
  inherited Destroy;
end;

function TDistCommandProcessor.ProcessDistanceCommand(aUsrOperation: TDistTreeDlgOption; aProcessPack: TProcessPack): Boolean;
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
      CheckForSelectionTest;
      ShowRuntimeProgress;
      SubsetTheData;
      if not LaunchDistCommandThread then
        raise Exception.Create('failed to launch distance calculation thread');
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

function TDistCommandProcessor.ProcessDisparityIndexTestCommand(aUsrOperation: TDistTreeDlgOption; aProcessPack: TProcessPack): Boolean;
begin
  IsDisparityTest := True;
  Result := ProcessDistanceCommand(aUsrOperation, aProcessPack);
end;

end.


