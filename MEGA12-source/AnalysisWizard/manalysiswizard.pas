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

unit MAnalysisWizard;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF VISUAL_BUILD}
  Dialogs, Controls, Forms, MEditorForm, ExtCtrls, applinkeroptions, MTreeList, ITimeTree,
  {$ELSE}
  KeywordConsts,
  {$ENDIF}
  LCLIntf, LCLType, manalysissettings,
  MegaUtils, MAnalysisInfo, MProcessPack, MDistPack, Classes,
  SysUtils, MD_InputSeqData, MegaConsts, AppLinker;

const
  IQ_TREE_ICON_INDEX = 8;

type
  TWizardFormUpdate = procedure(Step: TTtProcessStep) of object;
  TTimeTreeStartFrom = (ttsMegaMain, ttsTreeExplorer);

  { TAnalysisWizard }

  TAnalysisWizard = class(TObject)
    private
      FHumanReadableArgs: TStringList;
      FUsrOperation: TDistTreeDlgOption;
      FWizardMode: TAnalysisWizardMode;
      FAnalysisType: TAnalysisWizardAnalysis;
      FDataType: String;
      {$IFDEF VISUAL_BUILD}
      FAppLinkOptionsManager: TApplinkOptionsManager;
      {$ENDIF}
      FAppOptions: TStringList;
      procedure OnLoadTreeFinished;
      function LaunchHyphyAnalysis: Boolean;
      procedure CreateNexusFile(Filename: String; AnalysisInfo: TAnalysisInfo);
      function CreatePartitionsFile(filename: String; AnalysisInfo: TAnalysisInfo): Boolean;
      function SelectedGeneticCode: String;
      procedure InitProgress;
      procedure SetDataType(AValue: String);
      procedure ValidateSequenceData;
      procedure SetWizardMode(const Value: TAnalysisWizardMode);
      procedure SetUsrOperation(const Value: TDistTreeDlgOption);
      procedure SetAnalysisType(const Value: TAnalysisWizardAnalysis);
      procedure SeqDataInfoDoneCallback(Sender: TObject);
      procedure SeqDataInfoCancelCallback(Sender: TObject);
      procedure ShowFile(filename: String; newName: String);
      procedure InitSettingsValidator;
    public
      ProcessStep: TTtProcessStep;
      LaunchedFrom: TTimeTreeStartFrom;
      AnalysisInfo: TAnalysisInfo;
      ProcessPack: TProcessPack;
      WizardFormUpdate: TWizardFormUpdate;
      FSkipLoadTree: Boolean;
      FIsModelSel: Boolean;
      FIsTreeInf: Boolean;
      SkipTreeActionActivated: Boolean;

      constructor Create;
      destructor Destroy; override;
      procedure Reset;
      procedure CheckResetUserTree;
      procedure InitAnalysisInfo;

      function LoadTree: Boolean;
      function LoadSeqData: Boolean;
      function GetAnalysisPreferences: Boolean;
      function LaunchAnalysis: Boolean;
      property UsrOperation: TDistTreeDlgOption read FUsrOperation write SetUsrOperation;
      property WizardMode: TAnalysisWizardMode read FWizardMode write SetWizardMode;
      property AnalysisType: TAnalysisWizardAnalysis read FAnalysisType write SetAnalysisType;
      property DataType: String read FDataType write SetDataType;
  end;

{$IFNDEF VISUAL_BUILD}
var
  AnalysisWizardCC: TAnalysisWizard;
{$ENDIF}

implementation

uses
  {$IFDEF VISUAL_BUILD}
   mega_main, htmloptionsdlg, Graphics, app_options_frame, MAnalysisPrefDlg, mtreeloaders,
  MFileUtils, Menus,
  {$ELSE}
  MegaUtils_NV, MD_MegaMain, mparse_mao_file,
  {$ENDIF}
  MTreePack, MRuntimeProgressDlg, MegaErrUtils, ErrorMessages_HC, ProcessCodonOmegaCmds,
  mstringbuilder, math, ShowTrees, mcrossprojectutils, MegaAnalysisPrefStrings;

{ TAnalysisWizard }

constructor TAnalysisWizard.Create;
begin
  FHumanReadableArgs := TStringList.Create;
  WizardMode := awmSeqData;
  AnalysisInfo := nil;
  ProcessPack := nil;
end;

destructor TAnalysisWizard.Destroy;
begin
  if Assigned(FHumanReadableArgs) then
    FHumanReadableArgs.Free;
  inherited;
end;

procedure TAnalysisWizard.InitAnalysisInfo;
{$IFDEF VISUAL_BUILD}
var
  Progress: TRuntimeProgress;
{$ENDIF}
begin
  {$IFDEF VISUAL_BUILD}
  AnalysisInfo := TAnalysisInfo.Create;
  AnalysisInfo.DataFilename := MegaForm.DataFileName;
  Progress := TRuntimeProgress.Create(Application);
  Progress.DataFileName := MegaForm.DataFileName;
  Progress.DataTitle :=  MegaForm.DataTitle;
  AnalysisInfo.ARP := Progress;
  Progress.FMAI := AnalysisInfo;
  AnalysisInfo.MyProcessPack := ProcessPack;
  AnalysisInfo.MyTreePack := TTreePack.Create;
  AnalysisInfo.InitialUsrOperation := FUsrOperation;
  case FUsrOperation of
    dtdoMLCodonOmega: AnalysisInfo.SetupUsedOtuInfos(dtdoMLCodonOmega);
  end;
  AnalysisInfo.MyOriTreeList := TTreeList.Create;
  if not Assigned(AnalysisInfo.MyOtuNames) then
    AnalysisInfo.MyOtuNames := TStringList.Create;
  {$ENDIF}
end;


procedure TAnalysisWizard.InitProgress;
begin
  {$IFDEF VISUAL_BUILD}Application.ProcessMessages;{$ENDIF}
  AnalysisInfo.ARP.WriteAnalysisOptionsToStdOut;
  AnalysisInfo.ARP.AddRunStatusInfo('Status', 'Preparing data');
  AnalysisInfo.ARP.Show;
end;

procedure TAnalysisWizard.SetDataType(AValue: String);
begin
  if FDataType = AValue then Exit;
  FDataType := AValue;
end;

function TAnalysisWizard.LaunchAnalysis: Boolean;
begin
  case FUsrOperation of
    dtdoMLCodonOmega: Result := LaunchHyphyAnalysis;
    else
      raise Exception.Create('Invalid method called');
  end;
end;

function TAnalysisWizard.LaunchHyphyAnalysis: Boolean;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  try
    AnalysisInfo.MyUsrOperation := dtdoMLCodonOmega;
    ExecuteCodonOmegaCommand(AnalysisInfo);
    Result := True;
  except
    on E:Exception do
      ShowMessage('Oh no! Failed to launch HyPhy analysis: ' + E.Message);
  end;
  {$ENDIF}
end;


procedure TAnalysisWizard.CreateNexusFile(Filename: String;AnalysisInfo: TAnalysisInfo);
var
  SeqTypeAA: boolean;
  NexusList: TStringList = nil;
begin
  try
    begin
      AnalysisInfo.MySeqStrings := TStringList.Create;
      AnalysisInfo.MyIncludedSites := D_InputSeqData.PrepareDataForMLAnalysis(AnalysisInfo);
    end;
  finally
    if Assigned(NexusList) then
      NexusList.Free;
  end;
end;

function TAnalysisWizard.CreatePartitionsFile(filename: String; AnalysisInfo: TAnalysisInfo): Boolean;
var
  aFile: TextFile;
begin
  Result := False;
  try
    AssignFile(aFile, filename);
    Rewrite(aFile);
    WriteLn(aFile, '#NEXUS');
    WriteLn(aFile, 'BEGIN SETS;');
    WriteLn(aFile, Format('      charset part1 = %s, %d-%d;', [SelectedGeneticCode, 1, AnalysisInfo.MyNoOfSites]));
    WriteLn(aFile, '');
    WriteLn(aFile, 'END;');
  finally
    CloseFile(aFile);
  end;
  Result := FileExists(filename);
end;

function TAnalysisWizard.SelectedGeneticCode: String;
begin
  Result := EmptyStr;
  if Assigned(FAppOptions) and (FAppOptions.Count > 0) then
    if FAppOptions.IndexOfName('-code-table') >= 0 then
      Result := FAppOptions.Values['-code-table'];
end;


function TAnalysisWizard.LoadSeqData: Boolean;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
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
  if AnalysisType=ttaHyPhy then
     WizardFormUpdate(ttpsDoSettings)
  else
      WizardFormUpdate(ttpsLoadTree);
  {$ENDIF}
end;

function TAnalysisWizard.LoadTree: Boolean;
{$IFDEF VISUAL_BUILD}
var
  TreeLoader: ITreeLoader;
{$ENDIF}
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  try
    if not Assigned(AnalysisInfo) then
      InitAnalysisInfo;
    TreeLoader := TGuiTreeLoader.Create;
    if not TreeLoader.LoadTree(AnalysisInfo) then
    begin
      ShowMessage('Failed to load tree: ' + TreeLoader.GetMsg);
      WizardFormUpdate(ttpsLoadTree);
    end
    else
    begin
      LaunchedFrom := ttsMegaMain;
      FUsrOperation := AnalysisInfo.InitialUsrOperation;
      Result := True;
      SwitchDirectory(AnalysisInfo.MyUserTreeFile);
      OnLoadTreeFinished;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
      WizardFormUpdate(ttpsLoadTree);
    end;
  end;
  {$ENDIF}
end;

procedure TAnalysisWizard.OnLoadTreeFinished;
begin
  if (AnalysisType = ttaHyPhy) or (AnalysisType = ttaIQTree) then
    WizardFormUpdate(ttpsDoSettings);
end;


procedure TAnalysisWizard.Reset;
begin
  if Assigned(AnalysisInfo) then
  begin
    AnalysisInfo.MyProcessPack := nil;
    FreeAndNil(AnalysisInfo);
  end;
end;

procedure TAnalysisWizard.CheckResetUserTree;
begin
  if Assigned(AnalysisInfo) then
  begin
    if AnalysisInfo.MyUserTreeFile <> EmptyStr then
    begin
      AnalysisInfo.MyUserTreeFile := EmptyStr;
      if Assigned(AnalysisInfo.MyOriTreeList) then
      begin
        AnalysisInfo.MyOriTreeList.Free;
        AnalysisInfo.MyOriTreeList := nil;
      end;
    end;
  end;
end;

function TAnalysisWizard.GetAnalysisPreferences: Boolean;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  if AnalysisType = ttaHyPhy then
  begin
    if AnalysisInfo.GetAnalysisOptions(dtdoMLCodonOmega) then
    begin
      WizardFormUpdate(ttpsLaunchAnalysis);
      Result := True;
    end
    else
    begin
      WizardFormUpdate(ttpsDoSettings);
      Result := True;
    end;
    Exit;
  end;

  Result := False;
  if (SkipTreeActionActivated) then
     AnalysisPrefDlg.SkipTreeActionActivated := True
  else
      AnalysisPrefDlg.TreeFileNameEdit.Text := AnalysisInfo.MyUserTreeFile;
  Result := AnalysisInfo.GetMegaAndApplinkerOptions(FUsrOperation);
  AnalysisPrefDlg.ClearAppOptions;
  if Result then
  begin
    if AnalysisType = ttaIQTree then
    begin
      if (MD_InputSeqData.D_InputSeqData.FNoOfTaxa < 3) then
      begin
        ShowMessage('Sequence data must have at least three sequences to run IQ-TREE2 analysis.');
        Result := False;
        Exit;
      end;
      if (MD_InputSeqData.D_InputSeqData.FNoOfSites < 1) then
      begin
        ShowMessage('Sequence data must have at least one site to run IQ-TREE2 analysis.');
        Result := False;
        Exit;
      end;
      WizardFormUpdate(ttpsLaunchAnalysis)
    end
    else
       WizardFormUpdate(ttpsLaunchAnalysis);
    Result := True;
  end
  else
  begin
    WizardFormUpdate(ttpsDoSettings);
    Result := True;
  end;
  {$ENDIF}
end;

procedure TAnalysisWizard.SetAnalysisType(const Value: TAnalysisWizardAnalysis);
begin
  FAnalysisType := Value;
end;

procedure TAnalysisWizard.SeqDataInfoDoneCallback(Sender: TObject);
begin
  if AnalysisType =  ttaHyPhy then
  begin
     InitAnalysisInfo;
     WizardFormUpdate(ttpsDoSettings);
  end
  else
     WizardFormUpdate(ttpsLoadTree);
end;

procedure TAnalysisWizard.SeqDataInfoCancelCallback(Sender: TObject);
begin
  WizardFormUpdate(ttpsLoadSeqData);
end;

procedure TAnalysisWizard.ShowFile(filename: String; newName: String);
{$IFNDEF VISUAL_BUILD}
var
  aList : TStringList = nil;
{$ENDIF}
begin
  {$IFDEF VISUAL_BUILD}
  OpenFileAndFocus(filename, 0, 0);
  {$ELSE}
  try
    aList := TStringList.Create;
    aList.LoadFromFile(filename);
    aList.SaveToFile(NextAvailableFilenameNV(newName));
  finally
    if Assigned(aList) then
      aList.Free;
  end;
  {$ENDIF}
end;

procedure TAnalysisWizard.InitSettingsValidator;
begin
end;


procedure TAnalysisWizard.SetUsrOperation(const Value: TDistTreeDlgOption);
begin
  FUsrOperation := Value;
end;

procedure TAnalysisWizard.SetWizardMode(const Value: TAnalysisWizardMode);
begin
  FWizardMode := Value;
end;

procedure TAnalysisWizard.ValidateSequenceData;
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
