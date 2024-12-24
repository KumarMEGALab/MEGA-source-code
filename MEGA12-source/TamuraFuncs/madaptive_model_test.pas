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

unit madaptive_model_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MTreeData, MLTree, fgl, MLModels, mmodel_info_list,
  MLTreeAnalyzer, MAnalysisInfo, MegaConsts, MegaUtils_NV, mruntimeprogressdlg, mstringbuilder;

type
  TGammaRateVariationModelList = specialize TFPGList<TGammaRateVariationModel>;

  { TAdaptiveModelTestThread }

  TAdaptiveModelTestThread = class(TMegaThread)
    private
      FStringBuilder: TMegaStringBuilder;
      FBootTable: PArrayOfInt;
      FIsCancelled: Boolean;
      FIsDna: Boolean;
      FIsModelTamer: Boolean;
      FNumRates: Integer;
      FNumThreads: Integer;
      FOrigNumSites: Integer;
      FTargetBicDelta: Double;
      FTargetAicDelta: Double;
      FTestBaseModelsOnly: Boolean;
      FTotalModelsEvaluated: Integer;
      function GetNumBestModels: Integer;
    protected
      FNumPrepopulatedModels: Integer;
      FDevMsg: String;
      FNumSkipped: Integer;
      FNumRemoved: Integer;
      FStatus: String;
      FInfo: String;
      FInitialTree: TTreeData;
      FProgressDlg: TRuntimeProgress;
      FAnalysisInfo: TAnalysisInfo;
      FLog: TStringList;
      FAllModels: TGammaRateVariationModelList;

      { the 3 lists below whose name starts with P only hold pointers to models in other lists so models should not be free by referencing these pointers}
      PBaseModels: TGammaRateVariationModelList;
      PGammaInvarModels: TGammaRateVariationModelList;
      PFrequencyModels: TGammaRateVariationModelList;

      FFilteredModels: TModelInfoList;
      FBestModels: TModelInfoList;
      FCurrentBestModel: TModelInfo;
      function MaxModels: Integer;
      function HaveSameBaseModel(info1: TModelInfo; info2: TModelInfo): Boolean;
      procedure ReplaceFilteredModels;
      function BestModelsString: String;
      procedure UpdateLog(aMsg: String);
      procedure ClearModels;
      procedure InitDnaModels;
      procedure InitProteinModels;
      procedure TestBaseModels;
      procedure TestGammaInvarModels;
      procedure TestFrequencyModels;
      function CompareFrequencyModelToBaseModel(freqModel: TModelInfo; baseModel: TModelInfo): Integer;
      function BaseModelName(aModelName: String): String;
      procedure RemoveFromGammaInvarModels(aModelName: String);
      function IsInBestModelInfoList(aModel: TGammaRateVariationModel; checkPlusF: Boolean): Boolean;
      procedure Execute; override;
      procedure UpdateRunStatusInfo(aStatus: String; aInfo: String);
      procedure DoUpdateRunStatusInfo;
      function KeepModel(aModelInfo: TModelInfo): Boolean;
      procedure MyWriteToDevConsole(aMsg: String);
      procedure DoWriteToDevConsole;
      procedure ClearRunStatusInfo;
      function BestModelsStr: String;
      function GammaInvarModelsStr: String;
      function FreqModelsStr: String;
      procedure HideProgressDialog;
      procedure DoHideProgressDialog;
      function AnalysisDescription: String; override;
    public
      LoggingProc: TSendMessageProc;
      constructor Create(aInfo: TAnalysisInfo);
      constructor CreateFromModelInfoList(aInfo: TAnalysisInfo; aModels: TModelInfoList);
      destructor Destroy; override;
      function GenerateCaption(numEvaluated: Integer; numCandidates: Integer): String;
      property TargetBicDelta: Double read FTargetBicDelta;
      property TargetAicDelta: Double read FTargetAicDelta;
      property IsDna: Boolean read FIsDna;
      property NumRates: Integer read FNumRates;
      property NumThreads: Integer read FNumThreads;
      property Log: TStringList read FLog;
      property IsCancelled: Boolean read FIsCancelled;
      property ProgressDlg: TRuntimeProgress read FProgressDlg write FProgressDlg;
      property AnalysisInfo: TAnalysisInfo read FAnalysisInfo;
      property OrigNumSites: Integer read FOrigNumSites write FOrigNumSites;
      property IsModelTamer: Boolean read FIsModelTamer write FIsModelTamer;
      property BootTable: PArrayOfInt read FBootTable write FBootTable;
      property NumBestModels: Integer read GetNumBestModels;
      property TestBaseModelsOnly: Boolean read FTestBaseModelsOnly write FTestBaseModelsOnly;
      property TotalModelsEvaluated: Integer read FTotalModelsEvaluated;
  end;

implementation

uses
  {$IFDEF DEBUG}mdeveloper_console,{$ENDIF}
  math, MLSearchThread, MD_InputSeqData, mmodel_test_results, megautils, MegaAnalysisPrefStrings;

{ TAdaptiveModelTestThread }

function TAdaptiveModelTestThread.GetNumBestModels: Integer;
begin
  if Assigned(FBestModels) then
    Result := FBestModels.Count
  else
    Result := 0;
end;

function TAdaptiveModelTestThread.MaxModels: Integer;
begin
  if FAnalysisInfo.isAminoAcid then
    Result := 8*8 { 8 base models each with itself and +G, +I, +F, +G+I, +G+F, +G+I+F +I+F derivatives}
  else
    Result := 6*4; { 6 base models each with itself and +G, +I, +G+I derivatives}
end;

function TAdaptiveModelTestThread.HaveSameBaseModel(info1: TModelInfo; info2: TModelInfo): Boolean;
begin
  Result := (BaseModelName(info1.ModelName) = BaseModelName(info2.ModelName));
end;

function TAdaptiveModelTestThread.GenerateCaption(numEvaluated: Integer; numCandidates: Integer): String;
begin
  Result := Format('To enhance computational efficiency, ML calculations were omitted for %d substitution models deemed likely suboptimal based on BIC and AICc values of base models', [numCandidates - numEvaluated]);
end;

procedure TAdaptiveModelTestThread.ReplaceFilteredModels;
var
  i: Integer = -1;
begin
  if FFilteredModels.Count > 0 then
  begin
    for i := 0 to FFilteredModels.Count - 1 do
      FBestModels.Add(FFilteredModels[i]);
    FBestModels.Sort(@CompBIC);
    FFilteredModels.Clear;
  end;
end;

function TAdaptiveModelTestThread.BestModelsString: String;
var
  i: Integer = -1;
  aInfo: TModelInfo = nil;
begin
  Result := EmptyStr;
  if FBestModels.Count > 0 then
    for i := 0 to FBestModels.Count - 1 do
    begin
      aInfo := FBestModels[i];
      Result += aInfo.ModelName;
      if i <> (FBestModels.Count - 1) then
        Result += ', '
    end;
end;

procedure TAdaptiveModelTestThread.UpdateLog(aMsg: String);
var
  aLogStr: String = '';
begin
  if Assigned(LoggingProc) then
  begin
    LoggingProc(Format('--- %s', [aMsg]));
  end
  else
  begin
    if FLog.Count = 0 then
      FLog.Add(Format('%-20s %s', ['Time', 'Info']));
    aLogStr := Format('%-20s %s', [FormatDateTime('YYYY-MM-DD hh:mm:ss', Now), aMsg]);
    FLog.Add(aLogStr);
  end;
end;

procedure TAdaptiveModelTestThread.ClearModels;
var
  i: Integer = -1;
begin
  { these first 3 lists only hold pointers}
  PBaseModels.Clear;
  PGammaInvarModels.Clear;
  if Assigned(PFrequencyModels) then
    PFrequencyModels.Clear;

  { the lists below contain the actual instances}
  if FFilteredModels.Count > 0 then
    for i := FFilteredModels.Count - 1 downto 0 do
    begin
      if Assigned(FFilteredModels[i]) then
      begin
        FFilteredModels[i].Free;
        FFilteredModels.Delete(i);
      end;
    end;
  FFilteredModels.Clear;

  if FBestModels.Count > 0 then
    for i := FBestModels.Count - 1 downto 0 do
    begin
      if Assigned(FBestModels[i]) then
      begin
        FBestModels[i].Free;
        FBestModels.Delete(i);
      end;
    end;
  FBestModels.Clear;

  if FAllModels.Count > 0 then
    for i := FAllModels.Count - 1 downto 0 do
    begin
      if Assigned(FAllModels[i]) then
      begin
        FAllModels[i].Free;
        FAllModels.Delete(i);
      end;
    end;
  FAllModels.Clear;
end;

procedure TAdaptiveModelTestThread.InitDnaModels;
var
  i: Integer = -1;
  index: Integer = -1;
begin
  UpdateLog('Initializing base DNA models');
  { base models}
  FAllModels.Add(TGTRModel.Create(0, False, 1));
  FAllModels.Add(TTN93Model.Create(0, False, 1));
  FAllModels.Add(THKYModel.Create(0, False, 1));
  FAllModels.Add(TT3Model.Create(0, False, 1));
  FAllModels.Add(TK2Model.Create(0, False, 1));
  FAllModels.Add(TJCModel.Create(0, False, 1));
  for i := 0 to FAllModels.Count - 1 do
    PBaseModels.Add(FAllModels[i]);
  Assert(PBaseModels.Count = 6, Format('expected 6 models but got %d', [PBaseModels.Count]));

  if FTestBaseModelsOnly then
  begin
    UpdateLog('Skipping +G and +I models for now');
    Exit;
  end;

  index := FAllModels.Count;
  FAllModels.Add(TGTRModel.Create(0, True,  1));
  FAllModels.Add(TGTRModel.Create(0, False, FNumRates));
  FAllModels.Add(TGTRModel.Create(0, True, FNumRates));
  FAllModels.Add(TTN93Model.Create(0, True,  1));
  FAllModels.Add(TTN93Model.Create(0, False, FNumRates));
  FAllModels.Add(TTN93Model.Create(0, True, FNumRates));
  FAllModels.Add(THKYModel.Create(0, True,  1));
  FAllModels.Add(THKYModel.Create(0, False, FNumRates));
  FAllModels.Add(THKYModel.Create(0, True, FNumRates));
  FAllModels.Add(TT3Model.Create(0, True,  1));
  FAllModels.Add(TT3Model.Create(0, False, FNumRates));
  FAllModels.Add(TT3Model.Create(0, True, FNumRates));
  FAllModels.Add(TK2Model.Create(0, True,  1));
  FAllModels.Add(TK2Model.Create(0, False, FNumRates));
  FAllModels.Add(TK2Model.Create(0, True, FNumRates));
  FAllModels.Add(TJCModel.Create(0, True,  1));
  FAllModels.Add(TJCModel.Create(0, False, FNumRates));
  FAllModels.Add(TJCModel.Create(0, True, FNumRates));
  for i := index to FAllModels.Count - 1 do
    PGammaInvarModels.Add(FAllModels[i]);
  Assert(PGammaInvarModels.Count = 18, Format('expected 18 models but got %d', [PGammaInvarModels.Count]));
end;

procedure TAdaptiveModelTestThread.InitProteinModels;
var
  i: Integer = -1;
  index: Integer = -1;
begin
  UpdateLog('Initializing base protein models');
  FAllModels.Add(TDayhoffModel.Create(0,false,false,1));
  FAllModels.Add(TJTTModel.Create(0, false,false, 1));
  FAllModels.Add(TWAGModel.Create(0, false,false, 1));
  FAllModels.Add(TLGModel.Create(0, false,false, 1));
  FAllModels.Add(TmtREV24Model.Create(0, false, false, 1));
  FAllModels.Add(TcpREVModel.Create(0, false, false, 1));
  FAllModels.Add(TrtREVModel.Create(0, false, false, 1));
  FAllModels.Add(TPoissonModel.Create(0, false, false, 1));
  for i := 0 to FAllModels.Count - 1 do
    PBaseModels.Add(FAllModels[i]);

  if FTestBaseModelsOnly then
  begin
    UpdateLog('skipping +F, +G, and +I models for now');
    Exit;
  end;

  index := PBaseModels.Count;
  FAllModels.Add(TDayhoffModel.Create(0,false,true,1));
  FAllModels.Add(TJTTModel.Create(0, false, true, 1));
  FAllModels.Add(TWAGModel.Create(0, false,true, 1));
  FAllModels.Add(TLGModel.Create(0, false,true, 1));
  FAllModels.Add(TmtREV24Model.Create(0, false, true, 1));
  FAllModels.Add(TcpREVModel.Create(0, false, true, 1));
  FAllModels.Add(TrtREVModel.Create(0, false, true, 1));
  FAllModels.Add(TPoissonModel.Create(0, false, true, 1));
  for i := index to FAllModels.Count - 1 do
    PFrequencyModels.Add(FAllModels[i]);

  index := FAllModels.Count;
  FAllModels.Add(TDayhoffModel.Create(0, true,false, 1));
  FAllModels.Add(TDayhoffModel.Create(0, false,false, FNumRates));
  FAllModels.Add(TDayhoffModel.Create(0, true,false, FNumRates));
  FAllModels.Add(TDayhoffModel.Create(0, true,true, 1));
  FAllModels.Add(TDayhoffModel.Create(0, false,true, FNumRates));
  FAllModels.Add(TDayhoffModel.Create(0, true,true, FNumRates));

  FAllModels.Add(TJTTModel.Create(0, true,false, 1));
  FAllModels.Add(TJTTModel.Create(0, false, false, FNumRates));
  FAllModels.Add(TJTTModel.Create(0, true, false, FNumRates));
  FAllModels.Add(TJTTModel.Create(0, true, true, 1));
  FAllModels.Add(TJTTModel.Create(0, false, true, FNumRates));
  FAllModels.Add(TJTTModel.Create(0, true, true, FNumRates));

  FAllModels.Add(TWAGModel.Create(0, true,false, 1));
  FAllModels.Add(TWAGModel.Create(0, false,false, FNumRates));
  FAllModels.Add(TWAGModel.Create(0, true,false, FNumRates));
  FAllModels.Add(TWAGModel.Create(0, true,true, 1));
  FAllModels.Add(TWAGModel.Create(0, false,true, FNumRates));
  FAllModels.Add(TWAGModel.Create(0, true,true, FNumRates));

  FAllModels.Add(TLGModel.Create(0, true,false, 1));
  FAllModels.Add(TLGModel.Create(0, false,false, FNumRates));
  FAllModels.Add(TLGModel.Create(0, true,false, FNumRates));
  FAllModels.Add(TLGModel.Create(0, true,true, 1));
  FAllModels.Add(TLGModel.Create(0, false,true, FNumRates));
  FAllModels.Add(TLGModel.Create(0, true,true, FNumRates));

  FAllModels.Add(TmtREV24Model.Create(0, true, false, 1));
  FAllModels.Add(TmtREV24Model.Create(0, false, false, FNumRates));
  FAllModels.Add(TmtREV24Model.Create(0, true, false, FNumRates));
  FAllModels.Add(TmtREV24Model.Create(0, true, true, 1));
  FAllModels.Add(TmtREV24Model.Create(0, false, true, FNumRates));
  FAllModels.Add(TmtREV24Model.Create(0, true, true, FNumRates));

  FAllModels.Add(TcpREVModel.Create(0, true, false, 1));
  FAllModels.Add(TcpREVModel.Create(0, false, false, FNumRates));
  FAllModels.Add(TcpREVModel.Create(0, true, false, FNumRates));
  FAllModels.Add(TcpREVModel.Create(0, true, true, 1));
  FAllModels.Add(TcpREVModel.Create(0, false, true, FNumRates));
  FAllModels.Add(TcpREVModel.Create(0, true, true, FNumRates));

  FAllModels.Add(TrtREVModel.Create(0, true, false, 1));
  FAllModels.Add(TrtREVModel.Create(0, false, false, FNumRates));
  FAllModels.Add(TrtREVModel.Create(0, true, false, FNumRates));
  FAllModels.Add(TrtREVModel.Create(0, true, true, 1));
  FAllModels.Add(TrtREVModel.Create(0, false, true, FNumRates));
  FAllModels.Add(TrtREVModel.Create(0, true, true, FNumRates));

  FAllModels.Add(TPoissonModel.Create(0, true, false, 1));
  FAllModels.Add(TPoissonModel.Create(0, false, false, FNumRates));
  FAllModels.Add(TPoissonModel.Create(0, true, false, FNumRates));
  FAllModels.Add(TPoissonModel.Create(0, true, true, 1));
  FAllModels.Add(TPoissonModel.Create(0, false, true, FNumRates));
  FAllModels.Add(TPoissonModel.Create(0, true, true, FNumRates));
  for i := index to FAllModels.Count - 1 do
    PGammaInvarModels.Add(FAllModels[i]);
end;

procedure TAdaptiveModelTestThread.TestBaseModels;
var
  aThread: TModelTestThread = nil;
  i: Integer = -1;
  aInfo: TModelInfo = nil;
  aModels: TGammaRateVariationModelArray;
begin
  try
    if FAnalysisInfo.isAminoAcid then
      UpdateLog(Format('testing %d base amino acid models for best-fit based on BIC and AICc', [PBaseModels.Count]))
    else
      UpdateLog(Format('testing %d base nucleotide models for best-fit based on BIC and AICc', [PBaseModels.Count]));

    SetLength(aModels, PBaseModels.Count);
    for i := 0 to PBaseModels.Count - 1 do
      aModels[i] := PBaseModels[i];
    aThread := TModelTestThread.CreateForAdaptiveModelTest(aModels, FNumThreads);
    aThread.SkipSummaryUpdate := True;
    inc(FTotalModelsEvaluated, Length(aModels));
    if FAnalysisInfo.isAminoAcid then
      aThread.ProgressMessage := 'Testing base protein models'
    else
      aThread.ProgressMessage := 'Testing base DNA models';
    if FIsModelTamer then
    begin
      Assert(FOrigNumSites > 0, Format('bad count for orig num sites = %d', [FOrigNumSites]));
      aThread.OrigNumSites := FOrigNumSites;
      Assert(Assigned(FBootTable), 'missing boot table');
      aThread.BootTable := FBootTable;
    end;
    aThread.FreeOnTerminate := False;
    aThread.ProgressDlg := FAnalysisInfo.ARP;
    aThread.ShowProgress := True;
    FAnalysisInfo.ARP.Thread := aThread;

    aThread.Start;
    aThread.WaitFor;
    if aThread.Canceled then
      raise EAbort.Create('model selection analysis cancelled');
    if not aThread.IsSuccess then
      raise Exception.Create(aThread.MessagesLog.Text);
    for i := 0 to aThread.MLTreeAnalyzer.ModelInfoList.Count - 1 do
    begin
      aInfo := TModelInfo.Create;
      aInfo.Assign(aThread.MLTreeAnalyzer.ModelInfoList[i]);
      UpdateLog(Format('evaluated %-10s BIC = %-10.0n  AICc = %.0n', [aInfo.ModelName, aInfo.BIC, aInfo.AICc]));
      FBestModels.Add(aInfo);
    end;
    FBestModels.Sort(@compBIC);
    FCurrentBestModel := FBestModels[0];
    UpdateRunStatusInfo(CURRENT_BEST_MODEL, FCurrentBestModel.FullName);
    UpdateLog('filtering base models not meeting BIC and AICc thresholds');
    if (not TestBaseModelsOnly) and (FBestModels.Count > 1) then
    begin
      for i := FBestModels.Count - 1 downto 1 do
      begin
        if not KeepModel(FBestModels[i]) then
        begin
          aInfo := FBestModels[i];
          FFilteredModels.Add(aInfo);
          FBestModels.Delete(i);
          UpdateLog(Format('excluding %-10s ΔBIC = %-8.2f  ΔAIC = %.2f', [aInfo.ModelName,aInfo.BIC - FCurrentBestModel.BIC, aInfo.AICc - FCurrentBestModel.AICc]));
          inc(FNumRemoved);
        end;
      end;
    end;
    UpdateLog('iteration completed (base)');
    if FBestModels.Count > 1 then
      UpdateLog(Format('current best base model is %s [BIC = %.0n]. Second best model is %s [BIC = %.0n', [FCurrentBestModel.ModelName, FCurrentBestModel.BIC, FBestModels[1].ModelName, FBestModels[1].BIC]))
    else
      UpdateLog(Format('current best base model is %s [BIC = %.0n].', [FCurrentBestModel.ModelName, FCurrentBestModel.BIC]));
    if not TestBaseModelsOnly then
    begin
      UpdateLog(Format('continuing with %d base models out of %d initial candidate base models', [FBestModels.Count, PBaseModels.Count]));
      UpdateLog(Format('models under consideration = %s', [FBestModels.ModelNames]));
    end
  finally
    if Assigned(aThread) then
      aThread.Free;
  end;
end;

procedure TAdaptiveModelTestThread.TestGammaInvarModels;
var
  aThread: TModelTestThread = nil;
  i: Integer = -1;
  index: Integer = 0;
  aInfo: TModelInfo = nil;
  aModels: TGammaRateVariationModelArray;

  function GetFullModelName(aModel: TGammaRateVariationModel): String;
  var
    m: TModelInfo = nil;
  begin
    try
      m := TModelInfo.Create;
      aModel.GetInfo(m);
      Result := m.ModelName;
    finally
      if Assigned(m) then
        m.Free;
    end;
  end;

begin
  Synchronize(@ClearRunStatusInfo);
  try
    if FAnalysisInfo.isAminoAcid then
      UpdateLog('testing amino acid models with +G +I for best-fit based on BIC and AICc')
    else
      UpdateLog('testing base nucleotide models with +G +I for best-fit based on BIC and AICc');
    UpdateLog('models included in test are: ' + BestModelsString);
    UpdateRunStatusInfo('Candidate Models', BestModelsString);
    SetLength(aModels, 0);
    for i := 0 to PGammaInvarModels.Count - 1 do
    begin
      if IsInBestModelInfoList(PGammaInvarModels[i], True) then
      begin
        SetLength(aModels, index + 1);
        aModels[index] := PGammaInvarModels[i];
        inc(index);
      end
      else
      begin
        UpdateLog(Format('skipping test for %-14s - already excluded', [GetFullModelName(PGammaInvarModels[i])]));
        inc(FNumSkipped);
      end;
    end;
    Assert(Length(aModels) >= 3, Format('at least 3 models needed but got ', [Length(aModels)]));
    aThread := TModelTestThread.CreateForAdaptiveModelTest(aModels, FNumThreads);
    aThread.SkipSummaryUpdate := True;
    inc(FTotalModelsEvaluated, Length(aModels));
    if FAnalysisInfo.isAminoAcid then
      aThread.ProgressMessage := 'Testing protein models with +G +I'
    else
      aThread.ProgressMessage := 'Testing DNA models with +G +I';
    if FIsModelTamer then
    begin
      aThread.OrigNumSites := FOrigNumSites;
      aThread.BootTable := FBootTable;
    end;
    aThread.FreeOnTerminate := False;
    aThread.ProgressDlg := FAnalysisInfo.ARP;
    aThread.ShowProgress := True;
    FAnalysisInfo.ARP.Thread := aThread;
    aThread.Start;
    aThread.WaitFor;

    if aThread.Canceled then
    begin
      aThread.Canceled := False;
      raise EAbort.Create('model selection analysis cancelled');
    end;
    if not aThread.IsSuccess then
      raise Exception.Create(aThread.MessagesLog.Text);
    for i := 0 to aThread.MLTreeAnalyzer.ModelInfoList.Count - 1 do
    begin
      aInfo := TModelInfo.Create;
      aInfo.Assign(aThread.MLTreeAnalyzer.ModelInfoList[i]);
      UpdateLog(Format('evaluated %-10s BIC = %-10.0n AICc=%.0n', [aInfo.ModelName, aInfo.BIC, aInfo.AICc]));
      FBestModels.AddByDataType(aInfo, FAnalysisInfo.isAminoAcid);
    end;
    FBestModels.Sort(@compBIC);
    FCurrentBestModel := FBestModels[0];
    UpdateRunStatusInfo(CURRENT_BEST_MODEL, FCurrentBestModel.FullName);
    UpdateLog('iteration completed (+G +I)');
    if FBestModels.Count > 1 then
      UpdateLog(Format('current best model is %s [BIC = %.0n]. Second best model is %s [BIC = %.0n]', [FCurrentBestModel.ModelName, FCurrentBestModel.BIC, FBestModels[1].ModelName, FBestModels[1].BIC]))
    else
      UpdateLog(Format('current best model is %s [BIC = %.0n].', [FCurrentBestModel.ModelName, FCurrentBestModel.BIC]));
    UpdateLog('filtering +G and +I models not meeting BIC and AICc thresholds');
    for i := FBestModels.Count - 1 downto 1 do
    begin
      if not KeepModel(FBestModels[i]) then
      begin
        aInfo := FBestModels[i];
        FFilteredModels.Add(aInfo);
        FBestModels.Delete(i);
        UpdateLog(Format('excluding %-10s ΔBIC = %-8.2f  ΔAIC = %.2f', [aInfo.ModelName,aInfo.BIC - FCurrentBestModel.BIC, aInfo.AICc - FCurrentBestModel.AICc]));
        inc(FNumRemoved);
      end;
    end;
    UpdateLog(Format('%d models were evaluated and %d models were skipped', [FTotalModelsEvaluated, FNumSkipped]));
    UpdateLog(Format('%d of the models tested were excluded due to a high BIC value (delta > %.2f) and high AICc value (delta > %.2f)', [FNumRemoved, TargetBicDelta, TargetAicDelta]));
    if FNumPrepopulatedModels = 0 then
      UpdateLog(Format('finalizing with %d models out of %d candidate models', [FAllModels.Count - FNumSkipped - FNumPrepopulatedModels, FAllModels.Count]));
    UpdateLog(Format('models under consideration = %s', [FBestModels.ModelNames]));
  finally
    if Assigned(aThread) then
      aThread.Free;
  end;
end;

procedure TAdaptiveModelTestThread.TestFrequencyModels;
var
  aThread: TModelTestThread = nil;
  i: Integer = -1;
  index: Integer = 0;
  freqModelInfo: TModelInfo = nil;
  baseModelInfo: TModelInfo = nil;
  aModels: TGammaRateVariationModelArray;
  aName: String = '';
  modelTest: Integer = -1;
begin
  Synchronize(@ClearRunStatusInfo);
  try
    UpdateLog('testing amino acid models with +F for best-fit based on BIC and AICc');
    UpdateLog('models included in test are: ' + BestModelsString);
    UpdateRunStatusInfo('Candidate Models', BestModelsString);
    SetLength(aModels, 0);
    for i := 0 to PFrequencyModels.Count - 1 do
    begin
      if IsInBestModelInfoList(PFrequencyModels[i], False) then
      begin
        SetLength(aModels, index + 1);
        aModels[index] := PFrequencyModels[i];
        inc(index);
      end
      else
      begin
        UpdateLog(Format('skipping test for %-14s - already excluded', [PFrequencyModels[i].GetName + '+F']));
        inc(FNumSkipped);
      end;
    end;
    Assert(Length(aModels) >= 1, Format('at least 1 model needed but got ', [Length(aModels)]));
    aThread := TModelTestThread.CreateForAdaptiveModelTest(aModels, FNumThreads);
    aThread.SkipSummaryUpdate := True;
    aThread.ProgressMessage := 'Testing protein models with +F';
    inc(FTotalModelsEvaluated, Length(aModels));
    if FIsModelTamer then
    begin
      aThread.OrigNumSites := FOrigNumSites;
      aThread.BootTable := FBootTable;
    end;

    aThread.FreeOnTerminate := False;
    aThread.ProgressDlg := FAnalysisInfo.ARP;
    aThread.ShowProgress := True;
    FAnalysisInfo.ARP.Thread := aThread;
    aThread.Start;
    aThread.WaitFor;

    if aThread.Canceled then
    begin
      aThread.Canceled := False;
      raise EAbort.Create('model selection analysis cancelled');
    end;
    if not aThread.IsSuccess then
      raise Exception.Create(aThread.MessagesLog.Text);
    UpdateLog('filtering +F models not meeting BIC and AICc thresholds');
    if not Assigned(FCurrentBestModel) then
      FCurrentBestModel := FBestModels[0];
    for i := 0 to aThread.MLTreeAnalyzer.ModelInfoList.Count - 1 do
    begin
      freqModelInfo := TModelInfo.Create;
      freqModelInfo.Assign(aThread.MLTreeAnalyzer.ModelInfoList[i]);
      UpdateLog(Format('evaluated %-10s BIC = %-10.0n  AICc = %.0n', [freqModelInfo.ModelName, freqModelInfo.BIC, freqModelInfo.AICc]));
      baseModelInfo := FBestModels.GetBaseModel(freqModelInfo.BaseModelName);
      modelTest := CompareFrequencyModelToBaseModel(freqModelInfo, baseModelInfo);
      if modelTest = KEEP_FREQUENCY_MODEL then
      begin
        UpdateLog(Format('replaced %s model (BIC = %.0n, AICc = %.0n) with %s (BIC = %.0n, AICc = %.0n)', [baseModelInfo.ModelName, baseModelInfo.BIC, baseModelInfo.AICc, freqModelInfo.ModelName, freqModelInfo.BIC, freqModelInfo.AICc]));
        if baseModelInfo = FCurrentBestModel then
          FCurrentBestModel := freqModelInfo;
        FFilteredModels.AddByDataType(baseModelInfo, True);
        FBestModels.Remove(baseModelInfo);
        FBestModels.AddByDataType(freqModelInfo, True);
      end
      else if modelTest = KEEP_BASE_MODEL then
      begin
        FFilteredModels.AddByDataType(freqModelInfo, True);
        UpdateLog(Format('excluding %-10s ΔBIC = %-8.2f  ΔAIC = %.2f', [freqModelInfo.ModelName,freqModelInfo.BIC - FCurrentBestModel.BIC, freqModelInfo.AICc - FCurrentBestModel.AICc]));
        inc(FNumRemoved);
      end
      else if modelTest = KEEP_BOTH_MODELS then
        FBestModels.AddByDataType(freqModelInfo, True)
      else
        raise Exception.Create('missing handler for frequency model test');
    end;
    Assert(FBestModels.Count <= MaxModels, Format('model list count (%d) exceeds max (%d): %s', [FBestModels.Count, MaxModels, FBestModels.ModelNames]));
    FBestModels.Sort(@compBIC);
    FCurrentBestModel := FBestModels[0];
    UpdateRunStatusInfo(CURRENT_BEST_MODEL, FCurrentBestModel.FullName);
    UpdateLog('iteration completed (+F)');
    if FBestModels.Count > 1 then
      UpdateLog(Format('current best model is %s [BIC = %.0n]. Second best model is %s [BIC = %.0n]', [FCurrentBestModel.ModelName, FCurrentBestModel.BIC, FBestModels[1].ModelName, FBestModels[1].BIC]))
    else
      UpdateLog(Format('current best model is %s [BIC = %.0n].', [FCurrentBestModel.ModelName, FCurrentBestModel.BIC]));
    for i := FBestModels.Count - 1 downto 1 do
    begin
      if (not KeepModel(FBestModels[i])) and (not HaveSameBaseModel(FCurrentBestModel, FBestModels[i])) then
      begin
        freqModelInfo := FBestModels[i];
        FFilteredModels.Add(freqModelInfo);
        aName := BaseModelName(FBestModels[i].ModelName);
        UpdateLog(Format('excluding %-10s ΔBIC = %-8.2f  ΔAIC = %.2f', [freqModelInfo.ModelName,freqModelInfo.BIC - FCurrentBestModel.BIC, freqModelInfo.AICc - FCurrentBestModel.AICc]));
        RemoveFromGammaInvarModels(aName);
        FBestModels.Delete(i);
        inc(FNumRemoved);
      end;
    end;
    UpdateLog(Format('continuing with %d models out of %d candidate models', [FBestModels.Count, PBaseModels.Count + PFrequencyModels.Count]));
    UpdateLog(Format('models under consideration = %s', [FBestModels.ModelNames]));
  finally
    if Assigned(aThread) then
      aThread.Free;
  end;
end;

function TAdaptiveModelTestThread.CompareFrequencyModelToBaseModel(freqModel: TModelInfo; baseModel: TModelInfo): Integer;
var
  deltaBic: Double = 0;
  deltaAic: Double = 0;
begin
  if KeepModel(freqModel) then
  begin
    deltaBic := baseModel.BIC - freqModel.BIC;
    deltaAic := baseModel.AICc - freqModel.AICc;

    if (CompareValue(deltaBic, DEFAULT_BIC_THRESHOLD, FP_CUTOFF) > 0) and (CompareValue(deltaAic, DEFAULT_AICC_THRESHOLD, FP_CUTOFF) > 0) then
      Result := KEEP_FREQUENCY_MODEL
    else
      Result := KEEP_BOTH_MODELS;
  end
  else
    Result := KEEP_BASE_MODEL;
end;

function TAdaptiveModelTestThread.BaseModelName(aModelName: String): String;
begin
  Result := StringReplace(aModelName, '+F', EmptyStr, [rfReplaceAll]);
  Result := StringReplace(Result, '+G', EmptyStr, [rfReplaceAll]);
  Result := StringReplace(Result, '+I', EmptyStr, [rfReplaceAll]);
end;

procedure TAdaptiveModelTestThread.RemoveFromGammaInvarModels(aModelName: String);
var
  i: Integer = -1;
  aModel: TGammaRateVariationModel = nil;
begin
  if PGammaInvarModels.Count > 0 then
    for i := PGammaInvarModels.Count - 1 downto 0 do
    begin
      aModel := PGammaInvarModels[i];
      if aModel.GetName = aModelName then
      begin
        Assert(aModel is TProteinMatrixModel, Format('removing non-TProteinMatrixModel (%s) based on +F', [aModel.GetName]));
        UpdateLog(Format('excluding %-10s from the candidate list because %s +F has high BIC and AICc values', [aModel.GetDescription, aModel.GetName]));
        PGammaInvarModels.Delete(i);
        inc(FNumRemoved);
      end;
    end;
end;

function TAdaptiveModelTestThread.IsInBestModelInfoList(aModel: TGammaRateVariationModel; checkPlusF: Boolean): Boolean;
var
  i: Integer = -1;
  tempModelInfo: TModelInfo = nil;
begin
  Result := False;
  try
    Assert(FBestModels.Count > 0, 'empty best models info list');
    tempModelInfo := TModelInfo.Create;
    aModel.GetInfo(tempModelInfo);

    if FBestModels.Count > 0 then
      for i := 0 to FBestModels.Count - 1 do
      begin
        { when checking gamma and invar models, +F variants are a special case.
          We only keep the +F variants if the equivalent base model with +F is
          in the list of best models}
        if tempModelInfo.IsPlusFModel and checkPlusF then
        begin
          if FBestModels[i].IsPlusFModel then
            if tempModelInfo.IsSameBaseModel(FBestModels[i]) then
            begin
              Result := True;
              break;
            end;
        end
        else
        begin
          if tempModelInfo.IsSameBaseModel(FBestModels[i]) then
          begin
            Result := True;
            break;
          end;
        end;
      end;
  finally
    if Assigned(tempModelInfo) then
      tempModelInfo.Free;
  end;
end;

procedure TAdaptiveModelTestThread.Execute;
var
  i: Integer = -1;
begin
  try
    StartExecute;
    if not FTestBaseModelsOnly then
      UpdateLog(Format('starting adaptive model selection analysis with BIC threshold = %.2f and AICc threshold = %.2f', [FTargetBicDelta, FTargetAicDelta]));
    UpdateRunStatusInfo(CURRENT_BEST_MODEL, 'TBD');
    if FIsDna then
      InitDnaModels
    else
      InitProteinModels;
    if FBestModels.Count = 0 then { otherwise a list of already evaluated base models was provided}
      TestBaseModels;
    if not FTestBaseModelsOnly then
    begin
      if not FIsDna then
        TestFrequencyModels;
      TestGammaInvarModels;
    end;
    ReplaceFilteredModels;
    FAnalysisInfo.MyMLAnalysisPack.ModelInfoList.DeleteAll;
    for i := 0 to FBestModels.Count - 1 do
      FAnalysisInfo.MyMLAnalysisPack.ModelInfoList.Add(FBestModels[i]);
    EndExecute;
    FAnalysisInfo.ModelSelectionCaption := GenerateCaption(FBestModels.Count, FAllModels.Count);
    FBestModels.Clear;
    UpdateAnalysisSummary;
  except
    on E:EAbort do
    begin
      if NumBestModels > 0 then
      begin
        ReplaceFilteredModels;
        FAnalysisInfo.MyMLAnalysisPack.ModelInfoList.DeleteAll;
        for i := 0 to FBestModels.Count - 1 do
          FAnalysisInfo.MyMLAnalysisPack.ModelInfoList.Add(FBestModels[i]);
        FAnalysisInfo.ModelSelectionCaption := GenerateCaption(FBestModels.Count, FAllModels.Count);
      end;
      FIsCancelled := True;
      FIsSuccess := False;
      UpdateLog(E.Message);
      MessagesLog.Add(E.Message);
      if FBestModels.Count > 0 then
        UpdateLog(Format('model selection was cancelled but results for %d models are available', [FBestModels.Count]));
      HideProgressDialog;
    end;
    on E:Exception do
    begin
      HideProgressDialog;
      FIsSuccess := False;
      UpdateLog(E.Message);
      MessagesLog.Add(E.Message);
    end;
  end;
end;

procedure TAdaptiveModelTestThread.UpdateRunStatusInfo(aStatus: String; aInfo: String);
begin
  FStatus := aStatus;
  FInfo := aInfo;
  Synchronize(@DoUpdateRunStatusInfo);
end;

procedure TAdaptiveModelTestThread.DoUpdateRunStatusInfo;
begin
  if Assigned(FProgressDlg) then
  begin
    FProgressDlg.UpdateRunStatusInfo(FStatus, FInfo);
    {$IFDEF VISUAL_BUILD}
    FProgressDlg.Refresh;
    {$ENDIF}
  end;
end;

function TAdaptiveModelTestThread.KeepModel(aModelInfo: TModelInfo): Boolean;
var
  aDelta: Double = 0;
begin
  Assert(Assigned(FCurrentBestModel), 'accessing nil FCurrentBestModel');
  aDelta := aModelInfo.BIC - FCurrentBestModel.BIC;
  Result := (CompareValue(aDelta, FTargetBicDelta, FP_CUTOFF) <= 0);
  if not Result then
  begin
    aDelta := aModelInfo.AICc - FCurrentBestModel.AICc;
    Result := (CompareValue(aDelta, FTargetAicDelta, FP_CUTOFF) <= 0);
  end;
end;

procedure TAdaptiveModelTestThread.MyWriteToDevConsole(aMsg: String);
begin
  {$IFDEF DEBUG}{$IFDEF VISUAL_BUILD}
  FDevMsg := aMsg;
  Synchronize(@DoWriteToDevConsole);
  {$ENDIF}{$ENDIF}
end;

procedure TAdaptiveModelTestThread.DoWriteToDevConsole;
begin
  {$IFDEF DEBUG}{$IFDEF VISUAL_BUILD}
  WriteToDevConsole(FDevMsg);
  {$ENDIF}{$ENDIF}
end;

procedure TAdaptiveModelTestThread.ClearRunStatusInfo;
var
  i: Integer = -1;
  aList: TStringList = nil;
begin
  try
    if Assigned(FProgressDlg) then
    begin
      aList := TStringList.Create;
      if FAnalysisInfo.MyNumThreadsToUse > 0 then
      begin
        for i := 1 to FAnalysisInfo.MyNumThreadsToUse do
          aList.Add(Format('Thread-%d', [i]));
        FProgressDlg.RemoveRunStatusInfo(aList);
       end;
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TAdaptiveModelTestThread.BestModelsStr: String;
var
  i: Integer = -1;
begin
  Result := EmptyStr;
  FStringBuilder.Clean;
  if FBestModels.Count > 0 then
  begin
    for i := 0 to FBestModels.Count - 1 do
    begin
      FStringBuilder.Add(FBestModels[i].FullName);
      FStringBuilder.Add(', ');
    end;
    Result := FStringBuilder.GenerateString;
  end;
end;

function TAdaptiveModelTestThread.GammaInvarModelsStr: String;
var
  i: Integer = -1;
begin
  Result := EmptyStr;
  FStringBuilder.Clean;
  if PGammaInvarModels.Count > 0 then
  begin
    for i := 0 to PGammaInvarModels.Count - 1 do
    begin
      FStringBuilder.Add(PGammaInvarModels[i].GetName);
      FStringBuilder.Add(', ');
    end;
    Result := FStringBuilder.GenerateString;
  end;
end;

function TAdaptiveModelTestThread.FreqModelsStr: String;
var
  i: Integer = -1;
begin
  Result := EmptyStr;
  FStringBuilder.Clean;
  if PFrequencyModels.Count > 0 then
  begin
    for i := 0 to PFrequencyModels.Count - 1 do
    begin
      FStringBuilder.Add(PFrequencyModels[i].GetName);
      FStringBuilder.Add(', ');
    end;
    Result := FStringBuilder.GenerateString;
  end;
end;

procedure TAdaptiveModelTestThread.HideProgressDialog;
begin
  Synchronize(@DoHideProgressDialog);
end;

procedure TAdaptiveModelTestThread.DoHideProgressDialog;
begin
  if Assigned(FProgressDlg) and FProgressDlg.Visible then
    FProgressDlg.Hide;
end;

function TAdaptiveModelTestThread.AnalysisDescription: String;
begin
  Result := 'Filtered Model Test';
end;

constructor TAdaptiveModelTestThread.Create(aInfo: TAnalysisInfo);
begin
  inherited Create(True);
  FAInfo := aInfo;
  FNumPrepopulatedModels := 0;
  FTotalModelsEvaluated := 0;
  FTestBaseModelsOnly := False;
  FStringBuilder := TMegaStringBuilder.Create;
  FNumSkipped := 0;
  FNumRemoved := 0;
  FIsCancelled := False;
  FIsSuccess := True;
  FreeOnTerminate := True;
  {$IFDEF VISUAL_BUILD}
  FTargetBicDelta := aInfo.GetIntegerSetting(opsDeltaBicCutoff);
  FTargetAicDelta := aInfo.GetIntegerSetting(opsDeltaAicCutoff);
  {$ELSE}
  if aInfo.HasIntegerSetting(opsDeltaBicCutoff) then
    FTargetBicDelta := aInfo.GetIntegerSetting(opsDeltaBicCutoff)
  else
    FTargetBicDelta := DEFAULT_BIC_THRESHOLD;
  if aInfo.HasIntegerSetting(opsDeltaAicCutoff) then
    FTargetAicDelta := aInfo.GetIntegerSetting(opsDeltaAicCutoff)
  else
    FTargetAicDelta := DEFAULT_AICC_THRESHOLD;
  {$ENDIF}
  FLog := TStringList.Create;
  FAllModels := TGammaRateVariationModelList.Create;
  PBaseModels := TGammaRateVariationModelList.Create;
  PGammaInvarModels := TGammaRateVariationModelList.Create;
  if aInfo.isAminoAcid then
    PFrequencyModels := TGammaRateVariationModelList.Create;
  FFilteredModels := TModelInfoList.Create;
  FBestModels := TModelInfoList.Create;
  FIsDna := not aInfo.isAminoAcid;
  FNumRates := 5;
  FNumThreads := aInfo.MyNumThreadsToUse;
  FAnalysisInfo := aInfo;
  FProgressDlg := FAnalysisInfo.ARP;
  if Assigned(aInfo.MyMLAnalysisPack) and (aInfo.MyMLAnalysisPack.ModelInfoList.Count > 0) then
    aInfo.MyMLAnalysisPack.ModelInfoList.DeleteAll;
end;

constructor TAdaptiveModelTestThread.CreateFromModelInfoList(aInfo: TAnalysisInfo; aModels: TModelInfoList);
begin
  Create(aInfo);
  FBestModels.Assign(aModels);
  FNumPrepopulatedModels := aModels.Count;
end;

destructor TAdaptiveModelTestThread.Destroy;
begin
  FAnalysisInfo := nil;
  FCurrentBestModel := nil;
  if Assigned(FLog) then
    FLog.Free;
  ClearModels;
  if Assigned(FAllModels) then
    FAllModels.Free;
  if Assigned(FFilteredModels) then
    FFilteredModels.Free;
  if Assigned(FBestModels) then
    FBestModels.Free;
  if Assigned(PBaseModels) then
    PBaseModels.Free;
  if Assigned(PGammaInvarModels) then
    PGammaInvarModels.Free;
  if Assigned(PFrequencyModels) then
    PFrequencyModels.Free;
  if Assigned(FStringBuilder) then
    FStringBuilder.Free;
  inherited Destroy;
end;


end.

