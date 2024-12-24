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

unit mreltimethreads;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType,
  Classes, SysUtils, MTreeSearchThread, MLSearchThread, MAnalysisInfo, MTreeData,
  MRuntimeProgressDlg, MLTreeAnalyzer, MLTree, MDistPack, MReltimeComputer,
  MCalibrationData, MegaConsts, MSimpleTreeNode, mcorrelationtest,
  mcalibrationsampler;

type

  { TRelTimeBLenThread }

  TRelTimeBLenThread = class(TTreeSearchThread)
    private
      FAnalysisInfo: TAnalysisInfo;
      FNoOfOtus: Integer;
      FDoMerge: Boolean;
      FNumNodesProcessed: Integer;
      FCalibrationSampler: TCalibrationDensitySampler;
      FSamplingCheckCancel: TProgressAndStatusCheckCancel;
      FSamplingProgress: Integer;
      function SampleCalibrations(aData: TTreeData; calibs: TCalibrations; otuNames: TStringList; maxRateRatio: Extended): Boolean;
      function ProgressCallback(aProgress: Integer; aType: String; aInfo: String): Boolean;
      procedure DoProgressCallback;
      procedure DoSynchronizeErrorMessage;
    protected
      procedure Search; override;
      function AnalysisDescription: String; override;
    public
      MyExceptionName: String;
      MyExceptionMessage: String;
      Calibrations: TArrayOfCalibrationTime;
      procedure SynchronizeErrorMessage(E: Exception); { GS - switch context to main thread to display error message because the LCL is not thread safe}
      constructor Create(CreateSuspended: Boolean);
      destructor Destroy; override;
      procedure Execute; override;
      property AnalysisInfo: TAnalysisInfo read FAnalysisInfo write FAnalysisInfo;
      property DoMerge: Boolean read FDoMerge write FDoMerge;
      property NoOfOtus: Integer read FNoOfOtus write FNoOfOtus;
  end;

  { TRelTimeLSThread }

  TRelTimeLSThread = class(TTreeSearchThread)
    private
      FIsSuccess: Boolean;
      FLog: TStringList;
      FAnalysisInfo: TAnalysisInfo;
      FCalibrationSampler: TCalibrationDensitySampler;
      FSamplingCheckCancel: TProgressAndStatusCheckCancel;
      FSamplingProgress: Integer;
      function GetLogText: String;
      procedure SetAnalysisInfo(AValue: TAnalysisInfo);
      function SampleCalibrations(aData: TTreeData; calibs: TCalibrations; otuNames: TStringList; MaxRateRatio: Extended): Boolean;
      function ProgressCallback(aProgress: Integer; aType: String; aInfo: String): Boolean;
      procedure DoProgressCallback;
      procedure DoSynchronizeErrorMessage;
      function ComputeDistances: Boolean;
    protected
      procedure Search; override;
      function AnalysisDescription: String; override;
    public
      MyExceptionName: String;
      MyExceptionMessage: String;
      procedure SynchronizeErrorMessage(E: Exception); { GS - switch context to main thread to display error message because the LCL is not thread safe}
      constructor Create(CreateSuspended: Boolean);
      destructor Destroy; override;
      procedure Execute; override;
      property AnalysisInfo: TAnalysisInfo read FAnalysisInfo write SetAnalysisInfo;
      property IsSuccess: Boolean read FIsSuccess;
      property LogText: String read GetLogText;
  end;

  { TRelTimeMLThread }

  TRelTimeMLThread = class(TMLTreeThread)
  private
    FCorrelationTest: TCorrelationTest;
    MLACheckCancel: TCheckCancelFunc;
    FOptimizeParams: boolean;
    FAnalysisInfo: TAnalysisInfo;
    FMergeRates: Boolean;
    FIsSuccess: Boolean;

    procedure SetMergeRates(const Value: Boolean);
  protected
    FCalibrationSampler: TCalibrationDensitySampler;
    FSamplingCheckCancel: TProgressAndStatusCheckCancel;
    FSamplingProgress: Integer;
    function ProgressCallback(aProgress: Integer; aType: String; aInfo: String): Boolean;
    procedure DoProgressCallback;
    function SampleCalibrations(aData: TTreeData; calibs: TCalibrations; otuNames: TStringList; maxRateRatio: Extended): Boolean;
    function Initialize: Boolean; override;
    function CheckCancel(Progress: Integer; Status: AnsiString): Boolean;
    procedure Search; override;
    function AnalysisDescription: String; override;
  public
    SampleTimes: TDivTimesArray;
    constructor Create(MLTreeAnalyzer: TMLTreeAnalyzer);
    destructor Destroy; override;
    property OptimizeParams: boolean read FOptimizeParams write FOptimizeParams;
    property AnalysisInfo: TAnalysisInfo read FAnalysisInfo write FAnalysisInfo;
    property MergeRates: Boolean read FMergeRates write SetMergeRates;
    property IsSuccess: Boolean read FIsSuccess;
    property CorrelationTest: TCorrelationTest read FCorrelationTest;
  end;

  { TAnchoredRelTimeMLThread }

  TAnchoredRelTimeMLThread = class(TRelTimeMLThread)
    private
      FMaxTImes: TDivTimesArray;
      FMinTimes: TDivTimesArray;
      FOrigMaxTimes: TDivTimesArray;
      FOrigMinTimes: TDivTimesArray;
      procedure SetMaxTimes(AValue: TDivTimesArray);
      procedure SetMinTimes(AValue: TDivTimesArray);
    protected
      function Initialize: Boolean; override;
      procedure Search; override;
      function AnalysisDescription: String; override;
    public
      property MinTimes: TDivTimesArray read FMinTimes write SetMinTimes;
      property MaxTimes: TDivTimesArray read FMaxTImes write SetMaxTimes;
      property OrigMinTimes: TDivTimesArray read FOrigMinTimes;
      property OrigMaxTimes: TDivTimesArray read FOrigMaxTimes;
  end;




  { TAnchorMLReltimeTreeThread }
  { For calibration testing, we already have a reltime tree and we are anchoring on some set of available constraints}
  TAnchorMLReltimeTreeThread = class(TAnchoredRelTimeMLThread)
    protected
      function Initialize: Boolean; override;
      procedure Search; override;
    public
      Calibrations: TCalibrations;
      ClockTree: TTreeData;
  end;

  { TCorrTestMLThread }

  TCorrTestMLThread = class(TReltimeMLThread)
    private
      FCorrelationTest: TCorrelationTest;
    protected
      procedure Search; override;
      function Initialize: Boolean; override;
      function AnalysisDescription: String; override;
    public
      property CorrelationTest: TCorrelationTest read FCorrelationTest;
  end;

  { TCorrTestBlenThread }

  TCorrTestBlenThread = class(TTreeSearchThread)
  private
    FAnalysisInfo: TAnalysisInfo;
    FCorrelationTest: TCorrelationTest;
    procedure SetAnalysisInfo(AValue: TAnalysisInfo);
  protected
    procedure Search; override;
    function AnalysisDescription: String; override;
  public
    MyExceptionName: String;
    MyExceptionMessage: String;
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    property AnalysisInfo: TAnalysisInfo read FAnalysisInfo write SetAnalysisInfo;
    property CorrelationTest: TCorrelationTest read FCorrelationTest;
  end; 
  
implementation

uses
  {$IFNDEF VISUAL_BUILD}MD_MegaMain, MegaUtils_NV,{$ENDIF}
  MegaUtils, msitecoverage, MSubstitutionModelUtils, MTreeDataAdapter,
  MTreeList, KeywordConsts, Dialogs, MSeqDistBase, MNucDist, MAminoDist, MSynNonsynDist,
  MD_InputSeqData;

{ TCorrTestBlenThread }

procedure TCorrTestBlenThread.SetAnalysisInfo(AValue: TAnalysisInfo);
begin
  FAnalysisInfo := AValue;
end;

procedure TCorrTestBlenThread.Search;
var
  RelTimeComputer: TRelTimeComputer;
  MaxRateRatio: Extended;
begin
  RelTimeComputer := nil;
  if IsDeveloper and (FAnalysisInfo.MaxRateRatio > 0) then
    MaxRateRatio := FAnalysisInfo.MaxRateRatio
  else
    MaxRateRatio := DEFAULT_MAX_RATE_RATIO;

  try
    {$IFNDEF VISUAL_BUILD}
    D_MegaMain.AnalysisSummary.DataType := snTree;
    {$ELSE}
    FAnalysisInfo.AnalysisSummary.DataType := snTree;
    {$ENDIF}
    RelTimeComputer := TRelTimeComputer.Create;
    RelTimeComputer.IsBlensOnly := True;
    RelTimeComputer.ProgressProc := Self.OnProgress;
    UpdateRunStatusInfo('Status', 'Computing CorrTest');
    FCorrelationTest := ReltimeComputer.CorrelationTest(FAnalysisInfo.MyOriTreeList[0], MaxRateRatio, True);
    FAnalysisInfo.AnalysisSummary.AddCalculatedValue('CorrTest Score', Format('%.3e', [CorrelationTest.Score]));
    FAnalysisInfo.AnalysisSummary.AddCalculatedValue('CorrTest P-value', CorrelationTest.PValueString);
    {$IFNDEF VISUAL_BUILD}
    D_MegaMain.AnalysisSummary.AddCalculatedValue('CorrTest Score', Format('%.3e', [CorrelationTest.Score]));
    D_MegaMain.AnalysisSummary.AddCalculatedValue('CorrTest P-value', CorrelationTest.PValueString);
    D_MegaMain.AnalysisSummary.NumTaxa := FAnalysisInfo.MyOriTreeList[0].NoOfOTUs;
    {$ELSE}
    FAnalysisInfo.AnalysisSummary.NumTaxa := FAnalysisInfo.MyOriTreeList[0].NoOfOTUs;
    {$ENDIF}
    UpdateRunStatusInfo('Status', 'Preparing results');
    OnProgress(100);
  except
    on E: EAbort do
    begin
      FCanceled := True;
    end;
    On E : EOutOfMemory do // trap out of memory exceptions and handle it ourselves in the onterm procedure
    begin
      MyExceptionName := E.ClassName;
      MyExceptionMessage := E.Message;
    end;
    On E : Exception do
    begin
      {$IFNDEF VISUAL_BUILD}
      MyExceptionName := E.ClassName;
      MyExceptionMessage := E.Message; // the thread terminator will write out the error message and clean up memory
      {$ELSE}
      MyExceptionName := 'HandledException';  // so the onterm procedure won't try to display tree explorer
      MyExceptionMessage := 'Already taken care of.';
      {$ENDIF}
    end;
  end;
end;

function TCorrTestBlenThread.AnalysisDescription: String;
begin
  Result := 'CorrTest(branch lengths)';
end;

constructor TCorrTestBlenThread.Create;
begin
  inherited Create;
end;

destructor TCorrTestBlenThread.Destroy;
begin
  inherited Destroy;
end;

procedure TCorrTestBlenThread.Execute;
begin
  Search;
end;

{ TCorrTestMLThread }

procedure TCorrTestMLThread.Search;
const
  lnLDigits = 3;
  paraDigits = 2;
var
  UserTree: TTreeData = nil;
  ReltimeComputer: TReltimeComputer;
  MaxRateRatio: Extended;
begin
  if IsDeveloper then
    MaxRateRatio := FAnalysisInfo.MaxRateRatio
  else
    MaxRateRatio := DEFAULT_MAX_RATE_RATIO;
  ReltimeComputer := nil;

  try
    try
      if not Initialize then
        raise Exception.Create('Failed to initialize TCorrTestMLThread');
      MLTreeAnalyzer.InitTree.isSE := True;
      FAnalysisInfo.MyOriTreeList.isSE := True;
      MLTreeAnalyzer.InitTree.isStats := False;
      FAnalysisInfo.MyOriTreeList.isStats := False;
      FStatus := 'Optimizing user tree';
      OnProgress(FProgress);
      MLTreeAnalyzer.OptimizeMessage := 'Optimizing user tree';
      FAnalysisInfo.MergeRates := FMergeRates;
      MLTreeAnalyzer.Initialize;
      if OptimizeParams then
        MLTreeAnalyzer.Optimize
      else
        MLTreeAnalyzer.OptimizeBLens;
      // end of user tree analysis

      if Self.Terminated or Self.Canceled then
        Exit;

      // now make a clock tree from the ML optimized user tree
      FAnalysisInfo.MyOriTreeList.ValueName := 'LogL';
      MLTreeAnalyzer.ModelInfoList.Add(TModelInfo.Create); // put the non-clock tree stuff at index 1 so we don't have to set the index for TTreeViewForm
      MLTreeAnalyzer.GetModelInfo(MLTreeAnalyzer.ModelInfoList[0]);
      UpdateRunStatusInfo('status', 'Running CorrTest');
      UserTree := TTreeData.Create(FAnalysisInfo.NoOfSeqs, True, True, False, False);
      MLTreeAnalyzer.MLTree.GetTreeData(UserTree);
      FAnalysisInfo.MyOriTreeList.Delete(0); // dispose of the user's tree since we have recalculated branch lengths
      FAnalysisInfo.MyOriTreeList.Insert(0, UserTree);

      ReltimeComputer := TReltimeComputer.Create;
      FCorrelationTest := ReltimeComputer.CorrelationTest(FAnalysisInfo.MyOriTreeList[0], MaxRateRatio, True);
      {$IFNDEF VISUAL_BUILD}
      D_MegaMain.AnalysisSummary.AddCalculatedValue('CorrTest Score', Format('%.3e', [CorrelationTest.Score]));
      D_MegaMain.AnalysisSummary.AddCalculatedValue('CorrTest P-value', CorrelationTest.PvalueString);
      {$ELSE}
      FAnalysisInfo.AnalysisSummary.AddCalculatedValue('CorrTest Score', Format('%.3e', [CorrelationTest.Score]));
      FAnalysisInfo.AnalysisSummary.AddCalculatedValue('CorrTest P-value', CorrelationTest.PvalueString);
      {$ENDIF}
      MLTreeAnalyzer.ModelInfoList.Insert(0,TModelInfo.Create);
      MLTreeAnalyzer.GetModelInfo(MLTreeAnalyzer.ModelInfoList[0]); // copy the active model info (for clock tree) into the newly created model info

      FAnalysisInfo.LogLikelihoodWithClock  := FloatToStrF(MLTreeAnalyzer.ModelInfoList[0].LogL, ffFixed, 12, lnLDigits);
      FAnalysisInfo.LogLikelihoodWithoutClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[1].LogL, ffFixed, 12, lnLDigits);
      FAnalysisInfo.InvarWithClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[0].Invar, ffFixed, 12, paraDigits);
      FAnalysisInfo.GammaWithClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[0].Gamma, ffFixed, 12, paraDigits);
      FAnalysisInfo.InvarWithoutClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[1].Invar, ffFixed, 12, paraDigits);
      FAnalysisInfo.GammaWithoutClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[1].Gamma, ffFixed, 12, paraDigits);
      FAnalysisInfo.FullModelName := MLTreeAnalyzer.ModelInfoList[0].FullName;
      FAnalysisInfo.ARP.Progress := 100;
    except
      on E: EAbort do
      begin
        FCanceled := True;
      end;
      On E : EOutOfMemory do // trap out of memory exceptions and handle it ourselves in the onterm procedure
      begin
        FExceptionName := E.ClassName;
        FExceptionMessage := E.Message;
      end;
      On E : Exception do
      begin
        FExceptionName := E.ClassName;
        FExceptionMessage := E.Message; // the thread terminator will write out the error message and clean up memory
      end;
    end;
  finally

  end;
end;

function TCorrTestMLThread.Initialize: Boolean;
begin
  Result := inherited Initialize;
  MLTreeAnalyzer.NeedsRootByOutgroup := True;
end;

function TCorrTestMLThread.AnalysisDescription: String;
begin
  Result := 'CorrTest';
end;

{ TRelTimeBLenThread }

function TRelTimeBLenThread.SampleCalibrations(aData: TTreeData; calibs: TCalibrations; otuNames: TStringList; maxRateRatio: Extended): Boolean;
begin
  FSamplingProgress := 0;
  FCalibrationSampler := TCalibrationDensitySampler.Create(calibs, aData, otuNames, True);
  if Assigned(FAnalysisInfo) and Assigned(FAnalysisInfo.ARP) then
  begin
    FSamplingCheckCancel := FAnalysisInfo.ARP.ProgressAndStatusCheckCancel;
    FCalibrationSampler.CheckCancelFunc := ProgressCallback;
  end;
  Result := FCalibrationSampler.DoSampling;
  FCalibrationSampler.DoReltime(aData, maxRateRatio);
  FCalibrationSampler.OutputDeveloperData;
end;

function TRelTimeBLenThread.ProgressCallback(aProgress: Integer; aType: String;aInfo: String): Boolean;
begin
  FSamplingProgress := aProgress;
  Synchronize(DoProgressCallback);
  Result := FCanceled;
end;

procedure TRelTimeBLenThread.DoProgressCallback;
begin
  if Assigned(FSamplingCheckCancel) then
    FCanceled := FSamplingCheckCancel(FSamplingProgress, 'Status', 'Computing Calibration Density Trees');
end;

procedure TRelTimeBLenThread.DoSynchronizeErrorMessage;
begin
  {$IFDEF VISUAL_BUILD}
  ShowMessage(Format('Error (%s) in BLens search thread: %s', [MyExceptionName, MyExceptionMessage]));
  {$ELSE}
  error_nv(Format('Error (%s) in BLens search thread: %s', [MyExceptionName, MyExceptionMessage]));
  {$ENDIF}
end;

procedure TRelTimeBLenThread.Search;
var
  RelTimeTree: TTreeData = nil;
  ClockExportList: TStringList = nil;
  RelTimeComputer: TRelTimeComputer = nil;
  HasCalibrations: Boolean;
  MaxRateRatio: Extended;
  sampleTimes: TDivTimesArray;
  CorrelationTest: TCorrelationTest = nil;
begin
  SetLength(sampleTimes, 0);
  if IsDeveloper then
    MaxRateRatio := FAnalysisInfo.MaxRateRatio
  else
    MaxRateRatio := DEFAULT_MAX_RATE_RATIO;
  try
    try
      {$IFNDEF VISUAL_BUILD}
      D_MegaMain.AnalysisSummary.DataType := snTree;
      {$ELSE}
      FAnalysisInfo.AnalysisSummary.DataType := snTree;
      {$ENDIF}
      ClockExportList := TStringList.Create;
      RelTimeComputer := TRelTimeComputer.Create;
      RelTimeComputer.IsBlensOnly := True;
      FAnalysisInfo.MyRelTimeComputer := RelTimeComputer;
      RelTimeComputer.ProgressProc := Self.OnProgress;
      UpdateRunStatusInfo('Status', 'Computing RelTime divergence times');
      HasCalibrations := (FAnalysisInfo.HasCalibrations and (FAnalysisInfo.CalibrationTimes.Count > 0));
      if HasCalibrations then
      begin
        RelTimeComputer.PropagateConstraints := FAnalysisInfo.PropagateConstraints;
        if FAnalysisInfo.CalibrationTimes.IsSampleTimes then
        begin
          FAnalysisInfo.CalibrationTimes.PrepareSamplingTimeArray(sampleTimes, FAnalysisInfo.MyOriTreeList.OTUNameList);
          RelTimeTree := RelTimeComputer.ComputeRelTimeBLens(FAnalysisInfo.MyOriTreeList[0], sampleTimes, MaxRateRatio, FAnalysisInfo.MyOtuNames);
          if not Assigned(RelTimeTree) then
            if ReltimeComputer.Log.Text <> EmptyStr then
              raise Exception.Create(ReltimeComputer.Log.Text)
            else
              raise Exception.Create('Reltime computation failed with an unknown error');
          RelTimeComputer.GenerateSampledTimesClockTreeExport(ClockExportList,FAnalysisInfo.MyOriTreeList.OTUNameList, ReltimeTree.GetInternalNodeLabels, sampleTimes);
        end
        else
        begin
          if FAnalysisInfo.CalibrationTimes.GetUsesCalibrationDensities then
            if not SampleCalibrations(FAnalysisInfo.MyOriTreeList[0], FAnalysisInfo.CalibrationTimes, FAnalysisInfo.MyOriTreeList.OTUNameList, MaxRateRatio) then
              raise Exception.Create('sampling of calibration densities failed');
          RelTimeTree := RelTimeComputer.ComputeRelTimeBLens(FAnalysisInfo.MyOriTreeList[0],
                                                             ClockExportList,
                                                             FAnalysisInfo.MyOtuNames,
                                                             FAnalysisInfo.CalibrationTimes,
                                                             MaxRateRatio);
        end;
      end
      else
        RelTimeTree := RelTimeComputer.ComputeRelTimeBLens(FAnalysisInfo.MyOriTreeList[0],
                                                           ClockExportList,
                                                           FAnalysisInfo.MyOtuNames,
                                                           FAnalysisInfo.MyOriTreeList.InternalNodeLbls[0],
                                                           MaxRateRatio);
      if not Assigned(RelTimeTree) then
        if ReltimeComputer.Log.Text <> EmptyStr then
          raise Exception.Create(ReltimeComputer.Log.Text)
        else
          raise Exception.Create('Reltime computation failed with an unknown error');

      CorrelationTest := ReltimeComputer.AutomaticCorrelationTest(MaxRateRatio);
      {$IFDEF VISUAL_BUILD}
      FAnalysisInfo.AnalysisSummary.AddCalculatedValue('CorrTest Score', Format('%.3e', [CorrelationTest.Score]));
      FAnalysisInfo.AnalysisSummary.AddCalculatedValue('CorrTest P-value', CorrelationTest.PValueString);
      {$ELSE}
      CorrelationTest.SaveSummaryToFile(NextAvailableFilenameNV('_corrtest.txt'));
      D_MegaMain.AnalysisSummary.AddCalculatedValue('CorrTest Score', Format('%.3e', [CorrelationTest.Score]));
      D_MegaMain.AnalysisSummary.AddCalculatedValue('CorrTest P-value', CorrelationTest.PValueString);
      {$ENDIF}


      {$IFNDEF VISUAL_BUILD}
      D_MegaMain.AnalysisSummary.NumTaxa := RelTimeTree.NoOfOTUs;
      {$ELSE}
      FAnalysisInfo.AnalysisSummary.NumTaxa := RelTimeTree.NoOfOTUs;
      {$ENDIF}
      FAnalysisInfo.ClockTreeExport := ClockExportList;
      FAnalysisInfo.ReltimeNexusExport := ReltimeComputer.GenerateNexusReltimeExport(FAnalysisInfo.MyOtuNames, HasCalibrations, False);
      FAnalysisInfo.MyOriTreeList.Insert(0, ReltimeTree);

      UpdateRunStatusInfo('Status', 'Preparing results');
      OnProgress(100);
    except
      on E: EAbort do
      begin
        FCanceled := True;
      end;
      On E : EOutOfMemory do // trap out of memory exceptions and handle it ourselves in the onterm procedure
      begin
        MyExceptionName := E.ClassName;
        MyExceptionMessage := E.Message;
      end;
      On E : Exception do
      begin
        MyExceptionName := E.ClassName;
        MyExceptionMessage := E.Message; // the thread terminator will write out the error message and clean up memory
      end;
    end;
  finally
    if Assigned(CorrelationTest) then
      CorrelationTest.Free;
  end;
end;

function TRelTimeBLenThread.AnalysisDescription: String;
begin
  Result := 'Reltime(branch lengths)';
end;

procedure TRelTimeBLenThread.SynchronizeErrorMessage(E: Exception);
begin
  MyExceptionName := E.ClassName;
  MyExceptionMessage := E.Message;
  Synchronize(DoSynchronizeErrorMessage);
end;

constructor TRelTimeBLenThread.Create(CreateSuspended: Boolean);
begin
  inherited Create;
  MyExceptionName := 'none';
  MyExceptionMessage := 'none';
  FNumNodesProcessed := 0;
  FreeOnTerminate := True;
  FAnalysisInfo := nil;
  FDoMerge := False; // default
  FCalibrationSampler := nil;
end;

destructor TRelTimeBLenThread.Destroy;
begin
  if assigned(FAnalysisInfo) then
    FAnalysisInfo.Free;
  if Assigned(FCalibrationSampler) then
    FCalibrationSampler.Free;
  inherited Destroy;
end;

procedure TRelTimeBLenThread.Execute;
begin
  Search;
end;


{ TRelTimeThread }

function TRelTimeMLThread.Initialize: Boolean;
var
  Model: TGammaRateVariationModel;
  TreeData: TTreeData;
begin
  Result := False;
  Model := CreateSubstitutionModel(AnalysisInfo, SubTaskCheckCancelFunc);
  TreeData := TTreeData.Create(AnalysisInfo.NoOfSeqs,false,false,false);
  TreeData.Assign(AnalysisInfo.MyOriTreeList[0]);
  MLTreeAnalyzer := TMLTreeAnalyzer.Create(AnalysisInfo.MySeqStrings, TreeData, Model);
  MLTreeAnalyzer.NoOfThreadsToUse := AnalysisInfo.MyNumThreadsToUse;
  MLTreeAnalyzer.NeedsRootByOutgroup := True;
  MLTreeAnalyzer.IsGlobalClock := False;
  MLACheckCancel := MLTreeAnalyzer.CheckCancel;
  MLTreeAnalyzer.CheckCancel := CheckCancel;
  MLTreeAnalyzer.SubTaskCheckCancel := SubTaskCheckCancelFunc;
  AnalysisInfo.ClockTypeSet := True;

  Case AnalysisInfo.ClockLevel of
    clOneStdErr:
      MLTreeAnalyzer.GlobalClockLevel := 1;
    clTwoStdErr:
      MLTreeAnalyzer.GlobalClockLevel := 2;
    clThreeStdErr:
      MLTreeAnalyzer.GlobalClockLevel := 3;
  End;
  if AnalysisInfo.MLSearchFilter > 0 then
      MLTreeAnalyzer.SearchFilter := AnalysisInfo.MLSearchFilter;
  AnalysisInfo.MyMLAnalysisPack := MLTreeAnalyzer;
  //RootOnOutgroup(AnalysisInfo);
  AnalysisInfo.MyOriTreeList[0].isSE := True;
  if not AnalysisInfo.MyOriTreeList[0].isBLen then
    AnalysisInfo.MyOriTreeList.isBLen := True;
  Result := True;
end;

function TRelTimeMLThread.CheckCancel(Progress: Integer; Status: AnsiString): Boolean;
begin
  FProgress := Progress;
  FStatus   := Status;
  try
    if (FProgressDlg <> nil) {$IFDEF VISUAL_BUILD}and (FProgressDlg.PercentGauge <> nil){$ENDIF} then
      OnProgress(Progress);
  Except on E: Exception do
    // do nothing, it's not worth it to have the program crash if we lose a progress update.
  end;
  result := Terminated;
end;

constructor TRelTimeMLThread.Create(MLTreeAnalyzer: TMLTreeAnalyzer);
begin
  inherited Create(MLTreeAnalyzer);
  FMergeRates := False;
  FIsSuccess := True;
  FCalibrationSampler := nil;
  FSamplingCheckCancel := nil;
  FSamplingProgress := 0;
  FCorrelationTest := nil;
end;

destructor TRelTimeMLThread.Destroy;
begin
  if Assigned(MLTreeAnalyzer) and Assigned(MLACheckCancel) then
    MLTreeAnalyzer.CheckCancel := MLACheckCancel;
  if Assigned(FCalibrationSampler) then
    FCalibrationSampler.Free;
  if Assigned(FCorrelationTest) then
    FCorrelationTest.Free;
  inherited;
end;

procedure TRelTimeMLThread.Search;
const
  lnLDigits = 3;
  paraDigits = 2;
var
  ClockTree: TTreeData = nil;
  UserTree: TTreeData  = nil;
  i: Integer;
  ClockTreeExport: TStringList = nil;
  MaxRateRatio: Extended;
  NodeLabels: TStringList = nil;
begin
  if IsDeveloper and (FAnalysisInfo.MaxRateRatio > 0) then
    MaxRateRatio := FAnalysisInfo.MaxRateRatio
  else
    MaxRateRatio := DEFAULT_MAX_RATE_RATIO;
  try
    try
      if not Initialize then
        raise Exception.Create('Failed to initialize TReltimeMLThread');
      NodeLabels := TStringList.Create;
      NodeLabels.Assign(FAnalysisInfo.MyOriTreeList[0].GetInternalNodeLabels);
      MLTreeAnalyzer.InitTree.isSE := True;
      FAnalysisInfo.MyOriTreeList.isSE := True;
      MLTreeAnalyzer.InitTree.isStats := False;
      FAnalysisInfo.MyOriTreeList.isStats := False;
      FStatus := 'Optimizing user tree';
      OnProgress(FProgress);
      MLTreeAnalyzer.OptimizeMessage := 'Optimizing user tree';
      FAnalysisInfo.MergeRates := FMergeRates;
      MLTreeAnalyzer.Initialize;
      if OptimizeParams then
        MLTreeAnalyzer.Optimize
      else
        MLTreeAnalyzer.OptimizeBLens;
      // end of user tree analysis

      if Self.Terminated or Self.Canceled then
        Exit;

      // now make a clock tree from the ML optimized user tree
      FAnalysisInfo.MyOriTreeList.ValueName := 'LogL';
      MLTreeAnalyzer.ModelInfoList.Add(TModelInfo.Create); // put the non-clock tree stuff at index 1 so we don't have to set the index for TTreeViewForm
      MLTreeAnalyzer.GetModelInfo(MLTreeAnalyzer.ModelInfoList[0]);
      if FAnalysisInfo.MyBootReps > 0 then
        UpdateRunStatusInfo('status', 'Constructing bootstrap timetrees')
      else
        UpdateRunStatusInfo('status', 'Constructing timetree');
      ClockTree := TTreeData.Create(FAnalysisInfo.NoOfSeqs, True, True, True);
      UserTree := TTreeData.Create(FAnalysisInfo.NoOfSeqs, True, True, False, False);
      ClockTreeExport := TStringList.Create;
      MLTreeAnalyzer.MLTree.GetTreeData(ClockTree);
      MLTreeAnalyzer.MLTree.GetTreeData(UserTree);
      ClockTree.GetInternalNodeLabels.Assign(NodeLabels);
      UserTree.GetInternalNodeLabels.Assign(NodeLabels);
      FAnalysisInfo.MyOriTreeList.Delete(0); // dispose of the user's tree since we have recalculated branch lengths
      FAnalysisInfo.MyOriTreeList.Insert(0, UserTree);
      if Length(SampleTimes) = FAnalysisInfo.NoOfSeqs then
      begin
        for i := 0 to FAnalysisInfo.NoOfSeqs - 1 do
          MLTreeAnalyzer.SamplingTime[i] := SampleTimes[i];
      end;
      FAnalysisInfo.MyOriTreeList.Insert(0, ClockTree);
      ComputeSiteCoverage(FAnalysisInfo, ClockTree);
      UserTree.AssignDataCoverage(ClockTree);
      UpdateRunStatusInfo('Status', 'Constructing timetree');
      FAnalysisInfo.ARP.Progress := 1;
      FAnalysisInfo.MySeqStrings := nil; { this gets freed by MLAnalyzer}
      Assert(FMergeRates = False); { disabled in MEGA7}
      if IsDeveloper then
        MLTreeAnalyzer.MakeClockTree(ClockTree, ClockTreeExport, FAnalysisInfo.MyOtuNames, NodeLabels, FAnalysisInfo.MaxRateRatio)
      else
        MLTreeAnalyzer.MakeClockTree(ClockTree, ClockTreeExport, FAnalysisInfo.MyOtuNames, NodeLabels, DEFAULT_MAX_RATE_RATIO);
      FCorrelationTest := MLTreeAnalyzer.MLTree.ReltimeComputer.AutomaticCorrelationTest(MaxRateRatio);
      {$IFNDEF VISUAL_BUILD}
      FCorrelationTest.SaveSummaryToFile(NextAvailableFilenameNV('_corrtest.txt'));
      D_MegaMain.AnalysisSummary.AddCalculatedValue('CorrTest Score', Format('%.3e', [FCorrelationTest.Score]));
      D_MegaMain.AnalysisSummary.AddCalculatedValue('CorrTest P-value', FCorrelationTest.PvalueString);
      {$ELSE}
      FAnalysisInfo.AnalysisSummary.AddCalculatedValue('CorrTest Score', Format('%.3e', [FCorrelationTest.Score]));
      FAnalysisInfo.AnalysisSummary.AddCalculatedValue('CorrTest P-value', FCorrelationTest.PValueString);
      {$ENDIF}

      if Length(SampleTimes) = FAnalysisInfo.NoOfSeqs then
      begin
        ClockTreeExport.Clear;
        MLTreeAnalyzer.GenerateSampledTimesClockTreeExport(ClockTreeExport, FAnalysisInfo.MyOtuNames, FAnalysisInfo.MyOriTreeList.InternalNodeLbls[0], SampleTimes);
      end;
      FAnalysisInfo.ClockTreeExport := ClockTreeExport;
      FAnalysisInfo.ReltimeNexusExport := MLTreeAnalyzer.GenerateNexusReltimeExport(FAnalysisInfo.MyOtuNames, False);
      MLTreeAnalyzer.ModelInfoList.Insert(0,TModelInfo.Create);
      MLTreeAnalyzer.GetModelInfo(MLTreeAnalyzer.ModelInfoList[0]); // copy the active model info (for clock tree) into the newly created model info

      FAnalysisInfo.LogLikelihoodWithClock  := FloatToStrF(MLTreeAnalyzer.ModelInfoList[0].LogL, ffFixed, 12, lnLDigits);
      FAnalysisInfo.LogLikelihoodWithoutClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[1].LogL, ffFixed, 12, lnLDigits);
      FAnalysisInfo.InvarWithClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[0].Invar, ffFixed, 12, paraDigits);
      FAnalysisInfo.GammaWithClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[0].Gamma, ffFixed, 12, paraDigits);
      FAnalysisInfo.InvarWithoutClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[1].Invar, ffFixed, 12, paraDigits);
      FAnalysisInfo.GammaWithoutClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[1].Gamma, ffFixed, 12, paraDigits);
      FAnalysisInfo.FullModelName := MLTreeAnalyzer.ModelInfoList[0].FullName;
      FAnalysisInfo.ARP.Progress := 100;
    except
      on E: EAbort do
      begin
        FCanceled := True;
      end;
      On E : EOutOfMemory do // trap out of memory exceptions and handle it ourselves in the onterm procedure
      begin
        FExceptionName := E.ClassName;
        FExceptionMessage := E.Message;
      end;
      On E : Exception do
      begin
        FExceptionName := E.ClassName;
        FExceptionMessage := E.Message; // the thread terminator will write out the error message and clean up memory
      end;
    end;
  finally
    if Assigned(NodeLabels) then
      NodeLabels.Free;
  end;
end;

function TRelTimeMLThread.AnalysisDescription: String;
begin
  Result := 'Reltime';
end;

procedure TRelTimeMLThread.SetMergeRates(const Value: Boolean);
begin
  FMergeRates := Value;
end;

function TRelTimeMLThread.ProgressCallback(aProgress: Integer; aType: String; aInfo: String): Boolean;
begin
  FSamplingProgress := aProgress;
  Synchronize(DoProgressCallback);
  Result := FCanceled;
end;

procedure TRelTimeMLThread.DoProgressCallback;
begin
  if Assigned(FSamplingCheckCancel) then
    FCanceled := FSamplingCheckCancel(FSamplingProgress, 'Status', 'Computing Calibration Density Trees');
end;

function TRelTimeMLThread.SampleCalibrations(aData: TTreeData; calibs: TCalibrations; otuNames: TStringList; maxRateRatio: Extended): Boolean;
begin
  FCalibrationSampler := TCalibrationDensitySampler.Create(calibs, aData, otuNames, True);
  if Assigned(FAnalysisInfo) and Assigned(FAnalysisInfo.ARP) then
  begin
    FSamplingCheckCancel := FAnalysisInfo.ARP.ProgressAndStatusCheckCancel;
    FCalibrationSampler.CheckCancelFunc := ProgressCallback;
  end;
  Result := FCalibrationSampler.DoSampling;
  FCalibrationSampler.DoReltime(aData, maxRateRatio);
  FCalibrationSampler.OutputDeveloperData;
end;

{ TRelTimeLSThread }

procedure TRelTimeLSThread.SetAnalysisInfo(AValue: TAnalysisInfo);
begin
  FAnalysisInfo:=AValue;
end;

function TRelTimeLSThread.GetLogText: String;
begin
  Result := EmptyStr;
  if Assigned(FLog) then
    Result := FLog.Text;
end;

function TRelTimeLSThread.SampleCalibrations(aData: TTreeData; calibs: TCalibrations; otuNames: TStringList; MaxRateRatio: Extended): Boolean;
begin
  FCalibrationSampler := TCalibrationDensitySampler.Create(calibs, aData, otuNames, False);
  if Assigned(FAnalysisInfo) and Assigned(FAnalysisInfo.ARP) then
  begin
    FSamplingCheckCancel := FAnalysisInfo.ARP.ProgressAndStatusCheckCancel;
    FCalibrationSampler.CheckCancelFunc := ProgressCallback;
  end;
  Result := FCalibrationSampler.DoSampling;
  FCalibrationSampler.DoReltime(aData, MaxRateRatio);
  FCalibrationSampler.OutputDeveloperData;
end;

function TRelTimeLSThread.ProgressCallback(aProgress: Integer; aType: String; aInfo: String): Boolean;
begin
  FSamplingProgress := aProgress;
  Synchronize(DoProgressCallback);
  Result := FCanceled;
end;

procedure TRelTimeLSThread.DoProgressCallback;
begin
  if Assigned(FSamplingCheckCancel) then
    FCanceled := FSamplingCheckCancel(FSamplingProgress, 'Status', 'Computing Calibration Density Trees');
end;

procedure TRelTimeLSThread.DoSynchronizeErrorMessage;
begin
  {$IFDEF VISUAL_BUILD}
  ShowMessage(Format('Error (%s) in LS search thread: %s', [MyExceptionName, MyExceptionMessage]));
  {$ELSE}
  error_nv(Format('Error (%s) in LS search thread: %s', [MyExceptionName, MyExceptionMessage]));
  {$ENDIF}
end;

function TRelTimeLSThread.ComputeDistances: Boolean;
var
  i, j: Integer;
  ADMat : PDistanceMatrix = nil;
  DistComputer: TSeqDistBase = nil;
begin
  try
    try
      Result := False;
      ADMat := NewDistMatrix(FAnalysisInfo.MyNoOfSeqs, True);
      with FAnalysisInfo.MyDistPack do
        if DoesContain(gdOneNuc) then
          DistComputer := TNucDist.Create
        else if DoesContain(gdSynNonsyn)
          then DistComputer := TSynNonsynDist.Create
        else if DoesContain(gdAmino)
          then DistComputer := TAminoDist.Create;

      with DistComputer do
      begin
        DistPack  := FAnalysisInfo.MyDistPack;
        NoOfSeqs  := FAnalysisInfo.MyMappedData.Count;
        Sequences := FAnalysisInfo.MyMappedData;
        QuickExit := True;
        D         := ADMat;
        NoOfSites := FAnalysisInfo.MyNoOfSites;
      end;

      { code table is needed in some cases}
      if DistComputer is TSynNonsynDist  then
        with DistComputer as TSynNonsynDist do
          CodeTable := D_InputSeqData.CodeTable;

      UpdateRunStatusInfo('Status', 'Computing distances');
      DistComputer.SetRuntimeProgress(FAnalysisInfo.ARP);
      {$IFDEF VISUAL_BUILD}
      DistComputer.SetStopButton(FAnalysisInfo.ARP.StopBtn);
      {$ENDIF}

      try
        if DistComputer is TNucDist then
        begin
          if FAnalysisInfo.MyDistPack.DoesContain(gdMCL) then
            TNucDist(DistComputer).ComputeDistancesSE
          else
            TNucDist(DistComputer).ComputeDistances;
        end
        else if DistComputer is TSynNonsynDist then
          TSynNonsynDist(DistComputer).ComputeDistances
        else if DistComputer is TAminoDist     then
          TAminoDist(DistComputer).ComputeDistances;
      except
        on E : Exception do
        begin
          FIsSuccess := False;
          FLog.Add(E.Message);
          if E.ClassType = EOutOfMemory then
          begin
            {$IFNDEF VISUAL_BUILD}
            error_nv('MEGA cannot allocate enough memory for this analysis and is unable to complete the calculation', E);
            {$ENDIF}
            Exit;
          end
          else
          begin
            {$IFNDEF VISUAL_BUILD}
            error_nv('MEGA has encountered a calculation error and cannot continue with this analysis',  E);
            {$ENDIF}
            Exit;
          end;
        end;
      end;

      for i:=0 to FAnalysisInfo.MyNoOfSeqs-1 do
        ADMat[i,i] := 0;
      for i:=0 to FAnalysisInfo.MyNoOfSeqs-1 do
        for j:=0 to i-1 do
          ADMat[j,i] := ADMat[i,j];

      FAnalysisInfo.MyShowD := NewDistMatrix(FAnalysisInfo.MyNoOfSeqs, True);
      CopyDistMatrix(FAnalysisInfo.MyNoOfSeqs, FAnalysisInfo.MyShowD, ADMat, True);

      UpdateRunStatusInfo('Status', 'Computing LS tree');
      FAnalysisInfo.MyOriD := ADMat;
      ADMat := nil;
      Result := True;
      except
        on E:Exception do
        begin
          FIsSuccess := False;
          FLog.Add(E.Message);
        end;
      end;
    finally
      if Assigned(DistComputer) then
        DistComputer.Free;
    end;
end;

procedure TRelTimeLSThread.Search;
var
  OLSThread: TOLSBLenThread = nil;
  OLSAnalysisInfo: TAnalysisInfo = nil;
  RelTimeTree: TTreeData = nil;
  RelTimeComputer: TRelTimeComputer = nil;
  ClockExportList: TStringList = nil;
  TempNodeLabels: TStringList = nil;
  i: Integer;
  TempData: TTreeData = nil;
  HasCalibrations: Boolean;
  Rooter: TFpNodeTreeDataAdapter = nil;
  rootedTree: TTreeData = nil;
  MaxRateRatio: Extended;
  SamplingTimes: array of Extended;
  CorrelationTest: TCorrelationTest = nil;
begin
  SetLength(SamplingTimes, 0);
  if IsDeveloper then
    MaxRateRatio := FAnalysisInfo.MaxRateRatio
  else
    MaxRateRatio := DEFAULT_MAX_RATE_RATIO;
  TempNodeLabels := FAnalysisInfo.MyOriTreeList.InternalNodeLbls[0];
  try
    try
      if not ComputeDistances then
        raise Exception.Create('failed to compute distances');
      TempData := TTreeData.Create(FAnalysisInfo.MyOriTreeList.NoOfOTUs, True, False, False);
      TempData.Assign(FAnalysisInfo.MyOriTreeList[0]);
      OLSThread := TOLSBLenThread.Create(FAnalysisInfo.MyOriTreeList, FAnalysisInfo.MyOriD);
      OLSThread.FreeOnTerminate := False;
      OLSThread.UseInitialNJTree := False;
      OLSThread.MaxNoOfTrees := 1;
      OLSThread.ProgressDlg := FAnalysisInfo.ARP;
      OLSThread.ShowProgress := True;
      FAnalysisInfo.ARP.Thread := OLSThread;
      if not FAnalysisInfo.ARP.Visible then
        FAnalysisInfo.ARP.Show;
      OLSThread.Start;

      if OLSthread.WaitFor = 0 then
      begin
        OLSAnalysisInfo := OLSThread.ProgressDlg.FMAI;
        for i := 0 to FAnalysisInfo.MyOriTreeList.NoOfOTUs - 1 do
          FAnalysisInfo.MyOriTreeList[0].IsOutgroupMember[i] := TempData.IsOutgroupMember[i];
        Rooter := TFpNodeTreeDataAdapter.Create;
        Rooter.SetTreeData(FAnalysisInfo.MyOriTreeList[0]);
        Rooter.RootOnOutgroup;
        rootedTree := FAnalysisInfo.MyOriTreeList[0];
        Rooter.GetTreeData(rootedTree);
        ClockExportList := TStringList.Create;
        RelTimeComputer := TRelTimeComputer.Create;
        FAnalysisInfo.MyRelTimeComputer := RelTimeComputer;
        RelTimeComputer.ProgressProc := Self.OnProgress;
        RelTimeComputer.PropagateConstraints := FAnalysisInfo.PropagateConstraints;
        ComputeSiteCoverage(FAnalysisInfo);
        UpdateRunStatusInfo('Status', 'Computing RelTime divergence times');
        HasCalibrations := (Assigned(FAnalysisInfo.CalibrationTimes) and (FAnalysisInfo.CalibrationTimes.Count > 0));
        if HasCalibrations then
        begin
          if FAnalysisInfo.CalibrationTimes.IsSampleTimes then
          begin
            FAnalysisInfo.CalibrationTimes.PrepareSamplingTimeArray(SamplingTimes, OLSAnalysisInfo.MyOtuNames);
            ReltimeTree := ReltimeComputer.ComputeRelTimeBLens(OLSAnalysisInfo.MyOriTreeList[0], SamplingTimes, MaxRateRatio, FAnalysisInfo.MyOtuNames);
            if not Assigned(RelTimeTree) then
              if ReltimeComputer.Log.Text <> EmptyStr then
                raise Exception.Create(ReltimeComputer.Log.Text)
              else
                raise Exception.Create('Reltime computation failed with an unknown error');
            ReltimeComputer.GenerateSampledTimesClockTreeExport(ClockExportList, OLSAnalysisInfo.MyOtuNames, TempNodeLabels, SamplingTimes);
          end
          else
          begin
            if FAnalysisInfo.CalibrationTimes.GetUsesCalibrationDensities then
              if not SampleCalibrations(OLSAnalysisInfo.MyOriTreeList[0], FAnalysisInfo.CalibrationTimes, OLSAnalysisInfo.MyOtuNames, MaxRateRatio) then
                raise Exception.Create('Sampling of calibration densities failed');
            RelTimeTree := RelTimeComputer.ComputeRelTimeBLens(OLSAnalysisInfo.MyOriTreeList[0], ClockExportList, OLSAnalysisInfo.MyOtuNames, TempNodeLabels, FAnalysisInfo.CalibrationTimes, MaxRateRatio);
            if not Assigned(RelTimeTree) then
              if ReltimeComputer.Log.Text <> EmptyStr then
                raise Exception.Create(ReltimeComputer.Log.Text)
              else
                raise Exception.Create('Reltime computation failed with an unknown error');
          end;
        end
        else
          RelTimeTree := RelTimeComputer.ComputeRelTimeBLens(OLSAnalysisInfo.MyOriTreeList[0], ClockExportList, OLSAnalysisInfo.MyOtuNames, TempNodeLabels, MaxRateRatio);
        if not Assigned(RelTimeTree) then
          if ReltimeComputer.Log.Text <> EmptyStr then
            raise Exception.Create(ReltimeComputer.Log.Text)
          else
            raise Exception.Create('Reltime computation failed with an unknown error');
        if not Assigned(FAnalysisInfo.ClockTreeExport) then
          FAnalysisInfo.ClockTreeExport := TStringList.Create
        else
          FAnalysisInfo.ClockTreeExport.Clear;
        FAnalysisInfo.ClockTreeExport.Assign(ClockExportList);
        CorrelationTest := ReltimeComputer.AutomaticCorrelationTest(MaxRateRatio);
        {$IFNDEF VISUAL_BUILD}
        CorrelationTest.SaveSummaryToFile(NextAvailableFilenameNV('_corrtest.txt'));
        D_MegaMain.AnalysisSummary.AddCalculatedValue('CorrTest Score', Format('%.3e', [CorrelationTest.Score]));
        D_MegaMain.AnalysisSummary.AddCalculatedValue('CorrTest P-value', CorrelationTest.PvalueString);
        {$ELSE}
        FAnalysisInfo.AnalysisSummary.AddCalculatedValue('CorrTest Score', Format('%.3e', [CorrelationTest.Score]));
        FAnalysisInfo.AnalysisSummary.AddCalculatedValue('CorrTest P-value', CorrelationTest.PValueString);
        {$ENDIF}
        FAnalysisInfo.ReltimeNexusExport := RelTimeComputer.GenerateNexusReltimeExport(FAnalysisInfo.MyOtuNames, HasCalibrations, True);
        OLSAnalysisInfo := nil;
        FAnalysisInfo.MyOriTreeList.Insert(0, RelTimeTree);
        FAnalysisInfo.MyOriTreeList[1].AssignDataCoverage(ReltimeTree); { same tree so no need to recompute}
        UpdateRunStatusInfo('Status', 'Preparing results');
        OnProgress(99);
      end
      else
        raise Exception.Create('Unknown error in TOLSThread');
    except
      on E: EAbort do
      begin
        FCanceled := True;
      end;
      on E:Exception do
      begin
        MyExceptionName := E.ClassName;
        MyExceptionMessage := E.Message + ': ' + LogText;
      end;
    end;
  finally
    if Assigned(ClockExportList) then
      FreeAndNil(ClockExportList);
    if Assigned(OLSThread) then
      OLSThread.Free;
    if Assigned(TempData) then
      TempData.Free;
    if Assigned(Rooter) then
      Rooter.Free;
    if Assigned(CorrelationTest) then
      CorrelationTest.Free;
  end;
end;

function TRelTimeLSThread.AnalysisDescription: String;
begin
  Result := 'Reltime(OLS)';
end;

procedure TRelTimeLSThread.SynchronizeErrorMessage(E: Exception);
begin
  MyExceptionName := E.ClassName;
  MyExceptionMessage := E.Message;
  Synchronize(DoSynchronizeErrorMessage);
end;

constructor TRelTimeLSThread.Create(CreateSuspended: Boolean);
begin
  inherited Create;
  FAnalysisInfo := nil;
  FCalibrationSampler := nil;
  FSamplingProgress := 0;
  FSamplingCheckCancel := nil;
  FLog := TStringList.Create;
end;

destructor TRelTimeLSThread.Destroy;
begin
  if Assigned(FLog) then
    FLog.Free;
  if Assigned(FCalibrationSampler) then
    FCalibrationSampler.Free;
  inherited;
end;

procedure TRelTimeLSThread.Execute;
begin
  Search;
end;

{ TAnchoredRelTimeMLThread }

procedure TAnchoredRelTimeMLThread.SetMaxTimes(AValue: TDivTimesArray);
var
  i: Integer;
begin
  FMaxTImes:=AValue;
  SetLength(FOrigMaxTimes, Length(AValue));
  if Length(FOrigMaxTimes) > 0 then
    for i := 0 to Length(FOrigMaxTimes) - 1 do
      FOrigMaxTimes[i] := FMaxTimes[i];
end;

procedure TAnchoredRelTimeMLThread.SetMinTimes(AValue: TDivTimesArray);
var
  i: Integer;
begin
  FMinTimes:=AValue;
  SetLength(FOrigMinTimes, Length(AValue));
  if Length(FOrigMinTimes) > 0 then
    for i := 0 to Length(FOrigMinTimes) - 1 do
      FOrigMinTimes[i] := FMinTimes[i];
end;

function TAnchoredRelTimeMLThread.Initialize: Boolean;
begin
  Result:=inherited Initialize;
end;

procedure TAnchoredRelTimeMLThread.Search;
const
  lnLDigits = 3;
  paraDigits = 2;
var
  tempLabels: TStringList = nil;
  i: Integer = -1;
  ClockTree: TTreeData = nil;
  UserTree: TTreeData = nil;
  ClockTreeExport: TStringList = nil;
  lbls: TStringList = nil;
begin
  try
    if not Initialize then
      raise Exception.Create('Failed to initialize TAnchoredReltimeMLThread');
    // do the analysis of the user tree
    MLTreeAnalyzer.InitTree.isSE := True;
    FAnalysisInfo.MyOriTreeList.isSE := True;
    MLTreeAnalyzer.InitTree.isStats := False;
    FAnalysisInfo.MyOriTreeList.isStats := False;
    FStatus := 'Optimizing user tree';
    MLTreeAnalyzer.OptimizeMessage := 'Optimizing user tree';
    FAnalysisInfo.MergeRates := FMergeRates;
    MLTreeAnalyzer.Initialize;
    if OptimizeParams then
      MLTreeAnalyzer.Optimize
    else
      MLTreeAnalyzer.OptimizeBLens;
    // end of user tree analysis

    if Self.Terminated or Self.Canceled then
      Exit;

    // now make a clock tree from the ML optimized user tree
    FAnalysisInfo.MyOriTreeList.ValueName := 'LogL';
    MLTreeAnalyzer.ModelInfoList.Add(TModelInfo.Create); // put the non-clock tree stuff at index 1 so we don't have to set the index for TTreeViewForm
    MLTreeAnalyzer.GetModelInfo(MLTreeAnalyzer.ModelInfoList[0]);
    if FAnalysisInfo.MyBootReps > 0 then
      UpdateRunStatusInfo('status', 'Constructing  bootstrap timetrees')
    else
      UpdateRunStatusInfo('status', 'Constructing timetree');

    ClockTree := TTreeData.Create(FAnalysisInfo.NoOfSeqs, True, True, True);
    UserTree := TTreeData.Create(FAnalysisInfo.NoOfSeqs, True, True, False, False);

    ClockTreeExport := TStringList.Create;
    MLTreeAnalyzer.MLTree.GetTreeData(ClockTree);
    MLTreeAnalyzer.MLTree.GetTreeData(UserTree);
    FAnalysisInfo.MyOriTreeList.Delete(0); // dispose of the user's tree since we have recalculated branch lengths
    FAnalysisInfo.MyOriTreeList.Insert(0, UserTree);
    FAnalysisInfo.MyOriTreeList.Insert(0, ClockTree);
    ComputeSiteCoverage(FAnalysisInfo, ClockTree);
    UserTree.AssignDataCoverage(ClockTree); { same tree so no need to recompute}
    UpdateRunStatusInfo('Status', 'Constructing timetree');
    FAnalysisInfo.ARP.Progress := 1;
    FAnalysisInfo.MySeqStrings := nil; { this gets freed by MLAnalyzer}
    if FAnalysisInfo.CalibrationTimes.GetUsesCalibrationDensities then
      if not SampleCalibrations(ClockTree,FAnalysisInfo.CalibrationTimes, FAnalysisInfo.MyOriTreeList.OtuNameList, FAnalysisInfo.MaxRateRatio) then
        raise Exception.Create('failed to sample calibration times');
    lbls := FAnalysisInfo.MyOriTreeList[0].GetInternalNodeLabels;
    FAnalysisInfo.CalibrationTimes.PrepareDivTimeArrays(FMinTimes, FMaxTimes, ClockTree.NoOfOTUs, False, lbls);
    if IsDeveloper then
    begin
      MLTreeAnalyzer.MLTree.PropagateConstraints := FAnalysisInfo.PropagateConstraints;
      FIsSuccess := MLTreeAnalyzer.AnchorClockTree(ClockTree, FMinTimes, FMaxTimes, FAnalysisInfo.MaxRateRatio);
    end
    else
      FIsSuccess := MLTreeAnalyzer.AnchorClockTree(ClockTree, FMinTimes, FMaxTimes, DEFAULT_MAX_RATE_RATIO);
    if not FIsSuccess then
      raise Exception.Create('Unable to calculate divergence times. Please check the provided calibration constraints');
    if IsDeveloper then
      MLTreeAnalyzer.GenerateCalibratedClockTreeExport(ClockTreeExport, FAnalysisInfo.MyOtuNames, FAnalysisInfo.MyOriTreeList.InternalNodeLbls[0], FMinTimes, FMaxTimes)
    else
      MLTreeAnalyzer.GenerateCalibratedClockTreeExport(ClockTreeExport, FAnalysisInfo.MyOtuNames, FAnalysisInfo.MyOriTreeList.InternalNodeLbls[0], FOrigMinTimes, FOrigMaxTimes);
    FCorrelationTest := MLTreeAnalyzer.MLTree.ReltimeComputer.AutomaticCorrelationTest(FAnalysisInfo.MaxRateRatio);
    {$IFNDEF VISUAL_BUILD}
    FCorrelationTest.SaveSummaryToFile(NextAvailableFilenameNV('_corrtest.txt'));
    D_MegaMain.AnalysisSummary.AddCalculatedValue('CorrTest Score', Format('%.3e', [FCorrelationTest.Score]));
    D_MegaMain.AnalysisSummary.AddCalculatedValue('CorrTest P-value', FCorrelationTest.PvalueString);
    {$ELSE}
    FAnalysisInfo.AnalysisSummary.AddCalculatedValue('CorrTest Score', Format('%.3e', [FCorrelationTest.Score]));
    FAnalysisInfo.AnalysisSummary.AddCalculatedValue('CorrTest P-value', FCorrelationTest.PValueString);
    {$ENDIF}
    FAnalysisInfo.ReltimeNexusExport := MLTreeAnalyzer.GenerateNexusReltimeExport(FAnalysisInfo.MyOtuNames, True);
    FAnalysisInfo.ClockTreeExport := ClockTreeExport;

    MLTreeAnalyzer.ModelInfoList.Insert(0,TModelInfo.Create);
    MLTreeAnalyzer.GetModelInfo(MLTreeAnalyzer.ModelInfoList[0]); // copy the active model info (for clock tree) into the newly created model info

    FAnalysisInfo.LogLikelihoodWithClock  := FloatToStrF(MLTreeAnalyzer.ModelInfoList[0].LogL, ffFixed, 12, lnLDigits);
    FAnalysisInfo.LogLikelihoodWithoutClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[1].LogL, ffFixed, 12, lnLDigits);
    FAnalysisInfo.InvarWithClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[0].Invar, ffFixed, 12, paraDigits);
    FAnalysisInfo.GammaWithClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[0].Gamma, ffFixed, 12, paraDigits);
    FAnalysisInfo.InvarWithoutClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[1].Invar, ffFixed, 12, paraDigits);
    FAnalysisInfo.GammaWithoutClock := FloatToStrF(MLTreeAnalyzer.ModelInfoList[1].Gamma, ffFixed, 12, paraDigits);
    FAnalysisInfo.FullModelName := MLTreeAnalyzer.ModelInfoList[0].FullName;
    FAnalysisInfo.ARP.Progress := 100;
  except
    on E: EAbort do
    begin
      FCanceled := True;
    end;
    On E : Exception do
    begin
      {$IFNDEF VISUAL_BUILD}
      error_nv('Failure in Reltime analysis', E);
      {$ENDIF}
      FExceptionName := E.ClassName;
      FExceptionMessage := E.Message; // the thread terminator will write out the error message and clean up memory
    end;
  end;
end;

function TAnchoredRelTimeMLThread.AnalysisDescription: String;
begin
  Result := 'Calibrated Reltime';
end;


{ TAnchorMLReltimeTreeThread }

function TAnchorMLReltimeTreeThread.Initialize: Boolean;
begin
  Result := True;
  { we want to skip this as we already have a fully initialized TMLTreeAnalyzer}
  //Result:=inherited Initialize;
end;

procedure TAnchorMLReltimeTreeThread.Search;
var
  lbls: TStringList = nil;
begin
  try
    UpdateRunStatusInfo('Status', 'Anchoring timetree');
    FAnalysisInfo.ARP.Progress := 1;
    lbls := FAnalysisInfo.MyOriTreeList.InternalNodeLbls[0];
    Calibrations.PrepareDivTimeArrays(FMinTimes, FMaxTimes, FAnalysisInfo.NoOfSeqs, False, lbls);
    if IsDeveloper then
      FIsSuccess := MLTreeAnalyzer.AnchorExistingClockTree(ClockTree, MinTimes, MaxTimes, FAnalysisInfo.MaxRateRatio)
    else
      FIsSuccess := MLTreeAnalyzer.AnchorExistingClockTree(ClockTree, MinTimes, MaxTimes, DEFAULT_MAX_RATE_RATIO);
    if not FIsSuccess then
      raise Exception.Create('Unable to calculate divergence times. Please check the provided calibration constraints');
  except
    On E : Exception do
    begin
      FExceptionName := E.ClassName;
      FExceptionMessage := E.Message; // the thread terminator will write out the error message and clean up memory
    end;
  end;
end;

end.
