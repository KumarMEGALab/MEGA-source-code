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

unit ProcessTreeCmds;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface


uses
  Classes, MAnalysisInfo, MDistPack, DateUtils, MProcessPack, MCalibrationData,
  MRuntimeProgressDlg, MTreeData;

type

  { TRelTimeTerminator }

  TRelTimeTerminator = class(TObject)
  private
    FOperation: TDistTreeDlgOption;
  public
    procedure OnThreadDone(Thread: TObject);
    procedure OnBlensThreadDone(Thread: TObject);
    procedure OnLeastSquaresThreadDone(Thread: TObject);
    procedure OnCorrTestThreadDone(Thread: TObject);
    constructor Create(Operation: TDistTreeDlgOption);
    destructor Destroy; override;
    property Operation: TDistTreeDlgOption read FOperation;
  end;

  { TTreeThreadTerminator }

  TTreeThreadTerminator = class(TObject)
  public
    procedure OnThreadDone(Thread: TObject);
    procedure OnBootstrapThreadDone(thread: TObject);
  end;

procedure ProcessSeqDistPhylogenyCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack);
procedure ProcessSeqRelTimeCCCommand(UsrOperation: TDistTreeDlgOption; ProcessPack: TProcessPack);
procedure ProcessDistRelTimeCCCommand(UsrOperation: TDistTreeDlgOption; ProcessPack: TProcessPack);
procedure ProcessDistDistPhylogenyCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack);
procedure ProcessFourClusterAnalysisCommand(AnalysisType: AnsiString);
procedure ProcessPhyloQSeqCommand(ProcessPack: TProcessPack);
procedure ProcessPhylogenyBLensCommand(UsrOperation: TDistTreeDlgOption; ProcessPack: TProcessPack);
procedure ComputeRelTimeBLens(MAI: TAnalysisInfo); overload;
procedure ComputeRelTimeBLens(MAI: TAnalysisInfo; Calibrations: TCalibrations); overload;
procedure DoReltimeLeastSquares(MAI: TAnalysisInfo);
procedure RunCorrTest(MAI: TAnalysisInfo);
function GetInputDataFileName: String;
function GetInputDataTitle: String;
procedure RootOnOutgroup(MAI: TAnalysisInfo); overload;
procedure RootOnOutgroup(aData: TTreeData); overload;
function CheckAbortForIdenticalSeqsLS(aInfo: TAnalysisInfo): Boolean;
procedure ComputeRelTimeLSTree(MAI: TAnalysisInfo; IsUpdateARP: Boolean);
procedure ComputeTree(MAI: TAnalysisInfo; IsUpdateARP: Boolean);
procedure ConstructNJTree(MAI: TAnalysisInfo; IsShowProgress: Boolean);
procedure ConstructUPGMATree(MAI: TAnalysisInfo;IsShowProgress: Boolean);
procedure ConstructMETree(MAI: TAnalysisInfo; IsShowProgress: Boolean);
procedure AnalyzeOLSUserTree(MAI: TAnalysisInfo);
procedure SearchLastOTUPositionInUserTree(MAI: TAnalysisInfo);
procedure ConductCPTest(MAI: TAnalysisInfo);
procedure ComputeSiteCoverage(ProcessPack: TProcessPack);
procedure ComputeBootstrapTree(MAI: TAnalysisInfo; DoFullProgress: Boolean=False);
procedure BootstrapNJ(MAI: TAnalysisInfo; DoFullProgress: Boolean);
procedure BootstrapUPGMA(MAI: TAnalysisInfo; DoFullProgress: Boolean);
procedure BootstrapME(MAI: TAnalysisInfo);
function  BootstrapSampleD(ARP: TRuntimeProgress): Boolean;

implementation

uses
  {$IFDEF VISUAL_BUILD}Mega_Main,{$ELSE} MD_MegaMain, MegaUtils_NV,{$ENDIF}
  LCLIntf, LCLType, SysUtils, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons, MegaUtils, MegaConsts,
  MegaErrUtils, MSeqDistBase, MAminoDist, MNucDist, MSynNonsynDist,
  MD_InputSeqData, MD_InputDistData, MTreeList, MPartitionList,
  MTreePack, MTreeSearchThread, ShowTrees, ErrorMessages_HC, MD_Align, mreltimethreads, MReltimeComputer,
  MTreeDataAdapter, msitecoverage, mcrossprojectutils,
  mcheck_abort_reltime, mdist_command_threads, MAnalysisSummary;

procedure TTreeThreadTerminator.OnThreadDone(Thread: TObject);
var
  MAI: TAnalysisInfo = nil;
  t: TTreeSearchThread = nil;
  IsBootstrap: Boolean;
begin
  t := TTreeSearchThread(Thread);


  MAI := t.ProgressDlg.FMAI;
  IsBootstrap := (MAI.MyTreePack.DoesContain(ttBootstrap) or MAI.MyTreePack.DoesContain(ttCPTest));
  try
    try
      begin
        if (not (Thread as TTreeSearchThread).Canceled) or IsBootstrap then
        begin
          {$IFDEF VISUAL_BUILD}
          MAI.ARP.UpdateRunStatusInfo('Status', 'Preparing display...');
          MAI.ARP.Refresh;
          {$ELSE}
          MAI.ARP.UpdateRunStatusInfo('Status', 'Preparing Newick file data...');
          {$ENDIF}
          if MAI.MyTreePack.DoesContain(ttBootstrap) then
          begin
            MAI.MyValidReps := Round(MAI.MyBootPartitionList.TotalFrequency);
            ShowDistBootTree(MAI);
          end
          else if MAI.MyTreePack.DoesContain(ttCPTest) then
          begin
            ShowDistTree(MAI);
          end
          else
            ShowDistTree(MAI);
          MAI := nil;
        end;
      end;
    except
      on E:Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        ShowMessage('Oh no! An error occurred when finalizing the calculation: ' + E.Message);
        {$ELSE}
        error_nv('Oh no! An error occurred when finalizing the calculation: ' + E.Message, E);
        {$ENDIF}
      end;
    end;
  finally
    {$IFDEF VISUAL_BUILD}
    if IsBootstrap then
      MegaForm.MultithreadedCalcIsRunning := False;
    {$ENDIF}
    if Assigned(MAI) and Assigned(MAI.ARP) then { an exception occurred. If MAI.ARP is nil, then MAI was freed in ShowTrees.pas}
    begin
      MAI.ARP.Free;
      FreeAndNil(MAI);
    end;

    {$IFDEF VISUAL_BUILD}
    if Assigned(t) and (not t.FreeOnTerminate) then
    begin
      t.FreeOnTerminate := True;
      t.Terminate;
    end;
    Self.Free;
    {$ELSE}
    try { for some reason TThread.Free hangs on Linux for megacc so instead we just let the application terminate}
      NumActiveThreadsCS.Acquire;
      RunningThreadCount := RunningThreadCount - 1;
    finally
      NumActiveThreadsCS.Release;
    end;
    {$ENDIF}
  end;
end;

procedure TTreeThreadTerminator.OnBootstrapThreadDone(thread: TObject);
{$IFDEF MPI_MEGA}
var
  t: TBootstrapCustomTreeThread = nil;
  siteCoverageThread: TSiteCoverageThread = nil;
  MAI: TAnalysisInfo = nil;
{$ENDIF}
begin
  {$IFDEF MPI_MEGA}
  {$ELSE}
  OnThreadDone(thread);
  {$ENDIF}
end;

{ TRelTimeTerminator }

constructor TRelTimeTerminator.Create(Operation: TDistTreeDlgOption);
const
  ValidOps: set of TDistTreeDlgOption = [dtdoRelTimeML, dtdoRelTimeBLens, dtdoRelTimeLS, dtdoRtdtML, dtdoRtdtLS, dtdoRtdtBlens, dtdoCalTest, dtdoCorrTestML, dtdoCorrTestBlens, dtdoLbsTiming];
begin
  if not (Operation in ValidOps) then
    raise Exception.Create('Developer Error: Invalid operation supplied to TRelTimeTerminator');
  FOperation := Operation;
end;


destructor TRelTimeTerminator.Destroy;
begin
  inherited;
end;

procedure TRelTimeTerminator.OnBlensThreadDone(Thread: TObject);
var
  AnalysisInfo: TAnalysisInfo = nil;
  blenThread: TRelTimeBLenThread = nil;
begin
  Assert((Operation = dtdoRelTimeBLens) or (Operation = dtdoRtdtBlens), 'Invalid OnTerminate procedure called for Reltime thread');

  try
    blenThread := TRelTimeBLenThread(Thread);
    AnalysisInfo := blenThread.AnalysisInfo;

    if blenThread.MyExceptionName <> 'none' then
    begin
      if Assigned(AnalysisInfo) and Assigned(AnalysisInfo.ARP) then
        AnalysisInfo.ARP.Free;
      {$IFDEF VISUAL_BUILD}
      raise Exception.Create(blenThread.MyExceptionMessage);
      {$ELSE}
      error_nv('Reltime analysis failed: ' + blenThread.MyExceptionMessage);
      {$ENDIF}
    end
    else
    begin
      if blenThread.Canceled = False then
      begin
        if AnalysisInfo.UseFixedEvolutionaryRate then
          AnalysisInfo.MyRelTimeComputer.StrictClockRate := AnalysisInfo.FixedEvolutionaryRate;
        AnalysisInfo.ClockType := ctLocal;
        blenThread.AnalysisInfo := nil; { otherwise it will be freed in the Destructor}
        ShowRelTimeBLenResults(AnalysisInfo);
      end;
    end;
  Except
    on E: Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      blenThread.SynchronizeErrorMessage(E);
      {$ELSE}
      Error_NV(E.Message, E);
      {$ENDIF}
    end;
  end;
end;

procedure TRelTimeTerminator.OnLeastSquaresThreadDone(Thread: TObject);
var
  AnalysisInfo: TAnalysisInfo;
  MyExceptionName: String;
  MyExceptionMessage: String;
  aThread: TRelTimeLSThread;
begin
  AnalysisInfo := nil;
  aThread := (Thread as TRelTimeLSThread);
  try
    MyExceptionName := aThread.MyExceptionName;
    if MyExceptionName <> EmptyStr then
    begin
      try
        AnalysisInfo := aThread.AnalysisInfo;
        if Assigned(AnalysisInfo.ARP) then
          AnalysisInfo.ARP.Free;
        AnalysisInfo.Free;
      except

      end;
      MyExceptionMessage := aThread.MyExceptionMessage;
      if MyExceptionName = 'EOutOfMemory' then
      begin

        ReportThreadCrash(MyExceptionName, MyExceptionMessage);
      end
      else
      begin
        {$IFDEF VISUAL_BUILD}
        raise Exception.Create(MyExceptionMessage);
        {$ELSE}
        error_nv(aThread.MyExceptionMessage);
        {$ENDIF}
      end;
    end
    else
    begin
      if aThread.Canceled = False then
      begin
        AnalysisInfo := aThread.AnalysisInfo;
        if AnalysisInfo.UseFixedEvolutionaryRate then
          AnalysisInfo.MyRelTimeComputer.StrictClockRate := AnalysisInfo.FixedEvolutionaryRate;
        AnalysisInfo.ClockType := ctLocal;
        ShowRelTimeLSResults(AnalysisInfo);
      end;
    end;
  Except
    on E: Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      aThread.SynchronizeErrorMessage(E);
      {$ELSE}
      Error_NV(E.Message, E);
      {$ENDIF}
    end;
  end;
end;

procedure TRelTimeTerminator.OnCorrTestThreadDone(Thread: TObject);
var
  ctestThread: TCorrTestMLThread;
  ctestBlenThread: TCorrTestBlenThread;
  MAI: TAnalysisInfo;
begin
  if Thread is TCorrTestMLThread then
  begin
    ctestThread := TCorrTestMLThread(Thread);
    MAI := ctestThread.AnalysisInfo;
    {$IFDEF VISUAL_BUILD}
    if MegaForm.MultithreadedCalcIsRunning = True then
      MegaForm.MultithreadedCalcIsRunning := False
    else
      MegaForm.NumSingleThreadedMLAnalyses := MegaForm.NumSingleThreadedMLAnalyses - 1;
    {$ENDIF}
    ShowCorrTestMLResult(MAI, ctestThread.CorrelationTest);
  end
  else if Thread is TCorrTestBlenThread then
  begin
    ctestBlenThread := TCorrTestBlenThread(Thread);
    MAI := ctestBlenThread.AnalysisInfo;
    ShowCorrTestBLenResult(MAI, ctestBlenThread.CorrelationTest);
  end;
end; 


procedure TRelTimeTerminator.OnThreadDone(Thread: TObject);
var
  AnalysisInfo: TAnalysisInfo = nil;
  ReltimeThread: TRelTimeMLThread = nil;
begin
  Assert(Thread is TRelTimeMLThread);
  try
    try
      ReltimeThread := TRelTimeMLThread(Thread);
      AnalysisInfo := ReltimeThread.AnalysisInfo;
      if not ReltimeThread.IsSuccess then
      begin
        if Assigned(AnalysisInfo) and Assigned(AnalysisInfo.ARP) then
        begin
          AnalysisInfo.ARP.Free;
          AnalysisInfo.Free;
        end;
        raise Exception.Create(ReltimeThread.LogText);
      end
      else
      begin
        if ReltimeThread.Canceled = False then
        begin
          AnalysisInfo.ClockType := ctLocal;
          if ReltimeThread.IsSuccess then
          begin
            if AnalysisInfo.UseFixedEvolutionaryRate then
              ReltimeThread.MLTreeAnalyzer.MLTree.ReltimeComputer.StrictClockRate := AnalysisInfo.FixedEvolutionaryRate;
            if IsDeveloper and Assigned(ReltimeThread.CorrelationTest) then
              ShowRelTimeMLResults(AnalysisInfo, ReltimeThread.CorrelationTest)
            else
              ShowRelTimeMLResults(AnalysisInfo);
          end;
        end
        else
          FreeAndNil(AnalysisInfo);
      end;
    except
      on E:Exception do
        ReltimeThread.SynchronizeErrorMessage(E);
    end;
  finally
    {$IFDEF VISUAL_BUILD}
    if MegaForm.MultithreadedCalcIsRunning = True then
      MegaForm.MultithreadedCalcIsRunning := False
    else
      MegaForm.NumSingleThreadedMLAnalyses := MegaForm.NumSingleThreadedMLAnalyses - 1;
    {$ENDIF}
  end;
end;

procedure ProcessSeqDistPhylogenyCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack);
var
  {$IFNDEF VISUAL_BUILD}
  AlignCodons: Boolean;
  IsDna: Boolean;
  NewNewickTree: AnsiString;
  SwappedOtuNameList: TStringList = nil;
  {$ENDIF}
  i: Integer;
  ADMat : PDistanceMatrix = nil;
  DistComputer: TSeqDistBase = nil;
  ATreeList: TTreeList = nil;
  ARP : TRuntimeProgress = nil;
  MAI : TAnalysisInfo = nil;
  Calibrations: TCalibrations = nil;
  ACalibration: TCalibrationTime = nil;
  CalibrationFile: String = '';
  searchThread: TDistTreeSearchThread = nil;
begin
  try try
    {$IFNDEF VISUAL_BUILD}
    if UsrOperation = dtdoPhyloQ then
      MAI := TPhyloQAnalysisInfo.Create // subclassed TAnalysisInfo to simplify our lives.
    else
    {$ENDIF}
      MAI := TAnalysisInfo.Create;

    MAI.DataFilename := GetInputDataFileName;
    MAI.MyProcessPack := ProcessPack;
    MAI.PropagateConstraints := (not ProcessPack.HasProcessType(ppSkipMinMax));
    MAI.InitialUsrOperation := UsrOperation;

    ARP := TRuntimeProgress.Create(Application.MainForm);
    ARP.DataFileName  :=  GetInputDataFileName;
    ARP.DataTitle     :=  GetInputDataTitle;
    MAI.ARP := ARP;
    ARP.FMAI := MAI;

    if not MAI.GetAnalysisOptions(UsrOperation) then
    begin
      if Assigned(MAI.MyProcessPack) and MAI.MyProcessPack.IsGettingWorkflowSettings then
        MAI := nil;
      Exit;
    end;
    MAI.AnalysisSummary.NumTaxa := MAI.NoOfTaxa;
    MAI.AnalysisSummary.NumSites := MAI.NoOfSites;
    if MAI.MyProcessPack.HasProcessType(ppBuildTumorTree) then
      MAI.MyTreePack.AddType(ttTumorTree);
    ARP.WriteAnalysisOptionsToStdOut;

    {$IFNDEF VISUAL_BUILD}
    AlignCodons := False;
    IsDna := not MAI.isAminoAcid;
    if UsrOperation = dtdoPhyloQ then
    begin
      TPhyloQAnalysisInfo(MAI).ReadTargetData;
      if TPhyloQAnalysisInfo(MAI).MyProcessPack.ContainsProcessType(ppMuscle) then
        TPhyloQAnalysisInfo(MAI).PairWiseAlignments := MuscleAlignPairwiseExecute(TPhyloQAnalysisInfo(MAI).MyProcessPack, AlignCodons, IsDna, '', TPhyloQAnalysisInfo(MAI))
      else if TPhyloQAnalysisInfo(MAI).MyProcessPack.ContainsProcessType(ppClustalW) then
        TPhyloQAnalysisInfo(MAI).PairWiseAlignments := ClustalWAlignPairwiseExecute(TPhyloQAnalysisInfo(MAI))
      else
        Error_NV('Unable to determine alignment method for pairwise alignments. Please check your settings file.');
    end;
    {$ENDIF}

    ARP.AddRunStatusInfo('Status', 'Preparing data');
    ARP.Show;
    MAI.MyMappedData := TList.Create;

    if MAI.MyTreePack.DoesContain(ttUserTree) then
    begin
      if MAI.GetUserTreeInput(UsrOperation, MAI.MyUserTreeFile, MAI.ARP, nil, ttNJ, (UsrOperation = dtdoOLSComputeUserTreeBLens) or (UsrOperation = dtdoOLSInteriorBranchTest)) then
      begin
        {$IFNDEF VISUAL_BUILD}
        if UsrOperation = dtdoPhyloQ then
        begin
          NewNewickTree := AddTaxaNameToNewickString(TPhyloQAnalysisInfo(MAI).PhyloQTargetName, MAI.MyUserNewickTree);
          SwappedOtuNameList := TStringList.Create;
          SwappedOtuNameList.Assign(MAI.MyOtuNames);
          SwappedOtuNameList.Insert(0,TPhyloQAnalysisInfo(MAI).PhyloQTargetName);
          SwappedOtuNameList.Exchange(0, SwappedOtuNameList.Count - 1);
          MAI.MyOriTreeList := TTreeList.Create;
          MAI.MyOriTreeList.ImportFromNewick(NewNewickTree, SwappedOtuNameList);
        end
        else
        begin
          MAI.MyOriTreeList := TTreeList.Create;
          MAI.MyOriTreeList.ImportFromNewick(MAI.MyUserNewickTree, MAI.MyOtuNames);
        end;
        {$ELSE}
          MAI.MyOriTreeList := TTreeList.Create;
          MAI.MyOriTreeList.ImportFromNewick(MAI.MyUserNewickTree, MAI.MyOtuNames);
        {$ENDIF}

        if MAI.MyProcessPack.CalibrationFile <> EmptyStr then  // for MEGA-CC
          CalibrationFile := MAI.MyProcessPack.CalibrationFile
        else if MAI.CalibrationFile <> EmptyStr then // for MEGA-Visual
          CalibrationFile := MAI.CalibrationFile;
        if CalibrationFile <> EmptyStr then
        begin
          Calibrations := TCalibrations.Create;
          Calibrations.SetInfo(MAI);
          if not Calibrations.LoadFromFile(CalibrationFile) then
            raise Exception.Create('Failed to load calibrations');
          {$IFNDEF VISUAL_BUILD}
          Calibrations.FilterMissingTaxa(MAI.MyOtuNames);
          {$ENDIF}
          for i := 0 to Calibrations.Count - 1 do
          begin
            ACalibration := Calibrations.GetCalibration(i);
            MAI.MyOriTreeList.FinalizeCalibration(ACalibration);
          end;
          MAI.CalibrationTimes := Calibrations;
        end;
      end
      else
      begin
        ShowMessage('User cancelled the calculation');
        Exit;
      end;
    end;
    D_InputSeqData.ARP := ARP;
    MAI.MyIncludedSites := D_InputSeqData.PrepareDataForDistAnalysis(MAI.MySubsetOpt, MAI.MyMappedData, MAI.MyUsedOtuInfos, MAI.MyNoOfSeqs, MAI.MyNoOfSites,MAI.MyLabelsUsed, MAI.RecodeScheme, MAI.SiteCoverage);
    if MAI.MyNoOfSites < 1 then
      RaiseErrorMessage(HC_No_Common_Sites, 'No common sites found for computing distances. ' + NoCommonSitesStr);
    ARP.AddAnalysisOptions('No. of Sites', Format('%.0n', [MAI.MyNoOfSites*1.0]));

    if MAI.MyTreePack.DoesContain(ttBootstrap) then
    begin
      if MAI.MyNoOfSeqs < 4 then
        RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least four taxa are needed for bootstrapping.');
      {$IFDEF VISUAL_BUILD}
      if MegaForm.MultithreadedCalcIsRunning then
      begin
        ShowMessage('Bootstrap analyses are multi-threaded but another multi-threaded calculation is running. This bootstrap calculation cannot be launched until the other multi-threaded calculation is completed');
        ARP.Hide;
        Abort;
      end;
      {$ENDIF}
    end
    else if MAI.MyTreePack.DoesContain(ttCPTest) then
    begin
      if MAI.MyNoOfSeqs < 4 then
        RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least four taxa are needed for the CP Test.');
    end;

    ADMat := NewDistMatrix(MAI.MyNoOfSeqs, True);

    {$IFNDEF VISUAL_BUILD}
    if UsrOperation = dtdoPhyloQ then
      (MAI as TPhyloQAnalysisInfo).ComputePwDistances;
    {$ENDIF}
    with MAI.MyDistPack do
      if      DoesContain(gdOneNuc)    then
        DistComputer := TNucDist.Create
      else if DoesContain(gdSynNonsyn)
        then  DistComputer := TSynNonsynDist.Create
      else if DoesContain(gdAmino)
        then  DistComputer := TAminoDist.Create;

    with DistComputer do
    begin
      DistPack  := MAI.MyDistPack;
      NoOfSeqs  := MAI.MyMappedData.Count;
      Sequences := MAI.MyMappedData;
      QuickExit := True;
      D         := ADMat;
      NoOfSites := MAI.MyNoOfSites;
    end;
    if DistComputer is TSynNonsynDist  then
      with DistComputer as TSynNonsynDist do
        CodeTable := D_InputSeqData.CodeTable;

    ARP.UpdateRunStatusInfo('Status', 'Computing distance matrix');

    searchThread := TDistTreeSearchThread.Create(DistComputer, MAI, ADMat, UsrOperation);
    {$IFDEF VISUAL_BUILD}
    searchThread.OnTerminate := MegaForm.DistTreeSearchThreadDone;
    {$ELSE}
    searchThread.OnTerminate := D_MegaMain.DistTreeThreadDone;
    {$ENDIF}
    DistComputer := nil;
    MAI := nil;
    ARP := nil;
    ADMat := nil;

    {$IFDEF VISUAL_BUILD}
    searchThread.Start;
    searchThread := nil;
    {$ELSE}
    { for mega-cc when using the file iterator, we must wait for this thread
      to finish, otherwise the next thread (with different data) might get
      launched, depending on timing, which leads to all kinds of problems}
    searchThread.FreeOnTerminate := False;
    searchThread.Start;
    searchThread.WaitFor;
    FreeAndNil(searchThread);
    {$ENDIF}
  except
    on E: Exception do
      ShowErrorMessage(E);
  end;
  finally
    if Assigned(ATreeList) then
      ATreeList.Free;
    if ADMat <> nil then
      FreeDistMatrix(ADMat, MAI.MyNoOfSeqs);
    if Assigned(MAI) and Assigned(MAI.ARP) then
      MAI.ARP.Free;
    if Assigned(MAI) then
    begin
      MAI.MyProcessPack := nil;
      FreeAndNil(MAI);
    end;
    if Assigned(DistComputer) then
      DistComputer.Free;
  end;
end;

procedure ProcessFourClusterAnalysisCommand(AnalysisType: AnsiString);
var
  ADMat : PDistanceMatrix;
  ATreeList: TTreeList;
  ARP : TRuntimeProgress;
  MAI : TAnalysisInfo;
begin
  // initialize distance matrix holders etc.
  ADMat     := nil;
  ATreeList := nil;
  ARP := nil;
  MAI := nil;
  try try
    MAI := TAnalysisInfo.Create;

    // progress system setup
    ARP := TRuntimeProgress.Create(Application.MainForm);
    {$IFDEF VISUAL_BUILD}
    ARP.DataFileName  :=  MegaForm.DataFileName;
    ARP.DataTitle     :=  MegaForm.DataTitle;
    MAI.DataFilename := MegaForm.DataFilename;
    {$ELSE}
	  ARP.DataFileName  :=  D_MegaMain.DataFileName;
    ARP.DataTitle     :=  D_MegaMain.DataTitle;
    MAI.DataFilename  := D_MegaMain.DataFileName;
	  {$ENDIF}

    MAI.ARP := ARP;
    ARP.FMAI := MAI;

//    if not MAI.GetClusters(4) then
//      Exit;

    ShowMessage('Not implemented');

{
    //=== get options
    if not MAI.GetTreeOptions(dtdoClusterAnalysis, 'Bootstrap') then
      Exit;

    // now we need to subset the data
    ARP.AddRunStatusInfo('Status', 'Preparing data');
    ARP.Show;
    MAI.MyMappedData := TList.Create;
    SeqDataViewer.PrepareDataForDistAnalysis(MAI.MySubsetOpt, MAI.MyMappedData, MAI.MyUsedOtuInfos, MAI.MyNoOfSeqs, MAI.MyNoOfSites);
    if MAI.MyNoOfSites < 1 then
      RaiseErrorMessage(HC_No_Common_Sites, 'No common sites found for computing distances');
    ARP.AddAnalysisOptions('No. of Sites', IntToStr(MAI.MyNoOfSites));

    //Allocate space for a tree
    ATreeList := TTreeList.Create; // will be used later

    //Allocate Distance matrix
    ADMat := NewDistMatrix(MAI.MyNoOfSeqs, True);

    //Allocate DistComputer
    with MAI.MyDistPack do
      if      DoesContain(gdOneNuc)    then  DistComputer := TNucDist.Create
      else if DoesContain(gdSynNonsyn) then  DistComputer := TSynNonsynDist.Create
      else if DoesContain(gdAmino)     then  DistComputer := TAminoDist.Create;

    //Initialize DistComputer
    with DistComputer do
    begin
      DistPack  := MAI.MyDistPack;
      NoOfSeqs  := MAI.MyMappedData.Count;
      Sequences := MAI.MyMappedData;
      QuickExit := True;
      D         := ADMat;
      NoOfSites := MAI.MyNoOfSites;
    end;

    if DistComputer is TSynNonsynDist  then
      with DistComputer as TSynNonsynDist do
         CodeTable := SeqDataViewer.CodeTable;

    //Compute Distance matrix
    if MAI.MyTreePack.DoesContain(ttBootstrap) then
      ARP.UpdateRunStatusInfo('Status', 'Checking data for bootstrapping')
    else
      ARP.UpdateRunStatusInfo('Status', 'Computing distances');
    DistComputer.SetProgressBar(ARP.PercentGauge);
    DistComputer.SetStopButton(ARP.StopBtn);

    // now compute distances
    if      DistComputer is TNucDist       then  TNucDist(DistComputer).ComputeDistances
    else if DistComputer is TSynNonsynDist then  TSynNonsynDist(DistComputer).ComputeDistances
    else if DistComputer is TAminoDist     then  TAminoDist(DistComputer).ComputeDistances;

    // important for Koichiro based routines.
    for i:=0 to MAI.MyNoOfSeqs-1 do
      ADMat[i,i] := 0;
    for i:=0 to MAI.MyNoOfSeqs-1 do
      for j:=0 to i-1 do
        ADMat[j,i] := ADMat[i,j];

    // save this matrix for future display
    MAI.MyShowD := NewDistMatrix(MAI.MyNoOfSeqs, True);
    CopyDistMatrix(MAI.MyNoOfSeqs, MAI.MyShowD, ADMat, True);

    if not MAI.MyTreePack.DoesContain(ttBootstrap) then
      ARP.UpdateRunStatusInfo('Status', 'Computing tree');

    if UsrOperation = dtdoDispTreeWithOLSBLen then
    begin
      // ATreeList will be given to someone to compute branch lengths
    end
    else if MAI.MyTreePack.DoesContain(ttBootstrap) then
    begin
      DistComputer.SetProgressBar(nil);
      DistComputer.SetStopButton(nil);

      // allocate needed matrices
      MAI.MyBootD := NewDistMatrix(MAI.MyNoOfSeqs, True);
      CopyDistMatrix(MAI.MyNoOfSeqs, MAI.MyBootD, ADMat, True);
      // this is important to make the initial tree
      MAI.MyOriD := ADMat;
      ADMat := nil;

      //==
      ARP.UpdateRunStatusInfo('Status', 'Conducting Bootstrap Test');
      SetLength(MAI.MySiteFreqs, MAI.MyNoOfSites);
      DistComputer.FreqTable    := MAI.MySiteFreqs;
      DistComputer.FreqTableLen := MAI.MyNoOfSites;
      MAI.MyOriTreeList  := ATreeList;
      ATreeList := nil;

      if MAI.MyTreePack.DoesContain(ttUPGMA) then
         MAI.MyBootPartitionList := TPartitionList.Create(MAI.MyNoOfSeqs, 0, true) // keeps 0 bootstrap trees
      else
         MAI.MyBootPartitionList := TPartitionList.Create(MAI.MyNoOfSeqs, 0, false); // keeps 0 bootstrap trees

      DistComputer.D    := MAI.MyBootD;
      MAI.DistComputer  := DistComputer;
      MAI.MyNoOfReps    := MAI.MyTreePack.BootReps;

      ComputeBootstrapTree(MAI);

      ARP := nil;
      MAI := nil;
    end
    else if MAI.MyTreePack.DoesContain(ttCPTest) then
    begin
      DistComputer.SetProgressBar(nil);
      DistComputer.SetStopButton(nil);

      // allocate needed matrices
      MAI.MyBootD := NewDistMatrix(MAI.MyNoOfSeqs, True);
      CopyDistMatrix(MAI.MyNoOfSeqs, MAI.MyBootD, ADMat, True);
      // this is important to make the initial tree
      MAI.MyOriD := ADMat;
      ADMat := nil;

      //==
      ARP.UpdateRunStatusInfo('Status', 'Conducting Interior-Branch Test');
      SetLength(MAI.MySiteFreqs, MAI.MyNoOfSites);
      DistComputer.FreqTable    := MAI.MySiteFreqs;
      DistComputer.FreqTableLen := MAI.MyNoOfSites;
      MAI.MyOriTreeList := ATreeList;
      ATreeList := nil;

      DistComputer.D    := MAI.MyBootD;
      MAI.DistComputer  := DistComputer;
      MAI.MyNoOfReps    := MAI.MyTreePack.BootReps;

      //== issue computation command
      ConductCPTest(MAI);

      ARP := nil;
      MAI := nil;
    end
    else if not (MAI.MyTreePack.DoesContain(ttBootstrap)) then// and MAI.MyTreePack.DoesContain(ttME)) then
    begin
      MAI.MyOriD := ADMat;
      ADMat := nil;
      MAI.MyOriTreeList := ATreeList;
      ATreeList := nil;
      ComputeTree(MAI, True);
      ARP := nil;
      MAI := nil;
    end;
}
  except
    on E: Exception do ShowErrorMessage(E);
  end;
  finally
    if ATreeList <> nil then ATreeList.Free;
    if ADMat <> nil then  FreeDistMatrix(ADMat, MAI.MyNoOfSeqs);
    if ARP <> nil then ARP.Free;
    if MAI <> nil then
    begin
      MAI.MyProcessPack := nil;
      MAI.Free;
    end;
  end;

end;

procedure ProcessSeqRelTimeCCCommand(UsrOperation: TDistTreeDlgOption;ProcessPack: TProcessPack);
var
  Progress : TRuntimeProgress;
  AnalysisInfo : TAnalysisInfo;
  //ATree: TTreeData;
  Calibrations: TCalibrations;
  CalibrationFile: String;
  //MinTimes, MaxTimes: TDivTimesArray;
  //SamplingTimes: TDivTimesArray;
  HasSampleTimes: Boolean = False;
  HasDivTimes: Boolean = False;
begin
  Progress := nil;
  AnalysisInfo := nil;
  //ATree := nil;
  //SetLength(SamplingTimes, 0);
  //SetLength(MinTimes, 0);
  //SetLength(MaxTimes, 0);
  CalibrationFile := EmptyStr;
  Calibrations := nil;

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
    AnalysisInfo.MyMappedData := TList.Create;
    AnalysisInfo.MyIncludedSites := D_InputSeqData.PrepareDataForDistAnalysis(AnalysisInfo.MySubsetOpt,
                                              AnalysisInfo.MyMappedData,
                                              AnalysisInfo.MyUsedOtuInfos,
                                              AnalysisInfo.MyNoOfSeqs,
                                              AnalysisInfo.MyNoOfSites,
                                              AnalysisInfo.MyLabelsUsed,
                                              AnalysisInfo.RecodeScheme,
                                              AnalysisInfo.SiteCoverage);
    CheckAbortForIdenticalSeqsLS(AnalysisInfo);
    AnalysisInfo.ARP.AddAnalysisOptions('No. of Seqs', Format('%.0n', [AnalysisInfo.MyNoOfSeqs*1.0]));
    AnalysisInfo.ARP.AddAnalysisOptions('No. of Sites', Format('%.0n', [AnalysisInfo.MyNoOfSites*1.0]));
    if not AnalysisInfo.OutgroupDefined then { force the user to define an outgroup}
      raise Exception.Create('The timetree analysis requires that an outgroup be defined');
    if AnalysisInfo.MyNoOfSites < 1 then
      RaiseErrorMessage(HC_No_Common_Sites, 'No common sites found. ' + LineEnding + NoCommonSitesStr);

    if AnalysisInfo.MyNoOfSeqs < 3 then
      RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least three sequences are needed for reltime analysis');

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

    AnalysisInfo.MyOriTreeList := TTreeList.Create;
    AnalysisInfo.MyOriTreeList.ImportFromNewick(AnalysisInfo.MyUserNewickTree, AnalysisInfo.MyOtuNames);
    AnalysisInfo.GroupInfo.SetTreeOutgroupFromOtuInfos;
    {$IFNDEF VISUAL_BUILD}
    if not AnalysisInfo.IngroupDefined then
      error_nv('An ingroup must be defined for the timetree analysis');
    {$ENDIF}
    RootOnOutgroup(AnalysisInfo);
    AnalysisInfo.ClockTypeSet := True;

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
      if Calibrations.Count > 0 then
      begin
        AnalysisInfo.MyOriTreeList.ResolveCalibrationTimeNames(Calibrations);
        AnalysisInfo.MyOriTreeList[0].resolveRelTimes(Calibrations);
      end;
      AnalysisInfo.CalibrationTimes := Calibrations;
      Calibrations.GetCalibrationTypes(HasDivTimes, HasSampleTimes);
      DoReltimeLeastSquares(AnalysisInfo);
    end
    else
    begin
      DoReltimeLeastSquares(AnalysisInfo)
    end;

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
  end;
end;

procedure ProcessDistRelTimeCCCommand(UsrOperation: TDistTreeDlgOption;ProcessPack: TProcessPack);
begin
  raise Exception.Create('Not yet implemented');
end;

procedure ProcessDistDistPhylogenyCommand(UsrOperation: TDistTreeDlgOption; ProcessPack : TProcessPack);
var
  i, j: Integer;
  ADMat : PDistanceMatrix;
  ATreeList: TTreeList;
  ARP : TRuntimeProgress;
  MAI : TAnalysisInfo;
  Calibrations: TCalibrations;
  CalibrationFile: String;
begin
  // initialize distance matrix holders etc.
  ADMat     := nil;
  ATreeList := nil;
  ARP := nil;
  MAI := nil;
  Calibrations := nil;
  CalibrationFile := EmptyStr;

  try try
    MAI := TAnalysisInfo.Create;
    MAI.DataFilename := GetInputDataFileName;
    MAI.MyProcessPack := ProcessPack;
    MAI.PropagateConstraints := (not ProcessPack.HasProcessType(ppSkipMinMax));
    MAI.InitialUsrOperation := UsrOperation;
    // progress system setup
    ARP := TRuntimeProgress.Create(Application.MainForm);
    ARP.DataFileName := GetInputDataFileName;
    ARP.DataTitle := GetInputDataTitle;

    MAI.ARP := ARP;
    ARP.FMAI := MAI;
    //=== get options
    if not MAI.GetAnalysisOptions(UsrOperation) then
    begin
      if Assigned(MAI.MyProcessPack) and MAI.MyProcessPack.IsGettingWorkflowSettings then
        MAI := nil;
      Exit;
    end;
    ARP.WriteAnalysisOptionsToStdOut;
    // now we need to subset the data
    ARP.AddRunStatusInfo('Status', 'Preparing data');
    ARP.Show;

     //Allocate space for a tree
    ATreeList := TTreeList.Create;

    if MAI.MyTreePack.DoesContain(ttUserTree) then    // Otherwise HyPhy runs NJ automaticlly by default
    begin
      if MAI.GetUserTreeInput(UsrOperation, MAI.MyUserTreeFile, MAI.ARP, nil, ttNJ, (UsrOperation = dtdoOLSComputeUserTreeBLens) or (UsrOperation = dtdoPhyloQ)) then
      begin
         if MAI.MyTreePack.DoesContain(ttUserTree) then  // user did not change his/her mind
         begin
           ATreeList.ImportFromNewick(MAI.MyUserNewickTree, MAI.MyOtuNames);  // bugfix
           if MAI.MyProcessPack.CalibrationFile <> EmptyStr then  // for MEGA-CC
             CalibrationFile := MAI.MyProcessPack.CalibrationFile
           else if MAI.CalibrationFile <> EmptyStr then // for MEGA-Visual
             CalibrationFile := MAI.CalibrationFile;
           if CalibrationFile <> EmptyStr then
           begin
             Calibrations := TCalibrations.Create;
             Calibrations.SetInfo(MAI);
             if not Calibrations.LoadFromFile(CalibrationFile) then
               raise Exception.Create('Failed to load calibrations');
             {$IFNDEF VISUAL_BUILD}
             Calibrations.FilterMissingTaxa(MAI.MyOtuNames);
             {$ENDIF}
             MAI.MyOriTreeList.ResolveCalibrationTimeNames(Calibrations);
             MAI.CalibrationTimes := Calibrations;
           end;
         end;
      end
      else
      begin
        ShowMessage('User cancelled the calculation');
        Exit;
      end;
    end;

    //Allocate Distance matrix
    ADMat := NewDistMatrix(MAI.MyNoOfSeqs, True);
    //== get the data
    D_InputDistData.PrepareDataForAnalysis(ADMat, MAI.MyUsedOtuInfos, MAI.MyNoOfSeqs); //

    // check if all entries are valid
    for i:=0 to MAI.MyNoOfSeqs-1 do
      for j:= 0 to i-1 do
        if ADMat[i,j] < 0 then
          RaiseErrorMessage(HC_Missing_data_or_invalid_distances_in_the_matrix, 'Missing data or invalid distances in the marix');
    ARP.AddAnalysisOptions('No. of Taxa', IntToStr(MAI.MyNoOfSeqs));

    // important for Koichiro based routines.
    for i:=0 to MAI.MyNoOfSeqs-1 do
      ADMat[i,i] := 0;
    for i:=0 to MAI.MyNoOfSeqs-1 do
      for j:=0 to i-1 do
        ADMat[j,i] := ADMat[i,j];

    // save this matrix for future display
    MAI.MyShowD := NewDistMatrix(MAI.MyNoOfSeqs, True);
    CopyDistMatrix(MAI.MyNoOfSeqs, MAI.MyShowD, ADMat, True);

    ARP.UpdateRunStatusInfo('Status', 'Computing tree');
    //Compute tree
    MAI.MyOriD := ADMat;
    ADMat := nil;
    MAI.MyOriTreeList := ATreeList;
    ATreeList := nil;
    if (MAI.InitialUsrOperation = dtdoRelTimeLS) or (MAI.InitialUsrOperation = dtdoRtdtLS) then
      ComputeRelTimeLSTree(MAI, True)
    else
      ComputeTree(MAI, True);
    ARP := nil;
    MAI := nil;
  except
    on E: Exception do ShowErrorMessage(E);
  end;
  finally
    if ATreeList <> nil then
      ATreeList.Free;
    if ADMat <> nil then
      FreeDistMatrix(ADMat, MAI.MyNoOfSeqs);
    if ARP <> nil then
      ARP.Free;
    if MAI <> nil then
    begin
      MAI.MyProcessPack := nil;
      MAI.Free;
    end;
  end;
end;

procedure ComputeRelTimeLSTree(MAI: TAnalysisInfo; IsUpdateARP: Boolean);
var
  RelTimeThread: TRelTimeLSThread;
  Terminator: TRelTimeTerminator;
  i: Integer;
begin
  RelTimeThread := nil;
  Terminator := nil;

  if not Assigned(MAI.MyOtuNames) then
  begin
    MAI.MyOtuNames := TStringList.Create;
    for i := 0 to MAI.MyOriTreeList.NoOfOTUs - 1 do
      MAI.MyOtuNames.Add(MAI.MyOriTreeList.OTUName[i]);
  end;

  RelTimeThread := TRelTimeLSThread.Create(True);
  Terminator := TRelTimeTerminator.Create(dtdoRelTimeLS);
  RelTimeThread.OnTerminate := Terminator.OnLeastSquaresThreadDone;
  RelTimeThread.AnalysisInfo := MAI;
  if not MAI.MyOriTreeList[0].isBLen then { for the case where the user gave a tree with no branch lengths, otherwise things fail upstream}
    MAI.MyOriTreeList.isBLen := True;
  RelTimeThread.ProgressDlg := MAI.ARP;
  RelTimeThread.Start;
end;

procedure ComputeRelTimeBLens(MAI: TAnalysisInfo);
var
  RelTimeThread: TRelTimeBLenThread;
  Terminator: TRelTimeTerminator;
  i: Integer;
begin
  RelTimeThread := nil;
  Terminator := nil;

  if not Assigned(MAI.MyOtuNames) then
  begin
    MAI.MyOtuNames := TStringList.Create;
    for i := 0 to MAI.MyOriTreeList.NoOfOTUs - 1 do
      MAI.MyOtuNames.Add(MAI.MyOriTreeList.OTUName[i]);
  end;
  RelTimeThread := TRelTimeBLenThread.Create(True);
  Terminator := TRelTimeTerminator.Create(dtdoRelTimeBLens);
  RelTimeThread.OnTerminate := Terminator.OnBLensThreadDone;
  RelTimeThread.AnalysisInfo := MAI;
  RelTimeThread.ProgressDlg := MAI.ARP;
  RelTimeThread.Start;
end;

procedure ComputeRelTimeBLens(MAI: TAnalysisInfo; Calibrations: TCalibrations);
var
  RelTimeThread: TRelTimeBLenThread = nil;
  Terminator: TRelTimeTerminator = nil;
  i: Integer = -1;
  ErrorMsg: String = '';
begin
  MAI.CalibrationTimes.IsBLensOnly := False;

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
  if not Assigned(MAI.MyOtuNames) then
  begin
    MAI.MyOtuNames := TStringList.Create;
    for i := 0 to MAI.MyOriTreeList.NoOfOTUs - 1 do
      MAI.MyOtuNames.Add(MAI.MyOriTreeList.OTUName[i]);
  end;

  RelTimeThread := TRelTimeBLenThread.Create(True);
  Terminator := TRelTimeTerminator.Create(dtdoRelTimeBLens);
  RelTimeThread.OnTerminate := Terminator.OnBLensThreadDone;
  RelTimeThread.AnalysisInfo := MAI;
  RelTimeThread.ProgressDlg := MAI.ARP;
  RelTimeThread.Start;
end;

procedure DoReltimeLeastSquares(MAI: TAnalysisInfo);
begin
  try
    try
      ComputeRelTimeLSTree(MAI, True);
      MAI := nil;
  except
    on E: Exception do
      ShowErrorMessage(E);
  end;
  finally
    if Assigned(MAI) then
    begin
      if Assigned(MAI.ARP) then
        MAI.ARP.Free;
      MAI.MyProcessPack := nil;
      MAI.Free;
    end;
  end;
end;

procedure ComputeTree(MAI: TAnalysisInfo; IsUpdateARP: Boolean);
begin
  if MAI.MyTreePack.DoesContain(ttME) then
    ConstructMETree(MAI, IsUpdateARP)
  else if MAI.MyTreePack.DoesContain(ttNJ) then
    ConstructNJTree(MAI, IsUpdateARP)
  else if MAI.MyTreePack.DoesContain(ttUPGMA) then
    ConstructUPGMATree(MAI,IsUpdateARP)
  else if MAI.MyTreePack.DoesContain(ttBlens) then
    if IsPhyloQAnalysis then             // toggle analysis methods for testing: KT110819
    begin
      SearchLastOTUPositionInUserTree(MAI);
    end
    else
      AnalyzeOLSUserTree(MAI);
end;

procedure ComputeSiteCoverage(ProcessPack: TProcessPack);
var
  MAI: TAnalysisInfo = nil;
  ARP: TRuntimeProgress = nil;
  t: TSiteCoverageThread = nil;
begin
  MAI := TAnalysisInfo.Create;
  MAI.DataFilename := GetInputDataFileName;
  MAI.MyProcessPack := ProcessPack;
  MAI.InitialUsrOperation := dtdoSiteCoverage;

  ARP := TRuntimeProgress.Create(Application.MainForm);
  ARP.DataFileName := GetInputDataFileName;
  ARP.DataTitle := GetInputDataTitle;
  MAI.ARP := ARP;
  ARP.FMAI := MAI;

  if not MAI.GetAnalysisOptions(dtdoSiteCoverage) then
  begin
    if MAI.MyProcessPack.IsGettingWorkflowSettings then
      MAI := nil;
    Exit;
  end;

  ARP.WriteAnalysisOptionsToStdOut;
  MAI.ARP.AddRunStatusInfo('Status', 'Preparing data');
  MAI.ARP.Refresh;
  Application.ProcessMessages;
  MAI.MyMappedData := TList.Create;
  if not MAI.GetUserTreeInput(dtdoSiteCoverage,
                              MAI.MyUserTreeFile,
                              MAI.ARP,
                              nil,
                              ttNJ,
                              True) then
    Exit;

  try
    D_InputSeqData.ARP := MAI.ARP;
    MAI.MyIncludedSites := D_InputSeqData.PrepareDataForDistAnalysis(MAI.MySubsetOpt, MAI.MyMappedData, MAI.MyUsedOtuInfos, MAI.MyNoOfSeqs, MAI.MyNOOfSites, MAI.MyLabelsUsed, MAI.RecodeScheme, MAI.SiteCoverage);
    MAI.ARP.AddAnalysisOptions('No. of Seqs', Format('%.0n', [MAI.MyNoOfSeqs*1.0]));
    MAI.ARP.AddAnalysisOptions('No. of Sites', Format('%.0n', [MAI.MyNoOfSites*1.0]));
    MAI.ARP.Show;
    MAI.MyOriTreeList := TTreeList.Create;
    MAI.MyOriTreeList.ImportFromNewick(MAI.MyUserNewickTree, MAI.MyOtuNames, False);
    t := TSiteCoverageThread.CreateFromTreeData(MAI, MAI.MyOriTreeList[0]);
    {$IFDEF VISUAL_BUILD}
    t.OnTerminate := MegaForm.SiteCoverageThreadDone;
    {$ELSE}
    t.OnTerminate := D_MegaMain.SiteCoverageThreadDone;
    t.FreeOnTerminate := True;
    {$ENDIF}
    t.Start;
  except
    on E:Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      ShowMessage(Format('Application error: %s', [E.Message]));
      {$ELSE}
      error_nv('error preparing to compute site coverage', E);
      {$ENDIF}
    end;
  end;
end;

procedure ComputeBootstrapTree(MAI: TAnalysisInfo; DoFullProgress: Boolean=False);
begin
  try
    if MAI.MyTreePack.DoesContain(ttME) then
      BootstrapME(MAI)
    else if MAI.MyTreePack.DoesContain(ttNJ) then
      BootstrapNJ(MAI, DoFullProgress)
    else if MAI.MyTreePack.DoesContain(ttUPGMA) then
      BootstrapUPGMA(MAI, DoFullProgress);
  except
    on E:Exception do
    begin
      {$IFDEF VISUAL_BUILD}
       ShowMessage('APPLICATION ERROR: ' + E.Message);
      {$ELSE}
       error_nv('failed to launch bootstrap threads', E);
      {$ENDIF}
    end;
  end;
end;

//===  Modified by KT on 001205
procedure ConductCPTest(MAI: TAnalysisInfo);
var ACPTestThread: TBootstrapUserTreeDistThread;
    ACPTestTerminator: TTreeThreadTerminator;
    ATree: TTreeData;
begin
  ACPTestThread := nil;
  ACPTestTerminator := nil;
  ATree := nil;
  try
    ATree := TTreeData.Create(MAI.MyNoOfSeqs, True, True, True);
    MAI.MyOriTreeList.Add(ATree);
    ACPTestTerminator := TTreeThreadTerminator.Create;

    ACPTestThread := TBootstrapUserTreeDistThread.Create(MAI.MyOriTreeList, MAI.MyBootD);
    ACPTestThread.Info := MAI;
//    ACPTestThread.BootstrapDistanceProc := BootstrapSampleD;
    ACPTestThread.NoOfBootstraps := MAI.MyNoOfReps;
    ACPTestThread.ProgressDlg := MAI.ARP;
    if MAI.MyTreePack.DoesContain(ttNJ) then
      ACPTestThread.InitialTree := itNJ
    else if MAI.MyTreePack.DoesContain(ttME) then
      ACPTestThread.InitialTree := itME
    else
      ACPTestThread.InitialTree := itUser;
    ACPTestThread.OnTerminate := ACPTestTerminator.OnThreadDone;
    MAI.ARP.Thread := ACPTestThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    ACPTestThread.Start;

    ATree := nil;
    ACPTestThread := nil;
    ACPTestTerminator := nil;

  finally// this quashes any exceptions
    if ATree <> nil then ATree.Free;
    if ACPTestThread <> nil then ACPTestThread.Free;
    if ACPTestTerminator <> nil then ACPTestTerminator.Free;
  end;
end;

//========== subfunctions
procedure ConstructNJTree(MAI: TAnalysisInfo; IsShowProgress: Boolean);
var
  ANJThread: TNJTreeSearchThread;
  ANJThreadTerminator: TTreeThreadTerminator;
  ATree: TTreeData;
begin
  ANJThread := nil;
  ANJThreadTerminator := nil;
  ATree := nil;

  try
    ATree := TTreeData.Create(MAI.MyNoOfSeqs, True, False, False);
    MAI.MyOriTreeList.Add(ATree);
    ANJThread := TNJTreeSearchThread.Create(ATree, MAI.MyOriD);
    ANJThread.Info := MAI;
    ATree := nil;

    ANJThreadTerminator := TTreeThreadTerminator.Create;
    ANJThread.ProgressDlg := MAI.ARP;
    ANJThread.ShowProgress := IsShowProgress;
    ANJThread.OnTerminate := ANJThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := ANJThread;

    if not MAI.ARP.Visible then
      MAI.ARP.Show;

    ANJThread.Start;
    ANJThread := nil;
    ANJThreadTerminator := nil;
  finally
    if ATree <> nil then ATree.Free;
    if ANJThreadTerminator <> nil then ANJThreadTerminator.Free;
    if ANJThread <> nil then ANJThread.Free;
  end;
end;

procedure ConstructUPGMATree(MAI: TAnalysisInfo; IsShowProgress: Boolean);
var
  AUPGMAThread: TUPGMATreeSearchThread = nil;
  AUPGMAThreadTerminator: TTreeThreadTerminator = nil;
  ATree: TTreeData = nil;
begin
  try
    ATree := TTreeData.Create(MAI.MyNoOfSeqs, True, False, False);
    MAI.MyOriTreeList.Add(ATree);
    AUPGMAThreadTerminator := TTreeThreadTerminator.Create;

    AUPGMAThread := TUPGMATreeSearchThread.Create(ATree, MAI.MyOriD);
    AUPGMAThread.ProgressDlg := MAI.ARP;
    AUPGMAThread.ShowProgress := IsShowProgress;
    AUPGMAThread.Info := MAI;
    AUPGMAThread.OnTerminate := AUPGMAThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AUPGMAThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AUPGMAThread.Start;
    AUPGMAThread := nil;
    AUPGMAThreadTerminator := nil;
  finally
    if AUPGMAThreadTerminator <> nil then AUPGMAThreadTerminator.Free;
    if AUPGMAThread <> nil then AUPGMAThread.Free;
  end;
end;

procedure AnalyzeOLSUserTree(MAI: TAnalysisInfo);
var
  AOLSThread: TOLSBLenThread;
  AOLSThreadTerminator: TTreeThreadTerminator;
  ATree: TTreeData;
begin
  AOLSThread := nil;
  AOLSThreadTerminator := nil;
  ATree := nil;
  try
    ATree := TTreeData.Create(MAI.MyNoOfSeqs, True, False, False);
    MAI.MyOriTreeList.isBLen := True;
    MAI.MyOriTreeList.Add(ATree); // this is our initial tree
    ATree := nil;         // relinquish control
    AOLSThreadTerminator := TTreeThreadTerminator.Create;

    AOLSThread := TOLSBLenThread.Create(MAI.MyOriTreeList, MAI.MyOriD);
    AOLSThread.UseInitialNJTree := false;
    AOLSThread.MaxNoOfTrees := 1; // MAI.MyTreePack.MaxTrees
    AOLSThread.Info := MAI;

    AOLSThread.ProgressDlg := MAI.ARP;
    AOLSThread.ShowProgress := True; // false
    AOLSThread.OnTerminate := AOLSThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AOLSThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    AOLSThread.Start;

    AOLSThread := nil;
    AOLSThreadTerminator := nil;
  finally
    if ATree <> nil then ATree.Free;
    if AOLSThreadTerminator <> nil then AOLSThreadTerminator.Free;
    if AOLSThread <> nil then AOLSThread.Free;
  end;
end;

procedure SearchLastOTUPositionInUserTree(MAI: TAnalysisInfo);
var
  AOLSThread: TOLSSearchLastOTUPositionThread;
  AOLSThreadTerminator: TTreeThreadTerminator;
  ATree: TTreeData;
begin
  AOLSThread := nil;
  AOLSThreadTerminator := nil;
  ATree := nil;
  try
    ATree := TTreeData.Create(MAI.MyNoOfSeqs, True, False, False); //
    MAI.MyOriTreeList.Add(ATree); // this is our intial tree
    ATree := nil;         // relinquish control
    AOLSThreadTerminator := TTreeThreadTerminator.Create;

    AOLSThread := TOLSSearchLastOTUPositionThread.Create(MAI.MyOriTreeList, MAI.MyOriD);

    AOLSThread.ProgressDlg := MAI.ARP;
    MAI.ARP.FMAI := MAI;      // todo - fix this, we shouldn't need to have a circular reference like this!
    AOLSThread.ShowProgress := True; // false
    AOLSThread.OnTerminate := AOLSThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AOLSThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;

    AOLSThread.Start;
    AOLSThread := nil;
    AOLSThreadTerminator := nil;
  finally
    if ATree <> nil then ATree.Free;
    if AOLSThreadTerminator <> nil then AOLSThreadTerminator.Free;
    if AOLSThread <> nil then AOLSThread.Free;
  end;
end;

procedure ConstructMETree(MAI: TAnalysisInfo; IsShowProgress: Boolean);
var
  AMEThread: TMeTreeSearchThread;
  AMEThreadTerminator: TTreeThreadTerminator;
  ATree: TTreeData;
begin
  AMEThread := nil;
  AMEThreadTerminator := nil;
  ATree := nil;
  try
  
    ATree := TTreeData.Create(MAI.MyNoOfSeqs, True, False, False); //
    MAI.MyOriTreeList.Add(ATree); // this is our intial tree
    ATree := nil;         // relinquish control
    AMEThreadTerminator := TTreeThreadTerminator.Create;

    AMEThread := TMeTreeSearchThread.Create(MAI.MyOriTreeList, MAI.MyOriD);
    AMEThread.UseInitialNJTree := true;
    AMEThread.MaxNoOfTrees := MAI.MyTreePack.MaxTrees;
    AMEThread.Info := MAI;

//    AMEThread.Threshold := ???; // add it to the Compute Options Dlg in 2.2

    AMEThread.ProgressDlg := MAI.ARP;
    AMEThread.ShowProgress := IsShowProgress;
    AMEThread.OnTerminate := AMEThreadTerminator.OnThreadDone;
    MAI.ARP.Thread := AMEThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;

    AMEThread.Start;

    AMEThread := nil;
    AMEThreadTerminator := nil;
  finally
    if ATree <> nil then ATree.Free;
    if AMEThreadTerminator <> nil then AMEThreadTerminator.Free;
    if AMEThread <> nil then AMEThread.Free;
  end;
end;

function BootstrapSampleD(ARP: TRuntimeProgress): Boolean; // it fills the BootD
var
  MAI : TAnalysisInfo;
  i, j: Integer;
begin
  // MAI is overwritten here if second analysis was run during Bootstrap
  MAI := TAnalysisInfo(ARP.FMAI);
  Resample(MAI.MySiteFreqs, MAI.MyNoOfSites);
  // now compute distances
  try
    // Need to check for CML so we can call SE method instead why is SE so separate? - Joel
    if MAI.DistComputer is TNucDist then
    begin
      if MAI.MyDistPack.DoesContain(gdMCL) then
        TNucDist(MAI.DistComputer).ComputeDistancesSE
      else
        TNucDist(MAI.DistComputer).ComputeDistances;
    end
    else if MAI.DistComputer is TSynNonsynDist then
      TSynNonsynDist(MAI.DistComputer).ComputeDistances
    else if MAI.DistComputer is TAminoDist then
      TAminoDist(MAI.DistComputer).ComputeDistances;

    // important for Koichiro based routines.
    for i:=0 to MAI.MyNoOfSeqs-1 do
      MAI.MyBootD[i,i] := 0;
    for i:=0 to MAI.MyNoOfSeqs-1 do
      for j:=0 to i-1 do
        MAI.MyBootD[j,i] := MAI.MyBootD[i,j];
    Result := True;
  except
    on E:Exception do
    begin
      MAI.MyBootPartitionList.ErrorMessages.Add(E.Message);
      Result := False;
    end;
  end;
end;

procedure BootstrapNJ(MAI: TAnalysisInfo; DoFullProgress: Boolean);
var
  ABootstrapNJThread: TBootstrapNJThread = nil;
  ABootstrapNJTerminator: TTreeThreadTerminator = nil;
  ATree: TTreeData = nil;
begin
  try
    ATree := TTreeData.Create(MAI.MyNoOfSeqs, True, False, False);
    MAI.MyOriTreeList.Add(ATree);
    ABootstrapNJTerminator := TTreeThreadTerminator.Create;

    ABootstrapNJThread := TBootstrapNJThread.Create(ATree, MAI.MyBootD);
    ABootstrapNJThread.NoOfThreads := MAI.MyNumThreadsToUse;
    ABootstrapNJThread.BootstrapTrees := MAI.MyBootPartitionList;
//    ABootstrapNJThread.BootstrapDistanceProc := BootstrapSampleD;
    ABootstrapNJThread.NoOfBootstraps := MAI.MyNoOfReps;
    ABootstrapNJThread.ProgressDlg := MAI.ARP;
    ABootstrapNJThread.Info := MAI;
    ABootstrapNJThread.OnTerminate := ABootstrapNJTerminator.OnBootstrapThreadDone;
    MAI.ARP.Thread := ABootstrapNJThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    ABootstrapNJThread.DoFullProgress := DoFullProgress;
    ABootstrapNJThread.Start;
    ATree := nil;
    ABootstrapNJThread := nil;
    ABootstrapNJTerminator := nil;
  finally
    if ATree <> nil then
      ATree.Free;
    if ABootstrapNJThread <> nil then
    begin
      ABootstrapNJThread.OnTerminate := nil;
      ABootstrapNJThread.Free;
    end;
    if ABootstrapNJTerminator <> nil then
      ABootstrapNJTerminator.Free;
  end;
end;

procedure BootstrapUPGMA(MAI: TAnalysisInfo; DoFullProgress: Boolean);
var
  ABootstrapUPGMAThread: TBootstrapUPGMAThread = nil;
  ABootstrapUPGMATerminator: TTreeThreadTerminator = nil;
  ATree: TTreeData = nil;
begin
  try
    ATree := TTreeData.Create(MAI.MyNoOfSeqs, True, False, False);
    MAI.MyOriTreeList.Add(ATree);
    ABootstrapUPGMATerminator := TTreeThreadTerminator.Create;
    ABootstrapUPGMAThread                       := TBootstrapUPGMAThread.Create(ATree, MAI.MyBootD);
    ABootstrapUPGMAThread.NoOfThreads := MAI.MyNumThreadsToUse;
    ABootstrapUPGMAThread.DoFullProgress        := DoFullProgress;
    ABootstrapUPGMAThread.BootstrapTrees        := MAI.MyBootPartitionList;
    ABootstrapUPGMAThread.Info := MAI;
//    ABootstrapUPGMAThread.BootstrapDistanceProc := BootstrapSampleD;
    ABootstrapUPGMAThread.NoOfBootstraps        := MAI.MyNoOfReps;
    ABootstrapUPGMAThread.ProgressDlg           := MAI.ARP;
    ABootstrapUPGMAThread.OnTerminate           := ABootstrapUPGMATerminator.OnBootstrapThreadDone;
    MAI.ARP.Thread                              := ABootstrapUPGMAThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    ABootstrapUPGMAThread.Start;
    ATree := nil;
    ABootstrapUPGMAThread := nil;
    ABootstrapUPGMATerminator := nil;
  finally
    if ATree <> nil then ATree.Free;
    if ABootstrapUPGMAThread <> nil then
    begin
      ABootstrapUPGMAThread.OnTerminate := nil;
      ABootstrapUPGMAThread.Free;
    end;
    if ABootstrapUPGMATerminator <> nil then ABootstrapUPGMATerminator.Free;
  end;
end;

procedure BootstrapME(MAI: TAnalysisInfo);
var
  ABootstrapMEThread: TBootstrapMEThread = nil;
  ABootstrapMETerminator: TTreeThreadTerminator = nil;
  ATree: TTreeData = nil;
begin
  try
    ATree := TTreeData.Create(MAI.MyNoOfSeqs, true, false, false);
    MAI.MyOriTreeList.Add(ATree);
    ABootstrapMETerminator := TTreeThreadTerminator.Create;
    ABootstrapMEThread := TBootstrapMEThread.Create(MAI.MyOriTreeList, MAI.MyBootD);


    ABootstrapMEThread.NoOfThreads := MAI.MyNumThreadsToUse;
//    ABootstrapMEThread.MaxNoOfTrees := MAI.MyTreePack.MaxTrees;
    ABootstrapMEThread.BootstrapTrees := MAI.MyBootPartitionList;
//    ABootstrapMEThread.BootstrapDistanceProc := BootstrapSampleD;
    ABootstrapMEThread.Info := MAI;
    ABootstrapMEThread.NoOfBootstraps := MAI.MyNoOfReps;
    ABootstrapMEThread.ProgressDlg := MAI.ARP;
    ABootstrapMEThread.OnTerminate := ABootstrapMETerminator.OnBootstrapThreadDone;
    MAI.ARP.Thread := ABootstrapMEThread;
    if not MAI.ARP.Visible then MAI.ARP.Show;
    ABootstrapMEThread.Start;
    ABootstrapMEThread := nil;
    ABootstrapMETerminator := nil;
    ATree := nil;
  finally
    if ATree <> nil then ATree.Free;
    if ABootstrapMEThread <> nil then
    begin
      ABootstrapMEThread.OnTerminate := nil;
      ABootstrapMEThread.Free;
    end;
    if ABootstrapMETerminator <> nil then ABootstrapMETerminator.Free;
  end;
end;

{
var
  i, j, NReps, NBranches: Integer;
  ATreeData: TTreeData;
  AOlsInfo: TComputeOlsInfo;
  OriBL, BootBL, BLx, BLxx: array of Double;
begin
  MAI.ARP.Hide;
  MAI.ARP.UpdateRunStatusInfo('Status', 'Conducting Test');
  AOlsInfo := nil;
  BootBL := nil;
  OriBL := nil;

  try try
    NReps     := MAI.MyNoOfReps;
    NBranches := 2*MAI.MyNoOfSeqs-2;

    SetLength(OriBL, NBranches);
    SetLength(BootBL,NBranches);
    SetLength(BLx, NBranches);
    SetLength(BLxx,NBranches);

    AOlsInfo := TComputeOlsInfo.Create;
    ATreeData := MAI.MyOriTreeList[0]; // just get a pointer
    ATreeData.IsSE    := True; // to ensure memory for Standard errors
    ATreeData.IsStats := True; // to ensure memory for Standard errors
    AOlsInfo.NoOfTaxa := MAI.MyNoOfSeqs;
    AOlsInfo.InitTree(ATreeData);  //

    // set the bootstrap standard errors
    for j:=0 to NBranches-1 do
    begin
      BLx[j]   := 0;
      BLxx[j]  := 0;
    end;

    MAI.ARP.Show;
    MAI.ARP.UpdatePercentProgress(0);
    // set the number of reps
    for i:=0 to NReps-1 do
    begin
      BootstrapSampleD(MAI.ARP); // it fills MyBootD with new matrix
      AOlsInfo.CopyDistMatrix(MAI.MyBootD);
      AOlsInfo.GetBLens(BootBL);
      for j:=0 to NBranches-1 do
      begin
        BLx[j]   := BLx[j]  + abs(BootBL[j])/NReps;
        BLxx[j]  := BLxx[j] + abs(BootBL[j])*abs(BootBL[j])/NReps;
      end;
      MAI.ARP.UpdatePercentProgress(Floor(i*100/NReps));
    end;

    for j:=0 to NBranches-1 do
    begin
      if (ATreeData.BLen[j] <= 0) or ((BLxx[j]- BLx[j]*BLx[j]) < 0) then
      begin
        ATreeData.SE[j] := -1; //Sqrt(NReps*(BLxx[j]- BLx[j]*BLx[j])/(NReps-1))
        ATreeData.Stats[j] := 0;//tTest(BLen
      end
      else
      begin
        if BLxx[j]- BLx[j]*BLx[j] = 0 then
        begin
          ATreeData.SE[j] := 0;
          ATreeData.Stats[j] := 99;
        end
        else
        begin
          ATreeData.SE[j] := Sqrt(NReps*(BLxx[j]- BLx[j]*BLx[j])/(NReps-1));
          ATreeData.Stats[j] := 99 - 100*2*tTest(ATreeData.BLen[j]/ATreeData.SE[j]);
          if ATreeData.Stats[j] < 0 then
            ATreeData.Stats[j] :=0;
        end;
      end;
    end;
  except
    On E: Exception do
      raise;
  end;
  finally
    AOlsInfo.Free;
    OriBL := nil;
    BootBL := nil;
    MAI.ARP.Hide;
  end;
end;
}

procedure ProcessPhyloQSeqCommand(ProcessPack: TProcessPack);
var
  i, j: Integer;
  ADMat : PDistanceMatrix;
  DistComputer: TSeqDistBase;
  ARP : TRuntimeProgress;
  MyAnalysisInfo : TPhyloQAnalysisInfo;
  AlignCodons: Boolean;
  IsDna: Boolean;
  SwappedOtuNameList: TStringList;
  NewNewickTree: AnsiString;
begin
  ADMat := nil;
  ARP := nil;
  MyAnalysisInfo := nil;
  DistComputer := nil;
  SwappedOtuNameList := nil;

  try try
    MyAnalysisInfo := TPhyloQAnalysisInfo.Create; // subclassed TAnalysisInfo to simplify our lives.
    MyAnalysisInfo.MyProcessPack := ProcessPack;
    MyAnalysisInfo.InitialUsrOperation := dtdoPhyloQ;

    // progress system setup
    ARP := TRuntimeProgress.Create(Application.MainForm);
    {$IFDEF VISUAL_BUILD}
    ARP.DataFilename := MegaForm.DataFileName;
    ARP.DataTitle := MegaForm.DataTitle;
    {$ELSE}
	  ARP.DataFileName  :=  D_MegaMain.DataFileName;
    ARP.DataTitle     :=  D_MegaMain.DataTitle;
    {$ENDIF}

    MyAnalysisInfo.ARP := ARP;
    ARP.FMAI := MyAnalysisInfo;

    if not MyAnalysisInfo.GetAnalysisOptions(dtdoPhyloQ) then
      Exit;
    ARP.WriteAnalysisOptionsToStdOut;
    AlignCodons := False;
    IsDna := not MyAnalysisInfo.isAminoAcid;

    MyAnalysisInfo.ReadTargetData;
    if MyAnalysisInfo.MyProcessPack.ContainsProcessType(ppMuscle) then
    begin
      if MyAnalysisInfo.MyProcessPack.ContainsProcessType(ppProfileAlignment) then
        MyAnalysisInfo.PairWiseAlignments := MuscleAlignProfileExecute(MyAnalysisInfo.MyProcessPack, AlignCodons, IsDna, '', MyAnalysisInfo)
      else
        MyAnalysisInfo.PairWiseAlignments := MuscleAlignPairwiseExecute(MyAnalysisInfo.MyProcessPack, AlignCodons, IsDna, '', MyAnalysisInfo)
    end
    else if MyAnalysisInfo.MyProcessPack.ContainsProcessType(ppClustalW) then
      MyAnalysisInfo.PairWiseAlignments := ClustalWAlignPairwiseExecute(MyAnalysisInfo)
    else
    begin
      {$IFDEF VISUAL_BUILD}
      raise Exception.Create('PhyloQ analysis is not yet supported for the visual version of MEGA.');
      {$ELSE}
      Error_NV('Unable to determine alignment method for pairwise alignments. Please check your settings file.');
      {$ENDIF}
    end;

    // now we need to subset the data
    ARP.AddRunStatusInfo('Status', 'Preparing data for distance based analysis');
    MyAnalysisInfo.MyMappedData := TList.Create;
    MyAnalysisInfo.MyIncludedSites := D_InputSeqData.PrepareDataForDistAnalysis(MyAnalysisInfo.MySubsetOpt, MyAnalysisInfo.MyMappedData, MyAnalysisInfo.MyUsedOtuInfos, MyAnalysisInfo.MyNoOfSeqs, MyAnalysisInfo.MyNoOfSites,MyAnalysisInfo.MyLabelsUsed, MyAnalysisInfo.RecodeScheme, MyAnalysisInfo.SiteCoverage);
    if MyAnalysisInfo.MyNoOfSites < 1 then
    begin
      {$IFDEF VISUAL_BUILD}
      RaiseErrorMessage(HC_No_Common_Sites, 'No common sites found for computing distances.  This is NOT an error with MEGA, please click the help button for an explanation the problem.');
      {$ELSE}
      Error_NV('No common sites found for computing distances.  This is NOT an error with MEGA, please click the help button for an explanation the problem.');
//    ARP.AddAnalysisOptions('No. of Sites', IntToStr(MyAnalysisInfo.MyNoOfSites));
      {$ENDIF}
    end;

    ARP.AddRunStatusInfo('Status', 'Parsing user tree...');
    if MyAnalysisInfo.GetUserTreeInput(dtdoPhyloQ, MyAnalysisInfo.MyUserTreeFile, MyAnalysisInfo.ARP, nil, ttNJ, False) then
    begin
      NewNewickTree := AddTaxaNameToNewickString(MyAnalysisInfo.PhyloQTargetName, MyAnalysisInfo.MyUserNewickTree);
      SwappedOtuNameList := TStringList.Create;
      SwappedOtuNameList.Assign(MyAnalysisInfo.MyOtuNames);
      SwappedOtuNameList.Insert(0,MyAnalysisInfo.PhyloQTargetName);
      SwappedOtuNameList.Exchange(0, SwappedOtuNameList.Count - 1);
      MyAnalysisInfo.MyOriTreeList := TTreeList.Create;
      MyAnalysisInfo.MyOriTreeList.ImportFromNewick(NewNewickTree, SwappedOtuNameList);
    end
    else
    begin
      {$IFDEF VISUAL_BUILD}
      raise Exception.Create('PhyloQ analysis is not yet supported in the visual version of MEGA');
      {$ELSE}
      Error_NV('Failed to process the given input tree.');
      {$ENDIF}
    end;


//    These may be needed for future, if and when we enable validation tests for this analysis
//    if MyAnalysisInfo.MyTreePack.DoesContain(ttBootstrap) then
//    begin
//      if MyAnalysisInfo.MyNoOfSeqs < 4 then
//        RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least four taxa are needed for bootstrapping.');
//    end
//    else if MyAnalysisInfo.MyTreePack.DoesContain(ttCPTest) then
//    begin
//      if MyAnalysisInfo.MyNoOfSeqs < 4 then
//        RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least four taxa are needed for the CP Test.');
//    end;

    //Allocate space for a tree
//    ATreeList := TTreeList.Create; // will be used later
//
//    if MyAnalysisInfo.MyTreePack.DoesContain(ttUserTree) then
//      ATreeList.ImportFromNewick(MyAnalysisInfo.MyUserNewickTree, nil);

    //Allocate Distance matrix
    ADMat := NewDistMatrix(MyAnalysisInfo.MyNoOfSeqs, True);
    ARP.AddRunStatusInfo('Status', 'Computing pairwise distances for target sequence...');
    ARP.Show;
    MyAnalysisInfo.ComputePwDistances;

    //Allocate DistComputer
    if MyAnalysisInfo.MyDistPack.DoesContain(gdOneNuc)    then
      DistComputer := TNucDist.Create
    else if MyAnalysisInfo.MyDistPack.DoesContain(gdSynNonsyn)
      then  DistComputer := TSynNonsynDist.Create
    else if MyAnalysisInfo.MyDistPack.DoesContain(gdAmino)
      then  DistComputer := TAminoDist.Create;

    //Initialize DistComputer
    DistComputer.DistPack  := MyAnalysisInfo.MyDistPack;
    DistComputer.NoOfSeqs  := MyAnalysisInfo.MyMappedData.Count;
    DistComputer.Sequences := MyAnalysisInfo.MyMappedData;
    DistComputer.QuickExit := True;
    DistComputer.D         := ADMat;
    DistComputer.NoOfSites := MyAnalysisInfo.MyNoOfSites;

    if DistComputer is TSynNonsynDist  then
      with DistComputer as TSynNonsynDist do
        CodeTable := D_InputSeqData.CodeTable;

//    May be needed in the future, if and when we add support for validation tests
//    if MyAnalysisInfo.MyTreePack.DoesContain(ttBootstrap) then
//      ARP.UpdateRunStatusInfo('Status', 'Checking data for bootstrapping')
//    else
//      ARP.UpdateRunStatusInfo('Status', 'Computing distances');
//    DistComputer.SetRuntimeProgress(ARP);
//    {$IFDEF VISUAL_BUILD}
//    DistComputer.SetStopButton(ARP.StopBtn);
//   {$ENDIF}

    ARP.AddRunStatusInfo('Status', 'Computing pairwise distances among reference sequences...');
    try
      if DistComputer is TNucDist then
      begin
        if MyAnalysisInfo.MyDistPack.DoesContain(gdMCL) then
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
        if E.ClassType = EOutOfMemory then
        begin
          {$IFDEF VISUAL_BUILD}
          raise Exception.Create('PhyloQ analysis is not yet supported in the visual version of MEGA.');
          {$ELSE}
          Error_NV('MEGA cannot allocate enough memory for this analysis and is unable to complete the calculation. This is probably due to a very '+LineEnding+'large number of taxa in the input data file.');
          Exit;
          {$ENDIF}
        end
        else
        begin
          {$IFDEF VISUAL_BUILD}
          raise Exception.Create('PhyloQ analysis is not yet supported in the visual version of MEGA.');
          {$ELSE}
          Error_NV('MEGA has encountered an error and cannot continue with this analysis:' + E.Message);
          Exit;
          {$ENDIF}
        end;
      end;
    end;


    // important for Koichiro based routines.
    for i:=0 to MyAnalysisInfo.MyNoOfSeqs-1 do
      ADMat[i,i] := 0;
    for i:=0 to MyAnalysisInfo.MyNoOfSeqs-1 do
      for j:=0 to i-1 do
        ADMat[j,i] := ADMat[i,j];

    MyAnalysisInfo.MyOriD := ADMat;
    MyAnalysisInfo.UpdateAugmentedMatrix;
    MyAnalysisInfo.MyShowD := NewDistMatrix(MyAnalysisInfo.MyNoOfSeqs + 1, True);
    CopyDistMatrix(MyAnalysisInfo.MyNoOfSeqs + 1, MyAnalysisInfo.MyShowD, MyAnalysisInfo.MyOriD, True);

    ARP.AddRunStatusInfo('Status', 'Performing least squares analysis for placements in user tree...');
    ADMat := nil;
    ComputeTree(MyAnalysisInfo, True);
    ARP := nil;
    MyAnalysisInfo := nil;

  except
    on E: Exception do
      ShowErrorMessage(E);
  end;
  finally
    if ADMat <> nil then
      FreeDistMatrix(ADMat, MyAnalysisInfo.MyNoOfSeqs); // ERROR!  How can we do FreeDistmatrix if MAI is set to nil right before the finally block.
    if DistComputer <> nil then
      DistComputer.Destroy;
  end;
end;

procedure ProcessPhylogenyBLensCommand(UsrOperation: TDistTreeDlgOption; ProcessPack: TProcessPack);
var
  Progress : TRuntimeProgress;
  MAI : TAnalysisInfo;
  Calibrations: TCalibrations;
  CalibrationFile: String;
  NamesList: TStringList;
  MaxRateRatio: Extended = DEFAULT_MAX_RATE_RATIO;
  {$IFDEF VISUAL_BUILD}mrrStr: String;{$ENDIF}
begin
  Progress := nil;
  MAI := nil;
  Calibrations := nil;
  CalibrationFile := EmptyStr;
  {$IFDEF VISUAL_BUILD}
  mrrStr := Format('%.0f', [DEFAULT_MAX_RATE_RATIO]);
  if (not IsPrototyper) and (not ProcessPack.IsWorkflowPack) then
  begin
    if not InputQuery('Maximum Relative Rate Ratio', 'Please provide a number between 5 and 100', mrrStr) then
      exit;
    while (not TryStrToFloat(mrrStr, MaxRateRatio)) or (MaxRateRatio < 5) or (MaxRateRatio > 100) do
      if not InputQuery('Maximum Relative Rate Ratio', 'Please provide a number between 5 and 100', mrrStr) then
        exit;
  end;
  {$ELSE}
  MaxRateRatio := ProcessPack.MaxRateRatio;
  {$ENDIF}

  try
    try
      MAI := TAnalysisInfo.Create;
      MAI.MyProcessPack := ProcessPack;
      MAI.PropagateConstraints := (not ProcessPack.HasProcessType(ppSkipMinMax));
      MAI.MyTreePack := TTreePack.Create;
      MAI.InitialUsrOperation := UsrOperation;
      MAI.MaxRateRatio := MaxRateRatio;
      Progress := TRuntimeProgress.Create(Application.MainForm);
      Progress.DataFileName := GetInputDataFileName;
      Progress.DataTitle := 'User-supplied phylogeny';
      MAI.ARP := Progress;
      Progress.FMAI := MAI;
      if (UsrOperation = dtdoRelTimeBLens) or (UsrOperation = dtdoRtdtBlens) then
      begin
        Progress.AddAnalysisOptions('Analysis', 'Estimate Relative Divergence Times (RelTime)');
        Progress.AddAnalysisOptions('Method', 'Compute divergence times from user-supplied branch lengths');
      end
      else
      begin
        Progress.AddAnalysisOptions('Analysis', 'CorrTest (branch lengths)');
        Progress.AddAnalysisOptions('Method', 'Rate correlation test from user-supplied branch lengths');
      end;

      if IsPrototyper and ((UsrOperation = dtdoCorrTestBlens) or (UsrOperation = dtdoRelTimeBLens) or (UsrOperation = dtdoRtdtBlens)) then
      begin
        MAI.GetAnalysisOptions(UsrOperation);
        if MAI.MyProcessPack.IsGettingWorkflowSettings then
          MAI := nil;
      end
      else
      begin
        Progress.AddAnalysisOptions('Optimization', 'None');
        Progress.WriteAnalysisOptionsToStdOut;
        Progress.AddRunStatusInfo('Status', 'Parsing newick file');
        Progress.Show;
        if not FileExists(ProcessPack.TreeFile) then
        begin
          {$IFDEF VISUAL_BUILD}
          raise Exception.Create('The analysis was aborted because the specified tree file was not found. Tree file: ' + ProcessPack.TreeFile);
          {$ELSE}
          error_NV('The analysis was aborted because the specified tree file was not found. Tree file: ' + ProcessPack.TreeFile);
          {$ENDIF}
        end;
        MAI.MyOriTreeList := TTreeList.Create;
        if (not IsPrototyper) and MAI.MyOriTreeList.ImportFromNewickFile(ProcessPack.TreeFile, nil) then
        begin
          NamesList := TStringList.Create;
          NamesList.Assign(MAI.MyOriTreeList.OTUNameList);
          if not MAI.MyOriTreeList[0].isBLen then
          begin
            {$IFDEF VISUAL_BUILD}
            raise Exception.Create('MEGA did not find branch lengths in the newick file that was specified and therefore cannot complete the RelTime calculation.');
            {$ELSE}
            Error_NV('MEGA did not find branch lengths in the newick file that was specified and therefore cannot complete the RelTime calculation.');
            {$ENDIF}
          end
          else
          begin
            Progress.UpdateRunStatusInfo('Status', 'Launching RelTime calculation thread');
            {$IFDEF VISUAL_BUILD}
            MAI.MyOriTreeList[0].isStats := True;
            {$ENDIF}
            if not MAI.LoadGroupAndOutgroupInfo(MAI.MyProcessPack.GroupsFile, NamesList) then
            begin
            {$IFNDEF VISUAL_BUILD}
              error_nv('Failed to load out-group information. Please check the format of the ' + ExtractFilename(MAI.MyProcessPack.GroupsFile + ' file'));
            {$ELSE}
              raise Exception.Create('Failed to load group information. Please check the format of the ' + ExtractFilename(MAI.MyProcessPack.GroupsFile + ' file'));
            {$ENDIF}
            end;
            RootOnOutgroup(MAI);
            if MAI.MyProcessPack.CalibrationFile <> EmptyStr then  // for MEGA-CC
              CalibrationFile := MAI.MyProcessPack.CalibrationFile
            else if MAI.CalibrationFile <> EmptyStr then // for MEGA-Visual
              CalibrationFile := MAI.CalibrationFile;
            if CalibrationFile <> EmptyStr then
            begin
              Calibrations := TCalibrations.Create;
              Calibrations.SetInfo(MAI);
              if not Calibrations.LoadFromFile(CalibrationFile) then
                raise Exception.Create('Failed to load calibration file');
              {$IFNDEF VISUAL_BUILD}
              Calibrations.FilterMissingTaxa(MAI.MyOriTreeList.OTUNameList);
              {$ENDIF}
              MAI.MyOriTreeList.ResolveCalibrationTimeNames(Calibrations);
              MAI.MyOriTreeList[0].resolveRelTimes(Calibrations);
              if not Assigned(MAI.MyOtuNames) then
                MAI.MyOtuNames := TStringList.Create;
              MAI.MyOtuNames.Clear;
              MAI.MyOtuNames.Assign(NamesList);
			  Calibrations.IsBLensOnly := True;
              MAI.CalibrationTimes := Calibrations;
              ComputeRelTimeBLens(MAI, MAI.CalibrationTimes);
            end
            else
            begin
              if MAI.InitialUsrOperation = dtdoCorrTestBlens then
                RunCorrTest(MAI)
              else
              begin
                MAI.Calibrationtimes := TCalibrations.Create;
                ComputeRelTimeBLens(MAI);
              end;
            end;
          end;
        end
        else
        begin
          {$IFDEF VISUAL_BUILD}
          raise Exception.Create('MEGA encountered a problem importing the newick file; it may not be a valid newick standard file.');
          {$ELSE}
          error_NV('MEGA encountered a problem importing the newick file; it may not be a valid newick standard file.');
          {$ENDIF}
          // no need to call Exit. Instead, the analysis is aborted naturally
        end;
      end;
  except
    on E: Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      if Assigned(Progress) then
        Progress.Hide;
      if Assigned(MAI) then
      begin
        MAI.MyProcessPack := nil;
        MAI.Free;
      end;
      ShowErrorMessage(E);
      {$ELSE}
      Error_NV(E.Message, E);
      {$ENDIF}
    end;
  end;
  finally
    if IsPrototyper then
    begin
      if Assigned(Progress) then
        Progress.Free;
      if Assigned(MAI) then
      begin
        MAI.MyProcessPack := nil;
        MAI.Free;
      end;
    end;
    Progress := nil;
    MAI := nil;
  end;
end;

procedure RunCorrTest(MAI: TAnalysisInfo);
var
  ctestThread: TCorrTestBlenThread = nil;
  Terminator: TRelTimeTerminator = nil;
  i: Integer = -1;
begin
  if not Assigned(MAI.MyOtuNames) then
  begin
    MAI.MyOtuNames := TStringList.Create;
    for i := 0 to MAI.MyOriTreeList.NoOfOTUs - 1 do
      MAI.MyOtuNames.Add(MAI.MyOriTreeList.OTUName[i]);
  end;
  ctestThread := TCorrTestBLenThread.Create;
  Terminator := TRelTimeTerminator.Create(dtdoCorrTestBlens);
  ctestThread.OnTerminate := Terminator.OnCorrTestThreadDone;
  ctestThread.AnalysisInfo := MAI;
  ctestThread.ProgressDlg := MAI.ARP;
  ctestThread.Start;
end;

function GetInputDataFileName: String;
begin
  Result := GetActiveDataFileName;
end;

function GetInputDataTitle: String;
begin
  Result := GetActiveDataTitle;
end;

procedure RootOnOutgroup(MAI: TAnalysisInfo);
var
  ADapter: TFpNodeTreeDataAdapter;
  AData: TTreeData;
begin
  ADapter := nil;
  Assert(Assigned(MAI.MyOriTreeList) and Assigned(MAI.MyOriTreeList[0]));

  try
    ADapter := TFpNodeTreeDataAdapter.Create;
    ADapter.SetTreeData(MAI.MyOriTreeList[0]);
    ADapter.RootOnOutgroup;
    AData := MAI.MyOriTreeList[0];
    ADapter.GetTreeData(AData);
  finally
    if Assigned(ADapter) then
      ADapter.Free;
  end;
end;

procedure RootOnOutgroup(aData: TTreeData);
var
  ADapter: TFpNodeTreeDataAdapter = nil;
begin
  Assert(Assigned(aData));

  try
    ADapter := TFpNodeTreeDataAdapter.Create;
    ADapter.SetTreeData(aData);
    ADapter.RootOnOutgroup;
    ADapter.GetTreeData(AData);
  finally
    if Assigned(ADapter) then
      ADapter.Free;
  end;
end;

function CheckAbortForIdenticalSeqsLS(aInfo: TAnalysisInfo): Boolean;
var
  checkAbort: TCheckAbortReltime = nil;
begin
  try
    checkAbort := TCheckAbortReltime.Create(aInfo.isAminoAcid);
    Result := checkAbort.CheckAbortOLS(aInfo.MyMappedData, aInfo.MyOtuNames, aInfo.NoOfSites, aInfo.ARP);
  finally
    if Assigned(checkAbort) then
      checkAbort.Free;
  end;
end;

end.



