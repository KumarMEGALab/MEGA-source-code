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

unit mtree_display_setup;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF VISUAL_BUILD}
  MTreeViewForm,
  {$ENDIF}
  Classes, SysUtils, Dialogs, MAnalysisInfo, MegaConsts, MTreeList,
  MComputeParsimInfo, MPartitionList, msitecoverage, mmodeltestmatrixexport,
  MLegendGenerator, MLTree, MLTreeAnalyzer, mcorrelationtest, AppLinker;

type

  { TTreeDisplaySetup }

  TTreeDisplaySetup = class(TObject)
    private
      MAI: TAnalysisInfo;
      CI, RI, RCI, iCI, iRI, iRCI: double;
      ComputeParsimInfo : TComputeParsimInfo;
      CComputer: TSiteCoverageComputer;
      {$IFNDEF VISUAL_BUILD}
      NewickFileName: String;
      {$ENDIF}
      procedure InitTreeViewForm;
      procedure UpdateTreeList;
      procedure UpdateTreeListML;

      {$IFNDEF VISUAL_BUILD}
      function ExportAncStates(ParsInfo: TComputeParsimInfo; TreeList: TTreeList; Filename: String): Boolean;
      function ExportAncChanges(ParsInfo: TComputeParsimInfo; TreeList: TTreeList; MLAnalyzer: TMLTreeAnalyzer; Filename: String): Boolean;
      function ExportPartitionsSummary(aPartitionsList: TPartitionList; fileExt: String = '_partitions.txt'): Boolean;
      function GenerateConsensusTree: Boolean;
      {$ENDIF}
      function CheckAbortBootstrapAnalysis(AnalysisInfo: TAnalysisInfo): Boolean; { bootstrap replicates may fail (i.e. 'no common sites found' so we should check if results are valid}
      procedure WriteResultsFiles;
      procedure WriteResultsFilesML;
      procedure WriteReltimeResultsFiles;
      procedure WriteModelTestResultsFiles(MatrixExport : TModelTestMatrixExport; CaptionExpert: TLegendGenerator);
      procedure RelinquishControl;
      procedure Initialize;
      procedure CleanUp;
      procedure DoComputeParsimInfo;
      procedure ExportSiteRateExecute(aMAI: TAnalysisInfo);
      function GetMyPegImageFiles: String;
      {$IFDEF VISUAL_BUILD}
      function DoSessionTest(t: TObject): Boolean;
      procedure SetupTreeViewFormObservers;
      {$ENDIF}
      function GetClockTestLegendGenerator: TLegendGenerator;
      function GetCorrtestMLLegendGenerator(aModelInfo: TModelInfo; test: TCorrelationTest): TLegendGenerator;
      function GetCorrtestBlensLegendGenerator(test: TCorrelationTest): TLegendGenerator;
    public
      {$IFDEF VISUAL_BUILD}
      TreeExpl: TTreeViewForm;
      {$ENDIF}
      constructor Create;
      destructor Destroy; override;
      procedure ShowSiteCoverage(aMAI: TAnalysisInfo);
      procedure ShowDistTree(aMAI: TAnalysisInfo);
      procedure ShowDistBootTree(aMAI: TAnalysisInfo);
      procedure ShowParsimTree(aMAI: TAnalysisInfo);
      procedure ShowParsimBootTree(aMAI: TAnalysisInfo);
      procedure ShowMLTree(aMAI : TAnalysisInfo);
      function ShowMLBootTree(aMAI: TAnalysisInfo): Boolean;
      //{*BEGIN_CLOSED_SOURCE}
      {$IFDEF CALTEST}
      procedure ShowCalcTestResults(aMAI: TAnalysisInfo; UserTimeTree, ReltimeTree, BLensTree: TSimpleTreeNodeArray);
      {$ENDIF}
      //{*END_CLOSED_SOURCE}
      procedure ShowModelTestResults(aMAI :TAnalysisInfo; exportType: TExportType; aLocation: String);
      procedure ShowClockTestResults(aMAI :TAnalysisInfo);
      procedure ShowCorrTestMLResult(aMAI: TAnalysisInfo; aResult: TCorrelationTest);
      procedure ShowCorrTestBLenResult(aMAI: TAnalysisInfo; aResult: TCorrelationTest);
      {$IFDEF VISUAL_BUILD}
      procedure ShowCorrTestTreeExplorerResult(aMAI: TAnalysisInfo; aResult: TCorrelationTest);
      {$ENDIF}
      function MakeClockTestSummary(aMAI: TAnalysisInfo): TStringList; // for mega-cc using FPC, THTMLViewer crashes so make the summary our self until we have time to fix it right
      procedure ShowRelTimeBLenResults(aMAI: TAnalysisInfo);
      procedure ShowRelTimeMLResults(aMAI: TAnalysisInfo; corrTest: TCorrelationTest = nil);
      procedure ShowRelTimeLSResults(aMAI: TAnalysisInfo);
      procedure ShowPatternResults(aMAI :TAnalysisInfo);
      procedure ShowSiteRateResults(aMAI :TAnalysisInfo);
      procedure ShowGeneDupsTree(AnalysisInfo: TAnalysisInfo);
  end;

implementation

uses
  LCLIntf, LCLType, mpattertestmatrixexport,
  {$IFDEF VISUAL_BUILD}
  Forms, MEditorForm, MWriteOutputDlg, MWebBrowser,
  MutationDetailView, MTreeBox, Types,
  {$ELSE}
  MD_MegaMain,{$IFDEF MSWINDOWS} Windows, {$ENDIF}mancestralstatesexporter, mancestralstatesexportheader, mreltimethreads,
  StrUtils, MPleaseWait, MTreeEstFunc, MGlobalSettings, MegaVerConsts,  MOtuInfo, MegaUtils_NV,
  {$ENDIF}
  MegaUtils, MegaErrUtils, MTreePack, MDistPack,
  Graphics, ExcelWrite, MMatrixExport,
   Math, MAnalysisSummary,  MProcessPack,
  MD_InputSeqData, StringUtils, mtabular_tree_export,
  mclocktestexport, MTreeDataAdapter, mmodel_test_results, MTreeData;

{ TTreeDisplaySetup }

procedure TTreeDisplaySetup.UpdateTreeList;
var
  i: Integer;
begin
  with MAI.MyOriTreeList do
  begin
    Information.Assign(MAI.ARP.AnalysisOptStrList);
    if MAI.MyTreePack.DoesContain(ttCPTest) then
    begin
      StatsName := 'CP value';
      MaxStats := 100;
    end;
    if Assigned(MAI.MyBootPartitionList) then
    begin
      Information.Add('No Of Bootstrap Reps = ' + IntToStr(MAI.MyNoOfReps));
      StatsName := 'Bootstrap value';
      MaxStats := MAI.MyBootPartitionList.TotalFrequency;
      MAI.MyBootPartitionList.Information.Assign(MAI.MyOriTreeList.Information);
      MAI.MyBootPartitionList.FreqName    := 'Frequency';
      MAI.MyBootPartitionList.StatsName   := 'Bootstrap value';
      if Assigned(MAI.MyBootD) then
      begin
        MAI.MyBootPartitionList.DistanceMatrix := MAI.MybootD;
        MAI.MyBootD := nil;
      end;
      for i := 0 to MAI.MyOtuNames.Count - 1 do
        MAI.MyBootPartitionList.OTUName[i] := MAI.MyOtuNames[i];
    end;
    for i := 0 to MAI.MyOtuNames.Count - 1 do
      MAI.MyOriTreeList.OTUName[i] := MAI.MyOtuNames[i];
  end;
end;

procedure TTreeDisplaySetup.UpdateTreeListML;
var
  i: Integer;
  {$IFDEF VISUAL_BUILD}
  ImagesDir: String = '';
  bootstrapInfo: TStringList = nil;
  exporter: TTabularTreeExport = nil;
  {$ENDIF}
  wasRootedTree: Boolean = False;
  siteCoverageThread: TSiteCoverageThread = nil;
begin
  with MAI.MyOriTreeList do
  begin
    if Assigned(MAI.MyBootPartitionList) then
    begin
      if MAI.MyOriTreeList.NoOfTrees = 0 then
        MAI.MyOriTreeList.Add(TTreeData.Create(MAI.NoOfSeqs, true, true, false));
      Information.Add('No Of Bootstrap Reps = '+IntToStr(MAI.MyBootReps));
      StatsName := 'Bootstrap value';
      if MAI.MyMLAnalysisPack.IsLittleBootstrap then
        MaxStats := MAI.NumRepsPerSubSample
      else
        MaxStats := MAI.MyBootPartitionList.TotalFrequency;
      ValueName := 'LogL';
    end
    else
    begin
      wasRootedTree := MAI.MyOriTreeList.isRooted;
      Clear;
      Add(TTreeData.Create(MAI.MyNoOfSeqs,true,false,false));
      MAI.MyMLAnalysisPack.GetTreeData(Items[0]);
      IsRooted := (wasRootedTree and ((MAI.MyUsrOperation = dtdoMLInferAncSeq) or (MAI.MyUsrOperation = dtdoMLInferAncSeqMyPeg) or (MAI.MyUsrOperation = dtdoMLComputeUserTreeBLens)));
    end;

    Information.Assign(MAI.ARP.AnalysisOptStrList);
    for i := 0 to MAI.MyOtuNames.Count-1 do
      OTUName[i] := MAI.MyOtuNames[i];
    MAI.MyTreePack.SearchLevel := Round(MAI.MyTreePack.SearchLevel/2);
    MAI.MyOriTreeList.ValueName := 'LogL';
    ValueName := 'LogL';
    {$IFDEF VISUAL_BUILD}
    if not MAI.MyMLAnalysisPack.IsLittleBootstrap then
    begin
      try
        siteCoverageThread := TSiteCoverageThread.CreateFromTreeData(MAI, MAI.MyOriTreeList[0]);
        siteCoverageThread.Start;
        siteCoverageThread.WaitFor;
      finally
        siteCoverageThread.Free;
      end;
    end;

    InitTreeViewForm;
    {$ENDIF}
    if Assigned(MAI.MyBootPartitionList) then
    begin
      {$IFDEF VISUAL_BUILD}
      if MAI.IsBootstrapSubSampling then
      begin
        try
          exporter := TTabularTreeExport.Create(MAI.MyOriTreeList);
          bootstrapInfo := exporter.GenerateLittleBootstrapsExport(0);
          bootstrapInfo.Insert(0, Format('Little Bootstraps(s = %d, r = %d, l = %.0n)', [MAI.NumBootstrapSubSamples, MAI.NumRepsPerSubSample, MAI.NumSitesPerSubSample*1.0]));
          OpenStringList(bootstrapInfo, 'Little Bootstraps Info');
        finally
          if Assigned(bootstrapInfo) then
            bootstrapInfo.Free;
          if Assigned(exporter) then
            exporter.Free;
        end;
        if IsDeveloper and Assigned(MAI.DeveloperLog) then
          OpenStringList(MAI.DeveloperLog, 'LBS Run Log');
      end;
      {$ENDIF}
      with MAI.MyBootPartitionList do
      begin
        Information.Assign(MAI.MyOriTreeList.Information);
        for i := 0 to MAI.MyOtuNames.Count-1 do
         OTUName[i] := MAI.MyOtuNames[i];
        StatsName := 'Bootstrap value';
        ValueName := 'LogL';
        for i := 0 to MAI.MyOriTreeList.Count-1 do
        begin
          MAI.MyMLAnalysisPack.ModelInfoList.Add(TModelInfo.Create);
          MAI.MyMLAnalysisPack.GetModelInfo(MAI.MyMLAnalysisPack.ModelInfoList[i]);
        end;
        {$IFDEF VISUAL_BUILD}
        MAI.AnalysisSummary.AddAnalysisInfo(MAI);
        MAI.AnalysisSummary.ModelInfoList := MAI.MyMLAnalysisPack.ModelInfoList.Items[0].ToStringList;
        TreeExpl.SetData(MAI);
        {$ELSE}
        WriteResultsFilesML;
        {$ENDIF}
      end;
    end
    else
    begin
      if  MAI.MyTreePack.DoesContain(ttInferTree) or
          MAI.MyTreePack.DoesContain(ttAncState) or
          MAI.MyTreePack.DoesContain(ttBLens)
      then
      begin
        {$IFDEF VISUAL_BUILD}
        {$IFNDEF CALTEST}
        MAI.AnalysisSummary.AddAnalysisInfo(MAI);
        for i := 0 to MAI.MyOriTreeList.Count-1 do
        begin
          MAI.MyMLAnalysisPack.ModelInfoList.Add(TModelInfo.Create);
          MAI.MyMLAnalysisPack.GetModelInfo(MAI.MyMLAnalysisPack.ModelInfoList[i]);
        end;
        MAI.AnalysisSummary.ModelInfoList := MAI.MyMLAnalysisPack.ModelInfoList.Items[0].ToStringList;
        //InitTreeViewForm;
        TreeExpl.SetData(MAI);
        TreeExpl.Show;
        if MAI.MyTreePack.DoesContain(ttAncState) then
          TreeExpl.ActionAncStateShowAllExecute(nil);
        if MAI.MyTreePack.DoesContain(ttClock) and (not (MAI.InitialUsrOperation = dtdoMLComputeUserTreeBLens)) then
          TreeExpl.ActionLinearizedExecute(nil);
        if MAI.IsMyPegAnalysis then
        begin
          MutationDetailViewForm.AncMLAction.Enabled := True;
          MutationDetailViewForm.AncParsimonyAction.Enabled := True;
          TreeExpl.SetAncestralSiteNum(MAI.MyPegSite);
          TreeExpl.ActionRootByOutgroupExecute(nil);
          TreeExpl.ActionTopologyExecute(nil);
          try
            ImagesDir := GetMyPegImageFiles;
            if DirectoryExists(ImagesDir) then
              TreeExpl.LoadImagesDirect(ImagesDir);
          except
            // don't sweat it
          end;
        end;
        {$ENDIF CALTEST}
        {$ELSE VISUAL_BUILD}
        WriteResultsFilesML;
        {$ENDIF}
      end;
    end;
  end;
end;

{$IFNDEF VISUAL_BUILD}
function TTreeDisplaySetup.ExportAncStates(ParsInfo: TComputeParsimInfo;TreeList: TTreeList; Filename: String): Boolean;
var
  Exporter: TParsimonyAncestralStatesExporter;
  Header: TAncStatesExportHeader;
  i: Integer;
begin
  Exporter := nil;
  Header := nil;

  try
    for i := 0 to TreeList.NoOfTrees - 1 do
    begin
      try
        Header := GenerateAncStatesExportHeader(TreeList, i);
        Exporter := TParsimonyAncestralStatesExporter.Create(ParsInfo, Header, TreeList.NoOfOTUs, Header.Count, fALSE);
        Exporter.NewickString := TreeList.OutputNewickTree(i, False, False, 0.0);
        if TreeList.NoOfTrees > 1 then
          Exporter.ExportStates(EXText, ChangeFileExt(Filename, '_' + IntToStr(i) + '.txt'))
        else
          Exporter.ExportStates(EXtext, Filename);
      finally
        if Assigned(Header) then
          Header.Free;
        if Assigned(Exporter) then
          Exporter.Free;
      end;
    end;
  except
    on E:Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Oh no! An error occurred while exporting ancestral states: ' + E.Message);
      {$ELSE}
      warn_nv('Failed to export ancestral sequences: ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

function TTreeDisplaySetup.ExportAncChanges(ParsInfo: TComputeParsimInfo;TreeList: TTreeList; MLAnalyzer: TMLTreeAnalyzer; Filename: String): Boolean;
var
  ExportHeader: TAncStatesExportHeader = nil;
  Exporter: TParsimonyAncestralStatesExporter = nil;
  i: Integer;
  TreeAdapter: TSimpleTreeDataAdapter = nil;
begin
  Result := False;
  try
    for i := 0 to TreeList.NoOfTrees - 1 do
    begin
      try
        TreeAdapter := TSimpleTreeDataAdapter.Create;
        TreeAdapter.SetTreeData(TreeList[i], TreeList.isRooted);
        ExportHeader := GenerateAncStatesExportHeader(TreeList, i);
        Exporter := TParsimonyAncestralStatesExporter.Create(ExportHeader, ParsInfo, TreeList, TreeList.Items[i], TreeList.NoOfOTUs, TreeAdapter.NumNodes, MLAnalyzer, False);
        if TreeList.NoOfTrees > 1 then
           Exporter.ExportChangesList(EXText, ChangeFileExt(Filename, '_' + IntToStr(i) + '.txt'))
        else
          Exporter.ExportChangesList(EXtext, Filename);
        Result := True;
    except
      on E:Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Oh no! An error occurred while exporting ancestral changes: ' + E.Message);
      {$ELSE}
      warn_nv('Failed to export ancestral changes: ' + E.Message);
      {$ENDIF}
    end;
    end;
  end;
  finally
    if Assigned(Exporter) then
       FreeAndNil(Exporter);
    if Assigned(ExportHeader) then
        ExportHeader.Free;
    if Assigned(TreeAdapter) then
        TreeAdapter.Free;
  end;
end;

function TTreeDisplaySetup.ExportPartitionsSummary(aPartitionsList: TPartitionList; fileExt: String): Boolean;
var
  PartitionsSummary: TStringList = nil;
  Filename: String;
begin
  Result := False;
  Filename := ChangeFileExt(NextAvailableFilenameNV('.txt'), fileExt);

  try
    try
      PartitionsSummary := aPartitionsList.PartitionsSummary;
      PartitionsSummary.SaveToFile(Filename);
      Result := FileExists(Filename);
      if not Result then
        warn_nv('Failed to properly save the partitions list to ' + Filename);
    except
      on E:Exception do
        warn_nv('An error occurred when saving the partitions list: ' + E.Message);
    end;
  finally
    if Assigned(PartitionsSummary) then
      PartitionsSummary.Free;
  end;
end;

function TTreeDisplaySetup.GenerateConsensusTree: Boolean;
var
  ConsensusTreeData: TTreeData = nil;
  aData: TTreeData = nil;
  i: Integer;
  NoOfOtus: Integer;
begin
  Result := False;
  if MAI.MyOriTreeList.NoOfTrees > 0 then
  begin
    NoOfOtus := MAI.MyOriTreeList.NoOfOTUs;
    ConsensusTreeData := TTreeData.Create(NoOfOTUs, MAI.MyOriTreeList.isBLen, MAI.MyOriTreeList.isSe, true);
    MAI.MyBootPartitionList.GetConsensus(ConsensusTreeData);
    aData := MAI.MyOriTreeList[0];
    ConsensusTreeData.AssignDataCoverage(aData);
    for i := 0 to NoOfOTUs - 3 do
      ConsensusTreeData.Stats[i+NoOfOTUs] := ConsensusTreeData.Stats[i];
    for i := 0 to NoOfOTUs-1 do
      ConsensusTreeData.Stats[i] := 0.0;
    if MAI.IsBootstrapSubSampling then
      ConsensusTreeData.isSE := False;
    MAI.MyOriTreeList.Add(ConsensusTreeData);
    Result := True;
  end;
end;
{$ENDIF}

procedure TTreeDisplaySetup.InitTreeViewForm;
begin
  {$IFDEF VISUAL_BUILD}
  if MAI.MyUsrOperation = dtdoBuildTumorTree then
  begin
  end
  else
    TreeExpl := TTreeViewForm.Create(Application);

  if Assigned(MAI.MyProcessPack) and (MAI.MyProcessPack.IsWorkflowPack) then
    SetupTreeViewFormObservers;
  {$ENDIF}
end;

procedure TTreeDisplaySetup.RootOnHg19(aList: TTreeList);
var
  aData: TTreeData;
  i: Integer;
  rooter: TFpNodeTreeDataAdapter = nil;
begin
  try
    aList.MarkTaxonAsOutgroupMember('Normal');
    rooter := TFpNodeTreeDataAdapter.Create;
    for i := 0 to aList.Count - 1 do
    begin
      aData := aList[i];
      rooter.SetTreeData(aData);
      rooter.RootOnOutgroup;
      rooter.GetTreeData(aData);
    end;
  finally
    if Assigned(rooter) then
      rooter.Free;
  end;
end;

function TTreeDisplaySetup.CheckAbortBootstrapAnalysis(AnalysisInfo: TAnalysisInfo): Boolean;
var
  NumGoodReps: Integer;
  NumRepsTried: Integer;
  aMessage: String;
begin
  Result := False;
  NumRepsTried := Max(AnalysisInfo.MyNoOfReps, AnalysisInfo.MyBootReps);
  if NumRepsTried <= 0 then
    raise Exception.Create('Invalid number of bootstrap replicates: ' + IntToStr(NumRepsTried));

  NumGoodReps := AnalysisInfo.MyValidReps;
  if NumGoodReps = NumRepsTried then
    Exit;

  if NumGoodReps = 0 then
  begin
    Result := True;
    aMessage := 'The calculation has been aborted because all bootstrap replicates failed.';
    if AnalysisInfo.MyBootPartitionList.ErrorMessages.Count > 0 then
      aMessage := aMessage + ' ' + AnalysisInfo.MyBootPartitionList.ErrorMessages[0];
  end
  else if NumGoodReps < 25 then
  begin
    Result := True;
    aMessage := Format('The calculation has been aborted because %d of %d bootstrap replicates failed (minimum 25 replicates are needed to continue).', [NumRepsTried - NumGoodReps, NumRepsTried]);
    if AnalysisInfo.MyBootPartitionList.ErrorMessages.Count > 0 then
      aMessage := aMessage + ' ' + AnalysisInfo.MyBootPartitionList.ErrorMessages[0];
  end
  else
  begin
    aMessage := Format('Notice: During the bootstrapping process %d of %d replicates failed and so bootstrap values are based on only %d replicates.', [NumRepsTried - NumGoodReps, NumRepsTried, NumGoodReps]);
    if AnalysisInfo.MyBootPartitionList.ErrorMessages.Count > 0 then
      aMessage := aMessage + ' ' + AnalysisInfo.MyBootPartitionList.ErrorMessages[0];
    if AnalysisInfo.MyNoOfReps > 0 then
      AnalysisInfo.MyNoOfReps := NumGoodReps
    else if AnalysisInfo.MyBootReps > 0 then
      AnalysisInfo.MyBootReps := NumGoodReps;
  end;
  {$IFDEF VISUAL_BUILD}
  ShowMessage(aMessage);
  {$ELSE}
  if Result = True then
    error_nv(aMessage)
  else
    warn_nv(aMessage);
  {$ENDIF}
end;

procedure TTreeDisplaySetup.WriteResultsFiles;
begin
  {$IFNDEF VISUAL_BUILD}
  MAI.ARP.UpdateRunStatusInfo('Status', 'Writing Newick file...');
  NewickFileName := NextAvailableFileNameNV('.nwk');
  Mai.MyOriTreeList.ExportToNewickFile(NewickFileName, True, True, 0, D_MegaMain.MaxNumResults);
  MAI.ARP.UpdateRunStatusInfo('Status', 'Performing cleanup...');
  D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
  D_MegaMain.AnalysisSummary.WriteToFile(ChangeFileExt(newickfilename, '_summary.txt'));
  if ((MAI.MyNoOfReps > 0) or (MAI.ValidReps > 0)) and Assigned(MAI.MyBootPartitionList) then
  begin
    if IsPhyloQAnalysis then
      Mai.MyOriTreeList.ExportToNewickFile(NewickFilename, True, True, 0, D_MegaMain.MaxNumResults)
    else
    begin
      if MAI.IsBootstrapSubSampling then
        MAI.MyOriTreeList.ExportATreeToNewickFile(0, ChangeFileExt(NewickFileName, '_baby-bootstrap.nwk'), true, true, 0)
      else
      begin
        //MAI.MyOriTreeList.ExportATreeToNewickFile(0, NewickFileName, true, true, 0);
        if IsDeveloper and (MAI.ValidReps > 0) then
          MAI.MyOriTreeList[0].BootstrapValsToCsvFile(NextAvailableFilenameNV('_bootstrap_vals.csv'), MAI.ValidReps, MAI.MyOriTreeList.isRooted);
      end;
      if not GenerateConsensusTree then
        raise Exception.Create('Generation of consensus tree failed');
      MAI.MyOriTreeList.ExportATreeToNewickFile(MAI.MyOriTreeList.Count - 1, ChangeFileExt(NewickFileName, '_consensus.nwk'), false, true, 0);
    end;
    ExportPartitionsSummary(MAI.MyBootPartitionList);
  end;
  {$ENDIF}
end;

procedure TTreeDisplaySetup.WriteResultsFilesML;
{$IFNDEF VISUAL_BUILD}
var
  i: Integer;
  treeExport: TTabularTreeExport = nil;
{$ENDIF}
begin
  {$IFNDEF VISUAL_BUILD}
  if Assigned(MAI.MyBootPartitionList) then
  begin
    if MAI.IsBootstrapSubSampling then
    begin
      NewickFileName := NextAvailableFilenameNV('_baby-bootstrap.nwk');
      MAI.MyOriTreeList.ExportATreeToNewickFile(0, NewickFileName, False, True, 0);
      try
        treeExport := TTabularTreeExport.Create(MAI.MyOriTreeList);
        treeExport.ExportLittleBootstrapsTree(0, NextAvailableFilenameNV('_baby-bootstrap.txt'));
      finally
        if Assigned(treeExport) then
          treeExport.Free;
      end;
    end
    else
    begin
      NewickFileName := NextAvailableFilenameNV('.nwk');
      MAI.MyOriTreeList.ExportATreeToNewickFile(0, NewickFileName, true, true, 0);
      if IsDeveloper then
        MAI.MyOriTreeList[0].BootstrapValsToCsvFile(NextAvailableFilenameNV('_bootstrap_vals.csv'), MAI.ValidReps, MAI.MyOriTreeList.isRooted);
    end;
    if MAI.IsBootstrapSubSampling then
      MAI.MyOriTreeList.MaxStats := MAI.MyOriTreeList.MaxStats*MAI.NumBootstrapSubSamples;
    if not GenerateConsensusTree then
      raise Exception.Create('Generation of consensus tree failed');
    MAI.MyOriTreeList.ExportATreeToNewickFile(MAI.MyOriTreeList.Count -1, ChangeFileExt(NewickFileName, '_consensus.nwk'), false, true, 0);
    if MAI.IsBootstrapSubSampling then
    begin
      try
        treeExport := TTabularTreeExport.Create(MAI.MyOriTreeList);
        treeExport.ExportLittleBootstrapsTree(MAI.MyOriTreeList.Count -1, NextAvailableFilenameNV('_consensus.txt'));
      finally
        if Assigned(treeExport) then
          treeExport.Free;
      end;
    end;
    ExportPartitionsSummary(MAI.MyBootPartitionList);
    D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
    D_MegaMain.AnalysisSummary.ModelInfoList := MAI.MyMLAnalysisPack.ModelInfoList.Items[0].ToStringList;
    D_MegaMain.AnalysisSummary.WriteToFile(ChangeFileExt(newickfilename, '_summary.txt'));
  end
  else
  begin
    if MAI.MyTreePack.DoesContain(ttAncState) then
    begin
      if MAI.MyTreePack.DoesContain(ttPredictLivingSeq) then
      begin
        if D_MegaMain.DoAllFiles then
        begin
          for i := 1 to MAI.NoOfSeqs do
            MAI.MyMLAnalysisPack.ExportPredictLivingSequences(i);
        end
        else
          MAI.MyMLAnalysisPack.ExportPredictLivingSequences(1)
      end
      else
        MAI.MyMLAnalysisPack.ExportAllAncStates(MAI.MyOtuNames, MAI.MyOriTreeList.isRooted);
    end;

    for i := 0 to MAI.MyOriTreeList.Count-1 do
    begin
      MAI.MyMLAnalysisPack.ModelInfoList.Add(TModelInfo.Create);
      MAI.MyMLAnalysisPack.GetModelInfo(MAI.MyMLAnalysisPack.ModelInfoList[i]);
    end;
    NewickFileName := NextAvailableFilenameNV('.nwk');
    Mai.MyOriTreeList.ExportToNewickFile(NewickFilename, true, true, 0, D_MegaMain.MaxNumResults);
    D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
    D_MegaMain.AnalysisSummary.ModelInfoList := MAI.MyMLAnalysisPack.ModelInfoList.Items[0].ToStringList;
    D_MegaMain.AnalysisSummary.WriteToFile(ChangeFileExt(newickfilename, '_summary.txt'));
  end;
  {$ENDIF}
end;

procedure TTreeDisplaySetup.WriteReltimeResultsFiles;
{$IFNDEF VISUAL_BUILD}
var
  FileName: String;
  TempList: TStringList = nil;
  NewickStr: String;
{$ENDIF}
begin
  {$IFNDEF VISUAL_BUILD}
  try
    FileName := NextAvailableFileNameNV('.txt');
    MAI.ClockTreeExport.SaveToFile(FileName);
    D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
    if Assigned(MAI.MyMLAnalysisPack) then
      D_MegaMain.AnalysisSummary.ModelInfoList := MAI.MyMLAnalysisPack.ModelInfoList.Items[0].ToStringList;
    D_MegaMain.AnalysisSummary.WriteToFile(ChangeFileExt(Filename, '_summary.txt'));

    TempList := TStringList.Create;
    if Assigned(MAI.MyMLAnalysisPack) then
      NewickStr := MAI.MyMLAnalysisPack.TimeTreeToNewickString(MAI.MyOtuNames, False)
    else
      NewickStr := MAI.MyReltimeComputer.TimeTreeToNewickString(MAI.MyOtuNames, False);
    TempList.Add(NewickStr);
    TempList.SaveToFile(ChangeFileExt(Filename, '_relTimes.nwk'));

    if MAI.HasCalibrations then
    begin
      if Assigned(MAI.MyMLAnalysisPack) then
        NewickStr := MAI.MyMLAnalysisPack.TimeTreeToNewickString(MAI.MyOtuNames, True)
      else
        NewickStr := MAI.MyReltimeComputer.TimeTreeToNewickString(MAI.MyOtuNames, True);
      TempList.Clear;
      TempList.Add(NewickStr);
      TempList.SaveToFile(ChangeFileExt(Filename, '_exactTimes.nwk'));
    end;

    if Assigned(MAI.ReltimeNexusExport) then
       MAI.ReltimeNexusExport.SaveToFile(ChangeFileExt(Filename, '_nexus.tre'));
  finally
    if Assigned(TempList) then
      TempList.Free;
  end;
  {$ENDIF}
end;

procedure TTreeDisplaySetup.WriteModelTestResultsFiles(MatrixExport: TModelTestMatrixExport; CaptionExpert: TLegendGenerator);
{$IFNDEF VISUAL_BUILD}
var
  ExportType: TExportType;
  ResultsFileName: String;
{$ENDIF}
begin
  {$IFNDEF VISUAL_BUILD}
  ExportType := OutputFileTypeToExportType(D_MegaMain.OutputFormat);
  if ExportType = ExText then
    CaptionExpert.SaveCaptionToFile()
  else if (ExportType = ExCsvSave) or (ExportType = ExCsvDisp) then
  begin
    ResultsFileName := NextAvailableFileNameNV('.csv');
    MatrixExport.SavedExcelWrite.SaveFile(ResultsFileName, ExportCSV)
  end
  else if (ExportType = EXexcelSave) or (ExportType = EXexcelDisp) then
  begin
    ResultsFileName := NextAvailableFileNameNV('.xls');
    MatrixExport.SavedExcelWrite.SaveFile(ResultsFileName, ExportExelXML);
  end;
  D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
  D_MegaMain.AnalysisSummary.WriteToFile(ChangeFileExt(ResultsFileName, '_summary.txt'));
  {$ENDIF}
end;

procedure TTreeDisplaySetup.RelinquishControl;
begin
  if Assigned(MAI) and Assigned(MAI.ARP) then
    FreeAndNil(MAI.ARP);
  MAI := nil;
  {$IFDEF VISUAL_BUILD}
  if Assigned(TreeExpl) then
  begin
    {$IFDEF DARWIN}TreeExpl.Tree.Refresh;{$ENDIF}
    TreeExpl := nil;
  end;
  {$ENDIF}
end;

procedure TTreeDisplaySetup.Initialize;
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(MAI) and Assigned(MAI.ARP) then
    MAI.ARP.Hide;
  if Assigned(MAI) and Assigned(MAI.AnalysisSummary) then
    MAI.AnalysisSummary.AddAnalysisInfo(MAI);
  {$ENDIF}
end;

procedure TTreeDisplaySetup.CleanUp;
begin
  if Assigned(MAI) then
  begin
    if Assigned(MAI.ARP) then
      FreeAndNil(MAI.ARP);
    MAI.Free;
  end;
  {$IFDEF VISUAL_BUILD}
  if Assigned(TreeExpl) then
  begin
    TreeExpl.FormIsClosing := True;
    TreeExpl.Free;
  end;
  {$ENDIF}
  if Assigned(ComputeParsimInfo) then
    ComputeParsimInfo.Free;
  if Assigned(CComputer) then
    CComputer.Free;
end;

procedure TTreeDisplaySetup.DoComputeParsimInfo;
{$IFNDEF VISUAL_BUILD}
var
  i: Integer;
{$ENDIF}
begin
  with MAI.MyOriTreeList do
  begin
    {$IFNDEF VISUAL_BUILD}
      ComputeParsimInfo := TComputeParsimInfo.Create;
      ComputeParsimInfo.SetNucData(not(dsoUseAmino in MAI.MySubsetOpt));
      ComputeParsimInfo.NoOfSites := MAI.MyNoOfSites;
      ComputeParsimInfo.SetSeqs(MAI.MyInfoSites);
      ComputeParsimInfo.SetTreeData(MAI.MyOriTreeList[0]);
      ComputeParsimInfo.ComputeTreeIndices(CI, RI, RCI, iCI, iRI, iRCI);
      for i := 0 to MAI.MyOriTreeList.Count - 1 do
      begin
        MAI.MyOriTreeList[i].Value := ComputeParsimInfo.ComputeTreeLen;
        if  MAI.InitialUsrOperation = dtdoMPComputeUserTreeBLens then { per Sudhir's request, don't compute blens for tree search}
        begin
          MAI.MyOriTreeList[i].isBLen := True;
          ComputeParsimInfo.SetTreeData(MAI.MyOriTreeList[i]);
          ComputeParsimInfo.ComputeAvgMPBLens(MAI.MyOriTreeList[i].BLenArray);
        end;
      end;
      ExportAncStates(ComputeParsimInfo, MAI.MyOriTreeList, ChangeFileExt(NextAvailableFilenameNV('.txt'), '_ancestral_states.txt'));
      D_MegaMain.AnalysisSummary.AddParsimonyInfoString('Tree Length', FormatDoubleSafe(MAI.MyOriTreeList[0].Value, 0, 8));
      D_MegaMain.AnalysisSummary.AddParsimonyInfoString('CI',FormatDoubleSafe(CI, 3, 8) + '(Consistency Index for all sites)');
      D_MegaMain.AnalysisSummary.AddParsimonyInfoString('RI',FormatDoubleSafe(RI, 3, 8)+ '(Retention Index for all sites)');
      D_MegaMain.AnalysisSummary.AddParsimonyInfoString('RC',FormatDoubleSafe(RCI, 3, 8)+ '(Rescaled Index for all sites)');
      D_MegaMain.AnalysisSummary.AddParsimonyInfoString('iCI',FormatDoubleSafe(iCI, 3, 8) + '(Consistency Index for parsimony informative sites)');
      D_MegaMain.AnalysisSummary.AddParsimonyInfoString('iRI',FormatDoubleSafe(iRI, 3, 8)+ '(Retention Index for parsimony informative sites)');
      D_MegaMain.AnalysisSummary.AddParsimonyInfoString('iRC',FormatDoubleSafe(iRCI, 3, 8)+ '(Rescaled Index for parsimony informative sites)');
      //ExportAncChanges(ComputeParsimInfo, MAI.MyOriTreeList, MAI.MyMLAnalysisPack, ChangeFileExt(NextAvailableFilenameNV('.txt'), '_changes_list.txt')); { turned of per Sudhir's request}

    {$ELSE}
      TreeExpl.ComputeParsimInfo := TComputeParsimInfo.Create;
      TreeExpl.ComputeParsimInfo.SetNucData(not(dsoUseAmino in MAI.MySubsetOpt));
      TreeExpl.ComputeParsimInfo.NoOfSites := MAI.MyNoOfSites;
      TreeExpl.ComputeParsimInfo.SetSeqs(MAI.MyInfoSites);
      TreeExpl.ComputeParsimInfo.SetTreeData(MAI.MyOriTreeList[0]);
      TreeExpl.ComputeParsimInfo.ComputeTreeIndices(CI, RI, RCI, iCI, iRI, iRCI);
      MAI.MyOriTreeList[0].Value := TreeExpl.ComputeParsimInfo.ComputeTreeLen;

      MAI.AnalysisSummary.AddParsimonyInfoString('Tree Length', FormatDoubleSafe(MAI.MyOriTreeList[0].Value, 0, 8));
      MAI.AnalysisSummary.AddParsimonyInfoString('CI',FormatDoubleSafe(CI, 3, 8) + '(Consistency Index for all sites)');
      MAI.AnalysisSummary.AddParsimonyInfoString('RI',FormatDoubleSafe(RI, 3, 8)+ '(Retention Index for all sites)');
      MAI.AnalysisSummary.AddParsimonyInfoString('RC',FormatDoubleSafe(RCI, 3, 8)+ '(Rescaled Index for all sites)');
      MAI.AnalysisSummary.AddParsimonyInfoString('iCI',FormatDoubleSafe(iCI, 3, 8) + '(Consistency Index for parsimony informative sites)');
      MAI.AnalysisSummary.AddParsimonyInfoString('iRI',FormatDoubleSafe(iRI, 3, 8)+ '(Retention Index for parsimony informative sites)');
      MAI.AnalysisSummary.AddParsimonyInfoString('iRC',FormatDoubleSafe(iRCI, 3, 8)+ '(Rescaled Index for parsimony informative sites)');
    {$ENDIF}

    Information.Add('CI = '+FloatToStrF(CI, ffFixed, 15, 6) + '(for all sites)');
    Information.Add('RI = '+FloatToStrF(RI, ffFixed, 15, 6)+ '(for all sites)');
    Information.Add('RCI = '+FloatToStrF(RCI, ffFixed, 15, 6)+ '(for all sites)');
    Information.Add('iCI = '+FloatToStrF(iCI, ffFixed, 15, 6) + '(for parsimony informative sites)');
    Information.Add('iRI = '+FloatToStrF(iRI, ffFixed, 15, 6)+ '(for parsimony informative sites)');
    Information.Add('iRCI = '+FloatToStrF(iRCI, ffFixed, 15, 6)+ '(for parsimony informative sites)');
    ValueName := 'TreeLength';
  end;
end;

function TTreeDisplaySetup.GetMyPegImageFiles: String;
begin
  try
    GetPrivateFile('private' + PathDelim + 'myPeg' + PathDelim + 'bitmaps' + PathDelim + 'cavPor3.bmp');
    GetPrivateFile('private' + PathDelim + 'myPeg' + PathDelim + 'bitmaps' + PathDelim + 'equCab2.bmp');
    GetPrivateFile('private' + PathDelim + 'myPeg' + PathDelim + 'bitmaps' + PathDelim + 'gorGor1.bmp');
    GetPrivateFile('private' + PathDelim + 'myPeg' + PathDelim + 'bitmaps' + PathDelim + 'hg19.bmp');
    GetPrivateFile('private' + PathDelim + 'myPeg' + PathDelim + 'bitmaps' + PathDelim + 'mm9.bmp');
    GetPrivateFile('private' + PathDelim + 'myPeg' + PathDelim + 'bitmaps' + PathDelim + 'oryCun2.bmp');
    GetPrivateFile('private' + PathDelim + 'myPeg' + PathDelim + 'bitmaps' + PathDelim + 'panTro2.bmp');
    GetPrivateFile('private' + PathDelim + 'myPeg' + PathDelim + 'bitmaps' + PathDelim + 'ponAbe2.bmp');
    GetPrivateFile('private' + PathDelim + 'myPeg' + PathDelim + 'bitmaps' + PathDelim + 'rheMac2.bmp');
    Result := ExtractFileDir(GetPrivateFile('private' + PathDelim + 'myPeg' + PathDelim + 'bitmaps' + PathDelim + 'rn4.bmp'));
  except
    // don't sweat it
  end;
end;

{$IFDEF VISUAL_BUILD}
function TTreeDisplaySetup.DoSessionTest(t: TObject): Boolean;
var
  filename: String = '';
  te: TTreeViewForm = nil;
  aMsg: String = '';
begin
  try
    te := TTreeViewForm(t);
    filename := GetTempFileName;
    filename := ChangeFileExt(filename, '.mtsx');
    Result := te.SaveSession(filename, aMsg);
    if not Result then
      raise Exception.Create('tree session (SAVE) file test failed');
    Result := te.RetrieveSession(filename);
    if not Result then
      raise Exception.Create('tree session (LOAD) file test failed');
  finally
    if FileExists(filename) then
      DeleteFile(filename);
    te.Close;
    Halt(0);
  end;
end;
{$ENDIF}

function TTreeDisplaySetup.GetClockTestLegendGenerator: TLegendGenerator;
var
  Analyzer: TMLTreeAnalyzer = nil;
  TreeList: TTreeList = nil;
begin
  Analyzer := MAI.MyMLAnalysisPack;
  TreeList := MAI.MyOriTreeList;
  Result := TLegendGenerator.Create;
  Result.LoadTemplateFromFile('Molecular_clock.htm');
  Result.AssignData('LogLWithClock', MAI.LogLikelihoodWithClock);
  Result.AssignData('LogLWithoutClock', MAI.LogLikelihoodWithoutClock);
  Result.AssignData('NoOfParamsWClock', IntToStr(Analyzer.ModelInfoList[1].NoOfParams-(TreeList[0].NoOfOTUs-2)));
  Result.AssignData('NoOfParamsWoClock', IntToStr(Analyzer.ModelInfoList[0].NoOfParams));
  Result.AssignData('NoOfSeqs', IntToStr(Analyzer.NoOfSeqs));
  Result.AssignData('MolClockTest', MAI.MolecularClockTest);
  Result.AssignData('InvarWClock', MAI.InvarWithClock);
  Result.AssignData('InvarWoClock', MAI.InvarWithoutClock);
  Result.AssignData('GammaWClock', MAI.GammaWithClock);
  Result.AssignData('GammaWoClock', MAI.GammaWithoutClock);
  Result.AssignData('FullModelName', MAI.FullModelName);
  Result.AssignData('DegreesOfFreedom', MAI.DegreesOfFreedom);
  Result.AssignData('ChiStatistic', FloatToStrF(StrToFloat(MAI.LogLikelihoodWithoutClock) - StrToFloat(MAI.LogLikelihoodWithClock), ffFixed, 12, lnLDigits));
  Result.BindData(MAI.MyDistPack);
  Result.BindData(MAI);
  Result.BindData(MAI.MyTreePack);
end;

function TTreeDisplaySetup.GetCorrtestMLLegendGenerator(aModelInfo: TModelInfo; test: TCorrelationTest): TLegendGenerator;
var
  str: String;
  i: Integer;
begin
  Result := TLegendGenerator.Create;
  Result.LoadTemplateFromFile('corrtest.htm');
  Result.BindData(MAI);
  Result.BindData(MAI.MyDistPack);
  Result.BindData(MAI.MyTreePack);
  Result.BindData(test);
  Result.AssignData('IsML', 'true');
  Result.AssignData('CorrtestCaptionString', test.GetCaptionString);
  Result.AssignData('CorrtestCitation', 'Tao_et_al_2017');
  Result.AssignData('CorrtestScore', FormatDoubleSafe(test.Score, 4, 8, True));
  Result.AssignData('LogLikelihoodCE', Format('%.2f', [MAI.MyMLAnalysisPack.LogLikelihood]));
  if MAI.MyDistPack.DoesContain(gdGamma) then
  begin
    Result.AssignData('GammaPara', FormatDoubleSafe(aModelInfo.Gamma, 4, 12));
    Result.AssignData('NoOfGCats', IntToStr(aModelInfo.NoOfRates));
    str := FloatToStrF(aModelInfo.Rate[0], ffFixed, 12, paraDigits);
    for i := 1 to aModelInfo.NoOfRates-1 do
      str := str + ', ' + FloatToStrF(aModelInfo.Rate[i], ffFixed, 12, paraDigits);
    Result.AssignData('CatRates', str);
  end
  else
    Result.AssignData('GammaPara', 'N/A');
  if MAI.MyDistPack.DoesContain(gdInvar) then
    Result.AssignData('PropOfInvariant', Format('%.0f', [aModelInfo.Invar*100]))
  else
    Result.AssignData('PropOfInvariant', 'N/A');
end;

function TTreeDisplaySetup.GetCorrtestBlensLegendGenerator(test: TCorrelationTest): TLegendGenerator;
begin
  Result := TLegendGenerator.Create;
  Result.LoadTemplateFromFile('corrtest.htm');
  Result.BindData(MAI);
  Result.BindData(MAI.MyDistPack);
  Result.BindData(MAI.MyTreePack);
  Result.BindData(test);
  Result.AssignData('IsML', 'false');
  Result.AssignData('CorrtestCaptionString', test.GetCaptionString);
  Result.AssignData('CorrtestCitation', 'Tao_et_al_2017');
  Result.AssignData('CorrtestScore', FormatDoubleSafe(test.Score, 4, 8, True));
end;

{$IFDEF VISUAL_BUILD}
procedure TTreeDisplaySetup.SetupTreeViewFormObservers;
begin
  if MAI.MyProcessPack.IsWorkflowPack and Assigned(MAI.MyProcessPack.WorkflowElement) and Assigned(TreeExpl) then
    MAI.MyProcessPack.WorkflowElement.AddResultForm(TreeExpl);
end;
{$ENDIF}

constructor TTreeDisplaySetup.Create;
begin
  {$IFDEF VISUAL_BUILD}
  TreeExpl := nil;
  {$ENDIF}
  MAI := nil;
  ComputeParsimInfo := nil;
end;

destructor TTreeDisplaySetup.Destroy;
begin
  CleanUp;
  inherited Destroy;
end;

procedure TTreeDisplaySetup.ShowSiteCoverage(aMAI: TAnalysisInfo);
begin
  try
    MAI := aMAI;
    Initialize;
    UpdateTreeList;
    {$IFDEF VISUAL_BUILD}
    InitTreeViewForm;
    TreeExpl.SetData(MAI);
    TreeExpl.DisableCaption;
    TreeExpl.Tree.ShowDataCoverage := False;
    TreeExpl.ActionDataCoverageDisplayExecute(nil);
    {$ELSE}
    WriteResultsFiles;
    {$ENDIF}
    RelinquishControl;
  except
    On E: Exception do
      ShowErrorMessage(E);
  end;
end;


procedure TTreeDisplaySetup.ShowDistTree(aMAI: TAnalysisInfo);
begin
  try
    MAI := aMAI;
    Initialize;
    UpdateTreeList;
    with MAI.MyOriTreeList do
    begin
      DistanceMatrix := MAI.MyShowD;
      MAI.MyShowD := nil;
      IsRooted := MAI.MyTreePack.DoesContain(ttUpgma);
    end;
    {$IFDEF VISUAL_BUILD}
    InitTreeViewForm;
    TreeExpl.SetData(MAI);
    {$ELSE}
    WriteResultsFiles;
    {$ENDIF}
    RelinquishControl;
  except
    On E: Exception do
      ShowErrorMessage(E);
  end;
end;

procedure TTreeDisplaySetup.ShowDistBootTree(aMAI: TAnalysisInfo);
begin
  if CheckAbortBootstrapAnalysis(aMAI) then
  begin
    FreeAndNil(aMAI.ARP);
    FreeAndNil(aMAI);
    Exit;
  end;

  try
    MAI := aMAI;
    Initialize;
    UpdateTreeList;

    with MAI.MyOriTreeList do
    begin
      DistanceMatrix := MAI.MyShowD;
      MAI.MyShowD := nil;
      IsRooted := MAI.MyTreePack.DoesContain(ttUpgma);
    end;
    {$IFDEF VISUAL_BUILD}
    InitTreeViewForm;
    TreeExpl.SetData(MAI);
    {$ELSE}
    WriteResultsFiles;
    {$ENDIF}
    RelinquishControl;
  except
    On E: Exception do
      ShowErrorMessage(E);
  end;
end;

procedure TTreeDisplaySetup.ShowParsimTree(aMAI: TAnalysisInfo);
var
  ImagesDir: AnsiString;
  siteCoverageThread: TSiteCoverageThread = nil;
begin
  try
    MAI := aMAI;
    Initialize;
    UpdateTreeList;

    with MAI.MyOriTreeList do
    begin
      if (MAI.MyUsrOperation = dtdoFindClones) then
        IsRooted := True;
      //MAI.MyTreePack.SearchLevel := Round(MAI.MyTreePack.SearchLevel/2);
      InitTreeViewForm;
      DoComputeParsimInfo;
      if (MAI.InitialUsrOperation = dtdoMPInferAncSeq) or (MAI.InitialUsrOperation = dtdoMPInferAncSeqMyPeg) or (MAI.InitialUsrOperation = dtdoMPPredictLivingSeq) then
      begin
        try
          siteCoverageThread := TSiteCoverageThread.CreateFromTreeData(MAI, MAI.MyOriTreeList[0]);
          siteCoverageThread.Start;
          siteCoverageThread.WaitFor;
        finally
          siteCoverageThread.Free;
        end;
      end;
      {$IFDEF VISUAL_BUILD}
      TreeExpl.SetData(MAI);
      {$IFNDEF CALTEST}
      if MAI.MyTreePack.DoesContain(ttAncState) then
        TreeExpl.ActionAncStateShowAllExecute(nil);

      if MAI.IsMyPegAnalysis then
      begin
        MutationDetailViewForm.AncMLAction.Enabled := True;
        MutationDetailViewForm.AncParsimonyAction.Enabled := True;
        TreeExpl.SetAncestralSiteNum(MAI.MyPegSite);
        try
          ImagesDir := GetMyPegImageFiles;
          if DirectoryExists(ImagesDir) then
            TreeExpl.LoadImagesDirect(ImagesDir);
        except
          // don't sweat it
        end;
      end;
      {$ENDIF CALTEST}
      {$ELSE VISUAL_BUILD}
      WriteResultsFiles;
      {$ENDIF}
      RelinquishControl;
    end;
  except
    On E: Exception do
      ShowErrorMessage(E);
  end;
end;

procedure TTreeDisplaySetup.ShowParsimBootTree(aMAI: TAnalysisInfo);
begin
  MAI := aMAI;
  if CheckAbortBootstrapAnalysis(MAI) = True then
  begin
    FreeAndNil(MAI.ARP);
    FreeAndNil(MAI);
    Exit;
  end;

  try
    Initialize;
    UpdateTreeList;
    InitTreeViewForm;
    DoComputeParsimInfo;
    {$IFNDEF VISUAL_BUILD}
    WriteResultsFiles;
    {$ELSE}
    TreeExpl.SetData(MAI);
    {$ENDIF}
    RelinquishControl;
  except
    On E: Exception do
      ShowMessage('Application Error: ' + E.Message);
  end;
end;

procedure TTreeDisplaySetup.ShowMLTree(aMAI: TAnalysisInfo);
begin
  try
    MAI := aMAI;
    Initialize;
    UpdateTreeListML;
    {$IFDEF VISUAL_BUILD}
    if IsSessionTest then
      if not DoSessionTest(TreeExpl) then
        raise Exception.Create('tree session file testing failed');
    {$ENDIF}
    RelinquishControl;
  except
    On E: Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      if IsSessionTest then
        raise Exception.Create(E.Message)
      else
        ShowMessage('Oh no! An error occurred when displaying the tree: ' + E.Message);
      {$ELSE}
      Error_NV(E.Message, E);
      {$ENDIF}
    end;
  end;
end;

function TTreeDisplaySetup.ShowMLBootTree(aMAI: TAnalysisInfo): Boolean;
begin
  Result := False;
  MAI := aMAI;
  if CheckAbortBootstrapAnalysis(MAI) = True then
  begin
    MAI.MyMLAnalysisPack := nil; { will be freed by TMLTreeThread}
    FreeAndNil(MAI.ARP);
    FreeAndNil(MAI);
    Exit;
  end;

  Result := True;
  try
    Initialize;
    UpdateTreeListML;
    {$IFDEF VISUAL_BUILD}
    if IsSessionTest then
      if not DoSessionTest(TreeExpl) then
        raise Exception.Create('tree session file testing failed');
    {$ENDIF}
    RelinquishControl;
  except
    On E: Exception do
      if IsSessionTest then
        raise Exception.Create(E.Message)
      else
        ShowErrorMessage(E);
  end;
end;


procedure TTreeDisplaySetup.ShowModelTestResults(aMAI: TAnalysisInfo; exportType: TExportType; aLocation: String);
var
  {$IFDEF VISUAL_BUILD}
  ChromiumBrowser: TChromiumBrowser = nil;
  {$ENDIF}
  MatrixExport : TModelTestMatrixExport = nil;
  testResults: TModelTestResults = nil;
  CaptionExpert : TLegendGenerator = nil;
begin
  try
    try
      MAI := aMAI;
      Initialize;
      testResults := TModelTestResults.Create(MAI);
      MatrixExport := TestResults.GetMatrixExport;
      CaptionExpert := testResults.GetCaptionExpert;
      if (exportType <> EXnone) and MAI.MyProcessPack.IsWorkflowPack then
        MatrixExport.DoResultsExport(exportType, aLocation)
      else
      begin
        {$IFDEF VISUAL_BUILD}
        ChromiumBrowser := CreateNewChromiumBrowserWindow(bmResults);
        ChromiumBrowser.LoadHtmlFromString(CaptionExpert.GenerateLegend);
        ChromiumBrowser.SetMatrixExport(MatrixExport);
        MatrixExport := nil;
        MAI.AnalysisSummary.AddAnalysisInfo(MAI);
        if Assigned(MAI.MyProcessPack) and MAI.MyProcessPack.IsWorkflowPack and Assigned(MAI.MyProcessPack.WorkflowElement) then
          MAI.MyProcessPack.WorkflowElement.AddResultForm(ChromiumBrowser);
        {$ELSE}
        WriteModelTestResultsFiles(MatrixExport, CaptionExpert);
        {$ENDIF}
      end;
    except
      on E:Exception do
      begin
      {$IFDEF VISUAL_BUILD}{$IFNDEF CALTEST}
      if ChromiumBrowser <> nil then
      begin
        ChromiumBrowser.actExportExcel.Enabled := False;
        ChromiumBrowser.actExportCsv.Enabled := False;
      end;
      {$ENDIF CALTEST}{$ENDIF}
      end;
    end;
  finally
   if Assigned(testResults) then
     testResults.Free;
   RelinquishControl;
   if Assigned(CaptionExpert) then
     CaptionExpert.Free;
   if Assigned(MatrixExport) then
     MatrixExport.Free;
  end;
end;

procedure TTreeDisplaySetup.ShowClockTestResults(aMAI: TAnalysisInfo);
var
  {$IFDEF VISUAL_BUILD}
  ChromiumBrowser : TChromiumBrowser = nil;
  {$ELSE}
  Legend: String;
  FileName: String;
  {$ENDIF}
  CaptionStringList: TStringList = nil;
  Summary: TAnalysisSummary = nil;
  Analyzer: TMLTreeAnalyzer = nil;
  LegendGenerator: TLegendGenerator = nil;
  aExport: TClockTestExport = nil;
begin
  try
    try
      MAI := aMAI;
      Initialize;

      Analyzer := MAI.MyMLAnalysisPack;
      LegendGenerator := GetClockTestLegendGenerator;
      {$IFDEF VISUAL_BUILD}
      CaptionStringList := TStringList.Create;
      LegendGenerator.GenerateNoteAsText(CaptionStringList);
      aExport := TClockTestExport.Create;
      aExport.Initialize(MAI, Analyzer, CaptionStringList);
      ChromiumBrowser := CreateNewChromiumBrowserWindow(bmResults);
      ChromiumBrowser.loadHtmlFromString(LegendGenerator.GenerateLegend);
      ChromiumBrowser.SetMatrixExport(aExport);
      Summary := MAI.AnalysisSummary;
      Summary.AddAnalysisInfo(MAI);
      Summary.ModelInfoList := MAI.MyMLAnalysisPack.ModelInfoList.Items[0].ToStringList;
      Summary.ModelInfoListWithClock := MAI.MyMLAnalysisPack.ModelInfoList.Items[1].ToStringList;
      {$ELSE}
      FileName := NextAvailableFileNameNV('.txt');
      Legend := LegendGenerator.GenerateLegend;
      CaptionStringList := MakeClockTestSummary(MAI);
      CaptionStringList.SaveToFile(ChangeFileExt(FileName, '_clockTest.txt'));
      Summary := D_MegaMain.AnalysisSummary;
      Summary.AddAnalysisInfo(MAI);
      Summary.ModelInfoList := MAI.MyMLAnalysisPack.ModelInfoList.Items[0].ToStringList;
      Summary.ModelInfoListWithClock := MAI.MyMLAnalysisPack.ModelInfoList.Items[1].ToStringList(MAI.MyOriTreeList.NoOfOTUs - 2);
      Summary.WriteToFile(ChangeFileExt(Filename, '_summary.txt'));
      {$ENDIF}
  except
    On E: Exception do
      ShowErrorMessage(E);
  end;
  finally
    MAI.MyMLAnalysisPack := nil;
    FreeAndNil(MAI);
    if Assigned(LegendGenerator) then
      LegendGenerator.Free;
    if Assigned (CaptionStringList) then
      CaptionStringList.Free;
  end;
end;

procedure TTreeDisplaySetup.ShowCorrTestMLResult(aMAI: TAnalysisInfo; aResult: TCorrelationTest);
var
  aModelInfo: TModelInfo = nil;
  aCaption: TLegendGenerator = nil;
  captionString: String;
  {$IFDEF VISUAL_BUILD}
  aChromium: TChromiumBrowser = nil;
  {$ELSE}
  filename: String;
  aList: TStringList = nil;
  {$ENDIF}
begin
  try
    try
      MAI := aMAI;
      AModelInfo := TModelInfo.Create;
      MAI.MyMLAnalysisPack.GetModelInfo(AModelInfo);
      aCaption := GetCorrtestMLLegendGenerator(aModelInfo, aResult);
      {$IFDEF VISUAL_BUILD}
      captionString := aCaption.GenerateLegend;
      aChromium := CreateNewChromiumBrowserWindow(bmCaption);
      aChromium.LoadHtmlFromString(captionString);
      {$ELSE}
      aList := TStringList.Create;
      aList.Text := aCaption.GenerateLegend;
      D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
      filename := NextAvailableFilenameNV('.txt');
      aResult.SaveSummaryToFile(ChangeFileExt(filename, '_corrtest.txt'));
      D_MegaMain.AnalysisSummary.WriteToFile(ChangeFileExt(filename, '_summary.txt'));
      aList.SaveToFile(ChangeFileExt(filename, '_caption.html'));
      {$ENDIF}
    except
      on E:Exception do
      {$IFDEF VISUAL_BUILD}
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
      {$ELSE}
        error_nv('CorrTest calculation failed', E);
      {$ENDIF}
    end;
  finally
    FreeAndNil(MAI.ARP);
    FreeAndNil(MAI);
    aResult.Free;
    if Assigned(aModelInfo) then
      aModelInfo.Free;
    if Assigned(aCaption) then
      aCaption.Free;
    {$IFNDEF VISUAL_BUILD}
    if Assigned(aList) then
      aList.Free;
    {$ENDIF}
  end;
end;

procedure TTreeDisplaySetup.ShowCorrTestBLenResult(aMAI: TAnalysisInfo; aResult: TCorrelationTest);
var
  aCaption: TLegendGenerator = nil;
  captionString: String;
  {$IFDEF VISUAL_BUILD}
  aChromium: TChromiumBrowser = nil;
  {$ELSE}
  aList: TStringList = nil;
  filename: String = '';
  {$ENDIF}
begin
  try
    try
      MAI := aMAI;
      aCaption := GetCorrtestBlensLegendGenerator(aResult);
      {$IFDEF VISUAL_BUILD}
      captionString := aCaption.GenerateLegend;
      aChromium := CreateNewChromiumBrowserWindow(bmCaption);
      aChromium.LoadHtmlFromString(captionString);
      {$ELSE}
      aList := TStringList.Create;
      aList.Text := aCaption.GenerateLegend;
      D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
      filename := NextAvailableFilenameNV('.txt');
      aResult.SaveSummaryToFile(ChangeFileExt(filename, '_corrtest.txt'));
      D_MegaMain.AnalysisSummary.WriteToFile(ChangeFileExt(filename, '_summary.txt'));
      aList.SaveToFile(ChangeFileExt(filename, '_caption.html'));
      {$ENDIF}
    except
      on E:Exception do
      {$IFDEF VISUAL_BUILD}
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
      {$ELSE}
        error_nv('CorrTest calculation failed', E);
      {$ENDIF}
    end;
  finally
    FreeAndNil(MAI.ARP);
    FreeAndNil(MAI);
    aResult.Free;
    if Assigned(aCaption) then
      aCaption.Free;
    {$IFNDEF VISUAL_BUILD}
    if Assigned(aList) then
      aList.Free;
    {$ENDIF}
  end;
end;

{$IFDEF VISUAL_BUILD}
procedure TTreeDisplaySetup.ShowCorrTestTreeExplorerResult(aMAI: TAnalysisInfo; aResult: TCorrelationTest);
var
  aModelInfo: TModelInfo = nil;
  aCaption: TLegendGenerator = nil;
  captionString: String;
  aChromium: TChromiumBrowser = nil;
begin
  try
    try
      MAI := aMAI;
      if Assigned(aMAI.MyMLAnalysisPack) then
       begin
         AModelInfo := TModelInfo.Create;
         aMAI.MyMLAnalysisPack.GetModelInfo(AModelInfo);
         aCaption := GetCorrtestMLLegendGenerator(aModelInfo, aResult);
       end
       else
       begin
         aCaption := GetCorrtestBlensLegendGenerator(aResult);
       end;
       captionString := aCaption.GenerateLegend;
       aChromium := CreateNewChromiumBrowserWindow(bmCaption);
       aChromium.LoadHtmlFromString(captionString);
    except
      on E:Exception do
        ShowMessage('Application error when displaying CorrTest result: ' + E.Message);
    end;
  finally
    MAI :=  nil;
    if Assigned(aCaption) then
      aCaption.Free;
    if Assigned(aModelInfo) then
      aModelInfo.Free;
  end;
end;
{$ENDIF}

function TTreeDisplaySetup.MakeClockTestSummary(aMAI: TAnalysisInfo): TStringList;
var
  TL: TTreeList = nil;
  Analyzer: TMLTreeAnalyzer = nil;
  lnL: Double;
  Params: Integer;
  Gamma: Double;
  GammaStr: String;
  Invariant: Double;
  InvariantStr: String;
  Note: String;
begin
  MAI := aMAI;
  Analyzer := MAI.MyMLAnalysisPack;
  TL := MAI.MyOriTreeList;
  Result := TStringList.Create;

  Result.Add(Format('               %9s %6s %7s %6s', ['lnL', 'Params', 'Gamma', 'Invariant']));

  lnL := StrToFloat(MAI.LogLikelihoodWithClock);
  Params := Analyzer.ModelInfoList[1].NoOfParams - (TL.NoOfOTUs - 2);

  if MAI.MyDistPack.ComputeGammaParameter then
  begin
    Gamma := StrToFloat(MAI.GammaWithClock);
    GammaStr := Format('%7.3f', [Gamma]);
  end
  else
    GammaStr := 'n/a';

  if MAI.MyDistPack.ComputeInvarParameter then
  begin
    Invariant := StrToFloat(MAI.InvarWithClock);
    InvariantStr := Format('%9.3f', [Invariant]);
  end
  else
    InvariantStr := 'n/a';

  Result.Add(Format('With_Clock     %8.3f %6d %7s %9s', [lnL, Params, GammaStr, InvariantStr]));

  lnL := StrToFloat(MAI.LogLikelihoodWithoutClock);
  Params := Analyzer.ModelInfoList[0].NoOfParams;

  if MAI.MyDistPack.ComputeGammaParameter then
  begin
    Gamma := StrToFloat(MAI.GammaWithoutClock);
    GammaStr := Format('%7.3f', [Gamma]);
  end
  else
    GammaStr := 'n/a';

  if MAI.MyDistPack.ComputeInvarParameter then
  begin
    Invariant := StrToFloat(MAI.InvarWithoutClock);
    InvariantStr := Format('%9.3f', [Invariant]);
  end
  else
    InvariantStr := 'n/a';
  Result.Add(Format('Without_Clock  %8.3f %6d %7s %9s', [lnL, Params, GammaStr, InvariantStr]));


  Note := 'NOTE.-- The molecular clock test was performed by comparing the ML value for the given topology with and without the molecular clock constraints under the ' + MAI.FullModelName + '.';
  if MAI.MyDistPack.ComputeGammaParameter then
    Note := Note + ' Differences in evolutionary rates among sites were modeled using a discrete Gamma (G) distribution (shape parameter shown)';
  if MAI.MyDistPack.ComputeInvarParameter then
    Note := Note + ' and allowed for invariant (I) sites to exist  (Estimate of invariant sites is shown';
  Note := Note + '. The null hypothesis of equal evolutionary rates throughout the tree was ';
  if StrToFloat(MAI.MolecularClockTest) < 0.05 then
    Note := Note + 'rejected '
  else
    Note := Note + 'not rejected ';
  Note := Note + 'at a 5% significance level(P = ' + Format('%.5e', [StrToFloat(MAI.MolecularClockTest)]) + ', ';
  Note := Note + 'chi-square = ' + Format('%.5e', [(2*(StrToFloat(MAI.LogLikelihoodWithoutClock) - StrToFloat(MAI.LogLikelihoodWithClock)))]) + ', ';
  Note := Note + 'df = '  + MAI.DegreesOfFreedom + ').';
  Result.Add(Note);
end;

procedure TTreeDisplaySetup.ShowRelTimeBLenResults(aMAI: TAnalysisInfo);
begin
  try
    try
      MAI := aMAI;
      Initialize;
      MAI.MyOriTreeList.Information.Assign(MAI.ARP.AnalysisOptStrList);
      {$IFDEF VISUAL_BUILD}
      {$IFNDEF CALTEST}MAI.AnalysisSummary.AddAnalysisInfo(MAI);{$ENDIF CALTEST}
      InitTreeViewForm;
      TreeExpl.SetData(MAI);
      TreeExpl.Tree.LinearizeFunc := nil;
      {$ELSE VISUAL_BUILD}
      WriteReltimeResultsFiles;
      {$ENDIF}
      MAI.MyMLAnalysisPack := nil;
      RelinquishControl;
  except
    On E: Exception do
      ShowErrorMessage(E);
  end;
  finally
    if Assigned(MAI) then
    begin
      if MAI.ARP <> nil then
        FreeAndNil(MAI.ARP);
      MAI.MyMLAnalysisPack := nil;
      FreeAndNil(MAI);
    end;
  end;
end;

procedure TTreeDisplaySetup.ShowRelTimeMLResults(aMAI: TAnalysisInfo; corrTest: TCorrelationTest);
{$IFNDEF VISUAL_BUILD}
var
  Filename: String = '';
{$ENDIF}
begin
  try
    try
      MAI := aMAI;
      Initialize;

      MAI.MyOriTreeList.Information.Assign(MAI.ARP.AnalysisOptStrList);
      {$IFDEF VISUAL_BUILD}
      {$IFNDEF CALTEST}
      MAI.AnalysisSummary.AddAnalysisInfo(MAI);
      MAI.MyOriTreeList.isRooted := True;
      InitTreeViewForm;
      TreeExpl.SetData(MAI);
      TreeExpl.Tree.LinearizeFunc := nil;
      TreeExpl.FigureGenerator.AssignData('LogLikelihoodCE', Format('%.2f', [MAI.MyMLAnalysisPack.LogLikelihood]));
      TreeExpl.FigureGenerator.AssignData('FullModelName', MAI.MyMLAnalysisPack.ModelInfoList[0].FullName);
      TreeExpl.FigureGenerator.AssignData('NoOfSeqs', IntToStr(MAI.MyMLAnalysisPack.NoOfSeqs));
      MAI.AnalysisSummary.ModelInfoList := MAI.MyMLAnalysisPack.ModelInfoList.Items[0].ToStringList;
      {$ENDIF CALTEST}
      {$ELSE}
      WriteReltimeResultsFiles;
      if IsDeveloper and Assigned(corrTest) then
      begin
        Filename := NextAvailableFilenameNV('_corrtest.txt');
        corrTest.SaveSummaryToFile(Filename);
      end;
      {$ENDIF}
      MAI.MyMLAnalysisPack := nil;
      RelinquishControl;
  except
    On E: Exception do
      ShowErrorMessage(E);
  end;
  finally
    {$IFNDEF VISUAL_BUILD}
    if IsDeveloper and Assigned(corrTest) then
      corrTest.Free;
    {$ENDIF}
    if Assigned(MAI) then
    begin
      if MAI.ARP <> nil then
        FreeAndNil(MAI.ARP);
      FreeAndNil(MAI);
    end;
  end;
end;

procedure TTreeDisplaySetup.ShowRelTimeLSResults(aMAI: TAnalysisInfo);
begin
  try
    try
      MAI := aMAI;
      Initialize;
      InitTreeViewForm;
      MAI.MyOriTreeList.Information.Assign(MAI.ARP.AnalysisOptStrList);
      {$IFDEF VISUAL_BUILD}
      {$IFNDEF CALTEST}
      MAI.AnalysisSummary.AddAnalysisInfo(MAI);
      MAI.MyOriTreeList.isRooted := True;
      TreeExpl.SetData(MAI);
      TreeExpl.Tree.LinearizeFunc := nil;
      {$ENDIF CALTEST}
      {$ELSE}
      WriteReltimeResultsFiles;
      {$ENDIF}
      RelinquishControl;
  except
    On E: Exception do
      ShowErrorMessage(E);
  end;
  finally
    if Assigned(MAI) then
    begin
      if MAI.ARP <> nil then
        FreeAndNil(MAI.ARP);
      FreeAndNil(MAI);
    end;
  end;
end;

procedure TTreeDisplaySetup.ShowPatternResults(aMAI: TAnalysisInfo);
var
  Nucleotides: TStringList = nil;
  TableHTMLString: String;
  Caption: TStringList = nil;
  AminoAcids: TStringList = nil;
  str, HTMLtoLoad: String;
  i,j: integer;
  CaptionExpert : TLegendGenerator = nil;
  {$IFDEF VISUAL_BUILD}
  ChromiumBrowser: TChromiumBrowser = nil;
  {$ENDIF}
  MatrixExport : TPatternTestMatrixExport = nil;
  AModelInfo: TModelInfo = nil;
  CaptionStringList: TStringList = nil;
begin
  try
    MAI := aMAI;
    Initialize;


    AModelInfo := TModelInfo.Create;
    MAI.MyMLAnalysisPack.GetModelInfo(AModelInfo);

    with AModelInfo do
    begin
      CaptionExpert := TLegendGenerator.Create;
      CaptionExpert.LoadTemplateFromFile('ML_Substitution_Pattern.htm');
      CaptionExpert.AssignData('LogL', FloatToStrF(LogL, ffFixed, 12, lnLDigits));
      CaptionExpert.AssignData('NoOfSeqs', IntToStr(MAI.NoOfSeqs));
      CaptionExpert.AssignData('NoOfSites', IntToStr(MAI.NoOfSites));

      if MAI.MyMLAnalysisPack.IsUserSuppliedTree then
        CaptionExpert.AssignData('TreeUsedString','For estimating ML values, a user-specified topology was used.')
      else
        CaptionExpert.AssignData('TreeUsedString','For estimating ML values, a tree topology was automatically computed.');

      if (DataType = 'DNA') then
      begin
        Nucleotides := TStringList.Create;
        Nucleotides.CommaText := 'A, T/U, C, G';
        CaptionExpert.AssignData('FreqA', FloatToStrF(Freq[0]*100, ffFixed, 12, paraDigits)+'%');
        CaptionExpert.AssignData('FreqT', FloatToStrF(Freq[1]*100, ffFixed, 12, paraDigits)+'%');
        CaptionExpert.AssignData('FreqC', FloatToStrF(Freq[2]*100, ffFixed, 12, paraDigits)+'%');
        CaptionExpert.AssignData('FreqG', FloatToStrF(Freq[3]*100, ffFixed, 12, paraDigits)+'%');

        TableHTMLString := '<TABLE><TR><TH>&nbsp;</TH><TH>A</TH><TH>T/U</TH><TH>C</TH><TH>G</TH></TR>';
        for i:= 0 to Nucleotides.Count -1 do
        begin
          if i = Nucleotides.Count-1 then TableHTMLString := TableHTMLString + '<TR class="lastRow">'
                                     else TableHTMLString := TableHTMLString + '<TR>';

          for j := -1 to Nucleotides.Count-1  do
          begin
            if      j = -1 then TableHTMLString := TableHTMLString + '<TD class="rowHeader">' + AnsiUpperCase(Nucleotides.Strings[i]) + '</TD>'
            else if j =  i then TableHTMLString := TableHTMLString + '<TD align="center" nowrap = "nowrap">-</TD>'
            else
            begin
              TableHTMLString := TableHTMLString + '<TD align="center" nowrap = "nowrap">';
              if (j+i) = 3 then  TableHTMLString := TableHTMLString + '<B>'
                           else  TableHTMLString := TableHTMLString + '<I>';
              TableHTMLString := TableHTMLString +   FloatToStrF(Matrix[i,j]*100, ffFixed, 12, paraDigits) + '</TD>';
              if (j+i) = 3 then  TableHTMLString := TableHTMLString + '</B>'
                           else  TableHTMLString := TableHTMLString + '</I>';
            end;
          end;
          TableHTMLString := TableHTMLString + '</TR>';
        end;
        TableHTMLString := TableHTMLString + '</TABLE>';
        CaptionExpert.AssignData('MatrixTable', TableHTMLString);
        CaptionExpert.AssignData('SVR', FloatToStrF(SVR, ffFixed, 12, paraDigits));
        if SVR = 0.5   then
          CaptionExpert.AssignData('Rassumed', 'True')
        else
          CaptionExpert.AssignData('Rassumed', 'False');
      end
      else
      begin
        AminoAcids := TStringList.Create;
        AminoAcids.CommaText := 'A, R, N, D, C, Q, E, G, H, I, L, K, M, F, P, S, T, W, Y, V';

        //Assign all the frequencies calculated for each amino acid
        for i:=0 to AminoAcids.Count -1 do
          CaptionExpert.AssignData('Freq' + AnsiUpperCase(AminoAcids.Strings[i]), FloatToStrF(Freq[i]*100, ffFixed, 12, paraDigits)+'%');

        //Assign the results for the 20x20 matrix (how likely one amino acid is to change into another)
        TableHTMLString := '<TABLE><TR><TH>&nbsp;</TH><TH>A</TH><TH>R</TH><TH>N</TH><TH>D</TH><TH>C</TH><TH>Q</TH><TH>E</TH><TH>G</TH><TH>H</TH><TH>I</TH><TH>L</TH><TH>K</TH><TH>M</TH><TH>F</TH><TH>P</TH><TH>S</TH><TH>T</TH><TH>W</TH><TH>Y</TH><TH>V</TH></TH>';
        for i:= 0 to AminoAcids.Count -1 do
        begin
          if i = AminoAcids.Count-1 then TableHTMLString := TableHTMLString + '<TR class="lastRow">'
                                    else TableHTMLString := TableHTMLString + '<TR>';

          for j := -1 to AminoAcids.Count-1  do
          begin
            //CaptionExpert.AssignData('O' + AminoAcids.Strings[i] + AminoAcids.Strings[j], FloatToStrF(Matrix[i,j]*20, ffFixed, 12, 4));
            if      j = -1 then TableHTMLString := TableHTMLString + '<TD class="rowHeader">' + AnsiUpperCase(AminoAcids.Strings[i]) + '</TD>'
            else if j =  i then TableHTMLString := TableHTMLString + '<TD align="center" nowrap = "nowrap">-</TD>'
                         else TableHTMLString := TableHTMLString + '<TD align="center" nowrap = "nowrap">' + FloatToStrF(Matrix[i,j]*100, ffFixed, 12, paraDigits) + '</TD>';
          end;
          TableHTMLString := TableHTMLString + '</TR>';
        end;
        TableHTMLString := TableHTMLString + '</TABLE>';
        CaptionExpert.AssignData('MatrixTable', TableHTMLString);
      end;

      if MAI.MyTreePack.DoesContain(ttPattern) then
      begin
        CaptionExpert.AssignData('SubstitutionType', 'Matrix');
        CaptionExpert.AssignData('Title', 'Substitution Matrix');
      end
      else if MAI.MyTreePack.DoesContain(ttTsTvBias) then
      begin
        CaptionExpert.AssignData('SubstitutionType', 'TsTvBias');
        CaptionExpert.AssignData('Title', 'Transition/Transversion Bias');
      end
      else if MAI.MyTreePack.DoesContain(ttSiteRates) then
      begin
        CaptionExpert.AssignData('SubstitutionType', 'SiteRates');
        CaptionExpert.AssignData('Title', 'Evolutionary Rates at Sites');
      end
      else if MAI.MyTreePack.DoesContain(ttGamma) then
      begin
        CaptionExpert.AssignData('SubstitutionType', 'Gamma');
        CaptionExpert.AssignData('Title', 'Gamma Parameter for Site Rates');
      end;

      CaptionExpert.AssignData('DataType', DataType);
//      CaptionExpert.AssignData('TSTV', FloatToStrF(SVR, ffFixed, 12, 4));
      CaptionExpert.AssignData('ModelName', FullName);

      if MAI.MyDistPack.DoesContain(gdInvar) then
        CaptionExpert.AssignData('PropOfInvariant', Format('%.0f', [Invar*100]))
      else
        CaptionExpert.AssignData('PropOfInvariant', 'N/A');

      if MAI.MyDistPack.DoesContain(gdGamma) then
      begin
        CaptionExpert.AssignData('GammaPara', FormatDoubleSafe(Gamma, 4, 12));
        CaptionExpert.AssignData('NoOfGCats', IntToStr(NoOfRates));
        str := FloatToStrF(Rate[0], ffFixed, 12, paraDigits);
        for i := 1 to NoOfRates-1 do
          str := str +', ' +FloatToStrF(Rate[i], ffFixed, 12, paraDigits);
        CaptionExpert.AssignData('CatRates', str);
      end
      else
        CaptionExpert.AssignData('GammaPara', 'N/A');

      CaptionExpert.BindData(MAI, true);
      CaptionExpert.BindData(MAI.MyDistPack, true);

//      CaptionExpert.AssignData('k1', FloatToStrF(Matrix[0,3]/Matrix[1,3], ffFixed, 12, 4));
//      CaptionExpert.AssignData('k2', FloatToStrF(Matrix[1,2]/Matrix[0,2], ffFixed, 12, 4));
      MatrixExport := TPatternTestMatrixExport.Create;

      for i:=0 to 19 do //Send the Matrix's info to ExcelOutput so an excel file can be generated later
        MatrixExport.SetPatternDataRow(Matrix[i], i, DataType);
      Caption := TStringList.Create;
      CaptionExpert.GenerateLegendAsText(Caption);
      if MAI.MyUsrOperation = dtdoMLComputePattern then
        while (Pos('NOTE', Caption[0]) <= 0) do { we don't want the data, just the stuff after the table}
          Caption.Delete(0);

      MatrixExport.SetCaption(Caption);

      {$IFDEF VISUAL_BUILD}
      HTMLtoLoad := CaptionExpert.GenerateLegend;
      ChromiumBrowser := CreateNewChromiumBrowserWindow(bmResults);
      ChromiumBrowser.SetMatrixExport(MatrixExport);
      ChromiumBrowser.LoadHtmlFromString(HTMLToLoad);
      {$ELSE}
      MatrixExport.DoResultsExport(EXtext, NextAvailableFilenameNV('.csv'));
      CaptionStringList := TStringList.Create;
      CaptionStringList.Text := CaptionExpert.GenerateLegend;
      CaptionStringList.SaveToFile(NextAvailableFilenameNV('_caption.html'));
      D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
      D_MegaMain.AnalysisSummary.ModelInfoList := AModelInfo.ToStringList;
      D_MegaMain.AnalysisSummary.WriteToFile(NextAvailableFilenameNV('_summary.txt'));
      with MAI.MyOriTreeList do
      begin
        Clear;
        Add(TTreeData.Create(MAI.MyNoOfSeqs,true,false,false));
        MAI.MyMLAnalysisPack.GetTreeData(Items[0]);
        Information.Assign(MAI.ARP.AnalysisOptStrList);
        IsRooted    := False;
        for i := 0 to MAI.MyOtuNames.Count-1 do
          OTUName[i] := MAI.MyOtuNames[i];
        MAI.MyTreePack.SearchLevel := Round(MAI.MyTreePack.SearchLevel/2);
        MAI.MyOriTreeList.ValueName := 'LogL';
        ValueName := 'LogL';
        ExportToNewickFile(NextAvailableFilenameNV('.nwk'), True, False, 0.0);
      end;
      {$ENDIF}
    end;
   finally
     if Assigned(Nucleotides) then
       Nucleotides.Free;
     if Assigned(AminoAcids) then
       AminoAcids.Free;
    if Assigned(CaptionExpert) then
      CaptionExpert.Free;
    if Assigned(AModelInfo) then
      AModelInfo.Free;
    if Assigned(CaptionStringList) then
      CaptionStringList.Free;
    if Assigned(Caption) then
      Caption.Free;
    FreeAndNil(MAI);
  end;
end;

procedure TTreeDisplaySetup.ShowSiteRateResults(aMAI: TAnalysisInfo);
begin
  try
    MAI := aMAI;
    Initialize;
    UpdateTreeListML;
    ExportSiteRateExecute(MAI);
    {$IFNDEF VISUAL_BUILD}
    MAI.MyOriTreeList.ExportToNewickFile(NextAvailableFilenameNV('.nwk'), True, False, 0.0);
    {$ENDIF}
  finally
    FreeAndNil(MAI.ARP);
    FreeAndNil(MAI);
  end;
end;

procedure TTreeDisplaySetup.ShowGeneDupsTree(AnalysisInfo: TAnalysisInfo);
var
  NumDups: Integer;
  NumSpeciations: Integer;
begin
{$IFDEF VISUAL_BUILD}
  MAI := AnalysisInfo;
  try
    Initialize;
    NumDups := MAI.MyOriTreeList[0].NumGeneDups;  { getting this here because the search thread cannot have a reference to MAI because of a circular dependency issue}
    NumSpeciations := MAI.MyOriTreeList[0].NumSpeciations;
    MAI.NumGeneDups := NumDups;
    MAI.NumSpeciations := NumSpeciations;
    MAI.MyOriTreeList.Information.Assign(MAI.ARP.AnalysisOptStrList);
    InitTreeViewForm;
    TreeExpl.SetData(MAI);
    TreeExpl.Tree.ShowGeneDupMarkers := True;
    if NumDups > 0 then
      TreeExpl.Tree.HasGeneDups := True;
    if NumSpeciations > 0 then
    begin
      TreeExpl.Tree.HasSpeciations := True;
      TreeExpl.Tree.ShowSpeciationMarkers := True;
    end;
    TreeExpl.Tree.LinearizeFunc := nil;
    if MAI.GeneDupsRootIndex >= 0 then
      TreeExpl.RootOnNode(MAI.GeneDupsRootIndex); { because TreeExpl roots on midpoint by default}
    TreeExpl.Tree.Invalidate;
    RelinquishControl;
  except
    On E: Exception do
      ShowMessage('Oh no! An error occurred when displaying the tree: ' + E.Message);
  end;
  {$ENDIF}
end;


procedure TTreeDisplaySetup.ExportSiteRateExecute(aMAI: TAnalysisInfo);
var
  Note: TStringList = nil;
  csvList: TStringList = nil;
  CaptionExpert: TLegendGenerator = nil;
  node, site, i, DataOffsetX, DataOffsetY,imax:integer;
  MaxProbability: double;
  SaveLocation: String = '';
  MatrixExport : TMatrixExport = nil;  //Does the formating, exporting and opening of the file
  ExportType : TExportType;
  ExcelComponent : TExcelWrite = nil;
  ProbColor, ChangedColor : TColor;
  aRect : TRect;
  Prob: array [0..20] of extended;
  r,s: double;
  AModelInfo : TModelInfo = nil;
  IncludedSites: TIncludedSitesArray;
begin
  MAI := aMAI;
  for i := Low(Prob) to High(Prob) do
    Prob[i] := -1;
  {$IFDEF VISUAL_BUILD}
  MAI.ARP.Hide;
  if ((MAI.NoOfSites > 32760) or (MAI.MyOriTreeList[0].NoOfOTUs > 250)) {$IFDEF MSWINDOWS}and (not IsExcel2010OrGreater){$ENDIF} then  //The maximum number of characters Excel can support in a cell is 32767 but we allow for our use of cells at the top;  Max # of columns excel may have is 256 so we allow for use of a few columns for our own purposes
  begin
    //Note:  Disabled options will be re-allowed when the prompt box is closed so it won't affect the next use.
    WriteOutputDlg.Disallow(EXexcelDisp);
    WriteOutputDlg.Disallow(EXexcelSave);
  end;
  ExportType := PromptUserWriteOutput(SaveLocation);
  MAI.ARP.Show;
  MAI.ARP.UpdateRunStatusInfo('Status', 'Computing Rates at Each Site...');
  MAI.ARP.Progress := 0;
  {$ELSE}
  ExportType := OutputFileTypeToExportType(D_MegaMain.OutputFormat);
  {$ENDIF}
  if ExportType = EXnone then
    Exit;

  try
    ChangedColor := RGB(33, 255, 33);  //Color of borders for cells different than infered parents
    IncludedSites := D_InputSeqData.IncludedSites;
    ExcelComponent := TExcelWrite.Create(nil, 'Data');//First tab (worksheet) will contain the sequences vertically and have more information about their likelihood, etc.
    ExcelComponent.IsXLS := True;
    ExcelComponent.Add('Most Probable');
    ExcelComponent.WriteLine();
    //Put a box around the "Different than Inferred Parent" to show how it will look like in the data
    aRect := ExcelComponent.LastCellWriteXY();
    //the cell is large so we can merge it with the adjacent ones to not mess up layout
    aRect.Right := aRect.Right + 5;
    ExcelComponent.MergeCells(aRect);
    ExcelComponent.ColorCells(aRect, ChangedColor);

    ExcelComponent.AddBlankCell;
    ExcelComponent.AddBlankCell;
    if MAI.MyMLAnalysisPack.Model.UseInvar then
      ExcelCOmponent.AddBlankCell;
    ExcelComponent.Add('Gamma Categories');
    ExcelComponent.WriteLine();
    aRect := ExcelComponent.LastCellWriteXY();
    aRect.Left := 2;
    if MAI.MyMLAnalysisPack.Model.UseInvar then
      aRect.Left := 3;
    aRect.Right := aRect.Left + MAI.MyMLAnalysisPack.Model.NoOfRates-1;
    ExcelComponent.MergeCells(aRect);
    ExcelComponent.AlignCells(aRect, aCenter);

    ExcelComponent.AddBlankCell;
    ExcelComponent.AddBlankCell;
    if MAI.MyMLAnalysisPack.Model.UseInvar then
      ExcelCOmponent.AddBlankCell;
    for i := 1 to MAI.MyMLAnalysisPack.Model.NoOfRates do
      ExcelComponent.Add('#' + IntToStr(i));
    ExcelComponent.WriteLine();
    ExcelComponent.Add('Site No.');
    ExcelComponent.Add('Rel. Rate');
    if MAI.MyMLAnalysisPack.Model.UseInvar then
      ExcelComponent.Add('Invar');
    for i := 0 to MAI.MyMLAnalysisPack.Model.NoOfRates-1 do
      ExcelComponent.Add(MAI.MyMLAnalysisPack.Model.Rate[i]);
    ExcelComponent.WriteLine();

    s := 0;
    for site:=1 to MAI.NoOfSites do
    begin
      MAI.MyMLAnalysisPack.GetProbOfRateCategoryAtSite(site, Prob);
      r := 0;
      for i := 0 to MAI.MyMLAnalysisPack.Model.NoOfRates-1 do
        r := r +Prob[i]*MAI.MyMLAnalysisPack.Model.Rate[i]/(1 -MAI.MyMLAnalysisPack.Model.Invar);
      s := s + r;
    end;
    s := s/MAI.MyMLAnalysisPack.NoOfSites;

    for site:=1 to MAI.NoOfSites do
    begin
      MAI.ARP.Progress := Math.Ceil(100* ((site * 1.0) / (MAI.NoOfSites-1)));

      ExcelComponent.Add(IncludedSites[site - 1]);
      MAI.MyMLAnalysisPack.GetProbOfRateCategoryAtSite(site, Prob);

      imax := 0;
      MaxProbability := Prob[0];
      for i := 1 to MAI.MyMLAnalysisPack.Model.NoOfRates-1 do
      if Prob[i] > MaxProbability then
      begin
        imax := i;
        MaxProbability := Prob[i];
      end;

      node := MAI.NoOfSeqs;
      ProbColor := RGB(255, 255, 255);
      DataOffsetX := (ExcelComponent.LastCellWriteXY().Left - node);
      DataOffsetY := (ExcelComponent.LastCellWriteXY().Top - site);

      r := 0;
      for i := 0 to MAI.MyMLAnalysisPack.Model.NoOfRates-1 do
        r := r +Prob[i]*MAI.MyMLAnalysisPack.Model.Rate[i]/(1 -MAI.MyMLAnalysisPack.Model.Invar);
      if CompareValue(s, 0.0, FP_CUTOFF) = 0 then
        raise EDivByZero.Create('division by zero in ExportSiteRateExecute');
      r := r/s;

      ExcelComponent.Add(r, ProbColor);

      inc(node);

      if MAI.MyMLAnalysisPack.Model.UseInvar then
      begin
        ProbColor := RGB(255, 255, 255);

        DataOffsetX := (ExcelComponent.LastCellWriteXY().Left - node);
        DataOffsetY := (ExcelComponent.LastCellWriteXY().Top - site);
        ExcelComponent.Add(Prob[MAI.MyMLAnalysisPack.Model.NoOfRates], ProbColor);

        if imax = MAI.MyMLAnalysisPack.Model.NoOfRates then
        begin
          aRect.Top    := ExcelComponent.LastCellWriteXY().Top ;
          aRect.Bottom := aRect.Top;
          aRect.Left   := node + DataOffsetX;
          aRect.Right  := aRect.Left;
          ExcelComponent.ColorCells(aRect, ChangedColor);
        end;
        inc(node);
      end;

      for i := 0 to MAI.MyMLAnalysisPack.Model.NoOfRates-1 do
      begin
        DataOffsetX := (ExcelComponent.LastCellWriteXY().Left - node);
        DataOffsetY := (ExcelComponent.LastCellWriteXY().Top - site);
        ExcelComponent.Add(Prob[i]);

        if i = imax then
        begin
          aRect.Top    := ExcelComponent.LastCellWriteXY().Top ;
          aRect.Bottom := aRect.Top;
          aRect.Left   := node + DataOffsetX;
          aRect.Right  := aRect.Left;
          ExcelComponent.ColorCells(aRect, ChangedColor);
        end;
        inc(node);
      end;

      ExcelComponent.WriteLine();
    end;
    //Draw the black line for the first tab to seperate site # and data
    aRect.Top := DataOffsetY+1;
    aRect.Bottom := DataOffsetY + MAI.NoOfSites-1;
    aRect.Left := 0;
    aRect.Right := aRect.Left;
    ExcelComponent.ColorCells(aRect, RGB(0,0,0), xlBorderRight);

    ExcelComponent.AddWorksheet('Caption');
    CaptionExpert :=  TLegendGenerator.Create;
    Note := TStringList.Create;

    AModelInfo := TModelInfo.Create;
    MAI.MyMLAnalysisPack.GetModelInfo(AModelInfo);

    CaptionExpert.LoadTemplateFromFile('Estimate_position_by_position_rates.htm');
    CaptionExpert.AssignData('Title', 'Position-by-Position rates');
    CaptionExpert.AssignData('LogL', FloatToStrF(AModelInfo.LogL, ffFixed, 12, 3));
    CaptionExpert.AssignData('NoOfSeqs', IntToStr(MAI.MyMLAnalysisPack.NoOfSeqs));
    CaptionExpert.AssignData('NoOfSites', IntToStr(MAI.MyMLAnalysisPack.NoOfSites));
    if MAI.MyMLAnalysisPack.IsInitialTree then
      CaptionExpert.AssignData('TreeUsedString','For estimating ML values, a user-specified topology was used.')
    else
      CaptionExpert.AssignData('TreeUsedString','For estimating ML values, a tree topology was automatically computed.');

    CaptionExpert.AssignData('ModelName', AModelInfo.FullName);

  if MAI.MyMLAnalysisPack.Model.UseInvar then
      CaptionExpert.AssignData('PropOfInvariant', FloatToStrF(AModelInfo.Invar*100, ffFixed, 12, 4))
    else
      CaptionExpert.AssignData('PropOfInvariant', 'N/A');

  if MAI.MyMLAnalysisPack.NoOfRates > 1 then
    begin
      CaptionExpert.AssignData('GammaPara', FloatToStrF(AModelInfo.Gamma, ffFixed, 12, 4));
      CaptionExpert.AssignData('NoOfGCats', IntToStr(AModelInfo.NoOfRates));
    end
    else
      CaptionExpert.AssignData('GammaPara', 'N/A');

    CaptionExpert.BindData(MAI, true);
    CaptionExpert.BindData(MAI.MyDistPack, true);
    CaptionExpert.GenerateLegendAsText(note);
    {$IFDEF VISUAL_BUILD}
    ExcelComponent.AddCaptionAsWorksheet(Note, 1);
    MAI.AnalysisSummary.ModelInfoList := AModelInfo.ToStringList;
    MAI.AnalysisSummary.AddAnalysisInfo(MAI);
    MAI.AnalysisSummary.AddCaptionStrings(Note);
    {$ELSE}
    Note.Text := CaptionExpert.GenerateLegend;
    D_MegaMain.AnalysisSummary.ModelInfoList := AModelInfo.ToStringList;
    D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
    SaveLocation := NextAvailableFilenameNV('.csv');
    D_MegaMain.AnalysisSummary.WriteToFile(ChangeFileExt(SaveLocation, '_summary.txt'));
    Note.SaveToFile(ChangeFileExt(SaveLocation, '_caption.html'));
    {$ENDIF}
    case ExportType of
      EXtext, EXtextSave:
        begin
          csvList := ExcelComponent.GetCsvList;
          {$IFDEF VISUAL_BUILD}
          if ExportType = EXtext then
            OpenStringList(csvList, 'Most Probable Sequences', true)
          else
            csvList.SaveToFile(SaveLocation);
          {$ELSE}
          csvList.SaveToFile(SaveLocation);
          {$ENDIF}
        end;
      EXcsvDisp, EXcsvSave: ExcelComponent.SaveFile(SaveLocation, ExportCSV);
      EXexcelDisp, EXexcelSave: ExcelComponent.SaveFile(SaveLocation, ExportExcel);
      EXexcelXmlDisp, EXexcelXmlSave: ExcelComponent.SaveFile(SaveLocation, ExportExelXML);
      EXodsDisp, EXodsSave: ExcelComponent.SaveFile(SaveLocation, ExportODS);
    end;

    case ExportType of
      EXcsvDisp, EXexcelDisp, EXexcelXmlDisp, EXodsDisp: RunAProgram(SaveLocation);
    end;
    finally
      if Assigned(MatrixExport) then
        MatrixExport.Free;
      if Assigned(ExcelComponent) then
        ExcelComponent.Free;
      if Assigned(AModelInfo) then
        AModelInfo.Free;
      if Assigned(Note) then
        Note.Free;
      if Assigned(CaptionExpert) then
        CaptionExpert.Free;
      if Assigned(csvList) then
        csvList.Free;
    end;
end;

end.

