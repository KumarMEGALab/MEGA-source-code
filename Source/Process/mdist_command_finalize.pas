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

unit mdist_command_finalize;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MAnalysisInfo, MD_InputDistData, MOtuInfo, MDistPack,
  MProcessPack, MDisplayMatrixDlg, MRuntimeProgressDlg, MLegendGenerator,
  MSeqDistBase, MAminoDist, MNucDist, MSynNonsynDist, MegaConsts,
  mdist_command_results_wrapper, mdist_command_threads;

type

  { TDistCommandFinalize }

  TDistCommandFinalize = class(TObject)
    private
      FIsDisparityTest: Boolean;
      FLog: TStringList;
      procedure SetIsDisparityTest(AValue: Boolean);
    protected
      FIsWorkflowCalculation: Boolean;
      UsrOperation: TDistTreeDlgOption;
      ARP: TRuntimeProgress;
      ProcessPack: TProcessPack;
      FIsWorkflowOperation: Boolean;
      MyD, MyV : PDistanceMatrix;
      MyMeanGpD, MyMeanGpV: ArrayOfDouble; // within gp means
      MyMeanD, MyMeanV: Double; // overall means
      DistComputer:  TSeqDistBase;
      MAI: TAnalysisInfo;
      DispDlg: TD_DisplayMatrix;
      DispDlgVS: TVS_DisplayMatrix;
      {$IFDEF VISUAL_BUILD}
      DispDlgV: TDisplayMatrixDlg;
      {$ENDIF}
      emptySet: TMatrixExportOptionsSet;
      ExportOptions : TDistExportOptions;
      function GetDefaultExportOptions: TDistExportOptions;
      procedure InitDisplayMatrixDlg;
      procedure InitFigureLegend;
      procedure UpdateTaxaAndGroupCountsInMatrixDlg;
      procedure TransferMatricesToMatrixDlg;
      procedure UpdateMatrixDlgCaption;
      procedure BindFigureLegendData;
      procedure InitExportOptions;
      procedure PrepareDistCalculationResults;
      function GenerateResultsWrapper: TDistCommandResultsWrapper;
    public
      IsSelTest: Boolean;
      constructor Create(analysisInfo: TAnalysisInfo; aOperation: TDistTreeDlgOption; aDistComputer: TSeqDistBase; aData: TDistCommandResultsData);
      constructor CreateFromThread(aThread: TDistCommandThread);
      destructor Destroy; override;
      procedure ShowDistCalculationResults(aMAI: TAnalysisInfo);
      function Execute: Boolean;

      property IsDisparityTest: Boolean read FIsDisparityTest write SetIsDisparityTest;
      property Log: TStringList read FLog;
  end;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  Forms, mega_main, MegaVerConsts,
  {$ELSE}
  MD_MegaMain, MegaUtils_NV,
  {$ENDIF}
  LCLIntf, LCLType, MegaUtils;

{ TDistCommandFinalize }

procedure TDistCommandFinalize.SetIsDisparityTest(AValue: Boolean);
begin
  if FIsDisparityTest = AValue then Exit;
  FIsDisparityTest := AValue;
end;

function TDistCommandFinalize.GetDefaultExportOptions: TDistExportOptions;
begin
  Result := TDistExportOptions.Create;
  Result.Precision := 8;
  Result.OutputFormat := ExportMega;
  Result.WithStdErr := not DispDlg.OnlyDistance;
  Result.LowerLeft := DispDlg.IsLowerLeft;
  Result.OppositeSides := not DispDlg.isDataLowerLeft;
  Result.FTitle := DispDlg.Title;
  Result.FDAcronym := DispDlg.DAcronym;
  Result.FVAcronym := DispDlg.VAcronym;
  Result.NoOfRows := DispDlg.NoOfRows;
  Result.AllowNegative := DispDlg.AllowNeg;
  {$IFDEF VISUAL_BUILD}
  Result.SaveExcelFileTo := SKIP_SAVE_FILE;
  {$ELSE}
  Result.SaveExcelFileTo := NextAvailableFilenameNV(GetFileOutputExtension);
  {$ENDIF}
  Result.AllowStandardError := True;
end;

procedure TDistCommandFinalize.InitDisplayMatrixDlg;
begin
  with MAI do
  begin
    DispDlg := TD_DisplayMatrix.Create;
    DispDlgVS := TVS_DisplayMatrix.Create;
    {$IFDEF VISUAL_BUILD}
    DispDlgV := TDisplayMatrixDlg.Create(Application);
    DispDlgV.LinkDataClass(DispDlg);
    DispDlgV.LinkVisualStatusClass(DispDlgVS);
    if Assigned(MAI.MyProcessPack) and MAI.MyProcessPack.IsWorkflowPack and Assigned(MAI.MyProcessPack.WorkflowElement) then
      MAI.MyProcessPack.WorkflowElement.AddResultForm(DispDlgV);
    {$ENDIF}
    DispDlg.LinkVisualStatusClass(DispDlgVS);

    if IsDisparityTest then
    begin
      if MAI.MyDistPack.VarType <> gdNone then
      begin
        DispDlg.DAcronym  := 'Prob';
        DispDlg.VAcronym  := 'ID';
        {$IFDEF VISUAL_BUILD}
        ARP.AddAnalysisOptions('Prob (black)', 'Probability computed (must be <0.05 for hypothesis rejection at 5% level [yellow background])');
        ARP.AddAnalysisOptions('Stat (blue)',  'Disparity Index.');
        {$ELSE}
        ARP.AddAnalysisOptions('Prob (lower left values)', 'Probability computed (must be <0.05 for hypothesis rejection at 5% level)');
        ARP.AddAnalysisOptions('Stat (upper right values)',  'Disparity Index.');
        {$ENDIF}
      end
      else
      begin
        DispDlg.DAcronym  := MyDistPack.Acronym;
        ARP.AddAnalysisOptions(MyDistPack.Acronym,'Estimate');
      end;
    end
    else if IsSelTest then
    begin
      DispDlg.IsSelTest := True;
      DispDlg.DAcronym  := 'Prob';
      DispDlg.VAcronym  := 'Stat';
      {$IFDEF VISUAL_BUILD}
      ARP.AddAnalysisOptions('Prob (black)', 'Probability computed (must be <0.05 for hypothesis rejection at 5% level [yellow backgroud])');
      if MAI.MyDistPack.VarType <> gdNone then
        ARP.AddAnalysisOptions('Stat (blue)',   'Statistic used to compute the probability (blue).');
      {$ELSE}
      ARP.AddAnalysisOptions('Prob (below diagonal)', 'Probability computed (must be <0.05 for hypothesis rejection at 5% level)');
      if MAI.MyDistPack.VarType <> gdNone then
        ARP.AddAnalysisOptions('Stat (above diagonal)',   'Statistic used to compute the probability (in square brackets).');
      {$ENDIF}
    end
    else
    begin
      DispDlg.DAcronym  := MyDistPack.Acronym;
      ARP.AddAnalysisOptions(MyDistPack.Acronym,'Estimate');
      if MyDistPack.VarType <> gdNone then
      begin
        DispDlg.OnlyDistance := False;
        DispDlg.VAcronym  := 'S.E.';
        ARP.AddAnalysisOptions('S.E','Standard error');
      end
      else
        DispDlg.OnlyDistance := True;
    end;
    {$IFDEF VISUAL_BUILD}
    DispDlg.Title        := MegaForm.DataTitle;
    {$ELSE}
    DispDlg.Title        := D_MegaMain.DataTitle;
    {$ENDIF}
    //if Assigned(ProcessPack) and (not ProcessPack.IsWorkflowPack) then
    DispDlg.Description  := ARP.AnalysisOptStrList;
    DispDlg.Fullname         := MyDistPack.Name;
    DispDlg.ComputationType  := MyDistPack.ComputationType;
    DispDlg.AllowNeg      := (IsSelTest or MyDistPack.HasDifference) and (not IsDisparityTest);
    DispDlg.HasTestResult := IsSelTest or (IsDisparityTest and (MAI.MyDistPack.VarType <> gdNone)); // or (Adistpack.DoesContain(gdTestRandom) and (MyV <> nil));
  end;
  {$IFDEF VISUAL_BUILD}
  ARP.UpdateRunStatusInfo('Status', 'Preparing display of results');
  {$ELSE}
  ARP.UpdateRunStatusInfo('Status', 'Preparing output');
  {$ENDIF}
end;

procedure TDistCommandFinalize.InitFigureLegend;
begin
  if IsDisparityTest then
    DispDlg.FigureGenerator.LoadTemplateFromFile('Homogeneity_Test.htm')
  else if IsSelTest then
  begin
    if MAI.MyDistPack.DoesContain(gdExactTest) then
    begin
      DispDlg.FigureGenerator.LoadTemplateFromFile('Fishers_Exact_Test.htm');
    end
    else if MAI.MyDistPack.DoesContain(gdZSelectionTest) then
      DispDlg.FigureGenerator.LoadTemplateFromFile('Codon_based_Z_test.htm')
    else
      DispDlg.FigureGenerator.LoadTemplateFromFile('Distance_Matrix.htm');
  end
  else
  begin
    DispDlg.FigureGenerator.LoadTemplateFromFile('Distance_Matrix.htm');
  end;
end;

procedure TDistCommandFinalize.UpdateTaxaAndGroupCountsInMatrixDlg;
var
  i,j: Integer;
begin
  with MAI do
  begin
      case UsrOperation of
      dtdoPairwiseDist, dtdoDisparityIndexTest, dtdoCompositionDistance, dtdoDisparityIndex:
        begin
          DispDlg.NoOfTaxa := MyNoOfSeqs;
          for i:=0 to MyNoOfSeqs-1 do
            DispDlg.TaxonName[i] := TOtuInfo(MyUsedOtuInfos[i]).Name;
          if MyNoOfGps > 0 then
          begin
            DispDlg.NoOfGps := MyNoOfGps;
            for i:=0 to MyNoOfSeqs-1 do
              DispDlg.GpName[i] := TOtuInfo(MyUsedOtuInfos[i]).GpName;
          end;
        end;
      dtdoBetweenGroupMean,
      dtdoNetGroupMean,
      dtdoWithinGroupMean:
        begin
          DispDlg.NoOfGps := MyNoOfGps;
          for i:=0 to MyNoOfGps-1 do
            for j:=0 to MyNoOfSeqs-1 do
              if MyGpIds[j]=i then
              begin
                DispDlg.GpName[i] := TOtuInfo(MyUsedOtuInfos[j]).GpName;
                break;
              end;
        end;
      dtdoOverallMean:
        begin
          DispDlg.NoOfGps  := 1;
          DispDlg.GpName[0] := 'Overall';
        end;
      dtdoAvgDiversityForEntirePop,
      dtdoAvgDiversityWithinSubPops,
      dtdoInterPopDiversity,
      dtdoPropOfInterPopDiversity:
        begin
          DispDlg.ComputationType  := gdOverallMean; // it is just that
          DispDlg.NoOfGps  := 1;
          DispDlg.GpName[0] := 'Diversity';
          {$IFDEF VISUAL_BUILD}
          DispDlgV.SetExportOptions([meExcel, meCsv, meText]);
          {$ENDIF}
        end;
      end;
  end;
end;

procedure TDistCommandFinalize.TransferMatricesToMatrixDlg;
begin
  with MAI do
  begin
      case UsrOperation of
      dtdoPairwiseDist, dtdoDisparityIndexTest, dtdoCompositionDistance, dtdoDisparityIndex:
        begin
          DispDlg.DistMat := MyD;
          MyD := nil;
          if (MyDistPack.VarType <> gdNone) then
          begin
            // let's just see the variance
            if (not IsSelTest) and (not IsDisparityTest) then // and (not ADistPack.DoesContain(gdTestRandom)) then
              ConvertVarToStdErrMat(MyV, MyNoOfSeqs, False);
            DispDlg.StdErrMat := MyV;
            MyV := nil;
          end;
        end;
      dtdoBetweenGroupMean,
      dtdoNetGroupMean:
        begin
          DispDlg.DistMat := MyD;
          MyD := nil;
          if MyDistPack.VarType <> gdNone then
          begin
            if not IsSelTest then
              ConvertVarToStdErrMat(MyV, MyNoOfGps, False);
            DispDlg.StdErrMat := MyV;
            MyV := nil;
          end;
        end;
      dtdoWithinGroupMean:
        begin
          DispDlg.DistArray := MyMeanGpD;
          MyMeanGpD := nil;
          if MyDistPack.VarType <> gdNone then
          begin
            if not IsSelTest then
              ConvertVarToStdErrArray(MyMeanGpV, MyNoOfGps);
            DispDlg.StdErrArray := MyMeanGpV;
            MyMeanGpV := nil;
          end;
        end;
      dtdoOverallMean,
      dtdoAvgDiversityForEntirePop,
      dtdoAvgDiversityWithinSubPops,
      dtdoInterPopDiversity,
      dtdoPropOfInterPopDiversity:
        begin
          MyMeanGpD[0] := MyMeanD;
          DispDlg.DistArray := MyMeanGpD;
          MyMeanGpD := nil;
          if MyDistPack.VarType <> gdNone then
          begin
            MyMeanGpV[0] := MyMeanV;
            if not IsSelTest then
              ConvertVarToStdErrArray(MyMeanGpV, 1);
            DispDlg.StdErrArray := MyMeanGpV;
            MyMeanGpV := nil;
          end;
        end;
      end;
  end;
end;

procedure TDistCommandFinalize.UpdateMatrixDlgCaption;
begin
  with MAI do
  begin
    {$IFDEF VISUAL_BUILD}
    if IsDisparityTest then
      DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': ' + MAI.KumarGadagkar2001InfoCaption
    else if IsSelTest then
    begin
      if MyDistPack.DoesContain(gdExactTest) then
        DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Test Selection: Fisher''s Exact Test'
      else if MyDistPack.DoesContain(gdZSelectionTest) then
      begin
        if MyDistPack.DoesContain(gdNeutralEvolTest) then
          DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Z-Test of Neutral Evolution'
        else if MyDistPack.DoesContain(gdPosSelTest) then
          DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Z-Test of Positive Selection'
        else if MyDistPack.DoesContain(gdPurifySelTest) then
          DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Z-Test of Purifying Selection';
        //    gdMcdonaldKreitmanTest,
      end;
    end
    else
      case UsrOperation of
        dtdoPairwiseDist    : DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Pairwise Distances';
        dtdoBetweenGroupMean: DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Between Group Mean Distance';
        dtdoNetGroupMean    : DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Net Between Group Mean Distances';
        dtdoWithinGroupMean : DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Within Mean Group Distance';
        dtdoOverallMean     : DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Overall mean distance';
        dtdoAvgDiversityForEntirePop:  DispDlgV.Caption :=  VER_MEGA_WIN_CAPTION_PREFIX+ ': Mean Diversity in Entire Population';
        dtdoAvgDiversityWithinSubPops: DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Mean Diversity Within Subpopulations';
        dtdoInterPopDiversity: DispDlgV.Caption :=  VER_MEGA_WIN_CAPTION_PREFIX+ ': Mean Interpopulational Diversity';
        dtdoPropOfInterPopDiversity: DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Coefficient of Differentiation';
      end;
  {$ENDIF}
  end;
end;

procedure TDistCommandFinalize.BindFigureLegendData;
begin
  with DispDlg do
  begin
    FigureGenerator.BindData(MAI.MyDistPack, True);
    if not IsDisparityTest then
    begin
      FigureGenerator.BindData(DispDlg, True);
      {$IFDEF VISUAL_BUILD}
      FigureGenerator.BindData(DispDlgV, True);
      {$ENDIF}
    end;
    FigureGenerator.BindData(MAI, True);
  end;
end;

procedure TDistCommandFinalize.InitExportOptions;
begin
  {$IFDEF VISUAL_BUILD}
  // These operations only have 1 number, thus there's no reason to export them (and it causes issues).
  case UsrOperation of
    dtdoOverallMean, dtdoAvgDiversityWithinSubPops, dtdoAvgDiversityForEntirePop, dtdoInterPopDiversity, dtdoPropOfInterPopDiversity:
    begin
      DispDlgV.SetExportOptions(emptySet);
    end;
  end;
  DispDlgV.Show;
  DispDlgV.Caption := DispDlgV.Caption +' (' + ExtractFilename(MAI.DataFileName)+')';
  MAI.AnalysisSummary.AddAnalysisInfo(MAI);
  DispDlgV.AnalysisSummary := MAI.AnalysisSummary.ToStringList;
  {$ELSE}
  D_MegaMain.AnalysisSummary.AddAnalysisInfo(MAI);
  ExportOptions := GetDefaultExportOptions;
  DispDlg.WriteExportToFile(ExportOptions);
  D_MegaMain.AnalysisSummary.WriteToFile(ChangeFileExt(ExportOptions.SaveExcelFileTo, '_summary.txt'));
  {$ENDIF}
end;

procedure TDistCommandFinalize.PrepareDistCalculationResults;
begin
  InitDisplayMatrixDlg;
  InitFigureLegend;
  UpdateTaxaAndGroupCountsInMatrixDlg;
  TransferMatricesToMatrixDlg;
  UpdateMatrixDlgCaption;
  DispDlg.Initialize;
  BindFigureLegendData;
  InitExportOptions;
  if Assigned(MAI) and Assigned(MAI.ARP) then
    MAI.ARP.Hide;
end;

function TDistCommandFinalize.GenerateResultsWrapper: TDistCommandResultsWrapper;
begin
  Result := TDistCommandResultsWrapper.Create(DispDlg, DispDlgVS{$IFDEF VISUAL_BUILD}, DispDlgV{$ENDIF});
  Result.UsrOperation := UsrOperation;
  ExportOptions := GetDefaultExportOptions;
  Result.MatrixExportStrings := DispDlg.WriteExportToFile(ExportOptions);
end;

constructor TDistCommandFinalize.Create(analysisInfo: TAnalysisInfo; aOperation: TDistTreeDlgOption; aDistComputer: TSeqDistBase; aData: TDistCommandResultsData);
begin
  MAI := analysisInfo;
  IsSelTest := False;
  ProcessPack := MAI.MyProcessPack;
  FIsWorkflowCalculation := (Assigned(ProcessPack) and ProcessPack.IsWorkflowPack);
  ARP := MAI.ARP;
  UsrOperation := aOperation;
  DistComputer := aDistComputer;
  FLog := TStringList.Create;
  MyD := aData.MyD;
  MyV := aData.MyV;
  MyMeanD := aData.MyMeanD;
  MyMeanV := aData.MyMeanV;
  MyMeanGpD := aData.MyMeanGpD;
  MyMeanGpV := aData.MyMeanGpV;
  aData.Free;
end;

constructor TDistCommandFinalize.CreateFromThread(aThread: TDistCommandThread);
var
  aData: TDistCommandResultsData = nil;
begin
  MAI := aThread.MAI;
  IsSelTest := aThread.IsSelTest;
  IsDisparityTest := aThread.IsDisparityTest;
  ProcessPack := MAI.MyProcessPack;
  FIsWorkflowCalculation := (Assigned(ProcessPack) and ProcessPack.IsWorkflowPack);
  ARP := MAI.ARP;
  UsrOperation := aThread.UsrOperation;
  DistComputer := aThread.DistComputer;
  FLog := TStringList.Create;
  aData := aThread.GetResultsData;
  MyD := aData.MyD;
  MyV := aData.MyV;
  MyMeanD := aData.MyMeanD;
  MyMeanV := aData.MyMeanV;
  MyMeanGpD := aData.MyMeanGpD;
  MyMeanGpV := aData.MyMeanGpV;
  aThread.DistComputer := nil;
  aData.Free;
end;

destructor TDistCommandFinalize.Destroy;
begin
  if not FIsWorkflowCalculation then
  begin
    SetLength(MyMeanGpD, 0);
    SetLength(MyMeanGpV, 0);
    case UsrOperation of
      dtdoPairwiseDist, dtdoDisparityIndexTest, dtdoCompositionDistance, dtdoDisparityIndex:
        begin
           if MyD <> nil then  FreeDistMatrix(myD, MAI.MyNoOfSeqs);
           if MyV <> nil then  FreeDistMatrix(myV, MAI.MyNoOfSeqs);
        end;
      dtdoBetweenGroupMean,
      dtdoNetGroupMean:
        begin
          if MyD <> nil then  FreeDistMatrix(myD, MAI.MyNoOfGps);
          if MyV <> nil then  FreeDistMatrix(myV, MAI.MyNoOfGps);
        end;
    end;
  end;

  if DistComputer <> nil then
  begin
    if      DistComputer is TNucDist       then FreeAndNil(TNucDist(DistComputer))
    else if DistComputer is TSynNonsynDist then FreeAndNil(TSynNonsynDist(DistComputer))
    else if DistComputer is TAminoDist     then FreeAndNil(TAminoDist(DistComputer));
  end;
  if not FIsWorkflowCalculation then
    FreeAndNil(MAI);
  inherited Destroy;
end;

procedure TDistCommandFinalize.ShowDistCalculationResults(aMAI: TAnalysisInfo);
var
  wrapper: TDistCommandResultsWrapper = nil;
  aSummary: TStringList = nil;
begin
  MAI := aMAI;
  ARP := MAI.ARP;
  ProcessPack := MAI.MyProcessPack;
  FIsWorkflowCalculation := False;
  if not Assigned(MAI.DistCommandResultsWrapper) then
    raise Exception.Create('missing required TDistCommandResultsWrapper');
  if not (MAI.DistCommandResultsWrapper is TDistCommandResultsWrapper) then
    raise Exception.Create('invalid type cast for TDistCommandResultsWrapper');
  wrapper := TDistCommandResultsWrapper(MAI.DistCommandResultsWrapper);
  wrapper.OwnsResultsData := False;
  UsrOperation := wrapper.UsrOperation;
  {$IFDEF VISUAL_BUILD}
  aSummary := MAI.AnalysisSummary.ToStringList;
  wrapper.DispDlgV.AnalysisSummary := aSummary;
  wrapper.DispDlgV.Show;
  if Assigned(MAI.MyProcessPack.WorkflowElement) then
    MAI.MyProcessPack.WorkflowElement.AddResultForm(wrapper.DispDlgV);
  {$ELSE}
  raise Exception.Create('not implemented');
  {$ENDIF}
end;

function TDistCommandFinalize.Execute: Boolean;
begin
  try
    PrepareDistCalculationResults;
    Result := True;
    if Assigned(ProcessPack) and ProcessPack.IsWorkflowPack then
    begin
      MAI.DistCommandResultsWrapper := GenerateResultsWrapper;
      if Assigned(MAI.WorkflowElementFinishedCallBack) then
        MAI.WorkflowElementFinishedCallBack(MAI);
      {$IFDEF VISUAL_BUILD}
      if Assigned(ProcessPack) and Assigned(ProcessPack.WorkflowElement) then
        ProcessPack.WorkflowElement.AddResultForm(DispDlgV);
      {$ENDIF}
    end;
  except
    on E:Exception do
    begin
      Result := False;
      FLog.Add(E.Message);
    end;
  end;
end;

end.

