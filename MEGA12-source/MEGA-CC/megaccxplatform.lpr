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

program megaccxplatform;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, MD_MegaMain, MD_Sequences, MD_Align, StringUtils,
  MTreePack, MSubstitutionModelUtils, MSingleList, MProcessPack, MPartitionData,
  MOtuInfo, MLongintList, MLocusInfo, MGlobalSettings, MFileUtils,
  MegaVerConsts, MegaUtils_NV, MegaUtils, MegaPrivateFiles, MegaErrUtils,
  MegaConsts, MegaAnalysisPrefStrings, MDomainInfo, MDistPack, md5,
  MAnalysisInfo, Levenshtein, KeywordConsts, DistanceUtils, ExcelWrite,
  MFormatConvertToMega, MUsageStatistics, MD_InputSeqData, MTokenizer, MLexSeq,
  MLexDist, MDataFileInfo, MD_InputDistData, gutilities, gtokenizer, gfileutils,
  MCodons, MSynNonsynDist, MSeqDistBase, MNucDist, MComputeParsimInfo,
  MAminoMatrix, MAminoDist, MLTree, ShowTrees, GammaFuncs, MatrixConsts,
  MatrixFuncs, MLFuncs, MLModels, MLSearchThread, MTreeEstFunc, MTreeList,
  MTreeProc, MTreeSearchThread, ParsimSearchThreads, MCalibrationData,
  mreltimethreads, MegaMainPreferences, ProcessInputData,
  ProcessTreeCmds, ProcessTestCmds, ProcessParsimTreeCmds, ProcessMLTreeCmds,
  ProcessDistCmds, ProcessCodonOmegaCmds, mdist_command_threads,
  mdist_command_finalize, MPleaseWait, MRuntimeProgressDlg, MAnalysisSummary,
  mancestralstatesexporter, mancestralstatesexportheader, MMatrixExport,
  mtree_display_setup, MDisplayMatrixDlg, CustApp
  { you can add units after this }
  , Interfaces, laz_fpspreadsheet, mcustominifile, {$IFDEF MSWINDOWS}Windows,{$ELSE}LCLIntF,{$ENDIF}
  MTreeDataAdapter, msitecoverage, MPTree, MLTreeAnalyzer, MPartitionList,
  MTimeTreeValidation, MReltimeComputer, mcorrelationtest, mcalibrationsampler,
  mcalibrationdensity, MTreeBox, mtimelimit, nexustokens, nexusblock,
  nexustokenizer, nexusfile, mnexuswriter, mseqdataexport, AppLinker,
  esl_linker, mepthreads, mtajimaclocktestthread, mtajimaneutralitytestthread,
  mpartitions_blens_summary, mextendedlist, mmlcalculatedvalues, MTreeData,
  mworkflow_interfaces,
  //{$IFDEF DEBUG}LazLogger,{$ENDIF}
  mcheck_abort_reltime, mtipdatesfinder, mnode_time_data, mreltimeexport,
  mparse_mao_file, mgeographical_info, mega_citation, mcompare_bootstrap_trees,
  mtabular_tree_export, mmega_std_out, Video, midentical_tree_data_test,
  MSimpleTreeNode, madaptive_model_test,
  mmultinomial_sampler, mtree_split, mstringbuilder,
  mrobinson_foulds_metric,
  ml_thread_terminator, mtimetree_pruner,
  mdata_subset_caption, ml_rates_and_patterns_caption, ml_tree_caption,
  mreltime_caption, ml_gamma_param_caption,
  ml_tstv_caption, mp_tree_caption, mcaption_helper, mesl_input_data;


{  how the above uses clause looks before lazarus automically changes it when adding units

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, MD_MegaMain, MD_Sequences, MD_Align, StringUtils,
  MTreePack, MSubstitutionModelUtils, MSingleList,
  MProcessPack, MPartitionData, MOtuInfo, MLongintList,
  MLocusInfo, MGlobalSettings, MFileUtils, MegaVerConsts,
  MegaUtils_NV, MegaUtils, MegaPrivateFiles, MegaErrUtils, MegaConsts,
  MegaAnalysisPrefStrings, MDomainInfo, MDistPack, md5, MAnalysisInfo,
  Levenshtein, KeywordConsts, DistanceUtils, ExcelWrite,
  MFormatConvertToMega, MUsageStatistics, MD_InputSeqData, MTokenizer, MLexSeq,
  MLexDist, MDataFileInfo, MD_InputDistData, gutilities,
  gtokenizer, gfileutils, MCodons, MSynNonsynDist, MSeqDistBase, MNucDist,
  MComputeParsimInfo, MAminoMatrix, MAminoDist, MLTree,
  ShowTrees, GammaFuncs, MatrixConsts, MatrixFuncs, MLFuncs,
  MLModels, MLSearchThread, MTreeEstFunc, MTreeList, MTreeProc,
  MTreeSearchThread, ParsimSearchThreads, MCalibrationData,
  mreltimethreads, MegaMainPreferences, ProcessInputData, wpsCsvParser,
  ProcessTreeCmds, ProcessTestCmds, ProcessParsimTreeCmds, ProcessMLTreeCmds,
  ProcessDistCmds, ProcessCodonOmegaCmds, MPleaseWait, MRuntimeProgressDlg,
  MAnalysisSummary, mancestralstatesexporter, mancestralstatesexportheader,
  MMatrixExport, MDisplayMatrixDlg,
  CustApp
  { you can add units after this }
  , Interfaces, laz_fpspreadsheet, mcustominifile, {$IFDEF MSWINDOWS}Windows,{$ELSE}LCLIntF,{$ENDIF}
  MTreeDataAdapter, msitecoverage, MPTree, MLTreeAnalyzer, MPartitionList,
  MTimeTreeValidation, MReltimeComputer, mcorrelationtest,
  mcalibrationsampler, mcalibrationdensity, MTreeBox, mtimelimit,
  nexustokens, nexusblock, nexustokenizer, nexusfile,
  mnexuswriter, AppLinker, mepthreads, mtajimaclocktestthread,
  mtajimaneutralitytestthread,
  {$IFDEF DEBUG}, LazLogger{$ENDIF}
  ;


}


  {$R *.res}

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String = '';
begin
  // quick check parameters
  ErrorMsg := CheckOptions('h','help');
  try
    try
      {$IFDEF MSWINDOWS} { on windows, lazarus seems to think that this is a gui app (maybe because we use an html viewer?) so we have to set up the console window}
        {$IFDEF FPC}
        AllocConsole;
        IsConsole := True;
        SysInitStdIO;
        {$ENDIF}
      {$ENDIF}

      ExitCode := 1;
      D_MegaMain := TD_MegaMain.Create;
      D_MegaMain.Run;
      ExitCode := 0;
      D_MegaMain.Free;
    except
      on E:Exception do
        WriteLn(E.ClassName + ': ' + E.Message);
    end;
  finally
    {$IFDEF MPI_MEGA}
    {$ELSE}
    Terminate;
    {$ENDIF}
  end;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  DoTextOut(1, NextProgressLine, 'Usage: ' + ExeName +' -h', Red);
end;

var
  Application: TMyApplication;

//{$R *.res}

begin
  //{$IFDEF DEBUG}
  //SetHeapTraceOutput('heapTrace.trc');
  //{$ENDIF}
  Application:=TMyApplication.Create(nil);
  Application.Run;
  Application.Free;
end.

