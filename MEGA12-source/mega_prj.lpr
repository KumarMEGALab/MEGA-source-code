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

program mega_prj;

{$mode objfpc}{$H+}

uses
  {$DEFINE UseCThreads}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  InitSubProcess, // Linux requires CEF be initialized before LCL widgets
  Interfaces, // this includes the LCL widgetset
  {$IFDEF DARWIN}uCEFWorkScheduler,{$ENDIF}
  Forms, tachartlazaruspkg, printer4lazarus, Mega_Main, umegarenderer,
  uMegaBrowser, mceflogger2,
  mimageform, MAnalysisWizard, MAnalysisWizardForm, mtreeloaders, AlnBuilder_HC,
  Walk_Through_MEGA, TreeExplorer_HC, TextEditorHelp_HC, popuprefs_RH,
  PhylogeneticTrees_RH, Introduction_RH, InputData_RH, BasicStatistics_RH,
  CaptionExpert_HC, CentralDialogBox_HC, ContextHelp_HC, DataExplorerHelp_HC,
  DistanceMethods_HC, ErrorMessages_HC, FAQs_RH, Glossary_RH, manalysisprefdlg,
  MProcessPack, mdistpack, mtreepack, mglobalsettings, MegaConsts, MegaUtils,
  MSingleList, MegaVerConsts, KeywordConsts, MLTree, GammaFuncs, MatrixConsts,
  MTreeData, MatrixFuncs, MCalibrationData, StringUtils, MLModels, MD_Sequences,
  MD_InputSeqData, MD_InputDistData, MCodons, ExcelWrite, MegaPrivateFiles,
  MegaAnalysisPrefStrings, MegaErrUtils, MegaUtils_NV, mruntimeprogressdlg,
  MAnalysisInfo, MTaxaGpsDlg, MMultiStageProgress, MDomainInfo, MOtuInfo,
  MPartitionData, MLocusInfo, MLongIntList, ProcessInputData, MDataFileInfo,
  MInputDataOptionsDlg, MSelectGeneticCodeDlg, MTreeViewForm, MTreeInputForm,
  MV_SeqDataExplorer, MGeneDomainDlg, MEditorForm, MV_DistDataExplorer,
  MLexDist, MLexSeq, MTokenizer, MVS_DistDataExplorer, MVS_SeqDataExplorer,
  MPleaseWaitDlg, MPleaseWait, MProgressPanel, MSeqDataSearchThreads,
  MV_Columns, MExampleFiles, nexustokens, nexustokenizer, nexusfile, nexusblock,
  gutilities, gtokenizer, gfileutils, mnexusalignmentconverter, MFileUtils,
  MSeqDistBase, MTreeList, MPartition, MPartitionList, DistanceUtils,
  MLTreeAnalyzer, MTreeEstFunc, MLFuncs, MPTree, MTreeSearchThread,
  MSimpleTreeNode, MTreeProc, MTreeBox, MLSearchThread, mreltimecomputer,
  MTreeDataAdapter, MAminoDist, MNucDist, MAminoMatrix, MSynNonsynDist,
  MUsageStatistics, MegaMainPreferences, MAlignEditMainForm, MAlignGrid,
  MD_Align, MClustalW, MTraceEdit, miniformstream, MAlnProgress, MAlnThread,
  matrices, malignmentfileloader, mmegaalignmentfileloader, AppLinker,
  AppOptionsDlg, ProcessCodonOmegaCmds, MLegendGenerator,
  mcascadingstyles, MWriteOutputDlg, ShowTrees, MMatrixExport,
  MComputeParsimInfo, MAnalysisSummary, mnexuswriter,
  mancestralstatesexportheader, mancestralstatesexporter, mreltimethreads,
  MSiteCoverage, MCalibrationDlg, MTimeTreeWizard, MTimeTreeWizardForm,
  MVirtualScrollbox, MTreeRebuildProgress, MParsimonyEvoPathsDlg,
  MFormatConvertToMega, textsearchdialog, MInnerForm, MFormatConvertOptDlg,
  dlgConfirmReplace, dlgReplaceText, ProcessMLTreeCmds, ProcessTreeCmds,
  MGeneDuplicationWizard, GeneDuplicationWizardForm, MSubstitutionModelUtils,
  MutationDetailView, EvoD, MyPegThreads, ProcessParsimTreeCmds,
  ParsimSearchThreads, MParsNode, ProcessDistCmds, MDisplayMatrixDlg,
  MOutputMatrixDlg, ProcessTestCmds, MBrowserImportController,
  manalysissettings, mupdates, mcolorcodes, MMegaWindowInfo,
  MStringCleaner, Levenshtein, mcustominifile, mgroupinfo, MTimeTreeValidation,
  applinkeroptions, ClustalOptionForm, mupdatesdlg, maligneditwelcomedlg,
  MTraceEditForm, mdatatypedlg, mseqdataexportoptionsdlg, MFindDlg,
  MMyPegMessageDlg, mbrowserutils, sysutils, gettext, lhelpcontrolpkg,
  recentlyusedfiles, msubtreedlg, alignmentconcatenator, mfastaparser,
  mfastaalignmentfileloader, maboutbox, genedomainpropertiesdlg, msitepickform,
  mgridcolumnresizer, mhelpfiles, mhelpkeywords, htmloptionsdlg,
  mselectoutgrouptaxadlg, mwebdialogfiles, mformatdlg, mtreeinfoform,
  mutationexplorer, graphics, FrameViewer09, Dialogs, Classes, mdrawmenuitems,
  protodatatypeform, mseqdataexport, msvgstrings, mseqexportoptions,
  mabstracttreerenderer, mpdftreerenderer, mbitmaptreerenderer,
  msvgtreerenderer, mtajimaneutralitytestthread, mtajimaclocktestthread,
  MSpeciesMapDlg, mtajimatestexport, mmodeltestmatrixexport,
  mpattertestmatrixexport, mreltimeexport, mreltimetreenode, mclocktestexport,
  OutputUtils, MNewickExportOptions, mcalibrateclockdlg, memftreerenderer,
  mstrictclocktree, mdensitydistdlg, MCleanSpeciesNamesDlg, mcalibrationdensity,
  mtipdatesfinder, mparsetipdatesdlg, mcalibrationsampler, mshortcutshelper,
  {$IFDEF MSWINDOWS}{$IFDEF CPU32}windows,
  {$ENDIF}{$ENDIF}
  mversioninfo, mdist_command_results_wrapper, mmodel_test_results, mepthreads,
  mtree_display_setup, MJumpToGeneDupDlg, mmega_error_handler,
  mdist_command_threads, mdist_command_finalize, taxa_names_frame,
  branch_lengths_frame, node_stats_frame, layout_frame, frame_utils,
  subtree_frame, compute_frame, ancestors_frame, distance_scale_frame,
  div_times_frame, mdefine_genetic_code_dlg, mgeographical_info,
  mancestral_states_navigator, multi_trees_frame,
  caption_frame, mcl_command_threads,
  mcomposition_stats_thread, gene_duplications_frame, collapse_nodes_frame,
  tree_info_frame, mallele_frequency, mallele_frequency_search, mentropy,
  msequence_identity_dlg, mchar_scanner, mraw_fasta_loader,
  msequence_name_filter, mstringbuilder, mextendedlist, medit_site_labels_frame,
  msite_labels, mlexallelefreq, app_options_frame, mega_info_form, syncobjs,
  macos_files, mega_citation, midentical_tree_data_test,
  mnode_time_data, mcorrelationtest,
  mcheck_abort_reltime, mcustom_msg_dlg, mdeveloper_console, mimageexportdlg,
  MNewickExportDlg, mdisplay_settings_form, mcancellable, uCEFApplication,
  madaptive_model_test, mmodel_info_list, mtabular_tree_export,
  ml_thread_terminator, uCEFTypes, mdeveloper_settings_form, mesl_options_dlg,
  mgroup_names_editor, MCustomActionHint, mcutoff_dlg,
  tree_options_frame, subtree_bracket_frame, subtree_compress_frame,
  subtree_caption_frame, subtree_branches_frame, subtree_markers_frame,
  subtree_image_frame, mcodon_translation_selection_map,
  commonly_used_tools_frame, bootstrap_frame,
  mtaxa_name_search, mtree_display_settings_cache, mfont_name_dlg,
  mreltime_caption, mdata_subset_caption, ml_rates_and_patterns_caption,
  ml_tree_caption, ml_gamma_param_caption,
  mcaption_helper, template_helper, ml_tstv_caption, mdefault_tree_caption,
  mancestral_states_caption, mp_tree_caption, esl_linker, mesl_menu_builder,
  applinker_result_file, ml_substitution_pattern_caption, mesl_input_data,
  mimage_viewer, dr_phylo_example_files, mdomaininfo_search, mdrphylo_caption,
  mdata_collection_settings_form;

{$R *.res}

  procedure FinalizeChromium;
  {$IFNDEF DEBUG}
  var
    logFile: ustring;
    temp: PChar = nil;
  {$ENDIF}
  begin
    {$IFNDEF DEBUG}
    logFile := GlobalCEFApp.LogFile;
    {$ENDIF}
    {$IFDEF DARWIN}
    if GlobalCEFWorkScheduler <> nil then
       GlobalCEFWorkScheduler.StopScheduler;
    {$ENDIF}

    {$IFDEF DARWIN}
    DestroyGlobalCEFWorkScheduler;
    {$ENDIF}
    DestroyGlobalCEFApp;
    {$IFNDEF DEBUG}
    try
      temp := PChar(logFile);
      if FileExists(temp) then
        DeleteFile(temp);
    except

    end;
    {$ENDIF}
  end;

  procedure UpdateFormFonts;
  var
    i: Integer;
    aForm: TCustomForm;
    aFontName: String;
  begin
    aFontName := EmptyStr;
    {$IFDEF MSWINDOWS}
    if Screen.Fonts.IndexOf('Open Sans') >= 0 then
      aFontName := 'Open Sans';
    {$ELSE}
    if Screen.Fonts.IndexOf('OpenSymbol') >= 0 then
      aFontName := 'OpenSymbol';
    {$ENDIF}
    if Screen.CustomFormCount > 0 then
      for i := 0 to Screen.CustomFormCount - 1 do
      begin
        aForm := Screen.CustomForms[i];
        {$IFDEF MSWINDOWS}
        if aFontName <> EmptyStr then
          aForm.Font.Name := aFontName;
        aForm.Font.Size := 9;
        {$ELSE}
        if aFontName <> EmptyStr then
          aForm.Font.Name := aFontName;
        aForm.Font.Size := 10;
        {$ENDIF}
        aForm.Font.Color := $00333333;
      end;
    {$IFDEF MSWINDOWS}
    if aFontName <> EmptyStr then
      Screen.MenuFont.Name := aFontName;
    Screen.MenuFont.Size := 10;
    {$ELSE}
    if aFontName <> EmptyStr then
      Screen.MenuFont.Name := aFontName;
    Screen.MenuFont.Size := 11;
    {$ENDIF}
    Screen.MenuFont.Style := Screen.MenuFont.Style + [fsBold];
    Screen.MenuFont.Color := $00333333;
    {$IFDEF MSWINDOWS}
    if aFontName <> EmptyStr then
      Screen.MenuFont.Name := aFontName;
    Screen.SystemFont.Size := 10;
    {$ELSE}
    if aFontName <> EmptyStr then
      Screen.SystemFont.Name := aFontName;
    Screen.SystemFont.Size := 11;
    {$ENDIF}
    Screen.SystemFont.Style := Screen.SystemFont.Style + [fsBold];
    Screen.SystemFont.Color := $00333333;
  end;

var
  aLog: TStringList = nil;
begin
  //{$IFDEF DEBUG}
  //{$if declared(UseHeapTrace)}
  //GlobalSkipIfNoLeaks := true;     // supported as of debugger version 3.2.0
  //{$ifend}
  //SetHeapTraceOutput('memory-trace-gui.log'); // supported as of debugger version 3.2.0
  //{$ENDIF}                                 // KT: to detect memory leak
  {$IFDEF MSWINDOWS}
    {$IFDEF CPU32}
    {$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE};
    {$ENDIF}
  {$ENDIF}

  try
    try
      if (GlobalCEFApp.Status = asInitialized) then
      begin
  Application.Scaled := True;
  Application.Title := 'MEGA12';

        RequireDerivedFormResource := True;
        Application.Initialize;
        AnalysisPrefsCriticalSection := TCriticalSection.Create;
        Application.CreateForm(TMegaForm, MegaForm);
        Application.CreateForm(TImageForm, ImageForm);
        Application.CreateForm(TAnalysisPrefDlg, AnalysisPrefDlg);
        Application.CreateForm(TUpdatesDlg, UpdatesDlg);
        Application.CreateForm(TWelcomeForm, WelcomeForm);
        Application.CreateForm(TAlignmentDataTypeDlg, AlignmentDataTypeDlg);
        Application.CreateForm(TSubtreeDlg, SubtreeDlg);
        Application.CreateForm(TWriteOutputDlg, WriteOutputDlg);
        Application.CreateForm(THtmlOptionsDialog, HtmlOptionsDialog);
        Application.CreateForm(TSelectOutgroupTaxaDlg, SelectOutgroupTaxaDlg);
        Application.CreateForm(TProtoDataTypeDlg, ProtoDataTypeDlg);
  Application.CreateForm(TDisplaySettingsForm, DisplaySettingsForm);
        HintWindowClass := TCustomActionHint;
        {$IFNDEF DARWIN}
        UpdateFormFonts;
        {$ENDIF}
        Application.Run;
      end;
    except
      on E:Exception do
      begin
        {$IFDEF DEBUG}
        aLog := TStringList.Create;
        aLog.Text := E.Message;
        aLog.SaveToFile(ExtractFileDir(Application.ExeName) +'mega.log');
        aLog.Free;
        {$ENDIF}
        ShowMessage('MEGA application error: ' + E.Message);
      end;
    end;
  finally
    if Assigned(AnalysisPrefsCriticalSection) then
      FreeAndNil(AnalysisPrefsCriticalSection);
    if Assigned(GlobalCEFApp) then
        FinalizeChromium;
  end;
end.

