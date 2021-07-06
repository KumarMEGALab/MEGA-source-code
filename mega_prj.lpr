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

program mega7prj;

{$mode objfpc}{$H+}

uses
  {$DEFINE UseCThreads}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, printer4lazarus, Mega_Main, clone_finder_main_form,
  mreadcountgridrenderer, mabstractgridrenderer, mclonefreqsgridrenderer,
  mcnvdrawgridrenderer, mprocessclonefindercmds, mclonefinderoptions,
  monelocusonepopdlg, monelocusmultipopdlg, mmultilocusmultipopdlg,
  mallelefreqresultsdlg, mallelefreqviewer, mclonefinderoptionsform,
  mclonefinderthreads, binomialreplicates, mdefaultclonedataparser, mcnvparser,
  mclonefrequencyparser, mccfparser, mcancercellfractionsparser,
  mancestreeclonedataparser, mabstractclonedataparser, mtumorsampleprofile,
  mtumorclonefrequency, mcnvprovile, mclonefinderalignment, mcancercellfraction,
  clonefinderconstants, UniqueInstanceBase, UniqueInstance, UniqueInstanceRaw,
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
  gutilities, gtokenizer, gfileutils, MFileUtils, MSeqDistBase, MTreeList,
  MPartition, MPartitionList, DistanceUtils, MLTreeAnalyzer, MTreeEstFunc,
  MLFuncs, MPTree, MTreeSearchThread, MSimpleTreeNode, MTreeProc, MTreeBox,
  MLSearchThread, mreltimecomputer, MTreeDataAdapter, MAminoDist, MNucDist,
  MAminoMatrix, MSynNonsynDist, MUsageStatistics, MegaMainPreferences,
  MAlignEditMainForm, MAlignGrid, MD_Align, MClustalW, MTraceEdit,
  miniformstream, MAlnProgress, MAlnThread, matrices, malignmentfileloader,
  mmegaalignmentfileloader, AppLinker, AppOptionsDlg, ProcessCodonOmegaCmds,
  wpsCsvParser, MLegendGenerator, mcascadingstyles, MWriteOutputDlg, ShowTrees,
  MMatrixExport, MComputeParsimInfo, MAnalysisSummary, mnexuswriter,
  mancestralstatesexportheader, mancestralstatesexporter, mreltimethreads,
  MSiteCoverage, MCalibrationDlg, MTimeTreeWizard, MTimeTreeWizardForm,
  MVirtualScrollbox, MTreeRebuildProgress, MParsimonyEvoPathsDlg,
  MFormatConvertToMega, textsearchdialog, MInnerForm, ProcessMLTreeCmds,
  ProcessTreeCmds, MGeneDuplicationWizard, GeneDuplicationWizardForm,
  MSubstitutionModelUtils, MutationDetailView, EvoD, MyPegThreads,
  ProcessParsimTreeCmds, ParsimSearchThreads, MParsNode, ProcessDistCmds,
  MDisplayMatrixDlg, MOutputMatrixDlg, ProcessTestCmds, MWebBrowser,
  MBrowserImportController, mhtmltotext, MSeqNameOptionDlg, ceferr,
  manalysissettings, mupdates, mcolorcodes, MMegaWindowInfo, MStringCleaner,
  Levenshtein, mcustominifile, mgroupinfo, MTimeTreeValidation,
  applinkeroptions, ClustalOptionForm, mupdatesdlg, maligneditwelcomedlg,
  MTraceEditForm, mdatatypedlg, mseqdataexportoptionsdlg, MFindDlg,
  MMyPegMessageDlg, cef3lcl, cef3types, cef3intf, cef3ref, cef3lib,
  mrenderprocesshandler, ceffilescheme, mbrowserutils, mbrowserprocesshandler,
  sysutils, gettext, cef3own, lhelpcontrolpkg, recentlyusedfiles, msubtreedlg,
  alignmentconcatenator, mfastaparser, mfastaalignmentfileloader, maboutbox,
  genedomainpropertiesdlg, msitepickform, mgridcolumnresizer, mhelpfiles,
  mhelpkeywords, htmloptionsdlg, mselectoutgrouptaxadlg, mwebdialogfiles,
  mformatdlg, mtreeinfoform, mutationexplorer, graphics, FrameViewer09, Dialogs,
  Classes, cef3scp, mdrawmenuitems, protodatatypeform,
  mseqdataexport, msvgstrings, mseqexportoptions, mabstracttreerenderer,
  mpdftreerenderer, mbitmaptreerenderer, msvgtreerenderer,
  mtajimaneutralitytestthread, mtajimaclocktestthread, MSpeciesMapDlg,
  mtajimatestexport, mmodeltestmatrixexport, mpattertestmatrixexport,
  mreltimeexport, mreltimetreenode, mclocktestexport, mcalibrateclockdlg,
  memftreerenderer, mstrictclocktree, mdensitydistdlg, MCleanSpeciesNamesDlg,
  mcalibrationdensity, mtipdatesfinder, mparsetipdatesdlg, mcalibrationsampler,
  mshortcutshelper,
  mversioninfo, mdist_command_results_wrapper, mmodel_test_results, mepthreads,
  mtree_display_setup, MJumpToGeneDupDlg, mmega_error_handler,
  mdist_command_threads, mdist_command_finalize, taxa_names_frame,
  branch_lengths_frame, node_stats_frame, layout_frame, frame_utils,
  subtree_frame, compute_frame, ancestors_frame, distance_scale_frame,
  time_scale_frame, div_times_frame, mdefine_genetic_code_dlg,
  mgeographical_info, mancestral_states_navigator, mbaby_bootstrap_calculator,
  multi_trees_frame, caption_frame, mcl_command_threads,
  mcomposition_stats_thread, gene_duplications_frame, collapse_nodes_frame,
  tree_info_frame, mallele_frequency, mallele_frequency_search, mentropy,
  msequence_identity_dlg, mchar_scanner, mraw_fasta_loader,
  msequence_name_filter, mstringbuilder, mbootstrap_sampler,
  medit_site_labels_frame, msite_labels, app_options_frame, mega_info_form;


{
 orig uses clause

 uses
   {$DEFINE UseCThreads}
   {$IFDEF UNIX}{$IFDEF UseCThreads}
   cthreads,
   {$ENDIF}{$ENDIF}
   Interfaces, // this includes the LCL widgetset
   Forms, tachartlazaruspkg, printer4lazarus, Mega_Main, MAnalysisWizard,
   MAnalysisWizardForm, mtreeloaders, AlnBuilder_HC, Walk_Through_MEGA,
   TreeExplorer_HC, TextEditorHelp_HC, popuprefs_RH, PhylogeneticTrees_RH,
   Introduction_RH, InputData_RH, BasicStatistics_RH, CaptionExpert_HC,
   CentralDialogBox_HC, ContextHelp_HC, DataExplorerHelp_HC, DistanceMethods_HC,
   ErrorMessages_HC, FAQs_RH, Glossary_RH,
   manalysisprefdlg, MProcessPack, mdistpack, mtreepack, mglobalsettings,
   MegaConsts, MegaUtils, MSingleList, MegaVerConsts, KeywordConsts, MLTree,
   GammaFuncs, MatrixConsts, MTreeData, MatrixFuncs, MCalibrationData,
   StringUtils, MLModels, MD_Sequences, MD_InputSeqData, MD_InputDistData,
   MCodons, ExcelWrite, MegaPrivateFiles, MegaAnalysisPrefStrings, MegaErrUtils,
   MegaUtils_NV, mruntimeprogressdlg, MAnalysisInfo, MTaxaGpsDlg,
   MMultiStageProgress, MDomainInfo, MOtuInfo, MPartitionData, MLocusInfo,
   MLongIntList, ProcessInputData, MDataFileInfo, MInputDataOptionsDlg,
   MSelectGeneticCodeDlg, MTreeViewForm, MTreeInputForm, MV_SeqDataExplorer,
   MGeneDomainDlg, MEditorForm, MV_DistDataExplorer, MLexDist,
   MLexSeq, MTokenizer, MVS_DistDataExplorer, MVS_SeqDataExplorer,
   MPleaseWaitDlg, MPleaseWait, MProgressPanel, MSeqDataSearchThreads,
   MV_Columns, MExampleFiles, nexustokens, nexustokenizer, nexusfile, nexusblock,
   gutilities, gtokenizer, gfileutils, MFileUtils, MSeqDistBase, MTreeList,
   MPartition, MPartitionList, DistanceUtils, MLTreeAnalyzer, MTreeEstFunc,
   MLFuncs, MPTree, MTreeSearchThread, MSimpleTreeNode, MTreeProc, MTreeBox,
   MLSearchThread, mreltimecomputer, MTreeDataAdapter, MAminoDist, MNucDist,
   MAminoMatrix, MSynNonsynDist, MUsageStatistics, MegaMainPreferences,
   MAlignEditMainForm, MAlignGrid, MD_Align, MClustalW, MTraceEdit, MAlnProgress,
   MAlnThread, matrices, malignmentfileloader, mmegaalignmentfileloader,
   AppLinker, AppOptionsDlg, ProcessCodonOmegaCmds, wpsCsvParser,
   MLegendGenerator, mcascadingstyles, MWriteOutputDlg, ShowTrees, MMatrixExport,
   MComputeParsimInfo, MAnalysisSummary, mnexuswriter,
   mancestralstatesexportheader, mancestralstatesexporter, mreltimethreads,
   MSiteCoverage, MCalibrationDlg, MTimeTreeWizard, MTimeTreeWizardForm,
   MVirtualScrollbox, MTreeRebuildProgress, MParsimonyEvoPathsDlg,
   MFormatConvertToMega, textsearchdialog, MInnerForm, ProcessMLTreeCmds,
   ProcessTreeCmds, MGeneDuplicationWizard, GeneDuplicationWizardForm,
   MSubstitutionModelUtils, MutationDetailView, EvoD, MyPegThreads,
   ProcessParsimTreeCmds, ParsimSearchThreads, MParsNode, ProcessDistCmds,
   MDisplayMatrixDlg, MOutputMatrixDlg, ProcessTestCmds, MWebBrowser,
   MBrowserImportController, mhtmltotext, MSeqNameOptionDlg, ceferr,
   manalysissettings, mupdates, mcolorcodes, MMegaWindowInfo, MStringCleaner,
   Levenshtein, mcustominifile, mgroupinfo, MTimeTreeValidation,
   applinkeroptions, ClustalOptionForm, mupdatesdlg,
   maligneditwelcomedlg, MTraceEditForm, mdatatypedlg, mseqdataexportoptionsdlg,
   MFindDlg, MMyPegMessageDlg, cef3lcl, cef3types, cef3intf, cef3ref, cef3lib,
   mrenderprocesshandler, ceffilescheme, mbrowserutils, mbrowserprocesshandler,
   sysutils, gettext, cef3own, lhelpcontrolpkg, recentlyusedfiles, msubtreedlg,
   alignmentconcatenator, mfastaparser, mfastaalignmentfileloader, maboutbox,
   genedomainpropertiesdlg, msitepickform, mgridcolumnresizer, mhelpfiles,
   mhelpkeywords, htmloptionsdlg, mselectoutgrouptaxadlg, mwebdialogfiles,
   mformatdlg, mtreeinfoform, mutationexplorer, graphics, FrameViewer09, Dialogs,
   Classes, cef3scp, mdrawmenuitems, protodatatypeform,
   mseqdataexport, msvgstrings, mseqexportoptions, mabstracttreerenderer,
   mpdftreerenderer, mbitmaptreerenderer, msvgtreerenderer,
   mtajimaneutralitytestthread, mtajimaclocktestthread,
   MSpeciesMapDlg, mtajimatestexport, mmodeltestmatrixexport,
   mpattertestmatrixexport, mreltimeexport, mreltimetreenode, mclocktestexport,
   mcalibrateclockdlg, memftreerenderer, mstrictclocktree, mdensitydistdlg,
   MCleanSpeciesNamesDlg, mcalibrationdensity, mtipdatesfinder,
   mparsetipdatesdlg, mcalibrationsampler, mshortcutshelper
   ;

}
{$R *.res}

  procedure RegisterSchemes(const registrar: TCefSchemeRegistrarRef);
  begin
    registrar.AddCustomScheme('local', True, True, False, False, False, False);
  end;

  procedure InitializeChromium;
  var
    subprocess: ustring;
    PrjPath: ustring;
  begin
    {$IFNDEF NO_BROWSER}
    {$IFDEF MSWINDOWS}
      {$IFDEF CPU32}
      subprocess := ustring(ExtractFilePath(Application.ExeName) + ustring('mega_browser_32.exe'));
      {$ELSE}
      subprocess := ustring(ExtractFilePath(Application.ExeName) + 'mega_browser_64.exe');
      {$ENDIF}
    {$ELSE}
      {$IFDEF DARWIN}
      subprocess := ustring(ExtractFileDir(ExtractFileDir(ExpandFileName(Paramstr(0)))) + PathDelim + 'Frameworks/MEGAX Helper.app/Contents/MacOS/MEGAX Helper');
      {$ELSE}
      subprocess := ustring(ExtractFilePath(Application.ExeName) + 'mega_browser');
      {$ENDIF}
    {$ENDIF}
    if not FileExists(subprocess) then
      raise Exception.Create('MEGA is missing a needed resource file (mega_browser). Please re-install MEGA');
    CefBrowserSubprocessPath := subprocess;
    if IsDeveloper then
      CefRemoteDebuggingPort := 9222;
    PrjPath := ustring(GetAppConfigDir(False));
    {$IFNDEF DARWIN}
    CefLocalesDirPath := ustring(ExtractFilePath(Application.ExeName)) + ustring('locales');
    {$ELSE}
    CefLocalesDirPath := ustring(ExtractFileDir(ExtractFileDir(ExpandFileName(Paramstr(0))))+PathDelim+'Frameworks/Chromium Embedded Framework.framework/Resources');
    {$ENDIF}
    CefLocale := ustring('en');
    //GetLanguageIDs(Lang, FallbackLang);
    //CefLocale := UTF8Decode(FallbackLang);
    CefSingleProcess := false;
    CefCachePath := PrjPath + ustring('browser_cache');
    CefLogFile := PrjPath + ustring('cef_debug.log');
    {$IFDEF DEBUG}
    CefLogSeverity := LOGSEVERITY_DEFAULT;
    {$ELSE}
    CefLogSeverity := LOGSEVERITY_ERROR_REPORT;
    {$ENDIF}
    CefOnBeforeCommandLineProcessing := @OnBeforeCommandLineProcessing;
    {$IFDEF DARWIN}
    CefNoSandbox := True;
    {$ENDIF}
    CefBrowserProcessHandler := TCustomBrowserProcessHandler.Create;
    CefRenderProcessHandler := TCustomRenderProcessHandler.Create;
    CefOnRegisterCustomSchemes := @RegisterSchemes;
    try
      if not CefInitialize then
        raise Exception.Create('CefInitialize function failed');
    except
      on E:Exception do
        ShowMessage('failed to initialize Chromium: ' + E.Message);
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

  Application.Scaled:=True;
  Application.Title := 'MEGAX';
  RequireDerivedFormResource := True;
  Application.Initialize;
  InitializeChromium;
  Application.CreateForm(TMegaForm, MegaForm);
  Application.CreateForm(TAnalysisPrefDlg, AnalysisPrefDlg);
  Application.CreateForm(TUpdatesDlg, UpdatesDlg);
  Application.CreateForm(TWelcomeForm, WelcomeForm);
  Application.CreateForm(TAlignmentDataTypeDlg, AlignmentDataTypeDlg);
  Application.CreateForm(TSubtreeDlg, SubtreeDlg);
  Application.CreateForm(TWriteOutputDlg, WriteOutputDlg);
  Application.CreateForm(THtmlOptionsDialog, HtmlOptionsDialog);
  Application.CreateForm(TSelectOutgroupTaxaDlg, SelectOutgroupTaxaDlg);
  Application.CreateForm(TProtoDataTypeDlg, ProtoDataTypeDlg);
  Application.CreateForm(TImageForm, ImageForm);
  {$IFNDEF DARWIN}
  UpdateFormFonts;
  {$ENDIF}

  try
    try
      Application.Run;
    except
      on E:Exception do
      begin
        {$IFDEF DEBUG}
        aLog := TStringList.Create;
        aLog.Text := E.Message;
        aLog.SaveToFile(ExtractFileDir(Application.ExeName) +'mega.log');
        aLog.Free;
        {$ENDIF}
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
      end;
    end;
  finally
    //CefQuitMessageLoop;
    //CefShutDown;
  end;
end.

