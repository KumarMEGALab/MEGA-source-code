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

unit MegaPrivateFiles;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  sysutils;

const

  { Web Option Files}
  wofWebOptionsDialogsArchive = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs.zip';
  wofWebOptionsDialogsDir     = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs';
  wofWebSeqDataExportFile     = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'seqdata_export.html';
  wofDistDataExportFile = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'distdata_export.html';
  wofClustalParametersCodonsFile = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'clustalw_parameters_codons.html';
  wofClustalParametersDnaFile = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'clustalw_parameters_DNA.html';
  wofClustalParametersAAFile = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'clustalw_parameters_protein.html';
  wofSubtreeDrawingOptionsFile = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'subtree_drawing_options.html';
  wofTreeOptionsBranchFile = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'tree_options_branch.html';
  wofTreeOptionsCutoffFile = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'tree_options_cutoff.html';
  wofTreeOptionsLabelsFile = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'tree_options_labels.html';
  wofTreeOptionsScaleFile = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'tree_options_scale.html';
  wofTreeOptionsTreeStyleFile = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'tree_options_tree.html';
  wofAlignmentBuildMode = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'edit_build_alignment.html';
  wofInputDataDlg = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'input_data_options.html';
  wofNewickExportOptions = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'newick_export.html';
  wofSeqNameOptions = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'seqname_option.html';
  wofSelectGeneticCodeDlgFile = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'select_genetic_code_dlg.html';
  wofJSMessageDialog = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'js_message_dialog.html';
  wofSelectCDSsToImport = 'Private' + PathDelim + 'OptionsDialogs' + PathDelim + 'mega_web_dialogs' + PathDelim + 'select_cds_to_import.html';

  {MEGA Files}
  mfWebHelpArchive           = 'Private' + PathDelim + 'Help' + PathDelim + 'mega_web_help.zip';
  mfWebHelpDir               = 'Private' + PathDelim + 'Help' + PathDelim + 'mega_web_help';
  mfWebHelpIndex             = 'Private' + PathDelim + 'Help' + PathDelim + 'mega_web_help' + PathDelim + 'index.htm';
  mfHelpFile                 = 'Private' + PathDelim + 'Help' + PathDelim + 'mega8.chm';
  mfMegaProtoHelpFile        = 'Private' + PathDelim + 'Help' + PathDelim + 'MEGA6Proto.chm';
  mfQuickStartCCFile         = 'Private' + PathDelim + 'Help' + PathDelim + 'MEGA-CC-Quick-Start-Tutorial.pdf';
  mfMegaProtoRelTimeHelpFile = 'Private' + PathDelim + 'Help' + PathDelim + 'MEGA5ReltimeProto.chm';
  mfMyPegHelpFile            = 'Private' + PathDelim + 'Help' + PathDelim + 'MEGA_MD_Help.chm';
  mfAVIFile                  = 'Private' + PathDelim + 'Images' + PathDelim + 'mega_anim.AVI';
  mfAboutBMPFile             = 'Private' + PathDelim + 'Images' + PathDelim + 'MegaAbout.BMP';
  mfAboutMyPegFile           = 'Private' + PathDelim + 'Images' + PathDelim + 'MyPegAbout.bmp';
  mfSiteNodeFile             = 'Private' + PathDelim + 'Images' + PathDelim + 'siteno_nodeno.jpg';

  mfKeywordsFile             = 'Private' + PathDelim + 'Ini' + PathDelim + 'MKeywords.ini';
  mfFastaKeywordsFile        = 'Private' + PathDelim + 'Ini' + PathDelim + 'FastaKeywords.ini';
  mfCodeTables               = 'Private' + PathDelim + 'Ini' + PathDelim + 'MCodetables.ini';
  mfFixedLinksFile           = 'Private' + PathDelim + 'Ini' + PathDelim + 'MLink.ini';
  mfUserLinksFile            = 'Private' + PathDelim + 'Ini' + PathDelim + 'MUserLink.ini';
  mfMEGAStateIniFile         = 'Private' + PathDelim + 'Ini' + PathDelim + 'MEGA50State.ini';
  mfMyPegStateIniFile        = 'Private' + PathDelim + 'Ini' + PathDelim + 'MyPegState.ini';
  mfMyPegIconFile            = 'Private' + PathDelim + 'Images' + PathDelim + 'mypeg2.ico';
  mfMegaIconFile             = 'Private' + PathDelim + 'Images' + PathDelim + 'M6-banner.ico';
  mfRelTimeProtoIniFile      = 'Private' + PathDelim + 'Ini' + PathDelim + 'RelTimeProtoState.ini';
  mfPrototyperIniFile        = 'Private' + PathDelim + 'Ini' + PathDelim + 'Prototyper.ini';
  mfTemplateConstants        = 'Private' + PathDelim + 'Ini' + PathDelim + 'MTemplateConstants.ini';
  mfDpiSetting               = 'Private' + PathDelim + 'Ini' + PathDelim + 'dpiSettings.ini';
  mfCIPRESLogin              = 'Private' + PathDelim + 'Ini' + PathDelim + 'CIPRESLogin.ini';
  mfJobQueueIDs              = 'Private' + PathDelim + 'Ini' + PathDelim + 'JobQueueIDs.ini';
  mfDpiDetector              = 'Private' + PathDelim + 'bin' + PathDelim + 'WinDpiDetector.exe';

  mfMegaMainMruFiles         = 'Private' + PathDelim + 'Ini' + PathDelim + 'MegaMainMruFiles.txt';
  mfAlignmentExplorerFiles   = 'Private' + PathDelim + 'Ini' + PathDelim + 'AlignmentExplorerMruFiles.txt';

  TREE_SESSION_FILE_FILTER = 'MEGA Tree Sessions|*.mtsx;*.mts';
  SEQ_DATA_SESSION_FILE_FILTER = 'MEGA Data Sessions|*.msdx';
  
  mfMuscleDnaJsonDefault     = 'Private' + PathDelim + 'json' + PathDelim + 'muscle_dna.json';
  mfMuscleDnaJsonSaved       = 'Private' + PathDelim + 'json' + PathDelim + 'muscle_dna_saved.json';

  mfMuscleCodonsJsonDefault     = 'Private' + PathDelim + 'json' + PathDelim + 'muscle_codons.json';
  mfMuscleCodonsJsonSaved       = 'Private' + PathDelim + 'json' + PathDelim + 'muscle_codons_saved.json';

  mfMuscleAminoJsonDefault     = 'Private' + PathDelim + 'json' + PathDelim + 'muscle_amino.json';
  mfMuscleAminoJsonSaved       = 'Private' + PathDelim + 'json' + PathDelim + 'muscle_amino_saved.json';

  mfClustalDnaJsonDefault     = 'Private' + PathDelim + 'json' + PathDelim + 'clustal_dna.json';
  mfClustalDnaJsonSaved       = 'Private' + PathDelim + 'json' + PathDelim + 'clustal_dna_saved.json';

  mfClustalCodonsJsonDefault     = 'Private' + PathDelim + 'json' + PathDelim + 'clustal_codons.json';
  mfClustalCodonsJsonSaved       = 'Private' + PathDelim + 'json' + PathDelim + 'clustal_codons_saved.json';

  mfClustalAminoJsonDefault     = 'Private' + PathDelim + 'json' + PathDelim + 'clustal_amino.json';
  mfClustalAminoJsonSaved       = 'Private' + PathDelim + 'json' + PathDelim + 'clustal_amino_saved.json';
  mfCursorCompress = 'Private' + PathDelim + 'cursors' + PathDelim + 'compress.cur';
  mfCursorFlip = 'Private' + PathDelim + 'cursors' + PathDelim + 'flip.cur';
  mfCursorFlipall = 'Private' + PathDelim + 'cursors' + PathDelim + 'flipall.cur';
  mfCursorRoot = 'Private' + PathDelim + 'cursors' + PathDelim + 'root.cur';
  mfCursorZoom = 'Private' + PathDelim + 'cursors' + PathDelim + 'zoom.cur';

  mfAlleleFreqParserKeywords = 'Private' + PathDelim + 'Ini' + PathDelim + 'AlleleFreqKeywords.ini';
  mfAlleleFileStringToToken = 'Private' + PathDelim + 'Ini' + PathDelim + 'StringToToken.ini';

  MF_ANALYSIS_SETTINGS_FILE = 'Private' + PathDelim + 'json' + PathDelim + 'AnalysisSettings.json';

  { Session Files }
  MEGASessionFile = 'Private' + PathDelim + 'Ini' + PathDelim + 'MEGASessionFile.ini';
  sfAlignEditMainForm = 'Private' + PathDelim + 'Ini' + PathDelim + 'sfAlignEditMainForm.ini';
  sfTreeViewForm = 'Private' + PathDelim + 'Ini' + PathDelim + 'sfTreeViewForm.ini';
  sfTraceEditForm = 'Private' + PathDelim + 'Ini' + PathDelim + 'sfTraceEditForm.ini';

  {$IFDEF MSWINDOWS}
    {$IFDEF CPU32}
    CF_REGRESSION_EXE = 'nnls_i386.exe';
    {$ELSE}
    CF_REGRESSION_EXE = 'nnls_x86_64.exe';
    {$ENDIF}
  {$ELSE}
    {$IFDEF DARWIN}
    CF_REGRESSION_EXE = 'nnls_x86_64_darwin';
    {$ELSE}
      {$IFDEF CPU32}
      CF_REGRESSION_EXE = 'nnls_i386';
      {$ELSE}
      CF_REGRESSION_EXE = 'nnls_x86_64';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

implementation

end.
