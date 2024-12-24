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

unit mbrowserutils;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF VISUAL_BUILD}
  Interfaces, uCEFConstants,
  {$ENDIF}
  Classes, SysUtils, Messages;

const
  {$IFDEF VISUAL_BUILD}
  CLIENT_ID_TAB_COPY       = MENU_ID_USER_FIRST + 0;
  CLIENT_ID_WINDOW_COPY    = MENU_ID_USER_FIRST + 1;
  CLIENT_ID_CLIPBOARD_COPY = MENU_ID_USER_FIRST + 2;
  {$ENDIF}

  CEF_INITIALIZED      = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A50;
  CEF_DESTROYTAB       = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A51;
  CEF_CREATENEXTCHILD  = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A52;
  CEF_CREATENEXTTAB    = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A53;
  CEF_CHILDDESTROYED   = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A54;

  CEF_UPDATETITLE      = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A55;
  CEF_UPDATEADDRESS    = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A56;
  CEF_UPDATESTATE      = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A57;
  CEF_UPDATESTATUSTEXT = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A58;
  CEF_RECVPROCESSMSG   = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A59;
  CEF_CONTEXTMENUCMD   = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A5A;

  CEF_SETFOCUS         = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A5B;
  CEF_LOADERROR        = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A5C;
  CEF_CLOSEWINDOW      = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A5D;
  CEF_SHOWMESSAGE      = {$IFDEF WINDOWS}WM_APP +{$ENDIF} $A5E;


  DOMVISITOR_MSGNAME_PARTIAL  = 'domvisitorpartial';
  DOMVISITOR_MSGNAME_FULL     = 'domvisitorfull';
  RETRIEVEDOM_MSGNAME_PARTIAL = 'retrievedompartial';
  RETRIEVEDOM_MSGNAME_FULL    = 'retrievedomfull';
  RETRIEVE_FETCH_TREE_SCALE_OPTIONS = 'retrive_fetch_tree_scale_options';
  FRAMEIDS_MSGNAME            = 'getframeids';
  CONSOLE_MSG_PREAMBLE        = 'DOMVISITOR';
  TEST_MSG                    = 'TESTING';
  TEST_MSG2                   = 'TESTINGAGAIN';
  NEWICK_EXPORT_TRUTHVAL      = 'newicktruthval';
  RETRIEVE_NEWICK_DOM         = 'retrivenewickdom';


  NODE_ID = 'newick_branch_lengths';

  MEGA_JS_TEMPLATE_PLACEHOLDER = '//MEGA_JS_TEMPLATE_PLACEHOLDER';
  RENDER_PROCESS_ERROR = 'RENDER_PROCESS_ERROR';
  VISITDOMPROC_FETCHSEQSFROMHTML = 'visitdomproc-fetchseqsfromhtml';
  VISITDOMPROC_FETCHSEQSFROMHTML_COMPLETE = 'visitdomproc-fetchseqsfromhtml-complete';
  VISITDOMPROC_FETCHSEQSFROMHTML_EXPORT =   'visitdomproc-fetchseqsfromhtml-export';
  VISITDOMPROC_COPYALLTOCLIPBOARD = 'visitdomproc-copyalltoclipboard';
  VISITDOMPROC_COPYALLTOCLIPBOARD_COMPLETE = 'visitdomproc-copyalltoclipboard-complete';
  VISITDOMPROC_SAVECAPTIONTOTEXT = 'visitdomproc-savecaptiontotext';
  VISITDOMPROC_SAVECAPTIONTOTEXT_COMPLETE = 'visitdomproc-savecaptiontotext-complete';
  VISITDOMPROC_MACSAVECAPTIONTOTEXT = 'visitdomproc-macsavecaptiontotext';
  VISITDOMPROC_MACSAVECAPTIONTOTEXT_COMPLETE = 'visitdomproc-macsavecaptiontotext-complete';
  VISITDOMPROC_PRINTCAPTION = 'visitdomproc-printcaption';
  VISITDOMPROC_PRINTCAPTION_COMPLETE = 'visitdomproc-printcaption-complete';
  VISITDOMPROC_DOMVISITORCALLBACK = 'visitdomproc-domvisitorcallback';
  VISITDOMPROC_DOMVISITORCALLBACK_COMPLETE = 'visitdomproc-domvisitorcallback-complete';
  VISITDOMPROC_PLACEHOLDERLOADED = 'visitdomproc-placeholderloaded';
  VISITDOMPROC_PLACEHOLDERLOADED_COMPLETE = 'visitdomproc-placeholderloaded-complete';

  VISITDOMPROC_DISTEXPORTOPTIONS = 'visitdomproc-fetchdistmatrixexportoptions';
  VISITDOMPROC_DISTEXPORTOPTIONS_COMPLETE = 'visitdomproc-fetchdistmatrixexportoptions-complete';
  VISITDOMPROC_SEQDATAEXPORTOPTIONS = 'visitdomproc-seqdataexportoptions';
  VISITDOMPROC_SEQDATAEXPORTOPTIONS_COMPLETE = 'visitdomproc-seqdataexportoptions-complete';
  VISITDOMPROC_CLUSTALDNAOPTIONS = 'visitdomproc-clustaldnaoptions';
  VISITDOMPROC_CLUSTALDNAOPTIONS_COMPLETE = 'visitdomproc-clustaldnaoptions-complete';
  VISITDOMPROC_CLUSTALCODINGOPTIONS = 'visitdomproc-clustalcodingoptions';
  VISITDOMPROC_CLUSTALCODINGOPTIONS_COMPLETE = 'visitdomproc-clustalcodingoptions-complete';
  VISITDOMPROC_CLUSTALPROTEINOPTIONS = 'visitdomproc-clustalproteinoptions';
  VISITDOMPROC_CLUSTALPROTEINOPTIONS_COMPLETE = 'visitdomproc-clustalproteinoptions-complete';

  VISITDOMPROC_INPUT_DATA_OPTIONS = 'visitdomproc-input-data-options';
  VISITDOMPROC_INPUT_DATA_OPTIONS_COMPLETE = 'visitdomproc-input-data-options-complete';

  VISITDOMPROC_SUBTREE_DRAWING_OPTIONS = 'visitdomproc-subtreedrawingoptions';
  VISITDOMPROC_SUBTREE_DRAWING_OPTIONS_COMPLETE = 'visitdomproc-subtreedrawingoptions-complete';
  VISITDOMPROC_TREE_OPTIONS_TREE = 'visitdomproc-treeoptions';
  VISITDOMPROC_TREE_OPTIONS_TREE_COMPLETE = 'visitdomproc-treeoptions-complete';
  VISITDOMPROC_TREE_OPTIONS_BRANCH = 'visitdomproc-treebranchoptions';
  VISITDOMPROC_TREE_OPTIONS_BRANCH_COMPLETE = 'visitdomproc-treebranchoptions-complete';
  VISITDOMPROC_TREE_OPTIONS_SCALE = 'visitdomproc-treescaleoptions';
  VISITDOMPROC_TREE_OPTIONS_SCALE_COMPLETE = 'visitdomproc-treescaleoptions-complete';
  VISITDOMPROC_TREE_OPTIONS_LABELS = 'visitdomproc-treelabeloptions';
  VISITDOMPROC_TREE_OPTIONS_LABELS_COMPLETE = 'visitdomproc-treelabeloptions-complete';
  VISITDOMPROC_TREE_OPTIONS_CUTOFF = 'visitdomproc-treecutoffoptions';
  VISITDOMPROC_TREE_OPTIONS_CUTOFF_COMPLETE = 'visitdomproc-treecutoffoptions-complete';
  VISITDOMPROC_ALIGNMENT_MODE = 'visitdomproc-alignment-mode';
  VISITDOMPROC_ALIGNMENT_MODE_COMPLETE = 'visitdomproc-alignment-mode-complete';
  VISITDOMPROC_CHECK_FORM_VALIDATED = 'visitdomproc-check-form-validated';
  VISITDOMPROC_CHECK_FORM_VALIDATED_COMPLETE = 'visitdomproc-check-form-validated-complete';
  VISITDOMPROC_NEWICK_EXPORT_OPTIONS = 'visitdomproc-newick-export-options';
  VISITDOMPROC_NEWICK_EXPORT_OPTIONS_COMPLETE = 'visitdomproc-newick-export-options-complete';
  VISITDOMPROC_ALIGN_EDIT_WELCOME_OPTIONS = 'visitdomproc-align-edit-welcome-options';
  VISITDOMPROC_ALIGN_EDIT_WELCOME_OPTIONS_COMPLETE = 'visitdomproc-align-edit-welcome-options-complete';
  VISITDOMPROC_SEQNAME_OPTIONS = 'visitdomproc-seqname-options';
  VISITDOMPROC_SEQNAME_OPTIONS_COMPLETE = 'visitdomproc-seqname-options-complete';
  VISITDOMPROC_JS_MESSAGE_DIALOG = 'visitdomproc-js-message-dialog';
  VISITDOMPROC_JS_MESSAGE_DIALOG_COMPLETE = 'visitdomproc-js-message-dialog-complete';
  VISITDOMPROC_SELECT_CDS_TO_IMPORT = 'visitdomproc-select-cds-to-import';
  VISITDOMPROC_SELECT_CDS_TO_IMPORT_COMPLETE = 'visitdomproc-select-cds-to-import-complete';
  VISITDOMPROC_APPDEPOT_GET_SELECTIONS = 'visitdomproc_appdepot_get_selections';
  VISITDOMPROC_APPDEPOT_GET_SELECTIONS_COMPLETE = 'visitdomproc_appdepot_get_selections_complete';

  IS_VALIDATED = 'is_validated';
  IS_VALIDATED_INDEX = 0;
  CDS_FIELDSET = 'cds_fieldset';
  CDS_IMPORT_ONLY_CDS = 'import_only_cds';
  CDS_IMPORT_ALL_DATA = 'import_all_data';
  CDS_IMPORT_ONLY_CDS_INDEX = 0;
  CDS_IMPORT_ALL_DATA_INDEX = 1;

  { edit build alignment dialog options}
  CREATE_NEW_ALIGNMENT = 'create_new_alignment';
  CREATE_NEW_ALIGNMENT_INDEX = 0;
  OPEN_SAVED_ALIGNMENT = 'open_saved_alignment';
  OPEN_SAVED_ALIGNMENT_INDEX = 1;
  RETRIEVE_SEQUENCE_FROM_FILE = 'retrieve_sequence_from_file';
  RETRIEVE_SEQUENCE_FROM_FILE_INDEX = 2;

  { newick export options}
  NEWICK_BRANCH_LENGTHS = 'newick_branch_lengths';
  NEWICK_BOOTSTRAP_VALUES = 'newick_bootstrap_values';
  NEWICK_NODE_LABELS = 'newick_node_labels';
  NEWICK_GENE_DUPLICATIONS = 'newick_gene_duplications';
  NEWICK_SPECIATIONS = 'newick_speciations';
  NEWICK_RELATIVE_TIMES = 'newick_relative_times';
  NEWICK_DIVERGENCE_TIMES = 'newick_divergence_times';
  NEWICK_BRANCH_LENGTHS_INDEX = 0;
  NEWICK_BOOTSTRAP_VALUES_INDEX = 1;
  NEWICK_NODE_LABELS_INDEX = 2;
  NEWICK_GENE_DUPLICATIONS_INDEX = 3;
  NEWICK_SPECIATIONS_INDEX = 4;
  NEWICK_RELATIVE_TIMES_INDEX = 5;
  NEWICK_DIVERGENCE_TIMES_INDEX = 6;

  { input data dialog options}
  DNA_MISSING_DATA = 'dna_missing_data';
  DNA_ALIGNMENT_GAP = 'dna_alignment_gap';
  DNA_IDENTICAL_SYMBOL = 'dna_identical_symbol';
  PROTEIN_MISSING_DATA = 'protein_missing_data';
  PROTEIN_ALIGNMENT_GAP = 'protein_alignment_gap';
  PROTEIN_IDENTICAL_SYMBOL = 'protein_identical_symbol';
  PAIRWISE_MISSING_DATA = 'pairwise_missing_data';
  LOWER_LEFT_MATRIX = 'lower_left_matrix';
  UPPER_RIGHT_MATRIX = 'upper_right_matrix';
  NUCLEOTIDE_SEQUENCE = 'nucleotide_sequence_li';
  PROTEIN_SEQUENCE = 'protein_sequence_li';
  PAIRWISE_DISTANCE = 'pairwise_distance_li';

  ISSELECTED = 'isselected';

  DNA_MISSING_DATA_INDEX = 0;
  DNA_ALIGNMENT_GAP_INDEX = 1;
  DNA_IDENTICAL_SYMBOL_INDEX = 2;
  PROTEIN_MISSING_DATA_INDEX = 3;
  PROTEIN_ALIGNMENT_GAP_INDEX = 4;
  PROTEIN_IDENTICAL_SYMBOL_INDEX = 5;
  PAIRWISE_MISSING_DATA_INDEX = 6;
  LOWER_LEFT_MATRIX_INDEX = 7;
  UPPER_RIGHT_MATRIX_INDEX = 8;
  NUCLEOTIDE_SEQUENCE_INDEX = 9;
  PROTEIN_SEQUENCE_INDEX = 10;
  PAIRWISE_DISTANCE_INDEX = 11;
  IS_CODING_INDEX = 12;

  {subtree options}
  SUBTREE_OPTIONS_DISPLAY_FONT = 'subtree_options_display_font';
  SUBTREE_OPTIONS_CAPTION_FONT = 'subtree_options_caption_font';
  SUBTREE_OPTIONS_NAME_CAPTION = 'subtree_options_name_caption';
  SUBTREE_OPTIONS_NODE_SHAPE = 'subtree_options_node_shape';
  SUBTREE_COLOR1 = 'subtree_color';
  SUBTREE_OPTIONS_APPLY_TO_TAXON_MARKERS = 'subtree_options_apply_to_taxon_markers';
  SUBTREE_OPTIONS_BRANCH_LINES = 'subtree_options_branch_lines';
  SUBTREE_COLOR2 = 'subtree_color2';
  SUBTREE_OPTIONS_BRANCH_WIDTH = 'subtree_options_branch_width';
  SUBTREE_OPTIONS_BRANCH_STYLE = 'subtree_options_branch_style';
  SUBTREE_OPTIONS_DISPLAY_CAPTION = 'subtree_options_display_caption';
  SUBTREE_OPTIONS_ALIGN_VERT = 'subtree_options_align_vertically';
  SUBTREE_OPTIONS_DISPLAY_BRACKET = 'subtree_options_display_bracket';
  SUBTREE_OPTIONS_BRACKET_STYLE = 'subtree_options_bracket_style';
  SUBTREE_COLOR3 = 'subtree_color3';
  SUBTREE_DRAWING_OPTIONS_LINE_WIDTH = 'subtree_options_display_line_width';
  SUBTREE_OPTIONS_DISPLAY_TAXON_NAME = 'subtree_options_display_taxon_name';
  SUBTREE_OPTIONS_DISPLAY_NODE = 'subtree_options_display_node';
  SUBTREE_OPTIONS_DISPLAY_TAXON_MARKER = 'subtree_options_display_taxon_marker';
  SUBTREE_OPTIONS_COMPRESS = 'subtree_options_compress';
  SUBTREE_DRAWING_OPTIONS_VERTICAL_UNIT = 'subtree_drawing_options_vertical_unit';
  SUBTREE_OPTIONS_FILL_PATTERN = 'subtree_options_fill_pattern';
  SUBTREE_OPTIONS_DISPLAY_IMAGE = 'subtree_options_display_image';
  SUBTREE_OPTIONS_STYLE_OPTIONS = 'subtree_options_image_options';
  SUBTREE_OPTIONS_OVERWRITE = 'subtree_options_overwrite';
  SUBTREE_OPTIONS_IMAGE = 'subtree_options_image';
  SUBTREE_OPTIONS_IMAGE_PATH = 'subtree_options_image_path';
  SUBTREE_OPTIONS_CLEAR_IMAGE = 'subtree_options_clear_image_button';
  SUBTREE_OPTIONS_EXPORT_IMAGE = 'subtree_options_export_image_button';
  SUBTREE_COLOR4 = 'subtree_color4';
  SUBTREE_OPTIONS_GROUP_NAME = 'group_name';
  SUBTREE_OPTIONS_NODE_ID = 'node_id';
  SUBTREE_OPTIONS_DEFAULTS = 'subtree_options_defaults';


  SUBTREE_OPTIONS_DISPLAY_FONT_INDEX = 0;
  SUBTREE_OPTIONS_CAPTION_FONT_INDEX = 1;
  SUBTREE_OPTIONS_NAME_CAPTION_INDEX = 2;
  SUBTREE_OPTIONS_NODE_SHAPE_INDEX = 3;
  SUBTREE_COLOR1_INDEX = 4;
  SUBTREE_OPTIONS_APPLY_TO_TAXON_MARKERS_INDEX = 5;
  SUBTREE_OPTIONS_BRANCH_LINES_INDEX = 6;
  SUBTREE_COLOR2_INDEX = 7;
  SUBTREE_OPTIONS_BRANCH_WIDTH_INDEX = 8;
  SUBTREE_OPTIONS_BRANCH_STYLE_INDEX = 9;
  SUBTREE_OPTIONS_DISPLAY_CAPTION_INDEX = 10;
  SUBTREE_OPTIONS_ALIGN_VERT_INDEX = 11;
  SUBTREE_OPTIONS_DISPLAY_BRACKET_INDEX = 12;
  SUBTREE_OPTIONS_BRACKET_STYLE_INDEX = 13;
  SUBTREE_COLOR3_INDEX = 14;
  SUBTREE_DRAWING_OPTIONS_LINE_WIDTH_INDEX = 15;
  SUBTREE_OPTIONS_DISPLAY_TAXON_NAME_INDEX = 16;
  SUBTREE_OPTIONS_DISPLAY_NODE_INDEX = 17;
  SUBTREE_OPTIONS_DISPLAY_TAXON_MARKER_INDEX = 18;
  SUBTREE_OPTIONS_COMPRESS_INDEX = 19;
  SUBTREE_DRAWING_OPTIONS_VERTICAL_UNIT_INDEX = 20;
  SUBTREE_OPTIONS_FILL_PATTERN_INDEX = 21;
  SUBTREE_OPTIONS_DISPLAY_IMAGE_INDEX = 22;
  SUBTREE_OPTIONS_STYLE_OPTIONS_INDEX = 23;
  SUBTREE_OPTIONS_OVERWRITE_INDEX = 24;
  SUBTREE_OPTIONS_IMAGE_INDEX = 25;
  SUBTREE_OPTIONS_IMAGE_PATH_INDEX = 26;
  SUBTREE_OPTIONS_CLEAR_IMAGE_INDEX = 27;
  SUBTREE_OPTIONS_EXPORT_IMAGE_INDEX = 28;
  SUBTREE_COLOR4_INDEX = 29;
  SUBTREE_OPTIONS_GROUP_NAME_INDEX = 30;
  SUBTREE_OPTIONS_NODE_ID_INDEX = 31;
  SUBTREE_OPTIONS_DEFAULTS_INDEX = 32;

  { tree options}
  TREE_OPTIONS_RECT_TAXON_SEPARATION = 'tree_options_rect_taxon_separation';
  TREE_OPTIONS_RECT_WIDTH = 'tree_options_rect_width';
  TREE_OPTIONS_TREE_TAXON_NAME_CIRCLE = 'tree_options_tree_taxon_name_circle';
  TREE_OPTIONS_CIRCLE_START_ANGLE = 'tree_options_circle_start_angle';
  TREE_OPTIONS_CIRCLE_RADIUS = 'tree_options_circle_radius';
  TREE_OPTIONS_CIRCLE_CENTER_HOLE = 'tree_options_circle_center_hole';
  TREE_OPTIONS_TREE_TAXON_NAME_RAD = 'tree_options_tree_taxon_name_rad';
  TREE_OPTIONS_RAD_BRANCH_LENGTH = 'tree_options_rad_branch_length';
  TREE_OPTIONS_RAD_START_ANGLE = 'tree_options_rad_start_angle';

  TREE_OPTIONS_RECT_TAXON_SEPARATION_INDEX = 0;
  TREE_OPTIONS_RECT_WIDTH_INDEX = 1;
  TREE_OPTIONS_TREE_TAXON_NAME_CIRCLE_INDEX = 2;
  TREE_OPTIONS_CIRCLE_START_ANGLE_INDEX = 3;
  TREE_OPTIONS_CIRCLE_RADIUS_INDEX = 4;
  TREE_OPTIONS_CIRCLE_CENTER_HOLE_INDEX = 5;
  TREE_OPTIONS_TREE_TAXON_NAME_RAD_INDEX = 6;
  TREE_OPTIONS_RAD_BRANCH_LENGTH_INDEX = 7;
  TREE_OPTIONS_RAD_START_ANGLE_INDEX = 8;


  { tree branch options}
  TREE_OPTIONS_BRANCH_LINES = 'tree_options_branch_lines';
  TREE_OPTIONS_BRANCH_DISPLAY_STATS = 'tree_options_branch_display_stats';
  TREE_OPTIONS_BRANCH_STAT_PLACEMENT = 'tree_options_branch_stat_placement';
  TREE_OPTIONS_BRANCH_HORIZONTAL = 'tree_options_branch_horizontal';
  TREE_OPTIONS_BRANCH_VERTICAL = 'tree_options_branch_vertical';
  TREE_OPTIONS_BRANCH_HIDE_LOWER = 'tree_options_branch_hide_lower';
  TREE_OPTIONS_BRANCH_HIDE_VALUES = 'tree_options_branch_hide_values';
  TREE_OPTIONS_BRANCH_DISPLAY_BRANCH = 'tree_options_branch_display_branch';
  TREE_OPTIONS_BRANCH_BRANCH_PLACEMENT = 'tree_options_branch_branch_placement';
  TREE_OPTIONS_BRANCH_PRECISION = 'tree_options_branch_precision';
  TREE_OPTIONS_BRANCH_HIDE_SHORTER = 'tree_options_branch_hide_shorter';
  TREE_OPTIONS_HIDE_SHORTER = 'tree_options_hide_shorter';
  TREE_OPTIONS_BRANCH_DISPLAY_DIVERGENCE = 'tree_options_branch_display_divergence';
  TREE_OPTIONS_BRANCH_TIME_PLACEMENT = 'tree_options_branch_time_placement';
  TREE_OPTIONS_BRANCH_DIVERGENCE_PRECISION = 'tree_options_branch_divergence_precision';
  TREE_OPTIONS_BRANCH_DISTANCE_HORIZONTAL = 'tree_options_branch_distance_horizontal';
  TREE_OPTIONS_BRANCH_DISTANCE_VERTICAL = 'tree_options_branch_distance_vertical';
  TREE_OPTIONS_BRANCH_STATISTICS_FONT = 'tree_options_branch_statistics_font';
  TREE_OPTIONS_BRANCH_LENGTH_FONT = 'tree_options_branch_length_font';
  TREE_OPTIONS_BRANCH_DIVERGENCE_FONT = 'tree_options_branch_divergence_font';

  TREE_OPTIONS_BRANCH_LINES_INDEX = 0;
  TREE_OPTIONS_BRANCH_DISPLAY_STATS_INDEX = 1;
  TREE_OPTIONS_BRANCH_STAT_PLACEMENT_INDEX = 2;
  TREE_OPTIONS_BRANCH_HORIZONTAL_INDEX = 3;
  TREE_OPTIONS_BRANCH_VERTICAL_INDEX = 4;
  TREE_OPTIONS_BRANCH_HIDE_LOWER_INDEX = 5;
  TREE_OPTIONS_BRANCH_HIDE_VALUES_INDEX = 6;
  TREE_OPTIONS_BRANCH_DISPLAY_BRANCH_INDEX = 7;
  TREE_OPTIONS_BRANCH_BRANCH_PLACEMENT_INDEX = 8;
  TREE_OPTIONS_BRANCH_PRECISION_INDEX = 9;
  TREE_OPTIONS_BRANCH_HIDE_SHORTER_INDEX = 10;
  TREE_OPTIONS_HIDE_SHORTER_INDEX = 11;
  TREE_OPTIONS_BRANCH_DISPLAY_DIVERGENCE_INDEX = 12;
  TREE_OPTIONS_BRANCH_TIME_PLACEMENT_INDEX = 13;
  TREE_OPTIONS_BRANCH_DIVERGENCE_PRECISION_INDEX = 14;
  TREE_OPTIONS_BRANCH_DISTANCE_HORIZONTAL_INDEX = 15;
  TREE_OPTIONS_BRANCH_DISTANCE_VERTICAL_INDEX = 16;
  TREE_OPTIONS_BRANCH_STATISTICS_FONT_INDEX = 17;
  TREE_OPTIONS_BRANCH_LENGTH_FONT_INDEX = 18;
  TREE_OPTIONS_BRANCH_DIVERGENCE_FONT_INDEX = 19;

  { tree cutoff options}
  TREE_OPTIONS_CUTOFF_CONDENSED = 'tree_options_cutoff_condensed';
  TREE_OPTIONS_CUTOFF_CONSENSUS = 'tree_options_cutoff_consensus';

  TREE_OPTIONS_CUTOFF_CONDENSED_INDEX = 0;
  TREE_OPTIONS_CUTOFF_CONSENSUS_INDEX = 1;


  { tree label options}
  TREE_OPTIONS_LABELS_DISPLAY_TAXON_NAMES = 'tree_options_labels_display_taxon_names';
  TREE_OPTIONS_LABELS_DISPLAY_TAXON_MARKERS = 'tree_options_labels_display_taxon_markers';
  TAXA_NAMES_LIST = 'taxon_markers_list';
  TREE_OPTIONS_BRANCH_LABELS_FONT = 'tree_options_branch_labels_font';
  TREE_OPTIONS_BRANCH_LABELS_COLOR = 'tree_options_branch_labels_color';
  MARKER_INFO = 'marker_info';

  MARKER_SHAPE = 'marker_shape';
  MARKER_COLOR = 'color';

  TREE_OPTIONS_LABELS_DISPLAY_TAXON_NAMES_INDEX = 0;
  TREE_OPTIONS_LABELS_DISPLAY_TAXON_MARKERS_INDEX = 1;
  TAXA_NAMES_LIST_INDEX = 2;
  TREE_OPTIONS_BRANCH_LABELS_FONT_INDEX = 3;
  MARKER_INFO_INDEX = 4;
  TREE_OPTIONS_BRANCH_LABELS_COLOR_INDEX = 5;

  { tree scale options}
  TREE_OPTIONS_SCALE_LINES = 'tree_options_scale_lines';
  TREE_OPTIONS_SCALE_DISTANCE_SCALE = 'tree_options_scale_distance_scale';
  TREE_OPTIONS_SCALE_NAME_CAPTION_DISTANCE_SCALE = 'tree_options_scale_name_caption_distance_scale';
  TREE_OPTIONS_DISTANCE_SCALE_LENGTH = 'tree_options_distance_scale_length';
  TREE_OPTIONS_DISTANCE_TICK_INTERVAL = 'tree_options_distance_tick_interval';
  TREE_OPTIONS_SCALE_TIME_SCALE = 'tree_options_scale_time_scale';
  TREE_OPTIONS_SCALE_NAME_CAPTION_TIME_SCALE = 'tree_options_scale_name_caption_time_scale';
  TREE_OPTIONS_SCALE_MAJOR_TICK = 'tree_options_scale_major_tick';
  TREE_OPTIONS_SCALE_MINOR_TICK = 'tree_options_scale_minor_tick';
  TREE_OPTIONS_SCALE_NODE_HEIGHT_ERR = 'tree_options_scale_node_height_err';
  TREE_OPTIONS_SCALE_FONT = 'tree_options_scale_font';

  TREE_OPTIONS_SCALE_LINES_INDEX = 0;
  TREE_OPTIONS_SCALE_DISTANCE_SCALE_INDEX = 1;
  TREE_OPTIONS_SCALE_NAME_CAPTION_DISTANCE_SCALE_INDEX = 2;
  TREE_OPTIONS_DISTANCE_SCALE_LENGTH_INDEX = 3;
  TREE_OPTIONS_DISTANCE_TICK_INTERVAL_INDEX = 4;
  TREE_OPTIONS_SCALE_TIME_SCALE_INDEX = 5;
  TREE_OPTIONS_SCALE_NAME_CAPTION_TIME_SCALE_INDEX = 6;
  TREE_OPTIONS_SCALE_MAJOR_TICK_INDEX = 7;
  TREE_OPTIONS_SCALE_MINOR_TICK_INDEX = 8;
  TREE_OPTIONS_SCALE_NODE_HEIGHT_ERR_INDEX = 9;
  TREE_OPTIONS_SCALE_FONT_INDEX = 10;


  { ClustalW DNA formatting options}
  CLUSTALW_DNA_PAIRWISE_GAP_OPENING_PENALTY = 'clustalw_dna_pairwise_gap_opening_penalty';
  CLUSTALW_DNA_PAIRWISE_GAP_EXTENSION_PENALTY = 'clustalw_dna_pairwise_gap_extension_penalty';
  CLUSTALW_DNA_MULTIPLE_GAP_OPENING_PENALTY = 'clustalw_dna_multiple_gap_opening_penalty';
  CLUSTALW_DNA_MULTIPLE_GAP_EXTENSION_PENALTY = 'clustalw_dna_multiple_gap_extension_penalty';
  SELECT_DNA_WEIGHT_MATRIX = 'select_dna_weight_matrix';
  CLUSTALW_DNA_TRANSITION_WEIGHT = 'clustalw_dna_transition_weight';
  CLUSTALW_DNA_USE_NEGATIVE_MATRIX = 'clustalw_dna_use_negative_matrix';
  CLUSTALW_DNA_DIVERGENT_CUTOFF = 'clustalw_dna_divergent_cutoff';
  CLUSTALW_DNA_PREDEFINED_GAP = 'clustalw_dna_predefined_gap';
  CLUSTALW_DNA_UPLOAD_TREE = 'clustalw_dna_upload_guide_tree_file';

  CLUSTALW_DNA_PAIRWISE_GAP_OPENING_PENALTY_INDEX = 0;
  CLUSTALW_DNA_PAIRWISE_GAP_EXTENSION_PENALTY_INDEX = 1;
  CLUSTALW_DNA_MULTIPLE_GAP_OPENING_PENALTY_INDEX = 2;
  CLUSTALW_DNA_MULTIPLE_GAP_EXTENSION_PENALTY_INDEX = 3;
  SELECT_DNA_WEIGHT_MATRIX_INDEX = 4;
  CLUSTALW_DNA_TRANSITION_WEIGHT_INDEX = 5;
  CLUSTALW_DNA_USE_NEGATIVE_MATRIX_INDEX = 6;
  CLUSTALW_DNA_DIVERGENT_CUTOFF_INDEX = 7;
  CLUSTALW_DNA_PREDEFINED_GAP_INDEX = 8;
  CLUSTALW_DNA_UPLOAD_TREE_INDEX = 9;

  { ClustalW Protein formatting options}
  CLUSTALW_PROTEIN_PAIRWISE_GAP_OPENING_PENALTY = 'clustalw_protein_pairwise_gap_opening_penalty';
  CLUSTALW_PROTEIN_PAIRWISE_GAP_EXTENSION_PENALTY = 'clustalw_protein_pairwise_gap_extension_penalty';
  CLUSTALW_PROTEIN_MULTIPLE_GAP_OPENING_PENALTY = 'clustalw_protein_multiple_gap_opening_penalty';
  CLUSTALW_PROTEIN_MULTIPLE_GAP_EXTENSION_PENALTY = 'clustalw_protein_multiple_gap_extension_penalty';
  CLUSTALW_PROTEIN_WEIGHT_MATRIX = 'clustalw_protein_protein_weight_matrix';
  CLUSTALW_PROTEIN_RESIDUE_SPECIFIC_PENALTIES = 'clustalw_protein_residue_specific_pentalties';
  CLUSTALW_PROTEIN_HYDROPHILIC_PENALTIES = 'clustalw_protein_hydrophilic_penalties';
  CLUSTALW_PROTEIN_GAP_SEPARATION = 'clustalw_protein_gap_separation';
  CLUSTALW_PROTEIN_USE_NEGATIVE_MATRIX = 'clustalw_protein_use_negative_matrix';
  CLUSTALW_PROTEIN_DIVERGENT_CUTOFF = 'clustalw_protein_divergent_cutoff';
  CLUSTALW_PROTEIN_PREDEFINED_GAP = 'clustalw_protein_predefined_gap';
  CLUSTALW_PROTEIN_UPLOAD_TREE = 'clustalw_protein_upload_guide_tree_file';
  CLUSTALW_PROTEIN_END_GAP_SEPARATION = 'clustalw_protein_end_gap_separation';

  CLUSTALW_PROTEIN_PAIRWISE_GAP_OPENING_PENALTY_INDEX = 0;
  CLUSTALW_PROTEIN_PAIRWISE_GAP_EXTENSION_PENALTY_INDEX = 1;
  CLUSTALW_PROTEIN_MULTIPLE_GAP_OPENING_PENALTY_INDEX = 2;
  CLUSTALW_PROTEIN_MULTIPLE_GAP_EXTENSION_PENALTY_INDEX = 3;
  CLUSTALW_PROTEIN_WEIGHT_MATRIX_INDEX = 4;
  CLUSTALW_PROTEIN_RESIDUE_SPECIFIC_PENALTIES_INDEX = 5;
  CLUSTALW_PROTEIN_HYDROPHILIC_PENALTIES_INDEX = 6;
  CLUSTALW_PROTEIN_GAP_SEPARATION_INDEX = 7;
  CLUSTALW_PROTEIN_USE_NEGATIVE_MATRIX_INDEX = 8;
  CLUSTALW_PROTEIN_DIVERGENT_CUTOFF_INDEX = 9;
  CLUSTALW_PROTEIN_PREDEFINED_GAP_INDEX = 10;
  CLUSTALW_PROTEIN_UPLOAD_TREE_INDEX = 11;
  CLUSTALW_PROTEIN_END_GAP_SEPARATION_INDEX = 12;

  { ClustalW coding alignment formatting options}
  CLUSTALW_CODONS_PAIRWISE_GAP_OPENING_PENALTY = 'clustalw_codons_pairwise_gap_opening_penalty';
  CLUSTALW_CODONS_PAIRWISE_GAP_EXTENSION_PENALTY = 'clustalw_codons_pairwise_gap_extension_penalty';
  CLUSTALW_CODONS_MULTIPLE_GAP_OPENING_PENALTY = 'clustalw_codons_multiple_gap_opening_penalty';
  CLUSTALW_CODONS_MULTIPLE_GAP_EXTENSION_PENALTY = 'clustalw_codons_multiple_gap_extension_penalty';
  CLUSTALW_CODONS_PROTEIN_WEIGHT_MATRIX = 'clustalw_codons_protein_weight_matrix';
  CLUSTALW_CODONS_RESIDUE_SPECIFIC_PENALTIES = 'clustalw_codons_residue_specific_pentalties';
  CLUSTALW_CODONS_HYDROPHILIC_PENALTIES = 'clustalw_codons_hydrophilic_penalties';
  CLUSTALW_CODONS_GAP_SEPARATION = 'clustalw_codons_gap_separation';
  CLUSTALW_CODONS_END_GAP_SEPARATION = 'clustalw_codons_end_gap_separation';
  SELECT_GENETIC_CODE = 'select_genetic_code';
  SELECT_GENETIC_CODE_TABLE = 'select_genetic_code';
  GENETIC_CODE_VIEW_EXPORT_TYPE = 'clustalw_genetic_code_view_export_type';
  GENETIC_CODE_STATS_EXPORT_TYPE = 'clustalw_genetic_code_stats_export_type';
  CLUSTALW_GENETIC_CODE_EXPORT_TABLE = 'clustalw_genetic_code_export_table';
  CLUSTALW_GENETIC_CODE_EXPORT_STATISTICS = 'clustalw_genetic_code_export_statistics';

  CLUSTALW_CODONS_USE_NEGATIVE_MATRIX = 'clustalw_codons_use_negative_matrix';
  CLUSTALW_CODONS_DIVERGENT_CUTOFF = 'clustalw_codons_divergent_cutoff';
  CLUSTALW_CODONS_PREDEFINED_GAP = 'clustalw_codons_predefined_gap';
  CLUSTALW_CODONS_UPLOAD_TREE = 'clustalw_codons_upload_guide_tree_file';

  CLUSTALW_CODONS_PAIRWISE_GAP_OPENING_PENALTY_INDEX = 0;
  CLUSTALW_CODONS_PAIRWISE_GAP_EXTENSION_PENALTY_INDEX = 1;
  CLUSTALW_CODONS_MULTIPLE_GAP_OPENING_PENALTY_INDEX = 2;
  CLUSTALW_CODONS_MULTIPLE_GAP_EXTENSION_PENALTY_INDEX = 3;
  CLUSTALW_CODONS_PROTEIN_WEIGHT_MATRIX_INDEX = 4;
  CLUSTALW_CODONS_RESIDUE_SPECIFIC_PENALTIES_INDEX = 5;
  CLUSTALW_CODONS_HYDROPHILIC_PENALTIES_INDEX = 6;
  CLUSTALW_CODONS_GAP_SEPARATION_INDEX = 7;
  CLUSTALW_CODONS_END_GAP_SEPARATION_INDEX = 8;
  SELECT_GENETIC_CODE_INDEX = 9;
  CLUSTALW_CODONS_USE_NEGATIVE_MATRIX_INDEX = 10;
  CLUSTALW_CODONS_DIVERGENT_CUTOFF_INDEX = 11;
  CLUSTALW_CODONS_PREDEFINED_GAP_INDEX = 12;
  CLUSTALW_CODONS_UPLOAD_TREE_INDEX = 13;
  SELECT_GENETIC_CODE_TABLE_INDEX = 14;
  CLUSTALW_GENETIC_CODE_EXPORT_TABLE_INDEX = 15;
  CLUSTALW_GENETIC_CODE_EXPORT_STATISTICS_INDEX = 16;
  GENETIC_CODE_VIEW_EXPORT_TYPE_INDEX = 17;
  GENETIC_CODE_STATS_EXPORT_TYPE_INDEX = 18;


  { DMEO is for Distance Matrix Export Options}
  DMEO_FILE_FORMAT_ID = 'dist_export_file_format';
  DMEO_EXPORT_TYPE_ID = 'dist_export_type';
  DMEO_MATRIX_LAYOUT1_ID = 'dist_matrix_layout1';
  DMEO_MATRIX_LAYOUT2_ID = 'dist_matrix_layout2';
  DMEO_SHOW_DISTANCES_ID = 'distances';
  DMEO_SHOW_DISTANCES_AND_STD_ERRORS_ID = 'std_errors';
  DMEO_IS_CHECKED = 'ischecked';
  DMEO_PRECISION_ID = 'dist_matrix_precision';
  DMEO_SHOW_DISTANCES_AND_STD_ERRORS_DIV_ID = 'std_errors_div';


  DMEO_FILE_FORMAT_INDEX = 0;
  DMEO_EXPORT_TYPE_INDEX = 1;
  DMEO_MATRIX_LAYOUT1_INDEX = 2;
  DMEO_MATRIX_LAYOUT2_INDEX = 3;
  DMEO_SHOW_DISTANCES_INDEX = 4;
  DMEO_SHOW_DISTANCES_AND_STD_ERRORS_INDEX = 5;
  DMEO_PRECISION_INDEX = 6;

  DMEO_MATRIX = 'matrix';
  DMEO_COLUMN = 'column';
  DMEO_OPPOSITE_SIDES = 'opposite_sides';
  DMEO_DIST_STD_ERRS = 'dist_and_error';
  DMEO_UPPER_RIGHT = 'upper_right';
  DMEO_LOWER_LEFT = 'lower_left';

  { SDEO is for Sequence Data Export Options}
  SDEO_SEQDATA_TITLE_ID = 'seqdata_export_title';
  SDEO_SEQDATA_DESCRIPTION_ID = 'seqdata_export_description';
  SDEO_FILE_FORMAT_ID = 'seqdata_export_format';
  SDEO_SITES_PER_LINE_ID = 'sites_per_line';
  SDEO_IS_INTERLEAVED_ID = 'is_interleaved';
  SDEO_WRITING_SITE_NUMBERS_ID = 'seqdata_export_site_numbers';
  SDEO_INCLUDE_FIRST_POSITION_ID = 'first_position';
  SDEO_INCLUDE_SECOND_POSITION_ID = 'second_position';
  SDEO_INCLUDE_THIRD_POSITION_ID = 'third_position';
  SDEO_INCLUDE_NONCODING_POSITIONS_ID = 'noncoding_positions';
  SDEO_GAP_TREATMENT_ID = 'seqdata_export_gap_treatment';
  SDEO_INCLUDED_SITES_ID = 'seqdata_export_included_sites';
  SDEO_FULL_CODONS_ID = 'full_codons';
  SITES_TO_INCLUDE_BOX = 'sites_to_include_box';

  SDEO_SEQDATA_TITLE_INDEX = 0;
  SDEO_SEQDATA_DESCRIPTION_INDEX = 1;
  SDEO_FILE_FORMAT_INDEX = 2;
  SDEO_SITES_PER_LINE_INDEX = 3;
  SDEO_IS_INTERLEAVED_INDEX = 4;
  SDEO_WRITING_SITE_NUMBERS_INDEX = 5;
  SDEO_INCLUDE_FIRST_POSITION_INDEX = 6;
  SDEO_INCLUDE_SECOND_POSITION_INDEX = 7;
  SDEO_INCLUDE_THIRD_POSITION_INDEX = 8;
  SDEO_INCLUDE_NONCODING_POSITIONS_INDEX = 9;
  SDEO_GAP_TREATMENT_INDEX = 10;
  SDEO_INCLUDED_SITES_INDEX = 11;
  SDEO_FULL_CODONS_INDEX = 12;

  SDEO_NONE = 'None';
  SDEO_FOR_EACH_SITE = 'for Each Site';
  SDEO_AT_END_OF_LINE = 'At The End Of Line';
  SDEO_INC_ALL_SITES = 'Include Sites With Missing/Ambiguous Data And Gaps';
  SDEO_EXC_MISSING_AMBIGUOUS_GAP_SITES = 'Exclude Sites With Missing/Ambiguous Data And Gaps';
  SDEO_EXC_MISSING_AMBIGUOUS_ONLY = 'Exclude Sites With Missing/Ambiguous Data Only';
  SDEO_EXC_GAPS_ONLY = 'Exclude Sites With Alignment Gaps Only';
  SDEO_ALL_SITES = 'All Sites';
  SDEO_ONLY_HIGHLIGHTED_SITES = 'Only Highlighted Sites';
  SDEO_ONLY_UNHIGHLIGHTED_SITES = 'Only Un-highlighted Sites';
  SDEO_HAS_HIGHLIGHTED_SITES = 'has_highlighted_sites';
  SDEO_HAS_DOMAINS = 'has_domains';
  SDEO_IS_NUC_DATA = 'is_nuc_data';
  SDEO_IS_CHOOSE_BASES = 'is_choose_bases';

  { MEGA Export Format}
  MEF_MEGA = 'MEGA';
  MEF_PAUP4 = 'Nexus (PAUP 4.0)';
  MEF_PAUP3 = 'Nexus (PAUP 3.0/MacClade)';
  MEF_PHYLIP = 'Phylip 3.0';
  MEF_FASTA = 'Fasta';
  MEF_EXCEL_XML = 'Excel Workbook (.xlsx 2007+)';
  MEF_EXCEL = 'Excel Workbook (.xls all versions)';
  MEF_ODS = 'Open/Libre Office Workbook (.ods)';
  MEF_CSV = 'CSV (Comma Separated Values)';
  MEF_TEXT = 'Text';

   {Seqname Option Dialog}
   SEQNAME_FIRST = 'seqname_first';
   SEQNAME_SECOND = 'seqname_second';
   SEQNAME_THIRD = 'seqname_third';
   SEQNAME_FOURTH = 'seqname_fourth';
   SEQNAME_USE_INITIAL = 'seqname_use_initial';
   SEQNAME_FULL_INFO = 'seqname_full_info';
   SEQNAME_SEQ_LABEL = 'seqname_seq_label';
   SEQNAME_FULL_NAME = 'full_name';
   SEQNAME_ABBR_NAME = 'abbreviated_name';
   SEQNAME_FIRST_INDEX = 0;
   SEQNAME_SECOND_INDEX = 1;
   SEQNAME_THIRD_INDEX = 2;
   SEQNAME_FOURTH_INDEX = 3;
   SEQNAME_USE_INITIAL_INDEX = 4;
   SEQNAME_FULL_INFO_INDEX = 5;
   SEQNAME_SEQ_LABEL_INDEX = 6;
   SEQNAME_FULL_NAME_INDEX = 7;
   SEQNAME_ABBR_NAME_INDEX = 8;

   { Genetic Code Table Dialog }
   VISITDOMPROC_GENETIC_CODE_TABLE_OPTIONS = 'visitdomproc-genetic-code-table-options';
   VISITDOMPROC_GENETIC_CODE_TABLE_OPTIONS_COMPLETE = 'visitdomproc-genetic-code-table-options-complete';
   SEQDATA_GENETIC_CODE_NAME = 'codeData';
   SEQDATA_GENETIC_CODE_VIEW_EXPORT_TYPE = 'genetic_code_view_option';
   SEQDATA_GENETIC_CODE_STATS_EXPORT_TYPE = 'genetic_code_output_option';
   SEQDATA_GENETIC_CODE_TABLE = 'codeData';
   SEQDATA_GENETIC_CODE_EXPORT_TABLE = 'select_genetic_code_export_table';
   SEQDATA_GENETIC_CODE_EXPORT_STATISTICS = 'select_genetic_code_export_statistics';

   SEQDATA_GENETIC_CODE_NAME_INDEX = 0;
   SEQDATA_GENETIC_CODE_VIEW_EXPORT_TYPE_INDEX = 1;
   SEQDATA_GENETIC_CODE_STATS_EXPORT_TYPE_INDEX = 2;
   SEQDATA_GENETIC_CODE_TABLE_INDEX = 3;
   SEQDATA_GENETIC_CODE_EXPORT_TABLE_INDEX = 4;
   SEQDATA_GENETIC_CODE_EXPORT_STATISTICS_INDEX = 5;

   {JS Message Dialog}
   JS_MESSAGE = 'js_message';
   JS_MESSAGE_INDEX = 0;

   HTML_PLACEHOLDER_URL = 'html.placeholder.url';
   { AppDepot}
   APPS_CONTAINER = 'apps_container';
   {$IFDEF VISUAL_BUILD}
   function HtmlPlaceholderString: String;
   {$ENDIF}

implementation

{$IFDEF VISUAL_BUILD}
function HtmlPlaceholderString: String;
var
  CSS: String;
  fontName: String;
begin
  fontName := 'Arial, Helvetica, sans-serif';
  CSS := '<style type="text/css">' +
         'body { font-family: ' + fontName + '; ' +
         'color:#bc4650;' +
         'background-color: #F2F2F2;' +
         'font-size: 14px; ' +
         'text-align: center;' +
         'margin: 20px 0px 2px 0px; }' +
         '</style>';

  Result := '<html><head>' + CSS + '</head><body>';
  Result := Result + '<h4>Please Wait...</h4>';
  Result := Result + '<p>Intializing MEGA Resources';
  Result := Result + '</body></html>';
end;
{$ENDIF}

end.

