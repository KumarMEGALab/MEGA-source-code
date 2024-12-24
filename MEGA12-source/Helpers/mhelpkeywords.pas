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

unit mhelpkeywords;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function MapHelpContextToKeyword(HelpContext: LongInt): String;

implementation

uses
  AlnBuilder_HC, CentralDialogBox_HC,
  ContextHelp_HC, DataExplorerHelp_HC, ErrorMessages_HC,
  TextEditorHelp_HC, TreeExplorer_HC, Walk_Through_MEGA;

function MapHelpContextToKeyword(HelpContext: LongInt): String;
begin
  case HelpContext of
    HC_Introduction_to_Walk_Through_MEGA: Result := 'Introduction.htm';
    HC_Alignment_Builder: Result := 'Alignment_Explorer.htm';
    HC_Toolbars_in_Alignment_Explorer: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FMenu_Items_In_Alignment_Explorer%2FToolbar_in_Alignment_Explorer.htm';
    HC_Data_Menu_in_Alignment_Explorer: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FMenu_Items_In_Alignment_Explorer%2FData_Menu.htm';
    HC_Edit_Menu_in_Alignment_Explorer: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FMenu_Items_In_Alignment_Explorer%2FEdit_Menu.htm';
    HC_Search_Menu_in_Alignment_Explorer: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FMenu_Items_In_Alignment_Explorer%2FSearch_Menu.htm';
    HC_Alignment_Menu_in_Alignment_Explorer: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FMenu_Items_In_Alignment_Explorer%2FMenu_in_Alignment_Explorer.htm';
    HC_Web_Menu_in_Alignment_Explorer: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FMenu_Items_In_Alignment_Explorer%2FWeb_Menu.htm';
    HC_Sequencer_Menu_in_Alignment_Explorer: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FMenu_Items_In_Alignment_Explorer%2FSequencer_Menu.htm';
    HC_Display_Menu_in_Alignment_Explorer: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FMenu_Items_In_Alignment_Explorer%2FDisplay_Menu.htm';
    HC_CLUSTALW_Options_DNA_Dialog: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FClustalW%2FClustalW_Options_(DNA).htm';
    HC_CLUSTALW_Options_Protein_Dialog: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FClustalW%2FClustal_Options_(Protein).htm';
    Aligning_coding_sequences_via_protein_sequences: Result := 'Part_II_Assembling_Data_For_Analysis%2FAligning_Coding_Sequences_via_Protein_Sequences.htm';
    HC_Web_Browser: Result := 'Part_II_Assembling_Data_For_Analysis%2FWeb_Browser_and_Data_Miner.htm';
    HC_About_BLAST: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FBLAST%2FAbout_BLAST.htm';
    HC_CLUSTALW: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FClustalW%2FAbout_ClustalW.htm';
    HC_Trace_Data_File_Viewer_Editor: Result := 'Part_II_Assembling_Data_For_Analysis%2FTrace_Data_File_Viewer_Editor.htm';
    About_MUSCLE: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FMUSCLE%2FAbout_Muscle.htm';
    MUSCLE_Options_DNA_: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FMUSCLE%2FMuscle_Options_(DNA).htm';
    MUSCLE_Options_Protein_: Result := 'Part_II_Assembling_Data_For_Analysis%2FBuilding_Sequence_Alignments%2FMUSCLE%2FMUSCLE_Options_(Protein).htm';
    HC_Dist_Analysis_Option_Dialog: Result := 'Analysis_Preferences_Distance_Computation.htm';
    HC_Z_test_Analysis_Options: Result := 'Analysis_Preferences_Z-test_of_Selection.htm';
    HC_Fisher_Exact_Test_Analysis_Options: Result := 'Analysis_Preferences_Fisher_s_Exact_Test.htm';
    HC_NJ_Analysis_Options: Result := 'Analysis_Preferences_NJ_UPGMA.htm';
    HC_ME_Analysis_Options: Result := 'Analysis_Preferences_Minimum_Evolution.htm';
    HC_MP_Analysis_Option_Dialog: Result := 'Analysis_Preferences_Maximum_Parsimony.htm';
    HC_Disparity_Index_Analysis_Options: Result := 'Analysis_Preferences_Pattern_Homogeneity_Analysis.htm';
    HC_ML_Analysis_Options: Result := 'Analysis_Preferences_Maximum_Likelihood.htm';
    HC_CodonOmega_Analysis_Options: Result := 'Estimate_Selection_for_each_Codon_HyPhy.htm';
    HC_Tajima_Test_Relative_Rate: Result := 'Tajima_s_Test_Relative_Rate.htm';
    HC_Tajima_Test_of_Neutrality: Result := 'Tajima_s_Test_of_Neutrality.htm';
    Mega_Basics_HC: Result := 'Part_I_Getting_Started%2FA_Walk_Through_MEGA%2FMEGA_Basics.htm';
    HC_Setup_Taxa_Groups_Dlg: Result := 'Setup_Select_Taxa_Groups_Dialog.htm';
    HC_Input_Data_Format: Result := 'Input_Data_Format_Dialog.htm';
    HC_Select_Genetic_Code_Table_Dialog: Result := 'Select_Genetic_Code_Table_Dialog.htm';
    HC_Convert_to_MEGA_Format_Dialog_Box: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FConvert_to_MEGA_Format.htm';
    HC_Analyze_and_Prototyper_Modes: Result := 'Analyze_and_Prototype_Modes.htm';
    HC_Code_Table_Editor: Result := 'Code_Table_Editor.htm';

    // SequenceDataExplorer
    HC_Data_Menu_In_Sequence_Data_Explorer: Result := 'Data_Menu_(Sequence_Data_Explorer).htm';
    HC_Export_Data_in_Sequence_Data_Explorer: Result := 'Export_Data_(Sequence_Data_Explorer).htm';
    HC_Translate_Untranslate_in_Sequence_Data_Explorer: Result := 'Translate_Untranslate_(in_Sequence_Data_Explorer).htm';
    HC_Select_Genetic_Code_Table_in_Sequence_Data_Explorer: Result := 'Select_Genetic_Code_(in_Sequence_Data_Explorer).htm';
    HC_Setup_Select_Genes_Domains_in_Sequence_Data_Explorer_: Result := 'Setup_Select_Genes___Domains_(Sequence_Data_Explorer).htm';
    HC_Setup_Select_Taxa_Groups_in_Sequence_Data_Explorer: Result := 'Setup_Select_Taxa_Groups_in_Sequence_Data_Explorer.htm';
    HC_Quit_Data_Viewer: Result := 'Quit_Data_Viewer.htm';
    HC_Color_Cells: Result := 'Color_Cells.htm';

    HC_Display_Menu_In_Sequence_Data_Explorer: Result := 'Display_Menu_in_Sequence_Data_Explorer.htm';
    HC_Show_Only_Selected_Sequences: Result := 'Show_Only_Selected_Sequences.htm';
    HC_Use_Identical_Symbol: Result := 'Use_Identical_Symbol.htm';
    HC_Sort_Sequences: Result := 'Sort_Sequences.htm';
    HC_Sort_Sequences_By_Sequence_Name: Result := 'Sort_Sequences_By_Sequence_Name.htm';
    HC_Sort_Sequences_by_Group_Name: Result := 'Sort_Sequences_by_Group_Name.htm';
    HC_Sort_Sequences_by_Group_and_Sequence_Names: Result := 'Sort_Sequences_by_Group_and_Sequence_Names.htm';
    HC_Sort_Sequences_As_per_Taxa_Group_Organizer: Result := 'Sort_Sequences_As_per_Taxa_Group_Organizer.htm';
    HC_Restore_Input_Order: Result := 'Restore_Input_Order.htm';
    HC_Show_Sequence_Names: Result := 'Show_Sequence_Names.htm';
    HC_Show_Group_Names: Result := 'Show_Group_Names.htm';
    HC_Change_Font: Result := 'Change_Font.htm';

    HC_Highlight_Menu_In_Sequence_Data_Explorer: Result := 'Highlight_Menu_in_Sequence_Data_Explorer.htm';
    HC_Highlight_Conserved_Sites: Result := 'Highlight_Conserved_Sites.htm';
    HC_Highlight_Variable_Sites: Result := 'Highlight_Variable_Sites.htm';
    HC_Highlight_Parsimony_Informative_Sites: Result := 'Highlight_Parsimony_Informative_Sites.htm';
    HC_Highlight_Singleton_Sites: Result := 'Highlight_Singleton_Sites.htm';
    HC_Highlight_0_fold_Degenerate_Sites: Result := 'Highlight_0-fold_Degenerate_Sites.htm';
    HC_Highlight_2_fold_Degenerate_Sites: Result := 'Highlight_2-fold_Degenerate_Sites.htm';
    HC_Highlight_4_fold_Degenerate_Sites: Result := 'Highlight_4-fold_Degenerate_Sites.htm';

    HC_Stats_menu_in_Sequence_Data_Explorer: Result := 'Statistics_Menu_in_Sequence_Data_Explorer.htm';
    HC_Nucleotide_Composition: Result := 'Nucleotide_Composition.htm';
    HC_Nucleotide_Pair_Frequencies: Result := 'Nucleotide_Pair_Frequencies.htm';
    HC_Codon_Usage: Result := 'Codon_Usage.htm';
    HC_Amino_Acid_Composition: Result := 'Amino_Acid_Composition.htm';
    HC_Use_All_Selected_Sites: Result := 'Use_All_Selected_Sites.htm';
    HC_Use_only_Highlighted_Sites: Result := 'Use_only_Highlighted_Sites.htm';

    // TextEditor
    HC_Text_Editor: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_TE_New: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_TE_Open: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_TE_Save: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_TE_Save_As: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_Close_File_in_Text_Editor: Result := 'Close_File_in_Text_Editor.htm';
    HC_Convert_to_Mega_Format_in_Text_Editor: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FConvert_to_MEGA_Format.htm';
    HC_TE_Exit: Result := 'Exit_Editor_in_Text_Editor.htm';
    HC_TE_Print: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_TE_Cut: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_TE_Copy: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_TE_Paste: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_TE_Undo: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_Delete_in_Text_Editor: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_Select_All_in_Text_Editor: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_TE_Find: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_TE_Replace: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_TE_Find_again: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_Go_to_Line: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_TE_Font: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_Show_Line_Numbers_in_Text_Editor: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_Word_Wrap_in_Text_Editor: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_Copy_Screenshot_to_Clipboard: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FText_Editor.htm';
    HC_Format_Selected_Sequence: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FFormat_Selected_Sequence.htm';
    HC_Reverse_Complement: Result := 'Part_II_Assembling_Data_For_Analysis%2FText_Editor_And_Utilities%2FReverse_Complement.htm';
    HC_Distance_Data_Explorer: Result := 'Distance_Data_Explorer.htm';
    HC_Dist_Exp_File_Menu: Result := 'File_Menu_in_Distance_Data_Explorer.htm';
    HC_Dist_Exp_Display_Menu: Result := 'Display_Menu_in_Sequence_Data_Explorer.htm';
    HC_Dist_Exp_Average_Menu: Result := 'Average_Menu_in_Distance_Data_Explorer.htm';
    HC_Dist_Exp_Options_dialog_box: Result := 'Options_dialog_box.htm';

    // mtreeinputform
    RH_Tree_Topology_Editor: Result := 'Tree_Topology_Editor.htm';

    // displaymatrixdialog
    HC_Dist_Matrix_Exp_File_Menu: Result := 'File_Menu_in_Distance_Matrix_Explorer.htm';
    HC_Dist_Matrix_Exp_Display_Menu: Result := 'Display_Menu_in_Distance_Matrix_Explorer.htm';
    HC_Dist_Matrix_Exp_Average_Menu: Result := 'Average_Menu_in_Distance_Matrix_Explorer.htm';
    HC_Distance_Matrix_Explorer: Result := 'Distance_Matrix_Explorer.htm';

    // megamain from GetFileAndOpenData
    HC_Open_Data: Result := 'Open_Data.htm';

    // other megamain
    HC_Data_Menu: Result := 'Data_Menu.htm';
    HC_Open_A_File: Result := 'Open_A_File.htm';
    HC_Convert_To_MEGA_Format: Result := 'Convert_To_MEGA_Format_Main_File_Menu.htm';
    HC_Save_Data_Session_To_File: Result := 'Save_Data_Session_To_File.htm';
    HC_Export_Data: Result := 'Export_Data_(Sequence_Data_Explorer).htm';
    HC_Close_data: Result := 'Close_Data.htm';
    HC_Data_Explorer: Result := 'Sequence_Data_Explorer.htm';
    HC_Setup_Genes_Domains: Result := 'Setup_Select_Genes_Domains.htm';
    HC_Genes_Domains_Dialog: Result := 'Setup_Select_Genes_Domains_Dialog.htm';
    HC_Setup_Taxa_Groups: Result := 'Setup_Select_Taxa_Groups.htm';
    HC_Select_Genetic_Code_Table: Result := 'Select_Genetic_Code_Table.htm';
    HC_Alignment_Explorer_CLUSTAL: Result := 'ClustalW.htm';
    HC_Open_Saved_Alignment_Session: Result := 'Open_Saved_Alignment_Session.htm';
    HC_Edit_Sequencer_Files: Result := 'View_Edit_Sequencer_Files.htm';
    HC_Printer_Setup: Result := 'Printer_Setup.htm';
    HC_Exit: Result := 'Exit.htm';

    //Phylogeny
    HC_Neighbor_Joining: Result := 'Neighbor_Joining_Construct_Phylogeny.htm';
    HC_Minimum_Evolution: Result := 'Minimum_Evolution_Construct_Phylogeny.htm';
    HC_UPGMA: Result := 'UPGMA_Construct_Phylogeny.htm';
    HC_Maximum_likelihood_ML_: Result := 'Maximum_Likelihood_ML.htm';
    HC_Maximum_Parsimony: Result := 'Maximum_Parsimony_Construct_Phylogeny.htm';

    HC_Z_Test_of_Selection: Result := 'Codon_Based_Z-Test_large_sample.htm';
    HC_Fisher_Exact_Test_Of_Selection: Result := 'Codon_Based_Fisher_s_Exact_Test.htm';
    HC_Estimate_Selection_for_each_Codon_HyPhy_: Result := 'Estimate_Selection_for_each_Codon_HyPhy.htm';

    HC_Molecular_Clock_Test_ML_: Result := 'Molecular_Clock_Test_ML.htm';

    HC_Inferring_Ancestral_Sequences_ML_: Result := 'Inferring_Ancestral_Sequences_ML.htm';
    HC_Inferring_Ancestral_sequences_MP_: Result := 'Inferring_Ancestral_Sequences_MP.htm';
    HC_Find_Best_DNA_Protein_Models_ML_: Result := 'Find_Best_DNA_Protein_Models_ML.htm';

    HC_Estimate_Substitution_Matrix_ML_: Result := 'Estimate_Substitution_Matrix_ML.htm';
    HC_Estimate_Transition_Transversion_Bias_ML_: Result := 'Estimate_Transition_Transversion_Bias_ML.htm';
    HC_Estimate_Rate_Variation_among_Sites_ML_: Result := 'Estimate_Rate_Variation_among_Sites_ML.htm';
    HC_Compute_MCL_Substitution_Matrix: Result := 'Compute_MCL_Substitution_Matrix.htm';
    HC_Compute_MCL_Transversion_Transition_bias: Result := 'Compute_MCL_Transversion_Transition_bias.htm';
    HC_Estimate_Position_by_Position_Rates_ML_: Result := 'Estimate_Position-by-Position_Rates_ML.htm';
    HC_Pattern_Menu: Result := 'Pattern_Tests.htm';

    HC_Pairwise_Distances: Result := 'Compute_Pairwise.htm';
    HC_Overall_Mean: Result := 'Compute_Overall_Mean.htm';
    HC_Within_Group_Mean: Result := 'Compute_Within_Groups_Mean.htm';
    HC_Between_Groups_Means: Result := 'Compute_Between_Groups_Means.htm';
    HC_Net_Between_Groups_Means: Result := 'Compute_Net_Between_Groups_Means.htm';
    HC_Not_Enough_Groups_Selected: Result := 'Not_Enough_Groups_Selected.htm';

    HC_Sequence_Diversity: Result := 'Compute_Sequence_Diversity.htm';

    HC_Analyze_User_Tree_by_Least_Squares: Result := 'Analyze_User_Tree_by_Least_Squares.htm';
    HC_Analyze_User_Tree_by_Maximum_Likelihood: Result := 'Analyze_User_Tree_by_Maximum_Likelihood.htm';
    HC_Analyze_User_Tree_by_Parsimony: Result := 'Analyze_User_Tree_by_Parsimony.htm';
    HC_Conduct_Interior_Branch_Test_by_Least_Squares: Result := 'Interior_Branch_Test_of_Phylogeny.htm';
    HC_Display_Saved_Tree_session: Result := 'Display_Saved_Tree_Session.htm';
    HC_Display_Newick_Trees_from_File: Result := 'Display_Newick_Trees_from_File.htm';
    HC_Show_Web_Browser: Result := 'Show_Web_Browser.htm';
    HC_Browse_Databanks: Result := 'Query_Databanks.htm';
    HC_BLAST_Search: Result := 'Do_BLAST_Search.htm';

    // Tree Explorer
    HC_Tree_Explorer: Result := 'TreeExplorer_HC_files%2FTree_Explorer.htm';
    HC_File_Menu_in_Tree_Explorer: Result := 'TreeExplorer_HC_files%2FFile_Menu_in_Tree_Explorer.htm';
    HC_Image_Menu_in_Tree_Explorer: Result := 'Image_Menu_in_Tree_Explorer.htm';
    HC_Subtree_in_Tree_Explorer: Result := 'Subtree_Menu_in_Tree_Explorer.htm';
    HC_View_Menu_in_Tree_Explorer: Result := 'View_Menu_in_Tree_Explorer.htm';
    HC_Compute_in_Tree_Explorer: Result := 'Compute_Menu_in_Tree_Explorer.htm';
    HC_Tree_tab_in_Format_dialog_box: Result := 'Tree_tab_in_Options_dialog_box.htm';
    HC_Branch_tab_in_Format_dialog_box: Result := 'Branch_tab_in_Options_dialog_box.htm';
    HC_Taxon_Name_tab_in_Format_dialog_box: Result := 'Labels_tab_in_Options_dialog_box.htm';
    HC_Scale_Bar_tab_in_Format_dialog_box: Result := 'Scale_Bar_tab_in_Options_dialog_box.htm';

    HC_TEx_Information_Box: Result := 'Information_Box.htm';
    HC_Format_dialog_box_in_Tree_Explorer: Result := 'Options_dialog_box_in_Tree_Explorer.htm';

    HC_GENEDUPS_WIZARD: Result := 'Gene_Duplication_Inference.htm';
    HC_TIMETREE_WIZARD: Result := 'Time_Trees.htm';
    HC_CALIBRATIONS_EDITOR: Result := 'Calibration_Dialog.htm';
    HC_Subtree_Drawing_Options: Result := 'Subtree_Drawing_Options_in_Tree_Explorer.htm';
    HC_MUTATION_EXPLORER: Result := 'Mutation_Explorer.htm';
    HC_CALIBRATE_MOLECULAR_CLOCK: Result := 'Calibrate_Timetree_with_Molecular_Clock.htm';
    HC_EVOLUTIONARY_PROBABILITY: Result := 'Evolutionary_Probabilities.htm';
    HC_CORRTEST: Result := 'Autocorrelated_Rates_Test.htm';
    HC_COMMAND_STATEMENTS: Result := 'Writing_Command_Statements_for_Defining_Groups_of_Taxa.htm';
    else
      Result := 'First_Time_User.htm';
  end;
end;

end.

