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

(function() {
var index =  {"type":"data","keys":[{"type":"key","name":"Test Maximum Parsimony Tree","topics":[{"type":"topic","name":"Bootstrap Test of Phylogeny","url":"Bootstrap_Test_of_Phylogeny.htm"},{"type":"topic","name":"Building Trees From Sequence Data","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Building_Trees_From_Sequence_Data.htm"}]},{"type":"key","name":"Test Minimum","topics":[{"type":"topic","name":"Bootstrap Test of Phylogeny","url":"Bootstrap_Test_of_Phylogeny.htm"},{"type":"topic","name":"Interior Branch Test of Phylogeny","url":"Interior_Branch_Test_of_Phylogeny.htm"}]},{"type":"key","name":"Test Minimum Evolution Tree","topics":[{"type":"topic","name":"Minimum Evolution (Construct Phylogeny)","url":"Minimum_Evolution_Construct_Phylogeny.htm"}]},{"type":"key","name":"Test UPGMA Tree","topics":[{"type":"topic","name":"Bootstrap Test of Phylogeny","url":"Bootstrap_Test_of_Phylogeny.htm"},{"type":"topic","name":"UPGMA (Construct Phylogeny)","url":"UPGMA_Construct_Phylogeny.htm"}]},{"type":"key","name":"Teukolsky SA","topics":[{"type":"topic","name":"Press et al. 1993","url":"Press_et_al_1993.htm"}]},{"type":"key","name":"Text","topics":[{"type":"topic","name":"Display Results in XL/CSV/Text","url":"Display_Results_in_XL_CSV_Text.htm"},{"type":"topic","name":"Distance Data Explorer","url":"Distance_Data_Explorer.htm"},{"type":"topic","name":"Distance Matrix Explorer","url":"Distance_Matrix_Explorer.htm"},{"type":"topic","name":"First Time User","url":"First_Time_User.htm"},{"type":"topic","name":"Sequence Data Explorer","url":"Sequence_Data_Explorer.htm"}]},{"type":"key","name":"text and then move","topics":[{"type":"topic","name":"Text Editor","url":"Part_II_Assembling_Data_For_Analysis/Text_Editor_And_Utilities/Text_Editor.htm"}],"keys":[{"type":"key","name":"select","topics":[{"type":"topic","name":"Text Editor","url":"Part_II_Assembling_Data_For_Analysis/Text_Editor_And_Utilities/Text_Editor.htm"}]}]},{"type":"key","name":"Text Editor","topics":[{"type":"topic","name":"Close File (in Text Editor)","url":"Close_File_in_Text_Editor.htm"},{"type":"topic","name":"Computing Sequence Statistics","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Computing_Sequence_Statistics.htm"},{"type":"topic","name":"Constructing a Timetree (ML)","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Constructing_a_Timetree_(ML).htm"},{"type":"topic","name":"Convert to Mega Format (in Text Editor)","url":"Convert_to_Mega_Format_in_Text_Editor.htm"},{"type":"topic","name":"Copy (in Text Editor)","url":"Copy_in_Text_Editor.htm"},{"type":"topic","name":"Cut (in Text Editor)","url":"Cut_in_Text_Editor.htm"},{"type":"topic","name":"Delete (in Text Editor)","url":"Delete_in_Text_Editor.htm"},{"type":"topic","name":"Exit Editor (in Text Editor)","url":"Exit_Editor_in_Text_Editor.htm"},{"type":"topic","name":"Find (in Text Editor)","url":"Find_in_Text_Editor.htm"},{"type":"topic","name":"Find Again (in Text Editor)","url":"Find_Again_in_Text_Editor.htm"},{"type":"topic","name":"Font (in Text Editor)","url":"Font_in_Text_Editor.htm"},{"type":"topic","name":"Go to Line (in Text Editor)","url":"Go_to_Line_in_Text_Editor.htm"},{"type":"topic","name":"MEGA Basics","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/MEGA_Basics.htm"},{"type":"topic","name":"New (in Text Editor)","url":"New_in_Text_Editor.htm"},{"type":"topic","name":"Nucleotide Composition","url":"Nucleotide_Composition.htm"},{"type":"topic","name":"Open (in Text Editor)","url":"Open_in_Text_Editor.htm"},{"type":"topic","name":"Open Data","url":"Open_Data.htm"},{"type":"topic","name":"Paste (in Text Editor)","url":"Paste_in_Text_Editor.htm"},{"type":"topic","name":"Print (in Text Editor)","url":"Print_in_Text_Editor.htm"},{"type":"topic","name":"Reopen (in Text Editor)","url":"Reopen_in_Text_Editor.htm"},{"type":"topic","name":"Replace (in Text Editor)","url":"Replace_in_Text_Editor.htm"},{"type":"topic","name":"Save (in Text Editor)","url":"Save_in_Text_Editor.htm"},{"type":"topic","name":"Save As (in Text Editor)","url":"Save_As_in_Text_Editor.htm"},{"type":"topic","name":"Select All (in Text Editor)","url":"Select_All_in_Text_Editor.htm"},{"type":"topic","name":"Show Line Numbers (in Text Editor)","url":"Show_Line_Numbers_in_Text_Editor.htm"},{"type":"topic","name":"Statistics Menu (in Sequence Data Explorer)","url":"Statistics_Menu_in_Sequence_Data_Explorer.htm"},{"type":"topic","name":"Text Editor","url":"Part_II_Assembling_Data_For_Analysis/Text_Editor_And_Utilities/Text_Editor.htm"},{"type":"topic","name":"Undo (in Text Editor)","url":"Undo_in_Text_Editor.htm"},{"type":"topic","name":"Word Wrap (in Text Editor)","url":"Word_Wrap_in_Text_Editor.htm"}]},{"type":"key","name":"Text Editor contains","topics":[{"type":"topic","name":"Text Editor","url":"Part_II_Assembling_Data_For_Analysis/Text_Editor_And_Utilities/Text_Editor.htm"}],"keys":[{"type":"key","name":"menu bar","topics":[{"type":"topic","name":"Text Editor","url":"Part_II_Assembling_Data_For_Analysis/Text_Editor_And_Utilities/Text_Editor.htm"}]}]},{"type":"key","name":"Text File","topics":[{"type":"topic","name":"MEGA Basics","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/MEGA_Basics.htm"},{"type":"topic","name":"Upload a text file with the coordinate information for all nSNVs of interest","url":"Upload_a_text_file_with_the_coordinate_information_for_all_nSNVs_of_interest.htm"}],"keys":[{"type":"key","name":"Edit","topics":[{"type":"topic","name":"MEGA Basics","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/MEGA_Basics.htm"}]},{"type":"key","name":"Upload","topics":[{"type":"topic","name":"Upload a text file with the coordinate information for all nSNVs of interest","url":"Upload_a_text_file_with_the_coordinate_information_for_all_nSNVs_of_interest.htm"}]}]},{"type":"key","name":"Text File Editor","topics":[{"type":"topic","name":"Computing Sequence Statistics","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Computing_Sequence_Statistics.htm"},{"type":"topic","name":"MEGA Basics","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/MEGA_Basics.htm"},{"type":"topic","name":"Text Editor","url":"Part_II_Assembling_Data_For_Analysis/Text_Editor_And_Utilities/Text_Editor.htm"}],"keys":[{"type":"key","name":"Close","topics":[{"type":"topic","name":"Computing Sequence Statistics","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Computing_Sequence_Statistics.htm"}]}]},{"type":"key","name":"that all tips of","topics":[{"type":"topic","name":"Molecular Clock Test (ML)","url":"Molecular_Clock_Test_ML.htm"}],"keys":[{"type":"key","name":"tree are","topics":[{"type":"topic","name":"Molecular Clock Test (ML)","url":"Molecular_Clock_Test_ML.htm"}]}]},{"type":"key","name":"the bootstrap","topics":[{"type":"topic","name":"Interior Branch Test of Phylogeny","url":"Interior_Branch_Test_of_Phylogeny.htm"}]},{"type":"key","name":"the extension","topics":[{"type":"topic","name":"Importing Data","url":"Importing_Data.htm"}]},{"type":"key","name":"the interior","topics":[{"type":"topic","name":"Interior Branch Test of Phylogeny","url":"Interior_Branch_Test_of_Phylogeny.htm"}]},{"type":"key","name":"the number","topics":[{"type":"topic","name":"Modified Nei-Gojobori Method","url":"Modified_Nei-Gojobori_Method.htm"}],"keys":[{"type":"key","name":"nonsynonymous","topics":[{"type":"topic","name":"Modified Nei-Gojobori Method","url":"Modified_Nei-Gojobori_Method.htm"}]}]},{"type":"key","name":"the original Nei-Gojobori","topics":[{"type":"topic","name":"Modified Nei-Gojobori Method","url":"Modified_Nei-Gojobori_Method.htm"}]},{"type":"key","name":"the result window","topics":[{"type":"topic","name":"Bootstrap method to compute standard error of distance estimates","url":"Bootstrap_method_to_compute_standard_error_of_distance_estimates.htm"}]},{"type":"key","name":"Theory","topics":[{"type":"topic","name":"Sankoff and Cedergren 1983","url":"Sankoff_and_Cedergren_1983.htm"}]},{"type":"key","name":"Third Gene","topics":[{"type":"topic","name":"Site Label","url":"Site_Label.htm"},{"type":"topic","name":"What is a Site Label?","url":"What_is_a_Site_Label_.htm"}]},{"type":"key","name":"ThirdGene Domain","topics":[{"type":"topic","name":"Site Label","url":"Site_Label.htm"},{"type":"topic","name":"What is a Site Label?","url":"What_is_a_Site_Label_.htm"},{"type":"topic","name":"Writing Command Statements for Defining Groups of Taxa","url":"Writing_Command_Statements_for_Defining_Groups_of_Taxa.htm"}]},{"type":"key","name":"This window","topics":[{"type":"topic","name":"Information Box","url":"Information_Box.htm"}]},{"type":"key","name":"Thompson","topics":[{"type":"topic","name":"About ClustalW","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/ClustalW/About_ClustalW.htm"}]},{"type":"key","name":"Thornton JM","topics":[{"type":"topic","name":"Jones et al. 1992","url":"Jones_et_al_1992.htm"}]},{"type":"key","name":"Threads","topics":[{"type":"topic","name":"Analysis Options","url":"Analysis_Options.htm"}],"keys":[{"type":"key","name":"Number","topics":[{"type":"topic","name":"Analysis Options","url":"Analysis_Options.htm"}]}]},{"type":"key","name":"Throughout MEGA","topics":[{"type":"topic","name":"MEGA Basics","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/MEGA_Basics.htm"}]},{"type":"key","name":"Time Tree Tool","topics":[{"type":"topic","name":"Time Tree Tool","url":"Time_Tree_Tool.htm"}]},{"type":"key","name":"Time Tree Tool button","topics":[{"type":"topic","name":"Time Tree Tool","url":"Time_Tree_Tool.htm"}]},{"type":"key","name":"Time Tree Tutorial","topics":[{"type":"topic","name":"Time Trees","url":"Time_Trees.htm"}]},{"type":"key","name":"Time Trees","topics":[{"type":"topic","name":"Branch tab (in Options dialog box)","url":"Branch_tab_in_Options_dialog_box.htm"},{"type":"topic","name":"Calibration Dialog","url":"Calibration_Dialog.htm"},{"type":"topic","name":"Compute Menu (in Tree Explorer)","url":"Compute_Menu_in_Tree_Explorer.htm"},{"type":"topic","name":"File Menu (in Tree Explorer)","url":"TreeExplorer_HC_files/File_Menu_in_Tree_Explorer.htm"},{"type":"topic","name":"Time Tree Tool","url":"Time_Tree_Tool.htm"},{"type":"topic","name":"Time Trees","url":"Time_Trees.htm"}]},{"type":"key","name":"Time Warps","topics":[{"type":"topic","name":"Sankoff and Cedergren 1983","url":"Sankoff_and_Cedergren_1983.htm"}]},{"type":"key","name":"Timet","topics":[{"type":"topic","name":"Constructing a Timetree (ML)","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Constructing_a_Timetree_(ML).htm"},{"type":"topic","name":"What's New In This Version","url":"Part_I_Getting_Started/Features_and_Support/What_s_New_In_This_Version.htm"}],"keys":[{"type":"key","name":"factored","topics":[{"type":"topic","name":"What's New In This Version","url":"Part_I_Getting_Started/Features_and_Support/What_s_New_In_This_Version.htm"}]}]},{"type":"key","name":"Timetree","topics":[{"type":"topic","name":"Calibration Dialog","url":"Calibration_Dialog.htm"},{"type":"topic","name":"Constructing a Timetree (ML)","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Constructing_a_Timetree_(ML).htm"},{"type":"topic","name":"Preface","url":"Preface.htm"},{"type":"topic","name":"Time Tree Tool","url":"Time_Tree_Tool.htm"},{"type":"topic","name":"What's New In This Version","url":"Part_I_Getting_Started/Features_and_Support/What_s_New_In_This_Version.htm"}],"keys":[{"type":"key","name":"Constructing","topics":[{"type":"topic","name":"Constructing a Timetree (ML)","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Constructing_a_Timetree_(ML).htm"}]},{"type":"key","name":"create","topics":[{"type":"topic","name":"Constructing a Timetree (ML)","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Constructing_a_Timetree_(ML).htm"},{"type":"topic","name":"Preface","url":"Preface.htm"},{"type":"topic","name":"Time Tree Tool","url":"Time_Tree_Tool.htm"}]},{"type":"key","name":"generate","topics":[{"type":"topic","name":"Constructing a Timetree (ML)","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Constructing_a_Timetree_(ML).htm"}]},{"type":"key","name":"select","topics":[{"type":"topic","name":"Calibration Dialog","url":"Calibration_Dialog.htm"}]}]},{"type":"key","name":"Timetree tool","topics":[{"type":"topic","name":"Time Tree Tool","url":"Time_Tree_Tool.htm"}],"keys":[{"type":"key","name":"Using","topics":[{"type":"topic","name":"Time Tree Tool","url":"Time_Tree_Tool.htm"}]}]},{"type":"key","name":"Timetree Wizard","topics":[{"type":"topic","name":"Calibration Dialog","url":"Calibration_Dialog.htm"},{"type":"topic","name":"Time Tree Tool","url":"Time_Tree_Tool.htm"}],"keys":[{"type":"key","name":"advance","topics":[{"type":"topic","name":"Calibration Dialog","url":"Calibration_Dialog.htm"}]}]},{"type":"key","name":"Timetree Wizard window","topics":[{"type":"topic","name":"Constructing a Timetree (ML)","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Constructing_a_Timetree_(ML).htm"}]},{"type":"key","name":"timetrees","topics":[{"type":"topic","name":"Data Coverage","url":"Data_Coverage.htm"},{"type":"topic","name":"Filipski et al. 2014","url":"Filipski_et_al_2014.htm"},{"type":"topic","name":"View Menu (in Tree Explorer)","url":"View_Menu_in_Tree_Explorer.htm"},{"type":"topic","name":"What's New In This Version","url":"Part_I_Getting_Started/Features_and_Support/What_s_New_In_This_Version.htm"}]}]};
window.rh.model.publish(rh.consts('KEY_TEMP_DATA'), index, { sync:true });
})();