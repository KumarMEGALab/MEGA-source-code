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
var index =  {"type":"data","keys":[{"type":"key","name":"Average Menu","topics":[{"type":"topic","name":"Average Menu (in Distance Data Explorer)","url":"Average_Menu_in_Distance_Data_Explorer.htm"},{"type":"topic","name":"Average Menu (in Distance Matrix Explorer)","url":"Average_Menu_in_Distance_Matrix_Explorer.htm"},{"type":"topic","name":"Distance Matrix Explorer","url":"Distance_Matrix_Explorer.htm"},{"type":"topic","name":"MEGA Basics","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/MEGA_Basics.htm"}],"keys":[{"type":"key","name":"find","topics":[{"type":"topic","name":"MEGA Basics","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/MEGA_Basics.htm"}]}]},{"type":"key","name":"back","topics":[{"type":"topic","name":"Aligning Sequences","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Aligning_Sequences.htm"},{"type":"topic","name":"Constructing a Timetree (ML)","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Constructing_a_Timetree_(ML).htm"}],"keys":[{"type":"key","name":"MEGA","topics":[{"type":"topic","name":"Aligning Sequences","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Aligning_Sequences.htm"}]},{"type":"key","name":"Tree Explorer window","topics":[{"type":"topic","name":"Constructing a Timetree (ML)","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Constructing_a_Timetree_(ML).htm"}]}]},{"type":"key","name":"Background Color","topics":[{"type":"topic","name":"Alignment Explorer","url":"Alignment_Explorer.htm"},{"type":"topic","name":"Display Menu","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/Menu_Items_In_Alignment_Explorer/Display_Menu.htm"},{"type":"topic","name":"Display Menu (in Alignment Explorer)","url":"Display_Menu_in_Alignment_Explorer.htm"},{"type":"topic","name":"Display Menu (in Sequence Data Explorer)","url":"Display_Menu_in_Sequence_Data_Explorer.htm"},{"type":"topic","name":"Introduction to Alignment Explorer","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/Introduction_to_Alignment_Explorer.htm"},{"type":"topic","name":"Setup/Select Genes & Domains Dialog","url":"Setup_Select_Genes_Domains_Dialog.htm"}],"keys":[{"type":"key","name":"display","topics":[{"type":"topic","name":"Display Menu","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/Menu_Items_In_Alignment_Explorer/Display_Menu.htm"}]}]},{"type":"key","name":"Based","topics":[{"type":"topic","name":"Data Menu","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/Menu_Items_In_Alignment_Explorer/Data_Menu.htm"},{"type":"topic","name":"Data Menu (in Alignment Explorer)","url":"Data_Menu_in_Alignment_Explorer.htm"}]},{"type":"key","name":"Basic Functions","topics":[{"type":"topic","name":"Toolbar in Alignment Explorer","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/Menu_Items_In_Alignment_Explorer/Toolbar_in_Alignment_Explorer.htm"},{"type":"topic","name":"Toolbars in Alignment Explorer","url":"Toolbars_in_Alignment_Explorer.htm"}]},{"type":"key","name":"Basic Sequence Statistics","topics":[{"type":"topic","name":"Basic Sequence Statistics","url":"Part_IV_Evolutionary_Analyses/Computing_Basic_Statistical_Quantities_For_Sequence_Data/Basic_Sequence_Statistics.htm"}]},{"type":"key","name":"Battistuzzi FU","topics":[{"type":"topic","name":"Tamura et al. 2012","url":"Tamura_et_al_2012.htm"}]},{"type":"key","name":"BEGIN DATA","topics":[{"type":"topic","name":"Converting NEXUS Format","url":"Converting_NEXUS_Format.htm"}]},{"type":"key","name":"beginning","topics":[{"type":"topic","name":"Building Trees From Sequence Data","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Building_Trees_From_Sequence_Data.htm"},{"type":"topic","name":"Converting Phylip (Interleaved) Format","url":"Converting_Phylip_(Interleaved)_Format.htm"}],"keys":[{"type":"key","name":"computa","topics":[{"type":"topic","name":"Building Trees From Sequence Data","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Building_Trees_From_Sequence_Data.htm"}]},{"type":"key","name":"PHYLIP","topics":[{"type":"topic","name":"Converting Phylip (Interleaved) Format","url":"Converting_Phylip_(Interleaved)_Format.htm"}]}]},{"type":"key","name":"Best","topics":[{"type":"topic","name":"Mutation Explorer","url":"Mutation_Explorer.htm"},{"type":"topic","name":"Prediction Data Tab","url":"Prediction_Data_Tab.htm"}],"keys":[{"type":"key","name":"execute","topics":[{"type":"topic","name":"Prediction Data Tab","url":"Prediction_Data_Tab.htm"}]},{"type":"key","name":"Resize Columns","topics":[{"type":"topic","name":"Mutation Explorer","url":"Mutation_Explorer.htm"}]}]},{"type":"key","name":"Beta Test","topics":[{"type":"topic","name":"Acknowledgements","url":"Acknowledgements.htm"}]},{"type":"key","name":"Between Groups","topics":[{"type":"topic","name":"Average Menu (in Distance Matrix Explorer)","url":"Average_Menu_in_Distance_Matrix_Explorer.htm"},{"type":"topic","name":"MEGA Basics","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/MEGA_Basics.htm"}]},{"type":"key","name":"Bianchi","topics":[{"type":"topic","name":"Kumar Method","url":"Kumar_Method.htm"}]},{"type":"key","name":"Bianchi 1993","topics":[{"type":"topic","name":"Pamilo and Bianchi 1993","url":"Pamilo_and_Bianchi_1993.htm"},{"type":"topic","name":"Pamilo-Bianchi-Li Method","url":"Pamilo-Bianchi-Li_Method.htm"}]},{"type":"key","name":"Bianchi NO","topics":[{"type":"topic","name":"Pamilo and Bianchi 1993","url":"Pamilo_and_Bianchi_1993.htm"}]},{"type":"key","name":"Bibliography","topics":[{"type":"topic","name":"Citing MEGA in Publications","url":"Citing_MEGA_In_Publications.htm"}]},{"type":"key","name":"Bifurcating","topics":[{"type":"topic","name":"Bifurcating Tree","url":"Bifurcating_Tree.htm"}],"keys":[{"type":"key","name":"Tree","topics":[{"type":"topic","name":"Bifurcating Tree","url":"Bifurcating_Tree.htm"}]}]},{"type":"key","name":"Bigrab2.aln","topics":[{"type":"topic","name":"Converting Clustal Format","url":"Converting_Clustal_Format.htm"}]},{"type":"key","name":"Billing","topics":[{"type":"topic","name":"Tamura et al. 2012","url":"Tamura_et_al_2012.htm"}]},{"type":"key","name":"Biol","topics":[{"type":"topic","name":"About BLAST","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/BLAST/About_BLAST.htm"}]},{"type":"key","name":"BioNJ","topics":[{"type":"topic","name":"Analysis Options","url":"Analysis_Options.htm"}]},{"type":"key","name":"Biosciences","topics":[{"type":"topic","name":"Jones et al. 1992","url":"Jones_et_al_1992.htm"}]},{"type":"key","name":"Bioseq","topics":[{"type":"topic","name":"Converting XML Format","url":"Converting_XML_Format.htm"}]},{"type":"key","name":"Biotechnology Information","topics":[{"type":"topic","name":"NCBI","url":"NCBI.htm"}],"keys":[{"type":"key","name":"National Center","topics":[{"type":"topic","name":"NCBI","url":"NCBI.htm"}]}]},{"type":"key","name":"Bisection","topics":[{"type":"topic","name":"Analysis Preferences (Maximum Parsimony)","url":"Analysis_Preferences_Maximum_Parsimony.htm"},{"type":"topic","name":"Maximum Parsimony (Construct Phylogeny)","url":"Maximum_Parsimony_Construct_Phylogeny.htm"},{"type":"topic","name":"Maximum Parsimony (MP) Method","url":"Maximum_Parsimony_MP_Method.htm"}]},{"type":"key","name":"bitmap","topics":[{"type":"topic","name":"Image Menu (in Tree Explorer)","url":"Image_Menu_in_Tree_Explorer.htm"}]},{"type":"key","name":"Blackwell Science","topics":[{"type":"topic","name":"Page and Holmes 1998","url":"Page_and_Holmes_1998.htm"}]},{"type":"key","name":"Blank Names Are Not Permitted","topics":[{"type":"topic","name":"Blank Names Are Not Permitted","url":"Blank_Names_Are_Not_Permitted.htm"}]},{"type":"key","name":"BLAST","topics":[{"type":"topic","name":"About BLAST","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/BLAST/About_BLAST.htm"},{"type":"topic","name":"Alignment Explorer","url":"Alignment_Explorer.htm"},{"type":"topic","name":"Do BLAST Search","url":"Do_BLAST_Search.htm"},{"type":"topic","name":"Main MEGA Window","url":"Main_MEGA_Window.htm"},{"type":"topic","name":"Sequencer Menu (in Alignment Explorer)","url":"Sequencer_Menu_in_Alignment_Explorer.htm"},{"type":"topic","name":"Toolbar in Alignment Explorer","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/Menu_Items_In_Alignment_Explorer/Toolbar_in_Alignment_Explorer.htm"},{"type":"topic","name":"Toolbars in Alignment Explorer","url":"Toolbars_in_Alignment_Explorer.htm"},{"type":"topic","name":"Trace Data File Viewer/Editor","url":"Trace_Data_File_Viewer_Editor.htm"},{"type":"topic","name":"Trace Data File Viewer/Editor","url":"Part_II_Assembling_Data_For_Analysis/Trace_Data_File_Viewer_Editor.htm"},{"type":"topic","name":"Using BLAST Within MEGA","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/BLAST/Using_BLAST_Within_MEGA.htm"},{"type":"topic","name":"Web Menu","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/Menu_Items_In_Alignment_Explorer/Web_Menu.htm"},{"type":"topic","name":"Web Menu (in Alignment Explorer)","url":"Web_Menu_in_Alignment_Explorer.htm"}],"keys":[{"type":"key","name":"fill","topics":[{"type":"topic","name":"Toolbar in Alignment Explorer","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/Menu_Items_In_Alignment_Explorer/Toolbar_in_Alignment_Explorer.htm"},{"type":"topic","name":"Toolbars in Alignment Explorer","url":"Toolbars_in_Alignment_Explorer.htm"}]},{"type":"key","name":"launch","topics":[{"type":"topic","name":"Do BLAST Search","url":"Do_BLAST_Search.htm"}]}]},{"type":"key","name":"BLAST Search","topics":[{"type":"topic","name":"Do BLAST Search","url":"Do_BLAST_Search.htm"},{"type":"topic","name":"Trace Data File Viewer/Editor","url":"Part_II_Assembling_Data_For_Analysis/Trace_Data_File_Viewer_Editor.htm"},{"type":"topic","name":"Trace Data File Viewer/Editor","url":"Trace_Data_File_Viewer_Editor.htm"},{"type":"topic","name":"Web Menu","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/Menu_Items_In_Alignment_Explorer/Web_Menu.htm"},{"type":"topic","name":"Web Menu (in Alignment Explorer)","url":"Web_Menu_in_Alignment_Explorer.htm"}]},{"type":"key","name":"BLAST uses","topics":[{"type":"topic","name":"About BLAST","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/BLAST/About_BLAST.htm"}]},{"type":"key","name":"Blosum62","topics":[{"type":"topic","name":"Prediction Data Tab","url":"Prediction_Data_Tab.htm"}]},{"type":"key","name":"bolded","topics":[{"type":"topic","name":"Find Sequence Name","url":"Find_Sequence_Name.htm"},{"type":"topic","name":"Guide to Notations Used","url":"Part_I_Getting_Started/Features_and_Support/Guide_to_Notations_Used.htm"},{"type":"topic","name":"Hide Name Result","url":"Hide_Name_Result.htm"},{"type":"topic","name":"Sequence Data Explorer","url":"Sequence_Data_Explorer.htm"}]},{"type":"key","name":"Bootstrap","topics":[{"type":"topic","name":"Analysis Options","url":"Analysis_Options.htm"},{"type":"topic","name":"Analysis Preferences (Maximum Likelihood)","url":"Analysis_Preferences_Maximum_Likelihood.htm"},{"type":"topic","name":"Analysis Preferences (Minimum Evolution)","url":"Analysis_Preferences_Minimum_Evolution.htm"},{"type":"topic","name":"Analysis Preferences (NJ/UPGMA)","url":"Analysis_Preferences_NJ_UPGMA.htm"},{"type":"topic","name":"Efron 1982","url":"Efron_1982.htm"},{"type":"topic","name":"Testing Tree Reliability","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Testing_Tree_Reliability.htm"}]},{"type":"key","name":"bootstrap consensus tree","topics":[{"type":"topic","name":"Why can't I display branch lengths for a bootstrap consensus tree?","url":"Why_can_t_I_display_branch_lengths_for_a_bootstrap_consensus_tree.htm"}],"keys":[{"type":"key","name":"display branch lengths for","topics":[{"type":"topic","name":"Why can't I display branch lengths for a bootstrap consensus tree?","url":"Why_can_t_I_display_branch_lengths_for_a_bootstrap_consensus_tree.htm"}]}]},{"type":"key","name":"Bootstrap method","topics":[{"type":"topic","name":"Bootstrap method to compute standard error of distance estimates","url":"Bootstrap_method_to_compute_standard_error_of_distance_estimates.htm"}],"keys":[{"type":"key","name":"compute standard error","topics":[{"type":"topic","name":"Bootstrap method to compute standard error of distance estimates","url":"Bootstrap_method_to_compute_standard_error_of_distance_estimates.htm"}]}]},{"type":"key","name":"Bootstrap Replications","topics":[{"type":"topic","name":"Analysis Preferences (Distance Computation)","url":"Analysis_Preferences_Distance_Computation.htm"}]},{"type":"key","name":"Bootstrap Testing","topics":[{"type":"topic","name":"Bootstrap Test of Phylogeny","url":"Bootstrap_Test_of_Phylogeny.htm"},{"type":"topic","name":"Testing Tree Reliability","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Testing_Tree_Reliability.htm"}],"keys":[{"type":"key","name":"Phylogeny","topics":[{"type":"topic","name":"Bootstrap Test of Phylogeny","url":"Bootstrap_Test_of_Phylogeny.htm"}]}]},{"type":"key","name":"Bork","topics":[{"type":"topic","name":"Adzhubei et al. 2010","url":"Adzhubei_et_al_2010.htm"}]},{"type":"key","name":"Bound","topics":[{"type":"topic","name":"Analysis Preferences (Maximum Parsimony)","url":"Analysis_Preferences_Maximum_Parsimony.htm"}]},{"type":"key","name":"Bound Search Option","topics":[{"type":"topic","name":"Building Trees From Sequence Data","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Building_Trees_From_Sequence_Data.htm"}]},{"type":"key","name":"Bradford PG","topics":[{"type":"topic","name":"Purdom et al. 2000","url":"Purdom_et_al_2000.htm"}]}]};
window.rh.model.publish(rh.consts('KEY_TEMP_DATA'), index, { sync:true });
})();