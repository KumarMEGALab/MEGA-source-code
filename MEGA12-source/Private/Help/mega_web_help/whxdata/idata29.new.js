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
var index =  {"type":"data","keys":[{"type":"key","name":"Kumar","topics":[{"type":"topic","name":"Acknowledgements","url":"Acknowledgements.htm"},{"type":"topic","name":"Analysis Preferences (Z-test of Selection)","url":"Analysis_Preferences_Z-test_of_Selection.htm"},{"type":"topic","name":"Bootstrap method to compute standard error of distance estimates","url":"Bootstrap_method_to_compute_standard_error_of_distance_estimates.htm"},{"type":"topic","name":"Bootstrap Test of Phylogeny","url":"Bootstrap_Test_of_Phylogeny.htm"},{"type":"topic","name":"Branch-and-Bound algorithm","url":"Branch-and-Bound_algorithm.htm"},{"type":"topic","name":"Citing MEGA in Publications","url":"Citing_MEGA_In_Publications.htm"},{"type":"topic","name":"Close-Neighbor-Interchange (CNI)","url":"Close-Neighbor-Interchange_CNI.htm"},{"type":"topic","name":"Codon Based Fisher's Exact Test","url":"Codon_Based_Fisher_s_Exact_Test.htm"},{"type":"topic","name":"Codon Based Z-Test (large sample)","url":"Codon_Based_Z-Test_large_sample.htm"},{"type":"topic","name":"Condensed Trees","url":"Condensed_Trees.htm"},{"type":"topic","name":"Consensus Tree","url":"Consensus_Tree.htm"},{"type":"topic","name":"Disparity Index Test","url":"Disparity_Index_Test.htm"},{"type":"topic","name":"Equal Input Model (Gamma rates and Heterogeneous Patterns)","url":"Equal_Input_Model_Gamma_rates_and_Heterogeneous_Patterns.htm"},{"type":"topic","name":"Equal Input Model (Heterogeneous Patterns)","url":"Equal_Input_Model_Heterogeneous_Patterns.htm"},{"type":"topic","name":"Filipski et al. 2014","url":"Filipski_et_al_2014.htm"},{"type":"topic","name":"Gamma distance (Amino acids)","url":"Gamma_distance_Amino_acids.htm"},{"type":"topic","name":"General Comments on Statistical Tests","url":"General_Comments_on_Statistical_Tests.htm"},{"type":"topic","name":"Hedges et al. 1992","url":"Hedges_et_al_1992.htm"},{"type":"topic","name":"Interior Branch Test of Phylogeny","url":"Interior_Branch_Test_of_Phylogeny.htm"},{"type":"topic","name":"Introduction to myPEG","url":"Introduction_to_myPEG.htm"},{"type":"topic","name":"Jukes-Cantor distance","url":"Jukes-Cantor_distance.htm"},{"type":"topic","name":"Jukes-Cantor Gamma distance","url":"Jukes-Cantor_Gamma_distance.htm"},{"type":"topic","name":"Kimura 2-parameter distance","url":"Kimura_2-parameter_distance.htm"},{"type":"topic","name":"Kimura gamma distance","url":"Kimura_gamma_distance.htm"},{"type":"topic","name":"Kumar and Gadagkar 2001","url":"Kumar_and_Gadagkar_2001.htm"},{"type":"topic","name":"Kumar et al. 1993","url":"Kumar_et_al_1993.htm"},{"type":"topic","name":"Kumar et al. 2012","url":"Kumar_et_al_2012.htm"},{"type":"topic","name":"Kumar et al. EvoD 2012","url":"Kumar_et_al_EvoD_2012.htm"},{"type":"topic","name":"Kumar Method","url":"Kumar_Method.htm"},{"type":"topic","name":"Large Sample Tests of Selection","url":"Large_Sample_Tests_of_Selection.htm"},{"type":"topic","name":"Li-Wu-Luo Method","url":"Li-Wu-Luo_Method.htm"},{"type":"topic","name":"Max-mini branch-and-bound search","url":"Max-mini_branch-and-bound_search.htm"},{"type":"topic","name":"Maximum Composite Likelihood (Gamma Rates and Heterogeneous Patterns)","url":"Maximum_Composite_Likelihood_Gamma_Rates_and_Heterogeneous_Patterns.htm"},{"type":"topic","name":"Maximum Composite Likelihood (Heterogeneous Patterns)","url":"Maximum_Composite_Likelihood_Heterogeneous_Patterns.htm"},{"type":"topic","name":"Maximum Parsimony (MP) Method","url":"Maximum_Parsimony_MP_Method.htm"},{"type":"topic","name":"MEGA Software Development Team","url":"MEGA_Software_Development_Team.htm"},{"type":"topic","name":"Min-mini algorithm","url":"Min-mini_algorithm.htm"},{"type":"topic","name":"Minimum Evolution","url":"Minimum_Evolution.htm"},{"type":"topic","name":"Models for estimating distances","url":"Models_for_estimating_distances.htm"},{"type":"topic","name":"Modified Nei-Gojobori Method","url":"Modified_Nei-Gojobori_Method.htm"},{"type":"topic","name":"Nei and Kumar 2000","url":"Nei_and_Kumar_2000.htm"},{"type":"topic","name":"Nei et al. 1998","url":"Nei_et_al_1998.htm"},{"type":"topic","name":"Nei-Gojobori Method","url":"Nei-Gojobori_Method.htm"},{"type":"topic","name":"Neighbor Joining (Construct Phylogeny)","url":"Neighbor_Joining_Construct_Phylogeny.htm"},{"type":"topic","name":"Neighbor-Joining (NJ) Method","url":"Neighbor-Joining_NJ_Method.htm"},{"type":"topic","name":"No. of differences (Amino acids)","url":"No_of_differences_Amino_acids.htm"},{"type":"topic","name":"No. of differences (Nucleotide)","url":"No_of_differences_Nucleotide.htm"},{"type":"topic","name":"p-distance (Amino acids)","url":"p-distance_Amino_acids.htm"},{"type":"topic","name":"p-distance (Nucleotide)","url":"p-distance_Nucleotide.htm"},{"type":"topic","name":"Pamilo-Bianchi-Li Method","url":"Pamilo-Bianchi-Li_Method.htm"},{"type":"topic","name":"Pattern tests","url":"Pattern_Tests.htm"},{"type":"topic","name":"Phylogenetic Inference","url":"Phylogenetic_Inference.htm"},{"type":"topic","name":"Poisson Correction (PC) distance","url":"Poisson_Correction_PC_distance.htm"},{"type":"topic","name":"Prediction Data Tab","url":"Prediction_Data_Tab.htm"},{"type":"topic","name":"Purdom et al. 2000","url":"Purdom_et_al_2000.htm"},{"type":"topic","name":"Syn-Nonsynonymous","url":"Syn-Nonsynonymous.htm"},{"type":"topic","name":"Tajima Nei Distance (Gamma Rates and Heterogeneous patterns)","url":"Tajima_Nei_Distance_Gamma_Rates_and_Heterogeneous_patterns.htm"},{"type":"topic","name":"Tajima Nei Distance (Heterogeneous patterns)","url":"Tajima_Nei_Distance_Heterogeneous_patterns.htm"},{"type":"topic","name":"Tajima's Test (Relative Rate)","url":"Tajima_s_Test_Relative_Rate.htm"},{"type":"topic","name":"Tajima's Test of Neutrality","url":"Tajima_s_Test_of_Neutrality.htm"},{"type":"topic","name":"Tajima-Nei distance","url":"Tajima-Nei_distance.htm"},{"type":"topic","name":"Tamura 3 parameter (Gamma rates and Heterogeneous patterns)","url":"Tamura_3_parameter_Gamma_rates_and_Heterogeneous_patterns.htm"},{"type":"topic","name":"Tamura 3 parameter (Heterogeneous patterns)","url":"Tamura_3_parameter_Heterogeneous_patterns.htm"},{"type":"topic","name":"Tamura 3-parameter distance","url":"Tamura_3-parameter_distance.htm"},{"type":"topic","name":"Tamura and Kumar 2002","url":"Tamura_and_Kumar_2002.htm"},{"type":"topic","name":"Tamura et al. 2004","url":"Tamura_et_al_2004.htm"},{"type":"topic","name":"Tamura et al. 2007","url":"Tamura_et_al_2007.htm"},{"type":"topic","name":"Tamura et al. 2012","url":"Tamura_et_al_2012.htm"},{"type":"topic","name":"Tamura-Nei distance","url":"Tamura-Nei_distance.htm"},{"type":"topic","name":"Tamura-Nei distance (Gamma rates and Heterogeneous patterns)","url":"Tamura-Nei_distance_Gamma_rates_and_Heterogeneous_patterns.htm"},{"type":"topic","name":"Tamura-Nei distance (Heterogeneous Patterns)","url":"Tamura-Nei_distance_Heterogeneous_Patterns.htm"},{"type":"topic","name":"Tamura-Nei gamma distance","url":"Tamura-Nei_gamma_distance.htm"},{"type":"topic","name":"UPGMA","url":"UPGMA.htm"},{"type":"topic","name":"Zhang et al. 1997","url":"Zhang_et_al_1997.htm"}]},{"type":"key","name":"Kumar et al","topics":[{"type":"topic","name":"Kumar et al. 1993","url":"Kumar_et_al_1993.htm"},{"type":"topic","name":"Kumar et al. 2012","url":"Kumar_et_al_2012.htm"},{"type":"topic","name":"Kumar et al. EvoD 2012","url":"Kumar_et_al_EvoD_2012.htm"}]},{"type":"key","name":"Kumar Method","topics":[{"type":"topic","name":"Kumar Method","url":"Kumar_Method.htm"},{"type":"topic","name":"Syn-Nonsynonymous","url":"Syn-Nonsynonymous.htm"}]},{"type":"key","name":"Kumar_and_Gadagkar_2001","topics":[{"type":"topic","name":"Disparity Index Test","url":"Disparity_Index_Test.htm"},{"type":"topic","name":"Pattern tests","url":"Pattern_Tests.htm"}]},{"type":"key","name":"Kumar_et_al_1993","topics":[{"type":"topic","name":"Branch-and-Bound algorithm","url":"Branch-and-Bound_algorithm.htm"}]},{"type":"key","name":"Label","topics":[{"type":"topic","name":"Site Label","url":"Site_Label.htm"}]},{"type":"key","name":"Labeled Sites","topics":[{"type":"topic","name":"Analysis Options","url":"Analysis_Options.htm"},{"type":"topic","name":"Analysis Preferences (Distance Computation)","url":"Analysis_Preferences_Distance_Computation.htm"},{"type":"topic","name":"Analysis Preferences (Fisher's Exact Test)","url":"Analysis_Preferences_Fisher_s_Exact_Test.htm"},{"type":"topic","name":"Analysis Preferences (Maximum Likelihood)","url":"Analysis_Preferences_Maximum_Likelihood.htm"},{"type":"topic","name":"Analysis Preferences (Maximum Parsimony)","url":"Analysis_Preferences_Maximum_Parsimony.htm"},{"type":"topic","name":"Analysis Preferences (Minimum Evolution)","url":"Analysis_Preferences_Minimum_Evolution.htm"},{"type":"topic","name":"Analysis Preferences (NJ/UPGMA)","url":"Analysis_Preferences_NJ_UPGMA.htm"},{"type":"topic","name":"Analysis Preferences (Pattern Homogeneity Analysis)","url":"Analysis_Preferences_Pattern_Homogeneity_Analysis.htm"},{"type":"topic","name":"Analysis Preferences (Z-test of Selection)","url":"Analysis_Preferences_Z-test_of_Selection.htm"},{"type":"topic","name":"How to Label Sites","url":"How_to_Label_Sites.htm"},{"type":"topic","name":"Include Sites Option","url":"Include_Sites_Option.htm"},{"type":"topic","name":"Labeled Sites","url":"Labeled_Sites.htm"},{"type":"topic","name":"Sequence Data Subset Selection","url":"Sequence_Data_Subset_Selection.htm"}]},{"type":"key","name":"Labels Tab","topics":[{"type":"topic","name":"Labels tab (in Options dialog box)","url":"Labels_tab_in_Options_dialog_box.htm"},{"type":"topic","name":"Setup/Select Genes & Domains Dialog","url":"Setup_Select_Genes_Domains_Dialog.htm"}]},{"type":"key","name":"Lake 1987","topics":[{"type":"topic","name":"Lake 1987","url":"Lake_1987.htm"}]},{"type":"key","name":"Lake JA","topics":[{"type":"topic","name":"Lake 1987","url":"Lake_1987.htm"}]},{"type":"key","name":"Large Alignment","topics":[{"type":"topic","name":"Muscle Options (DNA)","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/MUSCLE/Muscle_Options_(DNA).htm"},{"type":"topic","name":"MUSCLE Options (Protein)","url":"Part_II_Assembling_Data_For_Analysis/Building_Sequence_Alignments/MUSCLE/MUSCLE_Options_(Protein).htm"}]},{"type":"key","name":"Large Molecular Phylogenies","topics":[{"type":"topic","name":"Tamura et al. 2012","url":"Tamura_et_al_2012.htm"}]},{"type":"key","name":"Large Sample Tests","topics":[{"type":"topic","name":"Large Sample Tests of Selection","url":"Large_Sample_Tests_of_Selection.htm"}],"keys":[{"type":"key","name":"Selection","topics":[{"type":"topic","name":"Large Sample Tests of Selection","url":"Large_Sample_Tests_of_Selection.htm"}]}]},{"type":"key","name":"launch","topics":[{"type":"topic","name":"Do BLAST Search","url":"Do_BLAST_Search.htm"},{"type":"topic","name":"Gene Duplication Inference","url":"Gene_Duplication_Inference.htm"},{"type":"topic","name":"Main MEGA Window","url":"Main_MEGA_Window.htm"}],"keys":[{"type":"key","name":"Analysis","topics":[{"type":"topic","name":"Gene Duplication Inference","url":"Gene_Duplication_Inference.htm"}]},{"type":"key","name":"BLAST","topics":[{"type":"topic","name":"Do BLAST Search","url":"Do_BLAST_Search.htm"}]},{"type":"key","name":"MEGA Web Browser","topics":[{"type":"topic","name":"Main MEGA Window","url":"Main_MEGA_Window.htm"}]}]},{"type":"key","name":"Launch Analysis button","topics":[{"type":"topic","name":"Inferring Gene Duplications","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Inferring_Gene_Duplications.htm"}]},{"type":"key","name":"launched with","topics":[{"type":"topic","name":"Mutation Detail View","url":"Mutation_Detail_View.htm"}]},{"type":"key","name":"Le","topics":[{"type":"topic","name":"Le and Gascuel 2008","url":"Le_and_Gascuel_2008.htm"}]},{"type":"key","name":"Le S.Q","topics":[{"type":"topic","name":"Le and Gascuel 2008","url":"Le_and_Gascuel_2008.htm"}]},{"type":"key","name":"Least Squares","topics":[{"type":"topic","name":"Analyze User Tree by Least Squares","url":"Analyze_User_Tree_by_Least_Squares.htm"}]},{"type":"key","name":"Leave","topics":[{"type":"topic","name":"Constructing a Timetree (ML)","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Constructing_a_Timetree_(ML).htm"}],"keys":[{"type":"key","name":"Min Divergence Time Edit","topics":[{"type":"topic","name":"Constructing a Timetree (ML)","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Constructing_a_Timetree_(ML).htm"}]}]},{"type":"key","name":"lect","topics":[{"type":"topic","name":"Constructing a Timetree (ML)","url":"Part_I_Getting_Started/A_Walk_Through_MEGA/Constructing_a_Timetree_(ML).htm"}]}]};
window.rh.model.publish(rh.consts('KEY_TEMP_DATA'), index, { sync:true });
})();