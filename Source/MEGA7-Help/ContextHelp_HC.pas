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

﻿{ Turbo Pascal Unit:  ContextHelp_HC.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in ContextHelp_HC.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever ContextHelp_HC.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ ContextHelp_HC.hh are the 'master values' and if you    }
{ modify the value in ContextHelp_HC.hh and then          }
{ save the ContextHelp_HC.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit ContextHelp_HC;
   Interface
   Const
	HC_Main_MEGA_Window = 3000;
	HC_File_Menu = 3001;
	HC_Open_Data = 3002;
	HC_Export_Data = 3003;
	HC_Close_Data = 3004;
	HC_Reopen_Data = 3005;
	HC_Convert_To_MEGA_Format = 3006;
	HC_Printer_Setup = 3007;
	HC_Exit = 3008;
	HC_Data_Menu = 3009;
	HC_Data_Explorer = 3010;
	HC_Setup_Taxa_Groups = 3011;
	HC_Setup_Taxa_Groups_Dlg = 3012;
	HC_Setup_Genes_Domains = 3013;
	HC_Select_Genetic_Code_Table = 3014;
	HC_Select_Preferences = 3015;
	HC_Handling_Gaps_and_Missing_Data = 3016;
	HC_Include_Codon_Positions = 3017;
	HC_Include_Labelled_Sites = 3018;
	HC_Choose_Model = 3019;
	HC_Distance_Menu = 3020;
	HC_Pairwise_Distances = 3021;
	HC_Overall_Mean = 3022;
	HC_Within_Group_Mean = 3023;
	HC_Between_Groups_Means = 3024;
	HC_Net_Between_Groups_Means = 3025;
	HC_Sequence_Diversity = 3026;
	HC_Pattern_Menu = 3027;
	HC_Phylogeny_Menu = 3028;
	HC_Construct_Phylogeny = 3029;
	HC_UPGMA = 3030;
	HC_Neighbor_Joining = 3031;
	HC_Minimum_Evolution = 3032;
	HC_Maximum_Parsimony = 3033;
	HC_Display_Saved_Tree_Session = 3034;
	HC_Display_Newick_Trees_from_File = 3035;
	HC_Bootstrap_Test_Phylogeny = 3036;
	HC_Interior_Branch_Test = 3037;
	HC_Tajima_Test_Relative_Rate = 3038;
	HC_Selection_Menu = 3039;
	HC_Tajima_Test_of_Neutrality = 3040;
	HC_Fisher_Exact_Test_Of_Selection = 3041;
	HC_Z_Test_of_Selection = 3042;
	HC_Alignment_Menu = 3043;
	HC_Alignment_Explorer_CLUSTAL = 3044;
	HC_Open_Saved_Alignment_Session = 3045;
	HC_Browse_Databanks = 3046;
	HC_BLAST_Search = 3047;
	HC_Show_Web_Browser = 3048;
	HC_Edit_Sequencer_Files = 3049;
	HC_Help_Menu = 3050;
	HC_Index = 3051;
	HC_About = 3052;
	HC_Citing_MEGA_in_Publications = 3053;
	HC_Select_Distance_Options_Dialog = 3054;
	HC_Genes_Domains_Dialog = 3055;
	HC_Select_Genetic_Code_Table_Dialog = 3056;
	HC_Code_table_editor = 3057;
	HC_Input_Data_Format = 3058;
	HC_Molecular_Clock_Test_ML_ = 3059;
	HC_Compute_MCL_Substitution_Matrix = 3060;
	HC_Compute_MCL_Transversion_Transition_bias = 3061;
	HC_Analyze_User_Tree_by_Least_Squares = 3062;
	HC_Conduct_Interior_Branch_Test_by_Least_Squares = 3063;
	HC_Get_Data_from_Saved_Session = 3064;
	HC_Open_A_File = 3065;
	HC_Save_Data_Session_To_File = 3066;
	HC_Maximum_Likelihood_ML_ = 3067;
	HC_Inferring_Ancestral_Sequences_ML_ = 3068;
	HC_Inferring_Ancestral_Sequences_MP_ = 3069;
	HC_Find_Best_DNA_Protein_Models_ML_ = 3070;
	HC_Estimate_Substitution_Matrix_ML_ = 3071;
	HC_Estimate_Transition_Transversion_Bias_ML_ = 3072;
	HC_Estimate_Rate_Variation_among_Sites_ML_ = 3073;
	HC_Analyze_User_Tree_by_Maximum_Likelihood = 3074;
	HC_Analyze_User_Tree_by_Parsimony = 3075;
	HC_Estimate_Position_by_Position_Rates_ML_ = 3076;
	HC_Estimate_Selection_for_each_Codon_HyPhy_ = 3077;
        HC_TIMETREE_WIZARD = 3093;
        HC_GENEDUPS_WIZARD = 3094;
        HC_CALIBRATIONS_EDITOR = 3095;
        HC_MUTATION_EXPLORER = 3096;
        HC_EVOLUTIONARY_PROBABILITY = 3097;
        HC_CORRTEST = 3098;
        HC_COMMAND_STATEMENTS = 3099;

	Implementation
	end.
