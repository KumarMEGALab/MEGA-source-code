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

﻿{ Turbo Pascal Unit:  Glossary_RH.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in Glossary_RH.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever Glossary_RH.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ Glossary_RH.hh are the 'master values' and if you    }
{ modify the value in Glossary_RH.hh and then          }
{ save the Glossary_RH.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit Glossary_RH;
   Interface
   Const
	RH_Alignment_Gaps = 2512;
	RH_Bifurcating_Tree = 2513;
	RH_Branch = 2514;
	RH_Branch_and_Bound_algorithm = 2515;
	RH_Close_Neighbor_Interchange_CNI = 2516;
	RH_Codon = 2517;
	RH_Complete_Deletion_Option = 2518;
	RH_Compress_Uncompress = 2519;
	RH_Condensed_Tree = 2520;
	RH_Consensus_Tree = 2521;
	RH_Constant_Site = 2522;
	RH_Degeneracy = 2523;
	RH_Domains = 2524;
	RH_Exon = 2525;
	RH_Extant_Taxa = 2526;
	RH_Flip = 2527;
	RH_Format_command = 2528;
	RH_Gamma_parameter = 2529;
	RH_Gene = 2530;
	RH_genetic_codes = 2531;
	RH_Groups_of_taxa = 2532;
	RH_Indels = 2533;
	RH_Independent_Sites = 2534;
	RH_Inferred_tree = 2535;
	RH_Intron = 2536;
	RH_Labeled_Sites = 2537;
	RH_Max_mini_branch_and_bound_search = 2538;
	RH_Maximum_Parsimony_Principle = 2539;
	RH_Mid_point_rooting = 2540;
	RH_Min_mini_algorithm = 2541;
	RH_Monophyletic = 2542;
	RH_mRNA = 2543;
	RH_Node = 2544;
	RH_Nonsynonymous_change = 2545;
	RH_Orthologous_Genes = 2546;
	RH_Outgroup = 2547;
	RH_Pairwise_deletion_option = 2548;
	RH_Parsimony_informative_site = 2549;
	RH_Polypeptide = 2550;
	RH_Positive_selection = 2551;
	RH_Protein_parsimony = 2552;
	RH_Purifying_selection = 2553;
	RH_Purines = 2554;
	RH_Pyrimidines = 2555;
	RH_Random_addition_trees = 2556;
	RH_Rooted_tree = 2557;
	RH_RSCU = 2558;
	RH_Singleton_Sites = 2559;
	RH_Site_Label = 2560;
	RH_Statements_in_input_files = 2561;
	RH_Swap = 2562;
	RH_Synonymous_change = 2563;
	RH_Taxa = 2564;
	RH_Topological_distance = 2565;
	RH_Topology = 2566;
	RH_Transition = 2567;
	RH_Transition_Matrix = 2568;
	RH_Transition_Transversion_Ratio_R = 2569;
	RH_Translation = 2570;
	RH_Transversion = 2571;
	RH_tree_length = 2572;
	RH_Unrooted_tree = 2573;
	RH_Variable_site = 2574;
	RH_OLS_branch_length_estimates = 2575;
	RH_NCBI = 2576;
	RH_ABI = 2577;
	RH_Staden = 2578;
	RH_ClustalW = 2579;
	RH_Newick_Format = 2580;
	RH_Composition_Distance = 2581;
	RH_Disparity_Index = 2582;
	RH_Alignment_session = 2583;
	Maximum_Composite_Likelihood = 2584;
	RH_Partial_Deletion_Option = 2587;
	Subtree_Pruning_Regrafting = 2589;
	Filter_for_zero = 2590;
	Tree_Bisection_Reconnection_TBR_ = 2591;
	Disparity_Index_Test = 2592;
	MEGA_CC = 2593;
	Data_Coverage = 2594;
	Filipski_et_al_2014 = 2595;
	Filipski_et_al_2014_1 = 2596;
	Implementation
	end.
