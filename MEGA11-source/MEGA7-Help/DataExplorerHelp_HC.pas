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

﻿{ Turbo Pascal Unit:  DataExplorerHelp_HC.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in DataExplorerHelp_HC.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever DataExplorerHelp_HC.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ DataExplorerHelp_HC.hh are the 'master values' and if you    }
{ modify the value in DataExplorerHelp_HC.hh and then          }
{ save the DataExplorerHelp_HC.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit DataExplorerHelp_HC;
   Interface
   Const
	HC_Sequence_Data_Explorer = 3500;
	HC_Export_Data_in_Sequence_Data_Explorer = 3501;
	HC_Translate_Untranslate_in_Sequence_Data_Explorer = 3502;
	HC_Setup_Select_Genes_Domains_in_Sequence_Data_Explorer_ = 3503;
	HC_Select_Genetic_Code_Table_in_Sequence_Data_Explorer = 3504;
	HC_Setup_Select_Taxa_Groups_in_Sequence_Data_Explorer = 3505;
	HC_Quit_Data_Viewer = 3506;
	HC_Show_Only_Selected_Sequences = 3507;
	HC_Use_Identical_Symbol = 3508;
	HC_Color_Cells = 3509;
	HC_Sort_Sequences = 3510;
	HC_Sort_Sequences_By_Sequence_Name = 3511;
	HC_Sort_Sequences_by_Group_Name = 3512;
	HC_Sort_Sequences_by_Group_and_Sequence_Names = 3513;
	HC_Sort_Sequences_As_per_Taxa_Group_Organizer = 3514;
	HC_Restore_Input_Order = 3515;
	HC_Show_Sequence_Names = 3516;
	HC_Show_Group_Names = 3517;
	HC_Change_Font = 3518;
	HC_Highlight_Conserved_Sites = 3519;
	HC_Highlight_Variable_Sites = 3520;
	HC_Highlight_Parsimony_Informative_Sites = 3521;
	HC_Highlight_Singleton_Sites = 3522;
	HC_Highlight_0_fold_Degenerate_Sites = 3523;
	HC_Highlight_4_fold_Degenerate_Sites = 3524;
	HC_Highlight_2_fold_Degenerate_Sites = 3525;
	HC_Nucleotide_Composition = 3526;
	HC_Nucleotide_Pair_Frequencies = 3527;
	HC_Codon_Usage = 3528;
	HC_Amino_Acid_Composition = 3529;
	HC_Use_All_Selected_Sites = 3530;
	HC_Use_only_Highlighted_Sites = 3531;
	HC_Data_Menu_in_sequence_Data_Explorer = 3532;
	HC_Display_Menu_in_Sequence_Data_Explorer = 3533;
	HC_Highlight_Menu_in_Sequence_Data_Explorer = 3534;
	HC_Stats_Menu_in_Sequence_Data_Explorer = 3535;
	HC_Selecting_Data_Subset_for_Sequence_Data = 3536;
	HC_Dist_Exp_Options_dialog_box = 3537;
	HC_Distance_Data_Explorer = 3538;
	HC_Dist_Exp_File_Menu = 3539;
	HC_Dist_Exp_Display_Menu = 3540;
	HC_Dist_Exp_Average_Menu = 3541;
	HC_Distance_Matrix_Explorer = 3542;
	HC_Dist_Matrix_Exp_Average_Menu = 3543;
	HC_Dist_Matrix_Exp_Display_Menu = 3544;
	HC_Dist_Matrix_Exp_File_Menu = 3545;
	Highlight_Coverage = 3546;
	Highlight_CpG_TpG_CpA = 3547;
	Save_Session_in_sequence_Data_Explorer_ = 3548;
	Display_Results_in_XL_CSV_Text = 3549;
	HC_Find_Prev_Name = 3550;
	HC_Find_Next_Name = 3551;
	HC_Find_Sequence_Name = 3552;
	HC_Hide_Name_Result = 3553;
	HC_Find_Prev_Motif = 3554;
	HC_Find_Next_Motif = 3555;
	Find_Motif = 3556;
	HC_Hide_Motif = 3557;
        HC_Display_Results_in_XL_CSV_Text = 3558;
        HC_COMMAND_STATEMENTS = 3099;
	Implementation
	end.
