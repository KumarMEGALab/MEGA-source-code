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

{ Turbo Pascal Unit:  FAQs_RH.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in FAQs_RH.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever FAQs_RH.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ FAQs_RH.hh are the 'master values' and if you    }
{ modify the value in FAQs_RH.hh and then          }
{ save the FAQs_RH.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit FAQs_RH;
   Interface
   Const
	RH_Finding_number_of_sites_in_pairwise_comparisons = 3066;
	RH_Writing_only_4_fold_degenerate_sites_to_an_output_file = 3067;
	RH_Computing_statistics_on_only_highlighted_sites_in_Data_Explorer = 3068;
	RH_get_more_information_about_the_codon_based_Z_test_for_selection = 3069;
	RH_Menus_in_MEGA_are_so_short = 3070;
	How_can_I_ignore_the_current_update_available_messag_in_MEGA_s_main_window = 3071;
	How_do_I_prevent_the_MEGA_Update_Available_message_showing_up = 3072;
	Implementation
	end.
