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

﻿{ Turbo Pascal Unit:  ErrorMessages_HC.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in ErrorMessages_HC.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever ErrorMessages_HC.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ ErrorMessages_HC.hh are the 'master values' and if you    }
{ modify the value in ErrorMessages_HC.hh and then          }
{ save the ErrorMessages_HC.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit ErrorMessages_HC;
   Interface
   Const
	HC_Blank_Names_Are_Not_Permitted = 4500;
	HC_Domains_Cannot_Overlap = 4501;
	HC_Fisher_s_Exact_Test_Has_Failed = 4502;
	HC_Gamma_Correction_Failed_Because_p_0_99 = 4503;
	HC_Gene_Names_Must_Be_Unique = 4504;
	HC_Inapplicable_Computation_Requested = 4505;
	HC_Incorrect_Command_Used = 4506;
	HC_Invalid_special_symbol_in_molecular_sequences = 4507;
	HC_Jukes_Cantor_Correction_Failed = 4508;
	HC_Kimura_1980_Distance_Correction_Failed = 4509;
	HC_Missing_data_or_invalid_distances_in_the_matrix = 4510;
	HC_No_Common_Sites = 4511;
	HC_Not_Enough_Groups_Selected = 4512;
	HC_Not_Enough_Taxa_Selected = 4513;
	HC_Not_Yet_Implemented = 4514;
	HC_p_distance_is_found_to_be_1 = 4515;
	HC_Poisson_Correction_Failed_because_p_0_99 = 4516;
	HC_Tajima_Nei_Distance_Could_Not_Be_Computed = 4517;
	HC_Tamura_1992_Correction_Could_Not_Be_Applied = 4518;
	HC_Tamura_Nei_Distance_Could_Not_Be_Computed = 4519;
	HC_LogDet_Distance_Could_Not_Be_Computed = 4520;
	HC_Dayhoff_Distance_Could_Not_Be_Computed = 4521;
	HC_Equal_Input_Correction_Failed = 4522;
	HC_Unexpected_Error = 4523;
	HC_User_Stopped_Computation = 4524;
	HC_Data_File_Parsing_Error = 4525;
	Implementation
	end.
