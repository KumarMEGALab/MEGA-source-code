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

﻿{ Turbo Pascal Unit:  myPEG_RH.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in myPEG_RH.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever myPEG_RH.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ myPEG_RH.hh are the 'master values' and if you    }
{ modify the value in myPEG_RH.hh and then          }
{ save the myPEG_RH.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit myPEG_RH;
   Interface
   Const
	EvoD_Server = 3558;
	Mutation_Explorer = 3559;
	Gene_Search_Tab = 3560;
	Prediction_Data_Tab = 3561;
	Coordinate_Info_Tab = 3562;
	Mutation_Detail_View = 3563;
	Sequence_Data_Explorer_Window = 3564;
	myPEG_Overview = 3565;
	Upload_a_text_file_with_the_coordinate_information_for_all_nSNVs_of_interest = 3566;
	Manually_enter_the_coordinate_information_using_the_integrated_entry_form = 3567;
	Specify_the_coordinate_information_using_the_Sequence_Data_Explorer = 3568;
	Implementation
	end.
