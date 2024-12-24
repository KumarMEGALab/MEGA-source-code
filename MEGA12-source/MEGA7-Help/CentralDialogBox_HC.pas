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

{ Turbo Pascal Unit:  CentralDialogBox_HC.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in CentralDialogBox_HC.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever CentralDialogBox_HC.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ CentralDialogBox_HC.hh are the 'master values' and if you    }
{ modify the value in CentralDialogBox_HC.hh and then          }
{ save the CentralDialogBox_HC.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit CentralDialogBox_HC;
   Interface
   Const
	HC_MP_Analysis_Option_Dialog = 2501;
	HC_Dist_Analysis_Option_Dialog = 2502;
	HC_NJ_Analysis_Options = 2503;
	HC_ME_Analysis_Options = 2504;
	HC_Fisher_Exact_Test_Analysis_Options = 2505;
	HC_Z_test_Analysis_Options = 2506;
	HC_Disparity_Index_Analysis_Options = 2507;
	HC_Include_Sites_Option = 2508;
	HC_Distance_Model_Options = 2509;
	Analysis_Options = 2510;
	Nearest_Neighbor_Interchange_NNI_ = 2511;
	HC_ML_Analysis_Options = 2585;
	Gamma_parameter = 2586;
	HC_CodonOmega_Analysis_Options = 2588;
	HC_Test_of_Selection_Tab = 2500;
	Implementation
	end.
