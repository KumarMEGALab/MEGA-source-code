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

﻿{ Turbo Pascal Unit:  TextEditorHelp_HC.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in TextEditorHelp_HC.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever TextEditorHelp_HC.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ TextEditorHelp_HC.hh are the 'master values' and if you    }
{ modify the value in TextEditorHelp_HC.hh and then          }
{ save the TextEditorHelp_HC.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit TextEditorHelp_HC;
   Interface
   Const
	HC_Text_Editor = 5000;
	HC_TE_New = 5001;
	HC_TE_Save = 5002;
	HC_TE_Open = 5003;
	HC_TE_Save_As = 5004;
	HC_TE_Print = 5005;
	HC_TE_Exit = 5006;
	HC_TE_Undo = 5007;
	HC_TE_Cut = 5008;
	HC_TE_Copy = 5009;
	HC_TE_Paste = 5010;
	HC_TE_Font = 5011;
	HC_TE_Find = 5012;
	HC_TE_Find_again = 5013;
	HC_TE_Replace = 5014;
	HC_Reopen_in_Text_Editor = 5015;
	HC_Close_File_in_Text_Editor = 5016;
	HC_Delete_in_Text_Editor = 5017;
	HC_Select_All_in_Text_Editor = 5018;
	HC_Go_to_Line = 5019;
	HC_Show_Line_Numbers_in_Text_Editor = 5020;
	HC_Word_Wrap_in_Text_Editor = 5021;
	HC_Convert_to_Mega_Format_in_Text_Editor = 5022;
	HC_Convert_to_MEGA_Format_Dialog_Box = 5023;
	HC_Format_Selected_Sequence = 5024;
	HC_Reverse_Complement = 5025;
	HC_Copy_Screenshot_to_Clipboard = 5026;
	Implementation
	end.
