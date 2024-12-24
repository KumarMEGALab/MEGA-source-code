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

{ Turbo Pascal Unit:  TreeExplorer_HC.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in TreeExplorer_HC.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever TreeExplorer_HC.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ TreeExplorer_HC.hh are the 'master values' and if you    }
{ modify the value in TreeExplorer_HC.hh and then          }
{ save the TreeExplorer_HC.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit TreeExplorer_HC;
   Interface
   Const
	HC_Tree_Explorer = 5500;
	HC_Divergence_Time_Dialog_Box = 5501;
	HC_File_Menu_in_Tree_Explorer = 5502;
	HC_Image_Menu_in_Tree_Explorer = 5503;
	HC_Subtree_in_Tree_Explorer = 5504;
	HC_View_Menu_in_Tree_Explorer = 5505;
	HC_Compute_in_Tree_Explorer = 5506;
	HC_Format_dialog_box_in_Tree_Explorer = 5507;
	HC_Tree_tab_in_Format_dialog_box = 5508;
	HC_Branch_tab_in_Format_dialog_box = 5509;
	HC_Taxon_Name_tab_in_Format_dialog_box = 5510;
	HC_Scale_Bar_tab_in_Format_dialog_box = 5511;
	HC_TEx_Information_Box = 5512;
	HC_Subtree_Drawing_Options = 5513;
	Cutoff_Values_Tab_in_format_dialog_box = 5514;
	RH_Tree_Topology_Editor = 5515;
        RelTime_Tool = 5516;
        HC_CALIBRATE_MOLECULAR_CLOCK = 5517;
	Implementation
	end.
