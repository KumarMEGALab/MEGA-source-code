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

{ Turbo Pascal Unit:  AlnBuilder_HC.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in AlnBuilder_HC.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever AlnBuilder_HC.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ AlnBuilder_HC.hh are the 'master values' and if you    }
{ modify the value in AlnBuilder_HC.hh and then          }
{ save the AlnBuilder_HC.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit AlnBuilder_HC;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

Interface
  Const
    HC_Alignment_Builder = 2000;
    HC_Toolbars_in_Alignment_Explorer = 2012;
    HC_Data_Menu_in_Alignment_Explorer = 2013;
    HC_Edit_Menu_in_Alignment_Explorer = 2014;
    HC_Search_Menu_in_Alignment_Explorer = 2015;
    HC_Alignment_Menu_in_Alignment_Explorer = 2016;
    HC_Web_Menu_in_Alignment_Explorer = 2017;
    HC_Sequencer_Menu_in_Alignment_Explorer = 2018;
    HC_Display_Menu_in_Alignment_Explorer = 2019;
    HC_CLUSTALW_Options_DNA_Dialog = 2020;
    HC_CLUSTALW_Options_Protein_Dialog = 2021;
    Aligning_coding_sequences_via_protein_sequences = 2022;
    HC_Web_Browser = 2023;
    HC_About_BLAST = 2024;
    HC_CLUSTALW = 2025;
    HC_Trace_Data_File_Viewer_Editor = 2026;
    About_MUSCLE = 2027;
    MUSCLE_Options_DNA_ = 2028;
    MUSCLE_Options_Protein_ = 2029;
    Mafft = 2009;


Implementation


end.
