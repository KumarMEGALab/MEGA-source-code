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

{ Turbo Pascal Unit:  Walk_Through_MEGA.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in Walk_Through_MEGA.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever Walk_Through_MEGA.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ Walk_Through_MEGA.hh are the 'master values' and if you    }
{ modify the value in Walk_Through_MEGA.hh and then          }
{ save the Walk_Through_MEGA.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit Walk_Through_MEGA;
   Interface
   Const
	HC_Introduction_to_Walk_Through_MEGA = 6000;
	Constructing_Trees_from_Distance_Data = 6001;
	Computing_Statistical_Quantities_for_Nucleotide_Sequences = 6002;
	Estimating_Evolutionary_Distances_from_Nucleotide_Sequences = 6003;
	Constructing_Trees_and_Selecting_OTUs_from_Nucleotide_Sequences = 6004;
	Tests_of_the_Reliability_of_a_Tree_Obtained = 6005;
	Test_of_Positive_Selection = 6006;
	Creating_Multiple_Sequence_Alignments = 6007;
	Working_With_Genes_and_Domains = 6008;
	Managing_Taxa_With_Groups = 6009;
	Mega_Basics_HC = 6010;
	Constructing_Likelihood_Trees_HC = 6011;
	Editing_Data_Files_HC = 6012;
	HC_First_Time_User = 6013;
	HC_First_Time_User_CC = 6014;
	Constructing_a_Time_Tree_ML_ = 6015;
	Inferring_Gene_Duplications = 6016;
        HC_Analyze_and_Prototyper_Modes = 6017;
	Implementation
	end.
