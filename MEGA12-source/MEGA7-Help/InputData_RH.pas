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

{ Turbo Pascal Unit:  InputData_RH.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in InputData_RH.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever InputData_RH.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ InputData_RH.hh are the 'master values' and if you    }
{ modify the value in InputData_RH.hh and then          }
{ save the InputData_RH.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit InputData_RH;
   Interface
   Const
	RH_MEGA_Format = 1501;
	RH_Common_Features = 1502;
	RH_Key_Words = 1503;
	RH_Rules_for_Taxa_Names = 1504;
	RH_Rules_for_Title_Statement = 1505;
	RH_Rules_for_Description_Statement = 1506;
	RH_General_Considerations_Sequence_Data_Format = 1507;
	RH_General_Considerationss_Distance_Data_Formats = 1508;
	RH_Editing_Distance_Data = 1509;
	RH_Editing_Tree_Data = 1510;
	RH_Rules_for_Format_Statement = 1511;
	RH_Keywords_for_Format_Statement_Sequence_data = 1512;
	RH_Keywords_for_Format_Statement_Distance_data = 1513;
	RH_IUPAC_single_letter_codes = 1514;
	RH_Writing_Comments = 1515;
	RH_Writing_Command_Statements_for_Defining_Genes_and_Domains = 1516;
	RH_Writing_Command_Statements_for_Defining_Groups_of_Taxa = 1517;
	RH_Keywords_for_Command_Statements_Genes_Domains = 1518;
	RH_Built_in_Genetic_Codes = 1519;
	RH_Adding_Modifying_Genetic_Code_Tables = 1520;
	RH_Computing_Statistical_Attributes_of_the_Genetic_Code = 1521;
	RH_Importing_Data_From_Other_Formats = 1522;
	RH_Converting_IG_Format_Files = 1523;
	RH_Converting_NBRF_Format = 1524;
	RH_Converting_XML_format = 1525;
	RH_Converting_PIR_Format = 1526;
	RH_Convert_GCG_Format = 1527;
	RH_Converting_MSF_Format = 1528;
	RH_Converting_Nexus_Format = 1529;
	RH_Converting_FASTA_format = 1530;
	RH_Converting_PHYLIP_Format = 1531;
	RH_Converting_PHYLIP_Noninterleaved_Format = 1532;
	RH_Converting_CLUSTAL_Format = 1533;
	MEGA_Saved_Sessions = 1534;
	Calibration_File_Format = 1535;
	Defining_Species_and_Populations = 1536;
	Implementation
	end.
