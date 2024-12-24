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

{ Turbo Pascal Unit:  BasicStatistics_RH.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in BasicStatistics_RH.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever BasicStatistics_RH.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ BasicStatistics_RH.hh are the 'master values' and if you    }
{ modify the value in BasicStatistics_RH.hh and then          }
{ save the BasicStatistics_RH.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit BasicStatistics_RH;
   Interface
   Const
	RH_Basic_Sequence_Statistics = 1085;
	RH_Nucleotide_and_Amino_Acid_Compositions = 1086;
	RH_Nucleotide_Pair_Frequencies = 1087;
	RH_Codon_Usage = 1088;
	Implementation
	end.
