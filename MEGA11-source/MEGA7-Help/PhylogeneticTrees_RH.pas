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

﻿{ Turbo Pascal Unit:  PhylogeneticTrees_RH.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in PhylogeneticTrees_RH.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever PhylogeneticTrees_RH.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ PhylogeneticTrees_RH.hh are the 'master values' and if you    }
{ modify the value in PhylogeneticTrees_RH.hh and then          }
{ save the PhylogeneticTrees_RH.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit PhylogeneticTrees_RH;
   Interface
   Const
	RH_Phylogenetic_Inference = 2001;
	RH_UPGMA = 2002;
	RH_Neighbor_Joining_Method = 2003;
	RH_Minimum_Evolution = 2004;
	RH_Maximum_Parsimony_Method = 2005;
	RH_Alignment_Gaps_and_Sites_with_Missing_Information_Trees = 2006;
	RH_Condensed_Trees = 2007;
	RH_General_Comments_on_Statistical_Tests = 2008;
	Overview = 2010;
	Calibration_Dialog = 2011;
	Gene_Duplication_Wizard = 2030;
	Gene_Duplication_Inference = 2031;
	Implementation
	end.
