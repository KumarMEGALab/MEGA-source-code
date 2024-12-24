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

{ Turbo Pascal Unit:  DistanceMethods_HC.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in DistanceMethods_HC.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever DistanceMethods_HC.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ DistanceMethods_HC.hh are the 'master values' and if you    }
{ modify the value in DistanceMethods_HC.hh and then          }
{ save the DistanceMethods_HC.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit DistanceMethods_HC;
   Interface
   Const
	HC_Models_for_estimating_distances = 4000;
	HC_No_of_differences_Nucleotide = 4001;
	HC_p_distance_Nucleotide = 4002;
	HC_Jukes_Cantor_distance = 4003;
	HC_Jukes_Cantor_Gamma_distance = 4004;
	HC_Tajima_Nei_distance = 4005;
	HC_Tajima_Nei_distance_Gamma_rates = 4006;
	HC_Tajima_Nei_Distance_Heterogeneous_patterns = 4007;
	HC_Tajima_Nei_Distance_Gamma_Rates_and_Heterogeneous_patterns = 4008;
	HC_Kimura_2_parameter_distance = 4009;
	HC_Kimura_2_parameter_Gamma_distance = 4010;
	HC_Tamura_3_parameter_distance = 4011;
	HC_Tamura_3_parameter_Gamma = 4012;
	HC_Tamura_3_parameter_Heterogeneous_patterns = 4013;
	HC_Tamura_3_parameter_Gamma_rates_and_Heterogeneous_patterns = 4014;
	HC_Tamura_Nei_distance = 4015;
	HC_Tamura_Nei_distance_with_Gamma_model = 4016;
	HC_Tamura_Nei_distance_Heterogeneous_Patterns = 4017;
	HC_Tamura_Nei_distance_Gamma_rates_and_Heterogeneous_patterns_ = 4018;
	HC_Nei_Gojobori_Method = 4019;
	HC_Modified_Nei_Gojobori_Method = 4020;
	HC_Li_Wu_Luo_Method = 4021;
	HC_Pamilo_Bianchi_Li_Method = 4022;
	HC_Kumar_Comeron_Method = 4023;
	HC_No_of_differences_Amino_acids = 4024;
	HC_p_distance_Amino_acids = 4025;
	HC_Poisson_Correction_distance = 4026;
	HC_Gamma_distance_Amino_acids = 4027;
	HC_bootstrap_method_to_compute_standard_error = 4028;
	HC_Pseudorandom_number_generator = 4029;
	HC_Computing_the_Gamma_Parameter = 4030;
	RH_Alignment_Gaps_and_Sites_with_Missing_Information = 4031;
	HC_Dayhoff_Model = 4032;
	HC_Dayhoff_and_JTT_distances_Gamma_rates = 4033;
	HC_Equal_Input_Model = 4034;
	HC_Equal_Input_Model_Gamma = 4035;
	HC_Equal_Input_Model_Heterogeneous_Patterns = 4036;
	HC_Equal_Input_Model_Gamma_rates_and_Heterogeneous_Patterns = 4037;
	HC_MCL = 4038;
	Maximum_Composite_Likelihood_Gamma = 4039;
	Maximum_Composite_Likelihood_Heterogeneous_Patterns = 4040;
	Maximum_Composite_Likelihood_Gamma_Rates_and_Heterogeneous_Patterns = 4041;
	Implementation
	end.
