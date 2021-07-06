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

﻿{ Turbo Pascal Unit:  popuprefs_RH.pas                  }
{                                                 }
{ This is an interface unit containing integer    }
{ mappings of Topic IDs (names of Help            }
{ Topics) which are located in popuprefs_RH.rtf     }
{                                                 }
{ This file is re-written by RoboHelp           }
{ whenever popuprefs_RH.rtf is saved.   	          }
{                                                 }
{ However, the numeric values stored in           }
{ popuprefs_RH.hh are the 'master values' and if you    }
{ modify the value in popuprefs_RH.hh and then          }
{ save the popuprefs_RH.rtf again, this file will }
{ reflect the changed values.                     }
{                                                 }

Unit popuprefs_RH;
   Interface
   Const
	Saitou_and_Nei_1987 = 1001;
	Jukes_and_Cantor_1969 = 1002;
	Tajima_1993 = 1003;
	Tajima_and_Nei_1984 = 1004;
	Kimura_1980 = 1005;
	Tamura_1992 = 1006;
	Tamura_and_Nei_1993 = 1007;
	Li_et_al_1985 = 1008;
	Nei_and_Gojobori_1986 = 1009;
	Pamilo_and_Bianchi_1993 = 1010;
	Tanaka_and_Nei_1989 = 1011;
	Nei_and_Jin_1989 = 1012;
	Nei_et_al_1976 = 1013;
	Dayhoff_1978 = 1014;
	Goldman_1993 = 1015;
	Tamura_1994 = 1016;
	Nei_1991 = 1017;
	Sharp_et_al_1986 = 1018;
	Tajima_1983 = 1019;
	Nei_1986 = 1020;
	Pamilo_and_Nei_1988 = 1021;
	Felsenstein_1988 = 1022;
	Felsenstein_1986 = 1023;
	Sneath_and_Sokal_1973 = 1024;
	Fitch_and_Margoliash_1967 = 1025;
	Felsenstein_1993 = 1026;
	Estabrook_et_al_1975 = 1027;
	Lake_1987 = 1028;
	Swofford_1993 = 1029;
	Tateno_et_al_1982 = 1030;
	Sourdis_and_Krimbas_1987 = 1031;
	Rzhetsky_and_Nei_1992 = 1032;
	Rzhetsky_and_Nei_1993 = 1033;
	Studier_and_Keppler_1988 = 1034;
	Maddison_and_Maddison_1992 = 1035;
	Fitch_1971 = 1036;
	Eck_and_Dayhoff_1966 = 1037;
	Sankoff_and_Cedergren_1983 = 1038;
	Felsenstein_1978 = 1039;
	Hendy_and_Penny_1989 = 1040;
	DeBry_1992 = 1041;
	Tateno_et_al_1994 = 1042;
	Sourdis_and_Nei_1988 = 1043;
	Hendy_and_Penny_1982 = 1044;
	Kishino_and_Hasegawa_1989 = 1045;
	Efron_1982 = 1046;
	Felsenstein_1985 = 1047;
	Penny_and_Hendy_1985 = 1048;
	Felsenstein_and_Kishino_1993 = 1049;
	Hillis_and_Bull_1993 = 1050;
	Nei_et_al_1985 = 1051;
	Hedges_et_al_1992 = 1052;
	Nei_and_Kumar_2000 = 1053;
	Press_et_al_1993 = 1054;
	Comeron_1995 = 1055;
	Dopazo_1994 = 1056;
	Kumar_et_al_1993 = 1057;
	Zhang_et_al_1998 = 1058;
	Zhang_et_al_1997 = 1059;
	Zhang_and_Gu_1998 = 1060;
	Swofford_1998 = 1061;
	Yang_1999 = 1062;
	Gu_and_Zhang_1997 = 1063;
	Hillis_et_al_1996 = 1064;
	Li_1997 = 1065;
	Page_and_Holmes_1998 = 1066;
	Nei_et_al_1998 = 1067;
	Takahashi_and_Nei_2000 = 1068;
	Li_1993 = 1069;
	Swofford_et_al_1996 = 1070;
	Tajima_1989 = 1071;
	Kumar_and_Gadagkar_2001 = 1072;
	Tamura_and_Kumar_2002 = 1073;
	RH_Dayhoff_1979 = 1074;
	RH_Jones_et_al_1992 = 1075;
	Tamura_et_al_2004 = 1076;
	Tamura_et_al_2007 = 1077;
	Takezaki_et_al_1995 = 1078;
	Tajima_and_Nei_1982 = 1079;
	Purdom_et_al_2000 = 1080;
	Zuckerkandl_and_Pauling_1965 = 1081;
	Tamura_et_al_2012 = 1082;
	Kumar_et_al_2012 = 1083;
	Le_and_Gascuel_2008 = 1084;
	Kumar_et_al_EvoD_2012 = 1089;
	Adzubhei_et_al_2010 = 1090;
	Pauline_and_Henikoff_2010 = 1091;
	Implementation
	end.
