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


MEGA (Molecular Evolutionary Genetics Analysis) Computational Core
Authors: Koichiro Tamura, Glen Stecher, and Sudhir Kumar
URL: http://www.megasoftware.net/
License: See the usageAgreement.txt file
 
MEGA-CC is the command-line version of MEGA that implements its core
analysis functions and is useful for iterative and automated execution.
MEGA-CC requires as input, a MEGA Analysis Options (.mao) file which can only
be generated using the MEGA X GUI application. The .mao file specifies the 
analysis to run as well as the analysis settings to use. If you do not already
have MEGA X, it can be downloaded from http://www.megasoftware.net. The MEGA X
GUI application also contains documentation and a tutorial for using MEGA-CC.

An example of how to run MEGA-CC is:

  megacc -a myMaoFile.mao -d mySequenceAlignment.fas -o myOutput

In addition to the megacc binary executable, a folder of example data 
files (Examples), a usageAgreement.txt file, and a man page (megacc.1) are included in the MEGA-CC
archive.

