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

unit ITimeTree;

interface

uses
  Classes, SysUtils, MAnalysisInfo, MCalibrationData;

type
  ITimetreeWizard = interface
    ['{55832426-69B0-4CD8-9E10-93DFC1444260}']
    procedure InitAnalysisInfo;
    function LoadTree: Boolean;
    function LoadSeqData: Boolean;
    function SetOutgroupViaTaxaGpsDlg: Boolean;
    function SetOutgroupViaTE: Boolean;
    function GetCalibrations: Boolean;
    function GetAnalysisPreferences: Boolean;
    function LaunchAnalysis: Boolean;
  end;

  ITreeLoader = interface
    ['{0563611C-1754-4D4D-AAB9-E06755C6472F}']
    function LoadTree(var AnalysisInfo: TAnalysisInfo; const FileName: String=''): Boolean;
    function GetMsg: String;
  end;

  IGetOutgroup = interface
    ['{6AE153BA-3472-462B-8E7E-EE9A42E39180}']
    function GetOutGroup(var AnalysisInfo: TAnalysisInfo): Boolean;
    function GetMsg: String;
  end;

  IGetCalibrations = interface
    ['{CD670EB4-CE32-4AA8-BAC6-1EB55AE7125B}']
    function GetCalibrations(var Calibs: TCalibrations): Boolean;
    function GetMsg: String;
  end;

  IGetAnalysisPreferences = interface
    ['{C5BA7634-5E15-43C3-B4AF-0FC59A79A33B}']
  end;

implementation

end.
