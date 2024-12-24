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

unit mworkflow_interfaces;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, MegaConsts;

type
  IDoesResultsExport = interface
    ['{1FF0F326-8BE5-4F3A-93D1-BCDAF7E48636}']
    procedure DoResultsExport(ExportType: TExportType; SaveLocation: String = '');
  end;

  IHasDefaultOptions = interface
    ['{BA88730D-7D84-4211-8C98-A20A6B0D3A47}']
    procedure GetDefaultOptions;
  end;

  IIsWorkflowStartNode = interface
    ['{F1901FD5-FAE0-416F-924D-9486243F0468}']
    property AppName: String;
    property Filename: String;
  end;

  IIsWorkflowResultsNode = interface
    ['{71385E03-5755-4F63-B69F-8DCF1620ABB2}']
    property ResultTypeName: String;
  end;

  IReportsProgress = interface
    ['{7FA9AF76-9E26-4384-8726-DD96ACC5E6DD}']
    function ProgressCheckCancel(Progress: Integer): Boolean;
    function ProgressAndStatusCheckCancel(Progress: Integer; AType: String; AInfo: String): Boolean;
    function ProgressAndStatusInfoCheckCancel(Progress: Integer; AInfo: String): Boolean;
    procedure UpdatePercentProgress(Progressin: Integer);
    procedure SetProgress(Progress: Integer);
  end;

  IGetsInputDataFilename = interface
    ['{F495F675-8D88-43AA-A933-F4EC99CDC584}']
    function Filename: String;
  end;

  IHasIterationCount = interface
    ['{7B24BCAD-65A6-4CE6-827A-D6E75279C68E}']
    function NumIterations: Integer;
  end;

  IIsWorkflowIterator = interface
    ['{B1051545-A74D-448B-8566-856EF5BB6345}']
    procedure SetIteration(aIteration: Integer);
  end;


implementation

end.

