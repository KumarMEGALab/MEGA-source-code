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

unit mtajimatestexport;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, MMatrixExport, MegaConsts;

type

  { TTajimaNeutralityTestExport }

  TTajimaNeutralityTestExport = class(TMatrixExport)
    private

    public
      constructor Create;
      procedure DoResultsExport(ExportType: TExportType; SaveLocation: String = ''); override;
      function ExportProcedureString: String; override;
  end;

  { TTajimaClockTestExport }

  TTajimaClockTestExport = class(TMatrixExport)
    public
      constructor Create;
      procedure DoResultsExport(ExportType: TExportType; SaveLocation: String = ''); override;
      function ExportProcedureString: String; override;
  end;

implementation

uses
  ExcelWrite;

{ TTajimaClockTestExport }

constructor TTajimaClockTestExport.Create;
begin
  inherited Create;
  SavedExcelWrite := TExcelWrite.Create(nil, 'Tajima Relative Rate Test');
  SavedExcelWrite.IsXLS := True;
  SavedExcelWrite.AutoFitCols := False;
end;

procedure TTajimaClockTestExport.DoResultsExport(ExportType: TExportType; SaveLocation: String);
begin
  SavedExcelWrite.IsXLS := True;
  SaveToTargetDestination(ExportType, SaveLocation);
end;

function TTajimaClockTestExport.ExportProcedureString: String;
begin
  Result := 'tajima_clock_test';
end;

{ TTajimaNeutralityTestExport }

constructor TTajimaNeutralityTestExport.Create;
begin
  inherited Create;
  SavedExcelWrite := TExcelWrite.Create(nil, 'Tajima Neutrality Test');
  SavedExcelWrite.IsXLS := True;
  SavedExcelWrite.AutoFitCols := False;
end;

procedure TTajimaNeutralityTestExport.DoResultsExport(ExportType: TExportType; SaveLocation: String);
begin
  SavedExcelWrite.IsXLS := True;
  SaveToTargetDestination(ExportType, SaveLocation);
end;

function TTajimaNeutralityTestExport.ExportProcedureString: String;
begin
  Result := 'tajima_neutrality_test';
end;

end.

