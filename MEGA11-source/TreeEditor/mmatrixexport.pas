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

Unit MMatrixExport;

interface

uses
  LCLIntF, LCLType, Classes, ExcelWrite, ComCtrls, sysutils, MegaUtils, MegaConsts,
  mworkflow_interfaces;

type

{ TMatrixExport }

 TMatrixExport = class abstract(TObject, IDoesResultsExport)
  protected
    DataType : AnsiString;
    FCaption : TStringList;
    function IsSpreadsheetExport(ExportType: TExportType): Boolean;
    procedure SaveToTargetDestination(ExportType: TExportType; var SaveLocation: String); virtual;
  public
    SavedExcelWrite: TExcelWrite;
    destructor Destroy; override;
    constructor Create;
    procedure Assign(Other: TMatrixExport);
    procedure DoResultsExport(ExportType: TExportType; SaveLocation: String = ''); virtual; abstract;
    procedure SetCaption(Caption : TStringList);
    function ExportProcedureString: String; virtual; abstract;
end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ENDIF}
  ActnList;

function TMatrixExport.IsSpreadsheetExport(ExportType: TExportType): Boolean;
begin
  Result := (ExportType = EXexcelDisp) or
            (ExportType = EXexcelSave) or
            (ExportType = EXodsDisp) or
            (ExportType = EXodsSave) or
            (ExportType = EXexcelXmlDisp) or
            (ExportType = EXexcelXmlSave);
end;

procedure TMatrixExport.SaveToTargetDestination(ExportType: TExportType; var SaveLocation: String);
begin
  {$IFDEF VISUAL_BUILD}
  if SaveLocation = EmptyStr then
    SaveLocation := GetSaveLocation;
  {$ELSE}
  SaveLocation := NextAvailableFileNameNV('.csv');
  {$ENDIF}

  {$IFDEF VISUAL_BUILD}
  SavedExcelWrite.SaveFile(SaveLocation, ExcelExportToFileType(ExportType));
  if SavedExcelWrite.IsXLS AND hasExcel then
    RunAProgram(SaveLocation)
  else if not SavedExcelWrite.IsXLS then
  begin
    SavedExcelWrite.SaveFile(SaveLocation, ExportCSV);
  end
  {$ELSE}
    SavedExcelWrite.SaveFile(SaveLocation, ExportCSV);
  {$ENDIF}
end;

procedure TMatrixExport.Assign(Other: TMatrixExport);
begin
  DataType := Other.DataType;
  if Assigned(Other.FCaption) then
  begin
    FCaption := TStringList.Create;
    FCaption.Assign(Other.FCaption);
  end;
end;

procedure TMatrixExport.SetCaption(Caption: TStringList);
begin
  FCaption := TStringList.Create;
  FCaption.Assign(Caption);
end;

destructor TMatrixExport.Destroy;
begin
  if Assigned(FCaption) then
    FCaption.Free;
  if Assigned(SavedExcelWrite) then
    SavedExcelWrite.Free;
  inherited Destroy;
end;

constructor TMatrixExport.Create;
begin
  FCaption := nil;
  SavedExcelWrite := nil;
end;

end.
