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

unit mpattertestmatrixexport;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils, MMatrixExport, MegaConsts;

type

  { TPatternTestMatrixExport }

  TPatternTestMatrixExport = class(TMatrixExport)
    private
      PatternData: Array[0..19,0..19] of double;
    public
      procedure SetPatternDataRow(Row: array of double; RowNum: Integer; DataTypeIn: String);
      procedure DoResultsExport(ExportType: TExportType; SaveLocation: String = ''); override;
      function ExportProcedureString: String; override;
      procedure Assign(Source: TPatternTestMatrixExport);
  end;

implementation

uses
  ExcelWrite, math;

{ TPatternTestMatrixExport }

procedure TPatternTestMatrixExport.SetPatternDataRow(Row: array of double; RowNum: Integer; DataTypeIn: String);
var
  i : integer;
begin
  DataType := DataTypeIn;
  for i:=0 to 19 do
    PatternData[RowNum][i] := Row[i];
end;

procedure TPatternTestMatrixExport.DoResultsExport(ExportType: TExportType; SaveLocation: String);
var
  aRect: TRect;
  i,j, size:integer;
  HeaderString : String;
begin
  if not Assigned(SavedExcelWrite) then
  begin
    SavedExcelWrite := TExcelWrite.Create(nil, 'Data Matrix');
    If DataType = 'DNA' then
    begin
      size := 3;
      HeaderString := 'ATCG';
    end
    else
    begin
      size := 19;
      HeaderString := 'ARNDCQEGHILKMFPSTWYV';
    end;
    SavedExcelWrite.IsXLS := True;
    SavedExcelWrite.Add('From\To');
    for i := 0 to size do
      SavedExcelWrite.Add(HeaderString[i+1]);
    SavedExcelWrite.WriteLine();

    for i:=0 to size do
    begin
      SavedExcelWrite.Add(HeaderString[i+1]);
      for j:=0 to size do
      begin
        if i<>j then
          SavedExcelWrite.Add(RoundTo((PatternData[i,j]*100), -4))
        else
          SavedExcelWrite.Add('-');
      end;
      SavedExcelWrite.WriteLine(0, 'A', '0.0000');
    end;
    aRect.Left :=0;
    aRect.Top :=0;
    aRect.Right := size+1;
    aRect.Bottom := size+1;
    SavedExcelWrite.AlignCells(aRect, aCenter);
    aRect.Bottom:=0;
    SavedExcelWrite.ColorCells(aRect, RGB(0,0,0), xlBorderBottom);
    aRect.Bottom:= size+1;
    aRect.Right := 0;
    SavedExcelWrite.ColorCells(aRect, RGB(0,0,0), xlBorderRight);

    if FCaption <> nil then
      SavedExcelWrite.AddCaptionAsWorksheet(FCaption);
  end;
  SaveToTargetDestination(ExportType, SaveLocation);
end;

function TPatternTestMatrixExport.ExportProcedureString: String;
begin
  Result := 'gamma_parameter_for_site_rates';
end;

procedure TPatternTestMatrixExport.Assign(Source: TPatternTestMatrixExport);
var
  i, j: Integer;
begin
  DataType := Source.DataType;
  if Assigned(Source.FCaption) then
  begin
    FCaption := TStringList.Create;
    FCaption.Assign(Source.FCaption);
  end;
  for i := 0 to 19 do
    for j := 0 to 19 do
      PatternData[i][j] := Source.PatternData[i][j];
end;

end.

