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

unit mclocktestexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MMatrixExport, MAnalysisInfo, MLTreeAnalyzer, MegaConsts;

type

  { TClockTestExport }

  TClockTestExport = class(TMatrixExport)
  protected
    //procedure SaveToTargetDestination(ExportType: TExportType; var SaveLocation: String); virtual;
  public
    //procedure Assign(Other: TMatrixExport); virtual;
    constructor Create;
    procedure DoResultsExport(ExportType: TExportType; SaveLocation: String = ''); override;
    function ExportProcedureString: String; override;
    procedure Initialize(aInfo: TAnalysisInfo; analyzer: TMLTreeAnalyzer; aCaption: TStringList);
  end;

implementation

uses
  ExcelWrite, Graphics;

{ TClockTestExport }

constructor TClockTestExport.Create;
begin
  inherited Create;
  SavedExcelWrite := TExcelWrite.Create(nil, 'Molecular Clock Test');
  SavedExcelWrite.IsXLS := True;
  SavedExcelWrite.AutoFitCols := False;
end;

procedure TClockTestExport.DoResultsExport(ExportType: TExportType; SaveLocation: String);
begin
  SavedExcelWrite.IsXLS := True;
  SaveToTargetDestination(ExportType, SaveLocation);
end;

function TClockTestExport.ExportProcedureString: String;
begin
  Result := 'molecular_clock_test';
end;

procedure TClockTestExport.Initialize(aInfo: TAnalysisInfo; analyzer: TMLTreeAnalyzer; aCaption: TStringList);
var
  ex: TExcelWrite;
  aRect: TRect;
begin
  ex := SavedExcelWrite;
  ex.Add('Results from a test of molecular clocks using the Maximum Likelihood method');
  ex.WriteLine;
  aRect := Rect(0, 0, 4, 0);
  ex.MergeCells(aRect, aCenter, aCenter);
  aRect.Right := 0;
  ex.BoldCells(aRect);
  ex.ColorCells(aRect, clBlack, xlBorderBottom);

  ex.AddBlankCell;
  ex.Add('lnL');
  ex.Add('Parameters');
  ex.Add('(+G)');
  ex.Add('(+I)');
  ex.WriteLine;
  aRect := Rect(1, 1, 4, 1);
  ex.BoldCells(aRect);
  aRect.Left := 0;

  ex.Add('With Clock');
  ex.AddP(aInfo.LogLikelihoodWithClock, 3);
  ex.Add(Analyzer.ModelInfoList[1].NoOfParams-(aInfo.MyOriTreeList[0].NoOfOTUs-2));
  if aInfo.MyDistPack.ComputeGammaParameter then
    ex.Add(aInfo.GammaWithClock)
  else
    ex.Add('n/a');
  if aInfo.MyDistPack.ComputeInvarParameter then
    ex.Add(aInfo.InvarWithClock)
  else
    ex.Add('n/a');
  ex.WriteLine;
  ex.Add('Without Clock');
  ex.AddP(aInfo.LogLikelihoodWithoutClock, 3);
  ex.Add(Analyzer.ModelInfoList[0].NoOfParams);
  if aInfo.MyDistPack.ComputeGammaParameter  then
    ex.Add(aInfo.GammaWithoutClock)
  else
    ex.Add('n/a');
  if aInfo.MyDistPack.ComputeInvarParameter then
    ex.Add(aInfo.InvarWithoutClock)
  else
    ex.Add('n/a');
  ex.WriteLine;
  aRect := Rect(0, 2, 0, 3);
  ex.BoldCells(aRect);
  ex.AddCaptionAsWorksheet(aCaption, 'Note');
  ex.AutoSizeColumns;
  aRect := Rect(0, 0, 4, 0);
  ex.ColWidth(aRect, 15, 0);
end;

end.

