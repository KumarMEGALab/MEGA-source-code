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

unit mgridcolumnresizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids;

const
  DRAWGRID_MIN_COLUMN_WIDTH = 24;
  WORKSHEET_MIN_COLUMN_WIDTH = 5;


type

  { TGridColumnResizer }

  TGridColumnResizer = class(TObject)
  private
    FDrawGrid: TDrawGrid;
  protected
    FMinColWidth: Integer;
    FCellPadding: Integer;
    FColumnWidths: array of Integer;
    FCellContents: TStringList;
    procedure SetCellPadding(AValue: Integer);
    procedure InitColumnWidths;
    function CellContentsAreValid: Boolean;
    function ColumnCount: Integer; virtual;
    function TextWidth(aText: String): Integer; virtual;
    procedure CalculateNewColumnWidths; virtual;
    procedure UpdateGridColumnWidths; virtual;
    function SumColumnWidths: Integer; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResizeGridColumns(var aGrid: TDrawGrid; const CellContents: TStringList); virtual;
    procedure AdjustLastColumnWidthIfNeeded(var aGrid: TDrawGrid); virtual;
    procedure AdjustLastColumnToBestFit(var aGrid: TDrawGrid; const longestStr: String; const visibleWidth: Integer); virtual;
    property CellPadding: Integer read FCellPadding write SetCellPadding;
  end;

implementation

uses
  math;

{ TGridColumnResizer }

procedure TGridColumnResizer.SetCellPadding(AValue: Integer);
begin
  if FCellPadding=AValue then Exit;
  FCellPadding:=AValue;
end;

procedure TGridColumnResizer.InitColumnWidths;
var
  i: Integer;
begin
  Assert(CellContentsAreValid);
  SetLength(FColumnWidths, ColumnCount);
  for i := 0 to Length(FColumnWidths) - 1 do
    FColumnWidths[i] := FMinColWidth;
end;

function TGridColumnResizer.CellContentsAreValid: Boolean;
begin
  Result := ((ColumnCount > 0) and (Assigned(FCellContents)) and (FCellContents.Count = ColumnCount));
end;

function TGridColumnResizer.ColumnCount: Integer;
begin
  if Assigned(FDrawGrid) then
    Result := FDrawGrid.ColCount
  else
    Result := 0;
end;

function TGridColumnResizer.TextWidth(aText: String): Integer;
begin
  Result := FDrawGrid.Canvas.TextWidth(aText);
end;

procedure TGridColumnResizer.CalculateNewColumnWidths;
var
  i: Integer;
  aText: String;
  aWidth: Integer=0;
  numCols: Integer;
begin
  numCols := ColumnCount;
  for i := 0 to numCols - 1 do
  begin
    aText := FCellContents[i];
    aWidth := TextWidth(aText);
    FColumnWidths[i] := Max(FMinColWidth, aWidth + (2 * FCellPadding));
  end;
end;

procedure TGridColumnResizer.UpdateGridColumnWidths;
var
  i: Integer;
begin
  for i := 0 to FDrawGrid.ColCount - 1 do
    FDrawGrid.ColWidths[i] := FColumnWidths[i];
  AdjustLastColumnWidthIfNeeded(FDrawGrid);
end;

function TGridColumnResizer.SumColumnWidths: Integer;
var
  i: Integer;
begin
  Result := 0;
  if FDrawGrid.ColCount = 0 then
    Exit;
  for i := 0 to FDrawGrid.ColCount - 1 do
    Result := Result + FDrawGrid.ColWidths[i];
end;

constructor TGridColumnResizer.Create;
begin
  FCellPadding := 8;
  SetLength(FColumnWidths, 0);
  FMinColWidth := DRAWGRID_MIN_COLUMN_WIDTH;
end;

destructor TGridColumnResizer.Destroy;
begin
  SetLength(FColumnWidths, 0);
  inherited Destroy;
end;

procedure TGridColumnResizer.ResizeGridColumns(var aGrid: TDrawGrid; const CellContents: TStringList);
begin
  FDrawGrid := aGrid;
  FCellContents := CellContents;
  InitColumnWidths;
  CalculateNewColumnWidths;
  UpdateGridColumnWidths;
  FDrawGrid := nil;
  FCellContents := nil;
end;

procedure TGridColumnResizer.AdjustLastColumnWidthIfNeeded(var aGrid: TDrawGrid);
var
  delta: Integer=0;
begin
  if not Assigned(aGrid) then
    Exit;

    FDrawGrid := aGrid;
    delta := (FDrawGrid.Width - SumColumnWidths);
    if delta > 0 then
      FDrawGrid.ColWidths[FDrawGrid.ColCount - 1] := FDrawGrid.ColWidths[FDrawGrid.ColCount - 1] + delta;
    FDrawGrid.Invalidate;
    FDrawGrid := nil;
end;

procedure TGridColumnResizer.AdjustLastColumnToBestFit(var aGrid: TDrawGrid; const longestStr: String; const visibleWidth: Integer);
var
  sumWidths: Integer;
  delta, minNeeded: Integer;
begin
  FDrawGrid := aGrid;
  sumWidths := SumColumnWidths;

  if visibleWidth > sumWidths then
    AdjustLastColumnWidthIfNeeded(aGrid)
  else
  begin
    if visibleWidth < sumWidths then
    begin
      minNeeded := aGrid.Canvas.TextWidth(longestStr);
      delta := (sumWidths - visibleWidth);
      if (aGrid.ColWidths[aGrid.ColCount - 1] - delta) > minNeeded then
        aGrid.ColWidths[aGrid.ColCount - 1] := aGrid.ColWidths[aGrid.ColCount - 1] - delta
      else
        aGrid.ColWidths[aGrid.ColCount - 1] := minNeeded;
    end;
    FDrawGrid := nil;
  end;
end;

end.

