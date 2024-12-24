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

unit MVS_DistDataExplorer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF VISUAL_BUILD}MV_DistDataExplorer,{$ENDIF} Graphics, Classes, MLongintList;

Type
TVS_DistDataExplorer = class

  private
    FTaxaGpsDlgOrderList : TLongIntList;
    function GetTaxaGpsDlgOrder(Index: LongInt):Integer;
    procedure SetTaxaGpsDlgOrder(Index: LongInt; Value: LongInt);
  public
  DispSelTaxaItem : Boolean;
  DispShowGpNames : Boolean;

  DispStr : AnsiString;

  IsFormInit : Boolean;
  PendingApply : boolean;

  NoOfSelTaxa:       LongInt;      // no of sequences currently selected
  MaxTaxaNamePixels: LongInt;      // Pixels needed to show taxa names
  MaxGpNamePixels:   LongInt;      // Pixels needed to show gps
  MaxTaxaNameLen:    LongInt;      // Characters needed to show taxa names
  MaxGpNameLen:      LongInt;      // Characters needed to show gps

  FPrecision: Integer;

  FIsResizing: Boolean;     //Set to true if the window is being resized

  MaxRowNamePixelLen: Integer; // maximum # of pixels for names
  LeftOfDecimalLen: Integer;     // x.y ; length of x

  CurFont: TFont;

  constructor create;
  destructor destroy; override;
  procedure Initialize;
  {$IFDEF VISUAL_BUILD}
  procedure ApplyToVisual(visual : TV_DistDataExplorer);
  {$ENDIF}
  property TaxaGpsDlgOrder[Index: LongInt]: integer read GetTaxaGpsDlgOrder write SetTaxaGpsDlgOrder;
end;

var
  VS_DistDataExplorer : TVS_DistDataExplorer;

implementation

uses
  MD_InputDistData;

{$IFDEF VISUAL_BUILD}
procedure TVS_DistDataExplorer.ApplyToVisual(visual: TV_DistDataExplorer);
begin
  if visual.DispSelTaxaItem.Checked <> DispSelTaxaItem then
    Visual.DispSelTaxaItem.Click;

  if visual.DispGpNamesItem.Checked <> DispShowGpNames then
    visual.DispGpNamesItem.Click;

  visual.DataGrid.Canvas.Font.Assign(CurFont);

  NoOfSelTaxa := D_InputDistData.FOtuInfos.NoOfSelOtus
end;
{$ENDIF}

constructor TVS_DistDataExplorer.create;
begin
  MaxGpNamePixels   := -1;
  MaxTaxaNamePixels := -1;

  LeftOfDecimalLen := 0;

  IsFormInit := False;

  NoOfSelTaxa :=        -1;
  MaxGpNameLen     := -1;

  FIsResizing := false;
  FPrecision := -1;
  MaxRowNamePixelLen := -1;
  
  CurFont := TFont.Create();

  FTaxaGpsDlgOrderList := TLongIntList.Create;
end;

destructor TVS_DistDataExplorer.destroy;
begin
  FTaxaGpsDlgOrderList.Free;
  inherited destroy;
end;

function TVS_DistDataExplorer.GetTaxaGpsDlgOrder(Index: Integer): Integer;
begin
  if FTaxaGpsDlgOrderList <> nil then
  begin
     if FTaxaGpsDlgOrderList.Count > index then
       Result := FTaxaGpsDlgOrderList.items[index]
     else
       Result := index;
    end;
end;

procedure TVS_DistDataExplorer.Initialize;
begin
  NoOfSelTaxa := D_InputDistData.FOtuInfos.NoOfSelOtus;
  {$IFDEF VISUAL_BUILD}
  DispSelTaxaItem := V_DistDataExplorer.DispSelTaxaItem.Checked;
  DispShowGpNames := V_DistDataExplorer.DispGpNamesItem.Checked;
  CurFont.Assign(V_DistDataExplorer.DataGrid.Canvas.Font);
  {$ENDIF}
end;

procedure TVS_DistDataExplorer.SetTaxaGpsDlgOrder(Index, Value: Integer);
begin
  while FTaxaGpsDlgOrderList.Count <= Index do
    FTaxaGpsDlgOrderList.Add(0);
  FTaxaGpsDlgOrderList[index] := Value;

end;

end.
