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

unit MV_Columns;

interface

uses
  LCLIntf, LCLType, Classes, Controls, StdCtrls, SysUtils, Forms, Graphics, Clipbrd, Dialogs,
  MegaConsts;

type
  TColumnsSortOrder = (soNone, soAscending, soDescending);

  TCol = Class(TObject)
  private
    FVisible: Boolean;
    FWidth: Integer;
  public
    ColName: AnsiString;
    Expanded: Boolean;
    SortOrder: TColumnsSortOrder;
    AllowSort: Boolean;
    isDown: Boolean;
    RestoreWidth: Integer;
    Owner: TObject;
    procedure Assign(Source: TCol);
    procedure SwapSort;
    constructor create;
    procedure setWidth(newWidth: Integer);
    procedure setVisible(isVisible: Boolean);
    property Visible: Boolean read FVisible write setVisible;
    property Width: Integer read FWidth write setWidth;
  end;

  TColList = Class(TObject)
  private
    Cols: Array of TCol;
    Parent : TComponent;
    function GetCount: Integer;
    function GetVisibleCount: Integer;
  public
    procedure colChanged;
    procedure Clear;
    procedure Assign(Source: TColList);
    constructor create(Owner: TComponent);
    destructor destroy; override;
    property Count: Integer read GetCount;
    property VisibleCount: Integer read GetVisibleCount;
    procedure Add(Name: AnsiString; AllowSort: Boolean); Overload;
    procedure Add(Col: TCol); Overload;
    function getIndex(Col: TCol): Integer;
    function getVisibleIndex(Col: TCol): Integer;
    function GetColByName(AName: String): TCol;
    function GetCol(Index: Integer): TCol;
    function GetVisibleCol(Index: Integer): TCol;
    procedure SetCol(Index: Integer; Value: TCol);
    property Col[Index: Integer]: TCol read GetCol write SetCol;
    property VisibleCol[Index: Integer]: TCol read GetVisibleCol;
  end;

implementation

{$IFDEF VISUAL_BUILD}
uses
  MV_SeqDataExplorer;
{$ENDIF}

{TColList }

procedure TColList.Add(Name: AnsiString; AllowSort: Boolean);
var
  aCol: TCol;
begin
  aCol := TCol.Create;
  aCol.Owner := Self;
  aCol.ColName := Name;
  aCol.AllowSort := AllowSort;
  aCol.RestoreWidth := 80;
  Cols[Count] := aCol;
  SetLength(Cols, Count+1);
  colChanged;
end;

procedure TColList.Add(Col: TCol);
begin
  Col.Owner := Self;
  Cols[Count] := Col;
  SetLength(Cols, Count+1);
  colChanged;
end;

procedure TColList.Assign(Source: TColList);
var
  aCol: TCol;
  i: Integer;
begin
  if Source.Count = 0 then exit;
  Clear;
  for i := 0 to Source.Count-1 do
  begin
    acol := TCol.Create;
    acol.Assign(Source.Cols[i]);
    Add(acol);  // overload add so that you can actually add a TCol!
  end;
  Parent := Source.Parent;
end;

procedure TColList.Clear;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    FreeAndNil(Cols[i]);
  //Integer(ExtColVar^) := Count;
  colChanged;
end;

procedure TColList.colChanged;
begin
{$IFDEF VISUAL_BUILD}
  if (Parent <> nil) and (Parent.ClassType = TV_SeqDataExplorer) then
    TV_SeqDataExplorer(Parent).customColsChanged;
{$ENDIF}
end;

constructor TColList.create(Owner: TComponent);
begin
  SetLength(Cols, 10);
  Parent := Owner;
end;

destructor TColList.destroy;
var
  i: Integer;
begin
  for i := 0 to self.Count-1 do
    FreeAndNil(Cols[i]);
  inherited;
end;

function TColList.GetCol(Index: Integer): TCol;
begin
  if Length(Cols)-1 < Index then raise Exception.Create('Accessing a col that does not exist!');
  result := Cols[index];
end;

function TColList.GetVisibleCol(Index: Integer): TCol;  // if Index=5 it would get the 4th visible column
var
  i, visCount: Integer;
begin
  visCount := 0;
  result := nil;
  for i:=0 to high(cols) do
    if (cols[i] <> nil) and (cols[i].Visible) then
    begin
      if visCount = Index then
      begin
        result := Col[i];
        Exit;
      end;
      Inc(visCount);
    end;
end;

function TColList.GetColByName(AName: String): TCol;
var
  i: Integer;
begin
  if Trim(AName) = EmptyStr then raise Exception.Create('Accessing a col that does not exist!');
  for i := 0 to length(Cols)-1 do
    if (Cols[i] <> nil) and (AnsiCompareText(Cols[i].ColName, AName) = 0) then
    begin
      result := Cols[i];
      Exit;
    end;
end;

function TColList.GetCount: Integer;
var
  i: Integer;
begin
  result := 0;
  for i:=0 to high(cols) do
    if cols[i] <> nil then
      Inc(result);
end;

function TColList.getIndex(Col: TCol): Integer;
var
  i, numBefore: Integer;
begin
  result := -1;
  numBefore := 0;
  for i := 0 to high(cols) do
    if (cols[i] <> nil) then
    begin
      if cols[i] = Col then
      begin
        result := numBefore;
        Exit;
      end;
      inc(numBefore);
    end;
end;

function TColList.GetVisibleCount: Integer;
var
  i: Integer;
begin
  result := 0;
  for i:=0 to high(cols) do
    if (cols[i] <> nil) and (cols[i].Visible) then
      Inc(result);
end;

function TColList.getVisibleIndex(Col: TCol): Integer;
var
  i, numBefore: Integer;
begin
  result := -1;
  numBefore := 0;
  for i := 0 to high(cols) do
    if (cols[i] <> nil) and cols[i].Visible then
    begin
      if cols[i] = Col then
      begin
        result := numBefore;
        Exit;
      end;
      inc(numBefore);
    end;
end;

procedure TColList.SetCol(Index: Integer; Value: TCol);
begin
  Cols[index] := Value;
end;


{ TCol }

procedure TCol.Assign(Source: TCol);
begin
  ColName := Source.ColName;
  Expanded := Source.Expanded;
  SortOrder := Source.SortOrder;
  AllowSort := Source.AllowSort;
  isDown := Source.isDown;
  Visible := Source.Visible;
  RestoreWidth := Source.RestoreWidth;
end;

constructor TCol.create;
begin
  Expanded := true;
  SortOrder := soNone;
  AllowSort := true;
  FVisible := true;
  isDown := false;
end;

procedure TCol.setVisible(isVisible: Boolean);
begin
  FVisible := isVisible;
  if Owner <> nil then
    TColList(Owner).colChanged;
end;

procedure TCol.setWidth(newWidth: Integer);
begin
  FWidth := newWidth;
  if Owner <> nil then
    TColList(Owner).colChanged;
end;

procedure TCol.SwapSort;
begin
  Case SortOrder of
    soNone: SortOrder       := soAscending;
    soAscending: SortOrder  := soDescending;
    soDescending: SortOrder := soAscending;
  end;
end;



end.
