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

unit MSingleList;

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}

interface

uses
  Classes;

type
  generic TList<T> = class(TObject)
      function Get(Index: Integer): T;
      procedure Put(Index: Integer; Item: T);
    public
      Count: Integer;
      constructor Create;
      destructor  Destroy; override;
      function  Add(Item: T): Integer;
      procedure Delete(Index: Integer);
      function  First: T;
      function  IndexOf(Item: T): Integer;
      procedure Insert(Index: Integer; Item: T);
      function  Last: T;
      function  Remove(Item: T): Integer;
      property Items[Index: Integer]: T read Get write Put; default;
  end;
type
  TSingleList = specialize TList<Single>;

type
  TArrayOfTSingleList = array[Word] of TSingleList;
  PArrayOfTSingleList = ^TArrayOfTSingleList;

implementation

constructor TList.Create;
begin
  inherited Create;
  Count := 0;
end;

destructor  TList.Destroy;
begin
  inherited Destroy;
end;

function TList.Get(Index: Integer): T;
begin
  Result := Items[Index];
end;

procedure TList.Put(Index: Integer; Item: t);
begin
  Items[Index] := Item;
end;

function TList.Add(Item: t): Integer;
begin
  inc(Count);
  Result := Add(Item);
end;

procedure TList.Delete(Index: Integer);
begin
  dec(Count);
  Delete(Index);
end;

function TList.First: T;
begin
  Result := First;
end;

function TList.IndexOf(Item: T): Integer;
begin
  Result := IndexOf(Item);
end;

procedure TList.Insert(Index: Integer; Item: T);
begin
  inc(Count);
  Insert(Index, Item);
end;

function TList.Last: T;
begin
  Result :=  Last;
end;

function TList.Remove(Item: T): Integer;
begin
  dec(Count);
  Result := Remove(Item);
end;

end.


