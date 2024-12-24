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

unit Levenshtein;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

//Objeto que calcula la distancia de Levenshtein entre 2 cadenas.
//Alvaro Jeria Madariaga. 04/10/2002
//barbaro@hotpop.com
interface
uses
  sysutils;

type
  Tdistance = class(TObject)
  private
    function minimum(a, b, c: Integer): Integer;
  public
    function LD(s, t: string): Integer;
  end;

implementation

function Tdistance.minimum(a, b, c: Integer): Integer;
var mi: Integer;
begin
  mi := a;
  if (b < mi) then
    mi := b;
  if (c < mi) then
    mi := c;
  Result := mi;
end;

function Tdistance.LD(s, t: string): Integer;
var
  d: array of array of Integer;
  n, m, i, j, costo: Integer;
  s_i, t_j: char;
begin
  n := Length(s);
  m := Length(t);
  if (n = 0) then begin
      Result := m;
      Exit;
    end;
  if m = 0 then begin
      Result := n;
      Exit;
    end;
  setlength(d, n + 1, m + 1);
  for i := 0 to n do
    d[i, 0] := i;
  for j := 0 to m do
    d[0, j] := j;
  for i := 1 to n do
    begin
      s_i := s[i];
      for j := 1 to m do
        begin
          t_j := t[j];
          if s_i = t_j then costo := 0 else costo := 1;
          d[i, j] := Minimum(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i -1][j - 1] + costo);
        end;
    end;
  Result := d[n, m];
end;
end.
