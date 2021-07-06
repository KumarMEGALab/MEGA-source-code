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

unit mentropy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, MegaConsts;

type

  { TShannonEntropy }

  TShannonEntropy = class(TObject)
    protected
      FSitesUsed: TIntArray;
      FIsAmino: Boolean;
      FSeqData: PAnsiChar;
      FBaseCounts: TIntArray;
      FEntropy: Extended;
      FNumSites: Integer;
      procedure ComputeEntropy;
      procedure CountBases;
    public
      constructor Create(seqData: PAnsiChar; sitesUsed: TIntArray; isAmino: Boolean);
      property Entropy: Extended read FEntropy;
  end;

  TShannonEntropyList = specialize TFPGList<TShannonEntropy>;

implementation

uses
  math, MegaUtils;

{ TShannonEntropy }

procedure TShannonEntropy.ComputeEntropy;
var
  base: Integer;
  p: Extended;
  e: Extended;
begin
  FEntropy := 0.0;
  if FNumSites = 0 then
    Exit;
  for base := Low(FBaseCounts) to High(FBaseCounts) do
  begin
    if FBaseCounts[base] > 0 then
    begin
      p := FBaseCounts[base]/FNumSites;
      e := p*log2(p);
      FEntropy := FEntropy + e + p;
    end;
  end;
  FEntropy := FEntropy*-1
end;

procedure TShannonEntropy.CountBases;
var
  site: Integer;
begin
  for site := 0 to FNumSites - 1 do
  begin
    if FIsAmino then
    begin
      inc(FBaseCounts[ord(AminoToDistMap(FSeqData[FSitesUsed[site]]))]);
    end
    else
    begin
      inc(FBaseCounts[ord(NucToDistMap(FSeqData[FSitesUsed[site]]))]);
    end;
  end;
end;

constructor TShannonEntropy.Create(seqData: PAnsiChar; sitesUsed: TIntArray; isAmino: Boolean);
var
  i: Integer;
begin
  FNumSites := Length(sitesUsed);
  FSitesUsed := sitesUsed;
  FIsAmino := isAmino;
  FSeqData := seqData;
  if isAmino then
    SetLength(FBaseCounts, 21)
  else
    SetLength(FBaseCounts, 5);
  for i := Low(FBaseCounts) to High(FBaseCounts) do
    FBaseCounts[i] := 0;
  CountBases;
  ComputeEntropy;
end;

end.

