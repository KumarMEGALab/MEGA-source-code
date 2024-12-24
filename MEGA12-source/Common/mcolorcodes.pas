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

unit mcolorcodes;

interface

uses
  SysUtils, Classes, Graphics;

function GetBGColorForNucleotide(Nucleotide: AnsiChar): TColor;
function GetBGColorForAminoAcid(AminoAcid: AnsiChar): TColor;
function GetFontColorForNucleotide(Nucleotide: AnsiChar): TColor;
function GetFontColorForAminoAcid(AminoAcid: AnsiChar): TColor;

implementation

function GetBGColorForNucleotide(Nucleotide: AnsiChar): TColor;
begin
  case UpCase(Nucleotide) of
    'A' : Result := $003CC63C; // $0000DD00;
    'T' : Result := $006060F4; // $000044FF;
    'U' : Result := $006060F4; // $000044FF;
    'C' : Result := $00FFD755; // $00FF8800;
    'G' : Result := $00F752B6; // $00FF00AA;
    '-' : Result := clWhite;
  else
    Result := $00C0C0C0; // clLtGray;   // KT changed as it was almost invisible.
  end;
end;

function GetBGColorForAminoAcid(AminoAcid: AnsiChar): TColor;
begin
  case UpCase(AminoAcid) of
    'A' : Result := $0052EEFF; // clYellow;
    'M' : Result := $0052EEFF; // clYellow;
    'L' : Result := $0052EEFF; // clYellow;
    'V' : Result := $0052EEFF; // clYellow;
    'I' : Result := $0052EEFF; // clYellow;
    'F' : Result := $0052EEFF; // clYellow;
    'C' : Result := $0085D3D3; // clOlive;
    'H' : Result := $00FFF861; // clTeal;
    'E' : Result := $006060F4; // clRed;
    'D' : Result := $006060F4; // clRed;
    'K' : Result := $00FFD755; // clBlue;
    'R' : Result := $00FFD755; // clBlue;
    'G' : Result := $00F752B6; // clFuchsia;
    'P' : Result := $00FFD755; // clBlue;
    'Y' : Result := $0055FFB9; // clLime;
    'T' : Result := $003CC63C; // clGreen;
    'N' : Result := $003CC63C; // clGreen;
    'S' : Result := $003CC63C; // clGreen;
    'Q' : Result := $003CC63C; // clGreen;
    'W' : Result := $003CC63C; // clGreen;
    '-' : Result := clWhite;
  else
    Result := $00C0C0C0; // clLtGray;       // KT changed as it was almost invisible.
  end
end;

function GetFontColorForNucleotide(Nucleotide: AnsiChar): TColor;
begin
  case UpCase(Nucleotide) of
    'A' : Result := clGreen;
    'T' : Result := clRed;
    'U' : Result := clRed;
    'C' : Result := clBlue;
    'G' : Result := clFuchsia;
    '-' : Result := clBlack;
  else
    Result := clGrayText;
  end;
end;

function GetFontColorForAminoAcid(AminoAcid: AnsiChar): TColor;
begin
  case UpCase(AminoAcid) of
    'A' : Result := $0004BCC9;
    'M' : Result := $0004BCC9;
    'L' : Result := $0004BCC9;
    'V' : Result := $0004BCC9;
    'I' : Result := $0004BCC9;
    'F' : Result := $0004BCC9;
    'C' : Result := clOlive;
    'H' : Result := clTeal;
    'E' : Result := clRed;
    'D' : Result := clRed;
    'K' : Result := clBlue;
    'R' : Result := clBlue;
    'G' : Result := clFuchsia;
    'P' : Result := clBlue;
    'Y' : Result := clLime;
    'T' : Result := clGreen;
    'N' : Result := clGreen;
    'S' : Result := clGreen;
    'Q' : Result := clGreen;
    'W' : Result := clGreen;
    '-' : Result := clBlack;
  else
    Result := clGrayText;
  end
end;

end.
