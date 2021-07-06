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

unit mega_citation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function MEGA_GUI_Citation(format: String): String;
function MEGA_CC_Citation(format: String): String;
function MEGA_Citation(aFormat, title, year, authors, journal, pages: String): String;
function LittleBootstrapsCitation(format: String): String;

const
  GUI_CITATION_TITLE = 'MEGA11: Molecular Evolutionary Genetics Analysis version 11';
  GUI_CITATION_YEAR = '2021';
  GUI_CITATION_AUTHORS = 'Tamura K, Stecher G, and Kumar S';
  GUI_CITATION_JOURNAL = 'Molecular Biology and Evolution';
  GUI_CITATION_PAGES = '(https://doi.org/10.1093/molbev/msab120)';

  CC_CITATION_TITLE = 'MEGA-CC: Computing Core of Molecular Evolutionary Genetics Analysis Program for Automated and Iterative Data Analysis';
  CC_CITATION_YEAR = '2012';
  CC_CITATION_AUTHORS = 'Kumar S, Stecher G, Peterson D, and Tamura K';
  CC_CITATION_JOURNAL = 'Bioinformatics';
  CC_CITATION_PAGES = '28:2685-2686 (Epub 2012 Aug 24)';

  LB_CITATION_TITLE = 'Fast and accurate bootstrap confidence limits on genome-scale phylogenies using little bootstraps';
  LB_CITATION_YEAR = '2021';
  LB_CITATION_AUTHORS = 'Sharma S and Kumar S';
  LB_CITATION_JOURNAL = '(submitted)';
  LB_CITATION_PAGES = '';

implementation

function MEGA_GUI_Citation(format: String): String;
begin
  Result := MEGA_Citation(format, GUI_CITATION_TITLE, GUI_CITATION_YEAR, GUI_CITATION_AUTHORS, GUI_CITATION_JOURNAL, GUI_CITATION_PAGES);
end;

function MEGA_CC_Citation(format: String): String;
begin
  Result := MEGA_Citation(format, CC_CITATION_TITLE, CC_CITATION_YEAR, CC_CITATION_AUTHORS, CC_CITATION_JOURNAL, CC_CITATION_PAGES);
end;

function MEGA_Citation(aFormat, title, year, authors, journal, pages: String): String;
begin
  if aFormat = 'one-line' then
    Result := Format('%s (%s). %s. %s %s', [authors, year, title, journal, pages])
  else if aFormat = 'html' then
  begin
    Result := Format('<blockquote>%s (%s) %s. ', [authors, year, title]);
    Result += Format('<i>%s</i> <b></b>%s.</blockquote></li></p>', [journal, pages]);
  end
  else if aFormat = 'ini-file' then
  begin
    Result := ';   ' + authors + LineEnding;
    Result := Result + ';   ' + title + LineEnding;
    Result := Result + Format(';   %s (%s) %s', [journal, year, pages]);
  end
  else
  begin
    Result := authors + LineEnding;
    Result += title + LineEnding;
    Result += Format('%s (%s) %s', [journal, year, pages]);
  end;
end;

function LittleBootstrapsCitation(format: String): String;
begin
  Result := MEGA_Citation(format, LB_CITATION_TITLE, LB_CITATION_YEAR, LB_CITATION_AUTHORS, LB_CITATION_JOURNAL, LB_CITATION_PAGES);
end;

end.

