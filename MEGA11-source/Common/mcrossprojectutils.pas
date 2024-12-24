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

unit mcrossprojectutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF VISUAL_BUILD}
    {$IFDEF CLONE_FINDER}
    mainform
    {$ELSE}
    mega_main
    {$ENDIF}
  {$ELSE}
   MD_MegaMain
  {$ENDIF};

  function GetActiveDataFileName: String;
  procedure SetActiveDataFileName(aName: String);
  function GetActiveDataTitle: String;
  procedure SetActiveDataTitle(aTitle: String);

implementation

function GetActiveDataFileName: String;
begin
  {$IFDEF VISUAL_BUILD}
    {$IFDEF CLONE_FINDER}
    Result := CloneFinderMainForm.DataFileName;
    {$ELSE}
    Result :=  MegaForm.DataFileName;
    {$ENDIF}
  {$ELSE}
   Result := D_MegaMain.DataFileName;
  {$ENDIF}
end;

procedure SetActiveDataFileName(aName: String);
begin
  {$IFDEF VISUAL_BUILD}
   MegaForm.DataFileName := aName;
  {$ELSE}
   D_MegaMain.DataFileName := aName;
  {$ENDIF}
end;

function GetActiveDataTitle: String;
begin
  {$IFDEF VISUAL_BUILD}
    {$IFDEF CLONE_FINDER}
    Result := CloneFinderMainForm.DataTitle;
    {$ELSE}
    Result :=  MegaForm.DataTitle;
    {$ENDIF}
  {$ELSE}
   Result := D_MegaMain.DataTitle;
  {$ENDIF}
end;

procedure SetActiveDataTitle(aTitle: String);
begin
  {$IFDEF VISUAL_BUILD}
    {$IFDEF CLONE_FINDER}
    CloneFinderMainForm.DataTitle := aTitle;
    {$ELSE}
    MegaForm.DataTitle := aTitle;
    {$ENDIF}
  {$ELSE}
   D_MegaMain.DataTitle := aTitle;
  {$ENDIF}
end;

end.

