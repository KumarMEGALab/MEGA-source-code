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

unit template_helper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MAnalysisInfo, MLegendGenerator, MegaConsts;

type

  { TTemplateHelper }

  TTemplateHelper = class(TObject)

  private

    public
      function LoadTemplate(MAI: TAnalysisInfo; treeBoxType: TTreeboxType; isIqTree: Boolean; var FigureGenerator: TLegendGenerator; var aMsg: String): Boolean;
  end;

implementation

uses
  mdistpack, mtreepack;

{ TTemplateHelper }

function TTemplateHelper.LoadTemplate(MAI: TAnalysisInfo; treeBoxType: TTreeBoxType; isIqTree: Boolean; var FigureGenerator: TLegendGenerator; var aMsg: String): Boolean;
var
  aTemplate: String = '';
begin
  if treeBoxType = tttCustomTimetreeBox then
    aTemplate := 'lbs_timetree.htm'
  else if treeBoxType = tttReltimeTree then
    aTemplate := 'Reltime.htm'
  else if Assigned(MAI) and MAI.IsSubsampling then
    aTemplate := 'ML_tree.htm'
  else if isIqTree then
    aTemplate := 'IQTree.htm'
  else if MAI.ClockType = ctLocal then
    aTemplate := 'Reltime.htm'
  else if MAI.MyUsrOperation = dtdoBEAM then
    aTemplate := 'BEAM.htm'
  else if MAI.MyTreePack.IsAncesteral then
    aTemplate := 'Infer_ancestral_sequences.htm'
  else if (MAI.MyTreePack.DoesContain(ttUserTree)) and (MAI.MyTreePack.DoesContain(ttClock)) then
    aTemplate := 'ML_tree.htm'
  else if MAI.MyTreePack.DoesContain(ttClock) or MAI.MyTreePack.DoesContain(ttClockTest) then
    aTemplate := 'Molecular_clock.htm'
  else if MAI.MyTreePack.NameAcronym = 'ML' then
    aTemplate := 'ML_tree.htm'
  else if MAI.MyTreePack.NameAcronym = 'MP' then
    aTemplate := 'MP_tree.htm'
  else if MAI.MyUsrOperation = dtdoGeneDupInference then
    aTemplate := 'Gene_duplication.htm'
  else
    aTemplate := 'Default_tree.htm';

  Result := FigureGenerator.LoadTemplateFromFile(aTemplate);
  if not Result then
    aMsg := 'failed to load ' + aTemplate
end;

end.

