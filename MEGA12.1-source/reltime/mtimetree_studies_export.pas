{
	Copyright 1992-2025 Sudhir Kumar and Koichiro Tamura

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

unit mtimetree_studies_export;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExcelWrite, mtimetree_map, MegaConsts;

function ExportTimetreeStudies(data: TTimeTreeMap): Boolean;

implementation

uses
  MWriteOutputDlg, MegaUtils;

function ExportTimetreeStudies(data: TTimeTreeMap): Boolean;
var
  ExportType : TExportType =  EXNone;
  writer : TExcelWrite = nil;
  SaveLocation: String = '';
  i: Integer = -1;
  taxon: TTimeTreeTaxonData = nil;
  study: TTimeTreeStudyData = nil;
begin
  Result := False;
  try
    SaveLocation := 'timetree-studies';
    WriteOutputDlg.Disallow(EXfasta);
    WriteOutputDlg.Disallow(EXtext);
    WriteOutputDlg.Disallow(EXcsvDisp);
    ExportType := PromptUserWriteOutput(SaveLocation);
    if (ExportType = EXnone) or (Trim(SaveLocation) = EmptyStr) then
      Exit;
    writer := TExcelWrite.Create(nil, 'Studies');
    writer.IsXLS := True;
    writer.Add('Age Estimate');
    writer.Add('No. of Trees');
    writer.Add('Year');
    writer.Add('First Author');
    writer.Add('Title');
    writer.WriteLine(0);
    if data.NumStudies > 0 then
    begin
      for i := 0 to data.NumStudies - 1 do
      begin
        study := data.Studies[i];
        writer.Add(study.AgeEstimate);
        writer.Add(study.NumTrees);
        writer.Add(study.Year);
        writer.Add(study.FirstAuthor);
        writer.Add(study.Title);
        writer.WriteLine(0);
      end;
    end;

    writer.AddWorksheet('Summary');
    writer.Add('NCBI ID');
    writer.Add(data.TimetreeMrcaId);
    writer.WriteLine(1);
    writer.Add('Median Time');
    writer.Add(data.MedianTime);
    writer.WriteLine(1);
    writer.Add('CI low');
    writer.Add(data.MinTime);
    writer.WriteLine(1);
    writer.Add('CI high');
    writer.Add(data.MaxTime);
    writer.WriteLine(1);

    writer.AddWorksheet('Taxa');
    writer.Add('NCBI ID');
    writer.Add('Scientific Name');
    writer.Add('Taxonomic Rank');
    writer.Add('Synonym');
    writer.Add('OTU Name');
    writer.WriteLine(2);
    if data.Count > 0 then
    begin
      for i := 0 to data.Count - 1 do
      begin
        taxon := data[i];
        writer.Add(taxon.NcbiId);
        writer.Add(taxon.TimetreeName);
        writer.Add(taxon.TaxonomicRank);
        writer.Add(taxon.Synonym);
        if taxon.OtuName <> EmptyStr then
          writer.Add(taxon.OtuName)
        else
          writer.AddBlankCell;
        writer.WriteLine(2);
      end;
    end;

    writer.SaveFile(SaveLocation, ExcelExportToFiletype(ExportType));
    If ExportIsWorkbookDisplay(ExportType) then
      RunAProgram(SaveLocation);
    Result := FileExists(SaveLocation);
  finally
    if Assigned(writer) then
      writer.Free;
  end;
end;



end.

