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

unit mdata_subset_caption;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MAnalysisInfo;

type

  { TDataSubsetCaption }

  TDataSubsetCaption = class(TObject)
      FAnalysisInfo: TAnalysisInfo;
      function CodonPositionsSummary: String;
      function DeletionOptionSummary: String;
    public
      constructor Create(aInfo: TAnalysisInfo);
      function GenerateCaption: String;
  end;

  function GenerateDataSubsetCaption(aInfo: TAnalysisInfo): String;

implementation

uses
  mstringbuilder, mdistpack, MegaConsts, MegaVerConsts, mega_citation;

function GenerateDataSubsetCaption(aInfo: TAnalysisInfo): String;
var
  c: TDataSubsetCaption = nil;
begin
  Result := EmptyStr;
  try
    c := TDataSubsetCaption.Create(aInfo);
    Result := c.GenerateCaption;
  finally
    if Assigned(c) then
      c.Free;
  end;
end;

{ TDataSubsetCaption }

function TDataSubsetCaption.CodonPositionsSummary: String;
begin
  Result := EmptyStr;
  if not FAnalysisInfo.IsCoding then Exit;
  Result := FAnalysisInfo.CodonPositions;
end;

function TDataSubsetCaption.DeletionOptionSummary: String;
var
  b: TMegaStringBuilder = nil;
begin
  Result := EmptyStr;

  try
    b := TMegaStringBuilder.Create;

    if FAnalysisInfo.IsCompleteDeletion then
      b.Add(' The complete deletion option was applied to eliminate positions containing gaps and missing data')
    else if FAnalysisInfo.IsPairwiseDeletion then
      b.Add(' The pairwise deletion option was applied to all ambiguous positions for each sequence pair')
    else if FAnalysisInfo.IsPartialDeletion then
      b.Add(Format(' The partial deletion option was applied to eliminate all positions with less than %d%% site coverage', [FAnalysisInfo.SiteCoverage]));

    if FAnalysisInfo.IsModelTamer then
    begin
      if FAnalysisInfo.HasDeletionOption then
        b.Add('.');
      b.Add(Format(' There were a total of %.0n positions in the final dataset, ', [FAnalysisInfo.NumSitesPerSubSample*1.0]));
      b.Add(FAnalysisInfo.ModelTamerCaption);
    end
    else
    begin
     if FAnalysisInfo.HasDeletionOption then
       b.Add(Format(' resulting in a final data set comprising %.0n positions.', [FAnalysisInfo.NoOfSites*1.0]));
    end;

    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

constructor TDataSubsetCaption.Create(aInfo: TAnalysisInfo);
begin
  FAnalysisInfo := aInfo;
end;

function TDataSubsetCaption.GenerateCaption: String;
var
  b: TMegaStringBuilder = nil;
  majVersion: Integer = -1;
  citation: String = '';
begin
  Result := EmptyStr;
  try
    b := TMegaStringBuilder.Create;

    b.Add(Format('The analytical procedure encompassed %.0n', [FAnalysisInfo.NoOfSeqs*1.0]));
    if FAnalysisInfo.isAminoAcid then
      b.Add(' amino acid sequences')
    else
    begin
      if FAnalysisInfo.IsCoding then
        b.Add(' coding');
      b.Add(' nucleotide sequences');
    end;

    if FAnalysisInfo.isAminoAcid and FAnalysisInfo.IsCoding then
      b.Add(Format(' having coding data translated using a %s genetic code table', [FAnalysisInfo.MyDistPack.GeneticCode]));
    if CodonPositionsSummary <> EmptyStr then
      b.Add(' using ' + CodonPositionsSummary + ' positions');
    if (not FAnalysisInfo.HasDeletionOption) and (not FAnalysisInfo.IsModelTamer) then
      b.Add(Format(' with %.0n positions in the final dataset.', [FAnalysisInfo.NoOfSites*1.0]))
    else
      b.Add('.');

    if FAnalysisInfo.HasDeletionOption or FAnalysisInfo.IsModelTamer then
      b.Add(DeletionOptionSummary);

    if FAnalysisInfo.SiteLabelsCaption <> EmptyStr then
      b.Add(Format(' Only sites with user-specified labels (%s) were included.', [FAnalysisInfo.SiteLabelsCaption]));

    if FAnalysisInfo.CodonsTreatedAsMissingData then
      b.Add(' Stop codons found in the input alignment were converted to missing data.');
    if FAnalysisInfo.TreeSessionVersion > 0 then
    begin
      majVersion := MapSessionVersionToMegaVersion(sftMts, FAnalysisInfo.TreeSessionVersion);
      citation := MapMajorVersionToCitation(majVersion);
      if majVersion = 10 then
        b.Add(Format(' Evolutionary analyses were conducted in MEGA X [{%s:%s}]', [POST_PROCESS_CITATION, citation]))
      else
        b.Add(Format(' Evolutionary analyses were conducted in MEGA%d [{%s:%s}]', [majVersion, POST_PROCESS_CITATION, citation]));
    end;
    {$IFDEF DARWIN}
    b.Add(Format('[{%s:MegaMacCitation}]', [POST_PROCESS_CITATION]));
    {$ENDIF}
    if FAnalysisInfo.NumberOfThreads > 1 then
      b.Add(Format(' utilizing up to %d parallel computing threads', [FAnalysisInfo.NumberOfThreads]));
    b.Add('.');
    Result := b.GenerateString;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

end.

