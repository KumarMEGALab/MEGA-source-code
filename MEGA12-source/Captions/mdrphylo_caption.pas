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

unit mdrphylo_caption;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, esl_linker, mstringbuilder;

type

  { TDrPhyloCaption }

  TDrPhyloCaption = class(TObject)
    private
      FLinker: TEslLinker;
      FStrBuilder: TMegaStringBuilder;
    public
      constructor Create(linker: TEslLinker);
      destructor Destroy; override;

      function GenerateCaption: String;
  end;

  function GenerateDrPhyloCaption(aLinker: TEslLinker): String;

implementation

uses
  MegaConsts, MLegendGenerator, MegaVerConsts;

function GenerateDrPhyloCaption(aLinker: TEslLinker): String;
var
  legendGenerator: TLegendGenerator = nil;
  drPhyloCaption: TDrPhyloCaption = nil;
begin
  try
    legendGenerator := TLegendGenerator.Create;
    legendGenerator.StyleSheet.IncrementFontSizes;
    legendGenerator.StyleSheet.IncrementFontSizes;
    legendGenerator.StyleSheet.IncrementFontSizes;
    legendGenerator.LoadTemplateFromFile('DrPhylo.htm');
    drPhyloCaption := TDrPhyloCaption.Create(aLinker);
    legendGenerator.AssignData('drPhyloCaption', drPhyloCaption.GenerateCaption);
    legendGenerator.AssignData('cladeSize', Format('%.0n', [aLinker.CladeSize*1.0]));
    Result := legendGenerator.GenerateLegend;
  finally
    if Assigned(drPhyloCaption) then
      drPhyloCaption.Free;
    if Assigned(legendGenerator) then
      legendGenerator.Free;
  end;
end;

{ TDrPhyloCaption }

constructor TDrPhyloCaption.Create(linker: TEslLinker);
begin
  FLinker := linker;
  FStrBuilder := TMegaStringBuilder.Create;
end;

destructor TDrPhyloCaption.Destroy;
begin
  FLinker := nil;
  if Assigned(FStrBuilder) then
    FStrBuilder.Free;
  inherited Destroy;
end;

function TDrPhyloCaption.GenerateCaption: String;
var
  domainType: String = '';
begin
  FStrBuilder.Clean;
  FStrBuilder.Add('The grid displaying the genetic model for the selected clade was inferred using the evolutionary');
  FStrBuilder.Add(Format(' sparse learning (ESL) approach [{%s:SparseLearningCitation}].', [POST_PROCESS_CITATION]));
  FStrBuilder.Add(Format(' The input matrix contained %.0n rows (e.g., species) and %.0n columns (e.g., genes).', [FLinker.NumSequences*1.0, FLinker.NumDomains*1.0]));
  if FLinker.CladeSize = FLinker.NumGridRows then
    FStrBuilder.Add(Format(' Of these, the selected clade contained %.0n species, all of which are displayed in the image.', [FLinker.CladeSize*1.0]))
  else
    FStrBuilder.Add(Format(' Of these, the selected clade contained %.0n species of which %.0n are displayed in the image.', [FLinker.CladeSize*1.0, FLinker.NumGridRows*1.0]));
  FStrBuilder.Add(Format(' Next to each row name is the species classification probability (SCP, in parentheses) of each species%s clustering', [#39]));
  FStrBuilder.Add(Format(' in the clade analyzed based on the ESL model [{%s:DrPhyloCitation}].', [POST_PROCESS_CITATION]));
  FStrBuilder.Add(' A value close to 1.0 is indicative of strong evidence for the grouping of the species in the clade analyzed.');
  FStrBuilder.Add(' An SCP is determined by the degree (intensity) of concordance (green) or discordance (magenta) of individual gene-species combinations.');
  FStrBuilder.Add(Format(' These are measured by the gene-species concordance (GSC) metric [{%s:DrPhyloCitation}].', [POST_PROCESS_CITATION]));
  FStrBuilder.Add(Format(' A cross-mark indicates missing data. On the top-left are species with the lowest SCP and genes', []));
  if FLinker.DataSource = ALIGNMENT_SEGMENTS then
    domainType := 'segments'
  else
    domainType := 'domains';
  FStrBuilder.Add(Format(' recieving the highest average |GSC| across all species in the clade. Shown are GSCs for as many as %.0n %s (columns)', [FLinker.NumGridColumns*1.0, domainType]));
  FStrBuilder.Add(Format(' and %.0n species, based on user preferences. ESL analyses were conducted in MEGA%d [{%s:MegaCitation}] which', [FLinker.NumGridRows*1.0, MAJOR_VERSION, POST_PROCESS_CITATION]));
  FStrBuilder.Add(Format(' used the --DrPhylo flag in the MyESL software [{%s:MyEslCitation}]', [POST_PROCESS_CITATION]));

  Result := FStrBuilder.GenerateString;
end;

end.

