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

unit KeywordConsts;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

type
  TSnTokenCode = (
    snNoToken,  // id = 0
      // Variables with distinct options
    snDataType, // 1
      {ToDo -o Kumar :  snGenotypeFreq,   as new data for analyis }
      {ToDo -o Kumar :  snMicrosatellites as new data for analyis }
      {ToDo -o Kumar :  snAlleleFreq as new data for analyis }
      {ToDo -o Kumar :  snRestrictionSite as new data for analyis }
      {ToDo -o Kumar :  snMicrosatellites as new data for analyis }
      {ToDo -o Kumar :  snMutation as new data for analyis }
      {ToDo -o Kumar :  snTree as new data for analyis }
      {ToDo -o Kumar :  multi-dist matrices as new data for analyis }

      snNucleotide, snProtein, snDistance,  // 2-4
      snGenotypeFreq, snMicrosatellites,  snAlleleFreq, // 5-7
      snRestrictionSite, snRFLP, snRAPD,
      snMutation, snTree,// 8-10
      snDataFormat,  // 11
      snContinuous, snBlockwise, snLowerMatrix, snUpperMatrix, snColumn,
      snLinear, //12-15
      snDomainType,  // 22
      snExon, snIntron, snDomainEnd, snCoding, snNoncoding, //23-27
      // Variables requiring Numeric option
      snNOtus, snNSites, // 28-29
      snNLoci, snNAlleles, // 30-31
      snNTrees, //32
      snCodonStart, //33
      // Variables requiring a single character option
      snIndel, snMissing, snIdentical, // 34-36
        // Variables requiring String as option
      snDomainName,  //37
      snGeneName,    // 38
      snCodeTableName, //39
      snLocusName,     //40
        // mark sites with special attributes
      snMark,  //41
      snBLenName, snConfidenceName, snPValueName, snStdErrName, snNodeFreqName, //42-45
        // frequency of an otu, as for pop gene data
      snOtuFreq, snCodingDna, snVariable, snOption, snCommand, snSymbol,
      snString, snTreeType, snTreeFormat, snInvalidBase, snBase, snInteger,
      snJustChar, snFloat, snRooted, snUnrooted, snBoolean // Todo -o Glen, we need to remove snCodingDna because it conflicts with the orignal intent of snTokenCode, i.e. snToken code should only be used by the tokenizer
  );

  function TokenCodeString(ACode: TSnTokenCode): String;
  function StringToTokenCode(aString: String): TSnTokenCode;
  function TokenCodeToString(aCode: TSnTokenCode): String;

implementation

uses
 typinfo, sysutils;

function TokenCodeString(ACode: TSnTokenCode): String;
begin
  Result := EmptyStr;
  case ACode of
    snNoToken: Result := 'none';
    snDataType: Result := 'data_type';
    snNucleotide: Result := 'nucleotide';
    snProtein: Result := 'protein';
    snDistance: Result := 'distance';
    snGenotypeFreq: Result := 'genotype_freq';
    snMicrosatellites: Result := 'microsatellites';
    snAlleleFreq: Result := 'allele_freq';
    snRestrictionSite: Result := 'restriction_site';
    snRFLP: Result := 'RFLP';
    snRAPD: Result := 'RAPD';
    snMutation: Result := 'mutation';
    snTree: Result := 'tree';
    snDataFormat: Result := 'data_format';
    snContinuous: Result := 'continuous';
    snBlockwise: Result := 'blockwise';
    snLowerMatrix: Result := 'lower_matrix';
    snUpperMatrix: Result := 'upper_matrix';
    snColumn: Result := 'column';
    snLinear: Result := 'linear';
    snDomainType: Result := 'domain_type';
    snExon: Result := 'exon';
    snIntron: Result := 'intron';
    snDomainEnd: Result := 'domain_end';
    snCoding: Result := 'coding';
    snNoncoding: Result := 'noncoding';
    snNOtus: Result := 'num_otus';
    snNSites: Result := 'num_sites';
    snNLoci: Result := 'num_loci';
    snNAlleles: Result := 'num_alleles';
    snNTrees: Result := 'num_trees';
    snCodonStart: Result := 'codon_start';
    snIndel: Result := 'indel';
    snMissing: Result := 'missing';
    snIdentical: Result := 'identical';
    snDomainName: Result := 'domain_name';
    snGeneName: Result := 'gene_name';
    snCodeTableName: Result := 'code_table_name';
    snLocusName: Result := 'locus_name';
    snMark: Result := 'mark';
    snBLenName: Result := 'branch_length_name';
    snConfidenceName: Result := 'confidence_name';
    snPValueName: Result := 'pvalue_name';
    snStdErrName: Result := 'std_err_name';
    snNodeFreqName: Result := 'node_freq_name';
    snOtuFreq: Result := 'otu_freq';
    snCodingDna: Result := 'coding_dna';
  end;
end;

function StringToTokenCode(aString: String): TSnTokenCode;
var
  ordinalVal: Integer;
begin
  ordinalVal := GetEnumValue(TypeInfo(TSnTokenCode), aString);
  if ordinalVal = -1 then
    Result := snNoToken
  else
    Result := TSnTokenCode(ordinalVal);
end;

function TokenCodeToString(aCode: TSnTokenCode): String;
begin
  Result := GetEnumName(TypeInfo(TSnTokenCode), integer(aCode));
end;

end.
