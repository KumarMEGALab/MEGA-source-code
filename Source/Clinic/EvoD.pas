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

unit EvoD;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
{$IFDEF VISUAL_BUILD}
uses
  LCLIntf, LCLType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, laz2_XMLRead, laz2_DOM,MLegendGenerator;


const
  MYPEG_PATH = '/MEGA-MD/';
  MYPEG_HOST = 'www.mypeg.info';
  SPECIES_ALIGNMENTS_PATH = MYPEG_PATH + '46SpeciesAlignments/'; // for retrieving the 46 species alignments
  BAD_NUMBER = -99999999;

  // these are column indices as the result string is returned to us (these are NOT the column indices for the treelist)
  CI_CHROMOSOME = 0;
  CI_CHROMOSOMEPOS = 1;
  CI_REF = 2;
  CI_ALT = 3;
  CI_RSID = 4;
  CI_STRAND = 5;
  CI_MRNAID = 6;
  CI_NUCPOS = 7;
  CI_WILDNUC = 8;
  CI_MUTANTNUC = 9;
  CI_PEPTIDEID = 10;
  CI_AAPOS = 11;
  CI_MUTANTALLELE = 12;
  CI_WILDALLELE = 13;
  CI_RATEPRIMATE = 14;
  CI_RATEMAMMAL = 15;
  CI_RATEVERTEBRATE = 16;
  CI_TIMEFITCHPRIMATE = 17;
  CI_TIMEFITCHMAMMAL = 18;
  CI_TIMEFITCHVERTEBRATE = 19;
  CI_EVODPRED = 20;
  CI_EVODSCORE = 21;
  CI_EVODPVAL = 22;
  CI_POLYPHENPRED_BALANCED = 23;
  CI_POLYPHENSCORE = 24;
  CI_POLYPHENPVAL = 25;
  CI_SIFTPRED_BALANCED = 26;
  CI_SIFTSCORE = 27;
  CI_SIFTPVAL = 28;
  CI_TIMEMUTPRIMATE = 29;
  CI_TIMEMUTMAMMAL = 30;
  CI_TIMEMUTVERTEBRATE = 31;
  CI_KGPOPALL = 32;
  CI_KGPOPAFR = 33;
  CI_KGPOPEUR = 34;
  CI_KGPOPAMER = 35;
  CI_KGPOPASIAN = 36;
  CI_EXPOPAFRAMER = 37;
  CI_EXPOPEURAMER = 38;
  CI_GRANTHAMDIST = 39;
  CI_BLOSUM62 = 40;
  CI_CONSPRED = 41;
  CI_POLYPHENPRED = 42;
  CI_SIFTPRED = 43;

  { these are the column indices for the PredictionsDrawGrid in the MutationExplorer}
  PDG_RSID_COL = 1;
  PDG_PEPTIDE_ID_COL = 2;
  PDG_MRNA_ID_COL = 3;
  PDG_REFERENCE_AA_COL = 4;
  PDG_MUTANT_AA_COL = 5;
  PDG_CONSENSUS_COL = 6;
  PDG_EVOD_PRED_COL = 7;
  PDG_EVOD_P_VALUE_COL = 8;
  PDG_POLYPHEN_2_ORIGINAL_COL = 9;
  PDG_POLYPHEN_2_BALANCED_COL = 10;
  PDG_SIFT_ORIGINAL_COL = 11;
  PDG_SIFT_BALANCED_COL = 12;
  PDG_EVOD_COL = 13;
  PDG_POLYPHEN_2_COL = 14;
  PDG_SIFT_COL = 15;
  PDG_GRANTHAM_DISTANCE_COL = 16;
  PDG_BLOSUM_62_COL = 17;

type
  TFieldArray = array of Variant;

  { TGeneEntry }

  TGeneEntry = class(TObject)
    GeneName: String;
    MRnaID: String;
    PeptideID: String;
    Product: String;

    constructor Create(aName, aMrnaId, aPeptideId, aProduct: String); overload;
    constructor Create(Source: TGeneEntry); overload;
    procedure Assign(Source: TGeneEntry);
  End;

  TEvodSearchResult = class(TObject)
  private
    results: Array of TGeneEntry;
    function getNumResults: Integer;
  public
    error: boolean;
    procedure addResult(toAdd: TGeneEntry);
    procedure parseXML(filename: String);
    procedure processTag(Node: TDomNode; theResult: TGeneEntry = nil);
    function getResult(index: Integer): TGeneEntry;
  published
    property numResults : Integer read getNumResults;
  end;

  TEvoDResult = class(TObject)
    mutant_allele: String;
    wild_allele: String;
    rate_category: String;
    impact_score: String;
    prediction: String;
    confidence: String;
  end;

  TEvoDPrediction = Class(TObject)
  private
    results: Array of TEvoDResult;
    nmid: String;
    aa_pos: Integer;
    mut_aa: String;
    procedure addResult(toAdd: TEvoDResult);
    procedure parseXML(filename: String);
    procedure processTag(Node: TDomNode; theResult: TEvoDResult = nil);
  public
    TempFileName: String;
    constructor create(nmid: String; aa_pos: Integer; mut_aa: String);
  End;

  { TMutationDiagnosis }

  TMutationDiagnosis = class(TObject)
    private
      FRsid: String;
      FPeptideid: String;
      FMrnaid: String;
      FWildallele: String;
      FMutantallele: String;
      FConspred: String;
      FEvodpred: String;
      FEvodpval: Extended;
      FPolyphenpred: String;
      FPolyphenpred_Balanced: String;
      FSiftpred: String;
      FSiftpred_Balanced: String;
      FEvodscore: Extended;
      FPolyphenscore: Extended;
      FSiftscore: Extended;
      FGranthamdist: Extended;
      FBlosum62: Extended;
      FRatevertebrate: Extended;
      FTimefitchvertebrate: Extended;
      FTimemutvertebrate: Extended;
      FChromosome: String;
      FChromosomepos: Int64;
      FStrand: String;
      FNucpos: Int64;
      FAapos: Int64;
      FWildnuc: String;
      FMutantnuc: String;
      function GetAapos: Int64;
      function GetBlosum62: Extended;
      function GetChromosome: String;
      function GetChromosomepos: Int64;
      function GetConspred: String;
      function GetEvodpred: String;
      function GetEvodpval: Extended;
      function GetEvodscore: Extended;
      function GetGranthamdist: Extended;
      function GetMrnaid: String;
      function GetMutantallele: String;
      function GetMutantnuc: String;
      function GetNucpos: Int64;
      function GetPeptideid: String;
      function GetPolyphenpred: String;
      function GetPolyphenpred_Balanced: String;
      function GetPolyphenscore: Extended;
      function GetRatevertebrate: Extended;
      function GetRsid: String;
      function GetSiftpred: String;
      function GetSiftpred_Balanced: String;
      function GetSiftscore: Extended;
      function GetStrand: String;
      function GetTimefitchvertebrate: Extended;
      function GetTimemutvertebrate: Extended;
      function GetWildallele: String;
      function GetWildnuc: String;
      procedure SetAapos(AValue: Int64);
      procedure SetBlosum62(AValue: Extended);
      procedure SetChromosome(AValue: String);
      procedure SetChromosomepos(AValue: Int64);
      procedure SetConspred(AValue: String);
      procedure SetEvodpred(AValue: String);
      procedure SetEvodpval(AValue: Extended);
      procedure SetEvodscore(AValue: Extended);
      procedure SetGranthamdist(AValue: Extended);
      procedure SetMrnaid(AValue: String);
      procedure SetMutantallele(AValue: String);
      procedure SetMutantnuc(AValue: String);
      procedure SetNucpos(AValue: Int64);
      procedure SetPeptideid(AValue: String);
      procedure SetPolyphenpred(AValue: String);
      procedure SetPolyphenpred_Balanced(AValue: String);
      procedure SetPolyphenscore(AValue: Extended);
      procedure SetRatevertebrate(AValue: Extended);
      procedure SetRsid(AValue: String);
      procedure SetSiftpred(AValue: String);
      procedure SetSiftpred_Balanced(AValue: String);
      procedure SetSiftscore(AValue: Extended);
      procedure SetStrand(AValue: String);
      procedure SetTimefitchvertebrate(AValue: Extended);
      procedure SetTimemutvertebrate(AValue: Extended);
      procedure SetWildallele(AValue: String);
      procedure SetWildnuc(AValue: String);
      function GetDoubleFromVariant(aValue: Variant): Double;
      function GetIntFromVariant(aValue: Variant): Integer;
    public
      constructor Create(Fields: TFieldArray);
      function TextSearch(query: String; startCol: Integer = 1): Integer;
      property Rsid: String read GetRsid write SetRsid;
      property Peptideid: String read GetPeptideid write SetPeptideid;
      property Mrnaid: String read GetMrnaid write SetMrnaid;
      property Wildallele: String read GetWildallele write SetWildallele;
      property Mutantallele: String read GetMutantallele write SetMutantallele;
      property Conspred: String read GetConspred write SetConspred;
      property Evodpred: String read GetEvodpred write SetEvodpred;
      property Evodpval: Extended read GetEvodpval write SetEvodpval;
      property Polyphenpred: String read GetPolyphenpred write SetPolyphenpred;
      property Polyphenpred_Balanced: String read GetPolyphenpred_Balanced write SetPolyphenpred_Balanced;
      property Siftpred: String read GetSiftpred write SetSiftpred;
      property Siftpred_Balanced: String read GetSiftpred_Balanced write SetSiftpred_Balanced;
      property Evodscore: Extended read GetEvodscore write SetEvodscore;
      property Polyphenscore: Extended read GetPolyphenscore write SetPolyphenscore;
      property Siftscore: Extended read GetSiftscore write SetSiftscore;
      property Granthamdist: Extended read GetGranthamdist write SetGranthamdist;
      property Blosum62: Extended read GetBlosum62 write SetBlosum62;
      property Ratevertebrate: Extended read GetRatevertebrate write SetRatevertebrate;
      property Timefitchvertebrate: Extended read GetTimefitchvertebrate write SetTimefitchvertebrate;
      property Timemutvertebrate: Extended read GetTimemutvertebrate write SetTimemutvertebrate;
      property Chromosome: String read GetChromosome write SetChromosome;
      property Chromosomepos: Int64 read GetChromosomepos write SetChromosomepos;
      property Strand: String read GetStrand write SetStrand;
      property Nucpos: Int64 read GetNucpos write SetNucpos;
      property Aapos: Int64 read GetAapos write SetAapos;
      property Wildnuc: String read GetWildnuc write SetWildnuc;
      property Mutantnuc: String read GetMutantnuc write SetMutantnuc;
  end;

  function StringToCaseSelect(Selector : string; CaseList: array of string): Integer;
{$ENDIF}
implementation

{$IFDEF VISUAL_BUILD}
{ Unit functions }

function StringToCaseSelect(Selector : string; CaseList: array of string): Integer;
var
  cnt: integer;
begin
  Result:=-1;
  for cnt:=0 to Length(CaseList)-1 do
begin
  if CompareText(Selector, CaseList[cnt]) = 0 then
    begin
      Result:=cnt;
      Break;
    end;
  end;
end;

{ TGeneEntry }

constructor TGeneEntry.Create(aName, aMrnaId, aPeptideId, aProduct: String);
begin
  GeneName := aName;
  MrnaId := aMrnaId;
  PeptideId := aPeptideId;
  Product := aProduct;
end;

constructor TGeneEntry.Create(Source: TGeneEntry);
begin
  Assign(Source);
end;

procedure TGeneEntry.Assign(Source: TGeneEntry);
begin
  GeneName := Source.GeneName;
  MrnaId := Source.MrnaId;
  PeptideId := Source.PeptideId;
  Product := Source.Product;
end;

{ TMutationDiagnosis }

function TMutationDiagnosis.GetAapos: Int64;
begin
  Result := FAapos;
end;

function TMutationDiagnosis.GetBlosum62: Extended;
begin
  Result := FBlosum62;
end;

function TMutationDiagnosis.GetChromosome: String;
begin
  Result := FChromosome;
end;

function TMutationDiagnosis.GetChromosomepos: Int64;
begin
  Result := FChromosomepos;
end;

function TMutationDiagnosis.GetConspred: String;
begin
  Result := FConspred;
end;

function TMutationDiagnosis.GetEvodpred: String;
begin
  Result := FEvodpred;
end;

function TMutationDiagnosis.GetEvodpval: Extended;
begin
  Result := FEvodpval;
end;

function TMutationDiagnosis.GetEvodscore: Extended;
begin
  Result := FEvodscore;
end;

function TMutationDiagnosis.GetGranthamdist: Extended;
begin
  Result := FGranthamdist;
end;

function TMutationDiagnosis.GetMrnaid: String;
begin
  Result := FMrnaid;
end;

function TMutationDiagnosis.GetMutantallele: String;
begin
  Result := FMutantallele;
end;

function TMutationDiagnosis.GetMutantnuc: String;
begin
  Result := FMutantnuc;
end;

function TMutationDiagnosis.GetNucpos: Int64;
begin
  Result := FNucpos;
end;

function TMutationDiagnosis.GetPeptideid: String;
begin
  Result := FPeptideid;
end;

function TMutationDiagnosis.GetPolyphenpred: String;
begin
  Result := FPolyphenpred;
end;

function TMutationDiagnosis.GetPolyphenpred_Balanced: String;
begin
  Result := FPolyphenpred_Balanced;
end;

function TMutationDiagnosis.GetPolyphenscore: Extended;
begin
  Result := FPolyphenscore;
end;

function TMutationDiagnosis.GetRatevertebrate: Extended;
begin
  Result := FRatevertebrate;
end;

function TMutationDiagnosis.GetRsid: String;
begin
  Result := FRsid;
end;

function TMutationDiagnosis.GetSiftpred: String;
begin
  Result := FSiftpred;
end;

function TMutationDiagnosis.GetSiftpred_Balanced: String;
begin
  Result := FSiftpred_Balanced;
end;

function TMutationDiagnosis.GetSiftscore: Extended;
begin
  Result := FSiftscore;
end;

function TMutationDiagnosis.GetStrand: String;
begin
  Result := FStrand;
end;

function TMutationDiagnosis.GetTimefitchvertebrate: Extended;
begin
  Result := FTimefitchvertebrate;
end;

function TMutationDiagnosis.GetTimemutvertebrate: Extended;
begin
  Result := FTimemutvertebrate;
end;

function TMutationDiagnosis.GetWildallele: String;
begin
  Result := FWildallele;
end;

function TMutationDiagnosis.GetWildnuc: String;
begin
  Result := FWildnuc;
end;

procedure TMutationDiagnosis.SetAapos(AValue: Int64);
begin
  FAapos := AValue;
end;

procedure TMutationDiagnosis.SetBlosum62(AValue: Extended);
begin
  FBlosum62:= AValue;
end;

procedure TMutationDiagnosis.SetChromosome(AValue: String);
begin
  FChromosome:= AValue;
end;

procedure TMutationDiagnosis.SetChromosomepos(AValue: Int64);
begin
  FChromosomepos:= AValue;
end;

procedure TMutationDiagnosis.SetConspred(AValue: String);
begin
  FConspred:= AValue;
end;

procedure TMutationDiagnosis.SetEvodpred(AValue: String);
begin
  FEvodpred:= AValue;
end;

procedure TMutationDiagnosis.SetEvodpval(AValue: Extended);
begin
  FEvodpval:= AValue;
end;

procedure TMutationDiagnosis.SetEvodscore(AValue: Extended);
begin
  FEvodscore:= AValue;
end;

procedure TMutationDiagnosis.SetGranthamdist(AValue: Extended);
begin
  FGranthamdist:= AValue;
end;

procedure TMutationDiagnosis.SetMrnaid(AValue: String);
begin
  FMrnaid:= AValue;
end;

procedure TMutationDiagnosis.SetMutantallele(AValue: String);
begin
  FMutantallele:= AValue;
end;

procedure TMutationDiagnosis.SetMutantnuc(AValue: String);
begin
  FMutantnuc:= AValue;
end;

procedure TMutationDiagnosis.SetNucpos(AValue: Int64);
begin
  FNucpos:= AValue;
end;

procedure TMutationDiagnosis.SetPeptideid(AValue: String);
begin
  FPeptideid:= AValue;
end;

procedure TMutationDiagnosis.SetPolyphenpred(AValue: String);
begin
  FPolyphenpred:= AValue;
end;

procedure TMutationDiagnosis.SetPolyphenpred_Balanced(AValue: String);
begin
  FPolyphenpred_Balanced:= AValue;
end;

procedure TMutationDiagnosis.SetPolyphenscore(AValue: Extended);
begin
  FPolyphenscore:= AValue;
end;

procedure TMutationDiagnosis.SetRatevertebrate(AValue: Extended);
begin
  FRatevertebrate:= AValue;
end;

procedure TMutationDiagnosis.SetRsid(AValue: String);
begin
  FRsid:= AValue;
end;

procedure TMutationDiagnosis.SetSiftpred(AValue: String);
begin
  FSiftpred:= AValue;
end;

procedure TMutationDiagnosis.SetSiftpred_Balanced(AValue: String);
begin
  FSiftpred_Balanced:= AValue;
end;

procedure TMutationDiagnosis.SetSiftscore(AValue: Extended);
begin
  FSiftscore:= AValue;
end;

procedure TMutationDiagnosis.SetStrand(AValue: String);
begin
  FStrand:= AValue;
end;

procedure TMutationDiagnosis.SetTimefitchvertebrate(AValue: Extended);
begin
  FTimefitchvertebrate:= AValue;
end;

procedure TMutationDiagnosis.SetTimemutvertebrate(AValue: Extended);
begin
  FTimemutvertebrate:= AValue;
end;

procedure TMutationDiagnosis.SetWildallele(AValue: String);
begin
  FWildallele:= AValue;
end;

procedure TMutationDiagnosis.SetWildnuc(AValue: String);
begin
  FWildnuc:= AValue;
end;

function TMutationDiagnosis.GetDoubleFromVariant(aValue: Variant): Double;
begin
  if VarIsNumeric(aValue) then
    Result := Double(aValue)
  else
    Result := BAD_NUMBER;
end;

function TMutationDiagnosis.GetIntFromVariant(aValue: Variant): Integer;
begin
  if VarIsNumeric(aValue) then
    Result := Integer(aValue)
  else
    Result := BAD_NUMBER;
end;

constructor TMutationDiagnosis.Create(Fields: TFieldArray);
begin
  FRsid := String(Fields[CI_RSID]);
  FPeptideid := String(Fields[CI_PEPTIDEID]);
  FMrnaid := String(Fields[CI_MRNAID]);
  FWildallele := String(Fields[CI_WILDALLELE]);
  FMutantallele := String(Fields[CI_MUTANTALLELE]);
  FConspred := String(Fields[CI_CONSPRED]);
  FEvodpred := String(Fields[CI_EVODPRED]);
  FEvodpval := GetDoubleFromVariant(Fields[CI_EVODPVAL]);
  FPolyphenpred := String(Fields[CI_POLYPHENPRED]);
  FPolyphenpred_Balanced := String(Fields[CI_POLYPHENPRED_BALANCED]);
  FSiftpred := String(Fields[CI_SIFTPRED]);
  FSiftpred_Balanced := String(Fields[CI_SIFTPRED_BALANCED]);

  FEvodscore := GetDoubleFromVariant(Fields[CI_EVODSCORE]);
  FPolyphenscore := GetDoubleFromVariant(Fields[CI_POLYPHENSCORE]);
  FSiftscore := GetDoubleFromVariant(Fields[CI_SIFTSCORE]);
  FGranthamdist := GetDoubleFromVariant(Fields[CI_GRANTHAMDIST]);
  FBlosum62 := GetDoubleFromVariant(Fields[CI_BLOSUM62]);
  FRatevertebrate := GetDoubleFromVariant(Fields[CI_RATEVERTEBRATE]);
  FTimefitchvertebrate := GetDoubleFromVariant(Fields[CI_TIMEFITCHVERTEBRATE]);
  FTimemutvertebrate := GetDoubleFromVariant(Fields[CI_TIMEMUTVERTEBRATE]);
  FChromosome := String(Fields[CI_CHROMOSOME]);
  FChromosomepos := GetIntFromVariant(Fields[CI_CHROMOSOMEPOS]);
  FStrand := String(Fields[CI_STRAND]);
  FNucpos := GetIntFromVariant(Fields[CI_NUCPOS]);
  FAapos := GetIntFromVariant(Fields[CI_AAPOS]);
  FWildnuc := String(Fields[CI_WILDNUC]);
  FMutantnuc := String(Fields[CI_MUTANTNUC]);
end;

function TMutationDiagnosis.TextSearch(query: String; startCol: Integer = 1): Integer;
var
  aQuery: String;
begin
  Result := -1;
  aQuery := UpperCase(query);
  if (startCol <= PDG_RSID_COL) and (Pos(aQuery, Rsid) > 0) then
    Result := PDG_RSID_COL
  else if (startCol <= PDG_PEPTIDE_ID_COL) and (Pos(aQuery, UpperCase(Peptideid)) > 0) then
    Result := PDG_PEPTIDE_ID_COL
  else if (startCol <= PDG_MRNA_ID_COL) and (Pos(aQuery, UpperCase(Mrnaid)) > 0) then
    Result := PDG_MRNA_ID_COL
  else if (startCol <= PDG_REFERENCE_AA_COL) and (Pos(aQuery, UpperCase(Wildallele)) > 0) then
    Result := PDG_REFERENCE_AA_COL
  else if (startCol <= PDG_MUTANT_AA_COL) and (Pos(aQuery, UpperCase(Mutantallele)) > 0) then
    Result := PDG_MUTANT_AA_COL
  else if (startCol <= PDG_CONSENSUS_COL) and (Pos(aQuery, UpperCase(Conspred)) > 0) then
    Result := PDG_CONSENSUS_COL
  else if (startCol <= PDG_EVOD_PRED_COL) and (Pos(aQuery, UpperCase(Evodpred)) > 0) then
    Result := PDG_EVOD_PRED_COL
  else if (startCol <= PDG_EVOD_P_VALUE_COL) and (Pos(aQuery, UpperCase(FloatToStr(Evodpval))) > 0) then
    Result := PDG_EVOD_P_VALUE_COL
  else if (startCol <= PDG_POLYPHEN_2_ORIGINAL_COL) and (Pos(aQuery, UpperCase(Polyphenpred)) > 0) then
    Result := PDG_POLYPHEN_2_ORIGINAL_COL
  else if (startCol <= PDG_POLYPHEN_2_BALANCED_COL) and (Pos(aQuery, UpperCase(Polyphenpred_Balanced)) > 0) then
    Result := PDG_POLYPHEN_2_BALANCED_COL
  else if (startCol <= PDG_SIFT_ORIGINAL_COL) and (Pos(aQuery, UpperCase(Siftpred)) > 0) then
    Result := PDG_SIFT_ORIGINAL_COL
  else if (startCol <= PDG_SIFT_BALANCED_COL) and (Pos(aQuery, UpperCase(Siftpred_Balanced)) > 0) then
    Result := PDG_SIFT_BALANCED_COL
  else if (startCol <= PDG_EVOD_COL) and (Pos(aQuery, UpperCase(FloatToStr(Evodscore))) > 0) then
    Result := PDG_EVOD_COL
  else if (startCol <= PDG_POLYPHEN_2_COL) and (Pos(aQuery, UpperCase(FloatToStr(Polyphenscore))) > 0) then
    Result := PDG_POLYPHEN_2_COL
  else if (startCol <= PDG_SIFT_COL) and (Pos(aQuery, UpperCase(FloatToStr(Siftscore))) > 0) then
    Result := PDG_SIFT_COL
  else if (startCol <= PDG_GRANTHAM_DISTANCE_COL) and (Pos(aQuery, UpperCase(FloatToStr(Granthamdist))) > 0) then
    Result := PDG_GRANTHAM_DISTANCE_COL
  else if (startCol <= PDG_BLOSUM_62_COL) and (Pos(aQuery, UpperCase(FloatToStr(Blosum62))) > 0) then
    Result := PDG_BLOSUM_62_COL;
end;

{ TEvodSearchResult }

procedure TEvodSearchResult.processTag(Node: TDomNode; theResult: TGeneEntry = nil);
var
  i: Integer;
begin
    try
      case StringToCaseSelect(Node.NodeName, ['result','error','gene_name', 'nmid','npid','product']) of
        0:
        begin
          theResult := TGeneEntry.Create;
          addResult(theResult);

          for I := 0 to Node.ChildNodes.Count-1 do
            processTag(Node.ChildNodes[I], theResult);
        end;
        1:
          begin
  //          ShowMessage(Node.NodeValue);  { TODO 1 -oglen -cmypeg : this needs to be handled by the interface, not the searchresult class }
          end;
        2: theResult.GeneName := Node.FirstChild.NodeValue;
        3: theResult.MRnaID := Node.FirstChild.NodeValue;
        4: theResult.PeptideID := Node.FirstChild.NodeValue;
        5: theResult.Product := Node.FirstChild.NodeValue;
      end;
    except
      on E: Exception do
      begin
        ShowMessage( e.Message);
        Exit;
      end;
    end;
end;

procedure TEvodSearchResult.addResult(toAdd: TGeneEntry);
begin
  setLength(results, length(results)+1);
  results[length(results)-1] := toAdd;
end;

function TEvodSearchResult.getNumResults: Integer;
begin
  result := length(results);
end;

function TEvodSearchResult.getResult(index: Integer): TGeneEntry;
begin
  if ((index < length(results)) and (index >= 0)) then
    result := results[index];
end;

procedure TEvodSearchResult.parseXML(filename: String);
var
  xml: TXMLDocument;
  Node, Root: TDomNode;
  i: Integer;
begin
  try
    ReadXmlFile(xml, filename);
    root := xml.FirstChild;
    for i:=0 to Root.ChildNodes.Count-1 do  // read in all the rows whcih are children of the <AppLink>
    begin
      Node := Root.ChildNodes[i];
      ProcessTag(Node);
    end;
  finally
    xml.Free;
  end;
end;

{ TEvoDPrediction }

constructor TEvoDPrediction.create(nmid: String; aa_pos: Integer;
  mut_aa: String);
begin
  self.nmid := nmid;
  self.aa_pos := aa_pos;
  self.mut_aa := mut_aa;
end;

procedure TEvoDPrediction.parseXML(filename: String);
var
  xml: TXMLDocument;
  Node, Root: TDomNode;
  i: Integer;
begin
  ReadXmlFile(xml, filename);

  // parse this stuff
  root := xml.FirstChild;
  for i:=0 to Root.ChildNodes.Count-1 do  // read in all the rows whcih are children of the <AppLink>
  begin
    Node := Root.ChildNodes[i];
    ProcessTag(Node);
  end;
end;

procedure TEvoDPrediction.addResult(toAdd: TEvoDResult);
begin
  setLength(results, length(results)+1);
  results[length(results)-1] := toAdd;
end;

procedure TEvoDPrediction.processTag(Node: TDomNode; theResult: TEvoDResult = nil);
var
  i: Integer;
begin
    try
	  case StringToCaseSelect(Node.NodeName, ['result','wild_allele','rate_category','impact_score', 'prediction','confidence','mutant_allele','error']) of
        0:
        begin
          theResult := TEvoDResult.Create;
          addResult(theResult);

          for I := 0 to Node.ChildNodes.Count-1 do
            processTag(Node.ChildNodes[I], theResult);
        end;
        1: theResult.wild_allele := Node.NodeValue;
        2: theResult.rate_category := Node.NodeValue;
        3: theResult.impact_score := Node.NodeValue;
        4: theResult.prediction := Node.NodeValue;
        5:
        begin
          if Node.NodeValue <> null then
            theResult.confidence := Node.NodeValue;
        end;
        6: theResult.mutant_allele := Node.NodeValue;
        7: ShowMessage(Node.NodeValue);
      end;

    except
      on E: Exception do
      begin
        ShowMessage( e.Message);
        Exit;
      end;
    end;
end;


{$ENDIF}
end.


