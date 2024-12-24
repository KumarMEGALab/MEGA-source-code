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

unit MD_Sequences;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF VISUAL_BUILD}
 Dialogs,
  {$ENDIF}
  Classes, KeywordConsts, StringUtils, Controls, StrHashMap, MegaConsts, mstringbuilder;

const
  FULL_SEQUENCE_STR = 'full sequence';

type
  TWebSeqDataFormat = (wsdfGenbank, wsdfFasta, wsdfUnknown);

  TCDS = record
    definition: String;
    accession: String;
    gene: string;
    allele: string;
    complement: boolean;
    n: integer;
    join: array [0..99] of TPoint;
  end;

  TCDSArray = array of TCDS;
  TCDSArrayArray = array of TCDSArray;

  { TSequence }

  TSequence = class
  private
    FIndex: Integer;
    function GetAA(Codon: array of AnsiChar): AnsiChar;
    function GetSeqData:AnsiString;
    procedure SetSeqData(Sequence :AnsiString);
  public
    CodeTable : AnsiString;
    FSeqData: AnsiString;
    FSeqDataTranslated: AnsiString;
    FSeqName: AnsiString;
    FSeqInfo: AnsiString;
    FSeqId: AnsiString;  // the unique identifier for this sequence
    FSeqIdPrefix: AnsiString;
    FAccessionNum: AnsiString;
    FGroupName: AnsiString;
    FSpeciesName: AnsiString;
    FPopulationName: AnsiString;
    FUIDS: AnsiString;
    FOrderNum: AnsiString;
    FFileName: String;
    FMarkedSiteIndex: integer;
    FisTranslated : Boolean;

    FSpecies: AnsiString;
    FSubspecies: AnsiString;
    FStrain:  AnsiString;
    FGene:    AnsiString;
    FAllele:  AnsiString;
    FHost:     AnsiString;
    FSenotype: AnsiString;

    function GetBase(index: integer):AnsiChar;
    procedure SetBase(index: integer; value: AnsiChar);
    function GetNoOfSites:integer;
  public
    constructor Create;
    procedure SetSeqDataLength(aLength: Int64);
    property Base[index: integer]: AnsiChar read GetBase write SetBase; default;
    property SeqData: AnsiString read GetSeqData write SetSeqData;
    property SeqName: AnsiString read FSeqName write FSeqName;
    property SeqInfo: AnsiString read FSeqInfo write FSeqInfo;
    property SeqId: AnsiString read FSeqId write FSeqId;
    property SeqIdPrefix: AnsiString read FSeqIdPrefix write FSeqIdPrefix;
    property AccessionNum: AnsiString read FAccessionNum write FAccessionNum;
    property GroupName: AnsiString read FGroupName write FGroupName;
    property SpeciesName: AnsiString read FSpeciesName write FSpeciesName;
    property PopulationName: AnsiString read FPopulationName write FPopulationName;
    property UIDS: AnsiString read FUIDS write FUIDS;
    property OrderNum: AnsiString read FOrderNum write FOrderNum;
    property FileName: String read FFileName write FFileName;
    property NoOfSites: integer read GetNoOfSites;
    property MarkedSiteIndex: integer read FMarkedSiteIndex write FMarkedSiteIndex;

    property Species: AnsiString read FSpecies write FSpecies;
    property Subspecies: AnsiString read FSubspecies write FSubspecies;
    property Strain:  AnsiString read FStrain write FStrain;
    property Gene:    AnsiString read FGene write FGene;
    property Allele:  AnsiString read FAllele write FAllele;
    property Host:     AnsiString read FHost write FHost;
    property Senotype: AnsiString read FSenotype write FSenotype;
    property Index: Integer read FIndex write FIndex;
    function Reverse(x0: Integer; x1: Integer):AnsiString;
    function Compliment(x0: Integer; x1: Integer):AnsiString;
    procedure Translate;
    procedure UnTranslate(DnaStringBuilder: TMegaStringBuilder);
    procedure Assign(source: TSequence);
    function ReverseCompliment: AnsiString;
  end;


   { TSequenceList }

   TSequenceList = class(TList)
  public
    FProgress: Integer;
    FIsDNA: boolean;
    FIsProteinCoding: boolean;
    FTitle: AnsiString;
    function GetItems(index: integer):TSequence;
    procedure SetItems(index: integer; value: TSequence);
    function GetMaxNoOfSites:integer;
    function GetMinNoOfSites:integer;
    function FormatLabelForMegaFile(aLabel: String): String;
    procedure FormatSequenceBlock(const aSequence: String; var aList: TStringList);
    procedure AppendMissingSeq(aSeq: TSequence; missingCharSym: Char; NumSites: Integer); virtual;
    procedure LengthenShortSeqs(missingCharSym: Char); virtual;
    function LongestSeqLength: Integer; virtual;
  public
    CheckCancel: TCheckCancelFunc;
    CodeTable : AnsiString;
    function HasDuplicateSeqNames: Boolean;
    function DataTypeString: String;
    function HasAtLeast2SeqsWithSitesWithBases: Boolean;
    function HasGroupNames: Boolean;
    function GetMegaSequenceAlignmentFileStrings(Description: String; aGapSymbol, aIdenticalSymbol, aMissingSymbol: Char): TStringList;
    function IndexOfSeqName(aName: String): Integer; virtual;
    property Title: AnsiString read FTitle write FTitle;
    property Items[Index: integer]: TSequence read GetItems write SetItems; default;
    property IsDNA: boolean read FIsDNA write FIsDNA;
    property IsProteinCoding: boolean read FIsProteinCoding write FIsProteinCoding;
    property MaxNoOfSites: integer read GetMaxNoOfSites;
    property MinNoOfSites: integer read GetMinNoOfSites;

    procedure Clear; override;
    procedure RelinquishSequences;
    procedure DeleteAll; { remove all seqs from the list - needed because when exporting to alignment explorer, no new copies are made so this avoids double free}
    procedure Assign(Source: TSequenceList); virtual;
    function AppendSequences(aList: TSequenceList; missingCharSym: Char = '?'; DoStrict: Boolean = True): Boolean; virtual;
    procedure AddSequences(aList: TSequenceList);
    Procedure Translate;
    procedure UnTranslate;
    procedure Polish;
    procedure Pack;
    function ToStringList: TStringList;
    function LooksLikeDna: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  { TFastSequenceList }

  TFastSequenceList = class(TSequenceList)
    private
      FFastLookup: TStringHashMap;
      FTaxaData: array of TList;
      FSeqLengths: array of Int64;
      FNumTotalSites: Int64;
      function GetSeqLength(index: Integer): Int64;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Finalize;
      procedure LengthenShortSeqs(missingCharSym: Char); override;
      function IndexOfSeqName(aName: AnsiString): Integer; override;
      function LongestSeqLength: Integer; override;
      procedure Assign(Source: TSequenceList); override;
      function AppendSequences(aList: TSequenceList; missingCharSym: Char = '?'; DoStrict: Boolean = True): Boolean; override;
      procedure AppendMissingSeq(aSeq: TSequence; missingCharSym: Char; NumSites: Integer); reintroduce;
      procedure AppendExistingSeq(aSeq: TSequence; Index: Integer);
      property NumTotalSites: Int64 read FNumTotalSites;
      property SeqLengths[index: Integer]: Int64 read GetSeqLength; default;
  end;

  { TSequenceFactory }

  TSequenceFactory = class
    private
      FErrorMsg: String;
      FRawData: TStringList;
      FNumCDSs: Integer;
      FCDSs: array of TCDS;
      tempCDSs: array of TCDS;
      function PullOutIdNumber(SequenceIdString: String; Identifier: String): String;
      function PullOutSequenceName(SequenceIdString: String): String;
      function ValidateFastaSequenceId(RawSeqId: String): boolean;
      function ValidateSequenceData(RawSeqData: String): boolean;
      function ReverseComplement(Source: AnsiString): AnsiString;
      function ExtractAccession(Source: AnsiString): AnsiString;
      function ExtractVersion(Source: AnsiString): AnsiString;
      function ExtractDefinition(var aSeq: TSequence): Boolean;
      function ExtractFeatures(var aSeq: TSequence): Boolean;
      function ExtractSequenceData(var aSeq: TSequence): Boolean; overload;
      function FindOrigin: Integer;
      function IsBoundary(Source: AnsiString): Boolean;

      function FindFeaturesLine: Integer;
      function FindDefinitionLine: Integer;
      function FindAccessionLine: Integer;
      function ExtractCDSs(const aCDS: TCDSArray; var aSeq: TSequence): Boolean;
    public
      constructor Create;
      destructor Destroy; override;
      function ExtractSequenceData(const GenbankResult: TStringList; var aSeq: TSequence): Integer; overload;
      function ExtractAllGenbankData(const GenbankResult: TStringList; const aCDS: TCDSArray; var aSeq: TSequence): Boolean; overload;
      function ExtractAllGenbankData(const GenbankResult: TStringList; var aSeq: TSequence): Boolean; overload;
      function CheckHasCDSs(RawData: TStringList): Boolean;
      function GetCDSsForData(RawData: TStringList): TCDSArray;
      function DetermineDataType(SequenceData: String): TSnTokenCode;
      function ValidateFasta(SequenceId: String; SequenceData: String): boolean; Overload;
      function ValidateMessyFasta(RawData: TStringList): boolean;
      function ValidateFasta(var RawData: TStringList): boolean; Overload;
      function ValidateGenbank(RawData: TStringList): boolean;
      function GenerateFastaSequenceObject(SequenceIdString: String; SequenceData:String): TSequence;
      function GenerateGenbankSequenceObject(GenbankResult: TStringList): TSequence;
      function FindGenbankAccessionNum(GenbankData: TStringList): String;
      property ErrorMsg: String read FErrorMsg;
  end;
  
procedure CopyCds(const source: TCDS; var destination: TCDS);

implementation

uses
  {$IFDEF VISUAL_BUILD}
  htmloptionsdlg,
  {$ENDIF}
  SysUtils, StrUtils, Math, mglobalsettings;

procedure CopyCds(const source: TCDS; var destination: TCDS);
var
  j: Integer;
begin
  destination.gene := source.gene;
  destination.allele := source.allele;
  destination.complement := source.complement;
  destination.n := source.n;
  for j := Low(destination.join) to High(destination.join) do
  begin
    destination.join[j].x := source.join[j].x;
    destination.join[j].y := source.join[j].y;
  end;
end;

{ TFastSequenceList }

function TFastSequenceList.GetSeqLength(index: Integer): Int64;
begin
  if (index >= Low(FSeqLengths)) and (index <= High(FSeqLengths)) then
    Result := FSeqLengths[index]
  else
    Result := -1;
end;

constructor TFastSequenceList.Create;
begin
  inherited Create;
  SetLength(FTaxaData, 0);
  SetLength(FSeqLengths, 0);
  FNumTotalSites := 0;
  FFastLookup := TStringHashMap.Create;
end;

destructor TFastSequenceList.Destroy;
var
  i, j: Integer;
  myData: PAnsiChar;
begin
  if Length(FTaxaData) > 0 then
    for i := 0 to Length(FTaxaData) - 1 do
    begin
      if FTaxaData[i].Count > 0 then
        for j := 0 to FTaxaData[i].Count - 1 do
        begin
          myData := FTaxaData[i][j];
          FreeMemAndNil(myData);
        end;
    end;
  if Assigned(FFastLookup) then
    FFastLookup.Free;
  inherited Destroy;
end;

procedure TFastSequenceList.Finalize;
var
  i, j, m: Integer;
  myData: PAnsiChar;
  curSite: Int64;
  aSeq: TSequence;
  temp: AnsiString;
  aLongestSeqLength: Int64;
begin
  if Count = 0 then
    Exit;
  aLongestSeqLength := LongestSeqLength;
  for i := 0 to Count - 1 do
  begin
    curSite := 1;
    aSeq := Items[i];
    temp := aSeq.GetSeqData;
    SetLength(temp, aLongestSeqLength);
    if FTaxaData[i].Count > 0 then
    begin
      for j := 0 to FTaxaData[i].Count - 1 do
      begin
        myData := FTaxaData[i][j];
        m := 0;
        while myData[m] <> #0 do
        begin
          temp[curSite] := myData[m];
          inc(m);
          inc(curSite);
        end;
      end;
      aSeq.SetSeqData(temp);
    end;
  end;
end;

procedure TFastSequenceList.LengthenShortSeqs(missingCharSym: Char);
var
  i, j: Integer;
  maxLen: Integer;
  myPchar: PAnsiChar;
  delta: Integer;
begin
  maxLen := LongestSeqLength;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if FSeqLengths[i] < maxLen then
      begin
        delta := (maxLen - FSeqLengths[i]);
        GetMem(myPchar, (delta + 1)*SizeOf(AnsiChar));
        for j := 0 to delta - 1 do
          myPchar[j] := missingCharSym;
        myPchar[delta] := #0;
        FTaxaData[i].Add(myPchar);
        FSeqLengths[i] := FSeqLengths[i] + delta;
      end;
end;

function TFastSequenceList.IndexOfSeqName(aName: AnsiString): Integer;
var
  p: Pointer = nil;
  aSeq: TSequence = nil;
begin
  if FFastLookup.Find(aName, p) then
  begin
    aSeq := TSequence(p);
    Result := aSeq.Index;
  end
  else
    Result := -1;
end;

function TFastSequenceList.LongestSeqLength: Integer;
var
  i: Integer;
begin
  Result := 0;
  if Length(FSeqLengths) > 0 then
    for i := 0 to Length(FSeqLengths) - 1 do
      if FSeqLengths[i] > Result then
        Result := FSeqLengths[i];
end;

procedure TFastSequenceList.Assign(Source: TSequenceList);
var
  i, j: Integer;
  MySeqPChar: PAnsiChar;
begin
  inherited Assign(Source);

  SetLength(FTaxaData, Count);
  SetLength(FSeqLengths, Count);
  if Count > 0 then
  begin
    FNumTotalSites := Items[0].NoOfSites;
    for i := 0 to Count - 1 do
    begin
      FTaxaData[i] := TList.Create;
      FSeqLengths[i] := Items[i].NoOfSites;
      GetMem(MySeqPchar, (Items[i].NoOfSites+1)*sizeof(AnsiChar));
      if Items[i].NoOfSites > 0 then
        for j := 0 to Items[i].NoOfSites - 1 do
          MySeqPchar[j] := Items[i].SeqData[j+1];
      MySeqPchar[FNumTotalSites] := #0;
      FTaxaData[i].Add(MySeqPchar);
      FFastLookup.Add(Items[i].SeqName, Items[i]);
    end;
  end;
end;

function TFastSequenceList.AppendSequences(aList: TSequenceList; missingCharSym: Char; DoStrict: Boolean): Boolean;
var
  i, Index: Integer;
  aSeq: TSequence;
  aLabel: AnsiString;
  origNumSites: Integer;
  tempLength: Integer = 0;
begin
  Result := False;
  if (not (Count = aList.Count)) and DoStrict then
    Exit;
  if Count > 0 then
  begin
    origNumSites := FNumTotalSites;
    if DoStrict then
    begin
      for i := 0 to aList.Count - 1 do
      begin
        aSeq := aList[i];
        aLabel := aSeq.SeqName;
        if (i >= Count) or (not (aLabel = items[i].SeqName)) then
          Exit
        else
          AppendExistingSeq(aSeq, i);
      end;
    end
    else
    begin
      for i := 0 to aList.Count - 1 do
      begin
        aSeq := aList[i];
        aLabel := aSeq.SeqName;
        Index := IndexOfSeqName(aLabel);
        if Index >= 0 then
        begin
          aSeq.Index := Index;
          AppendExistingSeq(aSeq, Index);
        end
        else
        begin
          aSeq.Index := Count;
          AppendMissingSeq(aSeq, missingCharSym, origNumSites)
        end;
      end;
    end;
  end
  else
  begin
    for i := 0 to aList.Count - 1 do
      AppendMissingSeq(aList[i], missingCharSym, 0);
  end;

  if not DoStrict then
    LengthenShortSeqs(missingCharSym);
  Result := True;
  tempLength := LongestSeqLength;
  if FNumTotalSites < tempLength then
    FNumTotalSites := tempLength;
end;

procedure TFastSequenceList.AppendMissingSeq(aSeq: TSequence; missingCharSym: Char; NumSites: Integer);
var
  newSeq: TSequence;
  myPSeqChar: PAnsiChar;
  i: Integer;
  p: Pointer = nil;
begin
  Assert(not FFastLookup.Find(aSeq.SeqName, p), Format('adding duplicate sequence name: %s', [aSeq.SeqName]));
  SetLength(FTaxaData, Length(FTaxaData) + 1);
  SetLength(FSeqLengths, Length(FSeqLengths) + 1);
  FTaxaData[Length(FTaxaData) - 1] := TList.Create;

  GetMem(myPSeqChar, (NumSites + aSeq.NoOfSites + 1)*SizeOf(AnsiChar));
  for i := 0 to NumSites - 1 do
    myPSeqChar[i] := missingCharSym;
  for i := 0 to aSeq.NoOfSites - 1 do
    myPSeqChar[i + NumSites] := aSeq.SeqData[i + 1];
  myPSeqChar[NumSites + aSeq.NoOfSites] := #0;
  FTaxaData[Length(FTaxaData) - 1].Add(myPSeqChar);
  FSeqLengths[Length(FSeqLengths) - 1] := NumSites + aSeq.NoOfSites;
  newSeq := TSequence.Create;
  newSeq.SeqName := aSeq.SeqName;
  newSeq.Index := Count;
  aSeq.Index := Count;
  FFastLookup.Add(newSeq.SeqName, newSeq);
  inherited Add(newSeq);
end;

procedure TFastSequenceList.AppendExistingSeq(aSeq: TSequence; Index: Integer);
var
  myPChar: PAnsiChar;
  i: Integer;
  p: Pointer = nil;
begin
  Assert(FFastLookup.Find(aSeq.SeqName, p), Format('trying to append sequence %s which does not yet exist in the list', [aSeq.SeqName]));
  GetMem(myPChar, (aSeq.NoOfSites + 1)*SizeOf(AnsiChar));
  for i := 0 to aSeq.NoOfSites - 1 do
    myPChar[i] := aSeq.SeqData[i + 1];
  myPChar[aSeq.NoOfSites] := #0;
  Assert((Index >= Low(FTaxaData)) and (Index <= High(FTaxaData)), Format('index out of bounds. Got %d but range is %d - %d', [Index, Low(FTaxaData), High(FTaxaData)]));
  FTaxaData[Index].Add(myPChar);
  FSeqLengths[Index] := FSeqLengths[Index] + aSeq.NoOfSites;
end;

function TSequence.GetBase(index: integer):AnsiChar;
begin
  result := FSeqData[index];
end;

procedure TSequence.SetBase(index: integer; value: AnsiChar);
begin
  FSeqData[index] := value;
end;

function TSequence.GetNoOfSites:integer;
begin
  result := Length(SeqData);
end;

constructor TSequence.Create;
begin
  FIndex := -1;
end;

procedure TSequence.SetSeqDataLength(aLength: Int64);
begin
  SetLength(FSeqData, aLength);
end;

procedure TSequence.Assign(source: TSequence);
begin
  FSeqData := source.SeqData;
  FSeqName := source.SeqName;
  FGroupName := source.GroupName;
  FSeqInfo := source.SeqInfo;
  FSeqId := source.SeqId;
  FSeqIdPrefix := source.SeqIdPrefix;
  FUIDS := source.UIDS;

  FSeqDataTranslated := source.FSeqDataTranslated;
  FAccessionNum      := source.FAccessionNum;
  FSpeciesName       := source.FSpeciesName;
  FPopulationName    := source.FPopulationName;
  FOrderNum          := source.FOrderNum;
  FFileName          := source.FFileName;
  FMarkedSiteIndex   := source.FMarkedSiteIndex;
  FisTranslated      := source.FisTranslated;

  FSpecies  := source.FSpecies;
  FStrain   := source.FStrain;
  FGene     := source.FGene;
  FAllele   := source.FAllele;
  FHost     := source.FHost;
  FSenotype := source.FSenotype;
  FIndex := source.FIndex;
  MarkedSiteIndex := source.MarkedSiteIndex;
end;

function TSequence.ReverseCompliment: AnsiString;
var
  str: AnsiString;
  i,n: integer;
begin
  str := SeqData;
  n := Length(SeqData);
  for i := 1 to n do
    case upcase(SeqData[i]) of
      'A': str[n-i+1] := 'T';
      'T': str[n-i+1] := 'A';
      'U': str[n-i+1] := 'A';
      'C': str[n-i+1] := 'G';
      'G': str[n-i+1] := 'C';
      'R': str[n-i+1] := 'Y';
      'Y': str[n-i+1] := 'R';
      'S': str[n-i+1] := 'S';
      'W': str[n-i+1] := 'W';
      'K': str[n-i+1] := 'M';
      'M': str[n-i+1] := 'K';
      'B': str[n-i+1] := 'V';
      'V': str[n-i+1] := 'B';
      'D': str[n-i+1] := 'H';
      'H': str[n-i+1] := 'D';
      'N': str[n-i+1] := 'N';
      '-': str[n-i+1] := '-';
    else
      str[n-i+1] := '?';
    end;
  result := str;
end;

constructor TSequenceList.Create;
begin
  inherited;

  FIsDNA           := true;
  FIsProteinCoding := true;
end;

destructor TSequenceList.Destroy;
begin
  Clear;
  inherited;
end;

function TSequenceList.GetItems(index: integer):TSequence;
begin
  Result := nil;
  if (Index < 0) or (Index >= Count) then
    Exit;
  result := TSequence(inherited Items[index]);
end;

procedure TSequenceList.SetItems(index: integer; value: TSequence);
begin
  inherited Items[index] := value;
  if Assigned(value) then
    value.Index := index;
end;

procedure TSequenceList.Clear;
var
  i: integer;
begin
  if Count = 0 then exit;
  for i := Count-1 downto 0 do
    Items[i].Free;
  inherited;
end;

procedure TSequenceList.RelinquishSequences;
begin
  inherited Clear;
end;

procedure TSequenceList.DeleteAll;
var
  i: Integer;
begin
  if Count > 0 then
    for i := Count - 1 downto 0 do
      inherited Delete(i);
end;

function TSequenceList.GetMaxNoOfSites:integer;
var
  i : integer;
begin
  result := 0;
  if Count > 0 then
    for i := 0 to Count-1 do
      if Length(Items[i].FSeqData) > result then
        result := Length(Items[i].FSeqData);
end;

function TSequenceList.GetMinNoOfSites:integer;
var
  i : integer;
begin
  result := MaxNoOfSites;
  if Count > 0 then
    for i := 0 to Count-1 do
      if Length(Items[i].FSeqData) < result then
        result := Length(Items[i].FSeqData);
end;

function TSequenceList.FormatLabelForMegaFile(aLabel: String): String;
begin
  Result := aLabel;
  if (Result[1] = #39) and (Result[Length(Result)] = #39) then
    Result := copy(Result, 2, Length(Result) - 2);
  Result := StringReplace(Result, ' ', '_', [rfReplaceAll]);
end;

procedure TSequenceList.FormatSequenceBlock(const aSequence: String; var aList: TStringList);
const
  NUM_SPACES = 7;
var
  tempStr: String;
  i: Integer;
  numChars: Integer;
  charsPerLine: Integer = -1;
  segmentLength: Integer = 0;
begin
  charsPerLine := SITES_PER_LINE + NUM_SPACES;
  aList.Clear;
  if Length(aSequence) > 0 then
  begin
    i := 1;
    numChars := 0;
    tempStr := EmptyStr;
    SetLength(tempStr, charsPerLine);
    while i <= Length(aSequence) do
    begin
      while (numChars < charsPerLine) and (i <= Length(aSequence)) do
      begin
        tempStr[numChars + 1] := aSequence[i];
        inc(i);
        inc(numChars);
        inc(segmentLength);
        if (segmentLength = SITES_PER_SEGMENT) and ((numChars + 1) <= High(tempStr)) then
        begin
          tempStr[numChars + 1] := ' ';
          inc(numChars);
          segmentLength := 0;
        end;
      end;
      segmentLength := 0;
      SetLength(tempStr, numChars);
      aList.Add(tempStr);
      numChars := 0;
      if Length(tempStr) <> charsPerLine then
        SetLength(tempStr, charsPerLine);
    end;
  end;
end;

function TSequenceList.DataTypeString: String;
begin
  if IsProteinCoding or IsDNA then
    Result := 'dna'
  else
    Result := 'protein';
end;

function TSequenceList.HasAtLeast2SeqsWithSitesWithBases: Boolean;
var
  i,j: Integer;
  NumSeqsWithBases: Integer;
begin
  NumSeqsWithBases := 0;
  Result := False;
  if Count > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      if Length(Items[i].SeqData) > 0 then
        for j := 1 to Length((Items[i]).SeqData) do
          if Items[i].SeqData[j] <> '-' then
          begin
            inc(NumSeqsWithBases);
            break;
          end;
      if NumSeqsWithBases >= 2 then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TSequenceList.HasGroupNames: Boolean;
var
  i: Integer;
begin
  Result := False;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if Assigned(Items[i]) and (Items[i].GroupName <> EmptyStr) then
      begin
        Result := True;
        break;
      end;
end;

function TSequenceList.IndexOfSeqName(aName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if Items[i].SeqName = aName then
      begin
        Result := i;
        Exit;
      end;
end;

procedure TSequenceList.LengthenShortSeqs(missingCharSym: Char);
var
  i: Integer;
  maxLen: Integer;
begin
  maxLen := LongestSeqLength;
  if Count > 0 then
    for i := 0 to Count - 1 do
      while Length(Items[i].SeqData) < maxLen do
        Items[i].SeqData := Items[i].SeqData + missingCharSym;
end;

function TSequenceList.LongestSeqLength: Integer;
var
  i: Integer;
begin
  Result := 0;
  if Count > 0 then
    Result := Length(Items[0].SeqData);
  if Count > 1 then
    for i := 1 to Count - 1 do
      Result := Max(Length(Items[i].SeqData), Result);
end;

function TSequenceList.HasDuplicateSeqNames: Boolean;
var
  i: Integer = -1;
  aList: TStringList = nil;
  seq: TSequence = nil;
begin
  Result := False;
  if Count = 0 then
    Exit;

  try
    aList := TStringList.Create;
    aList.Sorted := True;
    aList.Duplicates := dupIgnore;
    for i := 0 to Count - 1 do
    begin
      seq := Items[i];
      aList.Add(seq.SeqName)
    end;
    if aList.Count < Count then
    begin
      Assert(False, Format('seq list has %d items but only %d unique names', [Count, aList.Count]));
      Result := True;
    end
    else
      Result := False;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TSequenceList.GetMegaSequenceAlignmentFileStrings(Description: String; aGapSymbol, aIdenticalSymbol, aMissingSymbol: Char): TStringList;
var
  i: Integer;
  formattedStrings: TStringList;
  aLabel: String;
begin
  Assert(not HasDuplicateSeqNames, 'invalid TSequenceList - has duplicate names');
  formattedStrings := nil;
  Result := TStringList.Create;
  try
    formattedStrings := TStringList.Create;

    Result.Add('#MEGA');
    Result.Add(Format('!TITLE %s;', [Title]));
    Result.Add(EmptyStr);
    Result.Add('!Format');
    Result.Add(Format('   NSeqs=%d NSites=%d', [Count, Length(Items[0].SeqData)]));
    Result.Add(Format('   DataType=%s Indel=%s Identical=%s Missing=%s;', [DataTypeString, aGapSymbol, aIdenticalSymbol, aMissingSymbol]));
    Result.Add(Format('!DESCRIPTION - %s;', [Description]));
    for i := 0 to Count - 1 do
    begin
      Result.Add(EmptyStr);
      aLabel := FormatLabelForMegaFile(Items[i].SeqName);
      Result.Add(Format('#%s', [aLabel]));
      FormatSequenceBlock(Items[i].SeqData, formattedStrings);
      Result.AddStrings(formattedStrings);
      if Assigned(CheckCancel) then
      begin
        FProgress := Round(i/Count*100);
        CheckCancel(FProgress, 'Assembling alignment...');
      end;
    end;
  finally
    if Assigned(formattedStrings) then
      formattedStrings.Free;
  end;
end;

procedure TSequenceList.Assign(Source: TSequenceList);
var
  i: integer;
  seq: TSequence;
begin
  if Source.Count = 0 then exit;
  Clear;
  for i := 0 to Source.Count-1 do
  begin
    seq := TSequence.Create;
    seq.Assign(Source[i]);
    Add(seq);
  end;
  CheckCancel := Source.CheckCancel;
  Title := Source.Title;
  IsDNA := Source.IsDNA;
  IsProteinCoding := Source.IsProteinCoding;
  if IsProteinCoding then
    CodeTable := Source.CodeTable;
end;

procedure TSequenceList.AppendMissingSeq(aSeq: TSequence; missingCharSym: Char; NumSites: Integer);
var
  newSeq: TSequence;
  missingData: String = '';
begin
  if (Count > 0) and (NumSites > 0) then
    missingData := StringOfChar(missingCharSym, NumSites);
  newSeq := TSequence.Create;
  newSeq.Assign(aSeq);
  newSeq.SeqData := missingData + newSeq.SeqData;
  newSeq.Index := Count;
  inherited Add(newSeq);
end;

function TSequenceList.AppendSequences(aList: TSequenceList; missingCharSym: Char = '?'; DoStrict: Boolean = True): Boolean;
var
  i, Index: Integer;
  aSeq: TSequence = nil;
  aLabel: String = '';
  origNumSites: Integer = -1;
begin
  Result := False;
  if (not (Count = aList.Count)) and DoStrict then
    Exit;

  if Count > 0 then
  begin
    origNumSites := Items[0].NoOfSites;
    if DoStrict then
    begin
      for i := 0 to aList.Count - 1 do
      begin
        aSeq := aList[i];
        aLabel := aSeq.SeqName;
        if (i >= Count) or (not (aLabel = items[i].SeqName)) then
          Exit;
        items[i].SeqData := items[i].SeqData + aSeq.SeqData;
      end;
    end
    else
    begin
      for i := 0 to aList.Count - 1 do
      begin
        aSeq := aList[i];
        aLabel := aSeq.SeqName;
        Index := IndexOfSeqName(aLabel);
        if Index >= 0 then
          Items[Index].SeqData := Items[Index].SeqData + aSeq.SeqData
        else
          AppendMissingSeq(aSeq, missingCharSym, origNumSites);
      end;
    end;

    if not DoStrict then
      LengthenShortSeqs(missingCharSym);
  end
  else
  begin
    for i := 0 to aList.Count - 1 do
      AppendMissingSeq(aList[i], missingCharSym, 0);
  end;
  Result := True;
end;

procedure TSequenceList.AddSequences(aList: TSequenceList);
var
  aSeq: TSequence;
  i: Integer;
begin
  if aList.Count > 0 then
    for i := 0 to aList.Count - 1 do
    begin
      aSeq := TSequence.Create;
      aSeq.Assign(aList[i]);
      inherited Add(aSeq);
    end;
end;

procedure TSequenceList.Pack;
var
  i,j: integer;
  flag: boolean;
begin
  for j := MaxNoOfSites downto 1 do
  begin
    flag := false;
    for i := 0 to Count-1 do
    begin
      if (j <= Items[i].NoOfSites) and (Items[i].FSeqData[j] <> '-') then
      begin
        flag := true;
        break;
      end;
    end;
    if not flag then
      for i := 0 to Count-1 do
        if j <= Items[i].NoOfSites then
          System.Delete(Items[i].FSeqData, j, 1);
  end;
end;

procedure TSequenceList.Polish;
var
  i,j: integer;
begin
  if Count = 0 then exit;
  for i := 0 to Count-1 do
  begin
    j := Length(Items[i].FSeqData);
    while (j > 0) and (Items[i].FSeqData[j] = '-') do
      Dec(j);
    if j < Length(Items[i].FSeqData) then
      System.Delete(Items[i].FSeqData, j+1, Length(Items[i].FSeqData)-j);
  end;
end;


procedure TSequenceList.Translate;
var
  i: integer;
begin
  if not IsDNA then
    exit;

  for i := 0 to Count-1 do
  begin
    Items[i].CodeTable := CodeTable;
    Items[i].Translate;
  end;

  IsDNA := false;
end;

procedure TSequenceList.UnTranslate;
var
  i: integer;
  DnaStringBuilder: TMegaStringBuilder = nil;
begin
  if IsDNA  then
    exit;
  try
    DnaStringBuilder := TMegaStringBuilder.Create;

    for i := 0 to Count-1 do
    begin
      DnaStringBuilder.Clean;
      Items[i].UnTranslate(DnaStringBuilder);
    end;
    IsDNA := True;
  finally
    if Assigned(DnaStringBuilder) then
      DnaStringBuilder.Free;
  end;
end;

{ Get a string list where names and seq data comprise alternate strings}
function TSequenceList.ToStringList: TStringList;
var
  i: Integer;
  MyStringList: TStringList;
  MySequence: TSequence;
  MySeqData: AnsiString;
  MySeqName: AnsiString;
begin
  MyStringList := TStringList.Create;
  MySequence := TSequence.Create;
  for i := 0 to Count - 1 do
  begin
    MySequence := Items[i];
    MySeqData := MySequence.SeqData;
    MySeqName := MySequence.SeqName;
    MyStringList.Add(MySeqName);
    MyStringList.Add(MySeqData);
  end;

  Result := MyStringList;
  FreeAndNil(MySequence);
end;

function TSequenceList.LooksLikeDna: Boolean;
var
  numNucleotides: Integer = 0;
  numAminoAcids: Integer = 0;
  i, j: Integer;
  seq: TSequence = nil;
begin
  numNucleotides := 0;
  numAminoAcids := 0;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      seq := GetItems(i);
      for j := 1 to seq.NoOfSites do
        if Upcase(seq.Base[j]) in DNAbases then
          inc(numNucleotides)
        else if Upcase(seq.Base[j]) in AABases then
          inc(numAminoAcids);
    end;
  Result := ((numNucleotides > 27*numAminoAcids) or (numAminoAcids = 0));
end;

procedure TSequence.Translate;
var
  j: integer;
  codon: array[0..2] of AnsiChar;
begin
    SetLength(FSeqDataTranslated, NoOfSites div 3);
    for j := 0 to (NoOfSites div 3)-1 do
    begin
      codon[0] := SeqData[j*3+1];
      codon[1] := SeqData[j*3+2];
      codon[2] := SeqData[j*3+3];
     FSeqDataTranslated[j+1] := GetAA(codon);
    end;
    //System.Delete(self[i].FSeqData, (self[i].NoOfSites div 3)+1, self[i].NoOfSites);
    FisTranslated := True;
end;

procedure TSequence.UnTranslate(DnaStringBuilder: TMegaStringBuilder);
var
  j: integer = 0;
  seq: AnsiString = '';
  CurrentCodon: AnsiString = '';
  CurrentCodonIndex: Integer = 0;
begin
    { Developer warning: Updates to this function should also be considered
      for AlignGrid.Untranslate, as it performs essentially the same function. }
    if DnaStringBuilder.TotalLength > 0 then
      DnaStringBuilder.Clean;
    seq := FSeqData;
    CurrentCodonIndex := 1;

    for j := 1 to Length(FSeqDataTranslated) do
    begin
      if (FSeqDataTranslated[j] = '-') then
      begin
           DNAStringBuilder.Add('---');

           // necessary to handle translated sequence having a - at the end
           CurrentCodon := System.Copy(seq, CurrentCodonIndex, 3);
           if (CurrentCodon = '---') then
             inc(CurrentCodonIndex, 3);
        end
      else
      begin
          CurrentCodon := System.Copy(seq, CurrentCodonIndex, 3);
          while (CurrentCodon = '---') do
          begin
            inc(CurrentCodonIndex, 3);
            CurrentCodon := System.Copy(seq, CurrentCodonIndex, 3);
          end;
          DNAStringBuilder.Add(CurrentCodon);
          inc(CurrentCodonIndex, 3);
        end;
    end;

    // appends remaining DNA sites omitted from the initial translation
    while (Length(seq) > CurrentCodonIndex) do
    begin
      CurrentCodon := System.Copy(seq, CurrentCodonIndex, 3);
      if (CurrentCodon <> '---') then
         DNAStringBuilder.Add(System.Copy(seq, CurrentCodonIndex, 3));
      inc(CurrentCodonIndex, 3);
    end;

    FSeqData := DNAStringBuilder.GenerateString;
    FisTranslated := False;
end;

function TSequence.GetAA(Codon: array of AnsiChar):AnsiChar;
var
  x, y, z: LongInt;
begin

  x := -1; y := -1; z := -1;
  case Upcase(Codon[0]) of
    'U', 'T':
      x:=0;
    'C':
      x:=1;
    'A':
      x:=2;
    'G':
      x:=3;
  end;

  case Upcase(Codon[1]) of
    'U', 'T':
      y:=0;
    'C':
      y:=1;
    'A':
      y:=2;
    'G':
      y:=3;
  end;

  case Upcase(Codon[2]) of
    'U', 'T':
      z:=0;
    'C':
      z:=1;
    'A':
      z:=2;
    'G':
      z:=3;
  end;

  if (codon[0]='-') and (codon[1]='-') and (codon[2]='-') then
    Result := '-'
  else if (x < 0) or (y < 0) or (z < 0) then
    Result := '?'
  else
    Result := CodeTable[x*16+y*4+z+1];
end;



function TSequence.GetSeqData: AnsiString;
begin
  if FisTranslated then
    Result := FSeqDataTranslated
  else
    Result := FSeqData;

end;

procedure TSequence.SetSeqData(Sequence: AnsiString);
begin
 if FisTranslated then
    FSeqDataTranslated := Sequence
  else
    FSeqData := Sequence;
end;


function TSequence.Reverse(x0: Integer; x1: Integer): AnsiString;
var
  str, SelectionData: AnsiString;
  i,n: integer;
begin
  SelectionData := Copy(SeqData, x0, x1-x0+1);
  str := SelectionData;
  n := Length(SelectionData);
  for i := 1 to n do
    str[n-i+1] := upcase(SelectionData[i]);
  result := str;
end;

function TSequence.Compliment(x0: Integer; x1: Integer): AnsiString;
var
  str, SelectionData: AnsiString;
  i,n: integer;
begin
  SelectionData := Copy(SeqData, x0, x1-x0+1);
  str := SelectionData;
  n := Length(SelectionData);
  for i := 1 to n do
    case upcase(SelectionData[i]) of
      'A': str[i] := 'T';
      'T': str[i] := 'A';
      'U': str[i] := 'A';
      'C': str[i] := 'G';
      'G': str[i] := 'C';
      'R': str[i] := 'Y';
      'Y': str[i] := 'R';
      'S': str[i] := 'S';
      'W': str[i] := 'W';
      'K': str[i] := 'M';
      'M': str[i] := 'K';
      'B': str[i] := 'V';
      'V': str[i] := 'B';
      'D': str[i] := 'H';
      'H': str[i] := 'D';
      'N': str[i] := 'N';
      '-': str[i] := '-';
    else
      str[i] := '?';
    end;
  result := str;
end;

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//                         stuff for sequence factory
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

function TSequenceFactory.ValidateFastaSequenceId(RawSeqId: String): boolean;
begin
  Result := false; // guilty until proven innocent!
  if Pos('>', RawSeqId) = 0 then
  Exit;
  // other checks added here as needed

  Result := true;
end;


function TSequenceFactory.ValidateSequenceData(RawSeqData: String): boolean;
var
  i: Integer;
  IsValidChar: boolean;
const
  // list of valid characters retrieved from http://blast.ncbi.nlm.nih.gov/blastcgihelp.shtml
  ValidCharacters = ['A','B','C','D','E','F','G','H','I','K','L',
  	'M', 'N','P','Q','R','S','T','U','V','W','Y','Z','X','-','*',
    'a','b','c','d','e','f','g','h','i','k','l','m','n','p','q',
    'r','s','t','u','v','w','y','z','x'];
begin
  Assert(Length(RawSeqData) > 0); // must always be checked before calling this function
  Result := false;
  for i := 1 to Length(RawSeqData) do
  begin
    IsValidChar := (RawSeqData[i] in ValidCharacters);
    if not IsValidChar then
      Exit; // no need to continue if we already have found a problem
  end;
  Result := true; // then no problems were found
end;

function TSequenceFactory.ReverseComplement(Source: AnsiString): AnsiString;
var
  i,n: integer;
begin
  result := '';
  n := length(source);
  setlength(result, n);
  for i := n downto 1 do
    case upcase(source[i]) of
      'A': result[n-i+1] := 'T';
      'G': result[n-i+1] := 'C';
      'T': result[n-i+1] := 'A';
      'C': result[n-i+1] := 'G';
      'R': result[n-i+1] := 'Y';
      'Y': result[n-i+1] := 'R';
      'M': result[n-i+1] := 'K';
      'K': result[n-i+1] := 'M';
    else
      result[n-i+1] := upcase(source[i])
    end;
end;

function TSequenceFactory.ExtractAccession(Source: AnsiString): AnsiString;
begin
  Result := trim(Copy(Source, 11, length(Source) - 9));
end;

function TSequenceFactory.ExtractVersion(Source: AnsiString): AnsiString;
var
  s: Integer;
begin
  Result := '';
  s := Pos('GI:', Uppercase(Source)) + 3;
  while (s <= length(Source)) and (Source[s] in ['0'..'9']) do
  begin
    Result := Result + Source[s];
    inc(s);
  end;
end;

function TSequenceFactory.ExtractDefinition(var aSeq: TSequence): Boolean;
var
  TempString: AnsiString = '';
  line: Integer = -1;
begin
  Assert(Assigned(FRawData) and (FRawData.Count > 0));
  Result := False;
  line := FindDefinitionLine;
  if line < 0 then
    Exit;
  TempString := trim(Copy(FRawData[Line], 11, length(FRawData[Line]) - 10));
  aSeq.SeqName := TempString;
  while line < FRawData.Count do
  begin
    if IsBoundary(FRawData[line]) then
      Break;
    TempString := trim(FRawData[line]);
    aSeq.SeqName := aSeq.SeqName + ' ' + TempString;
    inc(line);
  end;
  Result := (Trim(aSeq.SeqName) <> EmptyStr);
end;

function TSequenceFactory.ExtractFeatures(var aSeq: TSequence): Boolean;
var
  k: integer;
  TempString: AnsiString = '';
  Line: Integer = -1;
begin
  Assert(Assigned(FRawData) and (FRawData.Count > 0));
  Result := False;
  Line := FindFeaturesLine;
  if Line < 0 then
    Exit;
  inc(Line);
  aSeq.Species  := '';
  aSeq.Strain   := '';
  aSeq.Host     := '';
  aSeq.Senotype := '';
  aSeq.Gene     := '';
  aSeq.Allele   := '';

  FNumCDSs := 0;
  while Line < FRawData.Count do
  begin
    if BeginsWith(FRawData[Line], 'ORIGIN') then
       Break;
    if Pos('source', FRawData[Line]) > 0 then
    begin
      while ((Line < FRawData.Count) and (not(BeginsWith(FRawData[Line], 'ORIGIN')))) do
      begin
        inc(Line);
        if (Pos('gene', FRawData[Line]) > 0) or (Pos('CDS', FRawData[Line]) > 0) then
          break;
        if Pos('/organism=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/organism=', FRawData[Line])+11, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Species := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Species := TempString;
        end
        else if Pos('/sub_species=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/sub_species=', FRawData[Line])+14, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Subspecies := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Subspecies := TempString;
        end
        else if Pos('/cultivar=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/cultivar=', FRawData[Line])+11, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Subspecies := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Subspecies := TempString;
        end
        else if Pos('/variety=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/variety=', FRawData[Line])+10, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Subspecies := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Subspecies := TempString;
        end
        else if Pos('/serovar=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/serovar=', FRawData[Line])+10, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Subspecies := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Subspecies := TempString;
        end
        else if Pos('/strain=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/strain=', FRawData[Line])+9, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Strain := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Strain := TempString;
        end
        else if Pos('/ecotype=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/ecotype=', FRawData[Line])+10, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Strain := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Strain := TempString;
        end
        else if Pos('/pop_variant=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/pop_variant=', FRawData[Line])+14, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Strain := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Strain := TempString;
        end
        else if Pos('/specimen_voucher=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/specimen_voucher=', FRawData[Line])+19, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Strain := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Strain := TempString;
        end
        else if (aSeq.Strain = '') and (Pos('/isolate=', FRawData[Line]) > 0) then
        begin
          TempString := Copy(FRawData[Line], Pos('/isolate=', FRawData[Line])+10, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Strain := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Strain := TempString;
        end
        else if Pos('/host=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/host=', FRawData[Line])+7, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Host := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Host := TempString;
        end
        else if Pos('/serotype=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/serotype=', FRawData[Line])+11, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Senotype := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Senotype := TempString;
        end;
      end;
    end
    else if Pos('gene', FRawData[Line]) > 0 then
    begin
      while (not(Pos('CDS',FRawData[Line]) > 0)) and (not(BeginsWith(FRawData[Line], 'ORIGIN'))) and (Line < FRawData.Count) do
      begin
        inc(Line);
        if Pos('/gene=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/gene=', FRawData[Line])+7, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            TempString := Copy(TempString, 1, Pos('"', TempString)-1);
          if aSeq.Gene = '' then
            aSeq.Gene := TempString
          else if TempString <> Copy(aSeq.Gene, 1, length(TempString)) then
            aSeq.Gene := aSeq.Gene +', ' +TempString;
        end
        else if (Pos('/locus_tag', FRawData[Line]) > 0) and (aSeq.Gene = '') then
        begin
          TempString := Copy(FRawData[Line], Pos('/locus_tag', FRawData[Line])+12, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Gene := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Gene := TempString;
        end
        else if (Pos('/product', FRawData[Line]) > 0) and (aSeq.Gene = '') then
        begin
          TempString := Copy(FRawData[Line], Pos('/product', FRawData[Line])+10, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Gene := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Gene := TempString;
        end
        else if Pos('/allele=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/allele=', FRawData[Line])+9, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Allele := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Allele := TempString;
        end
        else if Pos('/haplotype=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/haplotype=', FRawData[Line])+12, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            aSeq.Allele := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            aSeq.Allele := TempString;
        end;
      end;
    end
    else if Pos('CDS ', Trim(FRawData[Line])) > 0 then
    begin
      if FNumCDSs div 100 = 0 then
        setlength(FCDSs, length(FCDSs)+100);
      FCDSs[FNumCDSs].n := 0;
      if Pos('complement', FRawData[Line]) > 0 then
        FCDSs[FNumCDSs].complement := true
      else
        FCDSs[FNumCDSs].complement := false;
      k := Pos('..', FRawData[Line]);
      dec(k);
      while (k > 0) and (FRawData[Line][k] in ['0'..'9']) do
        dec(k);
      inc(k);
      TempString := Copy(FRawData[Line], k, length(FRawData[Line]));
      inc(Line);
      while not(Pos('/', FRawData[Line]) > 0) do
      begin
        TempString := TempString +Trim(Copy(FRawData[Line], 1, length(FRawData[Line])));
        inc(Line);
      end;
      while Pos('..', TempString) > 0 do
      begin
        k := Pos('..', TempString);
        if TempString[1] in ['0'..'9'] then
          FCDSs[FNumCDSs].join[FCDSs[FNumCDSs].n].x := StrToInt(Copy(TempString, 1, k-1))
        else
          FCDSs[FNumCDSs].join[FCDSs[FNumCDSs].n].x := 1;
        while (k <= length(TempString)) and not(TempString[k] in ['0'..'9']) do
          inc(k);
        Delete(TempString, 1, k-1);
        k := 1;
        while (k <= length(TempString)) and (TempString[k] in ['0'..'9']) do
          inc(k);
        if k > 1 then
          FCDSs[FNumCDSs].join[FCDSs[FNumCDSs].n].y := StrToInt(Copy(TempString, 1, k-1))
        else
          FCDSs[FNumCDSs].join[FCDSs[FNumCDSs].n].y := length(TempString);
        Delete(TempString, 1, k);
        k := 1;
        while (k <= length(TempString)) and not(TempString[k] in ['0'..'9']) do
          inc(k);
        Delete(TempString, 1, k-1);
        inc(FCDSs[FNumCDSs].n);
      end;
      while (Line < FRawData.Count) and (Pos('/', FRawData[Line]) > 0) do
      begin
        TempString := FRawData[Line];
         if Pos('/gene=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/gene=', FRawData[Line])+7, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            FCDSs[FNumCDSs].gene := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            FCDSs[FNumCDSs].gene := TempString;
        end
        else if Pos('/allele=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/allele=', FRawData[Line])+9, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            FCDSs[FNumCDSs].allele := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            FCDSs[FNumCDSs].allele := TempString;
        end
        else if Pos('/haplotype=', FRawData[Line]) > 0 then
        begin
          TempString := Copy(FRawData[Line], Pos('/haplotype=', FRawData[Line])+12, length(FRawData[Line]));
          if Pos('"', TempString) > 0 then
            FCDSs[FNumCDSs].allele := Copy(TempString, 1, Pos('"', TempString)-1)
          else
            FCDSs[FNumCDSs].allele := TempString;
        end;
        inc(Line);
      end;
      inc(FNumCDSs);
    end
    else
      inc(Line);
  end;
  Result := True;
end;

function TSequenceFactory.ExtractSequenceData(var aSeq: TSequence): Boolean;
var
  j, k: Integer;
  tmpstr: string = '';
  TempString: AnsiString = '';
  Line: Integer = -1;
begin
  Result := False;
  Line := FindOrigin;

  inc(Line);
  while (Line < FRawData.Count) and (Copy(FRawData[Line], 1, 2) <> '//') do
  begin
    SetLength(TempString, Length(FRawData[Line]));
    tmpstr := trim(FRawData[Line]);
    k := 0;
    if Pos('[gap ', FRawData[Line]) > 0 then
    begin
      tmpstr := '';
      j := Pos('[gap ', FRawData[Line]) + 5;
      while (FRawData[Line][j] in ['0'..'9']) and (j <= Length(FRawData[Line])) do
      begin
        tmpstr := tmpstr +FRawData[Line][j];
        inc(j);
      end;
      k := StrToInt(tmpstr);
      TempString := StringOfChar('N', k);
    end
    else
      for j := 1 to Length(FRawData[Line]) do
        if not (FRawData[Line][j] in ['0'..'9', ' ']) then
        begin
          inc(k);
          TempString[k] := FRawData[Line][j];
        end;

    SetLength(TempString, k);
    aSeq.SeqData := aSeq.SeqData + uppercase(TempString);
    inc(Line);
  end;
  Result := (Trim(aSeq.SeqData) <> EmptyStr);
end;

function TSequenceFactory.FindOrigin: Integer;
var
  i: Integer;
begin
  Result := -1;
  if Assigned(FRawData) and (FRawData.Count > 0) then
  begin
    for i := 0 to FRawData.Count - 1 do
      if Copy(FRawData[i], 1, 6) = 'ORIGIN' then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

function TSequenceFactory.ExtractCDSs(const aCDS: TCDSArray; var aSeq: TSequence): Boolean;
var
  j, k: Integer;
  tmpstr: string = '';
  TempString: AnsiString = '';
  cdsCount: Integer;
  line: Integer = 0;
begin
  Result := False;
  Assert(Assigned(FRawData) and (FRawData.Count > 0));
  cdsCount := Length(aCDS);
  line := FindOrigin;
  if line < 0 then
    raise Exception.Create('failed to find sequence data ORIGIN in Genbank result');
  inc(line);
  while (line < FRawData.Count) and (Copy(FRawData[line], 1, 2) <> '//') do
  begin
    SetLength(TempString, Length(FRawData[line]));
    tmpstr := trim(FRawData[line]);
    k := 0;
    if Pos('[gap ', FRawData[line]) > 0 then
    begin
      tmpstr := '';
      j := Pos('[gap ', FRawData[line]) + 5;
      while (FRawData[line][j] in ['0'..'9']) and (j <= Length(FRawData[line])) do
      begin
        tmpstr := tmpstr +FRawData[line][j];
        inc(j);
      end;
      k := StrToInt(tmpstr);
      TempString := StringOfChar('N', k);
    end
    else
      for j := 1 to Length(FRawData[line]) do
        if not (FRawData[line][j] in ['0'..'9', ' ']) then
        begin
          inc(k);
          TempString[k] := FRawData[line][j];
        end;

    SetLength(TempString, k);
    aSeq.SeqData := aSeq.SeqData + uppercase(TempString);
    inc(line);
  end;

  TempString := aSeq.SeqData;

  if (cdsCount > 0) then
    if cdsCount = 1 then
    begin
      TempString := '';
      for j := 0 to cdsCount-1 do
      begin
        tmpstr := '';
        for k := 0 to aCDS[j].n-1 do
          tmpstr := tmpstr +Copy(aSeq.SeqData, aCDS[j].join[k].x, aCDS[j].join[k].y-aCDS[j].join[k].x+1);
        if aCDS[j].complement then
          tmpstr := ReverseComplement(tmpstr);
        TempString := TempString +tmpstr;
      end;
      if length(TempString) < length(aSeq.SeqData) then
      begin
        aSeq.Gene    := aCDS[j].gene;
        aSeq.Allele  := aCDS[j].allele;
        aSeq.SeqData := TempString;
      end;
    end
    else
    begin
      aSeq.Allele := '';
      aSeq.Gene   := '';
      TempString  := '';
      for j := 0 to cdsCount - 1 do
      begin
        tmpstr := '';
        for k := 0 to aCDS[j].n-1 do
          tmpstr := tmpstr +Copy(aSeq.SeqData, aCDS[j].join[k].x, aCDS[j].join[k].y-aCDS[j].join[k].x+1);
        if aCDS[j].complement then
          tmpstr := ReverseComplement(tmpstr);
        if TempString = '' then
          TempString := tmpstr
        else
          TempString := TempString +'---' +tmpstr;
        if aSeq.Gene = '' then
          aSeq.Gene := aCDS[j].gene
        else
          aSeq.Gene := aSeq.Gene +', ' +aCDS[j].gene;
      end;
      aSeq.SeqData := TempString;
    end;
    Result := (Trim(aSeq.SeqData) <> EmptyStr);
end;

function TSequenceFactory.ExtractSequenceData(const GenbankResult: TStringList; var aSeq: TSequence): Integer;
var
  j, k: Integer;
  tmpstr: string = '';
  TempString: AnsiString = '';
  line: Integer;
begin
  FRawData := GenbankResult;
  line := FindOrigin;
  Result := Line;
  inc(Result);
  while (Result < GenbankResult.Count) and (Copy(GenbankResult[Result], 1, 2) <> '//') do
  begin
    SetLength(TempString, Length(GenbankResult[Result]));
    tmpstr := trim(GenbankResult[Result]);
    k := 0;
    if Pos('[gap ', GenbankResult[Result]) > 0 then
    begin
      tmpstr := '';
      j := Pos('[gap ', GenbankResult[Result]) + 5;
      while (GenbankResult[Result][j] in ['0'..'9']) and (j <= Length(GenbankResult[Result])) do
      begin
        tmpstr := tmpstr +GenbankResult[Result][j];
        inc(j);
      end;
      k := StrToInt(tmpstr);
      TempString := StringOfChar('N', k);
    end
    else
      for j := 1 to Length(GenbankResult[Result]) do
        if not (GenbankResult[Result][j] in ['0'..'9', ' ']) then
        begin
          inc(k);
          TempString[k] := GenbankResult[Result][j];
        end;

    SetLength(TempString, k);
    aSeq.SeqData := aSeq.SeqData + uppercase(TempString);
    inc(Result);
  end;
end;

function TSequenceFactory.ExtractAllGenbankData(const GenbankResult: TStringList; const aCDS: TCDSArray; var aSeq: TSequence): Boolean;
var
  accession: String;
begin
  FRawData := GenbankResult;
  Result := True;
  accession := FindGenbankAccessionNum(GenbankResult);
  if Trim(accession) <> EmptyStr then
    aSeq.AccessionNum := accession;
  Result := Result and ExtractDefinition(aSeq);
  Result := Result and ExtractFeatures(aSeq);
  Result := Result and ExtractCDSs(aCDS, aSeq);
end;

function TSequenceFactory.ExtractAllGenbankData(const GenbankResult: TStringList; var aSeq: TSequence): Boolean;
var
  accession: String;
begin
  FRawData := GenbankResult;
  Result := True;
  accession := FindGenbankAccessionNum(GenbankResult);
  if Trim(accession) <> EmptyStr then
    aSeq.AccessionNum := accession;
  Result := Result and ExtractDefinition(aSeq);
  Result := Result and ExtractFeatures(aSeq);
  Result := Result and ExtractSequenceData(aSeq);
end;

function TSequenceFactory.IsBoundary(Source: AnsiString): Boolean;
begin
  if ( (BeginsWith(Source, 'ACCESSION')) or
       (BeginsWith(Source, 'VERSION')) or
       (BeginsWith(Source, 'KEYWORDS')) or
       (BeginsWith(Source, 'SOURCE')) or
       (BeginsWith(Source, 'ORGANISM')) or
       (BeginsWith(Source, 'AUTHORS')) or
       (BeginsWith(Source, 'TITLE')) or
       (BeginsWith(Source, 'JOURNAL')) or
       (BeginsWith(Source, 'PUBMED')) or
       (BeginsWith(Source, 'REMARK')) or
       (BeginsWith(Source, 'REFERENCE')))
  then
    Result := true
  else
    Result := false;
end;

function TSequenceFactory.CheckHasCDSs(RawData: TStringList): Boolean;
var
  aSeq: TSequence = nil;
  index: Integer;
begin
  FRawData := RawData;
  Result := False;
  try
    index := FindFeaturesLine;
    if index >= 0 then
    begin
      aSeq := TSequence.Create;
      Result := ExtractFeatures(aSeq);
      Result := Result and (FNumCDSs > 0);
    end;
  finally
    if Assigned(aSeq) then
      aSeq.Free;
  end;
end;

function TSequenceFactory.GetCDSsForData(RawData: TStringList): TCDSArray;
var
  aSeq: TSequence = nil;
  index: Integer;
  i: Integer;
begin
  FRawData := RawData;
  SetLength(Result, 0);
  try
    index := FindFeaturesLine;
    if index >= 0 then
    begin
      aSeq := TSequence.Create;
      if not ExtractFeatures(aSeq) then
        Exit;
      if FNumCDSs > 0 then
      begin
        SetLength(Result, FNumCDSs);
        for i := 0 to FNumCDSs - 1 do
          CopyCds(FCDSs[i], Result[i]);
      end
      else
      begin
        SetLength(Result, 1);
        Result[0].accession := FindGenbankAccessionNum(RawData);
        Result[0].gene := FULL_SEQUENCE_STR;
        Result[0].join[0].x := 1;
        ExtractSequenceData(aSeq);
        Result[0].join[0].y := Length(aSeq.SeqData);
      end;
    end;
  finally
    if Assigned(aSeq) then
      aSeq.Free;
  end;
end;

function TSequenceFactory.FindFeaturesLine: Integer;
var
  i: Integer;
begin
  Result := -1;
  if Assigned(FRawData) and (FRawData.Count > 0) then
  begin
    for i := 0 to FRawData.Count - 1 do
      if Copy(FRawData[i], 1, 8) = 'FEATURES' then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

function TSequenceFactory.FindDefinitionLine: Integer;
var
  i: Integer;
begin
  Result := -1;
  if Assigned(FRawData) and (FRawData.Count > 0) then
  begin
    for i := 0 to FRawData.Count - 1 do
      if Copy(FRawData[i], 1, 10) = 'DEFINITION' then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

function TSequenceFactory.FindAccessionLine: Integer;
var
  i: Integer;
begin
  Result := -1;
  if Assigned(FRawData) and (FRawData.Count > 0) then
  begin
    for i := 0 to FRawData.Count - 1 do
      if Copy(FRawData[i], 1, 9) = 'ACCESSION' then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

constructor TSequenceFactory.Create;
begin
  FErrorMsg := EmptyStr;
end;

destructor TSequenceFactory.Destroy;
begin
  SetLength(FCDSs, 0);
  SetLength(tempCDSs, 0);
  inherited Destroy;
end;

function TSequenceFactory.ValidateFasta(SequenceId: String; SequenceData: String): boolean;
begin
  Result := (ValidateFastaSequenceId(SequenceId)) and (ValidateSequenceData(SequenceData));
end;

function TSequenceFactory.ValidateMessyFasta(RawData: TStringList): boolean;
var
  i: Integer;
  CurrentString: String;
  foundSeqName: Boolean;
begin
  foundSeqName := false;

  for i := 0 to RawData.Count - 1 do
  begin
    CurrentString := Trim(RawData[i]);
    if CurrentString = EmptyStr then // disregard any empty lines that may be in the raw data
      Continue;

    if BeginsWith(CurrentString, '>') then
    begin
      Result := ValidateFastaSequenceId(CurrentString);
      if not Result then
        Exit;
      // If we have found a valid > followed by a seq name, THEN check if the seq is valid.
      foundSeqName := true;
    end
    else if foundSeqName then
    begin
      Result := ValidateSequenceData(CurrentString);
      // Once we have found one line of valid sequence data, stop checking.
      // we don't know where the FASTA sequence ends and some potential footer begins.
      Exit;
    end;
  end;
end;

function RemoveExternalLink(CurrentString: String): String;
begin
  if ContainsText(CurrentString, 'External link') then
  CurrentString := EmptyStr;
  result := CurrentString;
end;

function TSequenceFactory.ValidateFasta(var RawData: TStringList): boolean;
var
  i: Integer;
  CurrentString: String;
begin
  for i := RawData.Count - 1 downto 0 do
  begin
    CurrentString := Trim(RawData[i]);
    if ContainsText(CurrentString, 'External link') then
      RawData.Delete(i);
    end;
  for i := 0 to RawData.Count - 1 do
  begin
    if CurrentString = EmptyStr then // disregard any empty lines that may be in the raw data
      Continue;

    if BeginsWith(CurrentString, '>') then
    begin
      Result := ValidateFastaSequenceId(CurrentString);
      if not Result then
        Exit;
    end
    else
    begin
      Result := ValidateSequenceData(CurrentString);
      if not Result then
        Exit;
    end;
  end;
  result := true;
end;

function TSequenceFactory.ValidateGenbank(RawData: TStringList): boolean;
var
  i: integer;
  HasDoubleSlash: boolean;
  HasOriginLabel: boolean;
begin
  HasDoubleSlash := false;
  HasOriginLabel := false;
  if RawData.Count >= 3 then
    for i := 0 to RawData.Count-1 do
      if HasOriginLabel then
      begin
        if Pos('//',  RawData[i]) > 0 then
        begin
          HasDoubleSlash := true;
          break;
        end
      end
      else if Pos('ORIGIN',  RawData[i]) > 0 then
      begin
        HasOriginLabel := true;
        continue;
      end;
  Result := HasOriginLabel and HasDoubleSlash;
end;

function TSequenceFactory.GenerateGenbankSequenceObject(GenbankResult: TStringList): TSequence;
begin
  Result := TSequence.Create;
  ExtractAllGenbankData(GenbankResult, Result);
end;

function TSequenceFactory.FindGenbankAccessionNum(GenbankData: TStringList): String;
var
  i: Integer;
begin
  Result := EmptyStr;
  if GenbankData.Count > 0 then
    for i := 0 to GenbankData.Count - 1 do
    begin
      if Copy(GenbankData[i], 1, 9) = 'ACCESSION' then
      begin
        Result := ExtractAccession(GenbankData[i]);
        Exit;
      end
    end;
end;

{ Generate a TSequence object using the given sequenceId and sequenceData.
 It is assumed that validation has already been done for both the sequenceId and
 the sequenceData.}
function TSequenceFactory.GenerateFastaSequenceObject(SequenceIdString: String; SequenceData:String): TSequence;
var
  MySequence: TSequence;
  i: Integer;
const
  ValidIdPrefixes: array[0..12] of String = ('gb','emb','pir','sp','ref','dbj','prf','tpg','tpe','tpd','tr','gpp','nat');
begin
  MySequence := TSequence.Create;
  MySequence.SeqData := Trim(SequenceData);

  MySequence.SeqName := PullOutSequenceName(SequenceIdString);
  if (MySequence.SeqName = EmptyStr) or (MySequence.SeqName = 'undefined') then
    MySequence.SeqName := Copy(SequenceIdString, 2, Length(SequenceIdString) - 1);
  if BeginsWith(SequenceIdString,'>gi') then  // first, try to find a GenInfo identifier (preferred)
  begin
    MySequence.SeqId := PullOutIdNumber(SequenceIdString, '>gi');
    MySequence.SeqIdPrefix := 'gi';
  end
  else
  begin
    MySequence.SeqIdPrefix := 'undefined';
    for i := 0 to Length(ValidIdPrefixes) - 1 do
      if BeginsWith(SequenceIdString, '>' + ValidIdPrefixes[i]) then
      begin
        MySequence.SeqId := PullOutIdNumber(SequenceIdString, '>' + ValidIdPrefixes[i]);
        MySequence.SeqIdPrefix := ValidIdPrefixes[i];
        Break;
      end;
  end;
  if MySequence.SeqIdPrefix = 'undefined' then
    MySequence.SeqId := 'undefined';
  Assert(MySequence.SeqName <> EmptyStr);
  Result := MySequence;
end;

{ Attempts to extract a unique identifier from the FASTA id string. This
  function assumes the FASTA id formats described at " http://www.ncbi.nlm.nih.gov/books/NBK7183/?rendertype=table&id=ch_demo.T5"
  This format is something like "Identifier|IdNumber|other stuff" so we split on the
  pipe character and if our Identifier is found we try to get the string immediately
  following the matched identifier. }
function TSequenceFactory.PullOutIdNumber(SequenceIdString: String; Identifier: String): String;
var
  MySplitStrings: TStringList;
  i: Integer;
begin
  Result := 'undefined';
  if Pos('|',SequenceIdString) = 0 then // then we don't know what to do with it
    Exit;

  MySplitStrings := SplitOnSingleChar(SequenceIdString, '|', true); // splits on the pipe character but retains white space

  for i := 0 to MySplitStrings.Count - 1 do
  begin
    if (MySplitStrings[i] = Identifier) and (i < MySplitStrings.Count - 1) then
    begin
      Result := MySplitStrings[i + 1];
      Break;
    end;
  end;
  FreeAndNil(MySplitStrings);
end;

{ Attemps to extract a sequence name from a FASTA id string, assuming
  that the id string has fields delimited by pipe characters and the last field
  is the sequence name. This assumption seems to hold valid for fasta sequences
  retrieved from NCBI but may not be valid for data coming from other sources.
  Therefore, if we can't extract the name, just return the full id string minus
  the '>' character at the beginning.}
function TSequenceFactory.PullOutSequenceName(SequenceIdString: String): String;
var
  MySplitStrings: TStringList;
  MyTempString: String;
begin
  MyTempString := Trim(SequenceIdString);
  Assert(MyTempString <> EmptyStr); // should have been tested upstream
  if (Pos('>', MyTempString) = 0) or (Pos('|', MyTempString) = 0) then // make sure both of these characters are in the id string
    Result := SequenceIdString; // then just give back the id string because whatever is wrong should not be handled here

  MySplitStrings := SplitOnSingleChar(MyTempString, '|', true); // split on the pipe character while preserving inner white space
  if MySplitStrings.Count < 2 then
    Result := Copy(MyTempString, 2, Length(MyTempString) - 1)
  else
    Result := MySplitStrings[MySplitStrings.Count - 1]; // then we just grab the last field

  FreeAndNil(MySplitStrings);
end;

{ Try and determine the type of data based on a small sample of bases.
  If modifying the MaxCharsToTest, look at everywhere this function is called as
  the amount of sequence data passed in is probably also limited (if you increase
  the MaxCharsToTest here but not else where, this function might never recieve
  more characters). This function assumes that SequenceData has already been
  validated and will return either snNucleotide or snProtein. If SequenceData
  is not validated before calling this function, the result may be incorrect.}
function TSequenceFactory.DetermineDataType(SequenceData: String): TSnTokenCode;
var
  i: Integer;
  NumCharsToTest: Integer;
  NumNucleotidesFound: Integer;
  TempString: String;
const
  MaxCharsToTest = 500;
  NucleotideBases = ['A','T','C','G','U','a','t','c','g','u'];
  Cutoff = 0.90; // if at least 90% of characters are nucleotides, assume it is nucleotide data; otherwise, protein data
begin
  TempString := Trim(SequenceData);
  Assert(TempString <> EmptyStr);

  if Length(TempString) < MaxCharsToTest then
    NumCharsToTest := Length(SequenceData)
  else
    NumCharsToTest := MaxCharsToTest;
  Assert(NumCharsToTest > 0);

  NumNucleotidesFound := 0;
  for i := 1 to NumCharsToTest do
    if TempString[i] in NucleotideBases then
      inc(NumNucleotidesFound)
    else if (TempString[i] = '-') or (TempString[i] = 'N') then
      dec(NumCharsToTest);  // exclude empty sites from cutoff calculation

  // default to nucleotide if sequence is empty
  if (NumCharsToTest = 0) or ((NumNucleotidesFound / NumCharsToTest) >= Cutoff) then
    Result:= snNucleotide
  else
    Result := snProtein;
end;


end.
