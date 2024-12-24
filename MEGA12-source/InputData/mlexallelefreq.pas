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

unit mlexallelefreq;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{
All programming assumes that every locus is in on nice rectangular form with
  no interleaving; In future I may allow interleaving
Features:
  if allele/genotype frequencies are > 1 then it computes proportions automatically
}
uses
  Classes, SysUtils, KeywordConsts, MLexBase, MegaConsts, MLocusInfo;

type
  // Reader for reading population geene freqwuency data
  TLexAlleleFreq = class(TLexBase)
  private
    FNoOfLoci    : LongInt;
    FLocusInfo   : TList;    // List of TLocusList classes
    FFreqInfo    : TList;    // List of Lists of Allelefreq arrays
                             //  LocusA: LocusPopFreqInfo for pop1
                             //          LocusPopFreqInfo for pop2
                             //  LocusB: LocusPopFreqInfo for pop1
                             //          LocusPopFreqInfo for pop2
    // temporary variables
    CurNoOfAlleles: Integer;
    CurNoOfIndv:     Double;
    CurLocusIndex:  Integer;      // Index of the current locus
    CurLocusInfo:   TLocusInfo;   // Currently read locus info
    CurFreqInfo:    TList;        // cur list of pops to add data for current locus

    procedure ReadFormat;  // Reads Format statement completely

    procedure ProcessCommandStatement;
    procedure ReadAlleleNames;
    procedure ReadAlleleFreqs;
    procedure ReadGenotypeFreqs;

    procedure UpdateOptions(Variable, Option: TSnTokenCode);
    procedure TokenLocusInfoVariableIs(AToken: TSnTokenCode);

  private
    function  GetLocusInfo: PArrayOfTLocusInfo;
    function  GetFreqInfo: TList;
  public
    constructor Create;
    destructor Destroy; override;

    function ReadHeader: Boolean;  // Reads full header
    function ReadData: Boolean;    // reads data assuming header already read

    property FileName;
    //property ProgressForm;
    property Title;
    property Description;
    property FoundFormat;
    property DataType;
    property DataFormat;
    property MissingSym;

    property NoOfOtus;   // No of pops
    property Taxon;      // provides name of the Pop
    property NoOfLoci:  LongInt read  FNoOfLoci;

    property LocusInfo: PArrayOfTLocusInfo read GetLocusInfo;
    property FreqInfo:  TList              read GetFreqInfo;

    property Row;
    property Col;
  end;

implementation

uses
  Graphics, Controls, Forms, Dialogs, Buttons,
  TypInfo, MegaUtils, MegaPrivateFiles;

// Now the lexical analyzer
constructor TLexAlleleFreq.Create;
begin
  inherited Create;
  FNoOfLoci  := 0;
  FFreqInfo  := nil;
  FLocusInfo := nil;

  CurNoOfAlleles    := -1;
  CurNoOfIndv        := -1;
  CurLocusIndex     := -1;
  CurLocusInfo      := nil;
  CurFreqInfo       := nil;
end;

destructor TLexAlleleFreq.Destroy;
var
  i, j: Integer;
  TempList: TList;
begin
  if FLocusInfo <> nil then
  begin
    for i:=0 to FLocusInfo.Count-1 do
       if FLocusInfo[i] <> nil then
         TLocusInfo(FLocusInfo[i]).Destroy;
    FLocusInfo.Free;
  end;
  if FFreqInfo <> nil then
  begin
    for i:=0 to FFreqInfo.Count-1 do
      if FFreqInfo[i] <> nil then
      begin
        TempList := FFreqInfo[i];
        for j := 0 to TempList.Count-1 do
          if TempList[j] <> nil then
            TLocusPopFreqInfo(TempList[j]).Destroy;
         TempList.Free;
       end;
    FFreqInfo.Free;
  end;
  inherited Destroy;
end;

function  TLexAlleleFreq.GetLocusInfo: PArrayOfTLocusInfo;
var
  i: Integer;
begin
  GetMem(Result, FLocusInfo.Count*sizeOf(TLocusInfo));
  for i:=0 to FLocusInfo.Count-1 do
    Result[i] := TLocusInfo(FLocusInfo[i]);
  FLocusInfo.Free;
  FLocusInfo := nil;
end;

function  TLexAlleleFreq.GetFreqInfo:  TList;
begin
  Result := FFreqInfo;
  FFreqInfo := nil;
end;

//---- Reads complete file header
function TLexAlleleFreq.ReadHeader: Boolean;  // only reads header
var
  keywordsFile: String;
  stringToTokenFile: String;
begin
 try
   Result := True;
   Unwind;       //--- Unwind and load keywords file

   if Keywords = nil then
   begin
     Keywords := TStringList.Create;
     keywordsFile := GetPrivateFile(mfAlleleFreqParserKeywords, True);
     Keywords.LoadFromFile(keywordsFile);
   end;

   if StringToToken = nil then
   begin
     StringToToken := TStringList.Create;
     stringToTokenFile := GetPrivateFile(mfAlleleFileStringToToken, True);
     StringToToken.LoadFromFile(stringToTokenFile);
     ProcessStringToTokenList;
   end;

   //--- Read #MEGA
   NextToken; CheckTokenSymbol('#');
   NextToken; CheckTokenSymbol('MEGA');
   //---#Title
   NextToken; CheckTokenSymbol('#');
   NextToken; CheckTokenSymbol('Title');

   ReadSpecialStringToken(#13);
   FTitle := TokenString;

   NextToken;
   if TokenSymbolIs('#') then
   begin
     NextToken;
     if TokenSymbolIs('Format') then
     begin
       FFoundFormat := True;
       ReadFormat;  // Read format
       NextToken;
       if TokenSymbolIs('#') then
       begin
         NextToken;
         if TokenSymbolIs('Description') then  // read description
         begin
           ReadSpecialStringToken(';');
           FDescription := TokenString;
           NextToken;
         end
       end;
     end
     else
     begin
       FFoundFormat := False;
       if TokenSymbolIs('Description') then  // read description
       begin
         ReadSpecialStringToken(';');
         FDescription := TokenString;
         NextToken;
       end
     end;
   end;
 except
   raise;
 end;
end;

//---- Reads format statement completely
procedure TLexAlleleFreq.ReadFormat;
var
  TokenStr : String;
  CurVariable: TSnTokenCode = snNoToken;
  CurOption: TSnTokenCode = snNoToken;
  CurVariableLink: Integer = -1;
begin
  while True do
  begin
    NextToken;
    TokenStr := TokenString;
    if TokenSymbolIs(';') or SameText(';', TokenStr) then
      Break;                  // format statement over
    //--- Read statement-by-statement
    FIsTokenPending := True;
    ReadAssignmentStatement(CurVariable, CurOption, CurVariableLink);
    TokenFormatVariableIs(CurVariable); // checks if it is valid Locus info
    if CurVariableLink > 0 then  // variable needs a valid symbol for option
      UpdateOptions(CurVariable, CurOption)
    else
      UpdateOptions(CurVariable, Token);  // the current token is important
  end;
end;

//---- reads data only; assumes header already read
function TLexAlleleFreq.ReadData: Boolean;
begin
  Result := True;
  // It reads data Locus-by-Locus;
  // So the first thing must be the Command statment
   //if FProgressForm <> nil then
   //  FProgressForm.Comment := 'Reading allele frequency data';
  OtuLabels  := TStringList.Create;  // Population names
  FFreqInfo  := TList.Create;
  FLocusInfo := TList.Create;

  repeat
    if TokenSymbolIs('#') then NextToken; // just get next token if we have #
    if not TokenSymbolIs('Command') then
      ErrorStr('Need a #Command statement in file with LocusName and NAlleles information. See manual.');

    ProcessCommandStatement;
    ReadAlleleNames;
    // now read the appropariate data
    case FDataType of
      snMicrosatellites,
      snAlleleFreq:    ReadAlleleFreqs;
      snGenotypeFreq:  ReadGenotypeFreqs;
    end;
    //if FProgressForm <> nil then
    //  FProgressForm.Comment := EmptyStr;
    // we must never fall here if NLoci is already known
    if (Token = snJustChar) and (LastChar = toEOF) then
      Break;
  until False;

  if FNoOfLoci <= 0 then
    FNoOfLoci := FLocusInfo.Count;
  //if FProgressForm <> nil then
  //   FProgressForm.Comment := 'Done reading';
end;

procedure TLexAlleleFreq.ProcessCommandStatement;
var
  CurVariable: TSnTokenCode = snNoToken;
  CurOption: TSnTokenCode = snNoToken;
  CurVariableLink: Integer = -1;
  i: Integer;
  TokenStr: String;
begin
  // reset all the temporaries
  CurNoOfAlleles    := -1;
  CurLocusIndex     := -1;
  CurLocusInfo := TLocusInfo.Create;
  FLocusInfo.Add(CurLocusInfo);

  // First we read the locusName, then the NAllels
  while True do
  begin
    NextToken;
    TokenStr := TokenString;
    if TokenSymbolIs(';') or SameText(';', TokenStr) then
      Break;                  // command statement over
    //--- Read statement-by-statement
    FIsTokenPending := True;
    ReadAssignmentStatement(CurVariable, CurOption, CurVariableLink);
    TokenLocusInfoVariableIs(CurVariable); // checks if it is valid Locus info
    if CurVariableLink > 0 then  // variable needs a valid symbol for option
      UpdateOptions(CurVariable, CurOption)
    else
      UpdateOptions(CurVariable, Token);  // the current token is important
  end;

   //if FProgressForm <> nil then
   //  FProgressForm.Comment := 'Reading Locus: '+CurLocusInfo.Name;

  // Check the LocusName first
  if CurNoOfAlleles <= 0 then
    ErrorStr('Number of alleles is not specified or locus contains less than one allele.');
  if Length(CurLocusInfo.Name) = 0 then
    ErrorStr('Locus name missing from the Command statment.');
  if (FNoOfLoci > 0) and (FLocusInfo.Count > FNoOfLoci) then
    ErrorStr('No. of loci found exceed the number expected.  Change the format statment.');
  for i:=0 to FLocusInfo.Count-2 do  // -2 since the last one is itself
    if CompareStr(TLocusInfo(FLocusInfo[i]).Name, CurLocusInfo.Name) = 0 then
      ErrorStr('Duplicate locus names are not allowed.');

  // here we set up the memory for storing the locus information
  CurLocusIndex := FLocusInfo.Count-1;
  CurFreqInfo   := TList.Create;
  FFreqInfo.Add(Pointer(CurFreqInfo));
end;

procedure TLexAlleleFreq.ReadAlleleNames;
var
  NextName: String;
  i: Integer;
begin
  if CurLocusInfo = nil then
    ErrorStr('Internal program error');

  for i:=0 to CurNoOfAlleles-1 do
  begin
    NextToken;
    if TokenSymbolIs('#') then
      ErrorStr('Not enough allele names found.');
    case Token of
      snSymbol, snInteger, snFloat: ;
    else
      ErrorStr('Allele name should be a number or a name.');
    end;

    // the name could be anything,
    NextName := TokenString;
    TrimTaxaName(NextName);
    if CurLocusInfo.HasAlleleName[NextName] then
      ErrorStr('Duplicate allele names are not allowed for a given locus');
    CurLocusInfo.AddAlleleName(NextName);
  end;
  NextToken;
  CheckTokenSymbol('#');
end;

procedure TLexAlleleFreq.ReadAlleleFreqs;
var
  i, PopsRead: Integer;
  TempInfo  : TLocusPopFreqInfo;
  Storage   : PArrayOfDouble;
  TotalFreq : Double;
  PopName   : String;
begin
  // now we start reading the population names, number of genes, and allele frequencies
  PopsRead := 0;
  while True do
  begin
    // read the population name
    NextToken;
    if Token <> snSymbol then
      ErrorStr(SSymbolExpected);

    if TokenSymbolIs('Command') then
    begin
      if FNoOfOtus <= 0 then
        FNoOfOtus := PopsRead
      else if FNoOfOtus <> PopsRead then
        ErrorStr('Expecting '+IntToStr(FNoOfOtus)+' populations, but found '+
                 IntToStr(PopsRead)+' populations.  Please check data file.');
      Break;
    end;

    CurNoOfIndv    :=-1;
    storage       := nil;
    PopName := TokenString;
    TrimTaxaName(PopName);
    if (FNoOfOtus > 0) and (OtuLabels.IndexOf(PopName) <> PopsRead) then // not same order
      ErrorStr('Populations are not written in the same order.');
    if (FNoOfOtus <= 0) and  (OtuLabels.IndexOf(PopName) > -1) then // duplicate name
      ErrorStr('Duplicate population names are not allowed');

    if FNoOfOtus <= 0 then
      OtuLabels.Add(PopName);
    Inc(PopsRead);
    if (FNoOfOtus > 0) and (PopsRead > FNoOfOtus) then
      ErrorStr('No of populations found is greater than that expected.');

    NextToken;
{    if ConvertStringToToken(TokenString) <> snNIndv then
      ErrorStr('Specify the number of chromosomes in NChr=20 type format after the population name.');
}
    NextToken; CheckTokenSymbol('='); // look for assignment
    NextToken;
    case Token of
      snInteger: CurNoOfIndv := TokenInt;
      snFloat:   CurNoOfIndv := TokenFloat;
    else
      ErrorStr(SNumberExpected);
    end;

    if CurNoOfIndv < 0 then
      ErrorStr('NChr has to be >= 0');

    TempInfo := TLocusPopFreqInfo.Create;
    CurFreqInfo.Add(TempInfo);

    Getmem(Storage, sizeOf(Double)*CurNoOfAlleles);
    TempInfo.AlleleFreq  := Storage;
    TempInfo.NoOfAlleles := CurNoOfAlleles;
    TempInfo.NoOfIndv     := CurNoOfIndv;

    // Only storage has important information
    for i:=0 to CurNoOfAlleles-1 do
      Storage[i] := -1;

    //=now read the whole line
    TotalFreq := 0;
    for i:=0 to CurNoOfAlleles-1 do
    begin
      NextToken;
      case Token of
        snInteger: Storage[i] := TokenInt;
        snFloat:   Storage[i] := TokenFloat;
        snJustChar:
          if LastChar = MissingSym then
          begin
            Storage[i] := -1;
            TempInfo.HasMissingData := False;
          end
          else if LastChar = toEOF then
            ErrorStr('Premature end of file.')
          else
            ErrorStr(SNumberExpected);
      else
        ErrorStr(SNumberExpected);
      end;
      if CurNoOfIndv = 0 then
        Storage[i] := -1
      else if Storage[i] >= 0.0 then TotalFreq := TotalFreq + Storage[i];
    end;

    if (CurNoOfIndv > 0) then
    begin
      if (TotalFreq = CurNoOfIndv) and (CurNoOfIndv > 1) then //
      begin
        for i:= 0 to CurNoOfAlleles-1 do
          if Storage[i] > 0 then
            Storage[i] := Storage[i]/CurNoOfIndv;
        TotalFreq := 1;
      end
      else if(abs(TotalFreq - 1) > 0.0000000001) then
      begin
        if MessageDlg('Allele frequencies for Locus '+
                      CurLocusInfo.Name+' for population '+
                      PopName          +' do not sum to 1. The sum is '+
                      FloatToStr(TotalFreq)+'. Is it OK to proceed further?',
                 mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
          ErrorStr('User aborted operation');
      end;
    end;

    NextToken;
    if Token = snJustChar then
    begin
      if LastChar <> toEOF then
        ErrorStr('Error reading data file.');
      // otherwise check stuff
      if FNoOfOtus <= 0 then
        FNoOfOtus := PopsRead
      else if FNoOfOtus <> PopsRead then
        ErrorStr('Expecting '+IntToStr(FNoOfOtus)+' populations, but found '+
                 IntToStr(PopsRead)+' populations.  Please check data file.');
      Break;
    end;
    CheckTokenSymbol('#');
  end;
end;

procedure TLexAlleleFreq.ReadGenotypeFreqs;
var
  PopName: String;
  TempInfo  : TLocusPopFreqInfo;
  i, j, PopsRead: Integer;
  storage: PDistanceMatrix;
  TotalFreq: Double;
begin
  // now we start reading the population names, number of genes, and allele frequencies
  PopsRead := 0;
  while True do
  begin
    // read the population name
    NextToken;
    if Token <> snSymbol then
      ErrorStr(SSymbolExpected);

    if TokenSymbolIs('Command') then
    begin
      if FNoOfOtus <= 0 then
        FNoOfOtus := PopsRead
      else if FNoOfOtus <> PopsRead then
        ErrorStr('Expecting '+IntToStr(FNoOfOtus)+' populations, but found '+
                 IntToStr(PopsRead)+' populations.  Please check data file.');
      Break;
    end;

    CurNoOfIndv    :=-1;
    storage       := nil;
    PopName := TokenString;
    TrimTaxaName(PopName);
    if (FNoOfOtus > 0) and (OtuLabels.IndexOf(PopName) <> PopsRead) then // not same order
      ErrorStr('Populations are not written in the same order.');
    if (FNoOfOtus <= 0) and  (OtuLabels.IndexOf(PopName) > -1) then // duplicate name
      ErrorStr('Duplicate population names are not allowed');

    if FNoOfOtus <= 0 then
      OtuLabels.Add(PopName);
    Inc(PopsRead);
    if (FNoOfOtus > 0) and (PopsRead > FNoOfOtus) then
      ErrorStr('No of populations found is greater than that expected.');

    NextToken;
    // if
{    if ConvertStringToToken(TokenString) = snNIndv then
    begin
      NextToken; CheckTokenSymbol('='); // look for assignment
      NextToken;
      case Token of
        snInteger: CurNoOfIndv := TokenInt;
        snFloat:   CurNoOfIndv := TokenFloat;
      else
        Error(SNumberExpected);
      end;

      if CurNoOfIndv <= 0 then
        ErrorStr('NChr has to be > 0');

      TempInfo := TLocusPopFreqInfo.Create;
      CurFreqInfo.Add(TempInfo);
      NextToken;
    end;
 }
    Storage := NewDistMatrix(CurNoOfAlleles, False);
    TempInfo.GenotypeFreq := Storage;
    TempInfo.NoOfAlleles  := CurNoOfAlleles;
    TempInfo.NoOfIndv      := CurNoOfIndv;
    TempInfo.GenotypeFreq := Storage;

    for i:=0 to CurNoOfAlleles-1 do
      for j:=0 to i do
        Storage[i][j] := -1;

    // now read the whole matrix
    TotalFreq := 0;
    for i:=0 to CurNoOfAlleles-1 do
      for j:=0 to i do
      begin
        NextToken;
        case Token of
          snInteger: Storage[i][j] := TokenInt;
          snFloat:   Storage[i][j] := TokenFloat;
          snJustChar:
            if LastChar = MissingSym then
            begin
              Storage[i][j] := -1;
              TempInfo.HasMissingData := False;
            end
            else if LastChar = toEOF then
              ErrorStr('Premature end of file.')
            else
              ErrorStr(SNumberExpected);
        else
          ErrorStr(SNumberExpected);
        end;
        if CurNoOfIndv = 0           then Storage[i][j] := -1
        else if Storage[i][j] > 0.0 then TotalFreq := TotalFreq + Storage[i][j];
      end;

    if (TotalFreq > 1.001) and
       ( ((CurNoOfIndv > 0) and (abs(TotalFreq - CurNoOfIndv) < 0.001)) or
         (CurNoOfIndv <= 0)) then
    begin
      for i:=0 to CurNoOfAlleles-1 do
        for j:=0 to i do
          if Storage[i][j] > 0 then
            Storage[i][j] := Storage[i][j]/TotalFreq;
    end
    else if(abs(TotalFreq - 1) > 0.0010000001) then
    begin
      if MessageDlg('Genotype frequencies for Locus '+
                     CurLocusInfo.Name    +' for population '+
                     PopName              +' do not sum to 1. The sum is '+
                     FloatToStr(TotalFreq)+'. Is it OK to proceed further?',
                     mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
         ErrorStr('User aborted operation');
    end;

    NextToken;
    if Token = snJustChar then
    begin
      if LastChar <> toEOF then
        ErrorStr('Error reading data file.');
      // otherwise check stuff
      if FNoOfOtus <= 0 then
        FNoOfOtus := PopsRead
      else if FNoOfOtus <> PopsRead then
        ErrorStr('Expecting '+IntToStr(FNoOfOtus)+' populations, but found '+
                 IntToStr(PopsRead)+' populations.  Please check data file.');
      Break;
    end;
    CheckTokenSymbol('#');
  end;
end;

procedure TLexAlleleFreq.updateOptions(Variable, Option: TSnTokenCode);
begin
 case Variable of
   snDataType:
       case Option of
         snMicrosatellites,
         snAlleleFreq,
         snGenotypeFreq:  FDataType := Option;
       else
         ErrorStr(SInvalidProperty);  //Invalid data type: '"<<tokenStr<<"'."<<ends;
       end;
   snNLoci:
       if Option = snInteger then FNoOfLoci := TokenInt
       else                       ErrorStr(SNumberExpected);
   snNAlleles:
       if Option = snInteger then CurNoOfAlleles := TokenInt
       else                       ErrorStr(SNumberExpected);
   snMissing:
       begin
         if Option <> snString then       ErrorStr(SStringExpected);
         if Length(TokenString) > 1 then  ErrorStr(SInvalidString);
         if Length(TokenString) < 1 then  ErrorStr(SStringExpected);
         FMissingSym   := TokenString[1];
       end;
   snLocusName:
       if (Option = snString) or (Option = snSymbol) then
         CurLocusInfo.Name := TokenString
       else
         ErrorStr(SInvalidProperty);
 end;
end;

procedure TLexAlleleFreq.TokenLocusInfoVariableIs(AToken: TSnTokenCode);
begin
  case AToken of
    snLocusName, snNAlleles: ;
  else
    ErrorStr('Invalid Domain information command: '+TokenString);
  end;
end;

end.

