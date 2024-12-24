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

unit MLexSeq;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes,
  MLongintList,
  MDomainInfo, MTokenizer,
  MegaConsts,
  KeywordConsts;

type
  TLexSeq = class(TTokenizer)
  private
    FNoOfSites      : LongInt;
    FCodeTableName  : AnsiString;
    DataRows        : TList;    // contain sequences; PChars

    FAllDomainInfo : TAllDomainInfo;     // contains all the domain type info, with gene name if present
    FDomainMark    : TAllSiteDomainMark; //

    FLabelledSiteNo: TLongIntList;          // site for which a label exists
    FSiteLabel     : TList;          // label for sites included

    IsDomainPending: Boolean;       // if a domain is being read
    CurDomainStart : LongInt;
    CurCodonStart  : Integer;
    CurDomainName  : AnsiString;
    CurGeneName    : AnsiString;
    CurDomainType  : TSnTokenCode;  // Coding/Noncoding/End etc

    TempArray     : array of AnsiChar;
    TempLen       : Integer;

    APendingCmdToken: Boolean;
    IsFirstBlock   : Boolean;

    NTotalSites: Integer;
    NTotalTaxa : Integer;

    NBlockTaxa     : Integer;
    NBlocks        : Integer;
    NBlockSitesRead: Integer;
    NBlockSeqsRead : Integer;
    BlockStartSite : Integer;
    BlockEndSite   : Integer;
    BlockFirstSeqLen:Integer;
    FIsForceEqualLen: Boolean; // distinguishes between aligned/unaligned data

    procedure ProcessCommandStatement(CurSite: LongInt; ShouldReadFile: Boolean);
    procedure ProcessMarkStatement(StartSite, EndSite: LongInt);
    procedure UpdateOptions(Variable, Option: AnsiString);
    procedure UpdateDefaults;        // call after setting all desired options
    procedure IsEverythingOK;

    procedure ReadSequenceData;

    procedure ReadLabelAndSequence;

    function  IsValidBase(x: AnsiChar): Boolean;
    procedure ValidNucSpecialIs(TheChar: AnsiChar);
    procedure ValidAminoSpecialIs(TheChar: AnsiChar);
    procedure ExamineThreeSymbols(gap,miss,iden: AnsiChar);

    function GetSequence(Index: Integer): PAnsiChar;
    function GetNoOfDomains: Integer;

    function GetNoOfLabelledSites: LongInt;
    function GetLabelledSiteNo(Index: LongInt): LongInt;
    function GetSiteLabel(Index: LongInt): AnsiChar;
  public
    constructor Create;
    destructor Destroy; override;

    function ReadHeader: Boolean;  // Reads full header
    function ReadData: Boolean;    // reads data assuming header already read
    procedure ConstructAllSiteDomainMark;  // must call before DomainMarks

    procedure DetectNoOfTaxa(var TaxaCount, BlockCount: Integer);
    property ForceEqualLenSeq : Boolean read FIsForceEqualLen write FIsForceEqualLen;

    property NoOfDomains : Integer read GetNoOfDomains;
    property AllSiteDomainMark: TAllSiteDomainMark read FDomainMark;
    property AllDomainInfo: TAllDomainInfo read FAllDomainInfo;

    property NoOfSites   : LongInt read FNoOfSites;
    property CodeTableName: AnsiString read FCodeTableName;

    property Sequence[Index: Integer]: PAnsiChar read GetSequence;

    property NoOfLabelledSites: LongInt read GetNoOfLabelledSites;
    property LabelledSite[Index: LongInt]: LongInt read GetLabelledSiteNo;
    property SiteLabel[Index: LongInt]:    AnsiChar    read GetSiteLabel;

    property Title;
    property Description;
    property FoundFormat;
    property MissingSym;
    property IdenticalSym;
    property GapSym;
    property NoOfOtus;
    property Taxon;
    property NoOfGps;
    property GpName;
    property Row;
    property Col;
    property CurrentDomainType: TSnTokenCode read CurDomainType;
  end;

implementation

uses
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ENDIF}
  SysUtils, Graphics, Controls, Forms, Dialogs, Buttons, TypInfo,
  MegaUtils, MegaErrUtils, ErrorMessages_HC, MGlobalSettings, mgeographical_info;

constructor TLexSeq.Create;
begin
  inherited Create;
  FNoOfSites      := 0;
  NTotalSites     := 0;
  FIdenticalSym   := '.';
  FIndelSym       := '-';

  DataRows        := TList.Create;

  UpdateOptProc := UpdateOptions;
  UpdateDefProc := UpdateDefaults;

  FAllDomainInfo := nil;
  FDomainMark    := nil;
  IsDomainPending:= False;
  CurDomainStart := -1;
  CurCodonStart  := 0;
  CurDomainName  := EmptyStr;
  CurGeneName    := EmptyStr;
  CurDomainType  := snNoncoding;

  FLabelledSiteNo := TLongIntList.Create;   // site for which a label exists
  FSiteLabel      := TList.Create;          // label for sites included

  FAllDomainInfo := TAllDomainInfo.Create;
  FIsForceEqualLen := True;
end;

destructor TLexSeq.Destroy;
var
  i: Integer;
begin
  if DataRows <> nil then
  begin
    for i:=0 to DataRows.Count-1 do
    begin
      if DataRows <> nil then
        FreeMem(DataRows[i]);
      DataRows[i] := nil;
    end;
    FreeAndNil(DataRows);
  end;

 { if FAllDomainInfo <> nil then
  begin
    FAllDomainInfo.DecOwnerCount;
    if AllDomainInfo.OwnerCount = 0 then
      FAllDomainInfo.Free;
    FAllDomainInfo := nil;
  end;

  if FDomainMark <> nil then
  begin
    FDomainMark.DecOwnerCount;
    if FDomainMark.OwnerCount = 0 then
      FDomainMark.Free;
    FDomainMark := nil;
  end;
  }
  TempArray := nil;

  FLabelledSiteNo.Free;
  FSiteLabel.Free;
  inherited Destroy;
end;

function TLexSeq.GetNoOfLabelledSites: LongInt;
begin
  Result := FLabelledSiteNo.Count;
end;

function TLexSeq.GetLabelledSiteNo(Index: LongInt): LongInt;
begin
  Result := FLabelledSiteNo[Index];
end;

function TLexSeq.GetSiteLabel(Index: LongInt): AnsiChar;
begin
  Result := AnsiChar(FSiteLabel[Index]);
end;

function TLexSeq.GetNoOfDomains: Integer;
begin
  if FAllDomainInfo = nil then Result := 0
  else                         Result := FAllDomainInfo.NoOfDomains;
end;

procedure TLexSeq.ConstructAllSiteDomainMark;
var
  i, j: Integer;
  ADomainInfo: TDomainInfo;
begin
  if FDomainMark <> nil then Exit;

  FDomainMark := TAllSiteDomainMark.Create;
  //FDomainMark.IncOwnerCount;
  FDomainMark.NoOfSites := FNoofSites;

  ADomainInfo := nil;
  if FAllDomainInfo.NoOfDomains = 0 then
  begin
    ADomainInfo := TDomainInfo.Create;
    ADomainInfo.Name := 'Data';
    ADomainInfo.FromSite := 0;
    ADomainInfo.ToSite   := FNoOfSites-1;
    FAllDomainInfo.Add(ADomainInfo);
    ADomainInfo := nil;
  end;

  for i:=0 to NoOfDomains-1 do
    with FAllDomainInfo[i] do
      if IsDomain and (FromSite >= 0) then
        for j:= FromSite to ToSite do
           FDomainMark[j] := FAllDomainInfo[i];

  if NoOfLabelledSites > 0 then
  begin
    j := NoOfLabelledSites;
    for i:=0 to j-1 do
      FDomainMark.SiteLabel[FLabelledSiteNo[i]] := SiteLabel[i];
  end;
end;

//--- ONLY USEFUL AFTER THE COMPLETE READING; BUT ERRORS MAY OCCUR DURING READING
procedure TLexSeq.DetectNoOfTaxa(var TaxaCount, BlockCount: Integer);
var
  UniqueTaxaList: TStringList;
  i: Integer;
begin
  TaxaCount := 0;
  BlockCount := 0;
  UniqueTaxaList:= nil;
  try
    UniqueTaxaList := TStringList.Create;

    // first find the block size
    UniqueTaxaList.Add(Lowercase(FTaxa[0]));
    for i:=1 to FTaxa.Count-1 do
    begin
      if UniqueTaxaList.IndexOf(Lowercase(FTaxa[i])) >= 0 then
        break;
      UniqueTaxaList.Add(Lowercase(FTaxa[i]));
    end;
    TaxaCount := UniqueTaxaList.Count;
    BlockCount := DataRows.Count div TaxaCount;
    if FTaxa.Count mod TaxaCount > 0 then
      ErrorStr(' Recheck the data file.');
  finally
    UniqueTaxaList.Free;
  end;
end;

function TLexSeq.GetSequence(Index: Integer): PAnsiChar;
var
  Seq0, SeqN, TempChar  : PAnsiChar;
  CurI, i, j: Integer;
begin
  // not smart enough yet to take care of unaligned sequences
  Result := nil;
  if DataRows = nil then
    Exit;
  if (Index < 0) or (Index >= NoOfOtus) then
    RaiseErrorMessage(HC_Unexpected_Error, 'Index overrun in '+ClassName+'.');

  TempChar := nil;

  try
    GetMem(TempChar, (NoOfSites+1)*sizeOf(AnsiChar));

    // for non-interleaved data
    if DataRows.Count = NoOfOtus then  // i.e., noninterleaved data
    begin
      Seq0 := DataRows[0];
      SeqN := DataRows[Index];
      for i:=0 to NoOfSites-1 do
        if SeqN[i] = FIdenticalSym then
          TempChar[i] := Seq0[i]
        else
          TempChar[i] := SeqN[i];
      TempChar[NoOfSites] := #0;
    end
    else // for interleaved data
    begin
      CurI := 0;
      for j:=0 to (DataRows.Count div NoOfOtus)-1 do
      begin
        Seq0 := DataRows[NoOfOtus*j];
        SeqN := DataRows[NoOfOtus*j+Index];
        i:=0;
        while SeqN[i] <> #0 do
        begin
          if SeqN[i] = FIdenticalSym then TempChar[CurI] := Seq0[i]
          else                            TempChar[CurI] := SeqN[i];
          Inc(CurI);
          Inc(i);
        end;
      end;
      TempChar[CurI] := #0;
      if CurI <> NoOfSites then
       RaiseErrorMessage(HC_Unexpected_Error, 'Site count mismatch in '+ClassName+'.');
    end;
    Result := TempChar;
    TempChar := nil;
  finally
    FreeMemAndNil(TempChar);
  end;
end;

//---- reads data only; assumes header already read
function TLexSeq.ReadData: Boolean;
begin
  Result := False;
  {$IFDEF VISUAL_BUILD}
  if FGauge   <> nil then  FGauge.Show;
  {$ENDIF}
  IsEverythingOK;
  UpdateDefaults;

  if FCaption <> nil then  FCaption.Text := 'Reading sequences';
    ReadSequenceData;
   {$IFDEF VISUAL_BUILD}
  if FGauge   <> nil then  FGauge.Position := 100;
   {$ENDIF}
  if FCaption <> nil then  FCaption.Text := 'Done Reading';
  Result := True;
end;


//---- Reads complete file header
function TLexSeq.ReadHeader: Boolean;  // only reads header
var
  IsCommandEverFound: Boolean;
begin
  Result := False;
  Unwind;       //--- Unwind

  APendingCmdToken:= False;
  FForceLen := False;
  FTokenLen := -1;

  //--- Read #MEGA
  NextToken;
  CheckTokenSymbol('#');
  NextToken;
  CheckTokenSymbol('MEGA');

  IsCommandEverFound := False;

  while True do  // read until valid commands are found
  begin
    NextToken;

    if Token = toComment then
    begin
      GetStringToken(toCommentEnd);
      continue;
    end;
    if Token = toCommentMEGA1 then
    begin
      GetStringToken(toCommentMEGA1);
      continue;
    end;

    if Token <> toCommand then
    begin
      if IsCommandEverFound then
      begin
        FIsTokenPending := True;
        Break;
      end;
      //otherwise it is a MEGA1 style format
      if TokenSymbolIs('Title') then
      begin
        GetStringToken(#13);
        FStringToken := Trim(FStringToken);
        if Length(FStringToken) > 0 then
          if FStringToken[length(FStringToken)] = ';' then // remove semi colon
            FStringToken[length(FStringToken)] := ' ';
        FStringToken := Trim(FStringToken);
        FTitle := FStringToken;
        FOptions.Add('Title='+FTitle);
        SkipUntilChar('#'); // this just removes all the garbage after Title line
        Break;
      end
      else
        ErrorStr('The data file must contain a Title command following the #mega');
    end;

    IsCommandEverFound := True;
    NextToken;
    if TokenSymbolIs('Title') then
    begin
      if FOptions.IndexOfName('Title') > 0 then
        ErrorStr('Duplicate Title found');
      GetStringToken(toCommandEnd);
      FTitle := FStringToken;
      FOptions.Add('Title='+FTitle);
    end
    else if TokenSymbolIs('Description') then
    begin
      if FOptions.IndexOfName('Description') > 0 then
        ErrorStr('Duplicate Descripton found');
      GetStringToken(toCommandEnd);
      FDescription := FStringToken;
      FOptions.Add('Description='+FDescription);
    end
    else if TokenSymbolIs('Format') then
    begin
      if FFoundFormat = True then
        ErrorStr('Duplicate Format command found');
      FIsTokenPending := True;
      ReadFormat;
      FFoundFormat := True;
    end
    else  // the command has to be associated with some data so don't worry
    begin
      APendingCmdToken:= True;
      FIsTokenPending := True;
      Break;
    end;
  end; // end of while
  Result := True;
end;

procedure TLexSeq.updateDefaults;
begin
  if FDataFormat = snNoToken then
    FDataFormat := snBlockwise;
end;

procedure TLexSeq.IsEverythingOK;
begin
  ExamineThreeSymbols(gapSym, missingSym, identicalSym);
end;

procedure TLexSeq.ReadSequenceData;
var
  AStr: AnsiString;
begin
  FForceLen := False;
  FTokenLen := -1;

  NTotalSites := 0;
  NTotalTaxa := 0;

  BlockStartSite := 0;
  BlockEndSite   := 0;

  NBlocks        := 0;
  NBlockTaxa     := 0;
  IsFirstBlock   := True;

  if APendingCmdToken then
    ProcessCommandStatement(NTotalSites, True);
  APendingCmdToken := False;
  while true do
  begin
    NextToken;
    case Token of
      toPound:
        begin
          FIsTokenPending := True;
          if (not IsFirstBlock) and (NBlockTaxa = NBlockSeqsRead) then
          begin
            NTotalSites := NTotalSites + NBlockSitesRead;
            NBlockSitesRead := 0;
            NBlockSeqsRead := 0;
          end;
          ReadLabelAndSequence;
        end;
      toCommand:
        begin
          NextToken;
          while Token = toComment do
          begin
            GetStringToken(toCommentEnd);
//            if FOptions <> nil then FOptions.Add('Comment after ! point' +'-'+IntToStr(NTotalSites)+'='+Trim(FStringToken));;
            NextToken;
          end;

          while Token = toCommentMEGA1 do
          begin
            GetStringToken(toCommentMEGA1);
//            if FOptions <> nil then FOptions.Add('Comment after ! point' +'-'+IntToStr(NTotalSites)+'='+Trim(FStringToken));;
            NextToken;
          end;

          if not (TokenSymbolIs('gene') or TokenSymbolIs('domain') or TokenSymbolIs('label')) then
              ErrorStr('All command statements in sequence data files must begin with gene, domain or mark keywords.');

          BlockEndSite := BlockStartSite + NBlockSitesRead - 1;

          // if a mark statement then read it properly
          if TokenSymbolIs('label') then
          begin
            if IsFirstBlock then
            begin
              NBlockTaxa := NBlockSeqsRead;
              NBlockSeqsRead := 0;
            end;
            NTotalSites := NTotalSites + NBlockSitesRead;
            if NBlockSitesRead > 0 then
              ProcessMarkStatement(BlockStartSite, BlockEndSite)
            else
              ErrorStr('No sequence data in the current block, so Mark statement is invalid.');
            NBlockSitesRead := 0;
          end
          else
          begin
            if (not IsFirstBlock) and (NBlockTaxa <> NBlockSeqsRead) then
              ErrorStr('Previous block did not contain enough sequences.');
            if (NBlockSeqsRead > 0) and (IsFirstBlock or (NBlockTaxa = NBlockSeqsRead)) then // modified for m2b3
              NTotalSites := NTotalSites + NBlockSitesRead;
            ProcessCommandStatement(NTotalSites, True);
            NBlockSitesRead := 0;
            NBlockSeqsRead := 0;
          end;
        end;
      toComment:
        begin
          GetStringToken(toCommentEnd);
//          if FOptions <> nil then FOptions.Add('Comment' +IntToStr(NBlockSeqsRead+1)+'-'+IntToStr(NTotalSites)+'='+Trim(FStringToken));;
        end;
      toCommentMEGA1:
        begin
          GetStringToken(toCommentMEGA1);
//          if FOptions <> nil then FOptions.Add('Comment' +IntToStr(NBlockSeqsRead+1)+'-'+IntToStr(NTotalSites)+'='+Trim(FStringToken));;
        end;

      toEOF:     break;

    else
       ErrorStr('Unexpected symbol : '+TokenString);
    end;
    if Token = toEOF then
    begin
      // may be the last block is left unread at this point
      //ShowMessage('Reached end of file.');
      break;
    end;
  end;

  //
  if IsDomainPending then
  begin
    NTotalSites := NTotalSites + NBlockSitesRead;
    NBlockSitesRead := 0;
    ProcessCommandStatement(NTotalSites, False);
    IsDomainPending := False;
  end;

  // if the the last block is not handled then
  if NBlockSitesRead > 0 then
    NTotalSites := NTotalSites + NBlockSitesRead;

  // here do clean up and ask the user about problems
  if (FNoOfSites > 0) and (FNoOfSites <> NTotalSites) then
  begin
    AStr := 'The specified number of sites in the format statement ('+
             IntToStr(FNoOfSites)+') does not match the number of sites found by MEGA ('+
             IntToStr(NTotalSites)+').  ';
    {$IFDEF VISUAL_BUILD}
    if MessageDlg(AStr + 'Ignore the difference and proceed with what MEGA read?', mtConfirmation, [mbYes,mbNo], 0) <> mryes then
      ErrorStr(AStr);
    {$ELSE}
    error_NV(ASTR);
    {$ENDIF}
  end;

  FNoOfSites := NTotalSites;

  if NBlockTaxa = 0 then
    NBlockTaxa:= NBlockSeqsRead;

  if NBlockSeqsRead <> 0 then // NBlockSeqsRead will be 0 when we only have one block.  In this case we expect to read 0 when we try to read the 'next' block.
    if NBlockSeqsRead <> NBlockTaxa then
      ErrorStr('Expecting : '+IntToStr(NBlockTaxa)+', but found only : ' +IntToStr(NBlockSeqsRead)+' sequences in the last block.');


  if (FNoOfOtus > 0) and (FNoOfOtus <> NBlockTaxa) then
  begin
    AStr := 'The specified number of sequences in the format statement ('+
             IntToStr(FNoOfOtus)+') does not match the number of sequences found by MEGA ('+
             IntToStr(NBlockTaxa)+').  ';
    {$IFDEF VISUAL_BUILD}
    if MessageDlg(AStr + 'Ignore the difference and proceed with what MEGA read?', mtConfirmation, [mbYes,mbNo], 0) <> mrYes then
      ErrorStr(AStr);
    {$ELSE}
    warn_nv(AStr);
    {$ENDIF}
  end;
  if NTotalTaxa = 0 then
    NTotalTaxa := NBlockTaxa;
  FNoOfOtus := NTotalTaxa;
  if FNoOfOtus = 0 then
    ErrorStr('No sequences found');

  if FNoOfSites = 0 then
    ErrorStr('All Sequences are of length zero');
end;

//==== Processes Command statements statement
procedure TLexSeq.ProcessCommandStatement(CurSite: LongInt; ShouldReadFile: Boolean);
var
  AVar: AnsiString = '';
  AOpt: AnsiString = '';
  DomainInfo: TDomainInfo;
begin
  DomainInfo := nil;
  if IsDomainPending then
  begin
    DomainInfo := TDomainInfo.Create;
    FAllDomainInfo.Add(DomainInfo);
    DomainInfo.Name     := CurDomainName;
    DomainInfo.FromSite := CurDomainStart;
    DomainInfo.ToSite   := CurSite-1;
    if CurGeneName <> EmptyStr then
      DomainInfo.GeneName := CurGeneName;
    DomainInfo.IsCoding := False;
    if FDataType = snNucleotide then
    begin
      case CurDomainType of
        snExon, snCoding:
        begin
          DomainInfo.IsCoding := True;
          DomainInfo.CodonStart := CurCodonStart;
        end;
        snIntron,
        snNonCoding: DomainInfo.IsCoding := False;
      end;
      // write up what I have read
//      if FOptions <> nil then
//      begin
//        FOptions.Add('Domain : '+DomainInfo.Name);
//        FOptions.Add(' From  : '+IntToStr(DomainInfo.FromSite+1) +' to '+IntToStr(DomainInfo.ToSite+1));
//        if DomainInfo.IsCoding then
//          FOptions.Add(' Coding with codon start at '+IntToStr(DomainInfo.CodonStart+1));
//      end;
    end;
    CurDomainName  := EmptyStr;
    CurGeneName    := EmptyStr;
    CurDomainType  := snNoncoding;
    IsDomainPending:= False;
  end
  else
    CurDomainType  := snNoncoding;

  // by this point, the previous block has ended
  if (NBlockSeqsRead > 0) and (NBlockTaxa <= 0) then
    NBlockTaxa := NBlockSeqsRead;

  if not ShouldReadFile then
    Exit;

  FIsTokenPending := False;
  while Token <> toCommandEnd do
  begin
    if (Token = toPound) or (Token = toCommand) then // common error of forgetting to put ;
         raise EParserError.CreateFmt(SParseError, [' all command statements should end with a semicolon.', SourceLine]);

    while Token = toComment do
    begin
      GetStringToken(toCommentEnd);
//      if FOptions <> nil then FOptions.Add('Comment' +'Command-'+IntToStr(NTotalSites)+'='+Trim(FStringToken));;
      NextToken;
    end;
    while Token = toCommentMEGA1 do
    begin
      GetStringToken(toCommentMEGA1);
//      if FOptions <> nil then FOptions.Add('Comment' +'Command-'+IntToStr(NTotalSites)+'='+Trim(FStringToken));;
      NextToken;
    end;

    if Token = toCommandEnd then
      break;
    ReadAssignmentStatement(AVar, AOpt);
    UpdateOptions(AVar, AOpt);
    NextToken;
  end;

  if CurDomainType = snDomainEnd then  // nothing to do; just end a domain
  begin
    Exit;
  end;

  // validate everything
  if CurSite > 0 then  CurDomainStart := CurSite // the next site will be the one
  else                 CurDomainStart := 0;

  // here validate the domain name first
  if (Length(CurGeneName) > 0) and (Length(CurDomainName) = 0) then
    CurDomainName := CurGeneName+' data'
  else if (Length(CurGeneName) = 0) and (Length(CurDomainName) = 0) then
      ErrorStr('No Gene/Domain name given.');

{  if (FAllDomainInfo <> nil) and (FAllDomainInfo.NoOfDomains > 0) then   // check if unique name
  begin
    for i:=0 to FAllDomainInfo.NoOfDomains-1 do
      if (CompareText(FAllDomainInfo[i].Name, CurDomainName) = 0) and
         (CompareText(FAllDomainInfo[i].GeneName, CurGeneName)=0) then
           ErrorStr('Duplicate Gene/domain name: '+CurDomainName);
  end;
}
  IsDomainPending := True;
end;

//==== Processes the Mark statement
procedure TLexSeq.ProcessMarkStatement;
var
  i: Integer;
  x: AnsiChar;
begin
  if (BlockStartSite < 0) or (BlockEndSite < StartSite) then
    ErrorStr('Please put the Mark statement on the line immediately following the sequence block.');

  if not FIsForceEqualLen then
    ErrorStr('Site marking is allowed only for aligned sequences');


  // curSite tells me the total number of sites
  if Token = toCommand then
  begin
    NextToken;
    FIsTokenPending:=False;
    NextToken;
  end;
  FIsTokenPending:=False;

  i:= BlockStartSite;  // now read the sequence
  try
    FForceLen := True;
    FTokenLen := 1;

    while True do
    begin
      NextToken;
      if Token = toCommandEnd then
      begin
        FIsTokenPending := False;
        Break;                  // command statement over
      end;

      while Token = toComment do
      begin
        GetStringToken(toCommentEnd);
//        if FOptions <> nil then FOptions.Add('Comment' +'Mark-'+IntToStr(i+1)+'='+Trim(FStringToken));
        NextToken;
      end;
      while Token = toCommentMEGA1 do
      begin
        GetStringToken(toCommentMEGA1);
//        if FOptions <> nil then FOptions.Add('Comment' +'Mark-'+IntToStr(i+1)+'='+Trim(FStringToken));
        NextToken;
      end;

      if (Token = toPound) or (Token = toCommand) then
        ErrorStr('All mark statements must end with a semicolon');

      if i>BlockEndSite then
        ErrorStr('Mark statements contains too many labels');

      x := TokenString[1];
      if not IsValidSiteLabel(x) then
        ErrorStr('Invalid site mark found.');

      if x <> '_' then
      begin
        FLabelledSiteNo.Add(i);
        FSiteLabel.Add(Pointer(x));
      end;
      Inc(i);
    end;
  finally
    FForceLen := False;
  end;
end;

{
What if the comment is after pound or before pound sign}
procedure TLexSeq.ReadLabelAndSequence;
var
  i, CurSeqLen: Integer;
  base: AnsiChar;
  MySeqPChar: PAnsiChar;
  aInfo: TGeographicalInfo = nil;
  aMsg: AnsiString = '';

  function HasTaxa(const HT_AStr: AnsiString): Boolean;
  var
    ht_i: Integer;
  begin
    Result := False;
    for ht_i := 0 to FTaxa.Count-1 do
      if CompareText(FTaxa[ht_i], HT_AStr) = 0 then
      begin
        Result := True;
        Exit;
       end;
  end;
begin
  // first read the OTU label and process
  ReadOtuLabel;
  if IsFirstBlock then
  begin
    if HasTaxa(OtuLabel) then
    begin
       // this is the first time it found a sequence name repeated
       // so if this is not the same as the first sequence then it is not interleaved
       // added in 2.0 beta build 2
       if CompareText(OtuLabel, FTaxa[0]) <> 0 then
        ErrorStr('MEGA has detected two sequences with the same name, but the data does not appear '+
                 'to be in proper interleaved format.  Check for duplicate names.');

      IsFirstBlock := False;
      Inc(NBlocks);
      if (NBlocks = 1) and (NBlockTaxa <= 0) then
        NBlockTaxa := NBlockSeqsRead
      else if (NBlockSeqsRead >0) and (NBlockSeqsRead <> NBlockTaxa) then
        ErrorStr('Different blocks appear to have different number of sequences');
      // in the future allow unequal number of sequences (add automatically)
      NBlockSeqsRead := 0;
      NTotalSites := NTotalSites + NBlockSitesRead;
      NBlockSitesRead := 0;
    end
    else
    begin
      FTaxa.Add(OtuLabel);
      if Length(GpLabel) > 0 then
        FGps.Add(GpLabel)
      else
        FGps.Add(EmptyStr);

      if Length(SpLabel) > 0 then
        FSps.Add(SpLabel)
      else
        FSps.Add(EmptyStr);

      if Length(PopLabel) > 0 then
        FPops.Add(PopLabel)
      else
        FPops.Add(EmptyStr);

      aInfo := TGeographicalInfo.Create;
      if not aInfo.InitFromLabels(GpLabel, SpLabel, PopLabel, ContinentLablel, CountryLabel, CityLabel, YearLabel, MonthLabel, DayLabel, TimeLabel, aMsg) then
        ErrorStr(Format('Failed to parse otu name command statement(%s) - %s', [FullLabel, aMsg]));
      FGeographicalInfoList.Add(aInfo);
      SetLength(FOutgroups, Length(FOutgroups) + 1);
      FOutgroups[Length(FOutgroups) - 1] := IsOutGroup;
      if IsOutgroup then
        FGps[FGps.Count - 1] := 'outgroup';
    end
  end;

  // always fall here
  if not IsFirstBlock then
  begin
    if NBlockSeqsRead = NBlockTaxa then // this means an interleaved format
    begin
      NBlockSeqsRead := 0;
      NBlockSitesRead:= 0;
    end;

    if NBlockSeqsRead = 0 then
      BlockStartSite := NTotalSites;

    if CompareText(OtuLabel, FTaxa[NBlockSeqsRead]) <> 0 then
      ErrorStr('Expecting Sequence '+FTaxa[NBlockSeqsRead]+', but found '+FullLabel+'.');
    if Length(GpLabel) > 0 then
      if CompareText(GpLabel, FGps[NBlockSeqsRead]) <> 0 then
        ErrorStr('Conflicting Group designations ['+ FTaxa[NBlockSeqsRead]+','+GpLabel+' for Sequence '+FullLabel+'.');
  end;

  //read the sequence
  if TempArray = nil then
  begin
    TempLen := 1024;
    SetLength(TempArray, TempLen);
  end;
  // read sequence
  FForceLen := True;
  FTokenLen := 1;
  CurSeqLen := 0;
  FDetectNewLine := False;
  FNeedSymbol := toChar;
  NextToken;

  while Token = toComment do
  begin
    GetStringToken(toCommentEnd);
//    if FOptions <> nil then FOptions.Add('Comment' +IntToStr(NBlockSeqsRead+1)+'-'+IntToStr(NTotalSites+CurSeqLen)+'='+Trim(FStringToken));;
    NextToken;
  end;
  while Token = toCommentMEGA1 do
  begin
    GetStringToken(toCommentMEGA1);
//    if FOptions <> nil then FOptions.Add('Comment' +IntToStr(NBlockSeqsRead+1)+'-'+IntToStr(NTotalSites+CurSeqLen)+'='+Trim(FStringToken));;
    NextToken;
  end;

  while Token = toSymbol do
  begin
    base := TokenString[1];

    // check if it is a valid base
    if not IsValidBase(base) then
      ErrorStr('Invalid base found: '+base);

    // now look for the identical symbol
    if (base = IdenticalSym) and (NBlockSeqsRead = 0) then
      ErrorStr('Cannot use identical symbol in the first sequence');

    TempArray[CurSeqLen] := base;
    Inc(CurSeqLen);
    if CurSeqLen >= (TempLen-1) then
    begin
      TempLen := TempLen + 1024;
      SetLength(TempArray, TempLen);
    end;
    NextToken;
    while Token = toComment do
    begin
      GetStringToken(toCommentEnd);
//      if FOptions <> nil then FOptions.Add('Comment' +IntToStr(NBlockSeqsRead+1)+'-'+IntToStr(NTotalSites+CurSeqLen)+'='+Trim(FStringToken));;
      NextToken;
    end;
    while Token = toCommentMEGA1 do
    begin
      GetStringToken(toCommentMEGA1);
//      if FOptions <> nil then FOptions.Add('Comment' +IntToStr(NBlockSeqsRead+1)+'-'+IntToStr(NTotalSites+CurSeqLen)+'='+Trim(FStringToken));;
      NextToken;
    end;
  end;
  FIsTokenPending := True;
  FForceLen := False;

  if CurSeqLen > NBlockSitesRead then // in unaligned seq, this will lead to largest size
    NBlockSitesRead := CurSeqLen;

  // force equal lengths
  if FIsForceEqualLen then
  begin
    if NBlockSeqsRead = 0 then
      BlockFirstSeqLen := CurSeqLen
    else if CurSeqLen <> BlockFirstSeqLen then
      ErrorStr('Aligned sequences must be equal lengths.')
  end;

  // now save the read sequence into DataRows
  MySeqPChar := nil;
  try
    GetMem(MySeqPchar, (CurSeqLen+1)*sizeof(AnsiChar));
    for i:=0 to CurSeqLen-1 do
      MySeqPChar[i] := TempArray[i];
    MySeqPChar[CurSeqLen] := #0;
    DataRows.Add(MySeqPChar);
    MySeqPChar := nil;
    Inc(NBlockSeqsRead);
  finally
    FreeMemAndNil(MySeqPChar);
  end;
  FNeedSymbol := #0;
end;

function TLexSeq.IsValidBase(x: AnsiChar): Boolean;
begin
  Result := False;
  if (x = FMissingSym) or (x = FIndelSym) or (x = FIdenticalSym) then
    Result := True
  else
    case FDataType of
      snNucleotide:
        case UpCase(x) of
          'A'..'D','G','H','K','M','R','S','T','U','V','W','Y','N': Result := True;
        end;
      snProtein:
        case UpCase(x) of
          'A'..'I','K'..'N', 'P'..'T', 'V'..'Z', '*': Result := True;
        end;
    end;
end;

procedure TLexSeq.ExamineThreeSymbols(gap,miss,iden: AnsiChar);
begin
  if (gap=miss) or (gap=iden) or (miss=iden) then
    ErrorStr('Gap, missing, and Identical site symbols must be different.');
end;

procedure TLexSeq.updateOptions(Variable, Option: AnsiString);
begin
  if FOptions <> nil then FOptions.Add(Variable+'='+Option);
  Variable := Lowercase(Variable);
  if CompareText(Variable, 'datatype') = 0 then
  begin
    Option := Lowercase(Option);
    if      CompareText(Option, 'DNA')        = 0 then FDataType := snNucleotide
    else if CompareText(Option, 'RNA')        = 0 then FDataType := snNucleotide
    else if CompareText(Option, 'Nucleotide') = 0 then FDataType := snNucleotide
    else if CompareText(Option, 'Protein') = 0 then FDataType := snProtein
    else
      Error(SInvalidProperty);  //Invalid data type: '"<<tokenStr<<"'."<<ends;
  end
  else if CompareText(Variable, 'ntaxa') = 0 then
  begin
    FNoOfOtus := StrToInt(Option);
  end
  else if CompareText(Variable, 'nseqs') = 0 then
  begin
    FNoOfOtus := StrToInt(Option);
  end
  else if CompareText(Variable, 'nsites') = 0 then
  begin
    FNoOfSites := StrToInt(Option);
  end
  else if CompareText(Variable, 'dataformat') = 0 then
  begin
    Option := Lowercase(Option);
    if      CompareText(Option, 'continuous') = 0 then FDataFormat := snContinuous
    else if CompareText(Option, 'noninterleaved')= 0  then FDataFormat := snContinuous
    else if CompareText(Option, 'blockwise') = 0  then FDataFormat := snBlockwise
    else if CompareText(Option, 'interleaved') = 0  then FDataFormat := snBlockwise
    else
      Error(SInvalidProperty);
  end
  else if CompareText(Variable, 'indel') = 0 then
  begin
    Option := Lowercase(Option);
    case FDataType of
      snNucleotide:  ValidNucSpecialIs(Option[1]);
      snProtein:     ValidAminoSpecialIs(Option[1]);
    end;
    FIndelSym := Option[1];
  end
  else if CompareText(Variable, 'missing') = 0 then
  begin
    Option := Lowercase(Option);
    case FDataType of
      snNucleotide:  ValidNucSpecialIs(Option[1]);
      snProtein:     ValidAminoSpecialIs(Option[1]);
    end;
    FMissingSym := Option[1];
  end
  else if (CompareText(Variable, 'identical') = 0) or (CompareText(Variable, 'matchchar') = 0) then
  begin
    Option := Lowercase(Option);
    case FDataType of
      snNucleotide:  ValidNucSpecialIs(Option[1]);
      snProtein:     ValidAminoSpecialIs(Option[1]);
    end;
    FIdenticalSym := Option[1];
  end
  else if CompareText(Variable, 'codetable') = 0 then   FCodeTableName := Option
  else if CompareText(Variable, 'codonstart') = 0 then  CurCodonStart := StrToInt(Option)-1
  else if CompareText(Variable, 'domain') = 0 then      CurDomainName := Option
  else if CompareText(Variable, 'gene') = 0 then        CurGeneName := Option
  else if CompareText(Variable, 'property') = 0 then
  begin
    Option := Lowercase(Option);
    if      CompareText(Option, 'exon')      = 0 then  CurDomainType  := snExon
    else if CompareText(Option, 'coding')    = 0 then  CurDomainType  := snCoding
    else if CompareText(Option, 'intron')    = 0 then CurDomainType   := snIntron
    else if CompareText(Option, 'noncoding') = 0 then CurDomainType   := snNonCoding
    else if CompareText(Option, 'domainend') = 0 then CurDomainType   := snDomainEnd
    else
      Error(SInvalidProperty);
 end
 else
   ErrorStr('Invalid statement in the Format command.');
end;

procedure TLexSeq.ValidNucSpecialIs(TheChar: AnsiChar);
begin
  case UpCase(TheChar) of
    'A'..'Z':
      case TheChar of
        'A'..'D','G','H','K','M','R','S', 'T', 'U', 'V','W','Y':  Error(SInvalidProperty);
      else
        Exit;
      end;
    '?','-','.':       Exit;
  else
    Error(SInvalidProperty);
  end;
end;

procedure TLexSeq.ValidAminoSpecialIs(TheChar: AnsiChar);
begin
  case UpCase(TheChar) of
    'A'..'Z':
      case UpCase(TheChar) of  // Wasn't changing TheChar to uppercase when comparing against J,O,U,X
        'J','O','U','X':
           Exit;
       else
         Error(SInvalidProperty);
      end;
    '?','-','.':
      Exit;
  else
    Error(SInvalidProperty);
  end;
end;

end.

